#!/usr/bin/env bash
# Default-deny egress firewall for the snap_qc dev container. Runs at container
# start (postStartCommand, via passwordless sudo). It is what makes running the
# agent with permission prompts off acceptable: outbound traffic is limited to
# the hosts the toolchain actually needs. Translated from the jspsi reference;
# the allowlist is tuned for R (CRAN/Posit), Python (PyPI), GitHub, and the
# Anthropic API instead of the Node stack.
set -euo pipefail
IFS=$'\n\t'

# Fail closed: if anything below errors, drop everything.
trap 'echo "firewall init FAILED; setting DROP"; iptables -P INPUT DROP; iptables -P OUTPUT DROP; iptables -P FORWARD DROP; exit 1' ERR

echo "[firewall] resetting rules"
iptables -F; iptables -X; iptables -t nat -F || true; iptables -t nat -X || true
ipset destroy allowed 2>/dev/null || true
ipset create allowed hash:net

# Preserve DNS, but only to the resolvers this container is actually configured
# to use -- Docker's embedded resolver (127.0.0.11) on a user-defined network,
# or the inherited host resolvers on the default bridge. An unrestricted
# "--dport 53 to anywhere" rule is an open DNS tunnel straight through the
# allowlist, which defeats the point of default-deny.
dns_servers=$(awk '/^nameserver/ {print $2}' /etc/resolv.conf | grep -E '^[0-9.]+$' || true)
if [ -z "$dns_servers" ]; then
  echo "[firewall] FATAL: no IPv4 nameserver found in /etc/resolv.conf"; exit 1
fi
while read -r ns; do
  [ -n "$ns" ] || continue
  iptables -A OUTPUT -p udp --dport 53 -d "$ns" -j ACCEPT
  iptables -A OUTPUT -p tcp --dport 53 -d "$ns" -j ACCEPT
  iptables -A INPUT  -p udp --sport 53 -s "$ns" -j ACCEPT
  iptables -A INPUT  -p tcp --sport 53 -s "$ns" -j ACCEPT
  echo "[firewall] DNS resolver $ns allowed"
done <<< "$dns_servers"

# Loopback and established/related traffic.
iptables -A OUTPUT -o lo -j ACCEPT
iptables -A INPUT  -i lo -j ACCEPT
iptables -A INPUT  -m state --state ESTABLISHED,RELATED -j ACCEPT
iptables -A OUTPUT -m state --state ESTABLISHED,RELATED -j ACCEPT

add_host() {  # resolve A records, add to ipset
  local host="$1" required="${2:-optional}" ips
  ips=$(dig +short A "$host" | grep -E '^[0-9.]+$' || true)
  if [ -z "$ips" ]; then
    if [ "$required" = required ]; then echo "[firewall] FATAL: cannot resolve $host"; exit 1; fi
    echo "[firewall] warn: no A record for $host (skipped)"; return
  fi
  while read -r ip; do [ -n "$ip" ] && ipset add allowed "$ip" 2>/dev/null || true; done <<< "$ips"
  echo "[firewall] allowed $host"
}

# Required: the agent API and the package sources the build/runtime depend on.
add_host api.anthropic.com          required
add_host registry.npmjs.org         required   # claude-code install/update
add_host cloud.r-project.org        required   # CRAN
add_host packagemanager.posit.co    required   # rocker binary R packages
add_host pypi.org                   required
add_host files.pythonhosted.org     required

# Optional best-effort (Claude web, VS Code marketplace, CRAN mirror, R docs).
for h in claude.ai console.anthropic.com statistics.anthropic.com \
         cran.r-project.org r-project.org \
         marketplace.visualstudio.com vscode.blob.core.windows.net \
         update.code.visualstudio.com; do
  add_host "$h" optional
done

# GitHub's published CIDR ranges (web + api + git) so github.com and gh work.
echo "[firewall] fetching GitHub CIDRs"
gh_meta=$(curl -fsSL https://api.github.com/meta || true)
if [ -n "$gh_meta" ]; then
  echo "$gh_meta" | jq -r '(.web + .api + .git)[]' | grep -E '^[0-9.]+/[0-9]+$' \
    | aggregate -q 2>/dev/null | while read -r cidr; do ipset add allowed "$cidr" 2>/dev/null || true; done
  echo "[firewall] added GitHub ranges"
else
  echo "[firewall] warn: could not fetch GitHub meta"
fi

# Allow the Docker host gateway (so the IDE/agent bridge works).
gw=$(ip route | awk '/default/ {print $3; exit}')
[ -n "${gw:-}" ] && iptables -A OUTPUT -d "$gw" -j ACCEPT && echo "[firewall] host gateway $gw allowed"

# Permit traffic to the allowed set; default-deny the rest with a clear reject.
iptables -A OUTPUT -m set --match-set allowed dst -j ACCEPT
iptables -P INPUT DROP
iptables -P FORWARD DROP
iptables -P OUTPUT DROP
iptables -A OUTPUT -j REJECT --reject-with icmp-port-unreachable

# No IPv6 egress: an unfiltered v6 path routes straight around the v4 allowlist.
# This must NOT be best-effort. If the kernel has IPv6 but ip6tables cannot set
# policy, the allowlist is not actually enforced, so fail closed and say why.
if ip6tables -L >/dev/null 2>&1; then
  ip6tables -F
  ip6tables -P INPUT DROP
  ip6tables -P FORWARD DROP
  ip6tables -P OUTPUT DROP
  ip6tables -A OUTPUT -o lo -j ACCEPT
  ip6tables -A INPUT  -i lo -j ACCEPT
  echo "[firewall] IPv6 egress dropped"
elif [ -e /proc/net/if_inet6 ]; then
  echo "[firewall] FATAL: IPv6 is enabled in the kernel but ip6tables is unusable;"
  echo "[firewall]        refusing to run with an unfiltered IPv6 path."
  exit 1
else
  echo "[firewall] IPv6 not enabled in kernel; nothing to filter"
fi

# Smoke test: a non-allowlisted host must fail, GitHub must succeed.
echo "[firewall] verifying"
if curl -fsS --max-time 4 https://example.com >/dev/null 2>&1; then
  echo "[firewall] ERROR: example.com reachable (allowlist not enforced)"; exit 1
fi
if ! curl -fsS --max-time 6 https://api.github.com/zen >/dev/null 2>&1; then
  echo "[firewall] warn: api.github.com not reachable"
fi
echo "[firewall] up: default-deny with allowlist"
