# Development container for snap_qc. R 4.5.1 (pinned to the host toolchain the
# project has always used) on the rocker/r-ver base, plus Python 3 for the deck
# and analysis scripts, LibreOffice for headless slide rendering (the Linux
# stand-in for the Windows PowerPoint COM automation we used on the host), and
# the git/gh/node/claude-code stack so an agent session runs entirely inside.
# It carries the iptables/ipset toolchain the egress firewall installs at
# startup, which is what lets the container run with permission prompts turned
# off safely.
FROM rocker/r-ver:4.5.1

ARG TZ=America/New_York
ENV TZ="$TZ"
ARG CLAUDE_CODE_VERSION=latest

# Base tools; the R packages' system build/runtime dependencies (libcurl,
# libssl, libxml2 for the tidyverse/haven/xml stack); Python 3 + venv/pip for
# the deck (python-pptx) and analysis scripts; LibreOffice for headless
# pptx->pdf/png rendering; and the iptables/ipset/dns toolchain the egress
# firewall uses (aggregate collapses the GitHub CIDR list, jq parses the meta
# feed, dnsutils provides dig, iproute2 provides ip).
RUN apt-get update && apt-get install -y --no-install-recommends \
  less git procps sudo fzf zsh man-db unzip gnupg2 curl ca-certificates \
  nano vim \
  libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev \
  python3 python3-pip python3-venv \
  libreoffice-impress libreoffice-core \
  nodejs npm \
  iptables ipset iproute2 dnsutils aggregate jq \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

# The GitHub CLI is not in the base repos; add its official signed apt source.
RUN mkdir -p -m 755 /etc/apt/keyrings \
  && curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg -o /etc/apt/keyrings/githubcli-archive-keyring.gpg \
  && chmod go+r /etc/apt/keyrings/githubcli-archive-keyring.gpg \
  && echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" > /etc/apt/sources.list.d/github-cli.list \
  && apt-get update && apt-get install -y --no-install-recommends gh \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

# R packages: the v2 pipeline on the first line, the munging script's extra
# dependencies on the second ({here} resolves the data paths, {tidyr} reshapes
# the standard-medical-deduction lookup, {yardstick} is loaded at its top).
# rocker configures the Posit binary repo, so these install as precompiled
# binaries (minutes, not a from-source build). The v1 legacy engines ({pre},
# {C50}, {xrf}) are intentionally omitted -- they are heavy and no current work
# needs them; add them if you must run the v1 scripts.
RUN install2.r --error --skipinstalled \
  dplyr ggplot2 ranger xgboost rpart haven scales \
  here tidyr yardstick \
  && rm -rf /tmp/downloaded_packages

# Python deck tooling (python-pptx); the analysis scripts use only the stdlib.
RUN pip3 install --no-cache-dir --break-system-packages python-pptx

# Non-root user with a writable global npm prefix so claude-code installs and
# self-updates without root, and its bin lands on the login-shell PATH.
RUN useradd -m -s /bin/zsh -u 1000 dev \
  && mkdir -p /usr/local/share/npm-global \
  && chown -R dev:dev /usr/local/share/npm-global \
  && echo 'export PATH="$PATH:/usr/local/share/npm-global/bin"' > /etc/profile.d/npm-global-path.sh

ENV DEVCONTAINER=true
RUN mkdir -p /workspace /home/dev/.claude && chown -R dev:dev /workspace /home/dev/.claude
WORKDIR /workspace
USER dev

ENV NPM_CONFIG_PREFIX=/usr/local/share/npm-global
ENV PATH=$PATH:/usr/local/share/npm-global/bin
ENV SHELL=/bin/zsh EDITOR=vim VISUAL=vim

RUN npm install -g @anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}

# Install the egress firewall and grant `dev` passwordless sudo for that one
# command, so devcontainer.json can apply it on start.
COPY init-firewall.sh /usr/local/bin/
USER root
RUN chmod +x /usr/local/bin/init-firewall.sh \
  && echo "dev ALL=(root) NOPASSWD: /usr/local/bin/init-firewall.sh" > /etc/sudoers.d/dev-firewall \
  && chmod 0440 /etc/sudoers.d/dev-firewall
USER dev
