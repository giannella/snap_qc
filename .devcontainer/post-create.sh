#!/usr/bin/env bash
# Runs once, after the container is first built. Verifies the R/Python
# toolchain, turns off Claude Code permission prompts inside the container
# (safe here because the egress firewall in init-firewall.sh confines what the
# agent can reach), wires up git auth from a token if present, and reports
# whether the data needed to run the pipeline is mounted.
set -euo pipefail

echo "== snap_qc dev container: post-create =="

# --- R toolchain check ---------------------------------------------------
Rscript -e 'pkgs <- c("dplyr","ggplot2","ranger","xgboost","rpart","haven","scales");
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)];
  if (length(miss)) stop("missing R packages: ", paste(miss, collapse=", "));
  cat("R", as.character(getRversion()), "OK; packages present:", paste(pkgs, collapse=", "), "\n")'

# --- Python deck tooling check -------------------------------------------
python3 -c 'import pptx; print("python-pptx", pptx.__version__, "OK")' || echo "WARN: python-pptx missing"

# --- Claude Code: skip permission prompts in-container --------------------
# The firewall is the security boundary here, not the prompts.
CLAUDE_DIR="${CLAUDE_CONFIG_DIR:-$HOME/.claude}"
mkdir -p "$CLAUDE_DIR"
SETTINGS="$CLAUDE_DIR/settings.json"
if [ ! -f "$SETTINGS" ]; then
  echo '{ "permissions": { "defaultMode": "bypassPermissions" } }' > "$SETTINGS"
  echo "wrote $SETTINGS (bypassPermissions)"
fi
# Pre-seed onboarding so the first session does not stop on welcome/trust dialogs.
DOTCLAUDE="$HOME/.claude.json"
[ -f "$DOTCLAUDE" ] || echo '{ "hasCompletedOnboarding": true }' > "$DOTCLAUDE"

# --- git auth from a token, if one was passed via .devcontainer/.env ------
TOKEN="${GH_TOKEN:-${GITHUB_TOKEN:-}}"
if [ -n "$TOKEN" ]; then
  git config --global credential.helper store
  printf 'https://x-access-token:%s@github.com\n' "$TOKEN" > "$HOME/.git-credentials"
  chmod 600 "$HOME/.git-credentials"
  echo "git credential helper configured from token"
else
  echo "no GH_TOKEN/GITHUB_TOKEN in .devcontainer/.env; run 'gh auth login' for pushes"
fi

# --- data presence report ------------------------------------------------
echo
if [ -f /workspace/reg_model_data.rds ]; then
  echo "reg_model_data.rds present -> all v2 auditions/sweeps/benchmarks can run."
else
  echo "reg_model_data.rds MISSING. Copy it into the workspace root, or rebuild it"
  echo "  from the raw QC files (see note below)."
fi
if [ -d /data/qc/qc_data ] && ls /data/qc/qc_data/*.sav >/dev/null 2>&1; then
  echo "raw QC .sav files mounted at /data/qc/qc_data."
  echo "NOTE: the munging script (1_data_munging_...R) hardcodes Windows paths"
  echo "  (C:/Users/ericg/qc/); rebuilding reg_model_data in-container needs those"
  echo "  paths parameterized to \$QC_DATA_DIR first. See .devcontainer/README.md."
else
  echo "raw QC data not mounted (set QC_DATA_DIR on the host to your qc/ folder)."
  echo "  Not needed for work that reads reg_model_data.rds, which is almost all of it."
fi
echo
echo "== post-create done. See RESUME.md for where the work stands. =="
