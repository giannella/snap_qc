# snap_qc dev container

A reproducible R 4.5.1 + Python environment for the v2 rule-mining pipeline,
so the work survives a machine restart or a new session and an agent can run
with far fewer permission prompts. Adapted from the Georgetown MDI `jspsi`
dev container, translated to this project's R/Python/deck toolchain.

## What's inside

- **R 4.5.1** (the exact host version) with `dplyr`, `ggplot2`, `ranger`,
  `xgboost`, `rpart`, `haven`, `scales` — everything the v2 pipeline uses.
  ({pre}, the v1 legacy engine, is omitted; add it only to run v1 scripts.)
- **Python 3** with `python-pptx` for the deck scripts (the analysis scripts
  use only the stdlib).
- **LibreOffice** (headless) to render `.pptx` slides to PDF/PNG — the Linux
  stand-in for the Windows PowerPoint automation used on the host.
- **git, gh, node, claude-code**, and the iptables/ipset egress firewall.

## Fewer approvals, safely

Inside the container Claude Code runs in `bypassPermissions` mode
(`post-create.sh` writes it). That is safe here only because
`init-firewall.sh` restricts outbound traffic to a fixed allowlist — the
Anthropic API, GitHub, CRAN/Posit, PyPI, npm — at container start. If you
remove the firewall (`postStartCommand`), remove `bypassPermissions` too.

On the **host** (outside the container), approvals are reduced instead by
`.claude/settings.local.json` (gitignored), which allow-lists the safe,
high-frequency read-only commands.

## Data (important)

The pipeline reads two things, both gitignored and NOT in the image:

1. **`reg_model_data.rds`** at the workspace root — the built modelling frame.
   It rides in through the workspace bind mount, so if it's present on the
   host it's present in the container. **Almost all current work** (auditions,
   sweeps, benchmarks, delivery builds) reads only this file.
2. **Raw QC `.sav` files** at `C:/Users/ericg/qc/` on the host — needed only
   to *rebuild* `reg_model_data.rds` from scratch. Mount them by setting
   `QC_DATA_DIR` on the host to that folder before opening the container; they
   appear at `/data/qc`. **Caveat:** the munging script
   (`1_data_munging_..._public_qc_data.R`) hardcodes `C:/Users/ericg/qc/`
   paths, so an in-container rebuild needs those paths parameterized to
   `$QC_DATA_DIR` first. Until then, treat the raw rebuild as a host task and
   copy the resulting `reg_model_data.rds` in.

## Resuming long jobs

The R sweep/mine scripts checkpoint per frame/pool to `.rds` under gitignored
cache folders, so a killed run resumes from the last checkpoint on re-launch —
the container just makes the *environment* durable too. See `RESUME.md` for
what's running and how to restart each study.

## GitHub auth

Put a token in `.devcontainer/.env` as `GH_TOKEN=...` (the file is created
empty on first open and is gitignored) and `post-create.sh` wires up
`git push`/`gh`. Otherwise run `gh auth login` once inside the container.

## Rendering slides

`soffice --headless --convert-to pdf slides/lessons_*.pptx` then rasterize a page,
or `--convert-to png`. This is a rough preview, not identical to PowerPoint;
final deck checks still belong on a machine with PowerPoint.
