# snap_qc dev container

A reproducible R 4.5.1 + Python environment for the v2 rule-mining pipeline,
so the work survives a machine restart or a new session and an agent can run
with far fewer permission prompts. Adapted from the Georgetown MDI `jspsi`
dev container, translated to this project's R/Python/deck toolchain.

## What's inside

- **R 4.5.1** (the exact host version) with `dplyr`, `ggplot2`, `ranger`,
  `xgboost`, `rpart`, `haven`, `scales` — everything the v2 pipeline uses —
  plus `here`, `tidyr`, and `yardstick`, which the munging script loads.
  (The v1 legacy engines {pre}, {C50}, and {xrf} are omitted; add them only
  to run v1 scripts.)
- **Python 3** with `python-pptx` for the deck scripts (the analysis scripts
  use only the stdlib).
- **LibreOffice** (headless) to render `.pptx` slides to PDF/PNG — the Linux
  stand-in for the Windows PowerPoint automation used on the host.
- **git, gh, Node 22, claude-code**, and the iptables/ipset egress firewall.
  Node comes from NodeSource, not the base distro: the distro ships Node 18
  and claude-code requires >= 22.

## Fewer approvals, safely

Inside the container Claude Code runs in `bypassPermissions` mode
(`post-create.sh` writes it). That is safe here only because
`init-firewall.sh` restricts outbound traffic to a fixed allowlist — the
Anthropic API, GitHub, CRAN/Posit, PyPI, npm — at container start. If you
remove the firewall (`postStartCommand`), remove `bypassPermissions` too.

Two things the allowlist deliberately lets through, worth knowing about:

- **The Docker host gateway**, on all ports, so the IDE/agent bridge works.
  Anything listening on your host — a local database, another dev server — is
  reachable from inside the container.
- **DNS to the container's configured resolvers**, needed for name
  resolution. It is scoped to the resolvers in `/etc/resolv.conf` rather than
  to any host, so it is not a general tunnel out.

The allowlist pins IP addresses resolved at container start, so CDN-backed
hosts drift as their addresses rotate. If package installs start failing on a
long-running container, re-run `sudo /usr/local/bin/init-firewall.sh`.

On the **host** (outside the container), approvals are reduced instead by
`.claude/settings.local.json` (gitignored), which allow-lists the safe,
high-frequency read-only commands.

## Data (important)

The pipeline reads two things:

1. **`reg_model_data.rds`** at the workspace root — the built modelling frame,
   gitignored and NOT in the image. It rides in through the workspace bind
   mount, so if it's present on the host it's present in the container.
   **Almost all current work** (auditions, sweeps, benchmarks, delivery
   builds) reads only this file.
2. **Raw QC `.sav` files** at `qc_data/` — needed only to *rebuild*
   `reg_model_data.rds` from scratch. These are tracked in the repo, so they
   arrive through the workspace bind mount at `/workspace/qc_data` with no
   extra setup. The munging script resolves its paths with `here()`, which is
   `/workspace` in this container, so an in-container rebuild just works.

The `QC_DATA_DIR` env var and the `/data/qc` bind mount predate the `here()`
change and are now vestigial — nothing reads them. They are left in place
because they cost nothing; ignore them unless you keep raw data outside the
repo, in which case symlink it to `/workspace/qc_data`.

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
