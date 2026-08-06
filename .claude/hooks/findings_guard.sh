#!/usr/bin/env bash
# findings_guard.sh: puts the repo's measured constraints in front of the model
# at the moment it edits a protected pipeline file or creates a study script.
#
# PreToolUse (Edit|Write): looks up the file in methods/known_constraints.md
#   and injects the matching section as additionalContext. Full text once per
#   session per file, a one-line pointer on repeats.
# PostToolUse (Edit|Write): after any edit to rule_mining_helpers.R, reminds
#   that the 27-check regression test is owed.
#
# Fails safe: on any parsing problem it stays silent and exits 0. It never
# blocks a tool call.

set -u

INPUT=$(cat 2>/dev/null) || INPUT=""

first_field() {
  printf '%s' "$INPUT" | grep -o "\"$1\"[[:space:]]*:[[:space:]]*\"[^\"]*\"" \
    | head -n 1 | sed 's/^"[^"]*"[[:space:]]*:[[:space:]]*"//; s/"$//'
}

EVENT=$(first_field hook_event_name)
TOOL=$(first_field tool_name)
SESSION=$(first_field session_id)
FILE=$(first_field file_path)

[ -n "$FILE" ] || exit 0
[ -n "$SESSION" ] || SESSION=nosession

# JSON gives Windows paths with escaped backslashes; normalize to forward slashes.
FILE=$(printf '%s' "$FILE" | tr '\\' '/' | sed 's#//*#/#g')
BASE=${FILE##*/}

ROOT=${CLAUDE_PROJECT_DIR:-}
if [ -z "$ROOT" ]; then
  ROOT=$(cd "$(dirname "$0")/../.." 2>/dev/null && pwd) || exit 0
fi
CF="$ROOT/methods/known_constraints.md"
[ -f "$CF" ] || exit 0

anchor=""
case "$BASE" in
  rule_mining_helpers.R) anchor="rule-mining-helpers" ;;
  INCL_find_inclusion_rules_by_hh_size_v2.R) anchor="incl-finder" ;;
  EXCL_find_exclusion_rules_by_hh_size_v2.R) anchor="excl-finder" ;;
  INCL_build_blended_delivery_list_v2.R) anchor="delivery-builder" ;;
  1_data_munging_and_raw_variable_reconstruction_for_using_public_qc_data.R) anchor="munging" ;;
  add_refill_metrics_v2.R) anchor="refill-metrics" ;;
esac

# A Write of an R or Python script under methods/ or runners/ is (usually) a
# new study coming into existence: inject the planning gate.
if [ -z "$anchor" ] && [ "$TOOL" = "Write" ]; then
  case "$FILE" in
    */methods/legacy_exploration/*) : ;;
    */methods/*.R|*/methods/*.py|*/runners/*.R) anchor="new-study" ;;
  esac
fi

emit() {
  # $1 = text to inject; escapes it into a JSON string.
  ctx=$(printf '%s' "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' | awk '{printf "%s\\n", $0}')
  printf '{"hookSpecificOutput":{"hookEventName":"%s","additionalContext":"%s"}}' "$EVENT" "$ctx"
}

section_text() {
  awk -v tag="{#$1}" '
    inb && /^## / { exit }
    index($0, tag) > 0 && /^## / { inb = 1 }
    inb { print }
  ' "$CF"
}

if [ "$EVENT" = "PostToolUse" ]; then
  if [ "$BASE" = "rule_mining_helpers.R" ]; then
    emit "rule_mining_helpers.R was modified. Before this change is done, run the regression test and get 27 of 27 PASS: Rscript methods/test_rule_mining_helpers.R"
  fi
  exit 0
fi

[ "$EVENT" = "PreToolUse" ] || exit 0
[ -n "$anchor" ] || exit 0

MARKDIR="${TMPDIR:-/tmp}/claude_findings_guard"
mkdir -p "$MARKDIR" 2>/dev/null || exit 0
MARK="$MARKDIR/$SESSION.$anchor"

if [ -e "$MARK" ]; then
  emit "Reminder: measured constraints apply to this file; they were shown earlier this session. Source: methods/known_constraints.md#$anchor"
  exit 0
fi

TEXT=$(section_text "$anchor")
if [ -n "$TEXT" ]; then
  : > "$MARK" 2>/dev/null
  emit "MEASURED CONSTRAINTS for the file being modified (from methods/known_constraints.md; each line cites its finding). These are settled by experiment; do not relax one without a new finding.

Routing (methods/known_constraints.md#routing): work in this file is written under the principal-data-scientist skill (invoke it now if this session has not), and the change is reviewed by a fresh senior-statistician subagent before any run or commit.

$TEXT"
fi
exit 0
