#!/usr/bin/env bash
# Doc-consistency checker for the SNAP QC docs.
#
# Run this after logging a validated finding and before cutting a release/tag.
# It guards against the two drift modes we have actually hit:
#   1. em-dashes creeping back into the docs (a standing style rule), and
#   2. a claim retired in the findings docs still being asserted in a derived doc
#      (README / GUIDANCE), e.g. a strata claim retired in modeling_findings but
#      left standing in GUIDANCE.
#
# This is the cheap first pass; it does NOT replace the periodic reader-expectation
# audit (numbers-trace + completeness + right-altitude), which should still run at
# each validated-change/release. See the log-finding skill's "Propagate" step.
#
# Usage:  methods/check_doc_consistency.sh   (exits non-zero if any check fails)

set -u
cd "$(dirname "$0")/.." || exit 2   # repo root

fail=0

# Reader-facing docs that must contain no em-dashes.
READER_DOCS=(README.md GUIDANCE.md PIPELINE.md methods/modeling_findings.md methods/modeling_findings_detailed.md methods/findings_ledger.md methods/known_constraints.md)
# Derived docs that only CITE the findings; they must not assert a retired claim.
DERIVED_DOCS=(README.md GUIDANCE.md PIPELINE.md)

echo "== 1. em-dash check (docs must contain none) =="
for f in "${READER_DOCS[@]}"; do
  [ -f "$f" ] || { echo "  skip (missing): $f"; continue; }
  n=$(grep -c '—' "$f" || true)
  if [ "${n:-0}" -ne 0 ]; then
    echo "  FAIL: $f has $n em-dash(es)"; fail=1
  else
    echo "  ok:   $f"
  fi
done

echo "== 2. retired-claim check (derived docs must not assert a retired claim) =="
CLAIMS=methods/retired_claims.txt
if [ -f "$CLAIMS" ]; then
  found=0
  while IFS= read -r line; do
    case "$line" in ''|\#*) continue ;; esac   # skip blanks and comments
    for f in "${DERIVED_DOCS[@]}"; do
      [ -f "$f" ] || continue
      if grep -Fn -- "$line" "$f" >/dev/null 2>&1; then
        echo "  FAIL: $f asserts a retired claim: \"$line\""; fail=1; found=1
      fi
    done
  done < "$CLAIMS"
  [ "$found" -eq 0 ] && echo "  ok:   no retired phrasings found"
else
  echo "  (no $CLAIMS yet; skipping)"
fi

echo "== 3. GUIDANCE citation check (every bullet must cite a findings section) =="
if [ -f GUIDANCE.md ]; then
  # a bullet runs from '- **' to the next bullet or heading; each must contain a §
  read -r bad n <<EOF2
$(awk '
  function flush() { if (buf != "") { n++; if (buf !~ /§/) bad++ } ; buf = "" }
  /^- \*\*/ { flush(); buf = $0; next }
  /^#/      { flush(); next }
  { if (buf != "") buf = buf " " $0 }
  END { flush(); print bad+0, n+0 }
' GUIDANCE.md)
EOF2
  if [ "${bad:-0}" -ne 0 ]; then
    echo "  FAIL: $bad of $n GUIDANCE bullet(s) carry no § citation"; fail=1
  else
    echo "  ok:   all $n GUIDANCE bullets cite a findings section"
  fi
fi

echo
if [ "$fail" -ne 0 ]; then
  echo "RESULT: FAIL. Fix the above, then re-run."
  exit 1
fi
echo "RESULT: PASS. (Still run the full reader-expectation audit at each release.)"
