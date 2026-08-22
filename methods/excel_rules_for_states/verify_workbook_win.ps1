# Stage 3 (Windows) — open the finished workbook in desktop Excel via COM,
# force a full recalculation, read probe cells back, scan for formula errors,
# and close without saving. Excel is the only authority on whether an xlsx is
# really valid; openpyxl can neither evaluate formulas nor detect a file Excel
# would offer to "repair".
#
# Usage:  powershell -File verify_workbook_win.ps1 <workbook.xlsx> ["Sheet!Cell" ...]
#
# Exits non-zero if the file fails to open, a probe cannot be read, or any
# visible sheet contains formula error values (#REF!, #NAME?, #DIV/0!, ...).
param(
    [Parameter(Mandatory = $true, Position = 0)][string]$Workbook,
    [Parameter(ValueFromRemainingArguments = $true)][string[]]$Probes
)

$ErrorActionPreference = 'Stop'
$Workbook = (Resolve-Path $Workbook).Path
$failed = $false
$xl = $null
$wb = $null
try {
    $xl = New-Object -ComObject Excel.Application
    $xl.Visible = $false
    $xl.DisplayAlerts = $false
    $xl.AskToUpdateLinks = $false
    $wb = $xl.Workbooks.Open($Workbook, 0, $true)   # read-only; never saves
    $xl.CalculateFullRebuild()
    while ($xl.CalculationState -ne 0) { Start-Sleep -Milliseconds 200 }  # 0 = xlDone

    # A probe may carry an expectation: "Sheet!Cell=value". The live value
    # must equal it (numeric compare when both parse as numbers). This is the
    # guard for SILENT formula collapse (2026-08-22: an in-row array formula
    # evaluated to a clean 0 on every row, the error scan passed, and only
    # the union count revealed it); the expectation is the plain build's
    # static figure, which the R cross-check independently verifies.
    foreach ($p in $Probes) {
        $expect = $null
        $spec = $p
        $eq = $p.LastIndexOf('=')
        if ($eq -gt $p.LastIndexOf('!')) { $expect = $p.Substring($eq + 1); $spec = $p.Substring(0, $eq) }
        $i = $spec.LastIndexOf('!')
        $sheet = $spec.Substring(0, $i); $cell = $spec.Substring($i + 1)
        try {
            $v = $wb.Worksheets.Item($sheet).Range($cell).Text
            if ($null -ne $expect) {
                $vn = 0.0; $en = 0.0
                $ok = if ([double]::TryParse($v, [ref]$vn) -and [double]::TryParse($expect, [ref]$en)) { [math]::Abs($vn - $en) -lt 1e-6 } else { $v -eq $expect }
                if ($ok) { "probe  {0,-28} = {1}  (== expected)" -f $spec, $v }
                else { "PROBE MISMATCH  $spec = $v  (expected $expect)"; $failed = $true }
            } else {
                "probe  {0,-28} = {1}" -f $spec, $v
            }
        } catch {
            "PROBE FAILED  $spec : $($_.Exception.Message)"
            $failed = $true
        }
    }

    # error scan: every formula cell evaluating to an error, per sheet
    $xlFormulas = -4123; $xlErrors = 16
    foreach ($ws in $wb.Worksheets) {
        $n = 0
        try {
            $bad = $ws.UsedRange.SpecialCells($xlFormulas, $xlErrors)
            $n = $bad.Count
            $addr = $bad.Address($false, $false)
            if ($addr.Length -gt 120) { $addr = $addr.Substring(0, 120) + '...' }
        } catch { $n = 0 }   # SpecialCells throws when nothing matches
        if ($n -gt 0) {
            "ERRORS  sheet '$($ws.Name)': $n formula error cell(s) at $addr"
            $failed = $true
        }
    }
    if (-not $failed) { "verified OK: $(Split-Path $Workbook -Leaf)" }
} catch {
    "OPEN FAILED: $($_.Exception.Message)"
    $failed = $true
} finally {
    if ($wb) { $wb.Close($false) }
    if ($xl) { $xl.Quit(); [void][Runtime.InteropServices.Marshal]::ReleaseComObject($xl) }
}
if ($failed) { exit 1 } else { exit 0 }
