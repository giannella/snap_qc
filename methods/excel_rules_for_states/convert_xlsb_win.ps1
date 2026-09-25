# Save a finished workbook as Excel Binary (.xlsb) via desktop Excel COM.
# The .xlsx stays the verified source; the .xlsb is a second copy of the same
# workbook (formulas, tables, validation and hidden sheets all carry over),
# smaller and faster to open. Excel recalculates before saving, so the .xlsb
# carries cached values (no blank cells in Protected View).
#
# Usage:  powershell -File convert_xlsb_win.ps1 <workbook.xlsx> [<out.xlsb>]
#
# Exits non-zero if the file fails to open or the save fails.
param(
    [Parameter(Mandatory = $true, Position = 0)][string]$Workbook,
    [Parameter(Position = 1)][string]$Out
)

$ErrorActionPreference = 'Stop'
$Workbook = (Resolve-Path $Workbook).Path
if (-not $Out) { $Out = [System.IO.Path]::ChangeExtension($Workbook, '.xlsb') }
$Out = [System.IO.Path]::GetFullPath($Out)
$xlExcel12 = 50   # xlsb
$failed = $false
$xl = $null
$wb = $null
try {
    $xl = New-Object -ComObject Excel.Application
    $xl.Visible = $false
    $xl.DisplayAlerts = $false
    $xl.AskToUpdateLinks = $false
    $wb = $xl.Workbooks.Open($Workbook, 0, $true)   # read-only source
    $xl.CalculateFullRebuild()
    while ($xl.CalculationState -ne 0) { Start-Sleep -Milliseconds 200 }
    if (Test-Path $Out) { Remove-Item $Out -Force }
    $wb.SaveAs($Out, $xlExcel12)
    $n = (Get-Item $Out).Length
    "saved $(Split-Path $Out -Leaf) ($([math]::Round($n / 1MB, 1)) MB)"
} catch {
    "CONVERT FAILED: $($_.Exception.Message)"
    $failed = $true
} finally {
    if ($wb) { $wb.Close($false) }
    if ($xl) { $xl.Quit(); [void][Runtime.InteropServices.Marshal]::ReleaseComObject($xl) }
}
if ($failed) { exit 1 } else { exit 0 }
