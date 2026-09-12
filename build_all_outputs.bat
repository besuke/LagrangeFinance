@echo off
cd /d C:\AnalyticFin\Projects\LagrangeFinance

echo.
echo === LagrangeFinance Full Build ===
echo.

powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0build_all_outputs.ps1" -Open

if errorlevel 1 (
    echo.
    echo === BUILD FAILED ===
    pause
    exit /b 1
)

echo.
echo === ALL COMPLETE ===
pause