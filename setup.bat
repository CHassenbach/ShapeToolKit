@echo off
echo === HaugShape v2 Setup ===
echo.
echo This will install all required R packages for HaugShape v2
echo Press any key to continue, or Ctrl+C to cancel
pause >nul

echo.
echo Starting R installation script...
echo.

"C:\Program Files\R\R-4.3.1\bin\x64\R.exe" --no-save --no-restore -e "setwd('%CD%'); source('setup.R')"

echo.
echo Setup complete! Press any key to exit.
pause >nul