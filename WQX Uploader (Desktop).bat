@echo off
SETLOCAL EnableDelayedExpansion

:: BVR WQX Uploader Launcher
:: Double-click to launch the Shiny app

:: Find R installation dynamically
SET "RPath="

:: Check common R installation locations (newest versions first)
FOR %%V IN (4.5.0 4.4.2 4.4.1 4.4.0 4.3.3 4.3.2 4.3.1 4.3.0 4.2.3 4.2.2 4.2.1 4.2.0 4.1.3 4.1.2) DO (
    IF EXIST "C:\Program Files\R\R-%%V\bin\x64\Rscript.exe" (
        SET "RPath=C:\Program Files\R\R-%%V\bin\x64"
        GOTO :FoundR
    )
    IF EXIST "C:\Users\%USERNAME%\AppData\Local\Programs\R\R-%%V\bin\x64\Rscript.exe" (
        SET "RPath=C:\Users\%USERNAME%\AppData\Local\Programs\R\R-%%V\bin\x64"
        GOTO :FoundR
    )
)

:: Check if R is in PATH
WHERE Rscript.exe >nul 2>&1
IF %ERRORLEVEL% EQU 0 (
    FOR /F "tokens=*" %%i IN ('WHERE Rscript.exe') DO (
        SET "RPath=%%~dpi"
        GOTO :FoundR
    )
)

echo.
echo ERROR: R installation not found.
echo Please install R from https://cran.r-project.org/
echo.
pause
EXIT /B 1

:FoundR
echo Found R at: %RPath%
echo.

:: Change to app directory (same folder as this batch file)
cd /d "%~dp0app"

:: Install dependencies if needed (first run)
echo Checking dependencies...
"%RPath%\Rscript.exe" install-deps.R

:: Launch the app
echo.
echo Starting WQX Uploader...
echo (A browser window will open shortly)
echo.
"%RPath%\R.exe" -e "shiny::runApp('.', launch.browser = TRUE)"

ENDLOCAL
