@echo off
SETLOCAL EnableDelayedExpansion

:: BVR WQX Uploader Launcher
:: Double-click to launch the Shiny app (auto-updates from GitHub)

SET "REPO=FlowWest/bvr-wqx-uploader"
SET "APP_DIR=%~dp0app"
SET "VERSION_FILE=%~dp0VERSION"

echo ============================================
echo   BVR WQX Uploader
echo ============================================
echo.

:: Check for updates using PowerShell
echo Checking for updates...
powershell -ExecutionPolicy Bypass -Command ^
    "$ErrorActionPreference = 'Stop'; ^
    try { ^
        $release = Invoke-RestMethod -Uri 'https://api.github.com/repos/%REPO%/releases/latest' -TimeoutSec 10; ^
        $latestVersion = $release.tag_name; ^
        $localVersion = ''; ^
        if (Test-Path '%VERSION_FILE%') { $localVersion = (Get-Content '%VERSION_FILE%' -Raw).Trim() }; ^
        if ($latestVersion -ne $localVersion) { ^
            Write-Host \"Update available: $localVersion -> $latestVersion\"; ^
            Write-Host 'Downloading update...'; ^
            $zipUrl = $release.zipball_url; ^
            $tempZip = Join-Path $env:TEMP 'bvr-wqx-update.zip'; ^
            $tempExtract = Join-Path $env:TEMP 'bvr-wqx-extract'; ^
            Invoke-WebRequest -Uri $zipUrl -OutFile $tempZip -TimeoutSec 120; ^
            if (Test-Path $tempExtract) { Remove-Item $tempExtract -Recurse -Force }; ^
            Expand-Archive -Path $tempZip -DestinationPath $tempExtract -Force; ^
            $extractedFolder = Get-ChildItem $tempExtract | Select-Object -First 1; ^
            $sourceApp = Join-Path $extractedFolder.FullName 'app'; ^
            if (Test-Path $sourceApp) { ^
                Write-Host 'Installing update...'; ^
                if (Test-Path '%APP_DIR%') { Remove-Item '%APP_DIR%' -Recurse -Force }; ^
                Copy-Item $sourceApp -Destination '%APP_DIR%' -Recurse; ^
            }; ^
            $latestVersion | Out-File -FilePath '%VERSION_FILE%' -NoNewline -Encoding ASCII; ^
            Remove-Item $tempZip -Force; ^
            Remove-Item $tempExtract -Recurse -Force; ^
            Write-Host 'Update complete!'; ^
        } else { ^
            Write-Host \"You have the latest version ($localVersion)\"; ^
        } ^
    } catch { ^
        Write-Host \"Could not check for updates: $_\"; ^
        Write-Host 'Continuing with current version...'; ^
    }"

echo.

:: Find R installation dynamically
SET "RPath="

:: Check common R installation locations (newest versions first)
FOR %%V IN (4.5.2 4.5.1 4.5.0 4.4.2 4.4.1 4.4.0 4.3.3 4.3.2 4.3.1 4.3.0 4.2.3 4.2.2 4.2.1 4.2.0 4.1.3 4.1.2) DO (
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

:: Change to app directory
cd /d "%APP_DIR%"

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
