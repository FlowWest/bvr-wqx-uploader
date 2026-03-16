@echo off
SETLOCAL EnableDelayedExpansion

:: BVR WQX Uploader Launcher (single-file distribution)
:: Double-click to launch the Shiny app (auto-updates from GitHub)
:: The app is installed into %LOCALAPPDATA%\BVR-WQX-Uploader\

SET "INSTALL_DIR=%LOCALAPPDATA%\BVR-WQX-Uploader"
SET "APP_DIR=%INSTALL_DIR%\app"
SET "VERSION_FILE=%INSTALL_DIR%\VERSION"

echo ============================================
echo   BVR WQX Uploader
echo ============================================
echo.

:: Create install directory if needed
IF NOT EXIST "%INSTALL_DIR%" (
    mkdir "%INSTALL_DIR%"
)

:: Check for updates using inline PowerShell
echo Checking for updates...
powershell -ExecutionPolicy Bypass -Command ^
 "$ErrorActionPreference = 'Stop'; " ^
 "$ProgressPreference = 'SilentlyContinue'; " ^
 "$Repo = 'FlowWest/bvr-wqx-uploader'; " ^
 "$AppDir = '%APP_DIR%'; " ^
 "$VersionFile = '%VERSION_FILE%'; " ^
 "try { " ^
 "  [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12; " ^
 "  $release = Invoke-RestMethod -Uri \"https://api.github.com/repos/$Repo/releases/latest\" -TimeoutSec 10; " ^
 "  $latestVersion = $release.tag_name; " ^
 "  $localVersion = ''; " ^
 "  if (Test-Path $VersionFile) { $localVersion = (Get-Content $VersionFile -Raw).Trim() }; " ^
 "  if ($latestVersion -ne $localVersion) { " ^
 "    if ($localVersion -eq '') { Write-Host \"First install: downloading $latestVersion...\" } " ^
 "    else { Write-Host \"Update available: $localVersion -> $latestVersion\" }; " ^
 "    Write-Host 'Downloading...'; " ^
 "    $zipUrl = $release.zipball_url; " ^
 "    $tempZip = Join-Path $env:TEMP 'bvr-wqx-update.zip'; " ^
 "    $tempExtract = Join-Path $env:TEMP 'bvr-wqx-extract'; " ^
 "    Invoke-WebRequest -Uri $zipUrl -OutFile $tempZip -TimeoutSec 120; " ^
 "    if (Test-Path $tempExtract) { Remove-Item $tempExtract -Recurse -Force }; " ^
 "    Expand-Archive -Path $tempZip -DestinationPath $tempExtract -Force; " ^
 "    $extractedFolder = Get-ChildItem $tempExtract -Directory | Select-Object -First 1; " ^
 "    $sourceApp = Join-Path $extractedFolder.FullName 'app'; " ^
 "    if (Test-Path $sourceApp) { " ^
 "      Write-Host 'Installing update...'; " ^
 "      if (Test-Path $AppDir) { Remove-Item $AppDir -Recurse -Force }; " ^
 "      Copy-Item $sourceApp -Destination $AppDir -Recurse " ^
 "    }; " ^
 "    $sourceData = Join-Path $extractedFolder.FullName 'data'; " ^
 "    $destData = Join-Path (Split-Path $AppDir -Parent) 'data'; " ^
 "    if (Test-Path $sourceData) { " ^
 "      if (Test-Path $destData) { Remove-Item $destData -Recurse -Force }; " ^
 "      Copy-Item $sourceData -Destination $destData -Recurse " ^
 "    }; " ^
 "    $latestVersion | Out-File -FilePath $VersionFile -NoNewline -Encoding ASCII; " ^
 "    Remove-Item $tempZip -Force -ErrorAction SilentlyContinue; " ^
 "    Remove-Item $tempExtract -Recurse -Force -ErrorAction SilentlyContinue; " ^
 "    Write-Host 'Update complete!' " ^
 "  } else { Write-Host \"You have the latest version ($localVersion)\" } " ^
 "} catch { Write-Host \"Could not check for updates: $_\"; Write-Host 'Continuing with current version...' }"

echo.

:: Verify app was installed
IF NOT EXIST "%APP_DIR%" (
    echo.
    echo ERROR: App not found at %APP_DIR%
    echo Please ensure you have internet connectivity and try again.
    echo.
    pause
    EXIT /B 1
)

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
