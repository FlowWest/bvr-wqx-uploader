@echo off
SETLOCAL EnableDelayedExpansion

:: BVR WQX Uploader Launcher (single-file distribution)
:: Double-click to launch the Shiny app (auto-updates from GitHub)
:: The app is installed into %LOCALAPPDATA%\BVR-WQX-Uploader\

SET "REPO=FlowWest/bvr-wqx-uploader"
SET "INSTALL_DIR=%LOCALAPPDATA%\BVR-WQX-Uploader"
SET "APP_DIR=%INSTALL_DIR%\app"
SET "VERSION_FILE=%INSTALL_DIR%\VERSION"

echo ============================================
echo   BVR WQX Uploader
echo ============================================
echo.

:: Create install directory if needed
IF NOT EXIST "%INSTALL_DIR%" mkdir "%INSTALL_DIR%"

:: Check for updates
echo Checking for updates...

:: Get latest release tag from GitHub API
curl -s "https://api.github.com/repos/%REPO%/releases/latest" > "%TEMP%\bvr-release.json" 2>nul
IF %ERRORLEVEL% NEQ 0 (
    echo Could not check for updates. Continuing with current version...
    GOTO :SkipUpdate
)

:: Parse tag_name from JSON
SET "LATEST_VERSION="
FOR /F "tokens=2 delims=:," %%a IN ('findstr "tag_name" "%TEMP%\bvr-release.json"') DO (
    SET "LATEST_VERSION=%%~a"
)
SET "LATEST_VERSION=%LATEST_VERSION: =%"
SET "LATEST_VERSION=%LATEST_VERSION:"=%"

IF "%LATEST_VERSION%"=="" (
    echo Could not determine latest version. Continuing with current version...
    GOTO :SkipUpdate
)

:: Get local version
SET "LOCAL_VERSION="
IF EXIST "%VERSION_FILE%" (
    SET /P LOCAL_VERSION=<"%VERSION_FILE%"
)

:: Compare versions
IF "%LATEST_VERSION%"=="%LOCAL_VERSION%" (
    echo You have the latest version ^(%LOCAL_VERSION%^)
    GOTO :SkipUpdate
)

IF "%LOCAL_VERSION%"=="" (
    echo First install: downloading %LATEST_VERSION%...
) ELSE (
    echo Update available: %LOCAL_VERSION% -^> %LATEST_VERSION%
)

:: Download release zip
echo Downloading...
SET "TEMP_ZIP=%TEMP%\bvr-wqx-update.zip"
SET "TEMP_EXTRACT=%TEMP%\bvr-wqx-extract"
curl -sL -o "%TEMP_ZIP%" "https://github.com/%REPO%/archive/refs/tags/%LATEST_VERSION%.zip"
IF %ERRORLEVEL% NEQ 0 (
    echo Download failed. Continuing with current version...
    GOTO :SkipUpdate
)

:: Extract zip
IF EXIST "%TEMP_EXTRACT%" rmdir /s /q "%TEMP_EXTRACT%"
mkdir "%TEMP_EXTRACT%"
tar -xf "%TEMP_ZIP%" -C "%TEMP_EXTRACT%" 2>nul
IF %ERRORLEVEL% NEQ 0 (
    echo Extraction failed. Continuing with current version...
    GOTO :SkipUpdate
)

:: Find extracted folder (e.g., bvr-wqx-uploader-v1.8.2)
SET "EXTRACTED_FOLDER="
FOR /D %%d IN ("%TEMP_EXTRACT%\*") DO SET "EXTRACTED_FOLDER=%%d"

:: Copy app folder
IF EXIST "%EXTRACTED_FOLDER%\app" (
    echo Installing update...
    IF EXIST "%APP_DIR%" rmdir /s /q "%APP_DIR%"
    xcopy "%EXTRACTED_FOLDER%\app" "%APP_DIR%\" /s /e /q /y >nul
)

:: Copy data folder
IF EXIST "%EXTRACTED_FOLDER%\data" (
    IF EXIST "%INSTALL_DIR%\data" rmdir /s /q "%INSTALL_DIR%\data"
    xcopy "%EXTRACTED_FOLDER%\data" "%INSTALL_DIR%\data\" /s /e /q /y >nul
)

:: Save version
echo|set /p="%LATEST_VERSION%" > "%VERSION_FILE%"

:: Cleanup temp files
del "%TEMP_ZIP%" 2>nul
rmdir /s /q "%TEMP_EXTRACT%" 2>nul
del "%TEMP%\bvr-release.json" 2>nul

echo Update complete!

:SkipUpdate
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
