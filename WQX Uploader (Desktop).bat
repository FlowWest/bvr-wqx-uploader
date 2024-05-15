@echo off
SETLOCAL
setlocal enabledelayedexpansion

REM Find R installation path
for /f "tokens=3*" %%a in ('reg query "HKEY_LOCAL_MACHINE\SOFTWARE\R-core\R" /v InstallPath ^| findstr "InstallPath"') do (
    set "RPath=%%a %%b"
)

REM Append "\bin\x64" to RPath
set "RPath=!RPath!\bin\x64"

REM Check if R directory is found
IF NOT DEFINED RPath (
    echo R directory not found.
    EXIT /B
) ELSE (
    echo Found R directory: !RPath!
)

REM Find the latest version of bvr-wqx-uploader
set "downloadsFolder=%USERPROFILE%\Downloads"
set "latestVersion="

for /f "tokens=*" %%a in ('dir /b /ad /o-n "%downloadsFolder%\bvr-wqx-uploader-*" 2^>nul') do (
    set "latestVersion=%%a"
    goto :VersionFound
)

:VersionFound
if not "%latestVersion%"=="" (
    cd "%downloadsFolder%\%latestVersion%\%latestVersion%"
    echo Latest version found: %latestVersion%
) else (
    echo No version of bvr-wqx-uploader found in the Downloads folder.
    EXIT /B
)


cd app

REM Install dependencies and launch R Shiny app
echo !RPath!
"!RPath!\RScript.exe" install-deps.R
"!RPath!\RScript.exe" python-setup.R
"!RPath!\R.exe" -e "shiny::runApp('.', launch.browser = TRUE)"
pause

ENDLOCAL