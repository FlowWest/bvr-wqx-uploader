@echo off
SETLOCAL
setlocal enabledelayedexpansion

@echo off
SETLOCAL

SET "RPath="

FOR %%G IN (
    "C:\Program Files\R\R-4.4.0\bin\x64",
    "C:\Users\%USERNAME%\AppData\Local\Programs\R\R-4.3.2\bin\x64",
    "C:\Users\%USERNAME%\Documents\R\R-4.1.2\bin\x64",
    "C:\Program Files\R\R-4.3.0\bin\x64",
    "C:\Program Files\R\R-4.3.2\bin\x64",
    "C:\Program Files\R\R-4.2.1\bin\x64"
) DO (
    IF EXIST %%G (
        SET "RPath=%%G"
        GOTO Found
    )
)

:Found
IF NOT DEFINED RPath (
    echo R directory not found.
) ELSE (
    echo Found R directory: %RPath%
)


set "downloadsFolder=%USERPROFILE%\Downloads"
set "latestVersion="

for /f "tokens=*" %%a in ('powershell -command "Get-ChildItem -Directory -Path %downloadsFolder% | Where-Object { $_.Name -match '^bvr-wqx-uploader-' } | Sort-Object { [Version]($_.Name -replace '^bvr-wqx-uploader-', '') } -Descending | Select-Object -First 1 | ForEach-Object { $_.Name }"') do (
    set "latestVersion=%%a"
    goto :found
)

:found
if not "%latestVersion%"=="" (
    cd "%downloadsFolder%\%latestVersion%\%latestVersion%"
    echo Latest version found: %latestVersion%
) else (
    echo No version of bvr-wqx-uploader found in the Downloads folder.
)


cd app

%Rpath%\RScript.exe install-deps.R
%Rpath%\RScript.exe python-setup.R
%Rpath%\R.exe -e "shiny::runApp('.', launch.browser = TRUE)"
pause

ENDLOCAL