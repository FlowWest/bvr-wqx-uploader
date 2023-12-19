@echo off
SETLOCAL

REM Function to find the Git executable
FOR %%I IN (
    "C:\Program Files\Git\cmd\git.exe"
    "C:\Program Files\Git\bin\git.exe"
    "C:\Program Files\Git\mingw64\bin\git.exe"
    "C:\Program Files\Git\mingw32\bin\git.exe"
    "C:\Program Files (x86)\Git\cmd\git.exe"
    "C:\Program Files (x86)\Git\bin\git.exe"
    "C:\Program Files (x86)\Git\mingw64\bin\git.exe"
    "C:\Program Files (x86)\Git\mingw32\bin\git.exe"
) DO (
    IF EXIST %%I (
        SET "GIT_EXECUTABLE=%%I"
        GOTO GitFound
    )
)

:GitFound
IF NOT DEFINED GIT_EXECUTABLE (
    echo Git not found. Please install Git or update the script with the correct path.
    exit /b 1
) ELSE (
    echo Found Git executable: %GIT_EXECUTABLE%
)



@REM set RPath=%USERPROFILE%\Documents\R\R-4.1.2\bin

@echo off
SETLOCAL

SET "RPath="

FOR %%G IN (
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

cd "%USERPROFILE%\Downloads\bvr-wqx-uploader\bvr-wqx-uploader"
 
REM Check if the folder is already a Git repository
IF EXIST "%CD%\.git" (
    echo This is already a Git repository.
) else (
    echo This is not a Git repository. Initializing a new one...
    :: Initialize a new Git repository
    git init

    :: Add a remote origin pointing to a GitHub repository with the same name
    set "githubRepo=https://github.com/FlowWest/bvr-wqx-uploader.git"
    git remote add origin !githubRepo!

    echo Initialized Git repository and added remote origin to:
    echo !githubRepo!
)

cd app

%Rpath%\RScript.exe install-deps.R
%Rpath%\RScript.exe python-setup.R
%Rpath%\R.exe -e "shiny::runApp('.', launch.browser = TRUE)"
pause

ENDLOCAL