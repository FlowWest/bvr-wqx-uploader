@echo off
for /f "delims=" %%i in ('dir "C:\Program Files\R" /b /ad /o-d') do set latest=%%i
set "R_PATH=C:/Program Files/R/%latest%/bin/x64"
"%R_PATH%/RScript.exe" install-deps.R
"%R_PATH%/RScript.exe" python-setup.R
"%R_PATH%/R.exe" -e "shiny::runApp('.', launch.browser = TRUE)"
pause
