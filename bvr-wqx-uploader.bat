@echo off
"C:\Program Files\R\R-4.3.0\bin\x64\RScript.exe" app\install-deps.R
"C:\Program Files\R\R-4.3.0\bin\x64\RScript.exe" app\python-setup.R
"C:\Program Files\R\R-4.3.0\bin\x64\R.exe" -e "shiny::runApp('./app', launch.browser = TRUE)"
pause
