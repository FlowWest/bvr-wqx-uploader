set RPath=C:\Program Files\R\R-4.2.1\bin

"%RPath%\RScript.exe" install-deps.R
"%RPath%\RScript.exe" python-setup.R
"%RPath%\R.exe" -e "shiny::runApp('.', launch.browser = TRUE)"
pause