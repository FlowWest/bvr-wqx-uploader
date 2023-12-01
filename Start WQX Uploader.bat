set RPath=%USERPROFILE%\Documents\R\R-4.1.2\bin

"%RPath%\RScript.exe" install-deps.R
"%RPath%\RScript.exe" python-setup.R
"%RPath%\R.exe" -e "shiny::runApp('.', launch.browser = TRUE)"
pause