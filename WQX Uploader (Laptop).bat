set RPath=C:\Users\%USERPROFILE%\AppData\Local\Programs\R\R-4.3.2\bin

"%RPath%\RScript.exe" install-deps.R
"%RPath%\RScript.exe" python-setup.R
"%RPath%\R.exe" -e "shiny::runApp('.', launch.browser = TRUE)"
pause