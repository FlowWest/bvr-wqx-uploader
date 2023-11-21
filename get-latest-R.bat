@echo off
for /f "delims=" %%i in ('dir "C:\Program Files\R" /b /ad /o-d') do set latest=%%i
echo %latest%