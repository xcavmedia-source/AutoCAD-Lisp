@echo off
rem ============================================================
rem  Deploy-CADHours.bat  -  install the tracker for the current user
rem
rem    Deploy-CADHours.bat                      install with defaults
rem    Deploy-CADHours.bat \\server\CADHours    install and set the log share
rem    Deploy-CADHours.bat /u                   remove it again
rem
rem  Copies the tracker into AutoCAD's ApplicationPlugins folder, which
rem  AutoCAD loads on its own at start-up.  No installer, no admin
rem  rights, nothing added to the machine outside the user's profile.
rem
rem  To roll it out to everyone, run this from a login script, or copy
rem  the finished CADHours.bundle folder to
rem      %PROGRAMDATA%\Autodesk\ApplicationPlugins
rem  which needs admin rights once but covers every user of the PC.
rem ============================================================

setlocal EnableExtensions
set "HERE=%~dp0"
set "ROOT=%HERE%.."
set "PLUGINS=%APPDATA%\Autodesk\ApplicationPlugins"
set "BUNDLE=%PLUGINS%\CADHours.bundle"
set "CONTENTS=%BUNDLE%\Contents"

if /i "%~1"=="/u"          goto :uninstall
if /i "%~1"=="/uninstall"  goto :uninstall

echo.
echo  Installing CAD Hours Tracker for %USERNAME%
echo    target: %BUNDLE%
echo.

if not exist "%ROOT%\src\CADHours.lsp" (
  echo  ** Cannot find %ROOT%\src\CADHours.lsp
  echo     Run this script from the install folder of the repository.
  exit /b 1
)

if not exist "%CONTENTS%" mkdir "%CONTENTS%" >nul 2>&1
if not exist "%CONTENTS%" (
  echo  ** Could not create %CONTENTS%
  exit /b 1
)

copy /y "%HERE%CADHours.bundle\PackageContents.xml" "%BUNDLE%\" >nul || goto :failed
copy /y "%ROOT%\src\*.lsp"                          "%CONTENTS%\" >nul || goto :failed
copy /y "%ROOT%\web\dashboard-template.html"        "%CONTENTS%\" >nul || goto :failed

rem never overwrite a config the site has already edited
if exist "%CONTENTS%\cadhours.ini" (
  echo  keeping the existing cadhours.ini
) else (
  copy /y "%HERE%cadhours.ini" "%CONTENTS%\" >nul || goto :failed
)

rem tell the tracker where it lives, so it can find its own template
reg add "HKCU\Software\CADHours" /v Home /t REG_SZ /d "%CONTENTS%" /f >nul

rem optional: point the log share at the path given on the command line
if not "%~1"=="" (
  echo  setting LogRoot to %~1
  powershell -NoProfile -Command ^
    "$p='%CONTENTS%\cadhours.ini';" ^
    "$t=Get-Content -LiteralPath $p;" ^
    "$t=$t -replace '^\s*LogRoot\s*=.*', 'LogRoot            = %~1';" ^
    "Set-Content -LiteralPath $p -Value $t" >nul
)

echo.
echo  Done.  Start AutoCAD and open any drawing - the job number
echo  pop-up appears on its own.  Type CADHOURS for the command list,
echo  or CHCONFIG to check the settings that are in force.
echo.
exit /b 0

:failed
echo.
echo  ** Copy failed.  Is AutoCAD still open with the files locked?
exit /b 1

:uninstall
echo.
echo  Removing %BUNDLE%
if exist "%BUNDLE%" rmdir /s /q "%BUNDLE%"
reg delete "HKCU\Software\CADHours" /v Home /f >nul 2>&1
echo  Done.  Logged hours are untouched - only the tracker was removed.
echo.
exit /b 0
