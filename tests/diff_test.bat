@echo off
setlocal

REM This script is used to test the deobfuscator
REM It downloads a JS file, runs the deobfuscator on it, and compares the output to the original file
REM both outputs are prettified before comparison
REM the diff is written to tests/transform.diff


REM Run from crate root
set "ROOT=%~dp0.."
pushd "%ROOT%" || exit /b 1

REM Optional arg: URL or local path to a JS file.
REM If omitted, defaults to Minecraft Classic Reversed app.js.
set "SOURCE=%~1"
if /I "%SOURCE%"=="-h" goto :usage
if /I "%SOURCE%"=="--help" goto :usage
if "%SOURCE%"=="" (
  set "SOURCE=https://github.com/TheSunCat/Minecraft-Classic-Reversed/raw/refs/heads/master/assets/js/app.js"
)

set "INPUT=tests\app.js"
set "DEOBF=tests\app_deobf.js"
set "DEOBF_PRETTY=tests\app_deobf_pretty.js"
set "DIFF_OUT=tests\transform.diff"

REM 0) Materialize input (download URL or copy local file) into tests\app.js
echo(%SOURCE% | findstr /I /B /C:"http://" /C:"https://" >nul
if not errorlevel 1 (
  echo [+] Downloading %SOURCE%
  powershell -NoProfile -ExecutionPolicy Bypass -Command "try { iwr -UseBasicParsing -Uri '%SOURCE%' -OutFile '%INPUT%' } catch { exit 1 }"
  if errorlevel 1 (
    echo [-] Download failed: %SOURCE%
    popd
    exit /b 1
  )
) else (
  if not exist "%SOURCE%" (
    echo [-] Input file not found: %SOURCE%
    popd
    exit /b 1
  )
  echo [+] Copying %SOURCE% to %INPUT%
  copy /Y "%SOURCE%" "%INPUT%" >nul
  if errorlevel 1 (
    echo [-] Copy failed: %SOURCE%
    popd
    exit /b 1
  )
)

REM 1) Prettify input in-place
call npx prettier --write "%INPUT%"
if errorlevel 1 (
  echo [-] Prettier failed on %INPUT%
  popd
  exit /b 1
)

REM 2) Run deobfuscator into app_deobf.js
call cargo run -- "%INPUT%" -o "%DEOBF%"
if errorlevel 1 (
  echo [-] cargo run failed
  popd
  exit /b 1
)

REM 3) Prettify deobfuscated output into app_deobf_pretty.js
call npx prettier --stdin-filepath "%DEOBF%" "%DEOBF%" > "%DEOBF_PRETTY%"
if errorlevel 1 (
  echo [-] Prettier failed on %DEOBF%
  popd
  exit /b 1
)

REM 4) Write diff into tests/transform_diff
call git diff --no-index -- "%INPUT%" "%DEOBF_PRETTY%" > "%DIFF_OUT%"
set "DIFF_EXIT=%ERRORLEVEL%"

REM git diff returns 1 when files differ (expected). Only treat >1 as error.
if %DIFF_EXIT% GTR 1 (
  echo [-] git diff failed with exit code %DIFF_EXIT%
  popd
  exit /b %DIFF_EXIT%
)

echo [+] Done
echo     Input prettified: %INPUT%
echo     Deobfuscated:     %DEOBF%
echo     Pretty deobf:     %DEOBF_PRETTY%
echo     Diff written:    %DIFF_OUT%

popd
exit /b 0

:usage
echo Usage:
echo     %~nx0 [URL^|PATH]
echo.
echo If omitted, defaults to:
echo     https://github.com/TheSunCat/Minecraft-Classic-Reversed/raw/refs/heads/master/assets/js/app.js
popd
exit /b 0
