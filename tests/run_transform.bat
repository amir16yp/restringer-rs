@echo off
setlocal

REM Run from crate root
set "ROOT=%~dp0.."
pushd "%ROOT%" || exit /b 1

set "INPUT=tests\app.js"
set "DEOBF=tests\app_deobf.js"
set "DEOBF_PRETTY=tests\app_deobf_pretty.js"
set "DIFF_OUT=tests\transform.diff"

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
