@echo off
setlocal
setlocal enableextensions
set PATH=C:\Erlang\erl10.0.1\bin;%PATH%
set JSX_FORCE_MAPS=true
for /r .\apps %%f in (*.app) do @if exist "%%f" echo "%%f" && del /f/q "%%f"
for /r . %%f in (*.beam) do @if exist "%%f" echo "%%f" && del /f/q "%%f"
escript rebar clean get-deps compile
for /d /r . %%d in (.rebar) do @if exist "%%d" echo "%%d" && rd /s/q "%%d"
endlocal
if errorlevel 1 pause
