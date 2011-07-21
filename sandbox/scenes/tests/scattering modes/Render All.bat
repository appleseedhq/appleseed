@echo off

for %%i in (*.appleseed) do ..\..\..\bin\Release\appleseed.cli.exe "%%i" -p "generic_tile_renderer.max_samples=64"

pause
