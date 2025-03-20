@echo off

echo Starting Geomates Game Server...
echo.
echo This window will run the game server. Keep it open while playing.
echo.

cd C:\DeepSeek_ICA_Agent\geomates
sbcl --script geomates.lisp

echo.
echo Server has stopped.
pause