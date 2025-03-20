@echo off
echo Connecting as human player using Telnet...
echo.
echo Controls:
echo   a: move left
echo   d: move right
echo   w: jump (disc) or stretch (rectangle)
echo   s: compress (rectangle only)
echo.
echo Starting telnet connection to localhost:45678...
echo Note: You need Telnet Client enabled in Windows features.
echo.

telnet localhost 45678

echo.
echo Telnet connection closed.
pause