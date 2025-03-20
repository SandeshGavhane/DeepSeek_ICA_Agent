@echo off
echo Shutting down Geomates Game System...

REM Close any SBCL processes (game server and ACT-R agent)
echo Closing SBCL processes...
taskkill /F /IM sbcl.exe /T

REM Close any telnet sessions (human player)
echo Closing telnet sessions...
taskkill /F /IM telnet.exe /T

REM Close any cmd windows with specific titles
echo Closing command windows...
taskkill /F /FI "WINDOWTITLE eq Starting ACT-R Agent*" /IM cmd.exe
taskkill /F /FI "WINDOWTITLE eq Starting Geomates Game Server*" /IM cmd.exe

REM Optional: Close the browser tab with viewer.html (this is tricky and might close other tabs)
REM Uncomment the line below if you want to attempt to close the browser, but be aware it might close more than intended
REM start "" "about:blank" & timeout /t 1 & taskkill /F /IM msedge.exe /T & taskkill /F /IM chrome.exe /T & taskkill /F /IM firefox.exe /T

echo.
echo All Geomates processes have been terminated.
echo Note: Browser windows with the viewer were not closed automatically.

pause