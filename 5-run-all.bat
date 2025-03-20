@echo off
echo Geomates Game - Master Control Script
echo ====================================
echo.
echo This script will start all components of the Geomates game:
echo 1. Game server (in separate window)
echo 2. Web viewer (in browser)
echo 3. ACT-R agent (in separate window)
echo 4. Human player connection (in separate window)
echo.
echo Follow the instructions in each window as they appear.
echo.
echo Press any key to begin...
pause > nul

echo.
echo Step 1: Starting game server...
start cmd /k "C:\DeepSeek_ICA_Agent\1-run-server.bat"

echo Waiting for server to initialize...
timeout /t 5

echo.
echo Step 2: Opening web viewer...
start "" "C:\DeepSeek_ICA_Agent\2-open-viewer.bat"

echo Waiting for viewer to connect...
timeout /t 3

echo.
echo Step 3: Starting ACT-R agent...
start cmd /k "C:\DeepSeek_ICA_Agent\3-run-actr-agent.bat"

echo Waiting for ACT-R to initialize...
timeout /t 5


echo.
echo Step 4: Starting human player connection...
echo Note: You should only start this AFTER the ACT-R agent has connected.
echo.
echo Press any key when you're ready to connect as the human player...
pause > nul

start cmd /k "C:\DeepSeek_ICA_Agent\4-connect-human.bat"

echo.
echo All components have been started.
echo.
echo Important Notes:
echo - The game server and ACT-R agent windows must remain open
echo - Use the telnet window to control your player
echo - If the telnet window doesn't respond, try restarting it
echo.
echo Press any key to close this control window...
pause > nul