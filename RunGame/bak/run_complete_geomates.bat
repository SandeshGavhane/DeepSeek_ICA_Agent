@echo off
echo Geomates Game System Complete Setup

REM Step 1: Start the game server in a new command window
echo Starting game server (geomates.lisp)...
start cmd /k "echo Starting Geomates game server... && sbcl --script C:\DeepSeek_ICA_Agent\geomates\geomates.lisp"

REM Step 2: Wait a moment for the server to initialize
echo Waiting for server to initialize...
timeout /t 5

REM Step 3: Open the viewer in the default browser
echo Opening viewer.html...
start "" "C:\DeepSeek_ICA_Agent\geomates\viewer.html"

REM Step 4: Wait a moment for the viewer to connect
echo Waiting for viewer to connect...
timeout /t 3

REM Step 5: Create ACT-R script with waits
echo Creating ACT-R agent script...

echo (defun wait-for-user () > C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo   (format t "~%%Press Enter to continue to the next step...") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo   (read-line)) >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo. >> C:\DeepSeek_ICA_Agent\actr_agent.lisp

echo (format t "~%%=== Step 1: Loading ACT-R ===~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (load "C:\DeepSeek_ICA_Agent\actr7.x\load-act-r.lisp") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (format t "~%%ACT-R loaded successfully.~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo. >> C:\DeepSeek_ICA_Agent\actr_agent.lisp

echo (format t "~%%=== Step 2: Loading experiment interface ===~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (load "C:\DeepSeek_ICA_Agent\geomates\act-r-experiment.lisp") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (format t "~%%Experiment interface loaded successfully.~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (wait-for-user) >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo. >> C:\DeepSeek_ICA_Agent\actr_agent.lisp

echo (format t "~%%=== Step 3: Starting ACT-R environment ===~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (format t "~%%This step may take some time. Please wait for the environment to fully load...~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (run-environment) >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (format t "~%%ACT-R environment started successfully.~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (wait-for-user) >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo. >> C:\DeepSeek_ICA_Agent\actr_agent.lisp

echo (format t "~%%=== Step 4: Loading DeepSeek agent model ===~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (load-act-r-model "C:\DeepSeek_ICA_Agent\geomates\DeepSeekAgent.lisp") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (format t "~%%DeepSeek agent model loaded successfully.~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (wait-for-user) >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo. >> C:\DeepSeek_ICA_Agent\actr_agent.lisp

echo (format t "~%%=== Step 5: Running geomates experiment ===~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (format t "~%%Starting the experiment now...~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (geomates-experiment) >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo (format t "~%%Experiment completed.~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp
echo. >> C:\DeepSeek_ICA_Agent\actr_agent.lisp

echo (format t "~%%All steps completed. Press Ctrl+C to exit when ready.~%%") >> C:\DeepSeek_ICA_Agent\actr_agent.lisp

REM Step 6: Start a new terminal for the ACT-R agent
echo Starting ACT-R agent...
start cmd /k "echo Starting ACT-R agent... && sbcl --load C:\DeepSeek_ICA_Agent\actr_agent.lisp"

REM Step 7: Provide instructions for human player (optional)
echo.
echo =================================================================
echo GEOMATES GAME SYSTEM STARTED
echo =================================================================
echo.
echo Components running:
echo 1. Game server (in separate window)
echo 2. Web viewer (in browser)
echo 3. ACT-R agent (in separate window)
echo.
echo INSTRUCTIONS FOR HUMAN PLAYER:
echo If you want to play as the second player, open a terminal and type:
echo    telnet localhost 45678
echo.
echo Then use the following controls:
echo    a: move left
echo    d: move right
echo    w: jump (disc) or stretch (rectangle)
echo    s: compress (rectangle only)
echo =================================================================
echo.
echo Press any key to close this window. The other components will continue running.
pause > nul