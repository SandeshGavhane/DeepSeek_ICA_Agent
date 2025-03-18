@echo off
echo Starting GeoMates Environment...

REM Open first command prompt, navigate to GeoMates directory and run sbcl script
start cmd /k "cd /d C:\DeepSeek_ICA_Agent\geomates && echo Running geomates.lisp... && sbcl --script geomates.lisp && echo geomates.lisp script completed"

REM Open viewer.html
timeout /t 2
start "" "C:\DeepSeek_ICA_Agent\geomates\viewer.html"

REM Open second command prompt for ACT-R, navigate to appropriate directory
start cmd /k "cd /d C:\DeepSeek_ICA_Agent && echo Loading ACT-R... && sbcl --load "actr7.x/load-act-r.lisp" && echo ACT-R loaded && echo. && echo Loading experiment files... && echo (load \"C:/DeepSeek_ICA_Agent/geomates/act-r-experiment.lisp\") && echo (run-environment) && echo (load-act-r-model \"C:/DeepSeek_ICA_Agent/geomates/model-dummy.lisp\") && echo (load-act-r-model \"C:/DeepSeek_ICA_Agent/geomates/DeepSeekAgent.lisp\") && echo (geomates-experiment) && echo. && echo Now enter these commands one by one in the SBCL prompt:"

REM Open third command prompt for Telnet
timeout /t 30
set "dummy="
set /p DUMMY=Hit ENTER to continue...
start cmd /k "echo Connecting to ACT-R via Telnet... && echo. && telnet localhost 45678"

echo.
echo All processes have been started!
echo.
echo.
pause