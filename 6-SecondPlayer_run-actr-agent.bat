@echo off

echo Starting ACT-R Agent...

REM Change to the DeepSeek_ICA_Agent directory
cd C:\DeepSeek_ICA_Agent_2

REM Create a temporary Lisp script file
echo ;;; ACT-R Agent Loader Script > actr_temp.lisp
REM Add command to load ACT-R
echo (load "C:/DeepSeek_ICA_Agent_2/actr7.x/load-act-r.lisp") >> actr_temp.lisp
REM Add command to load the experiment interface
echo (load "C:/DeepSeek_ICA_Agent_2/geomates/act-r-experiment.lisp") >> actr_temp.lisp

REM Add command to load the experiment interface
echo (load "C:/DeepSeek_ICA_Agent_2/geomates/execution-interface.lisp") >> actr_temp.lisp
REM Add command to load the experiment interface
echo (load "C:/DeepSeek_ICA_Agent_2/geomates/pddl-interface.lisp") >> actr_temp.lisp

REM Add command to run the ACT-R environment
echo (run-environment) >> actr_temp.lisp
REM Add command to load the DeepSeekAgent model
echo (load-act-r-model "C:/DeepSeek_ICA_Agent_2/geomates/DeepSeekAgent.lisp") >> actr_temp.lisp
REM Add command to run the experiment
::echo (geomates-experiment) >> actr_temp.lisp

REM Start SBCL and run the Lisp script
echo Starting SBCL with ACT-R...
sbcl --load actr_temp.lisp

REM Clean up by deleting the temporary file
del actr_temp.lisp
pause
