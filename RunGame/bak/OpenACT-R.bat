@echo off

REM Open second command prompt for ACT-R, navigate to appropriate directory
start cmd /k "cd /d C:\DeepSeek_ICA_Agent && echo Loading ACT-R... && sbcl --load "actr7.x/load-act-r.lisp"