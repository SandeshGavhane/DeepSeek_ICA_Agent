@echo off
start cmd /k "echo First Window & dir C:\ && cd C:\DeepSeek_ICA_Agent\geomates && sbcl --script geomates.lisp  & cmd"
start cmd /k "echo Second Window & dir C:\ && cd C:\DeepSeek_ICA_Agent & cmd"

start cmd /k "echo Third Window & start "" "C:\DeepSeek_ICA_Agent\geomates\viewer.html" & cmd"
exit