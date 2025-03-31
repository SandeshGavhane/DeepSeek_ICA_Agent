;;; ACT-R Agent Loader Script 
(load "C:/DeepSeek_ICA_Agent/actr7.x/load-act-r.lisp") 
(load "C:/DeepSeek_ICA_Agent/geomates/act-r-experiment.lisp") 
(load "C:/DeepSeek_ICA_Agent/geomates/execution-interface.lisp") 
(load "C:/DeepSeek_ICA_Agent/geomates/pddl-interface.lisp") 
(run-environment) 
(load-act-r-model "C:/DeepSeek_ICA_Agent/geomates/DeepSeekAgent.lisp") 
