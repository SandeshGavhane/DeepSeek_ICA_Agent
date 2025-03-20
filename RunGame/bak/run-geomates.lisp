;; Main script to run Geomates with ACT-R with proper waiting

;; Function to wait for user confirmation before proceeding
(defun wait-for-user ()
  (format t "~%Press Enter to continue to the next step...")
  (read-line))

;; First, load ACT-R
(format t "~%=== Step 1: Loading ACT-R ===~%")
(load "actr7.x/load-act-r.lisp")
(format t "~%ACT-R loaded successfully.~%")

;; Then load the experiment interface
(format t "~%=== Step 2: Loading experiment interface ===~%")
(load "C:/DeepSeek_ICA_Agent/geomates/act-r-experiment.lisp")
(format t "~%Experiment interface loaded successfully.~%")
(wait-for-user)

;; Start the ACT-R environment
(format t "~%=== Step 3: Starting ACT-R environment ===~%")
(format t "~%This step may take some time. Please wait for the environment to fully load...~%")
(run-environment)
(format t "~%ACT-R environment started successfully.~%")
(wait-for-user)

;; Load the DeepSeek agent model
(format t "~%=== Step 4: Loading DeepSeek agent model ===~%")
(load-act-r-model "C:/DeepSeek_ICA_Agent/geomates/DeepSeekAgent.lisp")
(format t "~%DeepSeek agent model loaded successfully.~%")
(wait-for-user)

;; Run the experiment
(format t "~%=== Step 5: Running geomates experiment ===~%")
(format t "~%Starting the experiment now...~%")
(geomates-experiment)
(format t "~%Experiment completed.~%")

;; Keep SBCL running so user can see the results
(format t "~%All steps completed. Press Ctrl+C to exit when ready.~%")

;; Don't automatically exit
;; (quit)