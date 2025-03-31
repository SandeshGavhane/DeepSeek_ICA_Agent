;;;
;;; Plan Execution Interface for DeepSeek Geomates Agent
;;;


;;; Global variables for execution control
(defparameter *current-plan* nil
  "Currently executing plan")

(defparameter *current-step* 0
  "Current step in the plan")

(defparameter *execution-status* :idle
  "Status of plan execution (:idle, :executing, :paused, :completed)")

;;; Action execution functions
(defun execute-move-left ()
  "Execute a move-left action"
  (format t "~%Executing move-left~%")
  (send-key-command "a"))

(defun execute-move-right ()
  "Execute a move-right action"
  (format t "~%Executing move-right~%")
  (send-key-command "d"))

(defun execute-jump ()
  "Execute a jump action (for disc player)"
  (format t "~%Executing jump~%")
  (send-key-command "w"))

(defun execute-stretch ()
  "Execute a stretch action (for rect player)"
  (format t "~%Executing stretch~%")
  (send-key-command "w"))

(defun execute-compress ()
  "Execute a compress action (for rect player)"
  (format t "~%Executing compress~%")
  (send-key-command "s"))

;;; Helper function for sending key commands
(defun send-key-command (key)
  "Send a key command to the game"
  (cond 
    ;; If actr-send-key is available, use it
    ((fboundp 'actr-send-key)
     (actr-send-key key))
    
    ;; If respond-to-key-press is directly available
    ((fboundp 'respond-to-key-press)
     (funcall 'respond-to-key-press nil key))
    
    ;; Otherwise just log what would happen
    (t 
     (format t "~%Would send key: ~A (but no sending mechanism available)~%" key))))

;;; Plan execution management
(defun start-plan-execution (plan)
  "Start executing a plan"
  (format t "~%Starting plan execution with ~A actions~%" (length plan))
  (setf *current-plan* plan
        *current-step* 0
        *execution-status* :executing)
  t)

(defun get-next-action ()
  "Get the next action from the current plan"
  (when (and *current-plan*
             (< *current-step* (length *current-plan*)))
    (let ((action (nth *current-step* *current-plan*)))
      (incf *current-step*)
      action)))

(defun has-more-actions ()
  "Check if there are more actions in the current plan"
  (and *current-plan* 
       (< *current-step* (length *current-plan*))))

(defun execute-next-step ()
  "Execute the next step in the current plan"
  (when (has-more-actions)
    (let ((action (get-next-action)))
      (when action
        (let ((key (getf action :action))
              (description (getf action :description)))
          (when key
            (format t "~%Executing action: ~A (~A)~%" key description)
            (send-key-command key))
          t)))))

(defun pause-execution ()
  "Pause plan execution"
  (setf *execution-status* :paused)
  (format t "~%Plan execution paused~%")
  t)

(defun resume-execution ()
  "Resume plan execution"
  (when (eq *execution-status* :paused)
    (setf *execution-status* :executing)
    (format t "~%Plan execution resumed~%")
    t))

(defun reset-execution ()
  "Reset execution state"
  (setf *current-plan* nil
        *current-step* 0
        *execution-status* :idle)
  (format t "~%Plan execution reset~%")
  t)

(defun check-plan-validity (game-state)
  "Check if the current plan is still valid given the current game state"
  ;; This is where you would implement more sophisticated validity checking
  ;; For now, just check if there are any diamonds left and if the plan is done
  (cond
    ;; No diamonds left, plan is complete
    ((null (getf game-state :diamonds))
     (setf *execution-status* :completed)
     nil)
    
    ;; Plan is complete but diamonds remain, need new plan
    ((not (has-more-actions))
     t)
    
    ;; Still have actions and diamonds, continue with current plan
    (t nil)))

(defun execution-test ()
  "Test the execution interface"
  (format t "~%Testing execution interface~%")
  
  ;; Create a test plan
  (let ((test-plan (list 
                    '(:action "a" :description "Move left")
                    '(:action "w" :description "Jump")
                    '(:action "d" :description "Move right"))))
    
    ;; Test plan execution
    (start-plan-execution test-plan)
    
    ;; Test getting and executing actions
    (dotimes (i (length test-plan))
      (let ((action (get-next-action)))
        (format t "~%Action ~A: ~A (~A)~%" 
                i (getf action :action) (getf action :description))))
    
    ;; Test reset
    (reset-execution)
    
    t))


(defun execute-next-step-with-feedback ()
  "Execute the next step with feedback checking"
  (when (has-more-actions)
    (let ((action (get-next-action)))
      (when action
        (let ((key (getf action :action))
              (description (getf action :description)))
          
          ;; Execute the action if it has a key command
          (when key
            (format t "~%Executing action: ~A (~A)~%" key description)
            (send-key-command key)
            
            ;; Wait a moment to let the action take effect
            (sleep 0.2)
            
            ;; Request an environment update to get feedback
            (send-key-command key)  ; Send the same key again to trigger update
            
            ;; Allow time for the update
            (sleep 0.1))
          
          t)))))