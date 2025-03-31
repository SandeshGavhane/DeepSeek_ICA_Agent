;;;
;;; Enhanced DeepSeekAgent for Geomates game
;;; With environment scanning and collaboration features

(clear-all)

;;; in the agent, arbitrary helper functions may be defined
;;; using Common Lisp, but also all add-ons of the SBCL lisp
;;; system, in particular loading shared libraries and calling
;;; functions in those libraries.
;;; For details, see SBCL manual regarding its alien function interace
;;; or have a look into geomates.lisp which connects to a C library
;;;
;;; Additionally, you can use run-program to call any external software.
;;; Note that the process will be run in a null environment by default, so
;;; all pathnames must be explicit. To handle different locations, a simple
;;; "or" may be all it takes:

(defparameter *my-ls* (or (probe-file "/bin/ls")
			  (probe-file "/usr/bin/ls")
			  (probe-file "some/path"))
  "binds to the first file that exists")

(defun count-entries ()
  "counts the number of files/directories in the root directory"
  (count #\Newline ; just count linebreaks since after printing a name, ls prints a newline
	 (with-output-to-string (result) ; temporary string output stream
	   (run-program (probe-file "/bin/ls") (list "/") :output result))))

;;; In case you need to differentiate different environments/OS/compilers:
;;; have a look at Common-Lisps reader macros #+/#- (like #ifdef in C),
;;; which refer to the global variable *features*
;;; examples:
;;; #+SBCL (print "I'm running the SBCL compiler")
;;; (defparameter *magic-code* #+LITTLE-ENDIAN #x0f12 #-LITTLE-ENDIAN 0x120f)

;;; Basic helper functions for planning

; ====================================================================
;; SAFE UTILITY FUNCTIONS - Add these to the beginning of your file
;; ====================================================================

(defun export-visicon-to-file ()
  "Exports the current visicon to a file with enhanced error handling"
  (format t "~%Attempting to export visicon to file...~%")
  (handler-case
    (with-open-file (vis "vision.txt"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (let ((visicon-content (printed-visicon)))
        (format t "~%Visicon has ~A characters~%" (length visicon-content))
        (format vis "~a" visicon-content)
        t))  ; Return success
    (error (e)
      (format t "~%ERROR exporting visicon: ~A~%" e)
      nil)))  ; Return failure

(defun get-player-type ()
  "Extracts player type from vision.txt file with properly fixed detection"
  ;; Debug print for monitoring
  (format t "~%DEBUG: Starting player type detection from file~%")
  
  ;; We'll check for specific standalone lines with disc or rect
  (with-open-file (vis "vision.txt" :direction :input)
    (let ((in-player-info-section nil))
      (loop for line = (read-line vis nil nil)
            while line
            do (progn
                 ;; Check if we're in the player-info section
                 (when (search "player-info" line)
                   (setf in-player-info-section t)
                   (format t "~%DEBUG: Found player-info section~%"))
                 
                 ;; Check for standalone DISC or RECT on its own line
                 (when in-player-info-section
                   (let ((trimmed-line (string-trim '(#\Space #\Tab) line)))
                     (cond
                       ;; Exact match for DISC or disc
                       ((or (string-equal trimmed-line "DISC") 
                            (string-equal trimmed-line "disc"))
                        (format t "~%DEBUG: Found standalone DISC line in player-info section~%")
                        (return-from get-player-type "disc"))
                       
                       ;; Exact match for RECT or rect
                       ((or (string-equal trimmed-line "RECT")
                            (string-equal trimmed-line "rect"))
                        (format t "~%DEBUG: Found standalone RECT line in player-info section~%")
                        (return-from get-player-type "rect"))
                       
                       ;; End of section detection
                       ((and (> (length trimmed-line) 0)
                             (not (search "player-info" line))
                             (search "FEATURE" line))
                        (setf in-player-info-section nil)
                        (format t "~%DEBUG: End of player-info section~%"))))))
            finally (format t "~%DEBUG: Finished scanning file~%")))
    
    ;; If we get here, we need to do a full scan for any mentions of disc or rect
    (format t "~%DEBUG: Doing a full file scan for player type mentions~%")
    (with-open-file (vis "vision.txt" :direction :input)
      (loop for line = (read-line vis nil nil)
            while line
            do (let ((trimmed-line (string-trim '(#\Space #\Tab) line)))
                 (cond
                   ;; Check for disc anywhere in the line
                   ((or (search "DISC" trimmed-line)
                        (search "disc" trimmed-line))
                    (format t "~%DEBUG: Found DISC mention in: ~a~%" trimmed-line)
                    (return-from get-player-type "disc"))
                   
                   ;; Check for rect anywhere in the line
                   ((or (search "RECT" trimmed-line)
                        (search "rect" trimmed-line))
                    (format t "~%DEBUG: Found RECT mention in: ~a~%" trimmed-line)
                    (return-from get-player-type "rect"))))))
    
    ;; Final fallback
    (format t "~%DEBUG: No player type found, defaulting to disc~%")
    "disc"))

(defun file-string (path)
  "Read file contents into a string"
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun test-visicon-export ()
  "Tests exporting the visicon to a file with enhanced debugging"
  (export-visicon-to-file)
  (format t "~%Visicon exported to vision.txt~%")
  (let ((player-type (get-player-type)))
    (format t "~%Player type detected: ~a~%" player-type)
    
    ;; Print out the first few lines of the file for debugging
    (with-open-file (vis "vision.txt" :direction :input)
      (format t "~%Vision.txt sample (first 10 lines):~%")
      (dotimes (i 10)
        (let ((line (read-line vis nil nil)))
          (when line
            (format t "~a~%" line)))))
    
    ;; Return the detected player type
    player-type))

;;; Define the model
(define-model geomates-enhanced-agent

;;; Define chunk types for game objects, communication, and environment mapping
(chunk-type (polygon-feature (:include visual-location)) regular)
(chunk-type (polygon (:include visual-object)) sides height width rotation diamonds player-type)
(chunk-type (text-feature (:include visual-location)))
(chunk-type (text (:include visual-object)) message sender)
(chunk-type goal state substate last-message)
(chunk-type time ticks)
(chunk-type player-info type diamonds)
(chunk-type other-player-info type position-x position-y diamonds)
(chunk-type outgoing-message content)
(chunk-type message-info content sender response-required)
(chunk-type diamond-location id x-pos y-pos distance reachable assigned-to)
(chunk-type environment-map platforms diamonds obstacles)
(chunk-type navigation-target x-pos y-pos status)
(chunk-type strategy mode priority-target fallback-target)

;;; Do this to avoid warnings when chunks are created
(define-chunks 
 true false polygon
 disc rect
 unreachable pending reachable collected
 efficient defensive collaborative
 horizontal vertical)

;;; Set vision module to look for the lowest (nearest) screen-x item first
(set-visloc-default screen-x lowest)

;;; Add declarative memory chunks
(add-dm
 ;; Keyboard controls
 (w) (a) (s) (d) (m)
 
 ;; Goal states
 (init-visicon)
 (waiting-for-update-1)
 (press-for-second-update)
 (waiting-for-update-2)
 (visicon-ready)
 (finding-diamond)
 (planning)
 (reading-player-type)
 (player-identified)
 (initialize-planning)
 (planning-execution)
 (test-export-visicon)
 (press-key-after-message)

 ;; Message testing states
 (msg-start)
 (msg-key-sent)
 (msg-content-sent)
 (msg-waiting)
 
 ;; Predefined messages
 (msg-1 isa outgoing-message content (:hello :i-am "deep-seek-agent"))
 (msg-2 isa outgoing-message content (:ready-to-collaborate :task "diamond-collection"))
 (msg-3 isa outgoing-message content (:strategy :mode "collaborative"))
 
 ;; Initial goal
 (first-goal isa goal state init-visicon)
)

;;; Set initial goal
(goal-focus first-goal)

;;;
;;; VISICON INITIALIZATION PHASE
;;;

;;; First press 'd' key to trigger visicon update
(p init-visicon-press-d
   =goal>
      state init-visicon
   ?manual>
      state free
==>
   !eval! (format t "~%Starting visicon update: pressing 'd'~%")
   +manual>
      cmd press-key
      key "d"
   =goal>
      state waiting-for-update-1
)

;;; Wait for first update
(p wait-for-first-update
   =goal>
      state waiting-for-update-1
   ?manual>
      state free
==>
   +temporal>
      isa time
      ticks 1.0  ; Wait 1 second for visicon to update
   =goal>
      state press-for-second-update
)

;;; Press 'a' key to trigger second visicon update 
(p press-for-second-update
   =goal>
      state press-for-second-update
   ?temporal>
      buffer requested
==>
   !eval! (format t "~%Second visicon update: pressing 'a'~%")
   +manual>
      cmd press-key
      key "a"
   =goal>
      state waiting-for-update-2
)

;;; Wait for second update
(p wait-for-second-update
   =goal>
      state waiting-for-update-2
   ?manual>
      state free
==>
   +temporal>
      isa time
      ticks 1.0  ; Wait another second
   =goal>
      state visicon-ready
)

;;; Transition to player detection phase
(p visicon-ready-transition
   =goal>
      state visicon-ready
   ?temporal>
      buffer requested
==>
   !eval! (format t "~%Visicon update complete - transitioning to player detection~%")
   =goal>
      state finding-diamond  ; Changed to finding-diamond for player detection
)

;;;
;;; PLAYER DETECTION PHASE
;;;

;;; Look for "player-info" in the visicon
(p detect-player-info
   =goal>
      state finding-diamond
   ?visual-location>
      state free
==>
   !eval! (format t "~%Looking for player-info in visicon~%")
   +visual-location>
      isa visual-location
      value "player-info"    ; Look for the player-info value
   =goal>
      state planning
)

;;; Process the player info once found
(p process-player-info
   =goal>
      state planning
   =visual-location>
      isa visual-location
   ?visual>
      state free
==>
   !eval! (format t "~%Found player-info, attending to it~%")
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state reading-player-type
)

;;; Extract and store the player type
(p extract-player-type
   =goal>
      state reading-player-type
   =visual>
      isa polygon
      player-type =type    ; Get the player-type slot value
   ?imaginal>
      state free
==>
   !eval! (format t "~%Player type identified as: ~S~%" =type)
   +imaginal>
      isa player-info
      type =type           ; Store the player type
      diamonds 0           ; Initialize diamond count
   =goal>
      state player-identified
)

;;; Alternative search for player using "rect" value if player-info search fails
(p detect-rect-player
   =goal>
      state finding-diamond
   ?visual-location>
      state error
==>
   !eval! (format t "~%No player-info found, looking for rect player~%")
   +visual-location>
      isa visual-location
      value "rect"         ; Try looking for "rect" directly
   =goal>
      state planning
)

;;; Alternative search for player using "disc" value if rect search fails
(p detect-disc-player
   =goal>
      state planning
   ?visual-location>
      state error
==>
   !eval! (format t "~%No rect player found, looking for disc player~%")
   +visual-location>
      isa visual-location
      value "disc"         ; Try looking for "disc" directly
   =goal>
      state planning
)

;;; Fallback if player detection completely fails
(p player-detection-fallback
   =goal>
      state planning
   ?visual-location>
      state error
==>
   !eval! (format t "~%Failed to detect player by any method, assuming disc player~%")
   ?imaginal>
      state free
==>
   +imaginal>
      isa player-info
      type "disc"        ; Default assumption
      diamonds 0         ; Initialize diamond count
   =goal>
      state player-identified
)

;;; Send hello message after player identification
(p send-hello-message
   =goal>
      state player-identified
   =imaginal>
      isa player-info
      type =ptype
   ?manual>
      state free
==>
   !eval! (format t "~%Sending hello message to other agent~%")
   !eval! (send-message '(:hello :i-am "deep-seek-agent" :type =ptype))
   =goal>
      state press-key-after-message
   =imaginal>
)

;;; Press a key after sending message
(p press-key-after-message
   =goal>
      state press-key-after-message
   =imaginal>
      isa player-info
      type =type
   ?manual>
      state free
==>
   =imaginal>
   !eval! (format t "~%Pressing key after sending message~%")
   +manual>
      cmd press-key
      key "d"  ; You can change this to any key you prefer
   =goal>
      state test-export-visicon

)
;;; Test export visicon and initialize planning
(p test-export-visicon
   =goal>
      state test-export-visicon
   =imaginal>
      isa player-info
      type =type
   ?manual>
      state free

==>
   =imaginal>
   !eval! (test-visicon-export)
   =goal>
      state initialize-planning

)


;;; Add a new production to trigger planning after player identification
(p trigger-pddl-planning
   =goal>
      state initialize-planning
   =imaginal>
      isa player-info
      type =ptype
==>
   !eval! (format t "~%triggering planning~%")
   !eval! (pddl-interface:init-planning)
   =goal>
      state planning-execution
   =imaginal>
)

(p trigger-pddl-planning-execution
   =goal>
      state planning-execution
   =imaginal>
      isa player-info
      type =ptype
   ?manual>
      state free
==>
   !eval! (format t "~%Triggering PDDL planning~%")
   !eval! (pddl-interface:generate-and-execute-plan)
   =goal>
      state executing-plan
)

(p execute-plan-step
   =goal>
      state executing-plan
   ?manual>
      state free
==>
   !eval! (let ((action (pddl-interface:get-next-action)))
            (when action
              (let ((key (getf action :action)))
                (when key
                  (format t "~%Executing action: ~A (~A)~%" 
                          key (getf action :description))
                  (goal-focus-fct
                   (car (define-chunks-fct
                         `((isa goal 
                             state executing-action
                             substate ,key)))))
                  ))))
   +temporal>
      isa time
      ticks 0.5  ; Wait a short time before checking for more actions
   =goal>
      state check-more-actions
)

;; This production executes when an action key needs to be pressed
(p execute-action-press-key
   =goal>
      state executing-action
      substate =key
   ?manual>
      state free
==>
   !eval! (format t "~%Pressing key: ~A~%" =key)
   +manual>
      cmd press-key
      key =key
   =goal>
      state executing-plan
)

;; Check if there are more actions in the plan
(p check-more-plan-actions
   =goal>
      state check-more-actions
   ?temporal>
      buffer requested
==>
   !eval! (let ((has-more (pddl-interface:has-more-actions)))
            (format t "~%Plan has more actions: ~A~%" has-more))
   =goal>
      state executing-plan
)

;; When the plan is complete, transition to scanning for new information
(p plan-execution-complete
   =goal>
      state check-more-actions
   !eval! (not (pddl-interface:has-more-actions))
==>
   !eval! (format t "~%Plan execution complete. Scanning environment.~%")
   =goal>
      state scanning-environment
)

;; Scan environment for updated information
(p scan-environment
   =goal>
      state scanning-environment
   ?manual>
      state free
==>
   !eval! (format t "~%Scanning environment for updates~%")
   +manual>
      cmd press-key
      key "d"
   +temporal>
      isa time
      ticks 1.0  ; Allow time for environment update
   =goal>
      state updating-plan
)

;; Decide whether to create a new plan or continue with current one
(p update-plan
   =goal>
      state updating-plan
   ?temporal>
      buffer requested
==>
   !eval! (let ((needs-new-plan (pddl-interface:check-plan-validity)))
            (if needs-new-plan
                (format t "~%Current plan no longer valid. Generating new plan.~%")
                (format t "~%Current plan still valid. Continuing execution.~%")))
   =goal>
      state planning-execution
)

;;; Set parameters for the model - remove unsupported parameters
(sgp :v t                ; verbose output
     :trace-detail high  ; detailed trace
     :esc t              ; enable subsymbolic computations 
     :mas 3.0            ; maximum associative strength
     :ans 0.5            ; base level constant
     :rt -10.0           ; retrieval threshold (much lower to ensure retrievals always succeed)
     :visual-movement-tolerance 3.0 ; tolerance for visual movements
     :visual-num-finsts 15        ; number of visual finsts
     :visual-finst-span 10.0       ; visual finst span
     :show-focus t                 ; show visual focus
     :dat 0.05           ; default action time - make actions quicker
     :randomize-time nil ; don't randomize timing for more predictable behavior 
     :egs 0.1            ; expected gain noise
     :time-master-start-increment 1.0 ; for temporal module timing
     :time-mult 1.0                   ; for accurate temporal delays
     )
)