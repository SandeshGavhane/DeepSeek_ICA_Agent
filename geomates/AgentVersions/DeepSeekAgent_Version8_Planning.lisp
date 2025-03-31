;;;
;;; Enhanced DeepSeekAgent for Geomates game
;;; With environment scanning and collaboration features
;;;  

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

(defun export-visicon-to-file ()
  "Exports the current visicon to a file"
  (with-open-file (vis "vision.txt"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format vis "~a" (printed-visicon))))

; ====================================================================
;; SAFE UTILITY FUNCTIONS - Add these to the beginning of your file
;; ====================================================================

;; Safe version of read-from-string that never errors
(defun safe-read-from-string (string &optional (default nil))
  "Safely read from string, returning default value if an error occurs"
  (handler-case
      (read-from-string string)
    (error (e)
      (format t "~%WARNING: Error reading string: ~a~%Error: ~a~%" string e)
      default)))

;; Safe file operations
(defun safe-file-string (path &optional (default ""))
  "Read file contents into a string, with error handling"
  (handler-case
      (with-open-file (stream path :if-does-not-exist nil)
        (if stream
            (let ((data (make-string (file-length stream))))
              (read-sequence data stream)
              data)
            default))
    (error (e)
      (format t "~%WARNING: Error reading file ~a: ~a~%" path e)
      default)))

;; Extract values safely from strings
(defun extract-value-after (text pattern &optional (default nil))
  "Extract value after a pattern in text"
  (let ((pos (search pattern text)))
    (if pos
        (let ((start (+ pos (length pattern))))
          (let ((end (position-if (lambda (c) (or (char= c #\Space) 
                                                  (char= c #\Tab)
                                                  (char= c #\Newline))) 
                                  text :start start)))
            (if end
                (subseq text start end)
                (subseq text start))))
        default)))

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


;;; Planning System for Geomates Agent

(defun create-domain-file (player-type)
  "Creates a PDDL domain file specific to the player type"
  (with-open-file (dom "domain.pddl"
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
    (format dom "(define (domain geomates-~a)~%" player-type)
    (format dom "  (:requirements :strips :typing)~%")
    (format dom "  (:types player diamond)~%")
    (format dom "  (:predicates~%")
    (format dom "    (player-at ?p - player ?x ?y)~%")
    (format dom "    (diamond-at ?d - diamond ?x ?y)~%")
    (format dom "    (player-has ?p - player ?d - diamond)~%")
    (format dom "    (can-reach ?p - player ?d - diamond)~%")
    (format dom "  )~%")
    
    ;; Common movement actions
    (format dom "  (:action move-left~%")
    (format dom "    :parameters (?p - player ?x ?y)~%")
    (format dom "    :precondition (and (player-at ?p ?x ?y))~%")
    (format dom "    :effect (and (not (player-at ?p ?x ?y))~%")
    (format dom "                 (player-at ?p (- ?x 1) ?y)))~%")
    
    (format dom "  (:action move-right~%")
    (format dom "    :parameters (?p - player ?x ?y)~%")
    (format dom "    :precondition (and (player-at ?p ?x ?y))~%")
    (format dom "    :effect (and (not (player-at ?p ?x ?y))~%")
    (format dom "                 (player-at ?p (+ ?x 1) ?y)))~%")
    
    ;; Player-specific actions
    (if (string= player-type "disc")
        ;; Disc-specific actions - jumping
        (progn
          (format dom "  (:action jump~%")
          (format dom "    :parameters (?p - player ?x ?y ?d - diamond)~%")
          (format dom "    :precondition (and (player-at ?p ?x ?y)~%")
          (format dom "                       (diamond-at ?d ?x ?y2)~%")
          (format dom "                       (> ?y2 ?y)~%")
          (format dom "                       (< (- ?y2 ?y) 8))~%")
          (format dom "    :effect (can-reach ?p ?d))~%"))
        ;; Rect-specific actions - stretching
        (progn
          (format dom "  (:action stretch-vertical~%")
          (format dom "    :parameters (?p - player ?x ?y ?d - diamond)~%")
          (format dom "    :precondition (and (player-at ?p ?x ?y)~%")
          (format dom "                       (diamond-at ?d ?x ?y2)~%")
          (format dom "                       (> ?y2 ?y)~%")
          (format dom "                       (< (- ?y2 ?y) 6))~%")
          (format dom "    :effect (can-reach ?p ?d))~%")))
    
    ;; Diamond collection action
    (format dom "  (:action collect-diamond~%")
    (format dom "    :parameters (?p - player ?d - diamond ?x ?y)~%")
    (format dom "    :precondition (and (player-at ?p ?x ?y)~%")
    (format dom "                       (or (diamond-at ?d ?x ?y)~%")
    (format dom "                           (can-reach ?p ?d)))~%")
    (format dom "    :effect (and (not (diamond-at ?d ?x ?y))~%")
    (format dom "                 (player-has ?p ?d)))~%")
    
    (format dom ")~%"))
  
  (format t "~%Created PDDL domain file for ~a player~%" player-type))

;; Extract diamond and player positions from the scene data
(defun extract-scene-data ()
  "Extracts diamond and player positions from the scene data"
  (let ((player-type nil)
        (player-x nil)
        (player-y nil)
        (diamonds nil))
    
    ;; Parse the scene data file from a previous visicon export
    (with-open-file (vis "vision.txt" :direction :input)
      (loop for line = (read-line vis nil nil)
            while line
            do (cond
                 ;; Check for disc player
                 ((search "\"disc\"" line)
                  (setf player-type "disc")
                  (let* ((parts (split-string line #\Space))
                         (coords (remove-if (lambda (s) (string= s "")) parts)))
                    (setf player-x (parse-number (nth 2 coords)))
                    (setf player-y (parse-number (nth 3 coords)))))
                 
                 ;; Check for rect player
                 ((search "\"rect\"" line)
                  (setf player-type "rect")
                  (let* ((parts (split-string line #\Space))
                         (coords (remove-if (lambda (s) (string= s "")) parts)))
                    (setf player-x (parse-number (nth 2 coords)))
                    (setf player-y (parse-number (nth 3 coords)))))
                 
                 ;; Check for diamonds
                 ((search "\"diamond\"" line)
                  (let* ((parts (split-string line #\Space))
                         (coords (remove-if (lambda (s) (string= s "")) parts)))
                    (push (list (parse-number (nth 2 coords))
                               (parse-number (nth 3 coords)))
                          diamonds))))))
    
    ;; If extraction failed, use hardcoded scene data
    (when (or (null player-type) (null player-x) (null player-y))
      (setf player-type (get-player-type))
      (setf player-x 40.0)
      (setf player-y 22.0))
    
    (when (null diamonds)
      (setf diamonds '((20.0 26.0) (60.0 26.0) (35.0 10.0))))
    
    (list :player-type player-type
          :player-x player-x
          :player-y player-y
          :diamonds diamonds)))

;; Create a problem file based on the scene data
(defun create-problem-file ()
  "Creates a PDDL problem file based on scene data"
  (let* ((scene-data (extract-scene-data))
         (player-type (getf scene-data :player-type))
         (player-x (getf scene-data :player-x))
         (player-y (getf scene-data :player-y))
         (diamonds (getf scene-data :diamonds)))
    
    (with-open-file (prob "problem.pddl"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format prob "(define (problem geomates-~a-problem)~%" player-type)
      (format prob "  (:domain geomates-~a)~%" player-type)
      (format prob "  (:objects~%")
      (format prob "    player1 - player~%")
      (dotimes (i (length diamonds))
        (format prob "    d~a - diamond~%" i))
      (format prob "  )~%")
      
      ;; Initial state
      (format prob "  (:init~%")
      (format prob "    (player-at player1 ~,1f ~,1f)~%" player-x player-y)
      
      ;; Add diamonds
      (dotimes (i (length diamonds))
        (let ((diamond (nth i diamonds)))
          (format prob "    (diamond-at d~a ~,1f ~,1f)~%" i (first diamond) (second diamond))))
      
      (format prob "  )~%")
      
      ;; Goal
      (format prob "  (:goal (and~%")
      (dotimes (i (length diamonds))
        (format prob "    (player-has player1 d~a)~%" i))
      (format prob "  ))~%")
      
      (format prob ")~%"))
    
    (format t "~%Created PDDL problem file based on scene data~%")))

;; Parse a number from a string
(defun parse-number (str)
  "Safely parse a number from a string"
  (when (and str (not (string= str "")))
    (read-from-string str)))

;; Split a string by delimiter
(defun split-string (string delimiter)
  "Split a string by delimiter"
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

;; Find the planner executable
(defun find-planner ()
  "Finds the FF planner executable"
  (or (probe-file "/usr/bin/ff")
      (probe-file "/usr/local/bin/ff")
      (probe-file "./ff")
      (probe-file "C:/DeepSeek_ICA_Agent/ff.exe")))

;; Modified run-planner function with improved error handling
(defun run-planner ()
  "Runs the FF planner with the domain and problem files with improved error handling"
  (let ((planner-path (find-planner)))
    (handler-case
      (if planner-path
          (progn
            (format t "~%Running FF planner at ~a...~%" planner-path)
            (let ((output (with-output-to-string (s)
                            (run-program planner-path
                                       (list "-o" "domain.pddl" "-f" "problem.pddl")
                                       :output s))))
              (with-open-file (plan "plan.txt"
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
                (format plan "~a" output))
              (format t "~%Plan saved to plan.txt~%")))
          (progn
            (format t "~%FF planner not found! Using simple hardcoded plan.~%")
            ;; Create a simple hardcoded plan if planner not found
            (with-open-file (plan "plan.txt"
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (let ((player-type (getf (extract-scene-data) :player-type)))
                (if (string= player-type "disc")
                    (format plan "(move-right player1 10.0 22.0)~%(move-right player1 11.0 22.0)~%")
                    (format plan "(move-left player1 40.0 22.0)~%(move-left player1 39.0 22.0)~%"))))
            (format t "~%Simple hardcoded plan saved to plan.txt~%")))
      ;; Catch any errors during planning
      (error (e)
        (format t "~%ERROR during planning: ~a~%" e)
        ;; Create an emergency fallback plan
        (with-open-file (plan "plan.txt"
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format plan "(move-right player1 40.0 22.0)~%(move-left player1 41.0 22.0)~%"))
        (format t "~%Emergency fallback plan created due to error~%")))))



;; Map planner actions to key presses
(defun action-to-key (action)
  "Converts a planner action to a key press"
  (let ((action-type (first action)))
    (cond
      ((eq action-type 'move-left) "a")
      ((eq action-type 'move-right) "d")
      ((eq action-type 'jump) "w")
      ((eq action-type 'stretch-vertical) "w")
      ((eq action-type 'collect-diamond) "s")
      (t "s"))))

;
;; ====================================================================
;; SIMPLIFIED PLANNING SYSTEM - Replace existing planning functions
;; ====================================================================

;; Simplified run-planning-cycle that produces a basic hardcoded plan
(defun run-planning-cycle ()
  "Runs a simplified planning cycle that uses hardcoded plans"
  (handler-case
      (let ((player-type (get-player-type)))
        (format t "~%Starting simplified planning cycle for ~a player~%" player-type)
        
        ;; Create a simple hardcoded plan based on player type
        (with-open-file (plan "plan.txt"
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (if (string= player-type "disc")
              (progn
                (format plan "(move-right player1 40.0 22.0)~%")
                (format plan "(move-right player1 41.0 22.0)~%")
                (format plan "(jump player1 41.0 22.0 d0)~%")
                (format plan "(collect-diamond player1 d0 41.0 26.0)~%"))
              (progn
                (format plan "(move-left player1 40.0 22.0)~%")
                (format plan "(move-left player1 39.0 22.0)~%")
                (format plan "(stretch-vertical player1 39.0 22.0 d0)~%")
                (format plan "(collect-diamond player1 d0 39.0 26.0)~%"))))
        
        (format t "~%Created simplified plan for ~a player~%" player-type))
    (error (e)
      (format t "~%ERROR in planning cycle: ~a~%" e)
      (with-open-file (plan "plan.txt"
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format plan "(move-right player1 40.0 22.0)~%")))))

;; Simplified parse-plan-file that handles any format issues
(defun parse-plan-file ()
  "Safely parses the plan file to extract actions"
  (let ((plan-actions '()))
    (handler-case
        (with-open-file (plan "plan.txt" :direction :input :if-does-not-exist nil)
          (when plan
            (loop for line = (read-line plan nil nil)
                  while line
                  do (when (and (> (length line) 0)
                               (char= (char line 0) #\())
                       (let ((action-symbol (extract-value-after line "(" 'move-right))
                             (target-key "d")) ; Default key direction
                         (cond
                           ((string= action-symbol "move-left") (setf target-key "a"))
                           ((string= action-symbol "move-right") (setf target-key "d"))
                           ((string= action-symbol "jump") (setf target-key "w"))
                           ((string= action-symbol "stretch-vertical") (setf target-key "w"))
                           ((string= action-symbol "collect-diamond") (setf target-key "s")))
                         (push (list 'press-key target-key) plan-actions))))))
      (error (e)
        (format t "~%ERROR parsing plan: ~a~%" e)
        (setf plan-actions '((press-key "d")))))
    
    (if plan-actions
        (reverse plan-actions)
        '((press-key "d")))))

;; Simplified execute-next-action that always returns a valid key
(defun execute-next-action ()
  "Executes the next action in the plan with guaranteed fallback"
  (handler-case
      (let ((actions (parse-plan-file)))
        (if actions
            (let ((next-action (first actions)))
              (format t "~%Executing action: ~a~%" next-action)
              ;; Remove the executed action from the plan
              (with-open-file (plan "plan.txt"
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
                (dolist (action (rest actions))
                  (format plan "~a~%" action)))
              (or (second next-action) "d")) ; Extract key or default to "d"
            "d"))
    (error (e)
      (format t "~%ERROR during action execution: ~a~%" e)
      "d")))
;;; ACT-R model for Geomates game with enhanced environment scanning and collaboration
;;;  

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
 (scanning-environment)
 (mapping-environment)
 (detecting-diamonds)
 (prioritizing-diamonds)
 (calculating-distances)
 (planning-collection)
 (approaching-target)
 (avoiding-obstacle)
 (communicating)
 (waiting-for-response)
 (processing-message)
 (coordinating-action)
  (planning-ready)
 (executing-plan)
 (stretching)
 (collecting)
 (complete)
 
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

;(p test-export-visicon
;   =goal>
;      state player-identified
;   ?manual>
;      state free
;==>
;   !eval! (test-visicon-export)
;   +manual>
;      cmd press-key
;      key "d"
;   =goal>
;      state testing-export)

;(p test-basic-functions
;   =goal>
;      state player-identified
;   ?manual>
;      state free
;==>
;   !eval! (test-basic-functions)
;   +manual>
;      cmd press-key
;      key "d"
;   =goal>
;      state testing-basics)

(p test-export-visicon
   =goal>
      state player-identified
   ?manual>
      state free
==>
   !eval! (test-visicon-export)
   +manual>
      cmd press-key
      key "d"
   =goal>
      state testing-export)


;; ====================================================================
;; REPLACEMENT PRODUCTION RULES - Add these to the ACT-R model
;; ====================================================================


;; Test production for the simplified planning system - replace existing one
(p test-planning-system
   =goal>
      state testing-export
   ?manual>
      state free
==>
   !eval! (format t "~%Testing simplified planning system...~%")
   !eval! (run-planning-cycle)
   +manual>
      cmd press-key
      key "d"
   =goal>
      state planning-ready)

;; Execute the first action from the plan - corrected version
(p execute-plan-action
   =goal>
      state planning-ready
   ?manual>
      state free
==>
   !eval! (format t "~%Executing plan action...~%")
   !eval! (let ((key (execute-next-action)))
            (format t "~%Pressing key: ~a~%" key))
   +manual>
      cmd press-key
      key "a"  ;; First action is always 'move-left' for rect player
   =goal>
      state executing-plan)

;; Continue plan execution - corrected version that directly uses press-key
(p continue-plan-execution
   =goal>
      state executing-plan
   ?manual>
      state free
==>
   !eval! (format t "~%Continuing plan execution...~%")
   !eval! (let ((key (execute-next-action)))
            (format t "~%Pressing key: ~a~%" key))
   +manual>
      cmd press-key
      key "a"  ;; Second action is 'move-left' for rect player
   =goal>
      state stretching)


;; Add new state for the stretch action
(p execute-stretch
   =goal>
      state stretching
   ?manual>
      state free
==>
   !eval! (format t "~%Executing stretch action...~%")
   +manual>
      cmd press-key
      key "w"  ;; Third action is 'stretch-vertical'
   =goal>
      state collecting)

;; Add new state for the collect action
(p execute-collect
   =goal>
      state collecting
   ?manual>
      state free
==>
   !eval! (format t "~%Executing collect action...~%")
   +manual>
      cmd press-key
      key "s"  ;; Fourth action is 'collect-diamond'
   =goal>
      state complete)

;; Add final state to keep moving
(p keep-exploring
   =goal>
      state complete
   ?manual>
      state free
==>
   !eval! (format t "~%Plan complete, exploring...~%")
   +manual>
      cmd press-key
      key "d"  ;; Continue moving right to explore
   =goal>
      state complete)


;;; Set parameters for the model
(sgp :v t                ; verbose output
     :trace-detail high  ; detailed trace
     :buffer-trace t     ; buffer tracing
     :esc t              ; enable subsymbolic computations 
     :mas 3.0            ; maximum associative strength
     :ans 0.5            ; base level constant
     :rt -10.0           ; retrieval threshold (much lower to ensure retrievals always succeed)
     :pm 1               ; partial matching enabled
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