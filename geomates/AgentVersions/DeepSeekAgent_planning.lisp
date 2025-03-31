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


(defun split-string (string &optional (delimiter #\Space))
  "Splits a string by delimiter"
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun set-equal (list1 list2 &key (test #'eql))
  "Checks if two lists contain the same elements (possibly in different orders)"
  (and (= (length list1) (length list2))
       (every (lambda (item) 
                (find item list2 :test test)) 
              list1)))

(defparameter *geomates-domain*
  '(define (domain geomates)
    (:requirements :strips :typing :fluents)
    
    (:types
      player - object
      diamond - object
      platform - object
      location - object)
    
    (:predicates
      (player-at ?p - player ?loc - location)
      (diamond-at ?d - diamond ?loc - location)
      (platform-at ?plat - platform ?loc - location)
      (connected ?loc1 - location ?loc2 - location)
      (can-reach ?p - player ?loc - location)
      (collected ?d - diamond)
      (is-disc ?p - player)
      (is-rect ?p - player))
    
    (:functions
      (distance ?loc1 - location ?loc2 - location)
      (diamonds-collected ?p - player))
    
    ;; Define actions for disc player
    (:action move-disc
      :parameters (?p - player ?from - location ?to - location)
      :precondition (and (is-disc ?p)
                        (player-at ?p ?from)
                        (connected ?from ?to)
                        (can-reach ?p ?to))
      :effect (and (not (player-at ?p ?from))
                  (player-at ?p ?to)))
    
    (:action jump-disc
      :parameters (?p - player ?from - location ?to - location)
      :precondition (and (is-disc ?p)
                        (player-at ?p ?from)
                        (connected ?from ?to)
                        (not (can-reach ?p ?to)))
      :effect (and (not (player-at ?p ?from))
                  (player-at ?p ?to)
                  (can-reach ?p ?to)))
    
    ;; Define actions for rect player
    (:action move-rect
      :parameters (?p - player ?from - location ?to - location)
      :precondition (and (is-rect ?p)
                        (player-at ?p ?from)
                        (connected ?from ?to)
                        (can-reach ?p ?to))
      :effect (and (not (player-at ?p ?from))
                  (player-at ?p ?to)))
    
    (:action stretch-rect
      :parameters (?p - player ?loc - location)
      :precondition (and (is-rect ?p)
                        (player-at ?p ?loc))
      :effect (and (can-reach ?p ?loc)))
    
    ;; Collect diamond action (works for both players)
    (:action collect-diamond
      :parameters (?p - player ?loc - location ?d - diamond)
      :precondition (and (player-at ?p ?loc)
                        (diamond-at ?d ?loc)
                        (not (collected ?d)))
      :effect (and (collected ?d)
                  (increase (diamonds-collected ?p) 1)))))

;;; Planning state variables
(defvar *current-plan* nil
  "The current plan being executed")

(defvar *plan-index* 0
  "Index into the current plan")

(defvar *replanning-required* nil
  "Flag indicating whether replanning is needed")

(defvar *last-player-position* nil
  "Last known player position")

(defvar *planning-initialized* nil
  "Flag indicating whether planning has been initialized")

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


(defun initialize-planning-system ()
  "Initializes the planning system"
  (setf *current-plan* nil
        *plan-index* 0
        *replanning-required* t
        *last-player-position* nil
        *planning-initialized* t)
  (format t "~%Planning system initialized"))


;;;
;;; PDDL PROBLEM GENERATION AND PLANNING
;;;

(defun extract-player-position ()
  "Extracts player position from vision.txt"
  (let ((player-type (get-player-type))
        (player-pos nil))
    
    (with-open-file (vis "vision.txt" :direction :input)
      (let ((header-line (read-line vis nil nil))) ; Skip header
        (declare (ignore header-line))
        (loop for line = (read-line vis nil nil)
              while line
              do (let* ((parts (split-string line))
                        (value (nth 3 parts)))
                   (when (or (and (string= player-type "disc") 
                                 (string= value "\"disc\""))
                            (and (string= player-type "rect") 
                                 (string= value "\"rect\"")))
                     (setf player-pos (list (read-from-string (nth 1 parts))
                                          (read-from-string (nth 2 parts)))))))))
    
    (when player-pos
      (format t "~%Extracted player position: ~A" player-pos))
    
    player-pos))

(defun find-all-diamonds ()
  "Finds all diamonds in the current visicon"
  (let ((diamonds nil))
    (with-open-file (vis "vision.txt" :direction :input)
      (let ((header-line (read-line vis nil nil)))
        (declare (ignore header-line))
        (loop for line = (read-line vis nil nil)
              while line
              do (let* ((parts (split-string line))
                        (value (nth 3 parts)))
                   (when (string= value "\"diamond\"")
                     (let ((diamond-id (format nil "diamond~A" (length diamonds)))
                           (x (read-from-string (nth 1 parts)))
                           (y (read-from-string (nth 2 parts))))
                       (push (list diamond-id x y) diamonds)))))))
    
    (when diamonds
      (format t "~%Found ~A diamonds:" (length diamonds))
      (dolist (d diamonds)
        (format t "~%  ~A at (~A, ~A)" (first d) (second d) (third d))))
    
    diamonds))

(defun find-all-platforms ()
  "Finds all platforms in the current visicon"
  (let ((platforms nil))
    (with-open-file (vis "vision.txt" :direction :input)
      (let ((header-line (read-line vis nil nil)))
        (declare (ignore header-line))
        (loop for line = (read-line vis nil nil)
              while line
              do (let* ((parts (split-string line))
                        (value (nth 3 parts)))
                   (when (string= value "\"platform\"")
                     (let ((platform-id (format nil "platform~A" (length platforms)))
                           (x (read-from-string (nth 1 parts)))
                           (y (read-from-string (nth 2 parts)))
                           (width (handler-case (read-from-string (nth 10 parts))
                                    (error () 1.0)))
                           (height (handler-case (read-from-string (nth 11 parts))
                                     (error () 1.0))))
                       (push (list platform-id x y width height) platforms)))))))
    
    (when platforms
      (format t "~%Found ~A platforms:" (length platforms))
      (dolist (p (subseq platforms 0 (min 3 (length platforms))))
        (format t "~%  ~A at (~A, ~A) size: ~A x ~A" 
                (first p) (second p) (third p) (fourth p) (fifth p))))
    
    platforms))

(defun generate-problem-from-visicon ()
  "Generates a PDDL problem from the current visicon state"
  (let ((objects nil)
        (init nil)
        (diamonds nil)
        (player-loc nil)
        (player-type (get-player-type))
        (platforms nil))
    
    ;; Extract player position
    (setf player-loc (extract-player-position))
    
    ;; Extract diamonds
    (setf diamonds (find-all-diamonds))
    
    ;; Extract platforms
    (setf platforms (find-all-platforms))
    
    ;; Add player object to the objects list
    (push "player1" objects)
    
    ;; Add player type predicates
    (if (string= player-type "disc")
        (push '(is-disc player1) init)
        (push '(is-rect player1) init))
    
    ;; Add player position
    (when player-loc
      (let ((loc-id (format nil "loc~A~A" (first player-loc) (second player-loc))))
        (push loc-id objects)
        (push `(player-at player1 ,loc-id) init)))
    
    ;; Add diamonds
    (dolist (diamond diamonds)
      (let ((diamond-id (first diamond))
            (x (second diamond))
            (y (third diamond))
            (loc-id (format nil "loc~A~A" x y)))
        
        ;; Add diamond and location to objects
        (push diamond-id objects)
        (push loc-id objects)
        
        ;; Add diamond predicates
        (push `(diamond-at ,diamond-id ,loc-id) init)))
    
    ;; Add platforms
    (dolist (platform platforms)
      (let ((platform-id (first platform))
            (x (second platform))
            (y (third platform))
            (loc-id (format nil "loc~A~A" x y)))
        
        ;; Add platform and location to objects
        (push platform-id objects)
        (unless (find loc-id objects :test #'string=)
          (push loc-id objects))
        
        ;; Add platform predicates
        (push `(platform-at ,platform-id ,loc-id) init)))
    
    ;; Add connected locations - create a simple grid of connections
    (let ((locations (remove-if-not (lambda (obj) 
                                     (and (stringp obj) 
                                          (char= (char obj 0) #\l)))
                                   objects)))
      (dolist (loc1 locations)
        (dolist (loc2 locations)
          (unless (string= loc1 loc2)
            (let* ((loc1-name (subseq loc1 3))
                   (loc2-name (subseq loc2 3))
                   (loc1-x (read-from-string 
                            (subseq loc1-name 0 (position-if-not #'digit-char-p loc1-name))))
                   (loc1-y (read-from-string 
                            (subseq loc1-name (position-if-not #'digit-char-p loc1-name))))
                   (loc2-x (read-from-string 
                            (subseq loc2-name 0 (position-if-not #'digit-char-p loc2-name))))
                   (loc2-y (read-from-string 
                            (subseq loc2-name (position-if-not #'digit-char-p loc2-name))))
                   (dx (abs (- loc1-x loc2-x)))
                   (dy (abs (- loc1-y loc2-y))))
              
              ;; Connect adjacent locations (Manhattan distance = 1)
              (when (= (+ dx dy) 1)
                (push `(connected ,loc1 ,loc2) init)))))))
    
    ;; Add initial reachability - all adjacent locations are initially reachable
    (let ((player-loc-id (format nil "loc~A~A" (first player-loc) (second player-loc))))
      (dolist (connected-pred init)
        (when (and (listp connected-pred)
                  (eq (first connected-pred) 'connected)
                  (string= (second connected-pred) player-loc-id))
          (push `(can-reach player1 ,(third connected-pred)) init))))
    
    ;; Add initial diamonds collected count
    (push '(= (diamonds-collected player1) 0) init)
    
    ;; Generate a goal to collect all diamonds
    (let ((goal `(and ,@(mapcar (lambda (d) `(collected ,(first d))) diamonds))))
      
      ;; Return the complete problem
      (let ((problem 
             `(define (problem geomates-current)
                (:domain geomates)
                (:objects ,@(remove-duplicates objects :test #'string=))
                (:init ,@init)
                (:goal ,goal))))
        
        (format t "~%Generated PDDL problem with:")
        (format t "~%  ~A objects" (length (remove-duplicates objects :test #'string=)))
        (format t "~%  ~A init conditions" (length init))
        (format t "~%  Goal to collect ~A diamonds" (length diamonds))
        
        problem))))

;;;
;;; PDDL PLANNING ALGORITHM
;;;

(defstruct pddl-state
  "Represents a state in the planning process"
  predicates  ; List of ground predicates true in this state
  functions   ; List of (function-name . value) pairs
  parent      ; Parent state
  action      ; Action that led to this state
  g-cost      ; Path cost
  h-cost)     ; Heuristic cost estimate

(defun action-applicable-p (action state)
  "Checks if an action is applicable in the given state"
  (every (lambda (precond) (predicate-true-p precond state))
         (action-preconditions action)))

(defun predicate-true-p (predicate state)
  "Checks if a predicate is true in the given state"
  (cond
    ;; Handle AND conditions
    ((eq (first predicate) 'and)
     (every (lambda (p) (predicate-true-p p state)) (rest predicate)))
    
    ;; Handle NOT conditions
    ((eq (first predicate) 'not)
     (not (predicate-true-p (second predicate) state)))
    
    ;; Handle numeric comparisons
    ((member (first predicate) '(< <= = >= >))
     (let ((op (first predicate))
           (lhs (evaluate-term (second predicate) state))
           (rhs (evaluate-term (third predicate) state)))
       (case op
         (< (< lhs rhs))
         (<= (<= lhs rhs))
         (= (= lhs rhs))
         (>= (>= lhs rhs))
         (> (> lhs rhs)))))
    
    ;; Handle regular predicates
    (t
     (member predicate (pddl-state-predicates state) :test #'equal))))

(defun evaluate-term (term state)
  "Evaluates a numeric term in the given state"
  (cond
    ;; Number literals
    ((numberp term) term)
    
    ;; Function calls
    ((listp term)
     (let ((func-val (assoc (first term) (pddl-state-functions state) :test #'equal)))
       (if func-val
           (cdr func-val)
           0)))  ; Default to 0 if function not found
    
    ;; Arithmetic operations
    ((and (listp term) (member (first term) '(+ - * /)))
     (let ((op (first term))
           (args (mapcar (lambda (arg) (evaluate-term arg state)) (rest term))))
       (case op
         (+ (apply #'+ args))
         (- (apply #'- args))
         (* (apply #'* args))
         (/ (apply #'/ args)))))
    
    ;; Default case
    (t 0)))

(defun apply-action (action state)
  "Applies an action to a state, returning a new state"
  (let* ((predicates (copy-list (pddl-state-predicates state)))
         (functions (copy-list (pddl-state-functions state)))
         (effects (action-effects action)))
    
    ;; Process the effects
    (dolist (effect effects)
      (cond
        ;; AND effects
        ((eq (first effect) 'and)
         (dolist (e (rest effect))
           (apply-single-effect e predicates functions)))
        
        ;; Regular effects
        (t (apply-single-effect effect predicates functions))))
    
    ;; Create a new state
    (make-pddl-state
     :predicates predicates
     :functions functions
     :parent state
     :action action
     :g-cost (1+ (pddl-state-g-cost state))
     :h-cost 0)))  ; We'll compute this later

(defun apply-single-effect (effect predicates functions)
  "Applies a single effect to the state"
  (cond
    ;; NOT effects (remove predicates)
    ((eq (first effect) 'not)
     (let ((pred (second effect)))
       (setf predicates (remove pred predicates :test #'equal))))
    
    ;; Numeric effects
    ((member (first effect) '(increase decrease assign))
     (let* ((op (first effect))
            (func (second effect))
            (value (third effect))
            (current-pair (assoc func functions :test #'equal))
            (current-value (if current-pair (cdr current-pair) 0))
            (new-value (case op
                         (increase (+ current-value value))
                         (decrease (- current-value value))
                         (assign value))))
       
       ;; Update function value
       (if current-pair
           (setf (cdr current-pair) new-value)
           (push (cons func new-value) functions))))
    
    ;; Regular effects (add predicates)
    (t (pushnew effect predicates :test #'equal))))

(defun a-star-search (initial-state goal actions heuristic-fn)
  "Performs A* search from initial state to goal"
  (let ((open-list (list initial-state))
        (closed-list nil)
        (current-best nil))
    
    (loop while open-list do
      ;; Get the state with lowest f-cost
      (let ((current (pop-min-f-cost open-list)))
        
        ;; Check if this is a goal state
        (when (goal-reached-p current goal)
          (return (extract-plan current)))
        
        ;; Add to closed list
        (push current closed-list)
        
        ;; Generate successors
        (dolist (action actions)
          (when (action-applicable-p action current)
            (let ((successor (apply-action action current)))
              
              ;; Compute heuristic
              (setf (pddl-state-h-cost successor)
                    (funcall heuristic-fn successor goal))
              
              ;; Check if successor is in closed list
              (unless (member successor closed-list :test #'state-equal-p)
                
                ;; Find in open list
                (let ((existing (find successor open-list :test #'state-equal-p)))
                  (if existing
                      ;; Update if better path found
                      (when (< (pddl-state-g-cost successor)
                               (pddl-state-g-cost existing))
                        (setf (pddl-state-parent existing) (pddl-state-parent successor))
                        (setf (pddl-state-action existing) (pddl-state-action successor))
                        (setf (pddl-state-g-cost existing) (pddl-state-g-cost successor)))
                      
                      ;; Add to open list
                      (push successor open-list)))))))))))

(defun pop-min-f-cost (open-list)
  "Pops the state with minimum f-cost from open list"
  (let* ((min-state (first open-list))
         (min-f-cost (+ (pddl-state-g-cost min-state)
                        (pddl-state-h-cost min-state)))
         (min-idx 0))
    
    ;; Find state with minimum f-cost
    (loop for state in (rest open-list)
          for idx from 1
          for f-cost = (+ (pddl-state-g-cost state)
                          (pddl-state-h-cost state))
          when (< f-cost min-f-cost)
          do (setf min-state state
                   min-f-cost f-cost
                   min-idx idx))
    
    ;; Remove from list and return
    (if (= min-idx 0)
        (pop open-list)
        (let ((result min-state))
          (setf open-list (remove min-state open-list :test #'eq :count 1))
          result))))

(defun state-equal-p (state1 state2)
  "Checks if two states are equal (same predicates and function values)"
  (and (set-equal (pddl-state-predicates state1)
                 (pddl-state-predicates state2)
                 :test #'equal)
       (set-equal (pddl-state-functions state1)
                 (pddl-state-functions state2)
                 :test #'equal)))

(defun goal-reached-p (state goal)
  "Checks if the goal condition is satisfied in the state"
  (predicate-true-p goal state))

(defun extract-plan (goal-state)
  "Extracts the plan from goal state back to initial state"
  (let ((plan nil)
        (current goal-state))
    (loop while (pddl-state-parent current) do
      (push (pddl-state-action current) plan)
      (setf current (pddl-state-parent current)))
    plan))

(defun manhattan-distance-heuristic (state goal)
  "Estimates cost based on Manhattan distance to diamonds"
  ;; For simplicity, just count remaining unachieved goals
  (let ((count 0))
    (when (eq (first goal) 'and)
      (dolist (subgoal (rest goal))
        (unless (predicate-true-p subgoal state)
          (incf count))))
    count))

;;;
;;; PDDL PROBLEM SOLVING
;;;

(defun extract-actions (domain)
  "Extracts actions from domain definition"
  (let ((actions nil))
    (dolist (part (cddr domain))
      (when (eq (first part) :action)
        (push (create-action part) actions)))
    actions))

(defun create-action (action-def)
  "Creates an action structure from action definition"
  (let ((name (second action-def))
        (parameters nil)
        (preconditions nil)
        (effects nil))
    (dolist (part (cddr action-def))
      (case (first part)
        (:parameters (setf parameters (second part)))
        (:precondition (setf preconditions (second part)))
        (:effect (setf effects (second part)))))
    (list :name name
          :parameters parameters
          :preconditions preconditions
          :effects effects)))

(defun make-action (action-def)
  "Creates an action structure from action definition"
  (let ((name (second action-def))
        (parameters nil)
        (preconditions nil)
        (effects nil))
    (dolist (part (cddr action-def))
      (case (first part)
        (:parameters (setf parameters (second part)))
        (:precondition (setf preconditions (second part)))
        (:effect (setf effects (second part)))))
    (list :name name
          :parameters parameters
          :preconditions preconditions
          :effects effects)))

(defun action-name (action)
  "Gets the name of an action"
  (first (getf action :name)))

(defun action-preconditions (action)
  "Gets the preconditions of an action"
  (third (getf action :preconditions)))

(defun action-effects (action)
  "Gets the effects of an action"
  (third (getf action :effects)))

(defun extract-init (problem)
  "Extracts init section from problem definition"
  (dolist (part (cddr problem))
    (when (eq (first part) :init)
      (return (rest part)))))

(defun extract-goal (problem)
  "Extracts goal section from problem definition"
  (dolist (part (cddr problem))
    (when (eq (first part) :goal)
      (return (second part)))))

(defun extract-predicates (init)
  "Extracts predicates from init section"
  (remove-if (lambda (x) (and (listp x) (eq (first x) '=))) init))

(defun extract-functions (init)
  "Extracts functions from init section"
  (let ((functions nil))
    (dolist (expr init)
      (when (and (listp expr) (eq (first expr) '=))
        (push (cons (second expr) (third expr)) functions)))
    functions))

(defun solve-pddl-problem (domain problem)
  "Solves a PDDL problem using the given domain"
  (let* ((domain-name (second (first domain)))
         (problem-name (second (first problem)))
         (actions (extract-actions domain))
         (init (extract-init problem))
         (goal (extract-goal problem))
         
         ;; Create initial state
         (initial-state (make-pddl-state
                        :predicates (extract-predicates init)
                        :functions (extract-functions init)
                        :parent nil
                        :action nil
                        :g-cost 0
                        :h-cost 0)))
    
    ;; Compute initial heuristic
    (setf (pddl-state-h-cost initial-state)
          (manhattan-distance-heuristic initial-state goal))
    
    ;; Perform search
    (format t "~%Starting A* search for problem ~A in domain ~A" 
            problem-name domain-name)
    
    (let ((plan (a-star-search initial-state goal actions 
                              #'manhattan-distance-heuristic)))
      (if plan
          (progn
            (format t "~%Plan found with ~A steps:" (length plan))
            (dolist (action plan)
              (format t "~%  ~A" (action-name action)))
            plan)
          (format t "~%No plan found.")))))

;;;
;;; PDDL ACTION EXECUTION
;;;

;;; Simplified planning functions
(defun initialize-planning-system ()
  "Initializes the planning system"
  (setf *current-plan* nil
        *plan-index* 0
        *replanning-required* t)
  (format t "~%Planning system initialized"))

(defun get-next-action-key ()
  "Gets the next key to press - simplified version"
  (let ((player-type (get-player-type)))
    (if (string= player-type "disc")
        ;; Disc actions: move right and occasionally jump
        (if (= (random 5) 0) "w" "d")
        ;; Rect actions: move right and occasionally stretch
        (if (= (random 5) 0) "w" "d"))))

;; Remove all the complex PDDL functions for now, and use this simplified approach
(defun plan-and-execute ()
  "Generates a simple plan - move toward diamonds"
  (format t "~%Planning: using simplified movement strategy")
  (setf *current-plan* '("d" "d" "w" "d" "d"))
  (setf *plan-index* 0)
  *current-plan*)

(defun get-next-action-key ()
  "Gets the next key to press based on the current plan"
  (if (and *current-plan* (< *plan-index* (length *current-plan*)))
      (let* ((action (nth *plan-index* *current-plan*))
             (action-name (action-name action))
             (player-type (get-player-type)))
        
        ;; Increment plan index
        (incf *plan-index*)
        
        ;; Translate action to keypress
        (cond
          ;; Disc actions
          ((and (string= player-type "disc") (eq action-name 'move-disc))
           (let* ((params (getf action :parameters))
                  (from (second params))
                  (to (third params)))
             ;; Simple movement left/right based on location names
             (if (string< from to) "d" "a")))
          
          ((and (string= player-type "disc") (eq action-name 'jump-disc))
           "w")  ; Jump
          
          ;; Rect actions
          ((and (string= player-type "rect") (eq action-name 'move-rect))
           (let* ((params (getf action :parameters))
                  (from (second params))
                  (to (third params)))
             ;; Simple movement left/right based on location names
             (if (string< from to) "d" "a")))
          
          ((and (string= player-type "rect") (eq action-name 'stretch-rect))
           "w")  ; Stretch horizontally
          
          ;; Fallback
          (t "d")))  ; Default: move right
      
      ;; If plan exhausted or no plan, do a default action
      (progn
        (setf *replanning-required* t)
        "d")))  ; Default: move right



;;; Define the model
(define-model geomates-enhanced-agent

;;; Define chunk types for game objects, communication, and environment mapping
(chunk-type (polygon-feature (:include visual-location)) regular)
(chunk-type (polygon (:include visual-object)) sides height width rotation diamonds player-type)
(chunk-type (text-feature (:include visual-location)))
(chunk-type (text (:include visual-object)) message sender)
(chunk-type goal state substate last-message)
(chunk-type timing ticks)  ;; Renamed from 'time' to 'timing'
(chunk-type player-info type diamonds)
(chunk-type other-player-info type position-x position-y diamonds)
(chunk-type outgoing-message content)
(chunk-type message-info content sender response-required)
(chunk-type diamond-location id x-pos y-pos distance reachable assigned-to)
(chunk-type environment-map platforms diamonds obstacles)
(chunk-type navigation-target x-pos y-pos status)
(chunk-type plan-strategy mode priority-target fallback-target)  ;; Renamed from 'strategy'
(chunk-type planning-request status action)
(chunk-type planning-response action key)

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


;;; Test export visicon and initialize planning
(p test-export-visicon
   =goal>
      state player-identified
   ?manual>
      state free
==>
   !eval! (test-visicon-export)
   !eval! (initialize-planning-system)
   =goal>
      state initialize-planning
)
#| 
;;; PLANNING PHASE
;;; This is a placeholder for now - will be expanded with actual planning logic
(p initialize-planning-phase
   =goal>
      state initialize-planning
   ?manual>
      state free
==>
   !eval! (format t "~%Planning system initialized. Ready for planning execution~%")
   +manual>
      cmd press-key
      key "d"  ; For now just press 'd' to move right
   =goal>
      state planning-execution
)

;;; For now, just move right as a placeholder action
(p execute-simple-action
   =goal>
      state planning-execution
   ?manual>
      state free
==>
   !eval! (format t "~%Executing simple action (move right)~%")
   +manual>
      cmd press-key
      key "d"
   +temporal>
      isa time
      ticks 0.5  ; Short wait between actions
   =goal>
      state planning-execution
)
|#
;;;
;;; PLANNING PHASE - Productions using PDDL planning
;;;

;;; Initialize planning system and generate first plan
(p initialize-planning-phase
   =goal>
      state initialize-planning
   ?manual>
      state free
==>
   !eval! (format t "~%Planning system initialized. Generating initial plan...~%")
   !eval! (plan-and-execute)
   =goal>
      state planning-execution
)

;;; Get and execute the next action from the plan
(p execute-planned-action
   =goal>
      state planning-execution
   ?manual>
      state free
==>
   !eval! (let ((key (get-next-action-key)))
            (format t "~%Executing planned action: ~A~%" key)
            (set-buffer-chunk 'imaginal 
                             (define-chunks-fct 
                               `((isa planning-response 
                                     action execute 
                                     key ,key)))))
   =goal>
      state execute-key
)

;;; Press the key determined by the planner
(p press-planned-key
   =goal>
      state execute-key
   =imaginal>
      isa planning-response
      action execute
      key =key
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key =key
   +temporal>
      isa time
      ticks 0.5  ; Short wait between actions
   =goal>
      state check-replanning
)

;;; Check if replanning is needed
(p check-if-replanning-needed
   =goal>
      state check-replanning
   ?temporal>
      buffer requested
==>
   !eval! (let ((replan-needed *replanning-required*))
            (format t "~%Checking if replanning needed: ~A~%" replan-needed)
            (if replan-needed
                (set-buffer-chunk 'imaginal 
                                 (define-chunks-fct 
                                   '((isa planning-request status replan))))
                (set-buffer-chunk 'imaginal 
                                 (define-chunks-fct 
                                   '((isa planning-request status continue))))))
   =goal>
      state handle-replanning
)

;;; Handle the case where replanning is needed
(p handle-replanning-needed
   =goal>
      state handle-replanning
   =imaginal>
      isa planning-request
      status replan
==>
   !eval! (format t "~%Replanning needed. Generating new plan...~%")
   !eval! (plan-and-execute)
   =goal>
      state planning-execution
)

;;; Handle the case where we can continue with the current plan
(p handle-continue-execution
   =goal>
      state handle-replanning
   =imaginal>
      isa planning-request
      status continue
==>
   !eval! (format t "~%Continuing with current plan...~%")
   =goal>
      state planning-execution
)

;;; Detect when plan is exhausted
(p plan-exhausted
   =goal>
      state check-replanning
   ?imaginal>
      state free
   !eval! (>= *plan-index* (length *current-plan*))
==>
   !eval! (format t "~%Plan exhausted. Replanning...~%")
   !eval! (setf *replanning-required* t)
   =goal>
      state handle-replanning
)

;;; Fall back to basic movement if planning fails
(p planning-fallback
   =goal>
      state planning-execution
   ?imaginal>
      state free
   !eval! (null *current-plan*)
==>
   !eval! (format t "~%No plan available. Using fallback movement~%")
   +manual>
      cmd press-key
      key "d"
   +temporal>
      isa time
      ticks 0.5
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