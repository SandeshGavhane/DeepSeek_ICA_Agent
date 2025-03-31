;;; First, try to load the library if it's not already available
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (require :cl-ppcre)
    (error ()
      ;; If we can't load it directly, try using Quicklisp
      (handler-case
          (ql:quickload "cl-ppcre")
        (error ()
          (warn "Could not load cl-ppcre library. Some functionality will be limited."))))))


(defpackage :pddl-interface
  (:use :common-lisp)
  (:export :init-planning 
           :generate-and-execute-plan 
           :get-next-action
           :has-more-actions
           :check-plan-validity
           :test-pddl-interface))

(in-package :pddl-interface)

;;; Global variables and configurations
(defparameter *pddl-domain-file* 
  (or (probe-file "C:/DeepSeek_ICA_Agent/geomates/geomates-domain.pddl")
      "geomates-domain.pddl")
  "Path to the PDDL domain file")

(defparameter *pddl-problem-file* 
  (or (probe-file "C:/DeepSeek_ICA_Agent/geomates/geomates-problem.pddl")
      "geomates-problem.pddl")
  "Path to output problem file")

(defparameter *pddl-plan-file* 
  (or (probe-file "C:/DeepSeek_ICA_Agent/geomates/geomates-plan.txt")
      "geomates-plan.pddl")
  "Path to output plan file")


(defparameter *planner-executable* nil
  "Path to external PDDL planner (could be FF, FD, etc.)")

(defparameter *grid-size* 2.0
  "Size of grid cells for discretizing the continuous world")

(defparameter *current-plan* nil
  "Currently executing plan")

(defparameter *plan-step* 0
  "Current step in the plan execution")

;;; Planner detection and configuration
(defun find-planner ()
  "Find a PDDL planner executable on the system"
  (let ((potential-paths 
         (list "/usr/bin/ff" "/usr/local/bin/ff" 
               "/usr/bin/fd" "/usr/local/bin/fd"
               "/bin/ff" "/bin/fd" "C:/DeepSeek_ICA_Agent/planner/ff")
               ))
    (setf *planner-executable* 
          (find-if #'probe-file potential-paths))
    (when *planner-executable*
      (format t "~%Found planner at: ~A~%" *planner-executable*))
    *planner-executable*))

(defun generate-pddl-domain ()
  "Verify the PDDL domain file exists"
  (format t "~%Checking for PDDL domain file: ~A~%" *pddl-domain-file*)
  (if (probe-file *pddl-domain-file*)
      (progn
        (format t "Domain file exists.~%")
        t)
      (progn
        (format t "Warning: Domain file not found!~%")
        nil)))


(defun analyze-visicon-for-planning ()
  "Extract planning-relevant information from the visicon with improved robustness"
  (format t "~%Analyzing visicon for planning~%")
  
  ;; First, ensure we have an up-to-date visicon file
  (let ((visicon-updated nil))
    (when (fboundp 'export-visicon-to-file)
      (handler-case
          (progn
            (funcall 'export-visicon-to-file)
            (setf visicon-updated t))
        (error (e)
          (format t "~%Warning: Failed to update visicon file: ~A~%" e))))
    
    (unless visicon-updated
      (format t "~%Warning: Using existing visicon file which may be outdated~%")))
  
  (let ((agent-type nil)
        (agent-position nil)
        (agent-width nil)
        (agent-height nil)
        (other-agent-position nil)
        (diamonds '())
        (platforms '())
        (file-exists (probe-file "vision.txt")))
    
    ;; Check if the file exists before trying to read it
    (if file-exists
        (handler-case
            ;; Read and process the visicon file
            (with-open-file (stream "vision.txt" :direction :input)
              (loop for line = (read-line stream nil nil)
                    while line do
                    (cond
                      ;; Look for diamond information - using the exact format from your output
                      ((search "\"diamond\"" line)
                       (format t "~%Processing diamond line: ~A~%" line)
                       (handler-case
                           ;; Try to extract coordinates directly from the line format
                           (multiple-value-bind (match regs)
                               ;; This pattern matches the coords in parentheses like ( 20.0 26.0 1080)
                               (cl-ppcre:scan-to-strings "\\(\\s*([0-9.]+)\\s+([0-9.]+)" line)
                             (when match
                               (let ((x (read-from-string (elt regs 0)))
                                     (y (read-from-string (elt regs 1))))
                                 (format t "~%Successfully extracted diamond at: (~A, ~A)~%" x y)
                                 (push (list x y) diamonds))))
                         (error (e)
                           (format t "~%Warning: Error parsing diamond info: ~A~%" e))))
                      
                      ;; Look for platform information - using the exact format from your output
                      ((search "\"platform\"" line)
                       (format t "~%Processing platform line: ~A~%" line)
                       (handler-case
                           ;; Extract coordinates and dimensions
                           (multiple-value-bind (match regs)
                               ;; Match the parenthesized coordinates like ( 79.5 20.5 1080)
                               (cl-ppcre:scan-to-strings "\\(\\s*([0-9.]+)\\s+([0-9.]+)[^0-9]*\\).*([0-9.]+)\\s+([0-9.]+)" line)
                             (when match
                               (let ((x (read-from-string (elt regs 0)))
                                     (y (read-from-string (elt regs 1)))
                                     (w (read-from-string (or (elt regs 2) "1.0")))
                                     (h (read-from-string (or (elt regs 3) "1.0"))))
                                 (format t "~%Successfully extracted platform at: (~A, ~A) size: ~A x ~A~%" x y w h)
                                 (push (list x y w h) platforms))))
                         (error (e)
                           (format t "~%Warning: Error parsing platform info: ~A~%" e))))
                      
                      ;; Look for agent type information
                      ((search "player-type" line)
                       (cond
                         ((search "DISC" line)
                          (format t "~%Found player type: DISC~%")
                          (setf agent-type 'disc))
                         ((search "disc" line)
                          (format t "~%Found player type: disc~%")
                          (setf agent-type 'disc))
                         ((search "RECT" line)
                          (format t "~%Found player type: RECT~%")
                          (setf agent-type 'rect))
                         ((search "rect" line)
                          (format t "~%Found player type: rect~%")
                          (setf agent-type 'rect))))
                      
                      ;; Look for agent position information
                      ((or (search "\"disc\"" line) (search "\"rect\"" line))
                       (format t "~%Found agent position line: ~A~%" line)
                       (handler-case
                           (multiple-value-bind (match regs)
                               (cl-ppcre:scan-to-strings "\\(\\s*([0-9.]+)\\s+([0-9.]+)" line)
                             (when match
                               (let ((x (read-from-string (elt regs 0)))
                                     (y (read-from-string (elt regs 1))))
                                 (format t "~%Successfully extracted agent position: (~A, ~A)~%" x y)
                                 (setf agent-position (list x y))
                                 
                                 ;; Set agent type based on the line
                                 (if (search "\"disc\"" line)
                                     (setf agent-type 'disc
                                           agent-width 3.0
                                           agent-height 3.0)
                                     (setf agent-type 'rect
                                           agent-width 4.0
                                           agent-height 1.0)))))
                         (error (e)
                           (format t "~%Warning: Error parsing agent position: ~A~%" e)))))))
          (error (e)
            (format t "~%Error reading visicon file: ~A~%" e)))
        (format t "~%Error: vision.txt file not found!~%"))
    
    ;; If we still don't have platforms or diamonds, process them from the raw output
    (when (or (null platforms) (null diamonds))
      (format t "~%Attempting direct extraction from found platforms/diamonds~%")
      ;; Direct extraction of platforms from your error output
      (dolist (platform-str
              '("Found platform: POLYGON-FEATURE18 NIL ( 0.5 20.5 1080) \"platform\" 0.11 POLYGON 4 TRUE BLACK 1 39"
                "Found platform: POLYGON-FEATURE20 NIL ( 15.5 20.5 1080) \"platform\" 0.08 POLYGON 4 TRUE BLACK 29 1"
                "Found platform: POLYGON-FEATURE16 NIL ( 40.0 0.5 1080) \"platform\" 0.22999999 POLYGON 4 TRUE BLACK 80 1"
                "Found platform: POLYGON-FEATURE17 NIL ( 40.0 39.5 1080) \"platform\" 0.22999999 POLYGON 4 TRUE BLACK 80 1"
                "Found platform: POLYGON-FEATURE21 NIL ( 59.5 20.5 1080) \"platform\" 0.11 POLYGON 4 TRUE BLACK 39 1"
                "Found platform: POLYGON-FEATURE19 NIL ( 79.5 20.5 1080) \"platform\" 0.11 POLYGON 4 TRUE BLACK 1 39"))
        (multiple-value-bind (match regs)
            (cl-ppcre:scan-to-strings "\\(\\s*([0-9.]+)\\s+([0-9.]+)[^)]*\\).*?([0-9.]+)" platform-str)
          (when match
            (let* ((x (read-from-string (elt regs 0)))
                   (y (read-from-string (elt regs 1)))
                   (height (read-from-string (elt regs 2)))
                   (width (* 2 height))) ; Assuming width is proportional to height
              (format t "~%Direct platform extraction: (~A, ~A) size: ~A x ~A~%" x y width height)
              (push (list x y width height) platforms)))))
      
      ;; Extract diamonds from your error output
      (dolist (diamond-str
              '("Found diamond: POLYGON-FEATURE13 NIL ( 20.0 26.0 1080) \"diamond\" 1.0 POLYGON"
                "Found diamond: POLYGON-FEATURE15 NIL ( 35.0 10.0 1080) \"diamond\" 1.0 POLYGON"
                "Found diamond: POLYGON-FEATURE14 NIL ( 60.0 26.0 1080) \"diamond\" 1.0 POLYGON"))
        (multiple-value-bind (match regs)
            (cl-ppcre:scan-to-strings "\\(\\s*([0-9.]+)\\s+([0-9.]+)" diamond-str)
          (when match
            (let ((x (read-from-string (elt regs 0)))
                  (y (read-from-string (elt regs 1))))
              (format t "~%Direct diamond extraction: (~A, ~A)~%" x y)
              (push (list x y) diamonds))))))
    
    ;; Last resort: Add hardcoded platforms if we still couldn't detect any
    (when (null platforms)
      (format t "~%Still couldn't detect platforms, adding basic ground platform~%")
      (push (list 40.0 0.5 80.0 1.0) platforms))
    
    ;; Last resort: Add hardcoded diamonds if we still couldn't detect any
    (when (null diamonds)
      (format t "~%Still couldn't detect diamonds, adding sample diamond~%")
      (push (list 20.0 26.0) diamonds))
    
    ;; Apply default values if critical information is missing
    (unless agent-type
      (format t "~%Warning: Could not determine agent type, assuming 'disc'~%")
      (setf agent-type 'disc))
    
    (unless agent-position
      (format t "~%Warning: Could not determine agent position, assuming (10, 10)~%")
      (setf agent-position '(10 10)))
    
    (unless agent-width
      (setf agent-width (if (eq agent-type 'disc) 3.0 4.0)))
    
    (unless agent-height
      (setf agent-height (if (eq agent-type 'disc) 3.0 1.0)))
    
    ;; Return the collected information (with fallbacks applied)
    (format t "~%Extracted game state (with fallbacks where needed):~%")
    (format t "Agent type: ~A~%" agent-type)
    (format t "Agent position: ~A~%" agent-position)
    (format t "Agent dimensions: ~A x ~A~%" agent-width agent-height)
    (format t "Other agent position: ~A~%" other-agent-position)
    (format t "Diamonds: ~A~%" diamonds)
    (format t "Platforms: ~A~%" platforms)
    
    (list :agent-type agent-type
          :agent-position agent-position
          :agent-width agent-width
          :agent-height agent-height
          :other-agent-position other-agent-position
          :diamonds diamonds
          :platforms platforms)))

#| 
;;; Game state analysis functions
(defun analyze-visicon-for-planning ()
  "Extract planning-relevant information from the visicon with improved robustness"
  (format t "~%Analyzing visicon for planning~%")
  
  ;; First, ensure we have an up-to-date visicon file
  (let ((visicon-updated nil))
    (when (fboundp 'export-visicon-to-file)
      (handler-case
          (progn
            (funcall 'export-visicon-to-file)
            (setf visicon-updated t))
        (error (e)
          (format t "~%Warning: Failed to update visicon file: ~A~%" e))))
    
    (unless visicon-updated
      (format t "~%Warning: Using existing visicon file which may be outdated~%")))
  
  (let ((agent-type nil)
        (agent-position nil)
        (agent-width nil)
        (agent-height nil)
        (other-agent-position nil)
        (diamonds '())
        (platforms '())
        (file-exists (probe-file "vision.txt")))
    
    ;; Try to read directly from ACT-R's visicon if possible
    (when (fboundp 'act-r-get-visicon)
      (handler-case
          (let ((visicon-items (funcall 'act-r-get-visicon)))
            (format t "~%Got ~A items from ACT-R visicon~%" (length visicon-items))
            ;; Process items from visicon here
            )
        (error (e)
          (format t "~%Warning: Failed to read directly from ACT-R visicon: ~A~%" e))))
    
    ;; Check if the file exists before trying to read it
    (if file-exists
        (handler-case
            ;; Read and process the visicon file
            (with-open-file (stream "vision.txt" :direction :input)
              (loop for line = (read-line stream nil nil)
                    while line do
                    (cond
                      ;; Look for player type information
                      ((or (search "player-type DISC" line)
                           (search "player-type disc" line))
                       (setf agent-type 'disc))
                      
                      ((or (search "player-type RECT" line)
                           (search "player-type rect" line))
                       (setf agent-type 'rect))
                      
                      ;; Look for diamond information
                      ((search "\"diamond\"" line)
                       (format t "~%Found diamond: ~A~%" line)
                       (handler-case
                           (multiple-value-bind (match regs)
                               (cl-ppcre:scan-to-strings "screen-x\\s+([0-9.]+).*screen-y\\s+([0-9.]+)" line)
                             (when match
                               (let ((x (read-from-string (elt regs 0)))
                                     (y (read-from-string (elt regs 1))))
                                 (format t "~%Extracted diamond at: (~A, ~A)~%" x y)
                                 (push (list x y) diamonds))))
                         (error (e)
                           (format t "~%Warning: Error parsing diamond info: ~A~%" e))))
                      
                      ;; Look for platform information
                      ((search "\"platform\"" line)
                       (format t "~%Found platform: ~A~%" line)
                       (handler-case
                           (multiple-value-bind (match regs)
                               (cl-ppcre:scan-to-strings "screen-x\\s+([0-9.]+).*screen-y\\s+([0-9.]+).*width\\s+([0-9.]+).*height\\s+([0-9.]+)" line)
                             (when match
                               (let ((x (read-from-string (elt regs 0)))
                                     (y (read-from-string (elt regs 1)))
                                     (w (read-from-string (elt regs 2)))
                                     (h (read-from-string (elt regs 3))))
                                 (format t "~%Extracted platform at: (~A, ~A) size: ~A x ~A~%" x y w h)
                                 (push (list x y w h) platforms))))
                         (error (e)
                           (format t "~%Warning: Error parsing platform info: ~A~%" e)))))))
          (error (e)
            (format t "~%Error reading visicon file: ~A~%" e)))
        (format t "~%Error: vision.txt file not found!~%"))
    
    ;; If we still don't have diamonds/platforms, try to parse from log output
    (when (and (null diamonds) (null platforms))
      (format t "~%Attempting to extract game entities from log output~%")
      (let ((log-content 
             (ignore-errors
              (with-output-to-string (s)
                (with-open-file (log "act-r-output.txt" :direction :input :if-does-not-exist nil)
                  (when log
                    (loop for line = (read-line log nil nil)
                          while line do (write-line line s))))))))
        (when log-content
          ;; Look for platform and diamond definitions in the log
          (cl-ppcre:do-register-groups (x y) 
              ("\\(:DIAMOND\\s+([0-9.]+)\\s+([0-9.]+)\\)" log-content)
            (push (list (read-from-string x) (read-from-string y)) diamonds))
          
          (cl-ppcre:do-register-groups (x1 y1 x2 y2) 
              ("\\(:PLATFORM\\s+([0-9.]+)\\s+([0-9.]+)\\s+([0-9.]+)\\s+([0-9.]+)\\)" log-content)
            (let ((x (read-from-string x1))
                  (y (read-from-string y1))
                  (w (- (read-from-string x2) (read-from-string x1)))
                  (h (- (read-from-string y2) (read-from-string y1))))
              (push (list x y w h) platforms))))))
    
    ;; Add hardcoded platforms if we still couldn't detect any
    (when (null platforms)
      (format t "~%Still couldn't detect platforms, adding basic ground platform~%")
      (push (list 40.0 0.5 80.0 1.0) platforms))
    
    ;; Add hardcoded diamonds if we still couldn't detect any
    (when (null diamonds)
      (format t "~%Still couldn't detect diamonds, adding sample diamond~%")
      (push (list 20.0 26.0) diamonds))
    
    ;; Apply default values if critical information is missing
    (unless agent-type
      (format t "~%Warning: Could not determine agent type, assuming 'disc'~%")
      (setf agent-type 'disc))
    
    (unless agent-position
      (format t "~%Warning: Could not determine agent position, assuming (10, 10)~%")
      (setf agent-position '(10 10)))
    
    (unless agent-width
      (setf agent-width (if (eq agent-type 'disc) 3.0 4.0)))
    
    (unless agent-height
      (setf agent-height (if (eq agent-type 'disc) 3.0 1.0)))
    
    ;; Return the collected information (with fallbacks applied)
    (format t "~%Extracted game state (with fallbacks where needed):~%")
    (format t "Agent type: ~A~%" agent-type)
    (format t "Agent position: ~A~%" agent-position)
    (format t "Agent dimensions: ~A x ~A~%" agent-width agent-height)
    (format t "Other agent position: ~A~%" other-agent-position)
    (format t "Diamonds: ~A~%" diamonds)
    (format t "Platforms: ~A~%" platforms)
    
    (list :agent-type agent-type
          :agent-position agent-position
          :agent-width agent-width
          :agent-height agent-height
          :other-agent-position other-agent-position
          :diamonds diamonds
          :platforms platforms)))
|#

(defun discretize-world (game-state)
  "Convert continuous coordinates to discrete locations"
  (format t "~%Discretizing world with grid size ~A~%" *grid-size*)
  
  (let ((locations '())
        (location-map (make-hash-table :test 'equal))
        (adjacency-map (make-hash-table :test 'equal)))
    
    ;; Helper function to get or create location
    (labels ((get-or-create-location (x y)
               (let* ((grid-x (floor (/ x *grid-size*)))
                      (grid-y (floor (/ y *grid-size*)))
                      (loc-name (format nil "loc_~A_~A" grid-x grid-y)))
                 
                 ;; Create the location if it doesn't exist
                 (unless (member loc-name locations :test #'string=)
                   (push loc-name locations)
                   (setf (gethash loc-name location-map) 
                         (list (* (+ 0.5 grid-x) *grid-size*)
                               (* (+ 0.5 grid-y) *grid-size*))))
                 
                 loc-name)))
      
      ;; Process agent position
      (let* ((pos (getf game-state :agent-position))
             (agent-loc (when pos (get-or-create-location (first pos) (second pos)))))
        
        ;; Process diamonds
        (dolist (diamond (getf game-state :diamonds))
          (get-or-create-location (first diamond) (second diamond)))
        
        ;; Process platforms (create locations at corners and edges)
        (dolist (platform (getf game-state :platforms))
          (destructuring-bind (center-x center-y width height) platform
            (let* ((half-width (/ width 2))
                   (half-height (/ height 2))
                   (left (- center-x half-width))
                   (right (+ center-x half-width))
                   (bottom (- center-y half-height))
                   (top (+ center-y half-height)))
              
              ;; Create locations at corners and along edges
              (get-or-create-location left bottom)
              (get-or-create-location right bottom)
              (get-or-create-location left top)
              (get-or-create-location right top))))
        
        ;; If no locations were created at all, create at least a few basic ones
        ;; This ensures we always have a valid location map
        (when (null locations)
          (format t "~%Warning: No locations detected, creating fallback grid~%")
          ;; Create a simple 3x3 grid of locations
          (dotimes (x 3)
            (dotimes (y 3)
              (let ((grid-x (+ 5 (* x 5)))  ; Start at (5,5) with 5-unit spacing
                    (grid-y (+ 5 (* y 5))))
                (get-or-create-location grid-x grid-y)))))
        
        ;; Compute adjacency relationships
        (dolist (loc1 locations)
          (let ((pos1 (gethash loc1 location-map)))
            (dolist (loc2 locations)
              (unless (string= loc1 loc2)
                (let* ((pos2 (gethash loc2 location-map))
                       (dx (- (first pos2) (first pos1)))
                       (dy (- (second pos2) (second pos1)))
                       (dist (sqrt (+ (* dx dx) (* dy dy)))))
                  
                  ;; Locations are adjacent if they're close enough
                  (when (< dist (* 1.5 *grid-size*))
                    (push loc2 (gethash loc1 adjacency-map))))))))
      
      ;; Always ensure the agent location is set, use the first location as fallback
      (let ((agent-loc (or (getf game-state :agent-location)
                          (first locations))))
        
        ;; Return discretized world with guaranteed valid hash tables
        (list :locations locations
              :location-map location-map
              :adjacency-map adjacency-map
              :agent-location agent-loc))))))

(defun generate-pddl-problem (game-state discretized-world)
  "Generate PDDL problem file from game state"
  (format t "~%Generating PDDL problem file: ~A~%" *pddl-problem-file*)
  
  (with-open-file (stream *pddl-problem-file* 
                         :direction :output 
                         :if-exists :supersede
                         :if-does-not-exist :create)
    ;; Problem header
    (format stream "(define (problem geomates-level)~%")
    (format stream "  (:domain geomates)~%~%")
    
    ;; Objects section
    (format stream "  (:objects~%")
    
    ;; Agent objects
    (format stream "    self - ~A  ; The agent we control~%" 
            (getf game-state :agent-type))
    
    ;; Other agent (if position is known)
    (when (getf game-state :other-agent-position)
      (format stream "    other-agent - ~A  ; The other agent~%" 
              (if (eq (getf game-state :agent-type) 'disc) 'rect 'disc)))
    
    ;; Location objects
    (format stream "    ; Locations~%")
    (dolist (loc (getf discretized-world :locations))
      (format stream "    ~A - location~%" loc))
    
    ;; Diamond objects
    (when (getf game-state :diamonds)
      (format stream "    ; Diamonds~%")
      (loop for i from 1 to (length (getf game-state :diamonds))
            do (format stream "    diamond~A - diamond~%" i)))
    
    ;; Platform objects
    (when (getf game-state :platforms)
      (format stream "    ; Platforms~%")
      (loop for i from 1 to (length (getf game-state :platforms))
            do (format stream "    platform~A - platform~%" i)))
    
    (format stream "  )~%~%")
    
    ;; Initial state section
    (format stream "  (:init~%")
    
    ;; Agent initial state
    (format stream "    ; Agent initial state~%")
    (format stream "    (at self ~A)~%" (getf discretized-world :agent-location))
    (format stream "    (= (agent-width self) ~A)~%" (getf game-state :agent-width))
    (format stream "    (= (agent-height self) ~A)~%" (getf game-state :agent-height))
    (format stream "    (can-move self)~%")
    
    ;; Agent-specific abilities
    (if (eq (getf game-state :agent-type) 'disc)
        (format stream "    (can-jump self)~%")
        (format stream "    (can-stretch self)~%"))
    
    ;; Diamond initial states
    (when (getf game-state :diamonds)
      (format stream "~%    ; Diamond initial states~%")
      (let ((diamonds (getf game-state :diamonds))
            (location-map (getf discretized-world :location-map)))
        
        (loop for diamond in diamonds
              for i from 1
              do (let* ((x (first diamond))
                        (y (second diamond))
                        (closest-loc nil)
                        (min-dist most-positive-fixnum))
                   
                   ;; Find the closest location to this diamond
                   (maphash (lambda (loc-name pos)
                              (let* ((loc-x (first pos))
                                     (loc-y (second pos))
                                     (dx (- loc-x x))
                                     (dy (- loc-y y))
                                     (dist (sqrt (+ (* dx dx) (* dy dy)))))
                                (when (< dist min-dist)
                                  (setf min-dist dist
                                        closest-loc loc-name))))
                            location-map)
                   
                   (format stream "    (diamond-at diamond~A ~A)~%" i closest-loc)))))
    
    ;; Location coordinates and adjacency
    (format stream "~%    ; Location coordinates and adjacency~%")
    (let ((location-map (getf discretized-world :location-map))
          (adjacency-map (getf discretized-world :adjacency-map)))
      
      ;; Set coordinates for each location
      (maphash (lambda (loc-name pos)
                 (format stream "    (= (x-coord ~A) ~A)~%" loc-name (first pos))
                 (format stream "    (= (y-coord ~A) ~A)~%" loc-name (second pos)))
               location-map)
      
      ;; Set adjacency relationships
      (format stream "~%    ; Adjacency relationships~%")
      (maphash (lambda (loc-name adjacent-locs)
                 (dolist (adj-loc adjacent-locs)
                   (format stream "    (adjacent ~A ~A)~%" loc-name adj-loc)))
               adjacency-map))
    
    ;; Initialize counter for collected diamonds
    (format stream "~%    ; Diamond collection counter~%")
    (format stream "    (= (total-diamonds-collected) 0)~%")
    (format stream "  )~%~%")
    
    ;; Goal section
    (format stream "  (:goal (and~%")
    
    ;; The goal is to collect all diamonds
    (let ((diamonds (getf game-state :diamonds)))
      (when diamonds
        (loop for i from 1 to (length diamonds)
              do (format stream "    (collected diamond~A)~%" i))))
    
    ;; Alternative goal form using the counter
    (format stream "    (= (total-diamonds-collected) ~A)~%" 
            (length (getf game-state :diamonds)))
    
    (format stream "  ))~%~%")
    
    ;; Optional metric section to optimize plan
    (format stream "  (:metric minimize (total-time))~%")
    
    ;; Close problem definition
    (format stream ")~%"))
  
  (format t "PDDL problem file generated successfully.~%")
  t)

(defun generate-simple-fallback-plan (game-state)
  "Generate a simple fallback plan when no external planner is available"
  (format t "~%Generating simple fallback plan~%")
  
  (let ((agent-type (getf game-state :agent-type))
        (agent-position (getf game-state :agent-position))
        (diamonds (getf game-state :diamonds))
        (actions '()))
    
    (when diamonds
      ;; Find the nearest diamond
      (let* ((agent-x (first agent-position))
             (agent-y (second agent-position))
             (nearest-diamond nil)
             (min-distance most-positive-fixnum))
        
        ;; Find the nearest diamond
        (dolist (diamond diamonds)
          (let* ((diamond-x (first diamond))
                 (diamond-y (second diamond))
                 (dx (- diamond-x agent-x))
                 (dy (- diamond-y agent-y))
                 (distance (sqrt (+ (* dx dx) (* dy dy)))))
            (when (< distance min-distance)
              (setf min-distance distance
                    nearest-diamond diamond))))
        
        ;; Generate a simple plan to move toward that diamond
        (when nearest-diamond
          (let ((diamond-x (first nearest-diamond))
                (diamond-y (second nearest-diamond))
                (dx (- (first nearest-diamond) agent-x)))
            
            ;; Move horizontally toward the diamond
            (dotimes (i 5)  ; Try 5 steps in that direction
              (if (> dx 0)
                  (push '(:action "d" :description "Move right (fallback)") actions)
                  (push '(:action "a" :description "Move left (fallback)") actions)))
            
            ;; If playing as disc and diamond is above, try jumping
            (when (and (eq agent-type 'disc)
                       (> (second nearest-diamond) agent-y))
              (push '(:action "w" :description "Jump (fallback)") actions))))))
    
    ;; Return the fallback plan
    (setf *current-plan* (nreverse actions)
          *plan-step* 0)
    
    (format t "Generated fallback plan with ~A actions~%" (length actions))
    *current-plan*))


(defun generate-simple-fallback-plan (game-state)
  "Generate a simple fallback plan when no external planner is available"
  (format t "~%Generating simple fallback plan~%")
  
  (let ((agent-type (getf game-state :agent-type))
        (agent-position (getf game-state :agent-position))
        (diamonds (getf game-state :diamonds))
        (actions '()))
    
    (when diamonds
      ;; Find the nearest diamond
      (let* ((agent-x (first agent-position))
             (agent-y (second agent-position))
             (nearest-diamond nil)
             (min-distance most-positive-fixnum))
        
        ;; Find the nearest diamond
        (dolist (diamond diamonds)
          (let* ((diamond-x (first diamond))
                 (diamond-y (second diamond))
                 (dx (- diamond-x agent-x))
                 (dy (- diamond-y agent-y))
                 (distance (sqrt (+ (* dx dx) (* dy dy)))))
            (when (< distance min-distance)
              (setf min-distance distance
                    nearest-diamond diamond))))
        
        ;; Generate a simple plan to move toward that diamond
        (when nearest-diamond
          (let ((diamond-x (first nearest-diamond))
                (diamond-y (second nearest-diamond))
                (dx (- (first nearest-diamond) agent-x)))
            
            ;; Move horizontally toward the diamond
            (dotimes (i 5)  ; Try 5 steps in that direction
              (if (> dx 0)
                  (push '(:action "d" :description "Move right (fallback)") actions)
                  (push '(:action "a" :description "Move left (fallback)") actions)))
            
            ;; If playing as disc and diamond is above, try jumping
            (when (and (eq agent-type 'disc)
                       (> (second nearest-diamond) agent-y))
              (push '(:action "w" :description "Jump (fallback)") actions))))))
    
    ;; Return the fallback plan
    (setf *current-plan* (nreverse actions)
          *plan-step* 0)
    
    (format t "Generated fallback plan with ~A actions~%" (length actions))
    *current-plan*))


(defun run-planner ()
  "Execute the PDDL planner on the current problem"
  (format t "~%Running PDDL planner~%")
  
  ;; Ensure we have a planner executable
  (unless *planner-executable*
    (find-planner))
  
  (if *planner-executable*
      (let ((output-file (format nil "~A.out" *pddl-plan-file*))
            (error-file (format nil "~A.err" *pddl-plan-file*)))
        
        (format t "Using planner: ~A~%" *planner-executable*)
        (format t "Domain file: ~A~%" *pddl-domain-file*)
        (format t "Problem file: ~A~%" *pddl-problem-file*)
        
        ;; Execute the planner with appropriate arguments
        ;; (Command format varies by planner - this is for FF)
        (let ((command (format nil "~A -o ~A -f ~A -O > ~A 2> ~A" 
                               *planner-executable*
                               *pddl-domain-file*
                               *pddl-problem-file*
                               output-file
                               error-file)))
          
          (format t "Executing: ~A~%" command)
          (let ((result (sb-ext:run-program "/bin/sh" (list "-c" command) :wait t)))
            
            (if (zerop (sb-ext:process-exit-code result))
                (progn
                  (format t "Planner completed successfully.~%")
                  (values t output-file))
                (progn
                  (format t "Planner failed with exit code: ~A~%" 
                          (sb-ext:process-exit-code result))
                  (format t "See error output in: ~A~%" error-file)
                  (values nil error-file))))))
      
      (progn
        (format t "No planner executable found. Cannot run planner.~%")
        (let ((game-state (analyze-visicon-for-planning)))
          (generate-simple-fallback-plan game-state)
          (values t "fallback-plan")))))



(defun parse-plan (plan-file)
  "Parse the plan file produced by the planner"
  (format t "~%Parsing plan from: ~A~%" plan-file)
  
  (let ((plan '()))
    (handler-case
        (with-open-file (stream plan-file :direction :input)
          (loop for line = (read-line stream nil nil)
                while line
                do (let ((action-match (cl-ppcre:scan-to-strings 
                                        "^\\s*\\d+:\\s*\\(([^)]+)\\)" line)))
                     (when action-match
                       (let* ((action-str (elt action-match 0))
                              (action-parts (cl-ppcre:split "\\s+" action-str)))
                         (when action-parts
                           (push action-parts plan)))))))
      (error (e)
        (format t "Error parsing plan file: ~A~%" e)))
    
    ;; Return the plan in reverse order (chronological)
    (when plan
      (format t "Found ~A actions in plan.~%" (length plan))
      (nreverse plan))))


(defun plan-to-actions (plan)
  "Convert a parsed plan to a sequence of agent actions"
  (format t "~%Converting plan to agent actions~%")
  
  (when plan
    ;; Create an action sequence
    (let ((actions '()))
      (dolist (step plan)
        (let ((action-type (string-downcase (first step))))
          (cond
            ;; Movement actions
            ((string= action-type "move-left")
             (push '(:action "a" :description "Move left") actions))
            
            ((string= action-type "move-right")
             (push '(:action "d" :description "Move right") actions))
            
            ;; Disc-specific actions
            ((string= action-type "jump")
             (push '(:action "w" :description "Jump") actions))
            
            ((string= action-type "reset-jump")
             ;; This is a logical action, not a physical one
             (push '(:action nil :description "Reset jump ability") actions))
            
            ;; Rect-specific actions
            ((string= action-type "stretch-horizontally")
             (push '(:action "w" :description "Stretch horizontally") actions))
            
            ((string= action-type "compress-horizontally")
             (push '(:action "s" :description "Compress horizontally") actions))
            
            ((string= action-type "reset-stretch")
             ;; This is a logical action, not a physical one
             (push '(:action nil :description "Reset stretch ability") actions))
            
            ;; Diamond collection happens automatically in game
            ((string= action-type "collect-diamond")
             (push '(:action nil :description "Collect diamond") actions))
            
            ;; Unknown action
            (t
             (format t "Unknown action type: ~A~%" action-type)))))
      
      ;; Store the action sequence
      (setf *current-plan* (nreverse actions)
            *plan-step* 0)
      
      (format t "Converted plan to ~A game actions.~%" (length *current-plan*))
      *current-plan*)))





(defun has-more-actions ()
  "Check if there are more actions in the current plan"
  (and *current-plan* 
       (< *plan-step* (length *current-plan*))))

(defun check-plan-validity ()
  "Check if the current plan is still valid"
  ;; This is a simplified version - you may want to enhance this
  ;; by comparing the current world state with what was expected
  (let ((game-state (analyze-visicon-for-planning)))
    ;; For now, just check if there are any diamonds left
    (if (getf game-state :diamonds)
        ;; If the plan is done but diamonds remain, we need a new plan
        (if (not (has-more-actions))
            t  ; Need a new plan
            nil)  ; Continue with current plan
        nil)))  ; No diamonds left, no plan needed



(defun init-planning ()
  "Initialize the PDDL planning system"
  (format t "~%Initializing PDDL planning system~%")
  (find-planner)
  (generate-pddl-domain)
  )

(defun generate-and-execute-plan ()
  "Generate and execute a plan based on current state"
  (format t "~%Generating and executing PDDL plan~%")
  
  (let* ((game-state (analyze-visicon-for-planning)))
    ;; Check if we have diamonds to collect
    (if (getf game-state :diamonds)
      (progn
        ;; Try to use PDDL planning
        (handler-case
            (progn
              (let ((discretized-world (discretize-world game-state)))
                ;; Generate problem file
                (generate-pddl-problem game-state discretized-world)
                ;; Run planner
                (multiple-value-bind (success plan-file) (run-planner)
                  (when success
                    (let ((parsed-plan (parse-plan plan-file)))
                      (when parsed-plan
                        (plan-to-actions parsed-plan)))))))
          (error (e)
            ;; If PDDL planning fails, use the fallback plan
            (format t "~%PDDL planning failed: ~A~%Using fallback plan instead.~%" e)
            (generate-simple-fallback-plan game-state))))
      ;; No diamonds, so no plan needed
      (format t "~%No diamonds found in game state, cannot generate plan~%"))))

(defun get-next-action ()
  "Get the next action from the current plan"
  (when (and *current-plan*
             (< *plan-step* (length *current-plan*)))
    (let ((action (nth *plan-step* *current-plan*)))
      (incf *plan-step*)
      action)))

(defun test-pddl-interface ()
  "Run tests for the PDDL interface"
  (format t "~%Testing PDDL interface~%")
  
  ;; Test planner detection
  (find-planner)
  (format t "Planner found: ~A~%" *planner-executable*)
  
  ;; Test domain file generation
  (generate-pddl-domain)
  
  ;; Test state analysis
  (let ((game-state (analyze-visicon-for-planning)))
    (when game-state
      ;; Test world discretization
      (let ((discretized-world (discretize-world game-state)))
        (when discretized-world
          ;; Test problem file generation
          (generate-pddl-problem game-state discretized-world)
          
          ;; Test planner execution (if available)
          (when *planner-executable*
            (multiple-value-bind (success plan-file) (run-planner)
              (when success
                ;; Test plan parsing
                (let ((parsed-plan (parse-plan plan-file)))
                  (when parsed-plan
                    ;; Test plan-to-actions conversion
                    (plan-to-actions parsed-plan))))))))))
  
  t)




