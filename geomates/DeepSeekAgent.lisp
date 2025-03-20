;;;
;;; Complete ACT-R model for Geomates game
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

;;;
;;; Now comes the core Act-R agent
;;;


;;;
;;; Basic ACT-R model for Geomates game - For testing player detection
;;;  

;;;
;;; ACT-R model for Geomates game - Player Detection Focus
;;;  

;;; Define the model
(define-model geomates-player-detection

;;; Define chunk types for game objects and player state awareness
(chunk-type (polygon-feature (:include visual-location)) regular)
(chunk-type (polygon (:include visual-object)) sides height width rotation diamonds player-type)
(chunk-type goal state intention target-x target-y warm-up-count)
(chunk-type control intention button)
(chunk-type time ticks)
(chunk-type player-info type)
(chunk-type diamond-location x y distance)
(chunk-type platform-location x y width height)
(chunk-type path-planning step next-move obstacle-ahead)

;;; Do this to avoid warnings when chunks are created
(define-chunks true false polygon)

;;; Set vision module to look for the lowest (nearest) screen-x item first
(set-visloc-default screen-x lowest)

;;; Add declarative memory chunks
(add-dm
 ;; Movement intentions
 (move-left) (move-right) (move-up) (move-down)
 (jump) (stretch) (compress)
 
 ;; Keyboard controls
 (w) (a) (s) (d)
 
 ;; Goal states
 (warming-up) (warm-up-waiting) (warm-up-delay) (planning) (finding-diamond) (moving-to-diamond) 
 (avoiding-obstacle) (jumping) (transforming)
 
 ;; Control mappings
 (up-control isa control intention move-up button w)
 (down-control isa control intention move-down button s)
 (left-control isa control intention move-left button a)
 (right-control isa control intention move-right button d)
 
 ;; Initial goal
 (first-goal isa goal state warming-up warm-up-count 0)
)

;;; Set initial goal
(goal-focus first-goal)

;;; Warm-up phase productions to initiate visicon updates with delays
(p warm-up-press-key
   =goal>
      state warming-up
      warm-up-count =count
   ?manual>
      state free
   !eval! (and (< =count 5) (evenp =count))  ; Even counts send "d"
==>
   =goal>
      state warm-up-waiting
   +manual>
      cmd press-key
      key "d"  ; Send a movement key to trigger visicon update
)

(p warm-up-press-different-key
   =goal>
      state warming-up
      warm-up-count =count
   ?manual>
      state free
   !eval! (and (< =count 5) (oddp =count))  ; Odd counts send "a"
==>
   =goal>
      state warm-up-waiting
   +manual>
      cmd press-key
      key "a"  ; Send a different movement key
)

(p warm-up-waiting
   =goal>
      state warm-up-waiting
   ?manual>
      state free
==>
   +temporal>
      isa time
      ticks 1.0  ; Wait approximately 1 second
   =goal>
      state warm-up-delay
)

(p warm-up-delay-complete
   =goal>
      state warm-up-delay
      warm-up-count =count
   ?temporal>
      buffer requested
==>
   =goal>
      state warming-up
      warm-up-count (+ =count 1)
)

(p warm-up-complete
   =goal>
      state warming-up
      warm-up-count =count
   ?manual>
      state free
   !eval! (>= =count 5)  ; After sending 5 keys, move to the next state
==>
   =goal>
      state finding-diamond
      warm-up-count 0
)

;;; Production rules for detecting player type
(p detect-player-type
   =goal>
      state finding-diamond
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      value "player-info"
   =goal>
      state planning
)

(p process-player-type
   =goal>
      state planning
   =visual-location>
      isa visual-location
      value "player-info"
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state finding-diamond
)

(p remember-player-type
   =goal>
      state finding-diamond
   =visual>
      isa polygon
      player-type =type
==>
   +imaginal>
      isa player-info
      type =type
   =goal>
      state finding-diamond
)

;;; Production rules for finding and moving to diamonds
(p find-diamond
   =goal>
      state finding-diamond
   ?visual-location>
      state free
   ?imaginal>
     state free
==>
   +visual-location>
      isa visual-location
      value "diamond"
   =goal>
      state moving-to-diamond
)

(p no-diamond-found
   =goal>
      state finding-diamond
   ?visual-location>
      state error
==>
   +manual>
      cmd press-key
      key "d"  ; Send a movement key to update the visicon
   =goal>
      state planning
)

(p focus-on-diamond
   =goal>
      state moving-to-diamond
   =visual-location>
      isa visual-location
      screen-x =x
      screen-y =y
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state planning
      target-x =x
      target-y =y
)

;;; Production rules for disc player (circular character)
(p disc-move-right-to-diamond
   =goal>
      state planning
      target-x =tx
   =visual>
      screen-x =vx
   =imaginal>
      type disc
   ?manual>
      state free
   !eval! (> =tx (+ =vx 2))
==>
   +manual>
      cmd press-key
      key "d"
   =goal>
      state moving-to-diamond
)

(p disc-move-left-to-diamond
   =goal>
      state planning
      target-x =tx
   =visual>
      screen-x =vx
   =imaginal>
      type disc
   ?manual>
      state free
   !eval! (< =tx (- =vx 2))
==>
   +manual>
      cmd press-key
      key "a"
   =goal>
      state moving-to-diamond
)

(p disc-jump-to-diamond
   =goal>
      state planning
      target-y =ty
   =visual>
      screen-y =vy
   =imaginal>
      type disc
   ?manual>
      state free
   !eval! (> =ty (+ =vy 2))
==>
   +manual>
      cmd press-key
      key "w"
   =goal>
      state jumping
)

;;; Production rules for rect player (rectangular character)
(p rect-move-right-to-diamond
   =goal>
      state planning
      target-x =tx
   =visual>
      screen-x =vx
   =imaginal>
      type rect
   ?manual>
      state free
   !eval! (> =tx (+ =vx 2))
==>
   +manual>
      cmd press-key
      key "d"
   =goal>
      state moving-to-diamond
)

(p rect-move-left-to-diamond
   =goal>
      state planning
      target-x =tx
   =visual>
      screen-x =vx
   =imaginal>
      type rect
   ?manual>
      state free
   !eval! (< =tx (- =vx 2))
==>
   +manual>
      cmd press-key
      key "a"
   =goal>
      state moving-to-diamond
)

(p rect-stretch-horizontally
   =goal>
      state planning
   =imaginal>
      type rect
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key "w"
   =goal>
      state transforming
)

(p rect-compress-horizontally
   =goal>
      state planning
   =imaginal>
      type rect
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key "s"
   =goal>
      state transforming
)

;;; General navigation rules
(p after-movement-look-for-diamond
   =goal>
      - state finding-diamond
   ?manual>
      state free
==>
   =goal>
      state finding-diamond
)

(p after-finding-diamond-analyze-position
   =goal>
      state moving-to-diamond
   ?visual>
      state free
==>
   =goal>
      state planning
)

(p at-diamond-look-for-next
   =goal>
      state planning
      target-x =tx
      target-y =ty
   =visual>
      screen-x =vx
      screen-y =vy
   !eval! (and (< (abs (- =tx =vx)) 2)
               (< (abs (- =ty =vy)) 2))
==>
   =goal>
      state finding-diamond
)

;;; Look for platforms to avoid or use
(p locate-platform
   =goal>
      state planning
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      value "platform"
   =goal>
      state avoiding-obstacle
)

(p process-platform
   =goal>
      state avoiding-obstacle
   =visual-location>
      isa visual-location
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state planning
)

;;; Fallback behavior if stuck
(p try-random-move-if-stuck
   =goal>
      state planning
   ?manual>
      state free
   ?imaginal>
      buffer empty
==>
   +manual>
      cmd press-key
      key "d"
   =goal>
      state moving-to-diamond
)

(p try-random-jump-if-stuck
   =goal>
      state planning
   ?manual>
      state free
   ?imaginal>
      buffer empty
==>
   +manual>
      cmd press-key
      key "w"
   =goal>
      state jumping
)

(p random-try-left-if-stuck
   =goal>
      state planning
   ?manual>
      state free
   ?imaginal>
      buffer empty
==>
   +manual>
      cmd press-key
      key "a"
   =goal>
      state moving-to-diamond
)

;;; Set parameters for the model
(sgp :v t                ; verbose output
     :trace-detail high  ; detailed trace
     :esc t              ; enable subsymbolic computations 
     :mas 3.0            ; maximum associative strength
     :ans 0.5            ; base level constant
     :rt 0               ; retrieval threshold
     :pm 1               ; partial matching enabled
     :visual-movement-tolerance 3.0 ; tolerance for visual movements
     :visual-num-finsts 10          ; number of visual finsts
     :visual-finst-span 10.0        ; visual finst span
     :show-focus t                  ; show visual focus
     :dat 0.05           ; default action time - make actions quicker for warm-up
     :randomize-time nil ; don't randomize timing for more predictable behavior
     :er t               ; enable randomness in utilities
     :egs 0.1            ; expected gain noise
     :time-master-start-increment 1.0 ; for temporal module timing
     :time-mult 1.0                   ; for accurate temporal delays
     )
)