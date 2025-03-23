;;;
;;; Fixed ACT-R model for Geomates game
;;; Maintaining existing structure but fixing flow issues
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
;;; ACT-R model for Geomates game - Player Detection Focus
;;;  

;;; Define the model
(define-model geomates-communication-test

;;; Set parameters for the model
(sgp :v t                ; verbose output
     :trace-detail high  ; detailed trace
     :esc t              ; enable subsymbolic computations 
     :mas 3.0            ; maximum associative strength
     :ans 0.5            ; base level constant
     :rt -10.0           ; retrieval threshold (much lower to ensure retrievals always succeed)
     :pm 1               ; partial matching enabled
     :visual-movement-tolerance 3.0 ; tolerance for visual movements
     :visual-num-finsts 10         ; number of visual finsts
     :visual-finst-span 10.0       ; visual finst span
     :show-focus t                 ; show visual focus
     :dat 0.05           ; default action time - make actions quicker
     :randomize-time nil ; don't randomize timing for more predictable behavior 
     :er t               ; enable randomness in utilities
     :egs 0.1            ; expected gain noise
     :time-master-start-increment 1.0 ; for temporal module timing
     :time-mult 1.0                   ; for accurate temporal delays
     )

;;; Define chunk types needed for player detection and messaging
(chunk-type (polygon-feature (:include visual-location)) regular)
(chunk-type (polygon (:include visual-object)) sides height width rotation diamonds player-type)
(chunk-type goal state phase)
(chunk-type time ticks)
(chunk-type player-state player-type rect-x rect-y rect-width test-sent)
(chunk-type position-record rect-x rect-y test-sent player-type)
(chunk-type player-info type)

;; Message-related chunk types
(chunk-type (text-feature (:include visual-location)))
(chunk-type (text (:include visual-object)) message sender)
(chunk-type message-info content sender response-required)
(chunk-type message-test state count last-sent timer)
(chunk-type outgoing-message content priority)

(chunk-type message-state phase current-message last-sent-time waiting-for-response)

;;; Do this to avoid warnings when chunks are created
(define-chunks true false polygon)

;;; Set vision module to look for the lowest (nearest) screen-x item first
(set-visloc-default screen-x lowest)

;;; Add declarative memory chunks
(add-dm
 ;; Keyboard controls
 (w) (a) (s) (d)
 
 ;; Goal states
 (first-step) (second-step) (initializing-game) (warming-up-game) (waiting-for-objects)
 (checking-for-rect) (ready-for-test) (waiting-for-response) (checking-rect) (player-identified)
 (message-testing) (preparing-message) (message-sent) (waiting-before-next) 
 (checking-message) (reading-message) (evaluating-message) (detecting-player)
 (message-phase-init)
 (message-phase-send-key)
 (message-phase-send-content)
 (message-phase-wait)
 (message-phase-check)
 ;; Phase tracking
 (phase-warming-up) (phase-detecting) (phase-messaging)(messaging-start)
 
 ;; Message testing control
 (message-test-control isa message-test state ready count 0 last-sent 0 timer 5.0)
  (msg-control isa message-state 
              phase message-phase-init 
              current-message 1 
              last-sent-time 0 
              waiting-for-response nil)
 ;; Test messages to send
 (test-msg-1 isa outgoing-message 
             content (:hello :i-am "testing communication") 
             priority 1)
 (test-msg-2 isa outgoing-message 
             content (:status :position (:x 25 :y 30) :player-type "unknown") 
             priority 2)
 (test-msg-3 isa outgoing-message 
             content (:ready-to-collaborate :task "diamond collection") 
             priority 3)
             
 ;; Initial goal and position record
 (first-goal isa goal state first-step phase phase-warming-up)
 (initial-positions isa position-record rect-x nil rect-y nil test-sent nil player-type unknown)
)

;;; Set initial goal
(goal-focus first-goal)

;;; GAME INITIALIZATION LOGIC 

;; Production to detect connected text and initialize game
(p step-1
   =goal>
      state first-step
      phase phase-warming-up
   ?manual>
      state free
==>
   =goal>
      state second-step
   +visual-location>
     kind text 
   !output! (find 'connected' text preparing to initialize game)
)

(p detect-connected-text
   =goal>
      state second-step
      phase phase-warming-up
   =visual-location>
     kind text
==>
   =goal>
      state initializing-game
   =visual-location>
   +visual>
      cmd move-attention
      screen-pos =visual-location
   !output! (Found 'connected' text preparing to initialize game)
)

;; Production to send key after attending to text
(p send-initial-key
   =goal>
      state initializing-game
      phase phase-warming-up
   =visual-location>
      kind text
   ?manual>
      state free
==>
   =goal>
      state warming-up-game
   +manual>
      cmd press-key
      key w
   !output! (Sending initial key to initialize game world)
)

; Add a new production that waits a moment before checking for objects
(p wait-for-game-initialization
   =goal>
      state warming-up-game
      phase phase-warming-up
   ?manual>
      state free
==>
   =goal>
      state warming-up-game  ; Stay in the same state
   +manual>
      cmd press-key
      key w              ; Press a neutral key that doesn't affect gameplay
   !output! (Waiting for game to initialize - delaying check)
)

;; Add a production to record the start time
(p record-start-time
   =goal>
      state warming-up-game
      phase phase-warming-up
   ?manual>
      state free
==>
   =goal>
      state waiting-for-objects  ; New intermediate state
   !bind! =start-time (mp-time)  ; Record current time
   !output! (Recording start time =start-time)
)

;; Then check if enough time has passed
(p finish-warming-up
   =goal>
      state waiting-for-objects
==>
   :!eval! (> (- (mp-time) =start-time) 1.0)  ; True if 1+ seconds passed
   =goal>
      state detecting-player
      phase phase-detecting
   !output! (Game should be initialized now - starting player detection)
)

;;; Production rules for detecting player type
(p check-player-info-direct
   =goal>
      state detecting-player
      phase phase-detecting
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      player-type =type
   !output! (Looking for direct player info)
)



)