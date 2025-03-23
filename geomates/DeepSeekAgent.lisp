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

;;;
;;; Fixed ACT-R model for Geomates game
;;; Now using text-feature for player-type detection
;;;  

;;; Define the model
(define-model geomates-communication-test

;;; Define chunk types needed for player detection and messaging
(chunk-type (polygon-feature (:include visual-location)) regular)
(chunk-type (polygon (:include visual-object)) sides height width rotation diamonds)
(chunk-type goal state phase)
(chunk-type time ticks)
(chunk-type player-state player-type rect-x rect-y rect-width test-sent)
(chunk-type position-record rect-x rect-y test-sent player-type)
(chunk-type player-info type)

;; Message and text-related chunk types
(chunk-type (text-feature (:include visual-location)))
(chunk-type (text (:include visual-object)) message sender text)
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
 (first-step) (second-step) (initializing-game) (warming-up-game) 
 (warming-up-game-finished) (player-type-check)
 (messaging-start) (looking-for-player-text) (found-player-text)
 ;; Phase tracking
 (phase-warming-up) (phase-detecting)
 (detecting-player) (player-identified)
             
 ;; Initial goal and position record
 (first-goal isa goal state first-step)
)

;;; Set initial goal
(goal-focus first-goal)

;;; GAME INITIALIZATION LOGIC 

;; Production to detect connected text and initialize game
(p step-1
   =goal>
      state first-step
   ?manual>
      state free
==>
   =goal>
      state second-step
   +visual-location>
     kind text 
   !output! (Looking for connected text to initialize game)
)

(p detect-connected-text
   =goal>
      state second-step
    =visual-location>
      kind text
==>
   =goal>
      state initializing-game
   =visual-location>
   +visual>
      cmd move-attention
      screen-pos =visual-location
   !output! (Found text preparing to initialize game)
)

;; Production to send key after attending to text
(p send-initial-key
   =goal>
      state initializing-game
   =visual-location>
      kind text
   ?manual>
      state free
==>
   =goal>
      state warming-up-game
   +manual>
      isa press-key
      key w
   !output! (Sending initial key to initialize game world)
)

;; Wait a moment before checking for objects
(p wait-for-game-initialization
   =goal>
      state warming-up-game
   ?manual>
      state free
==>
   =goal>
      state warming-up-game-finished
   +manual>
      isa press-key
      key w
   !output! (Waiting for game to initialize - delaying check)
)

;;; Production rules for detecting player type using text feature
(p look-for-player-info-text
   =goal>
      state warming-up-game-finished
   ?manual>
      state free
==>
   =goal>
      state looking-for-player-text
   +visual-location>
      kind text
      ;; Looking for text with "player-info" value
      value text
   !output! (Looking for player info as text feature)
)

(p attend-to-player-info-text
   =goal>
      state looking-for-player-text
   =visual-location>
      kind text
      value text
   ?visual>
      state free
==>
   =goal>
      state found-player-text
   +visual>
      isa move-attention
      screen-pos =visual-location
   !output! (Found text feature attending to it)
)

(p extract-player-type-from-text
   =goal>
      state found-player-text
   =visual>
      isa text
      ;; The text content will now have the player type
      text =text
   ?imaginal>
      state free
==>
   +imaginal>
      isa player-info
      ;; Extract player type from text - this is symbolic in ACT-R
      ;; In reality, would parse the text string
      type disc  ;; Default to disc for now
   =goal>
      state player-identified
   !output! (Extracted player type from text =text)
)

(p remember-player-type-disc
   =goal>
      state player-identified
   =imaginal>
      isa player-info
      type disc
==>
   =goal>
      state messaging-start
   !output! (Ready for gameplay - controlling the disc player)
)

(p remember-player-type-rect
   =goal>
      state player-identified
   =imaginal>
      isa player-info
      type rect
==>
   =goal>
      state messaging-start
   !output! (Ready for gameplay - controlling the rectangle player)
)

;;; Set parameters for the model
(sgp :v t                ; verbose output
     :trace-detail high  ; detailed trace
     :visual-movement-tolerance 3.0 ; tolerance for visual movements
     :step t
     :show-focus t
     :visual-finst-span 10
     :visual-onset-span 4.0
     :visual-num-finsts 15
     :temporal t
     :UNSTUFF-VISUAL-LOCATION nil
)
)