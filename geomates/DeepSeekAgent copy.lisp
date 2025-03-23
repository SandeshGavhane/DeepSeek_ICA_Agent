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
  ;; === PLAYER DETECTION PRODUCTIONS ===
  
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
        phase phase-warming-up
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
      state warming-up-game  ; Changed from waiting-for-objects
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

(p finish-warming-up
   =goal>
      state warming-up-game
      phase phase-warming-up
   ?manual>
      state free
   !eval! (> (mp-time) 1)  ; Only match after 800ms of model time has passed
==>
   -visual>
   =goal>
      state detecting-player
      phase phase-detecting
   !output! (Game should be initialized now - ready to check for objects)
)


;;; PLAYER DETECTION PRODUCTIONS


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

;; If player info is found, use it directly
(p use-player-info-direct
   =goal>
      state detecting-player
      phase phase-detecting
   =visual-location>
      isa visual-location
      player-type =type
   ?visual>
      state free
==>
   =visual-location>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state player-identified
      phase phase-detected
   !output! (Found player info directly - checking type)
)


(p remember-player-type
   =goal>
      state player-identified
      phase phase-detected
   =visual>
      isa polygon  ; Changed from 'player-info' to 'polygon' which is the actual visual chunk type
      player-type =type
   ?imaginal>
      state free
==>
   +imaginal>
      isa player-info
      type =type
   =goal>
      state player-identified
      phase messaging-start
   !output! (Remembered we are controlling the ~A player - ready for messaging) =type
)

;; Start message testing once player is identified
(p begin-message-testing
   =goal>
      state player-identified
      phase messaging-start
   =imaginal>  ; This now correctly matches the imaginal buffer produced by remember-player-type
      isa player-info
      type =type  ; Match on the 'type' slot instead of 'player-type'
   ?manual>
      state free
==>
   =goal>
      state messaging
   +retrieval>
      isa message-state
      phase message-phase-init
   !eval! (format t "~%PLAYER DETECTED: We are controlling the ~A player - Starting message test~%" =type)
   !output! (Beginning message testing protocol)
)
;; Step 1: Initialize message sending process
(p initialize-message-sending
   =goal>
      state messaging
   =retrieval>
      isa message-state
      phase message-phase-init
      current-message =msg-num
   ?manual>
      state free
==>
   =goal>
      state messaging
   +retrieval>  ; Request the specific message content
      isa outgoing-message
      priority =msg-num
   +imaginal>   ; Store message state in imaginal buffer
      isa message-state
      phase message-phase-send-key
      current-message =msg-num
   !output! (Initializing message ~D sending process) =msg-num
)

;; Step 2: Press 'm' key to initiate message
(p send-m-key-improved
   =goal>
      state messaging
   =imaginal>
      isa message-state
      phase message-phase-send-key
   =retrieval>
      isa outgoing-message
      content =content
   ?manual>
      state free
==>
   =goal>
      state messaging
   =imaginal>  ; Update phase
      phase message-phase-send-content
   +manual>
      cmd press-key
      key m
   !eval! (format t "~%SENDING 'm' KEY TO INITIATE MESSAGE~%")
   !output! (Sent 'm' key - will send content next)
)

;; Step 3: Direct send of message content using lisp function
;; This explicitly waits for motor module to be free after 'm' key
(p send-message-content-direct
   =goal>
      state messaging
   =imaginal>
      isa message-state
      phase message-phase-send-content
      current-message =msg-num
   =retrieval>
      isa outgoing-message
      content =content
   ?manual>
      state free  ; Ensure motor module is free (after 'm' key press)
==>
   =goal>
      state messaging
   =imaginal>
      phase message-phase-wait
      last-sent-time (mp-time)  ; Record current time
   !eval! (progn
            (format t "~%DIRECTLY SENDING MESSAGE CONTENT: ~S~%" =content)
            (send-message =content))  ; Call external function
   !output! (Sent message content directly - now waiting)
)

;; Step 4: Wait a fixed time after sending message
(p wait-after-message-content
   =goal>
      state messaging
   =imaginal>
      isa message-state
      phase message-phase-wait
      last-sent-time =time
   !eval! (> (- (mp-time) =time) 3)  ; Wait 3 seconds
==>
   =goal>
      state messaging
   =imaginal>
      phase message-phase-check
   !output! (Wait complete - checking for responses)
)

;; Step 5: Check for message responses
(p check-for-message-response
   =goal>
      state messaging
   =imaginal>
      isa message-state
      phase message-phase-check
   ?visual-location>
      state free
==>
   =goal>
      state messaging
   +visual-location>
      isa visual-location
      value "message"  ; Look for message in visicon
   !output! (Looking for message responses)
)

;; Step 6a: Process response if found
(p process-message-response
   =goal>
      state messaging
   =imaginal>
      isa message-state
      phase message-phase-check
   =visual-location>
      isa visual-location
      value "message"
   ?visual>
      state free
==>
   =goal>
      state messaging
   =imaginal>
      waiting-for-response true
   +visual>
      cmd move-attention
      screen-pos =visual-location
   !output! (Found response - reading content)
)

;; Step 6b: Continue to next message if no response found
(p continue-to-next-message-improved
   =goal>
      state messaging
   =imaginal>
      isa message-state
      phase message-phase-check
      current-message =msg-num
   ?visual-location>
      state error  ; No message found
   !eval! (< =msg-num 3)  ; Not the last message
==>
   =goal>
      state messaging
   +retrieval>
      isa message-state
      phase message-phase-init
   -imaginal>  ; Clear imaginal buffer
   !eval! (format t "~%MOVING TO NEXT MESSAGE (~D)~%" (1+ =msg-num))
   !output! (No response found - proceeding to next message)
)

;; Step 7: Send reply to response if needed
(p send-reply-to-response
   =goal>
      state messaging
   =imaginal>
      isa message-state
      waiting-for-response true
   =visual>
      isa text
      message =content
   ?manual>
      state free
==>
   =goal>
      state messaging
   =imaginal>
      waiting-for-response false
      phase message-phase-send-key
   +manual>
      cmd press-key
      key m
   !eval! (format t "~%SENDING REPLY TO MESSAGE: ~S~%" =content)
   !output! (Sending 'm' key to initiate reply)
)

;; Step 8: Send reply content
(p send-reply-content
   =goal>
      state messaging
   =imaginal>
      isa message-state
      phase message-phase-send-content
      waiting-for-response false
   =visual>
      isa text  ; Still have message in visual buffer
   ?manual>
      state free
==>
   =goal>
      state messaging
   =imaginal>
      phase message-phase-init
      current-message (1+ =current-message)
   !eval! (progn
            (format t "~%SENDING REPLY CONTENT: (:acknowledge :received)~%")
            (send-message '(:acknowledge :received)))
   !output! (Sent acknowledgment reply - moving to next message)
)

;; Step 9: Complete testing after all messages
(p finish-message-testing
   =goal>
      state messaging
   =imaginal>
      isa message-state
      phase message-phase-check
      current-message 3  ; Last message
   ?visual-location>
      state error  ; No response found
==>
   =goal>
      state testing-complete
   -imaginal>
   !eval! (format t "~%MESSAGE TESTING COMPLETE - ALL MESSAGES SENT~%")
   !output! (Message testing complete)
)

;;; Utility production to keep checking environment - RESTRICTED TO NON-CRITICAL STATES
(p periodic-visual-update
   =goal>
      - state first-step
      - state second-step
      - state initializing-game
      - state warming-up-game 
      - state waiting-for-rect-response
      - state checking-for-messages
      - state processing-messages
      - state reading-message
      - state message-sent
      - state waiting-before-next-message
      - state detecting-player
      - state player-identified
   ?visual-location>
      state free
   ?manual>
      state free
   ?retrieval>
      state free
==>
   +manual>
      cmd press-key
      key "d"  ; Send a key to update visicon
   =goal>  ; Keep current state
   !output! (Sending periodic key press to update visicon)
)

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
)