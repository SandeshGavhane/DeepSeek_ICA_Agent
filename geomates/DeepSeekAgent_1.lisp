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

  ;; Define chunk types
  (chunk-type (polygon-feature (:include visual-location)) regular rotation diamonds value)
  (chunk-type (polygon (:include visual-object)) sides screen-x screen-y radius rotation)
  (chunk-type (oval (:include visual-object)) screen-x screen-y radius diamonds)
  
  ;; Player info chunk type
  (chunk-type player-info player-type)
  
  ;; Basic player state
  (chunk-type player-state 
    player-type     ; rect or disc
    current-goal)   ; what the agent is trying to do

  ;; Goal state tracking
  (chunk-type goal-state 
    stage)          ; current detection stage
  
  ;; Define basic chunks
  (define-chunks
    (start isa chunk)
    (finding-text isa chunk)
    (found-text isa chunk)
    (sending-init-key isa chunk)
    (checking-player-info isa chunk)
    (waiting-for-objects isa chunk)
    (detected-player isa chunk)
    (moving isa chunk)
    (testing isa chunk)
  )

  ;; Create goal and focus on it
  (add-dm
    (g1 isa goal-state stage start)
  )
  (goal-focus g1)

  ;; 1. Look for connected text
  (p find-connected-text
     =goal>
        stage start
     ?visual-location>
        state free
  ==>
     =goal>
        stage finding-text
     +visual-location>
        kind text
     !output! (Looking for initial "connected" text)
  )

  ;; 2. Attend to found text
  (p attend-to-text
     =goal>
        stage finding-text
     =visual-location>
        kind text
     ?visual>
        state free
  ==>
     =goal>
        stage found-text
     +visual>
        cmd move-attention
        screen-pos =visual-location
     !output! (Found text - attending to it)
  )

  ;; 3. Check if it's connected text and send init key
  (p process-connected-text
     =goal>
        stage found-text
     =visual>
        isa text
        value "connected"
     ?manual>
        state free
  ==>
     =goal>
        stage sending-init-key
     +manual>
        cmd press-key
        key w
     !output! (Found "connected" text - sending initial key press)
  )

  ;; 4. Wait for init key to complete, then look for player info
  (p check-for-player-info
     =goal>
        stage sending-init-key
     ?manual>
        state free
     ?visual-location>
        state free
  ==>
     =goal>
        stage checking-player-info
     +visual-location>
        isa player-info
     !output! (Checking for player info in visicon)
  )

  ;; 5a. Found direct player info through act-r-experiment
  (p found-player-info
     =goal>
        stage checking-player-info
     =visual-location>
        isa player-info
     ?visual>
        state free
  ==>
     =goal>
        stage detected-player
     +visual>
        cmd move-attention
        screen-pos =visual-location
     !output! (Found player info - attending to it)
  )

  ;; 5b. Process the player info
  (p process-player-info
     =goal>
        stage detected-player
     =visual>
        isa player-info
        player-type =type
     ?imaginal>
        state free
  ==>
     =goal>
        stage moving
     +imaginal>
        isa player-state
        player-type =type
        current-goal move-around
     !output! (Detected player type =type from player-info)
  )

  ;; 6. If player info not found, look for game objects
  (p player-info-not-found
     =goal>
        stage checking-player-info
     ?visual-location>
        buffer failure
     ?visual>
        state free
  ==>
     =goal>
        stage waiting-for-objects
     -visual-location>
     +visual-location>
        kind polygon
     !output! (Player info not found - looking for game objects)
  )

  ;; 7. Test with keypresses if direct info not available
  (p test-for-player-type
     =goal>
        stage waiting-for-objects
     =visual-location>
        kind polygon
     ?visual>
        state free
     ?manual>
        state free
  ==>
     =goal>
        stage testing
     +visual>
        cmd move-attention
        screen-pos =visual-location
     +manual>
        cmd press-key
        key a
     !output! (Testing player detection by pressing A key)
  )

  ;; 8. Send more test keypresses after a delay
  (p continue-testing
     =goal>
        stage testing
     ?manual>
        state free
     !eval! (> (mp-time) 0.5)
  ==>
     =goal>
        stage waiting-for-objects
     +manual>
        cmd press-key
        key d
     !output! (Sending another test keypress)
  )

  ;; 9. If player has been detected, start moving
  (p start-moving-if-detected
     =goal>
        stage moving
     =imaginal>
        isa player-state
        player-type =type
     ?manual>
        state free
  ==>
     =goal>
     =imaginal>
     +manual>
        cmd press-key
        key w
     !output! (Player type confirmed as =type - starting to move)
  )

  ;; 10. Continue checking for player-info during testing
  (p check-for-player-info-during-testing
     =goal>
        stage testing
     ?manual>
        state free
     ?visual-location>
        state free
  ==>
     =goal>
        stage checking-player-info
     +visual-location>
        isa player-info
     !output! (Checking again for player info in visicon)
  )
  
  ;; 11. Eventually give up on direct detection and try to infer
  (p try-infer-disc-player
     =goal>
        stage testing
     ?manual>
        state free
     !eval! (> (mp-time) 5.0)  ; After several seconds
  ==>
     =goal>
        stage moving
     +imaginal>
        isa player-state
        player-type disc
        current-goal move-around
     !output! (Time limit reached - assuming DISC player type)
  )
)