;;;
;;; example dummy agent
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; Now comes the core Act-R agent
;;;

(define-model lost-agent1

  ;; [find explanation in actr7.x/examples/vision-module]
  (chunk-type (polygon-feature (:include visual-location)) regular)
  (chunk-type (polygon (:include visual-object)) sides)
  
  ;; [see definition in vision module]
  ;;(chunk-type (oval (:include visual-object)) (oval t))
  
  ;; [might be obsolete] Do this to avoid the warnings when the chunks are created
  (define-chunks true false polygon)
  
  ;; [might be obsolete] stuff the leftmost item
  (set-visloc-default screen-x lowest)

  ;; Set global parameters
    (sgp 
        :step t
        ;; :trace-detail high
        :v t
        :show-focus t
        :visual-finst-span 10
        :visual-onset-span 4.0
        :visual-num-finsts 10
        :temporal t
    )        

    ;; sub-intention:       a gropu of different small proucctioon under main intention
    ;; callback-intention:  when sub-intention is finished, the callback-intention
    ;;                      so the intention/sub-intention can be used as a group of hooks (like UI update for all elements)
    (chunk-type goal
        state
        intention
        sub-intention
        callback-intention
    )
    (chunk-type time ticks)
  (chunk-type control intention button)
  (chunk-type platform-record name x y width height)
  (chunk-type time ticks)

    ;; speed and expand are reserver for future use
    ;; disc speed: 0 not moving, 1 moving right once 2 move right twice etc, and - is moving left
    ;; block expand: 0 not expand, 1 expand  once 2 expand twice etc
    (chunk-type position-record 
                                is-disc speed  disc-x disc-y 
                                is-rect expand rect-x rect-y rect-width rect-height
                                diamond-x diamond-y
                                action-queue
                                )

  (add-dm
   (move-left) (move-right)
   (move-up)  (move-down)
   (w) (a) (s) (d)
   (i-dont-know-where-to-go)
   (something-should-change)
   (i-want-to-do-something)
   (i-dont-know-who-i-am)
   (yellow-disc)
   (red-rect)
   (up-control isa control intention move-up button w)
   (down-control isa control intention move-down button s)
   (left-control isa control intention move-left button a)
   (right-control isa control intention move-right button d)

   (position-record-chunk isa position-record 
                            is-disc 0 speed 0 disc-x nil disc-y nil 
                            is-rect 0 expand 0 rect-x nil rect-y nil 
                            diamond-x nil diamond-y nil 
                            action-queue ""
                            )

   (first-goal isa goal state initializing-game)
   ;; (first-goal isa goal state i-dont-know-who-i-am)
   ;; (first-goal isa goal state i-dont-know-where-to-go disc-x nil disc-y nil rect-x nil rect-y )
   )

   (goal-focus first-goal)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; check if game initialized
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (p initialized-game-check
        =goal>
            state initializing-game
    ==>
        +visual-location>
            value  "disc"
        =goal>
            state initializing-game-state
        !output! ("---- 0.0.0 will check if game initialized")
    )

    (p initialized-game-initializing
        =goal>
            state initializing-game-state
        ?visual-location>
            state error
        ?manual>
            state free
    ==>
        +manual>
            cmd press-key
            key s
        =goal>
            state waiting-for-key-press
        !output! ("---- 0.0.0 Disc not found, pressing some key to initialize game")
    )

    (p wait-for-key-press-complete
        =goal>
            state waiting-for-key-press
        ?manual>
            state free  ; This ensures the key press has completed
    ==>
        =goal>
            state initializing-game
        !output! ("---- 0.0.0 Key press complete, checking again for game initialization")
    )

    (p initialized-game-finished
        =goal>
            state initializing-game-state
        =visual-location>
        ?visual-location>
            state free
    ==>
        =goal>
            state initialize-platforms
        !output! ("---- 0.0.0 Disc found, game initialized")
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; ui-platforms
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Updated productions
    (p platform-find-platforms
        =goal>
            state initialize-platforms
        ?visual>
            state free
        ?imaginal>
            state free
    ==>
        +visual-location>
            value "platform"
            :attended nil
        +imaginal>
            isa platform-record
        =goal>
            intention find-platforms
        !output! ("---- 0.1.0 Searching for platforms")
    )

    (p platform-record-platform
        =goal>
            intention find-platforms
        =visual-location>
            screen-x =x
            screen-y =y
        ?visual>
            state free
        ?imaginal>
            state free
        =imaginal>
            isa platform-record
    ==>
        +visual>
            cmd move-attention
            screen-pos =visual-location
        =imaginal>
            x =x
            y =y
        =goal>
            intention record-platform
        !output! ("---- 0.2.0 Found platform at x=~S y=~S" =x =y)
    )

    (p platform-store-platform
        =goal>
            intention record-platform
        =visual>
            width =w
            height =h
        =imaginal>
            isa platform-record
            x =x
            y =y
    ==>
        ;; Create a new chunk in declarative memory
        +imaginal>
            isa platform-record
            name platform
            x =x
            y =y
            width =w
            height =h
        =goal>
            intention commit-platform
        !output! ("---- 0.3.0 Creating platform in imaginal buffer: x=~S y=~S w=~S h=~S" =x =y =w =h)
    )

    ;; Add a new production to commit the platform to DM
    (p platform-commit-platform-to-dm
        =goal>
            intention commit-platform
        =imaginal>
            isa platform-record
        ?imaginal>
            state free
    ==>
        ;; Clear the imaginal buffer and commit to DM
        -imaginal>
        =goal>
            intention find-platforms
        !output! ("---- 0.3.1 Committed platform to declarative memory")
    )

    ;; Key fix: Handle the case when no more platforms are found
    (p platform-find-failure
        =goal>
            intention find-platforms
        ?visual-location>
            state error
    ==>
        =goal>
            state i-dont-know-who-i-am
            intention nil
        !output! ("---- 0.4.0 No more platforms found, moving to next state")
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; ui-procedurals
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 1 nil
    ;; 2 searching-for-yellow-disc 
    ;; 3 ready-to-find-red-rect
    ;; 4 searching-for-rect 
    ;; 5 ready-to-get-rect-size
    ;; 6 update-ui-end
    (p find-yellow-disc
        =goal>
            intention       update-ui
            sub-intention   nil
        ?visual>
            state free
        ?imaginal>
            state free
        =imaginal>
            isa position-record
    ==>
        +visual-location>
            value       "disc"
        =imaginal>
            isa position-record
        =goal>
            sub-intention searching-for-yellow-disc
        ;; !output! ("---- x.0.0 Searching for yellow disc with specific criteria")
    )

    (p attend-to-yellow-disc
        =goal>
            intention       update-ui
            sub-intention   searching-for-yellow-disc
        =visual-location>
            screen-x =x
            screen-y =y
        ?visual>
            state free
        =imaginal>
    ==>
        +visual>
            cmd move-attention
            screen-pos =visual-location
        =imaginal>
            ;; speed 0
            disc-x =x
            disc-y =y
        =goal>
            sub-intention   ready-to-find-red-rect
        ;; !output! ("---- x.1.0 Moving attention to object at x: ~S y: ~S" =x =y)
    )
        
    ;; Step 1: Record the initial position of the yellow disc
    (p find-red-rect
        =goal>
            intention       update-ui
            sub-intention   ready-to-find-red-rect
        ?visual>
            state free
    ==>
        +visual-location>
            value       "rect"
            ;; :attended   nil
        =goal>
            sub-intention   searching-for-rect
        ;; !output! ("---- x.2.0 Searching for red block with specific criteria")
    )

    (p attend-to-red-rect
        =goal>
            intention       update-ui
            sub-intention   searching-for-rect
        =visual-location>
            screen-x =x
            screen-y =y
        ?visual>
            state free
        =imaginal>
    ==>
        +visual>
            cmd move-attention
            screen-pos =visual-location
        =imaginal>
            ;; expand 0
            rect-x =x
            rect-y =y
        =goal>
            sub-intention   ready-to-get-rect-size
        ;; !output! ("---- x.3.0 Moving attention to object at x: ~S y: ~S" =x =y)
    )

    (p get-rect-size
        =goal>
            intention       update-ui
            sub-intention   ready-to-get-rect-size
        =visual>
            width =w
            height =h
        =imaginal>
        ?imaginal>
            state free
    ==>
        =imaginal>
            rect-width =w
            rect-height =h
        =goal>
            sub-intention   searching-diamond
        ;; !output! ("---- x.4.0 Got rectangle size: width=~S height=~S" =w =h)
    )


    (p find-diamond-locations
        =goal>
            intention       update-ui
            sub-intention   searching-diamond
        ?visual>
            state free
    ==>
        +visual-location>
            value "diamond"
            ;; :attended nil
            screen-y highest
            screen-x lowest
        =goal>
            sub-intention evaluating-diamond-locations
        ;; !output! ("---- x.5.0 Requesting location of leftmost diamond")
    )

    (p attend-closest-diamond
        =goal>
            intention       update-ui
            sub-intention   evaluating-diamond-locations
        =visual-location>
            screen-x =any-diamond-x
            screen-y =any-diamond-y
        ?imaginal> 
            state free
        =imaginal>
            isa position-record
        ?visual>
            state free
        ;; - visual-location>  ;; Ensure this is the first match (simplistic closest check)
    ==>
        +visual>
            cmd move-attention
            screen-pos =visual-location
        =goal>
            intention       update-ui
            sub-intention   update-ui-end
        =imaginal>
            diamond-x =any-diamond-x
            diamond-y =any-diamond-y
        ;; !output! ("---- x.6.0 Attending to diamond at x=~S, y=~S" =any-diamond-x =any-diamond-y)
    )

    (p back-to-parent-goal
        =goal>
            intention          update-ui
            sub-intention      update-ui-end
            callback-intention =callback-intention
    ==>
        =goal>
            intention       =callback-intention
            sub-intention   nil
        !output! ("---- x.7.0 Go back to callback goal")
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; self-location
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (p i-dont-know-who-i-am
        =goal>
            state i-dont-know-who-i-am
            intention nil
        ?imaginal>
            state free
    ==>
        +imaginal>
            isa position-record
            is-disc 0
            is-rect 0
        =goal>
            state               i-dont-know-who-i-am
            intention           update-ui
            sub-intention       nil
            callback-intention  ready-to-move-right
        !output! ("---- 1.0.0 Update UI start")    
    )

    (p move-right-to-location
        =goal>
            intention   ready-to-move-right
        =imaginal>
        ?manual>
            state free
    ==>
        =goal>
            intention   waiting-for-move-update
        +manual>
            cmd press-key
            key d
        =imaginal>
        +temporal>
            isa time
            ticks 0
        !output! ("---- 1.1.0 Moving right, starting timer")
    )

    (p check-ui-update-timer
        =goal>
            intention   waiting-for-move-update
        ?temporal>
            state free
        =temporal>
            isa time
            ticks =ticks
            < ticks 20
    ==>
        =temporal>
        =goal>
            intention   waiting-for-move-update
        !output! ("---- 1.1.1 Still waiting for move update - ticks: ~D" =ticks)
    )

    (p ui-update-complete
        =goal>
            intention   waiting-for-move-update
        ?temporal>
            state free
        =temporal>
            ticks =ticks
            > ticks 20
    ==>
        -temporal>
        =goal>
            intention   ready-re-find-yellow-disc
        !output! ("---- 1.1.2 Move update should be complete, checking positions")
    )

    (p ready-re-find-yellow-disc
        =goal>
            intention   ready-re-find-yellow-disc
        ?visual>
            state free
    ==>
        +visual-location>
            value "disc"
            ;; :attended nil         ;; Look for unattended disc
        =goal>
            intention   searching-for-yellow-disc-after-move
        !output! ("---- 1.2.0 Re-Find yellow disc after moving")
    )

    (p re-attend-to-yellow-disc-no-change
        =goal>
            state       i-dont-know-who-i-am
            intention   searching-for-yellow-disc-after-move
        =visual-location>
            screen-x =new-disc-x
        =imaginal>
            isa position-record
            disc-x =old-disc-x
            disc-x =new-disc-x
        ?visual>
            state free
    ==>
        +visual>
            cmd move-attention  
            screen-pos =visual-location
        =goal>
            state       i-dont-know-who-i-am
            intention   ready-re-find-red-rect
        =imaginal>
            disc-x =new-disc-x
        !output! ("---- 1.2.1 Found disc at same position: x=~S (disc position unchanged from ~S)" =new-disc-x =old-disc-x)
    )

    (p re-attend-to-rect-no-change
        =goal>
            state       i-dont-know-who-i-am
            intention   searching-for-rect-after-move
        =visual-location>
            screen-x =new-rect-x
        =imaginal>
            isa position-record
            rect-x =old-rect-x
            rect-x =new-rect-x
        ?visual>
            state free
    ==>
        +visual>
            cmd move-attention  
            screen-pos =visual-location
        =goal>
            intention   ready-to-move-right
        =imaginal>
            rect-x =new-rect-x
        !output! ("---- 1.3.1 Found red block at same position: x=~S (block position unchanged from ~S)" =new-rect-x =old-rect-x)
    )

    (p re-attend-to-yellow-disc-changed
        =goal>
            state       i-dont-know-who-i-am
            intention   searching-for-yellow-disc-after-move
        =visual-location>
            screen-x =new-disc-x
        =imaginal>
            isa position-record
            disc-x =old-disc-x
            - disc-x =new-disc-x
        ?visual>
            state free
    ==>
        +visual>
            cmd move-attention  
            screen-pos =visual-location
        =goal>
            state       query-moving-collect
            intention   reload-query-move
        =imaginal>
            disc-x =new-disc-x
            is-disc 1
            is-rect 0
        !output! ("---- 1.2.2 Found disc at new position: x=~S (disc position changed from ~S)" =new-disc-x =old-disc-x)
    )

    (p ready-re-find-red-rect
        =goal>
            state       i-dont-know-who-i-am
            intention   ready-re-find-red-rect
        ?visual>
            state free
    ==>
        +visual-location>
            value "rect"
        =goal>
            intention   searching-for-rect-after-move
        !output! ("---- 1.3.0 Re-Find red block after moving")
    )

    (p re-attend-to-rect-changed
        =goal>
            state       i-dont-know-who-i-am
            intention   searching-for-rect-after-move
        =visual-location>
            screen-x =new-rect-x
        =imaginal>
            isa position-record
            rect-x =old-rect-x
            - rect-x =new-rect-x
        ?visual>
            state free
    ==>
        +visual>
            cmd move-attention  
            screen-pos =visual-location
        =goal>
            state       query-moving-collect
            intention   reload-query-move
        =imaginal>
            rect-x =new-rect-x
            is-rect 1
            is-disc 0
        !output! ("---- 1.3.2 Found red block at new position: x=~S (block position changed from ~S)" =new-rect-x =old-rect-x)
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; find nearest diamond
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (p query-moving-collect-reload
        =goal>
            state       query-moving-collect
            intention   queue-query-move
        =imaginal>
            action-queue ""
    ==>
        =imaginal>
        =goal>
            intention          update-ui
            callback-intention reload-query-move
        !output! ("---- 3.1.0 reload action queue")
    )

    (p query-moving-collect-consume
        =goal>
            state       query-moving-collect
            intention   queue-query-move
        =imaginal>
            action-queue =action-queue
            - action-queue ""
    ==>
        =imaginal>
        =goal>
            intention   consume-queue-query-move
        !output! ("---- 3.1.0 consume action queue: ~S" =action-queue)
    )

    (p decide-next-action-disc
        =goal>
            state       query-moving-collect
            intention   reload-query-move
        ?imaginal>
            state free
        =imaginal>
            isa position-record
            is-disc 1
            disc-x =dx
            disc-y =dy
            rect-x =rx
            rect-y =ry
            rect-width =rw
            rect-height =rh
            diamond-x =diamx
            diamond-y =diamy
    ==>
        !bind! =find-action-queue (find-next-action-disc-queue =diamx =diamy =dx =dy =rx =ry =rw =rh)
        !bind! =first-action (subseq  =find-action-queue 0 1)
        !bind! =left-action (subseq =find-action-queue 1)
        !bind! =first-action-symbol (convert-wasd-to-move =first-action)
        =imaginal>
            action-queue =left-action
        =goal>  
            intention  =first-action-symbol
        !output! ("---- 2.1.3b decide next action for disc")
        !output! ("---- 2.1.3b action queue: ~S" =find-action-queue)
        !output! ("---- 2.1.3b first action: ~S" =first-action)
        !output! ("---- 2.1.3b rest action: ~S" =left-action)
        !output! ("---- 2.1.3b first action symbol: ~S" =first-action-symbol)
    )

    (p decide-next-action-rect
        =goal>
            state       query-moving-collect
            intention   reload-query-move
        ?imaginal>
            state free
        =imaginal>
            isa position-record
            is-rect 1
            rect-x =rx
            rect-y =ry
            rect-width =rw
            rect-height =rh
            diamond-x =diamx
            diamond-y =diamy
    ==>
        !bind! =find-action-queue (find-next-action-rect-queue =rx =ry =rw =rh =diamx =diamy)
        !bind! =first-action (subseq  =find-action-queue 0 1)
        !bind! =left-action (subseq =find-action-queue 1)
        !bind! =first-action-symbol (convert-wasd-to-move =first-action)
        =imaginal>
            action-queue =left-action
        =goal>  
            intention  =first-action-symbol
        !output! ("---- 2.1.3a decide next action for rect")
        !output! ("---- 2.1.3a action queue: ~S" =find-action-queue)
        !output! ("---- 2.1.3a first action: ~S" =first-action)
        !output! ("---- 2.1.3a rest action: ~S" =left-action)
        !output! ("---- 2.1.3a first action symbol: ~S" =first-action-symbol)
    )

    (p decide-queue-query-move
            =goal>
                state       query-moving-collect
                intention   consume-queue-query-move
            ?imaginal>
                state free
            =imaginal>
                action-queue =next-action-queue
                - action-queue ""
        ==>
            !bind! =first-action (subseq  =next-action-queue 0 1)
            !bind! =left-action (subseq =next-action-queue 1)
            !bind! =first-action-symbol (convert-wasd-to-move =first-action)

            =imaginal>
                action-queue =left-action
            =goal>
                state       query-moving-collect
                intention   =first-action-symbol
            !output! ("---- 2.1.3b action queue is not empty, consume action queue ~S" =next-action-queue)
            !output! ("---- 2.1.3b first action: ~S" =first-action)
            !output! ("---- 2.1.3b left action: ~S" =left-action) 
            !output! ("---- 2.1.3b first action symbol: ~S" =first-action-symbol)
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (p perform-wait-up
        =goal>
            intention       wait-up
    ==>
        +temporal>
            isa         time
            ticks       0
        =goal>
            intention   waiting-for-move
        !output! ("---- 3.1.3 Waiting for 0.5 seconds before moving")
    )

    (p perform-wait-complete
        =goal>
            intention       waiting-for-move
        =temporal>
            isa         time
            ticks       5
    ==>
        =goal>
            intention   queue-query-move
        !output! ("---- 3.1.3.1 Wait complete, now querying next move")
    )

    (p perform-move-up
        =goal>
            intention       move-up
        ?manual>
            state free
    ==>
        +manual>
            cmd press-key
            key w
            duration 0.2
        =goal>
            intention   queue-query-move
        !output! ("---- 3.1.3 Moving up with 'w'")
    )

    (p perform-move-down
        =goal>
            intention       move-down
        ?manual>
            state free
    ==>
        +manual>
            cmd press-key
            key s
        =goal>
            intention   queue-query-move
        !output! ("---- 3.1.3 Moving down with 's'")
    )

    (p perform-move-left
        =goal>
            intention     move-left
        ?manual>
            state free
    ==>
        +manual>
            cmd press-key
            key a
        =goal>
            intention   queue-query-move
        !output! ("---- 3.1.1 Moving left with 'a'")
    )

    (p perform-move-right
        =goal>
            intention     move-right
        ?manual>
            state free
    ==>
        +manual>
            cmd press-key
            key d
        =goal>
            intention   queue-query-move
        !output! ("---- 3.1.1 Moving right with 'd'")
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (p want-to-move
        =goal>
        state i-want-to-do-something
        intention =intention
        ?retrieval>
        state free
    ==>
        =goal>
        state something-should-change
        +retrieval>
            intention =intention
    )
  
    (p move
        =goal>
        state something-should-change
        =retrieval>
        button =button
        ?manual>
        state free
    ==>
        =goal>
        state i-dont-know-where-to-go
        +manual>
        cmd press-key
        key =button
    )

    (p retrieval-failure
        =goal>
        state something-should-change
        ?retrieval>
        buffer failure
    ==>
        =goal>
            state i-dont-know-where-to-go
   )
  
    (p maybe-left
        =goal>
            state i-dont-know-where-to-go
        ?manual>
            state free
    ==>
        =goal>
            state i-want-to-do-something
            intention move-left
    )
)