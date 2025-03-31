(define (domain geomates)
  (:requirements :typing :fluents :negative-preconditions)
  
  (:types
    agent - object       ; disc or rect players
    disc rect - agent    ; subtypes of agent
    diamond - object     ; collectible items
    platform - object    ; solid platforms
    location - object    ; positions in the world
  )

  (:predicates
    (at ?a - agent ?l - location)             ; agent is at location
    (diamond-at ?d - diamond ?l - location)   ; diamond is at location
    (collected ?d - diamond)                  ; diamond has been collected
    (can-reach ?a - agent ?l1 ?l2 - location) ; agent can move from l1 to l2
    (adjacent ?l1 ?l2 - location)             ; locations are adjacent
    (on-platform ?a - agent ?p - platform)    ; agent is on a platform
    (can-jump ?a - disc)                      ; disc can jump now
    (can-stretch ?a - rect)                   ; rect can stretch now
  )

  (:functions
    (distance ?l1 ?l2 - location)       ; distance between locations
    (x-coord ?l - location)             ; x coordinate
    (y-coord ?l - location)             ; y coordinate
    (agent-width ?a - agent)            ; width of agent (especially for rect)
    (agent-height ?a - agent)           ; height of agent
  )

  (:action move-left
    :parameters (?a - agent ?from ?to - location)
    :precondition (and 
                    (at ?a ?from)
                    (adjacent ?from ?to)
                    (< (x-coord ?to) (x-coord ?from))
                  )
    :effect (and 
              (not (at ?a ?from))
              (at ?a ?to)
            )
  )

  (:action move-right
    :parameters (?a - agent ?from ?to - location)
    :precondition (and 
                    (at ?a ?from)
                    (adjacent ?from ?to)
                    (> (x-coord ?to) (x-coord ?from))
                  )
    :effect (and 
              (not (at ?a ?from))
              (at ?a ?to)
            )
  )

  (:action jump
    :parameters (?a - disc ?from ?to - location)
    :precondition (and 
                    (at ?a ?from)
                    (can-reach ?a ?from ?to)
                    (can-jump ?a)
                    (> (y-coord ?to) (y-coord ?from))
                  )
    :effect (and 
              (not (at ?a ?from))
              (at ?a ?to)
              (not (can-jump ?a))
            )
  )

  (:action stretch-horizontally
    :parameters (?a - rect)
    :precondition (and 
                    (can-stretch ?a)
                  )
    :effect (and 
              (increase (agent-width ?a) 0.1)
              (decrease (agent-height ?a) 0.05)
              (not (can-stretch ?a))
            )
  )

  (:action compress-horizontally
    :parameters (?a - rect)
    :precondition (and 
                    (can-stretch ?a)
                  )
    :effect (and 
              (decrease (agent-width ?a) 0.1)
              (increase (agent-height ?a) 0.05)
              (not (can-stretch ?a))
            )
  )

  (:action collect-diamond
    :parameters (?a - agent ?l - location ?d - diamond)
    :precondition (and 
                    (at ?a ?l)
                    (diamond-at ?d ?l)
                    (not (collected ?d))
                  )
    :effect (and 
              (collected ?d)
              (not (diamond-at ?d ?l))
            )
  )
)