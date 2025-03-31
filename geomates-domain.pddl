(define (problem geomates-level)
  (:domain geomates)

  (:objects
    self - RECT  ; The agent we control
    ; Locations
    loc_0_10 - location
    loc_7_10 - location
    loc_20_0 - location
    loc_19_0 - location
    loc_20_19 - location
    loc_19_19 - location
    loc_29_10 - location
    loc_39_10 - location
    loc_10_13 - location
    loc_17_5 - location
    loc_30_13 - location
    loc_20_11 - location
    ; Diamonds
    diamond1 - diamond
    diamond2 - diamond
    diamond3 - diamond
    diamond4 - diamond
    diamond5 - diamond
    diamond6 - diamond
    ; Platforms
    platform1 - platform
    platform2 - platform
    platform3 - platform
    platform4 - platform
    platform5 - platform
    platform6 - platform
  )

  (:init
    ; Agent initial state
    (at self loc_0_10)
    (= (agent-width self) 4.0)
    (= (agent-height self) 1.0)
    (can-move self)
    (can-stretch self)

    ; Diamond initial states
    (diamond-at diamond1 loc_30_13)
    (diamond-at diamond2 loc_17_5)
    (diamond-at diamond3 loc_10_13)
    (diamond-at diamond4 loc_30_13)
    (diamond-at diamond5 loc_17_5)
    (diamond-at diamond6 loc_10_13)

    ; Location coordinates and adjacency
    (= (x-coord loc_20_11) 41.0)
    (= (y-coord loc_20_11) 23.0)
    (= (x-coord loc_30_13) 61.0)
    (= (y-coord loc_30_13) 27.0)
    (= (x-coord loc_17_5) 35.0)
    (= (y-coord loc_17_5) 11.0)
    (= (x-coord loc_10_13) 21.0)
    (= (y-coord loc_10_13) 27.0)
    (= (x-coord loc_39_10) 79.0)
    (= (y-coord loc_39_10) 21.0)
    (= (x-coord loc_29_10) 59.0)
    (= (y-coord loc_29_10) 21.0)
    (= (x-coord loc_19_19) 39.0)
    (= (y-coord loc_19_19) 39.0)
    (= (x-coord loc_20_19) 41.0)
    (= (y-coord loc_20_19) 39.0)
    (= (x-coord loc_19_0) 39.0)
    (= (y-coord loc_19_0) 1.0)
    (= (x-coord loc_20_0) 41.0)
    (= (y-coord loc_20_0) 1.0)
    (= (x-coord loc_7_10) 15.0)
    (= (y-coord loc_7_10) 21.0)
    (= (x-coord loc_0_10) 1.0)
    (= (y-coord loc_0_10) 21.0)

    ; Adjacency relationships
    (adjacent loc_20_0 loc_19_0)
    (adjacent loc_19_0 loc_20_0)
    (adjacent loc_20_19 loc_19_19)
    (adjacent loc_19_19 loc_20_19)

    ; Diamond collection counter
    (= (total-diamonds-collected) 0)
  )

  (:goal (and
    (collected diamond1)
    (collected diamond2)
    (collected diamond3)
    (collected diamond4)
    (collected diamond5)
    (collected diamond6)
    (= (total-diamonds-collected) 6)
  ))

  (:metric minimize (total-time))
)
