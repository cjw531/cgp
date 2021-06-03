;;; TA COMMENTS

;;; Make sure to submit a packaged jar for CGP when you finally submit. I had to build from your github
;;; and that version doesn't seem aligned with the below so I can't run your model.

;;; Don't forget about filling out the INFO tab.

extensions [cgp] ;; import cgp extension

;; counter values for generation, number of pick-up, drop-offs, charing per each generation, and the sum of the fitness valu
globals [generation count-pickup count-dropoff count-charging sum-fitness]

;; turtles have battery rate, reward for each actions that they take, finess value evaluated after each generation,
;; tick counter to hold robot during charing, and distances from package and drop-off zone
turtles-own [battery action-reward fitness charging-tick distance-from-package distance-from-destination]

;; 3 types of agents: robot, items, and charging station
breed [robot a-robot]
breed [package a-package]
breed [charger a-charger]

to setup
  clear-all
  reset-ticks

  ;; set global variables
  set count-pickup 0
  set count-dropoff 0
  set count-charging 0
  set generation 1
  set sum-fitness 0

  repeat initial-num-robot [ ;; create robots
    ask one-of patches with [not any? turtles-here ;; do not place them over one another
      and (pxcor != max-pxcor and pxcor != min-pxcor and pycor != max-pycor and pycor != min-pycor)] [ ;; do not locate in wall area
      sprout-robot 1 [ ;; create a robot
        set shape "emblem" ;; robot shape as in emblem
        set size 2 ;; enlarge size a bit
        set battery 100 ;; initially fully charged
        set heading 0 ;; reset heading to point the top part as its facade
        rt (random 360) ;; random heading
        set color green ;; robots without items (initially) are in green color
        set charging-tick 0 ;; initially charging not in hold
        set fitness 0 ;; initially 0 because has not been evaluated yet
        set distance-from-package sqrt((14 - xcor) ^ 2) ;; calculate the initial distance from package
        set distance-from-destination sqrt((-16 - xcor) ^ 2) ;; calculate the initial distance from drop-off zone
        cgp:add-cgps num-input num-output num-arity num-lv-back num-row num-col ;; create cgp object with inputs
      ]
    ]
  ]

  create-package-charger ;; create package and charging stations

  ask patches [ set pcolor gray + 1 ] ;; set background color
  ask patches with [(pxcor >= -24) and (pxcor < -14)] [ set pcolor 118 ] ;; set color for the package drop-off zone
  ask patches with [(pxcor >= 12) and (pxcor < 24)] [ set pcolor 108 ] ;; set package pick-up zone color

  ;; set wall(boundary) color into red
  ask patches with [ pxcor = max-pxcor or pxcor = min-pxcor or pycor = max-pycor or pycor = min-pycor ] [ set pcolor red ]
end

to go
  ask robot [ ;; manipulate robots' movement depending on charging status
    let tick-check charging-tick ;; set it as local variable to avoid changing it by accident
    (ifelse tick-check = 0 [ ;; not in charge
      observation-to-action ;; put observation of each robot to cgp and solve the cgp
      if battery < 0 [die] ;; check whether the robot should die or not
      action-score ;; check for their task performance
    ] charging-tick = 1 [ ;; charging on hold but move out robot
      fd 1.5 ;; send robot out of charging station
      set charging-tick charging-tick - 1 ;; make it 0
    ]
    [ set charging-tick charging-tick - 1 ]) ;; charing on hold
  ]

  reproduce ;; reproduce 3 agents in after each tick
  if not any? robot [ stop ] ;; if all of the robot died out, stop
  if generation > max-num-generation [ ;; check if it exceed max generation
    set generation generation - 1 ;; since it adds +1 automatically, take the offset
    stop ;; stop if exceed the max generation
  ]
  tick ;; continue
end

to observation-to-action
  let obs get-observations ;; get the observation vector
  let action-vector cgp:get-action obs ;; pass the observation vector into the cgp extension, solve the cgp network recursively
  ;; returns the predicted action set (compose of a probability set, the sum of the list is 1) after solving the network

  ifelse action-vector = (list 0 0 0) [ ;; if the action vector does not have an action to choose
    let action one-of [1 2 3] ;; randomly pick one action out of 1, 2, and 3
    if action = 1 [fd 1] ;; move forward if it is 1
    if action = 2 [lt 20] ;; turn left if it is 2
    if action = 3 [rt 20] ;; turn right if it is 3
    ;; no battery penalty here if there is no action set returned, otherwise robots will die out and no evolution happens
  ]
  [
    ;; calculate cumulative sum
    ;; CDF reference: https://softwareengineering.stackexchange.com/questions/150616/get-weighted-random-item
    let cumulative-sum (list) ;; initialize an empty list
    set cumulative-sum lput (item 0 action-vector) cumulative-sum ;; put the first value and append
    set cumulative-sum lput (item 0 action-vector + item 1 action-vector) cumulative-sum ;; add the first and the second value then append
    set cumulative-sum lput (item 1 cumulative-sum + item 2 action-vector) cumulative-sum ;; add the first, second, and third value then append

    let n random-float sum action-vector ;; pick a random value, out of the total value of the action-vector

    ;; choose an action to perform, based on cumulative distribution function
    (ifelse n < (item 0 cumulative-sum) [ ;; if random value less than the first value
      fd 1 ;; move forward
      set battery battery - 1 ;; used some battery here (more than turning)
    ]
    n < (item 1 cumulative-sum) [ ;; if random value less than the second value
      lt 20 ;; turn left
      set battery battery - 0.5 ;; used some battery here
    ]
    n < (item 2 cumulative-sum) [ ;; if random value less than the third value
      rt 20 ;; turn right
      set battery battery - 0.5 ;; used some battery here
    ]
    [
      ;; empty else block
        print "Should not be here"
    ])
  ]
end

to action-score
  let pick one-of package-here ;; check for pick-up
  if pick != nobody [ ;; if there is a package to pick-up
    if color != orange [ ;; if the robot is not carrying any package yet
      ask pick [die] ;; pick up the package
      set action-reward action-reward + 5000 ;; reward for picking up an item
      set color orange ;; change the robot color into orange to mark it as carrying a package
      set count-pickup count-pickup + 1 ;; number of pick-up count +1
    ]
  ]

  let destination patch-here ;; check if it is on drop-off area
  if destination != nobody and pcolor = 118 [ ;; if robot located in drop-off zone
    ifelse color = orange [ ;; if robot is carrying the package
      set action-reward action-reward + 30000 ;; reward for deliverying the package
      set color green ;; hands free now, so color it back to green
      set count-dropoff count-dropoff + 1 ;; number of drop-off count +1
    ]
    [ set action-reward action-reward - 10 ] ;; penalize for staying here w/o item
  ]

  let wall patch-here ;; check whether the robot is in wall
  if wall != nobody and pcolor = red [ ;; if bumped
    set action-reward action-reward - 5 ;; penalize for bumping into the wall
    rt 180 ;; turn around
    fd 1 ;; move off from the wall
  ]

  let charging one-of charger-here ;; recharge the robot
  if charging != nobody and charging-tick = 0 [ ;; if robot stop by at the charging station
    set charging-tick ceiling (tick-per-generation * 0.05) + 1 ;; hold for 5% time of each generation
    set battery 100 ;; charge the battery to full
    set count-charging count-charging + 1 ;; number of charging count +1
    set action-reward action-reward + 10 ;; give reward for stopping by at the charging station
  ]

  let current-distance-from-package sqrt((14 - xcor) ^ 2) ;; calculate the current distance between package and the robot
  let current-distance-from-destination sqrt((-16 - xcor) ^ 2) ;; calculate the current distance between destination area and the robot
  ifelse color = orange [ ;; if robot carrying package
    (ifelse current-distance-from-package < distance-from-package [ set action-reward action-reward +  150 ] ;; if approaching to drop-off zone, reward
      current-distance-from-package = distance-from-package [ set action-reward action-reward - 30 ] ;; if stagnated, penalize
    [ set action-reward action-reward - 50 ] ) ;; if moving further away from the drop-off zone, penalize
  ]
  [ ;; if robot without carrying package
    (ifelse current-distance-from-destination < distance-from-destination [ set action-reward action-reward + 150 ] ;; if approaching to pick-up area, reward
    current-distance-from-destination = distance-from-destination [ set action-reward action-reward - 30 ] ;; if stagnated, penalize
    [ set action-reward action-reward - 50 ]) ;; if moving further away from pick-up area, penalize
  ]
end

to-report get-observations
  let obs [] ;; initialize the merged observation vector for the 3 angles (front, left, right)
  rt 20 ;; right turn by 20 (observation start from the most right side)
  repeat 3 [ ;; repeat 3 times for right-front-left
    set obs sentence obs (get-in-cone 7 20) ;; get the observation vector, append it
    lt 20 ;; left turn by 20 to go to front and then left one
  ]
  rt 40 ;; come back to its original position
  set obs map [i -> i / 7] obs ;; map distance values to range from 0-1
  report obs ;; return the final observation vector
end

to-report get-in-cone [dist angle]
  let obs [] ;; initialize observation vector

  let cone other turtles in-cone 7 20 ;; get turtles within the cone area
  let front-patches patches in-cone 7 20 ;; get patches within the cone area

  let r min-one-of cone with [is-a-robot? self] [distance myself] ;; get the distance between one of the min robot agent from the current itself
  if-else r = nobody [ set obs lput 0 obs ] ;; if none found, put 0
  [ set obs lput (7 - ((distance r) / 2)) obs ] ;; calculate the distance, offset it to make the range between 0-7

  let p min-one-of cone with [is-a-package? self] [distance myself] ;; get the distance between one of the min package agent from the current itself
  if-else p = nobody [ set obs lput 0 obs ] ;; if none found, put 0
  [ set obs lput (7 - ((distance p) / 2)) obs ] ;; calculate the distance, offset it to make the range between 0-7

  let c min-one-of cone with [is-a-charger? self] [distance myself] ;; get the distance between one of the min charger agent from the current itself
  if-else c = nobody [ set obs lput 0 obs ] ;; if none found, put 0
  [ set obs lput (7 - ((distance c) / 2)) obs ] ;; calculate the distance, offset it to make the range between 0-7

  let w min-one-of front-patches with [pcolor = red] [distance myself] ;; get the distance between one of the min wall patch from the current itself
  if-else w = nobody [ set obs lput 0 obs ] ;; if none found, put 0
  [ set obs lput (7 - ((distance w) / 2)) obs ] ;; calculate the distance, offset it to make the range between 0-7

  let d min-one-of front-patches with [pcolor = 118] [distance myself] ;; get the distance between one of the min drop-off patch from the current itself
  if-else d = nobody [ set obs lput 0 obs ] ;; if none found, put 0
  [ set obs lput (7 - ((distance d) / 2)) obs ] ;; calculate the distance, offset it to make the range between 0-7

  report obs ;; return the observation vector
end

to create-package-charger
  repeat initial-num-package [ ;; create packages
    ask one-of patches with [not any? turtles-here and (pxcor >= 12) and (pxcor < 24) ;; no overlapping packages, locate right side
      and (pxcor != max-pxcor and pxcor != min-pxcor and pycor != max-pycor and pycor != min-pycor)] [ ;; do not locate in wall area
      sprout-package 1 [ ;; create a package
        set shape "box" ;; box shape
        set color brown ;; brown box
        set size 1.5 ;; enlarge size a bit
      ]
    ]
  ]

  repeat initial-num-charger [ ;; create charging stations
    ask one-of patches with [not any? turtles-here and (pxcor > -14) and (pxcor < 12) ;; no overlapping charging stations, locate right side
      and (pxcor != max-pxcor and pxcor != min-pxcor and pycor != max-pycor and pycor != min-pycor)] [ ;; do not loate in wall area
      sprout-charger 1 [ ;; create a charging station
        set shape "building store" ;; gas station shape
        set color blue ;; colored in blue
        set size 1.5 ;; enlarge size a bit
      ]
    ]
  ]
end

to reproduce
  if ticks mod tick-per-generation = 0 and ticks != 0 [ ;; in every num-generation tick
    set sum-fitness 0 ;; reset the fitness sum for the current generation
    ask robot [ ;; evalute fitness value of each turtle
      set color red ;; to make it die, differentiate newly created ones and old ones
      set fitness action-reward + battery / 10 ;; calculate fitness value
      set sum-fitness sum-fitness + fitness ;; add up all fitness value
    ]

    repeat(initial-num-robot / 10) [ ;; pick the top (initial-num-robot / 10) number of parents
      let parent one-of (robot with-max [fitness]) ;; get the turtle with the highest fitness value
      repeat 10 [ ;; copy the robot software into the 10 new child robots
        ask one-of patches with [not any? turtles-here ;; do not overlap turtles
          and (pxcor != max-pxcor and pxcor != min-pxcor and pycor != max-pycor and pycor != min-pycor)] [ ;; do not locate in wall area
          sprout-robot 1 [ ;; create robot
            set shape "emblem" ;; robot shape as in emblem
            set size 2 ;; enlarge size a bit
            set battery 100 ;; initially fully charged
            set heading 0 ;; reset heading to point the top part as its facade
            rt (random 360) ;; random heading
            set color green ;; robots without items (initially) are in green color
            set charging-tick 0 ;; initially charging not in hold
            set fitness 0 ;; initially 0 because has not been evaluated yet
            set distance-from-package sqrt((14 - xcor) ^ 2) ;; calculate the initial distance from package
            set distance-from-destination sqrt((-16 - xcor) ^ 2) ;; calculate the initial distance from drop-off zone
            cgp:mutate-reproduce parent mutation-rate num-input num-output num-arity num-lv-back num-row num-col ;; mutate from the sparent & reproduce
          ]
        ]
      ]
      ask parent [die] ;; kill the parent so that another turtle with the next highest fitness value can be selected
    ]

    ;; kill the old agents from the previous generation
    ask robot with [color = red] [die]
    ask package [die]
    ask charger [die]
    create-package-charger ;; re-create package and charging stations

    set generation generation + 1 ;; increment the generation counter
    set count-pickup 0 ;; reset number of pick-up for the next generation
    set count-dropoff 0 ;; reset number of drop-off for the next generation
    set count-charging 0 ;; reset number of charging for the next generation
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
515
20
1302
553
-1
-1
15.9
1
10
1
1
1
0
0
0
1
-24
24
-16
16
1
1
1
ticks
30.0

BUTTON
130
300
205
340
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
260
90
365
150
mutation-rate
0.05
1
0
Number

BUTTON
46
300
121
340
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
38
355
498
558
Population per Generation
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"num-pickup" 1.0 0 -955883 true "" "if ticks mod tick-per-generation = 0 and ticks != 0 [plot count-pickup]"
"num-dropoff" 1.0 0 -8630108 true "" "if ticks mod tick-per-generation = 0 and ticks != 0 [plot count-dropoff]"
"num-charging" 1.0 0 -13345367 true "" "if ticks mod tick-per-generation = 0 and ticks != 0 [plot count-charging]"

SLIDER
30
90
215
123
initial-num-robot
initial-num-robot
10
50
30.0
10
1
NIL
HORIZONTAL

SLIDER
30
250
215
283
max-num-generation
max-num-generation
0
2000
500.0
100
1
NIL
HORIZONTAL

SLIDER
30
130
215
163
initial-num-package
initial-num-package
30
100
50.0
10
1
NIL
HORIZONTAL

SLIDER
30
170
215
203
initial-num-charger
initial-num-charger
0
5
3.0
1
1
NIL
HORIZONTAL

MONITOR
380
90
475
151
Generation
generation
15
1
15

SLIDER
30
210
215
243
tick-per-generation
tick-per-generation
300
1000
500.0
100
1
NIL
HORIZONTAL

INPUTBOX
30
15
107
75
num-input
10.0
1
0
Number

INPUTBOX
110
15
196
75
num-output
3.0
1
0
Number

INPUTBOX
200
15
275
75
num-arity
2.0
1
0
Number

INPUTBOX
280
15
367
75
num-lv-back
15.0
1
0
Number

INPUTBOX
370
15
434
75
num-row
15.0
1
0
Number

INPUTBOX
440
15
502
75
num-col
15.0
1
0
Number

PLOT
235
170
500
340
Average Fitness Per Generation
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"sum-fitness" 1.0 0 -16777216 true "" "if ticks mod tick-per-generation = 0 and ticks != 0 [plot sum-fitness / count robot]"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

building store
false
0
Rectangle -7500403 true true 30 45 45 240
Rectangle -16777216 false false 30 45 45 165
Rectangle -7500403 true true 15 165 285 255
Rectangle -16777216 true false 120 195 180 255
Line -7500403 true 150 195 150 255
Rectangle -16777216 true false 30 180 105 240
Rectangle -16777216 true false 195 180 270 240
Line -16777216 false 0 165 300 165
Polygon -7500403 true true 0 165 45 135 60 90 240 90 255 135 300 165
Rectangle -7500403 true true 0 0 75 45
Rectangle -16777216 false false 0 0 75 45

bus
false
0
Polygon -7500403 true true 15 206 15 150 15 120 30 105 270 105 285 120 285 135 285 206 270 210 30 210
Rectangle -16777216 true false 36 126 231 159
Line -7500403 false 60 135 60 165
Line -7500403 false 60 120 60 165
Line -7500403 false 90 120 90 165
Line -7500403 false 120 120 120 165
Line -7500403 false 150 120 150 165
Line -7500403 false 180 120 180 165
Line -7500403 false 210 120 210 165
Line -7500403 false 240 135 240 165
Rectangle -16777216 true false 15 174 285 182
Circle -16777216 true false 48 187 42
Rectangle -16777216 true false 240 127 276 205
Circle -16777216 true false 195 187 42
Line -7500403 false 257 120 257 207

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

emblem
true
0
Polygon -7500403 true true 300 210 285 180 15 180 0 210
Polygon -7500403 true true 270 165 255 135 45 135 30 165
Polygon -7500403 true true 240 120 225 90 75 90 60 120
Polygon -7500403 true true 150 15 285 255 15 255
Polygon -16777216 true false 225 225 150 90 75 225

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

my-mouse
true
0
Polygon -7500403 true true 210 75 165 15 135 15 90 75 90 90 75 120 75 135 90 150 120 150 135 135 135 120 135 105 135 135 120 150 90 150 75 135 75 165 75 195 90 225 105 240 120 255 135 255 135 270 150 285 165 285 195 285 210 270 195 270 165 270 150 270 150 255 165 255 210 225 225 195 225 135 210 150 180 150 180 105 225 105 225 135 225 105 210 75 165 15 135 15 90 75 90 90 75 120 75 135 90 150 120 150 135 135 135 105 75 105
Polygon -7500403 true true 225 135 225 105 75 105 75 135
Polygon -7500403 true true 210 75 180 15 135 15 75 105 225 105
Polygon -7500403 true true 180 135 180 150 210 150 225 135 180 135
Polygon -7500403 true true 120 150 90 150 75 135 135 135 120 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
