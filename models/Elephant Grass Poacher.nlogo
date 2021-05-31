extensions [cgp]

globals [generation]

elephants-own [energy age]
poachers-own [economy cooldown]
patches-own [countdown trap]

breed [elephants elephant]
breed [poachers poacher]
breed [rangers ranger]
breed [traps a-trap]


to setup
  clear-all
  reset-ticks

  create-elephants num-elephants [
    setxy random-xcor random-ycor
    set color gray + 2
    set shape "elephant"
    set size 4.5
    set energy 100
    cgp:add-cgps 21 3 4 12 12 ;; inputs outputs lvls rows cols
    set age 1
  ]

   create-poachers num-poachers [
    setxy random-xcor random-ycor
    set shape "poacher"
    set color black
    set size 3.0
    set economy 0
    cgp:add-cgps 21 3 2 4 4
  ]

;  repeat num-rangers [
;    ask one-of patches with [not any? rangers-here] [sprout-rangers 1 [
;      set shape "ranger"
;      set color blue
;      set size 3.0
;      ]
;    ]
;  ]
  ask patches [
;    ifelse pxcor = 0 [ ];set pcolor black]
;    [
      set pcolor one-of [ green brown ]
      ifelse pcolor = green
      [
        set countdown (grass-regrowth-time * 5)
      ]
      [
        set countdown random (grass-regrowth-time * 5)
      ]
;    ]
  ]
end


to go
  ask elephants [
    let obs get-observations
    let action-vector cgp:get-action obs
;    show action-vector

    ifelse action-vector = (list 0 0 0)
    [
      let action one-of [1 2 3]
      if action = 1 [
        fd 0.5
        set energy energy - 1
      ]
      if action = 2 [
        lt 20
        set energy energy - 0.5
      ]
      if action = 3 [
        rt 20
        set energy energy - 0.5
      ]
    ]
    [
      ;; get cumulative sums
      let cum-sum (list)
      set cum-sum lput (item 0 action-vector) cum-sum
      set cum-sum lput (item 0 action-vector + item 1 action-vector) cum-sum
      set cum-sum lput (item 1 cum-sum + item 2 action-vector) cum-sum

;      show action-vector

      let n random-float sum action-vector

      (ifelse n < (item 0 cum-sum) [
        ;; do first action
        fd 0.2
        set energy energy - 1
      ]
      n < (item 1 cum-sum) [
        ;; do second action
        lt 20
        set energy energy - 0.5
      ]
      n <= (item 2 cum-sum) [
        ;; do third action
        rt 20
        set energy energy - 0.5
      ]
      [
        ;; else should never come here
          print "Should not be here"
      ])
    ]
    eat
    reproduce
    check-trap
    check-death
    set age age + 1
  ]
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;; POACHERS SECTION
  ;;;;;;;;;;;;;;;;;;;;;;;;;

  ; elephant kill is gain of 10700 USD
  ; trap cost is 2000 USD
  ask poachers [
    let obs get-observations
    let action-vector cgp:get-action obs
        ifelse action-vector = (list 0 0 0)
    [
      let action one-of [1 2 3]
      if action = 1 [fd 0.2]
      if action = 2 [lt 20]
      if action = 3 [rt 20]
;      if action = 4 [place-trap]
    ]
    [
      ;; get cumulative sums
      let cum-sum (list)
      set cum-sum lput (item 0 action-vector) cum-sum
      set cum-sum lput (item 0 action-vector + item 1 action-vector) cum-sum
      set cum-sum lput (item 1 cum-sum + item 2 action-vector) cum-sum
;      set cum-sum lput (item 1 cum-sum + item 2 action-vector + item 3 action-vector) cum-sum

      let n random-float sum action-vector


      (ifelse n < (item 0 cum-sum) [
        ;; do first action
        fd 0.2
      ]
      n < (item 1 cum-sum) [
        ;; do second action
        lt 20
      ]
      n < (item 2 cum-sum) [
        ;; do third action
        rt 20
      ]
      [
        ;; else should never come here
          print "Should not be here"
      ])
    ]
    kill-elephant
    ifelse cooldown = 0  [
      place-trap
    ]
    [
     set cooldown cooldown - 1
    ]
    check-bankrupt
  ]

  ask patches [
   grow-grass
  ]

  if ticks mod num-generation = 0 [
     new-gen
   ]
  if count elephants = 0 [ stop ]
  tick
end

to kill-elephant
  let prey one-of elephants-here
  if prey != nobody [
   ; net gain of 10000
    ask prey [die]
    set economy economy + 10000
  ]
end

to place-trap
  if economy > 2000
  [
    if random 100 < prob-set-trap and trap = 0 [
;      print "Set trap"
      set trap 1
      set cooldown trap-cooldown
      set economy economy - 2000
      hatch-traps 1[
        set shape "trap"
        set color red
      ]
    ]
  ]
end

to check-trap
  if trap = 1 [
   ask traps-on patch-here [die]
   set trap 0
   die
  ]
end

to eat
  if pcolor = green [
    set pcolor brown
    set energy energy + elephant-gain-from-food
  ]
end


to-report get-observations
  let obs []
  rt 30
  repeat 3 [
    set obs sentence obs (get-in-cone 7 20)
    lt 20
  ]
  rt 30
  set obs map [i -> i / 7] obs
  report obs
end

to-report get-in-cone [dist angle]
  let obs []
  let cone other turtles in-cone dist angle
  let el min-one-of cone with [is-elephant? self] [distance myself]
  if-else el = nobody [
    set obs lput 0 obs
  ]
  [
    set obs lput (7 - ((distance el) / 2)) obs
  ]
  let p min-one-of cone with [is-poacher? self] [distance myself]
  if-else p = nobody [
    set obs lput 0 obs
  ]
  [
    set obs lput (7 - ((distance p) / 2)) obs
  ]
  let r min-one-of cone with [is-ranger? self] [distance myself]
  if-else r = nobody [
    set obs lput 0 obs
  ]
  [
    set obs lput (7 - ((distance r) / 2)) obs
  ]
  let pag min-one-of patches in-cone (dist) (angle) with [pcolor = green] [distance myself]
  if-else pag = nobody [
    set obs lput 0 obs
  ]
  [
    set obs lput (7 - ((distance pag) / 2)) obs
  ]
  let pab min-one-of patches in-cone (dist) (angle) with [pcolor = brown] [distance myself]
  if-else pab = nobody [
    set obs lput 0 obs
  ]
  [
    set obs lput (7 - ((distance pab) / 2)) obs
  ]
  let ytrap min-one-of patches in-cone (dist) (angle) with [trap = 1] [distance myself]
  if-else ytrap = nobody [
    set obs lput 0 obs
  ]
  [
    set obs lput (7 - ((distance ytrap) / 2)) obs
  ]
  let ntrap min-one-of patches in-cone (dist) (angle) with [trap = 0] [distance myself]
  if-else ntrap = nobody [
    set obs lput 0 obs
  ]
  [
    set obs lput (7 - ((distance ntrap) / 2)) obs
  ]
  report obs
end

to grow-grass
  if pcolor = brown [
   ifelse countdown <= 0
   [
      set pcolor green
      set countdown (grass-regrowth-time * 3)
   ]
   [
      set countdown countdown - 1
   ]
  ]
end


to move
  fd 1
end

to reproduce
  ;; in here, mutate to generate an offspring
  if random-float 100 < busses-reproduce and energy > 100[  ; throw "dice" to see if you will reproduce
    set energy (energy / 2)               ; divide energy between parent and offspring
    hatch 1 [
      rt random-float 360 fd 1
      cgp:mutate-reproduce myself mutation-diff-percent 21 3 4 12 12
      set age 1
    ]  ; hatch an offspring and move it forward 1 step
  ]
end

to new-gen
  new-gen-poachers
  new-gen-rangers
end

to new-gen-rangers

end

to new-gen-poachers
  set generation generation + 1
  let best-poacher one-of (poachers with-max [economy])
  if best-poacher != nobody [
    ask poachers with [who != [who] of best-poacher] [die ]
    repeat num-poachers [
      create-poachers 1 [
        setxy ([xcor] of best-poacher) ([ycor] of best-poacher)
        set shape "poacher"
        set color black
        set size 3.0
        cgp:mutate-reproduce best-poacher mutation-diff-percent 21 3 2 4 4
        set economy 0
        setxy random-xcor random-ycor
        set cooldown trap-cooldown
      ]
    ]
    ask best-poacher [die]
  ]
end

to check-death
  if age > max-age [cgp:clear-cgp die]
  if energy < 0 [cgp:clear-cgp die]
end

to check-bankrupt
  if economy < 0 [cgp:clear-cgp die]
end
@#$#@#$#@
GRAPHICS-WINDOW
398
13
894
510
-1
-1
14.8
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
119
315
187
348
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

BUTTON
25
314
91
347
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

SLIDER
21
14
193
47
num-elephants
num-elephants
0
100
9.0
1
1
NIL
HORIZONTAL

SLIDER
21
55
193
88
num-poachers
num-poachers
0
100
4.0
1
1
NIL
HORIZONTAL

SLIDER
21
96
193
129
num-rangers
num-rangers
0
100
2.0
1
1
NIL
HORIZONTAL

SLIDER
8
142
226
175
elephant-gain-from-food
elephant-gain-from-food
0
100
10.0
1
1
NIL
HORIZONTAL

PLOT
135
355
378
527
Populations
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
"elephants" 1.0 0 -16777216 true "" "plot count elephants"
"poachers" 1.0 0 -7500403 true "" "plot count poachers"

SLIDER
18
184
203
217
grass-regrowth-time
grass-regrowth-time
100
1000
120.0
10
1
NIL
HORIZONTAL

MONITOR
31
367
119
412
max-energy
max [energy] of elephants
17
1
11

SLIDER
23
229
195
262
busses-reproduce
busses-reproduce
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
22
267
194
300
num-generation
num-generation
20
1000
200.0
5
1
NIL
HORIZONTAL

MONITOR
240
127
330
172
Generation
generation
17
1
11

MONITOR
42
421
114
466
Max Econ
max [economy] of poachers
17
1
11

SLIDER
213
234
385
267
prob-set-trap
prob-set-trap
0
100
5.0
5
1
NIL
HORIZONTAL

SLIDER
213
275
385
308
trap-cooldown
trap-cooldown
20
500
80.0
20
1
NIL
HORIZONTAL

SLIDER
210
16
382
49
max-age
max-age
500
2000
1010.0
10
1
NIL
HORIZONTAL

MONITOR
248
71
305
116
Age
max [age] of elephants
17
1
11

SLIDER
212
191
406
224
mutation-diff-percent
mutation-diff-percent
0
0.2
0.05
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

Elephants, Poacher, Ranger model where poachers kill elephants and rangers kill poachers

## HOW IT WORKS

Agents call the cgp extension to get their next best action.

## HOW TO USE IT

num-elephants: number of elephants to create intiially 
num-rangers: number of rangers to create initially
num-poachers: number of poachers to create initially

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

elephant
true
0
Polygon -7500403 true true 150 240
Polygon -7500403 true true 165 270 105 225 105 120 150 120 105 120 105 60 150 30 210 45 225 60 210 75 180 60 195 90 240 90 240 120 195 120 195 165 210 180 240 165 240 210 195 210 180 240 150 240 180 270 165 270
Circle -16777216 false false 150 45 0
Circle -1 false false 150 45 0
Circle -1 false false 150 45 0
Circle -1 true false 135 75 0

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

poacher
true
0
Circle -7500403 true true 56 56 67
Polygon -7500403 true true 75 120 75 210 105 210 105 120 75 120
Polygon -7500403 true true 75 210 75 270 90 270 90 210 75 210
Polygon -7500403 true true 105 210 120 270 105 270 90 210 105 210
Polygon -7500403 true true 195 165 195 180 180 180 180 165 195 165
Polygon -7500403 true true 180 150 180 195 120 195 120 150 180 150
Polygon -7500403 true true 135 195 135 240 150 240 150 195 135 195
Polygon -7500403 true true 105 165 120 165 120 180 105 180 105 165

ranger
true
0
Circle -7500403 true true 120 60 60
Polygon -7500403 true true 135 120 135 195 165 195 165 120 135 120
Polygon -7500403 true true 135 195 135 255 150 255 150 195 135 195
Polygon -7500403 true true 165 195 180 255 165 255 150 195 165 195
Polygon -7500403 true true 120 75 105 45 195 45 180 75 150 60 135 60 120 75
Polygon -7500403 true true 90 165
Polygon -7500403 true true 135 135 120 195 105 180 135 120 135 135
Polygon -7500403 true true 165 120 195 180 180 195 165 135 165 120

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

trap
true
1
Polygon -7500403 true false 90 120 105 75 120 120 90 120
Polygon -7500403 true false 135 120 150 75 165 120 135 120
Polygon -7500403 true false 180 120 195 75 210 120 180 120
Polygon -7500403 true false 90 180 105 135 120 180 90 180
Polygon -7500403 true false 135 180 150 135 165 180 135 180
Polygon -7500403 true false 180 180 195 135 210 180 180 180
Polygon -7500403 true false 90 240 105 195 120 240 90 240
Polygon -7500403 true false 135 240 150 195 165 240 135 240
Polygon -7500403 true false 180 240 195 195 210 240 180 240

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
0
@#$#@#$#@
