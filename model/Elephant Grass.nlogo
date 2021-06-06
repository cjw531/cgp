extensions [cgp]

globals [grass-consumed]

elephants-own [energy age]
patches-own [countdown]

breed [elephants elephant]


to setup
  clear-all
  reset-ticks

  ;; create the elephants in the model with CGPs embedded into them
  create-elephants num-elephants [
    setxy random-xcor random-ycor
    set color gray + 2
    set shape "elephant"
    set size 4.5
    set energy 100
    cgp:add-cgps 9 3 10 12 24 ;; inputs outputs lvls rows cols
    set age 1
  ]

  ;; Randomly create brown and green patches and set countdown times
  ask patches [
      set pcolor one-of [ green brown ]
      ifelse pcolor = green
      [
        set countdown (grass-regrowth-time * 5)
      ]
      [
        set countdown random (grass-regrowth-time * 5)
      ]
  ]
  ;; count the number of patches that are green
  set grass-consumed count patches with [pcolor = green]
end


to go
  ask elephants [
    let obs get-observations ;; extract observations from current view space
    let action-vector cgp:get-action obs ;; get output from embedded CGP using extension
    ;; format action-vector to be probabilities
    set action-vector (map abs action-vector)
    let total (sum action-vector)
    ;; protected division
    ifelse total > 0 [
      set action-vector (map [i -> i / total] action-vector)
    ]
    [
      set action-vector (map [i -> i * 0] action-vector)
    ]
    ifelse action-vector = (list 0 0 0)
    [
      fd 0.2
      set energy energy - 0.2
    ]
    [
      ;; get cumulative sums
      let cum-sum (list)
      set cum-sum lput (item 0 action-vector) cum-sum
      set cum-sum lput (item 0 action-vector + item 1 action-vector) cum-sum
      set cum-sum lput (item 1 cum-sum + item 2 action-vector) cum-sum

      ;; perform CDF for weighted probability
      let n random-float sum action-vector
      (ifelse n < (item 0 cum-sum) [
        ;; move forward
        fd 0.2
        set energy energy - 0.2
      ]
      n < (item 1 cum-sum) [
        ;; turn left
        lt 20
        set energy energy - 0.1
      ]
      n <= (item 2 cum-sum) [
        ;; turn right
        rt 20
        set energy energy - 0.1
      ]
      [
        ;; else should never come here
      ])
    ]
    eat
    reproduce
    check-death
    set age age + 1
  ]

  ask patches [
   grow-grass
  ]


  ;; no elephants left in ecosystem
  if count elephants = 0 [ stop ]


  set grass-consumed (grass-consumed + count patches with [pcolor = green])

  tick
end

;; elephant consumes grass, turning patch from green to brown
to eat
  if pcolor = green [
    set pcolor brown
    set energy energy + elephant-gain-from-food
  ]
end

;; gets observation of elephant agent which includes its cone of vision in three different cones
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

;; put observations into list to then feed into CGP. Adds in information regarding elephants, green patches, and brown patches
to-report get-in-cone [dist angle]
  let obs []
  let cone other turtles in-cone dist angle
  let el min-one-of cone with [is-elephant? self] [distance myself] ;; distance between myself and green patch
  if-else el = nobody [
    set obs lput 0 obs ;; doesn't exist, so 0
  ]
  [
    set obs lput (7 - ((distance el) / 2)) obs
  ]
  let pag min-one-of patches in-cone (dist) (angle) with [pcolor = green] [distance myself] ;; distance between myself and other elephant
  if-else pag = nobody [
    set obs lput 0 obs ;; doesn't exist, so 0
  ]
  [
    set obs lput (7 - ((distance pag) / 2)) obs
  ]
  let pab min-one-of patches in-cone (dist) (angle) with [pcolor = brown] [distance myself];; distance between myself and brown patch
  if-else pab = nobody [
    set obs lput 0 obs ;; doesn't exist, so 0
  ]
  [
    set obs lput (7 - ((distance pab) / 2)) obs
  ]
  report obs
end

;; Patches grow grass based on a countdown
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

;; in here, mutate to generate an offspring
to reproduce
  if energy > 200 [
    set energy (energy / 2) ;; divide energy between parent and offspring
    hatch 1 [
      rt random-float 360 fd 1
      cgp:mutate-reproduce myself mutation-diff-percent
      set age 1
    ]  ; hatch an offspring and move it forward 1 step
  ]
end

;; if an elephant drops below 0 energy, die
to check-death
  if energy < 0 [cgp:clear-cgp die]
end
@#$#@#$#@
GRAPHICS-WINDOW
402
15
911
525
-1
-1
15.2
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
117
189
185
222
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
23
188
89
221
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
50.0
1
1
NIL
HORIZONTAL

SLIDER
21
56
196
89
elephant-gain-from-food
elephant-gain-from-food
0
100
6.0
1
1
NIL
HORIZONTAL

PLOT
19
257
388
525
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

SLIDER
18
95
203
128
grass-regrowth-time
grass-regrowth-time
100
1000
280.0
10
1
NIL
HORIZONTAL

MONITOR
304
87
392
132
max-energy
max [energy] of elephants
17
1
11

MONITOR
236
86
286
131
Age
max [age] of elephants
17
1
11

SLIDER
17
138
204
171
mutation-diff-percent
mutation-diff-percent
0
0.2
0.04
0.01
1
NIL
HORIZONTAL

PLOT
931
236
1300
528
Grass Presence per 1000 ticks
Ticks
Grass Amount
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"grass-consumed" 0.0 0 -16777216 true "" "if ticks mod 1000 = 0 [\nplotxy ticks (grass-consumed / 1000)\nset grass-consumed 0\n]"

@#$#@#$#@
## WHAT IS IT?

This model explores the behavior of elephants in an enviornment with only grass. Elephants are fitted with a cartesian genetic programming network that helps it choose the action to take. 


## HOW IT WORKS

Elephants call the cgp extension to get their next best action and aim to stay alive by consuming grass, which gains energy. Elephants lose energy after each movement. When elephants run out of energy, they die. Grass regrows at a specified rate and is generated randomly by the patches.

## HOW TO USE IT

1. Adjust the slider parameters (see below), of use the default settings. 
2. Press the SETUP button 
3. Press the GO button to begin the simulation of the elephants grass model 
5. Look at the monitors and plots to view the current population size of the elephants and the economy of the poachers. 

num-elephants: Number of elephants to start with 
elephant-gain-from-food: Amount of energy gained from consuming grass 
grass-regrowth-time: Countdown at which grass grows again 
mutation-diff-percent: Probability at which a node can be re-made during mutation 

## THINGS TO NOTICE

Notice that the elephants are learning via a CGP and as such, the longer they stay alive the smarter they become in seeking grass.  

## THINGS TO TRY

Try playing around with the mutation rate as well as trying to limit the population of the elephants to potentially help them learn faster or slower. 

## EXTENDING THE MODEL

More agents can be introduced to the model for extension. Also, grass generation can be extended to mirror more of the natural world in which grass does not grow everywhere. 

## NETLOGO FEATURES

Note the use of the patches to model grass. 

## RELATED MODELS

The sheep grass model would be the closest resemblance. 

## CREDITS AND REFERENCES

Wilensky, U., & Rand, W. (2015). An introduction to agent-based modeling: Modeling natural, social and engineered complex systems with NetLogo. Cambridge, MA: MIT Press.

Miller, Julian F. "Cartesian genetic programming." Cartesian Genetic Programming. Springer, Berlin, Heidelberg, 2011. 17-34.
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
<experiments>
  <experiment name="measure-grass-consumption" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
let grass-consumed 0</setup>
    <go>go</go>
    <metric>set grass-consumed (grass-consumed + count patches with [pcolor = green])</metric>
    <metric>if tick mod 1000 = 0 [</metric>
    <metric>report (grass-consumed / 1000)</metric>
    <metric>set grass-consumed 0</metric>
    <metric>]</metric>
    <enumeratedValueSet variable="mutation-diff-percent">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-elephants">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="elephant-gain-from-food">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-age">
      <value value="450"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="280"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="busses-reproduce">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
