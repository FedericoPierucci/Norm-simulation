extensions [CogLogo table]

globals [
  STORAGE
  ACCUMULATED-STORAGE
  COUNTER
  PAYOFF-TICKS
  contribution-days
  INCREMENT-VALUES
  REDISTRIBUTION-TEST
  contribution-test
  body-count
  mean-epsilon
  mean-lambda
  mean-wealth
  deontics
]

turtles-own [
  wealth           ;; the amount of sugar this turtle has
  metabolism      ;; the amount of sugar that each turtles loses each tick
  vision          ;; the distance that this turtle can see in the horizontal and vertical directions
  vision-points   ;; the points that this turtle can see in relative to it's current position (based on vision)
  age
  min-age
  max-age
  min-age-death
  max-age-death
  min-wealth
  max-wealth
  min-metabolism
  max-metabolism
  min-vision
  max-vision
  age-of-death
  epsilon
  lambda
  cooperating
  interaction-memory
  expectations
  incoming-command
  normative-belief
  threshold-1
  threshold-2
  norm-sensitivity
  observed-norm-action
  cooperation-norm
  what-amount-norm
  group
  choices
  stored-lambda
]

patches-own [
  psugar           ;; the amount of sugar on this patch
  max-psugar       ;; the maximum amount of sugar that can be on this patch
  incremented-psugar
]

;;
;; Setup Procedures
;;

to setup
  clear-all
  create-turtles initial-population [ coglogo:init-cognitons turtle-setup ]
  setup-patches
  set storage 0
  set COUNTER 0
  set PAYOFF-TICKS []
  set INCREMENT-VALUES []
  set ACCUMULATED-STORAGE []
  set contribution-days []
  set REDISTRIBUTION-TEST 0
  set contribution-test false
  set body-count 0
  set mean-epsilon []
  set mean-lambda []
  set mean-wealth []
  set deontics ["mandatory" "forbidden"]
  reset-ticks
end

to turtle-setup ;; turtle procedure
  set color red
  set shape "circle"
  move-to one-of patches with [not any? other turtles-here]
  set min-wealth 5
  set max-wealth 25
  set min-metabolism 1
  set max-metabolism 4
  set min-vision 1
  set max-vision 6
  set min-age 0
  set max-age 10
  set min-age-death 80
  set max-age-death 100
  set age random-in-range min-age max-age
  set wealth random-in-range min-wealth max-wealth
  set metabolism random-in-range min-metabolism max-metabolism
  set vision random-in-range min-vision max-vision
  set age-of-death random-in-range min-age-death max-age-death
  set norm-sensitivity random-float 1
  set epsilon random-float 1
  ifelse epsilon >= 0.5 [
    set lambda random-float-in-range 0.5 1
  ]
  [ set lambda random-float-in-range 0.4 0]
  set cooperating 0
  set interaction-memory []
  set expectations []
  set incoming-command []
  set normative-belief []
  set choices []
  set stored-lambda []
  set cooperation-norm table:make
  set what-amount-norm table:make
  set group "none"


  ;; turtles can look horizontally and vertically up to vision patches
  ;; but cannot look diagonally at all
  set vision-points []
  foreach (range 1 (vision + 1)) [ n ->
    set vision-points sentence vision-points (list (list 0 n) (list n 0) (list 0 (- n)) (list (- n) 0))
  ]
  run visualization
end

to setup-patches
  file-open "sugar-map.txt"
  foreach sort patches [ p ->
    ask p [
      set max-psugar file-read
      set psugar max-psugar
      set incremented-psugar []
      patch-recolor
    ]
  ]
  file-close
end



;;
;; Runtime Procedures
;;

to go
  set counter counter + 1
  test-contribution-day
  if not any? turtles [
    stop
  ]
  ask patches [
    patch-growback
    patch-recolor
  ]
  ask turtles [
    set age age + 1
    turtle-move
    turtle-eat
    contribution-cognitions
    if aggression = true [
      turtle-kill
    ]
        if social-behaviour = "mindless-conformers"[
      epsilon-observations
      lambda-observations
      adjust-observations
  ]
    if social-behaviour = "deffuant-and-norms" [
      turtle-talk
      adjust-expectations
      adjust-thresholds
      act-on-choices
      epsilon-action
      build-normative-belief
      select-normative-belief
      lambda-action
      adjust-observations
      enforce-norm
    ]

    run coglogo:choose-next-plan
    coglogo:report-agent-data

    turtle-reproduce

    if wealth <= 0 or age >= age-of-death
      [ die ]
    run visualization
  ]
  update-storage
  carefully [
    set MEAN-EPSILON  mean [epsilon] of turtles
    set MEAN-LAMBDA  mean [lambda] of turtles
    set MEAN-WEALTH mean [wealth] of turtles
  ]
    []
  tick
end

to turtle-move ;; turtle procedure
  ;; consider moving to unoccupied patches in our vision, as well as staying at the current patch
  let move-candidates (patch-set patch-here (patches at-points vision-points) with [not any? turtles-here])
  let possible-winners move-candidates with-max [psugar]
  if any? possible-winners [
    ;; if there are any such patches move to one of the patches that is closest
    move-to min-one-of possible-winners [distance myself]
  ]
end

to turtle-eat ;; turtle procedure
  ;; metabolize some sugar, and eat all the sugar on the current patch
  set wealth (wealth - metabolism + psugar)
  set psugar 0
end

to turtle-kill
  if any? other turtles at-points vision-points [
    if any? other turtles at-points vision-points with [wealth > [wealth] of myself] [
      ifelse random-float 1 < 0.5 [
      let target one-of other turtles at-points vision-points with [wealth > [wealth] of myself]
        let prize [wealth] of target
        let expected-wealth [max-psugar] of patch-here
        if prize > expected-wealth [
        move-to target
        set wealth wealth + prize
        set label "killer"
        set label-color black
        ask target [
          set body-count body-count + 1
          die]
        ]
      ]
      [set label ""]
    ]
      ]

end
to turtle-reproduce
  if age > age-of-reproduction and wealth > wealth-for-reproduction [
    let target-1 [lambda] of self
    let target-2 [epsilon] of self
    set wealth wealth - wealth * reproduction-cost
    hatch 1 [
      turtle-setup
      if random-float 1 < prob-of-inheritance [
        set lambda target-1
        set epsilon target-2
      ]
    ]
  ]
end

to turtle-contribute

  ifelse random-float 1 < epsilon [
    if contribution-test = true [
    let amount-given wealth * (lambda)
    if wealth - amount-given > 0 [
    set wealth wealth - amount-given
      set STORAGE STORAGE + amount-given
      set cooperating 1
      ]
    ]
  ]
  [set cooperating 0]

end

to movement-cognitions
  if any? patches at-points vision-points with [not any? turtles-here]
  [
    if [psugar] of patch-here >= [psugar] of one-of patches at-points vision-points
  [
      coglogo:set-cogniton-value "want-sugar" 1
      coglogo:set-cogniton-value "want-move"  0
  ]

    if [psugar] of patch-here < [psugar] of one-of patches at-points vision-points with [not any? turtles-here]
  [
      coglogo:set-cogniton-value "want-sugar" 0.2 + (metabolism / 10)
      coglogo:set-cogniton-value "want-move" 0.6
  ]
  ]
end

to contribution-cognitions
    if epsilon  < (0.1) [ ;; pure selfish turtles
      coglogo:set-cogniton-value "want-contribute" 0
     coglogo:set-cogniton-value "want-sugar" 1
    ]

    if epsilon < (0.5) and epsilon > (0.1) [ ;; selfish turtles
      coglogo:set-cogniton-value "want-contribute" 0.5
    coglogo:set-cogniton-value "want-sugar" 0.5 + epsilon
      ]

    if epsilon > (0.5) and epsilon < (0.9) [ ;; altruistic turtles
      coglogo:set-cogniton-value "want-contribute" 1
     coglogo:set-cogniton-value "want-sugar" 1 - epsilon

      ]

    if epsilon > (0.9)  [ ;; pure altruistic turtles
      coglogo:set-cogniton-value "want-contribute" 1.5
    coglogo:set-cogniton-value "want-sugar" 0

]

end

to epsilon-observations
  if any? other turtles at-points vision-points[
    ifelse count other turtles at-points vision-points with [cooperating = 1] > count other turtles at-points vision-points with [cooperating = 0]
    [set epsilon epsilon + 0.1]
    [set epsilon epsilon - 0.1]
  ]
end

to lambda-observations
  if any? other turtles at-points vision-points[
    ifelse count other turtles at-points vision-points with [ wealth > [wealth] of self] > count other turtles at-points vision-points with [ wealth < [wealth] of self]
    [set lambda lambda + 0.1]
    [set lambda lambda - 0.1]
  ]
end

to epsilon-action
   if any? other turtles at-points vision-points[
    if count other turtles at-points vision-points with [cooperating = 1] > count other turtles at-points vision-points with [cooperating = 0]
    [if threshold-1 > norm-sensitivity [
      let internalized "cooperating"
      ifelse not table:has-key? cooperation-norm internalized
        [table:put cooperation-norm internalized 0.1]
        [let i table:get-or-default cooperation-norm internalized 0
        table:put cooperation-norm internalized i + 0.1 ]
  ]
    ]
  ]
end

to lambda-action
    if any? other turtles at-points vision-points[
    if count other turtles at-points vision-points with [cooperating = 1] > count other turtles at-points vision-points with [cooperating = 0]
    [if threshold-1 > norm-sensitivity [
      let internalized "lambda+"
      ifelse not table:has-key? what-amount-norm internalized
        [table:put what-amount-norm internalized 0.1]
        [let i table:get-or-default what-amount-norm internalized 0
        table:put what-amount-norm internalized i + 0.1 ]
  ]
    ]
  ]
end

to build-normative-belief
  let norm-list table:to-list cooperation-norm
  if not empty? norm-list [
    let selected key-with-max-value cooperation-norm
    let belief-first word selected " is "
    let full-belief word belief-first first deontics
    let addendum word " with " precision lambda 1
    let complete-belief word full-belief addendum
    set stored-lambda (list( precision lambda 1))
    if not member? full-belief normative-belief [
      set normative-belief lput complete-belief normative-belief

  ]
  ]
end

to select-normative-belief
  if threshold-2 > norm-threshold - norm-sensitivity [
    if not empty? normative-belief [
     let selected last normative-belief
     if member? "cooperating" selected   [
      set epsilon 1
      coglogo:activate-cogniton "normative-goal"
      coglogo:set-cogniton-value "normative-goal" 3
      if not empty? stored-lambda [
         set lambda last stored-lambda
        ]
        let groups [0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1]
        foreach groups [x -> if last stored-lambda = x [set group x]]
        ]
      ]
  ]


end


to turtle-talk
  if any? turtles at-points vision-points [
    let receiver one-of other turtles at-points vision-points
    let sender self
    if abs(epsilon - [epsilon] of receiver) <= theta-value and abs(lambda - [lambda] of receiver) <= theta-value [
      if not member? [who] of receiver interaction-memory [
        set interaction-memory lput [who] of receiver interaction-memory
      ]
      ask receiver [
        if not member? [who] of sender interaction-memory [
        set interaction-memory lput [who] of sender interaction-memory

      ]
        ]
    ]

    ]

end

to adjust-expectations
  if any? turtles at-points vision-points [
    if not member? [who] of other turtles at-points vision-points expectations [
      set expectations lput [who] of other turtles at-points vision-points expectations
    ]
    ]
end

to adjust-thresholds
  carefully [
    let my-memory interaction-memory
    let x count other turtles at-points vision-points with [member? who my-memory]
    let y count other turtles at-points vision-points
    ifelse x > 0 [
    set threshold-1 y / x
  ]
  [set threshold-1 0]
  ]
  [set threshold-1 0]

  carefully [
    let target who
    let a count other turtles at-points vision-points with [member? target last expectations]
    let b count other turtles at-points vision-points
  ifelse a > 0 [
    set threshold-2 b / a
  ]
  [set threshold-2 0]
  ]
  [set threshold-2 0]
end

to enforce-norm
  if any? other turtles at-points vision-points [
    if group != "none" [
      let x count turtles with [group = [group] of self]
      let conversion [group] of self
       if x > count other turtles at-points vision-points [
        if not empty? normative-belief [
          if any? other turtles at-points vision-points with [group != conversion] [
            let receiver one-of other turtles at-points vision-points with [group != conversion]
            let sender self
            let test 0
            ifelse abs([epsilon] of receiver - epsilon) <= theta-value
             [set test 1]
             [set test 0]
             ask receiver [
               if test = 1 [
                set incoming-command lput [last normative-belief] of sender incoming-command
                if not empty? [stored-lambda] of sender [
                  set stored-lambda lput last [stored-lambda] of sender stored-lambda
    ]
  ]
            ]

                ]
               ]
             ]
          ]
        ]
end



to act-on-choices
  ifelse not empty? incoming-command [
    if not empty? expectations [
    set choices (list ([max-psugar] of patch-here) ((length last expectations) * sanction-value ))
    if not empty? choices [
        ifelse last choices > first choices [
        let addendum coglogo:get-cogniton-value "want-contribute"
        coglogo:set-cogniton-value "want-contribute" addendum + 1.5
        set normative-belief lput last incoming-command normative-belief
      ]
        [coglogo:set-cogniton-value "want-contribute" 0]

    ]
  ]
  ]
  [coglogo:set-cogniton-value "want-contribute" 0]

end


to adjust-observations
  if epsilon > 1 [set epsilon 1]
  if epsilon < 0 [set epsilon 0]
  if lambda > 1 [set lambda 1]
  if lambda < 0 [set lambda 0]
end

to update-storage
  set PAYOFF-TICKS n-values 1000 [n -> n * redistribution-ticks]
  if resources-redistribution = true [
    if member? COUNTER PAYOFF-TICKS [
      set ACCUMULATED-STORAGE lput round(STORAGE) ACCUMULATED-STORAGE
      let increments n-values 1000 [n -> n * 1000]
      if length ACCUMULATED-STORAGE > 1 [
       let second-last last (but-last ACCUMULATED-STORAGE)
       foreach increments [ x -> ifelse STORAGE > x and STORAGE - second-last >= 1000 [
          ask patches [
          set max-psugar max-psugar + sugar-increment
          set incremented-psugar lput sugar-increment incremented-psugar
            foreach incremented-psugar [ inc -> let total-increment (list(last incremented-psugar))
            set storage storage - sum total-increment
            ]
          ]
          ]
          []
          if STORAGE < 0 [set STORAGE 0]
          ]
        ]
       ]
  ]
end

to test-contribution-day
  set contribution-days n-values 1000 [n -> n * contribution-ticks]
  ifelse member? counter contribution-days [
    set contribution-test true
  ]
  [set contribution-test false]
end

to patch-recolor ;; patch procedure
  ;; color patches based on the amount of sugar they have
  set pcolor (yellow + 4.9 - psugar)
end

to patch-growback ;; patch procedure
  ;; gradually grow back all of the sugar for the patch
  if growback-method = "Gradual" [
  set psugar min (list max-psugar (psugar + 1))
  ]
  if growback-method = "Immediate" [
    set psugar max-psugar
  ]

end

;;
;; Utilities
;;

to-report random-in-range [low high]
  report low + random (high - low + 1)
end

to-report random-float-in-range [low high]
  report low + random-float (high - low)
end

to-report key-with-max-value [table]
   let norm-list table:to-list table; convert to list of key/value pairs
   report first reduce [[a b] -> ifelse-value (last a > last b) [a][b]]; find pair with max value, report key
   norm-list
end
;;
;; Visualization Procedures
;;

to no-visualization ;; turtle procedure
  set color red
end

to color-agents-by-vision ;; turtle procedure
  set color red - (vision - 3.5)
end

to color-agents-by-metabolism ;; turtle procedure
  set color red + (metabolism - 2.5)
end

to color-agents-by-cooperation
  ifelse epsilon >= 0.5 [
  set color blue + (epsilon * 5 - 25)
  ]
  [set color red + (epsilon * 5 - 20)]
end

to color-agents-by-norms
  ifelse not empty? normative-belief [
    set color blue]
  [set color red]
end


; Copyright 2009 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
300
10
708
419
-1
-1
8.0
1
14
1
1
1
0
1
1
1
0
49
0
49
1
1
1
ticks
30.0

BUTTON
10
50
90
90
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

BUTTON
95
50
185
90
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
0

BUTTON
190
50
285
90
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

CHOOSER
10
95
290
140
visualization
visualization
"no-visualization" "color-agents-by-vision" "color-agents-by-metabolism" "color-agents-by-cooperation" "color-agents-by-norms"
4

PLOT
725
320
945
475
Population
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
"default" 1.0 0 -16777216 true "" "plotxy ticks count turtles"

PLOT
730
10
950
165
Wealth distribution
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
"default" 1.0 1 -16777216 true "" "set-histogram-num-bars 10\nset-plot-x-range 0 (max [wealth] of turtles + 1)\nset-plot-pen-interval (max [wealth] of turtles + 1) / 10\nhistogram [wealth] of turtles"

SLIDER
10
15
290
48
initial-population
initial-population
10
1000
100.0
10
1
NIL
HORIZONTAL

PLOT
940
165
1160
320
Average epsilon
NIL
NIL
0.0
0.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [epsilon] of turtles"

PLOT
1165
160
1385
320
Average lambda
NIL
NIL
0.0
0.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy ticks mean [lambda] of turtles"

SLIDER
5
260
170
293
reproduction-cost
reproduction-cost
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
5
295
175
328
wealth-for-reproduction
wealth-for-reproduction
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
5
330
177
363
age-of-reproduction
age-of-reproduction
25
80
51.0
1
1
NIL
HORIZONTAL

PLOT
730
165
930
315
Storage
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
"default" 1.0 0 -16777216 true "" "plot storage"

SLIDER
5
365
177
398
prob-of-inheritance
prob-of-inheritance
0
1
1.0
0.1
1
NIL
HORIZONTAL

SWITCH
300
420
460
453
resources-redistribution
resources-redistribution
0
1
-1000

SLIDER
5
220
177
253
Sugar-increment
Sugar-increment
0
0.5
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
5
400
177
433
redistribution-ticks
redistribution-ticks
0
200
100.0
25
1
NIL
HORIZONTAL

CHOOSER
10
140
145
185
Growback-method
Growback-method
"Gradual" "Immediate"
0

CHOOSER
150
140
290
185
social-behaviour
social-behaviour
"mindless-conformers" "deffuant-and-norms"
1

SLIDER
5
185
177
218
Theta-value
Theta-value
0
1
0.25
0.25
1
NIL
HORIZONTAL

PLOT
1160
10
1385
160
Lambda Distribution
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "set-histogram-num-bars 10\nset-plot-x-range 0 (max [lambda] of turtles + 0.1)\nset-plot-pen-interval (max [lambda] of turtles + 0.1) / 10\nhistogram [lambda] of turtles"

MONITOR
465
425
560
474
population
count turtles
17
1
12

PLOT
955
10
1155
160
Epsilon Distribution
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
"default" 1.0 1 -16777216 true "" "set-histogram-num-bars 10\nset-plot-x-range 0 (max [epsilon] of turtles + 0.1)\nset-plot-pen-interval (max [epsilon] of turtles + 0.1) / 10\nhistogram [epsilon] of turtles"

BUTTON
185
190
295
223
Cognitive-scheme
coglogo:openeditor
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
5
470
177
503
sanction-value
sanction-value
0
50
0.0
0.5
1
NIL
HORIZONTAL

SLIDER
5
435
177
468
contribution-ticks
contribution-ticks
0
50
8.0
1
1
NIL
HORIZONTAL

PLOT
945
320
1185
480
Group-distribution
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
"Groups" 1.0 1 -5298144 true "" "set-histogram-num-bars 10\nset-plot-x-range 0 (max [group] of turtles + 0.1)\nset-plot-pen-interval (max [group] of turtles + 0.1) / 10\nhistogram [group] of turtles"

SLIDER
5
505
177
538
Norm-threshold
Norm-threshold
0
10
2.0
1
1
NIL
HORIZONTAL

SWITCH
300
455
417
488
Aggression
Aggression
1
1
-1000

MONITOR
565
425
655
470
Body-count
body-count
17
1
11

@#$#@#$#@
## WHAT IS IT?

This second model in the NetLogo Sugarscape suite implements Epstein & Axtell's Sugarscape Constant Growback model, as described in chapter 2 of their book Growing Artificial Societies: Social Science from the Bottom Up. It simulates a population with limited, spatially-distributed resources available. It differs from Sugarscape 1 Immediate Growback in that the growback of sugar is gradual rather than instantaneous.

## HOW IT WORKS

Each patch contains some sugar, the maximum amount of which is predetermined. At each tick, each patch regains one unit of sugar, until it reaches the maximum amount. The amount of sugar a patch currently contains is indicated by its color; the darker the yellow, the more sugar.

At setup, agents are placed at random within the world. Each agent can only see a certain distance horizontally and vertically. At each tick, each agent will move to the nearest unoccupied location within their vision range with the most sugar, and collect all the sugar there.  If its current location has as much or more sugar than any unoccupied location it can see, it will stay put.

Agents also use (and thus lose) a certain amount of sugar each tick, based on their metabolism rates. If an agent runs out of sugar, it dies.

## HOW TO USE IT

Set the INITIAL-POPULATION slider before pressing SETUP. This determines the number of agents in the world.

Press SETUP to populate the world with agents and import the sugar map data. GO will run the simulation continuously, while GO ONCE will run one tick.

The VISUALIZATION chooser gives different visualization options and may be changed while the GO button is pressed. When NO-VISUALIZATION is selected all the agents will be red. When COLOR-AGENTS-BY-VISION is selected the agents with the longest vision will be darkest and, similarly, when COLOR-AGENTS-BY-METABOLISM is selected the agents with the lowest metabolism will be darkest.

The four plots show the world population over time, the distribution of sugar among the agents, the mean vision of all surviving agents over time, and the mean metabolism of all surviving agents over time.

## THINGS TO NOTICE

The world has a carrying capacity, which is lower than the initial population of the world. Agents who are born in sugarless places or who consume more sugar than the land cannot be supported by the world, and die. Other agents die from competition - although some places in the world have enough sugar to support them, the sugar supply is limited and other agents may reach and consume it first.

As the population stabilizes, the average vision increases while the average metabolism decreases. Agents with lower vision cannot find the better sugar patches, while agents with high metabolism cannot support themselves. The death of these agents causes the attribute averages to change.

## THINGS TO TRY

How dependent is the carrying capacity on the initial population size?  Is there a direct relationship?

## EXTENDING THE MODEL

How does changing the amount or rate of sugar growback affect the behavior of the model?

## NETLOGO FEATURES

All of the Sugarscape models create the world by using `file-read` to import data from an external file, `sugar-map.txt`. This file defines both the initial and the maximum sugar value for each patch in the world.

Since agents cannot see diagonally we cannot use `in-radius` to find the patches in the agents' vision.  Instead, we use `at-points`.

## RELATED MODELS

Other models in the NetLogo Sugarscape suite include:

* Sugarscape 1 Immediate Growback
* Sugarscape 3 Wealth Distribution

## CREDITS AND REFERENCES

Epstein, J. and Axtell, R. (1996). Growing Artificial Societies: Social Science from the Bottom Up.  Washington, D.C.: Brookings Institution Press.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Li, J. and Wilensky, U. (2009).  NetLogo Sugarscape 2 Constant Growback model.  http://ccl.northwestern.edu/netlogo/models/Sugarscape2ConstantGrowback.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2009 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2009 Cite: Li, J. -->
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment-norms" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count turtles</metric>
    <metric>round storage</metric>
    <metric>mean-epsilon</metric>
    <metric>mean-lambda</metric>
    <metric>mean-wealth</metric>
    <metric>count turtles with [group = 0]</metric>
    <metric>count turtles with [group = 0.1]</metric>
    <enumeratedValueSet variable="wealth-for-reproduction">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Growback-method">
      <value value="&quot;Gradual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction-value">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aggression">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-behaviour">
      <value value="&quot;deffuant-and-norms&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-of-reproduction">
      <value value="51"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-of-inheritance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sugar-increment">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="redistribution-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norm-threshold">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;color-agents-by-cooperation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contribution-ticks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resources-redistribution">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Theta-value">
      <value value="0.25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-no-norms" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count turtles</metric>
    <metric>round storage</metric>
    <metric>mean-epsilon</metric>
    <metric>mean-lambda</metric>
    <metric>mean-wealth</metric>
    <metric>count turtles with [group = 0]</metric>
    <metric>count turtles with [group = 0.1]</metric>
    <enumeratedValueSet variable="wealth-for-reproduction">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Growback-method">
      <value value="&quot;Gradual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction-value">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aggression">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-behaviour">
      <value value="&quot;mindless-conformers&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-of-reproduction">
      <value value="51"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-of-inheritance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sugar-increment">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="redistribution-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norm-threshold">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;color-agents-by-cooperation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contribution-ticks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resources-redistribution">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Theta-value">
      <value value="0.25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-no-norms-no-enforcement" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count turtles</metric>
    <metric>round storage</metric>
    <metric>mean-epsilon</metric>
    <metric>mean-lambda</metric>
    <metric>mean-wealth</metric>
    <metric>count turtles with [group = 0]</metric>
    <metric>count turtles with [group = 0.1]</metric>
    <enumeratedValueSet variable="wealth-for-reproduction">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Growback-method">
      <value value="&quot;Gradual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sanction-value">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aggression">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-behaviour">
      <value value="&quot;deffuant-and-norms&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-of-reproduction">
      <value value="51"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-of-inheritance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sugar-increment">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="redistribution-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norm-threshold">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;color-agents-by-norms&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contribution-ticks">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resources-redistribution">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Theta-value">
      <value value="0.25"/>
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
1
@#$#@#$#@
