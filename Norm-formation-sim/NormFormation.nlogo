extensions [CogLogo array table]

turtles-own [
  wealth          ;; the amount of sugar this turtle has
  metabolism      ;; the amount of sugar that each turtles loses each tick
  vision          ;; the distance that this turtle can see in the horizontal and vertical directions
  vision-points   ;; the points that this turtle can see in relative to it's current position (based on vision)
  norms          ;;  the list of norms internalized by each agents
  cooperation-rate ;; a variable ranging from 0 to 1 that express used to generate lambda and epsilon
  lambda  ;;  a  variable ranging from 0 to 1  that express the percentage of the amount of wealth owned by the turtle that is  given to the STORAGE,
  epsilon ;; a dynamic ranging from 0 to 1 that express the probabily of giving to the central STORAGE
  age ;; the age of the turtle
  threshold-1 ;; first threshold-value for norm-internalization
  threshold-2 ;; second threshold-value for norm selection and execution
  expectations ;; ID list of other agents in sight
  observed-norm-actions
  selected-norm
  group ;; the group of the turtle
  theta ;; opinion threshold
  mu ;; convergence value
  interaction-memory ;; a list in which the agents with whom the agent have successfully interacted are stored
  wealth-donated ;; the amount of wealth that the turtle has donated to the storage
  enforcing
  cooperating
  incoming-command ;; a list of the deontic commands that the turtle has received
  norm-sensitivity ;; the propension of the agent to internalize/follow a norm
  normative-belief ;; the current belief of the turtle
]



patches-own [
  psugar           ;; the amount of sugar on this patch
  max-psugar       ;; the maximum amount of sugar that can be on this patch
  incremented-psugar ;; track the sugar-increments of the patch
]

globals [
  STORAGE  ;; amount in the central STORAGE
  ACCUMULATED-STORAGE
  COUNTER
  PAYOFF-TICKS
  INCREMENT-VALUES
  DEONTICS
  MEAN-EPSILON
  MEAN-LAMBDA


;;
;; Setup Procedures
;;
]
to setup
  clear-all
  create-turtles initial-population [ coglogo:init-cognitons  turtle-setup]
  setup-patches
  set COUNTER 0
  set PAYOFF-TICKS []
  set INCREMENT-VALUES []
  set ACCUMULATED-STORAGE []
  set DEONTICS ["mandatory"]
  set MEAN-EPSILON []
  set MEAN-LAMBDA []
  reset-ticks
end

to turtle-setup ;; turtle procedure

  set color red
  set shape "circle"
  move-to one-of patches with [not any? other turtles-here]
  set wealth random-in-range 5 25
  set metabolism random-in-range 1 4
  set vision random-in-range 1 6
  set age random-in-range  0 10
  ;; turtles can look horizontally and vertically up to vision patches
  ;; but cannot look diagonally at all
  set vision-points []
  foreach (range 1 (vision + 1)) [ n ->
    set vision-points sentence vision-points (list (list 0 n) (list n 0) (list 0 (- n)) (list (- n) 0))
  ]
  set norms table:make
  set cooperation-rate random-float 1
  ifelse cooperation-rate > 0.5 [
    ifelse random-float 1 < 0.7 [
      set epsilon random-float-range 0.5 1
      set lambda random-float-range 0.5 1
  ]
    [set epsilon random-float-range 0 0.5
     set lambda random-float-range 0 0.5
    ]
  ]
  [set epsilon random-float-range 0 0.5
     set lambda random-float-range 0 0.5
  ]
  set norm-sensitivity random-float 1
  set wealth-donated 0
  set selected-norm []
  set expectations []
  set observed-norm-actions []
  set group 0
  set enforcing 0
  set cooperating 0
  set theta theta-value
  set mu mu-value
  set interaction-memory []
  set normative-belief []
  set incoming-command []
  run visualization
end



to setup-patches
  file-open "sugar-map.txt"
  foreach sort patches [ p ->
    ask p [
      set max-psugar file-read
      set psugar max-psugar
      patch-recolor
      set incremented-psugar []
    ]
  ]
  file-close
end

;;
;; Runtime Procedures
;;

to go
  if not any? turtles [
    stop
  ]
  set COUNTER COUNTER + 1
  ;; The common storage is updated, and if the right conditions are met, some sugar is redistribuited on the patches
  update-storage
  ;; The patches regrow their sugar, and get recolored
  ask patches [
    patch-growback
    patch-recolor
  ]
  ;; The patches lose sugar every n ticks (can be deactivated in the interface, the amount lost is controlled by the "deplation-rate" switch)
  if depletion = true [
    let depletion-time n-values 1000 [n -> n * 100]
    if member? COUNTER depletion-time [
      ask patches [
        set max-psugar max-psugar - deplation-rate
        if max-psugar < 0 [set max-psugar 0]
      ]
    ]
  ]

  ask turtles [
    set age age + 1
    ;; run the visualization procedure
    run visualization
    ;; turtles eat the sugar that is on the patches on which they are located
    turtle-eat
    ;; turtles move to the nearest free patch with more sugar then the current one
    turtle-move
    ;; turtles act following the congiton mechanism ( modified by pressing the "coglogo-editor" in the interface")
    act-on-cognitons
    ;; turtles adjust their triggers for internalizing and following a norm
    adjust-triggers
    ;; turtles build their expectation list, looking for other agents in-sight
    adjust-expectations
    ;; turtles adjust their lambda value
    lambda-observations
    ;; turtle internalize the most salient action stored in observed-norm-action
    build-norm
    ;; turtle adjust their epsilon value
    epsilon-observations
    if group-behavior = true
    [
    ;; turtles modify their behavior according to deffuant model of opinion dynamic
    turtle-talk
    ;; turtle modify their behavior according to their group belonging
    group-cognitions
    ;; turtle send a deontic-command to other turtles in sight that pass that have similar epsilon-value
    ;; the message is reinforced if other turltes in sight belong to the same group of the sender-turtle
    enforce-norm
    ]
   adjust-behavior
    run coglogo:choose-next-plan
    coglogo:report-agent-data
    turtle-reproduce
    if wealth <= 0 [die]
    if age >= 100 [die]
   ]
    carefully [
    set MEAN-EPSILON lput mean [epsilon] of turtles MEAN-EPSILON
    set MEAN-LAMBDA lput mean [lambda] of turtles MEAN-LAMBDA
  ]
    []

    tick
end

;;;
;;; TURTLE-COGNITIONS
;;;

to act-on-cognitons
   movement-cognitions
   contribution-cognitions
   group-cognitions
end



to movement-cognitions
  if any? patches at-points vision-points with [not any? turtles-here]
  [
    if [psugar] of patch-here >= [psugar] of one-of patches at-points vision-points
  [
      coglogo:set-cogniton-value "wantsugar" 1
      coglogo:set-cogniton-value "wantmove"  0
  ]

    if [psugar] of patch-here < [psugar] of one-of patches at-points vision-points with [not any? turtles-here]
  [
      coglogo:set-cogniton-value "wantsugar" 0.2 + (metabolism / 10)
      coglogo:set-cogniton-value "wantmove" 0.6
  ]
  ]
end

to contribution-cognitions
    if epsilon  < (0.1) [ ;; pure selfish turtles
      coglogo:set-cogniton-value "wantcontribute" 0
     coglogo:set-cogniton-value "wantsugar" 1
    ]

    if epsilon < (0.5) and epsilon > (0.1) [ ;; selfish turtles
      coglogo:set-cogniton-value "wantcontribute" 0.5
    coglogo:set-cogniton-value "wantsugar" 0.5 + epsilon
      ]

    if epsilon > (0.5) and epsilon < (0.9) [ ;; altruistic turtles
      coglogo:set-cogniton-value "wantcontribute" 1
     coglogo:set-cogniton-value "wantsugar" 1 - epsilon

      ]

    if epsilon > (0.9)  [ ;; pure altruistic turtles
      coglogo:set-cogniton-value "wantcontribute" 1.5
    coglogo:set-cogniton-value "wantsugar" 0

]

end

to group-cognitions
  if wealth > 50 and group != 0 [
    ifelse enforcing = 0 [
    coglogo:set-cogniton-value "enforce-norm" 2
    set enforcing 1
  ]
    [coglogo:set-cogniton-value "enforce-norm" 0]
  ]
  if enforcing = 1 [
    coglogo:set-cogniton-value "enforce-norm" 0
  ]
end

;;;
;;; TURTLE PROCEDURES
;;;

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

to turtle-talk ;; Deffuant model of opinion-dynamics and interaction memory mechanism
  if any? turtles at-points vision-points [
    let receiver one-of other turtles at-points vision-points
    let sender self
    if abs(epsilon - [epsilon] of receiver) <= theta [
      if not member? [who] of receiver interaction-memory [
        set interaction-memory lput [who] of receiver interaction-memory
      ]
      set epsilon epsilon + mu * ([epsilon] of receiver - epsilon)
      if epsilon > 1 [set epsilon 1]
      ask receiver [
        if not member? [who] of sender interaction-memory [
        set interaction-memory lput [who] of sender interaction-memory
      ]
      set epsilon epsilon + mu * ([epsilon] of sender - epsilon)
      if epsilon > 1 [set epsilon 1]
        ]
    ]
  ]
end

to lambda-observations ;; build norm-actions from observing the enviroment
  if any? other turtles at-points vision-points[
    ifelse count other turtles at-points vision-points with [wealth-donated != 0] > count other turtles at-points vision-points with [wealth-donated = 0] [

        set observed-norm-actions lput "lambda+" observed-norm-actions
      ]
       [ if selfish-norms? = true [
        set observed-norm-actions lput "lambda-" observed-norm-actions
      ]

    ]
    ]


end

to epsilon-observations ;; build norm-actions from observing enviroment
  if any? other turtles at-points vision-points[
    ifelse count other turtles at-points vision-points with [cooperating = 1] > count other turtles at-points vision-points with [cooperating = 0]
      [
        set observed-norm-actions lput "epsilon+" observed-norm-actions
    ]
    [if selfish-norms? = true  [
        set observed-norm-actions lput "epsilon-" observed-norm-actions
      ]

    ]
  ]

end


to turtle-contribute
  if wealth > 50 [
  ifelse random-float 1 < epsilon [
    let amount-given wealth * (lambda)
    set wealth wealth - amount-given
    set wealth-donated wealth-donated + amount-given
    set STORAGE STORAGE + amount-given
    set cooperating 1
    ]
    [set cooperating 0]
  ]
end


to turtle-reproduce
  if initial-population * population-cap > count turtles [
  if wealth > 100 and age > 45 [
    set wealth wealth - 50
    let target-1 [lambda] of self
    let target-2 [epsilon] of self
    hatch 1 [
    if random-float 1 < 0.5 [
        set lambda target-1
        set epsilon target-2
      ]
    set age random-in-range 0 10
    setxy random-xcor random-ycor]
  ]
  ]
end

;;;
;;; norm-formation-procedures
;;;


to adjust-triggers
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

to build-norm   ;; if a frist threshold test is succesfull, norm-action becomes internalized. If norm is present, add the weight of the norm
  if threshold-1 > norm-threshold - norm-sensitivity [
    if not empty? observed-norm-actions [
      let internalized last observed-norm-actions
      ifelse not table:has-key? norms internalized
        [table:put norms internalized 0.1]
        [let i table:get-or-default norms internalized 0
        table:put norms internalized i + 0.1 ]
        build-normative-belief
  ]
  ]

  if norms? = true [
   if threshold-2 > norm-threshold - norm-sensitivity [
   select-norm
   change-norm

  ]
  ]
end

to select-norm ;; turtle observe the norm that has internalized, and select the norm with the strongest weight. Higher cooperation rate will favor a selection for an altruist norm.

  if not empty? normative-belief [
  let x epsilon
  let p 1 - epsilon
  if random-float 1 < x [
  let selected last normative-belief

     ;; altruistic norms
  if selected = "epsilon+ is mandatory" [
      set selected-norm lput "epsilon+" selected-norm
      set group 1
      set epsilon epsilon + 0.1
      coglogo:activate-cogniton "normative-goal"
      coglogo:set-cogniton-value "normative-goal" 3

  ]
  if selected = "lambda+ is mandatory" [
    set selected-norm lput "lambda+" selected-norm
    set group 2
    set lambda lambda + 0.1
    coglogo:activate-cogniton "normative-goal"
    coglogo:set-cogniton-value "normative-goal" 3
    ]
    ]

if selfish-norms? = true [

if random-float 1 < p [
  let selected key-with-max-value (norms)
  if selected = "epsilon- is mandatory" [
      set selected-norm lput "epsilon-" selected-norm
      set group -1
      set epsilon epsilon - 0.1
        ]

  if selected = "lambda- is mandatory" [
    set selected-norm lput "lambda-" selected-norm
    set group -2
    set lambda lambda - 0.1
     ]
    ]
   ]
  ]
end

to change-norm
  if not empty? normative-belief [
    if not empty? incoming-command [
      if last normative-belief != last incoming-command [
        set normative-belief lput last incoming-command normative-belief
      ]
    ]
  ]
end

to build-normative-belief
  let norm-list table:to-list norms
  if not empty? norm-list [
    let selected key-with-max-value norms
    let belief-first word selected " is "
    let full-belief word belief-first one-of DEONTICS
    if not member? full-belief normative-belief [
      set normative-belief lput full-belief normative-belief
    ]
  ]
end

to enforce-norm
  if any? other turtles at-points vision-points [
    if group != 0 [
      let x count turtles with [group = [group] of self]
      let conversion [group] of self
       if x > count other turtles at-points vision-points [
        if not empty? normative-belief [
          if any? other turtles at-points vision-points with [group != conversion] [
            let receiver one-of other turtles at-points vision-points with [group != conversion]
            let sender self
            let test 0
            set wealth wealth - 10
            ifelse abs([epsilon] of receiver - epsilon) <= theta
             [set test 1]
             [set test 0]
             ask receiver [
               if test = 1 [
                set incoming-command lput [last normative-belief] of sender incoming-command
                ]
               ]
             ]
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

  if not empty? incoming-command [
      if any? other turtles at-points vision-points with [not empty? normative-belief] [
      if not member? [who] of other turtles at-points vision-points expectations [
       carefully [
      set expectations lput [who] of other turtles at-points vision-points with [last normative-belief = [last incoming-command] of self] expectations
    ]
    []
    ]
      ]
    ]
  ]
end

to adjust-behavior
  if epsilon > 1 [set epsilon 1]
  if epsilon < 0 [set epsilon 0]
  if lambda > 1 [set lambda 1]
  if lambda < 0 [set lambda 0]
end




;;;
;;; GENERAL PROCEDURES
;;;

to update-storage
  set PAYOFF-TICKS n-values 100 [n -> n * 50]
  if resources-redistribution = true [
    if member? COUNTER PAYOFF-TICKS [
      set ACCUMULATED-STORAGE lput round(STORAGE) ACCUMULATED-STORAGE
      let increments n-values 1000 [n -> n * 1000]
      if length ACCUMULATED-STORAGE > 1 [
       let second-last last (but-last ACCUMULATED-STORAGE)
       foreach increments [ x -> if STORAGE > x and STORAGE - second-last >= 1000 [ ;; if, from the last payoff tick, the STORAGE has grown of more than 1000 units, redistribute resources
        ask patches [
          set max-psugar max-psugar + sugar-increment
          set incremented-psugar lput sugar-increment incremented-psugar
          let total-increment sum incremented-psugar
          set STORAGE STORAGE - total-increment
          if STORAGE < 0 [set STORAGE 0]
          ]
        ]
       ]
      ]
     ]
    ]
end

to patch-recolor ;; patch procedure
  ;; color patches based on the amount of sugar they have
  set pcolor (yellow + 4.9 - psugar)
end

to patch-growback ;; patch procedure
  ;; immediately grow back all of the sugar for the patch
  set psugar max-psugar
end

;;
;; Utilities
;;

to-report random-in-range [low high]
  report low + random (high - low + 1)
end

to-report random-float-range [low high]
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
  ifelse not empty? selected-norm
  [set color blue]
  [set color red]
end
@#$#@#$#@
GRAPHICS-WINDOW
290
10
698
419
-1
-1
8.0
1
10
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
5
50
85
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
55
185
95
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
200
55
290
95
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
105
290
150
Visualization
Visualization
"no-visualization" "color-agents-by-vision" "color-agents-by-metabolism" "color-agents-by-cooperation" "color-agents-by-norms"
3

PLOT
720
10
940
165
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
950
10
1170
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
Initial-population
Initial-population
10
500
150.0
10
1
NIL
HORIZONTAL

MONITOR
875
330
970
375
population
count turtles
17
1
11

PLOT
725
175
1170
325
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

MONITOR
725
330
807
375
Pure altruists
count turtles with [epsilon >= 0.9]
17
1
11

MONITOR
810
330
867
375
Altruists
count turtles with [ epsilon >= 0.5 and epsilon < 0.9]
17
1
11

MONITOR
725
380
802
425
Pure selfish
count turtles with [ epsilon <= 0.1 ]
17
1
11

MONITOR
805
380
870
425
Selfish
count turtles with [ epsilon > 0.1 and epsilon < 0.5]
17
1
11

BUTTON
0
460
152
493
edit-cognitive-scheme
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

MONITOR
875
380
972
425
storage
round(storage)
17
1
11

PLOT
290
420
490
570
Altruist-groups
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
"epsilon+" 1.0 0 -14070903 true "" "carefully [plot count turtles with [group = 1]][]"
"lambda+" 1.0 0 -2674135 true "" "carefully [plot count turtles with [group = 2]][]"

PLOT
490
420
690
570
Selfish-groups
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
"epsilon-" 1.0 0 -14985354 true "" "carefully [plot count turtles with [group = -1]][]"
"lambda-" 1.0 0 -2674135 true "" "carefully [plot count turtles with [group = -2]][]"

SWITCH
0
150
170
183
Resources-redistribution
Resources-redistribution
0
1
-1000

SWITCH
0
185
103
218
Norms?
Norms?
0
1
-1000

SWITCH
105
185
237
218
Selfish-norms?
Selfish-norms?
1
1
-1000

SWITCH
0
220
142
253
Group-behavior
Group-behavior
0
1
-1000

SLIDER
0
355
182
388
Deplation-rate
Deplation-rate
0
1
1.0
0.01
1
NIL
HORIZONTAL

SWITCH
170
150
270
183
Depletion
Depletion
0
1
-1000

SLIDER
0
390
172
423
Mu-value
Mu-value
0
0.5
0.25
0.25
1
NIL
HORIZONTAL

SLIDER
0
425
172
458
Theta-value
Theta-value
0
0.5
0.25
0.25
1
NIL
HORIZONTAL

SLIDER
0
250
172
283
Population-cap
Population-cap
0
5
3.0
1
1
NIL
HORIZONTAL

SLIDER
0
285
172
318
Norm-threshold
Norm-threshold
0
5
2.1
0.1
1
NIL
HORIZONTAL

SLIDER
0
320
172
353
Sugar-increment
Sugar-increment
0
1
0.5
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model build a simulation of a public-good game in which one can study the emergence of norms of cooperation between selfish agents, and the formation of groups related to those norms. The model is design on a sugarscape enviroment (Epstein Axtell 1996) in which agents move into the space extracting sugar that is present, in different amounts, on the patches. 

## HOW IT WORKS

Each patch contains some sugar, the maximum amount of which is predetermined. At each tick, each patch grows back fully to have the maximum amount of sugar. The amount of sugar a patch currently contains is indicated by its color; 

At setup, agents are placed at random within the world. Each agent can only see a certain distance horizontally and vertically. At each tick, each agent will move to the nearest unoccupied location within their vision range with the most sugar, and collect all the sugar there.  If its current location has as much or more sugar than any unoccupied location it can see, it will stay put.

Agents also use (and thus lose) a certain amount of sugar each tick, based on their metabolism rates. If an agent runs out of sugar, it dies.

## HOW TO USE IT

Press the "EDIT-COGNITIVE-SCHEME" before pressing SETUP. This modify the cognitive structure of the agents, making certain behavior more or less likely, and one can add or subtract the weight of certain desires of the agents, that are connected to a set of actions

Set the INITIAL-POPULATION slider before pressing SETUP. This determines the number of agents in the world.

Set the POPULATION-CAP to limit the number of agents (the cap is the value of the slider multiplied by the initial-population value)

Set the NORM-THRESHOLD slider to make less likely that agents internalize a norm. With higher values, more norm-following agents are required in sight of a given agent for making a norm internalized

Set the SUGAR-INCREMENT slider to modify the amount of sugar that is redistribuited to each pach after 50 ticks

Set the DEPLETION-RATE slider to modify the amount of sugar that patches lose every 50 ticks

Set the MU-VALUE SLIDER to modify the interval of the "epsilon" value in which two agents can modify their epsilon value

Set the THETA-VALUE slider to modify the interval of the "epsilon" value in which two agents can communicate effectivly between each-other

Press SETUP to populate the world with agents and import the sugar map data. GO will run the simulation continuously, while GO ONCE will run one tick.

The VISUALIZATION chooser gives different visualization options and may be changed while the GO button is pressed. 
When NO-VISUALIZATION is selected all the agents will be red. 
When COLOR-AGENTS-BY-VISION is selected the agents with the longest vision will be darkest.
COLOR-AGENTS-BY-METABOLISM is selected the agents with the lowest metabolism will be darkest. 
When COLOR-AGENTS-BY-COOPERATION is selected the agents with an epsilon value >= 0.5 will be colored in blue, and the agent with an epsilon value < 0.5 will be colored in red.
When COLOR-AGENTS-BY-NORMS is selected. the agents that are currently following a norm will be displayed in blue, those who are not, in red.

The three plots show the world population over time, the distribution of sugar among the agents, the amount of sugar stored in the storage.
The monitors show the type of agents that are currently present in the world, ranked by their altruism level.

## THINGS TO NOTICE

After 20 ticks or so, many agents are no longer moving or are only moving a little. This is because the agents have reached places in the world where they can no longer see better unoccupied locations near them. Since all sugar grows back instantaneously each tick, agents tend to remain on the same patch.

Agents tend to congregate in "layers" around borders where sugar production levels change. This unintended behavior comes from the limitation of the agents' vision ranges. Agents that cannot see past the current sugar production grounds have no incentive to move, and so each agent only moves to the closest location with more sugar. This effect is more less apparent depending on the initial population.

## THINGS TO TRY

Try varying the initial POPULATION. What effect does the initial POPULATION have on the final stable population? Does it have an effect on the distribution of agent properties, such as vision and metabolism?

## NETLOGO FEATURES

All of the Sugarscape models create the world by using `file-read` to import data from an external file, `sugar-map.txt`. This file defines both the initial and the maximum sugar value for each patch in the world.

Since agents cannot see diagonally we cannot use `in-radius` to find the patches in the agents' vision.  Instead, we use `at-points`.

## RELATED MODELS

Other models in the NetLogo Sugarscape suite include:

* Sugarscape 2 Constant Growback
* Sugarscape 3 Wealth Distribution

## CREDITS AND REFERENCES

Epstein, J. and Axtell, R. (1996). Growing Artificial Societies: Social Science from the Bottom Up.  Washington, D.C.: Brookings Institution Press.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Li, J. and Wilensky, U. (2009).  NetLogo Sugarscape 1 Immediate Growback model.  http://ccl.northwestern.edu/netlogo/models/Sugarscape1ImmediateGrowback.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

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
  <experiment name="experiment-no-storage" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count turtles</metric>
    <metric>round storage</metric>
    <metric>precision mean mean-epsilon 3</metric>
    <metric>precision mean mean-lambda 3</metric>
    <metric>count turtles with [group = one-of [1 2]]</metric>
    <enumeratedValueSet variable="Sugar-increment">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Resources-redistribution">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-population">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-cap">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Group-behavior">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Selfish-norms?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norm-threshold">
      <value value="2.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Visualization">
      <value value="&quot;color-agents-by-norms&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mu-value">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norms?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Depletion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Deplation-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Theta-value">
      <value value="0.25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-no-norms" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count turtles</metric>
    <metric>round storage</metric>
    <metric>precision mean mean-epsilon 3</metric>
    <metric>precision mean mean-lambda 3</metric>
    <metric>count turtles with [group = one-of [1 2]]</metric>
    <enumeratedValueSet variable="Sugar-increment">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Resources-redistribution">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-population">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-cap">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Group-behavior">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Selfish-norms?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norm-threshold">
      <value value="2.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Visualization">
      <value value="&quot;color-agents-by-norms&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Depletion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norms?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mu-value">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Deplation-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Theta-value">
      <value value="0.25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-norms" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count turtles</metric>
    <metric>round storage</metric>
    <metric>mean mean-epsilon</metric>
    <metric>mean mean-lambda</metric>
    <metric>count turtles with [group = one-of [1 2]]</metric>
    <enumeratedValueSet variable="Sugar-increment">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Resources-redistribution">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-population">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-cap">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Group-behavior">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Selfish-norms?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norm-threshold">
      <value value="2.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Visualization">
      <value value="&quot;color-agents-by-cooperation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mu-value">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Norms?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Depletion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Deplation-rate">
      <value value="1"/>
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
