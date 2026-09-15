extensions [csv time table gis nw profiler cbr]
__includes ["lib/stravvs_readinput.nls" "lib/stravvs_procedures.nls" "lib/stravvs_builder.nls"
  "lib/case-based-reasoning.nls" "lib/socialsim_worldbuilder.nls" "lib/socialsim2_worldbuilder.nls"
  "lib/kilda_worldbuilder.nls" "lib/scotland_worldbuilder.nls" "lib/tiny_worldbuilder.nls" "lib/hierarchy.nls"
  "lib/unit_testing.nls" "lib/stravvs_ontology.nls"]

globals [
  products-unique
  product-colour; table of products & their colours for consistent plotting
  product-values; table of products & attributes (price & shelf-life)
  product-types ; table of products & their ingredients
  product-catalysts ; table of products & their catalysts
  equation-yields; table of equation output conditional distributions
  equation-colour; table of equations & their colours for consistent plotting
                 ;
  suppliers ; table of product-type to agentset
  warnings ; table of warning messages so we don't repeat them
  notes ; table of note messages so we don't repeat them
  error-state? ; boolean -- are we in an error state
  global-state ; table of state variables at global level
  date
  region-map ; GIS map defining region boundaries
             ;
  things-ordered    ; table of products ordered
  things-made       ; table of products made
  things-delivered  ; table of products delivered
  things-consumed   ; table of products consumed
  things-imported   ; table of products imported
  things-run        ; table of equations being used
  things-amount     ; table of sum(amount) of products
                    ;
                    ; CBR
  global-case-base  ; ** until CBR working, use a normal table
  consumer-timeseries ;if using a consumer time series, records when consumer orders
  consumer-header ;headers of which products the consumer will want

  network; list of pairwise costs [from to cost]
         ;;
  index-case ; processor to spit out progress
             ;;
  processors-died ; count how many processors have died by changing too often
  output-file ; file to store results


  equation-header
  product-header
  output-used; table of processing status
  output-made; table of processing status
  output-equation; table of processing status
  output-imported; table of processing status

  n-strategical-changes ;record how many times any processor has changed equation (habit-change)
  n-tactical-changes ;record how many times any processor has changed equation (tactic)

  error?
  n-fails
  n-tests

  operators
  units
  infinite ;infinity
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        1         2         3         4         5         6          7         8
;23456789012345678901234567890123456789012345678901234567890123445678901234567890
;
; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to choose-seed
  if new-seed? [
    set run-seed new-seed
  ]
  random-seed run-seed
end

to show-warranty
  print "STRAVVS  Copyright (C) 2022  The James Hutton Institute"
  print "This program comes with ABSOLUTELY NO WARRANTY."
  print "This is free software, and you are welcome to redistribute it"
  print "under certain conditions. For more information on this and"
  print "the (lack of) warranty, see the LICENCE section in the Info tab."
end


; {observer} setup
;
; Setup routine that loads data in from files

to setup
  reset-timer
  ca
  set error-state? false
  choose-seed

  set operators (sentence "+ = - > <")
  set units (sentence "% |")
  set infinite 1e+10

  if testing? [
    set n-fails 0
    set n-tests 0
  ]

  if output? [
    ; pre-allocate output file with random number to make distinct
    set output-file (word output-dir "/output_" random-float 1000000 ".csv")
  ]

  ; read from input file
  if input-file? [
    read-inits
  ]

  ; construct world
  build-world

  ; make a look-up table of average distances between actors
  ;map-distances

  ; start clock
  if empty? start-date [
    set date time:create "2023/01/01"
  ]

  ; tables to store what is ordered, made and delivered
  set things-ordered table:make
  set things-made table:make
  set things-delivered table:make
  set things-consumed table:make
  set things-imported table:make
  set things-run table:make
  set things-amount table:make
  set processors-died 0

  ; prepare space for outputs
  set output-used table:make
  set output-made table:make
  set output-equation table:make
  set output-imported table:make

  ; prepare headers for outputs
  set equation-header []
  ask processes [
    set equation-header lput [name] of self equation-header
  ]
  set equation-header remove-duplicates sort equation-header

  ; prepare headers for outputs
  set product-header []
  ask products with [shelflife = "archetype"][
    set product-header lput [product-type] of self product-header
  ]
  set product-header remove-duplicates sort product-header

  ; pick & store consistent product colours for all plots
  product-clr
  equation-clr

  ;--- Processor building ----------------
  ; switch to determine if processors are given random resources at initialisation
  ; otherwise CBR will likely default to an uncatalysed equation every time
  if seed-resources? [
    ask processors [
      ; random selection of products to seed
      foreach n-of (random table:length product-values) (table:keys product-values) [ x ->
        hatch-products 1 [
          set size patch-size / 80
          set product-type x
          set amount ([amount] of one-of products with [product-type = x] );* get-capacity x)
          set shelflife random-float item 1 table:get product-values x
          set color table:get product-colour x
          ;fd 2
          create-stock-from myself
        ]
      ]
    ]
  ]

  ;price all processes (inputs+catalysts)
  equation-cost

  ; allocate equations to processors
  ask processors [
    ; just make sure that the budget is < change and therefore trigger the
    ; habit change
    set long-budget abs (2 * max-change)

;print (sentence "SETUP A) " self " Implements: " [name] of implement-neighbors)
    ; set first memory
    update-memory false
;print (sentence "SETUP B) " self " Implements: " [name] of implement-neighbors)

    ;print (word self " BEFORE : " whowhat)
    if count my-implements = 0 [
      change-habit "setup" strategical-choice "everything"
    ]
    ;print (word self " AFTER : " whowhat)
;print (sentence "SETUP C) " self " Implements: " [name] of implement-neighbors)

    set changed 0
    set discontent 0

    ; change resources to randomly sample shelf-life, therefore
    ; need replacing at some future point
    ask out-stock-neighbors [
      let good [product-type] of self
      set shelflife random item 1 table:get product-values good
    ]

    ; populate local processor case-base
    ;set my-memory table:make
    update-memory false
  ]



  set suppliers table:make ; this table is built as suppliers-of is used

  set global-state table:make

  ;  ask patches [
  ;    set patch-state table:make
  ;  ]

  ; zero timer
  reset-ticks

  ; pick one processor to track
  set index-case one-of processors

  if demand-random? [
    ask consumers [
      table:clear demand
      table:put demand [product-type] of one-of products with [shelflife = "archetype"] random 100
    ]
  ]

  ; zero change counter
  set n-strategical-changes 0
  set n-tactical-changes 0

  if video? [
    export-view "stravvs_view_0.png"
    export-interface "stravvs_interface_0.png"
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        1         2         3         4         5         6          7         8
;23456789012345678901234567890123456789012345678901234567890123445678901234567890
;
; Go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; {observer} go
;
; Schedule of actions that make the consumers 'suck' goods through
; the value network.

to go
;print ticks
  ; When running in headless mode, if an error has occurred we don't
  ; want to keep running

  if not error-state? [

;print ticks

    ; rezero order tables
    table:clear things-ordered    ; table of products ordered
    table:clear things-made       ; table of products made
    table:clear things-delivered  ; table of products delivered
    table:clear things-consumed   ; table of products consumed
    table:clear things-imported   ; table of products imported
    table:clear things-run

    if verbose? [print (word "\n\n")]
    ; consumers make orders
    ; update desired output for this tick if using a time series
    ; for consumption (otherwise defalults to hard-coded
    ; values in the world builder
    if consumer-series? = TRUE [
      ; catch error if trying to extend beyond end of consumer time series
      if ticks >= length consumer-timeseries [
        stop
      ]
      ask consumers [ consumer-want ticks ]
    ]

    if verbose? [print (word "--------- " ticks " ---------")  ]

;    print ticks
;    ask processors [
;      ;if verbose? [
;        print (word "BUDGET " self ": " budget);]
;    ]

    ask consumers with [table:length demand > 0] [
      if verbose? [print "Consumer orders -----------------------------"]
      make-orders
    ]

    ; processors make orders
    ask processors with [closed = FALSE AND count my-contracts > 0][
      if verbose? [print (word "Processor orders -" self "---------------------------")]
      make-orders
;print (sentence self " " capacity)
    ]

    ask importers [
      if verbose? [print "Import goods -----------------------------"     ]
      fulfil-imports
    ]

    ; transporters make deliveries
    ask transporters [
      if verbose? [print "Deliver A (imports) -----------------------------"     ]
      make-instant-deliveries
    ]

;    ask processors [
;      ;if verbose? [
;        print (word "BUDGET " self ": " budget);]
;    ]
;    print "\n"

    ;ask contracts [ print(word self " " end1 " --> " end2 ": " quantity-needed " " resource-name " " time-remaining)]

    ; processors make goods
    ask processors with [closed = FALSE AND count my-out-contracts > 0] [
      if verbose? [print (word "Make goods -" self "----------------------------")]

      ;print (word self " has stock: " stock-summary)

      ; update the current capacity for each linked process
      set capacity table:make
      foreach [name] of out-implement-neighbors [ x -> table:put capacity x get-capacity-process x ([stock-summary] of self)]

      ;update the count-down timer until new orders are made
      ;update-orders

      ; make the products
      make-goods
    ]
    ;print (word "PRODUCT A " sum [amount] of products with [product-type = "A" AND shelflife != "archetype"])
    ;print (word "PRODUCT B " sum [amount] of products with [product-type = "B" AND shelflife != "archetype"])

    ; processors fulfil orders
    ask processors with [closed = FALSE AND count my-out-contracts > 0] [
      if verbose? [print (word "Fulfill orders -" self "----------------------------")]
      fulfil-orders
      ; consolidate remaining stock
      consolidate-stock ; merge similar stock & delete excess agents
      if verbose? [print (sentence "GO check " self " stock: " stock-summary)]
    ]

;    ask processors [
;      ;if verbose? [
;        print (word "BUDGET " self ": " budget);]
;    ]
;    print "\n"


    ; transporters make deliveries
    ask transporters [
      if verbose? [print "Deliver B -----------------------------"         ]
      make-instant-deliveries
    ]

    ; consumers consume
    ask consumers [
      if verbose? [print "Consumption -----------------------------" ]
      consume

      ; consolidate remaining stock
      consolidate-stock ; merge similar stock & delete excess agents
    ]

    ;    ask processors [
    ;      consolidate-stock ; merge similar stock & delete excess agents
    ;    ]
  ]

  ;; total amount of each product
  foreach table:keys product-types [ x ->
    table:put things-amount x sum [amount] of products with [product-type = x AND shelflife != "archetype"]
  ]

  tick

  ; tidy up processors
  ask processors with [closed = FALSE] [
    ;; accumulate long-term budget
    set long-budget long-budget + budget

    ;----- tactics -----;
    ;; annual review of tactics
    if (ticks mod (convert-time 1 "year")) = 0 AND review-tactics? [

      ;print (sentence "GO - end of year review, count of implements: " count my-implements)
      check-tactics

      ;; update memory for local case-base of what was just completed
      ; only do this periodically to prevent the casebase unnecessarily
      ; duplicating every step, e.g. when there is/could be a change
      update-memory true

      ;; annual reset of counters
      set dormant 0
      set orders []
    ]

    ;----- strategy -----;
    ; either the processor has been dormant too long OR they've tried lots too many changes
    ;if ((dormant > max-dormant) OR (long-budget < max-change)) [
    if (((dormant > max-dormant) OR (changed > max-changes))) AND review-strategy? [
      ;print (sentence ticks " " self  " dormant: " dormant " v " max-dormant " | long-budget " long-budget " v " max-change "  | " changed " v " max-changes)
      check-strategy

      ;; update memory for local case-base of what was just completed
      ; only do this periodically to prevent the casebase unnecessarily
      ; duplicating every step, e.g. when there is/could be a change
      update-memory true
    ]


    ;; long-term review of strategy
    if budget-strategy? [
      (ifelse budget < (-10000 * max-budget) [
        print "budget shortfall"
        check-strategy
        update-memory true


        ;; accumulate negative budgets
        ;; set long-budget long-budget + budget
      ] budget > ( 2 * max-budget) [
        ;; ask processors to invest (increase capacity) if
        ;; budget surplus
        ;      if reset-capacity? [
        ;        ;; need to identify which process
        ;        ;table:put capacity ratelimit
        ;      ]
        ;if re-invest? [invest]
      ][
        ; default
        ]
      )
    ]

    ;; tally how long there have been no orders
    if length orders = 0 [
      set dormant dormant + 1
      set long-dormant long-dormant + 1
    ]

    ;; restart daily budget if the switch on the interface is on
    if reset-budget? [
      restart-budget
    ]

    ;; kill off if changed too often
    if changed > max-changes [; tally times changed and kill if too many
      set closed TRUE
      set color 5 ;grey
                  ;kill-off

      ; close consumers too
      ask consumers-here [
        table:clear demand
        set color 5 ;grey
      ]
    ]
  ]

  ; depreciate products
  ask products [
    depreciate
  ]



  if output? [
    ;    if (ticks mod 10) = 0 [
    ;      output-results
    ;    ]

    output-results-table

    table:clear output-used
    table:clear output-made
    table:clear output-equation
    table:clear output-imported
  ]

  if print-index? [
    print (word "\n")
    print (sentence "Time: " ticks)
    ;print [orders] of index-case
    ;ask index-case [print (word "Running: " [name] of out-implement-neighbors " " budget)]
    ask index-case [
      print (sentence self " Running " running)
    ]
  ]

  ;; stop if everybody is closed down
  if NOT any? processors with [closed = FALSE] [ stop ]

  if video? [
    export-view (word "stravvs_view_" ticks ".png")
    export-interface (word "stravvs_interface_" ticks ".png")
  ]


;print (word "\n")
;print (word ticks)
;print (word "Ordered   " things-ordered)
;print (word "Imported  " things-imported)
;print (word "Made      " things-made)
;print (word "Delivered " things-delivered)
;print (word "Consumed  " things-consumed)


end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        1         2         3         4         5         6          7         8
;23456789012345678901234567890123456789012345678901234567890123445678901234567890
;
; GUI helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to product-clr
  set product-colour table:make

  foreach table:keys product-values [ x ->
    table:put product-colour x one-of base-colors
  ]
end

to equation-clr
  set equation-colour table:make

  ask processes [
    table:put equation-colour [name] of self one-of base-colors
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        1         2         3         4         5         6          7         8
;23456789012345678901234567890123456789012345678901234567890123445678901234567890
;
; Standardized message output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to output-error [string]
  output-print (word "ERROR [" timer "]: " string)
  set error-state? true
  error string
end

to output-warning [string]
  if warnings = 0 [
    set warnings table:make
  ]
  ifelse table:has-key? warnings string [
    table:put warnings string 1 + table:get warnings string
  ] [
    output-print (word "WARNING [" timer "]: " string)
    table:put warnings string 1
  ]
end

to output-note [string]
  if notes = 0 [
    set notes table:make
  ]
  ifelse table:has-key? notes string [
    table:put notes string 1 + table:get notes string
  ] [
    output-print (word "NOTE [" timer "]: " string)
    table:put notes string 1
  ]
end

to print-progress [string]
  print (word "PROGRESS [" timer "]: " string)
end



;; print output to see products in the graph in an R-suitable format
to output-data
  let result []
  foreach table:keys product-values [ x ->
    set result lput count products with [product-type = x] result
  ]
  if ticks = 0 [
    if file-exists? "graphdata.txt" [file-delete "graphdata.txt"]

    file-open "graphdata.txt"
    file-print but-last but-first( word (sentence "time" table:keys product-values))
    file-close
  ]
  if ( ticks mod 1 ) = 0 [
    file-open "graphdata.txt"
    file-print but-last but-first ( word (sentence (time:show time:plus (time:create-with-format start-date "dd-MM-YYYY") ticks tick-unit "dd-MM-YYYY") result))
    file-close
  ]
end


;; population table of all products (as keys) with a list of ingredients for each
;; BEN ;; change this to look at products based on links
to product-table
  ; allocate space for the products table that details all products and their ingredients
  set product-types table:make
  ;print product-types
  ; allocate space for the products table that details all products and their catalysts
  set product-catalysts table:make

  ; ask every product what they are and add to the table
  ask products [
    table:put product-types product-type []
    table:put product-catalysts product-type []
  ]

  ; ask each process about each product, if it is an input
  ; for another product, then add in a list
  ask processes [
    let what-out []
    let what-in []
    let what-needed []

    ask my-out-outputs [
      set what-out lput [product-type] of end2 what-out
    ]
    ask my-in-inputs [
      set what-in lput [product-type] of end1 what-in
    ]
    ask my-in-catalysts [
      set what-needed lput [product-type] of end1 what-needed
    ]

    foreach what-out [thing ->
      table:put product-types thing lput what-in table:get product-types thing
    ]

    foreach what-out [thing ->
      table:put product-catalysts thing lput what-needed table:get product-catalysts thing
    ]
  ]
end

to-report n-business-shut
  report count processors with [closed = TRUE]
end

;to-report n-business-changed
;  report n-changed
;end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; monitor attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report counts

  ;; processors
  let Norders 0
  let Nrunning 0
  let Nbought 0
  let Nresources 0

  ask processors [
    set Norders Norders + length table:values orders
    set Nrunning Nrunning + length running
    set Nbought Nbought + length table:values ordered
    set Nresources Nresources + length table:values resources
  ]

  ;; consumers
  let Ndemand 0
  let Nconsumption 0
  let Nunfulfilled-demand 0
  let Nbought2 0

  ask consumers [
    set Ndemand Ndemand + length table:values demand
    set Nconsumption Nconsumption + length table:values consumption
    set Nunfulfilled-demand Nunfulfilled-demand + length table:values unfulfilled-demand
    set Nbought2 Nbought2 + length table:values ordered
  ]

  ;; globals
  let Nsuppliers table:length suppliers
  let Nproducts count products
  let Nturtles count turtles
  let Nstocks count stocks

  report (word Norders " " Nrunning " " Nbought " " Nresources " | " Ndemand " " Nconsumption " " Nunfulfilled-demand " " Nbought2 " | " Nsuppliers " " Nproducts " " Nturtles " " Nstocks)
end


; adapted from https://stackoverflow.com/questions/32100378/count-the-number-of-occurrences-of-each-item-in-a-list
to-report frequency [an-item a-list]
  report length (filter [ i -> i = an-item] a-list)
end

; from https://stackoverflow.com/questions/33570658/how-make-a-list-of-cumulative-sum-in-netlogo
to-report partial-sums [lst]
  report butfirst reduce [[result-so-far next-item] -> lput (next-item + last
    result-so-far) result-so-far] fput [0] lst
end

to-report outtab [table-name]

  let out-order ""
  foreach products-unique [ i ->
    ifelse table:has-key? table-name i [
      set out-order (word out-order " " table:get table-name i)
    ][
      set out-order (word out-order " 0")
    ]
  ]
  report  out-order
  ;report (sentence map [ x ->  (word item 0 x "|" item 1 x) ] table:to-list table-name)
end



to-report N-table [table-name]
  report map [i -> (word table-name "_" i)] products-unique
end



to output-results
  ; number of each equation run
  let eq []
  ask implements [
    if [closed] of end1 = FALSE [
      set eq lput [name] of end2 eq
    ]
  ]

  ; need to make soft-coded
  let eq-n map [ i -> frequency i eq] sort [name] of processes

  (ifelse file-exists? output-file [
    file-open output-file
    file-print (sentence ticks " " eq-n " " count processors with [closed = FALSE] " " mean [budget] of processors " " outtab things-ordered " " outtab things-made " "outtab things-delivered " " outtab things-consumed)
    file-close
  ] [
    file-open output-file
    file-print (sentence world "," node-type "," edge-type "," equation-file "," max-budget "," max-change "," max-changes "," max-dormant)
    ; unique products now in fixed order
    set products-unique sort table:keys product-types
    file-print (sentence "ticks " sort [name] of processes " active_processors_count mean_processors_budget " N-table "ordered" " " N-table "made" " " N-table "delivered" " " N-table "consumed")
    file-close
  ])
end

to output-results-table ;[title-order]
  if not file-exists? output-file [
    file-open output-file

    let headA map [i -> (word "used-" i)] product-header
    let headB map [i -> (word "made-" i)] product-header
    let headC map [i -> (word "import-" i)] product-header
    let what (sentence equation-header headA headB headC)
    ;file-print (sentence "tick" " " what)
    file-print csv:to-row what
    file-close
  ]

  file-open output-file
  ;; equations
  let what-values []
  foreach equation-header [i ->
    ifelse table:has-key? output-equation i [set what-values lput table:get output-equation i what-values
    ][
      set what-values lput 0 what-values
    ]
  ]
  ;; things used up
  foreach product-header [i ->
    ifelse table:has-key? output-used i [set what-values lput table:get output-used i what-values
    ][
      set what-values lput 0 what-values
    ]
  ]
  ;; things made
  foreach product-header [i ->
    ifelse table:has-key? output-made i [set what-values lput table:get output-made i what-values
    ][
      set what-values lput 0 what-values
    ]
  ]
  ;; things imported
  foreach product-header [i ->
    ifelse table:has-key? output-imported i [set what-values lput table:get output-imported i what-values
    ][
      set what-values lput 0 what-values
    ]
  ]
  ;file-print (sentence ticks " " what-values)
  file-print csv:to-row what-values
  file-close
end


to output-update [the-table the-thing the-amount]
  if not table:has-key? the-table the-thing [
    table:put the-table the-thing 0
  ]
  let previous table:get the-table the-thing
  table:put the-table the-thing (previous + the-amount)
end


to-report output-save
  let out []
  ; list how much of each product is in each stage of production
  foreach table:keys product-types [ x ->
    let count-ordered 0
    if table:has-key? things-ordered x [set count-ordered table:get things-ordered x]

    let count-made 0
    if table:has-key? things-made x [set count-made table:get things-made x]

    let count-delivered 0
    if table:has-key? things-delivered x [set count-delivered table:get things-delivered x]

    let count-imported 0
    if table:has-key? things-imported x [set count-imported table:get things-imported x]

    let count-consumed 0
    if table:has-key? things-consumed x [set count-consumed table:get things-consumed x]

    let count-amount 0
    if table:has-key? things-amount x [set count-amount table:get things-amount x]

    set out lput (sentence "product " x " " count-ordered " " count-made " " count-delivered " " count-imported " " count-consumed " " count-amount) out
  ]
  ; add list of equations
  let quantity []
  ; equations are not always run, but if not then they do not appear in the things-run table
  foreach [name] of processes [ x ->
    ifelse table:has-key? things-run x [ ; if it was recorded
    set quantity table:get things-run x
    set out lput (sentence "equation " x " " quantity) out
    ][ ; if it was NOT recorded
      set quantity 0
      set out lput (sentence "equation " x " " quantity) out
    ]
  ]
  ; add list of business status
  set out fput (sentence "[business open " count processors with [closed = FALSE] "] [business closed " n-business-shut "] [business strategies " n-strategical-changes "] [business tactics " n-tactical-changes "] [business budgets " sum [budget] of processors with [closed = FALSE] "]") out
  report out
end
@#$#@#$#@
GRAPHICS-WINDOW
5
149
574
719
-1
-1
17.0
1
10
1
1
1
0
0
0
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
6
111
72
144
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
73
111
136
144
step
go
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
0

BUTTON
137
111
200
144
NIL
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
0

INPUTBOX
7
10
130
70
run-seed
4.73587666E8
1
0
Number

SWITCH
7
74
130
107
new-seed?
new-seed?
0
1
-1000

TEXTBOX
154
12
311
52
STRAVVS (version 1.0)
14
0.0
1

BUTTON
203
111
349
144
profile (100 steps)
setup                  ;; set up the model\nprofiler:start         ;; start profiling\nrepeat 100 [ go ]       ;; run something you want to measure\nprofiler:stop          ;; stop profiling\nprint profiler:report  ;; view the results\nprofiler:reset         ;; clear the data
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
5
731
206
876
Ordered
NIL
count (log10)
0.0
10.0
0.0
30.0
true
false
"foreach table:keys product-types [ x ->\n    create-temporary-plot-pen (word x)\n    ;set-plot-pen-color one-of base-colors\n    set-plot-pen-color table:get product-colour x\n]" "foreach table:keys things-ordered [ x ->\n  set-current-plot-pen (word x)\n    set-plot-pen-mode 2; points\n  ;plot count products with [product-type = x]\n  let quantity table:get things-ordered x\n  ifelse quantity > 0 [\n    plotxy ticks  log quantity 10]\n    [ \n    plotxy ticks 0 \n  ]\n]\n\n"
PENS

PLOT
212
731
409
874
Made
NIL
Count (log10)
0.0
10.0
0.0
30.0
true
false
"foreach table:keys product-types [ x ->\n    create-temporary-plot-pen (word x)\n    ;set-plot-pen-color one-of base-colors\n    set-plot-pen-color table:get product-colour x\n]" "foreach table:keys things-made [ x ->\n  set-current-plot-pen (word x)\n    set-plot-pen-mode 2; points\n    ;plot count products with [product-type = x]\n  let quantity table:get things-made x\n  ifelse quantity > 0 [\n    plotxy ticks  log quantity 10]\n    [ \n    plotxy ticks 0 \n  ]\n\n]\n\n"
PENS

PLOT
413
732
607
874
Delivered
NIL
Count (log10)
0.0
10.0
0.0
30.0
true
false
"foreach table:keys product-types [ x ->\n    create-temporary-plot-pen (word x)\n    ;set-plot-pen-color one-of base-colors\n    set-plot-pen-color table:get product-colour x\n]" "foreach table:keys things-delivered [ x ->\n  set-current-plot-pen (word x)\n    set-plot-pen-mode 2; points\n  ;plot count products with [product-type = x]\n  let quantity table:get things-delivered x\n  ifelse quantity > 0 [\n    plotxy ticks  log quantity 10]\n    [ \n    plotxy ticks 0 \n  ]\n\n]\n\n"
PENS

PLOT
5
879
608
1023
Producer budgets
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "ask processors [\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks budget\n]\n"
PENS
"zero" 1.0 0 -3026479 false "" "plotxy ticks 0"

PLOT
611
732
808
875
Consumed
NIL
Count (log10)
0.0
10.0
0.0
5.0
true
false
"foreach table:keys product-types [ x ->\n    create-temporary-plot-pen (word x)\n    ;set-plot-pen-color one-of base-colors\n    set-plot-pen-color table:get product-colour x\n]" "foreach table:keys things-consumed [ x ->\n  set-current-plot-pen (word x)\n  set-plot-pen-mode 2; points\n  ;plot count products with [product-type = x]\n  let quantity table:get things-consumed x\n  ifelse quantity > 0 [\n    plotxy ticks  log quantity 10]\n    [ \n    ;plotxy ticks 0 \n  ]\n\n]\n\n"
PENS

PLOT
611
878
809
1023
imported
NIL
log 10
0.0
10.0
0.0
10.0
true
false
"foreach table:keys product-types [ x ->\n    create-temporary-plot-pen (word x)\n    ;set-plot-pen-color one-of base-colors\n    set-plot-pen-color table:get product-colour x\n]" "foreach table:keys things-imported [ x ->\n  set-current-plot-pen (word x)\n    set-plot-pen-mode 2; points\n  ;plot count products with [product-type = x]\n  let quantity table:get things-imported x\n  ifelse quantity > 0 [\n    plotxy ticks  log quantity 10 ]\n    [ \n    plotxy ticks 0 \n  ]\n]\n\n"
PENS

INPUTBOX
137
47
355
107
init-file
v11_inits.txt
1
0
String

MONITOR
613
677
670
722
Closed
n-business-shut
17
1
11

BUTTON
705
280
800
313
outN
let things table:keys product-types\n\nshow (word \"Products :\" things)\nshow (word \"Ordered :\" things-ordered)\n\nlet out-order \"\"\nforeach table:keys product-types [ i ->\n  ifelse table:has-key? things-ordered i [\n      set out-order (word out-order \" \" table:get things-ordered i) \n    ][\n      set out-order (word out-order \" 0\")\n  ]\n]\n\nshow out-order  \n\n
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
705
312
800
345
who what?
let nprint 10\n(ifelse count processors > 10 [\nset nprint 10\n][\nset nprint count processors\n])\n\nprint (word \"Printing \" nprint \" processors...\")\n\nask n-of nprint processors [\n  ifelse closed = TRUE [\n    print (word self \" does: \")\n    print (word \"CLOSED\")\n    print (word \"\\n\")\n  ][\n    print (word self \" does: \")\n    ask out-implement-neighbors[ print name]\n    print (word \" has: \" )\n    print stock-summary\n    print (word \" asked by: \" orders)\n    print (word \" asking for: \" ordered)\n    print (word \"\\n\")\n  ]\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1090
78
1239
138
max-budget
100000.0
1
0
Number

INPUTBOX
1090
140
1239
200
max-change
-50.0
1
0
Number

INPUTBOX
1091
264
1240
324
max-dormant
5.0
1
0
Number

INPUTBOX
1091
202
1240
262
max-changes
3.0
1
0
Number

SWITCH
589
430
723
463
output?
output?
1
1
-1000

BUTTON
705
345
800
378
suppliers?
foreach table:keys product-types [ x ->\n  let get-suppliers []\n  ifelse suppliers-of x != nobody [\n    ask suppliers-of x [\n      set get-suppliers lput (word breed \" (\" [who] of self \")\") get-suppliers\n    ]\n    ][\n      set get-suppliers \"nobody\"\n    ]\n  print (sentence x \" -> \" get-suppliers)\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
589
466
719
499
print-index?
print-index?
1
1
-1000

INPUTBOX
1090
468
1239
528
N-local-importers
3.0
1
0
Number

INPUTBOX
1085
767
1234
827
consumer-demand
beef.meat.kg
1
0
String

INPUTBOX
1349
930
1498
990
random-capacity
1.0
1
0
Number

BUTTON
596
313
697
346
equation-costs
ask processes [ \n  let costs 0\n  ask in-input-neighbors [\n    set costs costs + (amount * item 0 table:get product-values product-type)\n    ;print (sentence myself \" \" self \" \" product-type \" \" item 0 table:get product-values product-type)\n  ]\n  \n  let cat-costs 0\n  ask in-catalyst-neighbors [\n    ; catalyst costs / shelflife\n    set cat-costs cat-costs + (amount * (item 0 table:get product-values product-type) / (item 1 table:get product-values product-type))\n    ;print (sentence myself \" \" self \" \" product-type \" \" item 0 table:get product-values product-type)\n  ]\n  \n  let income 0\n  ask out-output-neighbors [\n    set income income + (amount * item 0 table:get product-values product-type)\n    ;print (sentence myself \" \" self \" \" product-type \" \" item 0 table:get product-values product-type)\n  ]\n  \n  print (sentence name \":  -\" costs \"[ -\" cat-costs \" ] +\" income \" = \" (0 - costs - cat-costs + income))\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
590
394
725
427
patch-yield?
patch-yield?
1
1
-1000

SWITCH
588
505
722
538
use-histogram?
use-histogram?
1
1
-1000

TEXTBOX
831
59
981
77
Inputs\n
11
0.0
1

INPUTBOX
829
378
1058
438
node-file
nodes_v1.txt
1
0
String

INPUTBOX
830
487
1059
547
edge-file
edges_v1.txt
1
0
String

INPUTBOX
1085
704
1314
764
consumer-file
scotland/consumer_short.csv
1
0
String

INPUTBOX
1347
742
1626
802
equation-file
scotland/FMH_equations_short.txt
1
0
String

INPUTBOX
1348
867
1627
927
product-attributes
scotland/FMH_products_short.txt
1
0
String

INPUTBOX
1348
805
1627
865
yield-file
scotland/FMH_yields_short.txt
1
0
String

INPUTBOX
1287
79
1516
139
supplier-choice
spread
1
0
String

INPUTBOX
1288
140
1517
200
next-delivery-choice
all-random
1
0
String

INPUTBOX
1288
201
1517
261
first-delivery-choice
all-random
1
0
String

INPUTBOX
1289
262
1518
322
transporter-choice
all-random
1
0
String

INPUTBOX
1289
323
1518
383
tactical-choice
cbr
1
0
String

INPUTBOX
1289
384
1518
444
strategical-choice
cbr
1
0
String

INPUTBOX
1290
490
1519
550
tick-unit
months
1
0
String

INPUTBOX
1290
553
1519
613
start-date
01-01-2023
1
0
String

INPUTBOX
1903
68
2132
128
river-num
2.0
1
0
Number

INPUTBOX
1904
129
2133
189
processor-num
20.0
1
0
Number

INPUTBOX
1905
190
2134
250
nesting-cliff-num
3.0
1
0
Number

INPUTBOX
1906
252
2135
312
nesting-cliff-size
5.0
1
0
Number

INPUTBOX
1906
313
2135
373
beach-num
3.0
1
0
Number

INPUTBOX
1907
374
2136
434
beach-size
5.0
1
0
Number

INPUTBOX
1908
436
2137
496
fertile-land-num
3.0
1
0
Number

INPUTBOX
1908
497
2137
557
fertile-land-size
5.0
1
0
Number

INPUTBOX
1908
558
2137
618
grazing-land-num
3.0
1
0
Number

INPUTBOX
1909
619
2138
679
grazing-land-size
5.0
1
0
Number

INPUTBOX
1909
680
2058
740
additional-trade-prob
1.0
1
0
Number

INPUTBOX
1909
741
2138
801
processor-layout
exponential
1
0
String

INPUTBOX
1909
803
2058
863
exponent
10.0
1
0
Number

INPUTBOX
1910
887
2059
947
n-samples
300.0
1
0
Number

CHOOSER
830
80
968
125
world
world
"kilda" "scotland" "socialsim" "socialsim2" "tiny"
1

CHOOSER
829
440
967
485
node-type
node-type
"Regular" "Inward" "Outward" "Corner" "Embankment" "Channel" "Side"
6

CHOOSER
831
549
969
594
edge-type
edge-type
"Grid" "Star" "Snake" "Vane" "Fern" "Galaxy" "Terrace" "Comb" "Myriapod" "Concentric" "Target" "Miller"
0

TEXTBOX
1294
56
1444
74
Choice mechanism
11
0.0
1

TEXTBOX
1904
44
2054
62
Environment simulation\n
11
0.0
1

TEXTBOX
1091
59
1241
77
Processor budget
11
0.0
1

INPUTBOX
828
229
1057
289
region-file
scotland/ERSA_regions.shp
1
0
String

INPUTBOX
830
919
979
979
n-non-file-transporters
1.0
1
0
Number

INPUTBOX
830
858
1059
918
transporter-file
NA
1
0
String

SWITCH
358
48
478
81
input-file?
input-file?
0
1
-1000

SWITCH
828
194
985
227
use-region-file?
use-region-file?
0
1
-1000

SWITCH
828
344
969
377
use-network?
use-network?
1
1
-1000

SWITCH
830
633
995
666
import-anything?
import-anything?
1
1
-1000

SWITCH
830
670
991
703
local-resources?
local-resources?
1
1
-1000

SWITCH
830
823
1018
856
use-transporter-file?
use-transporter-file?
1
1
-1000

SWITCH
831
981
1002
1014
instant-transport?
instant-transport?
0
1
-1000

SWITCH
1086
669
1255
702
consumer-series?
consumer-series?
1
1
-1000

SWITCH
831
705
1002
738
use-importer-file?
use-importer-file?
0
1
-1000

SWITCH
1345
635
1525
668
use-processor-file?
use-processor-file?
0
1
-1000

INPUTBOX
830
740
1059
800
importer-file
scotland/importers.csv
1
0
String

INPUTBOX
1346
671
1624
731
processor-file
scotland/simfarms_10pc_short.csv
1
0
String

SWITCH
1085
634
1264
667
use-consumer-file?
use-consumer-file?
0
1
-1000

INPUTBOX
1086
829
1315
889
consumer-timeseries-file
scotland/consumer_timeseries.csv
1
0
String

SWITCH
1091
326
1240
359
reset-budget?
reset-budget?
1
1
-1000

PLOT
5
1029
1113
1333
Equations
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"foreach table:keys equation-colour [ x ->\n    create-temporary-plot-pen (word x)\n    ;set-plot-pen-color one-of base-colors\n    set-plot-pen-color table:get equation-colour x\n]" "foreach table:keys things-run [ x ->\n  set-current-plot-pen (word x)\n  let quantity table:get things-run x\n  plotxy ticks quantity    \n]\n\n"
PENS

SWITCH
596
169
746
202
demand-random?
demand-random?
1
1
-1000

SWITCH
596
202
746
235
reset-capacity?
reset-capacity?
1
1
-1000

SWITCH
596
234
746
267
re-invest?
re-invest?
1
1
-1000

SWITCH
596
137
746
170
seed-resources?
seed-resources?
1
1
-1000

SLIDER
1091
362
1268
395
strategy-compare
strategy-compare
0
100
3.0
1
1
%
HORIZONTAL

MONITOR
672
676
746
721
Strategies
n-strategical-changes
17
1
11

MONITOR
748
676
805
721
Tactics
n-tactical-changes
17
1
11

SWITCH
1086
915
1260
948
use-regulator-file?
use-regulator-file?
1
1
-1000

INPUTBOX
1086
950
1315
1010
regulator-file
scotland/regulators.csv
1
0
String

SWITCH
362
112
467
145
testing?
testing?
1
1
-1000

BUTTON
734
87
797
120
test
print \"\\n\"\n\nask one-of consumers [\n  make-order\n]\n\nprint \"\\n\"\n\nask one-of processors [\n  make-order  \n]\n\nprint \"\\n\"\n\nask importers [\n  make-good\n]\n\nask processors [\n  make-good\n]\n\nask importers [\n  deliver-good\n]\n  \nask processors [\n  deliver-good\n  ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1522
385
1602
445
n-choices
3.0
1
0
Number

SWITCH
470
112
573
145
video?
video?
1
1
-1000

BUTTON
596
280
695
313
things-done
print (word \"\\n\")\nprint (word ticks)\nprint (word \"Ordered   \" things-ordered)\nprint (word \"Imported  \" things-imported)\nprint (word \"Made      \" things-made)\nprint (word \"Delivered \" things-delivered)\nprint (word \"Consumed  \" things-consumed)
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1015
17
1164
50
review-tactics?
review-tactics?
0
1
-1000

SWITCH
1166
17
1325
50
review-strategy?
review-strategy?
0
1
-1000

SWITCH
856
17
1013
50
review-subsidy?
review-subsidy?
1
1
-1000

INPUTBOX
594
10
823
70
output-dir
scotland
1
0
String

SWITCH
1084
594
1264
627
round-catalysts-orders?
round-catalysts-orders?
1
1
-1000

SWITCH
1083
559
1230
592
max-capacity?
max-capacity?
1
1
-1000

SWITCH
480
47
576
80
verbose?
verbose?
1
1
-1000

TEXTBOX
1586
62
1736
80
CBR
11
0.0
1

INPUTBOX
1584
81
1671
141
key-match
5.0
1
0
Number

INPUTBOX
1584
143
1670
203
value-match
3.0
1
0
Number

SWITCH
1591
252
1788
285
restockON-rescaleOFF
restockON-rescaleOFF
0
1
-1000

BUTTON
596
346
697
379
show-capacity
ask processors [ show capacity ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
588
585
727
618
multi-match?
multi-match?
0
1
-1000

SWITCH
832
301
1003
334
charge-transport?
charge-transport?
1
1
-1000

INPUTBOX
734
395
827
455
yield-precision
2.0
1
0
Number

BUTTON
21
163
107
196
amounts
\nprint (word \n\"A \" sum [amount] of products with [product-type = \"A\" AND shelflife != \"archetype\"] \" / \" count products with [product-type = \"A\" AND shelflife != \"archetype\"]\n\"   |   B \"  sum [amount] of products with [product-type = \"B\" AND shelflife != \"archetype\"] \" / \" count products with [product-type = \"B\" AND shelflife != \"archetype\"]\n\"   |   C \"  sum [amount] of products with [product-type = \"C\" AND shelflife != \"archetype\"] \" / \" count products with [product-type = \"C\" AND shelflife != \"archetype\"]\n\"   |   D \"  sum [amount] of products with [product-type = \"D\" AND shelflife != \"archetype\"] \" / \" count products with [product-type = \"D\" AND shelflife != \"archetype\"]\n\"   |   E \"  sum [amount] of products with [product-type = \"E\" AND shelflife != \"archetype\"] \" / \" count products with [product-type = \"E\" AND shelflife != \"archetype\"]\n\"   |   F \"  sum [amount] of products with [product-type = \"F\" AND shelflife != \"archetype\"] \" / \" count products with [product-type = \"F\" AND shelflife != \"archetype\"]\n)\n
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
600
88
717
121
1-step profile
setup                  ;; set up the model\nprofiler:start         ;; start profiling\nrepeat 1 [ go ]       ;; run something you want to measure\nprofiler:stop          ;; stop profiling\nprint profiler:report  ;; view the results\nprofiler:reset         ;; clear the data
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1327
17
1491
50
budget-strategy?
budget-strategy?
1
1
-1000

@#$#@#$#@
# STRAVVS Documentation

## WHAT IS IT?

STRAVVS is a model of value chains, built for the Integrated Socio-Environmental Modelling of Policy Scenarios for Scotland project: 
https://large-scale-modelling.hutton.ac.uk/
but intended for wider possible application. It simulates consumers whose demand for products causes processors to run the processes that create them. It also simulates the transportation of the goods, representing the whole network of a value system as far as is desired.

## HOW IT WORKS

Using configuration files, a network of processes is created that consume ingredients and use 'catalysts' to make end-user products. Processors are assigned to these processes, and importer agents created to bring products into the system that are not made within it. Consumers are then created that demand the end-user products from the processors that make them. This demand drives the value system to manufacture the end-user products via any intermediary products, thereby drawing goods through the value network.

## HOW TO USE IT

The model requires a file containing equations and a file containing agents.

If `instant-transport?` is `Off` then transporter agents drive the goods (not, currently, in any especially 'smart' way) from one place to another. If `On`, then the transporter agents are hidden, and transportation of goods takes place instantly.

## THINGS TO NOTICE

The main thing to notice at present is all the transporters sending goods everywhere when `instant-transport?` is `Off`. You can also see the stock of importers, processors and consumers in a 'ring' around them. Useful potential visualizations to implement include:

  * Reflecting the extent to which different transportation links are used
  * Volumes of goods imported each time step
  * Volumes of goods consumed each time step
  * Volumes of goods manufactured each time step
  * Volumes of goods transported each time step
  * Transport distances for each good (would need more information recorded)
  * Volumes of 'byproducts' (that no-one uses)
  * Laden and unladen distances travelled by transporters

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

At this stage, the implementation is highly simplified. The model needs the following enhancements to be capable of exploring value chain disruption scenarios

  * Create the setup function and the ability to load in data from various files
  * Implement a transport network that transporters have to follow and routefollowing in that network
  * Create 'smarter' transporter agents that do more to make their journeys efficient
  * Add depots for transporters
  * Add 'collector' agents (basically, like supermarkets -- they don't manufacture anything, but provide 'one stop shops' for consumers)
  * Allow agents to stockpile products
  * Allow (by)products to 'decay' and/or diffuse/pollute
  * Allow new processes and processor agents to be created
  * Give processors 'capacity' (treating the numbers in the process equations as ratios rather than absolute amounts)
  * Allow pollution from transportation
  * Allow initial setups to have stocks already
  * Allow different kinds of transportation (e.g. boat, plane) and different specialisms for transportation (e.g. perishable produce, livestock)
  * Allow importers to 'compete' with processors and export markets for products
  * Allow substitute products
  * Implement costs and pricing

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## ACKNOWLEDGEMENTS


## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)

# LICENCE
```text
                        GNU GENERAL PUBLIC LICENSE
                           Version 3, 29 June 2007
    
     Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
     Everyone is permitted to copy and distribute verbatim copies
     of this license document, but changing it is not allowed.
    
                                Preamble
    
      The GNU General Public License is a free, copyleft license for
    software and other kinds of works.
    
      The licenses for most software and other practical works are designed
    to take away your freedom to share and change the works.  By contrast,
    the GNU General Public License is intended to guarantee your freedom to
    share and change all versions of a program--to make sure it remains free
    software for all its users.  We, the Free Software Foundation, use the
    GNU General Public License for most of our software; it applies also to
    any other work released this way by its authors.  You can apply it to
    your programs, too.
    
      When we speak of free software, we are referring to freedom, not
    price.  Our General Public Licenses are designed to make sure that you
    have the freedom to distribute copies of free software (and charge for
    them if you wish), that you receive source code or can get it if you
    want it, that you can change the software or use pieces of it in new
    free programs, and that you know you can do these things.
    
      To protect your rights, we need to prevent others from denying you
    these rights or asking you to surrender the rights.  Therefore, you have
    certain responsibilities if you distribute copies of the software, or if
    you modify it: responsibilities to respect the freedom of others.
    
      For example, if you distribute copies of such a program, whether
    gratis or for a fee, you must pass on to the recipients the same
    freedoms that you received.  You must make sure that they, too, receive
    or can get the source code.  And you must show them these terms so they
    know their rights.
    
      Developers that use the GNU GPL protect your rights with two steps:
    (1) assert copyright on the software, and (2) offer you this License
    giving you legal permission to copy, distribute and/or modify it.
    
      For the developers' and authors' protection, the GPL clearly explains
    that there is no warranty for this free software.  For both users' and
    authors' sake, the GPL requires that modified versions be marked as
    changed, so that their problems will not be attributed erroneously to
    authors of previous versions.
    
      Some devices are designed to deny users access to install or run
    modified versions of the software inside them, although the manufacturer
    can do so.  This is fundamentally incompatible with the aim of
    protecting users' freedom to change the software.  The systematic
    pattern of such abuse occurs in the area of products for individuals to
    use, which is precisely where it is most unacceptable.  Therefore, we
    have designed this version of the GPL to prohibit the practice for those
    products.  If such problems arise substantially in other domains, we
    stand ready to extend this provision to those domains in future versions
    of the GPL, as needed to protect the freedom of users.
    
      Finally, every program is threatened constantly by software patents.
    States should not allow patents to restrict development and use of
    software on general-purpose computers, but in those that do, we wish to
    avoid the special danger that patents applied to a free program could
    make it effectively proprietary.  To prevent this, the GPL assures that
    patents cannot be used to render the program non-free.
    
      The precise terms and conditions for copying, distribution and
    modification follow.
    
                           TERMS AND CONDITIONS
    
      0. Definitions.
    
      "This License" refers to version 3 of the GNU General Public License.
    
      "Copyright" also means copyright-like laws that apply to other kinds of
    works, such as semiconductor masks.
    
      "The Program" refers to any copyrightable work licensed under this
    License.  Each licensee is addressed as "you".  "Licensees" and
    "recipients" may be individuals or organizations.
    
      To "modify" a work means to copy from or adapt all or part of the work
    in a fashion requiring copyright permission, other than the making of an
    exact copy.  The resulting work is called a "modified version" of the
    earlier work or a work "based on" the earlier work.
    
      A "covered work" means either the unmodified Program or a work based
    on the Program.
    
      To "propagate" a work means to do anything with it that, without
    permission, would make you directly or secondarily liable for
    infringement under applicable copyright law, except executing it on a
    computer or modifying a private copy.  Propagation includes copying,
    distribution (with or without modification), making available to the
    public, and in some countries other activities as well.
    
      To "convey" a work means any kind of propagation that enables other
    parties to make or receive copies.  Mere interaction with a user through
    a computer network, with no transfer of a copy, is not conveying.
    
      An interactive user interface displays "Appropriate Legal Notices"
    to the extent that it includes a convenient and prominently visible
    feature that (1) displays an appropriate copyright notice, and (2)
    tells the user that there is no warranty for the work (except to the
    extent that warranties are provided), that licensees may convey the
    work under this License, and how to view a copy of this License.  If
    the interface presents a list of user commands or options, such as a
    menu, a prominent item in the list meets this criterion.
    
      1. Source Code.
    
      The "source code" for a work means the preferred form of the work
    for making modifications to it.  "Object code" means any non-source
    form of a work.
    
      A "Standard Interface" means an interface that either is an official
    standard defined by a recognized standards body, or, in the case of
    interfaces specified for a particular programming language, one that
    is widely used among developers working in that language.
    
      The "System Libraries" of an executable work include anything, other
    than the work as a whole, that (a) is included in the normal form of
    packaging a Major Component, but which is not part of that Major
    Component, and (b) serves only to enable use of the work with that
    Major Component, or to implement a Standard Interface for which an
    implementation is available to the public in source code form.  A
    "Major Component", in this context, means a major essential component
    (kernel, window system, and so on) of the specific operating system
    (if any) on which the executable work runs, or a compiler used to
    produce the work, or an object code interpreter used to run it.
        
      The "Corresponding Source" for a work in object code form means all
    the source code needed to generate, install, and (for an executable
    work) run the object code and to modify the work, including scripts to
    control those activities.  However, it does not include the work's
    System Libraries, or general-purpose tools or generally available free
    programs which are used unmodified in performing those activities but
    which are not part of the work.  For example, Corresponding Source
    includes interface definition files associated with source files for
    the work, and the source code for shared libraries and dynamically
    linked subprograms that the work is specifically designed to require,
    such as by intimate data communication or control flow between those
    subprograms and other parts of the work.
    
      The Corresponding Source need not include anything that users
    can regenerate automatically from other parts of the Corresponding
    Source.
    
      The Corresponding Source for a work in source code form is that
    same work.
    
      2. Basic Permissions.
    
      All rights granted under this License are granted for the term of
    copyright on the Program, and are irrevocable provided the stated
    conditions are met.  This License explicitly affirms your unlimited
    permission to run the unmodified Program.  The output from running a
    covered work is covered by this License only if the output, given its
    content, constitutes a covered work.  This License acknowledges your
    rights of fair use or other equivalent, as provided by copyright law.
    
      You may make, run and propagate covered works that you do not
    convey, without conditions so long as your license otherwise remains
    in force.  You may convey covered works to others for the sole purpose
    of having them make modifications exclusively for you, or provide you
    with facilities for running those works, provided that you comply with
    the terms of this License in conveying all material for which you do
    not control copyright.  Those thus making or running the covered works
    for you must do so exclusively on your behalf, under your direction
    and control, on terms that prohibit them from making any copies of
    your copyrighted material outside their relationship with you.
    
      Conveying under any other circumstances is permitted solely under
    the conditions stated below.  Sublicensing is not allowed; section 10
    makes it unnecessary.
    
      3. Protecting Users' Legal Rights From Anti-Circumvention Law.
    
      No covered work shall be deemed part of an effective technological
    measure under any applicable law fulfilling obligations under article
    11 of the WIPO copyright treaty adopted on 20 December 1996, or
    similar laws prohibiting or restricting circumvention of such
    measures.
    
      When you convey a covered work, you waive any legal power to forbid
    circumvention of technological measures to the extent such circumvention
    is effected by exercising rights under this License with respect to
    the covered work, and you disclaim any intention to limit operation or
    modification of the work as a means of enforcing, against the work's
    users, your or third parties' legal rights to forbid circumvention of
    technological measures.
    
      4. Conveying Verbatim Copies.
    
      You may convey verbatim copies of the Program's source code as you
    receive it, in any medium, provided that you conspicuously and
    appropriately publish on each copy an appropriate copyright notice;
    keep intact all notices stating that this License and any
    non-permissive terms added in accord with section 7 apply to the code;
    keep intact all notices of the absence of any warranty; and give all
    recipients a copy of this License along with the Program.
    
      You may charge any price or no price for each copy that you convey,
    and you may offer support or warranty protection for a fee.
    
      5. Conveying Modified Source Versions.
    
      You may convey a work based on the Program, or the modifications to
    produce it from the Program, in the form of source code under the
    terms of section 4, provided that you also meet all of these conditions:
    
        a) The work must carry prominent notices stating that you modified
        it, and giving a relevant date.
    
        b) The work must carry prominent notices stating that it is
        released under this License and any conditions added under section
        7.  This requirement modifies the requirement in section 4 to
        "keep intact all notices".
    
        c) You must license the entire work, as a whole, under this
        License to anyone who comes into possession of a copy.  This
        License will therefore apply, along with any applicable section 7
        additional terms, to the whole of the work, and all its parts,
        regardless of how they are packaged.  This License gives no
        permission to license the work in any other way, but it does not
        invalidate such permission if you have separately received it.
    
        d) If the work has interactive user interfaces, each must display
        Appropriate Legal Notices; however, if the Program has interactive
        interfaces that do not display Appropriate Legal Notices, your
        work need not make them do so.
    
      A compilation of a covered work with other separate and independent
    works, which are not by their nature extensions of the covered work,
    and which are not combined with it such as to form a larger program,
    in or on a volume of a storage or distribution medium, is called an
    "aggregate" if the compilation and its resulting copyright are not
    used to limit the access or legal rights of the compilation's users
    beyond what the individual works permit.  Inclusion of a covered work
    in an aggregate does not cause this License to apply to the other
    parts of the aggregate.
    
      6. Conveying Non-Source Forms.
    
      You may convey a covered work in object code form under the terms
    of sections 4 and 5, provided that you also convey the
    machine-readable Corresponding Source under the terms of this License,
    in one of these ways:
    
        a) Convey the object code in, or embodied in, a physical product
        (including a physical distribution medium), accompanied by the
        Corresponding Source fixed on a durable physical medium
        customarily used for software interchange.
    
        b) Convey the object code in, or embodied in, a physical product
        (including a physical distribution medium), accompanied by a
        written offer, valid for at least three years and valid for as
        long as you offer spare parts or customer support for that product
        model, to give anyone who possesses the object code either (1) a
        copy of the Corresponding Source for all the software in the
        product that is covered by this License, on a durable physical
        medium customarily used for software interchange, for a price no
        more than your reasonable cost of physically performing this
        conveying of source, or (2) access to copy the
        Corresponding Source from a network server at no charge.
    
        c) Convey individual copies of the object code with a copy of the
        written offer to provide the Corresponding Source.  This
        alternative is allowed only occasionally and noncommercially, and
        only if you received the object code with such an offer, in accord
        with subsection 6b.
    
        d) Convey the object code by offering access from a designated
        place (gratis or for a charge), and offer equivalent access to the
        Corresponding Source in the same way through the same place at no
        further charge.  You need not require recipients to copy the
        Corresponding Source along with the object code.  If the place to
        copy the object code is a network server, the Corresponding Source
        may be on a different server (operated by you or a third party)
        that supports equivalent copying facilities, provided you maintain
        clear directions next to the object code saying where to find the
        Corresponding Source.  Regardless of what server hosts the
        Corresponding Source, you remain obligated to ensure that it is
        available for as long as needed to satisfy these requirements.
    
        e) Convey the object code using peer-to-peer transmission, provided
        you inform other peers where the object code and Corresponding
        Source of the work are being offered to the general public at no
        charge under subsection 6d.
    
      A separable portion of the object code, whose source code is excluded
    from the Corresponding Source as a System Library, need not be
    included in conveying the object code work.
    
      A "User Product" is either (1) a "consumer product", which means any
    tangible personal property which is normally used for personal, family,
    or household purposes, or (2) anything designed or sold for incorporation
    into a dwelling.  In determining whether a product is a consumer product,
    doubtful cases shall be resolved in favor of coverage.  For a particular
    product received by a particular user, "normally used" refers to a
    typical or common use of that class of product, regardless of the status
    of the particular user or of the way in which the particular user
    actually uses, or expects or is expected to use, the product.  A product
    is a consumer product regardless of whether the product has substantial
    commercial, industrial or non-consumer uses, unless such uses represent
    the only significant mode of use of the product.
    
      "Installation Information" for a User Product means any methods,
    procedures, authorization keys, or other information required to install
    and execute modified versions of a covered work in that User Product from
    a modified version of its Corresponding Source.  The information must
    suffice to ensure that the continued functioning of the modified object
    code is in no case prevented or interfered with solely because
    modification has been made.
    
      If you convey an object code work under this section in, or with, or
    specifically for use in, a User Product, and the conveying occurs as
    part of a transaction in which the right of possession and use of the
    User Product is transferred to the recipient in perpetuity or for a
    fixed term (regardless of how the transaction is characterized), the
    Corresponding Source conveyed under this section must be accompanied
    by the Installation Information.  But this requirement does not apply
    if neither you nor any third party retains the ability to install
    modified object code on the User Product (for example, the work has
    been installed in ROM).
    
      The requirement to provide Installation Information does not include a
    requirement to continue to provide support service, warranty, or updates
    for a work that has been modified or installed by the recipient, or for
    the User Product in which it has been modified or installed.  Access to a
    network may be denied when the modification itself materially and
    adversely affects the operation of the network or violates the rules and
    protocols for communication across the network.
    
      Corresponding Source conveyed, and Installation Information provided,
    in accord with this section must be in a format that is publicly
    documented (and with an implementation available to the public in
    source code form), and must require no special password or key for
    unpacking, reading or copying.
    
      7. Additional Terms.
    
      "Additional permissions" are terms that supplement the terms of this
    License by making exceptions from one or more of its conditions.
    Additional permissions that are applicable to the entire Program shall
    be treated as though they were included in this License, to the extent
    that they are valid under applicable law.  If additional permissions
    apply only to part of the Program, that part may be used separately
    under those permissions, but the entire Program remains governed by
    this License without regard to the additional permissions.
    
      When you convey a copy of a covered work, you may at your option
    remove any additional permissions from that copy, or from any part of
    it.  (Additional permissions may be written to require their own
    removal in certain cases when you modify the work.)  You may place
    additional permissions on material, added by you to a covered work,
    for which you have or can give appropriate copyright permission.
    
      Notwithstanding any other provision of this License, for material you
    add to a covered work, you may (if authorized by the copyright holders of
    that material) supplement the terms of this License with terms:
    
        a) Disclaiming warranty or limiting liability differently from the
        terms of sections 15 and 16 of this License; or
    
        b) Requiring preservation of specified reasonable legal notices or
        author attributions in that material or in the Appropriate Legal
        Notices displayed by works containing it; or
    
        c) Prohibiting misrepresentation of the origin of that material, or
        requiring that modified versions of such material be marked in
        reasonable ways as different from the original version; or
    
        d) Limiting the use for publicity purposes of names of licensors or
        authors of the material; or
    
        e) Declining to grant rights under trademark law for use of some
        trade names, trademarks, or service marks; or
    
        f) Requiring indemnification of licensors and authors of that
        material by anyone who conveys the material (or modified versions of
        it) with contractual assumptions of liability to the recipient, for
        any liability that these contractual assumptions directly impose on
        those licensors and authors.
    
      All other non-permissive additional terms are considered "further
    restrictions" within the meaning of section 10.  If the Program as you
    received it, or any part of it, contains a notice stating that it is
    governed by this License along with a term that is a further
    restriction, you may remove that term.  If a license document contains
    a further restriction but permits relicensing or conveying under this
    License, you may add to a covered work material governed by the terms
    of that license document, provided that the further restriction does
    not survive such relicensing or conveying.
    
      If you add terms to a covered work in accord with this section, you
    must place, in the relevant source files, a statement of the
    additional terms that apply to those files, or a notice indicating
    where to find the applicable terms.
    
      Additional terms, permissive or non-permissive, may be stated in the
    form of a separately written license, or stated as exceptions;
    the above requirements apply either way.
    
      8. Termination.
    
      You may not propagate or modify a covered work except as expressly
    provided under this License.  Any attempt otherwise to propagate or
    modify it is void, and will automatically terminate your rights under
    this License (including any patent licenses granted under the third
    paragraph of section 11).
    
      However, if you cease all violation of this License, then your
    license from a particular copyright holder is reinstated (a)
    provisionally, unless and until the copyright holder explicitly and
    finally terminates your license, and (b) permanently, if the copyright
    holder fails to notify you of the violation by some reasonable means
    prior to 60 days after the cessation.
    
      Moreover, your license from a particular copyright holder is
    reinstated permanently if the copyright holder notifies you of the
    violation by some reasonable means, this is the first time you have
    received notice of violation of this License (for any work) from that
    copyright holder, and you cure the violation prior to 30 days after
    your receipt of the notice.
    
      Termination of your rights under this section does not terminate the
    licenses of parties who have received copies or rights from you under
    this License.  If your rights have been terminated and not permanently
    reinstated, you do not qualify to receive new licenses for the same
    material under section 10.
    
      9. Acceptance Not Required for Having Copies.
    
      You are not required to accept this License in order to receive or
    run a copy of the Program.  Ancillary propagation of a covered work
    occurring solely as a consequence of using peer-to-peer transmission
    to receive a copy likewise does not require acceptance.  However,
    nothing other than this License grants you permission to propagate or
    modify any covered work.  These actions infringe copyright if you do
    not accept this License.  Therefore, by modifying or propagating a
    covered work, you indicate your acceptance of this License to do so.
    
      10. Automatic Licensing of Downstream Recipients.
    
      Each time you convey a covered work, the recipient automatically
    receives a license from the original licensors, to run, modify and
    propagate that work, subject to this License.  You are not responsible
    for enforcing compliance by third parties with this License.
    
      An "entity transaction" is a transaction transferring control of an
    organization, or substantially all assets of one, or subdividing an
    organization, or merging organizations.  If propagation of a covered
    work results from an entity transaction, each party to that
    transaction who receives a copy of the work also receives whatever
    licenses to the work the party's predecessor in interest had or could
    give under the previous paragraph, plus a right to possession of the
    Corresponding Source of the work from the predecessor in interest, if
    the predecessor has it or can get it with reasonable efforts.
    
      You may not impose any further restrictions on the exercise of the
    rights granted or affirmed under this License.  For example, you may
    not impose a license fee, royalty, or other charge for exercise of
    rights granted under this License, and you may not initiate litigation
    (including a cross-claim or counterclaim in a lawsuit) alleging that
    any patent claim is infringed by making, using, selling, offering for
    sale, or importing the Program or any portion of it.
    
      11. Patents.
    
      A "contributor" is a copyright holder who authorizes use under this
    License of the Program or a work on which the Program is based.  The
    work thus licensed is called the contributor's "contributor version".
    
      A contributor's "essential patent claims" are all patent claims
    owned or controlled by the contributor, whether already acquired or
    hereafter acquired, that would be infringed by some manner, permitted
    by this License, of making, using, or selling its contributor version,
    but do not include claims that would be infringed only as a
    consequence of further modification of the contributor version.  For
    purposes of this definition, "control" includes the right to grant
    patent sublicenses in a manner consistent with the requirements of
    this License.
    
      Each contributor grants you a non-exclusive, worldwide, royalty-free
    patent license under the contributor's essential patent claims, to
    make, use, sell, offer for sale, import and otherwise run, modify and
    propagate the contents of its contributor version.
    
      In the following three paragraphs, a "patent license" is any express
    agreement or commitment, however denominated, not to enforce a patent
    (such as an express permission to practice a patent or covenant not to
    sue for patent infringement).  To "grant" such a patent license to a
    party means to make such an agreement or commitment not to enforce a
    patent against the party.
    
      If you convey a covered work, knowingly relying on a patent license,
    and the Corresponding Source of the work is not available for anyone
    to copy, free of charge and under the terms of this License, through a
    publicly available network server or other readily accessible means,
    then you must either (1) cause the Corresponding Source to be so
    available, or (2) arrange to deprive yourself of the benefit of the
    patent license for this particular work, or (3) arrange, in a manner
    consistent with the requirements of this License, to extend the patent
    license to downstream recipients.  "Knowingly relying" means you have
    actual knowledge that, but for the patent license, your conveying the
    covered work in a country, or your recipient's use of the covered work
    in a country, would infringe one or more identifiable patents in that
    country that you have reason to believe are valid.
    
      If, pursuant to or in connection with a single transaction or
    arrangement, you convey, or propagate by procuring conveyance of, a
    covered work, and grant a patent license to some of the parties
    receiving the covered work authorizing them to use, propagate, modify
    or convey a specific copy of the covered work, then the patent license
    you grant is automatically extended to all recipients of the covered
    work and works based on it.
    
      A patent license is "discriminatory" if it does not include within
    the scope of its coverage, prohibits the exercise of, or is
    conditioned on the non-exercise of one or more of the rights that are
    specifically granted under this License.  You may not convey a covered
    work if you are a party to an arrangement with a third party that is
    in the business of distributing software, under which you make payment
    to the third party based on the extent of your activity of conveying
    the work, and under which the third party grants, to any of the
    parties who would receive the covered work from you, a discriminatory
    patent license (a) in connection with copies of the covered work
    conveyed by you (or copies made from those copies), or (b) primarily
    for and in connection with specific products or compilations that
    contain the covered work, unless you entered into that arrangement,
    or that patent license was granted, prior to 28 March 2007.
    
      Nothing in this License shall be construed as excluding or limiting
    any implied license or other defenses to infringement that may
    otherwise be available to you under applicable patent law.
    
      12. No Surrender of Others' Freedom.
    
      If conditions are imposed on you (whether by court order, agreement or
    otherwise) that contradict the conditions of this License, they do not
    excuse you from the conditions of this License.  If you cannot convey a
    covered work so as to satisfy simultaneously your obligations under this
    License and any other pertinent obligations, then as a consequence you may
    not convey it at all.  For example, if you agree to terms that obligate you
    to collect a royalty for further conveying from those to whom you convey
    the Program, the only way you could satisfy both those terms and this
    License would be to refrain entirely from conveying the Program.
    
      13. Use with the GNU Affero General Public License.
    
      Notwithstanding any other provision of this License, you have
    permission to link or combine any covered work with a work licensed
    under version 3 of the GNU Affero General Public License into a single
    combined work, and to convey the resulting work.  The terms of this
    License will continue to apply to the part which is the covered work,
    but the special requirements of the GNU Affero General Public License,
    section 13, concerning interaction through a network will apply to the
    combination as such.
    
      14. Revised Versions of this License.
    
      The Free Software Foundation may publish revised and/or new versions of
    the GNU General Public License from time to time.  Such new versions will
    be similar in spirit to the present version, but may differ in detail to
    address new problems or concerns.
    
      Each version is given a distinguishing version number.  If the
    Program specifies that a certain numbered version of the GNU General
    Public License "or any later version" applies to it, you have the
    option of following the terms and conditions either of that numbered
    version or of any later version published by the Free Software
    Foundation.  If the Program does not specify a version number of the
    GNU General Public License, you may choose any version ever published
    by the Free Software Foundation.
    
      If the Program specifies that a proxy can decide which future
    versions of the GNU General Public License can be used, that proxy's
    public statement of acceptance of a version permanently authorizes you
    to choose that version for the Program.
    
      Later license versions may give you additional or different
    permissions.  However, no additional obligations are imposed on any
    author or copyright holder as a result of your choosing to follow a
    later version.
    
      15. Disclaimer of Warranty.
    
      THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
    APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
    HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
    OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
    THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
    IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
    ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
    
      16. Limitation of Liability.
    
      IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
    WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
    THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
    GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
    USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
    DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
    PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
    EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
    SUCH DAMAGES.
    
      17. Interpretation of Sections 15 and 16.
    
      If the disclaimer of warranty and limitation of liability provided
    above cannot be given local legal effect according to their terms,
    reviewing courts shall apply local law that most closely approximates
    an absolute waiver of all civil liability in connection with the
    Program, unless a warranty or assumption of liability accompanies a
    copy of the Program in return for a fee.
    
                         END OF TERMS AND CONDITIONS
    
                How to Apply These Terms to Your New Programs
    
      If you develop a new program, and you want it to be of the greatest
    possible use to the public, the best way to achieve this is to make it
    free software which everyone can redistribute and change under these terms.
    
      To do so, attach the following notices to the program.  It is safest
    to attach them to the start of each source file to most effectively
    state the exclusion of warranty; and each file should have at least
    the "copyright" line and a pointer to where the full notice is found.
    
        <one line to give the program's name and a brief idea of what it does.>
        Copyright (C) <year>  <name of author>
    
        This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or
        (at your option) any later version.
    
        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.
    
        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <http://www.gnu.org/licenses/>.
    
    Also add information on how to contact you by electronic and paper mail.
    
      If the program does terminal interaction, make it output a short
    notice like this when it starts in an interactive mode:
    
        <program>  Copyright (C) <year>  <name of author>
        This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
        This is free software, and you are welcome to redistribute it
        under certain conditions; type `show c' for details.
    
    The hypothetical commands `show w' and `show c' should show the appropriate
    parts of the General Public License.  Of course, your program's commands
    might be different; for a GUI interface, you would use an "about box".
    
      You should also get your employer (if you work as a programmer) or school,
    if any, to sign a "copyright disclaimer" for the program, if necessary.
    For more information on this, and how to apply and follow the GNU GPL, see
    <http://www.gnu.org/licenses/>.

      The GNU General Public License does not permit incorporating your program
    into proprietary programs.  If your program is a subroutine library, you
    may consider it more useful to permit linking proprietary applications with
    the library.  If this is what you want to do, use the GNU Lesser General
    Public License instead of this License.  But first, please read
    <http://www.gnu.org/philosophy/why-not-lgpl.html>.
```

# ChangeLog


;;; BEN ;;;
fix       shelflife = archetype
- pick ONE product based on links to processes because there are multiple archetypes!!

?? looks like shelf-life is working
- added get-shelflife and edited scale-production
- added catch in the supplier capacity for importers for consumer customers (i.e. cutting out the processors)


2025-17-01 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * edited ordering to split orders across suppliers with partial capacity (already done for consumers, but now expanded to include processors... this does, however, bypass the choose-supplier

2025-15-01 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * re-instated stock consolidation. As well as summing amount, took maximum shelflife

2025-13-01 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * changed multi-get-tactic to circumvent errors in returning multiple matches from CBR.
  * means that stock summary needs to be passed between functions to allow for a 		temporary summary of unassigned stock. Some stock is allocated to an equation and other	 stock remains unallocated thereby permitting selection of another equation.
  * corrected not-intersect (used in CBR case comparator) which is not symmetrical (A not in B versus B not in A)
  * penalise CBR matches for mis-matched keys in the sense table (thereby demote matches that have extra resources and consequently promote uncatalysed eq)
  * removed current equations from the sense-world
2024-10-15 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * only change-habit at setup if there are no existing processes linked (i.e., if the equations was set to 'unknown' in the input)
2024-10-15 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * changed depletion to use in-input-neighbors rather than theoretical amounts given that there is an archetype for each product in each equation (i.e., more than one units)
* edited outputs for behavior space to include sum(budgets) and sum(amount|product-type)
2024-10-15 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * re-instated deplete-stock to use up inputs (absent before so they just grew)
  * reviewed make-goods
  ? unclear that processors need to order enough to satisfy ALL orders, not just one 
by one, i.e., cannot double-count existing stock that has been allocated already
2024-10-15 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * changed deliveries to hatch single product of full quantity
2024-10-11 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * edited make-orders to only look for suppliers with non-zero capacity
2024-10-01 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * revised the reading of shapefiles to populate the regional-status of each polygon
  * revised the yield constraints to look at regional limits
  * round yield when randomly sampled
2024-10-01 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * edited consume to better record what is eaten in partial meals
  * supplying self needs a contract link, but this now joins to the region (supply) and then these contracts are corrected to self with in fulfil-order
  * change costs so that suppliers get unit-cost + subsity, but sellers only get -unit-cost
2024-10-01 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added contracts to ontology and refactored to replaced orders table with links describing transactions
2024-26-09 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * changed restockON-rescaleOFF so that catalysts are ordered before they expire (accounting for delivery time and production from another process)
2024-13-09 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * moved all payments into "supply" and changed default subsidy to 0 (was 1, but because it is a additive not multiplicative this inflated costs)
2024-13-09 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * revision of CBR code, memories, sense of the world etc
  * changed dormancy to work on an annual basis, i.e. number of dormant steps in one year cycle
2024-13-09 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added hard-coded CBR numbers (key-match and value-match to UI)
  * split customer orders across all suppliers (if excess is ordered)
  * change "ordered" table to include amounts (list amount good supplier)
  * editing ordering of catalysts either (a) replace expiring stock or (b) re-set the scaling
  * changed clear-books so that it can remove just one process (e.g., for tactical choice) rather than all of them (e.g., for set up or strategical choice)
  * changed the memory & case-bases to include: 
  [catalysts][running budget][capacity][demand]
The problem is that just using catalysts means the best match is (eventually) an uncatalysed equation
2024-06-09 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * tidied up the order-status to record when things are ordered
  * ensure orders are scaled correctly
  * edited make-goods capacity statements to scale production to order or capacity
  * changed the can-run? function to use processors and report whether any/no functions run. Call in GO to determine whether to depreciate orders 
  * moved scheduling of updating orders to after first delivery of goods so that new stock is included - otherwise there is a built in delay and nobody consumes because orders are a time-step behind
  * edit out-save to record equations even if not run & added consumption to out-save
2024-03-09 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * edit capacity to calculate capacity per process or product & use maximum capacity.       Add check if there are or not catalysts
  * add function to estimate standard output quantity from processes
  * add switch on whether or not to ceiling the catalysts (whole catalysts or exact output)
  * add switch to produce the order, or to produce the maximum capacity
  * add switches for ignore habit changes
  * ??? check aggregation and deliveries
  * remove check on size of order versus capacity (it had means that places that have insufficient catalyst didn't order more catalysts!)
  * order inputs up to current capacity, order catalysts to meet current order (i.e., expand future catalyst capacity)
  * change ordering of goods (inputs and catalysts) if there are no catalysts to just supply what was ordered

2024-16-04 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * circumvented CBR code to make and compare case bases using tables

2024-27-03 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * overhaul of equation selection to pick equations based on current stock, but allocate
    stock to each equation to estimate capacity and then repeat until stock exhausted
  * capacity is now a table that is used to estimate the multiplier for an equation
    and used to check what can be run (table:keys)
  * CBR seems to get stuck on choices and won't return new options

2024-19-03 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * use 'running' table to record income from each processing being run

2024-11-03 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * make case bases local for each processor and append all processes to them each tick

2024-07-03 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added case-base to each processor using a table of equations run, the count of times      and a running average income

2024-29-02 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * export video if switch on interface 'on'

2024-02-01 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added n-choices to GUI to select 1 to n-choices equations when picking new equations
  * select random matches from cbr if >1

2024-02-01 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * changed read-input so that the first equation is chosen using all-random
  * changed long-budget to start at abs(2* max-change) so that it is positive (not 2*max-change) and therefore fewer strategies are changed
  * moved tactics to the procedures library

2023-11-21 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * changed consumer timeseries input and call in 'go' to updated consumer demand when nothing it demanded
  * edited implement creation to catch both processes and metaprocesses
  * changed where the things-ordered records orders (was in make-orders, now in order-*)
  * kill off zero-amount products in the consolidation function
  * comments out the ratelimit functions from go - these set capacity to zero if catalysts have depreciated to nothing, but then they aren't replaced in make-orders

2023-11-20 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added functions to modify the amount ordered or produced based on
  meta-processes

2023-11-10 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added metaprocesses as new breed and modifications to link to processes
  * changed read-equations to read new meta-processes with new grammar

2023-10-06 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added regulator file input to read what to subsidise
  * changed next-process to differentiate tactics and strategy options

2023-09-15 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added CBR back to script as an alternative to habit-random (see choose-process)
  * added 'biggest' to chose-supplier (pick largest capacity)

2023-09-12 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added investment when profits go up and edited ratelimit to change capacity (multiply     equation)
  * added stop if all processors are closed (i.e., maximum changes exhausted and ceased       trading) 

2023-09-04 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added "regulator" breed and "control" link for agent to subsidies or penalise
    the production of specified goods
  * pay suppliers (at the point of ordering). NOTE: paying for delivery is separate (at       point of fulfilling orders)

2023-09-01 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added switch to reset budget each tick

2023-08-18 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added output *.csv option with data on equations | products used | productus made
  * changed recording of when products are made so that the count-down only works on products that had requisit inputs present
  * updated the depletion which was inactive

2023-05-21 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * import additional file with distritbution data for equation outputs ("yields")
  * make "yields" with distribution information and link to processes with "uncertainty"
  * use one or more yields to generate an empirical cumulative density function (ecdf)
    using n-samples observations from each of the yields, then make a random draw to get 	    a variable output for each equation

2023-05-04 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * switch for local importers, either supplying everyone or their local processor
  * global importer switch, either all products or those not made by equations
  * stop consumers from ordering if matched to closed processor (i.e. same place)
  * tables of things-* cummulative over the model ticks
  * add random capacity to each processor from GUI
  * consumer demand soft coded in GUI
  * edited consume to move unfulfilled-demand to consumption (unfulfilled-demand was   never reduced otherwise)
  * changed choose-supplier to allow for nobody - processor go dormant, consumers stop

2023-04-12 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * split functions into nls files
  * import node & edge list from CSV for world builder

2023-03-13 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * kill processors that change too often
  * added missing links to global importer - put this as zero cost
  * added condition that suppliers have capacity, otherwise suppliers-of rolls over to 	      global importer
  * added things-imported tally of imports

2023-03-13 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added hook for picking processes that are under-served

2023-03-08 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * local importers at patch access points
  * consumer per household (processor)
  * added product shelf-life, but also made archetypes importal
  * table the things consumed (from unfulfilled orders)

2023-02-26 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * fixed the growing 'running' list (now a table)
  * added 'change-habit' to change equation (process) to random selection 
    if the agent has a negative balance for > threshold ticks (set in UI)

2023-02-26 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added names to processors, consumers & importers
  * add cost to trade links
  * charge for delivery in fulfill-orders by looking up trade link

2023-02-21 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * consumers demand based on a time series imported

2023-02-21 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * changed inputs to allow consumer time series
  * edited get-stock for empty stock
  * changed graphs for individual St Kilda products

2023-01-24 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * use global case-base to populate initial equation(s) for processors based on 
    resources read in 

2023-01-23 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * make global case-base

2022-12-7 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * read product attributes & calculate cost/income from each equation

2022-12-2 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * read in farm attributes more sensibly

2022-10-31 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * new ontology (not separating inputs/catalysts/outputs by breed)
  * added consolidation function ** needs review **
  * updated rate limiting calculation ** needs error catches **
  * changed loading of transporters to count agents not amount
  * added ordering for catalysts (order-catalyst)


2022-10-03 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * new gobal tables & plots of products ordered, made & delivered

2022-09-26 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * determine rate limiting resources to scale equations

2022-09-14 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added capacity as scalar to processors
  * added multiplier to order-inputs *doesn't work!*

2022-08-23 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * edited processor import to include resources in a table. 

2022-07-29 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * changed product-types to a table that also records ingredients
  * added checks to try to limit re-ordering of inputs when making orders

2022-07-25 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * added options for transporter & supplier selection

2022-07-25 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * edited so that only one equation of a given type can run at a time (otherwise a 
    process would start anew for every timestep meeting any given conditions)

2022-07-19 Ben McCormick <benjamin.mccormick@abdn.ac.uk>
  * edited consumer amounts

2022-07-06  Ben McCormick <benjamin.mccormick@abdn.ac.uk>

  * Added time extension & month criteria
  * Added GIS import - coordinates are either point (x,y) or name (region) linked to    shapefile
  * Added profiler

2022-06-29 Gary Polhill <gary.polhill@hutton.ac.uk>

  * Added 'hook' procedures to allow different decision-making algorithms to be
    used by the various agent types
  * Removed 'very-simple-setup' procedure

2022-06-01 Ben McCormick <benjamin.mccormick@abdn.ac.uk>

  * Added processing time to equations

2022-05-30 Ben McCormick <benjamin.mccormick@abdn.ac.uk>

  * Added procedures to read in equations and agents

2020-09-04 Gary Polhill <gary.polhill@hutton.ac.uk>

  * v0 released to Erika Palmer (in email)
  * Added comments to code, licence and documentation
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

boat
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6459832 true false 150 32 157 162
Polygon -13345367 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7500403 true true 158 33 230 157 182 150 169 151 157 156
Polygon -7500403 true true 149 55 88 143 103 139 111 136 117 139 126 145 130 147 139 147 146 146 149 55

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

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="test_1" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>table:get things-run "eq1"</metric>
    <metric>count products with [product-type = "A"]</metric>
    <metric>count products with [product-type = "B"]</metric>
    <metric>count products with [product-type = "C"]</metric>
    <metric>count products with [product-type = "a"]</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;days&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;testing/test_1_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;testing/test_1_importers.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;testing/test_1_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;testing/test_1_consumers.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;testing/test_1_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;testing/test_1_processors.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="-2110017941"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;testing/test_1_consumerseries.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/pub_las.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test_2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count products with [product-type = "A"]</metric>
    <metric>count products with [product-type = "B"]</metric>
    <metric>count products with [product-type = "C"]</metric>
    <metric>count products with [product-type = "a"]</metric>
    <metric>count products with [product-type = "b"]</metric>
    <metric>count products with [product-type = "c"]</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;days&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;testing/test_2/test_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;testing/test_2/test_importers.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;testing/test_2/test_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;testing/test_2/test_consumers.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;testing/test_2/test_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;testing/test_2/test_processors.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="-178940065"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;testing/test_2/test_consumerseries.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/pub_las.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test_3" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count products with [product-type = "A"]</metric>
    <metric>count products with [product-type = "B"]</metric>
    <metric>count products with [product-type = "C"]</metric>
    <metric>count products with [product-type = "D"]</metric>
    <metric>count products with [product-type = "E"]</metric>
    <metric>count products with [product-type = "a"]</metric>
    <metric>count products with [product-type = "b"]</metric>
    <metric>count products with [product-type = "c"]</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;days&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;testing/test_3/test_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;testing/test_3/test_importers.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;testing/test_3/test_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;testing/test_3/test_consumers.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;testing/test_3/test_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;testing/test_3/test_processors.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="-886572962"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;testing/test_3/test_consumerseries.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/pub_las.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test_5" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count products with [product-type = "A"]</metric>
    <metric>count products with [product-type = "B"]</metric>
    <metric>count products with [product-type = "C"]</metric>
    <metric>count products with [product-type = "D"]</metric>
    <metric>count products with [product-type = "E"]</metric>
    <metric>count products with [product-type = "a"]</metric>
    <metric>count products with [product-type = "b"]</metric>
    <metric>count products with [product-type = "c"]</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;days&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;testing/test_5/test_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;testing/test_5/test_importers.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;testing/test_5/test_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;testing/test_5/test_consumers.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;testing/test_5/test_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;testing/test_5/test_processors.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="576519450"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;testing/test_5/test_consumerseries.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/pub_las.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="medium-scale" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>output-save</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_n5_v3.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1523194191"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/pub_las.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>output-save</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_NE.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="368606153"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2024&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NE_stravvs_small" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>output-save</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr-local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations_2.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_NE_small.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1710112393"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2024&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NE_stravvs" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>output-save</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations_2.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_NE.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="-207094674"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2024&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NE_stravvs_small_2" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>output-save</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr-local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations_3.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_NE_small_2.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1710112393"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2024&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NE_stravvs_5k" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>output-save</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr-local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations_3.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_NE_5k.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1710112393"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2024&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NE_stravvs_5k" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>output-save</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr-local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations_3.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_NE_5k.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1710112393"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2024&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NE_stravvs_10pc" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>output-save</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr-local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;inits_scotland.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations_3.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_10pc.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="1710112393"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2024&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v1-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v1/v1_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;v1/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;v1/processors.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;v1/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;v1/v1_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;v1/v1_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;v1/v1_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v2-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v2/v2_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;v2/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;v2/processors.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;v2/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;v2/v2_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;v2/v2_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;v2/v2_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v3-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v3/v3_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;v3/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;v3/processors.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;v3/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;v3/v3_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;v3/v3_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;v3/v3_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v4-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v4/v4_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;v4/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;v4/processors.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;v4/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;v4/v4_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;v4/v4_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;v4/v4_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v5-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v5/v5_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;v5/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;v5/processors.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;v5/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;v5/v5_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;v5/v5_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;v5/v5_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v6-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v6/v6_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;v6/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;v6/processors.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;v6/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;v6/v6_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;v6/v6_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;v6/v6_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v7-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v7/v7_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;v7/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;v7/processors.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;v7/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;v7/v7_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;v7/v7_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;v7/v7_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v8-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v8/v8_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;v8/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;v8/processors.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;v8/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;v8/v8_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;v8/v8_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;v8/v8_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v9-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v9/v9_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;v9/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;v9/processors.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;v9/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;v9/v9_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;v9/v9_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;v9/v9_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v10-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v10/v10_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;v10/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;v10/processors.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;v10/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;v10/v10_products.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;v10/v10_equations.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;v10/v10_yields.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v11-test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;v11_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumer_short.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_10pc_short.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products_short.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations_short.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields_short.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;spread&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2023&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="interruption" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="budget-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations_simplified3.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;scotland/scotland_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multi-match?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_10pc_simplified3.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products_simplified3T.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2025&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;spread&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields_simplified3.txt&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="interruption short" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="budget-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseries.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations_short.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;scotland/scotland_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multi-match?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_10pc_simplified3.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products_simplified3T.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="431810180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2025&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;spread&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields_short.txt&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="interruption 2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>output-save</metric>
    <enumeratedValueSet variable="supplier-choice">
      <value value="&quot;spread&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-file">
      <value value="&quot;edges_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-regulator-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tactical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exponent">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="video?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategy-compare">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-unit">
      <value value="&quot;months&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="verbose?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-histogram?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategical-choice">
      <value value="&quot;cbr&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-importer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-subsidy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-budget">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-yield?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-file">
      <value value="&quot;scotland/scotland_inits.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restockON-rescaleOFF">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reset-budget?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-capacity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-demand">
      <value value="&quot;beef.meat.kg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="river-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-file">
      <value value="&quot;scotland/FMH_yields_simplified3.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-non-file-transporters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="import-anything?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beach-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="first-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-local-importers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-num">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-capacity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regulator-file">
      <value value="&quot;scotland/regulators.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-series?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-choices">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-index?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="value-match">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="additional-trade-prob">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-dir">
      <value value="&quot;v3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="round-catalysts-orders?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-file">
      <value value="&quot;NA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-tactics?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multi-match?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nesting-cliff-num">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importer-file">
      <value value="&quot;scotland/importers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-transporter-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world">
      <value value="&quot;scotland&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-region-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="product-attributes">
      <value value="&quot;scotland/FMH_products_simplified3T.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="re-invest?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="input-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-consumer-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-file">
      <value value="&quot;scotland/consumers.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="charge-transport?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="equation-file">
      <value value="&quot;scotland/FMH_equations_simplified3.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="review-strategy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-change">
      <value value="-50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-changes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-layout">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="budget-strategy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transporter-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="processor-file">
      <value value="&quot;scotland/simfarms_10pc_simplified3.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-dormant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="instant-transport?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-file">
      <value value="&quot;nodes_v1.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fertile-land-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-processor-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-seed">
      <value value="-177264005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-type">
      <value value="&quot;Grid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="node-type">
      <value value="&quot;Side&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="local-resources?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="key-match">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consumer-timeseries-file">
      <value value="&quot;scotland/consumer_timeseriesT.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="region-file">
      <value value="&quot;scotland/ERSA_regions.shp&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-date">
      <value value="&quot;01-01-2025&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="testing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="next-delivery-choice">
      <value value="&quot;all-random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-precision">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grazing-land-num">
      <value value="3"/>
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
