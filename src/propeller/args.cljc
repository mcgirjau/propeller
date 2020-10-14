(ns propeller.args
  (:require [propeller.problems.software.median :as median]
            [propeller.problems.software.number-io :as number-io]))

(def default-argmap
  {:instructions            median/instructions
   :error-function          median/error-function
   :max-generations         200
   :population-size         500
   :max-initial-plushy-size 100
   :step-limit              200
   :print-best-program      false
   :gp-type                 :generational
   :parent-selection        :lexicase
   :tournament-size         5
   :umad-rate               0.1
   :variation               {:umad 0.5 :crossover 0.5}
   :elitism                 false
   :prop-children           0.2})