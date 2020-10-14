(ns propeller.args
  (:require [propeller.problems.software.number-io :as number-io]))

(def default-argmap
  {:instructions            number-io/instructions
   :error-function          number-io/error-function
   :max-generations         500
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