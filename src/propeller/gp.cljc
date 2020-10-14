(ns propeller.gp
  (:require [clojure.string]
            [propeller.genome :as genome]
            [propeller.report :as report]
            [propeller.selection :as selection]
            [propeller.variation :as variation]
            [propeller.push.instructions.bool]
            [propeller.push.instructions.character]
            [propeller.push.instructions.code]
            [propeller.push.instructions.input-output]
            [propeller.push.instructions.numeric]
            [propeller.push.instructions.polymorphic]
            [propeller.push.instructions.string]
            [propeller.push.instructions.vector]))

(defn gp-generational
  "Main generational GP loop."
  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size print-best-program]
    :as   argmap}]
  ;;
  (println "Starting generational GP with args: " argmap)
  ;;
  (loop [generation 0
         population (repeatedly
                      population-size
                      #(hash-map :plushy (genome/make-random-plushy
                                           instructions
                                           max-initial-plushy-size)))]
    (let [evaluated-population (sort-by :total-error
                                        (#?(:clj  pmap
                                            :cljs map)
                                          (partial error-function argmap) population))
          best-individual (first evaluated-population)]
      (report/report-generational evaluated-population generation print-best-program)
      (cond
        ;; Success on training cases is verified on testing cases
        (zero? (:total-error best-individual))
        (do (println "SUCCESS at generation" generation)
            (print "Checking program on test cases... ")
            (if (zero? (:total-error (error-function argmap best-individual :test)))
              (println "Test cases passed.")
              (println "Test cases failed."))
            (#?(:clj shutdown-agents)))
        ;;
        (>= generation max-generations)
        nil
        ;;
        :else (recur (inc generation)
                     (if (:elitism argmap)
                       (conj (repeatedly
                               (dec population-size)
                               #(variation/new-individual evaluated-population argmap))
                             (first evaluated-population))
                       (repeatedly
                         population-size
                         #(variation/new-individual evaluated-population argmap))))))))

(defn gp-steady-state
  "Main steady-state GP loop."
  [{:keys [population-size error-function instructions max-initial-plushy-size
           prop-children num-children print-best-program]
    :as   argmap}]
  ;;
  (println "Starting steady-state GP with args: " argmap)
  ;;
  (loop [population-size population-size
         population (->>
                      ;; make population
                      (repeatedly population-size
                                  #(hash-map
                                     :plushy
                                     (genome/make-random-plushy
                                       instructions
                                       max-initial-plushy-size)))
                      ;; evaluate population
                      (#?(:clj  pmap
                          :cljs map)
                        (partial error-function argmap))
                      ;; sort population by total error
                      (sort-by :total-error))
         best-individual (first population)]
    (do
      (report/report-steady-state population population-size print-best-program)
      (cond
        ;; Success on training cases is verified on testing cases
        (zero? (:total-error best-individual))
        (do (println "SUCCESS! \n\n Checking program on test cases... ")
            (if (zero? (:total-error (error-function argmap best-individual :test)))
              (println "Test cases passed.")
              (println "Test cases failed."))
            (#?(:clj shutdown-agents)))
        ;;
        :else (let [new-individuals (->>
                                      ;; make new individuals
                                      (repeatedly num-children
                                                  #(variation/new-individual
                                                     population
                                                     argmap))
                                      ;; evaluate new individuals
                                      (#?(:clj  pmap
                                          :cljs map)
                                        (partial error-function argmap)))
                    survivors (selection/select-survivors population
                                                          population-size
                                                          (- 1 prop-children))
                    new-population (sort-by :total-error
                                            (concat new-individuals survivors))
                    new-best-individual (first new-population)]
                (recur (count new-population)
                       new-population
                       new-best-individual))))))

(defn gp
  [argmap]
  (case (:gp-type argmap)
    :generational (gp-generational argmap)
    :steady-state (->>
                    ;; compute number of children to produce (generational gap)
                    (* (:population-size argmap)
                       (:prop-children argmap))
                    (int)
                    ;; add the number of children to the argmap
                    (assoc argmap :num-children)
                    ;; run steady-state GP
                    (gp-steady-state))))
