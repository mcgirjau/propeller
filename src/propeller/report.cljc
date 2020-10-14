(ns propeller.report
  (:require [propeller.genome :as genome]))

(defn report-generational
  [pop generation print-best-program]
  (let [best (first pop)
        frame-length (count (str " | Best total error:" (:total-error best) "| "))]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (println)
    (when print-best-program
      (print "Best plushy: ") (prn (:plushy best))
      (print "Best program: ") (prn (genome/plushy->push (:plushy best))))
    (println (apply str (repeat frame-length "=")))
    (println "| BEST TOTAL ERROR:" (:total-error best) "|")
    (println (apply str (repeat frame-length "=")) "\n")
    (println "Best errors:" (:errors best) "\n")
    (println "Best behaviors:" (:behaviors best) "\n")
    (println "Genotypic diversity:"
             (float (/ (count (distinct (map :plushy pop))) (count pop))) "\n")
    (println "Average genome length:"
             (float (/ (reduce + (map count (map :plushy pop))) (count pop))) "\n")
    (println)))

(defn report-steady-state
  [iteration population population-size print-best-program]
  (let [best (first population)]
    (println "-------------------------------------------------------")
    (println "               Report for Iteration" iteration)
    (println "-------------------------------------------------------")
    (println)
    (when print-best-program
      (print "Best plushy: ") (prn (:plushy best))
      (print "Best program: ") (prn (genome/plushy->push (:plushy best))))
    (println "Best total error:" (:total-error best) "\n")
    (println "Best errors:" (:errors best) "\n")
    (println "Best behaviors:" (:behaviors best) "\n")
    (println "Current population size:" population-size "\n")
    (println "Genotypic diversity:"
             (float (/ (count (distinct (map :plushy population)))
                       population-size)) "\n")
    (println "Average genome length:"
             (float (/ (reduce + (map count (map :plushy population)))
                       population-size)) "\n")
    (println)))
