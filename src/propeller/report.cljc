(ns propeller.report
  (:require [propeller.genome :as genome]))

(defn report-generational
  [pop generation print-best-program]
  (let [best (first pop)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (println)
    (when print-best-program
      (print "Best plushy: ") (prn (:plushy best))
      (print "Best program: ") (prn (genome/plushy->push (:plushy best))))
    (println "Best total error:" (:total-error best) "\n")
    (println "Best errors:" (:errors best) "\n")
    (println "Best behaviors:" (:behaviors best) "\n")
    (println "Genotypic diversity:"
             (float (/ (count (distinct (map :plushy pop))) (count pop))) "\n")
    (println "Average genome length:"
             (float (/ (reduce + (map count (map :plushy pop))) (count pop))) "\n")
    (println)))

(defn report-steady-state
  [pop print-best-program]
  (let [best (first pop)]
    (when print-best-program
      (print "Best plushy: ") (prn (:plushy best))
      (print "Best program: ") (prn (genome/plushy->push (:plushy best))))
    (println "Best total error:" (:total-error best) "\n")
    (println "Best errors:" (:errors best) "\n")
    (println "Best behaviors:" (:behaviors best) "\n")
    (println "Genotypic diversity:"
             (float (/ (count (distinct (map :plushy pop))) (count pop))) "\n")
    (println "Average genome length:"
             (float (/ (reduce + (map count (map :plushy pop))) (count pop))) "\n")
    (println "MAKING CHILDREN...")
    (println)))
