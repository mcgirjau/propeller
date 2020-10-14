(ns propeller.problems.software.median
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.utils.helpers :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.push.state :as state]
            #?(:cljs [cljs.reader :refer [read-string]])))

;; =============================================================================
;; MEDIAN PROBLEM
;;
;; This problem file defines the following problem:
;;     Given 3 integers, print their median.
;;
;; Problem Source:
;;     C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for
;;     Automated Repair of C Programs," in IEEE Transactions on Software
;;     Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;     doi: 10.1109/TSE.2015.2454513
;;
;; NOTES:
;;     input stack: in1 (float),
;;                  in2 (int)
;;     output stack: printed output
;; =============================================================================

;; Random integer between -100 and 100
(defn random-int [] (- (rand-int 201) 100))

(def instructions
  (utils/not-lazy
    (concat
      ;; stack-specific instructions
      (get-stack-instructions #{:boolean :exec :integer :print})
      ;; input instructions
      (list :in1 :in2 :in3)
      ;; ERCs
      (list random-int))))

(defn correct-function
  [[x y z]]
  (cond
    (or (<= x y z) (>= x y z)) y
    (or (<= x z y) (>= x z y)) z
    (or (<= z x y) (>= z x y)) x))

(def train-and-test-data
  (let [inputs (vec (repeatedly 1100 #(vector (random-int) (random-int) (random-int))))
        outputs (mapv correct-function inputs)
        train-set {:inputs  (take 100 inputs)
                   :outputs (take 100 outputs)}
        test-set {:inputs  (drop 100 inputs)
                  :outputs (drop 100 outputs)}]
    {:train train-set
     :test  test-set}))

(defn error-function
  ([argmap individual]
   (error-function argmap individual :train))
  ([argmap individual subset]
   (let [program (genome/plushy->push (:plushy individual))
         data (get train-and-test-data subset)
         inputs (:inputs data)
         correct-outputs (:outputs data)
         outputs (map (fn [input]
                        (state/peek-stack
                          (interpreter/interpret-program
                            program
                            (assoc state/empty-state :input {:in1 (first input)
                                                             :in2 (second input)
                                                             :in3 (last input)}
                                                     :output '(""))
                            (:step-limit argmap))
                          :output))
                      inputs)
         parsed-outputs (map (fn [output]
                               (try (read-string output)
                                    #?(:clj (catch Exception e nil)
                                       :cljs (catch js/Error. e nil))))
                             outputs)
         errors (map (fn [correct-output output]
                       (if (= correct-output output) 0 1))
                     correct-outputs
                     parsed-outputs)]
     (assoc individual
       :behaviors parsed-outputs
       :errors errors
       :total-error (apply + errors)))))
