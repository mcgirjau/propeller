(ns propeller.push.utils.helpers
  (:require [clojure.set]
            [propeller.push.utils.globals :as globals]
            [propeller.push.core :as push]
            [propeller.push.state :as state]
            #?(:cljs [goog.string :as gstring])
            #?(:cljs [goog.string.format])))

;; Returns a version of the number n that is within reasonable size bounds
(defn keep-number-reasonable
  [n]
  (cond
    (integer? n)
    (cond
      (> n globals/max-number-magnitude) (int globals/max-number-magnitude)
      (< n (- globals/max-number-magnitude)) (int (- globals/max-number-magnitude))
      :else n)
    :else
    (cond
      (Double/isNaN n) 0.0
      (or (= n Double/POSITIVE_INFINITY)
          (> n globals/max-number-magnitude)) globals/max-number-magnitude
      (or (= n Double/NEGATIVE_INFINITY)
          (< n (- globals/max-number-magnitude))) (- globals/max-number-magnitude)
      (< (- globals/min-number-magnitude) n globals/min-number-magnitude) 0.0
      :else n)))

;; Returns true if the string is of a reasonable size
(defn reasonable-string-length?
  [string]
  (let [length (count string)]
    (<= length globals/max-string-length)))

;; Returns true if the vector is of a reasonable size
(defn reasonable-vector-length?
  [vector]
  (let [length (count vector)]
    (<= length globals/max-vector-length)))

;; Takes a state and a collection of stacks to take args from. If there are
;; enough args on each of the desired stacks, returns a map with keys
;; {:state :args}, where :state is the new state and :args is a list of args
;; popped from the stacks. If there aren't enough args on the stacks, returns
;; :not-enough-args without popping anything
(defn get-args-from-stacks
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [current-stack (first stacks)]
        (if (state/empty-stack? state current-stack)
          :not-enough-args
          (recur (state/pop-stack state current-stack)
                 (rest stacks)
                 (conj args (state/peek-stack state current-stack))))))))

;; A utility function for making Push instructions. Takes a state, a function
;; to apply to the args, the stacks to take the args from, and the stack to
;; return the result to. Applies the function to the args (popped from the
;; given stacks), and pushes the result onto the return-stack
(defn make-instruction
  [state function arg-stacks return-stack]
  (let [popped-args (get-args-from-stacks state arg-stacks)]
    (if (= popped-args :not-enough-args)
      state
      (let [result (apply function (:args popped-args))
            new-state (:state popped-args)]
        (cond
          (number? result)
          (state/push-to-stack new-state return-stack (keep-number-reasonable result))
          ;;
          (string? result)
          (if (reasonable-string-length? result)
            (state/push-to-stack new-state return-stack result)
            state)
          ;;
          (vector? result)
          (if (reasonable-vector-length? result)
            (state/push-to-stack new-state return-stack result)
            state)
          ;;
          :else
          (state/push-to-stack new-state return-stack result))))))

;; Given a set of stacks, returns all instructions that operate on those stacks
;; only. Won't include random instructions unless :random is in the set as well
(defn get-stack-instructions
  [stacks]
  (doseq [[instruction-name function] @push/instruction-table]
    (assert
      (:stacks (meta function))
      #?(:clj (format
                "ERROR: Instruction %s does not have :stacks defined in metadata."
                (name instruction-name))
         :cljs (gstring/format
                 "ERROR: Instruction %s does not have :stacks defined in metadata."
                 (name instruction-name)))))
  (for [[instruction-name function] @push/instruction-table
        :when (clojure.set/subset? (:stacks (meta function)) stacks)]
    instruction-name))

;; If a piece of data is a literal, return its corresponding stack name, e.g.
;; :integer. Otherwise, return nil"
(defn get-literal-type
  [data]
  (let [literals {:boolean        (fn [thing] (or (true? thing) (false? thing)))
                  :char           char?
                  :float          float?
                  :integer        integer?
                  :string         string?
                  :vector_boolean (fn [thing] (and (vector? thing)
                                                   (or (true? (first thing))
                                                       (false? (first thing)))))
                  :vector_float   (fn [thing] (and (vector? thing)
                                                   (float? (first thing))))
                  :vector_integer (fn [thing] (and (vector? thing)
                                                   (integer? (first thing))))
                  :vector_string  (fn [thing] (and (vector? thing)
                                                   (string? (first thing))))
                  :generic-vector (fn [thing] (= [] thing))}]
    (first (for [[stack function] literals
                 :when (function data)]
             stack))))

(defn get-vector-literal-type
  "Returns the literal stack corresponding to some vector stack."
  [vector-stack]
  (get state/vec-stacks vector-stack))

;; Pretty-print a Push state, for logging or debugging purposes
(defn print-state
  [state]
  (doseq [stack (keys state/empty-state)]
    #?(:clj (printf "%-15s = " stack)
       :cljs (print (gstring/format "%-15s = " stack)))
    (prn (if (get state stack) (get state stack) '()))
    (flush)))
