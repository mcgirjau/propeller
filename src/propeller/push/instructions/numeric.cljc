(ns propeller.push.instructions.numeric
  #?(:cljs (:require-macros
             [propeller.push.utils.macros :refer [def-instruction
                                                  generate-instructions]]))
  (:require [propeller.push.utils.helpers :refer [make-instruction]]
            [propeller.tools.math :as math]
            #?(:cljs [cljs.reader :refer [read-string]]
               :clj [propeller.push.utils.macros
                     :refer [def-instruction generate-instructions]])))

;; =============================================================================
;; FLOAT and INTEGER Instructions (polymorphic)
;; =============================================================================

;; Pushes TRUE onto the BOOLEAN stack if the second item is greater than the top
;; item, and FALSE otherwise
(def _gt
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state > [stack stack] :boolean)))

;; Pushes TRUE onto the BOOLEAN stack if the second item is greater than or
;; equal to the top item, and FALSE otherwise
(def _gte
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state >= [stack stack] :boolean)))

;; Pushes TRUE onto the BOOLEAN stack if the second item is less than the top
;; item, and FALSE otherwise
(def _lt
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state < [stack stack] :boolean)))

;; Pushes TRUE onto the BOOLEAN stack if the second item is less than or equal
;; to the top item, and FALSE otherwise
(def _lte
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state <= [stack stack] :boolean)))

;; Pushes the sum of the top two items onto the same stack
(def _add
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state +' [stack stack] stack)))

;; Pushes the difference of the top two items (i.e. the second item minus the
;; top item) onto the same stack
(def _subtract
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state -' [stack stack] stack)))

;; Pushes the product of the top two items onto the same stack
(def _mult
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state *' [stack stack] stack)))

;; Pushes the result of dividing the top two items (i.e. the second item divided
;; by the top item) onto the same stack. If the top item is zero, pushes 1
(def _div
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state
                      #(cond
                         (zero? %2) 1
                         (= :integer stack) (quot %1 %2)
                         (= :float stack) (/ %1 %2))
                      [stack stack]
                      stack)))

;; Pushes the second item modulo the top item onto the same stack. If the top
;; item is zero, pushes 1. The modulus is computed as the remainder of the
;; quotient, where the quotient has first been truncated towards negative
;; infinity.
(def _mod
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state #(if (zero? %2) 1 (mod %1 %2)) [stack stack] stack)))

;; Pushes the maximum of the top two items
(def _max
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state max [stack stack] stack)))

;; Pushes the minimum of the top two items
(def _min
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state min [stack stack] stack)))

;; Pushes 1 / 1.0 if the top BOOLEAN is TRUE, or 0 / 0.0 if FALSE
(def _from_boolean
  ^{:stacks #{:boolean}}
  (fn [stack state]
    (make-instruction state
                      #((if (= stack :integer) int float) (if % 1 0))
                      [:boolean]
                      stack)))

;; Pushes the ASCII value of the top CHAR
(def _from_char
  ^{:stacks #{:char}}
  (fn [stack state]
    (make-instruction state (if (= stack :integer) int float) [:char] stack)))

;; Pushes the value of the top STRING, if it can be parsed as a number.
;; Otherwise, acts as a NOOP
(def _from_string
  ^{:stacks #{:string}}
  (fn [stack state]
    (make-instruction state
                      #(try ((if (= stack :integer) int float) (read-string %))
                            #?(:clj (catch Exception e)
                               :cljs (catch js/Error. e)))
                      [:string]
                      stack)))

;; Pushes the increment (i.e. +1) of the top item of the stack
(def _inc
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state inc' [stack] stack)))

;; Pushes the decrement (i.e. -1) of the top item of the stack
(def _dec
  ^{:stacks #{}}
  (fn [stack state]
    (make-instruction state dec' [stack] stack)))

;; 2 types x 16 functions = 32 instructions
(generate-instructions
  [:float :integer]
  [_gt _gte _lt _lte _add _subtract _mult _div _mod _max _min _inc _dec
   _from_boolean _from_char _from_string])

;; =============================================================================
;; FLOAT Instructions only
;; =============================================================================

;; Pushes the cosine of the top FLOAT
(def-instruction
  :float_cos
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state math/cos [:float] :float)))

;; Pushes the sine of the top FLOAT
(def-instruction
  :float_sin
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state math/sin [:float] :float)))

;; Pushes the tangent of the top FLOAT
(def-instruction
  :float_tan
  ^{:stacks #{:float}}
  (fn [state]
    (make-instruction state math/tan [:float] :float)))

;; Pushes the floating point version of the top INTEGER
(def-instruction
  :float_from_integer
  ^{:stacks #{:float :integer}}
  (fn [state]
    (make-instruction state float [:integer] :float)))

;; =============================================================================
;; INTEGER Instructions only
;; =============================================================================

;; Pushes the result of truncating the top FLOAT towards negative infinity
(def-instruction
  :integer_from_float
  ^{:stacks #{:float :integer}}
  (fn [state]
    (make-instruction state int [:float] :integer)))
