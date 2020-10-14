(ns propeller.core
  #?(:clj (:gen-class))
  (:require [propeller.gp :as gp]
            [propeller.args :as args]
            [propeller.problems.simple-regression :as regression]
            [propeller.problems.string-classification :as string-classif]
            [propeller.problems.software.number-io :as number-io]
            [propeller.problems.software.smallest :as smallest]
            #?(:cljs [cljs.reader :refer [read-string]])))

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (gp/gp
    (update-in
      (merge
        args/default-argmap
        (apply hash-map
               (map read-string args)))
      [:error-function]
      identity)))
