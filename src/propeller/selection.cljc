(ns propeller.selection)

(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors (map rand-nth (vals (group-by :errors pop)))
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)))

(defn select-survivors
  "Selects survivors for a steady-state model."
  [population population-size max-prop-to-remove]
  (let [unique-ids (range population-size)
        population (map #(assoc %1 :id %2) population unique-ids)
        total-errors (map :total-error population)
        scaled-slices (reductions + total-errors)
        total (last scaled-slices)
        population (map #(assoc %1 :slice %2) population scaled-slices)
        max-individuals-to-remove (* max-prop-to-remove population-size)
        ids-to-remove (set
                        (for [i (range max-individuals-to-remove)
                              :let [index (rand total)]]
                          (->> population
                               (drop-while #(<= (:slice %) index))
                               (first)
                               (:id))))]
    (println "Killing" (count ids-to-remove) "individuals...")
    (remove #(get ids-to-remove (:id %)) population)))
