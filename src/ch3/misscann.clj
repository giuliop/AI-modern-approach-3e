(ns ch3.misscann)

(def start-state {:left  {:mis 3 :can 3}
                  :right {:mis 0 :can 0}
                  :boat :left})

(defn goal? [s]
  (and (= 3 (:mis (:right s)) (:can (:right s))) (= :right (:boat s))))

(def moves (for [m [0 1 2] c [0 1 2] :when (<= 1 (+ m c) 2)] {:mis m :can c}))

(defn other-side [side]
  (if (= :left side) :right :left))

(defn actions [s]
  (let [side (:boat s)
        enough-guys? (fn [move] (and (>= (:mis (side s)) (:mis move))
                                     (>= (:can (side s)) (:can move))))]
    (filter enough-guys? moves)))

(defn move->state [s move]
  (let [side (:boat s)
        other-side (other-side side)]
    {side (merge-with - (side s) move)
     other-side (merge-with + (other-side s) move)
     :boat other-side}))

(defn fail? [s]
  (some identity (for [side [:left :right]] (< 0 (:mis (side s)) (:can (side s))))))

(defn states-from [s]
  (remove fail? (map #(move->state s %) (actions s))))

(defn make-entry [path s]
  (conj path s))

; note that we don't test for goal when we generate the nodes but only when we
; select them; it works here but it's less efficient for breadth-first searches in
; large state spaces
(defn breadth-first []
  (loop [parent nil
         frontier (conj clojure.lang.PersistentQueue/EMPTY [start-state])]
    (let [path (peek frontier)
          s (peek path)]
      (if (goal? s) path
        (let [frontier (->> (states-from s)
                            (remove #(= parent %))
                            (map #(make-entry path %))
                            (reduce conj (pop frontier)))]
          (recur s frontier))))))

(def join clojure.string/join)
(defn print-state [s]
  (let [m "M " c "C " b "🚣 " pad "      " divider "  |  "
        [left right] (for [side [:left :right]]
                           (str (subs (str (join (repeat (:mis (side s)) m)) pad) 0 6)
                                (subs (str (join (repeat (:can (side s)) c)) pad) 0 6)
                                (if (= side (:boat s)) b "  ")))]
    (println " " left divider right)
    (newline)))

(defn print-sol [sol]
  (newline)
  (run! print-state sol)
  (println " Moves: " (dec (count sol)))
  (newline))

