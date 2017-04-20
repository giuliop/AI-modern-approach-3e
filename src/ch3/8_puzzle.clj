(ns ch3.8-puzzle)

(defn fact [n]
    (loop [res 1, n n]
      (if (zero? n) res
        (recur (* res n) (dec n)))))

(def all-states-count
  (fact 9))

; the blank
(def blank \#)

; state keeps track of the position of the blank \# and of what numbers are in each
; position of the board (0-8)
(def state1 {0 1, 1 2, 2 3, 3 4, 4 5, 5 6, 6 7, 7 8, 8 blank, blank 8})
(def state2 {0 2, 1 1, 2 3, 3 4, 4 5, 5 6, 6 7, 7 8, 8 blank, blank 8})

; return the state resulting from moving the key in position pos to the blank
(defn swap [state pos]
  (let [n (state pos)
        blank-pos (state blank)]
    (assoc state pos blank, blank pos, blank-pos n)))

(def legal-moves {0 #{1 3}
                  1 #{0 2 4}
                  2 #{1 5}
                  3 #{0 4 6}
                  4 #{3 5 1 7}
                  5 #{2 4 8}
                  6 #{3 7}
                  7 #{6 4 8}
                  8 #{5 7}})

; return new possible states from state
(defn states-from [state]
  (let [blank-pos (state blank)
        new-blank-pos (legal-moves blank-pos)]
    (map (partial swap state) new-blank-pos)))

(defn all-states-from [state]
  (loop [explored #{} frontier [state]]
    (if (empty? frontier) explored
      (let [s (peek frontier)
            frontier (pop frontier)
            explored (conj explored s)
            new-states (remove (partial contains? explored) (states-from s))
            frontier (apply conj frontier new-states)]
        (recur explored frontier)))))

(defn ex3-4 []
  (let [s1 (all-states-from state1)
        s2 (all-states-from state2)]
    (println "s1 is all states originating from " state1)
    (println "s2 is all states originating from " state2)
    (println "s1 has " (count s1) " states")
    (println "s2 has " (count s2) " states")
    (println "The total states of s1 and s2 are " (+ (count s1) (count s2)))
    (println "The total possible states are " all-states-count)
    (println "The intersection of s1 and s2 has "
             (count (clojure.set/intersection s1 s2)) " states")))

(def goal-state {0 1, 1 2, 2 3, 3 4, 4 5, 5 6, 6 7, 7 8, 8 blank, blank 8})

(defn non-blank-pos [state]
  (keys (dissoc state (state blank) blank)))

(defn inversions [state pos]
  (let [inverted? #(and (> % pos) (< (state %) (state pos)))
        inverted-keys (filter inverted? (non-blank-pos state))]
    (apply + (for [x inverted-keys] (state x)))))

(defn total-inversions [state]
  (apply + (map (partial inversions state) (non-blank-pos state))))

(defn goal-achievable? [state goal]
  (= (mod (total-inversions state) 2) (mod (total-inversions goal) 2)))

(defn gen-random-state [goal]
  (let [perm (shuffle [1 2 3 4 5 6 7 8 blank])
        state (apply hash-map (interleave [0 1 2 3 4 5 6 7 8] perm))
        state (assoc state blank (first (filter #(= blank (state %)) (keys state))))]
    (if (goal-achievable? state goal) state
      (recur goal))))

; solution is a map of states as keys with move number as values
(defn depth-limited [initial-state depth-limit]
  (letfn [(iter [state limit step solution]
            (cond (= goal-state state) solution
                  (zero? limit) 'cutoff
                  :else
                  (let [new-states (remove #(contains? solution %) (states-from state))
                        results (map #(iter % (dec limit) (inc step)
                                            (assoc solution % (inc step))) new-states)
                        reduce-results (fn [r1 r2] (cond (= () r2) r1
                                                         (= () r1) r2
                                                         (= 'cutoff r1) r2
                                                         :else r1))]
                    (reduce reduce-results () results))))]
    (iter initial-state depth-limit 0 {initial-state 0})))

(defn iterative-deepening [initial-state]
  (loop [depth 0]
    (println depth)
    (let [result (depth-limited initial-state depth)]
      (if (not= result 'cutoff) result
        (recur (inc depth))))))

; let's define a priority queue data structure to use for the frontier
; operations we need to support: conj an entry [cost state], get an entry
; (independently of the cost), peek the lowest cost entry, disj an entry
; the priority queue has two underlining data structures:
; 1. a sorted-map of the entries as keys (as [cost state] vetors)
;    with their parent state as value to rebuild the path later
; 2. a hash-map with the states as keys and their cost as values
(defn priority-queue
  ([] (priority-queue (sorted-map) (hash-map)))
  ([queue states]
   (fn [& params]
     (let [[op [cost state :as entry]] params]
       (case op
         :conj (priority-queue (conj queue entry)
                               (assoc states state cost))
         :get (when-let [cost (states state)] (apply conj [cost] state))
         :peek (when-not (empty? queue) (first queue))
         :disj (priority-queue (disj queue entry)
                               (dissoc states state)))))))

(declare build-solution)
(declare update-frontier)

(defn x [tile]
  (mod tile 3))
(defn y [tile]
  (quot tile 3))

(defn manhattan [tile state]
  (let [k (dec (state tile))
        xd (Math/abs (- (x tile) (x k)))
        yd (Math/abs (- (y tile) (y k)))]
    (+ xd yd)))

; heuristic for cost to goal based on manhattan distance
(defn estimate-cost [state]
 (apply + (map #(manhattan % state) (non-blank-pos state))))

(defn make-entry [state parent-cost]
  (let [cost (+ (inc parent-cost) (estimate-cost state))]
    [cost state]))

(defn astar [initial-state]
  (loop [explored #{}
         frontier ((priority-queue) :conj [0 initial-state])]
    (when-let [[cost state :as entry] (frontier :peek)]
      (if (= goal-state state) (build-solution state explored)
        (let [new-explored (conj explored state)
              new-frontier (->> (states-from state)
                                (remove #(contains? new-explored %))
                                (map #(make-entry % cost))
                                (reduce #(update-frontier frontier %)))]
          (recur new-explored new-frontier))))))

(defn print-board [state]
  (let [board (for [x (range 9)] (state x))]
    (doseq [row (partition 3 board)]
      (println row)))
    (newline))

(defn print-solution [sol]
  (let [steps (count sol)
        sol-by-step (reduce #(assoc %1 (sol %2) %2) {} (keys sol))
        steps (for [x (range steps)] (sol-by-step x))]
    (doseq [x steps] (print-board x))))

