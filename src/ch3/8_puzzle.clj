(ns ch3.4)

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

; return new possible states from state
(defn states-from [state]
  (let [swaps [1 -1 3 -3]
        legal? #(<= 0 % 8)
        blank-pos (state blank)
        new-blank-pos (filter legal? (map #(+ blank-pos %) swaps))]
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
