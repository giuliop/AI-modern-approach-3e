(ns astar
  (:use clojure.test))

(defn world-size
  "The [x y] size of the world"
  [world]
  (vector (count (first world)) (count world)))

(defn valid?
  "Returns whether a cell is inside the given size"
  [[x y] [size-x size-y]]
  (and (< -1 x size-x) (< -1 y size-y)))

(defn validate
  "Returns a filtering function of cells inside the world"
  [world]
  (fn [[x y]] (valid? [x y] (world-size world))))

(def steps [[0 1] [0 -1] [1 0] [-1 0]])

(defn neighbors
  "The neighboring cells of [x y] in world"
  [[x y] world]
  (filter (validate world) (map #(vec (map + [x y] %)) steps)))

(defn distance
  "The distance between two cells (no diagonal moves allowed)"
  [[x y] [x-end y-end]]
  (+ (Math/abs (- x-end x)) (Math/abs (- y-end y))))

(defn cost [world [x y]]
  (get-in world [y x]))

(defn continue-path
  "Return a function to build a new entry continuing the path from the input entry
   using the input estimate-f to calculate the cost to the goal"
  [[score square cost-to-square path] estimate-f world]
  (fn [new-square]
    (let [cost-to-square (+ cost-to-square (cost world new-square))
          path (conj path new-square)
          score (+ cost-to-square (estimate-f new-square))]
      [score new-square cost-to-square path])))

(defn get-score [entry]
  (first entry))

(defn get-square [entry]
  (second entry))

(defn get-path [entry]
  (last entry))

(defn not-in- [entries]
  (complement (fn [entry] (contains? entries entry))))

(defn update-open
  "Return updated open entries adding new items and changing old
  items if a better score is found"
  [open entry]
  (let [[score item] (open :get entry)]
    (if (or (nil? score) (< (get-score entry) score))
      (open :conj entry)
      open)))

(defn format-result
  "Return the best path"
  [entry iterations]
  {:score (get-score entry) :path (get-path entry) :iterations iterations})

; let's define a priority queue data structure to use for the open set
; operations we need to support: conj an entry, get an entry (independently of the
; score), peek the lowest score entry, disj an entry
; the priority queue has two underlining data structures:
; 1. a sorted-set of the entries by the score
; 2. a hash-map with part of the entries as keys and their scores as values
;    entries are vectors: [score square cost-to-square path-to-sqaure]
;    but in the hash-map the keys are just the square
(defn priority-queue
  ([] (priority-queue (sorted-set) (hash-map)))
  ([queue items]
   (fn [& params]
     (let [[op entry] params
           item (get-square entry)]
       (case op
         :conj (priority-queue (conj queue entry)
                             (assoc items item (get-score entry)))
         :get (when-let [score (items item)] [score item])
         :peek (when-not (empty? queue) (first queue))
         :disj (priority-queue (disj queue entry)
                               (dissoc items item)))))))

(defn astar [start goal world estimate-f]
  (let [cost-f (fn [square] (estimate-f square goal))]
    (loop [entry [(cost-f start) start (cost world start) [start]]
           open (priority-queue)
           closed (hash-set)
           iterations 1]
      (cond (nil? entry) "No path found"
            (= (get-square entry) goal) (format-result entry iterations)
            :else
            (let [new-open (->> (neighbors (get-square entry) world)
                                (filter (not-in- closed))
                                (map (continue-path entry cost-f world))
                                (reduce update-open open))
                  new-entry (new-open :peek)]
              (recur new-entry
                     (new-open :disj new-entry)
                     (conj closed (get-square entry))
                     (inc iterations)))))))


;;; TESTING ;;;
(def point1 {[10 0 1] {:to-here 5, :path [[0 0] [0 1]]}})
(def point1 {[100 0 3] {:to-here 5, :path [[0 0] [0 2]]}})

(defn dummy-world [x y]
  (repeat y (repeat x 0)))

(deftest neighbors-test
  (is (= (set [[3 1] [2 2]]) (set (neighbors [3 2] (dummy-world 4 3))))))

(deftest world-size-test
  (is (= [4 3] (world-size (dummy-world 4 3)))))

(deftest valid?-test
  (is (= true (valid? [3 1] [4 3]))))

(deftest estimate-test
  (is (= 2 (distance [3 1] [2 2]))))

(def z-world
  [[  1   1   1   1   1]
   [999 999 999 999   1]
   [  1   1   1   1   1]
   [  1 999 999 999 999]
   [  1   1   1   1   1]])

(def shubbery-world
  [[ 1 1 1   2 1]
   [ 1 1 1 999 1]
   [ 1 1 1 999 1]
   [ 1 1 1 999 1]
   [ 1 1 1   1 1]])

(def bunny-world
  [[ 1 1 1   2 1]
   [ 1 1 1 999 1]
   [ 1 1 1 999 1]
   [ 1 1 1 999 1]
   [ 1 1 1 666 1]])

(deftest cost-test
  (is (= 999 (cost z-world [0 1]))))

(deftest continue-path-test-test
  (is (= [1002 [0 2] 1001 [[0 0] [0 1] [0 2]]]
         ((continue-path [30 [0 1] 1000 [[0 0] [0 1]]] #(distance % [1 2]) z-world) [0 2]))))

(deftest entry-getters-test
  (let [e [1002 [0 2] 1001 [[0 0] [0 1] [0 2]]]]
    (is (= [1002 [0 2] [[0 0] [0 1] [0 2]]]
           [(get-score e) (get-square e) (get-path e)]))))

(deftest not-in-test
  (let [e1 [1002 [0 2] 1001 [[0 0] [0 1] [0 2]]]
        e2 [1002 [0 3] 1001 [[0 0] [0 1] [0 2]]]
        e3 [1002 [1 3] 1001 [[0 0] [0 1] [0 2]]]
        entries (sorted-set e1 e2)
        f (not-in- entries)]
    (is (= true (f e3)))
    (is (= false (f e2)))))

(deftest better-path?-test
  (let [e1 [1002 [0 2] 1001 [[0 0] [0 1] [0 2]]]
        e2 [1005 [0 3] 1001 [[0 0] [0 1] [0 2]]]]
    (is (= true (better-path? e2 e1)))))

(deftest update-open-test
  (let [e1 [1002 [0 2] 1001 [[0 0] [0 1] [0 2]]]
        open ((priority-queue) :conj e1)
        e2 [999 [0 2] 1001 [[0 0] [0 1] [0 2]]]
        open (update-open open e2)]
    (is (= e2 (open :peek)))))

(deftest astar-test
  (let [f (fn [world] (astar [0 0] (vec (map dec (world-size world))) world distance))]
    (is (= 17 (:score (f z-world))))
    (is (= 9 (:score (f shubbery-world))))
    (is (= 10 (:score (f bunny-world))))))
