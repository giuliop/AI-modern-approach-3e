(ns vacuum.random-env)

(defn gen-state []
  (rand-nth [:clean :dirty]))

(defn gen-agent-pos [maze]
  (let [x-size (:x-size maze)
        y-size (:y-size maze)
        pos [(rand-int x-size) (rand-int y-size)]]
    (if (get-in maze [pos :open]) pos
      (gen-agent-pos maze))))

; [0 0] is top left corner with x y coordinates
(defn gen-world
  ([x-size y-size]
   (let [maze (gen-maze x-size y-size)]
     (into maze
         {:agent-pos (gen-agent-pos maze)
          :score 0}))))

(defn gen-maze
  "Generate a random map of specified size by making each cell either open on all
  sides or closed on all sides"
  [x-size y-size]
  (let [cells (for [x (range x-size) y (range y-size)] [x y])
        gen-state (fn [] {:open (rand-nth [true true false]) :state (gen-state)})
        states (take (count cells) (repeatedly gen-state))]
    (into {:x-size x-size :y-size y-size}
           (apply hash-map (interleave cells states)))))

(defn draw-cell [world cell]
  (let [{:keys [open state]} (world cell)
        agent-pos (:agent-pos world)]
    (cond (not open) "#"
          (= agent-pos cell) (if (= :dirty state) "ä" "a")
          (= :dirty state) "."
          :else " ")))

(defn all-locations
  "Returns a matrix of all [x y] with y rows and x columns"
 [world]
  (let [cells (for [y (range(:y-size world)) x (range (:x-size world))] [x y])]
    (partition (:x-size world) cells)))

(defn format-world [world]
  (map #(map (partial draw-cell world) %) (all-locations world)))

(defn print-world [world]
  (let [world (format-world world)
        [lines last-line] (split-at (dec (count world)) world)]
    (dorun (map println lines))
    (print (first last-line))))

(def move-deltas {:left [-1 0] :right [1 0] :up [0 -1] :down [0 1]})

(defn direction [from to]
  (let [delta (map - to from)]
    (first (for [k (keys move-deltas) :when (= delta (k move-deltas))] k))))

(defn neighbors [[x y]]
  (map #(map + [x y] %) (vals move-deltas)))

(defn neighbor? [p1 p2]
  (some #{p1} (neighbors p2)))

(defn move-action [world direction]
  (let [new-pos (vec (map + (:agent-pos world) (direction move-deltas)))]
    (if (get-in world [new-pos :open])
      (assoc world :agent-pos new-pos :score (dec (:score world)))
      (assoc world :score (dec (:score world))))))

(defn clean-action [world _]
  (if (= :dirty (get-in world [(:agent-pos world) :state]))
    (-> world
        (assoc-in [(:agent-pos world) :state] :clean)
        (assoc :score (+ 100 (:score world))))
    world))

; agent actions are a dict {:action x :param y} where y is an optional parameter
; for the action
(def actions {:move move-action :clean clean-action})

(defn world-step [world agent-action]
  (let [{:keys [action param]} agent-action]
    (if-let [action (get actions action)]
      (action world param)
      world)))

(defn simulator [world the-agent steps print?]
  (if (pos? steps)
    (let [agent-action (the-agent world)
          new-world (world-step world agent-action)]
      (when print? (do (print-world world)
                       (println "   Score " (:score world) \newline)
                       (println "Action " agent-action)))
      (recur new-world the-agent (dec steps) print?))
    (:score world)))

(defn montecarlo [simulations steps-each create-agent-f world]
  (loop [score 0
         counter simulations]
    (if (zero? counter) (float (/ score simulations))
      (recur (+ score (simulator world (create-agent-f) steps-each false))
             (dec counter)))))

(defn test-worlds [worlds the-agent]
  (->> worlds
      (map (partial montecarlo 100 100 the-agent))
      (map vector worlds (repeat (count worlds) " -> "))
      (map println)))

; agent sensors
; location-sensor returns a map like {:pos :right, :state :clean}
(defn location-sensor [world]
  (let [pos (:agent-pos world)
        state (get-in world [pos :state])]
    {:pos pos :state state}))

; agent actuators
(defn clean-actuator []
  {:action :clean, :param nil})

(defn move-actuator [dir]
  {:action :move, :param dir})

(def no-action {:action nil, :param nil})

(defn create-simple-agent []
  (fn [world]
    (let [{:keys [pos state]} (location-sensor world)]
      (if (= state :dirty) (clean-actuator)
        (move-actuator (rand-nth (keys move-deltas)))))))

(defn create-state-agent []
  (let [maze (atom {}) ; map of the world
        to-visit (atom []) ; frontier still to visit
        path (atom [])] ; path so far to enable backtracking
    (fn [world]
      (let [{:keys [pos state]} (location-sensor world)
            initialize! (when (empty? @path) (swap! path conj pos))
            last-move (peek @path)]
        (if (not= last-move pos) ; bumped
          (do (swap! maze assoc last-move :closed)
              (swap! path pop))
          (when (not (contains? @maze pos))
            (do (swap! maze assoc pos :open)
                (apply swap! to-visit conj (remove #(contains? @maze %)
                                                   (neighbors pos))))))
        (let [next-pos (peek @to-visit)]
          (cond (= state :dirty) (clean-actuator)
                (not next-pos) no-action
                (neighbor? next-pos pos) (do (swap! path conj next-pos)
                                             (swap! to-visit pop)
                                             (move-actuator (direction pos next-pos)))
                :else (do (swap! path pop)
                          (move-actuator (direction pos (peek @path))))))))))
