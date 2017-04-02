(ns vacuum.left-right-env)

(defn gen-cell []
  (rand-nth [:clean :dirty]))

; [0 0] is top left corner with x y coordinates
(defn gen-world
  "Random world if no params given"
  ([]
   {[0 0] (gen-cell) [1 0] (gen-cell)
    :agent-pos (rand-nth [[0 0] [1 0]])
    :score 0})
  ([state-left state-right agent-pos]
   {[0 0] state-left [1 0] state-right
    :agent-pos agent-pos
    :score 0}))

(def move-deltas {:left [-1 0] :right [1 0] :up [0 -1] :down [0 1]})

(defn move-action [world direction]
  (let [new-pos (vec (map + (:agent-pos world) (direction move-deltas)))]
    (if (world new-pos)
      (assoc world :agent-pos new-pos :score (dec (:score world)))
      world)))

(defn clean-action [world _]
  (if (= :dirty (world (:agent-pos world)))
    (assoc world (:agent-pos world) :clean :score (+ 100 (:score world)))
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
      (when print? (println "World: " world " -> " "Action " agent-action))
      (recur new-world the-agent (dec steps) print?))
    (:score world)))

(defn montecarlo [simulations steps-each create-agent-f world]
  (loop [score 0
         counter simulations]
    (if (zero? counter) (float (/ score simulations))
      (recur (+ score (simulator world (create-agent-f) steps-each false))
             (dec counter)))))

(def all-worlds
  (for [left [:dirty :clean]
        right [:dirty :clean]
        agent-pos [[0 0] [1 0]]]
    (apply gen-world [left right agent-pos])))

(defn test-all-worlds [the-agent]
  (->> all-worlds
      (map (partial montecarlo 100 100 the-agent))
      (map vector all-worlds (repeat (count all-worlds) " -> "))
      (map println)))

; agent sensors
; location-sensor returns a map like {:pos :right, :state :clean}
(defn location-sensor [world]
  (let [loc (:agent-pos world)
        state (world loc)
        pos (case loc
              [0 0] :left
              [1 0] :right)]
    {:pos pos :state state}))

; agent actuators
(defn clean-actuator []
  {:action :clean, :param nil})

(defn move-actuator [pos]
  (let [dir (if (= :right pos) :left :right)]
    {:action :move, :param dir}))

(def no-action {:action nil, :param nil})

(defn create-simple-agent []
  (fn [world]
    (let [{:keys [pos state]} (location-sensor world)]
      (if (= state :dirty) (clean-actuator) (move-actuator pos)))))

(defn create-state-agent []
  (let [moved? (atom false)]
    (fn [world]
      (let [{:keys [pos state]} (location-sensor world)]
        (cond (= state :dirty) (clean-actuator)
              (false? @moved?) (do (swap! moved? not) (move-actuator pos))
              :else no-action)))))

(defn create-simple-sees-all-agent []
  (fn [world]
    (let [pos (:agent-pos world)
          other-cell (if (= [0 0] pos) [1 0] [0 0])
          other-dir (if (= [0 0] pos) :left :right )]
      (cond (= :dirty (world pos)) (clean-actuator)
            (= :dirty (world other-cell)) (move-actuator other-dir)
            :else no-action))))

;(defn cell-step [location]
  ;(cond (dirty? location) :dirty
        ;(> dirty-prob (rand)) :dirty
        ;:else :clean))
