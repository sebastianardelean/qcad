(ns app.library
  (:require [app.model :as model]
            [app.persist :as persist]))

(def library-key "qcad.library")

(defn- register
  ([id name qubits x y] (register id name qubits x y false))
  ([id name qubits x y classical]
   (cond-> {:id id :type :register
            :component (if classical :classical-register :quantum-register)
            :name name :qubits qubits :x x :y y}
     classical (assoc :classical true))))

(defn- gate
  ([id component x y] (gate id component x y 1))
  ([id component x y qubits]
   (cond-> {:id id :type :gate :component component :x x :y y}
     (not= 1 qubits) (assoc :qubits qubits))))

(defn- wire [id from from-index to to-index]
  {:id id
   :from from
   :to to
   :from-port {:kind :output :index from-index}
   :to-port {:kind :input :index to-index}})

(defn- bell-circuit []
  {:items [(register 1 "q" 2 40 40)
           (gate 2 :h 240 40)
           (gate 3 :cx 400 40 2)]
   :wires [(wire 1 1 0 2 0)
           (wire 2 2 0 3 0)
           (wire 3 1 1 3 1)]})

(defn- ghz-circuit []
  {:items [(register 1 "q" 3 40 40)
           (gate 2 :h 240 40)
           (gate 3 :cx 400 40 2)
           (gate 4 :cx 560 40 2)]
   :wires [(wire 1 1 0 2 0)
           (wire 2 2 0 3 0)
           (wire 3 1 1 3 1)
           (wire 4 3 0 4 0)
           (wire 5 1 2 4 1)]})

(defn- qft-circuit []
  {:items [(register 1 "q" 3 40 40)
           (gate 2 :h 240 40)
           (gate 3 :cx 400 40 2)
           (gate 4 :h 560 40)
           (gate 5 :cx 720 40 2)
           (gate 6 :swap 880 40 2)]
   :wires [(wire 1 1 0 2 0)
           (wire 2 2 0 3 0)
           (wire 3 1 1 3 1)
           (wire 4 3 0 4 0)
           (wire 5 3 1 5 0)
           (wire 6 1 2 5 1)
           (wire 7 5 0 6 0)
           (wire 8 4 0 6 1)]})

(defn- grover-circuit []
  {:items [(register 1 "q" 2 40 40)
           (gate 2 :h 240 40)
           (gate 3 :h 240 100)
           (gate 4 :cz 400 40 2)
           (gate 5 :h 560 40)
           (gate 6 :h 560 100)]
   :wires [(wire 1 1 0 2 0)
           (wire 2 1 1 3 0)
           (wire 3 2 0 4 0)
           (wire 4 3 0 4 1)
           (wire 5 4 0 5 0)
           (wire 6 4 1 6 0)]})

(defn- teleportation-circuit []
  {:items [(register 1 "q" 3 40 40)
           (gate 2 :h 240 40)
           (gate 3 :cx 400 40 2)
           (gate 4 :cx 560 40 2)
           (gate 5 :h 720 40)
           (register 6 "c" 2 40 220 true)
           (gate 7 :measure 880 40)
           (gate 8 :measure 1040 40)]
   :wires [(wire 1 1 1 2 0)
           (wire 2 2 0 3 0)
           (wire 3 1 2 3 1)
           (wire 4 1 0 4 0)
           (wire 5 3 0 4 1)
           (wire 6 4 0 5 0)
           (wire 7 5 0 7 0)
           (wire 8 7 0 6 0)
           (wire 9 4 1 8 0)
           (wire 10 8 0 6 1)]})

(def templates
  [{:id :bell :name "Bell state" :description "Entangled two-qubit pair"
    :build bell-circuit}
   {:id :ghz :name "GHZ state" :description "Three-qubit entangled state"
    :build ghz-circuit}
   {:id :qft :name "QFT" :description "Quantum Fourier transform sketch"
    :build qft-circuit}
   {:id :grover :name "Grover iteration" :description "Two-qubit amplitude amplification"
    :build grover-circuit}
   {:id :teleportation :name "Teleportation" :description "State transfer with measurement"
    :build teleportation-circuit}])

(defn by-id [id]
  (first (filter #(= id (:id %)) templates)))

(defn build [id]
  (when-let [template (by-id id)]
    ((:build template))))

(defn insert-template [state id]
  (if-let [circuit (build id)]
    (model/paste-snapshot state circuit 20 20)
    state))

(defn- storage []
  (try
    (when (and (exists? js/localStorage) js/localStorage)
      js/localStorage)
    (catch :default _ nil)))

(defn user-circuits []
  (if-let [storage (storage)]
    (try
      (if-let [text (.getItem storage library-key)]
        (vec (js->clj (js/JSON.parse text) :keywordize-keys true))
        [])
      (catch :default _ []))
    []))

(defn save-user-circuit! [name state]
  (let [name (str name)
        entry {:name name :json (persist/to-json state)}
        entries (vec (filter #(not= name (:name %)) (user-circuits)))
        entries (conj entries entry)]
    (when-let [storage (storage)]
      (.setItem storage library-key (js/JSON.stringify (clj->js entries))))
    entries))

(defn delete-user-circuit! [name]
  (let [entries (vec (filter #(not= name (:name %)) (user-circuits)))]
    (when-let [storage (storage)]
      (.setItem storage library-key (js/JSON.stringify (clj->js entries))))
    entries))

(defn insert-user-circuit [state name]
  (if-let [entry (first (filter #(= name (:name %)) (user-circuits)))]
    (try
      (model/paste-snapshot state (persist/from-json (:json entry)) 20 20)
      (catch :default _ state))
    state))

(def block-padding 20)
(def min-block-width 120)
(def min-block-height 80)

(defn- circuit-bounds [circuit]
  (when (seq (:items circuit))
    (let [rects (map model/item-bounds (:items circuit))]
      [(apply min (map first rects))
       (apply min (map second rects))
       (apply max (map #(nth % 2) rects))
       (apply max (map #(nth % 3) rects))])))

(defn- insert-block [state circuit name x y]
  (if-let [[x1 y1 x2 y2] (circuit-bounds circuit)]
    (let [width (max min-block-width (+ (- x2 x1) (* 2 block-padding)))
          height (max min-block-height (+ (- y2 y1) (* 2 block-padding)))
          dx (- (+ x block-padding) x1)
          dy (- (+ y block-padding) y1)
          state (model/paste-snapshot state circuit dx dy)]
      (model/add-item state {:id (:next-id state)
                             :type :box
                             :component :box
                             :text name
                             :width width
                             :height height
                             :x x
                             :y y}))
    state))

(defn insert-block-template [state id x y]
  (if-let [template (by-id id)]
    (insert-block state (build id) (:name template) x y)
    state))

(defn insert-block-user [state name x y]
  (if-let [entry (first (filter #(= name (:name %)) (user-circuits)))]
    (try
      (insert-block state (persist/from-json (:json entry)) name x y)
      (catch :default _ state))
    state))
