(ns app.model
  (:require [clojure.string :as str]
            [app.components :as components]))

(def initial-state
  {:items []
   :next-id 1
   :dialog nil
   :selected #{}
   :context-menu nil
   :wires []
   :next-wire-id 1
   :tool :select
   :wire-draft nil
   :view {:pan-x 0 :pan-y 0 :zoom 1}
   :snap true
   :snap-size 20
   :cursor nil
   :marquee nil})

(def rotation-step-degrees 90)

(def min-zoom 0.25)
(def max-zoom 4)

(def ^:private gate-size [44 36])
(def ^:private register-size [160 36])

(defn snap-value [value size]
  (* size (js/Math.round (/ value size))))

(defn snap-point [state [x y]]
  (if (:snap state)
    [(snap-value x (:snap-size state))
     (snap-value y (:snap-size state))]
    [x y]))

(defn toggle-snap [state]
  (update state :snap not))

(defn screen->world [{:keys [pan-x pan-y zoom]} [sx sy]]
  [(/ (- sx pan-x) zoom)
   (/ (- sy pan-y) zoom)])

(defn world->screen [{:keys [pan-x pan-y zoom]} [wx wy]]
  [(+ pan-x (* wx zoom))
   (+ pan-y (* wy zoom))])

(defn pan-by [state dx dy]
  (update state :view
          (fn [{:keys [pan-x pan-y] :as view}]
            (assoc view :pan-x (+ pan-x dx) :pan-y (+ pan-y dy)))))

(defn reset-view [state]
  (assoc state :view {:pan-x 0 :pan-y 0 :zoom 1}))

(defn- clamp [value low high]
  (max low (min high value)))

(defn zoom-at [state factor [sx sy]]
  (let [{:keys [zoom] :as view} (:view state)
        new-zoom (clamp (* zoom factor) min-zoom max-zoom)
        [wx wy] (screen->world view [sx sy])]
    (assoc state :view {:pan-x (- sx (* wx new-zoom))
                        :pan-y (- sy (* wy new-zoom))
                        :zoom new-zoom})))

(defn set-cursor [state point]
  (assoc state :cursor point))

(defn begin-marquee [state point]
  (assoc state :marquee {:start point :current point}))

(defn update-marquee [state point]
  (if (:marquee state)
    (assoc-in state [:marquee :current] point)
    state))

(defn cancel-marquee [state]
  (if (:marquee state)
    (assoc state :marquee nil)
    state))

(defn finish-marquee [state]
  (assoc state :marquee nil))

(defn marquee-rect [{:keys [start current]}]
  (let [[x1 y1] start
        [x2 y2] current]
    [(min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)]))

(defn item-bounds [{:keys [x y width height type]}]
  (let [[default-width default-height] (if (= :register type)
                                         register-size
                                         gate-size)]
    [x y
     (+ x (or width default-width))
     (+ y (or height default-height))]))

(defn rects-intersect? [[ax1 ay1 ax2 ay2] [bx1 by1 bx2 by2]]
  (and (< ax1 bx2) (< bx1 ax2) (< ay1 by2) (< by1 ay2)))

(defn marquee-item-ids [items marquee]
  (let [rect (marquee-rect marquee)]
    (into #{}
          (comp (filter #(rects-intersect? rect (item-bounds %)))
                (map :id))
          items)))

(defn update-marquee-selection [state items]
  (if-let [marquee (:marquee state)]
    (assoc state :selected (marquee-item-ids items marquee))
    state))

(defn parse-qubits [value]
  (let [text (some-> value str str/trim)]
    (when (and text (re-matches #"[0-9]+" text))
      (let [n (js/parseInt text 10)]
        (when (pos? n) n)))))

(defn validate-register [name qubits]
  (cond
    (str/blank? (str name)) {:name "Name is required."}
    (nil? (parse-qubits qubits)) {:qubits "Number of qubits must be a positive whole number."}
    :else nil))

(defn next-register-name [state]
  (let [names (into #{} (keep #(when (= :register (:type %)) (:name %)) (:items state)))]
    (first (remove names (map #(str "register-" %) (iterate inc 1))))))

(defn add-item [state item]
  (-> state
      (update :items conj item)
      (update :next-id inc)))

(defn add-gate [state component x y]
  (add-item state {:id (:next-id state)
                   :type :gate
                   :component (:id component)
                   :x x
                   :y y}))

(defn select-item [state item-id]
  (assoc state :selected #{item-id}))

(defn toggle-selection [state item-id]
  (update state :selected
          (fn [selected]
            (if (contains? selected item-id)
              (disj selected item-id)
              (conj selected item-id)))))

(defn clear-selection [state]
  (assoc state :selected #{}))

(defn move-items [state origins dx dy]
  (update state :items
          (fn [items]
            (mapv (fn [{:keys [id] :as item}]
                    (if-let [[x y] (get origins id)]
                      (let [[snapped-x snapped-y] (snap-point state [(+ x dx) (+ y dy)])]
                        (assoc item :x snapped-x :y snapped-y))
                      item))
                  items))))

(defn item-by-id [state item-id]
  (first (filter #(= item-id (:id %)) (:items state))))

(defn delete-items [state item-ids]
  (let [item-ids (set item-ids)
        draft (:wire-draft state)]
    (-> state
        (update :items (fn [items] (filterv #(not (contains? item-ids (:id %))) items)))
        (update :wires (fn [wires]
                         (filterv (fn [wire]
                                    (not (or (contains? item-ids (:from wire))
                                             (contains? item-ids (:to wire)))))
                                  wires)))
        (update :selected #(apply disj % item-ids))
        (assoc :context-menu nil)
        (cond-> (and draft (contains? item-ids (:from draft)))
          (assoc :wire-draft nil)))))

(defn delete-item [state item-id]
  (delete-items state [item-id]))

(defn wire-tool? [state]
  (= :wire (:tool state)))

(defn set-tool [state tool]
  (assoc state :tool tool :wire-draft nil))

(defn toggle-wire-tool [state]
  (set-tool state (if (wire-tool? state) :select :wire)))

(defn start-wire-draft [state item-id x y]
  (assoc state :wire-draft {:from item-id :x x :y y :points []}))

(defn update-wire-draft [state x y]
  (if-let [draft (:wire-draft state)]
    (assoc state :wire-draft (assoc draft :x x :y y))
    state))

(defn add-wire-point [state x y]
  (if-let [draft (:wire-draft state)]
    (let [point [x y]
          points (:points draft)]
      (assoc state :wire-draft
             (cond-> (assoc draft :x x :y y)
               (not= point (peek points))
               (update :points conj point))))
    state))

(defn cancel-wire-draft [state]
  (if (:wire-draft state)
    (assoc state :wire-draft nil)
    state))

(defn- without-duplicate-points [points]
  (reduce (fn [result point]
            (if (= point (peek result))
              result
              (conj result point)))
          []
          points))

(defn orthogonal-route [[x1 y1] [x2 y2]]
  (if (or (= x1 x2) (= y1 y2))
    [[x1 y1] [x2 y2]]
    (let [mid-x (js/Math.round (/ (+ x1 x2) 2))]
      (-> [[x1 y1] [mid-x y1] [mid-x y2] [x2 y2]]
          without-duplicate-points))))

(defn orthogonal-path [points]
  (if (< (count points) 2)
    (vec points)
    (-> (reduce (fn [result [from to]]
                  (into result (rest (orthogonal-route from to))))
                [(first points)]
                (partition 2 1 points))
        without-duplicate-points)))

(defn- wire-between? [state from-id to-id]
  (boolean (some (fn [wire]
                   (= #{from-id to-id} #{(:from wire) (:to wire)}))
                 (:wires state))))

(defn- register-item? [item]
  (= :register (:type item)))

(defn add-wire
  ([state from-id to-id] (add-wire state from-id to-id nil))
  ([state from-id to-id points]
   (let [from-item (item-by-id state from-id)
         to-item (item-by-id state to-id)]
     (if (and (not= from-id to-id)
              from-item
              to-item
              (not (and (register-item? from-item)
                        (register-item? to-item)))
              (not (wire-between? state from-id to-id)))
       (-> state
           (update :wires conj (cond-> {:id (:next-wire-id state)
                                        :from from-id
                                        :to to-id}
                                 (seq points) (assoc :points (vec points))))
           (update :next-wire-id inc))
       state))))

(defn finish-wire [state to-id]
  (if-let [draft (:wire-draft state)]
    (-> state
        (add-wire (:from draft) to-id (:points draft))
        (assoc :wire-draft nil))
    state))

(defn rotate-items [state item-ids degrees]
  (let [item-ids (set item-ids)]
    (update state :items
            (fn [items]
              (mapv (fn [item]
                      (if (contains? item-ids (:id item))
                        (assoc item :rotation
                               (mod (+ (or (:rotation item) 0) degrees) 360))
                        item))
                    items)))))

(defn rotate-item-by [state item-id degrees]
  (rotate-items state [item-id] degrees))

(defn rotate-item
  ([state item-id] (rotate-item-by state item-id rotation-step-degrees))
  ([state item-id degrees] (rotate-item-by state item-id degrees)))

(defn open-context-menu [state item-id x y]
  (assoc state
         :selected #{item-id}
         :context-menu {:item-id item-id :x x :y y}))

(defn close-context-menu [state]
  (assoc state :context-menu nil))

(defn open-register-dialog [state x y]
  (assoc state
         :dialog {:name (next-register-name state)
                  :qubits "2"
                  :x x
                  :y y
                  :error nil}
         :context-menu nil))

(defn handle-drop [state component-id x y]
  (if-let [component (components/by-id component-id)]
    (let [[x y] (snap-point state [x y])]
      (case (:kind component)
        :register (open-register-dialog state x y)
        :gate (add-gate state component x y)
        state))
    state))

(defn confirm-register [state name qubits]
  (if-let [dialog (:dialog state)]
    (if-let [errors (validate-register name qubits)]
      (assoc state :dialog (assoc dialog
                                  :name (str name)
                                  :qubits (str qubits)
                                  :error errors))
      (let [item {:id (:next-id state)
                  :type :register
                  :component :quantum-register
                  :name (str/trim (str name))
                  :qubits (parse-qubits qubits)
                  :x (:x dialog)
                  :y (:y dialog)}]
        (-> state
            (add-item item)
            (assoc :dialog nil))))
    state))

(defn cancel-register [state]
  (if (:dialog state)
    (assoc state :dialog nil)
    state))
