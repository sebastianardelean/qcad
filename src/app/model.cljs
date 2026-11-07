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
   :snap-components false
   :snap-size 20
   :cursor nil
   :marquee nil
   :metadata {:name "" :author "" :description ""}
   :includes ["stdgates.inc"]
   :raw-statements []
   :history []
   :history-index -1
   :clipboard nil
   :component-search ""
   :theme :light
   :baseline nil})

(def rotation-step-degrees 90)

(def min-zoom 0.25)
(def max-zoom 4)

(def history-limit 100)
(def default-angle "pi/2")
(def angle-presets ["pi/4" "pi/2" "pi" "2*pi"])

(def ^:private gate-size [44 36])
(def ^:private register-size [160 36])
(def ^:private annotation-size [140 30])
(def ^:private box-size [220 140])

(defn snap-value [value size]
  (* size (js/Math.round (/ value size))))

(defn snap-point [state [x y]]
  (if (:snap state)
    [(snap-value x (:snap-size state))
     (snap-value y (:snap-size state))]
    [x y]))

(defn toggle-snap [state]
  (update state :snap not))

(defn toggle-component-snap [state]
  (update state :snap-components not))

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
  (let [[default-width default-height]
        (case type
          :register register-size
          :annotation annotation-size
          :box box-size
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

(defn validate-custom-gate [qubits qasm]
  (cond
    (nil? (parse-qubits qubits)) {:qubits "Number of qubits must be a positive whole number."}
    (str/blank? (str qasm)) {:qasm "Gate definition is required."}
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

(def alignment-threshold 8)

(defn- bounds-of [items]
  (when (seq items)
    (let [rects (map item-bounds items)]
      [(apply min (map first rects))
       (apply min (map second rects))
       (apply max (map #(nth % 2) rects))
       (apply max (map #(nth % 3) rects))])))

(defn- axis-values [[x1 y1 x2 y2] axis]
  (if (= axis :x)
    [x1 (/ (+ x1 x2) 2) x2]
    [y1 (/ (+ y1 y2) 2) y2]))

(defn- best-axis-snap [moving-bounds static-bounds axis]
  (let [moving (axis-values moving-bounds axis)
        delta (->> (for [bounds static-bounds
                         a moving
                         b (axis-values bounds axis)
                         :let [delta (- b a)]
                         :when (<= (js/Math.abs delta) alignment-threshold)]
                     delta)
                   (sort-by js/Math.abs)
                   first)]
    (or delta 0)))

(defn- component-snap-offset [items moved-ids]
  (let [moving (filter #(contains? moved-ids (:id %)) items)
        static (remove #(contains? moved-ids (:id %)) items)]
    (if (or (empty? moving) (empty? static))
      [0 0]
      (let [moving-bounds (bounds-of moving)
            static-bounds (map item-bounds static)]
        [(best-axis-snap moving-bounds static-bounds :x)
         (best-axis-snap moving-bounds static-bounds :y)]))))

(defn- snapped-item [state origins dx dy item]
  (if-let [[x y] (get origins (:id item))]
    (let [[snapped-x snapped-y] (snap-point state [(+ x dx) (+ y dy)])]
      (assoc item :x snapped-x :y snapped-y))
    item))

(defn move-items [state origins dx dy]
  (let [moved-ids (set (keys origins))
        items (mapv #(snapped-item state origins dx dy %) (:items state))
        [ax ay] (if (:snap-components state)
                  (component-snap-offset items moved-ids)
                  [0 0])]
    (assoc state :items
           (if (and (zero? ax) (zero? ay))
             items
             (mapv (fn [item]
                     (if (contains? moved-ids (:id item))
                       (-> item
                           (update :x + ax)
                           (update :y + ay))
                       item))
                   items)))))

(defn item-by-id [state item-id]
  (first (filter #(= item-id (:id %)) (:items state))))

(defn- item-adjacency [state]
  (let [ids (into #{} (map :id (:items state)))]
    (reduce (fn [adj {:keys [from to]}]
              (if (and (contains? ids from) (contains? ids to))
                (-> adj
                    (update from (fnil conj #{}) to)
                    (update to (fnil conj #{}) from))
                adj))
            {}
            (:wires state))))

(defn- reachable-item-ids [state item-id]
  (let [adjacency (item-adjacency state)]
    (loop [visited #{} queue [item-id]]
      (if-let [id (peek queue)]
        (if (contains? visited id)
          (recur visited (pop queue))
          (recur (conj visited id)
                 (into (pop queue) (get adjacency id))))
        visited))))

(defn connected-registers [state item-id]
  (let [reachable (reachable-item-ids state item-id)]
    (->> (:items state)
         (filter #(and (= :register (:type %))
                       (contains? reachable (:id %))))
         (sort-by (juxt #(or (:y %) 0) #(or (:x %) 0) #(or (:id %) 0)))
         vec)))

(defn gate-target-count [gate]
  (let [qubits (:qubits gate)]
    (if (and (integer? qubits) (pos? qubits))
      qubits
      1)))

(defn gate-control-count [gate]
  (let [controls (:controls gate)]
    (if (and (integer? controls) (pos? controls))
      controls
      0)))

(defn gate-port-count [gate]
  (+ (gate-target-count gate) (gate-control-count gate)))

(defn classical-register? [item]
  (boolean (:classical item)))

(defn port-count [item kind]
  (case (:type item)
    :register (if (classical-register? item)
                (if (= :input kind) (or (:qubits item) 0) 0)
                (if (= :output kind) (or (:qubits item) 0) 0))
    :gate (if (contains? #{:input :output} kind) (gate-port-count item) 0)
    0))

(defn port-used? [state item-id kind index]
  (boolean
   (some (fn [wire]
           (or (and (= item-id (:from wire))
                    (= kind (get-in wire [:from-port :kind]))
                    (= index (get-in wire [:from-port :index])))
               (and (= item-id (:to wire))
                    (= kind (get-in wire [:to-port :kind]))
                    (= index (get-in wire [:to-port :index])))))
         (:wires state))))

(defn free-port-index [state item-id kind]
  (let [item (item-by-id state item-id)]
    (first (remove #(port-used? state item-id kind %)
                   (range (port-count item kind))))))

(defn- incoming-port-wire [state item-id index]
  (first (filter (fn [wire]
                   (and (= item-id (:to wire))
                        (= :input (get-in wire [:to-port :kind]))
                        (= index (get-in wire [:to-port :index]))))
                 (:wires state))))

(declare trace-input-target)

(defn- trace-output-target [state item-id index visited]
  (let [endpoint [item-id :output index]]
    (when-not (contains? visited endpoint)
      (trace-input-target state item-id index (conj visited endpoint)))))

(defn- trace-input-target [state item-id index visited]
  (let [endpoint [item-id :input index]]
    (when-not (contains? visited endpoint)
      (when-let [wire (incoming-port-wire state item-id index)]
        (let [from-id (:from wire)
              from-item (item-by-id state from-id)
              from-port (:from-port wire)
              source-index (:index from-port)]
          (when (and from-item
                     (= :output (:kind from-port))
                     (integer? source-index))
            (cond
              (= :register (:type from-item))
              (when (and (<= 0 source-index)
                         (< source-index (or (:qubits from-item) 0)))
                [from-id source-index])

              (= :gate (:type from-item))
              (trace-output-target state from-id source-index (conj visited endpoint)))))))))

(defn gate-targets [state gate-id]
  (if-let [gate (item-by-id state gate-id)]
    (if (= :gate (:type gate))
      (->> (range (gate-port-count gate))
           (keep #(trace-input-target state gate-id % #{}))
           distinct
           vec)
      [])
    []))

(defn gate-port-wired? [state gate-id]
  (boolean
   (some (fn [wire]
           (and (= gate-id (:to wire))
                (= :input (get-in wire [:to-port :kind]))))
         (:wires state))))

(defn output-register-targets [state item-id]
  (->> (:wires state)
       (keep (fn [wire]
               (when (and (= item-id (:from wire))
                          (= :output (get-in wire [:from-port :kind])))
                 (let [to-item (item-by-id state (:to wire))]
                   (when (and to-item
                              (= :register (:type to-item))
                              (classical-register? to-item)
                              (= :input (get-in wire [:to-port :kind])))
                     [(:to wire) (get-in wire [:to-port :index])])))))
       vec))

(defn output-target [state item-id index]
  (first (filter (fn [wire]
                   (and (= item-id (:from wire))
                        (= :output (get-in wire [:from-port :kind]))
                        (= index (get-in wire [:from-port :index]))))
                 (:wires state))))

(defn classical-targets [state gate-id]
  (->> (output-register-targets state gate-id)
       (sort-by (fn [[_ index]] index))
       vec))

(defn measure-targets [state gate-id]
  (let [quantum (gate-targets state gate-id)
        classical (classical-targets state gate-id)]
    [(first quantum) (first classical)]))

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

(defn start-wire-draft
  ([state item-id x y] (start-wire-draft state item-id x y nil))
  ([state item-id x y from-port]
   (assoc state :wire-draft
          (cond-> {:from item-id :x x :y y :points []}
            from-port (assoc :from-port from-port)))))

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

(defn- endpoint-match? [wire side item-id port]
  (let [wire-item ((if (= side :from) :from :to) wire)
        wire-port ((if (= side :from) :from-port :to-port) wire)]
    (and (= item-id wire-item)
         (if (some? port)
           (= port wire-port)
           (nil? wire-port)))))

(defn- wire-between? [state from-id to-id from-port to-port]
  (boolean
   (some (fn [wire]
           (or (and (endpoint-match? wire :from from-id from-port)
                    (endpoint-match? wire :to to-id to-port))
               (and (endpoint-match? wire :from to-id to-port)
                    (endpoint-match? wire :to from-id from-port))))
         (:wires state))))

(defn- register-item? [item]
  (= :register (:type item)))

(defn- valid-port? [item kind index]
  (and (integer? index)
       (<= 0 index)
       (< index (port-count item kind))))

(defn- valid-wire-ports? [state from-item to-item from-port to-port]
  (and from-port
       to-port
       (= :output (:kind from-port))
       (= :input (:kind to-port))
       (valid-port? from-item :output (:index from-port))
       (valid-port? to-item :input (:index to-port))
       (not (port-used? state (:id from-item) :output (:index from-port)))
       (not (port-used? state (:id to-item) :input (:index to-port)))))

(defn add-wire
  ([state from-id to-id] (add-wire state from-id to-id nil))
  ([state from-id to-id points]
   (add-wire state from-id to-id points nil nil))
  ([state from-id to-id points from-port to-port]
   (let [from-item (item-by-id state from-id)
         to-item (item-by-id state to-id)
         port-wire? (or (some? from-port) (some? to-port))
         ports-valid? (if port-wire?
                        (valid-wire-ports? state from-item to-item from-port to-port)
                        true)]
     (if (and (not= from-id to-id)
              from-item
              to-item
              (not (and (register-item? from-item)
                        (register-item? to-item)))
              ports-valid?
              (not (wire-between? state from-id to-id from-port to-port)))
       (-> state
           (update :wires conj (cond-> {:id (:next-wire-id state)
                                        :from from-id
                                        :to to-id}
                                 (seq points) (assoc :points (vec points))
                                 from-port (assoc :from-port from-port)
                                 to-port (assoc :to-port to-port)))
           (update :next-wire-id inc))
       state))))

(defn finish-wire
  ([state to-id] (finish-wire state to-id nil))
  ([state to-id to-port]
   (if-let [draft (:wire-draft state)]
     (-> state
         (add-wire (:from draft) to-id (:points draft) (:from-port draft) to-port)
         (assoc :wire-draft nil))
     state)))

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

(defn open-register-dialog
  ([state x y] (open-register-dialog state x y false))
  ([state x y classical]
   (assoc state
          :dialog (cond-> {:name (next-register-name state)
                           :qubits "2"
                           :x x
                           :y y
                           :error nil}
                    classical (assoc :classical true))
          :context-menu nil)))

(defn open-custom-dialog [state x y]
  (assoc state
         :dialog {:type :custom-gate
                  :x x
                  :y y
                  :qubits "1"
                  :qasm ""
                  :error nil}
         :context-menu nil))

(defn open-custom-editor [state item-id]
  (if-let [item (item-by-id state item-id)]
    (if (= :custom (:component item))
      (assoc state
             :dialog {:type :custom-gate
                      :item-id item-id
                      :x (:x item)
                      :y (:y item)
                      :qubits (str (gate-port-count item))
                      :qasm (or (:qasm item) "")
                      :error nil}
             :context-menu nil)
      state)
    state))

(defn open-annotation-dialog [state x y]
  (assoc state
         :dialog {:type :annotation
                  :x x
                  :y y
                  :text ""
                  :error nil}
         :context-menu nil))

(defn open-annotation-editor [state item-id]
  (if-let [item (item-by-id state item-id)]
    (if (contains? #{:annotation :box} (:type item))
      (assoc state
             :dialog {:type :annotation
                      :item-id item-id
                      :target-type (:type item)
                      :x (:x item)
                      :y (:y item)
                      :text (or (:text item) "")
                      :error nil}
             :context-menu nil)
      state)
    state))

(defn open-metadata-dialog [state]
  (assoc state
         :dialog {:type :metadata
                  :metadata (:metadata state)
                  :error nil}
         :context-menu nil))

(defn open-export-dialog [state]
  (assoc state
         :dialog {:type :export
                  :format "svg"
                  :scale 1
                  :transparent false
                  :include-ui false
                  :error nil}
         :context-menu nil))

(defn open-library-dialog [state]
  (assoc state
         :dialog {:type :library
                  :name "My circuit"
                  :error nil}
         :context-menu nil))

(defn open-diff-dialog [state diff]
  (assoc state
         :dialog {:type :diff
                  :diff diff
                  :error nil}
         :context-menu nil))

(defn open-gate-qubits-dialog [state gate-id]
  (if-let [gate (item-by-id state gate-id)]
    (if (= :gate (:type gate))
      (assoc state
             :dialog (cond-> {:type :gate-qubits
                              :gate-id gate-id
                              :qubits (str (gate-target-count gate))
                              :error nil}
                       (components/parameterized? (:component gate))
                       (assoc :angle (or (:angle gate) default-angle))

                       (pos? (gate-control-count gate))
                       (assoc :controls (str (gate-control-count gate))))
             :context-menu nil)
      state)
    state))

(defn- overflow-index? [index qubits]
  (and (integer? index) (>= index qubits)))

(defn- wire-overflows? [gate-id ports wire]
  (or (and (= gate-id (:from wire))
           (overflow-index? (get-in wire [:from-port :index]) ports))
      (and (= gate-id (:to wire))
           (overflow-index? (get-in wire [:to-port :index]) ports))))

(defn- draft-overflows? [draft gate-id ports]
  (and (= gate-id (:from draft))
       (overflow-index? (get-in draft [:from-port :index]) ports)))

(defn- update-gate-shape [state gate-id f]
  (let [gate (item-by-id state gate-id)
        updated (f gate)
        ports (gate-port-count updated)]
    (-> state
        (update :items
                (fn [items]
                  (mapv (fn [item]
                          (if (= (:id item) gate-id)
                            (dissoc updated :targets)
                            item))
                        items)))
        (update :wires
                (fn [wires]
                  (filterv #(not (wire-overflows? gate-id ports %)) wires)))
        (cond-> (and (:wire-draft state)
                     (draft-overflows? (:wire-draft state) gate-id ports))
          (assoc :wire-draft nil)))))

(defn- update-gate-qubits [state gate-id qubits]
  (update-gate-shape state gate-id #(assoc % :qubits qubits)))

(defn set-gate-controls [state gate-id controls]
  (let [controls (if (and (integer? controls) (pos? controls)) controls 0)]
    (update-gate-shape state gate-id #(cond-> %
                                        (pos? controls) (assoc :controls controls)
                                        (zero? controls) (dissoc :controls)))))

(defn toggle-gate-control [state gate-id]
  (if-let [gate (item-by-id state gate-id)]
    (if (= :gate (:type gate))
      (set-gate-controls state gate-id (if (pos? (gate-control-count gate)) 0 1))
      state)
    state))

(defn- parse-controls [value]
  (let [text (some-> value str str/trim)]
    (if (re-matches #"[0-9]+" (or text ""))
      (js/parseInt text 10)
      0)))

(defn- apply-gate-angle [item angle]
  (let [text (some-> angle str str/trim)]
    (if (and (components/parameterized? (:component item))
             (not (str/blank? text)))
      (assoc item :angle text)
      (dissoc item :angle))))

(defn confirm-gate-qubits
  ([state qubits] (confirm-gate-qubits state qubits nil nil))
  ([state qubits angle] (confirm-gate-qubits state qubits angle nil))
  ([state qubits angle controls]
   (if-let [dialog (:dialog state)]
     (if (= :gate-qubits (:type dialog))
       (if-let [n (parse-qubits qubits)]
         (let [gate-id (:gate-id dialog)
               with-shape (-> (update-gate-qubits state gate-id n)
                              (cond-> (some? controls)
                                (set-gate-controls gate-id (parse-controls controls))))]
           (-> with-shape
               (update :items
                       (fn [items]
                         (mapv #(if (= gate-id (:id %))
                                  (apply-gate-angle % angle)
                                  %)
                               items)))
               (assoc :dialog nil)))
         (assoc-in state [:dialog :error]
                   "Number of qubits must be a positive whole number."))
       state)
     state)))

(defn confirm-custom-gate [state qubits qasm]
  (if-let [dialog (:dialog state)]
    (if (= :custom-gate (:type dialog))
      (if-let [errors (validate-custom-gate qubits qasm)]
        (assoc state :dialog (assoc dialog
                                    :qubits (str qubits)
                                    :qasm (str qasm)
                                    :error errors))
        (let [n (parse-qubits qubits)
              text (str/trim (str qasm))]
          (if-let [item-id (:item-id dialog)]
            (-> (update-gate-qubits state item-id n)
                (update :items
                        (fn [items]
                          (mapv #(if (= item-id (:id %))
                                   (assoc % :qasm text)
                                   %)
                                items)))
                (assoc :dialog nil))
            (-> state
                (add-item {:id (:next-id state)
                           :type :gate
                           :component :custom
                           :qubits n
                           :qasm text
                           :x (:x dialog)
                           :y (:y dialog)})
                (assoc :dialog nil)))))
      state)
    state))

(defn- add-gate-with-defaults [state component x y]
  (let [qubits (components/default-qubits (:id component))
        state (add-gate state component x y)]
    (if (> qubits 1)
      (update-in state [:items (dec (count (:items state))) :qubits] (constantly qubits))
      state)))

(defn- add-box [state x y]
  (add-item state {:id (:next-id state)
                   :type :box
                   :component :box
                   :text "Group"
                   :width (first box-size)
                   :height (second box-size)
                   :x x
                   :y y}))

(defn handle-drop [state component-id x y]
  (if-let [component (components/by-id component-id)]
    (let [[x y] (snap-point state [x y])]
      (case (:kind component)
        :register (open-register-dialog state x y (:classical component))
        :custom (open-custom-dialog state x y)
        :annotation (open-annotation-dialog state x y)
        :box (add-box state x y)
        :gate (add-gate-with-defaults state component x y)
        state))
    state))

(defn confirm-register [state name qubits]
  (if-let [dialog (:dialog state)]
    (if-let [errors (validate-register name qubits)]
      (assoc state :dialog (assoc dialog
                                  :name (str name)
                                  :qubits (str qubits)
                                  :error errors))
      (let [classical? (:classical dialog)
            item (cond-> {:id (:next-id state)
                          :type :register
                          :component (if classical?
                                       :classical-register
                                       :quantum-register)
                          :name (str/trim (str name))
                          :qubits (parse-qubits qubits)
                          :x (:x dialog)
                          :y (:y dialog)}
                   classical? (assoc :classical true))]
        (-> state
            (add-item item)
            (assoc :dialog nil))))
    state))

(defn confirm-annotation [state text]
  (if-let [dialog (:dialog state)]
    (if (= :annotation (:type dialog))
      (let [text (str/trim (str text))]
        (if (str/blank? text)
          (assoc-in state [:dialog :error] "Text is required.")
          (if-let [item-id (:item-id dialog)]
            (-> state
                (update :items
                        (fn [items]
                          (mapv #(if (= item-id (:id %))
                                   (assoc % :text text)
                                   %)
                                items)))
                (assoc :dialog nil))
            (let [width (max 60 (+ 24 (* 8 (count text))))]
              (-> state
                  (add-item {:id (:next-id state)
                             :type :annotation
                             :component :annotation
                             :text text
                             :width width
                             :height 30
                             :x (:x dialog)
                             :y (:y dialog)})
                  (assoc :dialog nil))))))
      state)
    state))

(defn confirm-metadata [state metadata]
  (if-let [dialog (:dialog state)]
    (if (= :metadata (:type dialog))
      (let [clean (into {}
                        (map (fn [[key value]]
                               [key (str/trim (str value))]))
                        (select-keys metadata [:name :author :description]))]
        (-> state
            (assoc :metadata clean)
            (assoc :dialog nil)))
      state)
    state))

(defn cancel-dialog [state]
  (if (:dialog state)
    (assoc state :dialog nil)
    state))

(defn cancel-register [state]
  (cancel-dialog state))

(defn snapshot [state]
  (select-keys state [:items :wires :next-id :next-wire-id
                      :metadata :includes :raw-statements]))

(defn set-baseline [state]
  (assoc state :baseline (snapshot state)))

(defn append-history [before after]
  (let [history (:history before [])
        index (:history-index before -1)
        base (if (empty? history)
               [(snapshot before)]
               (subvec history 0 (inc index)))
        history (conj base (snapshot after))
        history (if (> (count history) history-limit)
                  (subvec history (- (count history) history-limit))
                  history)]
    (assoc after
           :history history
           :history-index (dec (count history)))))

(defn can-undo? [state]
  (pos? (or (:history-index state) 0)))

(defn can-redo? [state]
  (let [history (:history state [])
        index (:history-index state -1)]
    (< index (dec (count history)))))

(defn- restore-snapshot [state snapshot index]
  (merge state
         snapshot
         {:history (:history state)
          :history-index index
          :selected #{}
          :context-menu nil
          :dialog nil
          :wire-draft nil
          :marquee nil}))

(defn undo [state]
  (let [index (:history-index state -1)
        history (:history state [])]
    (if (pos? index)
      (restore-snapshot state (nth history (dec index)) (dec index))
      state)))

(defn redo [state]
  (let [index (:history-index state -1)
        history (:history state [])
        next-index (inc index)]
    (if (and (not (neg? next-index))
             (< next-index (count history)))
      (restore-snapshot state (nth history next-index) next-index)
      state)))

(defn selection-snapshot [state]
  (let [selected (set (:selected state))
        items (vec (filter #(contains? selected (:id %)) (:items state)))]
    (when (seq items)
      (let [min-x (apply min (map :x items))
            min-y (apply min (map :y items))
            ids (set (map :id items))
            items (mapv #(update % :x - min-x) items)
            items (mapv #(update % :y - min-y) items)
            wires (vec (filter (fn [wire]
                                 (and (contains? ids (:from wire))
                                      (contains? ids (:to wire))))
                               (:wires state)))]
        {:items items
         :wires wires
         :origin [min-x min-y]}))))

(defn- remap-item [item ids next-id]
  (let [new-id (get ids (:id item))]
    (assoc item :id new-id)))

(defn paste-snapshot [state snapshot dx dy]
  (if-let [items (:items snapshot)]
    (if (seq items)
      (let [next-id (:next-id state)
            ids (into {} (map-indexed (fn [index item]
                                        [(:id item) (+ next-id index)])
                                      items))
            items (mapv (fn [item]
                          (-> item
                              (remap-item ids next-id)
                              (update :x + dx)
                              (update :y + dy)))
                        items)
            next-wire-id (:next-wire-id state)
            wires (vec (map-indexed (fn [index wire]
                                      (assoc wire
                                             :id (+ next-wire-id index)
                                             :from (get ids (:from wire))
                                             :to (get ids (:to wire))))
                                    (:wires snapshot)))
            new-ids (set (vals ids))]
        (-> state
            (update :items into items)
            (update :wires into wires)
            (assoc :next-id (+ next-id (count items))
                   :next-wire-id (+ next-wire-id (count wires))
                   :selected new-ids
                   :context-menu nil
                   :clipboard snapshot)))
      state)
    state))

(defn copy-selection [state]
  (if-let [snapshot (selection-snapshot state)]
    (assoc state :clipboard snapshot)
    state))

(defn paste-clipboard [state dx dy]
  (if-let [snapshot (:clipboard state)]
    (paste-snapshot state snapshot dx dy)
    state))

(defn duplicate-selection [state]
  (if-let [snapshot (selection-snapshot state)]
    (paste-snapshot state snapshot 20 20)
    state))

(defn set-clipboard [state snapshot]
  (if snapshot
    (assoc state :clipboard snapshot)
    state))

(defn set-snap-size [state size]
  (if (and (number? size) (pos? size))
    (assoc state :snap-size size)
    state))

(defn set-theme [state theme]
  (assoc state :theme (if (= :dark theme) :dark :light)))

(defn toggle-theme [state]
  (set-theme state (if (= :dark (:theme state)) :light :dark)))

(defn set-component-search [state text]
  (assoc state :component-search (str (or text ""))))

(defn fit-view
  ([state] (fit-view state 800 600))
  ([state width height]
   (let [items (:items state)]
     (if (empty? items)
       (reset-view state)
       (let [bounds (map item-bounds items)
             x1 (apply min (map first bounds))
             y1 (apply min (map second bounds))
             x2 (apply max (map #(nth % 2) bounds))
             y2 (apply max (map #(nth % 3) bounds))
             margin 40
             content-width (max 1 (- x2 x1))
             content-height (max 1 (- y2 y1))
             zoom (clamp (min (/ (- width (* 2 margin)) content-width)
                              (/ (- height (* 2 margin)) content-height))
                         min-zoom
                         max-zoom)
             pan-x (- (/ width 2) (* zoom (/ (+ x1 x2) 2)))
             pan-y (- (/ height 2) (* zoom (/ (+ y1 y2) 2)))]
         (assoc state :view {:pan-x pan-x :pan-y pan-y :zoom zoom}))))))
