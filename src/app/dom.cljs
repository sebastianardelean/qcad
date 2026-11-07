(ns app.dom
  (:require [clojure.string :as str]
            [app.components :as components]
            [app.library :as library]
            [app.model :as model]))

(defn- el [tag {:keys [class text attrs style] :or {attrs {} style {}}}]
  (let [node (.createElement js/document tag)]
    (when class
      (.setAttribute node "class" class))
    (when (some? text)
      (set! (.-textContent node) text))
    (doseq [[key value] attrs]
      (when (some? value)
        (.setAttribute node (name key) (str value))))
    (doseq [[key value] style]
      (.setProperty (.-style node) (name key) (str value)))
    node))

(defn- append! [parent children]
  (doseq [child children
          node (if (sequential? child) child [child])]
    (when node
      (.appendChild parent node)))
  parent)

(defn- qubits-label [qubits]
  (str qubits " qubit" (when (not= 1 qubits) "s")))

(defn- bits-label [bits]
  (str bits " bit" (when (not= 1 bits) "s")))

(defn- component-label [item]
  (let [component (components/by-id (:component item))
        label (or (:label component)
                  (some-> (:component item) name)
                  "Gate")]
    (if (and (:parameterized component) (:angle item))
      (str label "(" (:angle item) ")")
      label)))

(defn- item-style [{:keys [x y rotation]}]
  (cond-> {:left (str x "px")
           :top (str y "px")}
    (pos? (or rotation 0))
    (assoc :transform (str "rotate(" rotation "deg)"))))

(def ^:private svg-ns "http://www.w3.org/2000/svg")

(def ^:private fallback-item-width 44)
(def ^:private fallback-item-height 36)

(defn- svg-el [tag attrs]
  (let [node (.createElementNS js/document svg-ns tag)]
    (doseq [[key value] attrs]
      (when (some? value)
        (.setAttribute node (name key) (str value))))
    node))

(defn- item-anchor [{:keys [x y]} node]
  (let [width (.-offsetWidth node)
        height (.-offsetHeight node)]
    (if (and (pos? width) (pos? height))
      [(+ (.-offsetLeft node) (/ width 2))
       (+ (.-offsetTop node) (/ height 2))]
      [(+ x (/ fallback-item-width 2))
       (+ y (/ fallback-item-height 2))])))

(defn- fallback-port-anchor [{:keys [x y width height type] :as item} kind index]
  (let [[default-width default-height] (if (= :register type)
                                         [160 fallback-item-height]
                                         [fallback-item-width fallback-item-height])
        width (or width default-width)
        height (or height default-height)
        count (max 1 (model/port-count item kind))
        x (if (= :output kind) (+ x width) x)
        y (+ y (* height (/ (inc index) (inc count))))]
    [x y]))

(defn- port-anchor [item node kind index]
  (let [port (when node
               (.querySelector node
                               (str ".item-port[data-port-kind=\"" (name kind)
                                    "\"][data-port-index=\"" index "\"]")))
        width (when node (.-offsetWidth node))
        height (when node (.-offsetHeight node))]
    (if (and port
             (pos? width)
             (pos? height)
             (pos? (.-offsetWidth port))
             (pos? (.-offsetHeight port)))
      [(+ (.-offsetLeft node)
          (.-offsetLeft port)
          (/ (.-offsetWidth port) 2))
       (+ (.-offsetTop node)
          (.-offsetTop port)
          (/ (.-offsetHeight port) 2))]
      (fallback-port-anchor item kind index))))

(defn- wire-anchor [item node port]
  (if port
    (port-anchor item node (:kind port) (:index port))
    (item-anchor item node)))

(defn- wire-line [wire from-anchor corners to-anchor draft? classical?]
  (let [points (model/orthogonal-path (into [from-anchor]
                                            (concat corners [to-anchor])))]
    (svg-el "polyline" {:class (str "canvas-wire"
                                    (when classical? " canvas-wire-classical")
                                    (when draft? " canvas-wire-draft"))
                        :id (when draft? "wire-draft")
                        :data-wire-id (:id wire)
                        :data-from (:from wire)
                        :data-to (:to wire)
                        :data-from-port (get-in wire [:from-port :index])
                        :data-to-port (get-in wire [:to-port :index])
                        :points (str/join " " (map (fn [[x y]] (str x "," y)) points))})))

(defn- marquee-rect [marquee]
  (when marquee
    (let [[x1 y1 x2 y2] (model/marquee-rect marquee)]
      (svg-el "rect" {:class "canvas-marquee"
                      :id "canvas-marquee"
                      :x x1
                      :y y1
                      :width (- x2 x1)
                      :height (- y2 y1)}))))

(defn- control-lines [items-by-id nodes-by-id]
  (mapcat
   (fn [[id item]]
     (when (= :gate (:type item))
       (let [controls (model/gate-control-count item)
             node (get nodes-by-id id)
             [tx ty] (item-anchor item node)]
         (for [index (range controls)]
           (let [[cx cy] (port-anchor item node :input index)]
             (svg-el "line" {:class "canvas-control-line"
                             :data-item-id id
                             :data-control-index index
                             :x1 cx
                             :y1 cy
                             :x2 tx
                             :y2 ty}))))))
   items-by-id))

(defn- render-wires [state items-by-id nodes-by-id]
  (let [svg (svg-el "svg" {:class "canvas-wires"
                           :aria-hidden "true"})
        item-at (fn [id] (get items-by-id id))
        node-at (fn [id] (get nodes-by-id id))
        classical-wire? (fn [from to]
                          (or (model/classical-register? (item-at from))
                              (model/classical-register? (item-at to))))
        wires (keep (fn [{:keys [from to points] :as wire}]
                      (when (and (item-at from) (item-at to))
                        (wire-line wire
                                   (wire-anchor (item-at from) (node-at from)
                                                (:from-port wire))
                                   points
                                   (wire-anchor (item-at to) (node-at to)
                                                (:to-port wire))
                                   false
                                   (classical-wire? from to))))
                    (:wires state))
        draft (:wire-draft state)
        draft-line (when-let [from (:from draft)]
                     (when-let [item (item-at from)]
                       (wire-line {:from from
                                   :from-port (:from-port draft)}
                                  (wire-anchor item (node-at from)
                                               (:from-port draft))
                                  (:points draft)
                                  [(:x draft) (:y draft)]
                                  true
                                  (model/classical-register? item))))]
    (append! svg (concat wires
                         (control-lines items-by-id nodes-by-id)
                         [draft-line (marquee-rect (:marquee state))]))
    svg))

(defn- render-ports
  ([kind count] (render-ports kind count 0))
  ([kind count controls]
   (append! (el "div" {:class (str "item-ports item-ports-" kind)})
            (map (fn [index]
                   (el "span" {:class (str "item-port item-port-" kind
                                           (when (and (= "input" kind)
                                                      (< index controls))
                                             " item-port-control"))
                               :attrs {:data-port-kind kind
                                       :data-port-index index
                                       :title (str (str/capitalize kind) " " index)}}))
                 (range count)))))

(defn- render-item [selected {:keys [id type component name qubits text] :as item}]
  (let [selected? (contains? selected id)
        classical? (model/classical-register? item)
        item-class (fn [kind]
                     (str "canvas-item canvas-" kind
                          (when classical? " canvas-classical-register")
                          (when selected? " selected")))]
    (case type
      :gate
      (let [node (el "div" {:class (str (item-class "gate")
                                        (when (components/barrier? component)
                                          " canvas-barrier")
                                        (when (components/measure? component)
                                          " canvas-measure")
                                        (when (components/reset? component)
                                          " canvas-reset"))
                            :attrs {:data-item-id (str id)}
                            :style (item-style item)})
            ports (model/gate-port-count item)
            controls (model/gate-control-count item)]
        (append! node [(render-ports "input" ports controls)
                       (el "span" {:class "item-label"
                                   :text (component-label item)})
                       (render-ports "output" ports)])
        node)

      :register
      (let [node (el "div" {:class (item-class "register")
                            :attrs {:data-item-id (str id)}
                            :style (item-style item)})
            label (str name " ("
                       (if classical?
                         (bits-label qubits)
                         (qubits-label qubits))
                       ")")]
        (append! node (cond-> [(el "span" {:class "item-label" :text label})
                               (render-ports "output" (if classical? 0 qubits))]
                       classical? (conj (render-ports "input" qubits))))
        node)

      :annotation
      (let [node (el "div" {:class (item-class "annotation")
                            :attrs {:data-item-id (str id)}
                            :style (merge (item-style item)
                                          {:width (str (or (:width item) 140) "px")
                                           :height (str (or (:height item) 30) "px")})})]
        (append! node [(el "span" {:class "item-label" :text text})])
        node)

      :box
      (let [node (el "div" {:class (item-class "box")
                            :attrs {:data-item-id (str id)}
                            :style (merge (item-style item)
                                          {:width (str (or (:width item) 220) "px")
                                           :height (str (or (:height item) 140) "px")})})]
        (append! node [(el "span" {:class "item-label box-label" :text text})])
        node)

      nil)))

(defn render-component-tile [{:keys [id label kind]}]
  (el "div" {:class (str "component-tile component-" (name kind))
             :text label
             :attrs {:data-component-id (name id)
                     :draggable "true"
                     :role "listitem"
                     :title (str "Drag " label " onto the canvas")}}))

(defn render-components-panel
  ([] (render-components-panel {}))
  ([{:keys [component-search]}]
   (let [panel (el "aside" {:class "components-panel"
                            :attrs {:id "components-panel"
                                    :aria-label "Components"}
                            :style {:width "20%"
                                    :flex-shrink "0"}})
         search (el "input" {:class "component-search"
                             :attrs {:id "component-search"
                                     :type "search"
                                     :placeholder "Search components"
                                     :autocomplete "off"
                                     :value component-search}})
         query (str/lower-case (str (or component-search "")))
         matches (filter #(str/includes? (str/lower-case (:label %)) query)
                         components/components)
         list (el "div" {:class "components-list"
                         :attrs {:id "components-list"
                                 :role "list"}})]
     (.setAttribute search "value" (or component-search ""))
     (set! (.-value search) (or component-search ""))
     (append! panel
              [(el "h2" {:class "panel-title"
                         :text "Components"})
               search
               (append! list (map render-component-tile matches))])
     panel)))

(defn- render-toolbar-button [id label title & [active disabled]]
  (el "button" {:class (str "tool-button" (when active " active"))
                :text label
                :attrs {:type "button"
                        :id id
                        :title title
                        :aria-pressed (when (some? active) (if active "true" "false"))
                        :disabled (when disabled "disabled")}}))

(defn- render-grid-select [snap-size]
  (let [select (el "select" {:class "tool-select"
                             :attrs {:id "grid-size-select"
                                     :title "Grid size"}})]
    (doseq [size [10 20 40 80]]
      (let [option (el "option" {:text (str size " px")
                                 :attrs {:value size}})]
        (when (= size snap-size)
          (set! (.-selected option) true))
        (.appendChild select option)))
    select))

(defn- render-toolbar [state]
  (let [{:keys [tool snap snap-components snap-size theme]} state
        wire? (= :wire tool)
        dark? (= :dark theme)]
    (append! (el "div" {:class "app-toolbar"})
             [(el "h2" {:class "panel-title app-title"
                        :text "QCAD"})
              (render-toolbar-button "wire-tool"
                                     (if wire? "Wire: on" "Wire")
                                     "Draw wires between components"
                                     wire?)
              (render-toolbar-button "snap-toggle"
                                     (if snap "Snap: on" "Snap: off")
                                     (str "Snap components to the " snap-size
                                          " px grid (G)")
                                     snap)
              (render-toolbar-button "component-snap-toggle"
                                     (if snap-components "Align: on" "Align")
                                     "Snap to nearby components (A)"
                                     snap-components)
              (render-grid-select snap-size)
              (render-toolbar-button "undo-button" "Undo"
                                     "Undo (Ctrl+Z)" nil
                                     (not (model/can-undo? state)))
              (render-toolbar-button "redo-button" "Redo"
                                     "Redo (Ctrl+Y)" nil
                                     (not (model/can-redo? state)))
              (render-toolbar-button "zoom-fit" "Fit" "Zoom to fit (F)")
              (render-toolbar-button "library-button" "Library"
                                     "Insert a circuit template")
              (render-toolbar-button "export-button" "Export"
                                     "Export the circuit as an image")
              (render-toolbar-button "properties-button" "Properties"
                                     "Edit circuit metadata")
              (render-toolbar-button "diff-button" "Diff"
                                     "Compare with the last saved version")
              (render-toolbar-button "theme-toggle"
                                     (if dark? "Light mode" "Dark mode")
                                     "Toggle dark mode")
              (when wire?
                (el "span" {:class "canvas-hint"
                            :text (str "Click an output port to start; click the canvas "
                                       "to add a corner; click an input port to finish.")}))])))

(defn- node-item-id [node]
  (some-> node
          (.getAttribute "data-item-id")
          (js/parseInt 10)))

(defn- render-stage [state item-nodes wires]
  (let [{:keys [pan-x pan-y zoom]} (:view state)
        stage (el "div" {:class "canvas-stage"
                         :attrs {:id "canvas-stage"}
                         :style {:position "absolute"
                                 :inset "0"
                                 :transform-origin "0 0"
                                 :transform (str "translate(" pan-x "px, " pan-y "px) "
                                                 "scale(" zoom ")")}})]
    (append! stage (concat item-nodes [wires]))))

(defn- render-status-bar [{:keys [cursor view snap snap-size]}]
  (let [{:keys [zoom]} view
        coordinates (if cursor
                      (str "X " (js/Math.round (first cursor))
                           "   Y " (js/Math.round (second cursor)))
                      "X --   Y --")]
    (append! (el "div" {:class "canvas-status"
                        :attrs {:id "canvas-status"
                                :aria-live "off"}})
             [(el "span" {:class "canvas-status-item"
                          :text coordinates
                          :attrs {:id "cursor-coordinates"}})
              (el "span" {:class "canvas-status-item"
                          :text (str "Zoom " (js/Math.round (* 100 zoom)) "%")
                          :attrs {:id "zoom-level"}})
              (el "span" {:class "canvas-status-item"
                          :text (str "Grid " snap-size)
                          :attrs {:id "grid-size"}})
              (el "span" {:class "canvas-status-item"
                          :text (if snap "Snap on" "Snap off")
                          :attrs {:id "snap-state"}})])))

(defn- render-minimap [state]
  (let [items (:items state)
        bounds (map model/item-bounds items)
        x1 (if (seq bounds) (apply min (map first bounds)) 0)
        y1 (if (seq bounds) (apply min (map second bounds)) 0)
        x2 (if (seq bounds) (apply max (map #(nth % 2) bounds)) 100)
        y2 (if (seq bounds) (apply max (map #(nth % 3) bounds)) 100)
        width (max 1 (- x2 x1))
        height (max 1 (- y2 y1))
        scale (min (/ 150 width) (/ 90 height))
        svg (svg-el "svg" {:class "canvas-minimap"
                           :id "minimap"
                           :width 160
                           :height 100
                           :viewBox "0 0 160 100"})
        offset-x (+ 5 (/ (- 150 (* width scale)) 2))
        offset-y (+ 5 (/ (- 90 (* height scale)) 2))]
    (append! svg
             (concat
              [(svg-el "rect" {:x 0 :y 0 :width 160 :height 100
                               :fill "rgba(255,255,255,0.9)"
                               :stroke "#cbd2d9"})]
              (map (fn [item]
                     (let [[ix iy ix2 iy2] (model/item-bounds item)]
                       (svg-el "rect"
                               {:x (+ offset-x (* (- ix x1) scale))
                                :y (+ offset-y (* (- iy y1) scale))
                                :width (max 1 (* (- ix2 ix) scale))
                                :height (max 1 (* (- iy2 iy) scale))
                                :fill (cond
                                        (model/classical-register? item) "#805ad5"
                                        (= :register (:type item)) "#2f855a"
                                        (= :annotation (:type item)) "#f6ad55"
                                        (= :box (:type item)) "#e9d8fd"
                                        :else "#2b6cb0")
                                :opacity 0.7})))
                   items)))
    svg))

(defn render-canvas [state]
  (let [wire? (= :wire (:tool state))
        {:keys [pan-x pan-y zoom]} (:view state)
        snap? (:snap state)
        grid (:snap-size state)
        canvas (el "main" {:class (str "canvas-panel"
                                       (when wire? " canvas-wire-mode")
                                       (when snap? " canvas-snap-on"))
                           :attrs {:id "canvas-panel"
                                   :aria-label "Canvas"}
                           :style {:flex-grow "1"
                                   :flex-shrink "1"
                                   :flex-basis "0"
                                   :position "relative"
                                   :overflow "hidden"
                                   :background-position (str pan-x "px " pan-y "px")
                                   :background-size (str (* grid zoom) "px "
                                                         (* grid zoom) "px")}})
        items (:items state)
        item-nodes (vec (keep #(render-item (:selected state) %) items))
        nodes-by-id (into {} (keep (fn [node]
                                     (when-let [id (node-item-id node)]
                                       [id node]))
                                   item-nodes))
        items-by-id (into {} (map (juxt :id identity)) items)
        wires (render-wires state items-by-id nodes-by-id)]
    (append! canvas [(render-stage state item-nodes wires)
                     (render-status-bar state)
                     (render-minimap state)])
    canvas))

(defn render-context-menu [menu]
  (when menu
    (let [node (el "div" {:class "context-menu"
                          :attrs {:id "context-menu"
                                  :role "menu"}
                          :style {:position "fixed"
                                  :left (str (:x menu) "px")
                                  :top (str (:y menu) "px")}})]
      (append! node
               [(el "button" {:class "context-menu-item"
                              :text "Delete (Esc)"
                              :attrs {:type "button"
                                      :role "menuitem"
                                      :id "context-delete"}})
                (el "button" {:class "context-menu-item"
                              :text "Rotate (R)"
                              :attrs {:type "button"
                                      :role "menuitem"
                                      :id "context-rotate"}})
                (el "button" {:class "context-menu-item"
                              :text "Toggle control"
                              :attrs {:type "button"
                                      :role "menuitem"
                                      :id "context-control"}})]))))

(defn- render-error [errors]
  (el "p" {:class "dialog-error"
           :text (if (map? errors)
                   (str/join " " (vals errors))
                   (str errors))
           :attrs {:role "alert"}}))

(defn- dialog-backdrop [title-id form]
  (append! (el "div" {:class "dialog-backdrop"})
           [(append! (el "div" {:class "dialog"
                                :attrs {:role "dialog"
                                        :aria-modal "true"
                                        :aria-labelledby title-id}})
                     [form])]))

(defn- render-register-dialog [dialog]
  (let [classical? (:classical dialog)
        name-input (el "input" {:attrs {:id "register-name"
                                        :name "name"
                                        :type "text"
                                        :autocomplete "off"}})
        qubits-input (el "input" {:attrs {:id "register-qubits"
                                          :name "qubits"
                                          :type "number"
                                          :min "1"
                                          :step "1"}})
        actions (append! (el "div" {:class "dialog-actions"})
                         [                          (el "button" {:class "button-primary"
                                        :text (if classical?
                                                "Add classical register"
                                                "Add register")
                                        :attrs {:type "submit"
                                                :id "register-confirm"}})
                          (el "button" {:text "Cancel"
                                        :attrs {:type "button"
                                                :id "register-cancel"}})])
        form (append! (el "form" {:attrs {:id "register-form"
                                          :novalidate "novalidate"}})
                       [(el "h2" {:class "dialog-title"
                                  :text (if classical?
                                          "Configure classical register"
                                          "Configure quantum register")
                                  :attrs {:id "register-dialog-title"}})
                       (el "label" {:text "Name"
                                     :attrs {:for "register-name"}})
                       name-input
                       (el "label" {:text (if classical?
                                            "Number of bits"
                                            "Number of qubits")
                                     :attrs {:for "register-qubits"}})
                       qubits-input
                       (when-let [errors (:error dialog)]
                         (render-error errors))
                       actions])]
    (set! (.-value name-input) (:name dialog))
    (set! (.-value qubits-input) (:qubits dialog))
    (dialog-backdrop "register-dialog-title" form)))

(defn- render-gate-qubits-dialog [state dialog]
  (let [gate (model/item-by-id state (:gate-id dialog))
        gate-label (or (:label (components/by-id (:component gate)))
                       (some-> (:component gate) name)
                       "gate")
        parameterized? (components/parameterized? (:component gate))
        qubits-input (el "input" {:attrs {:id "gate-qubits"
                                          :name "qubits"
                                          :type "number"
                                          :min "1"
                                          :step "1"}})
        angle-input (el "input" {:attrs {:id "gate-angle"
                                         :name "angle"
                                         :type "text"
                                         :autocomplete "off"
                                         :placeholder model/default-angle}})
        controls-input (el "input" {:attrs {:id "gate-controls"
                                            :name "controls"
                                            :type "number"
                                            :min "0"
                                            :step "1"}})
        presets (el "div" {:class "angle-presets"})
        actions (append! (el "div" {:class "dialog-actions"})
                         [(el "button" {:class "button-primary"
                                        :text "Apply"
                                        :attrs {:type "submit"
                                                :id "gate-qubits-confirm"}})
                          (el "button" {:text "Cancel"
                                        :attrs {:type "button"
                                                :id "gate-qubits-cancel"}})])
        form (append! (el "form" {:attrs {:id "gate-qubits-form"
                                          :novalidate "novalidate"}})
                      [(el "h2" {:class "dialog-title"
                                 :text (str "Configure " gate-label " gate")
                                 :attrs {:id "gate-qubits-dialog-title"}})
                       (el "p" {:class "dialog-help"
                                :text (str "Set the number of qubit ports. Wire each "
                                           "input port to the qubit it should act on.")})
                       (el "label" {:text "Number of qubits"
                                    :attrs {:for "gate-qubits"}})
                       qubits-input
                       (when parameterized?
                         (el "label" {:text "Angle" :attrs {:for "gate-angle"}}))
                       (when parameterized? angle-input)
                       (when parameterized?
                         (append! presets
                                  (map (fn [preset]
                                         (el "button" {:class "button-preset"
                                                       :text preset
                                                       :attrs {:type "button"
                                                               :data-angle preset}}))
                                       model/angle-presets)))
                       (el "label" {:text "Control qubits"
                                    :attrs {:for "gate-controls"}})
                       controls-input
                       (when-let [error (:error dialog)]
                         (el "p" {:class "dialog-error"
                                  :text error
                                  :attrs {:role "alert"}}))
                       actions])]
    (set! (.-value qubits-input) (:qubits dialog))
    (set! (.-value angle-input) (or (:angle dialog) model/default-angle))
    (set! (.-value controls-input) (or (:controls dialog) "0"))
    (dialog-backdrop "gate-qubits-dialog-title" form)))

(defn- render-custom-dialog [dialog]
  (let [editing? (some? (:item-id dialog))
        qubits-input (el "input" {:attrs {:id "custom-qubits"
                                          :name "qubits"
                                          :type "number"
                                          :min "1"
                                          :step "1"}})
        qasm-input (el "textarea" {:attrs {:id "custom-qasm"
                                           :name "qasm"
                                           :rows "7"
                                           :spellcheck "false"
                                           :placeholder "gate pair a, b {\n  h a;\n  cx a, b;\n}"}})
        actions (append! (el "div" {:class "dialog-actions"})
                         [(el "button" {:class "button-primary"
                                        :text (if editing? "Save gate" "Add gate")
                                        :attrs {:type "submit"
                                                :id "custom-confirm"}})
                          (el "button" {:text "Cancel"
                                        :attrs {:type "button"
                                                :id "custom-cancel"}})])
        form (append! (el "form" {:attrs {:id "custom-form"
                                          :novalidate "novalidate"}})
                      [(el "h2" {:class "dialog-title"
                                 :text (if editing? "Edit custom gate"
                                         "Define custom gate")
                                 :attrs {:id "custom-dialog-title"}})
                       (el "p" {:class "dialog-help"
                                :text (str "Enter an OpenQASM gate declaration. The gate "
                                           "is called with the qubits wired to its input ports.")})
                       (el "label" {:text "Number of qubits"
                                    :attrs {:for "custom-qubits"}})
                       qubits-input
                       (el "label" {:text "Gate definition (OpenQASM)"
                                    :attrs {:for "custom-qasm"}})
                       qasm-input
                       (when-let [error (:error dialog)]
                         (render-error error))
                       actions])]
    (set! (.-value qubits-input) (:qubits dialog))
    (set! (.-value qasm-input) (:qasm dialog))
    (dialog-backdrop "custom-dialog-title" form)))

(defn- render-annotation-dialog [dialog]
  (let [editing? (some? (:item-id dialog))
        textarea (el "textarea" {:attrs {:id "annotation-text"
                                         :name "text"
                                         :rows "4"
                                         :placeholder "Annotation text"}})
        actions (append! (el "div" {:class "dialog-actions"})
                         [(el "button" {:class "button-primary"
                                        :text (if editing? "Save" "Add annotation")
                                        :attrs {:type "submit"
                                                :id "annotation-confirm"}})
                          (el "button" {:text "Cancel"
                                        :attrs {:type "button"
                                                :id "annotation-cancel"}})])
        form (append! (el "form" {:attrs {:id "annotation-form"
                                          :novalidate "novalidate"}})
                      [(el "h2" {:class "dialog-title"
                                 :text (if editing? "Edit annotation"
                                         "Add annotation")
                                 :attrs {:id "annotation-dialog-title"}})
                       (el "label" {:text "Text" :attrs {:for "annotation-text"}})
                       textarea
                       (when-let [error (:error dialog)]
                         (el "p" {:class "dialog-error"
                                  :text error
                                  :attrs {:role "alert"}}))
                       actions])]
    (set! (.-value textarea) (:text dialog))
    (dialog-backdrop "annotation-dialog-title" form)))

(defn- render-metadata-dialog [dialog]
  (let [metadata (:metadata dialog)
        name-input (el "input" {:attrs {:id "metadata-name"
                                        :name "name"
                                        :type "text"
                                        :autocomplete "off"}})
        author-input (el "input" {:attrs {:id "metadata-author"
                                          :name "author"
                                          :type "text"
                                          :autocomplete "off"}})
        description-input (el "textarea" {:attrs {:id "metadata-description"
                                                  :name "description"
                                                  :rows "4"}})
        actions (append! (el "div" {:class "dialog-actions"})
                         [(el "button" {:class "button-primary"
                                        :text "Save metadata"
                                        :attrs {:type "submit"
                                                :id "metadata-confirm"}})
                          (el "button" {:text "Cancel"
                                        :attrs {:type "button"
                                                :id "metadata-cancel"}})])
        form (append! (el "form" {:attrs {:id "metadata-form"
                                          :novalidate "novalidate"}})
                      [(el "h2" {:class "dialog-title"
                                 :text "Circuit metadata"
                                 :attrs {:id "metadata-dialog-title"}})
                       (el "label" {:text "Name" :attrs {:for "metadata-name"}})
                       name-input
                       (el "label" {:text "Author" :attrs {:for "metadata-author"}})
                       author-input
                       (el "label" {:text "Description"
                                    :attrs {:for "metadata-description"}})
                       description-input
                       actions])]
    (set! (.-value name-input) (or (:name metadata) ""))
    (set! (.-value author-input) (or (:author metadata) ""))
    (set! (.-value description-input) (or (:description metadata) ""))
    (dialog-backdrop "metadata-dialog-title" form)))

(defn- render-export-dialog [dialog]
  (let [format-select (el "select" {:attrs {:id "export-format"
                                            :name "format"}})
        scale-select (el "select" {:attrs {:id "export-scale"
                                           :name "scale"}})
        transparent-input (el "input" {:attrs {:id "export-transparent"
                                               :name "transparent"
                                               :type "checkbox"}})
        include-ui-input (el "input" {:attrs {:id "export-include-ui"
                                              :name "include-ui"
                                              :type "checkbox"}})
        actions (append! (el "div" {:class "dialog-actions"})
                         [(el "button" {:class "button-primary"
                                        :text "Export"
                                        :attrs {:type "button"
                                                :id "export-confirm"}})
                          (el "button" {:text "Cancel"
                                        :attrs {:type "button"
                                                :id "export-cancel"}})])
        form (append! (el "form" {:attrs {:id "export-form"
                                          :novalidate "novalidate"}})
                      [(el "h2" {:class "dialog-title"
                                 :text "Export circuit"
                                 :attrs {:id "export-dialog-title"}})
                       (el "p" {:class "dialog-help"
                                :text "The exported image reflects the current canvas."})
                       (el "label" {:text "Format" :attrs {:for "export-format"}})
                       format-select
                       (el "label" {:text "Resolution" :attrs {:for "export-scale"}})
                       scale-select
                       (el "label" {:class "checkbox-label"
                                    :text "Transparent background"
                                    :attrs {:for "export-transparent"}})
                       transparent-input
                       (el "label" {:class "checkbox-label"
                                    :text "Include canvas grid"
                                    :attrs {:for "export-include-ui"}})
                       include-ui-input
                       (when-let [error (:error dialog)]
                         (el "p" {:class "dialog-error"
                                  :text error
                                  :attrs {:role "alert"}}))
                       actions])]
    (doseq [[value label] [["svg" "SVG"] ["png" "PNG"]]]
      (let [option (el "option" {:text label :attrs {:value value}})]
        (when (= value (:format dialog))
          (set! (.-selected option) true))
        (.appendChild format-select option)))
    (doseq [[value label] [[1 "1x"] [2 "2x"] [3 "3x"]]]
      (let [option (el "option" {:text label :attrs {:value value}})]
        (when (= value (or (:scale dialog) 1))
          (set! (.-selected option) true))
        (.appendChild scale-select option)))
    (set! (.-checked transparent-input) (boolean (:transparent dialog)))
    (set! (.-checked include-ui-input) (boolean (:include-ui dialog)))
    (dialog-backdrop "export-dialog-title" form)))

(defn- render-library-entry [name]
  (append! (el "li" {:class "library-entry"
                     :attrs {:draggable "true"
                             :data-block-library name
                             :title "Drag onto the canvas to insert as a block"}})
           [(el "span" {:class "library-name" :text name})
            (el "button" {:class "button-small"
                          :text "Insert"
                          :attrs {:type "button"
                                  :data-library-name name}})
            (el "button" {:class "button-small button-danger"
                          :text "Delete"
                          :attrs {:type "button"
                                  :data-library-delete name}})]))

(defn- render-library-dialog [dialog]
  (let [name-input (el "input" {:attrs {:id "library-name"
                                        :name "name"
                                        :type "text"
                                        :autocomplete "off"}})
        templates-list (append! (el "ul" {:class "library-list"
                                          :attrs {:id "library-template-list"}})
                                (map (fn [template]
                                       (append! (el "li" {:class "library-entry"
                                                          :attrs {:draggable "true"
                                                                  :data-block-template (name (:id template))
                                                                  :title "Drag onto the canvas to insert as a block"}})
                                                [(el "span" {:class "library-name"
                                                             :text (:name template)})
                                                 (el "span" {:class "library-description"
                                                             :text (:description template)})
                                                 (el "button" {:class "button-small"
                                                               :text "Insert"
                                                               :attrs {:type "button"
                                                                       :data-template-id (name (:id template))}})]))
                                     library/templates))
        user-list (append! (el "ul" {:class "library-list"
                                     :attrs {:id "library-user-list"}})
                           (map (fn [entry]
                                  (render-library-entry (:name entry)))
                                (library/user-circuits)))
        actions (append! (el "div" {:class "dialog-actions"})
                         [(el "button" {:text "Close"
                                        :attrs {:type "button"
                                                :id "library-cancel"}})])
        form (append! (el "form" {:attrs {:id "library-form"
                                          :novalidate "novalidate"}})
                      [(el "h2" {:class "dialog-title"
                                 :text "Circuit library"
                                 :attrs {:id "library-dialog-title"}})
                       (el "h3" {:class "dialog-section-title" :text "Templates"})
                       templates-list
                       (el "h3" {:class "dialog-section-title" :text "Saved circuits"})
                       user-list
                       (el "label" {:text "Save current circuit as"
                                    :attrs {:for "library-name"}})
                       name-input
                       (el "button" {:class "button-secondary"
                                     :text "Save to library"
                                     :attrs {:type "button"
                                             :id "library-save"}})
                       (when-let [error (:error dialog)]
                         (render-error error))
                       actions])]
    (set! (.-value name-input) (or (:name dialog) ""))
    (dialog-backdrop "library-dialog-title" form)))

(defn- render-diff-dialog [dialog]
  (let [lines (or (:diff dialog) [])
        actions (append! (el "div" {:class "dialog-actions"})
                         [(el "button" {:class "button-primary"
                                        :text "Close"
                                        :attrs {:type "button"
                                                :id "diff-close"}})])
        form (append! (el "form" {:attrs {:id "diff-form"
                                          :novalidate "novalidate"}})
                      [(el "h2" {:class "dialog-title"
                                 :text "Changes since last save"
                                 :attrs {:id "diff-dialog-title"}})
                       (el "pre" {:class "diff-content"
                                  :text (str/join "\n" lines)
                                  :attrs {:id "diff-content"}})
                       actions])]
    (dialog-backdrop "diff-dialog-title" form)))

(defn render-dialog [state]
  (when-let [dialog (:dialog state)]
    (case (:type dialog)
      :gate-qubits (render-gate-qubits-dialog state dialog)
      :annotation (render-annotation-dialog dialog)
      :metadata (render-metadata-dialog dialog)
      :export (render-export-dialog dialog)
      :library (render-library-dialog dialog)
      :diff (render-diff-dialog dialog)
      :custom-gate (render-custom-dialog dialog)
      (render-register-dialog dialog))))

(defn refresh-wires! [root state]
  (when-let [stage (.querySelector root "#canvas-stage")]
    (when-let [old-wires (.querySelector stage ".canvas-wires")]
      (let [item-nodes (vec (array-seq (.querySelectorAll stage ".canvas-item")))
            nodes-by-id (into {} (keep (fn [node]
                                         (when-let [id (node-item-id node)]
                                           [id node])))
                               item-nodes)
            items-by-id (into {} (map (juxt :id identity)) (:items state))]
        (.replaceChild stage
                       (render-wires state items-by-id nodes-by-id)
                       old-wires)))))

(defn- property-row [label value]
  (when (some? value)
    (append! (el "div" {:class "property-row"})
             [(el "span" {:class "property-label" :text label})
              (el "span" {:class "property-value" :text (str value)})])))

(defn- item-property-rows [item]
  (let [component (components/by-id (:component item))]
    [(property-row "Type" (name (or (:type item) :unknown)))
     (property-row "Component" (or (:label component)
                                   (some-> (:component item) name)))
     (property-row "Name" (:name item))
     (property-row "Qubits" (:qubits item))
     (property-row "Angle" (:angle item))
     (property-row "Controls" (when (pos? (model/gate-control-count item))
                                (model/gate-control-count item)))
     (property-row "Rotation" (when (pos? (or (:rotation item) 0))
                                (str (:rotation item) "°")))
     (property-row "Position" (str (:x item) ", " (:y item)))
     (property-row "Text" (:text item))]))

(defn- render-property-panel [state]
  (let [selected (vec (:selected state))
        items (keep #(model/item-by-id state %) selected)
        metadata (:metadata state)
        panel (el "aside" {:class "property-panel"
                           :attrs {:id "property-panel"
                                   :aria-label "Properties"}
                           :style {:width "20%"
                                   :flex-shrink "0"}})]
    (append! panel
             (concat
              [(el "h2" {:class "panel-title" :text "Properties"})]
              (cond
                (empty? items)
                [(el "p" {:class "property-empty"
                          :text "Select a component to inspect it."})
                 (el "h3" {:class "dialog-section-title" :text "Circuit"})
                 (property-row "Name" (:name metadata))
                 (property-row "Author" (:author metadata))
                 (property-row "Description" (:description metadata))
                 (property-row "Includes" (str/join ", " (:includes state)))
                 (property-row "Items" (count (:items state)))
                 (property-row "Wires" (count (:wires state)))]

                (= 1 (count items))
                (let [item (first items)]
                  (concat
                   (item-property-rows item)
                   [(el "button" {:class "button-secondary"
                                  :text (if (= :annotation (:type item))
                                          "Edit annotation"
                                          "Configure gate")
                                  :attrs {:type "button"
                                          :id "property-edit"}})]))

                :else
                [(el "p" {:class "property-empty"
                          :text (str (count items) " components selected")})])))
    panel))

(defn render-app [state]
  (append! (el "div" {:class (str "app" (when (= :dark (:theme state))
                                         " theme-dark"))
                      :style {:display "flex"
                              :flex-direction "column"
                              :height "100vh"}})
           [(render-toolbar state)
            (append! (el "div" {:class "app-main"
                                :style {:display "flex"
                                        :flex "1 1 0"
                                        :min-height "0"}})
                     [(render-components-panel state)
                      (render-canvas state)
                      (render-property-panel state)])
            (render-context-menu (:context-menu state))
            (render-dialog state)]))
