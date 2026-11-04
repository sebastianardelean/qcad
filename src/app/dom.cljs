(ns app.dom
  (:require [clojure.string :as str]
            [app.components :as components]
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

(defn- wire-line [wire from-anchor corners to-anchor draft?]
  (let [points (model/orthogonal-path (into [from-anchor]
                                            (concat corners [to-anchor])))]
    (svg-el "polyline" {:class (str "canvas-wire" (when draft? " canvas-wire-draft"))
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

(defn- render-wires [state items-by-id nodes-by-id]
  (let [svg (svg-el "svg" {:class "canvas-wires"
                           :aria-hidden "true"})
        item-at (fn [id] (get items-by-id id))
        node-at (fn [id] (get nodes-by-id id))
        wires (keep (fn [{:keys [from to points] :as wire}]
                      (when (and (item-at from) (item-at to))
                        (wire-line wire
                                   (wire-anchor (item-at from) (node-at from)
                                                (:from-port wire))
                                   points
                                   (wire-anchor (item-at to) (node-at to)
                                                (:to-port wire))
                                   false)))
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
                                  true)))]
    (append! svg (concat wires [draft-line (marquee-rect (:marquee state))]))
    svg))

(defn- render-ports [kind count]
  (append! (el "div" {:class (str "item-ports item-ports-" kind)})
           (map (fn [index]
                  (el "span" {:class (str "item-port item-port-" kind)
                              :attrs {:data-port-kind kind
                                      :data-port-index index
                                      :title (str (str/capitalize kind) " " index)}}))
                (range count))))

(defn- render-item [selected {:keys [id type component name qubits] :as item}]
  (let [selected? (contains? selected id)
        item-class (fn [kind]
                     (str "canvas-item canvas-" kind
                          (when selected? " selected")))]
    (case type
      :gate
      (let [node (el "div" {:class (item-class "gate")
                            :attrs {:data-item-id (str id)}
                            :style (item-style item)})
            ports (model/gate-port-count item)]
        (append! node [(render-ports "input" ports)
                       (el "span" {:class "item-label"
                                   :text (:label (components/by-id component))})
                       (render-ports "output" ports)])
        node)

      :register
      (let [node (el "div" {:class (item-class "register")
                            :attrs {:data-item-id (str id)}
                            :style (item-style item)})]
        (append! node [(el "span" {:class "item-label"
                                   :text (str name " (" (qubits-label qubits) ")")})
                       (render-ports "output" qubits)])
        node)

      nil)))

(defn render-component-tile [{:keys [id label kind]}]
  (el "div" {:class (str "component-tile component-" (name kind))
             :text label
             :attrs {:data-component-id (name id)
                     :draggable "true"
                     :role "listitem"
                     :title (str "Drag " label " onto the canvas")}}))

(defn render-components-panel []
  (let [panel (el "aside" {:class "components-panel"
                           :attrs {:id "components-panel"
                                   :aria-label "Components"}
                           :style {:width "20%"
                                   :flex-shrink "0"}})
        list (el "div" {:class "components-list"
                        :attrs {:id "components-list"
                                :role "list"}})]
    (append! panel
             [(el "h2" {:class "panel-title"
                        :text "Components"})
              (append! list (map render-component-tile components/components))])
    panel))

(defn- render-canvas-toolbar [{:keys [tool snap snap-size]}]
  (let [wire? (= :wire tool)]
    (append! (el "div" {:class "canvas-toolbar"})
             [(el "h2" {:class "panel-title canvas-title"
                        :text "Canvas"})
              (el "button" {:class (str "tool-button" (when wire? " active"))
                            :text (if wire? "Wire: on" "Wire")
                            :attrs {:type "button"
                                    :id "wire-tool"
                                    :aria-pressed (if wire? "true" "false")
                                    :title "Draw wires between components"}})
              (el "button" {:class (str "tool-button" (when snap " active"))
                            :text (if snap "Snap: on" "Snap: off")
                            :attrs {:type "button"
                                    :id "snap-toggle"
                                    :aria-pressed (if snap "true" "false")
                                    :title (str "Snap components to the " snap-size
                                                " px grid (G)")}})
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
    (append! canvas [(render-canvas-toolbar state)
                     (render-stage state item-nodes wires)
                     (render-status-bar state)])
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
                                      :id "context-rotate"}})]))))

(defn- render-error [errors]
  (el "p" {:class "dialog-error"
           :text (str/join " " (vals errors))
           :attrs {:role "alert"}}))

(defn- dialog-backdrop [title-id form]
  (append! (el "div" {:class "dialog-backdrop"})
           [(append! (el "div" {:class "dialog"
                                :attrs {:role "dialog"
                                        :aria-modal "true"
                                        :aria-labelledby title-id}})
                     [form])]))

(defn- render-register-dialog [dialog]
  (let [name-input (el "input" {:attrs {:id "register-name"
                                        :name "name"
                                        :type "text"
                                        :autocomplete "off"}})
        qubits-input (el "input" {:attrs {:id "register-qubits"
                                          :name "qubits"
                                          :type "number"
                                          :min "1"
                                          :step "1"}})
        actions (append! (el "div" {:class "dialog-actions"})
                         [(el "button" {:class "button-primary"
                                        :text "Add register"
                                        :attrs {:type "submit"
                                                :id "register-confirm"}})
                          (el "button" {:text "Cancel"
                                        :attrs {:type "button"
                                                :id "register-cancel"}})])
        form (append! (el "form" {:attrs {:id "register-form"
                                          :novalidate "novalidate"}})
                      [(el "h2" {:class "dialog-title"
                                 :text "Configure quantum register"
                                 :attrs {:id "register-dialog-title"}})
                       (el "label" {:text "Name"
                                    :attrs {:for "register-name"}})
                       name-input
                       (el "label" {:text "Number of qubits"
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
        qubits-input (el "input" {:attrs {:id "gate-qubits"
                                          :name "qubits"
                                          :type "number"
                                          :min "1"
                                          :step "1"}})
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
                       (when-let [error (:error dialog)]
                         (el "p" {:class "dialog-error"
                                  :text error
                                  :attrs {:role "alert"}}))
                       actions])]
    (set! (.-value qubits-input) (:qubits dialog))
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

(defn render-dialog [state]
  (when-let [dialog (:dialog state)]
    (case (:type dialog)
      :gate-qubits (render-gate-qubits-dialog state dialog)
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

(defn render-app [state]
  (append! (el "div" {:class "app"
                      :style {:display "flex"
                              :height "100vh"}})
           [(render-components-panel)
            (render-canvas state)
            (render-context-menu (:context-menu state))
            (render-dialog state)]))
