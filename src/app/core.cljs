(ns app.core
  (:require [app.dom :as dom]
            [app.model :as model]
            [app.qasm :as qasm]))

(defn- render! [root state]
  (set! (.-innerHTML root) "")
  (.appendChild root (dom/render-app state))
  (dom/refresh-wires! root state))

(defn- save-qasm! [state]
  (let [api (.-electronAPI js/window)]
    (if-let [save (and api (.-saveQasm api))]
      (.catch (.saveQasm api (qasm/to-qasm @state) "circuit.qasm")
              (fn [error]
                (js/console.error "Could not save OpenQASM." error)))
      (js/console.warn "Saving requires the Electron application."))))

(defn- subscribe-menu-save! [state]
  (when-let [api (.-electronAPI js/window)]
    (when-let [on-menu-save (.-onMenuSave api)]
      (on-menu-save #(save-qasm! state)))))

(defn- open-qasm! [state root]
  (let [api (.-electronAPI js/window)]
    (if-let [open (and api (.-openQasm api))]
      (-> (.openQasm api)
          (.then (fn [^js result]
                   (when (and result
                              (not (.-canceled result))
                              (string? (.-content result)))
                     (reset! state (qasm/from-qasm (.-content result)))
                     (render! root @state))))
          (.catch (fn [error]
                    (js/console.error "Could not open OpenQASM." error))))
      (js/console.warn "Opening requires the Electron application."))))

(defn- subscribe-menu-open! [state root]
  (when-let [api (.-electronAPI js/window)]
    (when-let [on-menu-open (.-onMenuOpen api)]
      (on-menu-open #(open-qasm! state root)))))

(defn- closest [event selector]
  (some-> (.-target event) (.closest selector)))

(defn- handle-dragstart! [event]
  (when-let [tile (closest event "[data-component-id]")]
    (when-let [data-transfer (.-dataTransfer event)]
      (.setData data-transfer
                "application/x-qcad-component"
                (.getAttribute tile "data-component-id"))
      (set! (.-effectAllowed data-transfer) "copy"))))

(defn- dragged-component-id [event]
  (some-> (.-dataTransfer event)
          (.getData "application/x-qcad-component")
          not-empty
          keyword))

(defn- canvas-node [root]
  (.querySelector root "#canvas-panel"))

(defn- screen-point [root event]
  (when-let [canvas (canvas-node root)]
    (let [rect (.getBoundingClientRect canvas)]
      [(- (.-clientX event) (.-left rect))
       (- (.-clientY event) (.-top rect))])))

(defn- world-point [state root event]
  (when-let [point (screen-point root event)]
    (model/screen->world (:view @state) point)))

(defn- handle-dragover! [event]
  (when (closest event "#canvas-panel")
    (.preventDefault event)
    (when-let [data-transfer (.-dataTransfer event)]
      (set! (.-dropEffect data-transfer) "copy"))))

(defn- handle-drop! [state root event]
  (when (closest event "#canvas-panel")
    (.preventDefault event)
    (when-let [component-id (dragged-component-id event)]
      (when-let [[x y] (world-point state root event)]
        (swap! state model/handle-drop component-id x y)
        (render! root @state)))))

(defn- handle-submit! [state root event]
  (when-let [form (closest event "form")]
    (.preventDefault event)
    (case (.-id form)
      "register-form"
      (let [name (.-value (.querySelector form "#register-name"))
            qubits (.-value (.querySelector form "#register-qubits"))]
        (swap! state model/confirm-register name qubits)
        (render! root @state))

      "gate-qubits-form"
      (let [qubits (.-value (.querySelector form "#gate-qubits"))]
        (swap! state model/confirm-gate-qubits qubits)
        (render! root @state))

      "custom-form"
      (let [qubits (.-value (.querySelector form "#custom-qubits"))
            qasm (.-value (.querySelector form "#custom-qasm"))]
        (swap! state model/confirm-custom-gate qubits qasm)
        (render! root @state))

      nil)))

(defn- item-id [event]
  (some-> (closest event "[data-item-id]")
          (.getAttribute "data-item-id")
          (js/parseInt 10)))

(defn- port-info [event]
  (when-let [node (closest event ".item-port")]
    (let [kind (some-> node (.getAttribute "data-port-kind") keyword)
          index (js/parseInt (.getAttribute node "data-port-index") 10)]
      (when (and (contains? #{:input :output} kind)
                 (not (js/isNaN index)))
        {:kind kind :index index}))))

(defn- free-port [state item-id kind]
  (when-let [index (model/free-port-index @state item-id kind)]
    {:kind kind :index index}))

(defn- wire-start-port [state event]
  (when-let [id (item-id event)]
    (if-let [port (port-info event)]
      (when (and (= :output (:kind port))
                 (not (model/port-used? @state id :output (:index port))))
        port)
      (free-port state id :output))))

(defn- wire-end-port [state event]
  (when-let [id (item-id event)]
    (if-let [port (port-info event)]
      (when (and (= :input (:kind port))
                 (not (model/port-used? @state id :input (:index port))))
        port)
      (free-port state id :input))))

(defn- measured-items [root state]
  (let [nodes (into {}
                    (keep (fn [node]
                            (when-let [id (some-> (.getAttribute node "data-item-id")
                                                  (js/parseInt 10))]
                              [id node])))
                    (array-seq (.querySelectorAll root ".canvas-item")))]
    (mapv (fn [item]
            (if-let [node (get nodes (:id item))]
              (let [width (.-offsetWidth node)
                    height (.-offsetHeight node)]
                (cond-> item
                  (pos? width) (assoc :width width)
                  (pos? height) (assoc :height height)))
              item))
          (:items state))))

(defn- sync-selection! [root state]
  (doseq [node (array-seq (.querySelectorAll root ".canvas-item"))]
    (when-let [id (some-> (.getAttribute node "data-item-id")
                          (js/parseInt 10))]
      (if (contains? (:selected state) id)
        (.add (.-classList node) "selected")
        (.remove (.-classList node) "selected")))))

(defn- handle-mousedown! [state root drag event]
  (let [button (.-button event)]
    (when (and (or (zero? button) (= 1 button))
               (not (closest event ".context-menu"))
               (not (closest event ".dialog-backdrop"))
               (not (closest event ".canvas-toolbar")))
      (let [had-context-menu? (some? (:context-menu @state))
            clicked-id (item-id event)]
        (reset! drag nil)
        (cond
          (= 1 button)
          (do
            (.preventDefault event)
            (reset! drag {:mode :pan
                          :start [(.-clientX event) (.-clientY event)]}))

          (model/wire-tool? @state)
          (if clicked-id
            (when-let [p (world-point state root event)]
              (.preventDefault event)
              (when-not (:wire-draft @state)
                (when-let [port (wire-start-port state event)]
                  (swap! state model/start-wire-draft
                         clicked-id (first p) (second p) port))))
            (when (:wire-draft @state)
              (.preventDefault event)))

          :else
          (if clicked-id
            (do
              (swap! state
                     (fn [s]
                       (-> (if (.-shiftKey event)
                             (model/toggle-selection s clicked-id)
                             (if (contains? (:selected s) clicked-id)
                               s
                               (model/select-item s clicked-id)))
                           model/close-context-menu)))
              (when-not (.-shiftKey event)
                (reset! drag {:mode :move
                              :origins (into {}
                                             (comp (filter #(contains? (:selected @state)
                                                                       (:id %)))
                                                   (map (juxt :id (juxt :x :y))))
                                             (:items @state))
                              :start [(.-clientX event) (.-clientY event)]})))
            (when-let [p (world-point state root event)]
              (swap! state #(-> %
                                model/clear-selection
                                model/close-context-menu
                                (model/begin-marquee p))))))
        (if (and (zero? button)
                 clicked-id
                 (not (model/wire-tool? @state))
                 (not had-context-menu?))
          (sync-selection! root @state)
          (render! root @state))))))

(defn- handle-mousemove! [state root drag event]
  (when-not (:dialog @state)
    (let [{:keys [mode origins start]} @drag
          client-x (.-clientX event)
          client-y (.-clientY event)
          p (world-point state root event)]
      (cond
        (= :pan mode)
        (do
          (swap! state model/pan-by (- client-x (first start)) (- client-y (second start)))
          (swap! drag assoc :start [client-x client-y])
          (render! root @state))

        (:marquee @state)
        (when p
          (let [items (measured-items root @state)]
            (swap! state #(-> %
                              (model/update-marquee p)
                              (model/update-marquee-selection items)
                              (model/set-cursor p)))
            (render! root @state)))

        (:wire-draft @state)
        (when p
          (swap! state #(-> %
                            (model/update-wire-draft (first p) (second p))
                            (model/set-cursor p)))
          (render! root @state))

        (= :move mode)
        (let [zoom (get-in @state [:view :zoom])]
          (swap! state model/move-items
                 origins
                 (/ (- client-x (first start)) zoom)
                 (/ (- client-y (second start)) zoom))
          (when p
            (swap! state model/set-cursor p))
          (render! root @state))

        :else
        (when p
          (swap! state model/set-cursor p)
          (render! root @state))))))

(defn- handle-mouseup! [state root drag event]
  (let [draft (:wire-draft @state)
        finishing-wire? (some? draft)
        finishing-marquee? (some? (:marquee @state))]
    (reset! drag nil)
    (let [changed?
          (cond
            finishing-wire?
            (if-let [id (item-id event)]
              (when (not= id (:from draft))
                (swap! state model/finish-wire id (wire-end-port state event))
                true)
              (when-let [p (world-point state root event)]
                (swap! state model/add-wire-point (first p) (second p))
                true))

            finishing-marquee?
            (do
              (swap! state model/finish-marquee)
              true))]
      (when changed?
        (render! root @state)))))

(defn- handle-wheel! [state root event]
  (when (closest event "#canvas-panel")
    (.preventDefault event)
    (let [rect (.getBoundingClientRect (canvas-node root))
          p [(- (.-clientX event) (.-left rect))
             (- (.-clientY event) (.-top rect))]
          factor (if (pos? (.-deltaY event)) (/ 1 1.15) 1.15)]
      (swap! state model/zoom-at factor p)
      (render! root @state))))

(defn- handle-contextmenu! [state root drag event]
  (when-let [id (item-id event)]
    (.preventDefault event)
    (reset! drag nil)
    (swap! state #(-> %
                      model/cancel-marquee
                      (model/open-context-menu id (.-clientX event) (.-clientY event))))
    (render! root @state)))

(defn- handle-dblclick! [state root event]
  (when (and (nil? (:dialog @state))
             (not (model/wire-tool? @state)))
    (when-let [id (item-id event)]
      (swap! state (fn [current]
                     (if (= :custom (:component (model/item-by-id current id)))
                       (model/open-custom-editor current id)
                       (model/open-gate-qubits-dialog current id))))
      (render! root @state))))

(defn- shortcut-targets [state]
  (if-let [item-id (get-in state [:context-menu :item-id])]
    #{item-id}
    (:selected state)))

(defn- delete-targets! [state root item-ids]
  (swap! state model/delete-items item-ids)
  (render! root @state))

(defn- rotate-targets! [state root item-ids degrees]
  (swap! state #(-> %
                    (model/rotate-items item-ids degrees)
                    model/close-context-menu))
  (render! root @state))

(defn- handle-keydown! [state root event]
  (let [key (.-key event)
        current @state
        targets (shortcut-targets current)]
    (cond
      (and (= key "Escape") (:dialog current))
      (do
        (.preventDefault event)
        (swap! state model/cancel-dialog)
        (render! root @state))

      (and (= key "Escape") (model/wire-tool? current))
      (do
        (.preventDefault event)
        (swap! state model/set-tool :select)
        (render! root @state))

      (and (= key "Escape") (:marquee current))
      (do
        (.preventDefault event)
        (swap! state #(-> %
                          model/cancel-marquee
                          model/clear-selection))
        (render! root @state))

      (and (= key "Escape") (seq targets))
      (do
        (.preventDefault event)
        (delete-targets! state root targets))

      (and (or (= key "g") (= key "G"))
           (nil? (:dialog current)))
      (do
        (.preventDefault event)
        (swap! state model/toggle-snap)
        (render! root @state))

      (and (= key "0")
           (nil? (:dialog current)))
      (do
        (.preventDefault event)
        (swap! state model/reset-view)
        (render! root @state))

      (and (or (= key "w") (= key "W"))
           (nil? (:dialog current)))
      (do
        (.preventDefault event)
        (swap! state model/toggle-wire-tool)
        (render! root @state))

      (and (or (= key "r") (= key "R"))
           (nil? (:dialog current))
           (seq targets))
      (do
        (.preventDefault event)
        (rotate-targets! state root targets model/rotation-step-degrees)))))

(defn- rotate-context-item! [state root]
  (when-let [{:keys [item-id]} (:context-menu @state)]
    (rotate-targets! state root #{item-id} model/rotation-step-degrees)))

(defn- handle-click! [state root event]
  (cond
    (closest event "#wire-tool")
    (do
      (swap! state #(-> %
                        model/toggle-wire-tool
                        model/close-context-menu))
      (render! root @state))

    (closest event "#snap-toggle")
    (do
      (swap! state model/toggle-snap)
      (render! root @state))

    (or (closest event "#register-cancel")
        (closest event "#gate-qubits-cancel")
        (closest event "#custom-cancel"))
    (do
      (swap! state model/cancel-dialog)
      (render! root @state))

    (closest event "#context-delete")
    (when-let [{:keys [item-id]} (:context-menu @state)]
      (delete-targets! state root #{item-id}))

    (closest event "#context-rotate")
    (rotate-context-item! state root)))

(defn- attach-events! [root state]
  (let [drag (atom nil)
        unsubscribe-menu-save! (subscribe-menu-save! state)
        unsubscribe-menu-open! (subscribe-menu-open! state root)
        root-handlers {:dragstart handle-dragstart!
                       :dragover handle-dragover!
                       :drop #(handle-drop! state root %)
                       :submit #(handle-submit! state root %)
                       :click #(handle-click! state root %)
                       :dblclick #(handle-dblclick! state root %)
                       :wheel #(handle-wheel! state root %)
                       :mousedown #(handle-mousedown! state root drag %)
                       :contextmenu #(handle-contextmenu! state root drag %)}
        document-handlers {:mousemove #(handle-mousemove! state root drag %)
                           :mouseup #(handle-mouseup! state root drag %)
                           :keydown #(handle-keydown! state root %)}]
    (doseq [[type handler] root-handlers]
      (.addEventListener root (name type) handler
                         (if (= :wheel type) #js {:passive false} false)))
    (doseq [[type handler] document-handlers]
      (.addEventListener js/document (name type) handler))
    {:stop! (fn []
              (doseq [[type handler] root-handlers]
                (.removeEventListener root (name type) handler false))
              (doseq [[type handler] document-handlers]
                (.removeEventListener js/document (name type) handler))
              (when unsubscribe-menu-save!
                (unsubscribe-menu-save!))
              (when unsubscribe-menu-open!
                (unsubscribe-menu-open!)))}))

(defn init! []
  (when-let [root (.getElementById js/document "app")]
    (let [state (atom model/initial-state)
          {:keys [stop!]} (attach-events! root state)]
      (render! root @state)
      {:root root
       :state state
       :stop! stop!})))
