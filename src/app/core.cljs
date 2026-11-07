(ns app.core
  (:require [clojure.string :as str]
            [app.dom :as dom]
            [app.export :as export]
            [app.library :as library]
            [app.model :as model]
            [app.persist :as persist]
            [app.qasm :as qasm]))

(def ^:private autosave-timer (atom nil))

(defn- commit! [state f & args]
  (swap! state
         (fn [before]
           (let [after (apply f before args)]
             (if (= (model/snapshot before) (model/snapshot after))
               after
               (model/append-history before after))))))

(defn- autosave-enabled? []
  (boolean (.-electronAPI js/window)))

(defn- schedule-autosave! [state]
  (when (autosave-enabled?)
    (when @autosave-timer
      (js/clearTimeout @autosave-timer))
    (reset! autosave-timer
            (js/setTimeout #(persist/save-autosave! state) 400))))

(def ^:private scroll-region-selectors
  ["#components-panel" "#property-panel" "#library-template-list"
   "#library-user-list" "#diff-content"])

(defn- scroll-offsets [root]
  (into {}
        (keep (fn [selector]
                (when-let [node (.querySelector root selector)]
                  (when (pos? (.-scrollTop node))
                    [selector (.-scrollTop node)]))))
        scroll-region-selectors))

(defn- restore-scroll! [root offsets]
  (doseq [[selector offset] offsets]
    (when-let [node (.querySelector root selector)]
      (set! (.-scrollTop node) offset))))

(defn- render! [root state]
  (let [offsets (scroll-offsets root)]
    (set! (.-innerHTML root) "")
    (.appendChild root (dom/render-app state))
    (dom/refresh-wires! root state)
    (restore-scroll! root offsets))
  (schedule-autosave! state))

(defn- download-url! [url name]
  (let [link (.createElement js/document "a")]
    (set! (.-href link) url)
    (set! (.-download link) name)
    (.appendChild (.-body js/document) link)
    (.click link)
    (.remove link)))

(defn- download-text! [text name mime]
  (download-url! (str "data:" mime ";charset=utf-8," (js/encodeURIComponent text))
                 name))

(defn- save-text!
  ([text default-name filters] (save-text! text default-name filters nil))
  ([text default-name filters on-saved]
   (let [api (.-electronAPI js/window)]
     (if-let [save (and api (.-saveFile api))]
       (-> (.saveFile api text default-name filters)
           (.then (fn [^js result]
                    (when (and on-saved
                               (not (and result (.-canceled result))))
                      (on-saved))))
           (.catch (fn [error]
                     (js/console.error "Could not save file." error))))
       (do
         (download-text! text default-name "text/plain")
         (when on-saved (on-saved)))))))

(defn- save-binary! [base64 default-name filters]
  (let [api (.-electronAPI js/window)]
    (if-let [save (and api (.-saveBinary api))]
      (-> (.saveBinary api base64 default-name filters)
          (.catch (fn [error]
                    (js/console.error "Could not save image." error))))
      (download-url! (str "data:image/png;base64," base64) default-name))))

(declare save-qasm!)
(declare mark-saved!)

(defn- save-json! [state]
  (let [api (.-electronAPI js/window)]
    (if (and api
             (not (.-saveFile api))
             (.-saveQasm api))
      (save-qasm! state)
      (save-text! (persist/to-json @state) "circuit.json"
                  [{:name "QCAD circuit" :extensions ["json"]}]
                  #(mark-saved! state)))))

(defn- save-qasm! [state]
  (let [api (.-electronAPI js/window)]
    (if-let [save-qasm (and api (.-saveQasm api))]
      (-> (.saveQasm api (qasm/to-qasm @state) "circuit.qasm")
          (.then (fn [^js result]
                   (when-not (and result (.-canceled result))
                     (mark-saved! state))))
          (.catch (fn [error]
                    (js/console.error "Could not save OpenQASM." error))))
      (save-text! (qasm/to-qasm @state) "circuit.qasm"
                  [{:name "OpenQASM" :extensions ["qasm"]}]
                  #(mark-saved! state)))))

(defn- prepare-loaded [loaded]
  (-> loaded
      (assoc :history [(model/snapshot loaded)]
             :history-index 0)
      (model/set-baseline)))

(defn- mark-saved! [state]
  (swap! state model/set-baseline))

(defn- load-content [state root text file-path]
  (let [json? (or (str/ends-with? (str/lower-case (str file-path)) ".json")
                  (str/starts-with? (str/trim (str text)) "{"))
        clipboard (:clipboard @state)
        loaded (if json?
                 (persist/from-json text)
                 (qasm/from-qasm text))]
    (reset! state (assoc (prepare-loaded loaded) :clipboard clipboard))
    (render! root @state)))

(defn- browser-open! [accept handler]
  (let [input (.createElement js/document "input")]
    (set! (.-type input) "file")
    (set! (.-accept input) accept)
    (set! (.-onchange input)
          (fn [_]
            (when-let [file (aget (.-files input) 0)]
              (let [reader (js/FileReader.)]
                (set! (.-onload reader)
                      #(handler (.-result reader) (.-name file)))
                (.readAsText reader file)))))
    (.click input)))

(defn- open-document! [state root]
  (let [api (.-electronAPI js/window)
        filters [{:name "QCAD circuit" :extensions ["json" "qasm"]}
                 {:name "All files" :extensions ["*"]}]
        handle-result (fn [^js result]
                        (when (and result
                                   (not (.-canceled result))
                                   (string? (.-content result)))
                          (load-content state root (.-content result)
                                        (.-filePath result))))]
    (cond
      (and api (.-openFile api))
      (-> (.openFile api filters)
          (.then handle-result)
          (.catch (fn [error]
                    (js/console.error "Could not open file." error))))

      (and api (.-openQasm api))
      (-> (.openQasm api)
          (.then handle-result)
          (.catch (fn [error]
                    (js/console.error "Could not open file." error))))

      :else
      (browser-open! ".json,.qasm,.qasm3"
                     #(load-content state root %1 %2)))))

(defn- open-recent! [state root file-path]
  (let [api (.-electronAPI js/window)]
    (when-let [open-file-at (and api (.-openFileAt api))]
      (-> (.openFileAt api file-path)
          (.then (fn [^js result]
                   (when (and result
                              (not (.-canceled result))
                              (string? (.-content result)))
                     (load-content state root (.-content result) file-path))))
          (.catch (fn [error]
                    (js/console.error "Could not open recent file." error)))))))

(defn- subscribe-menu-save! [state]
  (when-let [api (.-electronAPI js/window)]
    (let [unsub-save (when-let [on-menu-save (.-onMenuSave api)]
                       (on-menu-save #(save-json! state)))
          unsub-qasm (when-let [on-menu-save-qasm (.-onMenuSaveQasm api)]
                       (on-menu-save-qasm #(save-qasm! state)))]
      (cond
        (and unsub-save unsub-qasm) (fn [] (unsub-save) (unsub-qasm))
        unsub-save unsub-save
        unsub-qasm unsub-qasm
        :else nil))))

(defn- subscribe-menu-open! [state root]
  (when-let [api (.-electronAPI js/window)]
    (when-let [on-menu-open (.-onMenuOpen api)]
      (on-menu-open #(open-document! state root)))))

(defn- subscribe-menu-action! [api event-name handler]
  (when-let [subscribe (aget api event-name)]
    (subscribe handler)))

(defn- closest [event selector]
  (let [target (.-target event)]
    (when (and target (fn? (.-closest target)))
      (.closest target selector))))

(defn- typing? [event]
  (let [target (.-target event)]
    (or (contains? #{"INPUT" "TEXTAREA" "SELECT"} (.-tagName target))
        (.-isContentEditable target))))

(defn- navigator-clipboard []
  (when (exists? js/navigator)
    (.-clipboard js/navigator)))

(defn- write-selection-clipboard! [state]
  (when-let [snapshot (:clipboard @state)]
    (let [api (.-electronAPI js/window)
          text (persist/snapshot->json snapshot)]
      (cond
        (and api (.-writeClipboard api))
        (-> (.writeClipboard api text)
            (.catch (fn [error]
                      (js/console.error "Could not write to clipboard." error))))

        (navigator-clipboard)
        (-> (.writeText (navigator-clipboard) text)
            (.catch (fn [error]
                      (js/console.error "Could not write to clipboard." error))))))))

(defn- read-system-clipboard! [callback]
  (let [api (.-electronAPI js/window)]
    (cond
      (and api (.-readClipboard api))
      (-> (.readClipboard api)
          (.then (fn [text] (callback (persist/snapshot-from-json text))))
          (.catch (fn [_] (callback nil))))

      (navigator-clipboard)
      (-> (.readText (navigator-clipboard))
          (.then (fn [text] (callback (persist/snapshot-from-json text))))
          (.catch (fn [_] (callback nil))))

      :else
      (callback nil))))

(defn- block-payload [event]
  (when-let [node (closest event "[data-block-template], [data-block-library]")]
    (or (when-let [id (.getAttribute node "data-block-template")]
          (str "template:" id))
        (when-let [name (.getAttribute node "data-block-library")]
          (str "library:" name)))))

(defn- handle-dragstart! [event]
  (when-let [data-transfer (.-dataTransfer event)]
    (if-let [payload (block-payload event)]
      (do
        (.setData data-transfer "application/x-qcad-block" payload)
        (set! (.-effectAllowed data-transfer) "copy"))
      (when-let [tile (closest event "[data-component-id]")]
        (.setData data-transfer
                  "application/x-qcad-component"
                  (.getAttribute tile "data-component-id"))
        (set! (.-effectAllowed data-transfer) "copy")))))

(defn- dragged-component-id [event]
  (some-> (.-dataTransfer event)
          (.getData "application/x-qcad-component")
          not-empty
          keyword))

(defn- dragged-block [event]
  (some-> (.-dataTransfer event)
          (.getData "application/x-qcad-block")
          not-empty))

(defn- parse-block-payload [payload]
  (let [[kind value] (str/split payload #":" 2)]
    (case kind
      "template" {:type :template :id (keyword value)}
      "library" {:type :library :name value}
      nil)))

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
    (when-let [[x y] (world-point state root event)]
      (if-let [block (some-> (dragged-block event) parse-block-payload)]
        (do
          (commit! state
                   (fn [s]
                     (cond
                       (= :template (:type block))
                       (library/insert-block-template s (:id block) x y)

                       (= :library (:type block))
                       (library/insert-block-user s (:name block) x y)

                       :else s)))
          (swap! state model/cancel-dialog)
          (render! root @state))
        (when-let [component-id (dragged-component-id event)]
          (commit! state model/handle-drop component-id x y)
          (render! root @state))))))

(defn- handle-submit! [state root event]
  (when-let [form (closest event "form")]
    (.preventDefault event)
    (cond
      (= "register-form" (.-id form))
      (let [name (.-value (.querySelector form "#register-name"))
            qubits (.-value (.querySelector form "#register-qubits"))]
        (commit! state model/confirm-register name qubits)
        (render! root @state))

      (= "gate-qubits-form" (.-id form))
      (let [qubits (.-value (.querySelector form "#gate-qubits"))
            angle (some-> (.querySelector form "#gate-angle") .-value)
            controls (some-> (.querySelector form "#gate-controls") .-value)]
        (commit! state model/confirm-gate-qubits qubits angle controls)
        (render! root @state))

      (= "custom-form" (.-id form))
      (let [qubits (.-value (.querySelector form "#custom-qubits"))
            text (.-value (.querySelector form "#custom-qasm"))]
        (commit! state model/confirm-custom-gate qubits text)
        (render! root @state))

      (= "annotation-form" (.-id form))
      (let [text (.-value (.querySelector form "#annotation-text"))]
        (commit! state model/confirm-annotation text)
        (render! root @state))

      (= "metadata-form" (.-id form))
      (let [metadata {:name (.-value (.querySelector form "#metadata-name"))
                      :author (.-value (.querySelector form "#metadata-author"))
                      :description (.-value (.querySelector form "#metadata-description"))}]
        (commit! state model/confirm-metadata metadata)
        (render! root @state))

      :else nil)))

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
    (when (and (or (closest event ".components-panel")
                   (closest event ".property-panel"))
               (:context-menu @state))
      (swap! state model/close-context-menu)
      (render! root @state))
    (when (and (or (zero? button) (= 1 button))
               (not (closest event ".context-menu"))
               (not (closest event ".dialog-backdrop"))
               (not (closest event ".app-toolbar"))
               (not (closest event ".components-panel"))
               (not (closest event ".property-panel")))
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
                              :before (model/snapshot @state)
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

(defn- chrome-target? [event]
  (boolean (or (closest event ".components-panel")
               (closest event ".property-panel")
               (closest event ".app-toolbar")
               (closest event ".context-menu")
               (closest event ".dialog-backdrop"))))

(defn- handle-mousemove! [state root drag event]
  (when (and (not (:dialog @state))
             (or (not (chrome-target? event))
                 (:mode @drag)
                 (:marquee @state)
                 (:wire-draft @state)))
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
  (let [{:keys [mode before]} @drag
        draft (:wire-draft @state)
        finishing-wire? (some? draft)
        finishing-marquee? (some? (:marquee @state))]
    (reset! drag nil)
    (let [changed?
          (cond
            finishing-wire?
            (if-let [id (item-id event)]
              (when (not= id (:from draft))
                (commit! state model/finish-wire id (wire-end-port state event))
                true)
              (when-let [p (world-point state root event)]
                (swap! state model/add-wire-point (first p) (second p))
                true))

            finishing-marquee?
            (do
              (swap! state model/finish-marquee)
              true))]
      (when (and (= :move mode)
                 before
                 (not= before (model/snapshot @state)))
        (swap! state (fn [current]
                       (model/append-history (merge current before) current))))
      (when changed?
        (render! root @state))
      (when (and (= :move mode) before)
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

(defn- edit-item! [state id]
  (let [item (model/item-by-id state id)]
    (cond
      (contains? #{:annotation :box} (:type item))
      (model/open-annotation-editor state id)

      (= :gate (:type item))
      (if (= :custom (:component item))
        (model/open-custom-editor state id)
        (model/open-gate-qubits-dialog state id))

      :else state)))

(defn- handle-dblclick! [state root event]
  (when (and (nil? (:dialog @state))
             (not (model/wire-tool? @state)))
    (when-let [id (item-id event)]
      (swap! state edit-item! id)
      (render! root @state))))

(defn- shortcut-targets [state]
  (if-let [item-id (get-in state [:context-menu :item-id])]
    #{item-id}
    (:selected state)))

(defn- delete-targets! [state root item-ids]
  (commit! state model/delete-items item-ids)
  (render! root @state))

(defn- rotate-targets! [state root item-ids degrees]
  (swap! state #(-> %
                    (model/rotate-items item-ids degrees)
                    model/close-context-menu))
  (render! root @state))

(defn- fit-view! [state root]
  (let [panel (canvas-node root)]
    (swap! state model/fit-view
           (if panel (.-clientWidth panel) 800)
           (if panel (.-clientHeight panel) 600)))
  (render! root @state))

(defn- undo! [state root]
  (swap! state model/undo)
  (render! root @state))

(defn- redo! [state root]
  (swap! state model/redo)
  (render! root @state))

(defn- copy! [state root]
  (swap! state model/copy-selection)
  (write-selection-clipboard! state)
  (render! root @state))

(defn- paste! [state root]
  (read-system-clipboard!
   (fn [snapshot]
     (commit! state (fn [s]
                      (if snapshot
                        (model/paste-snapshot s snapshot 20 20)
                        (model/paste-clipboard s 20 20))))
     (render! root @state))))

(defn- handle-keydown! [state root event]
  (let [key (.-key event)
        current @state
        targets (shortcut-targets current)
        modifier? (or (.-ctrlKey event) (.-metaKey event))
        typing (typing? event)]
    (cond
      (and (= key "Escape") (:dialog current))
      (do
        (.preventDefault event)
        (swap! state model/cancel-dialog)
        (render! root @state))

      typing nil

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

      (and modifier? (= (str/lower-case key) "z") (not (.-shiftKey event)))
      (do
        (.preventDefault event)
        (undo! state root))

      (and modifier?
           (or (and (= (str/lower-case key) "z") (.-shiftKey event))
               (= (str/lower-case key) "y")))
      (do
        (.preventDefault event)
        (redo! state root))

      (and modifier? (= (str/lower-case key) "c"))
      (do
        (.preventDefault event)
        (copy! state root))

      (and modifier? (= (str/lower-case key) "v"))
      (do
        (.preventDefault event)
        (paste! state root))

      (and modifier? (= (str/lower-case key) "d"))
      (do
        (.preventDefault event)
        (commit! state model/duplicate-selection)
        (render! root @state))

      (and (or (= key "g") (= key "G"))
           (nil? (:dialog current)))
      (do
        (.preventDefault event)
        (swap! state model/toggle-snap)
        (render! root @state))

      (and (or (= key "a") (= key "A"))
           (nil? (:dialog current)))
      (do
        (.preventDefault event)
        (swap! state model/toggle-component-snap)
        (render! root @state))

      (and (or (= key "0") (= key "f") (= key "F"))
           (nil? (:dialog current)))
      (do
        (.preventDefault event)
        (if (= key "0")
          (do (swap! state model/reset-view)
              (render! root @state))
          (fit-view! state root)))

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
        (rotate-targets! state root targets model/rotation-step-degrees))

      (and (or (= key "Delete") (= key "Backspace"))
           (seq targets))
      (do
        (.preventDefault event)
        (delete-targets! state root targets)))))

(defn- rotate-context-item! [state root]
  (when-let [{:keys [item-id]} (:context-menu @state)]
    (rotate-targets! state root #{item-id} model/rotation-step-degrees)))

(defn- toggle-context-control! [state root]
  (when-let [{:keys [item-id]} (:context-menu @state)]
    (commit! state model/toggle-gate-control item-id)
    (swap! state model/close-context-menu)
    (render! root @state)))

(defn- export-svg! [state format transparent include-ui scale]
  (let [svg (export/to-svg @state {:transparent transparent
                                   :include-ui include-ui})
        scale (max 1 (or scale 1))]
    (if (= "png" format)
      (let [image (js/Image.)
            url (export/to-data-url svg)]
        (set! (.-onload image)
              (fn [_]
                (let [canvas (.createElement js/document "canvas")
                      width (max 1 (* scale (.-naturalWidth image)))
                      height (max 1 (* scale (.-naturalHeight image)))
                      context (.getContext canvas "2d")]
                  (set! (.-width canvas) width)
                  (set! (.-height canvas) height)
                  (.drawImage context image 0 0 width height)
                  (let [data-url (.toDataURL canvas "image/png")
                        base64 (second (str/split data-url #","))]
                    (save-binary! base64 "circuit.png"
                                  [{:name "PNG image" :extensions ["png"]}])))))
        (set! (.-onerror image)
              (fn [_] (js/console.error "Could not render PNG export.")))
        (set! (.-src image) url))
      (save-text! svg "circuit.svg"
                  [{:name "SVG image" :extensions ["svg"]}]))))

(defn- open-diff! [state]
  (let [baseline (or (:baseline @state)
                     (model/snapshot model/initial-state))
        diff (persist/diff-summary baseline (model/snapshot @state))]
    (swap! state model/open-diff-dialog diff)))

(defn- insert-template! [state root id]
  (commit! state library/insert-template id)
  (swap! state model/cancel-dialog)
  (render! root @state))

(defn- insert-user-circuit! [state root name]
  (commit! state library/insert-user-circuit name)
  (swap! state model/cancel-dialog)
  (render! root @state))

(defn- save-library-circuit! [state root]
  (let [name-input (.querySelector root "#library-name")]
    (if (and name-input (not (str/blank? (.-value name-input))))
      (do
        (library/save-user-circuit! (.-value name-input) @state)
        (render! root @state))
      (do
        (swap! state assoc-in [:dialog :error] "Enter a name for the circuit.")
        (render! root @state)))))

(defn- delete-library-circuit! [state root name]
  (library/delete-user-circuit! name)
  (render! root @state))

(defn- handle-input! [state root event]
  (cond
    (closest event "#component-search")
    (when-let [input (closest event "#component-search")]
      (swap! state model/set-component-search (.-value input))
      (render! root @state)
      (when-let [next-input (.querySelector root "#component-search")]
        (.focus next-input)
        (let [length (count (.-value next-input))]
          (.setSelectionRange next-input length length))))

    (closest event "#library-name")
    (swap! state assoc-in [:dialog :name]
           (.-value (closest event "#library-name")))

    :else nil))

(defn- handle-change! [state root event]
  (let [export-dialog? (= :export (get-in @state [:dialog :type]))]
    (cond
      (closest event "#grid-size-select")
      (do
        (swap! state model/set-snap-size
               (js/parseInt (.-value (closest event "#grid-size-select")) 10))
        (render! root @state))

      (and export-dialog? (closest event "#export-format"))
      (do
        (swap! state assoc-in [:dialog :format]
               (.-value (closest event "#export-format")))
        (render! root @state))

      (and export-dialog? (closest event "#export-scale"))
      (do
        (swap! state assoc-in [:dialog :scale]
               (js/parseInt (.-value (closest event "#export-scale")) 10))
        (render! root @state))

      (and export-dialog? (closest event "#export-transparent"))
      (do
        (swap! state assoc-in [:dialog :transparent]
               (.-checked (closest event "#export-transparent")))
        (render! root @state))

      (and export-dialog? (closest event "#export-include-ui"))
      (do
        (swap! state assoc-in [:dialog :include-ui]
               (.-checked (closest event "#export-include-ui")))
        (render! root @state))

      :else nil)))

(defn- handle-click! [state root event]
  (let [current @state]
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

      (closest event "#component-snap-toggle")
      (do
        (swap! state model/toggle-component-snap)
        (render! root @state))

      (closest event "#undo-button")
      (undo! state root)

      (closest event "#redo-button")
      (redo! state root)

      (closest event "#zoom-fit")
      (fit-view! state root)

      (closest event "#library-button")
      (do (swap! state model/open-library-dialog)
          (render! root @state))

      (closest event "#export-button")
      (do (swap! state model/open-export-dialog)
          (render! root @state))

      (closest event "#properties-button")
      (do (swap! state model/open-metadata-dialog)
          (render! root @state))

      (closest event "#diff-button")
      (do (open-diff! state)
          (render! root @state))

      (closest event "#theme-toggle")
      (do (swap! state model/toggle-theme)
          (render! root @state))

      (or (closest event "#register-cancel")
          (closest event "#gate-qubits-cancel")
          (closest event "#custom-cancel")
          (closest event "#annotation-cancel")
          (closest event "#metadata-cancel")
          (closest event "#export-cancel")
          (closest event "#library-cancel")
          (closest event "#diff-close"))
      (do
        (swap! state model/cancel-dialog)
        (render! root @state))

      (closest event "#context-delete")
      (when-let [{:keys [item-id]} (:context-menu current)]
        (delete-targets! state root #{item-id}))

      (closest event "#context-rotate")
      (rotate-context-item! state root)

      (closest event "#context-control")
      (toggle-context-control! state root)

      (closest event "#property-edit")
      (when-let [id (first (:selected current))]
        (swap! state edit-item! id)
        (render! root @state))

      (closest event "[data-template-id]")
      (when-let [node (closest event "[data-template-id]")]
        (insert-template! state root
                          (keyword (.getAttribute node "data-template-id"))))

      (closest event "[data-library-name]")
      (when-let [node (closest event "[data-library-name]")]
        (insert-user-circuit! state root (.getAttribute node "data-library-name")))

      (closest event "[data-library-delete]")
      (when-let [node (closest event "[data-library-delete]")]
        (delete-library-circuit! state root (.getAttribute node "data-library-delete")))

      (closest event "#library-save")
      (save-library-circuit! state root)

      (closest event "#export-confirm")
      (do
        (when-let [dialog (:dialog current)]
          (let [scale-select (.querySelector root "#export-scale")
                scale (when scale-select (js/parseInt (.-value scale-select) 10))]
            (export-svg! state (:format dialog) (:transparent dialog)
                         (:include-ui dialog) scale)))
        (swap! state model/cancel-dialog)
        (render! root @state))

      (closest event ".button-preset")
      (when-let [node (closest event ".button-preset")]
        (when-let [input (.querySelector root "#gate-angle")]
          (set! (.-value input) (.getAttribute node "data-angle"))))

      :else nil)))

(defn- attach-events! [root state]
  (let [drag (atom nil)
        api (.-electronAPI js/window)
        unsubscribe-menu-save! (subscribe-menu-save! state)
        unsubscribe-menu-open! (subscribe-menu-open! state root)
        unsubscribes (when api
                       [(when-let [f (subscribe-menu-action!
                                      api "onMenuExport"
                                      #(do (swap! state model/open-export-dialog)
                                           (render! root @state)))]
                          f)
                        (when-let [f (subscribe-menu-action!
                                      api "onMenuLibrary"
                                      #(do (swap! state model/open-library-dialog)
                                           (render! root @state)))]
                          f)
                        (when-let [f (subscribe-menu-action!
                                      api "onMenuOpenRecent"
                                      #(open-recent! state root %))]
                          f)])
        root-handlers {:dragstart handle-dragstart!
                       :dragover handle-dragover!
                       :drop #(handle-drop! state root %)
                       :submit #(handle-submit! state root %)
                       :click #(handle-click! state root %)
                       :input #(handle-input! state root %)
                       :change #(handle-change! state root %)
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
                (unsubscribe-menu-open!))
              (doseq [unsubscribe (keep identity unsubscribes)]
                (unsubscribe)))})) 

(defn init! []
  (when-let [root (.getElementById js/document "app")]
    (let [restored (when (autosave-enabled?) (persist/load-autosave))
          state (atom (prepare-loaded (or restored model/initial-state)))
          {:keys [stop!]} (attach-events! root state)]
      (render! root @state)
      {:root root
       :state state
       :stop! stop!})))
