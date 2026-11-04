(ns app.integration-test
  (:require [clojure.string :as str]
            [cljs.test :refer [deftest is testing use-fixtures]]
            [app.core :as core]
            ["jsdom" :as jsdom]))

(defonce ^:private dom
  (jsdom/JSDOM. "<!doctype html><html><body><div id=\"app\"></div></body></html>"))

(set! (.-window js/globalThis) (.-window dom))
(set! (.-document js/globalThis) (.-document (.-window dom)))

(defn- document []
  (.-document js/globalThis))

(defn- select [selector]
  (.querySelector (document) selector))

(defn- select-all [selector]
  (vec (array-seq (.querySelectorAll (document) selector))))

(defonce ^:private session (atom nil))

(defn- stop-app! []
  (when-let [stop! (:stop! @session)]
    (stop!))
  (reset! session nil))

(defn- reset-app! []
  (stop-app!)
  (set! (.-innerHTML (.-body (document))) "<div id=\"app\"></div>")
  (set! (.-electronAPI js/window) nil)
  (reset! session (core/init!)))

(defn- reset-app-with-api! [api]
  (stop-app!)
  (set! (.-innerHTML (.-body (document))) "<div id=\"app\"></div>")
  (set! (.-electronAPI js/window) api)
  (reset! session (core/init!)))

(use-fixtures :each (fn [test-fn]
                      (reset-app!)
                      (try
                        (test-fn)
                        (finally
                          (stop-app!)))))

(defn- fake-data-transfer []
  (let [data #js {}]
    #js {:setData (fn [type value] (aset data type value))
         :getData (fn [type] (or (aget data type) ""))
         :effectAllowed ""
         :dropEffect ""}))

(defn- dragstart-component! [component-id]
  (let [tile (select (str "[data-component-id=\"" (name component-id) "\"]"))
        event (js/window.Event. "dragstart" #js {:bubbles true :cancelable true})
        data-transfer (fake-data-transfer)]
    (set! (.-dataTransfer event) data-transfer)
    (.dispatchEvent tile event)
    data-transfer))

(defn- drop-at! [data-transfer x y]
  (let [canvas (select "#canvas-panel")
        event (js/window.MouseEvent. "drop"
                                     #js {:bubbles true
                                          :cancelable true
                                          :clientX x
                                          :clientY y})]
    (set! (.-dataTransfer event) data-transfer)
    (.dispatchEvent canvas event)))

(defn- drop-component! [component-id x y]
  (drop-at! (dragstart-component! component-id) x y))

(defn- submit-register! [name qubits]
  (set! (.-value (select "#register-name")) name)
  (set! (.-value (select "#register-qubits")) qubits)
  (let [form (select "#register-form")
        event (js/window.Event. "submit" #js {:bubbles true :cancelable true})]
    (.dispatchEvent form event)))

(defn- mouse-event [type options]
  (js/window.MouseEvent. type (clj->js (merge {:bubbles true
                                               :cancelable true}
                                              options))))

(defn- mouse-down! [target options]
  (.dispatchEvent target (mouse-event "mousedown" (merge {:button 0} options))))

(defn- mouse-move! [options]
  (.dispatchEvent (document) (mouse-event "mousemove" options)))

(defn- mouse-up! []
  (.dispatchEvent (document) (mouse-event "mouseup" {})))

(defn- right-click! [target x y]
  (.dispatchEvent target (mouse-event "contextmenu"
                                      {:button 2
                                       :clientX x
                                       :clientY y})))

(defn- key-down!
  ([key] (key-down! key nil))
  ([key options]
   (.dispatchEvent (document)
                   (js/window.KeyboardEvent.
                    "keydown"
                    (clj->js (merge {:key key
                                     :bubbles true
                                     :cancelable true}
                                    options))))))

(deftest layout-test
  (testing "the application is split into a components panel and a canvas"
    (let [panel (select "#components-panel")
          canvas (select "#canvas-panel")]
      (is (some? (select ".app")))
      (is (some? panel))
      (is (some? canvas))
      (is (= "20%" (.. panel -style -width)))
      (is (= "1" (.. canvas -style -flexGrow))))))

(deftest components-panel-test
  (testing "all required components are rendered as draggable rectangles"
    (let [tiles (select-all ".component-tile")]
      (is (= 15 (count tiles)))
      (is (= ["quantum-register" "h" "x" "y" "z" "s" "t" "sdag" "tdag" "rx" "ry" "rz" "p" "i"
              "custom"]
             (mapv #(.getAttribute % "data-component-id") tiles)))
      (is (every? #(= "true" (.getAttribute % "draggable")) tiles))
      (is (= "Quantum register" (.-textContent (first tiles))))
      (is (= "H" (.-textContent (second tiles)))))))

(deftest dragover-test
  (testing "dragging over the canvas is allowed"
    (let [canvas (select "#canvas-panel")
          data-transfer (fake-data-transfer)
          event (js/window.MouseEvent. "dragover"
                                       #js {:bubbles true
                                            :cancelable true})]
      (set! (.-dataTransfer event) data-transfer)
      (.dispatchEvent canvas event)
      (is (= "canvas-panel" (.getAttribute canvas "id")))
      (is (.-defaultPrevented event))
      (is (= "copy" (.-dropEffect data-transfer))))))

(deftest drop-gate-test
  (testing "dragging a gate onto the canvas places it at the drop position"
    (let [data-transfer (dragstart-component! :h)]
      (is (= "h" (.getData data-transfer "application/x-qcad-component")))
      (drop-at! data-transfer 120 80)
      (let [gate (select ".canvas-gate")]
        (is (some? gate))
        (is (= "H" (.-textContent gate)))
        (is (= "120px" (.. gate -style -left)))
        (is (= "80px" (.. gate -style -top)))))))

(deftest register-dialog-test
  (testing "dropping a register opens the configuration dialog"
    (drop-component! :quantum-register 50 60)
    (is (some? (select "#register-form")))
    (is (= "dialog" (.getAttribute (select ".dialog") "role")))
    (is (= "register-1" (.-value (select "#register-name"))))
    (is (= "2" (.-value (select "#register-qubits"))))
    (is (= 0 (count (select-all ".canvas-register"))))))

(deftest register-dialog-editing-test
  (testing "the register popup stays interactive while editing"
    (drop-component! :quantum-register 50 60)
    (let [input (select "#register-name")]
      (.focus input)
      (mouse-down! input {:clientX 60 :clientY 70})
      (mouse-up!)
      (is (identical? input (select "#register-name")))
      (is (identical? input (.-activeElement (document))))
      (set! (.-value input) "custom")
      (mouse-move! {:clientX 80 :clientY 90})
      (is (identical? input (select "#register-name")))
      (is (= "custom" (.-value (select "#register-name"))))
      (is (identical? input (.-activeElement (document)))))))

(deftest confirm-register-test
  (testing "a valid configuration adds the register where it was dropped"
    (drop-component! :quantum-register 50 60)
    (submit-register! "data" "3")
    (let [register (select ".canvas-register")]
      (is (nil? (select "#register-form")))
      (is (some? register))
      (is (= "data (3 qubits)" (.-textContent register)))
      (is (= "60px" (.. register -style -left)))
      (is (= "60px" (.. register -style -top))))))

(deftest single-qubit-register-label-test
  (drop-component! :quantum-register 0 0)
  (submit-register! "q" "1")
  (is (= "q (1 qubit)" (.-textContent (select ".canvas-register")))))

(deftest invalid-register-test
  (testing "invalid input keeps the dialog open and shows an error"
    (drop-component! :quantum-register 10 10)
    (submit-register! "   " "0")
    (is (some? (select "#register-form")))
    (is (some? (select ".dialog-error")))
    (is (= "   " (.-value (select "#register-name"))))
    (is (= "0" (.-value (select "#register-qubits"))))
    (is (= 0 (count (select-all ".canvas-register"))))))

(deftest cancel-register-test
  (testing "cancelling closes the dialog without adding a register"
    (drop-component! :quantum-register 10 10)
    (.click (select "#register-cancel"))
    (is (nil? (select "#register-form")))
    (is (= 0 (count (select-all ".canvas-register"))))))

(deftest unrelated-events-test
  (testing "drag and drop events outside their targets are ignored"
    (let [panel (select "#components-panel")
          canvas (select "#canvas-panel")]
      (.dispatchEvent canvas (js/window.Event. "dragstart"
                                               #js {:bubbles true
                                                    :cancelable true}))
      (.dispatchEvent panel (js/window.Event. "drop"
                                              #js {:bubbles true
                                                   :cancelable true}))
      (.dispatchEvent canvas (js/window.Event. "drop"
                                               #js {:bubbles true
                                                    :cancelable true}))
      (.dispatchEvent panel (js/window.Event. "submit"
                                              #js {:bubbles true
                                                   :cancelable true}))
      (.click panel)
      (is (= 0 (count (select-all ".canvas-item"))))
      (is (nil? (select "#register-form")))))

  (testing "dragover still allows a drop when no data transfer is present"
    (let [canvas (select "#canvas-panel")
          event (js/window.Event. "dragover"
                                  #js {:bubbles true
                                       :cancelable true})]
      (.dispatchEvent canvas event)
      (is (.-defaultPrevented event))))

  (testing "right clicking empty space does not open a menu"
    (right-click! (select "#canvas-panel") 20 20)
    (is (nil? (select "#context-menu"))))

  (testing "shortcuts are ignored when no context menu is open"
    (key-down! "r")
    (is (= 0 (count (select-all ".canvas-item"))))))

(deftest selection-test
  (testing "left clicking a component selects it"
    (drop-component! :h 100 100)
    (mouse-down! (select ".canvas-gate") {:clientX 100 :clientY 100})
    (is (= ["1"] (mapv #(.getAttribute % "data-item-id")
                       (select-all ".canvas-item.selected")))))

  (testing "shift clicking toggles additional components"
    (drop-component! :x 200 200)
    (mouse-down! (first (select-all ".canvas-gate")) {:clientX 100 :clientY 100})
    (mouse-down! (second (select-all ".canvas-gate"))
                 {:clientX 200 :clientY 200 :shiftKey true})
    (is (= 2 (count (select-all ".canvas-item.selected"))))
    (mouse-down! (first (select-all ".canvas-gate"))
                 {:clientX 100 :clientY 100 :shiftKey true})
    (is (= 1 (count (select-all ".canvas-item.selected")))))

  (testing "clicking empty canvas clears the selection"
    (mouse-down! (select "#canvas-panel") {:clientX 10 :clientY 10})
    (is (= 0 (count (select-all ".canvas-item.selected"))))))

(deftest drag-test
  (testing "dragging a selected component moves it"
    (drop-component! :h 100 100)
    (drop-component! :x 200 200)
    (mouse-down! (first (select-all ".canvas-gate")) {:clientX 100 :clientY 100})
    (mouse-move! {:clientX 130 :clientY 125})
    (let [[moved other] (select-all ".canvas-gate")]
      (is (= "140px" (.. moved -style -left)))
      (is (= "120px" (.. moved -style -top)))
      (is (= "200px" (.. other -style -left)))
      (is (str/includes? (.-className moved) "selected"))))

  (testing "moving the mouse after release does not move components"
    (mouse-up!)
    (mouse-move! {:clientX 400 :clientY 400})
    (is (= "140px" (.. (first (select-all ".canvas-gate")) -style -left)))))

(deftest multi-select-drag-test
  (testing "dragging one selected component moves the whole selection"
    (drop-component! :h 100 100)
    (drop-component! :x 200 200)
    (mouse-down! (first (select-all ".canvas-gate")) {:clientX 100 :clientY 100})
    (mouse-down! (second (select-all ".canvas-gate"))
                 {:clientX 200 :clientY 200 :shiftKey true})

    (mouse-down! (first (select-all ".canvas-gate")) {:clientX 100 :clientY 100})
    (mouse-move! {:clientX 125 :clientY 130})

    (let [[first-item second-item] (select-all ".canvas-gate")]
      (is (= "120px" (.. first-item -style -left)))
      (is (= "140px" (.. first-item -style -top)))
      (is (= "220px" (.. second-item -style -left)))
      (is (= "240px" (.. second-item -style -top)))
      (is (= 2 (count (select-all ".canvas-item.selected")))))
    (mouse-up!)))

(deftest context-menu-actions-test
  (testing "right clicking a component opens a menu at the pointer"
    (drop-component! :h 100 100)
    (right-click! (select ".canvas-gate") 140 150)
    (let [menu (select "#context-menu")]
      (is (some? menu))
      (is (= "140px" (.. menu -style -left)))
      (is (= "150px" (.. menu -style -top)))
      (is (= 1 (count (select-all ".canvas-item.selected"))))))

  (testing "the rotate action rotates clockwise by pi/2"
    (drop-component! :x 200 200)
    (right-click! (select "[data-item-id=\"2\"]") 200 200)
    (.click (select "#context-rotate"))
    (is (nil? (select "#context-menu")))
    (is (= "rotate(90deg)"
           (.. (select "[data-item-id=\"2\"]") -style -transform))))

  (testing "the delete action removes the component"
    (right-click! (select "[data-item-id=\"2\"]") 200 200)
    (.click (select "#context-delete"))
    (is (nil? (select "[data-item-id=\"2\"]")))
    (is (nil? (select "#context-menu"))))

  (testing "clicking elsewhere closes the menu without acting"
    (right-click! (select "[data-item-id=\"1\"]") 100 100)
    (mouse-down! (select "#components-panel") {:clientX 10 :clientY 10})
    (is (nil? (select "#context-menu")))
    (is (some? (select "[data-item-id=\"1\"]")))))

(deftest context-menu-shortcuts-test
  (testing "R rotates the target component"
    (drop-component! :h 100 100)
    (right-click! (select ".canvas-gate") 100 100)
    (key-down! "r")
    (is (nil? (select "#context-menu")))
    (is (= "rotate(90deg)" (.. (select ".canvas-gate") -style -transform))))

  (testing "Escape deletes the target component"
    (right-click! (select ".canvas-gate") 100 100)
    (key-down! "Escape")
    (is (nil? (select ".canvas-gate")))
    (is (nil? (select "#context-menu")))))

(deftest keyboard-selection-shortcuts-test
  (testing "R rotates the selected component without a context menu"
    (drop-component! :h 100 100)
    (mouse-down! (select ".canvas-gate") {:clientX 100 :clientY 100})
    (key-down! "r")
    (is (= "rotate(90deg)" (.. (select ".canvas-gate") -style -transform)))
    (key-down! "R")
    (is (= "rotate(180deg)" (.. (select ".canvas-gate") -style -transform))))

  (testing "Escape deletes the selection without a context menu"
    (key-down! "Escape")
    (is (nil? (select ".canvas-gate")))
    (is (nil? (select "#context-menu")))))

(deftest escape-cancels-dialog-test
  (testing "Escape closes the register dialog without adding a register"
    (drop-component! :quantum-register 10 10)
    (is (some? (select "#register-form")))
    (key-down! "Escape")
    (is (nil? (select "#register-form")))
    (is (= 0 (count (select-all ".canvas-register"))))))

(defn- toggle-wire-tool! []
  (.click (select "#wire-tool")))

(defn- ensure-wire-tool! []
  (when-not (some? (select ".canvas-wire-mode"))
    (toggle-wire-tool!)))

(defn- mouse-up-on! [target options]
  (.dispatchEvent target (mouse-event "mouseup" options)))

(defn- draw-wire!
  ([from-id to-id]
   (draw-wire! from-id to-id 100 100 200 200))
  ([from-id to-id from-x from-y to-x to-y]
   (ensure-wire-tool!)
   (mouse-down! (select (str "[data-item-id=\"" from-id "\"]"))
                {:clientX from-x :clientY from-y})
   (mouse-move! {:clientX to-x :clientY to-y})
   (mouse-up-on! (select (str "[data-item-id=\"" to-id "\"]"))
                 {:clientX to-x :clientY to-y})))

(defn- polyline-points [node]
  (mapv (fn [point]
          (mapv js/parseFloat (str/split point #",")))
        (str/split (.getAttribute node "points") #" ")))

(defn- axis-aligned? [points]
  (every? (fn [[[x1 y1] [x2 y2]]]
            (or (= x1 x2) (= y1 y2)))
          (partition 2 1 points)))

(deftest wire-tool-toggle-test
  (testing "the wire tool can be toggled from the canvas toolbar"
    (let [button (select "#wire-tool")]
      (is (some? button))
      (is (not (str/includes? (.-className button) "active")))
      (toggle-wire-tool!)
      (is (str/includes? (.-className (select "#wire-tool")) "active"))
      (is (str/includes? (.-className (select "#canvas-panel")) "canvas-wire-mode"))
      (toggle-wire-tool!)
      (is (not (str/includes? (.-className (select "#wire-tool")) "active")))
      (is (not (str/includes? (.-className (select "#canvas-panel"))
                              "canvas-wire-mode"))))))

(deftest draw-wire-test
  (testing "dragging between two components draws a wire"
    (drop-component! :h 100 100)
    (drop-component! :x 200 200)
    (draw-wire! 1 2)
    (let [wire (select ".canvas-wire")
          points (polyline-points wire)]
      (is (some? wire))
      (is (= "1" (.getAttribute wire "data-from")))
      (is (= "2" (.getAttribute wire "data-to")))
      (is (nil? (select "#wire-draft")))
      (is (= [[144 118] [172 118] [172 218] [200 218]] points))
      (is (axis-aligned? points)))))

(deftest wire-preview-test
  (testing "a dashed preview follows the pointer while wiring"
    (drop-component! :h 100 100)
    (drop-component! :x 200 200)
    (toggle-wire-tool!)
    (mouse-down! (select "[data-item-id=\"1\"]") {:clientX 100 :clientY 100})
    (mouse-move! {:clientX 150 :clientY 160})
    (let [draft (select "#wire-draft")
          points (polyline-points draft)]
      (is (some? draft))
      (is (= [[144 118] [147 118] [147 160] [150 160]] points))
      (is (axis-aligned? points)))
    (mouse-up-on! (select "#canvas-panel") {:clientX 150 :clientY 160})
    (is (some? (select "#wire-draft")))
    (is (= [[144 118] [147 118] [147 160] [150 160]]
           (polyline-points (select "#wire-draft"))))
    (is (nil? (select "[data-wire-id]")))
    (key-down! "Escape")
    (is (nil? (select "#wire-draft")))
    (is (nil? (select "[data-wire-id]")))))

(deftest wire-click-connect-test
  (testing "clicking two components connects them with a direct route"
    (drop-component! :h 100 100)
    (drop-component! :x 200 200)
    (toggle-wire-tool!)
    (mouse-down! (select "[data-item-id=\"1\"]") {:clientX 100 :clientY 100})
    (mouse-up-on! (select "[data-item-id=\"1\"]") {:clientX 100 :clientY 100})
    (mouse-down! (select "[data-item-id=\"2\"]") {:clientX 200 :clientY 200})
    (mouse-up-on! (select "[data-item-id=\"2\"]") {:clientX 200 :clientY 200})
    (is (nil? (select "#wire-draft")))
    (is (= [[144 118] [172 118] [172 218] [200 218]]
           (polyline-points (select "[data-wire-id]"))))))

(deftest wire-click-routing-test
  (testing "clicking a component starts a wire and canvas clicks add corners"
    (drop-component! :h 100 100)
    (drop-component! :x 200 200)
    (toggle-wire-tool!)
    (let [source (select "[data-item-id=\"1\"]")]
      (mouse-down! source {:clientX 100 :clientY 100})
      (mouse-up-on! (select "[data-item-id=\"1\"]") {:clientX 100 :clientY 100})
      (is (some? (select "#wire-draft")))

      (mouse-move! {:clientX 150 :clientY 160})
      (mouse-down! (select "#canvas-panel") {:clientX 150 :clientY 160})
      (mouse-up-on! (select "#canvas-panel") {:clientX 150 :clientY 160})
      (is (= [[144 118] [147 118] [147 160] [150 160]]
             (polyline-points (select "#wire-draft"))))

      (mouse-move! {:clientX 180 :clientY 190})
      (is (= [[144 118] [147 118] [147 160] [150 160]
              [165 160] [165 190] [180 190]]
             (polyline-points (select "#wire-draft"))))

      (mouse-down! (select "[data-item-id=\"2\"]") {:clientX 200 :clientY 200})
      (mouse-up-on! (select "[data-item-id=\"2\"]") {:clientX 200 :clientY 200})
      (is (nil? (select "#wire-draft")))
      (is (= [[144 118] [147 118] [147 160] [150 160]
              [175 160] [175 218] [200 218]]
             (polyline-points (select "[data-wire-id]")))))))

(deftest wire-follows-component-test
  (testing "a wire stays attached when a component moves"
    (drop-component! :h 100 100)
    (drop-component! :x 200 200)
    (draw-wire! 1 2)
    (toggle-wire-tool!)
    (mouse-down! (select "[data-item-id=\"1\"]") {:clientX 100 :clientY 100})
    (mouse-move! {:clientX 120 :clientY 110})
    (mouse-up!)
    (let [wire (select ".canvas-wire")
          points (polyline-points wire)]
      (is (= [[164 138] [182 138] [182 218] [200 218]] points))
      (is (axis-aligned? points)))))

(deftest wire-validation-test
  (testing "self wires are not created"
    (drop-component! :h 100 100)
    (draw-wire! 1 1)
    (is (nil? (select "[data-wire-id]"))))

  (testing "duplicate wires are not created"
    (drop-component! :x 200 200)
    (draw-wire! 1 2)
    (draw-wire! 1 2)
    (is (= 1 (count (select-all ".canvas-wire"))))))

(deftest no-register-to-register-wire-test
  (testing "two registers cannot be connected"
    (drop-component! :quantum-register 100 100)
    (submit-register! "a" "2")
    (drop-component! :quantum-register 200 200)
    (submit-register! "b" "2")
    (draw-wire! 1 2)
    (is (nil? (select ".canvas-wire")))))

(deftest register-to-gate-wire-test
  (testing "a register can be connected to a gate"
    (drop-component! :quantum-register 100 100)
    (submit-register! "a" "2")
    (drop-component! :h 200 200)
    (draw-wire! 1 2)
    (is (= 1 (count (select-all ".canvas-wire"))))))

(deftest escape-cancels-wire-test
  (testing "Escape cancels a wire in progress and leaves the wire tool"
    (drop-component! :h 100 100)
    (drop-component! :x 200 200)
    (toggle-wire-tool!)
    (mouse-down! (select "[data-item-id=\"1\"]") {:clientX 100 :clientY 100})
    (is (some? (select "#wire-draft")))
    (key-down! "Escape")
    (is (nil? (select "#wire-draft")))
    (is (nil? (select ".canvas-wire")))
    (is (not (str/includes? (.-className (select "#canvas-panel"))
                            "canvas-wire-mode")))))

(deftest delete-wire-with-component-test
  (testing "deleting a component removes its wires"
    (drop-component! :h 100 100)
    (drop-component! :x 200 200)
    (draw-wire! 1 2)
    (toggle-wire-tool!)
    (right-click! (select "[data-item-id=\"2\"]") 200 200)
    (.click (select "#context-delete"))
    (is (nil? (select "[data-item-id=\"2\"]")))
    (is (nil? (select ".canvas-wire")))))

(deftest keyboard-wire-shortcut-test
  (testing "W toggles the wire tool"
    (key-down! "w")
    (is (str/includes? (.-className (select "#wire-tool")) "active"))
    (is (some? (select ".canvas-wire-mode")))
    (key-down! "W")
    (is (not (str/includes? (.-className (select "#wire-tool")) "active")))
    (is (nil? (select ".canvas-wire-mode")))))

(defn- wheel-on! [target x y delta-y]
  (.dispatchEvent target
                  (js/window.WheelEvent.
                   "wheel"
                   (clj->js {:bubbles true
                             :cancelable true
                             :clientX x
                             :clientY y
                             :deltaY delta-y}))))

(defn- wheel! [x y delta-y]
  (wheel-on! (select "#canvas-panel") x y delta-y))

(defn- stage-transform []
  (.. (select "#canvas-stage") -style -transform))

(defn- status-text [id]
  (.-textContent (select id)))

(deftest cad-status-bar-test
  (testing "the canvas exposes CAD-style status information"
    (is (some? (select "#canvas-status")))
    (is (= "Zoom 100%" (status-text "#zoom-level")))
    (is (= "Grid 20" (status-text "#grid-size")))
    (is (= "Snap on" (status-text "#snap-state")))
    (is (= "X --   Y --" (status-text "#cursor-coordinates"))))

  (testing "moving the pointer reports world coordinates"
    (mouse-move! {:clientX 40 :clientY 60})
    (is (= "X 40   Y 60" (status-text "#cursor-coordinates")))))

(deftest cad-grid-snap-test
  (testing "components snap to the visible grid when placed"
    (drop-component! :h 7 8)
    (let [gate (select ".canvas-gate")]
      (is (= "0px" (.. gate -style -left)))
      (is (= "0px" (.. gate -style -top)))))

  (testing "the snap toggle disables snapping"
    (let [button (select "#snap-toggle")]
      (is (str/includes? (.-className button) "active"))
      (is (str/includes? (.-className (select "#canvas-panel")) "canvas-snap-on"))
      (.click button)
      (is (not (str/includes? (.-className (select "#snap-toggle")) "active")))
      (is (not (str/includes? (.-className (select "#canvas-panel"))
                              "canvas-snap-on")))
      (is (= "Snap off" (status-text "#snap-state")))
      (drop-component! :x 7 8)
      (is (= "7px" (.. (select "[data-item-id=\"2\"]") -style -left)))
      (is (= "8px" (.. (select "[data-item-id=\"2\"]") -style -top)))))

  (testing "G toggles snapping from the keyboard"
    (key-down! "g")
    (is (= "Snap on" (status-text "#snap-state")))
    (is (str/includes? (.-className (select "#canvas-panel")) "canvas-snap-on"))
    (key-down! "G")
    (is (= "Snap off" (status-text "#snap-state")))))

(deftest cad-zoom-test
  (testing "the mouse wheel zooms around the pointer"
    (wheel! 100 100 -100)
    (is (not= "Zoom 100%" (status-text "#zoom-level")))
    (is (str/includes? (stage-transform) "scale("))
    (wheel! 100 100 100)
    (is (= "Zoom 100%" (status-text "#zoom-level"))))

  (testing "wheel events over canvas children zoom the canvas"
    (drop-component! :h 100 100)
    (wheel-on! (select ".canvas-gate") 100 100 -100)
    (is (not= "Zoom 100%" (status-text "#zoom-level")))))

(deftest cad-pan-test
  (testing "the middle mouse button pans the canvas"
    (mouse-down! (select "#canvas-panel") {:button 1 :clientX 10 :clientY 20})
    (mouse-move! {:clientX 40 :clientY 60})
    (mouse-up!)
    (is (= "translate(30px, 40px) scale(1)" (stage-transform))))

  (testing "zero resets the view"
    (key-down! "0")
    (is (= "translate(0px, 0px) scale(1)" (stage-transform)))))

(deftest cad-pan-aware-drop-test
  (testing "drops are placed in world coordinates after panning"
    (mouse-down! (select "#canvas-panel") {:button 1 :clientX 0 :clientY 0})
    (mouse-move! {:clientX 30 :clientY 40})
    (mouse-up!)
    (drop-component! :h 130 140)
    (let [gate (select ".canvas-gate")]
      (is (= "100px" (.. gate -style -left)))
      (is (= "100px" (.. gate -style -top))))))

(deftest cad-marquee-selection-test
  (testing "dragging on empty canvas draws a selection marquee"
    (drop-component! :h 100 100)
    (drop-component! :x 300 300)
    (mouse-down! (select "#canvas-panel") {:clientX 50 :clientY 50})
    (mouse-move! {:clientX 200 :clientY 200})
    (let [marquee (select "#canvas-marquee")]
      (is (some? marquee))
      (is (= "50" (.getAttribute marquee "x")))
      (is (= "50" (.getAttribute marquee "y")))
      (is (= "150" (.getAttribute marquee "width")))
      (is (= "150" (.getAttribute marquee "height"))))
    (is (= ["1"] (mapv #(.getAttribute % "data-item-id")
                       (select-all ".canvas-item.selected"))))
    (mouse-up!)
    (is (nil? (select "#canvas-marquee")))
    (is (= ["1"] (mapv #(.getAttribute % "data-item-id")
                       (select-all ".canvas-item.selected"))))))

(deftest cad-marquee-escape-test
  (testing "Escape cancels an in-progress marquee selection"
    (drop-component! :h 100 100)
    (mouse-down! (select "#canvas-panel") {:clientX 50 :clientY 50})
    (mouse-move! {:clientX 200 :clientY 200})
    (is (some? (select "#canvas-marquee")))
    (key-down! "Escape")
    (is (nil? (select "#canvas-marquee")))
    (is (= 0 (count (select-all ".canvas-item.selected"))))))

(defn- save-api
  [calls callbacks unsubscribed]
  #js {:ping (fn [] (js/Promise.resolve "pong"))
       :saveQasm (fn [content suggested-name]
                   (swap! calls conj [content suggested-name])
                   (js/Promise.resolve #js {:canceled false :filePath "circuit.qasm"}))
       :onMenuSave (fn [callback]
                     (swap! callbacks conj callback)
                     (fn [] (swap! unsubscribed inc)))})

(deftest save-qasm-menu-test
  (testing "the File Save menu saves the drawn circuit without a toolbar button"
    (let [calls (atom [])
          callbacks (atom [])
          unsubscribed (atom 0)]
      (reset-app-with-api! (save-api calls callbacks unsubscribed))
      (is (nil? (select "#save-qasm")))
      (is (= 1 (count @callbacks)))
      (drop-component! :quantum-register 40 40)
      (submit-register! "data" "2")
      (drop-component! :h 200 40)
      (draw-wire! 1 2 40 40 200 40)
      ((first @callbacks))
      (is (= 1 (count @calls)))
      (let [[content suggested-name] (first @calls)]
        (is (= "circuit.qasm" suggested-name))
        (is (str/includes? content "qubit[2] data;"))
        (is (str/includes? content "h data[0];")))
      (stop-app!)
      (is (= 1 @unsubscribed)))))

(defn- thenable [result]
  #js {:then (fn [callback] (thenable (callback result)))
       :catch (fn [_] (thenable nil))})

(defn- open-api [calls callbacks unsubscribed result]
  #js {:openQasm (fn []
                   (swap! calls conj true)
                   (thenable result))
       :onMenuOpen (fn [callback]
                     (swap! callbacks conj callback)
                     (fn [] (swap! unsubscribed inc)))})

(deftest open-qasm-menu-test
  (testing "the File Open menu draws an imported circuit automatically"
    (let [calls (atom [])
          callbacks (atom [])
          unsubscribed (atom 0)
          content (str "OPENQASM 3.0;\n"
                       "include \"stdgates.inc\";\n"
                       "qubit[2] data;\n"
                       "h data[0];\n")]
      (reset-app-with-api! (open-api calls callbacks unsubscribed
                                     #js {:canceled false
                                          :filePath "circuit.qasm"
                                          :content content}))
      (is (= 1 (count @callbacks)))
      ((first @callbacks))
      (is (= 1 (count @calls)))
      (is (= "data (2 qubits)" (.-textContent (select ".canvas-register"))))
      (is (= "H" (.-textContent (select ".canvas-gate"))))
      (is (= 1 (count (select-all ".canvas-wire"))))
      (stop-app!)
      (is (= 1 @unsubscribed))))

  (testing "cancelling the file dialog leaves the current circuit unchanged"
    (let [calls (atom [])
          callbacks (atom [])
          unsubscribed (atom 0)]
      (reset-app-with-api! (open-api calls callbacks unsubscribed
                                     #js {:canceled true}))
      (drop-component! :h 100 100)
      ((first @callbacks))
      (is (= 1 (count @calls)))
      (is (= 1 (count (select-all ".canvas-gate")))))))

(defn- double-click! [target]
  (.dispatchEvent target (mouse-event "dblclick" {})))

(defn- item-ports [item-id kind]
  (vec (array-seq (.querySelectorAll (select (str "[data-item-id=\"" item-id "\"]"))
                                     (str "[data-port-kind=\"" kind "\"]")))))

(defn- port-node [item-id kind index]
  (first (filter #(= (str index) (.getAttribute % "data-port-index"))
                 (item-ports item-id kind))))

(defn- submit-gate-qubits! [qubits]
  (set! (.-value (select "#gate-qubits")) qubits)
  (.dispatchEvent (select "#gate-qubits-form")
                  (js/window.Event. "submit" #js {:bubbles true :cancelable true})))

(defn- submit-custom! [qubits qasm]
  (set! (.-value (select "#custom-qubits")) qubits)
  (set! (.-value (select "#custom-qasm")) qasm)
  (.dispatchEvent (select "#custom-form")
                  (js/window.Event. "submit" #js {:bubbles true :cancelable true})))

(defn- wire-between-ports!
  [from-id from-index to-id to-index]
  (ensure-wire-tool!)
  (mouse-down! (port-node from-id "output" from-index)
               {:clientX 0 :clientY 0})
  (mouse-up-on! (port-node from-id "output" from-index)
                {:clientX 0 :clientY 0})
  (mouse-down! (port-node to-id "input" to-index)
               {:clientX 0 :clientY 0})
  (mouse-up-on! (port-node to-id "input" to-index)
                {:clientX 0 :clientY 0}))

(deftest gate-qubits-dialog-test
  (testing "double clicking a gate opens a number-only dialog"
    (drop-component! :h 100 100)
    (double-click! (select "[data-item-id=\"1\"]"))
    (is (some? (select "#gate-qubits-form")))
    (is (nil? (select "[data-qubit-index]")))
    (is (= "1" (.-value (select "#gate-qubits"))))
    (submit-gate-qubits! "2")
    (is (nil? (select "#gate-qubits-form")))
    (is (= 2 (count (item-ports 1 "input"))))
    (is (= 2 (count (item-ports 1 "output")))))

  (testing "invalid counts keep the dialog open with an error"
    (reset-app!)
    (drop-component! :h 100 100)
    (double-click! (select "[data-item-id=\"1\"]"))
    (submit-gate-qubits! "0")
    (is (some? (select "#gate-qubits-form")))
    (is (= "Number of qubits must be a positive whole number."
           (.-textContent (select ".dialog-error"))))
    (is (= 1 (count (item-ports 1 "input"))))))

(deftest custom-gate-dialog-test
  (testing "dropping the custom component opens its configuration dialog"
    (drop-component! :custom 100 100)
    (is (some? (select "#custom-form")))
    (is (= "1" (.-value (select "#custom-qubits"))))
    (is (= "" (.-value (select "#custom-qasm"))))
    (is (= 0 (count (select-all ".canvas-item"))))
    (is (= "dialog" (.getAttribute (select ".dialog") "role"))))

  (testing "submitting a valid definition places a custom gate with ports"
    (submit-custom! "2" "gate pair a, b {\n  h a;\n  cx a, b;\n}")
    (let [gate (select ".canvas-gate")]
      (is (nil? (select "#custom-form")))
      (is (some? gate))
      (is (= "Custom" (.-textContent gate)))
      (is (= 2 (count (item-ports 1 "input"))))
      (is (= 2 (count (item-ports 1 "output"))))))

  (testing "an invalid definition keeps the dialog open with an error"
    (reset-app!)
    (drop-component! :custom 100 100)
    (submit-custom! "0" " ")
    (is (some? (select "#custom-form")))
    (is (= "0" (.-value (select "#custom-qubits"))))
    (is (= " " (.-value (select "#custom-qasm"))))
    (is (some? (select ".dialog-error")))
    (is (= 0 (count (select-all ".canvas-item")))))

  (testing "cancelling closes the dialog without adding a gate"
    (reset-app!)
    (drop-component! :custom 100 100)
    (.click (select "#custom-cancel"))
    (is (nil? (select "#custom-form")))
    (is (= 0 (count (select-all ".canvas-item"))))))

(deftest custom-gate-edit-test
  (testing "double clicking a custom gate reopens its definition dialog"
    (drop-component! :custom 100 100)
    (submit-custom! "2" "gate pair a, b { h a; }")
    (double-click! (select "[data-item-id=\"1\"]"))
    (is (some? (select "#custom-form")))
    (is (= "2" (.-value (select "#custom-qubits"))))
    (is (= "gate pair a, b { h a; }" (.-value (select "#custom-qasm"))))
    (submit-custom! "1" "gate solo q { x q; }")
    (is (nil? (select "#custom-form")))
    (is (= 1 (count (item-ports 1 "input"))))
    (is (= 1 (count (select-all ".canvas-gate"))))))

(deftest custom-gate-save-test
  (testing "saving emits the custom definition and one call with wired operands"
    (let [calls (atom [])
          callbacks (atom [])
          unsubscribed (atom 0)]
      (reset-app-with-api! (save-api calls callbacks unsubscribed))
      (drop-component! :quantum-register 40 40)
      (submit-register! "data" "2")
      (drop-component! :custom 240 40)
      (submit-custom! "2" "gate bell a, b {\n  h a;\n  cx a, b;\n}")
      (wire-between-ports! 1 0 2 0)
      (wire-between-ports! 1 1 2 1)
      ((first @callbacks))
      (let [[content] (first @calls)]
        (is (str/includes? content "gate bell a, b {"))
        (is (str/includes? content "bell data[0], data[1];"))))))

(deftest incomplete-custom-gate-save-test
  (testing "saving an incompletely wired custom gate omits the invalid call"
    (let [calls (atom [])
          callbacks (atom [])
          unsubscribed (atom 0)]
      (reset-app-with-api! (save-api calls callbacks unsubscribed))
      (drop-component! :quantum-register 40 40)
      (submit-register! "register_1" "2")
      (drop-component! :custom 240 40)
      (submit-custom! "2" "gate pair a, b {\n  h a;\n  cx a, b;\n}")
      (wire-between-ports! 1 0 2 0)
      ((first @callbacks))
      (let [[content] (first @calls)]
        (is (str/includes? content "gate pair a, b {"))
        (is (not (str/includes? content "pair register_1[0];")))
        (is (str/includes? content "1 of 2 qubits connected"))))))

(deftest gate-multi-register-target-test
  (testing "a gate can target qubits from more than one register by port order"
    (let [calls (atom [])
          callbacks (atom [])
          unsubscribed (atom 0)]
      (reset-app-with-api! (save-api calls callbacks unsubscribed))
      (drop-component! :quantum-register 40 40)
      (submit-register! "a" "2")
      (drop-component! :quantum-register 40 140)
      (submit-register! "b" "2")
      (drop-component! :h 240 40)
      (double-click! (select "[data-item-id=\"3\"]"))
      (submit-gate-qubits! "2")
      (wire-between-ports! 1 1 3 0)
      (wire-between-ports! 2 0 3 1)
      ((first @callbacks))
      (let [[content] (first @calls)]
        (is (str/includes? content "h a[1];"))
        (is (str/includes? content "h b[0];"))))))

(deftest selection-mousedown-test
  (testing "selecting an item updates classes without replacing its node"
    (drop-component! :h 100 100)
    (let [node (select "[data-item-id=\"1\"]")]
      (mouse-down! node {:clientX 100 :clientY 100})
      (is (identical? node (select "[data-item-id=\"1\"]")))
      (is (.contains (.-classList node) "selected")))))

(deftest register-output-ports-test
  (testing "a register shows one output port per qubit"
    (drop-component! :quantum-register 40 40)
    (submit-register! "data" "3")
    (let [ports (item-ports 1 "output")]
      (is (= 3 (count ports)))
      (is (= ["0" "1" "2"] (mapv #(.getAttribute % "data-port-index") ports)))
      (is (empty? (item-ports 1 "input"))))))

(deftest wire-port-snapping-test
  (testing "wires start and end on the clicked ports"
    (drop-component! :quantum-register 40 40)
    (submit-register! "data" "3")
    (drop-component! :h 200 40)
    (wire-between-ports! 1 1 2 0)
    (let [wire (select ".canvas-wire:not(.canvas-wire-draft)")]
      (is (some? wire))
      (is (= "1" (.getAttribute wire "data-from-port")))
      (is (= "0" (.getAttribute wire "data-to-port")))
      (is (nil? (select "#wire-draft"))))))

(deftest wire-driven-multi-qubit-target-test
  (testing "one register wire targets one qubit; more ports allow more wires"
    (let [calls (atom [])
          callbacks (atom [])
          unsubscribed (atom 0)]
      (reset-app-with-api! (save-api calls callbacks unsubscribed))
      (drop-component! :quantum-register 40 40)
      (submit-register! "data" "3")
      (drop-component! :h 200 40)
      (wire-between-ports! 1 0 2 0)
      (toggle-wire-tool!)
      (double-click! (select "[data-item-id=\"2\"]"))
      (submit-gate-qubits! "2")
      (is (= 2 (count (item-ports 2 "input"))))
      (is (= 2 (count (item-ports 2 "output"))))
      (wire-between-ports! 1 1 2 1)
      (toggle-wire-tool!)
      ((first @callbacks))
      (let [[content] (first @calls)]
        (is (str/includes? content "h data[0];"))
        (is (str/includes? content "h data[1];"))
        (is (not (str/includes? content "h data;")))))))

(deftest gate-ports-test
  (testing "gates grow ports to match their configured qubit count"
    (drop-component! :h 200 40)
    (is (= 1 (count (item-ports 1 "input"))))
    (is (= 1 (count (item-ports 1 "output"))))
    (double-click! (select "[data-item-id=\"1\"]"))
    (submit-gate-qubits! "2")
    (is (= 2 (count (item-ports 1 "input"))))
    (is (= 2 (count (item-ports 1 "output"))))))

(deftest gate-qubit-count-shrink-test
  (testing "shrinking a gate removes overflow ports and their wires"
    (drop-component! :quantum-register 40 40)
    (submit-register! "data" "3")
    (drop-component! :h 200 40)
    (double-click! (select "[data-item-id=\"2\"]"))
    (submit-gate-qubits! "2")
    (wire-between-ports! 1 0 2 0)
    (wire-between-ports! 1 1 2 1)
    (toggle-wire-tool!)
    (is (= 2 (count (item-ports 2 "input"))))
    (double-click! (select "[data-item-id=\"2\"]"))
    (submit-gate-qubits! "1")
    (is (= 1 (count (item-ports 2 "input"))))
    (is (= 1 (count (select-all ".canvas-wire:not(.canvas-wire-draft)"))))))
