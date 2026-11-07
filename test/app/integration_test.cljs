(ns app.integration-test
  (:require [clojure.string :as str]
            [cljs.test :refer [deftest is testing use-fixtures]]
            [app.core :as core]
            [app.dom :as dom]
            ["jsdom" :as jsdom]))

(defonce ^:private dom
  (jsdom/JSDOM. "<!doctype html><html><body><div id=\"app\"></div></body></html>"))

(set! (.-window js/globalThis) (.-window dom))
(set! (.-document js/globalThis) (.-document (.-window dom)))

(defonce ^:private storage-data (atom {}))

(defn- fake-storage []
  #js {:getItem (fn [key] (get @storage-data key))
       :setItem (fn [key value] (swap! storage-data assoc key (str value)))
       :removeItem (fn [key] (swap! storage-data dissoc key))
       :clear (fn [] (reset! storage-data {}))})

(set! (.-localStorage js/globalThis) (fake-storage))

(defn- clear-storage! []
  (reset! storage-data {})
  (when-let [storage (.-localStorage js/globalThis)]
    (when (.-clear storage) (.clear storage))))

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
  (clear-storage!)
  (set! (.-innerHTML (.-body (document))) "<div id=\"app\"></div>")
  (set! (.-electronAPI js/window) nil)
  (reset! session (core/init!)))

(defn- reset-app-with-api! [api]
  (stop-app!)
  (clear-storage!)
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

(defn- dragstart-block! [attribute value]
  (let [node (select (str "[" attribute "=\"" value "\"]"))
        event (js/window.Event. "dragstart" #js {:bubbles true :cancelable true})
        data-transfer (fake-data-transfer)]
    (is (some? node))
    (set! (.-dataTransfer event) data-transfer)
    (.dispatchEvent node event)
    data-transfer))

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

(defn- mouse-move-on! [target options]
  (.dispatchEvent target (mouse-event "mousemove" options)))

(defn- mouse-move! [options]
  (mouse-move-on! (document) options))

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
      (is (= "1" (.. canvas -style -flexGrow)))
      (is (some? (.closest (select "#wire-tool") ".app-toolbar")))
      (is (nil? (.closest (select "#wire-tool") "#canvas-panel"))))))

(deftest components-panel-test
  (testing "all required components are rendered as draggable rectangles"
    (let [tiles (select-all ".component-tile")]
      (is (= 25 (count tiles)))
      (is (= ["quantum-register" "classical-register" "h" "x" "y" "z" "s" "t"
              "sdag" "tdag" "rx" "ry" "rz" "p" "i" "cx" "cz" "swap" "ccx"
              "measure" "reset" "barrier" "custom" "annotation" "box"]
             (mapv #(.getAttribute % "data-component-id") tiles)))
      (is (every? #(= "true" (.getAttribute % "draggable")) tiles))
      (is (= "Quantum register" (.-textContent (first tiles))))
      (is (= "H" (.-textContent (nth tiles 2)))))))

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

(defn- change! [target]
  (.dispatchEvent target (js/window.Event. "change" #js {:bubbles true})))

(defn- input! [target value]
  (set! (.-value target) value)
  (.dispatchEvent target (js/window.Event. "input" #js {:bubbles true})))

(defn- json-save-api [calls callbacks]
  #js {:ping (fn [] (js/Promise.resolve "pong"))
       :saveFile (fn [content suggested-name filters]
                   (swap! calls conj [content suggested-name filters])
                   (thenable #js {:canceled false :filePath suggested-name}))
       :onMenuSave (fn [callback]
                     (swap! callbacks conj callback)
                     (fn [] nil))})

(defn- export-api [calls]
  #js {:ping (fn [] (js/Promise.resolve "pong"))
       :saveFile (fn [content suggested-name filters]
                   (swap! calls conj {:content content
                                      :name suggested-name
                                      :filters filters})
                   (thenable #js {:canceled false}))
       :saveBinary (fn [base64 suggested-name filters]
                     (swap! calls conj {:base64 base64
                                        :name suggested-name
                                        :filters filters})
                     (thenable #js {:canceled false}))})

(deftest undo-redo-buttons-test
  (testing "undo and redo work from the canvas toolbar"
    (drop-component! :h 100 100)
    (is (= 1 (count (select-all ".canvas-gate"))))
    (.click (select "#undo-button"))
    (is (= 0 (count (select-all ".canvas-gate"))))
    (is (not (.-disabled (select "#redo-button"))))
    (.click (select "#redo-button"))
    (is (= 1 (count (select-all ".canvas-gate"))))
    (key-down! "z" {:ctrlKey true})
    (is (= 0 (count (select-all ".canvas-gate"))))
    (key-down! "z" {:ctrlKey true :shiftKey true})
    (is (= 1 (count (select-all ".canvas-gate"))))))

(deftest copy-paste-test
  (testing "copy and paste duplicate the selection with an offset"
    (drop-component! :h 100 100)
    (mouse-down! (select "[data-item-id=\"1\"]") {:clientX 100 :clientY 100})
    (mouse-up!)
    (key-down! "c" {:ctrlKey true})
    (key-down! "v" {:ctrlKey true})
    (let [gates (select-all ".canvas-gate")]
      (is (= 2 (count gates)))
      (is (= ["100px" "20px"] (mapv #(.. % -style -left) gates)))
      (is (= ["100px" "20px"] (mapv #(.. % -style -top) gates)))
      (is (= 1 (count (select-all ".canvas-gate.selected")))))))

(defn- submit-gate-details! [qubits angle controls]
  (when-let [input (select "#gate-qubits")]
    (set! (.-value input) qubits))
  (when-let [input (select "#gate-angle")]
    (set! (.-value input) angle))
  (when-let [input (select "#gate-controls")]
    (set! (.-value input) controls))
  (.dispatchEvent (select "#gate-qubits-form")
                  (js/window.Event. "submit" #js {:bubbles true :cancelable true})))

(deftest gate-angle-save-test
  (testing "the angle entered in the gate dialog is exported"
    (let [calls (atom [])
          callbacks (atom [])]
      (reset-app-with-api! (save-api calls callbacks (atom 0)))
      (drop-component! :quantum-register 40 40)
      (submit-register! "data" "1")
      (drop-component! :rx 240 40)
      (double-click! (select "[data-item-id=\"2\"]"))
      (is (some? (select "#gate-angle")))
      (submit-gate-details! "1" "pi/4" "0")
      (wire-between-ports! 1 0 2 0)
      ((first @callbacks))
      (let [[content] (first @calls)]
        (is (str/includes? content "rx(pi/4) data[0];"))))))

(deftest gate-control-save-test
  (testing "controls entered in the gate dialog export a controlled gate"
    (let [calls (atom [])
          callbacks (atom [])]
      (reset-app-with-api! (save-api calls callbacks (atom 0)))
      (drop-component! :quantum-register 40 40)
      (submit-register! "data" "2")
      (drop-component! :h 240 40)
      (double-click! (select "[data-item-id=\"2\"]"))
      (submit-gate-details! "2" "" "1")
      (wire-between-ports! 1 0 2 0)
      (wire-between-ports! 1 1 2 1)
      ((first @callbacks))
      (let [[content] (first @calls)]
        (is (str/includes? content "ctrl @ h data[0], data[1];"))))))

(deftest barrier-rendering-test
  (testing "the barrier component gets its own canvas style"
    (drop-component! :barrier 100 100)
    (let [gate (select ".canvas-gate")]
      (is (some? gate))
      (is (.contains (.-classList gate) "canvas-barrier"))
      (is (= "Barrier" (.-textContent gate))))))

(deftest measure-and-classical-register-test
  (testing "a measurement can be wired to a classical register and saved"
    (let [calls (atom [])
          callbacks (atom [])]
      (reset-app-with-api! (save-api calls callbacks (atom 0)))
      (drop-component! :quantum-register 40 40)
      (submit-register! "q" "2")
      (drop-component! :classical-register 40 220)
      (submit-register! "c" "2")
      (let [classical (select ".canvas-classical-register")]
        (is (some? classical))
        (is (= "c (2 bits)" (.-textContent classical)))
        (is (= 2 (count (item-ports 2 "input")))))
      (drop-component! :measure 240 40)
      (let [measure (select "[data-item-id=\"3\"]")]
        (is (.contains (.-classList measure) "canvas-measure")))
      (wire-between-ports! 1 0 3 0)
      (wire-between-ports! 3 0 2 1)
      (toggle-wire-tool!)
      ((first @callbacks))
      (let [[content] (first @calls)]
        (is (str/includes? content "bit[2] c;"))
        (is (str/includes? content "measure q[0] -> c[1];"))))))

(deftest metadata-dialog-test
  (testing "circuit metadata is edited and shown in the properties panel"
    (.click (select "#properties-button"))
    (is (some? (select "#metadata-form")))
    (set! (.-value (select "#metadata-name")) "Bell")
    (set! (.-value (select "#metadata-author")) "Ada")
    (set! (.-value (select "#metadata-description")) "Two qubit demo")
    (.click (select "#metadata-confirm"))
    (is (nil? (select "#metadata-form")))
    (is (str/includes? (.-textContent (select "#property-panel")) "Bell"))
    (is (str/includes? (.-textContent (select "#property-panel")) "Ada"))
    (is (str/includes? (.-textContent (select "#property-panel"))
                       "Two qubit demo"))
    (.click (select "#properties-button"))
    (is (= "Bell" (.-value (select "#metadata-name"))))
    (is (= "Ada" (.-value (select "#metadata-author"))))
    (is (= "Two qubit demo" (.-value (select "#metadata-description"))))))

(deftest diff-dialog-test
  (testing "the diff dialog compares the circuit with the last save"
    (let [calls (atom [])
          callbacks (atom [])]
      (reset-app-with-api! (json-save-api calls callbacks))
      (drop-component! :h 100 100)
      ((first @callbacks))
      (is (= 1 (count @calls)))
      (.click (select "#diff-button"))
      (is (some? (select "#diff-form")))
      (is (= "No differences." (.-textContent (select "#diff-content"))))
      (.click (select "#diff-close"))
      (is (nil? (select "#diff-form")))
      (double-click! (select "[data-item-id=\"1\"]"))
      (submit-gate-qubits! "2")
      (.click (select "#diff-button"))
      (is (str/includes? (.-textContent (select "#diff-content")) "~ gate 1 h"))
      (.click (select "#diff-close")))))

(deftest export-dialog-test
  (testing "SVG export options are applied and the file is saved"
    (let [calls (atom [])]
      (reset-app-with-api! (export-api calls))
      (drop-component! :h 100 100)
      (.click (select "#export-button"))
      (is (some? (select "#export-form")))
      (is (= ["svg" "png"] (mapv #(.-value %) (select-all "#export-format option"))))
      (is (= ["1" "2" "3"] (mapv #(.-value %) (select-all "#export-scale option"))))
      (is (str/includes? (.-textContent (select ".checkbox-label"))
                         "Transparent background"))
      (is (not (.-checked (select "#export-transparent"))))
      (set! (.-checked (select "#export-transparent")) true)
      (change! (select "#export-transparent"))
      (is (some? (select "#export-transparent")))
      (change! (select "#export-scale"))
      (.click (select "#export-confirm"))
      (is (nil? (select "#export-form")))
      (let [{:keys [content name]} (first @calls)]
        (is (= "circuit.svg" name))
        (is (str/includes? content "<svg"))
        (is (str/includes? content "H"))
        (is (not (str/includes? content "<rect x=\"-24\" y=\"-24\" width=")))))))

(deftest library-dialog-test
  (testing "templates and saved circuits are available in the library"
    (drop-component! :h 100 100)
    (.click (select "#library-button"))
    (is (some? (select "#library-form")))
    (is (= ["bell" "ghz" "qft" "grover" "teleportation"]
           (mapv #(.getAttribute % "data-template-id")
                 (select-all "[data-template-id]"))))
    (.click (select "[data-template-id=\"bell\"]"))
    (is (nil? (select "#library-form")))
    (is (= 4 (count (select-all ".canvas-item"))))
    (is (= 3 (count (select-all ".canvas-wire"))))
    (.click (select "#library-button"))
    (input! (select "#library-name") "My circuit")
    (.click (select "#library-save"))
    (is (some? (select "[data-library-name=\"My circuit\"]")))
    (.click (select "[data-library-delete=\"My circuit\"]"))
    (is (nil? (select "[data-library-name=\"My circuit\"]")))
    (.click (select "#library-cancel"))
    (is (nil? (select "#library-form")))))

(deftest search-and-grid-test
  (testing "the component search filters the palette"
    (input! (select "#component-search") "swap")
    (is (= ["swap"] (mapv #(.getAttribute % "data-component-id")
                          (select-all ".component-tile"))))
    (is (= "swap" (.-value (select "#component-search"))))
    (input! (select "#component-search") "")
    (is (= 25 (count (select-all ".component-tile")))))

  (testing "changing the grid size snaps subsequent drops"
    (let [select-node (select "#grid-size-select")]
      (set! (.-value select-node) "40")
      (change! select-node))
    (is (= "40" (.-value (select "#grid-size-select"))))
    (drop-component! :h 105 107)
    (let [gate (select ".canvas-gate")]
      (is (= "120px" (.. gate -style -left)))
      (is (= "120px" (.. gate -style -top))))))

(deftest theme-toggle-test
  (testing "the theme toggle switches between light and dark"
    (is (not (str/includes? (.-className (select ".app")) "theme-dark")))
    (.click (select "#theme-toggle"))
    (is (str/includes? (.-className (select ".app")) "theme-dark"))
    (.click (select "#theme-toggle"))
    (is (not (str/includes? (.-className (select ".app")) "theme-dark")))))

(deftest electron-clipboard-test
  (testing "copy and paste use the Electron clipboard when available"
    (let [clipboard (atom "")
          calls (atom [])
          api #js {:ping (fn [] (js/Promise.resolve "pong"))
                   :writeClipboard (fn [text]
                                     (reset! clipboard text)
                                     (swap! calls conj :write)
                                     (thenable nil))
                   :readClipboard (fn []
                                    (swap! calls conj :read)
                                    (thenable @clipboard))}]
      (reset-app-with-api! api)
      (drop-component! :h 100 100)
      (mouse-down! (select "[data-item-id=\"1\"]") {:clientX 100 :clientY 100})
      (mouse-up!)
      (key-down! "c" {:ctrlKey true})
      (is (str/includes? @clipboard "\"items\""))
      (key-down! "v" {:ctrlKey true})
      (is (= 2 (count (select-all ".canvas-gate"))))
      (is (= [:write :read] @calls)))))

(defn- menu-api [callbacks calls]
  #js {:ping (fn [] (js/Promise.resolve "pong"))
       :onMenuSave (fn [callback]
                     (swap! callbacks assoc :save callback)
                     (fn [] (swap! calls conj :save-unsub)))
       :onMenuExport (fn [callback]
                       (swap! callbacks assoc :export callback)
                       (fn [] (swap! calls conj :export-unsub)))
       :onMenuLibrary (fn [callback]
                        (swap! callbacks assoc :library callback)
                        (fn [] (swap! calls conj :library-unsub)))
       :onMenuOpenRecent (fn [callback]
                           (swap! callbacks assoc :recent callback)
                           (fn [] (swap! calls conj :recent-unsub)))
       :openFileAt (fn [path]
                     (swap! calls conj path)
                     (thenable #js {:canceled false
                                    :filePath path
                                    :content (str "OPENQASM 3.0;\n"
                                                  "qubit[1] q;\n"
                                                  "h q[0];\n")}))})

(deftest application-menu-test
  (testing "menu actions open dialogs and recent files"
    (let [callbacks (atom {})
          calls (atom [])]
      (reset-app-with-api! (menu-api callbacks calls))
      ((:export @callbacks))
      (is (some? (select "#export-form")))
      (.click (select "#export-cancel"))
      (is (nil? (select "#export-form")))
      ((:library @callbacks))
      (is (some? (select "#library-form")))
      (.click (select "#library-cancel"))
      (is (nil? (select "#library-form")))
      ((:recent @callbacks) "recent.qasm")
      (is (= "recent.qasm" (first @calls)))
      (is (= "q (1 qubit)" (.-textContent (select ".canvas-register"))))
      (is (= "H" (.-textContent (select ".canvas-gate"))))
      (stop-app!)
      (is (some #{:save-unsub :export-unsub :library-unsub :recent-unsub} @calls)))))

(deftest component-snap-toolbar-test
  (testing "the alignment toggle updates the toolbar and responds to A"
    (let [button (select "#component-snap-toggle")]
      (is (some? button))
      (is (not (str/includes? (.-className button) "active")))
      (.click button)
      (is (str/includes? (.-className (select "#component-snap-toggle")) "active"))
      (key-down! "a")
      (is (not (str/includes? (.-className (select "#component-snap-toggle"))
                              "active"))))))

(deftest control-line-render-test
  (testing "gates with controls render a control line"
    (drop-component! :h 100 100)
    (double-click! (select "[data-item-id=\"1\"]"))
    (set! (.-value (select "#gate-controls")) "1")
    (submit-gate-qubits! "1")
    (is (= 1 (count (select-all ".canvas-control-line"))))
    (is (= 1 (count (select-all ".item-port-control"))))))

(deftest export-include-ui-test
  (testing "the include grid option adds a grid pattern to the export"
    (let [calls (atom [])]
      (reset-app-with-api! (export-api calls))
      (drop-component! :h 100 100)
      (.click (select "#export-button"))
      (is (some? (select "#export-include-ui")))
      (is (not (.-checked (select "#export-include-ui"))))
      (set! (.-checked (select "#export-include-ui")) true)
      (change! (select "#export-include-ui"))
      (.click (select "#export-confirm"))
      (let [{:keys [content]} (first @calls)]
        (is (str/includes? content "qcad-grid"))
        (is (str/includes? content "export-grid"))))))

(deftest block-drag-test
  (testing "a template is dragged from the library as a labelled block"
    (drop-component! :h 100 100)
    (.click (select "#library-button"))
    (let [data-transfer (dragstart-block! "data-block-template" "bell")]
      (is (= "template:bell" (.getData data-transfer "application/x-qcad-block")))
      (drop-at! data-transfer 300 200))
    (is (nil? (select "#library-form")))
    (is (= 1 (count (select-all ".canvas-box"))))
    (let [box (select ".canvas-box")]
      (is (= "Bell state" (.-textContent box)))
      (is (= "300px" (.. box -style -left)))
      (is (= "200px" (.. box -style -top))))))

(deftest block-library-drag-test
  (testing "a saved circuit is dragged from the library as a block"
    (drop-component! :h 100 100)
    (.click (select "#library-button"))
    (input! (select "#library-name") "demo block")
    (.click (select "#library-save"))
    (let [data-transfer (dragstart-block! "data-block-library" "demo block")]
      (is (= "library:demo block"
             (.getData data-transfer "application/x-qcad-block")))
      (drop-at! data-transfer 200 200))
    (is (nil? (select "#library-form")))
    (let [box (select ".canvas-box")]
      (is (some? box))
      (is (= "demo block" (.-textContent box)))
      (is (= 3 (count (select-all ".canvas-item")))))))

(deftest components-panel-scroll-test
  (testing "moving the pointer over the palette keeps its scroll position"
    (let [panel (select "#components-panel")]
      (set! (.-scrollTop panel) 240)
      (mouse-move-on! (select "[data-component-id=\"h\"]")
                      {:clientX 20 :clientY 200})
      (is (= 240 (.-scrollTop (select "#components-panel"))))))

  (testing "re-rendering the app preserves the palette scroll position"
    (let [panel (select "#components-panel")]
      (set! (.-scrollTop panel) 180)
      (set! (.-value (select "#grid-size-select")) "40")
      (change! (select "#grid-size-select"))
      (is (= 180 (.-scrollTop (select "#components-panel")))))))

(deftest toolbar-select-interaction-test
  (testing "moving the pointer over the grid select does not rebuild it"
    (let [select-node (select "#grid-size-select")]
      (.focus select-node)
      (set! (.-value select-node) "20")
      (mouse-move-on! select-node {:clientX 100 :clientY 10})
      (is (identical? select-node (select "#grid-size-select")))
      (is (identical? select-node (.-activeElement (document))))
      (set! (.-value (select "#grid-size-select")) "40")
      (change! (select "#grid-size-select"))
      (is (= "40" (.-value (select "#grid-size-select"))))
      (set! (.-value (select "#grid-size-select")) "20")
      (change! (select "#grid-size-select"))
      (is (= "20" (.-value (select "#grid-size-select"))))
      (is (= "Grid 20" (status-text "#grid-size"))))))

(defn- define-property! [target name value]
  (js/Object.defineProperty target name
                            #js {:configurable true
                                 :writable true
                                 :value value}))

(defn- capture-anchor-clicks! []
  (let [clicks (atom [])
        prototype (.-prototype (.-HTMLAnchorElement js/window))
        original (.-click prototype)]
    (define-property! prototype "click"
                      (fn []
                        (this-as node
                          (swap! clicks conj [(.-href node)
                                              (.-download node)]))))
    {:clicks clicks
     :restore! (fn [] (define-property! prototype "click" original))}))

(defn- rejecting-thenable [error]
  #js {:then (fn [_] (rejecting-thenable error))
       :catch (fn [callback] (callback error) (rejecting-thenable error))})

(deftest browser-save-fallback-test
  (testing "SVG export downloads a data URL when Electron is unavailable"
    (let [{:keys [clicks restore!]} (capture-anchor-clicks!)]
      (try
        (drop-component! :h 100 100)
        (.click (select "#export-button"))
        (change! (select "#export-format"))
        (change! (select "#export-scale"))
        (.click (select "#export-confirm"))
        (let [[href name] (first @clicks)]
          (is (= "circuit.svg" name))
          (is (str/starts-with? href "data:text/plain")))
        (finally (restore!))))))

(deftest menu-save-browser-fallback-test
  (testing "the save menu publishes a JSON download without Electron saving"
    (let [callbacks (atom {})
          api #js {:onMenuSave (fn [callback]
                                (swap! callbacks assoc :save callback)
                                (fn [] nil))}
          {:keys [clicks restore!]} (capture-anchor-clicks!)]
      (try
        (reset-app-with-api! api)
        (drop-component! :h 100 100)
        ((:save @callbacks))
        (let [[href name] (first @clicks)]
          (is (= "circuit.json" name))
          (is (str/starts-with? href "data:text/plain")))
        (finally (restore!))))))

(defn- fake-image []
  (let [image #js {:naturalWidth 120 :naturalHeight 60}]
    (js/Object.defineProperty
     image "src"
     #js {:configurable true
          :set (fn [_]
                 (when-let [onload (.-onload image)]
                   (onload nil)))})
    image))

(deftest png-export-fallback-test
  (testing "PNG export draws on a canvas and downloads the encoded image"
    (let [{:keys [clicks restore!]} (capture-anchor-clicks!)
          original-image (.-Image js/globalThis)
          prototype (.-prototype (.-HTMLCanvasElement js/window))
          original-get-context (.-getContext prototype)
          original-to-data-url (.-toDataURL prototype)]
      (try
        (set! (.-Image js/globalThis) (fn [] (fake-image)))
        (define-property! prototype "getContext"
                          (fn [_ _] #js {:drawImage (fn [& _] nil)}))
        (define-property! prototype "toDataURL"
                          (fn [_ _] "data:image/png;base64,QUJD"))
        (drop-component! :h 100 100)
        (.click (select "#export-button"))
        (let [format (select "#export-format")]
          (set! (.-value format) "png")
          (change! format))
        (let [scale (select "#export-scale")]
          (set! (.-value scale) "2")
          (change! scale))
        (.click (select "#export-confirm"))
        (let [[href name] (first @clicks)]
          (is (= "circuit.png" name))
          (is (= "data:image/png;base64,QUJD" href)))
        (finally
          (set! (.-Image js/globalThis) original-image)
          (define-property! prototype "getContext" original-get-context)
          (define-property! prototype "toDataURL" original-to-data-url)
          (restore!))))))

(defn- fake-file-reader [content]
  (fn []
    (this-as this
      (set! (.-result this) nil)
      (set! (.-onload this) nil)
      (set! (.-readAsText this)
            (fn [_ _]
              (set! (.-result this) content)
              ((.-onload this) nil)))
      this)))

(deftest browser-open-fallback-test
  (testing "the open menu loads a file through the browser file input"
    (let [callbacks (atom {})
          api #js {:onMenuOpen (fn [callback]
                                (swap! callbacks assoc :open callback)
                                (fn [] nil))}
          selected (atom nil)
          prototype (.-prototype (.-HTMLInputElement js/window))
          original-click (.-click prototype)
          original-reader (.-FileReader js/globalThis)]
      (try
        (set! (.-FileReader js/globalThis)
              (fake-file-reader (str "OPENQASM 3.0;\n"
                                     "qubit[1] q;\n"
                                     "h q[0];\n")))
        (define-property! prototype "click"
                          (fn [] (this-as node (reset! selected node))))
        (reset-app-with-api! api)
        ((:open @callbacks))
        (let [input @selected]
          (is (some? input))
          (define-property! input "files" #js [#js {:name "demo.qasm"}])
          ((.-onchange input) nil))
        (is (= "q (1 qubit)" (.-textContent (select ".canvas-register"))))
        (is (= "H" (.-textContent (select ".canvas-gate"))))
        (finally
          (set! (.-FileReader js/globalThis) original-reader)
          (define-property! prototype "click" original-click))))))

(deftest open-error-test
  (testing "file open failures are logged without changing the circuit"
    (let [callbacks (atom {})
          errors (atom [])
          original-error (.-error js/console)
          api #js {:onMenuOpen (fn [callback]
                                (swap! callbacks assoc :open callback)
                                (fn [] nil))
                   :onMenuOpenRecent (fn [callback]
                                       (swap! callbacks assoc :recent callback)
                                       (fn [] nil))
                   :openFile (fn [_]
                               (rejecting-thenable (js/Error. "open failed")))
                   :openFileAt (fn [_]
                                 (rejecting-thenable
                                  (js/Error. "recent failed")))}]
      (try
        (set! (.-error js/console)
              (fn [message _] (swap! errors conj message)))
        (reset-app-with-api! api)
        ((:open @callbacks))
        (is (some #(str/includes? % "Could not open file.") @errors))
        ((:recent @callbacks) "missing.qasm")
        (is (some #(str/includes? % "Could not open recent file.") @errors))
        (is (= 0 (count (select-all ".canvas-register"))))
        (finally (set! (.-error js/console) original-error))))))

(deftest menu-save-variants-test
  (testing "both save menu events are subscribed and unsubscribed"
    (let [events (atom [])
          api #js {:onMenuSave (fn [_] (fn [] (swap! events conj :save)))
                   :onMenuSaveQasm (fn [_] (fn [] (swap! events conj :qasm)))}]
      (reset-app-with-api! api)
      (stop-app!)
      (is (= #{:save :qasm} (set @events)))))

  (testing "a save callback without its OpenQASM counterpart still works"
    (reset-app!)
    (let [events (atom [])
          api #js {:onMenuSave (fn [_] (fn [] (swap! events conj :save)))}]
      (reset-app-with-api! api)
      (stop-app!)
      (is (= [:save] @events))))

  (testing "an OpenQASM save callback alone still works"
    (reset-app!)
    (let [events (atom [])
          api #js {:onMenuSaveQasm (fn [_] (fn [] (swap! events conj :qasm)))}]
      (reset-app-with-api! api)
      (stop-app!)
      (is (= [:qasm] @events)))))

(deftest navigator-clipboard-test
  (testing "copy and paste use the asynchronous browser clipboard"
    (let [clipboard (atom "")
          navigator-object (.-navigator js/window)
          original-navigator (.-navigator js/globalThis)
          original (.-clipboard navigator-object)]
      (try
        (js/Object.defineProperty
         navigator-object "clipboard"
         #js {:configurable true
              :value #js {:writeText (fn [text]
                                      (reset! clipboard text)
                                      (thenable nil))
                          :readText (fn [] (thenable @clipboard))}})
        (define-property! js/globalThis "navigator" navigator-object)
        (reset-app!)
        (drop-component! :h 100 100)
        (mouse-down! (select "[data-item-id=\"1\"]")
                     {:clientX 100 :clientY 100})
        (mouse-up!)
        (key-down! "c" {:ctrlKey true})
        (is (str/includes? @clipboard "\"items\""))
        (key-down! "v" {:ctrlKey true})
        (is (= 2 (count (select-all ".canvas-gate"))))
        (finally
          (define-property! js/globalThis "navigator" original-navigator)
          (js/Object.defineProperty navigator-object "clipboard"
                                    #js {:configurable true
                                         :value original}))))))

(deftest fit-view-test
  (testing "fit view controls the canvas from the toolbar and keyboard"
    (drop-component! :h 100 100)
    (.click (select "#zoom-fit"))
    (is (some? (select ".canvas-gate")))
    (key-down! "f")
    (is (some? (select ".canvas-gate")))
    (key-down! "0")
    (is (some? (select ".canvas-gate")))))

(deftest toggle-control-context-test
  (testing "the context menu toggles a gate control line"
    (drop-component! :h 100 100)
    (right-click! (select ".canvas-gate") 100 100)
    (is (some? (select "#context-control")))
    (.click (select "#context-control"))
    (is (nil? (select "#context-menu")))
    (is (= 1 (count (select-all ".canvas-control-line"))))
    (right-click! (select ".canvas-gate") 100 100)
    (.click (select "#context-control"))
    (is (= 0 (count (select-all ".canvas-control-line"))))))

(deftest insert-user-circuit-test
  (testing "a saved circuit is inserted from the library list"
    (drop-component! :h 100 100)
    (.click (select "#library-button"))
    (input! (select "#library-name") "")
    (.click (select "#library-save"))
    (is (str/includes? (.-textContent (select "#library-form"))
                       "Enter a name for the circuit."))
    (input! (select "#library-name") "Saved circuit")
    (.click (select "#library-save"))
    (is (some? (select "[data-library-name=\"Saved circuit\"]")))
    (.click (select "[data-library-name=\"Saved circuit\"]"))
    (is (nil? (select "#library-form")))
    (is (= 2 (count (select-all ".canvas-gate"))))))

(deftest annotation-dialog-test
  (testing "annotations are added and edited from their dialog"
    (drop-component! :annotation 100 100)
    (is (some? (select "#annotation-form")))
    (is (= "Add annotation" (.-textContent (select ".dialog-title"))))
    (set! (.-value (select "#annotation-text")) "Note")
    (.click (select "#annotation-confirm"))
    (is (nil? (select "#annotation-form")))
    (is (= "Note" (.-textContent (select ".canvas-annotation"))))
    (double-click! (select ".canvas-annotation"))
    (is (some? (select "#annotation-form")))
    (is (= "Edit annotation" (.-textContent (select ".dialog-title"))))
    (set! (.-value (select "#annotation-text")) "   ")
    (.click (select "#annotation-confirm"))
    (is (some? (select "#annotation-form")))
    (is (str/includes? (.-textContent (select "#annotation-form"))
                       "Text is required."))
    (set! (.-value (select "#annotation-text")) "Edited")
    (.click (select "#annotation-confirm"))
    (is (= "Edited" (.-textContent (select ".canvas-annotation"))))))

(deftest dom-edge-render-test
  (testing "fallback labels and all item kinds render without a session"
    (let [state (assoc @(:state @session)
                       :items [{:id 1 :type :gate :component :mystery
                                :x 10 :y 20}
                               {:id 2 :type :gate :component nil
                                :x 100 :y 20}
                               {:id 3 :type :annotation :text "Note"
                                :x 200 :y 20}
                               {:id 4 :type :box :text "Box"
                                :x 300 :y 20}]
                       :wires [{:id 1 :from 1 :to 2}
                               {:id 2 :from 1 :to 3
                                :from-port {:kind :output :index 0}
                                :to-port {:kind :input :index 0}}]
                       :selected #{1}
                       :context-menu nil
                       :dialog nil)
          node (dom/render-app state)
          gates (vec (array-seq (.querySelectorAll node ".canvas-gate")))]
      (is (= ["mystery" "Gate"] (mapv #(.-textContent %) gates)))
      (is (= "Note" (.-textContent (.querySelector node ".canvas-annotation"))))
      (is (= "Box" (.-textContent (.querySelector node ".canvas-box"))))
      (is (= 2 (count (array-seq (.querySelectorAll node ".canvas-wire")))))
      (set! (.-innerHTML (.-body (document))) "<div id=\"host\"></div>")
      (.appendChild (select "#host") node)
      (doseq [item-node (array-seq (.querySelectorAll node ".canvas-item"))]
        (define-property! item-node "offsetWidth" 120)
        (define-property! item-node "offsetHeight" 40))
      (doseq [port-node (array-seq (.querySelectorAll node ".item-port"))]
        (define-property! port-node "offsetWidth" 8)
        (define-property! port-node "offsetHeight" 8))
      (dom/refresh-wires! node state)
      (is (= 2 (count (array-seq (.querySelectorAll node ".canvas-wire"))))))))
