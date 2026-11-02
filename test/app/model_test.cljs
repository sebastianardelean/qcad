(ns app.model-test
  (:require [cljs.test :refer [deftest is testing]]
            [app.model :as model]))

(deftest initial-state-test
  (is (= [] (:items model/initial-state)))
  (is (= 1 (:next-id model/initial-state)))
  (is (nil? (:dialog model/initial-state))))

(deftest parse-qubits-test
  (is (= 1 (model/parse-qubits "1")))
  (is (= 42 (model/parse-qubits " 42 ")))
  (is (nil? (model/parse-qubits "")))
  (is (nil? (model/parse-qubits "0")))
  (is (nil? (model/parse-qubits "-3")))
  (is (nil? (model/parse-qubits "1.5")))
  (is (nil? (model/parse-qubits "abc")))
  (is (nil? (model/parse-qubits nil))))

(deftest validate-register-test
  (is (nil? (model/validate-register "q1" "3")))
  (is (contains? (model/validate-register "" "3") :name))
  (is (contains? (model/validate-register "   " "3") :name))
  (is (contains? (model/validate-register "q1" "0") :qubits))
  (is (contains? (model/validate-register "q1" "nope") :qubits)))

(deftest next-register-name-test
  (testing "starts with register-1"
    (is (= "register-1" (model/next-register-name model/initial-state))))

  (testing "skips names already on the canvas"
    (is (= "register-2"
           (model/next-register-name
            (assoc model/initial-state
                   :items [{:type :register :name "register-1"}]))))
    (is (= "register-1"
           (model/next-register-name
            (assoc model/initial-state
                   :items [{:type :register :name "custom"} {:type :gate :component :h}]))))
    (is (= "register-3"
           (model/next-register-name
            (assoc model/initial-state
                   :items [{:type :register :name "register-1"}
                           {:type :register :name "register-2"}]))))))

(deftest add-gate-test
  (testing "adds a gate with its position and a fresh id"
    (let [state (model/handle-drop model/initial-state :h 10 20)]
      (is (= [{:id 1 :type :gate :component :h :x 20 :y 20}] (:items state)))
      (is (= 2 (:next-id state)))
      (is (nil? (:dialog state)))))

  (testing "consecutive gates get increasing ids"
    (let [state (-> model/initial-state
                    (model/handle-drop :x 1 2)
                    (model/handle-drop :y 3 4))]
      (is (= [1 2] (mapv :id (:items state))))
      (is (= [:x :y] (mapv :component (:items state)))))))

(deftest open-register-dialog-test
  (testing "dropping a register opens a dialog without adding an item"
    (let [state (model/handle-drop model/initial-state :quantum-register 30 40)]
      (is (= [] (:items state)))
      (is (= {:name "register-1"
              :qubits "2"
              :x 40
              :y 40
              :error nil}
             (:dialog state)))))

  (testing "the default name avoids existing register names"
    (let [state (-> model/initial-state
                    (model/handle-drop :quantum-register 0 0)
                    (model/confirm-register "register-1" "2")
                    (model/handle-drop :quantum-register 5 5))]
      (is (= "register-2" (get-in state [:dialog :name]))))))

(deftest confirm-register-test
  (testing "adds the configured register and closes the dialog"
    (let [state (-> model/initial-state
                    (model/handle-drop :quantum-register 30 40)
                    (model/confirm-register "  data  " " 3 "))]
      (is (= [{:id 1
               :type :register
               :component :quantum-register
               :name "data"
               :qubits 3
               :x 40
               :y 40}]
             (:items state)))
      (is (nil? (:dialog state)))
      (is (= 2 (:next-id state)))))

  (testing "invalid values keep the dialog open with an error"
    (let [state (-> model/initial-state
                    (model/handle-drop :quantum-register 30 40)
                    (model/confirm-register "  " "0"))]
      (is (= [] (:items state)))
      (is (= "  " (get-in state [:dialog :name])))
      (is (= "0" (get-in state [:dialog :qubits])))
      (is (map? (get-in state [:dialog :error]))))))

(deftest cancel-register-test
  (let [state (-> model/initial-state
                  (model/handle-drop :quantum-register 0 0)
                  model/cancel-register)]
    (is (nil? (:dialog state)))
    (is (= [] (:items state))))

  (testing "cancelling without a dialog does not change the state"
    (is (= model/initial-state (model/cancel-register model/initial-state)))))

(deftest unknown-drop-test
  (is (= model/initial-state (model/handle-drop model/initial-state :unknown 0 0))))

(deftest selection-test
  (testing "selecting an item replaces the current selection"
    (let [state (-> model/initial-state
                    (model/select-item 1)
                    (model/select-item 2))]
      (is (= #{2} (:selected state)))))

  (testing "shift selection toggles items"
    (let [state (model/toggle-selection model/initial-state 1)]
      (is (= #{1} (:selected state)))
      (is (= #{} (:selected (model/toggle-selection state 1))))))

  (testing "selection can be cleared"
    (is (= #{} (:selected (-> model/initial-state
                              (model/select-item 1)
                              model/clear-selection))))))

(deftest move-items-test
  (let [state (assoc model/initial-state
                     :items [{:id 1 :x 10 :y 20}
                             {:id 2 :x 100 :y 200}])
        moved (model/move-items state {1 [10 20]} 15 -5)]
    (is (= [20 20] ((juxt :x :y) (first (:items moved)))))
    (is (= [100 200] ((juxt :x :y) (second (:items moved)))))))

(deftest delete-item-test
  (let [state (-> model/initial-state
                  (model/handle-drop :h 0 0)
                  (model/select-item 1)
                  (model/open-context-menu 1 5 5))
        deleted (model/delete-item state 1)]
    (is (= [] (:items deleted)))
    (is (= #{} (:selected deleted)))
    (is (nil? (:context-menu deleted)))))

(deftest rotate-item-test
  (testing "rotation moves clockwise in steps of pi/2"
    (is (= 90 model/rotation-step-degrees))
    (let [state (-> model/initial-state
                    (model/handle-drop :h 10 20)
                    (model/rotate-item 1))]
      (is (= 90 (-> state :items first :rotation)))

      (testing "rotation wraps at a full turn"
        (let [state (reduce (fn [state _] (model/rotate-item state 1))
                            state
                            (range 3))]
          (is (= 0 (-> state :items first :rotation)))))))

  (testing "unknown items are unchanged"
    (let [state (-> model/initial-state
                    (model/handle-drop :h 10 20)
                    (model/rotate-item 999))]
      (is (= 0 (or (-> state :items first :rotation) 0))))))

(deftest rotate-item-by-test
  (testing "rotating by a quarter turn"
    (let [state (-> model/initial-state
                    (model/handle-drop :h 10 20)
                    (model/rotate-item-by 1 90))]
      (is (= 90 (-> state :items first :rotation)))))

  (testing "rotation wraps at a full turn"
    (let [state (-> model/initial-state
                    (model/handle-drop :h 10 20)
                    (model/rotate-item-by 1 270)
                    (model/rotate-item-by 1 180))]
      (is (= 90 (-> state :items first :rotation))))))

(deftest rotate-item-arities-test
  (let [state (-> model/initial-state
                  (model/handle-drop :h 0 0))]
    (testing "the default rotation is pi/2"
      (is (= 90 (-> (model/rotate-item state 1) :items first :rotation))))
    (testing "an explicit rotation is honoured"
      (is (= 90 (-> (model/rotate-item state 1 90) :items first :rotation))))))

(deftest rotate-items-test
  (testing "rotates every targeted item and leaves the others alone"
    (let [state (-> model/initial-state
                    (model/handle-drop :h 0 0)
                    (model/handle-drop :x 10 10)
                    (model/handle-drop :y 20 20)
                    (model/rotate-items #{1 3} 90))
          by-id (into {} (map (juxt :id identity) (:items state)))]
      (is (= 90 (get-in by-id [1 :rotation])))
      (is (nil? (get-in by-id [2 :rotation])))
      (is (= 90 (get-in by-id [3 :rotation]))))))

(deftest delete-items-test
  (testing "deletes every targeted item and updates the selection"
    (let [state (-> model/initial-state
                    (model/handle-drop :h 0 0)
                    (model/handle-drop :x 10 10)
                    (model/handle-drop :y 20 20)
                    (model/select-item 1)
                    (model/toggle-selection 2)
                    (model/open-context-menu 2 0 0)
                    (model/delete-items #{1 2}))]
      (is (= [3] (mapv :id (:items state))))
      (is (= #{} (:selected state)))
      (is (nil? (:context-menu state))))))

(deftest context-menu-test
  (testing "opening the menu targets one item and selects it"
    (let [state (-> model/initial-state
                    (model/handle-drop :h 0 0)
                    (model/handle-drop :x 10 10)
                    (model/select-item 1)
                    (model/open-context-menu 2 55 66))]
      (is (= {:item-id 2 :x 55 :y 66} (:context-menu state)))
      (is (= #{2} (:selected state)))))

  (testing "closing the menu keeps the selection"
    (let [state (-> model/initial-state
                    (model/select-item 1)
                    (model/open-context-menu 1 0 0)
                    model/close-context-menu)]
      (is (nil? (:context-menu state)))
      (is (= #{1} (:selected state)))))

  (testing "opening the register dialog closes the context menu"
    (let [state (-> model/initial-state
                    (model/open-context-menu 1 0 0)
                    (model/open-register-dialog 10 20))]
      (is (nil? (:context-menu state)))
      (is (some? (:dialog state))))))

(defn- with-two-gates []
  (-> model/initial-state
      (model/handle-drop :h 10 20)
      (model/handle-drop :x 100 200)))

(deftest wire-tool-test
  (testing "the select tool is active by default"
    (is (= :select (:tool model/initial-state)))
    (is (empty? (:wires model/initial-state))))

  (testing "the wire tool toggles and clears any draft"
    (is (= :wire (:tool (model/toggle-wire-tool model/initial-state))))
    (let [state (-> model/initial-state
                    model/toggle-wire-tool
                    (model/start-wire-draft 1 10 10)
                    model/toggle-wire-tool)]
      (is (= :select (:tool state)))
      (is (nil? (:wire-draft state))))))

(deftest wire-draft-test
  (testing "a draft records its source and pointer position"
    (let [state (-> model/initial-state
                    (model/start-wire-draft 1 10 20))]
      (is (= {:from 1 :x 10 :y 20 :points []} (:wire-draft state)))
      (is (= {:from 1 :x 30 :y 40 :points []}
             (:wire-draft (model/update-wire-draft state 30 40))))
      (is (nil? (:wire-draft (model/cancel-wire-draft state))))))

  (testing "updating or cancelling without a draft is a no-op"
    (is (= model/initial-state (model/update-wire-draft model/initial-state 1 2)))
    (is (= model/initial-state (model/cancel-wire-draft model/initial-state)))))

(deftest add-wire-point-test
  (testing "clicking the canvas records a corner for the draft"
    (let [state (-> model/initial-state
                    (model/start-wire-draft 1 10 20)
                    (model/add-wire-point 30 40)
                    (model/add-wire-point 50 60))]
      (is (= [[30 40] [50 60]] (get-in state [:wire-draft :points])))
      (is (= [50 60] ((juxt :x :y) (:wire-draft state))))))

  (testing "the same corner is not recorded twice"
    (let [state (-> model/initial-state
                    (model/start-wire-draft 1 10 20)
                    (model/add-wire-point 30 40)
                    (model/add-wire-point 30 40))]
      (is (= [[30 40]] (get-in state [:wire-draft :points])))))

  (testing "adding a corner without a draft is a no-op"
    (is (= model/initial-state (model/add-wire-point model/initial-state 1 2)))))

(deftest add-wire-test
  (testing "a wire between two existing items is stored"
    (let [state (model/add-wire (with-two-gates) 1 2)]
      (is (= [{:id 1 :from 1 :to 2}] (:wires state)))
      (is (= 2 (:next-wire-id state)))))

  (testing "self wires are rejected"
    (let [state (with-two-gates)]
      (is (= state (model/add-wire state 1 1)))))

  (testing "wires to unknown items are rejected"
    (let [state (with-two-gates)]
      (is (= state (model/add-wire state 1 99)))
      (is (= state (model/add-wire state 99 1)))))

  (testing "duplicate wires are rejected in either direction"
    (let [state (model/add-wire (with-two-gates) 1 2)]
      (is (= state (model/add-wire state 1 2)))
      (is (= state (model/add-wire state 2 1)))))

  (testing "wires between two registers are rejected"
    (let [state (-> model/initial-state
                    (model/handle-drop :quantum-register 0 0)
                    (model/confirm-register "a" "2")
                    (model/handle-drop :quantum-register 10 10)
                    (model/confirm-register "b" "2"))]
      (is (= state (model/add-wire state 1 2)))
      (is (empty? (:wires state)))))

  (testing "wires between a register and a gate are allowed"
    (let [state (-> model/initial-state
                    (model/handle-drop :quantum-register 0 0)
                    (model/confirm-register "a" "2")
                    (model/handle-drop :h 10 10))
          wired (model/add-wire state 1 2)]
      (is (= [{:id 1 :from 1 :to 2}] (:wires wired)))))

  (testing "a wire stores the corners selected by the user"
    (let [state (model/add-wire (with-two-gates) 1 2 [[50 60] [70 80]])]
      (is (= [{:id 1 :from 1 :to 2 :points [[50 60] [70 80]]}]
             (:wires state))))))

(deftest finish-wire-test
  (testing "finishing a draft connects to the target and clears the draft"
    (let [state (-> (with-two-gates)
                    (model/start-wire-draft 1 0 0)
                    (model/finish-wire 2))]
      (is (= [{:id 1 :from 1 :to 2}] (:wires state)))
      (is (nil? (:wire-draft state)))))

  (testing "finishing a draft keeps its selected corners"
    (let [state (-> (with-two-gates)
                    (model/start-wire-draft 1 0 0)
                    (model/add-wire-point 50 60)
                    (model/finish-wire 2))]
      (is (= [{:id 1 :from 1 :to 2 :points [[50 60]]}] (:wires state)))
      (is (nil? (:wire-draft state)))))

  (testing "finishing on the source item does not create a wire"
    (let [state (-> (with-two-gates)
                    (model/start-wire-draft 1 0 0)
                    (model/finish-wire 1))]
      (is (empty? (:wires state)))
      (is (nil? (:wire-draft state)))))

  (testing "finishing without a draft is a no-op"
    (let [state (with-two-gates)]
      (is (= state (model/finish-wire state 2))))))

(deftest orthogonal-route-test
  (testing "horizontal and vertical routes need no bends"
    (is (= [[0 0] [10 0]] (model/orthogonal-route [0 0] [10 0])))
    (is (= [[0 0] [0 10]] (model/orthogonal-route [0 0] [0 10]))))

  (testing "diagonal routes use right-angled bends"
    (is (= [[0 0] [50 0] [50 40] [100 40]]
           (model/orthogonal-route [0 0] [100 40]))))

  (testing "routes start and end at the endpoints with axis-aligned segments"
    (doseq [[from to] [[[0 0] [100 40]]
                       [[0 0] [1 40]]
                       [[5 5] [-10 -20]]]]
      (let [points (model/orthogonal-route from to)]
        (is (= from (first points)))
        (is (= to (last points)))
        (is (every? (fn [[[x1 y1] [x2 y2]]]
                      (or (= x1 x2) (= y1 y2)))
                    (partition 2 1 points)))))))

(deftest orthogonal-path-test
  (testing "a path with no corners matches a single route"
    (is (= [[0 0] [50 0] [50 40] [100 40]]
           (model/orthogonal-path [[0 0] [100 40]]))))

  (testing "a path visits corners with right-angled bends"
    (let [points (model/orthogonal-path [[0 0] [50 60] [100 40]])]
      (is (= [[0 0] [25 0] [25 60] [50 60] [75 60] [75 40] [100 40]]
             points))
      (is (every? (fn [[[x1 y1] [x2 y2]]]
                    (or (= x1 x2) (= y1 y2)))
                  (partition 2 1 points))))))

(deftest delete-items-removes-wires-test
  (testing "wires attached to a deleted source are removed"
    (let [state (-> (with-two-gates)
                    (model/add-wire 1 2)
                    (model/delete-item 1))]
      (is (empty? (:wires state)))
      (is (= 2 (:next-wire-id state)))))

  (testing "wires attached to a deleted target are removed"
    (let [state (-> (with-two-gates)
                    (model/add-wire 1 2)
                    (model/delete-item 2))]
      (is (empty? (:wires state)))))

  (testing "the wire draft is cancelled when its source is deleted"
    (let [state (-> (with-two-gates)
                    (model/start-wire-draft 1 0 0)
                    (model/delete-item 1))]
      (is (nil? (:wire-draft state))))))

(deftest snap-test
  (testing "values snap to the nearest multiple of the grid size"
    (is (= 0 (model/snap-value 2 5)))
    (is (= 5 (model/snap-value 3 5)))
    (is (= 10 (model/snap-value 8 5)))
    (is (= -10 (model/snap-value -8 5)))
    (is (= 0 (model/snap-value -2 5))))

  (testing "snapping is on by default"
    (is (true? (:snap model/initial-state)))
    (is (= 20 (:snap-size model/initial-state))))

  (testing "snap-point rounds both coordinates while snapping is enabled"
    (is (= [20 40] (model/snap-point model/initial-state [27 38])))
    (is (= [-20 -40] (model/snap-point model/initial-state [-27 -38]))))

  (testing "snap-point is a no-op while snapping is disabled"
    (let [state (model/toggle-snap model/initial-state)]
      (is (false? (:snap state)))
      (is (= [7 8] (model/snap-point state [7 8])))))

  (testing "snapping can be toggled back on"
    (is (true? (:snap (-> model/initial-state
                         model/toggle-snap
                         model/toggle-snap))))))

(deftest snapping-placement-test
  (testing "dropped gates are placed on the grid"
    (let [state (model/handle-drop model/initial-state :h 7 8)]
      (is (= [0 0] ((juxt :x :y) (first (:items state)))))))

  (testing "register dialogs remember the snapped drop position"
    (let [state (model/handle-drop model/initial-state :quantum-register 13 27)]
      (is (= [20 20] ((juxt :x :y) (:dialog state))))))

  (testing "moving an item snaps its new position"
    (let [state (assoc model/initial-state
                       :items [{:id 1 :x 10 :y 20}])
          moved (model/move-items state {1 [10 20]} 12 13)]
      (is (= [20 40] ((juxt :x :y) (first (:items moved)))))))

  (testing "disabling snap keeps free-form positions"
    (let [state (model/toggle-snap model/initial-state)
          dropped (model/handle-drop state :h 7 8)]
      (is (= [7 8] ((juxt :x :y) (first (:items dropped))))))))

(deftest view-transform-test
  (testing "the default view is an identity transform"
    (is (= {:pan-x 0 :pan-y 0 :zoom 1} (:view model/initial-state)))
    (is (= [50 60] (model/screen->world (:view model/initial-state) [50 60])))
    (is (= [50 60] (model/world->screen (:view model/initial-state) [50 60]))))

  (testing "screen and world coordinates round-trip through pan and zoom"
    (let [view {:pan-x 10 :pan-y 20 :zoom 2}]
      (is (= [50 50] (model/screen->world view [110 120])))
      (is (= [110 120] (model/world->screen view [50 50])))))

  (testing "panning offsets the view"
    (let [state (model/pan-by model/initial-state 30 -15)]
      (is (= {:pan-x 30 :pan-y -15 :zoom 1} (:view state)))))

  (testing "resetting the view restores the identity transform"
    (let [state (-> model/initial-state
                    (model/pan-by 40 40)
                    model/reset-view)]
      (is (= {:pan-x 0 :pan-y 0 :zoom 1} (:view state))))))

(deftest zoom-at-test
  (testing "zooming keeps the pointer anchored to the same world point"
    (let [state (model/zoom-at model/initial-state 2 [100 100])]
      (is (= 2 (get-in state [:view :zoom])))
      (is (= [100 100] (model/screen->world (:view state) [100 100])))))

  (testing "zoom is clamped to the supported range"
    (is (= model/max-zoom
           (get-in (model/zoom-at model/initial-state 100 [0 0]) [:view :zoom])))
    (is (= model/min-zoom
           (get-in (model/zoom-at model/initial-state 0.001 [0 0]) [:view :zoom])))))

(deftest cursor-test
  (testing "the cursor position is tracked in world coordinates"
    (is (nil? (:cursor model/initial-state)))
    (is (= [12 34] (:cursor (model/set-cursor model/initial-state [12 34]))))))

(defn- with-three-gates []
  (-> model/initial-state
      (model/handle-drop :h 0 0)
      (model/handle-drop :x 100 100)
      (model/handle-drop :y 300 300)))

(deftest marquee-test
  (testing "a marquee records its start and current point"
    (let [state (-> model/initial-state
                    (model/begin-marquee [10 20])
                    (model/update-marquee [30 40]))]
      (is (= {:start [10 20] :current [30 40]} (:marquee state)))))

  (testing "updating or cancelling without a marquee is a no-op"
    (is (= model/initial-state (model/update-marquee model/initial-state [1 2])))
    (is (= model/initial-state (model/cancel-marquee model/initial-state))))

  (testing "a marquee rectangle is normalized to its bounds"
    (is (= [10 20 30 40]
           (model/marquee-rect {:start [30 40] :current [10 20]}))))

  (testing "finishing and cancelling clear the marquee"
    (let [state (model/begin-marquee model/initial-state [0 0])]
      (is (nil? (:marquee (model/cancel-marquee state))))
      (is (nil? (:marquee (model/finish-marquee state)))))))

(deftest marquee-selection-test
  (testing "item bounds default to their rendered size"
    (is (= [10 20 54 56] (model/item-bounds {:x 10 :y 20})))
    (is (= [10 20 170 56] (model/item-bounds {:x 10 :y 20 :type :register})))
    (is (= [10 20 30 40]
           (model/item-bounds {:x 10 :y 20 :width 20 :height 20}))))

  (testing "rectangles intersect when their areas overlap"
    (is (model/rects-intersect? [0 0 10 10] [5 5 15 15]))
    (is (not (model/rects-intersect? [0 0 10 10] [10 0 20 10])))
    (is (not (model/rects-intersect? [0 0 10 10] [20 20 30 30]))))

  (testing "a marquee selects every overlapping item"
    (let [state (-> (with-three-gates)
                    (model/begin-marquee [-5 -5])
                    (model/update-marquee [110 110])
                    (model/update-marquee-selection (:items (with-three-gates))))]
      (is (= #{1 2} (:selected state)))))

  (testing "a larger marquee selects all items"
    (let [state (with-three-gates)
          items (:items state)]
      (is (= #{1 2 3}
             (:selected (-> state
                            (model/begin-marquee [-10 -10])
                            (model/update-marquee [400 400])
                            (model/update-marquee-selection items)
                            model/finish-marquee))))))

  (testing "selection is untouched without a marquee"
    (let [state (model/select-item model/initial-state 1)]
      (is (= state (model/update-marquee-selection state []))))))
