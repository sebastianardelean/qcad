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

(deftest validate-custom-gate-test
  (is (nil? (model/validate-custom-gate "2" "gate pair a, b { h a; }")))
  (is (contains? (model/validate-custom-gate "0" "gate pair a, b { h a; }")
                 :qubits))
  (is (contains? (model/validate-custom-gate "2" "   ") :qasm))
  (is (contains? (model/validate-custom-gate "2" nil) :qasm)))

(deftest open-custom-dialog-test
  (testing "dropping the custom component opens its dialog without adding an item"
    (let [state (model/handle-drop model/initial-state :custom 13 27)]
      (is (= [] (:items state)))
      (is (= {:type :custom-gate
              :x 20
              :y 20
              :qubits "1"
              :qasm ""
              :error nil}
             (:dialog state))))))

(deftest confirm-custom-gate-test
  (testing "a valid definition adds a gate-like item with its QASM"
    (let [state (-> model/initial-state
                    (model/handle-drop :custom 30 40)
                    (model/confirm-custom-gate " 2 " "  gate pair a, b { h a; }  "))]
      (is (= [{:id 1
               :type :gate
               :component :custom
               :qubits 2
               :qasm "gate pair a, b { h a; }"
               :x 40
               :y 40}]
             (:items state)))
      (is (nil? (:dialog state)))
      (is (= 2 (:next-id state)))))

  (testing "invalid values keep the dialog open with errors"
    (let [state (-> model/initial-state
                    (model/handle-drop :custom 0 0)
                    (model/confirm-custom-gate "0" ""))]
      (is (= [] (:items state)))
      (is (= "0" (get-in state [:dialog :qubits])))
      (is (= "" (get-in state [:dialog :qasm])))
      (is (map? (get-in state [:dialog :error]))))))

(deftest edit-custom-gate-test
  (testing "opening the editor seeds it from the custom gate"
    (let [state (-> model/initial-state
                    (model/handle-drop :custom 0 0)
                    (model/confirm-custom-gate "2" "gate pair a, b { h a; }")
                    (model/open-custom-editor 1))]
      (is (= {:type :custom-gate
              :item-id 1
              :x 0
              :y 0
              :qubits "2"
              :qasm "gate pair a, b { h a; }"
              :error nil}
             (:dialog state)))))

  (testing "confirming an edit updates the existing item"
    (let [state (-> model/initial-state
                    (model/handle-drop :custom 0 0)
                    (model/confirm-custom-gate "2" "gate pair a, b { h a; }")
                    (model/open-custom-editor 1)
                    (model/confirm-custom-gate "3" "gate triple a, b, c { h a; }"))]
      (is (= 1 (count (:items state))))
      (is (= 3 (get-in state [:items 0 :qubits])))
      (is (= "gate triple a, b, c { h a; }" (get-in state [:items 0 :qasm])))))

  (testing "only custom gates can be edited"
    (let [state {:items [{:id 1 :type :register :name "a" :qubits 2}]
                 :wires []}]
      (is (= state (model/open-custom-editor state 1)))
      (is (= state (model/open-custom-editor state 99))))))

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

(defn- with-register-and-gate []
  (-> model/initial-state
      (model/handle-drop :quantum-register 0 0)
      (model/confirm-register "data" "3")
      (model/handle-drop :h 100 0)
      (update :items
              (fn [items]
                (mapv #(if (= :gate (:type %))
                         (assoc % :qubits 2)
                         %)
                      items)))))


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

  (testing "a draft records the source port when one is supplied"
    (let [port {:kind :output :index 2}
          state (model/start-wire-draft model/initial-state 1 10 20 port)]
      (is (= {:from 1
              :from-port port
              :x 10
              :y 20
              :points []}
             (:wire-draft state)))))

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

(deftest port-wire-test
  (testing "a port wire stores both endpoints"
    (let [from-port {:kind :output :index 1}
          to-port {:kind :input :index 1}
          state (-> (with-register-and-gate)
                    (model/add-wire 1 2 nil from-port to-port))]
      (is (= [{:id 1
               :from 1
               :to 2
               :from-port from-port
               :to-port to-port}]
             (:wires state)))))

  (testing "different ports allow multiple wires between the same items"
    (let [state (-> (with-register-and-gate)
                    (model/add-wire 1 2 nil
                                    {:kind :output :index 0}
                                    {:kind :input :index 0})
                    (model/add-wire 1 2 nil
                                    {:kind :output :index 1}
                                    {:kind :input :index 1}))]
      (is (= [[0 0] [1 1]]
             (mapv (juxt (comp :index :from-port) (comp :index :to-port))
                   (:wires state))))))

  (testing "a port can only be connected once"
    (let [state (-> (with-two-gates)
                    (model/add-wire 1 2 nil
                                    {:kind :output :index 0}
                                    {:kind :input :index 0}))]
      (is (= state (model/add-wire state 1 2 nil
                                   {:kind :output :index 0}
                                   {:kind :input :index 0})))))

  (testing "invalid port kinds and indices are rejected"
    (let [state (with-register-and-gate)]
      (is (= state (model/add-wire state 1 2 nil
                                   {:kind :input :index 0}
                                   {:kind :input :index 0})))
      (is (= state (model/add-wire state 1 2 nil
                                   {:kind :output :index 0}
                                   {:kind :output :index 0})))
      (is (= state (model/add-wire state 1 2 nil
                                   {:kind :output :index 9}
                                   {:kind :input :index 0})))
      (is (= state (model/add-wire state 1 2 nil
                                   {:kind :output :index 0}
                                   {:kind :input :index 9})))))

  (testing "wires cannot mix item-level and port-level endpoints"
    (let [state (with-two-gates)]
      (is (= state (model/add-wire state 1 2 nil
                                   {:kind :output :index 0}
                                   nil))))))

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

  (testing "finishing a port draft keeps both endpoints"
    (let [from-port {:kind :output :index 1}
          to-port {:kind :input :index 0}
          state (-> (with-register-and-gate)
                    (model/start-wire-draft 1 0 0 from-port)
                    (model/finish-wire 2 to-port))]
      (is (= [{:id 1
               :from 1
               :to 2
               :from-port from-port
               :to-port to-port}]
             (:wires state)))
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

(defn- target-circuit []
  {:items [{:id 1 :type :register :name "a" :qubits 2 :x 0 :y 0}
           {:id 2 :type :register :name "b" :qubits 3 :x 0 :y 100}
           {:id 3 :type :gate :component :h :x 100 :y 0}
           {:id 4 :type :gate :component :x :x 200 :y 0}]
   :wires [{:id 1 :from 1 :to 3}
           {:id 2 :from 2 :to 3}
           {:id 3 :from 3 :to 4}]})

(deftest connected-registers-test
  (testing "registers connected through gates and wires are found"
    (is (= [1 2] (mapv :id (model/connected-registers (target-circuit) 4))))
    (is (= [1 2] (mapv :id (model/connected-registers (target-circuit) 3)))))

  (testing "unconnected gates have no registers"
    (is (= [] (model/connected-registers (target-circuit) 99)))))

(defn- port-target-circuit []
  {:items [{:id 1 :type :register :name "a" :qubits 2 :x 0 :y 0}
           {:id 2 :type :register :name "b" :qubits 3 :x 0 :y 100}
           {:id 3 :type :gate :component :h :qubits 2 :x 100 :y 0}
           {:id 4 :type :gate :component :x :qubits 2 :x 200 :y 0}]
   :wires [{:id 1
            :from 1
            :to 3
            :from-port {:kind :output :index 1}
            :to-port {:kind :input :index 0}}
           {:id 2
            :from 2
            :to 3
            :from-port {:kind :output :index 0}
            :to-port {:kind :input :index 1}}
           {:id 3
            :from 3
            :to 4
            :from-port {:kind :output :index 0}
            :to-port {:kind :input :index 0}}
           {:id 4
            :from 3
            :to 4
            :from-port {:kind :output :index 1}
            :to-port {:kind :input :index 1}}]})

(deftest gate-targets-test
  (testing "targets are traced from register output ports"
    (is (= [[1 1] [2 0]] (model/gate-targets (port-target-circuit) 3))))

  (testing "targets propagate through gate input and output port indices"
    (is (= [[1 1] [2 0]] (model/gate-targets (port-target-circuit) 4))))

  (testing "the same qubit is not reported twice"
    (let [state (-> (port-target-circuit)
                    (assoc-in [:wires 1 :from] 1)
                    (assoc-in [:wires 1 :from-port]
                              {:kind :output :index 1}))]
      (is (= [[1 1]] (model/gate-targets state 3)))))

  (testing "cycles and unconnected gates produce no targets"
    (let [cycle {:items [{:id 2 :type :gate :component :h}
                         {:id 3 :type :gate :component :x}]
                 :wires [{:id 1
                          :from 3
                          :to 2
                          :from-port {:kind :output :index 0}
                          :to-port {:kind :input :index 0}}
                         {:id 2
                          :from 2
                          :to 3
                          :from-port {:kind :output :index 0}
                          :to-port {:kind :input :index 0}}]}]
      (is (= [] (model/gate-targets cycle 2)))))
  (testing "invalid source ports are ignored"
    (let [base (port-target-circuit)]
      (is (= [[2 0]] (model/gate-targets (assoc-in base [:wires 0 :from] 99) 3)))
      (is (= [[2 0]]
             (model/gate-targets
              (assoc-in base [:wires 0 :from-port] {:kind :input :index 0})
              3)))
      (is (= [[2 0]]
             (model/gate-targets
              (assoc-in base [:wires 0 :from-port] {:kind :output :index 9})
              3)))))
  (is (= [] (model/gate-targets (port-target-circuit) 1)))
  (is (= [] (model/gate-targets (port-target-circuit) 99)))
  (is (true? (model/gate-port-wired? (port-target-circuit) 3)))
  (is (true? (model/gate-port-wired? (port-target-circuit) 4)))
  (is (false? (model/gate-port-wired? (port-target-circuit) 99)))
  (is (false? (model/gate-port-wired?
               (assoc (port-target-circuit)
                      :wires [{:id 1 :from 3 :to 4
                               :from-port {:kind :output :index 0}}])
               3))))

(deftest open-gate-qubits-dialog-test
  (testing "the dialog starts from the gate's current port count"
    (let [state (model/open-gate-qubits-dialog (target-circuit) 3)]
      (is (= {:type :gate-qubits
              :gate-id 3
              :qubits "1"
              :error nil}
             (:dialog state)))
      (is (nil? (:context-menu state)))))

  (testing "an existing qubit count seeds the dialog"
    (let [state (-> (target-circuit)
                    (assoc-in [:items 2 :qubits] 3)
                    (model/open-gate-qubits-dialog 3))]
      (is (= "3" (get-in state [:dialog :qubits])))))

  (testing "only gates open the dialog"
    (is (= (target-circuit) (model/open-gate-qubits-dialog (target-circuit) 1)))
    (is (= (target-circuit) (model/open-gate-qubits-dialog (target-circuit) 99)))))

(deftest confirm-gate-qubits-test
  (testing "the qubit count is stored and old target selections are cleared"
    (let [state (-> (target-circuit)
                    (assoc-in [:items 2 :targets] {1 #{0}})
                    (model/open-gate-qubits-dialog 3)
                    (model/confirm-gate-qubits "2"))]
      (is (nil? (:dialog state)))
      (is (= 2 (get-in state [:items 2 :qubits])))
      (is (nil? (get-in state [:items 2 :targets])))))

  (testing "invalid counts keep the dialog open with an error"
    (let [state (-> (target-circuit)
                    (model/open-gate-qubits-dialog 3)
                    (model/confirm-gate-qubits "0"))]
      (is (= "Number of qubits must be a positive whole number."
             (get-in state [:dialog :error])))
      (is (nil? (get-in state [:items 2 :qubits])))))

  (testing "reducing the count removes wires on removed ports and stale drafts"
    (let [state (-> (port-target-circuit)
                    (model/open-gate-qubits-dialog 3)
                    (model/start-wire-draft 3 0 0 {:kind :output :index 1})
                    (model/confirm-gate-qubits "1"))]
      (is (= [1 3] (mapv :id (:wires state))))
      (is (nil? (:wire-draft state)))
      (is (= 1 (get-in state [:items 2 :qubits])))))

  (testing "confirming without a gate dialog is a no-op"
    (is (= model/initial-state
           (model/confirm-gate-qubits model/initial-state "2")))))

(deftest cancel-dialog-test
  (testing "cancel clears register and gate qubit dialogs"
    (let [register-state (assoc model/initial-state :dialog {:name "q"})
          gate-state (model/open-gate-qubits-dialog (target-circuit) 3)]
      (is (nil? (:dialog (model/cancel-dialog register-state))))
      (is (nil? (:dialog (model/cancel-dialog gate-state))))))
  (testing "cancelling without a dialog is a no-op"
    (is (= model/initial-state (model/cancel-dialog model/initial-state)))))

(deftest gate-port-count-test
  (testing "gates default to one input and output port"
    (is (= 1 (model/gate-port-count {:type :gate})))
    (is (= 1 (model/gate-port-count {:type :gate :qubits 0}))))

  (testing "port count follows the configured qubit count"
    (is (= 2 (model/gate-port-count {:type :gate :qubits 2})))
    (is (= 3 (model/gate-port-count {:type :gate :qubits 3}))))

  (testing "legacy target selections no longer control port count"
    (is (= 1 (model/gate-port-count {:type :gate :targets {1 #{0 2}}}))))

  (testing "only gates expose input and output ports"
    (is (= 2 (model/port-count {:type :register :qubits 2} :output)))
    (is (= 0 (model/port-count {:type :register :qubits 2} :input)))
    (is (= 1 (model/port-count {:type :gate} :input)))
    (is (= 1 (model/port-count {:type :gate} :output)))
    (is (= 0 (model/port-count {:type :gate} :other)))
    (is (= 0 (model/port-count {:type :other} :output)))))

(deftest free-port-index-test
  (testing "the first unused port is returned"
    (let [state (-> (with-register-and-gate)
                    (model/add-wire 1 2 nil
                                    {:kind :output :index 0}
                                    {:kind :input :index 0}))]
      (is (= 1 (model/free-port-index state 1 :output)))
      (is (= 1 (model/free-port-index state 2 :input)))
      (is (= 0 (model/free-port-index state 2 :output)))))

  (testing "an item with every port used has no free port"
    (let [state (-> (with-register-and-gate)
                    (model/add-wire 1 2 nil
                                    {:kind :output :index 0}
                                    {:kind :input :index 0})
                    (model/add-wire 1 2 nil
                                    {:kind :output :index 1}
                                    {:kind :input :index 1}))]
      (is (nil? (model/free-port-index state 2 :input)))
      (is (= 2 (model/free-port-index state 1 :output)))))

  (testing "an unknown item has no free ports"
    (is (nil? (model/free-port-index model/initial-state 99 :output)))))


(deftest multi-qubit-defaults-test
  (testing "multi-qubit gates are inserted with their default arity"
    (is (= 2 (get-in (model/handle-drop model/initial-state :cx 0 0)
                     [:items 0 :qubits])))
    (is (= 2 (get-in (model/handle-drop model/initial-state :cz 0 0)
                     [:items 0 :qubits])))
    (is (= 2 (get-in (model/handle-drop model/initial-state :swap 0 0)
                     [:items 0 :qubits])))
    (is (= 3 (get-in (model/handle-drop model/initial-state :ccx 0 0)
                     [:items 0 :qubits])))))

(deftest classical-register-test
  (testing "classical registers are opened from the classical component"
    (let [state (model/handle-drop model/initial-state :classical-register 0 0)]
      (is (true? (get-in state [:dialog :classical])))
      (is (= "register-1" (get-in state [:dialog :name])))))

  (testing "confirming a classical register stores its kind and ports"
    (let [state (-> model/initial-state
                    (model/handle-drop :classical-register 0 0)
                    (model/confirm-register "c" "2"))
          register (first (:items state))]
      (is (= :classical-register (:component register)))
      (is (true? (:classical register)))
      (is (= 2 (model/port-count register :input)))
      (is (= 0 (model/port-count register :output)))
      (is (model/classical-register? register)))))

(deftest gate-controls-test
  (testing "controls extend the gate port count"
    (let [state (model/handle-drop model/initial-state :h 0 0)
          state (model/set-gate-controls state 1 1)
          gate (first (:items state))]
      (is (= 1 (model/gate-control-count gate)))
      (is (= 2 (model/gate-port-count gate)))
      (is (= 2 (model/port-count gate :input)))
      (is (= 2 (model/port-count gate :output)))))

  (testing "controls can be toggled and cleared"
    (let [state (-> model/initial-state
                    (model/handle-drop :h 0 0)
                    (model/toggle-gate-control 1))]
      (is (= 1 (model/gate-control-count (first (:items state)))))
      (let [state (model/toggle-gate-control state 1)]
        (is (= 0 (model/gate-control-count (first (:items state))))))
      (let [state (model/set-gate-controls state 1 -3)]
        (is (= 0 (model/gate-control-count (first (:items state)))))))))

(deftest gate-dialog-shape-test
  (testing "qubits, angle and controls are applied together"
    (let [state (-> model/initial-state
                    (model/handle-drop :rx 0 0)
                    (model/open-gate-qubits-dialog 1)
                    (model/confirm-gate-qubits "3" "pi/4" "2"))
          gate (first (:items state))]
      (is (= 3 (:qubits gate)))
      (is (= "pi/4" (:angle gate)))
      (is (= 2 (:controls gate)))
      (is (nil? (:dialog state)))))

  (testing "invalid qubit counts keep the dialog open"
    (let [state (-> model/initial-state
                    (model/handle-drop :h 0 0)
                    (model/open-gate-qubits-dialog 1)
                    (model/confirm-gate-qubits "0"))]
      (is (some? (:dialog state)))
      (is (string? (get-in state [:dialog :error])))))

  (testing "angles are only kept on parameterised gates"
    (let [state (-> model/initial-state
                    (model/handle-drop :h 0 0)
                    (model/open-gate-qubits-dialog 1)
                    (model/confirm-gate-qubits "1" "pi/2"))]
      (is (nil? (get-in state [:items 0 :angle]))))))

(deftest measure-targets-test
  (let [state (-> model/initial-state
                  (model/add-item {:id 1 :type :register :component :quantum-register
                                   :name "q" :qubits 2 :x 0 :y 0})
                  (model/add-item {:id 2 :type :register :component :classical-register
                                   :classical true :name "c" :qubits 2 :x 0 :y 200})
                  (model/add-item {:id 3 :type :gate :component :measure :qubits 1
                                   :x 200 :y 0})
                  (model/add-wire 1 3 nil
                                  {:kind :output :index 0}
                                  {:kind :input :index 0})
                  (model/add-wire 3 2 nil
                                  {:kind :output :index 0}
                                  {:kind :input :index 1}))]
    (is (= [[1 0] [2 1]] (model/measure-targets state 3)))
    (is (= [[2 1]] (model/classical-targets state 3)))
    (is (= {:from 3 :to 2 :id 2
            :from-port {:kind :output :index 0}
            :to-port {:kind :input :index 1}}
           (model/output-target state 3 0)))))

(deftest history-test
  (let [s0 model/initial-state
        s1 (model/handle-drop s0 :h 0 0)
        s1 (model/append-history s0 s1)
        s2 (model/handle-drop s1 :x 200 0)
        s2 (model/append-history s1 s2)
        undone (model/undo s2)
        redone (model/redo undone)]
    (is (= 2 (count (:items s2))))
    (is (model/can-undo? s2))
    (is (not (model/can-redo? s2)))
    (is (= [:h] (mapv :component (:items undone))))
    (is (model/can-redo? undone))
    (is (= [:h :x] (mapv :component (:items redone))))
    (is (not (model/can-undo? (model/undo s0))))))

(deftest clipboard-test
  (let [state (-> model/initial-state
                  (model/add-item {:id 1 :type :register :component :quantum-register
                                   :name "q" :qubits 2 :x 0 :y 0})
                  (model/add-item {:id 2 :type :gate :component :h :x 100 :y 0})
                  (model/add-item {:id 3 :type :gate :component :x :x 200 :y 0})
                  (model/add-wire 1 2 nil
                                  {:kind :output :index 0}
                                  {:kind :input :index 0})
                  (model/add-wire 2 3 nil
                                  {:kind :output :index 0}
                                  {:kind :input :index 0})
                  (model/select-item 2)
                  (model/toggle-selection 3)
                  (model/copy-selection)
                  (model/paste-clipboard 400 100))
        pasted (filter #(>= (:id %) 4) (:items state))
        new-wires (filter #(>= (:id %) 3) (:wires state))]
    (is (= [4 5] (mapv :id pasted)))
    (is (= [400 500] (mapv :x pasted)))
    (is (= [100 100] (mapv :y pasted)))
    (is (= #{4 5} (:selected state)))
    (is (= [{:id 3 :from 4 :to 5
             :from-port {:kind :output :index 0}
             :to-port {:kind :input :index 0}}]
           (mapv #(select-keys % [:id :from :to :from-port :to-port]) new-wires)))
    (is (= model/initial-state
           (model/paste-clipboard model/initial-state 0 0)))))

(deftest duplicate-selection-test
  (let [state (-> model/initial-state
                  (model/handle-drop :h 100 100)
                  (model/select-item 1)
                  (model/duplicate-selection))]
    (is (= 2 (count (:items state))))
    (is (= [1 2] (mapv :id (:items state))))
    (is (= #{2} (:selected state)))))

(deftest canvas-preferences-test
  (testing "theme, search and snap size are normalised"
    (is (= :light (:theme model/initial-state)))
    (is (= :dark (:theme (model/toggle-theme model/initial-state))))
    (is (= :light (:theme (model/toggle-theme
                           (model/set-theme model/initial-state :dark)))))
    (is (= :light (:theme (model/set-theme model/initial-state :sepia))))
    (is (= "" (:component-search (model/set-component-search model/initial-state nil))))
    (is (= "bell" (:component-search (model/set-component-search model/initial-state "bell"))))
    (is (= 40 (:snap-size (model/set-snap-size model/initial-state 40))))
    (is (= (:snap-size model/initial-state)
           (:snap-size (model/set-snap-size model/initial-state -1))))))

(deftest annotation-and-metadata-test
  (testing "annotations are added through their dialog"
    (let [state (-> model/initial-state
                    (model/handle-drop :annotation 0 0)
                    (model/confirm-annotation "  hello  "))]
      (is (= "hello" (get-in state [:items 0 :text])))
      (is (= :annotation (get-in state [:items 0 :type])))
      (is (nil? (:dialog state)))))

  (testing "blank annotations are rejected"
    (let [state (-> model/initial-state
                    (model/handle-drop :annotation 0 0)
                    (model/confirm-annotation "   "))]
      (is (string? (get-in state [:dialog :error])))
      (is (empty? (:items state)))))

  (testing "metadata is trimmed and stored"
    (let [state (-> model/initial-state
                    (model/open-metadata-dialog)
                    (model/confirm-metadata {:name " Bell "
                                             :author " Ada "
                                             :description " test "
                                             :ignored "x"}))]
      (is (= {:name "Bell" :author "Ada" :description "test"}
             (:metadata state)))
      (is (nil? (:dialog state))))))

(deftest component-snap-test
  (testing "component snapping is disabled by default and can be toggled"
    (is (false? (:snap-components model/initial-state)))
    (is (true? (:snap-components (model/toggle-component-snap model/initial-state))))
    (is (false? (:snap-components
                 (-> model/initial-state
                     model/toggle-component-snap
                     model/toggle-component-snap)))))

  (testing "a moving item aligns to a nearby static item edge"
    (let [state (-> model/initial-state
                    model/toggle-snap
                    model/toggle-component-snap
                    (assoc :items [{:id 1 :type :gate :component :h :x 150 :y 0}
                                   {:id 2 :type :gate :component :x :x 100 :y 0}]))
          moved (model/move-items state {1 [150 0]} -5 0)]
      (is (= [144 0] ((juxt :x :y) (first (:items moved)))))))

  (testing "items further than the alignment threshold are left alone"
    (let [state (-> model/initial-state
                    model/toggle-snap
                    model/toggle-component-snap
                    (assoc :items [{:id 1 :type :gate :component :h :x 200 :y 0}
                                   {:id 2 :type :gate :component :x :x 100 :y 0}]))
          moved (model/move-items state {1 [200 0]} -5 0)]
      (is (= [195 0] ((juxt :x :y) (first (:items moved)))))))

  (testing "the vertical axis aligns independently"
    (let [state (-> model/initial-state
                    model/toggle-snap
                    model/toggle-component-snap
                    (assoc :items [{:id 1 :type :gate :component :h :x 300 :y 95}
                                   {:id 2 :type :gate :component :x :x 100 :y 50}]))
          moved (model/move-items state {1 [300 95]} 0 -5)]
      (is (= [300 86] ((juxt :x :y) (first (:items moved))))))))


(deftest coverage-edges-test
  (testing "marquee and item bounds handle every item type and reversed drags"
    (is (= [10 20 30 40] (model/marquee-rect {:start [30 40] :current [10 20]})))
    (is (= [1 2 45 38] (model/item-bounds {:x 1 :y 2 :type :gate})))
    (is (= [1 2 161 38] (model/item-bounds {:x 1 :y 2 :type :register})))
    (is (= [1 2 141 32] (model/item-bounds {:x 1 :y 2 :type :annotation})))
    (is (= [1 2 221 142] (model/item-bounds {:x 1 :y 2 :type :box})))
    (is (= [1 2 9 10] (model/item-bounds {:x 1 :y 2 :width 8 :height 8 :type :gate}))))

  (testing "gate and port counts tolerate malformed values"
    (is (= 1 (model/gate-target-count {})))
    (is (= 2 (model/gate-target-count {:qubits 2})))
    (is (= 0 (model/gate-control-count {})))
    (is (= 2 (model/gate-control-count {:controls 2})))
    (is (= 3 (model/gate-port-count {:qubits 2 :controls 1})))
    (is (false? (model/classical-register? {:type :register})))
    (is (true? (model/classical-register? {:classical true})))
    (is (= 2 (model/port-count {:type :register :qubits 2} :output)))
    (is (= 0 (model/port-count {:type :register :qubits 2} :input)))
    (is (= 2 (model/port-count {:type :register :classical true :qubits 2} :input)))
    (is (= 0 (model/port-count {:type :register :classical true :qubits 2} :output)))
    (is (= 2 (model/port-count {:type :gate :qubits 1 :controls 1} :input)))
    (is (= 0 (model/port-count {:type :annotation} :input)))
    (is (= 0 (model/port-count {:type :gate} :side))))

  (testing "wire drafting and cancellation edge cases"
    (is (= model/initial-state (model/update-wire-draft model/initial-state 1 2)))
    (is (= model/initial-state (model/add-wire-point model/initial-state 1 2)))
    (is (= model/initial-state (model/cancel-wire-draft model/initial-state)))
    (is (= model/initial-state (model/finish-wire model/initial-state 1)))
    (let [state (model/start-wire-draft model/initial-state 1 5 6
                                        {:kind :output :index 0})]
      (is (= {:from 1 :x 5 :y 6 :points [] :from-port {:kind :output :index 0}}
             (:wire-draft state)))
      (is (= 7 (get-in (model/update-wire-draft state 7 8) [:wire-draft :x])))
      (let [with-points (-> state
                            (model/add-wire-point 10 10)
                            (model/add-wire-point 10 10))]
        (is (= [[10 10]] (get-in with-points [:wire-draft :points]))))))

  (testing "invalid and duplicate wires are ignored"
    (let [base (-> model/initial-state
                   (model/add-item {:id 1 :type :register :component :quantum-register
                                    :name "q" :qubits 2 :x 0 :y 0})
                   (model/add-item {:id 2 :type :gate :component :h :x 100 :y 0})
                   (model/add-item {:id 3 :type :gate :component :x :x 200 :y 0})
                   (model/add-item {:id 4 :type :register :component :quantum-register
                                    :name "r" :qubits 2 :x 0 :y 200}))]
      (is (empty? (:wires (model/add-wire base 1 1))))
      (is (empty? (:wires (model/add-wire base 1 99))))
      (is (empty? (:wires (model/add-wire base 1 4))))
      (is (empty? (:wires (model/add-wire base 1 2 nil
                                          {:kind :input :index 0}
                                          {:kind :input :index 0}))))
      (is (empty? (:wires (model/add-wire base 1 2 nil
                                          {:kind :output :index 9}
                                          {:kind :input :index 0}))))
      (let [wired (model/add-wire base 1 2 nil
                                  {:kind :output :index 0}
                                  {:kind :input :index 0})]
        (is (= 1 (count (:wires wired))))
        (is (= 1 (count (:wires (model/add-wire wired 1 2 nil
                                                            {:kind :output :index 0}
                                                            {:kind :input :index 0})))))
        (is (= 1 (count (:wires (model/add-wire wired 2 1 nil
                                                            {:kind :output :index 0}
                                                            {:kind :input :index 0}))))))))

  (testing "context menus, rotations, and dialogs"
    (let [state (-> model/initial-state
                    (model/add-item {:id 1 :type :gate :component :rx :x 0 :y 0})
                    (model/add-item {:id 2 :type :gate :component :h :x 100 :y 0})
                    (model/add-item {:id 3 :type :annotation :component :annotation
                                     :text "old" :x 0 :y 100})
                    (model/add-item {:id 4 :type :box :component :box
                                     :text "group" :x 0 :y 200}))]
      (is (= {:item-id 1 :x 5 :y 6} (:context-menu (model/open-context-menu state 1 5 6))))
      (is (nil? (:context-menu (model/close-context-menu state))))
      (is (= 90 (get-in (model/rotate-item state 1) [:items 0 :rotation])))
      (is (= 45 (get-in (model/rotate-item state 1 45) [:items 0 :rotation])))
      (is (= 0 (get-in (model/rotate-item (model/rotate-item state 1) 1 270)
                       [:items 0 :rotation])))
      (is (= :classical (let [s (model/open-register-dialog state 1 2 true)]
                          (if (get-in s [:dialog :classical]) :classical :quantum))))
      (is (= 3 (get-in (model/open-annotation-editor state 3) [:dialog :item-id])))
      (is (= 4 (get-in (model/open-annotation-editor state 4) [:dialog :item-id])))
      (is (= state (model/open-annotation-editor state 2)))
      (is (= state (model/open-annotation-editor state 99)))
      (is (= :metadata (get-in (model/open-metadata-dialog state) [:dialog :type])))
      (is (= :export (get-in (model/open-export-dialog state) [:dialog :type])))
      (is (= :library (get-in (model/open-library-dialog state) [:dialog :type])))
      (is (= :diff (get-in (model/open-diff-dialog state "diff") [:dialog :type])))
      (is (= state (model/open-gate-qubits-dialog state 3)))
      (is (= state (model/open-gate-qubits-dialog state 99)))
      (is (= "1" (get-in (model/open-gate-qubits-dialog
                          (model/set-gate-controls state 1 0) 1)
                         [:dialog :qubits])))
      (is (= "1" (get-in (model/open-gate-qubits-dialog
                          (model/set-gate-controls state 2 1) 2)
                         [:dialog :controls])))))

  (testing "gate qubit confirmation validates and applies optional fields"
    (let [base (-> model/initial-state
                   (model/add-item {:id 1 :type :gate :component :rx :x 0 :y 0})
                   (model/add-item {:id 2 :type :gate :component :cx :qubits 2 :x 0 :y 0})
                   (model/open-gate-qubits-dialog 1))]
      (is (= model/initial-state (model/confirm-gate-qubits model/initial-state "1")))
      (is (= 2 (get-in (model/open-gate-qubits-dialog base 2) [:dialog :gate-id])))
      (is (= "2" (get-in (model/open-gate-qubits-dialog base 2) [:dialog :qubits])))
      (let [confirmed (model/confirm-gate-qubits base "3" "pi" "0")]
        (is (= 3 (get-in confirmed [:items 0 :qubits])))
        (is (= "pi" (get-in confirmed [:items 0 :angle])))
        (is (nil? (get-in confirmed [:dialog]))))
      (let [bad (model/confirm-gate-qubits base "0")]
        (is (string? (get-in bad [:dialog :error])))))
    (let [custom (-> model/initial-state
                     (model/open-custom-dialog 0 0))]
      (is (= model/initial-state (model/confirm-custom-gate model/initial-state "1" "gate x {}")))
      (is (string? (get-in (model/confirm-custom-gate custom "0" "") [:dialog :qubits])))))

  (testing "register, annotation, and box edge cases"
    (let [classical (model/open-register-dialog model/initial-state 0 0 true)
          confirmed (model/confirm-register classical " c " "2")]
      (is (true? (get-in confirmed [:items 0 :classical])))
      (is (= :classical-register (get-in confirmed [:items 0 :component]))))
    (let [state (-> model/initial-state
                    (model/handle-drop :box 10 20))]
      (is (= :box (get-in state [:items 0 :type])))
      (is (= "Group" (get-in state [:items 0 :text]))))
    (let [edited (model/confirm-annotation
                  (assoc model/initial-state
                         :items [{:id 1 :type :annotation :component :annotation
                                  :text "old" :x 0 :y 0}]
                         :dialog {:type :annotation :item-id 1
                                  :x 0 :y 0 :text "old" :error nil})
                  " new ")]
      (is (= "new" (get-in edited [:items 0 :text])))
      (is (nil? (:dialog edited))))
    (is (= model/initial-state (model/confirm-annotation model/initial-state "x")))
    (is (nil? (:dialog (model/confirm-annotation
                        (assoc model/initial-state :dialog {:type :annotation
                                                            :x 0 :y 0 :text "" :error nil})
                        "x"))))
    (is (= model/initial-state (model/confirm-register model/initial-state "q" "1")))
    (is (= model/initial-state (model/confirm-metadata model/initial-state {})))
    (is (nil? (:dialog (model/cancel-dialog
                        (assoc model/initial-state :dialog {:type :x})))))
    (is (= model/initial-state (model/cancel-dialog model/initial-state)))
    (is (= model/initial-state (model/cancel-register model/initial-state))))

  (testing "history, clipboard, and view helpers cover fallback paths"
    (is (= {:items [] :wires [] :next-id 1 :next-wire-id 1
            :metadata {:name "" :author "" :description ""}
            :includes ["stdgates.inc"] :raw-statements []}
           (model/snapshot model/initial-state)))
    (is (nil? (model/selection-snapshot model/initial-state)))
    (is (= model/initial-state (model/copy-selection model/initial-state)))
    (is (= model/initial-state (model/paste-clipboard model/initial-state 0 0)))
    (is (= model/initial-state (model/duplicate-selection model/initial-state)))
    (is (= model/initial-state (model/set-clipboard model/initial-state nil)))
    (is (= {:a 1} (:clipboard (model/set-clipboard model/initial-state {:a 1}))))
    (is (= model/initial-state (model/paste-snapshot model/initial-state {} 0 0)))
    (is (= model/initial-state (model/paste-snapshot
                                model/initial-state {:items [] :wires []} 0 0)))
    (is (= model/initial-state (model/redo model/initial-state)))
    (is (= model/initial-state (model/undo model/initial-state)))
    (is (false? (model/can-undo? model/initial-state)))
    (is (false? (model/can-redo? model/initial-state)))
    (is (= {:pan-x 0 :pan-y 0 :zoom 1} (:view (model/fit-view model/initial-state))))
    (let [fit (model/fit-view (assoc model/initial-state
                                     :items [{:id 1 :type :gate :component :h
                                              :x 100 :y 100}])
                              100 100)]
      (is (pos? (get-in fit [:view :zoom]))))
    (is (= 4 (get-in (model/fit-view (assoc model/initial-state
                                            :items [{:id 1 :type :gate :component :h
                                                     :x 0 :y 0}])
                                     100000 100000)
                     [:view :zoom])))))

