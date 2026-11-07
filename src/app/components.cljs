(ns app.components)

(def components
  [{:id :quantum-register :label "Quantum register" :kind :register}
   {:id :classical-register :label "Classical register" :kind :register :classical true}
   {:id :h :label "H" :kind :gate}
   {:id :x :label "X" :kind :gate}
   {:id :y :label "Y" :kind :gate}
   {:id :z :label "Z" :kind :gate}
   {:id :s :label "S" :kind :gate}
   {:id :t :label "T" :kind :gate}
   {:id :sdag :label "Sdag" :kind :gate}
   {:id :tdag :label "Tdag" :kind :gate}
   {:id :rx :label "Rx" :kind :gate :parameterized true}
   {:id :ry :label "Ry" :kind :gate :parameterized true}
   {:id :rz :label "Rz" :kind :gate :parameterized true}
   {:id :p :label "P" :kind :gate :parameterized true}
   {:id :i :label "I" :kind :gate}
   {:id :cx :label "CX" :kind :gate :multi-qubit true}
   {:id :cz :label "CZ" :kind :gate :multi-qubit true}
   {:id :swap :label "SWAP" :kind :gate :multi-qubit true}
   {:id :ccx :label "CCX" :kind :gate :multi-qubit true}
   {:id :measure :label "Measure" :kind :gate :measure true}
   {:id :reset :label "Reset" :kind :gate :reset true}
   {:id :barrier :label "Barrier" :kind :gate :barrier true}
   {:id :custom :label "Custom" :kind :custom}
   {:id :annotation :label "Text" :kind :annotation}
   {:id :box :label "Box" :kind :box}])

(defn by-id [id]
  (first (filter #(= id (:id %)) components)))

(defn classical? [component-id]
  (boolean (:classical (by-id component-id))))

(defn parameterized? [component-id]
  (boolean (:parameterized (by-id component-id))))

(defn multi-qubit? [component-id]
  (boolean (:multi-qubit (by-id component-id))))

(defn measure? [component-id]
  (boolean (:measure (by-id component-id))))

(defn reset? [component-id]
  (boolean (:reset (by-id component-id))))

(defn barrier? [component-id]
  (boolean (:barrier (by-id component-id))))

(defn default-qubits [component-id]
  (case component-id
    :cx 2
    :cz 2
    :swap 2
    :ccx 3
    :measure 1
    :reset 1
    :barrier 1
    1))
