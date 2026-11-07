(ns app.export-test
  (:require [clojure.string :as str]
            [cljs.test :refer [deftest is testing]]
            [app.export :as export]
            [app.model :as model]))

(defn- register
  ([id name qubits x y] (register id name qubits x y false))
  ([id name qubits x y classical]
   (cond-> {:id id :type :register
            :component (if classical :classical-register :quantum-register)
            :name name :qubits qubits :x x :y y}
     classical (assoc :classical true))))

(defn- gate
  ([id component x y] (gate id component x y 1))
  ([id component x y qubits]
   (cond-> {:id id :type :gate :component component :x x :y y}
     (not= 1 qubits) (assoc :qubits qubits))))

(defn- port-wire [id from to from-index to-index]
  {:id id
   :from from
   :to to
   :from-port {:kind :output :index from-index}
   :to-port {:kind :input :index to-index}})

(defn- circuit [items wires]
  (merge model/initial-state {:items items :wires wires}))

(deftest svg-document-test
  (testing "the SVG export contains the circuit items, wires and labels"
    (let [state (circuit
                 [(register 1 "q" 2 20 40)
                  (gate 2 :h 220 40)
                  (assoc (gate 3 :rx 360 40) :angle "pi/4")
                  (register 4 "c" 1 20 240 true)
                  (gate 5 :measure 220 240)
                  (assoc (gate 6 :custom 520 40 2)
                         :qasm "gate pair a, b { h a; }")]
                 [(port-wire 1 1 2 0 0)
                  (port-wire 2 1 5 1 0)
                  (port-wire 3 5 4 0 0)])
          svg (export/to-svg state)]
      (is (str/starts-with? svg "<?xml"))
      (is (str/includes? svg "<svg"))
      (is (str/includes? svg "q (2 qubits)"))
      (is (str/includes? svg "c (1 bit)"))
      (is (str/includes? svg ">H<"))
      (is (str/includes? svg "Rx(pi/4)"))
      (is (str/includes? svg ">Custom<"))
      (is (str/includes? svg "canvas-wire"))
      (is (str/includes? svg "canvas-wire-classical"))
      (is (str/includes? svg "stroke-dasharray=\"6 4\"")))))

(deftest svg-options-test
  (testing "the background can be omitted for transparent exports"
    (let [state (circuit [(gate 1 :h 40 40)] [])
          opaque (export/to-svg state)
          transparent (export/to-svg state {:transparent true})]
      (is (str/includes? opaque "<rect x=\"-24\" y=\"-24\""))
      (is (not (str/includes? transparent "<rect x=\"-24\" y=\"-24\"")))))

  (testing "gate rotations are preserved"
    (let [state (circuit [(assoc (gate 1 :h 40 40) :rotation 90)] [])
          svg (export/to-svg state)]
      (is (str/includes? svg "rotate(90")))))

  (testing "annotations and boxes render their text"
    (let [state (circuit [{:id 1 :type :annotation :text "note" :x 0 :y 0}
                          {:id 2 :type :box :text "group" :x 0 :y 80}]
                         [])
          svg (export/to-svg state)]
      (is (str/includes? svg "note"))
      (is (str/includes? svg "group"))))

(deftest empty-and-unknown-items-test
  (testing "an empty circuit still renders a usable SVG canvas"
    (let [svg (export/to-svg (circuit [] []))]
      (is (str/includes? svg "viewBox=\"-24 -24 148 148\""))))

  (testing "unknown item types do not break the export"
    (let [svg (export/to-svg (circuit [{:id 1 :type :mystery :x 0 :y 0}] []))]
      (is (str/includes? svg "<svg")))))

(deftest data-url-test
  (testing "SVG data URLs are URL encoded"
    (let [url (export/to-data-url "<svg>hi</svg>")]
      (is (str/starts-with? url "data:image/svg+xml;charset=utf-8,"))
      (is (str/includes? url "%3Csvg%3E")))))

(deftest control-marker-export-test
  (testing "control qubits are drawn as filled markers connected to the target"
    (let [state (circuit [(assoc (gate 1 :h 40 40 2) :controls 1)] [])
          svg (export/to-svg state)]
      (is (str/includes? svg "canvas-control-line"))
      (is (str/includes? svg "<circle"))
      (is (str/includes? svg "r=\"3\"")))))

(deftest include-ui-export-test
  (testing "the canvas grid is only included when requested"
    (let [state (circuit [(gate 1 :h 40 40)] [])
          plain (export/to-svg state)
          grid (export/to-svg state {:include-ui true})]
      (is (not (str/includes? plain "qcad-grid")))
      (is (str/includes? grid "qcad-grid"))
      (is (str/includes? grid "export-grid")))))

(deftest export-edge-test
  (testing "wires without ports and missing endpoints are tolerated"
    (let [state (circuit [(gate 1 :h 0 0) (gate 2 :x 100 0)]
                         [{:id 1 :from 1 :to 2}
                          {:id 2 :from 1 :to 99}])
          svg (export/to-svg state)]
      (is (str/includes? svg "canvas-wire"))
      (is (= 1 (count (re-seq #"<polyline" svg))))))

  (testing "unknown gates fall back to their component name"
    (let [state (circuit [{:id 1 :type :gate :component :mystery :x 0 :y 0}]
                         [])]
      (is (str/includes? (export/to-svg state) ">mystery<"))))

  (testing "items with no component name fall back to Gate"
    (let [state (circuit [{:id 1 :type :gate :x 0 :y 0}] [])]
      (is (str/includes? (export/to-svg state) ">Gate<")))))
