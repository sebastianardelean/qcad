(ns app.qasm-test
  (:require [clojure.string :as str]
            [cljs.test :refer [deftest is testing]]
            [app.qasm :as qasm]))

(defn- register
  ([id name qubits x y]
   {:id id :type :register :component :quantum-register
    :name name :qubits qubits :x x :y y}))

(defn- gate [id component x y]
  {:id id :type :gate :component component :x x :y y})

(defn- wire [id from to]
  {:id id :from from :to to})

(defn- circuit [items wires]
  {:items items :wires wires})

(defn- lines [state]
  (str/split-lines (qasm/to-qasm state)))

(defn- body-lines [state]
  (remove #(or (str/blank? %)
               (str/starts-with? % "OPENQASM")
               (str/starts-with? % "include")
               (str/starts-with? % "qubit["))
          (lines state)))

(deftest empty-circuit-test
  (testing "an empty circuit is still a valid OpenQASM 3 program"
    (is (= "OPENQASM 3.0;\ninclude \"stdgates.inc\";\n"
           (qasm/to-qasm (circuit [] []))))))

(deftest register-declaration-test
  (testing "registers become qubit declarations"
    (let [state (circuit [(register 1 "data" 3 20 40)] [])]
      (is (= ["OPENQASM 3.0;"
              "include \"stdgates.inc\";"
              ""
              "qubit[3] data;"]
             (lines state)))
      (is (str/ends-with? (qasm/to-qasm state) "\n"))))

  (testing "registers are declared top-to-bottom, then left-to-right"
    (let [state (circuit [(register 1 "right" 1 200 0)
                          (register 2 "bottom" 1 0 200)
                          (register 3 "top" 1 100 0)]
                         [])]
      (is (= ["qubit[1] top;"
              "qubit[1] right;"
              "qubit[1] bottom;"]
             (filter #(str/starts-with? % "qubit[") (lines state)))))))

(deftest identifier-test
  (testing "register names are converted to unique OpenQASM identifiers"
    (let [state (circuit [(register 1 "register-1" 2 0 0)
                          (register 2 "1st" 1 0 10)
                          (register 3 "x" 1 0 20)
                          (register 4 "data" 1 0 30)
                          (register 5 "data" 1 0 40)
                          (register 6 "data_2" 1 0 50)
                          (register 7 "data" 1 0 60)
                          (register 8 "   " 1 0 70)
                          (register 9 "my reg" 1 0 80)]
                         [])]
      (is (= ["qubit[2] register_1;"
              "qubit[1] q_1st;"
              "qubit[1] q_x;"
              "qubit[1] data;"
              "qubit[1] data_2;"
              "qubit[1] data_2_2;"
              "qubit[1] data_3;"
              "qubit[1] q;"
              "qubit[1] my_reg;"]
             (filter #(str/starts-with? % "qubit[") (lines state)))))))

(deftest gate-call-test
  (testing "gates become calls on the register they are wired to"
    (let [state (circuit [(register 1 "data" 3 0 0)
                          (gate 2 :h 100 0)
                          (gate 3 :rx 200 0)
                          (gate 4 :sdag 300 0)
                          (gate 5 :tdag 400 0)
                          (gate 6 :i 500 0)
                          (gate 7 :p 600 0)]
                         [(wire 1 1 2)
                          (wire 2 2 3)
                          (wire 3 3 4)
                          (wire 4 4 5)
                          (wire 5 5 6)
                          (wire 6 6 7)])]
      (is (= ["h data;"
              "rx(pi/2) data;"
              "sdg data;"
              "tdg data;"
              "id data;"
              "p(pi/2) data;"]
             (body-lines state)))))

  (testing "gates are emitted left-to-right, then top-to-bottom"
    (let [state (circuit [(register 1 "data" 1 0 0)
                          (gate 2 :x 300 0)
                          (gate 3 :h 200 0)
                          (gate 4 :z 200 100)]
                         [(wire 1 1 2)
                          (wire 2 1 3)
                          (wire 3 1 4)])]
      (is (= ["h data;" "z data;" "x data;"]
             (body-lines state))))))

(deftest multi-register-gate-test
  (testing "a gate connected to several registers is applied to each one"
    (let [state (circuit [(register 1 "a" 2 0 0)
                          (register 2 "b" 1 0 100)
                          (gate 3 :h 200 0)]
                         [(wire 1 1 3)
                          (wire 2 2 3)])]
      (is (= ["qubit[2] a;"
              "qubit[1] b;"
              ""
              "h a;"
              "h b;"]
             (drop 3 (lines state)))))))

(deftest unconnected-gate-test
  (testing "gates without a register are reported instead of emitted"
    (let [state (circuit [(register 1 "data" 1 0 0)
                          (gate 2 :h 100 0)
                          (gate 3 :x 200 0)]
                         [(wire 1 1 3)
                          (wire 2 99 200)])]
      (is (= ["x data;"
              "// Unconnected H gate omitted."]
             (body-lines state))))))

(deftest gate-chain-test
  (testing "registers propagate through chains and cycles of gates"
    (let [state (circuit [(register 1 "data" 1 0 0)
                          (gate 2 :h 100 0)
                          (gate 3 :x 200 0)
                          (gate 4 :z 300 0)]
                         [(wire 1 1 2)
                          (wire 2 2 3)
                          (wire 3 3 4)
                          (wire 4 4 2)])]
      (is (= ["h data;" "x data;" "z data;"]
             (body-lines state))))))

(deftest unknown-gate-test
  (testing "unknown connected gates keep their component name"
    (let [state (circuit [(register 1 "data" 1 0 0)
                          (gate 2 :mystery 100 0)
                          (gate 3 :other 200 0)]
                         [(wire 1 1 2)])]
      (is (= ["mystery data;"
              "// Unconnected other gate omitted."]
             (body-lines state))))))

(deftest targeted-gate-test
  (testing "selected qubits are emitted as indexed operands"
    (let [state (-> (circuit [(register 1 "a" 2 0 0)
                              (register 2 "b" 3 0 100)
                              (gate 3 :h 100 0)
                              (gate 4 :x 200 0)]
                             [(wire 1 1 3)
                              (wire 2 2 3)
                              (wire 3 3 4)])
                    (assoc-in [:items 2 :targets] {1 #{1} 2 #{0 2}})
                    (assoc-in [:items 3 :targets] {2 #{1}}))]
      (is (= ["h a[1];"
              "h b[0];"
              "h b[2];"
              "x b[1];"]
             (body-lines state)))))

  (testing "targets outside the connected registers are ignored"
    (let [state (-> (circuit [(register 1 "a" 2 0 0)
                              (register 2 "b" 2 0 100)
                              (gate 3 :h 100 0)]
                             [(wire 1 1 3)])
                    (assoc-in [:items 2 :targets] {1 #{0 1} 2 #{1}}))]
      (is (= ["h a[0];" "h a[1];"]
             (body-lines state)))))

  (testing "a gate with no selected qubits is reported"
    (let [state (-> (circuit [(register 1 "a" 2 0 0)
                              (gate 2 :h 100 0)]
                             [(wire 1 1 2)])
                    (assoc-in [:items 1 :targets] {}))]
      (is (= ["// H gate has no selected qubits."]
             (body-lines state))))))
