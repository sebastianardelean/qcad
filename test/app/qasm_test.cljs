(ns app.qasm-test
  (:require [clojure.string :as str]
            [cljs.test :refer [deftest is testing]]
            [app.qasm :as qasm]))

(defn- register
  ([id name qubits x y]
   {:id id :type :register :component :quantum-register
    :name name :qubits qubits :x x :y y}))

(defn- classical-register [id name bits x y]
  {:id id :type :register :component :classical-register
   :name name :qubits bits :x x :y y :classical true})

(defn- multi-gate [id component qubits x y]
  {:id id :type :gate :component component :qubits qubits :x x :y y})

(defn- gate [id component x y]
  {:id id :type :gate :component component :x x :y y})

(defn- wire [id from to]
  {:id id :from from :to to})

(defn- port-wire [id from to from-index to-index]
  (assoc (wire id from to)
         :from-port {:kind :output :index from-index}
         :to-port {:kind :input :index to-index}))

(defn- custom-gate [id qubits qasm x y]
  {:id id :type :gate :component :custom :qubits qubits :qasm qasm :x x :y y})

(defn- circuit [items wires]
  {:items items :wires wires})

(defn- lines [state]
  (str/split-lines (qasm/to-qasm state)))

(defn- body-lines [state]
  (remove #(or (str/blank? %)
               (str/starts-with? % "OPENQASM")
               (str/starts-with? % "include")
               (str/starts-with? % "qubit[")
               (str/starts-with? % "bit["))
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

(deftest custom-gate-test
  (testing "a custom gate definition is emitted and called with its wired operands"
    (let [state (circuit [(register 1 "data" 2 0 0)
                          (custom-gate 2 2 "gate pair a, b {\n  h a;\n  cx a, b;\n}"
                                       100 0)]
                         [(port-wire 1 1 2 0 0)
                          (port-wire 2 1 2 1 1)])]
      (is (= ["OPENQASM 3.0;"
              "include \"stdgates.inc\";"
              ""
              "gate pair a, b {"
              "  h a;"
              "  cx a, b;"
              "}"
              ""
              "qubit[2] data;"
              ""
              "pair data[0], data[1];"]
             (lines state)))))

  (testing "identical custom gate definitions are emitted once"
    (let [state (circuit [(register 1 "a" 1 0 0)
                          (custom-gate 2 1 "gate flip q { x q; }" 100 0)
                          (custom-gate 3 1 "gate flip q { x q; }" 200 0)]
                         [(port-wire 1 1 2 0 0)
                          (port-wire 2 1 3 0 0)])]
      (is (= 1 (count (filter #(= "gate flip q { x q; }" %) (lines state)))))
      (is (= ["gate flip q { x q; }" "flip a[0];" "flip a[0];"]
             (body-lines state)))))

  (testing "a QASM statement without a gate declaration is emitted verbatim"
    (let [state (circuit [(register 1 "data" 1 0 0)
                          (custom-gate 2 1 "h data[0];" 100 0)]
                         [(port-wire 1 1 2 0 0)])]
      (is (= ["h data[0];"] (body-lines state)))))

  (testing "an incompletely wired custom gate is not emitted as an invalid call"
    (let [state (circuit [(register 1 "register_1" 2 0 0)
                          (custom-gate 2 2 "gate pair a, b {\n  h a;\n  cx a, b;\n}"
                                       100 0)]
                         [(port-wire 1 1 2 0 0)])
          body (body-lines state)]
      (is (not-any? #(str/starts-with? % "pair ") body))
      (is (some #(str/includes? % "1 of 2 qubits connected") body))))

  (testing "a custom gate whose declaration arity differs from its item is not called"
    (let [state (circuit [(register 1 "data" 2 0 0)
                          (custom-gate 2 1 "gate pair a, b { h a; }" 100 0)]
                         [(port-wire 1 1 2 0 0)])
          body (body-lines state)]
      (is (not-any? #(str/starts-with? % "pair ") body))
      (is (some #(str/includes? % "1 of 2 qubits connected") body)))))

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

(deftest port-wire-target-test
  (testing "gate targets come from wired input ports in port order"
    (let [state (circuit [(register 1 "a" 3 0 0)
                          (assoc (gate 2 :h 100 0) :qubits 2)]
                         [(port-wire 1 1 2 2 1)
                          (port-wire 2 1 2 0 0)])]
      (is (= ["h a[0];"
              "h a[2];"]
             (body-lines state)))))

  (testing "wire targets take precedence over legacy targets"
    (let [state (-> (circuit [(register 1 "a" 2 0 0)
                              (gate 2 :h 100 0)]
                             [(port-wire 1 1 2 1 0)])
                    (assoc-in [:items 1 :targets] {1 #{0}}))]
      (is (= ["h a[1];"]
             (body-lines state)))))

  (testing "gate chains propagate the wired qubit through output ports"
    (let [state (circuit [(register 1 "a" 2 0 0)
                          (assoc (gate 2 :h 100 0) :qubits 2)
                          (assoc (gate 3 :x 200 0) :qubits 2)]
                         [(port-wire 1 1 2 1 0)
                          (port-wire 2 2 3 0 1)])]
      (is (= ["h a[1];"
              "x a[1];"]
             (body-lines state)))))

  (testing "a port-wired gate only emits its connected input ports"
    (let [state (circuit [(register 1 "a" 2 0 0)
                          (assoc (gate 2 :h 100 0) :qubits 2)]
                         [(port-wire 1 1 2 1 0)])]
      (is (= ["h a[1];"]
             (body-lines state)))))

  (testing "a port-wired gate with no traceable register is reported"
    (let [state (circuit [(gate 1 :h 100 0)
                          (gate 2 :x 200 0)]
                         [(port-wire 1 1 2 0 0)])]
      (is (= ["// X gate has no connected qubits."
              "// Unconnected H gate omitted."]
             (body-lines state))))))

(deftest from-qasm-empty-test
  (testing "an empty program produces an empty circuit"
    (let [state (qasm/from-qasm "OPENQASM 3.0;\ninclude \"stdgates.inc\";\n")]
      (is (= [] (:items state)))
      (is (= [] (:wires state)))
      (is (= 1 (:next-id state)))
      (is (= 1 (:next-wire-id state))))))

(deftest from-qasm-builtins-test
  (testing "registers, built-in gates, chains, and bare registers are imported"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "include \"stdgates.inc\";\n"
                      "qubit[2] data;\n"
                      "h data[0];\n"
                      "rx(pi/2) data[1];\n"
                      "h data;\n"))
          gates (filter #(= :gate (:type %)) (:items state))]
      (is (= [{:id 1 :type :register :component :quantum-register
               :name "data" :qubits 2 :x 40 :y 40}]
             (filter #(= :register (:type %)) (:items state))))
      (is (= [:h :rx :h] (mapv :component gates)))
      (is (= [1 1 2] (mapv :qubits gates)))
      (is (= 4 (count (:wires state))))
      (is (= [{:id 3 :from 2 :to 4
               :from-port {:kind :output :index 0}
               :to-port {:kind :input :index 0}}
              {:id 4 :from 3 :to 4
               :from-port {:kind :output :index 0}
               :to-port {:kind :input :index 1}}]
             (take-last 2 (:wires state))))
      (is (= ["h data[0];"
              "rx(pi/2) data[1];"
              "h data[0];"
              "h data[1];"]
             (body-lines state))))))

(deftest from-qasm-registers-test
  (testing "bare operands expand to all qubits in each register"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[2] a;\n"
                      "qubit[1] b;\n"
                      "h a;\n"
                      "h b;\n"))]
      (is (= ["a" "b"] (mapv :name (filter #(= :register (:type %))
                                           (:items state)))))
      (is (= [2 1] (mapv :qubits (filter #(= :gate (:type %))
                                         (:items state)))))
      (is (= ["h a[0];" "h a[1];" "h b[0];"] (body-lines state)))
      (is (= 3 (count (:wires state)))))))

(deftest from-qasm-custom-test
  (testing "custom gate declarations are stored and calls are wired"
    (let [definition "gate pair a, b {\n  h a;\n  cx a, b;\n}"
          state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "include \"stdgates.inc\";\n\n"
                      definition "\n\n"
                      "qubit[2] data;\n\n"
                      "pair data[0], data[1];\n"))
          custom (second (:items state))]
      (is (= :custom (:component custom)))
      (is (= 2 (:qubits custom)))
      (is (= definition (:qasm custom)))
      (is (= ["OPENQASM 3.0;"
              "include \"stdgates.inc\";"
              ""
              "gate pair a, b {"
              "  h a;"
              "  cx a, b;"
              "}"
              ""
              "qubit[2] data;"
              ""
              "pair data[0], data[1];"]
             (lines state))))))

(deftest from-qasm-unknown-gate-test
  (testing "an undeclared gate call is preserved as a custom gate"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[2] data;\n"
                       "frobnicate data[0], data[1];\n"))
          custom (second (:items state))]
      (is (= :custom (:component custom)))
      (is (= 2 (:qubits custom)))
      (is (= "frobnicate data[0], data[1];" (:qasm custom)))
      (is (= ["frobnicate data[0], data[1];"] (body-lines state))))))

(deftest from-qasm-comments-test
  (testing "line and block comments are ignored"
    (let [state (qasm/from-qasm
                 (str "/* generated by QCAD */\n"
                      "OPENQASM 3.0;\n"
                      "qubit[1] data;\n"
                      "// apply a gate\n"
                      "h data[0];\n"))]
      (is (= [:h] (mapv :component (filter #(= :gate (:type %))
                                           (:items state)))))
      (is (= 1 (count (:wires state)))))))

(deftest measurement-export-test
  (testing "measurement connects a quantum operand to a classical bit"
    (let [state (circuit [(register 1 "q" 2 0 0)
                          (classical-register 2 "c" 2 0 200)
                          (multi-gate 3 :measure 1 100 0)]
                         [(port-wire 1 1 3 1 0)
                          (port-wire 2 3 2 0 1)])
          text (qasm/to-qasm state)]
      (is (str/includes? text "bit[2] c;"))
      (is (= ["measure q[1] -> c[1];"] (body-lines state)))))

  (testing "a measurement without a classical target is reported"
    (let [state (circuit [(register 1 "q" 1 0 0)
                          (multi-gate 2 :measure 1 100 0)]
                         [(port-wire 1 1 2 0 0)])]
      (is (= ["// Measure gate has no classical target."] (body-lines state)))))

  (testing "a measurement without a quantum target is reported"
    (let [state (circuit [(classical-register 1 "c" 1 0 0)
                          (multi-gate 2 :measure 1 100 0)]
                         [(port-wire 1 2 1 0 0)])]
      (is (= ["// Measure gate has no connected qubits."] (body-lines state))))))

(deftest reset-and-barrier-export-test
  (testing "reset is emitted once per connected qubit"
    (let [state (circuit [(register 1 "q" 2 0 0)
                          (multi-gate 2 :reset 2 100 0)]
                         [(port-wire 1 1 2 0 0)
                          (port-wire 2 1 2 1 1)])]
      (is (= ["reset q[0];" "reset q[1];"] (body-lines state)))))

  (testing "barrier lists all connected qubits"
    (let [state (circuit [(register 1 "q" 2 0 0)
                          (multi-gate 2 :barrier 2 100 0)]
                         [(port-wire 1 1 2 0 0)
                          (port-wire 2 1 2 1 1)])]
      (is (= ["barrier q[0], q[1];"] (body-lines state)))))

  (testing "an unconnected barrier is omitted"
    (let [state (circuit [(register 1 "q" 1 0 0)
                          (multi-gate 2 :barrier 1 100 0)]
                         [])]
      (is (= ["// Unconnected Barrier gate omitted."] (body-lines state))))))

(deftest angle-and-control-export-test
  (testing "configured rotation angles are exported"
    (let [state (circuit [(register 1 "q" 1 0 0)
                          (assoc (gate 2 :rx 100 0) :angle "pi/4")]
                         [(port-wire 1 1 2 0 0)])]
      (is (= ["rx(pi/4) q[0];"] (body-lines state)))))

  (testing "controlled gates use the ctrl modifier"
    (let [state (circuit [(register 1 "q" 2 0 0)
                          (assoc (gate 2 :h 100 0) :controls 1)]
                         [(port-wire 1 1 2 0 0)
                          (port-wire 2 1 2 1 1)])]
      (is (= ["ctrl @ h q[0], q[1];"] (body-lines state))))))

(deftest metadata-and-include-test
  (testing "metadata comments survive an import/export round trip"
    (let [source (str "// name: Bell\n"
                      "// author: Ada\n"
                      "// description: demo\n"
                      "OPENQASM 3.0;\n"
                      "include \"stdgates.inc\";\n"
                      "qubit[1] q;\n")
          state (qasm/from-qasm source)
          text (qasm/to-qasm state)]
      (is (= {:name "Bell" :author "Ada" :description "demo"}
             (:metadata state)))
      (is (str/includes? text "// name: Bell"))
      (is (str/includes? text "// author: Ada"))
      (is (str/includes? text "// description: demo")))))

(deftest import-measurement-and-classical-test
  (testing "OpenQASM measurement and classical registers are imported"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "include \"stdgates.inc\";\n"
                      "qubit[2] q;\n"
                      "bit[2] c;\n"
                      "h q[0];\n"
                      "reset q[1];\n"
                      "barrier q[0], q[1];\n"
                      "measure q[0] -> c[1];\n"))
          registers (filter #(= :register (:type %)) (:items state))
          gates (filter #(= :gate (:type %)) (:items state))]
      (is (= ["q" "c"] (mapv :name registers)))
      (is (= [false true] (mapv #(boolean (:classical %)) registers)))
      (is (= [:h :reset :barrier :measure] (mapv :component gates)))
      (is (= ["h q[0];"
              "reset q[1];"
              "barrier q[0], q[1];"
              "measure q[0] -> c[1];"]
             (body-lines state)))))

  (testing "the assignment form of measure is also understood"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[1] q;\n"
                      "bit[1] c;\n"
                      "measure q[0] -> c[0];\n"))
          measure (first (filter #(= :measure (:component %)) (:items state)))]
      (is (some? measure))
      (is (= ["measure q[0] -> c[0];"] (body-lines state))))))

(deftest qasm2-import-test
  (testing "OpenQASM 2 register declarations and include files are imported"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 2.0;\n"
                      "include \"qelib1.inc\";\n"
                      "qreg q[2];\n"
                      "creg c[2];\n"
                      "h q[0];\n"
                      "measure q[0] -> c[0];\n"))]
      (is (= ["q" "c"] (mapv :name (filter #(= :register (:type %))
                                          (:items state)))))
      (is (= ["qelib1.inc"] (:includes state)))
      (is (some #(= :measure (:component %)) (:items state)))
      (is (str/includes? (qasm/to-qasm state) "include \"qelib1.inc\";")))))

(deftest raw-statement-test
  (testing "unsupported control flow statements are preserved verbatim"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[1] q;\n"
                      "bit[1] c;\n"
                      "measure q[0] -> c[0];\n"
                      "if (c == 1) x q[0];\n"))
          text (qasm/to-qasm state)]
      (is (= ["if (c == 1) x q[0]"] (:raw-statements state)))
      (is (str/includes? text "if (c == 1) x q[0];")))))

(deftest legacy-targets-and-annotations-test
  (testing "gates stored with legacy targets export one call per target"
    (let [state (circuit [(register 1 "q" 2 0 0)
                          (assoc (gate 2 :rx 100 0)
                                 :targets {1 #{0 1}})]
                         [(wire 1 1 2)])
          text (qasm/to-qasm state)]
      (is (str/includes? text "rx(pi/2) q[0];"))
      (is (str/includes? text "rx(pi/2) q[1];"))))

  (testing "annotations are exported as comments"
    (let [state (circuit [(register 1 "q" 1 0 0)
                          {:id 2 :type :annotation :component :annotation
                           :text "measure later" :x 100 :y 0}]
                         [])]
      (is (str/includes? (qasm/to-qasm state) "// measure later")))))

(deftest duplicate-register-symbols-test
  (testing "duplicate register names receive unique QASM symbols"
    (let [state (circuit [(register 1 "q" 1 0 0)
                          (register 2 "q" 1 0 100)]
                         [])
          text (qasm/to-qasm state)]
      (is (str/includes? text "qubit[1] q;"))
      (is (str/includes? text "qubit[1] q_2;")))))

(deftest unknown-gate-import-test
  (testing "unknown calls are imported as custom gate statements"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[1] q;\n"
                      "mystery q[0];\n"))
          gate (first (filter #(= :gate (:type %)) (:items state)))]
      (is (= :custom (:component gate)))
      (is (= "mystery q[0];" (:qasm gate)))
      (is (str/includes? (qasm/to-qasm state) "mystery q[0];")))))

(deftest negctrl-import-test
  (testing "negated controls are ignored without adding a broken gate"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[1] q;\n"
                      "negctrl @ x q[0];\n"))]
      (is (empty? (filter #(= :gate (:type %)) (:items state)))))))

(deftest control-flow-block-test
  (testing "block control flow is preserved as a single raw statement per block"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[2] q;\n"
                      "bit[2] c;\n"
                      "for (int i = 0; i < 2; i++) {\n"
                      "  h q[i];\n"
                      "}\n"
                      "while (c[0] == 1) {\n"
                      "  x q[0];\n"
                      "}\n"
                      "switch (c) {\n"
                      "  case 0: x q[0]; break;\n"
                      "  default: h q[1];\n"
                      "}\n"))
          raw (:raw-statements state)
          text (qasm/to-qasm state)]
      (is (= 3 (count raw)))
      (is (str/starts-with? (first raw) "for (int i = 0; i < 2; i++)"))
      (is (str/ends-with? (first raw) "}"))
      (is (str/includes? text "for (int i = 0; i < 2; i++) {"))
      (is (str/includes? text "  h q[i];\n}"))
      (is (str/includes? text "while (c[0] == 1) {"))
      (is (str/includes? text "switch (c) {"))
      (is (not (str/includes? text "};\n")))))

  (testing "if/else blocks keep their trailing semicolons inside the block"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[1] q;\n"
                      "bit[1] c;\n"
                      "if (c == 1) { x q[0]; } else { h q[0]; }\n"))
          raw (:raw-statements state)
          text (qasm/to-qasm state)]
      (is (= 2 (count raw)))
      (is (= "if (c == 1) { x q[0]; }" (first raw)))
      (is (= "else { h q[0]; }" (second raw)))
      (is (str/includes? text "if (c == 1) { x q[0]; }"))
      (is (str/includes? text "else { h q[0]; }")))))

(deftest opaque-definition-test
  (testing "opaque declarations are imported as custom gates with their arity"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "opaque my_gate a, b;\n"
                      "qubit[2] q;\n"
                      "my_gate q[0], q[1];\n"))
          gates (filter #(= :gate (:type %)) (:items state))
          custom (first gates)
          text (qasm/to-qasm state)]
      (is (= 1 (count gates)))
      (is (= :custom (:component custom)))
      (is (= 2 (:qubits custom)))
      (is (= "opaque my_gate a, b;" (:qasm custom)))
      (is (str/includes? text "opaque my_gate a, b;"))
      (is (str/includes? text "my_gate q[0], q[1];"))))

  (testing "opaque declarations with parameters keep their signature"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "opaque pulse(theta) a;\n"
                      "qubit[1] q;\n"
                      "pulse(pi/2) q[0];\n"))
          custom (first (filter #(= :gate (:type %)) (:items state)))]
      (is (= :custom (:component custom)))
      (is (= "opaque pulse(theta) a;" (:qasm custom)))
      (is (= 1 (:qubits custom)))))
)

(deftest export-edge-cases-test
  (testing "custom gates without usable declarations are handled"
    (let [state (circuit [(register 1 "q" 2 0 0)
                          (custom-gate 2 2 "" 100 0)
                          (custom-gate 3 2 "// no declaration" 200 0)
                          (assoc (custom-gate 4 2 "gate pair a, b { h a; }" 300 0)
                                 :qasm nil)]
                         [])]
      (is (some #(str/includes? % "no QASM definition") (lines state)))
      (is (some #(= "// no declaration" %) (lines state)))))

  (testing "a named custom gate with no wired operands is omitted"
    (let [state (circuit [(register 1 "q" 1 0 0)
                          (custom-gate 2 1 "gate idle a { }" 100 0)]
                         [])
          body (body-lines state)]
      (is (not-any? #(str/starts-with? % "idle ") body))))

  (testing "custom arity can be empty or parameterised"
    (let [empty-arity (circuit [(register 1 "q" 1 0 0)
                                (custom-gate 2 1 "gate zed() { }" 100 0)]
                               [(port-wire 1 1 2 0 0)])
          parameterised (circuit [(register 1 "q" 1 0 0)
                                  (custom-gate 2 1 "gate zed(theta) a { }" 100 0)]
                                 [(port-wire 1 1 2 0 0)])]
      (is (some #(= "zed q[0];" %) (body-lines empty-arity)))
      (is (some #(= "zed q[0];" %) (body-lines parameterised)))))

  (testing "measurement and barrier edge cases are reported or omitted"
    (let [state (circuit [(register 1 "q" 1 0 0)
                          (classical-register 2 "c" 1 0 100)
                          (gate 3 :measure 100 0)
                          (gate 4 :barrier 200 0)
                          (gate 5 :reset 300 0)]
                         [(port-wire 1 1 3 0 0)])]
      (is (some #(str/includes? % "no classical target") (body-lines state)))
      (is (not-any? #(str/starts-with? % "barrier ") (body-lines state)))
      (is (not-any? #(str/starts-with? % "reset ") (body-lines state)))))

  (testing "gates with no ports or registers use their fallbacks"
    (let [state (circuit [{:id 1 :type :gate :component :mystery :x 0 :y 0}]
                         [])]
      (is (some #(str/includes? % "Unconnected mystery gate omitted") (lines state))))
    (let [state (circuit [(register 1 "q" 2 0 0)
                          (gate 2 :h 100 0)]
                         [])]
      (is (some #(str/includes? % "Unconnected H gate omitted")
                (body-lines state))))))

(deftest import-edge-cases-test
  (testing "malformed definitions are skipped without crashing"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "// gate comment\n"
                      "gate { h q; }\n"
                      "opaque broken a\n"
                      "gate no_brace a;\n"
                      "qubit[1] q;\n"
                      "h q[0];\n"))]
      (is (= [:h] (mapv :component (filter #(= :gate (:type %)) (:items state)))))))

  (testing "definition keywords inside identifiers are ignored"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[1] q;\n"
                      "gateway q[0];\n"))]
      (is (= [:custom] (mapv :component (filter #(= :gate (:type %)) (:items state)))))))

  (testing "strings, escapes, nested braces, and blank statements are parsed"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[1] q;\n"
                      "bit[1] c;\n"
                      "if (c[0] == 1) { x q[0]; }\n"
                      "else { h q[0]; }\n"
                      "// a comment with ; and }\n"
                      "x q[0];\n"))]
      (is (= 2 (count (:raw-statements state))))
      (is (str/includes? (qasm/to-qasm state) "x q[0];"))))

  (testing "invalid and partial operands are ignored"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[1] q;\n"
                      "bit[1] c;\n"
                      "h q[9];\n"
                      "reset q[9];\n"
                      "barrier q[9];\n"
                      "measure q[9] -> c[0];\n"
                      "measure q[0];\n"
                      "rx(pi/2);\n"
                      "x;\n"))]
      (is (empty? (filter #(= :gate (:type %)) (:items state))))))

  (testing "undefined registers and negative indices are ignored"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[1] q;\n"
                      "h missing[0];\n"
                      "reset missing[0];\n"
                      "barrier missing[0];\n"
                      "measure missing[0] -> missing[0];\n"))]
      (is (empty? (filter #(= :gate (:type %)) (:items state))))))

  (testing "register declarations accept both syntaxes and single wires"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 2.0;\n"
                      "qreg q;\n"
                      "creg c;\n"
                      "qreg r[0];\n"
                      "creg d[0];\n"))]
      (is (= ["q" "c"] (mapv :name (filter #(= :register (:type %)) (:items state)))))
      (is (= [false true] (mapv #(boolean (:classical %))
                                (filter #(= :register (:type %)) (:items state)))))))

  (testing "control modifiers on custom and unknown gates are preserved"
    (let [state (qasm/from-qasm
                 (str "OPENQASM 3.0;\n"
                      "qubit[2] q;\n"
                      "ctrl @ mystery q[0], q[1];\n"
                      "ctrl @ rx(pi/4) q[0];\n"))]
      (is (= 1 (count (filter #(= :custom (:component %))
                              (filter #(= :gate (:type %)) (:items state))))))
      (is (some #(= "pi/4" (:angle %)) (:items state))))))
