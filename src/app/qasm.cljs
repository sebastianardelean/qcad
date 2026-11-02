(ns app.qasm
  (:require [clojure.string :as str]
            [app.components :as components]
            [app.model :as model]))

(def ^:private gate-map
  {:h "h"
   :x "x"
   :y "y"
   :z "z"
   :s "s"
   :t "t"
   :sdag "sdg"
   :tdag "tdg"
   :rx "rx"
   :ry "ry"
   :rz "rz"
   :p "p"
   :i "id"})

(def ^:private parameterized-gates #{:rx :ry :rz :p})

(def ^:private default-angle "pi/2")

(def ^:private reserved-identifiers
  (into (set (vals gate-map))
        ["OPENQASM" "include" "def" "defcal" "defcalgrammar" "gate" "opaque"
         "qubit" "bit" "int" "uint" "float" "angle" "bool" "duration"
         "stretch" "complex" "array" "void" "const" "readonly" "mutable"
         "input" "output" "return" "if" "else" "for" "while" "switch"
         "case" "default" "break" "continue" "end" "reset" "measure"
         "barrier" "delay" "box" "let" "alias" "pragma" "pi" "tau" "euler"
         "inv" "ctrl" "negctrl" "pow" "gphase"]))

(defn- base-identifier [name]
  (let [text (str/trim (str name))
        replaced (str/replace text #"[^A-Za-z0-9_]" "_")
        candidate (if (str/blank? replaced) "q" replaced)
        candidate (if (re-find #"^[A-Za-z_]" candidate)
                    candidate
                    (str "q_" candidate))]
    (if (contains? reserved-identifiers candidate)
      (str "q_" candidate)
      candidate)))

(defn- unique-symbol [base used]
  (loop [n 2]
    (let [candidate (str base "_" n)]
      (if (contains? used candidate)
        (recur (inc n))
        candidate))))

(defn- assign-symbols [registers]
  (second
   (reduce (fn [[used symbols] reg]
             (let [base (base-identifier (:name reg))
                   symbol (if (contains? used base)
                            (unique-symbol base used)
                            base)]
               [(conj used symbol) (assoc symbols (:id reg) symbol)]))
           [#{} {}]
           registers)))

(defn- order-by-position [item]
  [(or (:x item) 0) (or (:y item) 0) (or (:id item) 0)])

(defn- registers-by-position [items]
  (->> items
       (filter #(= :register (:type %)))
       (sort-by (juxt #(or (:y %) 0) #(or (:x %) 0) #(or (:id %) 0)))))

(defn- gates-by-position [items]
  (->> items
       (filter #(= :gate (:type %)))
       (sort-by order-by-position)))

(defn- targeted-operands [gate registers symbols]
  (vec (for [register registers
             :let [register-id (:id register)
                   qubits (sort (get (:targets gate) register-id #{}))]
             qubit qubits
             :when (and (integer? qubit)
                        (<= 0 qubit)
                        (< qubit (:qubits register)))]
         (str (get symbols register-id) "[" qubit "]"))))

(defn- gate-label [gate]
  (or (:label (components/by-id (:component gate)))
      (some-> (:component gate) name)
      "Unknown"))

(defn- gate-name [gate]
  (or (get gate-map (:component gate))
      (some-> (:component gate) name)
      "unknown"))

(defn- gate-call [gate operands]
  (let [parameter (when (contains? parameterized-gates (:component gate))
                    (str "(" default-angle ")"))]
    (str (gate-name gate) parameter " " (str/join ", " operands) ";")))

(defn- gate-lines [gate registers symbols]
  (if (some? (:targets gate))
    (let [operands (targeted-operands gate
                                      (filter #(contains? (:targets gate) (:id %))
                                              registers)
                                      symbols)]
      (if (seq operands)
        (mapv #(gate-call gate [%]) operands)
        [(str "// " (gate-label gate) " gate has no selected qubits.")]))
    (mapv #(gate-call gate [(get symbols (:id %))]) registers)))

(defn to-qasm [state]
  (let [items (:items state)
        registers (registers-by-position items)
        symbols (assign-symbols registers)
        rows (map (fn [gate]
                    [gate (model/connected-registers state (:id gate))])
                  (gates-by-position items))
        declarations (mapv (fn [reg]
                             (str "qubit[" (:qubits reg) "] " (get symbols (:id reg)) ";"))
                           registers)
        calls (vec (mapcat (fn [[gate connected]]
                             (gate-lines gate connected symbols))
                           rows))
        omitted (vec (keep (fn [[gate connected]]
                             (when (empty? connected)
                               (str "// Unconnected " (gate-label gate) " gate omitted.")))
                           rows))
        body (concat declarations
                     (when (and (seq declarations) (seq calls)) [""])
                     calls
                     (when (and (seq omitted) (or (seq declarations) (seq calls))) [""])
                     omitted)
        lines (concat ["OPENQASM 3.0;" "include \"stdgates.inc\";"]
                      (when (seq body) [""])
                      body)]
    (str (str/join "\n" lines) "\n")))
