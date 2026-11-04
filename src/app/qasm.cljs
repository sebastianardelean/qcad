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

(defn- custom-gate? [gate]
  (= :custom (:component gate)))

(defn- custom-qasm [gate]
  (str/trim (str (:qasm gate))))

(defn- custom-gate-name [qasm]
  (some-> (re-find #"(?m)^\s*gate\s+([A-Za-z_][A-Za-z0-9_]*)" (str qasm))
          second))

(defn- custom-gate-arity [qasm]
  (when-let [[_ arguments]
             (re-find #"(?m)^\s*gate\s+[A-Za-z_][A-Za-z0-9_]*\s*(?:\([^)]*\))?\s*([^{]*)\{"
                      (str qasm))]
    (let [arguments (str/trim arguments)]
      (if (str/blank? arguments)
        0
        (count (str/split arguments #","))))))

(defn- gate-name [gate]
  (if (custom-gate? gate)
    (or (custom-gate-name (custom-qasm gate))
        "custom")
    (or (get gate-map (:component gate))
        (some-> (:component gate) name)
        "unknown")))

(defn- gate-call [gate operands]
  (let [parameter (when (contains? parameterized-gates (:component gate))
                    (str "(" default-angle ")"))]
    (str (gate-name gate) parameter " " (str/join ", " operands) ";")))

(declare wire-operands)

(defn- custom-gate-lines [state gate symbols]
  (let [qasm (custom-qasm gate)
        name (custom-gate-name qasm)
        operands (wire-operands state gate symbols)
        actual (count operands)
        declaration-arity (custom-gate-arity qasm)
        expected (if (and (integer? declaration-arity) (pos? declaration-arity))
                   declaration-arity
                   (model/gate-port-count gate))]
    (cond
      (and name (= actual expected))
      [(str name " " (str/join ", " operands) ";")]

      (and name (pos? actual))
      [(str "// Incomplete custom gate " name ": " actual " of " expected
            " qubits connected.")]

      name
      []

      (seq qasm)
      [qasm]

      :else
      [(str "// " (gate-label gate) " gate has no QASM definition.")])))

(defn- target-operand [symbols [register-id qubit]]
  (when-let [symbol (get symbols register-id)]
    (str symbol "[" qubit "]")))

(defn- wire-operands [state gate symbols]
  (->> (model/gate-targets state (:id gate))
       (keep #(target-operand symbols %))
       vec))

(defn- gate-lines [state gate registers symbols]
  (if (custom-gate? gate)
    (custom-gate-lines state gate symbols)
    (let [operands (wire-operands state gate symbols)]
      (cond
        (seq operands)
        (mapv #(gate-call gate [%]) operands)

        (model/gate-port-wired? state (:id gate))
        [(str "// " (gate-label gate) " gate has no connected qubits.")]

        (some? (:targets gate))
        (let [operands (targeted-operands gate
                                          (filter #(contains? (:targets gate) (:id %))
                                                  registers)
                                          symbols)]
          (if (seq operands)
            (mapv #(gate-call gate [%]) operands)
            [(str "// " (gate-label gate) " gate has no selected qubits.")]))

        :else
        (mapv #(gate-call gate [(get symbols (:id %))]) registers)))))

(defn- custom-definitions [items]
  (->> items
       (filter custom-gate?)
       (keep (fn [gate]
               (let [text (custom-qasm gate)]
                 (when (custom-gate-name text)
                   text))))
       distinct
       vec))

(def ^:private qasm-gate-map
  (into {} (map (fn [[component name]] [name component]) gate-map)))

(def ^:private non-gate-statements
  #{"OPENQASM" "include" "gate" "qubit" "bit" "int" "uint" "float"
    "angle" "bool" "duration" "stretch" "complex" "array" "void" "const"
    "readonly" "mutable" "input" "output" "return" "if" "else" "for"
    "while" "switch" "case" "default" "break" "continue" "end" "reset"
    "measure" "barrier" "delay" "box" "let" "alias" "pragma" "def"
    "defcal" "defcalgrammar" "opaque"})

(defn- strip-comments [text]
  (-> (str text)
      (str/replace #"(?s)/\*.*?\*/" " ")
      (str/replace #"//[^\n]*" "")))

(defn- identifier-char? [character]
  (boolean (re-matches #"[A-Za-z0-9_]" (str character))))

(defn- whitespace? [character]
  (boolean (re-matches #"\s" (str character))))

(defn- find-gate-start [text from]
  (loop [start from]
    (let [index (.indexOf text "gate" start)]
      (if (neg? index)
        nil
        (let [before (when (pos? index) (.charAt text (dec index)))
              after-index (+ index 4)
              after (when (< after-index (count text))
                      (.charAt text after-index))]
          (if (and (or (nil? before) (not (identifier-char? before)))
                   (some? after)
                   (whitespace? after))
            index
            (recur (inc index))))))))

(defn- matching-brace-index [text open-index]
  (loop [index open-index
         depth 0]
    (if (< index (count text))
      (let [character (.charAt text index)
            depth (cond
                    (= character "{") (inc depth)
                    (= character "}") (dec depth)
                    :else depth)]
        (if (and (= character "}") (zero? depth))
          index
          (recur (inc index) depth)))
      -1)))

(defn- definition-at [text start]
  (let [after (.substring text (+ start 4))
        header (re-find #"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*(?:\([^)]*\))?\s*[^;{]*"
                        after)
        name (second header)
        open-relative (.indexOf after "{")]
    (when (and name (not (neg? open-relative)))
      (let [open-index (+ start 4 open-relative)
            close-index (matching-brace-index text open-index)]
        (when-not (neg? close-index)
          (let [definition (str/trim (.substring text start (inc close-index)))]
            {:name name
             :text definition
             :arity (custom-gate-arity definition)
             :start start
             :end (inc close-index)}))))))

(defn- blank-range [text start end]
  (str (.substring text 0 start)
       (apply str (repeat (- end start) " "))
       (.substring text end)))

(defn- extract-definitions [text]
  (loop [from 0
         definitions []]
    (if-let [start (find-gate-start text from)]
      (if-let [definition (definition-at text start)]
        (recur (:end definition) (conj definitions definition))
        (recur (inc start) definitions))
      {:definitions definitions
       :text (reduce (fn [remaining {:keys [start end]}]
                       (blank-range remaining start end))
                     text
                     (reverse definitions))})))

(defn- statements [text]
  (->> (str/split text #";")
       (map str/trim)
       (remove str/blank?)))

(defn- parse-register [statement]
  (or (when-let [[_ qubits name]
                 (re-find #"^qubit\s*\[\s*(\d+)\s*\]\s+([A-Za-z_][A-Za-z0-9_]*)$"
                          statement)]
        (let [count (js/parseInt qubits 10)]
          (when (pos? count)
            {:name name :qubits count})))
      (when-let [[_ name] (re-find #"^qubit\s+([A-Za-z_][A-Za-z0-9_]*)$"
                                   statement)]
        {:name name :qubits 1})))

(defn- parse-operand [text]
  (let [text (str/trim text)]
    (or (when-let [[_ name qubit]
                   (re-find #"^([A-Za-z_][A-Za-z0-9_]*)\s*\[\s*(\d+)\s*\]$"
                            text)]
          {:name name :qubit (js/parseInt qubit 10)})
        (when (re-matches #"^[A-Za-z_][A-Za-z0-9_]*$" text)
          {:name text :all true}))))

(defn- parse-operands [text]
  (if (str/blank? (str text))
    []
    (mapv parse-operand (str/split text #","))))

(defn- parse-call [statement]
  (when-let [[_ name _ operands]
             (re-find #"^([A-Za-z_][A-Za-z0-9_]*)\s*(?:\(([^)]*)\))?\s*(.*)$"
                      statement)]
    (when-not (contains? non-gate-statements name)
      {:name name
       :operands (parse-operands operands)
       :text (str statement ";")})))

(defn- expand-operand [registers {:keys [name qubit all]}]
  (when-let [register (get registers name)]
    (if all
      (mapv (fn [index] [(:id register) index])
            (range (:qubits register)))
      (when (and (integer? qubit)
                 (<= 0 qubit)
                 (< qubit (:qubits register)))
        [[(:id register) qubit]]))))

(defn- imported-gate [call definitions]
  (let [name (:name call)
        definition (get definitions name)
        component (get qasm-gate-map name)]
    (cond
      definition
      {:component :custom :qasm (:text definition)}

      component
      {:component component}

      :else
      {:component :custom :qasm (:text call)})))

(defn- add-register-items [state registers]
  (reduce (fn [state [index register]]
            (model/add-item state
                            {:id (:id register)
                             :type :register
                             :component :quantum-register
                             :name (:name register)
                             :qubits (:qubits register)
                             :x 40
                             :y (+ 40 (* index 120))}))
          state
          (map-indexed vector registers)))

(defn- connect-targets [state sources gate-id targets]
  (reduce (fn [[state sources] [input-index target]]
            (if-let [[source-id source-index] (get sources target)]
              [(model/add-wire state
                               source-id
                               gate-id
                               nil
                               {:kind :output :index source-index}
                               {:kind :input :index input-index})
               (assoc sources target [gate-id input-index])]
              [state sources]))
          [state sources]
          (map-indexed vector targets)))

(defn- import-calls [state calls registers definitions]
  (let [registers-by-id (into {} (map (juxt :id identity) registers))
        registers-by-name (into {} (map (juxt :name identity) registers))
        initial-sources (into {}
                              (for [register registers
                                    qubit (range (:qubits register))]
                                [[(:id register) qubit]
                                 [(:id register) qubit]]))]
    (first
     (reduce (fn [[state sources] [index call]]
               (let [targets (vec (mapcat #(expand-operand registers-by-name %)
                                          (:operands call)))]
                 (if (seq targets)
                   (let [gate-id (:next-id state)
                         y (apply min
                                  (map #(get-in registers-by-id [(first %) :y])
                                       targets))
                         item (assoc (imported-gate call definitions)
                                     :id gate-id
                                     :type :gate
                                     :qubits (count targets)
                                     :x (+ 260 (* index 140))
                                     :y y)
                         state (model/add-item state item)]
                     (connect-targets state sources gate-id targets))
                   [state sources])))
             [state initial-sources]
             (map-indexed vector calls)))))

(defn from-qasm [text]
  (let [{:keys [definitions text]} (extract-definitions (strip-comments text))
        statements (vec (statements text))
        registers (->> (keep parse-register statements)
                       vec
                       (map-indexed (fn [index register]
                                      (assoc register :id (inc index)))))
        calls (vec (keep parse-call statements))
        definitions (into {} (map (juxt :name identity) definitions))
        state (add-register-items model/initial-state registers)]
    (import-calls state calls registers definitions)))

(defn to-qasm [state]
  (let [items (:items state)
        registers (registers-by-position items)
        symbols (assign-symbols registers)
        definitions (custom-definitions items)
        definition-lines (vec (mapcat str/split-lines definitions))
        rows (map (fn [gate]
                    [gate (model/connected-registers state (:id gate))])
                  (gates-by-position items))
        declarations (mapv (fn [reg]
                             (str "qubit[" (:qubits reg) "] " (get symbols (:id reg)) ";"))
                           registers)
        calls (vec (mapcat (fn [[gate connected]]
                             (gate-lines state gate connected symbols))
                           rows))
        omitted (vec (keep (fn [[gate connected]]
                             (when (and (empty? connected)
                                        (nil? (:targets gate))
                                        (not (model/gate-port-wired? state (:id gate))))
                               (str "// Unconnected " (gate-label gate) " gate omitted.")))
                           rows))
        body (concat definition-lines
                     (when (and (seq definition-lines) (seq declarations)) [""])
                     declarations
                     (when (and (seq declarations) (seq calls)) [""])
                     calls
                     (when (and (seq omitted)
                                (or (seq definition-lines)
                                    (seq declarations)
                                    (seq calls)))
                       [""])
                     omitted)
        lines (concat ["OPENQASM 3.0;" "include \"stdgates.inc\";"]
                      (when (seq body) [""])
                      body)]
    (str (str/join "\n" lines) "\n")))
