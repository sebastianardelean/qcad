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
   :i "id"
   :cx "cx"
   :cz "cz"
   :swap "swap"
   :ccx "ccx"
   :measure "measure"
   :reset "reset"
   :barrier "barrier"})

(def ^:private parameterized-gates #{:rx :ry :rz :p})

(def ^:private multi-qubit-gates #{:cx :cz :swap :ccx})

(def ^:private default-angle model/default-angle)

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
  (some-> (re-find #"(?m)^\s*(?:gate|opaque)\s+([A-Za-z_][A-Za-z0-9_]*)" (str qasm))
          second))

(defn- custom-gate-arity [qasm]
  (when-let [[_ arguments]
             (re-find #"(?m)^\s*(?:gate|opaque)\s+[A-Za-z_][A-Za-z0-9_]*\s*(?:\([^)]*\))?\s*([^;{]*)(?:\{|;)"
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

(defn- gate-parameter [gate]
  (when (contains? parameterized-gates (:component gate))
    (str "(" (or (:angle gate) default-angle) ")")))

(defn- base-gate-call [gate operands]
  (str (gate-name gate) (gate-parameter gate) " "
       (str/join ", " operands) ";"))

(defn- gate-call [gate operands]
  (let [controls (model/gate-control-count gate)]
    (if (pos? controls)
      (str (apply str (repeat controls "ctrl @ "))
           (base-gate-call gate operands))
      (base-gate-call gate operands))))

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

(defn- measure-lines [state gate symbols]
  (let [[quantum classical] (model/measure-targets state (:id gate))
        quantum-operand (target-operand symbols quantum)
        classical-operand (target-operand symbols classical)]
    (cond
      (and quantum-operand classical-operand)
      [(str "measure " quantum-operand " -> " classical-operand ";")]

      quantum-operand
      [(str "// Measure gate has no classical target.")]

      :else
      [(str "// " (gate-label gate) " gate has no connected qubits.")])))

(defn- barrier-lines [state gate symbols]
  (let [operands (wire-operands state gate symbols)]
    (if (seq operands)
      [(str "barrier " (str/join ", " operands) ";")]
      [])))

(defn- reset-lines [state gate symbols]
  (let [operands (wire-operands state gate symbols)]
    (mapv #(str "reset " % ";") operands)))

(defn- gate-lines [state gate registers symbols]
  (let [component (:component gate)]
    (cond
      (custom-gate? gate)
      (custom-gate-lines state gate symbols)

      (= :measure component)
      (measure-lines state gate symbols)

      (= :barrier component)
      (barrier-lines state gate symbols)

      (= :reset component)
      (reset-lines state gate symbols)

      :else
      (let [operands (wire-operands state gate symbols)
            controlled? (pos? (model/gate-control-count gate))]
        (cond
          (and (seq operands)
               (or (contains? multi-qubit-gates component) controlled?))
          [(gate-call gate operands)]

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
          (mapv #(gate-call gate [(get symbols (:id %))]) registers))))))

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

(defn- find-keyword-start [text keyword from]
  (loop [start from]
    (let [index (.indexOf text keyword start)]
      (if (neg? index)
        nil
        (let [before (when (pos? index) (.charAt text (dec index)))
              after-index (+ index (count keyword))
              after (when (< after-index (count text))
                      (.charAt text after-index))]
          (if (and (or (nil? before) (not (identifier-char? before)))
                   (some? after)
                   (whitespace? after))
            index
            (recur (inc index))))))))

(defn- find-definition-start [text from]
  (let [gate (find-keyword-start text "gate" from)
        opaque (find-keyword-start text "opaque" from)]
    (cond
      (and gate opaque) (min gate opaque)
      gate gate
      opaque opaque
      :else nil)))

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
  (let [keyword (if (.startsWith (.substring text start) "opaque")
                  "opaque"
                  "gate")
        after (.substring text (+ start (count keyword)))
        header (re-find #"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*(?:\([^)]*\))?\s*[^;{]*"
                        after)
        name (second header)]
    (when name
      (if (= keyword "opaque")
        (let [semicolon-relative (.indexOf after ";")]
          (when (not (neg? semicolon-relative))
            (let [end (+ start (count keyword) semicolon-relative 1)
                  definition (str/trim (.substring text start end))]
              {:name name
               :text definition
               :arity (custom-gate-arity definition)
               :start start
               :end end})))
        (let [open-relative (.indexOf after "{")]
          (when (not (neg? open-relative))
            (let [open-index (+ start (count keyword) open-relative)
                  close-index (matching-brace-index text open-index)]
              (when-not (neg? close-index)
                (let [definition (str/trim (.substring text start (inc close-index)))]
                  {:name name
                   :text definition
                   :arity (custom-gate-arity definition)
                   :start start
                   :end (inc close-index)})))))))))

(defn- blank-range [text start end]
  (str (.substring text 0 start)
       (apply str (repeat (- end start) " "))
       (.substring text end)))

(defn- extract-definitions [text]
  (loop [from 0
         definitions []]
    (if-let [start (find-definition-start text from)]
      (if-let [definition (definition-at text start)]
        (recur (:end definition) (conj definitions definition))
        (recur (inc start) definitions))
      {:definitions definitions
       :text (reduce (fn [remaining {:keys [start end]}]
                       (blank-range remaining start end))
                     text
                     (reverse definitions))})))

(defn- statements [text]
  (let [text (str text)
        length (count text)]
    (loop [index 0
           start 0
           depth 0
           in-string? false
           escaped? false
           result []]
      (if (>= index length)
        (let [statement (str/trim (.substring text start))]
          (if (str/blank? statement) result (conj result statement)))
        (let [character (.charAt text index)]
          (cond
            escaped?
            (recur (inc index) start depth in-string? false result)

            (and in-string? (= character "\\"))
            (recur (inc index) start depth true true result)

            (= character "\"")
            (recur (inc index) start depth (not in-string?) false result)

            in-string?
            (recur (inc index) start depth true false result)

            (contains? {"(" 1 "[" 1 "{" 1} character)
            (recur (inc index) start (inc depth) false false result)

            (contains? {")" 1 "]" 1 "}" 1} character)
            (let [depth (max 0 (dec depth))]
              (if (and (= character "}") (zero? depth))
                (let [statement (str/trim (.substring text start (inc index)))]
                  (recur (inc index) (inc index) 0 false false
                         (if (str/blank? statement)
                           result
                           (conj result statement))))
                (recur (inc index) start depth false false result)))

            (= character ";")
            (if (zero? depth)
              (let [statement (str/trim (.substring text start index))]
                (recur (inc index) (inc index) 0 false false
                       (if (str/blank? statement)
                         result
                         (conj result statement))))
              (recur (inc index) start depth false false result))

            :else
            (recur (inc index) start depth false false result)))))))

(defn- parse-register [statement]
  (or (when-let [[_ qubits name]
                 (re-find #"^(?:qubit|qreg)\s*\[\s*(\d+)\s*\]\s+([A-Za-z_][A-Za-z0-9_]*)$"
                          statement)]
        (let [count (js/parseInt qubits 10)]
          (when (pos? count)
            {:name name :qubits count})))
      (when-let [[_ name qubits]
                 (re-find #"^(?:qreg)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\[\s*(\d+)\s*\]$"
                          statement)]
        (let [count (js/parseInt qubits 10)]
          (when (pos? count)
            {:name name :qubits count})))
      (when-let [[_ qubits name]
                 (re-find #"^(?:bit|creg)\s*\[\s*(\d+)\s*\]\s+([A-Za-z_][A-Za-z0-9_]*)$"
                          statement)]
        (let [count (js/parseInt qubits 10)]
          (when (pos? count)
            {:name name :qubits count :classical true})))
      (when-let [[_ name qubits]
                 (re-find #"^(?:creg)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\[\s*(\d+)\s*\]$"
                          statement)]
        (let [count (js/parseInt qubits 10)]
          (when (pos? count)
            {:name name :qubits count :classical true})))
      (when-let [[_ name] (re-find #"^(?:qubit|qreg)\s+([A-Za-z_][A-Za-z0-9_]*)$"
                                   statement)]
        {:name name :qubits 1})
      (when-let [[_ name] (re-find #"^(?:bit|creg)\s+([A-Za-z_][A-Za-z0-9_]*)$"
                                   statement)]
        {:name name :qubits 1 :classical true})))

(defn- parse-include [statement]
  (some-> (re-find #"^include\s+\"([^\"]+)\"$" statement) second))

(declare parse-operand)

(defn- parse-measure [statement]
  (or (when-let [[_ quantum classical]
                 (re-find #"^measure\s+(.+?)\s*->\s*(.+)$" statement)]
        {:quantum (parse-operand quantum)
         :classical (parse-operand classical)})
      (when-let [[_ classical quantum]
                 (re-find #"^(.+?)\s*=\s*measure\s+(.+)$" statement)]
        {:quantum (parse-operand quantum)
         :classical (parse-operand classical)})))

(defn- parse-metadata [text]
  (let [entries (keep (fn [line]
                        (when-let [[_ key value]
                                   (re-find #"^\s*//\s*(name|author|description)\s*:\s*(.*)$"
                                            line)]
                          [(keyword key) (str/trim value)]))
                      (str/split-lines (str text)))]
    (into {} entries)))

(defn- control-flow-statement? [statement]
  (boolean (re-find #"^(if|else|for|while|switch|case|box|delay|let|pragma)\b"
                    statement)))

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

(defn- strip-controls [statement]
  (loop [text (str/trim statement)
         controls 0]
    (if-let [[_ modifier remaining]
             (re-find #"^(ctrl|negctrl)\s*@\s*(.*)$" text)]
      (if (= modifier "ctrl")
        (recur remaining (inc controls))
        [nil text])
      [controls text])))

(defn- parse-call [statement]
  (when-let [[_ name parameters operands]
             (re-find #"^([A-Za-z_][A-Za-z0-9_]*)\s*(?:\(([^)]*)\))?\s*(.*)$"
                      statement)]
    (when-not (contains? non-gate-statements name)
      {:name name
       :parameters (some-> parameters str/trim not-empty)
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
                            (cond-> {:id (:id register)
                                     :type :register
                                     :component (if (:classical register)
                                                  :classical-register
                                                  :quantum-register)
                                     :name (:name register)
                                     :qubits (:qubits register)
                                     :x 40
                                     :y (+ 40 (* index 120))}
                              (:classical register) (assoc :classical true))))
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

(defn- target-y [registers-by-id targets]
  (apply min (map #(get-in registers-by-id [(first %) :y] 0) targets)))

(defn- gate-x [state]
  (+ 260 (* (- (:next-id state) 1) 140)))

(defn- import-gate [state sources registers-by-id registers-by-name definitions statement]
  (let [[controls base] (strip-controls statement)]
    (when (and (some? controls) (seq base))
      (when-let [call (parse-call base)]
        (let [targets (vec (mapcat #(expand-operand registers-by-name %)
                                   (:operands call)))]
          (if (seq targets)
            (let [gate-id (:next-id state)
                  imported (imported-gate call definitions)
                  item (cond-> (assoc imported
                                      :id gate-id
                                      :type :gate
                                      :qubits (count targets)
                                      :x (gate-x state)
                                      :y (target-y registers-by-id targets))
                         (and (:parameters call)
                              (components/parameterized? (:component imported)))
                         (assoc :angle (:parameters call))
                         (pos? controls)
                         (assoc :controls controls)
                         (and (pos? controls) (= :custom (:component imported)))
                         (assoc :qasm (str statement ";")))
                  state (model/add-item state item)]
              (connect-targets state sources gate-id targets))
            [state sources]))))))

(defn- import-reset [state sources registers-by-id registers-by-name statement]
  (when-let [[_ operands] (re-find #"^reset\s+(.+)$" statement)]
    (let [targets (vec (mapcat #(expand-operand registers-by-name %)
                               (parse-operands operands)))]
      (if (seq targets)
        (let [gate-id (:next-id state)
              item {:id gate-id
                    :type :gate
                    :component :reset
                    :qubits (count targets)
                    :x (gate-x state)
                    :y (target-y registers-by-id targets)}
              state (model/add-item state item)]
          (connect-targets state sources gate-id targets))
        [state sources]))))

(defn- import-barrier [state sources registers-by-id registers-by-name statement]
  (when-let [[_ operands] (re-find #"^barrier\s+(.+)$" statement)]
    (let [targets (vec (mapcat #(expand-operand registers-by-name %)
                               (parse-operands operands)))]
      (if (seq targets)
        (let [gate-id (:next-id state)
              item {:id gate-id
                    :type :gate
                    :component :barrier
                    :qubits (count targets)
                    :x (gate-x state)
                    :y (target-y registers-by-id targets)}
              state (model/add-item state item)]
          (connect-targets state sources gate-id targets))
        [state sources]))))

(defn- import-measure [state sources registers-by-id registers-by-name statement]
  (when-let [{:keys [quantum classical]} (parse-measure statement)]
    (let [quantum-targets (vec (expand-operand registers-by-name quantum))
          classical-targets (vec (expand-operand registers-by-name classical))]
      (if (and (seq quantum-targets) (seq classical-targets))
        (let [[qreg qindex] (first quantum-targets)
              [creg cindex] (first classical-targets)
              measure-id (:next-id state)
              item {:id measure-id
                    :type :gate
                    :component :measure
                    :qubits 1
                    :x (gate-x state)
                    :y (get-in registers-by-id [qreg :y] 0)}
              state (model/add-item state item)
              state (if-let [[source-id source-index] (get sources [qreg qindex])]
                      (model/add-wire state
                                      source-id
                                      measure-id
                                      nil
                                      {:kind :output :index source-index}
                                      {:kind :input :index 0})
                      state)]
          [(model/add-wire state
                           measure-id
                           creg
                           nil
                           {:kind :output :index 0}
                           {:kind :input :index cindex})
           sources])
        [state sources]))))

(defn- declaration-statement? [statement]
  (boolean (or (parse-register statement)
               (parse-include statement)
               (re-find #"^OPENQASM\b" statement))))

(defn- import-statements [state statements registers definitions]
  (let [registers-by-id (into {} (map (juxt :id identity) registers))
        registers-by-name (into {} (map (juxt :name identity) registers))
        initial-sources (into {}
                              (for [register registers
                                    qubit (range (:qubits register))]
                                [[(:id register) qubit]
                                 [(:id register) qubit]]))]
    (first
     (reduce (fn [[state sources] statement]
               (cond
                 (declaration-statement? statement)
                 [state sources]

                 (parse-measure statement)
                 (import-measure state sources registers-by-id
                                 registers-by-name statement)

                 (re-find #"^reset\b" statement)
                 (import-reset state sources registers-by-id
                               registers-by-name statement)

                 (re-find #"^barrier\b" statement)
                 (import-barrier state sources registers-by-id
                                 registers-by-name statement)

                 (parse-call statement)
                 (import-gate state sources registers-by-id registers-by-name
                              definitions statement)

                 :else
                 [state sources]))
             [state initial-sources]
             statements))))

(defn from-qasm [text]
  (let [metadata (parse-metadata text)
        {:keys [definitions text]} (extract-definitions (strip-comments text))
        statements (vec (statements text))
        registers (->> (keep parse-register statements)
                       vec
                       (map-indexed (fn [index register]
                                      (assoc register :id (inc index)))))
        includes (vec (keep parse-include statements))
        raw-statements (vec (filter control-flow-statement? statements))
        definitions (into {} (map (juxt :name identity) definitions))
        state (cond-> (add-register-items model/initial-state registers)
                (seq includes) (assoc :includes includes)
                (seq raw-statements) (assoc :raw-statements raw-statements)
                (seq metadata) (update :metadata merge metadata))]
    (import-statements state statements registers definitions)))

(defn- metadata-lines [state]
  (let [{:keys [name author description]} (:metadata state)]
    (cond-> []
      (not (str/blank? (str name))) (conj (str "// name: " name))
      (not (str/blank? (str author))) (conj (str "// author: " author))
      (not (str/blank? (str description))) (conj (str "// description: " description)))))

(defn- include-lines [state]
  (let [includes (seq (:includes state))]
    (mapv #(str "include \"" % "\";")
          (or includes ["stdgates.inc"]))))

(defn- annotation-lines [items]
  (->> items
       (filter #(= :annotation (:type %)))
       (sort-by order-by-position)
       (map #(str "// " (:text %)))))

(defn- raw-statement-line [statement]
  (let [statement (str/trim (str statement))]
    (cond
      (str/blank? statement) statement
      (str/ends-with? statement ";") statement
      (str/ends-with? statement "}") statement
      :else (str statement ";"))))

(defn to-qasm [state]
  (let [items (:items state)
        registers (registers-by-position items)
        quantum-registers (remove model/classical-register? registers)
        classical-registers (filter model/classical-register? registers)
        symbols (assign-symbols registers)
        definitions (custom-definitions items)
        definition-lines (vec (mapcat str/split-lines definitions))
        rows (map (fn [gate]
                    [gate (model/connected-registers state (:id gate))])
                  (gates-by-position items))
        declarations (vec (concat
                           (map (fn [reg]
                                  (str "qubit[" (:qubits reg) "] "
                                       (get symbols (:id reg)) ";"))
                                quantum-registers)
                           (map (fn [reg]
                                  (str "bit[" (:qubits reg) "] "
                                       (get symbols (:id reg)) ";"))
                                classical-registers)))
        calls (vec (mapcat (fn [[gate connected]]
                             (gate-lines state gate connected symbols))
                           rows))
        raw-lines (mapv raw-statement-line (:raw-statements state))
        annotations (vec (annotation-lines items))
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
                     (when (and (seq raw-lines)
                                (or (seq definition-lines)
                                    (seq declarations)
                                    (seq calls)))
                       [""])
                     raw-lines
                     (when (and (seq annotations)
                                (or (seq definition-lines)
                                    (seq declarations)
                                    (seq calls)
                                    (seq raw-lines)))
                       [""])
                     annotations
                     (when (and (seq omitted)
                                (or (seq definition-lines)
                                    (seq declarations)
                                    (seq calls)
                                    (seq raw-lines)
                                    (seq annotations)))
                       [""])
                     omitted)
        lines (concat (metadata-lines state)
                      ["OPENQASM 3.0;"]
                      (include-lines state)
                      (when (seq body) [""])
                      body)]
    (str (str/join "\n" lines) "\n")))
