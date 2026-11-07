(ns app.persist
  (:require [clojure.string :as str]
            [app.model :as model]))

(def autosave-key "qcad.autosave")

(defn- max-id [items]
  (reduce max 0 (keep :id items)))

(defn- max-wire-id [wires]
  (reduce max 0 (keep :id wires)))

(defn document [state]
  {:version 1
   :items (:items state)
   :wires (:wires state)
   :metadata (:metadata state)
   :includes (:includes state)
   :raw-statements (:raw-statements state)
   :next-id (:next-id state)
   :next-wire-id (:next-wire-id state)})

(defn to-json [state]
  (js/JSON.stringify (clj->js (document state)) nil 2))

(defn- keyword-value [value]
  (if (string? value) (keyword value) value))

(defn- normalize-port [port]
  (if (map? port)
    (update port :kind keyword-value)
    port))

(defn- normalize-item [item]
  (if (map? item)
    (cond-> (-> item
                (update :type keyword-value)
                (update :component keyword-value))
      (contains? item :from-port) (update :from-port normalize-port)
      (contains? item :to-port) (update :to-port normalize-port))
    item))

(defn- normalize-wire [wire]
  (if (map? wire)
    (cond-> wire
      (contains? wire :from-port) (update :from-port normalize-port)
      (contains? wire :to-port) (update :to-port normalize-port))
    wire))

(defn from-json [text]
  (let [data (js->clj (js/JSON.parse (str text)) :keywordize-keys true)
        items (mapv normalize-item (:items data))
        wires (mapv normalize-wire (:wires data))
        next-id (max 1 (or (:next-id data) 1) (inc (max-id items)))
        next-wire-id (max 1 (or (:next-wire-id data) 1) (inc (max-wire-id wires)))]
    (merge model/initial-state
           {:items items
            :wires wires
            :metadata (merge (:metadata model/initial-state)
                             (:metadata data))
            :includes (or (:includes data) (:includes model/initial-state))
            :raw-statements (vec (or (:raw-statements data) []))
            :next-id next-id
            :next-wire-id next-wire-id})))

(defn snapshot->json [snapshot]
  (js/JSON.stringify (clj->js snapshot) nil 2))

(defn- valid-item? [item]
  (and (map? item)
       (integer? (:id item))
       (keyword? (:type item))))

(defn- valid-wire? [wire]
  (and (map? wire)
       (integer? (:id wire))
       (integer? (:from wire))
       (integer? (:to wire))))

(defn snapshot-from-json [text]
  (try
    (let [data (js->clj (js/JSON.parse (str text)) :keywordize-keys true)
          items (when (sequential? (:items data))
                  (mapv normalize-item (:items data)))
          wires (when (sequential? (:wires data))
                  (mapv normalize-wire (:wires data)))
          origin (:origin data)]
      (when (and (map? data)
                 (sequential? items)
                 (sequential? wires)
                 (every? valid-item? items)
                 (every? valid-wire? wires)
                 (or (nil? origin)
                     (and (sequential? origin)
                          (= 2 (count origin))
                          (every? number? origin))))
        {:items (vec items)
         :wires (vec wires)
         :origin (if (sequential? origin) (vec origin) [0 0])}))
    (catch :default _ nil)))

(defn- storage []
  (try
    (when (and (exists? js/localStorage) js/localStorage)
      js/localStorage)
    (catch :default _ nil)))

(defn save-autosave! [state]
  (when-let [storage (storage)]
    (try
      (.setItem storage autosave-key (to-json state))
      true
      (catch :default _ false))))

(defn load-autosave []
  (when-let [storage (storage)]
    (try
      (when-let [text (.getItem storage autosave-key)]
        (from-json text))
      (catch :default _ nil))))

(defn clear-autosave! []
  (when-let [storage (storage)]
    (try
      (.removeItem storage autosave-key)
      true
      (catch :default _ false))))

(defn- describe-item [item]
  (str (name (:type item))
       " " (:id item)
       (when-let [component (:component item)] (str " " (name component)))
       (when (:name item) (str " " (:name item)))))

(defn- describe-wire [wire]
  (str "wire " (:from wire) " -> " (:to wire)))

(defn- changed-keys [before after]
  (->> (into (set (keys before)) (keys after))
       (remove #{:id})
       (filter #(not= (get before %) (get after %)))
       (sort)
       vec))

(defn- item-changes [before after]
  (let [before-items (into {} (map (juxt :id identity) (:items before)))
        after-items (into {} (map (juxt :id identity) (:items after)))]
    (concat
     (for [[id item] before-items
           :when (not (contains? after-items id))]
       (str "- " (describe-item item)))
     (for [[id item] after-items
           :when (not (contains? before-items id))]
       (str "+ " (describe-item item)))
     (for [[id before-item] before-items
           :let [after-item (get after-items id)]
           :when (and after-item (not= (dissoc before-item :id)
                                       (dissoc after-item :id)))]
       (str "~ " (describe-item after-item)
            " (" (str/join ", " (map name (changed-keys before-item after-item)))
            ")")))))

(defn- wire-changes [before after]
  (let [before-wires (into {} (map (juxt :id identity) (:wires before)))
        after-wires (into {} (map (juxt :id identity) (:wires after)))]
    (concat
     (for [[id wire] before-wires
           :when (not (contains? after-wires id))]
       (str "- " (describe-wire wire)))
     (for [[id wire] after-wires
           :when (not (contains? before-wires id))]
       (str "+ " (describe-wire wire)))
     (for [[id before-wire] before-wires
           :let [after-wire (get after-wires id)]
           :when (and after-wire (not= (dissoc before-wire :id)
                                       (dissoc after-wire :id)))]
       (str "~ " (describe-wire after-wire)
            " (" (str/join ", " (map name (changed-keys before-wire after-wire)))
            ")")))))

(defn diff-summary [before after]
  (let [changes (vec (sort (concat (item-changes before after)
                                   (wire-changes before after))))]
    (if (seq changes)
      changes
      ["No differences."])))
