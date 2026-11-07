(ns app.library-test
  (:require [clojure.string :as str]
            [cljs.test :refer [deftest is testing use-fixtures]]
            [app.library :as library]
            [app.model :as model]
            [app.qasm :as qasm]))

(defonce ^:private storage-data (atom {}))

(defn- fake-storage []
  #js {:getItem (fn [key] (get @storage-data key))
       :setItem (fn [key value] (swap! storage-data assoc key (str value)))
       :removeItem (fn [key] (swap! storage-data dissoc key))
       :clear (fn [] (reset! storage-data {}))})

(set! (.-localStorage js/globalThis) (fake-storage))

(use-fixtures :each (fn [test-fn]
                      (reset! storage-data {})
                      (when-let [storage (.-localStorage js/globalThis)]
                        (when (.-clear storage) (.clear storage)))
                      (test-fn)))

(deftest templates-test
  (testing "all built-in templates are registered"
    (is (= [:bell :ghz :qft :grover :teleportation]
           (mapv :id library/templates)))
    (is (= "Bell state" (:name (library/by-id :bell))))
    (is (nil? (library/by-id :missing))))

  (testing "every template builds a circuit"
    (doseq [{:keys [id]} library/templates]
      (let [circuit (library/build id)]
        (is (seq (:items circuit)) (str id " has items"))
        (is (vector? (:wires circuit)) (str id " has wires")))))

  (testing "templates produce valid OpenQASM without omitted gates"
    (doseq [{:keys [id]} library/templates]
      (let [state (merge model/initial-state (library/build id))
            text (qasm/to-qasm state)]
        (is (str/includes? text "OPENQASM 3.0;") (str id " is QASM"))
        (is (not (str/includes? text "Unconnected")) (str id " has no omissions"))
        (is (not (str/includes? text "no connected qubits"))
            (str id " has no unwired gates"))))))

(deftest insert-template-test
  (testing "a template is pasted with fresh identifiers and an offset"
    (let [state (library/insert-template model/initial-state :bell)
          items (:items state)
          wires (:wires state)]
      (is (= 3 (count items)))
      (is (= [1 2 3] (mapv :id items)))
      (is (= [1 2 3] (mapv :id wires)))
      (is (= 60 (:x (first items))))
      (is (= 260 (:x (second items))))
      (is (= #{1 2 3} (:selected state)))
      (is (= 4 (:next-id state)))
      (is (= 4 (:next-wire-id state)))))

  (testing "an unknown template leaves the state untouched"
    (is (= model/initial-state
           (library/insert-template model/initial-state :missing)))))

(deftest user-library-test
  (let [state (-> model/initial-state
                  (model/add-item {:id 1 :type :gate :component :h :x 0 :y 0}))]
    (testing "saving and listing user circuits"
      (let [entries (library/save-user-circuit! "demo" state)]
        (is (= ["demo"] (mapv :name entries)))
        (is (= ["demo"] (mapv :name (library/user-circuits))))))

    (testing "saving under an existing name replaces the entry"
      (library/save-user-circuit! "demo" state)
      (is (= 1 (count (library/user-circuits)))))

    (testing "a saved circuit is inserted into the current circuit"
      (let [inserted (library/insert-user-circuit model/initial-state "demo")]
        (is (= 1 (count (:items inserted))))
        (is (= 2 (:next-id inserted)))))

    (testing "an unknown name is ignored"
      (is (= model/initial-state
             (library/insert-user-circuit model/initial-state "missing"))))

    (testing "deleting a circuit removes it"
      (library/delete-user-circuit! "demo")
      (is (empty? (library/user-circuits)))
      (is (empty? (library/delete-user-circuit! "missing"))))))

(deftest corrupt-library-test
  (let [state (-> model/initial-state
                  (model/add-item {:id 1 :type :gate :component :h :x 0 :y 0}))]
    (testing "corrupt stored data is treated as an empty library"
      (.setItem js/localStorage library/library-key "not json")
      (is (empty? (library/user-circuits))))

    (testing "corrupt entries are skipped when inserting"
      (.setItem js/localStorage library/library-key
                (js/JSON.stringify
                 (clj->js [{:name "broken" :json "not json"}])))
      (is (= model/initial-state
             (library/insert-user-circuit model/initial-state "broken"))))

    (testing "the library works without local storage"
      (let [storage (.-localStorage js/globalThis)]
        (set! (.-localStorage js/globalThis) nil)
        (try
          (is (empty? (library/user-circuits)))
          (is (= ["saved"] (mapv :name (library/save-user-circuit! "saved" state))))
          (finally
            (set! (.-localStorage js/globalThis) storage)))))))

(deftest insert-block-test
  (testing "a template block wraps the pasted circuit in a labelled box"
    (let [state (library/insert-block-template model/initial-state :bell 100 200)
          items (:items state)
          box (last items)]
      (is (= 4 (count items)))
      (is (= :box (:type box)))
      (is (= "Bell state" (:text box)))
      (is (= [100 200] ((juxt :x :y) box)))
      (is (= 444 (:width box)))
      (is (= library/min-block-height (:height box)))
      (is (= 120 (:x (first items))))
      (is (= 220 (:y (first items))))
      (is (= 3 (count (:wires state))))
      (is (= 5 (:next-id state)))))

  (testing "a user circuit block wraps the stored circuit"
    (let [saved (-> model/initial-state
                    (model/add-item {:id 1 :type :gate :component :h :x 0 :y 0}))
          _ (library/save-user-circuit! "saved block" saved)
          state (library/insert-block-user model/initial-state "saved block" 50 60)
          items (:items state)]
      (is (= 2 (count items)))
      (is (= :box (:type (last items))))
      (is (= "saved block" (:text (last items))))
      (is (= [70 80] ((juxt :x :y) (first items))))))

  (testing "unknown block sources leave the state untouched"
    (is (= model/initial-state
           (library/insert-block-template model/initial-state :missing 0 0)))
    (is (= model/initial-state
           (library/insert-block-user model/initial-state "missing" 0 0)))))
