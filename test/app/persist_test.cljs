(ns app.persist-test
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [app.model :as model]
            [app.persist :as persist]))

(defonce ^:private storage-data (atom {}))

(defn- fake-storage []
  #js {:getItem (fn [key] (get @storage-data key))
       :setItem (fn [key value] (swap! storage-data assoc key (str value)))
       :removeItem (fn [key] (swap! storage-data dissoc key))
       :clear (fn [] (reset! storage-data {}))})

(set! (.-localStorage js/globalThis) (fake-storage))

(defn- clear-storage! []
  (reset! storage-data {})
  (when-let [storage (.-localStorage js/globalThis)]
    (when (.-clear storage) (.clear storage))))

(use-fixtures :each (fn [test-fn]
                      (clear-storage!)
                      (test-fn)))

(defn- sample-state []
  (-> model/initial-state
      (assoc :items [{:id 1 :type :register :component :quantum-register
                      :name "q" :qubits 2 :x 0 :y 0}
                     {:id 2 :type :gate :component :h :x 120 :y 0}]
             :wires [{:id 1 :from 1 :to 2
                      :from-port {:kind :output :index 0}
                      :to-port {:kind :input :index 0}}]
             :metadata {:name "Bell" :author "Ada" :description "demo"}
             :includes ["stdgates.inc" "custom.inc"]
             :raw-statements ["if (c == 1) x q[0]"]
             :next-id 3
             :next-wire-id 2)))

(deftest document-test
  (testing "the native document records the persistent circuit fields"
    (let [doc (persist/document (sample-state))]
      (is (= 1 (:version doc)))
      (is (= 2 (count (:items doc))))
      (is (= 1 (count (:wires doc))))
      (is (= "Bell" (get-in doc [:metadata :name])))
      (is (= ["stdgates.inc" "custom.inc"] (:includes doc)))
      (is (= ["if (c == 1) x q[0]"] (:raw-statements doc)))
      (is (= 3 (:next-id doc)))
      (is (= 2 (:next-wire-id doc))))))

(deftest json-round-trip-test
  (testing "a document survives a JSON round trip"
    (let [state (sample-state)
          restored (persist/from-json (persist/to-json state))]
      (is (= (:items state) (:items restored)))
      (is (= (:wires state) (:wires restored)))
      (is (= (:metadata state) (:metadata restored)))
      (is (= (:includes state) (:includes restored)))
      (is (= (:raw-statements state) (:raw-statements restored)))
      (is (= (:next-id state) (:next-id restored)))
      (is (= (:next-wire-id state) (:next-wire-id restored)))
      (is (= (model/snapshot state) (model/snapshot restored))))))

(deftest json-repair-test
  (testing "missing identifiers are repaired from the loaded content"
    (let [restored (persist/from-json
                    (str "{\"items\":[{\"id\":7,\"type\":\"gate\","
                         "\"component\":\"h\",\"x\":0,\"y\":0}],"
                         "\"wires\":[{\"id\":9,\"from\":7,\"to\":7}]}"))]
      (is (= 8 (:next-id restored)))
      (is (= 10 (:next-wire-id restored)))
      (is (= (:metadata model/initial-state) (:metadata restored)))
      (is (= (:includes model/initial-state) (:includes restored)))))

  (testing "explicit values smaller than the content are corrected"
    (let [restored (persist/from-json
                    (str "{\"items\":[{\"id\":4,\"type\":\"gate\","
                         "\"component\":\"h\",\"x\":0,\"y\":0}],"
                         "\"wires\":[],\"next-id\":1,\"next-wire-id\":0}"))]
      (is (= 5 (:next-id restored)))
      (is (= 1 (:next-wire-id restored))))))

(deftest autosave-test
  (testing "autosave stores and restores the current document"
    (let [state (sample-state)]
      (is (true? (persist/save-autosave! state)))
      (is (= (model/snapshot state)
             (model/snapshot (persist/load-autosave))))
      (is (true? (persist/clear-autosave!)))
      (is (nil? (persist/load-autosave))))))

(deftest snapshot-json-test
  (testing "selection snapshots serialize to clipboard-friendly JSON"
    (let [snapshot {:items [{:id 1 :type :gate :component :h :x 0 :y 0}]
                    :wires []
                    :origin [10 20]}
          restored (persist/snapshot-from-json (persist/snapshot->json snapshot))]
      (is (= (update snapshot :items vec) restored))))

  (testing "invalid clipboard content is rejected"
    (is (nil? (persist/snapshot-from-json "not json")))
    (is (nil? (persist/snapshot-from-json "{}")))
    (is (nil? (persist/snapshot-from-json
               "{\"items\":[{\"type\":\"gate\"}],\"wires\":[]}")))
    (is (nil? (persist/snapshot-from-json
               "{\"items\":[],\"wires\":[{\"id\":1,\"from\":1}]}"))))

  (testing "an origin defaults to zero when omitted"
    (is (= [0 0]
           (:origin (persist/snapshot-from-json
                     "{\"items\":[],\"wires\":[]}"))))))

(deftest diff-summary-test
  (testing "identical circuits report no differences"
    (is (= ["No differences."]
           (persist/diff-summary (sample-state) (sample-state)))))

  (testing "added and removed items and wires are reported"
    (let [before (-> model/initial-state
                     (assoc :items [{:id 1 :type :gate :component :h :x 0 :y 0}]
                            :wires []))
          after (-> model/initial-state
                    (assoc :items [{:id 2 :type :gate :component :x :x 0 :y 0}]
                           :wires [{:id 1 :from 1 :to 2}]))
          diff (persist/diff-summary before after)]
      (is (some #(= "- gate 1 h" %) diff))
      (is (some #(= "+ gate 2 x" %) diff))
      (is (some #(= "+ wire 1 -> 2" %) diff))))

  (testing "modified items and wires are reported with changed fields"
    (let [before (-> model/initial-state
                     (assoc :items [{:id 1 :type :gate :component :h :x 0 :y 0}]
                            :wires [{:id 1 :from 1 :to 2}]))
          after (-> model/initial-state
                    (assoc :items [{:id 1 :type :gate :component :h :x 40 :y 0}]
                           :wires [{:id 1 :from 1 :to 3}]))
          diff (persist/diff-summary before after)]
      (is (some #(= "~ gate 1 h (x)" %) diff))
      (is (some #(= "~ wire 1 -> 3 (to)" %) diff)))))
