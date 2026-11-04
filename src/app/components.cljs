(ns app.components)

(def components
  [{:id :quantum-register :label "Quantum register" :kind :register}
   {:id :h :label "H" :kind :gate}
   {:id :x :label "X" :kind :gate}
   {:id :y :label "Y" :kind :gate}
   {:id :z :label "Z" :kind :gate}
   {:id :s :label "S" :kind :gate}
   {:id :t :label "T" :kind :gate}
   {:id :sdag :label "Sdag" :kind :gate}
   {:id :tdag :label "Tdag" :kind :gate}
   {:id :rx :label "Rx" :kind :gate}
   {:id :ry :label "Ry" :kind :gate}
   {:id :rz :label "Rz" :kind :gate}
   {:id :p :label "P" :kind :gate}
   {:id :i :label "I" :kind :gate}
   {:id :custom :label "Custom" :kind :custom}])

(defn by-id [id]
  (first (filter #(= id (:id %)) components)))
