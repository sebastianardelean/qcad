
(ns app.core)

(defn greet []
  (let [output (.getElementById js/document "output")]
    (set! (.-textContent output)
          "Hello from ClojureScript!")))

(defn init []
  (let [button (.getElementById js/document "greet-button")]
    (.addEventListener button "click" greet)))

(init)
