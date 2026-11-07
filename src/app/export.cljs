(ns app.export
  (:require [clojure.string :as str]
            [app.components :as components]
            [app.model :as model]))

(def ^:private gate-size [44 36])
(def ^:private register-size [160 36])
(def ^:private padding 24)

(defn- xml-escape [text]
  (-> (str text)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&apos;")))

(defn- item-size [{:keys [type width height]}]
  (case type
    :register register-size
    :annotation [140 30]
    :box [220 140]
    (or (when (and width height) [width height]) gate-size)))

(defn- item-rect [{:keys [x y width height] :as item}]
  (let [[default-width default-height] (item-size item)
        width (or width default-width)
        height (or height default-height)]
    [x y (+ x width) (+ y height)]))

(defn- port-anchor [item kind index]
  (let [[x y x2 y2] (item-rect item)
        width (- x2 x)
        height (- y2 y)
        count (max 1 (model/port-count item kind))
        px (if (= :output kind) x2 x)
        py (+ y (* height (/ (inc index) (inc count))))]
    [px py]))

(defn- item-anchor [item]
  (let [[x y x2 y2] (item-rect item)]
    [(/ (+ x x2) 2) (/ (+ y y2) 2)]))

(defn- wire-anchor [item port]
  (if port
    (port-anchor item (:kind port) (:index port))
    (item-anchor item)))

(defn- wire-points [items-by-id wire]
  (let [from (get items-by-id (:from wire))
        to (get items-by-id (:to wire))]
    (when (and from to)
      (model/orthogonal-path
       (into [(wire-anchor from (:from-port wire))]
             (concat (:points wire)
                     [(wire-anchor to (:to-port wire))]))))))

(defn- points-attr [points]
  (str/join " " (map (fn [[x y]] (str x "," y)) points)))

(defn- item-label [item]
  (case (:type item)
      :register (str (:name item)
                     " (" (:qubits item) " "
                     (if (model/classical-register? item)
                       (if (= 1 (:qubits item)) "bit" "bits")
                       (if (= 1 (:qubits item)) "qubit" "qubits"))
                     ")")
      :annotation (:text item)
      :box (:text item)
      :gate (let [component (components/by-id (:component item))
                  label (or (:label component)
                            (some-> (:component item) name)
                            "Gate")
                  angle (when (and (:parameterized component) (:angle item))
                          (str "(" (:angle item) ")"))]
              (str label angle))
      ""))

(defn- render-wire [items-by-id wire]
  (when-let [points (wire-points items-by-id wire)]
    (let [from (get items-by-id (:from wire))
          to (get items-by-id (:to wire))
          classical? (or (model/classical-register? from)
                         (model/classical-register? to))]
      (str "<polyline class=\"canvas-wire"
           (when classical? " canvas-wire-classical")
           "\" points=\"" (points-attr points)
           "\" fill=\"none\" stroke=\""
           (if classical? "#805ad5" "#2b6cb0")
           "\" stroke-width=\"2\""
           (when classical? " stroke-dasharray=\"6 4\"")
           "/>"))))

(defn- control-markers [item]
  (let [controls (model/gate-control-count item)
        [tx ty] (item-anchor item)]
    (when (pos? controls)
      (apply str
             (for [index (range controls)]
               (let [[cx cy] (port-anchor item :input index)]
                 (str "<line class=\"canvas-control-line\" x1=\"" cx
                      "\" y1=\"" cy "\" x2=\"" tx "\" y2=\"" ty
                      "\" stroke=\"#1f2933\" stroke-width=\"2\"/>"
                      "<circle cx=\"" cx "\" cy=\"" cy
                      "\" r=\"3\" fill=\"#1f2933\"/>")))))))

(defn- render-item [item]
  (let [[x y x2 y2] (item-rect item)
        width (- x2 x)
        height (- y2 y)
        label (xml-escape (item-label item))
        rotation (or (:rotation item) 0)
        transform (when (pos? rotation)
                    (str " transform=\"rotate(" rotation " " (+ x (/ width 2))
                         " " (+ y (/ height 2)) ")\""))]
    (case (:type item)
      :gate
      (str "<g" transform ">"
           "<rect x=\"" x "\" y=\"" y "\" width=\"" width "\" height=\"" height
           "\" rx=\"4\" fill=\"#ffffff\" stroke=\"#486581\"/>"
           (or (control-markers item) "")
           "<text x=\"" (+ x (/ width 2)) "\" y=\"" (+ y (/ height 2) 4)
           "\" text-anchor=\"middle\" font-family=\"sans-serif\" font-size=\"12\""
           " font-weight=\"600\" fill=\"#1f2933\">" label "</text>"
           "</g>")

      :register
      (str "<g" transform ">"
           "<rect x=\"" x "\" y=\"" y "\" width=\"" width "\" height=\"" height
           "\" rx=\"5\" fill=\"#ffffff\" stroke=\""
           (if (model/classical-register? item) "#805ad5" "#2f855a")
           "\" stroke-width=\"2\""
           (when (model/classical-register? item) " stroke-dasharray=\"7 4\"")
           "/>"
           "<text x=\"" (+ x (/ width 2)) "\" y=\"" (+ y (/ height 2) 4)
           "\" text-anchor=\"middle\" font-family=\"sans-serif\" font-size=\"12\""
           " font-weight=\"600\" fill=\"#1f2933\">" label "</text>"
           "</g>")

      :box
      (str "<g" transform ">"
           "<rect x=\"" x "\" y=\"" y "\" width=\"" width "\" height=\"" height
           "\" rx=\"8\" fill=\"rgba(128,90,213,0.08)\" stroke=\"#805ad5\""
           " stroke-dasharray=\"6 4\"/>"
           "<text x=\"" (+ x 8) "\" y=\"" (+ y 18)
           "\" font-family=\"sans-serif\" font-size=\"12\" fill=\"#52606d\">"
           label "</text>"
           "</g>")

      :annotation
      (str "<text x=\"" x "\" y=\"" (+ y 18)
           "\" font-family=\"sans-serif\" font-size=\"14\" fill=\"#1f2933\">"
           label "</text>")

      "")))

(defn- content-bounds [items wires]
  (let [rects (map item-rect items)
        wire-points (mapcat (fn [wire]
                              (or (map (fn [[x y]] [x y x y])
                                       (wire-points (into {} (map (juxt :id identity) items))
                                                    wire))
                                  []))
                            wires)
        xs (concat (map first rects) (map #(nth % 2) rects)
                   (map first wire-points) (map #(nth % 2) wire-points))
        ys (concat (map second rects) (map #(nth % 3) rects)
                   (map second wire-points) (map #(nth % 3) wire-points))]
    (if (seq xs)
      [(min (apply min xs) 0) (min (apply min ys) 0)
       (max (apply max xs) 1) (max (apply max ys) 1)]
      [0 0 100 100])))

(defn- grid-defs []
  (str "<defs><pattern id=\"qcad-grid\" width=\"20\" height=\"20\""
       " patternUnits=\"userSpaceOnUse\">"
       "<path d=\"M 20 0 L 0 0 0 20\" fill=\"none\" stroke=\"#e1e7ec\""
       " stroke-width=\"1\"/></pattern></defs>\n"))

(defn to-svg
  ([state] (to-svg state {}))
  ([state {:keys [transparent include-ui]
           :or {transparent false include-ui false}}]
   (let [items (:items state)
         wires (:wires state)
         items-by-id (into {} (map (juxt :id identity)) items)
         [x1 y1 x2 y2] (content-bounds items wires)
         width (max 1 (+ (- x2 x1) (* 2 padding)))
         height (max 1 (+ (- y2 y1) (* 2 padding)))
         view-box (str (- x1 padding) " " (- y1 padding) " " width " " height)
         background-x (- x1 padding)
         background-y (- y1 padding)]
     (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" width
          "\" height=\"" height "\" viewBox=\"" view-box "\">\n"
          (when include-ui (grid-defs))
          (when-not transparent
            (str "<rect x=\"" background-x "\" y=\"" background-y
                 "\" width=\"" width "\" height=\"" height
                 "\" fill=\"#ffffff\"/>\n"))
          (when include-ui
            (str "<rect class=\"export-grid\" x=\"" background-x
                 "\" y=\"" background-y "\" width=\"" width
                 "\" height=\"" height "\" fill=\"url(#qcad-grid)\"/>\n"))
          (str/join "\n" (keep #(render-wire items-by-id %) wires))
          "\n"
          (str/join "\n" (map render-item items))
          "\n</svg>\n"))))

(defn to-data-url [svg]
  (str "data:image/svg+xml;charset=utf-8,"
       (js/encodeURIComponent svg)))
