(ns vijual.graphical
  (:use [vijual :only (shapes-height shapes-width center wrap layout-tree idtree graph-to-shapes layout-graph half tree-to-shapes)]
        clojure.contrib.math)
  (:import (java.io File)
           (javax.imageio ImageIO)
           (java.awt Color)
           (java.awt.image BufferedImage)
           (vijual.*)))

;;Maintained By Conrad Barski- Licensed under GPLV3

(def image-wrap-threshold 20)

(def image-dim {:width-fn (fn [text]
                            (* (+ (min image-wrap-threshold (count text)) 4) 7))
                :height-fn (fn [text]
                             (* (+ (count (wrap text image-wrap-threshold)) 2) 9))
                :wrap-fn (fn 
                           ([text]
                              (vec (map #(apply str " " %) (wrap text image-wrap-threshold))))
                           ([text width height]
                              (vec (map #(apply str " " %) (center (wrap text (/ (- width 4) 7)) (/ (- width 4) 7) (/ (- height 3) 9))))))
                :node-padding 10
                :row-padding 80
                :line-wid 3
                :line-padding 10})

(defn save-image 
  "This is just a convenience function for the examples- Saves a bitmap to a file."  
  [img name]
  (let [file (new File (str name ".png"))]
    (ImageIO/write (cast java.awt.image.BufferedImage img) "png" file)))

(def arrow-size 5)

(defn draw-shapes-image
  "Draws a list of shapes to an awt image."
  [{:keys [line-wid]} shapes]
  (let [img (new BufferedImage (shapes-width shapes) (shapes-height shapes)(. BufferedImage TYPE_4BYTE_ABGR))
        graphics (. img createGraphics)]
    (doseq [{:keys [type x y width height text]} shapes]
      (when (= type :rect)
        (when (= height line-wid)
          (.setColor graphics (Color. 255 255 255))
          (.fillRect graphics (+ x line-wid) (- y line-wid) (- width (* 2 line-wid)) (* line-wid 3)))
        (when (= width line-wid)
          (.setColor graphics (Color. 255 255 255))
          (.fillRect graphics (- x line-wid) (+ y line-wid) (* line-wid 3) (- height (* 2 line-wid))))
        (.setColor graphics (Color. 0 0 0))
        (.fillRect graphics x y width height)
        (.setColor graphics (Color. 240 240 240))
        (.fillRect graphics (+ x line-wid) (+ y line-wid) (- width (* line-wid 2)) (- height (* line-wid 2)))
        (.setColor graphics (Color. 0 0 0))
        (doseq [[s n] (map vector (seq text) (iterate inc 0))]
          (.drawString graphics s (floor (+ 2 x)) (floor (+ y 8 (* n 10)))))))
    (doseq [{:keys [type x y width height dir]} shapes]
      (when (= type :arrow)
        (.setColor graphics (Color. 255 255 255))
        (.fillRect graphics x y width height)
        (.setColor graphics (Color. 0 0 0))
        (condp = dir
          :left (.fillPolygon graphics (int-array [x (+ x arrow-size) (+ x arrow-size)]) (int-array [(+ y (half height)) (- (+ y (half height)) arrow-size) (+ y (half height) arrow-size)]) 3)
          :right (.fillPolygon graphics (int-array [(+ x width) (- (+ x width) arrow-size) (- (+ x width) arrow-size)]) (int-array [(+ y (half height)) (- (+ y (half height)) arrow-size) (+ y (half height) arrow-size)]) 3)
          :up (.fillPolygon graphics (int-array [(inc (+ x (half width))) (- (+ x (half width)) arrow-size) (+ x (half width) arrow-size 1)]) (int-array [(dec y) (+ y arrow-size) (+ y arrow-size)]) 3)
          :down (.fillPolygon graphics (int-array [(+ x (half width)) (- (+ x (half width)) arrow-size) (+ x (half width) arrow-size)]) (int-array [(+ y height) (- (+ y height) arrow-size) (- (+ y height) arrow-size)]) 3))))
    img))

(defn draw-tree-image
  "Draws a tree to a java image."
  [tree]
  (draw-shapes-image image-dim (tree-to-shapes image-dim (layout-tree image-dim (idtree tree)))))

(defn draw-graph-image
  "Draws an undirected graph to a java image. Requires a list of pairs representing the edges of the graph. Additionally, a separate map can be included containing node information mapped via the node ids in the edge list."
  ([edges nodes]
     (draw-shapes-image image-dim (graph-to-shapes image-dim (layout-graph image-dim edges nodes false))))
  ([edges]
     (draw-graph-image edges {})))

(defn draw-directed-graph-image
  "Draws an directed graph to a java image. Requires a list of pairs representing the edges of the graph. Additionally, a separate map can be included containing node information mapped via the node ids in the edge list."
  ([edges nodes]
     (draw-shapes-image image-dim (graph-to-shapes image-dim (layout-graph image-dim edges nodes true))))
  ([edges]
     (draw-directed-graph-image edges {})))

;(-> (draw-tree-image [[:north-america [:usa [:miami] [:seattle] [:idaho [:boise]]]] [:europe [:germany] [:france [:paris] [:lyon] [:cannes]]]])
;    (save-image "example"))

;(-> (draw-tree-image [["Eubacteria" ["Aquificae"] ["Nitrospira"] ["Proteobacteria"] ["Chlamydiae"] ["Actinobacteria"]] ["Eukaryotes" ["Archaeplastida" ["Green Plants" ["Prasinophytes"] ["Chlorophyceae"] ["Trebouxiophyceae"] ["Ulvophyceae"] ["Streptohyta" ["Zygnematales"] ["Charales"] ["Embryophytes (land plants)"]]] ["Rhodophyta"] ["Glaucophytes"]] ["Unikots" ["Opisthokonts" ["Animals" ["Bilateria" ["Ecdysozoa" ["Nematoda"] ["Arthropoda"]] ["Lophotrochozoa"] ["Deuterostoma" ["Echinodermata"] ["Hemichordata"] ["Chordata" ["Urochordata"] ["Cephalochordata"] ["Yonnanozoon"] ["Craniata"]]]] ["Cnidaria"] ["Porifera"]] ["Choanoflagellates"] ["Filasterea"] ["Ichthyosporea"] ["Fungi"] ["Nucleariidae"]]] ["Chromalveolates" ["Rhizaria" ["Cercozoa"] ["Foraminifera"] ["Radiolaria"]] ["Alveolates"] ["Stramenopiles"] ["Hacrobia"]] ["Excavates" ["Malawimonads"] ["Discicristates" ["Euglenozoa"] ["Heterolobosea"]] ["Fornicata"]]]])
;    (save-image "example"))

;(-> (draw-graph-image [[:a :b] [:b :c] [:c :d] [:a :d] [:e :f] [:a :f] [:g :e] [:d :e]])
;    (save-image "example"))

;(-> (draw-graph-image [[:a :b] [:b :c] [:c :d] [:a :d] [:e :f] [:a :f] [:g :e] [:d :e]] {:a "Upper Floor" :b "Lower Floor" :c "Garden" :d "Stairway" :e "Front Yard" :f "Basement" :g "Walk Way"})
;    (save-image "example"))

;(-> (draw-graph-image [[:to-cellar :living-room] [:living-room :kitchen] [:from-studio :kitchen] [:kitchen :attic] [:kitchen :behind-house] [:north-of-house :behind-house] [:north-of-house :west-of-house] [:west-of-house :south-of-house] [:south-of-house :behind-house] [:behind-house :clearing] [:clearing :forest1] [:forest1 :forest-path] [:forest-path :north-of-house] [:forest1 :thinned-out-forest] [:forest-path :up-a-tree] [:forest-path :clearing-with-grating] [:clearing-with-grating :grating-room] [:clearing-with-grating :forest-with-sun] [:forest-with-sun :forest2] [:forest-with-sun :forest-path] [:south-of-house :forest2] [:west-of-house :stone-barrow] [:stone-barrow :inside-barrow] [:clearing :canyon-view] [:forest2 :clearing] [:forest2 :canyon-view] [:canyon-view :rocky-ledge] [:rocky-ledge :canyon-bottom] [:canyon-bottom :end-of-rainbow] [:end-of-rainbow :on-the-rainbow] [:on-the-rainbow :aragain-falls]])
;    (save-image "example"))

;(-> (draw-directed-graph-image [[:a :b] [:b :c] [:c :d] [:d :e] [:e :f] [:f :a]])
;    (save-image "example"))

;(-> (draw-directed-graph-image [["AFn" "Obj"] ["AFn" "IFn"] ["AFn" "Serializable"] ["AFunction" "AFn"] ["AFunction" "Comparator"] ["AFunction" "Fn"] ["AMapEntry" "APersistentVector"] ["AMapEntry" "IMapEntry"] ["APersistentMap" "AFn"] ["APersistentMap" "IPersistentMap"] ["APersistentMap" "Map"] ["APersistentSet" "AFn"] ["APersistentSet" "IPersistentSet"] ["APersistentSet" "Collection"] ["APersistentSet" "Set"] ["APersistentVector" "AFn"] ["APersistentVector" "IPersistentVector"] ["APersistentVector" "List"] ["APersistentVector" "RandomAccess"] ["ARef" "AReference"] ["ARef" "IRef"] ["AReference" "IReference"] ["ASeq" "Obj"] ["ASeq" "ISeq"] ["ASeq" "List"] ["Agent" "ARef"] ["ArraySeq" "ASeq"] ["ArraySeq" "IndexedSeq"] ["ArraySeq" "IReduce"] ["ArrayStream" "AFn"] ["Associative" "IPersistentCollection"] ["Atom" "ARef"] ["ConcurrentMap" "Map"] ["Cons" "ASeq"] ["Delay" "IDeref"] ["EnumerationSeq" "ASeq"] ["IFn" "Callable"] ["IFn" "Runnable"] ["IMapEntry" "Map$Entry"] ["IPersistentCollection" "Seqable"] ["IPersistentList" "Sequential"] ["IPersistentList" "IPersistentStack"] ["IPersistentMap" "Associative"] ["IPersistentSet" "IPersistentCollection"] ["IPersistentStack" "IPersistentCollection"] ["IPersistentVector" "Associative"] ["IPersistentVector" "Sequential"] ["IPersistentVector" "IPersistentStack"] ["IRef" "IDeref"] ["ISeq" "IPersistentCollection"] ["ISeq" "Sequential"] ["IndexedSeq" "ISeq"] ["IteratorSeq" "ASeq"] ["IteratorStream" "AFn"] ["Keyword" "IFn"] ["LazilyPersistentVector" "APersistentVector"] ["LazySeq" "Obj"] ["LazySeq" "ISeq"] ["LazySeq" "List"] ["List" "Collection"] ["MapEntry" "AMapEntry"] ["MultiFn" "AFn"] ["Namespace" "AReference"] ["Obj" "IObj"] ["Object$Future$IDeref" "IProxy"] ["Object$Future$IDeref" "IDeref"] ["Object$Future$IDeref" "Future"] ["PersistentArrayMap" "APersistentMap"] ["PersistentHashMap" "APersistentMap"] ["PersistentHashSet" "APersistentSet"] ["PersistentList" "ASeq"] ["PersistentList" "IPersistentList"] ["PersistentList" "IReduce"] ["PersistentList" "List"] ["PersistentQueue" "Obj"] ["PersistentQueue" "IPersistentList"] ["PersistentQueue" "Collection"] ["PersistentStructMap" "APersistentMap"] ["PersistentTreeMap" "APersistentMap"] ["PersistentTreeMap" "Sorted"] ["PersistentTreeSet" "APersistentSet"] ["PersistentTreeSet" "Sorted"] ["PersistentVector" "APersistentVector"] ["ProxyHandler" "InvocationHandler"] ["Range" "ASeq"] ["Range" "IReduce"] ["Ref" "ARef"] ["Ref" "IFn"] ["Ref" "IRef"] ["RestFn" "AFunction"] ["SeqEnumeration" "Enumeration"] ["SeqIterator" "Iterator"] ["Set" "Collection"] ["Stream" "Seqable"] ["Stream" "Sequential"] ["StringSeq" "ASeq"] ["StringSeq" "IndexedSeq"] ["Symbol" "AFn"] ["Symbol" "Serializable"] ["TransactionalHashMap" "ConcurrentMap"] ["Var" "ARef"] ["Var" "IFn"] ["Var" "IRef"] ["Var" "Settable"]])
;    (save-image "example"))
