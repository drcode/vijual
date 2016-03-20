(ns vijual.core
  (:require [clojure.math.numeric-tower :refer [floor abs ceil]])
  (:import [java.io File]
           [javax.imageio ImageIO]
           [java.awt Color]
           [java.awt.image BufferedImage]))

;;Maintained By Conrad Barski- Licensed under GPLV3

;; Common functions to all layout algorithms

(defn positions
  "Returns a lazy sequence containing the positions at which pred
  is true for items in coll."
  [pred coll]
  (map first (filter (comp pred second) 
                     (map-indexed vector coll))))

(defn half [x]
  (/ x 2))

(defn fill 
  "Returns a string with the character repeated"
  ([c n]
     (apply str (take n (repeat c))))
  ([c]
     (fill c 1)))

(defn integer-shapes 
  "Takes a sequence of shapes and 'rounds off' all the dimensions so that it can be displayed in ASCII"
  [shapes]
  (map (fn [{:keys [x y width height type] :as item}]
         (let [nu-x (int (floor x))
               nu-y (int (floor y))]
           (assoc item :x nu-x :y nu-y :width (- (int (floor (+ x width))) nu-x) :height (- (int (floor (+ y height))) nu-y))))
       shapes))

(defn rect-relation
  "Calculates whether a rectangle intersects a y position."
  [ypos {:keys [y height]}]
  (let [bottom (dec (+ y height))]
    (cond (or (= ypos y) (= ypos bottom)) :on
          (and (> ypos y) (< ypos bottom)) :in)))

(defn shapes-height [shapes]
  (apply max
         (map (fn [{:keys [y height]}]
                (+ y height))
              shapes)))

(defn shapes-width [shapes]
  (apply max
         (map (fn [{:keys [x width]}]
                (+ x width))
              shapes)))

(defn draw-shapes 
  "Renders a list of shapes into ASCII. The two current shape types are ':rect', which represent nodes and lines between nodes (simply rectangles of minimal width). The arrow heads are represented as type ':arrow'"
  [{:keys [node-padding]} shapes]
  (doseq [ypos (range (shapes-height shapes))]
    (let [xcur (atom 0)
          sorted-shapes (sort (fn [a b]
                                (or (< (a :x) (b :x)) (and (= (a :x) (b :x)) (< (a :width) (b :width))) (and (= (a :x) (b :x)) (= (a :width) (b :width)) (> (a :height) (b :height)))))
                              (filter (fn [shape]
                                        (rect-relation ypos shape))
                                      shapes))]
      (loop [shapes sorted-shapes]
        (when-let [[{:keys [x y width text type dir on-top] :as shape} & more] (seq shapes)]
          (do (when (<= @xcur x)
                (print (fill \space (- x @xcur))))
              (let [s (if (= :on (rect-relation ypos shape))
                        (str (condp = type 
                               :arrow (condp = dir
                                        :right \>
                                        :left \<
                                        :up \^
                                        :down \V
                                        \*)
                               :cap (condp = dir
                                      :right \-
                                      :left \-
                                      :up \|
                                      :down \|)
                               \+)
                             (fill \- (- width 2))
                             (when (> width 1)
                               \+))
                        (str \|
                             (when (> width 1)
                               (let [index (- ypos y 1)
                                     s (if (>= index (count text))
                                         ""
                                         (text index))]
                                 (str s (fill \space (- width (count s) 2)) \|)))))
                    [overlapping s] (let [[x2 width2] (loop [k more]
                                                        (when-let [[{x2 :x width2 :width on-top2 :on-top} & more] (seq k)]
                                                          (when (<= x2 (+ x width))
                                                            (if (and (<= (+ x2 width2) (+ x width)) (or (not on-top) on-top2))
                                                              [x2 width2]
                                                              (recur more)))))]
                                      (if x2
                                        [(< (+ x2 width2) (+ x width)) (apply str (take (- x2 x) s))]
                                        [false s]))]
                (print (if (< x @xcur)
                         (apply str (drop (- @xcur x) s))
                         s))
                (compare-and-set! xcur @xcur (max (+ x (count s)) @xcur))
                (recur (if overlapping
                         (concat [(first more) shape] (next more))
                         more)))))))
    (prn)))

(defn out 
  "like 'print' but without inserted spaces and fully flushed"
  [& more]
  (print (apply str more))
  (flush)
  (first more))

(defn numbered [lst]
  (map vector (iterate inc 0) lst))

(defn label-text 
  "Returns a text string representing the label for the item. It handles the special case of keywords, so that :oak-tree ==> 'oak tree'"
  [text]
     (if (keyword? text)
       (apply str (replace {\- \space} (name text)))
       (str text)))

(defn center [lines width height]
  (vec (let [n (count lines)
             lines (map (fn [s]
                          (apply str (concat (repeat (half (- width (count s))) \space) s)))
                        lines)]
         (if (< n height)
           (concat (repeat (half (- height n)) [""]) lines)
           lines))))

(defn wrap [text width]
  "This function optimally wraps text to fit within a given number of characters, given a monospace font"
  (let [text (letfn [[f [text]
                      (lazy-seq (if (<= (count text) width)
                                  [text]
                                  (let [spc (positions (partial = \space) (take width text))]
                                    (if (seq spc)
                                      (cons (take (last spc) text) (f (drop (inc (last spc)) text)))
                                      (cons (take width text) (f (drop width text)))))))]]
               (map (partial apply str) (f text)))]
    text))

(defn horizontal [dir]
  (#{:right :left} dir))

(defn vertical [dir]
  (#{:up :down} dir))

;;Scan Functions- A "scan" is a run-length-encoded list of heights that are used to optimally calculate packing of tree and graphs. An example scan would be [[0 5] [7 10]] which would mean "The height is 5 for an x between 0 (inclusive) and 7 (exclusive). The height is 10 for any x greater than or equal to 7."

(defn scan-add [scan x y wid]
  "Adds a new height bar at x with a width of wid and a height of y."
  (let [xend (+ x wid)]
    (letfn [[advance [scan cury]
             (if (seq scan)
               (let [[[ax ay :as a] & d] scan]
                 (if (> x ax)
                   (cons a (advance d ay))
                   (cons [x y] (add scan cury))))
               (list [x y] [xend cury]))]
            [add [scan cury]
             (if (seq scan)
               (let [[[ax ay] & d] scan]
                 (if (<= ax xend)
                   (add d ay)
                   (cons [xend cury] scan)))
               (list [xend cury]))]]
      (advance scan nil))))

(defn scan-lowest-y
  "Finds the lowest y that is available at x with width y that would prevent it from intersecting with the scan."
  [scan x width]
  (loop [scan scan
         cury nil
         besty nil]
    (if (seq scan)
      (let [[[ax ay] & d] scan]
        (cond (<= (+ x width) ax) (if besty
                                   (max besty cury)
                                   cury)
              (< x ax) (recur d 
                               ay 
                               (if besty
                                 (max besty ay cury)
                                 (if cury
                                   (max ay cury)
                                   ay)))
              true (recur d ay besty)))
      (if besty
        (max besty cury)
        cury))))

;;This code is specific to ascii rendering

(def ascii-wrap-threshold 10) ;During ascii rendering, text in boxes will wrap after this many characters.

(def ascii-dim {:width-fn (fn [text]
                             (+ (min ascii-wrap-threshold (count text)) 4))
                 :height-fn (fn [text]
                              (+ (count (wrap text ascii-wrap-threshold)) 2))
                 :wrap-fn (fn 
                            ([text]
                               (vec (map #(apply str " " %) (wrap text ascii-wrap-threshold))))
                            ([text width height]
                               (vec (map #(apply str " " %) (center (wrap text (- width 4)) (- width 4) (- height 3))))))
                 :node-padding 1
                 :row-padding 8
                 :line-wid 1
                 :line-padding 1})

;;Functions specific to tree drawing 

(defn tree-to-shapes 
  "Converts a full layed-out tree into a bunch of shapes to be sent to backend rendering code."
  [{:keys [wrap-fn line-wid]} tree]
  (mapcat (fn [{:keys [text x y width height line-left line-right line-ypos leaf parent-line-y]}]
            (concat (when parent-line-y
                      [{:type :rect :x (+ x (half width) (- (half line-wid))) :y parent-line-y :width line-wid :height (inc (- y parent-line-y))}])
                    [{:type :rect :text (wrap-fn text width height) :x x :y y :width width :height height}]
                    (when-not leaf
                      [{:type :rect :x line-left :y line-ypos :width (- line-right line-left) :height line-wid}
                       {:type :rect :x (+ x (half width) (- (half line-wid))) :y (dec (+ y height)) :width line-wid :height (+ (- line-ypos y height) 2)}])))
          tree))

(defn make-rows 
  "Takes a tree and converts it into rows. This is needed since items in the same row and their widths will affect the spacing and layout of items."
  [tree]
  (when (seq tree)
    (cons (map (fn [{:keys [text id parent children]}]
                 {:text text :id id :parent parent :leaf (empty? children)})
               tree)
          (make-rows (mapcat (fn [{:keys [id children]}]
                               (map #(assoc % :parent id) children))
                             tree)))))

(defn wrap-text
  "calculates the wrapped text of each tree item and the resulting height based on how the text was broken into lines"
  [{:keys [wrap-fn height-fn]} rows]
  (map (fn [row]
         (map (fn [{:keys [text] :as item}]
                (assoc item :wrapped-text (wrap-fn text) :height (height-fn text)))
             row))
       rows))

(defn row-pos 
  "Calculates preliminary x positions for nodes in a tree. This will be refined later by the calculations from the 'space' functions."
  [{:keys [width-fn node-padding row-padding height-fn]} row y]
  (let [x (atom 0)]
    (map (fn [{:keys [text] :as item}]
           (let [w (width-fn text)
                 oldx @x]
             (swap! x #(+ % w node-padding))
             (merge item {:x oldx :width w :height (height-fn text)})))
         row)))

(defn space-row
  "This function calculates the x positions of tree nodes. All calculations start from the longest row in the tree. This function is then used to position nodes upwards or downwards from the longest row. Each row is positioned relative a 'target row', which is the neighboring row nearest to the longest row."
  [{:keys [node-padding]} fun total-width target-row row remaining]
  (let [curx (atom 0)
        remaining (atom remaining)]
    (map (fn [{:keys [width id] :as item}]
           (let [children (filter #(fun % item) target-row)
                 child-pos (map (fn [{:keys [x width] :as item}]
                                  (+ x (half width)))
                                children)
                 nu-remaining (- @remaining width node-padding)
                 nu-x (if (seq child-pos)
                        (let [child (first children)
                              siblings (filter #(fun child %) row)
                              left-scoot (if (and (< 1 (count siblings)) (= ((first siblings) :id) id))
                                           (half (- (apply + (map #(+ (% :width) node-padding) siblings)) node-padding (child :width)))
                                           0)
                              k (- (half (+ (first child-pos) (last child-pos))) (half width) left-scoot)
                              nu-right (+ k width node-padding)]
                          (if (< k @curx)
                            @curx
                            (if (< (- total-width nu-right) nu-remaining)
                              (- total-width nu-remaining node-padding width)
                              k)))
                        @curx)]
             (compare-and-set! remaining @remaining nu-remaining)
             (compare-and-set! curx @curx (+ nu-x width node-padding))
             (assoc item :x nu-x)))
         row)))

(defn space
  "Acessory function to space-row that allows a list of rows to by spaced"
  [{:keys [node-padding] :as dim} fun total-width target-row rest]
  (when (seq rest)
    (let [curx (atom 0)
          [[row remaining] & more] rest
          nu-row (space-row dim fun total-width target-row row remaining)]
      (cons nu-row (space dim fun total-width nu-row more)))))

(defn horz-lines [{:keys [line-wid]} rows]
  "This function calculates the left and right extents of the horizontal line below a node that leads to its children."
  (map (fn [cur next]
         (map (fn [{:keys [id] :as cur}]
                (let [bounds (fn [{:keys [x width]}]
                               (+ x (half width)))
                      ranges (cons (bounds cur)
                                   (for [{:keys [parent] :as chi} next :when (= id parent)]
                                     (bounds chi)))]
                  (merge cur {:line-left (- (apply min ranges) (half line-wid))
                              :line-right (+ (apply max ranges) (half line-wid))})))
              cur))
       rows
       (concat (rest rows) [[]])))

(defn level-lines 
  "This function is used to stagger horizontal lines vertically in an optimal fashion, so that nodes can connect to their children in a tree with the least amount of inbetween space."
  [{:keys [line-wid line-padding]} lined]
  (map (fn [row]
         (let [group-right (atom 0)
               line-y (atom 0)]
           (map (fn [{:keys [leaf line-left line-right x width] :as item}]
                  (cond leaf 
                        item
                        (and @group-right (> (+ @group-right line-wid) line-left))
                        (swap! line-y
                               (if (<= (+ x (half width)) (+ @group-right line-padding))
                                 dec
                                 inc))
                        true
                        (compare-and-set! line-y @line-y 0))
                  (swap! group-right (partial max line-right))
                  (assoc item :line-y @line-y))
                row)))
       lined))

(defn lev-children [levlines]
  "Updates children with the level of the horizontal line of their parents."
  (map (fn [cur par]
         (map (fn [item]
                (assoc item 
                  :parent-line-y 
                  (when-let [k (first (filter #(= (% :id) (item :parent)) par))]
                    (k :line-ypos))))
              cur))
       levlines
       (cons [] levlines)))

(defn place-boxes [{:keys [line-padding] :as dim} scan acc row]
  "Places boxes as high as possible during tree packing."
  (if-let [[{:keys [x width height] :as item} & r] (seq row)]
    (let [y (scan-lowest-y scan x width)]
      (recur dim (scan-add scan x (+ y height line-padding) width) (cons (assoc item :y y) acc) r))
    [(reverse acc) scan]))


(defn place-lines [{:keys [line-padding line-wid] :as dim} scan acc row]
  "Places lines as high as possible during tree packing."
  (if (seq row)
    (let [[{:keys [line-left line-right leaf] :as item} & r] row
          line-width (- line-right line-left)
          cury (scan-lowest-y scan line-left line-width)]
      (if leaf
        (recur dim scan (cons item acc) r)
        (recur dim (scan-add scan line-left (+ cury line-wid line-padding) line-width) (cons (assoc item :line-ypos cury) acc) r)))
    [(reverse acc) scan]))

(defn pack-tree 
  "gets rid of extra empty space in a tree by moving up nodes and horizontal lines as much as possible."
  [{:keys [line-padding line-wid] :as dim} rows]
  (letfn [[f [scan rows]
           (when-let [[row & more] (seq rows)]
             (let [[row scan] (place-boxes dim scan nil row)
                   sorted-row (sort-by :line-y row)
                   [lines-placed scan] (place-lines dim scan nil sorted-row)]
               (lazy-seq (cons lines-placed (f scan more)))))]]
    (f [[0 0]] rows)))

(defn idtree 
  "Assigns an id# to each node in a tree so that it can be flattened later on."
  [tree]
  (let [n (atom 0)]
    (letfn [[f [tree]
             (map (fn [[nam & chi]]
                    (swap! n inc)
                    {:text (label-text nam) :id (dec @n) :children (f chi)})
                  tree)]]
      (f tree))))

(defn tree-row-wid 
  "Figures out the width of a row in a tree."
  [{:keys [width-fn node-padding]} row]
  (+ (reduce + 
             (map (fn [{text :text}]
                    (width-fn text))
                  row))
     (* (dec (count row)) node-padding)))

(defn layout-tree [{:keys [row-padding height width width-fn] :as dim} tree]
  "This takes a tree and elegantly arranges it."
  (let [rows (make-rows tree)
        wrapped (wrap-text dim rows)
        total-height (* (count wrapped) row-padding)
        widths (map (partial tree-row-wid dim) wrapped)
        total-width (apply max widths)
        total-height (* (inc (count rows)) row-padding)
        top 0
        left 0
        divider (first (positions (partial = total-width) widths))
        pos (map vector (map (partial row-pos dim) wrapped (iterate inc 0)) widths)
        zipped-top (reverse (take divider pos))
        target-row (first (nth pos divider))
        zipped-bottom (drop (inc divider) pos)
        spaced-top (space dim
                          (fn [{p :parent} {id :id}]
                            (== p id))
                          total-width
                          target-row
                          zipped-top)
        spaced-bottom (space dim
                           (fn [{id :id} {p :parent}]
                             (== id p))
                           total-width
                           target-row
                           zipped-bottom)
        spaced-pos (concat (reverse spaced-top) [target-row] spaced-bottom)
        lined (horz-lines dim spaced-pos)
        leveled-lines (level-lines dim lined)
        packed (pack-tree dim leveled-lines)
        lev-chi (lev-children packed)]
    (apply concat lev-chi)))

;;Functions specific to graphs

(defn graph-to-shapes
  "Converts a graph into a list of shapes which is used by backend ascii or bitmap pipelines to render to a medium."
  [{:keys [line-padding line-wid wrap-fn]} tree]
  (mapcat (fn [{:keys [text xpos ypos width height links]}]
            (cons {:type :rect :text (wrap-fn text width height) :x xpos :y ypos :width width :height height}
                  (mapcat (fn [{:keys [legs dest arrow]}]
                            (let [shapes (map (fn [{x1 :xpos y1 :ypos dir1 :dir} {x2 :xpos y2 :ypos dir2 :dir}]
                                                (if (or (horizontal dir1) (vertical dir2))
                                                  {:type :rect :x (min x1 x2) :y (min y1 y2) :width (+ (abs (- x2 x1)) line-wid) :height line-wid}
                                                  {:type :rect :x (min x1 x2) :y (min y1 y2) :width line-wid :height (+ (abs (- y2 y1)) line-wid)}))
                                              legs
                                              (next legs))
                                  first-leg (first legs)
                                  rev-legs (reverse legs)
                                  last-leg (first rev-legs)]
                              (concat [{:type (if (= arrow :start) 
                                                :arrow 
                                                :cap)
                                        :x (first-leg :xpos) 
                                        :y (first-leg :ypos) 
                                        :width line-wid 
                                        :height line-wid 
                                        :on-top true
                                        :dir (condp = (first-leg :dir)
                                               :right :left
                                               :left :right
                                               :up :down
                                               :down :up)}
                                       {:type (if (= arrow :end)
                                                :arrow 
                                                :cap)
                                        :x (last-leg :xpos) 
                                        :y (last-leg :ypos) 
                                        :width line-wid 
                                        :height line-wid 
                                        :on-top true
                                        :dir ((first (next rev-legs)) :dir)}]
                                      (-> (vec shapes)
                                          (assoc-in [0 :on-top] true)
                                          (assoc-in [(dec (count shapes)) :on-top] true)))))
                          links)))
       tree))

(defn tension [edges pos]
  "Calculates the manhattan distance between all edges in a graph."
  (apply + 
         (map (fn [[f t]]
                (let [{fx :x fy :y} (pos f)
                      {tx :x ty :y} (pos t)]
                  (+ (abs (- fx tx)) (abs (- fy ty)))))
              edges)))

(defn get-side
  "All graphs currently start as logically square. This function therefore calculates length of side with the sqrt funciton."
  [col]
  (int (ceil (Math/sqrt (count col)))))

(defn shuffle-nodes
  "Randomly swaps two nodes of the graph"
  [pos nodes]
  (let [a (rand-nth nodes)
        b (rand-nth nodes)
        an (pos a)
        bn (pos b)]
    (merge pos 
           {a (assoc an :x (bn :x) :y (bn :y))
            b (assoc bn :x (an :x) :y (an :y))})))

(defn anneal 
  "A naive annealing algorithm for initial layout of nodes in graphs. The current code is crude and very prone to getting stuck in local maxima. Best replaced with a genetic algorithm in the future."
  [edges nodemap]
  (let [nodes (distinct (concat (apply concat edges) (keys nodemap)))
        nodenum (count nodes)
        side (get-side nodes)
        pos (into {}
                  (map (fn [s e]
                         [s {:x (mod e side) :y (int (/ e side)) :text (label-text (or (nodemap s) s))}])
                       (shuffle nodes)
                       (range nodenum)))]
    (loop [pos pos
           cur-tension (tension edges pos)
           n (let [nodenum (count nodes)]
               (min 50000 (max 100 (* nodenum nodenum))))]
      (if (zero? n)
        pos
        (let [nu-pos (if (zero? (rand-int 3))
                       (shuffle-nodes pos nodes)
                       (-> pos
                           (shuffle-nodes nodes)
                           (shuffle-nodes nodes)))
              nu-tension (tension edges nu-pos)]
          (if (<= nu-tension cur-tension)
            (recur nu-pos nu-tension (dec n))
            (recur pos cur-tension (dec n))))))))

(defn tension-directed
  "Calculates the total length of all edges, based on manhattan distance. Directional arrows pointing upwards are penalized, so that the graph will have mainly downward arrows."
  [edges pos]
  (apply + 
         (map (fn [[f t]]
                (let [{fx :x fy :y} (pos f)
                      {tx :x ty :y} (pos t)]
                  (* (+ (abs (- fx tx)) (abs (- fy ty)))
                     (cond (> fy ty) 2
                           (= fy ty) 1
                           true 1))))
              edges)))

(defn edge-map
  "Turns a list of edges into a map, keyed on the source of the edge."
  [edges]
  (into {} 
        (map (fn [[a more]]
               [a (map second more)])
             (group-by first edges))))

(defn depth-sort
  "Sorts nodes based on their depth (i.e. does it have children, grandchildren, etc)"
  [nodes edges]
  (let [emap (into {} (group-by second edges))]
    (letfn [[depth [node visited]
             (if (visited node)
               0
               (if-let [e (emap node)]
                 (inc (apply max (map #(depth (first %) (conj visited node)) e)))
                 0))]]
      (map second
           (sort-by first 
                    (map (fn [node]
                           [(depth node #{}) node])
                         nodes))))))

(defn anneal-directed
  "This function anneals a directed graph. The nodes are started off in a depth-sorted way, with the hopes that this will make it more likely that arrows will be predominantly pointing downwards."
  [edges nodemap]
  (let [nodes (distinct (apply concat edges))
        nodes (depth-sort nodes edges)
        nodenum (count nodes)
        side (get-side nodes)
        pos (into {}
                  (map (fn [s e]
                         [s {:x (mod e side) :y (int (/ e side)) :text (label-text (or (nodemap s) s))}])
                       nodes
                       (range nodenum)))]
    (loop [pos pos
           cur-tension (tension-directed edges pos)
           n (let [nodenum (count nodes)]
               (min 50000 (max 100 (* nodenum nodenum))))]
      (if (zero? n)
        pos
        (let [nu-pos (if (zero? (rand-int 3))
                       (shuffle-nodes pos nodes)
                       (-> pos
                           (shuffle-nodes nodes)
                           (shuffle-nodes nodes)))
              nu-tension (tension-directed edges nu-pos)]
          (if (<= nu-tension cur-tension)
            (recur nu-pos nu-tension (dec n))
            (recur pos cur-tension (dec n))))))))

(defn fill-details
  "This function fills in the width & height & text wrapping for each node, calculated from the appropriate functions."
  [{:keys [width-fn height-fn wrap-fn]} nodes]
  (into {}
        (map (fn [[k {:keys [text] :as item}]]
               [k (assoc item :width (width-fn text) :height (height-fn text) :wrapped-text (wrap-fn text))])
             nodes)))

(defn add-lines
  "This function connects nodes by lines based on their edges. Depending on the relative locations of the nodes, the lines will 'snake around' other nodes in different ways."
  [nodes edges]
  (into {}
        (map (fn [[k {:keys [x y height text] :as item}]]
               (let [e (filter #(= (first %) k) edges)
                     links (filter (fn [[a b]]
                                     (let [{bx :x by :y} (nodes b)]
                                       (or (< x bx) (and (= x bx) (< y by)))))
                                   e)]
                 [k 
                  (assoc item 
                    :links (map (fn [[a b]]
                                     (let [{by :y bx :x} (nodes b)]
                                       {:dest b
                                        :legs (cond (and (= y by) (not= (inc x) bx)) [{:x (inc (* x 2)) :y (inc (* y 2)) :dir :down}
                                                                                      {:x (inc (* x 2)) :y (+ (* y 2) 2) :dir :right}
                                                                                      {:x (inc (* bx 2)) :y (+ (* y 2) 2) :dir :up}
                                                                                      {:x (inc (* bx 2)) :y (inc (* by 2))}]
                                                    (= (dec y) by) [{:x (inc (* x 2)) :y (inc (* y 2)) :dir :up}
                                                                     {:x (inc (* x 2)) :y (* y 2) :dir :right}
                                                                    {:x (inc (* bx 2)) :y (* y 2) :dir :up}
                                                                    {:x (inc (* bx 2)) :y (inc (* by 2))}]
                                                    (= (inc y) by) [{:x (inc (* x 2)) :y (inc (* y 2)) :dir :down}
                                                                    {:x (inc (* x 2)) :y (+ (* y 2) 2) :dir :right}
                                                                    {:x (inc (* bx 2)) :y (* by 2) :dir :down}
                                                                    {:x (inc (* bx 2)) :y (inc (* by 2))}]
                                                    (= x bx) [{:x (inc (* x 2)) :y (inc (* y 2)) :dir :right}
                                                              {:x (+ (* x 2) 2) :y (inc (* y 2)) :dir :down}
                                                              {:x (+ (* x 2) 2) :y (inc (* by 2)) :dir :left}
                                                              {:x (inc (* bx 2)) :y (inc (* by 2))}]
                                                    (= (inc x) bx) (if (< by y)
                                                                     [{:x (inc (* x 2)) :y (inc (* y 2)) :dir :right}
                                                                      {:x (+ (* x 2) 2) :y (inc (* y 2)) :dir :up}
                                                                      {:x (+ (* x 2) 2) :y (inc (* by 2)) :dir :right}
                                                                      {:x (inc (* bx 2)) :y (inc (* by 2))}]
                                                                     [{:x (inc (* x 2)) :y (inc (* y 2)) :dir :right}
                                                                      {:x (+ (* x 2) 2) :y (inc (* y 2)) :dir :down}
                                                                      {:x (+ (* x 2) 2) :y (inc (* by 2)) :dir :right}
                                                                      {:x (inc (* bx 2)) :y (inc (* by 2))}])
                                                    (< by y) [{:x (inc (* x 2)) :y (inc (* y 2)) :dir :right}
                                                              {:x (+ (* x 2) 2) :y (inc (* y 2)) :dir :up}
                                                              {:x (+ (* x 2) 2) :y (+ (* by 2) 2) :dir :right}
                                                              {:x (inc (* bx 2)) :y (+ (* by 2) 2) :dir :up}
                                                              {:x (inc (* bx 2)) :y (inc (* by 2))}]
                                                    true [{:x (inc (* x 2)) :y (inc (* y 2)) :dir :right}
                                                          {:x (+ (* x 2) 2) :y (inc (* y 2)) :dir :down}
                                                          {:x (+ (* x 2) 2) :y (* by 2) :dir :right}
                                                          {:x (inc (* bx 2)) :y (* by 2) :dir :down}
                                                          {:x (inc (* bx 2)) :y (inc (* by 2))}])}))
                                links))]))
             nodes)))

(defn vline-groups
  "Returns a list of all vertical lines, grouped by what columns of nodes they fall between."
  [nodes]
  (for [curx (range (inc (get-side nodes)))]
    (for [[id {links :links}] nodes
          [link-index {:keys [legs]}] (numbered links)
          [leg-index {:keys [x dir]}] (numbered legs)
          :when (and (vertical dir) (= x (* curx 2)))]
      [id link-index leg-index])))

(defn hline-groups
  "Returns a list of all horizontal lines, grouped by what rows of nodes they fall between."
  [nodes]
  (for [cury (range (inc (get-side nodes)))]
    (for [[id {links :links}] nodes
          [link-index {:keys [legs]}] (numbered links)
          [leg-index {:keys [y dir]}] (numbered legs)
          :when (and (horizontal dir) (= y (* cury 2)))]
      [id link-index leg-index])))

(defn pos-map
  "Builds a map keyed on node location. (Used early in the code when position is still determined by row/column slots, not by exact x/y coordinates."
  [nodes]
  (into {}
        (map (fn [[key {:keys [x y]}]]
               [[x y] key])
             nodes)))

(defn reverse-links
  "Searches the graph for all edges/links and creates a map keyed on the destination of each link"
  [nodes]
  (into {}
        (for [id-goal (keys nodes)]
          [id-goal
           (for [[id {links :links}] nodes
                 [link-index {:keys [legs dest]}] (numbered links)
                 :when (= dest id-goal)]
             [id link-index])])))

(defn loose-layout
  "This is the first-pass function for laying out a graph. It makes the graph extremely sparse, which is fixed later on by the vcompact function."
  [{:keys [line-padding line-wid height-fn width-fn]} nodes]
  (let [side (get-side nodes)
        row-heights (vec (map (fn [y]
                                (apply max (cons 0 (map #(do (height-fn (% :text))) (filter #(= (% :y) y) (vals nodes))))))
                              (range side)))
        col-widths (vec (map (fn [x]
                               (apply max (cons 0 (map #(width-fn (% :text)) (filter #(= (% :x) x) (vals nodes))))))
                             (range side)))
        row-tots (vec (cons 0 (reductions + (map #(+ % line-padding) row-heights))))
        col-tots (vec (cons 0 (reductions + (map #(+ % line-padding) col-widths))))
        vlines (vline-groups nodes)
        hlines (hline-groups nodes)
        vlines-map (into {}
                         (for [[n group] (numbered vlines)
                               [m line] (numbered group)]
                           [line m]))
        hlines-map (into {}
                         (for [[n group] (numbered hlines)
                               [m line] (numbered group)]
                           [line m]))
        vline-counts (vec (cons 0 (reductions + (map #(* (count %) (+ line-padding line-wid)) vlines))))
        hline-counts (vec (cons 0 (reductions + (map #(* (count %) (+ line-padding line-wid)) hlines))))]
    (into {}
          (map (fn [[key {:keys [x y links text] :as item}]]
                 (let [xpos (+ (col-tots x) (vline-counts (inc x)) (half (- (col-widths x) (width-fn text))))
                       ypos (+ (row-tots y) (hline-counts (inc y)) (half (- (row-heights y) (height-fn text))))]
                   [key
                    (assoc item 
                      :xpos xpos
                      :ypos ypos
                      :links (vec (map (fn [{:keys [dest legs] :as link} link-index]
                                         (let [{x2 :x y2 :y text2 :text} (nodes dest)
                                               xpos2 (+ (col-tots x2) (vline-counts (inc x2)) (half (- (col-widths x2) (width-fn text2))))
                                               ypos2 (+ (row-tots y2) (hline-counts (inc y2)) (half (- (row-heights y2) (height-fn text2))))
                                               extended-legs (concat legs [{:x (inc (* x2 2)) :y (inc (* y2 2))}])]
                                           (assoc link :legs (vec (map (fn [{x1 :x y1 :y dir1 :dir :as pt1} {x2 :x y2 :y dir2 :dir :as pt2} index]
                                                                         (let [[xsrc ysrc] (if (or (vertical dir2) (horizontal dir1))
                                                                                             [0 1]
                                                                                             [1 0])
                                                                               xx (half ([x1 x2] xsrc))
                                                                               yy (half ([y1 y2] ysrc))]
                                                                           (assoc pt1 
                                                                             :xpos (if (ratio? xx)
                                                                                     (if (< index 2)
                                                                                       xpos
                                                                                       xpos2)
                                                                                     (+ (col-tots xx) (vline-counts xx) (* (vlines-map [key link-index (+ index -1 xsrc)]) (+ line-padding line-wid))))
                                                                             :ypos (if (ratio? yy)
                                                                                     (if (< index 2)
                                                                                       ypos
                                                                                       ypos2)
                                                                                     (+ (row-tots yy) (hline-counts yy) (* (hlines-map [key link-index (+ index -1 ysrc)]) (+ line-padding line-wid))))
                                                                             :dir dir1)))
                                                                       extended-legs
                                                                       (next extended-legs)
                                                                       (iterate inc 0))))))
                                       links
                                       (iterate inc 0))))]))
               nodes))))

(defn fix-leg-directions
  "This function should be obsolete in the near future. It checks all lines
  making up the edges to make sure they are marked with the correct direction
  (up, down, left, right). In the future, the lines will instead be marked as
  'horizontal' and 'vertical' instead, which will prevent any 'flips' in
  direction from happening."
  [nodes]
  (into {}
        (map (fn [[key {:keys [links] :as node}]]
               [key 
                (assoc node 
                  :links (vec (map (fn [{:keys [legs] :as link}]
                                     (assoc link
                                       :legs (vec (concat (map (fn [{xpos1 :xpos ypos1 :ypos dir1 :dir :as leg1} {xpos2 :xpos ypos2 :ypos :as leg2}]
                                                                 (let [nudir (if (= ypos1 ypos2)
                                                                                   (if (< xpos1 xpos2)
                                                                                     :right
                                                                                     :left)
                                                                                   (if (< ypos1 ypos2)
                                                                                     :down
                                                                                     :up))]
;                                                                   (when (and (not= dir1 nudir) (not (and (= xpos1 xpos2) (= ypos1 ypos2))))
;                                                                     (println "bad direction!"))
                                                                   (assoc leg1 
                                                                     :dir (if (and (not= dir1 nudir) (not (and (= xpos1 xpos2) (= ypos1 ypos2))))
                                                                            nudir
                                                                            dir1))))
                                                               legs
                                                               (next legs))
                                                          [(last legs)]))))
                                   links)))])
             nodes)))

(defn vcompact
  "This function is a monster and will probably be broken into smaller functions in the future.
  Its job is to pack the graph by pushing all nodes and lines upwards as much as possible.
  It also 'feathers out' all edges attaching to the same side of a single node, so that you can
  see separate attachment points for each."
  [{:keys [line-padding line-wid]} nodes]
  (let [side (get-side nodes)
        positions (pos-map nodes)
        rlinks (reverse-links nodes)]
    (first (loop [y 0
                  hlines (rest (hline-groups nodes))
                  m [nodes [[-1 0]]]]
             (if (= y side)
               m
               (let [k (loop [xx (let [[a b] (split-at (/ side 2) (range side))]
                                   (take side (interleave (concat b (repeat 0)) (concat (reverse a) (repeat 0))))
                                  (range side)
)
                              [nodes scan] m]
                         (if-let [[x & morex] xx]
                           (recur (next xx)
                                  (if-let [key (positions [x y])]
                                    (let [{:keys [ypos xpos width height]} (nodes key)
                                          nuy (scan-lowest-y scan xpos width)
                                          {:keys [links] :as node} (nodes key)
                                          nodes (assoc nodes 
                                                  key (assoc node 
                                                        :ypos nuy))
                                          links (vec (map (fn [{[l1 {dir :dir :as l2} & more] :legs :as link}]
                                                            (assoc link :legs (vec (concat [(assoc l1 :ypos nuy)
                                                                                            (if (vertical dir)
                                                                                              (assoc l2 :ypos nuy)
                                                                                              l2)]
                                                                                           more))))
                                                          links))
                                          minlegy (+ nuy line-padding line-wid)
                                          nodes nodes
                                          [nodes scan] (loop [links (rlinks key)
                                                              [nodes scan] [nodes scan]]
                                                         (if-let [[[other-key link-index] & more] (seq links)]
                                                           (recur more
                                                                  (let [legs (get-in nodes [other-key :links link-index :legs])
                                                                        [{xpos1 :xpos :as l1} {xpos2 :xpos dir2 :dir :as l2} & more] (reverse legs)]
                                                                    (if (horizontal dir2)
                                                                      (let [[nux nuwid] (if (= dir2 :left)
                                                                                          (let [nux (+ xpos width)]
                                                                                            [nux (+ (- xpos2 nux) line-wid)])
                                                                                          [xpos2 (+ (- xpos xpos2) line-wid)])
                                                                            nulegy (max minlegy (scan-lowest-y scan nux nuwid))]
                                                                        [(assoc-in nodes [other-key :links link-index :legs] (vec (reverse (concat [(assoc l1 :ypos nulegy :xpos (if (= dir2 :left) nux (- xpos line-wid))) (assoc l2 :ypos nulegy)] more)))) (scan-add scan nux (+ nulegy line-padding line-wid) nuwid)])
                                                                      [nodes scan])))
                                                           [nodes scan]))
                                          [nodes scan] (loop [links links
                                                                  acc []
                                                                  scan scan]
                                                             (if-let [[{[{xpos1 :xpos dir :dir :as leg1} {xpos2 :xpos :as leg2} & mlegs] :legs :as link} & more] (seq links)]
                                                               (if (horizontal dir)
                                                                 (let [[nux nuwid] (if (= dir :right)
                                                                                     (let [nux (+ xpos width)]
                                                                                       [nux (+ (- xpos2 nux) line-wid)])
                                                                                     [xpos1 (+ (- xpos1 xpos) line-wid)])
                                                                       nulegy (max minlegy (scan-lowest-y scan nux nuwid))]
                                                                   (recur more (conj acc (assoc link :legs (vec (concat [(assoc leg1 :ypos nulegy :xpos nux) (assoc leg2 :ypos nulegy)] mlegs)))) (scan-add scan nux (+ nulegy line-padding line-wid) nuwid)))
                                                                 (recur more (conj acc link) scan))
                                                               [(assoc nodes key (assoc (nodes key) :links acc)) scan]))
                                          nuheight (max height (+ (- (scan-lowest-y scan (+ xpos width) line-padding) nuy) line-padding) (+ (- (scan-lowest-y scan xpos 0) nuy) line-padding))
                                          nodes (update-in nodes 
                                                           [key :links] 
                                                           (fn [links]
                                                             (vec (map (fn [{[{:keys [dir ypos]} & {}] :legs :as link}]
                                                                         (assoc-in link
                                                                                   [:legs 0 :ypos] 
                                                                                   (cond (horizontal dir) ypos
                                                                                         (= dir :down) (+ nuy nuheight)
                                                                                         true (- nuy line-wid))))
                                                                       links))))
                                          nodes (loop [links (rlinks key)
                                                       nodes nodes]
                                                  (if-let [[[other-key link-index] & more] (seq links)]
                                                    (recur more
                                                           (update-in nodes 
                                                                      [other-key :links link-index :legs]
                                                                      (fn [legs]
                                                                        (let [[{ypos :ypos :as leg} {dir :dir}] (reverse legs)]
                                                                          (vec (reverse (cons (assoc leg :ypos
                                                                                                         (cond (horizontal dir) ypos
                                                                                                               (= dir :up) (+ nuy nuheight)
                                                                                                               true (- nuy line-wid)))
                                                                                                  (next (reverse legs)))))))))
                                                    nodes))
                                          nodes (assoc nodes key (assoc (nodes key) :height nuheight))
                                          scan (scan-add scan xpos (+ nuy line-padding nuheight) width)]
                                      [nodes scan])

                                    [nodes scan]))
                           [nodes scan]))]
                 (recur (inc y)
                        (rest hlines)
                        (loop [lin (first hlines)
                               [nodes scan] k]
                          (if-let [[[id link-index leg-index] & more] (seq lin)]
                            (recur more
                                   (let [path [id :links link-index :legs]
                                         legs (get-in nodes path)
                                         {xpos1 :xpos} (legs leg-index)
                                         {xpos2 :xpos} (legs (inc leg-index))
                                         xpos (min xpos1 xpos2)
                                         width (+ (abs (- xpos2 xpos1)) line-wid)
                                         nuy (scan-lowest-y scan xpos width)
                                         f (fn [nodes i]
                                             (assoc-in nodes (concat path [i :ypos]) nuy))]
                                     [(f (f nodes leg-index) (inc leg-index)) (scan-add scan xpos (+ nuy line-padding line-wid) width)]))
                            [nodes scan])))))))
    ;nodes
    ))

(defn mirror
  "Flips the graph along a diagonal axis going from the upper-left to bottom-right corner."
  [nodes]
  (into {}
        (map (fn [[id {:keys [x y xpos ypos width height links] :as node}]]
               [id (assoc node 
                     :xpos ypos
                     :ypos xpos
                     :width height
                     :height width
                     :x y
                     :y x
                     :links (vec (map (fn [{legs :legs :as link}]
                                        (assoc link :legs (vec (map (fn [{:keys [x y xpos ypos dir] :as leg}]
                                                                      (assoc leg 
                                                                        :xpos ypos
                                                                        :ypos xpos
                                                                        :x y
                                                                        :y x
                                                                        :dir ({:right :down
                                                                               :left :up
                                                                               :down :right
                                                                               :up :left} dir)))
                                                                    legs))))
                                      links)))])
             nodes)))

(defn make-undirected
  "Doubles up the edges in a graph to make the graph undirected"
  [lst]
  (concat lst
          (map (fn [[a b]]
                 [b a])
               lst)))

(defn hline-map
  "This builds a map, keyed on the y position, of all the lines in a graph. This lets us efficiently check for collisions."
  [nodes]
  (let [lst (mapcat (fn [[{} {links :links}]]
                      (mapcat :legs links))
                    nodes)]
    (into {}
          (map (fn [[key items]]
                 [key (map second items)])
               (group-by first
                         (map (fn [[{xpos1 :xpos ypos :ypos} {xpos2 :xpos}]]
                                    [(int (floor ypos)) [(int (floor (min xpos1 xpos2))) (int (floor (max xpos1 xpos2)))]])
                              (filter (fn [[{dir :dir} {}]]
                                        (horizontal dir))
                                      (map vector lst (next lst)))))))))

(defn line-unblocked
  "Checks a linemap (created by hline-map, probably) to see if a new line will cause a collision."
  [linemap y x1 x2]
  (> 2 
     (count (filter (fn [[xx1 xx2]]
                      (not (or (<= xx2 x1) (>= xx1 x2))))
                    (linemap (int (floor y)))))))

(defn remove-zigzags 
  "Nodes that are adjacent are connected by a 'zigzag' line for most of the algorithms, so that they have some 'give'. This function removes any zigzags for cases where nodes are close enough to be connected by just a straight line."
  [{:keys [line-padding line-wid]} nodes]
  (let [linemap (hline-map nodes)]
    (into {}
          (map (fn [[key {:keys [height xpos ypos links] :as node}]]
                 [key (assoc node :links (vec (map (fn [{{{xpos1 :xpos ypos1 :ypos dir :dir :as leg} 0 {ypos2 :ypos} 2 :as legs} :legs dest :dest :as link}]
                                                     (assoc link 
                                                       :legs
                                                       (if (and (= (count legs) 4) (= dir :right))
                                                         (let [{xpos-other :xpos ypos-other :ypos height-other :height} (nodes dest)
                                                               xend (- xpos-other line-wid)]
                                                           (cond (and (<= (+ ypos-other line-padding) ypos1 (- (+ ypos-other height-other) line-padding line-wid)) (line-unblocked linemap ypos1 xpos1 xend)) [leg (assoc leg :ypos ypos1 :xpos xend :dir nil)]
                                                                 (and (<= (+ ypos line-padding) ypos2 (- (+ ypos height) line-padding line-wid)) (line-unblocked linemap ypos2 xpos1 xend)) [(assoc leg :ypos ypos2) (assoc leg :ypos ypos2 :xpos xend :dir nil)]
                                                                 true legs))
                                                         legs)))
                                                   links)))])
               nodes))))

(defn mark-directed
  "All edges (aka links) are directed from the node closest to the upper-left corner to the other node. This is independent in directed graphs as to which way the arrow is pointing. This function checks the 'direction' of the edge and marks whether the arrow head goes on the start or end of the edge."
  [nodes edges]
  (let [emap (edge-map edges)]
    (into {}
          (map (fn [[key {:keys [links] :as node}]]
                 [key
                  (assoc node :links (vec (map (fn [{:keys [dest] :as link}]
                                                 (assoc link :arrow (if (some #{dest} (emap key))
                                                                      :end
                                                                      :start)))
                                               links)))])
               nodes))))

(defn layout-graph
  "Takes edges and nodes and generates a list of nodes with detailed attributes explaining their layout."
  [{:keys [width-fn height-fn] :as dim} edges nodes directed?]
  (let [undirected (make-undirected edges)
        annealed (if directed?
                   (anneal-directed edges nodes)
                   (anneal undirected nodes))
        lined (add-lines annealed undirected)
        loose (loose-layout dim lined)
        detailed (fill-details dim loose)
        hcompacted (fix-leg-directions (if true
                                         (mirror (vcompact dim (mirror detailed)))
                                         detailed))
        vcompacted (fix-leg-directions (if true
                                         (vcompact dim hcompacted)
                                         hcompacted))
        hcompacted2 (fix-leg-directions (if true
                                          (mirror (vcompact dim (mirror vcompacted)))
                                          vcompacted))
        vcompacted2 (fix-leg-directions (if false
                                          (vcompact dim hcompacted2)
                                          hcompacted2))
        hzigzags (fix-leg-directions (if true
                                       (remove-zigzags dim vcompacted2)
                                       vcompacted2))
        vzigzags (fix-leg-directions (if true
                                       (mirror (remove-zigzags dim (mirror hzigzags)))
                                       hzigzags))
        marked (fix-leg-directions (if directed?
                                     (mark-directed vzigzags edges)
                                     vzigzags))]
    (vals marked)))

;;Functions specific to binary trees

(defn line-info-btree 
  "Takes 2 rows of nodes in the binary tree and figures out the arrangement of lines between the two rows."
  [top bottom]
  (letfn [[f [top tx bottom bx in-line x]
           (if (and (seq top) (or (empty? bottom) (< (+ tx (first (first top))) (+ bx (first (first bottom))))))
             (let [[ind wid val left right] (first top)]
                  (concat (if left
                            [[:lbottom] [:line (- (+ tx ind) bx 2)] [:ltop] [:space wid] [:nop]] 
                            [[:space (- (+ tx ind wid) x)] [:nop]])
                          (f (rest top) (+ tx ind wid) bottom bx right (+ tx ind wid))))
             (when (seq bottom)
               (let [[ind wid val left right] (first bottom)]
                   (concat (if in-line
                             [[:rtop] [:line (- (+ bx ind) tx 2)] [:rbottom] [:space wid] [:nop]]
                             [[:space (- (+ bx ind wid) x)] [:nop]])
                           (f top tx (rest bottom) (+ bx ind wid) false (+ bx ind wid))))))]]
    (f top 0 bottom 0 false 0)))

(defn btree-row-wid [row]
  "Figures out the width of a row of the a binary tree."
  (apply + (map (fn [[a b c]]
                  (+ a b))
                row)))

(defn layout-btree 
  "Takes a binary tree and converts it into rows, with nodes at the same level of the tree in the same row. The data for each item in each row is a vector, formatted as [indentation width text left-children? right-children?"
  [btree]
  (if btree
    (let [[cur no yes] btree
          lno (layout-btree no)
          lyes (layout-btree yes)
          wno (apply max 0 (map btree-row-wid lno))
          wyes (apply max 0 (map btree-row-wid lyes))
          wid (+ (count (label-text cur)) 4)
          node-off (if (empty? lno)
                     0
                     (+ (btree-row-wid (first lno)) 2))
          yes-off (max (inc wno) (+ node-off wid 2))]
      (cons [[node-off wid cur (not (empty? lno)) (not (empty? lyes))]]
            (let [m (max (count lno) (count lyes))]
              (map (fn [rno ryes]
                     (if ryes
                       (let [[[a b c d e] & t] ryes]
                           (concat rno (cons [(- (+ a yes-off) (btree-row-wid rno)) b c d e] t)))
                       rno))
                   (take m (concat lno (repeat nil)))
                   (take m (concat lyes (repeat nil)))))))
    []))

(defn sp 
  "Returns 1->n spaces in a string"
  ([n]
     (out (fill \space n)))
  ([]
     (sp 1)))

(defn render-btree [rows]
  "Renders a binary tree. The rows handed to it contain the indentation already, so this function is concerned solely with rendering this row data to ASCII"
  (let [x rows]
    (loop [rows x]
      (when (seq rows)
        (let [row (first rows)]
          (doseq [[ind w str] row]
            (out (fill \space ind) \+ (fill \- (- w 2)) \+))
          (newline)
          (doseq [[ind w str] row]
            (dotimes [i ind]
              (out \space))
            (out "| " (label-text str) " |"))
          (newline)
          (doseq [[ind w str] row]
            (out (fill \space ind) \+ (fill \- (- w 2)) \+))
          (newline)
          (when (seq (rest rows))
            (let [li (line-info-btree row (first (rest rows)))]
              (doseq [[type n] li]
                (condp = type
                      :space (sp n)
                      :lbottom (sp)
                      :line (out (fill \_ n))
                      :ltop (out (fill \/))
                      :rtop (out (fill \\))
                      :rbottom (sp)
                      :nop nil))
              (newline)
              (doseq [[type n] li]
                (condp = type
                      :space (sp n)
                      :lbottom (out (fill \/))
                      :line (sp n)
                      :ltop (sp)
                      :rtop (sp)
                      :rbottom (out (fill \\))
                      :nop nil))
              (newline))))
        (recur (rest rows))))))

;Graphical Routines

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

;;Exported functions for interfacing with this library

(defn draw-tree 
  "Draws a tree to the console."
  [tree]
  (draw-shapes ascii-dim (integer-shapes (tree-to-shapes ascii-dim (layout-tree ascii-dim (idtree tree))))))
  
(defn draw-graph
  "Draws an undirected graph to the console. Requires a list of pairs representing the edges of the graph. Additionally, a separate map can be included containing node information mapped via the node ids in the edge list."
  ([edges nodes]
     (draw-shapes ascii-dim (integer-shapes (graph-to-shapes ascii-dim (layout-graph ascii-dim edges nodes false)))))
  ([edges]
     (draw-graph edges {})))

(defn draw-directed-graph
  "Draws an undirected graph to the console. Requires a list of pairs representing the edges of the graph. Additionally, a separate map can be included containing node information mapped via the node ids in the edge list. An effort is made to have majority of 'arrows' in the graph move in a downward direction."
  ([edges nodes]
     (draw-shapes ascii-dim (integer-shapes (graph-to-shapes ascii-dim (layout-graph ascii-dim edges nodes true)))))
  ([edges]
     (draw-directed-graph edges {})))

(defn draw-binary-tree
  "Draws a binary tree to the console. Nodes are in the form [text left right] where 'left' and 'right' are optional fields containing the children of this node."
  [btree]
  (render-btree (layout-btree btree)))

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

;(require 'vijual :reload)

;;Examples for use of this library

;(draw-tree [[:north-america [:usa [:miami] [:seattle] [:idaho [:boise]]]] [:europe [:germany] [:france [:paris] [:lyon] [:cannes]]]])

;(draw-tree [["Eubacteria" ["Aquificae"] ["Nitrospira"] ["Proteobacteria"] ["Chlamydiae"] ["Actinobacteria"]] ["Eukaryotes" ["Archaeplastida" ["Green Plants" ["Prasinophytes"] ["Chlorophyceae"] ["Trebouxiophyceae"] ["Ulvophyceae"] ["Streptohyta" ["Zygnematales"] ["Charales"] ["Embryophytes (land plants)"]]] ["Rhodophyta"] ["Glaucophytes"]] ["Unikots" ["Opisthokonts" ["Animals" ["Bilateria" ["Ecdysozoa" ["Nematoda"] ["Arthropoda"]] ["Lophotrochozoa"] ["Deuterostoma" ["Echinodermata"] ["Hemichordata"] ["Chordata" ["Urochordata"] ["Cephalochordata"] ["Yonnanozoon"] ["Craniata"]]]] ["Cnidaria"] ["Porifera"]] ["Choanoflagellates"] ["Filasterea"] ["Ichthyosporea"] ["Fungi"] ["Nucleariidae"]]] ["Chromalveolates" ["Rhizaria" ["Cercozoa"] ["Foraminifera"] ["Radiolaria"]] ["Alveolates"] ["Stramenopiles"] ["Hacrobia"]] ["Excavates" ["Malawimonads"] ["Discicristates" ["Euglenozoa"] ["Heterolobosea"]] ["Fornicata"]]]])

;(draw-graph [[:a :b] [:b :c] [:c :d] [:a :d] [:e :f] [:a :f] [:g :e] [:d :e]])

;(draw-graph [[:a :b] [:b :c] [:c :d] [:a :d] [:e :f] [:a :f] [:g :e] [:d :e]] {:a "Upper Floor" :b "Lower Floor" :c "Garden" :d "Stairway" :e "Front Yard" :f "Basement" :g "Walk Way"})

;(draw-graph [[:to-cellar :living-room] [:living-room :kitchen] [:from-studio :kitchen] [:kitchen :attic] [:kitchen :behind-house] [:north-of-house :behind-house] [:north-of-house :west-of-house] [:west-of-house :south-of-house] [:south-of-house :behind-house] [:behind-house :clearing] [:clearing :forest1] [:forest1 :forest-path] [:forest-path :north-of-house] [:forest1 :thinned-out-forest] [:forest-path :up-a-tree] [:forest-path :clearing-with-grating] [:clearing-with-grating :grating-room] [:clearing-with-grating :forest-with-sun] [:forest-with-sun :forest2] [:forest-with-sun :forest-path] [:south-of-house :forest2] [:west-of-house :stone-barrow] [:stone-barrow :inside-barrow] [:clearing :canyon-view] [:forest2 :clearing] [:forest2 :canyon-view] [:canyon-view :rocky-ledge] [:rocky-ledge :canyon-bottom] [:canyon-bottom :end-of-rainbow] [:end-of-rainbow :on-the-rainbow] [:on-the-rainbow :aragain-falls]])

;(draw-directed-graph [[:a :b] [:b :c] [:c :d] [:d :e] [:e :f] [:f :a]])

;(draw-directed-graph [["AFn" "Obj"] ["AFn" "IFn"] ["AFn" "Serializable"] ["AFunction" "AFn"] ["AFunction" "Comparator"] ["AFunction" "Fn"] ["AMapEntry" "APersistentVector"] ["AMapEntry" "IMapEntry"] ["APersistentMap" "AFn"] ["APersistentMap" "IPersistentMap"] ["APersistentMap" "Map"] ["APersistentSet" "AFn"] ["APersistentSet" "IPersistentSet"] ["APersistentSet" "Collection"] ["APersistentSet" "Set"] ["APersistentVector" "AFn"] ["APersistentVector" "IPersistentVector"] ["APersistentVector" "List"] ["APersistentVector" "RandomAccess"] ["ARef" "AReference"] ["ARef" "IRef"] ["AReference" "IReference"] ["ASeq" "Obj"] ["ASeq" "ISeq"] ["ASeq" "List"] ["Agent" "ARef"] ["ArraySeq" "ASeq"] ["ArraySeq" "IndexedSeq"] ["ArraySeq" "IReduce"] ["ArrayStream" "AFn"] ["Associative" "IPersistentCollection"] ["Atom" "ARef"] ["ConcurrentMap" "Map"] ["Cons" "ASeq"] ["Delay" "IDeref"] ["EnumerationSeq" "ASeq"] ["IFn" "Callable"] ["IFn" "Runnable"] ["IMapEntry" "Map$Entry"] ["IPersistentCollection" "Seqable"] ["IPersistentList" "Sequential"] ["IPersistentList" "IPersistentStack"] ["IPersistentMap" "Associative"] ["IPersistentSet" "IPersistentCollection"] ["IPersistentStack" "IPersistentCollection"] ["IPersistentVector" "Associative"] ["IPersistentVector" "Sequential"] ["IPersistentVector" "IPersistentStack"] ["IRef" "IDeref"] ["ISeq" "IPersistentCollection"] ["ISeq" "Sequential"] ["IndexedSeq" "ISeq"] ["IteratorSeq" "ASeq"] ["IteratorStream" "AFn"] ["Keyword" "IFn"] ["LazilyPersistentVector" "APersistentVector"] ["LazySeq" "Obj"] ["LazySeq" "ISeq"] ["LazySeq" "List"] ["List" "Collection"] ["MapEntry" "AMapEntry"] ["MultiFn" "AFn"] ["Namespace" "AReference"] ["Obj" "IObj"] ["Object$Future$IDeref" "IProxy"] ["Object$Future$IDeref" "IDeref"] ["Object$Future$IDeref" "Future"] ["PersistentArrayMap" "APersistentMap"] ["PersistentHashMap" "APersistentMap"] ["PersistentHashSet" "APersistentSet"] ["PersistentList" "ASeq"] ["PersistentList" "IPersistentList"] ["PersistentList" "IReduce"] ["PersistentList" "List"] ["PersistentQueue" "Obj"] ["PersistentQueue" "IPersistentList"] ["PersistentQueue" "Collection"] ["PersistentStructMap" "APersistentMap"] ["PersistentTreeMap" "APersistentMap"] ["PersistentTreeMap" "Sorted"] ["PersistentTreeSet" "APersistentSet"] ["PersistentTreeSet" "Sorted"] ["PersistentVector" "APersistentVector"] ["ProxyHandler" "InvocationHandler"] ["Range" "ASeq"] ["Range" "IReduce"] ["Ref" "ARef"] ["Ref" "IFn"] ["Ref" "IRef"] ["RestFn" "AFunction"] ["SeqEnumeration" "Enumeration"] ["SeqIterator" "Iterator"] ["Set" "Collection"] ["Stream" "Seqable"] ["Stream" "Sequential"] ["StringSeq" "ASeq"] ["StringSeq" "IndexedSeq"] ["Symbol" "AFn"] ["Symbol" "Serializable"] ["TransactionalHashMap" "ConcurrentMap"] ["Var" "ARef"] ["Var" "IFn"] ["Var" "IRef"] ["Var" "Settable"]])

;(draw-binary-tree [1 [2 [3] [4]] [5]])

;(draw-binary-tree [:organisms [:prokaryotes [:e-coli]] [:eukaryotes [:plants [:seed-plants [:carrot] [:oak-tree]] [:ferns]] [:animalia [:invertebrates [:sponge] [:worm]] [:mammals [:mouse] [:apes [:chimpanzee] [:human]]]]]])

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
