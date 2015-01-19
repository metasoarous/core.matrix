(ns clojure.core.matrix.impl.label-index)

(defprotocol PLabelIndex
  (li-labels [this] "In order of index, return the labels")
  (li-get [this i] "Get label with a given index")
  (li-index [this lname] "Get index corresponding to label name")
  (li-assoc [this i lname] "Assoc in label at index i"))

(deftype LabelIndex
  [^clojure.lang.IPersistentVector labels
   ^clojure.lang.IPersistentMap index-map]
  Object
  (toString [_]
    (str labels))
  PLabelIndex
  (li-labels [_]
    labels)
  (li-get [_ i]
    (get labels i))
  (li-index [_ lname]
    (get index-map lname))
  (li-assoc [this i new-lname]
    ; If new-lname is already in labels, we only allow if effectively a no-op
    (let [old-i (index-map new-lname)]
      (assert (or (nil? old-i)
                  (= i old-i))
              "Label new-lname is already the label for another index"))
    (LabelIndex.
      (assoc labels i new-lname)
      (assoc index-map new-lname i))))

(defn li-append
  [li lname]
  (li-assoc li (-> li li-labels count) lname))

(defn label-index
  "Construct a new LabelIndex with the given label names"
  [lnames]
  (let [uniq-lns (into [] (distinct lnames))]
    (LabelIndex.
      uniq-lns
      (into {} (map vector uniq-lns (range))))))

(defn li-lsubset [li lnames]
  (let [lns-set (set lnames)
        new-lns (filterv lns-set (li-labels li))]
    (label-index new-lns)))

(defn li-isubset [li is]
  (let [is-srtd (sort is)
        old-lnames (li-labels li)
        new-lns (map (partial get old-lnames) is-srtd)]
    (label-index new-lns)))

(defn li-join
  [& lis]
  (->> lis
       (mapcat li-labels)
       label-index))


