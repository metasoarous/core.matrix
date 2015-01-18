(ns clojure.core.matrix.impl.label-index)

(defprotocol PLabelIndex
  "Protocol for data structure supporting fast indexing by label and fast retreival of label given index"
  ; XXX - better to add a 2 arity of li-labels so that li-label is defined in terms of this? O/w ns clash
  (li-labels [this] "In order of index, return the labels")
  (li-label [this i] "Get label with a given index")
  (li-index [this lname] "Get index corresponding to label name")
  (li-add [this lname] [this i lname] "Add label either to end or at index i")
  (li-rename [this i new-lname] "Rename item i")
  (li-remove [this lname] "Remove label from index"))

(deftype LabelIndex
  [^clojure.lang.IPersistentVector labels
   ^clojure.lang.IPersistentMap index-map]
  Object
  (toString [_]
    (str labels))
  PLabelIndex
  (li-labels [_]
    labels)
  (li-label [_ i]
    (get labels i))
  (li-index [_ lname]
    (get index-map lname))
  (li-add [this lname]
    ; XXX - should this just be implemented in terms of the 3-arity version?
    (if (contains? index-map lname)
      this
      (LabelIndex.
        (conj labels lname)
        (assoc index-map lname (count labels)))))
  (li-rename [this i new-lname]
    ; If new-lname is already in labels, we only allow if effectively a no-op
    (let [old-i (index-map new-lname)]
      (assert (or (nil? old-i)
                  (= i old-i))
              "Label new-lname is already the label for another index"))
    (LabelIndex.
      (assoc labels i new-lname)
      (assoc index-map new-lname i)))
  (li-add [this i lname]
    (when (> i (count labels))
      (throw "Index out of range"))
    (if (contains? index-map lname)
      ; XXX - Hmm... don't like that this behaves differently than 2-arity version, but it's difficult to
      ; semantically figure out what it should do
      (if (= (li-label this i) lname)
        this
        (throw "Can't add a label that already exists"))
      (LabelIndex.
        (apply conj (into [] (take i labels)) lname (drop i labels))
        (reduce
          (fn [im ln]
            (update-in im [ln] inc))
          (assoc index-map lname i)
          (drop i labels)))))
  (li-remove [this lname]
    (if-not (contains? index-map lname)
      this
      (LabelIndex.
        (filterv (partial not= lname) labels)
        (reduce
          (fn [im ln]
            (update-in im [ln] dec))
          (dissoc index-map lname)
          (drop (inc (index-map lname)) labels))))))

(defn label-index
  "Construct a new LabelIndex with the given label names"
  [lnames]
  (let [uniq-lns (into [] (distinct lnames))]
    (LabelIndex.
      uniq-lns
      (into {} (map vector uniq-lns (range))))))

; XXX - Don't kow if we should put these in a separate PLabelIndexSubset or not
(defn li-lsubset [li lnames]
  (let [lns-set (set lnames)
        new-lns (filterv lns-set (li-labels li))]
    (label-index new-lns)))

(defn li-isubset [li is]
  (let [is-srtd (sort is)
        old-lnames (li-labels li)
        new-lns (map (partial get old-lnames) is-srtd)]
    (label-index new-lns)))


