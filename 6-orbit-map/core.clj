(ns orbit-map.core
  (:require [clojure.string :as s]))

(defn parse-input
  [filename]
  (->> (s/split (slurp filename) #"\n")
       (map #(map keyword (s/split % #"\)"))))  )

(defn build-graph
  [input]
  (->> input
       (group-by first)
       (reduce-kv (fn [m k v] (assoc m k (map second v))) {})))

(defn centre-of-mass
  [graph]
  (let [visited (atom #{})
        stack (atom [])]
    (letfn [(dfs [start]
              (swap! visited conj start)
              (doall (map dfs (remove #(@visited %) (graph start))))
              (swap! stack conj start))]
      (doall
       (map #(when-not (@visited %)
               (dfs %))
            (keys graph)))
      (last @stack))))

(get-centre-of-mass (parse-input "./6-orbit-map/test-input.txt"))

(defn compute-distances
  [graph v distance]
  (let [dists (apply merge (map #(compute-distances graph % (inc distance)) (graph v)))]
    (assoc dists v distance)))

(defn checksum
  [graph]
  (->> (compute-distances graph (centre-of-mass graph) 0)
       vals
       (apply +)))

;; Part 1

;; parse-input as a directed graph
;; With topological sort, find root of the tree
;;   (Turns out that :COM is always the root of the tree)
;; Using BFS, compute distances to every node in the tree
;; Sum all those distances

(->> (parse-input "./6-orbit-map/input.txt")
     build-graph
     checksum)

;; => 402879

;; Part 2
;; parse-input as a directed graph, but inverted
;; Find lowest common ancestor by
;;   Trace from :YOU to :COM
;;   Trace from :SAN to somewhere in the patch between :YOU and :COM
;; Find the distance between :YOU and :SAN with the two traces

(defn trace
  "Traces, in a graph, from from to any of the nodes in to (set)"
  [graph from to acc]
  (if (to from)
    (conj acc from)
    (recur graph (graph from) to (conj acc from))))

(defn build-graph-backward
  "Builds graph with edges pointing backward.
   Since every node orbits at most one node,
   there is no need to keep a list of nodes in the adjacency list,
   this is actually just a normal map"
  [input]
  (reduce (fn [m [u v]] (assoc m v u)) {} input))

(defn orbital-transfers
  [graph]
  (let [you-to-com (trace graph :YOU #{:COM} [])
        san-to-lca (trace graph :SAN (set you-to-com) [])
        lca (last san-to-lca)
        you-to-lca (take-while #(not= lca %) you-to-com)]
    (- (+ (count you-to-lca) (count san-to-lca)) 3)))

(->> (parse-input "./6-orbit-map/input.txt")
     build-graph-backward
     orbital-transfers)
