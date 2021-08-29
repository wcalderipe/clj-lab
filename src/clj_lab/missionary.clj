(ns clj-lab.missionary
  (:require [missionary.core :as m])
  (:import [java.util UUID]))

;;;; HELLO WORLD

;; Create a task.
(def hello-world
  (m/sp (println "Hello World!")))

;; Run a task.
(m/? hello-world)

;;;; SEQUENTIAL COMPOSITION

(def nap (m/sleep 1000))

(def slowmo-hello-world
  (m/sp (println "Hello")
        (m/? nap)
        (println "World")
        (m/? nap)
        (println "!")))

(m/? slowmo-hello-world)

;; Does it block the IO?
(do
  (println "Before")
  (m/? slowmo-hello-world)
  (println "After"))

;;;; PARALELL COMPOSITION

(def chatty-hello-world
  (m/join vector slowmo-hello-world slowmo-hello-world))

(m/? chatty-hello-world)

;; Simulate failure

(def unreliable-hello-world
  (m/sp (println "Hello")
        (m/? (m/sleep 500))
        (throw (ex-info "Something went wrong." {}))))

(m/? unreliable-hello-world)

(def unreliable-chatty-hello-world
  (m/join vector slowmo-hello-world unreliable-hello-world))

(m/? unreliable-chatty-hello-world)

;;;; EXECUTION MODEL

(defn blocking-hello-world []
  (println "Hello")
  (Thread/sleep 500)
  (println "World"))

(time (m/? (m/join vector (m/sp (blocking-hello-world)) (m/sp (blocking-hello-world)))))

;; Since blocking-hello-world is blocking the whole thread we're stuck. For
;; these purposes missionary allows offloading a task on a different
;; java.util.concurrent.Executor via the m/via macro. Missionary ships with 2
;; predefined executors, m/cpu for CPU bound tasks and m/blk for IO bound
;; (BLocKing) tasks.
(time (m/? (m/join vector (m/via m/blk (blocking-hello-world)) (m/via m/blk (blocking-hello-world)))))

(do (println "before")
    (m/? (m/join vector (m/via m/blk (blocking-hello-world)) (m/via m/blk (blocking-hello-world))))
    (println "after"))

;;;; HELLO FLOW

;; A flow producing the 10 first integers
(def input (m/seed (range 10)))

;; A task producing the sum of the 10 first integers
(def sum (m/reduce + input))

(m/? sum)

;; eduction passes a flow through a transducer.
(m/? (m/reduce conj (m/eduction (partition-all 4) input)))

(def hello-flow
  (m/ap
   (println (m/?> (m/seed ["Hello" "Flow" "!"])))
   (m/? (m/sleep 1000))))

;; The ?> operator pulls the first seeded value, forks evaluation and moves on
;; until end of body, producing result nil, then backtracks evaluation to the
;; fork point, pulls another value, forks evaluation again, and so on until
;; enumeration is exhausted. Meanwhile, reduce consolidates each result into a
;; vector. In an ap block, expressions have more than one possible value, that's
;; why they're called ambiguous process.

;; This is throwing:
;; "Wrong number of args (1) passed to: missionary.impl.Aggregate/2"
(m/? (m/reduce conj hello-world))

;;;; ITERATIVE QUERIES

(def uuids (repeatedly 10000 #(UUID/randomUUID)))

(def uuid-index
  (loop [uuids uuids
         index  {}]
    (if (seq uuids)
      (recur (rest uuids) (assoc index (first uuids) (rest uuids)))
      index)))

(defn pages
  ([] (pages :start))
  ([id]
   (m/ap
    (loop [id id]
      (let [x (->> [:page :next]
                   (map (m/? (api id)))         ;; fetch current page and next id
                   (remove nil?)                ;; detect end of stream
                   (m/seed)                     ;; seed branches
                   (m/?>))]                     ;; fork the process
        (if (uuid? x) (recur x) x))))))         ;; depending on the branch, emit the value or request more

(m/? (->> (pages)
          (m/eduction (map count))
          (m/reduce +)))

(defn api
  "pages through uuids, 10 at a time. a list-from of :start starts the listing"
  [list-from]
  (let [page (take 10 (if (= :start list-from)
                        uuids (get uuid-index list-from)))]
    (m/sleep 10 {:page page :next (last page)})))

(m/? (api nil))
