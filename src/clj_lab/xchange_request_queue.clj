(ns clj-lab.xchange-request-queue
  "This namespace is an experiment of a queue system to make requests to Binance
  without exceeding its IP rate limit.

  It has a producer that will create requests and publishing them into a queue. On
  the other side, a consumer will consume that queue based on an arbitrary
  rule."
  (:require [clojure.core.async :as async :refer [<! >! <!! >!! chan go go-loop timeout alts!]])
  (:import [java.util UUID]))

(defn fetch-trade-history!
  [idx]
  (go
    ;; Simulate network latency.
    (<! (timeout 250))
    {:idx idx}))

(def idx-channel (chan))
(def trading-history-channel (chan))
(def terminate-channel (chan))

;; Publish
(defn publisher []
  (go-loop []
    (let [[terminate] (async/alts! [terminate-channel] :default :continue)] ;; Kill switch
      (when (= terminate :continue)
        (>! idx-channel (UUID/randomUUID))
        #_(<! (timeout 500))
        (recur)))))

;; Ingestion
(defn ingestion []
  (go-loop []
    (dotimes [x 5]
      (let [idx (<! idx-channel)]
        (println "Fetching trade history" idx x)
        (>! trading-history-channel (<! (fetch-trade-history! idx)))))
    (<! (timeout 5000))
    (recur)))

(defn start []
  (go (while true
        (<! trading-history-channel))))

(comment
  (publisher)
  (ingestion)
  (start)

  (>!! terminate-channel :drop-the-mic)

  )
