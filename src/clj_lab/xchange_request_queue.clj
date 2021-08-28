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
  ;; Simulate network latency.
  (go
    (<! (timeout 1000))
    {:idx idx}))

(def idx-channel (chan))
(def trading-history-channel (chan))

;; Publish
(def idx-channel-loop
  (go-loop []
    (<! (timeout 1000))
    (>! idx-channel (UUID/randomUUID))
    (recur)))

;; Ingestion
(def trading-history-ingestion
  (go-loop []
    (let [idx (<! idx-channel)]
      (println "Fetching trade history" idx)
      (>! trading-history-channel (<! (fetch-trade-history! idx)))
      (recur))))

;; Kill switch
