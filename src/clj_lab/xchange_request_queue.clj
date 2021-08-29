(ns clj-lab.xchange-request-queue
  "This namespace is an experiment of a queue system to make requests to Binance
  without exceeding its IP rate limit.

  It has a producer that will create requests and publishing them into a queue. On
  the other side, a consumer will consume that queue based on an arbitrary
  rule."
  (:require [clojure.core.async :as async :refer [<! >! <!! >!! chan go go-loop timeout alts!]])
  (:import [java.util UUID]))

(defmacro with-kill-switch
  {:style/indent 1}
  [terminate-chan & body]
  `(let [[terminate#] (async/alts! [~terminate-chan] :default :continue)]
     (when (= terminate# :continue)
       ~@body)))

(defn fetch-trade-history!
  [idx]
  (async/go
    (async/<! (async/timeout 3000)) ;; Simulate network latency.
    (println "Fetched" idx)
    {:idx idx}))

;;;; Publisher

(defn publisher
  [{:keys [idx-chan kill-switch-chan]}]
  (async/go-loop []
    (with-kill-switch kill-switch-chan
      (async/>! idx-chan (UUID/randomUUID))
      (async/<! (async/timeout 250))
      (recur))))

;;;; Consumer

;;; Single request

(defn process
  [{:keys [in out wait-ms]}]
  (go-loop []
    (let [idx (<! in)]
      (println "Processing" idx)
      (>! out (<! (fetch-trade-history! idx))))
    (<! (timeout wait-ms))
    (recur)))

(defn start
  [{:keys [idx-chan trading-history-chan]}]
  (process {:in      idx-chan
            :out     trading-history-chan
            :wait-ms 1000}))

;;; Batch requests

(defn process-in-batch
  [{:keys [in out batch-size wait-ms]} f]
  (async/go-loop []
    (async/pipeline-async
     batch-size
     out
     (fn [input ch]
       (async/go
         (async/>! ch (async/<! (f input)))))
     in)
    (async/<! (async/timeout wait-ms))
    (recur)))

(defn start-batch-consumer
  [{:keys [idx-chan trading-history-chan]}]
  (process-in-batch
   {:in         idx-chan
    :out        trading-history-chan
    :batch-size 5
    :wait-ms    5000}
   fetch-trade-history!))

(comment
  (def idx-chan (async/chan))
  (def trading-history-chan (async/chan))
  (def kill-switch-chan (async/chan))

  (publisher {:idx-chan         idx-chan
              :kill-switch-chan kill-switch-chan})


  (start {:idx-chan             idx-chan
          :trading-history-chan trading-history-chan})

  (start-batch-consumer {:idx-chan             idx-chan
                         :trading-history-chan trading-history-chan})

  (async/close! kill-switch-chan)

  (<!! idx-chan)

  ;; For the single request process, taking from the output channel will release the parked thread.
  (<!! trading-history-chan)
  )
