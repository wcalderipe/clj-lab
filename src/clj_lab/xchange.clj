(ns clj-lab.xchange
  "XChange is Java library providing a simple and consistent interface for
  interacting with cryptocurrency exchanges.

  See https://github.com/knowm/XChange"
  (:require [clojure.core.protocols :as clj-protocols]
            [clojure.datafy :refer [datafy]])
  (:import [org.knowm.xchange
            Exchange
            ExchangeFactory
            ExchangeSpecification
            ExchangeSpecification$ResilienceSpecification]
           [org.knowm.xchange.binance BinanceExchange]
           [org.knowm.xchange.binance.service BinanceTradeHistoryParams]
           [org.knowm.xchange.currency
            Currency
            CurrencyPair]
           [org.knowm.xchange.dto.account
            Balance
            Wallet]
           [org.knowm.xchange.dto.trade
            UserTrades
            UserTrade]
           [org.knowm.xchange.dto
            Order$OrderType]))

;;;; DATAFY
;;
;; Provides datafication of XChange object types to Clojure's dialect. Moreover,
;; it cleans any noise (e.g., null values) out of the objects.
;;
;; References
;; - https://github.com/seancorfield/next-jdbc/blob/develop/src/next/jdbc/datafy.clj
;; - https://corfield.org/blog/2018/12/03/datafy-nav/
;;

(extend-protocol clj-protocols/Datafiable
  Currency
  (datafy [this]
    {:code (-> this (.getCurrencyCode) (.toLowerCase) (keyword))
     :name (.getDisplayName this)})

  CurrencyPair
  (datafy [this]
    [(datafy (.base this)) (datafy (.counter this))])

  Balance
  (datafy [this]
    ;; Balance has other attributes like available, frozen, borrowed, and loaned
    ;; to detail where your wallet money is at. It may be useful some day. I
    ;; guess for now just total is fine.
    (cond-> {:currency  (-> this (.getCurrency) (datafy))
             :total     (.getTotal this)}
      (.getTimestamp this) (merge {:timestamp (.getTimestamp this)})))

  Wallet
  (datafy [this]
    (cond-> {:balances (->> this
                            (.getBalances)
                            (vals)
                            (map datafy)
                            (filter #(> (:total %) 0)))}
      (.getId this)   (merge :id (.getId this))
      (.getName this) (merge :name (.getName this))))

  Order$OrderType
  (datafy [this]
    (-> this (.name) (.toLowerCase) (keyword)))

  UserTrade
  (datafy [this]
    {:id              (.getId this)
     :order-id        (.getOrderId this)
     :fee-amount      (.getFeeAmount this)
     :fee-currency    (-> this (.getFeeCurrency) (datafy))
     :order-user-ref  (.getOrderUserReference this)
     :type            (-> this (.getType) (datafy))
     :original-amount (.getOriginalAmount this)
     :instrument      (-> this (.getInstrument) (datafy))
     :price           (.getPrice this)
     :timestamp       (.getTimestamp this)
     :maker-order-id  (.getMakerOrderId this)
     :taker-order-id  (.getTakerOrderId this)})

  UserTrades
  (datafy [this]
    {:last-id          (.getlastID this) ; Yes, it's not formatted properly as camelCase.
     :next-page-cursor (.getNextPageCursor this)
     :trades           (->> this (.getTrades) (map datafy))})

  ExchangeSpecification$ResilienceSpecification
  (datafy [this]
    {:retry-enabled?        (.isRetryEnabled this)
     :rate-limiter-enabled? (.isRateLimiterEnabled this)})

  ExchangeSpecification
  (datafy [this]
    {:exchange-name         (.getExchangeName this)
     :host                  (.getHost this)
     :port                  (.getPort this)
     :plain-text-uri        (.getPlainTextUri this)
     :ssl-uri               (.getSslUri this)
     :username              (.getUserName this)
     :password              (.getPassword this)
     :api-key               (.getApiKey this)
     :secret-key            (.getSecretKey this)
     :proxy-host            (.getProxyHost this)
     :proxy-port            (.getProxyPort this)
     :resilience            (-> this (.getResilience) (datafy))
     :load-remote-metadata? (.isShouldLoadRemoteMetaData this)
     :class                 (.getExchangeClass this)}))

;;;; CONFIGURE AND MAKE AN EXCHANGE INSTANCE

(defmulti configure
  "Make an ExchangeSpecification object for the given exchange."
  :exchange)

(defmethod configure :default
  [{:keys [exchange]}]
  (throw (ex-info "Unsupported exchange" {:code     :exchange/unsupported-exchange
                                          :exchange exchange})))

(defmethod configure :binance
  [{:keys [api-key secret-key]}]
  (let [binance-spec (-> (new BinanceExchange)
                         (.getDefaultExchangeSpecification))]
    (doto binance-spec
      (.setApiKey api-key)
      (.setSecretKey secret-key))))

(defn make-exchange
  "Make an exchange instance for the given class or specification."
  [exchange-class-or-spec]
  (.createExchange ExchangeFactory/INSTANCE exchange-class-or-spec))

;;;; EXCHANGE

;; TODO: Rename to Exchange. Not a big fan of I prefix for interfaces.
;; See also https://stackoverflow.com/a/2814831/11095437
(defprotocol IExchange
  (fetch-wallets!                 [client]           "Fetch the wallets associated with the client instance credentials.")
  (fetch-listed-pairs!            [client]           "Fetch all listed currency pairs in the exchange.")
  (fetch-trading-history-by-pair! [client pair opts] "Fetch the trading history associated with the client instance credentials of a given pair."))

(defn currency->str
  [currency]
  (-> currency :code name (.toUpperCase)))

(defn pair->str
  [[base quote]]
  (str (currency->str base) "-" (currency->str quote)))

(defn ->CurrencyPair ^CurrencyPair
  [pair]
  (new CurrencyPair (pair->str pair)))

(defn contain-currency?
  [[base quote] currency]
  (or (= base currency)
      (= quote currency)))

(defn listed-pairs-for-wallet
  [wallet listed-pairs]
  (->> wallet
       :balances
       (mapcat (fn [{:keys [currency]}]
                 (filter #(contain-currency? % currency) listed-pairs)))
       (distinct)))

;;; BINANCE

(defn ->BinanceTradeHistoryParams ^BinanceTradeHistoryParams
  ([pair]
   (->BinanceTradeHistoryParams pair 0))
  ([pair start-id]
   (doto (new BinanceTradeHistoryParams)
     (.setCurrencyPair (->CurrencyPair pair))
     (.setStartId (str start-id)))))

(extend-protocol IExchange
  BinanceExchange
  (fetch-wallets! [client]
    (->> client
         (.getAccountService)
         (.getAccountInfo)
         (.getWallets)
         (vals)
         (map datafy)))

  (fetch-listed-pairs! [client]
    (->> client
         (.getExchangeSymbols)
         (map datafy)))

  ;; The function seems to lack the ability to fetch trading history
  ;; pagination. Accordingly to Binance's documentation, the endpoint to fetch
  ;; the history only returns 1000 items per result. What determines the
  ;; pagination cursor is the trade ID passed as `start-id`. Therefore, if a
  ;; response has more than 1000 items, the function (or another) needs to get
  ;; the last trade's ID and do it again on the same pair but this time with a
  ;; different `start-id` than zero.
  (fetch-trading-history-by-pair! [client pair opts]
    (-> client
        (.getTradeService)
        (.getTradeHistory (->BinanceTradeHistoryParams pair (get opts :start-id 0)))
        (datafy))))

;;;; RCF

(comment
  ;; Create an exchange instance which can only request data from public endpoints.
  (.createExchange ExchangeFactory/INSTANCE BinanceExchange)

  ;; Make an exchange is taking a few milliseconds. It's probably because the
  ;; option `shouldLoadRemoveMetaData` in the ExchangeSpecification is set to
  ;; true by default. It will do a request to fetch exchange metadata like
  ;; available currency pairs, trading rules, etc...
  ;;
  ;; See https://github.com/knowm/XChange/blob/develop/xchange-core/src/main/java/org/knowm/xchange/ExchangeSpecification.java#L35
  (def binance
    (-> {:exchange   :binance
         :api-key    ""
         :secret-key ""}
        (configure)
        (make-exchange)))

  ;; Be careful to print the account info on the REPL because it has a wallet
  ;; wiht balances of every listed currency in Binance - even the ones it
  ;; doesn't have any fund.
  (def account-info (-> binance
                        (.getAccountService)
                        (.getAccountInfo)))

  (def wallet (-> account-info
                  (.getWallets)
                  (vals)
                  (first)))

  (def balances (.getBalances wallet))

  (get balances Currency/ETH)

  (def balance (first balances-bigger-than-zero))

;;; DATAFY

  (datafy Currency/ETH)
  (datafy balance)
  (datafy wallet)

;;; TRADE HISTORY

  ;; XChange TradeService
  ;;
  ;; To fetch the trade history in Binance using method getTradeHistory in the
  ;; TradeService, I need the currency pair and a startId.
  ;;
  ;; It uses the startId argument to filter trades from the given trade ID
  ;; timestamp forward. If the startId is zero ("0"), it will fetch trades for
  ;; the currency pair from the oldest to newest, 1000 trades per page.
  ;;
  ;; The currency pair is a real problem because it enforces that consumers know
  ;; the pairs they have traded. In contrast, if you don't know the users traded
  ;; currency pairs, you'll have to use probability to fetch them based on the
  ;; wallet balances'.
  ;;
  ;; Assuming we have a wallet with ETH, BTC, and ADA. However, we don't know
  ;; how the user traded them.
  ;;
  ;; When we instantiate Binance, it fetches by default the exchange info, which
  ;; contains a list of trades currency pairs named "symbols" in the object. By
  ;; the time I'm writing this, it has 1589 pairs in the list, and the number
  ;; grows over time as they list more currencies.
  ;;
  ;; Now we need to guess the possible symbols for the currencies we have in our
  ;; balance.
  ;;
  ;; Pseudocode:
  ;;
  ;; possibleSymbols = []
  ;; for each balances
  ;;   for each symbols
  ;;     if (symbol.baseCurrency == balance.currency OR
  ;;         symbol.quoteCurrency == balance.currency)
  ;;           possibleSymbols.push(symbol)
  ;;
  ;; for each possibleSymbols
  ;;   Binance.getTradeHistory(currencyPair=possible_symbol, startId="0")
  ;;
  ;; They probably have a good reason for making that design decision. However,
  ;; it's an atrocious API design if we consider the strict rate limit those
  ;; exchange APIs have.
  ;;
  ;; References:
  ;; - https://dev.binance.vision/t/viable-way-to-get-trade-history-via-rest-api/1289/5
  ;; - https://dev.binance.vision/t/fetch-all-account-orders/279/9
  ;;

  (def trade-service
    (-> binance
        (.getTradeService)))

  (def trading-history-params
    (doto (new BinanceTradeHistoryParams)
      (.setCurrencyPair CurrencyPair/ETH_EUR)
      (.setStartId "0")))

  (def trades
    (.getTradeHistory trade-service trading-history-params))

;;; BINANCE

  ;; FETCH TRADING HISTORY OF A PAIR
  ;;
  ;; 1. Pair
  ;; 2. Binance listed pairs

  (def eur {:code :eur, :name "Euro"})
  (def eth {:code :eth, :name "Ether"})

  (def binance-pairs (fetch-listed-pairs! binance))

  (def eth-pairs
    (->> binance-pairs
         (filter (fn [[base quote]]
                   (or (= base eth)
                       (= quote eth))))))

  (count eth-pairs) ;; => 103

  (defn fetch-trading-history-by-pairs!
    [binance pairs]
    (doall (->> pairs
                (take 2)
                (map #(fetch-trading-history-by-pair! binance % {:start-id 0})))))

  ;; It work but takes a life time to fetch.
  (def eth-trading-history
    (fetch-trading-history-by-pairs! binance eth-pairs))

  (count eth-trading-history) ;; => 103

  ;; How long does it take to fetch the trading history of all pairs that contain ETH?
  ;; => It takes an average of 29seconds
  (time (fetch-trading-history-by-pairs! binance eth-pairs))

  ;; Fetch the trading history of my wallet

  (count (listed-pairs-for-wallet (datafy wallet) binance-pairs)) ;; => 894

  (defn fetch-wallet-trading-history!
    [binance wallet listed-pairs]
    (doall (->> listed-pairs
                (listed-pairs-for-wallet-balances wallet)
                (fetch-trading-history-by-pairs! binance))))

  ;; Of couse it threw an error
  ;;
  ;; Too much request weight used; current limit is 1200 request weight per 1
  ;; MINUTE. Please use the websocket for live updates to avoid polling the
  ;; API. (HTTP status code: 429)
  (def wallet-trading-history
    (time (fetch-wallet-trading-history! binance (datafy wallet) binance-pairs)))
  ,)

;;; THROTTLING
(comment
  (def wallet (-> binance
                  (fetch-wallets!)
                  (first)))

  (def pairs (->> binance
                  (fetch-listed-pairs!)
                  (listed-pairs-for-wallet wallet)))

  (def responses (atom []))

  ;;; BLOCKING

  (defn throttle
    [pairs f]
    (doseq [pair pairs]
      (f pair)
      (Thread/sleep 1000)))

  (defn throttle*
    [pairs batch-size f]
    (when (not (empty? pairs))
      (doseq [pair (take batch-size pairs)]
        (f pair))
      (Thread/sleep 61000) ;; Takes 1 minute to Binance's IP rate limit to refresh.
      (recur (nthrest pairs batch-size) batch-size f)))

  (defn progress-str
    [responses pairs]
    (str (count responses) "/" (count pairs)))

  (defn throttle-handler
    [pair]
    (println "Fetching trading history of" (pair->str pair) (progress-str @responses pairs))
    (let [res (fetch-trading-history-by-pair! binance pair {:start-id 0})]
      (swap! responses conj res)))

  (time (throttle pairs throttle-handler))
  ;; => "Elapsed time: 1202227.871292 msecs"

  ;; In Binance, IPs have a hard limit of 1200 credits (request weight on
  ;; Binance's terminology). Each request to the /myTrades endpoint costs 10
  ;; credits. In theory, the batch size must be less or equals than 1200 / 10.
  (time (throttle* pairs 100 throttle-handler))
  ;; => "Elapsed time: 856390.361417 msecs"

  (spit "responses.edn" @responses)

  (def responses (-> (slurp "responses.edn")
                     (read-string)
                     (atom)))

  (first @responses)

  (def trades (->> @responses
                   (mapcat :trades)))

  (keys (first trades))

  (->> trades
       (group-by :order-id)
       (filter (fn [[_order-id trades]]
                 (> (count trades) 1))))


  )
