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
           [org.knowm.xchange.currency Currency]
           [org.knowm.xchange.dto.account
            Balance
            Wallet]))

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

;;;; UTIL

(defn parse-date
  "Parse a date dd/MM/yyyy string to java.util.Date."
  [date-str]
  (.parse (new java.text.SimpleDateFormat "dd/MM/yyyy") date-str))

(defprotocol Timestamp
  (timestamp [date]))

(extend-protocol Timestamp
  java.util.Date
  (timestamp [date]
    (.getTime date))

  String
  (timestamp [date]
    (.getTime (parse-date date))))

;;;; RCF

(comment
  ;; Create an exchange instance which can only request data from public endpoints.
  (.createExchange ExchangeFactory/INSTANCE BinanceExchange)

  ;; Make an exchange is taking a few milliseconds. It's probably because the
  ;; option `shouldLoadRemoveMetaData` in the ExchangeSpecification is set to
  ;; true by default.
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


  ,)
