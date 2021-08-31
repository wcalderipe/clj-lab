(ns clj-lab.time
  (:import [java.time Instant]
           [java.text SimpleDateFormat]))

(System/currentTimeMillis) ;; => 1630390524779

(Instant/now) ;; => #inst "2021-08-31T06:15:34.697838000-00:00"

(-> (Instant/now)
    (.toEpochMilli)) ;; => 1630390552300

(-> (Instant/now)
    (.getEpochSecond)) ;; => 1630390570

(-> (new SimpleDateFormat "dd/MM/yyyy")
    (.parse "12/10/1990")) ;; => #inst "1990-10-11T23:00:00.000-00:00"

(-> (new SimpleDateFormat "dd/MM/yyyy")
    (.parse "12/10/1990")
    (.getTime)) ;; => 655686000000
