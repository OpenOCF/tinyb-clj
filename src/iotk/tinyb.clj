(ns iotk.tinyb
  (:require [clojure.core.async :as a]
            [clojure.math.numeric-tower :as math])
  (:import [tinyb BluetoothDevice BluetoothManager]))

(def ir-temp-service-id   "F000AA00-0451-4000-B000-000000000000")
(def ir-temp-data-id      "F000AA01-0451-4000-B000-000000000000")
(def ir-temp-config-id    "F000AA02-0451-4000-B000-000000000000")
(def ir-temp-period-id    "F000AA03-0451-4000-B000-000000000000")

(def humidity-service-id  "F000AA20-0451-4000-B000-000000000000")
(def humidity-data-id     "F000AA21-0451-4000-B000-000000000000")
(def humidity-config-id   "F000AA22-0451-4000-B000-000000000000")
(def humidity-period-id   "F000AA23-0451-4000-B000-000000000000")

(def barometer-service-id  "F000AA40-0451-4000-B000-000000000000")
(def barometer-data-id     "F000AA41-0451-4000-B000-000000000000")
(def barometer-config-id   "F000AA42-0451-4000-B000-000000000000")
(def barometer-period-id   "F000AA43-0451-4000-B000-000000000000")
(def barometer-calib-id    "F000AA44-0451-4000-B000-000000000000")

(def illuminance-service-id  "F000AA70-0451-4000-B000-000000000000")
(def lux-data-id             "F000AA71-0451-4000-B000-000000000000")
(def lux-config-id           "F000AA72-0451-4000-B000-000000000000")
(def lux-period-id           "F000AA73-0451-4000-B000-000000000000")

(def enable  (byte-array [(byte 0x01)]))
(def disable (byte-array [(byte 0x00)]))

(def red-tag   "68:C9:0B:05:C8:87")
(def green-tag "68:C9:0B:05:BE:02")

(def ^:dynamic *running* (atom false))

(def manager (BluetoothManager/getBluetoothManager))

(defn print-device
  [^BluetoothDevice device]
  (println "Address    = " (.getAddress device))
  (println " Name      = " (.getName device))
  (println " Connected = " (.getConnected device)))

(defn get-device
  [address]
  (loop [i 0
         devices (.getDevices manager)]
    (println "cycle " i)
    (doseq [dev devices] (print-device dev))
    (let [dev (filter #(re-matches (re-pattern (str "(?i)" address))
                                   (.getAddress %))
                      devices)]
      (if (not (empty? dev))
        (do
          (println "found device " address)
          dev)
        (if (< i 15)
          (do (println "device not found"))
          (do (Thread/sleep 2000)
              (recur (inc i) (.getDevices manager))))))))

(defn get-service
  [device uuid]
  (println "getting services for device " (.getName device))
  (loop [services (.getServices device)
         i 0]
         (if (nil? services)
           nil
           (do
             (doseq [service services]
               (println "discovered service UUID: " (.getUuid service)))
             (let [the-service (filter #(re-matches (re-pattern (str "(?i)" uuid))
                                                            (.getUuid %))
                                               services)]
               (if (not (empty? the-service))
                 (do
                   (println "found service " uuid)
                   (first the-service))
                 (if (or (not @*running*) (> i 5))
                   nil
                   (do
                     (Thread/sleep 2000)
                     (recur (.getServices device) (inc i))))))))))

(defn get-connected-sensor
  [uuid]
  (if-let [sensor (first (get-device uuid))]
    (do
      (.stopDiscovery manager)
      (println "found sensor " (str sensor) "; connecting")
      (if (.connect sensor)
        (do
          (println "Connected to Sensor " (.getName sensor))
          (.addShutdownHook
           (Runtime/getRuntime)
           (proxy [Thread] []
             (run []
               (reset! *running* false)
               (.disconnect sensor))))
          sensor)
        (do
          (println "Could not connect device.")
          nil
          )))
    ;; else
    (do (println "No sensor with address " uuid)
        nil)))

(defn get-characteristic
  [service uuid]
  (if-let [characteristics (.getCharacteristics service)]
    (let [char (filter #(re-matches (re-pattern (str "(?i)" uuid))
                                    (.getUuid %))
                       characteristics)]
      (if (empty? char)
        nil
        (first char)))
    nil))

(defn ->celsius
  [raw]
  (/ raw 128.0))

(defn read-temps
  [sensor-val sensor-config sensor-period]
  (println "read-temps")
  ;; turn on the Temperature Service
  (.writeValue sensor-config enable)
  (while @*running*
    (let [raw-bytes (.readValue sensor-val)]
      (print "Temp raw: {")
      (doseq [b raw-bytes]
        (print (format "%02x" b)))
      (println "}")

      (let [raw-vec (vec raw-bytes)
            _ (println "raw temp vec: " raw-vec)
            object-temp-raw (int (bit-or
                                  (bit-and (get raw-vec 0) 0xff)
                                  (bit-shift-left (get raw-vec 1) 8)))
            object-temp-c (float (->celsius object-temp-raw))

            ambient-temp-raw (int (bit-or
                                   (bit-and (get raw-vec 2) 0xff)
                                   (bit-shift-left (get raw-vec 3) 8)))
            ambient-temp-c (float (->celsius ambient-temp-raw))]

        (println (format " Temp: Object = %fC, Ambient = %fC"
                         object-temp-c ambient-temp-c))))
    (Thread/sleep 1000))
  (.writeValue sensor-config disable))

(defn monitor-temperature
  [sensor]
  (let [temperature-service (get-service sensor ir-temp-service-id)]
    (if (nil? temperature-service)
      (do
        (println "This device does not have the temperature service we are looking for.")
        #_(.disconnect sensor))          ;;System.exit(-1)
      (do
        (println "Found service " (.getUuid temperature-service))
        ;; get BluetoothGattCharacteristic
        (let [temp-value  (get-characteristic temperature-service ir-temp-data-id)
              temp-config (get-characteristic temperature-service ir-temp-config-id)
              temp-period (get-characteristic temperature-service ir-temp-period-id)]
          (if (some nil? [temp-value temp-config temp-period])
            (do
              (println "Could not find the correct characteristics.")
              #_(.disconnect sensor))
            (a/go (read-temps temp-value temp-config temp-period))))))))

(defn bytes->int16
  ([bytes]
   (bytes->int16 bytes 0))
  ([bytes offset]
   (reduce + 0
           (map (fn [i]
                  ;; (let [shift (* (- 2 1 i) 8)]
                  (let [shift (* i 8)]
                    (bit-shift-left (bit-and (nth bytes (+ i offset))
                                             0x00FF)
                                    shift)))
                (range 0 2)))))

(defn read-lux
  [sensor-val sensor-config sensor-period]
  (println "read-lux")
  (.writeValue sensor-config enable)
  (while @*running*
    ;; data is one 16-bit unsigned int, delivered as byte[] by tinyb
    (let [raw-bytes (.readValue sensor-val)]
      (print "Lux raw: {")
      (doseq [b raw-bytes]
        (print (format " 0x%02x" b)))
      (println "}")

      ;; float sensorOpt3001Convert(uint16_t rawData) {
      ;;   uint16_t e, m;
      ;;   m = rawData & 0x0FFF;
      ;;   e = (rawData & 0xF000) >> 12;
      ;;   return m * (0.01 * pow(2.0,e)); }
      (let [uint16 (bytes->int16 raw-bytes)
            _ (println "uint16: " uint16)
            m (int (bit-and uint16 0x0FFF))
            e (int (bit-shift-right (bit-and uint16 0xF000) 12))
            lux (* m (* 0.01 (math/expt 2.0 e)))]
        (println (format " Lux: %f" lux))))
    (Thread/sleep 1000))
  (.writeValue sensor-config disable))

(defn monitor-illuminance
  [sensor]
  (let [illuminance-service (get-service sensor illuminance-service-id)]
    (if (nil? illuminance-service)
      (do
        (println "This device does not have the illuminance service we are looking for.")
        #_(.disconnect sensor))          ;;System.exit(-1)
      (do
        (println "Found service " (.getUuid illuminance-service))
        ;; get BluetoothGattCharacteristic
        (let [lux-value  (get-characteristic illuminance-service lux-data-id)
              lux-config (get-characteristic illuminance-service lux-config-id)
              lux-period (get-characteristic illuminance-service lux-period-id)]
          (if (some nil? [lux-value lux-config lux-period])
            (do
              (println "Could not find the correct characteristics.")
              #_(.disconnect sensor))
            (a/go (read-lux lux-value lux-config lux-period))))))))

(defn go
  []
  (reset! *running* true)

  (let [started (.startDiscovery manager)]
    (println "discovery started? " (str started)))

  (if-let [sensor (get-connected-sensor red-tag)]
    (do
      (monitor-temperature sensor)
      (monitor-illuminance sensor))))

(go)

(reset! *running* false)
(reset! *running* true)

;; (def started (.startDiscovery manager))
;; ;; (.stopDiscovery manager)

;; (println "discovery started? " (str started))

;; (def sensor (get-connected-sensor red-tag))

;sensor

;; (def temperature-service (get-service sensor ir-temp-service-id))
;; (if (nil? temperature-service)
;;   (.disconnect sensor))

;; (println "Found service " (.getUuid temperature-service))

;; ;; get BluetoothGattCharacteristic s
;; (def temp-value  (get-characteristic temperature-service ir-temp-data-id))
;; (def temp-config (get-characteristic temperature-service ir-temp-config-id))
;; (def temp-period (get-characteristic temperature-service ir-temp-period-id))
;; (if (some nil? [temp-value temp-config temp-period])
;;   (do
;;     (println "Could not find the correct characteristics.")
;;     (.disconnect sensor)))

;; ;; run in background
;; (a/go (read-temps temp-value temp-config temp-period))


;; (monitor-illuminance sensor)

;; (def lux-service (get-service sensor illuminance-service-id))

;; (def lux-value   (get-characteristic lux-service lux-data-id))
;; (def lux-config  (get-characteristic lux-service lux-config-id))
;; (def lux-period  (get-characteristic lux-service lux-config-id))
;; ;; ;; run in background
;; (a/go (read-lux lux-value lux-config lux-period))

;; (reset! *running* true)
;; (reset! *running* false)
