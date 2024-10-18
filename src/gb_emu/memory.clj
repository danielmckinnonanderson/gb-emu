(ns gb-emu.memory
  (:import [clojure.lang PersistentVector]))

(def rom0-start 0x0000)
(def rom0-end 0x3FFF)
(def rom1-start 0x4000)
(def rom1-end 0x7FFF)
(def vram-start 0x8000)
(def vram-end 0x9FFF)
(def ex-ram-start 0xA000)
(def ex-ram-end 0xBFFF)
(def wram1-start 0xC000)
(def wram1-end 0xCFFF)
(def wram2-start 0xD000)
(def wram2-end 0xDFFF)
(def echo-start 0xE000)
(def echo-end 0xFDFF)
(def oam-start 0xFE00)
(def oam-end 0xFE9F)
(def prohib-start 0xFEA0)
(def prohib-end 0xFEFF)
(def io-start 0xFF00)
(def io-end 0xFF7F)
(def hram-start 0xFF80)
(def hram-end 0xFFFE)
(def ie-address 0xFFFF)

(defn addr-to-mem-key
  "Given an address of physical memory, return a key
   to the memory structure that can be used to access it."
  [addr]
  (cond
    (and (>= addr rom0-start)  (<= addr rom0-end))   :rom-0
    (and (>= addr rom1-start)  (<= addr rom1-end))   :rom-1
    (and (>= addr vram-start)  (<= addr vram-end))   :vram
    (and (>= addr ex-ram-start)(<= addr ex-ram-end)) :ex-ram
    (and (>= addr wram1-start) (<= addr wram1-end))  :wram-1
    (and (>= addr wram2-start) (<= addr wram2-end))  :wram-2
    (and (>= addr echo-start)  (<= addr echo-end))   :echo ;; Prohibited
    (and (>= addr oam-start)   (<= addr oam-end))    :oam
    (and (>= addr prohib-start)(<= addr prohib-end)) :prohibited
    (and (>= addr io-start)    (<= addr io-end))     :io
    (and (>= addr hram-start)  (<= addr hram-end))   :hram
    (= addr ie-address)                              :ie
    :else :unknown))


(defn create-memory
  "Create a map to each region of memory."
  []
  (atom {:rom  nil
         :vram nil
         :echo nil
         :hram nil
         :wram nil}))

(defn load-file-as-bytes
  "Given a filepath, return a vector of bytes reprsenting
   the content of the file."
  [file-path]
  (with-open [input-stream (java.io.FileInputStream. file-path)]
    (let [file-size (.available input-stream)
          bytes (byte-array file-size)]
      (.read input-stream bytes)
      bytes)))

(defn cartridge-name
  "Given a vec of bytes representing the ROM data,
   return the cartridge name as a single String.
   The cartridge name is encoded as ASCII characters stored
   in the bytes from 0x134 - 0x143 inclusive.
   If the cartridge name is shorter than the allotted space,
   the remaining bytes are filled with `NUL` (`\\u0000`).
   These `NUL` characters will be removed by this function."
  [^PersistentVector rom-bytes]
  (let [char-bytes (subvec rom-bytes 0x134 0x143)]
       (->> char-bytes
            (map char)
            (filter #(not= % \u0000))
            (apply str))))

(defn load-rom
  [file-path]
  (try
    (load-file-as-bytes file-path)
    (catch Exception e
      (println "Could not load ROM file:" (.getMessage e)))))
