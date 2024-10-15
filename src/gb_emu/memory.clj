(ns gb-emu.memory
  (:import [java.nio.file Files Paths]))

;; TODO - Unclear if I'll want to delineate the two ROM banks
(def ROM-START  0x0000)
(def ROM-END    0x7FFF)

(def VRAM-START 0x8000)
(def VRAM-END   0x9FFF)

;; TODO - External RAM from cartridge

(def WRAM-START 0xC000)
(def WRAM-END   0xDFFF)



(def ECHO-START 0xE000)
(def ECHO-END 0xFDFF)

(def OAM-START 0xFE00)
(def OAM-END 0xFE9F)


(defn addr-to-mem-key
  "Given an address of physical memory, return a key
   to the physical memory structure that can be used to access it."
  [addr]
  (cond
    (and (>= addr ROM-START)  (<= addr ROM-END))  :rom
    (and (>= addr VRAM-START) (<= addr VRAM-END)) :vram
    (and (>= addr WRAM-START) (<= addr WRAM-END)) :wram
    (and (>= addr ECHO-START) (<= addr ECHO-END)) :echo
    (and (>= addr OAM-START) (<= addr OAM-END)) :oam
    :else :unknown))

(defn create-memory
  "Create a map to each region of memory."
  []
  (atom {:rom nil
         :vram nil 
         :echo nil
         :hram nil
         :wram (byte-array (- ECHO-END ECHO-START))}))

(defn read-memory
  "Produce the byte at value `addr`, or nil.
   This fn will reach into the sub-modules of `memory`
   to produce the value."
  [memory addr]
  ;; FIXME
  (->> addr
      (addr-to-mem-key)
      (memory)))

(defn get-byte
  "Produce the value of the byte at address `addr`, or nil."
  [bytes addr]
  (aget bytes addr))

(defn read-file-to-bytes
  [file-path]
  (let [path (Paths/get file-path (into-array String []))]
    (Files/readAllBytes path)))

(defn load-rom
  [file-path]
  (try
    (read-file-to-bytes file-path)
    (catch Exception e
      (println "Could not load ROM file:" (.getMessage e)))))
