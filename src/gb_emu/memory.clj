(ns gb-emu.memory
  (:import [java.nio.file Files Paths]))

(def WRAM-SIZE 8192)

(defn create-wram
  "Initialize a new array of bytes for working random-access
   memory (WRAM)."
  []
  (byte-array WRAM-SIZE))

(def VRAM-SIZE 0)

(defn create-vram
  "Initialize a new array of bytes representing
   video (display) memory."
  []
  (byte-array VRAM-SIZE))


(def ROM-START  0x0000)
(def ROM-END    0x7FFF)

(def VRAM-START 0x8000)
(def VRAM-END   0x9FFF)

(def WRAM-START 0xC000)
(def WRAM-END   0xDFFF)

(defn addr-to-mem-key
  "Given an address of physical memory, return a key
   to the physical memory structure that can be used to access it."
  [addr]
  (cond
    (and (>= addr ROM-START)  (<= addr ROM-END))  :rom
    (and (>= addr VRAM-START) (<= addr VRAM-END)) :vram
    (and (>= addr WRAM-START) (<= addr WRAM-END)) :wram
    :else :unknown))

(defn create-memory
  "Create a map to each region of memory."
  []
  (atom {:rom nil
         :vram (create-vram)
         :hram nil
         :wram (create-wram)}))

(defn read-memory
  "Produce the byte at value `addr`, or nil.
   This fn will reach into the sub-modules of `memory`
   to produce the value."
  [memory addr]
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
