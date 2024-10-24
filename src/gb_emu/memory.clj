(ns gb-emu.memory
  (:import [clojure.lang
            Atom
            Keyword
            PersistentHashMap
            PersistentVector]))

(def rom0-start 0x0000)
(def rom0-end   0x3FFF)

(def rom1-start 0x4000)
(def rom1-end   0x7FFF)

(def vram-start 0x8000)
(def vram-end   0x9FFF)

(def ex-ram-start 0xA000)
(def ex-ram-end   0xBFFF)

(def wram1-start 0xC000)
(def wram1-end   0xCFFF)

(def wram2-start 0xD000)
(def wram2-end   0xDFFF)

(def echo-start 0xE000)
(def echo-end   0xFDFF)

(def oam-start 0xFE00)
(def oam-end   0xFE9F)

(def prohib-start 0xFEA0)
(def prohib-end   0xFEFF)

(def io-start 0xFF00)
(def io-end   0xFF7F)

(def hram-start 0xFF80)
(def hram-end   0xFFFE)

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

(defn region-size
  "Utility function for determining the size of a memory region
   based on its start and end addresses."
  [start end]
  (+ 1 (- end start)))

(defn create-memory-map
  "Create a map to each region of memory."
  []
  (atom {:rom-0      (vec (repeat (region-size rom0-start rom0-end) 0))
         :rom-1      (vec (repeat (region-size rom1-start rom1-end) 0))
         :vram       (vec (repeat (region-size vram-start vram-end) 0))
         :ex-ram     (vec (repeat (region-size ex-ram-start ex-ram-end) 0))
         :wram-1     (vec (repeat (region-size wram1-start wram1-end) 0))
         :wram-2     (vec (repeat (region-size wram2-start wram2-end) 0))
         :echo       (vec (repeat (region-size echo-start echo-end) 0))
         :oam        (vec (repeat (region-size oam-start oam-end) 0))
         :prohibited (vec (repeat (region-size prohib-start prohib-end) 0))
         :io         (vec (repeat (region-size io-start io-end) 0))
         :hram       (vec (repeat (region-size hram-start hram-end) 0))
         :ie         (vec (repeat 1 0))}))


(defn load-file-as-bytes
  "Given a filepath, return a vector of bytes reprsenting
   the content of the file."
  [^String file-path]
  (with-open [input-stream (java.io.FileInputStream. file-path)]
    (let [file-size (.available input-stream)
          bytes (byte-array file-size)]
      (.read input-stream bytes)
      bytes)))


(defn cartridge-type
  "Given the ROM bytes, read byte 0x143 to determine the
   cartridge type and return it as a symbol."
  [^PersistentVector rom-bytes]
  (let [t (get rom-bytes 0x143)]
    (cond
      (= t 0x00) :rom-only
      (= t 0x01) :mbc1
      (= t 0x02) :mbc1+ram
      (= t 0x03) :mbc1+ram+bat
      (= t 0x05) :mbc2
      (= t 0x06) :mbc2+bat
      (= t 0x08) :rom+ram
      (= t 0x09) :rom+ram+bat
      (= t 0x0B) :mmm01
      (= t 0x0C) :mmm01+ram
      (= t 0x0D) :mmm01+ram+bat
      (= t 0x0F) :mbc3+tim+bat
      (= t 0x10) :mbc3+tim+ram+bat
      (= t 0x11) :mbc3
      (= t 0x12) :mbc3+ram
      (= t 0x13) :mbc3+ram+bat
      (= t 0x15) :mbc4
      (= t 0x16) :mbc4+ram
      (= t 0x17) :mbc4+ram+bat
      (= t 0x19) :mbc5
      (= t 0x1A) :mbc5+ram
      (= t 0x1B) :mbc5+ram+bat
      (= t 0x1C) :mbc5+rum
      (= t 0x1D) :mbc5+rum+ram
      (= t 0x1E) :mbc5+rum+ram+bat
      (= t 0xFC) :pocket-cam
      (= t 0xFD) :bandai-tama5
      (= t 0xFE) :huc3
      (= t 0xFF) :huc1+ram+bat
      :else :unknown)))


(defn cartridge-bat?
  [^Keyword type]
  (or (= type :mbc1+ram+bat)
      (= type :rom+ram+bat)
      (= type :mmm01+ram+bat)
      (= type :mbc3+tim+bat)
      (= type :mbc3+tim+ram+bat)
      (= type :mbc3+ram+bat)
      (= type :mbc4+ram+bat)
      (= type :mbc5+ram+bat)
      (= type :mbc5+rum+ram+bat)
      (= type :huc1+ram+bat)))


(defn cartridge-size
  "Read the ROM size byte at address 0x148
   to determine the ROM bank number"
  [^PersistentVector rom-bytes]
  (let [b (get rom-bytes 0x148)]
    (cond
      (= b 0x00)   2
      (= b 0x01)   4
      (= b 0x02)   8
      (= b 0x03)  16
      (= b 0x04)  32
      (= b 0x05)  64
      (= b 0x06) 128
      (= b 0x52)  72
      (= b 0x53)  80
      (= b 0x54)  96)))

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

(defn cartridge-sgb?
  [^PersistentVector rom-bytes]
  (= 3 (get rom-bytes 0x146)))

(defn cartridge-japanese?
  "Read byte 0x14A. If the byte is `0`, the cartridge
   is Japanese."
  [^PersistentVector rom-bytes]
  (= 0 (get rom-bytes 0x14A)))

(defn load-cartridge
  [^String file-path]
  (println (format "Loading ROM from file \"%s\"..." file-path))
  (try
    (let [rom (vec (load-file-as-bytes file-path))]
      (println (format "ROM has %d bytes" (.length rom)))
      {:name      (cartridge-name rom)
       :size      (cartridge-size rom)
       :type      (cartridge-type rom)
       :battery?  (cartridge-bat? (cartridge-type rom))
       :sgb?      (cartridge-sgb? rom)
       :japanese? (cartridge-japanese? rom)
       :bytes     rom})
    (catch Exception e
      (println "Could not load ROM file:" (.getMessage e)))))

;; NOTE - Checked address 0x40, the value is 0xC3 = -61. The value of
;;        address 0x41 is 36 = 0x24.
;;        This indicates that the byte order is flipped (little endian)
;;        from what my hexdump file shows. This will likely require code chgs.
(defn mmap-cartridge
  "Given the loaded cartridge, memory-map the data
   into the address bus."
  [^PersistentVector rom-bytes
   ^Atom memory]
  (let [rom-len (.length rom-bytes)
        rom0-len (region-size rom0-start rom0-end)
        rom1-len (region-size rom1-start rom1-end)
        rom0-data (subvec rom-bytes 0x0 (min rom-len rom0-len))
        rom1-data (if (> rom-len rom0-len)
                      (subvec rom-bytes rom0-len (min rom-len (+ rom0-len rom1-len)))
                      (vec (repeat rom1-len 0x0)))]
    (swap! memory assoc :rom-0 rom0-data)
    (swap! memory assoc :rom-1 rom1-data)))

(defn memory-fetch
  "Fetch the byte at the supplied address."
  [^Atom memory
   ^Integer addr]
  (let [memkey (addr-to-mem-key addr)]
    ()))
