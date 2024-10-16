(ns gb-emu.cpu
  (:import [clojure.lang Atom Keyword IFn]))

(defn create-cpu
  "Create a new CPU with all 8-bit registers set to 0.
   A CPU has registers A, B, C, D, E, F, H, and L.
   The stack pointer is a 16-bit register, initialized to 0.
   The program counter is a 16-bit register, initialized
   to 0."
  []
  (atom {:reg-a 0x00
         :reg-b 0x00
         :reg-c 0x00
         :reg-d 0x00
         :reg-e 0x00
         :reg-f 2r00000000 ;; Flags
         :reg-h 0x00
         :reg-l 0x00
         :stack-ptr 0x0000 ;; Loc. in address-space
         :pgm-ctr 0x0000}))

(defn get-register
  "Get the value of the supplied register."
  [cpu, reg-key]
  (get @cpu reg-key))

(defn set-register
  [^Atom cpu, ^Keyword reg-key, ^Integer value]
  (swap! cpu assoc reg-key value)
  cpu)

(defn wrap-arith [val]
  (mod (+ val 0x100) 0x100))


;; Operation primitives

(defn ld-r-r'
  "Load to the 8-bit register `r`, data from the 8-bit register `r'`."
  [^Atom cpu ^Keyword r ^Keyword r']
  (set-register cpu r (get-register cpu r'))
  cpu)

(defn ld-r-n
  "Load to the 8-bit register `r`, the immediate data `n`."
  [^Atom cpu ^Keyword r ^Keyword n]
  (set-register cpu r n)
  cpu)

(defn ld-indir-hl-r
  "Load to the 8-bit register `r`, data from the absolute address
   specified by the 16-bit register `HL`."
  [^Atom cpu ^Keyword r]
  ;; TODO - Implementation should load the value stored at absolute address
  ;;        in the current 'hl' register, and 
  ())

(defn ld-r-indir-hl
  "Load to the absolute address specified by the 16-bit register `hl`,
   data from the 8-bit register `r`."
  [^Atom cpu ^Keyword r]
  ())

(def op-info
  "Table from operations to their supplemental info --
   :len is the length in bytes (including opcode byte),
   :dur is the duration in T-states,
   :flags is the flags the instruction affects, if any"
  {ld-r-r'       {:mnem "LD r,r'"   :len 1 :dur 4 :flags nil}
   ld-r-n        {:mnem "LD r,n"    :len 2 :dur 8 :flags nil}
   ld-indir-hl-r {:mnem "LD r,(HL)" :len 1 :dur 4 :flags nil}
   ld-r-indir-hl {:mnem "LD (HL),r" :len 1 :dur 4 :flags nil}})

;; 0x00
(defn nop
  "NOP
   No operation."
  [^Atom cpu]
  cpu)


;; Table of opcode bytes -> Map of functions and arguments.
(def ops
  {0x00 {:op nop :args [] } 0x01 nil 0x02 nil 0x03 nil 0x04 nil 0x05 nil 0x06 nil 0x07 nil
   0x08 nil 0x09 nil 0x0A nil 0x0B nil 0x0C nil 0x0D nil 0x0E nil 0x0F nil

   0x10 nil 0x11 nil 0x12 nil 0x13 nil 0x14 nil 0x15 nil 0x16 nil 0x17 nil
   0x18 nil 0x19 nil 0x1A nil 0x1B nil 0x1C nil 0x1D nil 0x1E nil 0x1F nil

   0x20 nil 0x21 nil 0x22 nil 0x23 nil 0x24 nil 0x25 nil 0x26 nil 0x27 nil
   0x28 nil 0x29 nil 0x2A nil 0x2B nil 0x2C nil 0x2D nil 0x2E nil 0x2F nil

   0x30 nil 0x31 nil 0x32 nil 0x33 nil 0x34 nil 0x35 nil 0x36 nil 0x37 nil
   0x38 nil 0x39 nil 0x3A nil 0x3B nil 0x3C nil 0x3D nil 0x3E nil 0x3F nil

    ;; 8-bit register loads
    ;; Register B
   0x40 {:op ld-r-r' :args [:reg-b :reg-b]} 0x41 {:op ld-r-r' :args [:reg-b :reg-c]}
   0x42 {:op ld-r-r' :args [:reg-b :reg-d]} 0x43 {:op ld-r-r' :args [:reg-b :reg-e]}
   0x44 {:op ld-r-r' :args [:reg-b :reg-h]} 0x45 {:op ld-r-r' :args [:reg-b :reg-l]}
   0x46 nil                                 0x47 {:op ld-r-r' :args [:reg-b :reg-a]}
   ;; Register C
   0x48 {:op ld-r-r' :args [:reg-c :reg-b]} 0x49 {:op ld-r-r' :args [:reg-c :reg-c]}
   0x4A {:op ld-r-r' :args [:reg-c :reg-d]} 0x4B {:op ld-r-r' :args [:reg-c :reg-e]}
   0x4C {:op ld-r-r' :args [:reg-c :reg-h]} 0x4D {:op ld-r-r' :args [:reg-c :reg-l]}
   0x4E nil                                 0x4F {:op ld-r-r' :args [:reg-c :reg-a]}
   ;; Register D
   0x50 {:op ld-r-r' :args [:reg-d :reg-b]} 0x51 {:op ld-r-r' :args [:reg-d :reg-c]}
   0x52 {:op ld-r-r' :args [:reg-d :reg-d]} 0x53 {:op ld-r-r' :args [:reg-d :reg-e]}
   0x54 {:op ld-r-r' :args [:reg-d :reg-h]} 0x55 {:op ld-r-r' :args [:reg-d :reg-l]}
   0x56 nil                                 0x57 {:op ld-r-r' :args [:reg-d :reg-a]}
   ;; Register E
   0x58 {:op ld-r-r' :args [:reg-e :reg-b]} 0x59 {:op ld-r-r' :args [:reg-e :reg-c]}
   0x5A {:op ld-r-r' :args [:reg-e :reg-d]} 0x5B {:op ld-r-r' :args [:reg-e :reg-e]}
   0x5C {:op ld-r-r' :args [:reg-e :reg-h]} 0x5D {:op ld-r-r' :args [:reg-e :reg-l]}
   0x5E nil                                 0x5F {:op ld-r-r' :args [:reg-e :reg-a]}
   ;; Register H
   0x60 {:op ld-r-r' :args [:reg-h :reg-b]} 0x61 {:op ld-r-r' :args [:reg-h :reg-c]}
   0x62 {:op ld-r-r' :args [:reg-h :reg-d]} 0x63 {:op ld-r-r' :args [:reg-h :reg-e]}
   0x64 {:op ld-r-r' :args [:reg-h :reg-h]} 0x65 {:op ld-r-r' :args [:reg-h :reg-l]}
   0x66 nil                                 0x67 {:op ld-r-r' :args [:reg-h :reg-a]}
   ;; Register L
   0x68 {:op ld-r-r' :args [:reg-l :reg-b]} 0x69 {:op ld-r-r' :args [:reg-l :reg-c]}
   0x6A {:op ld-r-r' :args [:reg-l :reg-d]} 0x6B {:op ld-r-r' :args [:reg-l :reg-e]}
   0x6C {:op ld-r-r' :args [:reg-l :reg-h]} 0x6D {:op ld-r-r' :args [:reg-l :reg-l]}
   0x6E nil                                 0x6F {:op ld-r-r' :args [:reg-l :reg-a]}

   0x70 nil
   0x71 nil
   0x72 nil
   0x73 nil
   0x74 nil
   0x75 nil
   0x76 nil
   0x77 nil

   ;; Register A
   0x78 {:op ld-r-r' :args [:reg-a :reg-b]}
   0x79 {:op ld-r-r' :args [:reg-a :reg-c]}
   0x7A {:op ld-r-r' :args [:reg-a :reg-d]}
   0x7B {:op ld-r-r' :args [:reg-a :reg-e]}
   0x7C {:op ld-r-r' :args [:reg-a :reg-h]}
   0x7D {:op ld-r-r' :args [:reg-a :reg-l]}
   0x7E nil
   0x7F {:op ld-r-r' :args [:reg-a :reg-a]}


   0x80 nil 0x81 nil 0x82 nil 0x83 nil 0x84 nil 0x85 nil 0x86 nil 0x87 nil
   0x88 nil 0x89 nil 0x8A nil 0x8B nil 0x8C nil 0x8D nil 0x8E nil 0x8F nil

   0x90 nil 0x91 nil 0x92 nil 0x93 nil 0x94 nil 0x95 nil 0x96 nil 0x97 nil
   0x98 nil 0x99 nil 0x9A nil 0x9B nil 0x9C nil 0x9D nil 0x9E nil 0x9F nil

   0xA0 nil 0xA1 nil 0xA2 nil 0xA3 nil 0xA4 nil 0xA5 nil 0xA6 nil 0xA7 nil
   0xA8 nil 0xA9 nil 0xAA nil 0xAB nil 0xAC nil 0xAD nil 0xAE nil 0xAF nil

   0xB0 nil 0xB1 nil 0xB2 nil 0xB3 nil 0xB4 nil 0xB5 nil 0xB6 nil 0xB7 nil
   0xB8 nil 0xB9 nil 0xBA nil 0xBB nil 0xBC nil 0xBD nil 0xBE nil 0xBF nil

   0xC0 nil 0xC1 nil 0xC2 nil 0xC3 nil 0xC4 nil 0xC5 nil 0xC6 nil 0xC7 nil
   0xC8 nil 0xC9 nil 0xCA nil 0xCB nil 0xCC nil 0xCD nil 0xCE nil 0xCF nil

   0xD0 nil 0xD1 nil 0xD2 nil 0xD3 nil 0xD4 nil 0xD5 nil 0xD6 nil 0xD7 nil
   0xD8 nil 0xD9 nil 0xDA nil 0xDB nil 0xDC nil 0xDD nil 0xDE nil 0xDF nil

   0xE0 nil 0xE1 nil 0xE2 nil 0xE3 nil 0xE4 nil 0xE5 nil 0xE6 nil 0xE7 nil
   0xE8 nil 0xE9 nil 0xEA nil 0xEB nil 0xEC nil 0xED nil 0xEE nil 0xEF nil

   0xF0 nil 0xF1 nil 0xF2 nil 0xF3 nil 0xF4 nil 0xF5 nil 0xF6 nil 0xF7 nil
   0xF8 nil 0xF9 nil 0xFA nil 0xFB nil 0xFC nil 0xFD nil 0xFE nil 0xFF nil})

(defn lookup-and-exec
  [cpu byte]
   ;; TODO - Insert something here that updates the flags
  (let [op (get ops byte)]
    (if (nil? op)
      (let [ins (op-info )]
        (throw (RuntimeException. (format "Unimplemented instruction %s", ins))))
      (eval (op cpu))))
  cpu)

