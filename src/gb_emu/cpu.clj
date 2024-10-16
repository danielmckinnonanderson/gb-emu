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

(defn op-to-result
  "Return a shape in the form of a vector of vectors
   where each vector is `(register old-val new-val xform)"
  [^Atom cpu ^Keyword reg ^IFn  xform]
  (let [old-val (get @cpu reg)]
    [[reg old-val (wrap-arith (xform old-val)) xform]]))

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

(defn op-to-info
  [^IFn op]
  (get op-info op))

;; 0x00
(defn nop
  "NOP
   No operation."
  [cpu] nil)

;; 0x40
(defn ld-b-b
  "LD B,B
   Functionally equivalent to NOP, but with more pizzazz.
   See https://retrocomputing.stackexchange.com/questions/19632/what-could-be-the-reason-an-ld-b-b-instruction-was-used-in-this-busy-loop"
  [^Atom cpu]
  (ld-r-r' cpu :reg-b :reg-b)
  cpu)

;; 0x41
(defn ld-b-c
  "LD B,C
  Load register C into register B."
  [^Atom cpu]
  (ld-r-r' cpu :reg-b :reg-c)
  cpu)

;; 0x42
(defn ld-b-d
  "LD B,D
  Load register D into register B."
  [^Atom cpu]
  (ld-r-r' cpu :reg-b :reg-d)
  cpu)

;; 0x43
(defn ld-b-e
  "LD B,E
  Load register E into register B."
  [^Atom cpu]
  (ld-r-r' cpu :reg-b :reg-e)
  cpu)

;; 0x44
(defn ld-b-h
  "LD B,H
  Load register H into register B."
  [^Atom cpu]
  (ld-r-r' cpu :reg-b :reg-h)
  cpu)

;; 0x45
(defn ld-b-l
  "LD B,L
  Load register L into register B."
  [^Atom cpu]
  (ld-r-r' cpu :reg-b :reg-l)
  cpu)

;; 0x47
(defn ld-b-a
  "LD B,A
  Load register A (accumulator) into register B."
  [^Atom cpu]
  (ld-r-r' cpu :reg-b :reg-a)
  cpu)

;; 0x48
(defn ld-c-b
  "LD C,B
  Load register B into register C."
  [^Atom cpu]
  (ld-r-r' cpu :reg-c :reg-b)
  cpu)

;; 0x49
(defn ld-c-c
  "LD C,C
   Equivalent to NOP"
  [^Atom cpu]
  cpu)

;; 0x4A
(defn ld-c-d
  "LD C,D
   Load register D into register C."
  [^Atom cpu]
  (ld-r-r' cpu :reg-c :reg-d)
  cpu)

;; 0x4B
(defn ld-c-e
  "LD C,E
   Load register E into register C."
  [^Atom cpu]
  (ld-r-r' cpu :reg-c :reg-e)
  cpu)

;; 0x4C
(defn ld-c-h
  "LD C,H
   Load register H into register C."
  [^Atom cpu]
  (ld-r-r' cpu :reg-c :reg-h)
  cpu)

;; 0x4E
(defn ld-c-l
  "LD C,L
   Load register L into register C."
  [^Atom cpu]
  (ld-r-r' cpu :reg-c :reg-l)
  cpu)

;; 0x4F
(defn ld-c-a
  "LD C,A
   Load register A (accumulator) into register C."
  [^Atom cpu]
  (ld-r-r' cpu :reg-c :reg-a)
  cpu)

;; 0x50
(defn ld-d-b
  "LD D,B
  Load register B into register D."
  [^Atom cpu]
  (ld-r-r' cpu :reg-d :reg-b)
  cpu)

;; 0x51
(defn ld-d-c
  "LD D,C
  Load register C into register D."
  [^Atom cpu]
  (ld-r-r' cpu :reg-d :reg-c)
  cpu)

;; 0x52
(defn ld-d-d
  "LD D,D
  Load register D into register D (NOP)."
  [^Atom cpu]
  (ld-r-r' cpu :reg-d :reg-d)
  cpu)

;; 0x53
(defn ld-d-e
  "LD D,E
  Load register E into register D."
  [^Atom cpu]
  (ld-r-r' cpu :reg-d :reg-e)
  cpu)

;; 0x54
(defn ld-d-h
  "LD D,H
  Load register H into register D."
  [^Atom cpu]
  (ld-r-r' cpu :reg-d :reg-h)
  cpu)

;; 0x55
(defn ld-d-l
  "LD D,L
  Load register L into register D."
  [^Atom cpu]
  (ld-r-r' cpu :reg-d :reg-l)
  cpu)

;; 0x56
(defn ld-d-a
  "LD D,A
  Load register A into register D."
  [^Atom cpu]
  (ld-r-r' cpu :reg-h :reg-a)
  cpu)

;; 0x58
(defn ld-e-b
  "LD E,B
  Load register B into register E."
  [^Atom cpu]
  (ld-r-r' cpu :reg-e :reg-b)
  cpu)

;; 0x59
(defn ld-e-c
  "LD E,C
  Load register C into register E."
  [^Atom cpu]
  (ld-r-r' cpu :reg-e :reg-c)
  cpu)

;; 0x5A
(defn ld-e-d
  "LD E,D
  Load register D into register E."
  [^Atom cpu]
  (ld-r-r' cpu :reg-e :reg-d)
  cpu)

;; 0x5B
(defn ld-e-e
  "LD E,E
  Load register E into register E (NOP)."
  [^Atom cpu]
  (ld-r-r' cpu :reg-e :reg-e)
  cpu)

;; 0x5C
(defn ld-e-h
  "LD E,H
  Load register H into register E."
  [^Atom cpu]
  (ld-r-r' cpu :reg-e :reg-h)
  cpu)

;; 0x5D
(defn ld-e-l
  "LD E,L
  Load register L into register E."
  [^Atom cpu]
  (ld-r-r' cpu :reg-e :reg-l)
  cpu)

;; 0x5F
(defn ld-e-a
  "LD E,A
   Load register A (accumulator) into register E."
  [^Atom cpu]
  (ld-r-r' cpu :reg-e :reg-a)
  cpu)

;; 0x60
(defn ld-h-b
  "LD H,B
  Load register B into register H."
  [^Atom cpu]
  (ld-r-r' cpu :reg-h :reg-b)
  cpu)

;; 0x61
(defn ld-h-c
  "LD H,C
  Load register C into register H."
  [^Atom cpu]
  (ld-r-r' cpu :reg-h :reg-c)
  cpu)

;; 0x62
(defn ld-h-d
  "LD H,D
  Load register D into register H."
  [^Atom cpu]
  (ld-r-r' cpu :reg-h :reg-d)
  cpu)

;; 0x63
(defn ld-h-e
  "LD H,E
  Load register E into register H."
  [^Atom cpu]
  (ld-r-r' cpu :reg-h :reg-e)
  cpu)

;; 0x64
(defn ld-h-h
  "LD H,H
  Load register H into register H (NOP)."
  [^Atom cpu]
  (ld-r-r' cpu :reg-h :reg-h)
  cpu)

;; 0x65
(defn ld-h-l
  "LD H,L
  Load register L into register H."
  [^Atom cpu]
  (ld-r-r' cpu :reg-h :reg-l)
  cpu)

;; 0x66
(defn ld-h-a
  "LD H,A
   Load register A (accumulator) into register H."
  [^Atom cpu]
  (ld-r-r' cpu :reg-h :reg-a)
  cpu)

;; 0x68
(defn ld-l-b
  "LD L,B
  Load register B into register L."
  [^Atom cpu]
  (ld-r-r' cpu :reg-l :reg-b)
  cpu)

;; 0x69
(defn ld-l-c
  "LD L,C
  Load register C into register L."
  [^Atom cpu]
  (ld-r-r' cpu :reg-l :reg-c)
  cpu)

;; 0x6A
(defn ld-l-d
  "LD L,D
  Load register D into register L."
  [^Atom cpu]
  (ld-r-r' cpu :reg-l :reg-d)
  cpu)

;; 0x6B
(defn ld-l-e
  "LD L,E
  Load register E into register L."
  [^Atom cpu]
  (ld-r-r' cpu :reg-l :reg-e)
  cpu)

;; 0x6C
(defn ld-l-h
  "LD L,H
  Load register H into register L."
  [^Atom cpu]
  (ld-r-r' cpu :reg-l :reg-h)
  cpu)

;; 0x6D
(defn ld-l-l
  "LD L,L
  Load register L into register L (NOP)."
  [^Atom cpu]
  (ld-r-r' cpu :reg-l :reg-l)
  cpu)

;; 0x6F
(defn ld-l-a
  "LD L,A
  Load register A into register H."
  [^Atom cpu]
  (ld-r-r' cpu :reg-h :reg-a)
  cpu)


;; TODO - Come back to this eventually. I have barely started this and I
;;        already see some obvious design flaws with encapsulating the
;;        op primitive (like ld-r-r') with these named variants,
;;        namely, it kind of renders my 'op-info' table moot.
(def ops
  {0x00 nop 0x01 nil 0x02 nil 0x03 nil 0x04 nil 0x05 nil 0x06 nil 0x07 nil
   0x08 nil 0x09 nil 0x0A nil 0x0B nil 0x0C nil 0x0D nil 0x0E nil 0x0F nil

   0x10 nil 0x11 nil 0x12 nil 0x13 nil 0x14 nil 0x15 nil 0x16 nil 0x17 nil
   0x18 nil 0x19 nil 0x1A nil 0x1B nil 0x1C nil 0x1D nil 0x1E nil 0x1F nil

   0x20 nil 0x21 nil 0x22 nil 0x23 nil 0x24 nil 0x25 nil 0x26 nil 0x27 nil
   0x28 nil 0x29 nil 0x2A nil 0x2B nil 0x2C nil 0x2D nil 0x2E nil 0x2F nil

   0x30 nil 0x31 nil 0x32 nil 0x33 nil 0x34 nil 0x35 nil 0x36 nil 0x37 nil
   0x38 nil 0x39 nil 0x3A nil 0x3B nil 0x3C nil 0x3D nil 0x3E nil 0x3F nil

   ;; 8-bit register loads
   0x40 ld-b-b 0x41 ld-b-c 0x42 ld-b-d 0x43 ld-b-e 0x44 ld-b-h 0x45 ld-b-l 0x46 nil 0x47 ld-b-a
   0x48 ld-c-b 0x49 ld-c-c 0x4A ld-c-d 0x4B ld-c-e 0x4C ld-c-h 0x4D ld-c-l 0x4E nil 0x4F ld-c-a

   0x50 ld-d-b 0x51 ld-d-c 0x52 ld-d-d 0x53 ld-d-e 0x54 ld-d-h 0x55 ld-d-l 0x56 nil 0x57 ld-d-a
   0x58 ld-e-b 0x59 ld-e-c 0x5A ld-e-d 0x5B ld-e-e 0x5C ld-e-h 0x5D ld-e-l 0x5E nil 0x5F ld-e-a

   0x60 ld-h-b 0x61 ld-h-c 0x62 ld-h-d 0x63 ld-h-e 0x64 ld-h-h 0x65 ld-h-l 0x66 nil 0x67 ld-h-a
   0x68 ld-l-b 0x69 ld-l-c 0x6A ld-l-d 0x6B ld-l-e 0x6C ld-l-h 0x6D ld-l-l 0x6E nil 0x6F ld-l-a

   0x70 nil 0x71 nil 0x72 nil 0x73 nil 0x74 nil 0x75 nil 0x76 nil 0x77 nil
   0x78 nil 0x79 nil 0x7A nil 0x7B nil 0x7C nil 0x7D nil 0x7E nil 0x7F nil

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
    (eval (op cpu)))
  cpu)

