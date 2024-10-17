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

(defn ld-r-<hl>
  "Load to the 8-bit register `r`,
   data form the absolute address specified by the 16-bit register `HL`."
  []
  ())

(defn ld-<hl>-r
  "Load to the absolute address specified by the 16-bit register `HL`,
   data from the 8-bit register `r`."
  [^Atom cpu ^Keyword r]
  ;; TODO - Implementation should load the value stored at absolute address
  ;;        in the current 'hl' register, and 
  ())

(defn ld-<hl>-n
  "Load to the absolute address specified by the 16-bit register `HL`,
   the immediate data `n`."
  [^Atom cpu ^Keyword r]
  ())


;; 0x00
(defn nop
  "NOP
   No operation."
  [^Atom cpu]
  cpu)


(def ops
  "Table from opcode bytes -> map of operation data (including its
   invocation function)"

  {0x00 {:op nop :args [] :mnem "NOP" :len 1 :dur 4 :flags nil}
   0x01 nil 0x02 nil 0x03 nil 0x04 nil 0x05 nil 0x06 nil 0x07 nil
   0x08 nil 0x09 nil 0x0A nil 0x0B nil 0x0C nil 0x0D nil 0x0E nil 0x0F nil

   0x10 nil 0x11 nil 0x12 nil 0x13 nil 0x14 nil 0x15 nil 0x16 nil 0x17 nil
   0x18 nil 0x19 nil 0x1A nil 0x1B nil 0x1C nil 0x1D nil 0x1E nil 0x1F nil

   0x20 nil 0x21 nil 0x22 nil 0x23 nil 0x24 nil 0x25 nil 0x26 nil 0x27 nil
   0x28 nil 0x29 nil 0x2A nil 0x2B nil 0x2C nil 0x2D nil 0x2E nil 0x2F nil

   0x30 nil 0x31 nil 0x32 nil 0x33 nil 0x34 nil 0x35 nil 0x36 nil 0x37 nil
   0x38 nil 0x39 nil 0x3A nil 0x3B nil 0x3C nil 0x3D nil 0x3E nil 0x3F nil

    ;; 8-bit register loads
    ;; Register B
   0x40 {:op ld-r-r' :args [:reg-b :reg-b] :mnem "LD B,B"    :len 1 :dur 4 :flags nil}
   0x41 {:op ld-r-r' :args [:reg-b :reg-c] :mnem "LD B,C"    :len 1 :dur 4 :flags nil}
   0x42 {:op ld-r-r' :args [:reg-b :reg-d] :mnem "LD B,D"    :len 1 :dur 4 :flags nil}
   0x43 {:op ld-r-r' :args [:reg-b :reg-e] :mnem "LD B,E"    :len 1 :dur 4 :flags nil}
   0x44 {:op ld-r-r' :args [:reg-b :reg-h] :mnem "LD B,H"    :len 1 :dur 4 :flags nil}
   0x45 {:op ld-r-r' :args [:reg-b :reg-l] :mnem "LD B,L"    :len 1 :dur 4 :flags nil}
   0x46 {:op :unimpl :args []              :mnem "LD B,[HL]" :len 1 :dur 8 :flags nil}
   0x47 {:op ld-r-r' :args [:reg-b :reg-a] :mnem "LD B,A"    :len 1 :dur 4 :flags nil}
   ;; Register C
   0x48 {:op ld-r-r' :args [:reg-c :reg-b] :mnem "LD C,B"    :len 1 :dur 4 :flags nil}
   0x49 {:op ld-r-r' :args [:reg-c :reg-c] :mnem "LD C,C"    :len 1 :dur 4 :flags nil}
   0x4A {:op ld-r-r' :args [:reg-c :reg-d] :mnem "LD C,D"    :len 1 :dur 4 :flags nil}
   0x4B {:op ld-r-r' :args [:reg-c :reg-e] :mnem "LD C,E"    :len 1 :dur 4 :flags nil}
   0x4C {:op ld-r-r' :args [:reg-c :reg-h] :mnem "LD C,H"    :len 1 :dur 4 :flags nil}
   0x4D {:op ld-r-r' :args [:reg-c :reg-l] :mnem "LD C,L"    :len 1 :dur 4 :flags nil}
   0x4E {:op :unimpl :args []              :mnem "LD B,[HL]" :len 1 :dur 8 :flags nil}
   0x4F {:op ld-r-r' :args [:reg-c :reg-a] :mnem "LD C,A"    :len 1 :dur 4 :flags nil}
   ;; Register D
   0x50 {:op ld-r-r' :args [:reg-d :reg-b] :mnem "LD D,B"    :len 1 :dur 4 :flags nil}
   0x51 {:op ld-r-r' :args [:reg-d :reg-c] :mnem "LD D,C"    :len 1 :dur 4 :flags nil}
   0x52 {:op ld-r-r' :args [:reg-d :reg-d] :mnem "LD D,D"    :len 1 :dur 4 :flags nil}
   0x53 {:op ld-r-r' :args [:reg-d :reg-e] :mnem "LD D,E"    :len 1 :dur 4 :flags nil}
   0x54 {:op ld-r-r' :args [:reg-d :reg-h] :mnem "LD D,H"    :len 1 :dur 4 :flags nil}
   0x55 {:op ld-r-r' :args [:reg-d :reg-l] :mnem "LD D,L"    :len 1 :dur 4 :flags nil}
   0x56 {:op :unimpl :args []              :mnem "LD D,[HL]" :len 1 :dur 4 :flags nil}
   0x57 {:op ld-r-r' :args [:reg-d :reg-a] :mnem "LD D,A"    :len 1 :dur 4 :flags nil}
   ;; Register E
   0x58 {:op ld-r-r' :args [:reg-e :reg-b] :mnem "LD E,B"    :len 1 :dur 4 :flags nil}
   0x59 {:op ld-r-r' :args [:reg-e :reg-c] :mnem "LD E,C"    :len 1 :dur 4 :flags nil}
   0x5A {:op ld-r-r' :args [:reg-e :reg-d] :mnem "LD E,D"    :len 1 :dur 4 :flags nil}
   0x5B {:op ld-r-r' :args [:reg-e :reg-e] :mnem "LD E,E"    :len 1 :dur 4 :flags nil}
   0x5C {:op ld-r-r' :args [:reg-e :reg-h] :mnem "LD E,H"    :len 1 :dur 4 :flags nil}
   0x5D {:op ld-r-r' :args [:reg-e :reg-l] :mnem "LD E,L"    :len 1 :dur 4 :flags nil}
   0x5E {:op :unimpl :args []              :mnem "LD E,[HL]" :len 1 :dur 4 :flags nil}
   0x5F {:op ld-r-r' :args [:reg-e :reg-a] :mnem "LD E,A"    :len 1 :dur 4 :flags nil}
   ;; Register H
   0x60 {:op ld-r-r' :args [:reg-h :reg-b] :mnem "LD H,B"    :len 1 :dur 4 :flags nil}
   0x61 {:op ld-r-r' :args [:reg-h :reg-c] :mnem "LD H,C"    :len 1 :dur 4 :flags nil}
   0x62 {:op ld-r-r' :args [:reg-h :reg-d] :mnem "LD H,D"    :len 1 :dur 4 :flags nil}
   0x63 {:op ld-r-r' :args [:reg-h :reg-e] :mnem "LD H,E"    :len 1 :dur 4 :flags nil}
   0x64 {:op ld-r-r' :args [:reg-h :reg-h] :mnem "LD H,H"    :len 1 :dur 4 :flags nil}
   0x65 {:op ld-r-r' :args [:reg-h :reg-l] :mnem "LD H,L"    :len 1 :dur 4 :flags nil}
   0x66 {:op :unimpl :args []              :mnem "LD H,[HL]" :len 1 :dur 4 :flags nil}
   0x67 {:op ld-r-r' :args [:reg-h :reg-a] :mnem "LD H,A"    :len 1 :dur 4 :flags nil}
   ;; Register L
   0x68 {:op ld-r-r' :args [:reg-l :reg-b] :mnem "LD L,B"    :len 1 :dur 4 :flags nil}
   0x69 {:op ld-r-r' :args [:reg-l :reg-c] :mnem "LD L,C"    :len 1 :dur 4 :flags nil}
   0x6A {:op ld-r-r' :args [:reg-l :reg-d] :mnem "LD L,D"    :len 1 :dur 4 :flags nil}
   0x6B {:op ld-r-r' :args [:reg-l :reg-e] :mnem "LD L,E"    :len 1 :dur 4 :flags nil}
   0x6C {:op ld-r-r' :args [:reg-l :reg-h] :mnem "LD L,H"    :len 1 :dur 4 :flags nil}
   0x6D {:op ld-r-r' :args [:reg-l :reg-l] :mnem "LD L,L"    :len 1 :dur 4 :flags nil}
   0x6E {:op :unimpl :args []              :mnem "LD L,[HL]" :len 1 :dur 4 :flags nil}
   0x6F {:op ld-r-r' :args [:reg-l :reg-a] :mnem "LD L,A"    :len 1 :dur 4 :flags nil}
   ;; Indirected-HL
   0x70 {:op :unimpl :args []              :mnem "LD [HL],B" :len 1 :dur 8 :flags nil}
   0x71 {:op :unimpl :args []              :mnem "LD [HL],C" :len 1 :dur 8 :flags nil}
   0x72 {:op :unimpl :args []              :mnem "LD [HL],D" :len 1 :dur 8 :flags nil}
   0x73 {:op :unimpl :args []              :mnem "LD [HL],E" :len 1 :dur 8 :flags nil}
   0x74 {:op :unimpl :args []              :mnem "LD [HL],H" :len 1 :dur 8 :flags nil}
   0x75 {:op :unimpl :args []              :mnem "LD [HL],L" :len 1 :dur 8 :flags nil}
   0x76 {:op :unimpl :args []              :mnem "HALT"      :len 1 :dur 8 :flags nil}
   0x77 {:op :unimpl :args []              :mnem "LD [HL],A" :len 1 :dur 8 :flags nil}
   ;; Register A
   0x78 {:op ld-r-r' :args [:reg-a :reg-b] :mnem "LD A,"     :len 1 :dur 4 :flags nil}
   0x79 {:op ld-r-r' :args [:reg-a :reg-c] :mnem "LD A,"     :len 1 :dur 4 :flags nil}
   0x7A {:op ld-r-r' :args [:reg-a :reg-d] :mnem "LD A,"     :len 1 :dur 4 :flags nil}
   0x7B {:op ld-r-r' :args [:reg-a :reg-e] :mnem "LD A,"     :len 1 :dur 4 :flags nil}
   0x7C {:op ld-r-r' :args [:reg-a :reg-h] :mnem "LD A,"     :len 1 :dur 4 :flags nil}
   0x7D {:op ld-r-r' :args [:reg-a :reg-l] :mnem "LD A,"     :len 1 :dur 4 :flags nil}
   0x7E {:op nil     :args []              :mnem "LD A,"     :len 1 :dur 4 :flags nil}
   0x7F {:op ld-r-r' :args [:reg-a :reg-a] :mnem "LD A,"     :len 1 :dur 4 :flags nil}


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
