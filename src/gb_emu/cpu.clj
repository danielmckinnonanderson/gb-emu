(ns gb-emu.cpu
  (:import [clojure.lang Atom Keyword IFn]))

;; CPU clock speed is 4.194304MHz
(def CLOCK-SPEED 4194304)

(defn create-cpu
  "Create a new CPU with all 8-bit registers set to 0.
   A CPU has registers A, B, C, D, E, F, H, and L.
   The stack pointer is a 16-bit register, initialized to 0.
   The program counter is a 16-bit register, initialized
   to 0."
  []
  (atom {:reg-a 0x00
         :reg-b 0x00
         :reg-c 0x13
         :reg-d 0x00
         :reg-e 0xD8
         :reg-f 2r00000000 ;; Flags
         :reg-h 0x01
         :reg-l 0x4D
         :stack-ptr 0xFFFE ;; Loc. in address-space
         :pgm-ctr 0x0100}))

(defn get-register
  "Get the value of the supplied register."
  [cpu, reg-key]
  (get @cpu reg-key))

(defn get-register-pair
  "Get the value of the supplied 8-bit registers as
   as single 16-bit value."
  [cpu, reg-key1 reg-key2]
  (bit-or (bit-shift-left (get-register cpu reg-key1) 0x08)
          (get-register cpu reg-key2)))

(defn set-register
  [^Atom cpu, ^Keyword reg-key, ^Integer value]
  (swap! cpu assoc reg-key value)
  cpu)

(defn wrap-arith [val max]
  (mod (+ val max) max))

(defn inc-pgm-ctr
  [^Atom cpu]
  (set-register cpu :pgm-ctr
                (wrap-arith (+ 1 (get-register cpu :pgm-ctr)) 0xFFFF)))

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

(defn implemented?
  [op]
  (not (= op :unimpl)))


(def ops
  "Table from opcode bytes -> map of operation data.

   Operation data includes the operation itself (as a function),
   the arguments to said function, the mnemonic (for debugging purposes),
   the length of the instruction in bytes, the duration in t-states,
   and the flags affected by the operation, if any.

   When no flags are affected by the operation, the value of :flags is nil.

   When flags are affected by the operation, the value is a vector with four elements.
   The first element is the zero-flag, the second element is the subtract-flag,
   the third element is the half-carry flag, and the final value is the carry-flag.
   Possible values within the vecotr include :calc for calculated based on the result
   of the instruction, 0 for reset, 1 for set."

  {0x00 {:op nop     :args [] :mnem "NOP"         :len 1 :dur  4 :flags nil}
   0x01 {:op :unimpl :args [] :mnem "LD BC,n16"   :len 3 :dur 12 :flags nil}
   0x02 {:op :unimpl :args [] :mnem "LD [BC],A"   :len 1 :dur  8 :flags nil}
   0x03 {:op :unimpl :args [] :mnem "INC BC"      :len 1 :dur  8 :flags nil}
   0x04 {:op :unimpl :args [] :mnem "INC B"       :len 1 :dur  4 :flags [:calc 0 :calc nil]}
   0x05 {:op :unimpl :args [] :mnem "DEC B"       :len 1 :dur  4 :flags [:calc 1 :calc nil]}
   0x06 {:op :unimpl :args [] :mnem "LD B,n8"     :len 2 :dur  8 :flags nil}
   0x07 {:op :unimpl :args [] :mnem "RLCA"        :len 1 :dur  4 :flags [0 0 0 :calc]}

   0x08 {:op :unimpl :args [] :mnem "LD [a16],SP" :len 3 :dur 20 :flags nil}
   0x09 {:op :unimpl :args [] :mnem "ADD HL,BC"   :len 1 :dur  8 :flags [nil 0 :calc :calc]}
   0x0A {:op :unimpl :args [] :mnem "LD A,[BC]"   :len 1 :dur  8 :flags nil}
   0x0B {:op :unimpl :args [] :mnem "DEC BC"      :len 1 :dur  8 :flags nil}
   0x0C {:op :unimpl :args [] :mnem "INC C"       :len 1 :dur  4 :flags [:calc 0 :calc nil]}
   0x0D {:op :unimpl :args [] :mnem "DEC C"       :len 1 :dur  4 :flags [:calc 1 :calc nil]}
   0x0E {:op :unimpl :args [] :mnem "LD C,n8"     :len 2 :dur  8 :flags nil}
   0x0F {:op :unimpl :args [] :mnem "RRCA"        :len 1 :dur  4 :flags [0 0 0 :calc]}

   0x10 {:op :unimpl :args [] :mnem "STOP n8"     :len 2 :dur  4 :flags nil}
   0x11 {:op :unimpl :args [] :mnem "LD DE,n16"   :len 3 :dur 12 :flags nil}
   0x12 {:op :unimpl :args [] :mnem "LD [DE],A"   :len 1 :dur  8 :flags nil}
   0x13 {:op :unimpl :args [] :mnem "INC DE"      :len 1 :dur  8 :flags nil}
   0x14 {:op :unimpl :args [] :mnem "INC D"       :len 1 :dur  4 :flags [:calc 0 :calc nil]}
   0x15 {:op :unimpl :args [] :mnem "DEC D"       :len 1 :dur  4 :flags [:calc 1 :calc nil]}
   0x16 {:op :unimpl :args [] :mnem "LD D,n8"     :len 2 :dur  8 :flags nil}
   0x17 {:op :unimpl :args [] :mnem "RLA"         :len 1 :dur  4 :flags [0 0 0 :calc]}

   0x18 {:op :unimpl :args [] :mnem "JR e8"       :len 2 :dur 12 :flags nil}
   0x19 {:op :unimpl :args [] :mnem "ADD HL,DE"   :len 1 :dur  8 :flags [nil 0 :calc :calc]}
   0x1A {:op :unimpl :args [] :mnem "LD A,[DE]"   :len 1 :dur  8 :flags nil}
   0x1B {:op :unimpl :args [] :mnem "DEC DE"      :len 1 :dur  8 :flags nil}
   0x1C {:op :unimpl :args [] :mnem "INC E"       :len 1 :dur  4 :flags [:calc 0 :calc nil]}
   0x1D {:op :unimpl :args [] :mnem "DEC E"       :len 1 :dur  4 :flags [:calc 1 :calc nil]}
   0x1E {:op :unimpl :args [] :mnem "LD E,n8"     :len 2 :dur  8 :flags nil}
   0x1F {:op :unimpl :args [] :mnem "RRA"         :len 1 :dur  4 :flags [0 0 0 :calc]}

   0x20 {:op :unimpl :args [] :mnem "JR NZ,e8"    :len 2 :dur [12 8] :flags nil}
   0x21 {:op :unimpl :args [] :mnem "LD HL,n16"   :len 3 :dur 12 :flags nil}
   0x22 {:op :unimpl :args [] :mnem "LD [HL+],A"  :len 1 :dur  8 :flags nil}
   0x23 {:op :unimpl :args [] :mnem "INC HL"      :len 1 :dur  8 :flags nil}
   0x24 {:op :unimpl :args [] :mnem "INC H"       :len 1 :dur  4 :flags [:calc 0 :calc nil]}
   0x25 {:op :unimpl :args [] :mnem "DEC H"       :len 1 :dur  4 :flags [:calc 1 :calc nil]}
   0x26 {:op :unimpl :args [] :mnem "LD H,n8"     :len 2 :dur  8 :flags nil}
   0x27 {:op :unimpl :args [] :mnem "DAA"         :len 1 :dur  4 :flags [:calc nil 0 :calc]}

   0x28 {:op :unimpl :args [] :mnem "JR Z,e8"     :len 2 :dur [12 8] :flags nil}
   0x29 {:op :unimpl :args [] :mnem "ADD HL,HL"   :len 1 :dur  8 :flags [nil 0 :calc :calc]}
   0x2A {:op :unimpl :args [] :mnem "LD A,[HL+]"  :len 1 :dur  8 :flags nil}
   0x2B {:op :unimpl :args [] :mnem "DEC HL"      :len 1 :dur  8 :flags nil}
   0x2C {:op :unimpl :args [] :mnem "INC L"       :len 1 :dur  4 :flags [:calc 0 :calc nil]}
   0x2D {:op :unimpl :args [] :mnem "DEC L"       :len 1 :dur  4 :flags [:calc 1 :calc nil]}
   0x2E {:op :unimpl :args [] :mnem "LD L,n8"     :len 2 :dur  8 :flags nil}
   0x2F {:op :unimpl :args [] :mnem "CPL"         :len 1 :dur  4 :flags [nil 1 1 nil]}

   0x30 {:op :unimpl :args [] :mnem "JR NC,e8"    :len 2 :dur [12 8] :flags nil}
   0x31 {:op :unimpl :args [] :mnem "LD SP,n16"   :len 3 :dur 12 :flags nil}
   0x32 {:op :unimpl :args [] :mnem "LD [HL-],A"  :len 1 :dur  8 :flags nil}
   0x33 {:op :unimpl :args [] :mnem "INC SP"      :len 1 :dur  8 :flags nil}
   0x34 {:op :unimpl :args [] :mnem "INC [HL]"    :len 1 :dur 12 :flags [:calc 0 :calc nil]}
   0x35 {:op :unimpl :args [] :mnem "DEC [HL]"    :len 1 :dur 12 :flags [:calc 1 :calc nil]}
   0x36 {:op :unimpl :args [] :mnem "LD [HL],n8"  :len 2 :dur 12 :flags nil}
   0x37 {:op :unimpl :args [] :mnem "SCF"         :len 1 :dur  4 :flags [nil 0 0 1]}

   0x38 {:op :unimpl :args [] :mnem "JR C,e8"     :len 2 :dur [12 8] :flags nil}
   0x39 {:op :unimpl :args [] :mnem "ADD HL,SP"   :len 1 :dur  8 :flags [nil 0 :calc :calc]}
   0x3A {:op :unimpl :args [] :mnem "LD A,[HL-]"  :len 1 :dur  8 :flags nil}
   0x3B {:op :unimpl :args [] :mnem "DEC SP"      :len 1 :dur  8 :flags nil}
   0x3C {:op :unimpl :args [] :mnem "INC A"       :len 1 :dur  4 :flags [:calc 0 :calc nil]}
   0x3D {:op :unimpl :args [] :mnem "DEC A"       :len 1 :dur  4 :flags [:calc 1 :calc nil]}
   0x3E {:op :unimpl :args [] :mnem "LD A,n8"     :len 2 :dur  8 :flags nil}
   0x3F {:op :unimpl :args [] :mnem "CCF"         :len 1 :dur  4 :flags [nil 0 0 :calc]}

   ;; --------------------
   ;; 8-bit register loads
   ;; --------------------
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

   ;; HALT
   0x76 {:op :unimpl :args []              :mnem "HALT"      :len 1 :dur 8 :flags nil}

   0x77 {:op :unimpl :args []              :mnem "LD [HL],A" :len 1 :dur 8 :flags nil}
   ;; Register A
   0x78 {:op ld-r-r' :args [:reg-a :reg-b] :mnem "LD A,B"    :len 1 :dur 4 :flags nil}
   0x79 {:op ld-r-r' :args [:reg-a :reg-c] :mnem "LD A,C"    :len 1 :dur 4 :flags nil}
   0x7A {:op ld-r-r' :args [:reg-a :reg-d] :mnem "LD A,D"    :len 1 :dur 4 :flags nil}
   0x7B {:op ld-r-r' :args [:reg-a :reg-e] :mnem "LD A,E"    :len 1 :dur 4 :flags nil}
   0x7C {:op ld-r-r' :args [:reg-a :reg-h] :mnem "LD A,H"    :len 1 :dur 4 :flags nil}
   0x7D {:op ld-r-r' :args [:reg-a :reg-l] :mnem "LD A,L"    :len 1 :dur 4 :flags nil}
   0x7E {:op :unimpl :args []              :mnem "LD A,[HL]" :len 1 :dur 4 :flags nil}
   0x7F {:op ld-r-r' :args [:reg-a :reg-a] :mnem "LD A,A"    :len 1 :dur 4 :flags nil}


   ;; ------------------------
   ;; 8-bit arithmetic / logic
   ;; ------------------------
   0x80 {:op :unimpl :args [:reg-a :reg-b] :mnem "ADD A,B"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x81 {:op :unimpl :args [:reg-a :reg-c] :mnem "ADD A,C"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x82 {:op :unimpl :args [:reg-a :reg-d] :mnem "ADD A,D"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x83 {:op :unimpl :args [:reg-a :reg-e] :mnem "ADD A,E"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x84 {:op :unimpl :args [:reg-a :reg-h] :mnem "ADD A,H"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x85 {:op :unimpl :args [:reg-a :reg-l] :mnem "ADD A,L"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x86 {:op :unimpl :args [:reg-a       ] :mnem "ADD A,[HL]" :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x87 {:op :unimpl :args [:reg-a :reg-a] :mnem "ADD A,A"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}

   0x88 {:op :unimpl :args [:reg-a :reg-b] :mnem "ADC A,B"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x89 {:op :unimpl :args [:reg-a :reg-c] :mnem "ADC A,C"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x8A {:op :unimpl :args [:reg-a :reg-d] :mnem "ADC A,D"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x8B {:op :unimpl :args [:reg-a :reg-e] :mnem "ADC A,E"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x8C {:op :unimpl :args [:reg-a :reg-h] :mnem "ADC A,H"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x8D {:op :unimpl :args [:reg-a :reg-l] :mnem "ADC A,L"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x8E {:op :unimpl :args [:reg-a       ] :mnem "ADC A,[HL]" :len 1 :dur 8 :flags [:calc 0     :calc :calc]}
   0x8F {:op :unimpl :args [:reg-a :reg-a] :mnem "ADC A,A"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}

   0x90 {:op :unimpl :args [:reg-a :reg-b] :mnem "SUB A,B"    :len 1 :dur 4 :flags [:calc 1     :calc :calc]}
   0x91 {:op :unimpl :args [:reg-a :reg-c] :mnem "SUB A,C"    :len 1 :dur 4 :flags [:calc 1     :calc :calc]}
   0x92 {:op :unimpl :args [:reg-a :reg-d] :mnem "SUB A,D"    :len 1 :dur 4 :flags [:calc 1     :calc :calc]}
   0x93 {:op :unimpl :args [:reg-a :reg-e] :mnem "SUB A,E"    :len 1 :dur 4 :flags [:calc 1     :calc :calc]}
   0x94 {:op :unimpl :args [:reg-a :reg-h] :mnem "SUB A,H"    :len 1 :dur 4 :flags [:calc 1     :calc :calc]}
   0x95 {:op :unimpl :args [:reg-a :reg-l] :mnem "SUB A,L"    :len 1 :dur 4 :flags [:calc 1     :calc :calc]}
   0x96 {:op :unimpl :args [:reg-a       ] :mnem "SUB A,[HL]" :len 1 :dur 8 :flags [:calc 1     :calc :calc]}
   0x97 {:op :unimpl :args [:reg-a :reg-a] :mnem "SUB A,A"    :len 1 :dur 4 :flags [1     1     0     0]}

   0x98 {:op :unimpl :args [:reg-a :reg-b] :mnem "SBC A,B"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x99 {:op :unimpl :args [:reg-a :reg-c] :mnem "SBC A,C"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x9A {:op :unimpl :args [:reg-a :reg-d] :mnem "SBC A,D"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x9B {:op :unimpl :args [:reg-a :reg-e] :mnem "SBC A,E"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x9C {:op :unimpl :args [:reg-a :reg-h] :mnem "SBC A,H"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x9D {:op :unimpl :args [:reg-a :reg-l] :mnem "SBC A,L"    :len 1 :dur 4 :flags [:calc 0     :calc :calc]}
   0x9E {:op :unimpl :args [:reg-a       ] :mnem "SBC A,[HL]" :len 1 :dur 8 :flags [:calc 0     :calc :calc]}
   0x9F {:op :unimpl :args [:reg-a :reg-a] :mnem "SBC A,A"    :len 1 :dur 4 :flags [:calc 0     :calc nil]}

   0xA0 {:op :unimpl :args [:reg-a :reg-b] :mnem "AND A,B"    :len 1 :dur 4 :flags [:calc 0     1     0]}
   0xA1 {:op :unimpl :args [:reg-a :reg-c] :mnem "AND A,C"    :len 1 :dur 4 :flags [:calc 0     1     0]}
   0xA2 {:op :unimpl :args [:reg-a :reg-d] :mnem "AND A,D"    :len 1 :dur 4 :flags [:calc 0     1     0]}
   0xA3 {:op :unimpl :args [:reg-a :reg-e] :mnem "AND A,E"    :len 1 :dur 4 :flags [:calc 0     1     0]}
   0xA4 {:op :unimpl :args [:reg-a :reg-h] :mnem "AND A,H"    :len 1 :dur 4 :flags [:calc 0     1     0]}
   0xA5 {:op :unimpl :args [:reg-a :reg-l] :mnem "AND A,L"    :len 1 :dur 4 :flags [:calc 0     1     0]}
   0xA6 {:op :unimpl :args [:reg-a       ] :mnem "AND A,[HL]" :len 1 :dur 8 :flags [:calc 0     1     0]}
   0xA7 {:op :unimpl :args [:reg-a :reg-a] :mnem "AND A,A"    :len 1 :dur 4 :flags [:calc 0     1     0]}

   0xA8 {:op :unimpl :args [:reg-a :reg-b] :mnem "XOR A,B"    :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xA9 {:op :unimpl :args [:reg-a :reg-c] :mnem "XOR A,C"    :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xAA {:op :unimpl :args [:reg-a :reg-d] :mnem "XOR A,D"    :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xAB {:op :unimpl :args [:reg-a :reg-e] :mnem "XOR A,E"    :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xAC {:op :unimpl :args [:reg-a :reg-h] :mnem "XOR A,H"    :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xAD {:op :unimpl :args [:reg-a :reg-l] :mnem "XOR A,L"    :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xAE {:op :unimpl :args [:reg-a       ] :mnem "XOR A,[HL]" :len 1 :dur 8 :flags [:calc 0     0     0]}
   0xAF {:op :unimpl :args [:reg-a :reg-a] :mnem "XOR A,A"    :len 1 :dur 4 :flags [1     0     1     0]}

   0xB0 {:op :unimpl :args [:reg-a :reg-b] :mnem "OR A,B"     :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xB1 {:op :unimpl :args [:reg-a :reg-c] :mnem "OR A,C"     :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xB2 {:op :unimpl :args [:reg-a :reg-d] :mnem "OR A,D"     :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xB3 {:op :unimpl :args [:reg-a :reg-e] :mnem "OR A,E"     :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xB4 {:op :unimpl :args [:reg-a :reg-h] :mnem "OR A,H"     :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xB5 {:op :unimpl :args [:reg-a :reg-l] :mnem "OR A,L"     :len 1 :dur 4 :flags [:calc 0     0     0]}
   0xB6 {:op :unimpl :args [:reg-a       ] :mnem "OR A,[HL]"  :len 1 :dur 8 :flags [:calc 0     0     0]}
   0xB7 {:op :unimpl :args [:reg-a :reg-a] :mnem "OR A,A"     :len 1 :dur 4 :flags [:calc 0     0     0]}

   0xB8 {:op :unimpl :args [:reg-a :reg-b] :mnem "CP A,B"     :len 1 :dur 4 :flags [:calc :calc :calc :calc]}
   0xB9 {:op :unimpl :args [:reg-a :reg-c] :mnem "CP A,C"     :len 1 :dur 4 :flags [:calc :calc :calc :calc]}
   0xBA {:op :unimpl :args [:reg-a :reg-d] :mnem "CP A,D"     :len 1 :dur 4 :flags [:calc :calc :calc :calc]}
   0xBB {:op :unimpl :args [:reg-a :reg-e] :mnem "CP A,E"     :len 1 :dur 4 :flags [:calc :calc :calc :calc]}
   0xBC {:op :unimpl :args [:reg-a :reg-h] :mnem "CP A,H"     :len 1 :dur 4 :flags [:calc :calc :calc :calc]}
   0xBD {:op :unimpl :args [:reg-a :reg-l] :mnem "CP A,L"     :len 1 :dur 4 :flags [:calc :calc :calc :calc]}
   0xBE {:op :unimpl :args [:reg-a       ] :mnem "CP A,[HL]"  :len 1 :dur 8 :flags [:calc :calc :calc :calc]}
   0xBF {:op :unimpl :args [:reg-a :reg-a] :mnem "CP A,A"     :len 1 :dur 4 :flags [1     1     0     0]}

  ;; -------
  ;; Various
  ;; -------
   0xC0 {:op :unimpl :args []              :mnem "RET NZ"      :len 1 :dur [20 8]  :flags nil}
   0xC1 {:op :unimpl :args [:reg-b :reg-c] :mnem "POP BC"      :len 1 :dur 12      :flags nil}
   0xC2 {:op :unimpl :args []              :mnem "JP NZ,a16"   :len 3 :dur [16 12] :flags nil}
   0xC3 {:op :unimpl :args []              :mnem "JP a16"      :len 1 :dur 16      :flags nil}
   0xC4 {:op :unimpl :args []              :mnem "CALL NZ,a16" :len 3 :dur [24 12] :flags nil}
   0xC5 {:op :unimpl :args []              :mnem "PUSH BC"     :len 1 :dur 16      :flags nil}
   0xC6 {:op :unimpl :args []              :mnem "ADD A,n8"    :len 2 :dur 8       :flags [:calc 0 :calc :calc]}
   0xC7 {:op :unimpl :args []              :mnem "RST $00"     :len 1 :dur 16      :flags nil}

   0xC8 {:op :unimpl :args []              :mnem "RET Z"       :len 1 :dur [20 8]  :flags nil}
   0xC9 {:op :unimpl :args []              :mnem "RET"         :len 1 :dur 16      :flags nil}
   0xCA {:op :unimpl :args []              :mnem "JP Z,a16"    :len 3 :dur [16 12] :flags nil}
   0xCB {:op :unimpl :args []              :mnem "PREFIX"      :len 1 :dur 4       :flags nil}
   0xCC {:op :unimpl :args []              :mnem "CALL Z,a16"  :len 3 :dur [24 12] :flags nil}
   0xCD {:op :unimpl :args []              :mnem "CALL a16"    :len 3 :dur 24      :flags nil}
   0xCE {:op :unimpl :args []              :mnem "ADC A,n8"    :len 2 :dur 8       :flags [:calc 0 :calc :calc]}
   0xCF {:op :unimpl :args []              :mnem "RST $08"     :len 1 :dur 16      :flags nil}

   0xD0 {:op :unimpl :args []              :mnem "RET NC"      :len 1 :dur [20 8]  :flags nil}
   0xD1 {:op :unimpl :args [:reg-d :reg-e] :mnem "POP DE"      :len 1 :dur 12      :flags nil}
   0xD2 {:op :unimpl :args []              :mnem "JP NC,a16"   :len 3 :dur [16 12] :flags nil}
   ;; There is no 0xD3 instruction
   0xD4 {:op :unimpl :args []              :mnem "CALL NC,a16" :len 3 :dur [24 12] :flags nil}
   0xD5 {:op :unimpl :args []              :mnem "PUSH DE"     :len 1 :dur 16      :flags nil}
   0xD6 {:op :unimpl :args []              :mnem "SUB A,n8"    :len 2 :dur 8       :flags [:calc 1 :calc :calc]}
   0xD7 {:op :unimpl :args []              :mnem "RST $10"     :len 1 :dur 16      :flags nil}

   0xD8 {:op :unimpl :args []              :mnem "RET C"       :len 1 :dur [20 8]  :flags nil}
   0xD9 {:op :unimpl :args []              :mnem "RETI"        :len 1 :dur 16      :flags nil}
   0xDA {:op :unimpl :args []              :mnem "JP C,a16"    :len 3 :dur [16 12] :flags nil}
   ;; There is no 0xDB instruction
   0xDC {:op :unimpl :args []              :mnem "CALL C,a16"  :len 3 :dur [24 12] :flags nil}
   ;; There is no 0xDD instruction
   0xDE {:op :unimpl :args []              :mnem "SBC A,n8"    :len 2 :dur 8       :flags [:calc 1 :calc :calc]}
   0xDF {:op :unimpl :args []              :mnem "RST $18"     :len 1 :dur 16      :flags nil}

   0xE0 {:op :unimpl :args []              :mnem "LD [a8],A"   :len 2 :dur 12      :flags nil}
   0xE1 {:op :unimpl :args []              :mnem "POP HL"      :len 1 :dur 12      :flags nil}
   0xE2 {:op :unimpl :args []              :mnem "LD [C],A"    :len 1 :dur 8       :flags nil}
   ;; There is no 0xE3 instruction
   ;; There is no 0xE4 instruction
   0xE5 {:op :unimpl :args []              :mnem "PUSH HL"     :len 1 :dur 16      :flags nil}
   0xE6 {:op :unimpl :args []              :mnem "AND A,n8"    :len 2 :dur 8       :flags [:calc 0 1 0]}
   0xE7 {:op :unimpl :args []              :mnem "RST $20"     :len 1 :dur 16      :flags nil}
   0xE8 {:op :unimpl :args []              :mnem "ADD SP,e8"   :len 2 :dur 16      :flags [0 0 :calc :calc]}
   0xE9 {:op :unimpl :args []              :mnem "JP HL"       :len 1 :dur 4       :flags nil}
   0xEA {:op :unimpl :args []              :mnem "LD [a16],A"  :len 3 :dur 16      :flags nil}
   ;; There is no 0xEB
   ;; There is no 0xEC
   ;; There is no 0xED
   0xEE {:op :unimpl :args []              :mnem "XOR A,n8"    :len 2 :dur 8       :flags [:calc 0 0 0]}
   0xEF {:op :unimpl :args []              :mnem "RST $28"     :len 1 :dur 16      :flags nil}


   0xF0 {:op :unimpl :args []              :mnem "LD A,[a8]"   :len 2 :dur 12      :flags nil}
   0xF1 {:op :unimpl :args []              :mnem "POP AF"      :len 1 :dur 12      :flags [:calc :calc :calc :calc]}
   0xF2 {:op :unimpl :args []              :mnem "LD A,[C]"    :len 1 :dur 8       :flags nil}
   0xF3 {:op :unimpl :args []              :mnem "DI"          :len 1 :dur 4       :flags nil}
   ;; There is no 0xF4 instruction
   0xF5 {:op :unimpl :args []              :mnem "PUSH AF"     :len 1 :dur 16      :flags nil}
   0xF6 {:op :unimpl :args []              :mnem "OR A,n8"     :len 2 :dur 8       :flags [:calc 0 0 0]}
   0xF7 {:op :unimpl :args []              :mnem "RST $30"     :len 1 :dur 16      :flags nil}
   0xF8 {:op :unimpl :args []              :mnem "LD HL,SP+e8" :len 2 :dur 12      :flags [0 0 :calc :calc]}
   0xF9 {:op :unimpl :args []              :mnem "LD SP,HL"    :len 1 :dur 8       :flags nil}
   0xFA {:op :unimpl :args []              :mnem "LD A,[a16]"  :len 3 :dur 16      :flags nil}
   0xFB {:op :unimpl :args []              :mnem "EI"          :len 1 :dur 4       :flags nil}
   ;; There is no 0xFC
   ;; There is no 0xFD
   0xFE {:op :unimpl :args []              :mnem "CP A,n8"     :len 2 :dur 8       :flags [:calc 1 :calc :calc]}
   0xFF {:op :unimpl :args []              :mnem "RST $38"     :len 1 :dur 16      :flags nil}})
