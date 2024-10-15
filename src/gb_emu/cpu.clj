(ns gb-emu.cpu)

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
         :reg-f 0x00 ;; Flags
         :reg-h 0x00
         :reg-l 0x00
         :stack-ptr 0x0000 ;; Loc. in address-space
         :pgm-ctr 0x0000}))

(defn get-register
  "Get the value of the supplied register."
  [cpu, reg-key]
  (get @cpu reg-key))

(defn set-register
  [cpu, reg-key, value]
  (swap! cpu assoc reg-key value))

(def byte-to-opcode-mnemonic
  {0x00 "NOP"          ;; No-operation
   0x01 "LD BC,d16"    ;; Load immediate value (u16) into BC
   0x02 "LD (BC),A"    ;; Load A into memory at addr BC
   0x03 "INC BC"       ;; Increment BC
   0x04 "INC B"        ;; Increment B
   0x05 "DEC B"        ;; Decrement B
   0x06 "LD B,d8"      ;; Load immediate value (u8) into B
   0x07 "RLCA"         ;; TODO
   0x08 "LD (a16),SP"  ;; Load the stack pointer's value into the current address
   0x09 "ADD HL,BC"    ;; Add BC
   0x0A "LD A,(BC)"
   0x0B "DEC BC"
   0x0C "INC C"
   0x0D "DEC C"
   0x0E "LD C,d8"
   0x0F "RRCA"})

(defn register-xform
  "Transform a register's value by applying xform to it
   and wrapping the value at 8-bit integer max."
  [cpu reg xform]
  (swap! cpu update reg
         (fn [val] (mod (+ (xform val) 0x100) 0x100)))
  @cpu)

(defn dec-b
  [cpu]
  (register-xform cpu :reg-b dec))

(defn inc-b
  [cpu]
  (register-xform cpu :reg-b inc))

(defn inc-bc
  [cpu]
  (register-xform cpu :reg-b inc)
  (register-xform cpu :reg-c inc))


(def op-code-to-op
  {0x00 nil
   0x01 nil
   0x02 nil
   0x03 nil
   0x04 'inc-b
   0x05 'dec-b})

(defn lookup-and-exec
  [cpu byte]
   (let [op (get op-code-to-op byte)]
     (if (not (nil? op))
       (eval (op cpu))
       nil))
  @cpu)
