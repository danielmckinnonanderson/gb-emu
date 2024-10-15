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
         :reg-f 2r0000 ;; Flags
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

(defn old-xform-reg
  "Transform a register's value by applying xform to it
   and wrapping the value at 8-bit integer max, or when going below zero.
   Expects `cpu` to be of type Atom<Map>.
   Return a vector of values in shape (reg-key, old val, new val, xform)"
  [cpu reg xform]
  (swap! cpu update reg
         (fn [val] (mod (+ (xform val) 0x100) 0x100)))
  @cpu)

(defn wrap-arith [val]
  (mod (+ val 0x100) 0x100))

(defn op-to-result
  "Return a shape in the form of a vector of vectors
   where each vector is `(register old-val new-val xform)"
  [cpu reg xform]
  (let [old-val (get @cpu reg)]
    [[reg old-val (wrap-arith (xform old-val)) xform]]))

(defn dec-b [cpu]
  (old-xform-reg cpu :reg-b dec))

(defn inc-b [cpu]
  (old-xform-reg cpu :reg-b inc))

(defn inc-bc [cpu]
  (old-xform-reg cpu :reg-b inc)
  (old-xform-reg cpu :reg-c inc))

(defn dec-bc [cpu]
  (old-xform-reg cpu :reg-b dec)
  (old-xform-reg cpu :reg-c dec))


(def op-code-to-op
  {0x00 (fn [cpu] cpu)
   0x01 (fn [cpu] cpu)
   0x02 (fn [cpu] cpu)
   0x03 inc-bc 
   0x04 inc-b
   0x05 dec-b
   0x0B dec-bc})

(defn op-carry?
  "Predicate to determine if an operation caused a carry.
   A carry occurs when:
   - The result of an 8-bit addition is higher than 0xFF
   - The result of a 16-bit addition is higher than 0xFFFF
   - The result of a subtraction or comparison is lower than zero
   - When a rotate or shift operation shifts out a 1 bit"
  [old new]
  ())

(defn op-zeroed?
  "Predicate to determine if an operation resulted in zero"
  [_old new]
  (zero? new))

(defn op-half-carry?
  "Predicate to determine if there is a carry
   from bit 3 to bit 4 when moving from old-val to new-val.
   Assumes that both old-val and new-val are 8-bit integers."
  [old-val new-val]
  (let [mask 0x0F
        old-nibble (bit-and old-val mask)
        new-nibble (bit-and new-val mask)]
    (< old-nibble new-nibble)))


(defn lookup-and-exec
  [cpu byte]
   ;; TODO - Insert something here that updates the flags
   (let [op (get op-code-to-op byte)]
         (eval (op cpu)))
  cpu)

