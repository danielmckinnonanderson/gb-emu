(ns gb-emu.cpu)

(defn create-cpu
  "Create a new CPU with all 8-bit registers set to 0.
   A CPU has registers A, B, C, D, E, F, H, and L.
   The stack pointer is a 16-bit register, initialized to 0.
   The program counter is a 16-bit register,linitialized
   to 0."
  []
  (atom {:reg-a 0x00
         :reg-b 0x00
         :reg-c 0x00
         :reg-d 0x00
         :reg-e 0x00
         :reg-f 0x00
         :reg-h 0x00
         :reg-l 0x00
         :stack-ptr 0x0000
         :pgm-ctr 0x0000}))

(defn get-register
  "Get the value of the supplied register."
  [cpu, reg]
  (get @cpu reg))
