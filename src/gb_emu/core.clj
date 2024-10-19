(ns gb-emu.core
  (:gen-class)
  (:require [gb-emu.cpu :as cpu])
  (:require [gb-emu.memory :as memory]))

;; CPU clock speed is 4.194304MHz
(def CLOCK-SPEED 4194304)

(def emulator
  {:cpu    (cpu/create-cpu)
   :memory (memory/create-memory)
   :paused? false
   :halted? false
   :cycle   0})


(defn -main
  [& args]
  ())
