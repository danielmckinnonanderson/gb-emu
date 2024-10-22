(ns gb-emu.core
  (:gen-class)
  (:require [gb-emu.cpu :as cpu :refer :all])
  (:require [gb-emu.memory :as memory :refer :all]) 
  (:import [clojure.lang PersistentHashMap]))

;; CPU clock speed is 4.194304MHz
(def CLOCK-SPEED 4194304)

(def emulator
  {:cpu    (cpu/create-cpu)
   :memory (memory/create-memory)
   :paused? false
   :halted? false
   :cycle   0})

(defn fetch-instruction
  [^PersistentHashMap emulator]
  (let [byte (get-register (:cpu emulator) :pgm-ctr)]
    byte))

(defn step
  [emulator]
  ())


(defn -main
  [& args]
  ())
