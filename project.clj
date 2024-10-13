(defproject gb-emu "0.1.0-SNAPSHOT"
  :description "Gameboy emulator"
  :license {:name "GPL-3.0-only"
            :url "https://www.gnu.org/licenses/gpl-3.0.txt"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main ^:skip-aot gb-emu.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
