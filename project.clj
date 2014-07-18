(defproject snake-httpkit "0.1.0-SNAPSHOT"
  :description "Multiplayer snake with httpkit"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-ring "0.8.10"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [http-kit "2.1.16"]
                 [compojure "1.1.8"]
                 [javax.servlet/servlet-api "2.5"]
                 [ring/ring-devel "1.3.0"]
                 [ring/ring-core "1.3.0"]
                 [cheshire "5.3.1"]]
  :main snake-httpkit.core)
