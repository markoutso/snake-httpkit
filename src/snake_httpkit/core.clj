(ns snake-httpkit.core
  (:gen-class)
  (:use org.httpkit.server
        [compojure.handler :only [site]]
        [compojure.core :only [defroutes GET]]
        [cheshire.core :only [generate-string parse-string]]
        [clojure.set :only [union difference]]
        [clojure.string :only [split]])
  (:require [compojure.route :as route]
            [ring.middleware.reload :as reload]))


(defn make-state []
  {:snakes {}
   :apples #{}
   :channels {}
   :id-counter 1})

(defonce state (atom (make-state)))

(def server (atom nil))

(def w 40)
(def h 40)

(defn transpose [m]
  (apply map vector m))


(defn distinct-random [n q]
  (take q (distinct (repeatedly #(rand-int (max n q))))))

(defn rand-points [state q]
  (let [taken (union
               (set (mapcat :body (vals (:snakes state))))
               (:apples state))
        coll (for [i (range w)
                   j (range h) :when (not (contains? taken [i j]))]
               [i j])
        c (count coll)]
    (if (= c 0) nil) (map #(nth coll %) (distinct-random c q))))

(defn rand-point [state]
  (when-let [s (rand-points state 1)]
    (first s)))


(defn update-grid [grid points values]
  (if (seq? values)
    (reduce (fn [grid [point value]] (assoc-in grid point value)) grid (transpose [points values]))
    (reduce (fn [grid point] (assoc-in grid point values)) grid points)))

(defn make-grid [] (vec (repeat w (vec (repeat h nil)))))

(defn reset-grid [state]
  (let [grid (make-grid)
        grid (update-grid grid (:apples state) :apple)
        grid (reduce (fn [grid [id snake]] (update-grid grid (:body snake) id)) grid (:snakes state))]
    grid))


(def dirs {:left [-1 0] :right [1 0] :up [0 -1] :down [0 1]})


(defn make-snake [p id]
  {:body [p]
   :direction (rand-nth [:up :down :left :right])
   ;:alive? true
   :score 0
   :id id})

(defn find-key [m val]
  (let [seq (filter (fn [[k v]] (= val v)) m)]
    (when-let [f (first seq)]
      (key f))))


(defn chan-id [channel]
  (second (split (str channel) #"<->")))


(defn open [state channel]
  (let [[apple head] (rand-points state 2)
        new-id (inc (:id-counter state))]
    (-> state
        (assoc-in [:channels new-id] channel)
        (update-in [:apples] conj apple)
        (assoc-in [:snakes new-id] (make-snake head new-id))
        (assoc :id-counter new-id))))



(def invalid-dirs {:left :right :right :left :up :down :down :up})

(defn valid-dir [current new]
  (not= new (invalid-dirs current)))

(defn open! [channel]
  (swap! state (fn [state] (open state channel))))

(defn close! [channel]
  (comment (swap! state update-in [:client-map] dissoc (chan-id channel))))

(defn turn! [channel dir]
  (swap! state (fn [state]
                 (let [id (find-key (:channels state) channel)
                       snake ((:snakes state) id)]
                   (if (and snake
                            (valid-dir (:direction snake) dir))                  
                     (update-in state [:snakes id] assoc :direction dir)
                     state)))))


(defn add-points [& pts]
  (vec (apply map + pts)))

(defn reset-coord [val length]
  (let [limit (dec length)]
    (cond (< val 0) limit
          (> val limit) 0
          :else val)))

(defn reset-point [[x y]]
  [(reset-coord x w) (reset-coord y h)])

(defn destruct-body [snake]
  (let [body (:body snake)]
    [(last body) (butlast body) body]))

(defn next-point [snake]
  (let [[head & rest] (destruct-body snake)]
    (reset-point (add-points head ((:direction snake) dirs)))))

(defn twice+ [seq]
  (second
   (reduce
    (fn [[unique twice] el]
      (if (unique el)
        [unique (conj twice el)]
        [(conj unique el) twice]))
    [#{} #{}] seq)))

(defn remove-dead [state]
  (let [all-points (mapcat :body (vals (:snakes state)))
        all-set (set all-points)
        doubles (twice+ all-points)
        dies? (fn [snake]                
                (let [[head tail _] (destruct-body snake)
                      next-head (next-point snake)]
                  (or (contains? (set tail) head)
                      (contains? doubles head)
                      (contains? all-set next-head))))
        {alive false dead true :or {alive [] dead []}} (group-by (comp dies? val) (:snakes state))]
    (assoc state :snakes (into {} alive))))

(defn update-head [state]
  (let [snakes (vals (:snakes state))
        new-points (map next-point snakes)
        new-snakes (map (fn [snake point] (update-in snake [:body] conj point)) snakes new-points)]
    (assoc state :snakes (into {} (map (fn [s] [(:id s) s]) new-snakes)))))

(defn update-tails-scores [state]
  (let [apples (:apples state)
        new-snakes (into {} (map (fn [[id snake]]
                                   (let [ate? (apples (last (:body snake)))]
                                     [id
                                      (-> snake
                                          (assoc :body (if ate? (:body snake) (vec (rest (:body snake)))))
                                          (assoc :score (if ate? (inc (:score snake)) (:score snake))))
                                      ]))
                                 (:snakes state)))]
    (assoc state :snakes new-snakes)))

(defn update-apples [state]
  (let [valid (count (:snakes state))
        total (count (:apples state))
        eaten (set (keep (fn [snake] ((:apples state) (last (:body snake)))) (vals (:snakes state))))
        base (difference (:apples state) eaten)]
    (let [new-apples
          (if (< (- total (count eaten)) valid)
            (union base (set (rand-points state (- valid total))))
            base)]
      (assoc state :apples new-apples))))

(defn delete-apples [state]
  (if (and (empty? (:snakes state))
           (not (empty? (:apples state))))
    (assoc state :apples #{})
    state))


(defn remove-closed [state]
  (reduce (fn [state [id channel]]
            (if (not (open? channel))
              (-> state
                  (update-in [:channels] dissoc id)
                  (update-in [:snakes] dissoc id))
              state))
          state (:channels state)))

(defn reset-state [state]
  (if (empty? (:snakes state))
    (reduce (fn [state [id channel]]
              (if (open? channel)
                (open state channel)
                state))
            (make-state) (:channels state))
    state))

(defn update []
  (swap! state (fn [state]
                 (-> state
                     remove-closed
                     delete-apples
                     remove-dead
                     reset-state
                     update-head
                     update-tails-scores
                     update-apples))))


(def updater (agent nil))

(defn send-to-all [obj]
  (doseq [[id c] (:channels @state)]
    (let [obj (assoc obj :id id)]
      (send! c (generate-string obj)))))

(defn update-channels [x]
  (try    
    (update)
    (let [obj {:grid (reset-grid @state)}]
      (send-to-all obj))
    (send-off *agent* #'update-channels)
    (catch Exception e (.printStackTrace e)))
  (Thread/sleep 80)
  nil)


(defn show-landing-page [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<html><head><script src=\"snake.js\"></script></head></html>"})

(defn stop-server []
  (when-not (nil? server)
    (@server :timeout 100)
    (reset! server nil)))

(defn handler [req]
  (with-channel req channel
    (open! channel)
    (on-close channel (close! channel))
    (on-receive channel (fn [data]
                          (try
                            (let [d (parse-string data true)]
                              (turn! channel (keyword (:turn d))))
                            (catch Exception e (.printStackTrace e)))))))

(defroutes all-routes
  (GET "/ws" [] #'handler)
  (GET "/" [] show-landing-page)
  (route/resources "/" {:root "public"})
  (route/not-found "page not found"))


(defn in-dev? [& args] true) ;; TODO read a config variable from command line, env, or file?

(defn -main [& args]
  (let [handler (if (in-dev? args)
                  (reload/wrap-reload (site #'all-routes)) ;; only reload when dev
                  (site all-routes))
        srv (run-server handler {:port 8080})]
    (send-off updater #'update-channels)
    (reset! server srv)))
