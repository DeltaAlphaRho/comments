(ns comments.core
  (:require [cemerick.austin.repls :refer (browser-connected-repl-js)]
            [net.cgrand.enlive-html :as html]
            [net.cgrand.reload :as reload]
            [compojure.handler :as handler]
            [compojure.route :refer (resources not-found)]
            [compojure.core :as compojure :refer (GET POST defroutes context)]
            ring.adapter.jetty
            [ring.util.response :as resp]
            [ring.middleware.params :refer (wrap-params)]
            [ring.middleware.nested-params :refer (wrap-nested-params)]
            [ring.middleware.keyword-params :refer (wrap-keyword-params)]
            [ring.middleware.session :refer (wrap-session)]
            [ring.middleware.session.store :refer (read-session)]
            [ring.middleware.reload :refer (wrap-reload)]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [cemerick.friend :as friend]
            (cemerick.friend [workflows :as workflows]
                             [credentials :as creds]))
  (:import java.net.URI)
  (:gen-class))

(reload/auto-reload *ns*) ; To automatically reload Enlive templates -
                          ; wrap-reload used below in handler

;;; Friend atom and accessor functions

(def users (atom {"friend@gmail.com" {:username "friend@gmail.com" 
                                      :password (creds/hash-bcrypt "clojure")
                                      :roles #{::commenter}}
                  "admin@example.com" {:username "admin@example.com"
                                       :password (creds/hash-bcrypt "admin")
                                       :roles #{::admin}}
                  "moderator@example.com" {:username "moderator@example.com"
                                           :password (creds/hash-bcrypt "moderator")
                                           :roles #{::moderator}}}))

(derive ::admin ::moderator)
(derive ::moderator ::commenter)

(defn check-user [{:keys [username password roles]}] ; strong password, non-blank username, doesn't already exist
  (and (not (nil? (re-matches #"^(?=.*\d)(?=.*[a-zA-Z]).{7,50}$" password)))
       (not (str/blank? username))
       (not (contains? @users username))
       (find-keyword (str *ns*) roles)))

(defn sanitize-user
  [{:keys [username password roles] :as user-data}]
  (let [lower-case-username (str/lower-case username)]
    (->  user-data (assoc :username lower-case-username
                          :password (creds/hash-bcrypt password)
                          :roles #{(keyword (str *ns*) roles)}))))
(defn add-user [user]
  (let [check (check-user user)]
   (if check
     (let [sanitized-user (sanitize-user user)
           username (:username sanitized-user)]
       (swap! users #(assoc-in % [username] sanitized-user))))))


(defn get-friend-username [req]
  (:username (friend/current-authentication req)))

(defn trim-email-address [email] (first (re-find #"(\S)+(?=@)" email)))

;;; Navigation, Templating, and Snippets

(def navigation-items  (array-map  "Home" "/" "About" "/about" "Contact" "/contact"))

(def navigation-items-user (array-map "Home" "/" "About" "/about" "Contact" "/contact" "App" "/welcome"))

(def navigation-items-invert (set/map-invert navigation-items))

(defn get-navigation-caption [req] (navigation-items-invert (req :uri)))

(html/defsnippet auth-profile (io/resource "public/welcome.html")
  [:body :div.user]
  [req]
  [#{:span.user}] (html/content (trim-email-address (get-friend-username req))))

(html/defsnippet navbar (io/resource "public/landing.html")
  [:body :div.navbar]
  [req]  
  [:ul [:li html/first-of-type]] (if (friend/identity req)
                                   ;users see the app item in their menu
                                   (html/clone-for [[caption uri] navigation-items-user]
                                                   [:li] (if (= (req :uri) uri)
                                                           (html/set-attr :class "active")
                                                           identity)
                                                   [:li :a] (html/content caption)
                                                   [:li :a] (html/set-attr :href uri))
                                   ;anonymous users do not see the app
                                   (html/clone-for [[caption uri] navigation-items]
                                                   [:li] (if (= (req :uri) uri)
                                                           (html/set-attr :class "active")
                                                           identity)
                                                   [:li :a] (html/content caption)
                                                   [:li :a] (html/set-attr :href uri)))
  [:div.sign-in-form] (if (friend/identity req) (html/substitute (auth-profile req)) identity))

(html/defsnippet non-app-content (io/resource "public/landing.html")
  [:#content]
  [req]
  [:#content] (case (get-navigation-caption req) 
                "Home" (html/set-attr :id "content") 
                "About" (html/do-> (html/content "See what we're all about")
                                   (html/wrap :h2))
                "Contact" (html/do-> (html/content "Learn how to make contact")
                                     (html/wrap :h2))
                (html/do-> (html/content  "Best check yo self, page not found!")
                                   (html/wrap :h2 {:class "alert alert-warning" :style "text-align: center;"}))))

(html/deftemplate landing (io/resource "public/landing.html")
  [req]
  [:body :div.navbar] (html/substitute (navbar req))
  [:body :#content] (html/substitute (non-app-content req))
  [:body] (html/append (html/html [:script (browser-connected-repl-js)]))
  )

;;; Default page for erroneous logins 
(html/deftemplate login (io/resource "public/landing.html")
  [req]
  [:body :div.navbar] (html/substitute (navbar req))
  ;[:body :#content] (html/substitute (non-app-content req))
  [:body] (html/append (html/html [:script (browser-connected-repl-js)]))
  [:body :div.navbar :input] (html/set-attr :style "color: red")
  [:body :div.navbar :input.username] (html/set-attr :placeholder "Re-enter Email Address")
  [:body :div.navbar :input.password] (html/set-attr :placeholder "Re-enter Password"))

;;; Page for erroneous registrations
(html/deftemplate reregister (io/resource "public/landing.html")
  [req]
  [:body :div.navbar] (html/substitute (navbar req))
  [:body :#content :form :input] (html/set-attr :class "input-block-level btn-lg register alert-danger")
  [:body :div.navbar :ul [:li html/first-of-type]] (html/set-attr :class "active")
  [:body] (html/append (html/html [:script (browser-connected-repl-js)])))

;;; App pages
(html/deftemplate welcome (io/resource "public/welcome.html")
  [req]
  [:body :div.navbar] (html/substitute (navbar req))
  [:body] (html/append
           (html/html [:script (browser-connected-repl-js)]))
  [#{:span.user}] (html/content (trim-email-address (get-friend-username req) )))

(html/deftemplate moderate (io/resource "public/moderate.html")
  [req])

(html/deftemplate admin (io/resource "public/admin.html")
  [req])

(defn admin-register [{:keys [username password roles] :as user}]
  (add-user user))

;;; Logging/Debugging
(defn log-request [req]
  (println ">>>>" req)
  (println "users" @users)) 

(defn wrap-verbose [h]
  (fn [req]
    (log-request req)
    (h req)))

;;; Compjure routes, site handler, ring server
(defroutes admin-routes
  (GET "/" req (admin req))
  (POST "/register" req
    (admin-register (:params req))
    (resp/redirect "/admin")))

(defroutes unsecured-site
  (resources "/")
  (GET "/" req (landing req))
  (GET "/about" req (landing req))
  (GET "/contact" req (landing req))
  (GET "/welcome" req
    ;(println "welcome req:" req)
    ;(println "(:user (req :params))" (:username (req :params)))
    ;(println "user name extraction: " (:authentications (:cemerick.friend/identity (:session req))))
    (friend/authenticated  (welcome req)))
  (GET "/login" req (login req))
  (GET "/logout" req (friend/logout* (resp/redirect "/")))
  (GET "/reregister" req (reregister req))
  (POST "/register" [username password]
    (let [roles "commenter"]
      (if (check-user {:username username :password password :roles roles})
        (let [user (sanitize-user :username username :password password :roles roles)]        
          (swap! users #(-> % (assoc (str/lower-case username) user))) ; (println "user is " user)        
          (friend/merge-authentication (resp/redirect "/welcome") user)) ; (println "register redirect req: " req)
        (resp/redirect "/reregister"))))  
  (context "/admin" req
    (friend/wrap-authorize admin-routes #{::admin}))
  (GET "/moderate" req (friend/authorize #{::moderator} (moderate req)))
  (not-found (landing {:uri "PageNotFound"}))) 
 
(def secured-site
  (-> unsecured-site
      (friend/authenticate {:allow-anon? true
                            :default-landing-uri "/welcome"
                            :credential-fn #(creds/bcrypt-credential-fn @users %)
                            :workflows [(workflows/interactive-form)]})
      #_(wrap-verbose) ; Logging/Debugging
      ; required Ring middlewares
      (wrap-keyword-params)
      (wrap-nested-params)
      (wrap-params)
      (wrap-session)
      (wrap-reload)))

(defn -main []
  (defonce ^:private server
    (ring.adapter.jetty/run-jetty #'secured-site {:port 8080 :join? false}))
  server)
