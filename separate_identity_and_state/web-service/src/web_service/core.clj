(ns web-service.core
  (:require [web-service.sentences :refer [strings->sentences]]
            [web-service.charset   :refer [wrap-charset]]
            [web-service.translate :refer [translate]]
            [web-service.session   :refer [new-session get-session]]
            [compojure.core        :refer :all]
            [compojure.handler     :refer [api]]
            [ring.util.response    :refer [charset response]]
            [ring.adapter.jetty    :refer [run-jetty]]))

(defn create-session []
  (let [snippets (repeatedly promise)
        translations (delay (map translate
                                 (strings->sentences (map deref snippets))))]
    (new-session {:snippets snippets :translations translations})))

(defn accept-snippet [session n text]
  (deliver (nth (:snippets session) n) text))

(defn get-translation [session n]
  @(nth @(:translations session) n))

(defroutes app-routes
  (POST "/session/create" []
    (response (str (create-session))))
	
  (context "/session/:session-id" [session-id]
    (let [session (get-session (edn/read-string session-id))]
      (routes
        (PUT "/snippet/:n" [n :as {:keys [body]}]
          (accept-snippet session (edn/read-string n) (slurp body))
          (response "OK"))
		  
        (GET "/translation/:n" [n]
          (response (get-translation session (edn/read-string n))))))))

(defn -main [& args]
  (run-jetty (wrap-charset (api app-routes)) {:port 3000}))