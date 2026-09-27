(ns hive-dirge.host-test
  "hive.dirge.host against hive-vessel's real SSE executor on loopback.
   hive.olympus is replaced by a RECORDING IOlympusControl passed through
   config (:dirge/olympus), or by a stub IAddon injected by the real mounter;
   no with-redefs anywhere."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-addon.mount :as mount]
            [hive-addon.mount.port :as mount-port]
            [hive-addon.protocol :as addon]
            [hive-dirge.host :as host]
            [hive-dirge.host.boundary :as boundary]
            [hive-dirge.host.domain :as domain]
            [hive-dirge.host.ports :as ports]
            [hive-vessel.executor.sse :as sse]
            [hive-vessel.wire :as wire])
  (:import (java.net URI)
           (java.net.http HttpClient HttpClient$Version HttpRequest
                          HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.nio.file Files LinkOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util.concurrent LinkedBlockingQueue TimeUnit)))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defrecord RecordingOlympus [calls]
  ports/IOlympusControl
  (focus! [_ agent-id] (swap! calls conj [:focus agent-id]) :focused)
  (next-tab! [_] (swap! calls conj [:next-tab]) :next)
  (prev-tab! [_] (swap! calls conj [:prev-tab]) :prev)
  (refresh! [_] (swap! calls conj [:refresh]) :refreshed))

(defn- recording [] (->RecordingOlympus (atom [])))

(defn- temp-discovery []
  (str (Files/createTempDirectory "hive-dirge-test" (make-array FileAttribute 0))
       "/run/hive-vessel/dirge.json"))

(defn- perms-of [path]
  (PosixFilePermissions/toString
   (Files/getPosixFilePermissions (.toPath (io/file path)) (make-array LinkOption 0))))

(def ^HttpClient client
  (-> (HttpClient/newBuilder) (.version HttpClient$Version/HTTP_1_1) (.build)))

(defn- discovery [path] (wire/read-json (slurp path)))

(defn- url [doc route & [token]]
  (str (get doc "url") route "?token=" (or token (get doc "token"))))

(defn- request [u & {:keys [method body headers]}]
  (let [b (reduce (fn [b [k v]] (.header b k v)) (HttpRequest/newBuilder (URI. u)) headers)
        b (if (= :post method)
            (.POST b (HttpRequest$BodyPublishers/ofString (or body "")))
            (.GET b))]
    (.send client (.build b) (HttpResponse$BodyHandlers/ofString))))

(defn- post-reply [doc body] (.statusCode (request (url doc "/reply") :method :post :body body)))

(defn- open-events
  "Subscribe to <url>/events; returns a queue of raw SSE lines."
  [doc]
  (let [q (LinkedBlockingQueue.)
        resp (.send client (.build (HttpRequest/newBuilder (URI. (url doc "/events"))))
                    (HttpResponse$BodyHandlers/ofLines))]
    (future (try (doseq [l (iterator-seq (.iterator (.body resp)))] (.put q l))
                 (catch Throwable _ nil)))
    q))

(defn- next-line [^LinkedBlockingQueue q] (.poll q 5 TimeUnit/SECONDS))

(defn- read-frame
  "The next complete SSE event from Q as {field value}, skipping comments."
  [q]
  (loop [acc {}]
    (let [l (next-line q)]
      (cond
        (nil? l) (when (seq acc) acc)
        (= "" l) (if (seq acc) acc (recur acc))
        (str/starts-with? l ":") (recur acc)
        :else (let [[k v] (str/split l #": ?" 2)] (recur (assoc acc k v)))))))

(defmacro with-host [[sym config] & body]
  `(let [~sym (host/addon-ctor ~config)]
     (try
       (is (:success? (addon/initialize! ~sym {})))
       ~@body
       (finally (addon/shutdown! ~sym)))))

(def panel {:op :ui/show-panel :panel/id "olympus/tab-1"
            :doc {:doc/title "Olympus" :doc/blocks [{:block/type :para :text "No active agents"}]}})

;; =============================================================================
;; Pure domain
;; =============================================================================

(deftest discovery-doc-shape
  (let [d (domain/discovery-doc {:port 4321 :token "t0k" :pid 9})]
    (is (= "http://127.0.0.1:4321/vessel" (get d "url")))
    (is (= "t0k" (get d "token")))
    (is (= "dirge" (get d "vessel")))
    (is (= "json" (get d "dialect")))
    (is (= d (wire/read-json (domain/discovery-json {:port 4321 :token "t0k" :pid 9}))))
    (is (= "/run/user/1000/hive-vessel/dirge.json" (domain/discovery-path "/run/user/1000")))
    (is (= "/tmp/hive-vessel/dirge.json" (domain/discovery-path nil)))))

(deftest reply-parsing
  (is (= {:command :olympus/focus :agent-id "ling-1"}
         (domain/parse-reply "{\"action\":\"focus\",\"target\":\"ling-1\"}")))
  (is (= {:command :olympus/next-tab} (domain/parse-reply "{\"action\":\"next-tab\"}")))
  (is (= :reply/missing-target (:reply/error (domain/parse-reply "{\"action\":\"focus\"}"))))
  (is (= :reply/unknown-action (:reply/error (domain/parse-reply "{\"action\":\"rm -rf\"}"))))
  (is (= :reply/unparseable (:reply/error (domain/parse-reply "{not json"))))
  (is (= :reply/not-an-object (:reply/error (domain/parse-reply "[1]")))))

(deftest route-to-recording-port
  (let [o (recording)]
    (is (= {:routed :olympus/focus :result :focused}
           (ports/route! o {:command :olympus/focus :agent-id "a"})))
    (ports/route! o {:command :olympus/unfocus})
    (ports/route! o {:command :olympus/next-tab})
    (ports/route! o {:command :olympus/prev-tab})
    (ports/route! o {:command :olympus/refresh})
    (is (= {:reply/error :reply/unparseable} (ports/route! o {:reply/error :reply/unparseable})))
    (is (= [[:focus "a"] [:focus nil] [:next-tab] [:prev-tab] [:refresh]] @(:calls o)))))

;; =============================================================================
;; Discovery file
;; =============================================================================

(deftest write-private-is-0600-in-0700
  (let [path (temp-discovery)]
    (boundary/write-private! path "{\"a\":1}")
    (is (= "rw-------" (perms-of path)))
    (is (= "rwx------" (perms-of (.getParent (io/file path)))))
    (is (= "{\"a\":1}" (slurp path)))
    (boundary/write-private! path "{\"a\":2}")
    (is (= "rw-------" (perms-of path)) "an overwrite stays 0600")
    (is (= ["dirge.json"] (vec (.list (.getParentFile (io/file path))))) "no temp file left")))

(deftest lifecycle-writes-and-removes-discovery
  (let [path (temp-discovery)
        a (host/addon-ctor {:dirge/discovery-path path :dirge/olympus (recording)})]
    (is (not (.exists (io/file path))) "construction is pure")
    (let [result (addon/initialize! a {})
          doc (discovery path)]
      (try
        (is (:success? result))
        (is (= "rw-------" (perms-of path)))
        (is (re-matches #"[0-9a-f]{32}" (get doc "token")))
        (is (= (str "http://127.0.0.1:" (get doc "port") "/vessel") (get doc "url")))
        (is (not (str/includes? (pr-str result) (get doc "token"))) "metadata never carries the token")
        (is (not (str/includes? (pr-str (addon/health a)) (get doc "token"))) "health never carries the token")
        (is (= :dirge (:vessel/id ((:vessel/target (addon/hooks a))))))
        (finally (addon/shutdown! a))))
    (is (not (.exists (io/file path))) "shutdown removes the discovery file")
    (is (= {} (addon/hooks a)))))

(deftest tokens-are-random
  (is (not= (boundary/new-token) (boundary/new-token))))

;; =============================================================================
;; Admission
;; =============================================================================

(deftest token-and-origin-admission
  (let [path (temp-discovery)]
    (with-host [a {:dirge/discovery-path path :dirge/olympus (recording)}]
      (let [doc (discovery path)]
        (is (= 200 (.statusCode (request (url doc "/health")))))
        (is (= 401 (.statusCode (request (url doc "/health" "wrong")))) "bad token")
        (is (= 401 (.statusCode (request (str (get doc "url") "/health")))) "no token")
        (is (= 403 (.statusCode (request (url doc "/health")
                                         :headers [["Origin" "http://127.0.0.1:3000"]])))
            "even a loopback browser Origin is refused")
        (is (= 403 (.statusCode (request (url doc "/reply") :method :post
                                         :body "{\"action\":\"refresh\"}"
                                         :headers [["Origin" "http://localhost"]])))
            "a browser POST never reaches olympus")
        (is (= 401 (.statusCode (request (url doc "/reply" "nope") :method :post
                                         :body "{\"action\":\"refresh\"}"))))
        (is (empty? @(:calls (:dirge/olympus (:seed a)))) "refused requests route nothing")))))

;; =============================================================================
;; Replies
;; =============================================================================

(deftest replies-route-to-olympus
  (let [path (temp-discovery)
        o (recording)]
    (with-host [a {:dirge/discovery-path path :dirge/olympus o}]
      (let [doc (discovery path)]
        (is (= 204 (post-reply doc "{\"action\":\"focus\",\"target\":\"ling-7\"}")))
        (is (= 204 (post-reply doc "{\"action\":\"next-tab\"}")))
        (is (= 204 (post-reply doc "{\"action\":\"refresh\"}")))
        (is (= 204 (post-reply doc "{\"action\":\"unfocus\"}")))
        (is (= 204 (post-reply doc "garbage")) "a bad body is still 204")
        (is (= [[:focus "ling-7"] [:next-tab] [:refresh] [:focus nil]] @(:calls o)))
        (let [replies ((:dirge/replies (addon/hooks a)))]
          (is (= 5 (count replies)))
          (is (= {:reply/error :reply/unparseable} (last replies))))))))

(defrecord StubAddon [id hook-map]
  addon/IAddon
  (addon-id [_] id)
  (addon-type [_] :native)
  (capabilities [_] #{:olympus})
  (initialize! [_ _] {:success? true})
  (shutdown! [_] nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] hook-map))

(def olympus-calls (atom []))
(def seat (atom {}))

(defn olympus-stub-ctor [_]
  (->StubAddon "hive.olympus"
               {:olympus/register-presenter! (fn [id target] (swap! seat assoc id target) id)
                :olympus/unregister-presenter! (fn [id] (swap! seat dissoc id) id)
                :olympus/focus! (fn [id] (swap! olympus-calls conj [:focus id]))
                :olympus/next-tab! (fn [] (swap! olympus-calls conj [:next-tab]))
                :olympus/prev-tab! (fn [] (swap! olympus-calls conj [:prev-tab]))
                :olympus/refresh! (fn [] (swap! olympus-calls conj [:refresh]))}))

(defn- manifest [file]
  (some-> (io/resource (str "META-INF/hive-addons/" file)) slurp edn/read-string))

(deftest manifests-shape
  (let [h (manifest "hive-dirge-host.edn")
        o (manifest "hive-olympus-dirge.edn")]
    (is (= "hive.dirge.host" (:addon/id h)))
    (is (= "hive-dirge.host" (:addon/init-ns h)))
    (is (= :foss (:addon/trust-class h)))
    (is (= "hive-olympus.harness" (:addon/init-ns o)))
    (is (= "hive.dirge.host" (get-in o [:addon/config :olympus/host])))
    (is (= #{"hive.olympus" "hive.dirge.host"} (:addon/dependencies o)))
    (let [ids (set (map :addon/id (:specs (mount/discover-specs))))]
      (is (contains? ids "hive.dirge.host"))
      (is (contains? ids "hive.olympus.dirge")))))

(deftest mounted-end-to-end
  (reset! olympus-calls [])
  (reset! seat {})
  (let [path (temp-discovery)
        specs [(update (manifest "hive-dirge-host.edn") :addon/config assoc
                        :dirge/discovery-path path
                        ;; the JDK client surfaces a chunk only once more bytes follow
                        :dirge/heartbeat-ms 100)
               (manifest "hive-olympus-dirge.edn")
               {:addon/id "hive.olympus" :addon/type :native
                :addon/init-ns "hive-dirge.host-test" :addon/init-fn "olympus-stub-ctor"
                :addon/capabilities #{:olympus}}]
        host (mount/atom-mount-host)
        report (mount/mount! (mount/solve specs) host {:license-gate (constantly nil)})]
    (try
      (is (:ok? report) (pr-str (:mounted report)))
      (is (= ["hive.olympus" "hive.dirge.host" "hive.olympus.dirge"] (:order report)))
      (is (= "hive.olympus.dirge" (addon/addon-id (mount-port/registered host "hive.olympus.dirge"))))
      (let [doc (discovery path)
            q (open-events doc)]
        (testing "SSE stream opens with a retry frame"
          (is (= "retry: 2000" (next-line q))))
        (testing "the harness presenter delivers olympus panels as SSE frames"
          (let [present (get @seat "hive.dirge.host")]
            (is (fn? present) "the harness registered under the host id")
            (Thread/sleep 200)
            (is (contains? (present [panel]) :ok))
            (let [frame (read-frame q)
                  data (wire/read-json (get frame "data"))]
              (is (= "vessel" (get frame "event")))
              (is (= "1" (get frame "id")))
              (is (= "ui/show-panel" (get data "op")))
              (is (= "olympus/tab-1" (get data "panel/id")))
              (is (vector? (get data "lines")) "rendered lines for a simple client")
              (is (= (sse/sse-frame 1 {"op" "x"}) "id: 1\nevent: vessel\ndata: {\"op\":\"x\"}\n\n")
                  "hive-vessel's frame format: id, event, one data line, blank line"))))
        (testing "a late subscriber gets the retained panel replayed"
          (let [q2 (open-events doc)]
            (is (= "retry: 2000" (next-line q2)))
            (is (= "olympus/tab-1" (get (wire/read-json (get (read-frame q2) "data")) "panel/id")))))
        (testing "replies reach the injected hive.olympus hooks"
          (is (= 204 (post-reply doc "{\"action\":\"focus\",\"target\":\"ling-2\"}")))
          (is (= 204 (post-reply doc "{\"action\":\"prev-tab\"}")))
          (is (= [[:focus "ling-2"] [:prev-tab]] @olympus-calls))))
      (finally
        (mount/teardown! host (:order report))
        (doseq [id (:order report)]
          (when-let [a (mount-port/registered host id)]
            (try (addon/shutdown! a) (catch Throwable _ nil))))))))

(deftest heartbeat-is-a-ping-comment
  (let [path (temp-discovery)]
    (with-host [_ {:dirge/discovery-path path :dirge/olympus (recording) :dirge/heartbeat-ms 100}]
      (let [q (open-events (discovery path))]
        (is (= "retry: 2000" (next-line q)))
        (is (= "" (next-line q)))
        (is (= ": ping" (next-line q)))))))
