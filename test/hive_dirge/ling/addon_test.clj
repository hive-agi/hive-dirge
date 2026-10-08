(ns hive-dirge.ling.addon-test
  "hive.dirge.ling makes :dirge a valid spawn_mode (H5): the spawn-mode spec
   is a trifecta, and the addon lifecycle registers and deregisters the mode
   through injected registry ports, never through the host."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-addon.protocol :as addon]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-dirge.ling.addon :as ling-addon]
            [hive-dirge.ling.backend :as backend]))

;; SPDX-License-Identifier: MIT

(def gen-config
  "Addon configs as a manifest or runtime merge would hand them over."
  (gen/hash-map :ling/priority (gen/one-of [(gen/return nil) gen/small-integer])
                :ling/slot-limit (gen/one-of [(gen/return nil) gen/nat])))

(defn- spawnable-headless-mode?
  "A spec hive-spi's registry accepts as an MCP-visible headless mode that
   carries every backend capability."
  [spec]
  (and (map? spec)
       (false? (:requires-emacs? spec))
       (true? (:mcp? spec))
       (nil? (:alias-of spec))
       (= :stdin-stdout (:io-model spec))
       (string? (:description spec))
       (every? (:capabilities spec) backend/capabilities)
       (contains? (:capabilities spec) :dispatch)))

(deftrifecta spawn-mode-spec
  hive-dirge.ling.addon/spawn-mode-spec
  {:golden-path "test/golden/ling-spawn-mode-spec.edn"
   :cases       {:empty       {}
                 :slot-limit  {:ling/slot-limit 4}
                 :priority    {:ling/priority 9}}
   :gen         gen-config
   :pred        spawnable-headless-mode?
   :num-tests   100
   :mutations   [["emacs mode"        (fn [_] {:description "x" :requires-emacs? true :io-model :stdin-stdout
                                               :mcp? true :alias-of nil :capabilities #{:dispatch}})]
                 ["hidden from MCP"   (fn [_] {:description "x" :requires-emacs? false :io-model :stdin-stdout
                                               :mcp? false :alias-of nil
                                               :capabilities (into #{:dispatch} backend/capabilities)})]
                 ["drops backend caps" (fn [_] {:description "x" :requires-emacs? false :io-model :stdin-stdout
                                                :mcp? true :alias-of nil :capabilities #{:dispatch}})]]})

(defn- recording-ports
  "Registry ports that record every call into LOG; REGISTERED? is what the
   headless registry answers."
  [log registered?]
  {:ling/register!         (fn [id _b _meta] (swap! log conj [:backend id])
                             {:registered? registered? :headless-id id})
   :ling/deregister!       (fn [id] (swap! log conj [:unbackend id]))
   :ling/register-mode!    (fn [id spec] (swap! log conj [:mode id (:mcp? spec)]))
   :ling/deregister-mode!  (fn [id] (swap! log conj [:unmode id]))
   :ling/progress!         nil})

(deftest registers-the-spawn-mode-after-the-backend
  (let [log (atom [])
        a (ling-addon/addon-ctor (recording-ports log true))
        init (addon/initialize! a {})]
    (is (true? (get-in init [:metadata :spawn-mode?])))
    (is (= [[:backend :dirge] [:mode :dirge true]] @log))
    (testing "shutdown deregisters the mode before the backend"
      (addon/shutdown! a)
      (is (= [[:unmode :dirge] [:unbackend :dirge]] (subvec @log 2))))))

(deftest no-spawn-mode-without-a-backend
  (let [log (atom [])
        a (ling-addon/addon-ctor (recording-ports log false))
        init (addon/initialize! a {})]
    (is (false? (get-in init [:metadata :spawn-mode?])))
    (is (= [[:backend :dirge]] @log))
    (addon/shutdown! a)
    (is (= [[:backend :dirge]] @log))))

(deftest a-throwing-mode-registry-degrades-to-no-mode
  (let [a (ling-addon/addon-ctor (assoc (recording-ports (atom []) true)
                                        :ling/register-mode! (fn [_ _] (throw (ex-info "boom" {})))))
        init (binding [*err* (java.io.StringWriter.)] (addon/initialize! a {}))]
    (is (true? (:success? init)))
    (is (false? (get-in init [:metadata :spawn-mode?])))
    (addon/shutdown! a)))
