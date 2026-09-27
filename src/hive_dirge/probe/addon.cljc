(ns hive-dirge.probe.addon
  "Canonical probe IAddon for the dirge integration.

   Derived from rescue/cljrs-probe/hd/probe_addon.cljc. Kept deliberately
   small and portable: this file must load unchanged under clojurust (cljrs)
   AND JVM Clojure, so it uses only core fns both runtimes share (no reader
   conditionals, no host interop).

   The record is named DirgeProbeAddon, not a generic name like ProbeAddon/
   Addon: cljrs keys protocol impls by the UNQUALIFIED record name, so two
   addons that each define `Addon` in different namespaces would clobber one
   another's IAddon impl there."
  (:require [hive-addon.protocol :as p]))

(def addon-id-str "hive.dirge.probe")

(defn- swarm-view-handler
  "Probe tool: reports how many :rows it was handed."
  [params]
  {:content [{:type "text" :text (str "rows=" (count (:rows params)))}]})

(defrecord DirgeProbeAddon [state]
  p/IAddon
  (addon-id [_] addon-id-str)
  (addon-type [_] :native)
  (capabilities [_] #{:tools :health-reporting :dirge/hooks :dirge/panels})
  (initialize! [_ config]
    (reset! state {:config config :initialized? true})
    {:success? true :errors [] :metadata {:addon/id addon-id-str}})
  (shutdown! [_]
    (reset! state {:config nil :initialized? false})
    {:success? true :errors []})
  (tools [_]
    [{:name        "swarm-view"
      :description "dirge probe: counts the rows it is handed"
      :inputSchema {:type "object"
                    :properties {:rows {:type "array"}}}
      :handler     swarm-view-handler}])
  (schema-extensions [_] {})
  (health [_]
    (let [s @state]
      (if (:initialized? s)
        {:status :ok :details {:config (:config s)}}
        {:status :down :details {:reason :not-initialized}})))
  (excluded-tools [_] #{})
  (hooks [_] {}))

(defn addon-ctor
  "Pure constructor (config -> uninitialized IAddon). The manifest's
   :addon/init-fn. Config is applied by initialize!, not here."
  [_config]
  (->DirgeProbeAddon (atom {:config nil :initialized? false})))
