(ns hive-dirge.root-tool-contract-test
  (:require [hive-addon.tool-contract.test :refer [deftest-root-tools]]
            [hive-addon.protocol :as proto]
            [hive-dirge.economy.addon :as economy]
            [hive-dirge.probe.addon :as probe]
            [hive-dirge.hive.addon :as hive]))

(deftest-root-tools economy-root-tools
  (proto/tools (economy/addon-ctor {})))

(deftest-root-tools probe-root-tools
  (proto/tools (probe/addon-ctor {})))

(deftest-root-tools hive-root-tools
  (proto/tools (hive/addon-ctor {})))
