# hive-dirge

The private hive side of the dirge integration. It holds:

- portable (`.cljc`) IAddons that dirge loads under
  [clojurust](../../clojurust) (`cljrs`), written against the
  `hive-addon` IAddon contract, which also load under JVM Clojure;
- later, the `hive.dirge.host` vessel target.

It depends on `hive-addon` (the contract) and `hive-dsl` only. It never
requires `hive-mcp.*`: addons depend on the contract, never on a host.

## Layout

```
.hive-project.edn                      project-id hive-dirge, parent hive
deps.edn                               clojure + hive-addon + hive-dsl; :dev, :test
src/hive_dirge/probe/addon.cljc        probe IAddon (record DirgeProbeAddon, ctor addon-ctor)
resources/META-INF/hive-addons/
  hive-dirge-probe.edn                 mount manifest, :addon/id "hive.dirge.probe"
test/hive_dirge/probe/addon_test.clj   manifest -> ctor -> IAddon -> lifecycle
test/fixtures/probe/                   hot-reload fixtures (v1 / v2 of probe.addon)
rescue/                                cljrs spike material, kept as found
```

## How dirge discovers addons

dirge scans `.dirge/addons/**/META-INF/hive-addons/*.edn`. Every manifest names
`:addon/init-ns` and `:addon/init-fn`. dirge requires the namespace under cljrs,
calls the constructor with `:addon/config`, and then drives the IAddon lifecycle
(`initialize!`, `tools`, `health`, `shutdown!`). A JVM host does the same thing
from the classpath (`hive-addon.mount.boundary/discover-specs`). The same
manifest is used for both.

To install this repo's addon into a dirge workspace, put (or symlink) `src/` and
`resources/` under `.dirge/addons/hive-dirge/`.

## Portability rules for addon code

- One `.cljc` file with no host interop and no reader conditionals, unless it
  really needs them.
- Give every record a globally unique name. cljrs keys protocol impls by the
  **unqualified** record name, so two namespaces that each define `Addon` would
  overwrite each other's IAddon impl.
- The constructor is pure (`config -> addon`). State lives in an atom that
  `initialize!` and `shutdown!` reset.

## Running

```sh
clojure -M:test                        # JVM suite (cognitect test-runner)
clojure -M:dev                         # against the sibling ../hive-addon checkout

# cljrs smoke: a main.cljc that requires hive-dirge.probe.addon
cljrs run --src-path src --src-path ../hive-addon/src main.cljc
```
