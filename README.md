# hive-dirge

[![ci](https://github.com/hive-agi/hive-dirge/actions/workflows/ci.yml/badge.svg)](https://github.com/hive-agi/hive-dirge/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

The hive side of the [dirge](https://github.com/dirge-code/dirge) integration.
It holds:

- portable (`.cljc`) IAddons that dirge loads under
  [clojurust](https://github.com/BuddhiLW/clojurust) (`cljrs`), written against the
  `hive-addon` IAddon contract, which also load under JVM Clojure;
- `hive.dirge.host`: the JVM vessel target `:dirge` (hive-vessel SSE bridge,
  0600 discovery file `$XDG_RUNTIME_DIR/hive-vessel/dirge.json`, `/reply` key
  routing to hive.olympus) and the `hive.olympus.dirge` harness manifest.

It depends on [`hive-addon`](https://github.com/hive-agi/hive-addon) (the
contract), [`hive-dsl`](https://github.com/hive-agi/hive-dsl),
[`hive-vessel`](https://github.com/hive-agi/hive-vessel) and
[`hive-olympus`](https://github.com/hive-agi/hive-olympus) (for the harness
init-ns). It never
requires `hive-mcp.*`: addons depend on the contract, never on a host.

## Layout

```
.hive-project.edn                      project-id hive-dirge, parent hive
deps.edn                               clojure + hive-addon + hive-dsl; :dev, :test
src/hive_dirge/probe/addon.cljc        probe IAddon (record DirgeProbeAddon, ctor addon-ctor)
src/hive_dirge/host.clj                hive.dirge.host IAddon (JVM); host/{domain,ports,boundary}.clj strata
test/hive_dirge/host_test.clj          discovery 0600, token/Origin, reply routing, SSE frames, mount e2e
resources/META-INF/hive-addons/
  hive-dirge-probe.edn                 mount manifest, :addon/id "hive.dirge.probe"
  hive-dirge-host.edn                  :addon/id "hive.dirge.host"
  hive-olympus-dirge.edn               olympus harness, host hive.dirge.host
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

## hive.dirge session hooks

`hive.dirge` (manifest `hive-dirge.edn`) registers two dirge hooks, available
from the dirge release "dirge addon session hooks" (older dirge ignores them;
`/hive catchup` and `/hive wrap` still work by hand):

- `:dirge/session-start` runs hive `workflow catchup` for the session cwd and
  injects the result into the first turn, bounded by `:hive/max-context-chars`
  (default 12000, truncated with a marker). Skipped when
  `:hive/auto-catchup?` is false or the `:hive/server` MCP server is not
  connected.
- `:dirge/session-end` runs hive `session wrap` when `:hive/auto-wrap?` is on
  and the session ends by `:exit`; a `:swap` wraps only with
  `:hive/wrap-on-swap?` true.

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

## License

MIT. Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW). See [LICENSE](LICENSE).
