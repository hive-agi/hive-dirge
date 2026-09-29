# hive-dirge

[![ci](https://github.com/hive-agi/hive-dirge/actions/workflows/ci.yml/badge.svg)](https://github.com/hive-agi/hive-dirge/actions/workflows/ci.yml)
[![release](https://github.com/hive-agi/hive-dirge/actions/workflows/release.yml/badge.svg)](https://github.com/hive-agi/hive-dirge/actions/workflows/release.yml)
[![Clojars Project](https://img.shields.io/clojars/v/io.github.hive-agi/hive-dirge.svg)](https://clojars.org/io.github.hive-agi/hive-dirge)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

The hive side of the [dirge](https://github.com/dirge-code/dirge) integration.
It holds:

- `hive.dirge`, the hive-mcp harness as dirge slash commands (see below);
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

## hive.dirge: hive from dirge's command line

With this repo installed as a dirge addon and hive-mcp configured as a dirge MCP
server (`mcp_servers.hive` in dirge's `config.json`), dirge gains:

| Command | What it does |
|---|---|
| `/hive catchup` | runs the project catchup and hands it to the model as the next prompt |
| `/hive wrap` | records a hive session wrap for this project |
| `/hive kanban [todo\|inprogress\|inreview\|done]` | this project's tasks in a side panel |
| `/hive memory <query>` | semantic search over hive memory, hits in the chat |
| `/hive swarm [all\|project\|<project-id>]` | hive agents and their status, working ones first, in a side panel |
| `/hive shout <message>` | posts progress to the hivemind |

The model also gets two tools, `hive_memory_search` and `hive_kanban_list`, and
a system-prompt note naming the commands. Everything calls hive-mcp through
dirge's own MCP connection (`dirge.harness/mcp-call`), so a command costs no
model turn. The server name comes from `:hive/server` in the manifest's
`:addon/config` (default `"hive"`).

Install: symlink this checkout into dirge's addon directory, then run
`/addons reload` in dirge (or restart it).

```sh
ln -s "$PWD" ~/.config/dirge/addons/hive-dirge
```

## Layout

```
.hive-project.edn                      project-id hive-dirge, parent hive
deps.edn                               clojure + hive-addon + hive-dsl; :dev, :test, :build
version.edn, VERSION                   hive-build release config (:publish :clojars)
src/hive_dirge/hive/addon.cljc         hive.dirge IAddon (record HiveDirgeAddon): /hive, tools, effects via a ports map
src/hive_dirge/hive/domain.cljc        pure: config, /hive parsing, MCP requests, answers -> text and panels
src/hive_dirge/harness.cljc            dirge.harness from portable code (notify, mcp-call, panel!, ...)
src/hive_dirge/probe/addon.cljc        probe IAddon (record DirgeProbeAddon, ctor addon-ctor)
src/hive_dirge/economy/addon.cljc      hive.dirge.economy IAddon: hooks + context_retrieve, wiring only
src/hive_dirge/economy/ports.cljc      IObservationLog, IObservationIndex, IDigestor
src/hive_dirge/economy/{domain,digest,markdown}.cljc
                                       pure: Observation/Handle, Digest build + budget fit, markdown render/parse
src/hive_dirge/economy/pipeline/       observe (after-tool-call), retrieve (tool), compact (compact hooks)
src/hive_dirge/economy/adapters/       local observation log, structured digestor
src/hive_dirge/economy/registry.cljc   strategy registry selected by :addon/config
src/hive_dirge/host.clj                hive.dirge.host IAddon (JVM); host/{domain,ports,boundary}.clj strata
test/hive_dirge/host_test.clj          discovery 0600, token/Origin, reply routing, SSE frames, mount e2e
resources/META-INF/hive-addons/
  hive-dirge.edn                       :addon/id "hive.dirge"
  hive-dirge-probe.edn                 mount manifest, :addon/id "hive.dirge.probe"
  hive-dirge-host.edn                  :addon/id "hive.dirge.host"
  hive-dirge-economy.edn               :addon/id "hive.dirge.economy"
  hive-olympus-dirge.edn               olympus harness, host hive.dirge.host
test/hive_dirge/probe/addon_test.clj   manifest -> ctor -> IAddon -> lifecycle
test/fixtures/probe/                   hot-reload fixtures (v1 / v2 of probe.addon)
rescue/                                cljrs spike material, kept as found
```

## How dirge discovers addons

dirge scans `.dirge/addons/` and `~/.config/dirge/addons/` for
`META-INF/hive-addons/*.edn` (and `META-INF/addons/*.edn`). Every manifest names
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

## hive.dirge.economy: context economy hooks

`hive.dirge.economy` (manifest `hive-dirge-economy.edn`) keeps a session's
context bounded without losing what was folded away:

- `:dirge/after-tool-call` logs every tool result under a short content handle
  (`§1a2b3c4d`), in memory and in `.dirge/economy/<session>.edn`. The
  `context_retrieve` tool reads a handle back, either whole or as a line or
  char range. The hook answers nil, so dirge appends nothing.
- `:dirge/compact` receives `{:span [{:role :text :tool :tool-use-id} ...]
  :tokens :reason :focus :ctx-max :pressure :session-id}` and answers
  `{:summary markdown}`, or nil so that dirge's built-in summarizer runs. The
  summary is a digest that uses dirge's own summary section names. It opens with
  a `REFERENCE-ONLY` line and holds:
  - Active Task: the latest user message, verbatim.
  - Goal: the first task statement, verbatim.
  - Completed Actions: a numbered, past-tense list (`E<epoch>.<step>`).
  - Relevant Files, Key Decisions, and errors (under Critical Context).
  - Remaining Work: the TODO/checkbox lines in their latest state.
  - Source Coverage: one citation per folded tool result, with its
    `context_retrieve §handle` hint.

  A later fold copies an earlier digest's lines and citations forward instead
  of summarizing it again, so handles stay valid across compactions. The digest
  fits a budget of `:economy/digest-ratio` (0.2) times the span's tokens,
  clamped to `:economy/digest-min-tokens` (400) and
  `:economy/digest-max-tokens` (3000), at about 4 chars per token. When it
  cannot fit, it answers nil.
- `:dirge/before-compact` only observes: fold count, tokens and the highest
  pressure show up in the addon's health details.

The digestor is picked from a strategy registry by `:economy/digestor` in
`:addon/config` (default `:structured`, which makes no model call). Adding a
strategy means adding an entry to that map.

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

clojure -T:build jar                   # local jar, as the release builds it
clojure -T:build verify-license        # LICENSE vs version.edn vs SPDX headers
```

## Releases

Published to Clojars as `io.github.hive-agi/hive-dirge` through
[hive-build](https://github.com/hive-agi/hive-build):

```edn
io.github.hive-agi/hive-dirge {:mvn/version "RELEASE"}
```

A push to `main` that changes `src/`, `resources/`, `test/`, `deps.edn`,
`version.edn` or the workflows runs `.github/workflows/release.yml`: the suite
gates the release, then `clojure -T:build bump :level :patch`, the changelog,
an annotated `v<version>` tag and `clojure -T:build deploy`. README-only pushes
do not mint a version, because a published pom is immutable. A push to
`staging` runs `staging-gate.yml` (the suite on the declared classpath) and never
publishes.

## License

MIT. Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW). See [LICENSE](LICENSE).
