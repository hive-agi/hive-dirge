# Dirge lenses

`/hive <lens> [args]` resolves an open lens registry; `/hive lens` lists it.
A lens contributes `{:lens/id :lens/title :lens/panel :lens/open :lens/verbs
:lens/keys :lens/cursor?}`. `:lens/open` receives `[ports config ctx]` and
returns `{:fx [show-panel-ops] :text chat-summary}`. A verb receives
`[ports row-id payload]`. Its show-panel carries `:keys` (lowercase dirge
chords, with either a reply verb or `{"invoke" "verb"}`) and `:cursor true`.
For a row-oriented doc, `:panel/rows` holds id-bearing lines alongside the
ordinary vessel doc; the dirge-specific JSON translator preserves their ids
and payloads inside `lines`. The panel id is the stable invoke routing key.

An IAddon exposes `:dirge/lenses` (a collection or zero-arg function) in its
`hooks` map and `:dirge/invoke` (fn of `{:panel :verb :row :payload}` returning
truthy if handled). The host collects these contributions, routes only verbs
registered for the owning panel and calls the owning addon's hook. The hive
addon also exposes `:dirge/register-lenses!` to overlay lenses at runtime;
no other addon has to edit hive-dirge core. For a mount-time contribution,
provide `:dirge/lenses` in the addon manifest capabilities so the host mounts
after it. The discovery doc publishes capabilities v1 with replies, derived
invokes and key hints keyed by panel id (`{"kanban" {"enter" ...} "carto"
{"enter" ...}}`), so a chord two lenses bind differently is never collapsed
into one lens's verb; each panel's own keys remain authoritative. The five Olympus reply actions are unchanged.

An addon that presents into dirge through `hive.dirge.host` (rather than as a
lens of `hive.dirge`) owns its panel from the outside: the host's hooks
`:vessel/register-panel-verbs!` (fn of `panel verbs`, where VERBS maps a verb
string to `(fn [invoke])`) and `:vessel/unregister-panel-verbs!` (fn of
`panel`) register the verbs an invoke reply on that panel runs. A lens-owned
panel still routes to its owner first; an unregistered verb is ignored with a
warning. Registrations live as long as the host instance, so a presenter
re-offers them after a host restart.

## Feature handshake (Lens C3)

dirge subscribes with `GET /vessel/events?token=..&vessel=dirge&features=spans,keys,cursor,open-file`.
hive-vessel (commit d7fda1a, branch lens-c3-features) parses the comma list per client and records
the set; hive-dirge's host reads it back and answers it as `:vessel/features` on the `:vessel/target`
hook. The feature set is versioned at **1** (`hive-dirge.host.domain/feature-set-version`, the
mirror of `hive-vessel.executor.handshake/feature-set-version`); bump only for a breaking change to
the `features` param semantics.

**Union semantics.** The target advertises the **union** of the connected dirge clients'
sets: a feature is present when *any* client advertised it. Every consumer of
`:vessel/features` gates on that union: the dirge show-panel translator ships
`spans`, `keys` and `cursor` whenever the union contains them. The union is the
capability-projection policy -- a feature a client never advertised must not
degrade the clients that did (and unknown fields are ignored by clients without that feature).
Plain lines always carry row `id`s, so invoke routing works on any client.

- `:spans` — structured span rows appear inside `"lines"`, never as a top-level `"spans"` field; without it lines flatten to text while retaining row ids and payloads.
- `:keys` — the message carries `"keys"` (dirge chords) when the panel declares them.
- `open-file` — reserved for the open-file feed op.

`deps.edn` still pins `hive-vessel 0.1.12`, whose bridge does not record features: the reader
resolves `hive-vessel.executor.sse/client-features` at call time and degrades to `#{}` (plain
lines), so this repo loads and runs green against either side.
