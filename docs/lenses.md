# Dirge lenses

`/hive <lens> [args]` resolves an open lens registry; `/hive lens` lists it.
A lens contributes `{:lens/id :lens/title :lens/panel :lens/open :lens/verbs
:lens/keys :lens/cursor?}`. `:lens/open` receives `[ports config ctx]` and
returns `{:fx [show-panel-ops] :text chat-summary}`. A verb receives
`[ports row-id payload]`. Its show-panel carries `:keys` (lowercase dirge
chords, with either a reply verb or `{"invoke" "verb"}`) and `:cursor true`.
For a row-oriented doc, `:lens/rows` holds id-bearing lines parallel to the
ordinary vessel doc; the dirge-specific JSON translator preserves those ids
in its top-level `lines`. The panel id is the stable invoke routing key.

An IAddon exposes `:dirge/lenses` (a collection or zero-arg function) in its
`hooks` map and `:dirge/invoke` (fn of `{:panel :verb :row :payload}` returning
truthy if handled). The host collects these contributions, routes only verbs
registered for the owning panel and calls the owning addon's hook. The hive
addon also exposes `:dirge/register-lenses!` to overlay lenses at runtime;
no other addon has to edit hive-dirge core. For a mount-time contribution,
provide `:dirge/lenses` in the addon manifest capabilities so the host mounts
after it. The discovery doc publishes capabilities v1 with replies, derived
invokes and key hints; each panel's keys remain authoritative when chords
collide across lenses. The five Olympus reply actions are unchanged.
