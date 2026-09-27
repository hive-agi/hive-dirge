Hot-reload fixtures copied (not moved) from `rescue/dirge-slice-reload/src/probe/`.
`addon.cljc` is v1, `addon_v2.cljc` is the same `probe.addon` ns with a changed
`describe`/`hello` and an added `added` fn. A reload test loads v1, then v2 over
the same ns, and checks the new definitions win. Not on any source path:
load them with `load-file`.
