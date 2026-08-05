# iNZight GTK4 TODO

App-side follow-ups for the gWidgets2Rgtk4 port. Toolkit blockers live in [`gWidgets2Rgtk4/docs/todo.md`](../../gWidgets2Rgtk4/docs/todo.md). Broader leak audit: monorepo [`inzight-gtk-gaps.md`](../../inzight-gtk-gaps.md).

## P0 — Import preview / `gdf` API — done

`GDf$get_column_index` added in gWidgets2Rgtk4 (integer identity + column-object lookup). `make test FILTER="'data_loads'"` green 2026-08-06 (**FAIL 0 | SKIP 1 | PASS 73**).

---

## P1 — Process hang after test suite

- [ ] **GUI / R session freezes after `make test` finishes**
  - Suite printed final summary then hung; required force-close / `^C`
  - Investigate leftover windows, main-loop / idle handlers, or missing dispose after UI tests

---

## P2 — Deferred (toolkit)

- [ ] **`multiple_x` UI** — skipped until gWidgets2Rgtk4 `gmultiselect` (see toolkit todo)
  - `test_D1_codepanel.R:57`

Test files use letter-group prefixes (`A0`–`G1`) so foundational GUI tests run first; see `tests/testthat.R`.

---

## Passed (orientation)

- The user interface loads / Various UI elements / control_panel
- Data loads (73) / Import/export clipboard data
- Code Panel (except `multiple_x` skip)
- Add to Plot window (49)
- Code writing (43)
- Help, preferences, linked data, state, export, manip, validate, updateplot, save plot, summary, inference, surveys, addons, FutureLearn — skipped or green as of last full run
