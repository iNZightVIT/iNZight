# iNZight GTK4 TODO

App-side follow-ups for the gWidgets2Rgtk4 port. Toolkit blockers live in [`gWidgets2Rgtk4/docs/todo.md`](../../gWidgets2Rgtk4/docs/todo.md). Broader leak audit: monorepo [`inzight-gtk-gaps.md`](../../inzight-gtk-gaps.md).

## P0 — Depends on toolkit (do not chase until green)

Fix these in **gWidgets2Rgtk4** first (see toolkit todo P0):

1. `gslider` factor/character `set_value` (hangs / GTK callback errors around G1/G2)
2. `gcombobox` length-zero `selected` (size by / symbol by)

Then re-run:

```sh
make test FILTER="'addtoplot'"
make test FILTER="'code_writing'"
```

---

## P1 — Recorded test failures (2026-08-05 suite)

Interrupted `make test` (exit 130 after `^C`). UI load / clipboard / control_panel were green; suite stopped at max fails during Code writing.

### Add to Plot — `tests/testthat/test_addtoplot.R`

- [ ] **Axes and Labels - bar plots** (`:230`)
  - Error: `ui$moduleWindow$body$children[[1]]$children[[1]]` subscript out of bounds
  - Likely cascade from earlier slider callback failure leaving `moduleWindow` incomplete
  - Re-check after toolkit `gslider` fix; if still failing, inspect add-to-plot page layout for bar plots

### Code writing — `tests/testthat/test_code_writing.R`

Many of these look like **stale G1/G2** after failed slider updates. Re-triage after toolkit P0.

| Status | Line | Test | Symptom |
|--------|------|------|---------|
| [ ] | `:114` | Plot code is generated correctly | Got `\| gender + age, g2.level=…`; expected `\| age, g1.level=…` |
| [ ] | `:121` | Plot code is generated correctly | Got `\| gender`; expected bare `~height` |
| [ ] | `:160` | colour by | Extra `\| gender` in plot code |
| [ ] | `:191` | colour by | Extra `\| gender` |
| [ ] | `:200` | colour by | Extra `\| gender` |
| [ ] | `:223` | size by | `selected` length zero → combobox init (toolkit P0 #2) |
| [ ] | `:274` | symbol by | same as `:223` |
| [ ] | `:333` | cat x cat | Extra `\| gender` |
| [ ] | `:339` | cat x cat | Extra `\| gender` |

If G1/G2 still mis-clear after slider fix, inspect reset paths in [`R/iNZControlWidget.R`](../R/iNZControlWidget.R) (`svalue(..., index = TRUE) <- 1L` and G1↔G2 promotion).

### Hangs observed (`^C`)

- During Code writing around G1/G2 / subset-slider messages (`height | age[7 - 11]`, `height …`, `height | gender`)
- Accompanied by `Ops.factor` / GTK callback errors from `gslider`

---

## P2 — After focused green

- [ ] Full `make test`; extend this list with anything past the previous max-fail cutoff (surveys, import, …)
- [ ] Commit UI test renames if desired (`test_ui_*.R` → `test_aaa_ui_loads.R` / `test_aab_ui_features.R` — currently unstaged)

---

## Passed in that run (orientation)

- The user interface loads
- Various UI elements
- Add-on Modules (1 skip)
- Import/export clipboard data
- control_panel (also green when run alone)
