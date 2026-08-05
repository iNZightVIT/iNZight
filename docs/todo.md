# iNZight GTK4 TODO

App-side follow-ups for the gWidgets2Rgtk4 port. Toolkit blockers live in [`gWidgets2Rgtk4/docs/todo.md`](../../gWidgets2Rgtk4/docs/todo.md). Broader leak audit: monorepo [`inzight-gtk-gaps.md`](../../inzight-gtk-gaps.md).

## P0 — Toolkit deps (landed)

Previously blocked on gWidgets2Rgtk4:

1. ~~`gslider` factor/character `set_value`~~ — discrete snap + labels
2. ~~`gcombobox` length-zero `selected`~~ — empty/`which()` miss → first item (RGtk2-compatible)

`make test FILTER="'code_writing'"` is green (2026-08-06).

---

## P1 — Recorded test failures (re-check)

### Add to Plot — `tests/testthat/test_D2_addtoplot.R`

- [ ] **Axes and Labels - bar plots** (`:230`)
  - Error: `ui$moduleWindow$body$children[[1]]$children[[1]]` subscript out of bounds
  - Re-check after toolkit gslider fix; if still failing, inspect add-to-plot page layout for bar plots

```sh
make test FILTER="'addtoplot'"
```

### Code writing — done

Former G1/G2 / size-by / symbol-by failures cleared after toolkit P0 + sizeMethod default hardening in `iNZPlotModWin.R`.

---

## P2 — After focused green

- [ ] Full `make test`; extend this list with anything past the previous max-fail cutoff (surveys, import, …)

Test files use letter-group prefixes (`A0`–`G1`) so foundational GUI tests run first; see `tests/testthat.R`.

---

## Passed (orientation)

- The user interface loads
- Various UI elements
- Add-on Modules (1 skip)
- Import/export clipboard data
- control_panel
- Code writing (43 pass)
