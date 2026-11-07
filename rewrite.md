# QCAD Rust Rewrite Plan

## Goal

Reimplement QCAD as a native desktop application in Rust, replacing the current
ClojureScript + Electron stack without losing behavior, file compatibility, or
test coverage.

The rewrite is layered: all circuit logic, OpenQASM handling, persistence, and
export live in pure Rust crates that can be tested headlessly. The UI is a thin
`egui`/`eframe` layer that maps input intents onto the core commands.

## Target stack

- **UI:** `egui` + `eframe` (immediate-mode custom-painted canvas)
- **Model / state:** plain Rust structs and enums, `serde` for serialization
- **OpenQASM:** hand-ported parser and serializer (`chumsky`/`pest` optional later)
- **File dialogs:** `rfd`
- **Clipboard:** `arboard`
- **PNG export:** `resvg` + `tiny-skia` + `image`
- **Persistence:** `serde_json` + `directories` (app-data files)
- **Errors:** `thiserror`; logging via `tracing`

## Workspace layout

```
qcad/
  crates/
    qcad-core/      # state, items, wiring, geometry, history, components
    qcad-qasm/      # OpenQASM 3 import/export
    qcad-persist/   # document JSON, autosave, library, recent files
    qcad-export/    # SVG + PNG
    qcad-app/       # eframe::App, canvas, panels, dialogs, menus
  resources/        # icons, fonts, OpenQASM grammar reference, filters
  tests/fixtures/   # parity fixtures captured from the CLJS implementation
```

## Current-to-Rust mapping

| Current (ClojureScript) | Rust target | Notes |
|---|---|---|
| `src/app/model.cljs` | `qcad-core::state` | `Item`/`Wire` enums, `AppState`, snapshot history |
| `src/app/components.cljs` | `qcad-core::components` | static catalog via `LazyLock<Vec<Component>>` |
| `src/app/core.cljs` event handlers | `qcad-app::canvas` + intent/command layer | hit-testing and drag intents kept pure where possible |
| `src/app/dom.cljs` | `qcad-app::{canvas,panels,dialogs}` | `egui::Painter` replaces `document.createElement` |
| `src/app/export.cljs::to-svg` | `qcad-export::svg` | 1:1 string generation; output must not change |
| browser canvas PNG export | `qcad-export::png` | rasterize the SVG with `resvg` |
| `src/app/persist.cljs`, `src/app/library.cljs` | `qcad-persist` | keep the existing JSON schema |
| `src/app/qasm.cljs` | `qcad-qasm::{parser,serializer}` | preserve lenient import and raw statements |
| `main.cjs` / `preload.cjs` | `qcad-app::{menu,files,clipboard}` | `egui` menus, `rfd`, `arboard`, recent-files list |
| `src/app/main.cljs` | `qcad-app::main` | `eframe::run_native`, 1100x700 window |

Because there is no webview, the browser-fallback branches in `core.cljs`
(`download-url!`, `browser-open!`, `navigator-clipboard`, `localStorage`) are
removed rather than ported.

## Testing and parity strategy

1. **Capture first.** Before deleting any ClojureScript, generate canonical
   fixtures for representative circuits: document JSON, autosave JSON, emitted
   OpenQASM, and exported SVG. Store them under `tests/fixtures/`.
2. **Pure crates get `#[test]` + `insta` golden tests.** Port
   `model_test` (1255 lines), `qasm_test` (706), `persist_test`, `export_test`,
   `library_test`, and `components_test`.
3. **Split input from rendering.** Introduce `Intent -> Command -> State` so the
   1583-line jsdom integration suite can be ported as controller tests without a
   window.
4. **Add a few `egui_kittest` smoke tests** for real widget interactions
   (drop, select, drag, dialog submit).
5. **Coverage gate** via `cargo llvm-cov`, matching the current `.c8rc.json`
   thresholds.

Definition of done for every feature: code + unit tests + parity fixture (where
output is observable) + no regression in previously ported tests.

## Risks and decisions

- **JSON compatibility:** existing `.json` circuits, autosave, and the library
  must load unchanged. Use serde `rename_all`/`default` and round-trip tests.
- **QASM fidelity:** the current importer is lenient and preserves unknown
  statements verbatim. Do not swap in a strict grammar until parity is proven.
- **Export independence:** SVG/PNG generation must not depend on `egui`, so
  output stays deterministic and testable.
- **UI differences:** text shaping, accessibility, and IME are weaker than a
  webview; bundle a font for consistent labels.
- **Integration parity:** jsdom assertions do not map 1:1; the intent/command
  split is the mitigation.

## Milestones

- **M1 - Headless core:** A1-A3, B1-B9 green on ported model tests.
- **M2 - Formats:** C1-C3, D1-D6 green on QASM/persist/export fixtures.
- **M3 - UI MVP:** E1-E4, F1-F3: window opens, components drop, canvas draws,
  selection/move/wire work.
- **M4 - Full parity:** E5-E7, F4-F6, G1-G10, H1-H3.
- **M5 - Release:** H4-H5, legacy stack removed.

Rough effort for one experienced developer: 6-9 weeks.

---

# Feature backlog

Priority: **P0** required for MVP, **P1** required for parity, **P2** polish.
Each feature is independently mergeable and should ship with its tests.

## Epic A - Foundation

### [ ] A1. Workspace and CI scaffolding
- **Priority:** P0
- **Depends on:** none
- **Scope:** Cargo workspace with the five crates, `rustfmt`/`clippy` config,
  GitHub Actions matrix (build + test + coverage), issue/feature tracking.
- **Acceptance:** `cargo build`, `cargo test`, `cargo clippy -D warnings` pass in
  CI on Windows, macOS, and Linux with empty crates.
- **Tests:** CI green; smoke test in each crate.

### [ ] A2. Parity fixture capture
- **Priority:** P0
- **Depends on:** none (uses current CLJS app)
- **Scope:** Script or test harness that emits fixtures for: empty circuit,
  registers + multi-qubit gates, custom gates, includes/metadata, control flow,
  classical bits/measure, boxes/annotations, library entries, autosave JSON,
  SVG export, and PNG-export SVG at several scales.
- **Acceptance:** `tests/fixtures/` contains versioned fixtures and a manifest
  describing each circuit.
- **Tests:** fixture generator is rerunnable and deterministic.

### [ ] A3. Core domain types
- **Priority:** P0
- **Depends on:** A1
- **Scope:** `Item` enum (`Register`, `Gate`, `Annotation`, `Box`), `Wire`,
  `Port`, `PortKind`, `View`, `Dialog` enum, `ContextMenu`, `WireDraft`,
  `Marquee`, `Metadata`, `AppState`, ID counters, snapshot type.
- **Acceptance:** serde representation matches existing JSON field names/values.
- **Tests:** serde round-trip against `tests/fixtures/*.json`.

## Epic B - Core model

### [ ] B1. Component catalog
- **Priority:** P0
- **Depends on:** A3
- **Scope:** Static component definitions (register, box, annotation, custom,
  library, built-in gates, parameterized and multi-qubit gates) with labels and
  port-count rules.
- **Acceptance:** `port_count`/`gate_port_count` match CLJS for every component.
- **Tests:** ported `components_test`; table-driven per component.

### [ ] B2. Geometry, anchors, orthogonal routing
- **Priority:** P0
- **Depends on:** A3, B1
- **Scope:** item rectangles/sizes, port anchors, item anchors, fallback anchors,
  `orthogonal_path`, content bounds used by export.
- **Acceptance:** identical anchors/paths to CLJS for representative states.
- **Tests:** ported geometry cases + `proptest` invariants (no zero-length
  segments, path starts/ends at anchors).

### [ ] B3. Wiring
- **Priority:** P0
- **Depends on:** B2
- **Scope:** wire draft start/finish/cancel, corner points, add/remove wire,
  free-port selection, overflow cleanup when gate qubits shrink.
- **Acceptance:** wire behavior matches `model_test` cases.
- **Tests:** ported wiring tests incl. overflow and draft cleanup.

### [ ] B4. Item CRUD, selection, rotation, resize
- **Priority:** P0
- **Depends on:** B2
- **Scope:** add/remove items, selection add/toggle/clear, rotate by 45/90,
  resize boxes with 80x60 minimum, item lookup, next-id management.
- **Acceptance:** same observable state transitions as CLJS model tests.
- **Tests:** ported `model_test` selection/rotate/resize cases.

### [ ] B5. Move, marquee, and snapping
- **Priority:** P0
- **Depends on:** B4
- **Scope:** move selected items with grid snap and component-to-component
  alignment snapping; marquee hit-testing; empty moving/static sets tolerated.
- **Acceptance:** positions match fixtures for grid and alignment snapping.
- **Tests:** ported snapping/marquee tests + property tests for invariants.

### [ ] B6. History (undo/redo)
- **Priority:** P0
- **Depends on:** B4
- **Scope:** snapshot history with limit, coalescing drags, undo/redo/can-undo/
  can-redo, diff summary used by the diff dialog.
- **Acceptance:** history cap and index behavior match CLJS.
- **Tests:** ported history tests, including capped history and diff summary.

### [ ] B7. View state
- **Priority:** P0
- **Depends on:** A3
- **Scope:** pan, zoom around pointer, reset, fit-view bounds, grid size and snap
  toggles.
- **Acceptance:** fit-view produces `[0 0 1]` for empty circuits and consistent
  bounds for non-empty ones.
- **Tests:** ported view tests.

### [ ] B8. Metadata, includes, raw statements
- **Priority:** P1
- **Depends on:** A3
- **Scope:** document metadata, `include` list, opaque/raw QASM statements,
  annotations; normalization rules.
- **Acceptance:** preserved across load/import/export cycles.
- **Tests:** round-trip fixtures.

### [ ] B9. Dialogs state machine
- **Priority:** P1
- **Depends on:** B3, B4, B8
- **Scope:** open/confirm/cancel flows for register, gate qubits, custom gate,
  annotation, metadata, export, library, diff; validation errors.
- **Acceptance:** dialog transitions match CLJS `model_test` cases.
- **Tests:** ported dialog tests.

## Epic C - OpenQASM

### [ ] C1. OpenQASM serializer
- **Priority:** P0
- **Depends on:** B1-B3, B8
- **Scope:** registers, wired gate calls, custom gates, measurements, resets,
  barriers, includes, metadata, annotations, raw statements, control-flow
  preservation.
- **Acceptance:** byte-identical output for all serializer fixtures.
- **Tests:** ported `qasm_test` serializer cases + `insta` golden files.

### [ ] C2. OpenQASM parser/importer
- **Priority:** P0
- **Depends on:** C1, B3
- **Scope:** declarations (`qubit`, `bit`), built-in/custom gate calls, indexed
  and whole-register operands, measure/reset/barrier, includes, metadata,
  opaque declarations, control flow kept raw, layout placement.
- **Acceptance:** imported state matches parsed fixtures, including gate
  reconnection through ports.
- **Tests:** ported `qasm_test` import cases + golden state snapshots.

### [ ] C3. Parser fidelity hardening
- **Priority:** P1
- **Depends on:** C2
- **Scope:** fuzz/property tests for comment stripping, brace matching, unknown
  statements, malformed input; explicit error reporting without panics.
- **Acceptance:** no panics on fuzzed input; unsupported constructs preserved.
- **Tests:** `proptest`/`cargo fuzz` corpus + regression fixtures.

## Epic D - Persistence and export

### [ ] D1. Document JSON
- **Priority:** P0
- **Depends on:** A3
- **Scope:** `to_json`/`from_json`, normalization of types/components/ports, ID
  repair, validation, snapshot serialization.
- **Acceptance:** loads existing `.json` and autosave fixtures unchanged and
  re-emits compatible JSON.
- **Tests:** ported `persist_test` + round-trip fixtures.

### [ ] D2. Autosave
- **Priority:** P1
- **Depends on:** D1, B7
- **Scope:** debounced save to app-data, load on start, clear when saved.
- **Acceptance:** autosave file is compatible with existing format.
- **Tests:** fake clock/storage unit tests.

### [ ] D3. Circuit library
- **Priority:** P1
- **Depends on:** D1
- **Scope:** save/load/delete/list user circuits, name validation, insertion as
  blocks.
- **Acceptance:** existing library data loads; insertion behavior matches CLJS.
- **Tests:** ported `library_test`.

### [ ] D4. Recent files
- **Priority:** P2
- **Depends on:** G9
- **Scope:** bounded recent list (max 8), submenu, open-at-path.
- **Acceptance:** recent list survives restart and deduplicates.
- **Tests:** unit tests for list maintenance.

### [ ] D5. SVG export
- **Priority:** P1
- **Depends on:** B2
- **Scope:** full SVG generation with content bounds, padding, grid, transparency,
  wires, control markers, item labels, XML escaping.
- **Acceptance:** byte-identical to `export_test` fixtures.
- **Tests:** ported `export_test` + golden files.

### [ ] D6. PNG export
- **Priority:** P1
- **Depends on:** D5
- **Scope:** SVG -> raster at requested scale, transparent/background/grid
  options, PNG encoding.
- **Acceptance:** pixel dimensions and option handling match expected behavior.
- **Tests:** image dimension/alpha assertions on fixtures.

## Epic E - Native UI shell and canvas

### [ ] E1. App shell and menu bar
- **Priority:** P0
- **Depends on:** A1
- **Scope:** `eframe::run_native`, 1100x700 window, File and View menus, quit,
  reload/devtools equivalents where applicable.
- **Acceptance:** app launches and menu actions dispatch commands.
- **Tests:** `egui_kittest` launch/menu smoke test.

### [ ] E2. Components side panel
- **Priority:** P0
- **Depends on:** B1, E1
- **Scope:** 20%-width left panel, component tiles grouped like the CLJS panel,
  drag-to-canvas and click-to-place, scroll behavior.
- **Acceptance:** every component can be placed and creates the expected dialog
  or item.
- **Tests:** controller tests for drop intents; kittest tile interaction.

### [ ] E3. Canvas rendering
- **Priority:** P0
- **Depends on:** B2, E1
- **Scope:** grid, register/gate/box/annotation drawing, ports, labels, wires
  (including classical dashes), control lines, control dots, marquee, selection
  outline, box resize handle.
- **Acceptance:** visual output covers every item type and wire kind; no panics
  on empty state.
- **Tests:** rendering snapshot tests + kittest geometry assertions.

### [ ] E4. Navigation and status
- **Priority:** P0
- **Depends on:** B7, E3
- **Scope:** pan (middle drag), wheel zoom around pointer, `0` reset, fit-view,
  status bar (world cursor, zoom, grid size, snap state).
- **Acceptance:** view math matches B7; status updates live.
- **Tests:** controller tests for pan/zoom/fit; kittest status text.

### [ ] E5. Toolbar and grid controls
- **Priority:** P1
- **Depends on:** E4
- **Scope:** toolbar buttons (wire, snap, align, undo/redo, fit, zoom, library),
  grid-size select, active/disabled states.
- **Acceptance:** controls reflect state and dispatch the correct commands.
- **Tests:** kittest button/select interaction.

### [ ] E6. Property panel
- **Priority:** P1
- **Depends on:** E3, B4
- **Scope:** selected-item property rows (type, component, qubits, angle,
  controls, position, size, rotation).
- **Acceptance:** values match model state for every item type.
- **Tests:** unit tests on the property projection.

### [ ] E7. Minimap
- **Priority:** P2
- **Depends on:** E3, E4
- **Scope:** overview of content bounds with current viewport indicator.
- **Acceptance:** indicator tracks pan/zoom; click/drag navigates (optional P2).
- **Tests:** projection unit tests.

## Epic F - Canvas interactions

### [ ] F1. Selection and marquee
- **Priority:** P0
- **Depends on:** B4, B5, E3
- **Scope:** click select, shift-toggle, marquee on empty canvas, selection sync
  with rendering, click-away clear.
- **Acceptance:** matches CLJS integration behavior for select/marquee.
- **Tests:** ported integration cases as controller tests.

### [ ] F2. Move and snapping interaction
- **Priority:** P0
- **Depends on:** B5, F1
- **Scope:** drag selection, grid/alignment snapping, coalesced single undo step,
  cursor feedback.
- **Acceptance:** one history entry per drag; positions match fixtures.
- **Tests:** controller tests for drag sequences.

### [ ] F3. Wire tool
- **Priority:** P0
- **Depends on:** B3, E3
- **Scope:** toggle wire mode, output port start, corner clicks, input port
  finish, cancel, wire snapping to ports.
- **Acceptance:** wires created through ports with correct routes.
- **Tests:** ported wire interaction tests.

### [ ] F4. Rotate and resize interaction
- **Priority:** P1
- **Depends on:** B4, F1
- **Scope:** `R` rotation, context-menu rotation, box handle drag with minimum
  size and history entry.
- **Acceptance:** resize geometry and undo behavior match CLJS.
- **Tests:** ported box-resize and rotation cases.

### [ ] F5. Context menu and keyboard shortcuts
- **Priority:** P1
- **Depends on:** F1-F4
- **Scope:** right-click menu (delete, rotate, toggle control, edit), Escape
  cancel/delete, `G`/`A`/`W`/`R`/`F`/`0`, Ctrl+Z/Y, Ctrl+C/V/D, Ctrl+O/S.
- **Acceptance:** shortcut and context actions map to the same commands as CLJS.
- **Tests:** controller tests per shortcut; kittest context menu.

### [ ] F6. Clipboard copy/paste
- **Priority:** P1
- **Depends on:** D1, F5
- **Scope:** copy selection as JSON payload, paste with new IDs and offset,
  duplicate, system clipboard via `arboard`.
- **Acceptance:** paste/duplicate behavior matches CLJS integration tests.
- **Tests:** controller tests + clipboard round-trip (headless fake).

## Epic G - Dialogs, files, and feedback

### [ ] G1. Register dialog
- **Priority:** P0
- **Depends on:** B9, E1
- **Scope:** name/qubits form, quantum vs classical, validation, create/edit.
- **Acceptance:** matches CLJS dialog behavior.
- **Tests:** kittest form submission + controller tests.

### [ ] G2. Gate qubits / angle / controls dialog
- **Priority:** P0
- **Depends on:** B9
- **Scope:** qubit count, optional angle, controls; overflow cleanup on shrink.
- **Acceptance:** matches CLJS behavior and tests.
- **Tests:** ported gate-dialog tests + kittest.

### [ ] G3. Custom gate dialog
- **Priority:** P1
- **Depends on:** B9, C2
- **Scope:** qubit count + OpenQASM definition, edit existing, validation,
  arity checks on export.
- **Acceptance:** matches CLJS behavior.
- **Tests:** controller + kittest tests.

### [ ] G4. Annotation dialog
- **Priority:** P1
- **Depends on:** B9
- **Scope:** add/edit annotation text with required-text validation.
- **Acceptance:** matches CLJS behavior.
- **Tests:** ported annotation tests.

### [ ] G5. Metadata dialog
- **Priority:** P2
- **Depends on:** B8, B9
- **Scope:** name, author, description editing.
- **Acceptance:** values persist through save/load.
- **Tests:** round-trip tests.

### [ ] G6. Export image dialog
- **Priority:** P1
- **Depends on:** D5, D6, B9
- **Scope:** SVG/PNG, scale, transparent background, include grid; save via `rfd`.
- **Acceptance:** options produce expected output; cancel is a no-op.
- **Tests:** ported export dialog tests.

### [ ] G7. Circuit library dialog
- **Priority:** P1
- **Depends on:** D3, B9
- **Scope:** save current circuit by name, list/insert/delete saved circuits,
  name validation.
- **Acceptance:** matches CLJS behavior.
- **Tests:** ported library dialog tests.

### [ ] G8. Circuit diff dialog
- **Priority:** P2
- **Depends on:** B6, B9
- **Scope:** before/after summary from history using the diff formatter.
- **Acceptance:** text matches CLJS diff output.
- **Tests:** golden diff tests.

### [ ] G9. File open/save/recent
- **Priority:** P0
- **Depends on:** D1, D4, C1, C2
- **Scope:** open/save document JSON and OpenQASM, save-as, recent menu, error
  handling for unreadable/malformed files.
- **Acceptance:** existing files open; saves are reloadable; errors surfaced.
- **Tests:** temp-dir integration tests + `rfd` abstraction with a fake.

### [ ] G10. Notifications and error surfacing
- **Priority:** P2
- **Depends on:** G9
- **Scope:** replace `console.error` paths with an in-app status/toast area;
  dialog-level validation errors.
- **Acceptance:** no silent failures for file/export/clipboard errors.
- **Tests:** controller tests asserting notification events.

## Epic H - Verification and delivery

### [ ] H1. Intent/command test harness
- **Priority:** P1
- **Depends on:** F1-F6
- **Scope:** pure controller that accepts pointer/key/dialog intents and mutates
  state, enabling headless port of `integration_test.cljs`.
- **Acceptance:** all ported integration scenarios pass without a window.
- **Tests:** full integration suite ported.

### [ ] H2. `egui_kittest` smoke suite
- **Priority:** P2
- **Depends on:** E1-G8
- **Scope:** window launch, component drop, select/move, wire, dialog submit,
  menu action, export dialog.
- **Acceptance:** suite runs headless in CI.
- **Tests:** kittest scenarios.

### [ ] H3. Coverage gate
- **Priority:** P1
- **Depends on:** H1
- **Scope:** `cargo llvm-cov` in CI with thresholds derived from current
  `.c8rc.json`; exclude generated/glue code intentionally.
- **Acceptance:** CI fails below threshold; report published as artifact.
- **Tests:** CI configuration verified on a throwaway branch.

### [ ] H4. Packaging and docs
- **Priority:** P1
- **Depends on:** H2, H3
- **Scope:** release builds and installers (`cargo-dist` or `cargo-bundle`),
  icons, window metadata, updated README with build/run/test instructions.
- **Acceptance:** installable artifact on Windows/macOS/Linux.
- **Tests:** packaging job green; manual smoke install.

### [ ] H5. Remove legacy stack
- **Priority:** P1
- **Depends on:** H4
- **Scope:** delete `src/`, `test/`, `main.cjs`, `preload.cjs`, `index.html`,
  `package.json`, `shadow-cljs.edn`, `public/js`, `node_modules`, coverage
  tooling; archive fixtures.
- **Acceptance:** repository builds and tests with only Cargo; no Node dependency.
- **Tests:** clean-machine build from README.

---

## Suggested merge order

1. A1 -> A2 -> A3
2. B1 -> B2 -> B3 -> B4 -> B5 -> B6 -> B7 -> B8 -> B9
3. C1 -> C2 -> C3
4. D1 -> D5 -> D6 -> D2 -> D3 -> D4
5. E1 -> E3 -> E2 -> E4 -> E5 -> E6 -> E7
6. F1 -> F2 -> F3 -> F4 -> F5 -> F6
7. G1 -> G2 -> G4 -> G3 -> G9 -> G6 -> G7 -> G8 -> G5 -> G10
8. H1 -> H3 -> H2 -> H4 -> H5

Features may be developed in parallel once their dependencies are merged; A2
must land before C1/D1/D5 so golden outputs exist.
