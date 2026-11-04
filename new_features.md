# New Features for QCAD (Quantum Circuit CAD)

## Overview

QCAD is a ClojureScript + Electron desktop app for building and editing quantum circuits with OpenQASM 3.0 import/export. This document proposes concrete, implementable features that would make it more useful for quantum circuit design, simulation prep, and education.

## High-Priority Features

### 1. Measurement & Classical Bits
**Impact:** High | **Effort:** Medium

- **Add measurement gates** (`Measure` / `M`) that connect quantum wires to classical bits
- **Classical register support** - separate from quantum registers; display differently on canvas
- **Classical wire visualization** - distinguish from quantum wires (different style/color)
- **OpenQASM support** - emit/parse `bit[n] c;` and `measure q -> c[0];` or `c = measure q;` forms
- **Reset gates** - `reset q;` for qubit initialization

*Rationale:* Essential for real quantum programs; most circuits need measurement. OpenQASM 3.0 already supports classical bits.

### 2. Controlled & Multi-Qubit Gates
**Impact:** High | **Effort:** Medium

- **CNOT/CX gate** - Add as built-in component (common, currently only available via custom/import)
- **CZ, SWAP gates** - Standard two-qubit gates
- **Controlled-U gates** - UI to create controlled versions of existing gates
- **Toffoli (CCX)** - Three-qubit gate support
- **Multi-control visualization** - Draw control dots connected by lines to targets
- **OpenQASM generation** - Use built-in gates where possible, else controlled modifiers

*Rationale:* Core quantum computing operations. CNOT is referenced in tests but not in the component palette.

### 3. Improved Parameterized Gates
**Impact:** High | **Effort:** Medium

- **Editable angles for Rx/Ry/Rz/P** - Currently hardcoded to `pi/2` in export; add angle editor
- **Angle input dialog** - Double-click or property panel to set angle (supports `pi`, `pi/2`, fractions, decimals)
- **Common presets** - Quick buttons for `pi/2`, `pi`, `pi/4`
- **Parameter expressions** - Support symbolic expressions in OpenQASM
- **Visual indication** - Show angle on gate label when non-default

*Rationale:* Currently rotations always export as `pi/2`, severely limiting utility.

## Medium-Priority Features

### 4. Undo/Redo System
**Impact:** High | **Effort:** Medium-High

- **History stack** - Track state changes (add/remove/move/rotate/wire/connect)
- **Keyboard shortcuts** - `Ctrl+Z` / `Ctrl+Y` (or `Cmd+Z`/`Cmd+Shift+Z` on Mac)
- **Coalescing** - Merge rapid moves/drags into single undo step
- **Memory management** - Limit history depth (e.g., 50-100 steps)
- **UI indicators** - Optional undo/redo buttons in toolbar

*Rationale:* Essential for any CAD/editor tool. Currently no way to undo mistakes.

### 5. Copy/Paste & Duplicate
**Impact:** High | **Effort:** Medium

- **Copy selection** - `Ctrl+C` copies selected items with relative positions
- **Paste** - `Ctrl+V` pastes at cursor or offset position
- **Duplicate** - `Ctrl+D` for quick duplication
- **Wire preservation** - Copy internal wires between copied items
- **Cross-circuit paste** - Serialize selection to JSON for clipboard

*Rationale:* Speeds up circuit construction significantly.

### 6. Circuit Simulation/State Preview
**Impact:** High | **Effort:** High

- **State vector simulator** - Built-in simple simulator for small circuits (n <= 12-16 qubits)
- **Bloch sphere visualization** - Show single-qubit states
- **Probability histogram** - Measurement probability distribution
- **Simulation panel** - Dockable panel showing results
- **Real-time preview** - Optional live simulation on circuit changes
- **Basis state display** - Show amplitudes

*Rationale:* Huge educational value; lets users verify circuits without leaving the app. Could use a pure ClojureScript implementation or WASM.

### 7. Barrier & Annotations
**Impact:** Medium | **Effort:** Low

- **Barrier gates** - Visual separator across selected qubits, emits `barrier q[0],q[1];`
- **Text annotations** - Place labels/comments on canvas
- **Box/grouping** - Visually group subcircuits
- **Metadata** - Circuit name, author, description

*Rationale:* Helps organize complex circuits and matches OpenQASM features.

### 8. Image Export
**Impact:** Medium | **Effort:** Medium

- **Export as PNG** - Render canvas to image
- **Export as SVG** - Vector graphics for publications
- **Export as PDF** - Via Electron
- **Options dialog** - Resolution, transparent background, include/exclude UI
- **LaTeX/TikZ export** - For academic papers (quantikz)

*Rationale:* Needed for sharing circuits in papers, presentations, slides.

## Lower-Priority / Nice-to-Have

### 9. Circuit Library & Templates
**Impact:** Medium | **Effort:** Medium

- **Built-in templates** - Bell state, GHZ, QFT, Grover, teleportation
- **User library** - Save custom circuits as reusable subcircuits
- **Template browser** - Dialog to insert pre-built circuits
- **Subcircuit instantiation** - Drop saved circuit as a block

### 10. Improved UX
**Impact:** Medium | **Effort:** Low-Medium

- **Property panel** - Side panel showing selected item properties
- **Grid snapping options** - Configurable grid size, snap to components
- **Zoom to fit** - Fit entire circuit in view
- **Minimap** - Overview for large circuits
- **Dark mode** - Theme support
- **Search components** - Filter component palette
- **Recent files** - File menu history

### 11. Advanced OpenQASM Features
**Impact:** Medium | **Effort:** Medium

- **OpenQASM 2.0 support** - Compatibility with older files
- **QASM 3.0 features** - Switch, loops, classical control flow
- **Stdlib gates** - Better support for `stdgates.inc`
- **Gate definitions import** - Better handling of opaque/gate definitions
- **Include files** - Support for custom includes

### 12. Collaboration & Versioning
**Impact:** Low | **Effort:** High

- **Save as JSON** - Native format (separate from QASM) preserving all metadata
- **Auto-save** - Periodic saves with recovery
- **Diff view** - Compare circuit versions
- **Git integration** - Basic commit from app

## Implementation Recommendations

### Architecture Notes

For this codebase (ClojureScript, atoms for state, shadow-cljs, Electron):

1. **Measurement/Classical bits** - Extend model with `:classical-registers`, `:classical-wires`. Update rendering in `dom.cljs`, OpenQASM in `qasm.cljs`.
2. **Undo/redo** - Add `:history` and `:history-index` to state; use middleware/wrappers around state changes or snapshots.
3. **Angle editing** - Extend gate items with `:angle` property; update dialogs and QASM export.
4. **Simulation** - Could be added as separate namespace `app.simulator`, keep UI in separate panel.
5. **Copy/paste** - Use Electron clipboard API or browser clipboard; serialize to EDN/JSON.

### Quick Wins (Easy to implement)
- Add CX, CZ, SWAP to components palette
- Add barrier component
- Fix hardcoded `pi/2` angle (make it configurable)
- Add zoom-to-fit command
- Add keyboard shortcuts for delete/rotate (already mostly there)

### Testing
All new features should include unit tests in the existing test structure (`test/app/*_test.cljs`) following current patterns.

## Suggested Next Step

**Start with "Measurement & Classical Bits" or "Controlled & Multi-Qubit Gates"** - these are fundamental to quantum computing and integrate well with the existing port/wire architecture.
