# QCAD

A ClojureScript + Electron desktop application for building quantum circuits.

## Development

```sh
npm install
npm run watch:cljs   # compile the renderer in watch mode
npm start            # launch Electron
```

Build the production renderer bundle with `npm run build:cljs`.

## Canvas controls

- Drag components from the panel onto the canvas; placement snaps to a 20 px grid.
- Left click selects, Shift+click toggles, and dragging empty canvas draws a marquee selection.
- Drag a selection to move it; the middle mouse button pans the canvas.
- Use the mouse wheel to zoom around the pointer and press `0` to reset the view.
- `G` toggles snapping, `A` toggles alignment to nearby components, `W` toggles the wire tool, `R` rotates the selection or context-menu target, and `Esc` cancels the current action or deletes the selection.
- In wire mode, click an output port to start, click the canvas to add a corner, and click an input port to finish. Wires snap to ports rather than component centers.
- Double-click any gate to set its **Number of qubits**. The gate grows one input/output port pair per qubit; connect each input port to the register qubit it should act on. Control qubits are drawn as filled dots joined to the gate.
- Drag a library template or saved circuit from the **Circuit Library** to insert it as a labelled block; the dialog closes after the drop.
- Drop the **Custom** component to open a dialog for its qubit count and an OpenQASM gate definition. Double-click it to edit the definition later.
- Gates display input and output ports, with one pair per configured qubit. Registers display one output port per qubit.
- The status bar reports world coordinates, zoom level, grid size, and snap state.

## Opening OpenQASM

Use **File → Open** (or `Ctrl+O`) to load an OpenQASM file and draw it on the
canvas automatically. The importer recognizes `qubit[n] name;` declarations,
built-in gates from the components panel, register or indexed operands, and
custom `gate` declarations with their calls. Register operands such as
`h data;` expand to every qubit in the register, and chained gate calls are
reconnected through their ports. Unsupported gate calls are preserved as
custom gates using their original QASM call text. Opaque declarations are
imported like gate definitions, and control-flow blocks such as `if`, `for`,
`while`, and `switch` are preserved as raw QASM statements.

## Saving OpenQASM

Use **File → Save** (or `Ctrl+S`) to export the circuit as OpenQASM 3.0.
Registers become `qubit[n] name;` declarations. A gate is emitted once per
wired input port, using indexed operands such as `h data[0];`; input-port order
selects the emitted qubit order. Gates connected through port wires propagate
their target qubit through gate chains. Legacy gates without wired ports still
apply to every qubit in each connected register. Gates with no register
connection are omitted from the output with a comment. Rotations without an
editable angle use `pi/2`.

Custom gates enter a full OpenQASM gate declaration such as
`gate pair a, b { h a; cx a, b; }`. The declaration is emitted once, and the
gate is called with the operands wired to its input ports, for example
`pair data[0], data[1];`. A custom gate whose field is a complete statement
rather than a `gate` declaration is emitted verbatim. Incomplete custom gates
are omitted with a comment instead of emitting a call with the wrong number of
operands.

Image export is available from **File → Export Image**; the options dialog
supports SVG or PNG, a scale factor, a transparent background, and including
the canvas grid.

## Tests

```sh
npm test             # unit and jsdom integration tests
npm run test:coverage
```

Coverage reports are written to `coverage/` and the command enforces minimum
thresholds configured in `.c8rc.json`.



opencode -s ses_e4ef75311ffe0GQFlL9Gr0O0T1