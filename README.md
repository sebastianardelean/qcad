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
- `G` toggles snapping, `W` toggles the wire tool, `R` rotates the selection or context-menu target, and `Esc` cancels the current action or deletes the selection.
- In wire mode, click a component to start, click the canvas to add a corner, and click another component to finish.
- The status bar reports world coordinates, zoom level, grid size, and snap state.

## Tests

```sh
npm test             # unit and jsdom integration tests
npm run test:coverage
```

Coverage reports are written to `coverage/` and the command enforces minimum
thresholds configured in `.c8rc.json`.
