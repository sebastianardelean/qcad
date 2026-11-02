# TODO List

## Preconditions

1. Implement only the items that are not strikethrough. 
2. Always use Test Driven Development.
3. Run automatic unit and integration tests.
4. Calculate code coverage. 
5. Try to reach 100% code coverage and no less than 60%.

## Features:

1. ~~Create a window that is splitted in 2 parts: Components and Canvas.~~
2. ~~20% of the window width shall be dedicated for the Components panel and the rest for the Canva~~
3. ~~In the Components panel add as rectangles:~~
   1. ~~Quantum register.~~
   2. ~~H,X,Y,Z,S,T,Sdag,Tdag,Rx,Ry,Rz,P, I gates.~~ 
4. ~~Make the all the components draggable.~~
5.~~Add the option to drag a quantum register from the Components panel onto the canvas. When dropped on the canvas open a popup window to configure the name of the register and the number of qubits.~~
6. ~~Add the option to select a component on the canvas using left mouse click.~~
7. ~~If SHIFT key is pressed, select multiple components placed on canvas.~~
8. ~~With left mouse button pressed, allow to move the selected component or components on the canvas.~~
9. ~~If the user right mouse button clicked on one component, show a contextual dialog. In the dialog have the following options:~~
   1. ~~Delete the component (shortcut ESC key)~~
   2. ~~Rotate the component clockwise by pi/4. (shortcut R)~~
10. ~~The popup window cannot be editable now.~~
11. ~~R key shortcut doesn't work.~~
12. ~~Rotate by pi/2~~
13. ~~ESC key doesn't work~~
14. ~~It still rotates by pi/4 instead of pi/2.~~
15. ~~On the canvas, add the option to draw wires between components.~~
16. ~~The wires shall be only horizontal or vertical, with 90 degree angles~~
17. ~~Do not allow to connect through wires 2 registers.~~
18. ~~Make it feel like a CAD tool.~~
19. ~~What is the use of snap on button? It doesn't work.~~
20. ~~Popup window when adding an register doesn't accept input.~~
21. ~~When in wire mode, click on a component to start a wire. If left mouse button is clicked again, the wire will snap to that location, being afterwards available to go up, down with the wire until the end component.~~
22. ~~Add a button (File -> Save)~~
23. ~~In project root resources/ there are the lexer and parser grammars for openqasm3.0. Save the drawn circuit as openqasm code.~~
24. ~~Remove Save QASM button~~
25. ~~add option to double click on each component (gate) connected to a register to specify on which qubits the gate is applied. It can be applied on multiple qubits in same register on multiple qubits in 1 or more registers.~~
25. For each single qubit gate, add in symbol a input and output port. 
26. If the gate will be applied on multiple qubits, when double clicking the gate, input the number of qubits. The change shall be reflected on the gate symbol, having multiple inputs and outputs.
27. For registers, add on the symbol on canvas, an output port for each qubit. If there are multiple qubits, then multiple ports.
