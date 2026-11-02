
const { app, BrowserWindow, Menu, dialog, ipcMain } = require("electron");
const fs = require("node:fs/promises");
const path = require("node:path");

function createApplicationMenu(win) {
  const template = [
    {
      label: "File",
      submenu: [
        {
          label: "Save",
          accelerator: "CmdOrCtrl+S",
          click: () => win.webContents.send("menu-save")
        },
        { type: "separator" },
        { role: "quit" }
      ]
    },
    {
      label: "View",
      submenu: [
        { role: "reload" },
        { role: "toggleDevTools" },
        { type: "separator" },
        { role: "resetZoom" },
        { role: "zoomIn" },
        { role: "zoomOut" }
      ]
    }
  ];

  Menu.setApplicationMenu(Menu.buildFromTemplate(template));
}

function createWindow() {
  const win = new BrowserWindow({
    width: 900,
    height: 650,
    webPreferences: {
      preload: path.join(__dirname, "preload.cjs"),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true
    }
  });

  createApplicationMenu(win);
  win.loadFile(path.join(__dirname, "index.html"));
}

app.whenReady().then(() => {
  ipcMain.handle("ping", () => "pong from Electron");

  ipcMain.handle("save-qasm", async (event, payload = {}) => {
    const content = payload && typeof payload.content === "string"
      ? payload.content
      : null;

    if (content === null) {
      throw new TypeError("save-qasm requires a string content value");
    }

    const requestedName = typeof payload.suggestedName === "string"
      ? payload.suggestedName.trim()
      : "";
    const suggestedName = requestedName ? path.basename(requestedName) : "circuit.qasm";
    const win = BrowserWindow.fromWebContents(event.sender);
    const { canceled, filePath } = await dialog.showSaveDialog(win, {
      title: "Save OpenQASM circuit",
      defaultPath: suggestedName,
      filters: [
        { name: "OpenQASM", extensions: ["qasm"] },
        { name: "All files", extensions: ["*"] }
      ]
    });

    if (canceled || !filePath) {
      return { canceled: true };
    }

    await fs.writeFile(filePath, content, "utf8");
    return { canceled: false, filePath };
  });

  createWindow();
});

app.on("window-all-closed", () => {
  if (process.platform !== "darwin") app.quit();
});
