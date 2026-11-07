
const { app, BrowserWindow, Menu, dialog, ipcMain, clipboard } = require("electron");
const fs = require("node:fs/promises");
const path = require("node:path");

const recentFiles = [];
let mainWindow = null;

function addRecentFile(filePath) {
  const index = recentFiles.indexOf(filePath);
  if (index !== -1) recentFiles.splice(index, 1);
  recentFiles.unshift(filePath);
  if (recentFiles.length > 8) recentFiles.length = 8;
  if (mainWindow && !mainWindow.isDestroyed()) {
    createApplicationMenu(mainWindow);
  }
}

function createApplicationMenu(win) {
  const recentSubmenu = recentFiles.length === 0
    ? [{ label: "No recent files", enabled: false }]
    : recentFiles.map((filePath) => ({
        label: path.basename(filePath),
        toolTip: filePath,
        click: () => win.webContents.send("menu-open-recent", filePath)
      }));

  const template = [
    {
      label: "File",
      submenu: [
        {
          label: "Open...",
          accelerator: "CmdOrCtrl+O",
          click: () => win.webContents.send("menu-open")
        },
        {
          label: "Open Recent",
          submenu: recentSubmenu
        },
        {
          label: "Save",
          accelerator: "CmdOrCtrl+S",
          click: () => win.webContents.send("menu-save")
        },
        {
          label: "Save as OpenQASM...",
          accelerator: "CmdOrCtrl+Shift+S",
          click: () => win.webContents.send("menu-save-qasm")
        },
        { type: "separator" },
        {
          label: "Export Image...",
          accelerator: "CmdOrCtrl+E",
          click: () => win.webContents.send("menu-export")
        },
        {
          label: "Circuit Library...",
          accelerator: "CmdOrCtrl+Shift+L",
          click: () => win.webContents.send("menu-library")
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
    width: 1100,
    height: 700,
    webPreferences: {
      preload: path.join(__dirname, "preload.cjs"),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true
    }
  });

  mainWindow = win;
  win.on("closed", () => {
    if (mainWindow === win) mainWindow = null;
  });
  createApplicationMenu(win);
  win.loadFile(path.join(__dirname, "index.html"));
}

function normalizeFilters(filters, fallback) {
  if (!Array.isArray(filters) || filters.length === 0) return fallback;
  return filters
    .filter((filter) => filter && typeof filter.name === "string")
    .map((filter) => ({
      name: filter.name,
      extensions: Array.isArray(filter.extensions)
        ? filter.extensions.map(String)
        : ["*"]
    }));
}

app.whenReady().then(() => {
  ipcMain.handle("ping", () => "pong from Electron");

  ipcMain.handle("write-clipboard", (_event, text) => {
    if (typeof text !== "string") {
      throw new TypeError("write-clipboard requires a string");
    }
    clipboard.writeText(text);
    return true;
  });

  ipcMain.handle("read-clipboard", () => clipboard.readText());

  ipcMain.handle("open-qasm", async (event) => {
    const win = BrowserWindow.fromWebContents(event.sender);
    const { canceled, filePaths } = await dialog.showOpenDialog(win, {
      title: "Open OpenQASM circuit",
      properties: ["openFile"],
      filters: [
        { name: "OpenQASM", extensions: ["qasm"] },
        { name: "All files", extensions: ["*"] }
      ]
    });

    if (canceled || !filePaths || filePaths.length === 0) {
      return { canceled: true };
    }

    const filePath = filePaths[0];
    const content = await fs.readFile(filePath, "utf8");
    addRecentFile(filePath);
    return { canceled: false, filePath, content };
  });

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
    addRecentFile(filePath);
    return { canceled: false, filePath };
  });

  ipcMain.handle("open-file", async (event, filters) => {
    const win = BrowserWindow.fromWebContents(event.sender);
    const { canceled, filePaths } = await dialog.showOpenDialog(win, {
      title: "Open QCAD circuit",
      properties: ["openFile"],
      filters: normalizeFilters(filters, [
        { name: "QCAD circuit", extensions: ["json", "qasm"] },
        { name: "All files", extensions: ["*"] }
      ])
    });

    if (canceled || !filePaths || filePaths.length === 0) {
      return { canceled: true };
    }

    const filePath = filePaths[0];
    const content = await fs.readFile(filePath, "utf8");
    addRecentFile(filePath);
    return { canceled: false, filePath, content };
  });

  ipcMain.handle("open-file-at", async (_event, filePath) => {
    if (typeof filePath !== "string" || filePath.length === 0) {
      throw new TypeError("open-file-at requires a file path");
    }
    const content = await fs.readFile(filePath, "utf8");
    addRecentFile(filePath);
    return { canceled: false, filePath, content };
  });

  ipcMain.handle("save-file", async (event, payload = {}) => {
    const content = payload && typeof payload.content === "string"
      ? payload.content
      : null;

    if (content === null) {
      throw new TypeError("save-file requires a string content value");
    }

    const requestedName = typeof payload.suggestedName === "string"
      ? payload.suggestedName.trim()
      : "";
    const suggestedName = requestedName ? path.basename(requestedName) : "circuit.json";
    const win = BrowserWindow.fromWebContents(event.sender);
    const { canceled, filePath } = await dialog.showSaveDialog(win, {
      title: "Save circuit",
      defaultPath: suggestedName,
      filters: normalizeFilters(payload.filters, [
        { name: "QCAD circuit", extensions: ["json"] },
        { name: "All files", extensions: ["*"] }
      ])
    });

    if (canceled || !filePath) {
      return { canceled: true };
    }

    await fs.writeFile(filePath, content, "utf8");
    addRecentFile(filePath);
    return { canceled: false, filePath };
  });

  ipcMain.handle("save-binary", async (event, payload = {}) => {
    const base64 = payload && typeof payload.base64 === "string"
      ? payload.base64
      : null;

    if (base64 === null) {
      throw new TypeError("save-binary requires a base64 string");
    }

    const requestedName = typeof payload.suggestedName === "string"
      ? payload.suggestedName.trim()
      : "";
    const suggestedName = requestedName ? path.basename(requestedName) : "circuit.png";
    const win = BrowserWindow.fromWebContents(event.sender);
    const { canceled, filePath } = await dialog.showSaveDialog(win, {
      title: "Save image",
      defaultPath: suggestedName,
      filters: normalizeFilters(payload.filters, [
        { name: "PNG image", extensions: ["png"] },
        { name: "All files", extensions: ["*"] }
      ])
    });

    if (canceled || !filePath) {
      return { canceled: true };
    }

    await fs.writeFile(filePath, Buffer.from(base64, "base64"));
    addRecentFile(filePath);
    return { canceled: false, filePath };
  });

  createWindow();
});

app.on("window-all-closed", () => {
  if (process.platform !== "darwin") app.quit();
});
