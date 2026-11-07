
const { contextBridge, ipcRenderer } = require("electron");

function subscribe(channel, callback) {
  const listener = (_event, payload) => callback(payload);
  ipcRenderer.on(channel, listener);
  return () => ipcRenderer.removeListener(channel, listener);
}

contextBridge.exposeInMainWorld("electronAPI", {
  ping: () => ipcRenderer.invoke("ping"),
  openQasm: () => ipcRenderer.invoke("open-qasm"),
  saveQasm: (content, suggestedName) =>
    ipcRenderer.invoke("save-qasm", { content, suggestedName }),
  openFile: (filters) => ipcRenderer.invoke("open-file", filters),
  openFileAt: (filePath) => ipcRenderer.invoke("open-file-at", filePath),
  saveFile: (content, suggestedName, filters) =>
    ipcRenderer.invoke("save-file", { content, suggestedName, filters }),
  saveBinary: (base64, suggestedName, filters) =>
    ipcRenderer.invoke("save-binary", { base64, suggestedName, filters }),
  writeClipboard: (text) => ipcRenderer.invoke("write-clipboard", text),
  readClipboard: () => ipcRenderer.invoke("read-clipboard"),
  onMenuOpen: (callback) => subscribe("menu-open", callback),
  onMenuSave: (callback) => subscribe("menu-save", callback),
  onMenuSaveQasm: (callback) => subscribe("menu-save-qasm", callback),
  onMenuExport: (callback) => subscribe("menu-export", callback),
  onMenuLibrary: (callback) => subscribe("menu-library", callback),
  onMenuOpenRecent: (callback) => subscribe("menu-open-recent", callback)
});
