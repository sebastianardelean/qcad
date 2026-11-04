
const { contextBridge, ipcRenderer } = require("electron");

contextBridge.exposeInMainWorld("electronAPI", {
  ping: () => ipcRenderer.invoke("ping"),
  openQasm: () => ipcRenderer.invoke("open-qasm"),
  saveQasm: (content, suggestedName) =>
    ipcRenderer.invoke("save-qasm", { content, suggestedName }),
  onMenuOpen: (callback) => {
    const listener = () => callback();
    ipcRenderer.on("menu-open", listener);
    return () => ipcRenderer.removeListener("menu-open", listener);
  },
  onMenuSave: (callback) => {
    const listener = () => callback();
    ipcRenderer.on("menu-save", listener);
    return () => ipcRenderer.removeListener("menu-save", listener);
  }
});
