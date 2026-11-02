
const { contextBridge, ipcRenderer } = require("electron");

contextBridge.exposeInMainWorld("electronAPI", {
  ping: () => ipcRenderer.invoke("ping"),
  saveQasm: (content, suggestedName) =>
    ipcRenderer.invoke("save-qasm", { content, suggestedName }),
  onMenuSave: (callback) => {
    const listener = () => callback();
    ipcRenderer.on("menu-save", listener);
    return () => ipcRenderer.removeListener("menu-save", listener);
  }
});
