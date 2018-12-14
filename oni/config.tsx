import * as React from "react"
import * as Oni from "oni-api"

export const activate = (oni: Oni.Plugin.Api) => {
    console.log("config activated")

    //
    // Add input bindings here:
    //
    oni.input.bind("<c-enter>", () => console.log("Control+Enter was pressed"))

    oni.input.bind(["<down>", "<C-n>", "<C-j>"], "contextMenu.next")
    oni.input.bind(["<up>", "<C-p>", "<C-k>"], "contextMenu.previous")

    oni.input.bind(["<down>", "<C-n>", "<C-j>"], "menu.next")
    oni.input.bind(["<up>", "<C-p>", "<C-k>"], "menu.previous")

    //
    // Or remove the default bindings here by uncommenting the below line:
    //
    // oni.input.unbind("<c-p>")

}

export const deactivate = (oni: Oni.Plugin.Api) => {
    console.log("config deactivated")
}

export const configuration = {
    //add custom config here, such as

    "ui.colorscheme": "nord",
    //"oni.useDefaultConfig": true,
    //"oni.bookmarks": ["~/Documents"],
    "oni.loadInitVim": true,
    "editor.fontSize": "16px",
    "editor.fontFamily": "Consolas",
    "tabs.mode": "tabs",

    // UI customizations
    "ui.animations.enabled": false,
    "ui.fontSmoothing": "auto",
    "editor.renderer": "webgl",
    "learning.enabled": false
}
