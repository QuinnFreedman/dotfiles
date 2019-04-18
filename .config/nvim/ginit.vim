
let s:black = [ '#232526', 233 ] " 272822
let s:gray = [ '#808080', 244 ]
let s:white = [ '#f8f8f2', 234 ]
let s:cyan = [ '#66d9ef', 81 ]
let s:green = [ '#a6e22e', 118 ]
let s:orange = [ '#ef5939', 166 ]
let s:pink = [ '#f92672', 161 ]
let s:red = [ '#ff0000', 160 ]
let s:yellow = [ '#e6db74', 229 ]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

let s:p.normal.left = [ [ s:black, s:cyan ], [ s:gray, s:black ] ]
let s:p.normal.middle = [ [ s:orange, s:black ] ]
let s:p.normal.right = [ [ s:black, s:cyan ], [ s:cyan, s:black ] ]
let s:p.normal.error = [ [ s:pink, s:black ] ]
let s:p.normal.warning = [ [ s:yellow, s:black ] ]
let s:p.insert.left = [ [ s:black, s:green ], [ s:gray, s:black ] ]
let s:p.visual.left = [ [ s:black, s:yellow ], [ s:gray, s:black ] ]
let s:p.replace.left = [ [ s:black, s:red ], [ s:red, s:black ] ]
let s:p.inactive.left =  [ [ s:gray, s:black ], [ s:gray, s:black ] ]
let s:p.inactive.middle = [ [ s:gray, s:black ] ]
let s:p.inactive.right = [ [ s:black, s:gray ], [ s:gray, s:black ] ]
let s:p.tabline.left = [ [ s:pink, s:black ] ]
let s:p.tabline.middle = [ [ s:pink, s:black] ]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.tabline.tabsel = [ [ s:black, s:pink ] ]

let g:lightline#colorscheme#monokai#palette = lightline#colorscheme#flatten(s:p)


colorscheme monokai
Guifont Hack Nerd Font Mono:h13
if exists('g:lightline')
    let g:lightline.colorscheme='monokai'
endif
call lightline#init()
call lightline#update()
