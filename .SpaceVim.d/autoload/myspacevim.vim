func! myspacevim#before() abort
    call rpcnotify(0, 'Gui', 'WindowMaximized', 1)
    call SpaceVim#custom#SPC('nore', ['f', 'v', 'e'], ':e ~/.SpaceVim.d/autoload/myspacevim.vim', 'myspacevim.vim', 1)
    call SpaceVim#layers#disable('core#statusline')
endf

func! myspacevim#after() abort
    iunmap jk
    inoremap kj <Esc>
    set nohlsearch
    set scrolloff=1
    set sidescrolloff=5
    nnoremap c "_c
    nnoremap C "_C
    nnoremap x "_x
    nnoremap s "_s
    vnoremap p "_dP
    vnoremap x "_x
    vnoremap s "_s
    nmap <C-j> <C-d>zz
    nmap <C-k> <C-u>zz

    call neomake#configure#automake('nwr', 500)
    set clipboard=unnamedplus
    let g:airline#extensions#whitespace#enabled = 0
    " let g:deoplete#ignore_sources = {'*': 'neco-look'}
    let g:deoplete#ignore_sources = get(g:,'deoplete#ignore_sources',{})
    let g:deoplete#ignore_sources.python = ['look']
    let g:deoplete#ignore_sources.vim = ['look']
    let g:deoplete#ignore_sources.rust = ['look']
endf
