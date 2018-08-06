let config_path = has('win32') ? '$LOCALAPPDATA/nvim' : '~/.config/nvim' 

call plug#begin(config_path)

" Plug 'reewr/vim-monokai-phoenix'
" Plug 'tamelion/neovim-molokai'
Plug 'altercation/vim-colors-solarized'

Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'easymotion/vim-easymotion'
Plug 'danro/rename.vim'

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

"Plug 'Shougo/echodoc.vim'
"Plug 'junegunn/fzf.vim'
Plug '/usr/local/opt/fzf'


" deoplete
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'ianks/vim-tsx'
Plug 'leafgarland/typescript-vim'
call plug#end()

augroup filetypedetect
    autocmd BufEnter *.tsx set filetype=typescript
    autocmd BufEnter *.ts set filetype=typescript
augroup end

" ##############################
"        Language Client
" ##############################

set hidden

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'stable', 'rls'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'javascript.jsx': ['javascript-typescript-stdio'],
    \ 'typescript': ['javascript-typescript-stdio'],
    \ 'python': ['pyls'],
    \ }

let g:LanguageClient_rootMarkers = {
    \ 'javascript': ['package.json'],
    \ 'rust': ['Cargo.toml'],
    \ }
let g:LanguageClient_autoStart = 1

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> gj :call LanguageClient#textDocument_references()<CR>:call lopen<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <leader>r :call LanguageClient#textDocument_rename()<CR>
noremap <silent><F3> :call LanguageClient_textDocument_codeAction()<CR>

autocmd BufWritePre * :call LanguageClient_textDocument_formatting()



let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_completion_start_length = 1
let g:deoplete#enable_smart_case = 1
call deoplete#custom#source('LanguageClient',
            \ 'min_pattern_length',
            \ 2)
"let g:deoplete#deoplete_onmni_patterns = get(g:, 'deoplete#force_omni_input_patterns', {})
"let g:deoplete#deoplete_onmni_patterns.javascript = '[^. \t]\.\w*'

inoremap <silent><expr> <TAB>
    \ pumvisible() ? "\<down>" :
    \ <SID>check_back_space() ? "\<TAB>" :
    \ "\<C-x><C-o>"
function! s:check_back_space() abort "{{{
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
endfunction"}}}

"inoremap <expr><Enter>
    "\ pumvisible() ? deoplete#close_popup() : 
    "\ "\<Enter>"


imap kj <Esc>
set clipboard=unnamedplus
set nohlsearch
set scrolloff=1
set sidescrolloff=5
set autoread
filetype plugin indent on
set tabstop=4
set shiftwidth=4
set expandtab
if has("gui_running") || has("gui_vimr")
    syntax enable
    set background=dark
    colorscheme desert
endif

nnoremap <SPACE> <Nop>
let mapleader=" "

nnoremap <C-n> :lnext<CR>
nnoremap <C-m> :lprevious<CR>

nnoremap c "_c
nnoremap C "_C
nnoremap x "_x
nnoremap s "_s
vnoremap p "_dP
vnoremap x "_x
vnoremap s "_s

map <leader>j <C-w>j
map <leader>k <C-w>k
map <leader>h <C-w>h
map <leader>l <C-w>l

map <leader>v <C-w>v
map <leader>s <C-w>s

map <leader>e :execute 'edit' config_path . '/init.vim'<cr>

map <leader>w :w<cr>
map <leader>q :q<cr>

nmap <C-j> <C-d>zz
nmap <C-k> <C-u>zz

noremap <leader>; :call NERDComment(0,"toggle")<CR>
vnoremap <leader>; :call NERDComment(0,"toggle")<CR>

map <leader>f <Plug>(easymotion-bd-w)
