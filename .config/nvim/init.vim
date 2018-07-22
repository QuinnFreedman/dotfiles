
call plug#begin('$LOCALAPPDATA/nvim')

" Plug 'reewr/vim-monokai-phoenix'
" Plug 'tamelion/neovim-molokai'

Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdcommenter'

"Plug 'w0rp/ale'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

"Plug 'steelsojka/deoplete-flow', { 'for': 'javascript' }

" deoplete
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

call plug#end()

" ##############################
"        Language Client
" ##############################

" Required for operations modifying multiple buffers like rename.
set hidden

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'javascript': ['flow-language-server', '--stdio'],
    \ 'javascript.jsx': ['flow-language-server', '--stdio'],
    \ 'python': ['pyls'],
    \ }

let g:LanguageClient_rootMarkers = {
    \ 'javascript': ['package.json'],
    \ 'rust': ['Cargo.toml'],
    \ }

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> gj :call LanguageClient#textDocument_references()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent> <leader>r :call LanguageClient#textDocument_rename()<CR>




let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_completion_start_length = 1
let g:deoplete#enable_smart_case = 1
let g:deoplete#deoplete_onmni_patterns = get(g:, 'deoplete#force_omni_input_patterns', {})
let g:deoplete#deoplete_onmni_patterns.javascript = '[^. \t]\.\w*'

inoremap <silent><expr> <TAB>
    \ pumvisible() ? "\<C-n>" :
    \ <SID>check_back_space() ? "\<TAB>" :
    \ "\<C-n>"
function! s:check_back_space() abort "{{{
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
endfunction"}}}

inoremap <expr><Enter>
    \ pumvisible() ? deoplete#close_popup() : 
    \ "\<Enter>"



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
colorscheme monokai
"Guifont operator mono:h13
"Guifont Consolas:h14

nnoremap <SPACE> <Nop>
let mapleader=" "

nnoremap <C-n> :lnext<CR>
nnoremap <C-m> :lprevious<CR>

map <leader>j <C-w>j
map <leader>k <C-w>k
map <leader>h <C-w>h
map <leader>l <C-w>l

map <leader>v <C-w>v
map <leader>s <C-w>s

map <leader>e :e ~/AppData/Local/nvim/init.vim<cr>

map <leader>w :w<cr>
map <leader>q :q<cr>

nmap <C-j> <C-d>zz
nmap <C-k> <C-u>zz

noremap <leader>; :call NERDComment(0,"toggle")<CR>
vnoremap <leader>; :call NERDComment(0,"toggle")<CR>

