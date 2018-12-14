inoremap kj <Esc>
set clipboard=unnamedplus
set nohlsearch
set scrolloff=1
set sidescrolloff=5
set autoread
filetype plugin indent on
set tabstop=4
set shiftwidth=4
set expandtab

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

nnoremap Y y$

map <leader>j <C-w>j
map <leader>k <C-w>k
map <leader>h <C-w>h
map <leader>l <C-w>l
map <leader>v <C-w>v
map <leader>s <C-w>s

map <leader>w :w<cr>
map <leader>q :close<cr>
map <leader>+ :only<cr>

nmap <C-j> <C-d>zz
nmap <C-k> <C-u>zz

vmap <leader>; gc
nmap <leader>; gc
nmap <leader>;; gc_

map <leader>f <Plug>(easymotion-bd-w)
map <leader>g <Plug>(easymotion-overwin-line)

" Line Numbers
set number relativenumber
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END
