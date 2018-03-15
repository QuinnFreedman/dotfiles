
call plug#begin('$LOCALAPPDATA/nvim')

" Plug 'reewr/vim-monokai-phoenix'
" Plug 'tamelion/neovim-molokai'

Plug 'ctrlpvim/ctrlp.vim'

" deoplete
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

call plug#end()

let g:deoplete#enable_at_startup = 1


imap jj <Esc>
set clipboard=unnamedplus
set nohlsearch
set scrolloff=1
set sidescrolloff=5
set autoread
