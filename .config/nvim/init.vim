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

set lazyredraw
set regexpengine=1

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

nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>h <C-w>h
nnoremap <leader>l <C-w>l
nnoremap <leader>v <C-w>v
nnoremap <leader>s <C-w>s

nnoremap <leader>w :w<cr>
nnoremap <leader>q :close<cr>
nnoremap <leader>+ :only<cr>

nmap <C-j> <C-d>zz
nmap <C-k> <C-u>zz

vmap <leader>; gc
nmap <leader>; gc
nmap <leader>;; gc_

map <leader>1 1gt
map <leader>2 2gt
map <leader>3 3gt
map <leader>4 4gt
map <leader>5 5gt
map <leader>6 6gt
map <leader>7 7gt
map <leader>8 8gt
map <leader>9 9gt
map <leader>t :tabnew<CR>

" *******************************************
" Appearance
" *******************************************
set fillchars+=vert:\ 
set t_Co=16
hi EndOfBuffer guibg=NONE guifg=#90908a
hi LineNr ctermbg=0
hi! link SignColumn LineNr
hi! link VertSplit LineNr
hi CursorLine cterm=NONE ctermbg=16 ctermfg=NONE gui=NONE


hi ActiveWindow cterm=None gui=None
hi! link InactiveWindow LineNr

" Call method on window enter
augroup WindowManagement
  autocmd!
  autocmd WinEnter * call HandleWinEnter()
  autocmd WinLeave * call HandleWinExit()
augroup END

" Change highlight group of active/inactive windows
function! HandleWinEnter()
  " setlocal winhighlight=Normal:ActiveWindow,NormalNC:InactiveWindow
  set relativenumber
  set cursorline
  " ownsyntax
endfunction

function! HandleWinExit()
  set nocursorline
  set norelativenumber
  set number
  " ownsyntax off
endfunction

autocmd VimResized * :wincmd =
autocmd VimEnter,WinEnter,BufWInEnter * :wincmd =

" Auto resize splits
set winwidth=104
" set winminwidth=5
set winheight=10
set winminheight=10
set winheight=999

" *******************************************
" Ctrl D
" *******************************************
nnoremap <C-d> *Ncgn


" *******************************************
" Between
" *******************************************
vnoremap au :<C-U>silent! normal! F_vf_<CR>
vnoremap iu :<C-U>silent! normal! T_vt_<CR>

omap au :normal vau<CR>
omap iu :normal viu<CR>



" ********************************************
"           Plugins
" ********************************************

call plug#begin('~/.vim/plugged')

Plug 'machakann/vim-highlightedyank'
Plug 'tpope/vim-commentary'
Plug 'easymotion/vim-easymotion' 

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'sickill/vim-monokai'

Plug 'itchyny/lightline.vim'


Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}

call plug#end()

let g:highlightedyank_highlight_duration = 300
hi HighlightedyankRegion ctermbg=7 guibg=LightGray

nmap <leader>f <Plug>(easymotion-bd-w)
nmap s <Plug>(easymotion-bd-f)
nmap <leader>L <Plug>(easymotion-overwin-line)

nnoremap <leader>of :FZF<CR>
nnoremap <leader>ob :Buffers<CR>
nnoremap <leader>om :Marks<CR>
nnoremap <leader>og :GFiles<CR>
nnoremap <leader>ow :Windows<CR>


" if hidden is not set, TextEdit might fail.
set hidden

" Better display for messages
" set cmdheight=2

" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
" set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> for trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[c` and `]c` for navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)
nmap <silent> gp <Plug>(coc-diagnostic-prev)
nmap <silent> gn <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K for show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
vmap <leader>m <Plug>(coc-format-selected)
nmap <leader>m <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
vmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
" nmap <leader>qf  <Plug>(coc-fix-current)

" Use `:Format` for format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` for fold current buffer
command! -nargs=? Fold :call   CocAction('fold', <f-args>)


" \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
" \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" },
function! lightline#colorscheme#background() abort
    return 'dark'
endfunction
let g:lightline = {
    \ 'component': {
    \   'lineinfo': "\ue0a1 %3l:%-2v",
    \ },
    \ 'component_function': {
    \   'readonly': 'LightlineReadonly',
    \   'fugitive': 'LightlineFugitive',
    \   'cocstatus': 'coc#status'
    \ },
    \ 'colorscheme': '16color',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'cocstatus', 'readonly', 'filename', 'modified' ] ],
    \   'right': [ ['lineinfo'], ['percent'] ]
    \ },
    \ }
function! LightlineReadonly()
    return &readonly ? '' : ''
endfunction
function! LightlineFugitive()
    if exists('*fugitive#head')
        let branch = fugitive#head()
        return branch !=# '' ? "\ue0a0".branch : ''
    endif
    return ''
endfunction

let g:coc_status_warning_sign=''
let g:coc_status_error_sign=''

" hi CocUnderline guibg=Red

" " Using CocList
" " Show all diagnostics
" nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" " Manage extensions
" nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" " Show commands
" nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" " Find symbol of current document
" nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" " Search workspace symbols
" nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" " Do default action for next item.
" nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" " Do default action for previous item.
" nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" " Resume latest coc list
" nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
"

