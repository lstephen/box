set nocompatible
set guioptions-=m
set guioptions-=T

filetype plugin on
set expandtab
set softtabstop=4
set shiftwidth=2
set autoindent

set wmh=0

map <C-J> <C-W>j<C-W>_
map <C-k> <C-W>k<C-W>_

map \o o<ESC><UP>
map \O O<ESC><DOWN>

map K i<Enter><ESC>

set guifont=Consolas:h10

map ,e :sp <C-R>=expand("%:p:h")."/"<CR>

set guioptions+=c
set nohlsearch
set noea
set foldmethod=indent
set foldlevel=1
set mps+=<:>
set mps+=?::
set list
set listchars=eol:·,tab:>-

set laststatus=2
set statusline=%f\ [%{&ff}]%=\ %c:%l\ %P

imap <buffer> ;cr <C-O>mzChange Request: <CR>Description: <CR>Action: <C-O>'z<ESC>$A
imap <buffer> ;cc <C-O>mz<C-R>=strftime("LS %d/%m/%y")<CR><CR><CR>Added<CR>Modified<CR>Deleted<CR><C-O>'z<ESC>jj

set backspace=2

