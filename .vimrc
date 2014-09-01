".vimrc file for setting on startup

"Setting up NeoBundle
if has('vim_starting')
set nocompatible    
set runtimepath+=~/.vim/bundle/neobundle.vim/
endif  
call neobundle#begin(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'
"Call all packages
NeoBundle 'LaTeX-Box-Team/LaTeX-Box', {'rev' : '61528a'}
NeoBundle 'scrooloose/nerdtree', {'rev' : 'b0bb78'}
"NeoBundle 'ivanov/vim-ipython', {'rev' : 'fa8c9b'}
NeoBundle 'Lokaltog/vim-powerline', {'rev' : '09c0ce'}
NeoBundle 'benmills/vimux'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'Lokaltog/vim-easymotion'
NeoBundle 'junegunn/goyo.vim'
NeoBundle 'junegunn/limelight.vim'
NeoBundle 'godlygeek/tabular'
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'SirVer/ultisnips'
NeoBundle 'honza/vim-snippets'
NeoBundle 'tpope/vim-surround'
call neobundle#end()

"automatically reload the .vimrc file
autocmd! bufwritepost .vimrc source %

"only show 5 words when suggesting spelling
set spellsuggest=5

"set vim to noncompatible mode which provides the most features
set nocompatible

"set text wrap automatically and linebreak so that words are not broken between
"lines
set wrap
"set textwidth=80
set linebreak

"set vim to use the same indent as the previous line
set autoindent

"toggle set list
nmap <leader>l :set list!<CR>

"alter the deafult behaviour of j and k for long lines of softwrapped text.
"This is most useful for latex files which have very long lines.
function! Changejk()
    nnoremap j gj
    nnoremap k gk
    vnoremap j gj
    vnoremap k gk
endfunction

"open .vimrc quickly
nnoremap <leader>v :e ~/.vimrc<CR>

"invisible characters
set listchars=tab:▸\ ,eol:¬

"set the number of commands the vim holds in its register
set history=700
set undolevels=700

"better copy and paste
set pastetoggle=<F2>
"set clipboard=unnamedplus

"display current cursor position in the bottom right of the screen and show incomplete commands
set ruler
set showcmd

"remap , to <leader> key
let mapleader = ","

"bind Ctrl+<movement> keys to move around the windows, instead of using
"Ctrl+w+<movement>

map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

"remap escape in insert mode
inoremap jk <ESC>
inoremap jj <ESC>
inoremap kj <ESC>

"show dollar sign at the end of a selection
set cpoptions+=$

"easier moving between tabs
map <Leader>n <esc>:tabprevious<CR>
map <Leader>m <esc>:tabnext<CR>

"map for running vimux/matlab command
nmap <Leader>r :call VimuxRunCommand("program('in.param')")<CR>

"map sort function! OmniPopup(action)
" ""     if pumvisible()
" ""         if a:action == 'j'
" ""             return "\<C-N>"
" ""         elseif a:action == 'k'
" ""             return "\<C-P>"
" ""         endif
" ""     endif
" ""     return a:action
" "" endfunction
"
" "" inoremap <silent><C-j> <C-R>=OmniPopup('j')<CR>
" "" inoremap <silent><C-k> <C-R>=OmniPopup('k')<CR>
"
"
" " Python folding
" " mkdir -p ~/.vim/ftplugin
" " wget -O ~/.vim/ftplugin/python_editing.vim
" http://www.vim.org/scripts/download_script.php?src_id=5492
set nofoldenable

