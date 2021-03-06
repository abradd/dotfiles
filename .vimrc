".vimrc file for setting on startup

"Setting up NeoBundle
if has('vim_starting')
set nocompatible    
set runtimepath+=~/.vim/bundle/neobundle.vim/
endif  
call neobundle#begin(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'
"Call all packages
"NeoBundle 'LaTeX-Box-Team/LaTeX-Box', {'rev' : '61528a'}
NeoBundle 'scrooloose/nerdtree', {'rev' : 'b0bb78'}
"NeoBundle 'Lokaltog/vim-powerline', {'rev' : '09c0ce'}
NeoBundle 'benmills/vimux'
NeoBundle 'majutsushi/tagbar'
"NeoBundle 'scrooloose/syntastic'
NeoBundle 'Lokaltog/vim-easymotion'
"NeoBundle 'junegunn/goyo.vim'
"NeoBundle 'junegunn/limelight.vim'
NeoBundle 'godlygeek/tabular'
NeoBundle 'christoomey/vim-tmux-navigator'
"NeoBundle 'SirVer/ultisnips'
"NeoBundle 'honza/vim-snippets'
NeoBundle 'tpope/vim-surround'
"NeoBundle 'tpope/vim-rsi'
"NeoBundle 'vimwiki/vimwiki'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'scrooloose/nerdcommenter'
"NeoBundle 'bling/vim-airline'
"NeoBundle 'edkolev/tmuxline.vim'
call neobundle#end()

"automatically reload the .vimrc file
autocmd! bufwritepost .vimrc source %

"only show 5 words when suggesting spelling
set spellsuggest=5

"set vim to noncompatible mode which provides the most features
set nocompatible

"omnicomplete
set omnifunc=syntaxcomplete#Complete

"set text wrap automatically and linebreak so that words are not broken between
"lines
set wrap
set linebreak
set textwidth=80
set wrapmargin=0

"show line numbers and length
set number  "show line numbers
set tw=79	"width of document
"set nowrap 	"don't automatically wrap on load
set fo-=t	"don't automatically wrap text when writing
"set colorcolumn=80
highlight ColorColumn ctermbg=233

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

"run cavestory or otherwise
nnoremap <leader>g :!make && make run<CR>

"invisible characters
set listchars=tab:▸\ ,eol:¬

"set the number of commands the vim holds in its register
set history=700
set undolevels=700

"better copy and paste
set pastetoggle=<F2>
set clipboard^=unnamed

"display current cursor position in the bottom right of the screen and show incomplete commands
set ruler
set showcmd

"remap , to <leader> key
let mapleader = " "

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

"vimwiki settings
let g:vimwiki_list = [{'path': '~/Dropbox/vimwiki'}]

"map sort function to a key
vnoremap <Leader>s :sort<CR>

"map for running vimux/matlab command
nmap <Leader>r :call VimuxRunCommand("run " . bufname('%'))<CR>

"easier moving of code blocks
vnoremap < <gv
vnoremap > >gv


"easier formatting of paragraphs
vmap Q gq
nmap Q gqap

"use spaces instead of tabs (especially useful for python)
set tabstop =4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab

"make search case insensitive
"set hlsearch
set incsearch
set ignorecase
set smartcase

"NerdTree
"https://github.com/scrooloose/nerdtree.git
map <C-n> :NERDTreeToggle<CR>

"Set .tex files to latex by default
let g:tex_flavor= "latex"

"Tagbar
"nmap <C-t> :TagbarToggle<CR>

""setup pathogen to manage plugins
"execute pathogen#infect()
"call pathogen#incubate()
"call pathogen#helptags()

"filetype detection
filetype plugin indent on
syntax on

"Color scheme
set t_Co=256
set background=dark
color solarized

"airline setup
let g:airline_powerline_fonts = 1

"Matlab settings
source $VIMRUNTIME/macros/matchit.vim

"Supertab settings
let g:SuperTabDefaultCompletionType = "context"

"Setting for Latex Box
let g:LatexBox_viewer="open"
"let g:LatexBox_latexmk_options="-pdflatex='pdflatex -synctex=1'"
let g:LatexBox_latexmk_async=0
let g:LatexBox_latexmk_preview_continuosly=0
"LatexBox_quickfix enables whether the quickfix dialogs opens
let LatexBox_quickfix=2
"============================================================================
"Python IDE Setup
" ============================================================================
"
"
" " Settings for vim-powerline
" " cd ~/.vim/bundle
" " git clone git://github.com/Lokaltog/vim-powerline.git
set laststatus=2
"
"
" " Settings for ctrlp
" " cd ~/.vim/bundle
" " git clone https://github.com/kien/ctrlp.vim.git
let g:ctrlp_max_height = 30
set wildignore+=*.pyc
set wildignore+=*_build/*
set wildignore+=*/coverage/*
"
"
" " Settings for python-mode
" " Note: I'm no longer using this. Leave this commented out
" " and uncomment the part about jedi-vim instead
" " cd ~/.vim/bundle
" " git clone https://github.com/klen/python-mode
" "" map <Leader>g :call RopeGotoDefinition()<CR>
" "" let ropevim_enable_shortcuts = 1
" "" let g:pymode_rope_goto_def_newwin = "vnew"
" "" let g:pymode_rope_extended_complete = 1
" "" let g:pymode_breakpoint = 0
" "" let g:pymode_syntax = 1
" "" let g:pymode_syntax_builtin_objs = 0
" "" let g:pymode_syntax_builtin_funcs = 0
" "" map <Leader>b Oimport ipdb; ipdb.set_trace() # BREAKPOINT<C-c>
"
"" Setting up AuotmaticLatexPlugin
"
"let b:atp_OpenViewer=1
"let b:atp_viewer = "open %S"

" " Settings for jedi-vim
" " cd ~/.vim/bundle
" " git clone git://github.com/davidhalter/jedi-vim.git
" related_names is deprecated
"let g:jedi#related_names_command = "<leader>z"
let g:jedi#usages_command = "<leader>z"
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0
map <Leader>b Oimport ipdb; ipdb.set_trace() # BREAKPOINT<C-c>
"
" " Better navigating through omnicomplete option list
" " See
" http://stackoverflow.com/questions/2170023/how-to-map-keys-for-popup-menu-in-vim
" "" set completeopt=longest,menuone
" "" function! OmniPopup(action)
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
"set nofoldenable

"================================================
"Matlab setup
"================================================
"https://github.com/sgeb/vim-matlab.git

source $VIMRUNTIME/macros/matchit.vim

"Compile checking
autocmd BufEnter *.m    compiler mlint

"============
"Vimux stuff
"============
let g:VimuxUseNearest = 1
