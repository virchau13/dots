filetype plugin indent on

set nocompatible
syntax on
set tabstop=4
set shiftwidth=4
set expandtab
set number
filetype on
" set autoindent
set encoding=utf-8
set termguicolors
set background=dark
set inccommand=split
set cursorline
" autocmd BufRead,BufNewFile *.pcse set ft=pcse
" autocmd BufRead,BufNewFile *.ebnf set syntax=ebnf

if (has("termguicolors"))
set termguicolors
endif

call plug#begin('~/.config/nvim/plugged')

" Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Plug 'dense-analysis/ale'
" Plug 'autozimu/LanguageClient-neovim', {
"             \ 'branch': 'next',
"             \ 'do': 'bash install.sh',
"             \ }
" Plug 'junegunn/fzf'
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'scrooloose/nerdtree'
" Plug 'pangloss/vim-javascript'
" Plug 'vim-airline/vim-airline'  (lmao vim airline)
Plug 'itchyny/lightline.vim'
Plug 'eemed/sitruuna.vim'
" Plug 'MaxMEllon/vim-jsx-pretty'
" Plug 'marciomazza/vim-brogrammer-theme'
" Plug 'wilsaj/chuck.vim'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/diagnostic-nvim'
" Plug 'nvim-lua/completion-nvim'
" Plug 'joshdick/onedark.vim'
" Plug 'sainnhe/gruvbox-material'
" Plug 'rakr/vim-two-firewatch'
" Plug 'aurieh/discord.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'rust-lang/rust.vim'
Plug 'liuchengxu/vista.vim'
Plug 'sainnhe/sonokai'
Plug 'rhysd/vim-clang-format'
" Plug 'delphinus/vim-firestore'
Plug 'sheerun/vim-polyglot'
Plug 'onsails/lspkind-nvim'
Plug 'hrsh7th/nvim-compe'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'hrsh7th/vim-vsnip'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'ryanoasis/vim-devicons'

call plug#end()

let g:sonokai_style = 'shusia'
let g:sonokai_enable_italic = 1

let g:lightline = {
            \ 'colorscheme': 'sonokai',
            \ }
" let g:lightline = { 'colorscheme': 'sitruuna' }

colorscheme sonokai " sitruuna gruvbox-material onedark material sitruuna brogrammer material monokai sitruuna  slate

" fix cursor styling for xterm / urxvt
if &term =~# 'xterm' || &term =~# 'rxvt'
    let &t_EI = "\<Esc>[2 q"
    let &t_SI = "\<Esc>[6 q"
    let &t_SR = "\<Esc>[4 q"
endif

let $BASH_ENV = "~/.bash_aliases"

set foldmethod=marker
set signcolumn=yes

" make <C-w> in terminal work
tnoremap <C-w> <C-\><C-n><C-w>

" compe
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })

set nohlsearch

" autocmd BufWritePost *.tex !latexmk -pdf main.tex
set linebreak

" Fix Parcel / Nodemon file watching
" set nobackup
" set nowritebackup
set backupdir=~/.local/share/nvim/backup

set updatetime=300
set shortmess+=c

" lua lsp config
lua << EOF
require 'lsp'
EOF

autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()
set completeopt=menuone,noinsert,noselect
" make Tab autocomplete (these binds are now set in lua/lsp.lua)
" inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Tab keybinds
nnoremap <leader>nt :<C-u>NERDTreeTabsToggle<CR>
nnoremap <leader>tn :<C-u>tabnew 

set spelllang=en_sg
inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u
autocmd BufEnter *.tex setlocal spell
nnoremap <Leader>cf :<C-u>ClangFormat<CR>
