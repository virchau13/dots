filetype plugin on

let $BASH_ENV = "~/.bash_aliases"

set nocompatible
syntax off
set tabstop=4
set shiftwidth=4
set expandtab
set number
filetype on
set autoindent smartindent
set encoding=utf-8
set termguicolors
set background=dark
set inccommand=split
set cursorline
set mouse=a
set undofile
" autocmd BufRead,BufNewFile *.pcse set ft=pcse
" autocmd BufRead,BufNewFile *.ebnf set syntax=ebnf

if (has("termguicolors"))
set termguicolors
endif

let g:polyglot_disabled = ['autoindent']

let g:sonokai_style = 'shusia'
let g:sonokai_enable_italic = 1

let g:lightline = {}
let g:lightline.separator = { 'left': "\ue0bc", 'right': "\ue0ba" }
let g:lightline.subseparator = { 'left': "\ue0bb", 'right': "\ue0bb" }
if 0
    let g:lightline.tabline_separator = { 'left': "\ue0b8", 'right': "\ue0be" }
    let g:lightline.tabline_subseparator = { 'left': "\ue0b9", 'right': "\ue0b9" }
endif
let g:lightline.colorscheme = 'tokyonight'

let g:nvim_tree_auto_close = 1
let g:nvim_tree_tab_open = 1

call plug#begin('~/.config/nvim/plugged')

Plug 'kyazdani42/nvim-web-devicons'
Plug 'folke/tokyonight.nvim', { 'branch': 'main' }
Plug 'kyazdani42/nvim-tree.lua'
" Plug 'pangloss/vim-javascript'
Plug 'itchyny/lightline.vim'
" Plug 'jistr/vim-nerdtree-tabs'
Plug 'eemed/sitruuna.vim'
Plug 'phaazon/hop.nvim'
" Plug 'MaxMEllon/vim-jsx-pretty'
" Plug 'marciomazza/vim-brogrammer-theme'
Plug 'neovim/nvim-lspconfig'
" Plug 'nvim-lua/completion-nvim'
" Plug 'joshdick/onedark.vim'
" Plug 'sainnhe/gruvbox-material'
" Plug 'rakr/vim-two-firewatch'
" Plug 'aurieh/discord.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'rust-lang/rust.vim'
Plug 'liuchengxu/vista.vim'
" Plug 'sainnhe/sonokai'
Plug 'rhysd/vim-clang-format'
Plug 'delphinus/vim-firestore'
" Plug 'sheerun/vim-polyglot'
Plug 'onsails/lspkind-nvim'
Plug 'hrsh7th/nvim-compe'
" Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
" Plug 'junegunn/fzf.vim'
Plug 'hrsh7th/vim-vsnip'
Plug 'ryanoasis/vim-devicons'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
Plug 'mhartington/formatter.nvim'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
" Plug 'lewis6991/gitsigns.nvim'
Plug 'ray-x/lsp_signature.nvim'
" Plug 'romgrk/nvim-treesitter-context'
Plug 'nacro90/numb.nvim'
" Plug 'romgrk/barbar.nvim'

call plug#end()

" let g:lightline = { 'colorscheme': 'sitruuna' }


" fix cursor styling for xterm / urxvt
" if &term =~# 'xterm' || &term =~# 'rxvt'
"     let &t_EI = "\<Esc>[2 q"
"     let &t_SI = "\<Esc>[6 q"
"     let &t_SR = "\<Esc>[4 q"
" endif


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

lua << EOF
require 'nvim-treesitter.configs'.setup {
    ensure_installed = "all",
    highlight = {
        enable = true,
    },
    -- indent = {
    --     enable = true,
    -- }
}

require 'format'

-- require('gitsigns').setup()
require('numb').setup()

local tree_cb = require'nvim-tree.config'.nvim_tree_callback

vim.g.nvim_tree_bindings = {
    { key = "t", cb = tree_cb("tabnew") }
}
EOF

" autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()
set completeopt=menuone,noinsert,noselect
" make Tab autocomplete (these binds are now set in lua/lsp.lua)
" inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Tab keybinds
nnoremap <leader>nt :<C-u>NvimTreeToggle<CR>
nnoremap <leader>tn :<C-u>tabnew 

" set spelllang=en_sg
" inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u
" autocmd BufEnter *.tex setlocal spell
" hop.nvim
nnoremap <Leader><Leader>w :<C-u>HopWord<CR>
nnoremap <Leader><Leader>l :<C-u>HopLine<CR>
nnoremap <Leader><Leader>/ :<C-u>HopPattern<CR>
" telescope.nvim
nnoremap <Leader>tl :<C-u>Telescope<CR>
nnoremap <Leader>op :<C-u>Telescope find_files<CR>

" show trailing spaces
highlight ExtraWhitespace guifg=#b0b0b0
match ExtraWhitespace /\s\+$/
" set listchars=trail:·
" set list

au BufRead,BufNewFile *.nix set filetype=nix
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

let g:tokyonight_style = 'night'
colorscheme tokyonight " sitruuna gruvbox-material onedark material sitruuna brogrammer material monokai sitruuna  slate
