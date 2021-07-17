filetype plugin on

let $BASH_ENV = "~/.bash_aliases"

set nocompatible
syntax off
set tabstop=4
set shiftwidth=4
set expandtab
set number
filetype on
set autoindent
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
let g:lightline.colorscheme = 'sonokai'

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
Plug 'phaazon/hop.nvim'
" Plug 'MaxMEllon/vim-jsx-pretty'
" Plug 'marciomazza/vim-brogrammer-theme'
" Plug 'wilsaj/chuck.vim'
Plug 'neovim/nvim-lspconfig'
" Plug 'nvim-lua/completion-nvim'
" Plug 'joshdick/onedark.vim'
" Plug 'sainnhe/gruvbox-material'
" Plug 'rakr/vim-two-firewatch'
" Plug 'aurieh/discord.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'rust-lang/rust.vim'
Plug 'liuchengxu/vista.vim'
Plug 'sainnhe/sonokai'
Plug 'rhysd/vim-clang-format'
Plug 'delphinus/vim-firestore'
" Plug 'sheerun/vim-polyglot'
Plug 'onsails/lspkind-nvim'
Plug 'hrsh7th/nvim-compe'
" Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
" Plug 'junegunn/fzf.vim'
Plug 'hrsh7th/vim-vsnip'
Plug 'jistr/vim-nerdtree-tabs'
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
Plug 'kyazdani42/nvim-web-devicons'
" Plug 'romgrk/barbar.nvim'

call plug#end()

" let g:lightline = { 'colorscheme': 'sitruuna' }

colorscheme sonokai " sitruuna gruvbox-material onedark material sitruuna brogrammer material monokai sitruuna  slate

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

" Lua treesitter config
lua << EOF
require 'nvim-treesitter.configs'.setup {
    ensure_installed = "all",
    highlight = {
        enable = true,
    },
    indent = {
        enable = true,
    }
}
EOF

" Lua formatter config
lua << EOF
require 'format'
EOF

lua << EOF
-- require('gitsigns').setup()
require'lsp_signature'.on_attach()
require('numb').setup()
EOF

autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()
set completeopt=menuone,noinsert,noselect
" make Tab autocomplete (these binds are now set in lua/lsp.lua)
" inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Tab keybinds
nnoremap <leader>nt :<C-u>NERDTreeTabsToggle<CR>
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

" barbar.nvim binds
" Move to previous/next
nnoremap <silent>    <A-,> :BufferPrevious<CR>
nnoremap <silent>    <A-.> :BufferNext<CR>
" Re-order to previous/next
nnoremap <silent>    <A-<> :BufferMovePrevious<CR>
nnoremap <silent>    <A->> :BufferMoveNext<CR>
" Goto buffer in position...
nnoremap <silent>    <A-1> :BufferGoto 1<CR>
nnoremap <silent>    <A-2> :BufferGoto 2<CR>
nnoremap <silent>    <A-3> :BufferGoto 3<CR>
nnoremap <silent>    <A-4> :BufferGoto 4<CR>
nnoremap <silent>    <A-5> :BufferGoto 5<CR>
nnoremap <silent>    <A-6> :BufferGoto 6<CR>
nnoremap <silent>    <A-7> :BufferGoto 7<CR>
nnoremap <silent>    <A-8> :BufferGoto 8<CR>
nnoremap <silent>    <A-9> :BufferLast<CR>
" Close buffer
nnoremap <silent>    <A-c> :BufferClose<CR>
" Wipeout buffer
"                          :BufferWipeout<CR>
" Close commands
"                          :BufferCloseAllButCurrent<CR>
"                          :BufferCloseBuffersLeft<CR>
"                          :BufferCloseBuffersRight<CR>
" Magic buffer-picking mode
nnoremap <silent> <C-s>    :BufferPick<CR>
" Sort automatically by...
nnoremap <silent> <Space>bd :BufferOrderByDirectory<CR>
nnoremap <silent> <Space>bl :BufferOrderByLanguage<CR>

" barbar.nvim options
let bufferline = get(g:, 'bufferline', {})
let bufferline.animation = v:false
let bufferline.auto_hide = v:true
