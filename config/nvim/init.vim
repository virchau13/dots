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

" Autodownload vim-plug
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

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
" Plug 'ray-x/lsp_signature.nvim'
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
    indent = {
        enable = {"jsx"},
    }
}

require 'format'

-- require('gitsigns').setup()
require('numb').setup()

-- following options are the default
require'nvim-tree'.setup {
  -- disables netrw completely
  disable_netrw       = true,
  -- hijack netrw window on startup
  hijack_netrw        = true,
  -- open the tree when running this setup function
  open_on_setup       = false,
  -- will not open on setup if the filetype is in this list
  ignore_ft_on_setup  = {},
  -- closes neovim automatically when the tree is the last **WINDOW** in the view
  auto_close          = false,
  -- opens the tree when changing/opening a new tab if the tree wasn't previously opened
  open_on_tab         = false,
  -- hijack the cursor in the tree to put it at the start of the filename
  hijack_cursor       = false,
  -- updates the root directory of the tree on `DirChanged` (when your run `:cd` usually) 
  update_cwd          = false,
  -- show lsp diagnostics in the signcolumn
  lsp_diagnostics     = false,
  -- update the focused file on `BufEnter`, un-collapses the folders recursively until it finds the file
  update_focused_file = {
    -- enables the feature
    enable      = false,
    -- update the root directory of the tree to the one of the folder containing the file if the file is not under the current root directory
    -- only relevant when `update_focused_file.enable` is true
    update_cwd  = false,
    -- list of buffer names / filetypes that will not update the cwd if the file isn't found under the current root directory
    -- only relevant when `update_focused_file.update_cwd` is true and `update_focused_file.enable` is true
    ignore_list = {}
  },
  -- configuration options for the system open command (`s` in the tree by default)
  system_open = {
    -- the command to run this, leaving nil should work in most cases
    cmd  = nil,
    -- the command arguments as a list
    args = {}
  },

  view = {
    -- width of the window, can be either a number (columns) or a string in `%`
    width = 30,
    -- side of the tree, can be one of 'left' | 'right' | 'top' | 'bottom'
    side = 'left',
    -- if true the tree will resize itself after opening a file
    auto_resize = false,
    mappings = {
      -- custom only false will merge the list with the default mappings
      -- if true, it will only use your list to set the mappings
      custom_only = false,
      -- list of mappings to set on the tree manually
      list = {}
    }
  }
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
