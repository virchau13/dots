require 'util'

setopt("encoding", "utf-8")
setopt("termguicolors")

-- Indentation
setopt("tabstop", 4)
setopt("shiftwidth", 4)
setopt("expandtab")
setopt("autoindent")
setopt("smartindent")

-- Line numbers
setopt("number")
-- Highlight line cursor is over
setopt("cursorline")
-- Left-side column
setopt("signcolumn", "yes")
-- Enable mouse
setopt("mouse", "a")
-- Preview `:s` replacement
setopt("inccommand", "split")
-- Enable persisting undos over multiple editing sessions
setopt("undofile")
setopt("backupdir", vim.env.HOME .. "/.local/share/nvim/backup")

setopt("foldmethod", "marker")
setopt("linebreak")
setopt("hlsearch", false)
setopt("completeopt", "menuone,noinsert,noselect")

setopt("updatetime", 300)
catopt("shortmess", "c")

-- Make the screen start scrolling when the cursor is
-- 5 lines from the top or bottom
setopt('scrolloff', 5)
