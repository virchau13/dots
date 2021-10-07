function setopt(name, val)
    if val == nil then
        val = true
    end
    vim.o[name] = val
end

function getopt(name)
    return vim.o[name]
end

function catopt(name, val)
    setopt(name, getopt(name) .. val)
end

function setgvar(name, val)
    vim.api.nvim_set_var(name, val)
end

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
