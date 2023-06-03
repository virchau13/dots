require 'util'

require('lualine').setup {
    options = {
        theme = 'one_monokai'
    }
}

if not os.getenv('NVIM_DISABLE_TS') then
    require 'nvim-treesitter.configs'.setup {
        -- ensure_installed = "all",
        -- auto_install = false,
        parser_install_dir = '~/.local/share/nvim/site',
        highlight = {
            enable = true,
            disable = function(lang, bufnr)
                -- disable in long buffers
                return vim.api.nvim_buf_line_count(bufnr) > 2000
            end,
            additional_vim_regex_highlighting = true -- to get vim-polyglot indentation to work
        },
        indent = {
            enable = false,
        },
        ignore_install = { 'go' },
    }
end

require('numb').setup()
-- TODO check performance: require('gitsigns').setup()

-- nvim-tree on_attach {{{
local function on_attach(bufnr)
  local api = require('nvim-tree.api')

  local function opts(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end


  -- Default mappings. Feel free to modify or remove as you wish.
  --
  -- BEGIN_DEFAULT_ON_ATTACH
  vim.keymap.set('n', '<C-]>', api.tree.change_root_to_node,          opts('CD'))
  vim.keymap.set('n', '<C-e>', api.node.open.replace_tree_buffer,     opts('Open: In Place'))
  vim.keymap.set('n', '<C-k>', api.node.show_info_popup,              opts('Info'))
  vim.keymap.set('n', '<C-r>', api.fs.rename_sub,                     opts('Rename: Omit Filename'))
  vim.keymap.set('n', '<C-t>', api.node.open.tab,                     opts('Open: New Tab'))
  vim.keymap.set('n', '<C-v>', api.node.open.vertical,                opts('Open: Vertical Split'))
  vim.keymap.set('n', '<C-x>', api.node.open.horizontal,              opts('Open: Horizontal Split'))
  vim.keymap.set('n', '<BS>',  api.node.navigate.parent_close,        opts('Close Directory'))
  vim.keymap.set('n', '<CR>',  api.node.open.edit,                    opts('Open'))
  vim.keymap.set('n', '<Tab>', api.node.open.preview,                 opts('Open Preview'))
  vim.keymap.set('n', '>',     api.node.navigate.sibling.next,        opts('Next Sibling'))
  vim.keymap.set('n', '<',     api.node.navigate.sibling.prev,        opts('Previous Sibling'))
  vim.keymap.set('n', '.',     api.node.run.cmd,                      opts('Run Command'))
  vim.keymap.set('n', '-',     api.tree.change_root_to_parent,        opts('Up'))
  vim.keymap.set('n', 'a',     api.fs.create,                         opts('Create'))
  vim.keymap.set('n', 'bmv',   api.marks.bulk.move,                   opts('Move Bookmarked'))
  vim.keymap.set('n', 'B',     api.tree.toggle_no_buffer_filter,      opts('Toggle No Buffer'))
  vim.keymap.set('n', 'c',     api.fs.copy.node,                      opts('Copy'))
  vim.keymap.set('n', 'C',     api.tree.toggle_git_clean_filter,      opts('Toggle Git Clean'))
  vim.keymap.set('n', '[c',    api.node.navigate.git.prev,            opts('Prev Git'))
  vim.keymap.set('n', ']c',    api.node.navigate.git.next,            opts('Next Git'))
  vim.keymap.set('n', 'd',     api.fs.remove,                         opts('Delete'))
  vim.keymap.set('n', 'D',     api.fs.trash,                          opts('Trash'))
  vim.keymap.set('n', 'E',     api.tree.expand_all,                   opts('Expand All'))
  vim.keymap.set('n', 'e',     api.fs.rename_basename,                opts('Rename: Basename'))
  vim.keymap.set('n', ']e',    api.node.navigate.diagnostics.next,    opts('Next Diagnostic'))
  vim.keymap.set('n', '[e',    api.node.navigate.diagnostics.prev,    opts('Prev Diagnostic'))
  vim.keymap.set('n', 'F',     api.live_filter.clear,                 opts('Clean Filter'))
  vim.keymap.set('n', 'f',     api.live_filter.start,                 opts('Filter'))
  vim.keymap.set('n', 'g?',    api.tree.toggle_help,                  opts('Help'))
  vim.keymap.set('n', 'gy',    api.fs.copy.absolute_path,             opts('Copy Absolute Path'))
  vim.keymap.set('n', 'H',     api.tree.toggle_hidden_filter,         opts('Toggle Dotfiles'))
  vim.keymap.set('n', 'I',     api.tree.toggle_gitignore_filter,      opts('Toggle Git Ignore'))
  vim.keymap.set('n', 'J',     api.node.navigate.sibling.last,        opts('Last Sibling'))
  vim.keymap.set('n', 'K',     api.node.navigate.sibling.first,       opts('First Sibling'))
  vim.keymap.set('n', 'm',     api.marks.toggle,                      opts('Toggle Bookmark'))
  vim.keymap.set('n', 'o',     api.node.open.edit,                    opts('Open'))
  vim.keymap.set('n', 'O',     api.node.open.no_window_picker,        opts('Open: No Window Picker'))
  vim.keymap.set('n', 'p',     api.fs.paste,                          opts('Paste'))
  vim.keymap.set('n', 'P',     api.node.navigate.parent,              opts('Parent Directory'))
  vim.keymap.set('n', 'q',     api.tree.close,                        opts('Close'))
  vim.keymap.set('n', 'r',     api.fs.rename,                         opts('Rename'))
  vim.keymap.set('n', 'R',     api.tree.reload,                       opts('Refresh'))
  vim.keymap.set('n', 's',     api.node.run.system,                   opts('Run System'))
  vim.keymap.set('n', 'S',     api.tree.search_node,                  opts('Search'))
  vim.keymap.set('n', 'U',     api.tree.toggle_custom_filter,         opts('Toggle Hidden'))
  vim.keymap.set('n', 'W',     api.tree.collapse_all,                 opts('Collapse'))
  vim.keymap.set('n', 'x',     api.fs.cut,                            opts('Cut'))
  vim.keymap.set('n', 'y',     api.fs.copy.filename,                  opts('Copy Name'))
  vim.keymap.set('n', 'Y',     api.fs.copy.relative_path,             opts('Copy Relative Path'))
  vim.keymap.set('n', '<2-LeftMouse>',  api.node.open.edit,           opts('Open'))
  vim.keymap.set('n', '<2-RightMouse>', api.tree.change_root_to_node, opts('CD'))
  -- END_DEFAULT_ON_ATTACH


  -- Mappings migrated from view.mappings.list
  --
  -- You will need to insert "your code goes here" for any mappings with a custom action_cb
  vim.keymap.set('n', 't', api.node.open.tab, opts('Open: New Tab'))
end
-- }}}


local tree_cb = require'nvim-tree.config'.nvim_tree_callback
require'nvim-tree'.setup {
    -- opens the tree when changing/opening a new tab if the tree wasn't previously opened
    open_on_tab         = true,
    -- show lsp diagnostics in the signcolumn
    diagnostics = { enable = false },
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
    },
    on_attach = on_attach,
}
local nt_api = require('nvim-tree.api')
vim.api.nvim_create_autocmd("BufEnter", {
    nested = true,
    callback = function()
        if vim.api.nvim_buf_get_name(0):match("NvimTree_") ~= nil then
            if #vim.api.nvim_list_wins() == 1 then
                -- automatically close nvim-tree when it's the last window
                -- (https://github.com/kyazdani42/nvim-tree.lua/discussions/1115)
                vim.cmd "quit"
            end
        else
            -- We entered a new buffer, find it in the tree
            -- (only if nt is actually open, of course)
            if vim.g.nvim_tree_is_open then
                nt_api.tree.find_file { open = true, update_root = false }
            end
        end
    end
})
-- vim.api.nvim_create_autocmd("BufLeave", {
--     nested = true,
--     callback = function()
--         if vim.api.nvim_buf_get_name(0):match("NvimTree_") ~= nil then
--         end
--     end
-- })

local NtEvent = nt_api.events.Event

nt_api.events.subscribe(NtEvent.TreeOpen, function()
    vim.g.nvim_tree_is_open = true
    vim.keymap.set('n', '[f', function()
        nt_api.node.navigate.git.prev()
        nt_api.node.open.edit()
    end)
    vim.keymap.set('n', ']f', function()
        nt_api.node.navigate.git.next()
        nt_api.node.open.edit()
    end)
end)

nt_api.events.subscribe(NtEvent.TreeClose, function()
    vim.g.nvim_tree_is_open = false
end)

require('fidget').setup {}

require('virt-column').setup()

-- require('lsp_lines').setup()
-- vim.diagnostic.config({
--     -- turn it off by default
--     virtual_lines = false,
-- })

require('lsp-inlayhints').setup()
