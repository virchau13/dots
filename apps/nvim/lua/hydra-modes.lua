local Hydra = require('hydra')

-- TODO put back

-- -- venn.nvim: draw ASCII diagrams
-- local hint = [[
--  Arrow^^^^^^   Select region with <C-v> 
--  ^ ^ _K_ ^ ^   _f_: surround it with box
--  _H_ ^ ^ _L_
--  ^ ^ _J_ ^ ^                      _<Esc>_
-- ]]
-- 
-- Hydra({
--    name = 'Draw Diagram',
--    hint = hint,
--    config = {
--       color = 'pink',
--       invoke_on_body = true,
--       hint = {
--          float_ops = {
--             border = 'rounded'
--          },
--       },
--       on_enter = function()
--          vim.o.virtualedit = 'all'
--       end,
--    },
--    mode = 'n',
--    body = '<leader>d',
--    heads = {
--       { 'H', '<C-v>h:VBox<CR>' },
--       { 'J', '<C-v>j:VBox<CR>' },
--       { 'K', '<C-v>k:VBox<CR>' },
--       { 'L', '<C-v>l:VBox<CR>' },
--       { 'f', ':VBox<CR>', { mode = 'v' }},
--       { '<Esc>', nil, { exit = true } },
--    }
-- })
