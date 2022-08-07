local builtin = require 'telescope.builtin'
local themes = require 'telescope.themes'

local M = {}

function M.lsp_code_actions() builtin.lsp_code_actions(themes.get_cursor()) end

return M
