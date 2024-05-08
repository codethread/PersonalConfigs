local M = {}

M.cwd = vim.fn.expand '~' .. '/Library/Mobile Documents/com~apple~CloudDocs/Documents/Notes'
M.pattern = M.cwd .. '/*'

return M
