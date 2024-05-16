local M = {}

M.cwd = os.getenv 'CT_NOTES'
M.pattern = M.cwd .. '/*'

return M
