local M = {}

M.has_notes = os.getenv 'CT_NOTES' ~= nil
M.cwd = os.getenv 'CT_NOTES' or 'nope'
M.pattern = M.cwd .. '/*'

return M
