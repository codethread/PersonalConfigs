local M = {}

function M.home(path) return os.getenv 'HOME' .. '/' .. path end
function M.bin(b) return '/opt/homebrew/bin/' .. b end

return M
