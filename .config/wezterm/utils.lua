local M = {}

function M.home(path) return os.getenv 'HOME' .. '/' .. path end
function M.bin(b) return '/opt/homebrew/bin/' .. b end

---@type 'home' | 'work' | 'unknown'
local user

---@param current 'home' | 'work'
---@return boolean
function M.machine(current)
	if not user then user = os.getenv 'CT_USER' or 'unknown' end
	return current == user
end

---@param fullPath string
---@return string
function M.getFilename(fullPath)
	-- Find the last occurrence of a slash
	local filename = fullPath:match '([^/\\]+)$'
	return filename
end

return M
