local str = require '_.str'
local Fs = {}

local cache = {}

---Read a file from disk
---@param file string
---@param opts? { no_cache?: boolean }
---@return Str | nil
function Fs.readfile(file, opts)
	opts = opts or { no_cache = false }
	local cached = cache[file]
	if not opts.no_cache and cached then return cached end

	local f = io.open(file, 'rb')
	if not f then return nil end
	local content = f:read '*all'
	if type(content) ~= 'string' then return nil end
	f:close()
	local out = str(content)
	cache[file] = out
	return out
end

return Fs
