local M = {}

function M.home(path) return os.getenv 'HOME' .. '/' .. path end
function M.bin(b) return '/opt/homebrew/bin/' .. b end

function M.serializeTable(val, name, skipnewlines, depth)
	skipnewlines = skipnewlines or false
	depth = depth or 0

	local tmp = string.rep(' ', depth)

	if name then tmp = tmp .. name .. ' = ' end

	if type(val) == 'table' then
		tmp = tmp .. '{' .. (not skipnewlines and '\n' or '')

		for k, v in pairs(val) do
			tmp = tmp
				.. M.serializeTable(v, k, skipnewlines, depth + 1)
				.. ','
				.. (not skipnewlines and '\n' or '')
		end

		tmp = tmp .. string.rep(' ', depth) .. '}'
	elseif type(val) == 'number' then
		tmp = tmp .. tostring(val)
	elseif type(val) == 'string' then
		tmp = tmp .. string.format('%q', val)
	elseif type(val) == 'boolean' then
		tmp = tmp .. (val and 'true' or 'false')
	else
		tmp = tmp .. '"[inserializeable datatype:' .. type(val) .. ']"'
	end

	return tmp
end

return M
