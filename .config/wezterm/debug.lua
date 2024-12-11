local function splitString(inputstr, sep)
	if sep == nil then
		sep = '%s' -- default to split by whitespace if no separator is provided
	end
	local t = {}
	for str in string.gmatch(inputstr, '([^' .. sep .. ']+)') do
		table.insert(t, str)
	end
	return t
end

---Not present in wezterm
local debug = {
	traceback = function(...) print(...) end,
	getinfo = function() end,
	_print_require_path = function() print(splitString(package.path, ';')) end,
}

return debug
