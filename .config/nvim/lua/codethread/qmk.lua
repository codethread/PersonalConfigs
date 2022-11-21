local ts = vim.treesitter

-- look through the keymap file and find all layouts, capturing
-- the keymap name and a list of all keys for formatting
local keymap = ts.parse_query(
	'cpp',
	[[
(initializer_pair
    designator: (subscript_designator (identifier) @keymap_name) 
    value: (call_expression
             function: (identifier) @id (#eq? @id "LAYOUT_preonic_grid")
             arguments: (argument_list) @key_list))
]]
)

-- take a layout, which is a list of keycodes
-- breaks into a table where each item represents a row (starting top left)
-- each row is a long string representation of the keys with even spacing adjusted columnwise
local function format_layout(layout)
	local width = 12 -- hard coded, but could be dynamic off config

	assert(
		#layout % width == 0,
		"length of keys '" .. #layout .. "' is not divisible by keyboard width '" .. width .. "'"
	)

	-- table of key rows, starting at the top left
	local rows = { {} }
	local output = { '' }
	local current_row = 1
	local max_rows = #layout / width

	assert(max_rows == 5, 'rows be wrong' .. max_rows)

	-- split layout into rows
	for i, key in ipairs(layout) do
		assert(current_row <= max_rows, 'Too many rows ' .. i)
		table.insert(rows[current_row], key)

		-- end of row, so prepare the next
		if i % width == 0 then
			current_row = current_row + 1
			rows[current_row] = {}
		end
	end

	-- move through all rows at the same time by colum, padding width by longest key
	for col = 1, width do
		local longest_key = 1
		for row = 1, current_row - 1 do -- HACK from above increment in loop
			local key = rows[row][col]
			if #key > longest_key then longest_key = #key end
		end

		for row = 1, current_row - 1, 1 do -- HACK from abov
			local key = rows[row][col]
			if col == 1 then
				-- first column so no comma
				output[row] = '  ' .. key .. string.rep(' ', longest_key - #key)
			elseif col == width and row == max_rows then
				-- no trailing comma for last key
				output[row] = output[row] .. ' , ' .. key .. string.rep(' ', longest_key - #key)
			elseif col == width then
				-- last key so trailing comma
				output[row] = output[row] .. ' , ' .. key .. ','
			elseif col == width / 2 then
				-- show middle of keyboard
				output[row] = output[row]
					.. ' , '
					.. key
					.. string.rep(' ', longest_key - #key)
					.. ' /* | */'
			else
				output[row] = output[row] .. ' , ' .. key .. string.rep(' ', longest_key - #key)
			end
		end
	end

	return output
end

local function format_qmk_keymaps()
	local bufnr = vim.api.nvim_get_current_buf()
	local parser = ts.get_parser(bufnr, 'cpp')
	local root = parser:parse()[1]:root()

	local layouts = {}
	local current_keymap = ''

	for keymap_id, keymap_node in keymap:iter_captures(root, bufnr, 0, -1) do
		local capture_name = keymap.captures[keymap_id]

		if capture_name == 'keymap_name' then
			-- create a table for the current layout
			local keymap_name = ts.get_node_text(keymap_node, bufnr)
			current_keymap = keymap_name
			layouts[current_keymap] = {}
		end
		if capture_name == 'key_list' then
			-- get all keys for layout
			for key_node in keymap_node:iter_children() do
				if key_node:named() and key_node:type() ~= 'comment' then
					local key_txt = ts.get_node_text(key_node, bufnr)
					table.insert(layouts[current_keymap], key_txt)
				end
			end

			-- { start row, start col, end row, end col }
			local range = { keymap_node:range() }
			local keylist_range = { start = range[1] + 1, final = range[3] }

			local formatted_text = format_layout(layouts[current_keymap])

			vim.api.nvim_buf_set_lines(
				bufnr,
				keylist_range.start,
				keylist_range.final,
				false,
				formatted_text
			)
		end
	end
end

vim.api.nvim_create_autocmd('BufWritePre', {
	desc = 'Format keymap',
	group = vim.api.nvim_create_augroup('QMK', {}),
	pattern = '*keymap.c',
	callback = format_qmk_keymaps,
})
