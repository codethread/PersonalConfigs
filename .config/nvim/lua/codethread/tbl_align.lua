local M = {}

local log = require('codethread.logger.init').new { plugin = 'tbl_align' }

---Align blocks in a simple but inferred manner.
---`--[[stylua: ignore]] --format` to mark a region
---region expects a table (or function table) containing nested tables
---each row will be aligned according to argument position
M.format_table = function(bufn)
	local ts = vim.treesitter
	local tree = ts.get_parser(bufn)
	assert(tree, 'No treesitter for buff')

	---@class Block
	---@field line number
	---@field width number

	---@type Block[]
	local blocks = {}
	local lines = (vim.api.nvim_buf_get_lines(bufn, 0, -1, false))

	for i, line in ipairs(lines) do
		if line:find('stylua: ignore', 0, true) and line:find('--format', 0, true) then
			---@type Block
			local block = { line = i, width = #line }
			table.insert(blocks, block)
			log.info('line:', line, 'block:', block)
		end
	end
	log.info('blocks', blocks)

	local nested_tables_query = vim.treesitter.query.parse(
		'lua',
		[[(table_constructor (field value: (table_constructor))) @tbl]]
	)

	---@class Changes
	---@field lines string[][]
	---@field start number
	---@field end_ number
	---@type Changes[]
	local changes = {}
	local padding = '\t'

	for _, block in ipairs(blocks) do
		---@type Changes
		local change = { lines = {}, start = -1, end_ = -1 }

		local node = tree:named_node_for_range { block.line, 1, block.line, block.width }
		assert(node, 'no block')
		local tbl = nil
		-- iter till we hit a table
		for id, nd in nested_tables_query:iter_captures(node, bufn) do
			-- TODO: how one line?
			tbl = nd
			break
		end
		if not tbl then return end

		local start_row, _, end_row = tbl:range()
		assert(start_row and end_row, 'missing start or end of node')
		change.start = start_row + 1
		change.end_ = end_row

		local key_rows = {}
		for nd in tbl:iter_children() do
			if nd:named() then table.insert(key_rows, nd) end
		end

		---@type string[][]
		local row_chunks = {}
		---@type number[]
		local col_widths = {}

		for i, row in ipairs(key_rows) do
			row_chunks[i] = {}
			local col = 0
			for child in row:child(0):iter_children() do
				if child:named() then
					col = col + 1
					local text = ts.get_node_text(child, bufn)
					col_widths[col] = math.max(col_widths[col] or 0, #text)
					table.insert(row_chunks[i], text)
				end
			end
		end

		---@type string[][]
		local output = {}

		for _, row in ipairs(row_chunks) do
			local row_text = {}
			for col, chunk in ipairs(row) do
				local remainder = col_widths[col] - #chunk
				if remainder > 0 then chunk = chunk .. string.rep(' ', remainder) end
				table.insert(row_text, chunk)
			end
			table.insert(output, row_text)
		end

		change.lines = output

		table.insert(changes, change)
	end

	local offset = 0
	for _, change in ipairs(changes) do
		local text = {}
		for _, row in ipairs(change.lines) do
			local txt = padding .. '{ ' .. table.concat(row, ', ') .. ' },'
			table.insert(text, txt)
		end
		vim.api.nvim_buf_set_lines(bufn, change.start + offset, change.end_ + offset, true, text)
		--calculate offset create by changing lines
		local original_size = (change.end_ - change.start)
		local new_size = #text
		local diff = new_size - original_size -- e.g was 5 but now 10, so diff is +5, which wil then be used for next insertion
		offset = offset + diff
	end
end

return M
