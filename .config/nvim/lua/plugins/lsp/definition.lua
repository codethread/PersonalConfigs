local M = {}

---@class ct.lspItem
---@field col number
---@field end_col number
---@field lnum number
---@field end_lnum number
---@field text string

---@class ct.OnList
---@field reuse_win boolean
---@field filter fun(item: ct.lspItem): boolean

---Filter out definitions based on a filter function
---Tries to use the default lsp.buf.definition implementation otherwise
---@param list_opts ct.OnList
function M.on_list_fact(list_opts)
	---@param opts vim.lsp.LocationOpts.OnList
	return function(opts)
		local title = 'LSP locations'
		local all_items = #opts.items == 1 and opts.items
			or vim.iter(opts.items):filter(list_opts.filter):totable()

		-- extracted from lsp.buf.def
		local jump = function()
			--#region assed this bit
			local win = vim.api.nvim_get_current_win()
			local from = vim.fn.getpos '.'
			local tagname = vim.fn.expand '<cword>'
			--#endregion

			local item = all_items[1]
			local b = item.bufnr or vim.fn.bufadd(item.filename)

			-- Save position in jumplist
			vim.cmd "normal! m'"
			-- Push a new item into tagstack
			local tagstack = { { tagname = tagname, from = from } }
			vim.fn.settagstack(vim.fn.win_getid(win), { items = tagstack }, 't')

			vim.bo[b].buflisted = true
			local w = list_opts.reuse_win and vim.fn.win_findbuf(b)[1] or win
			vim.api.nvim_win_set_buf(w, b)
			vim.api.nvim_win_set_cursor(w, { item.lnum, item.col - 1 })
			vim._with({ win = w }, function()
				-- Open folds under the cursor
				vim.cmd 'normal! zv'
			end)
		end

		if #all_items == 1 then
			jump()
			return
		end

		vim.fn.setqflist({}, ' ', { title = title, items = all_items })
		vim.cmd 'botright copen'
	end
end

return M
