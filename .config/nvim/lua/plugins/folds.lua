return {
	{
		'kevinhwang91/nvim-ufo',
		dependencies = { 'kevinhwang91/promise-async', 'nvim-treesitter/nvim-treesitter' },
		lazy = false,
		-- event = { 'BufReadPre', 'BufNewFile' },
		version = 'v1.*',
		init = function()
			vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
			vim.o.foldlevelstart = 99
			vim.o.foldenable = true

			-- hide foldcolumn
			vim.o.foldcolumn = '0'
			-- or show with
			-- vim.o.foldcolumn = '1'
			-- vim.o.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]] -- add nice fold icons
		end,
		keys = {
			{ 'zR', function() require('ufo').openAllFolds() end, desc = 'open all folds' },
			{
				'zM',
				function() require('ufo').closeAllFolds() end,
				desc = 'close all folds',
			},
			{
				'zp',
				function() require('ufo').peekFoldedLinesUnderCursor() end,
				desc = 'peak lines',
			},
			{
				'-',
				'zc',
				desc = 'open fold under cursor',
			},
			{
				'=',
				'zo',
				desc = 'close fold under cursor',
			},
			{
				'_',
				'zC',
				desc = 'close all folds under cursor',
			},
			{
				'+',
				'zO',
				desc = 'open all folds under cursor',
			},
		},
		opts = {
			provider_selector = function() return { 'treesitter', 'indent' } end,
			fold_virt_text_handler = function(virtText, lnum, endLnum, width, truncate)
				local newVirtText = {}
				local suffix = ('  %d '):format(endLnum - lnum)
				local sufWidth = vim.fn.strdisplaywidth(suffix)
				local targetWidth = width - sufWidth
				local curWidth = 0
				for _, chunk in ipairs(virtText) do
					local chunkText = chunk[1]
					local chunkWidth = vim.fn.strdisplaywidth(chunkText)
					if targetWidth > curWidth + chunkWidth then
						table.insert(newVirtText, chunk)
					else
						chunkText = truncate(chunkText, targetWidth - curWidth)
						local hlGroup = chunk[2]
						table.insert(newVirtText, { chunkText, hlGroup })
						chunkWidth = vim.fn.strdisplaywidth(chunkText)
						-- str width returned from truncate() may less than 2nd argument, need padding
						if curWidth + chunkWidth < targetWidth then
							suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
						end
						break
					end
					curWidth = curWidth + chunkWidth
				end
				table.insert(newVirtText, { suffix, 'MoreMsg' })
				return newVirtText
			end,
		},
	},
}
