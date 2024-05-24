return {
	{
		'kevinhwang91/nvim-ufo',
		dependencies = { 'kevinhwang91/promise-async', 'nvim-treesitter/nvim-treesitter' },
		version = 'v1.*',
		init = function()
			vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
			vim.o.foldlevelstart = 99
			vim.o.foldenable = true

			-- hide foldcolumn, can be enabled with foldcolumn = 1. This is a list in the left gutter, it's not useful for me
			vim.o.foldcolumn = '0'
			-- vim.o.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]] -- add nice fold icons
		end,
		opts = {
			close_fold_kinds_for_ft = {
				default = { 'imports' },
			},
			provider_selector = function(_, filetype)
				local ftMap = {
					vim = 'indent',
					typescriptreact = 'lsp',
					typescript = 'lsp',
					rust = 'lsp',
					git = '',
				}

				return ftMap[filetype] or { 'treesitter', 'indent' }
			end,
			fold_virt_text_handler = function(virtText, lnum, endLnum, width, truncate)
				local newVirtText = {}
				local suffix = ('  %d '):format(endLnum - lnum)
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
