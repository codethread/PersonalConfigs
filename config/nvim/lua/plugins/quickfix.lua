return {
	{
		-- better quickfix
		'kevinhwang91/nvim-bqf',
		ft = 'qf',
		init = function()
			vim.cmd [[
				nnoremap < :cprevious<CR>
				nnoremap > :cnext<CR>
				]]

			--[[stylua: ignore]] --format
			Keys.localleader_ft('qf', {
	{ '>', 'Next list', function() require('bqf.qfwin.handler').navHistory(true) end  },
	{ '<', 'Prev list', function() require('bqf.qfwin.handler').navHistory(false) end },
			})
		end,
		---@type BqfConfig
		opts = {
			---@diagnostic disable-next-line: missing-fields
			preview = {
				winblend = 0,
			},
			func_map = {
				filter = '<C-o>', -- create new list for signed items
				filterr = '<C-b>', -- create new list for non-signed items
			},
		},
	},
}
