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

			U.keys('qf', {
				{ '>', function() require('bqf.qfwin.handler').navHistory(true) end, 'Next list' },
				{
					'<',
					function() require('bqf.qfwin.handler').navHistory(false) end,
					'Prev list',
				},
			})
		end,
		---@type BqfConfig
		opts = {
			func_map = {
				filter = '<C-o>', -- create new list for signed items
				filterr = '<C-b>', -- create new list for non-signed items
			},
		},
	},
}
