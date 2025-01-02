--- Misc commands that I don't know where else to put, but don't deserve a keymap

vim.api.nvim_create_user_command(
	'News',
	function()
		Snacks.win {
			file = vim.api.nvim_get_runtime_file('doc/news.txt', false)[1],
			width = 0.6,
			height = 0.6,
			wo = {
				spell = false,
				wrap = false,
				signcolumn = 'yes',
				statuscolumn = ' ',
				conceallevel = 3,
			},
		}
	end,
	{}
)
