return {
	{
		'zbirenbaum/copilot-cmp',
		config = true,
		cmd = 'Copilot',
		event = 'InsertEnter',
		dependencies = {
			{
				-- Because the copilot server takes some time to start up, it is recommend that you lazy load copilot
				'zbirenbaum/copilot.lua',
				opts = {
					copilot_node_command = vim.fn.expand '$HOME'
						.. '/.volta/tools/image/node/18.16.0/bin/node',
					suggestion = { enabled = false },
					panel = { enabled = false },
				},
				init = function()
					vim.api.nvim_create_autocmd('BufEnter', {
						desc = 'project level',
						group = vim.api.nvim_create_augroup('Copilot', {}),
						pattern = os.getenv 'HOME' .. '/dev/projects/generated/*',
						callback = function() vim.cmd [[let b:copilot_enabled = v:false]] end,
					})
				end,
			},
		},
	},
}
