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

---Reload all modules inside a package, e.g
---:Reload ct.
---reloads ct.* modules
vim.api.nvim_create_user_command('Reload', function(opts)
	---@param pkg string
	local function reload(pkg)
		local invalidated = {}
		for key in pairs(package.loaded) do
			if vim.startswith(key, pkg) then
				table.insert(invalidated, key)
				require('plenary.reload').reload_module(key)
			end
		end

		vim.notify('cache invalidated for:\n' .. table.concat(invalidated, '\n'))
		vim.notify('Loading: ', pkg)
		require(pkg)
	end

	local pkg = opts.fargs[1]

	if not pkg then
		vim.ui.select({
			'ct',
			'qmk',
			'codethread',
		}, {}, function(choice)
			if not choice then return end
			reload(choice)
		end)
	else
		reload(pkg)
	end
end, {
	desc = 'Reload an entire package by passing the name',
	nargs = '?',
})

vim.api.nvim_create_user_command('DebugSpacing', function()
	local state = vim.b.ct_debug_spacing or false
	if state then
		vim.b.ct_debug_spacing = false
		vim.b.miniindentscope_disable = false
		vim.cmd 'set nolist'
		vim.api.nvim_set_hl(0, 'Whitespace', { fg = U.hl 'base' })
		require('ibl').setup_buffer(0, { enabled = true })
	else
		vim.b.ct_debug_spacing = true
		vim.b.miniindentscope_disable = true
		vim.cmd 'set list'
		vim.api.nvim_set_hl(0, 'Whitespace', { fg = U.hl 'leaf' })
		require('ibl').setup_buffer(0, { enabled = false })
	end
end, { desc = 'Show listchars to see things like tabs/spaces and trailing lines' })

vim.api.nvim_create_user_command('DiagnosticToggle', function()
	local state = vim.b.ct_diagnostics_off or true
	if state then
		vim.diagnostic.hide()
		vim.b.ct_diagnostics_off = true
	else
		vim.diagnostic.show()
		vim.b.ct_diagnostics_off = false
	end
end, {})
