local status_ok, glow = pcall(require, 'glow')
if not status_ok then
	print 'could not load glow'
	return
end

glow.setup {
	border = 'single',
	-- your override config
}

vim.api.nvim_create_autocmd('User', {
	desc = 'reload Glow after theme change',
	group = vim.api.nvim_create_augroup('MyGlow', {}),
	pattern = { 'ThemeChangedDark' },
	callback = function()
		vim.pretty_print 'laoded dark'
		glow.setup {
			style = 'dark',
		}
	end,
})
vim.api.nvim_create_autocmd('User', {
	desc = 'reload Glow after theme change',
	group = vim.api.nvim_create_augroup('MyGlow', {}),
	pattern = { 'ThemeChangedLight' },
	callback = function()
		vim.pretty_print 'laoded light'
		glow.setup {
			style = 'light',
		}
	end,
})
