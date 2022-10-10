local status_ok, lsp_installer = pcall(require, 'nvim-lsp-installer')
if not status_ok then
	print 'lsp installer failed to load'
	return
end

local cmp_ok, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
if not cmp_ok then
	print 'lsp cmp failed to load'
	return
end

local capabilities = vim.lsp.protocol.make_client_capabilities()

lsp_installer.on_server_ready(function(server)
	local opts = {}

	local config_ok, config_opts = pcall(require, 'codethread.lsp.settings.' .. server.name)

	if config_ok then
		opts = config_opts
	else
		print('no configs for server ' .. server.name)
	end

	opts.capabilities = cmp_nvim_lsp.update_capabilities(capabilities)

	-- nvim-ufo
	-- Tell the server the capability of foldingRange,
	-- Neovim hasn't added foldingRange to default capabilities, users must add it manually
	-- opts.capabilities.textDocument.foldingRange = {
	-- 	dynamicRegistration = false,
	-- 	lineFoldingOnly = true,
	-- }
	-- require('ufo').setup()

	server:setup(opts)
end)
