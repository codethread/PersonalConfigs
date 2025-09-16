return {
	{
		'williamboman/mason.nvim',
		cmd = 'Mason',
		version = '^1.11.0',
		opts = {
			ensure_installed = {},
		},
		---@param opts MasonSettings | {ensure_installed: string[]}
		config = function(_, opts)
			require('mason').setup(opts)
			local registry = require 'mason-registry'

			registry:on(
				'package:handle',
				vim.schedule_wrap(
					function(pkg, _)
						vim.notify(string.format('Installing %s', pkg.name), 'info', {
							title = 'Mason',
							timeout = 1500,
						})
					end
				)
			)

			registry:on(
				'package:install:success',
				vim.schedule_wrap(
					function(pkg, _)
						vim.notify(string.format('Successfully installed %s', pkg.name), 'info', {
							title = 'Mason',
							timeout = 1500,
						})
					end
				)
			)
			local mr = require 'mason-registry'
			local function ensure_installed()
				for _, tool in ipairs(opts.ensure_installed) do
					local p = mr.get_package(tool)
					if not p:is_installed() then p:install() end
				end
			end
			if mr.refresh then
				mr.refresh(ensure_installed)
			else
				ensure_installed()
			end
		end,
	},
}
