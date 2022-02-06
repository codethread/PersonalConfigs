-- COMMANDS
-- Remove any disabled or unused plugins
-- :PackerClean
-- Perform `PackerUpdate` and then `PackerCompile`
-- :PackerSync
--
-- stuff is stored at .local/share/nvim

-- Bootstrap Packer
local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
	vim.cmd("packadd packer.nvim")
end

-- Update on save
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]])

-- protected load
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	print("could not load packer")
	return
end

packer.init({
	-- config here
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

return packer.startup(function(use)
	-- Packer can manage itself
	use("wbthomason/packer.nvim")
	use("nvim-lua/plenary.nvim")
	use("nvim-lua/popup.nvim")

	-- help for lua, TODO need to make this work
	-- use 'wsdjeg/luarefvim'
	-- use 'rafcamlet/nvim-luapad'

	-- colorscheme
	use("shaunsingh/nord.nvim")
	-- use 'navarasu/onedark.nvim'

	-- modeline
	use({
		"hoob3rt/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
	})

	-- project navigation
	use({
		"goolord/alpha-nvim",
		config = function()
			require("dashboard")
		end,
	})
	use("nvim-telescope/telescope.nvim")
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })

	use("tpope/vim-fugitive")
	use("tpope/vim-rhubarb")
	use("tpope/vim-vinegar")
	use("tpope/vim-eunuch")

	-- core
	use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
	use("folke/which-key.nvim")

	-- lsp
	use("neovim/nvim-lspconfig")
	use("williamboman/nvim-lsp-installer")
	use("tamago324/nlsp-settings.nvim") -- language server settings defined in json for

	use("jose-elias-alvarez/null-ls.nvim")

	-- completion
	use("hrsh7th/cmp-buffer")
	use("hrsh7th/cmp-path")
	use("hrsh7th/cmp-cmdline")
	use("hrsh7th/cmp-nvim-lua")
	use("hrsh7th/nvim-cmp")
	use("hrsh7th/cmp-nvim-lsp")

	use("saadparwaiz1/cmp_luasnip")
	use("L3MON4D3/LuaSnip")
	use("rafamadriz/friendly-snippets")

	-- editing
	use("tpope/vim-commentary")
	use("tpope/vim-surround")
	use("windwp/nvim-autopairs") -- Autopairs, integrates with both cmp and treesitter
	-- use 'chun-yang/auto-pairs'

	-- uses tree sitter for comment detection
	use("JoosepAlviste/nvim-ts-context-commentstring")

	-- git
	use("lewis6991/gitsigns.nvim")

	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
