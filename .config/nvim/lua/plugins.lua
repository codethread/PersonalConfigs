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

	-- misc
	use("wakatime/vim-wakatime")
	use("folke/which-key.nvim")
	use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
	use("nvim-treesitter/playground")

	-- help for lua, TODO need to make this work
	-- use 'wsdjeg/luarefvim'
	-- use 'rafcamlet/nvim-luapad'

	-- colorscheme
	-- TODO: https://github.com/rebelot/kanagawa.nvim
	use("shaunsingh/nord.nvim")
	use({
		"mcchrish/zenbones.nvim",
		-- Optionally install Lush. Allows for more configuration or extending the colorscheme
		-- If you don't want to install lush, make sure to set g:zenbones_compat = 1
		-- In Vim, compat mode is turned on as Lush only works in Neovim.
		requires = "rktjmp/lush.nvim",
	})
	-- use 'navarasu/onedark.nvim'

	-- modeline
	use({
		"hoob3rt/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
	})
	use("arkav/lualine-lsp-progress")

	-- project navigation
	use({
		"goolord/alpha-nvim",
		config = function()
			require("dashboard")
		end,
	})
	use("nvim-telescope/telescope.nvim")
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	-- use({ "gbrlsnchs/telescope-lsp-handlers.nvim" })
	use({ "nvim-telescope/telescope-ui-select.nvim" })
	use({
		"lewis6991/spellsitter.nvim",
		config = function()
			require("spellsitter").setup()
		end,
	})

	use({
		"kyazdani42/nvim-tree.lua",
		requires = {
			"kyazdani42/nvim-web-devicons", -- optional, for file icon
		},
		config = function()
			require("nvim-tree").setup({})
		end,
	})

	-- use("tpope/vim-vinegar")

	use("tpope/vim-eunuch") -- unix helpers, :Rename, :Delete

	-- project editing
	use("famiu/bufdelete.nvim") -- delete buffer
	use({ -- automatically creates missing folders
		"jghauser/mkdir.nvim",
		config = function()
			require("mkdir")
		end,
	})
	use("windwp/nvim-spectre") -- find/replace

	-- lsp
	use("neovim/nvim-lspconfig")
	use("williamboman/nvim-lsp-installer")
	use("tamago324/nlsp-settings.nvim") -- language server settings defined in json for
	use("jose-elias-alvarez/null-ls.nvim")
	use("jose-elias-alvarez/nvim-lsp-ts-utils")

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
	use("nvim-treesitter/nvim-treesitter-textobjects")
	use("windwp/nvim-ts-autotag") -- close <div tags, and ciw
	use("tpope/vim-commentary")
	use("tpope/vim-surround")
	use("windwp/nvim-autopairs") -- Autopairs, integrates with both cmp and treesitter
	use("mbbill/undotree")

	use("JoosepAlviste/nvim-ts-context-commentstring") -- uses tree sitter for comment detection

	-- git
	use({ "TimUntersberger/neogit" })
	use("lewis6991/gitsigns.nvim")

	use("tpope/vim-fugitive")
	use("tpope/vim-rhubarb") -- :GBrowse and other git things

	-- terminal
	use("akinsho/toggleterm.nvim")
	use({ "camgraff/telescope-tmux.nvim" })

	use({
		"christoomey/vim-tmux-navigator",
		config = function()
			vim.cmd([[
      " Disable tmux navigator when zooming the Vim pane
      let g:tmux_navigator_disable_when_zoomed = 1

      let g:tmux_navigator_no_mappings = 1
      nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
      nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
      nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
      nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
      ]])
		end,
	})

	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
