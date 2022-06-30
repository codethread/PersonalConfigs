-- COMMANDS
-- Remove any disabled or unused plugins
-- :PackerClean
-- Perform `PackerUpdate` and then `PackerCompile`
-- :PackerSync
--
-- stuff is stored at .local/share/nvim

-- Bootstrap Packer
require("codethread.plugins_init")

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
	-- snapshot = "zero-point-six",
	-- snapshot_path = vim.fn.expand("%:h"),
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
	-- use({ "shaunsingh/nord.nvim" })
	use("folke/tokyonight.nvim")

	-- modeline
	use({
		"nvim-lualine/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
	})
	use("arkav/lualine-lsp-progress")

	-- project navigation
	use({
		"goolord/alpha-nvim",
		config = function()
			require("codethread.dashboard")
		end,
	})
	use("nvim-telescope/telescope.nvim")
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	-- use({ "nvim-telescope/telescope-ui-select.nvim" })
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
			require("nvim-tree").setup({
				hijack_netrw = false,
				filters = { -- remove things from view
					dotfiles = false,
				},
				-- view = {
				-- 	auto_resize = true,
				-- },
				actions = {
					change_dir = {
						enable = false, -- stay in the current directory
					},
					open_file = {
						quit_on_open = true,
						window_picker = {
							chars = "jfkdlsa;",
						},
					},
				},
			})
		end,
	})

	use("tpope/vim-vinegar") -- netrw improvements

	use("tpope/vim-eunuch") -- unix helpers, :Rename, :Delete

	-- visual
	use({
		"norcalli/nvim-colorizer.lua", -- show colors for css
		config = function()
			require("colorizer").setup({
				"css",
				"scss",
				"html",
				"svelte",
			})
		end,
	})

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
	use("jose-elias-alvarez/null-ls.nvim")
	use("jose-elias-alvarez/nvim-lsp-ts-utils")
	use("b0o/schemastore.nvim")
	use("gbrlsnchs/telescope-lsp-handlers.nvim")

	-- completion
	use("hrsh7th/nvim-cmp")
	use("hrsh7th/cmp-buffer")
	use("hrsh7th/cmp-path")
	use("hrsh7th/cmp-cmdline")
	use("hrsh7th/cmp-nvim-lua")
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
	use("tpope/vim-abolish") -- string  Coercion
	-- Want to turn fooBar into foo_bar? Press crs (coerce to snake_case). MixedCase
	-- (crm), camelCase (crc), snake_case (crs), UPPER_CASE (cru), dash-case (cr-),
	-- dot.case (cr.), space case (cr<space>), and Title Case (crt) are all just 3 keystrokes away.

	use("JoosepAlviste/nvim-ts-context-commentstring") -- uses tree sitter for comment detection

	use({
		"phaazon/hop.nvim",
		branch = "v1", -- optional but strongly recommended
		config = function()
			-- you can configure Hop the way you like here; see :h hop-config
			require("hop").setup({ keys = "fjdksla;rucnei" })
			vim.api.nvim_set_keymap(
				"n",
				"f",
				"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>",
				{}
			)
			vim.api.nvim_set_keymap(
				"n",
				"F",
				"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>",
				{}
			)
			vim.api.nvim_set_keymap(
				"o",
				"f",
				"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true, inclusive_jump = true })<cr>",
				{}
			)
			vim.api.nvim_set_keymap(
				"o",
				"F",
				"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true, inclusive_jump = true })<cr>",
				{}
			)
			vim.api.nvim_set_keymap(
				"",
				"t",
				"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>",
				{}
			)
			vim.api.nvim_set_keymap(
				"",
				"T",
				"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>",
				{}
			)

			vim.api.nvim_set_keymap(
				"n",
				"s",
				"<cmd>lua require'hop'.hint_char2({ jump_on_sole_occurrence = true })<cr>",
				{}
			)
		end,
	})

	-- git
	use({
		"TimUntersberger/neogit",
		config = function()
			require("neogit").setup({
				integrations = {
					diffview = true,
				},
			})
		end,
	})
	use("lewis6991/gitsigns.nvim")
	use({ "sindrets/diffview.nvim" })

	-- use("tpope/vim-fugitive")
	-- use("tpope/vim-rhubarb") -- :GBrowse and other git things

	-- terminal
	use("akinsho/toggleterm.nvim")

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
