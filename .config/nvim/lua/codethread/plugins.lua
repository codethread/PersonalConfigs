-- COMMANDS
-- Remove any disabled or unused plugins
-- :PackerClean
-- Perform `PackerUpdate` and then `PackerCompile`
-- :PackerSync
--
-- stuff is stored at ~/.local/share/nvim
-- cache lives in ~/.cache/nvim/packer

-- Bootstrap Packer
require("codethread.plugins_init")

-- protected load
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	print("could not load packer")
	return
end

packer.startup({
	function(use)
		-- Packer can manage itself
		use({ "wbthomason/packer.nvim", commit = "671076b3a81e5033a866ca769749e75a827784ef" })
		use({ "nvim-lua/plenary.nvim", commit = "968a4b9afec0c633bc369662e78f8c5db0eba249" })
		use({ "nvim-lua/popup.nvim", commit = "b7404d35d5d3548a82149238289fa71f7f6de4ac" })

		-- misc
		use({ "wakatime/vim-wakatime", tag = "9.*" })
		use({ "folke/which-key.nvim", commit = "bd4411a2ed4dd8bb69c125e339d837028a6eea71" })
		use({
			"nvim-treesitter/nvim-treesitter",
			run = ":TSUpdate",
			commit = "3b1ce2e1b30b731c80753fa9bbcb2cfec38a43da",
		})
		use({ "nvim-treesitter/playground", commit = "ce7e4b757598f1c785ed0fd94fc65959acd7d39c" })

		-- help for lua, TODO need to make this work
		-- use 'wsdjeg/luarefvim'
		-- use 'rafcamlet/nvim-luapad'

		-- colorscheme
		-- use({ "shaunsingh/nord.nvim" })
		use({ "folke/tokyonight.nvim" })

		-- modeline
		use({
			"nvim-lualine/lualine.nvim",
			requires = { "kyazdani42/nvim-web-devicons", opt = true },
			commit = "5113cdb32f9d9588a2b56de6d1df6e33b06a554a",
		})
		use({ "arkav/lualine-lsp-progress", commit = "56842d097245a08d77912edf5f2a69ba29f275d7" })

		-- project navigation
		use({
			"goolord/alpha-nvim",
			config = function()
				require("codethread.dashboard")
			end,
		})
		use({ "nvim-telescope/telescope.nvim", commit = "b98b9a93c67cb999493ccdc602e711c8a7a98d64" })
		use({
			"nvim-telescope/telescope-fzf-native.nvim",
			run = "make",
			commit = "6a33ecefa9b3d9ade654f9a7a6396a00c3758ca6",
		})
		-- use({ "nvim-telescope/telescope-ui-select.nvim" })
		use({
			"lewis6991/spellsitter.nvim",
			commit = "c1b318f8b959e015f5cc7941624d1ca0f84705dd",
			config = function()
				require("spellsitter").setup()
			end,
		})

		use({
			"kyazdani42/nvim-tree.lua",
			commit = "72858986f9de019dc0e151c76090de29954081f0",
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
			commit = "36c610a9717cc9ec426a07c8e6bf3b3abcb139d6",
			config = function()
				require("colorizer").setup({ "css", "scss", "html", "svelte" })
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

		use("junegunn/vim-easy-align")
		use({ "phaazon/hop.nvim", branch = "v1" })

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
		use({ "akinsho/toggleterm.nvim", tag = "v1.*" })

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
	end,
	config = {
		snapshot = "packersnap.json",
		snapshot_path = os.getenv("HOME") .. "/PersonalConfigs/.config/nvim",
		display = {
			open_fn = function()
				return require("packer.util").float({ border = "rounded" })
			end,
		},
	},
})

-- Update on save
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]])
