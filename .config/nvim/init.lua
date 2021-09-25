require('plugins')

require('nord').set()

-- use space as a the leader key
vim.g.mapleader = ' '

-- local nnoremap = vim.keymap.nnoremap
local nnoremap = require('astronauta.keymap').nnoremap

nnoremap { '<leader>hello', function() print("Hello world, from lua") end }


local set = vim.opt
-- Set the behavior of tab
set.tabstop = 2
set.shiftwidth = 2
set.softtabstop = 2
set.expandtab = true
vim.opt.cursorline = true

require('lualine').setup({
  options = {
    icons_enabled = true,
    theme = 'nord',
    component_separators = {'', ''},
    section_separators = {'', ''},
    disabled_filetypes = {}
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {}

})
-- vim.cmd[[colorscheme nord]]
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('fzf')
-- vim.cmd 'source ~/.config/nvim/keymap.vim'

local tele = require('telescope.builtin')
nnoremap { '<leader><leader>', function() tele.find_files() end }
nnoremap { '<leader>h', function() tele.help_tags() end }
nnoremap { '<leader>bl', function() tele.buffers() end }
nnoremap { '<leader>sf', function() tele.live_grep() end }
-- nnoremap { '<leader>sf' <cmd>Telescope live_grep<cr> }
-- nnoremap { '<leader>bl' <cmd>Telescope buffers<cr> }
-- nnoremap { '<leader>pt' <cmd>Telescope help_tags<cr> }

--vim.cmd 'source ~/.config/nvim/keymap.vim'
--
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  -- ignore_install = { "javascript" }, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = {   -- list of language that will be disabled
      "c", 
      "commonlisp"  -- chockes on my elisp ligatures
    },
  },
}

local nvim_lsp = require("lspconfig")

require("null-ls").config {}
require("lspconfig")["null-ls"].setup {}

nvim_lsp.tsserver.setup{
  on_attach = function(client, bufnr)
        -- disable tsserver formatting if you plan on formatting via null-ls
        client.resolved_capabilities.document_formatting = false
        client.resolved_capabilities.document_range_formatting = false

        local ts_utils = require("nvim-lsp-ts-utils")

        -- defaults
        ts_utils.setup {
            debug = false,
            disable_commands = false,
            enable_import_on_completion = false,

            -- import all
            import_all_timeout = 5000, -- ms
            import_all_priorities = {
                buffers = 4, -- loaded buffer names
                buffer_content = 3, -- loaded buffer content
                local_files = 2, -- git files or files with relative path markers
                same_file = 1, -- add to existing import statement
            },
            import_all_scan_buffers = 100,
            import_all_select_source = false,

            -- eslint
            eslint_enable_code_actions = true,
            eslint_enable_disable_comments = true,
            eslint_bin = "eslint_d",
            eslint_enable_diagnostics = true,
            eslint_opts = {},

            -- formatting
            enable_formatting = false,
            formatter = "prettier",
            formatter_opts = {},

            -- update imports on file move
            update_imports_on_move = false,
            require_confirmation_on_move = false,
            watch_dir = nil,

            -- filter diagnostics
            filter_out_diagnostics_by_severity = {},
            filter_out_diagnostics_by_code = {},
        }

        -- required to fix code action ranges and filter diagnostics
        ts_utils.setup_client(client)

        -- no default maps, so you may want to define some here
        local opts = { silent = true }
        vim.api.nvim_buf_set_keymap(bufnr, "n", "gs", ":TSLspOrganize<CR>", opts)
        vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", ":TSLspRenameFile<CR>", opts)
        vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", ":TSLspImportAll<CR>", opts)
    end
}
