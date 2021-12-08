require('general.settings')
vim.cmd 'source ~/.config/nvim/lua/general/mappings.vim'
require('plugins')

require('nord').set()
-- require('onedark').setup()

require('modeline')
require('tele')

require('completion')

require('lsp.config')
require('lsp.typescript')
require('lsp.rust')

-- require('nvim-autopairs').setup {}

-- local nnoremap = vim.keymap.nnoremap
-- local nnoremap = require('astronauta.keymap').nnoremap

-- nnoremap {
--   '<leader>hello', function()
--     print("Hello world, from lua")
--   end
-- }

-- vim.cmd[[colorscheme nord]]
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
-- vim.cmd 'source ~/.config/nvim/keymap.vim'

-- vim.cmd 'source ~/.config/nvim/keymap.vim'
--
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  -- ignore_install = { "javascript" }, -- List of parsers to ignore installing
  highlight = {
    enable = true, -- false will disable the whole extension
    disable = { -- list of language that will be disabled
      "c", "commonlisp" -- chockes on my elisp ligatures
    }
  }
}

HOME = vim.fn.expand('$HOME')

-- set the path to the sumneko installation; if you previously installed via the now deprecated :LspInstall, use
local sumneko_root_path = HOME .. '/.config/nvim/lua-language-server'
local sumneko_binary = HOME
                           .. '/.config/nvim/lua-language-server/bin/macOS/lua-language-server'

local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

require'lspconfig'.sumneko_lua.setup {
  cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"},
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
        -- Setup your lua path
        path = runtime_path
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'}
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true)
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {enable = false}
    }
  }
}

vim.api.nvim_command([[
  autocmd BufWritePre *.lua lua vim.lsp.buf.formatting_sync(nil, 100)
]])

require'lspconfig'.efm.setup {
  init_options = {documentFormatting = true},
  filetypes = {"lua"},
  settings = {
    rootMarkers = {".git/"},
    languages = {
      lua = {
        {
          formatCommand = "lua-format -i --no-keep-simple-function-one-line --no-break-after-operator --column-limit=80 --break-after-table-lb --indent-width=2",
          formatStdin = true
        }
      }
    }
  }
}

require('nvim-treesitter.configs').setup {
  context_commentstring = {enable = true}
}
