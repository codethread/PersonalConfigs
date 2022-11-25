-- https://github.com/topics/nvim-cmp
local imap = require('codethread.utils').imap
local cmp_status_ok, cmp = pcall(require, 'cmp')
if not cmp_status_ok then
	print 'could not load cmp'
	return
end

local snip_status_ok, ls = pcall(require, 'luasnip')
if not snip_status_ok then
	print 'could not load luasnip'
	return
end

require('luasnip.loaders.from_vscode').lazy_load {
	paths = {
		'~/.config/nvim/snippets_vscode',
		'~/.local/share/nvim/site/pack/packer/start/friendly-snippets',
	},
}

if cmp == nil then return end

cmp.setup {
	window = {
		completion = {
			winhighlight = 'Normal:Pmenu,FloatBorder:Pmenu,Search:None',
			col_offset = -3,
			side_padding = 0,
		},
		documentation = cmp.config.window.bordered(),
	},
	snippet = {
		expand = function(args)
			ls.lsp_expand(args.body) -- For `luasnip` users.
		end,
	},
	mapping = cmp.mapping.preset.insert {
		['<C-k>'] = cmp.mapping.select_prev_item(),
		['<C-j>'] = cmp.mapping.select_next_item(),
		['<C-l>'] = cmp.mapping.confirm { select = true }, -- select grabs first item if none were selected
		['<Esc>'] = cmp.mapping.abort(),
		-- ['<C-Space>'] = cmp.mapping.complete_common_string(),
		['<C-u>'] = cmp.mapping.scroll_docs(-4),
		['<C-d>'] = cmp.mapping.scroll_docs(4),

		['<Tab>'] = cmp.mapping(function(fallback)
			if ls.expandable() then
				ls.expand()
			elseif ls.expand_or_jumpable() then
				ls.expand_or_jump()
			else
				fallback()
			end
		end, {
			'i',
			's',
		}),
		['<S-Tab>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif ls.jumpable(-1) then
				ls.jump(-1)
			else
				fallback()
			end
		end, {
			'i',
			's',
		}),
	},
	formatting = {
		fields = { 'kind', 'abbr', 'menu' },
		format = function(entry, vim_item)
			local kind = require('lspkind').cmp_format { mode = 'symbol_text', maxwidth = 50 }(
				entry,
				vim_item
			)
			local strings = vim.split(kind.kind, '%s', { trimempty = true })
			kind.kind = ' ' .. strings[1] .. ' '
			kind.menu = '    (' .. strings[2] .. ')'

			return kind
		end,
	},

	sources = {
		{ name = 'nvim_lsp' },
	},
	completion = {
		keyword_length = 2,
	},
	confirm_opts = {
		behavior = cmp.ConfirmBehavior.Replace,
		select = false,
	},
}

cmp.setup.cmdline(':', {
	mapping = cmp.mapping.preset.cmdline(),
	sources = cmp.config.sources({
		{ name = 'path' },
	}, {
		{ name = 'cmdline' },
	}),
	completion = {
		keyword_length = 1,
	},
})

cmp.setup.cmdline('/', {
	mapping = cmp.mapping.preset.cmdline(),
	sources = {
		{ name = 'buffer' },
	},
	completion = {
		keyword_length = 1,
	},
})

-- require('codethread.theme').on_change(
-- 	function() require('codethread.utils').hl(require 'codethread.completion.colors') end
-- )

local function complete(name) cmp.complete { config = { sources = { { name = name } } } } end

imap('<C-Space>', function() complete 'nvim_lsp' end)
imap('<C-x><C-s>', function() complete 'luasnip' end)
imap('<C-x><C-n>', function() complete 'buffer' end)
imap('<C-x><C-f>', function() complete 'path' end)
