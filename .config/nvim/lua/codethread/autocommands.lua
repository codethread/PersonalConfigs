vim.cmd [[
" show cursor line for inactive buffer to make context switching easier
augroup CursorLine
  au!
  au WinEnter,BufWinEnter * setlocal nocursorline
  au WinLeave * setlocal cursorline
augroup END

augroup JSONChange
  au! BufRead,BufNewFile *.json set filetype=jsonc
augroup END
]]

-- Highlight on yank
vim.api.nvim_create_autocmd('TextYankPost', {
	callback = function()
		vim.highlight.on_yank {
			higroup = 'WildMenu',
			on_macro = true,
		}
	end,
	group = vim.api.nvim_create_augroup('YankHighlight', { clear = true }),
	pattern = '*',
})

-- stolen from https://github.com/mitinarseny/dotfiles/blob/4c78f07c0d046d491d086651aea6cf65ea9e6a1a/.config/nvim/after/plugin/lsp.lua#L185-L233
-- referenced https://github.com/rcarriga/nvim-notify/issues/70
-- vim.api.nvim_create_autocmd({ 'UIEnter' }, {
-- 	once = true,
-- 	callback = function()
-- 		local Spinner = require 'codethread.spinner'
-- 		local spinners = {}

-- 		local function format_msg(msg, percentage)
-- 			msg = msg or ''
-- 			if not percentage then return msg end
-- 			return string.format('%2d%%\t%s', percentage, msg)
-- 		end

-- 		vim.api.nvim_create_autocmd({ 'User' }, {
-- 			pattern = { 'LspProgressUpdate' },
-- 			group = vim.api.nvim_create_augroup('LSPNotify', { clear = true }),
-- 			desc = 'LSP progress notifications',
-- 			callback = function()
-- 				for _, c in ipairs(vim.lsp.get_active_clients()) do
-- 					for token, ctx in pairs(c.messages.progress) do
-- 						if not spinners[c.id] then spinners[c.id] = {} end
-- 						local s = spinners[c.id][token]
-- 						if not ctx.done then
-- 							if not s then
-- 								spinners[c.id][token] = Spinner(
-- 									format_msg(ctx.message, ctx.percentage),
-- 									vim.log.levels.INFO,
-- 									{
-- 										title = ctx.title and string.format(
-- 											'%s: %s',
-- 											c.name,
-- 											ctx.title
-- 										) or c.name,
-- 									}
-- 								)
-- 							else
-- 								s:update(format_msg(ctx.message, ctx.percentage))
-- 							end
-- 						else
-- 							c.messages.progress[token] = nil
-- 							if s then
-- 								s:done(ctx.message or 'Complete', nil, {
-- 									icon = '',
-- 								})
-- 								spinners[c.id][token] = nil
-- 							end
-- 						end
-- 					end
-- 				end
-- 			end,
-- 		})
-- 	end,
-- })

local client_notifs = {}

local function get_notif_data(client_id, token)
	if not client_notifs[client_id] then client_notifs[client_id] = {} end

	if not client_notifs[client_id][token] then client_notifs[client_id][token] = {} end

	return client_notifs[client_id][token]
end

local spinner_frames = { '⣾', '⣽', '⣻', '⢿', '⡿', '⣟', '⣯', '⣷' }

local function update_spinner(client_id, token)
	local notif_data = get_notif_data(client_id, token)

	if notif_data.spinner then
		local new_spinner = (notif_data.spinner + 1) % #spinner_frames
		notif_data.spinner = new_spinner

		notif_data.notification = vim.notify(nil, nil, {
			hide_from_history = true,
			icon = spinner_frames[new_spinner],
			replace = notif_data.notification,
		})

		vim.defer_fn(function() update_spinner(client_id, token) end, 100)
	end
end

local function format_title(title, client_name)
	return client_name .. (#title > 0 and ': ' .. title or '')
end

local function format_message(message, percentage)
	return (percentage and percentage .. '%\t' or '') .. (message or '')
end

vim.lsp.handlers['$/progress'] = function(_, result, ctx)
	local client_id = ctx.client_id

	local val = result.value

	if not val.kind then return end

	local notif_data = get_notif_data(client_id, result.token)

	if val.kind == 'begin' then
		local message = format_message(val.message, val.percentage)

		notif_data.notification = vim.notify(message, 'info', {
			title = format_title(val.title, vim.lsp.get_client_by_id(client_id).name),
			icon = spinner_frames[1],
			timeout = false,
			hide_from_history = true,
		})

		notif_data.spinner = 1
		update_spinner(client_id, result.token)
	elseif val.kind == 'report' and notif_data then
		notif_data.notification = vim.notify(format_message(val.message, val.percentage), 'info', {
			replace = notif_data.notification,
			hide_from_history = false,
		})
	elseif val.kind == 'end' and notif_data then
		notif_data.notification =
			vim.notify(val.message and format_message(val.message) or 'Complete', 'info', {
				icon = '',
				replace = notif_data.notification,
				timeout = 3000,
				hide_from_history = true,
			})

		notif_data.spinner = nil
	end
end
