local group = vim.api.nvim_create_augroup('CT_POMO', { clear = true })
local annoy_group = vim.api.nvim_create_augroup('CT_POMO_GROUP', { clear = true })

--- HACK: needs to be exposed over api
local function read_pomo()
	local config_path = os.getenv 'HOME' .. '/Library/Application Support/com.pomo.dev/pomo'
	local ok, f = pcall(vim.fn.readfile, config_path)

	if not ok then
		vim.notify_once 'Pomo config file not found'
		return true
	end

	local json = vim.json.decode(vim.fn.join(f, ''))
	local is_running = U.dig(json, 'store', 'status', 'running')
	return is_running
end

---Check there is currently a pomo timer running
---@param ok? fun(): nil
---@param err? fun(): nil
local function check_pomo_status(ok, err)
	if read_pomo() then
		if ok then ok() end
	else
		if err then err() end
	end
end

local function keep_annoying_me() vim.notify 'Go start that pomo!' end

local function stop_annoying_me() vim.api.nvim_clear_autocmds { group = annoy_group } end

local M = {}

---Create an annoying popup everytime you move your cursor if a pomo timer
---isn't currently running
M.setup = function()
	vim.api.nvim_create_autocmd({ 'FocusGained', 'VimEnter' }, {
		desc = 'check pomo status when returning to code',
		group = group,
		callback = function()
			stop_annoying_me()

			check_pomo_status(nil, function()
				vim.api.nvim_create_autocmd('CursorMoved', {
					group = annoy_group,
					desc = 'annoy me so i dont code till pomo is started',
					callback = function() check_pomo_status(stop_annoying_me, keep_annoying_me) end,
				})
			end)
		end,
	})
end

return M
