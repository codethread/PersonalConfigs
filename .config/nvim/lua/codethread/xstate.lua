local function isempty(s) return s == nil or s == '' or (type(s) == 'table' and s[1] == '') end

local function getErrors(s)
	local errs = {}
	local errd = false
	for _, msg in ipairs(s) do
		if msg:find '^Error' then
			table.insert(errs, msg)
			errd = true
		end
	end

	if errd then
		return errs
	else
		return nil
	end
end

local machine_notifications = {
	info = nil,
	warn = 0,
	err = 0,
}

local function machine_save()
	local file = vim.fn.expand '%'

	machine_notifications.info = vim.notify('Saving...', 'info', {
		title = 'Xstate cli',
		hide_from_history = true,
		timeout = 1500,
		on_close = function() machine_notifications.info = nil end,
	}).id

	vim.fn.jobstart({ 'yarn', 'xstate', 'typegen', file }, {
		env = {
			NODE_ENV = 'production', -- workaround for warning
		},
		stdout_buffered = true,
		stderr_buffered = true,
		on_exit = function(_, code)
			if code == 0 then
				vim.notify('Machine file saved', 'info', {
					replace = machine_notifications.info,
				})
			else
				vim.notify('Machine not saved', 'error', {
					replace = machine_notifications.info,
				})
			end
		end,
		on_stdout = function(_, data)
			if not isempty(data) then
				local msg = getErrors(data)
				if msg then
					vim.notify(msg, 'warn', {
						title = 'Xstate cli',
						timeout = 3000,
					})
				end
			end
		end,
		on_stderr = function(_, data)
			if not isempty(data) then
				vim.notify(data, 'warn', {
					title = 'Xstate cli',
					timeout = 3000,
				})
			end
		end,
	})
end

vim.api.nvim_create_autocmd({ 'BufWritePost' }, {
	pattern = { '*machine.ts', '*machine.tsx' },
	group = vim.api.nvim_create_augroup('Xstate', { clear = true }),
	callback = machine_save,
})
