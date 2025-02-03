local log = require('codethread.logger.init').new { plugin = 'find_node_module' }

---@enum Status
local State = {
	NO_MODULES = 1,
	HIT_ROOT = 2,
}

local M = {}

---comment
---@param opts { root: string, start_dir: string | number, target: string }
---@return string?, Status?
local function search(opts)
	log.info('Search', opts)

	local root, start_dir, target = opts.root, opts.start_dir, opts.target

	local package_root = vim.fs.root(start_dir, 'node_modules')
	if not package_root then return nil, State.NO_MODULES end

	log.info(package_root, #package_root, root, #root)

	log.debug(package_root, root)

	if #package_root < #root then return nil, State.HIT_ROOT end

	local node_modules_dir = vim.fs.joinpath(package_root, 'node_modules', target)

	log.info('finding', node_modules_dir)

	local res = vim.uv.fs_stat(node_modules_dir)

	if res then return node_modules_dir end

	return search {
		root = root,
		start_dir = vim.fs.dirname(package_root), -- go up a dir to search
		target = target,
	}
end

M.find_node_module = function()
	local cWord = vim.fn.expand '<cWORD>'
	local import = string.match(cWord, '"(.*)"')

	if not import then
		error(string.format("word under cursor does not appear to be a module import '%s'", cWord))
	end

	local root = vim.fs.root(0, '.git')

	if not root then error 'not a git module' end

	local found, status = search {
		root = root,
		target = import,
		start_dir = 0,
	}
	if found then
		local pj = vim.fs.joinpath(found, 'package.json')
		vim.cmd.vsplit(pj)
	elseif status == State.NO_MODULES then
		vim.nofitfy(string.format 'Never found node_modules')
	elseif status == State.HIT_ROOT then
		vim.notify(string.format('Never found a module for %s', import))
	end
end

return M
