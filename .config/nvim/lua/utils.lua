local M = {}

-- Keybindings / mappings
function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

function nmap(lhs, rhs, opts)
  map('n', lhs, rhs, opts)
end

function vmap(lhs, rhs, opts)
  map('v', lhs, rhs, opts)
end

return {
  map = map,
  nmap = nmap,
  vmap = vmap
}
