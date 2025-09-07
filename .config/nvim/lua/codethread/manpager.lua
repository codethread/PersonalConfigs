-- called by MANPAGER
-- Man! will set this, but i want to quit all from cli
vim.keymap.del('n', 'q', { buffer = true })
vim.keymap.set('n', 'q', ':qa!<CR>', { desc = 'clos!' })
