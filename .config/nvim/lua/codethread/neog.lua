local status_ok, neogit = pcall(require, "neogit")

if not status_ok then
	print("could not load neogit")
	return
end

neogit.setup({})
