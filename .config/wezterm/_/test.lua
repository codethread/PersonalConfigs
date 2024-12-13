local _ = {}

---@class Tagged
---@field _tag string

---@param tbl any
---@param tag 'foo'|'bar'|'baz'
function _.tag(tbl, tag)
	tbl._tag = tag
	return tbl
end

---@class Foo:Tagged
---@field a integer
---@field b number
---@field c Foo

---comment
---@param tbl any
local function Foo(tbl)
	return _.tag(tbl, 'foo') --[[@as Foo]]
end

local ssss = Foo {}

---@type Foo
local y = {}

---@generic A
---@generic B
---@param tbl A[]
---@param fn fun(cb: A): B
---@return B[]
function _.map(tbl, fn) return {} end

---@generic A
---@param tbl A[]
---@param fn fun(cb: A): boolean
---@return A[]
function _.filter(tbl, fn) return {} end

---@type string[]
local ss = {}

local y = _.map(ss, function(xx) return { foo = xx } end)
local a, b = table.unpack(y)
local x = _.map(ss --[=[@as number[]]=], function(xx) return { foo = xx } end)
local a, b = unpack(x)
local z = _.map(ss --[=[@as Foo[]]=], function(xx) return { foo = xx } end)
local a, b = unpack(z)

local a, b = unpack(y)
local www = y[1]

--notes
-- primitives work
-- arrity works
-- tables still act like `Partial` is applied no matter what
-- operators like .. and + seem to result in unknown types
-- generics are very limitied, i.e they work on primitives like string[] but don't work on a generic collection like List<T>
--
-- @return does not enforce anything except a return - and doesn't work for tables, better to use return {} --[[@as blah]]
