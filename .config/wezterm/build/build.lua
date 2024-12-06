--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]

local ____modules = {}
local ____moduleCache = {}
local ____originalRequire = require
local function require(file, ...)
    if ____moduleCache[file] then
        return ____moduleCache[file].value
    end
    if ____modules[file] then
        local module = ____modules[file]
        local value = nil
        if (select("#", ...) > 0) then value = module(...) else value = module(file) end
        ____moduleCache[file] = { value = value }
        return value
    else
        if ____originalRequire then
            return ____originalRequire(file)
        else
            error("module '" .. file .. "' not found")
        end
    end
end
____modules = {
["lualib_bundle"] = function(...) 
local function __TS__ArrayAt(self, relativeIndex)
    local absoluteIndex = relativeIndex < 0 and #self + relativeIndex or relativeIndex
    if absoluteIndex >= 0 and absoluteIndex < #self then
        return self[absoluteIndex + 1]
    end
    return nil
end

local function __TS__ArrayIsArray(value)
    return type(value) == "table" and (value[1] ~= nil or next(value) == nil)
end

local function __TS__ArrayConcat(self, ...)
    local items = {...}
    local result = {}
    local len = 0
    for i = 1, #self do
        len = len + 1
        result[len] = self[i]
    end
    for i = 1, #items do
        local item = items[i]
        if __TS__ArrayIsArray(item) then
            for j = 1, #item do
                len = len + 1
                result[len] = item[j]
            end
        else
            len = len + 1
            result[len] = item
        end
    end
    return result
end

local __TS__Symbol, Symbol
do
    local symbolMetatable = {__tostring = function(self)
        return ("Symbol(" .. (self.description or "")) .. ")"
    end}
    function __TS__Symbol(description)
        return setmetatable({description = description}, symbolMetatable)
    end
    Symbol = {
        asyncDispose = __TS__Symbol("Symbol.asyncDispose"),
        dispose = __TS__Symbol("Symbol.dispose"),
        iterator = __TS__Symbol("Symbol.iterator"),
        hasInstance = __TS__Symbol("Symbol.hasInstance"),
        species = __TS__Symbol("Symbol.species"),
        toStringTag = __TS__Symbol("Symbol.toStringTag")
    }
end

local function __TS__ArrayEntries(array)
    local key = 0
    return {
        [Symbol.iterator] = function(self)
            return self
        end,
        next = function(self)
            local result = {done = array[key + 1] == nil, value = {key, array[key + 1]}}
            key = key + 1
            return result
        end
    }
end

local function __TS__ArrayEvery(self, callbackfn, thisArg)
    for i = 1, #self do
        if not callbackfn(thisArg, self[i], i - 1, self) then
            return false
        end
    end
    return true
end

local function __TS__ArrayFill(self, value, start, ____end)
    local relativeStart = start or 0
    local relativeEnd = ____end or #self
    if relativeStart < 0 then
        relativeStart = relativeStart + #self
    end
    if relativeEnd < 0 then
        relativeEnd = relativeEnd + #self
    end
    do
        local i = relativeStart
        while i < relativeEnd do
            self[i + 1] = value
            i = i + 1
        end
    end
    return self
end

local function __TS__ArrayFilter(self, callbackfn, thisArg)
    local result = {}
    local len = 0
    for i = 1, #self do
        if callbackfn(thisArg, self[i], i - 1, self) then
            len = len + 1
            result[len] = self[i]
        end
    end
    return result
end

local function __TS__ArrayForEach(self, callbackFn, thisArg)
    for i = 1, #self do
        callbackFn(thisArg, self[i], i - 1, self)
    end
end

local function __TS__ArrayFind(self, predicate, thisArg)
    for i = 1, #self do
        local elem = self[i]
        if predicate(thisArg, elem, i - 1, self) then
            return elem
        end
    end
    return nil
end

local function __TS__ArrayFindIndex(self, callbackFn, thisArg)
    for i = 1, #self do
        if callbackFn(thisArg, self[i], i - 1, self) then
            return i - 1
        end
    end
    return -1
end

local __TS__Iterator
do
    local function iteratorGeneratorStep(self)
        local co = self.____coroutine
        local status, value = coroutine.resume(co)
        if not status then
            error(value, 0)
        end
        if coroutine.status(co) == "dead" then
            return
        end
        return true, value
    end
    local function iteratorIteratorStep(self)
        local result = self:next()
        if result.done then
            return
        end
        return true, result.value
    end
    local function iteratorStringStep(self, index)
        index = index + 1
        if index > #self then
            return
        end
        return index, string.sub(self, index, index)
    end
    function __TS__Iterator(iterable)
        if type(iterable) == "string" then
            return iteratorStringStep, iterable, 0
        elseif iterable.____coroutine ~= nil then
            return iteratorGeneratorStep, iterable
        elseif iterable[Symbol.iterator] then
            local iterator = iterable[Symbol.iterator](iterable)
            return iteratorIteratorStep, iterator
        else
            return ipairs(iterable)
        end
    end
end

local __TS__ArrayFrom
do
    local function arrayLikeStep(self, index)
        index = index + 1
        if index > self.length then
            return
        end
        return index, self[index]
    end
    local function arrayLikeIterator(arr)
        if type(arr.length) == "number" then
            return arrayLikeStep, arr, 0
        end
        return __TS__Iterator(arr)
    end
    function __TS__ArrayFrom(arrayLike, mapFn, thisArg)
        local result = {}
        if mapFn == nil then
            for ____, v in arrayLikeIterator(arrayLike) do
                result[#result + 1] = v
            end
        else
            local i = 0
            for ____, v in arrayLikeIterator(arrayLike) do
                local ____mapFn_3 = mapFn
                local ____thisArg_1 = thisArg
                local ____v_2 = v
                local ____i_0 = i
                i = ____i_0 + 1
                result[#result + 1] = ____mapFn_3(____thisArg_1, ____v_2, ____i_0)
            end
        end
        return result
    end
end

local function __TS__ArrayIncludes(self, searchElement, fromIndex)
    if fromIndex == nil then
        fromIndex = 0
    end
    local len = #self
    local k = fromIndex
    if fromIndex < 0 then
        k = len + fromIndex
    end
    if k < 0 then
        k = 0
    end
    for i = k + 1, len do
        if self[i] == searchElement then
            return true
        end
    end
    return false
end

local function __TS__ArrayIndexOf(self, searchElement, fromIndex)
    if fromIndex == nil then
        fromIndex = 0
    end
    local len = #self
    if len == 0 then
        return -1
    end
    if fromIndex >= len then
        return -1
    end
    if fromIndex < 0 then
        fromIndex = len + fromIndex
        if fromIndex < 0 then
            fromIndex = 0
        end
    end
    for i = fromIndex + 1, len do
        if self[i] == searchElement then
            return i - 1
        end
    end
    return -1
end

local function __TS__ArrayJoin(self, separator)
    if separator == nil then
        separator = ","
    end
    local parts = {}
    for i = 1, #self do
        parts[i] = tostring(self[i])
    end
    return table.concat(parts, separator)
end

local function __TS__ArrayMap(self, callbackfn, thisArg)
    local result = {}
    for i = 1, #self do
        result[i] = callbackfn(thisArg, self[i], i - 1, self)
    end
    return result
end

local function __TS__ArrayPush(self, ...)
    local items = {...}
    local len = #self
    for i = 1, #items do
        len = len + 1
        self[len] = items[i]
    end
    return len
end

local function __TS__ArrayPushArray(self, items)
    local len = #self
    for i = 1, #items do
        len = len + 1
        self[len] = items[i]
    end
    return len
end

local function __TS__CountVarargs(...)
    return select("#", ...)
end

local function __TS__ArrayReduce(self, callbackFn, ...)
    local len = #self
    local k = 0
    local accumulator = nil
    if __TS__CountVarargs(...) ~= 0 then
        accumulator = ...
    elseif len > 0 then
        accumulator = self[1]
        k = 1
    else
        error("Reduce of empty array with no initial value", 0)
    end
    for i = k + 1, len do
        accumulator = callbackFn(
            nil,
            accumulator,
            self[i],
            i - 1,
            self
        )
    end
    return accumulator
end

local function __TS__ArrayReduceRight(self, callbackFn, ...)
    local len = #self
    local k = len - 1
    local accumulator = nil
    if __TS__CountVarargs(...) ~= 0 then
        accumulator = ...
    elseif len > 0 then
        accumulator = self[k + 1]
        k = k - 1
    else
        error("Reduce of empty array with no initial value", 0)
    end
    for i = k + 1, 1, -1 do
        accumulator = callbackFn(
            nil,
            accumulator,
            self[i],
            i - 1,
            self
        )
    end
    return accumulator
end

local function __TS__ArrayReverse(self)
    local i = 1
    local j = #self
    while i < j do
        local temp = self[j]
        self[j] = self[i]
        self[i] = temp
        i = i + 1
        j = j - 1
    end
    return self
end

local function __TS__ArrayUnshift(self, ...)
    local items = {...}
    local numItemsToInsert = #items
    if numItemsToInsert == 0 then
        return #self
    end
    for i = #self, 1, -1 do
        self[i + numItemsToInsert] = self[i]
    end
    for i = 1, numItemsToInsert do
        self[i] = items[i]
    end
    return #self
end

local function __TS__ArraySort(self, compareFn)
    if compareFn ~= nil then
        table.sort(
            self,
            function(a, b) return compareFn(nil, a, b) < 0 end
        )
    else
        table.sort(self)
    end
    return self
end

local function __TS__ArraySlice(self, first, last)
    local len = #self
    first = first or 0
    if first < 0 then
        first = len + first
        if first < 0 then
            first = 0
        end
    else
        if first > len then
            first = len
        end
    end
    last = last or len
    if last < 0 then
        last = len + last
        if last < 0 then
            last = 0
        end
    else
        if last > len then
            last = len
        end
    end
    local out = {}
    first = first + 1
    last = last + 1
    local n = 1
    while first < last do
        out[n] = self[first]
        first = first + 1
        n = n + 1
    end
    return out
end

local function __TS__ArraySome(self, callbackfn, thisArg)
    for i = 1, #self do
        if callbackfn(thisArg, self[i], i - 1, self) then
            return true
        end
    end
    return false
end

local function __TS__ArraySplice(self, ...)
    local args = {...}
    local len = #self
    local actualArgumentCount = __TS__CountVarargs(...)
    local start = args[1]
    local deleteCount = args[2]
    if start < 0 then
        start = len + start
        if start < 0 then
            start = 0
        end
    elseif start > len then
        start = len
    end
    local itemCount = actualArgumentCount - 2
    if itemCount < 0 then
        itemCount = 0
    end
    local actualDeleteCount
    if actualArgumentCount == 0 then
        actualDeleteCount = 0
    elseif actualArgumentCount == 1 then
        actualDeleteCount = len - start
    else
        actualDeleteCount = deleteCount or 0
        if actualDeleteCount < 0 then
            actualDeleteCount = 0
        end
        if actualDeleteCount > len - start then
            actualDeleteCount = len - start
        end
    end
    local out = {}
    for k = 1, actualDeleteCount do
        local from = start + k
        if self[from] ~= nil then
            out[k] = self[from]
        end
    end
    if itemCount < actualDeleteCount then
        for k = start + 1, len - actualDeleteCount do
            local from = k + actualDeleteCount
            local to = k + itemCount
            if self[from] then
                self[to] = self[from]
            else
                self[to] = nil
            end
        end
        for k = len - actualDeleteCount + itemCount + 1, len do
            self[k] = nil
        end
    elseif itemCount > actualDeleteCount then
        for k = len - actualDeleteCount, start + 1, -1 do
            local from = k + actualDeleteCount
            local to = k + itemCount
            if self[from] then
                self[to] = self[from]
            else
                self[to] = nil
            end
        end
    end
    local j = start + 1
    for i = 3, actualArgumentCount do
        self[j] = args[i]
        j = j + 1
    end
    for k = #self, len - actualDeleteCount + itemCount + 1, -1 do
        self[k] = nil
    end
    return out
end

local function __TS__ArrayToObject(self)
    local object = {}
    for i = 1, #self do
        object[i - 1] = self[i]
    end
    return object
end

local function __TS__ArrayFlat(self, depth)
    if depth == nil then
        depth = 1
    end
    local result = {}
    local len = 0
    for i = 1, #self do
        local value = self[i]
        if depth > 0 and __TS__ArrayIsArray(value) then
            local toAdd
            if depth == 1 then
                toAdd = value
            else
                toAdd = __TS__ArrayFlat(value, depth - 1)
            end
            for j = 1, #toAdd do
                local val = toAdd[j]
                len = len + 1
                result[len] = val
            end
        else
            len = len + 1
            result[len] = value
        end
    end
    return result
end

local function __TS__ArrayFlatMap(self, callback, thisArg)
    local result = {}
    local len = 0
    for i = 1, #self do
        local value = callback(thisArg, self[i], i - 1, self)
        if __TS__ArrayIsArray(value) then
            for j = 1, #value do
                len = len + 1
                result[len] = value[j]
            end
        else
            len = len + 1
            result[len] = value
        end
    end
    return result
end

local function __TS__ArraySetLength(self, length)
    if length < 0 or length ~= length or length == math.huge or math.floor(length) ~= length then
        error(
            "invalid array length: " .. tostring(length),
            0
        )
    end
    for i = length + 1, #self do
        self[i] = nil
    end
    return length
end

local __TS__Unpack = table.unpack or unpack

local function __TS__ArrayToReversed(self)
    local copy = {__TS__Unpack(self)}
    __TS__ArrayReverse(copy)
    return copy
end

local function __TS__ArrayToSorted(self, compareFn)
    local copy = {__TS__Unpack(self)}
    __TS__ArraySort(copy, compareFn)
    return copy
end

local function __TS__ArrayToSpliced(self, start, deleteCount, ...)
    local copy = {__TS__Unpack(self)}
    __TS__ArraySplice(copy, start, deleteCount, ...)
    return copy
end

local function __TS__ArrayWith(self, index, value)
    local copy = {__TS__Unpack(self)}
    copy[index + 1] = value
    return copy
end

local function __TS__New(target, ...)
    local instance = setmetatable({}, target.prototype)
    instance:____constructor(...)
    return instance
end

local function __TS__InstanceOf(obj, classTbl)
    if type(classTbl) ~= "table" then
        error("Right-hand side of 'instanceof' is not an object", 0)
    end
    if classTbl[Symbol.hasInstance] ~= nil then
        return not not classTbl[Symbol.hasInstance](classTbl, obj)
    end
    if type(obj) == "table" then
        local luaClass = obj.constructor
        while luaClass ~= nil do
            if luaClass == classTbl then
                return true
            end
            luaClass = luaClass.____super
        end
    end
    return false
end

local function __TS__Class(self)
    local c = {prototype = {}}
    c.prototype.__index = c.prototype
    c.prototype.constructor = c
    return c
end

local __TS__Promise
do
    local function makeDeferredPromiseFactory()
        local resolve
        local reject
        local function executor(____, res, rej)
            resolve = res
            reject = rej
        end
        return function()
            local promise = __TS__New(__TS__Promise, executor)
            return promise, resolve, reject
        end
    end
    local makeDeferredPromise = makeDeferredPromiseFactory()
    local function isPromiseLike(value)
        return __TS__InstanceOf(value, __TS__Promise)
    end
    local function doNothing(self)
    end
    local ____pcall = _G.pcall
    __TS__Promise = __TS__Class()
    __TS__Promise.name = "__TS__Promise"
    function __TS__Promise.prototype.____constructor(self, executor)
        self.state = 0
        self.fulfilledCallbacks = {}
        self.rejectedCallbacks = {}
        self.finallyCallbacks = {}
        local success, ____error = ____pcall(
            executor,
            nil,
            function(____, v) return self:resolve(v) end,
            function(____, err) return self:reject(err) end
        )
        if not success then
            self:reject(____error)
        end
    end
    function __TS__Promise.resolve(value)
        if __TS__InstanceOf(value, __TS__Promise) then
            return value
        end
        local promise = __TS__New(__TS__Promise, doNothing)
        promise.state = 1
        promise.value = value
        return promise
    end
    function __TS__Promise.reject(reason)
        local promise = __TS__New(__TS__Promise, doNothing)
        promise.state = 2
        promise.rejectionReason = reason
        return promise
    end
    __TS__Promise.prototype["then"] = function(self, onFulfilled, onRejected)
        local promise, resolve, reject = makeDeferredPromise()
        self:addCallbacks(
            onFulfilled and self:createPromiseResolvingCallback(onFulfilled, resolve, reject) or resolve,
            onRejected and self:createPromiseResolvingCallback(onRejected, resolve, reject) or reject
        )
        return promise
    end
    function __TS__Promise.prototype.addCallbacks(self, fulfilledCallback, rejectedCallback)
        if self.state == 1 then
            return fulfilledCallback(nil, self.value)
        end
        if self.state == 2 then
            return rejectedCallback(nil, self.rejectionReason)
        end
        local ____self_fulfilledCallbacks_0 = self.fulfilledCallbacks
        ____self_fulfilledCallbacks_0[#____self_fulfilledCallbacks_0 + 1] = fulfilledCallback
        local ____self_rejectedCallbacks_1 = self.rejectedCallbacks
        ____self_rejectedCallbacks_1[#____self_rejectedCallbacks_1 + 1] = rejectedCallback
    end
    function __TS__Promise.prototype.catch(self, onRejected)
        return self["then"](self, nil, onRejected)
    end
    function __TS__Promise.prototype.finally(self, onFinally)
        if onFinally then
            local ____self_finallyCallbacks_2 = self.finallyCallbacks
            ____self_finallyCallbacks_2[#____self_finallyCallbacks_2 + 1] = onFinally
            if self.state ~= 0 then
                onFinally(nil)
            end
        end
        return self
    end
    function __TS__Promise.prototype.resolve(self, value)
        if isPromiseLike(value) then
            return value:addCallbacks(
                function(____, v) return self:resolve(v) end,
                function(____, err) return self:reject(err) end
            )
        end
        if self.state == 0 then
            self.state = 1
            self.value = value
            return self:invokeCallbacks(self.fulfilledCallbacks, value)
        end
    end
    function __TS__Promise.prototype.reject(self, reason)
        if self.state == 0 then
            self.state = 2
            self.rejectionReason = reason
            return self:invokeCallbacks(self.rejectedCallbacks, reason)
        end
    end
    function __TS__Promise.prototype.invokeCallbacks(self, callbacks, value)
        local callbacksLength = #callbacks
        local finallyCallbacks = self.finallyCallbacks
        local finallyCallbacksLength = #finallyCallbacks
        if callbacksLength ~= 0 then
            for i = 1, callbacksLength - 1 do
                callbacks[i](callbacks, value)
            end
            if finallyCallbacksLength == 0 then
                return callbacks[callbacksLength](callbacks, value)
            end
            callbacks[callbacksLength](callbacks, value)
        end
        if finallyCallbacksLength ~= 0 then
            for i = 1, finallyCallbacksLength - 1 do
                finallyCallbacks[i](finallyCallbacks)
            end
            return finallyCallbacks[finallyCallbacksLength](finallyCallbacks)
        end
    end
    function __TS__Promise.prototype.createPromiseResolvingCallback(self, f, resolve, reject)
        return function(____, value)
            local success, resultOrError = ____pcall(f, nil, value)
            if not success then
                return reject(nil, resultOrError)
            end
            return self:handleCallbackValue(resultOrError, resolve, reject)
        end
    end
    function __TS__Promise.prototype.handleCallbackValue(self, value, resolve, reject)
        if isPromiseLike(value) then
            local nextpromise = value
            if nextpromise.state == 1 then
                return resolve(nil, nextpromise.value)
            elseif nextpromise.state == 2 then
                return reject(nil, nextpromise.rejectionReason)
            else
                return nextpromise:addCallbacks(resolve, reject)
            end
        else
            return resolve(nil, value)
        end
    end
end

local __TS__AsyncAwaiter, __TS__Await
do
    local ____coroutine = _G.coroutine or ({})
    local cocreate = ____coroutine.create
    local coresume = ____coroutine.resume
    local costatus = ____coroutine.status
    local coyield = ____coroutine.yield
    function __TS__AsyncAwaiter(generator)
        return __TS__New(
            __TS__Promise,
            function(____, resolve, reject)
                local fulfilled, step, resolved, asyncCoroutine
                function fulfilled(self, value)
                    local success, resultOrError = coresume(asyncCoroutine, value)
                    if success then
                        return step(resultOrError)
                    end
                    return reject(nil, resultOrError)
                end
                function step(result)
                    if resolved then
                        return
                    end
                    if costatus(asyncCoroutine) == "dead" then
                        return resolve(nil, result)
                    end
                    return __TS__Promise.resolve(result):addCallbacks(fulfilled, reject)
                end
                resolved = false
                asyncCoroutine = cocreate(generator)
                local success, resultOrError = coresume(
                    asyncCoroutine,
                    function(____, v)
                        resolved = true
                        return __TS__Promise.resolve(v):addCallbacks(resolve, reject)
                    end
                )
                if success then
                    return step(resultOrError)
                else
                    return reject(nil, resultOrError)
                end
            end
        )
    end
    function __TS__Await(thing)
        return coyield(thing)
    end
end

local function __TS__ClassExtends(target, base)
    target.____super = base
    local staticMetatable = setmetatable({__index = base}, base)
    setmetatable(target, staticMetatable)
    local baseMetatable = getmetatable(base)
    if baseMetatable then
        if type(baseMetatable.__index) == "function" then
            staticMetatable.__index = baseMetatable.__index
        end
        if type(baseMetatable.__newindex) == "function" then
            staticMetatable.__newindex = baseMetatable.__newindex
        end
    end
    setmetatable(target.prototype, base.prototype)
    if type(base.prototype.__index) == "function" then
        target.prototype.__index = base.prototype.__index
    end
    if type(base.prototype.__newindex) == "function" then
        target.prototype.__newindex = base.prototype.__newindex
    end
    if type(base.prototype.__tostring) == "function" then
        target.prototype.__tostring = base.prototype.__tostring
    end
end

local function __TS__CloneDescriptor(____bindingPattern0)
    local value
    local writable
    local set
    local get
    local configurable
    local enumerable
    enumerable = ____bindingPattern0.enumerable
    configurable = ____bindingPattern0.configurable
    get = ____bindingPattern0.get
    set = ____bindingPattern0.set
    writable = ____bindingPattern0.writable
    value = ____bindingPattern0.value
    local descriptor = {enumerable = enumerable == true, configurable = configurable == true}
    local hasGetterOrSetter = get ~= nil or set ~= nil
    local hasValueOrWritableAttribute = writable ~= nil or value ~= nil
    if hasGetterOrSetter and hasValueOrWritableAttribute then
        error("Invalid property descriptor. Cannot both specify accessors and a value or writable attribute.", 0)
    end
    if get or set then
        descriptor.get = get
        descriptor.set = set
    else
        descriptor.value = value
        descriptor.writable = writable == true
    end
    return descriptor
end

local function __TS__Decorate(self, originalValue, decorators, context)
    local result = originalValue
    do
        local i = #decorators
        while i >= 0 do
            local decorator = decorators[i + 1]
            if decorator ~= nil then
                local ____decorator_result_0 = decorator(self, result, context)
                if ____decorator_result_0 == nil then
                    ____decorator_result_0 = result
                end
                result = ____decorator_result_0
            end
            i = i - 1
        end
    end
    return result
end

local function __TS__ObjectAssign(target, ...)
    local sources = {...}
    for i = 1, #sources do
        local source = sources[i]
        for key in pairs(source) do
            target[key] = source[key]
        end
    end
    return target
end

local function __TS__ObjectGetOwnPropertyDescriptor(object, key)
    local metatable = getmetatable(object)
    if not metatable then
        return
    end
    if not rawget(metatable, "_descriptors") then
        return
    end
    return rawget(metatable, "_descriptors")[key]
end

local __TS__DescriptorGet
do
    local getmetatable = _G.getmetatable
    local ____rawget = _G.rawget
    function __TS__DescriptorGet(self, metatable, key)
        while metatable do
            local rawResult = ____rawget(metatable, key)
            if rawResult ~= nil then
                return rawResult
            end
            local descriptors = ____rawget(metatable, "_descriptors")
            if descriptors then
                local descriptor = descriptors[key]
                if descriptor ~= nil then
                    if descriptor.get then
                        return descriptor.get(self)
                    end
                    return descriptor.value
                end
            end
            metatable = getmetatable(metatable)
        end
    end
end

local __TS__DescriptorSet
do
    local getmetatable = _G.getmetatable
    local ____rawget = _G.rawget
    local rawset = _G.rawset
    function __TS__DescriptorSet(self, metatable, key, value)
        while metatable do
            local descriptors = ____rawget(metatable, "_descriptors")
            if descriptors then
                local descriptor = descriptors[key]
                if descriptor ~= nil then
                    if descriptor.set then
                        descriptor.set(self, value)
                    else
                        if descriptor.writable == false then
                            error(
                                ((("Cannot assign to read only property '" .. key) .. "' of object '") .. tostring(self)) .. "'",
                                0
                            )
                        end
                        descriptor.value = value
                    end
                    return
                end
            end
            metatable = getmetatable(metatable)
        end
        rawset(self, key, value)
    end
end

local __TS__SetDescriptor
do
    local getmetatable = _G.getmetatable
    local function descriptorIndex(self, key)
        return __TS__DescriptorGet(
            self,
            getmetatable(self),
            key
        )
    end
    local function descriptorNewIndex(self, key, value)
        return __TS__DescriptorSet(
            self,
            getmetatable(self),
            key,
            value
        )
    end
    function __TS__SetDescriptor(target, key, desc, isPrototype)
        if isPrototype == nil then
            isPrototype = false
        end
        local ____isPrototype_0
        if isPrototype then
            ____isPrototype_0 = target
        else
            ____isPrototype_0 = getmetatable(target)
        end
        local metatable = ____isPrototype_0
        if not metatable then
            metatable = {}
            setmetatable(target, metatable)
        end
        local value = rawget(target, key)
        if value ~= nil then
            rawset(target, key, nil)
        end
        if not rawget(metatable, "_descriptors") then
            metatable._descriptors = {}
        end
        metatable._descriptors[key] = __TS__CloneDescriptor(desc)
        metatable.__index = descriptorIndex
        metatable.__newindex = descriptorNewIndex
    end
end

local function __TS__DecorateLegacy(decorators, target, key, desc)
    local result = target
    do
        local i = #decorators
        while i >= 0 do
            local decorator = decorators[i + 1]
            if decorator ~= nil then
                local oldResult = result
                if key == nil then
                    result = decorator(nil, result)
                elseif desc == true then
                    local value = rawget(target, key)
                    local descriptor = __TS__ObjectGetOwnPropertyDescriptor(target, key) or ({configurable = true, writable = true, value = value})
                    local desc = decorator(nil, target, key, descriptor) or descriptor
                    local isSimpleValue = desc.configurable == true and desc.writable == true and not desc.get and not desc.set
                    if isSimpleValue then
                        rawset(target, key, desc.value)
                    else
                        __TS__SetDescriptor(
                            target,
                            key,
                            __TS__ObjectAssign({}, descriptor, desc)
                        )
                    end
                elseif desc == false then
                    result = decorator(nil, target, key, desc)
                else
                    result = decorator(nil, target, key)
                end
                result = result or oldResult
            end
            i = i - 1
        end
    end
    return result
end

local function __TS__DecorateParam(paramIndex, decorator)
    return function(____, target, key) return decorator(nil, target, key, paramIndex) end
end

local function __TS__StringIncludes(self, searchString, position)
    if not position then
        position = 1
    else
        position = position + 1
    end
    local index = string.find(self, searchString, position, true)
    return index ~= nil
end

local Error, RangeError, ReferenceError, SyntaxError, TypeError, URIError
do
    local function getErrorStack(self, constructor)
        if debug == nil then
            return nil
        end
        local level = 1
        while true do
            local info = debug.getinfo(level, "f")
            level = level + 1
            if not info then
                level = 1
                break
            elseif info.func == constructor then
                break
            end
        end
        if __TS__StringIncludes(_VERSION, "Lua 5.0") then
            return debug.traceback(("[Level " .. tostring(level)) .. "]")
        else
            return debug.traceback(nil, level)
        end
    end
    local function wrapErrorToString(self, getDescription)
        return function(self)
            local description = getDescription(self)
            local caller = debug.getinfo(3, "f")
            local isClassicLua = __TS__StringIncludes(_VERSION, "Lua 5.0") or _VERSION == "Lua 5.1"
            if isClassicLua or caller and caller.func ~= error then
                return description
            else
                return (description .. "\n") .. tostring(self.stack)
            end
        end
    end
    local function initErrorClass(self, Type, name)
        Type.name = name
        return setmetatable(
            Type,
            {__call = function(____, _self, message) return __TS__New(Type, message) end}
        )
    end
    local ____initErrorClass_1 = initErrorClass
    local ____class_0 = __TS__Class()
    ____class_0.name = ""
    function ____class_0.prototype.____constructor(self, message)
        if message == nil then
            message = ""
        end
        self.message = message
        self.name = "Error"
        self.stack = getErrorStack(nil, self.constructor.new)
        local metatable = getmetatable(self)
        if metatable and not metatable.__errorToStringPatched then
            metatable.__errorToStringPatched = true
            metatable.__tostring = wrapErrorToString(nil, metatable.__tostring)
        end
    end
    function ____class_0.prototype.__tostring(self)
        return self.message ~= "" and (self.name .. ": ") .. self.message or self.name
    end
    Error = ____initErrorClass_1(nil, ____class_0, "Error")
    local function createErrorClass(self, name)
        local ____initErrorClass_3 = initErrorClass
        local ____class_2 = __TS__Class()
        ____class_2.name = ____class_2.name
        __TS__ClassExtends(____class_2, Error)
        function ____class_2.prototype.____constructor(self, ...)
            ____class_2.____super.prototype.____constructor(self, ...)
            self.name = name
        end
        return ____initErrorClass_3(nil, ____class_2, name)
    end
    RangeError = createErrorClass(nil, "RangeError")
    ReferenceError = createErrorClass(nil, "ReferenceError")
    SyntaxError = createErrorClass(nil, "SyntaxError")
    TypeError = createErrorClass(nil, "TypeError")
    URIError = createErrorClass(nil, "URIError")
end

local function __TS__ObjectGetOwnPropertyDescriptors(object)
    local metatable = getmetatable(object)
    if not metatable then
        return {}
    end
    return rawget(metatable, "_descriptors") or ({})
end

local function __TS__Delete(target, key)
    local descriptors = __TS__ObjectGetOwnPropertyDescriptors(target)
    local descriptor = descriptors[key]
    if descriptor then
        if not descriptor.configurable then
            error(
                __TS__New(
                    TypeError,
                    ((("Cannot delete property " .. tostring(key)) .. " of ") .. tostring(target)) .. "."
                ),
                0
            )
        end
        descriptors[key] = nil
        return true
    end
    target[key] = nil
    return true
end

local function __TS__StringAccess(self, index)
    if index >= 0 and index < #self then
        return string.sub(self, index + 1, index + 1)
    end
end

local function __TS__DelegatedYield(iterable)
    if type(iterable) == "string" then
        for index = 0, #iterable - 1 do
            coroutine.yield(__TS__StringAccess(iterable, index))
        end
    elseif iterable.____coroutine ~= nil then
        local co = iterable.____coroutine
        while true do
            local status, value = coroutine.resume(co)
            if not status then
                error(value, 0)
            end
            if coroutine.status(co) == "dead" then
                return value
            else
                coroutine.yield(value)
            end
        end
    elseif iterable[Symbol.iterator] then
        local iterator = iterable[Symbol.iterator](iterable)
        while true do
            local result = iterator:next()
            if result.done then
                return result.value
            else
                coroutine.yield(result.value)
            end
        end
    else
        for ____, value in ipairs(iterable) do
            coroutine.yield(value)
        end
    end
end

local function __TS__FunctionBind(fn, ...)
    local boundArgs = {...}
    return function(____, ...)
        local args = {...}
        __TS__ArrayUnshift(
            args,
            __TS__Unpack(boundArgs)
        )
        return fn(__TS__Unpack(args))
    end
end

local __TS__Generator
do
    local function generatorIterator(self)
        return self
    end
    local function generatorNext(self, ...)
        local co = self.____coroutine
        if coroutine.status(co) == "dead" then
            return {done = true}
        end
        local status, value = coroutine.resume(co, ...)
        if not status then
            error(value, 0)
        end
        return {
            value = value,
            done = coroutine.status(co) == "dead"
        }
    end
    function __TS__Generator(fn)
        return function(...)
            local args = {...}
            local argsLength = __TS__CountVarargs(...)
            return {
                ____coroutine = coroutine.create(function() return fn(__TS__Unpack(args, 1, argsLength)) end),
                [Symbol.iterator] = generatorIterator,
                next = generatorNext
            }
        end
    end
end

local function __TS__InstanceOfObject(value)
    local valueType = type(value)
    return valueType == "table" or valueType == "function"
end

local function __TS__LuaIteratorSpread(self, state, firstKey)
    local results = {}
    local key, value = self(state, firstKey)
    while key do
        results[#results + 1] = {key, value}
        key, value = self(state, key)
    end
    return __TS__Unpack(results)
end

local Map
do
    Map = __TS__Class()
    Map.name = "Map"
    function Map.prototype.____constructor(self, entries)
        self[Symbol.toStringTag] = "Map"
        self.items = {}
        self.size = 0
        self.nextKey = {}
        self.previousKey = {}
        if entries == nil then
            return
        end
        local iterable = entries
        if iterable[Symbol.iterator] then
            local iterator = iterable[Symbol.iterator](iterable)
            while true do
                local result = iterator:next()
                if result.done then
                    break
                end
                local value = result.value
                self:set(value[1], value[2])
            end
        else
            local array = entries
            for ____, kvp in ipairs(array) do
                self:set(kvp[1], kvp[2])
            end
        end
    end
    function Map.prototype.clear(self)
        self.items = {}
        self.nextKey = {}
        self.previousKey = {}
        self.firstKey = nil
        self.lastKey = nil
        self.size = 0
    end
    function Map.prototype.delete(self, key)
        local contains = self:has(key)
        if contains then
            self.size = self.size - 1
            local next = self.nextKey[key]
            local previous = self.previousKey[key]
            if next ~= nil and previous ~= nil then
                self.nextKey[previous] = next
                self.previousKey[next] = previous
            elseif next ~= nil then
                self.firstKey = next
                self.previousKey[next] = nil
            elseif previous ~= nil then
                self.lastKey = previous
                self.nextKey[previous] = nil
            else
                self.firstKey = nil
                self.lastKey = nil
            end
            self.nextKey[key] = nil
            self.previousKey[key] = nil
        end
        self.items[key] = nil
        return contains
    end
    function Map.prototype.forEach(self, callback)
        for ____, key in __TS__Iterator(self:keys()) do
            callback(nil, self.items[key], key, self)
        end
    end
    function Map.prototype.get(self, key)
        return self.items[key]
    end
    function Map.prototype.has(self, key)
        return self.nextKey[key] ~= nil or self.lastKey == key
    end
    function Map.prototype.set(self, key, value)
        local isNewValue = not self:has(key)
        if isNewValue then
            self.size = self.size + 1
        end
        self.items[key] = value
        if self.firstKey == nil then
            self.firstKey = key
            self.lastKey = key
        elseif isNewValue then
            self.nextKey[self.lastKey] = key
            self.previousKey[key] = self.lastKey
            self.lastKey = key
        end
        return self
    end
    Map.prototype[Symbol.iterator] = function(self)
        return self:entries()
    end
    function Map.prototype.entries(self)
        local items = self.items
        local nextKey = self.nextKey
        local key = self.firstKey
        return {
            [Symbol.iterator] = function(self)
                return self
            end,
            next = function(self)
                local result = {done = not key, value = {key, items[key]}}
                key = nextKey[key]
                return result
            end
        }
    end
    function Map.prototype.keys(self)
        local nextKey = self.nextKey
        local key = self.firstKey
        return {
            [Symbol.iterator] = function(self)
                return self
            end,
            next = function(self)
                local result = {done = not key, value = key}
                key = nextKey[key]
                return result
            end
        }
    end
    function Map.prototype.values(self)
        local items = self.items
        local nextKey = self.nextKey
        local key = self.firstKey
        return {
            [Symbol.iterator] = function(self)
                return self
            end,
            next = function(self)
                local result = {done = not key, value = items[key]}
                key = nextKey[key]
                return result
            end
        }
    end
    Map[Symbol.species] = Map
end

local function __TS__MapGroupBy(items, keySelector)
    local result = __TS__New(Map)
    local i = 0
    for ____, item in __TS__Iterator(items) do
        local key = keySelector(nil, item, i)
        if result:has(key) then
            local ____temp_0 = result:get(key)
            ____temp_0[#____temp_0 + 1] = item
        else
            result:set(key, {item})
        end
        i = i + 1
    end
    return result
end

local __TS__Match = string.match

local __TS__MathAtan2 = math.atan2 or math.atan

local __TS__MathModf = math.modf

local function __TS__NumberIsNaN(value)
    return value ~= value
end

local function __TS__MathSign(val)
    if __TS__NumberIsNaN(val) or val == 0 then
        return val
    end
    if val < 0 then
        return -1
    end
    return 1
end

local function __TS__NumberIsFinite(value)
    return type(value) == "number" and value == value and value ~= math.huge and value ~= -math.huge
end

local function __TS__MathTrunc(val)
    if not __TS__NumberIsFinite(val) or val == 0 then
        return val
    end
    return val > 0 and math.floor(val) or math.ceil(val)
end

local function __TS__Number(value)
    local valueType = type(value)
    if valueType == "number" then
        return value
    elseif valueType == "string" then
        local numberValue = tonumber(value)
        if numberValue then
            return numberValue
        end
        if value == "Infinity" then
            return math.huge
        end
        if value == "-Infinity" then
            return -math.huge
        end
        local stringWithoutSpaces = string.gsub(value, "%s", "")
        if stringWithoutSpaces == "" then
            return 0
        end
        return 0 / 0
    elseif valueType == "boolean" then
        return value and 1 or 0
    else
        return 0 / 0
    end
end

local function __TS__NumberIsInteger(value)
    return __TS__NumberIsFinite(value) and math.floor(value) == value
end

local function __TS__StringSubstring(self, start, ____end)
    if ____end ~= ____end then
        ____end = 0
    end
    if ____end ~= nil and start > ____end then
        start, ____end = ____end, start
    end
    if start >= 0 then
        start = start + 1
    else
        start = 1
    end
    if ____end ~= nil and ____end < 0 then
        ____end = 0
    end
    return string.sub(self, start, ____end)
end

local __TS__ParseInt
do
    local parseIntBasePattern = "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTvVwWxXyYzZ"
    function __TS__ParseInt(numberString, base)
        if base == nil then
            base = 10
            local hexMatch = __TS__Match(numberString, "^%s*-?0[xX]")
            if hexMatch ~= nil then
                base = 16
                numberString = (__TS__Match(hexMatch, "-")) and "-" .. __TS__StringSubstring(numberString, #hexMatch) or __TS__StringSubstring(numberString, #hexMatch)
            end
        end
        if base < 2 or base > 36 then
            return 0 / 0
        end
        local allowedDigits = base <= 10 and __TS__StringSubstring(parseIntBasePattern, 0, base) or __TS__StringSubstring(parseIntBasePattern, 0, 10 + 2 * (base - 10))
        local pattern = ("^%s*(-?[" .. allowedDigits) .. "]*)"
        local number = tonumber((__TS__Match(numberString, pattern)), base)
        if number == nil then
            return 0 / 0
        end
        if number >= 0 then
            return math.floor(number)
        else
            return math.ceil(number)
        end
    end
end

local function __TS__ParseFloat(numberString)
    local infinityMatch = __TS__Match(numberString, "^%s*(-?Infinity)")
    if infinityMatch ~= nil then
        return __TS__StringAccess(infinityMatch, 0) == "-" and -math.huge or math.huge
    end
    local number = tonumber((__TS__Match(numberString, "^%s*(-?%d+%.?%d*)")))
    return number or 0 / 0
end

local __TS__NumberToString
do
    local radixChars = "0123456789abcdefghijklmnopqrstuvwxyz"
    function __TS__NumberToString(self, radix)
        if radix == nil or radix == 10 or self == math.huge or self == -math.huge or self ~= self then
            return tostring(self)
        end
        radix = math.floor(radix)
        if radix < 2 or radix > 36 then
            error("toString() radix argument must be between 2 and 36", 0)
        end
        local integer, fraction = __TS__MathModf(math.abs(self))
        local result = ""
        if radix == 8 then
            result = string.format("%o", integer)
        elseif radix == 16 then
            result = string.format("%x", integer)
        else
            repeat
                do
                    result = __TS__StringAccess(radixChars, integer % radix) .. result
                    integer = math.floor(integer / radix)
                end
            until not (integer ~= 0)
        end
        if fraction ~= 0 then
            result = result .. "."
            local delta = 1e-16
            repeat
                do
                    fraction = fraction * radix
                    delta = delta * radix
                    local digit = math.floor(fraction)
                    result = result .. __TS__StringAccess(radixChars, digit)
                    fraction = fraction - digit
                end
            until not (fraction >= delta)
        end
        if self < 0 then
            result = "-" .. result
        end
        return result
    end
end

local function __TS__NumberToFixed(self, fractionDigits)
    if math.abs(self) >= 1e+21 or self ~= self then
        return tostring(self)
    end
    local f = math.floor(fractionDigits or 0)
    if f < 0 or f > 99 then
        error("toFixed() digits argument must be between 0 and 99", 0)
    end
    return string.format(
        ("%." .. tostring(f)) .. "f",
        self
    )
end

local function __TS__ObjectDefineProperty(target, key, desc)
    local luaKey = type(key) == "number" and key + 1 or key
    local value = rawget(target, luaKey)
    local hasGetterOrSetter = desc.get ~= nil or desc.set ~= nil
    local descriptor
    if hasGetterOrSetter then
        if value ~= nil then
            error(
                "Cannot redefine property: " .. tostring(key),
                0
            )
        end
        descriptor = desc
    else
        local valueExists = value ~= nil
        local ____desc_set_4 = desc.set
        local ____desc_get_5 = desc.get
        local ____desc_configurable_0 = desc.configurable
        if ____desc_configurable_0 == nil then
            ____desc_configurable_0 = valueExists
        end
        local ____desc_enumerable_1 = desc.enumerable
        if ____desc_enumerable_1 == nil then
            ____desc_enumerable_1 = valueExists
        end
        local ____desc_writable_2 = desc.writable
        if ____desc_writable_2 == nil then
            ____desc_writable_2 = valueExists
        end
        local ____temp_3
        if desc.value ~= nil then
            ____temp_3 = desc.value
        else
            ____temp_3 = value
        end
        descriptor = {
            set = ____desc_set_4,
            get = ____desc_get_5,
            configurable = ____desc_configurable_0,
            enumerable = ____desc_enumerable_1,
            writable = ____desc_writable_2,
            value = ____temp_3
        }
    end
    __TS__SetDescriptor(target, luaKey, descriptor)
    return target
end

local function __TS__ObjectEntries(obj)
    local result = {}
    local len = 0
    for key in pairs(obj) do
        len = len + 1
        result[len] = {key, obj[key]}
    end
    return result
end

local function __TS__ObjectFromEntries(entries)
    local obj = {}
    local iterable = entries
    if iterable[Symbol.iterator] then
        local iterator = iterable[Symbol.iterator](iterable)
        while true do
            local result = iterator:next()
            if result.done then
                break
            end
            local value = result.value
            obj[value[1]] = value[2]
        end
    else
        for ____, entry in ipairs(entries) do
            obj[entry[1]] = entry[2]
        end
    end
    return obj
end

local function __TS__ObjectGroupBy(items, keySelector)
    local result = {}
    local i = 0
    for ____, item in __TS__Iterator(items) do
        local key = keySelector(nil, item, i)
        if result[key] ~= nil then
            local ____result_key_0 = result[key]
            ____result_key_0[#____result_key_0 + 1] = item
        else
            result[key] = {item}
        end
        i = i + 1
    end
    return result
end

local function __TS__ObjectKeys(obj)
    local result = {}
    local len = 0
    for key in pairs(obj) do
        len = len + 1
        result[len] = key
    end
    return result
end

local function __TS__ObjectRest(target, usedProperties)
    local result = {}
    for property in pairs(target) do
        if not usedProperties[property] then
            result[property] = target[property]
        end
    end
    return result
end

local function __TS__ObjectValues(obj)
    local result = {}
    local len = 0
    for key in pairs(obj) do
        len = len + 1
        result[len] = obj[key]
    end
    return result
end

local function __TS__PromiseAll(iterable)
    local results = {}
    local toResolve = {}
    local numToResolve = 0
    local i = 0
    for ____, item in __TS__Iterator(iterable) do
        if __TS__InstanceOf(item, __TS__Promise) then
            if item.state == 1 then
                results[i + 1] = item.value
            elseif item.state == 2 then
                return __TS__Promise.reject(item.rejectionReason)
            else
                numToResolve = numToResolve + 1
                toResolve[i] = item
            end
        else
            results[i + 1] = item
        end
        i = i + 1
    end
    if numToResolve == 0 then
        return __TS__Promise.resolve(results)
    end
    return __TS__New(
        __TS__Promise,
        function(____, resolve, reject)
            for index, promise in pairs(toResolve) do
                promise["then"](
                    promise,
                    function(____, data)
                        results[index + 1] = data
                        numToResolve = numToResolve - 1
                        if numToResolve == 0 then
                            resolve(nil, results)
                        end
                    end,
                    function(____, reason)
                        reject(nil, reason)
                    end
                )
            end
        end
    )
end

local function __TS__PromiseAllSettled(iterable)
    local results = {}
    local toResolve = {}
    local numToResolve = 0
    local i = 0
    for ____, item in __TS__Iterator(iterable) do
        if __TS__InstanceOf(item, __TS__Promise) then
            if item.state == 1 then
                results[i + 1] = {status = "fulfilled", value = item.value}
            elseif item.state == 2 then
                results[i + 1] = {status = "rejected", reason = item.rejectionReason}
            else
                numToResolve = numToResolve + 1
                toResolve[i] = item
            end
        else
            results[i + 1] = {status = "fulfilled", value = item}
        end
        i = i + 1
    end
    if numToResolve == 0 then
        return __TS__Promise.resolve(results)
    end
    return __TS__New(
        __TS__Promise,
        function(____, resolve)
            for index, promise in pairs(toResolve) do
                promise["then"](
                    promise,
                    function(____, data)
                        results[index + 1] = {status = "fulfilled", value = data}
                        numToResolve = numToResolve - 1
                        if numToResolve == 0 then
                            resolve(nil, results)
                        end
                    end,
                    function(____, reason)
                        results[index + 1] = {status = "rejected", reason = reason}
                        numToResolve = numToResolve - 1
                        if numToResolve == 0 then
                            resolve(nil, results)
                        end
                    end
                )
            end
        end
    )
end

local function __TS__PromiseAny(iterable)
    local rejections = {}
    local pending = {}
    for ____, item in __TS__Iterator(iterable) do
        if __TS__InstanceOf(item, __TS__Promise) then
            if item.state == 1 then
                return __TS__Promise.resolve(item.value)
            elseif item.state == 2 then
                rejections[#rejections + 1] = item.rejectionReason
            else
                pending[#pending + 1] = item
            end
        else
            return __TS__Promise.resolve(item)
        end
    end
    if #pending == 0 then
        return __TS__Promise.reject("No promises to resolve with .any()")
    end
    local numResolved = 0
    return __TS__New(
        __TS__Promise,
        function(____, resolve, reject)
            for ____, promise in ipairs(pending) do
                promise["then"](
                    promise,
                    function(____, data)
                        resolve(nil, data)
                    end,
                    function(____, reason)
                        rejections[#rejections + 1] = reason
                        numResolved = numResolved + 1
                        if numResolved == #pending then
                            reject(nil, {name = "AggregateError", message = "All Promises rejected", errors = rejections})
                        end
                    end
                )
            end
        end
    )
end

local function __TS__PromiseRace(iterable)
    local pending = {}
    for ____, item in __TS__Iterator(iterable) do
        if __TS__InstanceOf(item, __TS__Promise) then
            if item.state == 1 then
                return __TS__Promise.resolve(item.value)
            elseif item.state == 2 then
                return __TS__Promise.reject(item.rejectionReason)
            else
                pending[#pending + 1] = item
            end
        else
            return __TS__Promise.resolve(item)
        end
    end
    return __TS__New(
        __TS__Promise,
        function(____, resolve, reject)
            for ____, promise in ipairs(pending) do
                promise["then"](
                    promise,
                    function(____, value) return resolve(nil, value) end,
                    function(____, reason) return reject(nil, reason) end
                )
            end
        end
    )
end

local Set
do
    Set = __TS__Class()
    Set.name = "Set"
    function Set.prototype.____constructor(self, values)
        self[Symbol.toStringTag] = "Set"
        self.size = 0
        self.nextKey = {}
        self.previousKey = {}
        if values == nil then
            return
        end
        local iterable = values
        if iterable[Symbol.iterator] then
            local iterator = iterable[Symbol.iterator](iterable)
            while true do
                local result = iterator:next()
                if result.done then
                    break
                end
                self:add(result.value)
            end
        else
            local array = values
            for ____, value in ipairs(array) do
                self:add(value)
            end
        end
    end
    function Set.prototype.add(self, value)
        local isNewValue = not self:has(value)
        if isNewValue then
            self.size = self.size + 1
        end
        if self.firstKey == nil then
            self.firstKey = value
            self.lastKey = value
        elseif isNewValue then
            self.nextKey[self.lastKey] = value
            self.previousKey[value] = self.lastKey
            self.lastKey = value
        end
        return self
    end
    function Set.prototype.clear(self)
        self.nextKey = {}
        self.previousKey = {}
        self.firstKey = nil
        self.lastKey = nil
        self.size = 0
    end
    function Set.prototype.delete(self, value)
        local contains = self:has(value)
        if contains then
            self.size = self.size - 1
            local next = self.nextKey[value]
            local previous = self.previousKey[value]
            if next ~= nil and previous ~= nil then
                self.nextKey[previous] = next
                self.previousKey[next] = previous
            elseif next ~= nil then
                self.firstKey = next
                self.previousKey[next] = nil
            elseif previous ~= nil then
                self.lastKey = previous
                self.nextKey[previous] = nil
            else
                self.firstKey = nil
                self.lastKey = nil
            end
            self.nextKey[value] = nil
            self.previousKey[value] = nil
        end
        return contains
    end
    function Set.prototype.forEach(self, callback)
        for ____, key in __TS__Iterator(self:keys()) do
            callback(nil, key, key, self)
        end
    end
    function Set.prototype.has(self, value)
        return self.nextKey[value] ~= nil or self.lastKey == value
    end
    Set.prototype[Symbol.iterator] = function(self)
        return self:values()
    end
    function Set.prototype.entries(self)
        local nextKey = self.nextKey
        local key = self.firstKey
        return {
            [Symbol.iterator] = function(self)
                return self
            end,
            next = function(self)
                local result = {done = not key, value = {key, key}}
                key = nextKey[key]
                return result
            end
        }
    end
    function Set.prototype.keys(self)
        local nextKey = self.nextKey
        local key = self.firstKey
        return {
            [Symbol.iterator] = function(self)
                return self
            end,
            next = function(self)
                local result = {done = not key, value = key}
                key = nextKey[key]
                return result
            end
        }
    end
    function Set.prototype.values(self)
        local nextKey = self.nextKey
        local key = self.firstKey
        return {
            [Symbol.iterator] = function(self)
                return self
            end,
            next = function(self)
                local result = {done = not key, value = key}
                key = nextKey[key]
                return result
            end
        }
    end
    function Set.prototype.union(self, other)
        local result = __TS__New(Set, self)
        for ____, item in __TS__Iterator(other) do
            result:add(item)
        end
        return result
    end
    function Set.prototype.intersection(self, other)
        local result = __TS__New(Set)
        for ____, item in __TS__Iterator(self) do
            if other:has(item) then
                result:add(item)
            end
        end
        return result
    end
    function Set.prototype.difference(self, other)
        local result = __TS__New(Set, self)
        for ____, item in __TS__Iterator(other) do
            result:delete(item)
        end
        return result
    end
    function Set.prototype.symmetricDifference(self, other)
        local result = __TS__New(Set, self)
        for ____, item in __TS__Iterator(other) do
            if self:has(item) then
                result:delete(item)
            else
                result:add(item)
            end
        end
        return result
    end
    function Set.prototype.isSubsetOf(self, other)
        for ____, item in __TS__Iterator(self) do
            if not other:has(item) then
                return false
            end
        end
        return true
    end
    function Set.prototype.isSupersetOf(self, other)
        for ____, item in __TS__Iterator(other) do
            if not self:has(item) then
                return false
            end
        end
        return true
    end
    function Set.prototype.isDisjointFrom(self, other)
        for ____, item in __TS__Iterator(self) do
            if other:has(item) then
                return false
            end
        end
        return true
    end
    Set[Symbol.species] = Set
end

local function __TS__SparseArrayNew(...)
    local sparseArray = {...}
    sparseArray.sparseLength = __TS__CountVarargs(...)
    return sparseArray
end

local function __TS__SparseArrayPush(sparseArray, ...)
    local args = {...}
    local argsLen = __TS__CountVarargs(...)
    local listLen = sparseArray.sparseLength
    for i = 1, argsLen do
        sparseArray[listLen + i] = args[i]
    end
    sparseArray.sparseLength = listLen + argsLen
end

local function __TS__SparseArraySpread(sparseArray)
    local _unpack = unpack or table.unpack
    return _unpack(sparseArray, 1, sparseArray.sparseLength)
end

local WeakMap
do
    WeakMap = __TS__Class()
    WeakMap.name = "WeakMap"
    function WeakMap.prototype.____constructor(self, entries)
        self[Symbol.toStringTag] = "WeakMap"
        self.items = {}
        setmetatable(self.items, {__mode = "k"})
        if entries == nil then
            return
        end
        local iterable = entries
        if iterable[Symbol.iterator] then
            local iterator = iterable[Symbol.iterator](iterable)
            while true do
                local result = iterator:next()
                if result.done then
                    break
                end
                local value = result.value
                self.items[value[1]] = value[2]
            end
        else
            for ____, kvp in ipairs(entries) do
                self.items[kvp[1]] = kvp[2]
            end
        end
    end
    function WeakMap.prototype.delete(self, key)
        local contains = self:has(key)
        self.items[key] = nil
        return contains
    end
    function WeakMap.prototype.get(self, key)
        return self.items[key]
    end
    function WeakMap.prototype.has(self, key)
        return self.items[key] ~= nil
    end
    function WeakMap.prototype.set(self, key, value)
        self.items[key] = value
        return self
    end
    WeakMap[Symbol.species] = WeakMap
end

local WeakSet
do
    WeakSet = __TS__Class()
    WeakSet.name = "WeakSet"
    function WeakSet.prototype.____constructor(self, values)
        self[Symbol.toStringTag] = "WeakSet"
        self.items = {}
        setmetatable(self.items, {__mode = "k"})
        if values == nil then
            return
        end
        local iterable = values
        if iterable[Symbol.iterator] then
            local iterator = iterable[Symbol.iterator](iterable)
            while true do
                local result = iterator:next()
                if result.done then
                    break
                end
                self.items[result.value] = true
            end
        else
            for ____, value in ipairs(values) do
                self.items[value] = true
            end
        end
    end
    function WeakSet.prototype.add(self, value)
        self.items[value] = true
        return self
    end
    function WeakSet.prototype.delete(self, value)
        local contains = self:has(value)
        self.items[value] = nil
        return contains
    end
    function WeakSet.prototype.has(self, value)
        return self.items[value] == true
    end
    WeakSet[Symbol.species] = WeakSet
end

local function __TS__SourceMapTraceBack(fileName, sourceMap)
    _G.__TS__sourcemap = _G.__TS__sourcemap or ({})
    _G.__TS__sourcemap[fileName] = sourceMap
    if _G.__TS__originalTraceback == nil then
        local originalTraceback = debug.traceback
        _G.__TS__originalTraceback = originalTraceback
        debug.traceback = function(thread, message, level)
            local trace
            if thread == nil and message == nil and level == nil then
                trace = originalTraceback()
            elseif __TS__StringIncludes(_VERSION, "Lua 5.0") then
                trace = originalTraceback((("[Level " .. tostring(level)) .. "] ") .. tostring(message))
            else
                trace = originalTraceback(thread, message, level)
            end
            if type(trace) ~= "string" then
                return trace
            end
            local function replacer(____, file, srcFile, line)
                local fileSourceMap = _G.__TS__sourcemap[file]
                if fileSourceMap ~= nil and fileSourceMap[line] ~= nil then
                    local data = fileSourceMap[line]
                    if type(data) == "number" then
                        return (srcFile .. ":") .. tostring(data)
                    end
                    return (data.file .. ":") .. tostring(data.line)
                end
                return (file .. ":") .. line
            end
            local result = string.gsub(
                trace,
                "(%S+)%.lua:(%d+)",
                function(file, line) return replacer(nil, file .. ".lua", file .. ".ts", line) end
            )
            local function stringReplacer(____, file, line)
                local fileSourceMap = _G.__TS__sourcemap[file]
                if fileSourceMap ~= nil and fileSourceMap[line] ~= nil then
                    local chunkName = (__TS__Match(file, "%[string \"([^\"]+)\"%]"))
                    local sourceName = string.gsub(chunkName, ".lua$", ".ts")
                    local data = fileSourceMap[line]
                    if type(data) == "number" then
                        return (sourceName .. ":") .. tostring(data)
                    end
                    return (data.file .. ":") .. tostring(data.line)
                end
                return (file .. ":") .. line
            end
            result = string.gsub(
                result,
                "(%[string \"[^\"]+\"%]):(%d+)",
                function(file, line) return stringReplacer(nil, file, line) end
            )
            return result
        end
    end
end

local function __TS__Spread(iterable)
    local arr = {}
    if type(iterable) == "string" then
        for i = 0, #iterable - 1 do
            arr[i + 1] = __TS__StringAccess(iterable, i)
        end
    else
        local len = 0
        for ____, item in __TS__Iterator(iterable) do
            len = len + 1
            arr[len] = item
        end
    end
    return __TS__Unpack(arr)
end

local function __TS__StringCharAt(self, pos)
    if pos ~= pos then
        pos = 0
    end
    if pos < 0 then
        return ""
    end
    return string.sub(self, pos + 1, pos + 1)
end

local function __TS__StringCharCodeAt(self, index)
    if index ~= index then
        index = 0
    end
    if index < 0 then
        return 0 / 0
    end
    return string.byte(self, index + 1) or 0 / 0
end

local function __TS__StringEndsWith(self, searchString, endPosition)
    if endPosition == nil or endPosition > #self then
        endPosition = #self
    end
    return string.sub(self, endPosition - #searchString + 1, endPosition) == searchString
end

local function __TS__StringPadEnd(self, maxLength, fillString)
    if fillString == nil then
        fillString = " "
    end
    if maxLength ~= maxLength then
        maxLength = 0
    end
    if maxLength == -math.huge or maxLength == math.huge then
        error("Invalid string length", 0)
    end
    if #self >= maxLength or #fillString == 0 then
        return self
    end
    maxLength = maxLength - #self
    if maxLength > #fillString then
        fillString = fillString .. string.rep(
            fillString,
            math.floor(maxLength / #fillString)
        )
    end
    return self .. string.sub(
        fillString,
        1,
        math.floor(maxLength)
    )
end

local function __TS__StringPadStart(self, maxLength, fillString)
    if fillString == nil then
        fillString = " "
    end
    if maxLength ~= maxLength then
        maxLength = 0
    end
    if maxLength == -math.huge or maxLength == math.huge then
        error("Invalid string length", 0)
    end
    if #self >= maxLength or #fillString == 0 then
        return self
    end
    maxLength = maxLength - #self
    if maxLength > #fillString then
        fillString = fillString .. string.rep(
            fillString,
            math.floor(maxLength / #fillString)
        )
    end
    return string.sub(
        fillString,
        1,
        math.floor(maxLength)
    ) .. self
end

local __TS__StringReplace
do
    local sub = string.sub
    function __TS__StringReplace(source, searchValue, replaceValue)
        local startPos, endPos = string.find(source, searchValue, nil, true)
        if not startPos then
            return source
        end
        local before = sub(source, 1, startPos - 1)
        local replacement = type(replaceValue) == "string" and replaceValue or replaceValue(nil, searchValue, startPos - 1, source)
        local after = sub(source, endPos + 1)
        return (before .. replacement) .. after
    end
end

local __TS__StringSplit
do
    local sub = string.sub
    local find = string.find
    function __TS__StringSplit(source, separator, limit)
        if limit == nil then
            limit = 4294967295
        end
        if limit == 0 then
            return {}
        end
        local result = {}
        local resultIndex = 1
        if separator == nil or separator == "" then
            for i = 1, #source do
                result[resultIndex] = sub(source, i, i)
                resultIndex = resultIndex + 1
            end
        else
            local currentPos = 1
            while resultIndex <= limit do
                local startPos, endPos = find(source, separator, currentPos, true)
                if not startPos then
                    break
                end
                result[resultIndex] = sub(source, currentPos, startPos - 1)
                resultIndex = resultIndex + 1
                currentPos = endPos + 1
            end
            if resultIndex <= limit then
                result[resultIndex] = sub(source, currentPos)
            end
        end
        return result
    end
end

local __TS__StringReplaceAll
do
    local sub = string.sub
    local find = string.find
    function __TS__StringReplaceAll(source, searchValue, replaceValue)
        if type(replaceValue) == "string" then
            local concat = table.concat(
                __TS__StringSplit(source, searchValue),
                replaceValue
            )
            if #searchValue == 0 then
                return (replaceValue .. concat) .. replaceValue
            end
            return concat
        end
        local parts = {}
        local partsIndex = 1
        if #searchValue == 0 then
            parts[1] = replaceValue(nil, "", 0, source)
            partsIndex = 2
            for i = 1, #source do
                parts[partsIndex] = sub(source, i, i)
                parts[partsIndex + 1] = replaceValue(nil, "", i, source)
                partsIndex = partsIndex + 2
            end
        else
            local currentPos = 1
            while true do
                local startPos, endPos = find(source, searchValue, currentPos, true)
                if not startPos then
                    break
                end
                parts[partsIndex] = sub(source, currentPos, startPos - 1)
                parts[partsIndex + 1] = replaceValue(nil, searchValue, startPos - 1, source)
                partsIndex = partsIndex + 2
                currentPos = endPos + 1
            end
            parts[partsIndex] = sub(source, currentPos)
        end
        return table.concat(parts)
    end
end

local function __TS__StringSlice(self, start, ____end)
    if start == nil or start ~= start then
        start = 0
    end
    if ____end ~= ____end then
        ____end = 0
    end
    if start >= 0 then
        start = start + 1
    end
    if ____end ~= nil and ____end < 0 then
        ____end = ____end - 1
    end
    return string.sub(self, start, ____end)
end

local function __TS__StringStartsWith(self, searchString, position)
    if position == nil or position < 0 then
        position = 0
    end
    return string.sub(self, position + 1, #searchString + position) == searchString
end

local function __TS__StringSubstr(self, from, length)
    if from ~= from then
        from = 0
    end
    if length ~= nil then
        if length ~= length or length <= 0 then
            return ""
        end
        length = length + from
    end
    if from >= 0 then
        from = from + 1
    end
    return string.sub(self, from, length)
end

local function __TS__StringTrim(self)
    local result = string.gsub(self, "^[%s ﻿]*(.-)[%s ﻿]*$", "%1")
    return result
end

local function __TS__StringTrimEnd(self)
    local result = string.gsub(self, "[%s ﻿]*$", "")
    return result
end

local function __TS__StringTrimStart(self)
    local result = string.gsub(self, "^[%s ﻿]*", "")
    return result
end

local __TS__SymbolRegistryFor, __TS__SymbolRegistryKeyFor
do
    local symbolRegistry = {}
    function __TS__SymbolRegistryFor(key)
        if not symbolRegistry[key] then
            symbolRegistry[key] = __TS__Symbol(key)
        end
        return symbolRegistry[key]
    end
    function __TS__SymbolRegistryKeyFor(sym)
        for key in pairs(symbolRegistry) do
            if symbolRegistry[key] == sym then
                return key
            end
        end
        return nil
    end
end

local function __TS__TypeOf(value)
    local luaType = type(value)
    if luaType == "table" then
        return "object"
    elseif luaType == "nil" then
        return "undefined"
    else
        return luaType
    end
end

local function __TS__Using(self, cb, ...)
    local args = {...}
    local thrownError
    local ok, result = xpcall(
        function() return cb(__TS__Unpack(args)) end,
        function(err)
            thrownError = err
            return thrownError
        end
    )
    local argArray = {__TS__Unpack(args)}
    do
        local i = #argArray - 1
        while i >= 0 do
            local ____self_0 = argArray[i + 1]
            ____self_0[Symbol.dispose](____self_0)
            i = i - 1
        end
    end
    if not ok then
        error(thrownError, 0)
    end
    return result
end

local function __TS__UsingAsync(self, cb, ...)
    local args = {...}
    return __TS__AsyncAwaiter(function(____awaiter_resolve)
        local thrownError
        local ok, result = xpcall(
            function() return cb(
                nil,
                __TS__Unpack(args)
            ) end,
            function(err)
                thrownError = err
                return thrownError
            end
        )
        local argArray = {__TS__Unpack(args)}
        do
            local i = #argArray - 1
            while i >= 0 do
                if argArray[i + 1][Symbol.dispose] ~= nil then
                    local ____self_0 = argArray[i + 1]
                    ____self_0[Symbol.dispose](____self_0)
                end
                if argArray[i + 1][Symbol.asyncDispose] ~= nil then
                    local ____self_1 = argArray[i + 1]
                    __TS__Await(____self_1[Symbol.asyncDispose](____self_1))
                end
                i = i - 1
            end
        end
        if not ok then
            error(thrownError, 0)
        end
        return ____awaiter_resolve(nil, result)
    end)
end

return {
  __TS__ArrayAt = __TS__ArrayAt,
  __TS__ArrayConcat = __TS__ArrayConcat,
  __TS__ArrayEntries = __TS__ArrayEntries,
  __TS__ArrayEvery = __TS__ArrayEvery,
  __TS__ArrayFill = __TS__ArrayFill,
  __TS__ArrayFilter = __TS__ArrayFilter,
  __TS__ArrayForEach = __TS__ArrayForEach,
  __TS__ArrayFind = __TS__ArrayFind,
  __TS__ArrayFindIndex = __TS__ArrayFindIndex,
  __TS__ArrayFrom = __TS__ArrayFrom,
  __TS__ArrayIncludes = __TS__ArrayIncludes,
  __TS__ArrayIndexOf = __TS__ArrayIndexOf,
  __TS__ArrayIsArray = __TS__ArrayIsArray,
  __TS__ArrayJoin = __TS__ArrayJoin,
  __TS__ArrayMap = __TS__ArrayMap,
  __TS__ArrayPush = __TS__ArrayPush,
  __TS__ArrayPushArray = __TS__ArrayPushArray,
  __TS__ArrayReduce = __TS__ArrayReduce,
  __TS__ArrayReduceRight = __TS__ArrayReduceRight,
  __TS__ArrayReverse = __TS__ArrayReverse,
  __TS__ArrayUnshift = __TS__ArrayUnshift,
  __TS__ArraySort = __TS__ArraySort,
  __TS__ArraySlice = __TS__ArraySlice,
  __TS__ArraySome = __TS__ArraySome,
  __TS__ArraySplice = __TS__ArraySplice,
  __TS__ArrayToObject = __TS__ArrayToObject,
  __TS__ArrayFlat = __TS__ArrayFlat,
  __TS__ArrayFlatMap = __TS__ArrayFlatMap,
  __TS__ArraySetLength = __TS__ArraySetLength,
  __TS__ArrayToReversed = __TS__ArrayToReversed,
  __TS__ArrayToSorted = __TS__ArrayToSorted,
  __TS__ArrayToSpliced = __TS__ArrayToSpliced,
  __TS__ArrayWith = __TS__ArrayWith,
  __TS__AsyncAwaiter = __TS__AsyncAwaiter,
  __TS__Await = __TS__Await,
  __TS__Class = __TS__Class,
  __TS__ClassExtends = __TS__ClassExtends,
  __TS__CloneDescriptor = __TS__CloneDescriptor,
  __TS__CountVarargs = __TS__CountVarargs,
  __TS__Decorate = __TS__Decorate,
  __TS__DecorateLegacy = __TS__DecorateLegacy,
  __TS__DecorateParam = __TS__DecorateParam,
  __TS__Delete = __TS__Delete,
  __TS__DelegatedYield = __TS__DelegatedYield,
  __TS__DescriptorGet = __TS__DescriptorGet,
  __TS__DescriptorSet = __TS__DescriptorSet,
  Error = Error,
  RangeError = RangeError,
  ReferenceError = ReferenceError,
  SyntaxError = SyntaxError,
  TypeError = TypeError,
  URIError = URIError,
  __TS__FunctionBind = __TS__FunctionBind,
  __TS__Generator = __TS__Generator,
  __TS__InstanceOf = __TS__InstanceOf,
  __TS__InstanceOfObject = __TS__InstanceOfObject,
  __TS__Iterator = __TS__Iterator,
  __TS__LuaIteratorSpread = __TS__LuaIteratorSpread,
  Map = Map,
  __TS__MapGroupBy = __TS__MapGroupBy,
  __TS__Match = __TS__Match,
  __TS__MathAtan2 = __TS__MathAtan2,
  __TS__MathModf = __TS__MathModf,
  __TS__MathSign = __TS__MathSign,
  __TS__MathTrunc = __TS__MathTrunc,
  __TS__New = __TS__New,
  __TS__Number = __TS__Number,
  __TS__NumberIsFinite = __TS__NumberIsFinite,
  __TS__NumberIsInteger = __TS__NumberIsInteger,
  __TS__NumberIsNaN = __TS__NumberIsNaN,
  __TS__ParseInt = __TS__ParseInt,
  __TS__ParseFloat = __TS__ParseFloat,
  __TS__NumberToString = __TS__NumberToString,
  __TS__NumberToFixed = __TS__NumberToFixed,
  __TS__ObjectAssign = __TS__ObjectAssign,
  __TS__ObjectDefineProperty = __TS__ObjectDefineProperty,
  __TS__ObjectEntries = __TS__ObjectEntries,
  __TS__ObjectFromEntries = __TS__ObjectFromEntries,
  __TS__ObjectGetOwnPropertyDescriptor = __TS__ObjectGetOwnPropertyDescriptor,
  __TS__ObjectGetOwnPropertyDescriptors = __TS__ObjectGetOwnPropertyDescriptors,
  __TS__ObjectGroupBy = __TS__ObjectGroupBy,
  __TS__ObjectKeys = __TS__ObjectKeys,
  __TS__ObjectRest = __TS__ObjectRest,
  __TS__ObjectValues = __TS__ObjectValues,
  __TS__ParseFloat = __TS__ParseFloat,
  __TS__ParseInt = __TS__ParseInt,
  __TS__Promise = __TS__Promise,
  __TS__PromiseAll = __TS__PromiseAll,
  __TS__PromiseAllSettled = __TS__PromiseAllSettled,
  __TS__PromiseAny = __TS__PromiseAny,
  __TS__PromiseRace = __TS__PromiseRace,
  Set = Set,
  __TS__SetDescriptor = __TS__SetDescriptor,
  __TS__SparseArrayNew = __TS__SparseArrayNew,
  __TS__SparseArrayPush = __TS__SparseArrayPush,
  __TS__SparseArraySpread = __TS__SparseArraySpread,
  WeakMap = WeakMap,
  WeakSet = WeakSet,
  __TS__SourceMapTraceBack = __TS__SourceMapTraceBack,
  __TS__Spread = __TS__Spread,
  __TS__StringAccess = __TS__StringAccess,
  __TS__StringCharAt = __TS__StringCharAt,
  __TS__StringCharCodeAt = __TS__StringCharCodeAt,
  __TS__StringEndsWith = __TS__StringEndsWith,
  __TS__StringIncludes = __TS__StringIncludes,
  __TS__StringPadEnd = __TS__StringPadEnd,
  __TS__StringPadStart = __TS__StringPadStart,
  __TS__StringReplace = __TS__StringReplace,
  __TS__StringReplaceAll = __TS__StringReplaceAll,
  __TS__StringSlice = __TS__StringSlice,
  __TS__StringSplit = __TS__StringSplit,
  __TS__StringStartsWith = __TS__StringStartsWith,
  __TS__StringSubstr = __TS__StringSubstr,
  __TS__StringSubstring = __TS__StringSubstring,
  __TS__StringTrim = __TS__StringTrim,
  __TS__StringTrimEnd = __TS__StringTrimEnd,
  __TS__StringTrimStart = __TS__StringTrimStart,
  __TS__Symbol = __TS__Symbol,
  Symbol = Symbol,
  __TS__SymbolRegistryFor = __TS__SymbolRegistryFor,
  __TS__SymbolRegistryKeyFor = __TS__SymbolRegistryKeyFor,
  __TS__TypeOf = __TS__TypeOf,
  __TS__Unpack = __TS__Unpack,
  __TS__Using = __TS__Using,
  __TS__UsingAsync = __TS__UsingAsync
}
 end,
["Fs"] = function(...) 
local ____lualib = require("lualib_bundle")
local Error = ____lualib.Error
local RangeError = ____lualib.RangeError
local ReferenceError = ____lualib.ReferenceError
local SyntaxError = ____lualib.SyntaxError
local TypeError = ____lualib.TypeError
local URIError = ____lualib.URIError
local __TS__New = ____lualib.__TS__New
local ____exports = {}
local ____tstl_2Dresult = require("lua_modules.@codethread.tstl-result.dist.index")
local ok = ____tstl_2Dresult.ok
local err = ____tstl_2Dresult.err
local fileCache = {}
____exports.Fs = {readFile = function(self, file, opts)
    if opts == nil then
        opts = {}
    end
    local inCache = fileCache[file]
    if not opts.noCache and inCache then
        return ok(inCache)
    end
    local f = io.open(file, "rb")
    if not f then
        return err(file)
    end
    local content = f:read("*all")
    if type(content) ~= "string" then
        error(
            __TS__New(Error, "ah"),
            0
        )
    end
    f:close()
    fileCache[file] = content
    return ok(content)
end}
return ____exports
 end,
["lua_modules.@codethread.tstl-result.dist.index"] = function(...) 
local ____lualib = require("lualib_bundle")
local Error = ____lualib.Error
local RangeError = ____lualib.RangeError
local ReferenceError = ____lualib.ReferenceError
local SyntaxError = ____lualib.SyntaxError
local TypeError = ____lualib.TypeError
local URIError = ____lualib.URIError
local __TS__New = ____lualib.__TS__New
local __TS__Unpack = ____lualib.__TS__Unpack
local __TS__SparseArrayNew = ____lualib.__TS__SparseArrayNew
local __TS__SparseArrayPush = ____lualib.__TS__SparseArrayPush
local __TS__SparseArraySpread = ____lualib.__TS__SparseArraySpread
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local __TS__PromiseAll = ____lualib.__TS__PromiseAll
local __TS__Spread = ____lualib.__TS__Spread
local __TS__Class = ____lualib.__TS__Class
local __TS__Promise = ____lualib.__TS__Promise
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local __TS__InstanceOf = ____lualib.__TS__InstanceOf
local ____exports = {}
local defaultErrorConfig = {withStackTrace = false}
local function createNeverThrowError(message, result, config)
    if config == nil then
        config = defaultErrorConfig
    end
    local data = result:isOk() and ({type = "Ok", value = result.value}) or ({type = "Err", value = result.error})
    local ____config_withStackTrace_0
    if config.withStackTrace then
        ____config_withStackTrace_0 = __TS__New(Error).stack
    else
        ____config_withStackTrace_0 = nil
    end
    local maybeStack = ____config_withStackTrace_0
    return {data = data, message = message, stack = maybeStack}
end
local function appendValueToEndOfList(value)
    return function(list)
        local ____array_1 = __TS__SparseArrayNew(__TS__Unpack(list))
        __TS__SparseArrayPush(____array_1, value)
        return {__TS__SparseArraySpread(____array_1)}
    end
end
--- Short circuits on the FIRST Err value that we find
local function combineResultList(resultList)
    return __TS__ArrayReduce(
        resultList,
        function(____, acc, result) return acc:isOk() and (result:isErr() and ____exports.err(result.error) or acc:map(appendValueToEndOfList(result.value))) or acc end,
        ____exports.ok({})
    )
end
local function combineResultAsyncList(asyncResultList)
    return ____exports.ResultAsync:fromSafePromise(__TS__PromiseAll(asyncResultList)):andThen(combineResultList)
end
--- Give a list of all the errors we find
local function combineResultListWithAllErrors(resultList)
    return __TS__ArrayReduce(
        resultList,
        function(____, acc, result)
            local ____result_isErr_result_6
            if result:isErr() then
                local ____acc_isErr_result_3
                if acc:isErr() then
                    local ____array_2 = __TS__SparseArrayNew(__TS__Unpack(acc.error))
                    __TS__SparseArrayPush(____array_2, result.error)
                    ____acc_isErr_result_3 = ____exports.err({__TS__SparseArraySpread(____array_2)})
                else
                    ____acc_isErr_result_3 = ____exports.err({result.error})
                end
                ____result_isErr_result_6 = ____acc_isErr_result_3
            else
                local ____acc_isErr_result_5
                if acc:isErr() then
                    ____acc_isErr_result_5 = acc
                else
                    local ____array_4 = __TS__SparseArrayNew(__TS__Unpack(acc.value))
                    __TS__SparseArrayPush(____array_4, result.value)
                    ____acc_isErr_result_5 = ____exports.ok({__TS__SparseArraySpread(____array_4)})
                end
                ____result_isErr_result_6 = ____acc_isErr_result_5
            end
            return ____result_isErr_result_6
        end,
        ____exports.ok({})
    )
end
local function combineResultAsyncListWithAllErrors(asyncResultList)
    return ____exports.ResultAsync:fromSafePromise(__TS__PromiseAll(asyncResultList)):andThen(combineResultListWithAllErrors)
end
--- Wraps a function with a try catch, creating a new function with the same
-- arguments but returning `Ok` if successful, `Err` if the function throws
-- 
-- @param fn function to wrap with ok on success or err on failure
-- @param errorFn when an error is thrown, this will wrap the error result if provided
function ____exports.fromThrowable(fn, errorFn)
    return function(...)
        local args = {...}
        do
            local function ____catch(e)
                local ____errorFn_7
                if errorFn then
                    ____errorFn_7 = errorFn(e)
                else
                    ____errorFn_7 = e
                end
                return true, ____exports.err(____errorFn_7)
            end
            local ____try, ____hasReturned, ____returnValue = pcall(function()
                local result = fn(__TS__Spread(args))
                return true, ____exports.ok(result)
            end)
            if not ____try then
                ____hasReturned, ____returnValue = ____catch(____hasReturned)
            end
            if ____hasReturned then
                return ____returnValue
            end
        end
    end
end
function ____exports.combine(resultList)
    return combineResultList(resultList)
end
function ____exports.combineWithAllErrors(resultList)
    return combineResultListWithAllErrors(resultList)
end
____exports.ok = function(value) return __TS__New(____exports.Ok, value) end
____exports.err = function(err) return __TS__New(____exports.Err, err) end
____exports.Ok = __TS__Class()
local Ok = ____exports.Ok
Ok.name = "Ok"
function Ok.prototype.____constructor(self, value)
    self.value = value
end
function Ok.prototype.isOk(self)
    return true
end
function Ok.prototype.isErr(self)
    return not self:isOk()
end
function Ok.prototype.map(self, f)
    return ____exports.ok(f(self.value))
end
function Ok.prototype.mapErr(self, _f)
    return ____exports.ok(self.value)
end
function Ok.prototype.andThen(self, f)
    return f(self.value)
end
function Ok.prototype.orElse(self, _f)
    return ____exports.ok(self.value)
end
function Ok.prototype.asyncAndThen(self, f)
    return f(self.value)
end
function Ok.prototype.asyncMap(self, f)
    return ____exports.ResultAsync:fromSafePromise(f(self.value))
end
function Ok.prototype.unwrapOr(self, _v)
    return self.value
end
function Ok.prototype.match(self, ok, _err)
    return ok(self.value)
end
function Ok.prototype._unsafeUnwrap(self, _)
    return self.value
end
function Ok.prototype._unsafeUnwrapErr(self, config)
    error(
        createNeverThrowError("Called `_unsafeUnwrapErr` on an Ok", self, config),
        0
    )
end
____exports.Err = __TS__Class()
local Err = ____exports.Err
Err.name = "Err"
function Err.prototype.____constructor(self, ____error)
    self.error = ____error
end
function Err.prototype.isOk(self)
    return false
end
function Err.prototype.isErr(self)
    return not self:isOk()
end
function Err.prototype.map(self, _f)
    return ____exports.err(self.error)
end
function Err.prototype.mapErr(self, f)
    return ____exports.err(f(self.error))
end
function Err.prototype.andThen(self, _f)
    return ____exports.err(self.error)
end
function Err.prototype.orElse(self, f)
    return f(self.error)
end
function Err.prototype.asyncAndThen(self, _f)
    return ____exports.errAsync(self.error)
end
function Err.prototype.asyncMap(self, _f)
    return ____exports.errAsync(self.error)
end
function Err.prototype.unwrapOr(self, v)
    return v
end
function Err.prototype.match(self, _ok, err)
    return err(self.error)
end
function Err.prototype._unsafeUnwrap(self, config)
    error(
        createNeverThrowError("Called `_unsafeUnwrap` on an Err", self, config),
        0
    )
end
function Err.prototype._unsafeUnwrapErr(self, _)
    return self.error
end
____exports.ResultAsync = __TS__Class()
local ResultAsync = ____exports.ResultAsync
ResultAsync.name = "ResultAsync"
function ResultAsync.prototype.____constructor(self, res)
    self._promise = res
end
function ResultAsync.fromSafePromise(self, promise)
    local newPromise = promise["then"](
        promise,
        function(____, value) return __TS__New(____exports.Ok, value) end
    )
    return __TS__New(____exports.ResultAsync, newPromise)
end
function ResultAsync.fromPromise(self, promise, errorFn)
    local newPromise = promise["then"](
        promise,
        function(____, value) return __TS__New(____exports.Ok, value) end
    ):catch(function(____, e) return __TS__New(
        ____exports.Err,
        errorFn(e)
    ) end)
    return __TS__New(____exports.ResultAsync, newPromise)
end
function ResultAsync.combine(self, asyncResultList)
    return combineResultAsyncList(asyncResultList)
end
function ResultAsync.combineWithAllErrors(self, asyncResultList)
    return combineResultAsyncListWithAllErrors(asyncResultList)
end
function ResultAsync.prototype.map(self, f)
    local ____exports_ResultAsync_9 = ____exports.ResultAsync
    local ____self_8 = self._promise
    return __TS__New(
        ____exports_ResultAsync_9,
        ____self_8["then"](
            ____self_8,
            function(____, res)
                return __TS__AsyncAwaiter(function(____awaiter_resolve)
                    if res:isErr() then
                        return ____awaiter_resolve(
                            nil,
                            __TS__New(____exports.Err, res.error)
                        )
                    end
                    return ____awaiter_resolve(
                        nil,
                        __TS__New(
                            ____exports.Ok,
                            __TS__Await(f(res.value))
                        )
                    )
                end)
            end
        )
    )
end
function ResultAsync.prototype.mapErr(self, f)
    local ____exports_ResultAsync_11 = ____exports.ResultAsync
    local ____self_10 = self._promise
    return __TS__New(
        ____exports_ResultAsync_11,
        ____self_10["then"](
            ____self_10,
            function(____, res)
                return __TS__AsyncAwaiter(function(____awaiter_resolve)
                    if res:isOk() then
                        return ____awaiter_resolve(
                            nil,
                            __TS__New(____exports.Ok, res.value)
                        )
                    end
                    return ____awaiter_resolve(
                        nil,
                        __TS__New(
                            ____exports.Err,
                            __TS__Await(f(res.error))
                        )
                    )
                end)
            end
        )
    )
end
function ResultAsync.prototype.andThen(self, f)
    local ____exports_ResultAsync_13 = ____exports.ResultAsync
    local ____self_12 = self._promise
    return __TS__New(
        ____exports_ResultAsync_13,
        ____self_12["then"](
            ____self_12,
            function(____, res)
                if res:isErr() then
                    return __TS__New(____exports.Err, res.error)
                end
                local newValue = f(res.value)
                return __TS__InstanceOf(newValue, ____exports.ResultAsync) and newValue._promise or newValue
            end
        )
    )
end
function ResultAsync.prototype.orElse(self, f)
    local ____exports_ResultAsync_15 = ____exports.ResultAsync
    local ____self_14 = self._promise
    return __TS__New(
        ____exports_ResultAsync_15,
        ____self_14["then"](
            ____self_14,
            function(____, res)
                return __TS__AsyncAwaiter(function(____awaiter_resolve)
                    if res:isErr() then
                        return ____awaiter_resolve(
                            nil,
                            f(res.error)
                        )
                    end
                    return ____awaiter_resolve(
                        nil,
                        __TS__New(____exports.Ok, res.value)
                    )
                end)
            end
        )
    )
end
function ResultAsync.prototype.match(self, ok, _err)
    local ____self_16 = self._promise
    return ____self_16["then"](
        ____self_16,
        function(____, res) return res:match(ok, _err) end
    )
end
function ResultAsync.prototype.unwrapOr(self, t)
    local ____self_17 = self._promise
    return ____self_17["then"](
        ____self_17,
        function(____, res) return res:unwrapOr(t) end
    )
end
ResultAsync.prototype["then"] = function(self, successCallback, failureCallback)
    local ____self_18 = self._promise
    return ____self_18["then"](____self_18, successCallback, failureCallback)
end
____exports.okAsync = function(value) return __TS__New(
    ____exports.ResultAsync,
    __TS__Promise.resolve(__TS__New(____exports.Ok, value))
) end
____exports.errAsync = function(err) return __TS__New(
    ____exports.ResultAsync,
    __TS__Promise.resolve(__TS__New(____exports.Err, err))
) end
____exports.fromPromise = ____exports.ResultAsync.fromPromise
____exports.fromSafePromise = ____exports.ResultAsync.fromSafePromise
return ____exports
 end,
["settings"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__StringReplace = ____lualib.__TS__StringReplace
local ____exports = {}
local v = require("lua_modules.@codethread.tstl-validate.dist.index")
local wezterm = require("wezterm")
local ____tstl_2Dresult = require("lua_modules.@codethread.tstl-result.dist.index")
local ok = ____tstl_2Dresult.ok
local ____Fs = require("Fs")
local Fs = ____Fs.Fs
____exports.Settings = {
    getEnvs = function(self)
        return Fs:readFile(wezterm.home_dir .. "/.config/envy/envs.json"):map(function(f) return __TS__StringReplace(f, "%s+", "") end):map(wezterm.json_parse):andThen(function(tbl) return v.safeParse(
            v.record(
                v.string(),
                v.string()
            ),
            tbl
        ) end)
    end,
    applyToConfig = function(self, config)
        config.set_environment_variables = self:getEnvs():orElse(function(e)
            if type(e) ~= "string" then
                wezterm.log_warn(v.flatten(e))
            else
                wezterm.log_warn("missing: " .. e)
            end
            return ok({})
        end):_unsafeUnwrap()
        config.default_prog = {"nu", "-l"}
        config.exit_behavior = "CloseOnCleanExit"
        config.font_size = 14
    end
}
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.error.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.custom.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.endsWith.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.notEndsWith.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.equal.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.finite.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.includes.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.maxLength.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.maxValue.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.minLength.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.minValue.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.multipleOf.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.startsWith.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.notStartsWith.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.value.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.matches.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.notMatches.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.notMatches.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates a pattern does not match against a string
-- 
-- @param requirement The pattern to be matched.
-- @param error The error message.
-- @returns A validation function.
function ____exports.notMatches(requirement, ____error)
    return function(input, info)
        local matches = {string.find(input, requirement)}
        if matches[1] ~= nil then
            return {issues = {getIssue(info, {validation = "matches", message = ____error or ("Invalid content, pattern \"" .. requirement) .. "\" should not match", input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.executePipe.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.getErrorAndPipe.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.getIssue.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.getPath.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.getPathInfo.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.getPipeInfo.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.getPipeInfo.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.getPipeInfo.getPipeInfo")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.getPipeInfo.getPipeInfo"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Returns the pipe info.
-- 
-- @param info The parse info.
-- @param reason The issue reason.
-- @returns The pipe info.
function ____exports.getPipeInfo(info, reason)
    return {
        origin = info and info.origin,
        path = info and info.path,
        abortEarly = info and info.abortEarly,
        abortPipeEarly = info and info.abortPipeEarly,
        reason = reason
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.getPathInfo.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.getPathInfo.getPathInfo")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.getPathInfo.getPathInfo"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Returns the parse info of a path.
-- 
-- @param info The parse info.
-- @param pathItem The path item.
-- @returns The parse info.
function ____exports.getPathInfo(info, path, origin)
    if origin == nil then
        origin = "value"
    end
    return {origin = origin, path = path, abortEarly = info and info.abortEarly, abortPipeEarly = info and info.abortPipeEarly}
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.getPath.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.getPath.getPath")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.getPath.getPath"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Returns the current path.
-- 
-- @param info The parse info.
-- @param key The current key.
-- @returns The current path.
function ____exports.getPath(prevPath, pathItem)
    local path = {}
    if prevPath then
        for ____, pathItem in ipairs(prevPath) do
            path[#path + 1] = pathItem
        end
    end
    path[#path + 1] = pathItem
    return path
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.getIssue.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.getIssue.getIssue")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.getIssue.getIssue"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Returns the final issue data.
-- 
-- @param info The parse info.
-- @param issue The issue data.
-- @returns The issue data.
-- @param info The validate info.
-- @param issue The issue data.
-- @returns The issue data.
function ____exports.getIssue(info, issue)
    return {
        reason = info and info.reason or issue.reason,
        validation = issue.validation,
        origin = info and info.origin or "value",
        message = issue.message,
        input = issue.input,
        path = info and info.path,
        issues = issue.issues,
        abortEarly = info and info.abortEarly,
        abortPipeEarly = info and info.abortPipeEarly
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.getErrorAndPipe.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.getErrorAndPipe.getErrorAndPipe")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.getErrorAndPipe.getErrorAndPipe"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Returns error and pipe from dynamic arguments.
-- 
-- @param arg1 First argument.
-- @param arg2 Second argument.
-- @returns The error and pipe.
function ____exports.getErrorAndPipe(arg1, arg2)
    if not arg1 or type(arg1) == "string" then
        local pipe = arg2 or ({})
        return {error = arg1, pipe = pipe}
    else
        local pipe = arg1 or ({})
        return {error = nil, pipe = pipe}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.executePipe.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.executePipe.executePipe")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.utils.executePipe.executePipeAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.executePipe.executePipeAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
--- Executes the async validation and transformation pipe.
-- 
-- @param input The input value.
-- @param pipe The pipe to be executed.
-- @param info The validation info.
-- @returns The output value.
function ____exports.executePipeAsync(input, pipe, info)
    return __TS__AsyncAwaiter(function(____awaiter_resolve)
        local output = input
        local issues
        for ____, action in ipairs(pipe) do
            local result = __TS__Await(action(output, info))
            if result.issues then
                if issues then
                    for ____, issue in ipairs(result.issues) do
                        issues[#issues + 1] = issue
                    end
                else
                    issues = result.issues
                end
                if info.abortEarly or info.abortPipeEarly then
                    break
                end
            else
                output = result.output
            end
        end
        return ____awaiter_resolve(nil, issues and ({issues = issues}) or ({output = output}))
    end)
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.utils.executePipe.executePipe"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Executes the validation and transformation pipe.
-- 
-- @param input The input value.
-- @param pipe The pipe to be executed.
-- @param info The validation info.
-- @returns The output value.
function ____exports.executePipe(input, pipe, info)
    local issues
    local output = input
    for ____, action in ipairs(pipe) do
        local result = action(output, info)
        if result.issues then
            if issues then
                for ____, issue in ipairs(result.issues) do
                    issues[#issues + 1] = issue
                end
            else
                issues = result.issues
            end
            if info.abortEarly or info.abortPipeEarly then
                break
            end
        else
            output = result.output
        end
    end
    return issues and ({issues = issues}) or ({output = output})
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.matches.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.matches.matches")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.matches.matches"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates a pattern against a string.
-- 
-- @param requirement The pattern to be matched.
-- @param error The error message.
-- @returns A validation function.
function ____exports.matches(requirement, ____error)
    return function(input, info)
        local matches = {string.find(input, requirement)}
        if matches[1] == nil then
            return {issues = {getIssue(info, {validation = "matches", message = ____error or ("Invalid content, expected to match pattern \"" .. requirement) .. "\"", input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.value.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.value.value")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.value.value"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates the value of a string or number.
-- 
-- @param requirement The value.
-- @param error The error message.
-- @returns A validation function.
function ____exports.value(requirement, ____error)
    return function(input, info)
        if input ~= requirement then
            return {issues = {getIssue(info, {validation = "value", message = ____error or "Invalid value", input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.notStartsWith.index"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__StringStartsWith = ____lualib.__TS__StringStartsWith
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates the start of a string.
-- 
-- @param requirement The start string.
-- @param error The error message.
-- @returns A validation function.
function ____exports.notStartsWith(requirement, ____error)
    return function(input, info)
        if __TS__StringStartsWith(input, requirement) then
            return {issues = {getIssue(info, {validation = "not_starts_with", message = ____error or ("Invalid start, " .. requirement) .. " not allowed", input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.startsWith.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.startsWith.startsWith")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.startsWith.startsWith"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__StringStartsWith = ____lualib.__TS__StringStartsWith
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates the start of a string.
-- 
-- @param requirement The start string.
-- @param error The error message.
-- @returns A validation function.
function ____exports.startsWith(requirement, ____error)
    return function(input, info)
        if not __TS__StringStartsWith(input, requirement) then
            return {issues = {getIssue(info, {validation = "starts_with", message = ____error or "Invalid start, expect " .. requirement, input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.multipleOf.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.multipleOf.multipleOf")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.multipleOf.multipleOf"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation function that validates whether a number is a multiple.
-- 
-- @param requirement The divisor.
-- @param error The error message.
-- @returns A validation function.
function ____exports.multipleOf(requirement, ____error)
    return function(input, info)
        if input % requirement ~= 0 then
            return {issues = {getIssue(info, {validation = "multipleOf", message = ____error or "Invalid multiple", input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.minValue.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.minValue.minValue")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.minValue.minValue"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates the value of a string, number or date.
-- 
-- @param requirement The minimum value.
-- @param error The error message.
-- @returns A validation function.
function ____exports.minValue(requirement, ____error)
    return function(input, info)
        if input < requirement then
            return {issues = {getIssue(
                info,
                {
                    validation = "min_value",
                    message = ____error or "Invalid value, expected more than " .. tostring(requirement),
                    input = input
                }
            )}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.minLength.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.minLength.minLength")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.minLength.minLength"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates the length of a string or array.
-- 
-- @param requirement The minimum length.
-- @param error The error message.
-- @returns A validation function.
function ____exports.minLength(requirement, ____error)
    return function(input, info)
        if #input < requirement then
            return {issues = {getIssue(
                info,
                {
                    validation = "min_length",
                    message = ____error or "Invalid length, expected more than " .. tostring(requirement),
                    input = input
                }
            )}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.maxValue.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.maxValue.maxValue")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.maxValue.maxValue"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates the value of a string, number or date.
-- 
-- @param requirement The maximum value.
-- @param error The error message.
-- @returns A validation function.
function ____exports.maxValue(requirement, ____error)
    return function(input, info)
        if input > requirement then
            return {issues = {getIssue(
                info,
                {
                    validation = "max_value",
                    message = ____error or "Invalid value, expected less than " .. tostring(requirement),
                    input = input
                }
            )}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.maxLength.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.maxLength.maxLength")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.maxLength.maxLength"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates the length of a string or array.
-- 
-- @param requirement The maximum length.
-- @param error The error message.
-- @returns A validation function.
function ____exports.maxLength(requirement, ____error)
    return function(input, info)
        if #input > requirement then
            return {issues = {getIssue(
                info,
                {
                    validation = "max_length",
                    message = ____error or "Invalid length, expected less than " .. tostring(requirement),
                    input = input
                }
            )}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.includes.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.includes.includes")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.includes.includes"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__StringIncludes = ____lualib.__TS__StringIncludes
local __TS__ArrayIsArray = ____lualib.__TS__ArrayIsArray
local __TS__ArrayJoin = ____lualib.__TS__ArrayJoin
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates the content of a string or array.
-- 
-- @param requirement The content to be included.
-- @param error The error message.
-- @returns A validation function.
function ____exports.includes(requirement, ____error)
    return function(input, info)
        if not __TS__StringIncludes(input, requirement) then
            return {issues = {getIssue(
                info,
                {
                    validation = "includes",
                    message = ____error or "Invalid content, expected to include one of " .. tostring(__TS__ArrayIsArray(requirement) and __TS__ArrayJoin(requirement, ",") or requirement),
                    input = input
                }
            )}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.finite.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.finite.finite")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.finite.finite"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__NumberIsFinite = ____lualib.__TS__NumberIsFinite
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation function that validates whether a number is finite.
-- 
-- @param error The error message.
-- @returns A validation function.
function ____exports.finite(____error)
    return function(input, info)
        if not __TS__NumberIsFinite(input) then
            return {issues = {getIssue(info, {validation = "finite", message = ____error or "Invalid infinite number", input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.equal.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.equal.equal")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.equal.equal"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation function that checks the value for equality.
-- 
-- @param requirement The required value.
-- @param error The error message.
-- @returns A validation function.
function ____exports.equal(requirement, ____error)
    return function(input, info)
        if input ~= requirement then
            return {issues = {getIssue(info, {validation = "equal", message = ____error or "Invalid input", input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.notEndsWith.index"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__StringEndsWith = ____lualib.__TS__StringEndsWith
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates the end of a string.
-- 
-- @param requirement The end string.
-- @param error The error message.
-- @returns A validation function.
function ____exports.notEndsWith(requirement, ____error)
    return function(input, info)
        if __TS__StringEndsWith(input, requirement) then
            return {issues = {getIssue(info, {validation = "ends_with", message = ____error or ("Invalid end, " .. requirement) .. " not allowed", input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.endsWith.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.endsWith.endsWith")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.endsWith.endsWith"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__StringEndsWith = ____lualib.__TS__StringEndsWith
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a validation functions that validates the end of a string.
-- 
-- @param requirement The end string.
-- @param error The error message.
-- @returns A validation function.
function ____exports.endsWith(requirement, ____error)
    return function(input, info)
        if not __TS__StringEndsWith(input, requirement) then
            return {issues = {getIssue(info, {validation = "ends_with", message = ____error or "Invalid end, expected " .. requirement, input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.custom.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.custom.custom")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.validations.custom.customAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.custom.customAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a async custom validation function.
-- 
-- @param requirement The async validation function.
-- @param error The error message.
-- @returns A async validation function.
function ____exports.customAsync(requirement, ____error)
    return function(input, info)
        return __TS__AsyncAwaiter(function(____awaiter_resolve)
            if not __TS__Await(requirement(input)) then
                return ____awaiter_resolve(
                    nil,
                    {issues = {getIssue(info, {validation = "custom", message = ____error or "Invalid input", input = input})}}
                )
            end
            return ____awaiter_resolve(nil, {output = input})
        end)
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.validations.custom.custom"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a custom validation function.
-- 
-- @param requirement The validation function.
-- @param error The error message.
-- @returns A validation function.
function ____exports.custom(requirement, ____error)
    return function(input, info)
        if not requirement(input) then
            return {issues = {getIssue(info, {validation = "custom", message = ____error or "Invalid input", input = input})}}
        end
        return {output = input}
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toCustom.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toLowerCase.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toMaxValue.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toMinValue.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmed.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmedEnd.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmedStart.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toUpperCase.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toUpperCase.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toUpperCase.toUpperCase")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toUpperCase.toUpperCase"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Creates a transformation function that converts all the alphabetic
-- characters in a string to uppercase.
-- 
-- @returns A transformation function.
function ____exports.toUpperCase()
    return function(input) return {output = string.upper(input)} end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmedStart.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmedStart.toTrimmedStart")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmedStart.toTrimmedStart"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__StringTrimStart = ____lualib.__TS__StringTrimStart
local ____exports = {}
--- Creates a transformation function that removes the leading white space and
-- line terminator characters from a string.
-- 
-- @returns A transformation function.
function ____exports.toTrimmedStart()
    return function(input) return {output = __TS__StringTrimStart(input)} end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmedEnd.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmedEnd.toTrimmedEnd")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmedEnd.toTrimmedEnd"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__StringTrimEnd = ____lualib.__TS__StringTrimEnd
local ____exports = {}
--- Creates a transformation function that removes the trailing white space and
-- line terminator characters from a string.
-- 
-- @returns A transformation function.
function ____exports.toTrimmedEnd()
    return function(input) return {output = __TS__StringTrimEnd(input)} end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmed.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmed.toTrimmed")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toTrimmed.toTrimmed"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__StringTrim = ____lualib.__TS__StringTrim
local ____exports = {}
--- Creates a transformation function that removes the leading and trailing
-- white space and line terminator characters from a string.
-- 
-- @returns A transformation function.
function ____exports.toTrimmed()
    return function(input) return {output = __TS__StringTrim(input)} end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toMinValue.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toMinValue.toMinValue")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toMinValue.toMinValue"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Creates a transformation function that sets a string, number or date to a
-- minimum value.
-- 
-- @param requirement The minimum value.
-- @returns A transformation function.
function ____exports.toMinValue(requirement)
    return function(input) return {output = input < requirement and requirement or input} end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toMaxValue.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toMaxValue.toMaxValue")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toMaxValue.toMaxValue"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Creates a transformation function that sets a string, number or date to a
-- maximum value.
-- 
-- @param requirement The maximum value.
-- @returns A transformation function.
function ____exports.toMaxValue(requirement)
    return function(input) return {output = input > requirement and requirement or input} end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toLowerCase.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toLowerCase.toLowerCase")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toLowerCase.toLowerCase"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Creates a transformation function that converts all the alphabetic
-- characters in a string to lowercase.
-- 
-- @returns A transformation function.
function ____exports.toLowerCase()
    return function(input) return {output = string.lower(input)} end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toCustom.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toCustom.toCustom")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.transformations.toCustom.toCustomAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toCustom.toCustomAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
--- Creates a async custom transformation function.
-- 
-- @param action The transform action.
-- @returns A async transformation function.
function ____exports.toCustomAsync(action)
    return function(input) return __TS__AsyncAwaiter(function(____awaiter_resolve)
        return ____awaiter_resolve(
            nil,
            {output = __TS__Await(action(input))}
        )
    end) end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.transformations.toCustom.toCustom"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Creates a custom transformation function.
-- 
-- @param action The transform action.
-- @returns A transformation function.
function ____exports.toCustom(action)
    return function(input) return {output = action(input)} end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.any.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.array.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.boolean.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.enumType.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.instance.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.literal.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nonNullable.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nonNullish.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nonOptional.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nullable.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nullish.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.number.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.object.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.optional.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.record.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.recursive.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.special.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.string.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.tuple.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.union.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.union.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.union.union")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.union.unionAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.union.unionAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates an async union schema.
-- 
-- @param union The union schema.
-- @param error The error message.
-- @returns An async union schema.
function ____exports.unionAsync(union, ____error)
    return {
        schema = "union",
        union = union,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                local issues
                local output
                for ____, schema in ipairs(union) do
                    local result = __TS__Await(schema:_parse(input, info))
                    if result.issues then
                        if issues then
                            for ____, issue in ipairs(result.issues) do
                                issues[#issues + 1] = issue
                            end
                        else
                            issues = result.issues
                        end
                    else
                        output = {result.output}
                        break
                    end
                end
                return ____awaiter_resolve(
                    nil,
                    output and ({output = output[1]}) or ({issues = {getIssue(info, {
                        reason = "type",
                        validation = "union",
                        message = ____error or "Invalid type, expected union",
                        input = input,
                        issues = issues
                    })}})
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.union.union"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a union schema.
-- 
-- @param union The union schema.
-- @param error The error message.
-- @returns A union schema.
function ____exports.union(union, ____error)
    return {
        schema = "union",
        union = union,
        async = false,
        _parse = function(self, input, info)
            local issues
            local output
            for ____, schema in ipairs(union) do
                local result = schema:_parse(input, info)
                if result.issues then
                    if issues then
                        for ____, issue in ipairs(result.issues) do
                            issues[#issues + 1] = issue
                        end
                    else
                        issues = result.issues
                    end
                else
                    output = {result.output}
                    break
                end
            end
            return output and ({output = output[1]}) or ({issues = {getIssue(info, {
                reason = "type",
                validation = "union",
                message = ____error or "Invalid type, expected union",
                input = input,
                issues = issues
            })}})
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.tuple.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.tuple.tuple")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.tuple.tupleAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.tuple.tupleAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIsArray = ____lualib.__TS__ArrayIsArray
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local __TS__ArrayMap = ____lualib.__TS__ArrayMap
local __TS__PromiseAll = ____lualib.__TS__PromiseAll
local __TS__ArraySlice = ____lualib.__TS__ArraySlice
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipeAsync = ____index.executePipeAsync
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPath = ____index.getPath
local getPathInfo = ____index.getPathInfo
local getPipeInfo = ____index.getPipeInfo
--- Creates an async tuple schema.
-- 
-- @param items The items schema.
-- @param pipe A validation and transformation pipe.
-- @returns An async tuple schema.
-- @param items The items schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async tuple schema.
-- @param items The items schema.
-- @param rest The rest schema.
-- @param pipe A validation and transformation pipe.
-- @returns An async tuple schema.
-- @param items The items schema.
-- @param rest The rest schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async tuple schema.
function ____exports.tupleAsync(items, arg2, arg3, arg4)
    local ____temp_0 = type(arg2) == "table" and not __TS__ArrayIsArray(arg2) and __TS__ObjectAssign(
        {rest = arg2},
        getErrorAndPipe(arg3, arg4)
    ) or getErrorAndPipe(arg2, arg3)
    local rest = ____temp_0.rest
    local ____error = ____temp_0.error
    local pipe = ____temp_0.pipe
    return {
        schema = "tuple",
        tuple = {items = items, rest = rest},
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if not __TS__ArrayIsArray(input) or not rest and #items ~= #input or rest and #items > #input then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "tuple", message = ____error or "Invalid type, expected tuple", input = input})}}
                    )
                end
                local issues
                local output = {}
                __TS__Await(__TS__PromiseAll({
                    __TS__PromiseAll(__TS__ArrayMap(
                        items,
                        function(____, schema, index)
                            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                                if not (info and info.abortEarly and issues) then
                                    local value = input[index + 1]
                                    local result = __TS__Await(schema:_parse(
                                        value,
                                        getPathInfo(
                                            info,
                                            getPath(info and info.path, {schema = "tuple", input = input, key = index, value = value})
                                        )
                                    ))
                                    if not (info and info.abortEarly and issues) then
                                        if result.issues then
                                            if issues then
                                                for ____, issue in ipairs(result.issues) do
                                                    issues[#issues + 1] = issue
                                                end
                                            else
                                                issues = result.issues
                                            end
                                            if info and info.abortEarly then
                                                error(nil, 0)
                                            end
                                        else
                                            output[index + 1] = result.output
                                        end
                                    end
                                end
                            end)
                        end
                    )),
                    rest and __TS__PromiseAll(__TS__ArrayMap(
                        __TS__ArraySlice(input, #items),
                        function(____, value, index)
                            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                                if not (info and info.abortEarly and issues) then
                                    local tupleIndex = #items + index
                                    local result = __TS__Await(rest:_parse(
                                        value,
                                        getPathInfo(
                                            info,
                                            getPath(info and info.path, {schema = "tuple", input = input, key = tupleIndex, value = value})
                                        )
                                    ))
                                    if not (info and info.abortEarly and issues) then
                                        if result.issues then
                                            if issues then
                                                for ____, issue in ipairs(result.issues) do
                                                    issues[#issues + 1] = issue
                                                end
                                            else
                                                issues = result.issues
                                            end
                                            if info and info.abortEarly then
                                                error(nil, 0)
                                            end
                                        else
                                            output[tupleIndex + 1] = result.output
                                        end
                                    end
                                end
                            end)
                        end
                    ))
                }):catch(function() return nil end))
                return ____awaiter_resolve(
                    nil,
                    issues and ({issues = issues}) or executePipeAsync(
                        output,
                        pipe,
                        getPipeInfo(info, "tuple")
                    )
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.tuple.tuple"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIsArray = ____lualib.__TS__ArrayIsArray
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipe = ____index.executePipe
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPath = ____index.getPath
local getPathInfo = ____index.getPathInfo
local getPipeInfo = ____index.getPipeInfo
--- Creates a tuple schema.
-- 
-- @param items The items schema.
-- @param pipe A validation and transformation pipe.
-- @returns A tuple schema.
-- @param items The items schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns A tuple schema.
-- @param items The items schema.
-- @param rest The rest schema.
-- @param pipe A validation and transformation pipe.
-- @returns A tuple schema.
-- @param items The items schema.
-- @param rest The rest schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns A tuple schema.
function ____exports.tuple(items, arg2, arg3, arg4)
    local ____temp_0 = type(arg2) == "table" and not __TS__ArrayIsArray(arg2) and __TS__ObjectAssign(
        {rest = arg2},
        getErrorAndPipe(arg3, arg4)
    ) or getErrorAndPipe(arg2, arg3)
    local rest = ____temp_0.rest
    local ____error = ____temp_0.error
    local pipe = ____temp_0.pipe
    return {
        schema = "tuple",
        tuple = {items = items, rest = rest},
        async = false,
        _parse = function(self, input, info)
            if not __TS__ArrayIsArray(input) or not rest and #items ~= #input or rest and #items > #input then
                return {issues = {getIssue(info, {reason = "type", validation = "tuple", message = ____error or "Invalid type, expected tuple", input = input})}}
            end
            local issues
            local output = {}
            do
                local index = 0
                while index < #items do
                    local value = input[index + 1]
                    local result = items[index + 1]:_parse(
                        value,
                        getPathInfo(
                            info,
                            getPath(info and info.path, {schema = "tuple", input = input, key = index, value = value})
                        )
                    )
                    if result.issues then
                        if issues then
                            for ____, issue in ipairs(result.issues) do
                                issues[#issues + 1] = issue
                            end
                        else
                            issues = result.issues
                        end
                        if info and info.abortEarly then
                            break
                        end
                    else
                        output[index + 1] = result.output
                    end
                    index = index + 1
                end
            end
            if rest then
                do
                    local index = #items
                    while index < #input do
                        local value = input[index + 1]
                        local result = rest:_parse(
                            value,
                            getPathInfo(
                                info,
                                getPath(info and info.path, {schema = "tuple", input = input, key = index, value = value})
                            )
                        )
                        if result.issues then
                            if issues then
                                for ____, issue in ipairs(result.issues) do
                                    issues[#issues + 1] = issue
                                end
                            else
                                issues = result.issues
                            end
                            if info and info.abortEarly then
                                break
                            end
                        else
                            output[index + 1] = result.output
                        end
                        index = index + 1
                    end
                end
            end
            return issues and ({issues = issues}) or executePipe(
                output,
                pipe,
                getPipeInfo(info, "tuple")
            )
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.string.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.string.string")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.string.stringAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.string.stringAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipeAsync = ____index.executePipeAsync
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPipeInfo = ____index.getPipeInfo
--- Creates an async string schema.
-- 
-- @param pipe A validation and transformation pipe.
-- @returns An async string schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async string schema.
function ____exports.stringAsync(arg1, arg2)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg1, arg2)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "string",
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if type(input) ~= "string" then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "string", message = ____error or "Invalid type, expected string", input = input})}}
                    )
                end
                return ____awaiter_resolve(
                    nil,
                    executePipeAsync(
                        input,
                        pipe,
                        getPipeInfo(info, "string")
                    )
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.string.string"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipe = ____index.executePipe
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPipeInfo = ____index.getPipeInfo
--- Creates a string schema.
-- 
-- @param pipe A validation and transformation pipe.
-- @returns A string schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns A string schema.
function ____exports.string(arg1, arg2)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg1, arg2)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "string",
        async = false,
        _parse = function(self, input, info)
            if type(input) ~= "string" then
                return {issues = {getIssue(info, {reason = "type", validation = "string", message = ____error or "Invalid type, expected string", input = input})}}
            end
            return executePipe(
                input,
                pipe,
                getPipeInfo(info, "string")
            )
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.special.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.special.special")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.special.specialAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.special.specialAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipeAsync = ____index.executePipeAsync
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPipeInfo = ____index.getPipeInfo
--- Creates an async special schema.
-- Creates a special schema.
-- 
-- @param check The type check function.
-- @param pipe A validation and transformation pipe.
-- @returns An async special schema.
-- @param check The type check function.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns A special schema.
function ____exports.specialAsync(check, arg2, arg3)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg2, arg3)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "special",
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if not __TS__Await(check(input)) then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "special", message = ____error or "Invalid type, expected special", input = input})}}
                    )
                end
                return ____awaiter_resolve(
                    nil,
                    executePipeAsync(
                        input,
                        pipe,
                        getPipeInfo(info, "special")
                    )
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.special.special"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipe = ____index.executePipe
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPipeInfo = ____index.getPipeInfo
--- Creates a special schema.
-- 
-- @param check The type check function.
-- @param pipe A validation and transformation pipe.
-- @returns A special schema.
-- @param check The type check function.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns A special schema.
function ____exports.special(check, arg2, arg3)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg2, arg3)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "special",
        async = false,
        _parse = function(self, input, info)
            if not check(input) then
                return {issues = {getIssue(info, {reason = "type", validation = "special", message = ____error or "Invalid type, expected special", input = input})}}
            end
            return executePipe(
                input,
                pipe,
                getPipeInfo(info, "special")
            )
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.recursive.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.recursive.recursive")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.recursive.recursiveAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.recursive.recursiveAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
--- Creates an async recursive schema.
-- 
-- @param getter The schema getter.
-- @returns An async recursive schema.
function ____exports.recursiveAsync(getter)
    return {
        schema = "recursive",
        getter = getter,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                return ____awaiter_resolve(
                    nil,
                    getter():_parse(input, info)
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.recursive.recursive"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Creates a recursive schema.
-- 
-- @param getter The schema getter.
-- @returns A recursive schema.
function ____exports.recursive(getter)
    return {
        schema = "recursive",
        getter = getter,
        async = false,
        _parse = function(self, input, info)
            return getter():_parse(input, info)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.record.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.record.record")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.record.recordAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.record.recordAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIsArray = ____lualib.__TS__ArrayIsArray
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ArrayIncludes = ____lualib.__TS__ArrayIncludes
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local __TS__ArrayMap = ____lualib.__TS__ArrayMap
local __TS__PromiseAll = ____lualib.__TS__PromiseAll
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipeAsync = ____index.executePipeAsync
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPath = ____index.getPath
local getPathInfo = ____index.getPathInfo
local getPipeInfo = ____index.getPipeInfo
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.string.index")
local ____string = ____index.string
local ____values = require("lua_modules.@codethread.tstl-validate.dist.schemas.record.values")
local BLOCKED_KEYS = ____values.BLOCKED_KEYS
--- Creates an async record schema.
-- 
-- @param value The value schema.
-- @param pipe A validation and transformation pipe.
-- @returns An async record schema.
-- @param value The value schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async record schema.
-- @param key The key schema.
-- @param value The value schema.
-- @param pipe A validation and transformation pipe.
-- @returns An async record schema.
-- @param key The key schema.
-- @param value The value schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async record schema.
function ____exports.recordAsync(arg1, arg2, arg3, arg4)
    local ____temp_0 = type(arg2) == "table" and not __TS__ArrayIsArray(arg2) and __TS__ObjectAssign(
        {key = arg1, value = arg2},
        getErrorAndPipe(arg3, arg4)
    ) or __TS__ObjectAssign(
        {
            key = ____string(),
            value = arg1
        },
        getErrorAndPipe(arg2, arg3)
    )
    local key = ____temp_0.key
    local value = ____temp_0.value
    local ____error = ____temp_0.error
    local pipe = ____temp_0.pipe
    return {
        schema = "record",
        record = {key = key, value = value},
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if not input or type(input) ~= "table" or __TS__ArrayIsArray(input) then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "record", message = ____error or "Invalid type, expected record", input = input})}}
                    )
                end
                local issues
                local output = {}
                __TS__Await(__TS__PromiseAll(__TS__ArrayMap(
                    __TS__ObjectEntries(input),
                    function(____, inputEntry)
                        return __TS__AsyncAwaiter(function(____awaiter_resolve)
                            local inputKey = inputEntry[1]
                            if not __TS__ArrayIncludes(BLOCKED_KEYS, inputKey) then
                                local inputValue = inputEntry[2]
                                local path = getPath(info and info.path, {schema = "record", input = input, key = inputKey, value = inputValue})
                                local keyResult, valueResult = unpack(__TS__Await(__TS__PromiseAll(__TS__ArrayMap(
                                    {{schema = key, input = inputKey, origin = "key"}, {schema = value, input = inputValue}},
                                    function(____, ____bindingPattern0)
                                        local origin
                                        local input
                                        local schema
                                        schema = ____bindingPattern0.schema
                                        input = ____bindingPattern0.input
                                        origin = ____bindingPattern0.origin
                                        return __TS__AsyncAwaiter(function(____awaiter_resolve)
                                            if not (info and info.abortEarly and issues) then
                                                local result = __TS__Await(schema:_parse(
                                                    input,
                                                    getPathInfo(info, path, origin)
                                                ))
                                                if not (info and info.abortEarly and issues) then
                                                    if result.issues then
                                                        if issues then
                                                            for ____, issue in ipairs(result.issues) do
                                                                issues[#issues + 1] = issue
                                                            end
                                                        else
                                                            issues = result.issues
                                                        end
                                                        if info and info.abortEarly then
                                                            error(nil, 0)
                                                        end
                                                    else
                                                        return ____awaiter_resolve(nil, result)
                                                    end
                                                end
                                            end
                                        end)
                                    end
                                )):catch(function() return {} end)))
                                if keyResult and valueResult then
                                    output[keyResult.output] = valueResult.output
                                end
                            end
                        end)
                    end
                )))
                return ____awaiter_resolve(
                    nil,
                    issues and ({issues = issues}) or executePipeAsync(
                        output,
                        pipe,
                        getPipeInfo(info, "record")
                    )
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.record.values"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
____exports.BLOCKED_KEYS = {"__proto__", "prototype", "constructor"}
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.record.record"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIsArray = ____lualib.__TS__ArrayIsArray
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ArrayIncludes = ____lualib.__TS__ArrayIncludes
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipe = ____index.executePipe
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPath = ____index.getPath
local getPathInfo = ____index.getPathInfo
local getPipeInfo = ____index.getPipeInfo
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.string.index")
local ____string = ____index.string
local ____values = require("lua_modules.@codethread.tstl-validate.dist.schemas.record.values")
local BLOCKED_KEYS = ____values.BLOCKED_KEYS
--- Creates a record schema.
-- 
-- @param value The value schema.
-- @param pipe A validation and transformation pipe.
-- @returns A record schema.
-- @param value The value schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns A record schema.
-- @param key The key schema.
-- @param value The value schema.
-- @param pipe A validation and transformation pipe.
-- @returns A record schema.
-- @param key The key schema.
-- @param value The value schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns A record schema.
function ____exports.record(arg1, arg2, arg3, arg4)
    local ____temp_0 = type(arg2) == "table" and not __TS__ArrayIsArray(arg2) and __TS__ObjectAssign(
        {key = arg1, value = arg2},
        getErrorAndPipe(arg3, arg4)
    ) or __TS__ObjectAssign(
        {
            key = ____string(),
            value = arg1
        },
        getErrorAndPipe(arg2, arg3)
    )
    local key = ____temp_0.key
    local value = ____temp_0.value
    local ____error = ____temp_0.error
    local pipe = ____temp_0.pipe
    return {
        schema = "record",
        record = {key = key, value = value},
        async = false,
        _parse = function(self, input, info)
            if not input or type(input) ~= "table" or __TS__ArrayIsArray(input) then
                return {issues = {getIssue(info, {reason = "type", validation = "record", message = ____error or "Invalid type, expected record", input = input})}}
            end
            local issues
            local output = {}
            for ____, inputEntry in ipairs(__TS__ObjectEntries(input)) do
                local inputKey = inputEntry[1]
                if not __TS__ArrayIncludes(BLOCKED_KEYS, inputKey) then
                    local inputValue = inputEntry[2]
                    local path = getPath(info and info.path, {schema = "record", input = input, key = inputKey, value = inputValue})
                    local keyResult = key:_parse(
                        inputKey,
                        getPathInfo(info, path, "key")
                    )
                    if keyResult.issues then
                        if issues then
                            for ____, issue in ipairs(keyResult.issues) do
                                issues[#issues + 1] = issue
                            end
                        else
                            issues = keyResult.issues
                        end
                        if info and info.abortEarly then
                            break
                        end
                    end
                    local valueResult = value:_parse(
                        inputValue,
                        getPathInfo(info, path)
                    )
                    if valueResult.issues then
                        if issues then
                            for ____, issue in ipairs(valueResult.issues) do
                                issues[#issues + 1] = issue
                            end
                        else
                            issues = valueResult.issues
                        end
                        if info and info.abortEarly then
                            break
                        end
                    end
                    if not keyResult.issues and not valueResult.issues then
                        output[keyResult.output] = valueResult.output
                    end
                end
            end
            return issues and ({issues = issues}) or executePipe(
                output,
                pipe,
                getPipeInfo(info, "record")
            )
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.optional.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.optional.optional")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.optional.optionalAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.optional.optionalAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
--- Creates an async optional schema.
-- 
-- @param wrapped The wrapped schema.
-- @returns An async optional schema.
function ____exports.optionalAsync(wrapped)
    return {
        schema = "optional",
        wrapped = wrapped,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if input == nil then
                    return ____awaiter_resolve(nil, {output = input})
                end
                return ____awaiter_resolve(
                    nil,
                    wrapped:_parse(input, info)
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.optional.optional"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Creates a optional schema.
-- 
-- @param wrapped The wrapped schema.
-- @returns A optional schema.
function ____exports.optional(wrapped)
    return {
        schema = "optional",
        wrapped = wrapped,
        async = false,
        _parse = function(self, input, info)
            if input == nil then
                return {output = input}
            end
            return wrapped:_parse(input, info)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.object.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.object.object")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.object.objectAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.object.objectAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIsArray = ____lualib.__TS__ArrayIsArray
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local __TS__ArrayMap = ____lualib.__TS__ArrayMap
local __TS__PromiseAll = ____lualib.__TS__PromiseAll
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipeAsync = ____index.executePipeAsync
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPath = ____index.getPath
local getPathInfo = ____index.getPathInfo
local getPipeInfo = ____index.getPipeInfo
--- Creates an async object schema.
-- 
-- @param object The object schema.
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
-- @param object The object schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
function ____exports.objectAsync(object, arg2, arg3)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg2, arg3)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    local cachedEntries
    return {
        schema = "object",
        object = object,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if not input or type(input) ~= "table" or __TS__ArrayIsArray(input) then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "object", message = ____error or "Invalid type", input = input})}}
                    )
                end
                cachedEntries = cachedEntries or __TS__ObjectEntries(object)
                local issues
                local output = {}
                __TS__Await(__TS__PromiseAll(__TS__ArrayMap(
                    cachedEntries,
                    function(____, objectEntry)
                        return __TS__AsyncAwaiter(function(____awaiter_resolve)
                            if not (info and info.abortEarly and issues) then
                                local key = objectEntry[1]
                                local value = input[key]
                                local result = __TS__Await(objectEntry[2]:_parse(
                                    value,
                                    getPathInfo(
                                        info,
                                        getPath(info and info.path, {schema = "object", input = input, key = key, value = value})
                                    )
                                ))
                                if not (info and info.abortEarly and issues) then
                                    if result.issues then
                                        if issues then
                                            for ____, issue in ipairs(result.issues) do
                                                issues[#issues + 1] = issue
                                            end
                                        else
                                            issues = result.issues
                                        end
                                        if info and info.abortEarly then
                                            error(nil, 0)
                                        end
                                    else
                                        output[key] = result.output
                                    end
                                end
                            end
                        end)
                    end
                )):catch(function() return nil end))
                return ____awaiter_resolve(
                    nil,
                    issues and ({issues = issues}) or executePipeAsync(
                        output,
                        pipe,
                        getPipeInfo(info, "object")
                    )
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.object.object"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIsArray = ____lualib.__TS__ArrayIsArray
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipe = ____index.executePipe
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPath = ____index.getPath
local getPathInfo = ____index.getPathInfo
local getPipeInfo = ____index.getPipeInfo
--- Creates an object schema.
-- 
-- @param object The object schema.
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
-- @param object The object schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
function ____exports.object(object, arg2, arg3)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg2, arg3)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    local cachedEntries
    return {
        schema = "object",
        object = object,
        async = false,
        _parse = function(self, input, info)
            if not input or type(input) ~= "table" or __TS__ArrayIsArray(input) then
                return {issues = {getIssue(info, {reason = "type", validation = "table", message = ____error or "Invalid type", input = input})}}
            end
            cachedEntries = cachedEntries or __TS__ObjectEntries(object)
            local issues
            local output = {}
            for ____, objectEntry in ipairs(cachedEntries) do
                local key = objectEntry[1]
                local value = input[key]
                local result = objectEntry[2]:_parse(
                    value,
                    getPathInfo(
                        info,
                        getPath(info and info.path, {schema = "object", input = input, key = key, value = value})
                    )
                )
                if result.issues then
                    if issues then
                        for ____, issue in ipairs(result.issues) do
                            issues[#issues + 1] = issue
                        end
                    else
                        issues = result.issues
                    end
                    if info and info.abortEarly then
                        break
                    end
                else
                    output[key] = result.output
                end
            end
            return issues and ({issues = issues}) or executePipe(
                output,
                pipe,
                getPipeInfo(info, "object")
            )
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.number.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.number.number")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.number.numberAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.number.numberAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipeAsync = ____index.executePipeAsync
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPipeInfo = ____index.getPipeInfo
--- Creates an async number schema.
-- 
-- @param pipe A validation and transformation pipe.
-- @returns An async number schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async number schema.
function ____exports.numberAsync(arg1, arg2)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg1, arg2)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "number",
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if type(input) ~= "number" then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "number", message = ____error or "Invalid type, expected number", input = input})}}
                    )
                end
                return ____awaiter_resolve(
                    nil,
                    executePipeAsync(
                        input,
                        pipe,
                        getPipeInfo(info, "number")
                    )
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.number.number"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipe = ____index.executePipe
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPipeInfo = ____index.getPipeInfo
--- Creates a number schema.
-- 
-- @param pipe A validation and transformation pipe.
-- @returns A number schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns A number schema.
function ____exports.number(arg1, arg2)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg1, arg2)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "number",
        async = false,
        _parse = function(self, input, info)
            if type(input) ~= "number" then
                return {issues = {getIssue(info, {reason = "type", validation = "number", message = ____error or "Invalid type, expected number", input = input})}}
            end
            return executePipe(
                input,
                pipe,
                getPipeInfo(info, "number")
            )
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nullish.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nullish.nullish")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nullish.nullishAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nullish.nullishAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
--- Creates an async nullish schema.
-- 
-- @param wrapped The wrapped schema.
-- @returns An async nullish schema.
function ____exports.nullishAsync(wrapped)
    return {
        schema = "nullish",
        wrapped = wrapped,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if input == nil or input == nil then
                    return ____awaiter_resolve(nil, {output = input})
                end
                return ____awaiter_resolve(
                    nil,
                    wrapped:_parse(input, info)
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nullish.nullish"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Creates a nullish schema.
-- 
-- @param wrapped The wrapped schema.
-- @returns A nullish schema.
function ____exports.nullish(wrapped)
    return {
        schema = "nullish",
        wrapped = wrapped,
        async = false,
        _parse = function(self, input, info)
            if input == nil or input == nil then
                return {output = input}
            end
            return wrapped:_parse(input, info)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nullable.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nullable.nullable")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nullable.nullableAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nullable.nullableAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
--- Creates an async nullable schema.
-- 
-- @param wrapped The wrapped schema.
-- @returns An async nullable schema.
function ____exports.nullableAsync(wrapped)
    return {
        schema = "nullable",
        wrapped = wrapped,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if input == nil then
                    return ____awaiter_resolve(nil, {output = input})
                end
                return ____awaiter_resolve(
                    nil,
                    wrapped:_parse(input, info)
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nullable.nullable"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Creates a nullable schema.
-- 
-- @param wrapped The wrapped schema.
-- @returns A nullable schema.
function ____exports.nullable(wrapped)
    return {
        schema = "nullable",
        wrapped = wrapped,
        async = false,
        _parse = function(self, input, info)
            if input == nil then
                return {output = input}
            end
            return wrapped:_parse(input, info)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nonOptional.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nonOptional.nonOptional")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nonOptional.nonOptionalAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nonOptional.nonOptionalAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates an async non optional schema.
-- 
-- @param wrapped The wrapped schema.
-- @param error The error message.
-- @returns An async non optional schema.
function ____exports.nonOptionalAsync(wrapped, ____error)
    return {
        schema = "non_optional",
        wrapped = wrapped,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if input == nil then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "non_optional", message = ____error or "Invalid type, expected non_optional", input = input})}}
                    )
                end
                return ____awaiter_resolve(
                    nil,
                    wrapped:_parse(input, info)
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nonOptional.nonOptional"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a non optional schema.
-- 
-- @param wrapped The wrapped schema.
-- @param error The error message.
-- @returns A non optional schema.
function ____exports.nonOptional(wrapped, ____error)
    return {
        schema = "non_optional",
        wrapped = wrapped,
        async = false,
        _parse = function(self, input, info)
            if input == nil then
                return {issues = {getIssue(info, {reason = "type", validation = "non_optional", message = ____error or "Invalid type, expected non_optional", input = input})}}
            end
            return wrapped:_parse(input, info)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nonNullish.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nonNullish.nonNullish")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nonNullish.nonNullishAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nonNullish.nonNullishAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates an async non nullish schema.
-- 
-- @param wrapped The wrapped schema.
-- @param error The error message.
-- @returns An async non nullish schema.
function ____exports.nonNullishAsync(wrapped, ____error)
    return {
        schema = "non_nullish",
        wrapped = wrapped,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if input == nil or input == nil then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "non_nullish", message = ____error or "Invalid type, expected non_nullish", input = input})}}
                    )
                end
                return ____awaiter_resolve(
                    nil,
                    wrapped:_parse(input, info)
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nonNullish.nonNullish"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a non nullish schema.
-- 
-- @param wrapped The wrapped schema.
-- @param error The error message.
-- @returns A non nullish schema.
function ____exports.nonNullish(wrapped, ____error)
    return {
        schema = "non_nullish",
        wrapped = wrapped,
        async = false,
        _parse = function(self, input, info)
            if input == nil or input == nil then
                return {issues = {getIssue(info, {reason = "type", validation = "non_nullish", message = ____error or "Invalid type, expected non_nullish", input = input})}}
            end
            return wrapped:_parse(input, info)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nonNullable.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nonNullable.nonNullable")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.nonNullable.nonNullableAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nonNullable.nonNullableAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates an async non nullable schema.
-- 
-- @param wrapped The wrapped schema.
-- @param error The error message.
-- @returns An async non nullable schema.
function ____exports.nonNullableAsync(wrapped, ____error)
    return {
        schema = "non_nullable",
        wrapped = wrapped,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if input == nil then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "non_nullable", message = ____error or "Invalid type, expected non_nullable", input = input})}}
                    )
                end
                return ____awaiter_resolve(
                    nil,
                    wrapped:_parse(input, info)
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.nonNullable.nonNullable"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a non nullable schema.
-- 
-- @param wrapped The wrapped schema.
-- @param error The error message.
-- @returns A non nullable schema.
function ____exports.nonNullable(wrapped, ____error)
    return {
        schema = "non_nullable",
        wrapped = wrapped,
        async = false,
        _parse = function(self, input, info)
            if input == nil then
                return {issues = {getIssue(info, {reason = "type", validation = "non_nullable", message = ____error or "Invalid type, expected non_nullable", input = input})}}
            end
            return wrapped:_parse(input, info)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.literal.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.literal.literal")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.literal.literalAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.literal.literalAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates an async literal schema.
-- 
-- @param literal The literal value.
-- @param error The error message.
-- @returns An async literal schema.
function ____exports.literalAsync(literal, ____error)
    return {
        schema = "literal",
        literal = literal,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if input ~= literal then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "literal", message = ____error or "Invalid type, expected literal", input = input})}}
                    )
                end
                return ____awaiter_resolve(nil, {output = input})
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.literal.literal"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a literal schema.
-- 
-- @param literal The literal value.
-- @param error The error message.
-- @returns A literal schema.
function ____exports.literal(literal, ____error)
    return {
        schema = "literal",
        literal = literal,
        async = false,
        _parse = function(self, input, info)
            if input ~= literal then
                return {issues = {getIssue(info, {reason = "type", validation = "literal", message = ____error or "Invalid type, expected literal", input = input})}}
            end
            return {output = input}
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.instance.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.instance.instance")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.instance.instanceAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.instance.instanceAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__InstanceOf = ____lualib.__TS__InstanceOf
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipeAsync = ____index.executePipeAsync
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPipeInfo = ____index.getPipeInfo
--- Creates an async instance schema.
-- 
-- @param of The class of the instance.
-- @param pipe A validation and transformation pipe.
-- @returns An async instance schema.
-- @param of The class of the instance.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async instance schema.
function ____exports.instanceAsync(of, arg2, arg3)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg2, arg3)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "instance",
        class = of,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if not __TS__InstanceOf(input, of) then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "instance", message = ____error or "Invalid type, expected instance", input = input})}}
                    )
                end
                return ____awaiter_resolve(
                    nil,
                    executePipeAsync(
                        input,
                        pipe,
                        getPipeInfo(info, "instance")
                    )
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.instance.instance"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__InstanceOf = ____lualib.__TS__InstanceOf
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipe = ____index.executePipe
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPipeInfo = ____index.getPipeInfo
--- Creates an instance schema.
-- 
-- @param of The class of the instance.
-- @param pipe A validation and transformation pipe.
-- @returns An instance schema.
-- @param of The class of the instance.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An instance schema.
function ____exports.instance(of, arg2, arg3)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg2, arg3)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "instance",
        class = of,
        async = false,
        _parse = function(self, input, info)
            if not __TS__InstanceOf(input, of) then
                return {issues = {getIssue(info, {reason = "type", validation = "instance", message = ____error or "Invalid type, expected instance", input = input})}}
            end
            return executePipe(
                input,
                pipe,
                getPipeInfo(info, "instance")
            )
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.enumType.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.enumType.enumType")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.enumType.enumTypeAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.enumType.enumTypeAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIncludes = ____lualib.__TS__ArrayIncludes
local __TS__ArrayJoin = ____lualib.__TS__ArrayJoin
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates an async enum schema.
-- 
-- @param enumValue The enum value.
-- @param error The error message.
-- @returns An async enum schema.
function ____exports.enumTypeAsync(enumValue, ____error)
    return {
        schema = "enum",
        enum = enumValue,
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if not __TS__ArrayIncludes(enumValue, input) then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(
                            info,
                            {
                                reason = "type",
                                validation = "enum",
                                message = ____error or "Invalid type, expected one of " .. __TS__ArrayJoin(enumValue, ", "),
                                input = input
                            }
                        )}}
                    )
                end
                return ____awaiter_resolve(nil, {output = input})
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.enumType.enumType"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIncludes = ____lualib.__TS__ArrayIncludes
local __TS__ArrayJoin = ____lualib.__TS__ArrayJoin
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a enum schema.
-- 
-- @param enumValue The enum value.
-- @param error The error message.
-- @returns A enum schema.
function ____exports.enumType(enumValue, ____error)
    return {
        schema = "enum",
        enum = enumValue,
        async = false,
        _parse = function(self, input, info)
            if not __TS__ArrayIncludes(enumValue, input) then
                return {issues = {getIssue(
                    info,
                    {
                        reason = "type",
                        validation = "enum",
                        message = ____error or "Invalid type, expected one of " .. __TS__ArrayJoin(enumValue, ", "),
                        input = input
                    }
                )}}
            end
            return {output = input}
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.boolean.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.boolean.boolean")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.boolean.booleanAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.boolean.booleanAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipeAsync = ____index.executePipeAsync
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPipeInfo = ____index.getPipeInfo
--- Creates an async boolean schema.
-- 
-- @param pipe A validation and transformation pipe.
-- @returns An async boolean schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async boolean schema.
function ____exports.booleanAsync(arg1, arg2)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg1, arg2)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "boolean",
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if type(input) ~= "boolean" then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "boolean", message = ____error or "Invalid type, expected boolean", input = input})}}
                    )
                end
                return ____awaiter_resolve(
                    nil,
                    executePipeAsync(
                        input,
                        pipe,
                        getPipeInfo(info, "boolean")
                    )
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.boolean.boolean"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipe = ____index.executePipe
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPipeInfo = ____index.getPipeInfo
--- Creates a boolean schema.
-- 
-- @param pipe A validation and transformation pipe.
-- @returns A boolean schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns A boolean schema.
function ____exports.boolean(arg1, arg2)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg1, arg2)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "boolean",
        async = false,
        _parse = function(self, input, info)
            if type(input) ~= "boolean" then
                return {issues = {getIssue(info, {reason = "type", validation = "boolean", message = ____error or "Invalid type, expected boolean", input = input})}}
            end
            return executePipe(
                input,
                pipe,
                getPipeInfo(info, "boolean")
            )
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.array.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.array.array")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.array.arrayAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.array.arrayAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIsArray = ____lualib.__TS__ArrayIsArray
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local __TS__ArrayMap = ____lualib.__TS__ArrayMap
local __TS__PromiseAll = ____lualib.__TS__PromiseAll
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipeAsync = ____index.executePipeAsync
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPath = ____index.getPath
local getPathInfo = ____index.getPathInfo
local getPipeInfo = ____index.getPipeInfo
--- Creates an async array schema.
-- 
-- @param item The item schema.
-- @param pipe A validation and transformation pipe.
-- @returns An async array schema.
-- @param item The item schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async array schema.
function ____exports.arrayAsync(item, arg2, arg3)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg2, arg3)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "array",
        array = {item = item},
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                if not __TS__ArrayIsArray(input) then
                    return ____awaiter_resolve(
                        nil,
                        {issues = {getIssue(info, {reason = "type", validation = "array", message = ____error or "Invalid type, expected array", input = input})}}
                    )
                end
                local issues
                local output = {}
                __TS__Await(__TS__PromiseAll(__TS__ArrayMap(
                    input,
                    function(____, value, key)
                        return __TS__AsyncAwaiter(function(____awaiter_resolve)
                            if not (info and info.abortEarly and issues) then
                                local result = __TS__Await(item:_parse(
                                    value,
                                    getPathInfo(
                                        info,
                                        getPath(info and info.path, {schema = "array", input = input, key = key, value = value})
                                    )
                                ))
                                if not (info and info.abortEarly and issues) then
                                    if result.issues then
                                        if issues then
                                            for ____, issue in ipairs(result.issues) do
                                                issues[#issues + 1] = issue
                                            end
                                        else
                                            issues = result.issues
                                        end
                                        if info and info.abortEarly then
                                            error(nil, 0)
                                        end
                                    else
                                        output[key + 1] = result.output
                                    end
                                end
                            end
                        end)
                    end
                )):catch(function() return nil end))
                return ____awaiter_resolve(
                    nil,
                    issues and ({issues = issues}) or executePipeAsync(
                        output,
                        pipe,
                        getPipeInfo(info, "array")
                    )
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.array.array"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIsArray = ____lualib.__TS__ArrayIsArray
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipe = ____index.executePipe
local getErrorAndPipe = ____index.getErrorAndPipe
local getIssue = ____index.getIssue
local getPath = ____index.getPath
local getPathInfo = ____index.getPathInfo
local getPipeInfo = ____index.getPipeInfo
--- Creates a array schema.
-- 
-- @param item The item schema.
-- @param pipe A validation and transformation pipe.
-- @returns A array schema.
-- @param item The item schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns A array schema.
function ____exports.array(item, arg2, arg3)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg2, arg3)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return {
        schema = "array",
        array = {item = item},
        async = false,
        _parse = function(self, input, info)
            if not __TS__ArrayIsArray(input) then
                return {issues = {getIssue(info, {reason = "type", validation = "list", message = ____error or "Invalid type", input = input})}}
            end
            local issues
            local output = {}
            do
                local index = 0
                while index < #input do
                    local value = input[index + 1]
                    local result = item:_parse(
                        value,
                        getPathInfo(
                            info,
                            getPath(info and info.path, {schema = "array", input = input, key = index, value = value})
                        )
                    )
                    if result.issues then
                        if issues then
                            for ____, issue in ipairs(result.issues) do
                                issues[#issues + 1] = issue
                            end
                        else
                            issues = result.issues
                        end
                        if info and info.abortEarly then
                            break
                        end
                    else
                        output[#output + 1] = result.output
                    end
                    index = index + 1
                end
            end
            return issues and ({issues = issues}) or executePipe(
                output,
                pipe,
                getPipeInfo(info, "array")
            )
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.any.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.any.any")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.schemas.any.anyAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.any.anyAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipeAsync = ____index.executePipeAsync
local getPipeInfo = ____index.getPipeInfo
--- Creates an async any schema.
-- 
-- @param pipe A validation and transformation pipe.
-- @returns An async any schema.
function ____exports.anyAsync(pipe)
    if pipe == nil then
        pipe = {}
    end
    return {
        schema = "any",
        async = true,
        _parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                return ____awaiter_resolve(
                    nil,
                    executePipeAsync(
                        input,
                        pipe,
                        getPipeInfo(info, "any")
                    )
                )
            end)
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.schemas.any.any"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local executePipe = ____index.executePipe
local getPipeInfo = ____index.getPipeInfo
--- Creates a any schema.
-- 
-- @param pipe A validation and transformation pipe.
-- @returns A any schema.
function ____exports.any(pipe)
    if pipe == nil then
        pipe = {}
    end
    return {
        schema = "any",
        async = false,
        _parse = function(self, input, info)
            return executePipe(
                input,
                pipe,
                getPipeInfo(info, "any")
            )
        end
    }
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.brand.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.coerce.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.is.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.keyof.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.merge.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.omit.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.parse.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.partial.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.pick.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.required.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.safeParse.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.strict.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.transform.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.unwrap.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.withDefault.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.withDefault.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.withDefault.withDefault")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.withDefault.withDefault"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local ____exports = {}
--- Passes a default value to a schema in case of an undefined input.
-- 
-- @param schema The affected schema.
-- @param value The default value.
-- @returns The passed schema.
function ____exports.withDefault(schema, value)
    return __TS__ObjectAssign(
        {},
        schema,
        {_parse = function(self, input, info)
            local ____schema_1 = schema
            local ____schema__parse_2 = schema._parse
            local ____temp_0
            if input == nil then
                ____temp_0 = value
            else
                ____temp_0 = input
            end
            return ____schema__parse_2(____schema_1, ____temp_0, info)
        end}
    )
end
--- See {@link withDefault}
-- 
-- @deprecated Function has been renamed to `withDefault`.
____exports.useDefault = ____exports.withDefault
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.unwrap.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.unwrap.unwrap")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.unwrap.unwrap"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Unwraps the wrapped schema.
-- 
-- @param schema The schema to be unwrapped.
-- @returns The unwrapped schema.
function ____exports.unwrap(schema)
    return schema.wrapped
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.transform.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.transform.transform")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.transform.transformAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.transform.transformAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local ____exports = {}
--- Adds an async transformation step to a schema, which is executed at the end
-- of parsing and can change the output type.
-- 
-- @param schema The schema to be used.
-- @param action The transformation action.
-- @returns A transformed schema.
function ____exports.transformAsync(schema, action)
    return __TS__ObjectAssign(
        {},
        schema,
        {
            async = true,
            _parse = function(self, input, info)
                return __TS__AsyncAwaiter(function(____awaiter_resolve)
                    local result = __TS__Await(schema:_parse(input, info))
                    return ____awaiter_resolve(
                        nil,
                        result.issues and result or ({output = __TS__Await(action(result.output))})
                    )
                end)
            end
        }
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.transform.transform"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local ____exports = {}
--- Adds a transformation step to a schema, which is executed at the end of
-- parsing and can change the output type.
-- 
-- @param schema The schema to be used.
-- @param action The transformation action.
-- @returns A transformed schema.
function ____exports.transform(schema, action)
    return __TS__ObjectAssign(
        {},
        schema,
        {_parse = function(self, input, info)
            local result = schema:_parse(input, info)
            return result.issues and result or ({output = action(result.output)})
        end}
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.strict.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.strict.strict")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.strict.strictAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.strict.strictAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local __TS__ObjectKeys = ____lualib.__TS__ObjectKeys
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a strict async object schema that throws an error if an input
-- contains unknown keys.
-- 
-- @param schema A object schema.
-- @param error The error message.
-- @returns A strict object schema.
function ____exports.strictAsync(schema, ____error)
    return __TS__ObjectAssign(
        {},
        schema,
        {_parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                local result = __TS__Await(schema:_parse(input, info))
                return ____awaiter_resolve(
                    nil,
                    not result.issues and #__TS__ObjectKeys(input) ~= #__TS__ObjectKeys(result.output) and ({issues = {getIssue(info, {reason = "object", validation = "strict", message = ____error or "Invalid keys", input = input})}}) or result
                )
            end)
        end}
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.strict.strict"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectKeys = ____lualib.__TS__ObjectKeys
local __TS__ArrayIncludes = ____lualib.__TS__ArrayIncludes
local __TS__ArrayFilter = ____lualib.__TS__ArrayFilter
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getIssue = ____index.getIssue
--- Creates a strict object schema that throws an error if an input contains
-- unknown keys.
-- 
-- @param schema A object schema.
-- @param error The error message.
-- @returns A strict object schema.
function ____exports.strict(schema, ____error)
    return __TS__ObjectAssign(
        {},
        schema,
        {_parse = function(self, input, info)
            local result = schema:_parse(input, info)
            if result.issues then
                return result
            end
            local iKeys = __TS__ObjectKeys(input)
            local rKeys = __TS__ObjectKeys(result.output)
            local newKeys = __TS__ArrayFilter(
                iKeys,
                function(____, key) return not __TS__ArrayIncludes(rKeys, key) end
            )
            return #newKeys > 0 and ({issues = {getIssue(
                info,
                {
                    reason = "object",
                    validation = "strict",
                    message = ____error or "Invalid keys: " .. table.concat(newKeys, ","),
                    input = input
                }
            )}}) or result
        end}
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.safeParse.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.safeParse.safeParse")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.safeParse.safeParseAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.safeParse.safeParseAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local __TS__New = ____lualib.__TS__New
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.error.index")
local ValiError = ____index.ValiError
--- Parses unknown input based on a schema.
-- 
-- @param schema The schema to be used.
-- @param input The input to be parsed.
-- @param info The optional parse info.
-- @returns The parsed output.
function ____exports.safeParseAsync(schema, input, info)
    return __TS__AsyncAwaiter(function(____awaiter_resolve)
        local result = __TS__Await(schema:_parse(input, info))
        return ____awaiter_resolve(
            nil,
            result.issues and ({
                success = false,
                error = __TS__New(ValiError, result.issues),
                issues = result.issues
            }) or ({success = true, data = result.output})
        )
    end)
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.error.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.error.flatten.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.error.ValiError.index")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.error.ValiError.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.error.ValiError.ValiError")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.error.ValiError.ValiError"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__Class = ____lualib.__TS__Class
local Error = ____lualib.Error
local RangeError = ____lualib.RangeError
local ReferenceError = ____lualib.ReferenceError
local SyntaxError = ____lualib.SyntaxError
local TypeError = ____lualib.TypeError
local URIError = ____lualib.URIError
local __TS__ClassExtends = ____lualib.__TS__ClassExtends
local ____exports = {}
--- A Valibot error with useful information.
____exports.ValiError = __TS__Class()
local ValiError = ____exports.ValiError
ValiError.name = "ValiError"
__TS__ClassExtends(ValiError, Error)
function ValiError.prototype.____constructor(self, issues)
    Error.prototype.____constructor(self, issues[1].message)
    self.name = "ValiError"
    self.issues = issues
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.error.flatten.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.error.flatten.flatten")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.error.flatten.flatten"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayMap = ____lualib.__TS__ArrayMap
local __TS__ArrayJoin = ____lualib.__TS__ArrayJoin
local __TS__SparseArrayNew = ____lualib.__TS__SparseArrayNew
local __TS__SparseArrayPush = ____lualib.__TS__SparseArrayPush
local __TS__SparseArraySpread = ____lualib.__TS__SparseArraySpread
local __TS__ArrayIsArray = ____lualib.__TS__ArrayIsArray
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
--- Flatten the error messages of a Vali error.
-- Flatten the error messages of issues.
-- 
-- @param error A Vali error.
-- @returns Flat errors.
-- @param issues The issues.
-- @returns Flat errors.
function ____exports.flatten(arg1)
    return __TS__ArrayReduce(
        __TS__ArrayIsArray(arg1) and arg1 or arg1.issues,
        function(____, flatErrors, issue)
            if issue.path then
                local path = __TS__ArrayJoin(
                    __TS__ArrayMap(
                        issue.path,
                        function(____, ____bindingPattern0)
                            local key
                            key = ____bindingPattern0.key
                            return key
                        end
                    ),
                    "."
                )
                local ____flatErrors_nested_1 = flatErrors.nested
                local ____array_0 = __TS__SparseArrayNew(unpack(flatErrors.nested[path] or ({})))
                __TS__SparseArrayPush(____array_0, issue.message)
                ____flatErrors_nested_1[path] = {__TS__SparseArraySpread(____array_0)}
            else
                local ____flatErrors_3 = flatErrors
                local ____array_2 = __TS__SparseArrayNew(unpack(flatErrors.root or ({})))
                __TS__SparseArrayPush(____array_2, issue.message)
                ____flatErrors_3.root = {__TS__SparseArraySpread(____array_2)}
            end
            return flatErrors
        end,
        {nested = {}}
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.safeParse.safeParse"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
local ____tstl_2Dresult = require("lua_modules.@codethread.tstl-result.dist.index")
local err = ____tstl_2Dresult.err
local ok = ____tstl_2Dresult.ok
--- Parses unknown input based on a schema.
-- 
-- @param schema The schema to be used.
-- @param input The input to be parsed.
-- @param info The optional parse info.
-- @returns The parsed output.
function ____exports.safeParse(schema, input, info)
    local result = schema:_parse(input, info)
    return result.issues and err(result.issues) or ok(result.output)
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.required.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.required.required")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.required.requiredAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.required.requiredAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local nonOptionalAsync = ____index.nonOptionalAsync
local objectAsync = ____index.objectAsync
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getErrorAndPipe = ____index.getErrorAndPipe
--- Creates an async object schema consisting of all properties of an existing
-- object schema set to none optional.
-- 
-- @param schema The affected schema.
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
-- @param schema The affected schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
function ____exports.requiredAsync(schema, arg3, arg4)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg3, arg4)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return objectAsync(
        __TS__ArrayReduce(
            __TS__ObjectEntries(schema.object),
            function(____, object, ____bindingPattern0)
                local schema
                local key
                key = ____bindingPattern0[1]
                schema = ____bindingPattern0[2]
                return __TS__ObjectAssign(
                    {},
                    object,
                    {[key] = nonOptionalAsync(schema)}
                )
            end,
            {}
        ),
        ____error,
        pipe
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.required.required"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local nonOptional = ____index.nonOptional
local object = ____index.object
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getErrorAndPipe = ____index.getErrorAndPipe
--- Creates an object schema consisting of all properties of an existing object
-- schema set to none optional.
-- 
-- @param schema The affected schema.
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
-- @param schema The affected schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
function ____exports.required(schema, arg3, arg4)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg3, arg4)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return object(
        __TS__ArrayReduce(
            __TS__ObjectEntries(schema.object),
            function(____, object, ____bindingPattern0)
                local schema
                local key
                key = ____bindingPattern0[1]
                schema = ____bindingPattern0[2]
                return __TS__ObjectAssign(
                    {},
                    object,
                    {[key] = nonOptional(schema)}
                )
            end,
            {}
        ),
        ____error,
        pipe
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.pick.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.pick.pick")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.pick.pickAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.pick.pickAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIncludes = ____lualib.__TS__ArrayIncludes
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local objectAsync = ____index.objectAsync
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getErrorAndPipe = ____index.getErrorAndPipe
--- Creates an async object schema that contains only the selected keys of an
-- existing schema.
-- 
-- @param schema The schema to pick from.
-- @param keys The selected keys
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
-- @param schema The schema to pick from.
-- @param keys The selected keys
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
function ____exports.pickAsync(schema, keys, arg3, arg4)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg3, arg4)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return objectAsync(
        __TS__ArrayReduce(
            __TS__ObjectEntries(schema.object),
            function(____, object, ____bindingPattern0)
                local schema
                local key
                key = ____bindingPattern0[1]
                schema = ____bindingPattern0[2]
                return __TS__ArrayIncludes(keys, key) and __TS__ObjectAssign({}, object, {[key] = schema}) or object
            end,
            {}
        ),
        ____error,
        pipe
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.pick.pick"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIncludes = ____lualib.__TS__ArrayIncludes
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local object = ____index.object
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getErrorAndPipe = ____index.getErrorAndPipe
--- Creates an object schema that contains only the selected keys of an existing
-- schema.
-- 
-- @param schema The schema to pick from.
-- @param keys The selected keys
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
-- @param schema The schema to pick from.
-- @param keys The selected keys
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
function ____exports.pick(schema, keys, arg3, arg4)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg3, arg4)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return object(
        __TS__ArrayReduce(
            __TS__ObjectEntries(schema.object),
            function(____, object, ____bindingPattern0)
                local schema
                local key
                key = ____bindingPattern0[1]
                schema = ____bindingPattern0[2]
                return __TS__ArrayIncludes(keys, key) and __TS__ObjectAssign({}, object, {[key] = schema}) or object
            end,
            {}
        ),
        ____error,
        pipe
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.partial.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.partial.partial")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.partial.partialAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.partial.partialAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local objectAsync = ____index.objectAsync
local optionalAsync = ____index.optionalAsync
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getErrorAndPipe = ____index.getErrorAndPipe
--- Creates an async object schema consisting of all properties of an existing
-- object schema set to optional.
-- 
-- @param schema The affected schema.
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
-- @param schema The affected schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
function ____exports.partialAsync(schema, arg3, arg4)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg3, arg4)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return objectAsync(
        __TS__ArrayReduce(
            __TS__ObjectEntries(schema.object),
            function(____, object, ____bindingPattern0)
                local schema
                local key
                key = ____bindingPattern0[1]
                schema = ____bindingPattern0[2]
                return __TS__ObjectAssign(
                    {},
                    object,
                    {[key] = optionalAsync(schema)}
                )
            end,
            {}
        ),
        ____error,
        pipe
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.partial.partial"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local object = ____index.object
local nullable = ____index.nullable
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getErrorAndPipe = ____index.getErrorAndPipe
--- Creates an object schema consisting of all properties of an existing object
-- schema set to nullable.
-- 
-- @param schema The affected schema.
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
-- @param schema The affected schema.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
function ____exports.partial(schema, arg3, arg4)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg3, arg4)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return object(
        __TS__ArrayReduce(
            __TS__ObjectEntries(schema.object),
            function(____, object, ____bindingPattern0)
                local schema
                local key
                key = ____bindingPattern0[1]
                schema = ____bindingPattern0[2]
                return __TS__ObjectAssign(
                    {},
                    object,
                    {[key] = nullable(schema)}
                )
            end,
            {}
        ),
        ____error,
        pipe
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.parse.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.parse.parse")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.parse.parseAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.parse.parseAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local __TS__New = ____lualib.__TS__New
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.error.index")
local ValiError = ____index.ValiError
--- Parses unknown input based on a schema.
-- 
-- @param schema The schema to be used.
-- @param input The input to be parsed.
-- @param info The optional parse info.
-- @returns The parsed output.
function ____exports.parseAsync(schema, input, info)
    return __TS__AsyncAwaiter(function(____awaiter_resolve)
        local result = __TS__Await(schema:_parse(input, info))
        if result.issues then
            error(
                __TS__New(ValiError, result.issues),
                0
            )
        end
        return ____awaiter_resolve(nil, result.output)
    end)
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.parse.parse"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__New = ____lualib.__TS__New
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.error.index")
local ValiError = ____index.ValiError
--- Parses unknown input based on a schema.
-- 
-- @param schema The schema to be used.
-- @param input The input to be parsed.
-- @param info The optional parse info.
-- @returns The parsed output.
function ____exports.parse(schema, input, info)
    local result = schema:_parse(input, info)
    if result.issues then
        error(
            __TS__New(ValiError, result.issues),
            0
        )
    end
    return result.output
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.omit.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.omit.omit")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.omit.omitAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.omit.omitAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIncludes = ____lualib.__TS__ArrayIncludes
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local objectAsync = ____index.objectAsync
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getErrorAndPipe = ____index.getErrorAndPipe
--- Creates an async object schema that contains only the selected keys of an
-- existing schema.
-- 
-- @param schema The schema to omit from.
-- @param keys The selected keys
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
-- @param schema The schema to omit from.
-- @param keys The selected keys
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
function ____exports.omitAsync(schema, keys, arg3, arg4)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg3, arg4)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return objectAsync(
        __TS__ArrayReduce(
            __TS__ObjectEntries(schema.object),
            function(____, object, ____bindingPattern0)
                local schema
                local key
                key = ____bindingPattern0[1]
                schema = ____bindingPattern0[2]
                return __TS__ArrayIncludes(keys, key) and object or __TS__ObjectAssign({}, object, {[key] = schema})
            end,
            {}
        ),
        ____error,
        pipe
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.omit.omit"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ArrayIncludes = ____lualib.__TS__ArrayIncludes
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ObjectEntries = ____lualib.__TS__ObjectEntries
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local object = ____index.object
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getErrorAndPipe = ____index.getErrorAndPipe
--- Creates an object schema that contains not the selected keys of an existing
-- schema.
-- 
-- @param schema The schema to omit from.
-- @param keys The selected keys
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
-- @param schema The schema to omit from.
-- @param keys The selected keys
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
function ____exports.omit(schema, keys, arg3, arg4)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg3, arg4)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return object(
        __TS__ArrayReduce(
            __TS__ObjectEntries(schema.object),
            function(____, object, ____bindingPattern0)
                local schema
                local key
                key = ____bindingPattern0[1]
                schema = ____bindingPattern0[2]
                return __TS__ArrayIncludes(keys, key) and object or __TS__ObjectAssign({}, object, {[key] = schema})
            end,
            {}
        ),
        ____error,
        pipe
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.merge.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.merge.merge")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.merge.mergeAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.merge.mergeAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local objectAsync = ____index.objectAsync
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getErrorAndPipe = ____index.getErrorAndPipe
--- Merges multiple async object schemas into a single one. Subsequent object
-- schemas overwrite the previous ones.
-- 
-- @param schemas The schemas to be merged.
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
-- @param schemas The schemas to be merged.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An async object schema.
function ____exports.mergeAsync(schemas, arg2, arg3)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg2, arg3)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return objectAsync(
        __TS__ArrayReduce(
            schemas,
            function(____, object, schemas) return __TS__ObjectAssign({}, object, schemas.object) end,
            {}
        ),
        ____error,
        pipe
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.merge.merge"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local __TS__ArrayReduce = ____lualib.__TS__ArrayReduce
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local object = ____index.object
local ____index = require("lua_modules.@codethread.tstl-validate.dist.utils.index")
local getErrorAndPipe = ____index.getErrorAndPipe
--- Merges multiple object schemas into a single one. Subsequent object schemas
-- overwrite the previous ones.
-- 
-- @param schemas The schemas to be merged.
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
-- @param schemas The schemas to be merged.
-- @param error The error message.
-- @param pipe A validation and transformation pipe.
-- @returns An object schema.
function ____exports.merge(schemas, arg2, arg3)
    local ____getErrorAndPipe_result_0 = getErrorAndPipe(arg2, arg3)
    local ____error = ____getErrorAndPipe_result_0.error
    local pipe = ____getErrorAndPipe_result_0.pipe
    return object(
        __TS__ArrayReduce(
            schemas,
            function(____, object, schemas) return __TS__ObjectAssign({}, object, schemas.object) end,
            {}
        ),
        ____error,
        pipe
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.keyof.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.keyof.keyof")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.keyof.keyof"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectKeys = ____lualib.__TS__ObjectKeys
local ____exports = {}
local ____index = require("lua_modules.@codethread.tstl-validate.dist.schemas.index")
local enumType = ____index.enumType
--- Creates a enum schema of object keys.
-- 
-- @param schema The object schema.
-- @returns A enum schema.
function ____exports.keyof(schema)
    return enumType(__TS__ObjectKeys(schema.object))
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.is.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.is.is")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.is.is"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
--- Checks if the input matches the scheme. By using a type predicate, this
-- function can be used as a type guard.
-- 
-- @param schema The schema to be used.
-- @param input The input to be tested.
-- @returns Whether the input matches the scheme.
function ____exports.is(schema, input)
    return not schema:_parse(input, {abortEarly = true}).issues
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.coerce.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.coerce.coerce")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
do
    local ____export = require("lua_modules.@codethread.tstl-validate.dist.methods.coerce.coerceAsync")
    for ____exportKey, ____exportValue in pairs(____export) do
        if ____exportKey ~= "default" then
            ____exports[____exportKey] = ____exportValue
        end
    end
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.coerce.coerceAsync"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__AsyncAwaiter = ____lualib.__TS__AsyncAwaiter
local __TS__Await = ____lualib.__TS__Await
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local ____exports = {}
--- Coerces the input of a async schema to match the required type.
-- 
-- @param schema The affected schema.
-- @param action The coerceation action.
-- @returns The passed schema.
function ____exports.coerceAsync(schema, action)
    return __TS__ObjectAssign(
        {},
        schema,
        {_parse = function(self, input, info)
            return __TS__AsyncAwaiter(function(____awaiter_resolve)
                return ____awaiter_resolve(
                    nil,
                    schema:_parse(
                        __TS__Await(action(input)),
                        info
                    )
                )
            end)
        end}
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.coerce.coerce"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__ObjectAssign = ____lualib.__TS__ObjectAssign
local ____exports = {}
--- Coerces the input of a schema to match the required type.
-- 
-- @param schema The affected schema.
-- @param action The coerceation action.
-- @returns The passed schema.
function ____exports.coerce(schema, action)
    return __TS__ObjectAssign(
        {},
        schema,
        {_parse = function(self, input, info)
            return schema:_parse(
                action(input),
                info
            )
        end}
    )
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.brand.index"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
do
    local ____brand = require("lua_modules.@codethread.tstl-validate.dist.methods.brand.brand")
    local brand = ____brand.brand
    ____exports.brand = brand
end
return ____exports
 end,
["lua_modules.@codethread.tstl-validate.dist.methods.brand.brand"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__Symbol = ____lualib.__TS__Symbol
local Symbol = ____lualib.Symbol
local ____exports = {}
local symbol = __TS__Symbol("brand")
--- Brands the output type of a schema.
-- 
-- @param schema The scheme to be branded.
-- @param brand The brand name.
-- @returns The branded schema.
function ____exports.brand(schema, name)
    return schema
end
return ____exports
 end,
["index"] = function(...) 
local ____lualib = require("lualib_bundle")
local __TS__StringStartsWith = ____lualib.__TS__StringStartsWith
local __TS__ArrayFind = ____lualib.__TS__ArrayFind
local __TS__ArrayMap = ____lualib.__TS__ArrayMap
local ____exports = {}
local wezterm = require("wezterm")
local ____settings = require("settings")
local Settings = ____settings.Settings
local act = wezterm.action
local config = wezterm.config_builder()
Settings:applyToConfig(config)
local runWorkProject = wezterm.action_callback(function(w, p)
    local TARGET = "work-web"
    local sessions = wezterm.mux.get_workspace_names()
    local runningSession = __TS__ArrayFind(
        sessions,
        function(____, s) return __TS__StringStartsWith(s, TARGET) end
    )
    if runningSession then
        w:perform_action(
            act.SwitchToWorkspace({name = runningSession}),
            p
        )
        return
    end
    local projects = {"deals-light-ui", "fe-review", "fe-native"}
    local choices = __TS__ArrayMap(
        projects,
        function(____, s) return {label = s} end
    )
    w:perform_action(
        act.InputSelector({
            fuzzy = true,
            fuzzy_description = wezterm.format({{Attribute = {Intensity = "Bold"}}, {Foreground = {AnsiColor = "Fuchsia"}}, {Text = "foo bar"}}),
            choices = choices,
            action = wezterm.action_callback(function(w, p, _, l)
                w:perform_action(
                    act.SwitchToWorkspace({name = (TARGET .. "-") .. l, spawn = {cwd = ("/Users/adam.hall/" .. "work/") .. l, args = {"testy"}}}),
                    p
                )
            end)
        }),
        p
    )
end)
____exports.config = config
____exports.runWorkProject = runWorkProject
return ____exports
 end,
["utils"] = function(...) 
--[[ Generated with https://github.com/TypeScriptToLua/TypeScriptToLua ]]
local ____exports = {}
____exports.Utils = {tap = function(____, msg) return function(____, s)
    print(("tap" .. (msg and " " .. msg or "")) .. ":", s)
    return s
end end}
return ____exports
 end,
}
return require("index", ...)
