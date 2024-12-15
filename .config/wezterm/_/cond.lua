----- Example usage
---local match = cond({
---    { { 1 }, 'only 1 number' },
---    { { 1, 2 }, '2 numbers long' },
---    { { 1, 2, 3 }, '3 numbers long' },
---})
---
----- Assertions to test our code
---assert(match({ 1 }) == 'only 1 number')
---assert(match({ 1, 2 }) == '2 numbers long')
---assert(match({ 1, 2, 3 }) == '3 numbers long')
---
---assert(match({}) == nil) -- no matched items so return nil
---assert(match({ 6 }) == nil) -- no matched items as the 6 is not equal to 1 from our `cond` function
---
---print("All assertions passed!")
---```
---
---### Explanation:
---1. The `cond` function takes `patterns` as an argument, which is a list of tuples (`{ pattern, result }`).
---2. It returns a nested function that takes `input`, which is the table we want to match against the patterns.
---3. Inside the nested function, we iterate over each pattern and check if the length of `input` matches the length of the current pattern.
---4. If the lengths match, we then compare each element of `input` to the corresponding element of the pattern. If all elements match, we return the corresponding result.
---5. If no patterns match by the end of the loop, we return `nil`.
---
---This implementation efficiently checks for matches and provides the expected results as per your usage examples.
return function(patterns)
	-- Return a function that will match the input against the patterns
	return function(input)
		-- Iterate over the patterns
		for _, pattern in ipairs(patterns) do
			local pat, result = pattern[1], pattern[2]

			-- Check if the length of the input matches the length of the pattern
			if #input == #pat then
				local match = true
				-- Check if all elements match
				for i = 1, #pat do
					if input[i] ~= pat[i] then
						match = false
						break
					end
				end

				-- If there is a match, return the corresponding result
				if match then return result end
			end
		end

		-- If no patterns matched, return nil
		return nil
	end
end
