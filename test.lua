local cmd = arg

local function lua(...)
	local x = table.concat({cmd[-1], ...}, "\t")
	return io.popen(x, "r"):read("*a")
end

-- Requires `find`
for inFile in io.popen("find tests/ -name '*.in' -print0", "rb"):read("*a"):gmatch("[\1-\255]+") do
	local test = inFile:match "^(.*)%.in$"
	assert(test, "bad filename `" .. inFile .. "`")
	local actual = lua("luafmt.lua", inFile)
	local outFile = io.open(test .. ".out", "r")
	if not outFile then
		print("# MISSING FILE `" .. test .. ".out`")
		os.exit(1)
	end
	local expected = io.open(test .. ".out", "r"):read("*a")

	if actual ~= expected then
		print("FAIL " .. test .. " " .. string.rep("#", 100 - #test))
		print("EXPECTED ```" .. expected .. "```")
		print("BUT GOT  ```" .. actual .. "```")
	else
		print("PASS " .. test)
	end
end

print(string.rep("-", 106))
