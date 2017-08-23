local filename = arg[1]
if not filename then
	printHelp()
end

local file = io.open(filename, "r")
if not file then
	print("cannot open file `" .. filename .. "`")
	os.exit(1)
end

local COLUMN_LIMIT = 80

--------------------------------------------------------------------------------

local function matcher(pattern, tag)
	assert(type(tag) == "string")
	return function(text, offset)
		local from, to = text:find("^" .. pattern, offset)
		if from then
			return to, tag
		end
	end
end

local IS_KEYWORD = {
	["if"] = true,
	["then"] = true,
	["elseif"] = true,
	["else"] = true,
	["end"] = true,
	["for"] = true,
	["in"] = true,
	["do"] = true,
	["repeat"] = true,
	["until"] = true,
	["while"] = true,
	["function"] = true,
	-- in line
	["local"] = true,
	["return"] = true,
	["break"] = true,
}

local TOKENS = {
	-- string literals
	function(text, offset)
		local quote = text:sub(offset, offset)
		if quote == "\"" or quote == "'" then
			local back = false
			for i = offset+1, #text do
				if back then
					back = false
				elseif text:sub(i, i) == "\\" then
					back = true
				elseif text:sub(i, i) == quote then
					return i, "string"
				end
			end
		end
	end,

	-- long string literals
	function(text, offset)
		local from, to = text:find("^%[=*%[", offset)
		if from then
			local size = to - from - 1
			local _, stop = text:find("%]" .. string.rep("=", size) .. "%]", offset)
			assert(stop)
			return stop, "string"
		end
	end,

	-- comments
	function(text, offset)
		if text:sub(offset, offset+1) == "--" then
			local start, startLen = text:find("^%[=*%[", offset + 2)
			if start then
				local size = startLen - start - 1
				local _, stop = text:find("%]" .. string.rep("=", size) .. "%]", offset)
				assert(stop)
				return stop, "comment"
			end
			return (text:find("\n", offset) or #text+1) - 1, "comment"
		end
	end,

	-- whitespace
	function(text, offset)
		local _, space = text:find("^%s+", offset)
		if space then
			local breaks = 0
			for _ in text:sub(offset, space):gmatch("\n") do
				breaks = breaks + 1
			end
			if breaks > 1 then
				return space, "empty"
			end
			return space, "whitespace"
		end
	end,

	-- number
	function(text, offset)
		local _, limit = text:find("^[0-9.-+eExa-fA-F]+", offset)
		local last
		for i = offset, limit or offset do
			if tonumber(text:sub(offset, i)) then
				last = i
			end
		end
		if last then
			return last, "number"
		end
	end,

	-- dots
	matcher("%.%.%.", "name"),

	-- concat
	matcher("%.%.", "operator"),

	-- identifiers and keywords
	function(text, offset)
		local from, to = text:find("^[a-zA-Z0-9_]+", offset)
		if to then
			local word = text:sub(from, to)
			local size = #word
			
			if IS_KEYWORD[word] then
				return to, word
			elseif word == "not" or word == "or" or word == "and" then
				return to, "operator"
			end
			return to, "word"
		end
	end,

	-- accessors
	matcher("[:.]", "access"),

	-- entry separator
	matcher("[;,]", "separator"),

	-- opening brace
	matcher("[%[{(]", "open"),

	-- closing brace
	matcher("[%]})]", "close"),

	-- operators
	function(text, offset)
		local operators = {
			"==",
			"<=",
			">=",
			"~=",
			"^",
			"*",
			"/",
			"%",
			"<",
			">",
			"+",
			"-",
			"#",
		}
		for _, op in ipairs(operators) do
			local to = offset + #op - 1
			if text:sub(offset, to) == op then
				return to, "operator"
			end
		end
	end,
	-- assignment
	matcher("=", "assign"),
}

local function tokenize(blob)
	local tokens = {}
	local offset = 1
	while offset < #blob do
		local didCut = false
		for _, t in ipairs(TOKENS) do
			local cut, tag = t(blob, offset)
			if cut then
				assert(type(cut) == "number", _ .. " number")
				assert(cut >= offset, _ .. " offset")
				assert(tag, tostring(_) .. " tag")
				if tag ~= "whitespace" then
					table.insert(tokens, {text = blob:sub(offset, cut), tag = tag})
				end
				offset = cut + 1
				didCut = true
				break
			end
		end
		assert(didCut, blob:sub(offset, offset+50))
	end
	return tokens
end

--------------------------------------------------------------------------------

local function splitLines(tokens)
	-- These token tags MUST be the first token in their line
	local MUST_START = {
		["if"] = true,
		["local"] = true,
		["repeat"] = true,
		["else"] = true,
		["elseif"] = true,
		["end"] = true,
		["until"] = true,
		["while"] = true,
		["for"] = true,

		-- statements
		["return"] = true,
		["break"] = true,

		-- Formatting
		["comment"] = true,
		["empty"] = true,
	}

	-- These token tags MUST be the final token in their line
	local MUST_END = {
		["then"] = true,
		["else"] = true,
		["end"] = true,
		["repeat"] = true,
		["do"] = true,

		-- statements
		["break"] = true,

		-- Formatting
		["empty"] = true,
		["comment"] = true,
	}

	-- These token tags MUST be the first token in their line when the
	-- associated function returns `true` for the given context
	local MIGHT_START = {
		["do"] = function(line)
			if line[1].tag == "for" or line[1].tag == "while" then
				return false
			end
			return true
		end,
		["function"] = function(line, context)
			if context(1).tag == "open" then
				-- anonymous functions don't begin lines
				return false
			elseif context(-1).tag == "local" then
				-- the `local` begins a `local function` line
				return false
			end
			return true
		end,
	}

	-- These token tags MUST be the final token in their line when the
	-- associated function returns `true` for the given context
	local MIGHT_END = {
		["close"] = function(line, context, token)
			if token.text == ")" then
				-- Find out if this closing parenthesis ends a function's
				-- parameter list by walking backwards, stopping at the first
				-- `)` (no) or `function` (yes)
				for i = -1, -math.huge, -1 do
					-- XXX: this could be linear in stupid scripts
					if context(i).tag == "close" or context(i).tag == "close-parameters" then
						return false
					elseif context(i).tag == "^" then
						return false
					elseif context(i).tag == "function" then
						token.tag = "close-parameters"
						return true
					end
				end
			end
			return false
		end,
	}

	local statementBreak = {
		{"close", "word"},
		{"word", "word"},
		{"string", "word"},
		{"number", "word"},
	}

	local lines = {{}}
	for i, token in ipairs(tokens) do
		local function context(offset)
			return tokens[i + offset] or {
				text = "",
				tag = offset < 0 and "^" or "$"
			}
		end

		if #lines[#lines] == 0 then
		elseif MUST_START[token.tag] or (MIGHT_START[token.tag] and MIGHT_START[token.tag](lines[#lines], context, token)) then
			table.insert(lines, {})
		end
		local line = lines[#lines]
		table.insert(line, token)

		local semicolon = false
		for _, pair in ipairs(statementBreak) do
			if token.tag == pair[1] and context(1).tag == pair[2] then
				semicolon = true
			end
		end

		if MUST_END[token.tag] or (MIGHT_END[token.tag] and MIGHT_END[token.tag](line, context, token)) or semicolon then
			table.insert(lines, {})
		end
	end

	if #lines[#lines] == 0 then
		table.remove(lines)
	end

	local INCREASE = {
		["if"] = true,
		["while"] = true,
		["repeat"] = true,
		["else"] = true,
		["elseif"] = true,
		["for"] = true,
		["do"] = true,
		["function"] = true,
	}
	local DECREASE = {
		["end"] = true,
		["else"] = true,
		["elseif"] = true,
		["until"] = true,
	}

	local out = {}
	for _, line in ipairs(lines) do
		if DECREASE[line[1].tag] then
			table.insert(out, {text = "", tag = "indent-decrease"})
		end
		table.insert(out, {text = "", tag = "newline"})
		for _, token in ipairs(line) do
			if out[#out] and out[#out].tag == "newline" and token.tag == "separator" then
				table.remove(out)
				table.insert(out, token)
				table.insert(out, {text = "", tag = "newline"})
			else
				table.insert(out, token)
			end
		end

		if INCREASE[line[1].tag] or line[#line].tag == "close-parameters" then
			table.insert(out, {text = "", tag = "indent-increase"})
		end
	end

	return out
end

--------------------------------------------------------------------------------

local function trimmed(s)
	return s:match("^%s*(.*)$"):match("^(.-)%s*$")
end

local function spaceTokens(tokens)
	local out = {}
	local indent = 0

	local GLUES = {
		{"*", "separator"},
		{"*", "newline"},
		{"*", "empty"},
		{"*", "indent-increase"},
		{"*", "indent-decrease"},
		{"*", "access"},
		{"access", "*"},
		{"word", "open"},
		{"open", "*"},
		{"*", "close"},
		{"*", "close-parameters"},
		{"function", "open"},
		{"'#", "word"},
		{"'#", "open"},
		{"*", "$"},
		{"close", "open"},
	}

	local SPACE = {
		{"word", "'{"},
	}

	local function lmatch(pattern, a, b)
		if pattern == "*" then
			return true
		elseif pattern:sub(1, 1) == "'" then
			return pattern:sub(2) == b
		end
		return pattern == a
	end

	for i, token in ipairs(tokens) do
		if token.tag == "newline" then
			table.insert(out, token)
		elseif token.tag == "empty" then
			-- skip
		elseif token.tag == "indent-increase" then
			table.insert(out, token)
		elseif token.tag == "indent-decrease" then
			table.insert(out, token)
		else
			table.insert(out, {
				text = trimmed(token.text),
				tag = token.tag,
			})
			
			local before = tokens[i]
			local after = tokens[i+1] or {tag = "$", text = ""}
			local glued = false
			for _, glue in ipairs(GLUES) do
				if lmatch(glue[1], before.tag, before.text) and lmatch(glue[2], after.tag, after.text) then
					glued = true
				end
			end
			for _, space in ipairs(SPACE) do
				if lmatch(space[1], before.tag, before.text) and lmatch(space[2], after.tag, after.text) then
					glued = false
				end
			end
			if not glued then
				table.insert(out, {text = " ", tag = "space"})
			end
		end
	end

	while out[#out] and out[#out].tag == "newline" do
		table.remove(out)
	end
	while out[1] and out[1].tag == "newline" do
		-- XXX: linear
		table.remove(out, 1)
	end

	return out
end

--------------------------------------------------------------------------------

local TAB_SIZE = 4

local function keepLongPaired(tokens)
	error("TODO")
end

local function breakLongPaired(tokens)
	assert(tokens[1].tag == "open")
	assert(tokens[#tokens].tag == "close")

	local out = {}

	return out
end

local function measureColumn(stack)
	stack[1].indent = 0

	local c = 0
	local from = 1
	for i = #stack, 1, -1 do
		if stack[i].tag == "newline" and stack[i].indent then
			from = i
			break
		end
	end
	local indent = stack[from].indent or 0

	for i = from, #stack do
		local token = stack[i]
		if token.tag == "newline" then
			c = TAB_SIZE * indent
			token.indent = indent
		elseif token.tag == "indent-increase" then
			indent = indent + 1
		elseif token.tag == "indent-decrease" then
			indent = indent - 1
		else
			c = c + #token.text
		end
	end
	return c
end

local function breakLong(tokens)
	local out = {}

	local map = {}
	local root = {first = 1, last = #tokens}
	root.container = root
	local container = root
	for i, token in ipairs(tokens) do
		if token.tag:sub(1, 5) == "close" then
			local c = {
				tag = "close",
				first = container.first,
				last = i,
				container = container.container,
			}
			table.insert(map, c)
			container.last = i
			container = container.container
		elseif token.tag == "open" then
			local o = {
				tag = "open",
				first = i,
				last = nil,
				container = container,
			}
			table.insert(map, o)
			container = o
		else
			table.insert(map, {container = container})
		end
	end

	local function measureWidth(from, to)
		local width = 0
		for i = from, to do
			width = width + #tokens[i].text
			if tokens[i].tag == "newline" then
				return math.huge
			end
		end
		return width
	end

	local function startParen(index)
		local container = map[index]
		if not container.tag then
			return container.container
		end
		return container
	end

	for i, token in ipairs(tokens) do
		local group = startParen(i)
		if token.tag == "open" then
			token.expand = measureColumn(out) + measureWidth(i, group.last) > COLUMN_LIMIT
		end
		local expand = tokens[group.first].expand

		if expand then
			if token.tag == "close" or token.tag == "close-parameters" then
				local needsNewline = out[#out] and out[#out].tag ~= "newline"

				if needsNewline then
					table.insert(out, {tag = "indent-decrease", text = ""})
					table.insert(out, {tag = "newline", text = ""})
				else
					table.insert(out, #out, {tag = "indent-decrease", text = ""})
				end
			end
		end

		if token.tag == "space" then
			if out[#out] and out[#out].tag ~= "newline" then
				table.insert(out, token)
			end
		else
			table.insert(out, token)
		end

		if expand then
			if token.tag == "open" then
				token.text = token.text
				table.insert(out, {tag = "indent-increase", text = ""})
				table.insert(out, {tag = "newline", text = ""})
			elseif token.tag == "separator" then
				if tokens[i+1] and tokens[i+1].tag ~= "newline" then
					-- A newline can already exist from the `end,` construction
					table.insert(out, {tag = "newline", text = ""})
				end
			end
		end
	end

	return out
end

--------------------------------------------------------------------------------

local function printTokens(tokens)
	local indent = 0

	for i, token in ipairs(tokens) do
		if token.tag == "newline" then
			io.write("\n")
			if tokens[i+1] and tokens[i+1].tag ~= "newline" then
				io.write(string.rep("\t", indent))
			end
		elseif token.tag == "indent-increase" then
			indent = indent + 1
		elseif token.tag == "indent-decrease" then
			indent = indent - 1
		else
			io.write(token.text)
		end
	end
	print()
end

local tokens = tokenize(file:read("*all"))
local split = breakLong(spaceTokens(splitLines(tokens)))

printTokens(split)
