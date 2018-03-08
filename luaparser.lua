--go@ bin/mingw64/luajit -jp *

--Lua lexer and parser using LuaJIT+ffi.
--Translated from llex.c v2.20.1.2 (Lua 5.1.5) by Cosmin Apreutesei.

local ffi = require'ffi'
local bit = require'bit'
local ljs = require'ljstring'
local C = ffi.C

--lexer ----------------------------------------------------------------------

local isalnum = ljs.isalnum
local isspace = ljs.isspace
local isdigit = ljs.isdigit
local isalpha = ljs.isalpha

local function isnewline(c)
	return c == 10 or c == 13
end

local tokens = {
	concat = '..',
	dots = '...',
	eq = '==',
	ge = '>=',
	le = '<=',
	ne = '~=',
}
local reserved = {}
for _,k in ipairs{
	'and', 'break', 'do', 'else', 'elseif',
	'end', 'false', 'for', 'function', 'if',
	'in', 'local', 'nil', 'not', 'or', 'repeat',
	'return', 'then', 'true', 'until', 'while',
} do
	tokens[k] = k
	reserved[k] = true
end

local function chunkid(source)
	--[[
	if (*source == '=') {
		strncpy(out, source+1, buflen);  /* remove first char */
		out[buflen-1] = '\0';  /* ensures null termination */
	}
	else {  /* out = "source", or "...source" */
		if (*source == '@') {
			size_t l;
			source++;  /* skip the `@' */
			buflen -= sizeof(" '...' ");
			l = strlen(source);
			strcpy(out, "");
			if (l > buflen) {
				source += (l-buflen);  /* get last part of file name */
				strcat(out, "...");
			}
			strcat(out, source);
		}
		else {  /* out = [string "string"] */
			size_t len = strcspn(source, "\n\r");  /* stop at first newline */
			buflen -= sizeof(" [string \"...\"] ");
			if (len > buflen) len = buflen;
			strcpy(out, "[string \"");
			if (source[len] ~= '\0') {  /* must truncate? */
				strncat(out, source, len);
				strcat(out, "...");
			}
			else
				strcat(out, source);
			strcat(out, "\"]");
		}
	}
	]]
	return source
end

local function lexer(read, source)

	local max_bufsize = 2^31-1
	local max_linenumber = 2^31-3

	local buf_ct = ffi.typeof'uint8_t[?]'
	local bufsize = 64
	local buf = buf_ct(bufsize) --token buffer
	local len = 0 --token buffer filled length
	local c --current character ascii code
	local linenumber = 1

	local str = ffi.string
	local b = string.byte

	local save --fw. decl.

	local function format_token(token)
		if token == 'name' or token == 'string' or token == 'number' then
			save(0)
			return str(buf)
		else
			return tokens[token] or token
		end
	end
	local function lexerror(msg, token)
		msg = string.format('%s:%d: %s', chunkid(source), linenumber, msg)
		if token then
			msg = string.format('%s near %s', msg, format_token(token))
		end
		error(msg)
	end

	local function resizebuffer(newsize)
		local newbuf = buf_ct(newsize)
		ffi.copy(newbuf, buf, len)
		buf = newbuf
		bufsize = newsize
	end

	local function resetbuffer()
		len = 0
	end

	local EOF = -1 --because is*(c) can handle c == -1

	local function nextchar_func()
		local size = 4096
		local buf = ffi.new('uint8_t[?]', size)
		local ofs, len = 0, 0
		return function()
			if len == 0 then
				ofs = 0
				len = read(buf, size)
			end
			local sz = math.min(1, len)
			if sz <= 0 then
				c = EOF
			else
				c = buf[ofs]
				ofs = ofs + 1
				len = len - 1
			end
		end
	end
	local nextchar = nextchar_func()

	function save(c)
		if len == bufsize then
			if bufsize >= max_bufsize then
				lexerror'lexical element too long'
			end
			resizebuffer(bufsize * 2)
		end
		buf[len] = c
		len = len + 1
	end

	local function save_and_nextchar()
		save(c)
		nextchar()
	end

	local function inclinenumber()
		local c0 = c
		assert(isnewline(c))
		nextchar()  -- skip `\n' or `\r'
		if isnewline(c) and c ~= c0 then
			nextchar()  -- skip `\n\r' or `\r\n'
		end
		linenumber = linenumber + 1
		if linenumber >= max_linenumber then
			lexerror'chunk has too many lines'
		end
	end

	local function check_next(c1)
		if c ~= c1 then
			return false
		end
		save_and_nextchar()
		return true
	end

	local function lex_number()
		assert(isdigit(c))
		repeat
			save_and_nextchar()
		until not (isdigit(c) or c == b'.')
		if check_next(b'E') or check_next(b'e') then  -- `E'?
			if not check_next(b'+') then  -- optional exponent sign
				check_next(b'-')
			end
		end
		while isalnum(c) or c == b'_' do
			save_and_nextchar()
		end
		save(0)
		return ljs.strscan(buf)
	end


	local function skip_sep()
		local count = 0
		local c0 = c
		assert(c == b'[' or c == b']')
		save_and_nextchar()
		while c == b'=' do
			save_and_nextchar()
			count = count + 1
		end
		return c == c0 and count or (-count) - 1
	end


	local function lex_long_string(seminfo, sep)
		local cont = 0
		save_and_nextchar()  -- skip 2nd `['
		if isnewline(c) then  -- string starts with a newline?
			inclinenumber()  -- skip it
		end
		while true do
			if c == EOF then
				lexerror(seminfo and 'unfinished long string'
					or 'unfinished long comment', 'eos')
				-- to avoid warnings
			elseif c == b']' then
				if skip_sep() == sep then
					save_and_nextchar()  -- skip 2nd `]'
					goto endloop
				end
			elseif isnewline(c) then
				save(b'\n')
				inclinenumber()
				if not seminfo then
					resetbuffer()
				end -- avoid wasting space
			else
				if seminfo then
					save_and_nextchar()
				else
					nextchar()
				end
			end
		end ::endloop::
		if seminfo then
			return str(buf + (2 + sep), len - 2 * (2 + sep))
		end
	end

	local function lex_string()
		local delim = c
		save_and_nextchar()
		while c ~= delim do
			if c == EOF then
				lexerror('unfinished string', 'eos')
			elseif isnewline(c) then
				lexerror('unfinished string', 'string')
			elseif c == b'\\' then
				nextchar() -- do not save the `\'
				if     c == b'a' then save(b'\a'); nextchar(); goto continue
				elseif c == b'b' then save(b'\b'); nextchar(); goto continue
				elseif c == b'f' then save(b'\f'); nextchar(); goto continue
				elseif c == b'n' then save(b'\n'); nextchar(); goto continue
				elseif c == b'r' then save(b'\r'); nextchar(); goto continue
				elseif c == b't' then save(b'\t'); nextchar(); goto continue
				elseif c == b'v' then save(b'\v'); nextchar(); goto continue
				elseif isnewline(c) then -- go through
					save(b'\n')
					inclinenumber()
					goto continue
				elseif c == EOF then
					goto continue  -- will raise an error next loop
				elseif not isdigit(c) then
					save_and_nextchar() --handles \\, \', \', and \?
					goto continue
				else  -- \xxx
					local i = 0
					local d = 0
					repeat
						d = 10 * d + (c - b'0')
						nextchar()
						i = i + 1
					until not (i < 3 and isdigit(c))
					if d > 255 then
						lexerror('escape sequence too large', 'string')
					end
					save(d)
					goto continue
				end
			else
				save_and_nextchar()
			end
			::continue::
		end
		save_and_nextchar() -- skip delimiter
		local s = str(buf + 1, len - 2)
		return s
	end

	local function lex()
		resetbuffer()
		while true do
			if isnewline(c) then
				inclinenumber()
				goto continue
			elseif c == b'-' then
				nextchar()
				if c ~= b'-' then return b'-' end
				-- else is a comment
				nextchar()
				if c == b'[' then
					local sep = skip_sep() --int
					resetbuffer()  -- `skip_sep' may dirty the buffer
					if sep >= 0 then
						local s = lex_long_string(nil, sep)  -- long comment
						resetbuffer()
						goto continue
					end
				end
				-- else short comment
				while c ~= EOF and not isnewline(c) do
					nextchar()
				end
				goto continue
			elseif c == b'[' then
				local sep = skip_sep()
				if sep >= 0 then
					local s = lex_long_string(true, sep)
					return 'string', s
				elseif sep == -1 then return '['
				else lexerror('invalid long string delimiter', 'string') end
			elseif c == b'=' then
				nextchar()
				if c ~= b'=' then return '='
				else nextchar() return 'eq' end
			elseif c == b'<' then
				nextchar()
				if c ~= b'=' then return '<'
				else nextchar() return 'le' end
			elseif c == b'>' then
				nextchar()
				if c ~= b'=' then return '>'
				else nextchar() return 'ge' end
			elseif c == b'~' then
				nextchar()
				if c ~= b'=' then return '~'
				else nextchar() return 'ne' end
			elseif c == b'"' or c == b'\'' then
				return 'string', lex_string()
			elseif c == b'.' then
				save_and_nextchar()
				if check_next(b'.') then
					if check_next(b'.') then
						return 'dots'  --...
					else
						return 'concat' --..
					end
				elseif not isdigit(c) then
					return '.'
				else
					return 'number', lex_number()
				end
			elseif c == EOF then
				return 'eos'
			else
				if isspace(c) then
					assert(not isnewline(c))
					nextchar()
					goto continue
				elseif isdigit(c) then
					return 'number', lex_number()
				elseif isalpha(c) or c == '_' then
					-- identifier or reserved word
					repeat
						save_and_nextchar()
					until not (isalnum(c) or c == '_')
					local s = str(buf, len)
					if reserved[s] then  -- reserved word?
						return s
					else
						return 'name', s
					end
				else
					local c0 = c
					nextchar()
					return c0  -- single-char tokens (+ - / ...)
				end
			end
			::continue::
		end
	end

	resizebuffer(64)  -- initialize buffer
	nextchar()  -- read first char

	local lexer = {}

	local token, info
	local lookahead_token, lookahead_info = 'eos'
	local lastline = 1 -- line of last token consumed

	function lexer.next()
		lastline = linenumber
		if lookahead_token ~= 'eos' then --is there a look-ahead token?
			token, info = lookahead_token, lookahead_info --use this one
			lookahead_token, lookahead_info = 'eos' --and discharge it
		else
			token, info = lex() --read next token
		end
		return token, info, linenumber
	end

	function lexer.lookahead()
		assert(lookahead_token == 'eos')
		lookahead_token, lookahead_info = lex()
		return lookahead_token, lookahead_info, linenumber
	end

	return lexer
end

--parser ---------------------------------------------------------------------

local function parser(lexer)

	local token, info, linenumber

	local function testnext(c)
		if token == c then
			token, info, linenumber = lexer:next()
			return true
		else
			return false
		end
	end

	local function check_match(what, who, where)
		if not testnext(what) then
			if where == linenumber then
				error_expected(what)
			else
				syntaxerror(luaO_pushfstring('"%s" expected (to close "%s" at line %d)',
						luaX_token2str(ls, what), luaX_token2str(ls, who), where))
			end
		end
	end

	#define hasmultret(k)		((k) == VCALL or (k) == VVARARG)

	#define getlocvar(fs, i)	((fs).f.locvars[(fs).actvar[i]])

	--nodes for block list (list of active blocks)
	ffi.cdef[[
	typedef struct BlockCnt {
		struct BlockCnt *previous,  -- chain
		int breaklist,  -- list of jumps out of this loop
		lu_byte nactvar,  -- # active locals outside the breakable structure
		lu_byte upval,  -- true if some variable in the block is an upvalue
		lu_byte isbreakable,  -- true if `block' is a loop
	} BlockCnt;
	]]

	-- prototypes for recursive non-terminal functions
	local chunk, expr --fw. decl.

	local function anchor_token()
		if token == 'name' or token == 'string' then
		 TString *ts = ls.t.seminfo.ts
		 luaX_newstring(ls, getstr(ts), ts.tsv.len)
		end
	end


	local function error_expected(token)
		syntaxerror(luaO_pushfstring(ls.L, LUA_QS " expected", luaX_token2str(ls, token)))
	end


	local function errorlimit(fs, limit, what)
		const char *msg = (fs.f.linedefined == 0) ?
		 luaO_pushfstring(fs.L, "main function has more than %d %s", limit, what) :
		 luaO_pushfstring(fs.L, "function at line %d has more than %d %s",
										 fs.f.linedefined, limit, what)
		luaX_lexerror(fs.ls, msg, 0)
	end

	local function check(c)
		if token ~= c then
			error_expected(ls, c)
		end
	end

	local function checknext(c)
		check(ls, c)
		lexer:next()
	end

	#define check_condition(ls,c,msg)	{ if (!(c)) syntaxerror(msg) }

	local function str_checkname()
		TString *ts
		check(ls, TK_NAME)
		ts = ls.t.seminfo.ts
		lexer:next()
		return ts
	end


	local function init_exp(e, k, i) then
		e.f = e.t = NO_JUMP
		e.k = k
		e.u.s.info = i
	end

	local function codestring(e, s)
		init_exp(e, VK, luaK_stringK(ls.fs, s))
	end

	local function checkname(e)
		codestring(ls, e, str_checkname(ls))
	end

	local function registerlocalvar(varname)
		FuncState *fs = ls.fs
		Proto *f = fs.f
		int oldsize = f.sizelocvars
		luaM_growvector(ls.L, f.locvars, fs.nlocvars, f.sizelocvars,
							LocVar, SHRT_MAX, "too many local variables")
		while (oldsize < f.sizelocvars) do
			f.locvars[oldsize++].varname = NULL
		end
		f.locvars[fs.nlocvars].varname = varname
		luaC_objbarrier(ls.L, f, varname)
		return fs.nlocvars++
	end

	#define new_localvarliteral(ls,v,n) \
		new_localvar(ls, luaX_newstring(ls, "" v, (sizeof(v)/sizeof(char))-1), n)


	local function new_localvar(name, n)
		FuncState *fs = ls.fs
		fs.actvar[fs.nactvar+n] = cast(unsigned short, registerlocalvar(ls, name))
	end


	local function adjustlocalvars(nvars) then
		FuncState *fs = ls.fs
		fs.nactvar = cast_byte(fs.nactvar + nvars)
		for ( nvars nvars--) then
		 getlocvar(fs, fs.nactvar - nvars).startpc = fs.pc
		end
	end

	local function removevars (tolevel) then
		FuncState *fs = ls.fs
		while (fs.nactvar > tolevel) do
		 fs.nactvar = fs.nactvar - 1
		 getlocvar(fs, fs.nactvar).endpc = fs.pc
		end
	end

	local function indexupvalue(fs, name, v)
		int i
		Proto *f = fs.f
		int oldsize = f.sizeupvalues
		for (i=0 i<f.nups i++) do
		 if (fs.upvalues[i].k == v.k and fs.upvalues[i].info == v.u.s.info) then
			assert(f.upvalues[i] == name)
			return i
		 end
		end
		-- new one
		luaM_growvector(fs.L, f.upvalues, f.nups, f.sizeupvalues,
							TString *, MAX_INT, "")
		while (oldsize < f.sizeupvalues) do
			f.upvalues[oldsize++] = NULL
		end
		f.upvalues[f.nups] = name
		luaC_objbarrier(fs.L, f, name)
		assert(v.k == VLOCAL or v.k == VUPVAL)
		fs.upvalues[f.nups].k = cast_byte(v.k)
		fs.upvalues[f.nups].info = cast_byte(v.u.s.info)
		return f.nups++
	end

	local function searchvar(fs, n)
		int i
		for i = fs.nactvar-1, 0, -1 do
		 if n == getlocvar(fs, i).varname then
			return i
		end
		return -1  -- not found
	end

	local function markupval(fs, level)
		BlockCnt *bl = fs.bl
		while (bl and bl.nactvar > level) do
			bl = bl.previous
		end
		if (bl) then bl.upval = 1 end
	end

	local function singlevaraux(fs, n, var, base)
		if (fs == NULL) then  -- no more levels?
		 init_exp(var, VGLOBAL, NO_REG)  -- default is global variable
		 return VGLOBAL
		end
		else then
		 int v = searchvar(fs, n)  -- look up at current level
		 if (v >= 0) then
			init_exp(var, VLOCAL, v)
			if (!base)
				markupval(fs, v)  -- local will be used as an upval
			return VLOCAL
		 end
		 else then  -- not found at current level try upper one
			if (singlevaraux(fs.prev, n, var, 0) == VGLOBAL)
				return VGLOBAL
			var.u.s.info = indexupvalue(fs, n, var)  -- else was LOCAL or UPVAL
			var.k = VUPVAL  -- upvalue in this level
			return VUPVAL
		 end
		end
	end

	local function singlevar(var)
		TString *varname = str_checkname(ls)
		FuncState *fs = ls.fs
		if (singlevaraux(fs, varname, var, 1) == VGLOBAL) then
			var.u.s.info = luaK_stringK(fs, varname)  -- info points to global name
		end
	end

	local function adjust_assign(nvars, nexps, e)
		FuncState *fs = ls.fs
		int extra = nvars - nexps
		if (hasmultret(e.k)) then
		 extra++  -- includes call itself
		 if (extra < 0) extra = 0
		 luaK_setreturns(fs, e, extra)  -- last exp. provides the difference
		 if (extra > 1) luaK_reserveregs(fs, extra-1)
		end
		else then
		 if (e.k ~= VVOID) luaK_exp2nextreg(fs, e)  -- close last expression
		 if (extra > 0) then
			int reg = fs.freereg
			luaK_reserveregs(fs, extra)
			luaK_nil(fs, reg, extra)
		 end
		end
	end


	local function enterblock(fs, bl, isbreakable)
		bl.breaklist = NO_JUMP
		bl.isbreakable = isbreakable
		bl.nactvar = fs.nactvar
		bl.upval = 0
		bl.previous = fs.bl
		fs.bl = bl
		assert(fs.freereg == fs.nactvar)
	end

	local function leaveblock(fs)
		BlockCnt *bl = fs.bl
		fs.bl = bl.previous
		removevars(fs.ls, bl.nactvar)
		if (bl.upval)
		 luaK_codeABC(fs, OP_CLOSE, bl.nactvar, 0, 0)
		-- a block either controls scope or breaks (never both)
		assert(!bl.isbreakable or !bl.upval)
		assert(bl.nactvar == fs.nactvar)
		fs.freereg = fs.nactvar  -- free registers
		luaK_patchtohere(fs, bl.breaklist)
	end


	local function pushclosure(func, v)
		FuncState *fs = ls.fs
		Proto *f = fs.f
		int oldsize = f.sizep
		int i
		luaM_growvector(ls.L, f.p, fs.np, f.sizep, Proto *,
							MAXARG_Bx, "constant table overflow")
		while (oldsize < f.sizep) do
			f.p[oldsize++] = NULL
		end
		f.p[fs.np++] = func.f
		luaC_objbarrier(ls.L, f, func.f)
		init_exp(v, VRELOCABLE, luaK_codeABx(fs, OP_CLOSURE, 0, fs.np-1))
		for (i=0 i<func.f.nups i++) then
			OpCode o = (func.upvalues[i].k == VLOCAL) ? OP_MOVE : OP_GETUPVAL
			luaK_codeABC(fs, o, 0, func.upvalues[i].info, 0)
		end
	end

	local function open_func(fs)
		lua_State *L = ls.L
		Proto *f = luaF_newproto(L)
		fs.f = f
		fs.prev = ls.fs  -- linked list of funcstates
		fs.ls = ls
		fs.L = L
		ls.fs = fs
		fs.pc = 0
		fs.lasttarget = -1
		fs.jpc = NO_JUMP
		fs.freereg = 0
		fs.nk = 0
		fs.np = 0
		fs.nlocvars = 0
		fs.nactvar = 0
		fs.bl = NULL
		f.source = ls.source
		f.maxstacksize = 2  -- registers 0/1 are always valid
		fs.h = luaH_new(L, 0, 0)
		-- anchor table of constants and prototype (to avoid being collected)
		sethvalue2s(L, L.top, fs.h)
		incr_top(L)
		setptvalue2s(L, L.top, f)
		incr_top(L)
	end

	local function close_func()
		lua_State *L = ls.L
		FuncState *fs = ls.fs
		Proto *f = fs.f
		removevars(ls, 0)
		luaK_ret(fs, 0, 0)  -- final return
		luaM_reallocvector(L, f.code, f.sizecode, fs.pc, Instruction)
		f.sizecode = fs.pc
		luaM_reallocvector(L, f.lineinfo, f.sizelineinfo, fs.pc, int)
		f.sizelineinfo = fs.pc
		luaM_reallocvector(L, f.k, f.sizek, fs.nk, TValue)
		f.sizek = fs.nk
		luaM_reallocvector(L, f.p, f.sizep, fs.np, Proto *)
		f.sizep = fs.np
		luaM_reallocvector(L, f.locvars, f.sizelocvars, fs.nlocvars, LocVar)
		f.sizelocvars = fs.nlocvars
		luaM_reallocvector(L, f.upvalues, f.sizeupvalues, f.nups, TString *)
		f.sizeupvalues = f.nups
		assert(luaG_checkcode(f))
		assert(fs.bl == NULL)
		ls.fs = fs.prev
		-- last token read was anchored in defunct function must reanchor it
		if (fs) then anchor_token(ls) end
		L.top -= 2  -- remove table and prototype from the stack
	end

	local function parser()
		struct LexState lexstate
		struct FuncState funcstate
		lexstate.buff = buff
		luaX_setinput(L, &lexstate, z, luaS_new(L, name))
		open_func(&lexstate, &funcstate)
		funcstate.f.is_vararg = VARARG_ISVARARG  -- main func. is always vararg
		luaX_next(&lexstate)  -- read first token
		chunk(&lexstate)
		check(&lexstate, TK_EOS)
		close_func(&lexstate)
		assert(funcstate.prev == NULL)
		assert(funcstate.f.nups == 0)
		assert(lexstate.fs == NULL)
		return funcstate.f
	end

	-- GRAMMAR RULES ----------------------------------------------------------

	local function field(v) -- ['.' | ':'] NAME
		FuncState *fs = ls.fs
		expdesc key
		luaK_exp2anyreg(fs, v)
		lexer:next()  -- skip the dot or colon
		checkname(ls, &key)
		luaK_indexed(fs, v, &key)
	end

	local function yindex(v)  -- '[' expr ']'
		lexer:next()  -- skip the '['
		expr(ls, v)
		luaK_exp2val(ls.fs, v)
		checknext(ls, ']')
	end

	-- Rules for Constructors -------------------------------------------------

	ffi.cdef[[
	struct ConsControl {
		expdesc v  -- last list item read
		expdesc *t  -- table descriptor
		int nh  -- total number of `record' elements
		int na  -- total number of array elements
		int tostore  -- number of array elements pending to be stored
	};
	]]

	local function recfield(cc) -- (NAME | `['exp1`]') = exp1
		FuncState *fs = ls.fs
		int reg = ls.fs.freereg
		expdesc key, val
		int rkkey
		if token == 'name' then
			checkname(ls, &key)
		else -- token == '['
			yindex(ls, &key)
		end
		cc.nh++
		checknext(ls, '=')
		rkkey = luaK_exp2RK(fs, &key)
		expr(ls, &val)
		luaK_codeABC(fs, OP_SETTABLE, cc.t.u.s.info, rkkey, luaK_exp2RK(fs, &val))
		fs.freereg = reg  -- free registers
	end


	local function closelistfield(fs, cc)
		if cc.v.k == VVOID then return end -- there is no list item
		luaK_exp2nextreg(fs, &cc.v)
		cc.v.k = VVOID
		if (cc.tostore == LFIELDS_PER_FLUSH) then
		 luaK_setlist(fs, cc.t.u.s.info, cc.na, cc.tostore)  -- flush
		 cc.tostore = 0  -- no more items pending
		end
	end


	local function lastlistfield(fs, cc)
		if cc.tostore == 0 then return end
		if hasmultret(cc.v.k) then
		 luaK_setmultret(fs, &cc.v)
		 luaK_setlist(fs, cc.t.u.s.info, cc.na, LUA_MULTRET)
		 cc.na--  -- do not count last expression (unknown number of elements)
		else
		 if cc.v.k ~= VVOID then
			luaK_exp2nextreg(fs, &cc.v)
		 end
		 luaK_setlist(fs, cc.t.u.s.info, cc.na, cc.tostore)
		end
	end

	local function listfield(cc)
		expr(ls, &cc.v)
		cc.na++
		cc.tostore++
	end

	local function constructor(t) -- ??
		FuncState *fs = ls.fs
		int line = ls.linenumber
		int pc = luaK_codeABC(fs, OP_NEWTABLE, 0, 0, 0)
		struct ConsControl cc
		cc.na = cc.nh = cc.tostore = 0
		cc.t = t
		init_exp(t, VRELOCABLE, pc)
		init_exp(&cc.v, VVOID, 0)  -- no value (yet)
		luaK_exp2nextreg(ls.fs, t)  -- fix it at stack top (for gc)
		checknext(ls, 'then')
		repeat
		 assert(cc.v.k == VVOID or cc.tostore > 0)
		 if (token == 'end') break
		 closelistfield(fs, &cc)
		 switch(token) then
			case TK_NAME: then  -- may be listfields or recfields
				luaX_lookahead(ls)
				if (ls.lookahead.token ~= '=')  -- expression?
				 listfield(ls, &cc)
				else
				 recfield(ls, &cc)
				break
			end
			case '[': then  -- constructor_item . recfield
				recfield(ls, &cc)
				break
			end
			default: then  -- constructor_part . listfield
				listfield(ls, &cc)
				break
			end
		 end
		until not (testnext',' or testnext'')
		check_match(ls, 'end', 'then', line)
		lastlistfield(fs, &cc)
		SETARG_B(fs.f.code[pc], luaO_int2fb(cc.na)) -- set initial array size
		SETARG_C(fs.f.code[pc], luaO_int2fb(cc.nh))  -- set initial table size
	end

	local function parlist()  -- [ param { `,' param } ]
		FuncState *fs = ls.fs
		Proto *f = fs.f
		int nparams = 0
		f.is_vararg = 0
		if token ~= ')' then  -- is `parlist' not empty?
			repeat
				if token == 'name' then -- NAME
					new_localvar(ls, str_checkname(ls), nparams++)
				elseif token == 'dots' then  -- `...'
					lexer:next()
					f.is_vararg |= VARARG_ISVARARG
				else
					syntaxerror("<name> or " LUA_QL("...") " expected")
				end
			until not (!f.is_vararg and testnext(ls, ','))
		end
		adjustlocalvars(ls, nparams)
		f.numparams = cast_byte(fs.nactvar - (f.is_vararg & VARARG_HASARG))
		luaK_reserveregs(fs, fs.nactvar)  -- reserve register for parameters
	end

	local function body(e, needself, line) -- `(' parlist `)' chunk END
		FuncState new_fs
		open_func(ls, &new_fs)
		new_fs.f.linedefined = line
		checknext(ls, '(')
		if needself then
			new_localvarliteral(ls, "self", 0)
			adjustlocalvars(ls, 1)
		end
		parlist(ls)
		checknext(ls, ')')
		chunk(ls)
		new_fs.f.lastlinedefined = ls.linenumber
		check_match(ls, TK_END, TK_FUNCTION, line)
		close_func(ls)
		pushclosure(ls, &new_fs, e)
	end

	local function explist1(v) -- expr { `,' expr }
		local n = 1  -- at least one expression
		expr(ls, v)
		while testnext',' then
			luaK_exp2nextreg(ls.fs, v)
			expr(ls, v)
			n = n + 1
		end
		return n
	end

	local function funcargs(f)
		FuncState *fs = ls.fs
		expdesc args
		int base, nparams
		int line = ls.linenumber
		if token == '(' then  -- `(' [ explist1 ] `)'
			if (line ~= ls.lastline)
				syntaxerror(ls,"ambiguous syntax (function call x new statement)")
			lexer:next()
			if (token == ')')  -- arg list is empty?
				args.k = VVOID
			else then
				explist1(ls, &args)
				luaK_setmultret(fs, &args)
			end
			check_match(ls, ')', '(', line)
			break
		elseif token == 'then' then  -- constructor
			constructor(ls, &args)
			break
		elseif token == 'string' then  -- STRING
			codestring(ls, &args, ls.t.seminfo.ts)
			lexer:next()  -- must use `seminfo' before `next'
			break
		else
			syntaxerror("function arguments expected")
		end
		assert(f.k == VNONRELOC)
		base = f.u.s.info  -- base register for call
		if hasmultret(args.k) then
			nparams = LUA_MULTRET  -- open call
		elseif args.k ~= VVOID then
			luaK_exp2nextreg(fs, &args)  -- close last argument
			nparams = fs.freereg - (base+1)
		end
		init_exp(f, VCALL, luaK_codeABC(fs, OP_CALL, base, nparams+1, 2))
		luaK_fixline(fs, line)
		fs.freereg = base+1  -- call remove function and arguments and leaves (unless changed) one result
	end

	-- Expression parsing -----------------------------------------------------

	local function prefixexp(v) -- NAME | '(' expr ')'
		if token == '(' then
			int line = ls.linenumber
			lexer:next()
			expr(ls, v)
			check_match(ls, ')', '(', line)
			luaK_dischargevars(ls.fs, v)
		elseif token == 'name' then
			singlevar(ls, v)
		else
			syntaxerror("unexpected symbol")
		end
	end


	-- primaryexp -> prefixexp { `.' NAME | `[' exp `]' | `:' NAME funcargs | funcargs }
	local function primaryexp(v)
		FuncState *fs = ls.fs
		prefixexp(ls, v)
		while true do
			if token == '.' then  -- field
				field(ls, v)
				break
			elseif token == '[' then  -- `[' exp1 `]'
				expdesc key
				luaK_exp2anyreg(fs, v)
				yindex(ls, &key)
				luaK_indexed(fs, v, &key)
				break
			elseif token == ':' then  -- `:' NAME funcargs
				expdesc key
				lexer:next()
				checkname(ls, &key)
				luaK_self(fs, v, &key)
				funcargs(ls, v)
				break
			elseif token == '(' or token == 'string' or token == 'then' then --funcargs
				luaK_exp2nextreg(fs, v)
				funcargs(ls, v)
				break
			else
				return
			end
		end
	end

	-- NUMBER | STRING | NIL | true | false | ... | constructor | FUNCTION body | primaryexp
	local function simpleexp(v)
		if token == 'number' then
			init_exp(v, VKNUM, 0)
			v.u.nval = ls.t.seminfo.r
		elseif token == 'string' then
			codestring(ls, v, ls.t.seminfo.ts)
		elseif token == 'nil' then
			init_exp(v, VNIL, 0)
		elseif token == 'true' then
			init_exp(v, VTRUE, 0)
		elseif token == 'false' then
			init_exp(v, VFALSE, 0)
		elseif token == 'dots' then  -- vararg
			FuncState *fs = ls.fs
			check_condition(ls, fs.f.is_vararg,
								 "cannot use " LUA_QL("...") " outside a vararg function")
			fs.f.is_vararg &= ~VARARG_NEEDSARG  -- don't need 'arg'
			init_exp(v, VVARARG, luaK_codeABC(fs, OP_VARARG, 0, 1, 0))
		elseif token == '{' then -- constructor
			constructor(ls, v)
			return
		elseif token == 'function' then
			lexer:next()
			body(ls, v, 0, ls.linenumber)
			return
		else
			primaryexp(ls, v)
			return
		end
		lexer:next()
	end

	local function getunopr(op)
		if op == 'not' then
			return 'not' --omonym but kept for validation
		elseif op == '-' then
			return 'minus'
		elseif op == '#' then
			return 'len'
		end
	end

	local ops = {
		['+'] = 'add',
		['-'] = 'sub',
		['*'] = 'mul',
		['/'] = 'div',
		['%'] = 'mod',
		['^'] = 'pow',
		['<'] = 'lt',
		['>'] = 'gt',
		--omonyms, but kept for validation
		concat = 'concat',
		ne = 'ne',
		eq = 'eq',
		le = 'le',
		ge = 'ge',
		['and'] = 'and',
		['or'] = 'or',
	}
	local function getbinopr(op)
		return ops[op] or op
	end

	ffi.cdef[[
	static const struct {
		lu_byte left;  /* left priority for each binary operator */
		lu_byte right; /* right priority */
	} priority[] = {  /* ORDER OPR */
		{6, 6}, {6, 6}, {7, 7}, {7, 7}, {7, 7},  /* `+' `-' `/' `%' */
		{10, 9}, {5, 4},                 /* power and concat (right associative) */
		{3, 3}, {3, 3},                  /* equality and inequality */
		{3, 3}, {3, 3}, {3, 3}, {3, 3},  /* order */
		{2, 2}, {1, 1}                   /* logical (and/or) */
	};
	]]

	local UNARY_PRIORITY = 8 -- priority for unary operators

	-- subexpr . (simpleexp | unop subexpr) then binop subexpr end
	-- where `binop' is any binary operator with a priority higher than `limit'

	local function subexpr(v, limit)
		local op
		local uop = getunopr(token)
		if uop then
			lexer:next()
			subexpr(ls, v, UNARY_PRIORITY)
			luaK_prefix(ls.fs, uop, v)
		else
			simpleexp(ls, v)
		end
		-- expand while operators have priorities higher than `limit'
		op = getbinopr(token)
		while op ~= OPR_NOBINOPR and priority[op].left > limit then
			expdesc v2
			BinOpr nextop
			lexer:next()
			luaK_infix(ls.fs, op, v)
			-- read sub-expression with higher priority
			nextop = subexpr(ls, &v2, priority[op].right)
			luaK_posfix(ls.fs, op, v, &v2)
			op = nextop
		end
		return op  -- return first untreated operator
	end

	local function expr(v)
		subexpr(ls, v, 0)
	end

	--Rules for statements ----------------------------------------------------

	local function block() -- chunk
		FuncState *fs = ls.fs
		BlockCnt bl
		enterblock(fs, &bl, 0)
		chunk(ls)
		assert(bl.breaklist == NO_JUMP)
		leaveblock(fs)
	end


	-- structure to chain all variables in the left-hand side of an
	-- assignment

	--LHS_assign {
	--	prev
	--	v  -- expdesc: variable (global, local, upvalue, or indexed)
	--}


	-- check whether, in an assignment to a local variable, the local variable
	-- is needed in a previous assignment (to a table). If so, save original
	-- local value in a safe place and use this safe copy in the previous
	-- assignment.

	local function check_conflict(lh, v)
		FuncState *fs = ls.fs
		int extra = fs.freereg  -- eventual position to save local variable
		int conflict = 0
		for ( lh lh = lh.prev) then
		 if (lh.v.k == VINDEXED) then
			if (lh.v.u.s.info == v.u.s.info) then  -- conflict?
				conflict = 1
				lh.v.u.s.info = extra  -- previous assignment will use safe copy
			end
			if (lh.v.u.s.aux == v.u.s.info) then  -- conflict?
				conflict = 1
				lh.v.u.s.aux = extra  -- previous assignment will use safe copy
			end
		 end
		end
		if (conflict) then
		 luaK_codeABC(fs, OP_MOVE, fs.freereg, v.u.s.info, 0)  -- make copy
		 luaK_reserveregs(fs, 1)
		end
	end


	local function assignment(lh, nvars)
		expdesc e
		check_condition(ls, VLOCAL <= lh.v.k and lh.v.k <= VINDEXED,
								 "syntax error")
		if (testnext(ls, ',')) then  -- assignment . `,' primaryexp assignment
		 struct LHS_assign nv
		 nv.prev = lh
		 primaryexp(ls, &nv.v)
		 if (nv.v.k == VLOCAL)
			check_conflict(ls, lh, &nv.v)
		 assignment(ls, &nv, nvars+1)
		end
		else then  -- assignment . `=' explist1
		 int nexps
		 checknext(ls, '=')
		 nexps = explist1(ls, &e)
		 if (nexps ~= nvars) then
			adjust_assign(ls, nvars, nexps, &e)
			if (nexps > nvars)
				ls.fs.freereg -= nexps - nvars  -- remove extra values
		 end
		 else then
			luaK_setoneret(ls.fs, &e)  -- close last expression
			luaK_storevar(ls.fs, &lh.v, &e)
			return  -- avoid default
		 end
		end
		init_exp(&e, VNONRELOC, ls.fs.freereg-1)  -- default assignment
		luaK_storevar(ls.fs, &lh.v, &e)
	end


	local function cond() -- exp
		expdesc v
		expr(ls, &v)  -- read condition
		if (v.k == VNIL) v.k = VFALSE  -- `falses' are all equal here
		luaK_goiftrue(ls.fs, &v)
		return v.f
	end


	local function breakstat()
		FuncState *fs = ls.fs
		BlockCnt *bl = fs.bl
		int upval = 0
		while (bl and !bl.isbreakable) then
		 upval |= bl.upval
		 bl = bl.previous
		end
		if (!bl)
		 syntaxerror("no loop to break")
		if (upval)
		 luaK_codeABC(fs, OP_CLOSE, bl.nactvar, 0, 0)
		luaK_concat(fs, &bl.breaklist, luaK_jump(fs))
	end


	local function whilestat(line) -- WHILE cond DO block END
		FuncState *fs = ls.fs
		int whileinit
		int condexit
		BlockCnt bl
		lexer:next()  -- skip WHILE
		whileinit = luaK_getlabel(fs)
		condexit = cond(ls)
		enterblock(fs, &bl, 1)
		checknext(ls, TK_DO)
		block(ls)
		luaK_patchlist(fs, luaK_jump(fs), whileinit)
		check_match(ls, TK_END, TK_WHILE, line)
		leaveblock(fs)
		luaK_patchtohere(fs, condexit)  -- false conditions finish the loop
	end


	local function repeatstat (int line) then -- REPEAT block UNTIL cond
		int condexit
		FuncState *fs = ls.fs
		int repeat_init = luaK_getlabel(fs)
		BlockCnt bl1, bl2
		enterblock(fs, &bl1, 1)  -- loop block
		enterblock(fs, &bl2, 0)  -- scope block
		lexer:next()  -- skip REPEAT
		chunk(ls)
		check_match(ls, TK_UNTIL, TK_REPEAT, line)
		condexit = cond(ls)  -- read condition (inside scope block)
		if (!bl2.upval) then  -- no upvalues?
		 leaveblock(fs)  -- finish scope
		 luaK_patchlist(ls.fs, condexit, repeat_init)  -- close the loop
		end
		else then  -- complete semantics when there are upvalues
		 breakstat(ls)  -- if condition then break
		 luaK_patchtohere(ls.fs, condexit)  -- else...
		 leaveblock(fs)  -- finish scope...
		 luaK_patchlist(ls.fs, luaK_jump(fs), repeat_init)  -- and repeat
		end
		leaveblock(fs)  -- finish loop
	end

	local function exp1()
		expdesc e
		int k
		expr(ls, &e)
		k = e.k
		luaK_exp2nextreg(ls.fs, &e)
		return k
	end

	local function forbody(base, line, nvars, isnum) -- forbody -> DO block
		BlockCnt bl
		FuncState *fs = ls.fs
		int prep, endfor
		adjustlocalvars(ls, 3)  -- control variables
		checknext(ls, TK_DO)
		prep = isnum ? luaK_codeAsBx(fs, OP_FORPREP, base, NO_JUMP) : luaK_jump(fs)
		enterblock(fs, &bl, 0)  -- scope for declared variables
		adjustlocalvars(ls, nvars)
		luaK_reserveregs(fs, nvars)
		block(ls)
		leaveblock(fs)  -- end of scope for declared variables
		luaK_patchtohere(fs, prep)
		endfor = (isnum) ? luaK_codeAsBx(fs, OP_FORLOOP, base, NO_JUMP) :
								luaK_codeABC(fs, OP_TFORLOOP, base, 0, nvars)
		luaK_fixline(fs, line)  -- pretend that `OP_FOR' starts the loop
		luaK_patchlist(fs, (isnum ? endfor : luaK_jump(fs)), prep + 1)
	end


	-- fornum -> NAME = exp1,exp1[,exp1] forbody
	local function fornum(varname, line)
		FuncState *fs = ls.fs
		int base = fs.freereg
		new_localvarliteral(ls, "(for index)", 0)
		new_localvarliteral(ls, "(for limit)", 1)
		new_localvarliteral(ls, "(for step)", 2)
		new_localvar(ls, varname, 3)
		checknext(ls, '=')
		exp1(ls)  -- initial value
		checknext(ls, ',')
		exp1(ls)  -- limit
		if (testnext(ls, ','))
		 exp1(ls)  -- optional step
		else then  -- default step = 1
		 luaK_codeABx(fs, OP_LOADK, fs.freereg, luaK_numberK(fs, 1))
		 luaK_reserveregs(fs, 1)
		end
		forbody(ls, base, line, 1, 1)
	end


	-- forlist -> NAME {,NAME} IN explist1 forbody
	local function forlist(indexname)
		FuncState *fs = ls.fs
		expdesc e
		int nvars = 0
		int line
		int base = fs.freereg
		-- create control variables
		new_localvarliteral(ls, "(for generator)", nvars++)
		new_localvarliteral(ls, "(for state)", nvars++)
		new_localvarliteral(ls, "(for control)", nvars++)
		-- create declared variables
		new_localvar(ls, indexname, nvars++)
		while (testnext(ls, ','))
		 new_localvar(ls, str_checkname(ls), nvars++)
		checknext(ls, TK_IN)
		line = ls.linenumber
		adjust_assign(ls, 3, explist1(ls, &e), &e)
		luaK_checkstack(fs, 3)  -- extra space to call generator
		forbody(ls, base, line, nvars - 3, 0)
	end


	local function forstat(line) -- forstat -> FOR (fornum | forlist) END
		FuncState *fs = ls.fs
		TString *varname
		BlockCnt bl
		enterblock(fs, &bl, 1)  -- scope for loop and control variables
		lexer:next()  -- skip `for'
		varname = str_checkname(ls)  -- first variable name
		switch (token) then
		 case '=': fornum(ls, varname, line) break
		 case ',': case TK_IN: forlist(ls, varname) break
		 default: syntaxerror(LUA_QL("=") " or " LUA_QL("in") " expected")
		end
		check_match(ls, TK_END, TK_FOR, line)
		leaveblock(fs)  -- loop scope (`break' jumps to this point)
	end


	-- test_then_block -> [IF | ELSEIF] cond THEN block
	local function test_then_block()
		int condexit
		lexer:next()  -- skip IF or ELSEIF
		condexit = cond(ls)
		checknext(ls, TK_THEN)
		block(ls)  -- `then' part
		return condexit
	end


	-- ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END
	local function ifstat(line)
		FuncState *fs = ls.fs
		int flist
		int escapelist = NO_JUMP
		flist = test_then_block(ls)  -- IF cond THEN block
		while (token == TK_ELSEIF) then
		 luaK_concat(fs, &escapelist, luaK_jump(fs))
		 luaK_patchtohere(fs, flist)
		 flist = test_then_block(ls)  -- ELSEIF cond THEN block
		end
		if (token == TK_ELSE) then
		 luaK_concat(fs, &escapelist, luaK_jump(fs))
		 luaK_patchtohere(fs, flist)
		 lexer:next()  -- skip ELSE (after patch, for correct line info)
		 block(ls)  -- `else' part
		end
		else
		 luaK_concat(fs, &escapelist, flist)
		luaK_patchtohere(fs, escapelist)
		check_match(ls, TK_END, TK_IF, line)
	end


	local function localfunc() then
		expdesc v, b
		FuncState *fs = ls.fs
		new_localvar(ls, str_checkname(ls), 0)
		init_exp(&v, VLOCAL, fs.freereg)
		luaK_reserveregs(fs, 1)
		adjustlocalvars(ls, 1)
		body(ls, &b, 0, ls.linenumber)
		luaK_storevar(fs, &v, &b)
		-- debug information will only see the variable after this point!
		getlocvar(fs, fs.nactvar - 1).startpc = fs.pc
	end


	local function localstat() -- stat -> LOCAL NAME {`,' NAME} [`=' explist1]
		int nvars = 0
		int nexps
		expdesc e
		do then
		 new_localvar(ls, str_checkname(ls), nvars++)
		end while (testnext(ls, ','))
		if (testnext(ls, '='))
		 nexps = explist1(ls, &e)
		else then
		 e.k = VVOID
		 nexps = 0
		end
		adjust_assign(ls, nvars, nexps, &e)
		adjustlocalvars(ls, nvars)
	end


	local function funcname(v) -- funcname -> NAME thenfieldend [`:' NAME]
		int needself = 0
		singlevar(ls, v)
		while (token == '.')
		 field(ls, v)
		if (token == ':') then
		 needself = 1
		 field(ls, v)
		end
		return needself
	end


	local function funcstat(line) -- funcstat -> FUNCTION funcname body
		int needself
		expdesc v, b
		lexer:next()  -- skip FUNCTION
		needself = funcname(ls, &v)
		body(ls, &b, needself, line)
		luaK_storevar(ls.fs, &v, &b)
		luaK_fixline(ls.fs, line)  -- definition `happens' in the first line
	end


	local function exprstat() -- stat -> func | assignment
		FuncState *fs = ls.fs
		struct LHS_assign v
		primaryexp(ls, &v.v)
		if (v.v.k == VCALL)  -- stat . func
		 SETARG_C(getcode(fs, &v.v), 1)  -- call statement uses no results
		else then  -- stat . assignment
		 v.prev = NULL
		 assignment(ls, &v, 1)
		end
	end

	local function block_follow(token)
		return token == 'else' or token == 'elseif' or token == 'end'
			or token == 'until' or token == 'eos'
	end

	local function retstat() -- stat -> RETURN explist
		local e --expdesc
		local first, nret  -- registers with returned values
		lexer:next() -- skip RETURN
		if block_follow(token) or token == ';' then
			first = 0
			nret = 0  -- return no values
		else
			nret = explist1(e)  -- optional return values
			if hasmultret(e.k) then
				luaK_setmultret(fs, e)
				if e.k == VCALL and nret == 1 then  -- tail call?
					--
				end
				first = fs.nactvar
				nret = LUA_MULTRET  -- return all values
			else
				if (nret == 1)  -- only one single value?
					first = luaK_exp2anyreg(fs, &e)
				else
					luaK_exp2nextreg(fs, &e)  -- values must go to the `stack'
					first = fs.nactvar  -- return all `active' values
					assert(nret == fs.freereg - first)
				end
			end
		end
		luaK_ret(fs, first, nret)
	end

	local function statement()
		local line = linenumber  -- may be needed for error messages
		if token == 'if' then  -- stat -> ifstat
			ifstat(ls, line)
			return false
		elseif token == 'while' then -- stat -> whilestat
			whilestat(ls, line)
			return false
		elseif token == 'do' then  -- stat -> DO block END
			lexer:next()  -- skip DO
			block()
			check_match('end', 'do', line)
			return false
		elseif token == 'for' then  -- stat -> forstat
			forstat(line)
			return false
		elseif token == 'repeat' then  -- stat -> repeatstat
			repeatstat(line)
			return false
		elseif token == 'function' then
			funcstat(line)  -- stat -> funcstat
			return false
		elseif token == 'local' then  -- stat -> localstat
			lexer:next()  -- skip LOCAL
			if testnext'function' then -- local function?
				localfunc()
			else
				localstat()
			end
			return false
		elseif token == 'return' then -- stat -> retstat
			retstat()
			return true  -- must be last statement
		elseif token == 'break' then  -- stat -> breakstat
			lexer:next()  -- skip BREAK
			breakstat()
			return true  -- must be last statement
		else
			exprstat()
			return false  -- to avoid warnings
		end
	end

	local function chunk() -- { stat [`;'] }
		local islast = 0
		while not islast and not block_follow(token) do
			islast = statement(ls)
			testnext(';')
		end
	end

end


if not ... then

	local fs = require'fs'
	local time = require'time'

	local clock = time.clock()
	local total_size = 0

	local function lex(file)
		local f = assert(fs.open(file))
		local bufread = f:buffered_read()
		local function read(buf, sz)
			--return assert(bufread(buf, sz))
			return assert(f:read(buf, sz))
		end
		local lexer = lexer(read, file)

		while true do
			local token, info, linenumber = lexer.next()
			if token == 'eos' then break end
		end

		f:close()
	end

	local files = 0
	for f,d in fs.dir() do
		if d:is'file' and f:find'%.lua$' then
			if f ~= 'harfbuzz_ot_demo.lua' and not f:find'^_' then
				print(f)
				total_size = total_size + d:attr'size'
				files = files + 1
				--os.execute([[bin\mingw64\luajit.exe -b ]]..f..' _'..f)
				local ok, err
				if false then
					ok, err = loadfile(f)
				else
					ok, err = xpcall(lex, debug.traceback, f)
				end
				if not ok then
					print(f, ok, err)
					break
				end
			end
		end
	end

	local size = total_size / 1024 / 1024
	local duration = time.clock() - clock
	local speed = size / duration
	print(string.format('%d files, %.1f MB, %.1fs, %d MB/s',
		files, size, duration, speed))


	return parser
end


return {
	lexer = lexer,
}
