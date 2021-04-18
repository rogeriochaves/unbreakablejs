(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (!x.$)
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}



// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800)
			+
			String.fromCharCode(code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList === 'function' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		(key !== 'value' || key !== 'checked' || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		value
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		value
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			var oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			var newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}



// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return elm$core$Maybe$Nothing;
	}
}


var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Decode$bool = _Json_decodeBool;
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$int = _Json_decodeInt;
var elm$json$Json$Decode$map2 = _Json_map2;
var author$project$Playground$keyCodeWithShift = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (shift, keyCode) {
			return shift ? elm$core$Maybe$Just(keyCode) : elm$core$Maybe$Nothing;
		}),
	A2(elm$json$Json$Decode$field, 'shiftKey', elm$json$Json$Decode$bool),
	A2(elm$json$Json$Decode$field, 'keyCode', elm$json$Json$Decode$int));
var author$project$Playground$Types$KeyDown = function (a) {
	return {$: 'KeyDown', a: a};
};
var author$project$Playground$Types$SelectCell = function (a) {
	return {$: 'SelectCell', a: a};
};
var author$project$Playground$Types$UpdateInput = F2(
	function (a, b) {
		return {$: 'UpdateInput', a: a, b: b};
	});
var elm$json$Json$Encode$string = _Json_wrap;
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$id = elm$html$Html$Attributes$stringProperty('id');
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'focus',
		elm$json$Json$Decode$succeed(msg));
};
var ohanhi$autoexpand$AutoExpand$Config = function (a) {
	return {$: 'Config', a: a};
};
var ohanhi$autoexpand$AutoExpand$config = function (values) {
	return ohanhi$autoexpand$AutoExpand$Config(
		{attributes: _List_Nil, _class: elm$core$Maybe$Nothing, id: elm$core$Maybe$Nothing, lineHeight: values.lineHeight, maxRows: values.maxRows, minRows: values.minRows, onInput: values.onInput, padding: values.padding, placeholder: elm$core$Maybe$Nothing});
};
var ohanhi$autoexpand$AutoExpand$withAttribute = F2(
	function (newAttribute, _n0) {
		var configInternal = _n0.a;
		return ohanhi$autoexpand$AutoExpand$Config(
			_Utils_update(
				configInternal,
				{
					attributes: A2(elm$core$List$cons, newAttribute, configInternal.attributes)
				}));
	});
var author$project$Playground$autoExpandConfig = F2(
	function (minRows, index) {
		return A2(
			ohanhi$autoexpand$AutoExpand$withAttribute,
			elm$html$Html$Attributes$id(
				'cellinput-' + elm$core$String$fromInt(index)),
			A2(
				ohanhi$autoexpand$AutoExpand$withAttribute,
				A2(
					elm$html$Html$Events$on,
					'keydown',
					A2(elm$json$Json$Decode$map, author$project$Playground$Types$KeyDown, author$project$Playground$keyCodeWithShift)),
				A2(
					ohanhi$autoexpand$AutoExpand$withAttribute,
					elm$html$Html$Events$onFocus(
						author$project$Playground$Types$SelectCell(index)),
					A2(
						ohanhi$autoexpand$AutoExpand$withAttribute,
						A2(elm$html$Html$Attributes$style, 'font-family', 'monospace, sans-serif'),
						A2(
							ohanhi$autoexpand$AutoExpand$withAttribute,
							A2(elm$html$Html$Attributes$style, 'font-size', '14px'),
							A2(
								ohanhi$autoexpand$AutoExpand$withAttribute,
								A2(elm$html$Html$Attributes$style, 'border', '1px solid #cfcfcf'),
								A2(
									ohanhi$autoexpand$AutoExpand$withAttribute,
									A2(elm$html$Html$Attributes$style, 'background', '#f7f7f7'),
									A2(
										ohanhi$autoexpand$AutoExpand$withAttribute,
										A2(elm$html$Html$Attributes$style, 'flex-grow', '1'),
										A2(
											ohanhi$autoexpand$AutoExpand$withAttribute,
											A2(elm$html$Html$Attributes$style, 'resize', 'none'),
											ohanhi$autoexpand$AutoExpand$config(
												{
													lineHeight: 18,
													maxRows: 50,
													minRows: minRows,
													onInput: author$project$Playground$Types$UpdateInput(index),
													padding: 5
												}))))))))));
	});
var author$project$Types$State = function (a) {
	return {$: 'State', a: a};
};
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var author$project$Types$emptyState = author$project$Types$State(elm$core$Dict$empty);
var ohanhi$autoexpand$AutoExpand$State = function (a) {
	return {$: 'State', a: a};
};
var ohanhi$autoexpand$AutoExpand$initState = function (_n0) {
	var configInternal = _n0.a;
	return ohanhi$autoexpand$AutoExpand$State(configInternal.minRows);
};
var author$project$Playground$newCell = F2(
	function (index, input) {
		return {
			autoexpand: ohanhi$autoexpand$AutoExpand$initState(
				A2(
					author$project$Playground$autoExpandConfig,
					elm$core$List$length(
						A2(elm$core$String$split, '\n', input)),
					index)),
			input: input,
			result: elm$core$Result$Ok(
				_Utils_Tuple2(author$project$Types$emptyState, elm$core$Maybe$Nothing)),
			submittedInput: ''
		};
	});
var Punie$elm_parser_extras$Parser$Expression$initOps = {lassoc: _List_Nil, nassoc: _List_Nil, postfix: _List_Nil, prefix: _List_Nil, rassoc: _List_Nil};
var Punie$elm_parser_extras$Parser$Expression$splitOp = F2(
	function (operator, ops) {
		var rassoc = ops.rassoc;
		var lassoc = ops.lassoc;
		var nassoc = ops.nassoc;
		var prefix = ops.prefix;
		var postfix = ops.postfix;
		switch (operator.$) {
			case 'Infix':
				switch (operator.b.$) {
					case 'AssocNone':
						var op = operator.a;
						var _n1 = operator.b;
						return _Utils_update(
							ops,
							{
								nassoc: A2(elm$core$List$cons, op, ops.nassoc)
							});
					case 'AssocLeft':
						var op = operator.a;
						var _n2 = operator.b;
						return _Utils_update(
							ops,
							{
								lassoc: A2(elm$core$List$cons, op, ops.lassoc)
							});
					default:
						var op = operator.a;
						var _n3 = operator.b;
						return _Utils_update(
							ops,
							{
								rassoc: A2(elm$core$List$cons, op, ops.rassoc)
							});
				}
			case 'Prefix':
				var op = operator.a;
				return _Utils_update(
					ops,
					{
						prefix: A2(elm$core$List$cons, op, ops.prefix)
					});
			default:
				var op = operator.a;
				return _Utils_update(
					ops,
					{
						postfix: A2(elm$core$List$cons, op, ops.postfix)
					});
		}
	});
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 'Good', a: a, b: b, c: c};
	});
var elm$parser$Parser$Advanced$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _n0) {
		var parseA = _n0.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n1 = parseA(s0);
				if (_n1.$ === 'Bad') {
					var p = _n1.a;
					var x = _n1.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _n1.a;
					var a = _n1.b;
					var s1 = _n1.c;
					var _n2 = callback(a);
					var parseB = _n2.a;
					var _n3 = parseB(s1);
					if (_n3.$ === 'Bad') {
						var p2 = _n3.a;
						var x = _n3.b;
						return A2(elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _n3.a;
						var b = _n3.b;
						var s2 = _n3.c;
						return A3(elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
					}
				}
			});
	});
var elm$parser$Parser$andThen = elm$parser$Parser$Advanced$andThen;
var elm$parser$Parser$Advanced$backtrackable = function (_n0) {
	var parse = _n0.a;
	return elm$parser$Parser$Advanced$Parser(
		function (s0) {
			var _n1 = parse(s0);
			if (_n1.$ === 'Bad') {
				var x = _n1.b;
				return A2(elm$parser$Parser$Advanced$Bad, false, x);
			} else {
				var a = _n1.b;
				var s1 = _n1.c;
				return A3(elm$parser$Parser$Advanced$Good, false, a, s1);
			}
		});
};
var elm$parser$Parser$backtrackable = elm$parser$Parser$Advanced$backtrackable;
var elm$parser$Parser$Advanced$map2 = F3(
	function (func, _n0, _n1) {
		var parseA = _n0.a;
		var parseB = _n1.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n2 = parseA(s0);
				if (_n2.$ === 'Bad') {
					var p = _n2.a;
					var x = _n2.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _n2.a;
					var a = _n2.b;
					var s1 = _n2.c;
					var _n3 = parseB(s1);
					if (_n3.$ === 'Bad') {
						var p2 = _n3.a;
						var x = _n3.b;
						return A2(elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _n3.a;
						var b = _n3.b;
						var s2 = _n3.c;
						return A3(
							elm$parser$Parser$Advanced$Good,
							p1 || p2,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3(elm$parser$Parser$Advanced$map2, elm$core$Basics$apL, parseFunc, parseArg);
	});
var elm$parser$Parser$keeper = elm$parser$Parser$Advanced$keeper;
var elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 'Append', a: a, b: b};
	});
var elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2(elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a.a;
				var remainingParsers = parsers.b;
				var _n1 = parse(s0);
				if (_n1.$ === 'Good') {
					var step = _n1;
					return step;
				} else {
					var step = _n1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2(elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3(elm$parser$Parser$Advanced$oneOfHelp, s, elm$parser$Parser$Advanced$Empty, parsers);
		});
};
var elm$parser$Parser$oneOf = elm$parser$Parser$Advanced$oneOf;
var elm$parser$Parser$Problem = function (a) {
	return {$: 'Problem', a: a};
};
var elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var elm$parser$Parser$Advanced$Problem = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			elm$parser$Parser$Advanced$AddRight,
			elm$parser$Parser$Advanced$Empty,
			A4(elm$parser$Parser$Advanced$Problem, s.row, s.col, x, s.context));
	});
var elm$parser$Parser$Advanced$problem = function (x) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A2(
				elm$parser$Parser$Advanced$Bad,
				false,
				A2(elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var elm$parser$Parser$problem = function (msg) {
	return elm$parser$Parser$Advanced$problem(
		elm$parser$Parser$Problem(msg));
};
var elm$parser$Parser$Advanced$succeed = function (a) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3(elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var elm$parser$Parser$succeed = elm$parser$Parser$Advanced$succeed;
var Punie$elm_parser_extras$Parser$Expression$makeParser = F2(
	function (ops, term) {
		var ambiguous = F2(
			function (assoc, op) {
				return elm$parser$Parser$backtrackable(
					A2(
						elm$parser$Parser$andThen,
						function (_n3) {
							return elm$parser$Parser$problem('ambiguous use of a ' + (assoc + ' associative operator'));
						},
						op));
			});
		var _n0 = A3(elm$core$List$foldr, Punie$elm_parser_extras$Parser$Expression$splitOp, Punie$elm_parser_extras$Parser$Expression$initOps, ops);
		var rassoc = _n0.rassoc;
		var lassoc = _n0.lassoc;
		var nassoc = _n0.nassoc;
		var prefix = _n0.prefix;
		var postfix = _n0.postfix;
		var lassocOp = elm$parser$Parser$oneOf(lassoc);
		var ambiguousLeft = A2(ambiguous, 'left', lassocOp);
		var nassocOp = elm$parser$Parser$oneOf(nassoc);
		var ambiguousNon = A2(ambiguous, 'non', nassocOp);
		var postfixOp = elm$parser$Parser$oneOf(postfix);
		var postfixP = elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					postfixOp,
					elm$parser$Parser$succeed(elm$core$Basics$identity)
				]));
		var prefixOp = elm$parser$Parser$oneOf(prefix);
		var prefixP = elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					prefixOp,
					elm$parser$Parser$succeed(elm$core$Basics$identity)
				]));
		var termP = A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				A2(
					elm$parser$Parser$keeper,
					elm$parser$Parser$succeed(
						F3(
							function (pre, x, post) {
								return post(
									pre(x));
							})),
					prefixP),
				term),
			postfixP);
		var rassocOp = elm$parser$Parser$oneOf(rassoc);
		var ambiguousRight = A2(ambiguous, 'right', rassocOp);
		var lassocP = function (x) {
			return elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						elm$parser$Parser$andThen,
						function (_n2) {
							var f = _n2.a;
							var y = _n2.b;
							return lassocP1(
								A2(f, x, y));
						},
						A2(
							elm$parser$Parser$keeper,
							A2(
								elm$parser$Parser$keeper,
								elm$parser$Parser$succeed(elm$core$Tuple$pair),
								lassocOp),
							termP)),
						ambiguousRight,
						ambiguousNon
					]));
		};
		var lassocP1 = function (x) {
			return elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						lassocP(x),
						elm$parser$Parser$succeed(x)
					]));
		};
		var nassocP = function (x) {
			return A2(
				elm$parser$Parser$andThen,
				function (_n1) {
					var f = _n1.a;
					var y = _n1.b;
					return elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								ambiguousRight,
								ambiguousLeft,
								ambiguousNon,
								elm$parser$Parser$succeed(
								A2(f, x, y))
							]));
				},
				A2(
					elm$parser$Parser$keeper,
					A2(
						elm$parser$Parser$keeper,
						elm$parser$Parser$succeed(elm$core$Tuple$pair),
						nassocOp),
					termP));
		};
		var rassocP = function (x) {
			return elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						elm$parser$Parser$keeper,
						A2(
							elm$parser$Parser$keeper,
							elm$parser$Parser$succeed(
								F2(
									function (f, y) {
										return A2(f, x, y);
									})),
							rassocOp),
						A2(elm$parser$Parser$andThen, rassocP1, termP)),
						ambiguousLeft,
						ambiguousNon
					]));
		};
		var rassocP1 = function (x) {
			return elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						rassocP(x),
						elm$parser$Parser$succeed(x)
					]));
		};
		return A2(
			elm$parser$Parser$andThen,
			function (x) {
				return elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							rassocP(x),
							lassocP(x),
							nassocP(x),
							elm$parser$Parser$succeed(x)
						]));
			},
			termP);
	});
var Punie$elm_parser_extras$Parser$Expression$buildExpressionParser = F2(
	function (operators, simpleExpr) {
		return A3(elm$core$List$foldl, Punie$elm_parser_extras$Parser$Expression$makeParser, simpleExpr, operators);
	});
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3(elm$parser$Parser$Advanced$map2, elm$core$Basics$always, keepParser, ignoreParser);
	});
var elm$parser$Parser$ignorer = elm$parser$Parser$Advanced$ignorer;
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3(elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.src);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.offset, offset) < 0,
					_Utils_Tuple0,
					{col: col, context: s0.context, indent: s0.indent, offset: offset, row: row, src: s0.src});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A5(elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.offset, s.row, s.col, s);
		});
};
var elm$parser$Parser$Advanced$spaces = elm$parser$Parser$Advanced$chompWhile(
	function (c) {
		return _Utils_eq(
			c,
			_Utils_chr(' ')) || (_Utils_eq(
			c,
			_Utils_chr('\n')) || _Utils_eq(
			c,
			_Utils_chr('\r')));
	});
var elm$parser$Parser$spaces = elm$parser$Parser$Advanced$spaces;
var Punie$elm_parser_extras$Parser$Extras$between = F3(
	function (opening, closing, p) {
		return A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$ignorer,
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$succeed(elm$core$Basics$identity),
					opening),
				elm$parser$Parser$spaces),
			A2(
				elm$parser$Parser$ignorer,
				A2(elm$parser$Parser$ignorer, p, elm$parser$Parser$spaces),
				closing));
	});
var elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 'ExpectingSymbol', a: a};
};
var elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var elm$core$Basics$not = _Basics_not;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var elm$parser$Parser$Advanced$token = function (_n0) {
	var str = _n0.a;
	var expecting = _n0.b;
	var progress = !elm$core$String$isEmpty(str);
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _n1 = A5(elm$parser$Parser$Advanced$isSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _n1.a;
			var newRow = _n1.b;
			var newCol = _n1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				elm$parser$Parser$Advanced$Bad,
				false,
				A2(elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var elm$parser$Parser$Advanced$symbol = elm$parser$Parser$Advanced$token;
var elm$parser$Parser$symbol = function (str) {
	return elm$parser$Parser$Advanced$symbol(
		A2(
			elm$parser$Parser$Advanced$Token,
			str,
			elm$parser$Parser$ExpectingSymbol(str)));
};
var Punie$elm_parser_extras$Parser$Extras$braces = A2(
	Punie$elm_parser_extras$Parser$Extras$between,
	elm$parser$Parser$symbol('{'),
	elm$parser$Parser$symbol('}'));
var elm$parser$Parser$Done = function (a) {
	return {$: 'Done', a: a};
};
var elm$parser$Parser$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var elm$parser$Parser$Advanced$map = F2(
	function (func, _n0) {
		var parse = _n0.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n1 = parse(s0);
				if (_n1.$ === 'Good') {
					var p = _n1.a;
					var a = _n1.b;
					var s1 = _n1.c;
					return A3(
						elm$parser$Parser$Advanced$Good,
						p,
						func(a),
						s1);
				} else {
					var p = _n1.a;
					var x = _n1.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				}
			});
	});
var elm$parser$Parser$map = elm$parser$Parser$Advanced$map;
var Punie$elm_parser_extras$Parser$Extras$manyHelp = F2(
	function (p, vs) {
		return elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					elm$parser$Parser$keeper,
					elm$parser$Parser$succeed(
						function (v) {
							return elm$parser$Parser$Loop(
								A2(elm$core$List$cons, v, vs));
						}),
					A2(elm$parser$Parser$ignorer, p, elm$parser$Parser$spaces)),
					A2(
					elm$parser$Parser$map,
					function (_n0) {
						return elm$parser$Parser$Done(
							elm$core$List$reverse(vs));
					},
					elm$parser$Parser$succeed(_Utils_Tuple0))
				]));
	});
var elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 'Done', a: a};
};
var elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var elm$parser$Parser$toAdvancedStep = function (step) {
	if (step.$ === 'Loop') {
		var s = step.a;
		return elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return elm$parser$Parser$Advanced$Done(a);
	}
};
var elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _n0 = callback(state);
			var parse = _n0.a;
			var _n1 = parse(s0);
			if (_n1.$ === 'Good') {
				var p1 = _n1.a;
				var step = _n1.b;
				var s1 = _n1.c;
				if (step.$ === 'Loop') {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3(elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _n1.a;
				var x = _n1.b;
				return A2(elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return elm$parser$Parser$Advanced$Parser(
			function (s) {
				return A4(elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
			});
	});
var elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					elm$parser$Parser$map,
					elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var Punie$elm_parser_extras$Parser$Extras$many = function (p) {
	return A2(
		elm$parser$Parser$loop,
		_List_Nil,
		Punie$elm_parser_extras$Parser$Extras$manyHelp(p));
};
var Punie$elm_parser_extras$Parser$Extras$parens = A2(
	Punie$elm_parser_extras$Parser$Extras$between,
	elm$parser$Parser$symbol('('),
	elm$parser$Parser$symbol(')'));
var author$project$Types$Boolean = function (a) {
	return {$: 'Boolean', a: a};
};
var author$project$Types$Untracked = function (a) {
	return {$: 'Untracked', a: a};
};
var author$project$Types$Value = function (a) {
	return {$: 'Value', a: a};
};
var author$project$AstParser$booleans = elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			elm$parser$Parser$ignorer,
			elm$parser$Parser$succeed(
				author$project$Types$Untracked(
					author$project$Types$Value(
						author$project$Types$Boolean(true)))),
			elm$parser$Parser$backtrackable(
				elm$parser$Parser$symbol('true'))),
			A2(
			elm$parser$Parser$ignorer,
			elm$parser$Parser$succeed(
				author$project$Types$Untracked(
					author$project$Types$Value(
						author$project$Types$Boolean(false)))),
			elm$parser$Parser$backtrackable(
				elm$parser$Parser$symbol('false')))
		]));
var elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var elm$core$Set$empty = elm$core$Set$Set_elm_builtin(elm$core$Dict$empty);
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Set$insert = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return elm$core$Set$Set_elm_builtin(
			A3(elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var elm$core$Set$fromList = function (list) {
	return A3(elm$core$List$foldl, elm$core$Set$insert, elm$core$Set$empty, list);
};
var elm$parser$Parser$ExpectingVariable = {$: 'ExpectingVariable'};
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Dict$member = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$get, key, dict);
		if (_n0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var elm$core$Set$member = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return A2(elm$core$Dict$member, key, dict);
	});
var elm$core$String$slice = _String_slice;
var elm$parser$Parser$Advanced$varHelp = F7(
	function (isGood, offset, row, col, src, indent, context) {
		varHelp:
		while (true) {
			var newOffset = A3(elm$parser$Parser$Advanced$isSubChar, isGood, offset, src);
			if (_Utils_eq(newOffset, -1)) {
				return {col: col, context: context, indent: indent, offset: offset, row: row, src: src};
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				}
			}
		}
	});
var elm$parser$Parser$Advanced$variable = function (i) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			var firstOffset = A3(elm$parser$Parser$Advanced$isSubChar, i.start, s.offset, s.src);
			if (_Utils_eq(firstOffset, -1)) {
				return A2(
					elm$parser$Parser$Advanced$Bad,
					false,
					A2(elm$parser$Parser$Advanced$fromState, s, i.expecting));
			} else {
				var s1 = _Utils_eq(firstOffset, -2) ? A7(elm$parser$Parser$Advanced$varHelp, i.inner, s.offset + 1, s.row + 1, 1, s.src, s.indent, s.context) : A7(elm$parser$Parser$Advanced$varHelp, i.inner, firstOffset, s.row, s.col + 1, s.src, s.indent, s.context);
				var name = A3(elm$core$String$slice, s.offset, s1.offset, s.src);
				return A2(elm$core$Set$member, name, i.reserved) ? A2(
					elm$parser$Parser$Advanced$Bad,
					false,
					A2(elm$parser$Parser$Advanced$fromState, s, i.expecting)) : A3(elm$parser$Parser$Advanced$Good, true, name, s1);
			}
		});
};
var elm$parser$Parser$variable = function (i) {
	return elm$parser$Parser$Advanced$variable(
		{expecting: elm$parser$Parser$ExpectingVariable, inner: i.inner, reserved: i.reserved, start: i.start});
};
var author$project$AstParser$identifier = elm$parser$Parser$variable(
	{
		inner: function (c) {
			return elm$core$Char$isAlphaNum(c) || _Utils_eq(
				c,
				_Utils_chr('_'));
		},
		reserved: elm$core$Set$fromList(_List_Nil),
		start: elm$core$Char$isLower
	});
var author$project$Types$Decrement = function (a) {
	return {$: 'Decrement', a: a};
};
var author$project$Types$Operation = F2(
	function (a, b) {
		return {$: 'Operation', a: a, b: b};
	});
var author$project$Types$Variable = function (a) {
	return {$: 'Variable', a: a};
};
var elm$parser$Parser$Advanced$getPosition = elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3(
			elm$parser$Parser$Advanced$Good,
			false,
			_Utils_Tuple2(s.row, s.col),
			s);
	});
var elm$parser$Parser$getPosition = elm$parser$Parser$Advanced$getPosition;
var author$project$AstParser$decrement = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F3(
						function (posVar, name, posInc) {
							return A2(
								track,
								posInc,
								A2(
									author$project$Types$Operation,
									author$project$Types$Decrement(name),
									A2(
										track,
										posVar,
										author$project$Types$Variable(name))));
						})),
				elm$parser$Parser$getPosition),
			elm$parser$Parser$backtrackable(author$project$AstParser$identifier)),
		A2(
			elm$parser$Parser$ignorer,
			elm$parser$Parser$getPosition,
			elm$parser$Parser$backtrackable(
				elm$parser$Parser$symbol('--'))));
};
var author$project$Types$Number = function (a) {
	return {$: 'Number', a: a};
};
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (maybe.$ === 'Just') {
			var v = maybe.a;
			return elm$core$Result$Ok(v);
		} else {
			return elm$core$Result$Err(err);
		}
	});
var elm$parser$Parser$ExpectingBinary = {$: 'ExpectingBinary'};
var elm$parser$Parser$ExpectingFloat = {$: 'ExpectingFloat'};
var elm$parser$Parser$ExpectingHex = {$: 'ExpectingHex'};
var elm$parser$Parser$ExpectingInt = {$: 'ExpectingInt'};
var elm$parser$Parser$ExpectingNumber = {$: 'ExpectingNumber'};
var elm$parser$Parser$ExpectingOctal = {$: 'ExpectingOctal'};
var elm$parser$Parser$Advanced$consumeBase = _Parser_consumeBase;
var elm$parser$Parser$Advanced$consumeBase16 = _Parser_consumeBase16;
var elm$core$String$toFloat = _String_toFloat;
var elm$parser$Parser$Advanced$bumpOffset = F2(
	function (newOffset, s) {
		return {col: s.col + (newOffset - s.offset), context: s.context, indent: s.indent, offset: newOffset, row: s.row, src: s.src};
	});
var elm$parser$Parser$Advanced$chompBase10 = _Parser_chompBase10;
var elm$parser$Parser$Advanced$isAsciiCode = _Parser_isAsciiCode;
var elm$parser$Parser$Advanced$consumeExp = F2(
	function (offset, src) {
		if (A3(elm$parser$Parser$Advanced$isAsciiCode, 101, offset, src) || A3(elm$parser$Parser$Advanced$isAsciiCode, 69, offset, src)) {
			var eOffset = offset + 1;
			var expOffset = (A3(elm$parser$Parser$Advanced$isAsciiCode, 43, eOffset, src) || A3(elm$parser$Parser$Advanced$isAsciiCode, 45, eOffset, src)) ? (eOffset + 1) : eOffset;
			var newOffset = A2(elm$parser$Parser$Advanced$chompBase10, expOffset, src);
			return _Utils_eq(expOffset, newOffset) ? (-newOffset) : newOffset;
		} else {
			return offset;
		}
	});
var elm$parser$Parser$Advanced$consumeDotAndExp = F2(
	function (offset, src) {
		return A3(elm$parser$Parser$Advanced$isAsciiCode, 46, offset, src) ? A2(
			elm$parser$Parser$Advanced$consumeExp,
			A2(elm$parser$Parser$Advanced$chompBase10, offset + 1, src),
			src) : A2(elm$parser$Parser$Advanced$consumeExp, offset, src);
	});
var elm$parser$Parser$Advanced$finalizeInt = F5(
	function (invalid, handler, startOffset, _n0, s) {
		var endOffset = _n0.a;
		var n = _n0.b;
		if (handler.$ === 'Err') {
			var x = handler.a;
			return A2(
				elm$parser$Parser$Advanced$Bad,
				true,
				A2(elm$parser$Parser$Advanced$fromState, s, x));
		} else {
			var toValue = handler.a;
			return _Utils_eq(startOffset, endOffset) ? A2(
				elm$parser$Parser$Advanced$Bad,
				_Utils_cmp(s.offset, startOffset) < 0,
				A2(elm$parser$Parser$Advanced$fromState, s, invalid)) : A3(
				elm$parser$Parser$Advanced$Good,
				true,
				toValue(n),
				A2(elm$parser$Parser$Advanced$bumpOffset, endOffset, s));
		}
	});
var elm$parser$Parser$Advanced$fromInfo = F4(
	function (row, col, x, context) {
		return A2(
			elm$parser$Parser$Advanced$AddRight,
			elm$parser$Parser$Advanced$Empty,
			A4(elm$parser$Parser$Advanced$Problem, row, col, x, context));
	});
var elm$parser$Parser$Advanced$finalizeFloat = F6(
	function (invalid, expecting, intSettings, floatSettings, intPair, s) {
		var intOffset = intPair.a;
		var floatOffset = A2(elm$parser$Parser$Advanced$consumeDotAndExp, intOffset, s.src);
		if (floatOffset < 0) {
			return A2(
				elm$parser$Parser$Advanced$Bad,
				true,
				A4(elm$parser$Parser$Advanced$fromInfo, s.row, s.col - (floatOffset + s.offset), invalid, s.context));
		} else {
			if (_Utils_eq(s.offset, floatOffset)) {
				return A2(
					elm$parser$Parser$Advanced$Bad,
					false,
					A2(elm$parser$Parser$Advanced$fromState, s, expecting));
			} else {
				if (_Utils_eq(intOffset, floatOffset)) {
					return A5(elm$parser$Parser$Advanced$finalizeInt, invalid, intSettings, s.offset, intPair, s);
				} else {
					if (floatSettings.$ === 'Err') {
						var x = floatSettings.a;
						return A2(
							elm$parser$Parser$Advanced$Bad,
							true,
							A2(elm$parser$Parser$Advanced$fromState, s, invalid));
					} else {
						var toValue = floatSettings.a;
						var _n1 = elm$core$String$toFloat(
							A3(elm$core$String$slice, s.offset, floatOffset, s.src));
						if (_n1.$ === 'Nothing') {
							return A2(
								elm$parser$Parser$Advanced$Bad,
								true,
								A2(elm$parser$Parser$Advanced$fromState, s, invalid));
						} else {
							var n = _n1.a;
							return A3(
								elm$parser$Parser$Advanced$Good,
								true,
								toValue(n),
								A2(elm$parser$Parser$Advanced$bumpOffset, floatOffset, s));
						}
					}
				}
			}
		}
	});
var elm$parser$Parser$Advanced$number = function (c) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			if (A3(elm$parser$Parser$Advanced$isAsciiCode, 48, s.offset, s.src)) {
				var zeroOffset = s.offset + 1;
				var baseOffset = zeroOffset + 1;
				return A3(elm$parser$Parser$Advanced$isAsciiCode, 120, zeroOffset, s.src) ? A5(
					elm$parser$Parser$Advanced$finalizeInt,
					c.invalid,
					c.hex,
					baseOffset,
					A2(elm$parser$Parser$Advanced$consumeBase16, baseOffset, s.src),
					s) : (A3(elm$parser$Parser$Advanced$isAsciiCode, 111, zeroOffset, s.src) ? A5(
					elm$parser$Parser$Advanced$finalizeInt,
					c.invalid,
					c.octal,
					baseOffset,
					A3(elm$parser$Parser$Advanced$consumeBase, 8, baseOffset, s.src),
					s) : (A3(elm$parser$Parser$Advanced$isAsciiCode, 98, zeroOffset, s.src) ? A5(
					elm$parser$Parser$Advanced$finalizeInt,
					c.invalid,
					c.binary,
					baseOffset,
					A3(elm$parser$Parser$Advanced$consumeBase, 2, baseOffset, s.src),
					s) : A6(
					elm$parser$Parser$Advanced$finalizeFloat,
					c.invalid,
					c.expecting,
					c._int,
					c._float,
					_Utils_Tuple2(zeroOffset, 0),
					s)));
			} else {
				return A6(
					elm$parser$Parser$Advanced$finalizeFloat,
					c.invalid,
					c.expecting,
					c._int,
					c._float,
					A3(elm$parser$Parser$Advanced$consumeBase, 10, s.offset, s.src),
					s);
			}
		});
};
var elm$parser$Parser$number = function (i) {
	return elm$parser$Parser$Advanced$number(
		{
			binary: A2(elm$core$Result$fromMaybe, elm$parser$Parser$ExpectingBinary, i.binary),
			expecting: elm$parser$Parser$ExpectingNumber,
			_float: A2(elm$core$Result$fromMaybe, elm$parser$Parser$ExpectingFloat, i._float),
			hex: A2(elm$core$Result$fromMaybe, elm$parser$Parser$ExpectingHex, i.hex),
			_int: A2(elm$core$Result$fromMaybe, elm$parser$Parser$ExpectingInt, i._int),
			invalid: elm$parser$Parser$ExpectingNumber,
			octal: A2(elm$core$Result$fromMaybe, elm$parser$Parser$ExpectingOctal, i.octal)
		});
};
var author$project$AstParser$digits = elm$parser$Parser$number(
	{
		binary: elm$core$Maybe$Nothing,
		_float: elm$core$Maybe$Just(
			A2(
				elm$core$Basics$composeR,
				author$project$Types$Number,
				A2(elm$core$Basics$composeR, author$project$Types$Value, author$project$Types$Untracked))),
		hex: elm$core$Maybe$Nothing,
		_int: elm$core$Maybe$Just(
			A2(
				elm$core$Basics$composeR,
				elm$core$Basics$toFloat,
				A2(
					elm$core$Basics$composeR,
					author$project$Types$Number,
					A2(elm$core$Basics$composeR, author$project$Types$Value, author$project$Types$Untracked)))),
		octal: elm$core$Maybe$Nothing
	});
var author$project$Types$Member = {$: 'Member'};
var author$project$Types$Operation2 = F3(
	function (a, b, c) {
		return {$: 'Operation2', a: a, b: b, c: c};
	});
var author$project$Types$String = function (a) {
	return {$: 'String', a: a};
};
var author$project$AstParser$dot = F2(
	function (track, expr) {
		return A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F2(
						function (pos, key) {
							return A2(
								track,
								pos,
								A3(
									author$project$Types$Operation2,
									author$project$Types$Member,
									expr,
									author$project$Types$Untracked(
										author$project$Types$Value(
											author$project$Types$String(key)))));
						})),
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$getPosition,
					elm$parser$Parser$symbol('.'))),
			author$project$AstParser$identifier);
	});
var author$project$Types$Increment = function (a) {
	return {$: 'Increment', a: a};
};
var author$project$AstParser$increment = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F3(
						function (posVar, name, posInc) {
							return A2(
								track,
								posInc,
								A2(
									author$project$Types$Operation,
									author$project$Types$Increment(name),
									A2(
										track,
										posVar,
										author$project$Types$Variable(name))));
						})),
				elm$parser$Parser$getPosition),
			elm$parser$Parser$backtrackable(author$project$AstParser$identifier)),
		A2(
			elm$parser$Parser$ignorer,
			elm$parser$Parser$getPosition,
			elm$parser$Parser$backtrackable(
				elm$parser$Parser$symbol('++'))));
};
var author$project$AstParser$notSupported = A2(
	elm$parser$Parser$andThen,
	function (syntax) {
		return ((syntax === '`var`') || (syntax === '`const`')) ? elm$parser$Parser$problem('sorry, ' + (syntax + ' is not supported yet, use let instead')) : ((syntax === '`null`') ? elm$parser$Parser$problem('sorry, ' + (syntax + ' is not supported yet, use undefined instead')) : elm$parser$Parser$problem('sorry, ' + (syntax + ' is not supported yet')));
	},
	elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed('try/catch'),
				elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							elm$parser$Parser$symbol('try '),
							elm$parser$Parser$symbol('try\n'),
							elm$parser$Parser$symbol('try{')
						]))),
				A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed('throw'),
				elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							elm$parser$Parser$symbol('throw '),
							elm$parser$Parser$symbol('throw\n')
						]))),
				A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed('switch'),
				elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							elm$parser$Parser$symbol('switch '),
							elm$parser$Parser$symbol('switch\n'),
							elm$parser$Parser$symbol('switch{')
						]))),
				A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed('`null`'),
				elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							elm$parser$Parser$symbol('null '),
							elm$parser$Parser$symbol('null;'),
							elm$parser$Parser$symbol('null\n')
						]))),
				A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed('`var`'),
				elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							elm$parser$Parser$symbol('var '),
							elm$parser$Parser$symbol('try\n'),
							elm$parser$Parser$symbol('try{')
						]))),
				A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed('`const`'),
				elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							elm$parser$Parser$symbol('const '),
							elm$parser$Parser$symbol('try\n'),
							elm$parser$Parser$symbol('try{')
						]))),
				A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed('console'),
				elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							elm$parser$Parser$symbol('console.')
						]))),
				A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed('`this`'),
				elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							elm$parser$Parser$symbol('this.'),
							elm$parser$Parser$symbol('this '),
							elm$parser$Parser$symbol('this;'),
							elm$parser$Parser$symbol('this\n')
						])))
			])));
var Punie$elm_parser_extras$Parser$Expression$AssocLeft = {$: 'AssocLeft'};
var Punie$elm_parser_extras$Parser$Expression$Prefix = function (a) {
	return {$: 'Prefix', a: a};
};
var Punie$elm_parser_extras$Parser$Expression$Infix = F2(
	function (a, b) {
		return {$: 'Infix', a: a, b: b};
	});
var author$project$AstParser$infixOperator = F4(
	function (track, operation, opParser, assoc) {
		var binaryOp = A2(
			elm$parser$Parser$keeper,
			elm$parser$Parser$succeed(
				F3(
					function (pos, expr1, expr2) {
						return A2(
							track,
							pos,
							A3(author$project$Types$Operation2, operation, expr1, expr2));
					})),
			A2(elm$parser$Parser$ignorer, opParser, elm$parser$Parser$spaces));
		return A2(Punie$elm_parser_extras$Parser$Expression$Infix, binaryOp, assoc);
	});
var author$project$Types$Addition = {$: 'Addition'};
var author$project$Types$And = {$: 'And'};
var author$project$Types$Division = {$: 'Division'};
var author$project$Types$Exponentiation = {$: 'Exponentiation'};
var author$project$Types$GreaterOrEqualThan = {$: 'GreaterOrEqualThan'};
var author$project$Types$GreaterThan = {$: 'GreaterThan'};
var author$project$Types$HardEquality = {$: 'HardEquality'};
var author$project$Types$HardNotEquality = {$: 'HardNotEquality'};
var author$project$Types$Multiplication = {$: 'Multiplication'};
var author$project$Types$Negative = {$: 'Negative'};
var author$project$Types$Or = {$: 'Or'};
var author$project$Types$Remainder = {$: 'Remainder'};
var author$project$Types$SmallerOrEqualThan = {$: 'SmallerOrEqualThan'};
var author$project$Types$SmallerThan = {$: 'SmallerThan'};
var author$project$Types$SoftEquality = {$: 'SoftEquality'};
var author$project$Types$SoftNotEquality = {$: 'SoftNotEquality'};
var author$project$Types$Subtraction = {$: 'Subtraction'};
var author$project$AstParser$operators = function (track) {
	var symb = function (sign) {
		return A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed(elm$core$Basics$identity),
				elm$parser$Parser$backtrackable(elm$parser$Parser$spaces)),
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$getPosition,
				elm$parser$Parser$symbol(sign)));
	};
	return _List_fromArray(
		[
			_List_fromArray(
			[
				Punie$elm_parser_extras$Parser$Expression$Prefix(
				A2(
					elm$parser$Parser$keeper,
					elm$parser$Parser$succeed(
						F2(
							function (pos, expr) {
								return A2(
									track,
									pos,
									A2(author$project$Types$Operation, author$project$Types$Negative, expr));
							})),
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$getPosition,
						elm$parser$Parser$symbol('-'))))
			]),
			_List_fromArray(
			[
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$Exponentiation,
				symb('**'),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft)
			]),
			_List_fromArray(
			[
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$Multiplication,
				symb('*'),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft),
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$Division,
				symb('/ '),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft)
			]),
			_List_fromArray(
			[
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$Remainder,
				symb('%'),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft)
			]),
			_List_fromArray(
			[
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$Addition,
				symb('+'),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft),
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$Subtraction,
				symb('-'),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft)
			]),
			_List_fromArray(
			[
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$HardEquality,
				symb('==='),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft),
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$SoftEquality,
				symb('=='),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft),
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$HardNotEquality,
				symb('!=='),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft),
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$SoftNotEquality,
				symb('!='),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft),
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$GreaterOrEqualThan,
				symb('>='),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft),
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$SmallerOrEqualThan,
				symb('<='),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft),
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$GreaterThan,
				symb('>'),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft),
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$SmallerThan,
				symb('<'),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft)
			]),
			_List_fromArray(
			[
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$And,
				symb('&&'),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft),
				A4(
				author$project$AstParser$infixOperator,
				track,
				author$project$Types$Or,
				symb('||'),
				Punie$elm_parser_extras$Parser$Expression$AssocLeft)
			])
		]);
};
var elm$parser$Parser$UnexpectedChar = {$: 'UnexpectedChar'};
var elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return elm$parser$Parser$Advanced$Parser(
			function (s) {
				var newOffset = A3(elm$parser$Parser$Advanced$isSubChar, isGood, s.offset, s.src);
				return _Utils_eq(newOffset, -1) ? A2(
					elm$parser$Parser$Advanced$Bad,
					false,
					A2(elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
					elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: 1, context: s.context, indent: s.indent, offset: s.offset + 1, row: s.row + 1, src: s.src}) : A3(
					elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: s.col + 1, context: s.context, indent: s.indent, offset: newOffset, row: s.row, src: s.src}));
			});
	});
var elm$parser$Parser$chompIf = function (isGood) {
	return A2(elm$parser$Parser$Advanced$chompIf, isGood, elm$parser$Parser$UnexpectedChar);
};
var elm$parser$Parser$chompWhile = elm$parser$Parser$Advanced$chompWhile;
var elm$parser$Parser$Expecting = function (a) {
	return {$: 'Expecting', a: a};
};
var elm$parser$Parser$toToken = function (str) {
	return A2(
		elm$parser$Parser$Advanced$Token,
		str,
		elm$parser$Parser$Expecting(str));
};
var elm$core$String$length = _String_length;
var elm$parser$Parser$Advanced$chompUntilEndOr = function (str) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _n0 = A5(_Parser_findSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _n0.a;
			var newRow = _n0.b;
			var newCol = _n0.c;
			var adjustedOffset = (newOffset < 0) ? elm$core$String$length(s.src) : newOffset;
			return A3(
				elm$parser$Parser$Advanced$Good,
				_Utils_cmp(s.offset, adjustedOffset) < 0,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: adjustedOffset, row: newRow, src: s.src});
		});
};
var elm$parser$Parser$Advanced$lineComment = function (start) {
	return A2(
		elm$parser$Parser$Advanced$ignorer,
		elm$parser$Parser$Advanced$token(start),
		elm$parser$Parser$Advanced$chompUntilEndOr('\n'));
};
var elm$parser$Parser$lineComment = function (str) {
	return elm$parser$Parser$Advanced$lineComment(
		elm$parser$Parser$toToken(str));
};
var author$project$AstParser$statementBreak = A2(
	elm$parser$Parser$ignorer,
	A2(
		elm$parser$Parser$ignorer,
		A2(
			elm$parser$Parser$ignorer,
			elm$parser$Parser$succeed(_Utils_Tuple0),
			elm$parser$Parser$chompWhile(
				function (c) {
					return _Utils_eq(
						c,
						_Utils_chr(' ')) || _Utils_eq(
						c,
						_Utils_chr('\t'));
				})),
		elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					elm$parser$Parser$chompIf(
					function (c) {
						return _Utils_eq(
							c,
							_Utils_chr('\n')) || _Utils_eq(
							c,
							_Utils_chr(';'));
					}),
					elm$parser$Parser$lineComment('//')
				]))),
	elm$parser$Parser$spaces);
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Basics$neq = _Utils_notEqual;
var elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _n0) {
		var parse = _n0.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n1 = parse(s0);
				if (_n1.$ === 'Bad') {
					var p = _n1.a;
					var x = _n1.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p = _n1.a;
					var a = _n1.b;
					var s1 = _n1.c;
					return A3(
						elm$parser$Parser$Advanced$Good,
						p,
						A2(
							func,
							A3(elm$core$String$slice, s0.offset, s1.offset, s0.src),
							a),
						s1);
				}
			});
	});
var elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2(elm$parser$Parser$Advanced$mapChompedString, elm$core$Basics$always, parser);
};
var elm$parser$Parser$getChompedString = elm$parser$Parser$Advanced$getChompedString;
var author$project$AstParser$strings = elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed(
					A2(
						elm$core$Basics$composeL,
						A2(elm$core$Basics$composeL, author$project$Types$Untracked, author$project$Types$Value),
						author$project$Types$String)),
				elm$parser$Parser$symbol('\"')),
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$getChompedString(
					elm$parser$Parser$chompWhile(
						function (c) {
							return (!_Utils_eq(
								c,
								_Utils_chr('\"'))) && (!_Utils_eq(
								c,
								_Utils_chr('\n')));
						})),
				elm$parser$Parser$symbol('\"'))),
			A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed(
					A2(
						elm$core$Basics$composeL,
						A2(elm$core$Basics$composeL, author$project$Types$Untracked, author$project$Types$Value),
						author$project$Types$String)),
				elm$parser$Parser$symbol('\'')),
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$getChompedString(
					elm$parser$Parser$chompWhile(
						function (c) {
							return (!_Utils_eq(
								c,
								_Utils_chr('\''))) && (!_Utils_eq(
								c,
								_Utils_chr('\n')));
						})),
				elm$parser$Parser$symbol('\'')))
		]));
var author$project$Types$ExplicitUndefined = {$: 'ExplicitUndefined'};
var author$project$Types$Undefined = function (a) {
	return {$: 'Undefined', a: a};
};
var author$project$AstParser$undefined = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		elm$parser$Parser$succeed(
			function (_n0) {
				var row = _n0.a;
				var col = _n0.b;
				var info = function () {
					var _n1 = A2(
						track,
						_Utils_Tuple2(row, col),
						author$project$Types$Value(
							author$project$Types$Undefined(_List_Nil)));
					if (_n1.$ === 'Tracked') {
						var trackInfo = _n1.a;
						return trackInfo;
					} else {
						return {column: col, filename: '', line: row};
					}
				}();
				return author$project$Types$Untracked(
					author$project$Types$Value(
						author$project$Types$Undefined(
							_List_fromArray(
								[
									{column: col, filename: info.filename, line: row, reason: author$project$Types$ExplicitUndefined}
								]))));
			}),
		A2(
			elm$parser$Parser$ignorer,
			elm$parser$Parser$getPosition,
			elm$parser$Parser$backtrackable(
				elm$parser$Parser$symbol('undefined'))));
};
var author$project$Types$Abstraction = F2(
	function (a, b) {
		return {$: 'Abstraction', a: a, b: b};
	});
var author$project$Types$Application = F2(
	function (a, b) {
		return {$: 'Application', a: a, b: b};
	});
var author$project$Types$ArrayExpression = function (a) {
	return {$: 'ArrayExpression', a: a};
};
var author$project$Types$Assignment = function (a) {
	return {$: 'Assignment', a: a};
};
var author$project$Types$Block = function (a) {
	return {$: 'Block', a: a};
};
var author$project$Types$ForLoop = F4(
	function (a, b, c, d) {
		return {$: 'ForLoop', a: a, b: b, c: c, d: d};
	});
var author$project$Types$IfCondition = F2(
	function (a, b) {
		return {$: 'IfCondition', a: a, b: b};
	});
var author$project$Types$IfElseCondition = F3(
	function (a, b, c) {
		return {$: 'IfElseCondition', a: a, b: b, c: c};
	});
var author$project$Types$LetAssignment = function (a) {
	return {$: 'LetAssignment', a: a};
};
var author$project$Types$Not = {$: 'Not'};
var author$project$Types$ObjectExpression = function (a) {
	return {$: 'ObjectExpression', a: a};
};
var author$project$Types$Return = function (a) {
	return {$: 'Return', a: a};
};
var author$project$Types$While = F2(
	function (a, b) {
		return {$: 'While', a: a, b: b};
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var elm$core$String$fromFloat = _String_fromNumber;
var elm$parser$Parser$Forbidden = {$: 'Forbidden'};
var elm$parser$Parser$Advanced$lazy = function (thunk) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _n0 = thunk(_Utils_Tuple0);
			var parse = _n0.a;
			return parse(s);
		});
};
var elm$parser$Parser$lazy = elm$parser$Parser$Advanced$lazy;
var elm$parser$Parser$Advanced$Forbidden = {$: 'Forbidden'};
var elm$parser$Parser$Advanced$Mandatory = {$: 'Mandatory'};
var elm$parser$Parser$Advanced$Optional = {$: 'Optional'};
var elm$parser$Parser$toAdvancedTrailing = function (trailing) {
	switch (trailing.$) {
		case 'Forbidden':
			return elm$parser$Parser$Advanced$Forbidden;
		case 'Optional':
			return elm$parser$Parser$Advanced$Optional;
		default:
			return elm$parser$Parser$Advanced$Mandatory;
	}
};
var elm$parser$Parser$Advanced$revAlways = F2(
	function (_n0, b) {
		return b;
	});
var elm$parser$Parser$Advanced$skip = F2(
	function (iParser, kParser) {
		return A3(elm$parser$Parser$Advanced$map2, elm$parser$Parser$Advanced$revAlways, iParser, kParser);
	});
var elm$parser$Parser$Advanced$sequenceEndForbidden = F5(
	function (ender, ws, parseItem, sep, revItems) {
		var chompRest = function (item) {
			return A5(
				elm$parser$Parser$Advanced$sequenceEndForbidden,
				ender,
				ws,
				parseItem,
				sep,
				A2(elm$core$List$cons, item, revItems));
		};
		return A2(
			elm$parser$Parser$Advanced$skip,
			ws,
			elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						A2(
						elm$parser$Parser$Advanced$skip,
						sep,
						A2(
							elm$parser$Parser$Advanced$skip,
							ws,
							A2(
								elm$parser$Parser$Advanced$map,
								function (item) {
									return elm$parser$Parser$Advanced$Loop(
										A2(elm$core$List$cons, item, revItems));
								},
								parseItem))),
						A2(
						elm$parser$Parser$Advanced$map,
						function (_n0) {
							return elm$parser$Parser$Advanced$Done(
								elm$core$List$reverse(revItems));
						},
						ender)
					])));
	});
var elm$parser$Parser$Advanced$sequenceEndMandatory = F4(
	function (ws, parseItem, sep, revItems) {
		return elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					elm$parser$Parser$Advanced$map,
					function (item) {
						return elm$parser$Parser$Advanced$Loop(
							A2(elm$core$List$cons, item, revItems));
					},
					A2(
						elm$parser$Parser$Advanced$ignorer,
						parseItem,
						A2(
							elm$parser$Parser$Advanced$ignorer,
							ws,
							A2(elm$parser$Parser$Advanced$ignorer, sep, ws)))),
					A2(
					elm$parser$Parser$Advanced$map,
					function (_n0) {
						return elm$parser$Parser$Advanced$Done(
							elm$core$List$reverse(revItems));
					},
					elm$parser$Parser$Advanced$succeed(_Utils_Tuple0))
				]));
	});
var elm$parser$Parser$Advanced$sequenceEndOptional = F5(
	function (ender, ws, parseItem, sep, revItems) {
		var parseEnd = A2(
			elm$parser$Parser$Advanced$map,
			function (_n0) {
				return elm$parser$Parser$Advanced$Done(
					elm$core$List$reverse(revItems));
			},
			ender);
		return A2(
			elm$parser$Parser$Advanced$skip,
			ws,
			elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						A2(
						elm$parser$Parser$Advanced$skip,
						sep,
						A2(
							elm$parser$Parser$Advanced$skip,
							ws,
							elm$parser$Parser$Advanced$oneOf(
								_List_fromArray(
									[
										A2(
										elm$parser$Parser$Advanced$map,
										function (item) {
											return elm$parser$Parser$Advanced$Loop(
												A2(elm$core$List$cons, item, revItems));
										},
										parseItem),
										parseEnd
									])))),
						parseEnd
					])));
	});
var elm$parser$Parser$Advanced$sequenceEnd = F5(
	function (ender, ws, parseItem, sep, trailing) {
		var chompRest = function (item) {
			switch (trailing.$) {
				case 'Forbidden':
					return A2(
						elm$parser$Parser$Advanced$loop,
						_List_fromArray(
							[item]),
						A4(elm$parser$Parser$Advanced$sequenceEndForbidden, ender, ws, parseItem, sep));
				case 'Optional':
					return A2(
						elm$parser$Parser$Advanced$loop,
						_List_fromArray(
							[item]),
						A4(elm$parser$Parser$Advanced$sequenceEndOptional, ender, ws, parseItem, sep));
				default:
					return A2(
						elm$parser$Parser$Advanced$ignorer,
						A2(
							elm$parser$Parser$Advanced$skip,
							ws,
							A2(
								elm$parser$Parser$Advanced$skip,
								sep,
								A2(
									elm$parser$Parser$Advanced$skip,
									ws,
									A2(
										elm$parser$Parser$Advanced$loop,
										_List_fromArray(
											[item]),
										A3(elm$parser$Parser$Advanced$sequenceEndMandatory, ws, parseItem, sep))))),
						ender);
			}
		};
		return elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(elm$parser$Parser$Advanced$andThen, chompRest, parseItem),
					A2(
					elm$parser$Parser$Advanced$map,
					function (_n0) {
						return _List_Nil;
					},
					ender)
				]));
	});
var elm$parser$Parser$Advanced$sequence = function (i) {
	return A2(
		elm$parser$Parser$Advanced$skip,
		elm$parser$Parser$Advanced$token(i.start),
		A2(
			elm$parser$Parser$Advanced$skip,
			i.spaces,
			A5(
				elm$parser$Parser$Advanced$sequenceEnd,
				elm$parser$Parser$Advanced$token(i.end),
				i.spaces,
				i.item,
				elm$parser$Parser$Advanced$token(i.separator),
				i.trailing)));
};
var elm$parser$Parser$sequence = function (i) {
	return elm$parser$Parser$Advanced$sequence(
		{
			end: elm$parser$Parser$toToken(i.end),
			item: i.item,
			separator: elm$parser$Parser$toToken(i.separator),
			spaces: i.spaces,
			start: elm$parser$Parser$toToken(i.start),
			trailing: elm$parser$Parser$toAdvancedTrailing(i.trailing)
		});
};
var author$project$AstParser$abstraction = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F3(
						function (pos, params, body) {
							return A2(
								track,
								pos,
								author$project$Types$Value(
									A2(author$project$Types$Abstraction, params, body)));
						})),
				elm$parser$Parser$getPosition),
			A2(
				elm$parser$Parser$ignorer,
				A2(
					elm$parser$Parser$ignorer,
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$backtrackable(
							elm$parser$Parser$sequence(
								{end: ')', item: author$project$AstParser$identifier, separator: ',', spaces: elm$parser$Parser$spaces, start: '(', trailing: elm$parser$Parser$Forbidden})),
						elm$parser$Parser$backtrackable(elm$parser$Parser$spaces)),
					elm$parser$Parser$backtrackable(
						elm$parser$Parser$symbol('=>'))),
				elm$parser$Parser$spaces)),
		elm$parser$Parser$lazy(
			function (_n19) {
				return A3(author$project$AstParser$expression_, track, true, true);
			}));
};
var author$project$AstParser$anonymousFunction = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F3(
						function (pos, params, body) {
							return A2(
								track,
								pos,
								author$project$Types$Value(
									A2(author$project$Types$Abstraction, params, body)));
						})),
				A2(
					elm$parser$Parser$ignorer,
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$getPosition,
						elm$parser$Parser$backtrackable(
							elm$parser$Parser$symbol('function'))),
					elm$parser$Parser$backtrackable(elm$parser$Parser$spaces))),
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$backtrackable(
					elm$parser$Parser$sequence(
						{end: ')', item: author$project$AstParser$identifier, separator: ',', spaces: elm$parser$Parser$spaces, start: '(', trailing: elm$parser$Parser$Forbidden})),
				elm$parser$Parser$spaces)),
		elm$parser$Parser$lazy(
			function (_n18) {
				return A2(author$project$AstParser$block, track, true);
			}));
};
var author$project$AstParser$arrays = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			elm$parser$Parser$succeed(
				F2(
					function (pos, items) {
						return A2(
							track,
							pos,
							author$project$Types$ArrayExpression(items));
					})),
			elm$parser$Parser$getPosition),
		elm$parser$Parser$sequence(
			{
				end: ']',
				item: author$project$AstParser$expression(track),
				separator: ',',
				spaces: elm$parser$Parser$spaces,
				start: '[',
				trailing: elm$parser$Parser$Forbidden
			}));
};
var author$project$AstParser$assignment = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							elm$parser$Parser$ignorer,
							elm$parser$Parser$succeed(
								F2(
									function (name, pos) {
										return A2(
											elm$core$Basics$composeL,
											track(pos),
											author$project$Types$Operation(
												author$project$Types$LetAssignment(name)));
									})),
							elm$parser$Parser$backtrackable(
								elm$parser$Parser$symbol('let '))),
							elm$parser$Parser$succeed(
							F2(
								function (name, pos) {
									return A2(
										elm$core$Basics$composeL,
										track(pos),
										author$project$Types$Operation(
											author$project$Types$Assignment(name)));
								}))
						])),
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$backtrackable(author$project$AstParser$identifier),
					elm$parser$Parser$backtrackable(elm$parser$Parser$spaces))),
			A2(
				elm$parser$Parser$ignorer,
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$getPosition,
					elm$parser$Parser$backtrackable(
						elm$parser$Parser$symbol('='))),
				elm$parser$Parser$spaces)),
		elm$parser$Parser$lazy(
			function (_n17) {
				return author$project$AstParser$expression(track);
			}));
};
var author$project$AstParser$atoms = function (track) {
	return elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				author$project$AstParser$booleans,
				author$project$AstParser$undefined(track),
				A2(
				elm$parser$Parser$keeper,
				A2(
					elm$parser$Parser$keeper,
					elm$parser$Parser$succeed(
						F2(
							function (pos, name) {
								return A2(
									track,
									pos,
									author$project$Types$Variable(name));
							})),
					elm$parser$Parser$getPosition),
				author$project$AstParser$identifier),
				author$project$AstParser$digits,
				author$project$AstParser$arrays(track),
				author$project$AstParser$strings
			]));
};
var author$project$AstParser$block = F2(
	function (track, withReturn) {
		var expressionLine = elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					elm$parser$Parser$keeper,
					elm$parser$Parser$succeed(elm$core$Basics$identity),
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$backtrackable(
							A3(author$project$AstParser$expression_, track, true, withReturn)),
						elm$parser$Parser$backtrackable(author$project$AstParser$statementBreak))),
					A2(
					elm$parser$Parser$keeper,
					elm$parser$Parser$succeed(elm$core$Basics$identity),
					A3(author$project$AstParser$expression_, track, true, withReturn))
				]));
		return A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F2(
						function (list, pos) {
							return A2(
								track,
								pos,
								author$project$Types$Block(list));
						})),
				elm$parser$Parser$backtrackable(
					Punie$elm_parser_extras$Parser$Extras$braces(
						Punie$elm_parser_extras$Parser$Extras$many(expressionLine)))),
			elm$parser$Parser$getPosition);
	});
var author$project$AstParser$expression = function (track) {
	return A3(author$project$AstParser$expression_, track, false, false);
};
var author$project$AstParser$expressionParsers = F2(
	function (track, withReturn) {
		var return_ = withReturn ? _List_fromArray(
			[
				author$project$AstParser$return(track)
			]) : _List_fromArray(
			[
				A2(
				elm$parser$Parser$ignorer,
				author$project$AstParser$return(track),
				elm$parser$Parser$problem('return can only be used inside a function body'))
			]);
		var expressions = _List_fromArray(
			[
				author$project$AstParser$notSupported,
				author$project$AstParser$objects(track),
				A2(author$project$AstParser$block, track, withReturn),
				author$project$AstParser$abstraction(track),
				author$project$AstParser$anonymousFunction(track),
				elm$parser$Parser$backtrackable(
				Punie$elm_parser_extras$Parser$Extras$parens(
					elm$parser$Parser$lazy(
						function (_n16) {
							return author$project$AstParser$expression(track);
						}))),
				author$project$AstParser$not_(track),
				author$project$AstParser$increment(track),
				author$project$AstParser$decrement(track),
				author$project$AstParser$atoms(track)
			]);
		return A2(
			elm$parser$Parser$andThen,
			author$project$AstParser$postfixOperators(track),
			elm$parser$Parser$oneOf(
				_Utils_ap(return_, expressions)));
	});
var author$project$AstParser$expression_ = F3(
	function (track, withDeclarations, withReturn) {
		var expressionParser = A2(
			Punie$elm_parser_extras$Parser$Expression$buildExpressionParser,
			author$project$AstParser$operators(track),
			elm$parser$Parser$lazy(
				function (_n15) {
					return A2(author$project$AstParser$expressionParsers, track, withReturn);
				}));
		var declarations = withDeclarations ? _List_fromArray(
			[
				author$project$AstParser$functionDeclaration(track),
				author$project$AstParser$assignment(track),
				author$project$AstParser$operationAssignment(track),
				A2(author$project$AstParser$ifElseCondition, track, withReturn),
				A2(author$project$AstParser$ifCondition, track, withReturn),
				author$project$AstParser$while(track),
				author$project$AstParser$forLoop(track)
			]) : _List_Nil;
		return elm$parser$Parser$oneOf(
			_Utils_ap(
				declarations,
				_List_fromArray(
					[expressionParser])));
	});
var author$project$AstParser$forLoop = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F3(
						function (pos, _n10, expr) {
							var assignment_ = _n10.a;
							var condition = _n10.b;
							var increment_ = _n10.c;
							return A2(
								track,
								pos,
								A4(author$project$Types$ForLoop, assignment_, condition, increment_, expr));
						})),
				A2(
					elm$parser$Parser$ignorer,
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$getPosition,
						elm$parser$Parser$backtrackable(
							elm$parser$Parser$symbol('for'))),
					elm$parser$Parser$spaces)),
			A2(
				elm$parser$Parser$ignorer,
				Punie$elm_parser_extras$Parser$Extras$parens(
					A2(
						elm$parser$Parser$keeper,
						A2(
							elm$parser$Parser$keeper,
							A2(
								elm$parser$Parser$keeper,
								A2(
									elm$parser$Parser$ignorer,
									elm$parser$Parser$succeed(
										F3(
											function (assignment_, condition, increment_) {
												return _Utils_Tuple3(assignment_, condition, increment_);
											})),
									elm$parser$Parser$spaces),
								A2(
									elm$parser$Parser$ignorer,
									A2(
										elm$parser$Parser$ignorer,
										A2(
											elm$parser$Parser$ignorer,
											elm$parser$Parser$lazy(
												function (_n11) {
													return A3(author$project$AstParser$expression_, track, true, false);
												}),
											elm$parser$Parser$spaces),
										elm$parser$Parser$symbol(';')),
									elm$parser$Parser$spaces)),
							A2(
								elm$parser$Parser$ignorer,
								A2(
									elm$parser$Parser$ignorer,
									A2(
										elm$parser$Parser$ignorer,
										elm$parser$Parser$lazy(
											function (_n12) {
												return A3(author$project$AstParser$expression_, track, true, false);
											}),
										elm$parser$Parser$spaces),
									elm$parser$Parser$symbol(';')),
								elm$parser$Parser$spaces)),
						A2(
							elm$parser$Parser$ignorer,
							elm$parser$Parser$lazy(
								function (_n13) {
									return A3(author$project$AstParser$expression_, track, true, false);
								}),
							elm$parser$Parser$spaces))),
				elm$parser$Parser$spaces)),
		elm$parser$Parser$lazy(
			function (_n14) {
				return author$project$AstParser$expression(track);
			}));
};
var author$project$AstParser$functionCall = F2(
	function (track, expr) {
		return A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F2(
						function (pos, args) {
							return A2(
								track,
								pos,
								A2(author$project$Types$Application, expr, args));
						})),
				elm$parser$Parser$getPosition),
			elm$parser$Parser$backtrackable(
				elm$parser$Parser$sequence(
					{
						end: ')',
						item: author$project$AstParser$expression(track),
						separator: ',',
						spaces: elm$parser$Parser$spaces,
						start: '(',
						trailing: elm$parser$Parser$Forbidden
					})));
	});
var author$project$AstParser$functionDeclaration = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				A2(
					elm$parser$Parser$keeper,
					elm$parser$Parser$succeed(
						F4(
							function (pos, name, params, body) {
								return A2(
									track,
									pos,
									A2(
										author$project$Types$Operation,
										author$project$Types$LetAssignment(name),
										A2(
											track,
											pos,
											author$project$Types$Value(
												A2(author$project$Types$Abstraction, params, body)))));
							})),
					A2(
						elm$parser$Parser$ignorer,
						A2(
							elm$parser$Parser$ignorer,
							elm$parser$Parser$getPosition,
							elm$parser$Parser$backtrackable(
								elm$parser$Parser$symbol('function'))),
						elm$parser$Parser$backtrackable(elm$parser$Parser$spaces))),
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$backtrackable(author$project$AstParser$identifier),
					elm$parser$Parser$spaces)),
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$sequence(
					{end: ')', item: author$project$AstParser$identifier, separator: ',', spaces: elm$parser$Parser$spaces, start: '(', trailing: elm$parser$Parser$Forbidden}),
				elm$parser$Parser$spaces)),
		elm$parser$Parser$lazy(
			function (_n9) {
				return A2(author$project$AstParser$block, track, true);
			}));
};
var author$project$AstParser$ifCondition = F2(
	function (track, withReturn) {
		return A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				A2(
					elm$parser$Parser$keeper,
					elm$parser$Parser$succeed(
						F3(
							function (pos, condition, expr) {
								return A2(
									track,
									pos,
									A2(author$project$Types$IfCondition, condition, expr));
							})),
					A2(
						elm$parser$Parser$ignorer,
						A2(
							elm$parser$Parser$ignorer,
							elm$parser$Parser$getPosition,
							elm$parser$Parser$backtrackable(
								elm$parser$Parser$symbol('if'))),
						elm$parser$Parser$spaces)),
				A2(
					elm$parser$Parser$ignorer,
					Punie$elm_parser_extras$Parser$Extras$parens(
						elm$parser$Parser$lazy(
							function (_n7) {
								return A3(author$project$AstParser$expression_, track, true, withReturn);
							})),
					elm$parser$Parser$spaces)),
			elm$parser$Parser$lazy(
				function (_n8) {
					return A3(author$project$AstParser$expression_, track, true, withReturn);
				}));
	});
var author$project$AstParser$ifElseCondition = F2(
	function (track, withReturn) {
		return A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				A2(
					elm$parser$Parser$keeper,
					A2(
						elm$parser$Parser$keeper,
						elm$parser$Parser$succeed(
							F4(
								function (pos, condition, exprIfTrue, exprIfFalse) {
									return A2(
										track,
										pos,
										A3(author$project$Types$IfElseCondition, condition, exprIfTrue, exprIfFalse));
								})),
						A2(
							elm$parser$Parser$ignorer,
							A2(
								elm$parser$Parser$ignorer,
								elm$parser$Parser$getPosition,
								elm$parser$Parser$backtrackable(
									elm$parser$Parser$symbol('if'))),
							elm$parser$Parser$backtrackable(elm$parser$Parser$spaces))),
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$backtrackable(
							Punie$elm_parser_extras$Parser$Extras$parens(
								elm$parser$Parser$lazy(
									function (_n4) {
										return A3(author$project$AstParser$expression_, track, true, withReturn);
									}))),
						elm$parser$Parser$backtrackable(elm$parser$Parser$spaces))),
				A2(
					elm$parser$Parser$ignorer,
					A2(
						elm$parser$Parser$ignorer,
						A2(
							elm$parser$Parser$ignorer,
							elm$parser$Parser$backtrackable(
								elm$parser$Parser$lazy(
									function (_n5) {
										return A2(author$project$AstParser$block, track, withReturn);
									})),
							elm$parser$Parser$backtrackable(elm$parser$Parser$spaces)),
						elm$parser$Parser$backtrackable(
							elm$parser$Parser$symbol('else'))),
					elm$parser$Parser$spaces)),
			elm$parser$Parser$lazy(
				function (_n6) {
					return A3(author$project$AstParser$expression_, track, true, withReturn);
				}));
	});
var author$project$AstParser$members = F2(
	function (track, expr) {
		return A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F2(
						function (pos, key) {
							return A2(
								track,
								pos,
								A3(author$project$Types$Operation2, author$project$Types$Member, expr, key));
						})),
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$getPosition,
					elm$parser$Parser$symbol('['))),
			A2(
				elm$parser$Parser$ignorer,
				author$project$AstParser$expression(track),
				elm$parser$Parser$symbol(']')));
	});
var author$project$AstParser$notSupportedPostfix = function (track) {
	return A2(
		elm$parser$Parser$andThen,
		function (syntax) {
			return elm$parser$Parser$problem('sorry, ' + (syntax + ' is not supported yet'));
		},
		elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					elm$parser$Parser$ignorer,
					A2(
						elm$parser$Parser$ignorer,
						A2(
							elm$parser$Parser$ignorer,
							A2(
								elm$parser$Parser$ignorer,
								A2(
									elm$parser$Parser$ignorer,
									elm$parser$Parser$succeed('array or object mutation'),
									elm$parser$Parser$backtrackable(
										elm$parser$Parser$symbol('['))),
								elm$parser$Parser$backtrackable(
									author$project$AstParser$expression(track))),
							elm$parser$Parser$backtrackable(
								elm$parser$Parser$symbol(']'))),
						elm$parser$Parser$backtrackable(elm$parser$Parser$spaces)),
					elm$parser$Parser$symbol('=')),
					A2(
					elm$parser$Parser$ignorer,
					A2(
						elm$parser$Parser$ignorer,
						A2(
							elm$parser$Parser$ignorer,
							A2(
								elm$parser$Parser$ignorer,
								elm$parser$Parser$succeed('object mutation'),
								elm$parser$Parser$backtrackable(
									elm$parser$Parser$symbol('.'))),
							elm$parser$Parser$backtrackable(author$project$AstParser$identifier)),
						elm$parser$Parser$backtrackable(elm$parser$Parser$spaces)),
					elm$parser$Parser$symbol('='))
				])));
};
var author$project$AstParser$not_ = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			elm$parser$Parser$succeed(
				F2(
					function (pos, expr) {
						return A2(
							track,
							pos,
							A2(author$project$Types$Operation, author$project$Types$Not, expr));
					})),
			A2(
				elm$parser$Parser$ignorer,
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$getPosition,
					elm$parser$Parser$symbol('!')),
				elm$parser$Parser$spaces)),
		elm$parser$Parser$lazy(
			function (_n3) {
				return author$project$AstParser$expression(track);
			}));
};
var author$project$AstParser$objects = function (track) {
	var objectItem = A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed(
					F2(
						function (key, value) {
							return _Utils_Tuple2(key, value);
						})),
				elm$parser$Parser$spaces),
			A2(
				elm$parser$Parser$ignorer,
				A2(
					elm$parser$Parser$ignorer,
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									author$project$AstParser$identifier,
									A3(
									Punie$elm_parser_extras$Parser$Extras$between,
									elm$parser$Parser$symbol('\''),
									elm$parser$Parser$symbol('\''),
									elm$parser$Parser$getChompedString(
										elm$parser$Parser$chompWhile(
											function (c) {
												return (!_Utils_eq(
													c,
													_Utils_chr('\''))) && (!_Utils_eq(
													c,
													_Utils_chr('\n')));
											}))),
									A3(
									Punie$elm_parser_extras$Parser$Extras$between,
									elm$parser$Parser$symbol('\"'),
									elm$parser$Parser$symbol('\"'),
									elm$parser$Parser$getChompedString(
										elm$parser$Parser$chompWhile(
											function (c) {
												return (!_Utils_eq(
													c,
													_Utils_chr('\"'))) && (!_Utils_eq(
													c,
													_Utils_chr('\n')));
											}))),
									elm$parser$Parser$number(
									{
										binary: elm$core$Maybe$Nothing,
										_float: elm$core$Maybe$Just(elm$core$String$fromFloat),
										hex: elm$core$Maybe$Nothing,
										_int: elm$core$Maybe$Just(elm$core$String$fromInt),
										octal: elm$core$Maybe$Nothing
									})
								])),
						elm$parser$Parser$spaces),
					elm$parser$Parser$symbol(':')),
				elm$parser$Parser$spaces)),
		A2(
			elm$parser$Parser$ignorer,
			A2(
				elm$parser$Parser$ignorer,
				author$project$AstParser$expression(track),
				elm$parser$Parser$spaces),
			elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						elm$parser$Parser$symbol(','),
						elm$parser$Parser$succeed(_Utils_Tuple0)
					]))));
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			elm$parser$Parser$succeed(
				F2(
					function (pos, dict) {
						return A2(
							track,
							pos,
							author$project$Types$ObjectExpression(
								elm$core$Dict$fromList(dict)));
					})),
			elm$parser$Parser$getPosition),
		elm$parser$Parser$backtrackable(
			Punie$elm_parser_extras$Parser$Extras$braces(
				Punie$elm_parser_extras$Parser$Extras$many(objectItem))));
};
var author$project$AstParser$operationAssignment = function (track) {
	var symb = F2(
		function (operation, str) {
			return A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed(operation),
				elm$parser$Parser$symbol(str));
		});
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				A2(
					elm$parser$Parser$keeper,
					A2(
						elm$parser$Parser$keeper,
						A2(
							elm$parser$Parser$keeper,
							elm$parser$Parser$succeed(
								F6(
									function (posVar, name, posSign, operation, posEqual, expr) {
										return A2(
											track,
											posEqual,
											A2(
												author$project$Types$Operation,
												author$project$Types$Assignment(name),
												A2(
													track,
													posSign,
													A3(
														author$project$Types$Operation2,
														operation,
														A2(
															track,
															posVar,
															author$project$Types$Variable(name)),
														expr))));
									})),
							elm$parser$Parser$getPosition),
						A2(
							elm$parser$Parser$ignorer,
							elm$parser$Parser$backtrackable(author$project$AstParser$identifier),
							elm$parser$Parser$backtrackable(elm$parser$Parser$spaces))),
					elm$parser$Parser$getPosition),
				elm$parser$Parser$backtrackable(
					elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								A2(symb, author$project$Types$Addition, '+'),
								A2(symb, author$project$Types$Subtraction, '-'),
								A2(symb, author$project$Types$Division, '/'),
								A2(symb, author$project$Types$Exponentiation, '**'),
								A2(symb, author$project$Types$Multiplication, '*'),
								A2(symb, author$project$Types$Remainder, '%')
							])))),
			A2(
				elm$parser$Parser$ignorer,
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$getPosition,
					elm$parser$Parser$backtrackable(
						elm$parser$Parser$symbol('='))),
				elm$parser$Parser$spaces)),
		elm$parser$Parser$lazy(
			function (_n2) {
				return author$project$AstParser$expression(track);
			}));
};
var author$project$AstParser$postfixOperators = F2(
	function (track, expr) {
		return elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					author$project$AstParser$notSupportedPostfix(track),
					A2(
					elm$parser$Parser$andThen,
					author$project$AstParser$postfixOperators(track),
					A2(author$project$AstParser$members, track, expr)),
					A2(
					elm$parser$Parser$andThen,
					author$project$AstParser$postfixOperators(track),
					A2(author$project$AstParser$dot, track, expr)),
					A2(
					elm$parser$Parser$andThen,
					author$project$AstParser$postfixOperators(track),
					A2(author$project$AstParser$functionCall, track, expr)),
					elm$parser$Parser$succeed(expr)
				]));
	});
var author$project$AstParser$return = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			elm$parser$Parser$succeed(
				F2(
					function (pos, expr) {
						return A2(
							track,
							pos,
							author$project$Types$Return(expr));
					})),
			A2(
				elm$parser$Parser$ignorer,
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$getPosition,
					elm$parser$Parser$backtrackable(
						elm$parser$Parser$symbol('return'))),
				elm$parser$Parser$spaces)),
		author$project$AstParser$expression(track));
};
var author$project$AstParser$while = function (track) {
	return A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F3(
						function (pos, condition, expr) {
							return A2(
								track,
								pos,
								A2(author$project$Types$While, condition, expr));
						})),
				A2(
					elm$parser$Parser$ignorer,
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$getPosition,
						elm$parser$Parser$backtrackable(
							elm$parser$Parser$symbol('while'))),
					elm$parser$Parser$spaces)),
			A2(
				elm$parser$Parser$ignorer,
				Punie$elm_parser_extras$Parser$Extras$parens(
					elm$parser$Parser$lazy(
						function (_n0) {
							return author$project$AstParser$expression(track);
						})),
				elm$parser$Parser$spaces)),
		elm$parser$Parser$lazy(
			function (_n1) {
				return author$project$AstParser$expression(track);
			}));
};
var elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var author$project$AstParser$programLoop = F2(
	function (track, expressions) {
		var appendExpr = function (expr) {
			var _n0 = elm$core$List$head(expressions);
			if (((_n0.$ === 'Just') && (_n0.a.$ === 'Tracked')) && (_n0.a.b.$ === 'Block')) {
				var _n1 = _n0.a;
				var items = _n1.b.a;
				return elm$parser$Parser$Loop(
					A2(
						elm$core$List$cons,
						author$project$Types$Untracked(
							author$project$Types$Block(
								_Utils_ap(
									items,
									_List_fromArray(
										[expr])))),
						A2(elm$core$List$drop, 1, expressions)));
			} else {
				return elm$parser$Parser$Loop(
					A2(elm$core$List$cons, expr, expressions));
			}
		};
		return elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					elm$parser$Parser$ignorer,
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$succeed(
							elm$parser$Parser$Done(
								elm$core$List$reverse(expressions))),
						elm$parser$Parser$backtrackable(
							elm$parser$Parser$chompWhile(
								function (c) {
									return _Utils_eq(
										c,
										_Utils_chr(' ')) || (_Utils_eq(
										c,
										_Utils_chr('\t')) || (_Utils_eq(
										c,
										_Utils_chr('\n')) || _Utils_eq(
										c,
										_Utils_chr(';'))));
								}))),
					elm$parser$Parser$symbol('EOF')),
					elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							elm$parser$Parser$keeper,
							elm$parser$Parser$succeed(appendExpr),
							A3(author$project$AstParser$expression_, track, true, false)),
							A2(
							elm$parser$Parser$ignorer,
							elm$parser$Parser$succeed(
								elm$parser$Parser$Loop(expressions)),
							author$project$AstParser$statementBreak)
						]))
				]));
	});
var author$project$AstParser$program = function (track) {
	return A2(
		elm$parser$Parser$loop,
		_List_Nil,
		author$project$AstParser$programLoop(track));
};
var author$project$Types$Tracked = F2(
	function (a, b) {
		return {$: 'Tracked', a: a, b: b};
	});
var author$project$AstParser$tracked = F2(
	function (filename, _n0) {
		var row = _n0.a;
		var col = _n0.b;
		return author$project$Types$Tracked(
			{column: col, filename: filename, line: row});
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {col: col, problem: problem, row: row};
	});
var elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3(elm$parser$Parser$DeadEnd, p.row, p.col, p.problem);
};
var elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 'Empty':
					return list;
				case 'AddRight':
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2(elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2(elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var elm$parser$Parser$Advanced$run = F2(
	function (_n0, src) {
		var parse = _n0.a;
		var _n1 = parse(
			{col: 1, context: _List_Nil, indent: 1, offset: 0, row: 1, src: src});
		if (_n1.$ === 'Good') {
			var value = _n1.b;
			return elm$core$Result$Ok(value);
		} else {
			var bag = _n1.b;
			return elm$core$Result$Err(
				A2(elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var elm$parser$Parser$run = F2(
	function (parser, source) {
		var _n0 = A2(elm$parser$Parser$Advanced$run, parser, source);
		if (_n0.$ === 'Ok') {
			var a = _n0.a;
			return elm$core$Result$Ok(a);
		} else {
			var problems = _n0.a;
			return elm$core$Result$Err(
				A2(elm$core$List$map, elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var author$project$AstParser$parse = F2(
	function (filename, content) {
		return A2(
			elm$parser$Parser$run,
			author$project$AstParser$program(
				author$project$AstParser$tracked(filename)),
			content + '\nEOF');
	});
var author$project$Interpreter$buildUndefined2 = F4(
	function (value, value2, trackStack, reason) {
		var _n0 = _Utils_Tuple2(value, value2);
		if (_n0.a.$ === 'Undefined') {
			var undefinedStack = _n0.a.a;
			return author$project$Types$Undefined(
				_Utils_ap(
					undefinedStack,
					trackStack(reason)));
		} else {
			if (_n0.b.$ === 'Undefined') {
				var undefinedStack = _n0.b.a;
				return author$project$Types$Undefined(
					_Utils_ap(
						undefinedStack,
						trackStack(reason)));
			} else {
				return author$project$Types$Undefined(
					trackStack(reason));
			}
		}
	});
var author$project$Interpreter$valueToBool = function (value) {
	valueToBool:
	while (true) {
		switch (value.$) {
			case 'Boolean':
				var a = value.a;
				return a;
			case 'Number':
				var a = value.a;
				return (!a) ? false : true;
			case 'Abstraction':
				return true;
			case 'Array':
				return true;
			case 'Object':
				return true;
			case 'Undefined':
				return false;
			case 'String':
				if (value.a === '') {
					return false;
				} else {
					return true;
				}
			default:
				var val = value.a;
				var $temp$value = val;
				value = $temp$value;
				continue valueToBool;
		}
	}
};
var author$project$Interpreter$valueToString = function (value) {
	valueToString:
	while (true) {
		switch (value.$) {
			case 'Number':
				var a = value.a;
				return elm$core$String$fromFloat(a);
			case 'Boolean':
				var a = value.a;
				return a ? 'true' : 'false';
			case 'Array':
				var item = value.a;
				return A2(
					elm$core$String$join,
					',',
					A2(elm$core$List$map, author$project$Interpreter$valueToString, item));
			case 'Object':
				return '[object Object]';
			case 'String':
				var str = value.a;
				return str;
			case 'Undefined':
				return 'undefined';
			case 'Abstraction':
				return '[Function]';
			default:
				var val = value.a;
				var $temp$value = val;
				value = $temp$value;
				continue valueToString;
		}
	}
};
var author$project$Interpreter$valueToNumber = function (value) {
	valueToNumber:
	while (true) {
		switch (value.$) {
			case 'Number':
				var a = value.a;
				return elm$core$Maybe$Just(a);
			case 'Boolean':
				var a = value.a;
				return elm$core$Maybe$Just(
					a ? 1 : 0);
			case 'Array':
				var $temp$value = author$project$Types$String(
					author$project$Interpreter$valueToString(value));
				value = $temp$value;
				continue valueToNumber;
			case 'String':
				if (value.a === '') {
					return elm$core$Maybe$Just(0);
				} else {
					var str = value.a;
					return elm$core$String$toFloat(str);
				}
			default:
				return elm$core$Maybe$Nothing;
		}
	}
};
var author$project$Types$KeyNotInObject = F2(
	function (a, b) {
		return {$: 'KeyNotInObject', a: a, b: b};
	});
var author$project$Types$OperationWithUndefined = function (a) {
	return {$: 'OperationWithUndefined', a: a};
};
var elm$core$Basics$pow = _Basics_pow;
var elm$core$Basics$round = _Basics_round;
var elm$core$Basics$truncate = _Basics_truncate;
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var author$project$Interpreter$applyOperation2 = F4(
	function (reserved, arg0, arg1, trackStack) {
		var numberComparison = function (comparator) {
			var _n12 = _Utils_Tuple2(
				author$project$Interpreter$valueToNumber(arg0),
				author$project$Interpreter$valueToNumber(arg1));
			if ((_n12.a.$ === 'Just') && (_n12.b.$ === 'Just')) {
				var a = _n12.a.a;
				var b = _n12.b.a;
				return A2(comparator, a, b);
			} else {
				return false;
			}
		};
		var softEquality = function () {
			var _n11 = _Utils_Tuple2(arg0, arg1);
			_n11$2:
			while (true) {
				_n11$3:
				while (true) {
					_n11$4:
					while (true) {
						_n11$5:
						while (true) {
							switch (_n11.a.$) {
								case 'Undefined':
									switch (_n11.b.$) {
										case 'Undefined':
											return true;
										case 'Boolean':
											break _n11$2;
										case 'String':
											break _n11$4;
										default:
											break _n11$5;
									}
								case 'Boolean':
									return numberComparison(elm$core$Basics$eq);
								case 'String':
									switch (_n11.b.$) {
										case 'Boolean':
											break _n11$2;
										case 'String':
											break _n11$3;
										default:
											break _n11$3;
									}
								default:
									switch (_n11.b.$) {
										case 'Boolean':
											break _n11$2;
										case 'String':
											break _n11$4;
										default:
											break _n11$5;
									}
							}
						}
						return numberComparison(elm$core$Basics$eq);
					}
					var a = _n11.a;
					var b = _n11.b.a;
					return _Utils_eq(
						author$project$Interpreter$valueToString(a),
						b);
				}
				var a = _n11.a.a;
				var b = _n11.b;
				return _Utils_eq(
					a,
					author$project$Interpreter$valueToString(b));
			}
			return numberComparison(elm$core$Basics$eq);
		}();
		var buildUndefined_ = A3(author$project$Interpreter$buildUndefined2, arg0, arg1, trackStack);
		switch (reserved.$) {
			case 'Addition':
				var stringConcat = author$project$Types$String(
					_Utils_ap(
						author$project$Interpreter$valueToString(arg0),
						author$project$Interpreter$valueToString(arg1)));
				var numberSum = function () {
					var _n2 = _Utils_Tuple2(
						author$project$Interpreter$valueToNumber(arg0),
						author$project$Interpreter$valueToNumber(arg1));
					if ((_n2.a.$ === 'Just') && (_n2.b.$ === 'Just')) {
						var a = _n2.a.a;
						var b = _n2.b.a;
						return author$project$Types$Number(a + b);
					} else {
						return buildUndefined_(
							author$project$Types$OperationWithUndefined('addition'));
					}
				}();
				var _n1 = _Utils_Tuple2(arg0, arg1);
				_n1$1:
				while (true) {
					_n1$2:
					while (true) {
						_n1$3:
						while (true) {
							_n1$4:
							while (true) {
								_n1$5:
								while (true) {
									_n1$6:
									while (true) {
										_n1$7:
										while (true) {
											_n1$8:
											while (true) {
												_n1$9:
												while (true) {
													_n1$11:
													while (true) {
														switch (_n1.a.$) {
															case 'String':
																return stringConcat;
															case 'Array':
																switch (_n1.b.$) {
																	case 'String':
																		break _n1$1;
																	case 'Array':
																		break _n1$2;
																	case 'Object':
																		break _n1$2;
																	case 'Boolean':
																		break _n1$2;
																	case 'Number':
																		break _n1$2;
																	default:
																		break _n1$2;
																}
															case 'Object':
																switch (_n1.b.$) {
																	case 'String':
																		break _n1$1;
																	case 'Array':
																		break _n1$3;
																	case 'Object':
																		break _n1$4;
																	case 'Boolean':
																		break _n1$4;
																	case 'Number':
																		break _n1$4;
																	default:
																		break _n1$4;
																}
															case 'Boolean':
																switch (_n1.b.$) {
																	case 'String':
																		break _n1$1;
																	case 'Array':
																		break _n1$3;
																	case 'Object':
																		break _n1$5;
																	case 'Boolean':
																		break _n1$6;
																	case 'Number':
																		break _n1$6;
																	default:
																		break _n1$6;
																}
															case 'Number':
																switch (_n1.b.$) {
																	case 'String':
																		break _n1$1;
																	case 'Array':
																		break _n1$3;
																	case 'Object':
																		break _n1$5;
																	case 'Boolean':
																		break _n1$7;
																	case 'Number':
																		break _n1$8;
																	default:
																		break _n1$8;
																}
															case 'Undefined':
																switch (_n1.b.$) {
																	case 'String':
																		break _n1$1;
																	case 'Array':
																		break _n1$3;
																	case 'Object':
																		break _n1$5;
																	case 'Boolean':
																		break _n1$7;
																	case 'Number':
																		break _n1$9;
																	case 'Undefined':
																		return numberSum;
																	default:
																		break _n1$11;
																}
															default:
																switch (_n1.b.$) {
																	case 'String':
																		break _n1$1;
																	case 'Array':
																		break _n1$3;
																	case 'Object':
																		break _n1$5;
																	case 'Boolean':
																		break _n1$7;
																	case 'Number':
																		break _n1$9;
																	default:
																		break _n1$11;
																}
														}
													}
													return stringConcat;
												}
												return numberSum;
											}
											return numberSum;
										}
										return numberSum;
									}
									return numberSum;
								}
								return stringConcat;
							}
							return stringConcat;
						}
						return stringConcat;
					}
					return stringConcat;
				}
				return stringConcat;
			case 'Subtraction':
				var _n3 = _Utils_Tuple2(
					author$project$Interpreter$valueToNumber(arg0),
					author$project$Interpreter$valueToNumber(arg1));
				if ((_n3.a.$ === 'Just') && (_n3.b.$ === 'Just')) {
					var a = _n3.a.a;
					var b = _n3.b.a;
					return author$project$Types$Number(a - b);
				} else {
					return buildUndefined_(
						author$project$Types$OperationWithUndefined('subtraction'));
				}
			case 'Multiplication':
				var _n4 = _Utils_Tuple2(
					author$project$Interpreter$valueToNumber(arg0),
					author$project$Interpreter$valueToNumber(arg1));
				if ((_n4.a.$ === 'Just') && (_n4.b.$ === 'Just')) {
					var a = _n4.a.a;
					var b = _n4.b.a;
					return author$project$Types$Number(a * b);
				} else {
					return buildUndefined_(
						author$project$Types$OperationWithUndefined('multiplication'));
				}
			case 'Division':
				var _n5 = _Utils_Tuple2(
					author$project$Interpreter$valueToNumber(arg0),
					author$project$Interpreter$valueToNumber(arg1));
				if ((_n5.a.$ === 'Just') && (_n5.b.$ === 'Just')) {
					var a = _n5.a.a;
					var b = _n5.b.a;
					return author$project$Types$Number(a / b);
				} else {
					return buildUndefined_(
						author$project$Types$OperationWithUndefined('division'));
				}
			case 'Exponentiation':
				var _n6 = _Utils_Tuple2(
					author$project$Interpreter$valueToNumber(arg0),
					author$project$Interpreter$valueToNumber(arg1));
				if ((_n6.a.$ === 'Just') && (_n6.b.$ === 'Just')) {
					var a = _n6.a.a;
					var b = _n6.b.a;
					return author$project$Types$Number(
						A2(elm$core$Basics$pow, a, b));
				} else {
					return buildUndefined_(
						author$project$Types$OperationWithUndefined('exponentiation'));
				}
			case 'Remainder':
				var _n7 = _Utils_Tuple2(
					author$project$Interpreter$valueToNumber(arg0),
					author$project$Interpreter$valueToNumber(arg1));
				if ((_n7.a.$ === 'Just') && (_n7.b.$ === 'Just')) {
					var a = _n7.a.a;
					var b = _n7.b.a;
					return author$project$Types$Number(
						function (val) {
							return val / 10000;
						}(
							elm$core$Basics$round(a * 10000) % elm$core$Basics$round(b * 10000)));
				} else {
					return buildUndefined_(
						author$project$Types$OperationWithUndefined('remainder'));
				}
			case 'SoftEquality':
				return author$project$Types$Boolean(softEquality);
			case 'HardEquality':
				var _n8 = _Utils_Tuple2(arg0, arg1);
				if ((_n8.a.$ === 'Undefined') && (_n8.b.$ === 'Undefined')) {
					return author$project$Types$Boolean(true);
				} else {
					return author$project$Types$Boolean(
						_Utils_eq(arg0, arg1));
				}
			case 'SoftNotEquality':
				return author$project$Types$Boolean(!softEquality);
			case 'HardNotEquality':
				var _n9 = _Utils_Tuple2(arg0, arg1);
				if ((_n9.a.$ === 'Undefined') && (_n9.b.$ === 'Undefined')) {
					return author$project$Types$Boolean(false);
				} else {
					return author$project$Types$Boolean(
						!_Utils_eq(arg0, arg1));
				}
			case 'GreaterThan':
				return author$project$Types$Boolean(
					numberComparison(elm$core$Basics$gt));
			case 'SmallerThan':
				return author$project$Types$Boolean(
					numberComparison(elm$core$Basics$lt));
			case 'GreaterOrEqualThan':
				return author$project$Types$Boolean(
					numberComparison(elm$core$Basics$gt) || softEquality);
			case 'SmallerOrEqualThan':
				return author$project$Types$Boolean(
					numberComparison(elm$core$Basics$lt) || softEquality);
			case 'Member':
				var returnUndefined = buildUndefined_(
					A2(author$project$Types$KeyNotInObject, arg0, arg1));
				var _n10 = _Utils_Tuple2(arg0, arg1);
				_n10$3:
				while (true) {
					switch (_n10.a.$) {
						case 'Array':
							if (_n10.b.$ === 'Number') {
								var arr = _n10.a.a;
								var index = _n10.b.a;
								return _Utils_eq(index | 0, index) ? A2(
									elm$core$Maybe$withDefault,
									returnUndefined,
									elm$core$List$head(
										A2(elm$core$List$drop, index | 0, arr))) : returnUndefined;
							} else {
								break _n10$3;
							}
						case 'Object':
							var dict = _n10.a.a;
							var key = _n10.b;
							return A2(
								elm$core$Maybe$withDefault,
								returnUndefined,
								A2(
									elm$core$Dict$get,
									author$project$Interpreter$valueToString(key),
									dict));
						case 'String':
							if (_n10.b.$ === 'Number') {
								var str = _n10.a.a;
								var index = _n10.b.a;
								if (_Utils_eq(index | 0, index)) {
									var _char = A2(
										elm$core$String$left,
										1,
										A2(elm$core$String$dropLeft, index | 0, str));
									return (_char !== '') ? author$project$Types$String(_char) : returnUndefined;
								} else {
									return returnUndefined;
								}
							} else {
								break _n10$3;
							}
						default:
							break _n10$3;
					}
				}
				return returnUndefined;
			case 'And':
				return author$project$Types$Boolean(
					author$project$Interpreter$valueToBool(arg0) && author$project$Interpreter$valueToBool(arg1));
			default:
				return author$project$Types$Boolean(
					author$project$Interpreter$valueToBool(arg0) || author$project$Interpreter$valueToBool(arg1));
		}
	});
var author$project$Interpreter$buildUndefined = F3(
	function (value, trackStack, reason) {
		if (value.$ === 'Undefined') {
			var undefinedStack = value.a;
			return author$project$Types$Undefined(
				_Utils_ap(
					undefinedStack,
					trackStack(reason)));
		} else {
			return author$project$Types$Undefined(
				trackStack(reason));
		}
	});
var author$project$Interpreter$setVariable = F3(
	function (name, value, _n0) {
		var state = _n0.a;
		return author$project$Types$State(
			A3(elm$core$Dict$insert, name, value, state));
	});
var author$project$Types$AssignmentToUndefined = function (a) {
	return {$: 'AssignmentToUndefined', a: a};
};
var author$project$Types$Stateful = F3(
	function (outScope, inScope, result) {
		return {inScope: inScope, outScope: outScope, result: result};
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var author$project$Interpreter$applyOperation = F4(
	function (inScope, operation, arg0, trackStack) {
		var returnValue = A2(author$project$Types$Stateful, author$project$Types$emptyState, author$project$Types$emptyState);
		var buildUndefined_ = A2(author$project$Interpreter$buildUndefined, arg0, trackStack);
		switch (operation.$) {
			case 'Assignment':
				var name = operation.a;
				var assignValue = function () {
					if (arg0.$ === 'Undefined') {
						return buildUndefined_(
							author$project$Types$AssignmentToUndefined(name));
					} else {
						return arg0;
					}
				}();
				return A3(
					author$project$Types$Stateful,
					A3(
						author$project$Interpreter$setVariable,
						name,
						_Utils_Tuple2(inScope, assignValue),
						author$project$Types$emptyState),
					author$project$Types$emptyState,
					assignValue);
			case 'LetAssignment':
				var name = operation.a;
				var assignValue = function () {
					if (arg0.$ === 'Undefined') {
						return buildUndefined_(
							author$project$Types$AssignmentToUndefined(name));
					} else {
						return arg0;
					}
				}();
				return A3(
					author$project$Types$Stateful,
					author$project$Types$emptyState,
					A3(
						author$project$Interpreter$setVariable,
						name,
						_Utils_Tuple2(inScope, assignValue),
						author$project$Types$emptyState),
					assignValue);
			case 'Increment':
				var name = operation.a;
				var newValue = A4(
					author$project$Interpreter$applyOperation2,
					author$project$Types$Addition,
					arg0,
					author$project$Types$Number(1),
					trackStack);
				return A3(
					author$project$Types$Stateful,
					A3(
						author$project$Interpreter$setVariable,
						name,
						_Utils_Tuple2(inScope, newValue),
						author$project$Types$emptyState),
					author$project$Types$emptyState,
					arg0);
			case 'Decrement':
				var name = operation.a;
				var newValue = A4(
					author$project$Interpreter$applyOperation2,
					author$project$Types$Subtraction,
					arg0,
					author$project$Types$Number(1),
					trackStack);
				return A3(
					author$project$Types$Stateful,
					A3(
						author$project$Interpreter$setVariable,
						name,
						_Utils_Tuple2(inScope, newValue),
						author$project$Types$emptyState),
					author$project$Types$emptyState,
					arg0);
			case 'Not':
				return returnValue(
					author$project$Types$Boolean(
						!author$project$Interpreter$valueToBool(arg0)));
			default:
				return returnValue(
					A2(
						elm$core$Maybe$withDefault,
						buildUndefined_(
							author$project$Types$OperationWithUndefined('negative')),
						A2(
							elm$core$Maybe$map,
							author$project$Types$Number,
							A2(
								elm$core$Maybe$map,
								elm$core$Basics$mul(-1),
								author$project$Interpreter$valueToNumber(arg0)))));
		}
	});
var author$project$Interpreter$getVariable = F2(
	function (name, _n0) {
		var state = _n0.a;
		return A2(elm$core$Dict$get, name, state);
	});
var author$project$Interpreter$removeTracking = function (expr) {
	if (expr.$ === 'Tracked') {
		var e = expr.b;
		return e;
	} else {
		var e = expr.a;
		return e;
	}
};
var author$project$Stateful$andThen = F2(
	function (fn, session_) {
		return A2(fn, session_.result, session_);
	});
var author$project$Stateful$map = F2(
	function (fn, session_) {
		return {
			inScope: session_.inScope,
			outScope: session_.outScope,
			result: fn(session_.result)
		};
	});
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3(elm$core$Dict$foldl, elm$core$Dict$insert, t2, t1);
	});
var author$project$Stateful$mergeStates = F2(
	function (_n0, _n1) {
		var a = _n0.a;
		var b = _n1.a;
		return author$project$Types$State(
			A2(elm$core$Dict$union, a, b));
	});
var author$project$Stateful$getVariables = function (_n0) {
	var state = _n0.a;
	return state;
};
var elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3(elm$core$Dict$insert, k, v, d) : d;
				}),
			elm$core$Dict$empty,
			dict);
	});
var author$project$Stateful$moveStateOutsideScope = F2(
	function (_n0, expressionResult) {
		var prevOutScope = _n0.a;
		var prevInScope = _n0.b;
		var outScopeFiltered = A2(
			author$project$Stateful$mergeStates,
			author$project$Types$State(
				A2(
					elm$core$Dict$filter,
					F2(
						function (identifier, _n2) {
							return !A2(
								elm$core$Dict$member,
								identifier,
								author$project$Stateful$getVariables(prevInScope));
						}),
					author$project$Stateful$getVariables(expressionResult.outScope))),
			prevOutScope);
		var inScopeUpdated = A2(
			author$project$Stateful$mergeStates,
			author$project$Types$State(
				A2(
					elm$core$Dict$filter,
					F2(
						function (identifier, _n1) {
							return A2(
								elm$core$Dict$member,
								identifier,
								author$project$Stateful$getVariables(prevInScope));
						}),
					author$project$Stateful$getVariables(expressionResult.outScope))),
			A2(author$project$Stateful$mergeStates, expressionResult.inScope, prevInScope));
		return {inScope: inScopeUpdated, outScope: outScopeFiltered, result: expressionResult.result};
	});
var author$project$Stateful$run = F2(
	function (fn, _n0) {
		var outScope = _n0.outScope;
		var inScope = _n0.inScope;
		return A2(
			author$project$Stateful$moveStateOutsideScope,
			_Utils_Tuple2(outScope, inScope),
			fn(
				A2(author$project$Stateful$mergeStates, inScope, outScope)));
	});
var author$project$Stateful$runInScope = F2(
	function (fn, _n0) {
		var outScope = _n0.outScope;
		var inScope = _n0.inScope;
		return A2(
			author$project$Stateful$moveStateOutsideScope,
			_Utils_Tuple2(outScope, inScope),
			fn(inScope));
	});
var author$project$Stateful$session = function (state) {
	return {
		inScope: author$project$Types$emptyState,
		outScope: state,
		result: author$project$Types$Undefined(_List_Nil)
	};
};
var author$project$Types$Array = function (a) {
	return {$: 'Array', a: a};
};
var author$project$Types$IfWithoutElse = {$: 'IfWithoutElse'};
var author$project$Types$LoopNeverTrue = {$: 'LoopNeverTrue'};
var author$project$Types$MissingPositionalArgument = F2(
	function (a, b) {
		return {$: 'MissingPositionalArgument', a: a, b: b};
	});
var author$project$Types$NotAFunction = function (a) {
	return {$: 'NotAFunction', a: a};
};
var author$project$Types$Object = function (a) {
	return {$: 'Object', a: a};
};
var author$project$Types$ReturnValue = function (a) {
	return {$: 'ReturnValue', a: a};
};
var author$project$Types$VariableNotDefined = function (a) {
	return {$: 'VariableNotDefined', a: a};
};
var author$project$Types$VoidReturn = {$: 'VoidReturn'};
var elm$core$Dict$values = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2(elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var elm_community$list_extra$List$Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _n0) {
				var i = _n0.a;
				var thisAcc = _n0.b;
				return _Utils_Tuple2(
					i + 1,
					A3(func, i, x, thisAcc));
			});
		return A3(
			elm$core$List$foldl,
			step,
			_Utils_Tuple2(0, acc),
			list).b;
	});
var author$project$Interpreter$callFunction = F4(
	function (trackStack, _n9, args, state) {
		var paramNames = _n9.a;
		var functionBody = _n9.b;
		var argOrDefault = F2(
			function (index, paramName) {
				return A2(
					elm$core$Maybe$withDefault,
					author$project$Types$Undefined(
						trackStack(
							A2(author$project$Types$MissingPositionalArgument, index, paramName))),
					elm$core$List$head(
						A2(elm$core$List$drop, index, args)));
			});
		var inState = A3(
			elm_community$list_extra$List$Extra$indexedFoldl,
			F2(
				function (index, paramName) {
					return A2(
						author$project$Interpreter$setVariable,
						paramName,
						_Utils_Tuple2(
							state,
							A2(argOrDefault, index, paramName)));
				}),
			author$project$Types$emptyState,
			paramNames);
		var closure = A2(author$project$Stateful$mergeStates, inState, state);
		var expressionResult = A2(
			author$project$Stateful$map,
			function (result) {
				var _n10 = _Utils_Tuple2(
					result,
					author$project$Interpreter$removeTracking(functionBody));
				if (_n10.a.$ === 'ReturnValue') {
					var val = _n10.a.a;
					return val;
				} else {
					if (_n10.b.$ === 'Block') {
						return author$project$Types$Undefined(
							trackStack(author$project$Types$VoidReturn));
					} else {
						return result;
					}
				}
			},
			A2(
				author$project$Stateful$moveStateOutsideScope,
				_Utils_Tuple2(state, inState),
				A2(author$project$Interpreter$eval, functionBody, closure)));
		return expressionResult;
	});
var author$project$Interpreter$eval = F2(
	function (expr, state) {
		var trackStack = function (reason) {
			if (expr.$ === 'Tracked') {
				var info = expr.a;
				return _List_fromArray(
					[
						{column: info.column, filename: info.filename, line: info.line, reason: reason}
					]);
			} else {
				return _List_Nil;
			}
		};
		var _return = function (result) {
			return {inScope: author$project$Types$emptyState, outScope: author$project$Types$emptyState, result: result};
		};
		var _n1 = author$project$Interpreter$removeTracking(expr);
		switch (_n1.$) {
			case 'Value':
				var val = _n1.a;
				return _return(val);
			case 'ArrayExpression':
				var items = _n1.a;
				return A2(
					author$project$Stateful$map,
					author$project$Types$Array,
					A2(author$project$Interpreter$evalList, items, state));
			case 'ObjectExpression':
				var dict = _n1.a;
				return A2(
					author$project$Stateful$map,
					function (result) {
						return author$project$Types$Object(
							elm$core$Dict$fromList(
								A3(
									elm$core$List$map2,
									elm$core$Tuple$pair,
									elm$core$Dict$keys(dict),
									result)));
					},
					A2(
						author$project$Interpreter$evalList,
						elm$core$Dict$values(dict),
						state));
			case 'Variable':
				var identifier = _n1.a;
				return A2(
					elm$core$Maybe$withDefault,
					_return(
						author$project$Types$Undefined(
							trackStack(
								author$project$Types$VariableNotDefined(identifier)))),
					A2(
						elm$core$Maybe$map,
						function (_n2) {
							var state_ = _n2.a;
							var result = _n2.b;
							return {inScope: state_, outScope: author$project$Types$emptyState, result: result};
						},
						A2(author$project$Interpreter$getVariable, identifier, state)));
			case 'Operation':
				var symbol = _n1.a;
				var expr0 = _n1.b;
				return A2(
					author$project$Stateful$andThen,
					function (arg0) {
						return author$project$Stateful$runInScope(
							function (inScope) {
								return A4(author$project$Interpreter$applyOperation, inScope, symbol, arg0, trackStack);
							});
					},
					A2(
						author$project$Stateful$run,
						author$project$Interpreter$eval(expr0),
						author$project$Stateful$session(state)));
			case 'Operation2':
				var symbol = _n1.a;
				var expr0 = _n1.b;
				var expr1 = _n1.c;
				return A2(
					author$project$Stateful$andThen,
					function (arg0) {
						return A2(
							elm$core$Basics$composeR,
							author$project$Stateful$run(
								author$project$Interpreter$eval(expr1)),
							author$project$Stateful$andThen(
								function (arg1) {
									return author$project$Stateful$run(
										function (_n3) {
											return _return(
												A4(author$project$Interpreter$applyOperation2, symbol, arg0, arg1, trackStack));
										});
								}));
					},
					A2(
						author$project$Stateful$run,
						author$project$Interpreter$eval(expr0),
						author$project$Stateful$session(state)));
			case 'Application':
				var fn = _n1.a;
				var args = _n1.b;
				var call = F2(
					function (evaluatedArgs, abstraction) {
						if (abstraction.$ === 'Abstraction') {
							var paramNames = abstraction.a;
							var functionBody = abstraction.b;
							return author$project$Stateful$run(
								A3(
									author$project$Interpreter$callFunction,
									trackStack,
									_Utils_Tuple2(paramNames, functionBody),
									evaluatedArgs));
						} else {
							return author$project$Stateful$run(
								function (_n5) {
									return _return(
										A3(
											author$project$Interpreter$buildUndefined,
											abstraction,
											trackStack,
											author$project$Types$NotAFunction(abstraction)));
								});
						}
					});
				return A2(
					author$project$Stateful$andThen,
					function (evaluatedArgs) {
						return A2(
							elm$core$Basics$composeR,
							author$project$Stateful$run(
								author$project$Interpreter$eval(fn)),
							author$project$Stateful$andThen(
								call(evaluatedArgs)));
					},
					A2(author$project$Interpreter$evalList, args, state));
			case 'Block':
				var blockExpressions = _n1.a;
				return A3(author$project$Interpreter$runBlock, state, blockExpressions, trackStack);
			case 'Return':
				var returnExpr = _n1.a;
				return A2(
					author$project$Stateful$map,
					author$project$Types$ReturnValue,
					A2(author$project$Interpreter$eval, returnExpr, state));
			case 'IfCondition':
				var condition = _n1.a;
				var exprIfTrue = _n1.b;
				return A2(
					author$project$Stateful$andThen,
					function (conditionResult) {
						return author$project$Interpreter$valueToBool(conditionResult) ? author$project$Stateful$run(
							author$project$Interpreter$eval(exprIfTrue)) : author$project$Stateful$run(
							author$project$Interpreter$eval(
								author$project$Types$Untracked(
									author$project$Types$Value(
										author$project$Types$Undefined(
											trackStack(author$project$Types$IfWithoutElse))))));
					},
					A2(
						author$project$Stateful$run,
						author$project$Interpreter$eval(condition),
						author$project$Stateful$session(state)));
			case 'IfElseCondition':
				var condition = _n1.a;
				var exprIfTrue = _n1.b;
				var exprIfFalse = _n1.c;
				return A2(
					author$project$Stateful$andThen,
					function (conditionResult) {
						return author$project$Interpreter$valueToBool(conditionResult) ? author$project$Stateful$run(
							author$project$Interpreter$eval(exprIfTrue)) : author$project$Stateful$run(
							author$project$Interpreter$eval(exprIfFalse));
					},
					A2(
						author$project$Stateful$run,
						author$project$Interpreter$eval(condition),
						author$project$Stateful$session(state)));
			case 'While':
				var condition = _n1.a;
				var exprWhile = _n1.b;
				var whileLoop = F2(
					function (prevResult, session) {
						return A2(
							author$project$Stateful$andThen,
							function (conditionResult) {
								return author$project$Interpreter$valueToBool(conditionResult) ? A2(
									elm$core$Basics$composeR,
									author$project$Stateful$run(
										author$project$Interpreter$eval(exprWhile)),
									author$project$Stateful$andThen(whileLoop)) : author$project$Stateful$map(
									function (_n6) {
										return prevResult;
									});
							},
							A2(
								author$project$Stateful$run,
								author$project$Interpreter$eval(condition),
								session));
					});
				return A2(
					whileLoop,
					author$project$Types$Undefined(
						trackStack(author$project$Types$LoopNeverTrue)),
					author$project$Stateful$session(state));
			default:
				var assignment = _n1.a;
				var condition = _n1.b;
				var increment = _n1.c;
				var exprFor = _n1.d;
				var forLoop = F2(
					function (prevResult, session) {
						return A2(
							author$project$Stateful$andThen,
							function (conditionResult) {
								return author$project$Interpreter$valueToBool(conditionResult) ? A2(
									elm$core$Basics$composeR,
									author$project$Stateful$run(
										author$project$Interpreter$eval(exprFor)),
									A2(
										elm$core$Basics$composeR,
										author$project$Stateful$run(
											author$project$Interpreter$eval(increment)),
										author$project$Stateful$andThen(forLoop))) : author$project$Stateful$map(
									function (_n7) {
										return prevResult;
									});
							},
							A2(
								author$project$Stateful$run,
								author$project$Interpreter$eval(condition),
								session));
					});
				return A2(
					forLoop,
					author$project$Types$Undefined(
						trackStack(author$project$Types$LoopNeverTrue)),
					A2(
						author$project$Stateful$run,
						author$project$Interpreter$eval(assignment),
						author$project$Stateful$session(state)));
		}
	});
var author$project$Interpreter$evalList = F2(
	function (expressions, state) {
		var iterate_ = F2(
			function (expr, acc) {
				return A2(
					author$project$Stateful$map,
					function (result) {
						return A2(elm$core$List$cons, result, acc.result);
					},
					A2(
						author$project$Stateful$run,
						author$project$Interpreter$eval(expr),
						acc));
			});
		return A2(
			author$project$Stateful$map,
			elm$core$List$reverse,
			A3(
				elm$core$List$foldl,
				iterate_,
				A3(author$project$Types$Stateful, state, author$project$Types$emptyState, _List_Nil),
				expressions));
	});
var author$project$Interpreter$runBlock = F3(
	function (state, blockExpressions, trackStack) {
		var iterate_ = F2(
			function (expr, acc) {
				var _n0 = acc.result;
				if (_n0.$ === 'ReturnValue') {
					return acc;
				} else {
					return A2(
						author$project$Stateful$run,
						author$project$Interpreter$eval(expr),
						acc);
				}
			});
		return A3(
			elm$core$List$foldl,
			iterate_,
			A3(
				author$project$Types$Stateful,
				state,
				author$project$Types$emptyState,
				author$project$Types$Undefined(
					trackStack(author$project$Types$VoidReturn))),
			blockExpressions);
	});
var author$project$Interpreter$run = F2(
	function (state, expressions) {
		var iterate_ = F2(
			function (expr, acc) {
				var statefulResult = A2(
					author$project$Stateful$run,
					author$project$Interpreter$eval(expr),
					acc);
				var outScope = function () {
					var _n2 = author$project$Interpreter$removeTracking(expr);
					if ((_n2.$ === 'Operation') && (_n2.a.$ === 'LetAssignment')) {
						return A2(author$project$Stateful$mergeStates, statefulResult.inScope, statefulResult.outScope);
					} else {
						return statefulResult.outScope;
					}
				}();
				return A2(
					author$project$Stateful$map,
					function (_n1) {
						return A2(elm$core$List$cons, statefulResult, acc.result);
					},
					_Utils_update(
						statefulResult,
						{inScope: author$project$Types$emptyState, outScope: outScope}));
			});
		return function (_n0) {
			var outScope = _n0.outScope;
			var inScope = _n0.inScope;
			var result = _n0.result;
			return _Utils_Tuple2(
				A2(author$project$Stateful$mergeStates, inScope, outScope),
				elm$core$List$reverse(result));
		}(
			A3(
				elm$core$List$foldl,
				iterate_,
				A3(author$project$Types$Stateful, state, author$project$Types$emptyState, _List_Nil),
				expressions));
	});
var author$project$Playground$Routes$Playground = {$: 'Playground'};
var elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var elm$url$Url$Parser$mapState = F2(
	function (func, _n0) {
		var visited = _n0.visited;
		var unvisited = _n0.unvisited;
		var params = _n0.params;
		var frag = _n0.frag;
		var value = _n0.value;
		return A5(
			elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var elm$url$Url$Parser$map = F2(
	function (subValue, _n0) {
		var parseArg = _n0.a;
		return elm$url$Url$Parser$Parser(
			function (_n1) {
				var visited = _n1.visited;
				var unvisited = _n1.unvisited;
				var params = _n1.params;
				var frag = _n1.frag;
				var value = _n1.value;
				return A2(
					elm$core$List$map,
					elm$url$Url$Parser$mapState(value),
					parseArg(
						A5(elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var elm$url$Url$Parser$oneOf = function (parsers) {
	return elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				elm$core$List$concatMap,
				function (_n0) {
					var parser = _n0.a;
					return parser(state);
				},
				parsers);
		});
};
var elm$url$Url$Parser$top = elm$url$Url$Parser$Parser(
	function (state) {
		return _List_fromArray(
			[state]);
	});
var author$project$Playground$Routes$routes = elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2(elm$url$Url$Parser$map, author$project$Playground$Routes$Playground, elm$url$Url$Parser$top)
		]));
var author$project$Playground$Routes$toPath = function (page) {
	return '/';
};
var author$project$Playground$Types$AddCell = {$: 'AddCell'};
var author$project$Playground$Types$NoOp = {$: 'NoOp'};
var author$project$Playground$Types$RunCell = {$: 'RunCell'};
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$Dom$focus = _Browser_call('focus');
var elm$browser$Browser$Navigation$load = _Browser_load;
var elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$core$String$trim = _String_trim;
var elm$core$Task$onError = _Scheduler_onError;
var elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(
					elm$core$Task$onError,
					A2(
						elm$core$Basics$composeL,
						A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
						elm$core$Result$Err),
					A2(
						elm$core$Task$andThen,
						A2(
							elm$core$Basics$composeL,
							A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
							elm$core$Result$Ok),
						task))));
	});
var elm$core$Tuple$mapSecond = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + elm$core$String$fromInt(port_));
		}
	});
var elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var elm$url$Url$toString = function (url) {
	var http = function () {
		var _n0 = url.protocol;
		if (_n0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _n1 = state.unvisited;
			if (!_n1.b) {
				return elm$core$Maybe$Just(state.value);
			} else {
				if ((_n1.a === '') && (!_n1.b.b)) {
					return elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				elm$core$List$cons,
				segment,
				elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var elm$url$Url$Parser$preparePath = function (path) {
	var _n0 = A2(elm$core$String$split, '/', path);
	if (_n0.b && (_n0.a === '')) {
		var segments = _n0.b;
		return elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _n0;
		return elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === 'RBNode_elm_builtin') {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === 'RBNode_elm_builtin') {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === 'RBNode_elm_builtin') {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (_n0.$ === 'Just') {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$url$Url$percentDecode = _Url_percentDecode;
var elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return elm$core$Maybe$Just(
				A2(elm$core$List$cons, value, list));
		}
	});
var elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _n0 = A2(elm$core$String$split, '=', segment);
		if ((_n0.b && _n0.b.b) && (!_n0.b.b.b)) {
			var rawKey = _n0.a;
			var _n1 = _n0.b;
			var rawValue = _n1.a;
			var _n2 = elm$url$Url$percentDecode(rawKey);
			if (_n2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _n2.a;
				var _n3 = elm$url$Url$percentDecode(rawValue);
				if (_n3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _n3.a;
					return A3(
						elm$core$Dict$update,
						key,
						elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			elm$core$List$foldr,
			elm$url$Url$Parser$addParam,
			elm$core$Dict$empty,
			A2(elm$core$String$split, '&', qry));
	}
};
var elm$url$Url$Parser$parse = F2(
	function (_n0, url) {
		var parser = _n0.a;
		return elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					elm$url$Url$Parser$State,
					_List_Nil,
					elm$url$Url$Parser$preparePath(url.path),
					elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					elm$core$Basics$identity)));
	});
var elm_community$list_extra$List$Extra$getAt = F2(
	function (idx, xs) {
		return (idx < 0) ? elm$core$Maybe$Nothing : elm$core$List$head(
			A2(elm$core$List$drop, idx, xs));
	});
var elm_community$list_extra$List$Extra$last = function (items) {
	last:
	while (true) {
		if (!items.b) {
			return elm$core$Maybe$Nothing;
		} else {
			if (!items.b.b) {
				var x = items.a;
				return elm$core$Maybe$Just(x);
			} else {
				var rest = items.b;
				var $temp$items = rest;
				items = $temp$items;
				continue last;
			}
		}
	}
};
var elm_community$list_extra$List$Extra$updateIfIndex = F3(
	function (predicate, update, list) {
		return A2(
			elm$core$List$indexedMap,
			F2(
				function (i, x) {
					return predicate(i) ? update(x) : x;
				}),
			list);
	});
var author$project$Playground$update = F2(
	function (msg, model) {
		update:
		while (true) {
			switch (msg.$) {
				case 'NoOp':
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				case 'UpdateInput':
					var index = msg.a;
					var state = msg.b.state;
					var textValue = msg.b.textValue;
					var updateCell = function (cell_) {
						return _Utils_update(
							cell_,
							{autoexpand: state, input: textValue});
					};
					return _Utils_eq(index, model.selectedCell) ? _Utils_Tuple2(
						_Utils_update(
							model,
							{
								cells: A3(
									elm_community$list_extra$List$Extra$updateIfIndex,
									elm$core$Basics$eq(index),
									updateCell,
									model.cells)
							}),
						elm$core$Platform$Cmd$none) : _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				case 'AddCell':
					var index = elm$core$List$length(model.cells);
					return A2(
						author$project$Playground$update,
						author$project$Playground$Types$SelectCell(index),
						_Utils_update(
							model,
							{
								cells: _Utils_ap(
									model.cells,
									_List_fromArray(
										[
											A2(author$project$Playground$newCell, index, '')
										]))
							}));
				case 'SelectCell':
					var index = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{selectedCell: index}),
						A2(
							elm$core$Task$attempt,
							function (_n1) {
								return author$project$Playground$Types$NoOp;
							},
							elm$browser$Browser$Dom$focus(
								'cellinput-' + elm$core$String$fromInt(index))));
				case 'RunCell':
					var runCell = function (cell_) {
						return elm$core$String$isEmpty(
							elm$core$String$trim(cell_.input)) ? elm$core$Result$Ok(
							_Utils_Tuple2(model.state, elm$core$Maybe$Nothing)) : A2(
							elm$core$Result$map,
							function (_n4) {
								var state = _n4.a;
								var results = _n4.b;
								return _Utils_Tuple2(
									state,
									elm_community$list_extra$List$Extra$last(results));
							},
							A2(
								elm$core$Result$map,
								author$project$Interpreter$run(model.state),
								A2(
									author$project$AstParser$parse,
									'Cell ' + elm$core$String$fromInt(model.selectedCell),
									cell_.input)));
					};
					var result = A2(
						elm$core$Maybe$withDefault,
						elm$core$Result$Ok(
							_Utils_Tuple2(model.state, elm$core$Maybe$Nothing)),
						A2(
							elm$core$Maybe$map,
							runCell,
							A2(elm_community$list_extra$List$Extra$getAt, model.selectedCell, model.cells)));
					var updateCell = function (cell_) {
						return _Utils_update(
							cell_,
							{result: result, submittedInput: cell_.input});
					};
					var updated = function () {
						var state = function () {
							if (result.$ === 'Ok') {
								var _n3 = result.a;
								var state_ = _n3.a;
								return state_;
							} else {
								return model.state;
							}
						}();
						return A2(
							author$project$Playground$update,
							author$project$Playground$Types$SelectCell(model.selectedCell + 1),
							_Utils_update(
								model,
								{
									cells: A3(
										elm_community$list_extra$List$Extra$updateIfIndex,
										elm$core$Basics$eq(model.selectedCell),
										updateCell,
										model.cells),
									state: state
								}));
					}();
					var updatedCmd = updated.b;
					var updatedModel = updated.a;
					return _Utils_eq(
						updatedModel.selectedCell,
						elm$core$List$length(model.cells)) ? A2(
						elm$core$Tuple$mapSecond,
						function (cmd) {
							return elm$core$Platform$Cmd$batch(
								_List_fromArray(
									[cmd, updatedCmd]));
						},
						A2(author$project$Playground$update, author$project$Playground$Types$AddCell, updatedModel)) : updated;
				case 'ClearPlayground':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								cells: _List_fromArray(
									[
										A2(author$project$Playground$newCell, 0, '')
									]),
								selectedCell: -1,
								state: author$project$Types$emptyState
							}),
						elm$core$Platform$Cmd$none);
				case 'SetExample':
					var example = msg.a;
					switch (example.$) {
						case 'Basics':
							return A2(
								author$project$Playground$update,
								author$project$Playground$Types$SelectCell(0),
								_Utils_update(
									model,
									{
										cells: _List_fromArray(
											[
												A2(author$project$Playground$newCell, 0, '1 + 1'),
												A2(author$project$Playground$newCell, 1, '\\frac{25}{2}'),
												A2(author$project$Playground$newCell, 2, '12!'),
												A2(author$project$Playground$newCell, 3, 'x = 5\n\\mathbf{y} = (1, 2, 3)'),
												A2(author$project$Playground$newCell, 4, '\\sqrt{x}'),
												A2(author$project$Playground$newCell, 5, '\\sum_{i = 1}^{100} (2 * i + 1)'),
												A2(author$project$Playground$newCell, 6, 'f(x) = x + 1\nf(5)'),
												A2(author$project$Playground$newCell, 7, 'f(\\mathbf{v})_{j} = v_{j} + 1\nf(\\mathbf{y})')
											]),
										state: author$project$Types$emptyState
									}));
						case 'Softmax':
							return A2(
								author$project$Playground$update,
								author$project$Playground$Types$SelectCell(0),
								_Utils_update(
									model,
									{
										cells: _List_fromArray(
											[
												A2(author$project$Playground$newCell, 0, '\\sigma(\\mathbf{z})_{j}=\\frac{e^{z_{j}}}{\\sum_{k=1}^{n} e^{z_{k}}}'),
												A2(author$project$Playground$newCell, 1, '\\mathbf{v} = (1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0)\nn = 7\n\\sigma(\\mathbf{v})'),
												A2(author$project$Playground$newCell, 2, '\\sum_{i = 1}^{n} \\sigma(\\mathbf{v})_{i}')
											]),
										state: author$project$Types$emptyState
									}));
						case 'Bitcoin':
							return A2(
								author$project$Playground$update,
								author$project$Playground$Types$SelectCell(0),
								_Utils_update(
									model,
									{
										cells: _List_fromArray(
											[
												A2(author$project$Playground$newCell, 0, 'q = 0.1\nz = 2\np = 1 - q\n\\lambda = z * \\frac{q}{p}'),
												A2(author$project$Playground$newCell, 1, '1 - \\sum_{k = 0}^{z} \\frac{(\\lambda ^ k) * e ^ {-\\lambda}}{k!} * (1 - (q / p) ^ {(z - k)})')
											]),
										selectedCell: 0,
										state: author$project$Types$emptyState
									}));
						default:
							return A2(
								author$project$Playground$update,
								author$project$Playground$Types$SelectCell(0),
								_Utils_update(
									model,
									{
										cells: _List_fromArray(
											[
												A2(author$project$Playground$newCell, 0, '\\mathbf{x} = (1, 3, 3, 6, 7, 8, 9)\nn = |\\mathbf{x}|'),
												A2(author$project$Playground$newCell, 1, 'Mean:\n\\bar{x} = \\frac{\\sum{\\mathbf{x}}}{n}'),
												A2(author$project$Playground$newCell, 2, 'Median:\n\\tilde{x} = x_{(n \\div 2 + 1)}'),
												A2(author$project$Playground$newCell, 3, 'Quartiles\\ and\\ IQR:\n\\operatorname{Q1} = x_{(n \\div 4 + 1)}\n\\operatorname{Q3} = x_{(n - n \\div 4)}\n\\operatorname{IQR} = \\operatorname{Q3} - \\operatorname{Q1}'),
												A2(author$project$Playground$newCell, 4, 'Outliers:\n\\operatorname{lower} = \\operatorname{Q1} - 1.5 * \\operatorname{IQR}\n\\operatorname{upper} = \\operatorname{Q3} + 1.5 * \\operatorname{IQR}\n(\\operatorname{lower}, \\operatorname{upper})'),
												A2(author$project$Playground$newCell, 5, 'Variance:\nv = \\frac{\\sum_{i = 1}^{n} (x_{i} - \\bar{x}) ^ 2}{n - 1}'),
												A2(author$project$Playground$newCell, 6, 'Standard\\ Deviation:\ns = \\sqrt{v}'),
												A2(author$project$Playground$newCell, 7, 'Z-Score:\nz(\\mathbf{x})_{i} = \\frac{x_{i} - \\bar{x}}{s}\nz(\\mathbf{x})'),
												A2(author$project$Playground$newCell, 8, 'Pearson\'s\\ R:\n\\mathbf{x} = (50, 100, 200, 300)\n\\bar{x} = 162.5\ns = 110.9\n\\mathbf{a} = z(\\mathbf{x})\n\n\\mathbf{y} = (50, 70, 70, 95)\n\\bar{x} = 71.3\ns = 18.4\n\\mathbf{b} = z(\\mathbf{y})\n\nn = |\\mathbf{x}|\nr = \\frac{\\sum_{i = 1}^{n} a_{i}*b_{i}}{n - 1}\n'),
												A2(author$project$Playground$newCell, 9, 'Regression\\ Coefficient:\n\\operatorname{Sx} = 110.9\n\\operatorname{Sy} = 18.4\nb = r * (\\frac{\\operatorname{Sy}}{\\operatorname{Sx}})'),
												A2(author$project$Playground$newCell, 10, 'Intercept:\n\\bar{x} = 162.5\n\\bar{y} = 71.3\na = \\bar{y} - b * \\bar{x}'),
												A2(author$project$Playground$newCell, 11, 'Regression\\ Line:\ny(\\mathbf{x})_{i} = a + b * x_{i}\ny(\\mathbf{x})'),
												A2(author$project$Playground$newCell, 12, 'Coefficient\\ of\\ Determination:\nr ^ 2'),
												A2(author$project$Playground$newCell, 13, 'Probability\\ Mass\\ Function:\n\\mathbf{x} = (1, 2, 3, 4, 5, 6)\n\\mathbf{f} = (10, 20, 40, 80, 40, 20)\np(\\mathbf{x})_{i} = \\frac{f_{i}}{\\sum{\\mathbf{f}}}\np(\\mathbf{x})'),
												A2(author$project$Playground$newCell, 14, 'Expected\\ Value:\nn = |\\mathbf{x}|\n\\operatorname{E}(\\mathbf{x}) = \\sum_{i = 1}^{n} x_{i} * p(\\mathbf{x})_{i}\n\\mu = \\operatorname{E}(\\mathbf{x})'),
												A2(author$project$Playground$newCell, 15, 'Variance\\ of\\ a\\ Random\\ Value:\ng(\\mathbf{x})_{i} = (x_{i} - \\mu) ^ 2\nv = \\operatorname{E}(g(\\mathbf{x}))\n\\sigma = \\sqrt{v}'),
												A2(author$project$Playground$newCell, 16, 'Normal\\ Distribution:\nf(x) = \\frac{1}{\\sigma * \\sqrt{2 * \\pi}} * e ^ {-0.5 * (\\frac{x - \\mu}{\\sigma}) ^ 2}\nf(4)'),
												A2(author$project$Playground$newCell, 17, 'Standard\\ Normal\\ Distribution:\nx = \\mu + z * \\sigma'),
												A2(author$project$Playground$newCell, 18, 'Binomial\\ Distribution:\nn = 4\np = 0.48\n\\operatorname{P}(x) = \\frac{n!}{x! * (n-x)!} * p ^ x * (1 - p) ^ {n - x}\n\\operatorname{P}(3)'),
												A2(author$project$Playground$newCell, 19, 'Cumulative\\ Binomial:\n\\operatorname{F}(x) = \\sum_{k = 0}^{x} \\frac{n!}{k! * (n-k)!} * p ^ k * (1 - p) ^ {n - k}\n\\operatorname{F}(3)'),
												A2(author$project$Playground$newCell, 20, 'Binomial\\ Mean:\n\\mu = n * p'),
												A2(author$project$Playground$newCell, 21, 'Binomial\\ Standard\\ Deviation:\n\\sigma = \\sqrt{n * p * (1 - p)}'),
												A2(author$project$Playground$newCell, 22, 'Sampling\\ Distribution\\ of\\ the\\ Sample\\ Mean:\n\\mu = 3.85\n\\sigma = 1.25\nn = 6\n\\bar{\\mu} = \\mu\n\\bar{\\sigma} = \\frac{\\sigma}{\\sqrt{n}}'),
												A2(author$project$Playground$newCell, 23, 'Sampling\\ Proportion\\ Standard\\ Deviation:\n\\pi = 0.2\nn = 400\n\\bar{\\sigma} = \\sqrt{\\frac{\\pi * (1 - \\pi)}{n}}'),
												A2(author$project$Playground$newCell, 24, 'Confidence\\ Interval\\ with\\ Population\\ Standard\\ Deviation:\n\\sigma = 0.8\n\\bar{x} = 3.8\nn = 150\nz = 1.96\n\\operatorname{SE} = \\frac{\\sigma}{\\sqrt{n}}\n\\operatorname{CI} = z * \\operatorname{SE}\n(\\bar{x} - \\operatorname{CI}, \\bar{x} + \\operatorname{CI})'),
												A2(author$project$Playground$newCell, 25, 'Confidence\\ Interval\\ without\\ Population\\ Standard\\ Deviation:\ns = 0.7\nt = 1.96\n\\operatorname{SE} = \\frac{s}{\\sqrt{n}}\n\\operatorname{CI} = t * \\operatorname{SE}\n(\\bar{x} - \\operatorname{CI}, \\bar{x} + \\operatorname{CI})\n                            '),
												A2(author$project$Playground$newCell, 26, 'Confidence\\ Interval\\ for\\ Proportion\\ without\\ Population\\ Standard\\ Deviation:\nn = 55\np = 0.77\nz = 2.58\n\\operatorname{SE} = \\sqrt{\\frac{p * (1 - p)}{n}}\n\\operatorname{CI} = z * \\operatorname{SE}\n(p - \\operatorname{CI}, p + \\operatorname{CI})'),
												A2(author$project$Playground$newCell, 27, 'Selecting\\ a\\ Sample\\ Size:\n\\sigma = 1.25\nz = 1.96\nm = 0.3\nn = \\frac{\\sigma ^ 2 * z ^ 2}{m ^ 2}'),
												A2(author$project$Playground$newCell, 28, 'Selecting\\ a\\ Sample\\ Size\\ for\\ Proportion:\np = 0.5\nz = 1.645\nm = 0.1\nn = \\frac{p * (1 - p) * z ^ 2}{m ^ 2}'),
												A2(author$project$Playground$newCell, 29, 'Hypothesis\\ Testing:\n\\operatorname{null} = 68\nn = 40\n\\bar{x} = 64\ns = 3\n\\alpha = 0.05\n\\operatorname{SE} = \\frac{s}{\\sqrt{n}}\nt = \\frac{\\bar{x} - \\operatorname{null}}{\\operatorname{SE}}\n\\operatorname{df} = n - 1\n\\operatorname{t95} = 2.042\n(t, -\\operatorname{t95}, \\operatorname{t95})'),
												A2(author$project$Playground$newCell, 30, 'Hypothesis\\ Testing\\ for\\ Proportion:\n\\operatorname{null} = 0.86\nn = 900\np = 0.84\n\\alpha = 0.05\n\\operatorname{SE} = \\sqrt{\\frac{\\operatorname{null} * (1 - \\operatorname{null})}{n}}\nz = \\frac{p - \\operatorname{null}}{\\operatorname{SE}}\n\\operatorname{z95} = 1.96\n(-\\operatorname{z95}, z, \\operatorname{z95})')
											]),
										state: author$project$Types$emptyState
									}));
					}
				case 'KeyDown':
					var key = msg.a;
					if ((key.$ === 'Just') && (key.a === 13)) {
						var $temp$msg = author$project$Playground$Types$RunCell,
							$temp$model = model;
						msg = $temp$msg;
						model = $temp$model;
						continue update;
					} else {
						return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
					}
				case 'OnUrlChange':
					var url = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								page: A2(
									elm$core$Maybe$withDefault,
									author$project$Playground$Routes$Playground,
									A2(elm$url$Url$Parser$parse, author$project$Playground$Routes$routes, url))
							}),
						elm$core$Platform$Cmd$none);
				case 'OnUrlRequest':
					var urlRequest = msg.a;
					if (urlRequest.$ === 'Internal') {
						var url = urlRequest.a;
						return _Utils_Tuple2(
							model,
							A2(
								elm$browser$Browser$Navigation$pushUrl,
								model.key,
								elm$url$Url$toString(url)));
					} else {
						var url = urlRequest.a;
						return _Utils_Tuple2(
							model,
							elm$browser$Browser$Navigation$load(url));
					}
				default:
					var page = msg.a;
					return _Utils_Tuple2(
						model,
						A2(
							elm$browser$Browser$Navigation$pushUrl,
							model.key,
							author$project$Playground$Routes$toPath(page)));
			}
		}
	});
var author$project$Playground$Types$OnUrlChange = function (a) {
	return {$: 'OnUrlChange', a: a};
};
var author$project$Playground$init = F3(
	function (flags, url, key) {
		return A2(
			author$project$Playground$update,
			author$project$Playground$Types$OnUrlChange(url),
			{
				cells: _List_fromArray(
					[
						A2(author$project$Playground$newCell, 0, '')
					]),
				key: key,
				page: author$project$Playground$Routes$Playground,
				selectedCell: -1,
				state: author$project$Types$emptyState
			});
	});
var elm$html$Html$div = _VirtualDom_node('div');
var author$project$Playground$Components$row = elm$html$Html$div;
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var author$project$Playground$cellLabelView = F2(
	function (attrs, str) {
		return A2(
			author$project$Playground$Components$row,
			_Utils_ap(
				attrs,
				_List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'width', '90px'),
						A2(elm$html$Html$Attributes$style, 'padding-right', '5px'),
						A2(elm$html$Html$Attributes$style, 'padding-top', '8px')
					])),
			_List_fromArray(
				[
					elm$html$Html$text(str)
				]));
	});
var author$project$Encoder$encode = function (value) {
	encode:
	while (true) {
		switch (value.$) {
			case 'Number':
				var num = value.a;
				return elm$core$String$fromFloat(num);
			case 'Array':
				var items = value.a;
				return '[' + (A2(
					elm$core$String$join,
					', ',
					A2(elm$core$List$map, author$project$Encoder$encode, items)) + ']');
			case 'Object':
				var dict = value.a;
				return 'Object {' + (A2(
					elm$core$String$join,
					', ',
					A2(
						elm$core$List$map,
						function (_n1) {
							var key = _n1.a;
							var val = _n1.b;
							return A2(elm$core$String$contains, ' ', key) ? ('\"' + (key + ('\": ' + author$project$Encoder$encode(val)))) : (key + (': ' + author$project$Encoder$encode(val)));
						},
						elm$core$Dict$toList(dict))) + '}');
			case 'Abstraction':
				return '[Function]';
			case 'Undefined':
				return 'undefined';
			case 'Boolean':
				var bool = value.a;
				return bool ? 'true' : 'false';
			case 'String':
				var string = value.a;
				return '\"' + (string + '\"');
			default:
				var val = value.a;
				var $temp$value = val;
				value = $temp$value;
				continue encode;
		}
	}
};
var author$project$Playground$cellName = function (index) {
	return 'Cell ' + elm$core$String$fromInt(index);
};
var author$project$Playground$Components$column = function (attrs) {
	return elm$html$Html$div(
		_Utils_ap(
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'display', 'flex')
				]),
			attrs));
};
var author$project$Playground$Style$monospace = A2(elm$html$Html$Attributes$style, 'font-family', 'monospace, sans-serif');
var author$project$Playground$Style$cellLabelInput = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'color', '#303F9F'),
		A2(elm$html$Html$Attributes$style, 'font-size', '14px'),
		author$project$Playground$Style$monospace,
		A2(elm$html$Html$Attributes$style, 'text-align', 'right')
	]);
var author$project$Playground$Style$cellLabelOutput = _Utils_ap(
	author$project$Playground$Style$cellLabelInput,
	_List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'color', '#298539')
		]));
var author$project$Playground$Style$errorMessage = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'color', '#E75C58'),
		author$project$Playground$Style$monospace
	]);
var author$project$Playground$Style$warnMessage = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'color', '#bd8d04'),
		author$project$Playground$Style$monospace
	]);
var elm$core$Debug$toString = _Debug_toString;
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			elm$core$String$join,
			after,
			A2(elm$core$String$split, before, string));
	});
var elm$html$Html$pre = _VirtualDom_node('pre');
var author$project$Playground$renderResult = F3(
	function (model, index, item) {
		var getLine = F2(
			function (filename, line) {
				var cellIndex = A2(
					elm$core$Maybe$withDefault,
					0,
					elm$core$String$toInt(
						A3(elm$core$String$replace, 'Cell ', '', filename)));
				return A2(
					elm$core$Maybe$withDefault,
					'<line ' + (elm$core$String$fromInt(line) + ' not found>'),
					A2(
						elm$core$Maybe$andThen,
						A2(
							elm$core$Basics$composeL,
							A2(
								elm$core$Basics$composeL,
								elm_community$list_extra$List$Extra$getAt(line),
								elm$core$String$split('\n')),
							function ($) {
								return $.submittedInput;
							}),
						A2(elm_community$list_extra$List$Extra$getAt, cellIndex, model.cells)));
			});
		var expressionResult = A2(
			elm$core$Result$map,
			elm$core$Maybe$map(
				function ($) {
					return $.result;
				}),
			A2(elm$core$Result$map, elm$core$Tuple$second, item.result));
		if (expressionResult.$ === 'Err') {
			var error = expressionResult.a;
			var firstError = A2(
				elm$core$Maybe$withDefault,
				{
					col: 0,
					problem: elm$parser$Parser$Problem('There is an error on the playground, please report'),
					row: 0
				},
				elm$core$List$head(error));
			var msg = 'Syntax error. I could not parse the code. The problem happened here:\n\n' + (elm$core$String$fromInt(firstError.row) + ('| ' + (A2(
				getLine,
				author$project$Playground$cellName(index),
				firstError.row - 1) + ('\n' + (A2(
				elm$core$String$repeat,
				firstError.col + elm$core$String$length(
					elm$core$String$fromInt(firstError.row)),
				'-') + ('^\n\n' + elm$core$Debug$toString(firstError.problem)))))));
			return A2(
				author$project$Playground$Components$column,
				_Utils_ap(
					author$project$Playground$Style$errorMessage,
					_List_fromArray(
						[
							A2(elm$html$Html$Attributes$style, 'padding-top', '7px'),
							A2(elm$html$Html$Attributes$style, 'padding-bottom', '10px')
						])),
				_List_fromArray(
					[
						A2(author$project$Playground$cellLabelView, author$project$Playground$Style$cellLabelOutput, ''),
						A2(
						elm$html$Html$pre,
						_List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'font-size', '14px'),
								A2(elm$html$Html$Attributes$style, 'margin', '0')
							]),
						_List_fromArray(
							[
								elm$html$Html$text(msg)
							]))
					]));
		} else {
			if (expressionResult.a.$ === 'Just') {
				if (expressionResult.a.a.$ === 'Undefined') {
					var stack = expressionResult.a.a.a;
					var stackMsgs = A2(
						elm$core$List$indexedMap,
						F2(
							function (i, error) {
								var repetitionN = (error.column + elm$core$String$length(
									elm$core$String$fromInt(error.line))) + 1;
								var reason = function () {
									var _n1 = error.reason;
									switch (_n1.$) {
										case 'VariableNotDefined':
											var identifier = _n1.a;
											return identifier + ' is not defined';
										case 'OperationWithUndefined':
											var operationName = _n1.a;
											return operationName + ' with undefined';
										case 'MissingPositionalArgument':
											var index_ = _n1.a;
											var paramName = _n1.b;
											var posName = function () {
												var _n2 = index_ + 1;
												switch (_n2) {
													case 1:
														return '1st';
													case 2:
														return '2nd';
													case 3:
														return '3rd';
													default:
														return elm$core$String$fromInt(index_) + 'th';
												}
											}();
											return 'missing argument ' + (paramName + (' (' + (posName + ' argument)')));
										case 'VoidReturn':
											return 'function returned void';
										case 'IfWithoutElse':
											return 'if condition evaluated to false and there is no else case';
										case 'ExplicitUndefined':
											return 'explicitly given undefined value';
										case 'LoopNeverTrue':
											return 'loop condition never evaluated to true so the loop was never executed';
										case 'KeyNotInObject':
											var obj = _n1.a;
											var key = _n1.b;
											return 'key ' + (author$project$Encoder$encode(key) + (' not found on ' + author$project$Encoder$encode(obj)));
										case 'AssignmentToUndefined':
											var name = _n1.a;
											return name + ' got assigned to undefined';
										default:
											var val = _n1.a;
											return author$project$Encoder$encode(val) + ' is not a function';
									}
								}();
								var msgGotFrom = (!i) ? ((elm$core$List$length(stack) > 1) ? 'How come? First I got undefined from ' : 'I got undefined from ') : 'Then from ';
								var filename = _Utils_eq(
									error.filename,
									author$project$Playground$cellName(index)) ? 'here' : error.filename;
								return msgGotFrom + (filename + (':\n\n' + (elm$core$String$fromInt(error.line) + ('| ' + (A2(getLine, error.filename, error.line - 1) + ('\n' + (A2(elm$core$String$repeat, repetitionN, '-') + ('^\n' + (reason + '\n\n')))))))));
							}),
						stack);
					return A2(
						author$project$Playground$Components$column,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								author$project$Playground$cellLabelView,
								_Utils_ap(author$project$Playground$Style$cellLabelOutput, author$project$Playground$Style$warnMessage),
								'Output:'),
								A2(
								elm$html$Html$div,
								_List_fromArray(
									[
										A2(elm$html$Html$Attributes$style, 'padding-top', '7px'),
										author$project$Playground$Style$monospace
									]),
								_List_fromArray(
									[
										A2(
										elm$html$Html$div,
										_List_Nil,
										_List_fromArray(
											[
												elm$html$Html$text('undefined')
											])),
										A2(
										elm$html$Html$pre,
										_Utils_ap(
											author$project$Playground$Style$warnMessage,
											_List_fromArray(
												[
													A2(elm$html$Html$Attributes$style, 'font-size', '14px'),
													A2(elm$html$Html$Attributes$style, 'margin', '0'),
													A2(elm$html$Html$Attributes$style, 'padding-top', '20px')
												])),
										_List_fromArray(
											[
												elm$html$Html$text(
												A2(elm$core$String$join, '\n', stackMsgs))
											]))
									]))
							]));
				} else {
					var expr = expressionResult.a.a;
					return A2(
						author$project$Playground$Components$column,
						_List_Nil,
						_List_fromArray(
							[
								A2(author$project$Playground$cellLabelView, author$project$Playground$Style$cellLabelOutput, 'Output:'),
								A2(
								elm$html$Html$div,
								_List_fromArray(
									[
										A2(elm$html$Html$Attributes$style, 'padding-top', '7px'),
										author$project$Playground$Style$monospace
									]),
								_List_fromArray(
									[
										elm$html$Html$text(
										author$project$Encoder$encode(expr))
									]))
							]));
				}
			} else {
				var _n3 = expressionResult.a;
				return A2(elm$html$Html$div, _List_Nil, _List_Nil);
			}
		}
	});
var author$project$Playground$Style$cell = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'border', '1px solid #FFF'),
		A2(elm$html$Html$Attributes$style, 'border-left', '5px solid #FFF')
	]);
var author$project$Playground$Style$selectedCell = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'border', '1px solid #999'),
		A2(elm$html$Html$Attributes$style, 'border-left', '5px solid #42A5F5')
	]);
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$textarea = _VirtualDom_node('textarea');
var elm$html$Html$Attributes$rows = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rows',
		elm$core$String$fromInt(n));
};
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$json$Json$Decode$string = _Json_decodeString;
var elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var ohanhi$autoexpand$AutoExpand$getRows = F2(
	function (configInternal, scrollHeight) {
		return A3(
			elm$core$Basics$clamp,
			configInternal.minRows,
			configInternal.maxRows,
			elm$core$Basics$ceiling((scrollHeight - (2 * configInternal.padding)) / configInternal.lineHeight));
	});
var ohanhi$autoexpand$AutoExpand$inputDecoder = function (configInternal) {
	return A3(
		elm$json$Json$Decode$map2,
		F2(
			function (t, s) {
				return configInternal.onInput(
					{state: s, textValue: t});
			}),
		A2(
			elm$json$Json$Decode$at,
			_List_fromArray(
				['target', 'value']),
			elm$json$Json$Decode$string),
		A2(
			elm$json$Json$Decode$map,
			A2(
				elm$core$Basics$composeL,
				ohanhi$autoexpand$AutoExpand$State,
				ohanhi$autoexpand$AutoExpand$getRows(configInternal)),
			A2(
				elm$json$Json$Decode$at,
				_List_fromArray(
					['target', 'scrollHeight']),
				elm$json$Json$Decode$int)));
};
var ohanhi$autoexpand$AutoExpand$textareaStyles = F2(
	function (configInternal, rowCount) {
		return _List_fromArray(
			[
				A2(
				elm$html$Html$Attributes$style,
				'padding',
				elm$core$String$fromFloat(configInternal.padding) + 'px'),
				A2(elm$html$Html$Attributes$style, 'box-sizing', 'border-box'),
				A2(
				elm$html$Html$Attributes$style,
				'line-height',
				elm$core$String$fromFloat(configInternal.lineHeight) + 'px'),
				A2(
				elm$html$Html$Attributes$style,
				'overflow',
				(_Utils_cmp(rowCount, configInternal.maxRows) < 1) ? 'visible' : 'scroll-y'),
				A2(elm$html$Html$Attributes$style, 'overflow-x', 'hidden')
			]);
	});
var ohanhi$autoexpand$AutoExpand$attributes = F3(
	function (_n0, _n1, textValue) {
		var configInternal = _n0.a;
		var rowCount = _n1.a;
		return _Utils_ap(
			_List_fromArray(
				[
					A2(
					elm$html$Html$Events$on,
					'input',
					ohanhi$autoexpand$AutoExpand$inputDecoder(configInternal)),
					elm$html$Html$Attributes$rows(rowCount),
					elm$html$Html$Attributes$value(textValue)
				]),
			_Utils_ap(
				configInternal.attributes,
				A2(ohanhi$autoexpand$AutoExpand$textareaStyles, configInternal, rowCount)));
	});
var ohanhi$autoexpand$AutoExpand$view = F3(
	function (conf, state, textValue) {
		return A2(
			elm$html$Html$textarea,
			A3(ohanhi$autoexpand$AutoExpand$attributes, conf, state, textValue),
			_List_Nil);
	});
var author$project$Playground$cellView = F3(
	function (model, index, item) {
		var cellStyle = _Utils_eq(model.selectedCell, index) ? author$project$Playground$Style$selectedCell : author$project$Playground$Style$cell;
		return A2(
			author$project$Playground$Components$row,
			_Utils_ap(
				cellStyle,
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(
						author$project$Playground$Types$SelectCell(index)),
						A2(elm$html$Html$Attributes$style, 'padding', '5px')
					])),
			_List_fromArray(
				[
					A2(
					author$project$Playground$Components$column,
					_List_Nil,
					_List_fromArray(
						[
							A2(author$project$Playground$cellLabelView, author$project$Playground$Style$cellLabelInput, 'Input:'),
							A3(
							ohanhi$autoexpand$AutoExpand$view,
							A2(author$project$Playground$autoExpandConfig, 1, index),
							item.autoexpand,
							item.input)
						])),
					A3(author$project$Playground$renderResult, model, index, item)
				]));
	});
var author$project$Playground$Style$toolbarButton = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'border', '1px solid #999'),
		A2(elm$html$Html$Attributes$style, 'background', '#FFF'),
		A2(elm$html$Html$Attributes$style, 'font-size', '13px'),
		A2(elm$html$Html$Attributes$style, 'border-radius', '5px'),
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer')
	]);
var elm$html$Html$button = _VirtualDom_node('button');
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var author$project$Playground$toolbarView = function () {
	var toolbarButton = function (attrs) {
		return elm$html$Html$button(
			_Utils_ap(
				author$project$Playground$Style$toolbarButton,
				_Utils_ap(
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('toolbarButton'),
							A2(elm$html$Html$Attributes$style, 'margin-right', '5px'),
							A2(elm$html$Html$Attributes$style, 'padding', '5px 10px')
						]),
					attrs)));
	};
	return A2(
		author$project$Playground$Components$column,
		_List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'padding-bottom', '20px')
			]),
		_List_fromArray(
			[
				A2(
				toolbarButton,
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(author$project$Playground$Types$AddCell)
					]),
				_List_fromArray(
					[
						elm$html$Html$text('Add Cell')
					])),
				A2(
				toolbarButton,
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(author$project$Playground$Types$RunCell)
					]),
				_List_fromArray(
					[
						elm$html$Html$text('Run Cell')
					]))
			]));
}();
var author$project$Playground$Components$container = F2(
	function (attrs, children) {
		return A2(
			author$project$Playground$Components$column,
			_Utils_ap(
				_List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'justify-content', 'center')
					]),
				attrs),
			_List_fromArray(
				[
					A2(
					author$project$Playground$Components$row,
					_List_fromArray(
						[
							A2(elm$html$Html$Attributes$style, 'flex-grow', '1'),
							A2(elm$html$Html$Attributes$style, 'max-width', '1140px'),
							A2(elm$html$Html$Attributes$style, 'padding', '0 10px')
						]),
					children)
				]));
	});
var author$project$Playground$Style$menuLink = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'color', 'rgb(40, 126, 213)'),
		A2(elm$html$Html$Attributes$style, 'font-size', '20px'),
		A2(elm$html$Html$Attributes$style, 'cursor', 'pointer'),
		A2(elm$html$Html$Attributes$style, 'text-decoration', 'none')
	]);
var author$project$Playground$Style$smallSubtitle = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'font-weight', 'normal'),
		A2(elm$html$Html$Attributes$style, 'color', '#AAA'),
		A2(elm$html$Html$Attributes$style, 'font-size', '15px')
	]);
var author$project$Playground$Style$submenuItem = _Utils_ap(
	author$project$Playground$Style$menuLink,
	_List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'font-size', '18px'),
			A2(elm$html$Html$Attributes$style, 'color', '#666')
		]));
var author$project$Playground$Style$title = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'font-weight', 'normal')
	]);
var elm$html$Html$a = _VirtualDom_node('a');
var elm$html$Html$h1 = _VirtualDom_node('h1');
var elm$html$Html$h2 = _VirtualDom_node('h2');
var author$project$Playground$Components$header = function () {
	var submenuItem = function (attrs) {
		return elm$html$Html$a(
			_Utils_ap(
				author$project$Playground$Style$submenuItem,
				_Utils_ap(
					_List_fromArray(
						[
							A2(elm$html$Html$Attributes$style, 'padding', '10px 20px'),
							A2(elm$html$Html$Attributes$style, 'display', 'block'),
							elm$html$Html$Attributes$class('submenuItem')
						]),
					attrs)));
	};
	var menuLink = function (attrs) {
		return elm$html$Html$a(
			_Utils_ap(
				author$project$Playground$Style$menuLink,
				_Utils_ap(
					_List_fromArray(
						[
							A2(elm$html$Html$Attributes$style, 'padding', '10px 20px'),
							elm$html$Html$Attributes$class('menuLink')
						]),
					attrs)));
	};
	return A2(
		author$project$Playground$Components$column,
		_List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'padding-top', '20px')
			]),
		_List_fromArray(
			[
				A2(
				author$project$Playground$Components$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$h1,
						_Utils_ap(
							author$project$Playground$Style$title,
							_List_fromArray(
								[
									A2(elm$html$Html$Attributes$style, 'margin', '0 0 5px -1px')
								])),
						_List_fromArray(
							[
								elm$html$Html$text('Untypescript')
							])),
						A2(
						elm$html$Html$h2,
						_Utils_ap(
							author$project$Playground$Style$smallSubtitle,
							_List_fromArray(
								[
									A2(elm$html$Html$Attributes$style, 'margin-top', '0'),
									A2(elm$html$Html$Attributes$style, 'padding-bottom', '10px')
								])),
						_List_fromArray(
							[
								elm$html$Html$text('JavaScript without runtime errors')
							]))
					]))
			]));
}();
var author$project$Playground$Style$card = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'background', '#FFF'),
		A2(elm$html$Html$Attributes$style, 'box-shadow', '0px 0px 12px 1px rgba(87, 87, 87, 0.2)')
	]);
var author$project$Playground$Style$header = author$project$Playground$Style$card;
var author$project$Playground$Style$notebook = author$project$Playground$Style$card;
var author$project$Playground$playground = function (model) {
	return _List_fromArray(
		[
			A2(
			author$project$Playground$Components$container,
			author$project$Playground$Style$header,
			_List_fromArray(
				[author$project$Playground$Components$header, author$project$Playground$toolbarView])),
			A2(
			author$project$Playground$Components$container,
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'padding-top', '20px')
				]),
			_List_fromArray(
				[
					A2(
					author$project$Playground$Components$row,
					_Utils_ap(
						author$project$Playground$Style$notebook,
						_List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'padding', '20px')
							])),
					A2(
						elm$core$List$indexedMap,
						author$project$Playground$cellView(model),
						model.cells))
				]))
		]);
};
var author$project$Playground$Style$general = _List_fromArray(
	[
		A2(elm$html$Html$Attributes$style, 'font-family', 'sans-serif')
	]);
var author$project$Playground$Style$footer = _Utils_ap(
	author$project$Playground$Style$general,
	_List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'color', '#999'),
			A2(elm$html$Html$Attributes$style, 'font-size', '13px')
		]));
var elm$html$Html$img = _VirtualDom_node('img');
var elm$html$Html$Attributes$alt = elm$html$Html$Attributes$stringProperty('alt');
var elm$html$Html$Attributes$href = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var elm$html$Html$Attributes$src = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var author$project$Playground$view = function (model) {
	return {
		body: _List_fromArray(
			[
				A2(
				author$project$Playground$Components$row,
				_Utils_ap(
					author$project$Playground$Style$general,
					_List_fromArray(
						[
							elm$html$Html$Attributes$id('main'),
							A2(elm$html$Html$Attributes$style, 'margin', '-8px'),
							A2(elm$html$Html$Attributes$style, 'min-height', '90vh')
						])),
				function () {
					var _n0 = model.page;
					return author$project$Playground$playground(model);
				}()),
				A2(
				author$project$Playground$Components$container,
				_Utils_ap(
					author$project$Playground$Style$footer,
					_List_fromArray(
						[
							A2(elm$html$Html$Attributes$style, 'padding-top', '25px')
						])),
				_List_fromArray(
					[
						elm$html$Html$text('Did you like this project? Drop me a message on '),
						A2(
						elm$html$Html$a,
						_List_fromArray(
							[
								elm$html$Html$Attributes$href('https://twitter.com/_rchaves_')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('twitter')
							]))
					])),
				A2(
				elm$html$Html$a,
				_List_fromArray(
					[
						elm$html$Html$Attributes$href('https://github.com/rogeriochaves/untypescript/')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$img,
						_List_fromArray(
							[
								A2(elm$html$Html$Attributes$style, 'position', 'fixed'),
								A2(elm$html$Html$Attributes$style, 'top', '0'),
								A2(elm$html$Html$Attributes$style, 'right', '0'),
								A2(elm$html$Html$Attributes$style, 'border', '0'),
								elm$html$Html$Attributes$src('https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png'),
								elm$html$Html$Attributes$alt('Fork me on GitHub')
							]),
						_List_Nil)
					]))
			]),
		title: 'Untypescript'
	};
};
var author$project$Playground$Types$OnUrlRequest = function (a) {
	return {$: 'OnUrlRequest', a: a};
};
var elm$browser$Browser$application = _Browser_application;
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var author$project$Playground$main = elm$browser$Browser$application(
	{
		init: author$project$Playground$init,
		onUrlChange: author$project$Playground$Types$OnUrlChange,
		onUrlRequest: author$project$Playground$Types$OnUrlRequest,
		subscriptions: elm$core$Basics$always(elm$core$Platform$Sub$none),
		update: author$project$Playground$update,
		view: author$project$Playground$view
	});
_Platform_export({'Playground':{'init':author$project$Playground$main(
	elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));