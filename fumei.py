from functools import reduce


CORE_FILE = './core.sml'

def seq(s1: str, s2: str) -> str:
	if not s1.strip():
		return s2
	return f"Seq({s1}, {s2 if s2 else 'Skip'})"


def parse_single(s: str) -> str:
	if s.isdigit():
		return f"Costant {s}"
	else:
		return f"Var \"{s}\""


def parseExp(M: str) -> str:
	M = M.split(" ")

	if not len(M) %2:
		raise Exception()
	elif len(M) == 1:
		return parse_single(M[0])
	else:
		operators = {
			"+" : "Sum",
			"-" : "Sub",
			"*" : "Mul",
			"/" : "Div"
		}
		op = [operators.get(m, None) for m in M[1::2]]
		if None in op:
			raise Exception()
		M = [m for m in M[::2]]

		out = parse_single(M[0])
		for i, o in enumerate(op):
			out = f"{o}({out}, {parse_single(M[i+1])})"
		return out


def parse_scope(s: str, divs='{}', cut=False) -> str:
	graph = 0
	parsed = ''
	for char in s:
		if char == divs[0]:
			graph += 1
		elif char == divs[1]:
			graph -= 1
			if cut and not graph:
				parsed += char
				break
		
		if graph and char in '\t\r\n':
			char = ''
		if graph < 0:
			raise Exception()

		if cut and not graph:
			continue
		parsed += char

	return parsed


def parse_cond(cond: str) -> str:
	cond = cond[1:-1]
	length = len(cond)
	i = 0
	parsed = ''
	while i < length:
		if cond[i] == ' ':
			i+=1
			continue

		if cond[i] == '(':
			sub = parse_scope(cond[i:], '()', 1)
			sub_cond = parse_cond(sub)
			parsed += sub_cond
			i += len(sub)
			continue

		if cond[i] == '!':
			parsed += 'Not('
			# TODO: finsh this
			i += 1
			continue

		parsed += cond[i]
		i += 1
	return parsed



def parse_row(s: str) -> str:
	s = s.strip()
	if not s:
		return ""

	if s.startswith('if'):
		s = s[2:].strip()
		cond, scope = parse_scope(s, '()', 1), parse_scope(s, '{}', 1)
		#print(s)
		#print(cond, scope)
		c = parse_cond(cond)
		print(c)



	elif s.startswith('while'):
		print('w', s)

	elif '=' in s:
		if s.endswith(';'):
			s = s[:-1]
		else:
			raise Exception()
		var, exp = s.split("=")
		#return f'Assign("{var.strip()}", {parseExp(exp.strip())})'
	return ""


def parse(program):
	parsed = parse_scope(program, divs='{}')
	return reduce(
		seq,
		map(
			parse_row,
			parsed.split('\n')
		),
		"",
	)


def transpile(program):
	with open(CORE_FILE, 'r') as f:
		core = f.read()
	transpiled = core % parse(program)
	with open('./transpiled.sml', 'w') as f:
		f.write(transpiled)


sample_program = """\
x = 2;
y = 5 + 6 * 5;
z = x + 1;
if (!(4 < (3 + 2 + if))) {
	f = 69;
	if (f == 42) {
		w = 104;
	}
}\
"""


# TODO: fare le operazioni con priorità


# TODO: FIX
print('Not Working')
exit(1)

TRANSPILE = 0
if TRANSPILE:
	transpile(sample_program)
else:
	print(parse(sample_program))
