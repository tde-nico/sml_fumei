from functools import reduce


CORE_FILE = './core.sml'

def seq(s1: str, s2: str) -> str:
	if not s1.strip():
		return s2
	return f"seq({s1}, {s2 if s2 else 'skip'})"


def parse_single(s: str) -> str:
	if s.isdigit():
		return f"costant {s}"
	else:
		return f"var \"{s}\""


def parseExp(M: str) -> str:
	M = M.split(" ")

	if not len(M) %2:
		raise Exception()
	elif len(M) == 1:
		return parse_single(M[0])
	else:
		operators = {
			"+" : "sum",
			"-" : "sub",
			"*" : "mul",
			"/": "divi"
		}
		op = [operators.get(m, None) for m in M[1::2]]
		if None in op:
			raise Exception()
		M = [m for m in M[::2]]

		out = parse_single(M[0])
		for i, o in enumerate(op):
			out = f"{o}({out}, {parse_single(M[i+1])})"
		return out


def parse_row(s: str) -> str:
	if not s.strip():
		return ""
	var, exp = s.split("=")
	return f'assign("{var.strip()}", {parseExp(exp.strip())})'


def parse(program):
	return reduce(
		seq,
		map(
			parse_row,
			program.split(';')
		),
		"",
	)


def transpile(program):
	with open(CORE_FILE, 'r') as f:
		core = f.read()
	transpiled = core % parse(program)
	with open('./transpiled.sml', 'w') as f:
		f.write(transpiled)


sample_program = """
x = 2;
y = 5 + 6 * 5;
z = x + 1;
"""


# TODO: fare le operazioni con priorità

TRANSPILE = 1
if TRANSPILE:
	transpile(sample_program)
else:
	print(parse(sample_program))
