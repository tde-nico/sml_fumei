from functools import reduce
import sys


CORE_FILE = './core.sml'


TK_THREAD = "th\n"
TK_SYNC = "sync"
TK_CRIT = "crit"
TK_ENDCRIT = "endcrit"
TK_IF = "if "
TK_ENDIF = "} else {"
TK_END = "}"
TK_WHILE = "while "


def seq(s1: str, s2: str) -> str:
	if not s1.strip():
		return s2
	return f"Seq({s1}, {s2 if s2 else 'Skip'})"


def parse_single(s: str) -> str:
	if s.isdigit():
		return f"Const {s}"
	else:
		return f"Var \"{s}\""



def parse_scope(s: str, divs='()') -> int:
	graph = 0

	for i, char in enumerate(s):
		if char == divs[0]:
			graph += 1
			continue
		elif char == divs[1]:
			graph -= 1
			if not graph:
				break

		if graph < 0:
			raise Exception()

		if not graph:
			continue

	return i + 1


def parseE(e: str) -> str:
	if e[0].isalnum():
		return parse_single(e)

	i = parse_scope(e)
	if i == len(e):
		return parse_single(e[1:-1])

	op1 = e[1:i-1]
	op = e[i]
	op2 = e[i+2:-1]

	operators = {
		"+" : "Sum",
		"-" : "Sub",
		"*" : "Mul",
		"/" : "Div"
	}
	op = operators.get(op, None)
	if op is None:
		raise Exception()
	
	return f'{op}({parseE(op1)}, {parseE(op2)})'


def parseA(a: str) -> str:
	a = ''.join([char for char in a if char != ' '])
	var, exp = a.split('=')
	exp = parseE(exp)
	return f'Assign("{var}", {exp})'


def parseB(b: str) -> str:
	if b[0] == '!':
		return f'Not({parseB(b[2:-1])})'

	i = parse_scope(b)
	if i == len(b):
		if b[0] == '(':
			return b[1:-1]
		return b

	op1 = b[1:i-1]
	j = b[i:].find('(') + i
	op = b[i:j]
	op2 = b[j+1:-1]

	operators = {
		"and" : "And",
		"or" : "Or",
		"==" : "Eq",
		">" : "Gt",
		"<" : "Lt"
	}
	op = operators.get(op, None)
	if op is None:
		raise Exception()

	if op in ['And', 'Or']:
		return f'{op}({parseB(op1)}, {parseB(op2)})'
	return f'{op}({parseE(op1)}, {parseE(op2)})'


def parseP(p: str) -> str:
	p = [line for line in p.split('\n') if line]
	line = 0
	res = []
	while line < len(p):
		l = p[line]
		if l == TK_SYNC:
			res.append('Sync')
		
		elif l == TK_CRIT:
			line += 1
			sub_if = []
			while p[line] != TK_ENDCRIT:
				sub_if.append(p[line])
				line += 1
			parsed = parseP('\n'.join(sub_if))
			res.append(f"Crit({parsed})")

		elif l.startswith(TK_IF):
			cond = parseB(
				''.join([
					c
					for c in l[len(TK_IF):-1]
					if c != ' '
				])[1:-1]
			)

			line += 1
			sub_if = []
			while p[line] != TK_ENDIF:
				sub_if.append(p[line][1:])
				line += 1
			_then = parseP('\n'.join(sub_if))

			line += 1
			sub_else = []
			while p[line] != TK_END:
				sub_else.append(p[line][1:])
				line += 1
			_else = parseP('\n'.join(sub_else))

			res.append(f"If({cond}, {_then}, {_else})")


		elif l.startswith(TK_WHILE):
			cond = parseB(
				''.join([
					c
					for c in l[len(TK_WHILE):-1]
					if c != ' '
				])[1:-1]
			)
			
			line += 1
			sub_while = []
			while p[line] != TK_END:
				sub_while.append(p[line][1:])
				line += 1
			cycle = parseP('\n'.join(sub_while))

			res.append(f"While({cond}, {cycle})")
			

		else:
			res.append(parseA(l))
		line += 1

	res = reduce(seq, res, "")
	return res


def parse(t: str) -> str:
	t = t.strip()
	threads = [th for th in t.split(TK_THREAD) if th]
	progs = []
	for th in threads:
		progs.append(parseP(th))

	res = 'Null'
	for t in progs[::-1]:
		res = f'Th({t}, {res})'

	return res


def transpile(parsed):
	with open(CORE_FILE, 'r') as f:
		core = f.read()
	transpiled = core % parsed
	with open('./transpiled.sml', 'w') as f:
		f.write(transpiled)


sample_program = """

th
x = (2)
if ( (x) == (2) ) {
	if ( (x) == (3) ) {
		x = (x) + (1)
	} else {
		x = (x) - (1)
	}
} else {
	x = (x) - (2)
}
sync


th
y = (5) + ((6) * (5))
while ((y) > (30)) {
	y = (y) - (1)
}
sync


th
z = (x) + (1)
sync
crit

if ( (! ((y) < ((z) + (x))) ) and (((2) == (3)) or (True)) ) {
	z = (z) + (1)
} else {
	z = (z) - (1)
}

endcrit


"""


if len(sys.argv) > 1:
	with open(sys.argv[1], 'r') as f:
		prog = f.read()
else:
	prog = sample_program


parsed = parse(prog)

RUN = 1
TRANSPILE = 1

if TRANSPILE:
	transpile(parsed)
else:
	print(parsed)


if RUN:
	from os import system
	system('cat main.sml | sml')
