open SMLofNJ.Cont;

datatype Exp = Const of int | Var of string | Sum of Exp * Exp | Sub of Exp * Exp | Mul of Exp* Exp | Div of Exp * Exp;

datatype Bool = True | False | And of Bool * Bool | Or of Bool * Bool | Not of Bool | Eq of Exp * Exp | Gt of Exp * Exp | Lt of Exp * Exp;

datatype Program = Skip | Seq of Program * Program | Assign of string * Exp | If of Bool * Program * Program | While of Bool * Program | Crit of Program | Sync;

datatype Thread = Th of Program * Program;

exception NotFoundException;

fun findElementByKey(key: string, lst: (string * int) list): int =
	case lst of
		[] => raise NotFoundException
		| (k,v) :: rest =>
			if k = key then
				v
			else
				findElementByKey(key, rest);


fun evalM(E:(string * int) list, M:Exp):int = 
	case M of
		Const v => v
		| Var s => findElementByKey(s, E)
		| Sum (e1, e2) => evalM(E, e1) + evalM(E, e2)
		| Sub (e1, e2) => evalM(E, e1) - evalM(E, e2)
		| Mul (e1, e2) => evalM(E, e1) * evalM(E, e2)
		| Div (e1, e2) => evalM(E, e1) div evalM(E, e2);


fun evalB(E:(string * int) list, B:Bool):bool =
	case B of
		True => true 
		| False => false
		| And (a,b) => evalB(E, a) andalso evalB(E, b)
		| Or (a,b) => evalB(E, a) orelse evalB(E, b)
		| Not a => not(evalB(E, a))
		| Eq (a,b) => evalM(E, a) = evalM(E, b)
		| Gt (a,b) => evalM(E, a) > evalM(E, b)
		| Lt (a,b) => evalM(E, a) < evalM(E, b);


fun evalP(E:(string * int) list, p:Program):((string * int) list) = 
	case p of
		Skip => E
		| Seq (q,m) => evalP(evalP(E, q), m)
		| Assign (x, M) => (x, evalM(E,M)) :: E
		| If (b, t, e) => if evalB(E, b) then evalP(E, t) else evalP(E, e)
		| While (b, q) => if evalB(E, b) then evalP(E, Seq(q, p)) else E
		| Crit q => evalP(E, q)
		| Sync => E;


fun evalI(E:(string * int) list, p:Program, k) =
	case p of
		Skip => throw k (E, Skip)
		| Seq (q,m) => throw k let 
			val (E1, q2) = 
				callcc(fn k1 => evalI(E, q, k1))
			in
				if q2 <> Skip then
					(E1, Seq(q2, m))
				else
					(E1, m)
			end
		| Assign (x, M) => throw k ((x, evalM(E,M)) :: E, Skip)
		| If (b, p1, p2) => if evalB(E, b) then throw k (E, p1) else throw k (E, p2)
		| While (b, q) => if evalB(E, b) then throw k (E, Seq(q, p)) else throw k (E, Skip)
		| Crit q => throw k (evalP(E, q), Skip)
		| Sync => throw k (E, p);


fun isSync(p:Program): bool = 
    case p of
        Skip => false
      | Seq (q,m) => isSync(q)
      | Assign (x, M) => false
      | If (b, t, e) => false
      | While (b, q) => false
      | Crit p1 => false
      | Sync => true;

fun removeSync(p:Program):Program = 
	case p of
		Seq (q,m) => Seq(removeSync(q), m)
		| Sync => Skip;


fun evalT(E:(string * int) list, t: Thread): ((string * int) list) =
	case t of
		Th (p1,p2) =>
			let
				val (E1, p1_1) = callcc(fn k => evalI(E, p1, k))
				val (E2, p2_1) = callcc(fn k => evalI(E1, p2, k))
			in
				if isSync(p1) andalso isSync(p2) then
					evalT(E2, Th(removeSync(p1_1), removeSync(p2_1)))
				else
					if p1_1 <> Skip orelse p2_1 <> Skip then
						evalT(E2, Th(p1_1, p2_1))
					else
						E2
			end;
