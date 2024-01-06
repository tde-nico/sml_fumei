open SMLofNJ.Cont;

datatype Exp = Const of int | Var of string | Sum of Exp * Exp | Sub of Exp * Exp | Mul of Exp* Exp | Div of Exp * Exp;
datatype Bool = True | False | And of Bool * Bool | Or of Bool * Bool | Not of Bool | Eq of Exp * Exp | Gt of Exp * Exp | Lt of Exp * Exp;
datatype Program = Skip | Seq of Program * Program | Assign of string * Exp | If of Bool * Program * Program | While of Bool * Program | Sync | Crit of Program;
datatype Thread = Null | Th of Program * Thread;

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

fun checkEnd(t:Thread): bool = 
	case t of
		Null => true
		| Th (p1, t1) => (p1 = Skip) andalso checkEnd t1;


fun checkSync(t:Thread): bool =
	case t of
		Null => true
		| Th (p1, t1) => (
			case p1 of
				Skip => false
				| Seq (q,m) => checkSync(Th(q, Null))
				| Assign (x, M) => false
				| If (b, t, e) => false
				| While (b, q) => false
				| Crit q => false
				| Sync => true
			) andalso checkSync t1;


fun removeSyncP(p:Program):Program = 
	case p of
		Sync => Skip
		| Seq (p1,p2) => Seq(removeSyncP p1, p2);


fun removeSync(t:Thread):Thread = 
	case t of 
		Null => Null
		| Th (p1, t1) => Th(removeSyncP p1, removeSync t1)


fun evalTOne(E:(string * int) list, t:Thread) = 
	case t of 
		Null => (E, Null)
		| Th(p1, t1) => 
			let
				val (E1, p1_1) = callcc(fn k1 => evalI(E, p1, k1))
				val (E2, t1_1) = evalTOne(E1, t1)
			in
				(E2, Th(p1_1, t1_1))
			end;


fun evalT(E:(string * int) list, t: Thread): ((string * int) list) =
	case t of 
		Null => E
		| Th (p1, t1) => 
			let
				val (E1, t1_1) = evalTOne(E, Th(p1, t1))
			in
				if checkSync t1_1 then
					evalT(E1, removeSync t1_1)
				else
					if not(checkEnd t1_1) then
						evalT(E1, t1_1)
					else
						E1
			end;



val E = [];

val prog = Th(
	Seq(
		Seq(
			Assign("x", Const 2),
			If(
				Eq(Var "x", Const 2),
				If(
					Eq(Var "x", Const 3),
					Assign("x",
						Sum(Var "x", Const 1)),
					Assign("x",
						Sub(Var "x", Const 1))
				),
				Assign("x",
					Sub(Var "x", Const 2))
			)
		),
		Sync
	),
	Th(
		Seq(
			Seq(
				Assign("y",
					Sum(Const 5,
						Mul(Const 6, Const 5))),
				While(
					Gt(Var "y", Const 30),
					Assign("y",
						Sub(Var "y", Const 1))
				)
			),
			Sync
		),
		Th(
			Seq(
				Seq(
					Assign("z",
						Sum(Var "x", Const 1)),
					Sync
				),
				Crit(
					If(
						And(
							Not(
								Lt(Var "y",
									Sum(Var "z", Var "x")
								)
							),
							Or(
								Eq(Const 2, Const 3),
								True
							)
						),
						Assign("z",
							Sum(Var "z", Const 1)),
						Assign("z",
							Sub(Var "z", Const 1))
					)
				)
			),
			Null
		)
	)
);

val result = evalT(E, prog);

(*

val E = [];

val prog = Th(
	Seq(
		Seq(
			Assign("x", Const 2),
			If(
				Eq(Var "x", Const 2),
				If(
					Eq(Var "x", Const 3),
					Assign("x",
						Sum(Var "x", Const 1)),
					Assign("x",
						Sub(Var "x", Const 1))
				),
				Assign("x",
					Sub(Var "x", Const 2))
			)
		),
		Sync
	),
	Th(
		Seq(
			Seq(
				Assign("y",
					Sum(Const 5,
						Mul(Const 6, Const 5))),
				While(
					Gt(Var "y", Const 30),
					Assign("y",
						Sub(Var "y", Const 1))
				)
			),
			Sync
		),
		Th(
			Seq(
				Seq(
					Assign("z",
						Sum(Var "x", Const 1)),
					Sync
				),
				Crit(
					If(
						And(
							Not(
								Lt(Var "y",
									Sum(Var "z", Var "x")
								)
							),
							Or(
								Eq(Const 2, Const 3),
								True
							)
						),
						Assign("z",
							Sum(Var "z", Const 1)),
						Assign("z",
							Sub(Var "z", Const 1))
					)
				)
			),
			Null
		)
	)
);

val result = evalT(E, prog);

(* [("z",4),("y",29),("y",30),("y",31),("y",32),("y",33),("x",1),("y",34),("z",3),("y",35),("x",2)] *)
*)


(*

val E = [];
val prog1 = Seq(
	Seq(
		Assign("x", Const 5),
		While(
			Lt(Var "x", Const 7),
			Assign("x", Sum(Var "x", Const 1))
		)
	),
	Sync
);

val prog2 = Seq(
	Seq(
		Assign("y", Const 3),
		Sync
	),
	If(
		Gt(Var "y", Const 3),
		Assign("y", Sum(Var "y", Const 1)),
		Assign("y", Sub(Var "y", Const 1))
	)
);

val prog3 = Seq(
	Crit(
		Seq(
			Seq(
				Assign("z", Const 6),
				Assign("z", Sum(Var "z", Const 1))
			),
			Seq(
				Assign("z", Sum(Var "z", Const 1)),
				Assign("z", Sum(Var "z", Const 1))
			)
		)
	),
	Seq(
		Seq(
			Assign("z", Sum(Var "z", Const 1)),
			Sync
		),
		Assign("z", Sum(Var "z", Const 1))
	)
);


val result = evalT(E, Th(prog1, Th(prog2, Th(prog3, Null))));

(* [("y",2),("z",11),("x",7),("x",6),("z",10),("z",9),("z",8),("z",7),("z",6),("y",3),("x",5)] *)
*)

(*
val E = [];
val prog1 = Seq(Seq(Seq(Assign("x", Const 5), Seq(Assign("x", Sum(Var "x", Const 1)), Assign("x", Sum(Var "x", Const 1)))), Sync), Assign("x", Sum(Var "x", Const 1)));
val prog2 = Seq(Seq(Assign("y", Const 3), Sync), Assign("y", Sum(Var "y", Const 1)));
val prog3 = Seq(Seq(Seq(Assign("z", Const 6), Sync), Assign("z", Sum(Var "z", Const 1))), Assign("z", Sum(Var "z", Const 1)));
val result = evalT(E, Th(prog1, Th(prog2, Th(prog3, Null))));
(* [("z",8),("z",7),("y",4),("x",8),("x",7),("x",6),("z",6),("y",3),("x",5)] *)
*)

(*
val prog1 = Seq(Seq(Seq(Assign("x", Const 5), Seq(Assign("x", Sum(Var "x", Const 1)), Assign("x", Sum(Var "x", Const 1)))), Sync), Assign("x", Sum(Var "x", Const 1)));
val prog2 = Seq(Seq(Assign("y", Const 3), Sync), Assign("y", Sum(Var "y", Const 1)));
(* [("y",4),("x",8),("x",7),("x",6),("y",3),("x",5)] *)
*)

(*
val prog1 = Crit(Seq(Seq(Seq(Assign("x", Const 5), Assign("x", Sum(Var "x", Const 1))), Assign("x", Sum(Var "x", Const 1))), Assign("x", Sum(Var "x", Const 1))));
val prog2 = Seq(Assign("y", Const 3), Assign("y", Sum(Var "y", Const 1)));
(* [("y",4),("y",3),("x",8),("x",7),("x",6),("x",5)] *)
*)

(*
val prog1 = Seq(Seq(Assign("x", Const 5), Assign("x", Sum(Var "x", Const 1))), If(Eq(Var "y", Const 4), Assign("x", Sum(Var "x", Const 1)), Assign("x", Sub(Var "x", Const 1))));
val prog2 = Seq(Assign("y", Const 3), Assign("y", Sum(Var "y", Const 1)));
(* [("x",7),("y",4),("x",6),("y",3),("x",5)] *)
*)

(*
val prog1 = Seq(Assign("x", Const 5), Assign("x", Sum(Var "x", Const 1)));
val prog2 = Seq(Assign("y", Const 3), Assign("y", Sum(Var "y", Const 1)));
(* [("y",4),("x",6),("y",3),("x",5)] *)
*)
(*
val prog1 = Seq(Assign("y", Const 2), Assign("y", Div(Var "x", Const 2)));
val prog2 = Seq(Seq(Seq(Assign("x", Const 6), Assign("x", Sum(Var "x", Const 1))), Assign("x", Sum(Var "y", Const 1))), 
	While(Gt(Var "y", Const 1), Seq(Assign("x", Sum(Var "x", Var "y")), Assign("y", Sub(Var "y", Const 1)))));
(* [("y",1),("x",9),("y",2),("x",7),("x",4),("x",7),("y",3),("x",6),("y",2)] *)
*)
