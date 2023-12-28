datatype exp = costant of int | var of string | sum of exp * exp | sub of exp * exp | mul of exp*exp | divi of exp * exp;

datatype bool = True | False | impl of bool * bool | eq of bool * bool;

datatype program = skip | seq of program * program | assign of string * exp;

exception NotFoundException;

fun findElementByKey(key: string, lst: (string * int) list): int =
    case lst of
        [] => raise NotFoundException
      | (k,v) :: rest =>
          if k = key then
              v
          else
              findElementByKey(key, rest);

fun evalM(E:(string * int) list, M:exp):int = 
  case M of
          costant v => v
        | var s => findElementByKey(s, E)
        | sum (e1, e2) => evalM(E, e1) + evalM(E, e2)
        | sub (e1, e2) => evalM(E, e1) - evalM(E, e2)
        | mul (e1, e2) => evalM(E, e1) * evalM(E, e2)
        | divi (e1, e2) => evalM(E, e1) div evalM(E, e2);

fun evalP (E:(string * int) list, p:program):((string * int) list) = 
  case p of
          skip => E
        | seq (q,m) => evalP(evalP(E, q), m)
        | assign (x, M) => (x, evalM(E,M)) :: E;


val E = [];
val prog = seq(seq(seq(assign("x", costant 2), assign("y", mul(sum(costant 5, costant 6), costant 5))), assign("z", sum(var "x", costant 1))), skip);

val result = evalP(E, prog);
