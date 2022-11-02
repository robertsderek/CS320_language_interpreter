(* parsing util functions *)

let is_lower_case c = 'a' <= c && c <= 'z'

let is_upper_case c = 'A' <= c && c <= 'Z'

let is_alpha c = is_lower_case c || is_upper_case c

let is_digit c = '0' <= c && c <= '9'

let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

let is_blank c = String.contains " \012\n\r\t" 

let explode s = List.of_seq (String.to_seq s)

let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)


(* parser combinators *)
(* Either trivial parsers or higher order functions that compose together other parsers *)

type 'a parser = char list -> ('a * char list) option

(* takes a parser p and a string s, and applies the parser to the exploded string *)
let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)

(* pure parser takes an input x and returns a parser that has type 'a. 
The parser doesn't inspect the input, just returns the x, next to the list. *)
let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)

(* another trivial parser that simply returns None instantly *)
let fail : 'a parser = fun ls -> None

(* The most powerful and most complicated. Takes in a parser p of type a and a 
function q of type 'a->'b parser, and returns a type 'b parser. The usage of bind
is a sequencing tool.  The parser q is allowed to depend on the results of parser p. *)
let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let ( >>= ) = bind

let ( let* ) = bind

(* reads it's input list and if the input has at least 1 character inside, it will return
that character along with the rest of the list  *)
let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

(* Similar to read, and returns a character parser. Instead of returning x immediatly like read,
it first applies f to x, and returns x if true. If f x is false or list is empty it returns None. *)
let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then
      Some (x, ls)
    else
      None
  | _ -> None

(* Takes in a single character and checks to see if the input list starts with the input character. *)
let char (c : char) : char parser = satisfy (fun x -> x = c)

(* applies parsers p1 and p2 in sequence, but p2 does not depend on p1, and the result 
of p1 is discarded *)
let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let ( >> ) = seq


(* applies p1 to this input list, and given the remaining input, it applies p2 to that.
Instead of taking p2 as the result, it takes p1 instead. *)
let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> (
      match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
  | None -> None

let ( << ) = seq'

(* takes two parsers p1 and p2 which must be of the same type. Attempts to apply p1 to the input list
and return that result. If p1 is not successful, it attempts to apply p2 and then return that result. *)
let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> Some (x, ls)
  | None -> p2 ls

let ( <|> ) = alt


(* applies parser p to the input list, if the parse was successful, then map applies 
the function f to that result and returns. *)
let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

(* takes in a parser p of type 'a, and tries to apply p as many times as possible to the input list.
For each successful parse, it will put the result in a list, and then output the list when p fails *)
let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

  (* Expects the parser p to be applied at least once. Many can be applied 0 or more times, 
  Many1 can be applied 1 or more times *)
let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

  (* takes a function from unit to prime 'a parser. Useful when going into mutual recursion using many'.
  Solves infinite recursion problems *)
let rec many' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

(* parser that returns the value of whitespace in a string *)
let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some ((), ls)
    else
      None
  | _ -> None

  (* parses 0 or more whitespaces *)
let ws : unit parser = many whitespace >| ()

  (* parses 1 or more whitespaces *)
let ws1 : unit parser = many1 whitespace >| ()

  (* parses digits *)
let digit : char parser = satisfy is_digit

let is_paren (c : char): bool =
  c = '(' || c = ')'

let paren : char parser = satisfy is_paren 

  (* parses numbers greater than 0*)
let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (int_of_string (implode xs), ls)
  | _ -> None


  (* parses the inputed string out of a list *)
let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match (cs, ls) with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c then
        loop cs xs
      else
        None
    | _ -> None
  in
  loop cs ls

    (* A literal parser with whitespaces trimmed off *)
let keyword (s : string) : unit parser = literal s >> ws >| ()

(* end of parser combinators *)  

let parenParser : string parser =
  fun ls ->
  match many1 paren ls with 
  | Some (xs, ls) -> Some ((implode xs), ls)
  | _ -> None

  let trueParser = 
    satisfy (fun x -> x = 'T') >>= fun c1 -> 
    satisfy (fun x -> x = 'r') >>= fun c2 -> 
    satisfy (fun x -> x = 'u') >>= fun c3 -> 
    satisfy (fun x -> x = 'e') >>= fun c4 -> 
    pure (implode [c1;c2;c3;c4])

  let falseParser = 
    satisfy (fun x -> x = 'F') >>= fun c1 -> 
    satisfy (fun x -> x = 'a') >>= fun c2 -> 
    satisfy (fun x -> x = 'l') >>= fun c3 -> 
    satisfy (fun x -> x = 's') >>= fun c4 -> 
    satisfy (fun x -> x = 'e') >>= fun c5 -> 
    pure (implode [c1;c2;c3;c4;c5])

  let boolParser =
    trueParser <|> falseParser

let integerParser = 
  natural <|> 
  (
    satisfy (fun x -> x = '-') >>= fun c1 ->
    natural >>= fun n ->
    pure (-1*n)
  )

let nameParser = 
  (char '_' <|> (satisfy is_alpha)) >>= 
  fun h -> many((satisfy is_alphanum) <|> (char '_') <|> (char '\'')) >>=
  fun t -> pure ((implode (h::t)))



type const = 
  | Int of int 
  | Bool of string
  | Unit of unit
  | Name of string
  | Clo of ((string * const) list * const * const * command list)

(* defines a 'stack' data type to be used as the general stack in the interpreter  *)
and 'a stack =
  | Empty 
  | Cons of (const * const stack)

and command =
  | Push of const 
  | Pop of int 
  | Trace of int
  | Add of int 
  | Sub of int 
  | Mul of int 
  | Div of int
  | And
  | Or
  | Not
  | Equal
  | Lte
  | Local 
  | Global
  | Lookup
  | BeginEnd of command list 
  | IfElse of (command list * command list)  
  | Function of (const * const * command list)
  | Call
  | TryEnd of command list
  | Switch of (int * command list) list


  (*checks for constants in the input string to be used by push parser*)
  let constantParse = 
    ((natural >>= fun i -> pure (Int(i)))<|>
    (boolParser >>= fun i -> pure (Bool(i))) <|>
    (parenParser >>= fun i -> pure (Unit()))) <|>
    (nameParser >>= fun i -> pure (Name(i)))

  (* checks for ints in the input string to be used by required parsers *)
  let constInt =
    (natural >>= fun i -> pure (i))
  
  let constName = 
    (nameParser >>= fun i -> pure (Name(i)))


  (* List of more concise parsers that forms a command list*)
  let pushParser =
    (keyword "Push" >> constantParse << ws >>= fun x -> pure (Push x))

  let popParser =
    (keyword "Pop" >> constInt << ws >>= fun x -> pure (Pop x))

  let traceParser =
    (keyword "Trace" >> constInt << ws >>= fun x -> pure (Trace x))

  let addParser =
    (keyword "Add" >> constInt << ws >>= fun x -> pure (Add x))

  let subParser =
    (keyword "Sub" >> constInt << ws >>= fun x -> pure (Sub x))

  let mulParser =
    (keyword "Mul" >> constInt << ws >>= fun x -> pure (Mul x))

  let divParser =
    (keyword "Div" >> constInt << ws >>= fun x -> pure (Div x))

  let andParser =
    (keyword "And" >> ws >>= fun x -> pure(And))

  let orParser =
    (keyword "Or" >> ws >>= fun x -> pure(Or))

  let notParser =
    (keyword "Not" >> ws >>= fun x -> pure(Not))

  let equalParser =
    (keyword "Equal" >> ws >>= fun x -> pure(Equal))

  let lteParser = 
    (keyword "Lte" >> ws >>= fun x -> pure(Lte))

  let localParser = 
    (keyword "Local" >> ws >>= fun x -> pure(Local))

  let globalParser = 
    (keyword "Global" >> ws >>= fun x -> pure(Global))

  let lookupParser = 
    (keyword "Lookup" >> ws >>= fun x -> pure(Lookup))    

  let callParser = 
    (keyword "Call" >> ws >>= fun x -> pure(Call))

  let rec mainParser () = 
    many (pushParser <|> popParser <|> traceParser <|> addParser <|> subParser <|> mulParser <|> divParser 
    <|> andParser <|> orParser <|> notParser <|> equalParser <|> lteParser <|> localParser <|> globalParser 
    <|> lookupParser <|> beginEnd() <|> ifElse() <|> functionParse() <|> callParser <|> tryEnd() <|> switch()) 
    
  and beginEnd() =
  keyword "Begin" >>= fun x -> mainParser() >>= fun commands ->
  keyword "End" >> pure (BeginEnd(commands))

  and ifElse() = 
  keyword "If" >>= fun x -> mainParser () >>= fun ifCommand -> 
  keyword "Else" >>= fun x -> mainParser () >>= fun elseCommand -> 
  keyword "End" >> pure (IfElse(ifCommand,elseCommand))

  and functionParse() =
  keyword "Fun" >>= fun x -> nameParser << ws >>= fun functionName ->
  nameParser << ws >>= fun functionArgs -> mainParser() >>= fun commands ->
  keyword "End" >> pure (Function(Name(functionName),Name(functionArgs),commands))

  and tryEnd() =
  keyword "Try" >>= fun x -> mainParser() >>= fun commands ->
  keyword "End" >> pure (TryEnd(commands))

  and caseParser() = 
  keyword "Case" >> integerParser << ws >>= fun c -> 
  mainParser() >>= fun commands -> 
  pure (c,commands)

  and switch() = 
  keyword "Switch" >> (many (caseParser())) >>= fun cases -> keyword "End" >> pure (Switch(cases))

  let const_to_string (const : const) : string =
    match const with 
    | Int(c) -> string_of_int(c)
    | Bool(c) -> c
    | Unit(c) -> "()"
    | Name(c) -> c
    | Clo(env,name,args,commands) -> "<fun>"

  let is_const_int (const : const) : int option =
    match const with 
    | Int(c) -> (Some c)
    | _ -> None

  let is_const_bool (const : const) =
    match const with
    | Bool(c) -> if (c = "True" || c = "False") then 1 else 0
    | _ -> 0 

  let rec isCharListName (ls : char list) =
    match ls with 
    | [] -> 1 
    | h::t -> if ( is_alphanum(h) || h = '_' || h = '\'' ) then isCharListName(t) else 0

  let rec is_const_name (const :const) =
    match const with
    | Name(c) -> if ( isCharListName(explode(c)) = 1 ) then 1 else 0
    | _ -> 0

  let flattenConst (opt : const option) : const =
    match opt with 
    | None -> Unit()
    | Some c -> c

  let flattenCommandList (commandList : command list option) : command list =
    match commandList with 
    | None -> []
    | Some lst -> lst

  let flattenStackOption (opt : const stack option) : const stack =
    match opt with
    | Some ((Cons(h,t))) -> (Cons(h,t))
    | Some (Empty) -> Empty
    | None -> Empty

  let flattenTupleOption (tupleOption : (const stack * string list * (string * const) list * (string * const) list) option ) : (const stack * string list * (string * const) list * (string * const) list) =
    match tupleOption with
    | Some ( (Cons(h,t)), log, localDict, globalDict) -> (Cons(h,t), log, localDict, globalDict)
    | Some (Empty, log, localDict, globalDict) -> (Empty, log, localDict, globalDict)
    | None -> (Empty,["Error"], [], [])

  let flattenTupleOption2 (tupleOption : (const * const * const stack) option) =
    match tupleOption with 
    | Some(c1,c2,stack) -> (c1,c2,stack)
    | None -> (Int(-1),Int(-1),Empty)

  let flattenTupleOption3 (tupleOption : (const * const stack) option) =
    match tupleOption with 
    | Some(head,stack) -> (head,stack)
    | None -> (Int(-1),Empty)

let peek (stack : 'a stack) = 
  match stack with 
  | Empty -> None
  | Cons (h,t) -> Some h

let popTop (stack : 'a stack) =
  match stack with 
  | Empty -> None 
  | Cons(h,t) -> Some t 

let popTop2 (stack : 'a stack) =
  match stack with 
  | Empty -> None 
  | Cons(h,t) -> Some (h,t) 

let rec popN (n : int) (stack : 'a stack) =
  match stack with 
  | Empty -> None 
  | Cons(h,t) -> if n=0 then Some t else popN (n - 1) (t)

let callPop (stack : 'a stack) =
  match stack with 
  | Empty -> None 
  | Cons (_, Empty) -> None
  | Cons(closure,Cons(value,t)) -> Some (closure,value,t)

let rec pop (n : int) (stack : 'a stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  if (n < 0) then None else
  match (n, stack) with 
  | (n, Empty) -> if (n > 0) then None else Some (Empty, log, localDict, globalDict)
  | (0, Cons(h,t) )  -> Some (Cons(h,t), log, localDict, globalDict)
  | ((n, Cons(h,t) )) -> match t with 
                        | Empty -> if (n = 1) then Some (Empty, log, localDict, globalDict) else None
                        | Cons(head,tail) -> pop (n - 1) (Cons(head,tail)) (log) (localDict) (globalDict)

let push (value: const) (stack : 'a stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match value with
  | Bool(x) -> if (is_const_bool(Bool(x)) = 1) then Some (Cons(value, stack), log, localDict, globalDict) else Some (stack, log, localDict, globalDict)
  | Name(x) -> if (is_const_name(Name(x)) = 1) then Some (Cons(value, stack), log, localDict, globalDict ) else Some (stack, log, localDict, globalDict)
  | _ -> Some (Cons(value, stack),log, localDict, globalDict)

let rec trace (n : int) (stack : 'a stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  if (n < 0) then None else 
  match (n, stack) with
  | (_, Empty) -> if (n > 0) then None else Some (Empty, log, localDict, globalDict )
  | (0, Cons(h,t) ) -> Some (Cons(h,t), log, localDict, globalDict )
  | (n, Cons(h,t) ) -> match t with 
                      | Empty -> if (n = 1) then Some (Empty, const_to_string(h)::log, localDict, globalDict ) else None
                      | Cons(head,tail) -> trace (n - 1) (Cons(head,tail)) (const_to_string(h)::log) (localDict) (globalDict)

let rec add (n : int) (stack : 'a stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  if (n < 0) then None else 
  match (n, stack) with 
  | (_, Empty) -> if (n > 0) then None else Some (Cons(Int(0), Empty), log, localDict, globalDict )
  | (0, Cons(h,t) ) -> push (Int(0)) ( Cons(h,t) ) (log) (localDict) (globalDict)
  | (1, Cons(h,t) ) -> if (is_const_int(h) != None) then Some (Cons(h,t),log, localDict, globalDict) else None
  | (n, Cons(h,t) ) -> match t with 
                       | Empty -> None 
                       | Cons(head,tail) -> match (h, head) with 
                                            | (Int(h), Int(head)) -> add (n - 1) (Cons ( (Int(h + head)), tail)) (log) (localDict) (globalDict)
                                            | _ ->  None

let rec sub (n : int) (stack : 'a stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  if (n < 0) then None else 
  match (n, stack) with 
  | (_, Empty) -> if (n > 0) then None else Some (Cons(Int(0), Empty), log, localDict, globalDict )
  | (0, Cons(h,t) ) -> push (Int(0)) ( Cons(h,t) ) (log) (localDict) (globalDict)
  | (1, Cons(h,t) ) -> if (is_const_int(h) != None) then Some (Cons(h,t), log, localDict, globalDict ) else None
  | (n, Cons(h,t) ) -> match t with 
                       | Empty -> None 
                       | Cons(head,tail) -> match (h, head) with 
                                            | (Int(h), Int(head)) -> sub (n - 1) (Cons(Int(h - head),tail)) (log) (localDict) (globalDict)
                                            | _ -> None

let rec mul (n : int) (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  if (n < 0) then None else 
  match (n, stack) with 
  | (_, Empty) -> if (n > 0) then None else Some (Cons(Int(1), Empty), log, localDict, globalDict )
  | (0, Cons(h,t) ) -> push (Int(1)) ( Cons(h,t) ) (log) (localDict) (globalDict)
  | (1, Cons(h,t) ) -> if (is_const_int(h) != None) then Some (Cons(h,t), log, localDict, globalDict ) else None
  | (n, Cons(h,t) ) -> match t with 
                       | Empty -> None 
                       | Cons(head,tail) -> match (h, head) with  
                                            | (Int(h), Int(head)) -> mul (n - 1) (Cons(Int(h * head),tail)) (log) (localDict) (globalDict)
                                            | _ -> None

let rec div (n : int) (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  if (n < 0) then None else 
  match (n, stack) with 
  | (_, Empty) -> if (n > 0) then None else Some (Cons(Int(1), Empty),log, localDict, globalDict )
  | (0, Cons(h,t) ) -> push (Int(1)) ( Cons(h,t) ) (log) (localDict) (globalDict)
  | (1, Cons(h,t) ) -> if (is_const_int(h) != None) then Some (Cons(h,t), log, localDict, globalDict ) else None
  | (n, Cons(h,t) ) -> match t with 
                       | Empty -> None 
                       | Cons(head,tail) -> match (h, head) with 
                                            | (_, Int(0)) -> None 
                                            | (Int(h), Int(head)) -> div (n - 1) (Cons(Int(h / head),tail)) (log) (localDict) (globalDict)
                                            | _ -> None

let andStack (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match stack with
  | Empty -> None
  | Cons(Bool(x), Cons(Bool(y), _)) -> (match (Bool(x),Bool(y)) with
                                       | (Bool("True"),Bool("True")) -> ( match ( (pop (2) (stack) (log) (localDict) (globalDict)) ) with 
                                                                            | Some (stack, log, localDict, globalDict) -> push (Bool("True")) (stack) (log) (localDict) (globalDict)
                                                                            | _ -> None )
                                       | _ -> ( match ( (pop (2) (stack) (log) (localDict) (globalDict)) ) with 
                                              | Some (stack, log, localDict, globalDict) -> push (Bool("False")) (stack) (log) (localDict) (globalDict)
                                              | _ -> None ) 
                                        )
  | _ -> None 

let orStack (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match stack with
  | Empty -> None
  | Cons(Bool(x), Cons(Bool(y), _)) -> (match (Bool(x),Bool(y)) with
                                       | (Bool("False"),Bool("False")) -> ( match ( pop (2) (stack) (log) (localDict) (globalDict) ) with 
                                                                            | Some (stack, log, localDict, globalDict) -> push (Bool("False")) (stack) (log) (localDict) (globalDict)
                                                                            | _ -> None )
                                       | _ -> ( match ( pop (2) (stack) (log) (localDict) (globalDict) ) with 
                                              | Some (stack, log, localDict, globalDict) -> push (Bool("True")) (stack) (log) (localDict) (globalDict)
                                              | _ -> None ) 
                                        )
  | _ -> None 
 
let notStack (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match stack with 
  | Empty -> None 
  | Cons(Bool(x), _) -> (match Bool(x) with 
                        | Bool("False") -> ( match ( pop (1) (stack) (log) (localDict) (globalDict) ) with 
                                             | Some (stack, log, localDict, globalDict) -> push (Bool("True")) (stack) (log) (localDict) (globalDict)
                                             | _ -> None )
                        | _ -> ( match ( pop (1) (stack) (log) (localDict) (globalDict) ) with 
                                | Some (stack, log, localDict, globalDict) -> push (Bool("False")) (stack) (log) (localDict) (globalDict)
                                | _ -> None )
                        )
  | _ -> None

let equalStack (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match stack with 
  | Empty -> None
  | Cons(Int(x), Cons(Int(y), _)) -> if (x = y) then (match ( pop (2) (stack) (log) (localDict) (globalDict) ) with
                                                      | Some (stack, log, localDict, globalDict) -> push (Bool("True")) (stack) (log) (localDict) (globalDict)
                                                      | _ -> None )
                                                else (match ( pop (2) (stack) (log) (localDict) (globalDict) ) with
                                                      | Some (stack, log, localDict, globalDict) -> push (Bool("False")) (stack) (log) (localDict) (globalDict)
                                                      | _ -> None  )
  | _ -> None
             
let lteStack (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match stack with 
  | Empty -> None
  | Cons(Int(x), Cons(Int(y), _)) -> if (x <= y) then (match ( pop (2) (stack) (log) (localDict) (globalDict) ) with
                                                      | Some (stack, log, localDict, globalDict) -> push (Bool("True")) (stack) (log) (localDict) (globalDict)
                                                      | _ -> None )
                                                else (match ( pop (2) (stack) (log) (localDict) (globalDict) ) with
                                                      | Some (stack, log, localDict, globalDict) -> push (Bool("False")) (stack) (log) (localDict) (globalDict)
                                                      | _ -> None  )
  | _ -> None


let rec addToDict (prevList : (string * const) list) (dict : (string * const) list ) (name : string) (value : const) : (string * const) list =
  match dict with 
  | [] -> [(name,value)]@prevList
  | (key, pair)::t ->  if (key = name) then ([(key, value)]@prevList@t) else addToDict (prevList@[key, pair]) (t) (name) (value)

let local (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match stack with 
  | Empty -> None
  | Cons(Name(name), Cons(const, _)) -> (match ( pop (2) (stack) (log) (localDict) (globalDict) ) with 
                                        | Some (stack, log, localDict, globalDict) -> push (Unit()) (stack) (log) (addToDict ([]) (localDict) (name) (const)) (globalDict)
                                        | _ -> None )
  | _ -> None

let global (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match stack with 
  | Empty -> None
  | Cons(Name(name), Cons(const, _)) -> (match ( pop (2) (stack) (log) (localDict) (globalDict) ) with 
                                        | Some (stack, log, localDict, globalDict) ->  push (Unit()) (stack) (log) (localDict) (addToDict ([]) (globalDict) (name) (const)) 
                                        | _ -> None )
  | _ -> None

let rec searchForBind (dict : (string * const) list ) (name : string) =
  match dict with 
  | [] -> None 
  | (key, pair)::t -> if (key = name) then (Some pair) else searchForBind (t) (name)

let lookup (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match stack with
  | Empty -> None 
  | Cons(Name(name), _) -> ( match (pop 1 stack log localDict globalDict) with 
                           | Some (stack, log, localDict, globalDict) -> (if (searchForBind (localDict) (name) != None) then push (flattenConst (searchForBind (localDict) (name))) (stack) (log) (localDict) (globalDict) 
                           else if (searchForBind (globalDict) (name) != None) then push (flattenConst (searchForBind (globalDict) (name))) (stack) (log) (localDict) (globalDict)
                           else None )
                           | _ -> None )
  | _ -> None

let rec beginEndFunction (triple : command list * const stack * (string * const) list) (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match triple with 
  | ([],tempStack,beginEndLocalDict) ->  if ((pop (1) (tempStack) (log) (beginEndLocalDict) (globalDict)) != None) then push (flattenConst (peek(tempStack))) (stack) (log) (localDict) (globalDict) else None
  | (h::t,tempStack,beginEndLocalDict) ->  match eval (h::t) (tempStack,log,beginEndLocalDict,globalDict) with
                                            | None -> None 
                                            | Some(tempStack,log,beginEndLocalDict,globalDict) -> if ((pop (1) (tempStack) (log) (beginEndLocalDict) (globalDict)) != None) then push (flattenConst (peek(tempStack))) (stack) (log) (localDict) (globalDict) else None  

and ifElseFunction (tuple : command list * command list) (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match stack with 
  | Cons(Bool("True"), _) -> (match tuple with
                              | (ls1,ls2) -> eval (ls1) (flattenTupleOption(pop 1 stack log localDict globalDict))
                              )
  | Cons(Bool("False"), _) -> (match tuple with
                              | (ls1,ls2) -> eval (ls2) (flattenTupleOption(pop 1 stack log localDict globalDict))
                              )
  | _ -> None

and defineFunction (name : const) (args : const) (commandlst : command list) (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  if (is_const_name(name) = 1 && is_const_name(args) = 1) 
  then Some(stack,log,(addToDict ([]) (localDict) (const_to_string(name)) (Clo(localDict,name,args,commandlst))),globalDict)
  else None

and call (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match flattenTupleOption2 ( callPop(stack) ) with 
  | (Clo(env,fname,arg,commands),const2, originalStack) -> ( match eval (commands) (Empty, log, (env @ [(const_to_string(arg)),const2] @ [(const_to_string(fname)),Clo(env,fname,arg,commands)] @ globalDict), globalDict) with 
                                                            | Some(callStack,log,callDict,globalDict) -> push (flattenConst(peek(callStack))) (originalStack) (log) (localDict) (globalDict)
                                                            | None -> None )
  | _ -> None 

and tryEndFunction (commands : command list) (tempStack : const stack) (tryEndDict : (string * const) list) (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  match eval (commands) (tempStack,log,tryEndDict,globalDict) with
  | None -> Some(stack,log,localDict,globalDict) 
  | Some(tempStack,log,localDict,globalDict) -> if ((pop (1) (tempStack) (log) (localDict) (globalDict)) != None) then push (flattenConst (peek(tempStack))) (stack) (log) (localDict) (globalDict) else None   

and switch (caseList : (int * command list) list) (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) : (const stack * string list * (string * const) list * (string * const) list) option =
  let rec aux (caseList : (int * command list) list) (brokenUpStack) = 
    match (brokenUpStack) with 
    | (Int(n), newStack) -> 
                        (match caseList with 
                        | [] -> None 
                        | h::t -> ( 
                                    match h with 
                                   | (i,commands) -> if (i = n) then (eval (commands) (newStack,log,localDict,globalDict) ) else aux (t) (brokenUpStack)
                                   )  
                        ) 
    | _ -> None
  in aux caseList (flattenTupleOption3 (popTop2 (stack)))  

 and execute (command : command) (stack : const stack) (log : string list) (localDict : (string * const) list) (globalDict : (string * const) list) =
  match command with 
  | Push(x) -> push x
  | Pop(x) -> pop x 
  | Trace(x) -> trace x
  | Add(x) -> add x
  | Sub(x) -> sub x
  | Mul(x) -> mul x
  | Div(x) -> div x
  | And -> andStack
  | Or -> orStack
  | Not -> notStack
  | Equal -> equalStack
  | Lte -> lteStack
  | Local -> local 
  | Global -> global
  | Lookup -> lookup 
  | BeginEnd(commands) -> beginEndFunction (commands,Empty,localDict) 
  | IfElse(ls1,ls2) -> ifElseFunction (ls1,ls2) 
  | Function(fname,arg,commandlst) -> defineFunction (fname) (arg) (commandlst)
  | Call -> call 
  | TryEnd(commands) -> tryEndFunction (commands) (Empty) ([])
  | Switch(caseList) -> switch (caseList) 

and eval (commandlst : command list) (quad : const stack * string list * ((string * const) list) * ((string * const) list) ) : (const stack * string list * (string * const) list * (string * const) list) option = 
  match commandlst with
  | [] -> ( match (quad) with 
            | (Empty,["Error"],localDict,globalDict) -> None
            | (stack,log,localDict,globalDict) -> Some (stack,log,localDict,globalDict)
          )
  | h::t -> ( match quad with 
            | (Empty,["Error"],_,_) -> None
            | (stack,log,localDict,globalDict) -> eval (t) (flattenTupleOption(execute h stack log localDict globalDict stack log localDict globalDict))
            )


            
let interp (src : string) : string list = 
  match parse (mainParser()) src with
  | Some (h, []) -> (match eval (h) (Empty, [], [], []) with
                     | None -> ["Error"]
                     | Some(_,log,_,_) -> log )
  | _ -> ["Error"] 

(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src


