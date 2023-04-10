let program = "(+ (- 3 10) 5)";;

exception Unreachable;;
exception UnexpectedChar of string;;
exception UnexpectedToken of string;;
exception IncompleteExpression;;

type operator =
    | Add
    | Subract
    | Multiply
    | Divide
;;

type token =
    | LParen
    | RParen
    | NumTok of float
    | Op of operator
;;

type ast =
    | Number of float
    | ArithCalc of {op: operator; lhs: ast; rhs: ast}

let is_white ch =
    match ch with
    | ' ' | '\t' | '\n' -> true
    | _ -> false

let is_digit ch =
    match ch with
    | '0' .. '9' -> true
    | _ -> false;;

let to_digit ch =
    match ch with
    | '0' .. '9' -> int_of_char ch - int_of_char '0'
    | _ -> raise Unreachable

let string_of_operator op = match op with
    | Add -> "+"
    | Subract -> "-"
    | Multiply -> "*"
    | Divide -> "/"

let string_of_token token =
    match token with
    | LParen -> "LParen"
    | RParen -> "RParen"
    | NumTok n -> "Number: " ^ (string_of_float n)
    | Op op -> "Operator: " ^ (string_of_operator op)

let string_of_tokens tokens =
    tokens
    |> List.map string_of_token
    |> String.concat "\n"

let string_of_ast ast =
    let rec impl ast indent =
        match ast with
        | Number n -> indent ^ string_of_float n
        | ArithCalc {op = op; lhs = lhs; rhs = rhs} ->
                let opstr = indent ^ string_of_operator op
                in
                let indent = indent ^ "  "
                in
                let lhsstr = impl lhs indent
                in
                let rhsstr = impl rhs indent
                in
                opstr ^ "\n" ^ lhsstr ^ "\n" ^ rhsstr
    in
    impl ast ""

let string_of_may_ast may_ast =
    match may_ast with
    | Some ast -> string_of_ast ast
    | None -> "(Nop)"

let lex src =
    let srcLen = String.length src
    in
    let next_char i =
        if i >= srcLen then
            (None, i)
        else
            let ch = String.get src i in (Some ch, i+1)
    in
    let next_num i =
        let rec next_num_impl i acc =
            let may_ch, i = next_char i
            in
            match may_ch with
            | Some ch ->
                    if is_digit ch then
                        let acc' = acc * 10 + to_digit ch
                        in
                        next_num_impl i acc'
                    else
                        (acc, (i-1))
            | None -> (acc, i)
        in
        next_num_impl i 0
    in
    let rec lex_impl i acc =
        let may_ch, i = next_char i
        in
        match may_ch with
        | None -> (acc, i)
        | Some ch ->
                if is_white ch then
                    lex_impl i acc
                else if is_digit ch then
                    let n, i = next_num (i-1)
                    in
                    lex_impl i (NumTok (float_of_int n) :: acc)
                else
                    let may_token = match ch with
                    | '(' -> Some LParen
                    | ')' -> Some RParen
                    | '+' -> Some (Op Add)
                    | '-' -> Some (Op Subract)
                    | '*' -> Some (Op Multiply)
                    | '/' -> Some (Op Divide)
                    | _ -> None
                    in
                    match may_token with
                    | None -> raise (UnexpectedChar (String.sub src (i-1) (srcLen-i+1)))
                    | Some token -> lex_impl i (token :: acc)
    in
    let tokens, _ = lex_impl 0 []
    in
    List.rev tokens
;;

let rec parse tokens =
    let fail_if_none may_ast =
        match may_ast with
        | Some ast -> ast
        | None -> raise IncompleteExpression
    in
    let rec impl tokens =
        match tokens with
        | first :: (second :: rest) -> (
                match first, second with
                | LParen, Op op ->
                        let may_lhs, rest = (match List.hd rest with
                        | NumTok n -> (Some (Number n), List.tl rest)
                        | _ -> impl rest
                        )
                        in
                        let lhs = fail_if_none may_lhs
                        in
                        let may_rhs, rest = (match List.hd rest with
                        | NumTok n -> (Some (Number n), List.tl rest)
                        | _ -> impl rest
                        )
                        in
                        let rhs = fail_if_none may_rhs
                        in
                        let ast = ArithCalc {op = op; lhs = lhs; rhs = rhs}
                        in
                        let car, cdr = (List.hd rest, List.tl rest)
                        in
                        (match car with
                        | RParen -> (Some ast, cdr)
                        | _ -> raise (UnexpectedToken ("RParen is required: " ^ string_of_tokens rest))
                        )
                | LParen, _ -> raise (UnexpectedToken ("Operator must come after LParen: " ^ string_of_tokens tokens))
                | _ -> raise (UnexpectedToken ("Unexpected Token: " ^ string_of_tokens tokens))
            )
        | [] -> (None, [])
        | _ -> raise (UnexpectedToken ("Invalid token is left: " ^ string_of_tokens tokens))
    in
    let may_ast, _ = impl tokens
    in
    may_ast
;;

let rec eval ast =
    match ast with
    | Number n -> n
    | ArithCalc {op = op; lhs = lhs; rhs = rhs} ->
            let lhs = eval lhs
            in
            let rhs = eval rhs
            in
            (match op with
            | Add -> lhs +. rhs
            | Subract -> lhs -. rhs
            | Multiply -> lhs *. rhs
            | Divide -> lhs /. rhs
            )
;;

let rec interact _ =
    print_string ">>> ";
    (let input = try read_line () with
    | End_of_file -> exit 0
    in
    let tokens = lex input
    in
    let may_ast = parse tokens
    in
    match may_ast with
    | None -> print_endline "(Nop)"
    | Some ast -> print_endline @@ string_of_float @@ eval ast);
    interact ()
;;

print_endline "Press CTRL-D to quit.";;
interact();;
