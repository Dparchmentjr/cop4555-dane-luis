// Lexer and recursive-descent parser for PCF
// Geoffrey Smith        October 25, 2010

// This sets F# to read from whatever directory contains this source file.
System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__

// The namespace is "Parser" (because the source file is "parser.fsx"),
// so the full name of this module is "Parser.Lex".
module Lex =

  type token =
  | IDTOK of string | NUMTOK of int | TRUETOK | FALSETOK | SUCCTOK | PREDTOK
  | ISZEROTOK | IFTOK | THENTOK | ELSETOK | FUNTOK | RECTOK | ARROWTOK
  | LPARENTOK | RPARENTOK | LETTOK | EQUALTOK | INTOK | EOF

  let explode (s : string) = [for c in s -> c]

  let isWhite c = c = ' ' || c = '\n' || c = '\r' || c = '\t'

  let isAlpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  let isDigit c = '0' <= c && c <= '9'

  let keyword = function
  | "true"   -> TRUETOK
  | "false"  -> FALSETOK
  | "succ"   -> SUCCTOK
  | "pred"   -> PREDTOK
  | "iszero" -> ISZEROTOK
  | "if"     -> IFTOK
  | "then"   -> THENTOK
  | "else"   -> ELSETOK
  | "fun"    -> FUNTOK
  | "rec"    -> RECTOK
  | "let"    -> LETTOK
  | "in"     -> INTOK
  | ident    -> IDTOK ident

  let rec getid ident = function
  | c :: cs when (isAlpha c || isDigit c) -> getid (ident + string c) cs
  | cs                                    -> (keyword ident, cs)

  let rec getnum num = function
  | c :: cs when isDigit c -> getnum (10*num + int c - int '0') cs
  | cs                     -> (NUMTOK num, cs)

  let rec gettok = function
  | [] -> (EOF, [])
  | c :: cs when isWhite c -> gettok cs
  | c :: cs when isAlpha c -> getid (string c) cs
  | c :: cs when isDigit c -> getnum (int c - int '0') cs
  | '(' :: cs -> (LPARENTOK, cs)
  | ')' :: cs -> (RPARENTOK, cs)
  | '=' :: cs -> (EQUALTOK, cs)
  | '-' :: '>' :: cs -> (ARROWTOK, cs)
  | '#' :: cs -> let rec skipline = function
                 | []          -> (EOF, [])
                 | '\n' :: cs1 -> gettok cs1
                 | _ :: cs1    -> skipline cs1
                 skipline cs
  | c :: cs -> (printf "Skipping illegal character: '%c'\n" c; gettok cs)

  let tokenize cs =
    let rec gettoks toks cs =
      match gettok cs with
      | (EOF, cs) -> List.rev (EOF::toks)
      | (tok, cs) -> gettoks (tok::toks) cs
    gettoks [] cs

  let lexstr sourcecode = sourcecode |> explode |> tokenize

  let lexfile filename = System.IO.File.ReadAllText filename |> explode |> tokenize

// The full name of the module is "Parser.Parse".
module Parse =

  type term =
  | ID of string | NUM of int | BOOL of bool | SUCC | PRED | ISZERO
  | IF of term * term * term | APP of term * term
  | FUN of string * term | REC of string * term | ERROR of string

  // Exp ::= x | n | true | false | succ | pred | iszero |
  //         if Exps then Exps else Exps | fun x -> Exps |
  //         rec x -> Exps | (Exps) | let x = Exps in Exps
  //
  // Exps ::= Exps Exp | Exp
  //
  // We resolve the ambiguity in the grammar by making concatenation
  // (i.e. function application) bind tighter than if, fun, rec, and let.

  open Lex

  /// startsExp tok tests whether tok can start an Exp.
  let startsExp = function
  | IDTOK x  -> true
  | NUMTOK n -> true
  | tok -> List.exists ((=) tok) [TRUETOK; FALSETOK; SUCCTOK; PREDTOK; ISZEROTOK;
                                  IFTOK; FUNTOK; RECTOK; LPARENTOK; LETTOK]

  /// parseExp takes a token list that ends with EOF, and returns a pair consisting
  /// of the largest Exp at the start of the token list, together with the remaining
  /// token list, which will still end with EOF.
  let rec parseExp = function
  | IDTOK x :: toks   -> (ID x, toks)
  | NUMTOK n :: toks  -> (NUM n, toks)
  | TRUETOK :: toks   -> (BOOL true, toks)
  | FALSETOK :: toks  -> (BOOL false, toks)
  | SUCCTOK :: toks   -> (SUCC, toks)
  | PREDTOK :: toks   -> (PRED, toks)
  | ISZEROTOK :: toks -> (ISZERO, toks)
  | IFTOK :: toks     ->
      match parseExps toks with
      | (ERROR s, toks1) -> (ERROR s, toks1)
      | (guard, THENTOK :: toks1) ->
          match parseExps toks1 with
          | (ERROR s, toks2) -> (ERROR s, toks2)
          | (thenbranch, ELSETOK :: toks2) ->
              match parseExps toks2 with
              | (ERROR s, toks3) -> (ERROR s, toks3)
              | (elsebranch, toks3) -> (IF (guard, thenbranch, elsebranch), toks3)
          | (thenbranch, tok2 :: toks2) ->
              (ERROR (sprintf "Expected 'else', found %A" tok2), [EOF])
          | (thenbranch, []) -> (ERROR "Lexer error: missing EOF token", [EOF])
      | (guard, tok1 :: toks1) ->
          (ERROR (sprintf "Expected 'then', found %A" tok1), [EOF])
      | (guard, []) -> (ERROR "Lexer error: missing EOF token", [EOF])
  | FUNTOK :: IDTOK x :: ARROWTOK :: toks ->
      match parseExps toks with
      | (ERROR s, toks1) -> (ERROR s, toks1)
      | (body, toks1)    -> (FUN (x, body), toks1)
  | FUNTOK :: IDTOK x :: tok :: toks ->
      (ERROR (sprintf "Expected '->' after 'fun %s', found %A" x tok), [EOF])
  | FUNTOK :: IDTOK x :: [] -> (ERROR "Lexer error: missing EOF token", [EOF])
  | FUNTOK :: tok :: toks ->
      (ERROR (sprintf "Expected identifier after 'fun', found %A" tok), [EOF])
  | FUNTOK :: [] -> (ERROR "Lexer error: missing EOF token", [EOF])
  | RECTOK :: IDTOK x :: ARROWTOK :: toks ->
      match parseExps toks with
      | (ERROR s, toks1) -> (ERROR s, toks1)
      | (body, toks1)    -> (REC (x, body), toks1)
  | RECTOK :: IDTOK x :: tok :: toks ->
     (ERROR (sprintf "Expected '->' after 'rec %s', found %A" x tok), [EOF])
  | RECTOK :: IDTOK x :: [] -> (ERROR "Lexer error: missing EOF token", [EOF])
  | RECTOK :: tok :: toks ->
      (ERROR (sprintf "Expected identifier after 'rec', found %A" tok), [EOF])
  | RECTOK :: [] -> (ERROR "Lexer error: missing EOF token", [EOF])
  | LPARENTOK :: toks ->
      match parseExps toks with
      | (ERROR s, toks1) -> (ERROR s, toks1)
      | (term1, RPARENTOK :: toks1) -> (term1, toks1)
      | (term1, tok1 :: toks1) -> (ERROR (sprintf "Expected ')', found %A" tok1), [EOF])
      | (term1, []) -> (ERROR "Lexer error: missing EOF token", [EOF])
  | LETTOK :: IDTOK x :: EQUALTOK :: toks ->
      match parseExps toks with
      | (ERROR s, toks1) -> (ERROR s, toks1)
      | (term1, INTOK :: toks1) ->
          match parseExps toks1 with
          | (ERROR s, toks2) -> (ERROR s, toks2)
          | (term2, toks2) ->
              // "let x = e1 in e2" is syntactic sugar for "(fun x -> e2) e1"
              (APP (FUN (x, term2), term1), toks2)
      | (term1, tok1 :: toks1) -> (ERROR (sprintf "Expected 'in', found %A" tok1), [EOF])
      | (term1, []) -> (ERROR "Lexer error: missing EOF token", [EOF])
  | LETTOK :: IDTOK x :: tok :: toks ->
      (ERROR (sprintf "Expected '=' after 'let %s', found %A" x tok), [EOF])
  | LETTOK :: IDTOK x :: [] -> (ERROR "Lexer error: missing EOF token", [EOF])
  | LETTOK :: tok :: toks ->
      (ERROR (sprintf "Expected identifier after 'let', found %A" tok), [EOF])
  | LETTOK :: [] -> (ERROR "Lexer error: missing EOF token", [EOF])
  | tok :: toks ->
      (ERROR (sprintf "Expected expression, found %A" tok), [EOF])
  | [] -> (ERROR "Lexer error: missing EOF token", [EOF])

  /// parseExps takes a token list that ends with EOF, and returns the longest
  /// sequence of Exp's at the start of the token list, together with the remaining
  /// token list, which will still end with EOF.
  and parseExps toks =
      let rec parseExps_aux = function
      | (ERROR s, toks1) -> (ERROR s, toks)
      | (term1, tok1 :: toks1) when startsExp tok1 ->
          match parseExp (tok1 :: toks1) with
          | (ERROR s, toks2) -> (ERROR s, toks2)
          | (term2, toks2) -> parseExps_aux (APP (term1, term2), toks2)
      | (term1, toks1) -> (term1, toks1)
      parseExps_aux (parseExp toks)

  let parse toks =
    match parseExps toks with
    | (ERROR s, toks1) -> ERROR s
    | (term1, EOF :: toks1) -> term1
    | (term1, tok1 :: toks1) -> ERROR (sprintf "EOF expected, found %A" tok1)
    | (term1, []) -> ERROR "Lexer error: missing EOF token"

  let parsestr sourcecode = sourcecode |> lexstr |> parse

  let parsefile filename = filename |> lexfile |> parse
