(* Delimiters end the token being completed. Besides whitespace,
   brackets and quotes, the set covers prefix syntax that sits
   directly against a symbol: Clojure's @deref, ^meta, ~unquote,
   #'var and `syntax-quote, Elixir's %Struct and &capture. Module
   separators (lists:reverse, clojure.string, str/join, Enum.map)
   stay in the token. One dialect-agnostic set on purpose: a
   mis-split token only costs a completion the server declines, so
   unlike form completeness below it earns no per-dialect
   machinery. *)
let is_delimiter = function
  | ' ' | '\t' | '(' | ')' | '[' | ']' | '{' | '}'
  | '"' | '\'' | '`' | ',' | ';' | '@' | '#' | '^' | '~' | '\\'
  | '&' | '%' -> true
  | _ -> false

let split_for_completion line =
  let n = String.length line in
  let rec token_start i =
    if i = 0 || is_delimiter line.[i - 1] then i else token_start (i - 1)
  in
  let s = token_start n in
  (String.sub line 0 s, String.sub line s (n - s))

type dialect = Lisp | Erlang | Generic

(* Elixir is checked before Erlang: repartee reports both, and the
   dot rule must not win there. Elixir's do/end blocks can't be
   bracket-counted either, so it falls back to Generic. *)
let dialect_of_versions names =
  let has name = List.mem name names in
  if has "clojure" || has "babashka" || has "basilisp" || has "jank" then Lisp
  else if has "elixir" then Generic
  else if has "erlang" then Erlang
  else Generic

(* Helpers shared by the scanners below. [scan_quoted] returns the
   position just past the closing quote, or [None] when the input
   ends inside the quoted region (or on a dangling escape), in which
   case the form cannot be complete. *)
let rec skip_line input i =
  if i >= String.length input || input.[i] = '\n' then i
  else skip_line input (i + 1)

let rec scan_quoted input i quote =
  if i >= String.length input then None
  else
    match input.[i] with
      | '\\' ->
          if i + 1 >= String.length input then None
          else scan_quoted input (i + 2) quote
      | c when c = quote -> Some (i + 1)
      | _ -> scan_quoted input (i + 1) quote

(* Bracket/string scanner shared by the Lisp and Generic rules. With
   [lisp], ;-comments hide the rest of the line and a backslash
   starts a character literal (\a, \(, \newline). *)
let balanced ~lisp input =
  let n = String.length input in
  let rec scan i depth =
    if i >= n then Some depth
    else
      match input.[i] with
        | ';' when lisp -> scan (skip_line input (i + 1)) depth
        | '\\' when lisp -> if i + 1 >= n then None else scan (i + 2) depth
        | '"' ->
            (match scan_quoted input (i + 1) '"' with
               | Some j -> scan j depth
               | None -> None)
        | '(' | '[' | '{' -> scan (i + 1) (depth + 1)
        | ')' | ']' | '}' -> scan (i + 1) (depth - 1)
        | _ -> scan (i + 1) depth
  in
  match scan 0 0 with
    | Some depth -> depth <= 0
    | None -> false

(* Erlang forms end with a dot. The dot flag set here survives only
   across whitespace and %-comments to the end of the input, so 3.14
   is not a terminator, and neither is a dot inside a string, a
   quoted atom or a [$c] character literal. *)
let erlang_complete input =
  let n = String.length input in
  let rec scan i depth dot =
    if i >= n then Some (depth, dot)
    else
      match input.[i] with
        | '%' -> scan (skip_line input (i + 1)) depth dot
        | '$' -> if i + 1 >= n then None else scan (i + 2) depth false
        | ('"' | '\'') as quote ->
            (match scan_quoted input (i + 1) quote with
               | Some j -> scan j depth false
               | None -> None)
        | '(' | '[' | '{' -> scan (i + 1) (depth + 1) false
        | ')' | ']' | '}' -> scan (i + 1) (depth - 1) false
        | '.' -> scan (i + 1) depth true
        | ' ' | '\t' | '\n' -> scan (i + 1) depth dot
        | _ -> scan (i + 1) depth false
  in
  match scan 0 0 false with
    | Some (depth, dot) -> dot && depth <= 0
    | None -> false

let complete dialect input =
  String.trim input <> ""
  && (match dialect with
        | Lisp -> balanced ~lisp:true input
        | Generic -> balanced ~lisp:false input
        | Erlang -> erlang_complete input)
