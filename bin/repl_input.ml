(* Delimiters end the token being completed. Besides whitespace,
   brackets and quotes, the set covers prefix syntax that sits
   directly against a symbol: Clojure's @deref, ^meta, ~unquote,
   #'var and `syntax-quote, Elixir's %Struct and &capture. Module
   separators (lists:reverse, clojure.string, str/join, Enum.map)
   stay in the token. *)
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
