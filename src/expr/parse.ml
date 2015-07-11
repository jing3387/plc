let parse str = Exprpar.main Exprlex.read (Lexing.from_string str)
