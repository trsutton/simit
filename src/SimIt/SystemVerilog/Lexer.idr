module SimIt.SystemVerilog.Lexer

import Text.Lexer

%access public export
%default total

-------------------------------------------------------------------------------
-- Lexical Tokens
-------------------------------------------------------------------------------

data LexicalToken : Type where
  ADD_ASSIGNMENT_OP : LexicalToken
  AND_ASSIGNMENT_OP : LexicalToken
  ASHIFT_LEFT : LexicalToken
  ASHIFT_RIGHT : LexicalToken
  ASHIFT_LEFT_ASSIGNMENT_OP : LexicalToken
  ASHIFT_RIGHT_ASSIGNMENT_OP : LexicalToken
  ASSIGNMENT_OP : LexicalToken
  BITWISE_AND : LexicalToken
  BITWISE_OR : LexicalToken
  BITWISE_NOT : LexicalToken
  CASE_EQ : LexicalToken
  CASE_NE : LexicalToken
  COLON : LexicalToken
  COMMENT : String -> LexicalToken
  COMPILER_DIRECTIVE : String -> LexicalToken
  COND_OP : LexicalToken
  DEC : LexicalToken
  DIV : LexicalToken
  DIV_ASSIGNMENT_OP : LexicalToken
  DOLLAR : LexicalToken
  DOT : LexicalToken
  EQUIVALENCE : LexicalToken
  GT : LexicalToken
  GTE : LexicalToken
  IDENTIFIER : (escaped: Bool) -> String -> LexicalToken
  IMPLICATION : LexicalToken
  INC : LexicalToken
  KEYWORD : String -> LexicalToken
  LBRACE : LexicalToken
  LBRACKET : LexicalToken
  LPARENS : LexicalToken
  LOGICAL_AND : LexicalToken
  LOGICAL_EQ : LexicalToken
  LOGICAL_NE : LexicalToken
  LOGICAL_NOT : LexicalToken
  LOGICAL_OR : LexicalToken
  LSHIFT_LEFT : LexicalToken
  LSHIFT_LEFT_ASSIGNMENT_OP : LexicalToken
  LSHIFT_RIGHT : LexicalToken
  LSHIFT_RIGHT_ASSIGNMENT_OP : LexicalToken
  LT : LexicalToken
  LTE : LexicalToken
  MINUS : LexicalToken
  MODULUS : LexicalToken
  MODULUS_ASSIGNMENT_OP : LexicalToken
  MUL_ASSIGNMENT_OP : LexicalToken
  MULT : LexicalToken
  NAND : LexicalToken
  NOR : LexicalToken
  OR_ASSIGNMENT_OP : LexicalToken
  PLUS : LexicalToken
  POWER : LexicalToken
  RBRACE : LexicalToken
  RBRACKET : LexicalToken
  RPARENS : LexicalToken
  SUB_ASSIGNMENT_OP : LexicalToken
  SYS_TASK_OR_FUNC_NAME : String -> LexicalToken
  WHITESPACE : String -> LexicalToken
  WILDCARD_EQ : LexicalToken
  WILDCARD_NE : LexicalToken
  XNOR : String -> LexicalToken
  XOR : LexicalToken
  XOR_ASSIGNMENT_OP : LexicalToken

compiler_directives : List String
compiler_directives =
  [ "`__FILE__", "`__LINE__", "`begin_keywords", "`celldefine", "`default_nettype"
  , "`define", "`else", "`elsif", "`end_keywords", "`endcelldefine", "`endif"
  , "`ifdef", "`ifndef", "`include", "`line", "`nounconnected_drive", "`pragma"
  , "`resetall", "`timescale", "`unconnected_drive", "`undef", "`undefineall"
  , "`default_decay_time", "`default_trireg_strength", "`delay_mode_distributed"
  , "`delay_mode_path", "`delay_mode_unit", "`delay_mode_zero"
  ]

compilerDirectivesDescByLength : List Lexer
compilerDirectivesDescByLength = map exact $ sortBy comparingLength compiler_directives
where
  comparingLength : String -> String -> Ordering
  comparingLength x y = compare (length y) (length x)

keywords : List String
keywords =
  [ "accept_on", "alias", "always", "always_comb", "always_ff", "always_latch"
  , "and", "assert", "assign", "assume", "automatic", "before", "begin", "bind"
  , "bins", "binsof", "bit", "break", "buf", "bufif0", "bufif1", "byte", "case"
  , "casex", "casez", "cell", "chandle", "checker", "class", "clocking", "cmos"
  , "config", "const", "constraint", "context", "continue", "cover", "covergroup"
  , "coverpoint", "cross", "deassign", "default", "defparam", "design", "disable"
  , "dist", "do", "edge", "else", "end", "endcase", "endchecker", "endclass"
  , "endclocking", "endconfig", "endconfig", "endfunction", "endgenerate", "endgroup"
  , "endinterface", "endmodule", "endpackage", "endprimitive", "endprogram", "endproperty"
  , "endspecify", "endsequence", "endtable", "endtask", "enum", "event", "eventually"
  , "expect", "export", "extends", "extern", "final", "first_match", "for", "force"
  , "foreach", "forever", "fork", "forkjoin", "function", "generate", "genvar"
  , "global", "highz0", "highz1", "if", "iff", "ifnone", "ignore_bins", "illegal_bins"
  , "implements", "implies", "import", "incdir", "include", "initial", "inout"
  , "input", "inside", "instance", "int", "integer", "interconnect", "interface"
  , "intersect", "join", "join_any", "join_none", "large", "let", "liblist", "library"
  , "local", "localparam", "logic", "longint", "macromodule", "matches", "medium"
  , "modport", "module", "nand", "negedge", "nettype", "new", "nexttime", "nmos"
  , "nor", "noshowcancelled", "not", "notif0", "notif1", "null", "or", "output"
  , "package", "packed", "parameter", "pmos", "posedge", "primitive", "priority"
  , "program", "property", "protected", "pull0", "pull1", "pulldown", "pullup"
  , "pulsestyle_ondetect", "pulsestyle_onevent", "pure", "rand", "randc", "randcase"
  , "randsequence", "rcmos", "real", "realtime", "ref", "reg", "reject_on", "release"
  , "repeat", "restrict", "return", "rnmos", "rpmos", "rtran", "rtranif0", "rtranif1"
  , "s_always", "s_eventually", "s_nexttime", "s_until", "s_until_with", "scalared"
  , "sequence", "shortint", "shortreal", "showcancelled", "signed", "small", "soft"
  , "solve", "specify", "specparam", "static", "string", "strong", "strong0", "strong1"
  , "struct", "super", "supply0", "supply1", "sync_accept_on", "sync_reject_on"
  , "table", "tagged", "task", "this", "throughout", "time", "timeprecision", "timeunit"
  , "tran", "tranif0", "tranif1", "tri", "tri0", "tri1", "triand", "trior", "trireg"
  , "type", "typedef", "union", "unique", "unique0", "unsigned", "until", "until_with"
  , "untyped", "use", "uwire", "var", "vectored", "virtual", "void", "wait", "wait_order"
  , "wand", "weak", "weak0", "weak1", "while", "wildcard", "wire", "with", "within"
  , "wor", "xnor", "xor"
  ]

keywordsDescByLength : List Lexer
keywordsDescByLength = map exact $ sortBy comparingLength keywords
  where
    comparingLength : String -> String -> Ordering
    comparingLength x y = compare (length y) (length x)

-------------------------------------------------------------------------------
-- Lexers
-------------------------------------------------------------------------------

whitespace : Lexer
whitespace = oneOf " \t\n\r"

add_assignment_op : Lexer
add_assignment_op = exact "+="

and_assignment_op : Lexer
and_assignment_op = exact "&="

ashift_left : Lexer
ashift_left = exact "<<<"

ashift_right : Lexer
ashift_right = exact ">>>"

ashift_left_assignment_op : Lexer
ashift_left_assignment_op = exact "<<<="

ashift_right_assignment_op : Lexer
ashift_right_assignment_op = exact ">>>="

assignment_op : Lexer
assignment_op = exact "="

bitwise_and : Lexer
bitwise_and = exact "&"

bitwise_or : Lexer
bitwise_or = exact "|"

bitwise_not : Lexer
bitwise_not = exact "~"

case_eq : Lexer
case_eq = exact "==="

case_ne : Lexer
case_ne = exact "!=="

colon : Lexer
colon = exact ":"

comment : Lexer
comment = (lineComment (exact "//")) <|> blockCommentWithoutNestedBlockComment
  where
    start : Lexer
    start = exact "/*"
    
    end : Lexer
    end = exact "*/"

    blockCommentWithoutNestedBlockComment : Lexer
    blockCommentWithoutNestedBlockComment = surround start end (non $ blockComment start end)

compiler_directive : Lexer
compiler_directive = choice compilerDirectivesDescByLength

conditional_op : Lexer
conditional_op = exact "?"

dec : Lexer
dec = exact "--"

div : Lexer
div = exact "/"

div_assignment_op : Lexer
div_assignment_op = exact "/="

dollar : Lexer
dollar = is '$'

dot : Lexer
dot = exact "."

equivalence : Lexer
equivalence = exact "<->"

gt : Lexer
gt = exact ">"

gte : Lexer
gte = exact ">="

identifier : Lexer
identifier = simple_identifier <|> escaped_identifier
  where
    simple_identifier : Lexer
    simple_identifier = first <+> rest
      where
        first : Lexer
        first = alpha <|> is '_'

        rest : Recognise False
        rest = many (alphaNum <|> is '_' <|> is '$')

    escaped_identifier : Lexer
    escaped_identifier = (escape '\\' (some $ pred isPrintable)) <+> whitespace
      where
        isPrintable : Char -> Bool
        isPrintable c = let x = ord c in
                        x >= 33 && x <= 126

implication : Lexer
implication = exact "->"

inc : Lexer
inc = exact "++"

keyword : Lexer
keyword = choice keywordsDescByLength

logical_and : Lexer
logical_and = exact "&&"

logical_eq : Lexer
logical_eq = exact "=="

logical_ne : Lexer
logical_ne = exact "!="

logical_not : Lexer
logical_not = exact "!"

logical_or : Lexer
logical_or = exact "||"

lbrace : Lexer
lbrace = exact "{"

lbracket : Lexer
lbracket = exact "["

lparens : Lexer
lparens = exact "("

lshift_left : Lexer
lshift_left = exact "<<"

lshift_right : Lexer
lshift_right = exact ">>"

lshift_left_assignment_op : Lexer
lshift_left_assignment_op = exact "<<="

lshift_right_assignment_op : Lexer
lshift_right_assignment_op = exact ">>="

lt : Lexer
lt = exact "<"

lte : Lexer
lte = exact "<="

minus : Lexer
minus = exact "-"

modulus : Lexer
modulus = exact "%"

modulus_assignment_op : Lexer
modulus_assignment_op = exact "%="

mul_assignment_op : Lexer
mul_assignment_op = exact "*="

mult : Lexer
mult = exact "*"

nand : Lexer
nand = exact "~&"

nor : Lexer
nor = exact "~|"

or_assignment_op : Lexer
or_assignment_op = exact "|="

plus : Lexer
plus = exact "+"

power : Lexer
power = exact "**"

rbrace : Lexer
rbrace = exact "}"

rbracket : Lexer
rbracket = exact "]"

rparens : Lexer
rparens = exact ")"

sub_assignment_op : Lexer
sub_assignment_op = exact "-="

sys_tag_or_func_name : Lexer
sys_tag_or_func_name = is '$' <+> validChar <+> many validChar
  where
    validChar : Lexer
    validChar = alphaNum <|> is '_' <|> is '$'

wildcard_eq : Lexer
wildcard_eq = exact "==?"

wildcard_ne : Lexer
wildcard_ne = exact "!=?"

xnor : Lexer
xnor = exact "~^" <|> exact "^~"

xor : Lexer
xor = exact "^"

xor_assignment_op : Lexer
xor_assignment_op = exact "^="

private
toIdentifier : String -> LexicalToken
toIdentifier ident = let ident' = trim ident in
                     if isPrefixOf "\\" ident'
                        then IDENTIFIER True $ substr 1 (length ident') ident'
                        else IDENTIFIER False ident'

runLexer : String -> (List (TokenData LexicalToken), (Int, Int, String))
runLexer input = lex tokenMap input where
  tokenMap =
    [ (comment, COMMENT)
    , (compiler_directive, COMPILER_DIRECTIVE)
    , (keyword, KEYWORD)
    , (sys_tag_or_func_name, SYS_TASK_OR_FUNC_NAME)
    , (identifier, toIdentifier)
    , (ashift_left_assignment_op, const ASHIFT_LEFT_ASSIGNMENT_OP)
    , (ashift_right_assignment_op, const ASHIFT_RIGHT_ASSIGNMENT_OP)
    , (lshift_left_assignment_op, const LSHIFT_LEFT_ASSIGNMENT_OP)
    , (lshift_right_assignment_op, const LSHIFT_RIGHT_ASSIGNMENT_OP)
    , (ashift_left, const ASHIFT_LEFT)
    , (ashift_right, const ASHIFT_RIGHT)
    , (lshift_left, const LSHIFT_LEFT)
    , (lshift_right, const LSHIFT_RIGHT)
    , (equivalence, const EQUIVALENCE)
    , (implication, const IMPLICATION)
    , (dec, const DEC)
    , (inc, const INC)
    , (xor_assignment_op, const XOR_ASSIGNMENT_OP)
    , (or_assignment_op, const OR_ASSIGNMENT_OP)
    , (and_assignment_op, const AND_ASSIGNMENT_OP)
    , (modulus_assignment_op, const MODULUS_ASSIGNMENT_OP)
    , (div_assignment_op, const DIV_ASSIGNMENT_OP)
    , (mul_assignment_op, const MUL_ASSIGNMENT_OP)
    , (sub_assignment_op, const SUB_ASSIGNMENT_OP)
    , (add_assignment_op, const ADD_ASSIGNMENT_OP)
    , (case_eq, const CASE_EQ)
    , (case_ne, const CASE_NE)
    , (wildcard_eq, const WILDCARD_EQ)
    , (wildcard_ne, const WILDCARD_NE)
    , (logical_eq, const LOGICAL_EQ)
    , (logical_ne, const LOGICAL_NE)
    , (logical_and, const LOGICAL_AND)
    , (logical_or, const LOGICAL_OR)
    , (power, const POWER)
    , (lte, const LTE)
    , (gte, const GTE)
    , (nand, const NAND)
    , (nor, const NOR)
    , (xnor, XNOR)
    , (bitwise_and, const BITWISE_AND)
    , (bitwise_or, const BITWISE_OR)
    , (bitwise_not, const BITWISE_NOT)
    , (xor, const XOR)
    , (div, const DIV)
    , (mult, const MULT)
    , (minus, const MINUS)
    , (plus, const PLUS)
    , (modulus, const MODULUS)
    , (logical_not, const LOGICAL_NOT)
    , (assignment_op, const ASSIGNMENT_OP)
    , (conditional_op, const COND_OP)
    , (colon, const COLON)
    , (lt, const LT)
    , (gt, const GT)
    , (dot, const DOT)
    , (lbrace, const LBRACE)
    , (rbrace, const RBRACE)
    , (lbracket, const LBRACKET)
    , (rbracket, const RBRACKET)
    , (lparens, const LPARENS)
    , (rparens, const RPARENS)
    , (dollar, const DOLLAR)
    , (whitespace, WHITESPACE)
    ]

-------------------------------------------------------------------------------
-- Type Class Implementations
-------------------------------------------------------------------------------

implementation Eq LexicalToken where
  ADD_ASSIGNMENT_OP == ADD_ASSIGNMENT_OP = True
  AND_ASSIGNMENT_OP == AND_ASSIGNMENT_OP = True
  ASHIFT_LEFT == ASHIFT_LEFT = True
  ASHIFT_LEFT_ASSIGNMENT_OP == ASHIFT_LEFT_ASSIGNMENT_OP = True
  ASHIFT_RIGHT == ASHIFT_RIGHT = True
  ASHIFT_RIGHT_ASSIGNMENT_OP == ASHIFT_RIGHT_ASSIGNMENT_OP = True
  ASSIGNMENT_OP == ASSIGNMENT_OP = True
  BITWISE_AND == BITWISE_AND = True
  BITWISE_OR == BITWISE_OR = True
  BITWISE_NOT == BITWISE_NOT = True
  CASE_EQ == CASE_EQ = True
  CASE_NE == CASE_NE = True
  COLON == COLON = True
  (COMMENT c1) == (COMMENT c2) = c1 == c2
  (COMPILER_DIRECTIVE cd1) == (COMPILER_DIRECTIVE cd2) = cd1 == cd2
  COND_OP == COND_OP = True
  DEC == DEC = True
  DIV == DIV = True
  DIV_ASSIGNMENT_OP == DIV_ASSIGNMENT_OP = True
  DOLLAR == DOLLAR = True
  DOT == DOT = True
  EQUIVALENCE == EQUIVALENCE = True
  GT == GT = True
  GTE == GTE = True
  (IDENTIFIER _ ident1) == (IDENTIFIER _ ident2) = ident1 == ident2
  IMPLICATION == IMPLICATION = True
  INC == INC = True
  (KEYWORD kw1) == (KEYWORD kw2) = kw1 == kw2
  LOGICAL_AND == LOGICAL_AND = True
  LOGICAL_EQ == LOGICAL_EQ = True
  LOGICAL_NE == LOGICAL_NE = True
  LOGICAL_NOT == LOGICAL_NOT = True
  LOGICAL_OR == LOGICAL_OR = True
  LBRACE == LBRACE = True
  LBRACKET == LBRACKET = True
  LPARENS == LPARENS = True
  LSHIFT_LEFT == LSHIFT_LEFT = True
  LSHIFT_LEFT_ASSIGNMENT_OP == LSHIFT_LEFT_ASSIGNMENT_OP = True
  LSHIFT_RIGHT == LSHIFT_RIGHT = True
  LSHIFT_RIGHT_ASSIGNMENT_OP == LSHIFT_RIGHT_ASSIGNMENT_OP = True
  LT == LT = True
  LTE == LTE = True
  MINUS == MINUS = True
  MODULUS == MODULUS = True
  MODULUS_ASSIGNMENT_OP == MODULUS_ASSIGNMENT_OP = True
  MUL_ASSIGNMENT_OP == MUL_ASSIGNMENT_OP = True
  MULT == MULT = True
  NAND == NAND = True
  NOR == NOR = True
  OR_ASSIGNMENT_OP == OR_ASSIGNMENT_OP = True
  PLUS == PLUS = True
  POWER == POWER = True
  RBRACE == RBRACE = True
  RBRACKET == RBRACKET = True
  RPARENS == RPARENS = True
  SUB_ASSIGNMENT_OP == SUB_ASSIGNMENT_OP = True
  (SYS_TASK_OR_FUNC_NAME s1) == (SYS_TASK_OR_FUNC_NAME s2) = s1 == s2
  (WHITESPACE w1) == (WHITESPACE w2) = w1 == w2
  WILDCARD_EQ == WILCARD_EQ = True
  WILDCARD_NE == WILDCARD_NE = True
  (XNOR _) == (XNOR _) = True
  XOR == XOR = True
  XOR_ASSIGNMENT_OP == XOR_ASSIGNMENT_OP = True
  _ == _ = False

implementation Show LexicalToken where
  show ADD_ASSIGNMENT_OP = "+="
  show AND_ASSIGNMENT_OP = "&="
  show ASHIFT_LEFT = "<<<"
  show ASHIFT_LEFT_ASSIGNMENT_OP = "<<<="
  show ASHIFT_RIGHT = ">>>"
  show ASHIFT_RIGHT_ASSIGNMENT_OP = ">>>="
  show ASSIGNMENT_OP = "="
  show BITWISE_AND = "&"
  show BITWISE_OR = "|"
  show BITWISE_NOT = "~"
  show CASE_EQ = "==="
  show CASE_NE = "!=="
  show COLON = ":"
  show (COMMENT c) = c
  show (COMPILER_DIRECTIVE cd) = cd
  show COND_OP = "?"
  show DEC = "--"
  show DIV = "/"
  show DIV_ASSIGNMENT_OP = "/="
  show DOLLAR = "$"
  show DOT = "."
  show EQUIVALENCE = "<->"
  show GT = ">"
  show GTE = ">="
  show (IDENTIFIER True ident) = "\\" ++ ident ++ " "
  show (IDENTIFIER False ident) = ident
  show IMPLICATION = "->"
  show INC = "--"
  show (KEYWORD kw) = kw
  show LOGICAL_AND = "&&"
  show LOGICAL_EQ = "=="
  show LOGICAL_NE = "!="
  show LOGICAL_NOT = "!"
  show LOGICAL_OR = "||"
  show LBRACE = "{"
  show LBRACKET = "["
  show LPARENS = "("
  show LSHIFT_LEFT = "<<"
  show LSHIFT_LEFT_ASSIGNMENT_OP = "<<="
  show LSHIFT_RIGHT = ">>"
  show LSHIFT_RIGHT_ASSIGNMENT_OP = ">>="
  show LT = "<"
  show LTE = "<="
  show MINUS = "-"
  show MODULUS = "%"
  show MODULUS_ASSIGNMENT_OP = "%="
  show MUL_ASSIGNMENT_OP = "*="
  show MULT = "*"
  show NAND = "~&"
  show NOR = "~|"
  show OR_ASSIGNMENT_OP = "|="
  show PLUS = "+"
  show POWER = "**"
  show RBRACE = "}"
  show RBRACKET = "]"
  show RPARENS = ")"
  show SUB_ASSIGNMENT_OP = "-="
  show (SYS_TASK_OR_FUNC_NAME sid) = sid
  show (WHITESPACE w) = w
  show WILDCARD_EQ = "==?"
  show WILDCARD_NE = "!=?"
  show (XNOR op) = op
  show XOR = "^"
  show XOR_ASSIGNMENT_OP = "^="