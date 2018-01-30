module SimIt.SystemVerilog.LexerTest

import SimIt.SystemVerilog.Lexer
import Specdris.Spec
import Text.Lexer

shouldLexAs : String -> List LexicalToken -> SpecResult
shouldLexAs input = 
  (===) (map tok $ fst (runLexer input))

shouldNotLexAs : String -> List LexicalToken -> SpecResult
shouldNotLexAs input =
  (/==) (map tok $ fst (runLexer input))

testKeyword : String -> SpecResult
testKeyword kw = shouldLexAs kw [KEYWORD kw]

allShouldSucceed : List SpecResult -> SpecResult
allShouldSucceed [] = Success
allShouldSucceed (x :: xs) =
  case x of
    Success => allShouldSucceed xs
    result => result

testCase : IO ()
testCase
  = do spec $ do
        describe "SimIt.Verilog.Lexer" $ do
          describe "whitespace" $ do
            it "should parse space" $ do
              shouldLexAs " " [(WHITESPACE " ")]

            it "should parse tab" $ do
              shouldLexAs "\t" [(WHITESPACE "\t")]
            
            it "should parse newline" $ do
              shouldLexAs "\n" [(WHITESPACE "\n")]
            
            it "should parse form feed" $ do
              shouldLexAs "\r" [(WHITESPACE "\r")]

          describe "comment" $ do
            it "should parse a line comment" $ do
              let c = "// line comment\n"
              shouldLexAs c [(COMMENT c)]

            it "should parse a block comment" $ do
              let c = "/* A block comment */"
              shouldLexAs c [(COMMENT c)]

            it "should ignore line comment inside block comment" $ do
              let c = "/* // line comment inside block comment */"
              shouldLexAs c [(COMMENT c)]

            it "should not parse block comment with nested block comment" $ do
              let c = "/* A block comment /*with nested block comment*/. */"
              shouldNotLexAs c [COMMENT c]

          describe "operators" $ do
            describe "assignment operators" $ do
              describe "arithmetic shift assignment operators" $ do
                it "should parse left shift assignment operator" $ do
                  shouldLexAs "<<<=" [ASHIFT_LEFT_ASSIGNMENT_OP]

                it "should parse right shift assignment operator" $ do
                  shouldLexAs ">>>=" [ASHIFT_RIGHT_ASSIGNMENT_OP]

              describe "logical shift assignment operators" $ do
                it "should parse left shift assignment operator" $ do
                  shouldLexAs "<<=" [LSHIFT_LEFT_ASSIGNMENT_OP]

                it "should parse right shift assignment operator" $ do
                  shouldLexAs ">>=" [LSHIFT_RIGHT_ASSIGNMENT_OP]

              describe "bitwise assignment operators" $ do
                it "should parse XOR assignment operator" $ do
                  shouldLexAs "^=" [XOR_ASSIGNMENT_OP]

                it "should parse OR assignment operator" $ do
                  shouldLexAs "|=" [OR_ASSIGNMENT_OP]

                it "should parse AND assignment operator" $ do
                  shouldLexAs "&=" [AND_ASSIGNMENT_OP]

              describe "binary arithmetic assignment operators" $ do
                it "should parse the modulus assignment operator" $ do
                  shouldLexAs "%=" [MODULUS_ASSIGNMENT_OP]
                
                it "should parse the division assignment operator" $ do
                  shouldLexAs "/=" [DIV_ASSIGNMENT_OP]

                it "should parse the multiplication assignment operator" $ do
                  shouldLexAs "*=" [MUL_ASSIGNMENT_OP]

                it "should parse the subtraction assignment operator" $ do
                  shouldLexAs "-=" [SUB_ASSIGNMENT_OP]

                it "should parse the addition assignment operator" $ do
                  shouldLexAs "+=" [ADD_ASSIGNMENT_OP]

                it "should parse the assignment operator" $ do
                  shouldLexAs "=" [ASSIGNMENT_OP]
            
            describe "conditional operators" $ do
              it "should parse the conditional operator" $ do
                shouldLexAs "?" [COND_OP]
              
              it "should parse a colon" $ do
                shouldLexAs ":" [COLON]

            describe "bitwise operators" $ do
              it "should parse the XNOR operator" $ do
                shouldLexAs "~^" [XNOR "~^"]
                shouldLexAs "^~" [XNOR "^~"]

              it "should parse the XOR operator" $ do
                shouldLexAs "^" [XOR]

              it "should parse the bitwise NOR reduction operator" $ do
                shouldLexAs "~|" [NOR]

              it "should parse the bitwise NAND operator" $ do
                shouldLexAs "~&" [NAND]

              it "should parse the bitwise AND operator" $ do
                shouldLexAs "&" [BITWISE_AND]

              it "should parse the bitwise OR operator" $ do
                shouldLexAs "|" [BITWISE_OR]

              it "should parse the bitwise NOT operator" $ do
                shouldLexAs "~" [BITWISE_NOT]
            
            describe "arithmetic operators" $ do
              it "should parse the division operator" $ do
                shouldLexAs "/" [DIV]

              it "should parse the multiplication operator" $ do
                shouldLexAs "*" [MULT]

              it "should parse the subtraction/negate operator" $ do
                shouldLexAs "-" [MINUS]

              it "should parse the plus/positive operator" $ do
                shouldLexAs "+" [PLUS]

              it "should parse the modulus operator" $ do
                shouldLexAs "%" [MODULUS]

              it "should parse the power operator" $ do
                shouldLexAs "**" [POWER]

              it "should parse arithmetic shift left operator" $ do
                shouldLexAs "<<<" [ASHIFT_LEFT]

              it "should parse arithmetic shift right operator" $ do
                shouldLexAs ">>>" [ASHIFT_RIGHT]

              it "should parse decrement operator" $ do
                shouldLexAs "--" [DEC]

              it "should parse increment operator" $ do
                shouldLexAs "++" [INC]

            describe "logical operators" $ do
              it "should parse the logical negation operator" $ do
                shouldLexAs "!" [LOGICAL_NOT]

              it "should parse the case equality operator" $ do
                shouldLexAs "===" [CASE_EQ]

              it "should parse the case inequality operator" $ do
                shouldLexAs "!==" [CASE_NE]

              it "should parse the logical equality operator" $ do
                shouldLexAs "==" [LOGICAL_EQ]

              it "should parse the logical inequality operator" $ do
                shouldLexAs "!=" [LOGICAL_NE]

              it "should parse wildcard equality operator" $ do
                shouldLexAs "==?" [WILDCARD_EQ]

              it "should parse wildcard inequality operator" $ do
                shouldLexAs "!=?" [WILDCARD_NE]
              
              it "should parse logical AND operator" $ do
                shouldLexAs "&&" [LOGICAL_AND]

              it "should parse logical OR operator" $ do
                shouldLexAs "||" [LOGICAL_OR]

              it "should parse less than operator" $ do
                shouldLexAs "<" [LT]
              
              it "should parse less than or equal operator" $ do
                shouldLexAs "<=" [LTE]

              it "should parse greater than operator" $ do
                shouldLexAs ">" [GT]

              it "should parse greater than or equal operator" $ do
                shouldLexAs ">=" [GTE]

              it "should parse logical shift left operator" $ do
                shouldLexAs "<<" [LSHIFT_LEFT]

              it "should parse logical shift right operator" $ do
                shouldLexAs ">>" [LSHIFT_RIGHT]

              it "should parse equivalence operator" $ do
                shouldLexAs "<->" [EQUIVALENCE]

              it "should parse implication operator" $ do
                shouldLexAs "->" [IMPLICATION]
            
            describe "misc operators" $ do
              it "should parse the '.' operator" $ do
                shouldLexAs "." [DOT]

              it "should parse left parens" $ do
                shouldLexAs "(" [LPARENS]

              it "should parse right parens" $ do
                shouldLexAs ")" [RPARENS]

              it "should parse left brace" $ do
                shouldLexAs "{" [LBRACE]

              it "should parse right brace" $ do
                shouldLexAs "}" [RBRACE]

              it "should parse left bracket" $ do
                shouldLexAs "[" [LBRACKET]

              it "should parse right bracket" $ do
                shouldLexAs "]" [RBRACKET]

              it "should parse dollar sign" $ do
                shouldLexAs "$" [DOLLAR]

          describe "identifiers" $ do
            describe "simple identifier" $ do
              it "should parse an identifier beginning with a lowercase letter" $ do
                shouldLexAs "abc123" [IDENTIFIER False "abc123"]

              it "should parse an identifier beginning with an uppercase letter" $ do
                shouldLexAs "Abc123_$" [IDENTIFIER False "Abc123_$"]

              it "should parse an identifier beginning with an underscore" $ do
                let ident = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_$"
                shouldLexAs ident [IDENTIFIER False ident]

              it "should parse a single letter identifier" $ do
                shouldLexAs "a" [IDENTIFIER False "a"]

              it "should parse only underscore as an identifier" $ do
                shouldLexAs "_" [IDENTIFIER False "_"]
            
            describe "escaped identifier" $ do
              it "should parse \\{one or more printable ascii characters} whitespace" $ do
                shouldLexAs "\\cpu3 " [IDENTIFIER True "cpu3"]
                shouldLexAs "\\!some-ident~\t" [IDENTIFIER True "!some-ident~"]
                shouldLexAs "\\123\n" [IDENTIFIER True "123"]

              it "should fail to parse an identifier that does not end in whitespace" $ do
                shouldLexAs "\\abc" []

              it "should fail to parse an identifier that contains non-printable ascii characters" $ do
                shouldLexAs "\\xxx\DEL " []

              it "should fail to parse an identifier with no printable characters between backslash and whitespace" $ do
                shouldLexAs "\\ " []

            describe "system identifier" $ do
              it "should parse a system identifier" $ do
                shouldLexAs "$display" [SYS_TASK_OR_FUNC_NAME "$display"]
                shouldLexAs "$1a" [SYS_TASK_OR_FUNC_NAME "$1a"]
                shouldLexAs "$_A1$" [SYS_TASK_OR_FUNC_NAME "$_A1$"]
                shouldLexAs "$$abc_123" [SYS_TASK_OR_FUNC_NAME "$$abc_123"]
              
          describe "keywords" $ do
            it "should parse each keyword" $ do
              allShouldSucceed $ map testKeyword keywords

          describe "compiler directives" $ do
            it "should parse each compiler directive" $ do
              allShouldSucceed $ map (\directive => shouldLexAs directive [COMPILER_DIRECTIVE directive]) compiler_directives

            it "should fail to parse an invalid compiler directive" $ do
              shouldLexAs "`some_non_existent_compiler_directive" []
export  
specSuite : IO ()
specSuite = do putStrLn "\n  spec:"
               testCase