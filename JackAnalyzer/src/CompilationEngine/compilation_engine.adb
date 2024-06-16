with Compilation_Engine;
with Token_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling;
package body Compilation_Engine is

   procedure Initialize (This : in out CompilationEngine; InputFile : in File_Ptr; InputFileName : in String) is
      xmlFile : File_Type;
   begin
      This.xmlHandler.Initialize (xmlFileName => InputFileName & ".xml");
      This.Tokenizer.Initialize (SourceFile     => InputFile,
                                 xmlFileName    => InputFileName & "T.xml");
      This.Tokenizer.Advance;
   end Initialize;
   
   
   procedure Close (This : in out CompilationEngine) is
   begin
      This.Tokenizer.Close;
      This.xmlHandler.Close;
   end Close;

   
   procedure CompileClass (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      Keyword1 : constant Keyword_Array := (1 => K_CLASS);
      Symbol1  : constant Symbol_Array  := ("{");
      Symbol2  : constant Symbol_Array  := ("}");
   begin
      This.xmlHandler.writeElement (ElementType => "class");
      This.xmlHandler.advanceScope;
      
      This.processKeyword (Keywords => Keyword1);
      This.processIdentifier;
      This.processSymbol (Symbols => Symbol1);
      while This.Tokenizer.TokenType = KEYWORD and then 
        This.Tokenizer.KeyWord in K_STATIC | K_FIELD loop
         This.CompileClassVarDec;
      end loop;
      while This.Tokenizer.TokenType = KEYWORD and then 
        This.Tokenizer.KeyWord in K_CONSTRUCTOR | K_FUNCTION | K_METHOD loop
         This.CompileSubroutine;
      end loop;
      This.processSymbol (Symbols => Symbol2);      
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "class", IsCloser => True);
   end CompileClass;

   
   procedure CompileClassVarDec (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      Keyword1 : constant Keyword_Array := (K_STATIC, K_FIELD);
      Keyword2 : constant Keyword_Array := (K_INT, K_CHAR, K_BOOLEAN);
      Symbol1  : constant Symbol_Array  := (",");
      Symbol2  : constant Symbol_Array  := (";");
   begin
      This.xmlHandler.writeElement (ElementType => "classVarDec");
      This.xmlHandler.advanceScope;
      
      This.processKeyword (Keywords => Keyword1);
      if This.Tokenizer.TokenType = KEYWORD then
         This.processKeyword (Keywords => Keyword2);
      else
         This.processIdentifier;
      end if;
      this.processIdentifier;
      while This.Tokenizer.TokenType = SYMBOL and then This.Tokenizer.Symbol = ',' loop
         This.processSymbol (Symbols => Symbol1);
         This.processIdentifier;
      end loop;
      This.processSymbol (Symbols => Symbol2);
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "classVarDec", IsCloser => True);
   end CompileClassVarDec;

   
   procedure CompileSubroutine (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      Keyword1 : constant Keyword_Array := (K_CONSTRUCTOR, K_FUNCTION, K_METHOD);
      Keyword2 : constant Keyword_Array := (K_INT, K_CHAR, K_BOOLEAN, K_VOID);
      Symbol1  : constant Symbol_Array  := ("(");
      Symbol2  : constant Symbol_Array  := (")");
      Symbol3  : constant Symbol_Array  := ("{");
      Symbol4  : constant Symbol_Array  := ("}");
   begin
      This.xmlHandler.writeElement (ElementType => "subroutineDec");
      This.xmlHandler.advanceScope;
      
      -- (constructor | function | method)
      This.processKeyword (Keywords => Keyword1);
      -- void | type
      if This.Tokenizer.TokenType = KEYWORD then 
         This.processKeyword (Keywords => Keyword2);
      else
         This.processIdentifier;
      end if;
      -- subroutineName
      This.processIdentifier;
      -- '('
      This.processSymbol (Symbols => Symbol1);
      -- parametersList
      This.CompileParameterList;
      -- ')'
      This.processSymbol (Symbols => Symbol2);
      -- subroutineBody
      This.xmlHandler.writeElement (ElementType => "subroutineBody");
      This.xmlHandler.advanceScope;
      
      -- '{'
      This.processSymbol (Symbols => Symbol3);
      -- varDec*
      while This.Tokenizer.TokenType = KEYWORD and then This.Tokenizer.KeyWord = K_VAR loop
         This.CompileVarDec;
      end loop;
      -- statements
      This.CompileStatements;
      -- '}'
      This.processSymbol (Symbols => Symbol4);
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "subroutineBody", IsCloser => True);
      
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "subroutineDec", IsCloser => True);
   end CompileSubroutine;

   
   procedure CompileParameterList (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      Keyword1 : constant Keyword_Array := (K_INT, K_CHAR, K_BOOLEAN);
      Symbol1  : constant Symbol_Array  := (",");
   begin
      This.xmlHandler.writeElement (ElementType => "parameterList");
      This.xmlHandler.advanceScope;
      
      -- if parameter list is not empty
      if This.Tokenizer.TokenType = IDENTIFIER or else
        (This.Tokenizer.TokenType = KEYWORD and then
         This.Tokenizer.KeyWord in K_INT | K_CHAR | K_BOOLEAN) then
         -- type
         if This.Tokenizer.TokenType = KEYWORD then 
            This.processKeyword (Keywords => Keyword1);
         else
            This.processIdentifier;
         end if;
         -- varName - identifier
         This.processIdentifier;
         -- (',' TYPE varName)*
         while This.Tokenizer.TokenType = SYMBOL and then This.Tokenizer.Symbol = ',' loop
            This.processSymbol (Symbols => Symbol1);
            -- type
            if This.Tokenizer.TokenType = KEYWORD then 
               This.processKeyword (Keywords => Keyword1);
            else
               This.processIdentifier;
            end if;
            -- varName - identifier
            This.processIdentifier;
         end loop;
      end if;
      
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "parameterList", IsCloser => True);
   end CompileParameterList;

   
   procedure CompileVarDec (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      Keyword1 : constant Keyword_Array := (1 => K_VAR);
      Keyword2 : constant Keyword_Array := (K_INT, K_CHAR, K_BOOLEAN);
      Symbol1  : constant Symbol_Array  := (",");
      Symbol2  : constant Symbol_Array  := (";");
   begin
      This.xmlHandler.writeElement (ElementType => "varDec");
      This.xmlHandler.advanceScope;
      
      -- 'var'      
      This.processKeyword (Keywords => Keyword1);
      -- type
         if This.Tokenizer.TokenType = KEYWORD then 
            This.processKeyword (Keywords => Keyword2);
         else
            This.processIdentifier;
         end if;
      -- varName - identifier
      This.processIdentifier;
      -- (',' varName)*
      while This.Tokenizer.TokenType = SYMBOL and then This.Tokenizer.Symbol = ',' loop
         This.processSymbol (Symbols => Symbol1);
         This.processIdentifier;
      end loop;
      -- ;
      This.processSymbol (Symbols => Symbol2);
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "varDec", IsCloser => True);
   end CompileVarDec;

   
   procedure CompileStatements (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      LetKeyword : constant Keyword_Array := (1 => K_LET);
      IfKeyword : constant Keyword_Array := (1 => K_IF);
      WhileKeyword : constant Keyword_Array := (1 => K_WHILE);
      DoKeyword : constant Keyword_Array := (1 => K_DO);
      ReturnKeyword : constant Keyword_Array := (1 => K_RETURN);
   begin
      This.xmlHandler.writeElement (ElementType => "statements");
      This.xmlHandler.advanceScope;
      
      -- statement*
      while This.Tokenizer.TokenType = KEYWORD and then 
        This.Tokenizer.KeyWord in K_LET | K_IF | K_WHILE | K_DO | K_RETURN
      loop
         case This.Tokenizer.KeyWord is
         when K_LET =>
            This.CompileLet;
         when K_IF =>
            This.CompileIf;
            
         when K_WHILE =>
            This.CompileWhile;
            
         when K_DO =>
            This.CompileDo;
            
         when K_RETURN =>
            This.CompileReturn;
         when others =>
            This.CompileDo; -- dummy function to araise error
         end case;
      end loop;
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "statements", IsCloser => True);
   end CompileStatements;

   
   procedure CompileDo (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      DoKeyword : constant Keyword_Array := (1 => K_DO);
      Symbol1   : constant Symbol_Array  := (";");
   begin
      This.xmlHandler.writeElement (ElementType => "doStatement");
      This.xmlHandler.advanceScope;
      
      -- 'do'
      This.processKeyword (Keywords => DoKeyword);
      -- subroutine call
      This.CompileSubroutineCall;
      -- ';'
      This.processSymbol (Symbols => Symbol1);
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "doStatement", IsCloser => True);
   end CompileDo;

   
   procedure CompileLet (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      LetKeyword : constant Keyword_Array := (1 => K_LET);
      DotComma    : constant Symbol_Array  := (";");
      LBar       : constant Symbol_Array  := ("[");
      RBar       : constant Symbol_Array  := ("]");
      Equal      : constant Symbol_Array  := ("=");
   begin
      This.xmlHandler.writeElement (ElementType => "letStatement");
      This.xmlHandler.advanceScope;
      
      
      -- 'let'
      This.processKeyword (LetKeyword);
      -- varName - identifier
      This.processIdentifier;
      -- ('[' expression ']')?
      if This.Tokenizer.TokenType = SYMBOL and then This.Tokenizer.Symbol = '[' then
         -- '['
         This.processSymbol (Symbols => LBar);
         -- expression
         This.CompileExpression;
         -- ']'
         This.processSymbol (Symbols => RBar);
      end if;
      -- '='
      This.processSymbol (Symbols => Equal);
      -- expression
      This.CompileExpression;
      This.processSymbol (Symbols => DotComma);
      
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "letStatement", IsCloser => True);
   end CompileLet;

   
   procedure CompileWhile (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      WhileKeyword   : constant Keyword_Array := (1 => K_WHILE);
      LParen       : constant Symbol_Array  := ("(");
      RParen       : constant Symbol_Array  := (")");
      LCurlyParen  : constant Symbol_Array  := ("{");
      RCurlyParen  : constant Symbol_Array  := ("}");
   begin
      This.xmlHandler.writeElement (ElementType => "whileStatement");
      This.xmlHandler.advanceScope;
      
      -- 'while'
      This.processKeyword (whileKeyword);
      -- '('
      This.processSymbol (Symbols => LParen);
      -- expression
      This.CompileExpression;
      -- ')'
      This.processSymbol (Symbols => RParen);
      -- '{'
      This.processSymbol (Symbols => LCurlyParen);
      -- statements
      This.CompileStatements;
      -- '}'
      This.processSymbol (Symbols => RCurlyParen);
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "whileStatement", IsCloser => True);
   end CompileWhile;

   
   procedure CompileReturn (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      ReturnKeyword   : constant Keyword_Array := (1 => K_RETURN);
      DotComma        : constant Symbol_Array  := (";");
   begin
      This.xmlHandler.writeElement (ElementType => "returnStatement");
      This.xmlHandler.advanceScope;
      
      -- 'return'
      This.processKeyword (ReturnKeyword);
      -- expression?
      if This.Tokenizer.TokenType /= SYMBOL or else This.Tokenizer.Symbol /= ';' then
         This.CompileExpression;
      end if;
      -- ';'
      This.processSymbol (Symbols => DotComma);

      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "returnStatement", IsCloser => True);
   end CompileReturn;

   
   procedure CompileIf (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      IfKeyword    : constant Keyword_Array := (1 => K_IF);
      ElseKeyword  : constant Keyword_Array := (1 => K_ELSE);
      LParen       : constant Symbol_Array  := ("(");
      RParen       : constant Symbol_Array  := (")");
      LCurlyParen  : constant Symbol_Array  := ("{");
      RCurlyParen  : constant Symbol_Array  := ("}");
   begin
      This.xmlHandler.writeElement (ElementType => "ifStatement");
      This.xmlHandler.advanceScope;
      
      -- 'if'
      This.processKeyword (IfKeyword);
      -- '('
      This.processSymbol (Symbols => LParen);
      -- expression
      This.CompileExpression;
      -- ')'
      This.processSymbol (Symbols => RParen);
      -- '{'
      This.processSymbol (Symbols => LCurlyParen);
      -- statements
      This.CompileStatements;
      -- '}'
      This.processSymbol (Symbols => RCurlyParen);
      -- 'else'?
      if This.Tokenizer.TokenType = KEYWORD and then This.Tokenizer.KeyWord = K_ELSE then
         -- 'else'
         This.processKeyword (ElseKeyword);
         -- '{'
         This.processSymbol (Symbols => LCurlyParen);
         -- statements
         This.CompileStatements;
         -- '}'
         This.processSymbol (Symbols => RCurlyParen);
      end if;
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "ifStatement", IsCloser => True);
   end CompileIf;

   
   procedure CompileExpression (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      Op : constant Symbol_Array  := ('+', '-', '*', '/', '&', '|', '<', '>', '=');
   begin
      This.xmlHandler.writeElement (ElementType => "expression");
      This.xmlHandler.advanceScope;
      
      -- term
      This.CompileTerm;
      -- (op term)*
      while This.Tokenizer.TokenType = SYMBOL and then
        This.Tokenizer.Symbol in '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
      loop
         -- op
         This.processSymbol (Symbols => Op);
         -- term
         This.CompileTerm;
      end loop;
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "expression", IsCloser => True);
   end CompileExpression;

   
   procedure CompileTerm (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      KeywordConstants   : constant Keyword_Array := (K_TRUE, K_FALSE, K_NULL, K_THIS);
      UnaryOp            : constant Symbol_Array  := ('-', '~');
      LParen             : constant Symbol_Array  := ("(");
      RParen             : constant Symbol_Array  := (")");
      LBar               : constant Symbol_Array  := ("[");
      RBar               : constant Symbol_Array  := ("]");
   begin
      This.xmlHandler.writeElement (ElementType => "term");
      This.xmlHandler.advanceScope;
      
      case This.Tokenizer.TokenType is
      when KEYWORD => -- Keyword constant
         This.processKeyword (Keywords => KeywordConstants);
      when INT_CONST => -- integerConstant
         This.processIntegerConst;
      when STRING_CONST => -- stringConstant
         This.processStringConst;
      when IDENTIFIER => -- varName, varName[expression], subroutineCall
         This.processIdentifier;
         if This.Tokenizer.TokenType = SYMBOL then
            if This.Tokenizer.Symbol = '[' then
               This.processSymbol (Symbols => LBar);
               This.CompileExpression;
               This.processSymbol (Symbols => RBar);
            elsif This.Tokenizer.Symbol = '(' or else This.Tokenizer.Symbol = '.' then
               This.CompileSubroutineCall (WasSubroutineNameTokenized => True);
            end if;
         end if;
      when SYMBOL => -- unaryOp term, '(' expression ')'
         if This.Tokenizer.Symbol in '-' | '~' then -- unaryOp term
            This.processSymbol (Symbols => UnaryOp);
            This.CompileTerm;
         else -- '(' expression ')'
            This.processSymbol (Symbols => LParen);
            This.CompileExpression;
            This.processSymbol (Symbols => RParen);
         end if;
      when others =>
         This.processIdentifier; -- dummy function to araise error
      end case;
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "term", IsCloser => True);
   end CompileTerm;

   
   procedure CompileSubroutineCall (This : in out CompilationEngine; WasSubroutineNameTokenized : in Boolean := False) is
      use keyword_types;
      use Token_Types;
      Keyword1 : constant Keyword_Array := (K_INT, K_CHAR, K_BOOLEAN);
      LParen   : constant Symbol_Array  := ("(");
      RParen   : constant Symbol_Array  := (")");
      Dot      : constant Symbol_Array  := (".");
      Comma    : constant Symbol_Array  := (",");
   begin
      -- subroutineName | className | varName - identifier
      if not WasSubroutineNameTokenized then
         This.processIdentifier;
      end if;
      if This.Tokenizer.TokenType = Symbol and then This.Tokenizer.Symbol = '.' then
         -- '.'
         This.processSymbol (Symbols => Dot);
         -- subroutineName - identifier
         This.processIdentifier;
      end if;
      -- '('
      This.processSymbol (Symbols => LParen);
      -- expressionList
      This.xmlHandler.writeElement (ElementType => "expressionList");
      This.xmlHandler.advanceScope;
      
      if This.Tokenizer.TokenType /= Symbol or else This.Tokenizer.Symbol /= ')' then
         -- expression
         This.CompileExpression;
         -- (',' expression)*
         while This.Tokenizer.TokenType = Symbol and then This.Tokenizer.Symbol = ',' loop
            This.processSymbol (Symbols => Comma);
            This.CompileExpression;
         end loop;
      end if;
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "expressionList", IsCloser => True);
      -- ')'
      This.processSymbol (Symbols => RParen);
   end CompileSubroutineCall;
   
   
   procedure processKeyword (This     : in out CompilationEngine;
                             Keywords : in Keyword_Array) is
      use Token_Types; use keyword_types;
      KeywordName : Unbounded_String;
      ErrorExpected : Unbounded_String;
      
      -- is the keyword token in the array
      function Keyword_In_Array (Element : KeywordType) return Boolean is
         Found : Boolean := False;
      begin
         for K of Keywords loop
            if K = Element then
               Found := True;
               exit;
            end if;
         end loop;
         return Found;
      end Keyword_In_Array;
   begin
      
      if This.Tokenizer.TokenType = Token_Types.KEYWORD and then Keyword_In_Array (This.Tokenizer.KeyWord) then
         KeywordName := To_Unbounded_String (Ada.Characters.Handling.To_Lower (This.Tokenizer.KeyWord'Image));
         Unbounded_Slice (Source => KeywordName,
                          Target => KeywordName,
                          Low    => To_String (KeywordName)'First + 2,
                          High   => To_String (KeywordName)'Last);
         This.xmlHandler.writeTerminal (ElementType => "keyword",
                                        Value       => To_String (KeywordName));
      else
         for K of Keywords loop
            ErrorExpected := ErrorExpected & " | " & K'Image;
         end loop;
         This.handleSyntaxError (Expected => "KEYWORD '" & To_String(ErrorExpected) & "'");
      end if;
      This.Tokenizer.Advance;
   end processKeyword;
   
   
   procedure processSymbol (This    : in out CompilationEngine;
                            Symbols : in Symbol_Array) is
      use Token_Types;
      ErrorExpected : Unbounded_String;
      CurrentSymbol : Unbounded_String;
      -- is the symbol in the given symbols array
      function Symbol_In_Array (Element : Character) return Boolean is
         Found : Boolean := False;
      begin
         for S of Symbols loop
            if S = Element then
               Found := True;
               exit;
            end if;
         end loop;
         return Found;
      end Symbol_In_Array;
      
   begin
      if This.Tokenizer.TokenType = Token_Types.SYMBOL and then Symbol_In_Array (This.Tokenizer.Symbol) then
         CurrentSymbol := To_Unbounded_String ("") & This.Tokenizer.Symbol;
         if CurrentSymbol = ">" then
            CurrentSymbol := To_Unbounded_String ("&gt;");
         elsif CurrentSymbol = "<" then
            CurrentSymbol := To_Unbounded_String ("&lt;");
         elsif CurrentSymbol = "" & '"' then
            CurrentSymbol := To_Unbounded_String ("&quot;");
         elsif CurrentSymbol = "&" then
            CurrentSymbol := To_Unbounded_String ("&amp;");
         end if;
         This.xmlHandler.writeTerminal (ElementType => "symbol",
                                        Value       => To_String (CurrentSymbol));
      else
         for S of Symbols loop
            ErrorExpected := ErrorExpected & " | " & S;
         end loop;
         This.handleSyntaxError (Expected => "SYMBOL '" & To_String (ErrorExpected) & "'");
      end if;
      This.Tokenizer.Advance;
   end processSymbol;
   
   
   procedure processIdentifier (This : in out CompilationEngine) is
      use Token_Types;
   begin
      if This.Tokenizer.TokenType = IDENTIFIER then
         This.xmlHandler.writeTerminal (ElementType => "identifier",
                                        Value       => This.Tokenizer.Identifier);
      else
         This.handleSyntaxError (Expected => "identifier");
      end if;
      This.Tokenizer.Advance;
   end processIdentifier;
   
   
   procedure processIntegerConst (This : in out CompilationEngine) is
      use Token_Types;
      use Ada.Strings;
      IntegerString : Unbounded_String := To_Unbounded_String (Integer'Image (This.Tokenizer.IntVal));
   begin
      if This.Tokenizer.TokenType = INT_CONST then
         Trim (Source => IntegerString,
               Side   => Both);
         This.xmlHandler.writeTerminal (ElementType => "integerConstant",
                                        Value       =>  To_String (IntegerString));
      else
         This.handleSyntaxError (Expected => "integer constant");
      end if;
      This.Tokenizer.Advance;
   end processIntegerConst;
   
   
   procedure processStringConst (This : in out CompilationEngine) is
      use Token_Types;
   begin
      if This.Tokenizer.TokenType = STRING_CONST then
         This.xmlHandler.writeTerminal (ElementType => "stringConstant",
                                        Value       => This.Tokenizer.StringVal);
      else
         This.handleSyntaxError (Expected => "string constant");
      end if;
      This.Tokenizer.Advance;
   end processStringConst;
   
   
   procedure handleSyntaxError (This : in out CompilationEngine; Expected : String) is
   begin
      Put_Line ("SYNTAX ERROR: expected token [" & Expected & "] got [" & Token_Types.TokenType'Image (This.Tokenizer.TokenType) & "]");
      This.xmlHandler.writeTerminal (ElementType => "ERROR",
                                     Value       => "expected " & Expected);
   end handleSyntaxError;

end Compilation_Engine;
