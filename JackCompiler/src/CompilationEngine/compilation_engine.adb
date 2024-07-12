with Compilation_Engine;
with Token_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with identifier_kinds; use identifier_kinds;      
with Vm_Segments; use Vm_Segments;
with vm_arithmentic_commands; use vm_arithmentic_commands;
with ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Compilation_Engine is

   procedure Initialize (This : in out CompilationEngine; InputFile : in File_Ptr; InputFileName : in String) is
      xmlFile : File_Type;
   begin
      This.xmlHandler.Initialize (xmlFileName => InputFileName & ".xml");
      This.Tokenizer.Initialize (SourceFile     => InputFile,
                                 xmlFileName    => InputFileName & "T.xml");
      This.symbolTable.Initialize;
      This.vmWriter.Initialize (OutputFile => InputFileName & ".vm");
      This.whileIndex := 0;
      This.ifIndex := 0;
      
      This.Tokenizer.Advance;
   end Initialize;
   
   
   procedure Close (This : in out CompilationEngine) is
   begin
      This.Tokenizer.Close;
      This.xmlHandler.Close;
      This.vmWriter.Close;
   end Close;

   
   procedure CompileClass (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      Keyword1 : constant Keyword_Array := (1 => K_CLASS);
      Symbol1  : constant Symbol_Array  := ("{");
      Symbol2  : constant Symbol_Array  := ("}");
   begin
      This.symbolTable.Initialize;
      
      This.xmlHandler.writeElement (ElementType => "class");
      This.xmlHandler.advanceScope;
      
      -- 'class'
      This.processKeyword (Keywords => Keyword1);
      -- className
      This.parsedClassName := To_Unbounded_String (This.Tokenizer.Identifier);
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
      
      -- for identifier entry in symbol table
      itype    : Unbounded_String;
      kind: IdentifierKind;
   begin
      This.xmlHandler.writeElement (ElementType => "classVarDec");
      This.xmlHandler.advanceScope;
      
      -- symbol table, determine kind
      if This.Tokenizer.KeyWord = K_STATIC then
         kind := I_STATIC;
      elsif This.Tokenizer.KeyWord = K_FIELD then
         kind := I_FIELD;
      end if;
      
      -- static | field
      This.processKeyword (Keywords => Keyword1);
      -- type
      if This.Tokenizer.TokenType = KEYWORD then -- int | char | boolean
         -- symbol table insert type (case int, char, boolean)
         case This.Tokenizer.KeyWord is
            when K_INT => itype     := To_Unbounded_String ("int");
            when K_CHAR => itype    := To_Unbounded_String ("char");
            when K_BOOLEAN => itype := To_Unbounded_String ("boolean");
            when others => null;
         end case;
         
         This.processKeyword (Keywords => Keyword2);
      else
         -- symbol table insert type (case ClassName)
         itype := To_Unbounded_String (this.Tokenizer.Identifier);
         This.processIdentifier; -- className
      end if;
      -- varName
      This.symbolTable.define (name  => This.Tokenizer.Identifier,
                               iType => To_String (itype),
                               kind  => kind);
      this.processIdentifier;
      while This.Tokenizer.TokenType = SYMBOL and then This.Tokenizer.Symbol = ',' loop
         This.processSymbol (Symbols => Symbol1);
         This.symbolTable.define (name  => This.Tokenizer.Identifier,
                                  iType => To_String (itype),
                                  kind  => kind);
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
      
      -- for compilition:
      subroutineType : KeywordType;
      subroutineName : Unbounded_String;
   begin
      This.symbolTable.startSubroutine;
      
      This.xmlHandler.writeElement (ElementType => "subroutineDec");
      This.xmlHandler.advanceScope;
      
      -- (constructor | function | method)
      subroutineType := This.Tokenizer.KeyWord;
      if subroutineType = K_METHOD then
         This.symbolTable.define (name  => "this",
                                  iType => To_String (This.parsedClassName),
                                  kind  => I_ARG);
      end if;
      This.processKeyword (Keywords => Keyword1);
      
      -- void | type
      if This.Tokenizer.TokenType = KEYWORD then 
         This.processKeyword (Keywords => Keyword2);
      else
         This.processIdentifier;
      end if;
      -- subroutineName
      subroutineName := This.parsedClassName & "." & To_Unbounded_String (This.Tokenizer.Identifier);
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
      
      -- compiling the function declaration
      This.vmWriter.WriteFunction (Name    => To_String (subroutineName),
                                   nLocals => This.symbolTable.varCount (kind => I_VAR));
      This.vmWriter.advanceScope;
      -- if constructor, construct the object
      if subroutineType = K_CONSTRUCTOR then
         This.vmWriter.WritePush (Segment => S_CONST,
                                  Index   => This.symbolTable.varCount(kind => I_FIELD));
         This.vmWriter.WriteCall (Name  => "Memory.alloc",
                                  nArgs => 1);
         This.vmWriter.WritePop (Segment => S_POINTER,
                                 Index   => 0);
      elsif subroutineType = K_METHOD then
         This.vmWriter.WritePush (Segment => S_ARG,
                                  Index   => 0);
         This.vmWriter.WritePop (Segment => S_POINTER,
                                 Index   => 0);
      end if;
      -- statements
      This.CompileStatements;
      -- '}'
      This.processSymbol (Symbols => Symbol4);
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "subroutineBody", IsCloser => True);
      
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "subroutineDec", IsCloser => True);
      
      This.vmWriter.withdrawScope;
   end CompileSubroutine;

   
   procedure CompileParameterList (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      Keyword1 : constant Keyword_Array := (K_INT, K_CHAR, K_BOOLEAN);
      Symbol1  : constant Symbol_Array  := (",");
      
      -- for identifier entry in symbol table
      itype    : Unbounded_String;
   begin
      This.xmlHandler.writeElement (ElementType => "parameterList");
      This.xmlHandler.advanceScope;
      
      -- if parameter list is not empty
      if This.Tokenizer.TokenType = IDENTIFIER or else
        (This.Tokenizer.TokenType = KEYWORD and then
         This.Tokenizer.KeyWord in K_INT | K_CHAR | K_BOOLEAN) then
         -- type
         if This.Tokenizer.TokenType = KEYWORD then
            -- symbol table insert type (case int, char, boolean)
            case This.Tokenizer.KeyWord is
               when K_INT => itype     := To_Unbounded_String ("int");
               when K_CHAR => itype    := To_Unbounded_String ("char");
               when K_BOOLEAN => itype := To_Unbounded_String ("boolean");
               when others => null;
            end case;
            This.processKeyword (Keywords => Keyword1);
         else
            itype := To_Unbounded_String (This.Tokenizer.Identifier);
            This.processIdentifier;
         end if;
         -- varName - identifier
         -- insert to symbol table
         This.symbolTable.define (name  => This.Tokenizer.Identifier,
                                  iType => To_String (itype),
                                  kind  => I_ARG);
         This.processIdentifier;
         -- (',' TYPE varName)*
         while This.Tokenizer.TokenType = SYMBOL and then This.Tokenizer.Symbol = ',' loop
            This.processSymbol (Symbols => Symbol1);
            -- type
            if This.Tokenizer.TokenType = KEYWORD then 
               -- symbol table insert type (case int, char, boolean)
               case This.Tokenizer.KeyWord is
               when K_INT => itype     := To_Unbounded_String ("int");
               when K_CHAR => itype    := To_Unbounded_String ("char");
               when K_BOOLEAN => itype := To_Unbounded_String ("boolean");
               when others => null;
               end case;
               This.processKeyword (Keywords => Keyword1);
            else
               itype := To_Unbounded_String (This.Tokenizer.Identifier);
               This.processIdentifier;
            end if;
            -- insert to symbol table
            This.symbolTable.define (name  => This.Tokenizer.Identifier,
                                     iType => To_String (itype),
                                     kind  => I_ARG);
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
      
      -- for identifier entry in symbol table
      itype    : Unbounded_String;
   begin
      This.xmlHandler.writeElement (ElementType => "varDec");
      This.xmlHandler.advanceScope;
      
      -- 'var'      
      This.processKeyword (Keywords => Keyword1);
      -- type
      if This.Tokenizer.TokenType = KEYWORD then 
         case This.Tokenizer.KeyWord is
            when K_INT => itype     := To_Unbounded_String ("int");
            when K_CHAR => itype    := To_Unbounded_String ("char");
            when K_BOOLEAN => itype := To_Unbounded_String ("boolean");
            when others => null;
         end case;
         This.processKeyword (Keywords => Keyword2);
      else
         itype := To_Unbounded_String (This.Tokenizer.Identifier);
         This.processIdentifier;
      end if;
      -- varName - identifier
      This.symbolTable.define (name  => This.Tokenizer.Identifier, -- symbol table insert
                               iType => To_String (itype),
                               kind  => I_VAR);
      This.processIdentifier;
      -- (',' varName)*
      while This.Tokenizer.TokenType = SYMBOL and then This.Tokenizer.Symbol = ',' loop
         This.processSymbol (Symbols => Symbol1);
         This.symbolTable.define (name  => This.Tokenizer.Identifier, -- symbol table insert
                                  iType => To_String (itype),
                                  kind  => I_VAR);
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
      This.vmWriter.WritePop (Segment => S_TEMP,
                              Index   => 0);
      
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
      Equal       : constant Symbol_Array  := ("=");
      
      -- for compile
      isArrayAssignment : Boolean := False;
      varName : Unbounded_String;
   begin
      This.xmlHandler.writeElement (ElementType => "letStatement");
      This.xmlHandler.advanceScope;
      
      
      -- 'let'
      This.processKeyword (LetKeyword);
      -- varName - identifier
      varName := To_Unbounded_String (This.Tokenizer.Identifier);
      This.processIdentifier;
      -- ('[' expression ']')?
      if This.Tokenizer.TokenType = SYMBOL and then This.Tokenizer.Symbol = '[' then
         -- '['
         This.processSymbol (Symbols => LBar);
         -- expression
         This.CompileExpression;
         -- ']'
         This.processSymbol (Symbols => RBar);
         This.writePushVariable(name => To_String (varName));
         This.vmWriter.WriteArithmetic (Command => C_ADD);
         isArrayAssignment := True;
      end if;
      -- '='
      This.processSymbol (Symbols => Equal);
      -- expression
      This.CompileExpression;
      This.processSymbol (Symbols => DotComma);
      if isArrayAssignment then
         This.vmWriter.WritePop (Segment => S_TEMP,
                                 Index   => 0);
         This.vmWriter.WritePop (Segment => S_POINTER,
                                 Index   => 1);
         This.vmWriter.WritePush (Segment => S_TEMP,
                                  Index   => 0);
         This.vmWriter.WritePop (Segment => S_THAT,
                                 Index   => 0);
      else
         This.writePopVariable  (name => To_String (varName));
      end if;
      
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "letStatement", IsCloser => True);
   end CompileLet;

   
   procedure CompileWhile (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      WhileKeyword   : constant Keyword_Array := (1 => K_WHILE);
      LParen         : constant Symbol_Array  := ("(");
      RParen         : constant Symbol_Array  := (")");
      LCurlyParen    : constant Symbol_Array  := ("{");
      RCurlyParen    : constant Symbol_Array  := ("}");
      
      -- for compile
      labelIndex : Natural := This.getWhileIndex;
   begin
      This.xmlHandler.writeElement (ElementType => "whileStatement");
      This.xmlHandler.advanceScope;
      
      This.advanceWhileIndex;
            
      -- 'while'
      This.processKeyword (whileKeyword);
      
      This.vmWriter.WriteLabel (Label => This.getWhileExpLabel (index => labelIndex));         -- Label while_exp
      -- '('
      This.processSymbol (Symbols => LParen);
      -- expression
      This.CompileExpression;                                                                  -- compile expression
      -- ')'
      This.processSymbol (Symbols => RParen);
      
      This.vmWriter.WriteArithmetic (Command => C_NOT);                                        -- not
      This.vmWriter.WriteIf (Label => This.getWhileEndLabel (index => labelIndex));            -- if-goto while_end
      -- '{'
      This.processSymbol (Symbols => LCurlyParen);
      -- statements
      This.CompileStatements;                                                                  -- compile statements
      -- '}'
      This.processSymbol (Symbols => RCurlyParen);
      This.vmWriter.WriteGoto (Label => This.getWhileExpLabel (index => labelIndex));          -- goto while_exp
      This.vmWriter.WriteLabel (Label => This.getWhileEndLabel (index => labelIndex));         -- Label while_end

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
      else
         This.vmWriter.WritePush (Segment => S_CONST,
                                  Index   => 0);
      end if;
      This.vmWriter.WriteReturn;
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
      
      -- for compile
      labelIndex : Natural := This.getIfIndex;
   begin
      This.xmlHandler.writeElement (ElementType => "ifStatement");
      This.xmlHandler.advanceScope;
      
      This.advanceIfIndex;
      
      -- 'if'
      This.processKeyword (IfKeyword);
      -- '('
      This.processSymbol (Symbols => LParen);
      -- expression
      This.CompileExpression;
      -- ')'
      This.processSymbol (Symbols => RParen);
      This.vmWriter.WriteArithmetic (Command => C_NOT);
      This.vmWriter.WriteIf (Label => This.getIfFalseLabel (index => labelIndex));
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
         This.vmWriter.WriteGoto (Label => This.getIfEndLabel (index => labelIndex));
         This.vmWriter.WriteLabel (Label => This.getIfFalseLabel (index => labelIndex));
         -- '{'
         This.processSymbol (Symbols => LCurlyParen);
         -- statements
         This.CompileStatements;
         -- '}'
         This.processSymbol (Symbols => RCurlyParen);
         This.vmWriter.WriteLabel (Label => This.getIfEndLabel (index => labelIndex));
      else
         This.vmWriter.WriteLabel (Label => This.getIfFalseLabel (index => labelIndex));
      end if;
      
      This.xmlHandler.withdrawScope;
      This.xmlHandler.writeElement (ElementType => "ifStatement", IsCloser => True);
   end CompileIf;

   
   procedure CompileExpression (This : in out CompilationEngine) is
      use keyword_types;
      use Token_Types;
      Op : constant Symbol_Array  := ('+', '-', '*', '/', '&', '|', '<', '>', '=');
      
      -- for compiling
      operationSymbol : Character;
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
         operationSymbol := This.Tokenizer.Symbol;
         This.processSymbol (Symbols => Op);
         -- term
         This.CompileTerm;
         
         This.writeOp (Op => operationSymbol);
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
      
      -- for compiling
      UnaryOpVar         : Character; -- the unary operation
      className          : Unbounded_String; -- class of the function/method to call (if there is such call)
      IsFunction         : Boolean; -- in case of soubroutine call, weather a function was called
   begin
      This.xmlHandler.writeElement (ElementType => "term");
      This.xmlHandler.advanceScope;
      
      case This.Tokenizer.TokenType is
      when KEYWORD => -- Keyword constant
         This.writeKeywordConstant (This.Tokenizer.KeyWord);
         This.processKeyword (Keywords => KeywordConstants);
      when INT_CONST => -- integerConstant
         This.vmWriter.WritePush (Segment => S_CONST,
                                  Index   => This.Tokenizer.IntVal);
         This.processIntegerConst;
      when STRING_CONST => -- stringConstant
         This.writeStringConstant (This.Tokenizer.StringVal);
         This.processStringConst;
      when IDENTIFIER => -- varName, varName[expression], subroutineCall
         if this.symbolTable.kindOf (name => This.Tokenizer.Identifier) /= identifier_kinds.I_NONE then -- a variable
            This.writePushVariable (This.Tokenizer.Identifier);
            IsFunction := False;
            className := To_Unbounded_String (This.symbolTable.typeOf(name => This.Tokenizer.Identifier));
         else
            IsFunction := True;
            className := To_Unbounded_String (This.Tokenizer.Identifier);
         end if;
         This.processIdentifier;
         if This.Tokenizer.TokenType = SYMBOL then
            if This.Tokenizer.Symbol = '[' then
               This.processSymbol (Symbols => LBar);
               This.CompileExpression;
               This.processSymbol (Symbols => RBar);
               This.vmWriter.WriteArithmetic (Command => C_ADD);
               This.vmWriter.WritePop (Segment => S_POINTER,
                                       Index   => 1);
               This.vmWriter.WritePush (Segment => S_THAT,
                                        Index   => 0);
               
            elsif This.Tokenizer.Symbol = '(' or else This.Tokenizer.Symbol = '.' then
               This.CompileSubroutineCall (IsFunction                 => IsFunction, -- it's a method
                                           WasSubroutineNameTokenized => True,
                                           TokenizedName              => To_String (className));
            end if;
         end if;
      when SYMBOL => -- unaryOp term, '(' expression ')'
         if This.Tokenizer.Symbol in '-' | '~' then -- unaryOp term
            UnaryOpVar := This.Tokenizer.Symbol;
            This.processSymbol (Symbols => UnaryOp);
            This.CompileTerm;
            This.writeUnaryOp (UnaryOp => UnaryOpVar);
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

   -----------------------
   -- advanceWhileIndex --
   -----------------------

   procedure advanceWhileIndex (This : in out CompilationEngine) is
   begin
      This.whileIndex := This.whileIndex + 1;
   end advanceWhileIndex;

   --------------------
   -- advanceIfIndex --
   --------------------

   procedure advanceIfIndex (This : in out CompilationEngine) is
   begin
      This.ifIndex := This.ifIndex + 1;
   end advanceIfIndex;

   ----------------
   -- getIfIndex --
   ----------------

   function getIfIndex (This : in out CompilationEngine) return Natural is
   begin
      return This.ifIndex;
   end getIfIndex;

   -------------------
   -- getWhileIndex --
   -------------------

   function getWhileIndex (This : in out CompilationEngine) return Natural is
   begin
      return This.whileIndex;
   end getWhileIndex;

   --------------------
   -- getIfTrueLabel --
   --------------------

   function getIfEndLabel (This : in out CompilationEngine; index : Natural) return String is
      indexString : String := Trim (Source => index'Image,
                                    Side   => Both);
   begin
      return To_String (This.parsedClassName & "." & IfEndLabel & indexString);
   end getIfEndLabel;

   ---------------------
   -- getIfFalseLabel --
   ---------------------

   function getIfFalseLabel (This : in out CompilationEngine; index : Natural) return String is
   indexString : String := Trim (Source => index'Image,
                                    Side   => Both);
   begin
      return To_String (This.parsedClassName & "." & IfFalseLabel & indexString);
   end getIfFalseLabel;

   ----------------------
   -- getWhileExpLabel --
   ----------------------

   function getWhileExpLabel (This : in out CompilationEngine; index : Natural) return String is
   indexString : String := Trim (Source => index'Image,
                                 Side   => Both);
   begin
      return To_String (This.parsedClassName & "." & whileExpLabel & indexString);
   end getWhileExpLabel;

   ----------------------
   -- getWhileEndLabel --
   ----------------------

   function getWhileEndLabel (This : in out CompilationEngine; index : Natural) return String is
   indexString : String := Trim (Source => index'Image,
                                 Side   => Both);
   begin
      return To_String (This.parsedClassName & "." & whileEndLabel & indexString);
   end getWhileEndLabel;

   --------------------------
   -- writeKeywordConstant --
   --------------------------

   procedure writeKeywordConstant
     (This : in out CompilationEngine; Keyword : in KeywordType)
   is
   begin
      case KeyWord is
         when K_NULL | K_FALSE =>
            This.vmWriter.WritePush (Segment => S_CONST,
                                     Index   => 0);
         when K_TRUE =>
            This.vmWriter.WritePush (Segment => S_CONST,
                                     Index   => 0);
            This.vmWriter.WriteArithmetic (Command => C_NOT);
         when K_THIS =>
            This.vmWriter.WritePush (Segment => S_POINTER,
                                     Index   => 0);
            --  This.vmWriter.banana (name => "this");
         when others =>
            null;
      end case;
   end writeKeywordConstant;

   -------------------------
   -- writeStringConstant --
   -------------------------

   procedure writeStringConstant
     (This : in out CompilationEngine; StringConst : in String)
   is
   begin
      -- creating a new string
      This.vmWriter.WritePush (Segment => S_CONST,
                               Index   => StringConst'Length);
      This.vmWriter.WriteCall (Name  => "String.new",
                               nArgs => 1);
      
      -- making the string by appending char by char
      for I in StringConst'Range loop
         This.vmWriter.WritePush (Segment => S_CONST,
                                  Index   => Character'Pos (StringConst (I)));
         This.vmWriter.WriteCall (Name  => "String.appendChar",
                                  nArgs => 2);
      end loop;
   end writeStringConstant;

   -------------------
   -- writeVariable --
   -------------------

   procedure writePushVariable (This : in out CompilationEngine; name : in String)
   is
      function KindToSegment (kind : IdentifierKind) return VMSegment is
      begin
         case kind is
            when I_FIELD => return S_THIS;
            when I_VAR => return S_LOCAL;
            when I_ARG => return S_ARG;
            when I_STATIC => return S_STATIC;
            when others => 
               raise Constraint_Error with (name & " not a variable identifier; not in symbol table");
         end case;
      end KindToSegment;
   begin
      if This.symbolTable.kindOf (name => name) = identifier_kinds.I_NONE then
         raise Constraint_Error with (name & " not a variable identifier; not in symbol table");
      end if;
      
      This.vmWriter.WritePush (Segment => KindToSegment(This.symbolTable.kindOf (name => name)),
                               Index   => This.symbolTable.indexOf (name => name));
   end writePushVariable;

   ----------------------
   -- writePopVariable --
   ----------------------

   procedure writePopVariable
     (This : in out CompilationEngine; name : in String)
   is
   function KindToSegment (kind : IdentifierKind) return VMSegment is
      begin
         case kind is
            when I_FIELD => return S_THIS;
            when I_VAR => return S_LOCAL;
            when I_ARG => return S_ARG;
            when I_STATIC => return S_STATIC;
            when others => 
               raise Constraint_Error with (name & " not a variable identifier; not in symbol table");
         end case;
      end KindToSegment;
   begin
      if This.symbolTable.kindOf (name => name) = identifier_kinds.I_NONE then
         raise Constraint_Error with (name & " not a variable identifier; not in symbol table");
      end if;
      
      This.vmWriter.WritePop (Segment => KindToSegment(This.symbolTable.kindOf (name => name)),
                               Index   => This.symbolTable.indexOf (name => name));
   end writePopVariable;

   -------------
   -- writeOp --
   -------------

   procedure writeOp (This : in out CompilationEngine; Op : in Character) is
      command : ArithmeticCommand;
   begin
      if op = '*' then
         This.vmWriter.WriteCall (Name  => "Math.multiply",
                                  nArgs => 2);
      elsif op = '/' then
         This.vmWriter.WriteCall (Name  => "Math.divide",
                                  nArgs => 2);
      else
         command := vm_arithmentic_commands.toArithmeticCommand (commandChar => Op,
                                                                 IsUnary     => False);
         This.vmWriter.WriteArithmetic (Command => command);
      end if;
   end writeOp;

   ------------------
   -- writeUnaryOp --
   ------------------

   procedure writeUnaryOp
     (This : in out CompilationEngine; UnaryOp : in Character)
   is
      command : ArithmeticCommand := vm_arithmentic_commands.toArithmeticCommand (commandChar => UnaryOp,
                                                                                  IsUnary     => True);
   begin
      This.vmWriter.WriteArithmetic (Command => command);
   end writeUnaryOp;

   
   procedure CompileSubroutineCall 
     (This : in out CompilationEngine; IsFunction : in Boolean := True; WasSubroutineNameTokenized : in Boolean := False; TokenizedName : in String := "") is
      use keyword_types;
      use Token_Types;
      Keyword1 : constant Keyword_Array := (K_INT, K_CHAR, K_BOOLEAN);
      LParen   : constant Symbol_Array  := ("(");
      RParen   : constant Symbol_Array  := (")");
      Dot      : constant Symbol_Array  := (".");
      Comma    : constant Symbol_Array  := (",");
      
      -- for compiling
      SubroutineFullName : Unbounded_String := To_Unbounded_String (TokenizedName);
      argsAmount : Natural := (if IsFunction then 0 else 1);
   begin
      -- subroutineName | className | varName - identifier
      if not WasSubroutineNameTokenized then
         if This.symbolTable.kindOf (name => This.Tokenizer.Identifier) /= I_NONE then -- its a variable name
            -- get the variable's class (to call ClassName.function)
            SubroutineFullName := To_Unbounded_String (This.symbolTable.typeOf (name => This.Tokenizer.Identifier));
            This.writePushVariable (name => This.Tokenizer.Identifier);
            argsAmount := 1;
         else -- its a subroutine/class name         
            -- get the subroutine name or class name (ClassName.function)
            SubroutineFullName := To_Unbounded_String (This.Tokenizer.Identifier);
            argsAmount := 0;
         end if;
         This.processIdentifier;
      end if;
      if This.Tokenizer.TokenType = Symbol and then This.Tokenizer.Symbol = '.' then
         -- '.'
         This.processSymbol (Symbols => Dot);
         -- subroutineName - identifier
         SubroutineFullName := SubroutineFullName & "." & This.Tokenizer.Identifier;
         This.processIdentifier;
      else -- its SubroutineName(expressionList), figure out subroutine name's class, push this
         SubroutineFullName := This.parsedClassName & "." & SubroutineFullName;
         This.writeKeywordConstant (K_THIS);
         argsAmount := 1;
      end if;
      -- '('
      This.processSymbol (Symbols => LParen);
      -- expressionList
      This.xmlHandler.writeElement (ElementType => "expressionList");
      This.xmlHandler.advanceScope;
      
      if This.Tokenizer.TokenType /= Symbol or else This.Tokenizer.Symbol /= ')' then
         -- expression
         This.CompileExpression;
         argsAmount := argsAmount + 1;
         -- (',' expression)*
         while This.Tokenizer.TokenType = Symbol and then This.Tokenizer.Symbol = ',' loop
            This.processSymbol (Symbols => Comma);
            This.CompileExpression;
            argsAmount := argsAmount + 1;
         end loop;
      end if;
      This.vmWriter.WriteCall (Name  => To_String (SubroutineFullName),
                               nArgs => argsAmount);
      
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
      name : String := This.Tokenizer.Identifier;
   begin
      if This.Tokenizer.TokenType = IDENTIFIER then
         if This.symbolTable.kindOf (name) /= I_NONE then
            This.xmlHandler.writeElement (ElementType => "identifier");
            This.xmlHandler.advanceScope;
         
            This.xmlHandler.writeTerminal (ElementType => "name",
                                           Value       => name);
            This.xmlHandler.writeTerminal (ElementType => "type",
                                           Value       => This.symbolTable.typeOf (name));
            This.xmlHandler.writeTerminal (ElementType => "kind",
                                           Value       => IdentifierKind'Image (This.symbolTable.kindOf (name)));
            This.xmlHandler.writeTerminal (ElementType => "index",
                                           Value       => Natural'Image (This.symbolTable.indexOf (name)));
         
            This.xmlHandler.withdrawScope;
            This.xmlHandler.writeElement (ElementType => "identifier",
                                          IsCloser    => True);
         else
            This.xmlHandler.writeTerminal (ElementType => "identifier",
                                           Value       => name);
         end if;
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
