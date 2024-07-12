with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with ada.strings.fixed;
with keyword_types; use keyword_types;
with Token_Types; use Token_Types;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
package body Jack_Tokenizer is
   
   procedure Initialize (This : out JackTokenizer; SourceFile : in File_Ptr; xmlFileName: in String) is
      content                 : Unbounded_String;
      symbols                 : constant String := "{}()[].,;+-*/&|><=~";
      delimeters              : constant String := symbols & " " & ASCII.LF & Character'Val (9) & '"';
      words                   : Strings_Vectors.Vector;
      isComment               : Boolean := False;
      isMultilineComment      : Boolean := False;
      isString                : Boolean := False;
      compiledStringConst     : Unbounded_String;
      Word                    : Unbounded_String;
      I                       : Natural;
      
      function is_numeric (str : String) return Boolean is
         Dummy : Float;
      begin
         Dummy := Float'Value (str);
         return True;
      exception
         when others =>
            return False;
      end is_numeric;
      
      function is_identifier (str : String) return Boolean is
         Valid_Chars : constant Ada.Strings.Maps.Character_Set :=
                         Ada.Strings.Maps.To_Set ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_");
         
      begin
         if To_Set (str) <= Valid_Chars then 
            return True;
         end if;
         return False;
      end is_identifier;
      
      function split_words (str : Unbounded_String) return Strings_Vectors.Vector is

         strs          : Strings_Vectors.Vector;
         word          : Unbounded_String;
         delim         : Unbounded_String;
         delimetersSet : constant Character_Set := To_Set (delimeters);
         F             : Positive;
         L             : Natural;
         I             : Natural := 1;
      begin
         while I in To_String (str)'Range loop
            Find_Token
              (Source  => str,
               Set     => delimetersSet,
               From    => I,
               Test    => Ada.Strings.Outside,
               First   => F,
               Last    => L);

            exit when L = 0;
            
            delim := Unbounded_Slice (Source => str,
                                      Low    => I,
                                      High   => F - 1);
            
            word := Unbounded_Slice (Source => str,
                                     Low    => F,
                                     High   => L);
            
            if delim /= "" then
               for C of To_String (delim) loop
                  strs.Append (New_Item => To_Unbounded_String ("" & C));
               end loop;
            end if;
            strs.Append (word);
            
            I := L + 1;
         end loop;
         
         delim := Unbounded_Slice (Source => str,
                                   Low    => F,
                                   High   => To_String (Str)'Length);
         for C of To_String (delim) loop
            strs.Append (New_Item => To_Unbounded_String ("" & C));
         end loop;
         
         return strs;
      end split_words;
      
   begin      
      This.current_line := To_Unbounded_String ("");
      This.FinishedParse := False;

      -- create xml output file
      Create (This.xmlFile, Out_File, xmlFileName);
      Put_Line (File => This.xmlFile, Item => "<tokens>");
     
      -- read file and split to tokens
      while not End_Of_File (sourceFile.all) loop
         Get_Line (sourceFile.all, Content);
         content := content & ASCII.LF;
         words.Append_Vector (split_words (content));
      end loop;
      
      I := words.First_Index;
      while I <= words.Last_Index loop
         Word := words (i);
         --Put_Line (Word);
         if isComment then
            if Word = "" & ASCII.LF then
               isComment := False;
            end if;
         elsif isMultilineComment then
            if Word = "*" and then words.Last_Index >= i + 1 and then To_String (words (i + 1)) = "/" then
               I := I + 1;
               isMultilineComment := False;
            end if;
         elsif Word = "" & '"' and not isMultilineComment and not isComment then
            if isString then
               Put (File => This.xmlFile, Item => "<stringConstant> ");  
               Put (File => This.xmlFile, Item => compiledStringConst);
               Put_Line (File => This.xmlFile, Item => " </stringConstant>");  
               isString := False;
            else 
               isString := True;
               compiledStringConst := To_Unbounded_String ("");
            end if;
         elsif isString and not isComment and not isMultilineComment then
            compiledStringConst := compiledStringConst & word;
         elsif Word = "/" and then words.Last_Index >= i + 1 and then To_String (words (i + 1)) = "/" then
            I := I + 1;
            isComment := True;
         
         elsif Word = "/" and then words.Last_Index >= i + 1 and then To_String (words (i + 1)) = "*" then
            I := I + 1;
            isMultilineComment := True;
         elsif To_String (Word) in "class" | "method" | "function" | "constructor" | "int" | "boolean" | "char" | "void" | "var" | "static" | "field" | "let" | "do" | "if" | "else" | "while" | "return" | "true" | "false" | "null" | "this" then
            Put (File => This.xmlFile, Item => "<keyword> ");
            Put (File => This.xmlFile, Item => Word);
            Put_Line (File => This.xmlFile, Item => " </keyword>");
         elsif To_Set (To_String (Word)) <= To_Set (symbols) then
            Put (File => This.xmlFile, Item => "<symbol> ");
            if Word = ">" then
               Put (File => This.xmlFile, Item => "&gt;");
            elsif Word = "<" then
               Put (File => This.xmlFile, Item => "&lt;");
            elsif Word = "" & '"' then
               Put (File => This.xmlFile, Item => "&quot;");
            elsif Word = "&" then
               Put (File => This.xmlFile, Item => "&amp;");
            else 
               Put (File => This.xmlFile, Item => Word);
            end if;
            Put_Line (File => This.xmlFile, Item => " </symbol>");   
         elsif is_numeric (To_String (Word)) then
            Put (File => This.xmlFile, Item => "<integerConstant> ");  
            Put (File => This.xmlFile, Item => Word);
            Put_Line (File => This.xmlFile, Item => " </integerConstant>"); 
         elsif not is_numeric (To_String (Head (Source => Word, Count  => 1))) and is_identifier (To_String (Word)) then 
            Put (File => This.xmlFile, Item => "<identifier> ");  
            Put (File => This.xmlFile, Item => Word);
            Put_Line (File => This.xmlFile, Item => " </identifier>");   
         end if;
         I := I + 1;
      end loop;
         
      Put_Line (File => This.xmlFile, Item => "</tokens>");
      Close (This.xmlFile);
      Open (This.xmlFile, In_File, xmlFileName);

      This.current_line := Get_Line (This.xmlFile);
   end Initialize;

   function hasMoreTokens (This : in JackTokenizer) return Boolean is
   begin
      return not This.FinishedParse;
   end hasMoreTokens;
   
   procedure advance (This : in out JackTokenizer) is
      use Token_Types;
   begin
      if not This.HasMoreTokens then
         return;
      elsif End_Of_File (This.xmlFile) then 
         This.current_line := To_Unbounded_String ("");
         This.FinishedParse := True;
      else
         This.current_line := Get_Line (This.xmlFile);
      end if;
      --  if This.tokenType = TOKENS then
      --     This.current_line := To_Unbounded_String ("");
      --  end if;
   end advance;

   function TokenType (This : in out JackTokenizer) return Token_Types.TokenType is
      token_string : String := To_String  (Unbounded_Slice (Source => This.current_line,
                                                            Low    => Index (This.current_line, Ada.Strings.Maps.To_Set ("<"), 1)+1,
                                                            High   => Index (This.current_line, Ada.Strings.Maps.To_Set (">"), 1)-1));
   begin
      if token_string = "keyword" then
         return Token_Types.KEYWORD;
      elsif token_string = "symbol" then
         return Token_Types.SYMBOL;
      elsif token_string = "identifier" then
         return Token_Types.IDENTIFIER;
      elsif token_string = "integerConstant" then
         return Token_Types.INT_CONST;
      elsif token_string = "stringConstant" then
         return Token_Types.STRING_CONST;
      elsif token_string = "error" then
         return Token_Types.ERROR;
      elsif token_string in "tokens" | "/tokens" then
         return Token_Types.TOKENS;
      end if;
      return Token_Types.ERROR;
   end tokenType;

   function keyWord (This : in out JackTokenizer) return KeywordType is
      keyWordString : string := This.getCurrentXmlValue;
   begin
      if keyWordString = "class" then
         return keyword_types.K_CLASS;
      elsif keyWordString = "method" then
         return keyword_types.K_METHOD;
      elsif keyWordString = "function" then
         return keyword_types.K_FUNCTION;
      elsif keyWordString = "constructor" then
         return keyword_types.K_CONSTRUCTOR;
      elsif keyWordString = "int" then
         return keyword_types.K_INT;
      elsif keyWordString = "boolean" then
         return keyword_types.K_BOOLEAN;
      elsif keyWordString = "char" then
         return keyword_types.K_CHAR;
      elsif keyWordString = "void" then
         return keyword_types.K_VOID;
      elsif keyWordString = "var" then
         return keyword_types.K_VAR;
      elsif keyWordString = "static" then
         return keyword_types.K_STATIC;
      elsif keyWordString = "field" then
         return keyword_types.K_FIELD;
      elsif keyWordString = "let" then
         return keyword_types.K_LET;
      elsif keyWordString = "do" then
         return keyword_types.K_DO;
      elsif keyWordString = "if" then
         return keyword_types.K_IF;
      elsif keyWordString = "else" then
         return keyword_types.K_ELSE;
      elsif keyWordString = "while" then
         return keyword_types.K_WHILE;
      elsif keyWordString = "return" then
         return keyword_types.K_RETURN;
      elsif keyWordString = "true" then
         return keyword_types.K_TRUE;
      elsif keyWordString = "false" then
         return keyword_types.K_FALSE;
      elsif keyWordString = "null" then
         return keyword_types.K_NULL;
      elsif keyWordString = "this" then
         return keyword_types.K_THIS;
      end if;
      return K_NULL; -- dummy return
   end keyWord;

   function symbol (This : in out JackTokenizer) return Character is
      symbolString : string := This.getCurrentXmlValue;
   begin
      if symbolString = "&lt;" then
         return '<';
      elsif symbolString = "&gt;" then
         return '>';
      elsif symbolString = "&quot;" then
         return '/';
      elsif symbolString = "&amp;" then
         return '&';
      else 
         return symbolString (1);
      end if;
   end symbol;
   
   function identifier (This : in out JackTokenizer) return String is
   begin
      return This.getCurrentXmlValue;
   end identifier;
   
   function intVal (This : in out JackTokenizer) return Integer is
   begin
      return Integer'Value (This.getCurrentXmlValue);
   end intVal;
   
   function stringVal (This : in out JackTokenizer) return String is
   begin
      return This.getCurrentXmlValue;
   end stringVal;
   
   
   function getCurrentXmlValue (This : in out JackTokenizer) return String is
      StartIndex : Natural;
      EndIndex   : Natural;
      Value      : Unbounded_String;
   begin
      if not This.HasMoreTokens then
         return "";
      end if;
      Trim (Source => This.current_line,
            Side   => Both);
      if This.current_line = "<tokens>" or This.current_line = "</tokens>" then
         return "tokens";
      end if;

      StartIndex := Index (Source => This.current_line,
                           Set    => Ada.Strings.Maps.To_Set (">"),
                           From   => 1) + 2;
      EndIndex := Index (Source => This.current_line,
                         Set    => Ada.Strings.Maps.To_Set ("<"),
                         From   => StartIndex) - 2;
      Value := Unbounded_Slice (Source => This.current_line,
                                Low    => StartIndex,
                                High   => EndIndex);
      return To_String (Value);
   end getCurrentXmlValue;
   
   
   procedure Close (This : out JackTokenizer) is
   begin
      Close (File => This.xmlFile);
   end Close;
   
end Jack_Tokenizer;
