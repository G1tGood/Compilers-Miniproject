with Ada.Text_IO; use Ada.Text_IO;
with Jack_Tokenizer; use Jack_Tokenizer;
with Xml_Handler;
with keyword_types; use keyword_types;
with Symbol_Table;
with VM_Writer;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  This package provides a Compilation Engine for compiling Jack language source code into VM code.
--  The Compilation Engine uses a tokenizer to process the input source code and writes the 
--  generated VM code to the specified output file.

package Compilation_Engine is

   --  A type representing the Compilation Engine.
   type CompilationEngine is tagged limited private;
   
   --| Initializes the Compilation Engine with the specified input and output files.
   --| @param This The Compilation Engine instance.
   --| @param InputFile The name of the Jack source file to be compiled.
   --| @param InputFileName the name of the input file (without file type [.jack, .exe, etc.])
   procedure Initialize (This : in out CompilationEngine; InputFile : File_Ptr; InputFileName : in String);
   
   --| Closes the Compilation Engine.
   --| @param This The Compilation Engine instance.
   procedure Close (This : in out CompilationEngine);

   --| Compiles a complete class.
   --| @param This The Compilation Engine instance.
   procedure CompileClass (This : in out CompilationEngine);

   --| Compiles class variable declarations.
   --| @param This The Compilation Engine instance.
   procedure CompileClassVarDec (This : in out CompilationEngine);

   --| Compiles a complete subroutine (method/function/constructor).
   --| @param This The Compilation Engine instance.
   procedure CompileSubroutine (This : in out CompilationEngine);

   --| Compiles a (possibly empty) parameter list, not including the enclosing "()".
   --| @param This The Compilation Engine instance.
   procedure CompileParameterList (This : in out CompilationEngine);

   --| Compiles a single variable declaration.
   --| @param This The Compilation Engine instance.
   procedure CompileVarDec (This : in out CompilationEngine);

   --| Compiles a sequence of statements, not including the enclosing "{}".
   --| @param This The Compilation Engine instance.
   procedure CompileStatements (This : in out CompilationEngine);

   --| Compiles a do statement.
   --| @param This The Compilation Engine instance.
   procedure CompileDo (This : in out CompilationEngine);

   --| Compiles a let statement.
   --| @param This The Compilation Engine instance.
   procedure CompileLet (This : in out CompilationEngine);

   --| Compiles a while statement.
   --| @param This The Compilation Engine instance.
   procedure CompileWhile (This : in out CompilationEngine);

   --| Compiles a return statement.
   --| @param This The Compilation Engine instance.
   procedure CompileReturn (This : in out CompilationEngine);

   --| Compiles an if statement, possibly including an else clause.
   --| @param This The Compilation Engine instance.
   procedure CompileIf (This : in out CompilationEngine);

   --| Compiles an expression.
   --| @param This The Compilation Engine instance.
   procedure CompileExpression (This : in out CompilationEngine);

   --| Compiles a term. This method is called recursively.
   --| @param This The Compilation Engine instance.
   procedure CompileTerm (This : in out CompilationEngine);

private

   --|  The private type representing the Compilation Engine.
   type CompilationEngine is tagged limited record
      Tokenizer : JackTokenizer;  -- The tokenizer for the input source code.
      xmlHandler : Xml_Handler.XmlHandler;  -- handler of the xml file
      symbolTable : Symbol_Table.SymbolTable;
      vmWriter    : VM_Writer.VMWriter;
      parsedClassName : Unbounded_String;
      whileIndex      : Natural := 0;
      ifIndex         : Natural := 0;
   end record;
   
   whileExpLabel : constant String := "_VM_While_Exp";
   whileEndLabel : constant String := "_VM_While_End";
   IfEndLabel   : constant String := "_VM_If_End";
   IfFalseLabel  : constant String := "_VM_If_False";
   
   --| array types for passing multiple keywords
   type Keyword_Array is array (Positive range <>) of KeywordType;
   --| array types for passing multiple symbols
   type Symbol_Array is array (Positive range <>) of Character;
   
   --| Advances the while label index.
   --| @param This the compilation engine instance
   procedure advanceWhileIndex (This : in out CompilationEngine);

   --| Advances the if label index.
   --| @param This the compilation engine instance
   procedure advanceIfIndex (This : in out CompilationEngine);

   --| Retrieves the current "If True" label.
   --| @param This the compilation engine instance
   --| @return The "If True" label string
   function getIfEndLabel (This : in out CompilationEngine; index : Natural) return String;

   --| Retrieves the current "If False" label.
   --| @param This the compilation engine instance
   --| @return The "If False" label string
   function getIfFalseLabel (This : in out CompilationEngine; index : Natural) return String;

   --| Retrieves the current "While Expression" label.
   --| @param This the compilation engine instance
   --| @return The "While Expression" label string
   function getWhileExpLabel (This : in out CompilationEngine; index : Natural) return String;

   --| Retrieves the current "While End" label.
   --| @param This the compilation engine instance
   --| @return The "While End" label string
   function getWhileEndLabel (This : in out CompilationEngine; index : Natural) return String;
   
   --| Retrieves the current If label index.
   --| @param This the compilation engine instance
   --| @return The current If label index
   function getIfIndex (This : in out CompilationEngine) return Natural;
   
   --| Retrieves the current While label index.
   --| @param This the compilation engine instance
   --| @return The current While label index
   function getWhileIndex (This : in out CompilationEngine) return Natural;
   
   --| writes keyword constant to vm output file
   --| @param This the compilation engine instance;
   --| @param Keyword the keyword to write
   procedure writeKeywordConstant (This : in out CompilationEngine; Keyword : in KeywordType);
   
   --| writes string constant to vm output file
   --| @param This the compilation engine instance;
   --| @param StringConst the string constant to write
   procedure writeStringConstant (This : in out CompilationEngine; StringConst : in String);
   
   --| writes push variable identifier to vm output file
   --| @param This the compilation engine instance;
   --| @param name name of the variable to write
   procedure writePushVariable (This : in out CompilationEngine; name : in String);
   
   --| writes pop variable identifier to vm output file
   --| @param This the compilation engine instance;
   --| @param name name of the variable to write
   procedure writePopVariable (This : in out CompilationEngine; name : in String);
   
   --| writes operation to vm output file
   --| @param This the compilation engine instance;
   --| @param Op the operation character
   procedure writeOp (This : in out CompilationEngine; Op : in Character);
   
   --| writes unary operation to vm output file
   --| @param This the compilation engine instance;
   --| @param UnaryOp the operation character
   procedure writeUnaryOp (This : in out CompilationEngine; UnaryOp : in Character);
  
   --| processes current keyword token
   --| @param This The Compilation Engine instance.
   --| @param Keyword the supposed keyword
   procedure processKeyword (This : in out CompilationEngine; Keywords : in Keyword_Array);
   
   --| processes current symbol token
   --| @param This The Compilation Engine instance.
   --| @param Symbol the supposed symbol
   procedure processSymbol (This : in out CompilationEngine; Symbols : in Symbol_Array);
   
   --| processes current identifier token
   --| @param This The Compilation Engine instance.
   procedure processIdentifier (This : in out CompilationEngine);
   
   --| processes current integer constant token
   --| @param This The Compilation Engine instance.
   procedure processIntegerConst (This : in out CompilationEngine);
   
   --| processes current string constant token
   --| @param This The Compilation Engine instance.
   procedure processStringConst (This : in out CompilationEngine);
   
   --| handles syntax errors
   --| @param This The Compilation Engine instance.
   --| @param Expected The expected token
   procedure handleSyntaxError (This : in out CompilationEngine; Expected : String);
   
   --| Compiles a subroutine call
   --| @param This The Compilation Engine instance.
   --| @param IsFunction weather or not the Subroutine is a function (if not, then it's a method)
   --| @param WasSubroutineNameTokenized weather or not the first identifier was already tokenized (and should be again)
   --| @param TokenizedName the tokenized class/subroutine name, if it was already tokenized
   procedure CompileSubroutineCall (This : in out CompilationEngine; IsFunction : in Boolean := True; WasSubroutineNameTokenized : in Boolean := False; TokenizedName : in String := "");
   
end Compilation_Engine;
