with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed;
with Keyword_Types; use Keyword_Types;
with Token_Types; use Token_Types;

package Jack_Tokenizer is
   type File_Ptr is access all File_Type;

   --  Type representing the Jack tokenizer.
   type JackTokenizer is tagged limited private;

   --| Initializes the Jack tokenizer with a given file name.
   --| @param This The Jack tokenizer instance.
   --| @param SourceFile The file to tokenize.
   --| @param SourceFileName The name of the file to tokenize.
   procedure Initialize (This : out JackTokenizer; SourceFile : in File_Ptr; xmlFileName : in String);

   --| Closes the Jack tokenizer.
   --| @param This The Jack tokenizer instance.
   procedure Close (This : out JackTokenizer);

   --| Checks if there are more tokens in the input.
   --| @param This The Jack tokenizer instance.
   --| @return True if there are more tokens, otherwise False.
   function HasMoreTokens (This : in JackTokenizer) return Boolean;

   --| Gets the next token from the input and makes it the current token.
   --| @param This The Jack tokenizer instance.
   procedure Advance (This : in out JackTokenizer);

   --| Returns the type of the current token.
   --| @param This The Jack tokenizer instance.
   --| @return The type of the current token.
   function TokenType (This : in out JackTokenizer) return TokenType;

   --| Returns the keyword which is the current token, if the current token is a keyword.
   --| @param This The Jack tokenizer instance.
   --| @return The keyword which is the current token.
   function KeyWord (This : in out JackTokenizer) return KeywordType;

   --| Returns the character which is the current token, if the current token is a symbol.
   --| @param This The Jack tokenizer instance.
   --| @return The character which is the current token.
   function Symbol (This : in out JackTokenizer) return Character;

   --| Returns the identifier which is the current token, if the current token is an identifier.
   --| @param This The Jack tokenizer instance.
   --| @return The identifier which is the current token.
   function Identifier (This : in out JackTokenizer) return String;

   --| Returns the integer value of the current token, if the current token is an integer constant.
   --| @param This The Jack tokenizer instance.
   --| @return The integer value of the current token.
   function IntVal (This : in out JackTokenizer) return Integer;

   --| Returns the string value of the current token, if the current token is a string constant.
   --| @param This The Jack tokenizer instance.
   --| @return The string value of the current token.
   function StringVal (This : in out JackTokenizer) return String;

private
   package Strings_Vectors is new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Positive,
         Element_Type => Unbounded_String);

   type JackTokenizer is tagged limited record
      xmlFile      : File_Type;  -- The XML file for tokenized output.
      current_line : Unbounded_String;  -- The current line being processed.
      FinishedParse : Boolean; -- wether the parse was finished
   end record;

   --| Returns the string value of the current token, if the current token is a string constant.
   --| @param This The Jack tokenizer instance.
   --| @return the value of the current token.
   function getCurrentXmlValue (This : in out JackTokenizer) return String;

end Jack_Tokenizer;
