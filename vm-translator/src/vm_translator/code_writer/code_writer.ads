with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Command_Types; use Command_Types;
with ada.Containers.Indefinite_Vectors;

package Code_Writer is
   
   -- The CodeWriter type (class) represents an object translating VM commands to HACK commands.
   type CodeWriter is tagged limited private;

   -- Output file initialization: initialize the CodeWriter object with a given output .asm file.
   -- @param FileName path of the output file
   procedure Initialize (This : out CodeWriter; FileName : in Unbounded_String);
   
   -- Input file initialization: set the VM file for the CodeWriter object.
   -- @param FileName path of the input VM file
   procedure setFileName (This : out CodeWriter; FileName : in Unbounded_String);
   
   -- flush the commands in the command container
   -- and write them into the output file
   procedure flushCommands (This : in out CodeWriter);
   
   -- Writes bootstrap code to the output file
   procedure writeInit (This : out CodeWriter);
   
   -- Translate a Label command to HACK and write it to .asm file.
   -- @param Label name of the label
   procedure writeLabel (This : in out CodeWriter; Label : in String);
   
   -- Translate a Label command to HACK and write it to .asm file.
   -- @param Label name of the label to jump to
   procedure writeGoto (This : in out CodeWriter; Label : in String);
   
   -- Translate an if-goto command to HACK and write it to .asm file.
   -- @param Label name of the label to jump to
   procedure writeIf (This : in out CodeWriter; Label : in String);
   
   -- Translate a call command to HACK and write it to .asm file.
   -- @param FunctionName name of the function
   -- @param NumArgs amount of arguments the function has
   procedure writeCall (This : in out CodeWriter; FunctionName : in String; NumArgs : in Natural);
   
   -- Translate a return command to HACK and write it to .asm file. 
   procedure writeReturn (This : in out CodeWriter);
   
   -- Translate a function command to HACK and write it to .asm file.
   -- @param FunctionName name of the function
   -- @param NumArgs amount of arguments the function has
   procedure writeFunction (This : in out CodeWriter; FunctionName : in String; NumLocals : in Natural);
   
   -- Translate an arithmetic command to HACK and write it to .asm file.
   procedure writeArithmetic (This : in out CodeWriter; Command : in String);

   -- Translate a push or pop command to HACK and write it to .asm file.
   procedure WritePushPop (This : in out CodeWriter; CommandType : in Command_Type; segment : in String; index : in Natural);

   -- Close the .asm file associated with the CodeWriter object.
   procedure Close (This : out CodeWriter);

private
   BootstrapInitFunc : Constant String := "Sys.init";
   TrueTabel : constant String := "IF_TRUE";
   EndLabel : constant String := "IF_FALSE";
   
   package Command_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Natural,
        Element_Type => String);
   
   -- The private part of the CodeWriter type, containing file information and file name.
   type CodeWriter is tagged limited record
      File       : File_Type; -- output .asm file handle
      FileName   : Unbounded_String; -- The name of the vm currently translated.
      LabelIndex : Natural := 0; -- current compiler generated label usable index
      retLabelIndex : Natural := 0; -- current return-from-function-call label index (restarts in every function parse)
      commandsVector : Command_Vectors.Vector; -- vector containing the translated commands
      currentlyParsedFunction : Unbounded_String; -- the currently passed function's name
   end record;
   
   -- Analyze a VM segment and return the appropriate assembly code.
   function segment_analyzer (This : in CodeWriter; segment : in String; index : in Natural) return Command_Vectors.Vector;
   
   -- returns current label for jumps if their statement is true
   function currentTrueLabel (This : in CodeWriter) return String;

   -- returns current label for jumps if their statement is false
   function currentFalseLabel (This : in CodeWriter) return String;
   
   -- advances the current compiler label index
   procedure advanceCompLabel (This : in out CodeWriter);
   
   -- advances the current return-from-function-call label index
   procedure advanceRetLabel (This : in out CodeWriter);
   
   -- returns current label for return-from-function-call label index
   function currentRetLabel (This : in CodeWriter) return String;
   
   -- reinitializes (to start index) return-from-function-call label index 
   procedure reinitRetLabel (This : in out CodeWriter);
   
   -- generates the string "(FileName.[given label])"
   -- @ param Label label to create (see general description)
   function genFileLabel (This : in CodeWriter; Label : String) return string;
   
end Code_Writer;
