with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Command_Types; use Command_Types;
package Code_Writer is
   
   -- The CodeWriter type represents an object transalting VM commands to HACK commands.
   type CodeWriter is tagged limited private;

   -- Initialize the CodeWriter object with a given output .asm file.
   procedure Initialize (This : out CodeWriter; FileName : in Unbounded_String);

   -- Set the VM file for the CodeWriter object.
   procedure setFileName (This : out CodeWriter; FileName : in Unbounded_String);

   -- Translate an arithmetic command to HACK and write it to .asm file.
   procedure writeArithmetic (This : in out CodeWriter; Command : in String);

   -- Translate a push or pop command to HACK and write it to .asm file.
   procedure WritePushPop (This : in out CodeWriter; CommandType : in Command_Type; segment : in String; index : in Natural);

   -- Close the .asm file associated with the CodeWriter object.
   procedure Close (This : out CodeWriter);
   
   TrueTabel : constant String := "IF_TRUE";
   EndLabel : constant String := "IF_FALSE";

private
   
   -- The private part of the CodeWriter type, containing file information and file name.
   type CodeWriter is tagged limited record
      File       : File_Type; -- output .asm file handle
      FileName   : Unbounded_String; -- The name of the vm currently translated.
      LabelIndex : Natural := 0; -- current label usable index
   end record;
   
   -- Analyze a VM segment and return the appropriate assembly code.
   function segment_analyzer (This : in CodeWriter; segment : in String; index : in Natural) return String;
   
   -- returns current label for jumps if their statement is true
   function currentTrueLabel (This : in CodeWriter) return String;

   -- returns current end label for jumps
   function currentFalseLable (This : in CodeWriter) return String;
   
   -- advances the current label index
   procedure advanceLabel (This : in out CodeWriter);
   
end Code_Writer;
