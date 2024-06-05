with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Strings;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
use Ada.Strings;
with Ada.Strings.Unbounded;
use Ada.Text_IO;
use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with VM_Parser;
with Code_Writer;
with Command_Types;
with Ada.Command_Line;
with Ada.Directories;

procedure VMTranslator is
   use Ada.Directories;

   Writer       : Code_Writer.CodeWriter;
   Path         : unbounded_string;
   My_Exception : exception;
   Idx          : Integer;
   package CLI renames Ada.Command_Line;

   -- remove extention from file path
   procedure removeExtention (FilePath : in out Unbounded_String) is
   begin
      Idx := Index (Source  => FilePath,
                    Pattern => ".",
                    From    => To_String (FilePath)'Last,
                    Going   => Backward);
      if Idx /= 0 then
         Unbounded_Slice (Source => FilePath,
                          Target => FilePath,
                          Low    => To_String (FilePath)'First,
                          High   => Idx - 1);
      end if;
   end removeExtention;

   -- parse single VM file
   -- @param Path path to t he VM file (without extention)
   -- @param ToInit weather or not to initialize the parse (True for init file in directory parse, False otherwise)
   procedure ParseVm (Path : Unbounded_String; ToInit : Boolean := False) is
      use Command_Types;
      Parser                      : VM_Parser.Parser;
      CommandType                 : Command_Type;
      FileName                    : Unbounded_String := To_Unbounded_String (Base_Name (To_String (Path))); -- only file name, without extention
   begin
      Put_Line ("[PARSING: " & Path & "]");
      Parser.Initialize (fileName => Path & ".vm");
      Writer.setFileName (FileName => FileName);
      if ToInit then
         Writer.writeInit;
         Writer.flushCommands;
      end if;

      Parser.advance;
      while Parser.hasMoreCommands loop
         CommandType := Parser.commandType;
         case CommandType is
         when C_ARITHMETIC => Writer.writeArithmetic (Command => Parser.arg1);

         when C_PUSH => Writer.WritePushPop (CommandType => Command_Types.C_PUSH,
                                             segment     => Parser.arg1,
                                             index       => Parser.arg2);

         when C_POP => Writer.WritePushPop (CommandType => Command_Types.C_POP,
                                            segment     => Parser.arg1,
                                            index       => Parser.arg2);

         when C_LABEL => Writer.writeLabel (Label => Parser.arg1);
         when C_RETURN => Writer.writeReturn;
         when C_IF => Writer.writeIf (Label => Parser.arg1);
         when C_GOTO => Writer.writeGoto (Label => Parser.arg1);
         when C_CALL => Writer.writeCall (FunctionName => Parser.arg1,
                                          NumArgs      => Parser.arg2);
         when C_FUNCTION => Writer.writeFunction (FunctionName => Parser.arg1,
                                                  NumLocals    => Parser.arg2);

         when others => raise My_Exception with "ERROR: no such command type";
         end case;
         Writer.flushCommands;

         Parser.advance;
      end loop;
   end ParseVm;

   -- parse directory with VM files;
   -- @param Path path to the directory
   procedure parseDir (Path : String) is
      use Ada.Directories;

      Dir_Search : Search_Type;
      Dir_Ent        : Directory_Entry_Type;
      InitFile : String := "Sys";
      VmFilePattern: String := "*.vm";
   begin
      Set_Directory (Path);
      ParseVm (Path   => To_Unbounded_String (InitFile),
               ToInit => True);

      Start_Search (Search    => Dir_Search,
                    Directory => Path,
                    Pattern   => VmFilePattern);

      loop
         Get_Next_Entry (Dir_Search, Dir_Ent);
         if Kind (Dir_Ent) = Ordinary_File and then Base_Name (Simple_Name (Dir_Ent)) /= InitFile then
            ParseVm (To_Unbounded_String (Base_Name (Simple_Name (Dir_Ent))));
         end if;

         exit when not More_Entries (Dir_Search);
      end loop;
   end parseDir;

begin
   if CLI.Argument_Count = 0 then
      Put ("please enter path: ");
      Path := Unbounded_IO.Get_Line; -- path to file
   elsif CLI.Argument_Count = 1 then
      Path := To_Unbounded_String (CLI.Argument (1));
   end if;

   if not Exists (To_String (Path)) then
      Put_Line ("file does not exist.");
      return;
   end if;

   Put_Line ("[starting parse of " & Path & "]");

   if Kind (To_String (Path)) = Directory then
      Writer.Initialize (Path & "\" & Simple_Name (Name => To_String (Path)) & ".asm");
      ParseDir (To_String (Path));
   else
      removeExtention (Path);
      Writer.Initialize (Path & ".asm");
      ParseVm (Path);
   end if;

   Put_Line ("[parse of " & Path & " completed]");

end VMTranslator;
