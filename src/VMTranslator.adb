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

procedure VMTranslator is
   Path         : unbounded_string;
   My_Exception : exception;
   FileName     : Unbounded_String; -- file name without full path
   Idx          : Integer;
   package CLI renames Ada.Command_Line;

   procedure ParseVm is
      Parser                      : VM_Parser.Parser;
      Writer                      : Code_Writer.CodeWriter;
      CommandType                 : Command_Types.Command_Type;
   begin
      Parser.Initialize (fileName => Path & ".vm");
      Writer.Initialize (FileName => path & ".asm");
      Writer.setFileName (FileName => Path & ".vm");

      Parser.advance;
      while Parser.hasMoreCommands loop
         CommandType := Parser.commandType;
         case CommandType is
         when Command_Types.C_ARITHMETIC => Writer.writeArithmetic (Command => Parser.arg1);
         when Command_Types.C_PUSH => Writer.WritePushPop (CommandType => Command_Types.C_PUSH,
                                                           segment     => Parser.arg1,
                                                           index       => Parser.arg2);
         when Command_Types.C_POP => Writer.WritePushPop (CommandType => Command_Types.C_POP,
                                                          segment     => Parser.arg1,
                                                          index       => Parser.arg2);
         when others => raise My_Exception with "ERROR: no such command type";
         end case;

         Parser.advance;
      end loop;
   end ParseVm;
begin
   if CLI.Argument_Count = 0 then
      Put ("please enter path: ");
      Path := Unbounded_IO.Get_Line; -- path to file
   elsif CLI.Argument_Count = 1 then
      Path := To_Unbounded_String(CLI.Argument(1));
   end if;

   Idx := Index (Source => Path,
                 Pattern => "\",
                 From => To_String (Path)'Last,
                 Going => Backward);
   if Idx = 0 then
      Put_Line ("bad path: please provide full path");
      return;
   end if;

   Unbounded_Slice (Source => Path,
                    Target => FileName,
                    Low    => Idx + 1,
                    High   => To_String (Path)'Last);

   Put_Line ("currently reading " & Path & ".vm");

   ParseVm;

end VMTranslator;
