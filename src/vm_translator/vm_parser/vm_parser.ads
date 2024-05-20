with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Command_Types; use Command_Types;

--  This package is used to parse a VM language file.
--  It provides functionality to create a parser, check for more commands,
--  advance to the next command, and retrieve command details.

package VM_Parser is

   --  A type representing the parser.
   type Parser is tagged limited private;

   --| Initializes the parser for the given VM language file.
   --| @param This The parser instance.
   --| @param fileName The name of the VM file to be parsed.
   procedure Initialize (This : out Parser; fileName : Unbounded_String);

   --| Checks if there are any more unparsed commands in the file.
   --| @param This The parser instance.
   --| @return True if there are more commands to parse, False otherwise.
   function hasMoreCommands (This : Parser) return Boolean;

   --| Advances the parser to the next command in the VM file.
   --| @param This The parser instance.
   procedure advance (This : in out Parser);

   --| Returns the type of the current command.
   --| @param This The parser instance.
   --| @return The CommandType of the current command.
   function commandType (This : Parser) return Command_Type;

   --| Returns the first argument of the current command, or the command itself for arithmetic commands.
   --| @param This The parser instance.
   --| @return The first argument as an Unbounded_String, or the command itself for arithmetic commands.
   function arg1 (This : Parser) return String;

   --| Returns the second argument of the current command.
   --| @param This The parser instance.
   --| @return The second argument as an Integer.
   function arg2 (This : Parser) return Integer;

private

   --|  The private type representing the parser.
   type Parser is tagged limited record
         InputFile : File_Type;  -- the VM file being parsed.
         CompletedParse : Boolean;  -- was the parsed completed (all command in the file were read).
         FirstArg : Unbounded_String; -- first argument of current parsed command, or command itself for arithmetic commands.
         SecondArg : Natural; -- second argument of current parsed command.
         CurrentCommandType : Command_Type; -- type of current parsed command.
      end record;

   --| remove the comment from the read line
   --| @param Line the line to remove the comment from
   procedure removeComment(Line : in out Unbounded_String);

   --| parses a command, extracts first argument, second argumentt and command type
   --| @param CommandLine the command line to parse
   --| @param FirstArg the first argument extracted from the command
   --| @param SecondArg the second argument extracted from the command
   --| @param CurrentCommandType the command type extracted from the command
   procedure parseCommand (CommandLine : in Unbounded_String;
                           FirstArg : Out Unbounded_String;
                           SecondArg : Out Natural;
                           CurrentCommandType : Out Command_Type);

end VM_Parser;
