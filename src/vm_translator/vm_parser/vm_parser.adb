with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Command_Types; use Command_Types;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Containers.Vectors;
with Ada.Strings;

package body VM_Parser is

   --  Creates a new parser for the given VM language file.
   procedure Initialize (This : out Parser; FileName : Unbounded_String) is
   begin
      Open(This.InputFile, In_File, To_String(fileName));
      This.CompletedParse := False;
   end Initialize;

   --  Checks if there are any more unparsed commands in the file.
   function hasMoreCommands (This : Parser) return Boolean is
   begin
      return not This.CompletedParse;
   end hasMoreCommands;

   --  Advances the parser to the next command in the VM file.
   procedure advance (This : in out Parser) is
      Line : Unbounded_String; -- line read from VM file
      FoundCommand : Boolean := False;
   begin
      if This.CompletedParse then
         return;
      end if;

      while not End_Of_File(This.InputFile) loop
         Ada.Text_IO.Unbounded_IO.Get_Line(File => This.InputFile,
                                           Item => Line); -- read line from file
         removeComment(Line); -- remove comment from the command line

         if Line /= "" then -- exit it's a line with a command
            FoundCommand := True;
            exit;
         end if;
      end loop;



      -- parse the command
      if FoundCommand then
         parseCommand (CommandLine        => Line,
                       FirstArg           => This.FirstArg,
                       SecondArg          => This.SecondArg,
                       CurrentCommandType => This.CurrentCommandType);
      else
         This.CompletedParse := True;
         Close(File => This.InputFile);
      end if;

   end advance;

   --  Returns the type of the current command.
   function commandType (This : Parser) return Command_Type is
   begin
      return This.CurrentCommandType;
   end commandType;

   --  Returns the first argument of the current command, or the command itself for arithmetic commands.
   function arg1 (This : Parser) return String is
   begin
      return To_String(This.FirstArg);
   end arg1;

   --  Returns the second argument of the current command.
   function arg2 (This : Parser) return Integer is
   begin
      return This.SecondArg;
   end arg2;

   procedure removeComment (Line : in out Unbounded_String) is
      IndexOfComment : Natural; -- index of the comment in the command line
   begin
      -- find comment in the line
      IndexOfComment := Index (Source => Line,
                               Pattern => CommentString);

      -- remove the comment
      if IndexOfComment /= 0 then
         Unbounded_Slice (Source => Line,
                          Target => Line,
                          Low    => 1,
                          High   => IndexOfComment - 1);
      end if;

      -- trim whitespaces
      Trim (Source => Line,
            Side   => Ada.Strings.Both);
   end removeComment;

   procedure parseCommand (CommandLine : in Unbounded_String;
                           FirstArg : Out Unbounded_String;
                           SecondArg : Out Natural;
                           CurrentCommandType : Out Command_Type)
   is
      package Strings_Vectors is new
        Ada.Containers.Vectors
          (Index_Type   => Positive,
           Element_Type => Unbounded_String);

      Command : Strings_Vectors.Vector;
      F   : Positive;
      L   : Natural;
      I   : Natural := 1;
      Whitespace : constant Character_Set := To_Set (' ' & Character'Val(9)); -- space, tab

   begin
      while I in To_String(CommandLine)'Range loop
         Find_Token
           (Source  => CommandLine,
            Set     => Whitespace,
            From    => I,
            Test    => Ada.Strings.Outside,
            First   => F,
            Last    => L);

         exit when L = 0;

         Command.Append (New_Item => Unbounded_Slice(Source => CommandLine,
                                                     Low    => F,
                                                     High   => L));
         I := L + 1;
      end loop;

      if To_String(Command.First_Element) in "add" | "sub" | "neg" | "eq" | "gt" | "lt" | "and" | "or" | "not" then
         CurrentCommandType := C_ARITHMETIC;
         FirstArg := Command.First_Element;
      elsif Command.First_Element = "push" then
         CurrentCommandType := C_PUSH;
         FirstArg:= Command.Element (Index => 2);
         SecondArg := Natural'Value(To_String(Command.Element (Index => 3)));
      elsif Command.First_Element = "pop" then
         CurrentCommandType := C_POP;
         FirstArg := Command.Element (Index => 2);
         SecondArg := Natural'Value(To_String(Command.Element (Index => 3)));
      end if;
      -- TODO: implement other parsing of other command types
   end parseCommand;

end VM_Parser;
