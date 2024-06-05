with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with ada.strings.fixed;
with Command_Types; use Command_Types;
with Ada.Containers.Indefinite_Hashed_Maps;  use Ada.Containers;
with Ada.Strings.Hash;

package body Code_Writer is
   procedure Initialize (This : out CodeWriter; FileName : Unbounded_String) is
   begin
      Create (This.File, Out_File, To_String (FileName));
   end Initialize;

   procedure setFileName (This : out CodeWriter; FileName : Unbounded_String) is
      Idx : Natural := Index (Source  => FileName,
                              Pattern => "\",
                              From    => To_String (FileName)'Last,
                              Going   => Backward);
   begin
      Put (This.File, CommentString);
      Put_Line (This.File, To_String (FileName));
      if Idx /= 0 then
         Unbounded_Slice (Source => FileName,
                          Target => This.FileName,
                          Low    => Idx + 1,
                          High   => To_String (FileName)'Last);
      else
         This.FileName := FileName;
         This.reinitRetLabel;
      end if;
   end setFileName;

   procedure writeArithmetic (This : in out CodeWriter; Command : String) is
      use Command_Vectors;
      prologue             : Vector := 
                               Empty_Vector &
                               "@SP" &
                               "A=M-1" &
                               "D=M" &
                               "A=A-1";
      epilogue             : Vector := 
                               Empty_Vector &
                               "@SP" &
                               "M=M-1";
      
      function condition_code return Command_Vectors.Vector is
         -- create mapping between conditional command in VM to HACK (eg. 'lt' -> 'JLT')
         package VM_To_HACK_Map is new Ada.Containers.Indefinite_Hashed_Maps
           (Key_Type        => String,
            Element_Type    => String,
            Hash            => Ada.Strings.Hash,
            Equivalent_Keys => "=");
         Vm_To_Hack : VM_To_HACK_Map.Map;
         
         Result : Vector;
      begin
         -- put conversions
         VM_To_HACK_Map.Include (Vm_To_Hack, "gt", "JLT");
         VM_To_HACK_Map.Include (Vm_To_Hack, "lt", "JGT");
         VM_To_HACK_Map.Include (Vm_To_Hack, "eq", "JEQ");
       
         Result.Append ("D=D-M");
         Result.Append (String'("@" & This.CurrentTrueLabel));
         Result.Append (String'("D;" & VM_To_HACK.Element (Command)));
         Result.Append ("D=0");
         Result.Append (String'("@" & This.CurrentFalseLabel));
         Result.Append ("0;JMP");
         Result.Append (String'("(" & This.CurrentTrueLabel & ")"));
         Result.Append ("D=-1");
         Result.Append (String'("(" & This.CurrentFalseLabel & ")"));
         Result.Append ("@SP");
         Result.Append ("A=M");
         Result.Append ("A=A-1");
         Result.Append ("A=A-1");
         Result.Append ("M=D");
         
         return Result;
      end condition_code;
         
   begin
      if Command not in "not" | "neg" then
         This.commandsVector.Append_Vector (prologue);
      end if;
         
      if Command = "add" then
         This.commandsVector.Append ("M=D+M");
      elsif Command = "sub" then
         This.commandsVector.Append ("M=M-D");
      elsif Command = "neg" then
         This.commandsVector.Append ("@SP");
         This.commandsVector.Append ("A=M");
         This.commandsVector.Append ("A=A-1");
         This.commandsVector.Append ("M=-M");
      elsif Command in "eq" | "lt" | "gt" then
         This.commandsVector.Append_Vector (condition_code);
         This.advanceCompLabel;
      elsif Command = "and" then
         This.commandsVector.Append ("M=D&M");
      elsif Command = "or" then
         This.commandsVector.Append ("M=D|M");
      elsif Command = "not" then
         This.commandsVector.Append ("@SP");
         This.commandsVector.Append ("A=M");
         This.commandsVector.Append ("A=A-1");
         This.commandsVector.Append ("M=!M");
      end if;
      
      if Command not in "not" | "neg" then
         This.commandsVector.Append_Vector (epilogue);
      end if;
   end writeArithmetic;

   
   function segment_analyzer (This : CodeWriter; segment : String; index : Natural) return Command_Vectors.Vector is
      use Command_Vectors;
      -- create mapping between segment in VM to HACK (eg. 'local' -> 'LCL')
      package VM_To_HACK_Map is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => String,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");
      Vm_To_Hack : VM_To_HACK_Map.Map;
      
      IndexString : String := ada.strings.fixed.trim (Natural'Image (index), Left);
      My_Except   : exception;
   begin
      -- put conversions
      VM_To_HACK_Map.Include (Vm_To_Hack, "local", "LCL");
      VM_To_HACK_Map.Include (Vm_To_Hack, "argument", "ARG");
      VM_To_HACK_Map.Include (Vm_To_Hack, "this", "THIS");
      VM_To_HACK_Map.Include (Vm_To_Hack, "that", "THAT");
      
      
      if segment in "local" | "argument" | "this" | "that" then
         return 
           Empty_Vector &
           String'("@" & IndexString) &
           "D=A" &
           String'("@" & Vm_To_Hack.Element (segment)) &
           "A=M" &
           "A=A+D";
      elsif segment = "constant" then
         return 
           Empty_Vector & 
           String'("@" & IndexString);
      elsif segment = "temp" then
         return
           Empty_Vector &
           String'("@" & ada.strings.fixed.trim (Natural'Image (index + 5), Left) & CommentString & " temp[" & IndexString & "]");
      elsif segment = "static" then
         return
           Empty_Vector &
           String'("@" & This.genFileLabel (IndexString));
      elsif segment = "pointer" then
         if index = 0 then
            return 
              Empty_Vector &
              "@THIS";
         elsif index = 1 then
            return
              Empty_Vector &
              "@THAT";
         end if;
         raise My_Except with "bad index with POINTER segment: " & IndexString & ASCII.LF;
      end if;
      raise My_Except with "bad segment: " & segment & ASCII.LF;
   end segment_analyzer;
   
   
   procedure WritePushPop (This : in out CodeWriter; CommandType : in Command_Type; segment : String; index : in Natural) is
      use Command_Vectors;
      addrToA   : Vector := This.segment_analyzer (segment, index);
      My_Except : exception;
      ValueToD  : String (1 .. 3);
   begin
      ValueToD := "D=M";
      if segment = "constant" then
         ValueToD := "D=A";
      end if;
      case CommandType is
         when C_PUSH =>
            This.commandsVector.Append_Vector 
              (
               addrToA &
                 ValueToD &
                 "@SP" &
                 "A=M" &
                 "M=D" &
                 "@SP" &
                 "M=M+1");
         when C_POP =>
            This.commandsVector.Append_Vector 
              (
               addrToA &
                 "D=A" & 
                 "@R15" & 
                 "M=D" & 
                 "@SP" & 
                 "A=M" & 
                 "A=A-1" &
                 "D=M" &
                 "@R15" &
                 "A=M" &
                 "M=D" &
                 "@SP" &
                 "M=M-1");
         when others => raise My_Except;
      end case;
   end WritePushPop;
   
   procedure writeInit (This : out CodeWriter) is
   begin
      This.currentlyParsedFunction := To_Unbounded_String("Bootstrap");
      This.commandsVector.Append ("// Bootstap code");
      This.commandsVector.Append ("@256");
      This.commandsVector.Append ("D=A");
      This.commandsVector.Append ("@SP");
      This.commandsVector.Append ("M=D");
      This.writeCall (FunctionName => BootstrapInitFunc,
                      NumArgs      => 0);
   end writeInit;

   procedure writeLabel (This : in out CodeWriter; Label : in String) is
   begin
      This.commandsVector.Append (String'("(" & This.genFileLabel (Label) & ")"));
   end writeLabel;

   procedure writeGoto (This : in out CodeWriter; Label : in String) is
      use Command_Vectors;
   begin
      This.commandsVector.Append_Vector (
                                         Empty_Vector &
                                           String'("@" & This.genFileLabel (Label)) &
                                           "0;JMP"
                                        );
   end writeGoto;

   procedure writeIf (This : in out CodeWriter; Label : in String) is
      use Command_Vectors;
   begin
      This.commandsVector.Append_Vector 
        (
         Empty_Vector &
           "@SP" &
           "M=M-1" & -- lower the stack and point to the topmost var
           "A=M" &   -- put topmost var location in A
           "D=M" &   -- D = topmost var
           String'("@" & This.genFileLabel (Label)) &
           "D;JNE" -- if topmost var != False goto Label
        );
   end writeIf;

   
   procedure writeCall (This : in out CodeWriter; FunctionName : in String; NumArgs : in Natural) is
      use Command_Vectors;
      use Command_Types;
      ReturnLabel        : String := To_String(This.currentlyParsedFunction) & "$ret_" & This.currentRetLabel;
      OffsetString       : String := ada.strings.fixed.trim (Natural'Image (NumArgs + 5), Left);
      
      function CustomPush (Adress : String; D_To : String) return Vector is
      begin
         return
           Empty_Vector &
           String'("@" & Adress) &
           String'("D=" & D_To) &
           "@SP" &
           "A=M" &
           "M=D" &
           "@SP" &
           "M=M+1";
      end CustomPush;
   begin
      This.advanceRetLabel;
      
      -- add vm command in comment
      This.commandsVector.Append (String'(CommentString & "call " & FunctionName & Natural'Image (NumArgs)));
      
      -- push return adress
      This.commandsVector.Append_Vector (CustomPush (Adress => ReturnLabel,
                                                     D_To   => "A"));
      
      -- push LCL
      This.commandsVector.Append_Vector (CustomPush (Adress => "LCL",
                                                     D_To   => "M"));
      
      -- push ARG
      This.commandsVector.Append_Vector (CustomPush (Adress => "ARG",
                                                     D_To   => "M"));
      
      -- push THIS
      This.commandsVector.Append_Vector (CustomPush (Adress => "THIS",
                                                     D_To   => "M"));
      
      -- push THAT
      This.commandsVector.Append_Vector (CustomPush (Adress => "THAT",
                                                     D_To   => "M"));
      
      -- ARG = SP-n-5
      This.commandsVector.Append_Vector 
        (
         Empty_Vector &
           "@SP" &
           "D=M" &
           String'("@" & OffsetString) &
           "D=D-A" &
           "@ARG" &
           "M=D"
        );
      
      -- LCL = SP
      This.commandsVector.Append_Vector 
        (
         Empty_Vector &
           "@SP" &
           "D=M" &
           "@LCL" &
           "M=D"
        );
      
      -- goto function
      This.commandsVector.Append_Vector
        (
         Empty_Vector &
         String'("@" & FunctionName) &
           "0;JMP"
        );
      
      -- return label
      This.commandsVector.Append  
        (
         String'("(" & ReturnLabel & ")")
        );
      
      
   end writeCall;

   
   procedure writeReturn (This : in out CodeWriter) is
      use Command_Vectors;
      
      -- help function: pop frame into the given adress
      function popFrame (Adress : String) return Vector is
      begin
         return
           Empty_Vector &
           "@LCL" &  -- FRAME location
           "M=M-1" & -- FRAME -= 1
           "A=M" &   -- A = FRAME
           "D=M" &   -- D = *FRAME
           String'("@" & Adress) &
           "M=D";   -- Adress = D = *FRAME
      end popFrame;
   begin
      -- Frame = LCL
      This.commandsVector.Append_Vector
        (
         Empty_Vector &
           "@LCL" &
           "D=M"
        );
      
      -- RET = *(FRAME - 5)
      This.commandsVector.Append_Vector
        (
         Empty_Vector &
           "@5" &
           "A=D-A" &
           "D=M" &
           "@13" &
           "M=D"
        );
      
      -- *ARG = pop()
      this.commandsVector.Append_Vector
        (
         Empty_Vector &
           "@SP" &
           "M=M-1" & -- SP -= 1
           "A=M" &   -- A = top of stack
           "D=M" &   -- d = *(top of stack)
           "@ARG" &
           "A=M" &   -- A = ARG
           "M=D"     -- *ARG = *(top of stackO)
        );
      
      -- SP = ARG + 1
      this.commandsVector.Append_Vector
        (
         Empty_Vector &
           "@ARG" &
           "D=M" & -- D = ARG
           "@SP" &
           "M=D+1"     -- SP = D+1 = ARG + 1
        );

      -- THAT = *(FRAME-1)
      This.commandsVector.Append_Vector (popFrame ("THAT"));

      -- THIS = *(FRAME-2)
      This.commandsVector.Append_Vector (popFrame ("THIS"));

      -- ARG = *(FRAME-3)
      This.commandsVector.Append_Vector (popFrame ("ARG"));

      -- LCL = *(FRAME-4)
      This.commandsVector.Append_Vector (popFrame ("LCL"));

      -- goto RET
      This.commandsVector.Append_Vector 
        (
         Empty_Vector &
           "@R13" &
           "A=M" &
           "0;JMP"
        );
      
   end writeReturn;

   
   procedure writeFunction (This : in out CodeWriter; FunctionName : in String; NumLocals : in Natural) is
      use Command_Vectors;
      
      FunctionLabel : String := FunctionName;
      EndLabel : String := FunctionName & "$End";
      LocalsLoopLabel : String := FunctionName & "$LocalsLoop";
      NumLocalsString : String := ada.strings.fixed.trim (Natural'Image (NumLocals), Left);
   begin
      This.reinitRetLabel; -- starting parse of new function, reilitialize return label index
      This.currentlyParsedFunction := To_Unbounded_String (FunctionName);
      
      This.commandsVector.Append_Vector 
        (
         Empty_Vector &
           String'("(" & FunctionLabel & ")") &
           String'("@" & NumLocalsString) &
           "D=A" &
           String'("@" & EndLabel) &
           "D;JEQ" &
           String'("(" & LocalsLoopLabel & ")") &
           "@SP" &
           "A=M" &
           "M=0" &
           "@SP" &
           "M=M+1" &
           String'("@" & LocalsLoopLabel) &
           "D=D-1;JNE" &
           String'("(" & EndLabel & ")")
        );
   end writeFunction;


   procedure Close (This : out CodeWriter) is
   begin
      Close (This.File);
   end close;
   
      
   function currentTrueLabel (This : in CodeWriter) return String is
      IndexString : String := ada.strings.fixed.trim (Natural'Image (This.LabelIndex), Left);
   begin
      return TrueTabel & IndexString;
   end currentTrueLabel;
   
      
   function currentFalseLabel (This : in CodeWriter) return String is
      IndexString : String := ada.strings.fixed.trim (Natural'Image (This.LabelIndex), Left);
   begin
      return EndLabel & IndexString;
   end currentFalseLabel;
   
      
   procedure advanceCompLabel (This : in out CodeWriter) is
   begin
      This.LabelIndex := This.LabelIndex + 1;
   end advanceCompLabel;
      
      
   procedure advanceRetLabel (This : in out CodeWriter) is
   begin
      This.retLabelIndex := This.retLabelIndex + 1;
   end advanceRetLabel;
   
      
   function currentRetLabel (This : in CodeWriter) return String is
      IndexString : String := ada.strings.fixed.trim (Natural'Image (This.retLabelIndex), Left);
   begin
      return IndexString;
   end currentRetLabel;
   
      
   procedure reinitRetLabel (This : in out CodeWriter) is
   begin
      This.retLabelIndex := 0;
   end reinitRetLabel;
      
   
   function genFileLabel (This : in CodeWriter; Label : String) return string is
   begin
      return To_String (This.FileName) & "." & Label;
   end genFileLabel;
   
   procedure flushCommands (This : in out CodeWriter) is
   begin
      This.commandsVector.Append ("");
      for Command of This.commandsVector loop
         Put_Line (File => This.File,
                   Item => Command);
      end loop;
      This.commandsVector.Clear;
   end flushCommands;

end Code_Writer;
