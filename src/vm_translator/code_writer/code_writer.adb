with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with ada.strings.fixed;
with Command_Types; use Command_Types;

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
      end if;
   end setFileName;

   procedure writeArithmetic (This : in out CodeWriter; Command : String) is
      prologue           : String := "@SP" & ASCII.LF &
                             "A=M-1" & ASCII.LF &
                             "D=M" & ASCII.LF &
                             "A=A-1" & ASCII.LF;
      epilogue           : String := "@SP" & ASCII.LF &
                             "M=M-1" & ASCII.LF;
      CurrentTrueLabel   : String := This.currentTrueLabel;
      CurrentFalseLable    : String := This.currentFalseLable;
   begin
      if Command = "add" then
         Put_Line (This.File, prologue & "M=D+M" & ASCII.LF & epilogue);
      elsif Command = "sub" then
         Put_Line (This.File, prologue & "M=M-D" & ASCII.LF & epilogue);
      elsif Command = "neg" then
         Put_Line (This.File,
                   "@SP" & ASCII.LF &
                     "A=M" & ASCII.LF &
                     "A=A-1" & ASCII.LF &
                     "M=-M" & ASCII.LF
                  );
      elsif Command = "eq" then
         Put_Line (This.File,
                   prologue &
                     "D=D-M" & ASCII.LF &
                     "@" & CurrentTrueLabel & ASCII.LF &
                     "D;JEQ" & ASCII.LF &
                     "D=0" & ASCII.LF &
                     "@" & CurrentFalseLable & ASCII.LF &
                     "0;JMP" & ASCII.LF &
                     "(" & CurrentTrueLabel & ")" & ASCII.LF &
                     "D=-1" & ASCII.LF &
                     "(" & CurrentFalseLable & ")" & ASCII.LF &
                     "@SP" & ASCII.LF &
                     "A=M" & ASCII.LF &
                     "A=A-1" & ASCII.LF &
                     "A=A-1" & ASCII.LF &
                     "M=D" & ASCII.LF &
                     epilogue);
         This.advanceLabel;
      elsif Command = "gt" then
         Put_Line (This.File,
                   prologue &
                     "D=D-M" & ASCII.LF &
                     "@" & CurrentTrueLabel & ASCII.LF &
                     "D;JLT" & ASCII.LF &
                     "D=0" & ASCII.LF &
                     "@" & CurrentFalseLable & ASCII.LF &
                     "0;JMP" & ASCII.LF &
                     "(" & CurrentTrueLabel & ")" & ASCII.LF &
                     "D=-1" & ASCII.LF &
                     "(" & CurrentFalseLable & ")" & ASCII.LF &
                     "@SP" & ASCII.LF &
                     "A=M" & ASCII.LF &
                     "A=A-1" & ASCII.LF &
                     "A=A-1" & ASCII.LF &
                     "M=D" & ASCII.LF &
                     epilogue);
                  This.advanceLabel;
      elsif Command = "lt" then
         Put_Line (This.File,
                   prologue &
                     "D=D-M" & ASCII.LF &
                     "@" & CurrentTrueLabel & ASCII.LF &
                     "D;JGT" & ASCII.LF &
                     "D=0" & ASCII.LF &
                     "@" & CurrentFalseLable & ASCII.LF &
                     "0;JMP" & ASCII.LF &
                     "(" & CurrentTrueLabel & ")" & ASCII.LF &
                     "D=-1" & ASCII.LF &
                     "(" & CurrentFalseLable & ")" & ASCII.LF &
                     "@SP" & ASCII.LF &
                     "A=M" & ASCII.LF &
                     "A=A-1" & ASCII.LF &
                     "A=A-1" & ASCII.LF &
                     "M=D" & ASCII.LF &
                     epilogue);
                  This.advanceLabel;
      elsif Command = "and" then
         Put_Line (This.File, prologue & "M=D&M" & ASCII.LF & epilogue);
      elsif Command = "or" then
         Put_Line (This.File, prologue & "M=D|M" & ASCII.LF & epilogue);
      elsif Command = "not" then
         Put_Line (This.File,
                   "@SP" & ASCII.LF &
                     "A=M" & ASCII.LF &
                     "A=A-1" & ASCII.LF &
                     "M=!M" & ASCII.LF
                  );
      end if;
   end writeArithmetic;

   function segment_analyzer (This : CodeWriter; segment : String; index : Natural) return String is
      IndexString : String := ada.strings.fixed.trim (Natural'Image (index), Left);
      My_Except   : exception;
   begin
      if segment = "static" then
         return "@" & To_String (This.FileName) & "." & IndexString & ASCII.LF;
      elsif segment = "local" then
         return "@" & IndexString & ASCII.LF &
           "D=A" & ASCII.LF &
           "@LCL" & ASCII.LF &
           "A=M" & ASCII.LF &
           "A=A+D" & ASCII.LF;
      elsif segment = "argument" then
         return "@" & IndexString & ASCII.LF &
           "D=A" & ASCII.LF &
           "@ARG" & ASCII.LF &
           "A=M" & ASCII.LF &
           "A=A+D" & ASCII.LF;
      elsif segment = "this" then
         return "@" & IndexString & ASCII.LF &
           "D=A" & ASCII.LF &
           "@THIS" & ASCII.LF &
           "A=M" & ASCII.LF &
           "A=A+D" & ASCII.LF;
      elsif segment = "that" then
         return "@" & IndexString & ASCII.LF &
           "D=A" & ASCII.LF &
           "@THAT" & ASCII.LF &
           "A=M" & ASCII.LF &
           "A=A+D" & ASCII.LF;
      elsif segment = "constant" then
         return "@" & IndexString  & ASCII.LF;
      elsif segment = "temp" then
         return "@" & ada.strings.fixed.trim (Natural'Image (index + 5), Left) & CommentString & " temp[" & IndexString & "]" & ASCII.LF;
      elsif segment = "pointer" then
         if index = 0 then
            return "@THIS" & ASCII.LF;
         elsif index = 1 then
            return "@THAT" & ASCII.LF;
         end if;
         return "@" & IndexString & ASCII.LF;
      end if;
      
      raise My_Except with "bad segment: " & segment & ASCII.LF;
   end segment_analyzer;
   
   procedure WritePushPop (This : in out CodeWriter; CommandType : in Command_Type; segment : String; index : in Natural) is
      addrToA   : String := This.segment_analyzer (segment, index);
      My_Except : exception;
      ValueToD  : String (1 .. 3);
   begin
      ValueToD := "D=M";
      if segment = "constant" then
         ValueToD := "D=A";
      end if;
      case CommandType is
      when C_PUSH =>
         Put_Line (This.File, addrToA &
                     ValueToD & ASCII.LF &
                     "@SP" & ASCII.LF &
                     "A=M" & ASCII.LF &
                     "M=D" & ASCII.LF &
                     "@SP" & ASCII.LF &
                     "M=M+1" & ASCII.LF);
      when C_POP =>
         Put_Line (This.File, addrToA &
                     "D=A" & ASCII.LF &
                     "@R15" & ASCII.LF &
                     "M=D" & ASCII.LF &
                     "@SP" & ASCII.LF &
                     "A=M" & ASCII.LF &
                     "A=A-1" & ASCII.LF &
                     "D=M" & ASCII.LF &
                     "@R15" & ASCII.LF &
                     "A=M" & ASCII.LF &
                     "M=D" & ASCII.LF &
                     "@SP" & ASCII.LF &
                     "M=M-1" & ASCII.LF);
      when others => raise My_Except;
      end case;
   end WritePushPop;

   procedure Close (This : out CodeWriter) is
   begin
      Close (This.File);
   end close;
   
   
   function currentTrueLabel (This : in CodeWriter) return String is
      IndexString : String := ada.strings.fixed.trim (Natural'Image (This.LabelIndex), Left);
   begin
      return TrueTabel & IndexString;
   end currentTrueLabel;
   
   function currentFalseLable (This : in CodeWriter) return String is
      IndexString : String := ada.strings.fixed.trim (Natural'Image (This.LabelIndex), Left);
   begin
      return EndLabel & IndexString;
   end currentFalseLable;
   
   procedure advanceLabel (This : in out CodeWriter) is
   begin
      This.LabelIndex := This.LabelIndex + 1;
      end advanceLabel;

end Code_Writer;
