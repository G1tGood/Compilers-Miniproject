with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;

package body VM_Writer is

   procedure Initialize(This : in out VMWriter; OutputFile : in String) is
   begin
      Create (File => This.VmFile,
              Mode => Out_File,
              Name => OutputFile);
      This.scopeIndex := 1;
   end Initialize;

   procedure WritePush (This : in out VMWriter; Segment : in VMSegment; Index : in Natural) is
      function segmentToString (Segment : in VMSegment) return String is
      begin
         case Segment is
         when S_POINTER => return "pointer";
         when S_CONST => return "constant";
         when S_ARG => return "argument";
         when S_LOCAL => return "local";
         when S_STATIC => return "static";
         when S_TEMP => return "temp";
         when S_THAT => return "that";
         when S_THIS => return "this";
         end case;
      end segmentToString;
   begin
      Set_Col (File => This.vmFile,
               To   => This.scopeIndex);
      Put_Line (File => This.VmFile,
                Item => "push " & segmentToString(Segment) & Index'Image);
   end WritePush;

   procedure WritePop(This : in out VMWriter; Segment : in VMSegment; Index : in Natural) is
      function segmentToString (Segment : in VMSegment) return String is
      begin
         case Segment is
         when S_POINTER => return "pointer";
         when S_CONST => return "constant";
         when S_ARG => return "argument";
         when S_LOCAL => return "local";
         when S_STATIC => return "static";
         when S_TEMP => return "temp";
         when S_THAT => return "that";
         when S_THIS => return "this";
         end case;
      end segmentToString;
   begin
      Set_Col (File => This.vmFile,
               To   => This.scopeIndex);
      Put_Line (File => This.VmFile,
                Item => "pop " & segmentToString (Segment) & Index'Image);
   end WritePop;

   procedure WriteArithmetic (This : in out VMWriter; Command : in ArithmeticCommand) is
      function commandToString (Command : in ArithmeticCommand) return String is
      begin
         case Command is
         when C_ADD => return "add";
         when C_AND => return "and";
         when C_EQ => return "eq";
         when C_GT => return "gt";
         when C_LT => return "lt";
         when C_NEG => return "neg";
         when C_NOT => return "not";
         when C_OR => return "or";
         when C_SUB => return "sub";
         end case;
      end commandToString;
   begin
      Set_Col (File => This.vmFile,
               To   => This.scopeIndex);
      Put_Line (File => This.VmFile,
                Item => commandToString (Command => Command));
   end WriteArithmetic;

   procedure WriteLabel(This : in out VMWriter; Label : in String) is
   begin
      Put_Line (File => This.VmFile,
                Item => "label " & Label);
   end WriteLabel;

   procedure WriteGoto(This : in out VMWriter; Label : in String) is
   begin
      Set_Col (File => This.vmFile,
               To   => This.scopeIndex);
      Put_Line (File => This.VmFile,
                Item => "goto " & Label);
   end WriteGoto;

   procedure WriteIf(This : in out VMWriter; Label : in String) is
   begin
      Set_Col (File => This.vmFile,
               To   => This.scopeIndex);
      Put_Line (File => This.VmFile,
                Item => "if-goto " & Label);
   end WriteIf;

   procedure WriteCall(This : in out VMWriter; Name : in String; nArgs : in Integer) is
   begin
      Set_Col (File => This.vmFile,
               To   => This.scopeIndex);
      Put_Line (File => This.VmFile,
                Item => "call " & name & nArgs'Image);
   end WriteCall;

   procedure WriteFunction(This : in out VMWriter; Name : in String; nLocals : in Natural) is
   begin
      Set_Col (File => This.vmFile,
               To   => This.scopeIndex);
      Put_Line (File => This.VmFile,
                Item => "function " & Name & nLocals'Image);
   end WriteFunction;

   procedure WriteReturn(This : in out VMWriter) is
   begin
      Set_Col (File => This.vmFile,
               To   => This.scopeIndex);
      Put_Line (File => This.VmFile,
                Item => "return");
   end WriteReturn;

   procedure Close(This : in out VMWriter) is
   begin
      Close(File => This.VmFile);
   end Close;

   ------------------
   -- advanceScope --
   ------------------

   procedure advanceScope (This : in out VMWriter) is
   begin
      This.scopeIndex := This.scopeIndex + scopeIndent;
   end advanceScope;

   -------------------
   -- withdrawScope --
   -------------------

   procedure withdrawScope (This : in out VMWriter) is
   begin
      if This.scopeIndex >= scopeIndent + 1 then
         This.scopeIndex := This.scopeIndex - scopeIndent;
      end if;
   end withdrawScope;

end VM_Writer;
