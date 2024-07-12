with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Vm_Segments; use Vm_Segments;
with vm_arithmentic_commands; use vm_arithmentic_commands;

package VM_Writer is

   --  Type representing the VM Writer.
   type VMWriter is tagged limited private;

   -- Creates a new file and prepares it for writing.
   -- @param This       An instance of VMWriter.
   -- @param OutputFile The name of the output file to be created.
   procedure Initialize(This : in out VMWriter; OutputFile : in String);
   
   -- Writes a VM push command.
   -- @param This    An instance of VMWriter.
   -- @param Segment The segment to push onto the stack (e.g., CONST, ARG, LOCAL, etc.).
   -- @param Index   The index within the segment.
   procedure WritePush(This : in out VMWriter; Segment : in VMSegment; Index : in Natural);

   -- Writes a VM pop command.
   -- @param This    An instance of VMWriter.
   -- @param Segment The segment to pop from the stack (e.g., CONST, ARG, LOCAL, etc.).
   -- @param Index   The index within the segment.
   procedure WritePop(This : in out VMWriter; Segment : in VMSegment; Index : in Natural);

   -- Writes a VM arithmetic command.
   -- @param This    An instance of VMWriter.
   -- @param Command The arithmetic command to write (e.g., ADD, SUB, NEG, etc.).
   procedure WriteArithmetic(This : in out VMWriter; Command : in ArithmeticCommand);

   -- Writes a VM label command.
   -- @param This  An instance of VMWriter.
   -- @param Label The label to write.
   procedure WriteLabel(This : in out VMWriter; Label : in String);

   -- Writes a VM goto command.
   -- @param This  An instance of VMWriter.
   -- @param Label The label to go to.
   procedure WriteGoto(This : in out VMWriter; Label : in String);

   -- Writes a VM if-goto command.
   -- @param This  An instance of VMWriter.
   -- @param Label The label to go to if the condition is true.
   procedure WriteIf(This : in out VMWriter; Label : in String);

   -- Writes a VM call command.
   -- @param This  An instance of VMWriter.
   -- @param Name  The name of the function to call.
   -- @param nArgs The number of arguments the function takes.
   procedure WriteCall(This : in out VMWriter; Name : in String; nArgs : in Integer);

   -- Writes a VM function command.
   -- @param This    An instance of VMWriter.
   -- @param Name    The name of the function.
   -- @param nLocals The number of local variables the function has.
   procedure WriteFunction(This : in out VMWriter; Name : in String; nLocals : in Natural);

   -- Writes a VM return command.
   -- @param This An instance of VMWriter.
   procedure WriteReturn(This : in out VMWriter);

   -- Closes the output file.
   -- @param This An instance of VMWriter.
   procedure Close (This : in out VMWriter);
   
   -- advances scope
   -- @param This An instance of VMWriter.
   procedure advanceScope (This : in out VMWriter);
   
   -- widthdraws scope
   -- @param This An instance of VMWriter.
   procedure withdrawScope (This : in out VMWriter);

private

   type VMWriter is tagged limited record
      VmFile : File_Type; -- the output vm file
      scopeIndex : Ada.Text_IO.Positive_Count; -- current scope in the vm file
   end record;
   
   scopeIndent : constant Ada.Text_IO.Positive_Count := 4;
   
end VM_Writer;

