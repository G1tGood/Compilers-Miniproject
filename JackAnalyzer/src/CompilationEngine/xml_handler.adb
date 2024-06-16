with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Xml_Handler is

   --| Initializes the XmlHandler instance with the given XML file.
   procedure Initialize (This : in out XmlHandler; xmlFileName : in String) is
   begin
      Create (File => This.xmlFile,
              Mode => Out_File,
              Name => xmlFileName);
      This.scopeIndex := 1;
   end Initialize;

   procedure Close (This : in out XmlHandler) is
   begin
      Close (File => This.xmlFile);
   end Close;
   
   --| Advances the scope inside the XML file.
   procedure advanceScope (This : in out XmlHandler) is
   begin
      This.scopeIndex := This.scopeIndex + scopeIndent;
   end advanceScope;

   --| Withdraws the scope inside the XML file.
   procedure withdrawScope (This : in out XmlHandler) is
   begin
      if This.scopeIndex >= scopeIndent + 1 then
         This.scopeIndex := This.scopeIndex - scopeIndent;
      end if;
   end withdrawScope;

   --| Writes a new XML entry of terminal in the current scope.
   procedure writeTerminal (This : in out XmlHandler; ElementType : in String; Value : in String) is
   begin
      Set_Col (File => This.xmlFile,
               To   => This.scopeIndex);
      Put (This.xmlFile, "<" & ElementType & "> ");
      Put (This.xmlFile, Value);
      Put_Line (This.xmlFile, " </" & ElementType & ">");
   end writeTerminal;
   
   --| Writes a new XML entry in the current scope.
   procedure writeElement (This : in out XmlHandler; ElementType : in String; IsCloser : in Boolean := False) is
   begin
      Set_Col (File => This.xmlFile,
               To   => This.scopeIndex);
      if IsCloser then
         Put_Line (This.xmlFile, "</" & ElementType & ">");
      else
         Put_Line (This.xmlFile, "<" & ElementType & ">");
      end if;
   end writeElement;

end Xml_Handler;
