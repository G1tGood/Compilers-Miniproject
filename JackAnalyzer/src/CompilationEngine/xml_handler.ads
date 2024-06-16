with Ada.Text_IO; use Ada.Text_IO;

-- this package provides an interface to handle xml files
package Xml_Handler is
   
   --| handler of xml file - provides function to write into xml file
   type XmlHandler is tagged limited private;
   
   --| Advances the scope inside the xml file
   --| @param This The XmlHandler instance
   --| @param xmlFileName the xml file name to write into
   procedure Initialize (This : in out XmlHandler; xmlFileName : in String);
   
   --| closes the xml handler
   --| @param This The XmlHandler instance
   procedure Close (This : in out XmlHandler);
   
   --| Advances the scope inside the xml file
   --| @param This The XmlHandler instance
   procedure advanceScope (This : in out  XmlHandler);
   
   --| Withdraws the scope inside the xml file
   --| @param This The XmlHandler instance
   procedure withdrawScope (This : in out  XmlHandler);
   
   --| writes a new xml entry terminal in the current scope
   --| @param This The XmlHandler instance
   --| @param ElementType type of the element to insert
   --| @param Value the value of the element to insert
   procedure writeTerminal (This : in out  XmlHandler; ElementType : String; Value : String);
   
   --| writes a new xml entry terminal in the current scope
   --| @param This The XmlHandler instance
   --| @param ElementType type of the element to insert
   --| @param isCloser is the nonterminal element an opening or closing one
   procedure writeElement (This : in out  XmlHandler; ElementType : in String;  IsCloser: in Boolean := False);
   
private
   
   type XmlHandler is tagged limited record
      xmlFile : File_Type; -- file to which the handle writes
      scopeIndex : Ada.Text_IO.Positive_Count; -- current scope in the xml file
   end record;
   
   scopeIndent : constant Ada.Text_IO.Positive_Count := 2;
   
end Xml_Handler;
