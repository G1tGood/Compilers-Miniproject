with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Identifier_Kinds; use Identifier_Kinds;
with Ada.Exceptions; use Ada.Exceptions;

package body Symbol_Table is

   -- Initialize the Symbol Table
   procedure Initialize (This : out SymbolTable) is
   begin
      This.classTable := new Identifier_Hashed_Maps.Map;
      This.subroutineTable := new Identifier_Hashed_Maps.Map;
   end Initialize;

   
   -- Start a new subroutine table (reset subroutine table)
   procedure startSubroutine (This : out SymbolTable) is
   begin
      This.subroutineTable.Clear;
   end startSubroutine;

   
   -- Define an identifier in the symbol table
   procedure define (This : in out SymbolTable;
                     Name : in String;
                     IType : String;
                     Kind  : Identifier_Kinds.IdentifierKind)
   is
      newEntry : IdentifierEntry;
      table : tablePointer := This.getTableScope(kind => kind);
   begin
      -- if the identifier already exists, don't define it
      if table.Contains (name) then
         return;
      end if;
      
      newEntry.iType := To_Unbounded_String(IType);
      newEntry.kind  := kind;
      newEntry.index := This.varCount (kind => Kind);
      
      table.Include (Key       => name,
                     New_Item  => newEntry);
   end define;

   
   -- Returns the amount of identifiers of the given kind already defined
   function varCount (This : in out SymbolTable;
                      Kind : Identifier_Kinds.IdentifierKind) return Natural is
      count : Natural := 0;
      table : tablePointer := This.getTableScope(kind => kind);
   begin
      for idEntry of table.all loop
         if idEntry.kind = kind then
            count := count + 1;
         end if;
      end loop;
      
      return count;
   end varCount;

   
   -- Returns the kind of the identifier with given name
   function KindOf (This : in SymbolTable;
                    Name : String) return Identifier_Kinds.IdentifierKind is
   begin
      if This.subroutineTable.Contains (name) then
         return This.subroutineTable.Element (name).kind;
      elsif This.classTable.Contains (name) then
         return This.classTable.Element (name).kind;
      end if;
      return I_NONE;
   end KindOf;

   
   -- Returns the type of the identifier with given name
   function TypeOf (This : in SymbolTable;
                    Name : String) return String is
      My_Except : exception;
   begin
      if This.subroutineTable.Contains (name) then
         return To_String (This.subroutineTable.Element (name).iType);
      elsif This.classTable.Contains (name) then
         return To_String (This.classTable.Element (name).iType);
      end if;
      raise My_Except with "error in TypeOf: no identifier with this name '" & name & "'";
   end TypeOf;

   
   -- Returns the index of the identifier with given name
   function IndexOf (This : in SymbolTable;
                     Name : String) return Natural is
      My_Except : exception;
   begin
      if This.subroutineTable.Contains (name) then
         return This.subroutineTable.Element (name).index;
      elsif This.classTable.Contains (name) then
         return This.classTable.Element (name).index;
      end if;
      raise My_Except with "error in IndexOf: no identifier with this name";
   end IndexOf;
   
   
   function getTableScope (This : in out SymbolTable; kind : in identifier_kinds.IdentifierKind) return tablePointer is
   begin
      case kind is
      when I_ARG | I_VAR =>
         return This.subroutineTable;
      when I_STATIC | I_FIELD =>
         return This.classTable;
      when others =>
         raise Constraint_Error with "Invalid identifier kind";
      end case;
   end getTableScope;
   

end Symbol_Table;
