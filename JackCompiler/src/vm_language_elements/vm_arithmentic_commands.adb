with Ada.Exceptions;
package body vm_arithmentic_commands is

   function toArithmeticCommand (commandChar : in Character; IsUnary : Boolean := False) return ArithmeticCommand
   is
   begin
      if not IsUnary then
         case commandChar is
         when '+' => return C_ADD;
         when '-' => return C_SUB;
         when '&' => return C_AND;
         when '|' => return C_OR;
         when '<' => return C_LT;
         when '>' => return C_GT;
         when '=' => return C_EQ;
         when others =>
            raise Constraint_Error with (commandChar & " cannot be translated to a vm command");
         end case;
      else
         case commandChar is
         when '-' => return C_NEG;
         when '~' => return C_NOT;
         when others =>
            raise Constraint_Error with (commandChar & " cannot be translated to a vm command");
         end case;
      end if;
   end toArithmeticCommand;
end vm_arithmentic_commands;
