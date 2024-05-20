--|  This package offers an enumeration of all VM language command types.
package Command_Types is

   --| All possible command types in the VM language.
   type Command_Type is (
                         C_ARITHMETIC,  --| Represents arithmetic commands.
                         C_PUSH,        --| Represents push commands.
                         C_POP,         --| Represents pop commands.
                         C_LABEL,       --| Represents label commands.
                         C_GOTO,        --| Represents goto commands.
                         C_IF,          --| Represents if-goto commands.
                         C_FUNCTION,    --| Represents function commands.
                         C_RETURN,      --| Represents return commands.
                         C_CALL         --| Represents call commands.
                        );

   --| the string starting a comment in VM
   CommentString : constant String := "//";

end Command_Types;
