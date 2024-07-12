package vm_arithmentic_commands is

   --| All arithmetic commands in VM languege
   type ArithmeticCommand is (
                              C_ADD,     -- ADD command
                              C_SUB,     -- SUB command
                              C_NEG,     -- NEQ command
                              C_EQ,      -- EQ command
                              C_GT,      -- GT command
                              C_LT,      -- LT command
                              C_AND,     -- AND command
                              C_OR,      -- OR command
                              C_NOT      -- NOT command
                             );
   --| translates a character representing some erithmetic command to an arithmetic command
   --| @param commandChar the command character
   --| @param IsUnary optional parameter, used for characters that represent both unary and binary operations
   function toArithmeticCommand (commandChar : in Character; IsUnary : Boolean := False) return ArithmeticCommand;
end vm_arithmentic_commands;
