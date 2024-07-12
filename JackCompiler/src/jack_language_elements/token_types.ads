package Token_Types is

   --| All possible command types in the JACK language.
   type TokenType is (
                       KEYWORD,
                       SYMBOL,
                       IDENTIFIER,
                       INT_CONST,
                       STRING_CONST,
                      ERROR,
                      TOKENS
                      );
end Token_Types;
