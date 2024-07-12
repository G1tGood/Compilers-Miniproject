package keyword_types is

   --| All possible keyword types in the JACK language.
   type KeywordType is (
                        K_CLASS,
                        K_METHOD,
                        K_FUNCTION,
                        K_CONSTRUCTOR,
                        K_INT,
                        K_BOOLEAN,
                        K_CHAR,
                        K_VOID,
                        K_VAR,
                        K_STATIC,
                        K_FIELD,
                        K_LET,
                        K_DO,
                        K_IF,
                        K_ELSE,
                        K_WHILE,
                        K_RETURN,
                        K_TRUE,
                        K_FALSE,
                        K_NULL,
                        K_THIS
                      );
end keyword_types;
