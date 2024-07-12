package identifier_kinds is

   --| All possible kinds for identifer in Jack language
   type IdentifierKind is (
                           I_STATIC,
                           I_FIELD,
                           I_ARG,
                           I_VAR,
                           I_NONE
                      );

end identifier_kinds;
