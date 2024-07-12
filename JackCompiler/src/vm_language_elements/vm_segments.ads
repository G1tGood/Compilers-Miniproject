package Vm_Segments is

   --| All segments in VM languege
   type VMSegment is (
                      S_CONST,     -- const segment
                      S_ARG,       -- arg segment
                      S_LOCAL,     -- local segment
                      S_STATIC,    -- static segment
                      S_THIS,      -- this segment
                      S_THAT,      -- that segment
                      S_POINTER,   -- pointer segment
                      S_TEMP       -- temp segment
                      );

end Vm_Segments;
