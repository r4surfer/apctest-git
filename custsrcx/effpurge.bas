        dim readkey$100,                                                 ~
            effYear$4,                                                   ~
            year$4

        dim                              /* Extra Variables            */~
            f2%(8%),                     /* = 0 if the file is open    */~
            f1%(8%),                     /* = 1 if READ was successful */~
            fs%(8%), axd$4,              /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(11%)20                 /* Text from file opening     */

            select #1,   "EWDEFFCY",                                     ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    7, keylen =    13,                   ~
                        alt key  1, keypos =    1, keylen =  19

            select #11,  "EWDEFFPY",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  7,   keylen =  15,                     ~
                        alt key  1, keypos  =     1, keylen = 21

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 500%, rslt$(1%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 500%, rslt$(11%))
            
            
            init(" ") readkey$, effYear$, year$
            year$ = "2015"
            
            gosub purgeEffcy
            init(" ") readkey$, effYear$
            gosub purgeEffpy

            goto exit_program

purgeEffcy
           read #1, hold, using EFFCY_FMT, readkey$, eod goto EffcyDone
EFFCY_FMT:         FMT POS(07), CH(13)           

             effYear$ = str(readkey$,2%,4%)
             if effYear$ > year$ and effYear$ <> " " then goto purgeEffcy
             
             delete #1
             
             goto purgeEffcy
           
EffcyDone
return

purgeEffpy 
           read #11, hold, using EFFPY_FMT, readkey$, eod goto EffPyDone
EFFPY_FMT:        FMT POS(07), CH(15)       

             effYear$ = str(readkey$,1%,4%) 
             if effYear$ > year$ and effYear$ <> " " then goto purgeEffpy
             
             delete #11
             
             goto purgeEffpy
           
EffpyDone
return           

exit_program
end


            