
        dim f2%(32%),                    /* = 0 if the file is open    */~
            f1%(32%),                    /* = 1 if READ was successful */~
            fs%(32%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32%)20                 /* Text from file opening     */

        dim readkey$50                   /* Misc. Purpose Read Key     */
        
        dim hdr$40,msg$(3)79, cnt$10      /* Ask User Message          */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************


            select #01,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup


            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, fs%( 1%), f2%( 1%), 200%, rslt$( 1%))
            cnt% = 0%
            
            init(" ") readkey$, cnt$
            str(readkey$,1,2) = "LO"

read_next:


            read #1, hold, key > readkey$, using cust_fmt, readkey$,    ~
                                                        eod goto read_done

cust_fmt:            FMT CH(09)

                  if str(readkey$,1,2) <> "LO" then goto read_done


                  put #1, using term_fmt, "NET 31"
term_fmt:                 FMT POS(543), CH(20)

                  rewrite #1, eod goto write_error

                  cnt% = cnt% + 1%
                      goto read_next

read_done:

            convert cnt% to cnt$, pic(##########)
            comp% = 2%
            hdr$ =    "           *** Terms Update ***                  "
            msg$(1) = "Number of Lowes Customers Updated -->  "  & cnt$
            msg$(2) = "                                                 "
            msg$(3) = "Press Any Key To Continue. "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
            
        end



write_error:

            call "SHOSTAT" ("Error Updating Customer Record" & str(readkey$,1,9))
            stop

              goto read_next



