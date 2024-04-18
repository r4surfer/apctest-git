        dim                              /* FILE = APCPLNUD            */~
            ud_rec$112,                  /* Record Format              */~
            ud_key$48,                   /* Primary Key                */~
            ud_key1$48,                  /* AWDPLNUD key               */~
            comp_date$10                 /* Compare Purge Date         */

        dim                                                              ~
            hdr$40,                      /* ASKUSER HEADER             */~
            msg$(3%)79                   /* ASKUSER TEXT               */


        dim f2%(15%),                    /* = 0 if the file is open    */~
                                         /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
            rslt$(15%)20                 /* Text from file opening     */


        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNUD ! Production Audit Update File             *~
            * #2  ! APCPLNUD ! Production Audit Update File             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,   "APCPLNUD",                                     ~
                        varc,     indexed,  recsize = 112,               ~
                        keypos =   35, keylen =   48,                    ~
                        alt key  1, keypos =    1, keylen =  49

            select #2,   "AWDPLNUD",                                     ~
                        varc,     indexed,  recsize = 112,               ~
                        keypos =   35, keylen =   48,                    ~
                        alt key  1, keypos =    1, keylen =  49

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 500%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 500%, rslt$(2%))



        init(" ") ud_rec$, ud_key$, ud_key1$, comp_date$

        comp_date$ = "01/01/2007"
        call "DATUFMTC" (comp_date$)

        gosub file_analysis
          goto exit_program


        file_analysis
            ud_key$ = all(hex(00))
        analysis_next
            read #1, hold, key > ud_key$, using ud_fmt, ud_rec$,     ~
                                           eod goto analysis_done
ud_fmt:            FMT CH(112)

                   ud_key$ = str(ud_rec$,35,48)

                   if str(ud_rec$,34,6) >= str(comp_date$,1,6)      ~
                                       then goto analysis_next

                   delete #1

                   gosub write_file
                     goto analysis_next
        analysis_done
        return

        write_file
           ud_key1$ = all(hex(00))
           ud_key1$ = ud_key$

           read #2, key = ud_key1$, eod goto create_rec

                return

create_rec:
 
             
          put #2, using awdplnud_fmt, ud_rec$
awdplnud_fmt:        FMT CH(112)

          write #2, eod gosub write_error

        return
        write_error
            errormsg$ = "(Write Error) - APCPLNUD = " & ud_key$
            gosub error_prompt
        return


        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end


