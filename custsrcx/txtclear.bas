        REM - Check for Upgrade to Release R6.04.03
        REM
        REM              AS OF 11/13/97  [TXTCLEAR and TXTBUILD] Same.
        REM - Create a new Text File with no "B, M, or W" Record Types
        REM
        REM   Note- The next record Counter is in the Type "N" record
        REM         and needs to be reset the the next record number
        REM         after 'TXTFIX' is run. Position is at POS(5) format
        REM         is BI(4).
        REM   Note- Sequence Number (For Multiple Lines) is at position
        REM         POS(10) format BI(2)

        dim txt$11,                     /* Text File Key               */~
            rslt$20, scr$51,            /* Open File and Screen Display*/~
            rec$(3%)70, text$64,        /* Text Records                */~
            hdr$38%, msg$(3%)79,        /* Askuser Variables           */~
            date$8, userid$3            /* Security Check              */

            hdr$ = "** Creating New Text File (        )**"
            str(hdr$,28%,8%) = "TXTROYS "

            select #1,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #2,  "TXTROYS",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11
                                      /* Current Caelus Live Text File */
            call "OPENCHCK" (#1, x%, y%,  0%, rslt$)
                                      /* New EWD (TXTHIST) Text File   */
            call "OPENCHCK" (#2, x%, y%,  0%, rslt$)

            if x% = 1% then goto error_1
                                      /*Text History File Already Exist*/

               x%, y% = 0%            /* Open and Create New Text File */
               call "OPENCHCK" (#2, x%, y%,100%, rslt$)

               call "EXTRACT" addr("ID", userid$)
               gosub check_access     /* Only (RHH, and DJD) Id's can  */
                                      /*   run this Utility.           */
               if userid$ <> "RHH" and userid$ <> "DJD" then             ~
                                                           goto exit_prog
                  if comp% <> 0% then goto exit_prog

         display                                                         ~
            at(01,21), hdr$

        REM - Initialize Text Primary Key
            txt$ = all(hex(00))
            cnt% = 0% : cnt1% = 0%
            scr$ = "Text Records Read [ ######## ] Updated [ ######## ]"
        next_rec
            read #1,key > txt$, using L00560, txt$, eod goto build_done
L00560:         FMT CH(11)
            cnt% = cnt% + 1%
            if mod(cnt%,100) <> 0 then goto L00640
               convert cnt% to str(scr$,21%,8%), pic(########)

               convert cnt1% to str(scr$,42%,8%), pic(########)

               print at(10,15);hex(84);scr$;
L00640:     p% = pos("BMW" = str(txt$,1%,1%))
            if p% <> 0% then goto next_rec  /* Skip Types "B, M, and W"*/

        REM - WRITE_REC                /* Create New TXTFILE with only */
                                       /* Record Types C, N, S, and T  */
             get #1,using L00700  , text$, rec$()
L00700:         FMT CH(64), 3*CH(70)
             write #2, using L00700  , text$, rec$(), eod goto error_2
             cnt1% = cnt1% + 1%
        goto next_rec

        build_done
            convert cnt% to str(scr$,21%,8%), pic(########)

            convert cnt1% to str(scr$,42%,8%), pic(########)


            call "SHOSTAT" ("(Done)" & scr$)
            goto exit_prog

        check_access
            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            comp% = 2%
            msg$(1%)= "****** Special Systems Maintenance Utility ******"
            msg$(2%)= "         C R E A T E   T E X T   F I L E         "
            msg$(3%)= "Press <RETURN> To Continue, Any PF() Key to Exit?"
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        error_1
             call "SHOSTAT" ("(Error) - Text File Already Exists")
             goto exit_prog
        error_2
             call "SHOSTAT" ("Unable to Write TEXT - "& txt$ ) : stop
             goto next_rec

        exit_prog
          stop
        end
