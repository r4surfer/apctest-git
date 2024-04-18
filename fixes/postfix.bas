       REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
           *                                                           *~
           *                                 FFFFF  IIIII  X   X       *~
           *                                 F        I     X X        *~
           *                                 FFFF     I      X         *~
           *                                 F        I     X X        *~
           *   PROGRAM NAME                  F      IIIII  X   X       *~
           *                                                           *~
           *-----------------------------------------------------------*~
           * PROGRAM NAME - Program Description                        *~
           * Fix post date/time stamp from postdttm                    *~
           *                                                           *~
           *                                                           *~
           *                                                           *~
           *-----------------------------------------------------------*~
           * This program contains valuable trade secrets and proprie- *~
           * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
           * bodying substantial creative efforts  and confidential    *~
           * information.  Unauthorized use, copying, decompiling,     *~
           * translating, disclosure, or transfer of it is prohibited. *~
           * Copyright (c) 1983, an unpublished work by CAELUS,        *~
           * INCORPORATED, Spokane, wa.  All rights reserved.          *~
           *-----------------------------------------------------------*~
           *                  M O D I F I C A T I O N S                *~
           *---When---+----------------What----------------------+-Who-*~
           * 12/07/98 ! Original                                 ! DXL *~
           CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        dim                                                              ~
            plowkey$99,                  /*                            */~
            r$150                        /*                            */

        dim                                                              ~
            proc_flag$1,                 /* SYSFILE2 process status    */~
                                         /*  null, 'I'n, or 'C'ompleted*/~
            rec_chk$10,                  /* Count on records looked at */~
            rec_chng$10,                 /* Count on records changed   */~
            syskey$20,                   /* SYSFILE2 key               */~
            working$60                   /* Display var to show action */~

        dim f2%(20),                     /* = 0 if the file is open    */~
            f1%(20),                     /* = 1 if READ was successful */~
        /*  FS%(20),                        = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20)20,                 /* Text from file opening     */~
             axd$(20)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
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
            * #01 ! SYSFIE2  !System File                               *~
            * #02 ! ORIGINAL !Original Master File                      *~ 
            * #12 ! NEWFILE  !Modified Master File                      *~ 
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20                      ~

            select #02,  "HNYDETAL",                                     ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup

                             
*        Check if this has already been done
            call "SHOSTAT" ("Preparing For HNYDETAL Modification.")
            call "OPENCHCK" (#01, 0%, f2%( 1%),   0%, " ")
            syskey$ = "POSTFIX" & "70002"
            call "READ100" (#01, syskey$, f1%(1%))
            if f1%(1%) = 0% then L02180 /* Never been here before */
                get #01 using L02160, proc_flag$
L02160:         fmt pos(21), ch(1)
                if proc_flag$ = "I" then gosub inprocess_message ~
                                    else gosub processed_message

*        See if they really want to do this deed
L02180:     ask% = 0%                          /* Window in the middle */
            call "ASKUSER" (ask%, "*** REQUEST CONFIRMATION ***",        ~
               "This program will fix posting"                           ~
                     & " dates in HNYDETAL.",                            ~
                "Press (RETURN) to continue",                            ~
                "Press F16 to ABORT and return to the menu.")
            if ask% = 16% then goto exit_program
            if ask% <> 0% then goto L02180

L02210:     ask% = 2%                   /* Window at the bottom */
            call "ASKUSER" (ask%, "*** REQUEST VERIFICATION ***",        ~
                 "You have chosen to Continue with the fix program.",    ~
                 "Press (RETURN) to confirm this decision  -- OR -- ",   ~
                 "Press F(16) to ABORT and return to the menu.")
            if ask% = 16% then goto exit_program
            if ask% <> 0% then goto L02210

            call "SHOSTAT" ("Opening Files")

*        Make sure the original file is available
            call "OPENFILE" ( #2, "VALID", f2%(2%), " ", " ")
            if f2%(2%) = 0% then goto L02441
L02371:         u3% = 0%
                call "ASKUSER" ( u3%, "*** OPEN ERROR ***",              ~
                     "The File HNYDETAL Could NOT be found," &          ~
                          " and therefore will not be converted.",        ~
                     "Press RETURN to Exit.", " ")
                if u3% <> 0% then L02371
                goto exit_program
L02441
	    rslt$(2%) = "REQUIRED"
            call "OPENFILE" ( #02, "SHARE", f2%(2%), rslt$(2%), axd$(2%) )


        REM *************************************************************~
            *          M A I N   P R O G R A M  P R O C E S S I N G     *~
            *                                                           *~
            * The Work is done here                                     *~
            *************************************************************
*    process_original:  
            call "SHOSTAT" ("Processing HNYDETAL file contents")

            proc_flag$ = "I"
            gosub write_sysfile_record

            rec_chk%, rec_chng%  = 0%
            working$ = "Working "
            plowkey$ = all(hex(00))

    plow_original:
            call "PLOWNXT1" (#02, plowkey$, 0%,  f1%(2%))
                if f1%(2%) <> 1% then goto rename_newfile

            get #02 using L09140, r$, datestamp%
L09140:     FMT CH(150), pos(36), bi(3)

            rec_chk% = rec_chk% + 1%
            /* show progress every 100 records*/
            if mod(rec_chk%, 100%) <> 1% then test_and_write_record
                if len(working$) > 59% then working$ = "Working "
                working$ = working$ & "."
                call "SHOSTAT" ( working$ )

    test_and_write_record:
            ok% = 1%
            /* test if record should be writen */
            if datestamp% > 14299447% then ok% = 0%
            if ok% <> 1% then plow_original
   
            /* if ok then write here */
            /* this example writes to a new file but it may not be needed */
            rec_chng% = rec_chng% + 1%

            delete #2
            put #2 using L09140, r$, datestamp% + 15177216%
            write #2

            goto plow_original

    rename_newfile:

            proc_flag$ = "C"
            gosub write_sysfile_record

            convert rec_chk% to rec_chk$, pic(##########)
            convert rec_chng% to rec_chng$, pic(##########)

            call "STRING"addr("LJ", rec_chk$, 10%)
            call "STRING"addr("LJ", rec_chng$, 10%)

            call"ASKUSER" (ask%,  "*** PROCESSING COMPLETED ***",       ~
                        "HNYDETAL - " & rec_chk$ &  " records checked.",~
                        "           " & rec_chng$ & " records changed.",~
                                 "Press RETURN to acknowldge.")

            goto exit_program

     write_sysfile_record:
            call "READ101" (#01, syskey$, f1%(1%))
            put #01 using sysrec, syskey$, proc_flag$
            if f1%(1%) = 1% then rewrite #01 else write #01
     sysrec: fmt ch(20), ch(1)
            return


     processed_message:
            ask% = 0%                          /* Window in the middle */
            call "ASKUSER" (ask%, "*** ALREADY PROCESSED ***",           ~
               "This program has already been run against "              ~
                     & "your HNYDETAL file.",                            ~
                "Continue anyway?",                                      ~
                "Press Enter to Continue or F16 to Exit" )
            if ask% = 16% then exit_program
            if ask% = 0% then return
            goto processed_message

     inprocess_message:
            ask% = 0%                          /* Window in the middle */
            call "ASKUSER" (ask%, "*** IN PROCESS ***",                  ~
                "Status indicates this program is running against "      ~
                     & "your HNYDETAL file.",                            ~
                "Continue anyway?",                                      ~
                "Press Enter to Continue or F16 to Exit" )
            if ask% = 16% then exit_program
            if ask% = 0% then return
            goto inprocess_message
           
        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program

            call "SHOSTAT" ("One Moment Please")
            end
