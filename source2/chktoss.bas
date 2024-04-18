        REM *************************************************************~
            *                                                           *~
            *   CCC   H   H  K   K  TTTTT   OOO    SSS    SSS           *~
            *  C   C  H   H  K  K     T    O   O  S      S              *~
            *  C      HHHHH  KKK      T    O   O   SSS    SSS           *~
            *  C   C  H   H  K  K     T    O   O      S      S          *~
            *   CCC   H   H  K   K    T     OOO    SSS    SSS           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CHKTOSS  - TOSSES CHECKS FOR A GIVEN USER FROM THE CHECK  *~
            *            GENERATION BUFFER INTO THE CASH DISBURSEMENTS  *~
            *            BUFFER FOR LATER PROCESSING.                   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/15/80 ! ORIGINAL                                 ! BCW *~
            * 07/31/80 ! BUFFER STRUCTURE REVISION                ! BCW *~
            * 06/02/81 ! ADDED 16 CHAR INVOICE NUMBER MODS        ! TOM *~
            * 07/23/81 ! TOSS OVER DISCOUNT AMT ON LINE ITEM      ! TEM *~
            * 01/09/86 ! TOSS OVER ENTIRE LINE THIS TIME          ! KAB *~
            *************************************************************

        dim                                                              ~
            bufferkey$11,                /* KEY TO BUFFER INFORMATION  */~
            detail$(1)100,               /* ONE CHECK DETAIL IN CORE   */~
            header$(10)10,               /* HEADER INFORMATION IN CORE */~
            key$7,                       /* KEY TO CASH DISB. BUFFER   */~
            readkey$50,                  /* KEY FOR PLOWING DETAILS    */~
            reversekey$7,                /* REVERSE BUFFER KEY INFO    */~
            userid$3                     /* USERID OF THIS USER        */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.08 02/14/86 VBK & VENDOR enhancements       "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 7 ! CSHBUFFR ! CASH DISBURSEMENTS BUFFER FILE           *~
            * # 8 ! CSHBUF2  ! CASH DISBURSEMENTS CHECK DETAILS         *~
            * # 9 ! CHKBUFFR ! CHECK GENERATION BUFFER AREA             *~
            * #10 ! CHKBUF2  ! CHECK GENERATION CHECK DETAIL BUFFER     *~
            *************************************************************

            select  #7, "CSHBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 7,                          ~
                        alternate key 1, keypos =  8, keylen =  7,       ~
                                  key 2, keypos = 15, keylen = 17,       ~
                                  key 3, keypos = 24, keylen =  8, dup

            select  #8, "CSHBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup

            select  #9, "CHKBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 07,                         ~
                        alternate key 1, keypos =  08, keylen = 07,      ~
                                  key 2, keypos =  24, keylen =  8

            select #10, "CHKBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alt key 1, keypos =21, keylen = 16, dup

            call "FILEOPEN" (#7, "SHARE", f2%(7), rslt$(7), axd$(7))
            call "FILEOPEN" (#8, "SHARE", f2%(8), rslt$(8), axd$(8))
            call "FILEOPEN" (#9, "SHARE", f2%(9), rslt$(9), axd$(9))
            call "FILEOPEN" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))

            REM MAKE SURE CHECK FILES ARE OPEN BEFORE WRITING TO THEM.
                if f2%(7) = 0 then L02470
                   call "FILEOPEN"(#7,"OUTPT",f2%(7),rslt$(7),axd$(7))
                   close #7
                   call "FILEOPEN"(#7,"SHARE",f2%(7),rslt$(7),axd$(7))

L02470:         if f2%(8) = 0 then L09000
                   call "FILEOPEN"(#8,"OUTPT",f2%(8),rslt$(8),axd$(8))
                   close #8
                   call "FILEOPEN"(#8,"SHARE",f2%(8),rslt$(8),axd$(8))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * SET USERID AND KEY TO BUFFER.                             *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            bufferkey$ = userid$

            REM GET BUFFERSEQNR%
                bufferseqnr% = 0
                call "REDALT2" (#7, bufferkey$, 1%, f1%(7))
                     if f1%(7) = 0 then L10000
                get #7, using L09150, temp$
L09150:                 FMT CH(7)
                if str(temp$, 1, 3) <> userid$ then L10000
                   convert str(temp$, 4, 4) to bufferseqnr%

L10000: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * PLOWS THROUGH BUFFER, WRITING CHECKS OF THIS GUY FROM THE *~
            * CHECK GENERATION BUFFER TO THE CASH DISBURSEMENTS BUFFER. *~
            *************************************************************


            call "SHOSTAT" ("Transferring Checks to Disbursements Buffer")

            call "PLOWNEXT" (#9, bufferkey$, 3%, f1%(9))
                 if f1%(9) = 0 then L10390

            get #9, using L10092,checknr$
L10092:         FMT XX(23),CH(8)
            if str(checknr$,1,1)="X" then L10000

            REM GET CHECK HEADER INFORMATION.
                get #9, using L10120, str(header$(),1)
L10120:                 FMT XX(14), CH(57)
                REM SET UP KEY AND REVERSE KEY INFORMATION.
                    call "FMTKEY" (#7, key$, reversekey$)

               write #7,using L10200,key$,reversekey$,str(header$(),1)," "
L10200:                   FMT CH(7), CH(7), CH(57),CH(29)

            REM NOW TOSS LINE ITEMS ONE AT A TIME.  FORTUNATELY IDENTICAL.
                readkey$ = str(header$(), 1, 17)
L10240:         call "PLOWNEXT" (#10, readkey$, 17%, f1%(10))
                     if f1%(10) = 0 then L10310
                get   #10, using L10280, str(detail$(), 1, 100)
                write #8,  using L10280, str(detail$(), 1, 100)
L10280:                   FMT CH(100)

                goto L10240

L10310:     REM DELETE THE OLD CHECK FROM THE BUFFER.
                call "DELETE" (#9, bufferkey$, 7%)
                readkey$ = str(header$(), 1, 17)
                call "DELETE" (#10, readkey$, 17%)

            REM AND NOW DO NEXT CHECK.
                goto L10000

L10390: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING BACK TO   *~
            * THE NEXT PROGRAM.                                         *~
            *************************************************************

            call "SHOSTAT" ("ONE MOMENT PLEASE")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
