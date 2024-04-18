        REM *************************************************************~
            *                                                           *~
            *   SSS   TTTTT  JJJJJ  PPPP    AAA    SSS    SSS     1     *~
            *  S        T      J    P   P  A   A  S      S       11     *~
            *   SSS     T      J    PPPP   AAAAA   SSS    SSS     1     *~
            *      S    T    J J    P      A   A      S      S    1     *~
            *   SSS     T     J     P      A   A   SSS    SSS   11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STJPASS1 - Auto Reversal at End of Month.  Loads entries  *~
            *            from Standard Journal Main File, Reverses, and *~
            *            places in Buffer for Processing.               *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/05/81 ! ORIGINAL                                 ! TEM *~
            * 01/09/84 ! ADDED STOP DATE TO DELETE ENTRIES <USE>  ! LDJ *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            *          ! CHANGES INCLUDED GLPOST, HNYPOST, JNLINFO!     *~
            *          !(NEW),GLPRTSUB(NEW), INCREASE IN GL BUFFER!     *~
            *          ! INTERFACE FILE RECORD SIZE FOR PASSING   !     *~
            *          ! MODULE, JOURNAL, AND POSTING SEQUENCE    !     *~
            * 08/19/85 ! Fixed STJLINES & STJBUF2 file formats    ! LDJ *~
            * 03/14/88 ! Change SAVEFLAG to 'N' on Currency trans ! JIM *~
            * 05/09/90 ! If reversing jnl not posted, no reversals! JDH *~
            * 06/28/91 ! Added Reversing entry flag for GL Export ! JBK *~
            *************************************************************

        dim acct$(100)9,                 /* ACCOUNT NUMBERS FOR ENTRY  */~
            adj$4,                       /* Adj Flag - Note Used       */~
            credit(100),                 /* CREDIT AMOUNTS THIS ENTRY  */~
            debit(100),                  /* DEBIT AMOUNTS THIS ENTRY   */~
            descr$(100)32,               /* ACCOUNT DESCRIPTIONS       */~
            description$36,              /* DESCRIPTION OF ENTRY       */~
            name$10,                     /* NAME OF JOURNAL ENTRY      */~
            newreadkey$50,               /* DITTO.                     */~
            oldreadkey$50,               /* KEY FOR PLOW ROUTINES      */~
            postedflag$1,                /* Has journal been posted?   */~
            ref1$(100)30,                /* REFERENCE 1                */~
            ref2$(100)34,                /* REFERENCE 2                */~
            reverseflag$3,               /* WHETHER OR NOT TO REVERSE  */~
            reversing_entry$1,           /* Flag for Reversing Entries */~
            saveflag$3,                  /* WHETHER OR NOT TO SAVE IT  */~
            stopdate$6,                  /* DELETE ENTRY DATE          */~
            userid$3                     /* USERID THIS USER           */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
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
            * # 5 ! STJMASTR ! STANDARD JOURNAL MASTER FILE             *~
            * # 6 ! STJLINES ! STANDARD JOURNAL LINE ITEMS FILE.        *~
            * # 9 ! STJBUFFR ! STANDARD JOURNAL HEADER BUFFER AREA.     *~
            * #10 ! STJBUF2  ! STANDARD JOURNAL DETAIL BUFFER AREA.     *~
            *************************************************************

            select  #5, "STJMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 10

            select  #6, "STJLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 13

            select  #9, "STJBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 7,                          ~
                        alt key 1, keypos= 8, keylen= 7,                 ~
                            key 2, keypos=15, keylen=10

            select #10, "STJBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 13

            call "OPENFILE" (#5,"SHARE",f2%(5),rslt$(5),axd$(5))
            call "OPENFILE" (#6,"SHARE",f2%(6),rslt$(6),axd$(6))
            call "OPENFILE" (#9,"SHARE",f2%(9),rslt$(9),axd$(9))
            call "OPENFILE" (#10,"SHARE",f2%(10),rslt$(10),axd$(10))

            REM MAKE SURE ALL THE FILES ARE OPENED.
                    if f2%(9) = 0 then L02180
                     call "OPENFILE" (#9,"OUTPT",f2%(9),rslt$(9),axd$(9))
                       close #9          /* AND REOPEN IN SHARED MODE  */
                     call "OPENFILE" (#9,"SHARE",f2%(9),rslt$(9),axd$(9))

L02180:         REM LOOK FOR DETAIL FILE, OPEN IF NOT FOUND.
                    if f2%(10) = 0 then L09000
                  call "OPENFILE" (#10,"OUTPT",f2%(10),rslt$(10),axd$(10))
                    close #10          /* AND REOPEN IN SHARED MODE  */
                  call "OPENFILE" (#10,"SHARE",f2%(10),rslt$(10),axd$(10))

L09000: REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * SETS USERID TO PLOW THRU BUFFER FOR THIS USER.            *~
            *************************************************************

            call "EXTRACT" addr ("ID", userid$)

        REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * READS JOURNAL ENTRIES FOR THIS USER AND DELETES THEM.     *~
            *************************************************************

            call "SHOSTAT" ("AUTO-REVERSING JOURNAL ENTRIES")

            oldreadkey$ = " "

L10100:     call "PLOWNEXT" (#5, oldreadkey$, 0%, f1%(5))
                 if f1%(5) = 0 then L65000

            init(" ") acct$(), descr$(), ref1$(), ref2$()
            mat credit = zer
            mat debit = zer

            REM GET HEADER
                get #5, using L10200,                                     ~
                    name$, description$, saveflag$, reverseflag$,        ~
                    stopdate$, postedflag$
L10200:             FMT CH(10), CH(36), 2*CH(1), CH(06), CH(1)

                if reverseflag$ <> "Y" then L10100
                if postedflag$  <> "Y" then L10100
                reverseflag$ = "N"
                reversing_entry$ = "Y"

            if str(name$,,4) = "CURR" then saveflag$ = "N"

            REM GET LINE ITEMS.  NOTE HOW I REVERSE THE CREDIT/DEBITS.
                newreadkey$ = name$
                maxlines% = 0
L10270:         call "PLOWNEXT" (#6, newreadkey$, 10%, f1%(6))
                     if f1%(6) = 0 then L10350
                     t%, maxlines% = maxlines% + 1
                     get #6, using L10320, acct$(t%), ref1$(t%),          ~
                                          ref2$(t%), adj$, descr$(t%),   ~
                                          credit(t%), debit(t%)
L10320:            FMT XX(13),CH(9),CH(30),CH(34),CH(4),CH(32),2*PD(14,4)
                go to L10270

L10350:     REM WRITE TO BUFFER
                if maxlines% = 0 then L10100
                call "FMTKEY" (#9, key$, reversekey$)
                gosub L30000
                go to L10100

L30000: REM *************************************************************~
            *      WRITE     A N   E N T R Y   T O     B U F F E R      *~
            *                                                           *~
            * PUTS     AN  ENTRY TO  THE BUFFER AND PUTS IT INTO THE    *~
            * NECESSARY ARRAYS.  DON'T PROCESS IF # OF LINES = 0.       *~
            *************************************************************

            REM WRITE HEADER RECORD AND FORMAT ENTRIES IN IT,
                write #9, using L30110, key$, reversekey$,                ~
                        name$, description$, saveflag$, reverseflag$,    ~
                        stopdate$, " ", reversing_entry$, " "

L30110:                 FMT CH(7), CH(7),          /* SPECIAL KEYS     */~
                            CH(10),                /* ENTRY NAME       */~
                            CH(36),                /* DESCRIPTION      */~
                            2*CH(1),               /* SAVE, REVERSE FLS*/~
                            CH(06),                /* DELETE ENTRY DATE*/~
                            CH(1),                 /* Filler           */~
                            CH(1),                 /* Reversing Entry  */~
                            CH(30)

            REM WRITE DATA ITEMS TO BUFFER FILE.
               for temp% = 1 to maxlines%
                   convert temp% to seqnr$, pic(###)
                   write #10, using L30230,                               ~
                             name$, seqnr$, acct$(temp%), ref1$(temp%),  ~
                             ref2$(temp%), " ", descr$(temp%),           ~
                             debit(temp%), credit(temp%)," "
                   next temp%
L30230:                      FMT CH(10),           /* ENTRY NAME       */~
                                 CH(3),            /* SEQUENCE NUMBER  */~
                                 CH(9),            /* ACCOUNT NUMBER   */~
                                 CH(30),           /* REFERENCE 1      */~
                                 CH(34),           /* REFERENCE 2      */~
                                 CH(04),           /* Adj Flag - N/A   */~
                                 CH(32),           /* DESCRIPTION      */~
                                 2*PD(14,4),       /* DEBIT, CREDIT AMT*/~
                                 CH(22)
                return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
