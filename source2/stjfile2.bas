        REM *************************************************************~
            *                                                           *~
            *   SSS   TTTTT  JJJJJ  FFFFF  IIIII  L      EEEEE   222    *~
            *  S        T      J    F        I    L      E          2   *~
            *   SSS     T      J    FFFF     I    L      EEEE    222    *~
            *      S    T    J J    F        I    L      E      2       *~
            *   SSS     T     J     F      IIIII  LLLLL  EEEEE  22222   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STJFILE2 - Transfers Standard Journal Auto-Reverse Entries*~
            *            at End of Month back to Main File.  Note that  *~
            *            the Debits/Credits are Re-Reversed.            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/07/81 ! ORIGINAL (FROM STJFILE1)                 ! TEM *~
            * 01/09/84 ! DELETES ENTRIES BASED ON STOP DATE  <USE>! LDJ *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            *          ! CHANGES INCLUDED GLPOST, HNYPOST, JNLINFO!     *~
            *          !(NEW),GLPRTSUB(NEW), INCREASE IN GL BUFFER!     *~
            *          ! INTERFACE FILE RECORD SIZE FOR PASSING   !     *~
            *          ! MODULE, JOURNAL, AND POSTING SEQUENCE    !     *~
            * 08/19/85 ! Corrected STJBUF2, STJLINES file formats ! LDJ *~
            * 05/09/90 ! Sets posted flag to 'N' after reversals. ! JDH *~
            * 06/26/96 ! Changes for the year 2000.               ! DXL *~
            * 09/15/97 ! Changed SHOWMSG to SHOSTAT (2 calls)     ! MLJ *~
            *************************************************************

        dim acct$(100)9,                 /* ACCOUNT NUMBERS FOR ENTRY  */~
            adj$4,                       /* Adj Flag - N/A here        */~
            blankdate$8,                 /* Blank date for comparison  */~
            credit(100),                 /* CREDIT AMOUNTS THIS ENTRY  */~
            date$6,                      /* CURRENT SYSTEM DATE        */~
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
            saveflag$3,                  /* WHETHER OR NOT TO SAVE IT  */~
            stopdate$6,                  /* DATE TO DELETE ENTRY       */~
            userid$3                     /* USERID THIS USER           */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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

            call"OPENFILE"(#5,"SHARE",f2%(5),rslt$(5),axd$(5))
            call"OPENFILE"(#6,"SHARE",f2%(6),rslt$(6),axd$(6))
            call"OPENFILE"(#9,"SHARE",f2%(9),rslt$(9),axd$(9))
            call"OPENFILE"(#10,"SHARE",f2%(10),rslt$(10),axd$(10))

            REM MAKE SURE ALL THE FILES ARE OPENED.
                    if f2%(5) = 0 then L02180
                       call"OPENFILE"(#5,"OUTPT",f2%(5),rslt$(5),axd$(5))
                       close #5          /* AND REOPEN IN SHARED MODE  */
                       call"OPENFILE"(#5,"SHARE",f2%(5),rslt$(5),axd$(5))

L02180:         REM LOOK FOR DETAIL FILE, OPEN IF NOT FOUND.
                    if f2%(6) = 0 then L09000
                       call"OPENFILE"(#6,"OUTPT",f2%(6),rslt$(6),axd$(6))
                       close #6          /* AND REOPEN IN SHARED MODE  */
                       call"OPENFILE"(#6,"SHARE",f2%(6),rslt$(6),axd$(6))

L09000: REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * SETS USERID TO PLOW THRU BUFFER FOR THIS USER.            *~
            *************************************************************

            call "EXTRACT" addr ("ID", userid$)
            date$ = date
            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

        REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * READS JOURNAL ENTRIES FOR THIS USER AND DELETES THEM.     *~
            *************************************************************

            call "SHOSTAT" ("Deleting Journal Entries From Buffer")

            oldentrykey$ = userid$

L10100:     call "READ102" (#9, oldentrykey$, f1%(9))
                 if f1%(9) = 0 then L65000
            get #9, using L10130, newentrykey$
L10130:             FMT CH(7)
                    if str(oldentrykey$, 1, 3)<>userid$ then L65000

            oldentrykey$ = newentrykey$
            gosub L30000                  /* GET INFORMATION ON FILE.   */

            call "DELETE" (#5, name$, 10%)
            call "DELETE" (#6, name$, 10%)
            if stopdate$ <> " " ~
               and stopdate$ <> blankdate$ ~
               and stopdate$ <= date$ then L10410
            if saveflag$ = "N" then L10410
            if maxlines% = 0 then L10410
            if postedflag$ = "Y" then postedflag$ = "N"

               write #5, using L10250,                                    ~
                      name$, description$, saveflag$, reverseflag$,      ~
                      stopdate$, postedflag$, " "
L10250:                  FMT CH(10),               /* NAME OF ENTRY    */~
                             CH(36),               /* DESCRIPTION OF IT*/~
                             2*CH(1),              /* SAVE, REVERSE FLG*/~
                             CH(06),               /* STOP DATE        */~
                             CH(1),                /* Posted Flag      */~
                             CH(45)                /* FILLER           */

               for temp% = 1 to maxlines%
                   convert temp% to seqnr$, pic(###)
                   write #6, using L10350,                                ~
                             name$, seqnr$, acct$(temp%), ref1$(temp%),  ~
                             ref2$(temp%), " ", descr$(temp%),           ~
                             debit(temp%), credit(temp%), " "
                   next temp%
L10350:                      FMT CH(10),           /* ENTRY NAME       */~
                                 CH(3),            /* SEQUENCE NUMBER  */~
                                 CH(9),            /* ACCOUNT NUMBER   */~
                                 CH(30),           /* REFERENCE 1      */~
                                 CH(34),           /* REFERENCE 2      */~
                                 CH(04),           /* Adj Flag - N/A   */~
                                 CH(32),           /* DESCRIPTION      */~
                                 2*PD(14,4),       /* DEBIT, CREDIT AMT*/~
                                 CH(22)            /* FILLER           */


L10410:     REM NOW DELETE THE THING FROM BUFFER.
                call "DELETE" (#9, newentrykey$, 7%)
                call "DELETE" (#10, name$, 10%)
                goto L10100

L30000: REM *************************************************************~
            *      L O A D   A N   E N T R Y   O F F   B U F F E R      *~
            *                                                           *~
            * SNATCHES THE ENTRY OFF THE BUFFER AND PUTS IT INTO THE    *~
            * NECESSARY ARRAYS.  DON'T PROCESS IF # OF LINES = 0.       *~
            *************************************************************

            REM LOAD HEADER RECORD AND FORMAT ENTRIES IN IT,
                get #9, using L30110,                                     ~
                        name$, description$, saveflag$, reverseflag$,    ~
                        stopdate$, postedflag$
L30110:                 FMT XX(14),                /* SPECIAL KEYS     */~
                            CH(10),                /* ENTRY NAME       */~
                            CH(36),                /* DESCRIPTION      */~
                            2*CH(1),               /* SAVE, REVERSE FLS*/~
                            CH(06),                /* DELETE ENTRY DATE*/~
                            CH(1)                  /* Posted Flag      */

            REM PLOW ROUTINE TO LOAD DATA FROM LINE ITEMS.
                oldreadkey$ = name$
                maxlines% = 0

L30200:         call "READ102" (#10, oldreadkey$, f1%(10))
                     if f1%(10) = 0 then L30360
                get #10, using L30230, newreadkey$
L30230:                        FMT CH(13)
                if str(newreadkey$,1,10)<>str(oldreadkey$,1,10) then L30360
                   oldreadkey$ = newreadkey$
                   maxlines% = maxlines% + 1
                   get #10, using L30310,                                 ~
                            acct$(maxlines%), ref1$(maxlines%),          ~
                            ref2$(maxlines%), adj$, descr$(maxlines%),   ~
                            credit(maxlines%), debit(maxlines%)
                   goto L30200
L30310:                           FMT XX(13),      /* KEY              */~
                                      CH(9),       /* ACCOUNT NUMBER   */~
                                      CH(30),      /* REFERENCE 1      */~
                                      CH(34),      /* REFERENCE 2      */~
                                      CH(04),      /* Adj Flag         */~
                                      CH(32),      /* DESCRIPTION      */~
                                      2*PD(14,4)   /* DEBITS, CREDITS  */

L30360:     REM RETURN
                return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            *************************************************************
            call "SHOSTAT" ("One Moment Please")

            end
