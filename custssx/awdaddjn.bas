        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y   AAA   DDDD   DDDD   JJJJJ  N   N   *~
            *  H   H  NN  N  Y   Y  A   A  D   D  D   D    J    NN  N   *~
            *  HHHHH  N N N   YYY   AAAAA  D   D  D   D    J    N N N   *~
            *  H   H  N  NN    Y    A   A  D   D  D   D  J J    N  NN   *~
            *  H   H  N   N    Y    A   A  DDDD   DDDD    J     N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYADDJN - Journal program for Inventory Additions. Prints*~
            *            recap of parts added to inventory, posts it to *~
            *            G/L, if necessary, prints daily recap, and     *~
            *            posts new averaged costs to inventory.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/19/80 ! ORIGINAL                                 ! BCW *~
            * 10/08/80 ! NEW MULTILOCATION FILE LAYOUTS           ! TEM *~
            * 02/04/81 ! NEW AND FINAL INVENTORY LAYOUTS          ! TEM *~
            * 05/50/81 ! ADDED WIP/JC REPORTING                   ! TOM *~
            * 05/30/83 ! MISSPELLED SRCEACCT BLOCKED POSTING      ! KEN *~
            * 06/17/83 ! DISALLOW BLANK DESCR, VALIDATE G/L,      ! JRW *~
            *          ! DISALLOW BLANK PART, JOURNAL PRINT OK    ! JRW *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            *          ! CHANGES INCLUDED GLPOST, HNYPOST, JNLINFO!     *~
            *          !(NEW),GLPRTSUB(NEW), INCREASE IN GL BUFFER!     *~
            *          ! INTERFACE FILE RECORD SIZE FOR PASSING   !     *~
            *          ! MODULE, JOURNAL, AND POSTING SEQUENCE    !     *~
            * 12/06/85 ! Clean up some bad formatting, delete     ! ERN *~
            *          ! transaction once updated.                !     *~
            * 01/27/87 ! Added Serial Number handling logic. Plus ! LDJ *~
            *          ! changes to the ADDBUFFR and HNYQUAN files!     *~
            *          ! now incorporates what was JOBHNYAD via a !     *~
            *          ! call to HNYADJOB (also prettied it up a  !     *~
            *          ! tad).                                    !     *~
            * 05/12/87 ! Expand costs from 3 to 12; Std cost mods ! JIM *~
            *          !   HNYPROC dropped. PROCPOST - HNYPROCU   !     *~
            * 01/03/90 ! Modified code to combine like accounts   ! LAB *~
            *          ! for gl postings - Accountancy Phase I.   !     *~
            *          ! String Searches to prevent false hits.   !     *~
            * 10/11/90 ! Merge GL Export IF & Accountancy Phase I.! JDH *~
            * 05/20/91 ! No extra reads if GL Export not on.      ! JDH *~
            * 05/29/91 ! PRR 11771 Print Posting Date on Headers  ! SID *~
            * 02/21/93 ! Added Coding for Core Value Project.     ! JBK *~
            *          !  Removed references to HNYADJPF File,    !     *~
            *          !  GLPRTSUB, and GLLOG$.  Modified         !     *~
            *          !  text passed to HNYPST2 per JDH.         !     *~
            * 04/29/93 ! HNYADDTF Description field now 32 bytes. ! JIM *~
            * 01/14/94 ! Added to GLCMBSUB argument list.         ! JDH *~
            * 04/25/95 ! PRR 11945,12026,12664. PO# to HNYPROCU.  ! JDH *~
            * 08/14/06 ! (AWD001) Mod to add GLORTRAN             ! CMG *~
            *************************************************************

        sub "AWDADDJN" (userid$, returncode%)

        dim                                                              ~
            account$9,                   /* DUMMY ARG TO STACK ROUTINES*/~
            acct$(4)9,                   /* array for glposting info   */~
            adjacct$16,                  /* ADJUSTMENT ACCT            */~
            core_inv_flag$1,             /* Core On for Inv Trans      */~
            cost(12),                    /* HNYADDTF costs             */~
            creditsstk$(250)9,           /* CREDITS FOR RCP#1 PHASE    */~
            creditsstk (250) ,           /* CREDIT AMOUNTS FOR RCP#1   */~
            date$8,                      /* DATE (FORMATTED)           */~
            datetime$7,                  /* Date Time Stamp            */~
            debitsstk$(250)9,            /* DEBIT ACCTS FOR RCP#1 PHASE*/~
            debitsstk (250),             /* DEBIT AMOUNTS FOR RCP#1    */~
            descr$40,                    /* DESCRIPTION FROM ENTRIES   */~
            export_on$1,                 /* G/L Export File processing?*/~
            glamount(4),                 /* array for glposting info   */~
            gltext$100,                  /* GL TEXT STRING             */~
            hdrdate$45,                  /* FORMATTED DATE/TIME INFO   */~
            hnyacct$9,                   /* INVENTORY ACCOUNT NUMBER   */~
            hnydate$6,                   /* USER'S INVENTORY DATE      */~
            hnydte$8,                    /* USER'S INVENTORY DATE      */~
            hnyflg$1,                    /* FLAG TO DETERMINE VAR ACCT */~
            jnlid$,                      /* JOURNAL ID                 */~
            jobnr$8,                     /* JOB NUMBER FROM BUFFER     */~
            linenumber%(5),              /* LINE POINTERS FOR JURN1    */~
            location$2,                  /* LOCATOR ARRAY FOR STACKS   */~
            lot$16,                      /* LOT NUMBER                 */~
            modno$2,                     /* MODULE NUMBER              */~
            nextitemkey$6,               /* NEXT LINE ITEM TO READ     */~
            part$25,                     /* PART NUMBER TO DO          */~
            partreadkey$50,              /* READ KEY FOR PLOWS         */~
            passedin_acct$(50)109,       /* array for glcmbsub         */~
            passedin_dbcr(50,2),         /* array for glcmbsub         */~
            passedin_type$(50)2,         /* array for glcmbsub         */~
            ponr$16,                     /* PURCHASE ORDER NUMBER      */~
            plowkey$64,                  /* File Plow Key Variable     */~
            plowwrk$64,                  /* File Plow Key Variable     */~
            prtacct$16,                  /* PRINT ROUTINE ACCOUNT# COPY*/~
            prtcost$10,                  /* L, M, O COSTS FOR PRINTING */~
            prtcostdescr$2,              /* "T:"                       */~
            prtcredit$10,                /* CREDIT AMOUNT TO PRINT     */~
            prtdebit$10,                 /* DEBIT AMOUNT TO PRINT      */~
            prtdescr$40,                 /* DESCRIPTION/WHERE FROM PRT */~
            prtpart$32,                  /* PART #/DESCR TO PRINT      */~
            prtqty$10,                   /* QUANTITY ADDED TO PRINT    */~
            rcpacct$(2)16,               /* ACCOUNT NUMBERS FOR RECAP  */~
            rcpacctdescr$(2)30,          /* ACCOUNT DESCRIPTIONS-RECAP */~
            rcpamt(2),                   /* RECAP AMOUNTS              */~
            rcpline%(2),                 /* LINE POINTERS FOR RECAP    */~
            rcpptr%(2),                  /* POINTER INTO D & C ARRAYS  */~
            readkey$50,                  /* File Read Key Variable     */~
            rpthnydate$8,                /* Reports Posting Date       */~
            serial$20,                   /* Serial Number to Add       */~
            srceacct$9,                  /* SOURCE (BUYING) ACCOUNT    */~
            store$3,                     /* STORE NUMBER               */~
            summary$1,                   /* SUMMARY INDICATOR          */~
            suspense_acct$16,            /* System Suspense Account    */~
            text$109,                    /* TEXT FOR GLCMBSUB          */~
            title$70,                    /* JOURNAL TITLE              */~
            title1$70,                   /* JOURNAL TITLE              */~
            userid$3,                    /* USERID THIS USER           */~
            procsrce$26,                 /* Source of Procurement      */~
            vendor$9                     /* VENDOR CODE                */

        dim                              /* G/L Export Posting Info    */~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            partcat$4,                   /* Part Category code         */~
            partclass$4,                 /* Part Class code            */~
            partgen$16,                  /* Part Generic code          */~
            parttype$3,                  /* Part Type code             */~
            tran_type$5,                 /* G/L Transaction type       */~
            uom$                         /* Part Unit of measure       */


        dim division$3,                  /* division number (AWD001)   */~
            schema$8                     /* schema          (AWD001)   */


        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch finalization of R6.04.00  "
        REM *************************************************************
            mat f2% = con

                     /* THEN VARIABLES F2%() AND AXD$() PLAY AN        */
                     /* INTRINSIC ROLE IN THE FILEOPEN ROUTINE AND     */
                     /* SHOULD NOT BE MODIFIED.                        */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            *  #1 ! USERINFO ! Default information for this user        *~
            *  #2 ! GLMAIN   ! General ledger master file (acct tests)  *~
            *  #3 ! HNYMASTR ! Inventory master file                    *~
            *  #4 ! HNYDETAL ! Inventory detail record file             *~
            *  #5 ! HNYADDTF ! Additions buffer for inventory           *~
            *  #6 ! GLDETAIL ! General ledger detail file               *~
            *  #7 ! SYSFILE2 ! System information file                  *~
            *  #8 ! HNYQUAN  ! Inventory store quantity file            *~
            * #10 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #11 ! HNYPOOL  ! Inventory pools file                     *~
            * #12 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #13 ! PIPMASTR ! Planned inv. position master             *~
            * #14 ! GLORTRAN ! GL Oracle transmit file     (AWD001)     *~
            * #15 ! GENCODES ! GENERAL CODES MASTER FILE   (AWD001)     *~
            * #21 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #50 ! WRKFILE  ! HNYPST2 work file                        *~
            * #51 ! COREWRK  ! Core Value Work File                     *~
            *************************************************************

            select  #1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1,  keylen = 3

            select  #2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #3, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos = 90, keylen = 4, dup

            select  #4, "HNYDETAL",      /* INVENTORY DETAIL FILE.     */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos = 1, keylen = 42,                         ~
                        alternate key 1, keypos = 43, keylen = 6, dup,   ~
                                  key 2, keypos = 49, keylen = 2, dup

            select  #5, "HNYADDTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 6

            select #6, "GLDETAIL",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 160,                                    ~
                       keypos = 1, keylen = 26

            select #7, "SYSFILE2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 500,                                    ~
                       keypos = 1, keylen = 20

            select #8, "HNYQUAN",                                        ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 650,                                    ~
                       keypos = 17, keylen = 44,                         ~
                       alternate key 1, keypos =  1, keylen = 44


            select #10, "SERTIF",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 62

            select #11,  "HNYPOOL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #12, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #13,  "PIPMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

/*(AWD001)*/

            select #14, "GLORTRAN",                                       ~
                        varc,     indexed, recsize = 512,                 ~
                        keypos = 1,    keylen = 33,                       ~
                        alt key 1, keypos = 31, keylen = 47,              ~
                            key 2, keypos = 81, keylen = 26


            select #15, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
/*(AWD001)*/


            select #21, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #50, "WRKFILE",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos =  1, keylen = 19                        ~

            select #51, "COREWRK",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos =  1, keylen = 19                        ~

            call "SHOSTAT"  ("Preparing To Post Direct Additions To Inven~
        ~tory On Hand")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))
/*(AWD001)*/
            call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))
            call "OPENFILE" (#15, "SHARE", f2%(15), rslt$(15), axd$(15))
/*(AWD001)*/

            call "OPENFILE" (#21, "SHARE", f2%(21), rslt$(21), axd$(21))
            call "WORKOPEN" (#50, "IO", 100%, f2%(50))

            REM IF DETAIL FILE NOT OPEN THEN OPEN IT
                if f2%(4) = 0 then L02000
                call "OPENFILE" (#4, "OUTPT", f2%(4), rslt$(4), axd$(4))
                close #4
                call "OPENFILE" (#4, "SHARE", f2%(4), rslt$(4), axd$(4))
L02000:                                                  /* (AWD001) */
                if f2%(14) = 0 then L09000
                call "OPENFILE" (#14, "OUTPT", f2%(14), rslt$(14), axd$(14))
                close #14
                call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * AS FAR AS INITIALIZATION AREAS GO, THIS ONE IS PRETTY HOT *~
            * BECAUSE WE'RE POSTING TO INVENTORY AND G/L AT THE SAME    *~
            * TIME, AS WELL AS DELETING STUFF OUT OF BUFFERS, AND A     *~
            * WHOLE LOT OF OTHER INTERESTING STUFF.  (I'm impressed ...)*~
            *************************************************************

                                                          /* (AWD001)  */

            schema_err%, schema% = 0%
            init(" ") schema$, division$
            call "SCHEMA" (schema$, schema%, #15, schema_err%)


            if schema% = 1% then division$ = "036"   /* NC */
            if schema% = 2% then division$ = "080"   /* NE */
            if schema% = 3% then division$ = "035"   /* AES*/

            sysdates% = 0%
            hnydte$ = date
            call "DATEFMT" (hnydte$)
            call "DATUNFMT" (hnydte$)
            hnydate$ = hnydte$

            REM SET SYSTEM DATE
                date$ = date

            REM SET KEY FOR ENTRIES TO READ OFF BUFFER.
REM                call "EXTRACT" addr("ID", userid$)

                nextitemkey$ = str(userid$) & hex(000000)

            REM RETRIEVE INVENTORY DATE FOR THIS USER
            if sysdates% = 0% then goto L09230
                call "READ100" (#1, userid$, f1%(1))
                      if f1%(1) = 0 then exit_program
                get #1, using L09200, hnydate$
L09200:                 FMT XX(27), CH(6)
                call "WHICHMON" (#7, hnydate$, whichmonth%)
                if whichmonth% < 1 or whichmonth% > 3 then exit_program
                rpthnydate$ = hnydate$  : call "DATEFMT" (rpthnydate$)

*        See if G/L Export is on
L09230:
            export_on$ = "N"  :  plowkey$ = "SWITCHS.GL"
            call "READ100" (#7, plowkey$, f1%(7))
            if f1%(7) = 1% then get #7 using L09232, export_on$
L09232:         FMT POS(22), CH(1)

            REM GET SYSFILE2 RECORD TO DETERMINE ACCOUNT FOR VARIANCE POST
                plowkey$ = "SWITCHS.HNY"
                call "READ100" (#7, plowkey$, f1%(7))
                      if f1%(7) = 0 then exit_program
                get #7, using L09280, hnyflg$
L09280:               FMT POS(96), CH(1)

*        Get System Suspense Account
                plowkey$ = "FISCAL DATES"
                call "READ100" (#7, plowkey$, f1%(7))
                      if f1%(7%) = 0% then L09380
                get #7, using L09340, suspense_acct$
L09340:              FMT POS(417), CH(16)

L09380:     REM SET GARBAGE FOR PRINTING.
                linenumber% = 1000
                pagenumber% = 0
L09410:      REM DO A GETPARM TO FIND JNLID$
REM                call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",~
                                     "HNYADD",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                jnlid$ = "IAD"
                if jnlid$ = " " then L09410

                modno$ = "04"
                tran_type$ = "IAD"
                returncode% = 0
                call "JNLINFO" (modno$, jnlid$, pstseq%, summary$,       ~
                              readkey$, hnydate$, #7, f2%(7), returncode%)
                title$, title1$ = readkey$
                call "FMTTITLE" (title$, "RECAP", 12%)
                call "FMTTITLE" (title1$, "JOURNAL", 12%)

*        See if Core is on
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#7, plowkey$, core_on%)
                if core_on% <> 1% then L09700
            get #7 using L09620, core_inv_flag$
L09620:         FMT POS(134), CH(1)
            if core_inv_flag$ <> "Y" then core_on% = 0%
            if core_on% <> 1% then L09700
                call "WORKOPEN" (#51, "IO", 100%, f2%(51%))


L09700:     call "SHOSTAT" ("Posting additions to inventory")

L10000: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * GETS SUCCESSIVE RECORDS OFF THE FILE, AND GOES THEREFROM  *~
            * TO POST TO G/L, INVENTORY, AND WHATEVER ELSE WE CAN THINK *~
            * OF.                                                       *~
            *************************************************************

            call "PLOWNEXT" (#5, nextitemkey$, 3%, f1%(5))
                 if f1%(5) = 0 then L40000
            call "HNYADJOB" (date$,      /* Current System Date        */~
                             hnydate$,   /* Inventory Posting Date     */~
                             #5,         /* ADDBUFFR record area       */~
                             #13,        /* PIPMASTR UFB               */~
                             #21)        /* SFCUM2   UFB               */
            gosub L30000                  /* GET INFORMATION & FORMAT   */
            x1% = 0%
            extension = round(quantity * totalcost, 2)

            REM POST STUFF IN ENTRY TO G/L STACKS.
                REM POST INVENTORY ACCOUNT DEBIT TO G/L
                    init (" ") passedin_acct$(), passedin_type$()
                    mat passedin_dbcr = zer
                    gltext$= jobnr$
                    str(gltext$,31)= str(part$) & str(store$) & lot$
                    str(gltext$,69)= descr$
                    passedin_acct$(1) = str(hnyacct$,1,9) & gltext$
                    passedin_acct$(2) = str(srceacct$,1,9) & gltext$
                    passedin_dbcr(1,1) = extension
                    passedin_dbcr(1,2) = 0
                    passedin_dbcr(2,1) = 0
                    passedin_dbcr(2,2) = extension
                    passedin_type$(1) = "01"
                    passedin_type$(2) = "02"
                    cntr% = 2

        rem**************************************************************~
           * the description for hnypost; which is eventually passed on *~
           * to the hnydetal file is working like this.                 *~
           * if the job number is not blank then we set the descr$ to   *~
           * read 'PART WITHDRAWN FROM JOB NUMBER 12345678'             *~
           * if the job is blank then the descr$ reads what was entered *~
           * by the operator in hnyaddns plus the purchase order number *~
           **************************************************************

           if jobnr$ = " " then L10440
             descr$ = "WITHDRAWN FROM PROJECT:" & jobnr$
             goto L10460

L10440:    if ponr$ = " " then L10460
             descr$ = str(descr$,1%,22%) & "/" &  ponr$

L10460:     REM POST QUANTITY ADDED TO QUANTITY ON HAND ("HNYPOST")
                call "HNYPST2" (part$,                                   ~
                                store$,                                  ~
                                lot$,              /* LOT NUMBER       */~
                                quantity,0,0,0,0,  /* POST TO ON HAND  */~
                                cost(),            /* Std costs        */~
                                totalcost,         /* Total std costs  */~
                                0,                 /* UNIT PRICE       */~
                                extension,                               ~
                                hnydate$,                                ~
                                "IA",              /* TRANSACTION TYPE */~
                                descr$,                                  ~
                                hnyacct$,                                ~
                                srceacct$, 3%, 2%, modno$, " ",          ~
                                pstseq%, gltext$, userid$, #8, #4, #7,   ~
                                #11, #3, #13, #21, #2, #6, #50, 0%, u3%)

            if core_on% <> 1% then L10620
                call "CORVALSB" ("IA", part$, store$, lot$, quantity,    ~
                                 hnydate$, hnyacct$, srceacct$, modno$,  ~
                                 jnlid$, pstseq%, gltext$, userid$, #8,  ~
                                 #7, #3, #51, u3%)
L10620:     gosub consolidate_array_and_post

            if jobnr$ <> " " then L10800
            call "LOTTRACK"                                              ~
                 ("D",             /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
                  descr$,          /* FROM PART                        */~
                  " ",             /* FROM LOT                         */~
                  " ",             /* FROM STORE                       */~
                  " ",             /* FROM WHATEVER                    */~
                  "H",             /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
                  part$,           /* TO PART (IF BLANK THEM SAME)     */~
                  lot$,            /* TO LOT                           */~
                  store$,          /* TO STORE                         */~
                  " ",             /* TO WHATEVER                      */~
                  quantity,        /* QUANTITY MOVED                   */~
                  #3,              /* 'HNYMASTR' FILE                  */~
                  #7)              /* 'SYSFILE2' FILE                  */

L10800:     procsrce$=" "
            if jobnr$<>" " then procsrce$="*" & jobnr$
            if procsrce$ <> " " then L10830
                if vendor$ = " " and ponr$ = " " then L10830
                    procsrce$ = str(vendor$) & "P" & str(ponr$)

L10830:     call "HNYPROCU" (part$, procsrce$, hnydate$, quantity,       ~
                             totalcost, hnydate$, #3, #7)
            REM PRINT JOURNAL FOR THIS ITEM.
                gosub L20000

            REM DO NEXT ENTRY.
                plowkey$ = "IA" & nextitemkey$
                gosub process_serial_numbers
                if core_on% <> 1% then L10888
                     gosub process_core_entries
                     if cntr% < 1% then L10888
                          x1% = 0%  :  coreprt% = 1%
                          gosub L20000
                          coreprt% = 0%
L10888:         call "DELETE" (#5, nextitemkey$, 6%)
                call "DELETE" (#10, plowkey$, 8%)
                goto L10000

        process_serial_numbers
            call "PLOWNEXT" (#10, plowkey$, 42%, f1%(10))
            if f1%(10) = 0% then return
            serial$ = str(plowkey$,43%)
            readkey$ = str(part$) & serial$
            call "READ101" (#12, readkey$, f1%(12))
            call "GETDTTM" addr(datetime$)
            put #12 using L35060,                                         ~
            "2",            /* Current Status Of a Serial Numbered Part*/~
            store$,         /* Warehouse or Store                      */~
            lot$,           /* Which lot in inventory                  */~
            " ",            /* Not used                                */~
            serial$,        /* Serial Number                           */~
            part$,          /* Part code                               */~
            serial$,        /* Serial number                           */~
            " ",            /* Job Number                              */~
            vendor$,        /* Vendor code                             */~
            " ",            /* Receiver Control Number                 */~
            ponr$,          /* Purchase Order Number                   */~
            " ",            /* Purchase Line Sequence Number (not ITEM */~
            " ",            /* Vendor Lot                              */~
            " ",            /* The specific BOM identifier for a Bill. */~
            " ",            /* The specific routing to use for a Bill. */~
            date$,          /* Date production job actually started    */~
            date$,          /* Date Added to System                    */~
            store$,         /* Warehouse or Store                      */~
            lot$,           /* Which lot in inventory                  */~
            datetime$,      /* Date & Time                             */~
            userid$,        /* user-id of specific user                */~
            hex(ffffffff),  /* Internal ID to text in TXTFILE.         */~
            "  ",           /* Transaction Type Code                   */~
            " ",            /* Transaction or Document Line Key value  */~
            " "             /* Filler (Internal, unused space)         */~

            if f1%(12) = 0% then write #12
            if f1%(12) = 1% then rewrite #12
            goto process_serial_numbers

                u3% = u3%

        load_gl_info

            put str(gl_post_info$(),,) using L13490,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Currency Units per Book    */~
                extension,               /* Functional Currency amount */~
                quantity,                /* Unit amount                */~
                " ",                     /* Customer code CH(9)        */~
                " ",                     /* Sales Order number CH(16)  */~
                " ",                     /* BOL number CH(3)           */~
                " ",                     /* Customer Type CH(2)        */~
                " ",                     /* State CH(2)                */~
                " ",                     /* Country CH(3)              */~
                " ",                     /* ZIP CH(9)                  */~
                " ",                     /* Sales Region CH(4)         */~
                " ",                     /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                " ",                     /* Salesman code CH(4)        */~
                " ",                     /* Invoice Number CH(8)       */~
                part$,                   /* Part Number CH(25)         */~
                partcat$,                /* Part Category CH(4)        */~
                partclass$,              /* Part Class CH(4)           */~
                partgen$,                /* Part Generic code CH(16)   */~
                parttype$,               /* Part Type CH(3)            */~
                uom$,                    /* Part UOM CH(4)             */~
                store$,                  /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                vendor$,                 /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                ponr$,                   /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                jobnr$,                  /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                " ",                     /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L13490: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Currency Units per Book    */~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
                CH(9),                   /* Customer code CH(9)        */~
                CH(16),                  /* Sales Order number CH(16)  */~
                CH(3),                   /* BOL number CH(3)           */~
                CH(2),                   /* Customer Type CH(2)        */~
                CH(2),                   /* State CH(2)                */~
                CH(3),                   /* Country CH(3)              */~
                CH(9),                   /* ZIP CH(9)                  */~
                CH(4),                   /* Sales Region CH(4)         */~
                CH(10),                  /* Sales Tax code CH(10)      */~
                CH(4),                   /* Shipping Region CH(4)      */~
                CH(4),                   /* Salesman code CH(4)        */~
                CH(8),                   /* Invoice Number CH(8)       */~
                CH(25),                  /* Part Number CH(25)         */~
                CH(4),                   /* Part Category CH(4)        */~
                CH(4),                   /* Part Class CH(4)           */~
                CH(16),                  /* Part Generic code CH(16)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice  CH(16)     */~
                CH(8),                   /* Check Payment Number CH(8) */~
                CH(8),                   /* Project code CH(8)         */~
                CH(8),                   /* Job number CH(8)           */~
                CH(4),                   /* Work Center CH(4)          */~
                CH(4),                   /* Activity code CH(4)        */~
                CH(12),                  /* Employee number CH(12)     */~
                CH(4),                   /* Department code CH(4)      */~
                CH(4),                   /* Cost Center CH(4)          */~
                CH(12),                  /* Earnings Type CH(12)       */~
                CH(12),                  /* Deduction Type CH(12)      */~
                CH(4),                   /* P/R Category CH(4)         */~
                CH(4),                   /* Labor class CH(4)          */~
                CH(191)                  /* Filler                     */

L20000: REM *************************************************************~
            *            P R I N T   J O U R N A L   E N T R Y          *~
            *                                                           *~
            * USE MULTI-COLUMN PRINT.  FORTUNATELY, SINCE THERE'S NOT A *~
            * OVERLAY ON THIS DOCUMENT, WE DON'T HAVE TO MESS AROUND    *~
            * WITH DUMMY ARGUMENTS, BUT UNFORTUNATELY, THE ROUTINE NEEDS*~
            * MORE COLUMNS THAN THE OTHER STUFF.                        *~
            *************************************************************

            mat linenumber% = con
            colsdone% = 0
            if linenumber% > 62 then linenumber% = 1000

L20130:     for column% = 1 to 5
                on column% gosub L21000, L22000, L23000, L24000, L25000
                next column%
                gosub L28000
                if colsdone% < 5 then L20210
                   REM PRINT SEPARATOR LINE, THEN RETURN.
                       print using L29160
                       return
L20210:         call "GLFMT" (prtacct$)
                print using L29190, prtpart$, prtqty$, prtcost$, prtacct$,~
                                   prtdebit$, prtcredit$, prtdescr$

                goto L20130

L21000:     REM HANDLES FIRST  COLUMN--PART NUMBER AND DESCRIPTION.
                prtpart$ = " "
                on linenumber%(1) gosub L21100, L21200, L21300
                   return
L21100:         REM FIRST  LINE--PART NUMBER
                    prtpart$ = part$
                    linenumber%(1) = 2
                    return
L21200:         REM SECOND LINE--DESCRIPTION
                    partreadkey$ = part$
                    call "DESCRIBE" (#3, partreadkey$, prtpart$, 0%,     ~
                                         f1%(3))
                    if f1%(3) = 0 then prtpart$ = "PART NOT ON FILE"
                    linenumber%(1) = 3
                    return
L21300:         REM THIRD  LINE--ZAP VARIABLES AND STUFF
                    prtpart$ = " "
                    prtpart$ = "JOB NUMBER: " & jobnr$
                    linenumber%(1) = 4
                    colsdone% = colsdone% + 1
                    return

L22000:    REM HANDLES SECOND COLUMN--ACCOUNT INFORMATION, IF ANY.
                x1% = x1% + 1%
                if passedin_acct$(x1%) = " " then L22110
                if passedin_dbcr(x1%,1) = 0 and                          ~
                     passedin_dbcr(x1%,2) = 0 then L22110
                call "CONVERT" (passedin_dbcr(x1%,1), 2.2, prtdebit$)
                call "CONVERT"(passedin_dbcr(x1%,2), 2.2, prtcredit$)
                prtacct$ = passedin_acct$(x1%)
                linenumber%(2) = 3
                return

L22110:     colsdone% = colsdone% + 1%
            prtdebit$ = " "
            prtcredit$ = " "
            prtacct$ = " "
            return

L23000:     REM HANDLES THIRD  COLUMN--TOTAL COST
                on linenumber%(3) gosub L23100, L23200, L23300, L23400
                   return
L23100:         REM FIRST  LINE--TOTAL COST -- Formerly LABOR cost -- JIM
                    call "CONVERT" (totalcost, 2.4, prtcost$)
                    linenumber%(3) = 2
                    return
L23200:         REM SECOND LINE--MATERIAL COST, OBSOLETE 05/12/87--JIM
                    prtcost$, prtcostdescr$ = " "
                    linenumber%(3) = 3
                    return
L23300:         REM THIRD  LINE--OVERHEAD COST, OBSOLETE 05/12/87--JIM
                    prtcost$, prtcostdescr$ = " "
                    linenumber%(3) = 4
                    return
L23400:         REM FOURTH LINE--ZAP VARIABLES
                    prtcost$, prtcostdescr$ = " "
                    linenumber%(3) = 5
                    colsdone% = colsdone% + 1
                    return

L24000:     REM HANDLES FOURTH COLUMN--DESCRIPTIVE TEXT INFORMATION.
                on linenumber%(4%) gosub L24100, L24200, L24300, L24400, L24500
                   return
L24100:         REM FIRST  LINE--WHERE FROM INFO, IF ANY
                    if vendor$ = " " then L24200
                    prtdescr$ = "VENDOR:"
                    str(prtdescr$, len(prtdescr$) + 2) = vendor$
                    call "PUTPAREN" (prtdescr$)
                    linenumber%(4) = 2
                    return
L24200:         REM SECOND LINE--DESCRIPTIVE TEXT, IF ANY.
                    if ponr$ = " " then L24300
                    prtdescr$ = "P.O. NUMBER: " & ponr$
                    linenumber%(4) = 3
                    return
L24300:         REM THIRD  LINE
                    prtdescr$ = descr$
                    if coreprt% <> 1% then L24360
                    linenumber%(4%) = 4%
                    goto L24380
L24360:             colsdone% = colsdone% + 1%
                    linenumber%(4%) = 5%
L24380:             return
L24400:       REM 4TH CASE -- Possible Core Value Message
                    prtdescr$ = " "
                    if coreprt% <> 1% then L24460
                    prtdescr$ = "Core Value:  G/L Transaction Only"
                    colsdone% = colsdone% + 1%
                    linenumber%(4%) = 5%
L24460:             return
L24500:       REM 5TH CASE -- VARIABLES ARE ZAPPED
                     prtdescr$ = " "
                     return
L25000:     REM HANDLE CASE TO PRINT QUANTITY AND STORE.
                on linenumber%(5) gosub L25100, L25200, L25300, L25400
                   return
L25100:         REM PRINT QUANTITY
                    if part$ = " " then L25400
                    call "CONVERT" (quantity, 2.2, prtqty$)
                    linenumber%(5) = 2
                    return
L25200:         REM PRINT STORE
                    prtqty$ = "STORE:"
                    str(prtqty$, 8) = store$
                    if coreprt% = 1% then str(prtqty$,8%,3%) = "***"
                    linenumber%(5) = 3
                    return
L25300:         REM PRINT LOT
                    prtqty$ = "LOT:" & lot$
                    if coreprt% = 1% then str(prtqty$,5%,6%) = "******"
                    linenumber%(5) = 4
                    return
L25400:        REM ZAP VARIABLES, INCREMENT COLSDONE%
                    prtqty$ = " "
                    linenumber%(5) = 5
                    colsdone% = colsdone% + 1
                    return

L28000:     REM PRINT PAGE CONTROL ROUTINE.
                select printer (134)
                linenumber% = linenumber% + 1
                if linenumber% < 59% then return
                   print page
                   pagenumber% = pagenumber% + 1
                   call "DATE" addr("HD", hdrdate$)
                   print using L29000, pagenumber%, title1$, hdrdate$
                   print "POST: " & rpthnydate$
                   print using L29040
                   print using L29070
                   print using L29100
                   print using L29130, "#"
                   print using L29160
                   linenumber% = 7
                   return

L29000: %PAGE #####        ##############################################~
        ~###################    ##########################################~
        ~###

L29040: %+--------------------------------+----------+-------------+-----~
        ~-----------------------------+-----------------------------------~
        ~-+

L29070: %!          PART  NUMBER          ! QUANTITY !             !     ~
        ~     ACCOUNT BREAKDOWN       !              WHERE FROM           ~
        ~ !

L29100: %!             AND                !  TO  BE  !             +-----~
        ~-------+----------+----------!                  AND              ~
        ~ !

L29130: %!          DESCRIPTION           !  ADDED   !   UNIT COST !ACCOU~
        ~NT #   !DEBIT  AMT!CREDIT AMT!           DESCRIPTIVE TEXT        ~
        ~ !

L29160: %+--------------------------------+----------+-------------+-----~
        ~-------+----------+----------+-----------------------------------~
        ~-+

L29190: %!################################!##########!  ########## !#####~
        ~#######!##########!##########!###################################~
        ~#!

L30000: REM *************************************************************~
            *     G E T   I N F O R M A T I O N   F R O M   D I S K     *~
            *                                                           *~
            * GETS INFORMATION FROM DISK AND PUTS IT INTO THE VARIOUS   *~
            * FIELDS.  THEN WE RETURN TO PROCESS THE HECK OUT OF IT.    *~
            *************************************************************

            get #5, using L35330,                                         ~
                    part$, store$, lot$, jobnr$, quantity, hnyacct$,     ~
                    srceacct$, cost(), vendor$, descr$, ponr$
            totalcost = cost( 1) + cost( 2) + cost( 3) + cost( 4) +      ~
                cost( 5) + cost( 6) + cost( 7) + cost( 8) + cost( 9) +   ~
                cost(10) + cost(11) + cost(12)
                if hnyflg$ = "V" or hnyflg$ = " " then L30260
                     call "HNYGLGET" (part$, store$, lot$, adjacct$, 6%, ~
                                      #3, #8)
                     if adjacct$ <> " " then L30260
                          adjacct$ = suspense_acct$

L30260:     if export_on$ = "Y" then gosub data_for_glexport
            return

*        Subroutine to get part data only if G/L Export is Active
        data_for_glexport
            call "READ100" (#3, part$, f1%(3%))
                if f1%(3%) = 0% then return /* Shouldn't happen */
            get #3 using L30460, partgen$, uom$, partcat$, partclass$,    ~
                                parttype$
L30460:         FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133),    ~
                    CH(4), POS(180), CH(3)
            return

        REM *************************************************************~
            * CALL TO CONSOLIDATE                                       *~
            *************************************************************

        consolidate_array_and_post
                first% = 1%  :  wf% = 50%
                plowwrk$  = all (hex(00))
L31050:         gosub read_work_file
                     if f1%(50%) = 0% and first% = 1% then L31260
                     if f1%(50%) = 0% then L31230
                     first% = 0%
                if hnyflg$ = "V" or hnyflg$ = " " then L31160
                chkacct$ = str(text$,1%,9%)
                if chkacct$ = hnyacct$ or chkacct$ = srceacct$ then      ~
                     L31160
                     text$ = str(adjacct$,1%,9%) & str(text$,10%,100%)
L31160:         cntr% = cntr% + 1%
                gosub load_gl_arrays
                    passedin_type$(cntr%) = "03"
                    if hnyflg$ = "A" then L31210
                    passedin_type$(cntr%) = "01"
                    if str(text$,76%,1%) = "H" then L31210
                    passedin_type$(cntr%) = "04"
                    if str(text$,74%,1%) = "S" then L31210
                    passedin_type$(cntr%) = "03"
L31210:         delete #50
                goto L31050

L31230:         call "GLCMBSUB" (passedin_acct$(), passedin_dbcr(),      ~
                                 passedin_type$(), #7, cntr%)

L31260:         gosub gl_post_loop
                return

        REM *************************************************************~
            * CALL TO PROCESS CORE ENTRIES, IF ANY                      *~
            *************************************************************

        process_core_entries
                cntr%, first% = 0%
                wf% = 51%
                init (" ") passedin_acct$(), passedin_type$()
                mat passedin_dbcr = zer
                plowwrk$  = all (hex(00))
L31600:         gosub read_work_file
                     if f1%(51%) = 0% and first% = 0% then return
                     if f1%(51%) = 0% then L31960
                     first% = 1%
                if cntr% > 0% then L31760
                     get #51, using L31690, part$, store$, lot$, quantity,~
                                           totalcost, extension
L31690:                   FMT POS(157), CH(25), CH(3), CH(16), 3*PD(14,4)
                     if export_on$ = "Y" then gosub data_for_glexport
L31760:         cntr% = cntr% + 1%
                gosub load_gl_arrays
                if cntr% > 2% then L31860
                    passedin_type$(cntr%) = "05"
                    if str(text$,76%,1%) = "H" then L31930
                    passedin_type$(cntr%) = "06"
                    goto L31930

L31860:             passedin_type$(cntr%) = "03"
                    if hnyflg$ = "A" then L31930
                    passedin_type$(cntr%) = "01"
                    if str(text$,76%,1%) = "H" then L31930
                    passedin_type$(cntr%) = "04"
                    if str(text$,74,1) = "S" then L31930
                    passedin_type$(cntr%) = "03"
L31930:         delete #51
                goto L31600

L31960:         call "GLCMBSUB" (passedin_acct$(), passedin_dbcr(),      ~
                                 passedin_type$(), #7, cntr%)

                gosub gl_post_loop
                return

        REM *************************************************************~
            * MISCELLANEOUS SUBROUTINES                                 *~
            *************************************************************

        read_work_file
            call "PLOWNXT1" (#wf%, plowwrk$, 0%, f1%(wf%))
                     if f1%(wf%) = 0% then return

            get #wf%, using L32090, text$, dbamt, cramt
L32090:         FMT XX(25), CH(109), 2*PD(14,4)
            return

        load_gl_arrays
            passedin_acct$(cntr%) = text$
            passedin_dbcr(cntr%,1%) = dbamt
            passedin_dbcr(cntr%,2%) = cramt
            return


        REM *************************************************************~
            *  Prepare the Accumulated Arrays for G/l Posting           *~
            *************************************************************
        gl_post_loop
            if export_on$ = "Y" then gosub load_gl_info
            for x% = 1% to 50%
                if passedin_acct$(x%) = " " then goto L33220
                acct$(1) = str(passedin_acct$(x%),,9)
                acct$(2) = acct$(1)
                gltext$  = str(passedin_acct$(x%),10)
                glamount(1) = passedin_dbcr(x%,1)
                glamount(2) = passedin_dbcr(x%,2)
                gosub'163(acct$(2), glamount(2))
                gosub'162(acct$(1), glamount(1))
                str(tran_type$,4,2) = passedin_type$(x%)
                iamt = glamount(1) - glamount(2)
                gosub post_gl
            next x%

L33220:     return

        REM *************************************************************~
            *         C O M M O N   G / L   P O S T   L O G I C         *~
            *                                                           *~
            * ACCT$(), GLTEXT$, and GLAMOUNT must be set prior to this. *~
            *************************************************************

        post_gl

            if export_on$ = "Y" then                                     ~
                put gl_post_info$() using L33330, tran_type$, iamt
L33330:              FMT CH(5), POS(18), PD(15,4)

            REM Account in ACCT$(1) is debited...

            if glamount(1) = 0 then L33550

            call "GLPOST2" (acct$(1),    /* ACCOUNT TO BE UPDATED      */~
                      glamount(1),       /* DEBIT AMOUNT (0 IF CREDIT) */~
                      0,                 /* CREDIT AMOUNT (0 IF DEBIT) */~
                      hnydate$,          /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      modno$,            /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      division$,         /* (AWD001) Division          */~
                      #2,                /* UFB ADDRESS OF G/L MAIN    */~
                      #6,                /* UFB ADDRESS OF G/L DETAILS */~
                      #7,                /* UFB ADDRESS OF SYSFILE2    */~
                      #14,               /* (AWD001) GLORTRAN          */~
                      returncode%,       /* ERROR RETURN FROM SUBROUTIN*/~
                      " ", gl_post_info$())

L33550:     REM Account in ACCT$(2), ...  is credited...
            for credit% = 2% to 4%
               if acct$(credit%) = " " then L33760
               if glamount(credit%) = 0 then L33760

            call "GLPOST2" (acct$(credit%),   /* ACCOUNT TO BE UPDATED */~
                      0,                 /* DEBIT AMOUNT (0 IF CREDIT) */~
                      glamount(credit%), /* CREDIT AMOUNT (0 IF DEBIT) */~
                      hnydate$,          /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      modno$,            /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      division$,         /* (AWD001) Division          */~
                      #2,                /* UFB ADDRESS OF G/L MAIN    */~
                      #6,                /* UFB ADDRESS OF G/L DETAILS */~
                      #7,                /* UFB ADDRESS OF SYSFILE2    */~
                      #14,               /* (AWD001) GLORTRAN          */~
                      returncode%,       /* ERROR RETURN FROM SUBROUTIN*/~
                      " ", gl_post_info$())

L33760:     next credit%

        return


        REM *************************************************************~
            *                  F I L E   F O R M A T S                  *~
            *-----------------------------------------------------------*~
            *  File Layout for SERMASTR.                                *~
            *************************************************************

L35060: FMT                 /* FILE: SERMASTR                          */~
            CH(1),          /* Current Status Of a Serial Numbered Part*/~
            CH(3),          /* Inventory Store                         */~
            CH(16),         /* Inventory Lot                           */~
            CH(11),         /* Filler                                  */~
            CH(20),         /* Serial Number                           */~
            CH(25),         /* Part code                               */~
            CH(20),         /* Serial Number                           */~
            CH(8),          /* Job Number                              */~
            CH(9),          /* Vendor code                             */~
            CH(16),         /* Receiver Control Number                 */~
            CH(16),         /* Purchase Order Number                   */~
            CH(3),          /* Purchase Line Sequence Number (not ITEM */~
            CH(16),         /* Lot Number                              */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(3),          /* The specific routing to use for a Bill. */~
            CH(6),          /* Date production job actually started    */~
            CH(6),          /* Date Job Completed                      */~
            CH(3),          /* Warehouse or Store                      */~
            CH(16),         /* Which lot in inventory - always used wit*/~
            CH(7),          /* Date & Time record was setup on the syst*/~
            CH(3),          /* user-id of specific user                */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(2),          /* Transaction Type Code                   */~
            CH(40),         /* Transaction or Document Line Key value  */~
            CH(43)          /* Filler (Internal, unused space)         */~

L35330:     FMT XX(6), /* File #5 HNYADDTF--SKIP KEY INFORMATION       */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                CH(25),                  /* PART NUMBER                */~
                CH(3),                   /* STORE NUMBER               */~
                CH(16),                  /* LOT NUMBER                 */~
                CH(8),                   /* JOB NUMBER                 */~
                PD(14,4),                /* QUANTITY                   */~
                CH(9),                   /* INVENTORY ACCOUNT          */~
                CH(9),                   /* SOURCE (BUYING) ACCT       */~
                12*PD(14,4),             /* COSTS                      */~
                CH(09),                  /* VENDOR CODE                */~
                CH(32),                  /* DESCRIPTION FIELD          */~
                CH(16)                   /* PURCHASE ORDER NUMBER      */~

L40000: REM *************************************************************~
            *         P R I N T   D A I L Y   R E C A P   I N F O       *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.  SHOWS OFF JUST FOR FUN.  WILL NOT PRINT *~
            * ANYTHING WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY *~
            *************************************************************

            if debitsptr% = 0 and creditsptr% = 0 then exit_program
               totaldebits, totalcredits, colsdone% = 0
               mat rcpline% = con
               mat rcpptr% = zer
               gosub L48000     /* SKIP TO TOP OF PAGE.                 */

L40105:     for column% = 1 to 2
                on column% gosub L41000, L42000
                next column%
                print                    /* FREE UP LINE.              */
            if  colsdone% >= 2 then exit_program          /* DONE */
                goto L40105

L41000:     REM HANDLES LEFT (DEBITS) COLUMN FOR REPORT
                on rcpline%(1) gosub L41100, L41200, L41300, L41400, L41500
                return

L41100:         REM PRINT CONTENTS OF DEBITS STACK, IF ANY.
                    if debitsptr% = 0 then L41300
                    rcpptr%(1) = rcpptr%(1) + 1
                    rcpamt(1)=debitsstk(rcpptr%(1))
                    rcpacct$(1)=debitsstk$(rcpptr%(1))
                    REM GET ACCOUNT DESCRIPTION
                        call "DESCRIBE" (#2,rcpacct$(1),rcpacctdescr$(1),~
                                                   0%, f1%(2))
                    call "GLFMT" (rcpacct$(1))
                    print using L49050,rcpacct$(1),rcpacctdescr$(1),      ~
                                         rcpamt(1);
                    totaldebits=totaldebits+debitsstk(rcpptr%(1))
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
L41200:         REM PRINTS SEPARATOR LINE.
                    print using L49025, "*--";
                    rcpline%(1) = 3
                    return
L41300:         REM PRINTS TOTAL LINE
                    print using L49060, totaldebits;
                    rcpline%(1) = 4
                    return
L41400:         REM PRINTS STARS
                    print using L49020, "*";
                    rcpline%(1) = 5
                    return
L41500:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    rcpacct$(1), rcpacctdescr$(1) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L42000:     REM HANDLES RIGHT HAND COLUMN--CREDITS.
                print tab(70);
                on rcpline%(2) gosub L42100, L42200, L42300, L42400, L42500
                   return
L42100:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L42300
                    rcpptr%(2) = rcpptr%(2) + 1
                    rcpamt(2)=creditsstk(rcpptr%(2))
                    rcpacct$(2)=creditsstk$(rcpptr%(2))
                    call "DESCRIBE" (#2, rcpacct$(2), rcpacctdescr$(2),  ~
                                             0%, f1%(2))
                    call "GLFMT" (rcpacct$(2))
                    print using L49050,rcpacct$(2),rcpacctdescr$(2),      ~
                               rcpamt(2);
                    totalcredits=totalcredits+creditsstk(rcpptr%(2))
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L42200:         REM PRINT SEPARATOR LINE
                    print using L49025, "*--";
                    rcpline%(2) = 3
                    return
L42300:         REM PRINT TOTAL CREDITS LINE
                    print using L49061, totalcredits;
                    rcpline%(2) = 4
                    return
L42400:         REM PRINT STARS
                    print using L49020,"*";
                    rcpline%(2) = 5
                    return
L42500:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

L48000:     REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP
                select printer (134)
                   call "DATE" addr ("HD", hdrdate$)
                   print page
                   rcppage% = 1
                   print using L49080, rcppage%, title$, hdrdate$
                   print "POST: " & rpthnydate$
                   print using L49120
                   print
                   print using L49160
                   print using L49200,"#","#"
                   print using L49240
                   return

L49020: %**************************#***********************************
L49025: %#--------------+--------------------------------+------------*
L49050: %* ############ ! ############################## !-#######.## *
L49060: %*              ! TOTAL DEBITS                   !-#######.## *
L49061: %*              ! TOTAL CREDITS                  !-#######.## *

L49080: %PAGE #####        ##############################################~
        ~#################      ##########################################~
        ~###

L49120: %=========================D E B I T S==========================  ~
        ~     ===========================C R E D I T S====================~
        ~==

L49160: %**************************************************************  ~
        ~     ************************************************************~
        ~**

L49200: %* ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT   *  ~
        ~     *    ACCOUNT # !     D E S C R I P T I O N      !   AMOUNT  ~
        ~ *

L49240: %*--------------+--------------------------------+------------*  ~
        ~     *--------------+--------------------------------+-----------~
        ~-*

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(account$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  search str(debitsstk$(),1) = str(account$)             ~
                               to location$ step 9 /* FIND ACCOUNT #   */
                  if location$ = hex(0000) then L50150  /* PUSH NEW ITEM*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH CELL?  */
                     debitsstk(junk%) = debitsstk(junk%) + amount
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     return
L50150:           REM PUSH NEW ITEM ONTO STACK.
                      debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = account$
                      debitsstk(debitsptr%) = amount
                      return

            deffn'163(account$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  search str(creditsstk$(),1) = str(account$)            ~
                               to location$ step 9
                               /* SCAN ONLY WHAT IS USED OF STACK      */
                  if location$ = hex(0000) then L50300  /* IF NO ==> NEW*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH ELEMENT*/
                     creditsstk(junk%) = creditsstk(junk%) + amount
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     return
L50300:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = account$
                      creditsstk(creditsptr%) = amount
                      return

        REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES THE OPEN FILES, AND DISPLAYS MESSAGE.              *~
            *************************************************************
        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "JNLCLOSE" (modno$, jnlid$, pstseq%, returncode%)
            call "FILEBGON" (#50)
            if core_on% = 1% then call "FILEBGON" (#51)
            end
