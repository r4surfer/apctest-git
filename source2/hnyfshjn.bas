        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y  FFFFF   SSS   H   H  JJJJJ  N   N   *~
            *  H   H  NN  N  Y   Y  F      S      H   H    J    NN  N   *~
            *  HHHHH  N N N   YYY   FFFF    SSS   HHHHH    J    N N N   *~
            *  H   H  N  NN    Y    F          S  H   H  J J    N  NN   *~
            *  H   H  N   N    Y    F       SSS   H   H   J     N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYFSHJN - Posts HNYFLUSH's potential additions and       *~
            *            Withdrawals to inventory and G/L and prints a  *~
            *            Journal. In the case of Cost Method 'C',       *~
            *            accumulates Parent Part (Assembly) cost for    *~
            *            posting.                                       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/06/93 ! Original (cloned fm HNYWDWJN @ R6.02.04  ! JIM *~
            *          !   and modified as per W.P. doc. 0222x).  ! JIM *~
            * 03/09/94 ! Corrected accounts & values for parent.  ! JDH *~
            * 06/25/96 ! Deleted obscure date check               ! DER *~
            *************************************************************

        dim                                                              ~
            account$9,                   /* DUMMY ARG TO STACK ROUTINES*/~
            acct$(4)9,                   /* Array for GL Posting Info  */~
            cost(12),                    /* HNYFSHTF costs             */~
            costmeth$1,                  /* Parent Part Cost Method    */~
            core_inv_flag$1,             /* Core Value Inv Trans Flag  */~
            creditsstk$(250)9,           /* CREDITS FOR RCP#1 PHASE    */~
            creditsstk (250) ,           /* CREDIT AMOUNTS FOR RCP#1   */~
            date$8,                      /* DATE (FORMATTED)           */~
            datetime$7,                  /* Date Time Stamp            */~
            debitsstk$(250)9,            /* DEBIT ACCTS FOR RCP#1 PHASE*/~
            debitsstk (250),             /* DEBIT AMOUNTS FOR RCP#1    */~
            descr$32,                    /* DESCRIPTION FROM ENTRIES   */~
            destacct$9,                  /* DESTINATION(EXPENSE) ACCT #*/~
            from$16,                     /* WHERE FROM INFORMATION     */~
            glamount(4),                 /* Array for GL Posting Info  */~
            gltext$100,                  /* GL TEXT STRING             */~
            hdrdate$45,                  /* FORMATTED DATE/TIME INFO   */~
            hnyacct$9,                   /* INVENTORY ACCOUNT NUMBER   */~
            hnydate$6,                   /* USER'S INVENTORY DATE      */~
            jnlid$3,                     /* JOURNAL ID                 */~
            jobnr$8,                     /* JOB NUMBER FROM BUFFER     */~
            linenumber%(5),              /* LINE POINTERS FOR JURN1    */~
            location$2,                  /* LOCATOR ARRAY FOR STACKS   */~
            lot$16,                      /* LOT NUMBER                 */~
            modno$2,                     /* MODULE NUMBER              */~
            nextitemkey$6,               /* NEXT LINE ITEM TO READ     */~
            parentcost(12),              /* Parent cost array          */~
            part$25,                     /* PART NUMBER TO DO          */~
            partreadkey$50,              /* READ KEY FOR PLOWS         */~
            passedin_acct$(50)109,       /* Array For Single Entry     */~
            passedin_dbcr(50,2),         /* Array For Single Entry     */~
            passedin_type$(50)2,         /* Array For Single Entry     */~
            plowkey$64,                  /* File Plow Key Variable     */~
            plowwrk$64,                  /* File Plow Key Variable     */~
            prtacct$16,                  /* PRINT ROUTINE ACCOUNT# COPY*/~
            prtcost$10,                  /* L, M, O COSTS FOR PRINTING */~
            prtcostdescr$2,              /* "L:", "M:", "O:" DEPENDING */~
            prtcredit$10,                /* CREDIT AMOUNT TO PRINT     */~
            prtdebit$10,                 /* DEBIT AMOUNT TO PRINT      */~
            prtdescr$32,                 /* DESCRIPTION/WHERE FROM PRT */~
            prtpart$32,                  /* PART #/DESCR TO PRINT      */~
            prtqty$10,                   /* QUANTITY ADDED TO PRINT    */~
            r_a$1,                       /* 'R'equested/'A'ctual Usage */~
            rcpacct$(2)16,               /* ACCOUNT NUMBERS FOR RECAP  */~
            rcpacctdescr$(2)30,          /* ACCOUNT DESCRIPTIONS-RECAP */~
            rcpamt(2),                   /* RECAP AMOUNTS              */~
            rcpline%(2),                 /* LINE POINTERS FOR RECAP    */~
            rcpptr%(2),                  /* POINTER INTO D & C ARRAYS  */~
            readkey$50,                  /* File Read Key Variable     */~
            record$100,                                                  ~
            rectype$1,                   /* 'C'omponent or 'P'arent?   */~
            rpthnydate$8,                /* Reports Posting Date       */~
            serial$20,                   /* Serial Number to Add       */~
            store$3,                     /* STORE NUMBER               */~
            summary$1,                   /* SUMMARY INDICATOR          */~
            tempacct$9,                  /* Temporary account for swap */~
            text$109,                    /* Text for G/L Arrays        */~
            time$8,                      /* TOD for EOR                */~
            title$70,                    /* JOURNAL TITLE              */~
            title1$70,                   /* JOURNAL TITLE              */~
            usedate$6,                   /* Usage Capture Date (unfor) */~
            useseq$3,                    /* Usage Capture Line Seg. #  */~
            useso$16,                    /* Usage Capture S/O Number   */~
            usetype$5,                   /* Usage Capture Type Code    */~
            userid$3,                    /* USERID THIS USER           */~
            whichpass$1                  /* Posting Inv or Core Value  */

        dim                              /* G/L Export Posting Info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            partcat$4,                   /* Part Category code         */~
            partclass$4,                 /* Part Class code            */~
            partgen$16,                  /* Part Generic code          */~
            parttype$3,                  /* Part Type code             */~
            tran_type$5,                 /* G/L Transaction type       */~
            uom$4                        /* Part Unit of measure       */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.  THEY ARE AN INTRINSIC PART OF THE   */
                     /* FILEOPEN ROUTINE.                              */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            *  #1 ! USERINFO ! Default information for this user        *~
            *  #2 ! GLMASTR  ! General ledger master file (acct tests)  *~
            *  #3 ! HNYMASTR ! Inventory master file                    *~
            *  #4 ! HNYDETAL ! Inventory detail record file             *~
            *  #5 ! HNYFSHTF ! Add/Withdrawal buffer for inventory      *~
            *  #6 ! GLDETAIL ! General ledger detail file               *~
            *  #7 ! SYSFILE2 ! System information file                  *~
            *  #8 ! HNYQUAN  ! Inventory store quantity file            *~
            *  #9 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #10 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #11 ! HNYPOOL  ! Inventory pool files                     *~
            * #12 ! PIPOUT   ! Planned withdrawal                       *~
            * #13 ! PIPMASTR ! Planned inv. position master             *~
            * #14 ! JOBMASTR ! Project master file                      *~
            * #15 ! JOBMTLDR ! Project materials file                   *~
            * #21 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #22 ! WRKFILE  ! HNYPST2 Work File                        *~
            * #23 ! COREWRK  ! Core Value Work File                     *~
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
                                  key 2, keypos = 90,  keylen = 4, dup

            select  #4, "HNYDETAL",      /* INVENTORY DETAIL FILE.     */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos = 1, keylen = 42,                         ~
                        alternate key 1, keypos = 43, keylen = 6, dup,   ~
                                  key 2, keypos = 49, keylen = 2, dup

            select  #5, "HNYFSHTF",                                      ~
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
                       keypos =17, keylen = 44,                          ~
                       alternate key 1, keypos =  1, keylen = 44


            select #9,  "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

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

            select #13,  "PIPMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

            select #21, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #12,  "PIPOUT",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 64,                                   ~
                         keypos = 1, keylen = 56,                        ~
                         alternate key 1, keypos = 20, keylen = 37

            select #14,  "JOBMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 8

            select #15,  "JOBMTLDR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 16

            select # 22, "WRKFILE",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 19                         ~

            select #23, "COREWRK",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos =  1, keylen = 19                        ~

            call "SHOSTAT"  ("Opening Files; One Moment Please.")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
            call "OPENFILE" (# 9, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#21, "SHARE", f2%(21), rslt$(21), axd$(21))
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))
            call "OPENFILE" (#15, "SHARE", f2%(15), rslt$(15), axd$(15))
            call "WORKOPEN" (#22, "IO   ", 10%, f2%(22))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * AS FAR AS INITIALIZATION AREAS GO, THIS ONE IS PRETTY HOT *~
            * BECAUSE WE'RE POSTING TO INVENTORY AND G/L AT THE SAME    *~
            * TIME, AS WELL AS DELETING STUFF OUT OF BUFFERS, AND A     *~
            * WHOLE LOT OF OTHER INTERESTING STUFF. (Whoopee ...)       *~
            *************************************************************


            REM SET SYSTEM DATE
                date$ = date

            REM SET KEY FOR ENTRIES TO READ OFF BUFFER.
                call "EXTRACT" addr("ID", userid$)
                nextitemkey$ = str(userid$) & hex(000000)

            REM RETRIEVE INVENTORY DATE FOR THIS USER
                call "READ100" (#1, userid$, f1%(1))
                      if f1%(1) = 0 then exit_program
                get #1, using L09210, hnydate$
L09210:                 FMT XX(27), CH(6)
                call "WHICHMON" (#7, hnydate$, whichmonth%)
                if whichmonth% < 1 or whichmonth% > 3 then exit_program
                rpthnydate$ = hnydate$  : call "DATEFMT" (rpthnydate$)

            REM SET PAGE CONTROL VARIABLES FOR PRINTING
                linenumber% = 1000
                pagenumber% = 0
L09290:      REM DO A GETPARM TO FIND JNLID$
                call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",~
                                     "HNYFSH",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                if jnlid$ = " " then L09290

                modno$ = "03"
                returncode% = 0
                call "JNLINFO" (modno$, jnlid$, pstseq%, summary$,       ~
                      readkey$, hnydate$, #7, f2%(7), returncode%)
                title$, title1$ = readkey$
                call "FMTTITLE" (title$, "RECAP", 12%)
                call "FMTTITLE" (title1$, "JOURNAL", 12%)

            call "SHOSTAT" ("Posting BackFlush Inventory Additions & With~
        ~drawals")

*        See if G/L Export is on
            export_on$ = "N"  :  plowkey$ = "SWITCHS.GL"
            call "READ100" (#7, plowkey$, f1%(7%))
            if f1%(7%) = 1% then get #7 using L09520, export_on$
L09520:         FMT POS(22), CH(1)

*        See if Core is on
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#7, plowkey$, core_on%)
                if core_on% <> 1% then L09640
            get #7 using L09590, core_inv_flag$
L09590:         FMT POS(134), CH(1)
            if core_inv_flag$ <> "Y" then core_on% = 0%
            if core_on% <> 1% then L09640
                call "WORKOPEN" (#23, "IO", 100%, f2%(23%))

L09640
*        Set-up Export Tran Type
            tran_type$ = "MFL"

L10000: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * GETS SUCCESSIVE RECORDS OFF THE FILE, AND GOES THEREFROM  *~
            * TO POST TO G/L, INVENTORY, DEBIT AND CREDIT STACKS.       *~
            *************************************************************

            call "PLOWNEXT" (#5, nextitemkey$, 3%, f1%(5%))
                 if f1%(5%) = 0% then L40000
            gosub L30000                  /* GET INFORMATION & FORMAT   */
            x1% = 0%
*        Based on record type, process the Parent OR Component.
            if rectype$ <> "P" then goto L10128
                gosub L12000                      /* Process the Parent */
                extension = round(quantity * parenttotalcost, 2)
                goto L10590
L10128:     gosub L11000                      /* Process the Components */
            extension = round(quantity * totalcost, 2)

            if jobnr$=" " then L10590
            if f2%(14%) <> 0% then L10360
                call "JMTDPOST"(jobnr$,  /* Job  to be updated         */~
                      part$,             /* Part number to post        */~
                      store$,            /* Store number               */~
                      lot$,              /* Lot number                 */~
                      hnydate$,          /* Date part moved to job     */~
                      quantity,          /* Quantity moved to job      */~
                      totalcost,         /* Cost of Item               */~
                      extension,         /* Total cost of item         */~
                      #14,               /* UFB Address of JOBMASTR    */~
                      #15,               /* UFB Address of JOBMTLDR    */~
                      #8,                /* UFB Address of HNYQUAN     */~
                      #3,                /* UFB Address of HNYMASTR    */~
                      #7,                /* UFB Address of SYSFILE2    */~
                      userid$,           /* Current User ID            */~
                      returncode%)       /* Error return from subroutin*/~
                                         /* 0  = Record posted         */~
                                         /* 99 = Record *not* posted   */~

                if returncode%<>0% then L10590
L10360:     pquantity=quantity
            put str(record$,1%,48%) using L10390, "JOB(PROJ): ", jobnr$,  ~
                part$, hex(00000000)
L10390:     FMT CH(11), CH(8), CH(25), CH(4)
L10400:     call "PLOWNXT1" (#12, record$, 44%, f1%(12%))
            if f1%(12%) = 0% then L10590
            get #12, using L10430, record$
L10430:          FMT CH(64)
            get str(record$) using L10450, indate%, pipquantity
L10450:          FMT XX(44), BI(4), XX(8), PD(14,4)
            remquantity=max(round(pipquantity-pquantity,2),0)
            if remquantity>0 then L10540
            delete #12
            call "PIPFLAGS"(part$,1%,indate%,pipquantity,#13,#21)
            pquantity=round(pquantity-pipquantity,2)
            if pquantity>0 then L10400
            goto L10590

L10540:     put str(record$,57%,8%), using L10550, remquantity
L10550:              FMT PD(14,4)
                rewrite #12, using L10430, record$
                call "PIPFLAGS" (part$,1%,indate%,pquantity,#13,#21)

L10590:     REM POST STUFF IN ENTRY TO G/L STACKS.
            REM IF HNYACCT$ = " " OR DESTACCT$ = " " THEN 10570
                REM POST INVENTORY ACCOUNT CREDIT TO G/L
                    init (" ") passedin_acct$(), passedin_type$()
                    mat passedin_dbcr = zer
                    gltext$= jobnr$
                    str(gltext$,31%)= str(part$) & str(store$) & lot$
                    str(gltext$,69%)= descr$
                    passedin_acct$(1%)   = str(destacct$,1%,9%) & gltext$
                    passedin_acct$(2%)   = str(hnyacct$,1%,9%)  & gltext$
                    passedin_dbcr(1%,1%) = extension
                    passedin_dbcr(1%,2%) = 0
                    passedin_dbcr(2%,1%) = 0
                    passedin_dbcr(2%,2%) = extension
                    passedin_type$(1%) = "02"
                    passedin_type$(2%) = "01"
                    cntr% = 2%

                    gosub process_hnypst2_work_file
                    whichpass$ = "H"
                    gosub gl_post_loop

            REM PRINT JOURNAL FOR THIS ITEM.
                gosub L20000
                whichpass$ = " "

            REM DO NEXT ENTRY.
                plowkey$ = "IW" & nextitemkey$
                gosub process_serial_numbers
                if core_on% <> 1% then L10950
                     whichpass$ = "C"
                     gosub process_core_entries
                     if cntr% < 1% then L10940
                          x1% = 0%
                          gosub L20000
L10940:                   whichpass$ = " "
L10950:         call "DELETE" (#5, nextitemkey$, 6%)
                call "DELETE" (#10, plowkey$, 8%)
                goto L10000

L11000: REM Subtract Component quantity withdrawn from quantity on hand.
            gosub usage_capture
            call "HNYPST2" (part$, store$, lot$, -quantity, 0, 0, 0, 0,  ~
                cost(), totalcost, 0, 0, hnydate$, "JK", descr$,         ~
                hnyacct$, destacct$, 3%, 0%, modno$, jnlid$, pstseq%,    ~
                gltext$, userid$, #8, #4, #7, #11, #3, #13, #21, #2, #6, ~
                #22, 1%, u3%)

            if core_on% <> 1% then L11420
                str(gltext$,69%) = descr$
                call "CORVALSB" ("IW", part$, store$, lot$, -quantity,   ~
                     hnydate$, destacct$, hnyacct$, modno$, jnlid$,      ~
                     pstseq%, gltext$, userid$, #8, #7, #3, #23, u3%)

L11420:     call "LOTTRACK" ("H", part$, lot$, store$, " ", "D",         ~
                str(descr$,,25%), str(descr$,26%,6%), str(descr$,32%,1%),~
                " ", quantity, #3, #7)
            return

        process_serial_numbers
            call "PLOWNEXT" (#10, plowkey$, 42%, f1%(10))
            if f1%(10) = 0% then return
            serial$ = str(plowkey$,43%)
            readkey$ = str(part$) & serial$
            call "READ101" (#9, readkey$, f1%(9%))
            if f1%(9%) = 0% then process_serial_numbers
            call "GETDTTM" addr(datetime$)
            put #9 using L11800,                                          ~
            "0",            /* Current Status Of a Serial Numbered Part*/~
            descr$,         /* Reason / Description                    */~
            store$,         /* Warehouse or Store                      */~
            lot$,           /* Which lot in inventory                  */~
            datetime$,      /* Date & Time                             */~
            userid$,        /* user-id of specific user                */~
            "  ",           /* Transaction Type Code                   */~
            " "             /* Transaction or Document Line Key value  */

            rewrite #9
            goto process_serial_numbers

L11800:     FMT CH(1), CH(30), POS(183), CH(3), CH(16), CH(7), CH(3),    ~
                CH(2), CH(40)

L12000: REM Add Parent quantity to quantity on hand.
            if costmeth$ = "C" then goto L12020
                mat parentcost = cost
                parenttotalcost = totalcost
L12020:     call "HNYPST2" (part$, store$, lot$, quantity, 0, 0, 0, 0,   ~
                parentcost(), parenttotalcost, 0, 0, hnydate$, "JC",     ~
                descr$, hnyacct$, destacct$, 3%, 0%, modno$, jnlid$,     ~
                pstseq%, gltext$, userid$, #8, #4, #7, #11, #3, #13, #21,~
                #2, #6, #22, 1%, u3%)

            if core_on% <> 1% then L12420
                str(gltext$,69%) = descr$
                call "CORVALSB" ("IW", part$, store$, lot$, quantity,    ~
                     hnydate$, destacct$, hnyacct$, modno$, jnlid$,      ~
                     pstseq%, gltext$, userid$, #8, #7, #3, #23, u3%)

L12420:     call "LOTTRACK" ("D", str(descr$,,25%), str(descr$,26%,6%),  ~
                str(descr$,32%,1%), " ", "H", part$, lot$, store$, " ",  ~
                quantity, #3, #7)
            return

        load_gl_info

            put str(gl_post_info$(),,) using L13490,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Transaction Currency amount*/~
                0,                       /* Functional Currency amount */~
                -quantity,               /* Unit amount                */~
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
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
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

        usage_capture             /* Post Quantity Used to Usage Files */
            r_a$ = "A"                                 /* Actual Usage */
            usetype$ = "PROD"                      /* Production Usage */
            usedate$ = hnydate$
            call "HNYUSESB" (useso$, useseq$, store$, part$, r_a$,       ~
                             usedate$, usetype$, quantity)
            return

        process_hnypst2_work_file
                wf% = 22%
                plowkey$  = all (hex(00))
L14530:         gosub read_work_file
                     if f1%(22%) = 0% then L14780
                cntr% = cntr% + 1%
                gosub load_gl_arrays
                    passedin_type$(cntr%) = "01"
                    if str(text$,76,1) = "H" then L14710
                    passedin_type$(cntr%) = "04"
                    if str(text$,74,1) = "S" then L14710
                    passedin_type$(cntr%) = "03"
L14710:         delete #22
                goto L14530

L14780:         return

        REM *************************************************************~
            * CALL TO PROCESS CORE ENTRIES, IF ANY                      *~
            *************************************************************

        process_core_entries
                cntr%, first% = 0%
                wf% = 23%
                init (" ") passedin_acct$(), passedin_type$()
                mat passedin_dbcr = zer
                plowwrk$  = all (hex(00))
L14900:         gosub read_work_file
                     if f1%(23%) = 0% and first% = 0% then return
                     if f1%(23%) = 0% then L15200
                     first% = 1%
                if cntr% > 0% then L14990
                     get #23, using L14970, part$, store$, lot$, quantity,~
                                           totalcost, extension
L14970:                   FMT POS(157), CH(25), CH(3), CH(16), 3*PD(14,4)
                     quantity = abs(quantity)
                     if export_on$ = "Y" then gosub data_for_glexport
L14990:         cntr% = cntr% + 1%
                gosub load_gl_arrays
                if cntr% > 2% then L15090
                    passedin_type$(cntr%) = "05"
                    if str(text$,76%,1%) = "H" then L15140
                    passedin_type$(cntr%) = "06"
                    goto L15140

L15090:             passedin_type$(cntr%) = "01"
                    if str(text$,76%,1%) = "H" then L15140
                    passedin_type$(cntr%) = "04"
                    if str(text$,74,1) = "S" then L15140
                    passedin_type$(cntr%) = "03"
L15140:         delete #23
                goto L14900

L15200:         gosub gl_post_loop
                return


        REM *************************************************************~
            * MISCELLANEOUS SUBROUTINES                                 *~
            *************************************************************

        read_work_file
            call "PLOWNXT1" (#wf%, plowwrk$, 0%, f1%(wf%))
                     if f1%(wf%) = 0% then return

            get #wf%, using L17090, text$, dbamt, cramt
L17090:         FMT XX(25), CH(109), 2*PD(14,4)
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
                if passedin_acct$(x%) = " " then goto L17410
                acct$(1%) = str(passedin_acct$(x%),,9%)
                acct$(2%) = acct$(1%)
                gltext$  = str(passedin_acct$(x%),10%)
                glamount(1%) = passedin_dbcr(x%,1%)
                glamount(2%) = passedin_dbcr(x%,2%)
                gosub'163(acct$(2%), glamount(2%))
                gosub'162(acct$(1%), glamount(1%))
                str(tran_type$,4%,2%) = passedin_type$(x%)
                iamt = glamount(1%) - glamount(2%)
                gosub post_gl
            next x%

L17410:     return

        REM *************************************************************~
            *         C O M M O N   G / L   P O S T   L O G I C         *~
            *                                                           *~
            * ACCT$(), GLTEXT$, and GLAMOUNT must be set prior to this. *~
            *************************************************************

        post_gl

            if export_on$ <> "Y" then L17560
                if whichpass$ = "C" then L17515
                     if x% > 2% then L17515
                        if x% = 1% then put gl_post_info$() using L17520, ~
                                        tran_type$, extension, quantity
                        if x% = 2% then put gl_post_info$() using L17520, ~
                                        tran_type$, -extension, -quantity
                        goto L17540
L17515:               put gl_post_info$() using L17520, tran_type$, iamt, 0
L17520:                   FMT CH(5), POS(18), 2*PD(15,4)

L17540:     REM Account in ACCT$(1) is debited...

L17560:     if glamount(1%) = 0 then L17740

            call "GLPOST2" (acct$(1%),   /* ACCOUNT TO BE UPDATED      */~
                      glamount(1%),      /* DEBIT AMOUNT (0 IF CREDIT) */~
                      0,                 /* CREDIT AMOUNT (0 IF DEBIT) */~
                      hnydate$,          /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      modno$,            /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      #2,                /* UFB ADDRESS OF G/L MAIN    */~
                      #6,                /* UFB ADDRESS OF G/L DETAILS */~
                      #7,                /* UFB ADDRESS OF SYSFILE2    */~
                      returncode%,       /* ERROR RETURN FROM SUBROUTIN*/~
                      " ", gl_post_info$())

L17740:     REM Account in ACCT$(2), ...  is credited...
            for credit% = 2% to 4%
               if acct$(credit%) = " " then L17950
               if glamount(credit%) = 0 then L17950

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
                      #2,                /* UFB ADDRESS OF G/L MAIN    */~
                      #6,                /* UFB ADDRESS OF G/L DETAILS */~
                      #7,                /* UFB ADDRESS OF SYSFILE2    */~
                      returncode%,       /* ERROR RETURN FROM SUBROUTIN*/~
                      " ", gl_post_info$())

L17950:     next credit%

        return

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
                print using L29190, prtpart$, prtqty$, prtacct$,          ~
                                   prtdebit$, prtcredit$, prtcostdescr$, ~
                                   prtcost$, prtdescr$
                goto L20130

L21000:     REM HANDLES FIRST  COLUMN--PART NUMBER AND DESCRIPTION.
                on linenumber%(1) gosub L21100, L21200, L21300, L21400
                   return
L21100:         REM FIRST  LINE--PART NUMBER
                    prtpart$ = part$
                    linenumber%(1) = 2
                    return
L21200:         REM SECOND LINE--DESCRIPTION
                    prtpart$ = " "
                    if part$ = " " then L21270
                    partreadkey$ = part$
                    call "DESCRIBE" (#3, partreadkey$, prtpart$, 0%,     ~
                                         f1%(3))
                    if f1%(3) = 0 then prtpart$ = "PART NOT ON FILE"
L21270:             linenumber%(1) = 3
                    return
L21300:         REM THIRD LINE OF PART DESCRIPTION (JOB NUMBER)
                    prtpart$ = " "
                    if jobnr$ = " " then L21340
                    prtpart$ = "JOB NUMBER: " & jobnr$
L21340:             linenumber%(1) = 4
                    return
L21400:         REM THIRD  LINE--ZAP VARIABLES AND STUFF
                    prtpart$ = " "
                    linenumber%(1%) = 5%
                    colsdone% = colsdone% + 1
                    return

L22000:    REM HANDLES SECOND COLUMN--ACCOUNT INFORMATION, IF ANY.
                x1% = x1% + 1%
                if passedin_acct$(x1%) = " " then L22110
                call "CONVERT" (passedin_dbcr(x1%,1%), 2.2, prtdebit$)
                call "CONVERT" (passedin_dbcr(x1%,2%), 2.2, prtcredit$)
                prtacct$ = passedin_acct$(x1%)
                linenumber%(2) = 3%
                return

L22110:     colsdone% = colsdone% + 1%
            prtdebit$ = " "
            prtcredit$ = " "
            prtacct$ = " "
            return

L23000:     REM HANDLES THIRD  COLUMN--TOTAL COSTS
                on linenumber%(3) gosub L23100, L23200, L23300, L23400
                   return
L23100:         REM FIRST  LINE--TOTAL COST -- Formerly Labor cost -- JIM
                    if abs(totalcost) < .000000001 then return
                    call "CONVERT" (totalcost, 2.4, prtcost$)
                    prtcostdescr$ = "T:"
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
                on linenumber%(4) gosub L24100, L24200, L24300, L24400
                   return
L24100:         REM FIRST  LINE--WHERE FROM INFO, IF ANY
                    if from$ = " " then L24200
                    prtdescr$ = "FROM:"
                    str(prtdescr$, len(prtdescr$) + 2) = from$
                    call "PUTPAREN" (prtdescr$)
                    linenumber%(4) = 2
                    return
L24200:         REM SECOND LINE--DESCRIPTIVE TEXT, IF ANY.
                    if descr$ = " " then L24250
                        prtdescr$ = descr$
                        linenumber%(4%) = 3%
                        return
L24250:             if whichpass$ = "C" then L24300
                        linenumber%(4%) = 4%
                        colsdone% = colsdone% + 1%
                        return
L24300:         REM THIRD  LINE--POSSIBLE CORE VALUE MESSAGE
                    prtdescr$ = " "
                    if whichpass$ <> "C" then L24340
                    prtdescr$ = "Core Value:  G/L Trans Only"
L24340:             linenumber%(4%) = 4%
                    colsdone% = colsdone% + 1
                    return
L24400:         REM FOURTH LINE--ZAP VARIABLES
                    prtdescr$ = " "
                    return
L25000:     REM HANDLE CASE TO PRINT QUANTITY, STORE AND LOT
                on linenumber%(5) gosub L25100, L25200, L25300, L25400
                   return
L25100:         REM PRINT QUANTITY
                    if part$ = " " then L25400
                    if rectype$ = "C"   /* Edit Qty for Comp or Parent */~
                          then call "CONVERT" (-quantity, 2.2, prtqty$)  ~
                          else call "CONVERT" (quantity, 2.2, prtqty$)
                    linenumber%(5) = 2
                    return
L25200:         REM PRINT STORE
                    prtqty$ = "STORE:"
                    str(prtqty$, 8) = store$
                    if whichpass$ = "C" then str(prtqty$,8%,3%) = "***"
                    linenumber%(5) = 3
                    return
L25300:         REM PRINT LOT
                    prtqty$ = "LOT:"
                    str(prtqty$, 5, 6) = lot$
                    if whichpass$ = "C" then str(prtqty$,5%,6%) = "******"
                    linenumber%(5) = 4
                    return
L25400:        REM ZAP VARIABLES, INCREMENT COLSDONE%
                    prtqty$ = " "
                    linenumber%(5) = 5
                    colsdone% = colsdone% + 1
                    return

L28000:     REM PRINT PAGE CONTROL ROUTINE.
                select printer  (134)
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
                   linenumber% = 7%
                   return

L29000: %PAGE ####    ###################################################~
        ~###################    ##########################################~
        ~###

L29040: %+--------------------------------+----------+-------------------~
        ~---------------+-------------+--------------------------------+

L29070: %!          PART  NUMBER          ! QUANTITY !        ACCOUNT BRE~
        ~AKDOWN         !             !          WHERE FROM/TO         !

L29100: %!              AND               ! ADDED OR +------------+------~
        ~----+----------+             !               AND              !

L29130: %!          DESCRIPTION           !WITHDRAWN !ACCOUNT #   ! DEBIT~
        ~ AMT!CREDIT AMT!   TOTAL COST!        DESCRIPTIVE TEXT        !

L29160: %+--------------------------------+----------+------------+------~
        ~----+----------+-------------+--------------------------------+

L29190: %!################################!##########!############!######~
        ~####!##########!## ##########!################################!

L30000: REM *************************************************************~
            *     G E T   I N F O R M A T I O N   F R O M   D I S K     *~
            *                                                           *~
            * GETS INFORMATION FROM DISK AND PUTS IT INTO THE VARIOUS   *~
            * FIELDS.  THEN WE RETURN TO PROCESS THE HECK OUT OF IT.    *~
            *************************************************************

            get #5, using L30230,                                         ~
                    part$, store$, lot$, jobnr$, quantity, destacct$,    ~
                    hnyacct$, cost(), from$, descr$, rectype$, costmeth$,~
                    tiebreaker%                            /* HNYFSHTF */
            totalcost = cost( 1%) + cost( 2%) + cost( 3%) + cost( 4%) +  ~
                        cost( 5%) + cost( 6%) + cost( 7%) + cost( 8%) +  ~
                        cost( 9%) + cost(10%) + cost(11%) + cost(12%)
            if tiebreaker% = tiebreakersave% then goto L30171
                mat parentcost = zer /* Different assy- zero out costs */
                parenttotlcost = 0
                tiebreakersave% = tiebreaker%
L30171:     if rectype$ = "P" then goto L30194 /* Don't accum the Parent */
                mat cost = (quantity) * cost
                mat parentcost = parentcost + cost  /* Accum Comp costs */
                parenttotlcost = parenttotlcost + (totalcost * quantity)
                goto L30205
L30194
*        This is a parent, so swap accounts
            tempacct$ = destacct$
            destacct$ =  hnyacct$
            hnyacct$  = tempacct$
L30205:     if export_on$ = "Y" then gosub data_for_glexport
            return

L30230:     FMT                          /* File #5- HNYFSHTF          */~
                XX(6),                   /* SKIP KEY INFORMATION       */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                CH(25),                  /* PART NUMBER                */~
                CH(3),                   /* STORE NUMBER               */~
                CH(16),                  /* LOT NUMBER                 */~
                CH(8),                   /* JOB NUMBER                 */~
                PD(14,4),                /* QUANTITY                   */~
                CH(9),                   /* DESTINATION (EXP) ACCT     */~
                CH(9),                   /* SOURCE (HNY) ACCT          */~
                12*PD(14,4),             /* COSTS                      */~
                CH(16),                  /* WHERE FROM INFORMATION     */~
                CH(32),                  /* FREE TEXT FIELD.           */~
                CH(01),                  /* 'C'omponent or 'P'arent    */~
                CH(01),                  /* Parent Part Cost Method    */~
                BI(04)                   /* Parent Cost Tie Breaker    */


*        Subroutine to get part data only if G/L Export is Active
        data_for_glexport
            call "READ100" (#3, part$, f1%(3%))
                if f1%(3%) = 0% then return /* Shouldn't happen */
            get #3 using L30470, partgen$, uom$, partcat$, partclass$,    ~
                                parttype$
L30470:         FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133),    ~
                    CH(4), POS(180), CH(3)
            return

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
            if  colsdone% < 2 then goto L40105       /* DONE W/ REPORT? */
                call "TIME" (time$)
                print : print using L49280, time$
                goto exit_program

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

L49080: %PAGE ####    ###################################################~
        ~###################    ##########################################~
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

L49280: %  ***** END OF REPORT @ ########

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(account$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  search str(debitsstk$(),1) = account$                  ~
                     to location$ step 9           /* FIND ACCOUNT #   */
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
                  search str(creditsstk$(),1) = account$                 ~
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
            *************************************************************
        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "JNLCLOSE" (modno$, jnlid$, pstseq%, returncode%)
            call "FILEBGON" (#22)
            if core_on% = 1% then call "FILEBGON" (#23)
            end
