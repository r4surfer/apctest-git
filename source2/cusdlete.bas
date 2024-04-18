        REM *************************************************************~
            *                                                           *~
            *   CCC   U   U   SSS   DDDD   L      EEEEE  TTTTT  EEEEE   *~
            *  C   C  U   U  S      D   D  L      E        T    E       *~
            *  C      U   U   SSS   D   D  L      EEEE     T    EEEE    *~
            *  C   C  U   U      S  D   D  L      E        T    E       *~
            *   CCC    UUU    SSS   DDDD   LLLLL  EEEEE    T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUSDLETE - DELETES CUSTOMER MASTER RECORDS FROM THE       *~
            *            CUSTOMER FILE WHEN THEY ARE NOT MENTIONED IN   *~
            *            ANY SOURCE DOCUMENT FILE--RECEIVABLES, CASH    *~
            *            RECEIPTS, BACKLOG, OR SALES ANALYSIS.          *~
            *            THIS IS SEPARATE PROGRAM FOR SECURITY PURPOSES.*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/01/81 ! ORIGINAL                                 ! BCW *~
            * 10/04/86 ! Rewritten for Revenue proj file changes. ! ERN *~
            * 03/27/87 ! Increased CRCMASTR file size.            ! SGA *~
            * 03/20/90 ! Fixed PRR 11129.  Delete record type 'S' ! SID *~
            *          ! special pricing from CPRPRICE and added  !     *~
            *          ! a test for a Credit Parent Code.         !     *~
            * 03/13/91 ! Purge CUSTOMERs to WRA Mailing List.     ! JIM *~
            * 04/18/91 ! Conditionally OPENs MALMASTR.            ! JIM *~
            * 11/17/92 ! Cust Credit- Delete from CCR files, too. ! JIM *~
            * 12/01/92 ! Delete Customers fm CORPARNT- Core Parn't! JDH *~
            * 12/02/92 ! Make any children of Core Parents on     ! KAB *~
            *          ! their own. While we are at it, why not   !     *~
            *          ! check ARMTRIAL. It is more active than   !     *~
            *          ! either ARI or CRC Master.                !     *~
            *************************************************************

        dim                                                              ~
            address$(6)30,               /* Customer Address Fields    */~
            ctype$2,                     /* Cust Type for MALMASTR     */~
            cus2mal$1,                   /* Purge CUSTOMERs to MailList*/~
            cuscode$9,                   /* Customer Code              */~
            cusname$30,                  /* Customer Name              */~
            dat$7,                       /* GETDTTM MALMASTR tie-brkr  */~
            date$8,                      /* Today's Date               */~
            errormsg$79,                 /* Error Message Text Info    */~
            inpmessage$79,               /* Input Informative Message  */~
            lastcust$9,                  /* Last Customer Code Deleted */~
            line2$79,                    /* Second Screen Line         */~
            mailaddr$(5)30, mailname$30, /* MALMASTR name & address    */~
            pf16$10,                     /* Screen Display PF key      */~
            plowkey$20,                  /* Plowkey for CPRPRICE       */~
            prname$8,                    /* PR Name for SA Summary     */~
            status$1,                    /* Customer Status Code       */~
            readkey$100, readkey2$100,   /* Key to Read file with      */~
            telphone$16,                 /* CUSTOMER telephone number  */~
            textid$4,                    /* Text ID                    */~
            warning$79                   /* Warning Message            */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            fs%(64)                      /* = 1 Open, -1 doesn't exist */

            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
            mat f2% = con


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 2 ! SYSFILE2 ! System Information File                  *~
            * # 3 ! CUSTOMER ! Customer Master                          *~
            * # 4 ! TXTFILE  ! System Text File                         *~
            * # 5 ! ARIMASTR ! Invoice Master File                      *~
            * # 6 ! ARMTRIAL ! A/R Trial Balance (Open Item) File       *~
            * # 7 ! CRCMASTR ! Cash Receipts Check Header File          *~
            * # 9 ! BCKMASTR ! Customer Backlog Master File             *~
            * #11 ! SASUMRY# ! Sales Analysis Summary File              *~
            * #12 ! CPRPRICE ! Customer Pricing File                    *~
            * #41 ! MALMASTR ! Mailing List Master file                 *~
            * #42 ! CORPARNT ! Core Deposit Tracking Core Parent file.  *~
            * #43 ! CORDRMAS ! Core Debit Master File                   *~
            * #44 ! CORCRMAS ! Core Credit Master File                  *~
            * #50 ! CCRMASTR ! Customer Credit Master file              *~
            * #51 ! CCRLINES ! Customer Credit Line Items (NSFs)        *~
            *************************************************************

            select #2,  "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos =     1, keylen =  20

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #4,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =   11

            select #5,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17

            select #6,  "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =  21

            select  #7, "CRCMASTR",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 17

            select #9,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #11, "SASUMRY#",                                      ~
                        varc, indexed, recsize = 1048,                   ~
                        keypos =      1, keylen =  56,                   ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #12, "CPRPRICE",                                      ~
                        varc, indexed, recsize =  700,                   ~
                        keypos =  1, keylen = 47

            select #41, "MALMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos = 1, keylen =  37

            select #42, "CORPARNT",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   10, keylen =   9,                     ~
                        alt key  1, keypos =    1, keylen = 18

            select #43, "CORDRMAS",                                      ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   10, keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29

            select #44, "CORCRMAS",                                      ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   10, keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29

            select #50, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            select #51, "CCRLINES",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    1, keylen =   12

        call "SHOSTAT"  ("Opening Files; One Moment Please.")
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ),   0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ),   0%, rslt$(3 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ),   0%, rslt$(5 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ),   0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ),   0%, rslt$(7 ))
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ),   0%, rslt$(9 ))
            call "OPENCHCK" (#12, fs%(12), f2%(12),   0%, rslt$(12))
            call "OPENCHCK" (#42, cp%    , f2%(42),   0%, rslt$(42))
            call "OPENCHCK" (#43, cd%    , f2%(43),   0%, rslt$(43))
            call "OPENCHCK" (#44, cc%    , f2%(44),   0%, rslt$(44))
            call "OPENCHCK" (#50, fs%(50), f2%(50),   0%, rslt$(50))
            call "OPENCHCK" (#51, fs%(51), f2%(51),   0%, rslt$(51))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION USED IN PROGRAM PROCESSING        *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            str(line2$,62%) = "CUSDLETE: " & str(cms2v$,,8)

*        Find out which summary file to use and open it. We ideally
*        want a ship-to (Group = 4) but will use an Account (=3) if
*        that is all we can get
            readkey2$ = " "
            readkey$ = "SA.FILES.SASUMRY" & hex(00)
L09140:     call "PLOWNEXT" (#2, readkey$, 16%, f1%(2))
            if f1%(2) = 0% then L09220
                get #2 using L09170, group1%, group2%
L09170:              FMT POS(53), 2*BI(1)
                if group1% = 4% or group2% = 4% then L09240
                if group1% = 3% or group2% = 3% then readkey2$ = readkey$
                goto L09140

L09220:     if readkey2$ = " " then L09321
            call "READ100" (#2, readkey2$, f1%(2))
L09240:     get #2 using L09250, prname$, group1%, group2%
L09250:         FMT XX(9), CH(8), POS(53), 2*BI(1)
            if group1% = 3% then sakey% = 2%  /* Account, 2nd Alt      */
            if group2% = 3% then sakey% = 1%  /* Account, 1st Alt      */
            if group2% = 4% then sakey% = 1%  /* Ship-to, 1st Alt      */
            if group1% = 4% then sakey% = 2%  /* Ship-to, 2nd Alt      */
            call "PUTPRNAM" addr (#11, prname$)
            call "OPENCHCK" (#11, fs%(11), f2%(11),   0%, rslt$(11))

L09321:     readkey2$ = "SWITCHS.WRA"
            call "READ100" (#2, readkey2$, f1%(2))
            if f1%(2) <> 0% then get #2 using L09324, cus2mal$
L09324:         FMT POS(24), CH(1)

        REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            * --------------------------------------------------------- *~
            * Gets Customer Code, Tests, then runs routine that displays*~
            * Customer Name and Address.  Then goes to routine that     *~
            * does the actual delete.                                   *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, cuscode$, cusname$,        ~
                      address$(), status$
            call "ALLFREE"

            for fieldnr% = 1 to 1
                gosub'051
L10150:         gosub'101
                      if keyhit%  = 16 then       L65000
                      if keyhit% <>  0 then       L10150
                gosub'151
                      if errormsg$ <> " " then L10150
                next fieldnr%

*        Get Confirmation prior to actually killing this guy
            inpmessage$ = "To Delete This Customer, Press PF-16," &      ~
                          " or PF-1 to start over."
            gosub L41000
                if keyhit% =  1% then inputmode
                if keyhit% = 16% then L19000 else inputmode


L19000: REM *************************************************************~
            *                 D E L E T E   R O U T I N E               *~
            * --------------------------------------------------------- *~
            * Delete Customer from Customer Master.                     *~
            *************************************************************

            call "READ101" (#3, cuscode$, f1%(3))
            if f1%(3) = 0% then L19080
                delete #3
                gosub add_to_mail_list
                gosub delete_core_parent
L19080:     call "TXTFUTIL" (#4, f2%(4), "DELE", textid$)
            call "DELETE" (#50, cuscode$, 9%)              /* CCRMASTR */
            call "DELETE" (#51, cuscode$, 9%)              /* CCRLINES */
        REM Delete record type 'S' for CPRPRICE
            str(plowkey$,,10) = "S" & str(cuscode$,,9)
            call "READ101" (#12, plowkey$, f1%(12))
            if f1%(12) <> 0% then delete #12
            lastcust$ = cuscode$
            goto inputmode

        add_to_mail_list
            if cus2mal$ <> "Y" then return
            get #3 using L19270, mailname$, mailaddr$(), telphone$, ctype$
            if fs%(41) = 0% then                                         ~
                call "OPENCHCK" (#41, fs%(41), f2%(41), 300%, rslt$(41))
            str(mailaddr$(5),21,10) = str(mailaddr$(5),22,9) & " "
            call "GETDTTM" addr (dat$)
            write #41 using L19320, mailname$, dat$, mailaddr$(),         ~
                telphone$, " ", "X", " ", ctype$, " ", eod goto L19250
L19250:     return

L19270:     FMT /* Partial #3 CUSTOMER record layout                   */~
                POS(253), 6*CH(30),      /* SHIP-TO name & address     */~
                POS(453), CH(10),        /* Telephone number           */~
                POS(1023), CH(02)        /* Customer Type              */

L19320:     FMT /* File #41 -- MALMASTR record layout                  */~
                CH(30),                  /* Name                       */~
                CH(07),                  /* Date/Time tie-breaker      */~
                5*CH(30),                /* Name, address, C,S,Z       */~
                CH(16),                  /* Telephone number           */~
                CH(16),                  /* FAX number                 */~
                CH(01),                  /* Source code: CUSTOMER      */~
                CH(05),                  /* Filler                     */~
                CH(02),                  /* Customer Type (DIVISION)   */~
                CH(73)                   /* Filler                     */

        delete_core_parent
            if cp% <= 0% then return                 /* C/T installed? */
            call "READ101" (#42, cuscode$, f1%(42))        /* CORPARNT */
            if f1%(42) = 0% then L19560            /* Record not found? */
            delete #42                              /* Delete CORPARNT */
        /* Any Children to set adrift? */
L19560:     readkey$ = cuscode$
L19570:     call "PLOWAL1" (#42, readkey$, 1%, 9%, f1%(42))
               if f1%(42%) = 0% then return
            put #42 using L19600, str(readkey$,10,9)      /* On His Own */
L19600:         FMT POS(1), CH(9)
            rewrite #42
            goto L19570

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * DEFAULT/ENABLE FOR CUSTOMER CODE ON FIRST PAGE.  SET      *~
            * APPROPRIATE MESSAGE TEXT.                                 *~
            *************************************************************

        deffn'051
            inpmessage$ = "Customer Code Must Not Have Any Activity."
            return

        REM *************************************************************~
            *      I N P U T   C U S T O M E R   T O   D E L E T E      *~
            * --------------------------------------------------------- *~
            * Enter the Customer Code to be deleted.                    *~
            *************************************************************

        deffn'101
            if str(lastcust$) <> " " then                                ~
                         str(line2$,,60) = "Last Customer: " & lastcust$

L40120:     accept                                                       ~
               at (01,02), "DELETE CUSTOMERS",                           ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Customer Code",                              ~
               at (06,30), fac(hex(81)),   cuscode$             , ch(09),~
               at (06,49), fac(hex(8c)),   cusname$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                     keys(hex(000d0f10)), key(keyhit%)

               if keyhit% <> 13% then L40320
                     call "MANUAL" ("CUSDLETE")
                     goto L40120

L40320:        if keyhit% <> 15 then return
                     call "PRNTSCRN"
                     goto L40120

L41000: REM *************************************************************~
            *                C O N F I R M   D E L E T E                *~
            * --------------------------------------------------------- *~
            * Confirms that we are in fact to delete this Customer.     *~
            *************************************************************


            pf16$ = "(16)DELETE"
L41070:     accept                                                       ~
               at (01,02), "DELETE CUSTOMERS",                           ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Customer Code",                              ~
               at (06,30), fac(hex(84)),   cuscode$             , ch(09),~
               at (06,49), fac(hex(84)),   cusname$             , ch(30),~
                                                                         ~
               at (07,02), "Ship-to Address",                            ~
               at (07,30), fac(hex(84)),   address$(1)          , ch(30),~
               at (08,30), fac(hex(84)),   address$(2)          , ch(30),~
               at (09,30), fac(hex(84)),   address$(3)          , ch(30),~
               at (10,30), fac(hex(84)),   address$(4)          , ch(30),~
               at (11,30), fac(hex(84)),   address$(5)          , ch(30),~
               at (12,30), fac(hex(84)),   address$(6)          , ch(30),~
                                                                         ~
               at (14,02), "Customer Status",                            ~
               at (14,30), fac(hex(84)),   status$              , ch(01),~
                                                                         ~
               at (16,02), fac(hex(84)), warning$               , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(84)),   pf16$                ,        ~
                     keys(hex(010d0f10)), key(keyhit%)

               if keyhit% <> 13% then L41370
                     call "MANUAL" ("CUSDLETE")
                     goto L41070

L41370:        if keyhit% <> 15 then return
                     call "PRNTSCRN"
                     goto L41070

        REM *************************************************************~
            *   D A T A   T E S T   F O R   C U S T O M E R   C O D E   *~
            * --------------------------------------------------------- *~
            * Test Data for Customer Code.  Make Sure that the Customer *~
            * is on file and not on file in any of the source document  *~
            * files or history.                                         *~
            *************************************************************

        deffn'151
            errormsg$, warning$ = " "

*        Test Data for Customer Code and load information
            if cuscode$ <> " " then L50150
                errormsg$ = "Customer Code May Not Be Blank!!"
                return
L50150:     call "READ100" (#3, cuscode$, f1%(3))
            if f1%(3) = 1% then L50190
                errormsg$ = "Customer Code Not On File"
                return
L50190:     get #3 using L50200, cusname$, address$(), textid$, status$
L50200:         FMT XX(9), CH(30), POS(253), 6*CH(30), POS(789), CH(4),  ~
                                                                 CH(1)
*        Now Test for Uses of this Customer.
*        Check 1- Make sure is not referenced as an Account
            call "REDALT0" (#3, cuscode$, 3%, f1%(3)) /* Account Xref */
            if f1%(3) = 0% then L50330
                if key(#3) = cuscode$ then L50290
L50270:              errormsg$ = "Customer is referenced as an Account"
                     return
L50290:         call "READNEXT" (#3, f1%(3))
                if f1%(3) = 0% then L50330
                     if key(#3, 3%) = cuscode$ then L50270

L50330
*        Check 2- Make sure is not referenced as a Bill-to
            call "REDALT0" (#3, cuscode$, 4%, f1%(3)) /* Account Xref */
            if f1%(3) = 0% then L50430
                if key(#3) = cuscode$ then L50390
L50370:              errormsg$ = "Customer is referenced as a Bill-to"
                     return
L50390:         call "READNEXT" (#3, f1%(3))
                if f1%(3) = 0% then L50430
                     if key(#3, 4%) = cuscode$ then L50370

L50430
*        Check 3- Test for any INVOICES on file.
            readkey$ = cuscode$
            call "PLOWNEXT" (#5, readkey$, 9%, f1%(5))
            if f1%(5) = 0% then L50500
                errormsg$ = "Customer still has Invoices on file."
                return

L50500
*        Check 4- Test for any CHECKS on file.
            readkey$ = cuscode$
            call "PLOWNEXT" (#7, readkey$, 9%, f1%(7))
            if f1%(7) = 0% then L50570
                errormsg$ = "Customer still has Checks on file."
                return

L50570
*        Check 5- Test for SALES ORDERS on file.
            readkey$ = cuscode$
            call "PLOWNEXT" (#9, readkey$, 9%, f1%(9))
            if f1%(9) = 0% then L50640
                errormsg$ = "Customer still has Sales Orders on file."
                return

L50640
*        Check 6- Test for SALES ANALYSIS on file.
            if sakey% = 0% then L50720
                readkey$ = cuscode$
                call "PLOWALTS" (#11, readkey$, sakey%, 9%, f1%(11))
                if f1%(11) = 0% then L50720
                    errormsg$ = "Customer still has Sales History."
                    return

L50720
*        Check 7- Test for Credit Parent.
            call "REDALT0" (#3, cuscode$, 5%, f1%(3)) /* Credit Parent*/
              if f1%(3) = 0% then L50810
                     errormsg$ = "Customer is referenced as a Credit "   ~
                               & "Parent Code."
                     return

L50810
*        Check 8- Test for ARMTRIAL Records on file.
                readkey$ = cuscode$
                call "PLOWALTS" (#6, readkey$, 0%, 9%, f1%(6))
                if f1%(6) = 0% then L50880
                    errormsg$ = "Customer still has Settlement Records."
                    return

L50880
*        Check 9- Test for CORDRMAS Records on file.
                if cd% <= 0% then L51020
                readkey$ = cuscode$
                call "PLOWALTS" (#43, readkey$, 0%, 9%, f1%(43))
                if f1%(43) = 0% then L50950
                   errormsg$ =  "Customer still has Core Debit Records."
                   return
L50950:         readkey$ = cuscode$
                call "PLOWALTS" (#43, readkey$, 1%, 9%, f1%(43))
                if f1%(43) = 0% then L51020
                   errormsg$ =  "Customer is a Parent on Core Debit " &  ~
                                "Records."
                   return

L51020
*        Check 10- Test for CORCRMAS Records on file.
                if cc% <= 0% then L51160
                readkey$ = cuscode$
                call "PLOWALTS" (#44, readkey$, 0%, 9%, f1%(44))
                if f1%(44) = 0% then L51090
                   errormsg$ =  "Customer still has Core Credit Records."
                   return
L51090:         readkey$ = cuscode$
                call "PLOWALTS" (#44, readkey$, 1%, 9%, f1%(44))
                if f1%(44) = 0% then L51160
                   errormsg$ =  "Customer is a Parent on Core Credit " & ~
                                "Records."
                   return

L51160
*        Check 11- Core Parent Warning
                if cp% <= 0% then L51270
                readkey$ = cuscode$
L51190:         call "PLOWALTS" (#42, readkey$, 1%, 9%, f1%(42))
                if f1%(42) = 0% then L51270
                if str(readkey$,10,9) = str(cuscode$,,9) then L51190
                   warning$ = "Customer is on file as a Core Parent."
                   warning$ = warning$ &                                 ~
                              " Dependents will become stand-alone."
                   goto L51270

L51270
*        Check 12- Place Holder
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            end
