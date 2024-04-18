        REM *************************************************************~
            *                                                           *~
            *  V   V  EEEEE  N   N  BBBB   FFFFF  IIIII  N   N  PPPP    *~
            *  V   V  E      NN  N  B   B  F        I    NN  N  P   P   *~
            *  V   V  EEEE   N N N  BBBB   FFFF     I    N N N  PPPP    *~
            *   V V   E      N  NN  B   B  F        I    N   N  P       *~
            *    V    EEEEE  N   N  BBBB   F      IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENBFINP - Manage Vendor Buy from file.                   *~
            *            Defaults recalled from Vendor Default file.    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/27/85 ! ORIGINAL (Cloned from VENINPUT).         ! ERN *~
            * 02/23/89 ! Corrected Administrator check on scrn #2 ! MJB *~
            * 04/24/00 ! Mod to allow Fax Number by Buy From Vendr! CMG *~
            *          !        (EWD0001)                         !     *~			
            * 02/27/07 ! (AWD002) mod to allow 'I' in confirmation! CMG *~
            *************************************************************

        dim                                                              ~
            address$(6)30,               /* VENDOR BUY FROM ADDRESS    */~
            bfcode$6,                    /* Buy From Code              */~
            bfdescr$30,                  /* Buy From Description       */~
            confirm$1,                   /* CONFORMATION FLAG          */~
            contact$20,                  /* CONTACT NAME               */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* SCREEN DISPLAY DATE        */~
            edtmessage$79,               /* "TO MODIFY VALUES..." TEXT */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fterms$1,                    /* FREIGHT TERMS              */~
            fob$30,                      /* F.O.B                      */~
            i$(24)80,                    /* SCREEN IMAGE -- NOT USED   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            interimgldescr$32,           /* INTERIM GL LIABILITY ACCT  */~
            intrmacct$16,                /* AND DESCRIPTION            */~
            lastven$16,                  /* LAST VENDOR PROCESSED      */~
            lfac$(20)1,                  /* FAC'S FOR LINEAR INPUT     */~
            pf1$(2,3)79,                 /* Scrn 1 PF Keys (Mode,Line) */~
            pf2$(2,3)79,                 /* Scrn 2 PF Keys (Mode,Line) */~
            pfk1$(2)20,                  /* Scrn 1 PF Keys (Mode)      */~
            pfk2$(2)20,                  /* Scrn 2 PF Keys (Mode)      */~
            phone$10,                    /* PHONE NUMBER THIS VENDOR   */~
/*EWD0001*/ vendrfax$10,                 /* Fax Number this Vendor     */~
            puracct$16,                  /* PURCHASES ACCOUNT          */~
            puracctdescr$32,             /* PURCHASES ACCT. DESCRIPTION*/~
            readkey$30,                  /* A Read Key                 */~
            rest$70,                     /* REST OF VENDOR RECORD      */~
            scr%(2,15), set%(255),       /* Enable arrays              */~
            scrnhdr$79,                  /* Screen Header Line         */~
            ship$30,                     /* SHIP VIA                   */~
            taxable$1,                   /* Y/N                        */~
            texta$(392,1)70,             /* Text Matrix for TXTINSUB   */~
            textid$4,                    /* X-ref to Part Text         */~
            textmsg$79,                  /* Message to TXTINSUB        */~
            thisvendor$16,               /* This Vendor/Buy From       */~
            vencode$9,                   /* VENDOR CODE                */~
            vendescr$30                  /* Vendor Descrip (Internal)  */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! VENDORBF ! Vendor Buy From file                     *~
            * # 2 ! GLMAIN   ! General Ledger.  (Account verification)  *~
            * # 3 ! VENDOR   ! Vendor Master File                       *~
            * # 6 ! TXTFILE  ! System Text File                         *~
            *************************************************************

            select # 1, "VENDORBF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 15

            select # 2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select # 3, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1, keylen =  9,                         ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #6,  "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENFILE"(#1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE"(#2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE"(#3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))

            if f2%(3) <> 0 then L65000    /* No VENDOR file, no work    */
            if f2%(1)  = 0 then L09000     /* VENDORBF                   */
               call "OPENFILE" (#1, "OUTPT", f2%(1), rslt$(1), axd$(1))
               close #1
               call "OPENFILE" (#1, "SHARE", f2%(1), rslt$(1), axd$(1))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Value And Press (ENTER)."


            date$ = date  :  call "DATEFMT" (date$)

*        Define Available PF Key Descriptiors
*                  "12345678901234567890123456789012345678901234567890"&~
*                  "12345678901234567890123456789"
        pf1$(1,1) = "P.F. Keys Available:                              "&~
                    "             (13)Instructions"
        pf1$(1,2) = "  (1)Start Over   (4)Previous Field               "&~
                    "             (15)Print Screen"
        pf1$(1,3) = "                                                  "&~
                    "             (16)Exit Program"
            pfk1$(1) = hex(0001040d0f10ffffffffffffffffffffffffffff)
        pf1$(2,1) = "P.F. Keys Available:                              "&~
                    "             (13)Instructions"
        pf1$(2,2) = "  (1)Start Over                   (12)Delete      "&~
                    "             (15)Print Screen"
        pf1$(2,3) = "                  (5)Next Screen  (28)Edit Text   "&~
                    "             (16)Save Data   "
            pfk1$(2) = hex(0001050c0d0f101c1dffffffffffffffffffffff)
        pf2$(1,1) = "P.F. Keys Available:                              "&~
                    "             (13)Instructions"
        pf2$(1,2) = "  (1)Start Over   (4)Previous Field               "&~
                    "             (15)Print Screen"
        pf2$(1,3) = "                                                  "&~
                    "                             "
            pfk2$(1) = hex(0001040d0fffffffffffffffffffffffffffffff)
        pf2$(2,1) = "P.F. Keys Available:                              "&~
                    "             (13)Instructions"
        pf2$(2,2) = "  (1)Start Over   (4)Prev Screen                  "&~
                    "             (15)Print Screen"
        pf2$(2,3) = "                                  (28)Edit Text   "&~
                    "             (16)Save Data   "
            pfk2$(2) = hex(0001040d0f101c1dffffffffffffffffffffffff)


        gosub init_enables

*        See if this guy (or gal) is an Administrator of sorts
            call "CMSMACHK" ("VBK", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Input mode Main Program.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, vencode$, bfcode$,         ~
                      bfdescr$, address$(), contact$, phone$, rest$,     ~
                      puracct$, puracctdescr$, confirm$, fterms$, fob$,  ~
                      ship$, taxable$, intrmacct$, interimgldescr$,      ~
                      vendescr$, vendrfax$                     /*  (EWD0001)  */
            textid$ = all(hex(ff))
            call "TXTFUTIL" (#6, f2%(6), "INTL", textid$)
            mode% = 1%


            for fieldnr% = 1 to 6
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10230
L10190:         gosub'201(fieldnr%,1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4 then fieldnr% = max(1, fieldnr%-1%)
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10190
L10230:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10190
                next fieldnr%

            for fieldnr% = 1 to  7
                gosub'162(fieldnr%)
                      if enabled% = 0 then L10330
L10300:         gosub'202(fieldnr%,1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4 then fieldnr% = max(1, fieldnr%-1%)
                      if keyhit% <>  0 then       L10300
L10330:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10300
                next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        edtpg1
            mode% = 2%
L11080:     gosub'201(0%,2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 then       edtpg2
                  if keyhit%  = 12 then gosub delete_buy_from
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 28 then gosub edit_text
                  if keyhit% <>  0 and keyhit% <> 29 then L11080
            fieldnr% = cursor%(1) - 5
            if fieldnr% <= 2% or   fieldnr% >  11% then L11080
            if fieldnr% >= 3% and  fieldnr% <=  8% then fieldnr% = 3%
            if fieldnr% >= 9% then fieldnr%  = fieldnr% - 5%
            if keyhit%  <> 29% then L11230
                if admin% <> 1% then L11220
                call "ENABLSUB" ("MODIFY", "VENBFINP", scr%(), set%(),   ~
                                 1%, fieldnr%, 0%, 0%)
L11220:         goto L11080
L11230:     call "ENABLSUB" ("SET", "VENBFINP", scr%(), set%(), 1%,      ~
                             fieldnr%, 2%, enabled%)
            if enabled% = 0% then L11080

L11270:     gosub'201(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11270
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11270
            goto L11080

        edtpg2
L11350:     gosub'202(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       edtpg1
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 28 then gosub edit_text
                  if keyhit% <>  0 and  keyhit% <> 29% then L11350
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  7 then L11350
            if keyhit% <> 29% then L11470
                if admin% <> 1% then L11460
                call "ENABLSUB" ("MODIFY", "VENBFINP", scr%(), set%(),   ~
                                 2%, fieldnr%, 0%, 0%)
L11460:         goto L11350
L11470:     call "ENABLSUB" ("SET", "VENBFINP", scr%(), set%(), 2%,      ~
                              fieldnr%, 2%, enabled%)
            if enabled% = 0% then L11350

L11510:     gosub'202(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11510
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11510
            goto L11350

        REM *************************************************************~
            * D E L E T E   B U Y   F R O M                             *~
            * --------------------------------------------------------- *~
            * Remove Buy From from file if proper confirmation is       *~
            * received.                                                 *~
            *************************************************************
        delete_buy_from

        str(scrnhdr$,74) = "DELETE"
L15080: accept                                                           ~
            at(01,02), "****  DELETE VENDOR BUY FROM  ****",             ~
            at(02,02), fac(hex(ac)), scrnhdr$,                           ~
            at(06,02), "Vendor Buy From",                                ~
            at(06,30), fac(hex(84)), vencode$,                           ~
            at(06,40), "-",                                              ~
            at(06,42), fac(hex(84)), bfcode$,                            ~
            at(07,30), fac(hex(84)), bfdescr$,                           ~
            at(09,02), "** NOTE: No checking is done to ensure that",    ~
            at(10,02), "         the Buy From Location is not in use.",  ~
            at(15,02), "PF KEYS:",                                       ~
            at(17,02), "  (1)RETURN to Edit Mode",                       ~
            at(19,02), " (32)DELETE Buy From Location",                  ~
                keys(hex(0120)),                                         ~
                key (keyhit%)

               if keyhit% <> 13 then L15280
                  call "MANUAL" ("VENBFINP")
                  goto L15080

L15280:        if keyhit% <> 15 then L15320
                  call "PRNTSCRN"
                  goto L40290

L15320:        if keyhit%  =  1% then return
               if keyhit% <> 32% then L15080

*        Delete this guy from the file.
            readkey$ = str(vencode$) & bfcode$
            call "DELETE" (#1, readkey$, 15%)
            call "TXTFUTIL" (#6, f2%(6), "DELE", textid$)
            return clear all
            goto inputmode


        REM *************************************************************~
            *               E D I T   T E X T                           *~
            * --------------------------------------------------------- *~
            * Allow editing of Vendor Text.                             *~
            *************************************************************
        edit_text
            textmsg$ = "Buy From: " & vencode$ & "-" & bfcode$ &         ~
                       ", '" & bfdescr$ & "'"
            call "TXTINSUB" (#6, f2%(6), "002", textmsg$, textid$,       ~
                                                            texta$())
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000
            lastven$  = vencode$ & "-" & bfcode$
            goto inputmode


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                call "ENABLSUB" ("SET", "VENBFINP", scr%(), set%(), 1%,  ~
                                 fieldnr%, 1%, enabled%)
                inpmessage$ = " "
                  on fieldnr% gosub L20150,         /* Vendor Code      */~
                                    L20200,         /* Buy From Code    */~
                                    L20250,         /* Buy From Addrss  */~
                                    L20300,         /* Vendor Contact   */~
                                    L20330,         /* Phone Number     */~
                                    L20400          /* Buy From Descr.  */
                     return

L20150
*        Default/Enable for VENDOR CODE
            inpmessage$ = "Enter blank or partial Vendor Code to sea" &  ~
                          "rch through existing Vendors."
            return

L20200
*        Default/Enable for BUY FROM CODE
            inpmessage$ = "Leave blank to search through existing"    &  ~
                          " locations for this Vendor."
            return

L20250
*        Default/Enable for BUY FROM / ADDRESS
            inpmessage$ = "Buy From is where Purchase Order will be sent."
            address$(1) = vendescr$
            return

L20300
*        Default/Enable for VENDOR CONTACT
            inpmessage$ = "Enter Vendor Contact's Name."
            return

L20330
*        Default/Enable for PHONE NUMBER/Fax
            inpmessage$ = "Enter Vendor's Phone Number & Fax Number."
            return

L20400
*        Default/Enable for BUY FROM DESCRIPTION
            inpmessage$ = "Enter description to identify this location."
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            * --------------------------------------------------------- *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'162(fieldnr%)
                call "ENABLSUB" ("SET", "VENBFINP", scr%(), set%(), 2%,  ~
                             fieldnr%, 1%, enabled%)
                inpmessage$ = " "
                  on fieldnr% gosub L21250,         /* Confirmation     */~
                                    L21300,         /* Freight Terms    */~
                                    L21350,         /* FOB              */~
                                    L21400,         /* Ship Via         */~
                                    L21450,         /* Taxable?         */~
                                    L21500,         /* G/L- Purchase    */~
                                    L21550          /*    - Intrm Liab  */
                     return

L21250
*        Default/Enable for CONFIRMATION
            inpmessage$ = "Enter if PO's need Confirmation (Y/N)."
            return

L21300
*        Default/Enable for FREIGHT TERMS
            inpmessage$ = "Enter default value for Freight Terms."
            return

L21350
*        Default/Enable for FOB
            inpmessage$ = "Enter default value for Free on Board."
            return

L21400
*        Default/Enable for SHIP VIA
            inpmessage$ = "Enter default value Ship Via."
            return

L21450
*        Default/Enable for TAXABLE
            inpmessage$ = "Enter default value for Taxable (Y/N)."
            return

L21500
*        Default/Enable for PURCHASES ACCOUNT
            inpmessage$ = "Enter default value for Purchases (Expense)" &~
                          " Account."
            return

L21550
*        Default/Enable for INTERIM LIABILITY ACCOUNT
            inpmessage$ = "Enter default value for Interim Liabilities "&~
                          " Account."
            return


        REM *************************************************************~
            * D E F A U L T  /  E N A B L E   R O U T I N E S           *~
            * --------------------------------------------------------- *~
            * Routines for handling default enable switches.            *~
            *************************************************************
        init_enables
*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer
            scr%(1, 1) =  1% : set%( 1) = 13%      /* Vendor Code      */
            scr%(1, 2) =  2% : set%( 2) = 13%      /* Buy From Code    */
            scr%(1, 3) =  3% : set%( 3) =  2%      /* Name/ Addrs      */
            scr%(1, 4) =  4% : set%( 4) =  2%      /* Contact          */
            scr%(1, 5) =  5% : set%( 5) =  2%      /* Phone #          */
            scr%(1, 6) =  6% : set%( 6) =  2%      /* BF Description   */

            scr%(2, 1) = 16% : set%(16) =  2%      /* Confimation      */
            scr%(2, 2) = 17% : set%(17) =  2%      /* Freight          */
            scr%(2, 3) = 18% : set%(18) =  2%      /* FOB              */
            scr%(2, 4) = 19% : set%(19) =  2%      /* Ship Via         */
            scr%(2, 5) = 20% : set%(20) =  2%      /* Tax              */
            scr%(2, 6) = 21% : set%(21) =  2%      /* GL-Purchases     */
            scr%(2, 7) = 22% : set%(22) =  2%      /*   -Intrm Lby     */

            call "ENABLSUB" ("INIT", "VENBFINP", scr%(), set%(),         ~
                             0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return

                return clear all
                goto inputmode

L30000: REM *************************************************************~
            *         L O A D   V E N D O R   F R O M   F I L E         *~
            * --------------------------------------------------------- *~
            * Load Vendor from the file.  Return if not found and let   *~
            * the calling routine decide what to do about that.         *~
            *************************************************************

            readkey$ = str(vencode$) & bfcode$
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then return

            get #1, using L30400,                                         ~
                     bfdescr$, address$(), contact$, phone$, confirm$,   ~
                     fterms$, fob$, ship$, taxable$, puracct$,           ~
                     intrmacct$, textid$, vendrfax$, rest$        /*  (EWD0001)  */
            call "TXTFUTIL" (#6, f2%(6), "LOAD", textid$)   /*Load Text*/
            if puracct$ = " " then L30280
            call "DESCRIBE" (#2, puracct$,  puracctdescr$,  1%, f1%(2))
                call "GLFMT" (puracct$)
L30280:     if intrmacct$ = " " then L30380
            call "DESCRIBE" (#2, intrmacct$,interimgldescr$, 1%, f1%(2))
                call "GLFMT" (intrmacct$)
L30380:   return

L30400:     FMT XX(9),                   /* Vendor Code                */~
                XX(6),                   /* Buy From Code              */~
                CH(30),                  /* Buy From Description       */~
                6*CH(30),                /* Buy From Name and Address  */~
                CH(20),                  /* Contact Name               */~
                CH(10),                  /* Phone Number               */~
                CH(01),                  /* Confirmation               */~
                CH(01),                  /* Freight Terms              */~
                CH(30),                  /* FOB                        */~
                CH(30),                  /* Ship Via                   */~
                CH(1),                   /* Taxable                    */~
                CH(09),                  /* Purchases Account          */~
                CH(09),                  /* Interim Liability Acct     */~
                CH(4),                   /* Text ID X-Ref              */~
                CH(10),                  /* Fax Number                 */~
                CH(150)                  /* Remainder of record        */


L31000: REM *************************************************************~
            *             W R I T E   D A T A   T O   F I L E           *~
            *                                                           *~
            * WRITE THE VENDOR TO THE FILE.  SINCE IT MIGHT BE EITHER   *~
            * NEW OR HELD, THEN WE HAVE TO WRITE OR REWRITE ACCORDINGLY.*~
            *************************************************************

            call "GLUNFMT" (puracct$)
            call "GLUNFMT" (intrmacct$)
            readkey$ = str(vencode$) & bfcode$
            call "READ101" (#1, readkey$, f1%(1))
            put #1, using L31270, vencode$, bfcode$,                      ~
                    bfdescr$, address$(), contact$,  phone$, confirm$,   ~
                    fterms$, fob$, ship$, taxable$, puracct$, intrmacct$,~
                    textid$,  vendrfax$, rest$                    /*  (EWD0001)  */
            if f1%(1) = 1 then rewrite #1 else write #1
            call "TXTFUTIL" (#6, f2%(6), "TOS2", textid$)   /*Save Text*/
            return

L31270:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(6),                   /* BUY FROM CODE              */~
                CH(30),                  /* BUY FROM DESCRIPTION       */~
                6*CH(30),                /* NAME AND ADDRESS           */~
                CH(20),                  /* CONTACT NAME               */~
                CH(10),                  /* PHONE NUMBER               */~
                CH(01),                  /* CONFIRMTION                */~
                CH(01),                  /* FRIEGHT TERMS              */~
                CH(30),                  /* FOB                        */~
                CH(30),                  /* SHIP VIA                   */~
                CH(01),                  /* TAXABLE                    */~
                CH(09),                  /* PURCHASES ACCOUNT          */~
                CH(09),                  /* INTERIM LIABILITY ACCT     */~
                CH(4),                   /* TEXT ID X-REF              */~
                CH(10),                  /* Fax NUMBER                 */~
                CH(150)                  /* REMAINDER OF RECORD        */

        REM *************************************************************~
            * L O A D   V E N D O R                                     *~
            * --------------------------------------------------------- *~
            * Load Information from the Remit-To vendor that might be   *~
            * be helpful as default values for the Buy From.            *~
            *************************************************************
        load_vendor

            get #3 using L32110, contact$, phone$, puracct$, confirm$,    ~
                                fterms$, fob$, taxable$, ship$,          ~
                                intrmacct$
L32110:         FMT XX(219), CH(20), CH(10), CH(9), POS(414), CH(1),     ~
                    CH(1), CH(30), CH(1), CH(30), XX(4), CH(9)

            call "DESCRIBE" (#2, puracct$,  puracctdescr$,  1%, f1%(2))
                call "GLFMT" (puracct$)
            call "DESCRIBE" (#2, intrmacct$,interimgldescr$, 1%, f1%(2))
                call "GLFMT" (intrmacct$)
            puracctdescr$, interimgldescr$ = " "
            return

        REM *************************************************************~
            * I N P U T  /  E D I T   S C R E E N  -   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Input and Edit of Screen Page 1.                          *~
            *************************************************************

            deffn'201(fieldnr%, edit%)
              init(hex(8c)) lfac$()
              if edit% = 2% and fieldnr% = 0% then init(hex(86)) lfac$()
                  on fieldnr% gosub L40200,         /* Vendor Code      */~
                                    L40200,         /* Buy From Code    */~
                                    L40200,         /* Remit to Address */~
                                    L40170,         /* Vendor Contact   */~
                                    L40200,         /* Phone Number     */~
                                    L40200          /* Buy From Descr   */
                     goto L40270

L40170:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40200:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40270:     scrnhdr$ = "Date: " & date$
            str(scrnhdr$,74) = "Page 1"
L40290:     accept                                                       ~
               at (01,02), "MAINTAIN VENDOR BUY FROM LOCATIONS",         ~
               at (01,52), "Last Vendor:",                               ~
               at (01,65), fac(hex(84)), lastven$               , ch(16),~
               at (02,02), fac(hex(ac)), scrnhdr$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Vendor Code",                                ~
               at (06,30), fac(lfac$( 1)), vencode$             , ch(09),~
               at (06,49), fac(hex(8c)),   vendescr$            , ch(30),~
               at (07,02), "Buy From Code",                              ~
               at (07,30), fac(lfac$( 2)), bfcode$              , ch(06),~
               at (08,02), "BUY FROM: Name           ",                  ~
               at (09,02), "          Address - 1    ",                  ~
               at (10,02), "          Address - 2    ",                  ~
               at (11,02), "          Address - 3    ",                  ~
               at (12,02), "          Address - 4    ",                  ~
               at (13,02), "          City, State, Zip",                 ~
               at (08,30), fac(lfac$( 3)), address$(1)          , ch(30),~
               at (09,30), fac(lfac$( 3)), address$(2)          , ch(30),~
               at (10,30), fac(lfac$( 3)), address$(3)          , ch(30),~
               at (11,30), fac(lfac$( 3)), address$(4)          , ch(30),~
               at (12,30), fac(lfac$( 3)), address$(5)          , ch(30),~
               at (13,30), fac(lfac$( 3)), str(address$(6),1,17), ch(17),~
               at (13,50), fac(lfac$( 3)), str(address$(6),19,2), ch(02),~
               at (13,54), fac(lfac$( 3)), str(address$(6),22,5), ch(05),~
               at (13,60), "-",                                          ~
               at (13,62), fac(lfac$( 3)), str(address$(6),27,4), ch(04),~
                                                                         ~
               at (14,02), "Vendor Contact",                             ~
               at (14,30), fac(lfac$( 4)), contact$             , ch(20),~
               at (15,02), "Phone Number",                               ~
               at (15,30), "(",                                          ~
               at (15,32), fac(lfac$( 5)), str(phone$, 1, 3)    , ch(03),~
               at (15,36), ")",                                          ~
               at (15,38), fac(lfac$( 5)), str(phone$, 4, 3)    , ch(03),~
               at (15,42), "-",                                          ~
               at (15,44), fac(lfac$( 5)), str(phone$, 7, 4)    , ch(04),~
/*(EWD0001)*/  at (15,51), "Fax",                                        ~
/* Begin   */  at (15,55), "(",                                          ~
               at (15,57), fac(lfac$( 5)), str(vendrfax$, 1, 3) , ch(03),~
               at (15,61), ")",                                          ~
               at (15,63), fac(lfac$( 5)), str(vendrfax$, 4, 3) , ch(03),~
/*(EWD0001)*/  at (15,67), "-",                                          ~
/* End     */  at (15,69), fac(lfac$( 5)), str(vendrfax$, 7, 4) , ch(04),~
               at (16,02), "Buy From Description",                       ~
               at (16,30), fac(lfac$( 6)), bfdescr$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pf1$(mode%, 1)         , ch(79),~
               at (23,02), fac(hex(8c)), pf1$(mode%, 2)         , ch(79),~
               at (24,02), fac(hex(8c)), pf1$(mode%, 3)         , ch(79),~
                    keys(pfk1$(mode%)),                                  ~
                    key (keyhit%)

               if keyhit% <> 13 then L40750
                  call "MANUAL" ("VENBFINP")
                  goto L40290

L40750:        if keyhit% <> 15 then L40790
                  call "PRNTSCRN"
                  goto L40290

L40790:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            * I N P U T  /  E D I T   S C R E E N  -   P A G E   2      *~
            * --------------------------------------------------------- *~
            * Input and Edit Modes for Page 2.                          *~
            *************************************************************

            deffn'202(fieldnr%, edit%)
               init(hex(8c)) lfac$()
               if edit% = 2% and fieldnr% = 0% then init(hex(86)) lfac$()
                  on fieldnr% gosub L41135,         /* Confirmation     */~
                                    L41135,         /* Freight Terms    */~
                                    L41135,         /* Free on Board    */~
                                    L41135,         /* Ship Via         */~
                                    L41135,         /* Taxable          */~
                                    L41135,         /* GL- Purchases    */~
                                    L41135          /*   - Interim Liab */
                     goto L41170

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41135:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41170:     scrnhdr$ = "Date: " & date$
            str(scrnhdr$,74) = "Page 2"
            thisvendor$ = vencode$ & "-" & bfcode$
            accept                                                       ~
               at (01,02), "MAINTAIN VENDOR BUY FROM LOCATIONS",         ~
               at (01,52), "This Vendor:",                               ~
               at (01,65), fac(hex(84)), thisvendor$            , ch(16),~
               at (02,02), fac(hex(ac)), scrnhdr$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
/*(AWD002)*/   at (06,02), "Confirmation Req'd (Y/N/I)",                 ~
               at (06,30), fac(lfac$( 1)), confirm$             , ch(01),~
               at (07,02), "Freight Terms",                              ~
               at (07,30), fac(lfac$( 2)), fterms$              , ch(01),~
               at (07,49), "A-Added, N-Not Added, C-Collect",            ~
               at (08,02), "Free on Board",                              ~
               at (08,30), fac(lfac$( 3)), fob$                 , ch(30),~
               at (09,02), "Ship Via",                                   ~
               at (09,30), fac(lfac$( 4)), ship$                , ch(30),~
               at (10,02), "Taxable? (Y/N)",                             ~
               at (10,30), fac(lfac$( 5)), taxable$             , ch(01),~
               at (11,02), "G/L- Purchases",                             ~
               at (11,30), fac(lfac$( 6)), puracct$             , ch(12),~
               at (11,49), fac(hex(8c)),   puracctdescr$        , ch(32),~
               at (12,02), "   - Interim Liabilities",                   ~
               at (12,30), fac(lfac$( 7)), intrmacct$           , ch(12),~
               at (12,49), fac(hex(8c)),   interimgldescr$      , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf2$(mode%,1)        , ch(79),~
               at (23,02), fac(hex(8c)),   pf2$(mode%,2)        , ch(79),~
               at (24,02), fac(hex(8c)),   pf2$(mode%,3)        , ch(79),~
                     keys(pfk2$(mode%)),                                 ~
                     key (keyhit%)

               if keyhit% <> 13 then L41585
                  call "MANUAL" ("VENBFINP")
                  goto L41170

L41585:        if keyhit% <> 15 then L41610
                  call "PRNTSCRN"
                  goto L41170

L41610:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the fields on Page 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50150,         /* Vendor Code      */~
                                    L50280,         /* Buy From Code    */~
                                    L50330,         /* Remit-To Addrs   */~
                                    L50380,         /* Vendor Contact   */~
                                    L50410,         /* Phone Number     */~
                                    L50440          /* Buy From Descr   */
                     return

L50150
*        Test data for VENDOR CODE
            vendescr$ = hex(06) & "Select Vendor Code."
            call "GETCODE" (#3, vencode$, vendescr$, 0%, 1, f1%(3))
            if f1%(3) <> 0 then L50230
                vendescr$ = " "
                errormsg$ = "Invalid Vendor Code."
                return
L50230:     gosub load_vendor
            return

L50280
*        Test data for VENDOR DESCRIPTION (aka SORT NAME)
            if bfcode$ <> " " then L50312
                bfdescr$ = hex(06) & "Select Buy From to Edit"
                readkey$ = str(vencode$) & bfcode$
                call "PLOWCODE" (#1, readkey$, bfdescr$, 9%, 0.30, f1%(1))
                if f1%(1) <> 0% then bfcode$ = str(readkey$,10)
                if f1%(1) <> 0% then L50312
                     bfdescr$ = " "  :  errormsg$ = hex(00)
                     return
L50312:     gosub L30000
                if f1%(1) = 0% then return
                return clear all
                goto edtpg1

L50330
*        Test for Remit To Address
            if address$(1) <> " " then return
                errormsg$ = "Vendor Name cannot be left blank."
                return

L50380
*        Test data for VENDOR CONTACT
            return

L50410
*        Test data for PHONE NUMBER/FAX NUMBER
            return

L50440
*        Test data for BUY FROM DESCRIPTION
            if bfdescr$ <> " " then return
                errormsg$ = "Buy From Description cannot be blank."
                return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51250,         /* Confirmation     */~
                                    L51300,         /* Freight Terms    */~
                                    L51350,         /* FOB              */~
                                    L51400,         /* Ship Via         */~
                                    L51450,         /* Taxable          */~
                                    L51500,         /* GL- Purchases    */~
                                    L51550          /*   - Interim Liab */
                  return

L51250
*        Test data for CONFIRMATION
/*(AWD002)*/
            if pos("INY" = confirm$) = 0 then                            ~
                     errormsg$="Must be 'Y' 'N' -or- 'I'. Please reenter."
            return

L51300
*        Test data for FREIGHT TERMS
            if pos("ANC" = fterms$) = 0 then                             ~
                  errormsg$ = "Must be 'A', 'N', or 'C'. Please reenter."
            return

L51350
*        Test data for FREE ON BOARD
            return

L51400
*        Test data for Ship Via
            return

L51450
*        Test data for TAXABLE
            if pos("YN" = taxable$) = 0 then                             ~
                      errormsg$="Must be 'Y' -or- 'N'. Please reenter."
                 return

L51500
*        Test data for PURCHASES ACCOUNT
            puracctdescr$ = " "  :  if puracct$ = " " then return
            call "GETCODE" (#2, puracct$, puracctdescr$, 1%, 0, f1%(2))
            if f1%(2) = 1 then return
                errormsg$ = "Purchases Account Not On File: " & puracct$
                return

L51550
*        Test data for INTERIM LIABILITIES ACCOUNT
            interimgldescr$ = " "  :  if intrmacct$ = " " then return
            call "GETCODE" (#2, intrmacct$, interimgldescr$, 1%, 0,f1%(2))
            if f1%(2) = 1 then return
                errormsg$ = "Interim Liability Account Not On File: " &  ~
                                                               intrmacct$
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
