        REM *************************************************************~
            *                                                           *~
            *  V   V  EEEEE  N   N  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  V   V  E      NN  N    I    NN  N  P   P  U   U    T     *~
            *  V   V  EEEE   N N N    I    N N N  PPPP   U   U    T     *~
            *   V V   E      N  NN    I    N  NN  P      U   U    T     *~
            *    V    EEEEE  N   N  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENINPUT - INPUT/EDIT VENDOR MASTER FILE INFORMATION.     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/07/81 ! ORIGINAL                                 ! TEM *~
            * 10/06/81 ! CHANGE FACS FOR ENTERING ADDRESSES,      ! RAC *~
            *          ! VENDOR NAME AND CONTACT TO UPERCASE ONLY !     *~
            * 12/06/83 ! NULL APPENDIX LINKAGE INCLUDED           ! KAB *~
            * 08/09/84 ! ADDED INTERIM GL LIABILITY ACCOUNT       ! JUDD*~
            * 08/13/85 ! GENERAL HOUSE CLEANING                   ! HES *~
            * 09/24/85 ! (1)Added Buy from Vendor File            ! ERN *~
            *          ! (2)Add Text Management                   !     *~
            *          ! (3)Name as alternate key added           !     *~
            *          ! (4)Additional defaults fields added.     !     *~
            *          ! (5)Store Level 'pass thru' for G/L.      !     *~
            *          ! (6)Clean-up and additional features.     !     *~
            *          ! (7)Added 'HEX00' Defaulting              !     *~
            * 05/18/87 ! Added Prc-Cst Variance Account           ! MJB *~
            * 09/30/87 ! Fixed remit to name field so it will     !     *~
            *          ! copy in vendor name from line above.     ! DAW *~
            * 06/28/88 ! Fixed SCR% dim. (2,16) for page 2        ! KAB *~
            * 06/07/90 ! Fixed PRR's                              ! SID *~
            *          !  10500- Fixed program Date Displayed.    !     *~
            *          !  10382- Added "RETURN" statement.        !     *~
            *          !  10895- Added the ability to view        !     *~
            *          !         "Buy From" Addresses.            !     *~
            * 04/02/91 ! Fixed PRR's                              ! SID *~
            *          !  11789- Fixed PF3 Copy Feature.          !     *~
            *          !  11867- Enlarge The A/P Balance Field    !     *~
            * 06/12/91 ! Added 'CALLFREE'                         ! SID *~
            * 10/12/92 ! Added Default Currency Code.             ! JDH *~
            *          ! PRR 12022 Added Fax Number (no area code)!     *~
            *          ! PRR 12299 No longer tests USERINFO.      !     *~
            *          ! PRR 12410 No longer shares text, now copy!     *~
            * 02/27/07 ! (AWD001) mod to allow 'I' in confirmation! CMG *~
            *************************************************************
        dim                                                              ~
            address$(6)30,               /* VENDOR BILLING ADDRESS     */~
            balance$14,                  /* OUTSTANDING BALANCE        */~
            billsdue$5,                  /* BILLS DUE (DAYS)           */~
            bfaddress$(6)30,             /* VENDOR BUY FROM ADDRESS    */~
            bfcode$6,                    /* VENDOR BUY FROM CODE       */~
            bfcontact$20,                /* BUY FROM CONTACT NAME      */~
            bfdescr$30,                  /* BUY FROM DESCRIPTION       */~
            bffac$(5)1,                  /* FAC'S FOR LINEAR INPUT     */~
            bfphone$10,                  /* BUY FROM PHONE NUMBER      */~
            cashacct$16,                 /* CASH IN BANK ACCOUNT       */~
            cashacctdescr$32,            /* CASH ACCOUNT DESCRIPTION   */~
            cvend$9,                     /* VENCOR CODE FOR COPY       */~
            confirm$1,                   /* CONFORMATION FLAG          */~
            contact$20,                  /* CONTACT NAME               */~
            curr_on_flag$1,              /* Multi-currency on? Y or N  */~
            currency$4, currdesc$30,     /* Currency code, description */~
            curr_msg$13,                 /* Screen message for currency*/~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* SCREEN DISPLAY DATE        */~
            dflts$(3)200,                /* Defaults Record            */~
            discacct$16,                 /* DISCOUNT ACCOUNT           */~
            discacctdescr$32,            /* DISCOUNT ACCT. DESCRIPTION */~
            discpercent$5,               /* DISCOUNT PERCENT           */~
            discsdue$5,                  /* DISCOUNTS DUE (DAYS)       */~
            edtmessage$79,               /* "TO MODIFY VALUES..." TEXT */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fax$7,                       /* Fax Number sans area code  */~
            frgtacct$16,                 /* VENDOR FREIGHT EXPENSE     */~
            frgtacctdescr$32,            /* VENDOR FREIGHT EXPENSE     */~
            fterms$1,                    /* FREIGHT TERMS              */~
            fob$30,                      /* F.O.B                      */~
            history$87,                  /* Really Filler              */~
            i$(24)80,                    /* SCREEN IMAGE -- NOT USED   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            interimgldescr$32,           /* INTERIM GL LIABILITY ACCT  */~
            intrmacct$16,                /* AND DESCRIPTION            */~
            lastven$9,                   /* LAST VENDOR PROCESSED      */~
            lfac$(20)1,                  /* FAC'S FOR LINEAR INPUT     */~
            main$9,                      /* Main Vendor # (Recap)      */~
            misc$50,                     /* Misc                       */~
            maindescr$30,                /* Main Vendor Description    */~
            payacct$16,                  /* PAYABLES ACCOUNT TYPE      */~
            payacctdescr$32,             /* PAYABLES ACCT. DESCRIPTION */~
            pcvaracct$16,                /* Prc-Var Variance Account   */~
            pcvaracctdescr$32,           /* Prc-Var Variance Acct Descr*/~
            pfd$(3)79,                   /* PF Key Descriptions        */~
            pfk$(20)1,                   /* PF Keys Available (HEX)    */~
            phone$10,                    /* PHONE NUMBER THIS VENDOR   */~
            puracct$16,                  /* PURCHASES ACCOUNT          */~
            puracctdescr$32,             /* PURCHASES ACCT. DESCRIPTION*/~
            readkey$50,                  /* Misc use Read Key          */~
            rest$98,                     /* REST OF VENDOR RECORD      */~
            record$(2)85,                /* VENDOR1 RECORD (APPENDIX)  */~
            svend$9, sdescr$30,          /* SAVE AREAS                 */~
            scr%(2,16), set%(255),       /* Enable arrays              */~
            scrnhdr$79,                  /* Screen Header Line         */~
            ship$30,                     /* SHIP VIA                   */~
            sysacct$(5)9,                /* AP System Accounts Defaults*/~
            taxable$1,                   /* Y/N                        */~
            temp$5,                      /* TEMPORARY VARIABLE         */~
            ten99$4,                     /* 1099 Category Code         */~
            ten99descr$30,               /* 1099 Category Descr        */~
            text$10,                     /* NOT USED                   */~
            texta$(392,1)70,             /* Text Matrix for TXTINSUB   */~
            textid$4,                    /* X-ref to Part Text         */~
            textmsg$79,                  /* Message to TXTINSUB        */~
            tin$12,                      /* Tax Identification Number  */~
            type$4,                      /* VENDOR TYPE (LIKE INV CAT.)*/~
            typedescr$30,                /* Vendor Type Description    */~
            userid$3,                    /* USERID-TO GET PAYABLES DATE*/~
            vencode$9,                   /* VENDOR CODE                */~
            vencoded$9,                  /* Vendor Code for Display    */~
            vendescr$30,                 /* Vendor Descrip (Internal)  */~
            vendor$9                     /* Dummy Vendor Code          */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM OPENCHCK  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
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
            * # 2 ! GLMAIN   ! General Ledger.  (Account verification)  *~
            * # 3 ! VENDOR   ! Vendor Master File                       *~
            * # 4 ! SYSFILE2 ! System Information File (Default Accts)  *~
            * # 5 ! GENCODES ! General Codes File                       *~
            * # 6 ! TXTFILE  ! System Text File                         *~
            * # 7 ! CURMASTR ! Multi-Currency Master file               *~
            * # 9 ! VENDORBF ! Vendor Buy From File                     *~
            *************************************************************

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

            select # 4, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1 , keylen = 20

            select #5,  "GENCODES", varc, indexed, recsize = 128,        ~
                        keypos = 1, keylen = 24

            select #6,  "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select #07, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #9,  "VENDORBF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                               keypos =    1, keylen =  15

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, 0%, f2%( 3), 100%, rslt$( 3))
            call "OPENCHCK" (# 4, 0%, f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (# 5, 0%, f2%( 5),   0%, rslt$( 5))
            call "OPENCHCK" (# 9, 0%, f2%( 9),   0%, rslt$( 9))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Value And Press RETURN."

            REM Read in Defaults Record
            readkey$ = all(hex(00))
            call "READ100" (#3, readkey$, dflts%)
            if dflts% = 1% then get #3, str(dflts$())

            REM Read in System Defaults Record
            call "READ100" (#4, "MODULE.DEFAULTS.AP  ", f1%(4))
                if f1%(4) = 0 then L09210
            get #4, using L09190, sysacct$()
L09190:     FMT XX(36), 5*CH(9)

L09210:     REM See if Vendor Type Codes are to be edited
            readkey$ = str(readkey$,,9) & "VEN TYPES"
            call "READ100" (#5, readkey$, types%)

            call "EXTRACT" addr ("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            REM See if this User is a module administrator
            call "CMSMACHK" ("VBK", lfac$(1), lfac$(2))
                if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%

            gosub init_enables

            str(scrnhdr$,62) = "VENINPUT: " & str(cms2v$,,8)

*        Check for Multi-Currency
            curr_on_flag$ = "N" : curr_msg$ = " "
            call "READ100" (#04, "SWITCHS.CUR", f1%(4%))
            if f1%(4%) <> 0% then get #04 using L09460, curr_on_flag$
L09460:         FMT POS(21), CH(1)
            if curr_on_flag$ <> "Y" then L10000
               call "OPENCHCK" (#07, 0%, f2%(7%),   0%, rslt$(7%))
               curr_msg$ = "Currency Code"

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Input mode Main Program.                                  *~
            *************************************************************

        inputmode
            vendor_default% = 0%

            init(" ") errormsg$, inpmessage$, main$, maindescr$,         ~
                      vencode$, vendescr$, address$(), contact$, phone$, ~
                      puracct$, puracctdescr$, payacct$, payacctdescr$,  ~
                      cashacct$, cashacctdescr$, discacct$, rest$,       ~
                      discacctdescr$, billsdue$, discsdue$, discpercent$,~
                      balance$, confirm$, fterms$, fob$, ship$, taxable$,~
                      type$, record$(), text$, intrmacct$, pcvaracct$,   ~
                      interimgldescr$, tin$, ten99$, ten99descr$,        ~
                      typedescr$, frgtacct$, frgtacctdescr$, history$,   ~
                      pcvaracctdescr$, currency$, currdesc$, fax$

            call "ALLFREE"

            textid$ = all(hex(ff))
            call "TXTFUTIL" (#6, f2%(6), "INTL", textid$)

            mostin% = 0%
            for fieldnr% = 1% to 10%
                if fieldnr% <= mostin% then edit% = -1% else edit% = 1%
                gosub'161(fieldnr%, edit%)
                     if enabled% = 0% then L10400
L10250:         gosub'201(fieldnr%, 1%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <> 4% then L10320
                           if fieldnr% = 1% then L10250
L10290:                    fieldnr% = fieldnr% - 1%
                           gosub'161(fieldnr%, -1%)
                           if enabled% = 0% then L10290 else L10250
L10320:              if keyhit%  = 16% then L65000
                     if keyhit% <> 17% or fieldnr% <> 1% then L10390
                          vencode$ = all(hex(00))
                          keyhit%  = 0%
                          gosub L30000    /* Attempt to load defaults */
                          inpmessage$ = " "
                          vendor_default% = 1%
                          if f1%(3) = 1% then edtpg1
L10390:              if keyhit% <>  0 then       L10250
L10400:         gosub'151(fieldnr%)
                     if errormsg$<> " " and vencode$ > hex(00) then L10250
                     errormsg$ = " "
                mostin% = max(mostin%, fieldnr%)
                next fieldnr%

            mostin% = 0%
            for fieldnr% = 1% to  16%
                if fieldnr% <= mostin% then edit% = -1% else edit% = 1%
                gosub'162(fieldnr%, edit%)
                     if enabled% = 0% then L10600
L10520:         gosub'202(fieldnr%, 1%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then L10590
L10550:                   fieldnr% = fieldnr% - 1%
                          if fieldnr% = 0% then L10660
                          gosub'162(fieldnr%, -1%)
                          if enabled% = 0% then L10550 else L10520
L10590:              if keyhit% <>  0% then       L10520
L10600:         gosub'152(fieldnr%)
                     if errormsg$ = " " then L10650
                          if vencode$ > hex(00) then L10520
                          errormsg$ = " "
L10650:         mostin% = max(mostin%, fieldnr%)
L10660:         next fieldnr%

            gosub set_for_pass
            err% = 0%
            call "VENAPPND" (3%, record$(), text$, err%)
            if err% = 99 then inputmode


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        edtpg1
L11070:     gosub'201(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then       edtpg2
                  if keyhit%  =  8% then see_buy_from_address
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 28% then gosub edit_text
                  if keyhit% <>  0% and keyhit% <> 29% then L11070
            fieldnr% = cursor%(1%) - 4%
            if fieldnr% <  2% or   fieldnr% >  15% then L11070
            if fieldnr% >= 3% and  fieldnr% <=  8% then fieldnr% = 3%
            if fieldnr% >= 9% then fieldnr%  = fieldnr% - 5%
            if keyhit%  <> 29% then L11210
                if admin% <> 1% then L11200
                call "ENABLSUB" ("MODIFY", "VENINPUT", scr%(), set%(),   ~
                                 1%, fieldnr%, 0%, 0%)
L11200:         goto L11070
L11210:     gosub'161(fieldnr%, 2%)
                if enabled% = 0% then L11070

L11240:     gosub'201(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11240
            gosub'151(fieldnr%)
                      if errormsg$ <> " " and str(vencode$,,1%) > hex(00)~
                                                              then L11240
            goto L11070

        edtpg2
L11330:     gosub'202(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       edtpg1
                  if keyhit%  =  5% then       edtpg3
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 28% then gosub edit_text
                  if keyhit% <>  0% and  keyhit% <> 29% then L11330
            fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 16% then L11330
            if keyhit% <> 29% then L11460
                if admin% <> 1% then L11450
                call "ENABLSUB" ("MODIFY", "VENINPUT", scr%(), set%(),   ~
                                 2%, fieldnr%, 0%, 0%)
L11450:         goto L11330
L11460:     gosub'162(fieldnr%, 2%)
                if enabled% = 0% then L11330

L11490:     gosub'202(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11490
            gosub'152(fieldnr%)
                if errormsg$ = " " then L11550
                     if fieldnr% = 9% or fieldnr% = 10% then L11490
                     if str(vencode$,,1%) > hex(00)     then L11490
L11550:     goto L11330

        edtpg3
            gosub set_for_pass
            err%=4%
L11600:     call "VENAPPND" (4%, record$(), text$, err%)
                if err% = 99% then inputmode
                if err% = 16% then datasave
                if err% =  4% then edtpg2
                goto L11600  /* Don't think this should happen. */


        REM *************************************************************~
            *               E D I T   T E X T                           *~
            * --------------------------------------------------------- *~
            * Allow editing of Vendor Text.                             *~
            *************************************************************
        edit_text
            textmsg$ = "Vendor: " & vencode$ & ", '" & vendescr$ & "'"
            call "TXTINSUB" (#6, f2%(6), "002", textmsg$, textid$,       ~
                                                            texta$())
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            save% = 1%
            if str(vencode$,,1) <> hex(00) then L19160
                if admin% = 1% then L19110
                     save% = 0%  :  goto L19160
L19110:         keyhit% = 2%
                call "ASKUSER" (keyhit%, "SAVE DEFAULTS??",              ~
                     "PRESS PF-16 TO SAVE Defaults on file", "- OR -",   ~
                     "RETURN to use for current session only.")
                if keyhit% <> 16% then save% = 0%
L19160:     gosub L31000
            lastven$  = vencode$
            if str(vencode$,,1) = hex(00) then lastven$ = "Defaults"
            goto inputmode


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%, edit%)
*              EDIT%: 1=Input Mode; 2=Edit Mode, -1=Input/Prev Field
            call "ENABLSUB" ("SET", "VENINPUT", scr%(), set%(), 1%,      ~
                             fieldnr%, abs(edit%), enabled%)
            if admin% = 1% and str(vencode$,,1) = hex(00) and            ~
                               fieldnr% > 2% then enabled% = 1%
            if edit%   = 2% and enabled% = 0% then return
            inpmessage$ = " "
                  on fieldnr% gosub L20250,         /* Vendor Code      */~
                                    L20300,         /* Vendor Descrptn  */~
                                    L20390,         /* Remit To Addrss  */~
                                    L20480,         /* Vendor Contact   */~
                                    L20540,         /* Phone Number/Fax */~
                                    L20600,         /* Vendor Type Code */~
                                    L20660,         /* Main Vendor #    */~
                                    L20720,         /* Tax ID Number    */~
                                    L20780,         /* 1099 Category    */~
                                    L20840          /* Currency Code    */
                     return

L20250
*        Default/Enable for VENDOR CODE
            inpmessage$ = "Enter blank Vendor Code to search through" &  ~
                          " existing Vendors."
            return

L20300
*        Default/Enable for VENDOR DESCRIPTION
            if str(vencode$,,1) <> hex(00) then L20350
                vendescr$ = "* DEFAULTS *"
                enabled%  = 0%
                return
L20350:     inpmessage$ = "Vendor Description is used for alphabetic" &  ~
                          " searches."
            return

L20390
*        Default/Enable for REMIT TO NAME / ADDRESS
            inpmessage$ = "Remit To is where checks will be sent."
            if edit% <> 1% then return
                if dflts% = 0% then address$(1) = vendescr$ else         ~
                                    str(address$()) = str(dflts$(),40,180)
                if dflts% = 0% and str(vencode$,,1) = hex(00) then       ~
                                    address$(1) = " "
                if vendescr$ <> " " and address$(1) = " " then           ~
                   address$(1) = vendescr$
                return

L20480
*        Default/Enable for VENDOR CONTACT
            inpmessage$ = "Enter Name of Vendor Contact."
            if edit% <> 1% then return
                if dflts% = 1% then contact$ = str(dflts$(), 220, 20)
                return

L20540
*        Default/Enable for PHONE NUMBER/Fax
            inpmessage$ = "Enter Vendor's Phone Number & Fax Number."
            if edit% <> 1% then return
                if dflts% = 1% then phone$ = str(dflts$(), 240%, 10%)
                if dflts% = 1% then fax$   = str(dflts$(), 532%,  7%)
                return

L20600
*        Default/Enable for VENDOR TYPE CODE
            inpmessage$ = "Enter Vendor Type Code."
            if edit% <> 1% then return
                if dflts% = 1% then type$ = str(dflts$(), 477, 4)
                return

L20660
*        Default/Enable for MAIN VENDOR NUMBER
            inpmessage$ = "Enter Main Vendor Code (or leave blank)."
            if edit% <> 1% then return
                if dflts% = 1% then main$ = str(dflts$(), 494, 9)
                return

L20720
*        Default/Enable for TAX IDENTIFICATION NUMBER
            inpmessage$ = "Enter Vendor's Tax Identification Number."
            if edit% <> 1% then return
                if dflts% = 1% then tin$ = str(dflts$(), 503, 12)
                return

L20780
*        Default/Enable for 1099 Category Code
            inpmessage$ = "Enter Vendor's 1099 Category Code."
            if edit% <> 1% then return
                if dflts% = 1% then ten99$ = str(dflts$(), 515, 4)
                return

L20840
*        Default/Enable for Currency Code
            if curr_on_flag$ <> "Y" then enabled% = 0%
            if curr_on_flag$ <> "Y" then return
                inpmessage$ = "Enter Default Currency Code; Enter '?' "& ~
                              "or Partial for List."
                if edit% <> 1% then return
                  if dflts% = 1% then currency$ = str(dflts$(), 528%, 4%)
                  return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'162(fieldnr%, edit%)
            call "ENABLSUB" ("SET", "VENINPUT", scr%(), set%(), 2%,      ~
                             fieldnr%, abs(edit%), enabled%)
            if admin% = 1% and str(vencode$,,1) = hex(00) and            ~
                               fieldnr% <> 15% then enabled% = 1%
            if edit%    = 2% and enabled% = 0% then return
            inpmessage$ = " "
                  on fieldnr% gosub L21290,         /* Confirmation     */~
                                    L21350,         /* Freight Terms    */~
                                    L21410,         /* FOB              */~
                                    L21470,         /* Ship Via         */~
                                    L21530,         /* Taxable?         */~
                                    L21590,         /* G/L- Purchase    */~
                                    L21661,         /*    - Prc-Cst Var */~
                                    L21670,         /*    - Intrm Liab  */~
                                    L21750,         /*    - A/P         */~
                                    L21830,         /*    - Cash        */~
                                    L21910,         /*    - Discounts   */~
                                    L21973,         /*    - Freight     */~
                                    L21990,         /* Bills Due        */~
                                    L22100,         /* Discount Due     */~
                                    L22210,         /* Discount %       */~
                                    L22300          /* Current Balance  */
                     return

L21290
*        Default/Enable for CONFIRMATION
            inpmessage$ = "Is Purchase Order Confirmation Required? (Y/N)"
            if edit%   <> 1% then return
                if dflts% = 1% then confirm$ = str(dflts$(), 414, 1)
                return

L21350
*        Default/Enable for FREIGHT TERMS
            inpmessage$ = "Enter Vendor's Standard Freight Terms."
            if edit%   <> 1% then return
                if dflts% = 1% then fterms$ = str(dflts$(), 415, 1)
                return

L21410
*        Default/Enable for FOB
            inpmessage$ = "Enter Vendor's Standard FOB Terms."
            if edit%   <> 1% then return
                if dflts% = 1% then fob$ = str(dflts$(), 416, 30)
                return

L21470
*        Default/Enable for SHIP VIA
            inpmessage$ = "Enter Standard Shipping Instructions."
            if edit%   <> 1% then return
                if dflts% = 1% then ship$ = str(dflts$(), 447, 30)
                return

L21530
*        Default/Enable for TAXABLE
            inpmessage$ = "Are Purchases Normally Taxable? (Y/N)."
            if edit%   <> 1% then return
                if dflts% = 1% then taxable$ = str(dflts$(), 446, 1)
                return

L21590
*        Default/Enable for PURCHASES ACCOUNT
            inpmessage$ = "Enter default Purchases Account."
            if edit%   <> 1% or dflts% = 0% then return
                puracct$ = str(dflts$(), 250, 9)
                call "DESCRIBE"(#2, puracct$, puracctdescr$, 1%, f1%(2))
                call "GLFMT" (puracct$)
                return

L21661
*        Default/Enable for Price Cost Variance Account
            inpmessage$ = "Enter default Price-Cost Variance Account."
            if edit%   <> 1% or dflts% = 0% then return
                pcvaracct$ = str(dflts$(), 318, 9)
                call "DESCRIBE"(#2, pcvaracct$, pcvaracctdescr$,         ~
                                                1%, f1%(2))
                call "GLFMT" (pcvaracct$)
                return

L21670
*        Default/Enable for INTERIM LIABILITY ACCOUNT
            inpmessage$ = "Enter default Interim Liability Account."
            if edit%   <> 1% or dflts% = 0% then return
                intrmacct$ =  str(dflts$(), 481, 9)
                call "DESCRIBE"(#2, intrmacct$, interimgldescr$,1%,f1%(2))
                call "GLFMT" (intrmacct$)
                return

L21750
*        Default/Enable for ACCOUNTS PAYABLE ACCOUNT
            inpmessage$ = "Enter default Accounts Payable Account."
            if edit%   <> 1% or dflts% = 0% then return
                payacct$ = str(dflts$(), 259, 9)
                call "DESCRIBE" (#2, payacct$, payacctdescr$, 1%, f1%(2))
                call "GLFMT" (payacct$)
                return

L21830
*        Default/Enable for CASH IN BANK ACCOUNT
            inpmessage$ = "Enter default Cash in Bank Account."
            if edit%   <> 1% or dflts% = 0% then return
                cashacct$ = str(dflts$(), 268, 9)
                call "DESCRIBE"(#2, cashacct$, cashacctdescr$, 1%, f1%(2))
                call "GLFMT" (cashacct$)
                return

L21910
*        Default/Enable for DISCOUNTS TAKEN ACCOUNT
            inpmessage$ = "Enter default Discounts Taken Account."
            if edit%   <> 1% or dflts% = 0% then return
                discacct$ = str(dflts$(), 277, 9)
                call "DESCRIBE"(#2, discacct$, discacctdescr$, 1%, f1%(2))
                call "GLFMT" (discacct$)
                return

L21973
*        Default/Enable for VENDOR FREIGHT EXPENSE ACCOUNT
            inpmessage$ = "Enter default Vendor Freight Expense Account."
            if edit%   <> 1% or dflts% = 0% then return
                frgtacct$ = str(dflts$(), 519, 9)
                call "DESCRIBE"(#2, frgtacct$, frgtacctdescr$, 1%, f1%(2))
                call "GLFMT" (frgtacct$)
                return

L21990
*        Default/Enable for BILLS NET DUE
            inpmessage$ = "Use the Prox Feature if needed; e.g.: 10P" &  ~
                          " means bill are due on the tenth."
            if edit%   <> 1% or dflts% = 0% then return
                get str(dflts$()) using L22040, billsdue
L22040:              FMT XX(285), PD(14,4)
                call "CONVERT" (abs(billsdue), -0.001, billsdue$)
                if billsdue < 0 then                                     ~
                          str(billsdue$, len(billsdue$)+1, 1) = "P"
            return

L22100
*        Default/Enable for BILLS DISCOUNT DUE
            inpmessage$ = "Note the Prox Feature: 1P means that"     &   ~
                          " discounts may be taken until the first."
            if edit%   <> 1% or dflts% = 0% then return
                get str(dflts$()) using L22150, discsdue
L22150:              FMT XX(293), PD(14,4)
                call "CONVERT" (abs(discsdue), -0.001, discsdue$)
                if discsdue < 0 then                                     ~
                          str(discsdue$, len(discsdue$)+1, 1) = "P"
            return

L22210
*        Default/Enable for DISCOUNT PERCENT
            inpmessage$ = "Enter default Discount Percentage."
            if edit%   <> 1% or dflts% = 0% then return
                get str(dflts$()) using L22250, discpercent
L22250:              FMT XX(301), PD(14,4)
                call "CONVERT" (discpercent, -2.2, discpercent$)
                if str(discsdue$,,1) = "0" then discpercent$ = "0.00"
                return

L22300
*        Default/Enable for CURRENT A/P BALANCE
            inpmessage$ = "NOTE: Modification may cause an imbalance" &  ~
                          " to occur between Vendor and A/P."
            if edit%   <> 1% or dflts% = 0% then return
                balance$ = "0.00"
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E                           *~
            * --------------------------------------------------------- *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR BUY FROM ADDRESS.    *~
            *************************************************************

            deffn'221(bffieldnr%)
                inpmessage$ = " "
                 on bffieldnr% gosub  L22510         /* Buy From Code    */
                     return

L22510
*        Default/Enable for BUY FROM CODE
            inpmessage$ = "Leave blank to search through existing"    &  ~
                          " locations for this Vendor."
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
            scr%(1, 2) =  2% : set%( 2) =  2%      /* Vendor Dscr      */
            scr%(1, 3) =  3% : set%( 3) =  2%      /* Name/ Addrs      */
            scr%(1, 4) =  4% : set%( 4) =  2%      /* Contact          */
            scr%(1, 5) =  5% : set%( 5) =  2%      /* Phone #/Fax #    */
            scr%(1, 6) =  6% : set%( 6) =  2%      /* Vendor Type      */
            scr%(1, 7) =  7% : set%( 7) =  2%      /* Primary Vendor   */
            scr%(1, 8) =  8% : set%( 8) =  2%      /* Tax ID Number    */
            scr%(1, 9) =  9% : set%( 9) =  2%      /* 1099 Category    */
            scr%(1,10) = 10% : set%(10) =  2%      /* Currency Code    */

            scr%(2, 1) = 16% : set%(16) =  2%      /* Confimation      */
            scr%(2, 2) = 17% : set%(17) =  2%      /* Freight          */
            scr%(2, 3) = 18% : set%(18) =  2%      /* FOB              */
            scr%(2, 4) = 19% : set%(19) =  2%      /* Ship Via         */
            scr%(2, 5) = 20% : set%(20) =  2%      /* Tax              */
            scr%(2, 6) = 21% : set%(21) =  2%      /* GL-Purchases     */
            scr%(2, 7) = 31% : set%(31) =  2%      /*   -Prc-Cst Var   */
            scr%(2, 8) = 22% : set%(22) =  2%      /*   -Intrm Lby     */
            scr%(2, 9) = 23% : set%(23) =  2%      /*   -A/P           */
            scr%(2,10) = 24% : set%(24) =  2%      /*   -Cash          */
            scr%(2,11) = 25% : set%(25) =  2%      /*   -Discounts     */
            scr%(2,12) = 30% : set%(30) =  2%      /*   -Freight       */
            scr%(2,13) = 26% : set%(26) =  2%      /*   -Bill Due      */
            scr%(2,14) = 27% : set%(27) =  2%      /*   -Disc Due      */
            scr%(2,15) = 28% : set%(28) =  2%      /*   -Disc %        */
            scr%(2,16) = 29% : set%(29) =  0%      /*   -Balance       */

            call "ENABLSUB" ("INIT", "VENINPUT", scr%(), set%(),         ~
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

            call "READ100" (#3, vencode$, f1%(3))
            if f1%(3) = 0% then return

            if str(vencode$,,1) <> hex(00) or dflts% <> 1% then L30130
                put #3, str(dflts$())

L30130:     get #3, using L30630,                                         ~
                     vendescr$, address$(), contact$,                    ~
                     phone$, puracct$, payacct$, cashacct$,              ~
                     discacct$, billsdue, discsdue,                      ~
                     discpercent, balance, pcvaracct$, history$,         ~
                     confirm$, fterms$, fob$, taxable$, ship$, type$,    ~
                     intrmacct$, textid$, main$, tin$, ten99$,           ~
                     frgtacct$, currency$, fax$, rest$
            call "TXTFUTIL" (#6, f2%(6), "LOAD", textid$)   /*Load Text*/
            call "CONVERT" (discpercent, -2.2, discpercent$)
            call "CONVERT" (abs(billsdue), -0.001, billsdue$)
            call "CONVERT" (abs(discsdue), -0.001, discsdue$)
            call "CONVERT" (balance, -2.2, balance$)

        REM If the COPY feature involked, Set A/P Balance to 0.00 and    ~
            give users option to copy the text
            if keyhit% <> 3% then L30270
                if textid$ = hex(ffffffff) then L30266
                   u3% = 2%
                   call "ASKUSER" (u3%, "COPY TEXT ?",                   ~
                   "Press PF-16 to copy text on file", "- OR -",         ~
                   "Press RETURN to ignore the text.")
                if u3% <> 16% then L30264
                     call "TXTFUTIL" (#06, f2%(6%), "COPY", textid$)
                     goto L30266
L30264:         textid$ = all(hex(ff))
L30266:         balance$ = "0.00"

L30270:     if str(vencode$,,1) = hex(00) then vendescr$ = "* DEFAULTS *"
            call "DESCRIBE" (#2, puracct$,  puracctdescr$,  1%, f1%(2))
                call "GLFMT" (puracct$)
                if puracct$ = " " then                                   ~
                                  puracctdescr$ = "[Uses System Default]"
            call "DESCRIBE" (#2, pcvaracct$, pcvaracctdescr$, 1%, f1%(2))
                call "GLFMT" (pcvaracct$)
                if pcvaracct$ = " " then                                 ~
                                pcvaracctdescr$ = "[Uses System Default]"
            call "DESCRIBE" (#2, payacct$,  payacctdescr$,  1%, f1%(2))
                call "GLFMT" (payacct$)
                if payacct$ = " " then                                   ~
                                  payacctdescr$ = "[Uses System Default]"
            call "DESCRIBE" (#2, cashacct$, cashacctdescr$, 1%, f1%(2))
                call "GLFMT" (cashacct$)
                if cashacct$= " " then                                   ~
                                 cashacctdescr$ = "[Uses System Default]"
            call "DESCRIBE" (#2, discacct$, discacctdescr$, 1%, f1%(2))
                call "GLFMT" (discacct$)
                if discacct$= " " then                                   ~
                                 discacctdescr$ = "[Uses System Default]"
            call "DESCRIBE" (#2, intrmacct$,interimgldescr$, 1%, f1%(2))
                call "GLFMT" (intrmacct$)
                if intrmacct$=" " then                                   ~
                                interimgldescr$ = "[Uses System Default]"
            call "DESCRIBE" (#2, frgtacct$, frgtacctdescr$, 1%, f1%(2))
                call "GLFMT" (frgtacct$)
                if frgtacct$=" " then                                    ~
                                frgtacctdescr$ = "[Uses System Default]"
            if sgn(billsdue) = -1                                        ~
                    then str(billsdue$, len(billsdue$)+1, 1)="P"
            if sgn(discsdue) = -1                                        ~
                    then str(discsdue$, len(discsdue$)+1, 1)="P"
            if ten99$ = " " then L30550
                readkey$ = "1099 CATS" & ten99$
                call "READ100" (#5, readkey$, f1%(5))
                if f1%(5) = 1% then get #5 using L30540, ten99descr$
L30540:              FMT XX(24), CH(30)
L30550:     if types% = 0% or type$ = " " then L30590
                readkey$ = "VEN TYPES" & type$
                call "READ100" (#5, readkey$, f1%(5))
                if f1%(5) = 1% then get #5 using L30540, typedescr$
L30590:     record$() = vencode$
            if curr_on_flag$ = "Y" then call "DESCRIBE" (#07, currency$, ~
                                                   currdesc$, 0%, f1%(7))
            call "VENAPPND" (1%, record$(), text$, err%)
          return

L30630:     FMT XX(9),                   /* Vendor Code                */~
                CH(30),                  /* Vendor Descr (Internal)    */~
                6*CH(30),                /* Remit-To Name and Address  */~
                CH(20),                  /* Contact Name               */~
                CH(10),                  /* Phone Number               */~
                CH(9),                   /* Purchases Account          */~
                CH(9),                   /* Payables Account           */~
                CH(9),                   /* Cash in Bank Account       */~
                CH(9),                   /* Discount Account           */~
                PD(14,4),                /* Bills Due (Prox is < 0)    */~
                PD(14,4),                /* Discs Due (Prox is < 0)    */~
                PD(14,4),                /* Discount Percent           */~
                PD(14,4),                /* Outstanding Balance        */~
                CH(9),                   /* Price-Cost Variance Account*/~
                CH(87),                  /* Filler                     */~
                CH(01),                  /* Confirmation               */~
                CH(01),                  /* Freight Terms              */~
                CH(30),                  /* FOB                        */~
                CH(01),                  /* Taxable                    */~
                CH(30),                  /* Ship Via                   */~
                CH(04),                  /* Type                       */~
                CH(09),                  /* Interim Liability Acct     */~
                CH(4),                   /* Text ID X-Ref              */~
                CH(9),                   /* Main Vendor X-ref          */~
                CH(12),                  /* Tax Identification Number  */~
                CH(04),                  /* 1099 Category Code         */~
                CH(09),                  /* Freight Expense Account    */~
                CH(04),                  /* Currency Code              */~
                CH(07),                  /* Fax Number Sans Area Code  */~
                CH(62)                   /* Remainder of record        */


L31000: REM *************************************************************~
            *             W R I T E   D A T A   T O   F I L E           *~
            *                                                           *~
            * WRITE THE VENDOR TO THE FILE.  SINCE IT MIGHT BE EITHER   *~
            * NEW OR HELD, THEN WE HAVE TO WRITE OR REWRITE ACCORDINGLY.*~
            *************************************************************

            convert discpercent$ to discpercent
            convert balance$     to balance
            call "GLUNFMT" (puracct$)
            call "GLUNFMT" (pcvaracct$)
            call "GLUNFMT" (payacct$)
            call "GLUNFMT" (cashacct$)
            call "GLUNFMT" (discacct$)
            call "GLUNFMT" (intrmacct$)
            call "GLUNFMT" (frgtacct$)
            call "READ101" (#3, vencode$, f1%(3))
            if str(vencode$,,1) = hex(00) then vendescr$ = all(hex(00))
            put #3, using L31320,                                         ~
                    vencode$, vendescr$, address$(), contact$,           ~
                    phone$, puracct$, payacct$, cashacct$, discacct$,    ~
                    billsdue, discsdue, discpercent, balance,            ~
                    pcvaracct$, history$, confirm$,                      ~
                    fterms$, fob$, taxable$, ship$, type$, intrmacct$,   ~
                    textid$, main$, tin$, ten99$, frgtacct$, currency$,  ~
                    fax$, rest$
            if save% = 0% then L31240
                if f1%(3) = 1 then rewrite #3  else  write #3
L31240:     if str(vencode$,,1) <> hex(00) then L31280
                get #3, str(dflts$())
                dflts% = 1%
                goto L31300
L31280:     call "TXTFUTIL" (#6, f2%(6), "TOS2", textid$)   /*Save Text*/
            call "VENAPPND" (2%, record$(), text$, err%)
L31300:     return

L31320:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(30),                  /* VENDOR DESCRIPTION         */~
                6*CH(30),                /* NAME AND ADDRESS           */~
                CH(20),                  /* CONTACT NAME               */~
                CH(10),                  /* PHONE NUMBER               */~
                CH(9),                   /* PURCHASES ACCOUNT          */~
                CH(9),                   /* PAYABLES ACCOUNT           */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                CH(9),                   /* DISCOUNT ACCOUNT           */~
                PD(14,4),                /* BILLS DUE (PROX IS < 0)    */~
                PD(14,4),                /* DISCOUNTS DUE (PROX IS < 0)*/~
                PD(14,4),                /* DISCOUNT PERCENT           */~
                PD(14,4),                /* OUTSTANDING BALANCE        */~
                CH(9),                   /* Price Cost Variance Acct   */~
                CH(87),                  /* Filler                     */~
                CH(01),                  /* CONFIRMTION                */~
                CH(01),                  /* FRIEGHT TERMS              */~
                CH(30),                  /* FOB                        */~
                CH(01),                  /* TAXABLE                    */~
                CH(30),                  /* SHIP VIA                   */~
                CH(04),                  /* TYPE                       */~
                CH(09),                  /* INTERIM LIABILITY ACCT     */~
                CH(4),                   /* TEXT ID X-REF              */~
                CH(9),                   /* MAIN VENDOR NUMBER         */~
                CH(12),                  /* Tax Identification Number  */~
                CH(04),                  /* 1099 Category Code         */~
                CH(09),                  /* Freight Expense Account    */~
                CH(04),                  /* Currency Code              */~
                CH(07),                  /* Fax Number Sans Area Code  */~
                CH(62)                   /* REMAINDER OF RECORD        */


        REM *************************************************************~
            *      S E E   B U Y  F R O M  A D D R E S S E S            *~
            *                                                           *~
            *************************************************************

        see_buy_from_address
            init(" ") bfcode$, bfdescr$, bfaddress$(), bfcontact$,       ~
                      bfphone$, errormsg$
            bfdescr$ = hex(06) & "Select Buy From to Display"
            readkey$ = str(vencode$) & bfcode$
            call "PLOWCODE" (#9, readkey$, bfdescr$, 9%, 0.30, f1%(9))
                 if f1%(9) <> 0% then L32140
                   errormsg$ = "Buy From Address Not Selected / On File."
            goto L32450
L32140:     bfcode$ = str(readkey$,10)
            readkey$ = str(vencode$) & bfcode$
            call "READ100" (#9, readkey$, f1%(9))
            if f1%(9) <> 0% then L32300
                   errormsg$ = "No Buy From Address Exist for this Vendor"
            goto L32450
L32300:     get #9, using L32320, vendor$, bfdescr$, bfaddress$(),        ~
                                 bfcontact$, bfphone$
L32320:                   FMT    CH(9), POS(16), CH(30), 6*CH(30),       ~
                                 CH(20), CH(10)

L32450:     gosub display_buyfrom
                  if keyhit% =   8% then see_buy_from_address
                  if keyhit% <> 16% then L32450
                     errormsg$ = " "
                  goto edtpg1


        REM *************************************************************~
            *      S E T   V A R I A B L E S   G O I N G   O U T        *~
            *                                                           *~
            *************************************************************

        set_for_pass
              str(record$(),,9)= vencode$
        return

        REM *************************************************************~
            * I N P U T  /  E D I T   S C R E E N  -   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Input and Edit of Screen Page 1.                          *~
            *************************************************************

            deffn'201(fieldnr%, edit%)
                init(hex(8c)) lfac$()
                if fieldnr% = 0% then init(hex(86)) str(lfac$(),2)
                gosub set_pf1
                if vendor_default% <> 1% then L40055
                   str(pfd$(2),33,15) = " " : pfk$(4) = hex(ff)

L40055:         on fieldnr%   gosub L40125,         /* Vendor Code      */~
                                    L40125,         /* Vendor Descr     */~
                                    L40125,         /* Remit to Address */~
                                    L40110,         /* Vendor Contact   */~
                                    L40125,         /* Phone Number/Fax */~
                                    L40125,         /* Vendor Type Code */~
                                    L40125,         /* Main Vendor #    */~
                                    L40125,         /* Tax ID Number    */~
                                    L40125,         /* 1099 Category    */~
                                    L40125          /* Currency Code    */
                goto L40160

L40110:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40125:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40160:     str(scrnhdr$,1,50) = "Last Vendor: " & lastven$

L40170:     accept                                                       ~
               at (01,02), "MAINTAIN VENDOR MASTER FILES",               ~
               at (01,67), "Date: ",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), scrnhdr$               , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Vendor Code",                                ~
               at (05,30), fac(lfac$( 1)), vencode$             , ch(09),~
               at (06,02), "Vendor Descr (Internal)",                    ~
               at (06,30), fac(lfac$( 2)), vendescr$            , ch(30),~
               at (07,02), "REMIT-TO: Name           ",                  ~
               at (08,02), "          Address - 1    ",                  ~
               at (09,02), "          Address - 2    ",                  ~
               at (10,02), "          Address - 3    ",                  ~
               at (11,02), "          Address - 4    ",                  ~
               at (12,02), "          City, State, Zip",                 ~
               at (07,30), fac(lfac$( 3)), address$(1)          , ch(30),~
               at (08,30), fac(lfac$( 3)), address$(2)          , ch(30),~
               at (09,30), fac(lfac$( 3)), address$(3)          , ch(30),~
               at (10,30), fac(lfac$( 3)), address$(4)          , ch(30),~
               at (11,30), fac(lfac$( 3)), address$(5)          , ch(30),~
               at (12,30), fac(lfac$( 3)), str(address$(6),1,17), ch(17),~
               at (12,50), fac(lfac$( 3)), str(address$(6),19,2), ch(02),~
               at (12,54), fac(lfac$( 3)), str(address$(6),22,5), ch(05),~
               at (12,60), "-",                                          ~
               at (12,62), fac(lfac$( 3)), str(address$(6),27,4), ch(04),~
                                                                         ~
               at (13,02), "Vendor Contact",                             ~
               at (13,30), fac(lfac$( 4)), contact$             , ch(20),~
               at (14,02), "Phone Number",                               ~
               at (14,30), "(",                                          ~
               at (14,32), fac(lfac$( 5%)), str(phone$, 1%, 3%) , ch(03),~
               at (14,36), ")",                                          ~
               at (14,38), fac(lfac$( 5%)), str(phone$, 4%, 3%) , ch(03),~
               at (14,42), "-",                                          ~
               at (14,44), fac(lfac$( 5%)), str(phone$, 7%, 4%) , ch(04),~
               at (14,49), "Fax",                                        ~
               at (14,53), fac(lfac$( 5%)), str(fax$,   1%, 3%) , ch(03),~
               at (14,57), "-",                                          ~
               at (14,59), fac(lfac$( 5%)), str(fax$,   4%, 4%) , ch(04),~
               at (15,02), "Vendor Type Code",                           ~
               at (15,30), fac(lfac$( 6)), type$                , ch(04),~
               at (15,49), fac(hex(8c))  , typedescr$           , ch(30),~
               at (16,02), "Primary Vendor Code",                        ~
               at (16,30), fac(lfac$( 7)), main$                , ch(09),~
               at (16,49), fac(hex(8c))  , maindescr$           , ch(30),~
               at (17,02), "Tax Identification Number",                  ~
               at (17,30), fac(lfac$( 8)), tin$                 , ch(12),~
               at (18,02), "1099 Category Code",                         ~
               at (18,30), fac(lfac$( 9)), ten99$               , ch(04),~
               at (18,49), fac(hex(8c))  , ten99descr$          , ch(30),~
               at (19,02), fac(hex(8c)),   curr_msg$            , ch(13),~
               at (19,30), fac(lfac$(10%)), currency$           , ch(04),~
               at (19,49), fac(hex(8c)),   currdesc$            , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pfd$(1)                , ch(79),~
               at (23,02), fac(hex(8c)), pfd$(2)                , ch(79),~
               at (24,02), fac(hex(8c)), pfd$(3)                , ch(79),~
                    keys(str(pfk$())),                                   ~
                    key (keyhit%)

               if keyhit% <> 13 then L40470
                  call "MANUAL" ("VENINPUT")
                  goto L40170

L40470:        if keyhit% <> 15 then L40490
                  call "PRNTSCRN"
                  goto L40170

L40490:        if keyhit%  <> 3% then L40555
               if fieldnr% <> 3% then L40170
                     misc$ = hex(06) & "Select Vendor to Copy"
                     call "GETCODE" (#3, cvend$, misc$, 0%, 0, f1%(3))
                     if f1%(3) = 0% then L40170
                          svend$ = vencode$ : sdescr$ = vendescr$
                          vencode$  = cvend$
                          gosub L30000
                          vencode$ = svend$ : vendescr$ = sdescr$
                          if len(cvend$) = 1 then cvend$=cvend$ & hex(00)
                          cvend$ = str(cvend$,,len(cvend$)-1) & hex(00)
                          return clear all
                          goto edtpg1

L40555:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
         if edit% = 2% then L40660
*        Set PF Prompts and Keys for INPUT Mode
          pfd$(1) = "                                                  "&~
                    "             (13)Instructions"
          pfd$(2) = "(1)Start Over   (4)Previous Field                 "&~
                    "             (15)Print Screen"
          pfd$(3) = "(17)Change Defaults                               "&~
                    "             (16)Exit Program"
            str(pfk$()) = hex(0001040d0f1011ffffffffffffffffffffffffff)

            if fieldnr% = 1% then L40624
                str(pfd$(3),1,20) = " "  :  pfk$(7) = hex(ff) : goto L40625
L40624:         str(pfd$(2),17,17) = " "  :  pfk$(3) = hex(ff)
L40625:     if fieldnr% <> 3% then return
                str(pfd$(1),40,10) = hex(84) & "(3)COPY" & hex(8c)
                pfk$(8) = hex(03)
                return

L40660
*        Set PF Prompts and Keys for EDIT Mode
          pfd$(1) = "                                                  "&~
                    "             (13)Instructions"
          pfd$(2) = "(1)Start Over                   (8)See Buy From   "&~
                    "             (15)Print Screen"
          pfd$(3) = "                (5)Next Screen  (28)Edit Text     "&~
                    "             (16)Save Data   "
            str(pfk$()) = hex(000105080d0f101c1dffffffffffffffffffffffff)
            if str(vencode$,,1) <> hex(00) then return
                str(pfd$(3), 33, 20) = " " : pfk$(8) = hex(ff)
                str(pfd$(3), 64    ) = "(16)Retain Dflts"
                return

        REM *************************************************************~
            * I N P U T  /  E D I T   S C R E E N  -   P A G E   2      *~
            * --------------------------------------------------------- *~
            * Input and Edit Modes for Page 2.                          *~
            *************************************************************

            deffn'202(fieldnr%, edit%)
                init(hex(8c)) lfac$()
                if fieldnr% = 0% then init(hex(86)) lfac$()
                gosub set_pf2
                on fieldnr%  gosub  L41290,         /* Confirmation     */~
                                    L41290,         /* Freight Terms    */~
                                    L41290,         /* Freight on Board */~
                                    L41290,         /* Ship Via         */~
                                    L41290,         /* Taxable          */~
                                    L41290,         /* GL- Purchases    */~
                                    L41290,         /*   - Prc-Cst Var  */~
                                    L41290,         /*   - Interim Liab */~
                                    L41290,         /*   - A/P          */~
                                    L41290,         /*   - Cash in Bank */~
                                    L41290,         /*   - Discounts    */~
                                    L41290,         /*   - Frgt Expense */~
                                    L41290,         /* Bills Due        */~
                                    L41290,         /* Discount Due     */~
                                    L41290,         /* Discount%        */~
                                    L41290          /* Balance          */
                     goto L41360

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41290:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41360:     str(scrnhdr$,1,50) = "Last Vendor: " & lastven$

            if str(vencode$,,1) = hex(00) then vencoded$ = "Defaults"    ~
                                          else vencoded$ = vencode$
            accept                                                       ~
               at (01,02), "MAINTAIN VENDOR MASTER FILES",               ~
               at (01,67), "Date: ",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), scrnhdr$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
/*(AWD001)*/   at (05,02), "Confirmation Req'd (Y/N/I)",                 ~
               at (05,30), fac(lfac$( 1)), confirm$             , ch(01),~
               at (06,02), "Freight Terms",                              ~
               at (06,30), fac(lfac$( 2)), fterms$              , ch(01),~
               at (06,49), "A-Added, N-Not Added, C-Collect",            ~
               at (07,02), "Freight On Board (FOB)",                     ~
               at (07,30), fac(lfac$( 3)), fob$                 , ch(30),~
               at (08,02), "Ship Via",                                   ~
               at (08,30), fac(lfac$( 4)), ship$                , ch(30),~
               at (09,02), "Taxable? (Y/N)",                             ~
               at (09,30), fac(lfac$( 5)), taxable$             , ch(01),~
               at (10,02), "G/L- Purchases",                             ~
               at (10,30), fac(lfac$( 6)), puracct$             , ch(12),~
               at (10,49), fac(hex(8c)),   puracctdescr$        , ch(32),~
               at (11,02), "   - Price-Cost Variance",                   ~
               at (11,30), fac(lfac$( 7)), pcvaracct$           , ch(12),~
               at (11,49), fac(hex(8c)),   pcvaracctdescr$      , ch(32),~
               at (12,02), "   - Interim Liabilities",                   ~
               at (12,30), fac(lfac$( 8)), intrmacct$           , ch(12),~
               at (12,49), fac(hex(8c)),   interimgldescr$      , ch(32),~
               at (13,02), "   - Accounts Payable",                      ~
               at (13,30), fac(lfac$( 9)), payacct$             , ch(12),~
               at (13,49), fac(hex(8c)),   payacctdescr$        , ch(32),~
               at (14,02), "   - Cash in Bank",                          ~
               at (14,30), fac(lfac$(10)), cashacct$            , ch(12),~
               at (14,49), fac(hex(8c)),   cashacctdescr$       , ch(32),~
               at (15,02), "   - Discounts Taken",                       ~
               at (15,30), fac(lfac$(11)), discacct$            , ch(12),~
               at (15,49), fac(hex(8c)),   discacctdescr$       , ch(32),~
               at (16,02), "   - Freight Expense",                       ~
               at (16,30), fac(lfac$(12)), frgtacct$            , ch(12),~
               at (16,49), fac(hex(8c)),   frgtacctdescr$       , ch(32),~
               at (17,02), "Bills Due (days)",                           ~
               at (17,30), fac(lfac$(13)), billsdue$            , ch(05),~
               at (18,02), "Discount Due (days)",                        ~
               at (18,30), fac(lfac$(14)), discsdue$            , ch(05),~
               at (19,02), "Discount Percent",                           ~
               at (19,30), fac(lfac$(15)), discpercent$         , ch(05),~
               at (20,02), "A/P Balance",                                ~
               at (20,30), fac(lfac$(16)), balance$             , ch(14),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfd$(1)              , ch(79),~
               at (23,02), fac(hex(8c)),   pfd$(2)              , ch(79),~
               at (24,02), fac(hex(8c)),   pfd$(3)              , ch(79),~
                     keys(str(pfk$())),                                  ~
                     key (keyhit%)

               if keyhit% <> 13 then L41930
                  call "MANUAL" ("VENINPUT")
                  goto L41360

L41930:        if keyhit% <> 15 then L41970
                  call "PRNTSCRN"
                  goto L41360

L41970:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
          if edit% = 2% then L42130
*        Set PF Prompts and Keys for INPUT Mode.
          pfd$(1) = "                                                  "&~
                    "             (13)Instructions"
          pfd$(2) = "(1)Start Over   (4)Previous Field                 "&~
                    "             (15)Print Screen"
          pfd$(3) = "                                                  "&~
                    "                             "
            str(pfk$()) = hex(0001040d0fffffffffffffffffffffffffffffff)
            return

L42130
*        Set PF Prompts and Keys for EDIT Mode.
          pfd$(1) = "                                                  "&~
                    "             (13)Instructions"
          pfd$(2) = "(1)Start Over   (4)Prev Screen                    "&~
                    "             (15)Print Screen"
          pfd$(3) = "                (5)Next Screen  (28)Edit Text     "&~
                    "             (16)Save Data   "
            str(pfk$()) = hex(000104050d0f101c1dffffffffffffffffffffff)
            str(pfd$(3),17,15) = " " /* Remove when Appnedix added     */
            if str(vencode$,,1) <> hex(00) then return
                str(pfd$(3), 33, 20) = " " : pfk$(8) = hex(ff)
                str(pfd$(3), 64    ) = "(16)Retain Dflts"
                return


        REM *************************************************************~
            * I N P U T  /  E D I T   S C R E E N                       *~
            * --------------------------------------------------------- *~
            * Input and Edit of Buy From Addresses.                     *~
            *************************************************************

        display_buyfrom
            init (hex(84)) bffac$(1)

L43240:     accept                                                       ~
               at (01,02), "VENDOR BUY FROM LOCATIONS",                  ~
               at (01,67), "Date: ",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), scrnhdr$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Vendor Code",                                ~
               at (06,30), fac(bffac$( 1)), vencode$            , ch(09),~
               at (06,49), fac(hex(8c))   , vendescr$           , ch(30),~
               at (07,02), "Buy From Code",                              ~
               at (07,30), fac(bffac$( 1)), bfcode$             , ch(06),~
               at (08,02), "BUY FROM: Name           ",                  ~
               at (09,02), "          Address - 1    ",                  ~
               at (10,02), "          Address - 2    ",                  ~
               at (11,02), "          Address - 3    ",                  ~
               at (12,02), "          Address - 4    ",                  ~
               at (13,02), "          City, State, Zip",                 ~
               at (08,30), fac(bffac$( 1)), bfaddress$(1)       , ch(30),~
               at (09,30), fac(bffac$( 1)), bfaddress$(2)       , ch(30),~
               at (10,30), fac(bffac$( 1)), bfaddress$(3)       , ch(30),~
               at (11,30), fac(bffac$( 1)), bfaddress$(4)       , ch(30),~
               at (12,30), fac(bffac$( 1)), bfaddress$(5)       , ch(30),~
               at (13,30), fac(bffac$(1)),str(bfaddress$(6),1,17),ch(17),~
               at (13,50), fac(bffac$(1)),str(bfaddress$(6),19,2),ch(02),~
               at (13,54), fac(bffac$(1)),str(bfaddress$(6),22,5),ch(05),~
               at (13,60), "-",                                          ~
               at (13,62), fac(bffac$(1)),str(bfaddress$(6),27,4),ch(04),~
                                                                         ~
               at (14,02), "Vendor Contact",                             ~
               at (14,30), fac(bffac$( 1)), bfcontact$          , ch(20),~
               at (15,02), "Phone Number",                               ~
               at (15,30), "(",                                          ~
               at (15,32), fac(bffac$( 1)), str(bfphone$, 1, 3) , ch(03),~
               at (15,36), ")",                                          ~
               at (15,38), fac(bffac$( 1)), str(bfphone$, 4, 3) , ch(03),~
               at (15,42), "-",                                          ~
               at (15,44), fac(bffac$( 1)), str(bfphone$, 7, 4) , ch(04),~
               at (16,02), "Buy From Description",                       ~
               at (16,30), fac(bffac$( 1)), bfdescr$            , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,34), "(8)Select Buy From"                 ,        ~
               at (22,65), "(13)Instruction"                    ,        ~
               at (23,65), "(15)Print Screen"                   ,        ~
               at (24,65), "(16)Return   "                      ,        ~
                    keys(hex(00080d0f10))                       ,        ~
                    key (keyhit%)

               if keyhit% <> 13 then L43705
                     call "MANUAL" ("VENINPUT")
                     goto L43240

L43705:        if keyhit% <> 15 then L43710
                     call "PRNTSCRN"
                     goto L43240

L43710:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the fields on Page 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$, inpmessage$ = " "
                  on fieldnr% gosub L50190,         /* Vendor Code      */~
                                    L50320,         /* Vendor Descriptn */~
                                    L50370,         /* Remit-To Addrs   */~
                                    L50420,         /* Vendor Contact   */~
                                    L50450,         /* Phone Number/Fax */~
                                    L50480,         /* Vendor Type Code */~
                                    L50580,         /* Main Vendor #    */~
                                    L50690,         /* Tax ID Number    */~
                                    L50720,         /* 1099 Category    */~
                                    L50840          /* Currency Code    */
                     return

L50190
*        Test data for VENDOR CODE
            if vencode$ <> " " then L50270
            vendescr$ = hex(06) & "Select Vendor To Edit"
            call "GETCODE" (#3, vencode$, vendescr$, 0%, 1, f1%(3))
                if f1%(3) <> 0 then L50270
                vendescr$ = " "
                errormsg$ = hex(00)
                return
L50270:     gosub L30000
                if f1%(3) = 0 then return
                return clear all
                goto edtpg1

L50320
*        Test data for VENDOR DESCRIPTION (aka SORT NAME)
            if vendescr$ <> " " then return
                errormsg$ = "Vendor Description cannot be blank."
                return

L50370
*        Test for Remit To Address
            if address$(1) <> " " then return
                errormsg$ = "Vendor Name cannot be left blank."
                return

L50420
*        Test data for VENDOR CONTACT
            return

L50450
*        Test data for PHONE NUMBER/FAX NUMBER
            return

L50480
*        Test data for VENDOR TYPE CODE
            if types% = 0% or type$ = " " then return
                typedescr$ = hex(06) & "Select Vendor Type Code"
                readkey$   = "VEN TYPES" & type$
                call "PLOWCODE" (#5, readkey$, typedescr$, 9%, .3, f1%(5))
                if f1%(5) = 1% then L50550
                     errormsg$ = "Invalid Vendor Type Code." : return
L50550:         type$ = str(readkey$,10)
                return

L50580
*        Test data for MAIN (PRIMARY) VENDOR NUMBER (aka RECAP)
            maindescr$ = " "
            if main$   = " " then return
            if main$   = vencode$ then return
            maindescr$ = hex(06) & "Select Primary Vendor Code"
            call "GETCODE" (#3, main$, maindescr$, 0%, 1, f1%(3))
            if f1%(3) = 1% then return
                errormsg$  = "Invalid Vendor Code for Primary Vendor"
                maindescr$ = " "
                return

L50690
*        Test data for TAX IDENTIFICATION NUMBER
            return

L50720
*        Test data for 1099 Category
            ten99descr$ = " "
            if ten99$   = " " then return
            readkey$ = "1099 CATS" & ten99$
            ten99descr$ = hex(06) & "Select 1099 Category"
            call "PLOWCODE" (#5, readkey$, ten99descr$, 9%, 0.30, f1%(5))
            if f1%(5) = 1% then L50810
                errormsg$ = "Invalid 1099 Category Code"
                return
L50810:     ten99$ = str(readkey$,10)
            return

L50840
*        Currency code                         CURRENCY$
            currdesc$ = " " : if currency$ = " " then return
            if curr_on_flag$ <> "Y" then return  /* Just to make sure */
            call "GETCODE" (#07, currency$, currdesc$, 0%, .3, f1%(7))
            if f1%(7) <> 0% then return
                errormsg$ = "Currency code not found on file." : return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$, inpmessage$ = " "
                  on fieldnr% gosub L51240,         /* Confirmation     */~
                                    L51290,         /* Freight Terms    */~
                                    L51340,         /* FOB              */~
                                    L51370,         /* Ship Via         */~
                                    L51400,         /* Taxable          */~
                                    L51450,         /* GL- Purchases    */~
                                    L51530,         /*   - Prc-Cst Var  */~
                                    L51540,         /*   - Interim Liab */~
                                    L51640,         /*   - A/P          */~
                                    L51730,         /*   - Cash in Bank */~
                                    L51820,         /*   - Discounts    */~
                                    L51894,         /*   - Frgt Expnse  */~
                                    L51910,         /* Bills Due        */~
                                    L52170,         /* Discount Due     */~
                                    L52440,         /* Discount %       */~
                                    L52490          /* A/P Balance      */
                  return

L51240
*        Test data for CONFIRMATION
/*(AWD001)*/
            if pos("INY" = confirm$) = 0 then                            ~
                     errormsg$="Must be 'Y' 'N' -or- 'I'. Please reenter."
            return

L51290
*        Test data for FREIGHT TERMS
            if pos(" ANC" = fterms$) = 0 then                            ~
                  errormsg$ = "Must be 'A', 'N', or 'C'. Please reenter."
            return

L51340
*        Test data for FREIGHT ON BOARD
            return

L51370
*        Test data for Ship Via
            return

L51400
*        Test data for TAXABLE
            if pos("YN" = taxable$) = 0 then                             ~
                      errormsg$="Must be 'Y' -or- 'N'. Please reenter."
                 return

L51450
*        Test data for PURCHASES ACCOUNT
            puracctdescr$ = "[Uses System Default]"
            if vencode$ < hex(01) then puracctdescr$ = " "
            if puracct$ = " " then return
            call "GETCODE" (#2, puracct$, puracctdescr$, 1%, 0, f1%(2))
            if f1%(2) = 1 then return
                errormsg$ = "Purchases Account Not On File: " & puracct$
                return

L51530
*        Test data for Price Cost Variance Account
            pcvaracctdescr$ = "[Uses System Default]"
            if vencode$ < hex(01) then pcvaracctdescr$ = " "
            if pcvaracct$ = " " then return
            call "GETCODE" (#2,pcvaracct$,pcvaracctdescr$,1%,0, f1%(2))
            if f1%(2) = 1 then return
                errormsg$ = "Price Cost Variance Account Not On File: "  ~
                             & pcvaracct$
                return

L51540
*        Test data for INTERIM LIABILITIES ACCOUNT
            interimgldescr$ = "[Uses System Default]"
            if vencode$ < hex(01) then interimgldescr$ = " "
            if intrmacct$ = " " then return
            call "GETCODE" (#2, intrmacct$, interimgldescr$, 1%, 0,f1%(2))
            if f1%(2) = 1 then return
                errormsg$ = "Interim Liability Account Not On File: " &  ~
                                                               intrmacct$
            return

L51640
*        Test data for PAYABLES ACCOUNT
            payacctdescr$ = "[Uses System Default]"
            if vencode$ < hex(01) then payacctdescr$ = " "
            if payacct$ = " " then return
            call "GETCODE" (#2, payacct$, payacctdescr$, 1%, 0, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Payables Account not on file: " & payacct$
                return

L51730
*        Test data for CASH IN BANK ACCOUNT
            cashacctdescr$ = "[Uses System Default]"
            if vencode$ < hex(01) then cashacctdescr$ = " "
            if cashacct$ = " " then return
            call "GETCODE"(#2, cashacct$, cashacctdescr$, 1%, 0, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Cash account not on file: " & cashacct$
                return

L51820
*        Test data for DISCOUNTS TAKEN ACCOUNT
            discacctdescr$ = "[Uses System Default]"
            if vencode$ < hex(01) then discacctdescr$ = " "
            if discacct$ = " " then return
            call "GETCODE"(#2, discacct$, discacctdescr$, 1%, 0, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Discount Account Not On File :" & discacct$
                return

L51894
*        Test data for FREIGHT EXPENSE ACCOUNT
            frgtacctdescr$ = "[Uses System Default]"
            if vencode$ < hex(01) then frgtacctdescr$ = " "
            if frgtacct$ = " " then return
            call "GETCODE"(#2, frgtacct$, frgtacctdescr$, 1%, 0, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "Freight Expense Account Not On File :"      ~
                                                             & frgtacct$
                return
L51910
*        Test data for Bills Due
            if billsdue$ = " " then billsdue$ = "0"
            if pos(billsdue$ = "P") = 0 then L52060
                if pos(billsdue$ = "P") = 1 then L52040
                temp$ = str(billsdue$, 1, pos(billsdue$ = "P")-1)
                call "NUMTEST" (temp$, 1, 31, errormsg$, 0.0, billsdue)
                if errormsg$ <> " " then return
                billsdue$ = temp$ & "P"
                billsdue  = -(abs(billsdue))
                return

L52040:         errormsg$ = "'P' must be entered AFTER number of days"
                return
L52060:  /* Compute for Regular (Non-Prox)                */
                call "NUMTEST" (billsdue$, 0, 9999, errormsg$,           ~
                                  0.0, billsdue)
                return

L52170
*        Test data for DISCOUNT DUE
            if discsdue$ = " " then discsdue$ = "0"
            if pos(discsdue$ = "P") = 0 then L52320
                if pos(discsdue$ = "P") = 1 then L52280
                temp$ = str(discsdue$, 1, pos(discsdue$ = "P")-1)
                call "NUMTEST" (temp$, 1, 31, errormsg$, 0.0, discsdue)
                if errormsg$ <> " " then return
                discsdue$ = temp$ & "P"
                discsdue  = -(abs(discsdue))   /* For file layout */
                return

L52280:         errormsg$ = "'P' must be entered AFTER number of days"
                return

L52320:     /* Compute for regular (non-prox)                */
                call "NUMTEST" (discsdue$, 0, 9999, errormsg$, 0.0,      ~
                                 discsdue)
            return
L52440
*        Test data for DISCOUNT PERCENT
            call "NUMTEST" (discpercent$, 0, 100, errormsg$, 2.2,        ~
                                                             discpercent)
            return

L52490
*        Test data for OUTSTANDING BALANCE
            call "NUMTEST" (balance$, -9e11, 9e11, errormsg$,2.2,balance)
            return


                     bfdescr$ = " "  :  errormsg$ = hex(00)
                     return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
