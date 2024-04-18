        REM *************************************************************~
            *                                                           *~
            *  V   V  EEEEE  N   N  DDDD    SSS   PPPP    SSS   BBBB    *~
            *  V   V  E      NN  N  D   D  S      P   P  S      B   B   *~
            *  V   V  EEEE   N N N  D   D   SSS   PPPP    SSS   BBBB    *~
            *   V V   E      N  NN  D   D      S  P          S  B   B   *~
            *    V    EEEEE  N   N  DDDD    SSS   P       SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENDSPSB - DISPLAY VENDOR MASTER FILE INFORMATION.        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/28/89 ! ORIGINAL                                 ! RJM *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 10/14/92 ! Added Fax # and Default Currency Code.   ! JDH *~
            *          ! PRR 12314 Fixed Buy From error msg.      !     *~
            *************************************************************

            sub "VENDSPSB" (vencode$,            /* Vendor Code        */~
                            #1,                  /* VENDOR   File UFB  */~
                            #2)                  /* VENDORBF File UFB  */

*          NOTE, VENCODE$ May be Modified. Take appropriate Precautions
*                in the calling program.

        dim                                                              ~
            address$(6)30,               /* VENDOR BILLING ADDRESS     */~
            balance$10,                  /* OUTSTANDING BALANCE        */~
            bfcode$6,                    /* VENDOR BUY FROM CODE       */~
            bfaddrs$(6)30,               /* VENDOR BF BILLING ADDRESS  */~
            bfcontact$20,                /* BUY FROM CONTACT NAME      */~
            bfphone$10,                  /* BUY FROM PHONE NUMBER      */~
            bfconfirm$1,                 /* BUY FROM CONFORMATION FLAG */~
            bffterms$1,                  /* BUY FROM FREIGHT TERMS     */~
            bffob$30,                    /* BUY FROM F.O.B             */~
            bfship$30,                   /* BUY FROM SHIP VIA          */~
            bftextid$4,                  /* BUY FROM X-REF TO TEXT     */~
            billsdue$5,                  /* BILLS DUE (DAYS)           */~
            confirm$1,                   /* CONFORMATION FLAG          */~
            contact$20,                  /* CONTACT NAME               */~
            curr_on_flag$1,              /* Multi-currency on? Y or N  */~
            currency$4, currdesc$32,     /* Currency code, description */~
            curr_msg$13,                 /* Screen message for currency*/~
            date$8,                      /* SCREEN DISPLAY DATE        */~
            discpercent$5,               /* DISCOUNT PERCENT           */~
            discsdue$5,                  /* DISCOUNTS DUE (DAYS)       */~
            errormsg$79,                 /* Error Message              */~
            fax$7,                       /* Fax Number sans area code  */~
            fterms$1,                    /* FREIGHT TERMS              */~
            fob$30,                      /* F.O.B                      */~
            inpmessage$79,               /* MISC MESSAGE               */~
            line2$79,                    /* SCREEN HEADING             */~
            pfd$(3)79,                   /* PF Key Descriptions        */~
            pfk$20,                      /* PF Keys Available (HEX)    */~
            phone$10,                    /* PHONE NUMBER THIS VENDOR   */~
            readkey$50,                  /* Misc use Read Key          */~
            save_vendor$9,               /* VENDOR CODE SAVE VARIABLE  */~
            ship$30,                     /* SHIP VIA                   */~
            texta$(392,1)70,             /* TEXT MATRIX FOR TXTDSPLY   */~
            textid$4,                    /* X-ref to Part Text         */~
            textmsg$79,                  /* Message to TXTINSUB        */~
            tin$12,                      /* Tax Identification Number  */~
            userid$3,                    /* USERID-TO GET PAYABLES DATE*/~
            vencode$9,                   /* VENDOR CODE                */~
            vendescr$32                  /* Vendor Descrip (Internal)  */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */


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
            * # 1 ! VENDOR   ! Vendor Master File                       *~
            * # 2 ! VENDORBF ! Vendor Buy From File                     *~
            * #04 ! SYSFILE2 ! System Information File (Default Accts)  *~
            * # 6 ! TXTFILE  ! System Text File                         *~
            * #07 ! CURMASTR ! Multi-Currency Master file               *~
            *************************************************************

            select #04, "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 1 , keylen = 20

            select #6,  "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select #07, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            if fs4% = 0% then call "OPENCHCK" (#4, fs4%, f2%(4%), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************
            save_vendor$ = vencode$
            call "GETUFBS1" addr(#1, f1%(1))
            if f1%(1) = 1% then L09100
                call "OPENCHCK" (#1, fs1%, f2%(1), 0%, " ")
                     if fs1% <> 1% then exit_program
L09100:     if fs2% = 0% then call "OPENCHCK" (#2, fs2%, f2%(2), 0%, " ")

            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = "VENDSPSB: " & str(cms2v$,,8)
            call "EXTRACT" addr("ID", userid$)

L09160:     temp = 0
            if vencode$ <> " " then temp = 99
            call "GETCODE" (#1, vencode$, vendescr$, 1%, temp, f1%(1))
                if f1%(1) = 0% then exit_program

*        Check for Multi-Currency
                curr_on_flag$ = "N" : curr_msg$ = " "
                call "READ100" (#04, "SWITCHS.CUR", f1%(4%))
                if f1%(4%) <> 0% then get #04 using L09260, curr_on_flag$
L09260:             FMT POS(21), CH(1)
                if curr_on_flag$ <> "Y" then L10000
                   if fs7% = 0% then call "OPENCHCK" (#7, fs7%, f2%(7%), ~
                                                                 0%, " ")
                   curr_msg$ = "Currency Code"

L10000: REM *************************************************************~
            *       S E E   V E N D O R   D A T A                       *~
            *                                                           *~
            * GET DATA THEN DISPLAY                                     *~
            *************************************************************

            init(" ") address$(), contact$, phone$, billsdue$, discsdue$,~
                      discpercent$, balance$, confirm$, fterms$, fob$,   ~
                      ship$, tin$, inpmessage$, currency$, currdesc$, fax$

            get #1, using L10140, address$(), contact$, phone$, billsdue, ~
                                 discsdue, discpercent, balance,         ~
                                 confirm$, fterms$, fob$, ship$, textid$,~
                                 tin$, currency$, fax$
L10140:     FMT XX(39), 6*CH(30), CH(20), CH(10), XX(36), 4*PD(14,4),    ~
                XX(96), CH(1), CH(1), CH(30), XX(1), CH(30), XX(13),     ~
                CH(4), XX(9), CH(12), POS(528), CH(4), CH(7)

            call "CONVERT" (discpercent, 2.2, discpercent$)
            call "CONVERT" (abs(billsdue), 0.0, billsdue$)
            call "CONVERT" (abs(discsdue), 0.0, discsdue$)
            call "CONVERT" (balance, 2.2, balance$)
            if sgn(billsdue) = -1                                        ~
                    then str(billsdue$, len(billsdue$)+1, 1)="P"
            if sgn(discsdue) = -1                                        ~
                    then str(discsdue$, len(discsdue$)+1, 1)="P"
            if curr_on_flag$ = "Y" then call "DESCRIBE" (#07, currency$, ~
                                                   currdesc$, 1%, f1%(7))

        REM *************************************************************~
            *                  D I S P L A Y   I T                      *~
            *                                                           *~
            * HANDLES OPERATION OF SCREEN                               *~
            *************************************************************

        edtpg1
            gosub'101
                  if keyhit%  =  8% then       get_buy_froms
                  if keyhit%  = 16% then       exit_program
                  if keyhit%  = 28% then gosub show_text
            goto edtpg1

        edtpg2
            gosub'102
                  if keyhit%  = 16% then       edtpg1
                  if keyhit%  = 28% then gosub show_bftext
            goto edtpg2

        REM *************************************************************~
            *             S H O W   V E N D O R   T E X T               *~
            *************************************************************
        show_text

            textmsg$ = "Vendor: " & vencode$ & ", " & vendescr$
            call "TXTDSPLY" (#6, f2%(6), "002", textmsg$, textid$,       ~
                                                            texta$())
            return

        REM *************************************************************~
            *     S H O W   V E N D O R   B U Y   F R O M   T E X T     *~
            *************************************************************
        show_bftext
            textmsg$ = "Buy From: " & vencode$ & "-" & bfcode$ &         ~
                       ", (" & bfdescr$ & ")"
            call "TXTDSPLY" (#6, f2%(6), "002", textmsg$, bftextid$,     ~
                                                            texta$())
            return

        REM *************************************************************~
            *  G E T   V E N D O R   B U Y   F R O M   L O C A T I O N  *~
            *************************************************************
        get_buy_froms

            if fs2% <> 1% then L30080    /* VENDORBF File Not Open */
            readkey$ = vencode$
            call "GETCODE" (#2, readkey$, bfdescr$, 0%, 0, f1%(2))
                if f1%(2) <> 0% then L30100
                   goto edtpg1
L30080:              errormsg$ = "NO Buy From Locations Found!"
                     goto edtpg1

L30100:     init(" ") bfaddrs$(), bfcontact$, bfphone$, bfcode$,         ~
                      bfconfirm$, bffterms$, bffob$, bfship$, bftextid$

            get #2, using L30180, bfcode$, bfdescr$, bfaddrs$(),          ~
                                 bfcontact$, bfphone$, bfconfirm$,       ~
                                 bffterms$, bffob$, bfship$, bftextid$
            goto edtpg2

L30180:     FMT XX(9), CH(6), CH(30), 6*CH(30), CH(20), CH(10), CH(1),   ~
                CH(1), CH(30), CH(30), XX(19), CH(4)


        REM *************************************************************~
            *   D I S P L A Y   V E N D O R   I N F O R M A T I O N     *~
            *************************************************************

            deffn'101

                gosub set_pf1

*          STR(LINE2$,1,60) = "Vendor:" & HEX(A4) & VENCODE$ & "  "     ~
*                                        & VENDESCR$ & HEX(AC)
            str(line2$,1,60) = "Vendor: " & vencode$ & "  " & vendescr$

L40120:     accept                                                       ~
               at (01,02), "Display Vendor Information",                 ~
               at (01,67), "Date: ",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "REMIT-TO: Name           ",                  ~
               at (06,02), "          Address - 1    ",                  ~
               at (07,02), "          Address - 2    ",                  ~
               at (08,02), "          Address - 3    ",                  ~
               at (09,02), "          Address - 4    ",                  ~
               at (10,02), "          City, State, Zip",                 ~
               at (05,30), fac(hex(84)),   address$(1)          , ch(30),~
               at (06,30), fac(hex(84)),   address$(2)          , ch(30),~
               at (07,30), fac(hex(84)),   address$(3)          , ch(30),~
               at (08,30), fac(hex(84)),   address$(4)          , ch(30),~
               at (09,30), fac(hex(84)),   address$(5)          , ch(30),~
               at (10,30), fac(hex(84)),   str(address$(6),1,17), ch(17),~
               at (10,50), fac(hex(84)),   str(address$(6),19,2), ch(02),~
               at (10,54), fac(hex(84)),   str(address$(6),22,5), ch(05),~
               at (10,60), "-",                                          ~
               at (10,62), fac(hex(84)),   str(address$(6),27,4), ch(04),~
                                                                         ~
               at (11,02), "Vendor Contact",                             ~
               at (11,30), fac(hex(84)),   contact$             , ch(20),~
               at (12,02), "Phone Number",                               ~
               at (12,30), "(",                                          ~
               at (12,32), fac(hex(84)),   str(phone$, 1, 3)    , ch(03),~
               at (12,36), ")",                                          ~
               at (12,38), fac(hex(84)),   str(phone$, 4, 3)    , ch(03),~
               at (12,42), "-",                                          ~
               at (12,44), fac(hex(84)),   str(phone$, 7, 4)    , ch(04),~
               at (12,49), "Fax",                                        ~
               at (12,53), fac(hex(84)),   str(fax$,   1%, 3%)  , ch(03),~
               at (12,57), "-",                                          ~
               at (12,59), fac(hex(84)),   str(fax$,   4%, 4%)  , ch(04),~
               at (13,02), "Tax Identification Number",                  ~
               at (13,30), fac(hex(84)),   tin$                 , ch(12),~
               at (14,02), "Confirmation Req'd (Y/N)",                   ~
               at (14,30), fac(hex(84)),   confirm$             , ch(01),~
               at (15,02), "Freight Terms",                              ~
               at (15,30), fac(hex(84)),   fterms$              , ch(01),~
               at (15,49), "A-Added, N-Not Added, C-Collect",            ~
               at (16,02), "Freight On Board (FOB)",                     ~
               at (16,30), fac(hex(84)),   fob$                 , ch(30),~
               at (17,02), "Ship Via",                                   ~
               at (17,30), fac(hex(84)),   ship$                , ch(30),~
               at (18,02), "Bills Due (days)",                           ~
               at (18,30), fac(hex(84)),   billsdue$            , ch(05),~
               at (19,02), "Discount Due (days)",                        ~
               at (19,30), fac(hex(84)),   discsdue$            , ch(05),~
               at (18,49), "Discount Percent",                           ~
               at (18,67), fac(hex(84)),   discpercent$         , ch(05),~
               at (19,49), "A/P Balance",                                ~
               at (19,62), fac(hex(84)),   balance$             , ch(10),~
               at (20,02), fac(hex(8c)),   curr_msg$            , ch(13),~
               at (20,30), fac(hex(84)),   currency$            , ch(04),~
               at (20,49), fac(hex(8c)),   currdesc$            , ch(32),~
                                                                         ~
               at (21,02), fac(hex(ac)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pfd$(1)                , ch(79),~
               at (23,02), fac(hex(8c)), pfd$(2)                , ch(79),~
               at (24,02), fac(hex(8c)), pfd$(3)                , ch(79),~
                    keys(pfk$),                                          ~
                    key (keyhit%)

               errormsg$ = " "

               if keyhit% <>  1 then L40770
                  vencode$ = " "
                  return clear all
                  goto L09160

L40770:        if keyhit% <> 13 then L40810
*                CALL "MANUAL" ("VENDSPSB")
                  goto L40120

L40810:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40120


        set_pf1

          pfd$(1) = "(1) Choose Another Vendor                         "&~
                    "             (13)Instructions"
          pfd$(2) = "                          (8)See Vendor Buy From L"&~
                    "ocations     (15)Print Screen"
          pfd$(3) = "                          (28)Display Text        "&~
                    "             (16)Return      "
          pfk$ = hex(01080d0f101c)

          return

        REM *************************************************************~
            *     D I S P L A Y   V E N D O R   B U Y - F R O M ' S     *~
            *************************************************************

            deffn'102

                gosub set_pf2

L41400:     accept                                                       ~
               at (01,02), "Display VENDOR Buy From Locations",          ~
               at (01,67), "Date: ",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (05,02), "Buy From Code",                              ~
               at (05,30), fac(hex(84)),   bfcode$              , ch(06),~
               at (06,02), "BUY FROM: Name           ",                  ~
               at (07,02), "          Address - 1    ",                  ~
               at (08,02), "          Address - 2    ",                  ~
               at (09,02), "          Address - 3    ",                  ~
               at (10,02), "          Address - 4    ",                  ~
               at (11,02), "          City, State, Zip",                 ~
               at (06,30), fac(hex(84)),   bfaddrs$(1)          , ch(30),~
               at (07,30), fac(hex(84)),   bfaddrs$(2)          , ch(30),~
               at (08,30), fac(hex(84)),   bfaddrs$(3)          , ch(30),~
               at (09,30), fac(hex(84)),   bfaddrs$(4)          , ch(30),~
               at (10,30), fac(hex(84)),   bfaddrs$(5)          , ch(30),~
               at (11,30), fac(hex(84)),   str(bfaddrs$(6),1,17), ch(17),~
               at (11,50), fac(hex(84)),   str(bfaddrs$(6),19,2), ch(02),~
               at (11,54), fac(hex(84)),   str(bfaddrs$(6),22,5), ch(05),~
               at (11,60), "-",                                          ~
               at (11,62), fac(hex(84)),   str(bfaddrs$(6),27,4), ch(04),~
                                                                         ~
               at (13,02), "Vendor Contact",                             ~
               at (13,30), fac(hex(84)),   bfcontact$           , ch(20),~
               at (14,02), "Phone Number",                               ~
               at (14,30), "(",                                          ~
               at (14,32), fac(hex(84)),   str(bfphone$, 1, 3)  , ch(03),~
               at (14,36), ")",                                          ~
               at (14,38), fac(hex(84)),   str(bfphone$, 4, 3)  , ch(03),~
               at (14,42), "-",                                          ~
               at (14,44), fac(hex(84)),   str(bfphone$, 7, 4)  , ch(04),~
               at (15,02), "Buy From Description",                       ~
               at (15,30), fac(hex(84)),   bfdescr$             , ch(30),~
               at (16,02), "Confirmation Req'd (Y/N)",                   ~
               at (16,30), fac(hex(84)),   bfconfirm$           , ch(01),~
               at (17,02), "Freight Terms",                              ~
               at (17,30), fac(hex(84)),   bffterms$            , ch(01),~
               at (17,49), "A-Added, N-Not Added, C-Collect",            ~
               at (18,02), "Freight On Board (FOB)",                     ~
               at (18,30), fac(hex(84)),   bffob$               , ch(30),~
               at (19,02), "Ship Via",                                   ~
               at (19,30), fac(hex(84)),   bfship$              , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfd$(1)              , ch(79),~
               at (23,02), fac(hex(8c)),   pfd$(2)              , ch(79),~
               at (24,02), fac(hex(8c)),   pfd$(3)              , ch(79),~
                     keys(pfk$),                                         ~
                     key (keyhit%)

               if keyhit% <> 13 then L41930
*                CALL "MANUAL" ("VENDSPSB")
                  goto L41400

L41930:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41400

        set_pf2

          pfd$(1) = "                                                  "&~
                    "             (13)Instructions"
          pfd$(2) = "                                                  "&~
                    "             (15)Print Screen"
          pfd$(3) = "                          (28)Display Text        "&~
                    "             (16)Return      "
          pfk$ = hex(0d0f101c)
          return


        REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************
        exit_program

            if vencode$ = " " then vencode$ = save_vendor$
            end
