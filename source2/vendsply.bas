        REM *************************************************************~
            *                                                           *~
            *  V   V  EEEEE  N   N  DDDD    SSS   PPPP   L      Y   Y   *~
            *  V   V  E      NN  N  D   D  S      P   P  L      Y   Y   *~
            *  V   V  EEEE   N N N  D   D   SSS   PPPP   L       YYY    *~
            *   V V   E      N  NN  D   D      S  P      L        Y     *~
            *    V    EEEEE  N   N  DDDD    SSS   P      LLLLL    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENDSPLY - DISPLAY 12 MONTH VENDOR PURCHASE HISTORY       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/16/81 ! ORIGINAL                                 ! JAM *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! Changed file open subroutines            ! HES *~
            * 10/03/85 ! Changed Select and format for VENDOR.    ! ERN *~
            * 12/13/85 ! Vendor file format changes               ! MJB *~
            * 09/23/86 ! Changed to get from VENHSTRY (Rewrite)   ! ERN *~
            * 05/05/92 ! Added Totals                             ! KAB *~
            * 02/05/93 ! Added vendor default currency code.      ! JDH *~
            * 08/28/96 ! Millie date conversion                   ! DER *~
            *************************************************************


        dim                                                              ~
            billto$(6)30,                /* Remit To Name and Address  */~
            ccyymmdd$8,                  /* ccyymmdd                   */~
            contact$20,                  /* Vendor Contact             */~
            curr$4,                      /* Vendor default Currency    */~
            curr_descr$30,               /* Currency description       */~
            date$8,                      /* date                       */~
            description$30,              /* Vendor Name                */~
            errormsg$79,                 /* Error Message Text         */~
            hdr$(6)10,                   /* Headers                    */~
            history(12),                 /* 12-Month History           */~
            inpmessage$79,               /* Messages for Input Mode    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$(14)79,                 /* Display Lines              */~
            line2$79,                    /* Second Screen Line         */~
            mc_display$22,               /* MC Currency Display        */~
            mc_field$12,                 /* MC Field Name              */~
            mc_on$1,                     /* Is multi-currency active?  */~
            months$(13)3,                /* Month Descriptors          */~
            pf7$18,                      /* PF key prompt              */~
            pfk$64,                      /* PF Keys Available          */~
            phone$12,                    /* Phone Number               */~
            readkey$30,                  /* Readkey                    */~
            vencode$9,                   /* Vendor Code                */~
            year$(6)2,                   /* Years to Display           */~
            yr_str$4                     /* Year string                */

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
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #02 ! SYSFILE2 ! Caelus Management System General Informa *~
            * # 3 ! VENHSTRY ! Vendor History File                      *~
            * # 4 ! VENDOR   ! VENDOR MASTER FILE                       *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            *************************************************************

            select #02,  "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select # 3, "VENHSTRY",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 11

            select # 4, "VENDOR",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 600,                                  ~
                         keypos = 1, keylen = 9,                         ~
                         alt key  1, keypos = 10, keylen = 30, dup

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

        call "SHOSTAT" ("Opening File, One Moment Please")
            call "OPENFILE" (#02, "SHARE", f2%(2%), rslt$(2%), axd$(2%))
            call "OPENFILE" (# 3, "SHARE", f2%(3%), rslt$(3%), axd$(3%))
            call "OPENFILE" (# 4, "SHARE", f2%(4%), rslt$(4%), axd$(4%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$, nix%, ccyymmdd$)

            str(line2$,62%) = "VENDSPLY: " & str(cms2v$,,8%)

*        Format Dates for History
            str(months$()) = "JanFebMarAprMayJunJulAugSepOctNovDecTot"

            convert str(ccyymmdd$, 1%, 4%) to year%      /* Current Year     */
            for y% = 1% to 6%
                year$(y%) = bin((year% - y% + 1%), 2%)
                convert (year% - y% + 1%) to yr_str$, pic(0000)
                hdr$(y%) = "   " & yr_str$
            next y%

*        Check for Multi-Currency
            mc_on$ = "N" : mc_field$ = " " : pf7$ = " "
            readkey$ = "SWITCHS.CUR         "
            call "READ100" (#02, readkey$, f1%(2%))
            if f1%(2%) = 0% then L10000
                get #02 using L09250, mc_on$
L09250:             FMT POS(21), CH(1)
                if mc_on$ <> "Y" then L10000
                    call "OPENFILE" (#40, "SHARE", f2%(40%), rslt$(40%), ~
                                                               axd$(40%))
                    mc_field$ = "Default Curr"
                    pf7$ = "(7)Currency Toggle"

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, vencode$, curr$,           ~
                      curr_descr$, mc_display$

            for fieldnr% = 1 to  1
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *      F O R M A T   A N D   D I S P L A Y   E N T R Y      *~
            * --------------------------------------------------------- *~
            * LOADS FROM THE DISK, FORMATS, AND DISPLAYS THE DESIRED    *~
            * ENTRY.                                                    *~
            *************************************************************


*        Load Vendor Record and Format Basic Information.
                get # 4, using L11120, vencode$, description$,            ~
                         billto$(), contact$, phone$, curr$

L11120:         FMT CH(9),               /* Vendor Code                */~
                    CH(30),              /* Vendor Description         */~
                    6*CH(30),            /* Remit-To Name & Address    */~
                    CH(20),              /* Contact                    */~
                    CH(10),              /* PHONE NUMBER               */~
                    POS(528), CH(4)      /* Currency Code              */

            phone$ = str(phone$,,3) & "-" & str(phone$,4,3) & "-" &      ~
                     str(phone$,7)

            if mc_on$ = "Y" then call "DESCRIBE" (#40, curr$,            ~
                                               curr_descr$, 0%, f1%(40%))
            mc_display$ = curr$ : curr% = 1% /* Start as currency code */

*        Get and Format Purchases History.
            init (" ") line$()
                for m% = 1% to 12%  /* Total Abbreviation looks tacky */
                     line$(m%) = months$(m%)
                next m%

            pos% = -8%
            for y% = 1% to 6%
                pos% = pos% + 13%
                total = 0
                readkey$ = str(vencode$) & year$(y%)
                call "READ100" (#3, readkey$, f1%(3))
                if f1%(3) = 0% then L11392
                     get #3 using L11340, history()
L11340:                   FMT XX(11), 12*PD(14,4)
                     for m% = 1% to 12%
                          if history(m%) = 0 then L11390
                               call "CONVERT" (history(m%), 2.2,         ~
                                                 str(line$(m%),pos%,10%))
                          total = total + history(m%)
L11390:              next m%
L11392:     str(line$(12%),pos%- 1%,1%) = hex(ac)
            if y% <> 6% then str(line$(12%),pos%+10%,1%) = hex(8c)
            if total = 0 then L11400
               /* Need to stagger if more than 12 positions or */
               /* total description is present.                */
               tl% = 13%            /* TL% = 13% + MOD(Y%, 2%) */
               call "CONVERT" (total, 2.2, str(line$(tl%),pos%-2%,12%))
L11400:     next y%


*        Display it and handle PF Keys
L11440:         gosub L41000
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       L10000
                      if keyhit% <>  0 then       L11440
                goto L10000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100          /* VENDOR CODE    */
                     return

L20100:     REM DEFAULT/ENABLE FOR VENDOR CODE
                enabled% = 1
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto L10000

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                  pfk$ = hex(00010d0f10)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40140          /* VENDOR CODE    */
                     goto L40210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Display Vendor Purchases History",                    ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02),                                               ~
                  "Vendor Code",                                         ~
               at (06,30), fac(lfac$( 1)), vencode$             , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                   keys(pfk$), key (keyhit%)

               if keyhit% <> 13% then L40440
                  call "MANUAL" ("VENDSPLY")
                  goto L40210

L40440:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L40210

L41000: REM *************************************************************~
            *         H I S T O R Y   S C R E E N                       *~
            * --------------------------------------------------------- *~
            * Displays History for Current and last 5 years.            *~
            *************************************************************

            if mc_on$ = "Y" then pfk$ = hex(0001070d0f10)                ~
                            else pfk$ = hex(00010d0f10)

L41060:     accept                                                       ~
               at (01,02), "Display Vendor Purchases History",           ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (03,02), "Vendor Code",                                ~
               at (03,14), fac(hex(84)), vencode$               , ch(09),~
               at (04,02), "Description",                                ~
               at (04,14), fac(hex(84)), description$           , ch(30),~
               at (05,02), "Location        ",                           ~
               at (05,14), fac(hex(84)), billto$(6)             , ch(30),~
               at (03,46), "Contact",                                    ~
               at (03,59), fac(hex(84)), contact$               , ch(20),~
               at (04,46), "Phone Number",                               ~
               at (04,59), fac(hex(84)), phone$                 , ch(12),~
               at (05,46), fac(hex(8c)), mc_field$              , ch(12),~
               at (05,59), fac(hex(84)), mc_display$            , ch(22),~
                                                                         ~
               at (07,06), fac(hex(ac)), hdr$(1)                , ch(10),~
               at (07,19), fac(hex(ac)), hdr$(2)                , ch(10),~
               at (07,32), fac(hex(ac)), hdr$(3)                , ch(10),~
               at (07,45), fac(hex(ac)), hdr$(4)                , ch(10),~
               at (07,58), fac(hex(ac)), hdr$(5)                , ch(10),~
               at (07,71), fac(hex(ac)), hdr$(6)                , ch(10),~
                                                                         ~
               at (08,02), fac(hex(8c)), line$( 1)              , ch(79),~
               at (09,02), fac(hex(8c)), line$( 2)              , ch(79),~
               at (10,02), fac(hex(8c)), line$( 3)              , ch(79),~
               at (11,02), fac(hex(8c)), line$( 4)              , ch(79),~
               at (12,02), fac(hex(8c)), line$( 5)              , ch(79),~
               at (13,02), fac(hex(8c)), line$( 6)              , ch(79),~
               at (14,02), fac(hex(8c)), line$( 7)              , ch(79),~
               at (15,02), fac(hex(8c)), line$( 8)              , ch(79),~
               at (16,02), fac(hex(8c)), line$( 9)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (21,02), fac(hex(ac)), line$(14)              , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (22,30), fac(hex(8c)), pf7$                   , ch(18),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Next Vendor ",                           ~
                   keys(pfk$), key (keyhit%)

               if keyhit% <>  7% then L41500
                  if curr% = 1% then L41492
                     curr% = 1% : mc_display$ = curr$
                     goto L41060
L41492:           curr% = 0% : mc_display$ = curr_descr$
                  goto L41060

L41500:        if keyhit% <> 13% then L41540
                  call "MANUAL" ("VENDSPLY")
                  goto L41060

L41540:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L41060

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* VENDOR CODE    */
                     return
L50100:     REM TEST DATA FOR VENDOR CODE
                call "GETCODE" (#4,vencode$,description$,0%, 1.3, f1%(4))
                     if f1%(4) <> 0 then return
                errormsg$ = "Vendor Code Not On File: " & vencode$
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
