        REM *************************************************************~
            *                                                           *~
            *  V   V  EEEEE  N   N  DDDD   L      EEEEE  TTTTT  EEEEE   *~
            *  V   V  E      NN  N  D   D  L      E        T    E       *~
            *  V   V  EEEE   N N N  D   D  L      EEEE     T    EEEE    *~
            *   V V   E      N  NN  D   D  L      E        T    E       *~
            *    V    EEEEE  N   N  DDDD   LLLLL  EEEEE    T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENDLETE - DELETES VENDOR MASTER RECORDS WHEN THEY ARE NOT*~
            *            MENTIONED IN PAYABLES, CASH DISBURSEMENTS, OR  *~
            *            VENDOR BACKLOG FILES.  THIS IS A SEPARATE      *~
            *            PROGRAM FOR SECURITY PURPOSES.                 *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/01/81 ! ORIGINAL                                 ! BCW *~
            * 12/16/85 ! Vendor file format changes; added check  ! MJB *~
            *          !  to make sure user is an Mod Admin;      ! ERN *~
            *          !  added check to POAPLINE                 !     *~
            * 05/20/86 !  Replaced POAPLINE with Receiver logic   ! HES *~
            * 05/21/87 !  Standard Costing Enhancements           ! ERN *~
            * 10/12/87 !  Added purge of VENPRICE                 ! HES *~
            *************************************************************

        dim                                                              ~
            address$(5)30,               /* VENDOR ADDRESS FIELD       */~
            date$8,                      /* TODAY'S CLOCK DATE         */~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            inpmessage$79,               /* INPUT INFORMATIVE MESSAGE  */~
            lastven$9,                   /* LAST VENDOR CODE DELETED   */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen Line 2              */~
            line3$80,                    /* Screen Line 3              */~
            readkey$90,                  /* KEY TO READ FILE WITH      */~
            vencode$9,                   /* VENDOR CODE INFORMATION    */~
            venname$30                   /* VENDOR NAME                */~

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.01 10/21/87 Patch release                   "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 3 ! VENDOR   ! VENDOR MASTER RECORD FILE                *~
            * # 4 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN FILE                       *~
            * # 6 ! VENDORBF ! VENDOR BUY FROM FILE                     *~
            * # 7 ! CSHMASTR ! CASH DISBURSEMENTS CHECK HEADER FILE     *~
            * # 8 ! TXTFILE  ! SYSTEM TEXT FILE                         *~
            * # 9 ! VBKMASTR ! VENDOR BACKLOG MASTER FILE               *~
            * #10 ! RCVLINES ! Receiver Line Items                      *~
            *************************************************************

            select #3,  "VENDOR"                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1, keylen = 9,                          ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select  #4, "VENPRICE",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =    1, keylen =  34, dup,    ~
                            key  2, keypos =   35, keylen =  34          ~

            select  #5, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select  #6, "VENDORBF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 15

            select  #7, "CSHMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 17,                         ~
             alt key 1, keypos =41, keylen =  9, dup,                    ~
                 key 2, keypos =50, keylen =  6, dup,                    ~
                 key 3, keypos =10, keylen =  8, dup                     ~

            select #8,  "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  11                      ~

            select # 9, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1030,                                  ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos = 10, keylen = 16

            select #10, "RCVLINES",                                      ~
                        varc, indexed, recsize = 800,                    ~
                        keypos =  26, keylen = 52,                       ~
             alt key 1, keypos =   1, keylen = 69,                       ~
                 key 2, keypos =  42, keylen = 36,                       ~
                 key 3, keypos = 128, keylen = 24

            call "SHOSTAT" ("Opening files, one moment please")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION USED IN PROGRAM PROCESSING        *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

*        Check that User is an administrator.
            call "CMSMACHK" ("VBK", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then L10000
                call "CMSMACHK" ("PAY", lfac$(1), lfac$(2))
                if lfac$(1) = "Y" or lfac$(2) = "Y" then L10000
                     errormsg$ = "You must be a PO, A/P or Data Base" &  ~
                                 " Administrator to run this program."
                     call "ASKUSER" (0, "SECURITY", " ", errormsg$, " ")
                     goto L65000

L10000: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * GETS VENDOR CODE, TESTS, THEN RUNS ROUTINE THAT DISPLAYS  *~
            * VENDOR NAME AND ADDRESS.  THEN GOES TO ROUTINE THAT       *~
            * DOES THE ACTUAL DELETE CYCLE.                             *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, vencode$, venname$,        ~
                      address$()

            for fieldnr% = 1 to 1
                gosub'161(fieldnr%)
                      if enabled%  = 0 then L10200
L10150:         gosub'201(fieldnr%)
                      if keyhit%  = 16 then       L65000
                      if keyhit% <>  0 then       L10150
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10150
L10200:         next fieldnr%

            REM ROUTINE TO SHOW VENDOR INFORMATION
                gosub L30000              /* LOAD VENDOR INFORMATION    */
                gosub L41000              /* Display Vendor Information */
                keyhit% = 2%
                call "ASKUSER" (keyhit%, "CONFIRMATION",                 ~
                     hex(8c) & "Vendor:" & hex(84) & vencode$ & hex(8c) &~
                    "Is about to Be Deleted" & hex(84),                  ~
                     hex(8c) & "To Delete, Press PF(16)." & hex(84),     ~
                    "Hit any other PF key To abort delete")
                if keyhit%  = 16 then       L19000      /* BOOM!  */
                goto inputmode

L19000: REM *************************************************************~
            *                 D E L E T E   R O U T I N E               *~
            * DELETES THE VENDOR FROM THE VENDOR FILE.  ALSO DELETES    *~
            *  VENDORBF AND ALL TEXT ASSOCIATED WITH EITHER FILE.       *~
            *************************************************************

            call "SHOSTAT" ("Deleting Vendor:" &hex(94)&vencode$&hex(84))
            init (hex(00)) readkey$
            str(readkey$,,9) = vencode$
L19090:     call "PLOWNEXT" (#6, readkey$, 9%, f1%(6))
                if f1%(6) = 0 then L19180
                get #6 using L19120, bftextid$
L19120:             FMT POS(337), CH(04)
                call "TXTFUTIL" (#8, 0%, "DELE", bftextid$)
                call "READ101" (#6, readkey$, f1%(6))
                delete #6
                goto L19090

L19180:     call "TXTFUTIL" (#8, 0%, "DELE", vtextid$)

            call "READ101" (#3, vencode$, f1%(3))
                 if f1%(3) <> 0 then delete #3

            REM lastly, purge any entries in price catalog...
L19240:     init (hex(00)) readkey$
            str(readkey$,,9) = vencode$
            call "PLOWAL1" (#4, readkey$, 1%, 9%, f1%(4))
                if f1%(4) = 0 then L19310
                delete #4
                goto L19240  /* dups */

L19310:     lastven$ = vencode$
            goto L10000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * DEFAULT/ENABLE FOR VENDOR CODE ON FIRST PAGE.  SET        *~
            * APPROPRIATE MESSAGE TEXT.                                 *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  inpmessage$ = " "
                  on fieldnr% gosub L20100          /* VENDOR CODE    */
                     return
L20100:     REM DEFAULT/ENABLE FOR VENDOR CODE
                enabled% = 1
                inpmessage$ = "Vendor May Not Have Any Current Activity"
                return

L30000: REM *************************************************************~
            *     L O A D     V E N D O R     I N F O R M A T I O N     *~
            *                                                           *~
            * LOADS VENDOR INFORMATION FROM THE VENDOR FILE.            *~
            *************************************************************

            get #3, using L30900, address$(), vtextid$

            return

L30900:     FMT XX(69), 5*CH(30), XX(20), XX(250), CH(4)

        REM *************************************************************~
            *      I N P U T     V E N D O R     T O   D E L E T E      *~
            *                                                           *~
            * INPUTS THE VENDOR CODE TO BE DELETED.                     *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) lfac$()
                  line2$ = " "
                  if lastven$ <> " " then line2$="Last Vendor Deleted: "&~
                                                                 lastven$
                  str(line2$,62) = "VENDLETE: " & cms2v$
                  on fieldnr% gosub L40170          /* VENDOR CODE      */
                     goto L40240
                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:     accept                                                       ~
               at (01,02), "Delete Old Vendor Master Records",           ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Vendor Code",                                         ~
               at (06,20), fac(lfac$( 1)), vencode$             , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)EXIT PROGRAM",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40490
                  call "MANUAL" ("VENDLETE")
                  goto L40240

L40490:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40240

L41000: REM *************************************************************~
            *                C O N F I R M   D E L E T E                *~
            *                                                           *~
            * CONFIRMS THAT WE ARE IN FACT DELETING THIS VENDOR.        *~
            *************************************************************

            init(hex(84)) lfac$()
            line3$ = hex(ac) & str(line2$)

            display                                                      ~
               at (01,02), "Delete Old Vendor Master Records",           ~
               at (01,60), "Todays Date:",                               ~
               at (01,73),               date$                  , ch(08),~
               at (02,01),               line3$                 , ch(80),~
               at (04,02),               errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "VENDOR CODE",                                ~
               at (06,30),                 vencode$             , ch(09),~
               at (07,02), "VENDOR NAME",                                ~
               at (07,30),                 venname$             , ch(30),~
               at (08,02), "ADDRESS (Line 1)",                           ~
               at (08,30),                 address$( 1)         , ch(30),~
               at (09,02), "ADDRESS (Line 2)",                           ~
               at (09,30),                 address$( 2)         , ch(30),~
               at (10,02), "ADDRESS (Line 3)",                           ~
               at (10,30),                 address$( 3)         , ch(30),~
               at (11,02), "ADDRESS (Line 4)",                           ~
               at (11,30),                 address$( 4)         , ch(30),~
               at (12,02), "ADDRESS (Line 5)",                           ~
               at (12,30),                 address$( 5)         , ch(30)

            return

        REM *************************************************************~
            *   D A T A   T E S T   F O R     V E N D O R     C O D E   *~
            *                                                           *~
            * TEST DATA FOR VENDOR CODE.  MAKE SURE THAT THE VENDOR     *~
            * IS ON FILE IN THE VENDOR MASTER FILE AND NOT ON FILE IN   *~
            * ANY OF THE SOURCE DOCUMENT FILES.                         *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130          /* VENDOR CODE      */
            return

L50130:     REM Test data for VENDOR CODE
            call "GETCODE" (#3, vencode$, venname$, 0%, 1.30, f1%(3))
            if f1%(3) <> 0% then L50220
                errormsg$ = "Vendor Code Not On File: " & vencode$
                return

L50220:     REM Now test transaction files for Vendor.
            readkey$ = vencode$
            call "PLOWNEXT" (#5, readkey$, 9%, f1%(5))
            if f1%(5) = 0 then L50290
                errormsg$ = "Vendor Invoices Still Outstanding."
                return

L50290:     readkey$ = vencode$
            call "PLOWNEXT" (#7, readkey$, 9%, f1%(7))
            if f1%(7) = 0 then L50350
                errormsg$ = "Vendor Checks Still Outstanding."
                return

L50350:     readkey$ = vencode$
            call "PLOWNEXT" (#9, readkey$, 9%, f1%(9))
            if f1%(9) = 0 then L50410
                errormsg$ = "Purchase Orders Still Outstanding."
                return

L50410:     readkey$ = vencode$
            call "PLOWALTS" (#10, readkey$, 2%, 9%, f1%(10))
            if f1%(10) = 0% then return
                errormsg$ = "Receiver Line still exists. Receiver: " &   ~
                                                      str(key(#10,0),,16)
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
