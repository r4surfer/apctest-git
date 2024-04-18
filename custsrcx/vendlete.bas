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
            * 12/22/97 ! (EWD) Mod to Purge All Detail Data for   ! RHH *~
            *          !  the Vendor Specified. Insure all activity     *~
            *          !  has been Purged.                        !     *~
            * 12/29/97 ! (EWD) Mod new Subroutine, PURGE_CSH,     ! RHH *~
            *          !  PURGE_PAY, PURGE_VBK, PURGE_RCV. Mods   !     *~
            *          !  at (40450-40550), (40350-40420).        !     *~
            * 03/31/98 ! Mods for (Y2K) EWD Version not Caelus    ! RHH *~
            * 10/07/98 ! (EWD001) - Purge VENHSTRY for Blind Delt;! BWS *~
            *          !    Bug fix--multi-line rcvrs not deltng. !     *~
            *************************************************************

        dim                                                              ~
            address$(5%)30,              /* VENDOR ADDRESS FIELD       */~
            date$8,                      /* TODAY'S CLOCK DATE         */~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            inpmessage$79,               /* INPUT INFORMATIVE MESSAGE  */~
            lastven$9,                   /* LAST VENDOR CODE DELETED   */~
            lfac$(20%)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen Line 2              */~
            line3$80,                    /* Screen Line 3              */~
            readkey$90, receiver$16,     /* KEY TO READ FILE WITH      */~
/*EWD001*/  readkey2$16,                 /* Key for RCVMASTR           */~
            vencode$9,  count$5,         /* VENDOR CODE INFORMATI(EWD) */~
            venname$30                   /* VENDOR NAME                */~

        dim f1%(64%)                     /* RECORD-ON-FILE FLAGS       */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.01 10/21/87 Patch release                   "
                                                      /* (EWD) - Begin */
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 3 ! VENDOR   ! VENDOR MASTER RECORD FILE                *~
            * # 4 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN FILE                       *~
            * #12 ! PAYLINES ! PAYABLES LINE ITEM FILE                  *~
            * # 6 ! VENDORBF ! VENDOR BUY FROM FILE                     *~
            * # 7 ! CSHMASTR ! CASH DISBURSEMENTS CHECK HEADER FILE     *~
            * #11 ! CSHLINES ! CASH DISBURSEMENTS CHECK HEADER DETAIL   *~
            * # 8 ! TXTFILE  ! SYSTEM TEXT FILE                         *~
            * # 9 ! VBKMASTR ! VENDOR BACKLOG MASTER FILE               *~
            * #13 ! VBKLINES ! VENDOR BACKLOG DETAIL FILE               *~
            * #10 ! RCVLINES ! Receiver Line Items                      *~
            * #14 ! RCVMASTR ! Receiver HEADER FILE                     *~
 /*EWD001*/ * #15 ! VENHSTRY ! Vendor History File                      *~
            *************************************************************
                                                        /* (EWD) - End */
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

            select #12, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alt key  1, keypos =    1, keylen =  63,         ~
                            key  2, keypos =   17, keylen =  47

            select  #6, "VENDORBF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 15
                                                     /* (EWD) - Begin */
            select  #7, "CSHMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 17,                         ~
             alt key 1, keypos =41, keylen =  9, dup,                    ~
                 key 2, keypos =50, keylen =  6, dup,                    ~
                 key 3, keypos =10, keylen =  8, dup                     ~

            select #11, "CSHLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
             alt key 1, keypos =21, keylen = 16, dup

            select #8,  "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  11                      ~

            select  #9, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1030,                                  ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos = 10, keylen = 16

            select #13, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos = 1, keylen = 28

            select #10, "RCVLINES",                                      ~
                        varc, indexed, recsize = 800,                    ~
                        keypos =  26, keylen = 52,                       ~
             alt key 1, keypos =   1, keylen = 69,                       ~
                 key 2, keypos =  42, keylen = 36,                       ~
                 key 3, keypos = 128, keylen = 24

            select #14, "RCVMASTR",                                      ~
                        varc, indexed, recsize = 150,                    ~
                        keypos =   1, keylen = 16

/*EWD001*/  select #15, "VENHSTRY",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 11

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#12, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#11, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#13, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#14, 0%, 0%, 0%, " ")
/*EWD001*/  call "OPENCHCK" (#15, 0%, 0%, 0%, " ")
                                                     /* (EWD) - End */
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
                      address$(), count$                 /* (EWD)     */

            for fieldnr% = 1 to 1
                gosub'161(fieldnr%)
                      if enabled%  = 0 then L10200
L10150:         gosub'201(fieldnr%)
                      if keyhit%  = 16 then       L65000
                      if keyhit%  = 12 then       L10180  /* (EWD) - */
                      if keyhit% <>  0 then       L10150
L10180:         gosub'151(fieldnr%)
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
                                                      /* (EWD) - Begin */
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
               at (23,40), "(12)Blind Delete",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)EXIT PROGRAM",                           ~
                                                                         ~
               keys(hex(00010c0f10)),                                    ~
               key (keyhit%)

        REM                                 /* Special (EWD) Mod       */
               if keyhit% <> 12% then goto L40560
               call "SHOSTAT" ("Clearing All Data for ("&vencode$&")" )
                  cnt% = 0%
                  gosub purge_csh           /* CSHMASTR, CSHLINES      */
                  gosub purge_pay           /* PAYMASTR, PAYLINES      */
/*EWD001*/        gosub purge_vbk           /* VBKMASTR/LINES, VENHSTRY*/
                  gosub purge_rcv           /* RCVLINES, RCVMASTR      */
                  convert cnt% to count$, pic(#####)
                                            /* Clear all Activity for  */
                                            /* the Specified Vendor    */

L40560:        if keyhit% <> 13 then L40600
                  call "MANUAL" ("VENDLETE")
                  goto L40240

L40600:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40240
                                            /* (EWD) - End            */
L41000: REM *************************************************************~
            *                C O N F I R M   D E L E T E                *~
            *                                                           *~
            * CONFIRMS THAT WE ARE IN FACT DELETING THIS VENDOR.        *~
            *************************************************************

            init(hex(84)) lfac$()
            line3$ = hex(ac) & str(line2$)
                                            /* (EWD) - Begin           */
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
               at (12,30),                 address$( 5)         , ch(30),~
               at (15,02), "Vendor Txn's Purged = ",                     ~
               at (15,25),                 count$               , ch(05)

            return
                                               /* (EWD) - End         */
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
            if f1%(3) <> 0% then L50180
                errormsg$ = "Vendor Code Not On File: " & vencode$
                return
L50180:     REM Now test transaction files for Vendor.
            readkey$ = vencode$
            call "PLOWNEXT" (#5, readkey$, 9%, f1%(5))
            if f1%(5) = 0 then L50250
                errormsg$ = "Vendor Invoices Still Outstanding."
                return

L50250:     readkey$ = vencode$
            call "PLOWNEXT" (#7, readkey$, 9%, f1%(7))
            if f1%(7) = 0 then L50310
                errormsg$ = "Vendor Checks Still Outstanding."
                return

L50310:     readkey$ = vencode$
            call "PLOWNEXT" (#9, readkey$, 9%, f1%(9))
            if f1%(9) = 0 then L50370
                errormsg$ = "Purchase Orders Still Outstanding."
                return

L50370:     readkey$ = vencode$
            call "PLOWALTS" (#10, readkey$, 2%, 9%, f1%(10))
            if f1%(10) = 0% then return
                errormsg$ = "Receiver Line still exists. Receiver: " &   ~
                                                      str(key(#10,0),,16)
                return
                                                     /* (EWD) - Begin */
        purge_csh
            init(" ") readkey$
            str(readkey$,1%,9%) = vencode$
        purge_csh_nxt
            read #7,hold,key > readkey$, using L50500, readkey$,          ~
                                                 eod goto purge_csh_lines
L50500:        FMT CH(17)
            if str(readkey$,1%,9%) <> vencode$ then goto purge_csh_lines
               delete #7
               cnt% = cnt% + 1%
               goto purge_csh_nxt
        purge_csh_lines
            init(" ") readkey$
            str(readkey$,1%,9%) = vencode$
        purge_csh_ln_nxt
            read #11,hold,key > readkey$, using L50610, readkey$,         ~
                                                eod goto purge_csh_done
L50610:        FMT CH(20)
            if str(readkey$,1%,9%) <> vencode$ then goto purge_csh_done
               delete #11
               cnt% = cnt% + 1%
               goto purge_csh_ln_nxt
        purge_csh_done
        return

        purge_pay
            init(" ") readkey$
            str(readkey$,1%,9%) = vencode$
        purge_pay_nxt
            read #5,hold,key > readkey$, using L50750, readkey$,          ~
                                                 eod goto purge_pay_lines
L50750:        FMT CH(25)
            if str(readkey$,1%,9%) <> vencode$ then goto purge_pay_lines
               delete #5
               cnt% = cnt% + 1%
               goto purge_pay_nxt
        purge_pay_lines
            init(" ") readkey$
            str(readkey$,1%,9%) = vencode$
        purge_pay_ln_nxt
            read #12,hold,key > readkey$, using L50860, readkey$,         ~
                                                eod goto purge_pay_done
L50860:        FMT POS(36), CH(28)
            if str(readkey$,1%,9%) <> vencode$ then goto purge_pay_done
               delete #12
               cnt% = cnt% + 1%
               goto purge_pay_ln_nxt
        purge_pay_done
        return

        purge_vbk
            init(" ") readkey$
            str(readkey$,1%,9%) = vencode$
        purge_vbk_nxt
            read #9,hold,key > readkey$, using L51000, readkey$,          ~
                                                 eod goto purge_vbk_lines
L51000:        FMT CH(25)
            if str(readkey$,1%,9%) <> vencode$ then goto purge_vbk_lines
               delete #9
               cnt% = cnt% + 1%
               goto purge_vbk_nxt
        purge_vbk_lines
            init(" ") readkey$
            str(readkey$,1%,9%) = vencode$
        purge_vbk_ln_nxt
            read #13,hold,key > readkey$, using L51110, readkey$,         ~
                                                eod goto purge_ven_hstry
L51110:        FMT CH(28)                           /*EWD001^*/
            if str(readkey$,1%,9%) <> vencode$ then goto purge_ven_hstry
               delete #13                           /*EWD001^*/
               cnt% = cnt% + 1%
               goto purge_vbk_ln_nxt
        purge_ven_hstry                             /*EWD001 - Begin*/
            readkey$ = all(hex(00))
            str(readkey$,1%,9%) = vencode$
        purge_ven_hs_nxt
            read #15,hold,key > readkey$, using L51120, readkey$,         ~
                                                eod goto purge_vbk_done 
L51120:        FMT CH(11)
            if str(readkey$,1%,9%) <> vencode$ then goto purge_vbk_done
               delete #15
               cnt% = cnt% + 1%
               goto purge_ven_hs_nxt                /*EWD001 -  End */
        purge_vbk_done
        return

        purge_rcv
            init(" ") readkey$, receiver$
            str(readkey$,1%,9%) = vencode$
        purge_rcv_nxt
            read #10,hold,key 2% > readkey$, using L51250, receiver$,     ~
                                                 readkey$, eod goto L51310
L51250:        FMT POS(26), CH(16), CH(36)
            if str(readkey$,1%,9%) <> vencode$ then goto L51310
               delete #10
               cnt% = cnt% + 1%
               gosub purge_receiver
               goto purge_rcv_nxt
L51310: return

        purge_receiver
/*EWD001*/  init(" ") readkey2$
/*EWD001*/  str(readkey2$,1%,16%) = receiver$
/*EWD001*/  read #14,hold,key = readkey2$, eod goto purge_rcv_done
               delete #14
               cnt% = cnt% + 1%
        purge_rcv_done
        return
                                                     /* (EWD) - End    */
L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end


