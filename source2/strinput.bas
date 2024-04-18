        REM *************************************************************~
            *                                                           *~
            *   SSS   TTTTT  RRRR   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  S        T    R   R    I    NN  N  P   P  U   U    T     *~
            *   SSS     T    RRRR     I    N N N  PPPP   U   U    T     *~
            *      S    T    R   R    I    N  NN  P      U   U    T     *~
            *   SSS     T    R   R  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STRINPUT - Allows the Administrator to input the names    *~
            *            and addresses of all the distributed stores.   *~
            *----------------------------------------------------------Q*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/29/80 ! ORIGINAL                                 ! TEM *~
            * 11/18/80 ! EXPANSION OF RECORD                      ! TEM *~
            * 01/15/81 ! SEPERATE HISTORY FILE AND LISTING FUNC.  ! TEM *~
            * 07/01/81 ! REPLACE CORE RECEIVABLES ACCOUNT W/FUTURE! TOM *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 11/07/85 ! Added Customer and Vendor Defaults       ! ERN *~
            * 06/18/86 ! Removed Store History.  Added some nexts ! ERN *~
            *          !   and changed A/R accounts list. Next SO !     *~
            *          !   changed to 8 characters.               !     *~
            * 02/11/87 ! Added prevention of Sales Order number   ! ERN *~
            *          !   beginning with a meaningful PIP tag    !     *~
            * 03/30/88 ! Added a test to check if inventory exists! TLJ *~
            *          !   within the store before deleting.      !     *~
            * 04/20/88 ! Added Variable Fields                    ! TLJ *~
            * 12/12/89 ! Added revaluation account.               ! JDH *~
            * 04/11/91 ! PRR 11497 Changed Literal NEXT PO NUMBER ! SID *~
            *          !           to LAST PO NUMBER.             !     *~
            *          ! Added 'ALLFREE'                          !     *~
            * 02/24/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            *************************************************************

        dim                                                              ~
            adm$1,                       /* DATA BASE ADMINISTOR?      */~
            cursor%(2),                  /* CURSOR POSITION IN EDITMODE*/~
            date$8,                      /* SYSTEM DATE FOR SCREEN     */~
            edtmessage$79,               /* "TO MOD DISP'S VALUES" TEXT*/~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            filler$84,                   /* Filler                     */~
            firstcode$3,                 /* FIRST STORE IN PRINT RANGE */~
            fac$(20)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            header$(2)79,                /* G/L Acct Screen Headers    */~
            hdrdate$50,                  /* FORMATTED DATED FOR PRINT  */~
            i$(24)80,                    /* JUNK SCREEN IMAGE          */~
            information$(5)30,           /* STORE NAMES AND ADDRESSES  */~
            in_inv(6),                   /* Values from HNYQUAN        */~
            inpmessage$79,               /* INPUT INSTRUCTION TEXT     */~
            invnr$8,                     /* Test variable for Inv#     */~
            lastcode$3,                  /* LAST CODE IN PRINT RANGE   */~
            linenumber%(3),              /* CONTROL VARIABLE IN PRINT  */~
            nbrstr$8,                    /* Total Stores Printed       */~
            nextadj$8,                   /* Next Invoice Adjustment #  */~
            nextcrmemo$8,                /* Next Credit Memo Number    */~
            nextinvnr$8,                 /* NEXT INVOICE NUMBER        */~
            nextponr$16,                 /* NEXT PO NUMBER             */~
            nextrcvnr$7,                 /* NEXT RECEIVER NUMBER       */~
            nextsonr$8,                  /* NEXT SALES ORDER NUMBER    */~
            pf$(3)79, pfkey$32,          /* PF Key Definitions         */~
            plowkey$44,                  /* Plowkey                    */~
            print$(7)100,                /* PRINT VARIABLES FOR PRINT  */~
            readkey$50,                  /* G.P. Read and Delete Key   */~
            scrn2$79,                    /* SECOND LINE OF SCREEN      */~
            storeid$3,                   /* STORE ID NUMBER            */~
            t1$3, t2$8,                  /* Work area for Inv # Test   */~
            taxcode$10,                  /* DEFAULT TAX CODE           */~
            taxcodedescr$32,             /* DEFAULT TAX CODE DESCR     */~
            thiscode$3,                  /* PRESENT STORE PRINTING     */~
            vf$200                       /* Variable Fields            */

        dim glaccts$(25)12,   /* CUSTOMER    1)  Accounts Receivable   */~
            gldescrs$(25)30,             /*  2)  Cash-In-Bank          */~
            glaccttype$1                 /*  3)  Freight               */~
                                         /*  4)  Sales Discounts       */~
                                         /*  5)  Sales Tax             */~
                                         /*  6)  Shipped, Not Billed   */~
                                         /*  7)  Sales                 */~
                                                                         ~
                              /* VENDOR      8)  Purchases Account     */~
                                         /*  9)  Interim Liabilities   */~
                                         /* 10)  Accounts Payable      */~
                                         /* 11)  Cash in Bank          */~
                                         /* 12)  Discounts Taken       */~
                                         /* 13)  Freight Account       */~
                                         /* 14)  Receiving Holding     */~
                                         /* 15)  QC Holding Acct.      */~
                                         /* 16)  Prc-Cst Variance      */~
                                                                         ~
                              /* SYSTEM     17)  Std Cost Revaluation  */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #3  ! STORNAME ! Store Master File                        *~
            * #4  ! GLMAIN   ! General Ledger Main File                 *~
            * #5  ! HNYQUAN  ! Inventory Quantities File                *~
            * #6  ! STXCODES ! Sales Tax Code File                      *~
            * #7  ! SYSFILE2 ! System catch all file                    *~
            *************************************************************

            select #3,  "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select #4,  "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #5,  "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   650,                                 ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #6, "STXCODES", varc, indexed,                        ~
                       recsize = 100, keypos = 1, keylen = 10

            select #7, "SYSFILE2", varc, indexed,                        ~
                       recsize = 500, keypos = 1, keylen = 20

        call "SHOSTAT" ("Opening files, one moment please")
            call "OPENCHCK" (#3, 0%, f2%(3), 100%, " ")
            call "OPENCHCK" (#4, 0%, f2%(4),   0%, " ")
            call "OPENCHCK" (#5, 0%, f2%(5),   0%, " ")
            call "OPENCHCK" (#6, 0%, f2%(1),   0%, " ")
            call "OPENCHCK" (#7, 0%, f2%(1),   0%, " ")


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initailizes program variables.                            *~
            *************************************************************

            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to desired line and Press (RETURN)."

            date$ = date  :  call "DATEFMT" (date$)


            str(scrn2$,62) = "STRINPUT: " & str(cms2v$,,8)


            call "CMSDBADM" (adm$, " ")

        REM ADM$ = " " /* TURN OFF ABILITY TO DELETE */

            header$(1) = "Store Level G/L Account Defaults for Accounts/R~
        ~eceivable"
            header$(2) = "Store Level G/L Account Defaults for Accounts/P~
        ~ayable"

        REM *************************************************************~
            *                 M A I N   P R O G R A M                   *~
            * --------------------------------------------------------- *~
            * ENTER STORE INFORMATION. IF ON FILE, DEPART TO EDIT MODE. *~
            *************************************************************

        inputmode
            init(" ") storeid$, information$(), nextinvnr$, nextponr$,   ~
                      nextsonr$, nextrcvnr$, nextcrmemo$, nextadj$,      ~
                      glaccts$(), gldescrs$(), errormsg$, vf$,           ~
                      taxcode$, taxcodedescr$, str(scrn2$,,60)
            delete%, proceed% = 0%
            call "ALLFREE"

            for fieldnr% = 1 to 14
L10150:         gosub'051(fieldnr%)
L10160:         if proceed%=0% or errormsg$<>" " then gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  3 and fieldnr% = 1 then printmode
                      if keyhit%  <> 4 then L10193
                                            fieldnr% = max(1%, fieldnr%-1)
                                            goto L10150
L10193:               if keyhit%  =  6 then proceed% = 1%
                      if keyhit%  = 16 then L65000
                      if keyhit% <> 0% and proceed% = 0% then L10160
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10160
                next fieldnr%

            for fieldnr% = 1 to 7
                inpmessage$ = " "
L10280:         if proceed%=0% or errormsg$<>" " then gosub'102(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4 then fieldnr% = max(1%, fieldnr%-1)
                      if keyhit%  =  6 then proceed% = 1%
                      if keyhit% <> 0% and proceed% = 0% then L10280
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10280
                next fieldnr%

            for fieldnr% = 1 to 9
                inpmessage$ = " "
L10380:         if proceed%=0% or errormsg$<>" " then gosub'103(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4 then fieldnr% = max(1%, fieldnr%-1)
                      if keyhit%  =  6 then proceed% = 1%
                      if keyhit% <> 0% and proceed% = 0% then L10380
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10380
                next fieldnr%

*        Do Input for Variable Fields
            if proceed% = 1% then editmode
            call "VFINPSUB" ("STORNAME", "I", "Manage Store Information",~
                            "Store: " & storeid$ & " " & information$(1),~
                            "NN", vf$, keyhit%)
            if keyhit% = 1% then inputmode


        REM *************************************************************~
            *                     E D I T   M O D E                     *~
            * --------------------------------------------------------- *~
            * EDITS USER INFORMATION INPUT.             NOTE THAT WE    *~
            * HAVE THE OPTION OF GOING TO INPUT MODE AND STARTING OVER  *~
            * IF WE MESS THIS ONE UP SO BADLY THAT IT'S UNRECOVERABLE,  *~
            * AND WE ALSO HAVE THE OPTION OF EXITING W/O SAVING STUFF.  *~
            *************************************************************

        editmode
L11100:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then gosub variable_fields
                  if keyhit%  =  4 then       edtpg2
                  if keyhit%  =  5 then       edtpg3
                  if keyhit%  = 12 then       del_store
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11100
L11180:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 2 or fieldnr% > 14 then L11100
            gosub'051(fieldnr%)
L11210:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L11210
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11210
                  if fieldnr% = cursor%(1) - 5% then L11100 else L11180

        edtpg2
            inpmessage$ = " "
L11300:     gosub'112(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then gosub variable_fields
                  if keyhit%  =  4 then       editmode
                  if keyhit%  =  5 then       edtpg3
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11300
L11360:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  7 then L11300

L11390:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11390
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11390
                  if fieldnr% = cursor%(1) - 5% then L11300 else L11360

        edtpg3
            inpmessage$ = " "
L11480:     gosub'113(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then gosub variable_fields
                  if keyhit%  =  4 then       editmode
                  if keyhit%  =  5 then       edtpg2
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11480
L11540:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  9 then L11480

L11570:     gosub'113(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11570
            gosub'153(fieldnr%)
                  if errormsg$ <> " " then L11570
                  if fieldnr% = cursor%(1) - 5% then L11480 else L11540

        variable_fields:
            call "VFINPSUB" ("STORNAME", "E", "Manage Store Information",~
                            "Store: " & storeid$ & " " & information$(1),~
                            "YN", vf$, u3%)

            if u3% = 1% then return clear all
            if u3% = 1% then inputmode
            return


L12000: REM *************************************************************~
            *       P R I N T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * GETS RANGE OF CODES TO PRINT AND PRINTS THEM.             *~
            *************************************************************

        printmode
            init(" ") errormsg$, firstcode$, lastcode$, thiscode$,       ~
                      inpmessage$
            firstcode$ = "ALL"

L12110:     gosub L49000
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then inputmode
            gosub L59000
                  if errormsg$ <> " " then L12110

            REM PLOW ROUTINE FOR PRINTING LISTING.
                page%   = 0
                stores% = 857%       /* # of stores printed on page   */
                call "SETPRNT" ("SYS002", " ", 0%, 0%)
                call "SHOSTAT" ("Printing Store Listing...")
                select printer(134)
                thiscode$ = firstcode$
                storesprinted% = 0
                call "DATE" addr("HD", hdrdate$)

L12260:         call "PLOWNEXT" (#3, thiscode$, 0%, f1%(1))
                if f1%(1) = 0 then L12340
                     if thiscode$ > lastcode$ then L12340
                     gosub L32000              /* GET STORE INFO        */
                     gosub L60000              /* AND PRINT IT.         */
                     storesprinted% = storesprinted% + 1
                     goto L12260

L12340:         REM RETURN FROM ROUTINE
                    if storesprinted% = 0 then  gosub page_heading
                    nbrstr = storesprinted%
                    call "CONVERT" (nbrstr, -0.01, nbrstr$)
                    print using L61250, nbrstr$
                    print
                    print using L61270
                    close printer
                    goto L12000


        REM *************************************************************~
            * WANT TO DELETE, LET'S MAKE VERY SURE                      *~
            *************************************************************
        del_store

            askkey% = 2%

            call "ASKUSER" (askkey%, "* * * WARNING * * *",              ~
            "Deletion of a Store Master Record is about to occur.",      ~
            "This could have an adverse impact on other records in this D~
        ~ata Base.",                                                      ~
            "Press PF28 to continue and delete. Any other PF Key will ret~
        ~urn to EDIT.")

            if askkey% <> 28% then editmode
               gosub str_in_use
               if inv_exists% = 0% then L13220
                  askkey% = 2%
                  call "ASKUSER" (askkey%, "* * * ERROR * * *",          ~
                  "This Store contains inventory.",                      ~
                  "It can NOT be deleted.",                              ~
                  "PRESS any PF Key to return to EDIT." )
                  goto L13280
L13220:        if store_exists% = 0% then L13290
                  askkey% = 2%
                  call "ASKUSER" (askkey%, "* * * ERROR * * *",          ~
                  "This Store exists in the HNYQUAN file with zero "    &~
                                                           "inventory.", ~
                  "HNYPURGE must be run before deletion will be "       &~
                                                           "allowed.",   ~
                  "PRESS any PF Key to return to EDIT." )
L13280:        goto editmode
L13290:        delete% = 1%
               goto datasave

        str_in_use:
            init (hex(00)) plowkey$
            store_exists% = 0%
            inv_exists% = 0%
L13360:     call "PLOWNEXT" (#5, plowkey$, 0%, f1%(5))
            if f1%(5) = 0% then L13460
               if storeid$ <> str(plowkey$,26,3) then L13360
                  store_exists% = 1%
                  get #5 using L13420, in_inv(1), in_inv(2), in_inv(3),   ~
                                      in_inv(4), in_inv(5), in_inv(6)
L13420:              FMT     POS(69), 6*PD(14,4)
                  for i=1% to 6%
                      if in_inv(i) <> 0% then inv_exists% = 1%
                  next i
                  if inv_exists% = 0% then L13360
L13460:     return

        REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            * --------------------------------------------------------- *~
            * WRITE DATA TO FILE, DELETING OLD IN THE PROCESS.          *~
            * ALSO, GO TO INPUT ANOTHER STORE WHEN FINISHED WRITING.    *~
            *************************************************************

        datasave
            call "DELETE" (#3, storeid$, 3%)
            readkey$ = "DEFAULTS.STORE." & str(storeid$)
            call "DELETE" (#7, readkey$, 20%)
            gosub L31000
            goto inputmode

        REM *************************************************************~
            *        D E F A U L T   /   E N A B L E S                  *~
            * --------------------------------------------------------- *~
            * Set input messages for fields                             *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            on fieldnr%   gosub          L20250,    /* Store Code       */~
                                         L20300,    /* Store Descr      */~
                                         L20350,    /* Store Name       */~
                                         L20400,    /* Addr 1           */~
                                         L20450,    /* Addr 2           */~
                                         L20500,    /* Addr 2           */~
                                         L20550,    /* Next SO          */~
                                         L20600,    /* Next Invoice     */~
                                         L20650,    /* Next CR Memo     */~
                                         L20700,    /* Next Inv Adj     */~
                                         L20750,    /* Next PO          */~
                                         L20800,    /* Next Receiver    */~
                                         L20850,    /* Sales Tax Code   */~
                                         L20890     /* Revaluation Acct */
            return

L20250
*        STORE CODE
            inpmessage$ = "Leave Store ID blank to see stores on file."
            return

L20300
*        STORE DESCRIPTION
            inpmessage$ = "Enter descriptive name for this store."
            return

L20350
*        STORE NAME
            inpmessage$ = "Enter mailing name for this store."
            return

L20400
*        STORE ADDRESS- LINE 1
            inpmessage$ = "Enter mailing address- first line."
            return

L20450
*        STORE ADDRESS- LINE 2
            inpmessage$ = "Enter mailing address- second line."
            return

L20500
*        STORE ADDRESS- LINE 3
            inpmessage$ = "Enter mailing address- third line."
            return

L20550
*        NEXT SO NUMBER
            inpmessage$ = "Enter next Sales Order Number."
            return

L20600
*        NEXT INVOICE NUMBER
            inpmessage$ = "Enter next Invoice Number."
            return

L20650
*        NEXT CREDIT MEMO NUMBER
            inpmessage$ = "Leave blank to use Next Invoice Number."
            return

L20700
*        NEXT INVOICE ADJUSTMENT NUMBER
            inpmessage$ = "Leave blank to use Next Invoice Number."
            return

L20750
*        NEXT PO NUMBER
            inpmessage$ = "Enter last Purchase Order Number."
            return

L20800
*        NEXT RECEIVER NUMBER
            inpmessage$ = "Enter next Purchasing Receiver Number."
            return

L20850
*        SALES TAX CODE
            inpmessage$ = "Enter Sales Tax Code for this store."
            return

L20890
*        STANDARD COST REVALUATION ACCOUNT
            inpmessage$ = "Enter Standard Cost Revaluation Account."
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
            *       L O A D   U S E R   I N F O   F R O M   F I L E     *~
            * --------------------------------------------------------- *~
            * Loads store from master file.                             *~
            *************************************************************

            storeonfile% = 0%
            call "READ100" (#3, storeid$, f1%(3))
            if f1%(3) = 1% then L30190
*            Set up defaults for new store
                nextsonr$  = "00000001"
                nextinvnr$ = "00000001"
                     str(nextinvnr$,,len(storeid$)) = storeid$
                     nextcrmemo$, nextadj$ = " "
                nextponr$  = "A000001"
                nextrcvnr$ = "0000001"
                return

L30190:     storeonfile% = 1%
            get #3, using L30490, information$(1),  /* Descriptive Name */~
                                 information$(2),  /* Receivables Name */~
                                 information$(3),  /* Rec. Address 1   */~
                                 information$(4),  /* Rec. Address 2   */~
                                 information$(5),  /* Rec. Address 3   */~
                                 nextinvnr$,       /* Next Invoice     */~
                                 nextponr$,        /* Next PO Number   */~
                                 nextsonr$,        /* Next Sales Order */~
                                 nextrcvnr$,       /* Next Receiver    */~
                                 nextcrmemo$,      /* Next CR Memo     */~
                                 nextadj$,         /* Next Adjustment  */~
                                 filler$           /* Filler           */

            call "READ100" (#7, "DEFAULTS.STORE."& str(storeid$), f1%(7))
            if f1%(7) = 0% then L30460
                get #7 using L30360, glaccts$(), taxcode$, vf$
L30360:              FMT XX(20), XX(6), 25*CH(9), CH(10), CH(200)

            for a% = 1 to 17
                call "DESCRIBE" (#4, glaccts$(a%), gldescrs$(a%),        ~
                                                             0%, f1%(4))
                call "GLFMT" (glaccts$(a%))
            next a%

            call "DESCRIBE" (#6, taxcode$, taxcodedescr$, 1%, f1%(6))

L30460:     return


L30490:     FMT XX(3),                   /* Skip Store ID              */~
                5*CH(30),                /* Store Information          */~
                CH(8),                   /* Next Invoice Number        */~
                CH(16),                  /* Next PO Number             */~
                XX(8),                   /* Filler                     */~
                CH(8),                   /* Next Sales Order Number    */~
                CH(7),                   /* Next Receiver Number       */~
                CH(8),                   /* Next Credit Memo Number    */~
                CH(8),                   /* Next Invoice Adj Number    */~
                CH(84)                   /* FILLER                     */


        set_scrn2
            str(scrn2$,,60) = "Store: " & storeid$ &                     ~
                              "   (" & information$(1) & ")"
            return


L31000: REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            * --------------------------------------------------------- *~
            * Writes Store Data to file.                                *~
            *************************************************************

            if delete% <> 0% then return

            write #3, using L31350,                                       ~
                               storeid$,           /* Store ID         */~
                               information$(1),    /* Descriptive Name */~
                               information$(2),    /* Receivables Name */~
                               information$(3),    /* Rec. Address 1   */~
                               information$(4),    /* Rec. Address 2   */~
                               information$(5),    /* Rec. Address 3   */~
                               nextinvnr$,         /* Next Invoice     */~
                               nextponr$,          /* Next PO Number   */~
                               " ",                /* Filler           */~
                               nextsonr$,          /* Next Sales Order */~
                               nextrcvnr$,         /* Next Receiver    */~
                               nextcrmemo$,        /* Next Credit Memo */~
                               nextadj$,           /* Next Invoice Adj */~
                               filler$             /* FILLER           */

            for a% = 1% to 17%
                call "GLUNFMT" (glaccts$(a%))
            next a%

            write #7, using L31450, "DEFAULTS.STORE.",                    ~
                               storeid$,           /* STORE ID         */~
                               " ",                /* Filler           */~
                               glaccts$(),         /* Default Accounts */~
                               taxcode$,           /* Default Tax Code */~
                               vf$                 /* Variable Fields  */

            return

L31350:     FMT CH(3),                   /* Store ID                   */~
                5*CH(30),                /* Store Information          */~
                CH(8),                   /* Next Invoice Number        */~
                CH(16),                  /* Next PO Number             */~
                CH(8),                   /* Filler                     */~
                CH(8),                   /* Next Sales Order Number    */~
                CH(7),                   /* Next Receiver Number       */~
                CH(8),                   /* Next Credit Memo Number    */~
                CH(8),                   /* Next Invoice Adjustment    */~
                CH(84)                   /* Filler                     */

L31450:     FMT CH(15),                  /* Record Id                  */~
                CH(5),                   /* Store ID Code              */~
                CH(6),                   /* Filler                     */~
                25*CH(9),                /* G/L Accounts               */~
                CH(10),                  /* Default Tax Code           */~
                CH(200)                  /* Variable Fields            */

L32000: REM *************************************************************~
            *  L O A D   S T O R E   I N F O    F O R   P R I N T       *~
            * --------------------------------------------------------- *~
            * LOADS THE STORE INFORMATION FROM THE FILE FOR PRINT.      *~
            *************************************************************

            get #3, using L32400, storeid$, information$(),               ~
                nextinvnr$, nextponr$, nextsonr$, nextrcvnr$,            ~
                nextcrmemo$, nextadj$

            init(" ") glaccts$(), gldescrs$(), taxcode$, taxcodedescr$
            call "READ100" (#7, "DEFAULTS.STORE."& str(storeid$), f1%(7))
            if f1%(7) = 0% then return
                get #7 using L32240, glaccts$(), taxcode$, vf$
L32240:              FMT XX(20), XX(6), 25*CH(9), CH(10), CH(200)

             /* Describe Accounts and Tax Code                         */
                for a% = 1 to 17
                     if glaccts$(a%) = " " then L32320
                     call "DESCRIBE" (#4, glaccts$(a%), gldescrs$(a%),   ~
                                                             0%, f1%(4))
                     call "GLFMT" (glaccts$(a%))
L32320:         next a%

                call "DESCRIBE" (#6, taxcode$, taxcodedescr$, 0%, f1%(6))

            return

L32400:     FMT CH(3),                   /* STORE ID CODE              */~
                5*CH(30),                /* STORE INFORMATION          */~
                CH(8),                   /* NEXT INVOICE NUMBER        */~
                CH(16),                  /* NEXT PO NUMBER             */~
                XX(8),                   /* Filler                     */~
                CH(8),                   /* NEXT SALES ORDER NUMBER    */~
                CH(7),                   /* NEXT RECEIVER NUMBER       */~
                CH(8),                   /* Next Credit Memo           */~
                CH(8)                    /* Next Invoice Adjustment    */

        REM *************************************************************~
            *                  M A I N   S C R E E N                    *~
            * --------------------------------------------------------- *~
            * Inputs the Store Information.                             *~
            *************************************************************

            deffn'101(fieldnr%)                    /* INPUT MODE       */
                init(hex(8c)) fac$()
                pf$(1) = "(1)Start Over          (4)Previous Field" &    ~
                         "                       (13)Instructions"
                pf$(2) = "                                        " &    ~
                         "                       (15)Print Screen"
                pf$(3) = "(3)Print Store Listing (6)Proceed to Edi" &    ~
                         "t                      (16)Exit Program"
                pfkey$ = hex(00010304ff060d0f10)
                if fieldnr% <> 1% then L40080
                     str(pf$(1),24,20) = " "
                     str(pfkey$,4,1) = hex(ff)
                     goto L40086
L40080:         str(pf$(3),,23) = " "
                str(pfkey$,3,1) = hex(ff)
L40086:         if fieldnr% > 2% then L40090
                     str(pf$(3),24,20) = " "
                     str(pfkey$,6,1) = hex(ff)
L40090:         goto L40210

            deffn'111(fieldnr%)                    /* EDIT MODE        */
              if fieldnr% <> 0% then L40165
                inpmessage$ = edtmessage$
                init(hex(86)) fac$()     /* Select Field/Option        */
                pf$(1) = "(1)Start Over          (4)A/R Account De" &    ~
                         "faults                 (13)Instructions"
                pf$(2) = "                       (5)A/P Account De" &    ~
                         "faults                 (15)Print Screen"
                pf$(3) = "(12)Delete Store       (9)Variable Field" &    ~
                         "s                      (16)Save Data   "
                pfkey$ = hex(00010405090c0d0f10)
                goto L40335

L40165:         init(hex(8c)) fac$()     /* Modify Field               */
                pf$(1) = "(1)Start Over                           " &    ~
                         "                       (13)Instructions"
                pf$(2) = "                                        " &    ~
                         "                       (15)Print Screen"
                pf$(3) = "                                        " &    ~
                         "                                       "
                pfkey$ = hex(00010d0f)

L40210:     on fieldnr%    gosub    L40300,         /* Store ID         */~
                                    L40285,         /* Descriptive Name */~
                                    L40285,         /* Receivables Name */~
                                    L40285,         /* Rec. Address 1   */~
                                    L40285,         /* Rec. Address 2   */~
                                    L40285,         /* Rec. Address 3   */~
                                    L40300,         /* Next SO Number   */~
                                    L40300,         /* Next Invoice     */~
                                    L40300,         /* Next CR Memo     */~
                                    L40300,         /* Next Inv Adj     */~
                                    L40300,         /* Next PO Number   */~
                                    L40315,         /* Next Rcv Number  */~
                                    L40300,         /* Default Tax Code */~
                                    L40300          /* Revaluation Acct */
                  go to L40335

L40285:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(fieldnr%) = hex(80)
                      return
L40300:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(fieldnr%) = hex(81)
                      return
L40315:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(fieldnr%) = hex(82)
                      return

L40335:     accept                                                       ~
               at (01,02), "Manage Store Master Information",            ~
               at (01,66), "Today:", at(01,73), fac(hex(8c)), date$,     ~
               at (02,02), fac(hex(ac)), scrn2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Store ID",                                   ~
               at (06,30), fac(fac$(1)), storeid$               , ch(03),~
                                                                         ~
               at (07,02), "Descriptive Name",                           ~
               at (07,30), fac(fac$(2)), information$(1)        , ch(30),~
               at (08,02), "Send Checks To ...",                         ~
               at (08,30), fac(fac$(3)), information$(2)        , ch(30),~
               at (09,02), "  Address Line 1",                           ~
               at (09,30), fac(fac$(4)), information$(3)        , ch(30),~
               at (10,02), "  Address Line 2",                           ~
               at (10,30), fac(fac$(5)), information$(4)        , ch(30),~
               at (11,02), "  Address Line 3",                           ~
               at (11,30), fac(fac$(6)), information$(5)        , ch(30),~
                                                                         ~
               at (12,02), "Next Sales Order Number",                    ~
               at (12,30), fac(fac$( 7)), nextsonr$             , ch(08),~
               at (13,02), "Next Invoice Number",                        ~
               at (13,30), fac(fac$( 8)), nextinvnr$            , ch(08),~
               at (14,02), "Next Credit Memo Number",                    ~
               at (14,30), fac(fac$( 9)), nextcrmemo$           , ch(08),~
               at (15,02), "Next Invoice Adjustment",                    ~
               at (15,30), fac(fac$(10)), nextadj$              , ch(08),~
               at (16,02), "Last Purchase Order Number",                 ~
               at (16,30), fac(fac$(11)), str(nextponr$,1,1)    , ch(01),~
               at (16,32), fac(fac$(11)), str(nextponr$,2,6)    , ch(06),~
               at (17,02), "Next Receiver Number",                       ~
               at (17,30), fac(fac$(12)), nextrcvnr$            , ch(07),~
                                                                         ~
               at (18,02), "Default Tax Code",                           ~
               at (18,30), fac(fac$(13)), taxcode$              , ch(10),~
               at (18,49), fac(hex(8c)) , taxcodedescr$         , ch(32),~
                                                                         ~
               at (19,02), "Std Cost Revaluation Acct",                  ~
               at (19,30), fac(fac$(14)), glaccts$(17)          , ch(12),~
               at (19,49), fac(hex(8c)) , gldescrs$(17)         , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1),                         ~
               at (23,02), fac(hex(8c)), pf$(2),                         ~
               at (24,02), fac(hex(8c)), pf$(3),                         ~
                   keys(pfkey$),                                         ~
                   key (keyhit%)

            if keyhit% <> 13 then L40590
                call "MANUAL" ("STRINPUT")
                goto L40335

L40590:     if keyhit% <> 15 then L40610
                call "PRNTSCRN"
                goto L40335

L40610:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            * --------------------------------------------------------- *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'102(fieldnr%)
                init(hex(8c)) fac$()
                gosub set_scrn2
                pf$(1) = "(1)Start Over          (4)Previous Field" &    ~
                         "                       (13)Instructions"
                pf$(2) = "                                        " &    ~
                         "                       (15)Print Screen"
                pf$(3) = "                       (6)Proceed to Edi" &    ~
                         "t                                      "
                pfkey$ = hex(0001ff04ffff060d0f)
                if fieldnr% <> 1% then L41095
                     str(pf$(1),24,20) = " "
                     str(pfkey$,4,1) = hex(ff)
L41095:         goto L41300

            deffn'112(fieldnr%)                    /* EDIT MODE        */
                gosub set_scrn2
              if fieldnr% <> 0% then L41185
                inpmessage$ = edtmessage$
                init(hex(86)) fac$()     /* Select Field/Option        */
                pf$(1) = "(1)Start Over          (4)Main Screen   " &    ~
                         "                       (13)Instructions"
                pf$(2) = "                       (5)A/P Account De" &    ~
                         "faults                 (15)Print Screen"
                pf$(3) = "                       (9)Variable Field" &    ~
                         "s                      (16)Save Data   "
                pfkey$ = hex(00010405ffffff090d0f10)
                goto L41452

L41185:         init(hex(8c)) fac$()     /* Modify Field               */
                pf$(1) = "(1)Start Over                           " &    ~
                         "                       (13)Instructions"
                pf$(2) = "                                        " &    ~
                         "                       (15)Print Screen"
                pf$(3) = "                                        " &    ~
                         "                                       "
                pfkey$ = hex(00010d0f)

L41300:         on fieldnr%   gosub L41396,         /* Shipped, not Inv */~
                                    L41396,         /* A/R              */~
                                    L41396,         /* Cash             */~
                                    L41396,         /* Frt              */~
                                    L41396,         /* Sales Tax Acct   */~
                                    L41396,         /* Sales            */~
                                    L41396          /* Sales Discounts  */
                goto L41452

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$ (fieldnr%) = hex(80)
                      return
L41396:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$ (fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$ (fieldnr%) = hex(82)
                      return

L41452:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Store Master  A/R G/L Account Defaults",       ~
               at (01,66), "Today:", at(01,73), fac(hex(8c)), date$,     ~
               at (02,02), fac(hex(ac)), scrn2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(a4)), header$(1)             , ch(79),~
               at (06,02),                                               ~
                  "Shipped, Unbilled Inventory",                         ~
               at (06,30), fac(fac$ ( 1)), glaccts$(6)          , ch(12),~
               at (06,49), fac(hex(8c)),   gldescrs$(6)         , ch(30),~
               at (07,02),                                               ~
                  "A/R (Net Invoice Amount)",                            ~
               at (07,30), fac(fac$ ( 2)), glaccts$(1)          , ch(12),~
               at (07,49), fac(hex(8c)),   gldescrs$(1)         , ch(30),~
               at (08,02),                                               ~
                  "Cash-in-Bank     ",                                   ~
               at (08,30), fac(fac$ ( 3)), glaccts$(2)          , ch(12),~
               at (08,49), fac(hex(8c)),   gldescrs$(2)         , ch(30),~
               at (09,02),                                               ~
                  "Freight                 ",                            ~
               at (09,30), fac(fac$ ( 4)), glaccts$(3)          , ch(12),~
               at (09,49), fac(hex(8c)),   gldescrs$(3)         , ch(30),~
               at (10,02),                                               ~
                  "Sales Tax          ",                                 ~
               at (10,30), fac(fac$ ( 5)), glaccts$(5)          , ch(12),~
               at (10,49), fac(hex(8c)),   gldescrs$(5)         , ch(30),~
               at (11,02),                                               ~
                  "Sales Distribution       ",                           ~
               at (11,30), fac(fac$ ( 6)), glaccts$(7)          , ch(12),~
               at (11,49), fac(hex(8c)),   gldescrs$(7)         , ch(30),~
               at (12,02),                                               ~
                  "Sales Discounts        ",                             ~
               at (12,30), fac(fac$ ( 7)), glaccts$(4)          , ch(12),~
               at (12,49), fac(hex(8c)),   gldescrs$(4)         , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1)                 , ch(79),~
               at (23,02), fac(hex(8c)), pf$(2)                 , ch(79),~
               at (24,02), fac(hex(8c)), pf$(3)                 , ch(79),~
                     keys(pfkey$),                                       ~
                     key (keyhit%)

               if keyhit% <> 13 then L41860
                  call "MANUAL" ("STRINPUT")
                  goto L41452

L41860:        if keyhit% <> 15 then L41892
                  call "PRNTSCRN"
                  goto L41452

L41892:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return


        REM *************************************************************~
            *                S C R E E N   P A G E   3                  *~
            * --------------------------------------------------------- *~
            * A/P Default Accounts.                                     *~
            *************************************************************

            deffn'103(fieldnr%)
                init(hex(8c)) fac$ ()
                gosub set_scrn2
                pf$(1) = "(1)Start Over          (4)Previous Field" &    ~
                         "                       (13)Instructions"
                pf$(2) = "                                        " &    ~
                         "                       (15)Print Screen"
                pf$(3) = "                       (6)Proceed to Edi" &    ~
                         "t                                      "
                pfkey$ = hex(0001ff04060d0f)
                if fieldnr% <> 1% then L42110
                     str(pf$(1),24,20) = " "
                     str(pfkey$,4,1) = hex(ff)
L42110:         goto L42300

            deffn'113(fieldnr%)                    /* EDIT MODE        */
                gosub set_scrn2
              if fieldnr% <> 0% then L42185
                inpmessage$ = edtmessage$
                init(hex(86)) fac$()     /* Select Field/Option        */
                pf$(1) = "(1)Start Over          (4)Main Screen   " &    ~
                         "                       (13)Instructions"
                pf$(2) = "                       (5)A/R Account De" &    ~
                         "faults                 (15)Print Screen"
                pf$(3) = "                       (9)Variable Field" &    ~
                         "s                      (16)Save Data   "
                pfkey$ = hex(00010405ffffff090d0f10)
                goto L42510

L42185:         init(hex(8c)) fac$()     /* Modify Field               */
                pf$(1) = "(1)Start Over                           " &    ~
                         "                       (13)Instructions"
                pf$(2) = "                                        " &    ~
                         "                       (15)Print Screen"
                pf$(3) = "                                        " &    ~
                         "                                       "
                pfkey$ = hex(00010d0f)

L42300:         on fieldnr%   gosub L42440,         /* Purchases        */~
                                    L42440,         /* Prc-Cst Variance */~
                                    L42440,         /* Interim Liability*/~
                                    L42440,         /* Accounts Payable */~
                                    L42440,         /* Cash-in-Bank     */~
                                    L42440,         /* Discounts Taken  */~
                                    L42440,         /* Freight Expense  */~
                                    L42440,         /* Rcvr Holding     */~
                                    L42440          /* QC Holding       */~

                     goto L42510

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$ (fieldnr%) = hex(80)
                      return
L42440:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$ (fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$ (fieldnr%) = hex(82)
                      return

L42510:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Store Master: A/P G/L Account Defaults",       ~
               at (01,66), "Today:", at(01,73), fac(hex(8c)), date$,     ~
               at (02,02), fac(hex(ac)), scrn2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(a4)), header$(2)             , ch(79),~
               at (06,02),                                               ~
                  "Purchases",                                           ~
               at (06,30), fac(fac$ ( 1)), glaccts$(8)          , ch(12),~
               at (06,49), fac(hex(8c)),   gldescrs$(8)         , ch(30),~
               at (07,02),                                               ~
                  "Price-Cost Variance",                                 ~
               at (07,30), fac(fac$ ( 2)), glaccts$(16)         , ch(12),~
               at (07,49), fac(hex(8c)),   gldescrs$(16)        , ch(30),~
               at (08,02),                                               ~
                  "Interim Liability",                                   ~
               at (08,30), fac(fac$ ( 3)), glaccts$(9)          , ch(12),~
               at (08,49), fac(hex(8c)),   gldescrs$(9)         , ch(30),~
               at (09,02),                                               ~
                  "Accounts Payable",                                    ~
               at (09,30), fac(fac$ ( 4)), glaccts$(10)         , ch(12),~
               at (09,49), fac(hex(8c)),   gldescrs$(10)        , ch(30),~
               at (10,02),                                               ~
                  "Cash-in-Bank",                                        ~
               at (10,30), fac(fac$ ( 5)), glaccts$(11)         , ch(12),~
               at (10,49), fac(hex(8c)),   gldescrs$(11)        , ch(30),~
               at (11,02),                                               ~
                  "Discounts Taken",                                     ~
               at (11,30), fac(fac$ ( 6)), glaccts$(12)         , ch(12),~
               at (11,49), fac(hex(8c)),   gldescrs$(12)        , ch(30),~
               at (12,02),                                               ~
                  "Vendor Freight",                                      ~
               at (12,30), fac(fac$ ( 7)), glaccts$(13)         , ch(12),~
               at (12,49), fac(hex(8c)),   gldescrs$(13)        , ch(30),~
               at (13,02),                                               ~
                  "Receipts Clearing",                                   ~
               at (13,30), fac(fac$ ( 8)), glaccts$(14)         , ch(12),~
               at (13,49), fac(hex(8c)),   gldescrs$(14)        , ch(30),~
               at (14,02),                                               ~
                  "Q.C. Clearing",                                       ~
               at (14,30), fac(fac$ (09)), glaccts$(15)         , ch(12),~
               at (14,49), fac(hex(8c)),   gldescrs$(15)        , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$),                                       ~
                     key (keyhit%)

               if keyhit% <> 13 then L43070
                  call "MANUAL" ("STRINPUT")
                  goto L42510

L43070:        if keyhit% <> 15 then L43110
                  call "PRNTSCRN"
                  goto L42510

L43110:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return


L49000: REM *************************************************************~
            *     G E T   R A N G E   T O   P R I N T   S C R E E N     *~
            * --------------------------------------------------------- *~
            * GETS RANGE TO PRINT FOR PRINTING STORE INFORMATION        *~
            *************************************************************

            str(scrn2$,,60) = " "

L49060:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Store Information Listing",                     ~
               at (01,66), "Today:", at(01,73), fac(hex(8c)), date$,     ~
               at (02,02), fac(hex(ac)), scrn2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "First Store",                                         ~
               at (06,30), fac(hex(81)), firstcode$             , ch(03),~
               at (07,02),                                               ~
                  "Last Store",                                          ~
               at (07,30), fac(hex(81)), lastcode$              , ch(03),~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return",                                 ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L49330
                call "MANUAL" ("STRINPUT")
                goto L49060

L49330:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L49060

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data on main screen.                                 *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50115,         /* Store ID         */~
                                    L50205,         /* Names, Addresses */~
                                    L50205,         /*                  */~
                                    L50205,         /*                  */~
                                    L50205,         /*                  */~
                                    L50205,         /*                  */~
                                    L50220,         /* Next SO          */~
                                    L50250,         /* Next Invoice     */~
                                    L50270,         /* Next CR Memo     */~
                                    L50290,         /* Next Adj         */~
                                    L50505,         /* Next PO          */~
                                    L50585,         /* Next Rcvr        */~
                                    L50630,         /* Deflt Tax Code   */~
                                    L50670          /* Revaluation Acct */
                  return

L50115
*        Test STORE ID
            if storeid$ <> " " then L50140
                call "GETCODE" (#3, storeid$, " ", 0%, 0, f1%(3))
                if f1%(3) = 1% then L50145
                     errormsg$ = hex(00) : return
L50140:     call "SPCSMASH" (storeid$)
L50145:     gosub L30000  :  if storeonfile% = 0% then L50160
                return clear all
                goto editmode
L50160:     if str(storeid$,,1) <> "*" then L50175
                errormsg$ = "STORE CODE MAY NOT BEGIN WITH AN '*'."
                return
L50175:     call "BCKPREFX" (storeid$, errormsg$)
            if errormsg$ = " " then return
                errormsg$ = "STORE CODE MAY NOT BEGIN WITH '" &          ~
                            str(storeid$,,2) & "'."
                return

L50205
*        Test STORE INFORMATION  (Null Test)
            return

L50220
*        Test NEXT SALES ORDER NUMBER
            invnr$ = nextsonr$ : gosub test_invnr
            nextsonr$ = invnr$ : if errormsg$ <> " " then return
            call "BCKPREFX" (nextsonr$, errormsg$)
            return

L50250
*        Test NEXT INVOICE NUMBER
            invnr$ = nextinvnr$ : gosub test_invnr
            nextinvnr$ = invnr$ : return

L50270
*        Test NEXT CREDIT MEMO NUMBER
            invnr$ = nextcrmemo$ : gosub test_invnr
            nextcrmemo$ = invnr$ : return

L50290
*        Test NEXT INVOICE ADJUSTMENT NUMBER
            invnr$ = nextadj$ : gosub test_invnr
            nextadj$ = invnr$ : return

          test_invnr      /* Common Test for Invoice, SO Numbers */
            call "STRING" addr("LJ", invnr$, 8%)
            f% = fieldnr% - 7%
            if f% > 1% then L50335
                if invnr$ <> " " then L50335
                     errormsg$ = "Cannot leave blank." : return
L50335
*         First Determine how much (if any) of a prefix exists.
            if invnr$ = " " then return  /* Use Invoice Number         */
            for p% = 8% to 1% step - 1%
                t1$ = str(invnr$,p%,1)
                if t1$ <> " " and (t1$ < "0" or t1$ > "9") then L50375
            next p%
            p% = 0% : s1% = 1% : s2% = 8% : t2$ = invnr$ : goto L50450

L50375:     if p% <= 3% then L50390
                errormsg$ = "Prefix cannot be longer than 3 characters."
                return
L50390:     s1% = p% + 1% : s2% =  8% - p%
            t1$ = str(invnr$,,p%)  :   t2$ = str(invnr$,s1%,s2%)

L50450
*        Test Suffix (numeric) portion
            convert t2$ to t2%, data goto L50460  :  goto L50465
L50460:         errormsg$ = "Invalid numeric portion."  : return
L50465:     convert t2% to t2$, pic(00000000)

*        Now concoct up the end result
            if p% = 0% then invnr$ = t2$ else                            ~
                            invnr$ = str(t1$,,p%) & str(t2$,s1%,s2%)
            return


L50505
*        Test NEXT PO NUMBER
            if nextponr$ <> " " then L50525
                errormsg$ = "Next P.O. Number Can't Be Blank"
                return
L50525:     if str(nextponr$,1,1) <  "A" then L50535
            if str(nextponr$,1,1) <= "Z" then L50560
L50535:         errormsg$ = "INVALID ALPHABETIC PREFIX"
                return

L50550:         errormsg$ = "INVALID NUMBER ENTERED: " & nextponr$
                return
L50560:     convert str(nextponr$,2,6) to temp, data goto L50550
            convert temp to str(nextponr$,2,6), pic(000000)
            init (" ") str(nextponr$, 8)
            return

L50585
*        Test NEXT RECEIVER NUMBER
            if nextrcvnr$ <> " " then L50605
                errormsg$ = "Next Receiver Number Can't Be Blank"
                return
L50605:     call "NUMTEST" (nextrcvnr$,1,9999999, errormsg$,-0.001,0)
            if errormsg$ <> " " then return
                tran(nextrcvnr$, "0 ")replacing
                return

L50630
*        Test DEFAULT TAX CODE
            taxcodedescr$ = " "
            if taxcode$ = " " then return
            call "GETCODE" (#6, taxcode$, taxcodedescr$, 1%,0, f1%(6))
            if f1%(6) = 1% then return
                errormsg$ = "Tax Code not found on file."
                return

L50670
*        Test Std Cost Revaluation Acct
            a% = 17% : gosub find_account
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51232,         /* Shipped Inventry */~
                                    L51240,         /* AR Account       */~
                                    L51300,         /* Cash Account     */~
                                    L51360,         /* Frt Account      */~
                                    L51420,         /* Sales Tax Acct   */~
                                    L51450,         /* Sales Account    */~
                                    L51480          /* Sales Discounts  */
                     return

L51232
*        Test Data for SHIPPED, NOT BILLED INVENTORY
            a% = 6% : gosub find_account
            return

L51240
*        Test Data for A/R ACCOUNT NUMBER
            a% = 1% : gosub find_account
            return

L51300
*        Test Data For CASH-IN-BANK ACCOUNT
            a% = 2% : gosub find_account
            if errormsg$ <> " " then return
                if glaccts$(a%) = " " then return
                     if glaccttype$ = "$" then return
                          errormsg$ =  "Must be a Cash Account"
                          return

L51360
*        Test Data For FREIGHT ACCOUNT
            a% = 3% : gosub find_account
            return

L51420
*        Test Data for SALES TAX ACCOUNT
            a% = 5% : gosub find_account
            return

L51450
*        Test Data For SALES ACCOUNT
            a% = 7% : gosub find_account
            return

L51480
*        Test Data for SALES DISCOUNTS ACCOUNT
            a% = 4% : gosub find_account
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * TESTS DATA FOR THE ITEMS ON PAGE 3.                       *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52510,         /* Purchases        */~
                                    L52522,         /* Prc-Cst Variance */~
                                    L52530,         /* Interim Liab     */~
                                    L52550,         /* A/P              */~
                                    L52570,         /* Cash-in-Bank     */~
                                    L52590,         /* Discounts Taken  */~
                                    L52610,         /* Vendor Freight   */~
                                    L52630,         /* Rcvr Holding     */~
                                    L52650          /* QC   Holding     */
                     return

L52510:     REM TEST DATA FOR  PURCAHSE ACCOUNT
                a% =  8%:gosub find_account:return
L52522:     REM TEST DATA FOR  PRICE-COST VARIANCE ACCOUNT
                a% = 16%:gosub find_account:return
L52530:     REM TEST DATA FOR  INTERIM LIABILITY
                a% =  9%:gosub find_account:return
L52550:     REM TEST DATA FOR  A/P ACCOUNT
                a% = 10%:gosub find_account:return
L52570:     REM TEST DATA FOR  CASH-IN-BANK
                a% = 11%:gosub find_account:return
L52590:     REM TEST DATA FOR  DISCOUNTS TAKEN
                a% = 12%:gosub find_account:return
L52610:     REM TEST DATA FOR  VENDOR FREIGHT ACCOUNT
                a% = 13%:gosub find_account:return
L52630:     REM TEST DATA FOR  RECEIVING CLEARING ACCOUNT
                a% = 14%:gosub find_account:return
L52650:     REM TEST DATA FOR  QC CLEARING ACCOUNT
                a% = 15%:gosub find_account:return

        find_account

            if glaccts$(a%) <> " " then L58530
               gldescrs$(a%) = " "
               return
L58530:     call "GETCODE" (#4, glaccts$(a%), gldescrs$(a%),             ~
                               0%, 0.30, f1%(4))
            if f1%(4) = 1 then goto L58580
                errormsg$ = "Invalid G/L Account Number: " & glaccts$(a%)
                return
L58580:     get #4 using L58590, glaccttype$
L58590:         FMT XX(39), CH(1)
            return

L59000: REM *************************************************************~
            *     T E S T   D A T A   F O R   P R I N T   R A N G E     *~
            *                                                           *~
            * VALIDATES THE RANGE TO BE PRINTED.                        *~
            *************************************************************

             errormsg$ = " "
             REM HANDLES CASE FOR "ALL" DEDUCTION DEFINITIONS
                 if firstcode$ <> "ALL" then L59120
                    init(hex(00)) firstcode$
                    init(hex(ff)) lastcode$
                    return
L59120:      REM HANDLES CASE FOR SINGLE CODE
                 if lastcode$ <> " " then L59160
                    lastcode$ = firstcode$

L59160:      REM HANDLES CASE FOR A RANGE OF CODES
                 if lastcode$ < firstcode$ then L59200
                    firstcode$ = firstcode$ addc all(hex(ff))
                    return
L59200:      REM HANDLES ERROR MESSAGE -- LAST < FIRST.
                 errormsg$ = "INVALID RANGE!  Please Respecify."
                 return

L60000: REM *************************************************************~
            *        P R I N T   S T O R E   L I S T I N G              *~
            *                                                           *~
            * THIS PROGRAM IS DEDICATED TO THE LEGENDARY GENE DULAY     *~
            * FROM SANTA CLARA SERVICE, WHO HAS PROVIDED TO ME MANY     *~
            * HOURS OF INSPIRATION. YES, FOLKS, GENE IS ONE OF THE BEST *~
            * BAD EXAMPLES I HAVE EVER MET.                             *~
            * (THAT WAS COURTESY OF A CERTIFIABLY WILD AND CRAZY GUY,   *~
            *  KNOWN AFFECTIONATELY TO HIS COHORTS IN MADNESS AS BABY   *~
            *  BRENT...MAY THE LORD BE MERCIFUL...)                     *~
            *************************************************************

            mat linenumber% = con

        /* Print all lines for this store until exhausted.             */
*         Line 1
                print$(1) = storeid$
                print$(2) = information$(1)
                print$(3) = " SO: " & nextsonr$
                print$(4) = "UNBILLED SHPMNTS"
                a%        = 6%
                gosub print_line

                print$(1) = " "

*         Line 2
                print$(2) = information$(2)
                print$(3) = "INV: " & nextinvnr$
                print$(4) = "ACCTS RECEIVABLE"
                a%        = 1%
                gosub print_line

*         Line 3
                print$(2) = information$(3)
                print$(3) = " CM: " & nextcrmemo$
                print$(4) = "A/R CASH-IN-BANK"
                a%        = 2%
                gosub print_line

*         Line 4
                print$(2) = information$(4)
                print$(3) = "ADJ: " & nextadj$
                print$(4) = "FREIGHT         "
                a%        = 3%
                gosub print_line

*         Line 5
                print$(2) = information$(5)
                print$(3) = " PO: " & nextponr$
                print$(4) = "SALES TAX       "
                a%        = 5%
                gosub print_line

*         Line 5
                print$(2) = " "
                print$(3) = "RCV: " & nextrcvnr$
                print$(4) = "SALES DEFAULT   "
                a%        = 7%
                gosub print_line
                print$(3) = " "

*         Lines 6 - 16  Finish up rest of Account Numbers
                print$(4) = "SALES DISCOUNTS " : a% = 4%: gosub print_line

                print$(3) = "TAX CODE: " & taxcode$
                print$(4) = "PURCHASES       " : a% = 8%: gosub print_line

                print$(3) = " "
                print$(4) = "PRC-CST VARIANCE" : a% =16%: gosub print_line
                print$(4) = "INTERIM LIABILTY" : a% = 9%: gosub print_line
                print$(4) = "ACCTS PAYABLE   " : a% =10%: gosub print_line
                print$(4) = "A/P CASH-IN-BANK" : a% =11%: gosub print_line
                print$(4) = "DISCOUNTS TAKEN " : a% =12%: gosub print_line
                print$(4) = "VENDOR FREIGHT  " : a% =13%: gosub print_line
                print$(4) = "RCVR CLEARING   " : a% =14%: gosub print_line
                print$(4) = "Q.C. CLEARING   " : a% =15%: gosub print_line
                print$(4) = "REVALUATION     " : a% =17%: gosub print_line

        /* Finished with this guy - print bottom line and return       */
                print using L61190
                stores% = stores% + 1%
                return


        print_line   /* Prints line      */
            print$(5) = glaccts$(a%)
            print$(6) = gldescrs$(a%)
            if stores% > 1% then gosub page_heading
            print using L61220, print$(1), print$(2), print$(3),          ~
                               print$(4), print$(5), print$(6)
            return

        page_heading
            print page
            page% = page% + 1
            print using L61030, page%, hdrdate$
            print
            print using L61070
            print using L61100
            print using L61130
            print using L61160
            print using L61190
            stores% = 0%
            return

L61030: %PAGE ###   SYS002                S T O R E   M A S T E R   L I S~
        ~ T I N G             ############################################~
        ~####

L61070: %+-----+-----------------------------------+---------------------~
        ~-+---------------------------------------------------------------~
        ~-+
L61100: %!     !                                   !                     ~
        ~ !               A C C O U N T   I N F O R M A T I O N           ~
        ~ !
L61130: %!STORE! N A M E S   A N D   A D D R E S S !NEXT DOCUMENT NUMBERS~
        ~ !----------------+----------------+-----------------------------~
        ~-!
L61160: %!     !                                   !                     ~
        ~ !    USED FOR    !  ACCOUNT CODE  !    D E S C R I P T I O N    ~
        ~ !
L61190: %+-----+-----------------------------------+---------------------~
        ~-+----------------+----------------+-----------------------------~
        ~-+
L61220: %! ### !###################################!#####################~
        ~#!################!################!#############################~
        ~#!
L61250: % ******  NUMBER OF STORES PRINTED THIS RUN: ########  ******

L61270: %            *****  END OF REPORT  *****

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND DISPLAYS A      ,*~
            * MESSAGE (ONLY IN FOREGROUND) WHILE LINKING TO THE NEXT    *~
            * PROGRAM.                                                  *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            call "SETPRNT" ("SYS002", " ", 0%, 1%)

            end
