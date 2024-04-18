        REM *************************************************************~
            *                                                           *~
            *   SSS   Y   Y   SSS   PPPP   U   U  RRRR    GGG   EEEEE   *~
            *  S       Y Y   S      P   P  U   U  R   R  G      E       *~
            *   SSS     Y     SSS   PPPP   U   U  RRRR   G  GG  EEEE    *~
            *      S    Y        S  P      U   U  R  R   G   G  E       *~
            *   SSS     Y     SSS   P       UUU   R   R   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SYSPURGE - Purges Old Data Items From The File When They  *~
            *            Are Past The Last Currently Open Month         *~
            *            According To A User-Set Purge Date.            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/14/80 ! ORIGINAL (MEGANUKE)                      ! BCW *~
            * 04/30/84 ! CHANGED RECLINES RECORD LENGTH           ! ERN *~
            * 12/20/84 ! CHG PROCESS FORM PLOW TO SEQUENTIAL, ... !     *~
            *          ! OPEN FILES IN IO MODE                    ! KAB *~
            * 12/20/84 ! REMOVED REFERENCES TO SAFILES (OBSOLETE) ! KAB *~
            * 02/06/85 ! ADDED POAPLINE DELETE IN VENDOR ROUTINE  ! RAC *~
            * 12/23/85 ! VENDOR, VBK format changes + kill PO     ! ERN *~
            *          !   text and changes history. Skip PO hdr  !     *~
            *          !   delete if last change date not exceeded!     *~
            *          !   Also cleaned up program to make life   !     *~
            *          !   easier for future Meganukers.          !     *~
            * 05/19/86 ! Change invoice file formats              ! HES *~
            * 07/18/86 ! Removed POAP refs, added RCV and new     ! HES *~
            *          ! 'when can go' logic for PAYMASTR & PO    ! HES *~
            * 10/02/86 ! File format changes for Revenue Project  ! ERN *~
            * 11/26/86 ! Changed name to SYSPURGE and implemented ! ERN *~
            *          !  Trial Balance changes so that A/R Invcs !     *~
            *          !  and Checks purged solely per date.      !     *~
            *          !  CRCMASTR & LINE formats changed too.    !     *~
            * 03/10/87 ! Added Serial # TIF purging for A/P Invs  ! ERN *~
            * 03/27/87 ! Removed use of old 'WHICHMON' record     ! HES *~
            * 01/06/88 ! Replaced Sales Order Purge with Inventory! JDH *~
            *          !  Procurment Purge                        !     *~
            * 01/29/88 ! Added currency-specific l.i. files 40-44.! JIM *~
            * 10/17/88 ! Removed A/R purges. ARMPURGE does it now.! JDH *~
            * 04/27/89 ! Added CSHMSCUR and CSHLNCUR to purge.    ! MLJ *~
            * 12/01/89 ! When A/R was removed the A/R purge thru  ! JDH *~
            *          !  date was blanked out. Shouldn't be.     !     *~
            * 05/10/91 ! Removed VBK as it's now done by VBKPURGE.! JDH *~
            * 06/28/91 ! Added ALLFREE.                           ! JDH *~
            * 10/06/92 ! No longer purges recurring or 'held'     ! MLJ *~
            *          !   invoices.                              !     *~
            * 09/22/93 ! Record Size Change on RCVHNYDS           ! KAB *~
            * 12/15/93 ! Modified Payables Purge to purge checks  ! JBK *~
            *          !   (and invoices) written against non-AP  !     *~
            *          !   G/L Liability Accounts.                !     *~
            * 02/07/94 ! PRR 12997 - Modified Purge criteria for  ! JBK *~
            *          !   RCVLINES.  Unvoiced quantities less    !     *~
            *          !   .0050 will now be candidates for purge.!     *~
            * 07/14/94 ! Defer PAYMASTR purge if VPCMASTR on file.! ERN *~
            * 01/16/95 ! PRR 13343. Non-A/P Lia. Accts to 50.     ! JDH *~
            * 07/19/96 ! Internal Unnumbered PRR.  Changed        ! JBK *~
            *          !   any 'DELETE #X' to Calls to "DELETE"   !     *~
            *          !   (maybe a little overkill) to prevent   !     *~
            *          !   deletion problems on UNIX.             !     *~
            * 09/05/96 ! Millie date conversion                   ! DER *~
            *************************************************************

        dim                                                              ~
            accttype$1,                  /* ACCOUNT TYPE THIS INVOICE  */~
            apkey$20,                    /* SYSFILE2 Key Field         */~
            blankdate$8,                 /* blank unfmt date           */~
            blankline$79,                /* LINE FOR INPUT SCREEN      */~
            chkdate$6,                   /* CHECK DATE                 */~
            contract$20,                 /* Contract ID & Line         */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            cutoffdate$8,                /* CUTOFF DATE FOR THIS MODULE*/~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dateposted$8,                /* DATE POSTED THIS RECORD    */~
            dateline$79,                 /* Date Line on screen        */~
            datetime$(6)14,              /* HOW LONG DID THIS TAKE     */~
            delekey$99,                  /* Delete Key                 */~
            diskkey$90,                  /* KEY FOR PLOW ROUTINES      */~
            edtmessage$79,               /* MESSAGE FOR EDIT MODE      */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(20)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            filler$(20)25,               /* SCALAR FILLER > 255 CHAR   */~
            full$4,                      /* FULL PURGE OR NORMAL       */~
            holdflag$1,                  /* Invoice Hold Flag          */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            index%(1),                   /* Search Receiver            */~
            inpmessage$79,               /* INPUT SCREEN MESSAGE       */~
            invoicedate$8,               /* DATE OF INVOICE            */~
            invoicenr$16,                /* INVOICE NUMBER             */~
            invoicetype$1,               /* Invoice Type               */~
            mindate$8,                   /* LATEST PURGE DATE ALLOWED  */~
            nonapacct$(50)9,             /* Valid Non-AP Liability Acct*/~
            oldpur$(6)8,                 /* LAST PURGE DATE USED       */~
            orderdate$8,                 /* ORDER EFFECTIVE DATE       */~
            payacct$9,                   /* Accounts Payable Account   */~
            pf16$16,                     /* PF-16 Description          */~
            postdate$6,                  /* DATE DOCUMENT POSTED       */~
            purdate$(6)8,                /* PURGE DATES TO USE THIS RUN*/~
            rcvdate$8,                   /* DATE RECEIVED              */~
            recflag$1,                   /* AP CHECK RECONCILLIATION FL*/~
            readkey$90,                  /* KEY TO READ DISK WITH      */~
            safeguard$3,                 /* SAFEGUARD ("YES"/"NO")     */~
            tempinvoice$16,              /* INVOICE THIS CHECK PAYS    */~
            textid$4,                    /* TEXT ID                    */~
            textid1$4,                   /* TEXT ID                    */~
            textid2$4                    /* TEXT ID                    */

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
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                    S E L E C T   F I L E S                *~
            *                                                           *~
            * FILES ARE OPENED AS THEY ARE NEEDED TO SPEED UP SYSTEM.   *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 2 ! SYSFILE2 ! Used to find the open months             *~
            * # 3 ! TXTFILE  ! System Text File                         *~
            * # 4 ! SERTIF   ! Serial Number Transaction File           *~
            * #10 ! PAYMASTR ! Payables Invoice Header File             *~
            * #11 ! PAYLINES ! Payables Invoice Line Item File          *~
            * #12 ! CSHMASTR ! Cash Disbursements Header File           *~
            * #13 ! CSHLINES ! Cash Disbursements Line Item File        *~
            * #14 ! VPCMASTR ! Contract Master File                     *~
            * #16 ! HNYDETAL ! Inventory Transaction Detail File        *~
            * #19 ! HNYPROC  ! Inventory Where Procurred                *~
            * #30 ! RCVMASTR ! Receiver Master File                     *~
            * #31 ! RCVLINES ! Receiver Line Items                      *~
            * #32 ! RCVHNYDS ! Receiver Inventory Distribution          *~
            * #41 ! PAYLNCUR ! Currency-specific counterpart of PAYLINES*~
            * #44 ! RCVLNCUR ! Currency-specific counterpart of RCVLINES*~
            * #46 ! CSHLNCUR ! Currency-specific counterpart of CSHLINES*~
            * #47 ! CSHMSCUR ! Currency-specific counterpart of CSHMASTR*~
            *************************************************************

            select  #2, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #3, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select  #4, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #10, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select #11, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #12, "CSHMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 17,                         ~
             alt key 1, keypos =41, keylen =  9, dup,                    ~
                 key 2, keypos =50, keylen =  6, dup,                    ~
                 key 3, keypos =10, keylen =  8, dup

            select #13, "CSHLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup

            select #14, "VPCMASTR",                                      ~
                        varc, indexed, recsize = 600,                    ~
                        keypos =  10,  keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29,         ~
                            key  2, keypos =   60, keylen =  26, dup

            select #16, "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos = 1, keylen = 42,                         ~
                        alternate key 1, keypos = 43, keylen = 6, dup,   ~
                                  key 2, keypos = 49, keylen = 2, dup

            select #19, "HNYPROC",                                       ~
                        varc,     indexed,  recsize =  134,              ~
                        keypos =   32, keylen =  40,                     ~
                        alt key  1, keypos =    7, keylen =  65,         ~
                            key  2, keypos =    1, keylen =  40, dup,    ~
                            key  3, keypos =   41, keylen =  31, dup

            select #30, "RCVMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos= 1, keylen = 16                           ~

            select #31, "RCVLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24              ~

            select #32, "RCVHNYDS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 1, keylen = 86,                          ~
                        alt key 1, keypos = 45, keylen = 42              ~

            select #41, "PAYLNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  28,                     ~
                        alt key  1, keypos =    1, keylen =  32

            select #44, "RCVLNCUR",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 5, keylen = 52,                         ~
                        alt key 1, keypos = 1, keylen = 56

            select #46, "CSHLNCUR",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 5, keylen = 20,                         ~
                        alt key 1, keypos = 1, keylen = 24

            select #47, "CSHMSCUR",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 5, keylen = 17,                         ~
                        alt key 1, keypos = 1, keylen = 21

        call "SHOSTAT" ("Opening files, one moment please")
            call "OPENFILE" (#2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (#3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (#4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes variables for screen input stuff.             *~
            * Initialization for the purge routines themselves is       *~
            * handled in the individual purge routines.                 *~
            *************************************************************

            inpmessage$  = "To Bypass Purging of any of the lines," &    ~
                           " Leave the Purge Date for that line Blank."

            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Value And Press RETURN."

            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            str(dateline$,62%) = "SYSPURGE: " & str(cms2v$,,8)

            call "DATE" addr ("G+", date, -90%, mindate$, err%)
                if err% <> 0 then L65000
            call "DATEOK" (mindate$, mindate%, errormsg$)
                if errormsg$ <> " " then L65000
                   /* Now set #2 record area to the last dates     */
                          call "READ100" (#2, "LAST PURGE DATES", f1%(2))
                          if f1%(2) = 0 then L10000
                               get #2, using L09300, oldpur$()
        REM   XX(6) below is because A/R has been removed
L09300:                        FMT XX(20), XX(6), 6*CH(6)
                          for i% = 1% to 6%
                             call "DATEFMT" (oldpur$(i%))
                          next i%

             init (" ")  nonapacct$()
             apkey$ = "APACCOUNTS-NON" & " "
             call "READ100" (#2, apkey$ , f1%(2%))
                 if f1%(2%) = 0% then L10000
             get #2 using L09430, nonacctnum%, nonapacct$()
L09430:          FMT XX(20), BI(2), 50*CH(09)

L10000: REM *************************************************************~
            *             I N P U T   P U R G E   D A T E S             *~
            * --------------------------------------------------------- *~
            * Inputs purge dates for determining when to purge.  Note   *~
            * that a blank purge date signifies that we will not purge  *~
            * this file at this time.                                   *~
            *************************************************************

        inputmode
            init(" ") purdate$(), errormsg$, full$
                      full$ = "NO"

            for fieldnr% = 1% to 5%
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10180
L10140:         gosub'201(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10140
L10180:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
                next fieldnr%

L11000: REM *************************************************************~
            *              E D I T   P U R G E   D A T E S              *~
            * --------------------------------------------------------- *~
            * Edits the purge information already input.                *~
            *************************************************************

L11060:     gosub'201(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       L12000
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 5 then L11060

L11130:     gosub'201(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

L12000: REM *************************************************************~
            * First a little test to see if anything can happen         *~
            *************************************************************

            for k% = 1% to 6%
               if purdate$(k%) <> " " and purdate$(k%) <> blankdate$ then L19000
            next k%

            errormsg$ = "At Least ONE PURGE DATE Must be NON-BLANK to" & ~
                        " Continue."
            goto L11000

L19000: REM *************************************************************~
            *     S H O W   L A S T   C H A N C E S   T H E N   G O     *~
            * --------------------------------------------------------- *~
            * SHOWS LAST CHANCE MESSAGES THEN HANDLES THE CONTROL--CALLS*~
            * THE SUBROUTINES FOR THE FILES THAT HAVE NON-BLANK PURGE   *~
            * DATES. WHEN ALL PURGING THROUGH, WRITE DATE LAST PURGED.  *~
            * ALSO RECOMPUTE CUSTOMER AND VENDOR BALANCES FOR ALL.      *~
            *************************************************************

*        Get "DO YOU WANT TO PROCEED (Y/N) INFO"
            safeguard$ = " "
L19110:     gosub L42000
                if keyhit%  =  1 then       L11000
                if keyhit% <>  0 then       L19110
            if safeguard$ <> "YES" then L11000

*        Loop that controls what gets purged (also unformats dates)
            for module% = 1 to 6
                datetime$(module%)   = date & time
                call "DATUNFMT" (purdate$(module%))
                if purdate$(module%) = blankdate$ then L19310
                errormsg$ = " "
                     cutoffdate$ = purdate$(module%)
                     call "ALLFREE"
                     on module% gosub         ,    /* LATER            */~
                                         L32000,    /* INV PROCURMENTS  */~
                                              ,    /* POs Removed      */~
                                         L34000,    /* INVENTORY DETAILS*/~
                                              ,    /* GL DETAILS       */~
                                         L35000     /* RECEIVER DETAILS */
                     if errormsg$ <> " " then purdate$(module%) = blankdate$
L19310:     next module%

            REM This to force pay files purge after receivers...
            datetime$(1)   = date & time
            if purdate$(1) = " " or purdate$(1) = blankdate$ then L19410
            errormsg$ = " "
            cutoffdate$ = purdate$(1)
            call "ALLFREE"
            gosub                        L31000     /* VENDOR INVOICES  */
            if errormsg$ <> " " then purdate$(1) = blankdate$

L19410: REM GOSUB 64000   /* Prints Time Stamp for monitor purposes */
            if full$ = "ONLY" then L65000

*        Now (re)Write unformatted dates to SYSFILE2.
*        ARPUR$ just keeps track of the last purge date for A/R which
*         is no longer handled here, but ARMPURGE continues to write it
            call "READ101" (#2, "LAST PURGE DATES", f1%(2))
            if f1%(2) = 0% then L19500
                get #2, using L19480, arpur$, oldpur$()
L19480:              FMT XX(20), CH(6), 6*CH(6)

L19500:     for i% = 1% to 6%
                if i% = 5 then L19540  /* Handled by another program */
                if purdate$(i%) > oldpur$(i%) then                  ~
                     oldpur$(i%) = purdate$(i%)
L19540:     next i%

            oldpur$(3) = blankdate$    /* POs Removed so blank this out */

            put #2, using L19570, "LAST PURGE DATES", arpur$, oldpur$(),  ~
                                                   str(filler$(),1,438)
L19570:     FMT CH(20), CH(6), 6*CH(6), CH(438)

            if f1%(2) = 0% then write #2  else rewrite #2
            goto L65000

        REM *************************************************************~
            *   D E F A U L T / E N A B L E   I N P U T   F I E L D S   *~
            * --------------------------------------------------------- *~
            * Sets default dates for the files, and loads from the      *~
            * SYSFILE2 file the date that file was last purged.         *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20200,         /* VEN INVS & CHX   */~
                                    L20300,         /* INV PROCUREMENTS */~
                                    L20475,         /* RECEIVERS        */~
                                    L20500,         /* INVENTORY DETAILS*/~
                                    L20600          /* FULL CHECK       */
                     return

L20200:     REM DEFAULT/ENABLE for purge vendor   checks and invoices
                /* PURDATE$( 1) = MINDATE$ */
                   return
L20300:     REM DEFAULT/ENABLE for purge inventory procurements
                /* PURDATE$( 2) = MINDATE$ */
                   return
L20475:     REM DEFAULT/ENABLE for purge receivers
                /* PURDATE$( 6) = MINDATE$ */
                   return
L20500:     REM DEFAULT/ENABLE for purge inventory details
                /* PURDATE$( 4) = MINDATE$ */
                   return
L20600:     REM DEFAULT/ENABLE for extended purge testing
                /* FULL$ = "NO" */
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L31000: REM *************************************************************~
            *   P U R G E   V E N D O R   I N V O I C E S   &   C H X   *~
            * --------------------------------------------------------- *~
            * Analogous to purge customer checks and invoices above.    *~
            *************************************************************

        call "SHOSTAT" ("Purging Vendor Invoices And Checks")
            call "OPENFILE" (#10, "IO   ", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "IO   ", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#41, "IO   ", f2%(41), rslt$(41), axd$(41))
            call "OPENFILE" (#12, "IO   ", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#47, "IO   ", f2%(47), rslt$(47), axd$(47))
            call "OPENFILE" (#13, "IO   ", f2%(13), rslt$(13), axd$(13))
            call "OPENFILE" (#46, "IO   ", f2%(46), rslt$(46), axd$(46))
            call "OPENFILE" (#31, "IO   ", f2%(31), rslt$(31), axd$(31))
                count% = f2%(10) + f2%(11) + f2%(12) + f2%(13)
                if count% = 0% then L31105
                     errormsg$ = "VENDOR INVOICES"
                     gosub file_missing
                     goto L31730

L31105: if full$ = "ONLY" then L31610
*        Get next Invoice and delete its associated checks
L31115:     call "READNXT1" (#10, f1%(10))
            if f1%(10) = 0% then L31425
                get #10, using L31140, vencode$, invoicenr$, invoicedate$,~
                         payacct$, accttype$, postdate$, balance,        ~
                         holdflag$,  invoicetype$
L31140:              FMT CH(9), CH(16), XX(16), CH(6), XX(9), CH(9),     ~
                         CH(1), XX(20), CH(6), XX(26), PD(14,4),         ~
                         POS(167), 2*CH(1)

          /* Check if this invoice is O.K. to purge                    */
                if invoicedate$ > cutoffdate$ then L31115
                if postdate$    > cutoffdate$ then L31115
                if holdflag$ = "Y" then L31115        /* Invoice on Hold */
                if invoicetype$ = "R" then L31115     /* Recurring Inv   */
                gosub ap_account_check               /* Check for Non-AP*/
                if accttype$ = "L" and abs(balance) > .001 then L31115

                REM Ok in terms of receivers?
                readkey$ = str(vencode$,,9) & str(invoicenr$,,16)
                call "PLOWNEXT" (#11, readkey$, 25%, f1%(11))
                goto L31225
L31220:         call "READNEXT" (#11, f1%(11))
L31225:         if f1%(11) = 0% then L31280
                     if str(key(#11),,25) <> str(readkey$,,25) then L31280
                     get #11, using L31240, diskkey$, str(diskkey$,26),   ~
                                           contract$
L31240:                   FMT CH(16), CH(19), POS(296), CH(20)
                     if contract$ = " " then L31245
                          call "READ100" (#14, contract$, f1%(14%))
                          if f1%(14%) = 1% then L31115
L31245:              if str(diskkey$,,16) = " " then L31220
                     str(diskkey$,17,9) = vencode$
                     str(diskkey$,45) = all(hex(00))
                     call "PLOWNEXT" (#31, diskkey$, 44%, f1%(31))
                     if f1%(31) <> 0 then L31115
                     goto L31220

L31280:         REM Out It Goes...
                    call "DELETE" (#10, key(#10), 25%)  /* PAYMASTR */
                    readkey$ = str(vencode$,,9) & str(invoicenr$,,16)
                    call "DELETE" (#41, readkey$, 25%)
                    call "DELETE" (#11, readkey$, 25%)
                    delekey$ = "VT" &                                    ~
                               str(vencode$,,9) & str(invoicenr$,,16)
                    call "DELETE" (#4, delekey$, 27%)  /* SERTIF     */
                if accttype$ <> "L" and accttype$ <> "N"                 ~
                                    then L31115 /* NO CHECKS EXPECTED */

           /* Load and purge checks for the invoice we're deleting     */
                call "REDALT1" (#13, invoicenr$, 1%, f1%(13))
L31345:         if f1%(13) = 0% then L31115          /* No line items   */
                     get #13, using L31360, tempvencode$, tempinvoice$,   ~
                                           payacct$, accttype$
L31360:                   FMT CH(9), XX(11), CH(16), CH(9), CH(1)

                 /* If O.K. to purge that detail, FREEZE it.           */
                   if tempinvoice$ <> invoicenr$ then L31115 /* ALL DONE */
                   gosub ap_account_check
                   if accttype$    <> "L"        then L31410 /* NO NEED  */
                   if tempvencode$ <> vencode$   then L31410 /* WRONG VEN*/
                          put #13, using L31400, "F"
L31400:                       FMT POS(79), CH(1)
                          rewrite #13
L31410:                   call "READNXT1" (#13, f1%(13))
                          goto L31345

L31425:   /* Plow the Check Headers for orphans                        */
            init (hex(00)) readkey$

            call "PLOWNEXT" (#12, readkey$, 0%, f1%(12))
L31445:     if f1%(12) = 0 then L31595
                get #12, using L31460, readkey$, chkdate$, postdate$,     ~
                                      recflag$
L31460:              FMT CH(17), CH(6), XX(26), CH(6), XX(26), CH(1)

                if chkdate$  > cutoffdate$ then L31580
                if postdate$ > cutoffdate$ then L31580
                if recflag$ <>"Y"          then L31580
                init (hex(00)) str(readkey$,18)
                     call "PLOWNEXT" (#13, readkey$, 17%, f1%(13))
L31495:              if f1%(13) = 0 then L31555       /* GET IT      */
                     if str(key(#13),,17) <> str(readkey$,,17)           ~
                                          then L31555 /* GET IT      */

                     get #13, using L31520, payacct$, accttype$, recflag$
L31520:                  FMT POS(37), CH(9), CH(1), POS(79), CH(1)
                     gosub ap_account_check
                     if accttype$ <> "L" then L31540  /* THIS GUY OK */
                     if recflag$  <> "F" then L31580  /* HE ISN'T    */
L31540:                 call "READNEXT" (#13, f1%(13))
                        goto L31495

L31555:              call "DELETE" (#12, readkey$,17%)
                     call "DELETE" (#47, readkey$,17%)
                     call "DELETE" (#13, readkey$,17%)
                     call "DELETE" (#46, readkey$,17%)

L31580:              call "READNEXT" (#12, f1%(12))
                     goto L31445

L31595
*        Check if FULL processing desired
            if full$ <> "YES" then L31730

L31610:  /* Plow the Invoice Line Items for orphans          */
            init (hex(00)) diskkey$, readkey$

L31625:     call "PLOWNEXT" (#11, diskkey$, 0%, f1%(11))
            if f1%(11) = 0% then L31670
                call "READ100"  (#10, diskkey$, f1%(10))
                if f1%(10) <> 0% then L31655
                     call "DELETE" (#41, diskkey$, 25%)
                     call "DELETE" (#11, diskkey$, 25%)
L31655:         init(hex(ff)) str(diskkey$, 26)
                goto L31625

L31670:   /* Plow the Check Details for orphans              */
            init (hex(00)) diskkey$, readkey$

L31685:     call "PLOWNEXT" (#13, diskkey$, 0%, f1%(13))
            if f1%(13) = 0% then L31730
            call "READ100"  (#12, diskkey$, f1%(12))
            if f1%(12) <> 0 then L31715
                call "DELETE" (#13, diskkey$, 17%)
                call "DELETE" (#46, diskkey$, 17%)
L31715:     init(hex(ff)) str(diskkey$, 18)
            goto L31685

L31730
*        Close files and exit Purge Vendor Invoices Routine.
                if f2%(10) = 0 then close #10
                if f2%(11) = 0 then close #11
                if f2%(41) = 0 then close #41
                if f2%(12) = 0 then close #12
                if f2%(14) = 0 then close #14
                if f2%(47) = 0 then close #47
                if f2%(13) = 0 then close #13
                if f2%(46) = 0 then close #46
                if f2%(31) = 0 then close #31
                f2%(10), f2%(11), f2%(12), f2%(47), f2%(13), f2%(46),    ~
                     f2%(31), f2%(41), f2%(14) = 1%
                return

        REM *************************************************************~
            *                CHECK A/P LIABILITY ACCOUNTS               *~
            * --------------------------------------------------------- *~
            * Check for Non-A/P General Ledger Liability Accounts       *~
            *                                                           *~
            *************************************************************
        ap_account_check
            if accttype$      <> "L" then return
            if payacct$        = " " then return
            if nonapacct$(1%)  = " " then return
            search str(nonapacct$(), 1%, 9% * nonacctnum%)               ~
                                   = str(payacct$) to index%() step 9%
                if index%(1%)  =  0% then return
            accttype$ = "N"
            return


L32000: REM *************************************************************~
            *        P U R G E   I N V   P R O C U R E M E N T S        *~
            * --------------------------------------------------------- *~
            * Purges Inventory Where Produred that                      *~
            *  are order dated before the cutoff date.                  *~
            *                                                           *~
            *************************************************************

        call "SHOSTAT" ("Purging Inventory Procurements")
            call "OPENFILE" (#19, "IO   ", f2%(19), rslt$(19), axd$(19))
                count% = f2%(19)
                if count% = 0% then L32190
                     errormsg$ = "INV PROCUREMENTS"
                     gosub file_missing
                     goto L32760

L32190: if full$ = "ONLY" then return
*        Routine to purge Inv Procurements.
L32210:     call "READNXT1" (#19, f1%(19))
            if f1%(19) = 0% then L32760
                get #19, using L32240, orderdate$
L32240:              FMT  POS(104), CH(6)
                if orderdate$ > cutoffdate$ then L32210


*        Delete Inv Procurements
            call "DELETE" (#19, key(#19), 40%)   /* Bye Bye  HNYPROC */
                goto L32210

L32760
*        Routine to return from INV PROC. PURGE ROUTINE.
            if f2%(19) = 0 then close #19
            f2%(19) = 1%
            return

L34000: REM *************************************************************~
            *         Z A P   I N V E N T O R Y   D E T A I L S         *~
            * --------------------------------------------------------- *~
            * Zaps inventory details posted before the cutoff date.     *~
            * This routine will look for orphaned details (of course,   *~
            * the notion of orphaned part number is absurd) and zap them*~
            *************************************************************

            if full$ = "ONLY" then return
            call "SHOSTAT" ("Purging Old Inventory Details")
            call "OPENFILE" (#16, "IO   ", f2%(16), rslt$(16), axd$(16))
                count% = f2%(16)
                if count% = 0% then L34170
                     errormsg$ = "INVENTORY DETAILS"
                     gosub file_missing
                     goto L34260

L34170:     init (hex(00)) readkey$
         /* Plow through looking for inventory details to KILL.  */
L34190:     call "READNXT1" (#16, f1%(16))
            if f1%(16) = 0% then L34260
                get #16, using L34220, dateposted$
L34220:              FMT XX(42), CH(6)
                if dateposted$ <= cutoffdate$ then                       ~
                       call "DELETE" (#16, key(#16), 42%)  /* HNYDETAL */
                goto L34190

L34260:     REM Done with Inventory Details...
                if f2%(16) = 0 then close #16
                f2%(16) = 1
                return

L35000: REM *************************************************************~
            *    SECTION FOR RCV FILES (RECEIVERS)                      *~
            *************************************************************

        call "SHOSTAT" ("Purging Completed Receivers")
            call "OPENFILE" (#30, "IO   ", f2%(30), rslt$(22), axd$(30))
            call "OPENFILE" (#31, "IO   ", f2%(31), rslt$(22), axd$(31))
            call "OPENFILE" (#44, "IO   ", f2%(44), rslt$(44), axd$(44))
            call "OPENFILE" (#32, "IO   ", f2%(32), rslt$(22), axd$(32))
                count% = f2%(30) + f2%(31) + f2%(32)
                if count% = 0% then L35140
                     errormsg$ = "RECEIVERS"
                     gosub file_missing
                     goto L35620

L35140: if full$ = "ONLY" then L35450

*        Purge 'Safe' RCV records
L35170:     call "READNEXT" (#30, f1%(30))
            if f1%(30) = 0 then L35440
                get #30 using L35200, readkey$, rcvdate$, textid$
L35200:              FMT CH(16), POS(86), CH(6), XX(1), CH(4)
                if rcvdate$ > cutoffdate$ then L35170
                header_deleteable% = 1

                     init (hex(00)) str(readkey$,17)
                     call "PLOWNXT1" (#31, readkey$, 16%, f1%(31))
                     goto L35280
L35270:              call "READNXT1" (#31, f1%(31))
L35280:              if f1%(31) = 0% then L35360
                     if str(key(#31),,16) <> str(readkey$,,16)           ~
                                                              then L35360
                     gosub purge_rcv_line
                     if gone% = 0 then header_deleteable% = 0
                     goto L35270

            /* Can header go to? */
L35360:         if header_deleteable% = 0 then L35170
                call "READ101" (#30, str(readkey$,,16), f1%(30))
                     if f1%(30) <> 0 then                                ~
                                     call "DELETE" (#30, key(#30), 16%)
                call "TXTFUTIL" (#3, f2%(3), "DELE", textid$)
                call "DELETE" (#32, str(readkey$,,16), 16%) /* J.I.C.*/
                goto L35170

*        Routine to catch Orphaned Details.
L35440: if full$ <> "YES" then L35620
L35450:     init (hex(00)) readkey$
L35460:     call "PLOWNEXT" (#31, readkey$, 0%, f1%(31))
            if f1%(31) = 0% then L35620
                call "READ100" (#30, readkey$, f1%(30))
                if f1%(30) <> 0% then L35590
                     diskkey$ = all(hex(00))
                     str(diskkey$,,16) = str(readkey$,,16)
L35520:              call "PLOWNXT1" (#31, diskkey$, 16%, f1%(31))
                     if f1%(31) = 0% then L35590
                          get #31 using L35550, rcvdate$
L35550:                   FMT POS(122), CH(6)
                          if rcvdate$ <= cutoffdate$ then                ~
                                                     gosub purge_rcv_line
                          goto L35520
L35590:         init (hex(ff)) str(readkey$, 17)
                goto L35460

L35620
*        Routine to return from Vendor Backlog purge routine
                if f2%(30) = 0 then close #30
                if f2%(31) = 0 then close #31
                if f2%(44) = 0 then close #44
                if f2%(32) = 0 then close #32
                f2%(30), f2%(31), f2%(32), f2%(44) = 1%
                return

        purge_rcv_line
             gone% = 0
             get #31 using L35730, rcvheld, inqc, qcheld, amt_not_inv,    ~
                                  uninvcd, textid1$, textid2$
L35730:      FMT POS(164), 3*PD(14,4), POS(256), PD(14,4), POS(356),     ~
                 PD(14,4),POS(388),2*CH(4)
             if abs(rcvheld)     > .0001 then return
             if abs(inqc)        > .0001 then return
             if abs(qcheld)      > .0001 then return
             if abs(uninvcd)     = 0    then L35771
             if abs(uninvcd)     > .0049 then return
             if abs(amt_not_inv) > .0049 then return
*           IF ABS(UNINVCD) > .0001 THEN RETURN
L35771:      init (hex(00)) delekey$
             get #31 using L35774, str(delekey$,3,16), str(delekey$,19,19)
L35774:          FMT POS(26), CH(16), POS(51), CH(19)
             str(delekey$,1,2) = "PO"
             call "READ101" (#44, key(#31), f1%(44))
                if f1%(44) <> 0% then call "DELETE" (#44, key(#44), 52%)
             call "DELETE" (#31, key(#31), 52%)   /* RCVLINES */
             call "DELETE" (#32, str(key(#31),,44), 44%)
             call "TXTFUTIL" (#3, f2%(3), "DELE", textid1$)
             call "TXTFUTIL" (#3, f2%(3), "DELE", textid2$)
             call "DELETE" (#4, delekey$, 37%)
             gone% = 1
        return

        REM *************************************************************~
            *     M I S S I N G   F I L E   N O T I F I C A T I O N     *~
            * --------------------------------------------------------- *~
            * Notify operator that a file was not opened and therefore, *~
            * purging for the module will not take place.               *~
            *************************************************************
        file_missing
            keyhit% = 0%
            errormsg$ = errormsg$ & ": unable to open all required files."
            inpmessage$ = "Purging for this module will NOT occur."
            edtmessage$ = "Press (RETURN) to continue..."
            call "ASKUSER" (0%, "FILE NOT OPENED", errormsg$,            ~
                                inpmessage$, edtmessage$      )
            return

        REM *************************************************************~
            *      I N P U T   P U R G E   D A T E S   S C R E E N      *~
            * --------------------------------------------------------- *~
            * INPUTS THE DESIRED PURGE DATE ONTO THE SCREEN.            *~
            *************************************************************

            deffn'201(fieldnr%, mode%)
                  init(hex(8c)) fac$()
                  if fieldnr% = 0% then init(hex(86)) fac$()
                  on fieldnr% gosub L40210,         /* VEN INVS & CHX   */~
                                    L40210,         /* INV PROCUREMENTS */~
                                    L40210,         /* RECEIVERS        */~
                                    L40210,         /* INVENTORY DETAILS*/~
                                    L40210          /* FULL PURGE       */
                     goto L40280

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
L40210:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L40280:     pf16$ = "(16)Exit Program"
            str(dateline$,,60) = "Enter Purge Dates"
            if mode% <> 2% then L40320
                if fieldnr% = 0% then pf16$ = "(16)BEGIN PURGE" else     ~
                                      pf16$ = " "

L40320:     accept                                                       ~
               at (01,02), "PURGE OLD INFORMATION",                      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), dateline$              , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,39), " Purge To ", at(05,52), "Last Purge",        ~
                                                                         ~
               at (06,02), "Vendor Invoices, Checks",                    ~
               at (06,53), fac(hex(8c)),  oldpur$ ( 1)          , ch(08),~
               at (06,40), fac(fac$( 1)), purdate$( 1)          , ch(08),~
                                                                         ~
               at (07,02), "Inventory Procurements",                     ~
               at (07,53), fac(hex(8c)),  oldpur$ ( 2)          , ch(08),~
               at (07,40), fac(fac$( 2)), purdate$( 2)          , ch(08),~
                                                                         ~
               at (08,02), "Purchasing Receivers",                       ~
               at (08,53), fac(hex(8c)),  oldpur$ ( 6)          , ch(08),~
               at (08,40), fac(fac$( 3)), purdate$( 6)          , ch(08),~
                                                                         ~
               at (09,02), "Inventory Details",                          ~
               at (09,53), fac(hex(8c)),  oldpur$ ( 4)          , ch(08),~
               at (09,40), fac(fac$( 4)), purdate$( 4)          , ch(08),~
                                                                         ~
               at (10,02), "Delete Orphans? (Yes/No/Only)",              ~
               at (10,40), fac(fac$( 5)), full$                 , ch(04),~
                                                                         ~
               at (18,20), "Purge Dates must be on or before",           ~
               at (18,53), fac(hex(a4)), mindate$               , ch(08),~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(84)), pf16$,                          ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40800
                  call "MANUAL" ("SYSPURGE")
                  goto L40320

L40800:        if keyhit% <> 15 then L40840
                  call "PRNTSCRN"
                  goto L40280

L40840:        close ws
               call "SCREEN" addr ("C", err%, "I", i$(), cursor%())
               return

L42000: REM *************************************************************~
            *            S A F E G U A R D   S C R E E N   1            *~
            * --------------------------------------------------------- *~
            * Safeguards the accidental purging of data by asking if he *~
            * really wants to purge it.                                 *~
            *************************************************************
            str(dateline$,,60) = "Enter Safeguard Word"

L42070:     accept                                                       ~
               at (01,02),                                               ~
                  "PURGE OLD INFORMATION",                               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), dateline$              , ch(79),~
                                                                         ~
               at (08,23), "**************************************",     ~
               at (09,23), "* PURGING of data is about to begin. *",     ~
               at (10,23), "*      Do you wish to PROCEED?       *",     ~
               at (11,23), "*                                    *",     ~
               at (12,23), "*                                    *",     ~
               at (13,23), "*                                    *",     ~
               at (14,23), "*     Responses other than 'YES'     *",     ~
               at (15,23), "*    Will return you to Edit Mode    *",     ~
               at (16,23), "**************************************",     ~
                                                                         ~
               at (12,40), fac(hex(81)), safeguard$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
                                                                         ~
               at (22,02), "(1)Return to Edit Mode",                     ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L42380
                  call "MANUAL" ("SYSPURGE")
                  goto L42070

L42380:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  go to L42070

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test purge dates against latest date, and make sure they  *~
            * are Okay syntactically.  Leave blanks alone; they are the *~
            * signal to NOT purge something.                            *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50190,         /* VEN INVS & CHX   */~
                                    L50190,         /* INV PROCUREMENTS */~
                                    L50220,         /* RECEIVERS        */~
                                    L50250,         /* INVENTORY DETAILS*/~
                                    L50360          /* FULL PURGE       */
                     return

L50190:     REM Test data for FIRST THREE DATES
            d% = fieldnr% : goto L50280

L50220:     REM Test data for RECEIVER purge date
            d% = 6 : goto L50280

L50250:     REM Test data for INVENTORY DETAILS purge date
            d% = 4

L50280:     if purdate$(d%) = " " or purdate$ (d%) = blankdate$ then return
                call "DATEOK" (purdate$(d%), date%, errormsg$)
                if errormsg$ <> " "      then return
                if date%     <= mindate% then return
                     errormsg$ = "Date for Purge Must Be ON or BEFORE " &~
                                 mindate$ & ": " & purdate$(d%)
                     return

L50360:     REM Test data for FULL DETAIL TEST AND PURGE
            if str(full$,,1) = "Y" then full$ = "YES"
            if str(full$,,1) = "N" then full$ = "NO"
            if str(full$,,1) = "O" then full$ = "ONLY"

            if full$ = "YES" or full$ = "NO" or full$ = "ONLY" then return
                errormsg$ = "'YES', 'NO' or 'ONLY' Please"
                return


*        PRINT TIME SUMMARY FOR NON-BELIEVERS
            select printer (134)
            print page
            for i = 1 to 6
                if i = 5 or i = 3 then L64060
                call "DATUNFMT" (oldpur$(i))
                print using L64200, purdate$(i), oldpur$(i), datetime$(i)
L64060:     next i
            close printer

            return

L64200: %######   ######   ##############
        %                  ##############

L65000: REM *************************************************************~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("ONE MOMENT PLEASE")

            end
