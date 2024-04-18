        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP    AAA   Y   Y  JJJJJ  U   U  RRRR   N   N  L       *~
            *  P   P  A   A  Y   Y    J    U   U  R   R  NN  N  L       *~
            *  PPPP   AAAAA   YYY     J    U   U  RRRR   N N N  L       *~
            *  P      A   A    Y    J J    U   U  R   R  N  NN  L       *~
            *  P      A   A    Y     J      UUU   R   R  N   N  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PAYJURNL - READS PAYUPDTE TRANSACTIONS FILE (PAYJRNTF)    *~
            *               THE FOLLOWING OPTIONS WILL RESORT IN ACTIONS*~
            *               BY ACCOUNT PROVIDING DESIRED LEVEL OF DETAIL*~
            *               FOR THE PRINTING AND AUDIT TRAIL POSTING.   *~
            *                                                           *~
            *            OPTIONS FOR PRINTING; FUL = LINE BY LINE       *~
            *                 (REPRTDET)       DTL = SUMMARY VEN & INV  *~
            *                                  SUM = SUMMARY BY USERID  *~
            *               SUM WILL RECAP TRANSACTIONS WITH FUL OR DTL *~
            *                       ------------------------------------*~
            *            OPTIONS ON GLPOSTING; FUL = POST LINE BY LINE  *~
            *                 (POSTME)         DTL =  "   VENDOR BY INVC*~
            *                                  SUM =  "      BY USERID  *~
            *                                  NO  = NO POSTING         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/18/85 ! ORIGINAL                                 ! JWG *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION   ! RAC *~
            * 07/09/85 ! FINAL CLEANUP & GL CODE FMT CONTROL      ! KEN *~
            *          ! PROGRAM STANDARD IS INTERNAL GL CODE     ! KEN *~
            * 12/13/85 ! Vendor file format changes               ! MJB *~
            * 01/06/86 ! Changed Call To GLPOST                   ! HES *~
            * 05/26/87 ! Removed HNYMASTR. Updated obsolete code. ! JIM *~
            * 08/06/90 ! G/L Export file modifications.           ! RAC *~
            * 01/28/92 ! PRR 11597 - Corrected JOURNAL spelling.  ! MLJ *~
            * 10/07/92 ! Eliminated Locator SORT for UNIX.        ! MLJ *~
            * 02/09/93 ! PRR 12569, 12590, 12719 Program reports  ! MLJ *~
            *          !  correct invoice total on adj and 0 bal  !     *~
            *          !  invoices using new inv amt from tif.    !     *~
            *          !  Corrected numerous implied integers.    !     *~
            * 08/09/93 ! PRR 13001 - changed lines 25318 & 41126  ! MLJ *~
            *          !  to print TOTALINVC$ (not NEWINVAMT$).   !     *~
            * 01/05/94 ! PRR 13080 - Corrected the calculation of ! JBK *~
            *          !  the Invoice Aount and Invoice Total.    !     *~
            *          !  Correction or deletation of existing    !     *~
            *          !  Invoice will show the new balance of    !     *~
            *          !  invoice. (ie., a deleted invoice will   !     *~
            *          !  show as zero).                          !     *~
            *          ! Miscellaneous change to journal printing.!     *~
            *          !  so debit and credit subtotals will print!     *~
            *          !  in all cases for a full report.         !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            account$9,                   /* GENERAL ACCOUNT NUMBER     */~
            acctdescr$32,                /* ACCOUNT PRINT DESCRIPTION  */~
            apdate$8,                    /* PAYJRNTF PAYABLES DATE     */~
            credit$12,                   /* CREDIT PRINT FIELD         */~
            date$45,                     /* DATE FOR SCREEN DISPLAY    */~
            debit$12,                    /* DEBIT PRINT FIELD          */~
            glacct$9,                    /* PAYJRNTF GLPOST ACCT$      */~
            gldate$6,                    /*    "       "    DATE$      */~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            gltext$100,                  /*    "       "    TEXT       */~
            textarr$(2,250)100,          /*    "       "     " ARRAY   */~
            invoice$16,                  /*    "     INVOICE THIS REC  */~
            jnlid$3,                     /* JOURNAL ID                 */~
            locarr%(250),                /* SORT LOCATION OF ACCT ARRAY*/~
            location$9,                  /* STEP VALUE FOR GOSUB'161   */~
            moduleno$20,                 /* MODULENO                   */~
            msg$70,                      /* MESSAGE FOR  'SHOWMSG      */~
            needsubtotals$1,             /* FLAG Y=VEN&INVC(POST/PRINT)*/~
            newinvamt$12,                /* NEW INV AMT FROM PAYJRNTF  */~
            option$(5)3,                 /* PROC OPTIONS VALID TEST ARR*/~
            payjrntfkey$50,              /*    "     READKEY VALUE     */~
            postacct$(250)9,             /* VEN & INVC ACCT ARRAY      */~
            postamnt(2,250),             /*     "       "  VALUES ARRAY*/~
            postme$3,                    /* PROC OPTION  ON POSTING    */~
            prtacct$(3)16,               /* FOR PRINT                  */~
            prtfulven$9,                 /* VENCODE FOR PRINT"FUL"OPTN */~
            prtfulvendesc$25,            /* VENDOR DESC      "         */~
            prtfulinvc$16,               /*    "  INVOICE    "         */~
            reprtdet$3,                  /* PROC OPTION FOR PRINTING   */~
            rptsel$10,                   /* Type for askuser           */~
            sortacct$(250)10,            /* USER ACCT SUMMARY ARRAY    */~
            sumacct$(250)9,              /* USER ACCT SUMMARY ARRAY    */~
            sumamnt(2,250),              /*   "  AMOUNT ARRAY,1=DB,2=CR*/~
            summary$1,                   /* SUMMARY INDICATOR          */~
            time$8,                      /* PAYJRNTF RECORD TIME       */~
            titlj$60,                    /* JOURNAL TITLE              */~
            updctl$(2)3,                 /* PROC ARRAY VALUES          */~
            usergldate$6,                /*  USER      "    DATE       */~
            userid$3,                    /* THIS USER'S ID CODE        */~
            vencode$9,                   /* PAYJRNTF VENDOR CODE       */~
            vendor$30                    /* VENDOR DESCRIPTION         */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! USERINFO ! Users Default Information File           *~
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! VENDOR   ! Vendor Master File                       *~
            * #15 ! GLMAIN   ! General Ledger Main File                 *~
            * #16 ! GLDETAIL ! General ledger detail file               *~
            * #23 ! PAYJRNTF ! PayJournal Transitnal file,from PAYUPDTE *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =   3                      ~

            select #2,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select #15, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #16, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos =    1, keylen =  26                      ~


            select #23, "PAYJRNTF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 688,                                  ~
                         keypos =  1, keylen = 36



            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#1, "SHARE",f2%(1%), rslt$(1%), axd$(1%))
            call "OPENFILE" (#2, "SHARE",f2%(2%), rslt$(2%), axd$(2%))
            call "OPENFILE" (#3, "SHARE",f2%(3%), rslt$(3%), axd$(3%))
            call "OPENFILE" (#15,"SHARE",f2%(15%),rslt$(15%),axd$(15%))
            call "OPENFILE" (#16,"SHARE",f2%(16%),rslt$(16%),axd$(16%))
            call "OPENFILE" (#23,"SHARE",f2%(23%),rslt$(23%),axd$(23%))

        REM *************************************************************~
            * THE GLDETAIL FILE WILL BE CREATED IF NEC. BY SUB 'GLPOST  *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            * INITIALIZES RECAP STACKS, SETS USER ID, OTHER STUFF.      *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            option$() = "FULDTLSUMYESNO "

        REM *************************************************************~
            *    INITIALIZATION BLOCK FOR ALL START-UP VARIABLES        *~
            *************************************************************

*        Verify the user's Accounts Payable posting date is within the
*          3 months open for posting.

            call "EXTRACT" addr ("ID", userid$)
                call "READ100" (#1, userid$, f1%(1%))
                if f1%(1%) = 1% then L09250
            f1%(1%) = 2%
            call "ASKUSER" (f1%(1%), "*** UNAUTHORIZED ACCESS ***",      ~
                "Sorry, " & userid$ & ", you are not authorized to post"&~
                " invoices", " ", "Press any PF key to terminate program")
                goto L65000

L09250:     get #1, using L09260, usergldate$
L09260:         FMT XX(9), CH(6)
            call "WHICHMON" (#2, usergldate$, whichmonth%)
                if whichmonth% <> 0% then L09330
            f1%(1%) = 2%
            call "ASKUSER" (f1%(1%), "*** POSTING DATE ERROR ***",       ~
                "Sorry, " & userid$ & ", your posting date is not in an"&~
                " open month", " ", "Press any PF key to terminate prog"&~
                "ram")
                goto L65000

          /* Do GETPARM to identify which update modules to execute    */
L09330:     call "GETPARM" addr ("I ", "R", "UPDCTL  ", " ", "0001",     ~
                                 "PAYJNL", "LEVEL OF DETAIL?", 16%,      ~
                                 "K","REPORT  ",updctl$(1),3%,0%,0%,"A", ~
                                 "K","POSTME  ",updctl$(2),3%,0%,0%,"A")
               reprtdet$ = updctl$(1%)
               postme$   = updctl$(2%)
            REM MUST CHECK FOR VALID OPTION ON REPRTDET$
               for i% = 1% to 3%
                  if reprtdet$ = option$(i%) then L09450
               next i%
               /*ERROR INPUT FOR REPRTDET$ MAKE THEM TRY AGAIN*/
               goto L09500
L09450:     REM MUST CHECK FOR VALID POSTME$ ALSO
               for i% = 1% to 5%
                  if postme$ = option$(i%) then L09530
               next i%
               /*ERROR INPUT FOR POSTME$ MAKE THEM TRY AGAIN*/
L09500:     msg$ = "One or both of the procedure options are invalid. P"&~
                "lease re-enter."
            gosub status_screen
               goto L09330
L09530:      REM DO A GETPARM TO FIND JNLID$
                call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",~
                                     "PAYJNL",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                if jnlid$ = " " then L09530

                returncode% = 1%
                moduleno$ = "02"
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                titlj$, " ", #2, f2%(2), returncode%)
                call "FMTTITLE" (titlj$, "JOURNAL", 12%)
                if postme$ = "NO" then L09710
                if summary$ = "Y" then postme$ = "SUM"
                if summary$ = "F" then postme$ = "FUL"
                if summary$ = "D" then postme$ = "DTL"
                if summary$ = "N" then postme$ = "DTL"
L09710
*        SET UP THE PRINTER
                select printer (134)
                call "SETPRNT" ("A/P010", " ", 0%, 0%)

            if reprtdet$ = "FUL" then rptsel$ = "Full"
            if reprtdet$ = "DTL" then rptsel$ = "Detail"
            if reprtdet$ = "SUM" then rptsel$ = "Summary"

            msg$ = "Printing " & rptsel$ & " Payables Journal with "     ~
                               & postme$ & " Posting to GL" & " "

            call "SHOSTAT" ( msg$ )

*       * Determine whether need summary by vend & invc anywhere
*       *  NEEDSUBTOTALS$ = "Y" for yes do search and keep track
                needsubtotals$ = "Y"  /*Default since probably need*/
            if reprtdet$ <> "DTL"    and     postme$ <> "DTL"            ~
                then needsubtotals$ = "N"

*       * IF F2%(23) = 0%    THEN 10000
*       * GOTO 65000    /*CANT READ THE FILE,I'm taking a break. BYE */

            payjrntfkey$ = str(userid$,,3%) & hex(000000)
            call "READ100" (#23, payjrntfkey$, f1%(23%))
                if f1%(23%) = 0% then L65000 /* No Batch Created */
            get #23 using L09940, pstseq%
L09940:     FMT XX(39), BI(4)

        REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * PLOWS THROUGH TRANSACTION FILE AND GETS NEXT INVOICE.     *~
            *************************************************************

            REM INITIALIZE THE ARRAYS AND POINTERS AND STACKS FOR THIS
                init(" ")     payjrntfkey$,prtfulven$,prtfulvendesc$,    ~
                              prtfulinvc$, postacct$(), newinvamt$
            totalinvc, totaldebits, totalcredits, newinvamt = 0
            payjrntfkey$ = str(userid$,,3%) & hex(0000)
               REM CHECK FOR ANY OF THIS USERS RECORDS
            call "PLOWNXT1" (#23, payjrntfkey$, 3%, f1%(23%))
               if f1%(23%) = 1% then L10240
               goto L65000

L10170
*                CONTINUE PROCESSING RECORDS FOR THIS USER
            totalinvc$ = newtotalinvc$
            call "PLOWNXT1" (#23, payjrntfkey$, 3%, f1%(23%))
               if f1%(23%) <> 1% then recap
               init(" ")     postacct$(), textarr$()
               mat postamnt = zer
               point1% = 0%
L10240:     invcnt% = invcnt% + 1%
            newinvcflag% = 1%  /* USE WITH PRINT"FUL" FOR SEPARATION*/
            gosub load_payjrntf
*          IF BALFLAG% = 1% THEN GLAMTX = -(GLAMT)  ELSE                ~
*             GLAMTX = GLAMT
            glamtx = newinvamt
            convert glamtx to newtotalinvc$, pic(-########.##)
            totalinvc = totalinvc + glamtx
               goto L10360

L10310:     call "PLOWNXT1" (#23, payjrntfkey$, 28%, f1%(23%))
               if f1%(23%) = 1% then L10340
                  gosub L20000   :  goto L10170
L10340
*             WE GOTTA GO READ ANOTHER REC TO PROCESS
            gosub load_payjrntf
L10360
*             PUSH THE VALUE ONTO THE SUMMARY AND/OR SUBTOTAL STACK
            gosub'161( glacct$, glamt, balflag% )
*             CHECK FOR "FUL" DETAIL ON PRINTING OR POSTING
            if reprtdet$ = "FUL" then gosub L25000

            if postme$ <> "FUL" then L10310
                   /* USE TEXT$ = GLTEXT$ FROM PAYJRNTF FILE*/
               if balflag% = 1% then gosub'151(glacct$,glamt,0 )         ~
                                else gosub'151(glacct$,0,glamt )
               goto L10310



L19000: REM *************************************************************~
            *  Tricky Locator SORT replacement.  Thanks Kenny.          *~
            *************************************************************

            init (hex(ff)) sortacct$()
            on sort% goto L19080, L19140
                return

L19080:     for s1% = 1% to point1%
                str(sortacct$(s1%),  1%, 9%) = str(postacct$(s1%), 1%, 9%)
                str(sortacct$(s1%), 10%, 1%) = bin(s1%, 1%)
            next s1%
            s1% = point1%
            goto L19200

L19140:     for s1% = 1% to point2%
                str(sortacct$(s1%), 1%, 9%) = str(sumacct$(s1%), 1%, 9%)
                str(sortacct$(s1%), 10%, 1%) = bin(s1%, 1%)
            next s1%
            s1% = point2%

L19200:     call "SORT" addr(sortacct$(), s1%, 10%)

            mat locarr% = zer
            for s2% = 1% to s1%
                locarr%(s2%) = val(str(sortacct$(s2%), 10%, 1%), 1%)
            next s2%

            return

L20000: REM *************************************************************~
            *  Print a summation of the G/L activity for this invoice.  *~
            *************************************************************


                 /*CHECK FOR DTL PRINT OR POST REQUEST*/
            if needsubtotals$ <> "Y" then return
                mat locarr% = zer
                if point1% < 1% then return

            REM SORT THE ACCOUNT ARRAY TO PRINT NICELY,RETAIN LOCATION
         /* CALL "SORT"ADDR(POSTACCT$(), POINT1%, 9%, LOCARR%(), 1%, 9%, ~
                               "A", "L", 4%) */
            sort% = 1% : gosub L19000
            switch% = 1%
*              DEBITS, CREDITS = 0
               if reprtdet$ <> "DTL" and summary$ <> "D" then            ~
                     debits, credits = 0

            for y% = 1% to point1%
               x% = locarr%(y%)
               if reprtdet$ <> "DTL" then L20192
                   gosub L20300
               call "DESCRIBE" (#15,postacct$(x%),acctdescr$,0%,f1%(15%))

               call "CONVERT" (postamnt(1%, x%), 2.2, debit$)
                      if abs(postamnt(1%,x%))< .005 then init(" ") debit$
               call "CONVERT" (postamnt(2%, x%), 2.2, credit$)
                      if abs(postamnt(2%,x%))< .005 then init(" ") credit$

               prtacct$(1%) = postacct$(x%)
               call "GLFMT" (prtacct$(1%))
                   print using L20840, prtacct$(1%), acctdescr$, debit$,  ~
                                      prtacct$(1%), acctdescr$, credit$
                   debits = debits + postamnt(1%, x%)
                   totaldebits = totaldebits + postamnt(1%, x%)
                   credits = credits + postamnt(2%, x%)
                   totalcredits = totalcredits + postamnt(1%, x%)
L20192:     if postme$ <> "DTL"  then  L20205
                     /* USE TEXT$ SENT FROM PAYJRNTF REC*/

               gltext$ = textarr$(1%,x%)
               gosub'151(postacct$(x%), postamnt(1%,x%), 0 )
               gltext$ = textarr$(2%,x%)
               gosub'151(postacct$(x%), 0, postamnt(2%,x%) )
L20205:     next y%
               if reprtdet$ <> "DTL" then return
                   call "CONVERT" (debits, 2.2, debit$)
                   call "CONVERT" (credits, 2.2, credit$)
                   print using L20900
                   print using L20870, debit$, credit$
                   print using L20900
                   print
                   line% = line% + 4%
                   return    : REM THIS GOES BACK TO MAIN PROGRAM SECTION

L20300: REM *************************************************************~
            *    PAGE CONTROL SUBROUTINE FOR PRINTING SALES JOURNAL.    *~
            *************************************************************

            line% = line% + 1%
               if page% = 0% then L20390
                   if switch% = 1%  then   L20437
                   if  line% < 55%   then return
                   print using L20900
L20390:            print page
              page% = page% + 1%  : line% = 0%
              call "DATE" addr("HD", date$)
              if postme$ <>"NO " then print using L20590,page%,reprtdet$, ~
                                 date$                                   ~
                                 else print using L20630,page%,reprtdet$, ~
                                 date$
                print  "A/P010"
L20437:        if  line% > 45%   then L20390
               print
               print using L20670

                  call "DESCRIBE" (#3, vencode$ , vendor$, 0%, f1%(3%))
               print using L20700, invcnt%, vendor$, vencode$

                  call "DATEFMT" (apdate$)
               print using L20730, invoice$, apdate$

               print using L20900
               print using L20770
               print using L20800
               print using L20900
               if  line% = 0% then line% = 9% else line% = line% + 7%

               switch% = 0%
               return

L20590: %PAGE ##### ### #################################################~
        ~########              ###########################################~
        ~##

L20630: %PAGE ##### ### #################################################~
        ~###### (NO GL POSTING  ##########################################~
        ~###

L20670:     %+-----------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-----+

L20700:     %! ###  VENDOR: ##############################       CODE: ##~
        ~#######                                                          ~
        ~     !

L20730:     %!      INVOICE: ################                A/P DATE: ##~
        ~######                                                           ~
        ~     !


L20770:     %!                 !          D E B I T S          !         ~
        ~     !                 !         C R E D I T S         !         ~
        ~     !

L20800:     %! ACCOUNT NUMBER  !      ACCOUNT DESCRIPTION      !    AMOUN~
        ~T    ! ACCOUNT NUMBER  !      ACCOUNT DESCRIPTION      !    AMOUN~
        ~T    !


L20840:     %! ################! ##############################! ########~
        ~#### ! ################! ##############################! ########~
        ~#### !

L20870:     %!  ** TOTALS **   !                               ! ########~
        ~#### !         <=  TOTALS SHOULD AGREE  =>             ! ########~
        ~#### !

L20900:     %+-----------------+-------------------------------+---------~
        ~-----+-----------------+-------------------------------+---------~
        ~-----+

L20930: %                                     D A I L Y   G E N E R A L  ~
        ~ L E D G E R  R E C A P

L25000: REM *************************************************************~
            *  Print This record from PAYJRNTF file no subtotal stuff   *~
            *************************************************************

              /*WHEN YOU GO TO 25300 CHECK FOR NEW VENDOR/INVOICE*/
               init(" ") prtfulven$,prtfulvendesc$,prtfulinvc$
               f1%(4%) = 0%
                   gosub L25300
               if reprtdet$ <> "FUL" then                                ~
               call "DESCRIBE" (#15,glacct$, acctdescr$,0%,f1%(15%))     ~
                       else                                              ~
               acctdescr$ = str(gltext$,65%,36%)
               if f1%(4%) = 1% then prtfulvendesc$ = str(gltext$,33%,32%)
               init(" ") debit$, credit$
               call "DATEFMT" (apdate$)
                  if balflag% = 2% then L25115
                  call "CONVERT" ( glamt, 2.2, str(debit$,,10%))
                      if abs( glamt)< .005 then init(" ") debit$
                   debits = debits + glamt
                   totaldebits = totaldebits + glamt
                      goto L25131
L25115:           call "CONVERT" ( glamt, 2.2, str(credit$,,10%))
                      if abs( glamt)< .005 then init(" ") credit$
                   credits = credits + glamt
                   totalcredits = totalcredits + glamt

L25131:            prtacct$(3%) = glacct$
                   call "GLFMT" (prtacct$(3%))
                   print using L25700, prtfulven$, prtfulvendesc$,        ~
                                      prtfulinvc$, apdate$, prtacct$(3%),~
                                      acctdescr$,debit$,credit$


                   return    : REM THIS GOES BACK TO MAIN PROGRAM SECTION

        REM *************************************************************~
            *    PAGE CONTROL SUBROUTINE FOR PRINTING SALES JOURNAL.    *~
            *************************************************************

L25300:     line% = line% + 1%
               if page% = 0% then L25350
                  if newinvcflag% <> 1% then L25329
                   call "CONVERT" (debits, 2.2, str(debit$,,10%))
                   call "CONVERT" (credits, 2.2, str(credit$,,10%))
                   print using L25780, totalinvc$, debit$, credit$
                   line% = line% + 1%  :  debits, credits = 0
                  /*NEW INVOICE SO LET THIS LINE PRINT VENDOR & INVC*/
                  prtfulven$ = vencode$
                  call "DESCRIBE" (#3, vencode$ , vendor$, 0%, f1%(3%))
                  prtfulvendesc$ = str(vendor$,1%,25%)
                  prtfulinvc$ = invoice$

L25329:       newinvcflag% = 0%
                   if line% < 55% then return
                   line% = 0%
                   print using L25740
L25350:            print page
              page% = page% + 1%
              call "DATE" addr("HD", date$)
              if postme$ <>"NO " then print using L20590,page%,reprtdet$, ~
                                 titlj$, date$                           ~
                                 else print using L20630,page%,reprtdet$, ~
                                 titlj$, date$
               print using L25810, jnlid$, pstseq%
               print using L25600
               print using L25630
               print using L25660
               print using L25740
                  /*NEW PAGE SO LET THIS LINE PRINT VENDOR & INVC*/
                  prtfulven$ = vencode$
                  call "DESCRIBE" (#3, vencode$ , vendor$, 0%, f1%(3%))
                  prtfulvendesc$ = str(vendor$,1%,25%)
                  prtfulinvc$ = invoice$

               if page% = 1% or line% = 0% then line% = 6%               ~
                                        else line% = line% + 4%
               newinvcflag% = 0%
               return

L25600: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-+
L25630: %! VENDOR  !  V E N D O R   N A M E  !  INVOICE       !   A/P  ! ~
        ~    G/L     !     D E S C R I P T I O N     !    A M O U N T S   ~
        ~ !
L25660: %!  CODE   !                         !   NUMBER       !  DATE  ! ~
        ~  ACCOUNT   !                               !   DEBIT  !  CREDIT ~
        ~ !

L25700: %!#########!#########################!################!########!#~
        ~############!###############################!##########!#########~
        ~#!

L25740: %+---------+-------------------------+----------------+--------+-~
        ~------------+-------------------------------+----------+---------~
        ~-+

L25780: %!         !    INVOICE    TOTAL     !    ############!        ! ~
        ~            !                               !##########!#########~
        ~#!
L25810: %JOURNAL: ###  POSTING SEQUENCE: -#########

L25840: %!         !     *** TOTALS ***      !    ############!        ! ~
        ~            !                               !##########!#########~
        ~#!

        REM *************************************************************~
            *        G L M A I N  P O S T I N G  R O U T I N E S        *~
            * POSTES THE INDICATED INFORMATION INTO THE DESIRED ACCOUNT *~
            *************************************************************
        def fn'151(account$, debitamnt, creditamnt )
             /* POST TO GLMAIN (TEXT$ FROM PAYJRNTF,EXCEPT ON SUM POST)*/
                call "GLPOST2"(account$, debitamnt, creditamnt, gldate$, ~
                            0%, "02", gltext$, jnlid$, pstseq%, userid$, ~
                            #15, #16, #2, 0%, invoice$, gl_post_info$())
                return

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

        def fn'161(account$, amount, loc% ) /*LOC%=1 DEBIT, 2=CREDIT*/
            REM MUST ALWAYS KEEP TRACK OF SUMMARY FOR PRINT
            if needsubtotals$ = "N" then L29700
                 /* DONT SUMMARIZE VENDOR&INVOICE IF NOT USED */
            search str(postacct$(),1%) = str(account$,1%,9%)             ~
                                                 to location$ step 9%
                if str(location$,1%,2%) = hex(0000) then L29650
                    junk% = int(val(location$,2%)/9%)+1%
                    postamnt(loc%,junk%) = postamnt(loc%,junk%) + amount
                    textarr$(loc%, point1%) = gltext$
                    goto L29700

L29650:     point1% = min(point1% + 1%, 250%)
                postacct$(point1%)= account$
                textarr$(loc%, point1%) = gltext$
                postamnt(loc%, point1%) = amount

L29700:     search str(sumacct$(),1%) = str(account$,1%,9%)              ~
                                                 to location$ step 9%
                if str(location$,1%,2%) = hex(0000) then L29760
                junk% = int(val(location$,2%)/9%)+1%
                sumamnt(loc%,junk%) = sumamnt(loc%,junk%) + amount
                return
L29760:
            point2% = min(point2% + 1%, 250%)
                sumacct$(point2%) = account$
                sumamnt(loc%,point2%)  = amount
                return

        REM ***GENERAL LEDGER TRANSACTION FILE READ SUBROUTINE*********

        load_payjrntf  /* READ from G/L transaction file           */
                       /* BALFLAG% -  1= DEBIT,   2 = CREDIT       */

          /* GLTEXT$ = "bbbbb " & GLTEXT$   phony batch number     */
            get   #23 using L30090, userid$, vencode$, invoice$, time$,   ~
                                   glacct$, balflag%, glamt, gltext$,    ~
                                   gldate$, pstseq%, newinvamt,          ~
                                   gl_post_info$()
L30090:         FMT  CH(3), CH(9), CH(16), CH(8), CH(9), BI(1),          ~
                     PD(14,4), CH(100), CH(6), BI(4), PD(14,4), XX(6),   ~
                     2*CH(255)
                apdate$ =  gldate$ & " "
                convert newinvamt to newinvamt$, pic(-########.##)

        REM NOW WE MUST DELETE RECORD SO AS NOT TO REPROCESS IT AGAIN
            call "DELETE"(#23, payjrntfkey$, 36%)
            return



        REM *************************************************************~
            *      D I S P L A Y   S T A T U S   S C R E E N            *~
            * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *~
            * Something better to look at than a normal message screen? *~
            *************************************************************

        status_screen

            display                                                      ~
               at (01,02), "PAYABLES TRANSACTION FILE PROCESSING",       ~
               at (02,02), "  FOR USER:",                                ~
               at (02,14),   userid$                            , ch(03),~
               at (06,02), "JOURNAL PRINTING DETAIL",                    ~
               at (06,30),   reprtdet$                          , ch(03),~
               at (07,02), "G/L LEVEL OF POSTING",                       ~
               at (07,30),   postme$                            , ch(03),~
               at (09,02), msg$                                 , ch(70)
           /* SIMULATED PAUSE   */
           for i% = 1% to 100%
           next i%
        return


        REM *************************************************************~
            *         P R I N T   D A I L Y   R E C A P   I N F O       *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.  SHOWS OFF JUST FOR FUN.  WILL NOT PRINT *~
            * ANYTHING WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY *~
            *************************************************************

        recap
            /* IF THERE WAS A "FUL" PRINT REQUESTED THEN ADD ENCLOSURE*/
            if reprtdet$ <> "FUL" then  L41120
                   call "CONVERT" (debits, 2.2, str(debit$,,10%))
                   call "CONVERT" (credits, 2.2, str(credit$,,10%))
                   print using L25780, newinvamt$, debit$, credit$
                   print using L25740
L41120:         mat locarr% = zer
                line% = line% + 2%
                gosub L25329
                convert totalinvc to totalinvc$, pic(-########.##)
                convert totaldebits to totaldebit$, pic(-######.##)
                convert totalcredits to totalcredit$, pic(-######.##)
                print using L25840, totalinvc$, totaldebit$, totalcredit$
                print using L25740
                if point2% < 1% then L65000

            REM SORT THE ACCOUNT ARRAY TO PRINT NICELY,RETAIN LOCATION
         /* CALL "SORT"ADDR(SUMACCT$(), POINT2%, 9%, LOCARR%(), 1%, 9%,  ~
                               "A", "L", 4%) */
            sort% = 2% : gosub L19000

        REM *************************************************************~
            *  Print a summation of the G/L activity for this RUN.      *~
            *************************************************************

            page%, line%  = 0%
                debits, credits = 0

            for y% = 1% to point2%
               x% = locarr%(y%)  /* SORTED ARRAY LOCATION FOR ACCOUNT*/
                   gosub L41600
               call "DESCRIBE" (#15,sumacct$(x%),acctdescr$,0%,f1%(15%))

               call "CONVERT" (sumamnt(1%, x%), 2.2, debit$)
                      if abs(sumamnt(1%,x%)) <.005 then init(" ") debit$
               call "CONVERT" (sumamnt(2%,x%), 2.2, credit$)
                      if abs(sumamnt(2%,x%)) <.005 then init(" ") credit$

               prtacct$(2%) = sumacct$(x%)
               call "GLFMT" (prtacct$(2%))
                   print using L20840, prtacct$(2%), acctdescr$, debit$,  ~
                                      prtacct$(2%), acctdescr$, credit$
                   debits = debits + sumamnt(1%,x%)
                   credits = credits + sumamnt(2%,x%)
            if postme$ <> "SUM"  then L41505
               gltext$ = str(gltext$,1%,6%) & " BATCH DEBIT  SUMMARY " & ~
         "TRANSACTION   " & time
                   gosub'151(sumacct$(x%), sumamnt(1%,x%), 0 )
               gltext$ = str(gltext$,1%,6%) & " BATCH CREDIT SUMMARY " & ~
         "TRANSACTION   " & time
                   gosub'151(sumacct$(x%), 0, sumamnt(2%,x%) )
L41505:     next y%
                   call "CONVERT" (debits, 2.2, debit$)
                   call "CONVERT" (credits, 2.2, credit$)
                   print using L20900
                   print using L20870, debit$, credit$
                   print using L20900
                   print
                   line% = line% + 4%
                   goto L65000

L41600: REM *************************************************************~
            *    PAGE CONTROL SUBROUTINE FOR PRINTING SALES JOURNAL.    *~
            *************************************************************

            line% = line% + 1%
               if page% = 0% then L41680
                   if line% < 55% then return
                   print using L20900
L41680:            print page
              page% = page% + 1%
              call "DATE" addr("HD", date$)
               print using L20590,page%,reprtdet$,titlj$,date$
               print using L20930
               print using L25810, jnlid$, pstseq%

               print using L20900
               print using L20770
               print using L20800
               print using L20900
               line% = 7%

               return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One moment please")

            call "JNLCLOSE" ("02", jnlid$, pstseq%, returncode%)
            payjrntfkey$ = str(userid$,,3%) & hex(000000)
            call "READ101" (#23, payjrntfkey$, f1%(23%))
                if f1%(23%) = 1% then delete #23
            call "SETPRNT" ("A/P010", " ", 0%, 1%)

            end
