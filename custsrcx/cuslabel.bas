        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   CCC   U   U   SSS   L       AAA   BBBB   EEEEE  L       *~
            *  C   C  U   U  S      L      A   A  B   B  E      L       *~
            *  C      U   U   SSS   L      AAAAA  BBBB   EEEE   L       *~
            *  C   C  U   U      S  L      A   A  B   B  E      L       *~
            *   CCC    UUU    SSS   LLLLL  A   A  BBBB   EEEEE  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUSLABEL - CREATES MAILING LABELS FOR SHIP-TO CUSTOMERS.  *~
            *            SELECT ON CUSTOMER TYPE, CUSTOMER CODE, ZIP    *~
            *            CODE, AND REGION.   SORT BY CUSTOMER CODE,     *~
            *            ZIP CODE, OR REGION.                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/03/83 ! ORIGINAL                                 ! ECR *~
            * 06/19/86 ! Modified per changes in customer file    ! ERN *~
            *          !  (embarassed to be here, too!)           !     *~
            * 02/21/89 ! Formatted ZIP Code as 12345-xxxx         ! MJB *~
            * 09/07/89 ! Minor tweek of ZIP format.               ! JDH *~
            * 10/07/91 ! MOD FOR APC PRINT - KEY LINE 30100       ! RHH *~
            *          !   SET FOR ONE ACROSS                     ! RHH *~
            * 03/19/92 ! MOD SKIP CUSTOMERS THAT ARE NOT 'A' OR   ! RHH *~
            *          !   'H'. LINE 11231                        ! RHH *~
            * 04/06/95 ! Minor Mods to Correct the Zip Code Sort  ! RHH *~
            *          !   and Selection Criteria. Added Sort     ! RHH *~
            *          !   Description.                           ! RHH *~
	    *          !            			          !     *~
	    * 11/25/97 ! Updated for 60403 Code Changes           ! DJD *~
            * 04/06/98 ! Y2K Compliance                           ! DJD *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        com                                                              ~
            extlen%(15),                 /* EXTERNAL FIELD LENGTHS     */~
            field%(15),                  /* SELECTABLE FIELD LIST      */~
            format$(15)1,                /* DATA TYPE FORMAT CODES     */~
            from$(15)25,                 /* LOW RANGE DATA TEST ITEMS  */~
            fromnr(15),                  /* LOW RANGE NUMERIC TEST ITEM*/~
            length%(15),                 /* INTERNAL FIELD LENGTHS     */~
            position%(15),               /* POSITION IN REC (FROM 1)   */~
            prompt$(15)25,               /* FIELD NAME PROMPT          */~
            record$(15)250,              /* 3 RECORDS * 1200 CHARS EA. */~
            record%(15),                 /* WHICH OF 3 RECORDS IT'S IN */~
            to$(15)25,                   /* HI VALUE RANGE DATA TEST   */~
            tonr(15)                     /* HI RANGE NUMERIC RANGE TEST*/~


        dim                                                              ~
            status$1, sort_msg$40,       /* CUSTOMER STATUS            */~
            a$(6)30,                     /* Address for samshing       */~
            b$31,                        /* Address work variable      */~
            cusbilladdress$(4,5)30,      /* CUSTOMER SHIP-TO ADDRESS   */~
            cusname$(4)30,               /* CUSTOMER NAME              */~
            cusprt$(4)30,                /* CUSTOMER TYPE - CODE - RGN */~
            date$8,                      /* TODAY'S DATE               */~
            errormsg$79,                 /* ERROR MESSAGE FOR DATA TEST*/~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            instr$18,                    /* Pgm Name & Rev #           */~
            keys$(11)50,                 /* ACTIVE KEYS IN MAIN SCREEN */~
            mode$10,                     /* FLAG FOR TEST    OR PRINT  */~
            prtfrom$(5)25,               /* RANGE FORMATTED FOR PRINT  */~
            prtto$(5)25,                 /* RANGE FORMATTED FOR PRINT  */~
            readkey$50,                  /* KEY TO FIND DATA WITH      */~
            sort$1,                      /* SORT OPTION FLAG           */~
            sortkey$100                  /* KEY FOR SORTING WORK FILE  */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 11/25/97 CMS2 / CMS-I Merge              "
        REM *************************************************************

            mat f2% = con
                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 3 ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #10 ! SORTWORK ! WORK FILE TO SORT WITH                   *~
            *************************************************************

            select #3,  "CUSTOMER"                                       ~
                        varc, indexed, recsize = 1200,                   ~
                        keypos=1, keylen=9,                              ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #10, "SORTWORK"                                       ~
                        varc, indexed, recsize = 220,                    ~
                        keypos = 1, keylen = 20

            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))

        REM *************************************************************~
            *         L O C A T I O N    O F    R O U T I N E S         *~
            *                                                           *~
            * DESPITE THE SIZE OF THIS PROGRAM, IT IS MERELY A COLLECT- *~
            * ION OF SMALL ROUTINES.  UNDERSTANDING THESE ROUTINES IS   *~
            * THE KEY TO UNDERSTANDING THIS LARGE BUT SIMPLE PROGRAM.   *~
            *                                                           *~
            * MAIN_SCREEN                                    10000      *~
            * SELECT_RECORDS                                 11000      *~
            * SORT_REGISTER              APC MOD - 04/06/95  12000 GONE *~
            *                                                           *~
            * PRINT_LINEUP_TEST          APC MOD - 10/04/91  18000      *~
            *                                                           *~
            * PRINT_LABELS               APC MOD - 10/04/91  24000      *~
            *                                                           *~
            * CONTROL_PRINT_LABELS       APC MOD - 04/06/95  28000 gone *~
            *                                                           *~
            * READ_NEXT_LABELS                               30000      *~
            *                                                           *~
            * SETUP_SUPER_SELECTOR                           34000      *~
            * RESET_SUPER_SELECTOR                           34000      *~
            *                                                           *~
            *                                                           *~
            * SUPER_SELECTOR_SCREEN                          40000      *~
            * TEST_OR_PRINT_SCREEN                           41000      *~
            *                                                           *~
            * TEST_SUPER_SELECTOR_RANGE                      50000      *~
            *                                                           *~
            * EXIT_PROGRAM                                   65000      *~
            *************************************************************

        REM *************************************************************~
            *    I N I T I A L I Z A T I O N   F O R   P R O G R A M    *~
            *                                                           *~
            * INITIALIZES USER ID AND A FEW OTHER THINGS THAT WE NEED.  *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

                keys$(1)  = "(1)        START OVER                     "
                keys$(2)  = "                                          "
                keys$(3)  = "(5)        PRINT LINE UP TEST             "
                keys$(4)  = "                                          "
                keys$(5)  = "(9)        PRINT LABELS                   "
                keys$(6)  = "                                          "
                keys$(7)  = "(15)       PRINT SCREEN                   "
                keys$(8)  = "(16)       EXIT PROGRAM                   "

            errormsg$ = " "

            instr$ = "CUSLABEL: " & str(cms2v$,1,8)

        REM *************************************************************~
            *                  M A I N    S C R E E N                   *~
            * --------------------------------------------------------- *~
            * GETS THE RANGE OF CUSTOMER CODES, TYPES, ZIP CODES,       *~
            * AND REGION CODES FOR CUSTOMER LABELS TO BE PRINTED.       *~
            *                    ALSO PICKS UP SORT FIELD BY TRICKING   *~
            * THE *** SUPER SELECTOR *** A LITTLE.                      *~
            *************************************************************

        main_screen

               gosub setup_super_selector
L10120:        prompt$(5) = "   SORT BY (1 - 4)"   /* RESET SORT PROMPT */

               gosub super_selector_screen
               prompt$(5) = " "                    /* TRICK SUPER SLCTR */
                     if keyhit%  =  1 then main_screen
                     if keyhit%  = 16 then exit_program
                     if keyhit% <>  0 then L10120

               gosub test_super_selector_range
                     if errormsg$ <> " " then L10120
               if sort% = 1% then                                        ~
                  sort_msg$ = "*** Sort Customers by Customer Code  ***"
               if sort% = 2% then                                        ~
                  sort_msg$ = "* Sort Customers by Customer Type Code *"
               if sort% = 3% then                                        ~
                  sort_msg$ = "****** Sort Customers by Zip Code ******"
               if sort% = 4% then                                        ~
                  sort_msg$ = "***** Sort Customers by Region Code ****"
L10300:        gosub test_or_print_screen
                     if keyhit%  =  1 then L10120
                     if keyhit%  =  5 then mode$ = "TEST"
                     if keyhit%  =  9 then mode$ = "PRINT"
                     if keyhit%  = 16 then exit_program

               if mode$ <> "TEST"    then L10392
                  gosub print_lineup_test
                  goto L10300

L10392:        call "WORKOPN2" (#10, "OUTPT", 500%, f2%(10))
               call "WORKOPN2" (#10, "SHARE", 500%, f2%(10))

               gosub select_records

               if mode$ = "PRINT"   then gosub print_labels

               call "FILEBGON" (#10)

               goto main_screen

        REM *************************************************************~
            *            S E L E C T   R E C O R D S                    *~
            *                                                           *~
            * SELECTS THE CUSTOMERS THE USER WISHES TO PRINT ON LABLES. *~
            *                               THEN THE SORT FILE IS SET   *~
            * UP ACCORDING TTO THE USER'S SELECTED SORT OPTION.         *~
            *************************************************************

        select_records:
            REM OPEN THE WORK FILE FOR SORTING

                call "SHOSTAT" ("Selecting CUSTOMER Records...One Moment ~
        ~Please")
                readkey$ = " "

L11180:         call "PLOWNEXT" (#3, readkey$, 0%, f1%(3))
                     if f1%(3) = 0 then return
                call "MOVERWA" addr(#3, str(record$(), 1))

                call "SLCTPASS" (maxfields%, select%)
                if select% = 0 then L11180
                                            /* SPECIAL APC MOD 3/19/92 */
                status$ = str(record$(),793%,1%)
                if status$ <> "A" and status$ <> "H" then goto L11180

        REM Now Process Sort Option On A Selected Record
            sortkey$ = all(" ")
            on sort% gosub L11490, L11540, L11590, L11640
                cusprt$(1%) = str(record$(), 1023, 2) &  " "  &          ~
                              str(record$(),    1, 9) &  " "  &          ~
                              str(record$(),  729, 4)
                str(a$()) = str(record$(), 253, 180)     /* Ship-to    */
                if str(a$(6%),17,1) <> " " or str(a$(6%),16,1) <> " " or ~
                    pos(str(a$(6%),27,4) = " ") > 0% then L11430
                        b$ = str(a$(6%),,26)
                        str(b$,28,4) = str(a$(6%),27,4)
                        str(b$,27,1) = "-"
                    call "SPCESMSH" (b$, 2%)
                    a$(6%) = b$
                    goto L11440
L11430:         call "SPCESMSH" (a$(6%), 2%)
L11440:         call "LINSMASH" (a$())
                write #10, using L11460, sortkey$, str(a$()), cusprt$(1%)
L11460:                    FMT CH(20), CH(180), CH(20)
                goto L11180

L11490: REM  Sort Customer Code By Customer Type
            str(sortkey$,  1,  9) = str(record$(),   1, 9)
            str(sortkey$, 10,  2) = str(record$(),1023, 2)
        return

L11540: REM  Sort Customer Type By Customer Code
            str(sortkey$,  1,  2) = str(record$(),1023, 2)
            str(sortkey$,  3,  9) = str(record$(),   1, 9)
        return

L11590: REM  Sort Zip Code By Customer Code
            str(sortkey$,  1, 10) = str(record$(), 424, 9)
            str(sortkey$, 11,  9) = str(record$(),   1, 9)
        return

L11640: REM  Sort Region Code By Customer Code
            str(sortkey$,  1,  4) = str(record$(), 729, 4)
            str(sortkey$,  5,  9) = str(record$(),   1, 9)
        return

        REM *************************************************************~
            *                S O R T    R E G I S T E R                 *~
            *                                                           *~
            * THIS CONTROLS THE SORTING OF THE WORK FILE CONTAINING THE *~
            * SELECTED CHECK REGISTER ENTRIES.                          *~
            *************************************************************

        REM *************************************************************~
            *       P R I N T   L I N E U P   T E S T                   *~
            *                                                           *~
            * PRINTS A LINEUP TEST FOR CUSTOMER LABELS.                 *~
            *                                                           *~
            *************************************************************

        print_lineup_test
            select printer(134)
            labelcnt% = 0%                       /* APC MOD - 10/04/91 */

            for row% = 1% to 3%
                for l% = 1% to 5%                /* APC MOD - 10/04/91 */
                    print using L28120
                next l%
                print skip (1)
            next row%

            close printer
            return

        REM *************************************************************~
            *              P R I N T    L A B E L S                     *~
            *                                                           *~
            * THIS CONTROLS THE PLOWING FOR PRINT OF LABELS.            *~
            *************************************************************

        print_labels:
            REM SETUP FOR PRINTING REGISTER
                select printer (134)

                call "SETPRNT" ("CUS004", " ", 0%, 0%)
                labelcnt% = 0%                   /* APC MOD - 10/04/91 */
                errormsg$ = " "
                call "SHOSTAT" ("Printing Customer Labels")
            sortkey$ = " "
            read #10,key > sortkey$, using L24160, sortkey$,              ~
                                               eod goto exit_print_labels
L24160:         FMT CH(20)
            goto L24210

            REM GET NEXT CHECK AND PRINT IT
L24200:     read #10,eod goto exit_print_labels
L24210:         gosub get_label_data     /* LOAD DATA FROM SORT FILE   */
                                                 /* APC MOD - 10/04/91 */
                   REM PRINT LINE ......................................
                         labelcnt% = labelcnt% + 1%
                         print using L28080, cusname$(1%)

                         print using L28080, cusbilladdress$(1%, 1%)

                         print using L28080, cusbilladdress$(1%, 2%)

                         print using L28080, cusbilladdress$(1%, 3%)

                         print using L28080, cusbilladdress$(1%, 4%)

                         print skip (1)
                goto L24200

        exit_print_labels:
             if labelcnt% = 0% then goto L24430
             print page                           /* APC MOD - 10/04/91 */
             close printer
             call "SETPRNT" ("CUS004", " ", 0%, 1%)
        return
L24430:       errormsg$ = "No Labels Were Selected Based On The Choices Y~
        ~ou Made"
        return

        REM *************************************************************~
            *     H A R D C O P Y    P A G E    C O N T R O L L E R     *~
            *                                                           *~
            * HANDLES PAGING AND HEADINGS WHEN WE ARE PRINTING THE      *~
            * REGISTER AND INDIVIDUAL CHECK INFORMATION.                *~
            *************************************************************


L28080: %##############################



L28120: %XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************


        REM *************************************************************~
            *    L O A D    &    F O R M A T    A D D R E S S E S       *~
            *-----------------------------------------------------------*~
            * Loads Customer Names & Addresses three at a time          *~
            *************************************************************

        get_label_data
            init(" ") cusprt$(),cusname$(),cusbilladdress$()
            l% = 1%                                /* APC MOD 10/07/91 */
            get #10,using L30170, cusname$(l%), cusbilladdress$(l%,1%),   ~
                          cusbilladdress$(l%,2%), cusbilladdress$(l%,3%),~
                          cusbilladdress$(l%,4%), cusbilladdress$(l%,5%),~
                          cusprt$(l%)
            call "SPCESMSH" (cusprt$(l%), 2%)
            call "RJUSTIFY" (cusprt$(l%))
        return

L30170:   FMT XX(20), CH(30), 5*CH(30), CH(17)

        REM *************************************************************~
            *      S E T   U P   S U P E R   S E L E C T O R            *~
            *                                                           *~
            * INITIALIZES ALL THE VARIABLES NEEDED TO DO THE REPORT.    *~
            *                                                           *~
            * HERE IS HOW THE PARAMETERS FOR THE SELECT (ALSO SORT) KEYS*~
            * WORK.  THE FIRST OF EACH DATA ITEM IS THE NAME OF THE KEY.*~
            * IT IS THE PROMPT THAT APPEARS ON THE SCREEN.              *~
            *      THE FIRST ITEM TO THE RIGHT OF THE PROMPT IS THE     *~
            * DATA TYPE.  THE DATA TYPE IS ONE OF THE FOLLOWING.        *~
            *                                                           *~
            *      U = UPPER CASE ALPHANUMERIC.                         *~
            *      L = UPPER/LOWER CASE ALPHANUMERIC.                   *~
            *      D = DATE-FORMATTED UPPER CASE FIELD.  MUST BE YYMMDD *~
            *      S = SINGLE PARAMTER FIELD (NO RANGE PROCESSING)      *~
            *      N = NUMERIC 8-BYTE FLOATING PT. ALL OTHER NUMERICS   *~
            *          SHOULD BE COMPARED ASCII, OR MODIFY THE ROUTINE. *~
            *                                                           *~
            * THE REMAINING NUMBERS IDENTIFY RESPECTIVELY THE LENGTH OF *~
            * THE FIELD ON THE DISK, THE NUMBER OF POSITIONS IT FILLS   *~
            * ON THE PAPER, WHICH OF THE UP-TO-3 RECORDS IN CORE THIS   *~
            * FIELD LIES IN, AND THE POSITION OF THE FIELD WITHIN THE   *~
            * RECORD.                                                   *~
            *                                                           *~
            * THE FIRST BLANK PROMPT NAME SIGNIFIES THE END OF THE      *~
            * LIST OF PROMPTS ON THE SYSTEM.                            *~
            *************************************************************

        setup_super_selector:

            REM SET UP DATA FOR SELECT INTERPRETATION.
                restore
                for temp% = 1 to 15
                    read prompt$(temp%), format$(temp%), length%(temp%), ~
                         extlen%(temp%), record%(temp%), position%(temp%)
                    next temp%

*                         Prompt                 Fmt Len Xln  R  Pos
                data "1. CUSTOMER CODE         ", "U",  9,  9, 1, 001,   ~
                     "2. CUSTOMER TYPE         ", "U",  2,  3, 1,1023,   ~
                     "3. ZIP CODE              ", "U",  9,  9, 1, 424,   ~
                     "4. REGION CODE           ", "U",  4,  4, 1, 729,   ~
                     "   SORT BY (1 - 4)       ", "S",  1,  1, 3, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001

            init(" ")  from$(), to$()
            mat fromnr = zer: mat tonr = zer: mat field% = zer

            for temp% = 1 to 15
                if prompt$(temp%) = " " then L34590
                   from$(temp%) = "ALL"
L34590:         next temp%
            from$(5) = "1"

            inpmessage$ = "Select Range Of Customers And Sorting Option..~
        ~.Then Press (ENTER)"

            return

        REM *************************************************************~
            *          D I S P L A Y   M A I N    S C R E E N           *~
            *                                                           *~
            * GETS A RANGE OF LABELS TO BE PRINTED           USING      *~
            * THE *** SUPER SELECTOR *** ROUTINES.                      *~
            *************************************************************

        super_selector_screen:
            call "SLCTSCRN" ("CUSTOMER LABEL PROCESSING",                ~
                              errormsg$, inpmessage$, keyhit%, instr$)
            return


        REM *************************************************************~
            *   D I S P L A Y   O R   P R I N T   S C R E E N           *~
            *                                                           *~
            * DISPLAY CHOICE OF DISPLAYING OR PRINTING THE CHK REGISTER *~
            *************************************************************

        test_or_print_screen:

            accept                                                       ~
               at (01,02), "CUSTOMER LABEL PROCESSING",                  ~
               at (01,75), "PAGE 2",                                     ~
               at (02,02), "DATE:",                                      ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (05,21), fac(hex(84)), sort_msg$              , ch(40),~
                                                                         ~
               at (09,07),                                               ~
                  "Select Function When Labels Are Mounted On Printer",  ~
               at (10,12),                                               ~
                  "P.F. KEY      ------------FUNCTION-----------",       ~
               at (11,15), fac(hex(8c)), keys$(1)               , ch(50),~
               at (12,15), fac(hex(8c)), keys$(2)               , ch(50),~
               at (13,15), fac(hex(8c)), keys$(3)               , ch(50),~
               at (14,15), fac(hex(8c)), keys$(4)               , ch(50),~
               at (15,15), fac(hex(8c)), keys$(5)               , ch(50),~
               at (16,15), fac(hex(8c)), keys$(6)               , ch(50),~
               at (17,15), fac(hex(8c)), keys$(7)               , ch(50),~
               at (18,15), fac(hex(8c)), keys$(8)               , ch(50),~
                                                                         ~
               keys(hex(0105090f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto test_or_print_screen

        REM *************************************************************~
            *                      T E S T   D A T A                    *~
            *                                                           *~
            * BE SURE THAT THE *** SUPER SELECTOR *** RANGES ARE OK     *~
            *************************************************************

        test_super_selector_range
            errormsg$ = " "

            call "SLCTTEST" (errormsg$,  maxfields%)
                  if errormsg$ <> " " then return

            REM TEST SORT OPTION
                sort$ = from$(5)
                convert sort$ to sort%, data goto L50440
                if sort% >= 1% and sort% <= 4% then L50470
L50440:            errormsg$ = "SORT OPTION Must Be 1,2,3 Or 4 !!"
                   return

L50470:     REM NOW FORMAT THE RANGES AGAIN (!!!) FOR PRINT OUT
                for temp% = 1% to 4%
                   prtfrom$(temp%) = from$(temp%)
                   prtto$  (temp%) = to$  (temp%)
                   if str(from$(temp%), 1, 5) = hex(0000000000) then     ~
                      prtfrom$(1) = "FIRST"
                   if str(to$(temp%)  , 1, 5) = hex(ffffffffff) then     ~
                      prtto$(1)   = "LAST"
                next temp%

                prtfrom$(5) = from$(5)
                prtto$  (5) = to$  (5)
                return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program:
            call "SHOSTAT" ("ONE MOMENT PLEASE")
        end

