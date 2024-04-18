        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP    AAA   Y   Y  H   H  N   N  Y   Y  RRRR   TTTTT   *~
            *  P   P  A   A  Y   Y  H   H  NN  N  Y   Y  R   R    T     *~
            *  PPPP   AAAAA   YYY   HHHHH  N N N   YYY   RRRR     T     *~
            *  P      A   A    Y    H   H  N  NN    Y    R   R    T     *~
            *  P      A   A    Y    H   H  N   N    Y    R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PAYHNYRT - SIMPLE REPORT OF ANY INVENTORY LEVEL CHANGES   *~
            *            FROM AN ACCOUNTS PAYABLE (PAYINPUT) TRANSACTION*~
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
            * 01/21/85 ! ORIGINAL                                 ! DSH *~
            * 12/13/85 ! Vendor file format change                ! MJB *~
            * 05/26/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            header$45,                   /* REPORT HEADER DATE, TIME   */~
            invoice$16,                  /* INVOICE NUMBER             */~
            lot$6,                       /* LOT                        */~
            partcode$25,                 /* PART CODE                  */~
            part$30,                     /* PART DESCRIPTIVE TEXT      */~
            plowkey$60,                  /* GENERAL PURPOSE PLOWKEY    */~
            quantity$10,                 /* FORMATTED QUANTITY         */~
            store$3,                     /* WAREHOUSE-STORE            */~
            vendcode$16,                 /* VENDOR CODE                */~
            vendor$30                    /* VENDOR NAME                */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard costs to 12            "
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
            * #1  ! PAYHNYRF ! Accounts Payable Inventory Report File   *~
            * #2  ! VENDOR   ! Vendor Master File                       *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "PAYHNYRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1,    keylen = 61

            select #2,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select #4,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#1,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#2,  "SHARE", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#4,  "SHARE", f2%(4 ), rslt$(4 ), axd$(4 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
                call "DATEFMT" (date$)

            call "EXTRACT" addr ("ID", str(plowkey$,,3))
                call "DATE" addr ("HD", header$)

            close ws
                select printer (132)
                call "SETPRNT" ("A/P005", " ", 0%, 0%)
                call "SHOSTAT" ("Printing A/P Inventory Receipts Log")


        REM *************************************************************~
            *   PICK UP EACH INVENTORY TRANSACTION RECORD AND PRINT IT  *~
            *      NOTE - "PAYUPDTE" ONLY WRITES TO THIS FILE AFTER A   *~
            *             CALL TO "HNYPOST".                            *~
            *************************************************************

            line% = 0
               page% = 0

L10090:     call "PLOWNXT1" (#1, plowkey$, 3%, f1%(1))
                if f1%(1) <> 1 then finished

            get #1, using L10140, vendcode$, invoice$, partcode$,         ~
                          quantity, store$, lot$, jnlid$, pstseq%, job$
L10140:         FMT XX(3), CH(9), CH(16), CH(25), XX(16), PD(14,4),      ~
                    CH(3), CH(6), CH(3), BI(4), CH(7)

            call "READ100" (#4, partcode$, f1%(4))
                if f1%(4) = 1 then L10210
                part$ = "** NON-STOCKED PART **"
                goto L10240
L10210:             get #4, using L10220, part$
L10220:                FMT XX(25), CH(30)

L10240:     call "READ100" (#2, vendcode$, f1%(2))
                if f1%(2) = 1 then L10280
                init(" ") vendor$
                goto L10310
L10280:             get #2, using L10290, vendor$
L10290:                FMT XX(39), CH(30)

L10310:     gosub control
                call "CONVERT" (quantity, 2.2, quantity$)
                print using L11250
                print using L11310, vendcode$, job$, invoice$, partcode$, ~
                                   store$, lot$, quantity$
                print using L11340, vendor$, part$
            line% = line% + 3
                delete #1
                goto L10090

        finished

            if page% <> 0 then print using L11250
                close printer
                goto L65000

        REM *************************************************************~
            *       P A G E   C O N T R O L   A N D   F O R M A T S     *~
            *************************************************************

        control

            if page% = 0 then L11100
                if line% < 60 then return
                    print using L11250

L11100:         page% = page% + 1
                    print page
                    print using L11190, str(plowkey$,,3), page%
                    print using L11220, header$

                    print using L11370, jnlid$, pstseq%
                    print using L11250
                    print using L11271
                    print using L11280

                line% = 7%
                    return

L11190: % POSTING ID:  ###            A C C O U N T S    P A Y A B L E   ~
        ~I N V E N T O R Y    R E C E I P T S   L O G          PAGE ###

L11220: %   A/P005                                                       ~
        ~                 #############################################

L11250: %+---------------------------------------------------------------~
        ~-------------------------------------------------------------+

L11271: %! VENDOR CODE                 JOB/PROJ !  INVOICE NUMBER  ! PART~
        ~ CODE                          ! STORE !   LOT  !  QUANTITY  !

L11280: %!      VENDOR NAME                     !                  !     ~
        ~PART DESCRIPTION               !       !        !            !

L11310: %! #########                   ######## ! ################ ! ####~
        ~#####################          !  ###  ! ###### ! ########## !

L11340: %!      ##############################  !                  !     ~
        ~############################## !       !        !            !

L11370: %JOURNAL: ###  POSTING SEQUENCE: -#########

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

            call "SETPRNT" ("A/P005", " ", 0%, 1%)

            end
