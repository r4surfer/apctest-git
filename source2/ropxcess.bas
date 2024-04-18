        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR    OOO   PPPP   X   X   CCC   EEEEE   SSS    SSS    *~
            *  R   R  O   O  P   P   X X   C   C  E      S      S       *~
            *  RRRR   O   O  PPPP     X    C      EEEE    SSS    SSS    *~
            *  R   R  O   O  P       X X   C   C  E          S      S   *~
            *  R   R   OOO   P      X   X   CCC   EEEEE   SSS    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ROPXCESS - This program prints a report of all items whose*~
            *            on hand position is in excess of the ROP by a  *~
            *            percentage >= to the excess percentage defined *~
            *            in the system level parameters.                *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/12/87 ! Original                                 ! LKM *~
            * 05/09/89 ! Merge CMS2/CMSI                          ! MJB *~
            *          !  - Added file ROPCLASS for class verify  !     *~
            *          !  - Added ASKUSER when NO records printed !     *~
            *          !  - Changed Search Logic to drive from    !     *~
            *          !     ROPHNY instead of HNYMASTR.          !     *~
            * 04/24/91 !(PRR 11839) Corrected branching error for ! RJB *~
            *          !     FORMULA$ selection in SEARCH FILE    !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cat$4,                       /* Part Category              */~
            class$4,                     /* Part Class                 */~
            company$50,                  /* For Report Header          */~
            date$8,                      /* Date for screen display    */~
            eoq$10,                      /* Economic Order Quantity    */~
            oh$10,                       /* On Hand Quantity           */~
            part$25,                     /* Part number                */~
            part_descr$32,               /* Part Description           */~
            planflags$(25)20,            /* Planning System Flags      */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            quan(1),                     /* For call to HNYTOTSB       */~
            rop$10,                      /* Re-order Point             */~
            stdcst$10,                   /* Standard Cost              */~
            svcat$4,                     /* Saved Part Category        */~
            svclass$4,                   /* Saved Part Class           */~
            svpart_descr$32,             /* Saved Part Description     */~
            svtype$3,                    /* Saved Part Type            */~
            totcst$10,                   /* Total extended cost        */~
            totpart$6,                   /* Total Parts Printed        */~
            type$3,                      /* Part Type                  */~
            userid$3,                    /* Current User Id            */~
            xcess$10                     /* Excess Amount              */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            * # 3 ! ROPHNY   ! File containing part specific ROP data   *~
            * # 4 ! PIPMASTR ! PIP Master file                          *~
            * # 5 ! SFCUM2   ! Cumulative Sales Forcast File            *~
            * # 6 ! WORK1    ! System Work File                         *~
            * # 7 ! ROPCLASS ! ROP Class Codes File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 2, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 3, "ROPHNY",                                        ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  104, keylen = 4, dup

            select # 4, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select # 5, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25

            select #6,  "WORK1",                                         ~
                        varc,     indexed,  recsize = 116,               ~
                        keypos = 1,    keylen = 68                       ~

            select #7, "ROPCLASS",                                       ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos = 1,    keylen =  4

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "WORKOPEN" (#6%, "IO", 1000%, f2%(6))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            store$, lot$ = " "
            u3% = 0

            call "READ100" (#1, "PLANNING SYSTEM FLAG", f1%(1))
            if f1%(1) = 0 then exit_program
            get #1, using L09170, str(planflags$(),1,480)
L09170:         FMT XX(20), CH(480)
            call "PIPINDEX" (#1, " ", req%, ret%)
            if ret% <> 0 then L65000
            today% = req%

        REM *************************************************************~
            *                   M A I N   L O G I C                     *~
            *-----------------------------------------------------------*~
            * Reads files and gathers information for report.           *~
            *************************************************************

L10051:     kh% = 2
            call "ASKUSER" (kh%,"PROCEED?","Press ENTER to print ROP " & ~
                 "Excess Report", "- or -", "Press PF16 to Exit Program.")
            if kh% = 16 then L65000
            if kh% <> 0 then L10051

            totpart, page% = 0
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            select printer(134)
            call "SETPRNT" ("ROP003", " ", 0%, 0%)
            call "SHOSTAT" ("Gathering Report Data")
            gosub search_file
            call "SHOSTAT" ("Printing Report")
            init (hex(00)) plowkey$
            gosub print_report
            if page% <> 0% then L10145
L10134:     ask% = 2%
            call "ASKUSER" (ask%, "***** NO REPORT *****", " There are" &~
                           " NO Parts That are Currently in an",         ~
                           "EXCESS INVENTORY POSITION", "Press PF-16 " & ~
                           "to EXIT Program")
            if ask% = 16% then L10200 else goto L10134
L10145:     print skip(1)
            call "CONVERT" (totpart, 0.0, totpart$)
            print using L13352, totpart$
            print
            print "** END REPORT **"
            call "SETPRNT" ("ROP003", " ", 0%, 1%)
L10200:     close printer
            call "FILEBGON" (#6)
            goto exit_program

        search_file
            call "READNEXT" (#3, f1%(3))
                if f1%(3) <> 1 then return
            get #3 using L11040, part$, rop, eoq, class$
L11040:         FMT CH(25), 2*PD(14,4), POS(104), CH(4)
            call "READ100" (#2, part$, f1%(2))
            get #2 using L11100, part$, part_descr$, cat$, type$
L11100:         FMT CH(25), CH(32), POS(90), CH(40), POS(180), CH(3)
            call "STCCOSTS" (part$, " ", #1, 1%, stdcst)
            call "HNYTOTSB" (part$, store$, lot$, quan(), 0%)
            oh = quan(1)
            call "READ100" (#7, class$, f1%(7))
            get #7 using L11160, formula$
L11160:         FMT POS(37), CH(1)
            xcess = oh
            if formula$ > "1" then L11210
                xcess = oh - rop - eoq
                goto L11750
L11210:     if formula$ > "2" then L11750
                call "PLANALLC" (part$, 9e7, qtyall, "1", "A", req%,     ~
                                 today%, planflags$(), #4, #5, ret%)
                if ret% <> 0 then search_file
                xcess = qtyall

L11750:     if xcess <= 0 then search_file
               totcst = stdcst * xcess
               write #6 using L11875, type$, cat$, class$, part_descr$,   ~
                                     part$, rop, eoq, oh, xcess, stdcst, ~
                                     totcst
L11875:        FMT CH(3), 2*CH(4), CH(32), CH(25), 6*PD(14,4)
               goto search_file

        print_report
            call "PLOWNEXT" (#6, plowkey$, 0%, f1%(6))
            if f1%(6) <> 1 then return
            get #6 using L11875, type$, cat$, class$, part_descr$, part$, ~
                                rop, eoq, oh, xcess, stdcst, totcst
            gosub print_record
            goto print_report

        print_record
            if page% = 0 or line% > 54 then gosub print_hdr
            call "CONVERT" (rop, 2.2, rop$)
            call "CONVERT" (eoq, 2.2, eoq$)
            call "CONVERT" (oh, 2.2, oh$)
            call "CONVERT" (xcess, 2.2, xcess$)
            call "CONVERT" (stdcst, 2.2, stdcst$)
            call "CONVERT" (totcst, 2.2, totcst$)
            if svtype$ = type$ then type$ = " " else svtype$ = type$
            if svcat$  = cat$  then cat$  = " " else svcat$  = cat$
            if svclass$=class$ then class$= " " else svclass$= class$
            if svpart_descr$ = part_descr$ then L13091     else           ~
               svpart_descr$ = part_descr$
            print using L13340, type$, cat$, class$, part_descr$, rop$,   ~
                               eoq$, oh$, xcess$, stdcst$, totcst$
L13091:     print using L13351, part$
            print
            totpart = totpart + 1
            line% = line% + 3
            return

        print_hdr
            page% = page% + 1
            print page
            print using L13260, date$, runtime$, company$
            print using L13280, page%
            print
            print using L13300
            print using L13320
            print
            line% = 6
            return

L13260: %RUN DATE: ######## ########             ########################~
        ~####################################              ROPXCESS:ROP003
L13280: %                                                    RE-ORDER POI~
        ~NT EXCESS REPORT                                      PAGE: ##
L13300: %TYPE CAT  CLASS PART DESCRIPTION/CODE                   ROP     ~
        ~   EOQ    ON HAND     EXCESS   STD COST      TOTAL
L13320: %---- ---- ----- -------------------------------- ---------- ----~
        ~------ ---------- ---------- ---------- ----------
L13340: %###  #### ####  ################################ ########## ####~
        ~###### ########## ########## ########## ##########
L13351: %                #########################
L13352: %TOTAL PARTS = ######

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
