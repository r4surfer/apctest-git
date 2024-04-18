        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  IIIII  N   N  U   U   SSS   EEEEE   *~
            *  H   H  NN  N   Y Y     I    NN  N  U   U  S      E       *~
            *  HHHHH  N N N    Y      I    N N N  U   U   SSS   EEEE    *~
            *  H   H  N  NN    Y      I    N  NN  U   U      S  E       *~
            *  H   H  N   N    Y    IIIII  N   N   UUU    SSS   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYINUSE - Checks to see if passed in Part Code is used   *~
            *            in any manner which could prevent part deletion*~
            *            from occurring.  Areas where used (if any) are *~
            *            passed back to the caller as text.             *~
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
            * 02/02/84 ! ORIGINAL                                 ! ERN *~
            * 02/07/86 ! Renamed from PRTNUSE to HNYINUSE, added  ! LDJ *~
            *          !   check for part in Physical Inventory.  !     *~
            * 09/25/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 10/04/86 ! Added check to Sales Analyis Files       ! ERN *~
            * 03/02/87 ! Serial Number checking                   ! JRH *~
            * 06/09/89 ! Added Check for Option Selection         ! MJB *~
            * 05/30/90 ! Added Check for Auto Replacement, did    ! JDH *~
            *          !  away with STDDETAL code; it's obsolete. !     *~
            * 05/24/91 ! PRR 11963 REM'd 'extraneous' SHOSTATs.   ! JIM *~
            * 12/01/92 ! Check Core Xref File.                    ! KAB *~
            * 03/08/94 ! Removed AutoReplace logic - its obsolete ! WPH *~
            * 10/26/98 ! (EWD001) Add logic for auto-delete.      ! BWS *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYINUSE" (part$, inuse$, use$(), #1, #2, #3, #5, #6, #7,   ~
                #8, sakey%, #9, #10, #12)

        REM     PART$ = PART TO CHECK (NOT CHANGED)                      ~
                INUSE$= RETURN STATUS- "YES" = IN USE                    ~
                                       "NO " = NOT IN USE                ~
                USE$  = (16)75  TABLE WITH DESCRIPTIONS OF WHERE IN USE  ~
                SEE SELECT TABLE FOR FILES

        dim                                                              ~
            f1%(14),                     /* = 1 IF READ WAS SUCCESSFUL */~
            demand$16,                   /* Demand Number for Option   */~
            oppart$25,                   /* Option Part                */~
            key$50,                      /* MISC KEY USEAGES           */~
            status$1,                    /* SERMASTR STATUS CODE       */~
/*EWD001*/  auto$1,                      /* Auto-pilot or manual drive?*/~
            use$(16)75                   /* IN USE MESSAGES TABLE      */

        EJECT
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #2  ! BOMMASTR ! BOM relationship file                    *~
            * #3  ! RTEMASTR ! Production routing master file           *~
            * #5  ! PIPIN    ! Planned inventory additions detail       *~
            * #6  ! PIPOUT   ! Planned inventory use detail rec         *~
            * #7  ! HNYPITKT ! Physical Inventory Ticket File           *~
            * #8  ! SASUMRY# ! Sales Analysis Summary File              *~
            * #9  ! SERMASTR ! Serial Number Master File                *~
            * #10 ! BOMSPEC  ! Option Selections File                   *~
            * #12 ! COREXREF ! Core Cross Reference                     *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
/*EWD001*/  if use$(1%) = "*AUTO*" then auto$ = "Y" else auto$ = "N"
            init (" ") use$()  /* IN USE ABUSE DESCRIPTIONS            */
            inuse$ = "NO"      /* "YES" IF IN USE, "NO" IF NOT         */
            use% = 0%          /* INDEX TO USE$()- # OF LINES          */

        EJECT
        REM *************************************************************~
            *   M A I N   P R O G R A M   L O G I C                     *~
            *                                                           *~
            *  PROGRAM LOGIC CONTROL.  SPECIALLY WRITTEN FOR ALL YOU    *~
            *  STRUCTURED PROGRAMMING FREAKS.                           *~
            *************************************************************

            gosub hnyquan      /* NO INVENTORY OR IN-PROCESS           */

            gosub bommastr     /* ALL BOMS MUST BE DELETED FIRST       */
            gosub rtemastr     /* NO ROUTES ALLOWED EITHER             */
            gosub pipin        /* PIPIN & PIPOUT COVERS ORDERS, JOBS,  */
            gosub pipout       /* PURCHASES, ETC.                      */
            gosub hnypitkt     /* In Physical Inventory Count Session ?*/
            gosub history      /* Sales Analysis History               */
/*EWD001*/  if auto$ = "Y" then end     /* That's all folks.....       */ 
            gosub bomspec      /* Option Selection File                */
            gosub corepart     /* Core Cross Reference                 */

            gosub sermastr     /* Serial Number Master File            */

        REM Insure that the following is always the last GOSUB *********
            gosub sermastr_last  /* ALWAYS THE LAST 'GOSUB' */
            end



        REM *************************************************************~
            *        D A T A   F I L E   R O U T I N E S                *~
            *                                                           *~
            *  TEST CONDITIONS REQUIRED FOR EACH FILE. IF TEST FAILS,   *~
            *  INUSE$ IS SET TO "YES" AND REASON IS ENTERED INTO        *~
            *  USE$ TABLE.                                              *~
            *************************************************************~

        hnyquan
        REM CALL "SHOSTAT" ("Checking Inventory Status ")
            key$ = part$
L30110:     call "PLOWNEXT" (#1, key$, 25%, f1%(1))
                if f1%(1) = 0% then return
            get #1 using L30131, onhand, bckordr, onorder, comtd, inproc
L30131:         FMT POS(69), 5*PD(14,4)
            onhand = onhand + bckordr + onorder + comtd + inproc
            if onhand = 0 then goto L30110
            inuse$ = "YES" : use% = use% + 1%      /* USE% <=  1%      */
            use$(use%) = "* Part has Inventory, Orders, POs, or a Job In-~
        ~process."
            return

        bommastr     /* CHECK THAT PART HAS NO BOMS                    */
        REM CALL "SHOSTAT" ("Checking for BOMs")
            key$ = part$
L30320:     call "PLOWNEXT" (#2, key$, 25%, f1%(2))
                if f1%(2) = 0% then bomcomp
                if str(key$,29,3) = "  0" then L30320
            inuse$ = "YES" : use% = use% + 1%      /* USE% <=  2%      */
            use$(use%) = "* Part still has a Bill-of-Material."

            bomcomp      /* CHECK THAT DOES NOT APPEAR AS A COMPONENT */
               key$ = part$
L30400:        call "PLOWALTS" (#2, key$, 1%, 25%, f1%(2))
                   if f1%(2) = 0% then return
                if str(key$,29,3) = "  0" then L30400
               inuse$ = "YES"  :  use% = use% + 1% /* USE% <=  3%      */
               use$(use%) = "* Part appears as a component of a B-O-M."
               return

        rtemastr
        REM CALL "SHOSTAT" ("Checking for any Routing Information")
            key$ = part$
            call "PLOWNEXT" (#3, key$, 25%, f1%(3))
                if f1%(3) = 0% then return
            inuse$ = "YES"  : use% = use% + 1%     /* USE% <=  4%      */
            use$(use%) = "* Part still has Routing defined."
            return

        pipin
        REM CALL "SHOSTAT" ("Checking for Incoming Inventory")
            init (hex(00)) key$
            str(key$,,25) = part$
            call "PLOWALTS" (#5, key$, 1%, 25%, f1%(5))
                if f1%(5) = 0% then return
            inuse$ = "YES"  :  use% = use% + 1%    /* USE% <=  5%      */
            use$(use%) = "* Part has Inventory Receipts Planned."
            return

        pipout
        REM CALL "SHOSTAT" ("Checking for any Planned Issues of Inventory~
        ")
            init (hex(00)) key$
            str(key$,,25) = part$
            call "PLOWALTS" (#6, key$, 1%, 25%, f1%(6))
                if f1%(6) = 0% then return
            inuse$ = "YES"  :  use% = use% + 1%    /* USE% <=  6%      */
            use$(use%) = "* Part has Inventory Issues Planned."
            return


        hnypitkt
        REM CALL "SHOSTAT" ("Checking for Physical Inventory Count in Pro~
        gress")
            key$ = part$
            call "PLOWALTS" (#7, key$, 1%, 25%, f1%(7))
            if f1%(7) = 0% then return
            inuse$ = "YES"  :  use% = use% + 1%    /* USE% <=  7%      */
            use$(use%) = "* Part included in Physical Inventory."
            return

        history
        REM CALL "SHOSTAT" ("Checking for use in S/A History")
            if sakey% = 0% then return
                key$ = part$
                call "PLOWALTS" (#8, key$, sakey%, 25%, f1%(8))
                if f1%(8) = 0% then return
                     inuse$ = "YES"  :  use% = use% + 1%  /* USE% <= 8% */
                     use$(use%) = "* Part still has Sales History."
                     return

        sermastr
        REM CALL "SHOSTAT" ("Checking for Serial #s in use")
            key$ = str(part$,,25) & hex(00)
L31110:     call "PLOWNEXT" (#9, key$, 25%, f1%(9))
            if f1%(9) = 0% then return
            get #9 using L31140, status$
L31140:         FMT  CH(1)
            if status$ <> "2" then L31110
                inuse$ = "YES"  :  use% = use% + 1%  /* USE% <=  9%    */
                use$(use%) = "* Serial #(s) currently in inventory."
                return

        REM Insure that the following routine is always last ************
        sermastr_last
            if inuse$ = "YES" then return
            key$ = str(part$,,25) & hex(00)
            call "PLOWNEXT" (#9, key$, 25%, f1%(9))
            if f1%(9) = 0% then return
L31240:         u3% = 0%
                call "ASKUSER" (u3%, "*** SERIAL #'D PART ***",          ~
                     "Part " & part$ & " is Serial numbered",            ~
                     "Press PF(16) to DELETE it & continue"            & ~
                     "                     -- OR --",                    ~
                     "Press (RETURN) to continue WITHOUT deleting it")
                if u3% =  16% then return
                if u3% <>  0% then L31240
                     inuse$ = "YES"  :  use% = use% + 1% /* USE% <= 10% */
                     use$(use%) = "* Operator option ... Serial #s."
                     return

        bomspec    /* Check for Selection as an Option */
L31340:     ask% = 2%
            call "ASKUSER" (ask%, "***** OPTION TESTING *****",          ~
                           "Press RETURN to test for Selection as an " & ~
                           "Option  - or -", "Press PF-16 to Skip "    & ~
                           "this Check", "THIS CHECK MAY RUN A WHILE!")
            if ask% = 16% then return
            if ask% <> 0% then L31340

            init(hex(00)) readkey$
            call "READ102" (#10, readkey$, f1%(10))
            goto L31480

        readspec
        REM CALL "SHOSTAT" ("Checking Selection as an Option Part")
            call "READNEXT" (#10, f1%(10))
L31480:         if f1%(10) = 0% then return
            get #10 using L31500, demand$, oppart$
L31500:         FMT POS(57), CH(16), POS(80), CH(25)
            if oppart$ <> part$ then readspec
                inuse$ = "YES"  :  use% = use% + 1%  /* USE% <= 11%    */
                use$(use%) = "* Part is Selected as an Option for "  &   ~
                             "Demand Number " & demand$
                return

        corepart    /* Check if part is a core for something    */
        REM CALL "SHOSTAT" ("Checking Selection as a Core Part")
            init (" ") key$
            str(key$,,25) = part$
            call "PLOWALTS" (#12, key$, 1%, 25%, f1%(12))
            if f1%(12) = 0% then return  /* Core Master at best */
                inuse$ = "YES"  :  use% = use% + 1%  /* USE% <= 13%    */
                use$(use%) = "* Part is listed as a Core Part,"
                use$(use%) = use$(use%) & " (" & str(key$,26%,25%)
                use$(use%) = use$(use%) & ")."
                return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *  Return to Caller.                                        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN
            end

