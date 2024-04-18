        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA    SSS   U   U  M   M  IIIII  N   N  PPPP    *~
            *  S      A   A  S      U   U  MM MM    I    NN  N  P   P   *~
            *   SSS   AAAAA   SSS   U   U  M M M    I    N N N  PPPP    *~
            *      S  A   A      S  U   U  M   M    I    N  NN  P       *~
            *   SSS   A   A   SSS    UUU   M   M  IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SASUMINP - Screen to allow user to select which SA Summary*~
            *            file to inquiry or report on.  Is essentially  *~
            *            a very stupid (or specialized, if you prefer)  *~
            *            version of GETCODE.                            *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/02/86 ! Original                                 ! ERN *~
            * 11/30/90 ! Corrected index for DESCR$ on line 9180  ! MJB *~
            * 02/15/93 ! PRR 10570 Use 'SASUMINP' for On-Screen   ! JIM *~
            *          !   prog nm. Concat caller's name w/ descr.! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        sub "SASUMINP" (caller$, callerdescr$, pf$, descrs$(), ret%)

*        CALLER$ = The program name who is calling the subroutine.
*
*        CALLERDESCR$ = The Screen Title
*
*        PF$ = 2 characters- 1st: PF1  displayed? (Start Over) Y/N
*                            2nd: PF16 displayed? (Exit Program) Y/N
*
*        DESCRS$ = 10 x 30 array of the summary file names.  File 0
*                  should be in element 1, File 1 in element 2, and
*                  so forth.  Blank implies not used.
*
*        RET%    = File number selected (0-9)
*                  -OR-  101 if PF-1 executed, 116 if PF 16 hit

        dim                                                              ~
            caller$8, callerdescr$50,    /* Calling Program & Descr    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descrs$(10)30,               /* Summary file Descriptions  */~
            display$(10)40,              /* Display Strings            */~
            hdr1$5, hdr2$30,             /* Column Headers             */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            line1$64,                    /* Screen line 1              */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pf$2,                        /* PF-1, 16 options           */~
            pf1$16, pf16$16,             /* PF 1/ 16 Labels            */~
            pfkeys$5                     /* Available PF Keys          */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            line1$ = caller$ & ": " & callerdescr$
            str(line2$,62%) = "SASUMINP" & ": " & str(cms2v$,,8%)
            ret% = 99%    /* No File Selected      */

*        Set display up
            l% = 0%
            for f% = 1% to 10%
                if descrs$(f%) = " " then L09190
                    l% = l% + 1%
                    str(display$(l%),,2) = hex(0b8c)
                    convert f% - 1% to str(display$(l%),5,1), pic(0)
                    str(display$(l%),10) = descrs$(f%)
L09190:     next f%

            inpmessage$ = "Move Cursor to Summary File desired and" &    ~
                          " Press Return"

            hdr1$ = "File#"
            hdr2$ = "File Description"

            pfkeys$ = hex(00010d0f10)
            pf1$  = "(1)Start Over"
            pf16$ = "(16)Exit Program"
            if str(pf$,1,1) = "Y" then L09320
                pf1$ = " "  :  str(pfkeys$,2,1) = hex(ff)
L09320:     if str(pf$,2,1) = "Y" then L10000
                pf16$ = " "  :  str(pfkeys$,5,1) = hex(ff)


L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            ret% = 116%

L11100:     gosub'101                   /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       end
                  if keyhit% <>  0 then       L11100
            f% = cursor%(1) - 5%
            if f% < 1% or f% > l% then L11100

*        Translate Cursor Posistion into Summary File Number
            convert str(display$(f%),5,1) to ret%, data goto L11100
            end


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                return clear all
                ret% = 101%
                end


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101


L40090:     accept                                                       ~
               at (01,02), fac(hex(8c)), line1$                 , ch(64),~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (05,21), fac(hex(ac)), hdr1$                  , ch(05),~
               at (05,28), fac(hex(ac)), hdr2$                  , ch(30),~
                                                                         ~
               at (06,19), fac(hex(81)), display$( 1)           , ch(40),~
               at (06,19), fac(hex(8e)), display$( 1)           , ch(40),~
               at (07,19), fac(hex(8e)), display$( 2)           , ch(40),~
               at (08,19), fac(hex(8e)), display$( 3)           , ch(40),~
               at (09,19), fac(hex(8e)), display$( 4)           , ch(40),~
               at (10,19), fac(hex(8e)), display$( 5)           , ch(40),~
               at (11,19), fac(hex(8e)), display$( 6)           , ch(40),~
               at (12,19), fac(hex(8e)), display$( 7)           , ch(40),~
               at (13,19), fac(hex(8e)), display$( 8)           , ch(40),~
               at (14,19), fac(hex(8e)), display$( 9)           , ch(40),~
               at (15,19), fac(hex(8e)), display$(10)           , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf1$,                         ~
               at (22,65),  "(13)Instructions",                          ~
               at (23,65),  "(15)Print Screen",                          ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                   keys(str(pfkeys$)), key (keyhit%)


            if keyhit% <> 13 then L40420
                call "MANUAL" (caller$)
                goto L40090

L40420:     if keyhit% <> 15 then L40460
                call "PRNTSCRN"
                goto L40090

L40460:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


            end
