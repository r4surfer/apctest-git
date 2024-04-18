        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  L       OOO   TTTTT  PPPP   TTTTT   *~
            *  H   H  NN  N  Y   Y  L      O   O    T    P   P    T     *~
            *  HHHHH  N N N   YYY   L      O   O    T    PPPP     T     *~
            *  H   H  N  NN    Y    L      O   O    T    P        T     *~
            *  H   H  N   N    Y    LLLLL   OOO     T    P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYLOTPT - Subroutine to Inquire to HNYQUAN to find Lots  *~
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
            * 02/09/87 ! Original                                 ! MJB *~
            * 04/04089 ! Fixed to actually display Expiration date! MLJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYLOTPT" (#1)   /* Channel Number of HNYQUAN */

        dim                                                              ~
            date$8,                      /* Date for screen display    */~
            dsc_map(8),                  /* Desc Map for display       */~
            hdr$(3)80,                   /* Column titles              */~
            inpmessage$79,               /* Informational Message      */~
            i_x(1),                      /* Incl - Excl                */~
            i_x$(1)2,                    /* Incl - Excl                */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lotno$6,                     /* Lot Number                 */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            plowdescr$80,                /* Desc for PLOWCODE          */~
            plowkey$99                   /* Miscellaneous Read/Plow Key*/

        dim f2%( 4),                     /* = 0 if the file is open    */~
            f1%( 4),                     /* = 1 if READ was successful */~
            fs%( 4),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$( 4)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
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
            * # 1 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 2, "WORKFILE",                                      ~
                         varc, consec, recsize = 5

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            date$ = date
            call "DATEFMT" (date$)

            dsc_map(1) =  17.25   :  dsc_map(2) =  1
            dsc_map(3) =  69.08   :  dsc_map(4) = 28.104
            dsc_map(5) = 404.061  :  dsc_map(6) = 48
            dsc_map(7) =  42.03   :  dsc_map(8) = 42

            i_x(1) = 0  :  i_x$(1) = " "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf16$ = "(16)Return"
            lotno$ , inpmessage$ = " "

            inpmessage$ = "Enter Specific Lot Number to Check, or '?' to ~
        ~View Lots Currently on File"

L10130:         gosub'101               /* Display & Accept Screen    */
                      if keyhit% = 16  then end
                      if keyhit% <>  0 then L10130

        REM *************************************************************~
            *             N O W   T H E   R E A L   S T U F F           *~
            *-----------------------------------------------------------*~
            * Perform the Plow and Stuff Here.                          *~
            *************************************************************
            init(hex(00)) plowkey$
            if lotno$ <> "?" then get_specific
            hdr$(2) = "  Lot Number                                     "~
                    & "                             ."
            hdr$(1) = " "
            hdr$(3) = "Listing of Open Lots Currently in Inventory"
            plowdescr$ = hex(06) & "Please Position Cursor & Press RETURN~
        ~ To View Lot Information"
            call "PLOWCODE"(#1, plowkey$, plowdescr$, -1006%, -1.001,    ~
                                f1%(1), hdr$())
                if f1%(1) = 0% then end
            lotno$ = str(plowkey$,1,16)

        get_specific
            call "SHOSTAT" ("Extracting Information For Lot '" & lotno$  ~
                              & "', One Moment Please")
            hdr$(1) = "  Part Number               Qty-On-Hand  Store   "~
                    & "Exp. Date                    ."
            hdr$(2) = " "
            hdr$(3) = "Listing of Part Information in Open Lots"
            init(hex(00)) plowkey$
            str(plowkey$,1,16) = lotno$
            plowdescr$ = hex(06) & "Lot '" & lotno$ & "' is Currently on ~
        ~File for the Part(s) Shown Below"
            call "PLOWCODE" (#1, plowkey$, plowdescr$,  9006%,  1.01,    ~
                  f1%(1), hdr$(), 0, 0, i_x(), i_x$(), "D", "Y",         ~
                  #2%, dsc_map())

            goto inputmode     /* Replace this with END later */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101
              str(line2$,62%) = "HNYLOTPT: " & str(cms2v$,,8%)

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Validate Open Lots in Inventory",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch( 8),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (06,02), "Lot Number",                                 ~
               at (06,30), fac(hex(81)),   lotno$               , ch( 6),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000d0f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L40320
                  call "MANUAL" ("HNYLOTPT")
                  goto L40090

L40320:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40090

