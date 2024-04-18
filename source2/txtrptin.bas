        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  TTTTT  X   X  TTTTT  RRRR   PPPP   TTTTT  IIIII  N   N   *~
            *    T     X X     T    R   R  P   P    T      I    NN  N   *~
            *    T      X      T    RRRR   PPPP     T      I    N N N   *~
            *    T     X X     T    R   R  P        T      I    N  NN   *~
            *    T    X   X    T    R   R  P        T    IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTRPTIN - Allows user to define which Text Types appear  *~
            *            on which outputs.                              *~
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
            * 10/21/86 ! Original                                 ! ERN *~
            * 04/13/87 ! Added Missing Source                     ! MJB *~
            * 04/14/87 ! Mod to set all print flags 'ON'          ! MJB *~
            * 05/11/87 ! Added Estimate Acks,AR late notes to list! HES *~
            * 01/06/89 ! Job Order Traveler JB0001 is now JB0004. ! JIM *~
            * 04/19/89 ! Added Part Master Text to report BOM001  ! RJM *~
            * 08/03/90 ! Added Part Master Text to report BOM002  ! MJB *~
            * 12/18/91 ! Modified by Grace Specialty Chemical -add! LDJ *~
            *          ! BOM Header Text as a valid source for    !     *~
            *          ! Sales Order Acknowledgements and BOM     !     *~
            *          ! and Part Text as a source for BOL's.     !     *~
            * 03/23/92 ! Rehost.  Text file may allow 1 - 28 lines! KAB *~
            *          !   of text per record.  Monitors record   !     *~
            *          !   length to determine how many.          !     *~
            *          ! One fix. ARI006 is now ARM006 to agree   !     *~
            *          !    with Report Id in ARMLATRP.           !     *~
            * 02/24/92 ! PRR 12023  Added SO text to Pick Lists & ! JDH *~
            *          !   BOLs.    Also, SO text to A/R Invoices.!     *~
            * 10/14/92 ! Added COR006-Core Deposit Acknowledgement! JIM *~
            * 11/24/92 ! PRR 12660  Added BCK008 to list. Ord Detl! JDH *~
            * 09/01/93 ! PRR 10697 - Added JB0016 to table.       ! MLJ *~
            * 03/31/94 ! PRR 13132  Added vendor & vpr to POs.    ! JDH *~
            * 06/28/94 ! Added Sources 021 & 037 to JB0004.       ! LDJ *~
            * 07/14/94 ! Added Report ID VPC001 - Purchase Contrac! LDJ *~
            * 10/07/94 ! Added Sources 014 to JB0004 & JB0016     ! HES *~
            * 11/10/94 ! Added Report ID VSA001 - VSA Sheet       ! LDJ *~
            * 12/08/94 ! PRR 13327.  Fixed rpt id for A/R invoices! JDH *~
            * 07/07/95 ! Added Customer to Acks & BOLs.           ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            filler$(10)200,              /* Blank Filler               */~
            filler64$64,                 /* Blank Filler               */~
            hfac$1,                      /* Header FAC                 */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pfkeys$20,                   /* PF Keys Available          */~
            pf$(3)79,                    /* PF Keys Descriptors        */~
            pflag$(10)1,                 /* Print Type Flags           */~
            readkey$20,                  /* Miscellaneous Read/Plow Key*/~
            pfac$1,                      /* Cursor Position Fac        */~
            plowkey$20,                  /* Miscellaneous Read/Plow Key*/~
            p$(3)1,                      /* Plowcode dummy argument    */~
            rpt$(300)67,                 /* Report Table (Keep < 32K!) */~
            rptid$6, rptdescr$30,        /* Report Number & Title      */~
            sfac$(10)1,                  /* Source FACs                */~
            srcedsply$(10)25,            /* Sources Display            */~
            srcehdr$25,                  /* Sources Header             */~
            srces$(10)3,                 /* Available Sources for Rpt  */~
            srces$30,                    /* Available Sources for Rpt  */~
            tfac$(10,2)1,                /* Types FACs                 */~
            typedsply$(10)25,            /* Types Display              */~
            typehdr$27,                  /* Types Header               */~
            typeids$(10)1,               /* Type IDs by Source         */~
            types$(10)10                 /* Types to Print             */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * #1  ! TXTFILE  ! System Text File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(1 ) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))

            if min(fs%()) < 0% then exit_program

            call "GETUFBRS" addr(#1, filler64$)
            textsize% = val(str(filler64$,,2), 2)
            textsize% = textsize% - 64%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)

*        Define Available Reports and the Sources that may appear
            gosub define_report_table

            for r% = 1% to 10000%        /* Add, update Xref file      */
                if str(rpt$(r%),,1) = "x" then L09540

                readkey$ = "X" & str(rpt$(r%),,6) & hex(00000000)
                call "PLOWNEXT" (#1, readkey$, 7%, f1%(1))
                if f1%(1) = 0% then set_types
                   if str(readkey$,10,1) = hex(00) then L09230
                      /* OLD FORMAT */
                      get #1 using L09200, str(types$(),,100)
L09200:                    FMT POS(72), CH(100)
                     goto L09360

L09230:            if textsize% < 130% then L09280
                      get #1 using L09250, str(types$(),,100)
L09250:                    FMT POS(95), CH(100)
                     goto L09360

L09280:              get #1 using L09290, str(types$(),,50%)
L09290:                   FMT POS(80), CH(50)
                     init (" ") str(types$(),51)
                     call "PLOWNEXT" (#1, readkey$, 7%, f1%(1))
                        if f1%(1) = 0% then L09360
                     get #1 using L09290, str(types$(),51%,50%)

*       ** Entry From SET_TYPES
L09360:        put filler64$ using L09370, readkey$, str(rpt$(r%),7,30), " "
L09370:            FMT CH(11), CH(30), CH(23)
               init (hex(00)) str(readkey$, 8)
               call "DELETE" (#1, readkey$, 7%)
               str(filler64$,8,4) = hex(20200001)
               if textsize% < 130% then L09460
               write #1, str(filler64$,,64), str(rpt$(r%),37,30),        ~
                   str(types$(),,100), str(filler$(),,textsize% - 130%)
               goto L09520

L09460:        write #1, str(filler64$,,64), str(rpt$(r%),37,15),        ~
                   str(types$(),,50), str(filler$(),,textsize% - 65%)
               str(filler64$,8,4) = hex(20200002)
               write #1, str(filler64$,,64), str(rpt$(r%),52,15),        ~
                   str(types$(),51,50), str(filler$(),,textsize% - 65%)

L09520:     next r%

L09540
*         Kill any stuff in the file that's NOT in the table
            plowkey$ = "X"
L09560:     call "PLOWNXT1" (#1, plowkey$, 1%, f1%(1))
            if f1%(1) = 0% then L09630
                search str(rpt$()) = str(plowkey$,2,6) to cursor%()      ~
                                                                step 67%
                if cursor%(1) = 0% then delete #1
                goto L09560

L09630
*        Now define other variables required for the program
            srcehdr$ = "Available Sources"
            typehdr$ = "  Types to Print"
            goto inputmode

        set_types     /* Load TYPES$() */
            for i% = 37% to 64% step 3%
                j% = 1%
                k% = (i%-34%)/3%
                str(types$(k%),1) = " "
                if str(rpt$(r%),i%,3%) = " " then L09830
                    plowkey$ = "T" & str(rpt$(r%),i%,3%)
                    if str(rpt$(r%),i%,3%) <> "CPY" then L09780
                        str(types$(k%),,1) = "1"
                        goto L09830
L09780:             call "PLOWNEXT" (#1, plowkey$, 4%, ok%)
                        if ok% = 0% then L09830
                    str(types$(k%),j%,1%) = str(plowkey$,5,1)
                    j% = j% + 1%
                    goto L09780
L09830:     next i%
            goto L09360

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub init_for_input


            for fieldnr% = 1% to 1%
                gosub'051(1%)           /* Set Input Message          */
L10330:         gosub'101(1%)           /* Get Report ID              */
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10330
                gosub'151(1%)           /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10330
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        if srces% > 1% then select_source
            cursor%(1) = 10%
            gosub'151(2%)  /* Loads Types for the single source    */
            goto select_types

        select_source
            gosub'051(2%)               /* Set Input Message           */
L11100:     gosub'101(2%)               /* Get which source to edit    */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then datasave
                  if keyhit% <>  0 then select_source
            gosub'151(2%)               /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11100


        select_types
            gosub'051(3%)               /* Set Input Message           */
            gosub'101(3%)               /* Get which source to edit    */
                  if keyhit%  =  1 then gosub startover
            gosub'151(3%)               /* Update Print Indicators     */
                  if keyhit%  = 16 and srces% > 1% then select_source
                  if keyhit%  = 16 and srces% = 1% then datasave
            goto select_types

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(screen%)
                  on screen%  gosub L20130,         /* Report ID        */~
                                    L20180,         /* Sources          */~
                                    L20230          /* Types            */
                  return


L20130
*        Enter Report ID
            inpmessage$ = "Enter Report ID to maintain.  Leave Blank" &  ~
                          " to see IDs on file."
            return

L20180
*        Select Which Source to manage
            inpmessage$ = "Position Cursor at Source to manage and"  &   ~
                          " press RETURN."
            return

L20230
*        Select Which Types to Print
            inpmessage$ = "Place a non-blank character next to Types" &  ~
                          " to print on the Report."


        REM *************************************************************~
            *         D E F I N E   R E P O R T   T A B L E             *~
            *-----------------------------------------------------------*~
            * Define which reports allow text and which Sources of text *~
            * are allowed on those reports                              *~
            *************************************************************
*       * IMPORTANT NOTE   ********************************************
*          When DELETING a source, replace source number with 'xxx'   *
*            and DO NOT move the remaining source numbers!            *
*          To CHANGE a source, DELETE the old and append the new at   *
*            the end of the existing list.                            *
*          To ADD a source, simply append the number at the end of    *
*            the existing list.                                       *
*       ***************************************************************

        define_report_table

*        POS(len):  1(6)   7(30)                        37(3x10)
*       Defintion: "RPT-ID Report  Title----------------S01S02......S10"
        rpt$( 1%) = "BOM001Single Level Bill of Materials009010001      "
        rpt$( 2%) = "JB0004Job Order Traveler            007008021037014"
        rpt$( 3%) = "VBK001Purchase Orders               001004003CPY002"
        rpt$( 3%) = rpt$( 3%) &                         "024            "
        rpt$( 4%) = "RTE001Routings                      007008         "
        rpt$( 5%) = "BOM002Exploded Bill of Materials    009010001      "
        rpt$( 6%) = "VEN001Vendor Master Listing         002            "
        rpt$( 7%) = "ARI006A/R Invoices                  001015016013014"
        rpt$( 8%) = "BCK002Sales Order Acknowledgements  001013014009012"
        rpt$( 9%) = "CUS001Customer Detail Listing       012            "
        rpt$(10%) = "CUS002Customer Listing- Summary     012            "
        rpt$(11%) = "EST002Estimates, Customer Copy      017            "
        rpt$(12%) = "EST001Estimates, Detailed Report    017            "
        rpt$(13%) = "ARM006A/R Late Notices              018            "
        rpt$(14%) = "SHP004Shipping Bill of Lading       001009013014012"
        rpt$(15%) = "SHP003Shipping Pick List            013014         "
        rpt$(16%) = "COR006Core Deposit Acknowledgements 030031         "
        rpt$(17%) = "BCK008Print Order Details           001013014      "
        rpt$(18%) = "JB0016Materials Requirements List   009021014      "
        rpt$(19%) = "VPC001Purchase Contracts Summary Rpt038            "
        rpt$(20%) = "VSA001Vendor Service Advice Sheets  037004         "
        rpt$(21%) = "xxxxxxIndicates End - A MUST!!!!!                  "

        return

        REM *************************************************************~
            *               I N I T I A L I Z A T I O N                 *~
            *-----------------------------------------------------------*~
            * Routine which Initialize Variables.                       *~
            *************************************************************

        init_for_input
            init(" ") errormsg$, inpmessage$, rptid$, rptdescr$

            return


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
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File. Record already in buffer.           *~
            *************************************************************
        load_report
            str(plowkey$,8) = hex(00000000)
            call "PLOWNEXT" (#1, plowkey$, 7%, f1%(1))
                if f1%(1) = 0% then return
            get #1 using L30070, rptid$, rptdescr$
L30070:         FMT XX(1), CH(6), XX(4), CH(30)
            if str(key(#1),10,1) = hex(00) then L30140
               /* OLD FORMAT */
            get #1 using L30110, str(srces$(),,30), str(types$(),,100)
L30110:         FMT POS(42), CH(30), CH(100)
            goto L30290

L30140:     if textsize% < 130% then L30190
            get #1 using L30160, str(srces$(),,30), str(types$(),,100)
L30160:         FMT POS(65), CH(30), CH(100)
            goto L30290

L30190:     get #1 using L30200, str(srces$(),,15), str(types$(),,50)
L30200:         FMT POS(65), CH(15), CH(50)
            init (" ") str(srces$(),16), str(types$(),51)
            plowkey$ = key(#1)
            call "PLOWNEXT" (#1, plowkey$, 7%, f1%(1))
               if f1%(1) = 0% then L30290
            get #1 using L30200, str(srces$(),16,15), str(types$(),51,50)


*        Describe Sources
L30290:     init (" ") srcedsply$()
            srces% = 0%
            if str(srces$()) <> " " then L30340
                errormsg$ = "No Sources Defined for this Report"
                return
L30340:     for srces% = 1% to 10%
                if srces$(srces%) = " " then L30460

                if srces$(srces%) <> "CPY" then L30400
                     srcedsply$(srces%) = "Copy Element Text"
                     goto L30450
L30400:         readkey$ = "S" & srces$(srces%)
                srcedsply$(srces%) = "Undefined"
                call "READ100" (#1, readkey$, f1%(1))
                if f1%(1) = 1% then get #1 using L30440, srcedsply$(srces%)
L30440:              FMT XX(11), CH(25)
L30450:     next srces%
L30460:     srces% = srces% - 1%
            return


        load_types
            init (" ") pflag$(), typedsply$(), typeids$()
            types% = 0%

            plowkey$ = "T" & srces$(srce%)
            if srces$(srce%) <> "CPY" then L30600
                types%        = 1%
                typeids$(1)   = "1"
                typedsply$(1) = "Copy Element Text"
                goto L30650
L30600:     call "PLOWNEXT" (#1, plowkey$, 4%, f1%(1))
            if f1%(1) = 0% then L30680
                types% = types% + 1%
                get #1 using L30640, typeids$(types%), typedsply$(types%)
L30640:              FMT XX(4), CH(1), XX(6), CH(25)
L30650:         if pos(str(types$(srce%)) = typeids$(types%)) <> 0% then ~
                                                    pflag$(types%) = "P"
                goto L30600
L30680:     if types% > 0% then return
                errormsg$ = "There are no Types on file for this Source"
                return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            readkey$ = "X" & rptid$ & hex(20202020)
            call "READ101" (#1, readkey$, f1%(1))
               if f1%(1) = 0% then L31160
            get #1 using L31100, srces$
L31100:         FMT POS(42), CH(30)
            delete #1       /* Out with the Old */
            put #1 using L31130, hex(20200001), " ", srces$
L31130:         FMT POS(8), CH(4), POS(42), CH(23), POS(65), CH(30)
            write #1        /* In with the New  */

L31160:     readkey$ = "X" & rptid$ & hex(20200001)
            call "READ101" (#1, readkey$, f1%(1))
               if f1%(1) = 0% then return

            if textsize% < 130% then L31260
               put #1 using L31220, str(types$(),,100)
L31220:            FMT POS(95), CH(100)
            rewrite #1
            return

L31260:     put #1 using L31270, str(types$(),,50)
L31270:            FMT POS(80), CH(50)
            rewrite #1
            readkey$ = "X" & rptid$ & hex(20200002)
            call "READ101" (#1, readkey$, f1%(1))
               if f1%(1) = 0% then return           /* Not Likely */
            put #1 using L31270, str(types$(),51,50)
            rewrite #1
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(screen%)
            str(line2$,62%) = "TXTRPTIN: " & str(cms2v$,,8%)
            on screen% gosub rptid, sources, types

L40100: accept                                                           ~
            at (01,02), "Manage Text Printing",                          ~
            at (01,66), "Today:",                                        ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,10), "Report ID",                                     ~
            at (06,26), fac(hfac$    ), rptid$                  , ch(06),~
            at (07,10), "Report Title",                                  ~
            at (07,26), fac(hex(84)),   rptdescr$               , ch(32),~
                                                                         ~
            at (09,10), fac(hex(ac)),   str(srcehdr$)           , ch(25),~
            at (10,10), fac(pfac$    ), srcedsply$( 1)          , ch(25),~
            at (10,10), fac(sfac$( 1)), srcedsply$( 1)          , ch(25),~
            at (11,10), fac(sfac$( 2)), srcedsply$( 2)          , ch(25),~
            at (12,10), fac(sfac$( 3)), srcedsply$( 3)          , ch(25),~
            at (13,10), fac(sfac$( 4)), srcedsply$( 4)          , ch(25),~
            at (14,10), fac(sfac$( 5)), srcedsply$( 5)          , ch(25),~
            at (15,10), fac(sfac$( 6)), srcedsply$( 6)          , ch(25),~
            at (16,10), fac(sfac$( 7)), srcedsply$( 7)          , ch(25),~
            at (17,10), fac(sfac$( 8)), srcedsply$( 8)          , ch(25),~
            at (18,10), fac(sfac$( 9)), srcedsply$( 9)          , ch(25),~
            at (19,10), fac(sfac$(10)), srcedsply$(10)          , ch(25),~
                                                                         ~
            at (09,47), fac(hex(ac)),   str(typehdr$)           , ch(27),~
            at (10,47), fac(tfac$( 1,1)), pflag$( 1)            , ch(01),~
            at (11,47), fac(tfac$( 2,1)), pflag$( 2)            , ch(01),~
            at (12,47), fac(tfac$( 3,1)), pflag$( 3)            , ch(01),~
            at (13,47), fac(tfac$( 4,1)), pflag$( 4)            , ch(01),~
            at (14,47), fac(tfac$( 5,1)), pflag$( 5)            , ch(01),~
            at (15,47), fac(tfac$( 6,1)), pflag$( 6)            , ch(01),~
            at (16,47), fac(tfac$( 7,1)), pflag$( 7)            , ch(01),~
            at (17,47), fac(tfac$( 8,1)), pflag$( 8)            , ch(01),~
            at (18,47), fac(tfac$( 9,1)), pflag$( 9)            , ch(01),~
            at (19,47), fac(tfac$(10,1)), pflag$(10)            , ch(01),~
                                                                         ~
            at (10,49), fac(tfac$( 1,2)), typedsply$( 1)        , ch(25),~
            at (11,49), fac(tfac$( 2,2)), typedsply$( 2)        , ch(25),~
            at (12,49), fac(tfac$( 3,2)), typedsply$( 3)        , ch(25),~
            at (13,49), fac(tfac$( 4,2)), typedsply$( 4)        , ch(25),~
            at (14,49), fac(tfac$( 5,2)), typedsply$( 5)        , ch(25),~
            at (15,49), fac(tfac$( 6,2)), typedsply$( 6)        , ch(25),~
            at (16,49), fac(tfac$( 7,2)), typedsply$( 7)        , ch(25),~
            at (17,49), fac(tfac$( 8,2)), typedsply$( 8)        , ch(25),~
            at (18,49), fac(tfac$( 9,2)), typedsply$( 9)        , ch(25),~
            at (19,49), fac(tfac$(10,2)), typedsply$(10)        , ch(25),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1)                  , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2)                  , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3)                  , ch(79),~
                keys(str(pfkeys$)), key(keyhit%)


               if keyhit% <> 13 then L40680
                  call "MANUAL" ("TXTRPTIN")
                  goto L40100

L40680:        if keyhit% <> 15 then L40720
                  call "PRNTSCRN"
                  goto L40100

L40720:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return



        rptid   /* Set the Screen up for Report Selection              */
           hfac$ = hex(81)
           init(hex(9c)) sfac$(), tfac$(), pfac$

           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Program"
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0f10ffffff00)

           return


        sources  /* Set up Screen for selection of Source              */
           hfac$ = hex(84)
           init(hex(8c)) sfac$()
           init(hex(9c)) tfac$()
           pfac$ = hex(80)

           for i% = 1% to srces%  :  sfac$(i%) = hex(86)  :  next i%

           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Save Data   "
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0f10ffffff00)

           return


        types   /* Set up the screen for type's selection    */
           hfac$ = hex(84)
           init(hex(8c)) sfac$() : sfac$(srce%) = hex(84)
           init(hex(9c)) tfac$(), pfac$

           for i% = 1% to types%
                tfac$(i%,1) = hex(81)
                if pflag$(i%) = " " then tfac$(i%,2) = hex(8c) else      ~
                                         tfac$(i%,2) = hex(84)
           next i%

           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)New Source  "
           if srces% = 1% then str(pf$(3),64) = "(16)Save Data"
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0f10ffffff00)

           return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(screen%)
            errormsg$ = " "
                  on screen%  gosub L50130,         /* Report ID        */~
                                    L50230,         /* Sources          */~
                                    L50300          /* Types            */
                  return

L50130
*        Report ID                             REPORT$
            plowkey$ = "X" & str(rptid$,,6) & hex(20200001)
            rptdescr$ = hex(06) & "Select Report to Manage"
*       *  CALL "PLOWCODE" (#1, PLOWKEY$, RPTDESCR$, 1%, 0.30, F1%(1))
            call "PLOWCODE" (#1, plowkey$, rptdescr$, 2001%, 0.30,       ~
                                 f1%(1), p$(), 6)
            if f1%(1) = 1% then L50200
                errormsg$ = "Invalid Report ID"
                return
L50200:     gosub load_report  /* Loads and formats sources */
            return

L50230
*        Test for Selected Source
            srce% = cursor%(1) - 9%
            if srce% >= 1% and srce% <= srces% then L50270
                errormsg$ = hex(00)  :  return
L50270:     gosub load_types
            return

L50300
*        Types- Stuff Print flags indicated into TYPES$
            types$(srce%) = " "
            for t% = 1% to types%
                if pflag$(t%) = " " then L50370
                     if types$(srce%) = " "                  then        ~
                          types$(srce%) = typeids$(t%)       else        ~
                          types$(srce%) = types$(srce%) & typeids$(t%)
L50370:     next t%
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

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
