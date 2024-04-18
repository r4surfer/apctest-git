        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII  RRRR   EEEEE   GGG    *~
            *  H   H  NN  N  Y   Y  P   P    I    R   R  E      G       *~
            *  HHHHH  N N N   YYY   PPPP     I    RRRR   EEEE   G GGG   *~
            *  H   H  N  NN    Y    P        I    R   R  E      G   G   *~
            *  H   H  N   N    Y    P      IIIII  R   R  EEEEE   GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPIREG - This program prints the Physical Inventory     *~
            *            Ticket Register and the Missing Tickets        *~
            *            Register for the specified Count Session.      *~
            *            Also updates the count session record in       *~
            *            HNYPISYS with a flag indicating whether or not *~
            *            all tickets have been accounted for (via data  *~
            *            entry) or not.                                 *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/10/86 ! Original                                 ! LDJ *~
            * 03/24/87 ! Changed To Print Quantity Case Break down! HES *~
            * 03/24/87 ! Miscel minor changes.                    ! LDJ *~
            * 10/30/91 !* CMS/DEC 'MASK' Project / Added 'ALLFREE'! SID *~
            *          !* Changed so that the report will properly!     *~
            *          !   shows the QTY BREAKDOWN Buckets.       !     *~
            *          !* PF)16 Exit Program is disabled during   !     *~
            *          !   the input mode once the user has gone  !     *~
            *          !   passed the 1st input field             !     *~
            *          !* Added SHOSTAT                           !     *~
            *          !* Added 4 '#' signs to the print format   !     *~
            *          !   of the 2nd Qty Breakdown Buckets       !     *~
            * 09/21/92 ! Changed ABC Class from CH(4) to CH(5)    ! SID *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            abc$5,                       /* ABC Classes to Count       */~
            accounted_for$1,             /* All Tickets Accounted For ?*/~
            at$(5)1,                     /* The 'AT' sign (@)          */~
            caseq(5),                    /* Quantity Per Case          */~
            cases(5),                    /* Number Of Cases            */~
            caseq$(5)5,                  /* Quantity Per Case          */~
            cases$(5)9,                  /* Number Of Cases            */~
            cat$(9,2)4,                  /* Categories to Count (Range)*/~
            check_digit$1,               /* Calc & Append Check Digit? */~
            company$60,                  /* Company / Division Name    */~
            count_date$8,                /* Date Counted               */~
            count_qty$10,                /* Ticket Count Qty           */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date1$8,                     /* Date Session Generated     */~
            date2$8,                     /* Date Quantities Captured   */~
            description$32,              /* Count Session Number       */~
            edtmessage$79,               /* Edit screen message        */~
            enter_date$8,                /* Date Count Entered         */~
            enter_time$8,                /* Time Count Entered         */~
            errormsg$79,                 /* Error message              */~
            extra$6,                     /* Number of Extra Tickets    */~
            file$8,                      /* HNYPITKT File Name         */~
            gvar$1,                      /* Update G/L variances flag  */~
            glvar$1,                     /* Update G/L variances flag  */~
            hyvar$,                      /* Update HNY variances Flag  */~
            hnyvar$,                     /* Update HNY variances Flag  */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastticket$6,                /* Last Ticket Nbr Processed  */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* HNYPITKT Library name      */~
            line2$79,                    /* Second Line of Screen Headr*/~
            loc$8,                       /* Part Location Code         */~
            lot$16,                      /* Lot Code                   */~
            lot_or_loc$1,                /* Tickets by Lot or Loc Flag */~
            missing$1,                   /* Print Open/Uncounted Only  */~
            name$20,                     /* Count by name              */~
            neg_only$1,                  /* Count Neg On-Hand Only ?   */~
            override$1,                  /* Cost Override flag         */~
            parameters$1,                /* Print Session Parameters   */~
            part$25,                     /* Part Code                  */~
            part$(3,2)25,                /* Parts to Count (Ranges)    */~
            part_req$1,                  /* Part Numbers required ?    */~
            pf16$16,                     /* PF 16 literal              */~
            pf4$17,                      /* PF  4 literal              */~
            plowkey$100,                 /* Misc Read / Plow Key       */~
            prefix$3,                    /* Ticket Number Prefix (Opt) */~
            print$1,                     /* Print tickets or sheets    */~
            printflag$1,                 /* Ticket/Count Sheets Flag   */~
            reportdate$45,               /* Formatted Date & Time      */~
            seq$1,                       /* Ticket Number Sequence     */~
            sequence$18,                 /* Ticket Number Sequence     */~
            session_date$8,              /* Planned Count Date         */~
            session_id$2,                /* Count Session Number       */~
            session_nbr$2,               /* Count Session Number       */~
            start_ticket$6,              /* Starting Ticket Number     */~
            status$40,                   /* Printed Status Message     */~
            store$3,                     /* Store / Warehouse Code     */~
            supplement$1,                /* Supplemental Ticket Flag   */~
            ticket$12,                   /* Formatted Ticket Number    */~
            time$8,                      /* Time Quantities Captured   */~
            title$42,                    /* Report Title               */~
            userid$3,                    /* Current User Id            */~
            user_id$3,                   /* Entered by user id         */~
            vol$6,                       /* HNYPITKT Volume name       */~
            whse$(9,2)3                  /* Warehouses to Count (Range)*/~

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
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
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
            * #1  ! HNYPISYS ! Physical Inventory System Session Contro *~
            * #2  ! HNYPITKT ! Physical Inventory Ticket File           *~
            * #3  ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYPISYS",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    7, keylen =   2,                     ~
                        alt key  1, keypos =    1, keylen =   8          ~

            select #2,  "HNYPITKT",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  52, dup,    ~
                            key  2, keypos =  313, keylen =  16          ~

            call "SHOSTAT" ("Opening Files, One Moment Please...")

            rslt$(1), rslt$(2) = "REQUIRED"

            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))

            if f2%(1) + f2%(2)  > 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value and Press (RETURN)."

            call "GETNAMES" addr(#2, file$, lib$, vol$)
            call "READFDR"  addr(file$,lib$,vol$,0%,"RC",records%,f2%(2))
            records% = max(1000%, records%)

            call "COMPNAME" (2%, company$, fieldnr%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, session_nbr$,              ~
                      description$     , missing$, parameters$

            call "ALLFREE"

            pf16$ = "(16)Exit Program"
            pf4$  = " "

            for fieldnr% = 1 to  3
L10100:         gosub'051(fieldnr%)
                      if fieldnr% = 1% then L10110
                         pf4$  = "(4)Previous Field"
                         pf16$ = " "
L10110:               if enabled% = 0 then L10240
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit%  = 16 and fieldnr% = 1% then exit_program
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10240:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
            pf4$  = " "
            pf16$ = "(16)Print Report"
            inpmessage$ = edtmessage$

L11060:     gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  3 then L11060
            gosub'051(fieldnr%)
                  if enabled% = 0% then L11060
L11130:     gosub'101(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Report Generation In Progress")
            call "SETPRNT" ("HNY006","HNYPI   ",records%, 0%)
            call "DATE" addr("HL", reportdate$)
            call "SPCESMSH" (reportdate$, 2%)
            call "STRING" addr("LJ",reportdate$, 45%)
            select printer (134)
            plowkey$ = str(session_nbr$)
            gosub dataload
            if missing$ = "N" then accounted_for$ = "Y"
            l% = 0%                     /* Number of Lines Left on Page*/
            page% = 0%                  /* Page Counter (of course)    */
            gosub print_report
            if parameters$ = "Y" then gosub print_params
            if l% < 1% then gosub print_heading
            print
            print using L34330
            close printer
            call "SETPRNT" ("HNY006","HNYPI   ",records%, 1%)
            gosub set_flag
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Count Session Num*/~
                                    L20200,         /* Print Open/Uncoun*/~
                                    L20300          /* Print Session Par*/
                     return
L20100:     REM Default/Enable for Count Session Number
                inpmessage$ = "Enter the Count Session Number to Print or~
        ~ RETURN to Find"
                return
L20200:     REM Default/Enable for Print Open/Uncounted Only
                inpmessage$ = "Enter 'Y' to Print Only Missing or Uncount~
        ~ed Tickets"
                if missing$ = " " then missing$ = "N"
                return
L20300:     REM Default/Enable for Print Session Parameters
                inpmessage$ = "Enter 'Y' to Print Session Parameters Info~
        ~rmation"
                if parameters$ = " " then parameters$ = "Y"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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
            u3% = 0%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload

            get #1 using L35030,                                          ~
            session_date$,  /* Date count session planned              */~
            session_nbr$,   /* Number corresponding to a Inventory Coun*/~
            description$,   /* Generic for general code descriptions   */~
            whse$(),        /* Warehouse or Stores  (Ranges)           */~
            cat$(),         /* Category codes (Ranges)                 */~
            part$(),        /* Part codes (ranges)                     */~
            neg_only$,      /* Count Negative items only ?             */~
            lot_or_loc$,    /* 1 Ticket per Lot or 1 per Location flag */~
            prefix$,        /* 1 to 3 character prefix to the P.I. Tick*/~
            start_ticket$,  /* Ticket Number to a Physical Inventory Co*/~
            lastticket$,    /* Last Ticket Number used                 */~
            check_digit$,   /* Calculate & append check digit ?        */~
            extra$,         /* Number of extra Physical Inventory Ticke*/~
            seq$,           /* Flag controlling what sequence tickets w*/~
            part_req$,      /* Part Numbers required in Count Entry ?  */~
            print$,         /* Print tickets or Count sheets  Indicator*/~
            hnyvar$,        /* Update inventory variances     Indicator*/~
            glvar$,         /* Update G/L       variances     Indicator*/~
            date1$,         /* Date a session generated                */~
            date2$,         /* Date costs/quantities captured          */~
            time$,          /* Time costs/quantities captured          */~
            accounted_for$, /* All Parts Accounted for Flag            */~
            abc$            /* ABC category                            */

            call "DATEFMT" (session_date$)
            call "DATEFMT" (date1$)
            call "DATEFMT" (date2$)
            call "TIME" (time$)

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        set_flag       /* Updates Accounted For Flag in Session Record */
            call "READ101" (#1, session_nbr$, f1%(1))
            if f1%(1) = 0% then return
            put #1 using L31090, accounted_for$
L31090:     FMT POS(365), CH(1)
            rewrite #1
            return


        print_report
            call "PLOWALTS" (#2, plowkey$, 2%, 0%, f1%(2))
            if f1%(2) = 0% then return
            get #2 using L35295,                                          ~
            ticket$,        /* Ticket Number to a Physical Inventory Co*/~
            recount%,       /* A number incremented for each recount (0*/~
            part$,          /* Part Number                             */~
            store$,         /* store number                            */~
            lot$,           /* lot number                              */~
            loc$,           /* bin location                            */~
            supplement$,    /* Supplemental Ticket                     */~
            hyvar$,         /* Flag or switch which controls posting to*/~
            gvar$,          /* Flag or switch which controls posting to*/~
            override$,      /* Override flag / indicator               */~
            name$,          /* Name of person who counted something    */~
            user_id$,       /* user-id of specific user                */~
            count_date$,    /* Date something was counted              */~
            enter_date$,    /* Date a transaction was entered          */~
            enter_time$,    /* The System Time when a transaction was e*/~
            count_qty,      /* Quantity counted of something           */~
            cases(),        /* Number Of Cases Counted                 */~
            caseq(),        /* Quantity Per Case                       */~
            session_id$,    /* Session Number                          */~
            printflag$      /* Ticket/Count Sheets Print Flag          */~

            str(plowkey$,4,12) = ticket$
            status$ = " "
            if enter_date$ = " " then status$ = " Not Entered / Accounted~
        ~ For"
            if session_id$ > session_nbr$ then return
            if printflag$ = "N" and count_qty >= 0 then                  ~
               status$ = "Ticket Not Yet Printed"
            if status$ > " " then accounted_for$ = " "
            if enter_date$ > " " and missing$ = "Y" then print_report
            if count_qty < 0 then status$ = " Voided Ticket"
            recount% = 99% - recount%
            if l% < 1% then gosub print_heading
            call "CONVERT" (count_qty, 0.2, count_qty$)
            if status$ > " " then count_qty$ = " "
            print using L34290, ticket$, recount%, store$, part$, lot$,   ~
                        loc$, supplement$, gvar$, hyvar$, override$,     ~
                        status$, count_qty$
            l% = l% - 1%
            if status$ > " " then L31690
            cases$(), caseq$(), at$() = " "
            test% = 0% : ii% = 1
            for i% = 1% to 5%
                if cases(i%) <= 0 then L31620
                at$(i%) = "@" : test% = test% + 1% : ii% = i%
                call "CONVERT" (cases(i%), 0.2, cases$(i%))
                call "CONVERT" (caseq(i%), -0.2, caseq$(i%))
L31620:     next i%
            if test% = 0% and caseq(ii%) = 1 then L31690
            print using L34322, cases$(1), at$(1), caseq$(1), cases$(2),  ~
                       at$(2), caseq$(2), cases$(3), at$(3), caseq$(3),  ~
                       cases$(4), at$(4), caseq$(4),  cases$(5), at$(5), ~
                       caseq$(5)
            l% = l% - 1%
L31690:     goto print_report
            return

        print_heading
            print page
            page% = page% + 1%
            print using L34051, reportdate$, company$, page%
            print using L34060
            if missing$="Y" then title$ = "MISSING TICKETS ONLY"         ~
                            else title$ = "ALL TICKETS"
            title$ = title$ & " FOR COUNT SESSION " & session_nbr$
            call "STRING" addr("CT", title$, 42%)
            print using L34130,              title$
            print
            l% = 52%
            if f1%(2) = 0% then return
            print using L34190
            print using L34220
            print using L34250
            return

        print_params
            gosub print_heading
            on val(seq$,1) - 48% gosub L33280, L33300, L33320, L33340, L33360,~
                                       L33380
            print using L34360
            print
            print using L34390, abc$, whse$(1,1), whse$(1,2), cat$(1,1),  ~
                               cat$(1,2), part$(1,1)
            print using L34430, neg_only$,whse$(2,1),whse$(2,2),cat$(2,1),~
                               cat$(2,2)
            print using L34460,              whse$(3,1), whse$(3,2),      ~
                               cat$(3,1), cat$(3,2), part$(1,2)
            print using L34500, lot_or_loc$, whse$(4,1), whse$(4,2),      ~
                               cat$(4,1), cat$(4,2)
            print using L34530,              whse$(5,1), whse$(5,2),      ~
                               cat$(5,1), cat$(5,2), part$(2,1)
            print using L34570, prefix$,     whse$(6,1), whse$(6,2),      ~
                               cat$(6,1), cat$(6,2)
            print using L34600, start_ticket$, whse$(7,1), whse$(7,2),    ~
                               cat$(7,1), cat$(7,2), part$(2,2)
            print using L34640, lastticket$, whse$(8,1), whse$(8,2),      ~
                               cat$(8,1), cat$(8,2)
            print using L34670, check_digit$, whse$(9,1), whse$(9,2),     ~
                               cat$(9,1), cat$(9,2), part$(3,1)
            print using L34710, extra$
            print using L34730, sequence$, part$(3,2)
            print using L34770, hnyvar$
            print using L34780, glvar$
            print using L34800, date1$
            print using L34810, date2$, time$
            return

L33280:     sequence$ = "Store/Loc/Part/Lot"
            return
L33300:     sequence$ = "Store/Part/Lot/Loc"
            return
L33320:     sequence$ = "Store/Part/Loc/Lot"
            return
L33340:     sequence$ = "Part/Store/Loc/Lot"
            return
L33360:     sequence$ = "Part/Loc/Lot/Store"
            return
L33380:     sequence$ = "Part/Lot/Loc/Store"
            return

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L34051: %################################### ############################~
        ~################################                        PAGE:####

L34060: %HNYPIREG                                         PHYSICAL INVENT~
        ~ORY TICKET REGISTER                                     RPT: HNY0~
        ~06

        %                                                     ###########~
        ~#########

L34130: %                                             ###################~
        ~#######################

L34190: %           Recnt                                                ~
        ~                Post Post Cost                                Tot~
        ~al

L34220: %Ticket Nbr  Nbr  Whse Part Code                 Lot    Location ~
        ~Extra G/L? Inv? Ovr?  Status / Error Message                  Cou~
        ~nt

L34250: %------------ --  ---- ------------------------- ------ -------- ~
        ~----- ---- ---- ----  ---------------------------------- --------~
        ~--

L34290: %############ ##   ### ######################### ###### ######## ~
        ~  #    #    #    #    #################################  ########~
        ~##

L34322: %                  Qty Breakdown: ######### # #####   ######### #~
        ~ #####   ######### # #####   ######### # #####   ######### # ####~
        ~#
L34330: %                                           *************** END O~
        ~F REGISTER ***************

L34360: %                                                      SESSION PA~
        ~RAMETERS SECTION

L34390:  % ABC Classes Selected: #####           Warehouses to Count: ###~
        ~ to ###   Categories: #### to ####   Parts: #####################~
        ~####

L34430:  % Count Negative QOH Only?: ###                              ###~
        ~ to ###               #### to ####              to

L34460:  % 1 Ticket per Part/Store/Lot (P),                           ###~
        ~ to ###               #### to ####          #####################~
        ~####

L34500:  %  or 1 per Part/Store/Lot/Loc (L): #                        ###~
        ~ to ###               #### to ####

L34530:  %                                                            ###~
        ~ to ###               #### to ####          #####################~
        ~####

L34570:  % Ticket Number Prefix   : ###                               ###~
        ~ to ###               #### to ####              to

L34600:  % Starting Ticket Number : ############                      ###~
        ~ to ###               #### to ####          #####################~
        ~####

L34640:  % Ending Ticket Number   : ############                      ###~
        ~ to ###               #### to ####

L34670:  % Check Digits ?         : ###                               ###~
        ~ to ###               #### to ####          #####################~
        ~####

L34710:  % Number of Extra Tickets: ######                               ~
        ~                                                to
L34730:  % Ticket Number Sequence : ##################                   ~
        ~                                            #####################~
        ~####

L34770:  % Post Var to Inventory ?: #
L34780:  % Post Variance to G/L  ?: #

L34800:  % Session Created On     : ########
L34810:  % Costs/Qtys Captured On : ########  at ########

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: HNYPISYS                          */~
            CH(6),          /* Date something was counted              */~
            CH(2),          /* Number corresponding to a Inventory Coun*/~
            CH(30),         /* Generic for general code descriptions   */~
            XX(3),          /* Filler                                  */~
            18*CH(3),       /* Warehouse or Stores                     */~
            18*CH(4),       /* category code                           */~
            6*CH(25),       /* Part code                               */~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(3),          /* 1 to 3 character prefix to the P.I. Tick*/~
            CH(06),         /* Ticket Number to a Physical Inventory Co*/~
            CH(06),         /* Last ticket number generated for a Count*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(6),          /* Number of extra Physical Inventory Ticke*/~
            CH(1),          /* Flag controlling what sequence tickets w*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(6),          /* Date a transaction was entered          */~
            CH(6),          /* System Date                             */~
            CH(6),          /* The System Time when a transaction was e*/~
            CH(1),          /* All Parts Accounted for Flag            */~
            CH(5)           /* ABC Categories                          */~
        /*  CH(1)           /* Source Flag                             */~
        /*  CH(141)         /* Filler                                  */~

L35295: FMT                 /* FILE: HNYPITKT                          */~
            XX(2),          /* Number corresponding to a Inventory Coun*/~
            CH(12),         /* Ticket Number to a Physical Inventory Co*/~
            BI(1),          /* A number incremented for each recount (0*/~
            CH(25),         /* Part Number                             */~
            CH(3),          /* store number                            */~
            CH(16),         /* lot number                              */~
            CH(8),          /* bin location                            */~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* Flag or switch which controls posting to*/~
            CH(1),          /* Flag or switch which controls posting to*/~
            CH(1),          /* Override flag / indicator               */~
            CH(20),         /* Name of person who counted something    */~
            CH(3),          /* user-id of specific user                */~
            CH(6),          /* Date something was counted              */~
            CH(6),          /* Date a transaction was entered          */~
            CH(6),          /* The System Time when a transaction was e*/~
            PD(14,4),       /* Quantity counted of something           */~
            5*PD(14,4),     /* Number Of Cases                         */~
            5*PD(14,4),     /* Quantity Per Case                       */~
            XX(112),        /* quantity & Cost fields (14)             */~
            CH(2),          /* Session Number                          */~
            CH(1),          /* Ticket/Count Sheets for Print Flag      */~
            CH(177)         /* Filler For Rest of Record or Internal Sp*/

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "HNYPIREG: " & str(cms2v$,,8%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40170,         /* Count Session Num*/~
                                    L40170,         /* Print Open/Uncoun*/~
                                    L40170          /* Print Session Par*/
                  goto L40240

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Physical Inventory Ticket Register",            ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Count Session Number      :",                         ~
               at (06,30), fac(lfac$( 1)), session_nbr$         , ch(02),~
               at (06,49), fac(hex(8c)),   description$         , ch(32),~
               at (07,02),                                               ~
                  "Print Missing Tickets Only:",                         ~
               at (07,30), fac(lfac$( 2)), missing$             , ch(01),~
               at (08,02),                                               ~
                  "Print Session Parameters  :",                         ~
               at (08,30), fac(lfac$( 3)), parameters$          , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,04), "(1)Start Over",                              ~
               at (23,25), fac(hex(8c)), pf4$                   , ch(17),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(84)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40640
                  call "MANUAL" ("HNYPIREG")
                  goto L40240

L40640:        if keyhit% <> 15 then L40680
                  call "PRNTSCRN"
                  goto L40240

L40680:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Page 1.                        *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Count Session Num*/~
                                    L50200,         /* Print Open/Uncoun*/~
                                    L50300          /* Print Session Par*/
                     return
L50100:     REM Test Data for Count Session Number
                call "GETCODE" (#1, session_nbr$,description$     , 1%,  ~
                                0, f1%(1))
                if f1%(1) = 0% then errormsg$ =                          ~
                   "Session Undefined or None on File"
                return
L50200:     REM Test Data for Print Open/Uncounted Only
                if missing$ = "Y" or missing$ = "N" then return
                errormsg$ = "Must be 'Y' or 'N'"
                return
L50300:     REM Test Data for Print Session Parameters
                if parameters$ = "Y" or parameters$ = "N" then return
                errormsg$ = "Must be 'Y' or 'N'"
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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            end
