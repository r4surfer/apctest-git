        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII  TTTTT  IIIII   CCC    *~
            *  H   H  NN  N  Y   Y  P   P    I      T      I    C   C   *~
            *  HHHHH  N N N   YYY   PPPP     I      T      I    C       *~
            *  H   H  N  NN    Y    P        I      T      I    C   C   *~
            *  H   H  N   N    Y    P      IIIII    T    IIIII   CCC    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPITIC - Prints either Tickets or Count Sheets (as set  *~
            *            determined by the Inventory Parameters set in  *~
            *            HNYPIDEF) for a Physical Inventory Session.    *~
            *            Program will either print a specified range or *~
            *            all "unprinted" tickets (alt key 1 in HNYPITKT)*~
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
            * 01/14/86 ! Original                                 ! LDJ *~
            * 11/05/86 ! Modified to print CAELUS Standard Form   ! MJB *~
            * 03/19/87 ! File layout changes, no significant      ! LDJ *~
            *          !  functionality changes to this program.  !     *~
            * 05/27/87 ! Standard Costing Enhancements            ! MJB *~
            * 08/13/90 ! Basic 4.3 & SSL Compatibility            ! KAB *~
            * 11/01/91 ! CMS/DEC 'MASK' Project/Added 'ALLFREE'   ! SID *~
            *          !    and 'SHOSTAT'                         !     *~
            * 06/04/92 ! For PRR 11453, HNYPIGEN does not automat-! RJH *~
            *          !  tically generate the HNYPICST so we     !     *~
            *          !  check here to see if it's been made yet.!     *~
            * 09/23/92 ! Use Location Quan for Tickets w/Location ! RJH *~
            *          !  & use 'L' sufix to denote as such.      !     *~
            * 10/19/92 ! Add TAB stops in Edit Mode.              ! RJH *~
            * 03/12/92 ! PRR 12822 - Fix printing of Quantity when! RJH *~
            *          ! ticket has a Bin Loc only.Prefixed Marker!     *~
            * 05/09/95 ! PRR 13404,13420.  Write System Flag, too.! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            boh$10,                      /* Book On Hand Quantity      */~
            company$60,                  /* Company / Division Name    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            description$32,              /* Count Session Description  */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* HNYPITKT File Name         */~
            from_ticket$12,              /* Print From Ticket          */~
            i$(24)80,                    /* Screen Image               */~
            infomsg$79,                  /* Print Output - C or T      */~
            inpmessage$79,               /* Informational Message      */~
            lastpart$25,                 /* Last Part Number Read      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lflag$1,                     /* Printing Qty from Locations*/~
            lib$8,                       /* HNYPITKT Library name      */~
            line2$79,                    /* Second Line of Screen Headr*/~
            loc$8,                       /* Part Location Code         */~
            lot$16,                      /* Lot Code                   */~
            part$25,                     /* Part Code                  */~
            partdescr$32,                /* Part Code Description      */~
            pf16$16,                     /* PF 16 literal              */~
            pf4$17,                      /* PF  4 literal              */~
            pf17$16,                     /* PF 17 literal              */~
            plowkey$100,                 /* Miscellaneous Read/Plow Key*/~
            print$1,                     /* Print tickets or Sheets    */~
            printquanflag$1,             /* Print BOH Quan on Tickets  */~
            printquansys$1,              /* Print BOH Quan on Tickets  */~
            range_flag$5,                /* Print Range or ALL         */~
            readkey$100,                 /* Miscellaneous Read/Plow Key*/~
            session_date$8,              /* Planned Session Date       */~
            session_nbr$2,               /* Count Session Number       */~
            sheethdr$(2%)7,              /* Print Header Var on Sheets */~
            store$3,                     /* Store / Warehouse Code     */~
            supplemental$1,              /* Extra Ticket flag          */~
            ticket$15,                   /* Formatted Ticket Number    */~
            tickethdr$(2%)10,            /* Print Header Var on Tickets*/~
            to_ticket$12,                /* Print To Ticket            */~
            userid$3,                    /* Current User Id            */~
            uom$4,                       /* Unit of Measure            */~
            vol$6                        /* HNYPITKT Volume name       */

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
            cms2v$ = "R6.04.01 06/23/95 Patch finalization of R6.04.00  "
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
            * #4  ! HNYQUAN  ! Inventory Part / Store / Lot Quantity Fi *~
            * #7  ! HNYPICST ! Physical Inventory Costs Snapshot File   *~
            * #8  ! HNYPILOC ! Physical Inventory Location Snapshot File*~
            * #10 ! SYSFILE2 ! System Records File                      *~
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
                            key  2, keypos =  313, keylen =  16

            select #3,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #4,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #7,  "HNYPICST",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  46

            select #8,  "HNYPILOC",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos =    1, keylen =  44

            select #10, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos  =  1, keylen =  20

            call "SHOSTAT" ("Opening Files, One Moment Please")

            rslt$(1), rslt$(2), rslt$(3) = "REQUIRED"

            call "OPENCHCK" (#1 ,  fs%( 1%), f2%( 1%), 0%, rslt$( 1%))
            call "OPENCHCK" (#2 ,  fs%( 2%), f2%( 2%), 0%, rslt$( 2%))
            call "OPENCHCK" (#3 ,  fs%( 3%), f2%( 3%), 0%, rslt$( 3%))
            call "OPENCHCK" (#4 ,  fs%( 4%), f2%( 4%), 0%, rslt$( 4%))
            call "OPENCHCK" (#7 ,  fs%( 7%), f2%( 7%), 0%, rslt$( 7%))
            call "OPENCHCK" (#8 ,  fs%( 8%), f2%( 8%), 0%, rslt$( 8%))
            call "OPENCHCK" (#10,  fs%(10%), f2%(10%), 0%, rslt$(10%))

            if f2%(1%) + f2%(2%) + f2%(3%) > 0% then exit_program
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
            pf17$ = "(17)Set Print Flag"
            call "GETNAMES" addr(#2, file$, lib$, vol$)
            call "READFDR"  addr(file$,lib$,vol$,0%,"RC",records%,ret%)
            records% = max(1000%, records%)

            call "COMPNAME" (2%, company$, ret%)

*        See if User is a Module Administrator or not...
            call "CMSMACHK" ("SFC", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%
            if str(userid$,,2) = "RN" then admin% = 1%
            if admin% <> 1% then pf17$ = " "                             ~
                            else pf17$ = "(17)Set Sys Flag"

*        Load Program Switchs Record...
            call "READ100" (#10, "PI.PRINTQNT.FLAG", f1%(10%))
            if f1%(10%) = 1% then L09310
                put #10 using L09290, "PI.PRINTQNT.FLAG", "N", " "

L09290:             FMT CH(20), CH(1), CH(79)
                write #10
L09310:     get #10 using L09330, printquansys$

L09330:         FMT POS(21), CH(1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, infomsg$, session_nbr$,    ~
                      description$, range_flag$, from_ticket$, to_ticket$~
                     ,printquanflag$

            call "ALLFREE"
            first_field% = 1%
        reprint_entry_point
            pf16$ = "(16)Exit Program"
            pf4$  = " "

            for fieldnr% = first_field% to  5%
                if fieldnr% <> 1% then pf4$  = "(4)Previous Field"
L10100:         gosub'051(fieldnr%)
                      if enabled% = 0% then L10240
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(first_field%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit%  = 16% then       exit_program
                      if keyhit%  = 17% then gosub mod_sys_printflag
                      if keyhit% <>  0% then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10240:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
            pf4$  = " "
            pf16$ = "(16)Print"
            if print$ = "C" then pf16$ = pf16$ & " Sheets"               ~
            else pf16$ = pf16$ & " Tickets"
            inpmessage$ = "Press PF16 to Print Tickets or Count Sheets"

            gosub'101(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 17% then gosub mod_sys_printflag
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editmode
            fieldnr% = cursor%(1%) - 10%
            if fieldnr% < 2% or fieldnr% > 5% then editmode
            if fieldnr% < first_field% or fieldnr% > 5% then editmode
            gosub'051(fieldnr%)
                  if enabled% = 0% then       editmode
L11140:     gosub'101(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11140
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11140
            goto editmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if print$="C" then call "SHOSTAT" ("Now Printing Count Shee" ~
                          & "ts...")                                     ~
            else call "SHOSTAT" ("Now Printing Tickets...")
            if print$ = "C" then                                         ~
               call "SETPRNT" ("HNY007","HNYPI   ",records%,0%)          ~
            else                                                         ~
               call "SETPRNT" ("HNY008","HNYPI   ",records%*6%,0%)
            if print$ = "C" then select printer (082)                    ~
            else                 select printer (092)
            if from_ticket$ = "FIRST" then from_ticket$ = " "
            if range_flag$ = "ALL"                                       ~
                       then plowkey$ = str(session_nbr$) & "N"           ~
                       else plowkey$ = str(session_nbr$) & from_ticket$

            if range_flag$ = "ALL" then to_ticket$ = hex(ff)
            if to_ticket$ = "LAST" then to_ticket$ = hex(ff)
            l% = 0%                     /* Number of Lines Left on Page*/
            page% = 0%                  /* Page Counter (of course)    */
            if print$ = "C" then gosub print_count_sheets                ~
            else                 gosub print_single_tickets
            close printer
            call "SETPRNT" ("HNY007","HNYPI   ",records%, 1%)
            if page% > 0% then L19270
               call "ASKUSER" (keyhit%, "*** NOTHING PRINTED!! ***",     ~
                 "There were no ticket entries eligible for printing.",  ~
                 " ", "Press RETURN to Acknowlege this Message Please.")
               goto inputmode            /* Didn't Print a Thing !     */

L19270:     if print$ = "C" then inputmode
            keyhit% = 2%
            call "ASKUSER" (keyhit%, "***TICKETS PRINTED OK?***",        ~
              "Press RETURN If All the Tickets Printed Out Successfully",~
                                      "-OR-",                            ~
              "Press PF1 to Reprint a Selected Range of Tickets")
            if keyhit% = 0% then inputmode
            if keyhit% <> 1% then L19270
            range_flag$ = "RANGE"
            first_field% = 3%
            from_ticket$, to_ticket$ = " "
            goto reprint_entry_point

        mod_sys_printflag
            if admin% = 0% then return
            aukey% = 0%
            printquanold$ = printquanflag$
            if printquanflag$ <> " " then L19520
                printquanflag$ = printquansys$
L19520:     call "ASKUSER" (aukey%, "** SET PRINT QUANTITY FLAG **",     ~
                            "Print Quantity Flag is " & printquanflag$ & ~
                            ".",                                         ~
                            "Press PF(16) To Save as System Flag     ",  ~
                            "Press (RETURN) To Continue Without Save ")
            if aukey% = 16% then gosub save_sys_flag
            if aukey% <> 0% then goto L19520
            printquanflag$ = printquanold$
            return

        save_sys_flag
            call "READ101" (#10, "PI.PRINTQNT.FLAG", f1%(10%))
                if f1%(10%) = 0% then return /* Shouldn't Happen */
            put #10 using L09330, printquanflag$
            rewrite #10
            aukey% = 0%
            printquansys$ = printquanflag$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20130,         /* Count Session Num*/~
                                    L20170,         /* Print ALL or Rang*/~
                                    L20220,         /* Print From Ticket*/~
                                    L20280,         /* Print To Ticket  */~
                                    L20350          /* Print Quantity   */
                     return
L20130:     REM Default/Enable for Count Session Number
                inpmessage$ = "Enter the Count Session Nbr to Print or RE~
        ~TURN to Find"
                return
L20170:     REM Default/Enable for Print All or Range
                inpmessage$ = "Enter ALL to Print All Unprinted Tickets o~
        ~r RANGE to print Range below"
                enabled% = 0%
                if range_flag$ = " " then range_flag$ = "ALL"
                return
L20220:     REM Default/Enable for Print From Ticket
                if range_flag$ <> "RANGE" then enabled% = 0%
                inpmessage$ = "Enter the first Ticket Number to Print"
                if enabled% = 0% then return
                if from_ticket$ = " " then from_ticket$ = "FIRST"
                return
L20280:     REM Default/Enable for Print To Ticket
                if range_flag$ <> "RANGE" then enabled% = 0%
                inpmessage$ = "Enter the last Ticket Number to Print"
                if enabled% = 0% then return
                if to_ticket$ = " " then to_ticket$ = "LAST"
                return

L20350:     REM Default/Enable for Print Quantity on Tickets
            if printquanflag$ = " " then printquanflag$ = printquansys$
            if admin% <> 1% then enabled% = 0%
            inpmessage$ = "Enter 'Y' to Print Quantity on Ticket or 'N'" ~
                          & " to not print"
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
            u3% = 2%
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
            session_date$,  /* Planned Count Date                      */~
            print$          /* Print tickets or Count sheets  Indicator*/

            if print$ = "C" then infomsg$ = "(Count Sheets is the Selecte~
        ~d Output Format)"                                                ~
            else infomsg$ = "(Tickets are the Selected Output Format)"
            call "STRING" addr("CT", infomsg$, 79%)
            call "DATEFMT" (session_date$)
            return

        load_ticket
            get #2 using L35275,                                          ~
            ticket$,        /* Ticket Number to a Physical Inventory Co*/~
            recount%,       /* A number incremented for each recount (0*/~
            part$,          /* Part Number                             */~
            store$,         /* store number                            */~
            lot$,           /* lot number                              */~
            loc$,           /* bin location                            */~
            supplemental$   /* Supplemental Tag Flag                   */
            boh$, lflag$ = " "
            if part$ = lastpart$ then L30650
               call "DESCRIBE" (#3, part$, partdescr$, 0%, f1%(3))
               if f1%(3) = 1% then L30600
                  uom$ = " "
                  goto L30640
L30600:        get #3 using L30620, uom$
L30620:        FMT POS(74), CH(4)
L30640:        lastpart$ = part$
L30650:        if printquanflag$ <> "Y" then return
               /* Print Quantities if Requested */
               if loc$ <> " " then L30700
L30660:        readkey$ = str(session_nbr$) & str(part$) & str(store$) & ~
                          str(lot$)
               call "READ100" (#07, readkey$, f1%(07%))
               boh$ = " "
               if f1%(07%) = 0% then return
               get #07 using L30820, boh
               call "CONVERT" (boh, 0.2, boh$)
               return
L30700:        /* Location Quantity */
               readkey$ = str(session_nbr$) & str(part$) & str(store$) & ~
                          str(lot$,,6%) & str(loc$)
               call "READ100" (#08, readkey$, f1%(08%))
               boh$ = " "
               if f1%(08%) = 0% then L30660 /*BinLoc so don't use LOC Qty*/
               get #08 using L30830, boh
               call "CONVERT" (boh, 0.2, boh$)
               lflag$ = "L"
               return

L30820:     FMT POS(47), PD(14,4)    /* HNYQUAN File */
L30830:     FMT POS(45), PD(14,4)    /* HNYPILOC File */

        REM *************************************************************~
            *          P R I N T   C O U N T   S H E E T S              *~
            *-----------------------------------------------------------*~
            * Prints Count Sheets & Turns off Print Index (Alt Key 1).  *~
            *************************************************************
        print_count_sheets
            if range_flag$ = "ALL" then                                  ~
               call "PLOWAL1" (#2, plowkey$, 2%, 2%, f1%(2))             ~
            else                                                         ~
               call "PLOWNXT1" (#2, plowkey$, 2%, f1%(2))
            if f1%(2) = 0% then return
            if range_flag$ <> "ALL" then L31080
               str(plowkey$,1,15) = key(#2,2)
               if str(plowkey$,1,2) > session_nbr$ then return
               if str(plowkey$,3,1) = "Y" then print_count_sheets
L31080:     if str(plowkey$,3%,12%) > to_ticket$ then return
            if range_flag$ = "RANGE" then str(plowkey$,15%,1%) = hex(ff)
            gosub load_ticket
            if l% < 1% then gosub print_heading
            recount% = 99% - recount%
            if recount% = 0% then L31300
            ticket$ = ticket$ & "-"
            if recount% < 10% then convert recount% to                   ~
               str(ticket$,len(ticket$)+1%,1%), pic(0)                   ~
            else convert recount% to                                     ~
               str(ticket$,len(ticket$)+1%,2%), pic(00)

L31300:     print using L34210, ticket$, part$, lot$, store$, loc$, uom$

            if part$ = " " and supplemental$ = "X" then                  ~
               print using L34270                                         ~
            else                                                         ~
               print using L34230, partdescr$, boh$, lflag$

            print
            l% = l% - 3%
            put #2 using L31332, "Y"
L31332:            FMT POS(315), CH(1)
            rewrite #2
            goto print_count_sheets

        print_heading
            print page
            page% = page% + 1%
            print using L34051, date$, company$, page%
            print using L34060
            print using L34090, session_nbr$, session_date$
            print
            print using L34111
            print using L34120, sheethdr$(1%)
            print using L34150, sheethdr$(2%)
            print using L34180
            l% = 49%
            return

        REM *************************************************************~
            *          P R I N T   C O U N T   T I C K E T S            *~
            *-----------------------------------------------------------*~
            * Prints Tickets & Turns off Print Index (Alt Key 1).       *~
            *************************************************************
        print_single_tickets
            if l% < 1% then gosub print_alignment_tickets
            if range_flag$ = "ALL" then                                  ~
               call "PLOWAL1" (#2, plowkey$, 2%, 2%, f1%(2))             ~
            else                                                         ~
               call "PLOWNXT1" (#2, plowkey$, 2%, f1%(2))
            if f1%(2) = 0% then return
            if range_flag$ <> "ALL" then L32110
               str(plowkey$,1,15) = key(#2,2)
               if str(plowkey$,1,2) > session_nbr$ then return
               if str(plowkey$,3,1) = "Y" then print_single_tickets
L32110:     if str(plowkey$,3%,12%) > to_ticket$ then return
            page% = page% + 1%   /* Just so we know it printed sumthin */
            if range_flag$ = "RANGE" then str(plowkey$,15%,1%) = hex(ff)
            gosub load_ticket
            recount% = 99% - recount%
            if recount% = 0% then L32220
            ticket$ = ticket$ & "-"
            if recount% < 10% then convert recount% to                   ~
               str(ticket$,len(ticket$)+1%,1%), pic(0)                   ~
            else convert recount% to                                     ~
               str(ticket$,len(ticket$)+1%,2%), pic(00)
L32220:     gosub print_ticket
            put #2 using L32240, "Y"
L32240:            FMT POS(315), CH(1)
            rewrite #2
            goto print_single_tickets

        print_alignment_tickets
            init("X")ticket$, part$, partdescr$, store$, lot$, loc$, uom$
            boh$ = " "
            for x% = 1% to 3%
                gosub print_ticket
            next x%
            l% = 99%
            return

        print_ticket
            print using L34380, session_nbr$, ticket$
            if part$ = " " and supplemental$ = "X" then print_extra
            print skip(1)
            print using L34410, part$, uom$
            print skip(1)
            print using L34430, partdescr$
            print skip(1)
            print using L34480, store$
            print skip(1)
            print using L34510, lot$
            print skip(1)
            print using L34530, loc$
            print skip(2)
            print using L34550, tickethdr$(1%)
            print using L34550, tickethdr$(2%)
            print using L34550, boh$, lflag$
            print skip(5)
            return

        print_extra
            print skip(20)
            return

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L34051: % ######## ######################################################~
        ~######SHEET:####

L34060: % HNYPITIC                PHYSICAL INVENTORY COUNT SHEET         ~
        ~     RPT: HNY007

L34090: %                FOR COUNT SESSION: ##   TO BE COUNTED BY: ######~
        ~##

L34111: % Counted By:_____________________ Date:___________

L34120: %                                                                ~
        ~####### Quantity

L34150: % Ticket Number   Part Code / Description   Lot    Whse Location ~
        ~####### Counted

L34180: % --------------- ------------------------- ------ ---- -------- ~
        ~------- --------

L34210: % ############### ######################### ###### ###  ######## ~
        ~  ####
L34230: %                 ################################          #####~
        ~##### # ________

L34270: %                 _________________________ ______ ____ ________ ~
        ~_______ ________

        REM *************************************************************~
            *              T I C K E T   F O R M A T S                  *~
            *-----------------------------------------------------------*~
            * IMAGE Statements for Tickets.                             *~
            *************************************************************

L34380: %                       ##                                       ~
        ~    ############

L34410: %                     #########################                  ~
        ~               ####

L34430: %                     ################################           ~

L34480: %                     ###

L34510: %                     ######

L34530: %                     ########

L34550: %                                                      ##########~
        ~ #

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: HNYPISYS                          */~
            CH(06),         /* Session Date                            */~
            POS(344),                                                    ~
            CH(1)           /* Print Tickets (T) or Count Sheets (C)   */

L35275: FMT                 /* FILE: HNYPITKT                          */~
            XX(2),          /* Number corresponding to a Inventory Coun*/~
            CH(12),         /* Ticket Number to a Physical Inventory Co*/~
            BI(1),          /* A number incremented for each recount (0*/~
            CH(25),         /* Part Number                             */~
            CH(3),          /* store number                            */~
            CH(16),         /* lot number                              */~
            CH(8),          /* bin location                            */~
            CH(1)           /* Supplemental flag                       */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'101(fieldnr%)
                  line2$ = "Generate Count Sheets"
                  str(line2$,62%) = "HNYPITIC: " & str(cms2v$,,8%)
*                INIT(HEX(84)) LFAC$()
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  on fieldnr% gosub L40170,         /* Count Session Num*/~
                                    L40170,         /* Print All/Range  */~
                                    L40170,         /* Print From Ticket*/~
                                    L40170,         /* Print To Ticket  */~
                                    L40170          /* Print Quantity   */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
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
                  "Print Physical Inventory Tickets /",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
        "  This program will print tickets or count sheets (as previously~
        ~ selected) for",                                                 ~
               at (06,02),                                               ~
        "  the given Count Session.  When you first run this it will prin~
        ~t all  ",                                                        ~
               at (07,02),                                               ~
        "  tickets in a session which have not yet been printed (this inc~
        ~ludes new  ",                                                    ~
               at (08,02),                                               ~
        "  recount tickets & extra tickets).  After that you have the opt~
        ~ion to ",                                                        ~
               at (09,02),                                               ~
        "  reprint a selected range of tickets if needed.",              ~
               at (11,02),                                               ~
        "  Count Session Number      :",                                 ~
               at (11,32), fac(lfac$(1)) , session_nbr$         , ch(02),~
               at (11,35), fac(hex(8c))  , description$         , ch(32),~
               at (12,02),                                               ~
        "  Print ALL Tickets or RANGE:",                                 ~
               at (12,32), fac(lfac$(2)) , range_flag$          , ch(05),~
               at (13,02),                                               ~
        "  If RANGE Print FROM Ticket:",                                 ~
               at (13,32), fac(lfac$(3)) , from_ticket$         , ch(12),~
               at (14,02),                                               ~
        "                   TO Ticket:",                                 ~
               at (14,32), fac(lfac$(4)) , to_ticket$           , ch(12),~
               at (15,02),                                               ~
        "  Print Quantity on Ticket  :",                                 ~
               at (15,32), fac(lfac$(5)) , printquanflag$       , ch(01),~
                                                                         ~
               at (18,02), fac(hex(84)),   infomsg$             , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), "(1)Start Over",                              ~
               at (23,25), fac(hex(8c)), pf4$                   , ch(17),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), fac(hex(8c)), pf17$                  , ch(16),~
               at (24,65), fac(hex(84)), pf16$                  , ch(16),~
               keys(hex(0001040d0f1011)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40650
                  call "MANUAL" ("HNYPITIC")
                  goto L40240

L40650:        if keyhit% <> 15 then L40690
                  call "PRNTSCRN"
                  goto L40240

L40690:        if fieldnr% > 0% then return
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
                                    L50191,         /* Print ALL/RANGE  */~
                                    L50200,         /* Print From Ticket*/~
                                    L50300,         /* Print To Ticket  */~
                                    L50430          /* Print Quantity   */
                     return
L50100:     REM Test Data for Count Session Number
                call "GETCODE" (#1, session_nbr$,description$     , 1%,  ~
                                0, f1%(1))
                if f1%(1) = 0% then  L50180
                if f2%(4) <> 0% then L50170 /* No HNYQUAN so not check on*/
                                                       /* HNYPICST File */
                plowkey$ = str(session_nbr$)
                call "PLOWNEXT" (#7, plowkey$, 2%, f1%(7))
                if f1%(7) = 1% then L50170
                     goto no_cost_file   /* Gota leave */
L50170:              gosub dataload  :  return
L50180:         errormsg$ =  "Session Undefined or None on File"
                return
L50191:     REM Test Data for Print ALL or RANGE
                if range_flag$ = "ALL" or range_flag$="RANGE" then return
                errormsg$ = "Must be ALL or RANGE"
                return
L50200:     REM Test Data for Print From Ticket
                if from_ticket$ = "FIRST" then return
                plowkey$=str(session_nbr$) & str(from_ticket$) & bin(99,1)
                call "READ100" (#2, plowkey$, f1%(2))
                if f1%(2) = 1% then return
                errormsg$ = "Ticket Number Not on File!"
                return
L50300:     REM Test Data for Print To Ticket
                if to_ticket$ = "LAST" then return
                plowkey$ = str(session_nbr$) & str(to_ticket$) & bin(99,1)
                call "READ100" (#2, plowkey$, f1%(2))
                if f1%(2) = 1% then L50350
                errormsg$ = "Ticket Number Not on File!"
                return
L50350:         if to_ticket$ >= from_ticket$ then return
                if from_ticket$ = "FIRST" then return
                errormsg$ = "TO Ticket CANNOT be less than FROM Ticket!"
                return
                gosub dataload   :  return
                errormsg$ =    "SESSION UNDEFINED OR NONE ON FILE"
                return

L50430:     REM Test Data for Print Quantity on Ticket
            p% = pos("YN" = printquanflag$)
            on p% + 1%   goto L50460, L50470, L50475
L50460:         errormsg$ = "Select 'Y' or 'N' for Print Quantity Flag."
                    return
L50470:         sheethdr$(1%)  = "  UOM /" :  sheethdr$(2%) = "BOH QTY"
                tickethdr$(1%) = "BOH QTY" :tickethdr$(2%) = "-----------"
                    return
L50475:         sheethdr$(1%)  = " "       :  sheethdr$(2%) = "  UOM  "
                tickethdr$(1%) = " "       : tickethdr$(2%) = " "
                    return

         no_cost_file
            keyhit% = 2%
            call "ASKUSER" (keyhit%, "** NO COST PICTURE FILE **",       ~
                 "No Cost Picture has been done for this session."     , ~
                 "You must capture the current cost vie HNYPICAP."     , ~
                 "Press Any Key to Exit    "  )
            goto  exit_program

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
