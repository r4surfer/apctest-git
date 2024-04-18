        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K   AAA    GGG   IIIII  N   N   GGG    *~
            *  B   B  C   C  K  K   A   A  G        I    NN  N  G       *~
            *  BBBB   C      KKK    AAAAA  G GGG    I    N N N  G GGG   *~
            *  B   B  C   C  K  K   A   A  G   G    I    N  NN  G   G   *~
            *  BBBB    CCC   K   K  A   A   GGG   IIIII  N   N   GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKAGING - Print report of aged open sales order dollars. *~
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
            * 02/03/87 ! Original                                 ! MJB *~
            * 08/19/87 ! Fixed bugs regarding open qty determintn ! ERN *~
            *          !  and aging.                              !     *~
            * 07/29/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            age_date$6,                  /* Date for item aging        */~
            aging%(10), aging$(7)20,     /* Aging Parameters           */~
            begin_date$8,                /* Based on ? Date            */~
            by$25,                       /* Part or ? for workfile     */~
            bydescr$32,                  /* Description of BY$         */~
            caldate$6,                   /* Planning Cal Start Date    */~
            col$(4)8,                    /* Columnar parametrics       */~
            company$60,                  /* Company name for header    */~
            cursor%(2),                  /* Cursor location for edit   */~
            cusacct$9,                   /* Customer Account Code      */~
            cusbill$9,                   /* Customer Bill-To           */~
            cuscode$9,                   /* Customer Ship-To           */~
            date$8,                      /* Date for screen display    */~
            datedescr$28,                /* Based-on message           */~
            days$(8)4,                   /* Day for report header      */~
            dollars(6),                  /* Accumulators               */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* Sortfile name              */~
            fromnr$25,                   /* For Range Test             */~
            how$20,                      /* How sorted for header      */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* Sortfile library           */~
            line2$79,                    /* Second Line of Screen Headr*/~
            mfac$1,                      /* Message Fac                */~
            month$(12)3,                 /* Month Abbreviations        */~
            msg$(4)75,                   /* Input messages             */~
            part$25,                     /* Part Number                */~
            partdescr$32,                /* Part Description           */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf8$19,                      /* PF 8 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            seldescr$80,                 /* For Report Header          */~
            sonbr$16,                    /* Sales Order Number         */~
            soduedate$8,                 /* SO Due Date                */~
            soshipdate$8,                /* SO Ship Date               */~
            sort$116,                    /* Argument for SORTCALL      */~
            sortby$1,                    /* Print Report By            */~
            sortdescr$35,                /* Sort Description           */~
            start_range$(1)25,           /* Range Start                */~
            stop_range$(1)25,            /* Range Stop                 */~
            testdate$8,                  /* Date for aging calc        */~
            time$8,                      /* Time for header            */~
            tonr$25,                     /* For Range Test             */~
            totalds(6),                  /* Total Accumulators         */~
            ttl$(2)16,                   /* Title literals             */~
            use_date$1,                  /* Based on ? Date - S or D   */~
            vol$6,                       /* Sortfile Volume            */~
            what$20,                     /* On What date for header    */~
            userid$3                     /* Current User Id            */~

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * # 1 ! BCKLINES ! Backlog Line Item File                   *~
            * # 2 ! BCKMASTR ! Backlog Master File                      *~
            * # 3 ! PIPOUT   ! Planned Inventory Usage Detail File      *~
            * # 4 ! CUSTOMER ! Customer Master File                     *~
            * # 5 ! WORKFILE ! Work File                                *~
            * # 6 ! CALMASTR ! Production Planning Calendar             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select # 2, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select # 3, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select # 4, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup     ~

            select # 5, "WORKFILE",                                      ~
                        varc,  consec,  recsize =  100

            select # 6, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))

            recnbr% = val(str(rslt$(1),17,4),4)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "READ100" (#6,"10", f1%( 6))
                if f1%(6) = 0% then big_trouble
            get #6, using L09150, caldate$
L09150:         FMT XX(2), CH(6)

            month$( 1) = "JAN" : month$( 2) = "FEB" : month$( 3) = "MAR"
            month$( 4) = "APR" : month$( 5) = "MAY" : month$( 6) = "JUN"
            month$( 7) = "JUL" : month$( 8) = "AUG" : month$( 9) = "SEP"
            month$(10) = "OCT" : month$(11) = "NOV" : month$(12) = "DEC"

            select printer(134)

            call "COMPNAME" (12%, company$, err%)
            time$ = " " : call "TIME" (time$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$, pf8$ = " "  :  pf16$ = "(16)Exit Program"
            gosub L29000

            for fieldnr% = 1% to  4%
                if fieldnr% > 1% then pf4$ = "(4)Previous Field"
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <>  0% then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

            call "ARMDATES" (testdate$,   "Aged Open Orders Report",     ~
                             aging%(), 2%, 0%, aging$(), u3%)
            if u3% = 1% then inputmode  /* Startover at sub */

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$ = " "  :  pf16$ = "(16)Print Report"
            pf8$ = "(8)Aging Parameters"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then gosub aging_parameters
                  if keyhit%  = 16% then       L12000
                  if keyhit% <>  0% then       editpg1
L11160:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf16$ = " "
L11220:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11220
                  lastfieldnr% = fieldnr%
            goto L11160

        aging_parameters
            call "ARMDATES" (testdate$,   "Aged Open Orders Report",     ~
                             aging%(), 2%, 0%, aging$(), u3%)
            if u3% <> 1% then return
                return clear all
                goto   inputmode

L12000: REM *************************************************************~
            *             M A I N   L O G I C   S E C T I O N           *~
            *-----------------------------------------------------------*~
            * First the Extract and write the work file                 *~
            *************************************************************
            call "SHOSTAT" ("Extracting Information")
            call "WORKOPN2" (#5, "OUTPT", recnbr%/2%, f2%(5))
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            mat totalds = zer

        read_bcklines
            call "READNEXT" (#1, f1%(1))
                if f1%(1) = 0% then sort_it
            get #1 using L12170, cuscode$, sonbr$, seqnbr$, part$,        ~
                   partdescr$, openqty, pi_qty, unitprice, discpct,      ~
                   soduedate$, soshipdate$

L12170:     FMT   /* BCKLINES File */                                    ~
                CH(9), CH(16), CH(3), POS(32), CH(25), CH(32), POS(109), ~
                PD(14,4), POS(133), 2*PD(14,4), POS(173), PD(14,4),      ~
                POS(206), 2*CH(6)
            openqty = max(0, openqty - pi_qty) /* less pre-invoiced    */
            if openqty <= 0 then read_bcklines

            gosub check_eligible
            if eligible% = 0% then read_bcklines

*        We found one that qualifies...
*        First we calculate the NET $$ and then write the workfile
            gross = openqty * unitprice
            net = gross - (gross * discpct/100)
            write #5 using L12320, by$, bydescr$, age_date$, net
L12320:         FMT CH(25), CH(32), CH(6), PD(14,4)
            goto read_bcklines

        sort_it
*        1st get the workfile name and location
            call "SHOSTAT" ("Sorting Report Records")
            call "GETNAMES" addr(#5, file$, lib$, vol$)
            close #5

            sort$ = file$
            str(sort$, 9, 8) = lib$
            str(sort$,17, 6) = vol$
            str(sort$,23,22) = str(sort$,,22)
            str(sort$,45, 9) = "0001025CA"

            call "SORTCALL" addr(sort$, err%)
                if err% <> 0% then big_trouble

            call "WORKOPN2" (#5, "INPUT", 0%, f2%(5))
                if f2%(5) = 1% then big_trouble

*        Now we can read it all back in and make a report of it!
            call "SHOSTAT" ("Printing Report...")
            lcntr% = 99%
            part$, partdescr$, by$, bydescr$, age_date$ = " "
            net = 0
            if sortby$ <> "P" then L12620
            how$ = "Part Number"
            ttl$(1) = "Part Number" : ttl$(2) = "Part Description"
            goto L12660
L12620:     if sortby$ = "B" then how$ = "Bill-To Customer"
            if sortby$ = "S" then how$ = "Ship-To Customer"
            if sortby$ = "C" then how$ = "Reference Customer"
            ttl$(1) = "Customer Number" : ttl$(2) = "Customer Name"
L12660:     if use_date$ = "P" then what$ = "Planned Ship Date"
            if use_date$ = "S" then what$ = "Required Ship Date"
            if use_date$ = "D" then what$ = "Sales Order Due Date"

            seldescr$ = "Reported by " & how$ & " Using " & what$ &      ~
                        " Aged from " & begin_date$

            daycnt% = 1%
            for z% = 1% to 4%
                convert aging%(z%) to days$(daycnt%), pic(-###)
                convert aging%(z%) + 1% to days$(daycnt% + 1%), pic(-###)
                daycnt% = daycnt% + 2%
            next z%

            for z% = 1% to 4%
                col$(z%) = str(aging$(z%),13%)
                call "DATUNFMT" (col$(z%))
            next z%

            gosub print_page_0

        read_loop
            call "READNEXT" (#5, f1%(5))
                if f1%(5) = 0% then end_report
            get #5 using L12320, by$, bydescr$, age_date$, net
            if sortby$ = "P" then L12890
            call "DESCRIBE" (#4, by$, bydescr$, 0%, f1%(4))
L12890:     if by$ <> part$ then gosub print_line

            if age_date$ > col$(1%) then L12940
                dollars(1%) = dollars(1%) + net
                goto read_loop
L12940:     if age_date$ > col$(2%) then L12970
                dollars(2%) = dollars(2%) + net
                goto read_loop
L12970:     if age_date$ > col$(3%) then L13000
                dollars(3%) = dollars(3%) + net
                goto read_loop
L13000:     if age_date$ > col$(4%) then L13040
                dollars(4%) = dollars(4%) + net
                goto read_loop

L13040:         dollars(5%) = dollars(5%) + net
                goto read_loop


        print_line
            if part$ = " " then L14090
            if lcntr% > 56% then gosub page_head
            dollars(1%) = round(dollars(1%),0)
            dollars(2%) = round(dollars(2%),0)
            dollars(3%) = round(dollars(3%),0)
            dollars(4%) = round(dollars(4%),0)
            dollars(5%) = round(dollars(5%),0)
            dollars(6%) = round(dollars(1%) + dollars(2%) + dollars(3%) +~
                                dollars(4%) + dollars(5%),0)
            print using L31100, part$, partdescr$, dollars(1), dollars(2),~
                          dollars(3), dollars(4), dollars(5), dollars(6)

            for i% = 1% to 6%
                totalds(i%) = totalds(i%) + dollars(i%)
            next i%

            lcntr% = lcntr% + 1%
L14090:     mat dollars = zer
            part$ = by$  : partdescr$ = bydescr$
            return

        end_report
*          Do some totaling and print some total lines in here
            gosub print_line
            print using L31300
            print using L31500, totalds(1), totalds(2), totalds(3),       ~
                               totalds(4), totalds(5), totalds(6)
            print skip(2)
            time$ = " " : call "TIME" (time$)
            print using L31660, time$
            close #1 : close printer
            call "FILEBGON" (#5)
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************
                                 dollars(4) = dollars(4) + net
        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20150,         /* Print Report By    */    ~
                              L20270,         /* Selection Range    */    ~
                              L20340,         /* Based on ? Date    */    ~
                              L20450          /* Begin Date         */
            return

L20150: REM Def/Enable Print Report By             SORTBY$
            if sortby$ = " " then sortby$ = "S"
            inpmessage$ = "Enter 'P' for Part, 'C' for Customer, 'B' for ~
        ~Bill-To or 'S' for Ship-To"
            msg$(1) = "Enter 'P' to Report Backlog by Part Number"
            msg$(2) = "Enter 'S' to Report Backlog by Ship-To Customer"
            msg$(3) = "Enter 'B' to Report Backlog by Bill-To Customer"
            msg$(4) = "Enter 'C' to Report Backlog by Reference Account N~
        ~umber"
            mfac$ = hex(8c)
            return

L20270: REM Def/Enable Selection Range   START_RANGE$ and STOP_RANGE$
            mfac$ = hex(9c)
            if start_range$(1) = " " then start_range$(1) = "ALL"
            inpmessage$ = "Enter Beginning and Ending Numbers for Range S~
        ~election"
            return

L20340: REM Def/Enable Based on ? Date             USE_DATE$
            if use_date$ = " " then use_date$ = "S"
            inpmessage$ = " "

            msg$(1) = "Enter 'S' to Base Aging on Sales Order Ship Date"
            msg$(2) = "Enter 'D' to Base Aging on Sales Order Due Date"
            msg$(3) = "Enter 'P' to Base Aging on Planned Ship Date"
            msg$(4) = " "
            mfac$ = hex(8c)
            return

L20450: REM Def/Enable Beginning Date              BEGIN_DATE$
            mfac$ = hex(9c)
            if begin_date$ <> " " then L20500
                call "DATEFMT" ( date, 0%, begin_date$ )
                str(begin_date$,7%,2%) = "01"
                call "DATEFMT" (begin_date$)
L20500:     inpmessage$ = "Enter a Date to Age FROM."
            return


L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, sortdescr$,                ~
                      sortby$                   , /* Print Report By */  ~
                      start_range$(1)           , /* Sel Range Hi    */  ~
                      stop_range$(1)            , /* Sel Range Lo    */  ~
                      begin_date$               , /* Begin Date      */  ~
                      use_date$                 , /* Based on ? Date */  ~
                      aging$()                    /* Aging Dates     */

            mat aging% = zer
                aging%(1) =  -1% : aging%( 2) =  30% : aging%(3) =  60%
                aging%(4) =  90% : aging%(10) =   5%
                aging%(8) = -99999%  :  aging%(5), aging%(9) = 99999%

            mat redim start_range$(1)25
            mat redim stop_range$(1)25
            mfac$ = hex(8c)
            time$ = " " : call "TIME" (time$)
            pcntr% = 0%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            *       R E P O R T   I M A G E   S T A T E M E N T S       *~
            *-----------------------------------------------------------*~
            * Image Statements for Report Print Lines                   *~
            *************************************************************
L30205: %RUN DATE: ######## @ ########      #############################~
        ~###############################                    BCKAGING:BCK00~
        ~9

L30225: %                                                 OPEN SCHEDULED ~
        ~SHIPMENTS REPORT                                       PAGE:####


L30300: %                              ##################################~
        ~##############################################

L30500: %                                                           * - -~
        ~ - - - - - - V a l u e   t o   S h i p - - - - - - - - *


L30700: %################          ################                 Befor~
        ~e #### #### to #### #### to #### #### to ####  Over ####       To~
        ~tal

L30900: %------------------------- -------------------------------- -----~
        ~------ ------------ ------------ ------------ ---------- --------~
        ~---

L31100: %######################### ################################  -###~
        ~######   -#########   -#########   -######### -######### -#######~
        ~###

L31300: %                                                           -----~
        ~------ ------------ ------------ ------------ ---------- --------~
        ~---

L31500: %                                       ***** TOTALS *****   -###~
        ~######   -#########   -#########   -######### -######### -#######~
        ~###

L31660: %                      ***** END OF REPORT @ ######## *****
L31670: %                               #################################~
        ~###############################################

        REM *************************************************************~
            *        S U B R O U T I N E S .............                *~
            *-----------------------------------------------------------*~

        check_eligible
            by$, bydescr$ = " "
            eligible% = 0%   /* Set to Don't want till find we do */
            if sortby$ = "B" or sortby$ = "C" then need_customer
            if sortby$ = "S" then check_ship
            if part$ = " " then return
            if part$ < fromnr$ or part$ > tonr$ then return
            by$ = part$  :  bydescr$ = partdescr$
            goto it_is

        check_ship
            if cuscode$ < fromnr$ or cuscode$ > tonr$ then return
            by$ = cuscode$
            goto it_is

        need_customer
            call "READ100" (#4, cuscode$, f1%(4))
                if f1%(4) = 0% then big_trouble
            get #4 using L35230, cusacct$, cusbill$
L35230:         FMT POS(771), 2*CH(9)
            if sortby$ <> "B" then check_acct
            if cusbill$ < fromnr$ or cusbill$ > tonr$ then return
            by$ = cusbill$
            goto it_is

        check_acct
            if cusacct$ < fromnr$ or cusacct$ > tonr$ then return
            by$ = cusacct$

        it_is
            eligible% = 1%
            if use_date$ = "P" then plan_date
            if use_date$ = "S" then age_date$ = soshipdate$
            if use_date$ = "D" then age_date$ = soduedate$
            return

        plan_date
            init(hex(00))plowkey$
            str(plowkey$,,16) = sonbr$ : str(plowkey$,17,3) = seqnbr$
            str(plowkey$,20,25) = part$
            call "PLOWNEXT" (#3, plowkey$, 44%, f1%(3))
                if f1%(3) = 1% then L35470
            eligible% = 0% : return
L35470:     get #3 using L35480, date%
L35480:           FMT XX(44), BI(4)
            call "DATE" addr("G+", caldate$, date%, age_date$, err%)
            return

        page_head
            pcntr% = pcntr% + 1%
            lcntr% = 5%
        page_0_heading
            print page
            print using L30205, date$, time$, company$
            print using L30225, pcntr%
            if pagesw% <> 0% then return
            print using L30300, seldescr$
            print skip(2)
            print using L30500
            print using L30700, ttl$(1), ttl$(2), days$(2%), days$(2%),   ~
                               days$(3%), days$(4%), days$(5%),          ~
                               days$(6%), days$(7%), days$(7%)
            print using L30900
            lcntr% = 5%
            return

        print_page_0
            pagesw% = 1%
            gosub page_0_heading
            pagesw% = 0%
L37050:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L37090
                str(i$(), i%, 1%) = " "
                goto L37050
L37090:     print skip (4)
            print using L31670, "            ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 14%
                print using L31670, i$(n%)   /* Selection Screen lines */
            next n%
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,62%) = "BCKAGING: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40340,         /* Print Report By   */   ~
                                L40340,         /* Selection Range   */   ~
                                L40340,         /* Based on ? Date   */   ~
                                L40320          /* Begin Date        */
              goto L40400

L40320:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40340:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40400:     accept                                                       ~
               at (01,02),                                               ~
                  "Aged Open Orders Report",                             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Print Report By",                            ~
               at (06,20), fac(lfac$( 1)), sortby$              , ch(01),~
               at (06,30), fac(hex(8c)),   sortdescr$           , ch(35),~
                                                                         ~
               at (07,02), "Selection Range",                            ~
               at (07,20), fac(lfac$( 2)), start_range$(1)      ,        ~
               at (07,52), fac(lfac$( 2)), stop_range$(1)       ,        ~
                                                                         ~
               at (08,02), "Using What Date?",                           ~
               at (08,20), fac(lfac$( 3)), use_date$            , ch(01),~
               at (08,30), fac(hex(8c)),   datedescr$           , ch(28),~
                                                                         ~
               at (09,02), "Age From Date",                              ~
               at (09,20), fac(lfac$( 4)), begin_date$          , ch(08),~
                                                                         ~
               at (16,05), fac(mfac$) ,    msg$(1)              , ch(75),~
               at (17,05), fac(mfac$) ,    msg$(2)              , ch(75),~
               at (18,05), fac(mfac$) ,    msg$(3)              , ch(75),~
               at (19,05), fac(mfac$) ,    msg$(4)              , ch(75),~
                                                                         ~
               at (10,02), "Aging Parameters:",                          ~
               at (10,20), "1)", at(11,20), "2)", at(12,20), "3)",       ~
               at (13,20), "4)", at(14,20), "5)", at(10,30), "Days",     ~
               at (10,23), fac(lfac$(5%)), aging%(1%)       ,pic(-#####),~
               at (11,23), fac(lfac$(5%)), aging%(2%)       ,pic(-#####),~
               at (12,23), fac(lfac$(5%)), aging%(3%)       ,pic(-#####),~
               at (13,23), fac(lfac$(5%)), aging%(4%)       ,pic(-#####),~
               at (14,23), fac(lfac$(5%)), aging%(5%)       ,pic(-#####),~
               at (10,37), fac(lfac$(5%)), aging$(1%)           , ch(20),~
               at (11,37), fac(lfac$(5%)), aging$(2%)           , ch(20),~
               at (12,37), fac(lfac$(5%)), aging$(3%)           , ch(20),~
               at (13,37), fac(lfac$(5%)), aging$(4%)           , ch(20),~
               at (14,37), fac(lfac$(5%)), aging$(5%)           , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,35), fac(hex(8c)), pf8$                           ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(00010405080d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13% then L41120
                  call "MANUAL" ("BCKAGING")
                  goto L40400

L41120:        if keyhit% <> 15% then L41200
                  call "PRNTSCRN"
                  goto L40400

L41200:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,         /* Print Report By   */     ~
                              L50350,         /* Selection Range   */     ~
                              L50400,         /* Based on ? Date   */     ~
                              L50460          /* Beginning Date    */
            return

L50140: REM Test for Print Report By              SORTBY$
            sortdescr$ = " "
            if sortby$ <> "P" then L50210
                mat redim start_range$(1)25
                mat redim stop_range$(1)25
                sortdescr$ = "(Report By Part Number)"
                return
L50210:     mat redim start_range$(1)9
            mat redim stop_range$(1)9
            if sortby$ <> "C" then L50260
                sortdescr$ = "(Report By Reference Account)"
                return
L50260:     if sortby$ <> "B" then L50290
                sortdescr$ = "(Report By Bill-To Customer)"
                return
L50290:     if sortby$ <> "S" then L50320
                sortdescr$ = "(Report By Ship-To Customer)"
                return
L50320:     errormsg$ = "Must be 'P', 'C', 'B' or 'S'"
            return

L50350: REM Test for Selection Range      START_RANGE$ and STOP_RANGE$
            call "TESTRNGE" (start_range$(1), stop_range$(1),            ~
                             fromnr$, tonr$, errormsg$)
            return

L50400: REM Test for Based on ? Date              USE_DATE$
            if use_date$ <> "S" then L50408
                datedescr$ = "(Based on SO Ship Date)"
                return
L50408:     if use_date$ <> "D" then L50414
                datedescr$ = "(Based on SO Due Date)"
                return
L50414:     if use_date$ <> "P" then L50430
                datedescr$ = "(Based on Planned Ship Date)"
                return
L50430:     errormsg$ = "Must enter 'S', 'D' or 'P'"
            return

L50460: REM Test for Beginning Date               BEGIN_DATE$
            call "DATEOK" (begin_date$, date%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (begin_date$)
*              STR(BEGIN_DATE$,5,2) = "01"
                call "DATEFMT" (begin_date$, 0%, testdate$)
                convert str(testdate$,5%,2%) to a%
                call "DATECONV" (testdate$)
                b% = a% + 1%  :  if b% > 12% then b% = b% - 12%
                c% = a% + 2%  :  if c% > 12% then c% = c% - 12%
                return

        big_trouble

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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "FILEBGON" (#5)
            call "SHOSTAT" ("One Moment Please")

            end
