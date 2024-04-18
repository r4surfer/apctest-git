        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  L       OOO   TTTTT  M   M  V   V  RRRR   PPPP   TTTTT   *~
            *  L      O   O    T    MM MM  V   V  R   R  P   P    T     *~
            *  L      O   O    T    M M M  V   V  RRRR   PPPP     T     *~
            *  L      O   O    T    M   M   V V   R   R  P        T     *~
            *  LLLLL   OOO     T    M   M    V    R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * LOTMVRPT - Reports Part Movement from Place to Place in   *~
            *            Typical Parent/Component Implosion/Explosion   *~
            *            Fashion.  'Variable' Key formats are trickey.  *~
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
            * 06/19/87 ! Original                                 ! KEN *~
            * 04/03/89 ! Added Right Justify to PO LINE#          ! MLJ *~
            * 03/16/93 ! PRR 12578 - Use current LOTMVNT file or  ! RJH *~
            *          !  an Archived (by year) File.             !     *~
            *          ! PRR 12487, 12787(3,5) Add range selection!     *~
            *          !  for starting type variables w/ PLOWCODE !     *~
            *          !  available to the 1st variable elements. !     *~
            *          ! PRR 12487, 12585, 12787(4) - Add print   !     *~
            *          !  mapping to report to limit whats printed!     *~
            *          ! PRR 12787(2) - Added option to stop the  !     *~
            *          !  report chain when a DIRECT is encoutered!     *~
            *          ! Added Page 0 and 'END of Report @' to the!     *~
            *          !  Report.                                 !     *~
            *          ! Some implied integers set to integer type!     *~
            * 04/26/93 ! Add Date Range selection option.         !     *~
            * 09/20/93 ! Use SUCCESS% test for Explode validity.  ! RJH *~
            *          ! Add option to Break Chain at Misc Part.  !     *~
            *          !  for source and application report.      !     *~
            * 08/14/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            answer$(4,4)30, ansdescr$30, /* Variable Response Fields   */~
            arcyear$7,                   /* Archived Year Litteral     */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            break_direct$1,              /* Break Chain at DIRECT Flag */~
            break_misc$1,                /* Break Chain at Misc Part   */~
            breakmsg$44,                 /* Break Chain Print Message  */~
            choice$4,                    /* PICKYEAR return choice     */~
            company$60,                  /* Company Name               */~
            component$44,                /* Component for Explosion    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$( 7)30,                /* Descriptions for types     */~
            detail$1,                    /* Print Detail?              */~
            detdate$8,                   /* Date of Detail Record      */~
            detkey$99,                   /* Key to LOTMVDTL            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fileid$4,                    /* File ID for PICKYEAR       */~
            fmdate$10,                   /* From Date                  */~
            hidate$10,                   /* High Date                  */~
            hirng$(3)30,                 /* MAX Ranges of Plowkey      */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(10)1,                  /* Field Attribute Characters */~
            lfcd$( 8)1,                  /* Field Attribute Characters */~
            lfcp$(5)1,                   /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lodate$10,                   /* Low Date                   */~
            lotdprname$8,                /* Current LOTMVDTL File Name */~
            lotmprname$8,                /* Current LOTMVMNT File Name */~
            lowrng$(3)30,                /* MIN Ranges of Plowkey      */~
            origpart$25,                 /* Original Part to track     */~
            p%(30),                      /* Dup Counter                */~
            p$(30)88,                    /* Part Number to Track       */~
            page_break$1,                /* Page Break Flag            */~
            part$25,                     /* Part Code                  */~
            partdescr$32,                /* Part Code                  */~
            partmoved$32,                /* Type of Move 'C' or 'P'    */~
            pcode$(30)1,                 /* Movement type (HPJXVDCA)etc*/~
            phead$90,                    /* Report Header              */~
            phead_orig$90,               /* Report Header              */~
            pf4$25,                      /* PF 4 Screen Literal        */~
            pf5$25,                      /* PF 5 Screen Literal        */~
            pf$(3)79,                    /* PF   Screen Literal        */~
            pfkeys$32,                   /* PF Key HEX Value           */~
            pline$50,                    /* Print Line                 */~
            plowdescr$60,                /* Plowcode Description       */~
            plowhdr$(3)60,               /* Plowcode Header Array      */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey1$99,                 /* Miscellaneous Read/Plow Key*/~
            plow_exp$99,                 /* Start Plowkey for Explode  */~
            includ_exclud$(2)79,         /* Include/Exclude fr PLOWCODE*/~
            includ_exclud(2),            /* Include/Exclude fr PLOWCODE*/~
            print$112,                   /* Print Line                 */~
            print$( 8)1,                 /* Print what movement info.  */~
            prompt$( 8,4)17,             /* Prompt Literals            */~
            promptl%( 8,4),              /* Prompt Lengths             */~
            prompts%( 8),                /* Prompt (How Many)          */~
            qty$10,                      /* Edited Quantities          */~
            rnglen%(3),                  /* Length of range elements   */~
            testparts$(31)25,            /* Parts to test Break on Misc*/~
            temptest$88,                 /* Temp Test Range String     */~
            type$1,                      /* Movement Type Code         */~
            todate$10,                   /* To Date                    */~
            typeshdr$(2)25,              /* Start Pt. & Types Scrn Hdr */~
            time$8,                      /* Time                       */~
            trandate$8,                  /* Date Record Was Written    */~
            selecthdr$(2)25,             /* Range Selection Screen Hdr */~
            userid$3,                    /* Current User Id            */~
            which$( 9)1                  /* Fill in the Box            */~

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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #10 ! VENDOR   ! Vendor Master File                       *~
            * #11 ! JBMASTR2 ! Production job master file               *~
            * #12 ! CUSTOMER ! Customer Master File                     *~
            * #20 ! LOTMVMNT ! Lot Movement File                        *~
            * #21 ! LOTMVDTL ! Lot Movement Details                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #10, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #11, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

            select #12, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #20, "LOTMVMNT",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  88,                     ~
                        alt key  1, keypos =   45, keylen =  88          ~

            select #21, "LOTMVDTL",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  96,                     ~
                        alt key  1, keypos =   97, keylen =  96          ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
            call "OPENCHCK" (#20, fs%(20), f2%(20), 0%, rslt$(20))
            call "OPENCHCK" (#21, fs%(21), f2%(21), 0%, rslt$(21))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            typeshdr$(1%)  = "Starting Point:"
            typeshdr$(2%)  = "Print Type Map:"
            selecthdr$(1%) = "From Selection"
            selecthdr$(2%) = "To Selection"

            mat promptl% = con

            if arcyear$ = " " then arcyear$ = "Current"

            descr$     ( 1%) = "Inventory Lot/Store"
            prompts%   ( 1%) =  2%
            prompt$ ( 1%,1%) = "Lot"
            promptl%( 1%,1%) =  6%
            prompt$ ( 1%,2%) = "Store"
            promptl%( 1%,2%) =  3%
            prompt$ ( 1%,3%) = "Date"
            promptl%( 1%,3%) =  10%

            descr$     ( 2%) = "Purchase Orders"
            prompts%   ( 2%) =  3%
            prompt$ ( 2%,1%) = "Vendor Code"
            promptl%( 2%,1%) =  9%
            prompt$ ( 2%,2%) = "Purchase Order"
            promptl%( 2%,2%) = 14%
            prompt$ ( 2%,3%) = "Line"
            promptl%( 2%,3%) =  3%
            prompt$ ( 2%,4%) = "Date"
            promptl%( 2%,4%) =  10%

            descr$     ( 3%) = "Vendor's Lot"
            prompts%   ( 3%) =  2%
            prompt$ ( 3%,1%) = "Vendor Code"
            promptl%( 3%,1%) =  9%
            prompt$ ( 3%,2%) = "Vendor's Lot"
            promptl%( 3%,2%) =  9%
            prompt$ ( 3%,3%) = "Date"
            promptl%( 3%,3%) =  10%

            descr$     ( 4%) = "Vendor's Invoice"
            prompts%   ( 4%) =  3%
            prompt$ ( 4%,1%) = "Vendor Code"
            promptl%( 4%,1%) =  9%
            prompt$ ( 4%,2%) = "Vendor's Invoice"
            promptl%( 4%,2%) = 16%
            prompt$ ( 4%,3%) = "Line"
            promptl%( 4%,3%) =  3%
            prompt$ ( 4%,4%) = "Date"
            promptl%( 4%,4%) =  10%

            descr$     ( 5%) = "Production Job"
            prompts%   ( 5%) =  1%
            prompt$ ( 5%,1%) = "Job Number"
            promptl%( 5%,1%) =  8%
            prompt$ ( 5%,2%) = "Date"
            promptl%( 5%,2%) =  10%

            descr$     ( 6%) = "Customer Shipment"
            prompts%   ( 6%) =  2%
            prompt$ ( 6%,1%) = "Customer Code"
            promptl%( 6%,1%) =  9%
            prompt$ ( 6%,2%) = "Customer Invoice"
            promptl%( 6%,2%) =  8%
            prompt$ ( 6%,3%) = "Date"
            promptl%( 6%,3%) =  10%

            descr$     ( 7%) = "Direct Movement"
            prompts%   ( 7%) =  1%
            prompt$ ( 7%,1%) = "Transaction Text"
            promptl%( 7%,1%) = 30%
            prompt$ ( 7%,2%) = "Date"
            promptl%( 7%,2%) =  10%

            call "COMPNAME" (12%, company$, u3%)

            goto inputmode    /* Jump this Test */

            if arcyear$ <> "Current" and f1%(20%)<> 1% then L09640 else L09720
L09640:         u3% = 2%
                call "ASKUSER" (u3%, "LOTMVMNT DISPLAY",                 ~
                   "There are no Movement Details for this Part",        ~
                   "for " & arcyear$ & " Archived Year",                 ~
                   "Press RETURN to return to the Current LOTMVMNT file")
                if u3% <> 0% then L09640
                goto exit_program

L09720:         u3% = 2%
                call "ASKUSER" (u3%, "LOTMVMNT DISPLAY",                 ~
                          "There are no Movement Details for this Part", ~
                          "Press PF 1 to Select From Archive Files",     ~
                          "Or RETURN to continue...")
                if u3% = 1% then select_archive_year
                goto exit_program

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode

            gosub L29000

L10100:     for fieldnr% = start% to  end%
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10240
L10140:         gosub'101(fieldnr%, 1%)     /* Display & Accept Screen */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10212
L10170:                  fieldnr% = max(start%, fieldnr% - 1%)
                         errormsg$ = " "
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = start% then L10120
                         goto L10170
L10212:               if keyhit%  =  7% then gosub select_answer
                      if keyhit%  =  8% then gosub select_answer
                      if keyhit%  = 23% then gosub select_answer
                      if keyhit%  = 24% then gosub select_answer
                      if keyhit%  = 14% then gosub select_archive_year
                      if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

            goto editpg1

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            gosub test_for_movement_within_range

            start% = 2%  :  end% = 10%
            pf4$, pf5$ = " "
            if implode% <> 0% then pf4$ = "(4)Print Sources"
            if explode% <> 0% then pf5$ = "(5)Print Applications"
            inpmessage$ = edtmessage$
            fieldnr% = 0%
            gosub'101(0%, 2%)          /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 and implode% <> 0% then datasave
                  if keyhit%  =  5 and explode% <> 0% then datasave
                  if keyhit%  =  7% then gosub select_answer
                  if keyhit%  =  8% then gosub select_answer
                  if keyhit%  = 23% then gosub select_answer
                  if keyhit%  = 24% then gosub select_answer
                  if keyhit%  = 14% then gosub select_archive_year
                  if keyhit%  = 16% then       exit_program
                  if keyhit% <>  0% then       editpg1
            if cursor%(1%) <  6% then editpg1
            if cursor%(1%) < 14% then L10100
            if cursor%(1%) < 19% then fieldnr% = cursor%(1%) - 12%
            if cursor%(1%) = 19% then fieldnr% = 7%
                if cursor%(2%) > 26% then fieldnr% =  8%
                if cursor%(2%) > 41% then fieldnr% =  9%
                if cursor%(2%) > 62% then fieldnr% = 10%
               if fieldnr% = 0% then editpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
L11300:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11300
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11300
            goto editpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto editpg1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Part Code              */~
                              L20200,         /* Start Point            */~
                              L20300,         /* Variable               */~
                              L20300,         /* Variable               */~
                              L20300,         /* Variable               */~
                              L20300,         /* Variable               */~
                              L20360,         /* Break Chain at DIRECT  */~
                              L20500,         /* Break Chain at Misc Par*/~
                              L20540,         /* Print Detail           */~
                              L20600          /* Page Break Flag        */

            return

L20100: REM Def/Enable Part Code                   PART$
            inpmessage$ = "Enter Part Code to Trace."
            return

L20200: REM Def/Enable Start Point                 WHICH$
            inpmessage$ = "Place 'X' at desired Start Point and Movement ~
        ~Types to Print.                "
            init (" ") str(which$(),,7%)
            return

L20300: REM Def/Enable Variable                    ANSWER$
            inpmessage$ = "Enter Range for Requested Information Please."
            if prompt$(which%, fieldnr% - 2%) = " " then enabled% = 0%
            return

L20360: REM Def/Enable Break Chain                 BREAK_DIRECT$
            inpmessage$ = "Enter 'Y' to Break the Chain at DIRECT."
            if break_direct$ = " " then break_direct$ = "N"
            return

L20500: REM Def/Enable Break Chain at Misc Part    BREAK_MISC$
            inpmessage$ = "Enter 'Y' to Break the Chain at Misc Part."
            if break_misc$ = " " then break_misc$ = "Y"
            return

L20540: REM Def/Enable Detail                      DETAIL$
            inpmessage$ = "Enter 'N' to not Print Details."
            if detail$ = " "  then detail$ = "Y"
            return

L20600: REM Def/Enable Page Break                  PAGE_BREAK$
            inpmessage$ ="Enter 'Y' to Page Break at each Part/Lot/Store."
            if page_break$ = " " then page_break$ = "N"
            return


L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, part$, partdescr$,         ~
                      which$(), answer$(), ansdescr$, detail$, print$(), ~
                      break_direct$, phead_orig$, fmdate$, todate$,      ~
                      hidate$, lodate$, break_misc$,testparts$(),origpart$
            which% = 8% : prev_which% = 8%
            start% = 1% : end% = 10%
            str(which$(), 8%,2%) = "XX"
            init("X") print$()

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        new_check_start_point
            init (" ") lowrng$(), hirng$()
            implode%, explode% = 0%
            plowkey$ = " "

            gosub set_hi_low_ranges

            /* TEST and SET PLOWKEY */
            path% = 0%
            gosub init_plowkey

L30085:     call "PLOWALTS" (#20, plowkey$, path%, 1%, explode%)
                if explode% = 0% then L30115             /* Try Alt Path */
            get #20 using L30103, trandate$
            gosub test_success
                if try_again% = 1% then L30085 /* Plow for another record*/
                explode%  = success%
                plow_exp$ = plowkey$
L30103:     FMT POS(147), CH(6)
                /* Falling thru so Explode must be possible */

L30115:     path% = 1%
            gosub init_plowkey
L30125:     call "PLOWALTS" (#20, plowkey$, path%, 1%, implode%)
                if implode% = 0% then return     /* No Implode possible */
            get #20 using L30103, trandate$
            gosub test_success
                if success% = 1% then return /* At least able to Implode*/
                if try_again% = 1% then L30125 /* Plow another Alt record*/
                implode% = 0%                    /* No Implode possible */
                return

        init_plowkey
            plowkey$ = type$ & str(lowrng$(1%),,rnglen%(1%)) /*& HEX(00)*/
*                           & STR(LOWRNG$(2%),,RNGLEN%(2%))             ~
*                           & STR(LOWRNG$(3%),,RNGLEN%(3%))
            plowkey$ = addc all(hex(ff))
            return

        test_success
            try_again% = 0%
            if trandate$ >= lodate$ and trandate$ <= hidate$ then L30210
                success% = 0%  :  try_again% = 1%  :  return
            /* Top Level */
L30210:     temptest$ = str(plowkey$, 2%, rnglen%(1%))
            if str(plowkey$, 2%, rnglen%(1%)) <= hirng$(1%) then L30235
                success% = 0%  :  try_again% = 0%  :  return

            /* Second Level */
L30235:     if rnglev% >= 2% then L30245
                goto L30390              /* Ok range so lets test PART */
L30245:     temptest$ = str(plowkey$, 2% + rnglen%(1%), rnglen%(2%))
            if str(plowkey$, 2% + rnglen%(1%), rnglen%(2%)) <= hirng$(2%)~
                                                            then L30275
                str(plowkey$, 2% + rnglen%(1%), ) = hex(ff)   /* Reset */
                success% = 0%  :  try_again% = 1%  :  return

L30275:     if str(plowkey$,2% + rnglen%(1%), rnglen%(2%)) >= lowrng$(2%)~
                                                            then L30310
                str(plowkey$,2%+ rnglen%(1%),) = lowrng$(2%) & lowrng$(3%)
                plowkey$ = addc all(hex(ff))
                success% = 0%  :  try_again% = 1%  :  return

            /* Third Level */
L30310:     if rnglev% = 3% then L30320
                goto L30390              /* Ok range so lets test PART */
L30320:     temptest$ = str(plowkey$, 2% + rnglen%(1%) + rnglen%(2%),    ~
                                                             rnglen%(3%))
            if str(plowkey$, 2% + rnglen%(1%) + rnglen%(2%), rnglen%(3%))~
                            <= hirng$(3%)                   then L30355
                str(plowkey$,2% + rnglen%(1%) + rnglen%(2%),) = hex(ff)
                success% = 0%  :  try_again% = 1%  :  return

L30355:     if str(plowkey$, 2% + rnglen%(1%) + rnglen%(2%), rnglen%(3%))~
                            >= lowrng$(3%)                  then L30390
                str(plowkey$,2%+ rnglen%(1%) + rnglen%(2%),) = lowrng$(3%)
                plowkey$ = addc all(hex(ff))
                success% = 0%  :  try_again% = 1%  :  return

            /* Key meets the Range Criteria.  Is it the Right PART ? */
L30390:     if which% = 1% or which% = 3% or which% = 5% then L30400      ~
                                                         else L30410
L30400:         success% = 1%  :  try_again% = 0%  :  return

L30410:     if str(plowkey$, 46%,25%) = part$ then L30400  /* Success */
                success% = 0%  :  try_again% = 1%  :  return /* Fail */



        set_hi_low_ranges
            on which% gosub    L30495, /* Inventory Lot/Store           */~
                               L30565, /* Purchase Order                */~
                               L30620, /* Vendor Lot                    */~
                               L30685, /* Vendor Invoice                */~
                               L30740, /* Production Job                */~
                               L30795, /* Customer Invoice              */~
                               L30850  /* Direct Movement               */

            return

L30495:   /* Inventory Lot/Store           */
            type$ = "H"                             : rnglev%     =  3%
            lowrng$(1%) = part$                     : rnglen%(1%) = 25%
            lowrng$(2%) = str(answer$(1%,3%),,6%)   : rnglen%(2%) =  6%
            lowrng$(3%) = str(answer$(2%,3%),,3%)   : rnglen%(3%) =  3%
            hirng$(1%)  = part$
            hirng$(2%)  = str(answer$(1%,4%),,6%)
            hirng$(3%)  = str(answer$(2%,4%),,3%)

            return

          /* Purchase Order                */
            call "RJUSTIFY" (str(answer$(3%,3%),,3))
            call "RJUSTIFY" (str(answer$(3%,4%),,3))
L30565:     type$ = "P"                             : rnglev%     =  3%
            lowrng$(1%) = str(answer$(1%,3%),,9%)   : rnglen%(1%) =  9%
            lowrng$(2%) = str(answer$(2%,3%),,16%)  : rnglen%(2%) = 16%
            lowrng$(3%) = str(answer$(3%,3%),,3%)   : rnglen%(3%) =  3%
            hirng$(1%)  = str(answer$(1%,4%),,9%)
            hirng$(2%)  = str(answer$(2%,4%),,16%)
            hirng$(3%)  = str(answer$(3%,4%),,3%)

            return

          /* Vendor Lot                    */
L30620:     type$ = "X"                             : rnglev%     =  3%
            lowrng$(1%) = part$                     : rnglen%(1%) = 25%
            lowrng$(2%) = str(answer$(1%,3%),,9%)   : rnglen%(2%) =  9%
            lowrng$(3%) = str(answer$(2%,3%),,9%)   : rnglen%(3%) =  9%
            hirng$(1%)  = part$
            hirng$(2%)  = str(answer$(1%,4%),,9%)
            hirng$(3%)  = str(answer$(2%,4%),,9%)

            return

          /* Vendor Invoice                */
            call "RJUSTIFY" (str(answer$(3%,3%),,3))
            call "RJUSTIFY" (str(answer$(3%,4%),,3))
L30685:     type$ = "V"                             : rnglev%     =  3%
            lowrng$(1%) = str(answer$(1%,3%),,9%)   : rnglen%(1%) =  9%
            lowrng$(2%) = str(answer$(2%,3%),,16%)  : rnglen%(2%) = 16%
            lowrng$(3%) = str(answer$(3%,3%),,3%)   : rnglen%(3%) =  3%
            hirng$(1%)  = str(answer$(1%,4%),,9%)
            hirng$(2%)  = str(answer$(2%,4%),,16%)
            hirng$(3%)  = str(answer$(3%,4%),,3%)

            return

          /* Production Job                */
L30740:     type$ = "J"                             : rnglev%     =  2%
            lowrng$(1%) = part$                     : rnglen%(1%) = 25%
            lowrng$(2%) = "*" &  str(answer$(1%,3%),,8%)
            lowrng$(3%) = " "                       : rnglen%(2%) =  9%
            hirng$(1%)  = part$                     : rnglen%(3%) =  1%
            hirng$(2%)  = "*" &  str(answer$(1%,4%),,8%)
            hirng$(3%)  = " "

            return

          /* Customer Invoice              */
L30795:     type$ = "C"                             : rnglev%     =  2%
            lowrng$(1%) = str(answer$(1%,3%),,9%)   : rnglen%(1%) =  9%
            lowrng$(2%) = str(answer$(2%,3%),,8%)   : rnglen%(2%) =  8%
            lowrng$(3%) = " "                       : rnglen%(3%) =  1%
            hirng$(1%)  = str(answer$(1%,4%),,9%)
            hirng$(2%)  = str(answer$(2%,4%),,8%)
            hirng$(3%)  = " "

            return

          /* Direct Movement               */
L30850:     type$ = "D"                             : rnglev%     =  1%
            lowrng$(1%) = str(answer$(1%,1%),,30%)  : rnglen%(1%) = 30%
            lowrng$(2%) = " "                       : rnglen%(2%) =  1%
            lowrng$(3%) = " "                       : rnglen%(3%) =  1%
            hirng$(1%)  = str(answer$(1%,4%),,30%)
            hirng$(2%)  = " "
            hirng$(3%)  = " "

            return


        REM *************************************************************~
            *          P R I N T   R E P O R T                          *~
            *-----------------------------------------------------------*~
            * Print the Report.                                         *~
            *************************************************************
        dataput

            time$ = " " : call "TIME" (time$)
            l%, page%, path% = 0%
            p$() = " "
            mat p% = zer
            select printer(134)
            call "SETPRNT" ("HNY004", " ", 0%, 0%)
            line% = 1000%
            if keyhit% = 4% then path% = 1%              /* Implode */   ~
                            else plowkey$ = plow_exp$    /* Explode */

            /* ** Start Print Loop ** */
            /* Initial PLOWKEY$ has been set in Check_Start_Point */
L31115:     gosub'9(plowkey$)

            /* Set next PLOWKEY */
            str(plowkey$,45%,) =  hex(ff)
L31130:     call "PLOWALTS" (#20, plowkey$, path%, 1%, f1%(20%))
                if f1%(20%) = 0%  then L31210 /* No More, so close her up*/
            get #20 using L30103, trandate$
            gosub test_success
                if success% = 1% then L31165
                if try_again% = 1% then L31130    /* Try another Record */
                goto L31210                         /* Nothing in Range */

L31165:     if page_break$ = "Y" then L31195      /* Force Page          */
            if page%       <  1% then L31195      /* Force Page          */
                gosub set_phead                  /* Header for Printing */
                print                            /* Continue Printing   */
                print using L32660 , phead$
                print using L32700
                line% = line% + 3%
                phead_orig$ = phead$
                goto L31115                         /* Print next one   */
L31195:     line% = 99%                            /* Force Page       */
            goto L31115                             /* Print next one   */

L31210:     gosub end_report
            return                       /* ALL DONE */

         deffn'9(plowkey1$)
              l% = l% + 1
              p$(l%) = str(plowkey1$,,44)
              if l% = 1% then L31440
                 search str(p$(),1%,88%*(l%-1%)) = str(p$(l%),1%,44%)    ~
                        to f1%() step 88
                 if f1%(1%) = 0% then L31440        /* NO LOOP HERE */
                    print$, pline$ = " "
                    pline$ = "REDUNDANCY DETECTED:"
                    convert int((f1%(1%) + 87%)/88%) to                  ~
                            str(pline$, len(pline$)+2,2), pic(##)
                    put str(print$, l%*2 -1), using L32740, l%, pline$
                    print using L32760, print$, " " : print
                    line% = line% + 2%
                    goto L31650

L31440: REM READ THE SELECTED KEYS
L31450:         call "PLOWALTS" (#20, p$(l%), path%, 44%, f1%(10))
                if f1%(10) = 0% then L31650
         temptest$ = p$(l%)
                  component$ = str(p$(l%),45)
                  if str(p$(l%),,1)  = "C" then L31510
                  if str(p$(l%),,1) <> "D" then L31550

L31510:           if l% = 1% then L31540
                    if str(component$,2,25) = str(p$(l%-1%),2,25)        ~
                                                 then L31550 else L31450
L31540:             if str(component$,2,25) <> part$ then L31450

L31550:           get #20, using L31560, qty, trandate$
L31560:               FMT POS(133), PD(14,4), POS(147), CH(6)
                  if l% > 1% then L31570
                  if trandate$ < lodate$ or trandate$ > hidate$ then L31440
L31570:           p%(l%) = p%(l%) + 1
                  call "DATEFMT" (trandate$)
                  call "CONVERT" (qty, 0.2, qty$)
                  gosub L31700            /* PROCESS PRINT ENTRY        */
                  if break_direct$ = "Y" and str(component$,,1%) = "D"   ~
                      then L31440              /* Break Chain at Direct */
                  gosub test_break_at_misc_part
                  if break% = 1% then L31440   /* Break Chain at Misc */

                  if l% < 30% then gosub'9(component$)
                                         /* DO COMPS IF NOT AT TOP     */
                  goto L31440

L31650:           REM END ROUTINE GRACEFULLY.
                      p%(l%) = 0
                      testparts$(l%) = " "
                      l% = l% - 1
                      return

L31700:     REM ROUTINE TO PRINT ENTRY JUST LOADED.
                gosub L32000              /* PAGE CONTROL SUBROUTINE    */
                gosub print_it
                hdr%, key% = path%
                gosub test_print_detail
                return

        test_break_at_misc_part
             /* BREAK%:  0% = NO Break, 1% = Break  */
                break% = 0%
                if break_misc$ =  "N"  then return     /* NO Break      */
                if path%       =   1%  then L31850      /* Source Report */

            /* Application test */
                if testparts$(l%) = origpart$ then return
                if l% < 2%  then return
                if pcode$(l%-1%) <> "J" then return    /* Prev. NOT Job */
                search str(testparts$(), 1%, 25% * (l% - 1%)) =          ~
                                       testparts$(l%) to f1%() step 25
                if f1%(1%) <> 0%  then return  /* else Must be Misc Part*/
                     breakmsg$ =                                         ~
                            "Misc. Part Encountered     :  Chain Broken"
                     goto break_and_print

            /* Source Test */
L31850:         if l% < 2% then return
                if pcode$(l%-1%) <> "J" then return    /* Prev. NOT Job */
                if pcode$(l%   ) <> "J" then return    /* This NOT Job */
                if partmoved$ <> "C" then L31880
                     breakmsg$ =                                         ~
                            "Component Moved From Job   :  Chain Broken"
                     goto break_and_print
L31880:         if partmoved$ =  "P" then return  /* Parent Part */
                if l% > 2% then L31900   /* jump */
                if testparts$(1%) = origpart$ or                         ~
                   testparts$(2%) = origpart$ then return                ~
                                 else L31910 /* Break and Print Message */
L31900:         if testparts$(l% -1%) = testparts$(l% - 2%)              ~
                      or testparts$(l%) = testparts$(l% - 2%) then return
L31910:             breakmsg$ =                                          ~
                            "Misc. Part Encountered     :  Chain Broken"
                    goto break_and_print

        break_and_print
                break% = 1%
                print$ = " "
                str(print$, l%*2%+3%) = breakmsg$
                line% = line% + 2%
                print using L32760, print$, " "
                print
                return

            /* *** END Misc Part Test *** */

L32000:     REM PAGE CONTROL ROUTINE.  FAIRLY SIMPLE, NO TAG LINE TRICKS.
                if page% <> 0% then L32040
                if path% = 1% then call "SHOSTAT" ("Printing Implosion") ~
                              else call "SHOSTAT" ("Printing Explosion")
L32040:         line% = line% + 1%
                if line% < 56% then return
                   if page% = 0% then gosub print_params
                   page% = page% + 1%
                   gosub print_header
                   gosub set_phead             /* Header for Printing */
                   print
                   print using L32620
                   if phead$ = phead_orig$ then print using L32690, phead$~
                        else  print using L32660, phead$
                   print using L32700
                   print
                   phead_orig$ = phead$
                   line% = 9%
                   return

        print_header
                   print page
                   print using L32500, date$, time$, company$
                   if path%  = 1% then print using L32540, page%
                   if path% <> 1% then print using L32580, page%
                   print using L32650, arcyear$
                   return

        set_phead
            on which% gosub    L32305, /* Inventory Lot/Store           */~
                               L32345, /* Purchase Order                */~
                               L32370, /* Vendor Lot                    */~
                               L32410, /* Vendor Invoice                */~
                               L32440, /* Production Job                */~
                               L32465, /* Customer Invoice              */~
                               L32486  /* Direct Movement               */

            phead$ = phead$ & "   DATE: " & fmdate$

            return

L32305:     /* Inventory */
            phead$ = "PART: "         & part$                          & ~
                     "   LOT: "       & str(plowkey$,27%,6%)           & ~
                     "   STORE: "     & str(plowkey$,33%,3%)
            return

            /* Purchase Order                */
L32345:     phead$ = "VENDOR: "       &  str(plowkey$, 2%, 9%)         & ~
                     " P.O. NUMBER: " &  str(plowkey$,11%,16%)         & ~
                     "   PART: "      &  part$
            return

            /* Vendor Lot                    */
L32370:     phead$ = "PART: "         & part$                          & ~
                     " VENDOR: "      &  str(plowkey$, 2%, 9%)         & ~
                     " VENDOR'S LOT:" &  str(plowkey$,11%, 9%)
            return

            /* Vendor Invoice                */
L32410:     phead$ = "VENDOR: "       &  str(plowkey$, 2%, 9%)         & ~
                     " INVOICE NO.: " &  str(plowkey$,11%,16%)         & ~
                     "   PART: "      &  part$
            return

            /* Production Job                */
L32440:     phead$ = "JOB NUMBER: "   &  str(plowkey$,28%, 8%)         & ~
                     "   PART: "      &  part$
            return

            /* Customer Invoice              */
L32465:     phead$ = "CUSTOMER : "    &  str(plowkey$, 2%, 9%)         & ~
                     " INVOICE NO.: " &  str(plowkey$,11%,16%)         & ~
                     "   PART: "      &  part$
            return

            /* Direct Movment                */
L32486:     phead$ = "PART: "         &  part$                         & ~
                     " DIRECT MOVEMENT: " & str(plowkey$, 2%,30%)
            return

L32500: %RUN DATE: ######## @ ########       ############################~
        ~################################                     LOTMVRPT-HNY~
        ~004

L32540: %                                L O T   T R A C K I N G:   S O U~
        ~ R C E   O F   I N V E N T O R Y                           PAGE #~
        ~###

L32580: %                            L O T   T R A C K I N G:   A P P L I~
        ~ C A T I O N   O F   I N V E N T O R Y                     PAGE #~
        ~###

L32620: %                                                                ~
        ~                                                       NET   DATE~
        ~ OF

L32650: %                                                       ARCHIVE Y~
        ~EAR: #######

L32660: %STARTING FROM ##################################################~
        ~################################################  QUANTITY LAST M~
        ~OVE
L32690: %   CONTINUE - ##################################################~
        ~################################################  QUANTITY LAST M~
        ~OVE

L32700: %----------------------------------------------------------------~
        ~------------------------------------------------ --------- ------~
        ~---

L32740: %##> ##################################################
L32760: %################################################################~
        ~##########################################################  #####~
        ~###


L32810:         %                          * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @   ########   * * * * * * * * * *

        REM *************************************************************~
            * SERVES BOTH THE IMPLOSION AND THE EXPLOSION ROUTINES.     *~
            *************************************************************
        print_it:
            pflag% = 0%               /* Flag to tell details to Print */
            print$, pline$ = " "
            testparts$(l%) = " "
            pcode$(l%) =  str(component$,,1%)
            pcode% = pos("HPXVJCDA" = str(component$,,1%))
            if pcode% = 0% then L33095
            if pcode% = 1% or pcode% = 5% then                           ~
                                    testparts$(l%) = str(component$,2,25)
            if pcode% = 5% then partmoved$ = str(component$,36%,1%)      ~
                           else partmoved$ = " "
            if print$(pcode%) = " " then return /* Not Mapped for Print */
            pflag% = 1%               /* Flag to tell details to Print */
            on pos("HPXVJCDA" = str(component$,,1%)) goto L33120, L33200,  ~
                                 L33300, L33400, L33500, L33600, L33700, L33730
L33095:     if print$(8%) = " " then return     /* Not Mapped for Print */
            pflag% = 1%               /* Flag to tell details to Print */
            pline$ = "????: " & str(component$,,44)
            goto L33800

L33120:     pline$ = "PART: " & str(component$,2,25)
            pline$ = pline$ & " LOT:" & str(component$,27,6)
            pline$ = pline$ & " STR:" & str(component$,33,3)
            goto L33800

L33200:     pline$ = "VENDOR: " & str(component$,2,9)
            pline$ = pline$ & " PO #: " & str(component$,11,16)
            pline$ = pline$ & " LINE: " & str(component$,27,3)
            goto L33800

L33300:     pline$ = "VENDOR: " & str(component$,27,9)
            pline$ = pline$ & " VENDOR'S LOT: " & str(component$,36,9)
            goto L33800

L33400:     pline$ = "VENDOR: " & str(component$,2,9)
            pline$ = pline$ & " INV#: " & str(component$,11,16)
            pline$ = pline$ & " LINE: " & str(component$,27,3)
            goto L33800

L33500:     pline$ = "PART: " & str(component$,2,25)
            pline$ = pline$ & " JOB: " & str(component$,28,8)
            goto L33800

L33600:     pline$ = "CUSTOMER:" & str(component$,2,9)
            pline$ = pline$ & " INVOICE:" & str(component$,11,8)
            goto L33800

L33700:     pline$ = "DIRECT: " & str(component$,2,34)
            goto L33800

L33730:     if print$(8%) = " " then return  /* Not Mapped for Print */
            temp$  =  str(component$,2,6) : call "DATEFMT" (temp$)
            pline$ = "SESN DATE: " & temp$
            pline$ = pline$ & "  SESN #: " & str(component$,10%,2%)
            pline$ = pline$ & "  TKT #: "  & str(component$,12%,6%)
            goto L33800

L33800:     put str(print$, l%*2 -1), using L32740, l%, pline$
            print using L32760, str(print$) & qty$, trandate$
        return

        print_params           /* Print Page Zero */
            gosub print_header
L33862:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L33878
                str(i$(), i%, 1%) = hex(20)
                goto L33862
L33878:     print skip(3)
            print tab(26);
            print "------------------- Lot Movement Selection Parameters ~
        ~--------------------------"
            print
            for x% = 4% to 19% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            return

        end_report
            if page% > 0% then L33930    /* Nothing done */
                gosub nothing_to_report
                goto L33960              /* Nothing done */
L33930:     time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L32810, time$        /* End of report line */
L33960:     close printer
            call "SETPRNT" ("HNY004", " ", 0%, 1%)
            phead_orig$ = " "
            return

        test_print_detail
            if detail$ <> "Y" or pflag% = 0%  then return
            mx% = 1% : detkey$ = xor detkey$
            str(detkey$,,88) = str(p$(l%))

L34050:     call "PLOWALTS" (#21, detkey$, key%, 88%, f1%(11))
            if f1%(11) <> 0% then goto L34140
                if hdr% = key% then goto L34090
                     print : line% = line% +1 : return
L34090:         mx% = -1% : detkey$ = xor detkey$
                str(detkey$,,88) = str(p$(l%))
                if key% <> 0% then goto L34130
                     key% = 1% : goto L34050
L34130:         key% = 0% : goto L34050
L34140:     get #21 using L34150, detdate%, qty
L34150:         FMT  POS(89), BI(4), POS(193), PD(14,4)
            convert detdate% to detdate$, pic (########)
            call "DATEFMT" (detdate$)
            convert qty*mx% to qty$, pic (######.##-)
            print$ = " "
            str(print$, l%*2%+1%) = str(qty$) & " ON " & detdate$
            gosub L32000
            print using L32760, print$, " "
            goto L34050

        select_archive_year
            fileid$ = "LOTM"
            call "PICKYEAR" (fileid$, choice$)
              if f2%(20%) = 0% then close #20/* Close Curr. Detail File */
              if choice$ = "CURR" or choice$ = " " then L34300 else L34310
L34300:       lotmprname$ = "LOTMVMNT" : lotdprname$ = "LOTDVMNT"
                  goto L34320
L34310:       lotmprname$ = "LOTM" & choice$
              lotdprname$ = "LOTD" & choice$
L34320:       call "PUTPRNAM" addr(#20, lotmprname$)
              call "OPENCHCK" (#20, fs%(20%), f2%(20%), 0%, rslt$(20%))
              call "PUTPRNAM" addr(#21, lotdprname$)
              call "OPENCHCK" (#21, fs%(21%), f2%(21%), 0%, rslt$(21%))
              if choice$ = "CURR" or choice$ = " "                       ~
               then arcyear$ = "Current" else arcyear$ = choice$ & "   "
              keyhit% = 14%        /* Belt and Galisters */
            return

        nothing_to_report
            hit% = 2%   :  hit% = hit%
            call "ASKUSER" (keyhit%, "**** No record Within Range ****", ~
                            " "                                     ,    ~
                            "No Record found within Range Specified",    ~
                            "Press Any Key to continue" )
            return


            /* **** General Plowcode Selection Subs **** */
        select_answer
            mat includ_exclud = zer
            on which% goto     L36055, /* Inventory Lot/Store           */~
                               L36195, /* Purchase Order                */~
                               L36345, /* Vendor Lot                    */~
                               L36490, /* Vendor Invoice                */~
                               L36645, /* Production Job                */~
                               L36790, /* Customer Invoice              */~
                               L36950  /* Direct Movement               */~


L36055:   /* ** Inventory ** */
              /* Test if Plow is worth doing */
            plowkey$     = "H" & str(part$) & hex(00)
            if keyhit% < 9% then path% = 0% else path% = 1%
            call "PLOWALTS" (#20, plowkey$, path%, 26%, f1%(20%))
                if f1%(20%) = 0% then return

            includ_exclud(1%) = 1.26  :  includ_exclud$(1%) = "H" & part$
            if keyhit% < 9% then L36100
                includ_exclud(1%) = 45.26
L36100:     plowdescr$ = hex(06) & "Select LOT Movement Source"
            plowhdr$(2%) = "                           :Lot "
            plowhdr$(1%) = " "
            plowhdr$(3%) = "Range Selection for FROM Lot Variable "
            if keyhit% = 8% or keyhit% = 24%                             ~
                then plowhdr$(3%) = "Range Selection for TO Lot Variable"
            if keyhit% > 8% then str(plowdescr$, 22%, ) = "Destination"
            if keyhit% < 9% then key_descr = -0.001                      ~
                            else key_descr = -1.001
            plowkey$     = "H" & str(part$) & hex(00)
            call "PLOWCODE" (#20, plowkey$, plowdescr$,-5032%, key_descr,~
                             f1%(20%),                                   ~
                             plowhdr$(), 00.00,     .0000,               ~
                             includ_exclud(), includ_exclud$())
            if keyhit% = 7% or keyhit% = 23%                             ~
                then  answer$(1%, 1%) = str(plowkey$, 27%, 6%)           ~
                else  answer$(1%, 2%) = str(plowkey$, 27%, 6%)
            return

L36195:   /* ** Purchase Order ** */
              /* Test if Plow is worth doing */
            plowkey$ = "P" & hex(00)
            if keyhit% < 9% then path% = 0% else path% = 1%
            call "PLOWALTS" (#20, plowkey$, path%, 1%, f1%(20%))
                if f1%(20%) = 0% then return

            includ_exclud(1%) =  1.01  :  includ_exclud$(1%) = "P"
            includ_exclud(2%) = 46.25  :  includ_exclud$(2%) = part$
            if keyhit% < 9% then L37005
                includ_exclud(1%) = 45.01
                includ_exclud(2%) = 90.25
            plowdescr$ = hex(06) & "Select Vendor Code Movement Source"
            plowhdr$(1%) = " "
            plowhdr$(2%) = "  :Vendor Code"
            plowhdr$(3%) = "Range Selection for FROM Vendor Code Variable"
            if keyhit% = 8% or keyhit% = 24%                             ~
                then plowhdr$(3%) = "Range Selection for TO Vendor Code V~
        ~ariable"
            if keyhit% > 8% then str(plowdescr$, 22%, ) = "Destination"
            if keyhit% < 9% then key_descr = -0.001                      ~
                            else key_descr = -1.001
            plowkey$     = "P"  & hex(00)
            call "PLOWCODE" (#20, plowkey$, plowdescr$,-5010%, key_descr,~
                             f1%(20%), plowhdr$(), 0.0, 0.0,             ~
                             includ_exclud(), includ_exclud$())
            if f1%(20%) = 0% then plowkey$ = all(hex(20))
            if keyhit% = 7% or keyhit% = 23%                             ~
                then  answer$(1%, 1%) = str(plowkey$,  2%, 9%)           ~
                else  answer$(1%, 2%) = str(plowkey$,  2%, 9%)
            return

L36345:   /* ** Vendor Lot ** */
              /* Test if Plow is worth doing */
            plowkey$     = "X" & str(part$) & hex(00)
            if keyhit% < 9% then path% = 0% else path% = 1%
            call "PLOWALTS" (#20, plowkey$, path%, 26%, f1%(20%))
                if f1%(20%) = 0% then return

            includ_exclud(1%) = 1.26  :  includ_exclud$(1%) = "X" & part$
            if keyhit% < 9% then L36390
                includ_exclud(1%) = 45.26
L36390:     plowdescr$ = hex(06) & "Select Vendor Code Movement Source"
            plowhdr$(2%) = "                         :Vendor Code"
            plowhdr$(1%) = " "
            plowhdr$(3%) = "Range Selection for FROM Vendor Code Variable"
            if keyhit% = 8% or keyhit% = 24%                             ~
                then plowhdr$(3%) = "Range Selection for TO Vendor Code V~
        ~ariable"
            if keyhit% > 8% then str(plowdescr$, 22%, ) = "Destination"
            if keyhit% < 9% then key_descr = -0.001                      ~
                            else key_descr = -1.001
            plowkey$     = "X" & str(part$) & hex(00)
            call "PLOWCODE" (#20, plowkey$, plowdescr$,-5035%, key_descr,~
                             f1%(20%), plowhdr$(), 0.0, 0.0,             ~
                             includ_exclud(), includ_exclud$())

            if f1%(20%) = 0% then plowkey$ = all(hex(20))
            if keyhit% = 7% or keyhit% = 23%                             ~
                then  answer$(1%, 1%) = str(plowkey$, 27%, 9%)           ~
                else  answer$(1%, 2%) = str(plowkey$, 27%, 9%)
            return

L36490:   /* ** Vendor Invoice ** */
              /* Test if Plow is worth doing */
            plowkey$     = "V" & hex(00)
            if keyhit% < 9% then path% = 0% else path% = 1%
            call "PLOWALTS" (#20, plowkey$, path%, 1%, f1%(20%))
                if f1%(20%) = 0% then return

            includ_exclud(1%) =  1.01  :  includ_exclud$(1%) = "V"
            includ_exclud(2%) = 46.25  :  includ_exclud$(2%) = part$
            if keyhit% < 9% then L36545
                includ_exclud(1%) = 45.01
                includ_exclud(2%) = 90.25
L36545:     plowdescr$ = hex(06) & "Select Vendor Code Movement Source"
            plowhdr$(2%) = "  :Vendor Code"
            plowhdr$(1%) = " "
            plowhdr$(3%) = "Range Selection for FROM Vendor Code Variable"
            if keyhit% = 8% or keyhit% = 24%                             ~
                then plowhdr$(3%) = "Range Selection for TO Vendor Code V~
        ~ariable"
            if keyhit% > 8% then str(plowdescr$, 22%, ) = "Destination"
            if keyhit% < 9% then key_descr = -0.001                      ~
                            else key_descr = -1.001
            plowkey$     = "V"  & hex(00)
            call "PLOWCODE" (#20, plowkey$, plowdescr$,-5010%, key_descr,~
                             f1%(20%), plowhdr$(), 0.0, 0.0,             ~
                             includ_exclud(), includ_exclud$())

            if f1%(20%) = 0% then plowkey$ = all(hex(20))
            if keyhit% = 7% or keyhit% = 23%                             ~
                then  answer$(1%, 1%) = str(plowkey$,  2%, 9%)           ~
                else  answer$(1%, 2%) = str(plowkey$,  2%, 9%)
            return

L36645:   /* ** Production Job ** */
              /* Test if Plow is worth doing */
            plowkey$     = "J" & str(part$) & hex(00)
            if keyhit% < 9% then path% = 0% else path% = 1%
            call "PLOWALTS" (#20, plowkey$, path%, 26%, f1%(20%))
                if f1%(20%) = 0% then return

            includ_exclud(1%) = 1.26  :  includ_exclud$(1%) = "J" & part$
            if keyhit% < 9% then L36690
                includ_exclud(1%) = 45.26
L36690:     plowdescr$ = hex(06) & "Select Job Number Movement Source"
            plowhdr$(2%) = "                          :Job Number"
            plowhdr$(1%) = " "
            plowhdr$(3%) = "Range Selection for FROM Job No. Variable "
            if keyhit% = 8% or keyhit% = 24%                             ~
                then plowhdr$(3%) = "Range Selection for TO Job No. "  & ~
                                     "Variable"
            if keyhit% > 8% then str(plowdescr$, 22%, ) = "Destination"
            if keyhit% < 9% then key_descr = -0.001                      ~
                            else key_descr = -1.001
            plowkey$     = "J" & str(part$) & hex(00)
            call "PLOWCODE" (#20, plowkey$, plowdescr$,-5035%, key_descr,~
                             f1%(20%), plowhdr$(), 0.0, 0.0,             ~
                             includ_exclud(), includ_exclud$())

            if f1%(20%) = 0% then plowkey$ = all(hex(20))
            if keyhit% = 7% or keyhit% = 23%                             ~
                then  answer$(1%, 1%) = str(plowkey$, 28%, 8%)           ~
                else  answer$(1%, 2%) = str(plowkey$, 28%, 8%)
            return

L36790:   /* ** Customer Invoice ** */
              /* Test if Plow is worth doing */
            plowkey$     = "C" & hex(00)
            if keyhit% < 9% then path% = 0% else path% = 1%
            call "PLOWALTS" (#20, plowkey$, path%, 1%, f1%(20%))
                if f1%(20%) = 0% then return

            includ_exclud(1%) =  1.01  :  includ_exclud$(1%) = "C"
            includ_exclud(2%) = 46.25  :  includ_exclud$(2%) = part$
            if keyhit% < 9% then L37005
                includ_exclud(1%) = 45.01
                includ_exclud(2%) = 90.25
            plowdescr$ = hex(06) & "Select Customer Code Movement Source"
            plowhdr$(1%) = " "
            plowhdr$(2%) = "  :Customer Code"
            plowhdr$(3%) = "Range Selection for FROM Customer Code Variab~
        ~le"
            if keyhit% = 8% or keyhit% = 24%                             ~
                then plowhdr$(3%) = "Range Selection for TO Customer Code~
        ~ Variable"
            if keyhit% > 8% then str(plowdescr$, 22%, ) = "Destination"
            if keyhit% < 9% then key_descr = -0.001                      ~
                            else key_descr = -1.001
            plowkey$     = "C"  & hex(00)
            call "PLOWCODE" (#20, plowkey$, plowdescr$,-5010%, key_descr,~
                             f1%(20%), plowhdr$(), 0.0, 0.0,             ~
                             includ_exclud(), includ_exclud$())

            if f1%(20%) = 0% then plowkey$ = all(hex(20))
            if keyhit% = 7% or keyhit% = 23%                             ~
                then  answer$(1%, 1%) = str(plowkey$,  2%, 9%)           ~
                else  answer$(1%, 2%) = str(plowkey$,  2%, 9%)
            return

L36950:   /* ** Direct Movment ** */
              /* Test if Plow is worth doing */
            plowkey$     = "D" & hex(00)
            if keyhit% < 9% then path% = 0% else path% = 1%
            call "PLOWALTS" (#20, plowkey$, path%, 1%, f1%(20%))
                if f1%(20%) = 0% then return

            includ_exclud(1%) =  1.01  :  includ_exclud$(1%) = "D"
            includ_exclud(2%) = 46.25  :  includ_exclud$(2%) = part$
            if keyhit% < 9% then L37005
                includ_exclud(1%) = 45.01
                includ_exclud(2%) = 90.25
L37005:     plowdescr$ = hex(06) & "Select Direct Movement Source Transac~
        ~tion Text"
            plowhdr$(2%) = "  :Transaction Text"
            plowhdr$(1%) = " "
            plowhdr$(3%) = "Range Selection for FROM Transaction Text"
            if keyhit% = 8% or keyhit% = 24%                             ~
                then plowhdr$(3%) =                                      ~
                           "Range Selection for TO Transaction Text"
            if keyhit% > 8% then str(plowdescr$, 22%, ) = "Destination"
            if keyhit% < 9% then key_descr = -0.001                      ~
                            else key_descr = -1.001
            plowkey$     = "D"  & hex(00)
            call "PLOWCODE" (#20, plowkey$, plowdescr$,-5031%, key_descr,~
                             f1%(20%), plowhdr$(), 0.0, 0.0,             ~
                             includ_exclud(), includ_exclud$() )
            if f1%(20%) = 0% then plowkey$ = all(hex(20))
            if keyhit% = 7% or keyhit% = 23%                             ~
                then  answer$(1%, 1%) = str(plowkey$,  2%, 30%)          ~
                else  answer$(1%, 2%) = str(plowkey$,  2%, 30%)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              str(line2$,62%) = "LOTMVRPT: " & str(cms2v$,,8%)
              gosub set_pf1
              if fieldnr% = 0% then                                      ~
                            init(hex(86)) lfac$(), lfcd$(), lfcp$()      ~
                               else                                      ~
                            init(hex(8c)) lfac$(), lfcd$(), lfcp$()

              on fieldnr% gosub L40120,         /* Part Code            */~
                                L40125,         /* Which                */~
                                L40130,         /* Variable             */~
                                L40130,         /* Variable             */~
                                L40130,         /* Variable             */~
                                L40130,         /* Variable             */~
                                L40160,         /* Break_Chain at Direct*/~
                                L40160,         /* Break_Chain at Misc P*/~
                                L40160,         /* Detail               */~
                                L40160          /* Page Break           */
              goto L40170

L40120:           init (hex(81)) str(lfac$(),1,1) : return
L40125:           init (hex(81)) str(lfcd$(),1%,8%) : return
L40130:           if prompts%(which%) = 0% then return
                  if which% <> 7% then L40160
                      lfac$(fieldnr%) = hex(80) : return
L40160:           lfac$(fieldnr%) = hex(81) : return

L40170:     accept                                                       ~
               at (01,02),                                               ~
                  "Part/Lot Movement Tracking Report",                   ~
               at (01,40), "Archive Year:",fac(hex(84)),arcyear$, ch(07),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part Code",                                  ~
               at (04,20), fac(lfac$( 1)), part$                , ch(25),~
               at (04,49), fac(hex(8c)),   partdescr$           , ch(32),~
                                                                         ~
               at (05,02), fac(hex(ac)), typeshdr$(1%)          , ch(25),~
               at (05,42), fac(hex(ac)), typeshdr$(2%)          , ch(25),~
                                                                         ~
               at (06,02), fac(lfcd$( 1)), which$( 1)           , ch( 1),~
               at (07,02), fac(lfcd$( 2)), which$( 2)           , ch( 1),~
               at (08,02), fac(lfcd$( 3)), which$( 3)           , ch( 1),~
               at (09,02), fac(lfcd$( 4)), which$( 4)           , ch( 1),~
               at (10,02), fac(lfcd$( 5)), which$( 5)           , ch( 1),~
               at (11,02), fac(lfcd$( 6)), which$( 6)           , ch( 1),~
               at (12,02), fac(lfcd$( 7)), which$( 7)           , ch( 1),~
                                                                         ~
               at (06,04), fac(hex(8c)),   descr$( 1)           , ch(30),~
               at (07,04), fac(hex(8c)),   descr$( 2)           , ch(30),~
               at (08,04), fac(hex(8c)),   descr$( 3)           , ch(30),~
               at (09,04), fac(hex(8c)),   descr$( 4)           , ch(30),~
               at (10,04), fac(hex(8c)),   descr$( 5)           , ch(30),~
               at (11,04), fac(hex(8c)),   descr$( 6)           , ch(30),~
               at (12,04), fac(hex(8c)),   descr$( 7)           , ch(30),~
                                                                         ~
               at (06,42), fac(lfcd$( 1%)), print$( 1%)         , ch( 1),~
               at (07,42), fac(lfcd$( 2%)), print$( 2%)         , ch( 1),~
               at (08,42), fac(lfcd$( 3%)), print$( 3%)         , ch( 1),~
               at (09,42), fac(lfcd$( 4%)), print$( 4%)         , ch( 1),~
               at (10,42), fac(lfcd$( 5%)), print$( 5%)         , ch( 1),~
               at (11,42), fac(lfcd$( 6%)), print$( 6%)         , ch( 1),~
               at (12,42), fac(lfcd$( 7%)), print$( 7%)         , ch( 1),~
               at (13,42), fac(lfcd$( 8%)), print$( 8%)         , ch( 1),~
                                                                         ~
               at (06,44), fac(hex(8c)),   descr$( 1%)          , ch(30),~
               at (07,44), fac(hex(8c)),   descr$( 2%)          , ch(30),~
               at (08,44), fac(hex(8c)),   descr$( 3%)          , ch(30),~
               at (09,44), fac(hex(8c)),   descr$( 4%)          , ch(30),~
               at (10,44), fac(hex(8c)),   descr$( 5%)          , ch(30),~
               at (11,44), fac(hex(8c)),   descr$( 6%)          , ch(30),~
               at (12,44), fac(hex(8c)),   descr$( 7%)          , ch(30),~
               at (13,44), "Any Other Movements",                        ~
                                                                         ~
               at (14,20), fac(hex(ac)), selecthdr$(1%)         , ch(25),~
               at (14,51), fac(hex(ac)), selecthdr$(2%)         , ch(25),~
                                                                         ~
               at (15,02), fac(hex(8c)),   prompt$(which%,1%)   , ch(17),~
               at (16,02), fac(hex(8c)),   prompt$(which%,2%)   , ch(17),~
               at (17,02), fac(hex(8c)),   prompt$(which%,3%)   , ch(17),~
               at (18,02), fac(hex(8c)),   prompt$(which%,4%)   , ch(17),~
                                                                         ~
               at (15,20), fac(lfac$(3%)),                               ~
                           str(answer$(1%,1%),,promptl%(which%,1%)),     ~
               at (16,20), fac(lfac$(4%)),                               ~
                           str(answer$(2%,1%),,promptl%(which%,2%)),     ~
               at (17,20), fac(lfac$(5%)),                               ~
                           str(answer$(3%,1%),,promptl%(which%,3%)),     ~
               at (18,20), fac(lfac$(6%)),                               ~
                           str(answer$(4%,1%),,promptl%(which%,4%)),     ~
                                                                         ~
               at (15,51), fac(lfac$(3%)),                               ~
                           str(answer$(1%,2%),,promptl%(which%,1%)),     ~
               at (16,51), fac(lfac$(4%)),                               ~
                           str(answer$(2%,2%),,promptl%(which%,2%)),     ~
               at (17,51), fac(lfac$(5%)),                               ~
                           str(answer$(3%,2%),,promptl%(which%,3%)),     ~
               at (18,51), fac(lfac$(6%)),                               ~
                           str(answer$(4%,2%),,promptl%(which%,4%)),     ~
                                                                         ~
               at (19,02), "Break Chain at: Direct?",                    ~
               at (19,26), fac(lfac$(7%)), break_direct$        , ch( 1),~
               at (19,29), "Misc. Part?",                                ~
               at (19,41), fac(lfac$(8%)), break_misc$          , ch( 1),~
               at (19,48), "Print Detail?",                              ~
               at (19,62), fac(lfac$(9%)), detail$              , ch( 1),~
               at (19,65), "Page Break?",                                ~
               at (19,77), fac(lfac$(10%)), page_break$         , ch( 1),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41040
                  call "MANUAL" ("LOTMVRPT")
                  goto L40170

L41040:        if keyhit% <> 15% then L41080
                  call "PRNTSCRN"
                  goto L40170

L41080:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return



        set_pf1
        if edit% = 2% then L41270     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                          (" &       ~
                      "7/23)FROM Source/Dest  (13)Instructions"
            pf$(2%) = "                 (4)Previous Field     (" &       ~
                      "8/24)TO Source/Dest    (15)Print Screen"
            pf$(3%) = "                                       (" &       ~
                      "14)Select Archive Year (16)Exit Program"
            pfkeys$ = hex(01ffff04ffff0708ffffffff0d0e0f10171800)
            if fieldnr% <> 1% then L41240
                str(pf$(2%),18%,22%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
                goto L41250
L41240:         str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L41250:     if fieldnr% = 3% then return
                str(pf$(1%),40%,24%) = " ": str(pfkeys$,17%,1%) = hex(ff)
                str(pf$(2%),40%,24%) = " ": str(pfkeys$,18%,1%) = hex(ff)

            return

L41270: if fieldnr% > 0% then L41370  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            if pf4$ = " " then L41346
            str(pf$(2%),18%,26%) = pf4$ : str(pfkeys$,4%,1%) = hex(04)
L41346:     if pf5$ = " " then L41360
            str(pf$(3%),18%,26%) = pf5$ : str(pfkeys$,5%,1%) = hex(05)
L41360:     return
L41370:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                          (" &       ~
                     "7/23)FROM Source/Dest  (13)Instructions"
            pf$(2%) = "                                       (" &       ~
                      "8/24)TO Source/Dest    (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfkeys$ = hex(01ffffffffff0708ffffffff0dff0fff171800)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Part Code              */~
                              L50200,         /* Start Point            */~
                              L50300,         /* Variable               */~
                              L50300,         /* Variable               */~
                              L50300,         /* Variable               */~
                              L50300,         /* Variable               */~
                              L50400,         /* Break_Direct           */~
                              L50500,         /* Break_Misc Part        */~
                              L50550,         /* Detail                 */~
                              L50600          /* Page Break             */

            return

L50100: REM Test for Part Code                    PART$
            call "GETCODE" (#01, part$, partdescr$, 0%, 0, f1%(1))
                    origpart$ = part$
               if f1%(1) <> 0% then return
                  partdescr$ = "* * * Part Not on File * * *"
                  if part$ = "?" or part$ = " "  then                    ~
                      errormsg$ = "Please Enter a Part Number."
                  return

L50200: REM Test for Start Point                  WHICH$
            which% = pos(str(which$()) > hex(20))
              init (" ") str(which$(), which% + 1%)
              str(which$(),8%,2%) = "XX"
            if which% > 0% and which% < 8% then L50255
                which% = cursor%(1) - 5%
                if which% < 1% or which% > 7% then L50240
                if which% > 0% and which% < 8% then L50255
L50240:            errormsg$ = "Indicate a Desired Starting Point."
                   return
L50255:     which$(which%) = "X"

          /* Test Print Types */
            if pos(str(print$()) > hex(20)) > 0% then L50285
                errormsg$ = "At Least One Print Type Must be Selected."
                return
L50285:     if prev_which% = which% then return
                prev_which% = which%  :  init(" ") answer$()
            return

L50300: REM Test for Variable                     ANSWER$
            if prompt$(which%,fieldnr% - 2%) = "Date" then goto set_dates
            if prompt$(which%,fieldnr% - 2%) = " " then return
            if answer$((fieldnr% - 2%),1%) = "?" or                      ~
               answer$((fieldnr% - 2%),2%) = "?" then                    ~
                  errormsg$ = "No '?' Selection Screen. Please Reenter."
            call "TESTRNGE" (answer$((fieldnr% - 2%),1%),                ~
                             answer$((fieldnr% - 2%),2%),                ~
                             answer$((fieldnr% - 2%),3%),                ~
                             answer$((fieldnr% - 2%),4%), errormsg$)
            if answer$((fieldnr% - 2%),1%) <> "ALL" then return
                answer$((fieldnr% - 2%),3%) = all(hex(20))

            return

L50400: REM Test for Break_Direct                 BREAK_DIRECT$
            if break_direct$ = "Y" or break_direct$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N' Please"
                return

L50500: REM Test for Break_Misc Part              BREAK_MISC$
            if break_misc$ = "Y" or break_misc$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'  Please"
                return

L50550: REM Test for Detail                       DETAIL$
            if detail$ = "Y" or detail$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N' Please"
                return

L50600: REM Test for Page Break                   PAGE_BREAK$
            if page_break$ = "Y" or page_break$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N' Please"
                return
        set_dates
            fmdate$ =  answer$((fieldnr% - 2%), 1%)
            todate$ =  answer$((fieldnr% - 2%), 2%)
            lowdate% = 0% :  highdate% = 99999999%
            if fmdate$ = " " or fmdate$ = blankdate$ then fmdate$ = "ALL"
            if fmdate$ = "ALL" then L50800
                call "DATEOKC" (fmdate$, lowdate%, errormsg$)
                if errormsg$ <> " " then return
                if todate$   <> " " and todate$ <> blankdate$ then L50745
                     todate$ = fmdate$ : highdate% = lowdate%
                     goto L50755
L50745:     call "DATEOKC" (todate$, highdate%, errormsg$)
            if errormsg$ <> " " then return
L50755:     if lowdate% > highdate% then errormsg$ =                     ~
                "FROM Date must be LESS than or EQUAL to the TO Date"
            goto L50810

L50800:     lodate$ = "19010101"  :  hidate$ = "20991231"
            call "DATFMTC"(lodate$): call "DATFMTC"(hidate$)
            goto L50815
L50810:     lodate$ = fmdate$     :  hidate$ = todate$
L50815:     call "DATUFMTC" (lodate$)  :  call "DATUFMTC" (hidate$)
            answer$((fieldnr% - 2%), 1%) = fmdate$
            answer$((fieldnr% - 2%), 2%) = todate$
            return

        test_for_movement_within_range
            gosub new_check_start_point
            if errormsg$ <> " " then return
            if implode% + explode% > 0% then return
               errormsg$ = "Invalid Movement Range. No Records Found."
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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            if arcyear$ = "Current" then L65210
                lotdprname$ = "LOTMVDTL" : close #21
                lotmprname$ = "LOTMVMNT" : close #20
                call "PUTPRNAM" addr(#21, lotdprname$)
                call "OPENCHCK" (#21, fs%(21%), f2%(21%), 0%, rslt$(21%))
                call "PUTPRNAM" addr(#20, lotmprname$)
                call "OPENCHCK" (#20, fs%(20%), f2%(20%), 0%, rslt$(20%))
                arcyear$ = "Current"
L65210:     end
