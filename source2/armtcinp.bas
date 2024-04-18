        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  TTTTT   CCC   IIIII  N   N  PPPP    *~
            *  A   A  R   R  MM MM    T    C   C    I    NN  N  P   P   *~
            *  AAAAA  RRRR   M M M    T    C        I    N N N  PPPP    *~
            *  A   A  R   R  M   M    T    C   C    I    N  NN  P       *~
            *  A   A  R   R  M   M    T     CCC   IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMTCINP - Manages the Payment Terms Code file ARMTERMS.  *~
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
            * 05/27/86 ! ORIGINAL                                 ! ERN *~
            * 03/19/90 ! Fixed (PRR 11255 and 11335)              ! SID *~
            *          ! Disallowed using a word "DATED" as a     !     *~
            *          ! Terms code. Enabled the Terms Description!     *~
            *          ! Field and a PF10 key to allow the user   !     *~
            *          ! to reset the Term Description Field.     !     *~
            * 08/06/90 ! Removed extra CMS2V$ = so that correct   ! MJB *~
            *          !  version displays.                       !     *~
            * 11/05/91 ! PRR 10356, 11272. Added new field as to  ! JDH *~
            *          !  printing description on invoice.        !     *~
            * 07/29/96 ! Changes for the year 2000.               ! DXL *~
            * 09/19/97 ! Fiddled w/ date fields. C/b dates or not.! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparision */~
            code$20,                     /* Terms Code                 */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date$(2)8, days(2),          /* Work Variables             */~
            descr$30,                    /* Terms Description          */~
            disc$8,                      /* Cash Discount Terms        */~
            edtmessage$79,               /* Edit screen message        */~
            eom$2,                       /* EOM Date                   */~
            errormsg$79,                 /* Error message              */~
            grace$2,                     /* Cash Receipts Grace Days   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            inv_print$1,                 /* Print on invoice?          */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Screen Line         */~
            net$8,                       /* Net Payment Terms          */~
            p(1),                        /* Search receiver argument   */~
            pf4$18,  pf16$16, pf10$21,   /* PF Descriptors             */~
            pf12$16, pf14$16,            /*                            */~
            runtime$8,                   /* Report Run Time            */~
            temp$8, terms$8, type$(2)4   /* Work Variables             */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARMTERMS ! A/R Payment Terms                        *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "ARMTERMS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   1,  keylen = 20

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1), 100%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value and Press (ENTER)."

            str(line2$,62) = "ARMTCINP: " & str(cms2v$,,8)

            call "COMPNAME" (12%, company$, u3%)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, code$, disc$, net$, eom$,  ~
                      grace$, descr$, pf10$, inv_print$
            disc = 0
            call "ALLFREE"

            for fieldnr% = 1% to 8%
L10120:         gosub'051(fieldnr%)
                      if enabled% = 0 then L10260
L10140:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10210
L10170:                   fieldnr% = max(2%, fieldnr%-1%)
                          if fieldnr% = 2% then L10140
                          gosub'051(fieldnr%)
                          if enabled% = 0% then L10170
L10210:               if keyhit%  = 14 and fieldnr% = 1 then print_report
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then L10120
                gosub'151(fieldnr%, 0%)
                      if errormsg$ <> " " then L10140
L10260:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editmode
            lastfieldnr% = 0%
L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 10 then gosub describe_terms
                  if keyhit%  = 12 then gosub delete_code
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editmode
L11110:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 7 then L11125
               fieldnr% = fieldnr% - 2%
               if fieldnr% < 7 then L11060
L11125:     if fieldnr% < 2 or  fieldnr% > 8 then  editmode
            if fieldnr% = lastfieldnr%       then  editmode

L11140:     gosub'051(fieldnr%)
                  if enabled% = 0% then  editmode
L11160:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11160
                  if fieldnr% =  7 then L11210 /* No test on Description */
            gosub'151(fieldnr%, 1%)
                  if errormsg$ <> " " then L11140
L11210:               lastfieldnr% = fieldnr%
                      goto L11110

        delete_code
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "D E L E T E",                     ~
                            "Press PF-16 to continue and delete code",   ~
                            "-or-", "Press RETURN to cancel delete.")
            if keyhit1% <> 16% then return
            call "DELETE" (#1, code$, 20%)
            return clear all
            goto inputmode


        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            * --------------------------------------------------------- *~
            * Dump the file onto paper.                                 *~
            *************************************************************
        print_report
            call "SHOSTAT" ("Printing TERMS CODE Listing")
            call "SETPRNT" ("ARM001", " ", 0%, 0%)
            code$ = hex(00)
            call "READ102" (#1, code$, f1%(1))
            if f1%(1) = 0% then return

            select printer(134)
            page%, count% = 0%  :  line% = 857%
            runtime$ = " " : call "TIME" (runtime$)
            goto L12190

        report_loop
            read #1, eod goto end_of_report
L12190:     get  #1 using L12210, code$, descr$, disc, disc$, net$, eom%, ~
                                grace%, inv_print$
L12210:         FMT CH(20), CH(30), PD(14,4), 2*CH(8), 2*BI(1), CH(1)

            if line% > 55% then gosub page_heading
            eom$, temp$, grace$ = " "
            if eom%   <> 0% then convert eom% to eom$, pic(#0)
            if grace% <> 0% then convert grace% to grace$, pic(#0)
            if disc   <> 0  then convert disc to str(temp$,,5), pic(#0.00)
            if len(disc$) = 6% then call "DATEFMT" (disc$)
            if len(net$ ) = 6% then call "DATEFMT" (net$ )
            if inv_print$ = "Y" then temp2$ = "Yes" else temp2$ = " No"
            print using L12750, code$, temp$, disc$, net$, descr$, eom$,  ~
                               grace$, temp2$
            print
            line% = line% + 2
            count% = count% + 1%
            goto report_loop


        page_heading
            page% = page% + 1%
            print using L12600, date$, runtime$, company$
            print using L12630, page%
            print
            print using L12660
            print using L12690
            print using L12720
            line% = 7%
            return


        end_of_report
            print
            print using L12780, count%
            print
            print using L12790
            close printer
            call "SETPRNT" ("ARM001", " ", 0%, 1%)
            goto inputmode


L12600: %RUN DATE: ######## ########         ############################~
        ~################################                     ARMTCINP:ARM~
        ~001
L12630: %                                                    PAYMENT TERM~
        ~S CODE LISTING                                           PAGE: ##~
        ~#
L12660: %                                     Cash     Discount     Net  ~
        ~                                   EOM  Grace  Invoice

L12690: %              Terms Code           Discount%    Terms     Terms ~
        ~  Terms Description               Date   Days   Print?

L12720: %              -------------------- ---------  --------  --------~
        ~  ------------------------------  ----  -----  -------

L12750: %              ####################    #####   ########  ########~
        ~  ##############################   ##     ##     ###

L12780: %                       #### CODES LISTED
L12790: %                            END OF REPORT


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub save_data
            goto inputmode


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Terms Code       */~
                                    L20200,         /* Cash Disc %      */~
                                    L20300,         /* Cash Disc Terms  */~
                                    L20400,         /* Net Payment Terms*/~
                                    L20500,         /* EOM Date         */~
                                    L20600,         /* Grace Days       */~
                                    L21005,         /* Terms Description*/~
                                    L21050          /* Invoice Print?   */
                     return

L20100
*        Default/Enable for TERMS CODE
            inpmessage$ = "Enter Terms Code (leave blank to see Codes" & ~
                          " on file)."
            return

L20200
*        Default/Enable for CASH DISCOUNT PERCENT
            inpmessage$ = "Enter Cash Discount Percentage (0 = None)."
            return

L20300
*        Default/Enable for CASH DISCOUNT TERMS
            inpmessage$ = "Enter Cash Discount Terms."
            if disc = 0 then enabled% = 0%
            return

L20400
*        Default/Enable for NET PAYMENT TERMS
            inpmessage$ = "Enter Net Payment Terms."
            return

L20500
*        Default/Enable for EOM DATE
            inpmessage$ = "Enter End-of-Month Day (31 = actual EOM)."
            if str(disc$,5) <> "EOM" and str(net$,5) <> "EOM" then       ~
                                                           enabled% = 0%
            return

L20600
*        Default/Enable for CASH RECEIPTS GRACE DAYS
            inpmessage$ = "Enter number of allowable payment grace days."
            return

L21005
*        Default/Enable for Terms Description
            inpmessage$ = "Enter Terms description that will appear on " ~
                        & "the Sales Order Acknowledgment."
            return

L21050
*        Default/Enable for Invoice Print?
            inpmessage$ = "Should above Terms Description print on" &    ~
                          " Invoice rather than Dated Terms (Y/N)."
            if inv_print$ = " " then inv_print$ = "N"
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
L29918:     keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            if keyhit1% <> 0% then L29918
                return clear all
                goto inputmode


        REM *************************************************************~
            *                 L O A D   D A T A                         *~
            * --------------------------------------------------------- *~
            * Read in Terms from file (already in buffer).              *~
            *************************************************************
        load_data
            get #1 using L30070, descr$, disc, disc$, net$, eom%, grace%, ~
                                inv_print$
L30070:         FMT XX(20), CH(30), PD(14,4), 2*CH(8), 2*BI(1), CH(1)

        REM Are they dates or are they offsets?
            if str(disc$,5%,4%) <> "DAYS" and str(disc$,5%,4%) <> "EOM " ~
                               then call "DATEFMT" (disc$)
            if str(net$, 5%,4%) <> "DAYS" and str(net$, 5%,4%) <> "EOM " ~
                               then call "DATEFMT" (net$ )
            if eom% <> 0% then convert eom% to eom$, pic(#0)
            convert grace% to grace$, pic(#0)

            return clear all
            goto editmode


        REM *************************************************************~
            *                 S A V E   D A T A                         *~
            * --------------------------------------------------------- *~
            * Write record out to the file.                             *~
            *************************************************************
        save_data
            eom% = 0%
            if eom$ <> " " then convert eom$ to eom%, data goto L31070
L31070:     convert grace$ to grace%, data goto L31080
L31080:     if str(disc$,5) <> "EOM" and str(disc$,5) <> "DAYS" then     ~
                                                 call "DATUNFMT" (disc$)
            if str(net$ ,5) <> "EOM" and str(net$ ,5) <> "DAYS" then     ~
                                                 call "DATUNFMT" (net$ )

            call "READ101" (#1, code$, f1%(1))
            put #1 using L31160, code$, descr$, disc, disc$, net$,        ~
                                eom%, grace%, inv_print$, " "
L31160:         FMT CH(20), CH(30), PD(14,4), 2*CH(8), 2*BI(1), CH(1),   ~
                    CH(23)
            if f1%(1) = 1% then rewrite #1  else  write #1
            return


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Inputs Document for the first time.                       *~
            *************************************************************

            deffn'101(fieldnr%)                    /* INPUT Mode       */
                init(hex(8c)) lfac$()
                pf12$ = " "
                if fieldnr% = 1% then pf16$ = "(16)Exit Program"
                if fieldnr% = 1% then pf14$ = "(14)Print Report"
                pf4$ = "(4)Previous Field"
                goto L40240

            deffn'111(fieldnr%)                    /* EDIT Mode        */
                init(hex(8c)) lfac$()
                pf4$, pf10$, pf12$, pf14$, pf16$ = " "
                if fieldnr% <> 0% then L40240
                     init(hex(86)) lfac$()
                     inpmessage$ = edtmessage$
                     pf10$       = "(10)Reset Terms Descr"
                     pf12$       = "(12)Delete Code"
                     pf16$       = "(16)Save Data"
                     goto L40370

L40240:     on fieldnr%  gosub      L40330,         /* Terms Code       */~
                                    L40340,         /* Cash Disc %      */~
                                    L40330,         /* Cash Disc Terms  */~
                                    L40330,         /* Net Payment Terms*/~
                                    L40340,         /* EOM Date         */~
                                    L40340,         /* Grace Days       */~
                                    L40320,         /* Terms Description*/~
                                    L40330          /* Invoice Print?   */
            goto L40370

L40320:         lfac$(fieldnr%) = hex(80) : return    /* UpLow   */
L40330:         lfac$(fieldnr%) = hex(81) : return    /* Upper   */
L40340:         lfac$(fieldnr%) = hex(82) : return    /* Numeric */


L40370:   accept                                                         ~
            at (01,02), "Payment Terms Codes Management",                ~
            at (01,66), "Today: ",                                       ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "Terms Code",                                    ~
            at (06,30), fac(lfac$( 1)), code$                   , ch(20),~
            at (07,02), "Cash Discount Percent",                         ~
            at (07,30), fac(lfac$( 2)), disc                , pic(#0.00),~
            at (08,02), "Cash Discount Terms",                           ~
            at (08,30), fac(lfac$( 3)), disc$                   , ch(08),~
            at (08,40), "Enter Specific Date -or- number of days",       ~
            at (09,02), "Net Payment Terms",                             ~
            at (09,30), fac(lfac$( 4)), net$                    , ch(08),~
            at (09,40), "followed by either 'DAYS' or 'EOM'.    ",       ~
            at (10,02), "EOM Date",                                      ~
            at (10,30), fac(lfac$( 5)), eom$                    , ch(02),~
            at (11,02), "Cash Receipts Grace Days",                      ~
            at (11,30), fac(lfac$( 6)), grace$                  , ch(02),~
                                                                         ~
            at (13,02), "Terms Description as will"   ,                  ~
            at (14,04), "appear on Acknowledgment:",                     ~
            at (14,30), fac(lfac$( 7)), descr$                  , ch(30),~
                                                                         ~
            at (15,02), "Print Above on Invoice?",                       ~
            at (15,30), fac(lfac$( 8)), inv_print$              , ch(01),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (22,40), fac(hex(8c)), pf10$,                             ~
            at (23,20), fac(hex(8c)), pf4$,                              ~
            at (23,40), fac(hex(8c)), pf12$,                             ~
            at (22,65), "(13)Instructions",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,40), fac(hex(8c)), pf14$,                             ~
            at (24,65), fac(hex(8c)), pf16$,                             ~
                                                                         ~
                keys(hex(0001040a0c0d0e0f10)),                           ~
                key (keyhit%)

                if keyhit% <> 13 then L40780
                     call "MANUAL" ("ARMTCINP")
                     goto L40370

L40780:         if keyhit% <> 15 then L40820
                     call "PRNTSCRN"
                     goto L40370

L40820:         close ws
                call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

            deffn'151(fieldnr%, edit%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Terms Code       */~
                                    L50200,         /* Cash Disc %      */~
                                    L50300,         /* Cash Disc Terms  */~
                                    L50400,         /* Net Payment Terms*/~
                                    L50500,         /* EOM Date         */~
                                    L50600,         /* Grace Days       */~
                                         ,         /* Terms Description*/~
                                    L50700          /* Invoice Print?   */
                     return

L50100
*        Test Data for TERMS CODE
            if code$ <> "DATED" then L50110
                errormsg$ = "Please choose another Terms Code.  'DATED'" ~
                          & " is a reserve word."
                return
L50110:     if code$ = " " then L50140
                call "READ100" (#1, code$, f1%(1))
                if f1%(1) = 1% then load_data else return
L50140:     call "GETCODE" (#1, code$, " ", 0%, 0.30, f1%(1))
            if f1%(1) = 1% then load_data
                errormsg$ = hex(00)
                return

L50200
*        Test Data for CASH DISCOUNT PERCENT
            if disc >= 0% and disc <= 99.99 then L50240
                errormsg$ = "Discount % must be between 0 and 99.99"
                return
L50240:     if disc  = 0% then disc$ = " "
            gosub describe_terms : errormsg$ = " "
            if disc = 0% or(disc$ <> " " and disc$ <> blankdate$)or edit% = 0% ~
                then L50290
                errormsg$ = hex(00) : fieldnr% = 3% : return
L50290:     return

L50300
*        Test Data for CASH DISCOUNT TERMS
            terms$ = disc$ : gosub test_syntax : disc$ = terms$
            if errormsg$ <> " " then L50355
            if net$ = " " or net$ = blankdate$ then L50355
                gosub translate_terms
                if type$(1)  <>  type$(2) then L50355
                     if days(1) < days(2) then L50355
                          errormsg$ = "Discount Terms must be less"  &   ~
                                      " than Net Terms."
                          return
L50355:     gosub describe_terms
            if str(disc$,5) <>"EOM" or eom$ <>" " or edit% = 0% then L50390
                errormsg$ = hex(00) : fieldnr% = 5% : return
L50390:     return

L50400
*        Test Data for NET PAYMENT TERMS
            terms$ = net$  : gosub test_syntax : net$ = terms$
            if errormsg$ <> " " then return
            gosub translate_terms
                if type$(1)  <>  type$(2) then L50455
                     if days(2) > days(1) then L50455
                          errormsg$ = "Net Terms must be greater than" & ~
                                      " Discount Terms."
                          return
L50455:     gosub describe_terms
            if str(net$,5) <> "EOM" or eom$ <>" " or edit% = 0% then L50490
                errormsg$ = hex(00) : fieldnr% = 5% : return
L50490:     return

L50500
*        Test Data for EOM DATE
            if eom$ = " " then eom$ = "25"
            convert eom$ to temp%, data goto L50530 : goto L50540
L50530:         errormsg$ = "Invalid entry for EOM Date."  :  return
L50540:     if temp% >= 10% and temp% <= 31% then L50550
                errormsg$ = "EOM must be between 10 and 31." : return
L50550:     convert temp% to eom$, pic(00)
            return

L50600:     REM Test Data for Cash Receipts Grace Days
            if grace$ = " " then grace$ = "0"
            convert grace$ to temp%, data goto L50630 : goto L50640
L50630:         errormsg$ = "Invalid entry for GRACE Days."  :  return
L50640:     if temp% >= 0% and temp% <= 99% then L50650
                errormsg$ = "GRACE DAYS must be between 0 and 99.":return
L50650:     convert temp% to grace$, pic(#0)
            return

L50700
*        Test Data for Invoice Printing
            if inv_print$ = "Y" or inv_print$ = "N" then return
                errormsg$ = "Invoice Printing Must be 'Y' or 'N'."
                return

        test_syntax       /* Check and reformat TERMS$                 */
            if len(terms$) >= 4 then L51040
                errormsg$ = "Invalid syntax for terms.  Please Re-enter."
                return
L51040:     search terms$ = "EOM"  to p() : if p(1) <> 0% then L51110
            search terms$ = "DAYS" to p() : if p(1) <> 0% then L51180

*        Not EOM nor DAYS, assume date entered.
            call "DATEOK" (terms$, u3%, errormsg$)
            return

L51110
*        EOM Terms
            min% = 1%
            if p(1) > 1% then L51160
                errormsg$ = "Specify number of days before 'EOM'."
                return
L51160:     temp$ = "### EOM "  :  goto L51250

L51180
*        DAYS Terms
            min% = 0%
            if p(1) > 1% then L51230
                errormsg$ = "Specify number of days before 'DAYS'."
                return
L51230:     temp$ = "### DAYS"

L51250
*        Common wrap-up for EOM and DAYS terms
            convert str(terms$,,p(1)-1%) to temp%, data goto L51280
            goto L51290
L51280:         errormsg$ = "Invalid entry for number of days." : return
L51290:     if temp% >= min% and temp% <= 999% then L51340
                if min% = 0% then                                        ~
                     errormsg$ = "Days must be between 0 and 999." else  ~
                     errormsg$ = "Days must be between 1 and 999."
                return
L51340:     convert temp% to str(temp$,,3), pic(##0)
            terms$ = temp$
            return


        translate_terms
            init (" ") type$(), date$()
            mat days = zer

            terms$ = disc$ : i% = 1% : gosub L52090
            terms$ = net$  : i% = 2% : gosub L52090
            return

L52090:     type$(i%) = str(terms$,5)
            if type$(i%) <> "EOM" and type$(i%) <> "DAYS" then L52130
                convert str(terms$,,3) to days(i%), data  goto L52120
L52120:         return
L52130:     type$(i%) = "DATE"
            date$(i%) = terms$
            if date$(i%) = " " or date$(i%) = blankdate$ then return
                call "DATEOK" (date$(i%), tempday%, errormsg$)
                days(i%) = tempday%
                return


        describe_terms    /* Build as much of Description as possible. */
            gosub translate_terms
            descr$ = " "
            if disc = 0 then L53060
                convert disc to str(descr$,,5), pic(#0.00)
                descr$ = descr$ & "% " & disc$
L53060:     if net$ = " " or net$ = blankdate$ then L53090
                descr$  = descr$ & ", NET " & net$
                if disc = 0 then descr$ = str(descr$,3)
L53090:     call "SPCESMSH" (descr$, 1%)
            if str(descr$,,1) = " " then descr$ = str(descr$,2)
            if str(disc$,5) <> "EOM" and str(net$,5) <> "EOM" then       ~
                                                              eom$ = " "
            return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")

            end
