        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  BBBB   K   K   AAA    GGG   IIIII  N   N   GGG    *~
            *  V   V  B   B  K  K   A   A  G        I    NN  N  G       *~
            *  V   V  BBBB   KKK    AAAAA  G GGG    I    N N N  G GGG   *~
            *   V V   B   B  K  K   A   A  G   G    I    N  NN  G   G   *~
            *    V    BBBB   K   K  A   A   GGG   IIIII  N   N   GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKAGING - Allows users to generate a report, by Vendor,  *~
            *            of all open P.O. lines.  This report takes the *~
            *            line item dollar amounts, and places them in   *~
            *            their respective 0-15, 16-30, and 30+ aging    *~
            *            buckets.  If you want to change the buckets,   *~
            *            it is a simple mod at about line 14000.        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/23/90 ! Original                                 ! RAN *~
            * 12/06/90 ! Changed to now call DATE subroutine to   ! WPH *~
            *          ! determine PO age.  Removed unused channel!     *~
            *          ! and calendar loading code.               !     *~
            * 01/13/93 ! Fix Page 0 Facs , & End Report Time.     ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            aging(4),                    /* Aging buckets 1, 2, 3 &tot */~
            buyers$(20)3,                /* Buyers that are behind     */~
            buyerbux(20,4),              /* Buyers' dollars late       */~
            buyer$3,                     /* Buyer ID                   */~
            buyertot(4),                 /* Buyer Totals for Summary   */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descript$30,                 /* Vendor Description         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            file$8, lib$8, vol$6,        /* File, library, volume      */~
            fmvenrange$9,                /* Vendor Range               */~
            hivenrange$9,                /* Vendor Range               */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lovenrange$9,                /* Vendor Range               */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpttitle$60,                 /* Report Title               */~
            srchptr%(1,2),               /* Search pointer for SEARCH  */~
            sort$116,                    /* Argument for SORTCALL      */~
            time$8,                      /* System Time                */~
            today$6,                     /* todays date as YYMMDD      */~
            tovenrange$9,                /* Vendor Range               */~
            userid$3                     /* Current User Id            */

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
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * #01 ! VENDOR   ! Vendor Master File                       *~
            * #02 ! VBKLINES ! Purchase Order Line Items File           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "VENDOR",                                        ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =    1, keylen =  9,                      ~
                        alt key 1, keypos = 10,   keylen = 30

            select #02, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #04, "WORKFILE",                                      ~
                        varc, consec, recsize = 41

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            today$, date$ = date
            call "DATEFMT" (date$)
            ret% = ret%
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Open Purchase Order Aging Repo" &               ~
                        "rt                            "

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "VBKAGING: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  1%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data

            call "SHOSTAT" ("Report Generation in Progress")

            call "WORKOPN2"(#4, "OUTPT", 8000%, f2%(4))

            init (hex(00)) plowkey$
            str(plowkey$,,9)=lovenrange$
L13110:     call "PLOWNEXT"(#2,plowkey$,9%,f1%(2))
               if f1%(2)<>0 then goto L13160
            gosub write_workfile
            init(hex(00)) plowkey$
            str(plowkey$,,9)= str(key(#2),,9)
               if str(plowkey$,,9)>hivenrange$ then generate_report
            call "READNEXT"(#2, f1%(2))
               if f1%(2) = 0 then generate_report
            gosub clear_amounts
            goto L13110
L13160:     get #2 using L13170, openqty, price, duedate$, buyer$
L13170:        FMT POS(109), PD(14,4), PD(14,7), POS(142), CH(6),        ~
                  POS(370), CH(3)
            if openqty = 0 then L13110

            call "DATE" addr( "G-", today$, duedate$, daydiff%, ret%)
               if daydiff% >= 0% then L13110
            daydiff% = abs(daydiff%)
            dollars = openqty*price
            if daydiff%>30 then dayptr% = 3%
            if daydiff%>15 and daydiff%<30 then dayptr% = 2%
            if daydiff%<=15 then dayptr% = 1%
            aging(dayptr%)=aging(dayptr%)+dollars
            aging(4) = aging(4)+dollars  /* Total dollars for vendor   */
            search buyers$() = buyer$ to srchptr%() step 3
               if srchptr%(1,1)<>0 then L14130
            buyers$(buyer%) = buyer$     /* Enter BUYER$ into array    */
            buyptr%=buyer% : buyer%=buyer%+1%
            goto L14140
L14130:     buyptr% = ((srchptr%(1,1)+2%)/3%)
L14140:     buyerbux(buyptr%,dayptr%) = buyerbux(buyptr%,dayptr%)+dollars
            buyerbux(buyptr%,4)=buyerbux(buyptr%,4)+dollars
            goto L13110

        write_workfile
            if aging(4) = 0  then return
            write #4 using L14180, str(plowkey$,,9), aging()
L14180:        FMT CH(9), 4*PD(14,4)
            writecount%= writecount%+1%
        return

        clear_amounts
            mat aging = zer
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100          /* Vendor Range           */
            return
L20100: REM Def/Enable Vendor Range                FMVENRANGE$
            if fmvenrange$         = " " then                            ~
               fmvenrange$         = "ALL"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Vendor Range                                           "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmvenrange$, hivenrange$, lovenrange$,             ~
                      tovenrange$
            writecount% = 0% : buyer% = 2%
            mat aging = zer
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report

            gosub sort_workfile
            call "WORKOPN2" (#4,"INPUT",0%,f2%(5))
            mat aging = zer
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("VBK008", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            printing_totals% = 0%
            if lcntr% > 56% then gosub page_head

L30120:     call "READNEXT" (#4, f1%(4))
               if f1%(4) = 0 then print_totals
            get #4 using L30142, vendor$, age1, age2, age3, total
L30142:        FMT CH(9), 4*PD(14,4)
            descript$, type$ = " "
            call "READ100" (#1, vendor$, f1%(1))
               if f1%(1)<>0 then get #1 using L30170, descript$, type$
L30170:              FMT POS(40), CH(30), POS(477), CH(4)
            print using L60170, vendor$, descript$, type$, total, age1,   ~
               age2, age3
            lcntr% = lcntr%+1%
            if mod(lcntr%,5%)<>0 then L30220
               print : lcntr% = lcntr%+1%
L30220:     if lcntr% > 50% then gosub page_head
            aging(1)=aging(1)+age1 : aging(2)=aging(2) + age2
            aging(3)=aging(3)+age3 : aging(4)=aging(4) + total
            goto L30120

        print_totals
            printing_totals% = 1%
            print using L60190
            print using L60210, aging(4), aging(1), aging(2), aging(3)
            gosub page_head  /* we'll do the totals by buyer on new page*/
            print using L60225
            print
            print
            print using L60230
            print using L60232
            print
               for i% = 1% to buyer%-1%
               print using L60240, buyers$(i%), buyerbux(i%,4),           ~
                  buyerbux(i%,1), buyerbux(i%,2), buyerbux(i%,3)
                  for j%=1% to 4%
                     buyertot(j%)=buyertot(j%)+buyerbux(i%,j%)
                  next j%
               print
               next i%
            print using L60190
            print using L60240, " ", buyertot(4), buyertot(1),            ~
               buyertot(2), buyertot(3)

*        END_REPORT                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L64990, time$  /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            close #4
            call "FILEBGON" (#4)
            goto inputmode

        page_head             /* New Page/Column Heading */
            pcntr% = pcntr% + 1%
            gosub report_title
            if pcntr% = 0% then gosub print_params
            if printing_totals% = 1% then return
            print using L60122
            print using L60130
            print using L60150
            print
            lcntr% = 1%
            return


        report_title          /* Page Heading Print Routine */
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "VBKAGING"
            print using L60110, rpttitle$, pcntr%
            print
            return

        print_params           /* Print Page Zero */
L34510:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34550
                str(i$(), i%, 1%) = hex(20)
                goto L34510
L34550:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            gosub report_title  /* on to the first real page */
            return

        sort_workfile
            if writecount% <> 0%  then goto L35080
            keyhit% = 2%
            call "ASKUSER"(keyhit%, "*** No Records Found ***",          ~
               "There were no records found that met your criteria.",    ~
               "Press any PF Key to return.")
            goto inputmode

L35080:     call "SHOSTAT"("Sorting Report Records")
            call "GETNAMES" addr(#4, file$, lib$, vol$)
            close #4

            sort$=file$
            str(sort$,9,8)=lib$
            str(sort$,17,6)=vol$
            str(sort$,23,22)=str(sort$,,22)
            str(sort$,45,9)="0034008PD"

            call "SORTCALL" addr(sort$,err%)
               if err% = 0% then return

            keyhit% = 2%
            call "ASKUSER"(keyhit%, "***  SORT FAILURE  ***",            ~
               "SORT Routine failed ...",                                ~
               "Press any PF key to acknowledge and exit.")
            goto L65000

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40075          /* Vendor Range      */
              goto L40090

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40075:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Open Purchase Order Aging Report",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Vendor Range",                               ~
               at (07,30), fac(lfac$( 1)), fmvenrange$          , ch(09),~
               at (07,56), fac(lfac$( 1)), tovenrange$          , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40215
                  call "MANUAL" ("VBKAGING") : goto L40090

L40215:        if keyhit% <> 15 then L40230
                  call "PRNTSCRN" : goto L40090

L40230:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40325     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40310
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40315
L40310:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40315:     return

L40325: if fieldnr% > 0% then L40370  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40370:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100          /* Vendor Range           */
            return
L50100: REM Test for Vendor Range                 FMVENRANGE$
            call "TESTRNGE"                                              ~
                  (fmvenrange$         , tovenrange$         ,           ~
                   lovenrange$         , hivenrange$         ,           ~
                   errormsg$)
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:VBK008

*       * Header Line 2
L60110: %                                                   #############~
        ~#################################                      PAGE: ####
L60122: %                                            Vend   Total Value  ~
        ~                                 Over
L60130: % Vendor     Vendor Name                     Type     Past Due   ~
        ~   0-15 Days    16-30 Days      30 Days
L60150: % ---------  ------------------------------  ----  ------------  ~
        ~------------  ------------  ------------
L60170: % #########  ##############################  ####  #,###,###.##  ~
        ~#,###,###.##  #,###,###.##  #,###,###.##
L60190: %                                                  ------------  ~
        ~------------  ------------  ------------
L60210: %                          *** Aging Totals ***    #,###,###.##  ~
        ~#,###,###.##  #,###,###.##  #,###,###.##
L60225: %                                                        Total Pa~
        ~st Due By Buyer
L60230: %                                   Buyer             Past Due   ~
        ~   0-15 Days    16-30 Days      30 Days
L60232: %                                    ---           ------------  ~
        ~------------  ------------  ------------
L60240: %                                    ###           #,###,###.##  ~
        ~#,###,###.##  #,###,###.##  #,###,###.##
        %** Report Title for page 0
        %############################################################

L64990:       %                            * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T  @  ########   * * * * * * * * * *

L65000: REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
