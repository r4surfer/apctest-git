        REM *************************************************************~
            *                                                           *~
            *  IIIII  N   N  V   V  PPPP   DDDD   IIIII   SSS   PPPP    *~
            *    I    NN  N  V   V  P   P  D   D    I    S      P   P   *~
            *    I    N N N  V   v  PPPP   D   D    I     SSS   PPPP    *~
            *    I    N  NN   V V   P      D   D    I        S  P       *~
            *  IIIII  N   N    V    P      DDDD   IIIII   SSS   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * INVPDISP - Display INVPOOL Info for Part Specified.       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/29/86 ! Original                                 ! ERN *~
            * 04/08/86 ! Standard Cost Project Modifications      ! ERN *~
            * 05/27/88 ! Added Inv Valuation Method to display    ! MJB *~
            * 10/10/89 ! Added Cost Type 'B'                      ! KAB *~
            * 03/03/06 ! (PAR000) CR347 Mod for New Part Number   ! RHH *~ 
            *************************************************************

        sub "INVPDISP"   (part$,         /* Part Number to Display     */~
                          str$,          /* Store Number for Display   */~
                          lot$,          /* Lot Number for Display     */~
                          #1,            /* HNYMASTR Channel           */~
                          #2,            /* HNYPOOL  Channel           */~
                          method$)       /* Val Method                 */

*        Part Number must be on file or the subroutine whips right
*        back to the caller.  Also the Caller must open the files.


        dim                                                              ~
            company$60,                  /* Company Name for Report    */~
            cursor%(2%),                 /* Cursor Position            */~
            date$8,                      /* Date for screen display    */~
            dfac$(15%)1,                 /* Display Facs               */~
            dsply$(16%)79,               /* Display Strings            */~
            descr$32,                    /* Part Description           */~
            errormsg$79,                 /* Error message              */~
            hdr$79,                      /* Screen Column Headings     */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$1,                      /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lot$6,                       /* Lot Info                   */~
            method$1,                    /* Inventory Valuation Method */~
            methdescr$30,                /* Method Description         */~
            mdescr$(11%)30,              /* Method Description lit's   */~
            part$45,                     /* Part Number        (PAR000)*/~
            pf$(3%)79, pfkeys$20,        /* PF Keys                    */~
            plowkey$99, plowkey1$99,     /* Misc Use Plow keys         */~
            qtys(6%), qtys$(6%)10,       /* Qtys for Report            */~
            rptdescr$128,                /* Report Select Criteria     */~
            runtime$8,                   /* Report Run Time            */~
            stds(12%), std$10,           /* Std Costs for Report       */~
            str$3,                       /* Store Info                 */~
            text$40,                     /* Posting Text               */~
            ttls(6%)                     /* Report Totals              */

        dim f1%(32%)                     /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "REV:01.00 03/03/06 New Part Number Mod            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! INVMASTR ! Inventory Master File            (PAR000)*~
            * #2  ! INVPOOL  ! Inventory Pool File              (PAR000)*~
            *************************************************************~


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)                          /* (PAR000) */
            str(line2$,62) = "INVPDISP: " & str(cms2v$,,8)

            hdr$ = " Qty Left " & hex(ac) & " Qty Orig " &               ~
                   hex(ac) & "Value Each" & hex(ac) & "Lst Tran" &       ~
                   hex(ac) & "Last Transaction Posting Text"

            call "COMPNAME" (12%, company$, u3%)

            call "READ100" (#1, part$, f1%(1%))             /* (PAR000) */
            if f1%(1%) = 0% then exit_program
                get #1 using L09200, part$, descr$
L09200:              FMT CH(45), CH(32)
                str(line2$,,61%) = "Part: " & part$ & " (" & str(descr$,1%,9%) & ")"

            plowkey$ = str(part$,,45) & str(str$,,3) & str(lot$,,6) &    ~
                       hex(00000000)                     
            call "PLOWNEXT" (#2, plowkey$, 54%, f1%(2%))    /* (PAR000) */
            if f1%(2) = 1% then L09380
                u3% = 2%
                call "ASKUSER" (u3%, "INVPOOL DISPLAY",                  ~
                          "There is no Pool Information for this Part",  ~
                          "Press RETURN to continue...", " ")
                goto exit_program

L09380:     mdescr$( 1) = "Average Cost"
            mdescr$( 2) = "Mod. Average Cost"
            mdescr$( 3) = "Fixed Cost"
            mdescr$( 4) = "Last Cost"
            mdescr$( 5) = "Manual Cost"
            mdescr$( 6) = "Actual Value, FIFO"
            mdescr$( 7) = "Actual Value, LIFO"
            mdescr$( 8) = "Standard LIFO"
            mdescr$( 9) = "Standard FIFO"
            mdescr$(10) = "Actual LIFO/Adj Acct"
            mdescr$(11) = "Actual FIFO/Adj Acct"

            methdescr$ = "Not Defined"
            if pos("ABFLMPRSTXY" = method$) = 0 then L10000
            methdescr$ = mdescr$(pos("ABFLMPRSTXY" = method$))

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode : first_screen
            init(" ") errormsg$, inpmessage$, dsply$()      /* (PAR000) */
            plowkey$ = str(part$,,45%) & str(str$,,3%) & str(lot$,,6%) &    ~
                       hex(00000000)
            gosub load_screen

*        Main Screen: Get Display Options
        main_screen
            gosub set_pf_main_screen
            gosub'101               /* Display & Accept Screen    */
                if keyhit%  =  1% then inputmode
                if keyhit%  =  2% then first_screen
                if keyhit%  =  5% then next_screen
                if keyhit%  = 14% then report_printing
                if keyhit%  = 16% then exit_program

        next_screen
            gosub load_screen
            goto  main_screen



        report_printing   /* Produce Report per Criteria specified     */
            page% = 0% : line% = 857%
            mat ttls = zer
            call "SHOSTAT" ("Printing LIFO/FIFO Report")
            call "SETPRNT" ("INV031", " ", 0%, 0%)
            select printer (134)
            call "TIME" (runtime$)
            plowkey1$ = str(part$,,45%) & str(str$,,3%) & str(lot$,,6%) &   ~
                        hex(00000000)                          /* (PAR000) */
            rptdescr$ = "PART: " & part$ & " DESCRIPTION: " & str(descr$,1%,12%) &   ~
                        ", STORE: " & str$ & ", LOT: " & lot$
            if lot$ = " " then rptdescr$ = rptdescr$ & " (BLANK)"
            call "STRING" addr("CT", rptdescr$, 128%)

        report_loop
            call "PLOWNEXT" (#2, plowkey1$, 54%, f1%(2%))
            if f1%(2%) = 0% then end_report
                                                              /* (PAR000)  */
            get #2 using L10470, qtys(1%), qtys(2%), std, postdate$, text$
L10470:         FMT POS(59), 3*PD(14,4), POS(179), CH(6), POS(203), CH(40)
            call "CONVERT" (qtys(1%), 2.2, qtys$(1%))
            call "CONVERT" (qtys(2%), 2.2, qtys$(2%))
            call "CONVERT" (std    , 4.4, std$    )
            call "DATEFMT" (postdate$)
            if line% > 55% then gosub page_heading
            print using L11030, qtys$(1%), qtys$(2%), std$, postdate$, text$
            ttls(1%) = ttls(1%) + qtys(1%)
            ttls(2%) = ttls(2%) + qtys(2%)
            line% = line% + 1%
            goto report_loop                                  /* (PAR000)  */

        end_report
            if line% > 55% then gosub page_heading
            for x% = 1% to 2%
                call "CONVERT" (ttls(x%), 2.2, qtys$(x%))
            next x%
            print using L11060
            print using L11090, qtys$(1%), qtys$(2%)
            print : print "** END OF REPORT **"
            close printer
            call "SETPRNT" ("INV031", " ", 0%, 1%)
            goto main_screen


        page_heading
            page% = page% + 1%
            line% = 8%
            print page
            print using L10860, date$, runtime$, company$
            print using L10890, page%
            print using L10920, rptdescr$
            print
            print using L10950
            print using L10970
            print using L11000
            return


L10860: %RUN DATE: ######## ########            #########################~
        ~###################################               INVPDISP-INV031

L10890: %                                               INVENTORY LIFO/FI~
        ~FO POOL INFORMATION                                   PAGE: ###

L10920: %################################################################~
        ~#################################################################

L10950: %                     QUANTITY   ORIGINAL                  LAST

L10970: %                    REMAINING   QUANTITY   VALUE EACH  TRANSACTI~
        ~ON  POSTING TEXT

L11000: %                   ----------  ----------  ----------  ---------~
        ~--  ----------------------------------------

L11030: %                   ##########  ##########  ##########   ########~
        ~    ########################################

L11060: %                   ----------  ----------


L11090: %        * TOTALS * ##########  ##########



        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads the Next Screen's Worth of Data.                    *~
            *************************************************************
        load_screen
            init (" ") dsply$()
            l% = 0%

L30090:     call "PLOWNEXT" (#2, plowkey$, 54%, f1%(2%))
            if f1%(2) = 1% then L30140
                l% = l% + 1%
                dsply$(l%) = "** END OF DATA **"
                return
L30140:     get #2 using L30160, qtyleft, qtyorig, std, stds(), postdate$,~
                                text$
L30160:         FMT POS(59), 15*PD(14,4), CH(6), POS(203), CH(40)
            l% = l% + 1%
            call "DATEFMT" (postdate$)
            call "CONVERT" (qtyleft, 2.2, str(dsply$(l%), 1,10))
            call "CONVERT" (qtyorig, 2.2, str(dsply$(l%),12,10))
            call "CONVERT" (std    , 4.4, str(dsply$(l%),23,10))
            str(dsply$(l%),34) = postdate$
            str(dsply$(l%),43) = text$
            if l% = 15% then return else goto L30090


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101                                  /* Display Screen   */

L40080:     accept                                                       ~
               at (01,02), "Display LIFO/FIFO Pool Information",         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), "Store:",                                     ~
               at (03,09), fac(hex(84)), str$                   , ch(03),~
               at (03,15), "Lot:",                                       ~
               at (03,20), fac(hex(84)), lot$                   , ch(06),~
               at (03,30), "Valuation Method is "               ,        ~
               at (03,51), fac(hex(84)), methdescr$             , ch(30),~
                                                                         ~
               at (04,02), fac(hex(ac)), hdr$                   , ch(79),~
                                                                         ~
               at (05,02), fac(dfac$( 1)), dsply$( 1%)          , ch(79),~
               at (06,02), fac(dfac$( 2)), dsply$( 2%)          , ch(79),~
               at (07,02), fac(dfac$( 3)), dsply$( 3%)          , ch(79),~
               at (08,02), fac(dfac$( 4)), dsply$( 4%)          , ch(79),~
               at (09,02), fac(dfac$( 5)), dsply$( 5%)          , ch(79),~
               at (10,02), fac(dfac$( 6)), dsply$( 6%)          , ch(79),~
               at (11,02), fac(dfac$( 7)), dsply$( 7%)          , ch(79),~
               at (12,02), fac(dfac$( 8)), dsply$( 8%)          , ch(79),~
               at (13,02), fac(dfac$( 9)), dsply$( 9%)          , ch(79),~
               at (14,02), fac(dfac$(10)), dsply$(10%)          , ch(79),~
               at (15,02), fac(dfac$(11)), dsply$(11%)          , ch(79),~
               at (16,02), fac(dfac$(12)), dsply$(12%)          , ch(79),~
               at (17,02), fac(dfac$(13)), dsply$(13%)          , ch(79),~
               at (18,02), fac(dfac$(14)), dsply$(14%)          , ch(79),~
               at (19,02), fac(dfac$(15)), dsply$(15%)          , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                   keys(pfkeys$),  key (keyhit%)

               if keyhit% <> 13% then L40450
                  call "MANUAL" ("HNYPDISP")
                  goto L40080

L40450:        if keyhit% <> 15% then L40490
                  call "PRNTSCRN"
                  goto L40080

L40490:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf_main_screen
           inpmessage$ = " "

           init (hex(84)) dfac$(), lfac$
           pf$(1) = "(2)First Screen                                   "&~
                    "             (13)Instructions"
           pf$(2) = "(5)Next Screen                                    "&~
                    "             (15)Print Screen"
           pf$(3) = "                 (14)Print Report                 "&~
                    "             (16)Exit Display"
           pfkeys$ = hex(ff02ffff05ffffffffffffff0d0e0f10ffffff00)
           return

        exit_program

            end
