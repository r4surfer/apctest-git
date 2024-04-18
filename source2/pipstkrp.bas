        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   IIIII  PPPP    SSS   TTTTT  K   K  RRRR   PPPP    *~
            *  P   P    I    P   P  S        T    K  K   R   R  P   P   *~
            *  PPPP     I    PPPP    SSS     T    KKK    RRRR   PPPP    *~
            *  P        I    P          S    T    K  K   R   R  P       *~
            *  P      IIIII  P       SSS     T    K   K  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPSTKRP - Allows selection and printing of PIP Stock     *~
            *            Status reports. A melange of PIPDSPLY's Status *~
            *            report and PLNMGRPT's PIP Detail data.         *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/05/91 ! Original.                                ! JIM *~
            * 02/03/92 ! Skip Summary Scrn if only 1 Part selected! JIM *~
            * 02/24/92 ! Corrected TESTRNGE values & compares.    ! JIM *~
            * 02/26/92 ! Added PF(11)Accept Defaults logic.       ! JIM *~
            * 02/26/92 ! 'Any'= any status including NO condition.! JIM *~
            * 03/31/92 ! QC Re-work for R6.02.00.                 ! JIM *~
            * 06/05/92 ! PRR 12480 Part Status 'ANY' clarified.   ! JDH *~
            * 07/02/92 ! Added call to COMPCHCK.                  ! JIM *~
            * 09/14/92 ! Elim implied integer convs; Matrix GETs. ! JIM *~
            * 10/21/92 ! Removed FACs from page zero.             ! JIM *~
            * 11/05/92 ! All Qties 2 dec pl; Enhance detail report! JIM *~
            * 10/13/93 ! Purchase Job Project - Added for support ! JBK *~
            *          !   'BW' and 'RW' type PIP tags.           !     *~
            *          ! PRR 12526 - Added Askuser before program !     *~
            *          !   exit on Planning Date error.           !     *~
            *          ! PRR 12753 - Re-initialized Time before   !     *~
            *          !   printing 'END OF REPORT' line.         !     *~
            * 01/06/94 ! Added 2 delimiters for detail reporting. ! JDH *~
            * 08/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            add_wdwal$1,                 /* Detail Adds and/or Withdrls*/~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buyrrnge$(4)3, buyr$3,       /* Buyer Part Class Range     */~
            clssrnge$(4)4, clss$4,       /* Part Class Range           */~
            col_header$51,               /* Screen Column Header       */~
            company$60,                  /* Company Name               */~
            ctgyrnge$(4)4, ctgy$4,       /* Part Category Range        */~
            cumf%(490),                  /* Cumulative Forcast array   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            daternge$(4)10,              /* Date Range                 */~
            ddatrnge$(4)10,              /* Date Range for detail      */~
            dsply$(15,2)79,              /* Display Summary Line Items */~
            dtlcol1$79,                  /* Column header for PIPs     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i1$(24)80, i2$(24)80,        /* Screen Images              */~
            inpmessage$79,               /* Informational Message      */~
            l%(2),                       /* Display control            */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            loc%(2),                     /* SEARCHIN'                  */~
            partdate$(1000)8, partdate$8,/* Part Status on this date   */~
            partdesc$(1000)32, partdesc$32, /* Selected Part Descrs    */~
            partnmbr$(1000)25, partnmbr$25, /* Selected Part Numbers   */~
            partrnge$(4)25,              /* Part Number Range          */~
            partstat$(1000)3, partstat$3,/* Part Status descriptive    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$33,                   /* PF Key Hex Values          */~
            pip%(490),                   /* PIPMASTR PIPs              */~
            pipbal(2000),                /* Net of PIPINs, PIPOUTs     */~
            pipdat$(2000)8,              /* PIP Dates per Part         */~
            pipdem(2000), pipsup(2000),  /* PIPIN, PIPOUT quantities   */~
            pippar$(2000)25, pippar$25,  /* Item(s) to build           */~
            pipqty(2000),                /* PIP Quantities per Part    */~
            piptag$(2000)19,             /* PIP Tags per Part          */~
            pipitag$19, pipotag$19,      /* PIP Tags                   */~
            pipipar$25, pipopar$25,      /* PIP Parents                */~
            planday1$6,                  /* Planning Day 1             */~
            planday490$6,                /* Planning Day 490           */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkeyi$99, plowkeyo$99,    /* Miscellaneous Read/Plow Key*/~
            prtcol1$79, prtcol2$79,      /* Column header for a part   */~
            prtdsp1$79, prtdsp2$79,      /* Display lines for a part   */~
            prtqtys$60,                  /* Edited quantities for print*/~
            qty_moq(1000),               /* Selected Part Quantity MOQ */~
            qty_pip(1000),               /* Selected Part Quantity PIP */~
            qtyhand(1000),               /* Selected Part Quantity OH  */~
            qtyprob(1000),               /* Selected Part Problem Qty  */~
            qtysfty(1000),               /* Selected Part Sfty Stk Lvl */~
            rptid$6,                     /* Report ID                  */~
            schdrnge$(4)3, schd$3,       /* Prod. Scheduler Class Rnge */~
            searcherror$12,              /* SEARCHIN'                  */~
            statcode$(7)1,               /* Part Status Codes          */~
            tag_type$(9)1,               /* Type of Adds or Wdwals?    */~
            temp$25,                     /* Miscellaneous strings      */~
            test_date$8,                 /* PIP Date formatting        */~
            test_tag$19,                 /* SEARCHIN'                  */~
            time$8,                      /* Time of Day                */~
            typernge$(4)3, type$3,       /* Part Type Range            */~
            userid$3,                    /* Current User Id            */~
            x%(2), y%(2), z%(2)          /* Display control            */

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
            * #01 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #02 ! HNYMASTR ! Inventory Master File                    *~
            * #03 ! PIPIN    ! Planned inventory additions detail       *~
            * #04 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #07 ! SYSFILE2 ! Caelus Management System Information     *~
            * #08 ! JBMASTR2 ! Production Job Master File               *~
            * #10 ! PIPIN    ! Planned inventory additions detail       *~
            * #41 ! SFCUM2   ! Cumulative Sales Forecast File           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #02, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #03, "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #04, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #07, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #08, "JBMASTR2",                                      ~
                        varc, indexed,  recsize =  1300,                 ~
                        keypos =    1, keylen =   8

            select #10, "PIPIN",          /* Alternate UFB for READ100 */~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #41, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25

            call "SHOSTAT" ("Opening files; one moment please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "OPENCHCK" (#08, fs%(08%), f2%(08%), 0%, rslt$(08%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#41, fs%(41%), f2%(41%), 0%, rslt$(41%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr ("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            edtmessage$ = "To modify displayed values, position cursor "&~
                "to desired value & press (RETURN)."
            str(line2$,62%) = "PIPSTKRP: " & str(cms2v$,,8%)
            col_header$ = "From                      To"
            prtcol1$ = "Part Number               Sta   xxxxxxxx      S"&~
                "Stk   On Hand       MOQ"
            prtcol2$ = "Part Description               xxxxxxxxx       "&~
                "          PIP   Problem   Date"
            dtlcol1$ = "--- Tag Number ----    Quantity Parent         "&~
                "           Due Date      Balance"
            max_parts% = dim(partnmbr$(),1%)
            max_lines% = dim(piptag$(),1%)
            l%(1%) =  8%                 /* # Parts per Summary Screen */
            l%(2%) = 13%         /* # PIP Line Items per Detail Screen */

            call "PIPINDEX" (#07, " ", today%, err%)
            if err% <> 0% then goto error_exit
            call "DATE" addr ("G+", date, 1% - today%, planday1$, err%)
            if err% <> 0% then goto error_exit
            call "DATE" addr ("G+", date, 490% - today%, planday490$,    ~
                err%)
            if err% <> 0% then goto error_exit

            call "COMPNAME" (12%, company$, err%)
            max_count% = 55%    /* Max number of print lines on a page */
            rptid$ = "PLN005"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 11%
L10100:         gosub'051(fieldnr%, 0%)           /* Default / Enables */
                     if enabled% = 0% then goto L10330
L10120:         gosub'202(1%, 1%)                 /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then goto L10210
L10150:                   errormsg$ = " "
L10160:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%, 0%)
                          if enabled% = 1% then goto L10120
                          if fieldnr% = 1% then goto L10100
                          goto L10160
L10210:              if keyhit% <> 10% then goto L10300
L10220:                   if fieldnr% = 1%                               ~
                               then gosub'202(2%, 1%)                    ~
                               else gosub'202(3%, 1%)
                          if keyhit% =  1% then gosub startover
                          if keyhit% =  4% then goto L10150
                          if keyhit% = 10% then goto L10120
                          if keyhit% = 11% then goto accept_defaults
                          if keyhit% <> 0% then goto L10220
L10300:              if keyhit% = 11% then goto accept_defaults
                     if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then goto L10120
L10330:         gosub'151(fieldnr%, 0%)  /* Edit Field for Valid Entry */
                if errormsg$ = " " then goto L10360
                     if a% = 1% then goto L10120 else goto L10220
L10360:     next fieldnr%
            goto editpg1

        accept_defaults
            gosub'051(fieldnr%, 1%)   /* Default all higher FIELDNR%'s */
            gosub'151(fieldnr%, 1%)     /* Validate current and higher */
            if errormsg$ = " " then goto editpg1   /* Valid?- EDITMODE */
            if a% = 1% then goto L10120 else goto L10220/* No- INPUTMODE */

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)             /* Display Screen - No Entry */
            function% = keyhit%
                if keyhit% =  1% then gosub startover
                if keyhit% = 16% then goto lets_rock_n_roll
                if keyhit% = 32% then goto lets_rock_n_roll
                if keyhit% <> 0% then goto editpg1
L11140:     if cursor%(1%) <  6% or cursor%(1%) > 19% then goto editpg1
            if cursor%(1%) =  8% or cursor%(1%) =  9% then goto editpg1
            if cursor%(1%) = 17% then goto editpg1
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% > 2% then fieldnr% = fieldnr% - 2%
            if fieldnr% > 9% then fieldnr% = fieldnr% - 1%
            if fieldnr% = lastfieldnr% then goto editpg1
            gosub'051(fieldnr%, 0%)     /* Check Enables, Set Defaults */
                if enabled% =  0% then goto editpg1
L11210:     gosub'202(1%, 2%)               /* Display & Accept Screen */
                if keyhit%  =  1% then gosub startover
                if keyhit% <> 10% then goto L11300
L11240:              if fieldnr% = 1%                                    ~
                          then gosub'202(2%, 2%)                         ~
                          else gosub'202(3%, 2%)
                     if keyhit% =  1% then gosub startover
                     if keyhit% = 10% then goto L11210
                     if keyhit% <> 0% then goto L11240
L11300:         if keyhit% <>  0% then goto L11210
            gosub'151(fieldnr%, 0%)      /* Edit Field for Valid Entry */
                if errormsg$ = " " then goto L11340
                     if a% = 1% then goto L11210 else goto L11240
L11340:         lastfieldnr% = fieldnr%
            goto L11140

        REM *************************************************************~
            * Report/Display processing occurs here.                    *~
            *************************************************************

        lets_rock_n_roll
            if keyhit% <> 32% then goto L12080
                gosub determine_report_type
                if u% = 1% then goto editpg1
L12080:     call "SHOSTAT" ("Part Data Selection in Progress ... Please s~
        ~tand by.")
            gosub initialize_routine

        plow_pipmastr /* Tag says it all. We're looking for candidates */
*        Can the part be eliminated based on Part # range?
            call "PLOWNEXT" (#01, plowkey$, 0%, f1%(1%))   /* PIPMASTR */
                if f1%(1%) = 0% then goto end_of_plow
            call "MXFL4GT" addr (#01, 26%, pip%(1%), 490%)
            get #01 using L12180, partnmbr$, qtyhand, qtysfty, qty_moq
L12180:         FMT POS(2), CH(25), POS(1987), 3*PD(14,4)
            if partnmbr$ > partrnge$(4%) then goto end_of_plow
            qtyhand = round(qtyhand, 2)
            qty_moq = round(qty_moq, 2)
            qtysfty, safety% = round(qtysfty, 2)
            surpls% = qtysfty + qty_moq

*        Can the part be eliminated via HNYMASTR fields vs. ranges?
            call "READ100" (#02, partnmbr$, f1%(2%))       /* HNYNASTR */
                if f1%(2%) = 0% then goto plow_pipmastr       /* Ouch! */
            get #02 using L12300, partdesc$, ctgy$, clss$, type$, buyr$,  ~
                schd$
L12300:         FMT POS(26), CH(32), POS(90), CH(4), POS(133), CH(4),    ~
                     POS(180), CH(3), POS(200), CH(3), POS(309), CH(3)
            if ctgy$ <= ctgyrnge$(3%) then goto plow_pipmastr
            if ctgy$ >  ctgyrnge$(4%) then goto plow_pipmastr
            if clss$ <= clssrnge$(3%) then goto plow_pipmastr
            if clss$ >  clssrnge$(4%) then goto plow_pipmastr
            if type$ <= typernge$(3%) then goto plow_pipmastr
            if type$ >  typernge$(4%) then goto plow_pipmastr
            if buyr$ <= buyrrnge$(3%) then goto plow_pipmastr
            if buyr$ >  buyrrnge$(4%) then goto plow_pipmastr
            if schd$ <= schdrnge$(3%) then goto plow_pipmastr
            if schd$ >  schdrnge$(4%) then goto plow_pipmastr

*        Can the part be eliminated based on status?
            partstat$, partdate$ = " "
            for s% = 1% to 7%  /* Check the 7 possible status requests */
                if statcode$(s%) = " " then goto L12680 /* Not requested*/
                on s% goto status, mfd, pur, mfd, pur, mfd, pur
                goto L12680                                    /* Error */

        mfd      /* The part remains a candidate if it is manufactured */
                if type$ < "500" then goto L12680
                if type$ > "789" and type$ < "800" then goto L12680
                goto status

        pur         /* The part remains a candidate if it is purchased */
                if type$ < "200" or type$ > "489" then goto L12680

        status /* Part remains a candidate if it has a selected status */
                mat cumf% = zer : qtyprob = 0
                stat% = 0%              /* Indicate 'no hit on status' */
                call "READ100" (#41, partnmbr$, f1%(41%))    /* SFCUM2 */
                if f1%(41%) <> 0% then call "MXFL4GT" addr (#41, 25%,    ~
                     cumf%(1%), 490%)
                on s% gosub test_any, test_surplus, test_surplus,        ~
                     test_safety_stk, test_safety_stk, test_shortage,    ~
                     test_shortage
                if stat% <> 0% then goto L12710     /* Part is SELECTED */
L12680:     next s%
            goto plow_pipmastr /* Part is REJECTED; Keep on keepin' on */

L12710
*        Well, OK -- this part has been selected. Store it in arrays.
            nbr_parts% = nbr_parts% + 1%
            if nbr_parts% <= max_parts% then goto L12760
                nbr_parts% = nbr_parts% - 1%
                goto end_of_plow
L12760:     partnmbr$(nbr_parts%) = partnmbr$
            partdesc$(nbr_parts%) = partdesc$
            partdate$(nbr_parts%) = partdate$
            partstat$(nbr_parts%) = partstat$
            qtyhand(nbr_parts%) = qtyhand
            qty_moq(nbr_parts%) = qty_moq
            qty_pip(nbr_parts%) = pip%(j%)
            qtysfty(nbr_parts%) = qtysfty
            qtyprob(nbr_parts%) = qtyprob
            goto plow_pipmastr                   /* Keep on keepin' on */

        end_of_plow
            if nbr_parts% > 0% then goto print_or_display
                call "ASKUSER" (2%, "*** NULL SET SELECTED ***",         ~
                     "There are no Parts that satisfy your criteria.",   ~
                     " ", "Press (RETURN) to acknowledge and continue.")
                goto inputmode

        print_or_display
            if function% = 16% then goto display_routine
            gosub print_routine
            goto inputmode

        REM *************************************************************~
            *    S E L E C T E D   D A T A   P R I N T E D   H E R E    *~
            *************************************************************

        print_routine
            gosub set_up_for_print
            gosub print_page_0
            goto print_part_loop_2

        print_part_loop
            y%(1%) = y%(1%) + 1%                   /* Access next Part */
        print_part_loop_2
            if y%(1%) > nbr_parts% then goto end_of_report
            detl_pipsw% = 0%  /* Reset Detail PIP column header switch */
            if linecount% > max_count% then gosub page_heading
            if qtyhand(y%(1%)) < 0                                       ~
                then convert qtyhand(y%(1%)) to str(prtqtys$, 1%,12%),   ~
                     pic (-####,###.##)                                  ~
                else convert qtyhand(y%(1%)) to str(prtqtys$, 1%,12%),   ~
                     pic (#####,###.##)
            convert qtysfty(y%(1%)) to str(prtqtys$,13%,12%),            ~
                pic (#####,###.##)
            if qty_pip(y%(1%)) < 0                                       ~
                then convert qty_pip(y%(1%)) to str(prtqtys$,25%,12%),   ~
                     pic (-####,###.##)                                  ~
                else convert qty_pip(y%(1%)) to str(prtqtys$,25%,12%),   ~
                     pic (#####,###.##)
            convert qty_moq(y%(1%)) to str(prtqtys$,37%,12%),            ~
                pic (#####,###.##)
            if qtyprob(y%(1%)) < 0                                       ~
                then convert qtyprob(y%(1%)) to str(prtqtys$,49%,12%),   ~
                     pic (-####,###.##)                                  ~
                else convert qtyprob(y%(1%)) to str(prtqtys$,49%,12%),   ~
                     pic (#####,###.##)
            print using L60220, partnmbr$(y%(1%)), partdesc$(y%(1%)),     ~
                partstat$(y%(1%)), prtqtys$, partdate$(y%(1%))
            linecount% = linecount% + 1%
            if detail% <> 1% then goto print_part_loop
            pipbal = qtyhand(y%(1%))       /* Detail beginning balance */

L13400
*        Access and print the Part's applicable PIPs here.
            gosub get_part_detail_data
            if f3% <> 0% or f4% <> 0% then goto L13470
                if detl_pipsw% = 0% then goto print_part_loop
                     print
                     linecount% = linecount% + 1%
                     goto print_part_loop
L13470:     if linecount% > max_count% then gosub page_heading
            if detl_pipsw% <> 0% then goto L13540
                print
                print using L60250                     /* Column Header */
                print using L60270                        /* Underscore */
                linecount% = linecount% + 3%
                detl_pipsw% = 1%
L13540:     pipbal = pipbal + (pipsup(1%) - pipdem(1%))
            if pipsup(1%) - pipdem(1%) < 0                               ~
                then convert pipsup(1%) - pipdem(1%) to str(temp$,,12),  ~
                     pic (-####,###.##)                                  ~
                else convert pipsup(1%) - pipdem(1%) to str(temp$,,12),  ~
                     pic (#####,###.##)
            print using L60290, str(piptag$(1%)), str(temp$,,12%),        ~
                pippar$(1%), pipdat$(1%), pipbal
            linecount% = linecount% + 1%
            goto L13400

        end_of_report
            if linecount% > max_count% then gosub page_heading
            print
            print using L60330, tot_ok, tot_sur, tot_cs, tot_ssi
            print
            time$ = " " : call "TIME" (time$)
            print using L60350, time$                  /* End of Report */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            printsw% = 0%               /* Indicate we're NOT printing */
            return

        REM *************************************************************~
            *  S E L E C T E D   D A T A   D I S P L A Y E D   H E R E  *~
            *************************************************************

        display_routine
            gosub set_up_for_display
            if nbr_parts% = 1% then goto detail_display_routine

        summary_display_routine
            gosub L44000
            if keyhit% =  1% then gosub startover
            if keyhit% =  2% then y%(1%) = 1%
            if keyhit% =  3% then y%(1%) = z%(1%)
            if keyhit% =  4% then y%(1%) = max(1%, y%(1%) - l%(1%))
            if keyhit% =  5% then y%(1%) = min(z%(1%), y%(1%) + l%(1%))
            if keyhit% =  6% then y%(1%) = max(1%, y%(1%) - 1%)
            if keyhit% =  7% then y%(1%) = min(z%(1%), y%(1%) + 1%)
            if keyhit% =  8% then goto search_routine
            if keyhit% <> 0% then goto L14270
                if cursor%(1%) < 5% or cursor%(1%) > 20%                 ~
                     then goto summary_display_routine
                if mod(cursor%(1%), 2%) <> 0%                            ~
                     then cursor%(1%) = cursor%(1%) + 1%
                if y%(1%) + ((cursor%(1%) / 2%) - 2%) - 1% > nbr_parts%  ~
                     then goto summary_display_routine
                y%(1%) = y%(1%) + ((cursor%(1%) / 2%) - 2%) - 1%
                goto detail_display_routine
L14270:     if keyhit% <>32% then goto L14330
                y1% = y%(1%)                    /* Save where we're at */
                gosub determine_report_type
                if u% = 1% then goto summary_display_routine
                     gosub print_routine
                     y%(1%) = y1%                   /* Then restore it */
L14330:     if keyhit% <>16%                                             ~
                then goto summary_display_routine                        ~
                else goto inputmode

        search_routine
            temp$ = " "
        search_routine_2
            gosub L44350
            searcherror$ = " "
            if keyhit% =  1% then goto summary_display_routine
            if str(i2$(24%),50%,25%) = " "                               ~
                then goto summary_display_routine
            if keyhit% <> 0% then goto search_routine_2
                mat loc% = zer
                temp$ = str(i2$(24%),50%,25%)
                search str(partnmbr$()) = temp$ to loc%() step 25%
                if loc%(1%) <> 0% then goto L14520
                     searcherror$ = hex(94) & "Not Found!" & hex(8c)
                     goto search_routine_2
L14520:         y%(1%) = loc%(1%) / 25% + 1%
                goto summary_display_routine

        detail_display_routine
            gosub get_part_detail_data
            y%(2%) = 1%/* Start new Parts at the top of their PIP items */
        detail_display_routine_2
            gosub L46000
            errormsg$ = " "
            if keyhit% =  1% then gosub startover
            if keyhit% =  2% then y%(2%) = 1%
            if keyhit% =  3% then y%(2%) = z%(2%)
            if keyhit% =  4% then y%(2%) = max(1%, y%(2%) - l%(2%))
            if keyhit% =  5% then y%(2%) = min(z%(2%), y%(2%) + l%(2%))
            if keyhit% =  6% then y%(2%) = max(1%, y%(2%) - 1%)
            if keyhit% =  7% then y%(2%) = min(z%(2%), y%(2%) + 1%)
            if keyhit% <> 0% then goto L14770
                if cursor%(1%) < 8% or cursor%(1%) > 20%                 ~
                    then goto detail_display_routine_2
                if y%(2%) + cursor%(1%) - 8% > nbr_lines% or             ~
                    y%(2%) + cursor%(1%) - 8% < 1%                       ~
                    then goto detail_display_routine_2
                call "COMPCHCK" (piptag$(y%(2%) + cursor%(1%) - 8%),     ~
                    #04, #02, #07, #03)
                goto detail_display_routine_2
L14770:     if keyhit% <> 18% then goto L14800
                y%(1%) = 1%
                goto detail_display_routine
L14800:     if keyhit% <> 19% then goto L14830
                y%(1%) = nbr_parts%
                goto detail_display_routine
L14830:     if keyhit% <> 20% then goto L14860
                y%(1%) = max(1%, y%(1%) - 1%)
                goto detail_display_routine
L14860:     if keyhit% <> 21% then goto L14890
                y%(1%) = min(nbr_parts%, y%(1%) + 1%)
                goto detail_display_routine
L14890:     if keyhit% <> 32% then goto L14970
                y1% = y%(1%) : y2% = y%(2%)     /* Save where we're at */
                gosub determine_report_type
                if u% = 1% then goto detail_display_routine_2
                     gosub print_routine
                     y%(1%) = y1%                   /* Then restore it */
                     gosub get_part_detail_data
                     y%(2%) = y2%
L14970:     if keyhit% <> 16% then goto detail_display_routine_2
                y%(1%) = min(y%(1%), z%(1%))
                goto summary_display_routine

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        initialize_routine
            plowkey$ = xor plowkey$
            str(plowkey$,,25%) = partrnge$(3%) /* Start PIPMASTR here */
            nbr_parts% = 0%
            tot_ok, tot_sur, tot_cs, tot_ssi = 0
            init (" ") partnmbr$(), partdesc$(), partdate$(), partstat$()
            mat qtyhand = zer
            mat qty_moq = zer
            mat qty_pip = zer
            mat qtyprob = zer
            mat qtysfty = zer
            return

        set_up_for_display
            x%(1%) = nbr_parts%
            z%(1%) = max(1%, x%(1%)-l%(1%)+1%)/* Highest top of screen */
            goto common_set_up

        determine_report_type
            detail% = 0%     /* Initialize type of report to 'Summary' */
            u% = 2%         /* Window at bottom covers enabled PF keys */
            call "ASKUSER" (u%, "*** WHAT TYPE OF REPORT? ***",          ~
                "Press PF(8) for a Summary report.",                     ~
                "Press PF(12) for a Detail report.",                     ~
                "Press PF(1) to cancel report.")
            if u% =  1% then return
            if u% <> 8% then goto L16330
                detail% = 0%                                /* Summary */
                return
L16330:     if u% <> 12% then goto determine_report_type
                detail% = 1%                                 /* Detail */
                return

        set_up_for_print
            if detail% = 1%                                              ~
                then call "SHOSTAT" ("PIP Stock Analysis Detail Report "&~
                     "in Progress")                                      ~
                else call "SHOSTAT" ("PIP Stock Analysis Summary Report"&~
                     " in Progress")
            printsw% = 1%                   /* Indicate we're printing */
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            linecount% = 999%
            page_nbr% = 0%
            time$ = " " : call "TIME" (time$)

        common_set_up
            y%(1%), y%(2%) = 1%              /* Start at the beginning */
            return

        print_page_0
            pagesw% = 1%
            gosub page_0_heading
            pagesw% = 0%
L16580:     i% = pos(str(i1$()) > hex(7f))
            if i% = 0% then goto L16620
                str(i1$(), i%, 1%) = " "
                goto L16580
L16620:     print skip (4)
            print using L60310, "            ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L60310, i1$(n%)   /* Selection Screen lines */
            next n%
            return

        page_heading
            page_nbr% = page_nbr% + 1%
            linecount% = 7%
        page_0_heading
            print page
            print using L60040, date$, time$, company$, "-" & rptid$
            if detail% = 1%                              /* Sub-Header */~
                then print using L60070, page_nbr%          /* 'Detail' */~
                else print using L60100, page_nbr%         /* 'Summary' */
            if pagesw% <> 0% then return
            print
            print using L60130                        /* Column Headers */
            print using L60160                        /* Column Headers */
            print using L60190                           /* Underscores */
            print
            detl_pipsw% = 0%  /* Reset Detail PIP Column Header switch */
            return

        deffn'200(test_tag$,d%,q)/*Will we keep this PIPIN/OUT record? */
            toss% = 1%              /* Indicate 'toss this record out' */
            if add_wdwal$ = "B" then L16898 /* No Add/Wdwal Delimiting. */
                if (q < 0 and add_wdwal$ = "A") or  /* Addition Delimit*/~
                   (q > 0 and add_wdwal$ = "W") then return /* W Delimit*/
L16898:     if d% < ddat1% or d% > ddat2% then return /* Date Delimit */
            mat loc% = zer   /* 'xx' below stands for 'SO' Sales Order */
            search "JOROWOPOxxQCBOBWRW" = str(test_tag$,,2%) to loc%()   ~
                step 2%
            if loc%(1%) = 0%    /* Derive subscript from SEARCH result */~
                then loc%(1%) = 5%  /* 0 assumes 5 -- 'SO' Sales Order */~
                else loc%(1%) = loc%(1%) / 2% + 1%  /* Else compute it */
            if tag_type$(loc%(1%)) = " " then return/* TOSS the record */
            toss% = 0%    /* We're going to use (NOT toss) this record */
            pippar$ = " "  /* Initialize the general 'Parent' variable */
            if str(test_tag$,,2%) = "JO" then gosub jo/*See if there's */
            if str(test_tag$,,2%) = "WO" then gosub wo/*a Parent Part# */
            return

        jo     /* Read the JBMASTR2 file to get the Parent Part Number */
            call "READ100" (#08, str(test_tag$,12%,8%), f1%(8%))
            if f1%(8%) <> 0% then get #08 using L17060, pippar$
L17060:         FMT POS(58), CH(25)
            return

        wo        /* Read the PIPIN file to get the Parent Part Number */
            call "READ100" (#10, test_tag$, f1%(10%))
            if f1%(10%) <> 0% then get #10 using L17120, pippar$
L17120:         FMT CH(25)
            return

        deffn'201(testdate%)  /* Get & Edit a date based on Plan Day 1 */
            test_date$ = " "
            call "DATE" addr ("G+", planday1$, testdate% - 1%,           ~
                str(test_date$,,6%), u3%)
            if u3% = 0% then call "DATEFMT" (test_date$)
            return

        deffn'202(a%, b%) /* INPUT or EDITMODE- retain the screen shown */
            on a% goto L17250, L17260, L17270
            return
L17250:     gosub'101(fieldnr%, b%) : return
L17260:     gosub L42000             : return
L17270:     gosub L43000             : return

        store_pipin   /* A PIPIN record is stored for display or print */
            piptag$(nbr_lines%) = pipitag$
            pippar$(nbr_lines%) = pipipar$
            gosub'201(pipidat%)
            pipdat$(nbr_lines%) = test_date$
            pipqty (nbr_lines%) = pipiqty
            pipsup (nbr_lines%) = pipiqty
            if nbr_lines% = 1%                                           ~
                then pipbal(nbr_lines%) = qtyhand(y%(1%)) + pipiqty      ~
                else pipbal(nbr_lines%) = pipbal(nbr_lines% - 1%) +      ~
                     pipiqty
            on_order = on_order + pipiqty
            return

        store_pipout /* A PIPOUT record is stored for display or print */
            piptag$(nbr_lines%) = pipotag$
            pippar$(nbr_lines%) = pipopar$
            gosub'201(pipodat%)
            pipdat$(nbr_lines%) = test_date$
            pipqty (nbr_lines%) = pipoqty
            pipdem (nbr_lines%) = pipoqty
            if nbr_lines% = 1%                                           ~
                then pipbal(nbr_lines%) = qtyhand(y%(1%)) - pipoqty      ~
                else pipbal(nbr_lines%) = pipbal(nbr_lines% - 1%) -      ~
                     pipoqty
            committed = committed + pipoqty
            return

        get_part_detail_data /* Get PIPIN or PIPOUT records for caller */
*        If caller is the display routine, max PIPs is the array limit.
*        If caller is the print routine, pass back unlimited # of PIPs.
            nbr_lines% = 0%
            init (" ") pipdat$(), piptag$(), pippar$()
            mat pipqty = zer
            mat pipdem = zer
            mat pipsup = zer
            mat pipbal = zer
            if printsw% = 0% then goto L17680
                if detl_pipsw% <> 0% then goto L17740
L17680:     plowkeyi$ = xor plowkeyi$
            plowkeyo$ = xor plowkeyo$
            pipidat%, pipodat% = 0%
            f3%, f4% = 1%                             /* Read 'em both */
            committed, on_order = 0
            str(plowkeyi$,,25%), str(plowkeyo$,,25%) = partnmbr$(y%(1%))

L17740
*        PIPIN, PIPOUT processing begins here.
*        Gets the next record from each file and passes back the right 1.
            if f3% = 0% then goto L17880
L17770:     call "PLOWALTS" (#03, plowkeyi$, 1%, 25%, f1%(3%))/* PIPIN */
            if f1%(3%) <> 0% then goto L17810
                pipidat% = 999%
                if pipodat% = 999% then goto end_of_pips else goto L17880
L17810:     get #03 using L17820, pipidat%, pipitag$, pipiqty
L17820:         FMT POS(26), BI(4), CH(19), PD(14,4)
            pipiqty = round(pipiqty, 2)
            gosub'200(pipitag$, pipidat%, pipiqty)        /* Keep it ? */
            if toss% <> 0% then goto L17770                       /* No */
            pipipar$ = pippar$

L17880:     if f4% = 0% then goto L18000
L17890:     call "PLOWALTS" (#04, plowkeyo$, 1%, 25%, f1%(4%))/* PIPOUT */
            if f1%(4%) <> 0% then goto L17930
                pipodat% = 999%
                if pipidat% = 999% then goto end_of_pips else goto L18000
L17930:     get #04 using L17940, pipotag$, pipodat%, pipoqty
L17940:         FMT CH(19), POS(45), BI(4), POS(57), PD(14,4)
            pipoqty = round(pipoqty, 2)
            gosub'200(pipotag$, pipodat%, -pipoqty)       /* Keep it ? */
            if toss% <> 0% then goto L17890                       /* No */
            pipopar$ = pippar$

L18000
*        Here we decide which record to pass to caller, based on dates.
            nbr_lines% = nbr_lines% + 1%
            if nbr_lines% <= max_lines% then goto L18050
                nbr_lines% = nbr_lines% - 1%
                goto end_of_pips
L18050:     if pipidat% > pipodat% then goto L18090
                gosub store_pipin
                f3% = 1% : f4% = 0%
                if printsw% <> 0% then return else goto L17740
L18090:     gosub store_pipout
            f4% = 1% : f3% = 0%
            if printsw% <> 0% then return else goto L17740

        end_of_pips
            f3%, f4% = 0%                        /* That's all, folks! */
            x%(2%) = nbr_lines%
            z%(2%) = max(1%, x%(2%)-l%(2%)+1%)/* Highest top of screen */
            return

        test_any/* Part is a candidate if ANY (or no) condition exists */
*        I.e., ALL parts are candidates. We just have to determine why.
            for j% = date1% to date2%
                if pip%(j%) > (surpls% + max(0%, cumf%(j%))) then L18320
                if pip%(j%) - max(0%, cumf%(j%)) < 0% then L18540
                if pip%(j%) - max(0%, cumf%(j%)) < safety% then L18440
            next j%
            tot_ok = tot_ok + 1
            goto report_the_part    /* No exceptional condition exists */

        test_surplus   /* Part remains a candidate if it has a surplus */
            for j% = date1% to date2%
                if pip%(j%) <= (surpls% + max(0%, cumf%(j%))) then L18360
L18320:              qtyprob = pip%(j%) - (surpls% + max(0%, cumf%(j%)))
                     partstat$ = "Sur"
                     tot_sur = tot_sur + 1
                     goto report_the_part
L18360:     next j%
            return           /* No Surplus condition during date range */

        test_safety_stk  /* Part remains a candidate if it has a S.S.I */
            for j% = date1% to date2%
*        However, we don't want to report Shortages as Intrusions.
                if pip%(j%) - max(0%, cumf%(j%)) <  0% then goto L18480
                if pip%(j%) - max(0%, cumf%(j%)) >= safety% then L18480
L18440:              qtyprob = safety% - (pip%(j%) - max(0%, cumf%(j%)))
                     partstat$ = "Int"
                     tot_ssi = tot_ssi + 1
                     goto report_the_part
L18480:     next j%
            return      /* No Safety Stock Intrusion during date range */

        test_shortage /* Part remains a candidate if it has a Shortage */
            for j% = date1% to date2%
                if pip%(j%) - max(0%, cumf%(j%)) >= 0% then goto L18580
L18540:              qtyprob = pip%(j%) - max(0%, cumf%(j%))
                     partstat$ = "C/S"
                     tot_cs = tot_cs + 1
                     goto report_the_part
L18580:     next j%
            return          /* No Shortage condition during date range */

        report_the_part      /* Note that 'J%' should not be disturbed */
*        Part has the requested status. Indicate such. Get date.
            stat% = 1%               /* Indicate 'got a HIT on status' */
            call "DATE" addr ("G+", planday1$, j%-1%, str(partdate$,,6%),~
                err%)
            call "DATEFMT" (partdate$)
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%, dflt%)
            enabled% = 1%
*        Are we 'Accepting Defaults' or capturing one at a time?
            if dflt% = 0% then goto L20220          /* Accept Defaults? */
                on fieldnr% goto L20120, L20130, L20140, L20150, L20160,      ~
                     L20170, L20180, L20190, L20192, L20194, L20200   /* Yes */
L20120:         gosub L20380                  /* Part Status            */
L20130:         gosub L20420                  /* Part Number Range      */
L20140:         gosub L20460                  /* Date Range             */
L20150:         gosub L20530                  /* Part Type Range        */
L20160:         gosub L20570                  /* Part Category Range    */
L20170:         gosub L20610                  /* Part Class Range       */
L20180:         gosub L20650                  /* Buyer Part Class Range */
L20190:         gosub L20690                  /* Prod. Scheduler Class  */
L20192:         gosub L20730                  /* Adds/Wdwals Delimit    */
L20194:         gosub L20760                  /* Detail Date Range      */
L20200:         return

L20220
*        No- capturing fields one at a time.
            on fieldnr% gosub L20340,         /* Type of Adds or Wdwals?*/~
                              L20380,         /* Part Status            */~
                              L20420,         /* Part Number Range      */~
                              L20460,         /* Date Range             */~
                              L20530,         /* Part Type Range        */~
                              L20570,         /* Part Category Range    */~
                              L20610,         /* Part Class Range       */~
                              L20650,         /* Buyer Part Class Range */~
                              L20690,         /* Prod. Scheduler Class  */~
                              L20730,         /* Adds/Wdwals Delimits   */~
                              L20760          /* Detail Date Range      */
            return

L20340: REM Def/Enable Type of Adds or Wdwals?     TAG_TYPE$()
            if str(tag_type$()) = " " then init ("X") tag_type$()
            return

L20380: REM Def/Enable Part Status                 STATCODE$()
            if str(statcode$()) = " " then str(statcode$()) = " XXXXXX"
            return

L20420: REM Def/Enable Part Number Range           PARTRNGE$()
            if str(partrnge$()) = " " then partrnge$(1%) = "ALL"
            return

L20460: REM Def/Enable Date Range                  DATERNGE$()
            if str(daternge$()) <> " " and ~
                daternge$(1%) <> blankdate$ and ~
                daternge$(2%) <> blankdate$ and ~
                daternge$(3%) <> blankdate$ and ~
                daternge$(4%) <> blankdate$ then return
                daternge$(1%) = date
                call "DATFMTC" (daternge$(1%))
                daternge$(2%) = planday490$
                call "DATFMTC" (daternge$(2%))
                return

L20530: REM Def/Enable Part Type Range             TYPERNGE$()
            if str(typernge$()) = " " then typernge$(1%) = "ALL"
            return

L20570: REM Def/Enable Part Category Range         CTGYRNGE$()
            if str(ctgyrnge$()) = " " then ctgyrnge$(1%) = "ALL"
            return

L20610: REM Def/Enable Part Class Range            CLSSRNGE$()
            if str(clssrnge$()) = " " then clssrnge$(1%) = "ALL"
            return

L20650: REM Def/Enable Buyer Part Class Range      BUYRRNGE$()
            if str(buyrrnge$()) = " " then buyrrnge$(1%) = "ALL"
            return

L20690: REM Def/Enable Prod. Scheduler Class Rng   SCHDRNGE$()
            if str(schdrnge$()) = " " then schdrnge$(1%) = "ALL"
            return

L20730: REM Def/Enable Adds/Wdwal Delimits         ADD_WDWAL$
            if add_wdwal$ = " " or add_wdwal$ = blankdate$ then add_wdwal$ = "B"
            return

L20760: REM Def/Enable Detail Date Range           DDATRNGE$()
            if str(ddatrnge$()) <> " " then return
                ddatrnge$(1%) = daternge$(1%)
                ddatrnge$(2%) = daternge$(2%)
                return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then goto L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$                     /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Select PIP Tag Type Codes by placing or removing 'X's. PF(10) t~
        ~o see a list.",                                                  ~
         "Select Part Status Codes by placing or removing 'X's. PF(10) to~
        ~ see a list.",                                                   ~
         "Enter Part Number range, 'FIRST', 'LAST', or 'ALL'.          ",~
         "Enter Date range for PIP, 'FIRST', 'LAST', 'TODAY', or 'ALL'.",~
         "Enter Part Type range or 'ALL'.                              ",~
         "Enter Part Category range or 'ALL'.                          ",~
         "Enter Part Class range or 'ALL'.                             ",~
         "Enter Buyer Part Class range or 'ALL'.                       ",~
         "Enter Prod. Scheduler Class range or 'ALL'.                  ",~
         "Enter 'A' to see only Additions; 'W' for only Withdrawals; 'B' ~
        ~for Both",                                                       ~
         "Enter Date range for Detail, 'FIRST', 'LAST', 'TODAY', or 'ALL'"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, daternge$(), tag_type$(),  ~
                partrnge$(), statcode$(), typernge$(), ctgyrnge$(),      ~
                clssrnge$(), buyrrnge$(), schdrnge$(), ddatrnge$(),      ~
                add_wdwal$
            pipidat%, pipodat% = 0%
            call "ALLFREE"
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            str(line2$,,61%) = " "
            gosub set_pf1
            if fieldnr% > 0%                                             ~
                then init(hex(8c)) lfac$()                               ~
                else init(hex(86)) lfac$()
            on fieldnr% gosub L40250,           /* Type of Adds or Wdwls*/~
                              L40250,           /* Part Status          */~
                              L40250,           /* Part Number Range    */~
                              L40250,           /* Date Range           */~
                              L40250,           /* Part Type Range      */~
                              L40250,           /* Part Category Range  */~
                              L40250,           /* Part Class Range     */~
                              L40250,           /* Buyer Part Class Rng */~
                              L40250,           /* Prod. Scheduler Class*/~
                              L40250,           /* Add/Wdwal Delimits   */~
                              L40250            /* Detail Date Range    */
            goto L40280

            lfac$(fieldnr%) = hex(80)  :  return         /* Up / Low   */
L40250:     lfac$(fieldnr%) = hex(81)  :  return         /* Upper Only */
            lfac$(fieldnr%) = hex(82)  :  return         /* Numeric    */

L40280:     accept                                                       ~
                at (01,02), "PIP Stock Status Report Criteria Selection",~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (04,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (06,02), "Type of Adds or Wdwals?",                   ~
                at (06,28), fac(lfac$( 1%)), tag_type$(1%)      , ch(01),~
                at (06,30), "JO",                                        ~
                at (06,34), fac(lfac$( 1%)), tag_type$(2%)      , ch(01),~
                at (06,36), "RO",                                        ~
                at (06,40), fac(lfac$( 1%)), tag_type$(3%)      , ch(01),~
                at (06,42), "WO",                                        ~
                at (06,46), fac(lfac$( 1%)), tag_type$(4%)      , ch(01),~
                at (06,48), "PO",                                        ~
                at (06,52), fac(lfac$( 1%)), tag_type$(5%)      , ch(01),~
                at (06,54), "SO",                                        ~
                at (06,58), fac(lfac$( 1%)), tag_type$(6%)      , ch(01),~
                at (06,60), "QC",                                        ~
                at (06,64), fac(lfac$( 1%)), tag_type$(7%)      , ch(01),~
                at (06,66), "BO",                                        ~
                at (06,70), fac(lfac$( 1%)), tag_type$(8%)      , ch(01),~
                at (06,72), "BW",                                        ~
                at (06,76), fac(lfac$( 1%)), tag_type$(9%)      , ch(01),~
                at (06,78), "RW",                                        ~
                                                                         ~
                at (07,02), "Part Status Codes",                         ~
                at (07,30), fac(lfac$( 2%)), statcode$(1%)      , ch(01),~
                at (07,32), "Any",                                       ~
                at (07,37), fac(lfac$( 2%)), statcode$(2%)      , ch(01),~
                at (07,39), "(2)",                                       ~
                at (07,44), fac(lfac$( 2%)), statcode$(3%)      , ch(01),~
                at (07,46), "(3)",                                       ~
                at (07,51), fac(lfac$( 2%)), statcode$(4%)      , ch(01),~
                at (07,53), "(4)",                                       ~
                at (07,58), fac(lfac$( 2%)), statcode$(5%)      , ch(01),~
                at (07,60), "(5)",                                       ~
                at (07,65), fac(lfac$( 2%)), statcode$(6%)      , ch(01),~
                at (07,67), "(8)",                                       ~
                at (07,72), fac(lfac$( 2%)), statcode$(7%)      , ch(01),~
                at (07,74), "(9)",                                       ~
                                                                         ~
                at (09,28), fac(hex(ac)), col_header$           , ch(51),~
                at (10,02), "Part Number Range",                         ~
                at (10,28), fac(lfac$( 3%)), partrnge$(1%)      , ch(25),~
                at (10,54), fac(lfac$( 3%)), partrnge$(2%)      , ch(25),~
                                                                         ~
                at (11,02), "Date Range",                                ~
                at (11,28), fac(lfac$( 4%)), daternge$(1%)      , ch(10),~
                at (11,54), fac(lfac$( 4%)), daternge$(2%)      , ch(10),~
                                                                         ~
                at (12,02), "Part Type Range",                           ~
                at (12,28), fac(lfac$( 5%)), typernge$(1%)      , ch(03),~
                at (12,54), fac(lfac$( 5%)), typernge$(2%)      , ch(03),~
                                                                         ~
                at (13,02), "Part Category Range",                       ~
                at (13,28), fac(lfac$( 6%)), ctgyrnge$(1%)      , ch(04),~
                at (13,54), fac(lfac$( 6%)), ctgyrnge$(2%)      , ch(04),~
                                                                         ~
                at (14,02), "Part Class Range",                          ~
                at (14,28), fac(lfac$( 7%)), clssrnge$(1%)      , ch(04),~
                at (14,54), fac(lfac$( 7%)), clssrnge$(2%)      , ch(04),~
                                                                         ~
                at (15,02), "Buyer Part Class Range",                    ~
                at (15,28), fac(lfac$( 8%)), buyrrnge$(1%)      , ch(03),~
                at (15,54), fac(lfac$( 8%)), buyrrnge$(2%)      , ch(03),~
                                                                         ~
                at (16,02), "Prod. Sched. Class Range",                  ~
                at (16,28), fac(lfac$( 9%)), schdrnge$(1%)      , ch(03),~
                at (16,54), fac(lfac$( 9%)), schdrnge$(2%)      , ch(03),~
                                                                         ~
                at (18,02), "Detail: Adds or Wdwals? ",                  ~
                at (18,28), fac(lfac$(10%)), add_wdwal$         , ch(01),~
                                                                         ~
                at (19,02), "        Date Range",                        ~
                at (19,28), fac(lfac$(11%)), ddatrnge$(1%)      , ch(10),~
                at (19,54), fac(lfac$(11%)), ddatrnge$(2%)      , ch(10),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then goto L41060
                call "MANUAL" ("PIPSTKRP") : goto L40280

L41060:     if keyhit% <> 15% then goto L41090
                call "PRNTSCRN" : goto L40280

L41090:     close ws
            call "SCREEN" addr ("C", u3%, "I", i1$(), cursor%())
            return

        set_pf1
        if edit% = 2% then goto L41290            /*  Input Mode        */
            pf$(1%) = "(1)Start Over (4)Prev Field       (10)Se" &       ~
                      "lect From List         (13)Instructions"
            pf$(2%) = "                                  (11)Ac" &       ~
                      "cept Defaults          (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0a0bff0dff0f1000)
            if fieldnr% = 1% then goto L41240
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L41240:     if fieldnr% > 1% then goto L41260
                str(pf$(1%),15%,13%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L41260:     gosub test_pf_10
            return

L41290: if fieldnr% > 0% then goto L41390    /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "      (32)Print Report (16)Display Data"
            pfkeys$ = all(hex(ff))
            str(pfkeys$,,18%) = hex(01ffffffffffffffffffffff0dff0f100020)
            return
L41390:                                     /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                     (10)Se" &       ~
                      "lect From List         (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffff0affff0dff0fff00)
            gosub test_pf_10
            return

        test_pf_10
            if fieldnr% = 1% or fieldnr% = 2% then return  /* 10 stays */
            str(pf$(1%),35%,20%) = " " : str(pfkeys$,10%,1%) = hex(ff)
            return

L42000: REM *************************************************************~
            * User may select PIP Tag Types from a list.                *~
            *************************************************************

            gosub set_pfx
            str(line2$,,61%) = "List of PIP Types for Selection"
            inpmessage$ = "Select the desired PIP Tag Type(s), then press~
        ~ (RETURN)."

L42090:     accept                                                       ~
                at (01,02), "PIP Stock Status Report Criteria Selection",~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (04,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (06,02), "Type of Adds or Wdwals?",                   ~
                at (06,30), fac(lfac$( 1%)), tag_type$(1%)      , ch(01),~
                at (06,32), "(JO) = Released Jobs",                      ~
                at (07,30), fac(lfac$( 1%)), tag_type$(2%)      , ch(01),~
                at (07,32), "(RO) = Purchase Directives (Requisitions)", ~
                at (08,30), fac(lfac$( 1%)), tag_type$(3%)      , ch(01),~
                at (08,32), "(WO) = Work Order Advices",                 ~
                at (09,30), fac(lfac$( 1%)), tag_type$(4%)      , ch(01),~
                at (09,32), "(PO) = Purchase Orders",                    ~
                at (10,30), fac(lfac$( 1%)), tag_type$(5%)      , ch(01),~
                at (10,32), "(SO) = Sales Orders",                       ~
                at (11,30), fac(lfac$( 1%)), tag_type$(6%)      , ch(01),~
                at (11,32), "(QC) = Pending in QC",                      ~
                at (12,30), fac(lfac$( 1%)), tag_type$(7%)      , ch(01),~
                at (12,32), "(BO) = Buy Advices",                        ~
                at (13,30), fac(lfac$( 1%)), tag_type$(8%)      , ch(01),~
                at (13,32), "(BW) = Buy Work (PJ) Advices",              ~
                at (14,30), fac(lfac$( 1%)), tag_type$(9%)      , ch(01),~
                at (14,32), "(RW) = Purchase Job Directives (Requisit.)",~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then goto L42420
                call "MANUAL" ("PIPSTKRP") : goto L42090

L42420:     if keyhit% <> 15% then goto L42450
                call "PRNTSCRN" : goto L42090

L42450:     close ws
            call "SCREEN" addr ("C", u3%, "I", i2$(), cursor%())
            return

        set_pfx
            pf$(1%) = "(1)Start Over                     (10)Ma" &       ~
                      "in Screen              (13)Instructions"
            pf$(2%) = "                                  (11)Ac" &       ~
                      "cept Defaults          (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffff0a0bff0dff0fff00)
            return

L43000: REM *************************************************************~
            * User may select Part Status Types from a list.            *~
            *************************************************************

            gosub set_pfy
            str(line2$,,61%) = "List of Status Codes for Selection"
            inpmessage$ = "Select Status Type(s); Note that 'Any' include~
        ~s Parts with No Problem Condition"

L43090:     accept                                                       ~
                at (01,02), "PIP Stock Status Report Criteria Selection",~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (04,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (07,02), "Part Status Codes",                         ~
                at (07,30), fac(lfac$( 2%)), statcode$(1%)      , ch(01),~
                at (07,32), "Any= ALL Parts, with or w/o Problem Status",~
                at (08,30), fac(lfac$( 2%)), statcode$(2%)      , ch(01),~
                at (08,32), "(2)= Mf'd Part Surplus Condition",          ~
                at (09,30), fac(lfac$( 2%)), statcode$(3%)      , ch(01),~
                at (09,32), "(3)= Purch Part Surplus Condition",         ~
                at (10,30), fac(lfac$( 2%)), statcode$(4%)      , ch(01),~
                at (10,32), "(4)= Mf'd Part Safety Stock Intrusion",     ~
                at (11,30), fac(lfac$( 2%)), statcode$(5%)      , ch(01),~
                at (11,32), "(5)= Purch Part Safety Stock Intrusion",    ~
                at (12,30), fac(lfac$( 2%)), statcode$(6%)      , ch(01),~
                at (12,32), "(8)= Mf'd Part Shortage",                   ~
                at (13,30), fac(lfac$( 2%)), statcode$(7%)      , ch(01),~
                at (13,32), "(9)= Purch Part Shortage",                  ~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then goto L43420
                call "MANUAL" ("PIPSTKRP") : goto L43090

L43420:     if keyhit% <> 15% then goto L43450
                call "PRNTSCRN" : goto L43090

L43450:     close ws
            call "SCREEN" addr ("C", u3%, "I", i2$(), cursor%())
            return

        set_pfy
            pf$(1%) = "(1)Start Over (4)Prev Field       (10)Ma" &       ~
                      "in Screen              (13)Instructions"
            pf$(2%) = "                                  (11)Ac" &       ~
                      "cept Defaults          (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffff04ffffffffff0a0bff0dff0fff00)
            if edit% <> 2% then return
                str(pf$(1%),15%,13%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
                return

L44000: REM *************************************************************~
            * Summary Display Screen -- Parts only.                     *~
            *************************************************************

*        Enable all possible PF keys.
            init (hex(ff)) pfkeys$
            str(pfkeys$,,18%) = hex(0102030405060708ffffffff0dff0f102000)
            pf$(1%) = "(1)Start Over (4)Prev Page  (7)Up       " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "(2)First Page (5)Next Page  (8)Find Part" &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page  (6)Down                   " &       ~
                      "      (32)Print Report (16)Return      "
            if nbr_parts% > l%(1%) then goto L44160
                str(pf$(2%),29%,12%) = " " : str(pfkeys$,8%,1%) = hex(ff)

L44160
*        The value of Y%(1%) determines which PF Key(s) remain enabled.
            if y%(1%) > 1% then goto L44190
                str(pfkeys$,2%,1%) = hex(ff) : str(pf$(2%),,14%) = " "
L44190:     if y%(1%) <> z%(1%) then goto L44210
                str(pfkeys$,3%,1%) = hex(ff) : str(pf$(3%),,14%) = " "
L44210:     if y%(1%) <> 1% then goto L44230
                str(pfkeys$,4%,1%) = hex(ff) : str(pf$(1%),15%,14%) = " "
L44230:     if y%(1%) <> max(0%,min(z%(1%),y%(1%)+l%(1%)))               ~
                and x%(1%) > l%(1%) then goto L44260
                str(pfkeys$,5%,1%) = hex(ff) : str(pf$(2%),15%,14%) = " "
L44260:     if y%(1%) <> 1% then goto L44280
                str(pfkeys$,6%,1%) = hex(ff) : str(pf$(3%),15%,14%) = " "
L44280:     if y%(1%) < x%(1%) - l%(1%) + 1% and x%(1%) > l%(1%)         ~
                then goto L44310
                str(pfkeys$,7%,1%) = hex(ff) : str(pf$(1%),29%, 5%) = " "
L44310:     inpmessage$ = "To see details for a Part, move the cursor t"&~
                "o that part and press (RETURN)."
            goto L44530           /* Skip over the PF(8)Find Part logic */

L44350: REM *************************************************************~
            * PF(8)Find Part logic. Set up the screen for user input.   *~
            *************************************************************

*        Enable all possible PF keys.
            init (hex(ff)) pfkeys$
            str(pfkeys$,,18%) = hex(01ffffffffffffffffffffff0dff0fffff00)
            pf$(1%) = "(1)Cancel Search                        " &       ~
                      "                       (13)Instructions"
            pf$(2%) = " "
            str(pf$(2%),48%,12%) = searcherror$
            str(pf$(2%),64%,16%) = "(15)Print Screen"
            pf$(3%) = "Enter Part # (or partial), then press (RETURN):" &~
                hex(81) & temp$
            str(pf$(3%),75%,1%) = hex(8c)
            inpmessage$ = "Enter Part # (or partial) to search for belo"&~
                "w, then press (RETURN)."

L44530
*        Now, edit the appropriate data into DSPLY$() for display.
            init (" ") dsply$()
            for p% = 1% to l%(1%)
                li% = p% + y%(1%) - 1%
                if li% > nbr_parts% then goto L44830
                dsply$(p%,1%) = partnmbr$(li%)
                str(dsply$(p%,1%),27%,3%) = partstat$(li%)
                convert qtysfty(li%) to str(dsply$(p%,1%),41%,10%),      ~
                     pic (#######.##)
                if qtyhand(li%) < 0                                      ~
                     then convert qtyhand(li%) to                        ~
                          str(dsply$(p%,1%),51%,10%), pic (-######.##)   ~
                     else convert qtyhand(li%) to                        ~
                          str(dsply$(p%,1%),51%,10%), pic (#######.##)
                convert qty_moq(li%) to str(dsply$(p%,1%),61%,10%),      ~
                     pic (#######.##)
                dsply$(p%,2%) = partdesc$(li%)
                if qty_pip(li%) < 0                                      ~
                     then convert qty_pip(li%) to                        ~
                          str(dsply$(p%,2%),51%,10%), pic (-######.##)   ~
                     else convert qty_pip(li%) to                        ~
                          str(dsply$(p%,2%),51%,10%), pic (#######.##)
                if qtyprob(li%) < 0                                      ~
                     then convert qtyprob(li%) to                        ~
                          str(dsply$(p%,2%),61%,10%), pic (-######.##)   ~
                     else convert qtyprob(li%) to                        ~
                          str(dsply$(p%,2%),61%,10%), pic (#######.##)
                str(dsply$(p%,2%),72%,8%) = partdate$(li%)
            next p%

L44830
*        Miscellaneous set-up for the ACCEPT statement.
            convert nbr_parts% to temp$, pic (#,###)
            call "STRING" addr ("LJ", temp$, len(temp$))
            str(line2$,,61%) = temp$ & " Part(s) satisfy your criteria"
            str(prtcol1$,33%,8%) = " "
            str(prtcol2$,32%,9%) = " "

*        Now all appropriate PF keys are enabled, etc. Show the display.
L44910:     accept                                                       ~
                at (01,02), "PIP Stock Status Summary Display",          ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(8c)), prtcol1$              , ch(79),~
                at (04,02), fac(hex(ac)), prtcol2$              , ch(79),~
                                                                         ~
                at (05,02), fac(hex(8c)),   dsply$(1%,1%)       , ch(79),~
                at (06,02), fac(hex(8c)),   dsply$(1%,2%)       , ch(79),~
                at (07,02), fac(hex(8c)),   dsply$(2%,1%)       , ch(79),~
                at (08,02), fac(hex(8c)),   dsply$(2%,2%)       , ch(79),~
                at (09,02), fac(hex(8c)),   dsply$(3%,1%)       , ch(79),~
                at (10,02), fac(hex(8c)),   dsply$(3%,2%)       , ch(79),~
                at (11,02), fac(hex(8c)),   dsply$(4%,1%)       , ch(79),~
                at (12,02), fac(hex(8c)),   dsply$(4%,2%)       , ch(79),~
                at (13,02), fac(hex(8c)),   dsply$(5%,1%)       , ch(79),~
                at (14,02), fac(hex(8c)),   dsply$(5%,2%)       , ch(79),~
                at (15,02), fac(hex(8c)),   dsply$(6%,1%)       , ch(79),~
                at (16,02), fac(hex(8c)),   dsply$(6%,2%)       , ch(79),~
                at (17,02), fac(hex(8c)),   dsply$(7%,1%)       , ch(79),~
                at (18,02), fac(hex(8c)),   dsply$(7%,2%)       , ch(79),~
                at (19,02), fac(hex(8c)),   dsply$(8%,1%)       , ch(79),~
                at (20,02), fac(hex(8c)),   dsply$(8%,2%)       , ch(79),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then goto L45260
                call "MANUAL" ("PIPSTKRP") : goto L44910

L45260:     if keyhit% <> 15% then goto L45290
                call "PRNTSCRN" : goto L44910

L45290:     close ws
            call "SCREEN" addr ("C", u3%, "I", i2$(), cursor%())
            return

L46000: REM *************************************************************~
            * Detail Display Screen -- Parts and their PIP Line Items.  *~
            *************************************************************

*        Enable all possible PF keys.
            init (hex(ff)) pfkeys$
            str(pfkeys$,,22%) = hex(01020304050607ffffffffff0dff0f1000121~
        3141520)
            pf$(1%) = "(1)Start Over (4)Prev Page  (7)Up       " &       ~
                      "      (20)Prev Part    (13)Instructions"
            pf$(2%) = "(2)First Page (5)Next Page (18)First Par" &       ~
                      "t     (21)Next Part    (15)Print Screen"
            pf$(3%) = "(3)Last Page  (6)Down      (19)Last Part" &       ~
                      "      (32)Print Report (16)Summary Scrn"

*        The value of Y%(1%) determines which Part Key(s) remain enabled.
            if y%(1%) > 1% then goto L46180
                str(pfkeys$,18%,1%) = hex(ff) : str(pf$(2%),28%,14%) = " "
L46180:     if y%(1%) <> nbr_parts% then goto L46200
                str(pfkeys$,19%,1%) = hex(ff) : str(pf$(3%),28%,14%) = " "
L46200:     if y%(1%) <> 1% then goto L46220
                str(pfkeys$,20%,1%) = hex(ff) : str(pf$(1%),47%,13%) = " "
L46220:     if y%(1%) <> nbr_parts% then goto L46250
                str(pfkeys$,21%,1%) = hex(ff) : str(pf$(2%),47%,13%) = " "

L46250
*        The value of Y%(2%) determines which Line Key(s) remain enabled.
            if y%(2%) > 1% then goto L46280
                str(pfkeys$,2%,1%) = hex(ff) : str(pf$(2%),,14%) = " "
L46280:     if y%(2%) <> z%(2%) then goto L46300
                str(pfkeys$,3%,1%) = hex(ff) : str(pf$(3%),,14%) = " "
L46300:     if y%(2%) <> 1% then goto L46320
                str(pfkeys$,4%,1%) = hex(ff) : str(pf$(1%),15%,14%) = " "
L46320:     if y%(2%) <> max(0%, min(z%(2%), y%(2%) + l%(2%)))           ~
                and x%(2%) > l%(2%) then goto L46350
                str(pfkeys$,5%,1%) = hex(ff) : str(pf$(2%),15%,13%) = " "
L46350:     if y%(2%) <> 1% then goto L46370
                str(pfkeys$,6%,1%) = hex(ff) : str(pf$(3%),15%,13%) = " "
L46370:     if y%(2%) < x%(2%) - l%(2%) + 1% and x%(2%) > l%(2%)         ~
                then goto L46410
                str(pfkeys$,7%,1%) = hex(ff) : str(pf$(1%),29%, 5%) = " "

L46410
*        Now, edit the appropriate data into DSPLY$() for display.
            init (" ") dsply$()
            for p% = 1% to l%(2%)
                li% = p% + y%(2%) - 1%
                if li% > nbr_lines% then goto L46610
                dsply$(p%,1%) = piptag$(li%)
                if pipsup(li%) - pipdem(li%) < 0                         ~
                     then convert pipsup(li%) - pipdem(li%) to           ~
                          str(dsply$(p%,1%),20%,12%), pic (-########.##) ~
                     else convert pipsup(li%) - pipdem(li%) to           ~
                          str(dsply$(p%,1%),20%,12%), pic (#########.##)
                str(dsply$(p%,1%),33%,25%) = pippar$(li%)
                str(dsply$(p%,1%),59%, 8%) = pipdat$(li%)
                if pipbal(li%) < 0                                       ~
                     then convert pipbal(li%) to                         ~
                          str(dsply$(p%,1%),68%,12%), pic (-########.##) ~
                     else convert pipbal(li%) to                         ~
                          str(dsply$(p%,1%),68%,12%), pic (#########.##)
            next p%

L46610
*        Miscellaneous set-up for the ACCEPT statement.
            inpmessage$ = "To see Component Status, move cursor to desi"&~
                "red Tag Number and press (RETURN)."
            convert nbr_lines% to temp$, pic (#,###)
            call "STRING" addr ("LJ", temp$, len(temp$))
            str(line2$,,61%) = temp$ & " PIP(s) satisfy your Add/Wdwal "&~
                "criteria for this Part"
            prtdsp1$ = partnmbr$(y%(1%))
            str(prtdsp1$,27%,3%) = partstat$(y%(1%))
            convert on_order to str(prtdsp1$,31%,10%), pic (#######.##)
            convert qtysfty(y%(1%)) to str(prtdsp1$,41%,10%),            ~
                pic (#######.##)
            if qtyhand(y%(1%)) < 0                                       ~
                then convert qtyhand(y%(1%)) to str(prtdsp1$,51%,10%),   ~
                     pic (-######.##)                                    ~
                else convert qtyhand(y%(1%)) to str(prtdsp1$,51%,10%),   ~
                     pic (#######.##)
            convert qty_moq(y%(1%)) to str(prtdsp1$,61%,10%),            ~
                pic (#######.##)
            prtdsp2$ = partdesc$(y%(1%))
            convert committed to str(prtdsp2$,31%,10%), pic (#######.##)
            if qty_pip(y%(1%)) < 0                                       ~
                then convert qty_pip(y%(1%)) to str(prtdsp2$,51%,10%),   ~
                     pic (-######.##)                                    ~
                else convert qty_pip(y%(1%)) to str(prtdsp2$,51%,10%),   ~
                     pic (#######.##)
            if qtyprob(y%(1%)) < 0                                       ~
                then convert qtyprob(y%(1%)) to str(prtdsp2$,61%,10%),   ~
                     pic (-######.##)                                    ~
                else convert qtyprob(y%(1%)) to str(prtdsp2$,61%,10%),   ~
                     pic (#######.##)
            str(prtdsp2$,72%,8%) = partdate$(y%(1))
            str(prtcol1$,33%,8%) = "On Order"
            str(prtcol2$,32%,9%) = "Committed"

*        Now all appropriate PF keys are enabled -- show the display.
L46970:     accept                                                       ~
                at (01,02), "PIP Stock Status Detail Display",           ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(8c)), prtcol1$              , ch(79),~
                at (04,02), fac(hex(ac)), prtcol2$              , ch(79),~
                at (05,02), fac(hex(8c)), prtdsp1$              , ch(79),~
                at (06,02), fac(hex(ac)), prtdsp2$              , ch(79),~
                at (07,02), fac(hex(ac)), dtlcol1$              , ch(79),~
                                                                         ~
                at (08,02), fac(hex(8c)),   dsply$( 1%,1%)      , ch(79),~
                at (09,02), fac(hex(8c)),   dsply$( 2%,1%)      , ch(79),~
                at (10,02), fac(hex(8c)),   dsply$( 3%,1%)      , ch(79),~
                at (11,02), fac(hex(8c)),   dsply$( 4%,1%)      , ch(79),~
                at (12,02), fac(hex(8c)),   dsply$( 5%,1%)      , ch(79),~
                at (13,02), fac(hex(8c)),   dsply$( 6%,1%)      , ch(79),~
                at (14,02), fac(hex(8c)),   dsply$( 7%,1%)      , ch(79),~
                at (15,02), fac(hex(8c)),   dsply$( 8%,1%)      , ch(79),~
                at (16,02), fac(hex(8c)),   dsply$( 9%,1%)      , ch(79),~
                at (17,02), fac(hex(8c)),   dsply$(10%,1%)      , ch(79),~
                at (18,02), fac(hex(8c)),   dsply$(11%,1%)      , ch(79),~
                at (19,02), fac(hex(8c)),   dsply$(12%,1%)      , ch(79),~
                at (20,02), fac(hex(8c)),   dsply$(13%,1%)      , ch(79),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then goto L47320
                call "MANUAL" ("PIPSTKRP") : goto L46970

L47320:     if keyhit% <> 15% then goto L47350
                call "PRNTSCRN" : goto L46970

L47350:     close ws
            call "SCREEN" addr ("C", u3%, "I", i2$(), cursor%())
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%, dflt%)
            errormsg$ = " "
*        Are we 'Accepting Defaults' or capturing one at a time?
            if dflt% = 0% then goto L50310          /* Accept Defaults? */
                on fieldnr% goto L50120, L50140, L50160, L50180, L50200,      ~
                     L50220, L50240, L50260, L50280, L50284, L50288   /* Yes */
L50120:         gosub L50430                  /* Type of Adds or Wdwals?*/
                if errormsg$ <> " " then return
L50140:         gosub L50480                  /* Part Status            */
                if errormsg$ <> " " then return
L50160:         gosub L50530                  /* Part Number Range      */
                if errormsg$ <> " " then return
L50180:         gosub L50580                  /* Date Range             */
                if errormsg$ <> " " then return
L50200:         gosub L51040                  /* Part Type Range        */
                if errormsg$ <> " " then return
L50220:         gosub L51090                  /* Part Category Range    */
                if errormsg$ <> " " then return
L50240:         gosub L51140                  /* Part Class Range       */
                if errormsg$ <> " " then return
L50260:         gosub L51190                  /* Buyer Part Class Range */
                if errormsg$ <> " " then return
L50280:         gosub L51240                  /* Prod. Scheduler Class  */
                if errormsg$ <> " " then return
L50284:         gosub L51290                  /* Add/Wdwal Delimits     */
                if errormsg$ <> " " then return
L50288:         gosub L51340                  /* Detail Date Range      */
                return

L50310
*        No- capturing fields one at a time.
            on fieldnr% gosub L50430,         /* Type of Adds or Wdwls? */~
                              L50480,         /* Part Status Codes      */~
                              L50530,         /* Part Number Range      */~
                              L50580,         /* Date Range             */~
                              L51040,         /* Part Type Range        */~
                              L51090,         /* Part Category Range    */~
                              L51140,         /* Part Class Range       */~
                              L51190,         /* Buyer Part Class Range */~
                              L51240,         /* Prod. Scheduler Class  */~
                              L51290,         /* Add/Wdwal Delimits     */~
                              L51340          /* Detail Date Range      */
            return

L50430: REM Test for Type of Adds or Wdwals?      TAG_TYPE$()
            if str(tag_type$()) <> " " then return
                errormsg$ = "You must select at least one Tag Type."
                return

L50480: REM Test for Part Status                  STATCODE$()
            if str(statcode$()) <> " " then return
                errormsg$ = "You must select at least one Status Code."
                return

L50530: REM Test for Part Number Range            PARTRNGE$()
            call "TESTRNGE" (partrnge$(1%), partrnge$(2%), partrnge$(3%),~
                partrnge$(4%), errormsg$)
            return

L50580: REM Test for Date Range                   DATERNGE$()
*        First, validate the 'From' Date.
            if daternge$(1%) <> "ALL" then goto L50650
                daternge$(2%) = " "
                daternge$(3%) = planday1$
                daternge$(4%) = planday490$
                goto L50880
L50650:     if daternge$(1%) <> "FIRST" then goto L50680
                daternge$(3%) = planday1$
                goto L50760
L50680:     if daternge$(1%) <> "TODAY" then goto L50710
                daternge$(3%) = date
                goto L50760
L50710:     call "DATEOKC" (daternge$(1%), u3%, errormsg$)
            if errormsg$ <> " " then return
            daternge$(3%) = daternge$(1%)
            call "DATUFMTC" (daternge$(3%))

L50760
*        Then, of course, the 'To' Date.
            if daternge$(2%) <> "LAST" then goto L50800
                daternge$(4%) = planday490$
                goto L50880
L50800:     if daternge$(2%) <> "TODAY" then goto L50830
                daternge$(4%) = date
                goto L50880
L50830:     call "DATEOKC" (daternge$(2%), u3%, errormsg$)
            if errormsg$ <> " " then return
            daternge$(4%) = daternge$(2%)
            call "DATUFMTC" (daternge$(4%))

L50880
*        Test to make sure there is an actual FROM - TO range.
            if daternge$(3%) <= daternge$(4%) then goto L50930
                errormsg$ = "The 'FROM' date must be earlier than or eq"&~
                     "ual to the 'TO' Date."
                return
L50930:     call "PIPINDEX" (#07, str(daternge$(3%),,6%), date1%, err%)
            if err% = 0% then goto L50980
                errormsg$ = "FROM Date has invalid data or is outside y"&~
                     "our Planning Calendar."
                return
L50980:     call "PIPINDEX" (#07, str(daternge$(4%),,6%), date2%, err%)
            if err% = 0% then return
                errormsg$ = "TO Date has invalid data or is outside you"&~
                     "r Planning Calendar."
                return

L51040: REM Test for Part Type Range              TYPERNGE$()
            call "TESTRNGE" (typernge$(1%), typernge$(2%), typernge$(3%),~
                typernge$(4%), errormsg$)
            return

L51090: REM Test for Part Category Range          CTGYRNGE$()
            call "TESTRNGE" (ctgyrnge$(1%), ctgyrnge$(2%), ctgyrnge$(3%),~
                ctgyrnge$(4%), errormsg$)
            return

L51140: REM Test for Part Class Range             CLSSRNGE$()
            call "TESTRNGE" (clssrnge$(1%), clssrnge$(2%), clssrnge$(3%),~
                clssrnge$(4%), errormsg$)
            return

L51190: REM Test for Buyer Part Class Range       BUYRRNGE$()
            call "TESTRNGE" (buyrrnge$(1%), buyrrnge$(2%), buyrrnge$(3%),~
                buyrrnge$(4%), errormsg$)
            return

L51240: REM Test for Prod. Scheduler Class Range  SCHDRNGE$()
            call "TESTRNGE" (schdrnge$(1%), schdrnge$(2%), schdrnge$(3%),~
                schdrnge$(4%), errormsg$)
            return

L51290: REM Test for Add/Wdwal Delimits           ADD_WDWAL$
            if pos("ABW" = add_wdwal$) <> 0% then return
                errormsg$ = "Add/Withdrawl Delimiter Must be 'A', 'W', "&~
                            "or 'B'."
                return

L51340: REM Test for Detail Date Range            DDATRNGE$()
*        First, validate the 'From' Date.
            if ddatrnge$(1%) <> "ALL" then goto L51410
                ddatrnge$(2%) = " "
                ddatrnge$(3%) = planday1$
                ddatrnge$(4%) = planday490$
                goto L51640
L51410:     if ddatrnge$(1%) <> "FIRST" then goto L51440
                ddatrnge$(3%) = planday1$
                goto L51520
L51440:     if ddatrnge$(1%) <> "TODAY" then goto L51470
                ddatrnge$(3%) = date
                goto L51520
L51470:     call "DATEOKC" (ddatrnge$(1%), u3%, errormsg$)
            if errormsg$ <> " " then return
            ddatrnge$(3%) = ddatrnge$(1%)
            call "DATUFMTC" (ddatrnge$(3%))

L51520
*        Then, of course, the 'To' Date.
            if ddatrnge$(2%) <> "LAST" then goto L51560
                ddatrnge$(4%) = planday490$
                goto L51640
L51560:     if ddatrnge$(2%) <> "TODAY" then goto L51590
                ddatrnge$(4%) = date
                goto L51640
L51590:     call "DATEOKC" (ddatrnge$(2%), u3%, errormsg$)
            if errormsg$ <> " " then return
            ddatrnge$(4%) = ddatrnge$(2%)
            call "DATUFMTC" (ddatrnge$(4%))

L51640
*        Test to make sure there is an actual FROM - TO range.
            if ddatrnge$(3%) <= ddatrnge$(4%) then goto L51690
                errormsg$ = "The 'FROM' date must be earlier than or eq"&~
                     "ual to the 'TO' Date."
                return
L51690:     call "PIPINDEX" (#07, str(ddatrnge$(3%),,6%), ddat1%, err%)
            if err% = 0% then goto L51740
                errormsg$ = "FROM Date has invalid data or is outside y"&~
                     "our Planning Calendar."
                return
L51740:     call "PIPINDEX" (#07, str(ddatrnge$(4%),,6%), ddat2%, err%)
            if err% = 0% then return
                errormsg$ = "TO Date has invalid data or is outside you"&~
                     "r Planning Calendar."
                return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                      PIPSTKRP####~
        ~###
L60070: %                          P. I. P.   S T O C K   S T A T U S   A~
        ~ N A L Y S I S   D E T A I L   R E P O R T                PAGE: #~
        ~###
L60100: %                         P. I. P.   S T O C K   S T A T U S   A ~
        ~N A L Y S I S   S U M M A R Y   R E P O R T               PAGE: #~
        ~###
L60130: %                                                               ~
        ~         ON      SAFETY                             PROBLEM  PROB~
        ~LEM
L60160: %PART NUMBER               DESCRIPTION                      STA ~
        ~       HAND       STOCK         PIP         MOQ    QUANTITY   DAT~
        ~E
L60190: %------------------------- -------------------------------- --- ~
        ~----------- ----------- ----------- ----------- ----------- -----~
        ~---
L60220: %######################### ################################ ### ~
        ~############################################################ ####~
        ~####
L60250: %                                          --- TAG NUMBER ----  ~
        ~   QUANTITY PARENT                    DUE DATE       BALANCE
L60270: %                                          ------------------- -~
        ~----------- ------------------------- -------- -------------
L60290: %                                          ################### #~
        ~########### ######################### ######## -#####,###.##
L60310: %                         ######################################~
        ~##########################################
L60330: % *** RUN TOTALS: ###,###,### OK; ###,###,### SURPLUS; ###,###,##~
        ~# CRITICAL SHORTAGE; ###,###,### SAFETY STOCK INTRUSION
L60350: %                                                  *** END OF REP~
        ~ORT  ######## ***

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        error_exit
            call "ASKUSER" (0%, "DATE ERROR", "There is a Problem in yo"&~
                            "ur PLANNING CALENDAR or SYSFILE2 File.",    ~
                            "Please correct before re-running this prog"&~
                            "ram.", "Press any PF Key to Continue")

        exit_program
            call "SHOSTAT" ("One moment please")
            end

