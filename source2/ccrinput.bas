        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    CCC   RRRR   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  C   C  C   C  R   R    I    NN  N  P   P  U   U    T     *~
            *  C      C      RRRR     I    N N N  PPPP   U   U    T     *~
            *  C   C  C   C  R   R    I    N  NN  P      U   U    T     *~
            *   CCC    CCC   R   R  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CCRINPUT - Permits Input/Edit/Maintenance functions on the*~
            *            Customer Credit Master and Line Item files.    *~
            *            The master file is a 'shadow' to the CUSTOMER  *~
            *            Master file and the line items (CCRLINES)      *~
            *            represent NSF Check events.                    *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/13/92 ! Original                                 ! JIM *~
            * 08/01/96 ! Changes for the year 2000.               ! DXL *~
            * 07/21/97 ! Corrected ModBy & Date if NSF deleted.   ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            avg_days$5,                  /* Average Days to Pay        */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            chnguser$3,                  /* Last Changed by ...        */~
            cursor%(2),                  /* Cursor location for edit   */~
            customer$9, custname$32,     /* Customer Code & Name       */~
            date$8,                      /* Date for screen display    */~
            dateopen$8,                  /* Date Account Opened        */~
            dsply$(15)79,                /* Screen display lines       */~
            dynamnts(4), dynamnts$(4)15, /* Dynamic Balance            */~
            dyndates$(5)8,               /* Dynamic Dates              */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hicrdate$8,                  /* High Credit Limit Date     */~
            hicrlimt$12,                 /* High Credit Limit          */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(21)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            line4$79,                    /* Screen Line #4             */~
            line5$79,                    /* Screen Line #5             */~
            linvamnt$14,                 /* Last Invoice Amount        */~
            linvnmbr$8,                  /* Last Invoice Number        */~
            lpayamnt$14,                 /* Last Payment Amount        */~
            lpaychek$10,                 /* Last Payment Check #       */~
            moddate$8, moduser$3,        /* Last Modified on ... by ...*/~
            msg$79,                      /* Misc message               */~
            nbrpmnts$5,                  /* # Payments in Average Days */~
            netoutst$14,                 /* Outstanding (net) NSF $$$  */~
            netsales$14,                 /* Net Sales to Date          */~
            nsf$3,                       /* Edited # of NSFs           */~
            nsf_amnt(100), nsf_amnt$12,  /* NSF Check Amount           */~
            nsf_atot(1),                 /* NSF Check Amount total     */~
            nsf_chek$(100)10,            /* NSF Check Number           */~
            nsf_date$(100)8,             /* NSF Check Date             */~
            nsf_dtpd$(100)8,             /* Date Paid                  */~
            nsf_invc$(100)8,             /* Invoice Number             */~
            nsf_modd$(100)8,             /* Last Modified on ...       */~
            nsf_modu$(100)3,             /* Last Modified by ...       */~
            nsf_net$15,                  /* Net of NSFs outstanding    */~
            nsf_ones(1, 100),            /* NSF $ total work area      */~
            nsf_paid(100), nsf_paid$12,  /* NSF Check Amount paid      */~
            nsf_ptot(1),                 /* NSF Check Amount paid total*/~
            nsf_sqnc$(100)3,             /* CCRLINES Key Sequence #    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pf16$16,                     /* PF(16) Descriptive         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            totcredt$12,                 /* Total Credits to Date      */~
            totsales$12,                 /* Total Sales to Date        */~
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
            * #01 ! CCRMASTR ! Customer Credit Master file              *~
            * #02 ! CCRLINES ! Customer Credit Line Items (NSFs)        *~
            * #10 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            select #02, "CCRLINES",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    1, keylen =   12

            select #10, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup,    ~
                            key  6, keypos = 1189, keylen =   3, dup

            call "SHOSTAT" ("Opening files. One moment please.")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 100%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 100%, rslt$(02%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),   0%, rslt$(10%))

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
            str(line2$,62%) = "CCRINPUT: " & str(cms2v$,,8%)
            l% = dim(dsply$(), 1%)   /* # NSF display lines on summary */
            mat nsf_ones = con           /* All 1's for MAT Multiplies */
            for x% = 1% to dim(nsf_amnt(), 1%) /* Generate Sequence #s */
                convert x% to nsf_sqnc$(x%), pic (00#)
            next x%
            line5$ = "Seq NSF Date   Check Amt Check #    Invoice#    A"&~
                "mt Paid  Date Pd  Outst. Amt"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables_1

            for fieldnr% = 1% to 5%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then goto L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then goto L10200
L10150:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                          if enabled% = 1% then goto L10120
                          if fieldnr% = 1% then goto L10100
                          goto L10150
L10200:              if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then goto L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then goto L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then goto nsf_summary_screen
                if keyhit% <> 11% then goto L11150
                     pf16$ = "(16)Next Append "
                     gosub append_nsf_event
                     goto editpg1
L11150:         if keyhit%  = 12% then gosub delete_all_nsfs
                if keyhit%  = 16% then goto datasave
                if keyhit% <>  0% then goto editpg1
L11180:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% > 5% then goto editpg1
            if fieldnr% = lastfieldnr% then goto editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then goto editpg1
L11230:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then goto L11230
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then goto L11230
                lastfieldnr% = fieldnr%
            goto L11180

        nsf_summary_screen
            p% = 1%                        /* Begin at first NSF event */
        display_the_nsfs
            gosub L44000
            if keyhit% =  1% then gosub startover
            if keyhit% =  2% then p% = 1%                /* First page */
            if keyhit% =  3% then p% = z%                 /* Last page */
            if keyhit% =  4% then p% = max(1%, p%-l%) /* Previous page */
            if keyhit% =  5% then p% = max(1%,min(z%,p%+l%)) /* Nxt pg */
            if keyhit% =  6% then p% = max(1%,p%-1%)           /* Down */
            if keyhit% =  7% then p% = max(1%,min(z%,p%+1%))     /* Up */
            if keyhit% <>11% then goto L11460
                pf16$ = "(16)Next Append "
                gosub append_nsf_event                       /* Append */
                goto display_the_nsfs
L11460:     if keyhit% = 12% then gosub delete_all_nsfs
            if keyhit% = 16% then goto editpg1               /* Return */
            if keyhit% <> 0% then goto display_the_nsfs       /* Edit? */
*        User wants to edit a line item.
            c% = cursor%(1%) - 5%
            if c% < 1% or c% > 15% then goto display_the_nsfs
            if c% + p% - 1% > nsf% then goto display_the_nsfs
            c% = c% + p% - 1%      /* This is the array element number */
            pf16$ = "(16)Return"
            gosub edit_nsf_event
            goto display_the_nsfs

        append_nsf_event
            c% = nsf% + 1%             /* Next available array element */
            if c% > dim(nsf_amnt(), 1%) then goto append_exit

*        INPUT_NSF_EVENT
            for fieldnr% = 1% to  6%
L11640:         gosub'052(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then goto L11770
L11660:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then goto L11740
L11690:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'052(fieldnr%)
                          if enabled% = 1% then goto L11660
                          if fieldnr% = 1% then goto L11640
                          goto L11690
L11740:              if keyhit% = 16% and fieldnr% = 1%                  ~
                          then goto append_exit
                     if keyhit% <> 0% then goto L11660
L11770:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then goto L11660
            next fieldnr%
            gosub edit_nsf_event
            nsf% = nsf% + 1%                      /* Hello, Line Item */
            gosub total_nsf_editor
            goto append_nsf_event

        append_exit /* This is the only exit point from APPEND, please */
            gosub compute_z
            return

        edit_nsf_event
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  4% then goto L11960
                     c% = max(1%, c% - 1%)
                     goto edit_nsf_event
L11960:         if keyhit% <>  5% then goto L11990
                     c% = min(nsf%, c% + 1%)
                     goto edit_nsf_event
L11990:         if keyhit%  = 12% then goto delete_nsf_record
                if keyhit% <> 16% then goto L12030
                     gosub total_nsf_editor
                     return
L12030:         if keyhit% <>  0% then goto edit_nsf_event
L12040:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 6% then edit_nsf_event
            if fieldnr% = lastfieldnr% then goto edit_nsf_event
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then goto edit_nsf_event
L12090:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then goto L12090
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then goto L12090
                lastfieldnr% = fieldnr%
            goto L12040

        delete_all_nsfs
            u3% = 2%
            call "ASKUSER" (u3%, "*** CONFIRM DELETION ***",             ~
                "Please confirm your request to delete ALL NSF items fo"&~
                "r this Customer.", "Press (RETURN) to DELETE.", "Press"&~
                " PF(1) to abort deletion.")
            if u3% =  1% then return
            if u3% <> 0% then goto delete_all_nsfs
*        Kiss ALL the line items goodbye. (SMACK!)
            gosub initialize_variables_2
            gosub delete_line_items
            gosub total_nsf_editor
            gosub compute_z
            return clear all
            goto editpg1

        delete_nsf_record
            u3% = 2%
            call "ASKUSER" (u3%, "*** CONFIRM DELETION ***",             ~
                "Please confirm your request to delete this NSF item.",  ~
                "Press (RETURN) to DELETE.", "Press PF(1) to abort dele"&~
                "tion.")
            if u3% =  1% then goto edit_nsf_event
            if u3% <> 0% then goto delete_nsf_record
*        Kiss the current line item goodbye via clobbering its array
*        elements with all subsequent NSF array elements.
            for d% = c% to nsf%
                if d% <> dim(nsf_amnt(), 1%) then goto L12490
                     nsf_amnt (d%), nsf_paid (d%) = 0
                     init (" ") nsf_chek$(d%), nsf_date$(d%),            ~
                          nsf_dtpd$(d%), nsf_invc$(d%)
                     goto L12550
L12490:         nsf_amnt (d%) = nsf_amnt (d%+1%)
                nsf_chek$(d%) = nsf_chek$(d%+1%)
                nsf_date$(d%) = nsf_date$(d%+1%)
                nsf_dtpd$(d%) = nsf_dtpd$(d%+1%)
                nsf_invc$(d%) = nsf_invc$(d%+1%)
                nsf_paid (d%) = nsf_paid (d%+1%)
                nsf_modu$(d%) = nsf_modu$(d%+1%)
                nsf_modd$(d%) = nsf_modd$(d%+1%)
L12550:     next d%
            nsf% = nsf% - 1%                               /* (SMACK!) */
            gosub total_nsf_editor
            gosub compute_z
            return

        REM *************************************************************~
            *             C O M M O N   S U B R O U T I N E S           *~
            *************************************************************

        total_nsf_editor
            convert nsf% to nsf$, pic(###)
            call "STRING" addr ("LJ", nsf$, len(str(nsf$)))
            mat nsf_atot = nsf_ones * nsf_amnt
            mat nsf_ptot = nsf_ones * nsf_paid
            if nsf_atot(1%) - nsf_ptot(1%) < 0                           ~
                then convert nsf_atot(1%) - nsf_ptot(1%) to nsf_net$,    ~
                    pic (-$##,###,###.##)                                ~
                else convert nsf_atot(1%) - nsf_ptot(1%) to nsf_net$,    ~
                    pic ($###,###,###.##)
            call "STRING" addr ("LJ", nsf_net$, len(str(nsf_net$)))
            str(line2$,,61%) = nsf$ & " NSF Check items; " & nsf_net$ &  ~
                " outstanding"
            return

        compute_net_sales
            netsales = totsales - totcredt
            if netsales < 0                                              ~
                then convert netsales to netsales$, pic (-########.##)   ~
                else convert netsales to netsales$, pic (#########.##)
            return

        compute_nsf_outstanding
            if nsf_amnt(c%) - nsf_paid(c%) < 0                           ~
                then convert nsf_amnt(c%) - nsf_paid(c%) to netoutst$,   ~
                    pic (-##,###,###.##)                                 ~
                else convert nsf_amnt(c%) - nsf_paid(c%) to netoutst$,   ~
                    pic (###,###,###.##)
            return

        delete_line_items
            plowkey$ = xor plowkey$
            str(plowkey$,,9%) = customer$
            call "DELETE" (#02, plowkey$, 9%)   /* Bye-bye, line items */
            return

        compute_z
            z% = max(1%, nsf% - l% + 1%)  /* Highest NSF top of screen */
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "READ101" (#01, customer$, f1%(1%))       /* CCRMASTR */
            avg_days, totsales, totcredt, hicrlimt = 0
            nbrpmnts% = 0%

*        Update the editable data fields for output.
            convert totsales$ to totsales, data goto L19130
L19130:     convert totcredt$ to totcredt, data goto L19140
L19140:     convert hicrlimt$ to hicrlimt, data goto L19150
L19150:     convert avg_days$ to avg_days, data goto L19160
L19160:     convert nbrpmnts$ to nbrpmnts%, data goto L19170
L19170:     call "DATUNFMT" (hicrdate$)
            call "DATUNFMT" (moddate$)

*        The non-editable (dynamic) balances. Round 'em as per CUSINPUT.
            for x% = 1% to 4%
                dynamnts(x%) = round(dynamnts(x%), 2)
            next x%

*        The non-editable (dynamic) dates.
            for x% = 1% to 5%
                call "DATUNFMT" (dyndates$(x%))
            next x%

*        Et, voila!
            put #01 using L35040, customer$, totsales, totcredt, hicrlimt,~
                hicrdate$, linvamnt, linvnmbr$, lpayamnt, lpaychek$,     ~
                avg_days, nbrpmnts%, dyndates$(), dynamnts(), moduser$,  ~
                moddate$, " "                              /* CCRMASTR */
            if f1%(1) = 0% then write #01 else rewrite #01

*        If there are line items (NSF events), put 'em out to disk.
            gosub delete_line_items
            if nsf% = 0% then goto datasave_exit

            for x% = 1% to nsf%
                call "DATUNFMT" (nsf_date$(x%))
                call "DATUNFMT" (nsf_dtpd$(x%))
                call "DATUNFMT" (nsf_modd$(x%))
                write #02 using L35220, customer$, nsf_sqnc$(x%),         ~
                     nsf_date$(x%), nsf_chek$(x%), nsf_invc$(x%),        ~
                     nsf_amnt(x%), nsf_paid(x%), nsf_dtpd$(x%),          ~
                     nsf_modu$(x%), nsf_modd$(x%), " "     /* CCRLINES */
            next x%

        datasave_exit                     /* Always exit DATASAVE here */
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20150,         /* Customer Code          */~
                              L20180,         /* Total Sales to Date    */~
                              L20210,         /* Total Credits to Date  */~
                              L20240,         /* High Credit Limit & Dt */~
                              L20290          /* Average Days to Pay    */
            return

L20150: REM Def/Enable Customer Code               CUSTOMER$
            return

L20180: REM Def/Enable Total Sales to Date         TOTSALES$
            return

L20210: REM Def/Enable Total Credits to Date       TOTCREDT$
            return

L20240: REM Def/Enable High Credit Limit           HICRLIMT$

        REM Def/Enable High Credit Limit Date      HICRDATE$
            return

L20290: REM Def/Enable Average Days to Pay         AVG_DAYS$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L22160,         /* NSF Check Date         */~
                              L22220,         /* NSF Check Number       */~
                              L22250,         /* Invoice Number         */~
                              L22280,         /* NSF Check Amount       */~
                              L22310,         /* Amount Paid            */~
                              L22340          /* Date Paid              */
            return

L22160: REM Def/Enable NSF Check Date              NSF_DATE$
            if nsf_date$(c%) <> " " and nsf_date$(c%) <> blankdate$ then return
                nsf_date$(c%) = date
                call "DATEFMT" (nsf_date$(c%))
                return

L22220: REM Def/Enable NSF Check Number            NSF_NMBR$
            return

L22250: REM Def/Enable Invoice Number              NSF_INVC$
            return

L22280: REM Def/Enable NSF Check Amount            NSF_AMNT$
            return

L22310: REM Def/Enable Amount Paid                 NSF_PAID$
            return

L22340: REM Def/Enable Date Paid                   NSF_DTPD$
            if nsf_paid(c%) <> 0 then goto L22390
                nsf_dtpd$(c%) = " "
                enabled% = 0%
                return
L22390:     if nsf_dtpd$(c%) <> " " and nsf_dtpd$(c%) <> blankdate$ then return
                nsf_dtpd$(c%) = date
                call "DATEFMT" (nsf_dtpd$(c%))
                return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Customer Code, partial, or '?' to see list.            ",~
         "Enter Total Sales to Date.                                   ",~
         "Enter Total Credits to Date.                                 ",~
         "Enter High Credit Limit and/or Date.                         ",~
         "Enter Average Days to Pay and/or # of applied Payments in compu~
        ~tation."

        scrn2_msg  :  data                                               ~
         "Enter NSF Check Date.                                        ",~
         "Enter NSF Check Number.                                      ",~
         "Enter Invoice Number if known.                               ",~
         "Enter NSF Check Amount.                                      ",~
         "Enter Amount of NSF check paid.                              ",~
         "Enter Date NSF Check Paid.                                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables_1
            init (" ") errormsg$, inpmessage$, avg_days$, customer$,     ~
                custname$, dynamnts$(), dyndates$(), hicrdate$,          ~
                hicrlimt$, linvnmbr$, lpaychek$, netsales$, totcredt$,   ~
                totsales$, str(line2$,,61%), moddate$, moduser$,         ~
                dateopen$, chnguser$, nbrpmnts$
            mat dynamnts = zer
            call "ALLFREE"

        initialize_variables_2
            init (" ") nsf_amnt$, nsf_chek$(), nsf_date$(), nsf_dtpd$(), ~
                nsf_invc$(), nsf_net$, nsf_paid$, nsf_modd$(), nsf_modu$()
            mat nsf_amnt = zer
            mat nsf_atot = zer
            mat nsf_ptot = zer
            mat nsf_paid = zer
            nsf% = 0%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            nsf% = 0%           /* Initialize number of NSF line items */
            call "READ100" (#01, customer$, f1%(1%))       /* CCRMASTR */
                if f1%(1%) = 0% then goto dataload_exit
            get #01 using L35040, customer$, totsales, totcredt, hicrlimt,~
                hicrdate$, linvamnt, linvnmbr$, lpayamnt, lpaychek$,     ~
                avg_days, nbrpmnts%, dyndates$(), dynamnts(), moduser$,  ~
                moddate$
            convert totsales to totsales$, pic (#########.##)
            convert totcredt to totcredt$, pic (#########.##)
            gosub compute_net_sales
            convert round(hicrlimt, 0) to hicrlimt$, pic (############)
            convert round(linvamnt, 2) to linvamnt$, pic (#########.##)
            convert round(lpayamnt, 2) to lpayamnt$, pic (#########.##)
            convert int(avg_days) to avg_days$, pic (####0)
            convert nbrpmnts% to nbrpmnts$, pic (####0)
            call "DATEFMT" (hicrdate$)
            call "DATEFMT" (moddate$)

*        The non-editable (dynamic) balances. Round 'em as per CUSINPUT.
            for x% = 1% to 4%
                convert round(dynamnts(x%), 2) to dynamnts$(x%),         ~
                     pic(-###,###,###.00)
            next x%

*        The non-editable (dynamic) dates.
            for x% = 1% to 5%
                call "DATEFMT" (dyndates$(x%))
            next x%

*        Get a couple of items from the CUSTOMER Master file.
            call "READ100" (#10, customer$, f1%(10%))
            if f1%(10%) = 0% then goto L30430   /* This would be awful! */
                get #10 using L30400, dateopen$, chnguser$
L30400:              FMT POS(220), CH(6), POS(250), CH(3)
                call "DATEFMT" (dateopen$)

L30430
*        If there are line items (NSF events), put 'em in the arrays.
            plowkey$ = xor plowkey$
            str(plowkey$,,9%) = customer$

        plow_the_nsfs
            call "PLOWNEXT" (#02, plowkey$, 9%, f1%(2%))   /* CCRLINES */
                if f1%(2%) = 0% then goto dataload_exit
            nsf% = nsf% + 1%
            if nsf% <= dim(nsf_amnt(), 1%) then goto L30540
                nsf% = nsf% - 1%
                goto dataload_exit
L30540:     get #02 using L35220, customer$, nsf_sqnc$(nsf%),             ~
                nsf_date$(nsf%), nsf_chek$(nsf%), nsf_invc$(nsf%),       ~
                nsf_amnt(nsf%), nsf_paid(nsf%), nsf_dtpd$(nsf%),         ~
                nsf_modu$(nsf%), nsf_modd$(nsf%)
            call "DATEFMT" (nsf_date$(nsf%))
            call "DATEFMT" (nsf_dtpd$(nsf%))
            call "DATEFMT" (nsf_modd$(nsf%))
            goto plow_the_nsfs

        dataload_exit           /* The ONLY exit from DATALOAD, please */
            gosub compute_z
            gosub total_nsf_editor
            return

        REM *************************************************************~
            *        R E C O R D   L A Y O U T   F O R M A T S          *~
            *************************************************************

L35040:     FMT /* File #01- CCRMASTR Master file                      */~
                CH(9),         /*   1/ 9-   Customer Code (Key)        */~
                PD(14,4),      /*  10/ 8-   Total Sales                */~
                PD(14,4),      /*  18/ 8-   Total Credits              */~
                PD(14,4),      /*  26/ 8-   High Credit Limit          */~
                CH(6),         /*  34/ 6-   High Credit Limit Date     */~
                PD(14,4),      /*  40/ 8-   Last Invoice Amount        */~
                CH(8),         /*  48/ 8-   Last Invoice Number        */~
                PD(14,4),      /*  56/ 8-   Last Payment Amount        */~
                CH(10),        /*  64/10-   Last Payment Check #       */~
                PD(14,4),      /*  74/ 8-   Average # Days to Pay      */~
                BI(2),         /*  82/ 2-   # Payments in Average Days */~
                5*CH(6),       /*  84/30-   'Dynamic' dates fr CUSTOMER*/~
                4*PD(14,4),    /* 114/32-   'Dynamic' amnts fr CUSTOMER*/~
                CH(3),         /* 146/ 3-   User Last Modified         */~
                CH(6),         /* 149/ 6-   Date Last Modified         */~
                CH(46)         /* 155/46-   Filler                     */

L35220:     FMT /* File #02- CCRLINES NSF Event file                   */~
                CH(9),                   /* Customer Code (Key)        */~
                CH(3),                   /* Key tie-breaker (Seq #)    */~
                CH(6),                   /* NSF Check Date             */~
                CH(10),                  /* NSF Check Number           */~
                CH(8),                   /* NSF Check Invoice #        */~
                PD(14,4),                /* NSF Check Amount           */~
                PD(14,4),                /* NSF Amount Check Paid      */~
                CH(6),                   /* NSF Date Check Paid        */~
                CH(3),                   /* User Last Modified         */~
                CH(6),                   /* Date Last Modified         */~
                CH(33)                   /* Filler                     */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% = 0% then lfac$(1%) = hex(8c)
            on fieldnr% gosub L40200,         /* Customer Code          */~
                              L40210,         /* Total Sales to Date    */~
                              L40210,         /* Total Credits to Date  */~
                              L40220,         /* High Credit Limit & Dt */~
                              L40210          /* Average Days to Pay    */
              goto L40240

            lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:     lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:     lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */
L40220:     lfac$(4%) = hex(82) : lfac$(21%) = hex(81) : return

L40240:     accept                                                       ~
                at (01,02), "Customer Credit File Maintenance",          ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (06,02), "Customer Code",                             ~
                at (06,25), fac(lfac$( 1%)), customer$          , ch(09),~
                at (06,40), fac(hex(8c)),    custname$          , ch(32),~
                                                                         ~
                at (07,02), "Total Sales to Date",                       ~
                at (07,25), fac(lfac$( 2%)), totsales$          , ch(12),~
                                                                         ~
                at (08,02), "Total Credits to Date",                     ~
                at (08,25), fac(lfac$( 3%)), totcredt$          , ch(12),~
                at (08,40), "Net Sales to Date",                         ~
                at (08,59), fac(hex(8c)),    netsales$          , ch(12),~
                                                                         ~
                at (09,02), "High Credit Limit",                         ~
                at (09,25), fac(lfac$( 4%)), hicrlimt$          , ch(12),~
                at (09,40), "High Credit Limit Date",                    ~
                at (09,63), fac(lfac$(21%)), hicrdate$          , ch(08),~
                                                                         ~
                at (10,02), "Average Days to Pay",                       ~
                at (10,32), fac(lfac$( 5%)), avg_days$          , ch(05),~
                at (10,40), "# applied Payments in Avg",                 ~
                at (10,66), fac(lfac$( 5%)), nbrpmnts$          , ch(05),~
                                                                         ~
                at (12,02), "Last Modified on",                          ~
                at (12,19), fac(hex(8c)),   moddate$            , ch(08),~
                at (12,28), "by",                                        ~
                at (12,31), fac(hex(8c)),   moduser$            , ch(03),~
                                                                         ~
                at (14,23), "S U M M A R Y   I N F O R M A T I O N",     ~
                                                                         ~
                at (15,02), "Account Opened",                            ~
                at (15,17), fac(hex(8c)),   dateopen$           , ch(08),~
                at (15,57), "Bill O/O",                                  ~
                at (15,66), fac(hex(8c)),   dynamnts$(1%)       , ch(15),~
                                                                         ~
                at (16,02), "Last Activity",                             ~
                at (16,17), fac(hex(8c)),   dyndates$(1%)       , ch(08),~
                at (16,57), "Ship O/O",                                  ~
                at (16,66), fac(hex(8c)),   dynamnts$(2%)       , ch(15),~
                                                                         ~
                at (17,02), "Last Invoice",                              ~
                at (17,17), fac(hex(8c)),   dyndates$(2%)       , ch(08),~
                at (17,26), fac(hex(8c)),   linvnmbr$           , ch(08),~
                at (17,37), fac(hex(8c)),   linvamnt$           , ch(12),~
                at (17,57), "Open A/R",                                  ~
                at (17,66), fac(hex(8c)),   dynamnts$(3%)       , ch(15),~
                                                                         ~
                at (18,02), "Last Payment",                              ~
                at (18,17), fac(hex(8c)),   dyndates$(3%)       , ch(08),~
                at (18,26), fac(hex(8c)),   lpaychek$           , ch(10),~
                at (18,37), fac(hex(8c)),   lpayamnt$           , ch(12),~
                at (18,57), "High A/R",                                  ~
                at (18,66), fac(hex(8c)),   dynamnts$(4%)       , ch(15),~
                                                                         ~
                at (19,02), "Last Changed",                              ~
                at (19,17), fac(hex(8c)),   dyndates$(4%)       , ch(08),~
                at (19,26), "by",                                        ~
                at (19,29), fac(hex(8c)),   chnguser$           , ch(03),~
                at (19,59), "... on",                                    ~
                at (19,73), fac(hex(8c)),   dyndates$(5%)       , ch(08),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L41010
                call "MANUAL" ("CCRINPUT") : goto L40240

L41010:     if keyhit% <> 15% then L41040
                call "PRNTSCRN" : goto L40240

L41040:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
        if edit% = 2% then L41230     /*  Input Mode             */
            pf$(1%) = "(1)Start Over  (4)Prev Field            " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                    "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41190
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L41190:     if fieldnr% > 2% then L41210
                str(pf$(1%),16%,13%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L41210:     return

L41230: if fieldnr% > 0% then L41370  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "(11)Append NSFs        (13)Instructions"
            pf$(2%) = "(2)NSF Summary                          " &       ~
                      "(12)Delete ALL NSFs    (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Save Data   "
            pfkeys$ = hex(0102ffffffffffffffff0b0c0dff0f1000)
            if nsf% < dim(nsf_amnt(), 1%) then goto L41330
                str(pf$(1%),41%,15%) = " " : str(pfkeys$,11%,1%) = hex(ff)
L41330:     if nsf% <> 0% then goto L41360
                str(pf$(2%), 1%,14%) = " " : str(pfkeys$, 2%,1%) = hex(ff)
                str(pf$(2%),41%,19%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41360:     return
L41370:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
            gosub'050(2%, fieldnr%)
            gosub set_pf2
            convert nsf_amnt(c%) to nsf_amnt$, pic (#########.##)
            convert nsf_paid(c%) to nsf_paid$, pic (#########.##)
            gosub compute_nsf_outstanding
            line4$ = "Customer: " & customer$ & " " & custname$ & "; " & ~
                "Sequence #: " & nsf_sqnc$(c%)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L42250,         /* NSF Check Date         */~
                              L42250,         /* NSF Check Number       */~
                              L42250,         /* Invoice Number         */~
                              L42260,         /* NSF Check Amount       */~
                              L42260,         /* Amount NSF Paid        */~
                              L42250          /* Date Paid              */
            goto L42280

            lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42250:     lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42260:     lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42280:     accept                                                       ~
                at (01,02), "Customer Credit File Maintenance- NSF Checks~
        ~",                                                               ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                at (04,02), fac(hex(8c)), line4$                , ch(79),~
                                                                         ~
                at (06,02), "NSF Check Date",                            ~
                at (06,25), fac(lfac$( 1%)), nsf_date$(c%)      , ch(08),~
                                                                         ~
                at (07,02), "NSF Check Number",                          ~
                at (07,25), fac(lfac$( 2%)), nsf_chek$(c%)      , ch(10),~
                                                                         ~
                at (08,02), "Invoice Number",                            ~
                at (08,25), fac(lfac$( 3%)), nsf_invc$(c%)      , ch(08),~
                                                                         ~
                at (09,02), "NSF Check Amount",                          ~
                at (09,25), fac(lfac$( 4%)), nsf_amnt$          , ch(12),~
                at (09,40), "Net Outstanding",                           ~
                at (09,58), fac(hex(8c)),    netoutst$          , ch(12),~
                                                                         ~
                at (10,02), "Amount of Check Paid",                      ~
                at (10,25), fac(lfac$( 5%)), nsf_paid$          , ch(12),~
                                                                         ~
                at (11,02), "Date Paid",                                 ~
                at (11,25), fac(lfac$( 6%)), nsf_dtpd$(c%)      , ch(08),~
                                                                         ~
                at (16,02), "Last Modified By",                          ~
                at (16,25), fac(hex(8c)),   nsf_modu$(c%)       , ch(03),~
                at (16,29), "on",                                        ~
                at (16,32), fac(hex(8c)),   nsf_modd$(c%)       , ch(08),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L42720
                call "MANUAL" ("CCRINPUT") : goto L42280

L42720:     if keyhit% <> 15% then L42750
                call "PRNTSCRN" : goto L42280

L42750:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf2
        if edit% = 2% then L42940     /*  Input Mode             */
            pf$(1%) = "(1)Start Over  (4)Prev Field            " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Append "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L42900
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L42900:     if fieldnr% > 2% then L42920
                str(pf$(1%),16%,13%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L42920:     return

L42940: if fieldnr% > 0% then L43070  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over  (4)Edit Prev NSF         " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "               (5)Edit Next NSF         " &       ~
                      "(12)Delete This NSF    (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       " & pf16$
            pfkeys$ = hex(01ffff0405ffffffffffff0c0dff0f1000)
            if c% <> 1% then goto L43040
                str(pf$(1%),16%,16%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L43040:     if c% < nsf% then goto L43060
                str(pf$(2%),16%,16%) = " " : str(pfkeys$,5%,1%) = hex(ff)
L43060:     return
L43070:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

L44000: REM *************************************************************~
            *           N S F   C H E C K   S U M M A R Y               *~
            *************************************************************

            gosub set_pf3
            line4$ = "Customer: " & customer$ & " " & custname$
            init (" ") dsply$()
            for t% = 1% to min(l%, nsf%, nsf% - p% + 1%)
                str(dsply$(t%), 1%, 3%) = nsf_sqnc$(p%+t%-1%)
                str(dsply$(t%), 5%, 8%) = nsf_date$(p%+t%-1%)
                convert nsf_amnt(p%+t%-1%) to str(dsply$(t%),13%,12%),   ~
                     pic (#########.##)
                str(dsply$(t%),26%,10%) = nsf_chek$(p%+t%-1%)
                str(dsply$(t%),37%, 8%) = nsf_invc$(p%+t%-1%)
                convert nsf_paid(p%+t%-1%) to str(dsply$(t%),45%,12%),   ~
                     pic (#########.##)
                str(dsply$(t%),58%, 8%) = nsf_dtpd$(p%+t%-1%)
                if nsf_amnt(p%+t%-1%) - nsf_paid(p%+t%-1%) < 0           ~
                    then convert nsf_amnt(p%+t%-1%) - nsf_paid(p%+t%-1%) ~
                        to str(dsply$(t%),66%,12%), pic (-########.##)   ~
                    else convert nsf_amnt(p%+t%-1%) - nsf_paid(p%+t%-1%) ~
                        to str(dsply$(t%),66%,12%), pic (#########.##)
            next t%

L44240:     accept                                                       ~
                at (01,02), "Customer Credit File Maintenance- NSF Check ~
        ~Summary",                                                        ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (04,02), fac(hex(8c)), line4$                , ch(79),~
                at (05,02), fac(hex(ac)), line5$                , ch(79),~
                                                                         ~
                at (06,02), fac(hex(8c)), dsply$( 1%)           , ch(79),~
                at (07,02), fac(hex(8c)), dsply$( 2%)           , ch(79),~
                at (08,02), fac(hex(8c)), dsply$( 3%)           , ch(79),~
                at (09,02), fac(hex(8c)), dsply$( 4%)           , ch(79),~
                at (10,02), fac(hex(8c)), dsply$( 5%)           , ch(79),~
                at (11,02), fac(hex(8c)), dsply$( 6%)           , ch(79),~
                at (12,02), fac(hex(8c)), dsply$( 7%)           , ch(79),~
                at (13,02), fac(hex(8c)), dsply$( 8%)           , ch(79),~
                at (14,02), fac(hex(8c)), dsply$( 9%)           , ch(79),~
                at (15,02), fac(hex(8c)), dsply$(10%)           , ch(79),~
                at (16,02), fac(hex(8c)), dsply$(11%)           , ch(79),~
                at (17,02), fac(hex(8c)), dsply$(12%)           , ch(79),~
                at (18,02), fac(hex(8c)), dsply$(13%)           , ch(79),~
                at (19,02), fac(hex(8c)), dsply$(14%)           , ch(79),~
                at (20,02), fac(hex(8c)), dsply$(15%)           , ch(79),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L44590
                call "MANUAL" ("CCRINPUT") : goto L44240

L44590:     if keyhit% <> 15% then L44620
                call "PRNTSCRN" : goto L44240

L44620:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf3
            pf$(1%) = "(1)Start Over  (4)Prev Page  (7)Up      " &       ~
                      "(11)Append NSFs        (13)Instructions"
            pf$(2%) = "(2)1st Page    (5)Next Page             " &       ~
                      "(12)Delete ALL NSFs    (15)Print Screen"
            pf$(3%) = "(3)Last Page   (6)Down                  " &       ~
                      "                       (16)Return      "
            pfkeys$ = hex(01020304050607ffffff0b0c0dff0f1000)
            if nsf% < dim(nsf_amnt(), 1%) then goto L44760
                str(pf$(1%),41%,15%) = " " : str(pfkeys$,11%,1%) = hex(ff)
L44760:     if nsf% <> 0% then goto L44780
                str(pf$(2%),41%,19%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L44780:     if p% <> 1% then goto L44830
                str(pf$(2%),,11%), str(pf$(1%),16%,12%),                 ~
                     str(pf$(3%),16%,7%) = " "
                str(pfkeys$,2%,1%), str(pfkeys$,4%,1%),                  ~
                     str(pfkeys$,6%,1%) = hex(ff)
L44830:     if p% <> z% then goto L44880
                str(pf$(3%),,12%), str(pf$(2%),16%,12%),                 ~
                     str(pf$(1%),30%,5%) = " "
                str(pfkeys$,3%,1%), str(pfkeys$,5%,1%),                  ~
                     str(pfkeys$,7%,1%) = hex(ff)
L44880:     inpmessage$ = "Select option -OR- position cursor and press"&~
                " (RETURN) to see an NSF event."
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            moddate$ = date$
            moduser$ = userid$
            on fieldnr% gosub L50170,         /* Customer Code          */~
                              L50340,         /* Total Sales to Date    */~
                              L50390,         /* Total Credits to Date  */~
                              L50440,         /* High Credit Limit & Dt */~
                              L50530          /* Average Days to Pay    */
            return

L50170: REM Test for Customer Code                CUSTOMER$
            custname$ = " "
            msg$ = hex(06) & "Select a Customer Code."
            if customer$ = "?" then customer$ = " "
*        Customer selection list (if any) will be from the CUSTOMER file.
            call "PLOWCODE" (#10, customer$, msg$, 0%, .3, f1%(10%))
            if f1%(10%) <> 0% then goto L50260
                errormsg$ = "Customer Code not in CUSTOMER file."
                return
L50260:     get #10 using L50270, customer$, custname$
L50270:          FMT CH(9), CH(30)
            call "PUTPAREN" (custname$)
            gosub dataload
            if f1%(1%) = 0% then return
                return clear all
                goto editpg1

L50340: REM Test for Total Sales to Date          TOTSALES$
            call "NUMTEST" (totsales$, 0, 9e11, errormsg$, -2.2, totsales)
            if errormsg$ = " " then gosub compute_net_sales
            return

L50390: REM Test for Total Credits to Date        TOTCREDT$
            call "NUMTEST" (totcredt$, 0, 9e11, errormsg$, -2.2, totcredt)
            if errormsg$ = " " then gosub compute_net_sales
            return

L50440: REM Test for High Credit Limit            HICRLIMT$
            call "NUMTEST" (hicrlimt$, 0, 9e11, errormsg$, -.0001,       ~
                hicrlimt)
            if errormsg$ <> " " then return

        REM Test for High Credit Limit Date       HICRDATE$
            call "DATEOK" (hicrdate$, u3%, errormsg$)
            return

L50530: REM Test for Average Days to Pay          AVG_DAYS$, NBRPMNTS$
            call "NUMTEST" (avg_days$, 0, 99999, errormsg$, -.0001,      ~
                avg_days)
            if errormsg$ = " " then goto L50590
                errormsg$ = errormsg$ & ": Avg # Days"
                return
L50590:     avg_days = int(avg_days)
            call "NUMTEST" (nbrpmnts$, 0, 99999, errormsg$, -.0001,      ~
                nbrpmnts)
            if errormsg$ = " " then goto L50650
                errormsg$ = errormsg$ & ": # Payments"
                return
L50650:     nbrpmnts% = int(nbrpmnts)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            nsf_modd$(c%) = date$
            nsf_modu$(c%) = userid$
            on fieldnr% gosub L52180,         /* NSF Check Date         */~
                              L52220,         /* NSF Check Number       */~
                              L52250,         /* Invoice Number         */~
                              L52280,         /* NSF Check Amount       */~
                              L52340,         /* Amount of NSF Paid     */~
                              L52400          /* Date Paid              */
            return

L52180: REM Test for NSF Check Date               NSF_DATE$
            call "DATEOK" (nsf_date$(c%), u3%, errormsg$)
            return

L52220: REM Test for NSF Check Number             NSF_NMBR$
            return

L52250: REM Test for Invoice Number               NSF_INVC$
            return

L52280: REM Test for NSF Check Amount             NSF_AMNT$
            call "NUMTEST" (nsf_amnt$, 0, 9e11, errormsg$, -2.2,         ~
                nsf_amnt(c%))
            if errormsg$ = " " then gosub compute_nsf_outstanding
            return

L52340: REM Test for NSF Check Amount Paid        NSF_PAID$
            call "NUMTEST" (nsf_paid$, 0, 9e11, errormsg$, -2.2,         ~
                nsf_paid(c%))
            if errormsg$ = " " then gosub compute_nsf_outstanding
            return

L52400: REM Test for Date Paid                    NSF_DTPD$
            if nsf_dtpd$(c%) = " " or nsf_dtpd$(c%) = blankdate$ then return
            call "DATEOK" (nsf_dtpd$(c%), u3%, errormsg$)
            return

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
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
