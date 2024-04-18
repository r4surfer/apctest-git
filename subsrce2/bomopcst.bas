        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    OOO   M   M   OOO   PPPP    CCC    SSS   TTTTT   *~
            *  B   B  O   O  MM MM  O   O  P   P  C   C  S        T     *~
            *  BBBB   O   O  M M M  O   O  PPPP   C       SSS     T     *~
            *  B   B  O   O  M   M  O   O  P      C   C      S    T     *~
            *  BBBB    OOO   M   M   OOO   P       CCC    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMOPCST - Subroutine to print Option BOM cost audit      *~
            *            report intended to allow the user to           *~
            *            see how the costs of regular parts and option  *~
            *            replacement parts contribute to the total cost *~
            *            of a generic part when replacements have been  *~
            *            made.                                          *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/12/94 ! Original                                 ! WPH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "BOMOPCST"                                               ~
                 (demand$,               /* Sales Order + Line #       */~
                 explode_all$,           /* Flag, 'N' = explode generic*/~
                 print_descr$,           /* Flag to print part descrpts*/~
                 assypart$,              /* Top level part number      */~
                 effdate$,               /* Due date                   */~
                 #4,                     /* HNYMASTR                   */~
                 #5,                     /* BOMMASTR                   */~
                 #7,                     /* BOMSPEC                    */~
                 #10,                    /* SYSFILE2                   */~
                 #11,                    /* ENGMASTR                   */~
                 ret%)                   /* Return Code                */

        dim                                                              ~
            assy$25,                     /* Part Number                */~
            bomid$3,                     /* Component Bill version     */~
            bom$(490)3,                  /* Effective Bill versions    */~
            assypartdescr$32,            /* Top Assembly Part Number   */~
            assypart$25,                 /* Top Assembly Part Number   */~
            assypartkey$99,              /* Key for top level part     */~
            bommkr$1,                    /* Bill marker                */~
            cbomid$3,                    /* Component Bill version     */~
            cdesc$32,                    /* Component Description      */~
            company$60,                  /* Company or Division Name   */~
            componentkey$28,             /* Component part + bim id    */~
            column%(30),                 /* Column                     */~
            component$25,                /* Component part             */~
            date$8,                      /* Date for screen display    */~
            dot$10,                      /* Indention dots             */~
            demand$19,                   /* SO number + line           */~
            effdate$6,                   /* Effective date (due date)  */~
            explode_all$1,               /* Explode All Parts?         */~
            ext(30),                     /* Qty extensions             */~
            hdr_msg$35,                  /* Report header message      */~
            print_descr$1,               /* Print Part Descriptions?   */~
            inbomid$3,                   /* BOM version                */~
            id$3,                        /* BOM version                */~
            line2$79,                    /* Screen Line #2             */~
            m$(30)1,                     /* Array of markers           */~
            material(30),                /* Sum of mtl cost at a level */~
            message$79,                  /* Text for use in ASKUSER    */~
            op$1,                        /* Option Flag                */~
            parttype$3,                  /* Part type                  */~
            p_descrip$34,                /* Part Descriptions          */~
            p$(30)31,                    /* Part, bomid, seq as key    */~
            part$25,                     /* Part Number                */~
            partkey$28,                  /* Part Number and BOMID      */~
            parent_key$(30)33,           /* Parent stack               */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            ref_key$31,                  /* Part, bom, seq number      */~
            rpttitle$60,                 /* Report Title               */~
            temp$20,                     /* Working variable           */~
            time$8,                      /* System Time                */~
            total$10,                    /* Total calculated cost      */~
            rtecost$10,                  /* Route Value Added          */~
            dtlcost$10,                  /* Misc. Value Added          */~
            qtyper$10,                   /* Quantity per parent        */~
            qtyext$10,                   /* Cumulative extended mult.  */~
            contrib$10,                  /* VA cost conribution        */~
            extcost$10,                  /* Cost Extension             */~
            unitcost$10,                 /* Unit std cost of each item */~
            seq$3,                       /* Sequence number            */~
            seqnr$4,                     /* Sequence number            */~
            level$3,                     /* Bill level                 */~
            userid$3                     /* Current User Id            */~

        dim sum(12),                     /* Cost arrays from STCCOST   */~
            bom(12),                     /*             "              */~
            rte(12),                     /*             "              */~
            dtl(12)                      /*             "              */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64)                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00   "
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
            * #01 ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen = 33,                      ~
                        alt key  1, keypos =   26, keylen =  8

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            ret% = 0%  /* all OK, returns RET% = 1% if error */
            fs%(64) = fs%(64)  /* just so I don't have to delete it */

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, k%)

            rpttitle$ = "Option Bill Cost Roll-up Audit" &               ~
                        " Report                       "

            call "STRING" addr("CT", rpttitle$, 60%, rpttitle$)

            str(line2$,62) = "BOMOPCST: " & str(cms2v$,,8)

        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************

            call "SHOSTAT" ("Building the Workfile")

            call "PIPINDEX" (#10, effdate$, dateindex%, idxerr%)
            if idxerr% = 0% then L13090
               message$ = "Fatal Calendar Problem."
               goto L13120

L13090:     call "READ100"(#4, assypart$, f1%(4))
               if f1%(4) = 1% then L13095
            message$ = "Couldn't read HNYMASTR file for part " & assypart$
               goto L13120
L13095:     get #4, using L13096, assypartdescr$,   parttype$
L13096:        FMT POS(26), CH(32), POS(180), CH(3)

*       If part is not generic then blow out with askuser message
            if parttype$ = "000" then L13180
            message$ = "The part sold on this sales order line is not " &~
                       "generic part."

L13120:     k% = 2%
            call "ASKUSER"(k%, "* * * CANNOT CONTINUE * * *",            ~
            message$,                                                    ~
            "Try another sales order line, or inform your system admin"& ~
            "istrator.",                                                 ~
            "Press any key to acknowledge this message.")
            ret% = 1%
            goto exit_program


L13180
*       Check to see if options have been selected and obtain BOM ID

            init (hex(00)) readkey2$
            str(readkey2$,1,19) = str(demand$,,)

            call "PLOWALTS" (#7, readkey2$, 1%, 19%, f1%(7))
                if f1%(7) = 1% then L13192
                message$ = "Options not yet selected for this SO Line."
                   goto L13120
L13192:     get #7 using L13193, inbomid$
L13193:         FMT POS(51), CH(3)

*       Open the workfile and get going
            call "WORKOPEN" (#1, "SHARE", 100%, f2%(1))
            mat material = zer
            assypartkey$ = str(assypart$,,25) & str(inbomid$,,3)
            pl% = 0%
            l%, p% = 0%
            mat column% = zer
            init(" ")  p$(), m$()
            dtlcost, rtecost = 0
            call "STCCOSTS" (assypart$, " ", #10, 3%, cstccost,          ~
                                      sum(), bom(), rte(), dtl())
                for i% = 1% to 12%
                     dtlcost = dtlcost + dtl(i%)
                     rtecost = rtecost + rte(i%)
                next i%
            count% = 1%
            put   #1, using L14950, assypart$, count%, pl%,               ~
                         dtlcost,  rtecost, 1, 1, rtecost + dtlcost,     ~
                         inbomid$, 0 , " "
            write #1

            str(parent_key$(pl% + 1%),1,25) = str(assypart$,,25)
            put str(parent_key$(pl% + 1%),26,), count%
            put str(parent_key$(pl% + 1%),30,), pl%

            gosub'10(assypartkey$, 1)

*       Add lower level costs to top assembly material cost
            call "READ101"(#1, str(parent_key$(1%),,), f1%(1))
               get #1, using L14073, value_added_contrib
L14073:          FMT POS(66), PD(14,4)

            ext_cost =  material(1) + value_added_contrib

               put #1, using L14078, ext_cost
L14078:          FMT POS(77), PD(14,4)
            rewrite #1
            goto generate_report

        deffn'10(partkey$, extqty)
            pl% = pl% + 1%           /* PL% is actual level      */
            l% = max(1, pl% - p%)    /* L% is the 'Pseudo-Level' */
            p$(pl%) = partkey$
            ext(pl%) = extqty
            if extqty <= 0 then L14730  /* end */

L14170:     call "PLOWNEXT" (#5, p$(pl%), 28%, f1%(5))
                if f1%(5) = 0% then L14730

*        Load From BOMMASTR
            if str(p$(pl%),29,3) = "  0" then L14170 /* skip header */
            count% = count% + 1%
                get #5, using L14240, component$,assy$,id$,seq$,qn,sz,    ~
                                     fixed,over,bommkr$,op$,cbomid$
L14240:         FMT 2*CH(25),2*CH(3),4*PD(14,4),CH(2),CH(1),CH(3)

            m$(pl%) = bommkr$
            call "DESCRIBE"(#4, component$, cdesc$, 1%, f1%(4))
                if f1%(4) = 0% then L14170 /* non-stocked part */

            if explode_all$ = "Y" then L14320
            get #4, using L14300, type$
L14300:         FMT XX(179), CH(3)
            if op$ <> "Y" and type$ <> "000" then grab_std /* & Move On */

L14320:     gosub'190(component$)  /* Find effect. BOM for Component*/
            componentkey$ = str(component$,,25) & str(bomid$,,3)
            if op$ <> "Y" then L14550  /* Option is somewhere below.*/
                  column%(l%)=column%(l%)+1% /* Count Of Hits This Levl*/

                /* read from BOMSPEC to bring in the replacement */
                  init(hex(00)) readkey$
                  str(readkey$,1,28) =  str(p$(pl%),,28)
                  str(readkey$,29,3) =  str(seq$,,)
                  str(readkey$,32,19) =  str(demand$,,19)
                  put readkey$, using L14450, l%, column%(l%) - 1%
L14450:              FMT POS(51), BI(2), BI(2)
            /* use a plow in the event that previous re-selects have  */
            /* mucked up the sequence numbers                         */
               call "PLOWNEXT" (#7, readkey$, 52%, f1%(7))
                   get #7, using L14510,           column%(l%), ref_key$, ~
                       qn, sz, cbomid$
L14510:            FMT  POS(78), BI(2), CH(25), 2*PD(14,4), CH(3)
               component$ = ref_key$
               gosub'190(component$)
               componentkey$ = str(component$,,25) & str(bomid$,,3)
L14550:     if m$(pl%) = "P" then p% = p% + 1%
            dtlcost, rtecost = 0
            call "STCCOSTS" (componentkey$,  " ", #10, 3%, cstccost,     ~
                                      sum(), bom(), rte(), dtl())
                for i% = 1% to 12%
                     dtlcost = dtlcost + dtl(i%)
                     rtecost = rtecost + rte(i%)
                next i%

            per_parent, extqty = qn * sz  + over
            extqty = round((extqty*ext(pl%)) + fixed, 2)
            va = (per_parent * ext(pl%)) * (rtecost + dtlcost)

            put #1, using L14950, component$,                count%, pl%, ~
             dtlcost, rtecost, per_parent, ext(pl%) , va, bomid$, 0 , " "

            write #1

*        Capture this level material so we can do extended value later
            material(pl%) = material(pl%) + va

*        Save the key so we can get back into the workfile later
            str(parent_key$(pl% + 1%),1,25) = str(component$,,25)
            put str(parent_key$(pl% + 1%),26,), count%
            put str(parent_key$(pl% + 1%),30,), pl%

            if pl% < 30% then gosub'10(componentkey$,extqty)
                                          /* Do Comps If Not At Bottom */
            goto L14170  /* Plow for next component */

L14730
*        End routine gracefully
            pl% = pl% - 1%
            if pl% = 0% then goto L14770/* Avoid M$(PL%) blow-up */
                if m$(pl%) = "P" then p% = p% - 1%
L14770:     l% = max(1, pl% - p%)   /* L% Is The 'Pseudo-Level'*/

            if pl% = 0% then L14800     /* Avoid (PL% = 0%) blow-up */

*       Update the parent assy wf record on our way up
            call "READ101"(#1, str(parent_key$(pl% + 1%),,), f1%(1))
               get #1, using L14786, value_added_contrib
L14786:          FMT POS(66), PD(14,4)

            ext_cost =  material(pl% + 1%) + value_added_contrib

            put #1, using L14796, ext_cost
L14796:          FMT POS(77), PD(14,4)
            rewrite #1
            material(pl%) =  material(pl%) + material(pl% + 1%)
            material(pl% + 1%) = 0
L14800:     return /* Eventually This Will Return Completely Out Of '10 */


        grab_std
            call "STCCOSTS" (component$, " ", #10, 3%, cstccost,         ~
                                sum(), bom(), rte(), dtl())
            qty_per = qn * sz + over
            ext_qty = round((qty_per * ext(pl%)) + fixed, 2)
            va = ext_qty * cstccost

            for i% = 1% to 12%
                dtlcost = dtlcost + dtl(i%)
                rtecost = rtecost + rte(i%)
                bomcost = bomcost + bom(i%)
            next i%

            dtlcost = dtlcost + rtecost  /* Total 'this level' */
            rtecost = bomcost  /* double use of the variable */

            gosub'190(component$)  /* Find effect. BOM for Component*/

            put   #1, using L14950, component$, count%, pl%, dtlcost,     ~
                      rtecost, qty_per + fixed, ext(pl%) , va , bomid$,  ~
                      ext_qty * cstccost,  " "
            write #1
                material(pl%) = material(pl%) +  va


            goto L14170

*       Format of workfile                                   POS  LEN
L14950:     FMT  CH(25),    /* pk    Part Number                1  25  */~
                 BI(04),    /* pk, 1 Seq Number encountered     26  4  */~
                 BI(04),    /* pk, 1 Level                      30  4  */~
                 PD(14,4),  /*       This level Detail Costs    34  8  */~
                 PD(14,4),  /*       This level Route Costs     42  8  */~
                 PD(14,4),  /*       Qty per immediate parent   50  8  */~
                 PD(14,4),  /*       Cumm. higher level extentn 58  8  */~
                 PD(14,4),  /*       VA Cost Contribution       66  8  */~
                 CH(03),    /*       Bom Id                     74  3  */~
                 PD(14,4),  /*       Extended value of this part77  8  */~
                 CH(16)     /*       Filler                     85 16  */~


        REM *************************************************************~
            *      G E T   E F F E C T I V E   B O M                    *~
            *-----------------------------------------------------------*~
            * Gets the current BOM for passed part based on dateindex%  *~
            *************************************************************

        deffn'190(part$)
            if ref_key$ <> "** DON'T USE ANYTHING **" then L15200
              bomid$, cbomid$ = " "
              return
L15200:     bomid$ = cbomid$
            if part$ <> assypart$ and cbomid$ <> " " then return
            if dateindex% = 0% then return  /* OFF PROD CALENDER? */
                readkey2$ = " "
                readkey2$ = str(part$,,25) & "1" & " "
                    call "PLOWNEXT" (#11, readkey2$, 26%, f1%(11))
                    if f1%(11) <> 1% then return
                       get #11, using L15280, str(bom$())
L15280:                         FMT XX(29), CH(1470)
                    if str(readkey2$,,25) <> str(part$,,25) then return
                    bomid$ = bom$(dateindex%)
            return

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            call "SHOSTAT" ("Printing the Report")
            hdr_msg$ = "Exploding all Assemblies"
            if explode_all$ <> "Y" then hdr_msg$ =                       ~
                       "Exploding Generic Assemblies Only"
            call "STRING" addr("CT", hdr_msg$, 35%, hdr_msg$)

            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head

            init(hex(00)) plowkey$
            total = 0

        print_next_record
            if lcntr% > 56% then gosub page_head
            call "PLOWALTS" (#1 , plowkey$, 1%, 0%, f1%(1))
              if f1%(1) = 0% then end_report

            get #1 , using L14950, part$,               seq%, level%,     ~
                                rtecost, dtlcost, qtyper,                ~
                       qtyext, contrib, bomid$, extcost, temp$

            if qtyper * qtyext <> 0 then L30196
                unitcost = 0
                goto L30210
L30196:     unitcost = extcost/(qtyper * qtyext)
L30210:     total = total + contrib


            call "CONVERT" (rtecost, 2.2, rtecost$)
            call "CONVERT" (dtlcost, 2.2, dtlcost$)
            call "CONVERT" (qtyper, 2.2, qtyper$)
            call "CONVERT" (qtyext, 2.2, qtyext$)
            call "CONVERT" (contrib, 2.2, contrib$)
            call "CONVERT" (extcost, 2.2, extcost$)
            call "CONVERT" (unitcost, 2.2, unitcost$)

            convert seq% to seqnr$, pic(####)
            convert level% to level$, pic(###)
            dot$ = " "
            if level% = 0% then L30380  /* no dots for top assy part */
            for i% = 1% to level%
                str(dot$, i%, 1%) = "*"
            next i%

*       Print the report lines
            if part$ = " " then part$ = "** DON'T USE ANYTHING **"
L30380:     print using L60170, dot$, part$, seqnr$, level$, bomid$,      ~
                               qtyper$, qtyext$, contrib$, extcost$,     ~
                              unitcost$
            lcntr% = lcntr% + 1%
            if print_descr$ = "N" then print_next_record

            call "DESCRIBE" (#4, part$, p_descrip$, 1%, f1%(4))
            print using L60180, p_descrip$
            lcntr% = lcntr% + 1%
            goto print_next_record


        end_report                /* Report Ending Routine */
            call "CONVERT" (total, 2.2, total$)
            print using L60190 /* line */
            print using L60200, assypart$, total$

            print skip(2)
            print using L64990     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            call "FILEBGON"(#1)
            goto exit_program

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "BOMOPCST"
            print using L60110, rpttitle$, pcntr%
            print using L60130, assypart$, hdr_msg$
            print using L60135, assypartdescr$
            print

            print using L60150  /* column headers */

            print using L60160  /* underline   */
            lcntr% = 6%
            return

        print_params           /* Print Page Zero */
            print page
            print using L64980, rpttitle$
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print skip(3)
            print using L64982, str(demand$,1,16)
            print using L64984, str(demand$,17,3)
            print using L64986, explode_all$
            print using L64988, print_descr$
            print skip(10)
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:RPTID

*       * Header Line 2
L60110: %                                    ############################~
        ~################################                      PAGE: ####

L60130: %Report Part: #########################           ###############~
        ~####################
L60135: %Description: ################################

*       * Report Body
L60150: %  Indent   Part Number/Descrip       Seq  Lvl BOM Qty/Parent Ext~
        ~ension  VA Contrib Ext Value  Unit Cost

L60160: %---------- ------------------------- ---- --- --- ---------- ---~
        ~------- ---------- ---------- ----------

L60170: %########## ######################### #### ### ### ########## ###~
        ~####### ########## ########## ##########

L60180: %              ##################################


L60190: %                                                                ~
        ~        ----------
L60200: %                        Total Cost for Part: ###################~
        ~######  ##########


        %** Report Title for page 0
L64980: %############################################################
L64982: %                           Sales Order Number          #########~
        ~#######
L64984: %                           Sales Order Line            ###

L64986: %                           Explode All Parts?            #
L64988: %                           Print Part Descriptions?      #

L64990: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            end
