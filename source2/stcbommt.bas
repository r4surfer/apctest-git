        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   TTTTT   CCC   BBBB    OOO   M   M  M   M  TTTTT   *~
            *  S        T    C      B   B  O   O  MM MM  MM MM    T     *~
            *   SSS     T    C      BBBB   O   O  M M M  M M M    T     *~
            *      S    T    C      B   B  O   O  M   M  M   M    T     *~
            *   SSS     T     CCC   BBBB    OOO   M   M  M   M    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCBOMMT - Prints Priced Bills of Materails.  Up to 16    *~
            *            Assembly Number May be Entered.  BOM's are     *~
            *            priced using the selected cost set.            *~
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
            * 07/22/87 ! ORIGINAL - REPLACES BOMSTAND             ! MJB *~
            * 10/26/87 ! No longer reverts to current cost set    ! HES *~
            * 11/10/87 ! Stop divide by zero in END_LEVEL logic   ! HES *~
            * 11/20/87 ! Extend totals on subassemblies           ! HES *~
            * 05/04/89 ! Resurrected from R5.00.04 and renamed to ! MJB *~
            *          !  STCBOMMT.  Added total cost to header.  !     *~
            * 06/12/91 ! Malco- Add Stocking UOM to report.       ! JIM *~
            * 09/20/93 ! Added support to new BOM Markers 'NC' &  ! JDH *~
            *          !  'NP', which are ignored for costing.    !     *~
            *          ! Fixed 'RE' & 'TL'; Ignored for costing.  !     *~
            *          ! PRR 12012  Return to INPUTMODE when done.!     *~
            *          ! PRR 12865  Corrected phantom costing.    !     *~
            *          ! Added PF14 to display report.            !     *~
            * 10/15/93 ! Added support to new BOM Markers 'BP'    ! JDH *~
            *          !   which has costs subtracted from roll-up!     *~
            * 04/04/97 ! Change PUTPARM call for NT Compatibility ! LDJ *~
	    CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            assypartdesc$32,             /* Assembly description       */~
            assykey$50,                  /* File Key                   */~
            compkey$50,                  /* File Key                   */~
            b$(490)3,                    /* All effective BOM's        */~
            bkid$(12)10,                 /* Cost Bucket ID's           */~
            bkdd$(12)20,                 /* Cost Bucket Descriptions   */~
            bom$3,                       /* Which BOM is effective     */~
            bom2$3,                      /* Component BOMID            */~
            bomid$(20)3,                 /* BOMID from input screen    */~
            bomcost$10,                  /* Top Level BOM Cost         */~
            bomtitl$45,                  /*                            */~
            bomprt$9,                    /*                            */~
            component$25,                /* Component part number      */~
            compdescr$32,                /* Component description      */~
            count%(20),                  /* Indent indexes this level  */~
            cset$8,                      /* Current Cost Set ID        */~
            csetdescr$30,                /* Current Cost Set Descr     */~
            cursor%(2),                  /* Cursor location for edits  */~
            cstpart$25,                  /* Part Number for costing    */~
            date$8,                      /* Screen display date        */~
            edtmessage$79,               /* Edit message               */~
            edttran$80,                  /* Translation string for edit*/~
            errormsg$79,                 /* Error message text info    */~
            extcost$10,                  /* Extended Cost for print    */~
            fac$(21,2)1,                 /* Field attribute characters */~
            hdr$(2)79,                   /* Header for PLOWCODE        */~
            hdrdescr$60,                 /* Comp Name Line             */~
            cst_bom(12), cst_rte(12),    /* costs                      */~
            cst_msc(12), cst_tot(12),    /* costs                      */~
            i$(24)80,                    /* Screen image (not used)    */~
            infomsg$79,                  /* Informative message text   */~
            inpmessage$79,               /* Input message text info    */~
            keeper(20,10),               /* Holding area               */~
            keys$17,                     /* Active PF Keys             */~
            lfac$(24)1,                  /* Field attribute characters */~
            line2$79,                    /* Screen line #2             */~
            misccost$10,                 /* Top Level MISC Cost        */~
            mrkr$2,                      /* Component Marker           */~
            p$(20)31,                    /* Next BOM rec each level    */~
            assypart$(20)25,             /* Part numbers for multiple  */~
            part$25,                     /* Part number                */~
            part22$25,                   /* Part numbers for multiple  */~
            partkey$50,                  /* File key                   */~
            pf4$25,                      /* PF4 Literal                */~
            plowkey$50,                  /* Key for PLOWCODE           */~
            prtpart$132,                 /* Part number/descr indented */~
            readkey$100,                 /* Key to process in reading  */~
            rtecost$10,                  /* Top Level RTE Cost         */~
            scrnttl1$25,                 /* Column header for screen   */~
            scrnttl2$5,                  /* Column header for screen   */~
            set$8,                       /* Calling set ID             */~
            setid$4,                     /* Cost Set ID                */~
            setdescr$30,                 /* Cost Set description       */~
            smflag$1,                    /* Single - Multi level flag  */~
            time$8,                      /* Report Run Time            */~
            ttlqty$10,                   /* Total Quantity for print   */~
            totalcost$10,                /* Top Level Total Cost       */~
            uom$4,                       /* Stocking Unit of Measure   */~
            ucost$10,                    /* Part unit cost for print   */~
            varstd$10,                   /* Variance line standard     */~
            varsum$10,                   /* Variance line summation    */~
            varnce$10,                   /* Variance line variance     */~
            tod$8                        /* first day of plan period   */

        dim f1%(64),                     /* Record-on-file flags       */~
            f2%(64)                      /* File open flag             */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 4 ! HNYMASTR ! Inventory master file                    *~
            * # 5 ! BOMMASTR ! Bill of materials relationship file      *~
            * # 6 ! STCBOMXF ! Standard Cost Set / BOM-RTE X-Ref        *~
            * #10 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * #24 ! ENGMASTR ! Engineering master file                  *~
            * #34 ! SYSFILE2 ! System file for months open              *~
            *************************************************************

            select  #4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select # 5, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select  #6, "STCBOMXF",                                      ~
                        varc, indexed, recsize = 72,                     ~
                        keypos = 29, keylen = 33,                        ~
                        alt key 1, keypos =  1, keylen = 36,             ~
                            key 2, keypos = 37, keylen = 36

            select #10, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 25

            select #34, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

           select #24, "ENGMASTR" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29


            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 4, 0%, f2%( 4), 0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5), 0%, " ")
            call "OPENCHCK" (# 6, 0%, f2%( 6), 0%, " ")
            call "OPENCHCK" (#24, 0%, f2%(24), 0%, " ")
            call "OPENCHCK" (#34, 0%, f2%(34), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES FIELDS NECESSARY FOR PROCESSING THE REPORT.   *~
            *************************************************************

            date$ = date
            call "READ100" (#34, "MONTHS OPEN         ", f1%(34))
                     if f1%(34) <> 1% then goto L09140
            get #34, using L09100, tod$
L09100:     FMT XX(32), CH(6)
            call "DATE" addr ("G-", tod$, date$, today%, r%)
            today% = today% + 1%
            goto L09170
L09140:     today% = 1%
            r% = r%

L09170:     call "DATEFMT" (date$)
*       * Set up strings for edit translation.
            init(hex(00)) str(edttran$,  1, 80)
            init(hex(01)) str(edttran$,  1, 33)
            init(hex(02)) str(edttran$, 33,  8)
            init(hex(01)) str(edttran$, 41, 27)
            init(hex(02)) str(edttran$, 68, 13)

            scrnttl1$ = "Assembly Number"  :  scrnttl2$ = "BOMID"
            hdr$(1) = "  BOMID      BOM Description"
            hdr$(2) = "  "

            str(line2$,62) = "STCBOMMT: " & str(cms2v$,,8)
            call "COMPNAME" (12%, hdrdescr$, buckets%)
            edtmessage$ = "To EDIT, Position cursor to appropriate field ~
        ~and press RETURN"

*        Get Cost Bucket Descriptors
            buckets% = 3%
            cset$ =  "        "
            call "STCSETID" (buckets%, #34, cset$, setid$,               ~
                             bkid$(), bkdd$(), csetdescr$)
            if buckets% > 0% then L10000
                setid$ = " " : csetdescr$ = "NO CURRENT COST SET"

L10000: REM *************************************************************~
            *               I N P U T M O D E                           *~
            *-----------------------------------------------------------*~
            * Input for Assembly Numbers and BOMID's                    *~
            *************************************************************

        inputmode
            init(" ") assypart$(), bomid$(), inpmessage$, errormsg$,     ~
                      smflag$, infomsg$
            set$ = cset$ : setdescr$ = csetdescr$ : edit% = 0%
            pf16$ = "(16)Exit Program" : pf14$ = " "
            for fieldnr% = 1 to 2
                gosub'161(fieldnr%)
                    if enabled% =  0 then L10107
L10093:         gosub'201(fieldnr%)
                    if keyhit%  =  1 then gosub startover
                    if keyhit%  = 16 and fieldnr% = 1% then end_program
                    if keyhit% <>  0 then L10093
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10093
                pf16$ = " "
L10107:         next fieldnr%

            for screenline% = 1 to 20
                c% = screenline%
                for fieldnr% = 1 to 2
                    gosub'163(fieldnr%)
                          if enabled% = 0 then L10270
L10190:             gosub'203(fieldnr%)
                    if keyhit%  =  1 then gosub startover
                    if keyhit%  <> 4 then L10220
                         errormsg$ = " "
                         fieldnr% = 1%
                         gosub'163(fieldnr%)
                         if enabled% = 1% then L10190
L10220:             if keyhit% <>  0 then       L10190
                    gosub'153(fieldnr%)
                          if errormsg$ <> " " then L10190
                next fieldnr%
                if assypart$(c%) <> " " then maxpart% = c%
L10270:     next screenline%

        REM *************************************************************~
            *                  E D I T M O D E                          *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, infomsg$
            edit% = 1%

            pf16$ = "(16)Print BOMs" : pf14$ = "(14)View BOMs"
L11200:     gosub'201(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then L30000
                  if keyhit%  = 16% then L30000
                  if keyhit% <>  0% then L11200
            if cursor%(1%) >  9% then L11480        /* edit table data */
               fieldnr% = cursor%(1%) - 5%
               if fieldnr% < 1% or fieldnr% > 2% then L11200
                  gosub'161(fieldnr%)
                        if enabled% =  0% then L11200
L11340:           gosub'201(fieldnr%)
                        if keyhit%  =  1% then gosub startover
                        if keyhit% <>  0% then L11340
                  gosub'151(fieldnr%)
                        if errormsg$  <> " " then L11340
                  goto L11200

L11480
*       * Edit tabular data fields.
            if cursor%(1) < 10 or cursor%(1) > 19 then L11200
            fieldnr% = val(str(edttran$, cursor%(2)))
            if fieldnr% < 1% then L11200
            c%, screenline% =                                            ~
                (2% * (cursor%(1) - 10%) + 1%) + int(cursor%(2)/41%)
            c%, screenline% =  min(maxpart% + 1%, c%)
            if fieldnr% = 2% and assypart$(c%) = " " then L11200

            gosub'163(fieldnr%)
                if enabled% = 0 then L11200
L11660:     gosub'203(fieldnr%)
                if keyhit%  =  1 then gosub startover
                if keyhit%  <> 4 then L11800
                     errormsg$ = " "
                     fieldnr% = 1%
                     gosub'163(fieldnr%)
                     if enabled% = 1% then L11660
L11800:         if keyhit% <>  0 then L11660
            gosub'153(fieldnr%)
                if errormsg$ <> " " then L11660
                if assypart$(c%) <> " " then maxpart% = max(maxpart%, c%)

            goto L11200

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   S E C T I O N          *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables input for the part number screen*~
            * a field is disabled if the previous part field is blank.  *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1
                  inpmessage$ = " "
                  on fieldnr% gosub L20042,         /* Cost Set?        */~
                                    L20056          /* Sgl/Multi?       */
                  return

L20042: REM Default/Enable for Cost Set To use
                inpmessage$ = "Use the Current Cost Set as shown, " &    ~
                              "or select another"
                return

L20056: REM Default/Enable for single or multi level listings
                smflag$ = "S"
                inpmessage$ = "Enter 'S' to print single level or "  &   ~
                              "'M' to print multi level "
                return

            deffn'163(fieldnr%)
                  enabled% = 0
                  inpmessage$ = " "
                  on fieldnr% gosub L20130,         /* Part number      */~
                                    L20180          /* BOM ID           */
                     return

L20130
*       * Default/enable for part number
            if screenline% < 2 then L20145
            if assypart$(screenline%-1) = " " then return
L20145:         enabled% = 1%
                inpmessage$ = "Enter Assembly Number or Leave Blank "  & ~
                              "to end input."
                return

L20180
*       * Default/enable for BOMID
            if assypart$(screenline%) = " " then return
                enabled% = 1%
                inpmessage$ = "Enter Specific BOM ID, '?' to see list" & ~
                              " or blank to use BOM currently effective"
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM Allow user opportunity to start over.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *   P R I N T   P R I C E D   B I L L S                     *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

            call "SHOSTAT" ("Generating Costed Bill(s) of Materials")
            select printer(134)
            call "SETPRNT" ("BOM003", " ", 0%, 0%)
            if keyhit% = 14% then call "SET" addr("PM", "K")
            time$ = " " : call "TIME" (time$)

            for p% = 1 to 20
                if str(assypart$(p%),1,25) = " " then L30620
                mat keeper = zer
                page%, l% = 0
                line% = 999%
                call "DESCRIBE" (#4, assypart$(p%), assypartdesc$,       ~
                                     0%, f1%(4))
                uom$=" " : if f1%(4) <> 0% then get #4 using L30303, uom$
L30303:              FMT POS(74), CH(4)
                if bomid$(p%) <> "   " then L30380
                     part22$ = assypart$(p%) : gosub get_bomid
                     bomid$(p%) = bom$
L30380:         bom$ = bomid$(p%)
                bomtitl$ = assypart$(p%) & " USING BOM: " & bomid$(p%)
                gosub'88(assypart$(p%))
                gosub'90(assypart$(p%))
                if used% = 1 then L30440
                     bomcst, rtecst, msccst = 0
L30440:         call "CONVERT" (bomcst, -4.4, bomcost$)
                call "CONVERT" (rtecst, -4.4, rtecost$)
                call "CONVERT" (msccst, -4.4, misccost$)
                totalcost = bomcst + rtecst + msccst
                call "CONVERT" (totalcost, -4.4, totalcost$)
                keeper(1,1) = bomcst : keeper(1,3) = 1
                assykey$ = str(assypart$(p%),1,25) & str(bomid$(p%),1,3)
                gosub'7(assykey$)
                print skip(1)
                print using L38920
L30620:     next p%
            time$ = " " : call "TIME" (time$)
            print : print using L38965, time$
            if keyhit% = 14% then call "GETPRTNM" addr(file$, lib$, vol$)
            close printer
            call "SETPRNT" ("BOM003", " ", 0%, 1%)
            if keyhit% <> 14% then inputmode

           REM Display printed file...
           close ws
           call "PUTPARM" addr("E", "INPUT   ",4%,                       ~
                    "FILE    ", file$, 8%, "LIBRARY ", lib$, 8%,         ~
                "VOLUME  ", vol$, 6%, "ACCESS  ", "PRINT ", 6%, "@", ret%)
           call"LINK"addr("DISPLAY ","S"," "," ",0%," "," ", 0%,"N", 0%, ~
			ret%)
           call "SCRATCH" addr("F", file$, lib$, vol$, " ", " ", 0%)
           goto inputmode

            deffn'7(partkey$)
                if smflag$ = "S" then single_level
                l% = l% + 1
                p$(l%) = partkey$

                count%(l%) = 0         /* Sequence number for print  */
L30780:         call "PLOWNEXT" (#5, p$(l%), 28%, f1%(5))
                    if f1%(5) = 0 then end_level
                if str(p$(l%),29,3) <> "  0" then L30900
                     gosub'88(p$(l%))
                     gosub'90(p$(l%))
                     if used% = 0 then bomcst = 0
                     keeper(l%,1) = bomcst     /* find me a home */
                     goto L30780
L30900:         gosub L39000            /* Load bom record & info.    */
                if skip_it% = 1% then L30780

*       * Check to see if this part has components or not
                if bom2$ <> " " then L31020
                     part22$ = component$ : gosub get_bomid
                     bom2$ = bom$
L31020:         bom$ = bom2$
                compkey$ = str(component$,1,25) & str(bom$,1,3)
L31060:         call "PLOWNEXT" (#5, compkey$, 28%, f1%(5))
                if str(compkey$,29,3) = "  0" then L31060
                keeper(l%,3) = ttlqty
                count%(l%) = count%(l%) + 1
                gosub process_line
                goto L30780

        end_level   /* Step back one level */
            if l% = 1 then L31300
               keeper(l%,1) = round(keeper(l%,1) * keeper(l%-1,3), 4)
L31300:     if round(keeper(l%,1),4) = round(keeper(l%,2),4) then L31480
            bomvar = keeper(l%,2) - keeper(l%,1)
            call "CONVERT" (keeper(l%,1), -4.4, varstd$)
            call "CONVERT" (keeper(l%,2), -4.4, varsum$)
            call "CONVERT" (bomvar, -4.4, varnce$)
            print using L38840, p$(l%), varstd$, varsum$, varnce$
            line% = line% + 1%
            if bomvar <> 0 then keeper(1,2) = keeper(1,2) + bomvar

L31480:     keeper(l%,1), keeper(l%,2), keeper(l%,3) = 0
            print skip(1)
            line% = line% + 1%
            l% = l% - 1
            return

        single_level
            plowkey$ = str(assypart$(p%),1,25) & str(bomid$(p%),1,3)
            l% = 1
            p$(l%) = assypart$(p%)
            count%(l%) = 0         /* Sequence number for print  */
L32050:     call "PLOWNEXT" (#5, plowkey$, 28%, f1%(5))
                if f1%(5) = 0 then end_level
            if str(plowkey$,29,3) <> "  0" then L32130
                gosub'88(plowkey$)
                     gosub'90(plowkey$)
                     if used% = 0 then bomcst = 0
                     keeper(l%,1) = bomcst
                     goto L32050
L32130:         gosub L39000            /* Load bom record & info.    */
                if skip_it% = 1% then L32050
                if bom2$ <> " " then L32138
                     part22$ = component$ : gosub get_bomid
                     bom2$ = bom$
L32138:         bom$ = bom2$
                keeper(l%,3) = ttlqty
                count%(l%) = count%(l%) + 1
                gosub process_line
                goto L32050

        deffn'88(cstpart$)       /* get standard costs for assemblies */
            tot_cst, bomcst, rtecst, msccst = 0
            mat cst_bom = zer : mat cst_rte = zer
            mat cst_msc = zer : mat cst_tot = zer
            call "STCCOSTS" (cstpart$, set$, #34, 3%, tot_cst,           ~
                             cst_tot(), cst_bom(), cst_rte(), cst_msc())

            for k% = 1% to 12%
                bomcst = bomcst + cst_bom(k%)
                rtecst = rtecst + cst_rte(k%)
                msccst = msccst + cst_msc(k%)
                totcst = totcst + cst_tot(k%)
            next k%
            if l% = 0 then return
            if f1%(5) = 0 then bomcst = tot_cst

            return


        deffn'90(part$)   /* See if assy # with BOMID in cost set */
            readkey$ = str(part$,,25) & str(bom$,,3) & str(set$,,8)
            call "REDALT0" (#6, readkey$, 1%, used%)
            return

        process_line   /* Get it, format it, print it & then stack it */
            prtpart$ = " "
            if line% > 56% then gosub page_head
            gosub'88(component$)
            if mrkr$ = "BP" then L37041  /* Subtract cost if 'BP', */
              if ttlqty < 0 then L37142  /* otherwise don't.       */
L37041:     if bom$ = " " then L37044
                gosub'90(component$)
                if used% = 0 then tot_cst = 0
L37044:     if l% <> 1% then ttlqty = keeper(l%-1%,3) * ttlqty
            keeper(l%,3) = ttlqty
            if mrkr$ = "PH" then tot_cst = tot_cst - (rtecst + msccst)
            extcost = round(ttlqty * tot_cst, 4)
            keeper(l%,2) = keeper(l%,2) + extcost

*          IF MRKR$ = "PH" THEN KEEPER(L%, 2) = KEEPER(L%, 2) -         ~
*                               (TTLQTY * (RTECST + MSCCST))
            call "CONVERT" (ttlqty, 4.4, ttlqty$)
            call "CONVERT" (tot_cst, 4.4, ucost$)
            call "CONVERT" (extcost, 2.4, extcost$)
            bomprt$ = " "
            if bom$ <> " " then bomprt$ = ", BOM:" & bom$
            put str(prtpart$, (l%) * 3 - 2, 100), using L37220,           ~
                    count%(l%), component$ & " / " & compdescr$ &        ~
                    bomprt$, str(ttlqty$), str(uom$) & str(ucost$) &     ~
                    str(extcost$)
L37142:     if mrkr$ = "BP" then L37160
            if ttlqty < 0 then put str(prtpart$, (l%) * 3 - 2, 100),     ~
                using L37250, count%(l%), component$ & " / " & compdescr$,~
                       "BYPRODUCT not included in Cost"

L37160:     print using L38760, prtpart$
            line% = line% + 1%
            if f1%(5) = 0% then return
            if smflag$ = "S" then return
            if ttlqty < 0 then return
            gosub'7(str(component$,1,25) & str(bom$,1,3))
            return

L37220: %###. ###########################################################~
        ~ ########## ########################

L37250: %###. ###########################################################~
        ~# ##################################

        page_head
            page% = page% + 1
            print page
            print using L38380, date$, time$, hdrdescr$
            print using L38300, page%
            print using L38460, set$, setdescr$
            print skip(1)
            if keeper(1,1) <> 0 then print using L38520, bomtitl$,        ~
                                     bomcost$, rtecost$, misccost$       ~
                                else print using L38544, bomtitl$
            if keeper(1,1) <> 0 then print using L38600, assypartdesc$,   ~
                                     totalcost$                          ~
                                else print using L38580, assypartdesc$
            print skip(1)
            print using L38620
            print using L38680
            line% = 8%
            return

L38300: %                    B I L L   O F   M A T E R I A L S   C O S T ~
        ~E D   A T   C U R R E N T   S T A N D A R D             PAGE:  ##~
        ~##

L38380: %RUN: ######## ########            ##############################~
        ~##############################                      STCBOMMT:BOM0~
        ~08

L38460: %                                       COST SET SELECTED IS: ###~
        ~##### #############################

L38520: %ASSEMBLY NUMBER: ##########################################  BOM~
        ~ COST = ########## RTE COST = ########## MISC COST = ##########

L38544: %ASSEMBLY NUMBER: ##########################################  ***~
        ~** NO STANDARD COSTS IN THE SELECTED COST SET *****

L38580: %                  ################################

L38600: %                  ################################              ~
        ~                                        TOTAL COST = ##########

L38620: %PART NUMBER / DESCRIPTION                                       ~
        ~  QTY REQ'D UOM  UNIT COST EXTENSION

L38680: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---

L38760: %################################################################~
        ~#################################################################~
        ~###

L38840: %***** MATERIAL COST VARIANCE FOR  ######################### BOM ~
        ~COST IS ########## CALCULATED IS ########## VARIANCE IS #########~
        ~#

L38920: %              *********   E N D   O F   S T A N D A R D   C O S ~
        ~T E D   B I L L   O F   M A T E R I A L S   *********

L38965: %                        *********   E N D   O F   R E P O R T  (~
        ~@ ########)   *********

L39000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *-----------------------------------------------------------*~
            * Loads the bill of materials record from the file          *~
            *************************************************************

            get #5, using L39080, component$, quantity, xused, over,      ~
                                 mrkr$, bom2$

L39080:     FMT          CH(25),         /* Component part number      */~
                POS(57), 2*PD(14,4),     /* Qty & Times used           */~
                POS(81), PD(14,4),       /* Overage                    */~
                POS(89), CH(2),          /* Component Marker           */~
                POS(92), CH(3)           /* Hard Pegged BOMID          */

            if mrkr$ = "RE" or mrkr$ = "TL" or mrkr$ = "NC" or           ~
               mrkr$ = "NP" then skip_it% = 1% else skip_it% = 0%
            if skip_it% = 1% then return

            ttlqty = quantity * xused + over
            call "DESCRIBE" (#4, component$, compdescr$, 0%, f1%(4))
                if f1%(4) = 0 then compdescr$ = "NOT ON FILE"
                uom$=" " : if f1%(4) <> 0% then get #4 using L30303, uom$
            return

*       * Find effective bill structure for the part passed in
        get_bomid
           bom$ = " "
           if mrkr$ = "BP" then return  /* By-Prods don't explode */
           call "READ100" (#24, str(part22$,,25) & "1", f1%(24))
                if f1%(24) <> 1% then return
           get #24, using L39250, b$()
L39250:    FMT XX(29), 490*CH(3)
           if b$(today%) <> " " then bom$ = b$(today%)
        return


        REM *************************************************************~
            *         I N P U T  /  E D I T   S C R E E N               *~
            *-----------------------------------------------------------*~
            * Input / Edit all data.                                    *~
            *************************************************************

            deffn'201(fieldnr%)
                  keys$ = hex(01ffffffffffffffffffffff0d0e0f1000)
                  pf4$ = " "
                  init(hex(84)) lfac$(), fac$()
                  if fieldnr% <> 0% then L40110
                  if edit% = 1% then inpmessage$ = edtmessage$
                  init(hex(86)) lfac$(), str(fac$(),1,2%*maxpart%+1%)
L40110:           on fieldnr% gosub L40190,         /* Set to use?      */~
                                    L40190          /* single / multi?  */
                  goto L41210

L40190:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

            deffn'203(fieldnr%)
                  keys$ = hex(01ffffffffffffffffffffff0d0e0f1000)
                  init(hex(84)) fac$(), lfac$()
                     if fieldnr% = 2% then str(keys$,4,1) = hex(04)      ~
                                      else str(keys$,4,1) = hex(ff)
                     if fieldnr% = 1% then pf4$ = " " else               ~
                                      pf4$ = "(4)Back to Assy Number"
                  if fieldnr% <> 0% then L41100
                  if edit% = 1% then inpmessage$ = edtmessage$
                  init(hex(86)) lfac$(), str(fac$(), 1, 2%*maxpart%+1%)
L41100:           on fieldnr% gosub L41140,         /* Part number      */~
                                    L41140          /* BOMID            */
                     goto L41210

L41140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L41210:     accept                                                       ~
               at (01,02), "Print Standard Costed Bills of Materials",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$,                   ch(08),~
               at (02,02), fac(hex(ac)), line2$,                  ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Cost Set to use for costing?",               ~
               at (06,33), fac(lfac$(1)), set$                  , ch(08),~
               at (06,45), fac(hex(8c)),  setdescr$             , ch(30),~
                                                                         ~
               at (07,02), "Single or Multi Level?",                     ~
               at (07,33), fac(lfac$(2)), smflag$               , ch(01),~
                                                                         ~
               at (09,06), fac(hex(ac)), scrnttl1$              , ch(25),~
               at (09,33), fac(hex(ac)), scrnttl2$              , ch(05),~
               at (09,41), fac(hex(ac)), scrnttl1$              , ch(25),~
               at (09,68), fac(hex(ac)), scrnttl2$              , ch(05),~
                                                                         ~
               at (10,06), fac(fac$( 1,1)), assypart$( 1)       , ch(25),~
               at (10,33), fac(fac$( 1,2)), bomid$   ( 1)       , ch(03),~
               at (10,41), fac(fac$( 2,1)), assypart$( 2)       , ch(25),~
               at (10,68), fac(fac$( 2,2)), bomid$   ( 2)       , ch(03),~
               at (11,06), fac(fac$( 3,1)), assypart$( 3)       , ch(25),~
               at (11,33), fac(fac$( 3,2)), bomid$   ( 3)       , ch(03),~
               at (11,41), fac(fac$( 4,1)), assypart$( 4)       , ch(25),~
               at (11,68), fac(fac$( 4,2)), bomid$   ( 4)       , ch(03),~
               at (12,06), fac(fac$( 5,1)), assypart$( 5)       , ch(25),~
               at (12,33), fac(fac$( 5,2)), bomid$   ( 5)       , ch(03),~
               at (12,41), fac(fac$( 6,1)), assypart$( 6)       , ch(25),~
               at (12,68), fac(fac$( 6,2)), bomid$   ( 6)       , ch(03),~
               at (13,06), fac(fac$( 7,1)), assypart$( 7)       , ch(25),~
               at (13,33), fac(fac$( 7,2)), bomid$   ( 7)       , ch(03),~
               at (13,41), fac(fac$( 8,1)), assypart$( 8)       , ch(25),~
               at (13,68), fac(fac$( 8,2)), bomid$   ( 8)       , ch(03),~
               at (14,06), fac(fac$( 9,1)), assypart$( 9)       , ch(25),~
               at (14,33), fac(fac$( 9,2)), bomid$   ( 9)       , ch(03),~
               at (14,41), fac(fac$(10,1)), assypart$(10)       , ch(25),~
               at (14,68), fac(fac$(10,2)), bomid$   (10)       , ch(03),~
               at (15,06), fac(fac$(11,1)), assypart$(11)       , ch(25),~
               at (15,33), fac(fac$(11,2)), bomid$   (11)       , ch(03),~
               at (15,41), fac(fac$(12,1)), assypart$(12)       , ch(25),~
               at (15,68), fac(fac$(12,2)), bomid$   (12)       , ch(03),~
               at (16,06), fac(fac$(13,1)), assypart$(13)       , ch(25),~
               at (16,33), fac(fac$(13,2)), bomid$   (13)       , ch(03),~
               at (16,41), fac(fac$(14,1)), assypart$(14)       , ch(25),~
               at (16,68), fac(fac$(14,2)), bomid$   (14)       , ch(03),~
               at (17,06), fac(fac$(15,1)), assypart$(15)       , ch(25),~
               at (17,33), fac(fac$(15,2)), bomid$   (15)       , ch(03),~
               at (17,41), fac(fac$(16,1)), assypart$(16)       , ch(25),~
               at (17,68), fac(fac$(16,2)), bomid$   (16)       , ch(03),~
               at (18,06), fac(fac$(17,1)), assypart$(17)       , ch(25),~
               at (18,33), fac(fac$(17,2)), bomid$   (17)       , ch(03),~
               at (18,41), fac(fac$(18,1)), assypart$(18)       , ch(25),~
               at (18,68), fac(fac$(18,2)), bomid$   (18)       , ch(03),~
               at (19,06), fac(fac$(19,1)), assypart$(19)       , ch(25),~
               at (19,33), fac(fac$(19,2)), bomid$   (19)       , ch(03),~
               at (19,41), fac(fac$(20,1)), assypart$(20)       , ch(25),~
               at (19,68), fac(fac$(20,2)), bomid$   (20)       , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,25), fac(hex(8c)), pf4$,                    ch(25),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,50), fac(hex(8c)), pf14$,                   ch(13),~
               at (24,65), fac(hex(8c)), pf16$,                   ch(16),~
                                                                         ~
               keys(keys$),                                              ~
               key (keyhit%)

               if keyhit% <> 13% then L41880
                  call "MANUAL" ("STCBOMMT")
                  goto L41210

L41880:        if keyhit% <> 15% then L41920
                  call "PRNTSCRN"
                  goto L41210

L41920:        REM GET CURSOR LOCATION FOR EDIT MODE COMPUTATIONS.
                   close ws
                   call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                   return


        REM *************************************************************~
            *    T E S T   D A T A   F O R   L I N E A R   I N P U T    *~
            *-----------------------------------------------------------*~
            * Tests data for linear input mode.                         *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50130,         /* Cost Set?        */~
                                    L50500          /* Single / Multi?  */
                  return

L50130: REM Test data for Cost Set to use
            plowkey$   = "STC.HDR." & set$
            setdescr$ = hex(06) & "Select Cost Set"
            call "PLOWCODE" (#34, plowkey$, setdescr$, 8%, 0.30, onfile%)
            if onfile% = 1% then L50240
                errormsg$ = "Please select a valid Cost Set"
                return
L50240:     set$ = str(plowkey$,9)
            call "STCFOPEN" (set$, "S     ", #34, errormsg$,             ~
                             #10, #10, #10, #10, #10, #10)
            return

L50500: REM Test data for single or multi level explosion
            if smflag$ = "S" or smflag$ = "M" then return
            errormsg$ = "Must enter either 'S' or 'M'"
            return

        REM *************************************************************~
            *      T E S T   T A B U L A R   I N F O R M A T I O N      *~
            *-----------------------------------------------------------*~
            * Tests tabular information to make sure that the data is   *~
            * OK.  Zap quantity if blank part number entered.           *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51120,         /* Part number      */~
                                    L51300          /* BOMID            */
                     return
L51120: REM Test data for part number
            if assypart$(c%) <> " " then L51170
            if assypart$(  )  = " " then L51282
                bomid$(c%) = " "
                goto L51490

L51170:     call "GETCODE" (#4, assypart$(c%), assypartdesc$,            ~
                                               0%, 0, f1%(4))
                if f1%(4) = 0 then L51270
            readkey$ = str(assypart$(c%),1,25) & " "
            call "PLOWNEXT" (#5, readkey$, 25%, f1%(5))
                if f1%(5) = 0 then L51250
                if bomid$(c%) = " " then return
                gosub L51360
                if errormsg$ = " " then return

                fac$(c%,1) = hex(86) : fac$(c%,2) = hex(82)
                fieldnr% = 2%
            return

L51250:    errormsg$ = "Part Has No Bill of Materials: " & assypart$(c%)
                return
L51270:    errormsg$ = "Assembly Number Not On File: " & assypart$(c%)
                return
L51282:    errormsg$ = "You must enter at least one Assembly Number"
                return

L51300: REM Test data for BOMID$
            if assypart$(c%) = " " then L51430
            if bomid$(c%) <> " " or bomid$(c%) = "?" then L51360
                infomsg$ = " "
                part22$ = assypart$(c%) : gosub get_bomid
                if f1%(24) = 1 then L51350
                     infomsg$ = "  There is NO effective BOM!"
                     goto L51360
L51350:         bomid$(c%) = bom$
                return
L51360:     if bomid$(c%) = "?" then bomid$(c%) = "   "
            readkey$ = str(assypart$(c%),,25) & str(bomid$(c%))
            infomsg$ = hex(06) & "Valid BOM's for assy " &               ~
                       assypart$(c%) & " are shown." & infomsg$
            call "PLOWCODE" (#5, readkey$, infomsg$, 2025%, 0.30,        ~
                             f1%(5), hdr$(), 3.0)
                if f1%(5) = 0 then L51410
                bomid$(c%) = str(readkey$,26,3)
                return

L51410:     errormsg$ = "BOM Specified is NOT on File"
                return
L51430:     errormsg$ = "Cannot enter BOMID if Assembly Number is Blank"
                return
            errormsg$ = "There is NO EFFECTIVE BOM on file, Please " &   ~
                        "Enter a specific BOM ID"
                return

L51490
*       * Clean up stack
            if c% > maxpart% then return
            if c% = maxpart% then L51580
            if maxpart% < 2% then L51580
            for i% = c% to maxpart% - 1%
                assypart$(i%) = assypart$(i%+1%)
                bomid$(i%) = bomid$(i%+1%)
            next i%

L51580:     maxpart% = max(maxpart% - 1%, 0%)
            init (" ") assypart$(maxpart%+1%), bomid$(maxpart%+1%)
            return

        REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************
        end_program
            call "SHOSTAT" ("One Moment Please")
            end
