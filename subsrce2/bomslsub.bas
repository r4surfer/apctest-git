        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    OOO   M   M   SSS   L       SSS   U   U  BBBB    *~
            *  B   B  O   O  MM MM  S      L      S      U   U  B   B   *~
            *  BBBB   O   O  M M M   SSS   L       SSS   U   U  BBBB    *~
            *  B   B  O   O  M   M      S  L          S  U   U  B   B   *~
            *  BBBB    OOO   M   M   SSS   LLLLL   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMSLSUB - Prints single level Parts list using specified *~
            *             BOM ID or Effectivity Date.                   *~
            *            If both a specific BOM ID & Effectivity date   *~
            *            are passed, the BOM ID is looked for first, if *~
            *            not found then the effective BOM is printed.   *~
            *            Passes back the plowkey for the next part or   *~
                         next BOM (if effective date = 'ALL'). This key *~
            *            is used by BOMSLPRT to pass back to here, if OK*~
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
            * 04/28/87 ! ORIGINAL (Carved from BOMSLPRT)          ! MJB *~
            * 08/12/87 ! Batch quantity logic installed.          ! JIM *~
            * 03/09/88 ! All print & lookup logic from BOMSLPRT.  ! RJM *~
            *          !  Sub now duel purpose, called by BOMSLPRT!  "  *~
            *          !  and JBPICKCR.  Standard report headings.!  "  *~
            * 04/18/88 ! Did not work with alpha BOM ID's. Now OK ! RJM *~
            * 04/29/88 ! Fixed printing of reference locations    ! RJM *~
            * 04/19/89 ! Added ARG to print Part Text, optionally ! RJM *~
            * 04/08/91 ! (PRR 11708) Fixed printing of Effective  ! RJB *~
            *          !      Dates in Heading when Sorting by PN !     *~
            * 03/25/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 07/12/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "BOMSLSUB" (assypnin$,                                       ~
                        bomidin$,                                        ~
                        prttxt$,   /* Print Bom Text ?          */       ~
                        hnytxt$,   /* Print Part Text ?         */       ~
                        prtref$,   /* Print Reference Locations */       ~
                        sortsw$,   /* Sort by Part Number ?     */       ~
                        #1,                                              ~
                        #2,                                              ~
                        #3,                                              ~
                        #4,                                              ~
                        #11,                                             ~
                        #34,                                             ~
                        retkey$, /* Returns Next Assy Key, for BOMSLPRT*/~
                        effdate$,   /* BOM Effectivity Date */           ~
                        call_flag$, /* Set to '1' if BOMSLPRT called */  ~
                        jobcode$)

        dim                                                              ~
            assydescr$32,                /* Assy Part Description      */~
            assypnin$25,                 /* Assy Part Number           */~
            batch$8,                     /* Edited Batch quantity      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom$(490)3,                  /* Effective Bom array        */~
            bomid$3,                     /* Bom ID                     */~
            bomidin$3,                   /* Bom ID PASSED IN           */~
            bomidtest$3,                 /* BOM ID FOR TESTING         */~
            bomkey$31,                   /* Readkey for BOMMASTR       */~
            bommkr$2,                    /* Bom Marker                 */~
            call_flag$1,                 /* Indicates Calling Program  */~
            company$60,                  /* COMPANY NAME               */~
            compdescr$32,                /* Cpmponent part Description */~
            component$25,                /* Component part number      */~
            curr_part$25,                /* CURRENT PART NUMBER  temp  */~
            date$8,                      /* Date for screen display    */~
            dsize$1,                     /* Drawing Size               */~
            effdate$6,                   /* BOM Effectivity Date       */~
            effdatef$6,                  /* BOM EFFECTIVITY DATE       */~
            effdates$(10,2)8,            /* BOM Effective Dates (all)  */~
            find%(1),                    /* USED FOR A SEARCH          */~
            headline$50,                 /* Header Line for Text Page  */~
            hnytextid$4,                 /* PART TEXT ID               */~
            hnytxt$1,                    /* Print Part Text ?          */~
            jobcode$8,                   /* JOB CODE, printed if passed*/~
            line$130,                    /* PRINT LINE FOR HEADER      */~
            loc$(1000)12,                /* Location Designators       */~
            parent$25,                   /* Parent part number         */~
            parenttype$3,                /* Part Type of Parent        */~
            parttype$3,                  /* Part Type                  */~
            pldate$6,                    /* START OF PLANNING CALENDAR */~
            print$(5)8,                  /* Numeric Print Fields       */~
            prtref$1,                    /* Print Reference Locations? */~
            prttxt$1,                    /* Print Bom Text ?           */~
            readkey$60,                  /* Work Key for Reads         */~
            retkey$31,                   /* Plowkey Passed back        */~
            seqno$3,                     /* Bom Structure Sequence No. */~
            sortsw$1,                    /* Sort by Part Number ?      */~
            srec$(300)98,                /* SORT RECORD                */~
            textidh$4,                   /* Bom Header Text ID         */~
            textidl$4,                   /* Bom Component Text ID      */~
            time$8,                      /* TIME OF REPORT             */~
            uom$4,                       /* Unit of Measure            */~
            usrid$3                      /* USER ID FOR REPORT         */


        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

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
            * #1  ! BOMMASTR ! BOM relationship file                    *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #3  ! TXTFILE  ! System Text File                         *~
            * #4  ! BOMREFER ! Location Reference File                  *~
            * #11 ! ENGMASTR ! Engineering Master File                  *~
            * #34 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            effdatef$ = effdate$
            bomidtest$ = bomidin$
            if called% = 1% then L09180   /* BYPASS INITS IF ALREADY SET */

            date$ = date
            call "DATEFMT" (date$)
            select printer(134)

            call "COMPNAME" (12%, company$, ret%)
            call "EXTRACT" addr ("ID", usrid$)
            call "TIME" (time$)

            call "READ100" (#34, "MONTHS OPEN", f1%(34))
                if f1%(34) = 0% then L65000
            get #34, using L09160, pldate$
L09160:         FMT XX(32), CH(6)

L09180:     init (" ") bom$(), bomid$, batch$, dsize$, line$, effdates$()
            bom_loaded%, dateindex%, page%, kkk% = 0%
            called% = 1%
            if effdate$ = "ALL" then all_flag% = 1% else all_flag% = 0%

        REM *************************************************************~
            *  G E N E R A T E   B I L L S   O F   M A T E R I A L S    *~
            *                                                           *~
            * Prints the selected single level Bills of Material        *~
            *************************************************************

            if all_flag% = 1% then L19080
            if bomidtest$ = " " then gosub L24000 /* LOAD EFFECTIVE BOM */
            if bomidtest$ = " " then no_bom
L19080:     init (hex(00)) bomkey$
L19090:     str(bomkey$, 1, 25) = assypnin$
            str(bomkey$,26,  3) = bomidtest$

            if sortsw$ <> "Y" then L19170
                gosub sorted_bom
                if max% = 0% and call_flag$ = "1" then no_bom
                if max% = 0% then L65000
                goto L19530

        REM GET 1ST BOM RECORD
L19170:     call "PLOWNEXT" (#1, bomkey$, 25%, f1%(1))
            if f1%(1) = 1% then L19270
              if all_flag% = 1% then L19250
              if not( bomidtest$ <> " " and effdatef$ <> " " ~
                      and effdatef$ <> blankdate$ ) then L19250
                     gosub L24000 /* FIND EFF. BOM, IF BOM ID NOT FOUND */
                     if bomidtest$ <> " " then L19220
                          if call_flag$ = "1" then no_bom  else L65000
L19220:              effdatef$ = " "
                     goto L19090
L19250:         if call_flag$ = "1" then no_bom
                goto L65000
L19270:     if str(bomkey$,29,3) <> "  0" then L19300
                gosub get_header_textid
                goto L19170
L19300:     gosub get_line
            if bomid$ <> bomidtest$ and all_flag% <> 1% then no_bom

            call "READ100" (#2, parent$, f1%(2))
                if f1%(2) = 1% then L19380
                     assydescr$ = "*** PART NOT ON FILE ***"
                     parenttype$ = " "
                     goto L19410
L19380:     get #2, using L19390, assydescr$, hnytextid$, parenttype$
L19390:         FMT XX(25), CH(32), POS(98), CH(4), POS(180), CH(3)

L19410:     gosub L25000             /* GET LIST OF EFFECTIVITY DATES */
            curr_part$ = parent$
            if prttxt$ = "Y" then gosub header_text
            gosub page_head
            tm% = 1%
            if hnytxt$ = "Y" then gosub part_text
            gosub print_line

L19450:     call "PLOWNEXT" (#1, bomkey$, 28%, f1%(1))
            if f1%(1) = 0 then L19530
            if str(bomkey$,29,3) = "  0" then L19450
            get #1, using L35180, component$, seqno$, qty, xused,         ~
                                 fixed, overage, bommkr$, textidl$
            gosub print_line
            goto L19450

L19530: REM  END OF PARTS LIST ROUTINE
            print skip(2)
            print using L20130
            if call_flag$ <> "1" then L65000

            if all_flag% = 1% then L19570
            str(bomkey$,26,3) = "zzz"            /* Force to last BomID*/
L19570:     call "PLOWNEXT" (#1, bomkey$, 0%, f1%(1)) /* Next Assy */
                if f1%(1) = 0% then bomkey$ = " "
            goto L65000

        no_bom

            if call_flag$ = "1" then L19765  /* Only if BOMSLPRT called */

                call "READ100" (#2, assypnin$, f1%(2))
                    if f1%(2) = 1% then L19680
                         parent$ = assypnin$
                         assydescr$ = "*** PART NOT ON FILE ***"
                         parenttype$ = " "
                         goto L19710
L19680:         get #2, using L19690, parent$, assydescr$, parenttype$
L19690:             FMT CH(25), CH(32), POS(180), CH(3)

L19710:         gosub page_head
                print skip(2)
                print using L20190
                print skip(2)
                print using L20130
                goto L65000

L19765:     init (hex(00)) bomkey$
            str(bomkey$, 1, 25) = assypnin$
            str(bomkey$,26,  3) = "zzz"     /* Force to last Bom ID */
            call "PLOWNEXT" (#1, bomkey$, 0%, f1%(1)) /* Next Assy */
                if f1%(1) = 0% then bomkey$ = " "
            goto L65000

L19830: %BY: ###                                   S I N G L E   L E V E ~
        ~L   P A R T S   L I S T                                 PAGE: ###

L19860: %ASSEMBLY NUMBER: #########################   BOMID: ###  PART TY~
        ~PE: ###  BATCH SIZE: ######## DRAWING SIZE: #
L19880: %    DESCRIPTION: ################################

L19900:  %                                                               ~
        ~     BATCH                         BATCH                       PA~
        ~RT

L19940:  % SEQ  PART NUMBER               DESCRIPTION                    ~
        ~  QUANTITY    * USED  ADD OVER  TOTL QTY  FIXED/RUN  UOM   MKR TY~
        ~PE

L19980:  % ---  ------------------------- -------------------------------~
        ~- --------  --------  --------  --------  ---------  ----  --- --~
        ~--
L20010:  % ###  ######################### ###############################~
        ~# ########  ########  ########  ########   ########  ####   ##  #~
        ~##

L20050:   %SEQ. PART NUMBER               DESCRIPTION                    ~
        ~  QTY PER FIXED/RUN UOM  MKR TYP ASSEMBLY REFERENCE DESIGNATIONS
L20070:   %---- ------------------------- -------------------------------~
        ~- -------- -------- ---- --- --- --------------------------------~
        ~-----
L20100:   % ### ######################### ###############################~
        ~# ######## ######## ####  ## ### ################################~
        ~#####
L20130: %                                         ********** E N D   O F ~
        ~  R E P O R T **********

L20160: %BILL OF MATERIAL TEXT FOR ######################################~
        ~#############

L20190: %                      ******  NOTE:  NO BILL OF MATERIAL ID WAS ~
        ~SPECIFIED  ******

L20220: %RUN DATE: ######## @ ########     ##############################~
        ~##############################                      BOMSLPRT: BOM~
        ~001

L20300: %EFFECTIVE DATES: ###############################################~
        ~#################################################################~
        ~###

L20320: %                 ###############################################~
        ~#################################################################~
        ~###

L20360: %                                                  JOB ORDER NUMB~
        ~ER: ########

        REM *************************************************************~

        header_text
           if textidh$ = hex(20202020) or                                ~
              textidh$ = hex(00000000) or                                ~
              textidh$ = hex(ffffffff) then return
           stat% = 0%
L21060:    line% = 5%
           print page
           print skip(2)
           headline$ = "ASSEMBLY LEVEL TEXT FOR PART  " & parent$ & " BOM~
        ~ID " & bomid$
           print using L20160, headline$
           print skip(2)
           call "TXTPRINT" (#3, f3%, 134%, textidh$, "BOM001",  5%,      ~
                             line%, 56%, "Y", " ", stat%)
           if stat% = 0% then return
           goto L21060

        REM *************************************************************~

        part_text
           if hnytextid$ = hex(20202020) or                              ~
              hnytextid$ = hex(00000000) or                              ~
              hnytextid$ = hex(ffffffff) then return
           stat% = 0%
           saveline% = page% * 1000% + line%
L21580:    call "TXTPRINT" (#3, f3%, 134%, hnytextid$, "BOM001", tm%,    ~
                             line%, 56%, "Y", " ", stat%)
           if stat% = 0% then L21670
           gosub page_head
           headline$ = "Part Text For Part  " & curr_part$ & " Continued ~
        ~. . ."
           print using L20320, headline$
           print skip(1)
           line% = line% + 2%
           goto L21580

L21670:    if saveline% = page% * 1000% + line% then return
           print skip(1)
           line% = line% + 1%
           return

        REM *************************************************************~
            *        GENERATE BOM SORTED BY PART NUMBER                 *~
            *************************************************************

        sorted_bom

L22060:     init(hex(ff)) srec$()
            max% = 0%
L22070:     call "PLOWNEXT" (#1, bomkey$, 25%, f1%(1))
            if f1%(1) <> 0% then L22090
              if all_flag% = 1% then L22088
              if not( bomidtest$ <> " " and effdatef$ <> " " ~
                      and effdatef$ = blankdate$ ) then L22088
                     gosub L24000 /* FIND EFF. BOM, IF BOM ID NOT FOUND */
                     if bomidtest$ = " " then return

                     effdatef$ = " "
                     goto L19090
L22088:       return

L22090:     if str(bomkey$,29,3) <> "  0" then L22120
            gosub get_header_textid
            goto L22070
L22120:     gosub get_line
            if bomid$ <> bomidtest$ then return

L22160:     max% = max% + 1%
            put srec$(max%) using L35280, parent$, bomid$, component$,    ~
                            seqno$, qty, xused, fixed, overage, bommkr$, ~
                            textidl$, textidh$

            call "PLOWNEXT" (#1, bomkey$, 28%, f1%(1))
            if f1%(1) = 0 then L22270
            get #1, using L35180, component$, seqno$, qty, xused,         ~
                                 fixed, overage, bommkr$, textidl$
            goto L22160

L22270:     call "SORT" addr (srec$(), max%, 98%, srec$(), 1%, 56%, "A")
            page% = 0%
            get srec$(1%) using L35280, parent$, bomid$, component$,      ~
                          seqno$, qty, xused, fixed, overage, bommkr$,   ~
                          textidl$, textidh$

            call "READ100" (#2, parent$, f1%(2))
                if f1%(2) = 1% then L22324
                     assydescr$ = "*** PART NOT ON FILE ***"
                     parenttype$ = " "
                     goto L22329
L22324:     get #2, using L22326, assydescr$, hnytextid$, parenttype$
L22326:         FMT XX(25), CH(32), POS(98), CH(4), POS(180), CH(3)

L22329:     gosub L25000
            curr_part$ = parent$
            if prttxt$ = "Y" then gosub header_text
            gosub page_head
            tm% = 1%
            if hnytxt$ = "Y" then gosub part_text
            gosub print_line

            if max% < 2% then L22060
            for i% = 2% to max%
                get srec$(i%) using L35280, parent$, bomid$, component$,  ~
                          seqno$, qty, xused, fixed, overage, bommkr$,   ~
                          textidl$, textidh$
                gosub print_line
            next i%

            return        /* ALL DONE, FINISH REPORT & EXIT */

L23000: REM *************************************************************~
            *      G E T   E F F E C T I V E   B O M                    *~
            *                                                           *~
            * GETS THE CURRENT BOM FOR PASSED PART BASED ON DATEINDEX%  *~
            *************************************************************

                 if dateindex% = 0% then return  /* OFF PROD CALENDER? */
                   readkey$ = str(assypnin$,,25) & "1001"
                   bom_loaded% = 1%
                   call "READ100" (#11, readkey$, f1%(11%))
                      if f1%(11%) <> 1% then return
                   get #11, using L23245, readkey$, bom$()
L23245:               FMT CH(29), 490 * CH(3)
                   if str(readkey$,,25) <> str(assypnin$,,25) then return
                   if str(readkey$,26,1) <> "1" then return
                   bomidtest$ = bom$(dateindex%)

                   return

L24000: REM *************************************************************~
            *   G E T   E F F E C T I V I T Y   D A T E   I N D E X     *~
            *                                                           *~
            *************************************************************

            bomidtest$ = " "               /* Set for Error */
            call "PIPINDEX" (#34, effdatef$, dateindex%, ret%)
                if ret% <> 0% then return
            gosub L23000
            return

L25000: REM *************************************************************~
            *   L O A D   A L L   E F F E C T I V I T Y   D A T E S     *~
            * GETS ALL EFFECTIVE DATE RANGES FOR THE BOM                *~
            *************************************************************

            if str(bom$()) <> " " then L25090
                if bom_loaded% = 1% then return
                dateindex% = 1%
                gosub L23000         /* GET BOM EFFECTIVITY ARRAY */
L25090:     kkk% = 0%
            a% = 1%
L25110:     search str(bom$(),a%,) = str(bomid$,,) to find%() step 3
                if find%(1%) = 0% then return
            find%(1%) = find%(1%) + a% - 1%
            kkk% = kkk% + 1%
            z% = int(find%(1%) / 3%)
          call "DATE" addr ("G+", pldate$, z%, effdates$(kkk%,1%), ret%)
          call "DATEFMT" (effdates$(kkk%,1%))
            a% = find%(1%)
            search str(bom$(),a%,) <> str(bomid$,,) to find%() step 3
                if find%(1%) = 0% then find%(1%) = 1471%
            if find%(1%) <> 1471% then find%(1%) = find%(1%) + a% - 1%
            z% = int(find%(1%) / 3%) - 1%
          call "DATE" addr ("G+", pldate$, z%, effdates$(kkk%,2%), ret%)
          call "DATEFMT" (effdates$(kkk%,2%))
            a% = find%(1)
            if a% < 1468% and kkk% < 10% then L25110
            return

L29000: REM *************************************************************~
            * Formats and Prints the BOM Effectivity Dates              *~
            *************************************************************

            if kkk% > 0% then L29070
                print using L20300, "BOM NOT EFFECTIVE"
                return
L29070:     for i% = 1% to kkk%
                if i% <> 6% then L29110
                     print using L20300, line$
                     line$ = " "
L29110:         if i% < 6% then a% = i% else a% = i% - 5%
                put str(line$,(a%-1%)*22% + 1%,22) using L29130,          ~
                          effdates$(i%,1%), effdates$(i%,2%)
L29130:              % ######## -> ########
            next i%
            if kkk% < 6% then print using L20300, line$                   ~
                         else print using L20320, line$
            return

        REM *************************************************************~
            * Loads the data from the first read for the parent and the *~
            *   first part number.                                      *~
            *************************************************************

        get_line
            get #1, using L35060, component$, parent$, bomid$, seqno$,    ~
                    qty, xused, fixed, overage, bommkr$, textidl$
            str(bomkey$,1,25) = parent$
            str(bomkey$,26,3) = bomid$
        return


        get_header_textid
            get #1, using L30150, textidh$, dsize$, batch
L30150:         FMT POS(90), CH(4), CH(1), POS(107), PD(14,4)
            if batch < 0 or batch > 9e7 then batch = 1
            call "CONVERT" (batch, -0.4, batch$)
        return

        page_head    /* PAGE HEADER ROUTINE */
            print page
            page% = page% + 1%
            print using L20220, date$, time$, company$
            print using L19830, usrid$, page%
            if jobcode$ = " " then L30250
                print using L20360, jobcode$
                goto L30260
L30250:     print skip(1)
L30260:     print using L19860, parent$, bomid$, parenttype$, batch$,     ~
                               dsize$
            print using L19880, assydescr$
            if page% = 1% then gosub L29000
            print skip(1)
            if prtref$ <> "Y" then L30340
               print using L20050
               print using L20070
               line% = 9%
               goto L30380
L30340:     print using L19900
            print using L19940
            print using L19980
            line% = 10%
L30380:     print skip(1)
            if page% = 1% then line% = line% + kkk%/6% + 1%
            return

        print_line    /*  SET & PRINT DETAIL LINE     */
            if line% > 58 then gosub page_head
            ttlqty = qty * xused + overage
            call "READ100" (#2, component$, f1%(2))
                if f1%(2) = 1% then L30442
                     compdescr$ = "*** PART NOT ON FILE ***"
                     parttype$ = " "
                     goto L30470
L30442:     get #2, using L30444, compdescr$, uom$, hnytextid$, parttype$
L30444:         FMT XX(25), CH(32), XX(16), CH(4), POS(98), CH(4),       ~
                    POS(180), CH(3)

L30470:     if batch <= 1                                                ~
                then call "CONVERT" (batch * qty, 0.4, print$(1))        ~
                else call "CONVERT" (batch * qty * xused, 0.4, print$(1))
            print$(2) = " "
            if batch <= 1 then call "CONVERT" (xused, 0.4, print$(2))
            call "CONVERT" (batch * overage, 0.4, print$(3))
            call "CONVERT" (batch * ttlqty, 0.4, print$(4))
            call "CONVERT" (fixed, 0.4, print$(5))
            if prtref$ <> "Y" then L30920
               REM Print References in place of quantity breakdowns...

               temp% = 1%      /* Next Ref To Print */
               gosub load_refs
L30770:        readkey$ = " "
L30780:        if temp% > refs% then L30850
               if len(readkey$) + len(loc$(temp%)) + 1% > 37% then L30850
                  if readkey$ = " " then readkey$ = loc$(temp%)          ~
                         else readkey$ = readkey$ & "," & loc$(temp%)
                  temp% = temp% + 1%
                  goto L30780

L30850:        if line% > 58 then gosub page_head
               print using L20100, seqno$,component$,compdescr$,print$(4),~
                                  print$(5), uom$, bommkr$, parttype$,   ~
                                  readkey$

               line% = line% + 1%
               component$, compdescr$, print$(4),print$(5),bommkr$ = " "
               parttype$, uom$ = " "
               if temp% > refs% then L30970
               goto L30770

L30920:     print using L20010, seqno$, component$, compdescr$, print$(1),~
                               print$(2), print$(3), print$(4),          ~
                               print$(5), uom$, bommkr$, parttype$
            line% = line% + 1%

L30970:     if prttxt$ <> "Y" then L31090
            saveline% = page% * 1000% + line%
L30990:     call "TXTPRINT" (#3, f3%, 134%, textidl$, "BOM001",  9%,     ~
                             line%, 56%, "N", " ", stat%)
            if stat% = 0% then L31050
            gosub page_head
            goto L30990

L31050:     if saveline% = page% * 1000% + line% then L31090
            print skip(1)
            line% = line% + 1%

L31090:     if hnytxt$ <> "Y" then return
            curr_part$ = component$
            tm% = 9%
            gosub part_text
            return

        REM *************************************************************~
            *              Load Reference Designators                   *~
            *************************************************************

        load_refs
            refs% = 0% : loc$() = " "
            readkey$ = str(parent$) & str(bomid$) & str(seqno$) & hex(00)
L31170:     call "PLOWNEXT" (#4, readkey$, 31%, f1%(4))
                if f1%(4) = 0 or refs% > 998% then return
            refs% = refs% + 1%
            get #4, using L31210, loc$(refs%), qty_here
L31210:     FMT POS(60), CH(6), PD(14,4)
            if qty_here = 0 or qty_here = 1 then L31170
                loc$(refs%) = loc$(refs%) & "("
                strt% = pos(-loc$(refs%) = "(") + 1%: len% = 12% - strt%
                call "CONVERT" (qty_here,-.2,str(loc$(refs%),strt%,len%))
                loc$(refs%) = loc$(refs%) & ")"
                goto L31170

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35060: FMT                 /* BOMMASTR For 1st parent read            */~
            CH(25),         /* component part number                   */~
            CH(25),         /* assembly part number                    */~
            CH(3),          /* The specific BOM identifier for alternat*/~
            CH(3),          /* bill of materials seq. no.              */~
            PD(14,4),       /* quantity required of comp. for asse.    */~
            PD(14,4),       /* times used (size)                       */~
            PD(14,4),       /* fixed quantity per run                  */~
            PD(14,4),       /* added overage                           */~
            CH(2),          /* BOM marker for STandard, PHantom, Option*/~
            POS(95), CH(4)  /* Text ID Fields                          */

L35180: FMT                         /* BOMMASTR for component reads    */~
                CH(25),             /* COMPONENT PART NUMBER           */~
                XX(25),             /* ASSEMBLY PART NUMBER            */~
                XX(3),              /* BOM STRUCTURE ID                */~
                CH(3),              /* SEQUENCE NUMBER                 */~
                4*PD(14,4),         /* Qty, Times used, Fixed, Over    */~
                CH(2),              /* BOM MARKER                      */~
                POS(95),                                                 ~
                CH(04)              /* Text ID Fields                  */

L35280: FMT                 /* Array record format for sorted BOM      */~
            CH(25),         /* parent part                             */~
            CH(3),          /* BomID                                   */~
            CH(25),         /* assembly part                           */~
            CH(3),          /* sequence no.                            */~
            4*PD(14,4),     /* quantity / times used                   */~
            CH(2),          /* BOM marker                              */~
            2*CH(04)        /* Text ID Fields                          */

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            retkey$ = bomkey$
            end
