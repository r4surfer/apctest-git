        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   DDDD   TTTTT  L      RRRR   PPPP    *~
            *  S        T    C   C  D   D    T    L      R   R  P   P   *~
            *   SSS     T    C      D   D    T    L      RRRR   PPPP    *~
            *      S    T    C   C  D   D    T    L      R   R  P       *~
            *   SSS     T     CCC   DDDD     T    LLLLL  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCDTLRP - Prints Standard Cost details for parts in cost *~
            *            set.  Provides enough detail for user to be    *~
            *            able to validate the accuracy of his input that*~
            *            lead to the cost calculated for the part.      *~
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
            * 07/30/87 ! Original                                 ! HES *~
            * 11/12/87 ! Don't run display if print selected      ! HES *~
            * 03/28/88 ! Swapped "Per Part" & "Per Hour".         ! JIM *~
            * 06/03/88 ! BOM per unit / fixed overage error       ! KAB *~
            * 06/13/88 ! Miscellaneous QC Re-work for Rel 5.01.   ! JIM *~
            * 08/04/88 ! Modified to print two ways               ! LAB *~
            * 10/24/88 ! Fixed some oversights w/previous version ! KAB *~
            * 01/06/89 ! Enlarged CAT$ in ACCEPT to CH(4)         ! MJB *~
            * 10/04/91 ! PRR 11894. Incl components not in cst set! JDH *~
            *          ! PRR 11052. Added rpt header descriptions.!     *~
            * 09/22/93 ! Added support for new BOM Markers 'NC' & ! JDH *~
            *          !   'NP'.                                  !     *~
            * 10/15/93 ! Added support to new BOM Markers 'BP'    ! JDH *~
            *          !   which has costs subtracted from roll-up!     *~
            * 10/03/94 ! PRR 13198 - Now consider Overage Quantity! RJH *~
            *          !   for Phantom Parts.                     !     *~
            * 11/30/95 ! PRR 13407. Look to HNYACYXF for VEND step! JDH *~
	    *          !					  !     *~
	    *          !					  !     *~
	    *          !					  !     *~
	    * 11/19/97 ! CHANGE 1 - ADDED COMMENTS FROM TEST1     ! DJD *~
	    *	       ! RHHSRCE VERSION                          !     *~
	    *          !					  !     *~
            * 06/07/90 ! RANGE ONLY MANUFACTURED PARTS            ! RHH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            act_descr$30,                /* Activity Code Description  */~
            back$1,                      /* Background flag            */~
            bktotals(12),                /* Cost Totals By Bucket      */~
            bk_ids$(12)10,               /* Cost Bucket IDs            */~
            cat$(2,2)4,                  /* Part Category Range        */~
            company$60,                  /* Company / Division Name    */~
            chrn$1,                      /* Chronological order flag   */~
            ctype$8,                     /* Descriptive message        */~
            ct(12),                      /* Counter for printing       */~
            cost$10,                     /* Cost Cumulative total      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            desc$40,                     /* short description          */~
            desc1$30,                    /* Work Center Description    */~
            det_descr$40,                /* Detail description         */~
            dist_yield$1,                /* Distribute Yield Allowance */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* Name Of Print File         */~
            fin_bom(12),                 /* Fold-in BOM costs          */~
            fixt$10,                     /* FIXED_EACH TOTALS          */~
            global$1,                    /* Cost set Status Flag       */~
            hny_map$8,                   /* Mapping to Use             */~
            hold$10,                     /* AMOUNT PLACE HOLDER        */~
            i$(24)80,                    /* Screen Image               */~
            incl(1), incl$(1)1,          /* Dummy PLOWCODE params      */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* Library Of Print File      */~
            line2$79,                    /* Screen Line #2             */~
            map%(12),                    /* Fold-in cost mapping       */~
            message$78,                  /* Informational Message      */~
            msg$(1)1,                    /* PLOWCODE parameter         */~
            part$(2,2)25,                /* Part Number Range          */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey1$99,                 /* Miscellaneous Read/Plow Key*/~
            plowkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            print$(7)10,                 /* Amounts For Print          */~
            print_title$60,              /* Report Sorted By Title     */~
            prt$25, totprt$25,           /* Part Number                */~
            rpt_hdr_msg$(2)13,           /* Descr for report           */~
            rpt_time$8,                  /* Report Time                */~
            set$8,                       /* Cost Set ID                */~
            set_descr$30,                /* Cost Set Description       */~
            spart$25,                    /* Part To Cost               */~
            spart_descr$34,              /* Part Description           */~
            stup$10,                     /* FORMATTED SETUP            */~
            srq$8,                       /* Formatted Standard Run Qty */~
            sub$10,                      /* Subtotals for printing     */~
            textid$4,                    /* Part's Text ID             */~
            userid$3,                    /* Current User Id            */~
            vol$6                        /* Volume Of Print File       */

        dim                                                              ~
            act$(5)4,                    /* Activity Code              */~
            assybom$3,                   /* Assembly BOM               */~
            assyrte$3,                   /* Assembly Route             */~
            bktt$10,                     /* BUCKET TOTALS              */~
            bom$3,                       /* Bill of Materials          */~
            bomt$10,                     /* BOM TOTALS                 */~
            bomcst1(12),                 /* Bill of Material Costs     */~
            bomcst2(12),                 /* Bill of Material Costs     */~
            bomcst3(12),                 /* Bill of Material Costs     */~
            bomcst4(12),                 /* Bill of Material Costs     */~
            bomcost(12),                 /* Bill of Material Costs     */~
            bucket%(7),                  /* Standard Cost Bucket       */~
            compplowkey$31,              /* BOM Plow Key               */~
            lclass$(4)4,                 /* Labor Classes Associated   */~
            lmult(4),                    /* Labor Class Multiplier     */~
            msccost(12),                 /* Misc. Costs                */~
            mkr$2,                       /* BOM Marker                 */~
            norm(5),                     /* Concurrent WC Factor       */~
            oldcompplowkey$(101)31,      /* BOM Plow Key Holder        */~
            otht$10,                     /* OTHER TOTALS               */~
            palevel%(101),               /* Phantom Assy Level         */~
            paok$(101)1,                 /* Phantom Assy O.K.?         */~
            part$25,                     /* Part Code                  */~
            part_descr$32,               /* Part Description           */~
            pbs%(1),                     /* Pick By Step Search Rec.   */~
            pbs$5,                       /* Pick By Step               */~
            pbs$(500)5,                  /* Pick By Steps              */~
            pbshold%(101),               /* Pick By Steps              */~
            phfact(101),                 /* Phantom Multiplier         */~
            readkey$100,                 /* Miscellaneous Read/Plow Key*/~
            rate(7),                     /* Standard Cost Rates        */~
            rtet$10,                     /* ROUTE TOTALS               */~
            rtecost(12),                 /* Route Costs                */~
            rtestep$(500)160,            /* Route Steps                */~
            wc$(5)4,                     /* Work Center                */~
            yield%(500),                 /* Step Yield                 */~
            yield(500)                   /* Cumulative Net Yield       */~

        dim f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CATEGORY ! Part Category File                       *~
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! GENCODES ! System General Codes file.               *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! BOMMASTR ! BOM relationship file                    *~
            * #6  ! WCMASTR  ! Workcenter Master File                   *~
            * #7  ! ENGMASTR ! Engineering Master Filer                 *~
            * #8  ! STCBOMXF ! Standard Cost / BOM-RTE Cross Reference  *~
            * #9  ! TXTFILE  ! System Text File                         *~
            * #10 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * #11 ! STCDETAL ! Standard Cost Details                    *~
            * #12 ! STCWCACT ! Standard Cost Work Center / Activities   *~
            * #13 ! STCLABOR ! Standard Costing Labor Standards         *~
            * #14 ! STCMAPNG ! Standard Cost Set Mappings               *~
            * #15 ! STCCHNGS ! Tickler file of changed components       *~
            * #16 ! RTEMASTR ! Route Master File                        *~
            * #20 ! HNYACTXF ! HNY, WC ACTIVITY CROSS REFERENCE         *~
            * #22 ! TXTFILE  ! Stsem Text File                          *~
            * #25 ! WORKFILE ! Work File For Sort Selection             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos  =   1, keylen = 4

            select #2 , "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #3 , "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #4 , "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1, keylen = 25,                        ~
                        alt key 1, keypos = 102, keylen = 9, dup,        ~
                            key 2, keypos =  90, keylen = 4, dup

            select #5 , "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen = 56

            select #6 , "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            select #7 , "ENGMASTR",                                      ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos =    1, keylen =  29

            select #8 , "STCBOMXF",                                      ~
                        varc,     indexed,  recsize = 72,                ~
                        keypos =   29, keylen =  33,                     ~
                        alt key 1, keypos =  1, keylen = 36,             ~
                            key 2, keypos = 37, keylen = 36

            select #9 , "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos  = 1, keylen = 11

            select #10, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 25

            select #11, "STCDETAL",                                      ~
                        varc,     indexed,  recsize =  340,              ~
                        keypos = 1,    keylen = 28

            select #12, "STCWCACT",                                      ~
                        varc,     indexed,  recsize =  381,              ~
                        keypos =  1,   keylen = 8

            select #13, "STCLABOR",                                      ~
                        varc,     indexed,  recsize =  323,              ~
                        keypos =  1,   keylen = 4

            select #14, "STCMAPNG",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =  1,   keylen =  8

            select #15, "STCCHNGS",                                      ~
                        varc,     indexed,  recsize =   26,              ~
                        keypos =    1, keylen =  26

            select #16, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =   5, keylen = 31,                       ~
                        alt key  1, keypos = 1, keylen = 35

            select #20, "HNYACTXF",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =  1,   keylen =  29,                     ~
                        alt key   1,   keypos =  26, keylen =   4, dup


            select #22, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =  1, keylen =  11

            select #25, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 200 ,              ~
                        keypos = 9, keylen = 8,                          ~
                        alt key 1, keypos = 1, keylen = 16


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#2,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#3,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#4,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#5,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#6,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#7,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#8,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#16, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#20, 0%, 0%, 0%, " ")


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$, "TT", back$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            str(line2$,62) = "STCDTLRP: " & str(cms2v$,,8)
            call "COMPNAME" (12%, company$, 0%)
            cvt = 4.4
            if back$ = "B" then printing_in_background

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub L29000

            wkey% = 0%
            f1%(25%) = 1%

            for fieldnr% = 1 to  6
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0 then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if str(errormsg$,9,1) = hex(84) then L10240
                      if errormsg$ <> " " then L10120
L10240:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 14 then       datasave
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editmode
L11130:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  6 then editmode
            if fieldnr% = lastfieldnr% then editmode
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editmode
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************
        datasave
            if back$ <> "Y" then L19240
                call "READ101" (#2, "zSTCDTLRP." & userid$, f1%(2))
                put #2, using L19110, "zSTCDTLRP." & userid$,             ~
                                     set$, part$(), cat$(), chrn$,       ~
                                     dist_yield$
                if f1%(2) = 0 then write #2  else rewrite #2
L19110:         FMT CH(20), 19*CH(25)
                call "TASKUP" ("ME", 0%)
                goto L65000

        printing_in_background
                errormsg$ = "Canceled."
                call "READ101" (#2, "zSTCDTLRP." & userid$, f1%(2))
                     if f1%(2) = 0 then error_exit
                get #2, using L19110, inpmessage$, set$, part$(), cat$(), ~
                                     chrn$, dist_yield$
                delete #2
                gosub open_and_test_set
                if errormsg$ = " " then L19240
                if str(errormsg$,9,1) = hex(84) then L19240
                   goto error_exit

L19240:     call "SHOSTAT" ("Generating Report")
            call "SETPRNT" ("STC100","        ", 2000%, 0%)
            if keyhit% = 14% then call "SET" addr("PM", "K")
            rpt_time$ = " "             /* Get System Time            */
            call "TIME" (rpt_time$)
            select printer (134)
            errors%, page_no = 0%
            lastmap$ = " "
            plowkey$ = part$(2,1)

L19340:     REM Plow through parts in SCTHNY in part number order...
            call "PLOWNEXT" (#10, plowkey$, 0%, f1%(10))
                if f1%(10) = 0% then all_done

	    /* CHANGE 2 - ADDED THE LINE BELOW   */
            if len(plowkey$) < 19 then goto L19340 /* ONLY MANUFACTURED */
	    /* END CHANGE 2  */


            if str(plowkey$,,25) > part$(2,2) then all_done
            get #10, using L19390, str(plowkey1$,,25), str(plowkey1$,26)
L19390:         FMT CH(25), POS(38), CH(3)
            spart$ = plowkey1$
            call "DESCRIBE" (#4, spart$, spart_descr$, 1%, f1%(4))
                if f1%(4) <> 0 then L19450
                errors% = errors% + 1%
                goto L19340
L19450:     get #4, using L19460, cat$
L19460:     FMT POS(90), CH(4)
            if cat$ <= cat$(2,1) or cat$ > cat$(2,2) then L19340
            l% = -99%
            if workopen% = 0% then                                       ~
                              call "WORKOPEN" (#25, "IO", 1%, f1%(25%))
            wkey% = 0% : workopen% = 1%
            gosub cost_part_for_print
            if wkey% > 0% then gosub print_report
            init (hex(00)) delkey$ : call "DELETE" (#25, delkey$, 0%)
            goto L19340

        all_done
           call "FILEBGON"(#25) : workopen% = 0%
           if page_no = 0 then L19580
           print
           print using L37455
           if back$ = "B" or keyhit% <> 14% then L19580
                 call "GETPRTNM" addr(file$, lib$, vol$)
L19580:    close printer
           call "SETPRNT" (" ", " ", 0%, 1%)
           if back$ = "B" then L19750
           if page_no < 1 then L19690
           if keyhit% <> 14% then inputmode

           REM Display printed file...
           close ws
           call "PUTPARM" addr("E", "INPUT   ",4%,                       ~
                    "FILE    ", file$, 8%, "LIBRARY ", lib$, 8%,         ~
                    "VOLUME  ", vol$, 6%, "ACCESS  ", "PRINT ", 6%, "@")
           call"LINK"addr("DISPLAY ","S"," "," ",0%," "," ",0%,"N",0%,0%)
           call "SCRATCH" addr("F", file$, lib$, vol$, " ", " ", 0%)
           goto inputmode
L19690:          call "ASKUSER" (2%, "INVALID SELECTION PARAMETERS",     ~
                    "Sorry, no parts eligible for printing were found ", ~
                    "using the given selection parameters.",             ~
                    "Press RETURN to change the parameters or exit.")
                 goto inputmode

L19750:     message$ = "rptReport STCDTLRP in background: No data met sel~
        ~ection criteria."
                if page_no = 0 then L19830
            message$ = "rptReport STCDTLRP in background: Completed"
            call "TIME" (str(message$,45,8))
            goto L19830
        error_exit
            message$ = "rptBackground Report STCDTLRP: " & errormsg$
L19830:     call "MESSAGE"addr("XM",str(userid$)&hex(20),message$,78%,0%)
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20130,         /* Cost Set ID        */    ~
                              L20160,         /* Part Number Range  */    ~
                              L20200,         /* Part Cat Range     */    ~
                              L20250,         /* Print In Background*/    ~
                              L20300,         /* Print Chronologically */ ~
                              L20330          /* Dist. Yield Allowance */
            return
L20130: REM Def/Enable Cost Set ID                 SET$
            return

L20160: REM Def/Enable Part Number Range           PART$()
            if part$(1,1)=" " and part$(1,2)=" " then part$(1,1) = "ALL"
            return

L20200: REM Def/Enable Part Category Range         CAT$()
            if cat$(1,1)=" " and cat$(1,2)=" " then cat$(1,1) = "ALL"
            if part$(1,1) = part$(1,2) then enabled% = 0%
            return

L20250: REM Def/Enable Print In Background         BACK$
            if back$ = " " then back$ = "N"
            return

L20300: REM Def/Enable Print Chronologically       CHRN$
            if chrn$ = " " then chrn$ = "N"
            return

L20330: REM Def/Enable Distribute Yield Allowance  DIST_YIELD$
            if dist_yield$ = " " then dist_yield$ = "N"
            if chrn$ <> "N" then return
               dist_yield$ = "N"
               enabled% = 0%
               return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Select Cost Set You Wish To Report On.",                       ~
         "Enter Part Number Range To Be Included On Report.",            ~
         "Enter Part Category Range.",                                   ~
         "Enter 'Y' to print by this report in background.",             ~
         "Enter 'Y' to print report chronologically.",                   ~
         "Enter 'Y' to distribute Yield allowances."

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            errormsg$, inpmessage$, cat$(), part$(), bk_ids$(), back$=" "
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
            *                P R I N T   R E P O R T                    *~
            *-----------------------------------------------------------*~
            * Recals costs to capture details, then prints details.     *~
            *************************************************************

        cost_part_for_print   /* Recost, printing as we go... */

            get #10, using L30100, hny_map$, textid$, assybom$, assyrte$, ~
                                  srq, msccost()
L30100:     FMT POS(26), CH(8), CH(4), 2*CH(3), PD(14,4), POS(252),      ~
                12*PD(14,4)
            call "CONVERT" (srq, -0.2, srq$)

            if hny_map$ = lastmap$ then L30240
            REM Load Up Fold In Mapping if required...
            if hny_map$ = " " then L30240
            call "READ100" (#14, hny_map$, f1%(14))
                if f1%(14) = 1% then L30210
                hny_map$ = " " : mat map% = zer : lastmap$ = " "
                goto L30240
L30210:     get #14 using L30220, map%() : lastmap$ = hny_map$
L30220:     FMT POS(39), 12*BI(1)

L30240:     mat rtecost = zer:mat bomcost = zer
            mat bktotals = zer
            rte% = 0%
            if assyrte$ <> " " then gosub route_costs

            seq% = 0%
            if assybom$ <> " " then gosub bom_costs

            seq% = 0% : gosub misc_costs

            gosub total_costs

            return

        REM *************************************************************~
            * DERIVE ROUTE COSTS.                                       *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        route_costs
            call "STCRTE"                /*                            */~
                     (plowkey1$,         /* PART NEEDED                */~
                      assyrte$,          /* THE WC ROUTE TO USE        */~
                      assybom$,          /* WHICH BOM TO USE           */~
                      rtestep$(),        /* THE DERIVED ROUTE          */~
                      pbs$(),            /* PICK-BY-STEPS              */~
                      rte%,              /* # OF ELEMENTS              */~
                      #5,                /* BOMMASTR                   */~
                      #16,               /* RTEMASTR                   */~
                      #10)               /* STCHNY                     */~

            if rte% > 0% then L31220
               assyrte$ = " ":errors% = errors% + 1%:return

L31220:     for i% = 1% to rte%

                get rtestep$(i%) using L31290, wc$(1), mq%, setup%, run,  ~
                                 wc$(2), norm(2), wc$(3), norm(3),       ~
                                 wc$(4), norm(4), yield%(i%), yield(i%), ~
                                 pcpt%, act$(1), act_descr$, act$(2),    ~
                                 act$(3), act$(4), act$(5), phfactor
L31290:        FMT CH(4), POS(36), BI(4),BI(2), PD(14,4), POS(50), CH(4),~
                        PD(7,4), CH(4), PD(7,4), CH(4), PD(7,4), POS(79),~
                        BI(1), PD(14,7), POS(93), BI(1), CH(4), CH(30),  ~
                        POS(136), 4*CH(4), PD(14,4)

                if wc$(1%) <> "VEND" then L31330
                     gosub determine_vendor_costs
                     if determined% = 1% then L31530
L31330:         prtstep$ = pbs$(i%) : costsrce$ = "RUN"
                if str(prtstep$,5,1) = hex(00) then L31337
                   convert val(str(prtstep$,5,1),1) to str(prtstep$,6,2),~
                   pic(00)
                   str(prtstep$,5,1) = "-"
                   goto L31340
L31337:         str(prtstep$,5) = " "
L31340:         wcfactor = 24
                call "READ100" (#6, wc$(1), f1%(6))
                   if f1%(6) = 0% then L31400
                get #6 using L31380, wcfactor
L31380:             FMT XX(2019), BI(2)

L31400:         norm(1), norm(5) = 1 : wc$(5) = wc$(1)
                asrq = (100 * phfactor * srq)/yield(i%)
                run = (24 * run) / wcfactor
                if run = 0 then qtyrun = 0 else qtyrun = 1 / run
                setup = (24 * setup%) / wcfactor

                for step% = 1% to 4% : prtwc$ = wc$(step%)
                   gosub step_cost
                next step%

                if act$(5) = " " then L31530
                prtwc$ = wc$(5%) : costsrce$ = "SETUP"
                step% = 5% : asrq = 1 : run = (24 * setup%) / wcfactor
                gosub step_cost

L31530:     next i%

            return

        REM *************************************************************~
            * DERIVE COST OF A PARTICULAR STEP.                         *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        step_cost

            if wc$(step%) = " " and act$(step%) = " " then return

            readkey$ = str(wc$(step%),,4) & act$(step%)
            call "READ100" (#12, readkey$, f1%(12))
               if f1%(12) <> 0% then L31790
            str(readkey$,1,4) = " "
            if readkey$ = " " then return
            call "READ100" (#12, readkey$, f1%(12))
               if f1%(12) <> 0% then L31790
            readkey$ = wc$(step%)
            if readkey$ = " " then return
            call "READ100" (#12, readkey$, f1%(12))
               if f1%(12) = 0% then return

L31790:     get #12 using L31800, rate(1%), rate(2%), rate(3%), rate(4%), ~
                                 rate(5%), rate(6%), bucket%(1%),        ~
                                 bucket%(2%), bucket%(3%), bucket%(4%),  ~
                                 bucket%(5%), bucket%(6%), lclass$(),    ~
                                 lmult()
L31800:         FMT POS(9), 6*PD(14,4), 6*BI(1), 4*CH(4), 4*PD(14,4)
            rate(7%) = 0 : bucket%(7%) = 0%   /* Vendor Step element */

            prtclass$ = " " : norm% = step% : gosub set_cost

            for labor% = 1% to 4%
                if lclass$(labor%) = " " then L31930
                   call "READ100" (#13, lclass$(labor%), f1%(13))
                      if f1%(13) = 0% then L31930
                   get #13 using L31890, rate(1%), rate(2%), rate(3%),    ~
                                        rate(4%), rate(5%), rate(6%),    ~
                                        bucket%(1%), bucket%(2%),        ~
                                        bucket%(3%), bucket%(4%),        ~
                                        bucket%(5%), bucket%(6)
L31890:                FMT POS(5), 6*PD(14,4), 6*BI(1)
                   norm% = 1% : norm(1%) = lmult(labor%)
                   prtclass$ = lclass$(labor%)
                   gosub set_cost
L31930:     next labor%
            norm(1%) = 1
            return

        set_cost

            if bucket%(1%) = 0% then L32020
               temptot = rate(1%) * norm(1%)
               n% = 1% : gosub write_workfile
L32020:     if bucket%(2%) = 0% then L32050
               temptot = rate(2%) * norm(1%)
               n% = 2% : gosub write_workfile
L32050:     if bucket%(3%) = 0% then L32080
               temptot = rate(3%) * asrq * norm(norm%)
               n% = 3% : gosub write_workfile
L32080:     if bucket%(4%) = 0% then L32110
               temptot = rate(4%) * asrq * norm(norm%)
               n% = 4% : gosub write_workfile
L32110:     if bucket%(5%) = 0% then L32140
               temptot = rate(5%) * asrq * run * norm(norm%)
               n% = 5% : gosub write_workfile
L32140:     if bucket%(6%) = 0% then return
               temptot = rate(6%) * asrq * run * norm(norm%)
               n% = 6% : gosub write_workfile
               return

            REM Total & print the info...
        write_workfile
            ctype$ = "FIXED"
            if n% > 2% then ctype$ = "PER PART"
            if n% > 4% then ctype$ = "PER HOUR"
            if n% > 6% then ctype$ = "PER UOM "
            temptot = temptot * (1/srq)
            multiplier = 0
            if rate(n%) <> 0 then multiplier = temptot/rate(n%)
            rtecost(bucket%(n%)) = rtecost(bucket%(n%)) + temptot
            bktotals(bucket%(n%)) = bktotals(bucket%(n%)) + temptot
            wkey% = wkey% + 1%
            write #25, using L39913, i%, -2%, 1%, wkey%, prtstep$, prtwc$,~
                       bucket%(n%), part$, bk_ids$(bucket%(n%)),         ~
                       str(readkey$,5,4), prtclass$, ctype$, act_descr$, ~
                       costsrce$, qtyrun, 0, rate(n%), multiplier,       ~
                       " ", temptot, mq%, setup, run, yield(i%), pcpt%,  ~
                       yield%(i%)
            return

        determine_vendor_costs
            determined% = 0%
            readkey$ = str(spart$,,25%) & str(act$(1%),,4%)
            call "READ100" (#20, readkey$, f1%(20%))
                if f1%(20%) = 0% then return
            n% = 7%
            get #20 using L32560, multiplier, rate(n%), bucket%(n%)
L32560:         FMT POS(37), PD(14,7), PD(14,7), BI(4)
            determined% = 1%
*        Dummy up some variable to write to workfile
            temptot = multiplier * rate(n%) * srq
            prtstep$ = pbs$(i%)
            if str(prtstep$,5,1) = hex(00) then L32596
               convert val(str(prtstep$,5,1),1) to str(prtstep$,6,2),    ~
                                                                  pic(00)
               str(prtstep$,5,1) = "-"
               goto L32600
L32596:     str(prtstep$,5) = " "
L32600:     costsrce$ = "VENDOR"
            prtwc$ = wc$(1%)
            str(readkey$,5%,4%) = act$(1)
            prtclass$ = " "
            qtyrun = 1
            setup = 0
            gosub write_workfile
            return

        REM *************************************************************~
            * DERIVE COSTS INHERITED FROM LOWER LEVEL ASSEMBLIES.       *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        bom_costs
           ti%  = 1% : phfact(1%) = 1
           palevel%, palevel%(ti%) = 0%:paok$(ti%) = "Y"

           compplowkey$ = str(plowkey1$,,25) & str(assybom$,,3) & "  0"

L33130:    call "PLOWNEXT" (#5, compplowkey$, 28%, f1%(5))
                if f1%(5%) <> 0% then L33200
                   assybom$ = " ": errors% = errors% + 1%: return

L33170:    call "PLOWNEXT" (#5, compplowkey$, 28%, f1%(5))
                if f1%(5%) =  0% then return

L33200:    get #5, using L33210, part$, qu, tu, addon, fu, mkr$, pbs$
L33210:       FMT CH(25), XX(31), 4*PD(14,4), CH(2), XX(8), CH(4)

           if mkr$ = "RE" or mkr$ = "TL" then L33170
           if mkr$ = "NC" or mkr$ = "NP" then L33170
           seq% = seq% + 1%
           if ti% = 1% then L33300
           if paok$(ti%) = "Y" then L33300
              pbs% = pbshold%(ti% - 1%)
              goto L33380
L33300:    if rte% <= 0% then L33320
           if pbs$ <> " " then L33330
L33320:       pbs% = 1%:goto L33380
L33330:    put str(pbs$,5) using L33340, palevel%(ti%)
L33340:        FMT BI(1)
           search str(pbs$(),1%, 5%*rte%) = str(pbs$,,5) to pbs%() step 5
           pbs% = min(max(1%, int((pbs%(1) + 4%)/5%)), rte%)

L33380:    qtyu  = srq * phfact(ti%) * (qu*tu + fu)
           qtyo = qtyu
           if rte% > 0% then qtyu  = (100 * qtyu) / yield(pbs%)
           qtyu  = qtyu + addon
           qtyo = qtyo + addon
           if qtyu < 0 and mkr$ = "BP" then L33430  /* Subract 'BP's */
               if qtyu <= 0 then L33170

L33430:    if mkr$ = "PH" then goto  L34310
           if mkr$ = "PA" then goto  L34310

        REM ADD IN COSTS HERE
           mat bomcst1 = zer : mat bomcst2 = zer : mat bomcst3 = zer
           call "READ100"(#10, part$, in_set%)
              if in_set% = 0% then L33771
L33490:    get #10, using L33500, bomcst1(), bomcst2(), bomcst3()
L33500:        FMT POS(60), 12*PD(14,4), 12*PD(14,4), 12*PD(14,4)

           if hny_map$ = " " then L33710
              REM Fold in data for print...
              mat fin_bom = zer
              for n% = 1% to 12%
                if map%(n%) = 0% then map%(n%) = n%
                fin_bom(map%(n%)) = fin_bom(map%(n%)) + bomcst1(n%)
              next n%
              mat bomcst1 = fin_bom : mat fin_bom = zer
              for n% = 1% to 12%
                if map%(n%) = 0% then map%(n%) = n%
                fin_bom(map%(n%)) = fin_bom(map%(n%)) + bomcst2(n%)
              next n%
              mat bomcst2 = fin_bom : mat fin_bom = zer
              for n% = 1% to 12%
                if map%(n%) = 0% then map%(n%) = n%
                fin_bom(map%(n%)) = fin_bom(map%(n%)) + bomcst3(n%)
              next n%
              mat bomcst3 = fin_bom

L33710:    REM Figure SRQ & Add To Totals...
	   /* CHANGE 3 - THE FOLLOWING LINES ARE REMARKED OUT  */
           /* THEY ARE NOT REMARKED OUT IN THE TEST1 RHHSRCE   */
           /* VERSION BUT I WANTED TO SHOW THE NUMBER OF LINES */
           /* ALTERED BY THIS CHANGE                           */

REM        qtyu = qtyu * (1/srq)
REM        qtyo = qtyo * (1/srq)
REM        mat bomcst4 = (qtyu) * bomcst1
REM        mat bomcost = bomcost + bomcst4
REM        mat bomcst4 = (qtyu) * bomcst2
REM        mat bomcost = bomcost + bomcst4
REM        mat bomcst4 = (qtyu) * bomcst3
REM        mat bomcost = bomcost + bomcst4

	   /* NOW THESE LINES ARE REPLACING THE ABOVE REMARKED */
           /* LINES 					       */

           qtyu = qtyu * (1/srq)
           qtyo = qtyo * (1/srq)
           for n% = 1% to 12%                 /* MOD FILE - 12/20/89 */
               bomcst4(n%) = (qtyu) * bomcst1(n%)
               bomcost(n%) = bomcost(n%) + bomcst4(n%)
               bomcst4(n%) = (qtyu) * bomcst3(n%)
               bomcost(n%) = bomcost(n%) + bomcst4(n%)
           next n%
           mat bomcst4 = (qtyu) * bomcst2
           mat bomcost = bomcost + bomcst4

           for n% = 1% to 12%
               bomcost(n%) = round(bomcost(n%),4)
           next n%

	   /* END CHANGE 3  */

           /* CHANGE 4 - CHANGED GOTO LINE NUMBER TO BE L33763 */
	   if dist_yield$ = "N" then L33796
                 mat bomcst1 = (qtyo) * bomcst1
                 mat bomcst2 = (qtyo) * bomcst2
                 mat bomcst3 = (qtyo) * bomcst3
                 goto L33771
L33796:          mat bomcst1 = (qtyu) * bomcst1
                 goto L33763				/* CHANGE 5 */

/* CHANGE 6 - ADDED LINE NUMBER L33763 - THE REST OF THE CODE WAS THERE */
L33763:          mat bomcst2 = (qtyu) * bomcst2
                 mat bomcst3 = (qtyu) * bomcst3
/* CHANGE 7 - REMOVED L33800 AND REPLACED WITH L33771 */
L33771:    for n% = 1% to 12%
               bomcst1(n%) = round(bomcst1(n%),4)
               bomcst2(n%) = round(bomcst2(n%),4)
               bomcst3(n%) = round(bomcst3(n%),4)
           next n%
/* CHANGE 7 END - THE LINE BELOW USED TO BE L33800   */
           REM Format And Print Da Datas...
           call "DESCRIBE" (#4, part$, part_descr$, 0%, f1%(4))
                if f1%(4) = 0 then part_descr$ = "** Not On File **"
           lastseq% = wkey%
           for n% = 1% to 12%
               if in_set% = 0% then L33900
               if abs(bomcst1(n%)) > 0 then L33930
               if abs(bomcst2(n%)) > 0 then L33930
               if abs(bomcst3(n%)) > 0 then L33930
                  goto L34200
L33900:        save_bkt$ = bk_ids$(n%)
               bk_ids$(n%) = "Not In Set"
L33930:        line_total = bomcst1(n%) + bomcst2(n%) + bomcst3(n%)
               temptot, bktotals(n%) = bktotals(n%) + line_total
               wkey% = wkey% + 1%
               write #25, using L39913, pbs%, seq%, 2%, wkey%,            ~
                          pbs$, " ", n%, part$,                          ~
                          bk_ids$(n%), " ", " ", " ", part_descr$, " ",  ~
                          qtyu, bomcst1(n%), bomcst2(n%), bomcst3(n%),   ~
                          " ", line_total, 0%, 0, qtyo, 100, 100%, 100%
           if in_set% <> 0% then L34060
               bk_ids$(n%) = save_bkt$
               in_set% = 1%  /* Now act like normal */
L34060:    if dist_yield$ = "N" then L34200
           if yield(pbs%) = 100 then L34200
              for ii% = pbs% to rte%
                  if yield%(ii%) = 100% then L34190
                  prtstep$ = pbs$(ii%)
                  if str(prtstep$,5,1) <> hex(00) then L34095
                     str(prtstep$,5) = " "
                     goto L34102
L34095:           convert val(str(prtstep$,5,1),1)                       ~
                                            to str(prtstep$,6,2), pic(00)
                  str(prtstep$,5,1) = "-"
L34102:     temp  = yield%(ii%)
            temp1 = (bomcst1(n%) * (100 - temp)) / yield(ii%)
            temp2 = (bomcst2(n%) * (100 - temp)) / yield(ii%)
            temp3 = (bomcst3(n%) * (100 - temp)) / yield(ii%)
            temp4 = temp1 + temp2 + temp3
            wkey% = wkey% + 1%
            write #25, using L39913, ii%, -1%, 1%, wkey%, prtstep$, " ",  ~
                       n%, " ", " ", " ", " ", " ", " ", " ", 0,         ~
                       temp1, temp2, temp3, " ", temp4, 0%, 0, 0, 100,   ~
                       100%, 100%
L34190:       next ii%
L34200:    next n%
           if lastseq% <> wkey% then L33170
               REM All costs were zero for comp, must show them that...
               wkey% = wkey% + 1%
               write #25, using L39913, pbs%, seq%, 2%, wkey%,            ~
                                       pbs$, " ", 1%, part$,             ~
                          "N/A", " ", " ", " ", part_descr$, " ", qtyu,  ~
                          0, 0, 0, " ", 0, 0%, 0, 0, 100, 100%, 100%

               goto L33170

L34310: REM * * * PHANTOM LOOP LOGIC * * *

             call "READ100"(#10, part$, f1%(10))
                if f1%(10) = 0% then L33170
             get #10, using L34360, bom$
L34360:         FMT POS(38), CH(3)
             if bom$ = " " then L33490
             if ti% > 99% then L33490
             oldcompplowkey$(ti%) = compplowkey$
             pbshold%(ti%) = pbs%
             ti% = ti% + 1%
             if mkr$ <> "PA" then L34460
             if paok$(ti% - 1%) <> "Y" then L34460
                palevel%, palevel%(ti%) = palevel% + 1%
                paok$(ti%) = "Y"
L34460
*           PHFACT(TI%) = PHFACT(TI%-1%) * QU * TU
             phfact(ti%) = qtyu / srq
             compplowkey$ = str(part$,1,25) & str(bom$,1,3) & "  0"
             gosub L33130
             paok$(ti%) = " "
             ti% = ti% - 1%
             compplowkey$ = oldcompplowkey$(ti%)
             goto L33170

        REM *************************************************************~
            * PRINT COSTS ADDED DIRECTLY AT THIS LEVEL.                 *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        misc_costs
            plowkey2$ = str(spart$,,25) & hex(00)
L35090:     call "PLOWNEXT" (#11, plowkey2$, 25%, f1%(11))
                if f1%(11) = 0% then L35330
            get #11 using L35120, det_descr$, fix, per, n%
L35120:         FMT  POS(29), CH(40), 2*PD(14,4), BI(1)
            seq% = seq% + 1%
            fixed_each = 0
            if srq <> 0 then fixed_each = round(fix/srq,4)
            per = round(per,4)
            line_total = per + fixed_each
            bktotals(n%) = bktotals(n%) + line_total
            wkey% = wkey% + 1%
            write #25, using L39913, -2%, seq%, 3%, wkey%,                ~
                       " ", " ", n%, " ",                                ~
                       bk_ids$(n%), " ", " ", " ", det_descr$, " ", 0,   ~
                       0, per, fix, " ", line_total, 0%, 0, 0, 100,      ~
                       100%, 100%
            goto L35090

L35330:     if seq% > 0% then return
            det_descr$ = "Direct Miscellaneous"
            mat bktotals = bktotals + msccost
            for n% = 1% to 12%
            if msccost(n%) = 0 then L35430
            wkey% = wkey% + 1%
            seq% = seq% + 1%
            write #25, using L39913, -2%, seq%, 3%, wkey%,                ~
                       " ", " ", n%, " ", bk_ids$(n%), " ", " ", " ",    ~
                       det_descr$, " ", 0, 0, msccost(n%), 0, " ",       ~
                       msccost(n%), 0%, 0, 0, 100, 100%, 100%
L35430:     next n%
            return

        REM *************************************************************~
            * PRINT TOTAL COST INFO BY BUCKET.                          *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        total_costs
           bigast$ = " "
           for n% = 1% to 12%
               if abs(bomcost(n%)) > 0 then L36170
               if abs(rtecost(n%)) > 0 then L36170
               if abs(msccost(n%)) > 0 then L36170
                  goto L36330
L36170:        line_total = bomcost(n%) + rtecost(n%) + msccost(n%)
               ast$ = " "
               if round(line_total,4) <> round(bktotals(n%),4) then      ~
                                                              ast$ = "<>"
               pbs$ = all(hex(ff))
               wkey% = wkey% + 1%
               write #25, using L39913, -1%, n%, 4%, wkey%,               ~
                         " ", " ", n%, spart$,                           ~
                         bk_ids$(n%), " ", " ", " ", spart_descr$, " ",  ~
                         0, bomcost(n%), rtecost(n%), msccost(n%), ast$, ~
                         line_total, 0%, 0, 0, 100, 100%, 100%
               if ast$ <> " " then bigast$ = "**"
L36330:    next n%
           return

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

        page_control
            if l% < 1% then gosub print_heading
            if section% = lastsection% then L37065
            on section% gosub route_header, bom_header, misc_header,     ~
                summary_header, chron_headings
            if l% < 4% then gosub print_heading
            lastsection% = section%
L37065:     l% = l% - 1%  /* Decrease for line about to be printed */
            return

        print_heading /* Print new page header */
            print page
            print_title$ = "Standard Cost Detail Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            rpt_hdr_msg$(1) = "Cost Group"
            rpt_hdr_msg$(2) = " "
            if chrn$ <> "Y" then L37100
                rpt_hdr_msg$(1) = "Chronologic"
                if dist_yield$ = "Y" then rpt_hdr_msg$(2) = "Yield by WC"~
                                   else rpt_hdr_msg$(2) = "Yield by Part"
L37100:     if l% = -99% then page_no = int(page_no + 1)
            page_no = page_no + .01
            print using L37410, date$, rpt_time$, company$
            print using L37425, userid$, rpt_hdr_msg$(1), print_title$,   ~
                               rpt_hdr_msg$(2), page_no
            inpmessage$ = spart$ & " " & spart_descr$
            print using L37440, set$, inpmessage$, cat$, assybom$,        ~
                               assyrte$,srq$
            l% = 56%
            lastsection% = 99%
            return

        route_header
            if l% < 5% then gosub print_heading
            print
            print using L37475
            print using L37485
            print using L37500
            l% = l% - 4%
            wrkkey% = 1%
            return

        bom_header
            if l% < 4% then gosub print_heading
            print
            print using L37560, "#"
            print using L37575
            l% = l% - 3%
            seqkey% = 1%
            return

        misc_header
            if l% < 4% then gosub print_heading
            print
            print using L38100, "#"
            print using L38130
            l% = l% - 3%
            return

        summary_header
            if l% < 4% then gosub print_heading
            print
            print using L38250
            print using L38280
            l% = l% - 3%
            totkey% = 1%
            header% = 0%
            return

        chron_headings
            if l% < 5% then gosub print_heading
            print
            print using L38450
            print using L38470
            print using L38490
            print using L38500
            print using L38510
            for c% = 1% to 11%
               print using L39998, bk_ids$(c%);
            next c%
            print using L39998, bk_ids$(12%)
            print using L38530
            print using L38560
            print using L38590
            print using L38610
            l% = l% - 12%
            return
        REM Image statements for report in general...
L37410: %######## ########                   ############################~
        ~################################                    STCDTLRP:STC1~
        ~00
L37425: %USER ID: ###           ###########  ############################~
        ~################################  #############       PAGE:#####.~
        ~##
L37440: %SET:########  PART:#############################################~
        ~############### CAT:#### BOM:### RTE:###  Standard Run Qty:######~
        ~##
L37455: %                                        * * * * *   E N D   O F ~
        ~  R E P O R T   * * * * *

        REM Image statements for RTE costs...
L37475: %        WORK                                 COST  WCTR/ACTV  LA~
        ~BOR   COST
L37485: %STEP    CNTR  ACTIVITY DESCRIPTION          SOURCE   CODES    CL~
        ~ASS   TYPE      RATE    MULTIPLIER    TOTAL   BUCKET ID./CUMULATI~
        ~VE
L37500: %======= ==== ============================== ====== =========  ==~
        ~=== ======== ========== ========== ========== ========== ========~
        ~==
L37515: %####### #### ############################## ###### #### ####  ##~
        ~##  ######## ########## ########## ########## ########## ########~
        ~##
L37530: %             ------------------------------                     ~
        ~                                   ----------
L37540: %                  Total Costs From Routing:                     ~
        ~                                   ##########

        REM Image statements for BOM Costs...
L37560: %SEQ#  COMPONENT PART NUMBER     PART DESCRIPTION         QUANTIT~
        ~Y  BOM COST   RTE COST  OTHER COST    TOTAL   BUCKET ID./CUMULATI~
        ~VE
L37575: %===== ========================= ======================== =======~
        ~= ========== ========== ========== ========== ========== ========~
        ~==
L38010: %####) ######################### ######################## #######~
        ~# ########## ########## ########## ########## ########## ########~
        ~##
L38040: %                                ------------------------        ~
        ~  ---------- ---------- ---------- ----------
L38060: %                                   Total Costs From BOM:        ~
        ~  ########## ########## ########## ##########

        REM Image statements for Misc Costs...
L38100: %SEQ#  DESCRIPTION OF DETAIL                         PER PART    ~
        ~ FIXED AMNT EACH @ SRQ                TOTAL   BUCKET ID./CUMULATI~
        ~VE
L38130: %===== ========================================    ==========    ~
        ~ ========== ==========             ========== ========== ========~
        ~==
L38160: %####) ########################################    ##########    ~
        ~ ########## ##########             ########## ########## ########~
        ~##
L38190: %      ----------------------------------------    ----------    ~
        ~ ---------- ----------             ----------
L38210: %               Total Costs Details This Level:    ##########    ~
        ~ ########## ##########             ##########

        REM Image statements for Total All Costs...
L38250: %PART NUMBER (IN SUMMARY)    PART DESCRIPTION                   <~
        ~>  BOM COST   RTE COST  OTHER COST    TOTAL   BUCKET ID./CUMULATI~
        ~VE
L38280: %=========================   ================================== =~
        ~= ========== ========== ========== ========== ========== ========~
        ~==
L38310: %#########################   ################################## #~
        ~# ########## ########## ########## ########## ########## ########~
        ~##
L38340: %                            ---------------------------------- -~
        ~- ---------- ---------- ---------- ----------
L38360: %                                     Total Cost Each For Part: #~
        ~# ########## ########## ########## ########## at SRQ of: ########
        %  WARNING--WARNING--WARNING--WARNING--WARNING--WARNING--WARNING-~

        %**Last calculated cost does not equal report calculated cost.  P~
        ~robably result of changes to BOM or ROUTE since last global rollu~
        ~p.
        REM IMAGE FOR CHRONOLOGICAL HEADING
L38450: %TYPE SEQ   PART NUMBER               DESCRIPTION                ~
        ~       YLD ADJ QTY    QUANTITY
L38470: %==== ===   ========================= ===========================~
        ~=====   ==========  ==========
L38490: %TYPE STEP    WORK      MQ        SETUP        RUN    UNITS PER  ~
        ~ %   %  CUMULATIVE  ---ACTIVITY--------------
L38500: %     NBR     CTR      DAYS       HOURS       HOURS        HOUR C~
        ~OMP YLD      YIELD  CODE DESCRIPTION
L38510: %==== ====    ====     =====   ========  ==========  ========== =~
        ~=== === ==========  =========================
L38513: %#    ####### ####     ##### ##########  ##########  ##########  ~
        ~### ### ##########  #########################
L38530: %========== ========== ========== ========== ========== =========~
        ~= ========== ========== ========== ========== ========== ========~
        ~==
L38546: %# #####    ######################### ###########################~
        ~#####   ##########  ##########

L38560: %                                                                ~
        ~                                              COMP/STEP  CUMULATI~
        ~VE
L38590: %                                                                ~
        ~                                                 COST        COST
L38610: %                                                                ~
        ~                                              =========  ========~
        ~==
        REM * THIS CODE PRINTS THE TOTALS LINE FOR NON CHRON ORDER
        wrk_tot

             call "CONVERT" (bktt, cvt, bktt$)
             gosub page_control
             print using L37530
             l% = l% - 1% /* AVOID THESE TWO LINES BEING SPLIT */
             print using L37540, bktt$
             bktt = 0
             lastsection% = 0%
             return
        bom_tot

             call "CONVERT" (bomt, cvt, bomt$)
             call "CONVERT" (rtet, cvt, rtet$)
             call "CONVERT" (otht, cvt, otht$)
             call "CONVERT" (bktt, cvt, bktt$)
             gosub page_control
             print using L38040
             l% = l% - 1% /* AVOID THESE TWO LINES BEING SPLIT */
             print using L38060, bomt$, rtet$, otht$, bktt$
             bktt, bomt, rtet, otht = 0
             lastsection% = 0%
             return


        msc_tot

             call "CONVERT" (fixt, cvt, fixt$)
             call "CONVERT" (rtet, cvt, rtet$)
             call "CONVERT" (otht, cvt, otht$)
             call "CONVERT" (bktt, cvt, bktt$)
             gosub page_control
             print using L38190
             l% = l% - 1% /* AVOID THESE TWO LINES BEING SPLIT */
             print using L38210, rtet$, otht$, fixt$, bktt$
             bktt, fixt, rtet, otht = 0
             lastsection% = 0%
             return


        tot_tot

             call "CONVERT" (bomt, cvt, bomt$)
             call "CONVERT" (rtet, cvt, rtet$)
             call "CONVERT" (otht, cvt, otht$)
             call "CONVERT" (bktt, cvt, bktt$)
             gosub page_control
             print using L38340
             l% = l% - 1% /* AVOID THESE TWO LINES BEING SPLIT */
             print using L38360, ch$, bomt$, rtet$, otht$, bktt$, srq$
             bktt, bomt, rtet, otht = 0

L39810:      line% = 60% - l%
             call "TXTPRINT" (#22, 0%, 134%, textid$, "STC100", 6%,      ~
                              line%, 60%, "Y", " ", stat%)
             if stat% = 0% then L39840
             l% = 0% : gosub page_control
             goto L39810

L39840:      lastsection% = 0%
             return

        REM *************************************************************~
            *                                                           *~
            *               F O R M A T   S T A T E M E N T S           *~
            *                                                           *~
            *************************************************************

        REM FORMAT STATEMENT FOR WORK FILE - 25%
L39913:     FMT BI(4),                             /* PBS%, -2, -1     */~
                BI(4),                             /* -1, BOM, MSC SEQ */~
                BI(4),                             /* STEP ID (TYPE)   */~
                BI(4),                             /* SEQUENCE NUM     */~
                CH(7),                             /* STEP NUMBER      */~
                CH(4),                             /* WORK CENTER      */~
                BI(2),                             /* COUNTER          */~
                CH(25),                            /* PART NUMBER      */~
                CH(10),                            /* BUCKET ID        */~
                CH(4),                             /* ACTIVITY CODE    */~
                CH(4),                             /* LABOR CLASS      */~
                CH(8),                             /* COST TYPE        */~
                CH(40),                            /* DESCRIPTION      */~
                CH(6),                             /* COST SOURCE      */~
                PD(14,7),                          /* QUANTITY         */~
                PD(14,7),                          /* BOM COSTS        */~
                PD(14,7),                          /* RTE COST/ RATE   */~
                PD(14,7),                          /* OTHER COST/MULT  */~
                CH(2),                             /* CHARACTER        */~
                PD(14,7),                          /* LINE TOTAL       */~
                BI(4),                             /* MOVE/QUEUE DAY   */~
                PD(14,7),                          /* SET UP TIME HRS  */~
                PD(14,7),                          /* RUN TIME HRS/PRT */~
                PD(14,7),                          /* YIELD PCT        */~
                BI(1),                             /* PCT COMPLETE     */~
                BI(1)                              /* STEP YIELD       */

        REM FORMAT STATEMENTS FOR PRINT LINES
L39997: FMT XX(110), CH(10), XX(1), CH(10)
L39998: FMT CH(10), XX(1)

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40180,         /* Cost Set ID       */   ~
                                L40180,         /* Part Number Range */   ~
                                L40180,         /* Part Cat Range    */   ~
                                L40180,         /* Print In Backgroun*/   ~
                                L40180,         /* Print Chronologica*/   ~
                                L40180          /* Distribute Yeild ?*/
              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Part Standard Cost Detailed Analysis",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Cost Set ID",                                ~
               at (06,30), fac(lfac$( 1)), set$                 , ch(08),~
               at (06,45), fac(hex(8c)),   set_descr$           , ch(30),~
                                                                         ~
               at (07,02), "Part Number Range",                          ~
               at (07,30), fac(lfac$( 2)), part$(1,1)           , ch(25),~
               at (07,56), fac(lfac$( 2)), part$(1,2)           , ch(25),~
                                                                         ~
               at (08,02), "Part Category Range",                        ~
               at (08,30), fac(lfac$( 3)), cat$(1,1)            , ch(04),~
               at (08,56), fac(lfac$( 3)), cat$(1,2)            , ch(04),~
                                                                         ~
               at (09,02), "Print Report In Background?",                ~
               at (09,30), fac(lfac$( 4)), back$                , ch(01),~
                                                                         ~
               at (10,02), "Print Rpt Chronologically?",                 ~
               at (10,30), fac(lfac$( 5)), chrn$                , ch(01),~
                                                                         ~
               at (11,02), "Distribute Yield Allowance?",                ~
               at (11,30), fac(lfac$( 6)), dist_yield$          , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then L40570
               if fieldnr% = 1% then L40570
               spart_descr$ = hex(06) & "Part Numbers In Set: " & set$
               call "PLOWCODE" (#10, " ", spart_descr$, 8000%, .32, 0%,  ~
                             msg$(), 0, 0, incl(), incl$(), "Y", "Y", #4)

L40570:        if keyhit% <> 13% then L40591
                  call "MANUAL" ("STCDTLRP") : goto L40210

L40591:        if keyhit% <> 23% then L40593
                  cvt = 4.4:goto L40210
L40593:        if keyhit% <> 24% then L40600
                  cvt = 4.7:goto L40210
L40600:        if keyhit% <> 15% then L40630
                  call "PRNTSCRN" : goto L40210

L40630:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40840     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)See Parts In Set    " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffff0dff0f10001718)
            str(pf$(3),63,1) = hex(84)
            if fieldnr% = 1% then L40790
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40790:     if fieldnr% > 1% then L40820
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
                str(pf$(3),18,36) = " "  :  str(pfkeys$, 9,1) = hex(ff)
L40820:     return

L40840: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)See Parts In Set    " &        ~
                     "  (14)Display Report   (16)Print Report"
            pfkeys$ = hex(01ffffffffffffff09ffffff0d0e0f10001718)
            if back$ = "Y" then str(pfkeys$,14,1) = hex(ff)
            if back$ = "Y" then pf$(3) = " "
            if back$ = "Y" then str(pf$(3),64) = "(16)Submit Task"
            str(pf$(3),42,1) = hex(84)
            return
L40970:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)See Parts In Set    " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffff09ffffff0dff0fff00)
            return

        REM *************************************************************~
            * THIS IS WHERE WE ARE GOING TO PRINT THE REPORT.  ONE PART *~
            * AT A TIME.                                                *~
            *************************************************************~

        print_report

            wrkkey%   = 0%
            header%   = 0%
            seqkey%   = -1%
            totkey%   = -1%
            lastseq%  = -1%
            lastseq1% = -1%
            laststep$ = all (hex(ff))
            totprt$   = all (hex(ff))
            subt = 0
            cost = 0
            first% = -1%
            subtot = 0
            mat ct = zer
            ss$ = " "

            readkey$ = all(hex(00))

            x% = 0%
            if chrn$ = "N" then goto L42140
            x% = 1%

L42140: REM READ THE INDEXED FILE AND PRINT THE DATA

            call "PLOWALTS"(#25, readkey$, x%, 0%, f1%(25%))
L42155:         if f1%(25%) = 0% then L43550
            get #25%, using L39913, junk%, seq%, s_id%, junk%,            ~
                                   step$, wrk$, ctr%, prt$,              ~
                bck$, act$, lbr$, typ$, desc$, cst$, qty, bom, rte,      ~
                oth, ch$, bk, mq%, setup, run, yld, pcpt%, yld%

            on x% goto L45000

            ct(ctr%) = ct(ctr%) + bk
            call "CONVERT" (qty, 0.4, str(print$(1%),,8))
            call "CONVERT" (bom, cvt, print$(2%))
            call "CONVERT" (rte, cvt, print$(3%))
            call "CONVERT" (oth, cvt, print$(4%))
            call "CONVERT" (bk, cvt, print$(5%))
            temp = ct(ctr%) : call "CONVERT" (temp, cvt, print$(6%))
            if bck$ <> "Not In Set" then L42225
                print$(2%), print$(3%), print$(4%), print$(5%) = " "

L42225:     on s_id% goto L42235, L42440, L43070, L43305
               goto L43515

L42235:     desc1$ = str(desc$,1,30)
            section% = 1% : ss$ = "W"
            gosub page_control
            print using L37515, step$, wrk$, desc$, cst$, wrk$, act$,     ~
            lbr$, typ$, print$(3%), print$(4%), print$(5%), bck$,        ~
            print$(6%)
            bktt = bktt + bk
            goto L43515

L42440:     desc$ = str(desc$,1,25)
            if ss$ = "W" then gosub wrk_tot
            section% = 2%
            ss$ = "B"
            gosub page_control
            print using L38010, seq%, prt$, desc$, print$(1%), print$(2%),~
                print$(3%), print$(4%), print$(5%), bck$, print$(6%)
            bktt = bktt + bk
            bomt = bomt + bom
            rtet = rtet + rte
            otht = otht + oth
            goto L43515

        REM misc_header
L43070:     if ss$ = "W" then gosub wrk_tot
            if ss$ = "B" then gosub bom_tot
            section% = 3%
            ss$ = "M"
            fixed_each = 0
            if srq <> 0 then fixed_each = round(oth/srq,4)
            desc$ = str(desc$,1,40)
            call "CONVERT"(fixed_each, cvt, print$(1%))
            gosub page_control
            print using L38160, seq%, desc$, print$(3%), print$(4%),      ~
               print$(1%), print$(5%), bck$, print$(6%)
            bktt = bktt + bk
            fixt = fixt + fixed_each
            rtet = rtet + rte
            otht = otht + oth
            goto L43515

L43305:     desc$ = str(desc$,1,34)
            if ss$ = "M" then gosub msc_tot
            if ss$ = "B" then gosub bom_tot
            if ss$ = "W" then gosub wrk_tot
            section% = 4%
            ss$ = "T"
            subtot = subtot + bom + rte + oth
            call "CONVERT" (subtot, cvt, print$(6%))
            gosub page_control
            if header% = 0% then                                         ~
            print using L38310, prt$, desc$, ch$, print$(2%), print$(3%), ~
                     print$(4%), print$(5%), bck$, print$(6%)            ~
                            else                                         ~
            print using L38310,  " ",   " ", ch$, print$(2%), print$(3%), ~
                     print$(4%), print$(5%), bck$, print$(6%)
            header% = 1%
            bktt = bktt + bk
            bomt = bomt + bom
            rtet = rtet + rte
            otht = otht + oth
L43515:     if seqkey% = -1% then seqkey% = 0%
            if totkey% = -1% then totkey% = 0%

            call "READNEXT"(#25, f1%(25%))

            goto L42155

L43550: REM THIS IS THE PRINT FOR TOTAL CUMULATIVE
           subt = 0 : first% = 1%
           if chrn$ = "N" then gosub tot_tot else gosub control_1
           return

L45000: REM *************************************************************~
            * THIS IS WHERE WE ARE GOING TO PRINT THE REPORT.  ONE PART *~
            * AT A TIME.                                                *~
            *************************************************************~

            section% = 5%  /* SET EQUAL TO 5 TO START */

            call "CONVERT" (qty, 0.4, print$(1%))

            on s_id% goto L45470, L45890, L46360, L46840
               goto L47390

L45470:     desc1$ = str(desc$,1,30)
            if step$ = laststep$ then goto L47300
                     gosub control_1
                     gosub page_control
                     call "CONVERT" (setup, 2.2, stup$)
                     call "CONVERT" (run, 4.7, print$(2))
                     call "CONVERT" (yld, 4.7, print$(3))
                     print using L38513, "S", step$, wrk$, mq%, stup$,    ~
                          print$(2), print$(1%), pcpt%, yld%,            ~
                          print$(3), desc$
                     laststep$ = step$
            goto L47300

L45890:     ss$ = "B"
            desc$ = str(desc$,1,25)
            if seq% = lastseq% then goto L47300
                     gosub control_1
                     gosub page_control
                     call "CONVERT" (run, 0.4, print$(2))
                     print using L38546, "C", seq%, prt$,                 ~
                           desc$, print$(1%), print$(2%)
                     lastseq% = seq%
            goto L47300

        REM misc_header
L46360:     ss$ = "M"
            fixed_each = 0
            if srq <> 0 then fixed_each = round(oth/srq,4)
            desc$ = str(desc$,1,40)
            if seq% = lastseq1% then goto L47300
                     gosub control_1
                     gosub page_control
                     print using L38546, "M", seq%, prt$,                 ~
                           desc$, " "
                     lastseq1% = seq%
            goto L47300

L46840:     desc$ = str(desc$,1,34)
            if prt$ = totprt$ then goto L47300
                     gosub control_1
                     print skip(2)
                     l% = l% - 2%
                     gosub page_control
                     print using L38546, "T", " ", prt$, desc$
                     totprt$ = prt$
                     cost = 0
                     mat ct = zer

L47300:     subt = subt + bk
            cost = cost + bk
            ct(ctr%) =  ct(ctr%) + bk

L47390:     call "READNEXT"(#25, f1%(25%))
            goto L42155

        control_1
            if first% =  -1  then goto L47740
            for countloop% = 1% to 12%
                hold$ = " "
                if ct(countloop%) <= 0 then L47670
                temp = ct(countloop%):call "CONVERT" (temp, cvt, hold$)
L47670:         print using L39998, hold$;
            next countloop%
            print
            l% = l% - 1%
            temp = cost : call "CONVERT" (temp, cvt, cost$)
            call "CONVERT" (subt, cvt, sub$)
            if first% > 0% then sub$ = " "
            gosub page_control
            print using L39997, sub$, cost$
L47740:     subt = 0
            first% = 0%
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130,         /* Cost Set ID       */     ~
                              L50360,         /* Part Number Range */     ~
                              L50410,         /* Part Cat Range    */     ~
                              L50460,         /* Print in Backgroun*/     ~
                              L50500,         /* Print Chronologica*/     ~
                              L50550          /* Dist. Yield Allow.*/
            return
L50130: REM Test for Cost Set ID                  SET$
            plowkey$   = "STC.HDR." & set$
            set_descr$ = hex(06) & "Select Cost Set"
            call "PLOWCODE" (#2, plowkey$, set_descr$, 8%, 0.30, onfile%)
            if onfile% = 1% then L50210
                errormsg$ = "Set Not On File: " & set$
                set_descr$ = " "
                return
L50210:     global$ = " "
            set$ = str(plowkey$,9)
        open_and_test_set
            call "STCFOPEN" (set$, "SSSSSS", #2, errormsg$,              ~
                                            #10, #11, #12, #13, #14, #15)
            if errormsg$ <> " " then return
            REM Retrieve control flag to determine set status...
            readkey$ = "STC.HDR." & set$
            call "READ100" (#2, readkey$, f1%(2))
            get #2 using L50310, bk_ids$(), global$
L50310:         FMT POS(60), 12*CH(10), POS(420), CH(1)
            if global$ = " " then return
                errormsg$ = "Warning: Set Is Flagged For Global Recalc."
                str(errormsg$,9,1) = hex(84)
                return

L50360: REM Test for Part Number Range            PART$
            call "TESTRNGE"   (part$(1,1), part$(1,2), part$(2,1),       ~
                               part$(2,2), errormsg$, #4)
            return

L50410: REM Test for Part Category Range          CAT$
            call "TESTRNGE"   (cat$(1,1), cat$(1,2), cat$(2,1),          ~
                               cat$(2,2), errormsg$, #1)
            return

L50460: REM Test for Print In Background          BACK$
            if back$ <> "Y" then back$ = "N"
            return

L50500: REM Test for Printing Chronologically     CHRN$
            if chrn$ <> "Y" then chrn$ = "N"
            return

L50550: REM Test for Distribute Yield Allowance   DIST_YIELD$
            if chrn$ <> "Y" then dist_yield$ = "N"
            if dist_yield$ <> "Y" then dist_yield$ = "N"
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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

            end
