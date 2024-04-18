        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   EEEEE  N   N   GGG    SSS   BBBB    *~
            *  S        T    C   C  E      NN  N  G      S      B   B   *~
            *   SSS     T    C      EEEE   N N N  G GGG   SSS   BBBB    *~
            *      S    T    C   C  E      N  NN  G   G      S  B   B   *~
            *   SSS     T     CCC   EEEEE  N   N   GGG    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCENGSB - Builds Standard Costs for Purchased Parts      *~
            *            using the last average stored in HNYQUAN, or   *~
            *            the Vendor Price Catalog, or Receiver.  Also   *~
            *            allows the operator the option of resetting the*~
            *            effective BOM based on the planning calendar in*~
            *            ENGMASTR. Optionally establishes parts in      *~
            *            STCHNY if they don't exist. Optionally deletes *~
            *            detail costs For Build Parts.                  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/26/87 ! Original                                 ! JIM *~
            * 03/10/88 ! Re-wrote To Include Purchase Price Extrct! HES *~
            * 03/24/89 ! Fixed Bug; when updating cost of Purchased RJM *~
            *          !    Parts & part has to be added to the   !     *~
            *          !    Cost Set, the detail of source was    !     *~
            *          !    not being added to the STCDETAL file. !     *~
            *          !    Now it works correctly.               !     *~
            * 06/10/91 ! PRR 11059.  Items no longer added in 'S' ! JDH *~
            *          !   mode if no costs(purc) or BOM/RTE(mfg).!     *~
            *          ! PRR 11817.  Added errmsg if including    !     *~
            *          !   purch parts & no costing param selectd.!     *~
            * 04/07/92 ! PRR 11461. Rpt Optn to only list warnings! JDH *~
            * 01/17/94 ! PRR 12905, 12962 Now honors the price to ! JDH *~
            *          !   use flag in VENPRICE for purchase cost.!     *~
            *          ! PRR 12902 Now uses store list of five.   !     *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            * 12/20/99 ! Mod to change vendor's price to allow    ! CMG *~
            *          !     five decimal places.  (EWD0001)      !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "STCENGSB" (costset$,  /* Cost set                     */~
                            #02,       /* SYSFILE2 File UFB            */~
                            #03,       /* STCHNY   File UFB            */~
                            #04,       /* HNYMASTR File UFB            */~
                            #07,       /* STCBOMXF File UFB            */~
                            #08,       /* STCDETAL File UFB            */~
                            #09,       /* BOMMASTR File UFB            */~
                            #10,       /* RTEMASTR File UFB            */~
                            #11)       /* ENGMASTR File UFB            */

        dim                                                              ~
            a(1), b(1,12),               /* Std cost computation arrays*/~
            array$(490)3,                /* Work Array For Effectivity */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom$3,                       /* Bill Of Materials Id.      */~
            cat$(2,2)4,                  /* Part Category Range        */~
            company$50,                  /* Company Header             */~
            costs(12),                   /* Costs got from file        */~
            costset$8,                   /* Cost Set To Update         */~
            curr$4,                      /* Currency Code              */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dater$8,                     /*                            */~
            edtmessage$79,               /* Edit screen message        */~
            effdate$8,                   /* Effectivity Date           */~
            errormsg$79,                 /* Error message              */~
            est$1,                       /* Set At Estimate Flag       */~
            highdate$6,                  /* hi date                    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mc_on$1,                     /* Is Multi-Currency Active?  */~
            mc_tbl$1,                    /* Multi-Currency Table for PO*/~
            mdelete$1,                   /* Delete Misc Costs Flag     */~
            part$(2,2)25,                /* Part Number Range          */~
            partnr$25,                   /* Part Number                */~
            partdescr$32,                /* Part Number Description    */~
            pcost$(3)1,                  /* Purchase Price Sources     */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            primpric$1,                  /* Primary price for part cost*/~
            primvend$9,                  /* Primary Vendor For Part    */~
            printmsg$32,                 /* Print Only Problems Msg    */~
            printonly$1,                 /* Print Only Problems Option */~
            printtext$80,                /* Free text for print        */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rte$3,                       /* Routing Id.                */~
            set$8,                       /* Cost Set To Update         */~
            setdescr$32,                 /* Cost Set Description       */~
            stat$4,                      /* Statutory Currency Code    */~
            stdate$6,                    /* Start date for vendor price*/~
            store$(5)3,                  /* Store Codes                */~
            strdescr$32,                 /* Cost Set Description       */~
            temp(7),                     /* Work Variable              */~
            temp1(7),                    /* Work Variable              */~
            title$(4)50,                 /* Screen Column Headings     */~
            type$(1,2)3,                 /* Part Type Range            */~
            udescr$50,                   /* Update Flag Description    */~
            uflag$1,                     /* Update Flag                */~
            userid$3,                    /* Current User Id            */~
            zeros$96                     /* Zeroes                     */

        dim f1%(64)                      /* = 1 if READ was successful */

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
            * #01 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! STORNAME ! Store Master                             *~
            * #06 ! WORKAREA ! Work File                                *~
            * #07 ! STCBOMXF ! Standard Cost / BOM-RTE Cross Reference  *~
            * #08 ! STCDETAL ! Standard Cost Details                    *~
            * #09 ! BOMMASTR ! BOM relationship file                    *~
            * #10 ! RTEMASTR ! Product Routing File W/Alternate Routes  *~
            * #11 ! ENGMASTR ! Engineering Master File                  *~
            * #12 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * #13 ! RCVLINES ! Receiver Line Items                      *~
            * #14 ! ESTMASTR ! Estimate master file                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alternate key 1, keypos = 1, keylen = 24

            select #05, "STORNAME",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 3

            select #06, "WORKAREA",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 50,                                   ~
                         keypos = 1, keylen = 2

            select #12, "VENPRICE",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  59

            select #13, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos=    26, keylen =  52,                     ~
                        alt key 1, keypos =  1, keylen = 69

            select #14, "ESTMASTR",                                      ~
                        varc,     indexed,  recsize =  2000,             ~
                        keypos =   10, keylen =  8,                      ~
                        alt key  1, keypos =  18, keylen =  30, dup,     ~
                            key  2, keypos =  48, keylen =  25, dup

            call "OPENCHCK" (#01, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#05, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#12, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#13, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#14, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" ( blankdate$ )

            set$ = costset$ : setdescr$ = " "
            if date$ <> " " and date$ <> blankdate$ then L10000
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
*        Check for Multi-Currency
            mc_on$ = "N" : stat$ = " "
            readkey$ = "SWITCHS.CUR         "
            call "READ100" (#02, readkey$, f1%(2%))
            if f1%(2) <> 0% then get #2 using L09132, mc_on$, stat$, mc_tbl$
L09132:         FMT POS(21), CH(1), CH(4), POS(28), CH(1)

            str(line2$,62) = "STCENGSB: " & str(cms2v$,,8)
            title$(1) = "From"  :  title$(2) = "To  "
            title$(3) = "Price Definition For Purchased Parts..."
            title$(4) = "Structure Definition For Manufactured Parts..."
            zeros$ = all(hex(00))
            call "WORKOPEN" (#6, "IO   ", 10%, 1%)
            write #6, using L09260, "A", "All Parts In Range"
            write #6, using L09260, "O", "Only If Cost In Set=0, or BOM/RTE~
        ~ in set = blank"
            write #6, using L09260, "C", "Only Parts Not Already In Set"
            write #6, using L09260, "S", "Only If Data [cost>0, Effective B~
        ~OM] is derived."
L09260:     FMT CH(2), CH(48)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            errormsg$ = " "
        inputmode1
            gosub initialize_variables

            for fieldnr% = 1% to  11%
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
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% <  1% or fieldnr% > 15% then editpg1
            if fieldnr% =  8% or fieldnr% =  7% then editpg1
            if fieldnr% = 12% or fieldnr% = 11% then editpg1
            if fieldnr% > 11% then fieldnr% = fieldnr% - 2%
            if fieldnr% >  7% then fieldnr% = fieldnr% - 2%
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11210:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11210
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11210
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "COMPNAME" (12%, company$, u3%)
            page% = 0% : line% = 12345%
            select printer(134)
            call "SETPRNT" ("STC101", " ", 0%, 0%)
            up_uflag$ = uflag$/*Archive These Bits For Future Reference*/
            gosub datamake
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20200,         /* Part Number Range      */~
                              L20250,         /* Part Type Range        */~
                              L20300,         /* Part Category Rng      */~
                              L20350,         /* Cost Set To Update     */~
                              L20400,         /* Update Flag            */~
                              L20640,         /* Print Option           */~
                              L20430,         /* Cost Source (Purch)    */~
                              L20470,         /* Store Code             */~
                              L20510,         /* Effective Date         */~
                              L20550,         /* Delete Misc Flag       */~
                              L20600          /* Cost From Estimates?   */
            return

L20200: REM Def/Enable Part Number Range           PART$(1)
            if part$(1,1) <> " " or part$(1,2) <> " " then return
               part$(1,1) = "ALL"
            return

L20250: REM Def/Enable Part Type Range             TYPE$(1)
            if type$(1,1) <> " " or type$(1,2) <> " " then return
               type$(1,1) = "000" : type$(1,2) = "999"
            return

L20300: REM Def/Enable Part Category Range         CAT$(1)
            if cat$(1,1) <> " " or cat$(1,2) <> " " then return
               cat$(1,1) = "ALL"
            return

L20350: REM Def/Enable Cost Set To Update          SET$
            if set$ = " " then set$ = saveset$
            enabled% = 0%
            return

L20400: REM Def/Enable Update Flag                 UFLAG$
            if uflag$ = " " then uflag$ = up_uflag$
            return

L20430: REM Def/Enable Cost Source (Purch)         PCOST$()
            tran(str(pcost$()), " 0")replacing
            return

L20470: REM Def/Enable Store Code                  STORE$()
            if pcost$(1%) = "0" then enabled% = 0%
            return

L20510: REM Def/Enable Effective Date              EFFDATE$
            if effdate$ = " " or effdate$ = blankdate$ then effdate$ = date$
            return

L20550: REM Def/Enable Delete Misc Flag            MDELETE$
            if effdate$ = " " or effdate$ = blankdate$ then mdelete$ = "N"
            if mdelete$ = " " then mdelete$ = "D"
            return

L20600: REM Def/Enable Cost From Estimates?        EST$
            if est$ = " " then est$ = "N"
            return

L20640: REM Def/Enable Cost From Estimates?        PRINTONLY$
            if printonly$ = " " then printonly$ ="N"
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
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Part Number Range.",                                     ~
         "Enter Part Type Range. 001-499 Are Purchased, Rest Are Build.",~
         "Enter Part Category Range.",                                   ~
         "Select Cost Set To Be Updated.",                               ~
         "Leave Blank And Press RETURN To See Options Provided.",        ~
         "Enter 'Y' if Report Should ONLY List Parts with Problems.",    ~
         "Place 0,1,2 or 3 In Boxes. Search Order is 1,2,3 Until a Cost I~
        ~s Found. 0=skip",                                                ~
         "Select Inventory Stores To Retrieve Costs From.  List in Order ~
        ~of Search.",                                                     ~
         "Select Effectivity Date To Determine BOM and RTE Ids For Set.",~
         "'D'=DELETE/zero misc Details; 'Z'=ZERO Direct costs; 'N'=NO zer~
        ~oing",                                                           ~
         "'Y' To Force Cost To Last Estimated Cost, If One Is Found (No B~
        ~OM/RTE Linkage)"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") mdelete$, inpmessage$, cat$(), store$(), part$(),  ~
                      type$(), strdescr$, pcost$(), uflag$, udescr$,     ~
                      effdate$, est$, printonly$
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            *         P U R C H A S E D   P A R T S   L O G I C         *~
            *-----------------------------------------------------------*~
            * Contains main read loop, and purchased parts update area. *~
            *************************************************************

        datamake
            call "SHOSTAT" ("Updating Structures/Costs In Set: " & set$)
            plowkey$ = part$(2,1) : did_some% = 0%
            read_next_part : call "PLOWNEXT" (#4, plowkey$, 0%, f1%(4))
                if f1%(4) = 0 then flag_for_global
            if plowkey$ > part$(2,2) then flag_for_global
            get #4, using L31065, partnr$,partdescr$,cat$,primvend$,type$
L31065:     FMT CH(25),CH(32),POS(90),CH(4),POS(102),CH(9),POS(180),CH(3)
            srq = 1

            if cat$ <=cat$(2,1) then read_next_part
            if cat$ > cat$(2,2) then read_next_part
            if type$ < type$(1,1) then read_next_part
            if type$ > type$(1,2) then read_next_part
            if type$ < "001" or type$ > "499" then manuf_part_logic

            REM Alright, we buy it, so look for some costs...
            totcost = 0 : mat costs = zer
            printtext$ = "No Costs Found"
            problem% = 1%
            for i% = 1% to 3%
                convert i% to cs$, pic(#)
                on pos(pcost$()=cs$) gosub from_hnyquan,  /* Find Cost */~
                                           from_venprice, /* Find Cost */~
                                           from_rcvlines  /* Find Cost */
                if totcost > 0 then L31155
            next i%
L31155:     goto update_purchase_cost

        from_hnyquan
            REM Look through HNYQUAN to find cost...
            for s% = 1% to 5%
                if store$(s%) = " " then L31222
                    str(plowkey$,26%,3%) = store$(s%)
                    str(plowkey$,29%) = all(hex(00))
L31185:             call "PLOWNEXT" (#1, plowkey$, 28%, f1%(1%))
                        if f1%(1%) = 0% then L31222
                    get #1, using L31200, totcost, costs()
L31200:                 FMT POS(117), 13*PD(14,4)
                    if totcost = 0 then L31185
                        printtext$ = "INVENTORY ON HAND, STORE: " &      ~
                                     store$(s%) & " LOT: " &             ~
                                     str(readkey$,29%,6%)
                        problem% = 0%
                        return
L31222:         next s%
                return

        from_venprice
            REM Look through Price Catalog Entries to find cost...
            readkey$ = str(partnr$)
            triad% = 1% /* 1st of 3 sections for price - Flagged to use */
L31236:     call "PLOWNEXT" (#12, readkey$, 25%, f1%(12%))
                if f1%(12%) = 0% then prime_prim
            get #12 using L31242, primpric$
L31242:         FMT POS(162), CH(1)
            if primpric$ = "Y" then goto L31270 else goto L31236
          prime_prim /* Prime Key for Primary Vendor */
            readkey$ = str(partnr$) & str(primvend$)
            triad% = 2% /* 2nd section for price - Primary Vendor */
          plow_prim /* Plow through Primary Vendor */
            call "PLOWNEXT" (#12, readkey$, 34%, f1%(12))
                if f1%(12) <> 0 then L31270
            readkey$ = str(partnr$)
            triad% = 3% /* 3rd section for price - Any Vendor */
          plow_any /* Plow through Any Vendor */
            call "PLOWNEXT" (#12, readkey$, 25%, f1%(12))
                if f1%(12) = 0 then return
L31270:     get #12, using L31280, venprce, factor, nextprice, stdate$,   ~
                                  expdate$, curr$
L31280:         FMT POS(69), PD(14,5), POS(93), PD(14,4), PD(14,5), 2*CH(6), ~
                    POS(158), CH(4)              /* (EWD0001) */
            if curr$ = " " and mc_on$ = "Y" then curr$ = stat$
            if stdate$ = " " or stdate$ = blankdate$ then L31300
            if date < stdate$ or date > expdate$ then L31300
               venprce = nextprice
L31300:     if factor <= 0 or factor > 1000000 then factor = 1
            factor = round(factor,4)
            totcost = round(venprce/factor,4)
            if mc_on$ = "Y" and curr$ <> stat$ then gosub convert_curr   ~
                                               else temp = 0
            if temp <> -666 then L31318 /* No Exch Rate problem, Go on */
                on triad% goto prime_prim,/*Prob Flagged, Start Primary*/~
                               plow_prim, /*Prob Primary, Read Another */~
                               plow_any  /*Problem w/Any, Read Another */
L31318:     costs(defpur%) = totcost
            printtext$ = "CATALOG, VEN:" & str(readkey$,26,9)
            printtext$ = printtext$ & " VENPART:" & str(readkey$,35,25)
            problem% = 0%
        return

        from_rcvlines
            REM Look through Receivers to find last cost...
            highdate$ = blankdate$
            readkey$ = str(partnr$)
L31360:     call "PLOWALTS" (#13, readkey$, 1%, 25%, f1%(13))
                if f1%(13) = 0 then return
            get #13, using L31375, dater$, costr
L31375:     FMT POS(122), CH(6), POS(364), PD(14,7)
            if dater$ < highdate$ or dater$ = " " then L31360
                highdate$ = dater$
                call "DATEFMT" (dater$)
                totcost = round(costr,4)
                costs(defpur%) = costr
                printtext$ = "RCVR:" & str(key(#13,0),,16)
                printtext$ = printtext$&" VEN:"&str(key(#13,0),17,9)
                printtext$ = printtext$ & " DATE:" & dater$
                problem% = 0%
                goto L31360

        update_purchase_cost
            REM Real good.  Now update the standard costs file...
            call "READ101" (#3, partnr$, f1%(3))
                if f1%(3) = 0 then add_purchased  /*add to set*/
            get #3, using L31480, oldcost
            if uflag$ = "C" then read_next_part
            if uflag$ = "O" and oldcost > 0 then read_next_part
            if uflag$ = "S" and totcost = 0 then read_next_part
            if totcost < 0 then read_next_part
            call "CONVERT" (oldcost, 2.4, str(old$,,11))
            put #3, using L31480, totcost, zeros$, zeros$, costs()
L31480:     FMT POS(52), PD(14,4), 2*CH(96), 12*PD(14,4)
            put #3, using L32860, " ", " ", srq
            rewrite #3
            gosub update_detail
            goto read_next_part

        add_purchased
            if printtext$ = "No Costs Found" and uflag$ = "S" then       ~
                                                           read_next_part
            bom$, rte$ = " " : old$ = "n/a"
            gosub add_to_file
            gosub update_detail
            goto read_next_part

        update_detail
            readkey$ = str(partnr$) & hex(00)
            call "DELETE" (#7, readkey$, 25%) /*Clear any old BOM links*/
            call "DELETE" (#8, readkey$, 25%) /*Clear any old details*/

            REM Write a Detail line to preserve these facts in history...
            for i% = 1% to 12%
            if costs(i%) = 0 then L31710
            write #8 using L31700,partnr$,i%,printtext$,0,costs(i%),i%," "
L31700:     FMT CH(25), PIC(000), CH(40), 2*PD(14,4), BI(1), CH(255)
L31710:     next i%

            call "CONVERT" (totcost, 2.4, str(new$,,11))
            if old$ = new$ then return
               if str(new$,,1%) = " " then str(new$,,1%) = "*"
               did_some% = 1%
               gosub print_line
            return

        convert_curr  /* Convert Price to Statutory at current rate */
            call "CURRATSB" (curr$, mc_tbl$, date$, cur_factor, temp,    ~
                                                               errormsg$)
            if errormsg$ = " " then L31880
                errormsg$ = " "
                temp = -666 /* No Exchange Rate! */
                totcost = 0 /* Set to zero since we can't convert it */
                return
L31880:     totcost = round(totcost * cur_factor, 4)
            return

        REM *************************************************************~
            *     M A N U F A C T U R E D   P A R T S   L O G I C       *~
            *-----------------------------------------------------------*~
            * Sets up data for manufactured parts.                      *~
            *************************************************************

        manuf_part_logic
            printtext$ = " " : mat costs = zer
            problem% = 0%
            bom$, rte$ = " "
            highdate$  = blankdate$
            if est$ <> "Y" then L32390

                REM Logic to determine latest cost quoted...
                call "REDALT0" (#14, partnr$, 2%, f1%(14))
                     if f1%(14) = 0% then L32390 /* No estimates */
                goto L32180
L32150:         call "READNEXT" (#14, f1%(14))
                    if f1%(14) = 0% or key(#14,2) <> partnr$ then        ~
                                                     update_purchase_cost
L32180:         get #14, using L32190, cus$, temp(), temp1(), dater$, qty$
L32190:         FMT CH(9), POS(767), 7*PD(14,4), POS(1159), 7*PD(14,4),  ~
                    POS(1422), CH(6), POS(1854), CH(1)
                if dater$ < highdate$ or dater$ = " " then L32150
                   qty%, it% = 0%
                   convert qty$ to qty%, data goto L32240
L32240:            for i% = 1% to 7%
                     if temp(i%) <= 0 then L32280
                        if i% = qty% or qty% = 0% then it% = i%
                        if it% <> 0% then i% = 8%
L32280:            next i%
                   if it% = 0% then L32150   /* Say What?? */
                   highdate$ = dater$
                   call "DATEFMT" (dater$)
                   srq = temp(it%)
                   totcost = temp1(it%)
                   costs(defpur%) = totcost
                   printtext$ = "ESTIMATE:" & key(#14,0)
                   if cus$ = " " then cus$ = "NOT KNOWN"
                   printtext$ = printtext$ & " CUS:" & cus$
                   printtext$ = printtext$ & " DATE:" & dater$
                   problem% = 0%
                   goto L32150   /* Find Next Estimate This Part */

L32390:     if effdate$ = " " or effdate$ = blankdate$ then ~
                                      zero_this_level_costs
            printtext$ = "WARNING: No effective BOM"
            problem% = 1%
            call "READ100" (#11, str(partnr$) & "1001", f1%(11))
                if f1%(11) = 0% then update_manuf_cost
            get #11 using L32440, array$()
L32440:     FMT POS(30), 490*CH(3)
            bom$ = array$(index%)
            printtext$ = "WARNING: BOM " & bom$ & " Is Not On File"
            problem% = 1%
            readkey$ = str(partnr$) & str(bom$) & hex(00)
            call "PLOWNEXT" (#9, readkey$, 28%, f1%(9))
                if f1%(9) = 0% then bom$ = " "
                if f1%(9) = 0% then update_manuf_cost
            REM First record should be seq '  0', but just in case...
            if str(readkey$,29,3) = "  0" then get #9 using L32530, rte$
L32530:     FMT POS(87), CH(3)

            REM Validate That Route Is In Fact On File...
            printtext$ = "WARNING: No RTE Attached To BOM"
            problem% = 1%
            if rte$ = " " then update_manuf_cost
            printtext$ = "WARNING: RTE " & rte$ & " Is Not On File"
            problem% = 1%
            readkey$ = str(partnr$) & str(rte$)
            call "PLOWNEXT" (#10, readkey$, 28%, f1%(10))
                if f1%(10) = 0% then rte$ = " "
                if f1%(10) = 0% then update_manuf_cost
            printtext$ = "Updated based on BOM Effectivity Calendar"
            problem% = 0%

        update_manuf_cost
            REM Update BOM and Route ID In Main STC file Now...
            old$, new$ = "B:xxx R:xxx"
            totcost = 0
            str(new$,3,3) = bom$ : str(new$,9,3) = rte$
            call "READ101" (#3, partnr$, f1%(3))
                if f1%(3) = 0% then add_build
            get #3, using L32860, str(old$,3,3), str(old$,9,3), srq
            if old$ <> new$ then str(new$,6,1) = "*"
            get #3, using L32750, costs()
L32750:     FMT POS(252), 12*PD(14,4)
            mat b = con : mat a = b * costs
            oldcost = a(1)
            if uflag$ = "C" then read_next_part
            if uflag$ = "O" and str(old$,3,3) <> " " then read_next_part
            if uflag$ = "S" and bom$ = " " then read_next_part
            if uflag$ = "S" and rte$ = " " then read_next_part

            if old$ = new$ and srq > 0 then L33030
               if srq < .01 then srq = 1
               put #3 using L32860, bom$, rte$, srq
L32860:        FMT POS(38), CH(3), CH(3), PD(14,4)
               rewrite #3
               did_some% = 1%
               goto L32940

        add_build
            if uflag$ = "S" and bom$ = " " then read_next_part
            if uflag$ = "S" and rte$ = " " then read_next_part
            old$ = "n/a" : oldcost = 0
            gosub add_to_file
L32940:     gosub print_line

            REM Update BOM cross reference file...
            readkey$ = str(set$) & str(partnr$)
            call "READ101" (#7, readkey$, f1%(7))
                if f1%(7) <> 0% then delete #7
            if bom$ = " " and rte$ = " " then L33030
            write #7 using L33020,partnr$,bom$,set$,partnr$,rte$,set$
L33020:     FMT CH(25), CH(3), CH(8), CH(25), CH(3), CH(8)
L33030:     if oldcost = 0 then read_next_part /* No Misc Costs */

        zero_this_level_costs
            if mdelete$ = "N" then read_next_part
            call "READ101" (#3, partnr$, f1%(3))
                if f1%(3) = 0% then read_next_part
            get #3, using L32750, costs()
            mat b = con : mat a = b * costs
            oldcost = a(1)
            if oldcost = 0 then read_next_part /* No Misc Costs */

            REM Zero This level costs if requested...
            mat costs = zer
            readkey$ = partnr$
            REM This level costs supported by any details??
            call "PLOWNEXT" (#8, readkey$, 25%, f1%(8))
                if f1%(8) = 1% and mdelete$ = "Z" then read_next_part
                if f1%(8) = 1% then printtext$ = "'Misc' Details deleted"
            put #3, using L31480, 0, zeros$, zeros$, costs()
            rewrite #3
            if mdelete$ = "D" then call "DELETE" (#8, readkey$, 25%)
            did_some% = 1%
            old$, new$ = " "
            printtext$ = "Misc (This Level) costs cleared"
            problem% = 0%
            gosub print_line
            goto read_next_part

        add_to_file
            write #3 using L33340, partnr$, " ", hex(ffffffff), bom$,     ~
                     rte$, srq, totcost, zeros$, zeros$, costs(), bom$,  ~
                     rte$, srq, " ", eod goto read_next_part
L33340:     FMT CH(25), CH(8), CH(4), 2*CH(3), 2*PD(14,4), 2*CH(96),     ~
                12*PD(14,4), 2*CH(3), PD(14,4), CH(139)
            did_some% = 1%
            return

        flag_for_global
            gosub end_report
            errormsg$="Note:"&hex(8c)&"No data in cost set was changed"
            if did_some% = 0% then inputmode1

            REM Flag Cost Set For Global...
            plowkey$ = "STC.HDR." & set$
            call "READ101" (#2, plowkey$, f1%(2))
                if f1%(2) = 0 then L65000
            put #2 using L33490, "Y"
L33490:     FMT POS(420), CH(1)
            rewrite #2
            goto inputmode  /* Close incision and hit the bar */

        REM *************************************************************~
            *         R E P O R T  C O N T R O L   S E C T I O N        *~
            *-----------------------------------------------------------*~
            * Code used to produce the 'what is going on' report...     *~
            *************************************************************

        print_line
            if problem% = 0% and printonly$ = "Y" then return
            gosub page_heading
            print using L34520, partnr$, partdescr$, type$, old$, new$,   ~
                               printtext$
        return

        page_heading
            line% = line% + 1%
            if line% < 60% then return
               page% = page% + 1%  :  line% = 8%
               runtime$ = " "
               call "TIME" (runtime$)
               print page
               print using L34350, date$, runtime$, company$
               print using L34380, userid$, page%
               print
               print using L34410, set$, setdescr$, printmsg$
               print
               print using L34440
               print using L34460
               return

        end_report
            gosub page_heading
            print using L34490
            print "** END OF REPORT **"
            call "SETPRNT" ("STC101", " ", 0%, 1%)
            close printer
            return

L34350: %RUN DATE: ######## ########             ########################~
        ~####################################              STCENGSB:STC101

L34380: %BY: ###                                       STANDARD COST AUTO~
        ~ BUILD AUDIT TRAIL                                    PAGE:###

L34410: %  COST SET: ######## ################################   ########~
        ~########################

L34440:   %PART NUMBER               DESCRIPTION                      TYP~
        ~E         OLD          NEW               SOURCE OF DATA
L34460:   %------------------------- -------------------------------- ---~
        ~- -----------  ----------- --------------------------------------~
        ~----
L34490: %================================================================~
        ~=================================================================~
        ~==
L34520:   %######################### ################################ ###~
        ~# ###########  ########### ######################################~
        ~####

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
              on fieldnr% gosub L40250,       /* Part Number Range      */~
                                L40250,       /* Part Type Range        */~
                                L40250,       /* Part Category Range    */~
                                L40250,       /* Cost Set To Update     */~
                                L40250,       /* Update Flag            */~
                                L40250,       /* Print Option           */~
                                L40250,       /* Cost Source (Purch)    */~
                                L40250,       /* Store Code             */~
                                L40250,       /* Effective Date         */~
                                L40250,       /* Delete Misc Flag       */~
                                L40250        /* Cost From Estimates?   */
              str(pf$(3),63,1) = hex(84)
              goto L40280

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40250:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40280:     accept                                                       ~
               at (01,02),                                               ~
                  "Build Standard Cost Structures/Purchase Prices",      ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,26), fac(hex(ac)), title$(1)              , ch(25),~
               at (04,54), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (05,02), "Part Number Range",                          ~
               at (05,26), fac(lfac$( 1)), part$(1,1)           , ch(25),~
               at (05,54), fac(lfac$( 1)), part$(1,2)           , ch(25),~
                                                                         ~
               at (06,02), "Part Type Range",                            ~
               at (06,26), fac(lfac$( 2)), type$(1,1)           , ch(03),~
               at (06,54), fac(lfac$( 2)), type$(1,2)           , ch(03),~
                                                                         ~
               at (07,02), "Part Category Range",                        ~
               at (07,26), fac(lfac$( 3)), cat$(1,1)            , ch(04),~
               at (07,54), fac(lfac$( 3)), cat$(1,2)            , ch(04),~
                                                                         ~
               at (08,02), "Cost Set To Update",                         ~
               at (08,26), fac(lfac$( 4)), set$                 , ch(08),~
               at (08,35), fac(hex(84)),  setdescr$             , ch(32),~
                                                                         ~
               at (09,02), "Update Cost Set Only If",                    ~
               at (09,26), fac(lfac$( 5)), uflag$               , ch(01),~
               at (09,28), fac(hex(84)),  udescr$               , ch(50),~
                                                                         ~
               at (10,02), "List Only Problems?",                        ~
               at (10,26), fac(lfac$( 6)), printonly$           , ch(01),~
                                                                         ~
               at (12,02), fac(hex(ac)),  title$(3)             , ch(39),~
               at (13,04), "Cost Source(s)",                             ~
               at (13,26), fac(lfac$( 7)), pcost$(1)            , ch(01),~
               at (13,28), "On Hand Average",                            ~
               at (13,46), fac(lfac$( 7)), pcost$(2)            , ch(01),~
               at (13,48), "Price Catalog",                              ~
               at (13,64), fac(lfac$( 7)), pcost$(3)            , ch(01),~
               at (13,66), "Latest Receiver",                            ~
                                                                         ~
               at (14,04), "Inventory Store Codes: 1 -",                 ~
               at (14,31), fac(lfac$(8%)), store$(1%)           , ch(03),~
               at (14,36), "2 -",                                        ~
               at (14,40), fac(lfac$(8%)), store$(2%)           , ch(03),~
               at (14,45), "3 -",                                        ~
               at (14,49), fac(lfac$(8%)), store$(3%)           , ch(03),~
               at (14,54), "4 -",                                        ~
               at (14,58), fac(lfac$(8%)), store$(4%)           , ch(03),~
               at (14,63), "5 -",                                        ~
               at (14,67), fac(lfac$(8%)), store$(5%)           , ch(03),~
                                                                         ~
               at (16,02), fac(hex(ac)),  title$(4)             , ch(46),~
               at (17,04), "Flag Parts To Use The BOMs & RTEs That Are Ef~
        ~fective On",                                                     ~
               at (17,61), fac(lfac$( 9)), effdate$             , ch(08),~
                                                                         ~
               at (18,04), "Clear Miscellaneous (This Level) Costs If Fou~
        ~nd? (D/Z/N)",                                                    ~
               at (18,61), fac(lfac$(10)), mdelete$             , ch(01),~
                                                                         ~
               at (19,04), "Set Cost To Latest Estimate (via This Level) ~
        ~If Estimate Found?",                                             ~
               at (19,68), fac(lfac$(11)), est$                 , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40950
                  call "MANUAL" ("STCENGSB") : goto L40280

L40950:        if keyhit% <> 15 then L40980
                  call "PRNTSCRN" : goto L40280

L40980:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41170     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Return"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41130
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41130:     if fieldnr% > 1% then L41150
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41150:     return

L41170: if fieldnr% > 0% then L41260  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Update Costs"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41260:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50200,         /* Part Number Range      */~
                              L50250,         /* Part Type Range        */~
                              L50320,         /* Part Category Range    */~
                              L50370,         /* Cost Set To Update     */~
                              L50640,         /* Update Flag            */~
                              L51260,         /* Print Option           */~
                              L50690,         /* Cost Source (Purch)    */~
                              L50860,         /* Store Code             */~
                              L50940,         /* Effective Date         */~
                              L51140,         /* Delete Misc Flag       */~
                              L51200          /* Cost From Estimates?   */
            return

L50200: REM Test for Part Number Range            PART$(1)
                call "TESTRNGE"   (part$(1,1), part$(1,2), part$(2,1),   ~
                                   part$(2,2), errormsg$)
            return

L50250: REM Test for Part Type Range              TYPE$(1)
            if type$(1,1) < "000" then L50290
            if type$(1,2) > "999" then L50290
            if type$(1,1) <= type$(1,2) then return
L50290:         errormsg$ = "Invalid Range. Maximum 000 thru 999 Allowed"
                return

L50320: REM Test for Part Category Range          CAT$(1)
                call "TESTRNGE"   (cat$(1,1), cat$(1,2), cat$(2,1),      ~
                                   cat$(2,2), errormsg$)
            return

L50370: REM Test for Cost Set To Update           SET$
            plowkey$   = "STC.HDR." & set$
            setdescr$ = hex(06) & "Select Cost Set"
            call "PLOWCODE" (#2, plowkey$, setdescr$, 8%, 0.30, onfile%)
                if onfile% = 1% then L50440
                errormsg$ = "Please select a valid Cost Set"
                return
L50440:     set$ = str(plowkey$,9)
            call "PUTPAREN" (setdescr$)
            call "STCFOPEN" (set$, "SS    ", #2, errormsg$,              ~
                             #3, #8, #3, #3, #3, #3)

            REM Retrieve control flag to determine set status...
            call "GETUFBS1" addr(#3, onfile%)
                if onfile% <> 0% then L50540
                errormsg$ = "Sorry, Data For Cost Set Has Been Deleted"
                return
L50540:     plowkey$ = "STC.HDR." & set$
            call "READ100" (#2, plowkey$, f1%(2))
            get #2 using L50570, frozen$, defpur%
L50570:         FMT POS(433), CH(6), POS(442), BI(1)
            if defpur% < 1% or defpur% > 12% then defpur% = 1%
            saveset$ = set$
            if frozen$ = " " then return
                errormsg$ = "Sorry, Set Is Frozen, So Can't Be Updated."
                return

L50640: REM Test for Update Flag                  UFLAG$
            call "GETCODE" (#6, uflag$, udescr$, 1%, .48, f1%(6))
                if f1%(6) = 0% then errormsg$ = "Please Enter Valid Code"
            return

L50690: REM Test for Cost Source (Purch)          PCOST$()
            tran(str(pcost$()), "0 ")replacing
            for i% = 1% to 3%
                if pcost$(i%) = "0" then L50830
                if pos("0123"=str(pcost$(i%))) <> 0% then L50770
                    errormsg$ = "Please Enter '0', '1', '2' or '3':"
                    errormsg$ = errormsg$ & " " & pcost$(i%)
                    return
L50770:         for j% = 1% to 3%
                    if j% = i% then L50820
                    if pcost$(j%) <> pcost$(i%) then L50820
                    errormsg$ = "Can't Occur In Two Boxes: " & pcost$(i%)
                    return
L50820:         next j%
L50830:     next i%
            if type$(1,2) < "200" or type$(1,1) > "499" then return
                for i% = 1% to 3%
                    if pcost$(i%) <> "0" then return
                    next i%
                    errormsg$ = "Purchased parts (Part Type 200-499) se"&~
                                "lected!  Cannot skip all 3 cost sources."
                return

L50860: REM Test for Store Code                   STORE$()
            if enabled% <> 0% then L50892
                store$(), strdescr$ = " "
                return
L50892:     for s% = 1% to 5%
                if store$(s%) = " " then L50916
                     convert s% to temp$, pic(#)
                     strdescr$ = hex(0684) & "Choose Store - " & temp$
                     call "GETCODE" (#5, store$(s%), strdescr$, 1%, 0,   ~
                                                                 f1%(5%))
                         if f1%(5%) <> 0% then L50916
                     errormsg$ = "Invalid Entry for Store - " & temp$
                     return
L50916:         next s%
            return

L50940: REM Test for Effective Date               EFFDATE$
            if effdate$ <> " " and effdate$ <> blankdate$ then L50980
                index% = 0%
                return
L50980:     call "DATEOK" (effdate$, 0%, errormsg$)
                if errormsg$ <> " " then return
            temp$ = effdate$
            call "DATUNFMT" (temp$)
            call "PIPINDEX" (#2, temp$, index%, u3%)
                if u3% = 0% then return
            on u3% goto L51060,L51070,L51050,L51110,L51050,L51050,L51050,L51110
L51050:     errormsg$ = "Unknown Error in PIP Subroutine"        : return
L51060:     errormsg$ = "There is no open month in SYSFILE2"     : return
L51070:     if index% = 1%                                               ~
                then errormsg$ = "Date falls prior to planning calendar" ~
                else errormsg$ = "Date falls beyond planning calendar"
            return
L51110:     errormsg$ = "Date cannot be analyzed by PIP Subroutine"
            return

L51140: REM Test for Delete Misc Flag             MDELETE$
            if mdelete$ = " " then mdelete$ = "Z"
            if pos("DZN"=mdelete$) <> 0% then return
                errormsg$ = "Please Enter 'D', 'Z' or 'N'"
                return

L51200: REM Test for Cost From Estimates?         EST$
            if est$ = " " then est$ = "N"
            if pos("YN"=est$) <> 0% then return
                errormsg$ = "Please Enter 'Y' or 'N'"
                return

L51260: REM Test for Cost From Estimates?         PRINTONLY$
            printmsg$ = " "
            if printonly$ = "N" then return
                if printonly$ = "Y" then L51320
                     errormsg$ = "Please Enter 'Y' or 'N'"
                     return
L51320:         printmsg$ = "Listing Only Parts with Problems"
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
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            end