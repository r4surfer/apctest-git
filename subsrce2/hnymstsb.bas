        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y  M     M   SSSS  TTTTT   SSSS  BBBB  *~
            *  H   H  NN  N  Y   Y  MM   MM  S        T    S      B   B *~
            *  HHHHH  N N N   YYY   M M M M   SSS     T     SSS   BBBB  *~
            *  H   H  N  NN    Y    M  M  M      S    T        S  B   B *~
            *  H   H  N   N    Y    M     M  SSSS     T    SSSS   BBBB  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYMSTSB - Display HNYMASTR data.                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1990, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/30/90 ! ORIGINAL  (cloned from HNYINPUT)         ! FMZ *~
            * 03/19/92 ! PRR 12247.  PF25 Part Text Added.        ! JDH *~
            * 11/16/92 ! Change display Screens to current        ! RJH *~
            *          !  HNYINPUT displays.                      !     *~
            * 05/12/93 ! Added Count Rate.                        ! JDH *~
            * 09/29/93 ! PF2 Alternates is hilit if alts on file. ! JDH *~
            * 03/30/94 ! Added PFKey prompt and call to ECRINQSB, ! LDJ *~
            *          ! Removed lonely call to HNYAPPND.         !     *~
            *----------+------------------------------------------+-----*~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "HNYMSTSB" (part$,                                           ~
               #1, /* SYSFILE2 ! System Default Information File      */ ~
               #2, /* HNYMASTR ! Inventory Master File                */ ~
               #3, /* CATEGORY ! Inventory Category File              */ ~
               #4, /* GLMAIN   ! General Ledger Main File             */ ~
               #5, /* HNYALTRS ! Inventory Alternate Parts File       */ ~
               #6, /* VENDOR   ! Vendor Master File                   */ ~
               #7, /* HNYPROC  ! Procurement History File             */ ~
               #8, /* VENPRICE ! Vendor Price Catalogue File          */ ~
               #9, /* HNYGENER ! Generic Part Cross Reference File    */ ~
              #12, /* PIPMASTR ! Planned Inventory Position Master    */ ~
              #14, /* ENGMASTR ! Engineering Master Filer             */ ~
              #15, /* PIPIN    ! PIPIN File                           */ ~
              #16, /* PIPOUT   ! PIPOUT File                          */ ~
              #17, /* HNYQUAN  ! Inventory Quantities File            */ ~
              #18, /* SFCUM2   ! Cumulative Forecast File             */ ~
              #19, /* TXTFILE  ! System Text File                     */ ~
              #20, /* GENCODES ! General Codes File                   */ ~
              #21) /* BOMMASTR ! Bill of Materials Master file        */ ~

        dim                                                              ~
            a_descrs$(19)20,             /* Account Descriptors        */~
            a_fnxref$19, a_line$19,      /* Acct/Field # X-Refs        */~
            acct$(19)12,                 /* General Ledger Accounts    */~
            acct_descr$(19)30,           /*  (in seq of record layout) */~
            altdesr$(100)34,             /* Alternate Part Descr.      */~
            altpart$(100)25,             /* Alternate Part #s          */~
            altrmsg$79,                  /* Generic Part Message       */~
            atch$10,                     /* ATC Horizon in days        */~
            bin$8,                       /* Bin Location               */~
            bclass$(2)3,                 /* Buyer/planner Classes      */~
            bcdesc$(2)32,                /* Buyer/planner Classes      */~
            category$4,                  /* Inventory Category code    */~
            categorydescr$32,            /* Description of Category    */~
            class$4, class_descr$30,     /* Free (User Definable) Field*/~
            conversion$10,               /* Conversion Factor (P==>S)  */~
            costtype$1,                  /* Inventory Costing Method   */~
            costtypedescr$32,            /* Inventory Costing Method   */~
            createdate$8,                /* Date Record was created    */~
            cursor%(2),                  /* Cursor Location            */~
            cyclecat$1,                  /* Cycle Count Category       */~
            cycledate$8,                 /* Last Count Date            */~
            date$8,                      /* System Calendar Date       */~
            description$32,              /* Description                */~
            draw_ref$16, draw_size$2,    /* Drawing Reference & Size   */~
            ecrpfk$14,                   /* ECR Inquiry PFKey Prompt   */~
            edttran$80,                  /* Map to Field # from Cursor */~
            errormsg$79,                 /* Error message text string  */~
            fac25$1,                     /* PF(25) FAC                 */~
            gendescr$30,                 /* Generic Part Designator Des*/~
            generic$16,                  /* Generic Part Designator    */~
            hdr$(1)79,                   /* PLOWCODE Argument          */~
            i$(24)80,                    /* Screen Image               */~
            incl(1), incl$(1)1,          /* PLOWCODE Argument          */~
            indicator$4,                 /* Special/obsolete Indicator */~
            infomsg$79, inpmessage$79,   /* Informative message text   */~
            leadtime$10, lt_msg$30,      /* Lead Time (Days)           */~
            line2$79,                    /* Screen Underline           */~
            llcode$4,                    /* Low Level Code             */~
            lottrack$1,                  /* Subject to Lot Tracking?   */~
            oldlottrack$1,               /* Saved LOTTRACK Flag        */~
            message$30,                  /* Message for Line Items scrn*/~
            minsoqty$10, minsoinc$10,    /* Min SO Quantity & Increment*/~
            misc$79,                     /* Miscellaneous Junk Variable*/~
            modby$3,                     /* Record Modified By         */~
            moddate$8,                   /* Record Modified Date       */~
            negqty$1,                    /* Can Pools Go Negative      */~
            onfileptype$3,               /* Part Type On File          */~
            ovrrecpct$8, ovrrecqty$10,   /* Max. %, units over receipt */~
            ovrshppct$8, ovrshpqty$10,   /* Max. %, units overshipment */~
            pansize$10,                  /* Mfg/Purchase Increment     */~
            part$25,                     /* Part Number being input.   */~
            partkey$60,                  /* Part Read key              */~
            pfkeys$(4)17,                /* PFKeys enabled in tabular  */~
            pfkey$32,                    /* PFKeys enabled             */~
            pfmsg$(3)79,                 /* PFKeys Prompts             */~
            plowkey$100,                 /* Key for PLOWCODE calls     */~
            pmoq$10,                     /* Minimum Order Quantity     */~
            potency$10,                  /* Standard Potency Factor    */~
            ptype$3,                     /* Part Type (numeric)        */~
            ptypedescr$32,               /* Type Description           */~
            readkey$50,                  /* Multi-purpose Read Key     */~
            safety$10,                   /* Safety Stock               */~
            serial$1,                    /* Serial Numbered Part       */~
            seqnr$8,                     /* Converted no. of Alternates*/~
            set$8,                       /* Cost Set ID                */~
            prcunit$4,                   /* Sales Unit of Measure      */~
            prcunitdescr$32,             /* Sales Unit of Measure Descr*/~
            sopriority$1,                /* Sales Order Plan Priority  */~
            stkunit$4,                   /* Stock Unit of Measure      */~
            stkunitdescr$32,             /* Stock U-O-M Description    */~
            taxable$1,                   /* Part Taxable Flag          */~
            tdate$6,                     /* Todays date unformated     */~
            text$(113,1)70,              /* Text Sizing Array          */~
            textid$4,                    /* X-ref to Part Text         */~
            textmsg$79,                  /* Message to TXTDSPLY        */~
            tfac$(20,2)1,                /* Fac's for Tabular Screens  */~
            tttle$(4,2)55,               /* Titles for Tabular Screens */~
            uom$1, uomconv$1,            /* On-file? (Y/N)             */~
            userid$3,                    /* Current User ID            */~
            vencode$9,                   /* Vendor Code                */~
            vendname$32,                 /* Vendor's Name (Internal)   */~
            vfs$200                      /* Variable Fields            */

        dim f2%(64),                     /* File Status Flags          */~
            fs%(64),                     /* File Status Flags          */~
            f1%(64),                     /* Record-on-file Flags       */~
            rslt$(64)20                  /* Returned from "OPENFILE"   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * SETS DATES, TRANSLATION TABLES, ETC                       *~
            *************************************************************

            date$, tdate$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)
            str(line2$,62%) = "HNYMSTSB: " & str(cms2v$,,8%)
            init(hex(01)) edttran$

            a_fnxref$ = hex(010203040513060708090a0b0c0d0e0f101112)
            a_line$   = hex(060708090a0c0e0f101112130e0f101112130b)
            a_descrs$( 1%) = "Purchasing Source"
            a_descrs$( 2%) = "Work-in-Process Source"
            a_descrs$( 3%) = "Inventory Assets"
            a_descrs$( 4%) = "Cost of Goods Sold"
            a_descrs$( 5%) = "Sales Distribution"
            a_descrs$( 6%) = "Inventory Adjustments"
            a_descrs$( 7%) = "First Variance"
            a_descrs$( 8%) = "Second Variance"
            a_descrs$( 9%) = "Third Variance"
            a_descrs$(10%) = "Fourth Variance"
            a_descrs$(11%) = "Fifth Variance"
            a_descrs$(12%) = "Sixth Variance"
            a_descrs$(13%) = "Seventh Variance"
            a_descrs$(14%) = "Eighth Variance"
            a_descrs$(15%) = "Ninth Variance"
            a_descrs$(16%) = "Tenth Variance"
            a_descrs$(17%) = "Eleventh Variance"
            a_descrs$(18%) = "Twelfth Variance"
            a_descrs$(19%) = "Sales Discounts"

            tttle$(1%,1%) = "(1)Start Over(2)Col 1(4)Line Above(16)Edit M~
        ~ode"
            tttle$(2%,1%) = "(1) Start Over (2) First (3) Last (4) Prev"
            tttle$(2%,2%) = "(5) Next       (6) Down  (7) Up  (16) Return"
            tttle$(3%,1%) = "Supply Requested Items and Press (ENTER),  o~
        ~r PRESS"
            tttle$(3%,2%) = "(1)To Exit Insert Mode (2)Column One (4)Line~
        ~ Above"
            tttle$(4%,1%) = "Press (ENTER) to Delete Flashing Line, or"
            tttle$(4%,2%) = "Press (1) To Exit Delete Mode"

            pfkeys$(1%) = hex(000102040f10ffffffffffffffffffffff)
            pfkeys$(2%) = hex(00010203040506070f10ffffffffffffff)
            pfkeys$(3%) = hex(000102040fffffffffffffffffffffffff)
            pfkeys$(4%) = hex(00010fffffffffffffffffffffffffffff)

            gosub load_calendar

*        See if UOM and UOMCONV files in place; also Part Class
            uom$, uomconv$ = "N"
            readkey$ = "UOM"
            call "PLOWNEXT" (#20, readkey$, 9%, f1%(20%))
            if f1%(20%) = 1% then uom$ = "Y"

            readkey$ = "UOMCONV"
            call "PLOWNEXT" (#20, readkey$, 9%, f1%(20%))
            if f1%(20%) = 1% then uomconv$ = "Y"

            readkey$ = "PARTCLASS"
            call "PLOWNEXT" (#20, readkey$, 9%, f1%(20%))

            readkey$ = "PARTTYPE "
            call "PLOWNEXT" (#20, readkey$, 9%, f1%(20%))
            if f1%(20%) = 1% then   part_type_chk$ = "Y"

        REM *************************************************************~
            *            M A I N   I N P U T   R O U T I N E            *~
            * --------------------------------------------------------- *~
            * This enters all the header information for a part number. *~
            * If part is on file, go directly to Edit Mode.             *~
            *************************************************************

        inputmode
            description$, vencode$, vendname$, pmoq$, ptype$, generic$,  ~
               stkunit$, prcunit$, bclass$(), conversion$, costtype$,    ~
               category$, taxable$, indicator$, seqnr$, leadtime$,       ~
               categorydescr$, ptypedescr$, altpart$(), errormsg$,       ~
               altdesr$(), draw_size$, draw_ref$, acct$(), acct_descr$(),~
               costtypedescr$, gendescr$, safety$, pansize$, cyclecat$,  ~
               cycledate$, onfileptype$, sopriority$, atch$, altrmsg$,   ~
               stkunitdescr$, prcunitdescr$, bin$, bcdesc$(), lottrack$, ~
               negqty$, serial$, class$, class_descr$, potency$, vfs$,   ~
               lt_msg$, ovrrecpct$, ovrrecqty$, minsoqty$, minsoinc$,    ~
               oldlottrack$, infomsg$, ovrshppct$, ovrshpqty$, ecrpfk$,  ~
               createdate$, moddate$, modby$                    = " "
            maxlines%, keyhit%, dfltsedit% = 0%
            textid$ = all(hex(ff))
            alts% = 0%

            gosub dataload

        first_screen
L10260:     gosub display_page1
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub display_altrs
                  if keyhit%  =  5% then       second_screen
                  if keyhit%  =  8% then gosub see_procurement_his
                  if keyhit%  = 10% then gosub call_stcmatrx
                  if keyhit%  = 11% then gosub view_ecr_info
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       datasave
                  if keyhit%  = 25% then gosub see_text
                  if keyhit% <>  0% and keyhit% <> 29% then L10260
                goto L10260

        second_screen
L10510:     gosub display_page2
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub display_altrs
                  if keyhit%  =  4% then       first_screen
                  if keyhit%  =  5% then       third_screen
                  if keyhit%  =  8% then gosub see_procurement_his
                  if keyhit%  = 10% then gosub call_stcmatrx
                  if keyhit%  = 11% then gosub view_ecr_info
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       datasave
                  if keyhit%  = 25% then gosub see_text
                  if keyhit% <>  0% and keyhit% <> 29% then L10510
                goto L10510

        third_screen
L11010:     gosub display_page3
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub display_altrs
                  if keyhit%  =  4% then       second_screen
                  if keyhit%  =  5% then       display_vf
                  if keyhit%  =  8% then gosub see_procurement_his
                  if keyhit%  = 10% then gosub call_stcmatrx
                  if keyhit%  = 11% then gosub view_ecr_info
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       datasave
                  if keyhit%  = 25% then gosub see_text
                  if keyhit% <>  0% and keyhit% <> 29% then L11010
                goto L11010

        display_vf
            keyhit% = 99%
            str(line2$,,60%) = "This Part: " & part$
L12020:     call "VFINPSUB" ("HNYMASTR", "D", "Manage Parts Master File",~
                             str(line2$,,60%), "YN", vfs$, keyhit%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       third_screen
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       datasave
                  if keyhit%  = 99% then       third_screen
                  if keyhit% <>  0% and keyhit% <> 29% then L12020
                goto L11010

        call_stcmatrx
            set$ = " "
            call "STCMATRX" (part$, set$, #1, #2)
            return

        display_altrs
            message$ = "ALTERNATE PARTS"
            infomsg$ = " "
            line%, screenline% = 0

            screen% = 2%
            init (hex(86)) tfac$()
L13190:     gosub alternate_display
                  if keyhit1%  =  1% then gosub startover
                  if keyhit1%  =  2% then line% = 0%
                  if keyhit1%  =  3% then line% = max(0%,maxlines%-15%)
                  if keyhit1%  =  4% then line% = max(0%,line% - 15%)
                  if keyhit1%  =  5% then line% = min(line% + 15%,       ~
                                                  max(0%,maxlines% - 20%))
                  if keyhit1%  =  6% then line% = max(0%,line% - 1%)
                  if keyhit1%  =  7% then line% = min(line% + 1%,        ~
                                                  max(0%,maxlines% - 20%))
                  if keyhit1%  = 16% then       return
                goto L13190

        REM *************************************************************~
            *               V I E W   T E X T                           *~
            * --------------------------------------------------------- *~
            * Allow viewing of Part Text.                               *~
            *************************************************************
        see_text
            textmsg$ = "Part " & part$ & ", '" & description$ & "'"
            call "TXTDSPLY" (#19, f2%(19%), "001", textmsg$, textid$,    ~
                                                                 text$())
            return

        deffn'040(txt$)
            fac25$ = hex(84) /* Highlight PF(25)Manage Text */
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then fac25$ = hex(8c) /* Dim PF(25)Manage Text */
            return

        REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *-----------------------------------------------------------*~
            * Updates the Inventory Master & Supporting Part Files.     *~
            *************************************************************

        datasave
          goto L65000


        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--harder to screw it up.   *~
            *************************************************************

        startover
            keyhit1% = 2%  /* Put msg area on bottom of screen  */
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
                if keyhit1% <> 0% then startover
                     return clear all
                     goto inputmode

        REM *************************************************************~
            *            R E A D   D A T A   F R O M   F I L E          *~
            *-----------------------------------------------------------*~
            * Loads the part from the main file and formats the entries.*~
            * Also loads the Suppprting part files into arrays.         *~
            *************************************************************
        dataload
            temp%, maxlines%, dfltsedit% = 0%
            call "READ100" (#2, part$, f1%(2%))
            if f1%(2%) = 0% then L65000

            get #2, using L31020, description$, generic$, stkunit$,       ~
                        prcunit$, conversion, category$, llcode$,        ~
                        textid$,vencode$,cyclecat$,cycledate$,cyclelock$,~
                        count_rate, taxable$, atch%, lottrack$, serial$, ~
                        negqty$, class$, draw_size$, draw_ref$, bin$,    ~
                        indicator$, leadtime$, ptype$, pmoq$, bclass$(1),~
                        potency, ovrrecpct, ovrrecqty, createdate$,      ~
                        moddate$, modby$, allocdflt$, demtypedflt$,      ~
                        costtype$, bclass$(2), safety, pansize,          ~
                        sopriority$, acct$(), vfs$, minsoqty, minsoinc,  ~
                        ovrshppct, ovrshpqty

            plowkey$ = part$  /* Test if alternates are on file */
            call "PLOWNEXT" (#5, plowkey$, 25%, alts%)

*        Retrieve descriptions from main files
            if fs%(3%) = 0% then                                         ~
                     call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "DESCRIBE" (#3, category$, categorydescr$, 1%, f1%(3%))
            if fs%(6%) = 0% then                                         ~
                     call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "DESCRIBE" (#6, vencode$, vendname$, 1%, f1%(4%))
            readkey$ = "BYCLASSES" & bclass$(1%)
            call "DESCRIBE" (#20, readkey$, bcdesc$(1%), 1%, f1%(20%))
            readkey$ = "PSCLASSES" & bclass$(2%)
            call "DESCRIBE" (#20, readkey$, bcdesc$(2%), 1%, f1%(20%))
            readkey$ = "UOM      " & stkunit$
            call "DESCRIBE" (#20, readkey$, stkunitdescr$, 1%, f1%(20%))
            readkey$ = "UOM      " & prcunit$
            call "DESCRIBE" (#20, readkey$, prcunitdescr$, 1%, f1%(20%))
            call "DATEFMT" (cycledate$)
            readkey$ = "PARTCLASS" & class$
            call "DESCRIBE" (#20, readkey$, class_descr$, 1%, f1%(20%))

           ptype% = 0% : convert ptype$ to ptype%, data goto L30500
           convert ptype% to ptype$, pic(000)
L30500:    if part_type_chk$ <> "Y" then L30506                           ~
                                    else readkey$ = "PARTTYPE " & ptype$
           call "DESCRIBE" (#20,  readkey$, ptypedescr$, 1%, f1%(20%))
           if f1%(20%) = 1% then L30510
L30506:        call "HNYTYPE" (ptype%, ptypedescr$, 1%)

L30510:    onfileptype$ = ptype$
           atch = atch%
           call "CONVERT" (atch   , -0.001, atch$)
           call "CONVERT" (potency,   -4.2, potency$)
           call "CONVERT" (ovrrecpct, -2.4, ovrrecpct$)
           call "CONVERT" (ovrrecqty, -2.2, ovrrecqty$)
           call "CONVERT" (ovrshpqty, -2.2, ovrshpqty$)
           call "CONVERT" (ovrshppct, -2.4, ovrshppct$)
           call "CONVERT" (count_rate, -2.2, count_rate$)

*         See if part has variable leadtimes.  Inform if so.
            if fs%(14) = 0% then                                         ~
                     call "OPENCHCK" (#14,fs%(14),f2%(14),0%,rslt$(14))
            call "READ100" (#14, str(part$,,25) & "2001", f1%(14))
            if f1%(14) = 1% then lt_msg$ = "Part has variable lead times"

*        Format numbers and Accounts for screen display
            pmoq = 0 : convert pmoq$ to pmoq, data goto L30640
L30640:     call "CONVERT" (pmoq, 2.2, pmoq$)
            call "CONVERT" (safety    , 2.2, safety$)
            call "CONVERT" (pansize   , 2.2, pansize$)
            call "CONVERT" (minsoqty, 2.2, minsoqty$)
            call "CONVERT" (minsoinc, 2.2, minsoinc$)
            call "CONVERT" (conversion, 0.7, conversion$)
            call "DATEFMT" (createdate$)
            call "DATEFMT" (moddate$)
            if fs%(4) = 0% then                                          ~
                           call "OPENCHCK" (#4,fs%(4),f2%(4),0%,rslt$(4))
            for a% = 1% to 19%
                call "DESCRIBE" (#4, acct$(a%), acct_descr$(a%), 0%, u3%)
                call "GLFMT"    (acct$(a%))
            next a%

            if costtype$ = " " then costtype$ = "R"
            gosub L34210

*        Get Generic Part Description
            call "PLOWALTS" (#9,str(generic$,,16) & hex(00),1%,16%,f1%(9))
            if f1%(9) = 0% then L30860
                get #9, using L30820, gendescr$
L30820:              FMT XX(41), CH(30)

L30860:         partkey$ = part$  /* Load Alternate part numbers */
                temp%    = 0%
L30880:         call "PLOWNEXT" (#5, partkey$, 25%, f1%(5))
                if f1%(5) = 0% then L30970
                     temp%, maxlines% = temp% + 1%
                     get #5, using L30920, altpart$(temp%)
L30920:                   FMT XX(33), CH(25)
                     call "DESCRIBE" (#2, altpart$(temp%),               ~
                                          altdesr$(temp%), 1%, f1%(2))
                     goto L30880

L30970: REM Set PF Key prompt for ECR Inquiry (if any ECRs)
            ecrpfk$ = "(11)"
            call "ECRINQSB" ("C",        /* "C" to Check for ECR Info  */~
                                         /*    and return PFKey Prompt */~
                             part$,      /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                                         /* OUT: Formatted PFKey Prompt*/~
                                         /*    Will be BLANK if no ECRs*/~
                             #01,        /* SYSFILE2                   */~
                             #02)        /* HNYMASTR                   */

            return

*        Format for Inventory Master File (HNYMASTR)
L31020:     FMT XX(25),          /* Part Number                        */~
                CH(32),          /* Description                        */~
                CH(16),          /* Vendor Part Number                 */~
                2*CH(4),         /* Sales, Pricing UOMs                */~
                PD(14,7),        /* Conversion factor (P==>S)          */~
                CH(4),           /* Category code                      */~
                CH(4),           /* Lower Level Code                   */~
                CH(4),           /* Text ID X-Ref                      */~
                CH(9),           /* Vendor code                        */~
                CH(1),           /* Cycle count category               */~
                CH(6),           /* Last Count date                    */~
                CH(1),           /* ABC Lock Flag                      */~
                PD(14,4),        /* Count Rate                         */~
                CH(1),           /* Part Taxable Flag                  */~
                BI(2),           /* ATC Horizon days                   */~
                CH(1),           /* Lot Tracking Flag                  */~
                CH(1),           /* Serial Number Flag                 */~
                CH(1),           /* Negative Quantity Flag             */~
                CH(4),           /* Class                              */~
                CH(2), CH(16),   /* Drawing Size and Reference         */~
                CH(8),           /* Primary Bin Location               */~
                XX(3),           /* Filler                             */~
                CH(4),           /* Special/obsolete field             */~
                CH(10),          /* Lead time (days)                   */~
                CH(3), XX(7),    /* Type                               */~
                CH(10),          /* MOQ                                */~
                CH(3),           /* Buyer/planner 1                    */~
                PD(14,4),        /* Standard Potency                   */~
                2*PD(14,4),      /* Max. Receipt over Order % & units  */~
                CH(6),           /* Created Date                       */~
                CH(6),           /* Modified Date                      */~
                CH(3),           /* Last modified By                   */~
                CH(1),           /* Allocation Default Flag            */~
                CH(1),           /* Demand Type Default Flag           */~
                XX(63),          /* Filler                             */~
                CH(1),           /* Cost selection                     */~
                XX(1),           /* Filler                             */~
                CH(3),           /* Buyer/planner 2                    */~
                XX(6),           /* Filler                             */~
                PD(14,4),        /* Safety stock level                 */~
                PD(14,4),        /* MFG/PUR Increment (Pansize)        */~
                CH(1),           /* SO Priority assigned               */~
                19*CH(9),        /* Account Numbers                    */~
                CH(200),         /* Variable Fields                    */~
                2*PD(14,4),      /* Min SO Qty & Increment             */~
                2*PD(14,4)       /* Overshipment % & units             */

        rem**************************************************************~
            *          l o a d   c a l m a s t r    d a t a             *~
            *-----------------------------------------------------------*~
            * get planning calendar index for today.                    *~
            *************************************************************

        load_calendar
            today% = 0%
            call "PIPINDEX" (#1, " ", today%, err%)
            if err% = 0% then return

            call "ASKUSER" (0%, "***INVALID PLANNING CALENDAR***",       ~
            "Your Planning/Production Calendar is Missing or Invalid!",  ~
            "Please Press RETURN to Acknowledge this message & EXIT,",   ~
            "Then run the Procedure PROCCAL to correct the situation!")
            goto exit_program


        REM *************************************************************~
            *         D E S C R I B E   C O S T   T Y P E               *~
            *************************************************************

L34210:     costtypedescr$=" "
            if costtype$ ="R" then costtypedescr$="(Actual Value, LIFO)"
            if costtype$ ="X" then costtypedescr$="(Actual LIFO/Adj Act)"
            if costtype$ ="A" then costtypedescr$="(Average Cost)"
            if costtype$ ="B" then costtypedescr$="(Mod. Average Cost)"
            if costtype$ ="S" then costtypedescr$="(Standard LIFO)"
            if costtype$ ="F" then costtypedescr$="(Fixed Standard)"
            if costtype$ ="L" then costtypedescr$="(Last Cost)"
            if costtype$ ="M" then costtypedescr$="(Manual Cost)"
            if costtype$ ="T" then costtypedescr$="(Standard FIFO)"
            if costtype$ ="P" then costtypedescr$="(Actual Value, FIFO)"
            if costtype$ ="Y" then costtypedescr$="(Actual FIFO/Adj Act)"
            return

        REM *************************************************************~
            *      1 S T   P A G E   O F   I N P U T   S C R E E N      *~
            *-----------------------------------------------------------*~
            * Gets all the information on the first page of the display.*~
            *************************************************************

        display_page1
            gosub set_pf1

L40230:     accept                                                       ~
               at (01,02), "MANAGE INVENTORY PART MASTER FILE",          ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), infomsg$,  ch(79),              ~
               at (04,02), fac(hex(94)), errormsg$, ch(79),              ~
               at (05,02), "Part Number",                                ~
               at (05,30), fac(hex(84)) , part$                 , ch(25),~
               at (06,02), "Part Description",                           ~
               at (06,30), fac(hex(84)) , description$          , ch(32),~
               at (07,02), "Generic Part Designation",                   ~
               at (07,30), fac(hex(84)) , generic$              , ch(16),~
               at (07,48), fac(hex(8c)), gendescr$              , ch(30),~
               at (08,02), "Unit of Measure: Stocking",                  ~
               at (08,30), fac(hex(84)) , stkunit$              , ch(04),~
               at (08,48), fac(hex(8c)) , stkunitdescr$         , ch(32),~
               at (09,02), "                 Pricing",                   ~
               at (09,30), fac(hex(84)) , prcunit$              , ch(04),~
               at (09,48), fac(hex(8c)) , prcunitdescr$         , ch(32),~
               at (10,02), "Conversion (Stk==>Pricing)",                 ~
               at (10,30), fac(hex(84)) , conversion$           , ch(10),~
               at (11,02), "Category Code",                              ~
               at (11,30), fac(hex(84)) , category$             , ch(04),~
               at (11,48), fac(hex(8c)),  categorydescr$        , ch(32),~
               at (12,02), "Vendor Code",                                ~
               at (12,30), fac(hex(84)) , vencode$              , ch(9), ~
               at (12,48), fac(hex(8c)),  vendname$             , ch(32),~
               at (13,02), "Std Purchase Leadtime: Days",                ~
               at (13,30), fac(hex(84)) , leadtime$             , ch(10),~
               at (13,45), fac(hex(8c)) , lt_msg$               , ch(30),~
               at (14,02), "Part Type",                                  ~
               at (14,30), fac(hex(84)) , ptype$                , ch(03),~
               at (14,48), fac(hex(8c)), ptypedescr$            , ch(32),~
               at (15,02), "Minimum Order Quantity",                     ~
               at (15,30), fac(hex(84)) , pmoq$                 , ch(10),~
               at (16,02), "Mfg/Pur Increment (Pansize)",                ~
               at (16,30), fac(hex(84)) , pansize$              , ch(10),~
               at (16,58), "Created "   ,                                ~
               at (16,67), fac(hex(8c)) ,  createdate$          , ch(08),~
               at (17,02), "Min Sale Qty / SO Increment",                ~
               at (17,30), fac(hex(84)) , minsoqty$             , ch(10),~
               at (17,44), fac(hex(84)) , minsoinc$             , ch(10),~
               at (17,58), "Modified"   ,                                ~
               at (17,67), fac(hex(8c)) ,  moddate$             , ch(08),~
               at (18,02), "Safety Stock",                               ~
               at (18,30), fac(hex(84)) , safety$               , ch(10),~
               at (18,58), "By       "  ,                                ~
               at (18,67), fac(hex(8c)) ,  modby$               , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg$(1)              , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg$(2)              , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg$(3)              , ch(79),~
                     keys(pfkey$), key(keyhit%)
               errormsg$ = " "

            if keyhit% <> 13% then L40590
                call "MANUAL" ("HNYMSTSB")
                goto L40230

L40590:     if keyhit% <> 14% then L40630
                gosub where_used
                goto L40230

L40630:     if keyhit% <> 15% then L40670
                call "PRNTSCRN"
                goto L40230

L40670:     return

        set_pf1
            pfmsg$(1%) = "                                        " &    ~
                        "     (10)See Std Costs (13)Instructions"
            pfmsg$(2%) = " (2)Alternate Parts (5)Next Screen      " &    ~
                        "     (14)Where Used    (15)Print Screen"
            pfmsg$(3%) = "                    (8)Procurement Histo" &    ~
                        "ry   (25)Part Text     (16)Return      "
            pfkey$ = hex(ff02ffff05ffff08ff0a0bff0d0e0f102019ffff)
            if ecrpfk$ = " " then str(pfkey$,11%,1%) = hex(ff)
            str(pfmsg$(3%),1%,14%) = ecrpfk$
            str(pfmsg$(3),59,1) = hex(8c)
            gosub'040(textid$)
            str(pfmsg$(3),45,1) = fac25$
            if alts% = 0% then str(pfmsg$(2%),1%,1%) = hex(8c)
            if alts% <> 1% then return
                str(pfmsg$(2%),1%,1%) = hex(84)
                str(pfmsg$(2%),20%,1%) = hex(8c)
                return

        REM *************************************************************~
            * 2 N D   P A G E   O F   I N P U T   M A I N   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gets the second page of input for the screen. Prices,     *~
            * Costs, and that sort of thing.                            *~
            *************************************************************

        display_page2
            str(line2$,,61)= "This Part: " & part$
            gosub set_pf2

L41125:     accept                                                       ~
               at (01,02), "MANAGE INVENTORY PART MASTER FILE",          ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), infomsg$,  ch(79),              ~
               at (04,02), fac(hex(94)), errormsg$, ch(79),              ~
                                                                         ~
               at (05,02), "Cost Method",                                ~
               at (05,30), fac(hex(84)) , costtype$             , ch(01),~
               at (05,49), fac(hex(8c)) , costtypedescr$        , ch(32),~
                                                                         ~
               at (06,02), "Source Account- Purchasing",                 ~
               at (06,30), fac(hex(84)) , acct$( 1%)            , ch(12),~
               at (06,49), fac(hex(8c)),  acct_descr$( 1%)      , ch(32),~
                                                                         ~
               at (07,02), "Source Account- WIP",                        ~
               at (07,30), fac(hex(84)) , acct$( 2%)            , ch(12),~
               at (07,49), fac(hex(8c)),  acct_descr$( 2%)      , ch(32),~
                                                                         ~
               at (08,02), "Inventory Assets Account",                   ~
               at (08,30), fac(hex(84)) , acct$( 3%)            , ch(12),~
               at (08,49), fac(hex(8c)),  acct_descr$( 3%)      , ch(32),~
                                                                         ~
               at (09,02), "Cost of Sales Account",                      ~
               at (09,30), fac(hex(84)) , acct$( 4%)            , ch(12),~
               at (09,49), fac(hex(8c)),  acct_descr$( 4%)      , ch(32),~
                                                                         ~
               at (10,02), "Sales Distribution",                         ~
               at (10,30), fac(hex(84)) , acct$( 5%)            , ch(12),~
               at (10,49), fac(hex(8c)),  acct_descr$( 5%)      , ch(32),~
                                                                         ~
               at (11,02), "Sales Discounts",                            ~
               at (11,30), fac(hex(84)) , acct$(19%)            , ch(12),~
               at (11,49), fac(hex(8c)),  acct_descr$(19%)      , ch(32),~
                                                                         ~
               at (12,02), "Inventory Adjustments",                      ~
               at (12,30), fac(hex(84)) , acct$( 6%)            , ch(12),~
               at (12,49), fac(hex(8c)),  acct_descr$( 6%)      , ch(32),~
                                                                         ~
               at (13,02), "Inventory Variance Accounts:",               ~
               at (14,02), " 1", at(15,02), " 2",  at (16,02), " 3",     ~
               at (17,02), " 4", at(18,02), " 5",  at (19,02), " 6",     ~
               at (14,42), " 7", at(15,42), " 8",  at (16,42), " 9",     ~
               at (17,42), "10", at(18,42), "11",  at (19,42), "12",     ~
        /* STOPPED HERE */                                               ~
               at (14,05), fac(hex(84)) , acct$( 7%)            , ch(12),~
               at (15,05), fac(hex(84)) , acct$( 8%)            , ch(12),~
               at (16,05), fac(hex(84)) , acct$( 9%)            , ch(12),~
               at (17,05), fac(hex(84)) , acct$(10%)            , ch(12),~
               at (18,05), fac(hex(84)) , acct$(11%)            , ch(12),~
               at (19,05), fac(hex(84)) , acct$(12%)            , ch(12),~
               at (14,45), fac(hex(84)) , acct$(13%)            , ch(12),~
               at (15,45), fac(hex(84)) , acct$(14%)            , ch(12),~
               at (16,45), fac(hex(84)) , acct$(15%)            , ch(12),~
               at (17,45), fac(hex(84)) , acct$(16%)            , ch(12),~
               at (18,45), fac(hex(84)) , acct$(17%)            , ch(12),~
               at (19,45), fac(hex(84)) , acct$(18%)            , ch(12),~
               at (14,18), fac(hex(8c)) , acct_descr$( 7%)      , ch(23),~
               at (15,18), fac(hex(8c)) , acct_descr$( 8%)      , ch(23),~
               at (16,18), fac(hex(8c)) , acct_descr$( 9%)      , ch(23),~
               at (17,18), fac(hex(8c)) , acct_descr$(10%)      , ch(23),~
               at (18,18), fac(hex(8c)) , acct_descr$(11%)      , ch(23),~
               at (19,18), fac(hex(8c)) , acct_descr$(12%)      , ch(23),~
               at (14,58), fac(hex(8c)) , acct_descr$(13%)      , ch(23),~
               at (15,58), fac(hex(8c)) , acct_descr$(14%)      , ch(23),~
               at (16,58), fac(hex(8c)) , acct_descr$(15%)      , ch(23),~
               at (17,58), fac(hex(8c)) , acct_descr$(16%)      , ch(23),~
               at (18,58), fac(hex(8c)) , acct_descr$(17%)      , ch(23),~
               at (19,58), fac(hex(8c)) , acct_descr$(18%)      , ch(23),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg$(1%)             , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg$(2%)             , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg$(3%)             , ch(79),~
                     keys(pfkey$), key(keyhit%)
               errormsg$ = " "

            if keyhit% <> 13% then L41525
                call "MANUAL" ("HNYMSTSB")
                goto L41125

L41525:     if keyhit% <> 14% then L41545
                gosub where_used
                goto L41125

L41545:     if keyhit% <> 15% then L41565
                call "PRNTSCRN"
                goto L41125

L41565:     close ws
            call "SCREEN" addr("C", u%, "I", i$(), cursor%())
            return

        set_pf2
            pfmsg$(1%) = "                    (4)Previous Screen  " &    ~
                        "     (10)See Std Costs (13)Instructions"
            pfmsg$(2%) = " (2)Alternate Parts (5)Next Screen      " &    ~
                        "     (14)Where Used    (15)Print Screen"
            pfmsg$(3%) = "                    (8)Procurement Histo" &    ~
                        "ry   (25)Part Text     (16)Return      "
            pfkey$ = hex(ff02ff040506ff08ff0a0bff0d0e0f102019ff00)
            if ecrpfk$ = " " then str(pfkey$,11%,1%) = hex(ff)
            str(pfmsg$(3%),1%,14%) = ecrpfk$
            str(pfmsg$(3%),59%,1%) = hex(8c)
            gosub'040(textid$)
            str(pfmsg$(3%),45%,1%) = fac25$
            if alts% = 0% then str(pfmsg$(2%),1%,1%) = hex(8c)
            if alts% <> 1% then return
                str(pfmsg$(2%),1%,1%) = hex(84)
                str(pfmsg$(2%),20%,1%) = hex(8c)
                return

        REM *************************************************************~
            * 3 R D   P A G E   O F   I N P U T   M A I N   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gets the third page of input for the screen.              *~
            *************************************************************

        display_page3
            str(line2$,,61)= "This Part: " & part$
            gosub set_pf3

L42420:     accept                                                       ~
               at (01,02), "MANAGE INVENTORY PART MASTER FILE",          ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$, ch(79),              ~
                                                                         ~
               at (04,02), "Primary Bin Location",                       ~
               at (04,30), fac(hex(84)) , bin$                  , ch(08),~
               at (05,02), "Buyer Part Class Code",                      ~
               at (05,30), fac(hex(84)) , bclass$(1)            , ch(03),~
               at (05,49), fac(hex(8c)) , bcdesc$(1)            , ch(32),~
               at (06,02), "Production Scheduler Class",                 ~
               at (06,30), fac(hex(84)) , bclass$(2)            , ch(03),~
               at (06,49), fac(hex(8c)) , bcdesc$(2)            , ch(32),~
               at (07,02), "Cycle Code(ABC)/Date",                       ~
               at (07,30), fac(hex(84)) , cyclecat$             , ch( 1),~
               at (07,35), fac(hex(84)) , cycledate$            , ch( 8),~
               at (07,49), "ABC Lock"            ,                       ~
               at (07,58), fac(hex(84)) , cyclelock$            , ch( 1),~
               at (07,60), "Count Rate"          ,                       ~
               at (07,71), fac(hex(84)) , count_rate$           , ch(10),~
               at (08,02), "Special/Obsolete Indicator",                 ~
               at (08,30), fac(hex(84)) , indicator$            , ch(4), ~
               at (09,02), "Part Class (User Def.)",                     ~
               at (09,30), fac(hex(84)) , class$                , ch(04),~
               at (09,49), fac(hex(8c)) , class_descr$          , ch(30),~
               at (10,02), "Part Taxable? (Y/N/ )",                      ~
               at (10,30), fac(hex(84)) , taxable$              , ch(01),~
               at (11,02), "Lot Tracking Required?",                     ~
               at (11,30), fac(hex(84)) , lottrack$             , ch(01),~
               at (12,02), "Ser. Numbers Required?",                     ~
               at (12,30), fac(hex(84)) , serial$               , ch(01),~
               at (13,02), "Prevent Negative Lots?",                     ~
               at (13,30), fac(hex(84)) , negqty$               , ch(01),~
               at (14,02), "Default Std Potency Factor",                 ~
               at (14,30), fac(hex(84)) , potency$              , ch(10),~
               at (15,02), "Drawing Size & Reference",                   ~
               at (15,30), fac(hex(84)) , draw_size$            , ch(02),~
               at (15,40), fac(hex(84)) , draw_ref$             , ch(16),~
               at (16,02), "Max Receipt ovr Ord % /Unit",                ~
               at (16,30), fac(hex(84)) , ovrrecpct$            , ch(08),~
               at (16,40), fac(hex(84)) , ovrrecqty$            , ch(10),~
               at (17,02), "S.O. Planning, Priority",                    ~
               at (17,30), fac(hex(84)) , sopriority$           , ch(01),~
               at (17,35), "Demand Type"            ,                    ~
               at (17,48), fac(hex(84)) , demtypedflt$          , ch(01),~
               at (17,53), "Allocation Method"      ,                    ~
               at (17,72), fac(hex(84)) , allocdflt$            , ch(01),~
               at (18,02), "ATC Horizon in Days",                        ~
               at (18,30), fac(hex(84)) , atch$                 , ch(10),~
               at (19,02), "Allow Overshipment % /Unit",                 ~
               at (19,30), fac(hex(84)) , ovrshppct$            , ch(08),~
               at (19,40), fac(hex(84)) , ovrshpqty$            , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg$(1%)             , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg$(2%)             , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg$(3%)             , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key(keyhit%)

               errormsg$ = " "

               if keyhit% <> 13% then L43010
                  call "MANUAL" ("HNYMSTSB")
                  goto L42420

L43010:        if keyhit% <> 14% then L43050
                  gosub where_used
                  goto L42420

L43050:        if keyhit% <> 15% then L43090
                  call "PRNTSCRN"
                  goto L42420

L43090:         close ws
                call "SCREEN" addr("C", u%, "I", i$(), cursor%())
                return

        set_pf3
            pfmsg$(1%) = "                    (4)Previous Screen  " &    ~
                        "     (10)See Std Costs (13)Instructions"
            pfmsg$(2%) = " (2)Alternate Parts (5)See Variable Fields" &  ~
                          "   (14)Where Used    (15)Print Screen"
            pfmsg$(3%) = "                    (8)Procurement Histo" &    ~
                        "ry   (25)Part Text     (16)Return      "
            pfkey$ = hex(ff02ff040506ff08ff0a0bff0d0e0f102019ff00)
            if ecrpfk$ = " " then str(pfkey$,11%,1%) = hex(ff)
            str(pfmsg$(3%),1%,14%) = ecrpfk$
            if vfs$ <> " " then L43230
               str(pfmsg$(2),21,24) = " " : str(pfkey$,5,1) = hex(ff)
L43230:     str(pfmsg$(3),59,1) = hex(8c)
            gosub'040(textid$)
            str(pfmsg$(3),45,1) = fac25$
            if alts% = 0% then str(pfmsg$(2%),1%,1%) = hex(8c)
            if alts% <> 1% then return
                str(pfmsg$(2%),1%,1%) = hex(84)
                str(pfmsg$(2%),20%,1%) = hex(8c)
                return

        where_used
            if fs%(21%) = 0% then                                        ~
                call "OPENCHCK" (#21, fs%(21%), f2%(21%), 0%, rslt$(21%))
            plowkey$ = str(part$,,25)
            init (hex(00)) str(plowkey$,26)
            call "PLOWALTS" (#21, plowkey$, 1%, 25%, f1%(21%))
            if f1%(21%) <> 0% then L45790
                errormsg$ = "Not found as a component of any Product" &  ~
                            " Structure (BOM)."
                return
L45790:     misc$ = hex(06) & "Part " & part$ & " is used in these BOM's"
            hdr$(1%) = "  Assembly Part Code        ID   Part Description"
            incl(1%) = 0
            plowkey$ = str(part$,,25)
            call "PLOWCODE" (#21, plowkey$, misc$, 8025%, 1.32, f1%(21), ~
                             hdr$(), 28, 0, incl(), incl$()," ","Y", #2)
            return

        REM *************************************************************~
            *  A L T E R N A T E   P A R T   I N P U T   S C R E E N S  *~
            *-----------------------------------------------------------*~
            * Screen code for input, edit, insert, and delete modes.    *~
            *************************************************************

        deffn'104(fieldnr%)
            screen% = 2%
            goto L46145

        deffn'124(fieldnr%)
            screen% = 3%
            goto L46145

        deffn'134(screenline%)
            screen% = 4%
            init(hex(8c)) tfac$()
            for temp% = 1% to 1%
                tfac$(screenline%, temp%) = hex(94)
            next temp%
            goto L46185

*        Set FAC's for input screens.
L46145:     init(hex(84)) tfac$()
            on fieldnr% gosub  L46170           /* PART NUMBER      */
            goto L46185

                    tfac$(screenline%, fieldnr%) = hex(80)  : return
L46170:             tfac$(screenline%, fieldnr%) = hex(81)  : return
                    tfac$(screenline%, fieldnr%) = hex(82)  : return

        alternate_display

L46185:     accept                                                       ~
               at (01,02), fac(hex(8c)), tttle$(screen%, 1)     , ch(54),~
               at (02,02), fac(hex(8c)), tttle$(screen%, 2)     , ch(54),~
               at (01,56), fac(hex(84)), part$                  , ch(25),~
               at (02,56), fac(hex(84)), description$           , ch(25),~
               at (03,02), fac(hex(94)), errormsg$              , ch(63),~
               at (04,02), fac(hex(a4)), infomsg$               , ch(79),~
               at (05,02), fac(hex(84)), message$                       ,~
                                                                         ~
         at (05,20), fac(hex(84))    , altpart$(line%+  1%)     , ch(25),~
         at (06,20), fac(hex(84))    , altpart$(line%+  2%)     , ch(25),~
         at (07,20), fac(hex(84))    , altpart$(line%+  3%)     , ch(25),~
         at (08,20), fac(hex(84))    , altpart$(line%+  4%)     , ch(25),~
         at (09,20), fac(hex(84))    , altpart$(line%+  5%)     , ch(25),~
         at (10,20), fac(hex(84))    , altpart$(line%+  6%)     , ch(25),~
         at (11,20), fac(hex(84))    , altpart$(line%+  7%)     , ch(25),~
         at (12,20), fac(hex(84))    , altpart$(line%+  8%)     , ch(25),~
         at (13,20), fac(hex(84))    , altpart$(line%+  9%)     , ch(25),~
         at (14,20), fac(hex(84))    , altpart$(line%+ 10%)     , ch(25),~
         at (15,20), fac(hex(84))    , altpart$(line%+ 11%)     , ch(25),~
         at (16,20), fac(hex(84))    , altpart$(line%+ 12%)     , ch(25),~
         at (17,20), fac(hex(84))    , altpart$(line%+ 13%)     , ch(25),~
         at (18,20), fac(hex(84))    , altpart$(line%+ 14%)     , ch(25),~
         at (19,20), fac(hex(84))    , altpart$(line%+ 15%)     , ch(25),~
         at (20,20), fac(hex(84))    , altpart$(line%+ 16%)     , ch(25),~
         at (21,20), fac(hex(84))    , altpart$(line%+ 17%)     , ch(25),~
         at (22,20), fac(hex(84))    , altpart$(line%+ 18%)     , ch(25),~
         at (23,20), fac(hex(84))    , altpart$(line%+ 19%)     , ch(25),~
         at (24,20), fac(hex(84))    , altpart$(line%+ 20%)     , ch(25),~
                                                                         ~
         at (05,47), fac(hex(8c))    , altdesr$(line%+  1%)     , ch(34),~
         at (06,47), fac(hex(8c))    , altdesr$(line%+  2%)     , ch(34),~
         at (07,47), fac(hex(8c))    , altdesr$(line%+  3%)     , ch(34),~
         at (08,47), fac(hex(8c))    , altdesr$(line%+  4%)     , ch(34),~
         at (09,47), fac(hex(8c))    , altdesr$(line%+  5%)     , ch(34),~
         at (10,47), fac(hex(8c))    , altdesr$(line%+  6%)     , ch(34),~
         at (11,47), fac(hex(8c))    , altdesr$(line%+  7%)     , ch(34),~
         at (12,47), fac(hex(8c))    , altdesr$(line%+  8%)     , ch(34),~
         at (13,47), fac(hex(8c))    , altdesr$(line%+  9%)     , ch(34),~
         at (14,47), fac(hex(8c))    , altdesr$(line%+ 10%)     , ch(34),~
         at (15,47), fac(hex(8c))    , altdesr$(line%+ 11%)     , ch(34),~
         at (16,47), fac(hex(8c))    , altdesr$(line%+ 12%)     , ch(34),~
         at (17,47), fac(hex(8c))    , altdesr$(line%+ 13%)     , ch(34),~
         at (18,47), fac(hex(8c))    , altdesr$(line%+ 14%)     , ch(34),~
         at (19,47), fac(hex(8c))    , altdesr$(line%+ 15%)     , ch(34),~
         at (20,47), fac(hex(8c))    , altdesr$(line%+ 16%)     , ch(34),~
         at (21,47), fac(hex(8c))    , altdesr$(line%+ 17%)     , ch(34),~
         at (22,47), fac(hex(8c))    , altdesr$(line%+ 18%)     , ch(34),~
         at (23,47), fac(hex(8c))    , altdesr$(line%+ 19%)     , ch(34),~
         at (24,47), fac(hex(8c))    , altdesr$(line%+ 20%)     , ch(34),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit1%)

               if keyhit1% <> 15% then L46475
                  call "PRNTSCRN"
                  goto L46185

L46475: REM    IF SCREEN% <> 2% THEN RETURN
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        see_procurement_his              /* View Procurement History */
            if fs%(8%) = 0% then                                         ~
                    call "OPENCHCK" (#8, fs%(8%), f2%(8%), 0%, rslt$(8%))
            if fs%(7%) = 0% then                                         ~
                    call "OPENCHCK" (#7, fs%(7%), f2%(7%), 0%, rslt$(7%))
            if fs%(6%) = 0% then                                         ~
                    call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "HNYPRCSB" (vencode$, part$, 0%, #7, #2, #6, #8)
            return

        view_ecr_info                    /* View ECR History this Part */
            call "ECRINQSB" ("A",        /* "A" - Show ALL ECRs this   */~
                                         /*    part, open or closed.   */~
                             part$,      /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                             #01,        /* SYSFILE2                   */~
                             #02)        /* HNYMASTR                   */
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_program
            end
