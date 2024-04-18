        REM *************************************************************~
            * APCCST03 - 1. Utility Program to Update or Put in the Cost*~
            *               for Raw Material Part Numbers.              *~
            *                                                           *~
            *            2. Print report with Raw Materials that have a *~
            *               Zero Cost.                                  *~
            *                                                           *~
            *            3. For Specified date freeze the cost for all  *~
            *               Raw Materials.                              *~
            *                                                           *~
            *            4. Print report Showing all Raw Materials with *~
            *               a variance greater than the specified.      *~
            *                                                           *~
            *            5. Zero all 'On-Hand' Quantities in the HNYQUAN*~
            *               File just before Inventory. Usually done in *~
            *               the Test Database. Hidden PF(10) Key.       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/06/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 08/22/95 ! Mod to Freeze Raw Material Costs         ! RHH *~
            * 12/18/95 ! Mod with Hidden PF(10) Key to Zero       ! RHH *~
            *          !   On-Hand Quantities in the HNYQUAN File.!     *~
            *          !   Only done in TEST Database.            !     *~
            * 11/11/97 ! Mod for New Release Upgrade to R6.04.03  ! RHH *~
            *          !                                          !     *~
            * 03/12/98 ! Y2K Conversion                           ! djd *~
            * 05/26/06 ! (AWD001) mod to set up more ids to zero  ! CMG *~
	    *************************************************************

        dim                                                              ~
            hdr$40, msg$(3%)79,          /* Askusers Strings           */~
            readkey$50,                  /* Zero Cost Key              */~
            sel$1, sel_d$35,             /* Report Selection           */~
            var_amt$8, var$8,            /* Variance Check Amount      */~
            beg_p$25, end_p$25,          /* Begin/End Raw Material No.s*/~
            beg_d$30, end_d$30,          /* Begin/End Descriptions     */~
            apc_key$44, rpt_key$25,      /* HNYQUAN Primary Key        */~
            freeze_key$28, scr$(10%)40,  /* Freeze Key                 */~
            partno$25, partno_d$30,      /* Raw Material Part No./ Desc*/~
            part$25, cnt$10,             /* Raw Material Part No.      */~
            store$3, chk_store$3,        /* Store Number               */~
            cost$10, f_date$8,           /* New Cost Value             */~
            ss$(10%)10,                  /* Raw Mat'l Store Values     */~
            oh$(10%)20,                  /*           OnHand Values    */~
            or$(10%)20,                  /*           OnOrder Values   */~
            cc$(10%)20,                  /*           Cost Values      */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        dim blankdate$6,                 /* Empty Date                 */~
            workdate6$6,                 /* Temporary Date             */~
            workdate8$8                  /* Work Date                  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Costing Raw Material Utility      "
            pname$ = "APCCST03 - Rev: R6.04"

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
            * #1  ! HNYQUAN  ! Inventory Master Quantities File         *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #3  ! APCCSTFZ ! Master File of Frozen Costs              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #3,  "APCCSTFZ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  28

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "PAUSE" addr(300%)
            
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),500%, rslt$(3%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            scr$( 1%) = "****************************************"
            scr$( 2%) = "*         Report Selections            *"
            scr$( 3%) = "*                                      *"
            scr$( 4%) = "*(1) Rpt Raw Materials with Zero Cost. *"
            scr$( 5%) = "*(2) Rpt Raw Materials Outside Variance*"
            scr$( 6%) = "****************************************"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% then gosub begin_process
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
                  if keyhit%  = 14% then gosub begin_process
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        begin_process
           gosub'102(1%)
           if keyhit% = 1% then gosub startover
           if keyhit% <> 16% then goto L19065
              return clear all
              goto inputmode

L19065:    errormsg$ = " "
           if sel$ <> " " then goto L19080
              sel% = 1%
L19080:       convert sel$ to sel%, data goto L19085
L19085:
              sel_d$ = str(scr$(sel% + 3%),6%,34%)

           if chk_store$ <> " " then goto L19115
L19105:       chk_store$ = "ALL"
              goto L19140
L19115:    if chk_store$ = "ALL" then goto L19140
              convert chk_store$ to chk_store%, data goto L19105

              convert chk_store% to chk_store$,pic(000)

L19140:    if beg_p$ <> " " then goto L19180
              beg_p$ = " "
              beg_d$ = hex(06) & "Select Beginning Raw Mat'l  "
              call "PLOWCODE" (#2, beg_p$, beg_d$, 0%, .30, f1%(2%))
              if beg_p$ <> " " then goto L19180
                 str(beg_p$,1%,17%) = "ALL Raw Materials"
                 end_p$ = beg_p$
                 goto begin_process
L19180:    if end_p$ <> " " then goto L19220
              end_p$ = " "
              end_d$ = hex(06) & "Select Ending Raw Mat'l No. "
              call "PLOWCODE" (#2, end_p$, end_d$, 0%, .30, f1%(2%))
              if end_p$ <> " " then goto L19220
                 end_p$ = beg_p$
                 end_d$ = beg_d$
                 goto begin_process
L19220:    if end_p$ >= beg_p$ then goto L19235
              end_p$ = beg_p$
              end_d$ = beg_d$
L19235:    if var_amt$ <> " " then goto L19245
              var_amt$ = "0.02"
L19245:    var_amt = .02
           convert var_amt$ to var_amt, data goto L19255
L19255:
           convert var_amt to var_amt$,pic(####.##-)

           if keyhit% <> 14% then goto begin_process

           if sel$ = "1" then                                            ~
                         call "SHOSTAT" ("Raw Mat'l Zero Cost Report")   ~
                        else call "SHOSTAT" ("Raw Mat'l Variance Report")

           gosub select_printer
           partno$ = all(hex(00))
           if str(beg_p$,1%,1%) = "A" then goto begin_next
              partno$ = beg_p$
              read #2,key = partno$,using L19350, partno$,                ~
                                                      eod goto begin_done
              goto L19355
        begin_next
           read #2,key > partno$,using L19350, partno$,                   ~
                                                      eod goto begin_done
L19350:        FMT CH(25)
L19355:    if str(beg_p$,1%,1%) = "A" then goto L19370
              if partno$ > end_p$ then goto begin_done
L19370:    if len(partno$) > 14 then goto begin_next  /* Raw Mat'l Only*/
           gosub dataload
           if sel$ <> "1" then goto L19400
              if check% = 0% then goto begin_next     /* Zero Cost Only*/
              gosub print_detail
              goto begin_next
L19400:    gosub freeze_check
           goto begin_next
        begin_done
           if lcnt% <> 99% then goto L19440
              gosub print_header
              print using L55160
              print using L55230," NO DATA FOUND "

L19440:    if lcnt% <> 99% then print using L55050
           gosub close_printer
        return clear all
        goto inputmode

        freeze_check
            for i% = 1% to store_max%
                init(" ") f_date$, freeze_key$, var$
                f_cost, cost, test = 0.0
                store$ = str(ss$(i%),8%,3%)
                convert str(cc$(i%),11%,10%) to cost,data goto L19495
L19495:
                if str(chk_store$,1%,1%) = "A" then goto L19510
                   if chk_store$ <> store$ then goto L19570
L19510:               str(freeze_key$,1%,3%)  = store$
                      str(freeze_key$,4%,25%) = part$
                   read #3,key = freeze_key$,using L19530, f_date$,f_cost,~
                                                          eod goto L19560
L19530:                FMT POS(29), CH(8), PD(14,4)
                     test = abs(f_cost - cost)
                     convert test to var$, pic(####.##-)

                     if test >= abs(var_amt) then gosub print_detail_a
                     goto L19570
L19560:            var$ = "NEW PART"
                   gosub print_detail_a
L19570:     next i%
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
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
         "Enter a Valid Raw Material Part Number?                      ",~
         "Enter a Valid New Raw Material Cost?                         "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, apc_key$, ss$(), oh$(),    ~
                      or$(), cc$(), partno$, partno_d$, part$, store$,   ~
                      cost$, rpt_key$, beg_p$, beg_d$, end_p$, end_d$,   ~
                      chk_store$, var_amt$, sel$, sel_d$, f_date$


        return

        REM *************************************************************~
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
            store_max%, check%, k% = 0%
            init(" ") ss$(), oh$(), or$(), cc$(), apc_key$
            str(apc_key$,1%,25%) = partno$
L30100:     read #1,key > apc_key$,using L35040, apc_key$, on_hand,       ~
                                           on_order, cost,eod goto L30320
            part$   = str(apc_key$,1%,25%)
            store$  = str(apc_key$,26%,3%)
            if part$ <> partno$ then goto L30320
            if chk_store$ = "ALL" then goto L30180
               if store$ <> chk_store$ then goto L30100

L30180:        k% = k% + 1%
               ss$(k%) = "Store: " & store$

               oh$(k%) = "On-Hand : "
               convert on_hand to str(oh$(k%),11%,10%), pic(#######.#-)

               or$(k%) = "On-Order: "
               convert on_order to str(or$(k%),11%,10%),pic(#######.#-)

               cc$(k%) = "Cost    : "
               convert cost     to str(cc$(k%),11%,10%),pic(####.####-)

               if cost <= 0.0 then check% = 1%
               goto L30100
L30320: part$ = partno$
        store_max% = k%
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            apc_key$ = " "
            str(apc_key$,1%,25%) = partno$
L31080:     read #1,hold,key > apc_key$,using L31100, apc_key$,           ~
                                                           eod goto L31160
L31100:        FMT POS(17), CH(44)
            if str(apc_key$,1%,25%) <> partno$ then goto L31160
               put #1,using L31130, cost
L31130:          FMT POS(117), PD(14,4)
               rewrite #1
               goto L31080
L31160: return clear all
        goto inputmode

        dataput_a                         /* File = (APCCSTFZ)      */
            freeze_key$ = " "
            str(freeze_key$,1%,3%) = store$
            str(freeze_key$,4%,25%)    = part$
/* Y2K */
            workdate6$ = date
/* Y2K */
            read #3,hold,key = freeze_key$, eod goto L31250
               delete #3
L31250:     put #3,using L35150, store$,        /* Raw Material Store No. */   ~
                                part$,          /* Raw Material Part No.  */   ~
                                workdate6$,     /* Date Last Frozen       */   ~
                                cost,           /* Cost of Material       */   ~
                                " "             /* Filler Area            */
            write #3, eod goto L31320
        return
L31320:     call "SHOSTAT" ("Update Error - "&freeze_key$) : stop
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                 /* FILE: HNYQUAN                           */~
            POS(17),        /*                                         */~
            CH(44),         /* Raw Material Part Number Primary Key    */~
            POS(69),        /*                                         */~
            PD(14,4),       /* quantity on-hand                        */~
            XX(8),                                                       ~
            PD(14,4),       /* quantity on order                       */~
            XX(24),                                                      ~
            PD(14,4)        /* Total Cost                              */~

L35150: FMT                 /* FILE: APCCSTFZ                          */~
            CH(3),          /*  Raw Material Store Code                */~
            CH(25),         /*  Raw Material Part Number               */~
            CH(08),         /*  Date Cost Last Updated                 */~
            PD(14,4),       /*  Cost For Specified Date                */~
            CH(20)          /*  Filler Area                            */

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
              on fieldnr% gosub L40170,         /* Part Number       */   ~
                                L40170          /* Part Cost         */

              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Raw Mat'l Part No. :",                       ~
               at (06,25), fac(lfac$(1%)), partno$              , ch(25),~
               at (06,55), fac(hex(84)), partno_d$              , ch(25),~
                                                                         ~
               at (07,02), "Raw Mat'l Part Cost:",                       ~
               at (07,25), fac(lfac$(2%)), cost$                , ch(12),~
                                                                         ~
               at (10,02), fac(hex(84)), ss$(1%)                , ch(10),~
               at (10,14), fac(hex(84)), oh$(1%)                , ch(20),~
               at (10,36), fac(hex(84)), or$(1%)                , ch(20),~
               at (10,58), fac(hex(84)), cc$(1%)                , ch(20),~
                                                                         ~
               at (11,02), fac(hex(84)), ss$(2%)                , ch(10),~
               at (11,14), fac(hex(84)), oh$(2%)                , ch(20),~
               at (11,36), fac(hex(84)), or$(2%)                , ch(20),~
               at (11,58), fac(hex(84)), cc$(2%)                , ch(20),~
                                                                         ~
               at (12,02), fac(hex(84)), ss$(3%)                , ch(10),~
               at (12,14), fac(hex(84)), oh$(3%)                , ch(20),~
               at (12,36), fac(hex(84)), or$(3%)                , ch(20),~
               at (12,58), fac(hex(84)), cc$(3%)                , ch(20),~
                                                                         ~
               at (13,02), fac(hex(84)), ss$(4%)                , ch(10),~
               at (13,14), fac(hex(84)), oh$(4%)                , ch(20),~
               at (13,36), fac(hex(84)), or$(4%)                , ch(20),~
               at (13,58), fac(hex(84)), cc$(4%)                , ch(20),~
                                                                         ~
               at (14,02), fac(hex(84)), ss$(5%)                , ch(10),~
               at (14,14), fac(hex(84)), oh$(5%)                , ch(20),~
               at (14,36), fac(hex(84)), or$(5%)                , ch(20),~
               at (14,58), fac(hex(84)), cc$(5%)                , ch(20),~
                                                                         ~
               at (15,02), fac(hex(84)), ss$(6%)                , ch(10),~
               at (15,14), fac(hex(84)), oh$(6%)                , ch(20),~
               at (15,36), fac(hex(84)), or$(6%)                , ch(20),~
               at (15,58), fac(hex(84)), cc$(6%)                , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40732
                  gosub freeze_cost

L40732:        if keyhit% <> 10% then goto L40740
                  gosub zero_onhand

L40740:        if keyhit% <> 15 then goto L40780
                  call "PRNTSCRN"
                  goto L40200

L40780:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40990     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (9)Freeze Mat'l Cost   " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff090affffff0e0f1000)
            if fieldnr% = 1% then L40950
               str(pf$(1%),64%)     = " " : str(pfkeys$,14%,1%) = hex(ff)
               str(pf$(3%),18%,26%) = " " : str(pfkeys$,16%,1%) = hex(ff)
               str(pf$(3%),64%)     = " " : str(pfkeys$, 9%,1%) = hex(ff)
L40950:     if fieldnr% > 1% then L40970
               str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40970:     return

L40990: if fieldnr% > 0% then L41080  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Update Data "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L41080:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
            gosub set_pf2
            inpmessage$ = "Enter the Applicable Data for Report Selection~
        ~?"

L41290:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Selection       :",                   ~
               at (06,25), fac(hex(81)),   sel$                 , ch(01),~
               at (06,30), fac(hex(84)),   sel_d$               , ch(34),~
                                                                         ~
               at (07,02), "Store No. or 'ALL'     :",                   ~
               at (07,25), fac(hex(81)),   chk_store$           , ch(03),~
                                                                         ~
               at (08,02), "Beginning Raw Mat'l No.:",                   ~
               at (08,25), fac(hex(81)),   beg_p$               , ch(25),~
               at (08,55), fac(hex(84)),   beg_d$               , ch(25),~
                                                                         ~
               at (09,02), "Ending Raw Mat'l No.   :",                   ~
               at (09,25), fac(hex(81)),   end_p$               , ch(25),~
               at (09,55), fac(hex(84)),   end_d$               , ch(25),~
                                                                         ~
               at (10,02), "Variance Amount        :",                   ~
               at (10,25), fac(hex(81)),   var_amt$             , ch(08),~
                                                                         ~
               at (13,21), fac(hex(84)),   scr$( 1%)            , ch(40),~
               at (14,21), fac(hex(84)),   scr$( 2%)            , ch(40),~
               at (15,21), fac(hex(84)),   scr$( 3%)            , ch(40),~
               at (16,21), fac(hex(84)),   scr$( 4%)            , ch(40),~
               at (17,21), fac(hex(84)),   scr$( 5%)            , ch(40),~
               at (18,21), fac(hex(84)),   scr$( 6%)            , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L41720
                  call "PRNTSCRN"
                  goto L41290

L41720:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50120,         /* Raw Mat'l Part No.    */ ~
                              L50290          /* Raw Material Cost     */
            return

L50120: REM Raw Material Part Number              PARTNO$
           if partno$ <> " " then goto L50180
              partno$ = " "
              partno_d$ = hex(06) & "Select Raw Material Part No."
              call "PLOWCODE" (#2, partno$, partno_d$, 0%, .30, f1%(1))

L50180:    if len(partno$) > 14 then goto L50250
           read #2,key = partno$,using L50200,partno_d$,eod goto L50250
L50200:       FMT POS(26), CH(30)
           if len(partno$) > 14 then goto L50250
              chk_store$ = "ALL"
              gosub dataload
        return
L50250:     errormsg$ = "(Error) - Invalid Raw Material Part Number?"
            init(" ") partno$, partno_d$
        return

L50290: REM Raw Material Cost                     COST$
            if cost$ <> " " then goto L50320
               cost$ = "0.0"
L50320:     cost = 0.0
            convert cost$ to cost, data goto L50380

            convert cost to cost$, pic(######.####-)

        return
L50380:     errormsg$ = "(Error) - Invalid Raw Material Cost Value?"
            cost$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                 /* Report Formats     */
L55050: %+---------------------------------------------------------------~
        ~-------+

L55100: %!########@########   ########################################  P~
        ~age: ##!
                                                 /* Report (1) Col Hdr.*/
L55130: %!---------------------------------------------------------------~
        ~-------!
L55150: %!<---- Raw Material ----->!St1! Cost (1) !St2! Cost (2) !St3! Co~
        ~st (3) !
L55160: %!-------------------------!---!----------!---!----------!---!---~
        ~-------!
                                                 /* Report (1) Detail  */
L55200: %!#########################!###!##########!###!##########!###!###~
        ~#######!

L55230: %!#########################!   !          !   !          !   !   ~
        ~       !

                                                 /* Report (2) Col Hdr.*/
L55290: %!Store!<---- Raw Material ----->!Frz Cost!Frz Date!!Cur Cost! Va~
        ~riance !
L55310: %!-----!-------------------------!--------!--------!!--------!---~
        ~-------!
                                                 /* Report (2) Detail  */
L55340: %! ### !#########################!####.##-!########!!####.##-! ##~
        ~###### !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
          if lcnt% <> 99% then print using L55050
          page_no% = page_no% + 1%
          print page
          print using L55050
          print using L55100, date$, rpt_time$, print_title$, page_no%
          print using L55130
          if sel$ = "1" then print using L55150                           ~
                        else print using L55290
          lcnt% = 5%
        return

        print_detail
         if lcnt% > 60% then gosub print_header
         print using L55160
         print using L55200,part$,str(ss$(1%),8%,3%),str(cc$(1%),11%,10%),~
                                 str(ss$(2%),8%,3%),str(cc$(2%),11%,10%),~
                                 str(ss$(3%),8%,3%),str(cc$(3%),11%,10%)
          lcnt% = lcnt% + 2%
        return

        print_detail_a
/* Y2K */
          workdate8$ = f_date$
          call "DATEFMT" (workdate8$)
/* Y2K */
          if lcnt% > 60% then gosub print_header
          print using L55310
          print using L55340,store$, part$, f_cost, workdate8$, cost, var$
          lcnt% = lcnt% + 2%
        return

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            if sel$ = "1" then                                           ~
                print_title$ = "Listing of Raw Materials With Zero Cost" ~
                          else                                           ~
                print_title$ = "Rpt of Raw Materials Outside Variance  "

            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "SETPRNT" ("APCCCC", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCCCC", " ", 0%, 1%)
        return

        freeze_cost
            cnt% = 0% : cnt$ = "[ ###### ]"
            call "SHOSTAT" ("Freezing Raw Mat'l Cost For"& date$)
            chk_store$ = "ALL"
            partno$ = " "
        freeze_next
            read #2,key > partno$, using L60580, partno$,                 ~
                                                     eod goto freeze_done
L60580:        FMT CH(25)
            cnt% = cnt% + 1%
            if mod(cnt%,50) <> 0 then goto L60640
               convert cnt% to str(cnt$,3%,6%),pic(######)
               print at(04,36);hex(84);cnt$

L60640:     if len(partno$) > 14 then goto freeze_next
            gosub dataload
            for i% = 1% to store_max%
                store$ = str(ss$(i%),8%,3%)
                cost = 0.0
                convert str(cc$(i%),11%,10%) to cost, data goto L60700
L60700:
                gosub dataput_a
            next i%
            goto freeze_next
        freeze_done
        return clear all
        goto inputmode

        zero_onhand
            gosub ok_zero
            if comp% <> 0% then goto zero_onhand_done
            cnt% = 0% : cnt$ = "[ ###### ]"
            call "SHOSTAT" ("Zero Raw Mat'l Quantity "& date$)
            readkey$ = " "
        zero_onhand_next
            read #1,hold,key 1% > readkey$,using L60870, readkey$,        ~
                                               eod goto zero_onhand_done
L60870:        FMT CH(44)
            cnt% = cnt% + 1%
            if mod(cnt%,50) <> 0 then goto L60930
               convert cnt% to str(cnt$,3%,6%),pic(######)
               print at(04,36);hex(84);cnt$

L60930:     put #1, using L60940, 0.0
L60940:        FMT POS(69), PD(14,4)

            rewrite #1
            goto zero_onhand_next
        zero_onhand_done
        return clear all
        goto inputmode

        ok_zero
            comp% = 1%
	    /* (AWD001)- begin */
	    id% = 0%  
	    gosub check_ids
REM            if userid$ <> "RHH" and userid$ <> "FJH" and userid$ <> "TST"~
                                                         then return
            if id% = 0% then return
	    /* (AWD001) -end */
	    init(" ") hdr$, msg$()
            comp% = 2%
            hdr$ = "**Zero - Inventory On-Hand Quantities**"
            msg$(1) = " All Inventory On-Hand Quantities will be set to"
            msg$(2) = "******** Z e r o   Q u a n t i t i e s  ********"
            msg$(3) = "Press <RETURN> To Continue, Any PF() Key To Exit"
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return
/*(AWD001) -begin */
	check_ids
	    id% = 1%
	    if userid$ = "RHH" then return
	    if userid$ = "FJH" then return
	    if userid$ = "TST" then return
	    if userid$ = "CMG" then return
	    if userid$ = "CG1" then return
	    if userid$ = "TSN" then return

	    id% = 0%
        return
	/* (AWD001) -end */
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
