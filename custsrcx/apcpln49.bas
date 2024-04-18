        REM *************************************************************~
            *              ( Replaces - Old (APCRPT05) )                *~
            *  Program Name      - APCPLN49                             *~
            *  Creation Date     - 01/07/97                             *~
            *  Last Modified Date- 11/14/97                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Special Report to Consolidate Pull's *~
            *                      for all Active Loads.                *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *                                                           *~
            *  Subroutine Used   - (APCPLN1B) - Used to Display         *~
            *                                   Planning Codes          *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/06/97 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 07/30/97 ! Modification to Calc the Current On-Hand !     *~
            *          ! balance for MFG Products.                !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            scr_load$5, mode$5,          /* LOAD NUMBER                */~
            apc_desc$30,                 /* LOAD DESCRIPTION           */~
            end_load$5,                  /* Ending Load Number         */~
            end_desc$30,                 /* Ending Load Description    */~
            apc_key$5,                   /* APCMAST PRIMARY KEY        */~
            apc_status$2,                /* APCPLNLD Load Status       */~
            sc_key1$27,                  /* APCPLNLD Alt Key 1         */~
            sc_st$2,                     /* APCPLNSC S.O. Line Status  */~
            sc_part$25,                  /* APCPLNSC Line Item Part No.*/~
                                         /* SC_PQTY%, SC_PQTY1% - Pulls*/~
            wrk_key$25,                  /* APCWRK05 PART NO KEY       */~
            on_hand$6,                   /* HNYQUAN QUQNTITY STORE(300)*/~
            t_bal_qty$5,                 /* Total On-Hand Remaining    */~
            pull_rec$64,                 /* APCPULLS - Record          */~
            readkey$44,                  /* HNYQUAN LOOKUP KEY         */~
            part_desc$32,                /* PART DESCRIPTION           */~
            print_title$43,              /* For Report Title           */~
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

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Consilidated Pull's Report        "
            pname$ = "APCPLN49 - Rev: R6.04"

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
            * #1  ! APCPLNLD ! Planning/Scheduling Load Master - APCMAST*~
            * #2  ! APCPLNSC ! New Planning S.O. Line Item Master       *~
            * #3  ! HNYQUAN  ! QUANTITY MASTER FILE                     *~
            * #4  ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * #5  ! APCPULLS ! Product Pull Allocation File             *~
            * #10 ! APCWRK49 ! APC Work File                            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,   "APCPLNLD",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   11, keylen =    5,                    ~
                        alt key  1, keypos =    3, keylen =  13,         ~
                            key  2, keypos =    1, keylen =  15

            select #2,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   24, keylen =  10,                     ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #3,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #4,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #5,  "APCPULLS",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  10,  keylen =  10,                     ~
                        alt key  1, keypos  =     1, keylen = 19,        ~
                            key  2, keypos  =    20, keylen = 25, dup

            select #10, "APCWRK49",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  25

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))

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

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   2%
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
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        begin_process
           call "SHOSTAT" ("Scanning Data")
           mode% = 1% : gosub open_work
           mode% = 3% : gosub open_work

           apc_key$ = all(hex(00))
           apc_key$ = scr_load$
           read #1,key = apc_key$, using    L19130, apc_key$, apc_status$,~
                                                    eod goto begin_done
L19130:       FMT POS(11), CH(5), POS(94), CH(2)
           goto L19180
        begin_next
           read #1,key > apc_key$, using L19130, apc_key$, apc_status$,   ~
                                                    eod goto begin_done
L19180:    if apc_status$ > "14" then goto begin_next
              if str(apc_key$,1%,1%)  <> "A" then goto L19210
                 if str(scr_load$,1%,1%) <> "A" then goto begin_done
L19210:    if apc_key$ > end_load$ then goto begin_done
              gosub scan_load_pulls
              goto begin_next
        begin_done
           gosub generate_report
           gosub delete_work
        return clear all
        goto inputmode

        scan_load_pulls
           call "SHOSTAT" ("Scanning Load ( "& apc_key$ & " )")
           sc_key1$ = all(hex(00))
           str(sc_key1$,1%,5%) = apc_key$
           read #2,key 1% > sc_key1$, using   L19370, sc_key1$, sc_part$, ~
                                           sc_pqty%, sc_pqty1%, sc_st$,  ~
                                                  eod goto scan_load_done
L19370:       FMT POS(7), CH(27), CH(25), POS(72), 2*BI(2), POS(110),    ~
                  CH(2)
           goto L19430
        scan_next
           read #2, using L19370, sc_key1$, sc_part$, sc_pqty%, sc_pqty1%,~
                                          sc_st$, eod goto scan_load_done
L19430:    if apc_key$ <> str(sc_key1$,1%,5%) then goto scan_load_done
           if sc_st$ > "14" then goto scan_next
           rhh% = sc_pqty% + sc_pqty1%         /* Stock and Mull Pulls */
           if rhh% = 0% then goto scan_next
              gosub update_work
              goto scan_next
        scan_load_done
        return

        update_work                                /* Build Work File */
            wrk_key$ = all(hex(00))
            str(wrk_key$,1%,25%) = sc_part$        /* Part Number-Pull*/
            wrk_qty% = rhh%
            read #10,hold,key = wrk_key$, using L19580, wrk_qty%,         ~
                                                       eod goto L19630
L19580:       FMT POS(26), BI(2)
            wrk_qty% = wrk_qty% + rhh%             /* Accumulate Qty  */
            put #10, using L19580, wrk_qty%
            rewrite #10
        return                                     /* New Record      */
L19630:     write #10, using L19640, wrk_key$, wrk_qty%, "     "
L19640:       FMT CH(25), BI(2), CH(5)
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
         "Enter a Valid Beginning Active Load Number?                  ",~
         "Enter a Valid Ending Active Load Number?                     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, wrk_key$, on_hand$,        ~
                      scr_load$, apc_desc$, sc_part$, part_desc$,        ~
                      apc_key$, readkey$, wrk_qty$, end_load$, end_desc$
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
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
        REM DATALOAD
        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

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
              on fieldnr% gosub L40160,         /* LOAD NUMBER       */   ~
                                L40160          /* ENDING LOAD NO.   */
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Starting Load Number:",                      ~
               at (06,25), fac(lfac$(1%)), scr_load$            , ch(05),~
               at (06,40), fac(hex(84)), apc_desc$              , ch(30),~
                                                                         ~
               at (07,02), "Ending Load Number  :",                      ~
               at (07,25), fac(lfac$(2%)), end_load$            , ch(05),~
               at (07,40), fac(hex(84)), end_desc$              , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40450
                  call "PRNTSCRN"
                  goto L40190

L40450:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40640     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40600
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40600:     if fieldnr% > 1% then L40620
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40620:     return

L40640: if fieldnr% > 0% then L40730  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40730:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50120,         /* Load Number           */ ~
                              L50270          /* Ending Load No.       */
            return

L50120: REM Load Number                           SCR_LOAD$
            if scr_load$ <> " " then goto L50150
               goto L50230
L50150:     convert scr_load$ to scr_load%, data goto L50230

            convert scr_load% to scr_load$, pic(00000)

            read #1,key = scr_load$, using L50210, apc_desc$,             ~
                                                           eod goto L50230
L50210:        FMT POS(16), CH(30)
            return
L50230:     errormsg$ = "Invalid Load Number Entered."
            scr_load$ = " "
        return

L50270: REM Ending Load Number                    END_LOAD$
            if end_load$ <> " " then goto L50300
               end_load$ = scr_load$
L50300:     convert end_load$ to end_load%, data goto L50380

            convert end_load% to end_load$, pic(00000)

            read #1,key = end_load$, using L50360, end_desc$,             ~
                                                           eod goto L50380
L50360:        FMT POS(16), CH(30)
            return
L50380:     errormsg$ = "Invalid Ending Load Number Entered."
            init(" ") end_load$, end_desc$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %+---------------------------------------------------------------~
        ~--------------+

L55100: %!########  ########   ##########################################~
        ~#   Page:#### !
                                                   /* COLUMN 1 HEADER */
L55130: %!---------------------------------------------------------------~
        ~--------------!
L55150: %!<----- Part Number ----->!<------ Part Description ------>!Pull~
        ~s!Stock ! Bal !
L55160: %!-------------------------!--------------------------------!----~
        ~-!------!-----!
                                                   /* DETAIL 1      */
L55200: %!#########################!################################!####~
        ~#!######!#####!

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
          print using L55150
          lcnt% = 5%
        return

        print_detail
          if lcnt% > 60% then gosub print_header
          print using L55160
          print using L55200, wrk_key$, part_desc$, wrk_qty$, on_hand$,   ~
                             t_bal_qty$
          lcnt% = lcnt% + 2%
          return

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            init(" ") rpt_time$, date$, print_title$
            print_title$ = "Consolidated Pull from Stock (Active Loads)"
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "SETPRNT" ("APCRPT", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCRPT", " ", 0%, 1%)
        return

        generate_report
            call "SHOSTAT" ("Printing Report")
            gosub select_printer
            wrk_key$ = all(hex(00))
            read #10,key > wrk_key$, using L60480, wrk_key$, wrk_qty%,    ~
                                                   eod goto generate_done
            goto L60490
        generate_next
            read #10, using L60480, wrk_key$, wrk_qty%,                   ~
                                                   eod goto generate_done
L60480:        FMT CH(25), BI(2)
L60490:     convert wrk_qty% to wrk_qty$, pic(#####)
            gosub get_hnyquan

        REM ERR$ = "     "
        REM IF WRK_QTY% > X% THEN ERR$ = "<----"

            gosub print_detail
            goto generate_next
        generate_done
            print using L55050
            gosub close_printer
        return

        get_hnyquan
          x% = 0%
          readkey$ = all(hex(00))
          str(readkey$,1%,25%) = wrk_key$
        get_hnyquan_next
          read #3,key > readkey$, using L60690, readkey$, on_hand,        ~
                                                           eod goto L60740
L60690:        FMT POS(17), CH(44), POS(69), PD(14,4)
          if str(readkey$,1%,25%) <> wrk_key$ then goto L60740
          if str(readkey$,26%,3%) <> "300" then goto get_hnyquan_next
             x% = x% + int(on_hand)
             goto get_hnyquan_next
L60740: convert x% to on_hand$, pic(#####-)

        gosub scan_pulls                 /* Total of all Pulls for MFG */
          read #4,key = wrk_key$, using L60790, part_desc$,               ~
                                                           eod goto L60800
L60790:      FMT POS(26), CH(32)
L60800: return

        scan_pulls
            inv_on_hand% = x%
            t_pull_qty% = 0% : t_bal_qty% = 0%
            init(" ") readkey$, t_bal_qty$
            read #5,key > readkey$ using L60910, pull_rec$,               ~
                                                       eod goto scan_done
            goto L60920
        scan_nxt
            read #5, using L60910, pull_rec$, eod goto scan_done
L60910:        FMT CH(64)
L60920:     if str(pull_rec$,20%,25%) <> wrk_key$ then goto scan_nxt
               get str(pull_rec$,56%,4%), using  L60940, pull_qty%
L60940:           FMT BI(2)
               t_pull_qty% = t_pull_qty% + pull_qty%
               goto scan_nxt
        scan_done
            t_bal_qty% = inv_on_hand% - t_pull_qty%
            convert t_bal_qty% to t_bal_qty$, pic(#####)

        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#10,mode$, 500%, f2%)
            if f2% <> 0% then goto L61110
        return
L61110:     call "SHOSTAT" ("Error - Cannot Open (APCWRK49)") : stop
        return
        delete_work
            call "FILEBGON" (#10)
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            gosub delete_work
            end
