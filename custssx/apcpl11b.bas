        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPL11B - Program (APCPLN11)        *~
            *  Creation Date     - 07/02/96                             *~
            *  Last Modified Date- 12/18/98                             *~
            *  Written By        - J. Browning Fields                   *~
            *                                                           *~
            *  Description       - Utility Program used to Resequence   *~
            *                      Planning File APCPLNDT.  Option to   *~
            *                      resequence an entire day or one      *~
            *                      department.  Append or recreate.     *~
            *                                                           *~
            *  Code Tables Used  - PLAN_DEPT                            *~
            *                                                           *~
            *  Subroutine Used   - APCPLN9B, EXPAND                     *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/15/96 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 03/14/98 ! Y2K                                      ! LDJ *~
            * 05/14/98 ! Added edits to insure data exists for    ! ERN *~
            *          !   beginning and ending production dates. !     *~
            * 10/29/98 ! (EWD001) - Special Fix related to Warranty!RHH *~
            * 12/18/98 ! (EWD002) - Rebuild Sort When Sequencing  ! RHH *~
            *          !    Disable Debug File Update. Only for   !     *~
            *          !    testing. 'expand_sub'                 !     *~     
            *************************************************************

        sub "APCPL11B"

        dim                              /* Record/Screen Variables    */~
            pl_sort$15,sort_key$60,      /* New Sort Index     (EWD002)*/~
            pl_sort1$3,                  /* Expanded Sort Index(EWD002)*/~
            pl_key$11, ld_yr$2, ld_wk$2, /* Use find Sort Code (EWD002)*/~
            ld_load$5,                   /*                    (EWD002)*/~ 
            sq_rec$20,                   /* APCPLNSQ Record            */~
            sq_key$11,                   /* APCPLNSQ Read Key          */~
            readkey$24,                  /* GENCODES Read Key          */~
            dt_key$23,                   /* APCPLNDT Read Key          */~
            dt_key1$57, sav_key$256,     /* APCPLNDT Read Key (Alt 1)  */~
            dt_rec$256,                  /* APCPLNDT Record            */~
            genc$128,                    /* GENCODES Record            */~
            dept$(48%)5,                 /* Department/Process Array   */~
            hdr$40, msg$(3%)79,          /* Shostat Error Messages     */~
            seq$5, seq%(48%),            /* Sequence Number Array      */~
            scr_dte_b$10,                /* Screen Begin  Prod.        */~
            scr_dte_e$10,                /* Screen Ending Prod.        */~
            beg_dte$10,                  /* Save Begin  Date           */~
            end_dte$10,                  /* Save Ending Date           */~
            sav_dte$10,                  /* Save Date for Key          */~
            scr_dept$3,                  /* Screen Department Code     */~
            sav_dept$5,                  /* Save Department Code       */~
            dept_desc$30,                /* Department Description     */~
            dept_proc$1,                 /* Departments to process     */~
            scr_proc$2,                  /* Screen Process Code        */~
            proc_desc$30,                /* Process Description        */~
            temp_date$10,                /* ShoStat Display Date       */~
            temp_date2$10,               /*    ditto                   */~
            dt_in$30, dt_out$50,         /* Use for Expand (EWD002)    */~
            reseq$1,                     /* Resequence Select  (A),(R) */~
            reseq_desc$30                /* Resequence Select Descript.*/

        dim                              /* (Program) - Variables      */~
            desc$30,                     /* Generic Description        */~
            cursor%(2%),                 /* Cursor location for edit   */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            edtmessage$79,               /* Edit Message               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(8%),                     /* = 0 if the file is open    */~
            f1%(8%),                     /* = 1 if READ was successful */~
            fs%(8%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(8%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Planning Assign Sequence Numbers  "
            pname$ = "APCPL11B - Rev: R6.04"

        REM *************************************************************
            mat f2% = con
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNSQ ! (NEW) Planning Sequence No. File         *~
            * #2  ! APCPLNDT ! (NEW) Planning Detail File               *~
            * #3  ! GENCODES ! Master Code Tables File                  *~
            * #4  ! APCPLNUC ! (NEW) Planning Master Units Capacity     *~
            * #5  ! APCPLNLD ! (New) Planning/Scheduling Load Master    *~
            * #6  ! EWDSEQTS ! (New) Use for Debugging Sort Change      *~ 
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCPLNSQ",                                      ~
                        varc,     indexed,  recsize =   20,              ~
                        keypos =  1,   keylen = 11

            select #2,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24
                                                    /* (EWD002) Begin   */
            select #4,  "APCPLNUC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =  1,   keylen = 11,                      ~
                        alt key 1, keypos =  7, keylen = 11

            select #5,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen = 5,                       ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15

            select #6,  "EWDSEQTS",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =  1,   keylen = 60

                                                    /* (EWD002) - End   */
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),   0%, rslt$(3%))
                                                    /* (EWD002)         */
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),   0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),   0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%), 500%, rslt$(6%))
                                                    /* (EWD002)         */
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            date$       =   date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor"& ~
                          " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 5%
L10090:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0%    then L10240

L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  = 1%    then gosub startover
                     if keyhit% <> 4%    then L10220

L10160:         fieldnr%         = max(1%, fieldnr% - 1%)
                gosub'051(fieldnr%)
                     if enabled% = 1%    then L10120
                     if fieldnr% = 1%    then L10090

                goto L10160
L10220:         if keyhit%  = 16% and fieldnr% = 1% then exit_program
                if keyhit% <>  0%        then L10120
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10120

            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            lastfieldnr%    = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  = 1%         then gosub startover
                if keyhit%  = 14%        then gosub dataload
                if keyhit%  = 16%        then exit_program
                if keyhit% <> 0%         then editpg1

L11130:     fieldnr%        = cursor%(1%)    - 5%
            if fieldnr%     < 1% or fieldnr% > 5% then editpg1
            if fieldnr%     = lastfieldnr%        then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% = 0%         then editpg1

L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  = 1%         then gosub startover
                if keyhit% <> 0%         then L11190

            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " "      then L11190
                lastfieldnr%  = fieldnr%
            goto L11130

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************
        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *     I N I T I A L I Z E   I N P U T   M E S S A G E S     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************
        deffn'050(scrnr%, fieldnr%)
            if fieldnr%    <> 0%         then L28110
                inpmessage$ = edtmessage$
            return

*        Define the Input Message for the Screen/Field Indicated
L28110:     if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Beginning Production Date?                           ",~
         "Enter an Ending Production Date?                             ",~
         "Enter a Valid Department?                                    ",~
         "Enter a Valid Process?                                       ",~
         "Enter a Resequence Selection - (A)ppend or (R)esequence?     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, desc$, dept$(),  ~
                      scr_dte_b$, sav_dte$, scr_dept$, sav_dept$, reseq$,~
                      dept_desc$, scr_proc$, proc_desc$, reseq_desc$,    ~
                      dept_proc$, beg_dte$, end_dte$, scr_dte_e$
            update%                  =  0%
            gosub load_dept

        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************
        startover
            u3%                      =  2%
            call "STARTOVR" (u3%)
            if u3%                   =  1%        then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            dt_key1$                 =  all(hex(00))
            str(dt_key1$, 1%, 6%)    =  str(beg_dte$,1%,6%)
            read #2,key 1% > dt_key1$, using L35070, dt_rec$,             ~
                eod goto eod_break
            sav_key$ = str(dt_rec$,47%,57%)       /* Sale Alt Key 1  */
            gosub data_prime
        date_check
            if str(dt_rec$,47%,6%)  <>  str(sav_dte$,1%,6%) then date_break
            if str(dt_rec$,59%,5%)   =  sav_dept$ then reseq_check
            if dept_proc$           <> "A"        then skip_dataput
            sav_dept$                =  str(dt_rec$,59%,5%)
                                            /* (EWD001) - Fix       */
            if str(sav_dept$,4%,2%) <> "01" then                       ~
                                            str(sav_dept$,4%,2%) = "01"
            temp_date2$ = sav_dte$
            call "DATFMTC" (temp_date2$)
            call "SHOSTAT" ("Resequence for " & temp_date2$ & " " & sav_dept$)

        reseq_check
            if reseq$                = "R"        then do_dataput
            if str(dt_rec$,111%,5%) <> "00000"    then skip_dataput

        do_dataput
            gosub dataput

        skip_dataput
            dt_key1$                 =  sav_key$
            read #2,key 1% > dt_key1$, using L35070, dt_rec$,             ~
                eod goto eod_break
            sav_key$ = str(dt_rec$,47%,57%)
            goto date_check

        eod_break
            str(dt_rec$,47%,6%) = "999999"

        date_break
            if update%               =  1%        then gosub update_plnsq
            if str(dt_rec$,47%,6%)   >  str(end_dte$,1%,6%) then end_dataload
            str(sav_dte$,1%,6%)      =  str(dt_rec$,47%,6%)
            gosub data_prime

            goto date_check

        data_prime
            mat seq% = zer
            if reseq$                = "A"        then gosub load_plnsq
            if dept_proc$            = "A"        then                   ~
                sav_dept$            =  str(dt_rec$,59%,5%)
                                            /* (EWD001) - Fix       */
            if str(sav_dept$,4%,2%) <> "01" then                       ~
                                            str(sav_dept$,4%,2%) = "01"
            str(temp_date2$,1%,6%) = str(sav_dte$,1%,6%)
            call "DATFMTC" (temp_date2$)
            call "SHOSTAT" ("Resequence for " & temp_date2$ & " " & sav_dept$)

        return

        end_dataload
        return clear all
        goto inputmode

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            update%                  =  1%
            for i%                   =  1% to dept_max%
                if dept$(i%)        <>  sav_dept$ then do_next_dept
                   seq%(i%) = seq%(i%) + 1%
                   goto L31150
        do_next_dept
            next i%
            goto L31210                             /* ERROR - NO SEQ. */

L31150:     dt_key$                  =  str(dt_rec$,24%,23%)
            read   #2,hold,key = dt_key$, using L35070, dt_rec$,         ~
                                                        eod goto L31215
                delete #2 
                                             /* (EWD002) - Begin       */   
            convert seq%(i%) to str(dt_rec$,111%,5%), pic(00000)
                                           
        REM - Build Index (DT_INDEX$)
              gosub Load_sort_code

              call "APCPLN9B" ( "0",         /* Set Flag for Data File */~
                                pl_sort$,    /* Sort Code From APCPLNUC*/~
                                dt_rec$,     /* (APCPLNDT) Record      */~
                                sort_key$,   /* Output Index Built     */~
                                #3 )         /* (GENCODES)             */
              str(dt_rec$,66%,30%) = str(sort_key$,1%,30%)
                                             
            write #2, using L35070, dt_rec$, eod goto L31210
            gosub expand_sub                 /* (EWD002) - End         */
        return
L31210:     errormsg$ = "Error - Dept = ("& sav_dept$&") Sales Order = (" ~
                           & str(dt_key$,1%,8%) & "-" & str(dt_key$,9%,2%)~
                           & ")"
            gosub error_prompt
        return
L31215:     errormsg$ = "Error - Sales Order = ("                         ~
                           & str(dt_key$,1%,8%) & "-" & str(dt_key$,9%,2%)~
                           & ")"
            gosub error_prompt
        return

        update_plnsq
            temp_date2$ = sav_dte$
            call "DATFMTC" (temp_date2$)
            call "SHOSTAT" ("Updating PLNSQ for " & temp_date2$)
            update% = 0%
            for i%                   =  1% to dept_max%
                init(" ") sq_key$, sq_rec$
                str(sq_key$, 1%,6%)  =  str(sav_dte$,1%,6%)
                str(sq_key$, 7%,5%)  =  dept$(i%)
                read   #1,hold,key = sq_key$, eod goto L31340
                   delete #1
L31340:         str(sq_rec$, 1%,6%)  =  str(sav_dte$,1%,6%)
                str(sq_rec$, 7%,5%)  =  dept$(i%)
                str(sq_rec$,12%,2%)  = "01"
                convert seq%(i%) to str(sq_rec$,14%,5%), pic(00000)
                str(sq_rec$,19%,2%)  = " "
                write  #1, using L35040, sq_rec$,  eod goto L31410
L31410:     next i%
        return

        load_sort_code                            /* (EWD002)          */
            init(" ") ld_load$, ld_yr$, ld_wk$, pl_key$, pl_sort$
            pl_sort% = 0% 
            ld_load$ = str(dt_rec$,1%,5%)
            read #5,key = ld_load$, using L31500, ld_Yr$, ld_wk$,        ~
                                                  eod goto L31530
L31500:        FMT POS(104), 2*CH(2)
            str(pl_key$,1%,2%) = ld_yr$               /* Prod Year     */
            str(pl_key$,3%,2%) = ld_wk$               /* Prod Week     */
            str(pl_key$,5%,2%) = str(dt_rec$,104%,2%) /* Shift Code    */
            str(pl_key$,7%,5%) = str(dt_rec$,42%,5%)  /* Dept and Proc */
            read #4,key = pl_key$, using L31510, str(pl_sort$,1%,12%),   ~
                                              pl_sort1$, eod goto L31530 
L31510:        FMT POS(18), CH(12), POS(123), CH(3)
            str(pl_sort$,13%,3%) = pl_sort1$           
            pl_sort% = 1%
L31530: return
                                                  /* (EWD002) - end    */   
        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* (APCPLNSQ) - FILE          */
L35040:     FMT CH(20)                   /* Sequence Record      - #1  */

                                         /* (APCPLNDT) - FILE          */
L35070:     FMT CH(256)                  /* Plan Detail Record   - #2  */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'101(fieldnr%, edit%)
L40060:     gosub'050(1%, fieldnr%)
            gosub set_pf1
                if fieldnr% > 0% then init(hex(8c)) lfac$()              ~
                                 else init(hex(86)) lfac$()

            on fieldnr% gosub L40180,      /* Begin  Prod. Date      */   ~
                              L40180,      /* Ending Prod. Date      */   ~
                              L40180,      /* Department             */   ~
                              L40180,      /* Process                */   ~
                              L40180       /* Append or Resequence   */
            goto L40210
                lfac$(fieldnr%) = hex(80) : return  /* Up / Low   */
L40180:         lfac$(fieldnr%) = hex(81) : return  /* Upper Only */
                lfac$(fieldnr%) = hex(82) : return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
                at (06,02), "Begin Production Date:"            ,        ~
                at (06,25), fac(lfac$(1%)), scr_dte_b$          , ch(10),~
                                                                         ~
                at (07,02), "End   Production Date:"            ,        ~
                at (07,25), fac(lfac$(2%)), scr_dte_e$          , ch(10),~
                                                                         ~
                at (08,02), "Department Code      :"            ,        ~
                at (08,25), fac(lfac$(3%)), scr_dept$           , ch(03),~
                at (08,35), fac(hex(84)),   dept_desc$          , ch(30),~
                                                                         ~
                at (09,02), "Process    Code      :"            ,        ~
                at (09,25), fac(lfac$(4%)), scr_proc$           , ch(02),~
                at (09,35), fac(hex(84)),   proc_desc$          , ch(30),~
                                                                         ~
                at (10,02), "(A)ppend/(R)esequence:"            ,        ~
                at (10,25), fac(lfac$(5%)), reseq$              , ch(01),~
                at (10,35), fac(hex(84)),   reseq_desc$         , ch(30),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15 then goto L40570
                     call "PRNTSCRN"
                     goto L40060
L40570:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
        if edit%    = 2%      then L40760     /*  Input Mode            */
            pf$(1)  = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2)  = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3)  = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1%  then L40720
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%)= hex(ff)
L40720:     if fieldnr% > 1%  then L40740
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40740:     return

L40760: if fieldnr% > 0%      then L40850  /*  Edit Mode - Select Fld */
            pf$(1)  = "(1)Start Over                           " &       ~
                      "                       (14)Resequence  "
            pf$(2)  = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3)  = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40850:                                  /*  Edit Mode - Enabled    */
            pf$(1)  = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2)  = "                                        " &       ~
                      "                                       "
            pf$(3)  = "                                        " &       ~
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
            on fieldnr% gosub L50140,           /* Begin  Prod. Date */   ~
                              L50290,           /* Ending Prod. Date */   ~
                              L50430,           /* Department Code   */   ~
                              L50670,           /* Process    Code   */   ~
                              L50810            /* Append or Reseq.  */
        return

L50140: REM Verify Begin Production Date           SCR_DTE_B$
            if scr_dte_b$         > " "   then L50170
            scr_dte_b$ = date                                                   /* (Y2K, LDJ) */
            call "DATFMTC" (scr_dte_b$)                                         /* (Y2K, LDJ) */
       
L50170:     date%                 = 0%
            call "DATEOKC"   (scr_dte_b$, date%, errormsg$)
            if date%              = 0%    then L50250
            call "DATUFMTC" (scr_dte_b$)                                        /* (Y2K, LDJ) */
            str(beg_dte$,1%,6%)   = str(scr_dte_b$,1%,6%)
            str(sav_dte$,1%,6%)   = str(beg_dte$,1%,6%)
            call "DATFMTC"  (scr_dte_b$)                                        /* (Y2K, LDJ) */

            dt_key1$                 =  all(hex(00))
            str(dt_key1$, 1%, 6%)    =  str(beg_dte$,1%,6%)
            read #2, key 1% > dt_key1$, using L35070, dt_rec$, eod goto L50220
            if str(dt_rec$,47,6) = str(beg_dte$,1,6) then return
L50220:         errormsg$ = "(Error) - No records for BEGIN Prod. Date " & scr_dte_b$
                goto L50260

L50250:     errormsg$ = "(Error) - Invalid BEGIN Prod. Date?"
L50260:     init(" ") scr_dte_b$, beg_dte$, sav_dte$
            return

L50290: REM Verify Ending Production Date          SCR_DTE_E$
            if scr_dte_e$         = " "   then scr_dte_e$ = scr_dte_b$
            date%                 = 0%
            call "DATEOKC"   (scr_dte_e$, date%, errormsg$)                     /* (Y2K, LDJ) */
            if date%              = 0%    then L50390
            call "DATUFMTC" (scr_dte_e$)                                        /* (Y2K, LDJ) */
            str(end_dte$,1%,6%)   = str(scr_dte_e$,1%,6%)
            if end_dte$ < beg_dte$        then L50390
            call "DATFMTC"  (scr_dte_e$)                                        /* (Y2K, LDJ) */

            dt_key1$                 =  all(hex(00))
            str(dt_key1$, 1%, 6%)    =  str(end_dte$,1%,6%)
            read #2, key 1% > dt_key1$, using L35070, dt_rec$, eod goto L50320
            if str(dt_rec$,47,6) = str(end_dte$,1,6) then return
L50320:         errormsg$ = "(Error) - No records for END Prod. Date " & scr_dte_e$
                goto L50395

L50390:     errormsg$ = "(Error) - Invalid ENDING Prod. Date?"
L50395:     init(" ") scr_dte_e$, end_dte$
            return

L50430: REM Verify Department Code                 SCR_DEPT$
            if scr_dept$          = " "   then scr_dept$ = "ALL"
            if scr_dept$         <> "ALL" then L50500
                dept_desc$        = "All Departments"
                dept_proc$        = "A"
                return

L50500:     convert scr_dept$ to scr_dept%, data goto L50630

            convert scr_dept% to scr_dept$, pic(000)

            readkey$              = " "
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) =  scr_dept$
            read #3,key = readkey$, using L50580, desc$, eod goto L50630
L50580:         FMT POS(25), CH(30)

            dept_desc$            =  str(desc$,1%,30%)
            sav_dept$             =  scr_dept$ & "01"
        return
L50630:     errormsg$ = "(Error) - Invalid DEPARTMENT Lookup ?"
            init(" ") dept_desc$, scr_dept$
        return

L50670: REM Verify Process Code                    SCR_PROC$
            scr_proc$             = "01"
            readkey$              = " "
            str(readkey$,1%,9%)   = "PLAN PROC"
            str(readkey$,10%,15%) =  scr_proc$
            read #3,key = readkey$, using L50730, desc$, eod goto L50770
L50730:         FMT POS(25), CH(30)

            proc_desc$            =  str(desc$,1%,30%)
        return
L50770:     errormsg$ = "(Error) - Invalid PROCESS Lookup ?"
            init(" ") proc_desc$, scr_proc$
        return

L50810: REM Verify Resequence Selection            RESEQ$
            if reseq$             = " "   then reseq$ = "A"
            if reseq$             = "R"   then                           ~
               reseq_desc$        = "Resequence Department(s)"
            if reseq$             = "A"   then                           ~
               reseq_desc$        = "Append Department(s) Sequence"
            if reseq$             = "R"   then return
            if reseq$             = "A"   then return
            errormsg$ = "(Error) - Invalid RESEQUENCE Selection?"
            init(" ") reseq_desc$, reseq$
        return

        REM *************************************************************~
            *               S P E C I A L   R O U T I N E S             *~
            *-----------------------------------------------------------*~
            * Subroutines for Edit or Update Processes                  *~
            *************************************************************
        load_dept
            call "SHOSTAT" ("Loading Department Array")
            i%                       =  0%
            readkey$                 =  all(hex(20))
            str(readkey$,1%,9%)      = "PLAN DEPT"
        dept_next
            read #3,key > readkey$, using L60130, readkey$,               ~
                                                        eod goto dept_max
L60130:        FMT CH(24)
            if str(readkey$,1%,9%)  <>  "PLAN DEPT" then goto dept_max
            if str(readkey$,10%,3%)  = "100"        then goto dept_next
            if str(genc$,10%,3%)     > "104"        then goto dept_next
            i%                       =  i% + 1%
            dept$(i%)                =  str(readkey$,10%,3%) & "01"
            goto dept_next
        dept_max
            dept_max%                =  i%
        return

        load_plnsq
            str(sav_dte$,1%,6%) = str(dt_rec$,47%,6%)
            str(temp_date$,1%,6%) = str(sav_dte$,1%,6%)
            call "DATFMTC" (temp_date$)
            call "SHOSTAT" ("Loading PLNSQ for " & temp_date$)
            for i%                   =  1% to dept_max%
                str(sq_key$, 1%,6%)  =  str(sav_dte$,1%,6%)
                str(sq_key$, 7%,5%)  =  dept$(i%)
                read #1,key = sq_key$, using L60310, seq$, eod goto L60360
L60310:            FMT POS(14), CH(5)
                seq%(i%) = 0%
                convert seq$ to seq%(i%), data goto L60350

L60350:     next i%
L60360: return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                             /* (EWD002)               */  
        expand_sub                           /* New Data Expansion     */
          return                             /* Diable Error Log       */

            init(" ") dt_in$, dt_out$        /* Conversion of Index    */  
            dt_in% = 30% : dt_out% = 0%      /* Max Characters (40)    */
            dt_in$ = str(sort_key$,1%,30%)
            ret% = 0%
            call "EXPAND" (dt_in$,dt_in%,dt_out$,dt_out%, ret%)
            if ret% <> 0% then gosub L61020  /* Error has Occurred     */
               write #6, using L61000, str(dt_rec$,42%,3%),              ~    
                           str(dt_rec$,47%,57%), dt_out$, eod goto L61010
L61000:           FMT CH(3), CH(57), CH(50)
L61010: return
                                             /* (EWD002)               */ 
L61020: errormsg$ = "(Error) Expanding sort Data for "
        gosub error_prompt
        init(" ") errormsg$
        return
 
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
