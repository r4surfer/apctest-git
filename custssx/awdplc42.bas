************************************************************************~
*        Flex Line (W e l d m a t e  and  L i n e a l m a t e )        *~
*                  (PAR000) 01/15/2006 CR347         Dept (002)        *~
*                                                                      *~
*        *Special Size Test for Window Height of Sash's on the         *~
*                 Sash Welder. Don't pass to Welder when the Window    *~
*                 Height is less than 28.5. Sash Welder Limit.         *~
*                                                      (From APCPLU42) *~
*                                                                      *~
*        *Special Size Test for Window Height of Frame's on the        *~
*                 Frame Welder. Don't pass to Welder when the Window   *~
*                 Height is Greater than 77.0. Frame Welder Limit.     *~
*                                                      (From EWDPLC42) *~
*                                                                      *~
*        *Special Lock test for Saw Sash width Materials. If the cut is*~
*                 a Sash width cut and there is one lock. The 3rd      *~
*                 character of the Machine Code is changed to a "1".   *~
*                                                      (From APCPLM42) *~
*                                                                      *~
*        (Below Test is Commented out)                                 *~
*        *Special Size Test for set_up$ = '1' for saws                 *~
*                 Add 13/16 or .8125 to Width or Height.               *~
*                 Saw adjustment. (From APCPLM42)                      *~
*                                                                      *~
*                                                                      *~
*                  New Flex Line Processing for 211 Line               *~
*                  ( As of 01/15/2006 - RHH  Dept 002 )                *~
*                                                                      *~
*        AWDPLC42 - Create File with Data for Saw Optimization         *~
*                   with Weld information also.                        *~
*                                                                      *~
*         Subs Replaced - APCPLM42 (L), APCPLX42 (L)                   *~
*                         APCPLU42 (W), EWDPLC42 (W)                   *~
*                                                                      *~
*        (EWD030)                                                      *~
*            File Layout                                               *~
*                 FR930318                                             *~
*                 SN123 <Schedule Number>(100-999)                     *~
*                 TI<Title Record>(30)                                 *~
*                 TW<?????>                                            *~
*                 WE  <Frame>                                          *~
*                 SA  <Frame Saw>                                      *~
*                 SA  <Frame Saw> Etc.                                 *~
*                 WE  <SASH>                                           *~
*                 SA  <Sash Saw>                                       *~
*                 SA  <Sash Saw>                                       *~
*                 WE  <Frame> Etc.                                     *~
*                 '                                                    *~
*                 end                                                  *~
*                 FR                                                   *~
*                                                                      *~
*            Note(1)Primary Build Subs  'build_weld'    (Frame - Sash) *~
*                                       'build_saw_recs (Frame - Sash) *~
*                                                                      *~
*                (2)Primary Detail Subs 'build_detail_weld'            *~
*                                       'build_detail_saw'             *~
*                                                                      *~
*                   Each called once for Frame and once for Sash       *~
*                                                                      *~
*                (3)Primary Frame-Sash Sub 'check_cuts' for            *~
*                   Frame and Sash. Uses the "NEWFAMILY' Table         *~
*                   Obtains, Machine Code, Setup No, Saw No.           *~
*                                                                      *~
*                (4)Fields 15, 16, 17 are used for those machine that  *~
*                   require (Machine Code), (Machine Number) and       *~
*                   (Machine setup No.)                                *~
*                                                                      *~
*                (5)Primary Welder Frams - Sash Sub 'check_welder' for *~
*                   Frame and Sash Codes. Uses 'NEWFLEXWD' Table.      *~
*                   Loads <Style> <Welder No.> <Welder Setup Code>     *~
*                                                                      *~
*                (6)When changes are made to the following Tables, the *~
*                   subroutine will need to be checked for             *~
*                   Modifications.                                     *~
*                   build_descript uses (SCREEN, LOCK, HINGE) Tables   *~
*                                                                      *~
*                (7)Special Code Screen Codes for Sashs (4,5,6) are    *~
*                   not put in the Frame or Sash Data.                 *~
*                                                                      *~
*                                                                      *~
*10/01/2009! (AWD001) changes for optional head expander         ! CMG *~
*10/01/2009! (AWD002) add to descripton if Bay/Bow               ! CMG *~
*03/29/2011! (AWD003) mod for sash limiter desc                  ! CMG *~
*02/10/2012! (AWD004) mod for extra sash weld record             ! CMG *~
*05/19/2014! (CUT001) mod to add dim fields to CUTCC             ! CMG *~
*07/08/2015! (SR66708) mod to add cot/or sash cuts and welds     ! MES *~
*09/08/2016! (CR656) mod to add 'F' for foam for NEZ             ! MES *~
*08/11/2017! (CR1002)  - new top bottom parameter                ! RDB *~
*05/29/2018! (CR1196) TSO sash only sizes                        ! MES *~
*09/02/2021! (CR2887) add head expander cut logic                ! RDB *~
*01/17/2023! CR3224 Missing foam on some orders                  ! RDB *~
************************************************************************

        sub "AWDPLC42" (size%,           /* Batch Size (No. Windows)  */ ~
                        sched%,          /* Starting Schedule Number  */ ~
                        scr_dte$,        /* Production Date Formatted */ ~
                        scr_dept$,       /* Production Department     */ ~
                        scr_prod$,       /* Product Line              */ ~
                        scr_load$,       /* Production Load           */ ~
                        lk_fn$(),        /* 1,2 Lock with Fin (PAR000)*/ ~
                        #1,              /* (APCCUTWK) Saw Work File  */ ~
                        #2,              /* (GENCODES)                */ ~
                        #4,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #5,              /* (AMTBOMCD) Equation File  */ ~
                        #6 )             /* (AMTBOMIF) Validity File  */

        dim readkey$24,                  /* GENCODES Primary Key      */ ~
                                         /* (PAR000)                  */ ~
            lk_fn$(5%)30,                /* 1%=1Lock,2%=2Lock,3%=WFin */ ~
            cl$2, save_cl$1,             /* Color Code - WH=White     */ ~
            width$(50%)9,                /* Width Size - Weld         */ ~
            height$(50%)9,               /* Height Size - Weld        */ ~
            raw_mat$(50%)15,             /* Raw Material              */ ~
/*(AWD004) */ style$(3%)15,              /* Style Information         */ ~
/*(AWD004) */ welder$(3%)2,              /* Welder Numbers            */ ~
/*(AWD004) */ welder_set$(3%)2,          /* Welder Setup Codes        */ ~
/*(AWD004) */ welder_type$(3%)3,         /* Welder Type Frame or Sash */ ~
            welder_err$(2%)50,           /* Welder Error Message      */ ~
            saw_type$(2%)3,              /* Saw Type Frame or Sash    */ ~
            saw_err$(2%)50,              /* Saw Error Message         */ ~
            model$3,                     /* Model Code                */ ~
            size_w$9,                    /* Size Lookup - Weld        */ ~
            desc$32,                     /* GENCODES Description      */ ~
            size$3,                      /* Batch size-No windows Bat */ ~
            sched$3,                     /* Schedule Numbers          */ ~
            tsched$3,                    /* Starting Schedule Number  */ ~
            co$30,                       /* Linealmate Descriptive Not*/ ~
            c_o$2,                       /* COTTAGE, ORIEL CODE-CO,OR */ ~
            bat_rec$149,                 /* Batch Record              */ ~
            bat$2,                       /* Number of Batches         */ ~
            apc_scr$120,                 /* Screen Text               */ ~
            apc_prt$60,                  /* Print Text                */ ~
            apc_sze$20,                  /* Size                      */ ~
            scr_dte$8,                   /* Completion Date           */ ~
            scr_prod$1, scr_dept$3,      /* Product Line              */ ~
            scr_load$5,                  /* Production Load           */ ~
            wrk_key1$51,                 /* WORK KEY                  */ ~
            wrk_rec$200,                 /* WORK RECORD               */ ~
            seq$3, seq_w$3,              /* Record Number Key         */ ~
            dtl_load$5,                  /* Load Number               */ ~
            dtl_part$25,                 /* MFG Part Number           */ ~
            dtl_subpart$20,              /* (SR66708) subpart         */ ~
            save_part$25,                /* MFG Part Number Weld      */ ~
            save_part_s$25,              /* MFG Part Number Saw       */ ~
            ref_no$5, dt_ref$8,          /* Part Reference Number     */ ~
            ssq$3, dt_seq$5,             /* Daily Sequence Number     */ ~
            s$1,                         /* Screen Code               */ ~
            dt_samp$1,                   /* 0=NA, 1=SAMP, 2=DISP      */ ~
            col$(500%)25,                /* Cut Description           */ ~
            eq$(500%)8,                  /* Equation Codes and Number */ ~
            ct$(500%)9,                  /* Cut Widths and Heights    */ ~
            ct(500%),                    /* Cut Wid/Height Decimal    */ ~
            sh$(500%)1,                  /* Sash Type                 */ ~
            cr$(500%)10,                 /* Raw Material Part Number  */ ~
            cp$(500%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(500%)1,                  /* Cut Piece Yes or No       */ ~
            tw$1,                        /* WIDTH CUT PARTS           */ ~
            th$1,                        /* HEIGHT CUT PARTS          */ ~
            sa_d2$30,                    /* MFG Part Description      */ ~
            sa_type$(500%)2,             /* RECORD TYPE 'SA' OR 'LA'  */ ~
            sa_piece$(500%)4,            /* Number of Pieces's to Cut */ ~
            sa_cut$(500%)9,              /* Cut Size for Piece's      */ ~
            sa_part$(500%)15,            /* Raw Material Part Number  */ ~
            sa_rack$(500%)18,            /* Bin Loc and No. of Pieces */ ~
            sa_d1$(500%)16,              /* Raw Material Description  */ ~
            sa_m$(500%)8,                /* WINDOW TYPE,PROFILE TYPE  */ ~
            sa_s$(500%)1,                /* SAW SET-UP NUMBER         */ ~
            sa_cut_type$(500%)1,         /* 2 = Frame, 1 = Sash       */ ~
            type$1,                      /* Test for Fame or Sash     */ ~
            machine$3, set_up$1, lk$1,   /* Store Window Type code    */ ~
            saw_no$2, saw_no$(500%)2,    /* Machine Saw Number        */ ~
            ff_nam$7,                    /* Bridge File Name          */ ~
            file$30,                     /* Schedule Title Name       */ ~
            inc$5,                       /* Store Batch Number        */ ~
            inc1$6,                      /* Store Deptment code       */ ~
            inc2$5,                      /* Store 1st Sequence No.    */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79,                 /* Error Message Text        */ ~
            tb_w$(500%)1                 /* T/B cut (CR1002)          */ 

        dim                                                              ~
            sub_part$20,                 /* Subpart (AWD001)          */ ~
            save_sub$20,                 /* Save Subpart (AWD001)     */ ~
            save_sub1$20,                /* Save subpart1 (AWD001)    */ ~
            dtl_new_part$45,             /* (SR66708) entire part     */ ~
            save_newpart_s$45,           /* (SR66708) New Part Number */ ~
            cr_addl$(500%)5,             /* (SR66708) addl material num*/ ~
            s_f$(500%)1,                 /* (SR66708)Sash / Frame      */~
            die$(500%)15,                /* (SR66708)Die Number        */~
            adj(500%)                    /* (SR66708)Adjustment amt    */~

        dim dt_cust$9                    /* Customer Code (AWD002)    */

/*(SR66708)*/
        dim sz$100,                      /* FRACTIONS FOR 16'THS      */ ~
            calc$9                       /* Temp for Conversion       */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

                                               /* SET-UP SAW FILE(S)   */
            select #3,  "@SAW002@",                                       ~
                                consec , recsize = 149


            ff% = 3% : ff_nam$ = "@SAW002@"     /* DEPT 002 */

        REM ***********************************************************
                                            /* (AWDPLC42) Dept = 02   */
                                            /*Welder Setup Information*/
                                            /* 1% = Frame, 2% = Sash  */
                                            /* 'NEWFLEXWD' Table      */
                                            /* style$(),welder$(),    */
                                            /* welder_set$()          */

            welder_type$(1%) = "@F "        /* Frame Welder       (03)*/
            welder_type$(2%) = "@S "        /* Sash Welder        (03)*/

            Welder_err$(1%) = "Error Writing Welder Frame Record??"
            welder_err$(2%) = "Error Writing Welder Sash Record??"

                                            /* Saw Setup Information  */
                                            /* 1% = Sash, 2% = Frame  */
            saw_type$(1%) = "@S "           /* Sash Saw           (03)*/
            saw_type$(2%) = "@F "           /* Frame Saw          (03)*/

            saw_err$(1%) = "Error Writing Saw Sash Record??"
            saw_err$(2%) = "Error Writing Saw Frame Record??"

        REM ***********************************************************

            cw%, ch%, csw%, csh% = 0%
            init(" ") rslt$(), axd$
                                                 /* Create Saw Batches */
            call "OPENFILE" (#ff%, "IO   ", f2%(ff%), rslt$(ff%), axd$ )
            if f2%(ff%) <> 0% then goto L01350
               gosub file_exists
               if comp% <> 16% then goto L01310
                  call "FILEBGON" addr(#ff%)
                  goto L01350

L01310:        close #ff%
            call "OPENFILE" (#ff%, "EXTND", f2%(ff%), rslt$(ff%), axd$ )
               goto L01410

L01350:     str(rslt$(ff%),1%,6%)  = "OUTPTP"
            str(rslt$(ff%),7%,8%)  = "00001000"
            str(rslt$(ff%),15%,3%) = "100"
            str(rslt$(ff%),18%,3%) = "100"
            call "OPENFILE" (#ff%, "OUTPT", f2%(ff%), rslt$(ff%), axd$ )

L01410:
        REM - Adjust Schedule
                                             /*    *** IMPORTANT ***   */
            scr_prod$ = "2"                  /* For 200 Series         */
                                             /*    *** IMPORTANT ***   */
            hit% = 0%                        /* Set to (1) When Header */
            bat_no% = 0% : count% = 0%       /* is Built               */
            bat$ = "00"

            init(" ") save_part$, save_part_s$       /* (AWD001) */
            init(" ") save_part$, save_part_s$, save_sub$, save_sub1$
                                             /* Starting Schedule      */
            convert sched% to tsched$, pic(###)

                                             /* Batch Size Number      */
            convert size% to size$, pic(###)

           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
                 ~ 3/4 13/16 7/8 15/16"           /*(SR66708)*/

            call "SHOSTAT" ("Starting Schedule ("&tsched$&") for Dept ("&~
                                                           scr_dept$&")")

            init(" ") tsw$, tsh$               /*(SR66708)*/
            tw$ = "1" : th$ = "2"             /* Load Cut Descriptions */
REM            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
REM            if err% <> 0% then goto exit_program

/*(SR66708)*/
            call "AWDCUTLD" (scr_prod$, cw%, ch%, csw%, csh%, tw$, ~
                            th$, tsw$, tsh$,#2, err% )
            if err% <> 0% then goto create_next         /*(SR66708\)*/

            xcount% = 0%
            wrk_key1$ = all(hex(00))
                                             /* New Family of Windows  */
            read #1,key > wrk_key1$, using L01620 , wrk_key1$, wrk_rec$,  ~
                                                     eod goto create_done
            goto L01630
        create_next
            read #1, using L01620 , wrk_key1$, wrk_rec$,                  ~
                                                     eod goto create_done
L01620:          FMT POS(6), CH(51), CH(200)

L01630:     dt_ref$   = str(wrk_rec$,1%,8%)   /* Part Reference Number*/

            dt_seq$   = str(wrk_rec$,9%,5%)   /* Prod. Daily Seq. No. */

            dtl_load$ = str(wrk_rec$,29,5%)   /* Appian Load Number   */

            dtl_part$ = str(wrk_rec$,38%,25%) /* MFG Part Number      */

            dt_samp$  = str(wrk_rec$,77%,1%)  /* 0=No, 1=Samp, 2=Disp */

            ref_no$   = str(dt_ref$,4%,5%)    /* Shortend Ref No. Load*/

            ssq$      = str(dt_seq$,3%,3%)    /* Shortend Daily Seq.  */

            model$    = str(dtl_part$,1%,3%)  /* Model Code           */

            s$        = str(dtl_part$,11%,1%) /* Set Screen Code      */

            sub_part$ = str(wrk_rec$,81%,20%) /* Subpart (AWD001)     */
/*(SR66708))*/
            dtl_subpart$ = str(wrk_rec$,81%,20%)
            str(dtl_new_part$,1%,25%)  = str(dtl_part$,1%,25%)
            str(dtl_new_part$,26%,20%) = str(dtl_subpart$,1%,20%)

            dt_cust$  = str(wrk_rec$,121%,9%) /* Customer (AWD002)    */

                                              /* Skip 4, 5, 6 Sashs   */
            gosub lookup_color                /* Get Color Code       */

REM            p% = pos("456" = s$)          /*(SR66708)              */
                                            /* Save value for Special */
                                            /* Sashes                 */
REM            if p% <> 0% then goto create_next/*(SR66708)              */
            if s$ = "6" then goto create_next /*(SR66708)              */
            if len(dtl_part$) < 19 then goto create_next
/* (SR66708) If tso or bso and it is cottage or oriel then skip*/
            if (s$ = "4" or s$ = "5") and (str(dtl_part$,9,2) >= "70" ~
                 and str(dtl_part$,9,2) <= "97") then goto create_next

            if count% = 0% then gosub build_schedule   /* CREATE BATCH */
                                                       /* New Area     */
               gosub build_weld               /* Get Weld Info for     */
                                              /* J% = 1% (Frame)       */
                                              /* J% = 2% (Sash)        */

               gosub build_saw_recs
                   j% = 1%                    /* Frame Weld Record     */
                   k% = 1%
                   type$ = "2"
                   type% = 2%
                   gosub build_detail_weld
                                              /* Frame Saw Records     */
                   seq% = 0%
                   for sa% = 1% to sa_max%
                       gosub build_detail_saw
                   next sa%

                   j% = 2%                    /* Sash Weld Record      */
                   k% = 2%
                   type$ = "1"
                   type% = 1%
                   gosub build_detail_weld
/* (AWD004) */
REM this enables the sash record to be welded on two different types of welders
REM like a urban and sampson
                   if extraSash% = 0% then goto noExtraSash
REM                  call "SHOSTAT" ("HERE at Extra Sash ")  stop
                     j% = 2%                    /* Sash Weld Record      */
                     k% = 3%
                     type$ = "1"
                     type% = 1%
                     gosub build_detail_weld
noExtraSash:
/* (AWD004\)
                                              /* Sash Saw Records      */
                   seq% = 0%
                   for sa% = 1% to sa_max%
                       gosub build_detail_saw
                   next sa%
                                              /* New Area              */
                  count% = count% + 1%
                  if count% < size%  then goto create_next
                     gosub build_end
                     count% = 0% : hit% = 0%
                     goto create_next
        create_done
            gosub build_end
            close #ff%
            convert (bat_no% + 1%) to bat$, pic(##)
        goto exit_program

        build_schedule                         /* SET-UP SCHEDULE REV. */
          if hit% = 1% then return             /* Schedule has not been*/
          init(" ") bat_rec$                   /* Completed Yet.       */
          inc$  = " (" & bat$ & ")"                    /* Batch No (5) */
          inc1$ = "(" & scr_dept$ & ") "              /* Dept Code (6) */
          inc2$ = "(" & ssq$ & ")"             /* Sarting Seq. No. (5) */

          file$ = inc2$ & inc1$ & "WD-SW " & scr_dte$ & inc$
          if str(scr_load$,1%,1%) <> "N" then str(file$,6%,2%) = "/U"
          str(bat_rec$,1%,2%)   = "FR"                  /* FILE REV.   */
          str(bat_rec$,3%,6%)   = "930318"              /* REVISION NO */
          str(bat_rec$,9%,141%) = " "                   /* LINE FEED   */
          write #ff%, bat_rec$, eod goto L02070

          gosub build_schedule_no
          gosub build_schedule_title
          gosub build_schedule_quantity
          hit% = 1%
        return
L02070:   errormsg$ = "(Error) When Writing Revision Record??"
          gosub error_prompt
        return

        build_schedule_no
          init(" ") bat_rec$
          gosub assign_schedule
          convert sched% to sched$, pic(000)

          str(bat_rec$,1%,2%) = "SN"                /* SCHEDULE NUMBER */
          str(bat_rec$,3%,3%) = sched$              /* SCHED (100-999) */
          str(bat_rec$,6%,144%) = " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L02200
        return
L02200:   errormsg$ = "(Error) When writing Schedule Record??"
          gosub error_prompt
        return

        build_schedule_title                        /* SCHEDULE TITLE  */
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "TI"              /* SCHEDULE TITLE  */
          str(bat_rec$,3%,30%)  = file$             /* TITLE DESCRIPT  */
          str(bat_rec$,33%,117%)= " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L02310
        return
L02310:   errormsg$ = "(Error) When Writing Schedule Title??"
          gosub error_prompt
        return

        build_schedule_quantity                     /* SCHEDULE Quantity*/
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "TW"              /* SCHEDULE Quantity*/
          str(bat_rec$,3%,3%)   = size$             /* Batch Size       */
          str(bat_rec$,6%,144%) = " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L02320
        return
L02320:   errormsg$ = "(Error) When Writing Schedule Quantity"
          gosub error_prompt
        return

        build_detail_saw
          if sa_cut_type$(sa%) <> type$ then return /* type$ = 2 Frame */
                                                    /* type$ = 1 Sash  */
          xcount% = xcount% + 1%
          init(" ") bat_rec$
          seq% = seq% + 1%
          convert seq% to seq$, pic(###)
          str(bat_rec$,1%,2%)   = sa_type$(sa%)         /* 'SA'        */
          str(bat_rec$,3%,5%)   = ref_no$               /* Load Number */
          str(bat_rec$,8%,5%)   = "     "
          str(bat_rec$,13%,3%)  = seq$                  /* Item Number */
          str(bat_rec$,16%,4%)  = sa_piece$(sa%)        /* Unit Qty    */
          str(bat_rec$,20%,9%)  = sa_cut$(sa%)          /* Piece Cut   */
          str(bat_rec$,29%,9%)  = "        "            /* Reserved    */
          str(bat_rec$,38%,15%) = sa_part$(sa%)         /* Part No.    */
          str(bat_rec$,58%,4%)  = "    "                /* Reserved    */
          str(bat_rec$,57%,18%) = sa_rack$(sa%)         /* Harp Rack/  */
                                                        /* Bin Location*/
          str(bat_rec$,57%,3%) = saw_type$(type%)       /* Saw Type    */

          str(bat_rec$,75%,16%) = sa_d1$(sa%)           /* Part Desc   */
          str(bat_rec$,91%,30%) = sa_d2$                /* Label Desc  */
          str(bat_rec$,121%,1%) = " "                   /* Line Feed   */
          str(bat_rec$,122%,2%) = "  "                  /* No. Labels  */
          str(bat_rec$,124%,15%)= "               "     /* License Plat*/
          str(bat_rec$,139%,8%) = sa_m$(sa%)            /* Machine Code*/
          str(bat_rec$,147%,2%) = saw_no$(sa%)          /* Saw Number  */
          str(bat_rec$,149%,1%) = sa_s$(sa%)            /* Saw Set-up  */
                                                        /*  Number     */
          write #ff%, bat_rec$, eod goto L02400
        return
L02400:   errormsg$ = saw_err$(type%)
          gosub error_prompt
        return

        build_detail_weld                         /* j% = 1%   (Frame) */
                                                  /* j% = 2%   (Sash)  */
/* (SR66708) */
               /* do not write frame record for tso or bso */
          if (s$ = "4" or s$ = "5") and j% = 1% then return

          sash_coor_weld% = 0%
          pass% = 0%
        second_weld
          if type% = 1% and (c_o$ = "OR" or c_o$ = "CO") then    ~
                                 sash_coor_weld% = 1%
/* (\SR66708) */
          init(" ") bat_rec$
          convert (count% + 1%) to seq_w$, pic(###)
          str(bat_rec$,1%,2%)   = "WE"                  /* 'WE' Weld   */
          str(bat_rec$,3%,5%)   = ref_no$               /* Load Number */
          str(bat_rec$,8%,5%)   = "     "
          str(bat_rec$,13%,3%)  = seq_w$                /* Window No.  */
          str(bat_rec$,16%,4%)  = "0001"                /* Unit Qty    */

          str(bat_rec$,20%,9%)  = width$(j%)            /* Width Cut   */
          str(bat_rec$,29%,9%)  = height$(j%)           /* Height Cut  */

/*(SR66708)*/
          if sash_coor_weld% = 1% and pass% = 0% then ~
                str(bat_rec$,29%,9%)  = height$(co_h1%) /* Height Cut  */

          if sash_coor_weld% = 1% and pass% = 1% then ~
                str(bat_rec$,29%,9%)  = height$(co_h2%) /* Height Cut  */
/*(\SR66708)*/
                                                        /* Frame/Sash  */
          str(bat_rec$,38%,15%) = style$(k%)            /* Style Code  */

          str(bat_rec$,53%,4%)  = cl$ & "  "            /* Color Code  */
                                                      /* Cart Type Pieces*/
          str(bat_rec$,57%,18%) = sa_rack$(1%)
          str(bat_rec$,57%,3%)  = welder_type$(j%)
                                                        /* Bin Location*/
          str(bat_rec$,75%,16%) = "STD - Window    "    /* Part Desc   */
          str(bat_rec$,91%,30%) = sa_d2$                /* Label Desc  */
          str(bat_rec$,121%,1%) = " "                   /* lable Format*/
          str(bat_rec$,122%,2%) = "  "                  /* No. Labels  */
          str(bat_rec$,124%,15%)= "               "     /* License Plat*/
          str(bat_rec$,139%,8%) = "        "            /* Machine Code*/
          str(bat_rec$,147%,2%) = welder$(k%)           /* Welder No.  */
/*(CR656) if need to change tool id change welder_set*/
          str(bat_rec$,149%,1%) = welder_set$(k%)       /* Welder Setup*/
                                                        /*  Number     */
          gosub convert_height
          if j% = 1% then goto L02450
                                                        /* Special Test*/
                                                        /* Sash Limit  */
REM             if aa < 28.5 then goto L02600
                goto L02500                             /* Ok          */

L02450:                                                 /* Special Test*/
             if aa > 77.0 then goto L02600              /* Frame Limit */

L02500:   write #ff%, bat_rec$, eod goto L02640

/*(SR66708)*/
          if sash_coor_weld% <> 1% then return
             pass% = pass% + 1%
             if pass% = 1% then goto second_weld
L02600: return

L02640: errormsg$ = welder_err$(j%)
        gosub error_prompt
        return

        convert_height                              /* Of window       */
            aa = 0.0
            convert str(dtl_part$,17%,2%) to a1, data goto CS3
CS3:
            convert str(dtl_part$,19%,1%) to a2, data goto CS4
CS4:
            aa = a1 + (a2/8.0)                     /* Decimal Height   */

        return

        build_end
          if count% = 0% then return
          init(" ") bat_rec$
          str(bat_rec$,1%,2%) = "**"                /* END OF BATCH    */
          str(bat_rec$,3%,3%) = "END"               /* LINE FEED       */
          str(bat_rec$,6%,144%) = " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L02750
          bat_no% = bat_no% + 1%
          convert bat_no% to bat$, pic(00)

        return
L02750:   errormsg$ = "(Error) Building End Record??"
          gosub error_prompt
        return

        build_saw_recs
/*(AWD001) */
REM          if save_part_s$ <> dtl_part$ then goto L02850
REM             if save_part_s$ <> dtl_part$ or save_sub$ <> sub_part$ ~
                                             then goto L02850
/*(SR66708)*/if save_newpart_s$ <> dtl_new_part$ then goto L02850

             for i% = 1% to sa_max%
                 str(sa_rack$(i%),5%,9%)  = ssq$& "-A" &ssq$& "/"
             next i%
REM             return           /* CR3224 */

L02850:   save_newpart_s$ = dtl_new_part$          /* (SR66708)      */
REM       save_part_s$ = dtl_part$                 /* NEWFAMILY Info */
REM          save_sub$    = sub_part$                 /* (AWD001) */
          init(" ") sa_type$(), sa_piece$(), sa_cut$(), sa_part$(),      ~
                    sa_rack$(), sa_d1$(), sa_d2$, sa_m$(), sa_s$(),     ~
                    saw_no$(), sa_cut_type$(), die$()

          call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, #6,  ~
                                                                    err%)
          sa_d2$ = str(apc_prt$,1%,30%)

          sa% = 0%
REM          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */      ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                                                        #4, #5, #2, err%)
          init(" ") tb_w$()   /* CR1002 */                                                           
          call "AWDCUTCC" ( dtl_new_part$, 0%, 0%, 0%,    /*(SR66708*/~
                             0%, cw%, ch%, csw%, csh%,                ~
                             eq$(), ct$(), cr$(), cr_addl$(), cp$(),  ~
                             cc$(), col$(), ct(), sh$(), tw$, th$,    ~
                             s_f$(), die$(), adj(), tb_w$(), #4, #5, #2, err%)


          gosub build_descript
          eq% = cw% + ch% + csw% + csh%          /* (SR66708) */
          for i% = 1% to eq%
        REM *RHH*                               /* REMOVE 'LA' RECORDS */
            if cc$(i%) = "N" or cc$(i%) = " " then goto L03430

            cut% = 0%                    /* Skip - No Equation Records */
            convert str(ct$(i%),1%,3%) to cut%, data goto L03040
L03040:
            if cut% = 0% then goto L03430
               gosub check_cut                 /* Check NEWFAMILY Info */
               if check% = 0% then goto L03430 /* Skip Equation        */

               sa% = sa% + 1%                  /* Save Table Info.     */
               sa_type$(sa%)  = "SA"
               if cc$(i%) = "N" then sa_type$(sa%) = "LA"
               sa_piece$(sa%) = "  " & cp$(i%)     /* No Pieces Needed */
               sa_cut$(sa%)   = ct$(i%)            /* Length of Cut    */

        REM       if set_up$ <> "1" then goto L03045
        REM          ct(i%) = ct(i%) + .8125          /* ADD      13/16  */
        REM          init(" ") sa_cut$(sa%)
        REM          convert ct(i%) to str(sa_cut$(sa%),1%,7%), pic(###.###)

/* (SR66708) */
              if s$ <> "4" then goto notTSO
/* Only Add to Keeper Rail and Style */
                 if str(eq$(i%),6,3) = "601" then goto addLineal
                 if str(eq$(i%),6,3) = "701" then goto addLineal
                    goto notTSO

addLineal:
                      ct(i%) = ct(i%) + .8125          /* ADD    13/16   */
                      init(" ") sa_cut$(sa%), calc$
                      calc = ct(i%)
                      gosub con_fract
                      sa_cut$(sa%) = calc$

notTSO:

                                        /* cut_type% = 1% sash material */
               if cut_type% <> 1% then goto not_sash
                 if c_o$ <> "OR" and c_o$ <> "CO" then goto not_sash
                   if i% <= cw% then goto not_sash
                     if str(eq$(i%),7,2) <> "05" then goto not_sash
                      ct(i%) = ct(i%) + .8125           /*ADD   13/16   */
                      init(" ") sa_cut$(sa%), calc$
                      calc = ct(i%)
                      gosub con_fract
                      sa_cut$(sa%) = calc$

not_sash:
/* \SR66708) */
               str(sa_part$(sa%),1%,10%) = cr$(i%) /* Raw Material Part*/
               str(sa_part$(sa%),11%,5%) = "     "

/* (AWD001)  CR2887 */
            if str(eq$(i%),7,2) <> "04" then goto not_opt_head
               if (i% > cw%) then goto not_opt_head


REM               if str(sub_part$,13,1) = "1" and (i% <= cw%) and       ~
                 str(eq$(i%),7,2) = "03" then str(sa_part$(sa%),11,1) = "1"

/*(CR656)             REM NE Sill Angle Head Expander */
/* CR2887 added back with equation 04 */
            if str(sub_part$,13,1) = "1" then str(sa_part$(sa%),11,1) = "1"
             REM NE Sill Angle No Head Expander
            if str(sub_part$,13,1) = "2" then str(sa_part$(sa%),11,1) = "3"
             REM Stand Sill Angle no Head Expander
            if str(sub_part$,13,1) = "4" then str(sa_part$(sa%),11,1) = "2"

/*  (\CR656) removed because not offered on this product*/

not_opt_head:

/* (\AWD001)  */
/* (SR66708) */
               p% = 0%
               for k% = 1% to 5%
                if str(sa_part$(sa%),10%+k%,1%) <> " " then p%=k%
                if p% <> 0% then k% = 5%
               next k%
               str(sa_part$(sa%),p%+11,5%-len%) =                   ~
                    str(cr_addl$(i%),1%,5%-len%)
/* (\SR66708) */
/* (CR656)    */
            if cut_type% <> 2% then goto no_sash_f
            if str(eq$(i%),7,2) = "03" then goto no_sash_f
                p% = 0%
                p%=pos(sa_part$(sa%) = " ")
                if str(dtl_subpart$,5%,1%) = "3" then  /* (AWD002) */    ~
                                           str(sa_part$(sa%),p%,1%) = "F"
                 if str(dtl_subpart$,5%,1%) = "4" then  /* (AWD002) */    ~
                                           str(sa_part$(sa%),p%,1%) = "F"

no_sash_f:

               str(sa_rack$(sa%),1%,4%)  = "@F A"  /* Bin Location     */
               str(sa_rack$(sa%),5%,9%)  = ssq$& "-A" &ssq$& "/"
               str(sa_rack$(sa%),14%,2%) = cp$(i%) /* No. of Pieces    */
               str(sa_rack$(sa%),16%,3%) = "   "

               str(sa_d1$(sa%),1%,2%)    = "0/"    /* No. of Labels    */
               str(sa_d1$(sa%),3%,12%)   = str(co$,1%,12%)
               str(sa_d1$(sa%),15%,2%)   = "/-"

                                                   /* Special Logic    */
               if type$ = "1" then goto L03050
                                                   /* Special for Frame*/
                  if dt_samp$ = "1" or dt_samp$ = "2" then               ~
                                       str(sa_d1$(sa%),15%,2%) = "/S"
                  if str(co$,1%,1%) = "9" then                           ~
                                       str(sa_d1$(sa%),15%,2%) = "/B"
                                                  /* Set Machine Codes */
                  goto L03060
L03050:                                            /* Special for Sash */
                    if set_up$ = "1" then str(sa_d1$(sa%),15%,2%) = "4"  ~
                                     else str(sa_d1$(sa%),15%,2%) = "2"

                                                  /* Special Logic     */
L03060:
               sa_m$(sa%)        = machine$ & "     " /* From Table    */
               sa_s$(sa%)        = set_up$         /* From Table       */
               saw_no$(sa%)      = saw_no$         /* From Table       */
               sa_cut_type$(sa%) = cut_type$       /* Frame or Sash    */
L03430:      next i%
             sa_max% = sa%
        return

        build_descript
            init(" ") co$, x$, s$, readkey$, c_o$, lk$
            cnt% = 3%                            /* 1st Set Model Code */
            str(co$,1%,3%) = str(dtl_part$,1%,3%)
            s$ = str(dtl_part$,11%,1%)           /* Set Screen Code    */
            x$ = str(dtl_part$,12%,1%)           /* Set Lock Code      */
                                                 /* (PAR000)           */
            p% = pos(lk_fn$(1%) = x$)            /* 1 Lock Codes       */
            if p% = 0% then goto L03570
               str(co$,cnt%+1%,3%) = "/1K"
/* (AWD003) */
               if str(sub_part$,16%,1%) = "1" then                  ~
                        str(co$,cnt%+1%,3%) = "/1N"
               if str(sub_part$,16%,1%) = "2" then                  ~
                        str(co$,cnt%+1%,3%) = "/1S"
/* (\AWD003) */
               lk$ = "1"
               goto L03600
                                                 /* (PAR000)           */
L03570:     p% = pos(lk_fn$(2%) = x$)           /* 2 Lock Codes       */
            if p% = 0% then goto L03610
               str(co$,cnt%+1%,3%) = "/2K"
/* (AWD003) */
               if str(sub_part$,16%,1%) = "1" then                  ~
                            str(co$,cnt%+1%,3%) = "/2N"
               if str(sub_part$,16%,1%) = "2" then                  ~
                            str(co$,cnt%+1%,3%) = "/2S"
/* (\AWD003) */
               lk$ = "2"
L03600:     cnt% = cnt% + 3%
L03610:     p% = pos("456" = s$)
            if p% = 0% then goto L03670
               str(co$,cnt%+1%,3%) = "/TS"         /* Set as Default   */
               if s$ = "5" then str(co$,cnt%+1%,3%) = "/BS"
               if s$ = "6" then str(co$,cnt%+1%,3%) = "/FG"
               cnt% = cnt% + 3%
                                                   /* (PAR000)         */
L03670:     p% = pos(lk_fn$(3%) = x$)              /* With Fin Codes   */
            if p% = 0% then goto L03710
               str(co$,cnt%+1%,3%) = "/WF"         /* Set With Fin     */
               cnt% = cnt% + 3%
L03710:     str(readkey$,1%,9%)   = "HINGE    "    /* Check Cot/Oriel  */
            str(readkey$,10%,15%) = str(dtl_part$,9%,2%)
            read #2,key = readkey$, using L03740, desc$, eod goto L03900
L03740:        FMT POS(25), CH(32)
            p% = pos(desc$ = "-")
            if str(desc$,1%,2%) <> "CO" and str(desc$,1%,2%) <> "OR"     ~
                                        then goto L03830
               str(co$,cnt%+1%,3%) = "/CO"         /* Set as Default   */
               c_o$ = "CO"
               if str(desc$,1%,2%) = "OR" then str(co$,cnt%+1%,3%)="/OR"
               if str(desc$,1%,2%) = "OR" then c_o$ = "OR"
               cnt% = cnt% + 3%
L03830:     if p% = 0% then goto L03900
            if str(desc$, p%+2%, 4%) <> "TWIN" and                       ~
               str(desc$, p%+2%, 4%) <> "TRPL" then goto L03900
                  str(co$, cnt%+1%,3%) = "/TW"     /* Set as Default   */
                  if str(desc$,p%+2%,4%) = "TRPL" then                   ~
                     str(co$, cnt%+1%,3%) = "/TR"
               cnt% = cnt% + 3%
L03900:     if str(dtl_part$,9%,2%) <> "09" then goto L03940
               str(co$,cnt%+1%,3%) = "/33"                 /* 1/3,1/3  */
               cnt% = cnt% + 3%

/* (AWD002) */
L03940:     if str(dt_cust$,1,5) <> "BA111" then goto L03945
               str(co$,cnt%+1%,4%) = "/BAY"           /* Bay/Bow      */
               cnt% = cnt% + 4%
L03945: return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%)= "        The File ("&ff_nam$&") Already Exists.   "
            msg$(2%)= "             O P T I M I Z A T I O N             "
            msg$(3%)= "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        check_cut                                    /* 1st Frame      */
                                                     /* 2nd Sash       */
            cut_type% = 0%
            check% = 0%
            init(" ") readkey$, machine$, set_up$, saw_no$, desc$,cut_type$
            str(readkey$,1%,9%)  = "NEWFAMILY"
            str(readkey$,10%,1%) = "F"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
        REM    if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE */
        REM    if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL   */

         /* (CR1196) */   
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
            
             if s$ = "4" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "6"
            if s$ = "4" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "7"
            if s$ = "5" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "8"
            if s$ = "5" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "9"
         /* (CR1196) */
         
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04240
L04180:       FMT POS(25), CH(10)

              machine$ = str(desc$,1%,3%)            /* Machine Code   */
              set_up$   = str(desc$,5%,1%)           /* Set-Up Code    */
              saw_no$   = str(desc$,7%,2%)           /* Saw Number     */
              check%    = 1%                         /* Valid Equation */
              cut_type% = 2%                         /* Frame Part     */
              cut_type$ = "2"                        /* Frame Part     */
            return
                                                     /* Sash Logic     */
L04240:     init(" ") readkey$, machine$, set_up$, saw_no$, desc$
            str(readkey$,1%,9%)  = "NEWFAMILY"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
/* (SR66708) */
REM           if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
REM            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL   */
            if i% > cw% and c_o$ = "CO" then           ~
                          str(readkey$,14%,1%) = "3" /* COTTAGE  */
            if i% > cw% and c_o$ = "OR" then          ~
                          str(readkey$,14%,1%) = "4" /* ORIEL    */


            if s$ = "4" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "6"
            if s$ = "4" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "7"
            if s$ = "5" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "8"
            if s$ = "5" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "9"
/* (SR66708\) */

            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04260

               machine$ = str(desc$,1%,3%)           /* Machine Code   */

            if i% > cw% then goto L04250
               if lk$ = "1" then str(machine$,3%,1%) = "1"

L04250:        set_up$   = str(desc$,5%,1%)          /* Set-Up Code    */
               saw_no$   = str(desc$,7%,2%)          /* Saw Number     */
               check%    = 1%                        /* Valid Equation */
               cut_type% = 1%                        /* Sash Part      */
               cut_type$ = "1"                       /* Sash Part      */
L04260: return

        check_welder                                 /* 1st Frame      */
                                                     /* 2nd Sash       */
            check% = 0%
            init(" ") readkey$, style$(), welder$(), welder_set$(), desc$
            str(readkey$,1%,9%)  = "NEWFLEXWD"
            str(readkey$,10%,1%) = "F"               /* F = Frame Welder*/
                                                     /* S = Sash Welder */
                                                     /* Model No.       */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            read #2,key = readkey$, using L04280 , desc$, eod goto L04290
L04280:       FMT POS(25), CH(25)

              style$(1%)      = str(desc$,1%,15%)    /* Style Code     */
              welder$(1%)     = str(desc$,17%,2%)    /* Welder No.     */
              welder_set$(1%) = str(desc$,20%,1%)    /* Set Up Code    */

              check%    = 1%                         /* Valid Equation */
                                                     /* Sash Logic     */
L04290:     init(" ") readkey$, desc$
            str(readkey$,1%,9%)  = "NEWFLEXWD"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
                                                     /* Mode No.       */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            read #2,key = readkey$, using L04280 , desc$, eod goto L04300

              style$(2%)      = str(desc$,1%,15%)    /* Style Code     */
              welder$(2%)     = str(desc$,17%,2%)    /* Welder No.     */
              welder_set$(2%) = str(desc$,20%,1%)    /* Set Up Code    */

              check%    = 1%                         /* Valid Equation */
/* (AWD004) */
L04300:     extraSash% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)  = "NEWFLEXW2"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
                                                     /* Mode No.       */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            read #2,key = readkey$, using L04280 , desc$, eod goto L04350

              style$(3%)      = str(desc$,1%,15%)    /* Style Code     */
              welder$(3%)     = str(desc$,17%,2%)    /* Welder No.     */
              welder_set$(3%) = str(desc$,20%,1%)    /* Set Up Code    */

              extraSash%    = 1%                    /* Valid Equation */
L04350: return

        assign_schedule
            init(" ") readkey$, sched$
            str(readkey$,1%,9%)   = "PLANSCHED"
            str(readkey$,10%,15%) = "LINEALMA"
            read #2,hold,key = readkey$, using L04320 , sched$,          ~
                                                           eod goto L04400
L04320:       FMT POS(25), CH(3)
            convert sched$ to sched%, data goto L04400

            convert (sched% + 1%) to sched$, pic(000)
            if (sched% + 1%) > 999% then sched$ = "100"
            put #2, using L04320 , sched$
            rewrite #2
        return
L04400:    errormsg$ = "(Error) When assigning New Schedule Number??"
           gosub error_prompt
        return

        exit_program
          if xcount% > 0% then goto L04500
             call "SHOSTAT" ("N O   L I N E A L M A T E   D A T A")
             call "PAUSE" addr(300%)

L04500: end

       build_weld                               /* Find Weld Records F/S */
/*(AWD001) */
REM          if dtl_part$ = save_part$ then return /*Frame 1st-Sash 2nd  */
          if dtl_part$ = save_part$ and save_sub1$ = sub_part$ then return
             save_part$ = dtl_part$
             save_sub1$ = sub_part$

          gosub check_welder                    /* Load Welder info for  */
                                                /* current Model         */
/* (SR66708) */
          gosub check_hgl

          init(" ") width$() , height$(), raw_mat$()
          w%, h%, weld% = 0%

/* (SR66708) record cottage / oriel first and second width cuts */
          co_h1%, co_h2% = 0%
          if str(dtl_part$,11,1) = "4" or               ~
                   str(dtl_part$,11,1) = "5" then w% = 1%
          if str(dtl_part$,11,1) = "4" or               ~
                   str(dtl_part$,11,1) = "5" then h% = 1%
/*(SR66708\)*/

REM          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */      ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                                                        #4, #5, #2, err%)
/* (SR66708) */
             init(" ") tb_w$()   /* CR1002 */
             call "AWDCUTCC" ( dtl_new_part$, 0%, 0%, 0%,    /*(CUT001)*/~
                             0%, cw%, ch%, csw%, csh%,                ~
                             eq$(), ct$(), cr$(), cr_addl$(), cp$(),  ~
                             cc$(), col$(), ct(), sh$(), tw$, th$,    ~
                             s_f$(), die$(), adj(), tb_w$(), #4, #5, #2, err%)
          eq% = cw% + ch% + csw% + csh%
          for i% = 1% to eq%
            if sh$(i%) = "W" or sh$(i%) = "H" then goto L05000
               goto L05020

L05000:     a  = ct(i%)                        /* Welder Sizes are in   */
                                               /* Decimal               */
/* (SR66708) has to be cottage or oriel has to be equations 3 or 4 */
          add% = 0%

               if c_o$ <> "CO" and c_o$ <> "OR"  then goto no_add
               if str(eq$(i%),6,1) <> "3" and str(eq$(i%),6,1) <> "4"  ~
                           then goto no_add
                if c_o$ = "CO" and str(eq$(i%),6,1) <> "3" then goto no_add
                if c_o$ = "OR" and str(eq$(i%),6,1) <> "4" then goto no_add
                   add% = 1%
no_add:

/* (SR66708) if the part is a top sash then 0.8125 width and Height      */
/* so the welder will open for the correct bottom size and the top sash */

            if str(dtl_part$,11,1) = "4" then a = a + 0.8125
            if add% = 1% and str(eq$(i%),7,2) = "05" then a = a + 0.8125
/* (SR66708\) */


            a% = int(a)                        /* Integer Size          */
            b  = ( a - a% ) * 100.0
            b% = int(b)                        /* Fraction Part         */
            convert a% to str(size_w$,1%,3%), pic(000)

            convert b% to str(size_w$,4%,2%), pic(00)

            convert a to size_w$, pic(###.####-) /* Decimal Value       */

            if sh$(i%) = "W" then w% = w% + 1%

            if sh$(i%) = "H" then h% = h% + 1%

            if sh$(i%) = "W" then width$(w%)  = size_w$
            if sh$(i%) = "H" then height$(h%) = size_w$

                                                    /* raw Material    */
            if sh$(i%) = "W" then str(raw_mat$(w%),1%,10%) = cr$(i%)
            if sh$(i%) = "W" then str(raw_mat$(w%),11%,5%) = "     "

/* (SR66708) has to be cottage or oriel has to be equations 3 or 4 */
            if add% <> 1% then goto L05020
                  if str(eq$(i%),7,2) = "05" then co_h1% = h%
                  if str(eq$(i%),7,2) = "06" then co_h2% = h%
/* (SR66708\) */

L05020:   next i%
          weld% = w%
        return

        lookup_color
          if str(dtl_part$,4%,1%) = save_cl$ then return
             save_cl$ = str(dtl_part$,4%,1%)
          init(" ") readkey$
          str(readkey$,1%,15%) = "COLOR    " & str(dtl_part$,4%,1%)
          read #2,key = readkey$, using L05050, cl$, eod goto L05060
L05050:      FMT POS(25), CH(2)
L05060: return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

/* (SR66708) */
        check_hgl
           str(readkey$,1%,9%)   = "HINGE    "    /* Check Cot/Oriel  */
           str(readkey$,10%,15%) = str(dtl_part$,9%,2%)
           read #2,key = readkey$, using L03740, desc$, eod goto no_hgl

           p% = pos(desc$ = "-")
           if str(desc$,1%,2%) <> "CO" and str(desc$,1%,2%) <> "OR"     ~
                                       then goto no_hgl
              c_o$ = "CO"
              if str(desc$,1%,2%) = "OR" then c_o$ = "OR"
        no_hgl
        return

        con_fract                            /* Convert to Sixteenth's */
              calc = round( calc, 4 ) : calc$ = " "
              a% = int(calc) : b% = int((calc - a%) * 10000)
              if b% = 0% then goto L02270                /****************/
                 d% = int(b%/625)                      /* Conversion of*/
                 if mod(b%,625) <> 0 then d% = d% + 1% /* Decimals to  */
                 b% = d%                               /*  Sixteenth's */
                 if b% <> 16% then goto L02270           /****************/
                    a% = a% + 1% : b% = 0%          /* A% = Whole Part */
L02270:       convert a% to str(calc$,1%,3%), pic(###)
              if b% <> 0% then                                           ~
                              str(calc$,5%,5%) = str(sz$,(b%*5%) - 4%,5%)
        return

/* (SR66708\) */