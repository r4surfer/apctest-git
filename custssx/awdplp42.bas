**************************************************************************~
*        Flex Line (W e l d m a t e  and  L i n e a l m a t e )          *~
*                                                    Dept (008)          *~
*                                                                        *~
*        *Special Size Test for Window Height of Sash's on the Sampson   *~
*                 Sash Welder. Don't pass when Window Height less than   *~
*                 28.5. Sampson Sash Welder Limitation. (From APCPLK42)  *~
*                                                                        *~
*        (Below Test is Commented out)                                   *~
*        *Special Size Test for set_up$ = '1' for saws                   *~
*                 Add 13/16 or .8125 to Width or Height.                 *~
*                 Saw adjustment. (From APCPLM42)                        *~
*                                                                        *~
*                                                                        *~
*                  New Flex Line Processing for Casement Line            *~
*                  ( As of 06/11/2007 - CMG  Dept 008 )                  *~
*                                                                        *~
*        AWDPLP42 - Create File with Data for Saw Optimization           *~
*                   with Weld information also.                          *~
*                                                                        *~
*         Subs Replaced - APCPLO42    , APCPLP42                         *~
*                                                                        *~
*        (EWD029)                                                        *~
*            File Layout                                                 *~
*                 FR930318                                               *~
*                 SN123 <Schedule Number>(100-999)                       *~
*                 TI<Title Record>(30)                                   *~
*                 TW<?????>                                              *~
*                 WE  <Frame>                                            *~
*                 SA  <Frame Saw>                                        *~
*                 SA  <Frame Saw> Etc.                                   *~
*                 WE  <SASH>                                             *~
*                 SA  <Sash Saw>                                         *~
*                 SA  <Sash Saw>                                         *~
*                 WE  <Frame> Etc.                                       *~
*                 '                                                      *~
*                 end                                                    *~
*                 FR                                                     *~
*                                                                        *~
*            Note(1)Primary Build Subs  'build_weld'    (Frame - Sash)   *~
*                                       'build_saw_recs (Frame - Sash)   *~
*                                                                        *~
*                (2)Primary Detail Subs 'build_detail_weld'              *~
*                                       'build_detail_saw'               *~
*                                                                        *~
*                   Each called once for Frame and once for Sash         *~
*                                                                        *~
*                (3)Primary Frame-Sash Sub 'check_cuts' for              *~
*                   Frame and Sash. Uses the "NEWFAMILY' Table           *~
*                   Obtains, Machine Code, Setup No, Saw No.             *~
*                                                                        *~
*                (4)Fields 15, 16, 17 are used for those machine that    *~
*                   require (Machine Code), (Machine Number) and         *~
*                   (Machine setup No.)                                  *~
*                                                                        *~
*                (5)Primary Welder Frams - Sash Sub 'check_welder' for   *~
*                   Frame and Sash Codes. Uses 'NEWFLEXWD' Table.        *~
*                   Loads <Style> <Welder No.> <Welder Setup Code>       *~
*                                                                        *~
*                (6)When changes are made to the following Tables, the   *~
*                   subroutine will need to be checked for               *~
*                   Modifications.                                       *~
*                   build_descript uses (SCREEN, LOCK, HINGE) Tables     *~
*                                                                        *~
*                (7)Special Code Screen Codes for Sashs (4,5,6) are      *~
*                   not put in the Frame or Sash Data.                   *~
*                                                                        *~
*                                                                        *~
* 02/12/2008 ! (AWD001) Mod to add 1 to both Frame and Sash Cuts   ! CMG *~
* 05/16/2011 ! (AWD002) mod to descr on pw casement models for WS  ! CMG *~
*11/04/2011  ! (AWD003) mod to look up quantities from SASHQTY table!CMG *~
*12/16/2009  ! (AWD004) add to descripton if Bay/Bow               ! CMG *~
*05/21/2012  ! (AWD005) replace apccutcc with awdcutcc             ! CMG *~
*03/19/2013  ! (AWD006) mod to awdcutcc addl data                  ! CMG *~
*02/25/2014  ! (AWD007) mod to add extra line for 3lite center sash! MES *~
*06/03/2014  ! (CUT001) mod to add dim fields to CUTCC             ! MES *~
*08/11/2017  ! (CR1002)  - new top bottom parameter                ! RDB *~
**************************************************************************

        sub "AWDPLP42" (size%,           /* Batch Size (No. Windows)  */ ~
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
            width$(5%)9,                 /* Width Size - Weld         */ ~
            height$(5%)9,                /* Height Size - Weld        */ ~
            raw_mat$(5%)15,              /* Raw Material              */ ~
/*(AWD007)*/style$(3%)15,                /* Style Information         */ ~
/*(AWD007)*/welder$(3%)2,                /* Welder Numbers            */ ~
/*(AWD007)*/welder_set$(3%)2,            /* Welder Setup Codes        */ ~
/*(AWD007)*/welder_type$(3%)3,           /* Welder Type Frame or Sash */ ~
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
            dtl_subpart$20,              /* (AWD005) subpart          */ ~
            dtl_new_part$45,             /* (AWD005) entire part      */ ~
            save_part$25,                /* MFG Part Number Weld      */ ~
            save_part_s$25,              /* MFG Part Number Saw       */ ~
            ref_no$5, dt_ref$8,          /* Part Reference Number     */ ~
            ssq$3, dt_seq$5,             /* Daily Sequence Number     */ ~
            dt_samp$1,                   /* 0=NA, 1=SAMP, 2=DISP      */ ~
            col$(500%)25,                /* Cut Description           */ ~
            eq$(500%)8,                  /* Equation Codes and Number */ ~
            ct$(500%)9,                  /* Cut Widths and Heights    */ ~
            ct(500%),                    /* Cut Wid/Height Decimal    */ ~
            sh$(500%)1,                  /* Sash Type                 */ ~
            cr$(500%)10,                 /* Raw Material Part Number  */ ~
            cr_addl$(500%)5,             /* (AWD005) addl material num*/ ~
/*AWD006*/  s_f$(500%)1,                 /* Sash / Frame               */~
/*AWD006*/  die$(500%)15,                /* Die Number                 */~
/*AWD006*/  adj(500%),                   /* Adjustment amt             */~            
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
            machine1$5,                  /* Machine 1                 */ ~
            saw_no$2, saw_no$(500%)2,    /* Machine Saw Number        */ ~
            ff_nam$7,                    /* Bridge File Name          */ ~
            file$30,                     /* Schedule Title Name       */ ~
            inc$5,                       /* Store Batch Number        */ ~
            inc1$6,                      /* Store Deptment code       */ ~
            inc2$5,                      /* Store 1st Sequence No.    */ ~
            s$1, x$2, y$1,               /* Schedule File Identifier  */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79,                 /* Error Message Text        */ ~
            tb_w$(500%)1                 /* T/B cut (CR1002)          */ 

/* (AWD002) */
        dim location$2,                  /* Text Location             */ ~
            srch_mdl$100                 /* Models to search          */

        dim dt_cust$9,                   /* Customer Code (AWD004)    */ ~
            sav_cust$9                   /* Save Customer (AWD004)    */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

                                               /* SET-UP SAW FILE(S)   */
            select #3,  "@SAW008@",                                       ~
                                consec , recsize = 149


            ff% = 3% : ff_nam$ = "@SAW008@"     /* DEPT 008 */

        REM ***********************************************************
                                            /* (AWDPLP42) Dept = 08   */
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

/* (AWD002) */
            srch_mdl$ = "819,827,D19,D27,D28,D30,D38"

        REM ***********************************************************

            cw%, ch% = 0%
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
            scr_prod$ = "8"                  /* For Casement Series    */
                                             /*    *** IMPORTANT ***   */
            hit% = 0%                        /* Set to (1) When Header */
            bat_no% = 0% : count% = 0%       /* is Built               */
            bat$ = "00"

            init(" ") save_part$, save_part_s$
                                             /* Starting Schedule      */
            convert sched% to tsched$, pic(###)

                                             /* Batch Size Number      */
            convert size% to size$, pic(###)

            call "SHOSTAT" ("Starting Schedule ("&tsched$&") for Dept ("&~
                                                           scr_dept$&")")
            tw$ = "1" : th$ = "2"             /* Load Cut Descriptions */
            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
            if err% <> 0% then goto exit_program
            xcount% = 0%
            wrk_key1$ = all(hex(00))
                                             /* New Family of Windows  */
            read #1,key > wrk_key1$, using L01620 , wrk_key1$, wrk_rec$,   ~
                                                     eod goto create_done
            goto L01630
        create_next
            read #1, using L01620 , wrk_key1$, wrk_rec$,                   ~
                                                     eod goto create_done
L01620:          FMT POS(6), CH(51), CH(200)

L01630:     dt_ref$   = str(wrk_rec$,1%,8%)   /* Part Reference Number*/

            dt_seq$   = str(wrk_rec$,9%,5%)   /* Prod. Daily Seq. No. */

            dtl_load$ = str(wrk_rec$,29,5%)   /* Appian Load Number   */

            dtl_part$ = str(wrk_rec$,38%,25%) /* MFG Part Number      */

            dtl_subpart$ = str(wrk_rec$,81%,20%)                  /*(AWD005)*/
            str(dtl_new_part$,1%,25%)  = str(dtl_part$,1%,25%)    /*(AWD005)*/
            str(dtl_new_part$,26%,20%) = str(dtl_subpart$,1%,20%) /*(AWD005)*/

            dt_samp$  = str(wrk_rec$,77%,1%)  /* 0=No, 1=Samp, 2=Disp */

            ref_no$   = str(dt_ref$,4%,5%)    /* Shortend Ref No. Load*/

            ssq$      = str(dt_seq$,3%,3%)    /* Shortend Daily Seq.  */

            model$    = str(dtl_part$,1%,3%)  /* Model Code           */

            s$        = str(dtl_part$,11%,1%) /* Set Screen Code      */
                                              /* Skip 4, 5, 6 Sashs   */
            dt_cust$  = str(wrk_rec$,121%,9%) /* Customer (AWD004)    */

            gosub lookup_color                /* Get Color Code       */

            p% = pos("456" = s$)            /* Save value for Special */
                                            /* Sashes                 */
            if p% <> 0% then goto create_next
            if len(dtl_part$) < 19 then goto create_next
            if count% = 0% then gosub build_schedule   /* CREATE BATCH */
                                                       /* New Area     */
               gosub build_weld               /* Get Weld Info for     */
                                              /* J% = 1% (Frame)       */
                                              /* J% = 2% (Sash)        */

               gosub build_saw_recs

                   j% = 1%                    /* Frame Weld Record     */
 /*(AWD007)*/      k% = 1%
                   type$ = "2"
                   type% = 2%
                   gosub build_detail_weld
                                              /* Frame Saw Records     */
                   seq% = 0%
                   for sa% = 1% to sa_max%
                       gosub build_detail_saw
                   next sa%

                   j% = 2%                    /* Sash Weld Record      */
/*(AWD007)*/       k% = 2%        
                   type$ = "1"
                   type% = 1%
                   gosub build_detail_weld
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
/* (AWD001) not sash */
          if sa_cut_type$(sa%) <> "1" then                              ~
          str(bat_rec$,139%,8%) = sa_m$(sa%)            /* Machine Code*/
          str(bat_rec$,147%,2%) = saw_no$(sa%)          /* Saw Number  */
          str(bat_rec$,149%,1%) = sa_s$(sa%)            /* Saw Set-up  */
                                                        /*  Number     */
          write #ff%, bat_rec$, eod goto L02400

/* (AWD001) */

        return
L02400:   errormsg$ = saw_err$(type%)
          gosub error_prompt
        return

        build_detail_weld                         /* j% = 1%   (Frame) */
                                                  /* j% = 2%   (Sash)  */
          init(" ") bat_rec$
          convert (count% + 1%) to seq_w$, pic(###)
          str(bat_rec$,1%,2%)   = "WE"                  /* 'WE' Weld   */
          str(bat_rec$,3%,5%)   = ref_no$               /* Load Number */
          str(bat_rec$,8%,5%)   = "     "
          str(bat_rec$,13%,3%)  = seq_w$                /* Window No.  */
          str(bat_rec$,16%,4%)  = "0001"                /* Unit Qty    */
/* (AWD003) */
                                               /* Modify Weld Quantity */
          if j% = 2% then gosub check_sash_qty

          str(bat_rec$,20%,9%)  = width$(j%)            /* Width Cut   */
          str(bat_rec$,29%,9%)  = height$(j%)           /* Height Cut  */

                                                        /* Frame/Sash  */
          str(bat_rec$,38%,15%) = style$(j%)            /* Style Code  */

          str(bat_rec$,53%,4%)  = cl$ & "  "            /* Color Code  */
                                                        /* Cart Type Pieces */
          str(bat_rec$,57%,18%) = sa_rack$(1%)
          str(bat_rec$,57%,3%)  = welder_type$(j%)
                                                        /* Bin Location*/
          str(bat_rec$,75%,16%) = "STD - Window    "    /* Part Desc   */
          str(bat_rec$,91%,30%) = sa_d2$                /* Label Desc  */
          str(bat_rec$,121%,1%) = " "                   /* lable Format*/
          str(bat_rec$,122%,2%) = "  "                  /* No. Labels  */
          str(bat_rec$,124%,15%)= "               "     /* License Plat*/
          str(bat_rec$,139%,8%) = "        "            /* Machine Code*/
          str(bat_rec$,147%,2%) = welder$(j%)           /* Welder No.  */
          str(bat_rec$,149%,1%) = welder_set$(j%)       /* Welder Setup*/
                                                        /*  Number     */

          write #ff%, bat_rec$, eod goto L02640
        return

L02640: errormsg$ = welder_err$(j%)
        gosub error_prompt
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
          if save_part_s$ <> dtl_part$ then goto L02850
/* (AWD004) */
             if sav_cust$ <> dt_cust$ then goto L02850

             for i% = 1% to sa_max%
                 str(sa_rack$(i%),5%,9%)  = ssq$& "-A" &ssq$& "/"
             next i%
             return

L02850:   save_part_s$ = dtl_part$                 /* NEWFAMILY Info */
          sav_cust$    = dt_cust$                  /* (AWD004) */
          init(" ") sa_type$(), sa_piece$(), sa_cut$(), sa_part$(),      ~
                    sa_rack$(), sa_d1$(), sa_d2$, sa_m$(), sa_s$(), saw_no$(), ~
                    sa_cut_type$()

          call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, #6,  ~
                                                                    err%)
          sa_d2$ = str(apc_prt$,1%,30%)

          sa% = 0%
/*(AWD005)*/
REM          CALL "APCCUTCC" (DTL_PART$, 0%, CW%, CH%, EQ$(), CT$(), CR$(), ~
REM                     CP$(), CC$(), COL$(), CT(), SH$(), TW$, TH$,        ~
REM                                                        #4, #5, #2, ERR%)
                                          /* (AWD006) */
          init(" ") tb_w$()   /* CR1002 */
          call "AWDCUTCC" ( dtl_new_part$,  0%, 0%, 0%, /* (CUT001) */    ~
                             0%, cw%, ch%, csw%, csh%,                    ~
                             eq$(), ct$(), cr$(), cr_addl$(), cp$(),      ~
                             cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                             s_f$(), die$(), adj(), tb_w$(), #4, #5, #2, err%)

          gosub build_descript
          eq% = cw% + ch% + csw% + csh%
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
        REM          ct(i%) = ct(i%) + .8125          /* ADD      13/16   */
        REM          init(" ") sa_cut$(sa%)
        REM          convert ct(i%) to str(sa_cut$(sa%),1%,7%), pic(###.###)

               str(sa_part$(sa%),1%,10%) = cr$(i%) /* Raw Material Part*/
               str(sa_part$(sa%),11%,5%) = "     "

                                                   /* Frame Equations  */
REM               if cut_type% <> 2% then goto L03260  /* (AWD001) */
                                  /* Both Frame and Sash Width Cuts*/
                  if i% >  cw% then goto L03260    /* Width  Eq        */
                  str(sa_part$(sa%),11%,1%) = "1"


L03260:        str(sa_rack$(sa%),1%,4%)  = "@F A"  /* Bin Location     */
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
/* (AWD005) */
REM               if dt_seq$ = "00076" then call "SHOSTAT" ("I AM HERE ") 
REM               if dt_seq$ = "00076" then stop
               p% = 0%
               for k% = 1% to 5% 
                if str(sa_part$(sa%),10%+k%,1%) <> " " then p%=k%
                if p% <> 0% then k% = 5%
               next k%
               str(sa_part$(sa%),p%+11,5%-len%) = str(cr_addl$(i%),1%,5%-len%)
L03430:      next i%
             sa_max% = sa%
        return

        build_descript
            init(" ") co$, x$, s$, readkey$, c_o$, lk$, y$
            cnt% = 3%                            /* 1st Set Model Code */
            str(co$,1%,3%) = str(dtl_part$,1%,3%)
            s$ = str(dtl_part$,11%,1%)           /* Set Screen Code    */
            y$ = str(dtl_part$,12%,1%)           /* Lock Code          */
            x$ = str(dtl_part$,9%,2%)            /* Set hinge Code     */
                                                 /* (PAR000)           */

            str(co$,cnt%+1%,3%) = "/" & x$       /* Use the Actual Hinge*/
            cnt% = cnt% + 3%                     /* Code in Descript   */
            p% = pos("456" = s$)
            if p% = 0% then goto L03600
               str(co$,cnt%+1%,3%) = "/TS"         /* Set as Default   */
               if s$ = "5" then str(co$,cnt%+1%,3%) = "/BS"
               if s$ = "6" then str(co$,cnt%+1%,3%) = "/FG"
               cnt% = cnt% + 3%

L03600:     p% = pos(lk_fn$(3%) = y$)              /* With Fin Codes   */
            if p% = 0% then goto L03640
               str(co$,cnt%+1%,3%) = "/WF"         /* Set With Fin     */
               cnt% = cnt% + 3%
L03640:     str(readkey$,1%,9%)   = "HINGE    "    /* Check Cot/Oriel  */
            str(readkey$,10%,15%) = str(dtl_part$,9%,2%)
            read #2,key = readkey$, using L03670, desc$, eod goto L03830
L03670:        FMT POS(25), CH(32)
            p% = pos(desc$ = "-")
            if str(desc$,1%,2%) <> "CO" and str(desc$,1%,2%) <> "OR"     ~
                                        then goto L03760
               str(co$,cnt%+1%,3%) = "/CO"         /* Set as Default   */
               c_o$ = "CO"
               if str(desc$,1%,2%) = "OR" then str(co$,cnt%+1%,3%)="/OR"
               if str(desc$,1%,2%) = "OR" then c_o$ = "OR"
               cnt% = cnt% + 3%
L03760:     if p% = 0% then goto L03830
            if str(desc$, p%+2%, 4%) <> "TWIN" and                       ~
               str(desc$, p%+2%, 4%) <> "TRPL" then goto L03830
                  str(co$, cnt%+1%,3%) = "/TW"     /* Set as Default   */
                  if str(desc$,p%+2%,4%) = "TRPL" then                   ~
                     str(co$, cnt%+1%,3%) = "/TR"
               cnt% = cnt% + 3%
L03830:     if str(dtl_part$,9%,2%) <> "09" then goto L03870
               str(co$,cnt%+1%,3%) = "/33"                 /* 1/3,1/3  */
               cnt% = cnt% + 3%

L03870:     location$ = all(hex(00))
            search srch_mdl$ = str(dtl_part$,1%,3%) to location$ step 4
            if str(location$,1,2) = hex(0000) then goto L03880
             if str(dtl_part$,23,3) <> " " then goto checkWD1
             if str(dtl_part$,20,3) <> " " then goto checkWD2
               goto L03880
checkWD1:
              if str(dtl_part$,23,1) < "A" then goto L03880
               goto WDFOUND
checkWD2:
              if str(dtl_part$,20,1) < "A" then goto L03880
WDFOUND:
               str(co$,7%,6%) = "/WDSUR"
/* (AWD004) */
L03880:     if str(dt_cust$,1,5) <> "BA111" then goto L03890
               str(co$,cnt%+1%,4%) = "/BAY"           /* Bay/Bow      */
               cnt% = cnt% + 4%
L03890: return


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
            init(" ") readkey$, machine$, set_up$, saw_no$, desc$, cut_type$
            init(" ") machine1$
            str(readkey$,1%,9%)  = "NEWFAMILY"
            str(readkey$,10%,1%) = "F"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
        REM    if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
        REM    if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04240
L04180:       FMT POS(25), CH(30)

              machine$ = str(desc$,1%,3%)            /* Machine Code   */
              machine1$ = str(desc$,18%,5%)          /* (AWD001) machine 1*/
REM              call "SHOSTAT" ("Machine1 --> " & machine1$)  stop
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
            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04260

               machine$ = str(desc$,1%,3%)           /* Machine Code   */
               machine1$ = str(desc$,17%,5%)         /* (AWD001) Machine 1*/
REM              call "SHOSTAT" ("Machine1 --> " & machine1$)stop
               set_up$   = str(desc$,5%,1%)          /* Set-Up Code    */
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
L04300: return

        assign_schedule
            init(" ") readkey$, sched$
            str(readkey$,1%,9%)   = "PLANSCHED"
            str(readkey$,10%,15%) = "LINEALMA"
            read #2,hold,key = readkey$, using L04320 , sched$,            ~
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
          if dtl_part$ = save_part$ then return /* Frame 1st - Sash 2nd  */
             save_part$ = dtl_part$

          gosub check_welder                    /* Load Welder info for  */
                                                /* current Model         */
          init(" ") width$() , height$(), raw_mat$()
          w%, h%, weld% = 0%
REM          call "APCCUTCC" (dtl_part$, 0%, cw%, ch%, eq$(), ct$(), cr$(), ~
REM                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
REM                                                        #4, #5, #2, err%)

REM          CALL "AWDCUTCC" ( DTL_NEW_PART$, 0%, CW%, CH%, CSW%, CSH%,   ~
                             EQ$(), CT$(), CR$(), CR_ADDL$(), CP$(),   ~
                             CC$(), COL$(), CT(), SH$(), TW$, TH$,     ~
                             #4, #5, #2, ERR%)
          init(" ") tb_w$()   /* CR1002 */
          call "AWDCUTCC" ( dtl_new_part$, 0%, 0%, 0%, /* (CUT001) */  ~
                             0%, cw%, ch%, csw%, csh%,                 ~
                             eq$(), ct$(), cr$(), cr_addl$(), cp$(),   ~
                             cc$(), col$(), ct(), sh$(), tw$, th$,     ~
                             s_f$(), die$(), adj(), tb_w$(), #4, #5, #2, err%)
                             
          eq% = cw% + ch% + csw% + csh%
          for i% = 1% to eq%
            if sh$(i%) = "W" or sh$(i%) = "H" then goto L05000
               goto L05020

L05000:     a  = ct(i%)                        /* Welder Sizes are in   */
                                               /* Decimal               */
            a% = int(a)                        /* Integer Size          */
            b  = ( a - a% ) * 100.0
            b% = int(b)                        /* Fraction Part         */
            convert a% to str(size_w$,1%,3%), pic(000)

            convert b% to str(size_w$,4%,2%), pic(00)

            init(" ") size_w$

            convert a to size_w$, pic(###.####-) /* Decimal Value       */

            if sh$(i%) = "W" then w% = w% + 1%
            if w% > 5% then w% = 5%

            if sh$(i%) = "H" then h% = h% + 1%
            if h% > 5% then h% = 5%

            if sh$(i%) = "W" then width$(w%)  = size_w$
            if sh$(i%) = "H" then height$(h%) = size_w$

                                                    /* raw Material    */
            if sh$(i%) = "W" then str(raw_mat$(w%),1%,10%) = cr$(i%)
            if sh$(i%) = "W" then str(raw_mat$(w%),11%,5%) = "     "

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
/* (AWD003) */
        check_sash_qty
            str(readkey$,1%,9%)   = "SASHQTY  "
            str(readkey$,10%,15%) = str(dtl_part$,1%,3%)
            qty% = 0%
            read #2,key = readkey$, using L03670, desc$, eod goto no_sashqty

            convert str(desc$,1%,4%) to qty%, data goto no_sashqty

            convert qty% to str(bat_rec$,16%,4%), pic(0000)
        no_sashqty
        return
/* (\AWD003) */

