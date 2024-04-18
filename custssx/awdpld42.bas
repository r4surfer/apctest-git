*************************************************************************~
*        Flex Line (W e l d m a t e  and  L i n e a l m a t e )        *~
*                  (PAR000)  01/15/2006 CR347        Dept (049)        *~
*                                                                      *~
*        *Special Size Test for Window Height of Sash's on the Sampson *~
*                 Sash Welder. Don't pass when Window Height less than *~
*                 25.00 Sampson Welder Limitation Frame. (FromAPCPLK42)*~
*                 11.9375 Sash Welder Limitation Sash   (AWD004)       *~
*                                                                      *~
*        (Below Test is Commented out)                                 *~
*        *Special Size Test for set_up$ = '1' for saws                 *~
*                 Add 13/16 or .8125 to Width or Height.               *~
*                 Saw adjustment. (From APCPLM42)                      *~
*                                                                      *~
*                                                                      *~
*                  New Flex Line Processing for 413 Line               *~
*                  ( As of 01/15/2006 - RHH  Dept 049 )                *~
*                                                                      *~
*        AWDPLD42 - Create File with Data for Saw Optimization         *~
*                   with Weld information also.                        *~
*                                                                      *~
*         Subs Replaced - APCPLJ42 (L), APCPLM42 (L)                   *~
*                         APCPLL42 (W), APCPLK42 (W)                   *~
*                                                                      *~
*        (EWD029)                                                      *~
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
* 05/06/07 ! (AWD001) mod for new 413 saws                       ! CMG *~
* 07/10/07 ! (AWD002) mod for combo lines 413/267                ! CMG *~
* 03/05/08 ! (AWD003) mod for sdl                                ! CMG *~
* 08/06/08 ! (AWD004) mod for welder frame and sash limits       ! CMG *~
*07/13/2009! (AWD005) mod for new sill angle                     ! CMG *~
*10/01/2009! (AWD006) changes for optional head expander         ! CMG *~
*10/01/2009! (AWD007) add to descripton if Bay/Bow               ! CMG *~
*04/21/2010! (AWD008) mod for dept 007 - 267C line               ! CMG *~
*03/29/2011! (AWD009) mod for sash limiter                       ! CMG *~
*03/08/2012! (AWD010) mod for cot and oriel, tso and bso         ! CMG *~
*05/19/2014! (CUT001) mod to add dim fields to CUTCC             ! CMG *~
*12/15/2015!(SR70993) mod to add F for Foam to mat'l             ! MES *~
*08/11/2017! (CR1002)  - new top bottom parameter                ! RDB *~
*01/15/2019! (CR1842) F for foam sash parts                      ! MES *~
*01/25/2021! CR2754 add T or B to foam sash F on 268             ! RDB *~
*01/25/2022! CR2985 Skip lookup on the newfamily/newflexd files  ! RDB *~
*03/16/2022! CR2704   Change weld description to show paint      ! RDB *~
*01/17/2023! CR3224 Missing foam on some orders                  ! RDB *~
************************************************************************

        sub "AWDPLD42" (size%,           /* Batch Size (No. Windows)  */ ~
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
            cldesc$30,                   /* color description  CR2704 */ ~               
            width$(50%)9,                /* Width Size - Weld         */ ~
            height$(50%)9,               /* Height Size - Weld        */ ~
            raw_mat$(50%)15,             /* Raw Material              */ ~
            style$(2%)15,                /* Style Information         */ ~
            welder$(2%)2,                /* Welder Numbers            */ ~
            welder_set$(2%)2,            /* Welder Setup Codes        */ ~
            welder_type$(2%)3,           /* Welder Type Frame or Sash */ ~
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
            save_part$25,                /* MFG Part Number Weld      */ ~
            save_part_s$25,              /* MFG Part Number Saw       */ ~
            ref_no$5, dt_ref$8,          /* Part Reference Number     */ ~
            ssq$3, dt_seq$5,             /* Daily Sequence Number     */ ~
            s$1,                         /* Screen Code               */ ~
/*(CR1842)*/foam$1,                      /* Foam Codes                */ ~
            dt_samp$1,                   /* 0=NA, 1=SAMP, 2=DISP      */ ~
            col$(100%)25,                /* Cut Description           */ ~
            eq$(100%)8,                  /* Equation Codes and Number */ ~
            ct$(100%)9,                  /* Cut Widths and Heights    */ ~
            ct(100%),                    /* Cut Wid/Height Decimal    */ ~
            sh$(100%)1,                  /* Sash Type                 */ ~
            cr$(100%)10,                 /* Raw Material Part Number  */ ~
            cp$(100%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(100%)1,                  /* Cut Piece Yes or No       */ ~
            tw$1,                        /* WIDTH CUT PARTS           */ ~
            th$1,                        /* HEIGHT CUT PARTS          */ ~
            sa_d2$30,                    /* MFG Part Description      */ ~
            sa_type$(100%)2,             /* RECORD TYPE 'SA' OR 'LA'  */ ~
            sa_piece$(100%)4,            /* Number of Pieces's to Cut */ ~
            sa_cut$(100%)9,              /* Cut Size for Piece's      */ ~
            sa_part$(100%)15,            /* Raw Material Part Number  */ ~
            sa_rack$(100%)18,            /* Bin Loc and No. of Pieces */ ~
            sa_d1$(100%)16,              /* Raw Material Description  */ ~
            sa_m$(100%)8,                /* WINDOW TYPE,PROFILE TYPE  */ ~
            sa_m1$(100%)5,               /* WINDOW TYPE,PROFILE TYPE  */ ~
            sa_m2$(100%)256,             /* Fabrication               */ ~
            sa_s$(100%)1,                /* SAW SET-UP NUMBER         */ ~
            sa_cut_type$(100%)1,         /* 2 = Frame, 1 = Sash       */ ~
            type$1,                      /* Test for Fame or Sash     */ ~
            machine$3, set_up$1, lk$1,   /* Store Window Type code    */ ~
            machine1$5,                  /* Machine 1                 */ ~
            saw_no$2, saw_no$(100%)2,    /* Machine Saw Number        */ ~
            ff_nam$8,                    /* Bridge File Name          */ ~
            file$30,                     /* Schedule Title Name       */ ~
            inc$5,                       /* Store Batch Number        */ ~
            inc1$6,                      /* Store Deptment code       */ ~
            inc2$5,                      /* Store 1st Sequence No.    */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79,                 /* Error Message Text        */ ~
            setcode$1,                   /* Set code for CR2754       */ ~
/*(AWD002)*/                                                             ~
            fab1$(5%)1,                  /* First Part of Fab         */ ~
            fab2$(5%)1,                  /* Second Part of Fab        */ ~
            sub_part$20,                 /* Subpart (AWD003)          */ ~
            save_sub$20,                 /* Save Subpart (AWD005)     */ ~
            save_sub1$20                 /* Save subpart1 (AWD005)    */ 

            
/* (AWD010) */
        dim sz$100,                      /* FRACTIONS FOR 16'THS      */ ~
            calc$9,                      /* Temp for Conversion       */ ~
            cut_part$25                  /* Save Cut Part             */

        dim dt_cust$9                    /* Customer Code (AWD007)    */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

                                               /* SET-UP SAW FILE(S)   */
            select #3,  "@SAW049@",                                       ~
                                consec , recsize = 149
                                
            select #7,  "@SAW005@",                                       ~
                                consec , recsize = 149
                                


            ff% = 3% : ff_nam$ = "@SAW049@"     /* DEPT 049 */
/* (AWD008) */
            if scr_dept$ <> "005" then goto not005File
              ff% = 7%
              ff_nam$ = "@SAW005@"

not005File:
        REM ***********************************************************
                                            /* (AWDPLD42) Dept = 49   */
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
            scr_prod$ = "4"                  /* For 400 Series         */
                                             /*    *** IMPORTANT ***   */
            hit% = 0%                        /* Set to (1) When Header */
            bat_no% = 0% : count% = 0%       /* is Built               */
            bat$ = "00"

            init(" ") save_part$, save_part_s$, save_sub$, save_sub1$
                                             /* Starting Schedule      */
            convert sched% to tsched$, pic(###)

                                             /* Batch Size Number      */
            convert size% to size$, pic(###)
            
/* (AWD010) */
           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
                 ~ 3/4 13/16 7/8 15/16"

            call "SHOSTAT" ("Starting Schedule ("&tsched$&") for Dept ("&~
                                                           scr_dept$&")")
            tw$ = "1" : th$ = "2"             /* Load Cut Descriptions */
REM            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
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

L01630:
            dt_ref$   = str(wrk_rec$,1%,8%)   /* Part Reference Number*/

            dt_seq$   = str(wrk_rec$,9%,5%)   /* Prod. Daily Seq. No. */

            dtl_load$ = str(wrk_rec$,29,5%)   /* Appian Load Number   */

            dtl_part$ = str(wrk_rec$,38%,25%) /* MFG Part Number      */
/* (AWD008) */
            call "APCCUTLD" (str(dtl_part$,1,1), cw%, ch%, tw$, th$, #2, err% )

            sub_part$ = str(wrk_rec$,81%,20%) /* Subpart (AWD003)     */

            dt_samp$  = str(wrk_rec$,77%,1%)  /* 0=No, 1=Samp, 2=Disp */

            ref_no$   = str(dt_ref$,4%,5%)    /* Shortend Ref No. Load*/

            ssq$      = str(dt_seq$,3%,3%)    /* Shortend Daily Seq.  */

            model$    = str(dtl_part$,1%,3%)  /* Model Code           */

            s$        = str(dtl_part$,11%,1%) /* Set Screen Code      */
            
            foam$     = str(sub_part$,5%,1%) /*Set foam codes*/ /*(CR1842)*/

            dt_cust$  = str(wrk_rec$,121%,9%) /* Customer (AWD007)    */

                                              /* Skip 4, 5, 6 Sashs   */
            gosub lookup_color                /* Get Color Code       */

/* (AWD010) */
REM            p% = pos("456" = s$)            /* Save value for Special */
REM                                            /* Sashes                 */
REM            if p% <> 0% then goto create_next
            if s$ = "6" then goto create_next
            if len(dtl_part$) < 19 then goto create_next
/* (AWD010) */
/* If tso or bso and it is cottage or oriel then skip*/
            if (s$ = "4" or s$ = "5") and (str(dtl_part$,9,2) >= "70" ~
                 and str(dtl_part$,9,2) <= "97") then goto create_next
            
            if count% = 0% then gosub build_schedule   /* CREATE BATCH */
                                                       /* New Area     */
               gosub build_weld               /* Get Weld Info for     */
                                              /* J% = 1% (Frame)       */
                                              /* J% = 2% (Sash)        */

               gosub build_saw_recs

                   j% = 1%                    /* Frame Weld Record     */
                   type$ = "2"
                   type% = 2%
                   gosub build_detail_weld
                                              /* Frame Saw Records     */
                   seq% = 0%
                   for sa% = 1% to sa_max%
                       gosub build_detail_saw
                   next sa%

                   j% = 2%                    /* Sash Weld Record      */
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
REM          if sa_cut_type$(sa%) <> "1" then

          str(bat_rec$,139%,8%) = sa_m$(sa%)            /* Machine Code*/
          str(bat_rec$,147%,2%) = saw_no$(sa%)          /* Saw Number  */
          str(bat_rec$,149%,1%) = sa_s$(sa%)            /* Saw Set-up  */
                                                        /*  Number     */
          write #ff%, bat_rec$, eod goto L02400

/* (AWD001) */

REM              gosub write_mc_record
        return
L02400:   errormsg$ = saw_err$(type%)
          gosub error_prompt
        return

        build_detail_weld                         /* j% = 1%   (Frame) */
                                                  /* j% = 2%   (Sash)  */
                                                  
/* (AWD010) */
               /* do not write frame record for tso or bso */
          if (s$ = "4" or s$ = "5") and j% = 1% then return

          sash_coor_weld% = 0%
          pass% = 0%
        second_weld
          if type% = 1% and (c_o$ = "OR" or c_o$ = "CO") then    ~
                                 sash_coor_weld% = 1%
/* (\AWD010) */

          init(" ") bat_rec$
          convert (count% + 1%) to seq_w$, pic(###)
          str(bat_rec$,1%,2%)   = "WE"                  /* 'WE' Weld   */
          str(bat_rec$,3%,5%)   = ref_no$               /* Load Number */
          str(bat_rec$,8%,5%)   = "     "
          str(bat_rec$,13%,3%)  = seq_w$                /* Window No.  */
          str(bat_rec$,16%,4%)  = "0001"                /* Unit Qty    */

          str(bat_rec$,20%,9%)  = width$(j%)            /* Width Cut   */
          str(bat_rec$,29%,9%)  = height$(j%)           /* Height Cut  */
          
/*(AWD010)*/
          if sash_coor_weld% = 1% and pass% = 0% then ~
                str(bat_rec$,29%,9%)  = height$(co_h1%) /* Height Cut  */

          if sash_coor_weld% = 1% and pass% = 1% then ~
                str(bat_rec$,29%,9%)  = height$(co_h2%) /* Height Cut  */
/*(\AWD010)*/

                                                        /* Frame/Sash  */
          str(bat_rec$,38%,15%) = style$(j%)            /* Style Code  */

          str(bat_rec$,53%,4%)  = cl$ & "  "            /* Color Code  */
                                                        /* Cart Type Pieces */
          str(bat_rec$,57%,18%) = sa_rack$(1%)
          str(bat_rec$,57%,3%)  = welder_type$(j%)
                                                        /* Bin Location*/
          str(bat_rec$,75%,16%) = "STD - Window    "    /* Part Desc   */
          if str(cldesc$,1%,2%) = "pt" then  ~            
                 str(bat_rec$,75%,16%) = "STD - PAINT     "    /* CR2704 */             
          str(bat_rec$,91%,30%) = sa_d2$                /* Label Desc  */
          str(bat_rec$,121%,1%) = " "                   /* lable Format*/
          str(bat_rec$,122%,2%) = "  "                  /* No. Labels  */
          str(bat_rec$,124%,15%)= "               "     /* License Plat*/
          str(bat_rec$,139%,8%) = "        "            /* Machine Code*/
          str(bat_rec$,147%,2%) = welder$(j%)           /* Welder No.  */
          str(bat_rec$,149%,1%) = welder_set$(j%)       /* Welder Setup*/
                                                        /*  Number     */
REM          if j% = 1% then goto L02500
/*(AWD010) if dept = 005 then skip height check */
          if scr_dept$ = "005" then goto L02500
             aa = 0.0                                   /* Special Test*/
                                                        /* Sash Limit  */
             gosub convert_height

             if j% = 1% and aa < 25.00 then goto L02600

             if j% = 2% and aa < 11.9375 then goto L02600



L02500:   write #ff%, bat_rec$, eod goto L02640

/*(AWD010)*/
          if sash_coor_weld% <> 1% then return
             pass% = pass% + 1%
             if pass% = 1% then goto second_weld

L02600: return

L02640: errormsg$ = welder_err$(j%)
        gosub error_prompt
        return

        convert_height                              /* Of window       */

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
/*(AWD005)*/
REM          if save_part_s$ <> dtl_part$ then goto L02850
             if save_part_s$ <> dtl_part$ or save_sub$ <> sub_part$ ~
                                   then goto L02850

             for i% = 1% to sa_max%
                 str(sa_rack$(i%),5%,9%)  = ssq$& "-A" &ssq$& "/"
             next i%
REM             return           /* CR3224 */

L02850:   save_part_s$ = dtl_part$                 /* NEWFAMILY Info */
          save_sub$    = sub_part$                 /* (AWD005)       */
          init(" ") sa_type$(), sa_piece$(), sa_cut$(), sa_part$(),      ~
                    sa_rack$(), sa_d1$(), sa_d2$, sa_m$(), sa_s$(), saw_no$(), ~
                    sa_cut_type$(), sa_m1$(), sa_m2$()

          call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, #6,  ~
                                                                    err%)
          sa_d2$ = str(apc_prt$,1%,30%)
          
/* (AWD010) on tso calc cut for bso */
          init(" ") cut_part$

          sa% = 0%
          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                                                        #4, #5, #2, err%)
                                                        
REM          call "AWDCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
REM                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
REM                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
 REM                                                       #4, #5, #2, err%)                                                        
          gosub build_descript
          eq% = cw% + ch%
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
        
/* (AWD010) */
              if s$ <> "4" then goto notTSO
/* Only Add to Keeper Rail and Style */
                 if str(eq$(i%),6,3) = "601" then goto addLineal
                 if str(eq$(i%),6,3) = "701" then goto addLineal
                    goto notTSO

addLineal:
                      ct(i%) = ct(i%) + .8125          /* ADD      13/16   */
                      init(" ") sa_cut$(sa%), calc$
                      calc = ct(i%)
                      gosub con_fract
                      sa_cut$(sa%) = calc$

notTSO:


/* (AWD010) */
               if cut_type% <> 1% then goto not_sash
                 if c_o$ <> "OR" and c_o$ <> "CO" then goto not_sash
                   if i% <= cw% then goto not_sash
                     if str(eq$(i%),7%,2%) <> "05" then goto not_sash
                      ct(i%) = ct(i%) + .8125          /* ADD      13/16   */
                      init(" ") sa_cut$(sa%), calc$
                      calc = ct(i%)
                      gosub con_fract
                      sa_cut$(sa%) = calc$

not_sash:
        

               str(sa_part$(sa%),1%,10%) = cr$(i%) /* Raw Material Part*/
               str(sa_part$(sa%),11%,5%) = "     "

/*(SR70993)*/
               if cut_type% <> 2% then goto check_sdl 
               if str(eq$(i%),7%,2%) <> "01" and str(eq$(i%),7%,2%) <> "02" ~
               			then goto check_sdl 
                p% = 0%							   /* (SR70993)        */
                p% = pos(sa_part$(sa%) = " ")
				if foam$ = "3" then  /* (SR70993) */    ~
                                              str(sa_part$(sa%),p%,1%) = "F"
                 if foam$ = "4" then  /* (SR70993) */    ~
                                              str(sa_part$(sa%),p%,1%) = "F"
/* CR2754 */
                 if model$ <> "268" or str(sa_part$(sa%),p%,1%) <> "F" ~ 
                    then goto notcase2754
                    
                 if str(dtl_part$,11%,1%) = "4" then  ~
                     str(sa_part$(sa%),p%,2%) = "TF"
                 if str(dtl_part$,11%,1%) = "5" then  ~
                     str(sa_part$(sa%),p%,2%) = "BF"                    
notcase2754:
/*(SR70993)*/
               
/* (AWD003) - sdl bottom lift rail */
check_sdl:

               if str(sub_part$,8,1) <> "1" then goto not_sdl
               if cut_type% = 1% and str(eq$(i%),7,2) = "09"           ~
                                 then str(sa_part$(sa%),11%,1%) = "S"

not_sdl:

/* (AWD001) must have 1 in subpart pos 13, be a width cut */
/*          and equation number 04 */
/* (AWD006)  */
            if str(eq$(i%),7,2) <> "04" then goto not_opt_head
               if (i% > cw%) then goto not_opt_head


REM               if str(sub_part$,13,1) = "1" and (i% <= cw%) and       ~
                    str(eq$(i%),7,2) = "04" then str(sa_part$(sa%),11,1) = "1"

             REM NE Sill Angle Head Expander
               if str(sub_part$,13,1) = "1" then str(sa_part$(sa%),11,1) = "1"
             REM NE Sill Angle No Head Expander
               if str(sub_part$,13,1) = "2" then str(sa_part$(sa%),11,1) = "3"
             REM Stand Sill Angle no Head Expander
               if str(sub_part$,13,1) = "4" then str(sa_part$(sa%),11,1) = "2"

not_opt_head:

/* (\AWD006)  */

/* (AWD010) */
               init(" ") cmgEq$ : cmgEq$ = eq$(i%)
               if cut_type% <> 1% then goto notSash
REM dont put T or B on Glazing bead part
               if str(dtl_part$,11,1) = "4" and     ~
                    str(eq$(i%),7,2) = "06" then goto noBead
               if str(dtl_part$,11,1) = "4" and     ~
                    str(eq$(i%),7,2) = "02" then goto noBead
               if str(dtl_part$,11,1) = "5" and     ~
                    str(eq$(i%),7,2) = "10" then goto noBead
               if str(dtl_part$,11,1) = "5" and     ~
                    str(eq$(i%),7,2) = "02" then goto noBead

               if str(dtl_part$,11,1) = "4"          ~
                              then str(sa_part$(sa%),11%,1%) = "T"

               if str(dtl_part$,11,1) = "5"          ~
                              then str(sa_part$(sa%),11%,1%) = "B"
               
               if model$ <> "268" then goto noBead
               if (foam$ <> "3" and foam$ <> "4") then goto noBead
                  if str(eq$(i%),6,3) = "108" or str(eq$(i%),6,3) = "206"     ~
                              then str(sa_part$(sa%),11%,1%) = "F" /*(CR1842)*/
/* CR2754 */
              init(" ") setcode$
              setcode$ = str(sa_part$(sa%),11%,1%)
              if str(dtl_part$,11,1) = "4" and                 ~
                 (setcode$ = "F" or setcode$ = "T")       then ~
                    str(sa_part$(sa%),11%,2%) = "TF"          
              if str(dtl_part$,11,1) = "5" and                 ~
                 (setcode$ = "F" or setcode$ = "B")       then ~
                    str(sa_part$(sa%),11%,2%) = "BF" 

noBead:
               if c_o$ <> "OR" and c_o$ <> "CO" then goto not_co_or
               if str(eq$(i%),7,2) = "05"         ~
                              then str(sa_part$(sa%),11%,1%) = "T"

               if str(eq$(i%),7,2) = "06"         ~
                              then str(sa_part$(sa%),11%,1%) = "B"
               
               if model$ <> "268" then goto not_co_or
               if (foam$ <>"3" and foam$ <> "4") then goto not_co_or
                  if str(eq$(i%),7,2) = "05" or str(eq$(i%),7,2) = "06"      ~
                              then str(sa_part$(sa%),12%,1%) = "F" /*(CR1842)*/
/* CR2754 */
              init(" ") setcode$
              setcode$ = str(sa_part$(sa%),12%,1%)
              if str(dtl_part$,11%,1%) = "4" and               ~
                 (setcode$ = "F" or setcode$ = "T")       then ~
                    str(sa_part$(sa%),12%,2%) = "TF"          
              if str(dtl_part$,11%,1%) = "5" and               ~
                 (setcode$ = "F" or setcode$ = "B")       then ~
                    str(sa_part$(sa%),12%,2%) = "BF" 
notSash:
not_co_or:

/* (\AWD010) */

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
/* (AWD001) */
               sa_m1$(sa%)       = machine1$       /* From Table    */

/*(AWD002)*/

REM            if dt_seq$ = "00009" then ~
            call "SHOSTAT" (" Set fab " )
REM            if dt_seq$ = "00009" then stop


               pos% = 1%
               for fab% = 1% to 5%
                 if fab1$(fab%) <> " " then str(sa_m2$(sa%),pos%,26%) =    ~
                      "$WKP;" & hex(22) & fab1$(fab%) & hex(22) & ";" & ~
                       fab2$(fab%) & ";0.00;0.00;0.00;"
                 pos% = pos% + 26%
               next fab%
/*(AWD002)*/
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
/* (AWD009) */
               if str(sub_part$,16%,1%) = "1" then str(co$,cnt%+1%,3%) = "/1N"
               if str(sub_part$,16%,1%) = "2" then str(co$,cnt%+1%,3%) = "/1S"
/* (\AWD009) */
               lk$ = "1"
               goto L03600
                                                  /* (PAR000)           */
L03570:     p% = pos(lk_fn$(2%) = x$)             /* 2 Lock Codes       */
            if p% = 0% then goto L03610
               str(co$,cnt%+1%,3%) = "/2K"
/* (AWD009) */
               if str(sub_part$,16%,1%) = "1" then str(co$,cnt%+1%,3%) = "/2N"
               if str(sub_part$,16%,1%) = "2" then str(co$,cnt%+1%,3%) = "/2S"
/* (\AWD009) */
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
/* (AWD007) */
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
            init(" ") readkey$, machine$, set_up$, saw_no$, desc$, cut_type$
            init(" ") machine1$
/* (AWD002) */

            if scr_dept$ <> "049" then goto not049
            found% = 0%
            gosub check_newfamil2
            if found% = 1% then return
/* (AWD002/) */
/* (AWD008) */
not049
            if scr_dept$ <> "005" then goto not005
            found% = 0%
            gosub check_newfamil3
            convert found% to found$,  pic(000)         
            if found% = 1% then return

not005
/* (AWD002/) */
            return                 /* CR2985 Stop using NEWFAMILY */
            str(readkey$,1%,9%)  = "NEWFAMILY"
            str(readkey$,10%,1%) = "F"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
        REM    if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
        REM    if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
/* (AWD010) */
            if s$ = "4" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "6"
            if s$ = "4" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "7"
            if s$ = "5" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "8"
            if s$ = "5" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "9"
                         
            
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04240
L04180:       FMT POS(25), CH(30)

              machine$ = str(desc$,1%,3%)            /* Machine Code   */
              machine1$ = str(desc$,17%,5%)          /* (AWD001) machine 1*/
REM              call "SHOSTAT" ("Machine1 --> " & machine1$)  stop
              set_up$   = str(desc$,5%,1%)           /* Set-Up Code    */
              saw_no$   = str(desc$,7%,2%)           /* Saw Number     */
              check%    = 1%                         /* Valid Equation */
              cut_type% = 2%                         /* Frame Part     */
              cut_type$ = "2"                        /* Frame Part     */

              gosub lookup_fabrication               /* (AWD002) */
            return
                                                     /* Sash Logic     */
L04240:     init(" ") readkey$, machine$, set_up$, saw_no$, desc$
            str(readkey$,1%,9%)  = "NEWFAMILY"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
/* (AWD010) moved stmt up */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
            
REM            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
REM            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */

            if i% > cw% and c_o$ = "CO" then           ~
                          str(readkey$,14%,1%) = "3" /* COTTAGE  */
            if i% > cw% and c_o$ = "OR" then          ~
                          str(readkey$,14%,1%) = "4" /* ORIEL    */

/* (AWD010) */
            if s$ = "4" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "6"
            if s$ = "4" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "7"
            if s$ = "5" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "8"
            if s$ = "5" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "9"

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

/* (AWD002) */
            if scr_dept$ <> "049" then goto notWeld049
            found% = 0%
            gosub check_newflexw2
            if found% = 1% then return
/* (AWD002/) */
/* (AWD008) */
notWeld049:
            if scr_dept$ <> "005" then goto notWeld005
            found% = 0%
            gosub check_newflexw3
            if found% = 1% then return
notWeld005:
/* (AWD002/) */
            return                   /* CR2985 Stop processing NEWFLEXD */
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


/* (AWD002) */
        lookup_fabrication
REM            if dt_seq$ = "00009" then ~
            call "SHOSTAT" (" Lookup fab " )
REM            if dt_seq$ = "00009" then stop
            init(" ") fab1$(), fab2$()
            str(readkey$,1,9) = "FABRICATI"
            read #2,key = readkey$, using L04280 , desc$, eod goto nofab

            flex% = 1%
            for fab% = 1% to 30% step 4%

               if str(desc$,fab%,3%) = " " then goto next_fab
               fab1$(flex%) = str(desc$,fab%,1%)
               fab2$(flex%) = str(desc$,fab%+2%,1%)
                   flex% = flex% + 1%

next_fab
            next fab%
nofab
        return

/* (\AWD002)*/

/* (AWD002) */
        check_newfamil2
            str(readkey$,1%,9%)  = "NEWFAMIL2"
            str(readkey$,10%,1%) = "F"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
        REM    if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
        REM    if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
/* (AWD010) */
            if s$ = "4" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "6"
            if s$ = "4" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "7"
            if s$ = "5" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "8"
            if s$ = "5" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "9"
                         
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04245

              machine$ = str(desc$,1%,3%)            /* Machine Code   */
              machine1$ = str(desc$,17%,5%)          /* (AWD001) machine 1*/
REM              call "SHOSTAT" ("Machine1 --> " & machine1$)  stop
              set_up$   = str(desc$,5%,1%)           /* Set-Up Code    */
              saw_no$   = str(desc$,7%,2%)           /* Saw Number     */
              check%    = 1%                         /* Valid Equation */
              cut_type% = 2%                         /* Frame Part     */
              cut_type$ = "2"                        /* Frame Part     */

              gosub lookup_fabrication               /* (AWD002) */
              found% = 1%
            return
                                                     /* Sash Logic     */
L04245:     init(" ") readkey$, machine$, set_up$, saw_no$, desc$
            str(readkey$,1%,9%)  = "NEWFAMIL2"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
/* (AWD010) moved stmt up */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */

REM            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
REM            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */

            if i% > cw% and c_o$ = "CO" then           ~
                          str(readkey$,14%,1%) = "3" /* COTTAGE  */
            if i% > cw% and c_o$ = "OR" then          ~
                          str(readkey$,14%,1%) = "4" /* ORIEL    */

/* (AWD010) */
            if s$ = "4" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "6"
            if s$ = "4" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "7"
            if s$ = "5" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "8"
            if s$ = "5" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "9"
                         
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04265

               machine$ = str(desc$,1%,3%)           /* Machine Code   */
               machine1$ = str(desc$,17%,5%)         /* (AWD001) Machine 1*/
REM              call "SHOSTAT" ("Machine1 --> " & machine1$)stop
               set_up$   = str(desc$,5%,1%)          /* Set-Up Code    */
               saw_no$   = str(desc$,7%,2%)          /* Saw Number     */
               check%    = 1%                        /* Valid Equation */
               cut_type% = 1%                        /* Sash Part      */
               cut_type$ = "1"                       /* Sash Part      */
               found% = 1%
L04265: return


/* (AWD008) */
        check_newfamil3
            str(readkey$,1%,9%)  = "NEWFAMIL3"
            str(readkey$,10%,1%) = "F"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
        REM    if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
        REM    if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
/* (AWD010) */
            if s$ = "4" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "6"
            if s$ = "4" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "7"
            if s$ = "5" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "8"
            if s$ = "5" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "9"
                         
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04250

              machine$ = str(desc$,1%,3%)            /* Machine Code   */
              machine1$ = str(desc$,17%,5%)          /* (AWD001) machine 1*/
REM              call "SHOSTAT" ("Machine1 --> " & machine1$)  stop
              set_up$   = str(desc$,5%,1%)           /* Set-Up Code    */
              saw_no$   = str(desc$,7%,2%)           /* Saw Number     */
              check%    = 1%                         /* Valid Equation */
              cut_type% = 2%                         /* Frame Part     */
              cut_type$ = "2"                        /* Frame Part     */

              gosub lookup_fabrication               /* (AWD002) */
              found% = 1%
            return
                                                     /* Sash Logic     */
L04250:     init(" ") readkey$, machine$, set_up$, saw_no$, desc$
            str(readkey$,1%,9%)  = "NEWFAMIL3"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
/* (AWD010) moved stmt up */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */

REM            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
REM            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */

            if i% > cw% and c_o$ = "CO" then           ~
                          str(readkey$,14%,1%) = "3" /* COTTAGE  */
            if i% > cw% and c_o$ = "OR" then          ~
                          str(readkey$,14%,1%) = "4" /* ORIEL    */

/* (AWD010) */
            if s$ = "4" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "6"
            if s$ = "4" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "7"
            if s$ = "5" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "8"
            if s$ = "5" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "9"
                         
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04270

               machine$ = str(desc$,1%,3%)           /* Machine Code   */
               machine1$ = str(desc$,17%,5%)         /* (AWD001) Machine 1*/
REM              call "SHOSTAT" ("Machine1 --> " & machine1$)stop
               set_up$   = str(desc$,5%,1%)          /* Set-Up Code    */
               saw_no$   = str(desc$,7%,2%)          /* Saw Number     */
               check%    = 1%                        /* Valid Equation */
               cut_type% = 1%                        /* Sash Part      */
               cut_type$ = "1"                       /* Sash Part      */
               found% = 1%
L04270: return
/* (AWD008\) */

        check_newflexw2
            found% = 0%
            init(" ") readkey$, style$(), welder$(), welder_set$(), desc$
            str(readkey$,1%,9%)  = "NEWFLEXW2"
            str(readkey$,10%,1%) = "F"               /* F = Frame Welder*/
                                                     /* S = Sash Welder */
                                                     /* Model No.       */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            read #2,key = readkey$, using L04280 , desc$, eod goto L04291

              style$(1%)      = str(desc$,1%,15%)    /* Style Code     */
              welder$(1%)     = str(desc$,17%,2%)    /* Welder No.     */
              welder_set$(1%) = str(desc$,20%,1%)    /* Set Up Code    */

              found%    = 1%                         /* Valid Equation */
                                                     /* Sash Logic     */
L04291:     init(" ") readkey$, desc$
            str(readkey$,1%,9%)  = "NEWFLEXW2"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
                                                     /* Mode No.       */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            read #2,key = readkey$, using L04280 , desc$, eod goto L04301

              style$(2%)      = str(desc$,1%,15%)    /* Style Code     */
              welder$(2%)     = str(desc$,17%,2%)    /* Welder No.     */
              welder_set$(2%) = str(desc$,20%,1%)    /* Set Up Code    */

              found%    = 1%                         /* Valid Equation */
L04301: return


/* (AWD002/) */

/* (AWD008) */
        check_newflexw3
            found% = 0%
            init(" ") readkey$, style$(), welder$(), welder_set$(), desc$
            str(readkey$,1%,9%)  = "NEWFLEXW3"
            str(readkey$,10%,1%) = "F"               /* F = Frame Welder*/
                                                     /* S = Sash Welder */
                                                     /* Model No.       */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            read #2,key = readkey$, using L04280 , desc$, eod goto L04292

              style$(1%)      = str(desc$,1%,15%)    /* Style Code     */
              welder$(1%)     = str(desc$,17%,2%)    /* Welder No.     */
              welder_set$(1%) = str(desc$,20%,1%)    /* Set Up Code    */

              found%    = 1%                         /* Valid Equation */
                                                     /* Sash Logic     */
L04292:     init(" ") readkey$, desc$
            str(readkey$,1%,9%)  = "NEWFLEXW3"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
                                                     /* Mode No.       */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            read #2,key = readkey$, using L04280 , desc$, eod goto L04302

              style$(2%)      = str(desc$,1%,15%)    /* Style Code     */
              welder$(2%)     = str(desc$,17%,2%)    /* Welder No.     */
              welder_set$(2%) = str(desc$,20%,1%)    /* Set Up Code    */

              found%    = 1%                         /* Valid Equation */
L04302: return

/* (AWD008\) */

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
/* (AWD005) */
REM          if dtl_part$ = save_part$ then return /* Frame 1st - Sash 2nd  */
             if dtl_part$ = save_part$ and save_sub1$ = sub_part$ then return
             save_part$ = dtl_part$
             save_sub1$ = sub_part$

          gosub check_welder                    /* Load Welder info for  */
                                                /* current Model         */
/* (AWD010) */
          gosub check_hgl
          
          init(" ") width$() , height$(), raw_mat$()
          w%, h%, weld% = 0%
/* (AWD010) record cottage / oriel first and second width cuts */
          co_h1%, co_h2% = 0%
          if str(dtl_part$,11,1) = "4" or               ~
                   str(dtl_part$,11,1) = "5" then w% = 1%
          if str(dtl_part$,11,1) = "4" or               ~
                   str(dtl_part$,11,1) = "5" then h% = 1%
/*(AWD010\)*/
          
          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                                                        #4, #5, #2, err%)
          eq% = cw% + ch%
          for i% = 1% to eq%
            if sh$(i%) = "W" or sh$(i%) = "H" then goto L05000
               goto L05020

L05000:     a  = ct(i%)                        /* Welder Sizes are in   */
                                               /* Decimal               */
                                               
* (AWD010) has to be cottage or oriel has to be equations 3 or 4 */
          add% = 0%
          if c_o$ <> "CO" and c_o$ <> "OR"  then goto no_add
               if str(eq$(i%),6,1) <> "3" and str(eq$(i%),6,1) <> "4"  ~
                           then goto no_add
                if c_o$ = "CO" and str(eq$(i%),6,1) <> "3" then goto no_add
                if c_o$ = "OR" and str(eq$(i%),6,1) <> "4" then goto no_add
                   add% = 1%
no_add:

/* (AWD010) if the part is a top sash then 0.8125 width and Height      */
/* so the welder will open for the correct bottom size and the top sash */

            if str(dtl_part$,11,1) = "4" then a = a + 0.8125
            if add% = 1% and str(eq$(i%),7,2) = "05" then a = a + 0.8125
/* (AWD010\) */
                                               
                                               
            a% = int(a)                        /* Integer Size          */
            b  = ( a - a% ) * 100.0
            b% = int(b)                        /* Fraction Part         */
            convert a% to str(size_w$,1%,3%), pic(000)

            convert b% to str(size_w$,4%,2%), pic(00)

            init(" ") size_w$

            convert a to size_w$, pic(###.####-) /* Decimal Value       */

            if sh$(i%) = "W" then w% = w% + 1%

            if sh$(i%) = "H" then h% = h% + 1%

            if sh$(i%) = "W" then width$(w%)  = size_w$
            if sh$(i%) = "H" then height$(h%) = size_w$

                                                    /* raw Material    */
            if sh$(i%) = "W" then str(raw_mat$(w%),1%,10%) = cr$(i%)
            if sh$(i%) = "W" then str(raw_mat$(w%),11%,5%) = "     "
            
/* (AWD010) has to be cottage or oriel has to be equations 3 or 4 */
            if add% <> 1% then goto L05020
                  if str(eq$(i%),7,2) = "05" then co_h1% = h%
                  if str(eq$(i%),7,2) = "06" then co_h2% = h%
/* (AWD010\) */

L05020:   next i%
          weld% = w%
        return

        lookup_color
          if str(dtl_part$,4%,1%) = save_cl$ then return
             save_cl$ = str(dtl_part$,4%,1%)
          init(" ") readkey$, cldesc$                            /* CR2704 */
          str(readkey$,1%,15%) = "COLOR    " & str(dtl_part$,4%,1%)
          read #2,key = readkey$, using L05050, cl$, cldesc$, eod goto L05060
L05050:      FMT POS(25), CH(2), POS(30), CH(30)                 /* CR2704 */
L05060: return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        write_mc_record
          if sa_cut_type$(sa%) <> "1" then goto write_frame

          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "MC"
          str(bat_rec$,3%,10%)  = ref_no$               /* End of Warr */
          str(bat_rec$,13%,3%)  = seq$                  /* Item Number */
          str(bat_rec$,16%,4%)  = " "                   /* Reserved    */
          str(bat_rec$,20%,5%)  = "$LDB;"               /* Special     */
          str(bat_rec$,25%,2%)  = str(sa_m1$(sa%),1,2)
          str(bat_rec$,27%,1%)  = ";"
          str(bat_rec$,28%,2%)  = str(sa_m1$(sa%),4,2)
          str(bat_rec$,30%,1%)  = ";"



          write #ff%, bat_rec$, eod goto bad_mc_rec



          return

write_frame

          if sa_m1$(sa%) = " " then goto check_fab
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "MC"
          str(bat_rec$,3%,10%)  = ref_no$               /* End of Warr */
          str(bat_rec$,13%,3%)  = seq$                  /* Item Number */
          str(bat_rec$,16%,4%)  = " "                   /* Reserved    */
          str(bat_rec$,20%,5%)  = "$LDB;"               /* Special     */
          str(bat_rec$,25%,2%)  = str(sa_m1$(sa%),1,2)
          str(bat_rec$,27%,1%)  = ";"
          str(bat_rec$,28%,2%)  = str(sa_m1$(sa%),4,2)
          str(bat_rec$,30%,1%)  = ";"

          if sa_m1$(sa%) = " " then goto write_ldb

             str(bat_rec$,31,118) = sa_m2$(sa%)
write_ldb

          write #ff%, bat_rec$, eod goto bad_mc_rec

          return
/* (AWD002) */
check_fab
          if sa_m2$(sa%) = " " then return

REM            if dt_seq$ = "00009" then ~
            call "SHOSTAT" (" WRITE fab " )
REM            if dt_seq$ = "00009" then stop

          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "MC"
          str(bat_rec$,3%,10%)  = ref_no$               /* End of Warr */
          str(bat_rec$,13%,3%)  = seq$                  /* Item Number */
          str(bat_rec$,16%,4%)  = " "                   /* Reserved    */

          str(bat_rec$,20%,129%) = sa_m2$(sa%)


          write #ff%, bat_rec$, eod goto bad_mc_rec



        bad_mc_rec
        return

/* (AWD010) */
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

/* (AWD010\) */


