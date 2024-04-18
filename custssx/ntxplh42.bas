************************************************************************~
*        Flex Line (W e l d m a t e  and  L i n e a l m a t e )        *~
*             (PAR000) 01/15/06 CR1722            NTX Dept (071)       *~
*             Models (JW1)                                             *~
*																	   *~	                    
*             First NTX Linealmate file                                *~  
*        (Below Test has been commented out)                           *~ 
*        *Special Size Test for set_up$ = '1' for saws                 *~
*                 Add 13/16 or .8125 to Width or Height.               *~
*                 Saw adjustment. (From EWDPLB42)                      *~
*                                                                      *~
*        *Special Code for Sash Weld quanties                          *~
*                 Not Applicable                                       *~
*                                                                      *~
*                                                                      *~
*        *Special Code for Frame Saws for Cot/Orl                      *~
*                 Only Height Cuts                                     *~ 
*                 Cottage Machine code "3"                             *~
*                 Oriel Machine Code "4"                               *~
*                                                                      *~                                  
*                  New Flex Line Processing for Brickmould             *~
*                  ( As of 01/15/2005 - RHH  Dept 027 )                *~
*                                                                      *~
*        AWDPLU42 - Create File with Data for Saw Optimization         *~
*                   with Weld information also.                        *~
*                                                                      *~
*         Subs Replaced - EWDPLZ42 (L-F/S), EWDPLH42 (W-F/S)           *~
*                                                                      *~
*        (EWD033)                                                      *~                                          
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
*----------------------------------------------------------------------*~
*                             M O D I F I C A T I O N S                *~
*--------------WHEN---+----------------WHAT----------------------+-WHO-*~
*           08/04/2010! (AWD001) mod to allow sash only          ! CMG *~
*           06/06/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
*           10/15/2014! (AWD002) mod to add Sturtz new           ! MES *~
*           12/23/2014! (AWD003) mod for no PW Sash weld line    ! MES *~
*           04/15/2016!(SR73939) mod to add dept 057             ! MES *~
*           08/11/2017!(CR1002)  new top bottom parameter        ! RDB *~ 
*           01/23/2019!(CR1722)  PlyGem Saws Dept 071            ! MES *~
************************************************************************

        sub "NTXPLH42" (size%,           /* Batch Size (No. Windows)  */ ~
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
            bat_rec$222,                 /* Batch Record              */ ~
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
            sclmr$3,                     /* Mtg Rail Location CR1722  */ ~
            hgt$3,                       /* Wd Hgt CR1722             */~
            hng$2,                       /* Hinge Code  CR1722        */ ~     
            dt_samp$1,                   /* 0=NA, 1=SAMP, 2=DISP      */ ~
            col$(100%)25,                /* Cut Description           */ ~
            eq$(100%)8,                  /* Equation Codes and Number */ ~
            ct$(100%)9,                  /* Cut Widths and Heights    */ ~
            ct(100%),                    /* Cut Wid/Height Decimal    */ ~
            sh$(100%)1,                  /* Sash Type                 */ ~
            cr$(100%)10,                 /* Raw Material Part Number  */ ~
            cr_addl$(100%)5,             /* (AWD002) addl raw material */~            
            cp$(100%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(100%)1,                  /* Cut Piece Yes or No       */ ~
            tw$1,                        /* WIDTH CUT PARTS           */ ~
            th$1,                        /* HEIGHT CUT PARTS          */ ~
            tsw$1,                       /* (AWD002) WH Type SubPart  */ ~
            tsh$1,                       /* (AWD002) HT Type SubPart  */ ~             
            sa_d2$30,                    /* MFG Part Description      */ ~
            sa_type$(100%)2,             /* RECORD TYPE 'SA' OR 'LA'  */ ~
            sa_piece$(100%)4,            /* Number of Pieces's to Cut */ ~
            sa_cut$(100%)9,              /* Cut Size for Piece's      */ ~
            sa_part$(100%)15,            /* Raw Material Part Number  */ ~
            sa_rack$(100%)18,            /* Bin Loc and No. of Pieces */ ~
            sa_d1$(100%)16,              /* Raw Material Description  */ ~
            sa_m$(100%)8,                /* WINDOW TYPE,PROFILE TYPE  */ ~
            sa_s$(100%)1,                /* SAW SET-UP NUMBER         */ ~
            sa_cut_type$(100%)1,         /* 2 = Frame, 1 = Sash       */ ~
            type$1,                      /* Test for Fame or Sash     */ ~
            machine$3, set_up$1, lk$1,   /* Store Window Type code    */ ~
            saw_no$2, saw_no$(100%)2,    /* Machine Saw Number        */ ~
            ff_nam$7,                    /* Bridge File Name          */ ~
            file$30,                     /* Schedule Title Name       */ ~
            inc$5,                       /* Store Batch Number        */ ~
            inc1$6,                      /* Store Deptment code       */ ~
            inc2$5,                      /* Store 1st Sequence No.    */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79,                  /* Error Message Text        */ ~
            sub_part$20,                 /* Subpart (AWD002)          */ ~
            die$(100%)15,                /* (AWD002) Die Number       */ ~
            dtl_new_part$45,             /* (AWD002) New Part Number  */ ~
            save_newpart_s$45,           /* (AWD002) MFG Part Number  */ ~            
            save_newpart_w$45,           /* (AWD002) mfg part Number  */ ~ 
            sa_jpart$(100%)20,           /* (AWD002) Sturtz cluster   */ ~
            sa_jmodel$(100%)20,          /* (AWD002) die+color        */ ~
            jpart$20,                    /* (AWD002) Sturtz cluster   */ ~
            nosash$2,                    /* (AWD003) no sash weld     */ ~
            tb_w$(500%)1,                /* T/B cut (CR1002)          */ ~
            fm_calc$(100%)5,             /* Fmr Hole Calc (CR1722)    */ ~
            fm_bal$(100%)5               /* FM Bal Hole Calc (CR1722) */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */
            
            dept% = 0%                   /* (AWD002)                   */
            convert scr_dept$ to dept%, data goto ntx_dept
            
ntx_dept:
                                               /* SET-UP SAW FILE(S)   */
            select #3,  "@SAW071@",                                       ~
                                consec , recsize = 222  /*(AWD002)*/
                                

            ff% = 3% : ff_nam$ = "@SAW071@"     /* DEPT 071 */

REM StartFile            

        REM ***********************************************************
                                            /* (NTXPLH42) Dept = 71 */
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
                                             /* (AWD033) - Special Note*/
            scr_prod$ = "5"                  /* For 500 Series        */
            hit% = 0%                        /* Set to (1) When Header */
            bat_no% = 0% : count% = 0%       /* is Built               */      
            bat$ = "00"

            init(" ") save_part$, save_part_s$, save_newpart_s$, save_newpart_w$ 
                                             /* Starting Schedule      */
            convert sched% to tsched$, pic(###)

                                             /* Batch Size Number      */
            convert size% to size$, pic(###)

            call "SHOSTAT" ("Starting Schedule ("&tsched$&") for Dept ("&~
                                                           scr_dept$&")")
                                                           
            init(" ") tsw$, tsh$     /* (AWD002) */                                               
            tw$ = "1" : th$ = "2"             /* Load Cut Descriptions */
REM          call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
REM            if err% <> 0% then goto exit_program
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

L01630:     dt_ref$   = str(wrk_rec$,1%,8%) /* Part Reference Number  */

            dt_seq$   = str(wrk_rec$,9%,5%) /* Prod. Daily Seq. No.   */           

            dtl_load$ = str(wrk_rec$,29,5%) /* Appian Load Number     */

            dtl_part$ = str(wrk_rec$,38%,25%) /* MFG Part Number      */
            
            scr_prod$ = str(dtl_part$,1%,1%)           
            call "AWDCUTLD" (scr_prod$, cw%, ch%, csw%, csh%, tw$, ~ 
                            th$, tsw$, tsh$,#2, err% )
            if err% <> 0% then goto create_next
            
            sub_part$ = str(wrk_rec$,81%,20%) /* Subpart (AWD002)     */
/*(AWD002)*/            
            dtl_new_part$ = str(dtl_part$,1%,25%) & str(sub_part$,1%,20%)

            hng$      = str(dtl_part$,9%,2%)  /* (AWD002) */

            dt_samp$  = str(wrk_rec$,77%,1%)  /* 0=No, 1=Samp, 2=Disp */

            ref_no$   = str(dt_ref$,4%,5%)    /* Shortend Ref No. Load*/

            ssq$      = str(dt_seq$,3%,3%)    /* Shortend Daily Seq.  */

            model$    = str(dtl_part$,1%,3%)  /* Model Code           */

            s$        = str(dtl_part$,11%,1%) /* Set Screen Code      */
                                              /* Skip 4, 5, 6 Sashs   */
                                              
            sclmr$    = str(dtl_part$,20%,3%) /* Mtg Rail (CR1722)    */
            
            hgt$      = str(dtl_part$,17%,3%) /* Mtg Rail (CR1722)    */
            
            gosub lookup_color                /* Get Color Code       */

/* (AWD001) */
REM            p% = pos("456" = s$)
REM            if p% <> 0% then goto create_next
            if len(dtl_part$) < 19 then goto create_next
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
                                                      /* New Area      */
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
          inc$  = " (" & bat$ & ")"                   /*  Batch No (5) */
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
          if sa_type$(sa%) = "FM" then goto L02615
          str(bat_rec$,1%,2%)   = sa_type$(sa%)         /* 'SA'        */
          str(bat_rec$,3%,5%)   = ref_no$               /* Load Number */
          str(bat_rec$,8%,5%)   = "     "
          str(bat_rec$,13%,3%)  = seq$                  /* Item Number */
          str(bat_rec$,16%,4%)  = sa_piece$(sa%)        /* Unit Qty    */
          str(bat_rec$,20%,9%)  = sa_cut$(sa%)          /* Piece Cut   */
          str(bat_rec$,29%,9%)  = "        "            /* Reserved    */
REM       str(bat_rec$,38%,15%) = sa_part$(sa%)         /* Part No.    */
          str(bat_rec$,38%,15%) = sa_jmodel$(sa%)       /* InvMat No.  */
          												/* (AWD002)    */
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
          str(bat_rec$,183%,20%) = sa_jpart$(sa%)      /* Sturtz ID (AWD002)*/
REM       str(bat_rec$,203%,20%) = sa_jmodel$(sa%)      /* Not needed(AWD002)*/
                                                        /*  Number     */
                                                        
          write #ff%, bat_rec$, eod goto L02620
REM          if i% > cw% and eqNum$ = "01" then goto write_fm_balance2
          if saw_no$(sa%) = "94" then goto write_fm_balance2
        return
 L02615:   if sa_jmodel$(sa%) = "MTG" then goto write_fm_mtgrail   /*(CR1722)*/
          if sa_jmodel$(sa%) = "BAL" then goto write_fm_balance    
          return          

        
L02620:   errormsg$ = saw_err$(type%)
          gosub error_prompt
        return

        build_detail_weld                         /* j% = 1%   (Frame) */
                                                  /* j% = 2%   (Sash)  */
                                                  
/* (AWD002) */
          if type% = 2% and s$ = "4" then return
          if type% = 2% and s$ = "5" then return
          
          nosash$ = str(model$, 2%, 2%)                 /* (AWD003)   */
          if j% = 2% and nosash$ = "PW"  ~
          		then goto L02635  						/* (AWD003)   */ 
          if j% = 2% and nosash$ = "TP"  ~
          		then goto L02635  						/* (AWD003)   */ 
          		
          init(" ") bat_rec$
          convert (count% + 1%) to seq_w$, pic(###)
          str(bat_rec$,1%,2%)   = "WE"                  /* 'WE' Weld   */
          str(bat_rec$,3%,5%)   = ref_no$               /* Load Number */
          str(bat_rec$,8%,5%)   = "     "
          str(bat_rec$,13%,3%)  = seq_w$                /* Window No.  */
          str(bat_rec$,16%,4%)  = "0001"                /* Unit Qty    */
                                               /* Modify Weld Quantity */

          str(bat_rec$,20%,9%)  = width$(j%)            /* Width Cut   */
          str(bat_rec$,29%,9%)  = height$(j%)           /* Height Cut  */
                                                 
                                                        /* Frame/Sash  */
          str(bat_rec$,38%,15%) = style$(j%)            /* Style Code  */

          str(bat_rec$,53%,4%)  = cl$ & "  "            /* color Code  */
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

          write #ff%, bat_rec$, eod goto L02640
L02635:          
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
          if save_newpart_s$ <> dtl_new_part$ then goto L02850
             for i% = 1% to sa_max%
                 str(sa_rack$(i%),5%,9%)  = ssq$& "-A" &ssq$& "/"
             next i%
             return

L02850:   save_newpart_s$ = dtl_new_part$                 /* NEWFAMILY Info */
          init(" ") sa_type$(), sa_piece$(), sa_cut$(), sa_part$(),      ~
                    sa_rack$(), sa_d1$(), sa_d2$, sa_m$(), sa_s$(), saw_no$(), ~
                    sa_cut_type$()

          call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, #6,  ~
                                                                    err%)
          sa_d2$ = str(apc_prt$,1%,30%)

          sa% = 0%
REM          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
REM                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
REM                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                                                        #4, #5, #2, err%)
             init(" ") tb_w$()   /* CR1002 */
/*(AWD002)*/ call "AWDCUTCC" ( dtl_new_part$, 0%, 0%, 0%, /* (CUT001) */  ~
                             0%, cw%, ch%, csw%, csh%,                  ~
                             eq$(), ct$(), cr$(), cr_addl$(), cp$(),    ~
                             cc$(), col$(), ct(), sh$(), tw$, th$,      ~
                             s_f$(), die$(), adj(), tb_w$(),#4, #5, #2, err%)
                             
         gosub build_descript
          eq% = cw% + ch%
          for i% = 1% to eq%
        REM *RHH*                               /* REMOVE 'LA' RECORDS */
            if cc$(i%) = "N" or cc$(i%) = " " then goto L03430

            cut% = 0%                    /* Skip - No Equation Records */
            convert str(ct$(i%),1%,3%) to cut%, data goto L03040
L03040:
            if cut% = 0% then goto L03430
            eqNum$ = str(eq$(i%),7%,2%)     /* (AWD002) */
               gosub check_cut                 /* Check NEWFAMILY Info */
               if check% = 0% then goto L03430 /* Skip Equation        */

               sa% = sa% + 1%                  /* Save Table Info.     */
               sa_type$(sa%)  = "SA"
            if i% > cw% and (eqNum$ = "08" or eqNum$ = "10") ~
                   then sa_type$(sa%) = "FM" /*(CR1722)*/
            if i% < cw% then goto L03050       
            if i% > cw% and eqNum$ <> "01" then goto L03050    

                fm_bal$(sa%) = "00134"        /*std balance hole */
                if hng$ = "00" then goto L03045
          
                                                      /* SCLMR balance hole */
                convert str(sclmr$,3%,1%) to cdec
                convert str(sclmr$,1%,2%) to calc
                
                convert str(hgt$,3%,1%) to hdec
                convert str(hgt$,1%,2%) to hcalc
                
                hcalc = hcalc + (hdec/8.0)
          
                calc = calc + (cdec/8.0)  
                calc = calc * 2  
                calc = calc - hcalc
                if calc < 1.000 then calc = 1.00 
                calc = calc + .34
      
                        
                  a = calc
                   a% = int(a)
                   b = (a-a%)*100.0
                   b% = int(b)
                   convert a% to str(fm_bal$(sa%),1%,3%), pic(000)
                   convert b% to str(fm_bal$(sa%),4%,2%), pic(00)
                
L03045:                 
          
        REM       if cc$(i%) = "N" then sa_type$(sa%) = "LA"

L03050:               
               sa_piece$(sa%) = "  " & cp$(i%)     /* No Pieces Needed */
               sa_cut$(sa%)   = ct$(i%)            /* Length of Cut    */

        REM       if set_up$ <> "1" then goto L03240
        REM          ct(i%) = ct(i%) + .8125          /* ADD      13/16   */
        REM          init(" ") sa_cut$(sa%)
        REM          convert ct(i%) to str(sa_cut$(sa%),1%,7%), pic(###.###)
        
               if sa_type$(sa%) <> "FM" then goto L03240            /*(CR1722)*/
                   a = ct(i%)
                   a% = int(a)
                   b = (a-a%)*100.0
                   b% = int(b)
                   convert a% to str(fm_calc$(sa%),1%,3%), pic(000)
                   convert b% to str(fm_calc$(sa%),4%,2%), pic(00)

                

/*(AWD002)*/        
REM               str(sa_part$(sa%),1%,7%) = cr$(i%) /* Raw Material Part*/
REM               str(sa_part$(sa%),8%,2%) = cr_addl$(i%)
REM               str(sa_part$(sa%),10%,6%) = "    "
  
L03240: 
               str(sa_rack$(sa%),1%,4%)  = "@F A"  /* Bin Location     */
               str(sa_rack$(sa%),5%,9%)  = ssq$& "-A" &ssq$& "/"
               str(sa_rack$(sa%),14%,2%) = cp$(i%) /* No. of Pieces    */
               str(sa_rack$(sa%),16%,3%) = "   "

               str(sa_d1$(sa%),1%,2%)    = "0/"    /* No. of Labels    */
               str(sa_d1$(sa%),3%,12%)   = str(co$,1%,12%)
               str(sa_d1$(sa%),15%,2%)   = "/-"
               
/*(AWD002) non std changes to frame jambs*/
  			if cut_type% <> 2% then goto NoFab  /* Only Frames (AWD002) */
                    sh% = 1%         /*(AWD002*/
REM                 	if model$ = "5S2"  then slider% = 2%     /*(CR1722)*/
REM                 	if model$ = "8S2"  then slider% = 2%     /*(CR1722)*/
REM                 	if model$ = "5S3" then slider% = 3%    /*(CR1722)*/
REM                 	if model$ = "8S3" then slider% = 3%    /*(CR1722)*/
                                                       /* Skip Meeting Rail */
REM                  	if eqNum$ = "33" and slider% = 0%  then goto NoFab   
REM                 	if i% > cw% and eqNum$ = "39" and slider% <> 0%  ~
REM                         then goto NoFab  /*slider jambs*/
                  	p% = 0%
                 	 p% = pos(sa_part$(sa%) = " ")
             
REM            if slider% = 2% or slider% = 3% then gosub sliderhs
               if sh% = 1% then gosub shjamb     	     	 
                  
                 	 
                 	 
                
NoFab:                 	  	 
               sa_m$(sa%)        = machine$ & "     "
               sa_s$(sa%)        = set_up$         /* From Table      */
               saw_no$(sa%)      = saw_no$         /* From Table      */
               sa_cut_type$(sa%) = cut_type$       /* Frame or Sash   */
               sa_jpart$(sa%)    = jpart$          /*(AWD002)         */
               sa_jmodel$(sa%)   = die$(i%)        /*(AWD002)         */        
L03430:      next i%
             sa_max% = sa%
        return

REM    Begin Slider H&S non std (AWD002) not needed with CR1722 but left for reference
REM		sliderhs
REM		 if i% > cw% then return
REM		 if eqNum$ <> "41" then return
REM		 if slider% = 2% and hng$ = "07" then jpart$ = "5S225" 
REM		 if slider% = 3% and hng$ = "09" then jpart$ = "5S206" 
		 
		 return
		 
REM Begin SH Jamb non std (CR1722) 
		shjamb
		 if i% <= cw% then return
		 if eqNum$ <> "01" then return
		 if c_o$ = "OR" then jpart$ = "JW104"
		 if c_o$ = "CO" then jpart$ = "JW103"
		 
		 return	        
/*(AWD002) end */        
        
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
               lk$ = "1"
               goto L03600
                                                 /* (PAR000)           */
L03570:     p% = pos(lk_fn$(2%) = x$)            /* 2 Lock Codes       */
            if p% = 0% then goto L03610
               str(co$,cnt%+1%,3%) = "/2K"
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

L03940: return

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
            init(" ") readkey$, machine$, set_up$, saw_no$, desc$,     ~
                 cut_type$, jpart$						/*(AWD002)     */
                 
            if dept% = 71% then str(readkey$,1%,9%)  = "NEWFAMILY" /*(AWD002)*/  

                 
            str(readkey$,10%,1%) = "F"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
REM            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
REM            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */

/* (AWD001) */
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

              machine$  = str(desc$,1%,3%)           /* Machine Code   */
                                                     /* Cottage        */
REM            if i% > cw% and c_o$ = "CO" then str(machine$,3%,1%)  = "3"
                                                     /* oriel          */
REM            if i% > cw% and c_o$ = "OR" then str(machine$,3%,1%)  = "4"

              set_up$   = str(desc$,5%,1%)           /* Set-Up Code    */
              saw_no$   = str(desc$,7%,2%)           /* Saw Number     */
              jpart$ = str(desc$,10%,20%)           /* Urban prt (AWD002)*/        
              check%    = 1%                         /* Valid Equation */
              cut_type% = 2%                         /* Frame Part     */
              cut_type$ = "2"                        /* Frame Part     */
            return
                                                     /* Sash Logic     */
L04240:     init(" ") readkey$, machine$, set_up$, saw_no$, desc$, jpart$

            if dept% = 71% then str(readkey$,1%,9%)  = "NEWFAMILY" /*(AWD002)*/  
                 
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
/*(AWD002)*/            
REM            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
REM            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
            
/* (AWD001) */
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
               set_up$   = str(desc$,5%,1%)          /* Set-Up Code    */
               saw_no$   = str(desc$,7%,2%)          /* Saw Number     */
               jpart$ = str(desc$,10%,20%)           /* Sturtz prt (AWD002)*/               
               check%    = 1%                        /* Valid Equation */
               cut_type% = 1%                        /* Sash Part      */
               cut_type$ = "1"                       /* Sash Part      */ 
L04260: return


        check_welder                                 /* 1st Frame      */
                                                     /* 2nd Sash       */
            check% = 0%
            init(" ") readkey$, style$(), welder$(), welder_set$(), desc$
            
            if dept% = 71% then str(readkey$,1%,9%)  = "NEWFLEXWD" /*(AWD002)*/  
        
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

            if dept% = 71% then str(readkey$,1%,9%)  = "NEWFLEXWD" /*(AWD002)*/  


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
                                               /* Frame 1st - Sash 2nd  */     
       if dtl_new_part$ = save_newpart_w$ then return  
         save_newpart_w$ = dtl_new_part$

          gosub check_welder                    /* Load Welder info for  */
                                                /* current Model         */ 
          init(" ") width$() , height$(), raw_mat$()
          w%, h%, weld% = 0%
          
/* (AWD002) force to sash  */

          if str(dtl_part$,11,1) = "4" or               ~
                   str(dtl_part$,11,1) = "5" then w% = 1%
          if str(dtl_part$,11,1) = "4" or               ~
                   str(dtl_part$,11,1) = "5" then h% = 1%
                   
                   
REM          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
REM                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
REM                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
REM                                                        #4, #5, #2, err%)
           init(" ") tb_w$()   /* CR1002 */
call "AWDCUTCC" ( dtl_new_part$, 0%, 0%, 0%, /* (CUT001) */  ~
                             0%, cw%, ch%, csw%, csh%,                  ~
                             eq$(), ct$(), cr$(), cr_addl$(), cp$(),    ~
                             cc$(), col$(), ct(), sh$(), tw$, th$,      ~
                             s_f$(), die$(), adj(), tb_w$(), #4, #5, #2, err%)
          eq% = cw% + ch%
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

            convert a to size_w$, pic(###.####-) /* Decimal Value       */

            if sh$(i%) = "W" then w% = w% + 1%

            if sh$(i%) = "H" then h% = h% + 1%

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

        write_fm_mtgrail                                  /*(CR1722)*/
          if sa_type$(sa%) <> "FM" then goto L01730

                    
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "FM"
          str(bat_rec$,3%,5%)  = ref_no$                /* End of Warr */
          str(bat_rec$,8%,5%)  = "     "                 /* blanks */
          str(bat_rec$,13%,3%) = "  1"                    /* Item Number */
          str(bat_rec$,16%,15%)  = "MEETINGRAIL    "     /* FM name    */
          str(bat_rec$,31%,2%)  = "  "                  /* blanks     */
          str(bat_rec$,33%,5%)  = fm_calc$(sa%)


          write #ff%, bat_rec$, eod goto L01730
          
REM          goto write_fm_balance
          
L01730:   return

        write_fm_balance
if sa_type$(sa%) <> "FM" then goto L01731
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = sa_type$(sa%)
          str(bat_rec$,3%,5%)  = ref_no$                /* End of Warr */
          str(bat_rec$,8%,5%)  = "     "                  /* blanks */
          str(bat_rec$,13%,3%) = "  1"                    /* Item Number */
          str(bat_rec$,16%,15%)  = "BALANCEHOLE   "     /* FM name    */
          str(bat_rec$,31%,2%)  = "  "                  /* blanks     */
          str(bat_rec$,33%,5%)  = fm_calc$(sa%)


          write #ff%, bat_rec$, eod goto L01731
        
L01731:   return                                         /* (CR1722)*/
          
          write_fm_balance2
REM       if sa_type$(sa%) <> "FM" then goto L01731
          
REM          if hng$ = "00" then fm_bal$ = "00134"        /*std balance hole */
REM          goto L01740
          
                                                      /* SCLMR balance hole */
REM          convert str(sclmr$,3%,1%) to cdec
REM          convert str(sclmr$,1%,2%) to calc
          
REM          calc = calc + (cdec/8.0)  
          
REM          calc = calc * 2
  
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "FM"
          str(bat_rec$,3%,5%)  = ref_no$                /* End of Warr */
          str(bat_rec$,8%,5%)  = "     "                  /* blanks */
          str(bat_rec$,13%,3%) = "  1"                    /* Item Number */
          str(bat_rec$,16%,15%)  = "BALANCEHOLE   "     /* FM name    */
          str(bat_rec$,31%,2%)  = "  "                  /* blanks     */
          str(bat_rec$,33%,5%)  = fm_bal$(sa%)


          write #ff%, bat_rec$, eod goto L01741
        
L01741:   return                                         /* (CR1722)*/
        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
  
