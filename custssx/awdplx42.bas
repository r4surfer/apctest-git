************************************************************************~
*        Flex Line ( L i n e a l m a t e )                             *~
*        Dept (053)                                                    *~
*             Models (511, 512, 513, 514)                              *~
*                                                                      *~  
*        Dept 007  CR2985                                              *~  
*              Models (267)                                            *~
*                                                                      *~  
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
*        AWDPLX42 - Create File with Data for Saw Optimization         *~
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
*                 SA  <Frame Saw>                                      *~
*                 LA  <Label Information>                              *~
*                      Text values and barcodes                        *~
*                 SA  <Frame Saw>                                      *~
*                 LA  <Label Information>                              *~
*                      Text values and barcodes                        *~
*                 SA  <Sash Saw>                                       *~
*                 LA  <Label Information>                              *~
*                      Text values and barcodes                        *~
*                 SA  <Sash Saw>                                       *~
*                 LA  <Label Information>                              *~
*                      Text values and barcodes                        *~
*                 '                                                    *~
*                 end                                                  *~
*                 FR                                                   *~
*                                                                      *~
*            Note(1)Primary Build Subs  'build_weld'    (Frame - Sash) *~
*                                       'build_saw_recs (Frame - Sash) *~
*                                                                      *~
*                (2)Primary Detail Subs 'build_detail_weld no write'   *~
*                                       'build_detail_saw'             *~
*                                                                      *~
*                   Each called once for Frame and once for Sash       *~
*                                                                      *~
*                (3)Primary Frame-Sash Sub 'check_cuts' for            *~
*                   Frame and Sash. Uses the "NEWFAMIL3' Table         *~
*                   Obtains, Machine Code, Setup No, Saw No.           *~
*                                                                      *~
*                (4)Fields 15, 16, 17 are used for those machine that  *~
*                   require (Machine Code), (Machine Number) and       *~
*                   (Machine setup No.)                                *~
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
*           09/01/2021! Copy of dept 027 program  AWDPLF42       ! RDB *~
*           09/27/2021! CR2906 .25 inch adjustment in Caelus file! RDB *~
*           01/06/2022! CR2985 Add dept 007 to Sturts file pgm   ! RDB *~
*           03/16/2022! CR2704  Change weld descrpt to show paint! RDB *~
*           01/06/2023! CR3223 Add F (foam) to die number dept053! RDB *~
*           01/17/2023! CR3224 Missing foam on some dept 007     ! RDB *~
************************************************************************

        sub "AWDPLX42" (size%,           /* Batch Size (No. Windows)  */ ~
                        sched%,          /* Starting Schedule Number  */ ~
                        scr_dte$,        /* Production Date Formatted */ ~
                        scr_dept$,       /* Production Department     */ ~
                        scr_prod$,       /* Product Line              */ ~
                        scr_load$,       /* Production Load           */ ~
                        lk_fn$(),        /* 1,2 Lock with Fin (PAR000)*/ ~
                        partstyle$,      /* Bcksubpt style            */ ~
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
            width$(5%)9,                 /* Width Size - Weld         */ ~
            height$(5%)9,                /* Height Size - Weld        */ ~
            raw_mat$(5%)15,              /* Raw Material              */ ~
            style$(3%)15,                /* Style Information         */ ~
            welder$(3%)2,                /* Welder Numbers            */ ~
            welder_set$(3%)2,            /* Welder Setup Codes        */ ~
            welder_type$(3%)3,           /* Welder Type Frame or Sash */ ~
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
            sa_type$(100%)2,             /* RECORD TYPE 'SA' OR 'LA'  */ ~
            sa_piece$(100%)4,            /* Number of Pieces's to Cut */ ~
            sa_cut$(100%)9,              /* Cut Size for Piece's      */ ~
            sa_part$(100%)15,            /* Raw Material Part Number  */ ~
            sa_rack$(100%)18,            /* Bin Loc and No. of Pieces */ ~
            sa_d1$(100%)16,              /* Raw Material Description  */ ~
            sa_m$(100%)8,                /* WINDOW TYPE,PROFILE TYPE  */ ~
            sa_s$(100%)1,                /* SAW SET-UP NUMBER         */ ~
            sa_cut_type$(100%)1,         /* 2 = Frame, 1 = Sash       */ ~
            sa_die_nbr$(100%)15,         /* Die number                */ ~
            type$1,                      /* Test for Fame or Sash     */ ~
            machine$3, set_up$1, lk$1,   /* Store Window Type code    */ ~
            saw_no$2, saw_no$(100%)2,    /* Machine Saw Number        */ ~
            ff_nam$7,                    /* Bridge File Name          */ ~
            file$30,                     /* Schedule Title Name       */ ~
            inc$5,                       /* Store Batch Number        */ ~
            inc1$6,                      /* Store Deptment code       */ ~
            inc2$5,                      /* Store 1st Sequence No.    */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            screen$30,                   /* Screen Description CR2803 */ ~
            drywall$30,                  /* Dry wall description      */ ~
            partstyle$10,                /* Bcksubpt style            */ ~
            scr_code$1,                  /* Screen in part number     */ ~
            sa_die_desc$20,              /* Die number with foam      */ ~
            grid$20,                     /* Grid information          */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79,                 /* Error Message Text        */ ~
            dtl_subpart$20               /* (SR70993) subpart         */ 
            
        dim label_desc$10, label_info$80, leftseq$3, wd$5, ht$5
        
        dim sa_key$8, sa_rec$64

        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

                                               /* SET-UP SAW FILE(S)   */
            select #3,  "@SAW053@",                                       ~
                                consec , recsize = 222
/* CR2985 */
            select #7,  "@SAW007@",                                       ~
                                consec , recsize = 222
                                
            ff% = 3% : ff_nam$ = "@SAW053@"     /* DEPT 053  Struts */
            if scr_dept$ = "053" then goto L00001
            
            ff% = 7% : ff_nam$ = "@SAW007@"     /* CR2985 Dept 007 Sturts */
L00001:
        REM ***********************************************************
                                            /* (AWDPLX42) Dept = 53   */
                                            /* Dept = 7 CR2985        */
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
            scr_prod$ = "5"                  /* For 500 Searies        */
            if ff% = 7% then scr_prod$ = "2" /* CR2985                 */
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

L01630:     dt_ref$   = str(wrk_rec$,1%,8%) /* Part Reference Number  */

            dt_seq$   = str(wrk_rec$,9%,5%) /* Prod. Daily Seq. No.   */
            
            dtl_load$ = str(wrk_rec$,29,5%) /* Appian Load Number     */

            dtl_part$ = str(wrk_rec$,38%,25%) /* MFG Part Number      */

            dt_samp$  = str(wrk_rec$,77%,1%)  /* 0=No, 1=Samp, 2=Disp */
            
            dtl_subpart$ = str(wrk_rec$,81%,20%) /*(SR70993)*/

            ref_no$   = str(dt_ref$,4%,5%)    /* Shortend Ref No. Load*/

            ssq$      = str(dt_seq$,3%,3%)    /* Shortend Daily Seq.  */

            model$    = str(dtl_part$,1%,3%)  /* Model Code           */

            s$        = str(dtl_part$,11%,1%) /* Set Screen Code      */
                                              /* Skip 4, 5, 6 Sashs   */
            gosub lookup_color                /* Get Color Code       */

            if len(dtl_part$) < 19 then goto create_next
            if count% = 0% then gosub build_schedule   /* CREATE BATCH */
                                                       /* New Area     */
               gosub build_weld               /* Get Weld Info for     */    
                                              /* J% = 1% (Frame)       */
                                              /* J% = 2% (Sash)        */
                                              /* (CR2295) K% extra weld*/

               gosub build_saw_recs
                   j% = 1%                    /* Frame Weld Record     */
                   k% = 1%                    /* (CR2295) */                  
                   type$ = "2"
                   type% = 2%

                   gosub build_detail_weld
                                              /* Frame Saw Records     */
                   seq% = 0%
                   for sa% = 1% to sa_max%
                       gosub build_detail_saw 
                   
                       gosub build_label                  /* CR2803 */                       
                   next sa%

                   j% = 2%                    /* Sash Weld Record      */
                   k% = 2%                    /* (CR2295) */                   
                   type$ = "1"
                   type% = 1%
                   gosub build_detail_weld
/* +(CR2294) */
REM this enables the sash record to be welded on two different types of welders
REM like a urban and sampson
                   if extraSash% = 0% then goto noExtraSash
REM CALL "SHOSTAT" ("HERE AT EXTRA SASH ")  STOP
                     j% = 2%                    /* Sash Weld Record      */
                     k% = 3%
                     type$ = "1"
                     type% = 1%
                     gosub build_detail_weld
noExtraSash:
/* -(CR2294) */
                                              /* Sash Saw Records      */
                   seq% = 0% 
                   for sa% = 1% to sa_max%
                       gosub build_detail_saw
                   
                       gosub build_label                  /* CR2803 */                    
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
          str(bat_rec$,1%,2%)   = sa_type$(sa%)         /* 'SA'        */
          str(bat_rec$,3%,5%)   = ref_no$               /* Load Number */
          str(bat_rec$,8%,5%)   = "     "
          str(bat_rec$,13%,3%)  = seq$                  /* Item Number */
          str(bat_rec$,16%,4%)  = sa_piece$(sa%)        /* Unit Qty    */
          str(bat_rec$,20%,9%)  = sa_cut$(sa%)          /* Piece Cut   */
          str(bat_rec$,29%,9%)  = "        "            /* Reserved    */
REM          if str(sa_part$(sa%),9%,1%) = " "             /* Part No.    */ 
REM            then str(bat_rec$,38%,15%) = sa_part$(sa%)                     
REM            else str(bat_rec$,38%,15%) = str(sa_part$(sa%),3%,10%) 
          str(bat_rec$,38%,15%) = sa_part$(sa%)         /* Part        */ 
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
REM          sa_die_desc$ = sa_die_nbr$(sa%) & str(sa_part$(sa%),11%,1%)  
/* CR3223 check raw material part for F for foam and add to die number */
          sa_die_desc$ = sa_die_nbr$(sa%) 
		  p% = 0%
          p% = POS(sa_part$(sa%) = "F") 	  
          if p% > 0% then  sa_die_desc$ = sa_die_nbr$(sa%) & "F"
          call "SPCSMASH" (sa_die_desc$)                                                        
          str(bat_rec$,183%,20%) = sa_die_desc$
                                                        /* Die Number  */               
          
          write #ff%, bat_rec$, eod goto L02620
        return
L02620:   errormsg$ = saw_err$(type%)
          gosub error_prompt
        return
        
     REM*****************************************************************~   
        * Generate a header label records, then printed text records    *~
        * followed by barcodes for label.                               *~
        *****************************************************************
/* CR2803 */
        build_label
          if sa_cut_type$(sa%) <> type$ then return /* type$ = 2 Frame */
                                                    /* type$ = 1 Sash  */
                                                    
          init(" ") bat_rec$, leftseq$, label_desc$, label_info$                                                   
          str(bat_rec$,1%,2%)   = "LA" 
          str(bat_rec$,3%,10%)   = ref_no$      
          
          if str(seq$,2%,1%) = " " ~
             then leftseq$ = "  " & str(seq$,3%,1%)  ~
             else leftseq$ = " " & str(seq$,2%,1%)

          str(bat_rec$,13%,3%)  = leftseq$
          str(bat_rec$,16%,10%) = "FE        "
          str(bat_rec$,26%,40%) = "Format1"
          write #ff%, bat_rec$, eod goto L02622    /* write header label format */

/* Loop to write text lines for the label.  30 is the max text lines  */
          for rb% = 1% to 30%
             init(" ") bat_rec$, label_desc$, label_info$  
             label_desc$ = "S "
    
             if rb% > 14%  then goto done_slabel   /* Stop at 14 lines for now */
             convert rb% to lcount$, pic(###)
             
             gosub set_desc_info
             
             str(bat_rec$,1%,2%)   = "LA" 
             str(bat_rec$,3%,10%)  = ref_no$  
             str(bat_rec$,13%,3%)  = leftseq$
             
             label_desc$ = "S" & lcount$
             call "SPCSMASH" (label_desc$)
             str(bat_rec$,16%,10%) = label_desc$
             str(bat_rec$,26%,40%) = label_info$
             
             write #ff%, bat_rec$, eod goto L02622
             
          next rb%

done_slabel:       
     REM************************************************************~
        * This is the label barcode lines                          *~
        ************************************************************
            
          for rb% = 1% to 2%
             init(" ") bat_rec$, label_desc$, label_info$               
             label_desc$ = "BA"
              
             convert rb% to lcount$, pic(###)
             
             gosub set_desc_info
             
             str(bat_rec$,1%,2%)   = "LA" 
             str(bat_rec$,3%,10%)  = ref_no$  
             str(bat_rec$,13%,3%)  = leftseq$
             
             label_desc$ = "BA" & lcount$
             call "SPCSMASH" (label_desc$)             
             str(bat_rec$,16%,10%) = label_desc$
             str(bat_rec$,26%,40%) = label_info$
    
             write #ff%, bat_rec$, eod goto L02622
             
          next rb%
        
        return
        
L02622:   errormsg$ = "LA"
          gosub error_prompt
        return
        
     REM**********************************************************~
        *  set label information by lines                        *~
        **********************************************************
        
        set_desc_info

           if str(label_desc$,1%,2%) = "BA" then goto barcode_lines

L02700:           
           if rb% <> 1% then goto next_linea
/* CR2906 */           
             convert width$(j%) to weldwdsz, data goto next_linea
             convert weldwdsz - .25 to weldwidth$, pic(###.####)
        
             label_info$ = weldwidth$             /* width              */
             goto label_line_done             
next_linea:
           if rb% <> 2% then goto next_lineb
/* CR2906 */             
             convert height$(j%) to weldhtsz, data goto next_linea
             convert weldhtsz - .25 to weldheight$, pic(###.####)   
             
             label_info$ = weldheight$            /* height             */             
             goto label_line_done
next_lineb:           
           if rb% <> 3% then goto next_linec
             label_info$ = "Warranty : " & dt_ref$  /* warranty         */
             goto label_line_done   
next_linec:
           if rb% <> 4% then goto next_lined
             label_info$ =  style$(k%)            /* style nbr          */
             goto label_line_done  
next_lined:
           if rb% <> 5% then goto next_linee      
              label_info$ = cl$                    /* color             */
              goto label_line_done  
next_linee:              
           if rb% <> 6% then goto next_linef
              label_info$ = dt_seq$                /* prod line sequence */
              goto label_line_done  
next_linef:
           if rb% <> 7% then goto next_lineg
              label_info$ = scr_dte$               /* production date    */
              goto label_line_done  
next_lineg: 
           if rb% <> 8% then goto next_lineh
              label_info$ = sa_die_desc$           /* unique nbr in die field */
              goto label_line_done
next_lineh:   
           if rb% <> 9% then goto next_linei           
              label_info$ = "# " & lk$             /* lock number        */
              goto label_line_done 
next_linei:
           if rb% <> 10% then goto next_linej
              gosub setLITING
              label_info$ = grid$                  /* grid pattern       */
              goto label_line_done 
next_linej:
           if rb% <> 11% then goto next_linek
              label_info$ = screen$                /* screen description */
              goto label_line_done 
next_linek:
/* lines 12 - 14 are specials listing on the label */
           if rb% <> 12% then goto next_linel      
              if str(dtl_subpart$,16%,1%) = "2" then label_info$ = "WOCD"
              goto label_line_done 
next_linel:
           if rb% <> 13% then goto next_linem 
              gosub check_drywall            
              label_info$ = drywall$               /* drywall            */
              goto label_line_done
next_linem:
           if rb% <> 14% then goto label_line_done           
              label_info$ = " "                    /* unused right now   */
              goto label_line_done              
              
barcode_lines:                                  /* barcodes on the label */
           if rb% <> 1% then goto next_line1
           
             wd$ = str(weldwidth$,1%,3%) & str(weldwidth$,5%,2%)
             ht$ = str(weldheight$,1%,3%) & str(weldheight$,5%,2%)
             
             if str(wd$,1%,1%) = " " then str(wd$,1%,1%) = "0"
             if str(ht$,1%,1%) = " " then str(ht$,1%,1%) = "0"
             
             label_info$ = style$(k%) & " " & wd$ & " " & ht$
             goto label_line_done
next_line1:     
           if rb% <> 2% then goto label_line_done  
             label_info$ = ref_no$  

            
label_line_done:
        return
        
        build_detail_weld                         /* j% = 1%   (Frame) */
                                                  /* j% = 2%   (Sash)  */
                                                  
          if type% = 2% and s$ = "4" then return
          if type% = 2% and s$ = "5" then return
          
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
REM str(bat_rec$,38%,15%) = style$(j%)            /* Style Code  (CR2295) */
          str(bat_rec$,38%,15%) = style$(k%)            /* StyleCode(CR2295)*/

          str(bat_rec$,53%,4%)  = cl$ & "  "            /* color Code  */
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
          str(bat_rec$,147%,2%) = welder$(k%)           /*WelderNo.(CR2295) */
          str(bat_rec$,149%,1%) = welder_set$(k%)       /*WelderSetu(CR2295)*/

/* CR2803 No weld line but data needed */
REM          write #ff%, bat_rec$, eod goto L02640    
        return
REM  L02640: errormsg$ = welder_err$(j%)
REM        gosub error_prompt
REM        return

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
             for i% = 1% to sa_max%
                 str(sa_rack$(i%),5%,9%)  = ssq$& "-A" &ssq$& "/"
             next i%
REM             return                             /* CR3224 bypassed logic */

L02850:   save_part_s$ = dtl_part$                 /* NEWFAMIL3 Info */
          init(" ") sa_type$(), sa_piece$(), sa_cut$(), sa_part$(),      ~
                    sa_rack$(), sa_d1$(), sa_d2$, sa_m$(), sa_s$(), saw_no$(), ~
                    sa_cut_type$()

          call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, #6,  ~
                                                                    err%)
          sa_d2$ = str(apc_prt$,1%,30%)

          sa% = 0%
          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                                              #4, #5, #2, err% )
          gosub build_descript
          eq% = cw% + ch%

          for i% = 1% to eq%
        REM *RHH*                               /* REMOVE 'LA' RECORDS */
            if cc$(i%) = "N" or cc$(i%) = " " then goto L03430

            cut% = 0%                    /* Skip - No Equation Records */
            convert str(ct$(i%),1%,3%) to cut%, data goto L03040
L03040:
            if cut% = 0% then goto L03430

               gosub check_cut                 /* Check NEWFAMIL3 Info */
               if check% = 0% then goto L03430 /* Skip Equation        */

               sa% = sa% + 1%                  /* Save Table Info.     */
               sa_type$(sa%)  = "SA"
               if cc$(i%) = "N" then sa_type$(sa%) = "LA"
               sa_piece$(sa%) = "  " & cp$(i%)     /* No Pieces Needed */
               sa_cut$(sa%)   = ct$(i%)            /* Length of Cut    */
            
               gosub look_up_dienbr

               str(sa_part$(sa%),1%,10%) = cr$(i%) /* Raw Material Part*/
               str(sa_part$(sa%),11%,5%) = "     "
/*(SR70993)*/
               if cut_type% <> 2% then goto no_sash_f  
				if str(dtl_subpart$,5%,1%) = "3" then  /* (AWD002) */    ~
                                              str(sa_part$(sa%),11%,1%) = "F"
                 if str(dtl_subpart$,5%,1%) = "4" then  /* (AWD002) */    ~
                                              str(sa_part$(sa%),11%,1%) = "F"
	             call "SPCSMASH" (sa_part$(sa%)) 
/*(SR70993)*/
no_sash_f:  
               str(sa_rack$(sa%),1%,4%)  = "@F A"  /* Bin Location     */
               str(sa_rack$(sa%),5%,9%)  = ssq$& "-A" &ssq$& "/"
               
               str(sa_rack$(sa%),14%,2%) = cp$(i%) /* No. of Pieces    */
               str(sa_rack$(sa%),16%,3%) = "   "

               str(sa_d1$(sa%),1%,2%)    = "0/"    /* No. of Labels    */
               str(sa_d1$(sa%),3%,12%)   = str(co$,1%,12%)
               str(sa_d1$(sa%),15%,2%)   = "/-"

               sa_m$(sa%)        = machine$ & "     "
               sa_s$(sa%)        = set_up$         /* From Table      */
               saw_no$(sa%)      = saw_no$         /* From Table      */
               sa_cut_type$(sa%) = cut_type$       /* Frame or Sash   */ 
L03430:      next i%
             sa_max% = sa%
        return

        build_descript
            init(" ") co$, x$, s$, readkey$, c_o$
            lk$ = "0"
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

L03940: 
            init(" " ) screen$
            scr_code$ = str(dtl_part$,11%,1%)
            p% = pos("01289ABCJTWX" = scr_code$)      /* CR3224 add screen code */
            if p% = 0%  then goto L03945 
              init(" ") readkey$
              str(readkey$,1%,9%)   = "SCREEN   "   
              str(readkey$,10%,1%)  = str(dtl_part$,11%,1%)
              read #2,key = readkey$, using L03740, desc$, eod goto L03945
                 p% = pos(desc$ = "-") 
                 screen$ = str(desc$,p%+2%,15%)   
                 
                 init(" ") readkey$
                 str(readkey$,1%,9%)   = "SCRNMESH "                  
                 str(readkey$,10%,1%) = str(dtl_subpart$,15%,1%)
                 read #2,key = readkey$, using L03740, desc$, eod goto L03945
                    screen$ = str(screen$,1%,15%) & " " & str(desc$,1%,15%)
            
L03945: return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%)= "        The File ("&ff_nam$&") Already Exists.   "
            msg$(2%)= "             O P T I M I Z A T I O N             "
            msg$(3%)= "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

/* CR2803 */
        look_up_dienbr
           init(" ") sa_key$
           
           str(sa_key$,1%,8%) = eq$(i%) 

           read #4, key 1% = sa_key$,  using L01860, sa_rec$, eod goto L01865
L01860:      FMT CH(64)
              sa_die_nbr$(sa%) = str(sa_rec$,39%,15%)
L01865:              
        return
        
        check_cut                                    /* 1st Frame      */
                                                     /* 2nd Sash       */
            cut_type% = 0%
            check% = 0%
            init(" ") readkey$, machine$, set_up$, saw_no$, desc$, cut_type$
            str(readkey$,1%,9%)  = "NEWFAMIL3"
            if ff% = 7% then str(readkey$,1%,9%)  = "NEWFAMILY"  /* CR2985 */
            str(readkey$,10%,1%) = "F"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
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
L04180:       FMT POS(25), CH(10)

              machine$  = str(desc$,1%,3%)           /* Machine Code   */
                                                     /* Cottage        */
            if i% > cw% and c_o$ = "CO" then str(machine$,3%,1%)  = "3"
                                                     /* oriel          */
            if i% > cw% and c_o$ = "OR" then str(machine$,3%,1%)  = "4"

              set_up$   = str(desc$,5%,1%)           /* Set-Up Code    */
              saw_no$   = str(desc$,7%,2%)           /* Saw Number     */
              check%    = 1%                         /* Valid Equation */
              cut_type% = 2%                         /* Frame Part     */
              cut_type$ = "2"                        /* Frame Part     */
            return
                                                     /* Sash Logic     */
L04240:     init(" ") readkey$, machine$, set_up$, saw_no$, desc$
            str(readkey$,1%,9%)  = "NEWFAMIL3"
            if ff% = 7% then str(readkey$,1%,9%)  = "NEWFAMILY"    /* CR2985 */
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
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
               check%    = 1%                        /* Valid Equation */
               cut_type% = 1%                        /* Sash Part      */
               cut_type$ = "1"                       /* Sash Part      */ 
L04260: return


        check_welder                                 /* 1st Frame      */
                                                     /* 2nd Sash       */
            check% = 0%
            init(" ") readkey$, style$(), welder$(), welder_set$(), desc$
            str(readkey$,1%,9%)  = "NEWFLEXWD"
            if ff% = 7% then str(readkey$,1%,9%)  = "NEWFLEXWD"     /* CR2985 */
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
            str(readkey$,1%,9%)  = "NEWFLEXW3"
            if ff% = 7% then str(readkey$,1%,9%)  = "NEWFLEXWD"     /* CR2985 */            
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
                                                     /* Mode No.       */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            read #2,key = readkey$, using L04280 , desc$, eod goto L04300
              style$(2%)      = str(desc$,1%,15%)    /* Style Code     */
              welder$(2%)     = str(desc$,17%,2%)    /* Welder No.     */
              welder_set$(2%) = str(desc$,20%,1%)    /* Set Up Code    */

              check%    = 1%                         /* Valid Equation */
/* +(CR2295) */
L04300:     extraSash% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)  = "NEWFLEXW3"
            if ff% = 7% then str(readkey$,1%,9%)  = "NEWFLEXWD"     /* CR2985 */            
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
/* - (CR2295) */

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
          
/* (AWD002) force to sash  */

          if str(dtl_part$,11,1) = "4" or               ~
                   str(dtl_part$,11,1) = "5" then w% = 1%
          if str(dtl_part$,11,1) = "4" or               ~
                   str(dtl_part$,11,1) = "5" then h% = 1%
                   
                   
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
          init(" ") readkey$, cldesc$                            /* CR2704 */
          str(readkey$,1%,15%) = "COLOR    " & str(dtl_part$,4%,1%)
          read #2,key = readkey$, using L05050, cl$, cldesc$, eod goto L05060
L05050:      FMT POS(25), CH(2), POS(30), CH(30)                 /* CR2704 */
L05060: return

        REM *************************************************************~
            *   Get Standard, 1 LITING or Oriel LITING information      *~ 
            *************************************************************
        setLITING
           init(" ") muttin$, vhmuttin$, grid$ 
           vert% = 0% : horz% = 0% : er1% = 0% : litingerr% = 0% : x% = 7%
           lits$ = "0"
/* coded to match glass label */
           if str(dtl_part$,7%,2%) = "A0" then litingerr% = 1%

           if str(dtl_part$,7%,2%) = "57" then goto set_mut
           if str(dtl_part$,7%,1%) = "V" and (str(partstyle$,1%,2%) <> "SH" and ~
              str(partstyle$,1%,2%) <> "DH")                                    ~
                then goto set_mut
           
           if str(dtl_part$,11%,1%) = "5" or ~
              str(dtl_part$,11%,1%) = "7" then L61030
           view$ = "T"     /* TOP View */               

           call "APCGSLIT" (dtl_part$,        /* MFG Part Number     */~
                            muttin$,          /* Grid Vert/Horiz Code*/~
                            lits$,            /* No. of Lits         */~
                            view$,            /* T or B              */~
                            vert%,            /* Number of Verticals */~
                            horz%,            /* Number of Horizontal*/~
                            #2,               /* (GENCODES)          */~
                            er1% )            /* Error Code          */

           if er1% <> 0% then goto L61030
           
           mtt% = pos(muttin$ = "x")
           
           convert vert% to vert$, pic(0)     
           convert horz% to horz$, pic(0)  
           vhmuttin$ = horz$ & "Hx" & vert$ & "V"           
           if mtt% = 0% then grid$ = muttin$  ~
                        else grid$ = vhmuttin$ 

L61030:           
           init(" ") muttin$, vhmuttin$
           if str(ldtl_part$,11%,1%) = "4" or ~
              str(ldtl_part$,11%,1%) = "6" then L61035
              
           view$ = "B"   /* BOTTOM View */                

           call "APCGSLIT" (dtl_part$,         /* MFG Part Number     */~
                            muttin$,          /* Grid Vert/Horiz Code*/~
                            lits$,            /* No. of Lits         */~
                            view$,            /* T or B              */~
                            vert%,            /* Number of Verticals */~
                            horz%,            /* Number of Horizontal*/~
                            #2,               /* (GENCODES)          */~
                            er2% )            /* Error Code          */

           if er1% <> 0% and er2% <> 0% then litingerr% = 1%
           if er2% <> 0% then goto L61035     

           mtt% = pos(muttin$ = "x")
           if er1% = 0% then x% = 7% else x% = 1%
           if str(grid$,7%,1%) <> " " then x% = 9% 
           if str(grid$,8%,1%) <> " " then x% = 10% 
           str(grid$,x%,10%) = "-         "   /* clear space after - */
           
           convert vert% to vert$, pic(0)     
           convert horz% to horz$, pic(0)
           vhmuttin$ = horz$ & "Hx" & vert$ & "V"                 
           if mtt% = 0% then str(grid$,x%+2%,12%)  = muttin$  ~
                        else str(grid$,x%+2%,12%)  = vhmuttin$ 
                        
L61035:             
           return
set_mut:
           sonly% = 0%
           sonly% = pos("4567" = str(dtl_part$,11%,1%) )
           
           if str(dtl_part$,7%,2%) = "57" then grid$ = "PERIM - PRAIRIE"
           
           if sonly% <> 0% then goto L61040
           if str(dtl_part$,7%,2%) = "V0" then grid$ = "CUST_VAL - CUST_VAL"
           if str(dtl_part$,7%,2%) = "V1" then grid$ = "VALA1 - VALA1"
           if str(dtl_part$,7%,2%) = "V2" then grid$ = "VALA2 - VALA2"
           if str(dtl_part$,7%,2%) = "V3" then grid$ = "VALA3 - VALA3"
           if str(dtl_part$,7%,2%) = "V4" then grid$ = "VALA4 - VALA4"
           if str(dtl_part$,7%,2%) = "V5" then grid$ = "VALA5 - VALA4"
        return 
L61040:
           if str(dtl_part$,7%,2%) = "V0" then grid$ = "CUST_VAL"
           if str(dtl_part$,7%,2%) = "V1" then grid$ = "VALA1"
           if str(dtl_part$,7%,2%) = "V2" then grid$ = "VALA2"
           if str(dtl_part$,7%,2%) = "V3" then grid$ = "VALA3"
           if str(dtl_part$,7%,2%) = "V4" then grid$ = "VALA4"
           if str(dtl_part$,7%,2%) = "V5" then grid$ = "VALA5"
        return 

        check_drywall                               /* Put DryWall Models as Special */
            drywall% = 0%
               init(" ") readkey$, drywall$
               str(readkey$,1%,9%)   = "PLAN DRYW"
               str(readkey$,10%,15%) = str(dtl_part$,1%,3%)     /* Model  */

               read #2,key = readkey$, using L03740, drywall$, eod goto not_dry_wall
                     
                    return

        not_dry_wall
            drywall% = 1%
        return
		
        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
  
