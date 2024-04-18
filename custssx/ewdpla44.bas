        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLA44                             *~
            *  Creation Date     - 06/28/01                             *~
            *  Last Modified Date- 08/24/2016                           *~
            *  Written By        - Christie Gregory                     *~
            *  Last Modified By  - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Subroutine to update staging, loading*~
            *                      remove from load, update inv, and    *~
            *                      backorders                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            * Subroutine - Called by AWDPLA44 & EWDPLN79                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/28/01 ! Original                                 ! CMG *~
            * 10/02/03 ! (EWD001) Mod for Update New Status       ! CMG *~
            * 02/24/04 ! (EWD002) Mod for New Surge Inventory     ! CMG *~
            * 03/03/06 ! (PAR001) Mod for New Sub Part Number in  ! RHH *~
            *          !    the label file. No Change             !     *~
            * 11/07/06 ! (AWD003) mod to only change orders to 19 ! CMG *~
            *          !   after they have been planned           !     *~
            *08/24/2016! (CR456) DC Center status 15 RGA          ! CMG *~
            *          !   related to EWD001                      !     *~
            *12/19/2018! CR1726 write ATLaS trigger record for    ! RDB *~
            *          !        delete                                  *~
            *04/18/2023! CR3300 New RGA bldg 300 option with dept ! RDB *~			
            *************************************************************

             sub  "EWDPLA44" (call%,  /* 1 = APCPA44 2=EWDPLN79   */ ~
                             pass%,  /* 1 = Updte St 2= Updte Hdr*/ ~
                             scr_code%, /* Screen Code Selection */ ~
                             scr_load$, /* Load Number           */ ~
                             dt_bar$,   /* Bar Code Number       */ ~
                             userid$,   /* Userid Entering       */ ~
                             authid$,   /* Authorizing Id        */ ~
                             bk_area$,  /* Area Code             */ ~
                             bk_found$, /* Found Code            */ ~
                             bk_reas$,  /* Reason Code           */ ~
                             bk_text$(),/* Text Area             */ ~
                             #2,        /* APCPLNDT              */ ~
                             #3,        /* APCPLNOR              */ ~
                             #4,        /* APCPLNSC              */ ~
                             #5,        /* GENCODES              */ ~
                             #6,        /* APCPLNAD              */ ~
                             #10,       /* EWDBOLRM              */ ~
                             #12,       /* EWDPRDLB      (PAR001)*/ ~
                             #13,       /* EWDBOLBK              */ ~
                             #23,       /* PGORLNTR              */ ~
                             err%       /* Error Code            */ )

        dim                              /* FILE = EWDBOLRM            */~
            rm_key$16,                   /* Primary Key                */~
            rm_dte$6,                    /* Remove Date                */~
            rm_userid$3,                 /* Removed By                 */~
            userid$3,                    /* User Id                    */~
            authid$3,                    /* Authorized User Id         */~
            bk_area$1,                   /* Area Code                  */~
            bk_found$1,                  /* Found Code                 */~
            bk_reas$3,                   /* Reason Code                */~
            bk_text$(3%)20,              /* Text Area                  */~
            bk_seq$5,                    /* Planning Seq Number        */~
            bk_date$6,                   /* Production Date (AWD003)   */~
            bk_fil$24,                   /* Filler Area                */~
            bk_time$8,                   /* Time Stamp                 */~
            timeord$6,                   /* Current time               */~
            filler1$180,                 /* Trigger filler area        */~
            rm_type$2,                   /* Type Remove or Backorder   */~
            rm_fil$3                     /* Filler Area                */

        dim                              /* FILE = APCPLNSC            */~
            sc_rec$128,                  /* Record Format              */~
            sc_key$10,                   /* Primary Key                */~
            sc_key1$27, sc_st$2,         /* Alt Key 1 AND STATUS       */~
            sc_part$25                     /* MFG Part Number  (EWD002)  */

        dim                              /* FILE = APCPLNAD            */~
            ad_rec$64,                   /* Record Format              */~
            ad_key$31,                   /* Primary Key                */~
            ad_time$8                    /* Time Stamp                 */
		
        dim                              /* FILE = APCPLNDT            */~
            dt_rec$256,                  /* Record Format              */~
            savDtRec$256,                /* Save DT Record Format (CR456)*/~
            dt_key$23,                   /* Primary Key                */~
            dt_bar$18,                   /* Barcode                    */~
            dt_st$2,                     /* Scann Status               */~
            dt_dept$3, sav_dept$3,       /* Department                 */~
            dt_cust$9                    /* Customer number            */

        dim                              /* Screen Information         */~
            scr_load$5,                  /* Screen Load Number         */~
            save_code$8,                 /* Sales Order                */~
            readkey$24,                  /* GEN CODES KEY              */~
            p_key1$23,                   /* (EWDPRDLB) Labels          */~
            save_code1$10                /* Sales Order / Line Item    */

        dim                              /* FILE = APCPLNOR            */~
            or_rec$170                   /* Record Format              */

        dim rgaSt$2,                     /* (CR456) RGA Status         */~
            dtRgaSt$2,                   /* (CR456) DT Rga status check*/~
			rga300Dpt$3,                 /* CR3300 new dept            */~
            rgaDpt$3                     /* (CR456) RGA Dept           */
/* CR1727 */            
        dim schema$8                     /* Schema                     */
			
        schema_err%, schema% = 0%
        init(" ") schema$
        call "SCHEMA" (schema$, schema%, #5, schema_err%)                                
/* CR1727 */
        
REM SCR_CODE% = 5% still used in BACKORDER EWDPLN79      
        timeord$ = time                             /* CR1726 */                                               
        if call% = 1% and scr_code% = 5% then scr_code% = 6% 
        if pass% = 1% then gosub update_status   /*Pass-(1)(APCPLNDT)*/



        if pass% = 2% then gosub update_load_header  /*Pass-(2)(APCPLNSC)*/

        goto exit_sub

REM************************************************************************

        check_detail
           check% = 0%
           init(" ") dt_key$, sc_st$
           str(dt_key$,1%,8%) = save_code$                /* S.O. No. */
        check_detail_nxt
           read #2,key > dt_key$, using L35040, dt_rec$,                 ~
                                              eod goto check_detail_done
L35040:     FMT CH(256)                  /* (APCPLNDT) - FILE          */
           dt_key$ = str(dt_rec$,24%,23%)
           if save_code$ <> str(dt_key$,1%,8%) then                     ~
                                                 goto check_detail_done
                                                 
           sc_st$ = str(dt_rec$,64%,2%)
           if sc_st$ >= "16" then check% = 1%    /*check for cart status 26 */
             if sc_st$ < dt_st$ then check% = 1% /*  and rga status 32      */
           goto check_detail_nxt
        check_detail_done
        return

        update_apcplnor
          if str(scr_load$,1%,1%) = "S" then return /* SKIP STOCK LOAD*/
          read #3,hold,key 4% = save_code$, using L35060, or_rec$,       ~
                                          eod goto update_apcplnor_done
L35060:     FMT CH(170)                  /* (APCPLNOR) - FILE          */
          if str(or_rec$,60%,2%) >= dt_st$ then                         ~
                                               goto update_apcplnor_done

            str(or_rec$,60%,2%) = dt_st$
            str(or_rec$,62%,8%) = date
            put #3, using L35060, or_rec$
            rewrite #3
        update_apcplnor_done
        return

        update_load_header                    /* Pass - (2) (APCPLNSC) */
           init(" ") save_code$, save_code1$, sc_key1$, sc_rec$
           str(sc_key1$,1%,5%) = scr_load$
        next_header
           read #4,key 1% > sc_key1$, using L35080, sc_rec$,             ~
                                                     eod goto header_done
L35080:     FMT CH(128)                  /* (APCPLNSC) - FILE          */
          sc_key1$ = str(sc_rec$,7%,27%)
          if scr_load$ <> str(sc_key1$,1%,5%) then goto header_done
/*(AWD003)*/
          if str(sc_rec$,110%,2%) < "03" then goto next_header
            save_code$  = str(sc_key1$,18%,8%)
            gosub check_detail
            if check%  = 0% then gosub update_apcplnor  /* Sales Ord*/
            if check%  = 0% then gosub update_apcplnsc  /* Line It's*/
            str(sc_key1$,26%,2%) = "99"
        goto next_header
        header_done
        return


        update_status                    /* Pass - (1) - APCPLNDT      */
          call "SHOSTAT" (" Processing " )
          init(" ") dt_key$, dt_rec$, sav_dept$, dt_st$, dt_dept$,      ~
/*(CR456)*/         rgaSt$, rgaDpt$, savDtRec$, dtRgaSt$

          init (" ") rga300Dpt$
		  
          dt_flag% = 0%
          rgaSt$   = "11"    /* (CR456) for RGA Dept Record */
          rgaDpt$  = "032"   /* (CR456) */
		  rga300Dpt$ = "026" /* CR3300 new dept */
		  if scr_code% = 7% then rga300Dpt$ = "026"    /* CR3200 */
/*-----------------------------------------------------*/
/* scr_code -> 1; Staged                               */
/* scr_code -> 2; loaded                               */
/* scr_code -> 3; Remove from Load                     */
/* scr_code -> 5; Update RGA - shows as 5 on primary   */
/*-----------------------------------------------------*/
          if scr_code% = 1% then dt_st$ = "14"                /*Staged   */
          if scr_code% = 2% then dt_st$ = "16"                /*Loaded   */
          if scr_code% = 3% then dt_st$ = "19"                /*Removed  */
/* (CR456) update all existing barcodes back to 14 */
          if scr_code% = 5% then dt_st$ = "30"                /*BackOrder*/
          if scr_code% = 6% then dt_st$ = "32"                /*RGA      */
/* CR3300 */
          if scr_code% = 7% then dt_st$ = "32"                /* 300 RGA */		  
REM IF SCR_CODE% = 6% THEN DT_ST$ = "12"                      /*RGA      */
          str(dt_key$,1%,18%) = dt_bar$
        update_status_nxt                              /* #2 APCPLNDT */
		  init(" ") bckad_rec$ 
          read #2,hold,key > dt_key$,using L35040, dt_rec$,              ~
                                           eod goto update_status_done
           dt_key$ = str(dt_rec$,24%,23%)
           if dt_bar$ <> str(dt_key$,1%,18%) then goto update_status_done
              sav_dept$, dt_dept$ = str(dt_rec$,42%,3%)
REM ------
REM  if status already = rgast then skip - dtRgaSt$ is DT current status
REM ------
REM DTRGAST$ = STR(DT_REC$,64%,2%)     /* DONT UPDATE RGA   */
REM IF SCR_CODE% = 6% AND DTRGAST$ = RGAST$ /* RECORD TWICE */  ~
REM THEN GOTO UPDATE_STATUS_NXT

              gosub check_support
              gosub update_audit
              read #2,hold,key = dt_key$,using L35040, dt_rec$,           ~
                                              eod goto update_status_done

                 delete #2

/* (CR456) delete APCPLNDT if exists for dept 032 but do not write back */
              if scr_code% = 6% and dt_dept$ = rgaDpt$ then update_status_nxt
/* CR3300 */			  
              if scr_code% = 7% and dt_dept$ = rga300Dpt$ then update_status_nxt

              if scr_code% = 3% or scr_code% = 5% then gosub update_remove
              if scr_code% = 5% and call% = 2% then gosub update_backorder
              if scr_code% = 3% or scr_code% = 5% then goto L60400

REM IF SCR_CODE% = 3% THEN GOSUB UPDATE_REMOVE
REM IF SCR_CODE% = 3% THEN GOTO L60400
              init(" ") ad_time$
              call "TIME" (ad_time$)                          /* (CR456) */
              str(dt_rec$,53%,6%) = date               /* Staged, Loaded */
              str(dt_rec$,64%,2%) = dt_st$
              str(dt_rec$,116%,8%) = ad_time$
			  
              write #2,using L35040, dt_rec$, eod goto update_status_nxt
L60400:
/*(CR456)save primary record*/
              if supp% = 0% then savDtRec$ = dt_rec$
              if dt_flag% <> 0% then goto update_status_nxt
              if scr_code% = 3% or scr_code% = 5% then gosub correct_apcplnsc
              if scr_code% = 3% or scr_code% = 5% then gosub correct_surge 

REM IF SCR_CODE% = 3% THEN GOSUB CORRECT_APCPLNSC
REM IF SCR_CODE% = 3% THEN GOSUB CORRECT_SURGE

              goto update_status_nxt
        update_status_done
          if scr_code% = 6% then gosub addRga                 /* (CR456) */
		  if scr_code% = 7% then gosub add300Rga              /* CR3300  */
        return
                                                              /* (CR456) */
        addRga                 /* No Primary Found or No Record to Write */
          if savDtRec$ <= " " then return
          init(" ") dt_rec$
          dt_rec$ = savDtRec$
          str(dt_rec$,42%,3%) = rgaDpt$    /* Dept code store 2 locations*/
          str(dt_rec$,59%,3%) = rgaDpt$    /*  in apcplndt               */
          str(dt_rec$,64%,2%) = rgaSt$


          write #2,using L35040, dt_rec$, eod goto addRgaDne

          dt_st$ = rgaSt$
		  supp% = 0%    /* CR3300 was missing write if last dept was support */		  
          gosub update_audit
          init(" ") dt_rec$
        addRgaDne
        return

        add300Rga                             /*  CR3300  */
          if savDtRec$ <= " " then return
          init(" ") dt_rec$
          dt_rec$ = savDtRec$
          str(dt_rec$,42%,3%) = rga300Dpt$    /* Dept code store 2 locations*/
          str(dt_rec$,59%,3%) = rga300Dpt$    /*  in apcplndt               */
          str(dt_rec$,64%,2%) = rgaSt$


          write #2,using L35040, dt_rec$, eod goto add300RgaDne

          dt_st$ = rgaSt$
		  supp% = 0%
          gosub update_audit
          init(" ") dt_rec$
        add300RgaDne
        return
		

/* (\CR456) */

        correct_apcplnsc                  /* For Remove - Only Do Once */
           sc_tqty% = 0% : sc_mqty% = 0% : sc_pqty% = 0% : sc_pqty1% = 0%
           inv_qty% = 0%                           /*  (EWD002)  */
           init(" ") sc_key$, sc_part$
           sc_key$ = str(dt_bar$,1%,10%)             /* S.O. Line Item */
           if str(scr_load$,1%,1%) = "S" then return  /*SKIP STOCK LOAD*/

                                                      /*  (EWD002)      */
           read #4,hold,key = sc_key$, using L60465, sc_part$, sc_tqty%, ~
                                       sc_mqty%, sc_pqty%, sc_pqty1%,    ~
                                       sc_price, sc_units, eod goto L60550
L60465:       FMT POS(34), CH(25), POS(68), 4*BI(2), PD(14,4), XX(8), PD(14,4)

           x = round(sc_units/sc_tqty%, 2)    /* Line Item Load Unit   */
           y = round(sc_price/sc_tqty%, 2)    /* Line Item Unit Price  */
           sc_tqty% = sc_tqty% - 1%
                                              /*      (EWD002)         */
           if sav_dept$ = "102" then inv_qty% = inv_qty% + 1%
           if sc_tqty% > 0% then goto L60505
              delete #4                       /* Remove Record         */
           dt_flag% = 1%
        return
L60505:    sc_units = round(x * sc_tqty%, 2)
           sc_price = round(y * sc_tqty%, 2)
           if sav_dept$ = "102" then sc_pqty%  = sc_pqty% - 1%
           if sav_dept$ = "104" then sc_pqty1% = sc_pqty1% - 1%
           if sav_dept$ <> "102" and sav_dept$ <> "104" then             ~
                                     sc_mqty% = sc_mqty% - 1%
           put #4, using L60465,sc_part$, sc_tqty%, sc_mqty%, sc_pqty%,  ~
                                sc_pqty1%, sc_price, sc_units
           rewrite #4
           dt_flag% = 1%
        return
L60550: call "SHOSTAT" ("(Error) - No Record in APCPLNSC to update ("&~
                         str(dt_bar$,1%,10%)&")" )
        stop
        err% = 1%
        return

        correct_surge                               /*  (EWD002)  - Beg  */
          if inv_qty% <= 0% then return
          call "AWDSURGE" (sc_part$, inv_qty%)
        return                                      /*  (EWD002)  - End  */


        update_remove
            dt_dept$ = sav_dept$
            if schema% = 2% and scr_code% = 3%   ~
                then gosub load_pg_order_trigger                   /* CR1727 */
            gosub remove_label
            gosub check_support
            if supp% = 1% then return
            init(" ") rm_key$, rm_dte$, rm_userid$, rm_fil$, rm_type$
            str(rm_key$,1%,5%)  = str(dt_rec$,1%,5%)  /* Load Number   */
            str(rm_key$,6%,8%)  = str(dt_rec$,24%,8%) /* S.O. Number   */
            str(rm_key$,14%,1%) = " "                 /* Leading Space */
            str(rm_key$,15%,2%) = str(dt_rec$,32%,2%) /* Line Item No. */
            rm_fil$    = "   "
            rm_qty%    = 1%                           /* Default Qty   */
            rm_dte$    = date                         /* Remove Date   */
            rm_userid$ = userid$                      /* Removed By    */
            rm_type$ = "RM"
            if scr_code% = 5% then rm_type$ = "BK"
            read #10,hold,key = rm_key$, using L60970, rm_qty%,          ~
                                                          eod goto L60980
L60970:        FMT POS(17), BI(2)
                 delete #10

            rm_qty% = rm_qty% + 1%
L60980:     put #10, using L60990, rm_key$, rm_qty%, rm_dte$, rm_userid$,~
                                                     rm_type$, rm_fil$
L60990:        FMT CH(16), BI(2), CH(6), CH(3), CH(2), CH(3)
            write #10, eod goto L60995
        return
L60995:     call "SHOSTAT" ("Error Updating (EWDBOLRM)-"&rm_key$) : stop
        err% = 2%
        return


        update_backorder
            if supp% = 1% then return
            init(" ") bk_key$, bk_time$, bk_date$, bk_fil$
            bk_date$ = str(dt_rec$,47%,6%)                  /* (AWD003)  */
            call "TIME" (bk_time$)
            bk_key$ = rm_key$
            read #13,hold,key = bk_key$, using L61530, bk_seq$, eod goto L61500
L61530:          FMT POS(94), CH(5)

                 delete #13
                 goto L61520

L61500:     bk_seq$ = str(dt_rec$,111%,5%)

L61520:     put #13, using L61510, bk_area$, bk_found$, bk_area$, bk_reas$,~
                                   bk_key$, authid$, bk_time$, bk_text$(), ~
                                   bk_seq$, bk_date$, bk_fil$
                                     /*  (AWD003) - Mod to add prod date */
L61510:         FMT CH(1), CH(1), CH(1), CH(3), CH(16), CH(3), CH(8),   ~
                    3*CH(20), CH(05), CH(06), CH(24)

            write #13
        return


        check_support
           supp% = 0%
           if dt_dept$ = "102" or dt_dept$ = "104" then                  ~
                                                  goto check_support_done

           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = dt_dept$
           read #5,key = readkey$, eod goto check_support_done
           supp% = 1%
        check_support_done
        return



        update_audit
           if supp% = 1% and scr_code% = 6% then return
           if supp% = 1% and scr_code% = 7% then return    /* CR3300     */		   
           init(" ") ad_rec$, ad_time$, ad_key$
           str(ad_rec$,1%,18%) = str(dt_rec$,24%,18%)      /* Barcode    */
           str(ad_rec$,19%,6%) = date                      /* Scan Date  */
           str(ad_rec$,25%,5%) = str(dt_rec$,42%,5%)       /* Dept, Proc */
           str(ad_rec$,30%,2%) = str(dt_rec$,104%,2%)      /* Shift      */
           str(ad_rec$,32%,2%) = dt_st$                    /* Status     */
           str(ad_rec$,34%,18%) = str(dt_rec$,24%,18%)
           call "TIME" (ad_time$)
           str(ad_rec$,52%,8%) = ad_time$
           str(ad_rec$,60%,3%) = userid$
           str(ad_rec$,63%,2%) = " "
           ad_key$ = str(ad_rec$,19%,33%)
           read #6,hold,key = ad_key$, using L35120, bckad_rec$, ~
         		   eod goto L62890
		   
              delete #6

 L62890:
/* (CR456) delete audit if exists for dept 032 but do not write back */
           if scr_code% = 6% and dt_dept$ = rgaDpt$ then return
/* CR3200 */
           if scr_code% = 7% and dt_dept$ = rga300Dpt$ then return
		   
           write #6, using L35120, ad_rec$, eod goto L62895
L35120:     FMT CH(64)                   /* (APCPLNAD) - FILE          */
L62895: return


        update_apcplnsc                              /* Pass - (2A)      */
           if str(scr_load$,1%,1%) = "S" then return   /* SKIP STOCK LOAD*/
           init(" ") sc_key$                         /* (APCPLNSC) Lines */
           sc_key$ = save_code$                     /* All S.O. Lines   */
        update_apcplnsc_nxt
           read #4,hold,key > sc_key$, using L35080, sc_rec$,             ~
                                            eod goto update_apcplnsc_done
           sc_key$ = str(sc_rec$,24%,10%)
           if save_code$ <> str(sc_key$,1%,8%) then                      ~
                                                goto update_apcplnsc_done
           if str(sc_rec$,110%,2%) >= dt_st$ then goto update_apcplnsc_nxt
              str(sc_rec$,110%,2%) = dt_st$
              str(sc_rec$,112%,6%) = date
              put #4, using L35080, sc_rec$
              rewrite #4
              goto update_apcplnsc_nxt
        update_apcplnsc_done
        return


        remove_label                                        /* (PAR001)  */
            init(" ") p_key1$
            str(p_key1$,1%,23%) = str(dt_rec$,24%,23%)
            read #12,hold,key 1% = p_key1$, eod goto L63000
               delete #12

L63000: return

/************************************************************************/
/* Load the Order Line trigger for ATLas extract later.       CR1726    */
/************************************************************************/
        load_pg_order_trigger
            init(" ") filetype$, transmit$, filetype2$, salesorder$, linenbr$, ~
                      pgdate$, time$, upddte$, updtime$, filler1$    
       
            str(tr_key$,1%,8%) = str(dt_bar$,1%,8%)
            str(tr_key$,9%,2%) = str(dt_bar$,9%,2%)

            read #23, key 2% >= tr_key$,   ~
                   using L62705, filetype$, transmit$, pgdate$, time$,       ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,     eod goto new_or_trigger

              goto trigger_or_process
               
            nxt_or_trigger   
               read #23, using L62705, filetype$, transmit$, pgdate$, time$, ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,    eod goto new_or_trigger
trigger_or_process:                     
                  if salesorder$ <> str(dt_bar$,1%,8%) or      ~
                     linenbr$ <> str(dt_bar$,9%,2%)  then goto new_or_trigger
                     
                  if str(filler1$,1%,2%) = "98" and transmit$ = "0" and ~
                     salesorder$ = str(dt_bar$,1%,8%) and               ~
                     linenbr$ = str(dt_bar$,9%,2%)  then goto trigger_or_done
               goto nxt_or_trigger      
                               
            new_or_trigger
               filetype$ = "ORDERLINE"
               transmit$ = "0"
               str(pgdate$,1%,6%) = date
               time$ = timeord$                     /* set one time at start */
               filetype2$ = "ORDERLINE"
               salesorder$ = str(dt_bar$,1%,8%)
               linenbr$  = str(dt_bar$,9%,2%)
               upddte$ = " "
               updtime$ = " "
               str(filler1$,1%,2%) = "98"
               str(filler1$,3%,10%) = "EWDPLA44"
               dt_cust$ = str(dt_rec$,124%,9%) 
               str(filler1$,13%,9%) = dt_cust$ 
               put #23, using L62705, filetype$, transmit$, pgdate$, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,      ~
                            updtime$, filler1$
            
L62705:           FMT CH(20), CH(1), CH(6), CH(6), CH(20), CH(8), ~
                           CH(3), CH(6), CH(6), CH(180)
                           
               write #23
               
        trigger_or_done
        return 
		
        exit_sub
          end


