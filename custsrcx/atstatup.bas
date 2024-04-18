        REM *************************************************************~
            *                                                           *~
            *  Program Name      - ATSTATUP                             *~
            *  Creation Date     - 04/01/2109                           *~
            *  Written By        - Ricky Beane                          *~
            *                                                           *~
            *  Description       - Process ATLaS input of status by     *~
            *                      barcode,                             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/14/19 ! New Program for PlyGem ATLaS input data  ! RDB *~
            * 04/15/19 ! Fix new audit write                      ! RDB *~
            * 06/25/19 ! CR2097 Add AWDCOMSB call                 ! RDB *~
            * 07/09/19 ! CR2097 Add status 14 dept 106 & 16 as 108! RDB *~
            * 10/18/19 ! CR2282 New status rules for complt/trnsfr! RDB *~
            * 11/05/19 ! CR2108 Add warranty change on cross-dock ! RDB *~ 
            * 04/23/20 ! CR2532 Add new garden window model       ! RDB *~  
            * 02/22/22 ! CR3033 Change APCPLNAD Time Input Source ! RDB *~   
            * 02/14/23 ! CR3250 Request to always save Truck_Close! RDB *~
            *                   transactions in the audit         !     *~			
            *************************************************************
  
        dim filename$8,                  /* Use with EWDOPEN - EWD016  */~
            readkey$24, desc$30,         /* GENERIC KEY                */~
            table$9, code$15,            /* TABLE LOOKUP NAME AND CODE */~
            modulename$20,               /* what called update audit   */~
            dt_rec$256,                  /* Production Detail          */~
            dt_part$25,                  /* (APCPLNDT) Part Number     */~
            dt_load$5,                   /* Scanning Load Number - Only*/~
            dt_dept$3,                   /* Department in APCPLNDT     */~
            dt_date$6,                   /* Production Date            */~
            dt_key$23,                   /* Barcode key to APCPLNDT    */~
            upd_st$2,                    /* New status for updates     */~
            hold_upd_st$2,               /* Hold new status for updates*/~
            barcode$18,                  /* Scanned Barcode            */~
            barcode_shp$20,              /* Shipping Barcode   (AWD028)*/~
            awd_app_key0$20,             /* Label Primary Key  (AWD028)*/~
            rec$(4%)256,                 /* Appian Label Record(AWD054)*/~
            app_scan_tme$4,              /* Appian Scan Time   (awd028)*/~
            dateout$8,                   /* Time Extract For Screen    */~
            prevcode$18,                 /* Previous Bar Code Entered  */~
            howship$2,                   /* How ship - check tech pick */~
            calc_time$8,                 /* Use for Re-make Calc       */~
            dt_st$2,                     /* Bar Code Status            */~
            dt_proc$2,                   /* DT Process code            */~
            dt_ref$8,                    /* DT referenc warranty CR2108 */~
            sav_bar$18,                  /* sav_bar                    */~
            strtshft1$5,                 /* Start 1st shift time       */~
            strtshft2$5,                 /* Start 2nd shift time       */~
            strtshft3$5,                 /* Start 3rd shift time       */~
            endshft1$5,                  /* End 1st shift time         */~
            endshft2$5,                  /* End 2nd shift time         */~
            endshft3$5,                  /* End 3rd shift time         */~
            date$8,                      /* Date for screen display    */~
            errcnt$10,                   /* Count of errors            */~
            log_id$3,                    /* ID associated with change  */~
            scr_dept$3,                  /* Department for subroutine  */~
            scr_id$3,                    /* User ID for subroutine     */~
            schema$8                     /* Schema code                */
            
           dim ld_app_rec$128,              /* (awdappld) New Load File */~
               ld_app_key0$5                /* Load No. Primary key     */
            
          dim cross_warranty$8,            /* " "         for Cross      */~
              cross_warranty2$8,           /* Cross Dock Warranty(AWD051)*/~
              cross_rec$128,               /* Warranty Record    (PAR004)*/~
              cross_rec1$128,              /* Cross Dock Record  (AWD051)*/~
              warranty_key$18,             /* Alt 2 Warr Key-S.O. Line It*/~
              cross_dock_key1$23,          /* Save Detail Key            */~
              cross_part$25,               /* Save Part Number fro Cross */~
              cross_sub_part$20,           /* Save Sub Part Number       */~
              cross_sub_info$20            /* Save Sub Info      (PAR004)*/
   
          dim barcode_trailer$8,           /* Barcode for Trailer Scan   */~
              wandchar_trailer$1,          /* Wand Character Trailer     */~
              tr_key0$20,                  /* Trailer File Primary Key   */~
              tr_rec$128,                  /* Trailer Record             */~
              sav_drop$2, scan_drop$       /* Save Curr and Scan drop No */

           dim                                                              ~
               flag$1,                      /* Calling Program Flag       */~
               pgm$1,                       /* Calling Program BCKUPDTE?? */~
               so_inv$8,                    /* Sales Order or Invoice     */~
               item_no$3,                   /* Item Number                */~
               bcksubpt_rec$256,            /* BCKSUBPT Record            */~
               flds$(35%)4,                 /* Part Number Fields         */~
               info_flds$(35%)4             /* Additional Info Fields     */
          
           dim at_key$57,                   /* Atlas Status input key      */~
               at_reca$184,                 /* Atlas Status record         */~
               at_recb$176,                 /* Atlas Status Caelus data    */~
               at_status$30,                /* Atlas Status Code           */~
               audit_key$108                /* Audit Atlas key             */
            
           dim at_transuid$50,             /* Atals Unique Transaction ID  */ ~
               at_from_dept$03,            /* Atlas scan from dept code    */ ~
               at_from_type$30,            /* Atlas from Transaction Types */ ~
               at_to_dept$03,              /* Atlas scan to dept code      */ ~
               at_to_type$30,              /* Atlas To Transaction Types   */ ~
               scan_date$10,               /* Atlas scan date              */ ~
               scan_time$10,               /* Atlas scan time              */ ~
               load_nbr$10,                /* Atlas load number            */ ~
               full_name$50,               /* Atlas user full name         */ ~
               usr_name$30                 /* Atlas user ID                */ 
           
           dim au_TransUID$50,             /* Atals Unique Transaction ID   */~
               au_ProdDate$10,             /* System Date                   */~
               au_Barcode$18,              /* Caelus Barcode                */~
               au_Department$03,           /* Atlas department codes        */~
               au_Atl_Dept$03,             /* Atlas from department         */~
               au_cntr$05,                 /* Unique key counter            */~
               au_BeforeStatus$02,         /* Caelus status before change   */~
               au_AfterStatus$02,          /* Caelus status after change    */~
               au_AuditDate$10, 	       /* Date of change                */~
               au_AuditTime$10,            /* Time of change                */~
               au_Shift$02,	         	   /* Shift of Atlas scan time      */~
               au_UserName$50,         	   /* Scanner user ID               */~
               au_TransType$15,            /* Atlas transaction status type */~
               au_Scan_Date$6,             /* Atlas scan date               */~
               au_Scan_Time$10,            /* Atlas scan time               */~
               au_Comments$20              /* Comments about change type    */
           
           dim ad_rec$64, ad_rec1$64,      /* Prod Scanning Audit File   */~
               ad_time$8, ad_key$33,       /* Time Stamp                 */~
               ad_atlas_time$8,            /* Atlas time          CR3033 */~
               ad_dept$3                   /* Aduit Dept and Process     */
               
           dim errmsg$32,                  /* Report variables           */~
               title$30, company$25,                                       ~
               rpt_time$8, rp_sel_d$30
               
           dim f2%(45%),                    /* = 0 if the file is open    */~
               fs%(45%),                    /* = 1 if file open, -1 if it */~
                                            /*   doesn't exist, or 0 if   */~
                                            /*   not yet checked (OPENCHCK*/~
               rslt$(45%)20                 /* Text from file opening     */                                            

            mat f2% = con
            mat fs% = con
 
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNDT ! Planning Tracking File                   *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! AWDAPPLD ! Appian Load File                         *~
            * #4  ! APCPLNAD ! Planning Master Audit File               *~
            * #5  ! APCPLNLD ! Planning/Scheduling Load Master File     *~
            * #6  ! AWDTRAIL ! Track Trailer and Load Assigned          *~
            * #7  ! AWDAPPLS ! New Appian Shipping Label File           *~
            * #8  ! BCKMASTR ! S.O. HEADER FILE                         *~
            * #9  ! BCKSUBPT ! New Sub Part Number File                 *~
            * #10 ! ATORSTTS ! PlyGem ATLaS Status Feed                 *~
            * #11 ! ATSTTSAD ! PlyGem Atlas Audit on Status Updates     *~
            * #12 ! AWDBAYBW ! New file for complaints          CR2097  *~
            * #14 ! AWDPLNCD ! Cross Docing Warranty ID Cross-Ref CR2108*~  
            * #15 ! APCPLNWT ! Warranty ID Cross-Ref to S.O.    CR2108  *~            
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen = 24
                        
            select #3, "AWDAPPLD",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =    5,                    ~
                        alt key 1,  keypos =  1,   keylen =  16,         ~
                            key 2,  keypos =  2,   keylen =  15,         ~
                            key 3,  keypos = 17,   keylen =  15
                                       
            select #4,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33

            select #5,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15

            select #6,  "AWDTRAIL",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  20,                     ~
                        alt key  1, keypos =   21, keylen =  20

            select #7,  "AWDAPPLS",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   20,                    ~
                        alt key 1,  keypos = 21,   keylen =  34, dup ,   ~
                            key 2,  keypos = 23,   keylen =  32, dup ,   ~
                            key 3,  keypos = 56,   keylen =  10, dup

            select #8, "BCKMASTR",                                       ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #9, "BCKSUBPT",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

            select #10, "ATORSTTS",                                      ~
                        varc,     indexed,  recsize =  358,              ~
                        keypos =    1, keylen =  57,                     ~
                        alt key  1, keypos =  183, keylen =  61, dup

            select #11, "ATSTTSAD",                                      ~
                        varc,     indexed,  recsize =  255,              ~
                        keypos =    1, keylen =  106         
/* CR2097 */                        
            select #12, "AWDBAYBW",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   29
/* CR2108 */
            select #14, "AWDPLNCD",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,   keylen =  8,                       ~
                        alt key 1, keypos =  9, keylen = 10, dup,        ~
                            key 2, keypos =  9, keylen = 18,             ~
                            key 3, keypos = 92, keylen =  8, dup
                                                   
            select #15, "APCPLNWT",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,   keylen =  8,                       ~
                        alt key 1, keypos =  9, keylen = 10, dup,        ~
                            key 2, keypos =  9, keylen = 18

REM            call "SHOSTAT" ("Opening Files, One moment Please?")
                                                        
            filename$ = "APCPLNDT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPLD" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDTRAIL" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPLS" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ATORSTTS" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ATSTTSAD" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDBAYBW" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error        
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),100%, rslt$(14%))    
            filename$ = "APCPLNWT" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error              
            
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
        init(" ") schema$    : schema% = 0%
        call "SCHEMA" (schema$,             /* What switch 1-NC 2-NE    */~
                       schema%,             /* Schema                   */~
                       #2,                  /* GENCODES                 */~
                       err% )               /* error                    */

        init(" ") scr_shft$, errcnt$  : pgmerr% = 0%  : errcnt%  = 0%
        rp_sel_d$ = "Atlas Status Update to Caelus Errors "
   
REM------------------------------------------------------------------------
REM             M A I N                                                   -
REM------------------------------------------------------------------------
        gosub select_printer
        gosub read_input
        REM gosub report_process
        gosub close_printer
        goto exit_program
      
REM------------------------------------------------------------------------
REM       B E G I N N I N G    O F    R O U T I N E S                     -
REM------------------------------------------------------------------------
        read_input
          at_key$  = hex(00)
          au_cntr% = 0%

L00100:          
          read #10, hold, key > at_key$, using L00120, at_reca$, at_recb$, ~
                     eod goto L00199

L00120:       FMT CH(182), CH(176)
            
            at_key$ = str(at_reca$,1%,57%)
            
            pgmerr% = 0%
            log_id$ = "ATL"
            gosub determine_shift
            gosub process_input_type

            if pgmerr% > 0% then gosub report_process            
REM            delete #10
            
            goto L00100      /* read next record */
        
L00199: return


REM         Transaction UID		CH(57)   at_reca$
REM			Pipe delimiter      CH(01)
REM         From Location  		CH(30)
REM			Pipe delimiter      CH(01)
REM         From Type		    CH(30)
REM			Pipe delimiter      CH(01)
REM         To Location		    CH(30)
REM			Pipe delimiter      CH(01)
REM         To Type			    CH(30)
REM			Pipe delimiter      CH(01)
REM         Barcode  	 	    CH(30)   at_recb$
REM			Pipe delimiter      CH(01)
REM         Transaction Type	CH(30)		(STATUS)
REM			Pipe delimiter      CH(01)
REM         Date			    CH(10)
REM 		Pipe delimiter      CH(01)
REM         Time			    CH(10)
REM			Pipe delimiter      CH(01)
REM         Load		    	CH(10)
REM			Pipe delimiter      CH(01)
REM         User Full Name		CH(50)
REM			Pipe delimiter      CH(01)
REM         User Name		    CH(30)

REM------------------------------------------------------------------------
REM Process Atlas input data
REM------------------------------------------------------------------------
        process_input_type
           init(" ") table$, code$, upd_st$, barcode$, scan_date$, scan_time$, ~
                     load_nbr$, full_name$, usr_name$
           at_transuid$  = str(at_reca$,1%,57%)
           at_from_dept$ = str(at_reca$,59%,3%)
           at_from_type$ = str(at_reca$,90%,30%)
           at_to_dept$   = str(at_reca$,121%,3%)
           at_to_type$   = str(at_reca$,152%,30%)
           
           trans_save% = 0%     /* CR3250 flag to save trans in audit */           
           table$ = "ATLASTRAN"
           code$  = str(at_recb$,32%,15%)    /* max length of 15 on status */
           if str(code$,1%,8%) = "TRANSFER" and ~
              str(at_to_type$,1%,4%) = "Zone"  then  ~
                str(code$,9%,4%) = "ZONE"
           at_status$ = str(at_recb$,32%,30%) 
           gosub check_code
           if code% = 0  then goto L00300
           if desc$ = "NA"  then return    /* no status update on these types */
/* CR3250 */
           if str(code$,1%,11%) = "TRUCK_CLOSE" then trans_save% = 1%
			  
           upd_st$    = str(desc$,1%,2%)           
           barcode$   = str(at_recb$,1%,18%)
           
           if str(barcode$,1%,4%) = "D-SO" then pgmerr% = 3%
           if pgmerr% = 3% then return
           
           scan_date$ = str(at_recb$,63%,10%)
           call "DATUFMTC" (scan_date$)
           scan_time$ = str(at_recb$,74%,10%)     /* hh:mm:ss */
           load_nbr$  = str(at_recb$,85%,10%)
           full_name$ = str(at_recb$,96%,50%)
           usr_name$  = str(at_recb$,147%,30%)
           
           if upd_st$ < "11" then return
           if upd_st$ < "15" or upd_st$= "26" then  gosub prod_scan_stage
           if upd_st$ = "16" then  gosub prod_scan_load
           if pgmerr% = 0%  then return
L00300:
          /*  error with transcation status */
           pgmerr% = 1%                /* error processing status */          
REM  L00399:           
        return

REM------------------------------------------------------------------------
REM Determined the shift by Atlas scan time  
REM------------------------------------------------------------------------
        determine_shift
           strtshft1$ = "06:00" : endshft1$ = "14:59"
           strtshft2$ = "15:00" : endshft2$ = "21:59"
           strtshft3$ = "22:00" : endshft3$ = "05:59"
           scn_time$ = str(at_recb$,74%,10%)
          
           scr_shft$ = "03"
           if scn_time$ >= strtshft1$ and scn_time$ <= endshft1$ then ~
             scr_shft$ = "01"  
           if scn_time$ >= strtshft2$ and scn_time$ <= endshft2$ then ~
               scr_shft$ = "02"
/* CR3033 + */              
           init(" ") ad_atlas_time$
           str(ad_atlas_time$,1%,2%) = str(at_recb$,74%,2%)
           str(ad_atlas_time$,3%,2%) = str(at_recb$,77%,2%)
           str(ad_atlas_time$,5%,2%) = str(at_recb$,80%,2%)
           call "TIME" (ad_atlas_time$)           
       
/* CR3033 - */       
        return
        
REM------------------------------------------------------------------------
REM  Generic routine to lookup a table in the GENCODES file
REM------------------------------------------------------------------------                                                
        check_code
          code% = 0%
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = table$
          str(readkey$,10%,15%) = code$
          read #2,key = readkey$, using L00500, desc$, eod goto L00599
L00500:       FMT POS(25), CH(30)
          code% = 1%
L00599: return


REM------------------------------------------------------------------------
REM       S T A G I N G                                                   -
REM------------------------------------------------------------------------
        prod_scan_stage                       
        
           if updatedCrossDock% <> 1% then goto L01000
           brand% = 99%

L01000:                                                        
          init(" ") sav_bar$
          updatedCrossDock% = 0%
                                                            
          if schema% = 1% and str(barcode$,1%,1%) = "A" then          ~
                                                   goto notUpdateCrossShip
          if schema% = 2% and str(barcode$,1%,1%) = "B" then          ~
                                                   goto notUpdateCrossShip
                                                           
          if str(barcode$,1%,1%) <> "A" and str(barcode$,1%,1%) <> "B" ~
                                              then goto notUpdateCrossShip
             updatedCrossDock% = 1%
             sav_bar$ = barcode$
             str(barcode$,1%,1%) = "0"

notUpdateCrossShip:
/*CR2097 */    autostage% = 0%
               hold_upd_st$ = upd_st$
               gosub read_dt_status               /* get current status */
               if dt_st$ = "!!" then goto notst12 /* still send for error rpt*/
               
               if upd_st$ = "14" and dt_st$ < "12" then autostage% = 1%
               
               if autostage% = 0% then goto notst12
               
               upd_st$ = "12"
               log_id$ = "AT1"
               gosub update_shipping 
               upd_st$ = hold_upd_st$           /* reset to 14 */               
               log_id$ = "ATL"
notst12:               
          gosub update_shipping                 /* UPDATE_SHIPPING */
          gosub set_load_start

           brand% = 99%
        return


REM------------------------------------------------------------------------
REM       L O A D I N G                                                   -
REM------------------------------------------------------------------------
        prod_scan_load

            init(" ") sav_bar$
            updatedCrossDock% = 0%

            init(" ") wandchar_trailer$

            gosub check_trailer
REM            if trailer%=1% then gosub set_load_start
                                                 
REM            if by_ups% = 1% then trailer% = 1%

REM            if trailer% = 0% then goto prod_scan_3

               if schema% = 1% and str(barcode$,1%,1%) = "A" then           ~
                                                   goto notUpdatedCrossDock
               if schema% = 2% and str(barcode$,1%,1%) = "B" then           ~
                                                   goto notUpdatedCrossDock
               if str(barcode$,1%,1%) <> "A" and str(barcode$,1%,1%) <> "B" ~
                                              then goto notUpdatedCrossDock
                 updatedCrossDock% = 1%
                 sav_bar$ = barcode$
                 str(barcode$,1%,1%) = "0"

notUpdatedCrossDock:
/*CR2097 */    autostage% = 0%
               hold_upd_st$ = upd_st$
               gosub read_dt_status
               if dt_st$ = "!!" then goto notauto
               
               if upd_st$ = "16" and dt_st$ < "12" then autostage% = 1%
               
               if autostage% = 0% then goto notauto
            
               upd_st$ = "12"
               log_id$ = "AT2"
               gosub update_shipping 
               upd_st$ = hold_upd_st$       /* reset to 16 */               
               log_id$ = "ATL"

notauto:
               gosub update_shipping    
               gosub update_shipping_appian           

        return
        
REM------------------------------------------------------------------------
REM  Read the status for the barcode to use in auto backflash             -
REM------------------------------------------------------------------------
        read_dt_status                           
            init(" ") dt_key$, dt_rec$
            dt_st$ = "!!"
            valid_bar% = 0%
            
            str(dt_key$,1%,18%) = barcode$
        read_status_nxt
            read #1, key > dt_key$, using L03000 , dt_rec$,            ~
                                                     eod goto L02400
            dt_key$ = str(dt_rec$,24%,23%)  
            
            if str(dt_key$,1%,18%) <> barcode$ then goto L02200
            if str(dt_rec$,42%,3%) = "001" then goto read_status_nxt
            
            dt_st$ = str(dt_rec$,64%,2%) 

L02200:            
        return
L02400: 
        valid_bar% = 1%
        return
  
REM------------------------------------------------------------------------
REM  Update the shipping status for all barcodes and do audit             -
REM------------------------------------------------------------------------
        update_shipping                           
            init(" ") prevcode$, dateout$, dt_key$, dt_rec$, dt_part$, ~
                      dt_load$, dt_dept$, dt_proc$, dt_date$, dt_st$, ~
                      dt_ref$
            call "TIME" (dateout$)
            valid_bar% = 0%
            firstpass% = 1%
            
            str(dt_key$,1%,18%) = barcode$
        update_shipping_nxt
            read #1,hold,key > dt_key$, using L03000 , dt_rec$,            ~
                                                     eod goto L03900
L03000:        FMT CH(256)
            dt_key$ = str(dt_rec$,24%,23%)
            if str(dt_key$,1%,18%) <> barcode$ then goto L03900
               valid_bar% = 1%
               current_drop$ = str(dt_rec$,11%,12%)            
               current_load$ = str(dt_rec$,1%, 5%)  
               str(barcode_shp$,1%,18%)  = barcode$                
               str(barcode_shp$,19%,02%) = current_drop$
               dt_part$ = str(dt_rec$,189%,25%)
               dt_load$ = str(dt_rec$,1%,5%)
               dt_dept$ = str(dt_rec$,42%,3%)
               dt_proc$ = str(dt_rec$,45%,2%)
               dt_date$ = str(dt_rec$,47%,6%)
               dt_st$   = str(dt_rec$,64%,2%)
               dt_ref$ = str(dt_rec$,96%,8%)        /* CR2108 */
               if dt_st$ = "16" then pgmerr% = 4%   /* already completed */
               if upd_st$ = "12" and dt_st$ > "12" then pgmerr% = 5% 
               if upd_st$ = "13" and dt_st$ > "12" then pgmerr% = 6%
               if upd_st$ = "14" and dt_st$ > "14" then pgmerr% = 9%
               if upd_st$ = dt_st$ then pgmerr% = 7%
               if pgmerr% <> 0% then goto L03800
/* CR2282 */
               if upd_st$ > "13" then goto L03700
               gosub edit_support_depts
               if dt_update% = 0% then update_shipping_nxt
               
L03700:        gosub lookup_sub_part
       
               delete #1
               
               str(dt_rec$,53%,6%) = scan_date$      /* Product Scan Date*/
               str(dt_rec$,64%,2%) = upd_st$         /* Atlas Scanned  */
REM               str(dt_rec$,116%,8%)= dateout$        /* TimeProdScanned  */
               str(dt_rec$,116%,8%)= ad_atlas_time$  /* CR3033 Atlas Time   */
                          
               put #1, using L03000 , dt_rec$
               write #1, eod goto L03900
 
               modulename$ = "UPDATE_SHIPPING"
               ad_dept$ = dt_dept$
               gosub write_audit                        /* new audit file */
               if firstpass% = 1% and (upd_st$ = "14" or upd_st$ = "16") then ~
                      gosub process_14_16
               firstpass% = 0%
               ad_dept$ = dt_dept$
               gosub update_audit                       /* old audit file */
/* CR2097 */
               scr_dept$ = str(dt_rec$,42%,3%)      /* Scanning Department*/
               if upd_st$ = "14" then scr_dept$ = "106"
               if upd_st$ = "16" then scr_dept$ = "108"
               scr_id$   = log_id$                   
/* CR2097 */   call "AWDCOMSB" (dt_rec$,#12,scr_dept$,scr_id$) 

L03800:        if pgmerr% <> 0% then gosub report_process

/* CR3250 if truck close and not automate staging, add truck close */               
               if trans_save% = 1% and  upd_st$ = "16" then gosub truck_audit
               goto update_shipping_nxt
               
L03900:     prevcode$ = barcode$
            if valid_bar% = 0% then pgmerr% = 2%  /* barcode not found */
            if pgmerr% <> 0% then gosub report_process
            
/* only for truck loads CR2108 */         
            if upd_st$ < "14" or pgmerr% <> 0% then goto L03910   
            gosub check_cross_dock                
L03910:  
        return                                 

REM------------------------------------------------------------------------
REM   Write audit for Truck_Load and Truck_Close everytime  CR3250        -
REM------------------------------------------------------------------------
       truck_audit
	   
          modulename$ = "SAVETRUCKTRANS"
		  reset_status% = 1%
          ad_dept$ = dt_dept$
          gosub write_audit                        /* new audit file */
		  reset_status% = 0%
		   
       return
	   
REM------------------------------------------------------------------------
REM   Validate support departments                   CR2282               -
REM------------------------------------------------------------------------
       edit_support_depts
         
         dt_update% = 0%         
         table$ = "PLAN SUPP"
         code$  = str(at_from_dept$,1%,3%)
         gosub check_code
         atlas_support% = code%
         
         dt_support% = 0%
         code$  = dt_dept$
         gosub check_code
         if dt_dept$ = "001" then code% = 0%
         dt_support% = code%

         valid_dept% = 0%
         table$ = "PLAN DEPT"
         code$  = str(at_from_dept$,1%,3%) 
         gosub check_code
         valid_dept% = code%
         
/* criteria to not update status */         
       /* regular transfer status 13 not needed as of 10/15/2019 per CMG */
         if upd_st$ = "13" then goto L04800  
         
         if atlas_support% = 1% and dt_dept$ <> str(at_from_dept$,1%,3%) then ~
             goto L04800
             
         if atlas_support% = 0% and dt_support% = 1% then goto L04800
         
         dt_update% = 1%
          
L04800:  return
       
REM------------------------------------------------------------------------
REM   Write Audit of status update made                                   -
REM------------------------------------------------------------------------
       write_audit
           au_cntr% = au_cntr% + 1% 
           convert au_cntr% to au_cntr$, pic (00000)
  
           str(audit_key$,1%,57%) = at_transuid$
           str(audit_key$,58%,6%) = date
           str(audit_key$,64%,8%) = time
           str(audit_key$,72%,6%) = dt_date$
           str(audit_key$,78%,18%) = barcode$
           str(audit_key$,96%,3%) = dt_dept$
           str(audit_key$,99%,3%) = at_from_dept$
           str(audit_key$,102%,5%) = au_cntr$
           read #11, hold, key = audit_key$, using L05100, au_TransUID$,      ~
                  au_AuditDate$, au_AuditTime$, au_ProdDate$, au_Barcode$,    ~
                  au_Department$, au_Atl_Dept$, au_cntr$,                     ~
                  au_BeforeStatus$, au_AfterStatus$, au_Shift$,               ~
                  au_UserName$, au_TransType$, au_Scan_Date$, au_Scan_Time$,  ~
                  au_Comments$, ~
             eod goto L05500

L05100:    FMT CH(57), CH(06), CH(08), CH(06), CH(18), CH(03), CH(03), CH(05), ~
               CH(02), CH(02), CH(02), CH(50), CH(15), CH(06), CH(10), CH(20)

             delete #11
             
             au_TransUID$ = at_transuid$
             au_AuditDate$ 	= date
             au_AuditTime$	= time
             au_ProdDate$ = dt_date$    
             au_Barcode$  = barcode$
             au_Department$  = dt_dept$	
             au_Atl_Dept$    = at_from_dept$
             au_cntr$        = au_cntr$
             au_BeforeStatus$ = dt_st$
             au_AfterStatus$ = upd_st$ 
/* CR3250 */ if reset_status% = 1% then au_AfterStatus$ = "18"		 
             au_Shift$		= scr_shft$
             au_UserName$	= full_name$
             au_TransType$  = str(at_recb$,32%,15%)
             au_Scan_Date$  = scan_date$
             au_Scan_Time$  = scan_time$
             au_Comments$   = modulename$

             put #11, using L05100, au_TransUID$,      ~
                  au_AuditDate$, au_AuditTime$, au_ProdDate$, au_Barcode$,    ~
                  au_Department$, au_Atl_Dept$, au_cntr$,                     ~
                  au_BeforeStatus$,au_AfterStatus$, au_Shift$,                ~
                  au_UserName$, au_TransType$, au_Scan_Date$, au_Scan_Time$,  ~
                  au_Comments$
             write #11, eod goto L05900
           return
                         
L05500:
       
             au_TransUID$ = at_transuid$
             au_AuditDate$ 	= date
             au_AuditTime$	= time
             au_ProdDate$ = dt_date$   
             au_Barcode$  = barcode$
             au_Department$  = dt_dept$	
             au_Atl_Dept$    = at_from_dept$
             au_cntr$        = au_cntr$
             au_BeforeStatus$ = dt_st$
             au_AfterStatus$ = upd_st$ 
/* CR3250 */ if reset_status% = 1% then au_AfterStatus$ = "18"			 
             au_Shift$		= scr_shft$
             au_UserName$	= full_name$
             au_TransType$  = str(at_recb$,32%,15%)
             au_Scan_Date$  = scan_date$
             au_Scan_Time$  = scan_time$
             au_Comments$   = modulename$
           
             put #11, using L05100, au_TransUID$,      ~
                  au_AuditDate$, au_AuditTime$, au_ProdDate$, au_Barcode$,    ~
                  au_Department$, au_Atl_Dept$, au_cntr$,                     ~
                  au_BeforeStatus$,au_AfterStatus$, au_Shift$,                ~
                  au_UserName$, au_TransType$, au_Scan_Date$, au_Scan_Time$,  ~
                  au_Comments$
             write #11, eod goto L05900

        return
        
L05900:     pgmerr% = 10%
            gosub report_process
        return 
        
REM------------------------------------------------------------------------
REM   Set the audit on status 14 or 16                                    -
REM------------------------------------------------------------------------
        process_14_16
        
          if upd_st$ = "14" then ad_dept$ = "106"
          if upd_st$ = "16" then ad_dept$ = "108"
          
          gosub update_audit

        return
        
REM------------------------------------------------------------------------
REM   Write Or Update old Audit of status update made                     -
REM------------------------------------------------------------------------
        update_audit                               

          init(" ") ad_rec$, ad_time$, ad_key$, ad_rec1$
          call "TIME" (ad_time$)
          str(ad_rec$,1%,18%) = barcode$                    /* Barcode   */
          str(ad_rec$,19%,6%) = scan_date$                  /* Scan Date */
          str(ad_rec$,25%,3%) = ad_dept$                    /* Department*/
          str(ad_rec$,28%,2%) = dt_proc$                    /* Process   */
          str(ad_rec$,30%,2%) = scr_shft$                   /* Shift Code*/
          str(ad_rec$,32%,2%) = upd_st$                     /* Status    */
                                                         
REM          if override% = 1% and modulename$ = "UPDATE_TRACK_OVERRID"    ~
             then str(ad_rec$,32%,2%) = "12"
                                                          
          str(ad_rec$,34%,18%)= barcode$                    /* Barcode   */
REM          str(ad_rec$,52%,8%) = ad_time$                    /* Time Stamp*/
/* CR3033 */
          str(ad_rec$,52%,8%) = ad_atlas_time$              /* Atlas Scan Time*/
          str(ad_rec$,60%,3%) = log_id$                     /* User Id Atlas */
          str(ad_rec$,63%,2%) = "  "                        /* Filler    */

          ad_key$ = str(ad_rec$,19%,33%)
          read #4,hold,key = ad_key$, using L07430,ad_rec1$,eod goto L07420

             delete #4

L07420:     put #4, using L07430 , ad_rec$
L07430:       FMT CH(64)
            write #4, eod goto L07460

        return
        
L07460:     pgmerr% = 8%
            gosub report_process
        return
      
REM------------------------------------------------------------------------
REM   read for trailer information
REM------------------------------------------------------------------------

        check_trailer                                  
           init(" ") scr_load$, sav_drop$, scan_drop$
           trailer%  = 0%
           init(" ") tr_key0$

           str(tr_key0$,1%,1%) = "0"             /* Look for Open Trailer*/
           str(tr_key0$,2%,8%) = barcode_trailer$   /*Trailer No Scanned */

           read #6, key 0% > tr_key0$, using L11800, tr_rec$, eod goto L11810
L11800:         FMT CH(128)

           if str(tr_rec$,1%,9%) <> str(tr_key0$,1%,9%) then goto L11810
             scr_load$ = str(tr_rec$,10%,5%)
             gosub check_load
               if code% = 0% then goto L11810    /*Invalid Load Number   */
             sav_drop$  = "XX"                   /* Init Save Drop No.   */
             scan_drop$ = "YY"                   /* Init Current ScanDrop*/
             trailer%  = 1%                      /* Valid Trailer & Load */
             current_load$ = scr_load$           /* SR69112              */

L11810: return

REM------------------------------------------------------------------------
REM  Check for a valid load
REM------------------------------------------------------------------------
        check_load
          code% = 0%
          convert scr_load$ to scr_load%, data goto L11930

          convert scr_load% to scr_load$, pic(00000)

          goto L11940
                                                 /* Alpha or Stock Loads */
L11930:   convert str(scr_load$,2%,4%) to scr_load%, data goto L11920

          convert scr_load% to str(scr_load$,2%,4%), pic(0000)
L11940:

          read #5,key = scr_load$, using L11890, scr_load_d$,         ~
                                                    eod goto L11920
L11890:          FMT POS(16), CH(30)
            code% = 1%
        return
                                                /* This should Not Occur */
L11920:   errormsg$ = "(Error) - Invalid Load Number. Load No. Required?"
          init(" ") scr_load$, scr_load_d$
        return
        

REM------------------------------------------------------------------------
REM  Check the current setting on the Appian record
REM------------------------------------------------------------------------
        check_shipping_appian                             /* (AWD043)    */
           err%   = 0%
           check% = 0%
           sav_drop% = 0%                                 /* Drop Changed*/
           init(" ") awd_app_key0$
           awd_app_key0$ = barcode_shp$
           read #7, key 0% = awd_app_key0$, using LAPP_FMT, howship$,  ~
                   eod goto LAPP_1
LAPP_FMT:    FMT POS(488), CH(2)              /* add howship CR1791 */

             check% = 1%
             scan_drop$   = str(barcode_shp$,19%,2%) /* From Scanned Brcd*/

              if scan_drop$ = "99" then return       /* Skip '99' Drops  */
                                                     /* Init Drop Sort   */
              if sav_drop$ = "XX" then sav_drop$ = scan_drop$
                                                    /* Verify CurrentDrop*/
              if sav_drop$ = scan_drop$ then sav_drop% = 1%

LAPP_1:     check% = 0%
            err% = 24%

        return

REM------------------------------------------------------------------------
REM  Update the Appian scan date and time 
REM------------------------------------------------------------------------
        update_shipping_appian
           init(" ") awd_app_key0$, errormsg$, rec$(), calc_time$,        ~
                     app_scan_tme$

           calc_time$ = time                      /* Military - HHMMSSXX */
           app_scan_tme$ = str(calc_time$,1%,4%)
           awd_app_key0$ = barcode_shp$
           read #7,hold,key 0% = awd_app_key0$, using LAPP_3, rec$(),    ~
                                                            eod goto LAPP_4
LAPP_3:       FMT 4*CH(256)                        /* (AWD054)           */

           delete #7

           str(rec$(),522%,6%) = date              /* Scan Date          */
           str(rec$(),528%,4%) = app_scan_tme$     /* Scan Time-Military */
           put #7, using LAPP_3, rec$()
           write #7, eod goto LAPP_4

LAPP_4: return

REM--------------------------------------------------
REM Set the start date and time for the load 
REM--------------------------------------------------
        set_load_start                             /* Update (AWDAPPLD)  */
            init(" ") ld_app_key0$, ld_app_rec$
            ld_app_key0$ = dt_load$
            read #3, hold, key 0% = ld_app_key0$, using L51000, ld_app_rec$,~
                                                      eod goto L51200
L51000:        FMT CH(128)  
            
            if str(ld_app_rec$,93%,4%) > " " then goto L51200  /* start time */

            delete #3
            
            str(ld_app_rec$,87%,6%) = date         /* truck start load date */
            calc_time$ = time                      /* Military - HHMMSSXX */
            app_scan_tme$ = str(calc_time$,1%,4%)  
            str(ld_app_rec$,93%,4%) = app_scan_tme$  

            write #3 using L51000, ld_app_rec$, eod goto L51200          

L51200:     return

REM------------------------------------------------------------------------
REM   New Cross Docking Routine    CR2108
REM------------------------------------------------------------------------
        check_cross_dock
            cross_err% = 0%                      /* Cross Dock Situation */
            if dt_dept$ = "102" then return
REM DO NOT CROSS DOCK HOPPER OR GARDEN WINDOWS
            if str(dt_part$,1%,3%) = "998" and dt_dept$ = "101" then return
/* CR2532 */
            if str(dt_part$,1%,3%) = "995" and dt_dept$ = "101" then return
            if str(dt_part$,1%,3%) = "H01" and dt_dept$ = "101" then return
            if dt_dept$ <> "100" and dt_dept$ <> "101" then return
REM same build and ship locations
            if info_flds$(5%) = info_flds$(7%) then return    
            
            gosub find_warranty             
            if cross_err% <> 0% then goto CROSS_DOCK_ERR2
             
            init(" ") dt_key$
            str(dt_key$,1%,18%) = str(cross_dock_key1$,1%,18%)
            str(dt_key$,19%,3%) = dt_dept$
            str(dt_key$,22%,2%) = dt_proc$
            read #1, hold, key = dt_key$, using L53000, dt_rec$,     ~
                                                  eod goto CROSS_DOCK_ERR2
L53000:        FMT CH(256)
            delete #1

            str(dt_rec$,96%,8%) = cross_warranty$
            put #1, using L53000 , dt_rec$
            write #1, eod goto CROSS_DOCK_ERR2

CROSS_DOCK_ERR2:
        return

 REM *************** FIND WARRANTY ****************************  CR2108
        find_warranty                              /* (APCPLNWT)         */
                                                   /* (AWD051)-(AWDPLNCD)*/
           init (" ") warranty_key$, cross_warranty$, cross_dock_key1$
                                                   /* (PAR004)2ND Scanned*/
           cross_dock_key1$ = barcode$                                                   
           warranty_key$ = "A" & str(barcode$,2%,18%)   /*  Production Barcode*/
           read #15,key 2% = warranty_key$, using L16000, cross_rec$,      ~
                                                           eod goto L16020
L16000:       FMT CH(128)
                                              /* Save Warranty ID Found  */
              cross_warranty$ = str(cross_rec$,1%,8%)

                          /* Verify Part Number */
              if cross_part$ <> str(cross_rec$,27%,25%) then goto L16025
                                              /* Verify sub Part         */
              if cross_sub_part$ <> str(cross_rec$,52%,20%) then goto L16025

                                              /* Correct MFG Part        */
                                           /* Read Warranty again locked */
              read #15,hold,key 0% = cross_warranty$, using L16000,        ~
                                              cross_rec$, eod goto L16020

                 delete #15
        /* Replace the Production Barcode with 1st label Scanned's Barcode*/
              str(cross_rec$,9%,18%) = str(cross_dock_key1$,1%,18%)
              put #15, using L16000, cross_rec$
              write #15, eod goto L16020

                           /* Now Update New Warranty database (AWDPLNCD)*/
              cross_warranty2$ = dt_ref$
              gosub update_cross_dock

        return
L16020:    cross_err%  = 1%                   /* Warranty Error          */
        return
L16025:    cross_err%  = 2%                   /* Part Number Validation  */
        return
        
REM *************** UPDATE_CROSS_DOCK ****************************   CR2108
        update_cross_dock                          /* (AWDPLNCD)          */
          init(" ") cross_rec1$                    /* Shouldn't Exist     */
                                                   /* 2nd ProductionBarcod*/
                                                   /* Scanned.            */
          read #14,hold,key 0% = cross_warranty2$, using L16000,           ~
                                          cross_rec1$, eod goto L16100

              delete #14
L16100:

              cross_rec1$ = cross_rec$        /* 45 Digit Part Number    */
                                              /* Info and New Info Fields*/
                                              /* Replace Warranty Id with*/
                                              /* the Warranty Id from the*/
                                              /* 1st Label Scanned       */
              str(cross_rec1$,1%,8%)  = cross_warranty2$
                                              /* Replace the Production  */
                                              /* Barcode with the Product*/
                                              /* Barcode from the 2nd Lbl*/
                                              /* Scanned                 */
              str(cross_rec1$,9%,18%) = warranty_key$
                                              /* New Cross Dock Record   */
                                              /* Lnk AWDPLNCD to APCPLNWT*/
                                              /* Warranty ID from the    */
                                              /* 2nd Label Scanned       */
              str(cross_rec1$,92%,8%)   = cross_warranty$  /*In APCPLNWT */
                                              /* Production Barcode from */
                                              /* the 1st label Scanned   */
              str(cross_rec1$,100%,18%) = cross_dock_key1$  /*In APCPLNWT*/
              put #14, using L16000, cross_rec1$
              write #14, eod goto L16110

         return
L16110:    cross_err%  = 1%                   /* Warranty Error          */
        return                                  

REM------------------------------------------------------------------------
REM   Lookup subpart number
REM------------------------------------------------------------------------
        lookup_sub_part
           init(" ") bcksubpt_rec$, flds$(), info_flds$(), cross_part$,   ~
                     cross_sub_part$, cross_sub_info$
           flag$ = "0"                       /* Sales Order Info         */
           pgm$  = "1"
           err1% = 0%
           so_inv$ = str(barcode$,1%,8%)
           item_no$ = str(barcode$,9%,2%)
           
           convert so_inv$ to so_inv%, data goto convert_alpha

           convert so_inv% to so_inv$, pic(00000000)

           goto order_converted

convert_alpha:
           convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
           convert so_inv% to str(so_inv$,2%,7%), pic(0000000)

order_converted:
           convert item_no$ to item_no%, data goto sub_part2
sub_part2:
           convert item_no% to item_no$, pic(###)

           call "AWDBKSUB"   (flag$,      /* Flag 0=SalesOrder 1=Invoice*/~
                              pgm$,       /* Calling Program 0=BCKUPDTE */~
                                          /* 1=Any Other 2=Delete       */~
                                          /* 3=Invoice                  */~
                              so_inv$,    /* SO or Invoice Num to lookup*/~
                              item_no$,   /* Item Number                */~
                              bcksubpt_rec$, /* Record If BCKUPDTE then */~
                                          /* pass in else pass out      */~
                              flds$(),    /* Part Number Fields         */~
                              info_flds$(), /* Information Fields       */~
                              #9,         /* BCKSUBPT File              */~
                              err1%)      /* Error Code                 */

            if err1% <> 0% then                             ~
                    str(bcksubpt_rec$,48%,20%) = "00000               "

              cross_part$     = str(bcksubpt_rec$,23%,25%)

              cross_sub_part$ = str(bcksubpt_rec$,48%,20%)

              cross_sub_info$ = str(bcksubpt_rec$,132%,20%)
              if err1% = 0% then return

       return
REM------------------------------------------------------------------------
REM    Print report section
REM------------------------------------------------------------------------


L55040: %+---------------------------------------------------------------------~
        ~-----------------------------+
L55080: %!######## @ ########                   #########################      ~
        ~                   Page: ### !

        REM - Detail                                                    
L55150: %! Barcode          !DT Dpt!From Dpt!Atlas Status                  !Err~
        ~or Message                   !
L55170: %!##################!######!########!##############################!###~
        ~#############################!
L55190: %!------------------!------!--------!------------------------------!---~
        ~-----------------------------!
L55220: %!Report Totals : ##########                                           ~
        ~                             !

       select_printer
            pageno% = 0%
            lcnt%    = 99%
            title$ = rp_sel_d$                       
            call "FMTTITLE" (title$, " ", 12%)
            date$ = date  :  call "DATEFMT" (date$)
            company$ = "Atrium Windows and Doors"     
            call "SETPRNT" ("ATSTA", " ", 0%, 0%)
            select printer (134)
        return
        
       report_process
          if pgmerr% = 1% then errmsg$ = "Unable to find status sent     "
          if pgmerr% = 2% then errmsg$ = "Barcode was not found in Caelus"
          if pgmerr% = 3% then errmsg$ = "Atlas Barcode, do not process  "
          if pgmerr% = 4% then errmsg$ = "Barcode Already Loaded         "
          if pgmerr% = 5% then errmsg$ = "Barcode Already Complete       "
          if pgmerr% = 6% then errmsg$ = "Barcode beyond Transfer        "
          if pgmerr% = 7% then errmsg$ = "Barcode Already Set to Status  "
          if pgmerr% = 8% then errmsg$ = "Unable to write old audit rec  "
          if pgmerr% = 9% then errmsg$ = "Barcode beyond Staged          "
          if pgmerr% =10% then errmsg$ = "Issue writing new audit record "
                  
          errcnt% = errcnt% + 1                  
          gosub print_dtl
          pgmerr% = 0%        /* reset */
       return
       
       print_header
            init(" ") rpt_time$
            call "TIME" (rpt_time$)
            pageno% = pageno% + 1%
            if lcnt% <> 99% then print using L55040
            print page
            print using L55040
            print using L55080, date$, rpt_time$, company$, pageno%
            print using L55150
            print using L55190
            lcnt% = 7%
       return 
            
       print_dtl
            if lcnt% > 58% then gosub print_header
            print using L55170, barcode$, dt_dept$, at_from_dept$, at_status$,~
                                errmsg$
            lcnt% = lcnt% + 1%
       return
        
       close_printer
            print using L55040
            convert errcnt% to errcnt$, pic(#########0)
            print using L55220, errcnt$
            call "SETPRNT" ("ATSTA", " ", 0%, 1%)
       return
        
REM------------------------------------------------------------------------
REM       F I L E  O P E N  E R R O R                                     -
REM------------------------------------------------------------------------

        open_error
            errormsg$ = "(Open Error) - File = " & filename$
        return

REM------------------------------------------------------------------------
REM       F I L E  O P E N  E R R O R                                     -
REM------------------------------------------------------------------------        
REM        open_file
REM            init(" ") library$, volume$
REM            library$        = "NEDATA"
REM            volume$ = "NE2"                                            
REM            if schema% = 1% then volume$ = "CARLO2"
REM            if schema% = 1% then library$ = "NEDATA"
REM
REM             open nodisplay #10,  output, space = 500%,                 ~
REM                dpack   = 500%, ipack = 500%, file = file$,              ~
REM                library = library$, volume = volume$, blocks = 30%
REM        return
REM------------------------------------------------------------------------
REM       E X I T    P R O G R A M                                        -
REM------------------------------------------------------------------------

        exit_program
REM            call "SHOSTAT" ("One Moment Please")
               call "ALLFREE"
        end
