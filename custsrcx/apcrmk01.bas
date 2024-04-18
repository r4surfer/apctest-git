      REM   *************************************************************~
            *  Program Name      - APCRMK01.BAS                         *~
            *  Creation Date     - 08/14/2019                           *~
            *  Written By        - Ricky Beane                          *~
            *                                                           *~
            *  Description       - Input from Remake Now System         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *09/11/2019| SRProj Do not error on dept check        ! RDB *~
             *************************************************************


        dim filename$8,                  /* Use with EWDOPEN           */~
            readkey$24, desc$30,         /* GENERIC KEY                */~
            table$9, code$3,             /* TABLE LOOKUP NAME AND CODE */~
            xx$(7%)50                    /* Screen Display area Text   */

REM        dim rlib$8,                   /* Run library for link call  */~
            rvol$6,                      /* Run volume for link call   */~
            run$8                        /* Program to Run             */

        dim                            /* (APCPLNGR) - File            */~
            rm_key$12, rm_rec$(4%)128,   /* Re-Make Key and Record     */~
            rm_bar$12,                   /* Remake Glass Barcode       */~
            rm_reason$2, reason_d$30,    /* Glass Remake Reason Code   */~
            rm_part$25, rm_gls$2,        /* Remake part and glass      */~ 
            rm_lit$2,                    /* Remake MFG                 */~
            glstype$20,                  /* Glass type PLAN SCH2       */~
            rma_rec$64,                  /* (APCPLNGA)                 */~
                                       /* (APCRMKNW)                   */~
            now_key$60,                  /* Key                        */~
            now_rec$125,                 /* Input record               */~
            now_date$10,                 /* Date YYYY-MM-DD            */~
            now_time$10,                 /* Time HH:MM:SS              */~
            now_barcode$10,              /* Glass barcode              */~
            now_reason_d$30,             /* Reason code description    */~
            now_dept$10,                 /* Caelus department number   */~
            now_reason$10,               /* Remake status              */~
                                       /* (APCPLNDT)                   */~
            dt_key$23,                   /* Key                        */~
            dt_bar$18,                   /* Production barcode         */~
                                       /* (APCRMKER)                   */~
            er_rec$125,                  /* Record                     */~
            er_msg$(99%)25,              /* Error Description          */~
            er_error$3,                  /* Error code                 */~
            filler1$83                   /* Filler space               */
   
        dim hldso$8,                     /* SALES ORDER NUMBER         */~
            hldln$2,                     /* S.O. LINE ITEM NO.         */~
            hldlnitem$4,                 /* LINE ITEM PIECE            */~
            hldKey$16,                   /* HLDSCHED Key               */~
            hldLamn$1,                   /* Laminate Flag              */~
            savHld$16,                   /* Save HLDSCHED Key          */~
            hldRec$256,                  /* HLDSCHED Rec               */~
            hldType$1,                   /* Hld Type                   */~
            hldStatus$1,                 /* Hld Status                 */~
            hldscrmkKey$28,              /* HLDSCRMK Key 2             */~
            hldscrmkRec$256,             /* HLDSCRMK Rec               */~
            hldbar$9,                    /* remake barcode             */~
            hldnum$3,                    /* remake number              */~
            hldtime$8,                   /* Remake Time                */~
            hldrmkStatus$1,              /* HldRemake Status           */~
            hldrmkType$1                 /* HldRemake Type             */
            
         dim rm_dept_n$3,                /* New Department No          */~
             dept_d$30,                  /* Department Desc            */~
             rm_status$1                 /* Remake Status              */

        dim f2%(50%),                    /* = 0 if the file is open    */~
            fs%(50%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(50%)20                 /* Text from file opening     */
 
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCRMKNW ! Remake Now Input file                    *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! APCPLNGR ! Glass Remake File                        *~
            * #4  ! APCPLNDT ! Planning Tracking File                   *~
            * #5  ! APCPLNGA ! Re-Make Glass Audit File                 *~
            * #7  ! APCRMKER ! Remake Now Error File                    *~
            * #8  ! HLDSCHED ! Hold Schedule file                       *~
            * #9  ! HLDSCRMK ! Special Temp Glass Remake                *~
            * #10 ! APCPLNDT ! Planning Tracking File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCRMKNW",                                      ~
                        varc,     indexed,  recsize =  125,              ~
                        keypos = 1,   keylen = 60

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen = 24

            select #3,  "APCPLNGR",                                      ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos = 22,   keylen = 12,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33,             ~
                            key 3, keypos = 13, keylen = 21

            select #4,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #5,  "APCPLNGA",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =  1,   keylen = 18,                      ~
                        alt key 1, keypos =  7, keylen = 12

            select #7,  "APCRMKER",                                      ~
                        varc,     indexed,  recsize =  125,              ~
                        keypos =  1,   keylen = 29

            select #8, "HLDSCHED",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  40,                     ~
                        alt key  1, keypos =   16, keylen =  25,         ~
                            key  2, keypos =   27, keylen =  16

            select #9,  "HLDSCRMK",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,   keylen = 52,                       ~
                        alt key 1, keypos =  16, keylen = 37,            ~
                            key 2, keypos =  27, keylen = 28

            select #10,  "APCPLNDT",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
                            
            filename$ = "APCRMKNW" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNGR" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),500%, rslt$(5%))
            
            filename$ = "APCRMKER" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HLDSCHED" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HLDSCRMK" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

        init(" ") now_rec$, now_key$
         
        er_msg$(1%)  =  "Write error to Glass File"       
        er_msg$(10%) =  "Barcode not in Caelus"
        er_msg$(11%) =  "Caelus Status Issue"
        er_msg$(12%) =  "Z invalid status" 
        er_msg$(14%) =  "Hold Schedule Write Issue"
        er_msg$(15%) =  "Invalid Reason Code"
        er_msg$(16%) =  "Already a remake"
        er_msg$(18%) =  "Glass not scheduled"
        er_msg$(22%) =  "Audit File Write Error"
        er_msg$(26%) =  "Dept Not Valid"  
        er_msg$(99%) =  "Error Opening Files"

        gosub read_input
        
        goto exit_program

         
        REM *************************************************************~
            * Process input records from Remake Now.                    *~
            *************************************************************
        read_input
              read #1, key > now_key$, using L00001, now_rec$,  eod goto L01000
L00001:        FMT CH(125)
                goto L00500
                
        read_nxt_input
              read #1, key > now_key$, using L00001 , now_rec$, eod goto L01000
L00500:
              now_key$      = str(now_rec$,1%,60%)
              now_date$     = str(now_rec$,1%,10%)
              now_time$     = str(now_rec$,11%,10%)      
              now_barcode$  = str(now_rec$,21%,10%)
              now_reason_d$ = str(now_rec$,31%,30%)
              now_dept$     = str(now_rec$,61%,10%)   
              now_reason$   = str(now_rec$,71%,10%) 
              
              gosub glass_scan
              err%  = 0%
              goto read_nxt_input
L01000:
        return        
        
        REM *************************************************************~
            * Validate data sent and call remake updates                *~
            *************************************************************
        glass_scan                                   
            init (" ") rm_bar$, rm_reason$, xx$(), rm_dept_n$, dept_d$, ~
                       rm_status$

            gosub lookup_main_dept    
            if err% <> 0% then gosub error_report
            if err% <> 0% then return

            gosub check_glass
            if check% = 0% then gosub error_report
            if err% <> 0% then return
            
            gosub update_glass
        return
        
        REM *************************************************************~
            * Check glass record                                        *~
            *************************************************************
        check_glass                        /* Check Routine */

          check% = 0% : cnt% = 0%        /*    Note - Only (1) Glass */
          tempered% = 0%                      
          laminate% = 0%                     
          init(" ") rm_key$, sav_st$     /*    record per Barcode    */

          rm_len% = 0%
          rm_len% = len(now_barcode$)          

          if rm_len% < 11% then str(rm_key$,1%,9%) = now_barcode$          ~
                           else str(rm_key$,1%,12%) = now_barcode$
                                          
          gosub check_glass_file              
          if glass% <> 1% then goto L05980   
          cnt% = cnt% + 1%

          rm_len% = len(rm_bar$)
          if cnt% > 1% and rm_len% > 11% then goto L05980
          rm_key$ = str(rm_rec$(),22%,12%)   /* Glass Barcode & Rm No*/

          if rm_len% < 11% and str(rm_key$,1%,9%) <> rm_bar$ then goto L05980

          if rm_len% > 11% and str(rm_key$,1%,12%) <> rm_bar$ then goto L05980
                                               
            sav_st$ = str(rm_rec$(),13%,1%)

            gosub check_reason               /* Can Only Re-Make Comp*/
             if code% = 0% then goto L06150
            gosub check_specials             /* Spec/Temp */

            gosub check_dept_rmk            
REM             if code% = 0% then goto L06160 /* May not be planning dept */
            dept_d$ = str(desc$,1%,30%)      
            check% = 1%                        /* Valid Scan of Glass  */
        return

L05980:     if cnt% > 0% then goto L06040
            err% = 10% : goto L06170           /* Not on File/Invalid  */

L06040:     err% = 18%
            goto L06170                        /* Glass Not Scheduled  */
            
L06150:     err% = 15%                         /* Invalid Reason Code  */
            goto L06170
            
L06160:     err% = 26%

L06170:     check% = 0%
        return
        
        REM *************************************************************~
            * Find glass record for reset to remake                     *~
            *************************************************************
        check_glass_file                        
          glass% = 0%
          tempered% = 0%
          laminate% = 0%
          if rm_len% < 11% then                                            ~
             read #3,key > rm_key$, using L05850 , rm_rec$(), eod goto no_glass~
          else                                             ~
             read #3,key = rm_key$, using L05850 , rm_rec$(), eod goto no_glass
L05850:        FMT 4*CH(128)                       

               if str(rm_rec$(),22%,09%) <> now_barcode$ then goto no_glass
               glstype$ = str(rm_rec$(),335%,20%)
               if str(glstype$,1%,8%) = "TEMPERED" then tempered% = 1%
               if str(glstype$,9%,5%) = "LAMIN" then laminate% = 1%
          glass% = 1%
        no_glass
        return
        
        REM *************************************************************~
            * Update the glass record for remake                        *~
            *************************************************************
        update_glass                            /* (APCPLNGR) - File   */
          init(" ") dateout$
          call "TIME" (dateout$)
          read #3,hold,key = rm_key$, using L06260, rm_rec$(), eod goto L06440
L06260:        FMT 4*CH(128)                           

            delete #3
            set_flag% = 0%

            str(rm_rec$(),7%,6%)   = date    /* Glass Scan Date        */
            str(rm_rec$(),13%,1%)  = "0"     /* Re-Schedule as Re-Make */
            if tempered% = 1% then str(rm_rec$(),13%,1%)  = "9"  /* Status */
            if laminate% = 1% then str(rm_rec$(),13%,1%)  = "9"
            str(rm_rec$(),34%,2%)  = "79"    /* Re-Make Reason Code */

            rm_num% = 1%
            convert str(rm_rec$(),32%,2%) to rm_num%, data goto L06390
L06390:
            convert (rm_num% + 1%) to str(rm_rec$(),32%,2%), pic(00)
            str(rm_rec$(),44%,8%)  = time     /* Set Re-make           */
            str(rm_rec$(),52%,6%)  = date     /* Date/Time Stamp       */
            str(rm_rec$(),58%,3%)  = "NOW"    /* User id               */
            str(rm_rec$(),61%,4%)  = "0000"   /* Total Hrs/Mins        */
                                              /* Clock Starts          */
            str(rm_rec$(),249%,3%) = str(now_dept$, 1%, 3%) 
            str(rm_rec$(),356%,1%) = "1"  
            if tempered% = 1% or laminate% = 1% then gosub findHldsched 
            if err% = 14% then gosub error_report         

REM  L06420:     
            str(rm_rec$(),14%,8%) = dateout$  /* Time of Status Change */
            str(rm_rec$(),36%,6%) = date      /* Date of Status Change */
            str(rm_rec$(),42%,2%) = "01"      /* Scanning Shift Code   */
            str(rm_rec$(),252%,3%) = str(rm_rec$(),31%,3%)   /*REMAKE NO*/
            str(rm_rec$(),65%,1%) = "RMK"        
            str(rm_rec$(),65%,1%) = "01"

            put #3, using L06260, rm_rec$()
            write #3, eod goto L06430
               tt_unit% = tt_unit% + 1%          /* Calc Scanned Units */

            gosub update_glass_audit         

        return
L06430:     err% = 1%
            gosub error_report
            goto L06450

L06440:    err% = 10%
           gosub error_report
L06450: 
        return

        findHldsched
          updteCnt% = 0%
          gosub check_temp_stock
          gosub lookup_dt
          if dt_rec% <> 1% then return
          gosub lookupHldSched
          if hldsch% = 0% and updteCnt% = 0% then err% = 14%
          
        return

        REM *************************************************************~
            *  Check if stock order                                     *~           
            *************************************************************
        check_temp_stock
          if tempered% = 0% then return
          temp_stock% = 0%
          init(" ") readkey$, desc$, rm_part$, rm_subpart$

          rm_part$ = str(rm_rec$(),125%,25%)
          rm_subpart$ = str(rm_rec$(),255%,20%)             

          str(readkey$, 1%,9%)  = "TEMPSTOCK"
          str(readkey$,10%,3%) = str(rm_part$,1%,3%)
          str(readkey$,13%,2%) = str(rm_part$,5%,2%)
          str(readkey$,15%,4%) = str(rm_part$,13%,4%)
          str(readkey$,19%,3%) = str(rm_part$,17%,3%)

          read #2,key = readkey$, eod goto not_stock

             temp_stock% = 1%
             str(rm_rec$(),13%,1%)  = "0"
        not_stock
        return

        REM *************************************************************~
            *  Get the sales order, line number and item line nbr       *~           
            *************************************************************
        lookup_dt
           dt_rec% = 0%
           init(" ") dt_key$, dt_bar$, hldso$, hldln$, hldlnitem$
           str(dt_key$,1%,8%) = str(rm_rec$(),22%,8%)

           read #10, key 4% = dt_key$, using L07370, dt_key$, eod goto no_dt

L07370:            FMT POS(96), CH(8)
            if str(dt_key$,1%,8%) <> str(rm_rec$(),22%,8%) then goto no_dt

              get #10, using L07380, dt_bar$

L07380:       FMT POS(24), CH(18), POS(64), CH(2), POS(124), CH(9)

              hldso$      = str(dt_bar$,1%,8%)
              hldln$      = str(dt_bar$,9%,2%)
              hldlnitem$  = str(dt_bar$,11%,4%)
           dt_rec% = 1%
        no_dt
        return
 
        REM *************************************************************~
            *  Look for glass order and update remake order             *~           
            *************************************************************
        lookupHldSched                                   /* #8 HLDSCHED */
          hldsch% = 0%                /* Read look for Tempered or Lamin */
          init(" ") hldKey$, savHld$, hldType$, hldStatus$, hldrmkStatus$,  ~
                    hldrmkType$, hldLamn$
          str(hldKey$,1%,8%)  = hldso$
          str(hldKey$,9%,2%)  = hldln$
          str(hldKey$,11%,4%) = hldlnitem$
          savHld$ = hldKey$
hldSchedNext:
          read #8, key 2% > hldKey$, using HLDFMT, hldRec$, eod goto noSchdKey
HLDFMT:       FMT CH(256)

             hldKey$ = str(hldRec$,27%,16%)
             if str(hldKey$,1%,14%) <> str(savHld$,1%,14%) then goto noHldKey

             hldsch% = hldsch% + 1
             hldStatus$ = str(hldRec$,7%,1)
             hldType$   = str(hldRec$,8%,1)
             hldLamn$   = str(hldRec$,114%,1)

             if hldStatus$ = "0" then goto hldSchedNext /* 0 means never ordered*/
                                  /* Only 1 -> tempered and B -> laminate    */
             if hldType$ <> "1" and hldType$ <> "B" then goto hldSchedNext
             hldrmkStatus$ = "0"
             hldrmkType$   = "5"

             if hldType$ = "B" and (hldLamn$ = "L" and obs% = 0%)            ~
                                                       then goto hldSchedNext
             if hldType$   = "B" then hldrmkType$ = "D"

             gosub writeHldSched       /* Now check and write remake file */

             goto hldSchedNext         /* Loop look for other records */
        noHldKey
           if hldsch% = 0% then goto noSchdKey
        return
        
        noSchdKey
           err% = 17%
           gosub error_report
        return
 
        REM *************************************************************~
            *  Write the hold schedule remake file                      *~           
            *************************************************************
        writeHldSched                                    /* #9 - HLDSCRMK */
          init(" ") hldscrmkKey$, hldbar$, hldnum$, hldscrmkRec$
          hldbar$ = str(rm_rec$(),22%,9%)
          hldnum$ = str(rm_rec$(),31%,3%)

/* Key Sales Order, Ln, Ln_item, Glass Bar and Num; then status and type */
          str(hldscrmkKey$,1%,14%) = str(hldRec$,27%,14%)
          str(hldscrmkKey$,15%,9%) = hldbar$
          str(hldscrmkKey$,24%,3%) = hldnum$
          str(hldscrmkKey$,27%,1%) = hldrmkStatus$
          str(hldscrmkKey$,28%,1%) = hldrmkType$

          read #9, key 2% = hldscrmkKey$, eod goto noscrmkRec

/* the record should not already exit  */
           err% = 15%
           gosub error_report
           return
           
        noscrmkRec
          init(" ") hldtime$
          call "TIME" (hldtime$)
          str(hldscrmkRec$,1%,40%)   = str(hldRec$,1%,40%)
          str(hldscrmkRec$,41%,9%)   = hldbar$
          str(hldscrmkRec$,50%,3%)   = hldnum$
          str(hldscrmkRec$,53%,204%) = str(hldRec$,41%,204%)

          str(hldscrmkRec$,1%,6%)    = date
          str(hldscrmkRec$,7%,1%)    = hldrmkStatus$ /* Status */
          str(hldscrmkRec$,8%,1%)    = hldrmkType$   /* Type */
          str(hldscrmkRec$,16%,1%)   = hldrmkStatus$ /* Status */
          str(hldscrmkRec$,17%,1%)   = hldrmkType$   /* Type */
          str(hldscrmkRec$,53%,1%)   = hldrmkStatus$ /* Status */
          str(hldscrmkRec$,54%,1%)   = hldrmkType$   /* Type */
          str(hldscrmkRec$,86%,4%)   = "0001"        /* Qty    */
          str(hldscrmkRec$,92%,3%)   = "NOW"
          str(hldscrmkRec$,95%,6%)   = date
          str(hldscrmkRec$,101%,8%)  = hldtime$

          put #9, using HLDSCRMK1, hldscrmkRec$
HLDSCRMK1:     FMT CH(256)
          write #9, eod goto PUTERR
          updteCnt% = 1%
        return
PUTERR:
          err% = 22%
          gosub error_report 
        return        
        
         REM *************************************************************~
             *  Update glass update on remake                            *~           
             *************************************************************
        update_glass_audit                        
           set_flag% = 0%
           init(" ") rma_rec$
           str(rma_rec$,1%,6%)   = str(rm_rec$(),52%,6%)  /* Date     */
           str(rma_rec$,7%,9%)   = str(rm_rec$(),22%,9%)  /* Bar Code */
           str(rma_rec$,16%,3%)  = str(rm_rec$(),31%,3%)  /* Rmk No.  */
           str(rma_rec$,19%,8%)  = str(rm_rec$(),44%,8%)  /* Rmk Time */
           str(rma_rec$,27%,3%)  = str(rm_rec$(),58%,3%)  /* User Id  */
           str(rma_rec$,30%,4%)  = str(rm_rec$(),61%,4%)  /* Completed*/
           str(rma_rec$,34%,3%)  = str(rm_rec$(),249%,3%) /* Dept     */
           str(rma_rec$,37%,2%)  = str(rm_rec$(),34%,2%)  /* Reason Cd*/
           str(rma_rec$,39%,26%) = " "
                                                      
           write #5%, using L06510, rma_rec$, eod goto L06520
L06510:         FMT CH(64)

REM  GOSUB UPDATE_EWDPLNGT
        return
L06520:   err% = 22%
          gosub error_report
        return


        REM *************************************************************~
            * Validate the reason code supplied by the file             *~
            *************************************************************
        check_reason              
           table$ = "PLAN REMK"   
           init(" ") code$
           code$  = "79"  : rm_reason% = 99%  
           
           convert rm_reason$ to rm_reason%, data goto L11170
           
L11170:    gosub check_code                
           if rm_reason$ = "99" then code% = 0%
           if code% = 0% then goto L11220
              reason_d$ = desc$
        return
L11220:    err% = 15% : code% = 0% : check% = 0%  
        return      
                
        REM *************************************************************~
            * Check for special glass like tempered, laminate or obscure*~
            *************************************************************
        check_specials                           /* Tempered Glass and */

          rm_part$ = str(rm_rec$(),125%,25%)    /* Special Lighting   */
          rm_gls$  = str(rm_part$,5%,2%)        /* Glass Type         */
          rm_lit$  = str(rm_part$,7%,2%)        /* Liting             */
          if glstype$ <> " " then goto checkOBS
          table$ = "PLAN TEMP"
          code$  = rm_gls$                      /* Check Tempered Gl  */
          gosub check_code
           if code% = 1% then tempered% = 1%

          table$ = "PLAN LAMN"
          code$  = rm_gls$                      /* Check Tempered Gl  */
          gosub check_code
           if code% = 1% then laminate% = 1%

checkOBS:
          obs% = 0%
          table$ = "OBS GED"
          code$  = rm_gls$                      /* Check Tempered Gl  */
          gosub check_code
           if code% = 1% then obs% = 1%
        return

        REM *************************************************************~
            * Find the Planned Department in the remake request         *~
            *************************************************************
       check_dept_rmk                               
           table$ = "PLAN DEPT"
           code$  = rm_dept_n$
           gosub check_code

       return        

        REM *************************************************************~
            * Generic call to find Gencodes data                        *~
            *************************************************************               
       check_code
           code% = 0%
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = table$
           str(readkey$,10%,15%) = code$
           read #2, key = readkey$, using L12070, desc$, eod goto L12090
L12070:       FMT POS(25), CH(30)
           code% = 1%
L12090: return

        REM *************************************************************~
            * Check barcode in Glass file for status checks             *~
            *************************************************************
       lookup_main_dept                                                                 
           init(" ") rm_dept_n$, rm_status$, rm_key$
           rm_key$ = str(now_barcode$,1%,12%)
           rm_len% = 0%
           rm_len% = len(now_barcode$)

           if rm_len% < 12% then                                           ~
              read #3,key > rm_key$, using L20000, rm_status$, rm_key$,    ~
                                       rm_dept_n$, eod goto not_main       ~
           else                                                            ~
              read #3,key = rm_key$, using L20000 , rm_status$, rm_key$,   ~
                                     rm_dept_n$, eod goto not_main

L20000:         FMT POS(13), CH(01), POS(22), CH(12), POS(249), CH(03)

             if str(now_barcode$,1%,9%) <> str(rm_key$,1%,9%) then goto not_main
             
             rm_bar$ = str(rm_key$,1%,12%)
        return
        
        not_main
         err% = 10%    
        return

        REM *************************************************************~
            *  Error reporting                                          *~
            *************************************************************
        error_report
        
            init(" ") filler1$
            str(er_rec$,1%,6%)   = date                    /* er_date$      */          
            str(er_rec$,7%,8%)   = time                    /* er_time$      */
            convert err% to er_error$, pic(000)                             
            str(er_rec$,15%,3%)  = er_error$                                  
            str(er_rec$,18%,12%) = now_barcode$            /* er_barcode$   */
            str(er_rec$,30%,25%) = er_msg$(err%)           
            str(er_rec$,55%,10%) = now_date$               /* er_scan_date$ */
            str(er_rec$,65%,10%) = now_time$               /* er_scan_time$ */
            str(er_rec$,75%,30%) = now_reason_d$           /* er_reason_d$  */
            str(er_rec$,105%,3%) = str(now_dept$, 1%, 3%)  /* er_dept$      */
            str(er_rec$,108%,43%) = filler1$
            
           write #7, using L30000, er_rec$, eod goto L30099
               
L30000:         FMT CH(125)
        return
                    
L30099: 
        return
        
       
        REM *************************************************************~
            *  Layout for APCRMKER                                      *~
            *************************************************************
REM               er_date$       6      
REM               er_time$       8      
REM               er_error$      3     
REM               er_barcode$   12  
REM               er_msg$       25
REM               er_scan_date$ 10
REM               er_scan_time$ 10
REM               er_reason_d$  30 
REM               er_dept$       3      
REM               filler1$      18     

        REM *************************************************************~
            *  Exit program                                             *~
            *************************************************************
        open_error

            err% = 99%
            now_barcode$ = filename$
            gosub error_report 
            
            goto exit_program
        return
        
        REM *************************************************************~
            *  Exit program                                             *~
            *************************************************************
        exit_program
           close #1
        end