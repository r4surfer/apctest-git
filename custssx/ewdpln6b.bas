        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLN6B                             *~
            *  Creation Date     - 01/17/00                             *~
            *  Last Modified Date- 09/17/02                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Update Customer Marketing Data       *~
            *                      for Samples and Displays. Also for   *~
            *                      Literature and Apparel.              *~
            *                                                           *~
            *  PLAN HOWS      - Sample/Display/Literature/Apparel Codes *~
            *                 2 UPS Sample                              *~
            *                 3 UPS Display                             *~
            *                 4 Sample Our Truck                        *~
            *                 5 Display Our Truck                       *~
            *                 6 Sample Local Pick-up                    *~
            *                22 Sample repair                           *~
            *                                                           *~
            *                32 Literature                              *~
            *                33 Apparel                                 *~
            *                                                           *~
            *  Tables        - ELLISON06 - Marketing Digit 13 Codes     *~
            *                - ELLISON07 - Marketing Cross-Ref to Parent*~
            *                - APC WOOD  - 001 thru 080 Samples/Displays*~
            *                              with Units Per Manhour       *~
            *                                                           *~                 
            *-----------------------------------------------------------*~
            *                                                           *~
            * Subroutine - Called by (EWDPLN6B) from BCKUPDTE           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/20/00 ! Original - New Program                   ! RHH *~
            * 09/17/02 ! (EWD001) Fix for Special Shapes Grid Code! CMG *~
            *************************************************************

        sub "EWDPLN6B" (opt%,            /* 1=Add,8=Change,9=Delete    */~
                        lk_prv$,         /* Customer Private Label     */~ 
                        lk_cuscode$,     /* Customer Code              */~
                        lk_po$,          /* Customer P.O. Number       */~
                        lk_so$,          /* Customer Sales Order No    */~
                        lk_ln$,          /* Sales Order Line Item      */~
                        lk_hows$,        /* S.O. Howship Code          */~
                        lk_due$,         /* S.O. Due Date              */~
                        lk_qty%,         /* Line Item Quantity         */~
                        lk_price,        /* Line Item Price Unit       */~
                        lk_disc,         /* Line Item Discount Percent */~
                        lk_disc_or,      /* Oder Discount Percent      */~ 
                        lk_part$,        /* MFG Part Number            */~  
                        #3,              /* GENCODES Master Tables     */~
                        error%)          /* Return Code                */

        dim readkey$24, desc$30,         /* Gencodes lookup            */~
            filename$8,                  /* File Name for Open         */~
            parent$9,                    /* Parent or Bill to Account  */~
            lk_cuscode$9,                /* Customer Code              */~
            lk_po$16,                    /* Customer P.O. Number       */~
            lk_so$8,                     /* Customer Sales Order       */~
            lk_ln$3,                     /* Sales Order Line Item      */~
            lk_hows$2, dt_hows$2,        /* S.O. How Ship Code         */~
            lk_due$6,                    /* Sales Order Due Date       */~
            lk_part$25, dt_part$25,      /* MFG Part Number            */~
            lk_key$9,                    /* Cus Link Key               */~
            dt_key$11,                   /* Detail Key                 */~
            dt_inv$1,                    /* S.O. Invoiced (Y) or (N)   */~ 
            part$25                      /* Testing Part Number        */

        dim lk_prv$2,                    /* Private Label Code         */~
            dt_prv$2,                    /* Detail Private Label       */~  
            lk_samples%(80%),            /* Sample Units by Code       */~
            lk_samples(80%),             /* Sample Dollars by Code     */~
            lk_marketing%(40%),          /* Marketing Units by Code    */~
            lk_marketing(40%)            /* Marketing Dollars by Code  */

        dim f2%(3%),                     /* = 0 if the file is open    */~
            f1%(3%),                     /* = 1 if READ was successful */~
            fs%(3%)                      /*                            */
 
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21      
            apc$   = "(EWD)Update Marketing Data dor S/D/L/A      "
            pname$ = "EWDPLN6B - Rev: R7.00"

        REM *************************************************************
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CUSTLINK ! Additional Customer Sales & Marketing Dat*~
            * #2  ! CUSDETAL ! Customer Detail for Samples/Displays/Lit *~
            * #3  ! GENCODES ! Master Table File                        *~       
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTLINK",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    3, keylen =  9,                      ~
                        alt key  1, keypos =    1, keylen =  11

            select #2,  "CUSDETAL",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  28,                     ~
                        alt key  1, keypos =   35, keylen =  11,         ~
                            key  2, keypos =   29, keylen =  17

            if beenherebefore% = 1% then goto L10000 
               error% = 0%
               filename$ = "CUSTLINK" 
               call "EWDOPEN" (#1, filename$, err%)
               if err% <> 0% then gosub open_error

               filename$ = "CUSDETAL" 
               call "EWDOPEN" (#2, filename$, err%)
               if err% <> 0% then gosub open_error
               beenherebefore% = 1%

L10000:     mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            if error% <> 0% then goto exit_sub
            dt_inv$ = "N"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************

            gosub Load_parent                  /* Store in Program      */  
            gosub load_customer                /* Load In Customer Data */
            gosub lookup_detail
            if opt% = 9% then goto exit_sub    /* Delete Data Only      */
                                               /* Cutomer May or May Not*/
            gosub update_detail                /* be in Marketing Program*/
            goto exit_sub


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
    
        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************

L35000:     FMT POS(20), 80*BI(2), 80*PD(14,4), 40*BI(2),         ~
                40*PD(14,4), BI(2), PD(14,4), BI(2), PD(14,4) 

L35010:     FMT CH(24), CH(30)

L35020:     FMT POS(46), CH(2), CH(2), BI(2), 6*PD(14,4), CH(25)

L35030:     FMT CH(16), CH(9), CH(3), CH(6), CH(8), CH(3), CH(2), CH(2), ~
                BI(2), 6*PD(14,4), CH(25), CH(1), CH(3)
 
        REM *************************************************************~
            *           S P E C I A L   R O U T I N E S                 *~
            *************************************************************

        lookup_detail
            ss% = 0%                               /* Sample Bucket     */ 
            ll% = 0%                               /* Literature Bucket */
            dt_qty%  = 0%  : dt_price = 0.0 : dt_disc = 0.0 : dt_disc_or = 0.0 
            dt_gross = 0.0 : dt_disca = 0.0 : dt_net  = 0.0 

            init(" ") dt_key$
            str(dt_key$,1%,8%) = lk_so$            /* Customer Sales Ord*/
            str(dt_key$,9%,3%) = lk_ln$            /* S.O. Line Item    */
            read #2,hold,key 1% = dt_key$, using L35020, dt_hows$, dt_prv$,~
                               dt_qty%, dt_price, dt_disc, dt_disc_or,   ~
                               dt_gross, dt_disca, dt_net, dt_part$,     ~
                               eod goto LD3      /* New Sales Order    */ 
               delete #2
            part$ = dt_part$                     /* Set Part Number    */
            if program% = 0% then goto LD3       /* Not In Program     */
 
            if dt_hows$ = "32" or dt_hows$ = "33"                        ~
                                  then gosub calc_literature_bucket      ~
                                  else gosub calc_sample_bucket
 
            if ss% = 0% then goto LD1
               lk_samples%(ss%) = lk_samples%(ss%) - dt_qty%
               lk_samples(ss%)  = round(lk_samples(ss%)  - dt_disca, 2)
                                                 /* Totals            */
               lk_samples%(80%) = lk_samples%(80%) - dt_qty%
               lk_samples(80%)  = round(lk_samples(80%)  - dt_disca, 2)

                                                 /* Purchased Samples */
               if dt_net > .01 then lk_samples% = lk_samples% - dt_qty%
               if dt_net > .01 then lk_samples  = round(lk_samples  - dt_net, 2)

               if lk_samples%(ss%) < 0% then lk_samples%(ss%) = 0%
               if lk_samples(ss%) < 0.0 then lk_samples(ss%) = 0.0 

               if lk_samples%(80%) < 0% then lk_samples%(80%) = 0%
               if lk_samples(80%) < 0% then lk_samples(80%) = 0.0

               if lk_samples% < 0% then lk_samples% = 0%
               if lk_samples < 0.0 then lk_samples = 0.0 
 
               goto LD2

LD1:        if ll% = 0% then goto LD3
               lk_marketing%(ll%) = lk_marketing%(ll%) - dt_qty%
               lk_marketing(ll%)  = round(lk_marketing(ll%)  - dt_disca, 2)
                                                 /* Totals               */
               lk_marketing%(40%) = lk_marketing%(40%) - dt_qty%
               lk_marketing(40%)  = round(lk_marketing(40%)  - dt_disca, 2)
            
                                                 /* Purchased Literature */
               if dt_net > .01 then lk_marketing% = lk_marketing% - dt_qty%
               if dt_net > .01 then lk_marketing  = round(lk_marketing  - dt_net, 2)

               if lk_marketing%(ll%) < 0% then lk_marketing%(ll%) = 0%
               if lk_marketing(ll%) < 0.0 then lk_marketing(ll%) = 0.0
   
               if lk_marketing%(40%) < 0% then lk_marketing%(40%) = 0%
               if lk_marketing(40%) < 0.0 then lk_marketing(40%) = 0.0
   
               if lk_marketing% < 0% then lk_marketing% = 0%
               if lk_marketing < 0.0 then lk_marketing = 0.0
   
LD2:        gosub update_customer   
LD3:    return

        update_detail
            ss% = 0% : ll% = 0%
            dt_gross =  round(  lk_price * lk_qty%, 2)
                                        /* Line Item Discounted Price  */
            dt_damta  =  round( dt_gross * lk_disc * .01, 2)
            dt_net    =  round( dt_gross - dt_damta, 2)
                                        /* S.O. Discount Percent       */
            dt_damto  =  round( dt_net * lk_disc_or * .01, 2)
            dt_net    =  round( dt_net - dt_damto, 2)
                                   /* Line Item + S.O. Discount Amount */
            dt_disca  =  round( dt_damta + dt_damto, 2)
                                   /* Special Note - The amount given  */
                                   /*    away to the customer is the   */
                                   /* total discount amount.           */
            put #2, using L35030, lk_po$,        /* P.O. Number        */~
                                  lk_cuscode$,   /* Customer Code      */~
                                  lk_ln$,        /* S.O. Line Item     */~
                                  lk_due$,       /* S.O. Due Date      */~
                                  lk_so$,        /* Customer S.O.      */~
                                  lk_ln$,        /* S.O. Line Item     */~
                                  lk_hows$,      /* S.O. Howship       */~
                                  lk_prv$,       /* Private Label      */~
                                  lk_qty%,       /* Line Item Quantity */~
                                  lk_price,      /* Line Item Price Unt*/~
                                  lk_disc,       /* Line Item Discount%*/~
                                  lk_disc_or,    /* S.O. Discount %    */~
                                  dt_gross,      /* Gross amount       */~
                                  dt_disca,      /* Discount Amount    */~
                                  dt_net,        /* Net amount         */~
                                  lk_part$,      /* MFG Part No.       */~
                                  dt_inv$,       /* Invoiced (Y) or (N)*/~
                                  " "            /* Filler Area        */

            write #2,eod goto L_ERR
            if program% = 0% then goto UD3       /* Not In Program     */

            part$ = lk_part$                     /* Set Part Number    */
            if lk_hows$ = "32" or lk_hows$ = "33"                        ~
                                  then gosub calc_literature_bucket      ~
                                  else gosub calc_sample_bucket
 
            if ss% = 0% then goto UD1
               lk_samples%(ss%) = lk_samples%(ss%) + lk_qty%
               lk_samples(ss%)  = round(lk_samples(ss%)  + dt_disca, 2)
                                                 /* Totals            */
               lk_samples%(80%) = lk_samples%(80%) + lk_qty%
               lk_samples(80%)  = round(lk_samples(80%)  + dt_disca, 2)

                                                 /* Purchased Samples */
               if dt_net > .01 then lk_samples% = lk_samples% + lk_qty%
               if dt_net > .01 then lk_samples  = round(lk_samples  + dt_net, 2)
               goto UD2

UD1:        if ll% = 0% then goto UD3
               lk_marketing%(ll%) = lk_marketing%(ll%) + lk_qty%
               lk_marketing(ll%)  = round(lk_marketing(ll%)  + dt_disca, 2)
                                                 /* Totals              */
               lk_marketing%(40%) = lk_marketing%(40%) + lk_qty%
               lk_marketing(40%)  = round(lk_marketing(40%)  + dt_disca, 2)

                                                 /* Purchased Literature */
               if dt_net > .01 then lk_marketing% = lk_marketing% + lk_qty%
               if dt_net > .01 then lk_marketing  = round(lk_marketing  + dt_net, 2)

UD2:        gosub update_customer   
UD3:    return
L_ERR:      error% = 1%
        goto exit_sub

        load_parent
            program% = 0%
            init(" ") readkey$, desc$, parent$
            str(readkey$,1%,9%)   = "ELLISON07"
            str(readkey$,10%,15%) = lk_cuscode$
            read #3,key = readkey$, using L35010, readkey$, desc$,  ~
                                                        eod goto L_PAR
            parent$ = str(desc$,1%,9%)
            program% = 1%                       /* On the Program */
L_PAR:  return
  
        load_customer
            mat lk_samples%   = zer
            mat lk_samples    = zer
            mat lk_marketing% = zer
            mat lk_marketing  = zer
            lk_samples%   = 0% : lk_samples   = 0.0
            lk_marketing% = 0% : lk_marketing = 0.0
            if program% = 0% then goto L_CUS

            init(" ") lk_key$
            lk_key$ = parent$
            read #1,key = lk_key$, eod goto L_CUS
                get #1, using L35000,lk_samples%(), lk_samples(),      ~
                                     lk_marketing%(), lk_marketing(),  ~
                                     lk_samples%, lk_samples,          ~
                                     lk_marketing%, lk_marketing
L_CUS:  return        

        update_customer
            if program% = 0% then goto L_CUSU     /* Not In Program  */
            init(" ") lk_key$
            lk_key$ = parent$
            read #1,hold,key = lk_key$, eod goto L_CUSU
                put #1, using L35000,lk_samples%(), lk_samples(),      ~
                                     lk_marketing%(), lk_marketing(),  ~
                                     lk_samples%, lk_samples,          ~
                                     lk_marketing%, lk_marketing
                rewrite #1
L_CUSU: return        
 
        calc_sample_bucket
            ss% = 0%
            if len(part$) < 20 then goto LS2       /* Quick Test      */
            convert str(part$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1 /* Not Samp/Disp   */
                                                   /*   (EWD001)      */
            if str(part$,7%,2%) > "99" then goto LS1

        return                                     /* Code Found      */
LS1:        convert str(part$,23%,3%) to ss%, data goto LS2

            if ss% < 1% or ss% > 80% then goto LS2
                                                   /* Code Found      */ 
        return
LS2:        ss% = 0%
        return  

        calc_literature_bucket
            ll% = 0%
            init(" ") readkey$, desc$
            if str(part$,1%,3%) <> "003" then goto LL1  /* Quick Test */
            str(readkey$,1%,9%)   = "ELLISON06"
            str(readkey$,10%,15%) = str(part$,13%,1%)
            read #3,key = readkey$, using L35010, readkey$, desc$,  ~
                                                        eod goto LL1
            convert str(desc$,1%,2%) to ll%, data goto LL1

LL1:    return
  
        open_error
            error% = 2%
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_sub

        end

