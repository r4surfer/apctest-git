        REM *************************************************************~
            *  Called By         - (EWDPLN58),(EWDPLN60),(EWDPLN64)     *~
            *                                                           *~
            *  Program Name      - EWDPLA58 - Subroutine                *~  
            *  Creation Date     - 08/20/98                             *~
            *  Last Modified Date- 06/02/2014                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Display Detail Infor for a given     *~
            *                      Customer, Sales Order, Line Item     *~
            *                                                           *~
            *  Subroutines Used  - (EWDGLSSB)                           *~
            *                                                           *~ 
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/20/98 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/10/98 ! (EWD001) Private Label Modes             ! RHH *~
            * 12/09/98 ! (EWD002) Mod to put Load No. on Display  ! RHH *~
            * 01/22/99 ! (EWD003)Mod to change Dimension of gdd$()! RHH *~
            *          !   to (12), and also add Rack Look-up     ! RHH *~  
            * 03/14/05 ! (AWD004) Mods for AWDPLN60 Information   ! CMG *~
            * 10/28/08 ! (AWD005) mods for sash stop              ! CMG *~
            *04/30/2010! (AWD006) add subpart for ewdglssb call   ! CMG *~
            *04/30/2014! (AWD007) mod to check for casement model ! CMG *~
            *06/02/2013! (AWD008) Op shapes dim fields for CUTCC  ! CMG *~
            *************************************************************

        sub "EWDPLA58" (switch%,         /* 0%=Info Only, 1%=Info+Glass*/~
                                         /* 2%=AWDPLN60                */~
                        or_so$,          /* Sales Order Number         */~
                        or_ln$,          /* Sales order Line Item      */~
                        rk_barcode$,     /* Rack/Glass Barcode (EWD003)*/~
                        rk_seq$,         /* Production Seq No. (EWD003)*/~
                        wgt_top$,        /* Weight of top              */~
                        wgt_bot$,        /* Weight of bottom           */~
                        hgt_top$,        /* Sash Height of Top         */~
                        hgt_bot$,        /* Sash Height of Bottom      */~   
                        loc_top$,        /* Location of top balance    */~
                        loc_bot$,        /* Location of bot balance    */~
/*(AWD005)*/            sash_stop_bot$,  /* Sash Stop Position         */~
                        subpart$,        /* Subpart Number    (AWD006) */~
                        dim1es,          /* (AWD008) leg height        */~
                        dim2es,          /* (AWD008) leg height        */~
                        dim3es,          /* (AWD008) leg height        */~
                        #1,              /* (APCPLNOR) Planning Header */~
                        #2,              /* (APCPLNSC) Planning Ln Item*/~
                        #3,              /* (CUSTOMER) Customer Master */~
                        #4,              /* (GENCODES) Master Tables   */~
                        #5,              /* (AMTBOMIF) Master Validity */~
                        #6,              /* (TXTFILE ) Sales Text File */~
                        #7,              /* (AMTBOMCD) Master Equation */~
                        #8,              /* (BCKLINES) (EWD001)        */~
                        #9,              /* (EWDPLNRK) (EWD003)        */~
                        err% )           /* 0% = Ok, Non-Zero = Error  */

        dim                                                              ~
            title$52,                    /* Analysis Title and Time    */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            or_so$8,                     /* Customer Sales Order       */~
            or_cuscode$9,                /* Customer Code              */~
            or_ln$2,                     /* S.O. Line Item             */~
            or_route$5,                  /* Customer Route Code        */~
            or_due$8, or_due_date$8,     /* S.O. Due Date              */~
            or_cutoff$2,                 /* S.O. Line item No.         */~
            or_load$5,                   /* Planned Load No.  (EWD002) */~
            sc_st$2, status$15,          /* Current Sataus Disp(EWD002)*/~
            sc_part$25,                  /* MFG Part Number            */~
            sc_qty$4,                    /* Line Item Quantity         */~
            sc_text$4,                   /* Line item Text Id          */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Long Form             */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code            */~
            s_so$8,                      /* Sales Order       (EWD001) */~
            s_ln$3,                      /*** Line Item (3)   (EWD001) */~
            s_prv$30,                    /* Private Label Name(EWD001) */~
            s_1$2,                       /* Private Label Code(EWD001) */~
            code$3, tab$(10%)9,          /* Tables for Lookup          */~
            sel$(4%)20,                  /* Display Text               */~
            textid$4,                    /* Text Id's                  */~
            txtid$4, tx$(30%)70,         /* Text page                  */~
            text_key$11, sav_txt$9,      /* Text Lookup Key            */~
            vf$(5%)20,                   /* Customer Variable Fields   */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            errormsg$79,                 /* Error message              */~
            cursor%(2%),                 /* Cursor location for edit   */~
            i$(24%)80,                   /* Screen Image               */~
            dsp_msg$79,                  /* Screen Display Message     */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32                    /* PF Key Hex Values          */

         dim ctt$(10%,3%)9, dept$3,       /* 1-5=Top, 6-10=Bot          */~
            gdd$(12%)10,                 /* 1-6=Top, 7-12=Bot          */~
            ajj$(10%,2%)6,               /* 1-5=Top, 6-10=Bot          */~
            dcc$(10%,2%)8,               /* 1-5=Top, 6-10=Bot          */~
            wd$7,                        /* Actual Width               */~
            ht$6                         /* Actual Height              */

                                         /* (EWD003) - Begin           */          
        dim rk_barcode$9,                /* Rack Barcode = Glass       */~
            rk_dte$6, rk_date$10,        /* Date Assigned              */~
            rk_number$4,                 /* Rack Number                */~
            rk_loc$1,                    /* Location 0=Top, 1=Bot      */~       
            rk_seq$5,                    /* Seq Number                 */~
            t1$(5%)                      /* Display                    */
                                         /* (EWD003) - End             */ 

       dim tt$(20%)79                   /* Detail Display             */

       dim                              /*      AWD004                */~
            wgt_top$14,                 /* Weight of top              */~
            wgt_bot$14,                 /* Weight of bottom           */~
            hgt_top$14,                 /* Sash Height of Top         */~
            hgt_bot$14,                 /* Sash Height of Bottom      */~
            loc_top$14,                 /* Sash Location of Top       */~
            loc_bot$14,                 /* Sash Location of Bottom    */~
            sash_stop_bot$14,           /* Sash Stop Location  (AWD005)*/~
            calc$9,                     /* Generic Calc Value         */~
            sze$30,                     /* Save Eights                */~
            sz$100                      /* Save Sixteenths            */
       
       dim subpart$20                   /* (AWD006)   Subpart         */ 


                                                      /* (AWD004) - BEG */
        REM - NEAREST 8TH INCH
           sze$ = "1/81/43/81/25/83/47/8         "

        REM - NEAREST 16TH OF AN INCH
           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16   "

                                                      /* (AWD004) - END */


        tab$(1%) = "COLOR    " : tab$(2%) = "GLASS    "
        tab$(3%) = "LITING   " : tab$(4%) = "PLAN STAT"   /* (EWD002)  */ 
        tab$(5%) = "GLASS05  "

        init (hex(ff)) textid$
        x% = 0% : u3% = 0% : er% = 0%
        call "TXTFUTIL" (#6, x%, "INTL", textid$)

                                         /* (APCPLNOR) Lookup Customer */        
        read #1,key 4% = or_so$, using L00100, or_due$, or_route$,       ~
                                    or_cuscode$, or_load$, eod goto L00300
L00100:        FMT CH(8), POS(11), CH(5), POS(27), CH(9), POS(94), CH(5)
                                                   /* (EWD002) or_load$ */
        str(sc_key$,1%,8%) = or_so$
        str(sc_key$,9%,2%) = or_ln$
        read #2,key = sc_key$, using L00200, sc_part$, sc_tqty%, sc_text$, ~
                                             sc_st$, eod goto L00300    
L00200:    FMT POS(34), CH(25), POS(68), BI(2), POS(100), CH(4), POS(110), ~
               CH(2) 
        
        convert sc_tqty% to sc_qty$, pic(0000)

        gosub lookup_cust                /* fine the Cut Off Day       */
        gosub lookup_status              /* (EWD002) Status Display    */
        gosub lookup_rack                /* (EWD003)                   */

        gosub display_detail
        goto exit_sub

L00300: errormsg$ = "(Error) - Unable to Lookup [ "&or_so$&" ]"
        gosub error_prompt
        goto exit_sub
         
        REM *************************************************************~
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_detail
  
L01000:     gosub set_pf1   
            accept                                                       ~
               at (01,02), fac(hex(84)), sel$(1%)               , ch(16),~
               at (01,33), fac(hex(94)), status$                , ch(15),~ 
               at (01,63), fac(hex(84)), sel$(3%)               , ch(16),~
               at (02,02), fac(hex(84)), sel$(2%)               , ch(16),~
               at (02,63), fac(hex(84)), sel$(4%)               , ch(16),~ 
               at (03,15), fac(hex(a4)), title$                 , ch(52),~
                                                                         ~
               at (05,13), fac(hex(84)), tt$(1%)                , ch(55),~
               at (06,13), fac(hex(84)), tt$(2%)                , ch(55),~
               at (07,13), fac(hex(84)), tt$(3%)                , ch(55),~
               at (08,13), fac(hex(84)), tt$(4%)                , ch(55),~
               at (09,13), fac(hex(84)), tt$(5%)                , ch(55),~
               at (10,13), fac(hex(84)), tt$(6%)                , ch(55),~
               at (11,13), fac(hex(84)), tt$(7%)                , ch(55),~
               at (12,13), fac(hex(84)), tt$(8%)                , ch(55),~
               at (13,13), fac(hex(84)), tt$(9%)                , ch(55),~
                                                                         ~
               at (15,14), fac(hex(84)), tt$(10%)               , ch(64),~
                                                                         ~
               at (16,02), fac(hex(84)), t1$(1%)                , ch(11),~
               at (16,14), fac(hex(84)), tt$(11%)               , ch(64),~
                                                                         ~
               at (17,02), fac(hex(84)), t1$(2%)                , ch(11),~
               at (17,14), fac(hex(84)), tt$(12%)               , ch(64),~
                                                                         ~
               at (18,02), fac(hex(84)), t1$(3%)                , ch(11),~
               at (18,14), fac(hex(84)), tt$(13%)               , ch(64),~
                                                                         ~
               at (19,02), fac(hex(84)), t1$(4%)                , ch(11),~
               at (19,14), fac(hex(84)), tt$(14%)               , ch(64),~
                                                                         ~
               at (20,02), fac(hex(84)), t1$(5%)                , ch(11),~
               at (20,14), fac(hex(84)), tt$(15%)               , ch(64),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L01010
                  call "PRNTSCRN"
                  goto L01000

L01010:        if keyhit% <> 0% then goto L01000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return 

        set_pf1
            init(" ") tt$(), dsp_msg$
            dsp_msg$ = "Press <Return> To Continue?"     /* (EWD002)    */
            str(dsp_msg$,41%,18%) = "Load No. = (" & or_load$ & ")"
            sel$(1%) = "Date: xxxxxxxxxx"
            sel$(2%) = "Time: xxxxxxxx  "
            sel$(3%) = "S.O.: xxxxxxxx  "
            sel$(4%) = "Cust: xxxxxxxxx "    
            x$ = date
            call "DATFMTC" (x$)
            str(sel$(1%),7%,10%) = x$
            init(" ") x$
            call "TIME" (x$)
            str(sel$(2%),7%,8%) = x$
            str(sel$(3%),7%,8%) = or_so$
            str(sel$(4%),7%,9%) = or_cuscode$  

            title$ = "(Display) Customer Sales Order Line Item Information"

          tt$(1%)="Customer: xxxxxxxxx    Route Code: xxxxx    CutOff: xx "
          tt$(2%)="Sales Order: xxxxxxxx  Line It: xx  Due Date: xxxxxxxx "
          tt$(3%)="Part Number: xxxxxxxxxxxxxxxxxxxxxxxxx Color: xxxxxxxx "
          tt$(4%)="Desc: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxxxxxx "
          tt$(5%)="T1: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx "
          tt$(6%)="T2: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxx "
          tt$(7%)="T3: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxx "
          tt$(8%)="Glass: XXXXXXXXXXXXXXXXXXXX  Grid: XXXXXXXXXXXXXXXXXXX "
          tt$(9%)="Window Width: xxxxxxx   Height: xxxxxx  Quantity: xxxx "
          tt$(10%) =                                                       ~
          "Top: Width     Height    Clmr    Bot: Width     Height    Clmr  " 
          tt$(11%) =                                                       ~
          "  xxxxxxxxx x xxxxxxxx xxxxxxxxx !xxxxxxxxx x xxxxxxxx xxxxxxxxx"
          tt$(12%) =                                                       ~
          "  xxxxxxxxx x xxxxxxxx xxxxxxxxx !xxxxxxxxx x xxxxxxxx xxxxxxxxx"
          tt$(13%) =                                                       ~
          "  xxxxxxxxx x xxxxxxxx xxxxxxxxx !xxxxxxxxx x xxxxxxxx xxxxxxxxx"
          tt$(14%) =                                                       ~
          "  xxxxxxxxx x xxxxxxxx xxxxxxxxx !xxxxxxxxx x xxxxxxxx xxxxxxxxx"
          tt$(15%) =                                                       ~
          "  xxxxxxxxx x xxxxxxxx xxxxxxxxx !xxxxxxxxx x xxxxxxxx xxxxxxxxx"
                                                 /* (EWD003) - Begin     */
          t1$(1%) = "Date: MM DD"
          t1$(2%) = "Rack: XXXX "
          t1$(3%) = "Loc.:      "
          t1$(4%) = "Slot: XX   "
          t1$(5%) = "Seq.: xxxxx"

          str(t1$(1%),7%,3%)  = str(rk_date$,1%,2%) 
          str(t1$(1%),10%,2%) = str(rk_date$,4%,2%)
          if len(rk_barcode$) > 5 then str(t1$(1%),9%,1%) = "/" 
          str(t1$(2%),7%,4%)  = rk_number$
          if rk_loc$ = "0" then str(t1$(3%),7%,3%) = "Top"
          if rk_loc$ = "1" then str(t1$(3%),7%,3%) = "Bot"
          str(t1$(4%),7%,2%)  = rk_slot$
          str(t1$(5%),7%,5%)  = rk_seq$
                                                  /* (EWD003) - End       */ 

              or_due_date$ = or_due$
              call "DATEFMT" (or_due_date$)

              str(tt$(1%),11%,9%) = or_cuscode$
              str(tt$(1%),36%,5%) = or_route$
              str(tt$(1%),53%,2%) = or_cutoff$

              str(tt$(2%),14%,8%) = or_so$
              str(tt$(2%),33%,2%) = or_ln$
              str(tt$(2%),47%,8%) = or_due_date$
                                                /* Lookup color        */ 
              str(tt$(3%),14%,25%) = sc_part$
              init(" ") code$ : tab% = 1%
              code$ = str(sc_part$,4%,1%)
              gosub check_code
              p% = pos(desc$ = "-")
              if p% = 0% then p% = 4%  
              str(tt$(3%),47%,8%) = str(desc$,p%+2%, 8%)
                                                /* apc_prt = Long Desc */
              call "APCDESCR" (sc_part$, apc_scr$, apc_prt$, apc_sze$,   ~
                                                             #5, err% )
                                                /* (EWD001) - Begin    */
              s_23% = 0%                        /* Find Private Label  */
              s_23m$ = str(sc_part$,1%,3%)
              s_so$  = or_so$                   /*                     */
              s_ln$  = or_ln$                   /*                     */
              init(" ") s_prv$, s_1$, s_23$ 
              prv% = 1%                             /* Use BCKLINES    */
              call "APCPRZSB" (prv%, s_1$, or_cuscode$, s_23m$, s_so$,   ~
                                   s_ln$, s_prv$, s_23$, s_23%,          ~
                                   #3, #4, #8, #8, x_er% )
              if x_er% <> 0% then L01020
              if len(sc_part$) < 18% then L01020
                  str(apc_prt$,1%,8%)   = s_23$
                                                /* MFG Description     */
L01020:       str(tt$(4%),7%,48%) = str(apc_prt$,1%,48%)
                                                /* (EWD001) - End      */    
                                                /* find Text           */
              txtid$ = sc_text$                 /* Three (3) Lines     */
              gosub lookup_text
              str(tt$(5%),5%,50%) = str(tx$(1%),1%,50%)
              str(tt$(6%),5%,50%) = str(tx$(2%),1%,50%)
              str(tt$(7%),5%,50%) = str(tx$(3%),1%,50%)
                                                /* Find Glass          */
              init(" ") code$ : tab% = 2%
              code$ = str(sc_part$,5%,2%)
              gosub check_code
              p% = pos(desc$ = "-")
              if p% = 0% then p% = 4%  
              str(tt$(8%),8%,20%) = str(desc$,p%+2%, 20%)
                                                /* Find Liting         */ 
              init(" ") code$ : tab% = 3%
              code$ = str(sc_part$,7%,2%)
              gosub check_code
              p% = pos(desc$ = "-")
              if p% = 0% then p% = 4%  
              str(tt$(8%),36,20%) = str(desc$,p%+2%, 20%)
                                                /* Window Width/Height */
              str(tt$(9%),15%,7%) = str(apc_sze$,1%,7%)
              str(tt$(9%),33%,6%) = str(apc_sze$,11%,6%)
                                                /* Line Item Qty       */
              str(tt$(9%),51%,4%) = sc_qty$

              if switch% = 1% then goto L01030
              if switch% = 2% then goto L01030
                 init(" ") tt$(10%), tt$(11%), tt$(12%), tt$(13%),       ~
                           tt$(14%), tt$(15%)
                  goto L01040
                            
L01030:       gosub calc_glass_size          /* Glass Calculations  */
                                                
              str(tt$(11%),3%,9%)  = ctt$(1%,1%)      /* 1st Top     */
              str(tt$(11%),15%,8%) = ctt$(1%,2%)
              str(tt$(11%),24%,9%) = ctt$(1%,3%)

              str(tt$(11%),35%,9%) = ctt$(6%,1%)      /* 1st Bot     */
              str(tt$(11%),47%,8%) = ctt$(6%,2%)
              str(tt$(11%),56%,9%) = ctt$(6%,3%)

              if switch% = 2% then goto L01035         /* (AWD004)  */
  
              str(tt$(12%),3%,9%)  = ctt$(2%,1%)      /* 2nd Top     */
              str(tt$(12%),15%,8%) = ctt$(2%,2%)
              str(tt$(12%),24%,9%) = ctt$(2%,3%)

              str(tt$(12%),35%,9%) = ctt$(7%,1%)      /* 2nd Bot     */
              str(tt$(12%),47%,8%) = ctt$(7%,2%)
              str(tt$(12%),56%,9%) = ctt$(7%,3%)
  
              str(tt$(13%),3%,9%)  = ctt$(3%,1%)      /* 3rd Top     */
              str(tt$(13%),15%,8%) = ctt$(3%,2%)
              str(tt$(13%),24%,9%) = ctt$(3%,3%)

              str(tt$(13%),35%,9%) = ctt$(8%,1%)      /* 3rd Bot     */
              str(tt$(13%),47%,8%) = ctt$(8%,2%)
              str(tt$(13%),56%,9%) = ctt$(8%,3%)
  
              str(tt$(14%),3%,9%)  = ctt$(4%,1%)      /* 4th Top     */
              str(tt$(14%),15%,8%) = ctt$(4%,2%)
              str(tt$(14%),24%,9%) = ctt$(4%,3%)

              str(tt$(14%),35%,9%) = ctt$(9%,1%)      /* 4th Bot     */
              str(tt$(14%),47%,8%) = ctt$(9%,2%)
              str(tt$(14%),56%,9%) = ctt$(9%,3%)
  
              str(tt$(15%),3%,9%)  = ctt$(5%,1%)      /* 5th Top     */
              str(tt$(15%),15%,8%) = ctt$(5%,2%)
              str(tt$(15%),24%,9%) = ctt$(5%,3%)

              str(tt$(15%),35%,9%) = ctt$(10,1%)      /* 5th Bot     */
              str(tt$(15%),47%,8%) = ctt$(10,2%)
              str(tt$(15%),56%,9%) = ctt$(10,3%)
                         goto L01040
L01035:       

REM              convert wgt_top$ to calc, data goto wgt_bot
REM              gosub con_fract
REM              wgt_top$ = calc$
REM wgt_bot
REM              convert wgt_bot$ to calc, data goto hgt_top
REM              gosub con_fract
REM              wgt_bot$ = calc$
REM hgt_top

              convert hgt_top$ to calc, data goto hgt_bot
              gosub con_fract
              hgt_top$ = calc$
hgt_bot:

              convert hgt_bot$ to calc, data goto loc_top
              gosub con_fract
              hgt_bot$ = calc$
loc_top:

              convert loc_top$ to calc, data goto loc_bot
              gosub con_fract
              loc_top$ = calc$
loc_bot:

              convert loc_bot$ to calc, data goto sash_stop
              gosub con_fract
              loc_bot$ = calc$

/* (AWD005) - begin */
sash_stop:
              convert sash_stop_bot$ to calc, data goto dec_finished 
              gosub con_fract
              sash_stop_bot$ = calc$
/* (AWD005) - end  */

dec_finished:


              init(" ") t1$()
              str(tt$(12%),3%,14%)  = wgt_top$        /* weight of sash */
              str(tt$(12%),17%,10%) = "WEIGHT"       /* weight of sash */
              str(tt$(13%),3%,14%)  = hgt_top$        /* Height of sash */
              str(tt$(13%),17%,10%) = "HEIGHT"       /* Height of sash */

              str(tt$(14%),3%,14%)  = loc_top$        /* Location of sash */
              str(tt$(14%),17%,10%) = "LOCATION"       /* Location of sash */

              str(tt$(12%),35%,14%) = wgt_bot$       /* weight of sash */
              str(tt$(12%),49%,10%) = "WEIGHT"       /* weight of sash */
              str(tt$(13%),35%,14%) = hgt_bot$       /* Height of sash */
              str(tt$(13%),49%,10%) = "HEIGHT"       /* Height of sash */

              str(tt$(14%),35%,14%)  = loc_bot$        /* Location of sash */
              str(tt$(14%),49%,10%) = "LOCATION"       /* Location of sash */

/* (AWD005) - begin */
              str(tt$(15%),35%,14%)  = sash_stop_bot$  /* Sash Stop Location */
              str(tt$(15%),49%,10%) = "SASH STP"       /* sash Stop          */
/* (AWD005) - end   */
  
L01040:     pf$(1) = "                                        " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       " 
            pfkeys$ = hex(ffffffffffffffffffffffffffff0f1000)
            return

        lookup_cust
           init(" ") readkey$, desc$, or_cutoff$
           read #3,key = or_cuscode$, eod goto L01120
              get #3, using L01100   , vf$()

L01100:        FMT POS(820), 5*CH(20)
           or_cutoff$ = str(vf$(4%),1%,2%)     /* Cust Delivery Code   */

           str(readkey$,1%,9%)   = "PLAN CUTO"
           str(readkey$,10%,15%) = or_cutoff$
           read #4,key = readkey$, using L01110, desc$, eod goto L01120
L01110:       FMT POS(25), CH(1)
           or_cutoff$ = "0" & desc$            /* cut off Day 1 thru 7 */
L01120: return

        lookup_status
            init(" ") status$, code$
            tab% = 4%  
            code$ = sc_st$
            gosub check_code
            if code% = 0% then return
               p% = pos(desc$ = "-")
               status$ = sc_st$ & "=" & str(desc$,1%, p%-1%)
        return

        check_code                         /* Lookup Code in (GENCODES) */ 
            code% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = tab$(tab%)
            str(readkey$,10%,15%) = code$
            read #4,key = readkey$, using L02000, desc$,                ~
                                                eod goto check_code_done
L02000:        FMT POS(25), CH(30)
            code% = 1%
        check_code_done
        return

        deffn'099(textid$)                   /* Verify Text Id         */ 
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_key$, sav_txt$, tx$()
            gosub'099(txtid$)
            if txt% = 0% then return

            text_key$ = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = txtid$
            str(text_key$,9%,1%) = "1"
            sav_txt$ = text_key$
            for jj% = 1% to 25% step 3%
                read #6,key > text_key$, using L03000 , text_key$,         ~
                                                      eod goto L03020
L03000:            FMT CH(11)
                if sav_txt$ <> str(text_key$,1%,9%) then return
                   get #6, using L03010 , tx$(jj%), tx$(jj% + 1%),         ~
                                                  tx$(jj% + 2%)
L03010:          FMT POS(64), 3*CH(70)
            next jj%
L03020: return
    

        calc_glass_size                  /* (New) Glass Calculation    */ 
        opts% = 0%                       /*       Subroutine.          */
        dept$ = "000"
        g_cnt% = 0% 
        ct% = 0%
/* (AWD007) */
        gosub check_casement_model              /* (AWD006) */
        if code% = 1% then dept$ = "008"
/* (\AWD007) */        
REM        IF STR(SC_PART$,1%,1%) = "8" THEN DEPT$ = "008"      
         
        spType% = 0%                                 /* (AWD006) */
        
        call "EWDGLSSB" (opts%,          /* Options 0% = No Calc       */~
                        sc_part$,        /* MFG Part Number            */~
                        subpart$,        /* Subpart (AWD006)           */~
                        dim1es,          /* (AWD008)                   */~
                        dim2es,          /* (AWD008)                   */~
                        dim3es,          /* (AWD008)                   */~
                        dept$,           /* Department Code            */~
                        ct%,             /* Glass Piece Count          */~
                        ctt$(),          /* 1-5 = Top, 6-10 = Bot      */~
                        gdd$(),          /* 1-6 = Top, 7-12 = Bot      */~
                        ajj$(),          /* Window Adjustment (GED) Top*/~
                        dcc$(),          /* Decimal 1-5=Top, 6-10=Bot  */~
                        wd$,             /* Window width Eights        */~
                        ht$,             /* window Height Eights       */~
                        g_cnt%,          /* Glass Piece Cut Count      */~
                        spType%,         /* IG Type (AWD006)           */~
                        #4,              /* (GENCODES) Master Tables   */~
                        #7,              /* (AMTBOMCD) Equations       */~
                        er% )            /* 0%=Ok, Non-Zero = Error    */

        return
                                                /* (EWD003) - Begin   */
        lookup_rack
           init(" ") rk_date$, rk_dte$, rk_number$, rk_slot$
           read #9,key = rk_barcode$, using L04000, rk_dte$, rk_number$,~
                             rk_loc$, rk_slot$, rk_seq$, eod goto L04010
L04000:       FMT POS(10), CH(6), CH(4), CH(1), CH(2), XX(1), CH(5)
        rk_date$ = rk_dte$
        call "DATFMTC" (rk_date$)
        return
L04010:    init(" ") rk_barcode$
        return 
                                                /* (EWD003) - End     */

                                                /*  (AWD004)  - BEG */
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
                                                /*  (AWD004)  - END */


        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
/* (AWD007) */        
        check_casement_model
          init(" ") status$, code$
          tab% = 4%    :  code% = 0%
          code$ = str(sc_part$,1%,3%)
          gosub check_code
        return
/* (\AWD007) */        
        exit_sub
        end

