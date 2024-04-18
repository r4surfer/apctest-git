        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPR5SB                             *~
            *  Creation Date     - 01/15/94                             *~
            *  Last Modified Date- 08/27/2009                           *~
            *  Description       - This Subroutine Calculates the       *~
            *                      Standard deduction for both Width    *~
            *                      and Height.                          *~
            *                                                           *~
            *  Special Comments  - Uses Table (PRICE 010) for Deduction *~
            *                      Codes. 001 - Near't 1/8 Inch W/H     *~
            *                             002 - Near't 1/4 Inch W/H     *~
            *                             003 - Near't 1/8 W,1 5/8 H    *~
            *                             004 - Near't 1/4 W,1 5/8 H    *~
            *                             006 - Special Adj. (312)      *~
            *                             010 - Near't 1/8 Inch Twin/Trp*~
            *                             011 - Near't 1/4 Inch Twin/Trp*~
            *                    - Special code for Standard Deduction  *~
            *                      for (312) Vinyl Patio Doors. (1260)  *~
            *                    - If SIZE$ = "F" then Change the Value *~
            *                      to "E" to Use the Exact size Calc.   *~
            *                      Process.                             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/27/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 07/14/94 ! Mod for (312) Standard Deduction on      ! RHH *~
            *          ! Three (3) Panel Doors - Line (1260)      !     *~
            * 03/25/97 ! Mods to Clean-Up for Unix and for New    ! RHH *~
            *          !   Product Changes.                       !     *~
            * 03/31/97 ! Mods to add new CAL% Codes 010 and 011   ! RHH *~
            *          !   to use for STD Deduction Spec's over   !     *~
            *          !   Twins and Triples                      !     *~
            * 10/31/97 ! Check for Release Upgrade to R6.04.03    ! RHH *~
            *08/27/2009! (AWD001) mod to end if 'E'xact Size      ! CMG *~ 
            *************************************************************

          sub "APCPR5SB" (size$,        /* (E)xact or (O)pening        */~
                          part$,        /* MFG Part Always Exact       */~
                          part_o$,      /* MFG Part Always Opening     */~
                          cuscode$,     /* Customer Code               */~
                          sp%,          /* Special Flag 0%=Cat,1%=Spc  */~
                          pc_c$,        /* Special Catalog Code or 0000*/~
                          pc_cm$,       /* Special Catalog Method or 00*/~
                          #1,           /* (APCPCMST) - File           */~
                          #2,           /* (GENCODES) - File           */~
                          err% )        /* Error Non Zero Value        */
        dim                              /* (APCPCMST) Price Calc Def. */~
            size$1,                      /* (E)xact or (O)pening       */~
            part$25,                     /* Part Number Always Exact   */~
            part_o$25,                   /* Part Number Always Opening */~
            cuscode$9,                   /* Customer Code              */~
            pc_c$4,                      /* Pricing Catalog Code       */~
            pc_cm$2,                     /* Pricing Catalog Method Code*/~
            pc_m$3,                      /* Model Code                 */~
            pc_k$25,                     /* Price Generic Key          */~
            p_key$53, s_key$28,          /* Primary Key                */~
            r_key$24, r_desc$32,         /* Generic Key - Tables       */~
            e_w$4, o_w$4,                /* Exact and Opening Width    */~
            e_h$3, o_h$3,                /* Exact and Opening Height   */~
            hg$2, tab_desc$30,           /* Hinge Codes/ Code Tab Desc */~
            panels$2,                    /* Number of Panels           */~
            cal$3, rr$4                  /* Special Calc Code          */

        REM - Main Line Routine
            err% = 0%
            pc_c$  = "0000"              /* Set Initial Values to begin*/
            pc_cm$ = "00"                /* Pricing and Obtain Standard*/
            pc_m$  = str(part$,1%,3%)    /* Deduction, or Override     */

REM         if size$ = "E" then end

            sp% = 0% :  cal% = 0%        /* Customer Deduction         */
            w_val% = 0% : h_val% = 0%
            gosub lookup_deduction      /* Obtain Standard Catalog 1st */
            gosub lookup_customer       /* Check For Special Catalogs  */
            if sp% <> 0% then gosub lookup_deduction /*Check for Spec. */
                                        /* Deduction Override for Cust.*/
            gosub part_size             /* Using CAL%, Calculate the   */
                                        /* W_VAL% and H_VAL% for W/H   */
        end                             /* End of Main Line Routines   */

        REM *************************************************************~
            *                L o o k u p   D a t a                      *~
            *************************************************************

        lookup_deduction                         /* all Models have APC*/
            init(" ") p_key$, panels$, s_key$    /* Catalog Deduction  */
            str(p_key$,1%,1%)  = "A"             /* Only Active Recs   */
            str(p_key$,2%,4%)  = pc_c$           /* APC Catalog 1st    */
            str(p_key$,6%,2%)  = pc_cm$          /* Calc Cat. then Spec*/
            str(p_key$,8%,16%) = pc_m$           /* MFG Model Code     */
            str(p_key$,24%,2%) = "01"            /* Std Size Ref.      */
            str(p_key$,26%,3%) = "000"           /* Standard Deduction */
            read #1,key > p_key$, using L00930 , s_key$, pc_k$, cal$,       ~
                                                            eod goto L01290
L00930:        FMT POS(9), CH(28), CH(25), POS(110), CH(3)
            if s_key$ <> str(p_key$,1%,28%) then goto L01290
                                               /* No Deduction Defined */
            rr$ = str(pc_k$,1%,4%)
            rr% = pos(rr$ = "-")
            if rr% <> 0% then rr$ = str(rr$,1%,rr% - 1%)
            convert rr$ to w_val%, data goto L01000
L01000:                                        /* Width Deduction Value*/
            if rr% <> 0% then w_val% = (-1.0) * w_val%  /* Chg to Neg. */

            rr$ = str(pc_k$,5%,3%)
            rr%  = pos(rr$ = "-")
            if rr% <> 0% then rr$ = str(rr$,1%,rr% - 1%)
            convert rr$ to h_val%, data goto L01070
L01070:                                        /* Height Deduction Val */
            if rr% <> 0% then h_val% = (-1.0) * h_val%

            convert cal$ to cal%, data goto L01110
L01110:
            if cal% <> 6% then L01180
               gosub calc_panels           /* Special (312) Adjustment */
               if panels$ = "02" then return
               if panels$ = "03" then w_val% = w_val% -  6% /* 1/2 DED */
               if panels$ = "04" then w_val% = w_val% + 23%
               return
L01180:     if cal% <> 10% then goto L01240
               gosub find_twin_trpl
               if lt% = 1% then return
                  if lt% = 2% then w_val% = (2% * w_val%) + 1%   /*1/8"*/
                  if lt% = 3% then w_val% = (3% * w_val%) + 2%   /*1/4"*/
                  return
L01240:     if cal% <> 11% then return
               gosub find_twin_trpl
               if lt% = 1% then return
                  if lt% = 2% then w_val% = (2% * w_val%) + 1%   /*1/8"*/
                  if lt% = 3% then w_val% = (3% * w_val%) + 2%   /*1/4"*/
L01290: return

        lookup_customer               /* 1st Check for Special Catalog */
            init(" ") r_key$, r_desc$ /* for all of Customer Group     */
            str(r_key$,1%,9%) = "PRICECUST" /* Customer Catalog/Method */
REM            str(r_key$,10%,15%) = str(cuscode$,1%,2%) & "0000"
REM            read #2,key = r_key$, using   L01360, r_desc$, eod goto L01390
L01360:        FMT POS(25), CH(32)
REM               sp% = sp% + 1%
                                      /* 2nd Check for Store Catalog   */
L01390:     str(r_key$,10%,15%) = cuscode$
            read #2,key = r_key$, using   L01360, r_desc$, eod goto L01420
               sp% = sp% + 1%
L01420:     if sp% = 0% then return   /* Customer is APC Catalog Only  */
               pc_c$  = str(r_desc$,1%,4%)    /* Customer Catalog Code */
               pc_cm$ = str(r_desc$,8%,2%)    /* Calc Method Code      */
               sp% = 1%                       /* Special Customer Cat. */
        return

        REM *************************************************************~
            *                   P A R T   S I Z E                       *~
            *************************************************************

        part_size
            if size$ <> "F" then goto L01550
               size$ = "E" : return
L01550:     init(" ") e_w$, e_h$, o_w$, o_h$
            e_w% = 0% : e_h% = 0% : o_w% = 0% :  o_h% = 0%
            e_w$ = str(part$,13%,4%) : o_w$ = e_w$
            e_h$ = str(part$,17%,3%) : o_h$ = e_h$
            convert e_w$ to e_w%, data goto L01600 /* Std Width Exact  */
L01600:
            o_w% = e_w%                          /* Set Opening-Exact*/
            convert e_h$ to e_h%, data goto L01630 /* Std Height Exact */
L01630:
            o_h% = e_h%                          /* Set Opening-Exact*/
        REM - Exact Size Done
            a1% = 0% : a2% = 0%
            if size$ <> "E" then goto L01920       /* Calc for Opening */
               convert str(o_w$,1%,3%) to a1%, data goto L01690
L01690:
               convert str(o_w$,4%,1%) to a2%, data goto L01710
L01710:                                              /* For the Width */
               x1  = ((a1% * 8%) + a2% + w_val%)/8.0 /* Total Eight's */
               a1% = int(x1)
               x2  = x1 - a1%                        /* Dec. Eight's  */
               o_w% = (a1% * 10%) + int(x2*8)
               a1% = 0% : a2% = 0%
               convert str(o_h$,1%,2%) to a1%, data goto L01780
L01780:
               convert str(o_h$,3%,1%) to a2%, data goto L01800
L01800:                                              /* For the Height */
               x1  = ((a1% * 8%) + a2% + h_val%)/8.0 /* Total Eight's  */
               a1% = int(x1)
               x2  = x1 - a1%                        /* Dec. Eight's   */
               o_h% = (a1% * 10%) + int(x2*8)

               part_o$ = part$
               convert o_w% to str(part_o$,13%,4%), pic(0000)

               convert o_h% to str(part_o$,17%,3%), pic(000)

               goto L02110                           /* Opening Complete */
L01920: REM - Do Calculation Process for Exact Size
            a1% = 0% : a2% = 0%
            convert str(e_w$,1%,3%) to a1%, data goto L01950
L01950:
            convert str(e_w$,4%,1%) to a2%, data goto L01970
L01970:                                              /* For the Width  */
            x1  = ((a1% * 8%) + a2% - w_val%)/8.0    /* Total Eight's  */
            a1% = int(x1)
            x2  = x1 - a1%                           /* Dec. Eight's   */
            e_w% = (a1% * 10%) + int(x2*8)
            a1% = 0% : a2% = 0%
            convert str(e_h$,1%,2%) to a1%, data goto L02040
L02040:
            convert str(e_h$,3%,1%) to a2%, data goto L02060
L02060:                                              /* For the Height */
            x1  = ((a1% * 8%) + a2% - h_val%)/8.0    /* Total Eight's  */
            a1% = int(x1)
            x2  = x1 - a1%                           /* Dec. Eight's   */
            e_h% = (a1% * 10%) + int(x2*8)           /* Exact Complete */
L02110: gosub size_correct
        if size$ <> "E" then part_o$ = part$
        convert e_w% to str(part$,13%,4%), pic(0000)

        convert e_h% to str(part$,17%,3%), pic(000)
                                      /* PART$   = Exact Size Always   */
        return                        /* PART_O$ = Opening Size Always */

        size_correct                     /* Adjust Exact Size          */
           a1% = 0%                      /* (1) - N'st 1/8 W - H       */
           a2% = 0%                      /* (2) - N'st 1/4 W - H       */
           a3% = 0%                      /* (3) - N'st 1/8 W - 1-5/8 H */
                                         /* (4) - N'st 1/8 W - 1-5/8 H */
           if cal% < 2% or cal% > 4% then return
              convert e_w% to e_w$, pic(0000)

              convert e_h% to e_h$, pic(000)
                                                 /* Width Eight's */
           convert str(e_w$,4%,1%) to a1%, data goto L02300
L02300:                                          /* Height Eight's */
           convert str(e_h$,3%,1%) to a2%, data goto L02320
L02320:
           convert str(e_h$,1%,2%) to a3%, data goto L02340
L02340:
           if cal% = 2% then goto L02480            /* W = ?/4, H = ?/4  */
                                                  /* Cal% = 3 and 4    */
              if a2% <> 0% then goto L02440         /* Height 1/8 or 5/8 */
                 a3% = a3% - 1%
                 convert a3% to str(e_h$,1%,2%), pic(00)
                 str(e_h$,3%,1%) = "5"
                 if cal% = 3% then goto L02550
                    goto L02480

L02440:       if a2% >= 1% and a2% <= 4% then str(e_h$,3%,1%) = "1"      ~
                                         else str(e_h$,3%,1%) = "5"
              if cal% = 3% then goto L02550
                                            /* Nearest 1/4 Inch Width  */
L02480:    if mod(a1%,2%) = 0 then goto L02510                /* 0,2,4,6 */
              convert (a1%-1%) to str(e_w$,4%,1%), pic(0)

L02510:    if cal% <> 2% then goto L02550      /* Nearest 1/4 Inch Height */
           if mod(a2%,2%) = 0 then goto L02550                /* 0,2,4,6 */
              convert (a2%-1%) to str(e_h$,3%,1%), pic(0)

L02550:    convert e_w$ to e_w%, data goto L02560
L02560:
           convert e_h$ to e_h%, data goto L02580
L02580:
        return

        calc_panels
           hg$ = str(part$,9%,2%)
           panels$ = "02"
           if hg$ = "37" then panels$ = "03"
           if hg$ = "42" then panels$ = "04"
        return

        find_twin_trpl                   /* Check for Twin or Triple   */
           hg$ = str(part$,9%,2%)
           lt% = 1%                      /* Single Window              */
           call "APCPR1SB" (0%, 5%, hg$, tab_desc$, p%, #2, tab_rec%)
           if tab_rec% = 0% then return
              if str(tab_desc$,p%+2%,4%) = "TWIN" then lt% = 2%
              if str(tab_desc$,p%+2%,4%) = "TRPL" then lt% = 3%
        return
