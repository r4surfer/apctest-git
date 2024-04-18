        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCDESCR                             *~
            *  Creation Date     - 03/24/92                             *~
            *  Last Modified Date- 04/24/00                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Description       - Build Screen Display Descriptions    *~
            *                      (PRT) Print Display Description      *~
            *                      (SZE) Size Description               *~
            *                       (Skip - for Print 'N/A')            *~
            *                                                           *~
            *     - PARTNO$  - Manufactured Part Number          ( In  )*~
            *                  (Must be Greater Than 16)                *~
            *     - APC_SCR$ - Screen Display Description (120)  ( Out )*~
            *                                                           *~
            *     - APC_PRT$ - Print Display Description  ( 60)  ( Out )*~
            *                                                           *~
            *     - APC_SZE$ - Size Description Long Form ( 20)  ( Out )*~
            *                                                           *~
            *     - #1       - Channel for (AMTBOMIF) File       ( In  )*~
            *                                                           *~
            *     - ERR%     - Error Code                        ( Out )*~
            *                  (0%) All Ok                              *~
            *                  (1%) In Valid Description                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/24/92 ! New Subroutine for (APC) - LAST MOD DATE ! RHH *~
            * 11/17/97 ! Mof for Upgrade to R6.04.03              ! RHH *~
            * 05/26/98 ! (EWD001) - Mod for Wood Surround Factory ! RHH *~
            *          !   Mull. Show Mull Code                   !     *~
            * 02/24/00 ! (EWD002) - Mod Build the Description for ! RHH *~
            *          !            literature using files 2,8,9  !     *~
            * 02/29/00 ! (EWD003) - Mod to not set size on sample ! RHH *~
            *          !            parts to Zero.                !     *~
            * 04/24/00 ! (EWD004) - Mod to correct problem for new! RHH *~
            *          !            Sample code '027' Note part   !     *~
            *          !            range is (12 thru 26)         !     *~      
            * 08/14/01 ! (EWD005) - Mod to correct problem for new! CMG *~
            *          !            Sample code change to '035'   !     *~
            * 09/16/02 ! (EWD006) Fix for Special Shapes Grid Code! CMG *~
            *************************************************************

        sub "APCDESCR"   (partno$,       /* MFG Part No > 18           */~
                          apc_scr$,      /* Screen Display Description */~
                          apc_prt$,      /* Print Display Description  */~
                          apc_sze$,      /* Size Display Description   */~
                          #1,            /* AMTBOMIF File              */~
                          err% )         /* Error Code 0 = Ok, 1 = err */

        dim                                                              ~
            partno$25,                   /* Part Number (Manufactured) */~
            apc_scr$120,                 /* Screen Display Description */~
            apc_prt$60,                  /* Print Display Description  */~
            apc_sze$20,                  /* Size Display Description   */~
            scr$20,                      /* Screen Display Description */~
            prt$30,                      /* Printer Display Description*/~
            i$24,                        /* Field Number Array         */~
            size$40,                     /* Size Table                 */~
            readkey$50,                  /* GENCODES Key               */~
            fld_val$(11%)4,              /* Field Values               */~
            wd$7,                        /* Replacement Width          */~
            ht$6                         /* Replacement Height         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Build Long/Short Product Descript "
            pname$ = "APCDESCR - Rev: R6.04"

        REM *************************************************************


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AMTBOMIF ! Master Validity File                     *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************
            err% = 0%                                /* SET ERROR FLAG */
            init(" ") apc_scr$, apc_prt$, apc_sze$, wd$, ht$, fld_val$()

            fld_val$(1%) = str(partno$,1%,3%)          /* Model Number */
            fld_val$(2%) = str(partno$,4%,1%)          /* Color        */
            fld_val$(3%) = str(partno$,5%,2%)          /* Glass        */
            fld_val$(4%) = str(partno$,7%,2%)          /* Liting       */
            fld_val$(5%) = str(partno$,9%,2%)          /* Hinge        */
            fld_val$(6%) = str(partno$,11%,1%)         /* Screen       */
            fld_val$(7%) = str(partno$,12%,1%)         /* Locks        */
            fld_val$(8%) = str(partno$,13%,4%)
            fld_val$(9%) = str(partno$,17%,3%)

            wd$          = str(partno$,13%,4%)         /* Width        */
            ht$          = str(partno$,17%,3%)         /* Height       */
            fld_val$(10%)= STR(PARTNO$,20%,3%)         /* CLMR         */
            fld_val$(11%)= STR(PARTNO$,23%,3%)         /* WALLWIDT     */

        REM *************************************************************~
            *             B U I L D   D E S C R I P T I O N S           *~
            *                                                           *~
            *************************************************************
                                      /* STRING FIELD VALUE            */
            i$ = "XX0102030405060708091011"

            readkey$ = all(hex(00))
            str(readkey$,1%,15%) = fld_val$(1%)
               for i% = 1% to 11%
                 init(" ") scr$, prt$ : x% = 0% /* (EWD001) - Mod   */
                 z% = (i%*2%) + 1%
                 str(readkey$,16%,2%)  = str(i$, z%, 2%)
                 str(readkey$,18%,15%) = fld_val$(i%)
                 if i% = 8% or i% = 9% and str(partno$,1%,3%) <> "003" ~
                                  then goto L01210 /* (EWD001) */
                    if i% < 10% then goto L01070
                       convert str(fld_val$(i%),1%,1%) to x%,         ~
                                                 data goto L01170

                       goto L01210          /* (EWD001) Not W/S     */  
L01070:          read #1,key = readkey$, using L01080 , scr$, prt$,        ~
                                                      eod goto L01100
L01080:            FMT POS(34), CH(20), CH(30)
                 goto L01140
L01100:            scr$, prt$ = " "
                   scr$ = "E(" & str(i$, z%+1%, 1%) & ")"
                   prt$ = "E(" & str(i$, z%+1%, 1%) & ")"
                   err% = err% + 1%
L01140:          if i% > 1% then  goto L01180
                    if str(partno$,1%,3%) = "003" then goto L01210
                    apc_scr$ = scr$
                    apc_prt$ = prt$
                    goto L01210
                                             /* (EWD001) - Begin */
L01170:          scr$ = fld_val$(i%)            
                 prt$ = fld_val$(i%)
                                             /* (EWD001) - End   */
L01180:          apc_scr$ = apc_scr$ & " " & scr$
                 if str(prt$,1%,3%) = "N/A" then goto L01210
                                             /* (EWD002)         */
                 if str(partno$,1%,3%) <> "003" then goto L01190
                    if i% = 2% then apc_prt$ = apc_prt$ & " " & prt$
                    if i% = 8% then apc_prt$ = apc_prt$ & " " & prt$
                    if i% = 9% then apc_prt$ = apc_prt$ & " " & prt$
                    goto L01210
                                             /* (EWD002)         */ 
L01190:             apc_prt$ = apc_prt$ & " " & prt$

L01210:        next i%

        REM Build Size Long Form
                              /* F0%       - FRACT. NEW PART WIDTH    */
                              /* F1%       - FRACT. NEW PART HEIGHT   */
                              /* WD$   - REPLACEMENT WIDTH & FRACT (7)*/
                              /* HT$   - REPLACEMENT HEIGHT & FRACT(6)*/
                              /* SIZE$ - DECIMAL TO NEAREST 8TH INCH  */
           size$ = "XXXX 1/8 1/4 3/8 1/2 5/8 3/4 7/8 ERR    "

           if str(wd$,1%,1%) = "0" then str(wd$,1%,1%) = " "

           convert str(wd$,4%,1%) to f0%, data goto L01380

           convert str(ht$,3%,1%) to f1%, data goto L01380

           goto L01400
L01380:      err% = err% + 1%
             f0%, f1% = 8%
L01400:    if f0% = 0% then f0% = 9%
           if f1% = 0% then f1% = 9%
                                         /* Build Width with Fraction */
           str(wd$,4%,4%) = str(size$,f0%*4%+1%, 4%)  /* Save Fraction */
                                         /* Build Height with Fraction */
           str(ht$,3%,4%) = str(size$,f1%*4%+1%, 4%)  /* Save Fraction */
                                         /* Build Long Form Size Descr */
           apc_sze$ = str(wd$) & " X " & str(ht$)

           gosub check_samples           /* (EWD004) Parts 12 thru 26  */       
                                         /* (EWD005) Parts 12 thru 35  */
           if ss% < 12% or ss% > 35% then end
                                         /* Sample Part                */  
              apc_sze$ = "000 0/0" & " X " & "00 0/0"  
         
  end
                                                      /* (EWD003)       */
        check_samples
            ss% = 0%
            if len(partno$) < 20 then goto LS2      /* Quick Test      */
            if str(partno$,1%,1%) = "9" then goto LS2 /* Bay/Bow       */
            convert str(partno$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1   /* Not Samp/Disp   */

            if str(partno$,7%,2%) > "99" then goto LS1 /*   (EWD006)    */

        return                                       /* Code Found      */
LS1:        convert str(partno$,23%,3%) to ss%, data goto LS2

            if ss% < 1% or ss% > 80% then goto LS2
                                                     /* Code Found      */ 
        return
LS2:        ss% = 0%
        return  
                                                       /* (EWD003)      */

