        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    CCC    AAA   L       SSSS  BBBBB   *~
            *  A   A  P   P  C   C  C   C  A   A  L      S      B    B  *~
            *  AAAAA  PPPP   C      C      AAAAA  L       SSS   BBBBB   *~
            *  A   A  P      C   C  C   C  A   A  L          S  B    B  *~
            *  A   A  P       CCC    CCC   A   A  LLLLL  SSSS   BBBBR   *~
            *                  ( SUBROUTINE )                           *~
            *-----------------------------------------------------------*~
            * APCCALSB - Build Calculated Width and Height Associated   *~
            *            with the Phantom Designator Passed to          *~
            *            Routine.                                       *~
            *     - CAL%     - 1% ( Width and Height )                  *~
            *                - 2% ( Width Only       )                  *~
            *                - 3% ( Height Only      )                  *~
            *                                                           *~
            *     - PARTNO$  - Manufactured Part Number          ( In  )*~
            *                  (Must be Greater Than 18)                *~
            *     - PHANTOM$ - Phantom Designator                ( In  )*~
            *                                                           *~
            *     - WIDTH    - Width in Decimal                  ( Out )*~
            *                                                           *~
            *     - HEIGHT   - Height in Decimal                 ( Out )*~
            *                                                           *~
            *     - #1       - Channel for (AMTBOMCD) File       ( In  )*~
            *                                                           *~
            *     - ERR%     - Error Code                        ( Out )*~
            *                  (0%) All Ok                              *~
            *                  (1%) Error with Width                    *~
            *                  (2%) Error with Height                   *~
            *                  (-1) Error No Equation                   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/16/91 ! New Subroutine for (APC) - LAST MOD DATE ! RHH *~
            * 04/28/08 ! (AWD001) mod for rounding error          ! CMG *~
            *07/22/2013! (AWD002) mod for dimensions 1 - 3        ! CMG *~
            *************************************************************

        sub "APCCALSB"   (cal%,          /* CAL TYPE 1%, 2%, 3%        */~
                          partno$,       /* MFG Part No > 18           */~
                          dim1es,        /* Dim1es (AWD002)            */~
                          dim2es,        /* Dim2es (AWD002)            */~
                          dim3es,        /* Dim3es (AWD002)            */~
                          phantom$,      /* Phantom Designator         */~
                          width,         /* Width Calculated in Decimal*/~
                          height,        /* Height Calculated in Dec.  */~
                          #1,            /* AMTBOMCD File - Equations  */~
                          err% )         /* Error Code 0 = Ok, 1 = err */

        dim                                                              ~
            partno$25,                   /* Part Number (Manufactured) */~
            phantom$25,                  /* Phantom Designator         */~
            cal_key$42,                  /* Equation Lookup Key        */~
            fld_val$(11)4,               /* Coverted Value of all Field*/~
            fld_no$2,                    /* Field Number               */~
/*AWD001*/  test$12,                     /* Test Value                 */~
            value$(6,3)12,               /* Equation                   */~
            value1$(6)8,                 /* Equation                   */~
            value2$(6)8,                 /* Equation                   */~
            value3$(6)8,                 /* Equation                   */~
            oper$(6,3)1,                 /* Equation                   */~
            oper1$(6)1,                  /* Equation                   */~
            oper2$(6)1,                  /* Equation                   */~
            result(6),                   /* Results From Each Level    */~
            calc_resl(2),                /* (1) - Width, (2) - Height  */~
            calc$4                       /* Used for Width Height Calc */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 01/16/91 NEW Build Descriptions Sub.    "
        REM *************************************************************


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AMTBOMCD ! Master Equation File                     *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************
            err% = 0%                                /* SET ERROR FLAG */
            init(" ") cal_key$, fld_no$, test$, fld_val$(), calc$

            fld_val$( 1) = str(partno$,1%,3%)          /* Model Number */
            fld_val$( 2) = str(partno$,4%,1%)          /* Color        */
            fld_val$( 3) = str(partno$,5%,2%)          /* Glass        */
            fld_val$( 4) = str(partno$,7%,2%)          /* Liting       */
            fld_val$( 5) = str(partno$,9%,2%)          /* Hinge        */
            fld_val$( 6) = str(partno$,11%,1%)         /* Screen       */
            fld_val$( 7) = str(partno$,12%,1%)         /* Locks        */
            fld_val$( 8) = str(partno$,13%,4%)         /* Width        */
            fld_val$( 9) = str(partno$,17%,3%)         /* Height       */
            fld_val$(10) = str(partno$,20%,3%)         /* CLMR         */
            fld_val$(11) = str(partno$,23%,3%)         /* WALLWIDT     */

            calc_resl(1%), calc_resl(2%) = 0.0

        REM *************************************************************~
            *     C A L C U L A T E   W I D T H   AND   H E I G H T     *~
            *                                                           *~
            *************************************************************

            c% = 0%
L10060:     c% = c% + 1%
            if c% = 3% then goto exit_program
            if c% = 1% then fld_no$ = "08" else fld_no$ = "09"
                                                   /* WIDTH ONLY     */
            if c% = 2% and cal% = 2% then goto exit_program
            if cal% <> 3% then goto L10150
               c% = 2%                             /* HEIGHT ONLY    */
               fld_no$ = "09"

L10150:     cal_key$ = " "
            str(cal_key$,1%,15%)  = fld_val$(1%)
            str(cal_key$,16%,2%)  = fld_no$            /* 8 OR 9      */
            str(cal_key$,18%,25%) = phantom$           /* GLASS OR SCR*/

            mat result = zer
            calc_resl(c%) = 0.0
            init (" ") value$(), value1$(), value2$(), value3$(),oper$(),~
                       oper1$(), oper2$()
                                    /* Load Equation for Field Calc.   */
            read #1,key = cal_key$,using L30000, value1$(), oper1$(),     ~
                                                value2$(), oper2$(),     ~
                                                value3$(),eod goto L10290
            goto L10320
L10290:        err% = -1%                       /* NO EQUATION FOUND   */
               goto exit_program
                                                /* DO CALCULATION      */
L10320:     if value1$(1) <> " " then L10350
               goto L10290                       /* NO EQUATION         */

L10350:     for x% = 1% to 6%       /* Unpack (6) Level Equation */
              value$(x%,1) = value1$(x%) : oper$(x%,1) = oper1$(x%)
              value$(x%,2) = value2$(x%) : oper$(x%,2) = oper2$(x%)
              value$(x%,3) = value3$(x%)
            next x%

            for x% = 1% to 6%            /* Process Equation */
                 for i% = 1% to 3%
                   cmg$ = value$(x%,i%)
                   if str(value$(x%,i%),1%,1%) <> "F" then goto L10510
                                              /* Convert to Decimal    */
                      if str(value$(x%,i%),2%,2%) = "08" then            ~
                                         gosub convert_width else        ~
                                         gosub convert_height
                      if err% <> 0% then goto exit_program
                      value$(x%,i%) = test$   /* Converted Field Value */
                      goto L10570
/* (AWD002) */                      
L10510:            if str(value$(x%,i%),1%,1%) <> "D" then goto L10550
                      if str(value$(x%,i%),2%,2%) = "01" then      ~
                              gosub convert_dim1es
                      if str(value$(x%,i%),2%,2%) = "02" then      ~
                              gosub convert_dim2es
                      if str(value$(x%,i%),2%,2%) = "03" then      ~
                              gosub convert_dim3es 
                      if err% <> 0% then goto exit_program
                      value$(x%,i%) = test$   /* Converted Field Value */
                      goto L10570
/* (\AWD002) */
L10550:            if str(value$(x%,i%),1%,1%) <> "R" then goto L10560
                      test$ = str(value$(x%,i%),2%,2%)
                      gosub find_results
                      if err% <> 0% then goto exit_program
                      value$(x%,i%) = test$   /* Converted Result No.  */
L10560:            if value$(x%,i%) = " " then goto L10660   /* NEXT I% */
L10570:               if i% <> 1% then goto L10600
                         convert value$(x%,i%) to result(x%)
                         goto L10660                         /* NEXT I% */
L10600:               convert value$(x%,i%) to calc    /* Fld/Rslt Val */

               if oper$(x%,i%-1) = "+" then result(x%) = result(x%) + calc
               if oper$(x%,i%-1) = "-" then result(x%) = result(x%) - calc
               if oper$(x%,i%-1) = "*" then result(x%) = result(x%) * calc
               if oper$(x%,i%-1) = "/" then result(x%) = result(x%) / calc
L10660:          next i%
            next x%
L10680:     x% = x% - 1%                      /* Backward thru Results */
            if x% = 0% then goto L10060             /* Calc Next Field  */
               if result(x%) = 0% then goto L10680  /* Check Previous   */
                  calc_resl(c%) = result(x%)       /* Result for Field */
                  goto L10060                         /* Calc Next Field*/

L30000: FMT                 /* FILE: AMTBOMCD                          */~
            XX(15),         /* Key Field One Value                     */~
            XX(2),          /* Key Field Two                           */~
            XX(25),         /* Phantom Identifier                      */~
            6*CH(8),        /* Value or Field # or Result Line #       */~
            6*CH(1),        /* Operand(*+-/)                           */~
            6*CH(8),        /* Value or Field # or Result Line #       */~
            6*CH(1),        /* Operand(*-+/)                           */~
            6*CH(8),        /* Value or Field # or Result Line #       */~
            XX(52)          /* Filler                                  */~


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *                                                           *~
            *************************************************************

        convert_width
           calc$ = fld_val$(8) : err% = 0%
           convert str(calc$,4%,1%) to cdec,data goto L60140

           convert str(calc$,1%,3%) to calc,data goto L60140

           calc = calc + (cdec/8.0)           /* CONVERT TO DECIMAL */
           convert calc to test$, pic(0000.0000000)   /* (AWD001) */
        return
L60140:    err% = 1%
        return

        convert_height
           calc$ = fld_val$(9) : err% = 0%
           convert str(calc$,3%,1%) to cdec,data goto L60260

           convert str(calc$,1%,2%) to calc,data goto L60260

           calc = calc + (cdec/8.0)           /* CONVERT TO DECIMAL */
           convert calc to test$, pic(0000.0000000)   /* (AWD001) */
        return
L60260:    err% = 2%
        return

        find_results
           convert str(test$,2%,2%) to num%, data goto L60350

           convert result(num%) to test$, pic(0000.0000000) 
                                                   /* (AWD001) */
        return
L60350:    err% = 1%
        return
        
/* (AWD002) */
        convert_dim1es
           err% = 0%
           convert dim1es to test$, pic(0000.0000000)
        return

        convert_dim2es
           err% = 0%
           convert dim2es to test$, pic(0000.0000000)
        return
        
        convert_dim3es
           err% = 0%
           convert dim3es to test$, pic(0000.0000000)
        return                

/* (\AWD002) */        

        exit_program

           width  = calc_resl(1%)
           height = calc_resl(2%)

        REM  CONVERT ERR% TO ERR$, PIC(-##)
        REM  STOP "PART = " & PARTNO$ & " ERR CODE = " & ERR$
        REM  CLOSE WS

        end

