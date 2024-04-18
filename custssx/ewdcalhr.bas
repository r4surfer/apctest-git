        REM *************************************************************~
            *                                                           *~
            *  EEEEE  W   W  DDDD    CCC    AAA   L      H   H  RRRRR   *~
            *  E      W   W  D   D  C   C  A   A  L      H   H  R    R  *~
            *  EEEE   W   W  D   D  C      AAAAA  L      HHHHH  RRRRR   *~
            *  E      W W W  D   D  C   C  A   A  L      H   H  R    R  *~
            *  EEEEE   W W   DDDD    CCC   A   A  LLLLL  H   H  R    R  *~
            *                  ( SUBROUTINE )                           *~
            *-----------------------------------------------------------*~
            * EWDCALHR - Build Calculated Width, Height, and Radius for *~
            *            specified Half Rounds.                         *~
            *            Routine.                                       *~
            *                                                           *~
            *     - PARTNO$  - Manufactured Part Number          ( In  )*~
            *                  (Must be Greater Than 18)                *~
            *                                                           *~
            *     - WIDTH    - Width in Decimal                  ( Out )*~
            *                                                           *~
            *     - HEIGHT   - Height in Decimal                 ( Out )*~
            *                                                           *~
            *     - #1       - Channel for (AMTBOMCD) File       ( In  )*~
            *                                                           *~
            *     - #2       - Channel for (GENCODES) File       ( In  )*~
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
            *************************************************************

        sub "EWDCALHR"   (partno$,       /* MFG Part No > 18           */~
                          width,         /* Width Calculated in Decimal*/~
                          height,        /* Height Calculated in Dec.  */~
                          #1,            /* AMTBOMCD File - Equations  */~
                          #3,            /* GENCODES File              */~
                          err% )         /* Error Code 0 = Ok, 1 = err */

        dim                                                              ~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            descr$30,                    /* Use for GENCODES Look-Up   */~
            partno$25,                   /* Part Number (Manufactured) */~
            fld_val$(11)4,               /* Coverted Value of all Field*/~
            test$8                       /* Test Value                 */

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
            * #2  ! GENCODES ! Master System Table File                 *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************
            err% = 0%                                /* SET ERROR FLAG */
            init(" ") test$, fld_val$(), calc$

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

            p, g, factor1, factor2, factor_exp, radius, calc = 0.00
        REM *************************************************************~
            *     C A L C U L A T E   W I D T H   AND   H E I G H T     *~
            *                                                           *~
            *************************************************************

            readkey$ = " "
            str(readkey$,1%,9%)   = "SHPHFOFF "
            str(readkey$,10%,15%) = fld_val$(1%)

                                    /* Load Equation for Field Calc.   */
            read #3,key = readkey$, using L30000, descr$, eod goto no_eq

            convert str(descr$,1%,6%)  to p, data goto no_eq

            convert str(descr$,24%,6%) to g, data goto no_eq

            gosub convert_width
            gosub convert_height

            radius = (height - p - g)
            height = (radius - p - g)

            if fld_val$(1%) = "553" or fld_val$(1%) = "554" then          ~
                                    height = height + .25

            factor1 = (2 * height)
            factor_exp = (height * height)
            factor2 = ((radius * factor1) - factor_exp)

            width = SQR(factor2)
            width = (width * 2)

        goto exit_program
        no_eq
          err% = 1%
          goto exit_program   

L30000: FMT                 /* FILE: GENCODES                          */~
            POS(25),        /*                                         */~
            CH(30)

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *                                                           *~
            *************************************************************

        convert_width
           calc$ = fld_val$(8) : err% = 0%
           convert str(calc$,4%,1%) to cdec,data goto L60140

           convert str(calc$,1%,3%) to calc,data goto L60140

           calc  = calc + (cdec/8.0)           /* CONVERT TO DECIMAL */
           width = calc
           convert calc to test$, pic(0000.000)
        return
L60140:    err% = 1%
        return

        convert_height
           calc$ = fld_val$(9) : err% = 0%
           convert str(calc$,3%,1%) to cdec,data goto L60260

           convert str(calc$,1%,2%) to calc,data goto L60260

           calc   = calc + (cdec/8.0)           /* CONVERT TO DECIMAL */
           height = calc
           convert calc to test$, pic(0000.000)
        return
L60260:    err% = 2%
        return



        exit_program

REM        width  = calc_resl(1%)
REM        height = calc_resl(2%)

        REM  CONVERT ERR% TO ERR$, PIC(-##)
        REM  STOP "PART = " & PARTNO$ & " ERR CODE = " & ERR$
        REM  CLOSE WS

        end



