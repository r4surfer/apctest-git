        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  JJJJJ  BBBB   TTTTT   CCC   RRRR    AAA   TTTTT  EEEEE   *~
            *    J    B   B    T    C   C  R   R  A   A    T    E       *~
            *    J    BBBB     T    C      RRRR   AAAAA    T    EEEE    *~
            *  J J    B   B    T    C   C  R   R  A   A    T    E       *~
            *   J     BBBB     T     CCC   R   R  A   A    T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBTCRATE - Determines the Base Rate and the Pay Rate      *~
            *            for a JB labor distribution entry per the      *~
            *            arguments supplied.                            *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/08/88 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "JBTCRATE"   (               /* Incomming Arguments        */~
                          emp$,          /*   Employee Code            */~
                          earn_type$,    /*   Earnings Type            */~
                          dept$,         /*   Department Code          */~
                          shift$,        /*   Shift Number (1-4)       */~
                          ot$,           /*   Overtime? 'Y' = Yes      */~
                          hol$,          /*   Holiday?  'Y' = Yes      */~
                                                                         ~
                                         /* Returned Arguments         */~
                          use_dept_rate$,/*   Use Department Rate?     */~
                          base,          /*   Base Rate                */~
                          rate,          /*   Pay Rate                 */~
                                                                         ~
                                         /* File Channels              */~
                          #1, #2,        /*   EMPMASTR  PRLDEPTF       */~
                          #3      )      /*   EMPEARN1                 */

        dim                                                              ~
            dept$4,                      /* Department Code            */~
            earn_type$12,                /* Earnings Type              */~
            emp$12,                      /* Employee Code              */~
            hol$1,                       /* Holiday? (Y/-)             */~
            ot$1,                        /* Overtime? (Y/-)            */~
            readkey$50,                  /* Read Key                   */~
            shift$1,                     /* Shift Number (1-4)         */~
            use_dept_rate$1,             /* Use dept rate? (Y/N)       */~
                                                                         ~
                                         /* FACTORS                    */~
            shift_factor(4),             /*   Shift                    */~
            ot_factor(4),                /*   Overtime                 */~
            hol_factor(4),               /*   Holiday                  */~
            hol_ot_factor(4)             /*   Holiday & Overtime       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            base, rate        = 0
            mat shift_factor  = con
            mat ot_factor     = con
            mat hol_factor    = con
            mat hol_ot_factor = con

            shift% = 1% : convert shift$ to shift%, data goto L09140

L09140
*        Since we said we would return a base rate, we'll get one
*        rather we need it or not.
            call "READ100" (#1, emp$, f1%)                 /* empmastr */
            if f1% = 1% then get #1 using L09180, emp_base
L09180:         FMT POS(121), PD(14,4)

            call "READ100" (#2, dept$, f1%)                /* prldeptf */
            if f1% = 1% then get #2 using L09240,                          ~
                          use_dept_rate$, shift_factor(), ot_factor(),   ~
                          hol_factor(), hol_ot_factor(), dept_base
L09240:         FMT POS(57), CH(1), POS(98), 16*PD(14,6), PD(14,4)

            if use_dept_rate$ = "Y" then base = dept_base                ~
                                    else base = emp_base


        REM *************************************************************~
            *                  M A I N   L O G I C                      *~
            *-----------------------------------------------------------*~
            * Main Logic Section.                                       *~
            *************************************************************

        if earn_type$ = " " then L10150

*        Determine Rate via Earnings Type...
            readkey$ = str(emp$,,12) & earn_type$
            call "PLOWALTS" (#3, readkey$, 1%, 24%, f1%)
            if f1% = 1% then get #3 using L10120, rate
L10120:         FMT POS(52), PD(14,4)
            goto exit_program

L10150
*        Determine Rate via Department Record...
            factor  = shift_factor(shift%)
            if ot$  = "Y" then factor = ot_factor(shift%)
            if hol$ = "Y" then factor = hol_factor(shift%)
            if ot$  = "Y" and hol$ = "Y" then                            ~
                                           factor = hol_ot_factor(shift%)
            rate = round(base * factor, 4)
            goto exit_program


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
