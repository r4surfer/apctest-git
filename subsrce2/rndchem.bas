        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  RRRR   N   N  DDDD    CCC   H   H  EEEEE  M   M          *~
            *  R   R  NN  N  D   D  C      H   H  E      MM MM          *~
            *  RRRR   N N N  D   D  C      HHHHH  EEE    M M M          *~
            *  R   R  N  NN  D   D  C      H   H  E      M   M          *~
            *  R   R  N   N  DDDD    CCC   H   H  EEEEE  M   M          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RNDCHEM  - Special number rounding function for chemicals.*~
            *            All rounding is metric only - gm, kg, ml, lt.  *~
            *            Note that narcotics are handled in gm and ml   *~
            *             ONLY.                                         *~
            *            NIN = The rounded number in input units        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/16/83 ! ORIGINAL                                 ! ECR *~
            * 02/21/87 ! Complete Rewrite for Current System      ! MJB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

         sub "RNDCHEM" (n, deaclass$, units$, prtn$, nin)

         dim                                                             ~
             deaclass$4,                 /* DEA class of drug          */~
             index%(1),                  /* Index for search           */~
             narc$1,                     /* narcotic drug? (Y/N)       */~
             prtn$15,                    /* formatted number w/ units  */~
             temp$12,                    /* temporary STRING$          */~
             unitslist$( 5)2,            /* list of valid units of meas*/~
             units$2                     /* input unit of measure      */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.19.01 03/30/87 Patch release                   "
        REM *************************************************************


        rem* unitslist$ contains the five allowable solid and liquid    *~
           * units of measure; mg, gm, kg, ml and l.  units may be in   *~
           * either upper or lower case & must be 2 characters.  if none*~
           * passed in, the number will be rounded according to "G10.0" *

            str(unitslist$(), 1%, 10%) = "mggmkgmll "

             sign% = sgn(n)
             n     = abs(n)

             narc$ = "N"
             if deaclass$ <> " " then narc$ = "Y"
             tran (units$, "gGkKlLmM") replacing
             search str(unitslist$(), 1%)  = units$ to index%() step 2
                if index%(1) > 0% then L10200
                    prtn$ = units$
                    nna = n
                    nin = n
                    goto L10290

L10200:     index%(1) = index%(1) / 2% + 1%
            on index%(1) gosub  L20100,      /*  We have milligrams  */   ~
                                L20200,      /*  We have grams       */   ~
                                L20300,      /*  We have kilograms   */   ~
                                L30100,      /*  We have milliliters */   ~
                                L30200       /*  We have liters      */

            nna = sign% * nn
            nin = round(sign% * nin, 8%)
L10290:     call "CONVERT" (nna, 0.4, temp$)
            prtn$ = temp$ & " " & prtn$
            end

*        First we'll do all of the solid measurement routines
L20100: REM Incomming unit is in milligrams
            ngm = n / 1000
            gosub round_solids
            nin = ngm * 1000
            return

L20200: REM Incomming unit is in grams
            ngm = n
            gosub round_solids
            nin = ngm
            return

L20300: REM Incomming unit is in Kilograms
            ngm = n * 1000
            gosub round_solids
            nin = ngm / 1000
            return

        REM * This is where we round and determine units of weights *
        round_solids
*        1st range check is 0 - 300 grams
            if ngm > 300 then L21100
                nn = abs(int(-ngm * 100) / 100)
                prtn$ = "gm"              /* round up to even .01 gm */
                ngm = nn
                return

*        2nd range check is 300.01 - 1000 grams
L21100:     if ngm > 1000 and narc$ = "N" then L21170  /* Narcs in gms */
            nn = abs(int(-ngm))           /* round up to even 1.0 gm */
            prtn$ = "gm"
            ngm = nn
            return

*        Next range check is 1000 - 45000 grams
L21170:     if ngm > 45000 then L21280
            nkg = round(ngm, 8) / 1000   /* Round up to even .005 kg  */
            temp = abs(int(-nkg * 100) / 100 )
            nn = temp
            if abs(nkg - temp) = 0 then L21230
            if abs(nkg - temp) <= .005 then nn = temp + .005             ~
                                       else nn = temp + .010
L21230:     prtn$ = "kg"
            ngm = nn * 1000
            return

*        And Lastly, over 45000 grams
L21280:     nkg = round(ngm, 8) / 1000
            temp = int(nkg)            /* Round up to even .2 kg level */
            diff = nkg - temp
            frac = (int(diff / .2)) * .2
            diff = diff - frac
            if diff = 0 then nn = temp + frac else nn = temp + frac + .2
            prtn$ = "kg"
            ngm = nn * 1000
            return


*        These are the calculations for liquid measurements
L30100: REM Incomming unit is in milliliters
              nml = n
              gosub round_liquids
              nin = nml
              return

L30200: REM Incomming unit is in liters
              nml = n * 1000
              gosub round_liquids
              nin = nml / 1000
              return

        REM * This is where we round and determine units of liquids *
        round_liquids
*        1st range check is 0 - 1000 milliliters
            if nml > 1000 then L31100
            nn = abs(int(-nml))           /* round up to even 1.0 ml */
            prtn$ = "ml"
            nml = nn
            return

*        2nd range check is from 1001 - 4000 milliliters
L31100:     if nml > 4000 then L31170
            nn = abs(int(-nml  /10) * 10)  /* round up to even 10 ml */
            prtn$ = "ml"
            nml = nn
            return

*        Next range check is from 4001 - 10000 milliliters
L31170:       if nml > 10000 and narc$ = "N" then L31310
                                                   /* Leave narcs in ml*/
            nml = round(nml, 8)          /* round up to even 50 ml */
            temp = int(nml / 100) * 100
            diff = nml - temp
            frac = int(diff / 50) * 50
            diff = diff - frac
            if diff = 0 then nn = temp + frac else nn = temp + frac + 50
            prtn$ = "ml"
            nml = nn
            return


*        Next range is from 10000 milliliters (10 liters) - 100 liters
L31310:     if nml > 1e05 then L31390
            nlt = round(nml, 8) / 1000   /* round up to even .1 liter */
            nn = abs(int(-nlt * 10) / 10)
            prtn$ = "l "
            nml = nn * 1000
            return

*        And lastly, over 100 liters
L31390:     nlt = round(nml, 0) / 1000
            nn = abs(int(-nlt))          /* round up to even liter */
            prtn$ = "l "                      /*  1.0 LT          */
            nml = nn * 1000
            return
