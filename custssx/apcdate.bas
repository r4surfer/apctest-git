        REM *************************************************************~
            *                                                           *~
            * (APCDATE) - APC Subroutine to set Sales Order default     *~
            *             dates.                                        *~
            * Note (1)  - Mod 11/17/97 - Change Date after 11PM -       *~
            *                            Line - 360                     *~
            *      (2)  - Used by (BCKFASTR), (APCEDIPO), (APCEDIQT)    *~
            *                                                           *~
            *                                                           *~
            *************************************************************~
            * 03/31/98  ERN  Y2K modifications                          *~
            *************************************************************

            /* Note - def_dte$ is passed in and returned in packed fmt */

            sub "APCDATE" (def_dte$)        /* Calculated Default DATE */

        dim days$(7%)9,                       /* DAYS OF THE WEEK */     ~
            day$9,                            /* DAY OF THE WEEK      */ ~
            dte1$6,                           /* SAVE TODAYS DATE     */ ~
            dte2$6,                           /* SAVE THRUDAYS DATE   */ ~
            dte3$6,                           /* CALCULATED DATE      */ ~
            def_dte$6,                        /* DEFAULT DATE OUT     */ ~
            time$8                            /* SAVE CURRENT TIME    */

            days$(1%) = "THURSDAY "           /* Day of the week table*/
            days$(2%) = "FRIDAY   "
            days$(3%) = "SATURDAY "
            days$(4%) = "SUNDAY   "
            days$(5%) = "MONDAY   "
            days$(6%) = "TUESDAY  "
            days$(7%) = "WEDNESDAY"

        REM call "DATUNFMT" (def_dte$)   /* Get Current Date and Time */

            dte1$ = str(def_dte$,1%,6%)  /* APC Mod - 08/27/90        */

            time$ = time
                                         /* CONVERT TIME TO INTEGER   */
            convert time$ to time%, data goto L00330
                                         /* GET THE DAY OF THE WEEK   */
L00330:     call "DATE" addr("GD", dte1$, day$, err%)

            for i% = 1% to 7%          /* FIND PROPER DATE ADJUSTMENT */
              if str(days$(i%)) = str(day$) then inc% = i% - 1%
            next i%
            dte2$ = dte1$              /* SET DATES EQUAL IN THE EVENT*/
                                       /* THAT TODAY IS THURSDAY.     */
            if inc% = 0% and time% > 23000000% then goto L00450
            if inc% = 0% then inc% = 7%
            inc% = inc% * (-1)         /* SET PROPER DATE ADJUSTMENT  */
            call "DATE" addr("G+", dte1$, inc%, dte2$, err%)
                                       /* CALCULATE DEFAULT DATE      */
L00450:     call "DATE" addr("G+", dte2$, +18%, dte3$, err%)
            def_dte$ = dte3$

          end
