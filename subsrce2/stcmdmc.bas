        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS   TTTTT   CCC   M   M  DDDD   M   M   CCC           *~
            *  S        T    C   C  MM MM  D   D  MM MM  C   C          *~
            *   SSS     T    C      M M M  D   D  M M M  C              *~
            *      S    T    C   C  M   M  D   D  M   M  C   C          *~
            *   SSS     T     CCC   M   M  DDDD   M   M   CCC           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCMDMC  - Calculate MDMC for a part.                     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/23/91 ! Original - Grabbed from GSC (STCROLUP)   ! KAB *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "STCMDMC" (part$, set$, totals(), mdmc, #1, #2, #3)

            dim                                                          ~
               cat$4,                 /* Category Code                 */~
               mdmc_on$1,             /* Part Code                     */~
               part$25,               /* Part Code                     */~
               readkey$50,            /* General Purpose Read Key      */~
               set$8,                 /* Cost Set ID                   */~
               total(1),              /* Adjusted Standard Costs       */~
               totals(12),            /* Standard Costs                */~
               work(1,12)             /* Factors                       */

        REM *************************************************************~
            * Initialization                                            *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L09042
            cms2v$ = "R6.01.00 10/07/91 CMS General Release            "
L09042: REM *************************************************************
            if mdmc_on$ <> " " then L10000

            /* See if Management Reporting is on */
            mdmc = -1      /* -1 is a flag that Mgt Rptng is off */
            mdmc_on$ = "N"
            call "READ100" (#3, "SWITCHS.GL", f1%)
            if f1% = 1% then get #3 using L09110, mdmc_on$
L09110:         FMT POS(59), CH(1)
            if mdmc_on$ <> "Y" then mdmc_on$ = "N"

*       *** The following is unnecessary if we go to 12 factors

            /* Get the Intercompany Cost Bucket ID for this cost set */
            id% = 0%
            readkey$ = str(set$) & "B" & hex(00)
            call "PLOWNEXT" (#2, readkey$, 9%, f1%)
               if f1% =  1% then get #2 using L09210, id%
L09210:           FMT POS(20), BI(1)
            if id% <  1% then mdmc_on$ = "N"
            if id% > 12% then mdmc_on$ = "N"

L10000: REM *************************************************************~
            * Process                                                   *~
            *************************************************************
            mdmc = -1      /* -1 is a flag that Mgt Rptng is off */
            if mdmc_on$ <> "Y" then exit_program

            readkey$ = str(part$,,25)
            mat work = con  /* Start with Management & Corp Cost equal */

            /* Now get the factors for the part category               */
            /* First determine the part category                       */
            call "READ100" (#1, readkey$, f1%)
               if f1% <> 1% then L10500   /* OUCH!!! */
            get #1 using L10150, cat$
L10150:         FMT POS(90), CH(4)

            /* Second determine the billing & transfer factors         */
            readkey$ = str(set$,,8) & "C" & str(cat$) & hex(00)
            call "PLOWNEXT" (#2, readkey$, 13%, f1%)
               if f1% <> 1% then L10500   /* No factors on file */
*       *** Execute the following if 12 factors on file
*          GET #2 USING 10230, WORK()
*              FMT POS(???), 12*PD(14,4)
*       *** And bypass this to 10500
            get #2 using L10260, billing, transfer
L10260:         FMT POS(20), 2*PD(14,4)
            if billing = 0 /* OR TRANSFER = 0 */ then L10500
            work(1,id%) = transfer / billing   /* Why Xfer = 0 ? */

*       *** Finally, Let's do it.

L10500:     mat total = work * totals  /* use the factors.           */

            mdmc = round(total(1), 4)

                    /* Whew, we determined the Management DMC */

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
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program

            end
