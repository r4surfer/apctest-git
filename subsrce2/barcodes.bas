        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  BBBB     A    RRRR    CCC    OOO   DDDD   EEEEE   SSS    *~
            *  B   B   A A   R   R  C   C  O   O  D   D  E      S       *~
            *  BBBB   AAAAA  RRRR   C      O   O  D   D  EEEE    SSS    *~
            *  B   B  A   A  R   R  C   C  O   O  D   D  E          S   *~
            *  BBBB   A   A  R   R   CCC    OOo   DDDD   EEEEE   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BARCODES - This subroutine prints the passed in data      *~
            *            barcoded.  Routine will help keep track of     *~
            *            line counters, simialr to txtprint.  See       *~
            *            jbpicksl for sample utilization.               *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/07/94 ! ORIGINAL                                 ! HES *~
            * 11/02/95 ! Added support for HP Lasar & Matrix prtrs! HES *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "BARCODES" (func$,      /* Function to be performed        */~
                        a1$,        /* First value to be barcoded      */~
                                    /* FUNC=I) Accepts Barcode Type    */~
                        c1$,        /* Column it is to print in        */~
                        a2$,        /* Second value to be barcoded     */~
                        c2$,        /* Column it is to print in        */~
                        a3$,        /* Third value to be barcoded      */~
                        c3$,        /* Column it is to print in        */~
                        mask$,      /* Print Line Mask                 */~
                        bl%,        /* Extra Blank lines. neg if B4    */~
                                    /* FUNC=I) Accepts Print Line Lnght*/~
                        line%)      /* Line Count Control Arg          */
                                    /* FUNC=I) Returns Barcode height  */
                                    /* FUNC=P) Accepts & updates your  */
                                    /*         programs line counter   */

        dim a1$99,                       /* Bar Code Variable          */~
            a2$99,                       /* Bar Code Variable          */~
            a3$99,                       /* Bar Code Variable          */~
            bar_code$1,                  /* Bar Code Control Switch    */~
            bar$99,                      /* Printed Bar Code String    */~
            bar1$99,                     /* Work Variable              */~
            border$2,                    /* Work Variable              */~
            c1$3,                        /* Column for 1st bar code    */~
            c2$3,                        /* Column for 2nd bar code    */~
            c3$3,                        /* Column for 2nd bar code    */~
            ec$1,                        /* Escape Character for Bars  */~
            first_line$50,               /* String For Beginning Of A  */~
                                         /* Print File (barcode setup) */~
            last_line$50,                /* String Indicating The End  */~
                                         /* Of A Print File            */~
            func$1,                      /* Barcode Routine Function Cd*/~
            mask$132                     /* Print Line Mask For Barcods*/~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto L10000
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "

L10000: REM *************************************************************~
            *                                                           *~
            *                  Release Version ID Section               *~
            *                                                           *~
            *************************************************************

            if func$  = "I" then init_for_barcodes
            if func$  = "E" then close_for_barcodes
            if func$ <> "P" then end
            bar$ = " "
            if bar_code$ = "B" then L10220  /* Bear Rock PC systm */

            REM Convert any special characters to 'Full code 39'...
            bar$ = a1$ : gosub trans_late : a1$ = bar$
            bar$ = a2$ : gosub trans_late : a2$ = bar$
            bar$ = a3$ : gosub trans_late : a3$ = bar$
            bar$ = " "

            if bar_code$ = "M" then L10490  /* HP Matrix (HP256x)*/
            if bar_code$ = "L" then L10220  /* HP Lazerjet       */
            end

L10220:     REM Bear Rock and HP Lazerjet Logic...
            if a1$ = " " or str(a1$,1,1) = "_"  then L10280
            convert c1$ to c1%
            str(bar$,c1%) = border$
            bar$ = bar$ & a1$
            bar$ = bar$ & border$
L10280:     if a2$ = " " or str(a2$,1%,1%) = "_"  then L10330
               convert c2$ to c2%
               str(bar$,c2%) = border$
               bar$ = bar$ & a2$
               bar$ = bar$ & border$
L10330:        if a3$ = " " or str(a3$,1%,1%) = "_"  then L10380
                  convert c3$ to c3%
                  str(bar$,c3%) = border$
                  bar$ = bar$ & a3$
                  bar$ = bar$ & border$
L10380:     if bar$ = " " then end
               if bar_code$ <> "L" then L10420
                  first_line$ = ec$ & "(0Y" & ec$ & "(s0p8.1h12v0s0b0T"
                  print first_line$
L10420:        if bl% < 0% then print skip(abs(bl%))
               if bsiz% = 1% then L10650
                  for i% = 1% to bsiz% - 1%
                      print bar$
                  next i%
                  goto L10650

L10490:     REM A1$/A2$ are strings to bar code; C1$/C2$ are column pos.
            if a1$ = " " or str(a1$,1,1) = "_"  then end
            bar$ = ec$ & "*z" & c1$
            bar$ = bar$ & "c<" & a1$
            bar$ = bar$ & ">"
            if a2$ = " " or str(a2$,1%,1%) = "_"  then L10580
               bar$ = bar$ & "z" & c2$
               bar$ = bar$ & "c<" & a2$
               bar$ = bar$ & ">"
L10580:        if a3$ = " " or str(a3$,1%,1%) = "_"  then L10620
                  bar$ = bar$ & "z" & c3$
                  bar$ = bar$ & "c<" & a3$
                  bar$ = bar$ & ">"
L10620:     bar$ = bar$ & "Z"
            if bl% = -1% then print skip(1)

L10650:     print bar$
            if bar_code$ <> "L" then L10690
               first_line$ = ec$ & "(19U" & ec$ & "(s0p10h0s0b4099T"
               print first_line$
L10690:     if bl% > 0% then print skip(bl%)
            line% = line% + bsiz% + abs(bl%)
            end

         init_for_barcodes
            select printer (bl%)
            bsiz%, line% = 0%
            bar_code$ = a1$
            if bar_code$ = "Y" then bar_code$ = "B" /* Force to  */
                                                    /* Bear Rock */
            if pos("MBLN" = bar_code$) = 0% then bar_code$ = "N"

            if bar_code$ <> "M" then L10900
               REM Control strings for HP 256X and 2300 series printers
               ec$ = hex(1b)
               first_line$ = ec$ & "*z0v5h0Q"
               last_line$ = ec$ & "E"
               print first_line$
               bsiz%, line% = 3%
               end

L10900:     if bar_code$ <> "L" then L10980
               border$ = "*"
               ec$ = hex(1b)
               last_line$ = ec$ & "Z"
*             PRINT LAST_LINE$
               bsiz%, line% = 3% : line% = 4
               end

L10980:     if bar_code$ <> "B" then end
               border$ = "@@" :print "HDHDHDHD"
               bsiz%, line% = 3%
               end

         trans_late
L11040:     x% = pos(bar$ = "*")
            if x% = 0% or x% > 98% then return
               bar1$ = bar$
               str(bar$,x%) = "/J" & str(bar1$,x%+1%)
               goto L11040

         close_for_barcodes
            if bar_code$ = "M" then print last_line$
            if bar_code$ = "L" then print last_line$
            end
