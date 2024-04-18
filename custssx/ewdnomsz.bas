        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDNOMSZ                             *~
            *  Creation Date     - 04/23/99                             *~
            *  Last Modified Date- 07/28/99                             *~
            *  Description       - This Subroutine Calculates the       *~
            *                      Nominal Size for Exact or Opening    *~
            *                      Size.                                *~
            *                                                           *~
            *  Special Comments  - Calls subroutine APCPR5SB (if needed)*~
            *                      to calculate the Opening Size.       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/23/99 ! Original.                                ! BWS *~
            * 07/14/99 ! (EWD001) Review Routine Used when printing! RHH*~
            *          !   the new Production Labels.             !     *~    
            *************************************************************

          sub "EWDNOMSZ" (size$,        /* (E)xact or (O)pening        */~
                          partin$,      /* MFG Part Exact or Opening   */~
                          part_o$,      /* Opening Part No. - OUT      */~
                          nomsz$,       /* Nominal Size (Formatted)    */~
                          cuscode$,     /* Customer Code               */~
                          #1,           /* (APCPCMST) - File           */~
                          #2,           /* (GENCODES) - File           */~
                          err% )        /* Error Non Zero Value        */

        dim size$1,                      /* (E)xact or (O)pening       */~
            partin$25,                   /* Passed-In Part Number      */~
            part$25,                     /* Part Number Always Exact   */~
            part_o$25,                   /* Part Number Always Opening */~
            nomsz$7,                     /* Nominal Size Fmtd (www hhh)*/~
            cuscode$9,                   /* Customer Code              */~
            w$4, h$3,                    /* Width & Height             */~
            pc_c$4,                      /* Pricing Catalog Code       */~
            pc_cm$2                      /* Pricing Catalog Method Code*/~


        part_o$ = partin$
        if size$ = "O" then goto L00100
            part$ = partin$
            part_o$ = " "                /* (EWD001) - Remove cuscode$  */
            sp% = 0%
            pc_c$ = "0000"  :  pc_cm$ = "00"
            call"APCPR5SB" (size$,       /* (E)xact or (O)pening        */~
                            part$,       /* MFG Part Always Exact       */~
                            part_o$,     /* MFG Part Always Opening     */~
                            cuscode$,    /* Customer Code               */~
                            sp%,         /* Special Flag 0%=Cat,1%=Spc  */~
                            pc_c$,       /* Special Catalog Code or 0000*/~
                            pc_cm$,      /* Special Catalog Method or 00*/~
                            #1,          /* (APCPCMST) - File           */~
                            #2,          /* (GENCODES) - File           */~
                            err% )       /* Error Non Zero Value        */
            if err% <> 0% then end

L00100: nomsz$ = " "

        w$ = str(part_o$,13%,3%)
        h$ = str(part_o$,17%,2%)

        convert w$ to w%, data goto calc_err
        convert h$ to h%, data goto calc_err

        if str(cuscode$,1%,2%) = "LO" and str(part_o$,19%,1%) <> "0" ~
                                          then h% = h% + 1%
        w1% = int(w% / 12%)
        h1% = int(h% / 12%)

        w2% = w% - (w1% * 12)
        h2% = h% - (h1% * 12)

        convert w1% to str(nomsz$,1%,1%), pic(0)
        convert w2% to str(nomsz$,2%,2%), pic(#0)
        call "SPCESMSH" (str(nomsz$,1%,3%),0%)
        convert h1% to str(nomsz$,5%,1%), pic(0)
        convert h2% to str(nomsz$,6%,2%), pic(#0)
        call "SPCESMSH" (str(nomsz$,5%,3%),0%)

        end


    calc_err
        err% = 99%
        end


