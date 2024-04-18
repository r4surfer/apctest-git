        REM *************************************************************~
            *                                                           *~
            *  Program Name      - PRCSERST -                           *~
            *  Creation Date     - 12/29/2015                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Norman                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *12/29/2015! New Subroutine                           ! CMG *~
            *************************************************************

        sub "PRCSERST" (model$,                /* Model Number - IN    */~
                        series$,               /* Series       - OUT   */~
                        style$,                /* Style        - OUT   */~
                        #1,                    /* GENCODES             */~
                        err%)                  /* Error Code   - OUT   */

        dim model$3,                           /* Model Number         */~
            series$16,                         /* Series               */~
            style$10                           /* Style Code           */

        dim readkey$50, desc$30                /* Gencodes Readkey&Desc*/


        err% = -1%
        init(" ") series$, style$, readkey$, desc$
        gosub check_style

        end

        check_style
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "PRICESERI"
          str(readkey$,10%,15%) = model$
          read #1, key = readkey$, using GEN_FMT, readkey$, desc$, ~
                             eod goto style_done
GEN_FMT:   FMT CH(24), CH(30)
           p% = 0%
           p% = pos(desc$ = " ")
           series$ = str(desc$,1%,p%)
           p% = p% + 1%
           style$  = str(desc$,p%,10%)

           err% = 0%
        style_done
        return


