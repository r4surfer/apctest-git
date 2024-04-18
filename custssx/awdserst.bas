        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDSERST -                           *~
            *  Creation Date     - 09/16/2014                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Sanders                     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *09/16/2014! New Subroutine                           ! CMG *~
            *************************************************************

        sub "AWDSERST" (model$,                /* Model Number - IN    */~
                        series$,               /* Series       - OUT   */~
                        style$,                /* Style        - OUT   */~
                        #1,                    /* GENCODES             */~
                        err%)                  /* Error Code   - OUT   */

        dim model$3,                           /* Model Number         */~
            series$16,                         /* Series               */~
            style$10                           /* Style Code           */

        dim readkey$50, desc$30                /* Gencodes Readkey&Desc*/


        err% = -1%

        gosub check_style

        end

        check_style
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "ELLISON05"
          str(readkey$,10%,15%) = model$
          read #1, key > readkey$, using GEN_FMT, readkey$, desc$, ~
                             eod goto style_done
GEN_FMT:   FMT CH(24), CH(30)
           if str(readkey$,10%,3%) <> model$ then goto style_done
           p% = 0%
           p% = pos(desc$ = " ")
           series$ = str(desc$,1%,p%)
           p% = p% + 1%
           style$  = str(desc$,p%,(30%-p%))

           err% = 0%
        style_done
        return


