        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDEMP1B                             *~
            *  Creation Date     - 07/25/2017                           *~
            *  Last Modified Date-                                      *~
            *  Description       - This Subroutine Always Calculates    *~
            *                      the Production Year, Week, Day based *~
            *                      on Ent_Date.                         *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *11/12/2012! New Program for (AWD) - Last Mod Date    ! CMN *~
            *************************************************************

            sub "AWDEMP1B" ( ent_yr$,    /* Entry Production Year (OUT)*/~
                             ent_bi_yr$, /* Entry Prod Binary Year(OUT)*/~ 
                             ent_wk$,    /* Entry Prod Week       (OUT)*/~
                             ent_dy$,    /* Entry Production Day  (OUT)*/~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date (10) */~
                             #1,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = Ok, Not Zero error    */

        dim                              /* Subroutine - Variables     */~
            date$8,                      /*                            */~
            x$10, current$10,            /* System Date                */~
            ent_yr$4,                    /* Entry Year                 */~
            ent_bi_yr$2,                 /* Julian Year and Day YYYYDDD*/~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8,       /* Prod Week Date Form/Unform */~
            readkey$24,                  /* GENCODES Readkey           */~
            cur_dte$6,                   /* Unformatted Beg Week       */~
            cur_date$10,                 /* Beg Of Week Date           */~
            end_dte$6,                   /* Unformatted End Week       */~
            end_date$10,                 /* End Of Week Date           */~
            descr$30                     /* Description                */

           init(" ") date$, x$, current$, ent_bi_yr$, ent_yr$, ent_wk$, ~
                     ent_dy$, readkey$, cur_date$, end_date$, descr$, ~
                     cur_dte$, end_dte$

           pl_e% = 99%
           date$ = date
           current$ = date
           dim entry_date$10, entry_dte$6
           entry_date$ = ent_date$   /* Debug */
           entry_dte$  = ent_dte$    /* Debug */
           
         gosub findYrWkDy
         goto exit_sub

        findYrWkDy 
          cmg$ = ent_dte$
          init(" ") readkey$
          str(readkey$,1%,9%) = "WEEKS"
        get_cur_nxt
           read #1, key > readkey$, eod goto noWeeksEmp

             get #1, using L02010, readkey$, descr$

L02010:                     FMT CH(24), CH(30)

              cur_date$ = str(descr$,1%,8%)
              end_date$ = str(descr$,19%,8%)              

              call "DATFMTC" (cur_date$)          /* Format from table */
              x$  = cur_date$
              call "DATUFMTC" (x$)          /* Unformat for comparison */
              cur_dte$ = x$

              call "DATFMTC" (end_date$)         /* Format from table */
              x$  = end_date$              
              call "DATUFMTC" (x$)   /* Unformat for comparison */
              end_dte$ = x$              

              if cur_dte$ > ent_dte$ then goto get_cur_nxt
              if end_dte$ < ent_dte$ then goto get_cur_nxt

                ent_yr$ = str(readkey$,10%,4%)
                convert ent_yr$ to year%, data goto no_entYr
no_entYr:
                ent_bi_yr$ = bin(year%,2%)
                ent_wk$ = str(readkey$,14%,2%) 
                convert ent_wk$ to week%, data goto no_entWk
no_entWk:
                convert week% to ent_wk$, pic(#0)
                gosub assignDay
                pl_e% = 0%
        noWeeksEmp
        return
        

        assignDay
          call "DAY" addr(ent_dte$, day%)

          day% = day% - 1%    /* MONDAY = 1, ETC.  */
          if day% = 0% then day% = 7%
          convert day% to ent_dy$, pic(#)
        return


        exit_sub
          end


