*       ****************************************************************~
*                           ( As of 11/07/97 - RHH )                   *~
*        APCCUTLD - Load All Equation Numbers and Column Headings      *~
*                   Associated with a Specified Product Line and       *~
*                   For Width and Height Type Code.                    *~
*                                                                      *~
*                     PD$  = Product Line                              *~
*                     CW%  = Number of Width Calcs          1st        *~
*                     CH%  = Number of Height Calcs         2nd        *~
*                  ** TW$  = Width Calc Type Code Parts                *~
*                  ** TH$  = Height Calc Type Code Parts               *~
*                     EQ%  = (CW% + CH% )                 Total Calcs  *~
*                                                                      *~
*        Note - (**) Type Codes ( 1(W) & 2(H) ) Linealmate / Saw Cuts  *~
*                               ( A(W) & B(H) ) Costing Misc Parts     *~
*                               ( C(W) & D(H) ) Costing Grid/Liting    *~
*                                                                      *~
*       ****************************************************************

        sub "APCCUTLD" (pd$,             /* Product Line              */ ~
                        cw%,             /* Number of Width Cuts      */ ~
                        ch%,             /* Number of Heights Cuts    */ ~
                        tw$,             /* Width Type Code           */ ~
                        th$,             /* Height Type Code          */ ~
                        #1,              /* (GENCODES)TABLES          */ ~
                        err% )           /* 0%=Ok, Non-Zero Error     */

        dim pd$1,                        /* Product Line              */ ~
            readkey$24,                  /* GENCODES - Key            */ ~
            tw$1,                        /* Width Type Code           */ ~
            th$1,                        /* Height Type Code          */ ~
            type$1,                      /* Type of Eq. (TW$ or TH$)  */ ~
            save_key$10                  /* Save Product Key          */

            init(" ") readkey$, save_key$
            err% = 1%
            cw%, ch% = 0%
            readkey$ = all(hex(00))
            str(readkey$,1%,9%)  = "EQUATIONS"
            str(readkey$,10%,1%) = pd$                /* Product Line */
            save_key$ = str(readkey$,1%,10%)
            read #1,key > readkey$, using L00470, readkey$,                 ~
                                                       eod goto read_done
           goto L00480
        read_next
           read #1, using L00470, readkey$, eod goto read_done
L00470:       FMT CH(24)
L00480:    if str(readkey$,1%,10%) <> save_key$ then read_done
              if str(readkey$,14%,2%) = "00" then goto read_next
              type$ = str(readkey$,12%,1%)
              if type$ = tw$ then cw%  = cw% + 1%       /* Width       */
              if type$ = th$ then ch%  = ch% + 1%       /* Height      */
              goto read_next
        read_done
          if (cw% + ch%) <> 0% then err% = 0%
        end
