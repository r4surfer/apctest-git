*       ****************************************************************~
*                           ( As of 03/31/06 - CMG )                   *~
*        AWDCUTLD - Load All Equation Numbers and Column Headings      *~
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

        sub "AWDCUTLD" (pd$,             /* Product Line              */ ~
                        cw%,             /* Number of Width Cuts      */ ~
                        ch%,             /* Number of Heights Cuts    */ ~
                        csw%,            /* Num of Width Cut Subpart  */ ~ 
                        csh%,            /* Num of Height cut subpart */ ~
                        tw$,             /* Width Type Code           */ ~
                        th$,             /* Height Type Code          */ ~
                        tsw$,            /* Width Type code subpart   */ ~
                        tsh$,            /* Height Type code subpart  */ ~
                        #1,              /* (GENCODES)TABLES          */ ~
                        err% )           /* 0%=Ok, Non-Zero Error     */

        dim pd$1,                        /* Product Line              */ ~
            readkey$24,                  /* GENCODES - Key            */ ~
            rw$,                         /* Readkey type width code   */ ~
            rh$,                         /* readkey type height code  */ ~
            tw$1,                        /* Width Type Code           */ ~
            th$1,                        /* Height Type Code          */ ~
            type$1,                      /* Type of Eq. (TW$ or TH$)  */ ~
            save_key$10,                 /* Save Product Key          */ ~
            tsw$1,                       /* Width Type subpart        */ ~
            tsh$1                        /* Height type subpart       */ 

            init(" ") readkey$, save_key$, rw$, rh$
            err% = 1%
            rw%, rh% = 0%
            cw%, ch% = 0%
            csw%, csh% = 0%

            rw$ = tw$
            rh$ = th$
            gosub get_equations
            cw% = rw%
            ch% = rh%

            rw%, rh% = 0%
            rw$ = tsw$
            rh$ = tsh$
            gosub get_equations
            csw% = rw%
            csh% = rh%


           goto exit_sub

        get_equations

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
              if type$ = rw$ then rw%  = rw% + 1%       /* Width       */
              if type$ = rh$ then rh%  = rh% + 1%       /* Height      */
              goto read_next
        read_done
          if (rw% + rh%) <> 0% then err% = 0%

        return


        exit_sub
        end


