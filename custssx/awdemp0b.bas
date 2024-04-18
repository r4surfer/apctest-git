        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDEMP0B                             *~
            *  Creation Date     - 11/12/2012                           *~
            *  Last Modified Date- 08/03/2018                           *~
            *  Description       - This Subroutine Always Calculates    *~
            *                      the Current Production Year, Week,   *~
            *                      and Day. The Routine will also       *~
            *                      Validate the Entered Year, Week, and *~
            *                      optional Day. Formatted and          *~
            *                      unformatted dates are returned for   *~
            *                      both, also Prior year Valus.         *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *11/12/2012! New Program for (AWD) - Last Mod Date    ! CMG *~
            *08/03/2018! (CR1489) Mod to use prod WEEKS for Hours ! CMN *~            
            *************************************************************

            sub "AWDEMP0B" ( cur_yr$,    /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr$,    /* Entry Production Year (IN) */~
                             ent_wk$,    /* Entry Prod Week       (IN) */~
                             ent_dy$,    /* Entry Production Day (OPT) */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #1,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = Ok, Not Zero error    */

        dim                              /* Subroutine - Variables     */~
            date$8,                      /*                            */~
            x$10, current$10,            /* System Date                */~
            dy$(7%)9,                    /* Days and Day of Week       */~
            prv_yr$2,  cur_yr$2,         /* Previous / Current Year    */~
            ent_year$4, cur_year$4,      /* Previous / Current Year    */~        
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$8,       /* Prod Week Date Form/Unform */~
            ent_yr$2,                    /* Julian Year and Day YYYYDDD*/~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8,       /* Prod Week Date Form/Unform */~
            readkey$24,                  /* GENCODES Readkey           */~
            end_date$10,                 /* End Of Week Date           */~
            descr$30                     /* Description                */

            dy$(2%) = "MONDAY   " : dy$(6%) = "FRIDAY   "
            dy$(3%) = "TUESDAY  " : dy$(7%) = "SATURDAY "
            dy$(4%) = "WEDNESDAY" : dy$(1%) = "SUNDAY   "
            dy$(5%) = "THURSDAY "


            date% = 0%
            pl_e% = 0%
            date$ = date 
            current$ = date

            if  str(ent_yr$,1%,2%) = " " then gosub get_current
            convert val(ent_yr$,2) to ent_year$, pic(0000)

            if ent_yr$ <> " " and ent_wk$ <> " " then gosub get_ent
            if ent_yr$ = " "  or  ent_wk$ = " " then gosub get_cur
               if date% <> 1% then goto exit_err1
                goto exit_sub







        exit_err1 : pl_e% = 1% : goto exit_sub   /* Invalid Entry Week */

REM        EXIT_ERR2 : PL_E% = 2% : GOTO EXIT_SUB   /* INVALID ENTRY DAY  */
            
        exit_sub


        end



        get_ent
          ent% = 0%
          convert ent_wk$ to week%, data goto no_ent

          convert week% to ent_wk$, pic(00)

          init(" ") readkey$
REM STR(READKEY$,1%,9%) = "WEEKSEMP"
          str(readkey$,1%,9%) = "WEEKS"     /* (CR1489) */
          convert val(ent_yr$,2) to str(readkey$,10%,4%), pic(0000)
REM STR(READKEY$,10%,4%) = ENT_YR$
          str(readkey$,14%,2%) = ent_wk$

          read #1, key = readkey$, eod goto no_ent
                
             get #1, using L02000, descr$
L02000:                   FMT POS(25), CH(30)

               ent_date$ = str(descr$,1%,8%)
               call "DATFMTC" (ent_date$)
               call "DATUFMTC" (ent_date$)

               ent_dte$ = ent_date$

               call "DATFMTC" (ent_date$)

               ent_year$ = str(readkey$,10%,4%)
               ent_wk$   = str(readkey$,14%,2%)

               convert ent_wk$ to week%, data goto no_ent

               convert week% to ent_wk$, pic(#0)

               ent% = 1%
               gosub get_cur
               ent% = 0%
REM                       GOSUB ASSIGN_WEEK
               date% = 1%
        no_ent            
        return


        get_cur
          init(" ") readkey$
REM STR(READKEY$,1%,9%) = "WEEKSEMP"   /* (CR1489) */    
          str(readkey$,1%,9%) = "WEEKS"                
        get_cur_nxt
           read #1, key > readkey$, eod goto no_cur
                
             get #1, using L02010, readkey$, descr$

L02010:                     FMT CH(24), CH(30)
              
               cur_date$ = str(descr$,1%,8%)
               end_date$ = str(descr$,19%,8%) 

               call "DATFMTC" (cur_date$)         /* Format from table */
               call "DATUFMTC" (cur_date$)  /* Unformat for comparison */
               cur_dte$ = cur_date$

               call "DATFMTC" (cur_date$)         /* Format again */
REM                       CMG$  = CUR_DATE$
REM                       CMG1$ = CUR_DTE$
               call "DATFMTC" (end_date$)         /* Format from table */
               call "DATUFMTC" (end_date$)  /* Unformat for comparison */
REM                       CMG$ = END_DATE$

               if cur_dte$ > date$ then goto get_cur_nxt

                  if end_date$ < date$  then goto get_cur_nxt 

                   cur_year$ = str(readkey$,10%,4%)
                   convert cur_year$ to year%, data goto no_curr

                   cur_yr$ = bin(year%,2%)
                                                /* Week              */

                   cur_wk$ = str(readkey$,14%,2%)         
                   convert cur_wk$ to week%, data goto no_cur

                   convert week% to cur_wk$, pic(#0)

                   gosub assign_week

                   if ent% = 1% then return

                   ent_year$ = cur_year$
                   ent_yr$   = cur_yr$
                   ent_wk$   = cur_wk$
                   ent_date$ = cur_date$         
                   ent_dte$  = cur_dte$
               date% = 1%
        no_cur             
        return


        assign_week
          gosub find_day
          ent_year% = 0%

          convert ent_year$ to ent_year%, data goto L02050

L02050:
          yy% = ent_year%                         /* Save Integer of   */
          ent_yr$ = bin(yy%,2)                    /* Save Current Year */
          yy% = yy% - 1%                          /* Previous Year     */
          prv_yr$ = bin(yy%,2)                    /* Previous year     */
        
        
REM            ASSIGN_DONE
        return

        find_day
          if ent_dy$ <> " " then cur_dy$ = ent_dy$
          if ent_dy$ <> " " then return

          call "DAY" addr(date$, day%)
REM SUNDAY is now 1          /* (CR1489) */
          day% = day% - 1%    /* So Monday = 1, etc.  */
          if day% = 0% then day% = 7%
          convert day% to ent_dy$, pic(#)
          cur_dy$ = ent_dy$
        return

        get_current
          leap% = 0%
          call "DATEFMT" (current$,leap%,x$) 
          convert str(x$,1%,4%) to year%, data goto no_curr
 


          ent_yr$ = bin(year%,2)
        no_curr
        return

