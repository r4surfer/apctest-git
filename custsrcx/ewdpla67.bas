        REM (EWDPLA67) Fix Warranty Id's for History
        REM    Only the warranty Id's for Sales Orders found in (EWDHIST)
        REM    are moved. This utility should only run after the applicable
        REM    years history has been moved into the consolidated history
        REM    database.
        REM    
        REM    In 2001 when 1999 is moved to history do more research on 
        REM    reason for Skipped records.
        REM 
        REM As of 03/06/2001    (May no longer need to correct Problem)
        REM As of 06/09/2006    Problem may be associated with the 
        REM                     wr$() Array size, max now is 32765 was 500
        REM                     (AWD001)

        dim wt_key$8, wt_rec$128, rslt$20, wt1_key$15, wt1_rec$32,        ~
                     so_key$15, text$70, rhh$3
                                              /* (APCPLNWT) Renamed    */
                                              /* (AWD001)              */
            select #1,  "APCPLNXX",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   1,  keylen =   8,                     ~
                        alt key 1, keypos =  9, keylen = 10, dup,        ~
                            key 2, keypos =  9, keylen = 18

                                              /* (AWD001)              */

            select #2,  "EWDWARR",                                       ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =   1,  keylen =  15,                     ~
                        alt key 1, keypos =  16, keylen = 15, dup

            select #3,  "EWDHIST",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   17, keylen =  32,                     ~
                        alt key  1, keypos =   54, keylen =  15,         ~
                            key  2, keypos =   49, keylen =  20,         ~
                            key  3, keypos =   77, keylen =  26,         ~
                            key  4, keypos =    1, keylen =  25, dup,    ~
                            key  5, keypos =  193, keylen =   8, dup 

             init(" ") rslt$
            call "OPENCHCK" (#1, x%, y%,  0%, rslt$)

            init(" ") rslt$ : x%, y% = 0%
            call "OPENCHCK" (#2, x%, y%,  0%, rslt$)

            init(" ") rslt$ : x%, y% = 0%
            call "OPENCHCK" (#3, x%, y%,  0%, rslt$)
            x%, y% = 0%

            text$ = "Srch=[xxxxxxxx] Mov=[xxxxxxxx] Skp=[######] [######] Rep=[XXXXXXX]"

         display                                                         ~
            at(01,21), "Transfering (Warranty's) from (APCPLNWT) to (EWDWARR)."

            cnt% = 0%  : cnt1% = 0% 
            cnt2% = 0% : cnt3% = 0% : cnt4% = 0%
            wt_key$ = all(hex(00))
                                             /* (AWD001)               */
        next_rec                             /* (APCPLNWT) - Read Id's */
            init(" ") so_key$, rhh$, wt_rec$, wt1_rec$
            read #1,hold,key 0% > wt_key$,using L00100, wt_rec$,            ~
                                                     eod goto scan_done 
L00100:         FMT CH(128)
                                             /* (AWD001)               */
            cnt% = cnt% + 1% 
            wt_key$ = str(wt_rec$,1%,8%)        /* Get Warranty Id No. */ 
            str(so_key$,1%,8%) = str(wt_rec$,9%,8%) /* Save Sales Ord  */  
            rhh$ = str(wt_rec$,17%,2%)          /* S.O. Line Item No.  */
            rhh% = 1%                           /* Default Line Item 1 */
            convert rhh$ to rhh%, data goto L00110
L00110:
            convert rhh% to rhh$,pic(###)       /* Convert to 3 Digit  */
            str(so_key$,9%,3%) = rhh$           /* Line Item for EWDWARR */

            if mod(cnt%,500) <> 0 then goto L00120
                                                /* Records Searched    */  
               convert cnt%  to str(text$,7%,8%),  pic(########)
                                                /* New Warranty Record */
               convert cnt1% to str(text$,22%,8%), pic(########)
                                                /* Skipped Rec's       */ 
               convert cnt2% to str(text$,37%,6%), pic(######)
                                                /* Not on File         */
               convert cnt3% to str(text$,46%,6%), pic(######)
                                                /* Replaced Rec's      */
               convert cnt4% to str(text$,59%,7%), pic(#######)

               print at(10,05);hex(84);text$;
                                                /* Lookup Sales Order  */
                                                /* and Line Item in    */
                                                /* History (EWDHIST)   */
L00120:     read #3,key 1% > so_key$, using L00130, so_key$, eod goto not_on_file
L00130:        FMT POS(54), CH(15)
                                           /* (EWDHIST) to get the Year  */ 
            if str(so_key$,1%,8%) <> str(wt_rec$,9%,8%) then goto not_on_file

                                           /* Create New Warranty Record */
               str(wt1_rec$,1%,8%)  = wt_key$       /* Warranty Number   */
               str(wt1_rec$,9%,3%)  = rhh$          /* Line Item         */
               str(wt1_rec$,12%,4%) = str(so_key$,12%,4%) /* Year        */
               str(wt1_rec$,16%,15%)= str(so_key$,1%,15%) /* S.O. Alt Key*/

            wt1_key$ = str(wt_rec$,1%,15%) /* (EWDWARR) - New Warranty   */
            read #2,hold,key = wt1_key$, eod goto L00140
               delete #2
               cnt4% = cnt4% + 1%

L00140:     put #2, using L00100, wt1_rec$
            write #2, eod goto skip_rec
            cnt1% = cnt1% + 1%                      /* Warranty Records  */
            goto next_rec                           /* (New)             */ 
        scan_done

               convert cnt%  to str(text$,7%,8%),  pic(########)
               convert cnt1% to str(text$,22%,8%), pic(########)
               convert cnt2% to str(text$,37%,6%), pic(######)
               convert cnt3% to str(text$,46%,6%), pic(######)
               convert cnt4% to str(text$,59%,7%), pic(#######)

               print at(10,05);hex(84);text$;"Done?"
               stop

        end

        skip_rec
            cnt2% = cnt2% + 1%
            goto next_rec

        not_on_file
            cnt3% = cnt3% + 1%
            goto next_rec

