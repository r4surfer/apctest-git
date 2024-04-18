        REM *************************************************************~
            *                                                           *~
            *                         APCROLEZ                          *~
            *                    Copy of APCOLEX                        *~
            *************************************************************~
            *   Date    ! Description of Modifications            ! WHO *~
            *-----------!-----------------------------------------!-----*~
            * 04/09/2018! Original                                ! RDB *~
            * 02/11/2019! CR-1894 Increase Emp Dept field         ! DES *~
            * 04/30/2020! CR2490  Employee Number size increase   ! RDB *~
            * 06/24/2020! CR2614  Array size increase             ! RDB *~
            *************************************************************
/* called by DSDPLN07 */

            call "SHOSTAT" ("Loading (R o l o d e x )")

        dim                                                              ~
            rolodex$30, total$6,         /* Rolodex Screen             */~
/*EWD001*/  scr$(4000%)79,               /* ROLODEX TEXT     CR2614    */~
/*EWD002*/  user_auth%,                  /* security check             */~
            readkey$50,                  /* Table Key                  */~
            desc$32,                     /* Table Description          */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            emp_key$30,                  /* Employee Alt Key           */~
            e_hacd$3,                    /* Home Area Code             */~
            e_hphone$7,                  /* Home Phone                 */~
            e_eacd$3,                    /* Emerg Area Code            */~
            e_ephone$7,                  /* Emerg Phone                */~
            e_dept$3,                    /* Department Code            */~
/*EWD001*/  find_name$15,                /* Last Name to Find          */~
/*EWD001  CR2490 */                                                      ~
            find_numb$8,                 /* Emp. No. to Find           */~
            msk_hphone$14,               /* FORMAT HOME PHONE          */~
            msk_ephone$14,               /* FORMAT EMERG PHONE         */~
            rec$195,                     /* PARTIAL EMP REC            */~
            temp$3,                      /* for debugging              */~
            userid$3                     /* Current User Id            */

        dim f2%(2%),                     /* = 0 if the file is open    */~
            f1%(2%),                     /* = 1 if READ was successful */~
            fs%(2%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(2%)20                  /* Text from file opening     */

            mat f2% = con

            select #1,  "APCEMPLY",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos = 7,    keylen =  5,                      ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos =   12, keylen =  26, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            call "OPENCHCK" (#01, fs%(01), f2%(01),200%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02),      0%,  0%, rslt$(02))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            fieldnr%, u3% = 0%

            h1$ = "Emp No."
            h2$ = "Last Name      "
            h3$ = "First Name"
            h4$ = "I"
            h5$ = "Home Phone    "
            h6$ = "Emerg Phone   "
            h7$ = " Depart. "

            init(" ") scr$(), emp_key$, total$

        REM ************************************************************

            gosub rolodex

            end

        REM ************************************************************

        rolodex
            i%, i_max% = 0%
            emp_key$ = all(hex(00))
            read #1,key 2% > emp_key$, using L00890  , rec$,                ~
                                                    eod goto rolodex_done
            goto L00900
        rolodex_next
            read #1, using L00890  , rec$, eod goto rolodex_done
L00890:       FMT CH(195)
L00900:     if str(rec$,152%,1%) <> "A" then goto rolodex_next
            if str(rec$,1%,3%) <> "060" and str(rec$,1%,3%) <> "018" ~
                    then goto rolodex_next
            i% = i% + 1%
            if i% > 4000% then i% = 4000%          /* CR2614 */
            e_dept$               = str(rec$,1%,3%)
REM            str(scr$(i%),1%,8%)   = str(rec$,7%,5%)
/* CR2490 */
            get str(rec$,7%,4%), using L11111, conv_number%
L11111:             FMT BI(4)
            convert conv_number% to str(scr$(i%),1%,8%), pic(#####000)
            call "SPCSMASH" (scr$(i%))
            str(scr$(i%),11%,15%)  = str(rec$,12%,15%)
            str(scr$(i%),27%,10%) = str(rec$,27%,10%)
            str(scr$(i%),38%,1%)  = str(rec$,37%,1%)
/* EWD002 */ 
    
            user_auth% = 0%
	    gosub check_permission
REM         user_auth% = 0% if not authorised.
            e_hacd$   = "   "                
            e_hphone$ = "       "        
            e_eacd$   = "   "
            e_ephone$ = "       "
	    if user_auth% = 0% then goto L00950
            e_hphone$ = str(rec$,130%,7%)
            e_ephone$ = str(rec$,187%,7%)
            e_hacd$   = str(rec$,127%,3%)
            e_eacd$   = str(rec$,184%,3%)

L00950:     msk_hphone$ = "(   )    -    "
            msk_ephone$ = "(   )    -    "
            str(msk_hphone$,2%,3%)  = e_hacd$
            str(msk_hphone$,7%,3%)  = str(e_hphone$,1%,3%)
            str(msk_hphone$,11%,4%) = str(e_hphone$,4%,4%)
            str(msk_ephone$,2%,3%)  = e_eacd$
            str(msk_ephone$,7%,3%)  = str(e_ephone$,1%,3%)
            str(msk_ephone$,11%,4%) = str(e_ephone$,4%,4%)

            str(scr$(i%),41%,14%) = msk_hphone$         /* CR2490 */
            str(scr$(i%),57%,14%) = msk_ephone$         /* CR2490 */
            gosub lookup_dept
            str(scr$(i%),73%,6%) = desc$                /* CR2490 */
            goto rolodex_next
        rolodex_done
            i_max% = i%
            i% = 1%
            total$ = "[    ]"
            convert i_max% to str(total$,2%,4%),pic(####)

            rolodex$ = "R o l o d e x   S c r e e n"
            gosub'104(1%)
        return

        lookup_dept                         /* Department/Product Line */
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "EMP DEPT "
            str(readkey$,10%,15%) = e_dept$
            read #2,key = readkey$,using L01310 ,desc$, eod goto L01320
L01310:       FMT POS(25), CH(32)
L01320: return

        REM *************************************************************~
            *               R o l o d e x   S c r e e n                 *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            * CR2490 screen layout changes for user ID                  *~
            *************************************************************

        deffn'104(fieldnr%)
L01410:     gosub set_pf4
L01420:     accept                                                       ~
               at (01,02), fac(hex(84)), total$                 , ch(06),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,27), fac(hex(a4)), rolodex$               , ch(27),~
                                                                         ~
               at (04,03), fac(hex(a4)), h1$                    , ch(08),~
               at (04,13), fac(hex(a4)), h2$                    , ch(15),~
               at (04,29), fac(hex(a4)), h3$                    , ch(10),~
               at (04,40), fac(hex(a4)), h4$                    , ch(01),~
               at (04,43), fac(hex(a4)), h5$                    , ch(14),~
               at (04,59), fac(hex(a4)), h6$                    , ch(14),~
               at (04,75), fac(hex(a4)), h7$                    , ch(06),~
                                                                         ~
               at (05,03), fac(hex(84)), scr$(i%)               , ch(78),~
               at (06,03), fac(hex(84)), scr$(i% +  1%)         , ch(78),~
               at (07,03), fac(hex(84)), scr$(i% +  2%)         , ch(78),~
               at (08,03), fac(hex(84)), scr$(i% +  3%)         , ch(78),~
               at (09,03), fac(hex(84)), scr$(i% +  4%)         , ch(78),~
               at (10,03), fac(hex(84)), scr$(i% +  5%)         , ch(78),~
               at (11,03), fac(hex(84)), scr$(i% +  6%)         , ch(78),~
               at (12,03), fac(hex(84)), scr$(i% +  7%)         , ch(78),~
               at (13,03), fac(hex(84)), scr$(i% +  8%)         , ch(78),~
               at (14,03), fac(hex(84)), scr$(i% +  9%)         , ch(78),~
               at (15,03), fac(hex(84)), scr$(i% + 10%)         , ch(78),~
               at (16,03), fac(hex(84)), scr$(i% + 11%)         , ch(78),~
               at (17,03), fac(hex(84)), scr$(i% + 12%)         , ch(78),~
               at (18,03), fac(hex(84)), scr$(i% + 13%)         , ch(78),~
               at (19,03), fac(hex(84)), scr$(i% + 14%)         , ch(78),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
/*EWD001*/     at (22,02), fac(hex(8c)),   str(pf$(1%),,53%)    , ch(53),~
/* Begin*/     at (22,55), fac(hex(a1)),   find_numb$           , ch(08),~
               at (22,64), fac(hex(8c)),   str(pf$(1%),64%,16%) , ch(16),~
               at (23,02), fac(hex(8c)),   str(pf$(2%),,58%)    , ch(58),~
/*EWD001*/     at (23,61), fac(hex(a1)),   find_name$           , ch(15),~
/* End  */     at (23,77), fac(hex(8c)),   str(pf$(2%),76%,4%)  , ch(04),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               if keyhit% <> 2% then goto L01810
                  i% = 1% : goto L01410

L01810:        if keyhit% <> 3% then goto L01860
                  x% = int(i_max%/15.0)
                  i% = (x% * 15%) + 1%
                  goto L01410

L01860:        if keyhit% <> 4% then goto L01910
                  if i% = 1% then goto L01410
                  i% = i% - 15%
                  goto L01410

L01910:        if keyhit% <> 5% then goto L01930
                  i% = i% + 15%
                  goto L01410

L01930:        if keyhit% <> 8% then goto L01940    /*EWD001 - New*/
/* CR2490 */
                  for x% = 1% to i_max%
                    if str(scr$(x%),1%,8%) = find_numb$ then i% = x% else~
                  next x%
                  goto L01410

L01940:        if keyhit% <> 9% then goto L01950    /*EWD001 - New*/
                  i% = 1%                           /* CR2614 */
                  if str(find_name$,1%,2%) = "A " then goto L01410  /* CR2614 */
/* CR2490 */
                  for x% = 1% to i_max%
                    if str(scr$(x%),11%,15%) >= find_name$ then i%=x% 
                    if i% = x% and x% > 1% then goto L01410  /* CR2614 */
                  next x%
                  if x% = i_max% then i% = i_max%            /* CR2614 */
                  goto L01410

L01950:        if keyhit% <> 15 then L01980
                  call "PRNTSCRN" : goto L01420

L01980:        if keyhit% <> 16% then goto L01410
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf4
         inpmessage$ = "Select Applicable (PF) Key ?"
            pf$(1) = "(2)First                                " &        ~
/*EWD001*/           "(8)Find Emp#: xxxxxxxx (Exact Match)   "
            pf$(2) = "(3)Last          (4)Previous Page       " &        ~
/*EWD001*/           "(9)Find Last Name: xxxxxxxxxxxxxxx     "
            pf$(3) = "                 (5)Next Page           " &        ~
                     "                       (16)Exit Rolodex"
/*EWD001*/  pfkeys$ = hex(0102030405ffff0809ffffffffffff1000)
/*EWD001*/  if i% > 1% then goto L02140
               str(pf$(1%),1%,10%) = " " : str(pfkeys$, 2%,1%) = hex(ff)
L02140:     if i% > 15% then goto L02150         /*EWD001*/
               str(pf$(2%),18%,20%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L02150:     if (i% + 14%) < i_max% then goto L02170
               str(pf$(2%),1%,10%) = " " : str(pfkeys$, 3%,1%) = hex(ff)
L02170:     if i% <= (i_max% - 15%) then goto L02190
               str(pf$(3%),18%,20%) = " " : str(pfkeys$, 5%,1%) = hex(ff)
L02190: return

REM         +---------------------------------------------------------------+
REM         |  EWD002     security check for displaying phone numbers       |
REM         +---------------------------------------------------------------+

       check_permission
            user_auth% = 0%
            gosub lookup_manager
	    if user_auth% = 0% then gosub lookup_other
L03020: return

        lookup_manager                      
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "EMP SECUR"
            str(readkey$,10%,12%) = userid$
            read #2,key = readkey$,using L03110 ,desc$, eod goto L03120
L03110:       FMT POS(25), CH(30)
            user_auth% = 1% 
L03120: return

        lookup_other                    
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "EMPACCESS"
            str(readkey$,10%,12%) = userid$
            read #2,key = readkey$,using L03210 ,desc$, eod goto L03220
L03210:       FMT POS(25), CH(30) 
/* let them view all departments */
            user_auth% = 1%
/* comment out dept test
            for l% = 1% to 28% step 3%
                temp$ = str(desc$,l%,3%)     
                if str(desc$,l%,3%) <> e_dept$ then goto L03215     
                    user_auth% = 1%
                    l% = 28 
L03215:
            next l%
	    */
L03220: return

