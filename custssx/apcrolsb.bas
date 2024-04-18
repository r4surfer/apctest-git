        REM *************************************************************~
            *     Check for Upgrade to R6.04.03    11/14/97 RHH         *~
            *                         APCROLEX                          *~
            *                                                           *~
            *                  ( SAME AS 'APCROLEX' )                   *~
            *05/11/2020 ! CR2490 Employee Number format change      !RDB*~
            *************************************************************

        sub "APCROLSB" (pt_dept$)
            call "SHOSTAT" ("Loading (R o l o d e x )")

        dim pt_dept$3, xx_dept$3, x1_dept$3, /* Department Code        */~
            rolodex$30, total$6,         /* Rolodex Screen             */~
            scr$(600%)79,                /* ROLODEX TEXT               */~
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
            msk_hphone$14,               /* FORMAT HOME PHONE          */~
            msk_ephone$14,               /* FORMAT EMERG PHONE         */~
            rec$195,                     /* PARTIAL EMP REC            */~
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

            h1$ = "Emp No. "
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
            x1_dept$ = "ALL"
            if len(pt_dept$) < 3 then goto L00890
               if str(pt_dept$,1%,1%) = "A" then goto L00890
                  x1_dept$ = pt_dept$

L00890:     i%, i_max% = 0%
            emp_key$ = all(hex(00))
            read #1,key 2% > emp_key$, using L00960  , rec$,                ~
                                                    eod goto rolodex_done
            goto L00970
        rolodex_next
            read #1, using L00960  , rec$, eod goto rolodex_done
L00960:       FMT CH(195)
L00970:     if str(rec$,152%,1%) <> "A" then goto rolodex_next
            e_dept$               = str(rec$,1%,3%)
            if x1_dept$ = "ALL" then goto L01020
REM            xx_dept$ = "0" & e_dept$
               xx_dept$ = e_dept$
               if x1_dept$ <> xx_dept$ then goto rolodex_next
L01020:     i% = i% + 1%
/* CR2490 */
            get str(rec$,7%,4%), using L12345, e_no%             /* CR2490 */  
L12345:             FMT BI(4)
            convert e_no% to str(scr$(i%),1%,8%), pic(######00)
REM            str(scr$(i%),1%,5%)   = str(rec$,7%,5%)
            str(scr$(i%),11%,15%)  = str(rec$,12%,15%)
            str(scr$(i%),27%,10%) = str(rec$,27%,10%)
            str(scr$(i%),38%,1%)  = str(rec$,37%,1%)
            e_hacd$   = str(rec$,127%,3%)
            e_hphone$ = str(rec$,130%,7%)
            e_eacd$   = str(rec$,184%,3%)
            e_ephone$ = str(rec$,187%,7%)

            msk_hphone$ = "(   )    -    "
            msk_ephone$ = "(   )    -    "
            str(msk_hphone$,2%,3%)  = e_hacd$
            str(msk_hphone$,7%,3%)  = str(e_hphone$,1%,3%)
            str(msk_hphone$,11%,4%) = str(e_hphone$,4%,4%)
            str(msk_ephone$,2%,3%)  = e_eacd$
            str(msk_ephone$,7%,3%)  = str(e_ephone$,1%,3%)
            str(msk_ephone$,11%,4%) = str(e_ephone$,4%,4%)

            str(scr$(i%),41%,14%) = msk_hphone$
            str(scr$(i%),57%,14%) = msk_ephone$
            gosub lookup_dept
            str(scr$(i%),72%,9%) = desc$
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
            read #2,key = readkey$,using L01410 ,desc$, eod goto L01420
L01410:       FMT POS(25), CH(32)
L01420: return

        REM *************************************************************~
            *               R o l o d e x   S c r e e n                 *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
/* CR2490 screen layout change */
        deffn'104(fieldnr%)
L01510:     gosub set_pf4
L01520:     accept                                                       ~
               at (01,02), fac(hex(84)), total$                 , ch(06),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,27), fac(hex(a4)), rolodex$               , ch(27),~
                                                                         ~
               at (04,02), fac(hex(a4)), h1$                    , ch(08),~
               at (04,12), fac(hex(a4)), h2$                    , ch(15),~
               at (04,28), fac(hex(a4)), h3$                    , ch(10),~
               at (04,39), fac(hex(a4)), h4$                    , ch(01),~
               at (04,42), fac(hex(a4)), h5$                    , ch(14),~
               at (04,58), fac(hex(a4)), h6$                    , ch(14),~
               at (04,72), fac(hex(a4)), h7$                    , ch(09),~
                                                                         ~
               at (05,02), fac(hex(84)), scr$(i%)               , ch(78),~
               at (06,02), fac(hex(84)), scr$(i% +  1%)         , ch(78),~
               at (07,02), fac(hex(84)), scr$(i% +  2%)         , ch(78),~
               at (08,02), fac(hex(84)), scr$(i% +  3%)         , ch(78),~
               at (09,02), fac(hex(84)), scr$(i% +  4%)         , ch(78),~
               at (10,02), fac(hex(84)), scr$(i% +  5%)         , ch(78),~
               at (11,02), fac(hex(84)), scr$(i% +  6%)         , ch(78),~
               at (12,02), fac(hex(84)), scr$(i% +  7%)         , ch(78),~
               at (13,02), fac(hex(84)), scr$(i% +  8%)         , ch(78),~
               at (14,02), fac(hex(84)), scr$(i% +  9%)         , ch(78),~
               at (15,02), fac(hex(84)), scr$(i% + 10%)         , ch(78),~
               at (16,02), fac(hex(84)), scr$(i% + 11%)         , ch(78),~
               at (17,02), fac(hex(84)), scr$(i% + 12%)         , ch(78),~
               at (18,02), fac(hex(84)), scr$(i% + 13%)         , ch(78),~
               at (19,02), fac(hex(84)), scr$(i% + 14%)         , ch(78),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               if keyhit% <> 2% then goto L01910
                  i% = 1% : goto L01510

L01910:        if keyhit% <> 3% then goto L01960
                  x% = int(i_max%/15.0)
                  i% = (x% * 15%) + 1%
                  goto L01510

L01960:        if keyhit% <> 4% then goto L02010
                  if i% = 1% then goto L01510
                  i% = i% - 15%
                  goto L01510

L02010:        if keyhit% <> 5% then goto L02050
                  i% = i% + 15%
                  goto L01510

L02050:        if keyhit% <> 15 then L02080
                  call "PRNTSCRN" : goto L01520

L02080:        if keyhit% <> 16% then goto L01510
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf4
         inpmessage$ = "Select Applicable (PF) Key ?"
            pf$(1) = "(2)First                                " &        ~
                     "                                       "
            pf$(2) = "(3)Last          (4)Previous Page       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next Page           " &        ~
                     "                       (16)Exit Rolodex"
            pfkeys$ = hex(0102030405ffffffffffffffff0e0f1000)
            if i% > 15% then goto L02250
               str(pf$(1%),1%,10%) = " " : str(pfkeys$, 2%,1%) = hex(ff)
               str(pf$(2%),18%,20%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L02250:     if (i% + 14%) < i_max% then goto L02270
               str(pf$(2%),1%,10%) = " " : str(pfkeys$, 3%,1%) = hex(ff)
L02270:     if i% <= (i_max% - 15%) then goto L02290
               str(pf$(3%),18%,20%) = " " : str(pfkeys$, 5%,1%) = hex(ff)
L02290: return

