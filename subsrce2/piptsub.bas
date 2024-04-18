        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  PPPP   IIIII  PPPP   TTTTT   SSS   U   U  BBBB           *~
            *  P   P    I    P   P    T    S      U   U  B   B          *~
            *  PPPP     I    PPPP     T     SSS   U   U  BBBB           *~
            *  P        I    P        T        S  U   U  B   B          *~
            *  P      IIIII  P        T     SSS    UUU   BBBB           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPTSUB  - THIS SUBROUTINE CAN CALL ITSELF TO NEXT INTO   *~
            *            TRACING THE PIP DATA FOR A PARTICULAR JOB      *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/14/84 ! ORIGINAL                                 ! JRW *~
            * 03/23/89 ! Will now show RO's in the PIPIN file     ! RJM *~
            * 11/01/93 ! Purchase Jobs Project - Added support for! JBK *~
            *          !   for PIP types 'BW' and 'RW'.           !     *~
            *          ! Miscellaneous updated screens to more    !     *~
            *          !   current screen standards.  Replace     !     *~
            *          !   old Startove code with call to STARTOVR!     *~
            * 03/27/94 ! PRR 12027. Sources include Vendor.       ! JDH *~
            *          ! PRR 12807. Uses include Customer.        !     *~
            *          ! PRR 13009. Sources include Control #.    !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub  "PIPTSUB" (reviewpart$, #1, #2, #3, #4, #5, #7, #8)

        dim                                                              ~
            col_head$(3,2)79,            /* Column Headings            */~
            control_nbr$19,              /* Job Control #              */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            cuscode$9,                   /* Customer Code              */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            part$25,                                                     ~
            piparray$(100)30,                                            ~
            piparray%(100),                                              ~
            savepart$25,                                                 ~
            savejob$ 08,                                                 ~
            traceline$79,                                                ~
            loadmsg$79,                                                  ~
            line2$79,                    /* Screen Line #2             */~
            errormsg$79,                                                 ~
            inpmessage$79,                                               ~
            pipout$(20)19,                                               ~
            part1$25,                                                    ~
            jb$8,                        /* Job Number                 */~
            po$14,                       /* Purchase Order #           */~
            so$19,                       /* Sales Order # for PLOW     */~
            jobdescr$60,                                                 ~
            jbpart$25,                                                   ~
            jbqty$10,                                                    ~
            jbpartdescr$60,                                              ~
            plowkey$100,                                                 ~
            key$60,                                                      ~
            p$19,                                                        ~
            p_c_$(20)25,                 /* Part or Customer Code      */~
            p_c_descr$(20)32,            /* Part or Customer Descr     */~
            c_v_$(20)19,                 /* Control # or Vendor Code   */~
            do$(20)8,                                                    ~
            qty$(20)10,                                                  ~
            quan$(20)10,                                                 ~
            tag$(20)19,                                                  ~
            di$(20)8,                                                    ~
            ds$(20)8,                                                    ~
            vencode$9,                   /* Vendor Code                */~
            where$(20)10,                                                ~
            plowkey2$100,                                                ~
            plowkey3$100,                                                ~
            partdescr$60,                                                ~
            part2$25,                                                    ~
            part3$25,                                                    ~
            reviewpart$25,                                               ~
            tag$19

        dim f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            f2%(64)                      /* = 0 if the file is open    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/28/96 Last Wang Release"
        REM *************************************************************
            savepart$ = reviewpart$

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! PIPIN    ! Planned inventory additions detail  feed *~
            * #03 ! PIPOUT   ! Planned inventory use detail rec  feeds  *~
            * #04 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #05 ! HNYMASTR ! Inventory Master File                    *~
            * #07 ! JBMASTR2 ! Production job master file               *~
            * #08 ! DEMMASTR ! Demand Master File                       *~
            * #10 ! VBKMASTR ! Backlog main header file                 *~
            * #11 ! BCKLINES ! Back Log Line Item File                  *~
            * #12 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #10, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  10,  keylen =  16

            select #11,  "BCKLINES",                                     ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #12,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            if beenherebefore% = 1% then L09000
                beenherebefore% = 1%
                call "OPENCHCK" (#10, 0%, f2%(10%), 0%, " ")
                call "OPENCHCK" (#11, 0%, f2%(11%), 0%, " ")
                call "OPENCHCK" (#12, 0%, f2%(12%), 0%, " ")


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            call "READ100" (#1, "MONTHS OPEN", f1%(1%))
            if f1%(1%) = 0% then L65000
            get #1, using L09120, pldate$
L09120:         FMT XX(32), CH(6)

            loadmsg$ = "Enter the desired search key and search method."

            traceline$ = "S = Sources, U = Uses, C= Components For A Job ~
        ~(Kit List)"

            str(line2$,62) = " PIPTSUB: " & str(cms2v$,,8)

            col_head$(1%,1%) = " "
            col_head$(1%,2%) = "Part Code                    Part Descrip~
        ~tion              Date Out    Quantity"
            col_head$(2%,1%) = "Origin of                      Vendor (If~
        ~ P.O.)    Start    Expected   Expected"
            col_head$(2%,2%) = "Part       PIP Additions Tag   Control # ~
        ~(If Job)  Date     Date In    Quantity"
            col_head$(3%,1%) = "                      Required Expected  ~
        ~           Customer (If for S.O.)     "
            col_head$(3%,2%) = "PIP Withdrawal Tag    Quantity Date Out T~
        ~ype of Use Part (If for Job)          "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        /* KEY CONVENTIONS 4 = USES FOR PART
                           5 = SOURCES OF PART
                           6 = KIT LIST FOR JOB
                           7 = USES FOR JOB PART
                           8 = SOURCES OF JOB PART(OTHER JOBS?)    */

        inputmode
            init(" ") inpmessage$, piparray$()
            mat piparray% = zer

                part$     =  savepart$
                job$      =  savejob$
                gosub get_part_or_job
                savepart$ = part$
                savejob$  = job$
                errormsg$ = " "

                if keyhit% <> 16% then L10180
                    goto L65000
L10180:         if keyhit% <> 4% and keyhit% <> 5%  then L10241

                    if part$ <> " " then L10200
                    call "GETCODE" (#5, part$, key$, 0%, 0, f1%(5%))
                    if part$ <> " " then savepart$ = part$
                    goto inputmode

L10200:                 call "READ100" (#5, part$, f1%(5%))
                        if f1%(5%) <> 0% then L10320
                           errormsg$ = "PART NOT PLANNED: " & part$
                           goto inputmode

L10241:             if job$ <> " " then L10250
                    call "GETCODE" (#7, job$, key$, 0%, 0, f1%(7%))
                    if job$ <> " " then savejob$ = job$
                    goto inputmode

L10250:                 call "READ100" (#7, job$, f1%(7%))
                        if f1%(7%) <> 0% then L10290
                           errormsg$ = "JOB NOT ON FILE: " & job$
                           goto inputmode
L10290:                 get #7, using L10300, part$
L10300:                     FMT XX(57), CH(25)

L10320:         current% = 1%
                if keyhit% <> 4% then L10380
                   piparray$(current%) = "U " & part$
                   gosub'203
                   goto inputmode

L10380:         if keyhit% <> 5% then L10430
                   piparray$(current%) = "S " & part$
                   gosub'202
                   goto inputmode

L10430:         if keyhit% <> 6% then L10480
                   piparray$(current%) = "C " & "JOB ORDER: " & job$
                   gosub'201
                   goto inputmode

L10480:         if keyhit% <> 7% then L10530
                   piparray$(current%) = "U " & part$
                   gosub'203
                   goto inputmode

L10530:         if keyhit% <> 8% then L10560
                   piparray$(current%) = "S " & part$
                   gosub'202
L10560:            goto inputmode

        REM *************************************************************
        REM KIT LIST FOR JOB
        deffn'201
L11030:         init (hex(00)) plowkey$
                str(plowkey$,,19%) = str(piparray$(current%),3%,19%)
L11050:         gosub loadpipout
                gosub showpipout /* AT 45000 */
                      array% = min( max(cursor%(1%)-4%,1%) , 16%)

                      if keyhit% =  1% then gosub startover
                      if keyhit% =  2% then goto L11030
                      if keyhit% =  3% then goto L11050
                      if keyhit% = 16% then return

                      if keyhit% <> 4% then goto L11230
                         current% = current% + 1%
                         piparray%(current%) = 0%
                         piparray$(current%) = "U " & p_c_$(array%)
                            gosub'203
                         piparray$(current%) = " "
                         current% = current% - 1%
                      goto L11030

L11230:              if keyhit% <> 5% then goto L11320
                        current% = current% + 1%
                        piparray%(current%) = 0%
                        piparray$(current%) = "S " & p_c_$(array%)
                           gosub'202
                        piparray$(current%) = " "
                        current% = current% - 1%
                        goto L11030

L11320:              if keyhit% <> 7% then goto L11410
                        current% = current% + 1%
                        piparray%(current%) = 0%
                        piparray$(current%) = "U " & jbpart$
                           gosub'203
                        piparray$(current%) = " "
                        current% = current% - 1%
                        goto L11030

L11410:              if keyhit% <> 8% then goto L11500
                        current% = current% + 1%
                        piparray%(current%) = 0%
                        piparray$(current%) = "S " & jbpart$
                           gosub'202
                        piparray$(current%) = " "
                        current% = current% - 1%
                        goto L11030

L11500:              if keyhit% = 10% then gosub showarray
                     goto L11030

        REM *************************************************************
        REM SOURCES 0F PARTS
        deffn'202
L12026:         init (hex(00)) plowkey2$
                str(plowkey2$,,25%) = str(piparray$(current%),3%,25%)
L12030:         gosub loadpipin
L12040:         gosub showpipin
                      array% = min( max(cursor%(1%)-4%,1%), 16%)

                      if keyhit% =  1% then gosub startover
                      if keyhit% =  2% then goto L12026
                      if keyhit% =  3% then goto L12030
                      if keyhit% = 16% then return

                      if keyhit% <> 6% then goto L12160
                      if str(tag$(array%),1%,2%)  = "JO" then L12100
                      if str(tag$(array%),1%,2%)  = "BW" then L12100
                      if str(tag$(array%),1%,2%)  = "RW" then L12100
                      if str(tag$(array%),1%,2%) <> "WO" then L12040
L12100:                  current% = current% + 1%
                         piparray%(current%) = 1%
                         piparray$(current%) = "C " & tag$(array%)
                           gosub'201
                         piparray$(current%) = " "
                         current% = current% - 1%
                      goto L12026

L12160:              if keyhit% <> 4% then goto L12250
                        current% = current% + 1%
                        piparray%(current%) = 0%
                        piparray$(current%)= "U " & part2$
                          gosub'203
                        piparray$(current%) = " "
                        current% = current% - 1%
                     goto L12026

L12250:              if keyhit% = 10% then gosub showarray
                     goto L12040

        REM *************************************************************
        REM USES 0F PARTS
        deffn'203
                mode% = 1%
L13030:         init (hex(00)) plowkey3$
                str(plowkey3$,,25%) = str(piparray$(current%),3%,25%)
L13050:         gosub loadpipout1
L13060:         if mode% > 0% then gosub showpipout1                     ~
                              else gosub showpipout2
                      array% = min( max(cursor%(1)-4%,1%), 16%)

                      if keyhit% =  1% then gosub startover
                      if keyhit% =  2% then goto L13030
                      if keyhit% =  3% then goto L13050
                      if keyhit% <> 9% then L13120
                                 mode% = - mode%
                                 goto L13060
L13120:               if keyhit% = 16% then return

                      if keyhit% <> 6% then goto L13250
                      if str(tag$(array%),1%,2%)  = "JO" then L13170
                      if str(tag$(array%),1%,2%)  = "BW" then L13170
                      if str(tag$(array%),1%,2%)  = "RW" then L13170
                      if str(tag$(array%),1%,2%) <> "WO" then L13060
L13170:                  current% = current% + 1%
                         piparray%(current%) = 1%
                         piparray$(current%) = "C " & tag$(array%)
                           gosub'201
                         piparray$(current%) = " "
                         current% = current% - 1%
                      goto L13030

L13250:              if keyhit% <> 5% then goto L13340
                        current% = current% + 1%
                        piparray%(current%) = 0%
                        piparray$(current%)="S " & part3$
                          gosub'202
                        piparray$(current%) = " "
                        current% = current% - 1%
                     goto L13030

L13340:              if p_c_$(array%) = " " then L13700

                     if keyhit% <> 8% then goto L13450
                        current% = current% + 1%
                        piparray%(current%) = 0%
                        piparray$(current%) = "S " & p_c_$(array%)
                          gosub'202
                        piparray$(current%) = " "
                        current% = current% - 1%
                     goto L13030

L13450:              if keyhit% <> 7% then goto L13700
                        current% = current% + 1%
                        piparray%(current%) = 0%
                        piparray$(current%) = "U " & p_c_$(array%)
                          gosub'203
                        piparray$(current%) = " "
                        current% = current% - 1%
                     goto L13030

L13700:              if keyhit% = 10% then gosub showarray
                     goto L13060

        REM **************************************************************

        showarray :   /* SHOW THE NESTED SEARCH ARRAY */
L14030:         gosub'102   /* ACCEPT STATEMENT */
                     if keyhit% =  1% then gosub startover
                     if keyhit% = 16% then L14080
                goto L14030

L14080:     keyhit% = -99%
            return



        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        loadpipout:
                     line% = 0%
                     if str(plowkey$,1%,2%) = "WO" then L30084
                     job$ = str(plowkey$,12%,8%)
                     jobdescr$ = " "
                     call "DESCRIBE" (#7, job$, jobdescr$, 0%, f1%(7%))
                        if f1%(7%) <> 0% then L30100
L30084:                 jobdescr$ = str(plowkey$,1%,19%)
                        call "READ100" (#2, str(jobdescr$,1%,19%),f1%(2%))
                           if f1%(2%) = 0% then L30140
                           get #2, using L30092, jbpart$, qty
                           goto L30126
L30092:                        FMT CH(25), XX(23), PD(14,4)

L30100:              l% = max(len(job$),1%) : m% = max(len(jobdescr$),1%)
                     jobdescr$ = str(job$,,l%) & ", " & str(jobdescr$,,m%)
                     get #7, using L30124, jbpart$, qty
L30124:                   FMT XX(57), CH(25), PD(14,4)
L30126:              jbpartdescr$ = " "
                     call "DESCRIBE" (#5,jbpart$,jbpartdescr$,0%,f1%(5%))
                     l%=max(len(jbpart$),1%):m%=max(len(jbpartdescr$),1%)
                     jbpartdescr$ = str(jbpart$,,l%) &                   ~
                                             ", " & str(jbpartdescr$,,m%)
                     call "NUMPRINT" (qty, 2, jbqty$)
L30140:             init (" ") p_c_$(),qty$(),p_c_descr$(),pipout$(),do$()
L30160:              call "PLOWNEXT" (#3, plowkey$, 19%, f1%(3%))
                          if f1%(3%) = 0% then L30540
                                         /*ELSE*/
                                     get #3 using L30480,p$,part1$,do%,qty
                                     line% = line% + 1%
                                     pipout$(line%) = p$
                 call "DATE" addr("G+", pldate$, do%-1%, do$(line%), err%)
                    call "DATEFMT" (do$(line%))
                                     p_c_$(line%) = part1$
                                     p_c_descr$(line%) = "NOT ON FILE"

                                     key$ = " "
                    call "DESCRIBE" (#5, part1$, key$, 0%, f1%(5%))
                                     p_c_descr$(line%) = key$

                                     call "NUMPRINT" (qty,.5, qty$(line%))
L30480:                           FMT CH(19),CH(25),BI(04),XX(08),PD(14,4)
                                     if line% < 16% then L30160
                                     return
L30540:  p_c_$(line% + 1%) =  hex(86) & "** EOD **"
         return

        loadpipin:
                     part2$ = plowkey2$
                     partdescr$ = " "
                     call "DESCRIBE" (#5, part2$, partdescr$, 0%, f1%(5%))
                     l% = max(len(part2$),1%):m% = max(len(partdescr$),1%)
                     partdescr$ =str(part2$,,l%) &", "&str(partdescr$,,m%)
                     line% = 0%
                     init (" ") quan$(), tag$(), di$(), ds$(), where$(), ~
                                c_v_$()
L31080:              call "PLOWALTS" (#2, plowkey2$, 1%, 25%, f1%(2%))
                          if f1%(2%) = 0% then L31390
                                         /*ELSE*/
                                     get #2 using L31120,di%,tag$,qty,ds%
L31120:                                  FMT XX(25), BI(4), CH(19),      ~
                                             PD(14,4), BI(4)
                                     line% = line% + 1%
                s$ = str(tag$,1%,2%)
                                   where$(line%) = "Sales Ordr"
                if  s$ = "BO" then where$(line%) = "Buy Order "
                if  s$ = "RO" then where$(line%) = "Pur Dir   "
                if  s$ = "PO" then where$(line%) = "Pur Order "
                if  s$ = "JO" then where$(line%) = "Job Order "
                if  s$ = "WO" then where$(line%) = "Work Order"
                if  s$ = "QC" then where$(line%) = "Qual Cntrl"
                if  s$ = "BW" then where$(line%) = "Buy Work  "
                if  s$ = "RW" then where$(line%) = "Pur Jb Dir"

                       tag$(line%) = tag$
                       if s$ = "JO" then gosub get_control_nbr
                       if s$ = "PO" then gosub get_vendor

                       if where$(line%) <> "Sales Ordr" then L31310
                       init (" ") quan$(line%),where$(line%),tag$(line%),~
                                  di$(line%), ds$(line%)
                       line% = line% - 1%
                       goto L31080

L31310:         call "DATE" addr("G+", pldate$, di%-1%, di$(line%), err%)
                call "DATEFMT" (di$(line%))
                call "DATE" addr("G+", pldate$, ds%-1%, ds$(line%), err%)
                call "DATEFMT" (ds$(line%))
                                     call "NUMPRINT" (qty,.5,quan$(line%))
                                     if line% < 16% then L31080
                                     return

L31390:  where$(line% + 1%) =  hex(86) & "** EOD **"
         return

        get_control_nbr
            jb$ = str(tag$, 12%, 8%)
            control_nbr$ = " "
            call "READ100" (#7, jb$, f1%(7%))
            if f1%(7%) = 1% then get #7 using L31470, control_nbr$
L31470:         FMT POS(1120), CH(19)
            c_v_$(line%) = control_nbr$
            return

        get_vendor
            po$ = str(tag$, 3%, 14%)
            vencode$ = " "
            call "REDALT0" (#10, po$, 1%, f1%(10%))
            if f1%(10%) = 1% then get #10 using L31550, vencode$
L31550:         FMT CH(9)
            c_v_$(line%) = vencode$
            return

        loadpipout1:
                     part3$ = plowkey3$
                     partdescr$ = " "
                     call "DESCRIBE" (#5, part3$, partdescr$, 0%, f1%(5%))
                     l% = max(len(part3$),1%):m% = max(len(partdescr$),1%)
                     partdescr$ =str(part3$,,l%) &", "&str(partdescr$,,m%)
                     line% = 0%
                           init (" ") quan$(), tag$(), di$(), p_c_$(),   ~
                                      where$(), p_c_descr$()
L32080:              call "PLOWALTS" (#3, plowkey3$, 1%, 25%, f1%(3%))
                          if f1%(3%) = 0% then L32390
                                         /*ELSE*/
                              get #3 using L32120,tag$,di%,qty
L32120:                         FMT CH(19), XX(25), BI(4), XX(8), PD(14,4)

                                     line% = line% + 1%
                           s$ = str(tag$,1%,2%)
                       if  s$  = "WO" then L32190
                       if  s$  = "BW" then L32190
                       if  s$  = "RW" then L32190
                       if  s$ <> "JO" then L32230
L32190:                    call "READ100" (#2, tag$, f1%(2%))
                              if f1%(2%) = 0% then L32230
                           get #2, using L32220, p_c_$(line%)
L32220:                        FMT CH(25)
                           call "DESCRIBE" (#5, p_c_$(line%),            ~
                                 p_c_descr$(line%), 0%, f1%(5%))

L32230:                tag$(line%) = tag$
                                   where$(line%) = "Sales Ordr"
                if  s$ = "BO" then where$(line%) = "Buy Order "
                if  s$ = "RO" then where$(line%) = "Pur Dir   "
                if  s$ = "PO" then where$(line%) = "Pur Order "
                if  s$ = "JO" then where$(line%) = "Job Order "
                if  s$ = "WO" then where$(line%) = "Work Order"
                if  s$ = "QC" then where$(line%) = "Qual Cntrl"
                if  s$ = "BW" then where$(line%) = "Buy Work  "
                if  s$ = "RW" then where$(line%) = "Pur Jb Dir"

                if where$(line%) = "Sales Ordr" then gosub get_customer

                call "DATE" addr("G+", pldate$, di%-1%, di$(line%), err%)
                call "DATEFMT" (di$(line%))
                                     call "NUMPRINT" (qty,.5,quan$(line%))
                                     if line% < 16% then L32080
                                     return

L32390:  tag$(line% + 1%) =  hex(86) & "** EOD **"
         return

        get_customer
            so$ = str(tag$,, 16%) & hex(00)
            cuscode$ = " "
            call "PLOWNEXT" (#11, so$, 16%, f1%(11%))
            if f1%(11%) <> 1% then return
                get #11 using L32480, cuscode$
L32480:             FMT CH(9)
                p_c_$(line%) = cuscode$
                p_c_descr$(line%) = "** Not on File **"
                call "DESCRIBE" (#12, cuscode$, p_c_descr$(line%), 0%,   ~
                                                                 f1%(12%))
                return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

        FMT                      /* FILE: PIPIN                        */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date in subscript for PIP          */~
            CH(19),              /* Tag number in level 2 planning     */~
            PD(14,4),            /* Quantity of something in packed de */~
            BI(4)                /* Date to start as a date subscript  */~

        FMT                      /* FILE: PIPOUT                       */~
            CH(19),              /* Tag number in level 2 planning     */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date out of PIP in date subscript  */~
            CH(8),               /* Time from the system clock         */~
            PD(14,4)             /* Quantity of something in packed de */~

        FMT                      /* FILE: PIPMASTR                     */~
            CH(1),               /* General purpose status indicator   */~
            CH(25),              /* Part code                          */~
            490*BI(4),           /* Planned inventory position         */~
            PD(14,4),            /* Quantity of something in packed de */~
            PD(14,4),            /* Safety stock                       */~
            PD(14,4),            /* minimum order qty. / minimum order */~
            CH(14)               /* filler for rest of record or inter */~

        FMT                      /* FILE: JBMASTR2                     */~
            CH(8),               /* Production job code                */~
            CH(30),              /* Description of production job      */~
            CH(19),              /* Tag number in level 2 planning     */~
            CH(25),              /* Part code                          */~
            PD(14,4),            /* Quantity to make                   */~
            PD(14,4),            /* Quantity completed to date         */~
            PD(14,4),            /* Total material cost                */~
            PD(14,4),            /* Total labor cost                   */~
            PD(14,4),            /* Total overhead cost                */~
            PD(14,4),            /* Material credit per unit           */~
            PD(14,4),            /* Labor credit per unit              */~
            PD(14,4),            /* Over head credit per part          */~
            CH(6),               /* Date production job actually start */~
            CH(6),               /* Date production job actually ended */~
            CH(9),               /* Work in process GL account code    */~
            CH(6),               /* Date production job planned to sta */~
            CH(6),               /* Date production job planned to be  */~
            CH(71)               /* filler for rest of record or inter */~


        REM AT (22,02),                                                  ~
            "1)START OVER  4)USES FOR PART  6)KIT LIST  7)USES FOR JOB PA~
        RT  13)INSTRUCTIONS"                                             ~
            AT (23,02),                                                  ~
            "2)FIRST       5)SOURCES FOR PART           8)SRCES FOR JOB P~
        ART 15)PRINT SCREEN"                                             ~
            AT (24,02),                                                  ~
            "3)NEXT                        10)SHOW PATH OF TRACE         ~
            16)RETURN      "                                             ~
        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'102
                init (" ")  str(line2$,1%,60%)
                str(line2$,1%,60%) = "Nesting of Current Job Search"
L40035:     accept                                                       ~
               at (01,02), "Show PIP Status By Job Or Part",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$            , ch(08),      ~
               at (02,02), fac(hex(ac)), line2$           , ch(79),      ~
               at (03,02), "01)",                                        ~
               at (04,02), "02)",                                        ~
               at (05,02), "03)",                                        ~
               at (06,02), "04)",                                        ~
               at (07,02), "05)",                                        ~
               at (08,02), "06)",                                        ~
               at (09,02), "07)",                                        ~
               at (10,02), "08)",                                        ~
               at (11,02), "09)",                                        ~
               at (12,02), "10)",                                        ~
               at (13,02), "11)",                                        ~
               at (14,02), "12)",                                        ~
               at (15,02), "13)",                                        ~
               at (16,02), "14)",                                        ~
               at (17,02), "15)",                                        ~
               at (18,02), "16)",                                        ~
               at (19,02), "17)",                                        ~
               at (20,02), "18)",                                        ~
               at (21,02), "19)",                                        ~
               at (22,02), "20)",                                        ~
                                                                         ~
               at (03,29), "21)",                                        ~
               at (04,29), "22)",                                        ~
               at (05,29), "23)",                                        ~
               at (06,29), "24)",                                        ~
               at (07,29), "25)",                                        ~
               at (08,29), "26)",                                        ~
               at (09,29), "27)",                                        ~
               at (10,29), "28)",                                        ~
               at (11,29), "29)",                                        ~
               at (12,29), "30)",                                        ~
               at (13,29), "31)",                                        ~
               at (14,29), "32)",                                        ~
               at (15,29), "33)",                                        ~
               at (16,29), "34)",                                        ~
               at (17,29), "35)",                                        ~
               at (18,29), "36)",                                        ~
               at (19,29), "37)",                                        ~
               at (20,29), "38)",                                        ~
               at (21,29), "39)",                                        ~
               at (22,29), "40)",                                        ~
                                                                         ~
               at (03,56), "41)",                                        ~
               at (04,56), "42)",                                        ~
               at (05,56), "43)",                                        ~
               at (06,56), "44)",                                        ~
               at (07,56), "45)",                                        ~
               at (08,56), "46)",                                        ~
               at (09,56), "47)",                                        ~
               at (10,56), "48)",                                        ~
               at (11,56), "49)",                                        ~
               at (12,56), "50)",                                        ~
               at (13,56), "51)",                                        ~
               at (14,56), "52)",                                        ~
               at (15,56), "53)",                                        ~
               at (16,56), "54)",                                        ~
               at (17,56), "55)",                                        ~
               at (18,56), "56)",                                        ~
               at (19,56), "57)",                                        ~
               at (20,56), "58)",                                        ~
               at (21,56), "59)",                                        ~
               at (22,56), "60)",                                        ~
                                                                         ~
                                                                         ~
               at (03,06), fac(hex(85)), piparray$(01)          , ch(22),~
               at (04,06), fac(hex(85)), piparray$(02)          , ch(22),~
               at (05,06), fac(hex(85)), piparray$(03)          , ch(22),~
               at (06,06), fac(hex(85)), piparray$(04)          , ch(22),~
               at (07,06), fac(hex(85)), piparray$(05)          , ch(22),~
               at (08,06), fac(hex(85)), piparray$(06)          , ch(22),~
               at (09,06), fac(hex(85)), piparray$(07)          , ch(22),~
               at (10,06), fac(hex(85)), piparray$(08)          , ch(22),~
               at (11,06), fac(hex(85)), piparray$(09)          , ch(22),~
               at (12,06), fac(hex(85)), piparray$(10)          , ch(22),~
               at (13,06), fac(hex(85)), piparray$(11)          , ch(22),~
               at (14,06), fac(hex(85)), piparray$(12)          , ch(22),~
               at (15,06), fac(hex(85)), piparray$(13)          , ch(22),~
               at (16,06), fac(hex(85)), piparray$(14)          , ch(22),~
               at (17,06), fac(hex(85)), piparray$(15)          , ch(22),~
               at (18,06), fac(hex(85)), piparray$(16)          , ch(22),~
               at (19,06), fac(hex(85)), piparray$(17)          , ch(22),~
               at (20,06), fac(hex(85)), piparray$(18)          , ch(22),~
               at (21,06), fac(hex(85)), piparray$(19)          , ch(22),~
               at (22,06), fac(hex(85)), piparray$(20)          , ch(22),~
                                                                         ~
               at (03,33), fac(hex(85)), piparray$(21)          , ch(22),~
               at (04,33), fac(hex(85)), piparray$(22)          , ch(22),~
               at (05,33), fac(hex(85)), piparray$(23)          , ch(22),~
               at (06,33), fac(hex(85)), piparray$(24)          , ch(22),~
               at (07,33), fac(hex(85)), piparray$(25)          , ch(22),~
               at (08,33), fac(hex(85)), piparray$(26)          , ch(22),~
               at (09,33), fac(hex(85)), piparray$(27)          , ch(22),~
               at (10,33), fac(hex(85)), piparray$(28)          , ch(22),~
               at (11,33), fac(hex(85)), piparray$(29)          , ch(22),~
               at (12,33), fac(hex(85)), piparray$(30)          , ch(22),~
               at (13,33), fac(hex(85)), piparray$(31)          , ch(22),~
               at (14,33), fac(hex(85)), piparray$(32)          , ch(22),~
               at (15,33), fac(hex(85)), piparray$(33)          , ch(22),~
               at (16,33), fac(hex(85)), piparray$(34)          , ch(22),~
               at (17,33), fac(hex(85)), piparray$(35)          , ch(22),~
               at (18,33), fac(hex(85)), piparray$(36)          , ch(22),~
               at (19,33), fac(hex(85)), piparray$(37)          , ch(22),~
               at (20,33), fac(hex(85)), piparray$(38)          , ch(22),~
               at (21,33), fac(hex(85)), piparray$(39)          , ch(22),~
               at (22,33), fac(hex(85)), piparray$(40)          , ch(22),~
                                                                         ~
               at (03,60), fac(hex(85)), piparray$(41)          , ch(21),~
               at (04,60), fac(hex(85)), piparray$(42)          , ch(21),~
               at (05,60), fac(hex(85)), piparray$(43)          , ch(21),~
               at (06,60), fac(hex(85)), piparray$(44)          , ch(21),~
               at (07,60), fac(hex(85)), piparray$(45)          , ch(21),~
               at (08,60), fac(hex(85)), piparray$(46)          , ch(21),~
               at (09,60), fac(hex(85)), piparray$(47)          , ch(21),~
               at (10,60), fac(hex(85)), piparray$(48)          , ch(21),~
               at (11,60), fac(hex(85)), piparray$(49)          , ch(21),~
               at (12,60), fac(hex(85)), piparray$(50)          , ch(21),~
               at (13,60), fac(hex(85)), piparray$(51)          , ch(21),~
               at (14,60), fac(hex(85)), piparray$(52)          , ch(21),~
               at (15,60), fac(hex(85)), piparray$(53)          , ch(21),~
               at (16,60), fac(hex(85)), piparray$(54)          , ch(21),~
               at (17,60), fac(hex(85)), piparray$(55)          , ch(21),~
               at (18,60), fac(hex(85)), piparray$(56)          , ch(21),~
               at (19,60), fac(hex(85)), piparray$(57)          , ch(21),~
               at (20,60), fac(hex(85)), piparray$(58)          , ch(21),~
               at (21,60), fac(hex(85)), piparray$(59)          , ch(21),~
               at (22,60), fac(hex(85)), piparray$(60)          , ch(21),~
                                                                         ~
               at (23,02), fac(hex(a4)), traceline$,                     ~
                                                                         ~
               at (24,02),                                               ~
                  "(1)Start Over",                                       ~
               at (24,30),                                               ~
                  "(13)Instructions",                                    ~
               at (24,50),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,71),                                               ~
                  "(16)Return",                                          ~
                                                                         ~
               keys(hex(010d0f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L40775
                  call "MANUAL" ("PIPTRACE")
                  goto L40035

L40775:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40035


        get_part_or_job
            init (" ")  str(line2$,1%,60%)
L41010:         accept                                                   ~
                     at (01,02), "Show PIP Status By Job Or Part",       ~
                     at (01,66), "Today:",                               ~
                     at (01,73), fac(hex(8c)), date$            , ch(08),~
                     at (02,02), fac(hex(ac)), line2$           , ch(79),~
                     at (04,02), fac(hex(94)), errormsg$        , ch(79),~
                     at (06,02), "Enter A Part Code or Job Number.",     ~
                     at (07,02), "Part Code",                            ~
                     at (08,02), "Job Number",                           ~
                     at (07,25), fac(hex(81)), part$            , ch(25),~
                     at (08,25), fac(hex(81)), job$             , ch(08),~
                     at (21,02), fac(hex(a5)), loadmsg$         , ch(79),~
                                                                         ~
            at (22,02),                                                  ~
            "              4)Uses For Part  6)Kit List  7)Uses For Job Pa~
        ~rt  13)Instructions",                                            ~
            at (23,02),                                                  ~
            "              5)Sources For Part           8)Srces For Job P~
        ~art 15)Print Screen",                                            ~
            at (24,02),                                                  ~
            "                                                            ~
        ~    16)Exit Program",                                            ~
            keys(hex(04050607080d0f10)),                                 ~
            key (keyhit%)

               if keyhit% <> 13 then L41260
                  call "MANUAL" ("PIPTRACE")
                  goto L41010

L41260:        if keyhit% <> 15 then L41300
                  call "PRNTSCRN"
                  goto L41010

L41300:     return


        REM *************************************************************

        showpipout:

            inpmessage$ = "Position cursor to a line and press (4), (5)."
            init (" ")  str(line2$,1%,60%)
            str(line2$,1%,60%) = jbqty$ & " of Part " & jbpartdescr$


L42030:     accept                                                       ~
               at (01,02),                                               ~
                  "Parts Required For Job:",                             ~
               at (01,26), fac(hex(85)), jobdescr$              , ch(50),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), col_head$(1%,1%)       , ch(79),~
               at (04,02), fac(hex(ac)), col_head$(1%,2%)       , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8e)), p_c_$( 1)              , ch(25),~
               at (06,02), fac(hex(8e)), p_c_$( 2)              , ch(25),~
               at (07,02), fac(hex(8e)), p_c_$( 3)              , ch(25),~
               at (08,02), fac(hex(8e)), p_c_$( 4)              , ch(25),~
               at (09,02), fac(hex(8e)), p_c_$( 5)              , ch(25),~
               at (10,02), fac(hex(8e)), p_c_$( 6)              , ch(25),~
               at (11,02), fac(hex(8e)), p_c_$( 7)              , ch(25),~
               at (12,02), fac(hex(8e)), p_c_$( 8)              , ch(25),~
               at (13,02), fac(hex(8e)), p_c_$( 9)              , ch(25),~
               at (14,02), fac(hex(8e)), p_c_$(10)              , ch(25),~
               at (15,02), fac(hex(8e)), p_c_$(11)              , ch(25),~
               at (16,02), fac(hex(8e)), p_c_$(12)              , ch(25),~
               at (17,02), fac(hex(8e)), p_c_$(13)              , ch(25),~
               at (18,02), fac(hex(8e)), p_c_$(14)              , ch(25),~
               at (19,02), fac(hex(8e)), p_c_$(15)              , ch(25),~
               at (20,02), fac(hex(8e)), p_c_$(16)              , ch(25),~
                                                                         ~
               at (05,31), fac(hex(8c)), p_c_descr$( 1)         , ch(32),~
               at (06,31), fac(hex(8c)), p_c_descr$( 2)         , ch(32),~
               at (07,31), fac(hex(8c)), p_c_descr$( 3)         , ch(32),~
               at (08,31), fac(hex(8c)), p_c_descr$( 4)         , ch(32),~
               at (09,31), fac(hex(8c)), p_c_descr$( 5)         , ch(32),~
               at (10,31), fac(hex(8c)), p_c_descr$( 6)         , ch(32),~
               at (11,31), fac(hex(8c)), p_c_descr$( 7)         , ch(32),~
               at (12,31), fac(hex(8c)), p_c_descr$( 8)         , ch(32),~
               at (13,31), fac(hex(8c)), p_c_descr$( 9)         , ch(32),~
               at (14,31), fac(hex(8c)), p_c_descr$(10)         , ch(32),~
               at (15,31), fac(hex(8c)), p_c_descr$(11)         , ch(32),~
               at (16,31), fac(hex(8c)), p_c_descr$(12)         , ch(32),~
               at (17,31), fac(hex(8c)), p_c_descr$(13)         , ch(32),~
               at (18,31), fac(hex(8c)), p_c_descr$(14)         , ch(32),~
               at (19,31), fac(hex(8c)), p_c_descr$(15)         , ch(32),~
               at (20,31), fac(hex(8c)), p_c_descr$(16)         , ch(32),~
                                                                         ~
               at ( 5,61), fac(hex(8c)), do$( 1)                , ch(08),~
               at ( 6,61), fac(hex(8c)), do$( 2)                , ch(08),~
               at ( 7,61), fac(hex(8c)), do$( 3)                , ch(08),~
               at ( 8,61), fac(hex(8c)), do$( 4)                , ch(08),~
               at ( 9,61), fac(hex(8c)), do$( 5)                , ch(08),~
               at (10,61), fac(hex(8c)), do$( 6)                , ch(08),~
               at (11,61), fac(hex(8c)), do$( 7)                , ch(08),~
               at (12,61), fac(hex(8c)), do$( 8)                , ch(08),~
               at (13,61), fac(hex(8c)), do$( 9)                , ch(08),~
               at (14,61), fac(hex(8c)), do$(10)                , ch(08),~
               at (15,61), fac(hex(8c)), do$(11)                , ch(08),~
               at (16,61), fac(hex(8c)), do$(12)                , ch(08),~
               at (17,61), fac(hex(8c)), do$(13)                , ch(08),~
               at (18,61), fac(hex(8c)), do$(14)                , ch(08),~
               at (19,61), fac(hex(8c)), do$(15)                , ch(08),~
               at (20,61), fac(hex(8c)), do$(16)                , ch(08),~
                                                                         ~
               at ( 5,71), fac(hex(8c)), qty$( 1)               , ch(10),~
               at ( 6,71), fac(hex(8c)), qty$( 2)               , ch(10),~
               at ( 7,71), fac(hex(8c)), qty$( 3)               , ch(10),~
               at ( 8,71), fac(hex(8c)), qty$( 4)               , ch(10),~
               at ( 9,71), fac(hex(8c)), qty$( 5)               , ch(10),~
               at (10,71), fac(hex(8c)), qty$( 6)               , ch(10),~
               at (11,71), fac(hex(8c)), qty$( 7)               , ch(10),~
               at (12,71), fac(hex(8c)), qty$( 8)               , ch(10),~
               at (13,71), fac(hex(8c)), qty$( 9)               , ch(10),~
               at (14,71), fac(hex(8c)), qty$(10)               , ch(10),~
               at (15,71), fac(hex(8c)), qty$(11)               , ch(10),~
               at (16,71), fac(hex(8c)), qty$(12)               , ch(10),~
               at (17,71), fac(hex(8c)), qty$(13)               , ch(10),~
               at (18,71), fac(hex(8c)), qty$(14)               , ch(10),~
               at (19,71), fac(hex(8c)), qty$(15)               , ch(10),~
               at (20,71), fac(hex(8c)), qty$(16)               , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
            at (22,02),                                                  ~
            "1)Start Over  4)Uses For Part              7)Uses For Job Pa~
        ~rt  13)Instructions",                                            ~
            at (23,02),                                                  ~
            "2)First       5)Sources For Part           8)Srces For Job P~
        ~art 15)Print Screen",                                            ~
            at (24,02),                                                  ~
            "3)Next                        10)Show Path of Trace         ~
        ~    16)Return      ",                                            ~
                                                                         ~
               keys(hex(010203040507080a0d0f10)),                        ~
               key (keyhit%)

               if keyhit% <> 13 then L42545
                  call "MANUAL" ("PIPTRACE")
                  goto L42030

L42545:        if keyhit% <> 15 then L42565
                  call "PRNTSCRN"
                  goto L42030

L42565:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        REM *************************************************************

        showpipin:

            inpmessage$ = "Position cursor to a line and press (6)"
            init (" ")  str(line2$,1%,60%)
            str(line2$,1%,60%) = partdescr$
L43025:     accept                                                       ~
               at (01,02),                                               ~
                  "Show Sources of Part",                                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), col_head$(2%,1%)       , ch(79),~
               at (04,02), fac(hex(ac)), col_head$(2%,2%)       , ch(79),~
                                                                         ~
               at ( 5,02), fac(hex(8e)), where$( 1)             , ch(10),~
               at ( 6,02), fac(hex(8e)), where$( 2)             , ch(10),~
               at ( 7,02), fac(hex(8e)), where$( 3)             , ch(10),~
               at ( 8,02), fac(hex(8e)), where$( 4)             , ch(10),~
               at ( 9,02), fac(hex(8e)), where$( 5)             , ch(10),~
               at (10,02), fac(hex(8e)), where$( 6)             , ch(10),~
               at (11,02), fac(hex(8e)), where$( 7)             , ch(10),~
               at (12,02), fac(hex(8e)), where$( 8)             , ch(10),~
               at (13,02), fac(hex(8e)), where$( 9)             , ch(10),~
               at (14,02), fac(hex(8e)), where$(10)             , ch(10),~
               at (15,02), fac(hex(8e)), where$(11)             , ch(10),~
               at (16,02), fac(hex(8e)), where$(12)             , ch(10),~
               at (17,02), fac(hex(8e)), where$(13)             , ch(10),~
               at (18,02), fac(hex(8e)), where$(14)             , ch(10),~
               at (19,02), fac(hex(8e)), where$(15)             , ch(10),~
               at (20,02), fac(hex(8e)), where$(16)             , ch(10),~
                                                                         ~
               at ( 5,13), fac(hex(8c)), tag$( 1)               , ch(19),~
               at ( 6,13), fac(hex(8c)), tag$( 2)               , ch(19),~
               at ( 7,13), fac(hex(8c)), tag$( 3)               , ch(19),~
               at ( 8,13), fac(hex(8c)), tag$( 4)               , ch(19),~
               at ( 9,13), fac(hex(8c)), tag$( 5)               , ch(19),~
               at (10,13), fac(hex(8c)), tag$( 6)               , ch(19),~
               at (11,13), fac(hex(8c)), tag$( 7)               , ch(19),~
               at (12,13), fac(hex(8c)), tag$( 8)               , ch(19),~
               at (13,13), fac(hex(8c)), tag$( 9)               , ch(19),~
               at (14,13), fac(hex(8c)), tag$(10)               , ch(19),~
               at (15,13), fac(hex(8c)), tag$(11)               , ch(19),~
               at (16,13), fac(hex(8c)), tag$(12)               , ch(19),~
               at (17,13), fac(hex(8c)), tag$(13)               , ch(19),~
               at (18,13), fac(hex(8c)), tag$(14)               , ch(19),~
               at (19,13), fac(hex(8c)), tag$(15)               , ch(19),~
               at (20,13), fac(hex(8c)), tag$(16)               , ch(19),~
                                                                         ~
               at ( 5,33), fac(hex(8c)), c_v_$( 1)              , ch(19),~
               at ( 6,33), fac(hex(8c)), c_v_$( 2)              , ch(19),~
               at ( 7,33), fac(hex(8c)), c_v_$( 3)              , ch(19),~
               at ( 8,33), fac(hex(8c)), c_v_$( 4)              , ch(19),~
               at ( 9,33), fac(hex(8c)), c_v_$( 5)              , ch(19),~
               at (10,33), fac(hex(8c)), c_v_$( 6)              , ch(19),~
               at (11,33), fac(hex(8c)), c_v_$( 7)              , ch(19),~
               at (12,33), fac(hex(8c)), c_v_$( 8)              , ch(19),~
               at (13,33), fac(hex(8c)), c_v_$( 9)              , ch(19),~
               at (14,33), fac(hex(8c)), c_v_$(10)              , ch(19),~
               at (15,33), fac(hex(8c)), c_v_$(11)              , ch(19),~
               at (16,33), fac(hex(8c)), c_v_$(12)              , ch(19),~
               at (17,33), fac(hex(8c)), c_v_$(13)              , ch(19),~
               at (18,33), fac(hex(8c)), c_v_$(14)              , ch(19),~
               at (19,33), fac(hex(8c)), c_v_$(15)              , ch(19),~
               at (20,33), fac(hex(8c)), c_v_$(16)              , ch(19),~
                                                                         ~
               at ( 5,53), fac(hex(8c)), ds$( 1)                , ch(08),~
               at ( 6,53), fac(hex(8c)), ds$( 2)                , ch(08),~
               at ( 7,53), fac(hex(8c)), ds$( 3)                , ch(08),~
               at ( 8,53), fac(hex(8c)), ds$( 4)                , ch(08),~
               at ( 9,53), fac(hex(8c)), ds$( 5)                , ch(08),~
               at (10,53), fac(hex(8c)), ds$( 6)                , ch(08),~
               at (11,53), fac(hex(8c)), ds$( 7)                , ch(08),~
               at (12,53), fac(hex(8c)), ds$( 8)                , ch(08),~
               at (13,53), fac(hex(8c)), ds$( 9)                , ch(08),~
               at (14,53), fac(hex(8c)), ds$(10)                , ch(08),~
               at (15,53), fac(hex(8c)), ds$(11)                , ch(08),~
               at (16,53), fac(hex(8c)), ds$(12)                , ch(08),~
               at (17,53), fac(hex(8c)), ds$(13)                , ch(08),~
               at (18,53), fac(hex(8c)), ds$(14)                , ch(08),~
               at (19,53), fac(hex(8c)), ds$(15)                , ch(08),~
               at (20,53), fac(hex(8c)), ds$(16)                , ch(08),~
                                                                         ~
               at ( 5,62), fac(hex(8c)), di$( 1)                , ch(08),~
               at ( 6,62), fac(hex(8c)), di$( 2)                , ch(08),~
               at ( 7,62), fac(hex(8c)), di$( 3)                , ch(08),~
               at ( 8,62), fac(hex(8c)), di$( 4)                , ch(08),~
               at ( 9,62), fac(hex(8c)), di$( 5)                , ch(08),~
               at (10,62), fac(hex(8c)), di$( 6)                , ch(08),~
               at (11,62), fac(hex(8c)), di$( 7)                , ch(08),~
               at (12,62), fac(hex(8c)), di$( 8)                , ch(08),~
               at (13,62), fac(hex(8c)), di$( 9)                , ch(08),~
               at (14,62), fac(hex(8c)), di$(10)                , ch(08),~
               at (15,62), fac(hex(8c)), di$(11)                , ch(08),~
               at (16,62), fac(hex(8c)), di$(12)                , ch(08),~
               at (17,62), fac(hex(8c)), di$(13)                , ch(08),~
               at (18,62), fac(hex(8c)), di$(14)                , ch(08),~
               at (19,62), fac(hex(8c)), di$(15)                , ch(08),~
               at (20,62), fac(hex(8c)), di$(16)                , ch(08),~
                                                                         ~
               at ( 5,71), fac(hex(8c)), quan$( 1)              , ch(10),~
               at ( 6,71), fac(hex(8c)), quan$( 2)              , ch(10),~
               at ( 7,71), fac(hex(8c)), quan$( 3)              , ch(10),~
               at ( 8,71), fac(hex(8c)), quan$( 4)              , ch(10),~
               at ( 9,71), fac(hex(8c)), quan$( 5)              , ch(10),~
               at (10,71), fac(hex(8c)), quan$( 6)              , ch(10),~
               at (11,71), fac(hex(8c)), quan$( 7)              , ch(10),~
               at (12,71), fac(hex(8c)), quan$( 8)              , ch(10),~
               at (13,71), fac(hex(8c)), quan$( 9)              , ch(10),~
               at (14,71), fac(hex(8c)), quan$(10)              , ch(10),~
               at (15,71), fac(hex(8c)), quan$(11)              , ch(10),~
               at (16,71), fac(hex(8c)), quan$(12)              , ch(10),~
               at (17,71), fac(hex(8c)), quan$(13)              , ch(10),~
               at (18,71), fac(hex(8c)), quan$(14)              , ch(10),~
               at (19,71), fac(hex(8c)), quan$(15)              , ch(10),~
               at (20,71), fac(hex(8c)), quan$(16)              , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
            at (22,02),                                                  ~
            "1)Start Over  4)Uses For Part  6)Kit List                   ~
        ~    13)Instructions",                                            ~
            at (23,02),                                                  ~
            "2)First                                                     ~
        ~    15)Print Screen",                                            ~
            at (24,02),                                                  ~
            "3)Next                        10)Show Path of Trace         ~
        ~    16)Return      ",                                            ~
                                                                         ~
               keys(hex(0001020304060a0d0f10)),                          ~
               key (keyhit%)

               if keyhit% <> 13 then L43610
                  call "MANUAL" ("PIPTRACE")
                  goto L43025

L43610:        if keyhit% <> 15 then L43630
                  call "PRNTSCRN"
                  goto L43025

L43630:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************

        showpipout1:

L44020:     inpmessage$ = "Position cursor to a line and press (6), (7), ~
        ~OR (8)."
            init (" ")  str(line2$,1%,60%)
            str(line2$,1%,60%) = partdescr$

            accept                                                       ~
               at (01,02),                                               ~
                  "Show Uses of Part",                                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), col_head$(3%,1%)       , ch(79),~
               at (04,02), fac(hex(ac)), col_head$(3%,2%)       , ch(79),~
                                                                         ~
               at ( 5,02), fac(hex(8c)), tag$( 1)               , ch(19),~
               at ( 6,02), fac(hex(8c)), tag$( 2)               , ch(19),~
               at ( 7,02), fac(hex(8c)), tag$( 3)               , ch(19),~
               at ( 8,02), fac(hex(8c)), tag$( 4)               , ch(19),~
               at ( 9,02), fac(hex(8c)), tag$( 5)               , ch(19),~
               at (10,02), fac(hex(8c)), tag$( 6)               , ch(19),~
               at (11,02), fac(hex(8c)), tag$( 7)               , ch(19),~
               at (12,02), fac(hex(8c)), tag$( 8)               , ch(19),~
               at (13,02), fac(hex(8c)), tag$( 9)               , ch(19),~
               at (14,02), fac(hex(8c)), tag$(10)               , ch(19),~
               at (15,02), fac(hex(8c)), tag$(11)               , ch(19),~
               at (16,02), fac(hex(8c)), tag$(12)               , ch(19),~
               at (17,02), fac(hex(8c)), tag$(13)               , ch(19),~
               at (18,02), fac(hex(8c)), tag$(14)               , ch(19),~
               at (19,02), fac(hex(8c)), tag$(15)               , ch(19),~
               at (20,02), fac(hex(8c)), tag$(16)               , ch(19),~
                                                                         ~
               at ( 5,22), fac(hex(8c)), quan$( 1)              , ch(10),~
               at ( 6,22), fac(hex(8c)), quan$( 2)              , ch(10),~
               at ( 7,22), fac(hex(8c)), quan$( 3)              , ch(10),~
               at ( 8,22), fac(hex(8c)), quan$( 4)              , ch(10),~
               at ( 9,22), fac(hex(8c)), quan$( 5)              , ch(10),~
               at (10,22), fac(hex(8c)), quan$( 6)              , ch(10),~
               at (11,22), fac(hex(8c)), quan$( 7)              , ch(10),~
               at (12,22), fac(hex(8c)), quan$( 8)              , ch(10),~
               at (13,22), fac(hex(8c)), quan$( 9)              , ch(10),~
               at (14,22), fac(hex(8c)), quan$(10)              , ch(10),~
               at (15,22), fac(hex(8c)), quan$(11)              , ch(10),~
               at (16,22), fac(hex(8c)), quan$(12)              , ch(10),~
               at (17,22), fac(hex(8c)), quan$(13)              , ch(10),~
               at (18,22), fac(hex(8c)), quan$(14)              , ch(10),~
               at (19,22), fac(hex(8c)), quan$(15)              , ch(10),~
               at (20,22), fac(hex(8c)), quan$(16)              , ch(10),~
                                                                         ~
               at ( 5,33), fac(hex(8c)), di$( 1)                , ch(08),~
               at ( 6,33), fac(hex(8c)), di$( 2)                , ch(08),~
               at ( 7,33), fac(hex(8c)), di$( 3)                , ch(08),~
               at ( 8,33), fac(hex(8c)), di$( 4)                , ch(08),~
               at ( 9,33), fac(hex(8c)), di$( 5)                , ch(08),~
               at (10,33), fac(hex(8c)), di$( 6)                , ch(08),~
               at (11,33), fac(hex(8c)), di$( 7)                , ch(08),~
               at (12,33), fac(hex(8c)), di$( 8)                , ch(08),~
               at (13,33), fac(hex(8c)), di$( 9)                , ch(08),~
               at (14,33), fac(hex(8c)), di$(10)                , ch(08),~
               at (15,33), fac(hex(8c)), di$(11)                , ch(08),~
               at (16,33), fac(hex(8c)), di$(12)                , ch(08),~
               at (17,33), fac(hex(8c)), di$(13)                , ch(08),~
               at (18,33), fac(hex(8c)), di$(14)                , ch(08),~
               at (19,33), fac(hex(8c)), di$(15)                , ch(08),~
               at (20,33), fac(hex(8c)), di$(16)                , ch(08),~
                                                                         ~
               at (05,54), fac(hex(8e)), p_c_$( 1)              , ch(27),~
               at (06,54), fac(hex(8e)), p_c_$( 2)              , ch(27),~
               at (07,54), fac(hex(8e)), p_c_$( 3)              , ch(27),~
               at (08,54), fac(hex(8e)), p_c_$( 4)              , ch(27),~
               at (09,54), fac(hex(8e)), p_c_$( 5)              , ch(27),~
               at (10,54), fac(hex(8e)), p_c_$( 6)              , ch(27),~
               at (11,54), fac(hex(8e)), p_c_$( 7)              , ch(27),~
               at (12,54), fac(hex(8e)), p_c_$( 8)              , ch(27),~
               at (13,54), fac(hex(8e)), p_c_$( 9)              , ch(27),~
               at (14,54), fac(hex(8e)), p_c_$(10)              , ch(27),~
               at (15,54), fac(hex(8e)), p_c_$(11)              , ch(27),~
               at (16,54), fac(hex(8e)), p_c_$(12)              , ch(27),~
               at (17,54), fac(hex(8e)), p_c_$(13)              , ch(27),~
               at (18,54), fac(hex(8e)), p_c_$(14)              , ch(27),~
               at (19,54), fac(hex(8e)), p_c_$(15)              , ch(27),~
               at (20,54), fac(hex(8e)), p_c_$(16)              , ch(27),~
                                                                         ~
               at ( 5,42), fac(hex(8e)), where$( 1)             , ch(10),~
               at ( 6,42), fac(hex(8e)), where$( 2)             , ch(10),~
               at ( 7,42), fac(hex(8e)), where$( 3)             , ch(10),~
               at ( 8,42), fac(hex(8e)), where$( 4)             , ch(10),~
               at ( 9,42), fac(hex(8e)), where$( 5)             , ch(10),~
               at (10,42), fac(hex(8e)), where$( 6)             , ch(10),~
               at (11,42), fac(hex(8e)), where$( 7)             , ch(10),~
               at (12,42), fac(hex(8e)), where$( 8)             , ch(10),~
               at (13,42), fac(hex(8e)), where$( 9)             , ch(10),~
               at (14,42), fac(hex(8e)), where$(10)             , ch(10),~
               at (15,42), fac(hex(8e)), where$(11)             , ch(10),~
               at (16,42), fac(hex(8e)), where$(12)             , ch(10),~
               at (17,42), fac(hex(8e)), where$(13)             , ch(10),~
               at (18,42), fac(hex(8e)), where$(14)             , ch(10),~
               at (19,42), fac(hex(8e)), where$(15)             , ch(10),~
               at (20,42), fac(hex(8e)), where$(16)             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
            at (22,02),                                                  ~
            "1)Start Over                   6)Kit List  7)Uses For Job Pa~
        ~rt  13)Instructions",                                            ~
            at (23,02),                                                  ~
            "2)First       5)Sources For Part           8)Srces For Job P~
        ~art 15)Print Screen",                                            ~
            at (24,02),                                                  ~
            "3)Next        9)Descr Toggle  10)Show Path of Trace         ~
        ~    16)Return      ",                                            ~
                                                                         ~
               keys(hex(0001020305060708090a0d0f10)),                    ~
               key (keyhit%)

               if keyhit% <> 13 then L44605
                  call "MANUAL" ("PIPTRACE")
                  goto L44020

L44605:        if keyhit% <> 15 then L44625
                  call "PRNTSCRN"
                  goto L44020

L44625:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************

        showpipout2:

L45020:     inpmessage$ = "Position cursor to a line and press (6), (7), ~
        ~OR (8)."
            init (" ")  str(line2$,1%,60%)
            str(line2$,1%,60%) = partdescr$

            accept                                                       ~
               at (01,02),                                               ~
                  "Show Uses of Part",                                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), col_head$(3%,1%)       , ch(79),~
               at (04,02), fac(hex(ac)), col_head$(3%,2%)       , ch(79),~
                                                                         ~
               at ( 5,02), fac(hex(8c)), tag$( 1)               , ch(19),~
               at ( 6,02), fac(hex(8c)), tag$( 2)               , ch(19),~
               at ( 7,02), fac(hex(8c)), tag$( 3)               , ch(19),~
               at ( 8,02), fac(hex(8c)), tag$( 4)               , ch(19),~
               at ( 9,02), fac(hex(8c)), tag$( 5)               , ch(19),~
               at (10,02), fac(hex(8c)), tag$( 6)               , ch(19),~
               at (11,02), fac(hex(8c)), tag$( 7)               , ch(19),~
               at (12,02), fac(hex(8c)), tag$( 8)               , ch(19),~
               at (13,02), fac(hex(8c)), tag$( 9)               , ch(19),~
               at (14,02), fac(hex(8c)), tag$(10)               , ch(19),~
               at (15,02), fac(hex(8c)), tag$(11)               , ch(19),~
               at (16,02), fac(hex(8c)), tag$(12)               , ch(19),~
               at (17,02), fac(hex(8c)), tag$(13)               , ch(19),~
               at (18,02), fac(hex(8c)), tag$(14)               , ch(19),~
               at (19,02), fac(hex(8c)), tag$(15)               , ch(19),~
               at (20,02), fac(hex(8c)), tag$(16)               , ch(19),~
                                                                         ~
               at ( 5,22), fac(hex(8c)), quan$( 1)              , ch(10),~
               at ( 6,22), fac(hex(8c)), quan$( 2)              , ch(10),~
               at ( 7,22), fac(hex(8c)), quan$( 3)              , ch(10),~
               at ( 8,22), fac(hex(8c)), quan$( 4)              , ch(10),~
               at ( 9,22), fac(hex(8c)), quan$( 5)              , ch(10),~
               at (10,22), fac(hex(8c)), quan$( 6)              , ch(10),~
               at (11,22), fac(hex(8c)), quan$( 7)              , ch(10),~
               at (12,22), fac(hex(8c)), quan$( 8)              , ch(10),~
               at (13,22), fac(hex(8c)), quan$( 9)              , ch(10),~
               at (14,22), fac(hex(8c)), quan$(10)              , ch(10),~
               at (15,22), fac(hex(8c)), quan$(11)              , ch(10),~
               at (16,22), fac(hex(8c)), quan$(12)              , ch(10),~
               at (17,22), fac(hex(8c)), quan$(13)              , ch(10),~
               at (18,22), fac(hex(8c)), quan$(14)              , ch(10),~
               at (19,22), fac(hex(8c)), quan$(15)              , ch(10),~
               at (20,22), fac(hex(8c)), quan$(16)              , ch(10),~
                                                                         ~
               at ( 5,33), fac(hex(8c)), di$( 1)                , ch(08),~
               at ( 6,33), fac(hex(8c)), di$( 2)                , ch(08),~
               at ( 7,33), fac(hex(8c)), di$( 3)                , ch(08),~
               at ( 8,33), fac(hex(8c)), di$( 4)                , ch(08),~
               at ( 9,33), fac(hex(8c)), di$( 5)                , ch(08),~
               at (10,33), fac(hex(8c)), di$( 6)                , ch(08),~
               at (11,33), fac(hex(8c)), di$( 7)                , ch(08),~
               at (12,33), fac(hex(8c)), di$( 8)                , ch(08),~
               at (13,33), fac(hex(8c)), di$( 9)                , ch(08),~
               at (14,33), fac(hex(8c)), di$(10)                , ch(08),~
               at (15,33), fac(hex(8c)), di$(11)                , ch(08),~
               at (16,33), fac(hex(8c)), di$(12)                , ch(08),~
               at (17,33), fac(hex(8c)), di$(13)                , ch(08),~
               at (18,33), fac(hex(8c)), di$(14)                , ch(08),~
               at (19,33), fac(hex(8c)), di$(15)                , ch(08),~
               at (20,33), fac(hex(8c)), di$(16)                , ch(08),~
                                                                         ~
               at (05,54), fac(hex(8e)), p_c_descr$( 1)         , ch(27),~
               at (06,54), fac(hex(8e)), p_c_descr$( 2)         , ch(27),~
               at (07,54), fac(hex(8e)), p_c_descr$( 3)         , ch(27),~
               at (08,54), fac(hex(8e)), p_c_descr$( 4)         , ch(27),~
               at (09,54), fac(hex(8e)), p_c_descr$( 5)         , ch(27),~
               at (10,54), fac(hex(8e)), p_c_descr$( 6)         , ch(27),~
               at (11,54), fac(hex(8e)), p_c_descr$( 7)         , ch(27),~
               at (12,54), fac(hex(8e)), p_c_descr$( 8)         , ch(27),~
               at (13,54), fac(hex(8e)), p_c_descr$( 9)         , ch(27),~
               at (14,54), fac(hex(8e)), p_c_descr$(10)         , ch(27),~
               at (15,54), fac(hex(8e)), p_c_descr$(11)         , ch(27),~
               at (16,54), fac(hex(8e)), p_c_descr$(12)         , ch(27),~
               at (17,54), fac(hex(8e)), p_c_descr$(13)         , ch(27),~
               at (18,54), fac(hex(8e)), p_c_descr$(14)         , ch(27),~
               at (19,54), fac(hex(8e)), p_c_descr$(15)         , ch(27),~
               at (20,54), fac(hex(8e)), p_c_descr$(16)         , ch(27),~
                                                                         ~
               at ( 5,42), fac(hex(8e)), where$( 1)             , ch(10),~
               at ( 6,42), fac(hex(8e)), where$( 2)             , ch(10),~
               at ( 7,42), fac(hex(8e)), where$( 3)             , ch(10),~
               at ( 8,42), fac(hex(8e)), where$( 4)             , ch(10),~
               at ( 9,42), fac(hex(8e)), where$( 5)             , ch(10),~
               at (10,42), fac(hex(8e)), where$( 6)             , ch(10),~
               at (11,42), fac(hex(8e)), where$( 7)             , ch(10),~
               at (12,42), fac(hex(8e)), where$( 8)             , ch(10),~
               at (13,42), fac(hex(8e)), where$( 9)             , ch(10),~
               at (14,42), fac(hex(8e)), where$(10)             , ch(10),~
               at (15,42), fac(hex(8e)), where$(11)             , ch(10),~
               at (16,42), fac(hex(8e)), where$(12)             , ch(10),~
               at (17,42), fac(hex(8e)), where$(13)             , ch(10),~
               at (18,42), fac(hex(8e)), where$(14)             , ch(10),~
               at (19,42), fac(hex(8e)), where$(15)             , ch(10),~
               at (20,42), fac(hex(8e)), where$(16)             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
            at (22,02),                                                  ~
            "1)Start Over                   6)Kit List  7)Uses For Job Pa~
        ~rt  13)Instructions",                                            ~
            at (23,02),                                                  ~
            "2)First       5)Sources For Part           8)Srces For Job P~
        ~art 15)Print Screen",                                            ~
            at (24,02),                                                  ~
            "3)Next        9)Descr Toggle  10)Show Path of Trace         ~
        ~    16)Return      ",                                            ~
                                                                         ~
               keys(hex(0001020305060708090a0d0f10)),                    ~
               key (keyhit%)

               if keyhit% <> 13 then L45605
                  call "MANUAL" ("PIPTRACE")
                  goto L45020

L45605:        if keyhit% <> 15 then L45625
                  call "PRNTSCRN"
                  goto L45020

L45625:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end
