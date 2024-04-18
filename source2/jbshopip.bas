        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB    SSS   H   H   OOO   PPPP   IIIII  PPPP    *~
            *    J    B   B  S      H   H  O   O  P   P    I    P   P   *~
            *    J    BBBB    SSS   HHHHH  O   O  PPPP     I    PPPP    *~
            *  J J    B   B      S  H   H  O   O  P        I    P       *~
            *   J     BBBB    SSS   H   H   OOO   P      IIIII  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBSHOPIP - THIS SUBROUTINE CAN CALL ITSELF TO NEST INTO   *~
            *            TRACING THE PIP DATA FOR A PARTICULAR JOB      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/14/84 ! ORIGINAL                                 ! JRW *~
            * 07/02/87 ! HNYMASTR record length change for stan-  ! JIM *~
            *          !   dard cost. PIPMASTR & HNYMAST2 removed.!     *~
            * 11/30/88 ! Added check for 'RO' in TAG$             ! MJB *~
            * 11/11/92 ! PRR 12674  Corrected Dates; value & colmn! JDH *~
            *          ! Changed to more closely meet standards.  !     *~
            *          ! Only JOs & WOs explode from Origin Screen!     *~
            * 11/19/92 ! Added call to VBKLIDSP for POs.          ! JDH *~
            * 10/12/93 ! Purchase Jobs Project - Added recognition! JBK *~
            *          !   of 'BW' and 'RW' tags.  Added 'BW' and !     *~
            *          !   'RW' to explode from Origin Screen.    !     *~
            *          !   Purchase Jobs will call VBKLIDSP Also. !     *~
            *          ! Misc. - Added '%' in various places.     !     *~
            * 07/05/94 ! Added missing arg in DESCRIBE.           ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            eol$16,                                                      ~
            errormsg$79,                 /* Error message              */~
            hdr_date$8,                  /* Column Header - Date       */~
            hdr_date_in$8,               /* Column Header - Date In    */~
            hdr_date_out$8,              /* Column Header - Date Out   */~
            hdr_origin$16,               /* Column Header - Part Origin*/~
            hdr_pc$25,                   /* Column Header - Part Code  */~
            hdr_pd$32,                   /* Column Header - Part Descr */~
            hdr_pip$19,                  /* Column Header - PIP Add Tag*/~
            hdr_qty$10,                  /* Column Header - Quantity   */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            job$8,                       /* Job Number                 */~
            line2$79,                    /* Second Line of Screen Headr*/~
            part$25,                                                     ~
            pf2fac$1, pf2msg$8,                                          ~
            pf5fac$1, pf5msg$8,                                          ~
            pf$(3)79,                    /* PF Prompts                 */~
            pfkeys$17,                                                   ~
            pldate$6,                    /* Start of Planning Calendar */~
            pokey$19,                    /* PO Number and Line Number  */~
            s$2,                         /* PIP Tag Type               */~
            vendor$9                     /* Vendor Code                */

        dim piparray$(100)25,                                            ~
            piparray%(100),                                              ~
            blankline$79,                                                ~
            loadmsg$79,                                                  ~
            inpmessage$79

        dim pipout$(20)19,                                               ~
            part1$25,                                                    ~
            jobdescr$38,                                                 ~
            plowkey$60,                                                  ~
            p$19,                                                        ~
            part$  (20)25,                                               ~
            partdescr$(20)32,                                            ~
            do$(20)8,                                                    ~
            qty$   (20)10

        dim quan$(20)10,                                                 ~
            tag$(20)19,                                                  ~
            di$(20)8,                                                    ~
            ds$(20)8,                                                    ~
            where$(20)16,                                                ~
            plowkey2$100,                                                ~
            partdescr$46,                                                ~
            part2$25,                                                    ~
            tag$19

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! PIPIN    ! Planned inventory additions detail  feed *~
            * #03 ! PIPOUT   ! Planned inventory use detail rec  feeds  *~
            * #05 ! HNYMASTR ! Inventory Master File                    *~
            * #07 ! JBMASTR2 ! Production job master file               *~
            * #08 ! VBKMASTR ! Backlog main header file                 *~
            * #09 ! VBKLINES ! Backlog line item file                   *~
            * #10 ! STORNAME ! Store Info File - Name/Address           *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #02, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #03, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #05, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #07, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

            select #08, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  10,  keylen =  16

            select #09, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =  28,                     ~
                        alt key 1, keypos = 333, keylen = 20, dup

            select #10, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   3

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, "SHARE", f2%(1%), rslt$(1%), axd$(1%))
            call "OPENFILE" (#02, "SHARE", f2%(2%), rslt$(2%), axd$(2%))
            call "OPENFILE" (#03, "SHARE", f2%(3%), rslt$(3%), axd$(3%))
            call "OPENFILE" (#05, "SHARE", f2%(5%), rslt$(5%), axd$(5%))
            call "OPENFILE" (#07, "SHARE", f2%(7%), rslt$(7%), axd$(7%))
            call "OPENFILE" (#08, "SHARE", f2%(8%), rslt$(8%), axd$(8%))
            call "OPENFILE" (#09, "SHARE", f2%(9%), rslt$(9%), axd$(9%))
            call "OPENFILE" (#10, "SHARE", f2%(10%),rslt$(10%),axd$(10%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            pfkeys$ = hex(01ffffffffffff0809ffffff0dff0f1000)
            eol$ = hex(84) & "END OF LISTING"
            str(line2$,62%) = "JBSHOPIP: " & str(cms2v$,,8%)
            pf2msg$ = "(2)First" : pf5msg$ = "(5)Next"

            call "READ100" (#1, "MONTHS OPEN", f1%(1%))
            if f1%(1%) = 0% then L65000
            get #1, using L09100, pldate$
L09100:         FMT XX(32), CH(6)

            loadmsg$ = "Enter a Part or Job and press PF4 or PF5 for " & ~
                       "the search method."

            hdr_date_in$    = "Date  In"
            hdr_date_out$   = "Out Date"
            hdr_date$       = "  Date  "
            hdr_origin$     = "Origin of Part"
            hdr_pc$         = "Part Code"
            hdr_pd$         = "Part Decription"
            hdr_pip$        = "PIP Addition Tag"
            hdr_qty$        = "  Quantity"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") inpmessage$, piparray$()
            mat piparray% = zer
L10180:         gosub get_part_or_job    /* AT 44000  */
                if keyhit% = 16% then L65000
                current% = 1%
                gosub L50000
                     if errormsg$ <> " " then L10180
                if keyhit% = 5% then piparray$(current%) = "JOB ORDER: "&~
                                                             job$
                if keyhit% = 4% then piparray$(current%) = part$
                if keyhit% = 5% then gosub'201
                if keyhit% = 4% then gosub'202

        goto inputmode

        REM *************************************************************

        deffn'201
L11005:         init (hex(00)) plowkey$
                str(plowkey$,,19%) = piparray$(current%)
                gosub pf2_off
L11120:         gosub loadpipout
L11150: REM Set appropriate PF keys
                if f1%(3%) = 0% then gosub pf5_off else gosub pf5_on
                gosub showpipout
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then goto L11005
                      if keyhit% <>  5% then goto L11270
                          gosub pf2_on
                          goto L11120
L11270:               if keyhit%  = 16% then return
                      if keyhit%  =  8% then gosub showarray
                      if keyhit% <>  0% then goto L11150
                      array% = min(max(cursor%(1%)-4%,1%),16%)
                      if part$(array%) = " " or part$(array%) = eol$ then~
                          goto L11150
                      current% = current% + 1%
                      piparray%(current%) = 0%
                      piparray$(current%) = part$(array%)
                      gosub'202
                      piparray$(current%) = " "
                      current% = current% - 1%
                 goto L11005

        REM *************************************************************

        deffn'202
L12030:         init (hex(00)) plowkey2$
                str(plowkey2$,,25%) = piparray$(current%)
                gosub pf2_off
L12060:         gosub loadpipin
L12070: REM Set appropriate PF keys
                if f1%(2%) = 0% then gosub pf5_off else gosub pf5_on
                gosub showpipin
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then goto L12030
                      if keyhit% <>  5% then goto L12150
                          gosub pf2_on
                          goto L12060
L12150:               if keyhit%  = 16% then return
                      if keyhit%  =  8% then gosub showarray
                      if keyhit%  =  9% then gosub call_vbklidsp
                      if keyhit% <>  0% then goto L12070
                      array% = min(max(cursor%(1%)-4%,1%),16%)
                      if str(where$(array%),,2%) = "WO" or               ~
                         str(where$(array%),,2%) = "JO" then L12270
                      if str(where$(array%),,14%) = "BUY ORDER - PJ"     ~
                                                               then L12270
                      if str(where$(array%),,16%) = "REQUISITION - PJ"   ~
                                                               then L12270
                      if str(where$(array%),,12%) = "PURCHASE JOB"       ~
                                                               then L12270
                          goto L12070
L12270:               current% = current% + 1%
                      piparray%(current%) = 1%
                      piparray$(current%) = tag$(array%)
                      gosub'201
                      piparray$(current%) = " "
                      current% = current% - 1%
                 goto L12030

        call_vbklidsp
*        First is this a PO
            array% = min( max(cursor%(1)-4%,1%) , 16%)
            if str(tag$(array%),,2%) = "PO" then L12430
            if str(tag$(array%),,2%) = "JO" and                          ~
                             str(tag$(array%),12%,2%) = "PJ" then L12520
                return

L12430
*        Next find out the vendor
            call "REDALT0" (#08, str(tag$(array%),3%,14%), 1%, f1%(8%))
            if f1%(8%) = 0% then return
                get #8 using L12470, vendor$
L12470:              FMT CH(9)
                pokey$ = str(tag$(array%),3%,14%) & "  " &               ~
                                                 str(tag$(array%),17%,3%)
                goto L12590

L12520
*        Find vendor from JBMASTR2 for Purchase Job
            call "READ100" (#7, str(tag$(array%),12%,8%), f1%(7%))
                if f1%(7%) = 0% then return
            get #7 using L12560, vendor$, pokey$
L12560:         FMT POS(108), CH(9), CH(19)
            if vendor$ = " " or pokey$ = " " then return

L12590
*        Now the call (Ringgggggg)
            call "VBKLIDSP" (vendor$, str(pokey$,1%,16%),                ~
                             str(pokey$,17%,3%), #09, #05, #10)
            return

        REM **************************************************************

        showarray :   /* SHOW THE NESTED SEARCH ARRAY */
L13120:         gosub'102   /* ACCEPT STATEMENT */
                     if keyhit% =  1% then gosub startover
                     if keyhit% = 16% then return
                goto L13120

        REM ************* START OVER SCREEN ******************************
        startover:         REM ALLOW USER OPPORTUNITY TO START OVER.

            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        loadpipout:
                     line% = 0%
                     job$ = str(plowkey$,12%,8%)
                     jobdescr$ = " "
                     call "DESCRIBE" (#07, job$, jobdescr$, 0%, f1%(7%))
                     if f1%(7%) = 1% then L30250
                          jobdescr$ = piparray$(current%) /* Not a Job */
                          goto L30300
L30250:              l% = max(len(job$),1) : m% = max(len(jobdescr$),1)
                     jobdescr$ = str(job$,,l%) & ", " & str(jobdescr$,,m%)
L30300:             init (" ") part$(),qty$(),partdescr$(),pipout$(),do$()
L30350:              call "PLOWNEXT" (#03, plowkey$, 19%, f1%(3%))
                          if f1%(3%) = 0% then L31100
                                         /*ELSE*/
                                     get #03 using L30950,p$,part1$,do%,qty
                                     line% = line% + 1%
                                     pipout$(line%) = p$
                    call "DATE" addr("G+", pldate$, do% - 1%, do$(line%),~
                                                                     err%)
                    call "DATEFMT" (do$(line%))
                                     part$(line%) = part1$
                                     partdescr$(line%) = "NOT ON FILE"
                call "DESCRIBE" (#05, part1$, partdescr$(line%), 0%,     ~
                                                                 f1%(5%))
                call "CONVERT" (qty, 2.4, qty$(line%))
L30950:                           FMT CH(19),CH(25),BI(04),XX(08),PD(14,4)
                                     if line% < 16% then L30350
                                     return
L31100:  part$(line% + 1%) = eol$
         return

        loadpipin:
            part2$ = plowkey2$
            partdescr$ = " "
            call "DESCRIBE" (#05, part2$, partdescr$, 0%, f1%(5%))
            l% = max(len(part2$),1) : m% = max(len(partdescr$),1)
            partdescr$ =str(part2$,,l%) & ", " & str(partdescr$,,m%)
            line% = 0%
            init (" ") quan$(), tag$(), di$(), ds$(), where$()
L31600:     call "PLOWALTS" (#02, plowkey2$, 1%, 25%, f1%(2%))
            if f1%(2%) = 0% then L32750

                get #02 using L31800,di%,tag$,qty,ds%
L31800:            FMT        XX(25),BI(04),CH(19), PD(14,4), BI(04)

                line% = line% + 1%
                where$(line%) = "SALES ORDER"
                s$ = str(tag$,1,2)
                if  s$ = "BO" then where$(line%) = "BUY ORDER - PO"
                if  s$ = "BW" then where$(line%) = "BUY ORDER - PJ"
                if  s$ = "PO" then where$(line%) = "PURCHASE ORDER"
                if  s$ = "JO" then where$(line%) = "JOB ORDER"
                if  s$ = "WO" then where$(line%) = "WORK ORDER"
                if  s$ = "QC" then where$(line%) = "QUALITY CONTROL"
                if  s$ = "RO" then where$(line%) = "REQUISITION - PO"
                if  s$ = "RW" then where$(line%) = "REQUISITION - PJ"
                if  s$ = "JO" and str(tag$,12%,2%) = "PJ" then           ~
                                           where$(line%) = "PURCHASE JOB"

                tag$(line%) = tag$
                call "DATE" addr("G+", pldate$, di% - 1%, di$(line%),    ~
                                                                 err%)
                call "DATEFMT" (di$(line%))
                call "DATE" addr("G+", pldate$, ds% - 1%, ds$(line%),    ~
                                                                 err%)
                call "DATEFMT" (ds$(line%))
                call "CONVERT" (qty, 2.4, quan$(line%))
                                     if line% < 16% then L31600
                                     return

L32750:  where$(line% + 1%) = eol$
         return

        pf2_on
            str(pfkeys$,2,1) = hex(02) : pf2fac$ = hex(8c) : return
        pf2_off
            str(pfkeys$,2,1) = hex(ff) : pf2fac$ = hex(9c) : return
        pf5_on
            str(pfkeys$,5,1) = hex(05) : pf5fac$ = hex(8c) : return
        pf5_off
            str(pfkeys$,5,1) = hex(ff) : pf5fac$ = hex(9c) : return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'102
            str(line2$,,60) = "Nesting of Current Job Search"

L40090:     accept                                                       ~
               at (01,02), "Show PIP Status By Job or Part",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
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
               at (03,06), fac(hex(85)), piparray$(01%)         , ch(22),~
               at (04,06), fac(hex(85)), piparray$(02%)         , ch(22),~
               at (05,06), fac(hex(85)), piparray$(03%)         , ch(22),~
               at (06,06), fac(hex(85)), piparray$(04%)         , ch(22),~
               at (07,06), fac(hex(85)), piparray$(05%)         , ch(22),~
               at (08,06), fac(hex(85)), piparray$(06%)         , ch(22),~
               at (09,06), fac(hex(85)), piparray$(07%)         , ch(22),~
               at (10,06), fac(hex(85)), piparray$(08%)         , ch(22),~
               at (11,06), fac(hex(85)), piparray$(09%)         , ch(22),~
               at (12,06), fac(hex(85)), piparray$(10%)         , ch(22),~
               at (13,06), fac(hex(85)), piparray$(11%)         , ch(22),~
               at (14,06), fac(hex(85)), piparray$(12%)         , ch(22),~
               at (15,06), fac(hex(85)), piparray$(13%)         , ch(22),~
               at (16,06), fac(hex(85)), piparray$(14%)         , ch(22),~
               at (17,06), fac(hex(85)), piparray$(15%)         , ch(22),~
               at (18,06), fac(hex(85)), piparray$(16%)         , ch(22),~
               at (19,06), fac(hex(85)), piparray$(17%)         , ch(22),~
               at (20,06), fac(hex(85)), piparray$(18%)         , ch(22),~
               at (21,06), fac(hex(85)), piparray$(19%)         , ch(22),~
               at (22,06), fac(hex(85)), piparray$(20%)         , ch(22),~
                                                                         ~
               at (03,33), fac(hex(85)), piparray$(21%)         , ch(22),~
               at (04,33), fac(hex(85)), piparray$(22%)         , ch(22),~
               at (05,33), fac(hex(85)), piparray$(23%)         , ch(22),~
               at (06,33), fac(hex(85)), piparray$(24%)         , ch(22),~
               at (07,33), fac(hex(85)), piparray$(25%)         , ch(22),~
               at (08,33), fac(hex(85)), piparray$(26%)         , ch(22),~
               at (09,33), fac(hex(85)), piparray$(27%)         , ch(22),~
               at (10,33), fac(hex(85)), piparray$(28%)         , ch(22),~
               at (11,33), fac(hex(85)), piparray$(29%)         , ch(22),~
               at (12,33), fac(hex(85)), piparray$(30%)         , ch(22),~
               at (13,33), fac(hex(85)), piparray$(31%)         , ch(22),~
               at (14,33), fac(hex(85)), piparray$(32%)         , ch(22),~
               at (15,33), fac(hex(85)), piparray$(33%)         , ch(22),~
               at (16,33), fac(hex(85)), piparray$(34%)         , ch(22),~
               at (17,33), fac(hex(85)), piparray$(35%)         , ch(22),~
               at (18,33), fac(hex(85)), piparray$(36%)         , ch(22),~
               at (19,33), fac(hex(85)), piparray$(37%)         , ch(22),~
               at (20,33), fac(hex(85)), piparray$(38%)         , ch(22),~
               at (21,33), fac(hex(85)), piparray$(39%)         , ch(22),~
               at (22,33), fac(hex(85)), piparray$(40%)         , ch(22),~
                                                                         ~
               at (03,60), fac(hex(85)), piparray$(41%)         , ch(21),~
               at (04,60), fac(hex(85)), piparray$(42%)         , ch(21),~
               at (05,60), fac(hex(85)), piparray$(43%)         , ch(21),~
               at (06,60), fac(hex(85)), piparray$(44%)         , ch(21),~
               at (07,60), fac(hex(85)), piparray$(45%)         , ch(21),~
               at (08,60), fac(hex(85)), piparray$(46%)         , ch(21),~
               at (09,60), fac(hex(85)), piparray$(47%)         , ch(21),~
               at (10,60), fac(hex(85)), piparray$(48%)         , ch(21),~
               at (11,60), fac(hex(85)), piparray$(49%)         , ch(21),~
               at (12,60), fac(hex(85)), piparray$(50%)         , ch(21),~
               at (13,60), fac(hex(85)), piparray$(51%)         , ch(21),~
               at (14,60), fac(hex(85)), piparray$(52%)         , ch(21),~
               at (15,60), fac(hex(85)), piparray$(53%)         , ch(21),~
               at (16,60), fac(hex(85)), piparray$(54%)         , ch(21),~
               at (17,60), fac(hex(85)), piparray$(55%)         , ch(21),~
               at (18,60), fac(hex(85)), piparray$(56%)         , ch(21),~
               at (19,60), fac(hex(85)), piparray$(57%)         , ch(21),~
               at (20,60), fac(hex(85)), piparray$(58%)         , ch(21),~
               at (21,60), fac(hex(85)), piparray$(59%)         , ch(21),~
               at (22,60), fac(hex(85)), piparray$(60%)         , ch(21),~
                                                                         ~
               at (23,02), fac(hex(a4)), blankline$,                     ~
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

               if keyhit% <> 13% then L41590
                  call "MANUAL" ("JBSHOPIP")
                  goto L40090

L41590:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L40090


        get_part_or_job

           str(line2$,,60%) = " "
           pf$(1%)= "                 (4)Search Additions by Part      "&~
                    "             (13)Instructions"
           pf$(2%)= "                 (5)Search Withdrawals by Job     "&~
                    "             (15)Print Screen"
           pf$(3%)= "                                                  "&~
                    "             (16)Exit Program"

L42100:   accept                                                         ~
               at (01,02), "Show PIP Status By Job or Part",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Part Code",                                  ~
               at (07,02), "Job Number",                                 ~
               at (06,15), fac(hex(81)), part$,                  ch(25), ~
               at (07,15), fac(hex(81)), job$,                   ch(08), ~
               at (21,02), fac(hex(a5)), loadmsg$,               ch(79), ~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
            keys(hex(04050d0f10)),                                       ~
            key (keyhit%)

            gosub pf1315
            if keyhit% = 15% then L42100
            return

        pf1315
            if keyhit% <> 13% then L42370
                call "MANUAL" ("JBSHOPIP")
                keyhit% = 15%
                return
L42370:     if keyhit% <> 15% then return
                call "PRNTSCRN"
                return

        REM *************************************************************

        showpipout:

           inpmessage$ = "Position cursor to a line and press <ENTER>."
           str(line2$,,60%) = "Show Parts Required for: " & jobdescr$

L43070:     accept                                                       ~
               at (01,02), "Show PIP Status By Job or Part",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02),                                               ~
                  "                                                      ~
        ~     Expected    Required",                                      ~
               at (04,02), fac(hex(ac)), hdr_pc$                , ch(25),~
               at (04,28), fac(hex(ac)), hdr_pd$                , ch(32),~
               at (04,61), fac(hex(ac)), hdr_date_out$          , ch(08),~
               at (04,71), fac(hex(ac)), hdr_qty$               , ch(10),~
                                                                         ~
               at (05,02), fac(hex(8e)), part$(01%)             , ch(25),~
               at (05,28), fac(hex(8c)), partdescr$(01%)        , ch(32),~
               at (05,61), fac(hex(8c)), do$(01%)               , ch(08),~
               at (05,71), fac(hex(8c)), qty$(01%)              , ch(10),~
               at (06,02), fac(hex(8e)), part$(02%)             , ch(25),~
               at (06,28), fac(hex(8c)), partdescr$(02%)        , ch(32),~
               at (06,61), fac(hex(8c)), do$(02%)               , ch(08),~
               at (06,71), fac(hex(8c)), qty$(02%)              , ch(10),~
               at (07,02), fac(hex(8e)), part$(03%)             , ch(25),~
               at (07,28), fac(hex(8c)), partdescr$(03%)        , ch(32),~
               at (07,61), fac(hex(8c)), do$(03%)               , ch(08),~
               at (07,71), fac(hex(8c)), qty$(03%)              , ch(10),~
               at (08,02), fac(hex(8e)), part$(04%)             , ch(25),~
               at (08,28), fac(hex(8c)), partdescr$(04%)        , ch(32),~
               at (08,61), fac(hex(8c)), do$(04%)               , ch(08),~
               at (08,71), fac(hex(8c)), qty$(04%)              , ch(10),~
               at (09,02), fac(hex(8e)), part$(05%)             , ch(25),~
               at (09,28), fac(hex(8c)), partdescr$(05%)        , ch(32),~
               at (09,61), fac(hex(8c)), do$(05%)               , ch(08),~
               at (09,71), fac(hex(8c)), qty$(05%)              , ch(10),~
               at (10,02), fac(hex(8e)), part$(06%)             , ch(25),~
               at (10,28), fac(hex(8c)), partdescr$(06%)        , ch(32),~
               at (10,61), fac(hex(8c)), do$(06%)               , ch(08),~
               at (10,71), fac(hex(8c)), qty$(06%)              , ch(10),~
               at (11,02), fac(hex(8e)), part$(07%)             , ch(25),~
               at (11,28), fac(hex(8c)), partdescr$(07%)        , ch(32),~
               at (11,61), fac(hex(8c)), do$(07%)               , ch(08),~
               at (11,71), fac(hex(8c)), qty$(07%)              , ch(10),~
               at (12,02), fac(hex(8e)), part$(08%)             , ch(25),~
               at (12,28), fac(hex(8c)), partdescr$(08%)        , ch(32),~
               at (12,61), fac(hex(8c)), do$(08%)               , ch(08),~
               at (12,71), fac(hex(8c)), qty$(08%)              , ch(10),~
               at (13,02), fac(hex(8e)), part$(09%)             , ch(25),~
               at (13,28), fac(hex(8c)), partdescr$(09%)        , ch(32),~
               at (13,61), fac(hex(8c)), do$(09%)               , ch(08),~
               at (13,71), fac(hex(8c)), qty$(09%)              , ch(10),~
               at (14,02), fac(hex(8e)), part$(10%)             , ch(25),~
               at (14,28), fac(hex(8c)), partdescr$(10%)        , ch(32),~
               at (14,61), fac(hex(8c)), do$(10%)               , ch(08),~
               at (14,71), fac(hex(8c)), qty$(10%)              , ch(10),~
               at (15,02), fac(hex(8e)), part$(11%)             , ch(25),~
               at (15,28), fac(hex(8c)), partdescr$(11%)        , ch(32),~
               at (15,61), fac(hex(8c)), do$(11%)               , ch(08),~
               at (15,71), fac(hex(8c)), qty$(11%)              , ch(10),~
               at (16,02), fac(hex(8e)), part$(12%)             , ch(25),~
               at (16,28), fac(hex(8c)), partdescr$(12%)        , ch(32),~
               at (16,61), fac(hex(8c)), do$(12%)               , ch(08),~
               at (16,71), fac(hex(8c)), qty$(12%)              , ch(10),~
               at (17,02), fac(hex(8e)), part$(13%)             , ch(25),~
               at (17,28), fac(hex(8c)), partdescr$(13%)        , ch(32),~
               at (17,61), fac(hex(8c)), do$(13%)               , ch(08),~
               at (17,71), fac(hex(8c)), qty$(13%)              , ch(10),~
               at (18,02), fac(hex(8e)), part$(14%)             , ch(25),~
               at (18,28), fac(hex(8c)), partdescr$(14%)        , ch(32),~
               at (18,61), fac(hex(8c)), do$(14%)               , ch(08),~
               at (18,71), fac(hex(8c)), qty$(14%)              , ch(10),~
               at (19,02), fac(hex(8e)), part$(15%)             , ch(25),~
               at (19,28), fac(hex(8c)), partdescr$(15%)        , ch(32),~
               at (19,61), fac(hex(8c)), do$(15%)               , ch(08),~
               at (19,71), fac(hex(8c)), qty$(15%)              , ch(10),~
               at (20,02), fac(hex(8e)), part$(16%)             , ch(25),~
               at (20,28), fac(hex(8c)), partdescr$(16%)        , ch(32),~
               at (20,61), fac(hex(8c)), do$(16%)               , ch(08),~
               at (20,71), fac(hex(8c)), qty$(16%)              , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,25), fac(pf2fac$), pf2msg$,                        ~
               at (23,35), "(8)See Job/Part Nesting",                    ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,25), fac(pf5fac$), pf5msg$,                        ~
               at (24,65), "(16)Return",                                 ~
               keys(pfkeys$), key (keyhit%)

               gosub pf1315
               if keyhit% = 15% then L43070

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        REM *************************************************************

        showpipin:

           inpmessage$ = "Position cursor to a line and press <ENTER>."
           str(line2$,,60%) = "Show Origin of: " & partdescr$

L45070:     accept                                                       ~
               at (01,02), "Show PIP Status By Job or Part",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02),                                               ~
                  "                                            Start     ~
        ~    Expected     Expected",                                      ~
               at (04,02), fac(hex(ac)), hdr_origin$            , ch(16),~
               at (04,22), fac(hex(ac)), hdr_pip$               , ch(19),~
               at (04,45), fac(hex(ac)), hdr_date$              , ch(08),~
               at (04,60), fac(hex(ac)), hdr_date_in$           , ch(08),~
               at (04,71), fac(hex(ac)), hdr_qty$               , ch(10),~
                                                                         ~
               at (05,02), fac(hex(8e)), where$(01%)            , ch(16),~
               at (05,22), fac(hex(8c)), tag$(01%)              , ch(19),~
               at (05,45), fac(hex(8c)), ds$(01%)               , ch(08),~
               at (05,60), fac(hex(8c)), di$(01%)               , ch(08),~
               at (05,71), fac(hex(8c)), quan$(01%)             , ch(10),~
               at (06,02), fac(hex(8e)), where$(02%)            , ch(16),~
               at (06,22), fac(hex(8c)), tag$(02%)              , ch(19),~
               at (06,45), fac(hex(8c)), ds$(02%)               , ch(08),~
               at (06,60), fac(hex(8c)), di$(02%)               , ch(08),~
               at (06,71), fac(hex(8c)), quan$(02%)             , ch(10),~
               at (07,02), fac(hex(8e)), where$(03%)            , ch(16),~
               at (07,22), fac(hex(8c)), tag$(03%)              , ch(19),~
               at (07,45), fac(hex(8c)), ds$(03%)               , ch(08),~
               at (07,60), fac(hex(8c)), di$(03%)               , ch(08),~
               at (07,71), fac(hex(8c)), quan$(03%)             , ch(10),~
               at (08,02), fac(hex(8e)), where$(04%)            , ch(16),~
               at (08,22), fac(hex(8c)), tag$(04%)              , ch(19),~
               at (08,45), fac(hex(8c)), ds$(04%)               , ch(08),~
               at (08,60), fac(hex(8c)), di$(04%)               , ch(08),~
               at (08,71), fac(hex(8c)), quan$(04%)             , ch(10),~
               at (09,02), fac(hex(8e)), where$(05%)            , ch(16),~
               at (09,22), fac(hex(8c)), tag$(05%)              , ch(19),~
               at (09,45), fac(hex(8c)), ds$(05%)               , ch(08),~
               at (09,60), fac(hex(8c)), di$(05%)               , ch(08),~
               at (09,71), fac(hex(8c)), quan$(05%)             , ch(10),~
               at (10,02), fac(hex(8e)), where$(06%)            , ch(16),~
               at (10,22), fac(hex(8c)), tag$(06%)              , ch(19),~
               at (10,45), fac(hex(8c)), ds$(06%)               , ch(08),~
               at (10,60), fac(hex(8c)), di$(06%)               , ch(08),~
               at (10,71), fac(hex(8c)), quan$(06%)             , ch(10),~
               at (11,02), fac(hex(8e)), where$(07%)            , ch(16),~
               at (11,22), fac(hex(8c)), tag$(07%)              , ch(19),~
               at (11,45), fac(hex(8c)), ds$(07%)               , ch(08),~
               at (11,60), fac(hex(8c)), di$(07%)               , ch(08),~
               at (11,71), fac(hex(8c)), quan$(07%)             , ch(10),~
               at (12,02), fac(hex(8e)), where$(08%)            , ch(16),~
               at (12,22), fac(hex(8c)), tag$(08%)              , ch(19),~
               at (12,45), fac(hex(8c)), ds$(08%)               , ch(08),~
               at (12,60), fac(hex(8c)), di$(08%)               , ch(08),~
               at (12,71), fac(hex(8c)), quan$(08%)             , ch(10),~
               at (13,02), fac(hex(8e)), where$(09%)            , ch(16),~
               at (13,22), fac(hex(8c)), tag$(09%)              , ch(19),~
               at (13,45), fac(hex(8c)), ds$(09%)               , ch(08),~
               at (13,60), fac(hex(8c)), di$(09%)               , ch(08),~
               at (13,71), fac(hex(8c)), quan$(09%)             , ch(10),~
               at (14,02), fac(hex(8e)), where$(10%)            , ch(16),~
               at (14,22), fac(hex(8c)), tag$(10%)              , ch(19),~
               at (14,45), fac(hex(8c)), ds$(10%)               , ch(08),~
               at (14,60), fac(hex(8c)), di$(10%)               , ch(08),~
               at (14,71), fac(hex(8c)), quan$(10%)             , ch(10),~
               at (15,02), fac(hex(8e)), where$(11%)            , ch(16),~
               at (15,22), fac(hex(8c)), tag$(11%)              , ch(19),~
               at (15,45), fac(hex(8c)), ds$(11%)               , ch(08),~
               at (15,60), fac(hex(8c)), di$(11%)               , ch(08),~
               at (15,71), fac(hex(8c)), quan$(11%)             , ch(10),~
               at (16,02), fac(hex(8e)), where$(12%)            , ch(16),~
               at (16,22), fac(hex(8c)), tag$(12%)              , ch(19),~
               at (16,45), fac(hex(8c)), ds$(12%)               , ch(08),~
               at (16,60), fac(hex(8c)), di$(12%)               , ch(08),~
               at (16,71), fac(hex(8c)), quan$(12%)             , ch(10),~
               at (17,02), fac(hex(8e)), where$(13%)            , ch(16),~
               at (17,22), fac(hex(8c)), tag$(13%)              , ch(19),~
               at (17,45), fac(hex(8c)), ds$(13%)               , ch(08),~
               at (17,60), fac(hex(8c)), di$(13%)               , ch(08),~
               at (17,71), fac(hex(8c)), quan$(13%)             , ch(10),~
               at (18,02), fac(hex(8e)), where$(14%)            , ch(16),~
               at (18,22), fac(hex(8c)), tag$(14%)              , ch(19),~
               at (18,45), fac(hex(8c)), ds$(14%)               , ch(08),~
               at (18,60), fac(hex(8c)), di$(14%)               , ch(08),~
               at (18,71), fac(hex(8c)), quan$(14%)             , ch(10),~
               at (19,02), fac(hex(8e)), where$(15%)            , ch(16),~
               at (19,22), fac(hex(8c)), tag$(15%)              , ch(19),~
               at (19,45), fac(hex(8c)), ds$(15%)               , ch(08),~
               at (19,60), fac(hex(8c)), di$(15%)               , ch(08),~
               at (19,71), fac(hex(8c)), quan$(15%)             , ch(10),~
               at (20,02), fac(hex(8e)), where$(16%)            , ch(16),~
               at (20,22), fac(hex(8c)), tag$(16%)              , ch(19),~
               at (20,45), fac(hex(8c)), ds$(16%)               , ch(08),~
               at (20,60), fac(hex(8c)), di$(16%)               , ch(08),~
               at (20,71), fac(hex(8c)), quan$(16%)             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,25), fac(pf2fac$), pf2msg$,                        ~
               at (23,35), "(8)See Job/Part Nesting",                    ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,25), fac(pf5fac$), pf5msg$,                        ~
               at (24,35), "(9)See PO Line Info",                        ~
               at (24,65), "(16)Return",                                 ~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               gosub pf1315
               if keyhit% = 15% then L45070

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L50000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test Part or Job depending on modus operandi              *~
            *************************************************************

            errormsg$ = " "
            if keyhit% = 5% then L50200

*        TEST FOR PART NUMBER                      PART$
            call "GETCODE" (#05, part$, " ", 0%, 0, f1%(5%))
            if f1%(5%) <> 1% then errormsg$ = "Invalid Part Code."
            return

L50200
*        TEST FOR JOB NUMBER                       JOB$
            call "GETCODE" (#07, job$, " ", 0%, 0, f1%(7%))
            if f1%(7%) <> 1% then errormsg$ = "Invalid Job Number."
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

            call "SHOSTAT" ("One moment please")
            end
