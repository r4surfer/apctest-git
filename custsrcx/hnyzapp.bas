        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y   Y  ZZZZZZ    AA    PPPP   PPPP         *~
            *  H   H  NN  N  Y   Y      Z    A  A   P   P  P   P        *~
            *  HHHHH  N N N   YYY      Z    AAAAAA  PPPP   PPPP         *~
            *  H   H  N  NN    Y      Z     A    A  P      P            *~
            *  H   H  N   N    Y    ZZZZZZ  A    A  P      P            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYZAPP  - Converts inventory of one part number to       *~
            *            another part number.  Makes oranges into       *~
            *            apples.  Chevys into Caddys... Lumps of        *~
            *            coal into gold.  You get the picture.          *~
            *            Just tell the bean counters to hold onto their *~
            *            hats.  We're gonna make things happen now -    *~
            *            this is what software is all about!            *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/11/93 ! ORIGINAL  (Cloned from HNYMOVE)          ! WPH *~
            * 06/06/94 ! PRR13199 - Added Counter to GLCMBSUB List! RJH *~
            * 08/15/94 ! PRR 13273.  Fixed PLOW args on #22.      ! RJH *~
            * 08/14/06 ! (AWD001) Mod to add GLORTRAN             ! CMG *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim company$30,             /*                                 */~
            coractive$1,            /* Is core module active for HNY?  */~
            date$8,                 /*                                 */~
            hnydate$6,              /*                                 */~
            hnydatef$8,             /*                                 */~
            runtime$8,              /*                                 */~
            gltext$100,             /*                                 */~
            jnlid$3, hnytt$2,       /*                                 */~
            modno$2,                /*                                 */~
            summary$1,              /*                                 */~
            title$40,               /*                                 */~
            errormsg$79,            /*                                 */~
            inpmessage$79,          /*                                 */~
            line2$79,               /*                                 */~
            mm$50,                  /*                                 */~
            fpart$25,               /*                                 */~
            tpart$25,               /*                                 */~
            fdescr$32,              /*                                 */~
            tdescr$32,              /*                                 */~
            newacct$9,              /*                                 */~
            oldacct$9,              /*                                 */~
            store$(10)3,            /*                                 */~
            lot$(10)6,              /*                                 */~
            q$(10,5)10,             /*                                 */~
            mq$12,                  /*                                 */~
            fstore$3,               /*                                 */~
            flot$6,                 /*                                 */~
            tstore$3,               /*                                 */~
            tlot$6,                 /*                                 */~
            lfac$1,                 /* Fac for enabled fields          */~
            lfacf$1, lfact$1,       /* Facs for from and to lot numbers*/~
            plowkey$99,             /*                                 */~
            pf$(3)79,               /* PF key prompts                  */~
            pfkeys$32,              /* PF key activation string        */~
            readkey$99,             /*                                 */~
            rptid$6,                /*                                 */~
            pgmid$8,                /*                                 */~
            userid$3,               /*                                 */~
            cost(12),               /*                                 */~
            acct$(2)9,              /*                                 */~
            fadjacct$16,            /*                                 */~
            tadjacct$16,            /*                                 */~
            def_adjacct$16,         /*                                 */~
            adjacct$16, hnyacct$9,  /*                                 */~
            glamount(2),            /*                                 */~
            hnyflg$1,               /*                                 */~
            passedin_acct$(50)109,  /*                                 */~
            text$109,               /*                                 */~
            passedin_dbcr(50,2),    /*                                 */~
            passedin_type$(50)2     /*                                 */


        dim                              /* G/L Export Posting Info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            fpartcat$4,                  /* Part Category code         */~
            fpartclass$4,                /* Part Class code            */~
            fpartgen$16,                 /* Part Generic code          */~
            fparttype$3,                 /* Part Type code             */~
            tpartcat$4,                  /* Part Category code         */~
            tpartclass$4,                /* Part Class code            */~
            tpartgen$16,                 /* Part Generic code          */~
            tparttype$3,                 /* Part Type code             */~
            tran_type$5,                 /* G/L Transaction type       */~
            fuom$4, fuom_descr$32,       /* Part Unit of measure       */~
            tuom$4, tuom_descr$32        /* Part Unit of measure       */

        dim division$3,                  /* division number (AWD001)   */~
            schema$8                     /* schema          (AWD001)   */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            fs%(32)                      /* = 1 if file open, -1 if it */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.02 01/17/95 'A' PRRs & Critical Problems    "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE (INVENTORY DATE) *~
            * # 2 ! STORNAME ! STORE MASTER FILE                        *~
            * # 3 ! USERINFO ! USER INFORMATION FILE                    *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! GENCODES ! General Codes File                       *~
            * # 6 ! HNYDETAL ! INVENTORY TRANSACTION DETAIL FILE        *~
            * # 7 ! HNYQUAN  ! INVENTORY QUANTITY INFORMATION FILE      *~
            * #11 ! HNYPOOL  ! INVENTORY POOL FILES                     *~
            * #13 ! PIPMASTR ! PLANNED INV. POSITION MASTER             *~
            * #14 ! HNYADJPF ! INVENTORY ADJUSTMENT PRINT JOURNAL       *~
            * #15 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #16 ! LOCATION ! Location Master File                     *~
            * #19 ! GLMAIN   ! General Ledger Main File                 *~
            * #20 ! GLDETAIL ! General ledger detail file               *~
            * #21 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #23 ! GLORTRAN ! GL Oracle transmit file     (AWD001)     *~
            * #24 ! GENCODES ! GENERAL CODES MASTER FILE   (AWD001)     *~
            * #50 ! WRKFILE  ! Hnypst2 workfile                         *~
            *************************************************************

            select  #1, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select  #2, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select  #3, "USERINFO",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 3

            select  #4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select  #5,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select  #6, "HNYDETAL",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  150,                                 ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 43, keylen = 6, dup,  ~
                                   key 2, keypos = 49, keylen = 2, dup

            select #7,  "HNYQUAN",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 650,                                  ~
                         keypos= 17, keylen = 44,                        ~
                         alternate key 1, keypos =  1, keylen = 44

            select #11,  "HNYPOOL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #13,  "PIPMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

            select #14,  "HNYADJPF",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 19                         ~

            select #15,  "HNYLOCNS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 443, keylen = 42,     ~
                                   key 2, keypos = 485, keylen = 42,     ~
                                   key 3, keypos = 527, keylen = 42,     ~
                                   key 4, keypos = 590, keylen = 42      ~

            select #16,  "LOCATION",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 11,                        ~
                         alternate key 1, keypos = 4, keylen = 11        ~

            select #19, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #20, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos =    1, keylen =  26                      ~

            select #21, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #22, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

/*(AWD001)*/
            select #23, "GLORTRAN",                                       ~
                        varc,     indexed, recsize = 512,                 ~
                        keypos = 1,    keylen = 17,                       ~
                        alt key 1, keypos = 15, keylen = 31

            select #24, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
/*(AWD001)*/

            select #50, "WRKFILE",                                       ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 19                          ~

        call "SHOSTAT"  ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%( 1), f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (#2,  fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (#3,  fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (#4,  fs%( 4), f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (#5,  fs%( 5), f2%( 5),   0%, rslt$( 5))
            call "OPENCHCK" (#6,  fs%( 6), f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (#7,  fs%( 7), f2%( 7),   0%, rslt$( 7))
            call "OPENCHCK" (#11, fs%(11), f2%(11),   0%, rslt$(11))
            call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15),   0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
            call "OPENCHCK" (#19, fs%(19), f2%(19),   0%, rslt$(19))
            call "OPENCHCK" (#20, fs%(20), f2%(20),   0%, rslt$(20))
            call "OPENCHCK" (#21, fs%(21), f2%(21),   0%, rslt$(21))

/*(AWD001)*/
            call "OPENCHCK" (#23, fs%(23), f2%(23), 100%, rslt$(23))
            call "OPENCHCK" (#24, fs%(24), f2%(24),   0%, rslt$(24))
/*(AWD001)*/

            call "WORKOPEN" (#50, "IO", 100%, f2%(50))

*       Check to see if Core Module Installed
            call "READ100" (#1, "SWITCHS.COR", f1%(1))
                if f1%(1) = 0% then L09000

            get #01, using L04220, coractive$
L04220:         FMT  POS(134), CH(1)
            if coractive$ <> "Y" then L09000
            call "OPENCHCK" (#22, fs%(22), f2%(22),   0%, rslt$(22))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************


                                                          /* (AWD001)  */

            schema_err%, schema% = 0%
            init(" ") schema$, division$
            call "SCHEMA" (schema$, schema%, #24, schema_err%)


            if schema% = 1% then division$ = "036"   /* NC */
            if schema% = 2% then division$ = "080"   /* NE */
            if schema% = 3% then division$ = "035"   /* AES*/


            date$ = date : call "DATEFMT" (date$)
            call "COMPNAME" (02%, company$, u3%) : u3% = u3%
            call "TIME" (runtime$)
            ll% = 6%

*        Get Inventory Date Information.
            call "EXTRACT" addr ("ID", userid$)
            call "READ100" (#3, userid$, f1%(3))
            if f1%(3) = 1% then L09200
                call "ASKUSER" (0%, "***ERROR***",                       ~
                     "Unable to locate your Inventory Posting Date!",    ~
                     " ", "Press (RETURN) to acknowledge and Exit.")
                goto exit_program
L09200:     get #3, using L09210, hnydate$
L09210:         FMT XX(27), CH(6)
            call "WHICHPER" (#1, hnydate$, thismonth%)
            if thismonth% > 0% then L09300
                call "ASKUSER" (0%, "INVALID POSTING DATE",              ~
                     "Your Inventory Posting Date is not within the " &  ~
                     "posting window.",                                  ~
                     "Please change your posting date & try again.",     ~
                     "Press (RETURN) to acknowledge and Exit.")
                goto exit_program
L09300:     hnydatef$ = hnydate$  :  call "DATEFMT"(hnydatef$)

*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#1, "SWITCHS.GL", f1%(1))
            if f1%(1) = 1% then get #1 using L09360, export_on$
L09360:         FMT POS(22), CH(1)
            if export_on$ <> "Y" then export_on$ = "N"  /* No ???? */

*        Get SYSFILE2 record to determine account for variance posting
            call "READ100" (#1, "SWITCHS.HNY", f1%(1))
              if f1%(1) <> 0 then L09410
                 call "ASKUSER" (0%, "***ERROR***",                      ~
                      "Unable to locate Inventory System Switches!",     ~
                      " ", "Press (RETURN) to acknowledge and Exit.")
                 goto exit_program
L09410:     get #1, using L09420, hnyflg$
L09420:         FMT POS(96), CH(1)

            call "READ100" (#1, "FISCAL DATES", f1%(1))
               if f1%(1) = 0% then L09490
            get #1, using L09470, def_adjacct$
L09470:                   FMT POS(416), CH(16)

L09490
*        Journal Data elements
            jnlid$ = "IAA" : hnytt$ = "IZ"
            rptid$ = "HNY059" : pgmid$ = "HNYZAPP "
            str(line2$,62) = str(pgmid$,,8) & ": " & str(cms2v$,,8)

            modno$ = "04"
            jnlreturn% = 1%
            call "JNLINFO" (modno$, jnlid$, pstseq%, summary$,           ~
                            title$, hnydate$, #1, f2%(1), jnlreturn%)
            call "STRING" addr("RJ", title$, 40%)
            postflag% = 1%

            prtline = 100
            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)

        REM *************************************************************~
            *       Top of Loop                                         *~
            *************************************************************

        inputmode
           init (" ")  errormsg$, fpart$, fdescr$, newacct$, oldacct$,   ~
                       tpart$, tdescr$, fuom$, fuom_descr$, store$(),    ~
                       lot$(), q$(), fstore$, flot$, tstore$, tlot$,     ~
                       readkey$, plowkey$, mm$, mq$, tuom$, tuom_descr$

           totalcost, mq, avail = 0 : mat cost = zer

        inputmode_1
L10090:    gosub L40000
L10350:    if keyhit% = 16% then goto L65000
           gosub L50000
           if errormsg$ <> " " then L10090

        REM *************************************************************~
            * Second Screen Input Mode                                  *~
            *************************************************************
        inputmode_2
            edit% = 0%
            gosub data_load_0
L11050:     gosub L41000
            if keyhit% =  1% then inputmode
            if keyhit% =  16% then exit_program
            if keyhit% =  2% then inputmode_2
            if keyhit% <>  5% then L11130
               errormsg$ = " "
               if i% <= 10% then L11050
               gosub data_load_1
               goto L11050

L11130:     gosub L51000
               if errormsg$ <> " " then L11050
            if keyhit% =  0% then editmode /* If it passes, move it     */
            if keyhit% <> 8% then L11050    /* Test, then locations, FR  */
               gosub locations
               goto L11050

        REM *************************************************************~
            * Second Screen Edit Mode                                   *~
            *************************************************************
        editmode
            edit% = 1%
            gosub data_load_0
L12070:     gosub L41000
            if keyhit%  =  2% then editmode
            if keyhit% <>  5% then L12140
               errormsg$ = " "
               if i% <= 10% then L12070
               gosub data_load_1
               goto L12070
L12140:     if keyhit% =  1% then inputmode
            if keyhit% = 16% then data_save
            if keyhit% =  0% then inputmode_2
            if keyhit% <> 8% then L12070
               gosub locations
               goto L12070


        REM *************************************************************~
            *    A C C E S S   L O C A T I O N   M A N A G E M E N T    *~
            *-----------------------------------------------------------*~
            *  Call HNYLCSUB passing Part, Store, Lot and Qty invoking  *~
            *  the transfer facility within the subroutine.             *~
            *************************************************************
        locations
            if mq$ <> " " then L18110
                mq = 0
                goto L18130

L18110:     convert mq$ to mq

L18130:     call "HNYLCSUB" (fpart$,                                     ~
                             fstore$,                                    ~
                             flot$,                                      ~
                             mq,                                         ~
                             1%,       /*   Action = Do anything       */~
                             #1,       /*   SYSFILE2                   */~
                             #2,       /*   STORNAME                   */~
                             #3,       /*   USERINFO                   */~
                             #4,       /*   HNYMASTR                   */~
                             #15,      /*   HNYLOCNS                   */~
                             #7,       /*   HNYQUAN                    */~
                             #16,      /*   LOCATION                   */~
                             tstore$,  /*   TO STORE                   */~
                             tlot$)    /*   TO LOT                     */
            return

        REM *************************************************************~
            * Load Source Lots and their quantities                     *~
            *************************************************************

        data_load_0

            errormsg$ = " "
            plowkey$  = str(fpart$,1,25) & hex(00)
            keyhit% = 2%

        data_load_1

            init(" ") store$(), lot$(), q$() :  i% = 1%
            init(" ") oldacct$, newacct$

L30110:     call "PLOWNEXT" (#7, plowkey$, 25%, f1%(7%))
            if f1%(7%) = 1% then L30180
                if i% > 1% then return
                if keyhit% = 5% then data_load_0
                errormsg$ = "No Inventory Quantity records on file "  &  ~
                            "for  " & fpart$ & "."
                return clear all
                goto inputmode_1

L30180:     get #7 using L30200, store$(i%), lot$(i%), qty1, qty2, qty3,  ~
                                                    qty4, qty5
L30200:         FMT POS(42), CH(3), CH(6), POS(69), 5*PD(14,4)
           convert qty1 to q$(i%,1%), pic(-######.##)
           convert qty2 to q$(i%,2%), pic(-######.##)
           convert qty3 to q$(i%,3%), pic(-######.##)
           convert qty4 to q$(i%,4%), pic(-######.##)
           convert qty5 to q$(i%,5%), pic(-######.##)
           i% = i% + 1%
           if i% <= 10% then L30110

           return

        REM *************************************************************~
            * Save Data                                                 *~
            *************************************************************

        data_save

            call "SHOSTAT" ("Posting Transaction...")

            call "HNYHOLD" (#7, fpart$, fstore$, flot$, mq, return%)

            if postflag% = 0 then L31180
            jnlreturn% = 0%
            call "JNLINFO" (modno$, jnlid$, pstseq%, summary$, title$,   ~
                                    hnydate$, #1, f2%(1), jnlreturn%)
            postflag% = 0%

L31180:      if hnyflg$ <> "A" then L31280

                   call "HNYGLGET" (fpart$, fstore$, flot$, fadjacct$,   ~
                                    6%, #4, #7)
                   if fadjacct$ = " " then fadjacct$ = def_adjacct$

                   call "HNYGLGET" (tpart$, tstore$, tlot$, tadjacct$,   ~
                                    6%, #4, #7)
                   if tadjacct$ = " " then tadjacct$ = def_adjacct$

L31280:     totalcost = 0 : mat cost = zer

            if fpart$ <> tpart$ then L31340
            mm$ = "MOVED  TO  STR:" & tstore$ & " LOT:" & tlot$
            goto L31360

            /* 'PP' indicates 'part to part' conversion */
L31340:     mm$ = "PP:" & tpart$ & "/" & tstore$ & "/" & tlot$

L31360:     gltext$ = "FM STR: " & str(fstore$) & " LOT: " & str(flot$)
            str(gltext$,31) = str(fpart$) & str(fstore$) & str(flot$)
            str(gltext$,69,32) = "CV TO: " & tpart$
            call "HNYPST2" (fpart$, fstore$, flot$, -mq, 0,0,0,0, cost(),~
                            totalcost,0,0, hnydate$, hnytt$, mm$,        ~
                            oldacct$, " ", 3%, 0%, modno$, " ", pstseq%, ~
                            gltext$, userid$, #7, #6, #1, #11, #04, #13, ~
                            #21, #19, #20, #50, 1%, returncode%)

            movecost = round(totalcost * mq, 2%)
            gltext$ = "FM STR: " & str(fstore$) & " LOT: " & str(flot$)
            str(gltext$,31) = str(fpart$) & str(fstore$) & str(flot$)
            str(gltext$,69,32) = "CV TO: " & tpart$

            init(" ") passedin_acct$(), passedin_type$()
            mat passedin_dbcr = zer

            passedin_acct$(1) = str(oldacct$) & gltext$
            passedin_dbcr(1,1) = 0
            passedin_dbcr(1,2) = movecost
            passedin_type$(1) = "01"
            cntr% = 1%
            hnyacct$ = oldacct$ : adjacct$ = fadjacct$
            pass% = 1% : gosub consolidate_array_and_post

            if fpart$ <> tpart$ then L31650
            mm$ = "MOVED FROM STR:" & fstore$ & " LOT:" & flot$
            goto L31670

L31650:     mm$ = "PP:" & fpart$ & "/" & fstore$ & "/" & flot$

L31670:     this% = 0%
            gltext$ = "TO STR: " & str(tstore$) & " LOT: " & str(tlot$)
            str(gltext$,31) = str(tpart$) & str(tstore$) & str(tlot$)
            str(gltext$,69,32) = "CV FM: " &  fpart$
            call "HNYPST2" (tpart$, tstore$, tlot$, mq, 0,0,0,0, cost(), ~
                            totalcost,0,0, hnydate$, hnytt$, mm$,        ~
                            newacct$, " ", 3%, 0%, modno$, " ", pstseq%, ~
                            gltext$, userid$, #7, #6, #1, #11, #04, #13, ~
                            #21, #19, #20, #50, this%, returncode%)

            movecost = round(totalcost * mq, 2%)
            gltext$ = "TO STR: " & str(tstore$) & " LOT: " & str(tlot$)
            str(gltext$,31) = str(tpart$) & str(tstore$) & str(tlot$)
            str(gltext$,69,32) = "CV FM: " &  fpart$

            init(" ") passedin_acct$(), passedin_type$()
            mat passedin_dbcr = zer

            passedin_acct$(1) = str(newacct$) & gltext$
            passedin_dbcr(1,1) = movecost
            passedin_dbcr(1,2) = 0
            passedin_type$(1) = "01"
            cntr% = 1%
            hnyacct$ = newacct$ : adjacct$ = tadjacct$
            pass% = 2% : gosub consolidate_array_and_post

            call "LOTTRACK"                                              ~
                 ("H",             /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
                  fpart$,          /* PART BEING MOVED                 */~
                  flot$,           /* FROM LOT                         */~
                  fstore$,         /* FROM STORE                       */~
                  " ",             /* FROM MISC.                       */~
                  "H",             /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
                  tpart$,          /* TO PART (IF BLANK THEM SAME)     */~
                  tlot$,           /* TO LOT                           */~
                  tstore$,         /* TO STORE                         */~
                  " ",             /* TO MISC.                         */~
                  mq,              /* Quantity being moved             */~
                  #4,              /* 'HNYMASTR' FILE                  */~
                  #1)              /* 'SYSFILE2' FILE                  */~


*       *** Now Clean Up and See If We Want Another One

            if prtline > 55 then gosub header
            print using L32580, fpart$,fdescr$, mq, fstore$, flot$,       ~
                               tstore$, tlot$, tpart$
            prtline = prtline + 1

            totalcost, mq = 0 : mat cost = zer
            init(" ") fstore$, tstore$, flot$, tlot$, mq$
            close printer
            goto inputmode_2

        REM *************************************************************~
            * Logging Images, Header Routine                            *~
            *************************************************************

L32540: % RUN DATE: ######## ########                      ##############~
        ~#################                                   #############~
        ~###
L32560: %POST DATE: ########  BY: ###                    INVENTORY CONVER~
        ~SION AUDIT LISTING
L32580: %######################### ################################ -###,~
        ~###,###.##   ###   ######   ###   ######   ######################~
        ~###
L32610: %SOURCE PART NUMBER        SOURCE PART DESCRIPTION               ~
        ~  QUANTITY  STORE  LOT NR  STORE  LOT NR   DESTINATION PART
L32625: %------------------------- --------------------------------      ~
        ~  --------  -----  ------  -----  ------   ----------------------~
        ~---
L32630: %                                                                ~
        ~            -- F R O M --  ---- T O ----

        header
            temp$ = str(pgmid$,,8) & "-" & rptid$
            print page
            print using L32540, date$, runtime$, company$, temp$
            print using L32560, hnydatef$, userid$
            print
            print using L32630
            print using L32610
            print using L32625
            prtline = 6
            return

        REM *************************************************************~
            * CALL TO CONSOLIDATE                                       *~
            *************************************************************

        consolidate_array_and_post
                first% = 1%
                plowkey$ = all (hex(00))
L33070:         call "PLOWNXT1" (#50, plowkey$, 0%, f1%(50))
                     if f1%(50) = 0 and first% = 1% then set_up_posting
                     if f1%(50) = 0 then L33310
                     first% = 0%
                get #50, using L33120, text$, dbamt, cramt
L33120:         FMT XX(25), CH(109), 2*PD(14,4)
                if hnyflg$ <> "A" then L33170
                    chkacct$ = str(text$,1,9)
                    if chkacct$ = hnyacct$ then L33170
                       text$ = str(adjacct$,1,9) & str(text$,10,100)
L33170:         cntr% = cntr% + 1%
                passedin_acct$(cntr%) = text$
                passedin_dbcr(cntr%,1%) = dbamt
                passedin_dbcr(cntr%,2%) = cramt
                     passedin_type$(cntr%) = "01"
                     if str(passedin_acct$(cntr%),,9)= hnyacct$ then L33280
                     if hnyflg$ = "A" then L33270
                     if str(text$,76,1) = "H" then L33280
                     passedin_type$(cntr%) = "03"
                     if str(text$,74,1) = "S" then L33280
L33270:              passedin_type$(cntr%) = "02"
L33280:         delete #50
                goto L33070

L33310:         call "GLCMBSUB" (passedin_acct$(), passedin_dbcr(),      ~
                                 passedin_type$(), #1, cntr%)

        set_up_posting
            if export_on$ = "Y" then gosub load_gl_info
            for x% = 1% to 50%
                if passedin_acct$(x%) = " " then return
                acct$(1) = str(passedin_acct$(x%),,9)
                acct$(2) = acct$(1)
                glamount(1) = passedin_dbcr(x%,1)
                glamount(2) = passedin_dbcr(x%,2)
                gltext$ = str(passedin_acct$(x%),10)

                if export_on$ <> "Y" then L33560
                   temp1 = glamount(1) - glamount(2)
                   tran_type$ = jnlid$
                   str(tran_type$,4,2) = passedin_type$(x%)
                   if pass% = 2% then L33510
                      temp$ = fstore$ : temp2 = abs(mq) * (-1)
                         goto L33520
L33510:               temp$ = tstore$ : temp2 = abs(mq)
L33520:            put gl_post_info$() using L33540, tran_type$, temp1,   ~
                                                    temp2, temp$
L33540:               FMT CH(5), POS(18), 2*PD(15,4), POS(164), CH(3)

L33560:         gosub post_gl
            next x%

                return

        REM *************************************************************~
            *         C O M M O N   G / L   P O S T   L O G I C         *~
            *                                                           *~
            * ACCT$(), GLTEXT$, and GLAMOUNT must be set prior to this. *~
            *************************************************************

        post_gl

            REM Account in ACCT$(1) is debited...

            if glamount(1) = 0 then L34320

            call "GLPOST2" (acct$(1),    /* ACCOUNT TO BE UPDATED      */~
                      glamount(1),       /* DEBIT AMOUNT (0 IF CREDIT) */~
                      0,                 /* CREDIT AMOUNT (0 IF DEBIT) */~
                      hnydate$,          /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      "04",              /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      division$,         /* (AWD001) division          */~
                      #19,               /* UFB ADDRESS OF G/L MAIN    */~
                      #20,               /* UFB ADDRESS OF G/L DETAILS */~
                      #1,                /* UFB ADDRESS OF SYSFILE2    */~
                      #23,               /* (AWD001) GLORTRAN          */~
                      err%,              /* ERROR RETURN FROM SUBROUTIN*/~
                      " ",               /* Filler                     */~
                      gl_post_info$())   /* GLEXPORT Information       */

            call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$, hnydate$, ~
                         acct$(1), gltext$, glamount(1), 0, #14, f2%(14))

L34320:     REM Account in ACCT$(2) is credited...

            if acct$(2%) = " " then L34570
            if glamount(2%) = 0 then L34570

            call "GLPOST2" (acct$(2%),   /* ACCOUNT TO BE UPDATED      */~
                      0,                 /* DEBIT AMOUNT (0 IF CREDIT) */~
                      glamount(2%),      /* CREDIT AMOUNT (0 IF DEBIT) */~
                      hnydate$,          /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      "04",              /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      division$,         /* (AWD001) division          */~
                      #19,               /* UFB ADDRESS OF G/L MAIN    */~
                      #20,               /* UFB ADDRESS OF G/L DETAILS */~
                      #1,                /* UFB ADDRESS OF SYSFILE2    */~
                      #23,               /* (AWD001) GLORTRAN          */~
                      err%,              /* ERROR RETURN FROM SUBROUTIN*/~
                      " ",               /* Filler                     */~
                      gl_post_info$())   /* GLEXPORT Information       */

            call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$, hnydate$, ~
                       acct$(2%), gltext$, 0, glamount(2%), #14, f2%(14))

L34570:     return

        REM *************************************************************~
            * G/L Export Routines                                       *~
            *************************************************************

        load_gl_info
            if pass% = 2% then load_gl_info2
            put str(gl_post_info$(),,) using L38510,                      ~
                " ",                     /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Currency Units per Book    */~
                0,                       /* Functional Currency amount */~
                0,                       /* Unit amount                */~
                " ",                     /* Customer code CH(9)        */~
                " ",                     /* Sales Order number CH(16)  */~
                " ",                     /* BOL number CH(3)           */~
                " ",                     /* Customer Type CH(2)        */~
                " ",                     /* State CH(2)                */~
                " ",                     /* Country CH(3)              */~
                " ",                     /* ZIP CH(9)                  */~
                " ",                     /* Sales Region CH(4)         */~
                " ",                     /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                " ",                     /* Salesman code CH(4)        */~
                " ",                     /* Invoice Number CH(8)       */~
                fpart$,                  /* Part Number CH(25)         */~
                fpartcat$,               /* Part Category CH(4)        */~
                fpartclass$,             /* Part Class CH(4)           */~
                fpartgen$,               /* Part Generic code CH(16)   */~
                fparttype$,              /* Part Type CH(3)            */~
                fuom$,                   /* Part UOM CH(4)             */~
                " ",                     /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                " ",                     /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                " ",                     /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

        load_gl_info2

            put str(gl_post_info$(),,) using L38510,                      ~
                "?",                     /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Currency Units per Book    */~
                0,                       /* Functional Currency amount */~
                0,                       /* Unit amount                */~
                " ",                     /* Customer code CH(9)        */~
                " ",                     /* Sales Order number CH(16)  */~
                " ",                     /* BOL number CH(3)           */~
                " ",                     /* Customer Type CH(2)        */~
                " ",                     /* State CH(2)                */~
                " ",                     /* Country CH(3)              */~
                " ",                     /* ZIP CH(9)                  */~
                " ",                     /* Sales Region CH(4)         */~
                " ",                     /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                " ",                     /* Salesman code CH(4)        */~
                " ",                     /* Invoice Number CH(8)       */~
                tpart$,                  /* Part Number CH(25)         */~
                tpartcat$,               /* Part Category CH(4)        */~
                tpartclass$,             /* Part Class CH(4)           */~
                tpartgen$,               /* Part Generic code CH(16)   */~
                tparttype$,              /* Part Type CH(3)            */~
                tuom$,                   /* Part UOM CH(4)             */~
                " ",                     /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                " ",                     /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                " ",                     /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L38510: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Currency Units per Book    */~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
                CH(9),                   /* Customer code CH(9)        */~
                CH(16),                  /* Sales Order number CH(16)  */~
                CH(3),                   /* BOL number CH(3)           */~
                CH(2),                   /* Customer Type CH(2)        */~
                CH(2),                   /* State CH(2)                */~
                CH(3),                   /* Country CH(3)              */~
                CH(9),                   /* ZIP CH(9)                  */~
                CH(4),                   /* Sales Region CH(4)         */~
                CH(10),                  /* Sales Tax code CH(10)      */~
                CH(4),                   /* Shipping Region CH(4)      */~
                CH(4),                   /* Salesman code CH(4)        */~
                CH(8),                   /* Invoice Number CH(8)       */~
                CH(25),                  /* Part Number CH(25)         */~
                CH(4),                   /* Part Category CH(4)        */~
                CH(4),                   /* Part Class CH(4)           */~
                CH(16),                  /* Part Generic code CH(16)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice  CH(16)     */~
                CH(8),                   /* Check Payment Number CH(8) */~
                CH(8),                   /* Project code CH(8)         */~
                CH(8),                   /* Job number CH(8)           */~
                CH(4),                   /* Work Center CH(4)          */~
                CH(4),                   /* Activity code CH(4)        */~
                CH(12),                  /* Employee number CH(12)     */~
                CH(4),                   /* Department code CH(4)      */~
                CH(4),                   /* Cost Center CH(4)          */~
                CH(12),                  /* Earnings Type CH(12)       */~
                CH(12),                  /* Deduction Type CH(12)      */~
                CH(4),                   /* P/R Category CH(4)         */~
                CH(4),                   /* Labor class CH(4)          */~
                CH(191)                  /* Filler                     */


        load_fpart_info

            call "READ100" (#4, fpart$, f1%(4))
                 if f1%(4) = 0% then return /* Shouldn't happen */
            get #4 using L39060, fpartgen$, fuom$, fpartcat$, fpartclass$,~
                               fparttype$
L39060:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)
            plowkey$ = "UOM      " & fuom$
            call "DESCRIBE" (#5, plowkey$, fuom_descr$, 1%, f1%(5))
            return

        load_tpart_info

            call "READ100" (#4, tpart$, f1%(4))
                 if f1%(4) = 0% then return /* Shouldn't happen */
            get #4 using L39560, tpartgen$, tuom$, tpartcat$, tpartclass$,~
                               tparttype$
L39560:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)
            plowkey$ = "UOM      " & tuom$
            call "DESCRIBE" (#5, plowkey$, tuom_descr$, 1%, f1%(5))
            return

L40000: REM *************************************************************~
            * Screen 1                                                  *~
            *************************************************************

           inpmessage$ = "Enter the Source and Destination part Numbers."

L40060: accept                                                           ~
           at(01,02), "Convert Inventory from one Part Number to Another ~
        ~Part Number",                                                    ~
           at(01,67), "Post: ",                                          ~
           at(01,73), fac(hex(8c)), hnydatef$,                           ~
           at(02,02), fac(hex(ac)), line2$                      , ch(79),~
           at(03,02), fac(hex(94)), errormsg$                   , ch(79),~
                                                                         ~
           at(04,02), "From Part: ",                                     ~
           at(04,13), fac(hex(81)), fpart$                      , ch(25),~
           at(04,40), "To Part: ",                                       ~
           at(04,49), fac(hex(81)), tpart$                      , ch(25),~
           at(21,02), fac(hex(a4)), inpmessage$,                         ~
           at(22,65), "(13)Instructions",                                ~
           at(23,65), "(15)Print Screen",                                ~
           at(24,65), "(16)Exit Program",                                ~
                keys(hex(000d0f10)), key(keyhit%)

            if keyhit% <> 13% then L40270  :  call "MANUAL" (pgmid$)
                                            goto L40060

L40270:     if keyhit% <> 15% then L10350  :  call "PRNTSCRN"
                                            goto L40060

            return

L41000: REM *************************************************************~
            * SCREEN 2   Input and Edit Modes                           *~
            *************************************************************

            gosub set_pf_2
            if edit% = 1% then L41050
L41040:     inpmessage$ = "Enter quantity, source and destination "     &~
                      "and press RETURN."
            lfac$ = hex(81)
            lfacf$ = hex(81) : if enablef% = 0% then lfacf$ = hex(8c)
            lfact$ = hex(81) : if enablet% = 0% then lfact$ = hex(8c)
            goto L41070

L41050:     inpmessage$ = "Press RETURN to Edit or PF 16 to Process."
            lfac$, lfacf$, lfact$ = hex(8c)

L41070: accept                                                           ~
           at(01,02), "Convert Inventory from one Part Number to Another ~
        ~Part Number",                                                    ~
           at(01,67), "Post: ",                                          ~
           at(01,73), fac(hex(8c)), hnydatef$                   , ch(08),~
           at(02,02), fac(hex(ac)), line2$                      , ch(79),~
           at(03,02), fac(hex(94)), errormsg$                   , ch(79),~
                                                                         ~
           at(04,02), "From Part:",                                      ~
           at(04,13), fac(hex(84)), fpart$                      , ch(25),~
           at(04,40), "To Part:"  ,                                      ~
           at(04,49), fac(hex(84)), tpart$                      , ch(25),~
           at(05,13), fac(hex(84)), fdescr$                     , ch(32),~
           at(05,49), fac(hex(84)), tdescr$                     , ch(32),~
           at(06,08), "UOM:",                                            ~
           at(06,13), fac(hex(8c)), fuom$                       , ch(04),~
           at(06,18), fac(hex(8c)), fuom_descr$                 , ch(32),~
           at(06,44), "UOM:",                                            ~
           at(06,51), fac(hex(8c)), tuom$                       , ch(04),~
           at(06,56), fac(hex(8c)), tuom_descr$                 , ch(32),~
           at(07,02), "Qty:",                                            ~
           at(07,07), fac(lfac$), mq$                           , ch(12),~
           at(07,20), "units FROM Store/Lot ",                           ~
           at(07,41), fac(lfac$), fstore$                       , ch(03),~
           at(07,45), fac(lfacf$ ), str(flot$,,ll%),                     ~
           at(07,52), "TO Store/Lot",                                    ~
           at(07,65), fac(lfac$), tstore$                       , ch(03),~
           at(07,69), fac(lfact$ ), str(tlot$,,ll%),                     ~
                                                                         ~
           at(09,02), "Store  Lot         On-Hand   Open S.O.   On-Order ~
        ~     In Q.C.            ",                                       ~
           at(10,03), fac(hex(8d)), store$( 1), ch(3),                   ~
           at(10,09), fac(hex(8d)), lot$( 1), ch(6),                     ~
           at(10,18), fac(hex(8d)), q$(1, 1), ch(10),                    ~
           at(10,30), fac(hex(8d)), q$(1, 2), ch(10),                    ~
           at(10,41), fac(hex(8d)), q$(1, 3), ch(10),                    ~
           at(10,54), fac(hex(8d)), q$(1, 5), ch(10),                    ~
           at(11,03), fac(hex(8d)), store$( 2), ch(3),                   ~
           at(11,09), fac(hex(8d)), lot$( 2), ch(6),                     ~
           at(11,18), fac(hex(8d)), q$(2, 1), ch(10),                    ~
           at(11,30), fac(hex(8d)), q$(2, 2), ch(10),                    ~
           at(11,41), fac(hex(8d)), q$(2, 3), ch(10),                    ~
           at(11,54), fac(hex(8d)), q$(2, 5), ch(10),                    ~
           at(12,03), fac(hex(8d)), store$( 3), ch(3),                   ~
           at(12,09), fac(hex(8d)), lot$( 3), ch(6),                     ~
           at(12,18), fac(hex(8d)), q$(3, 1), ch(10),                    ~
           at(12,30), fac(hex(8d)), q$(3, 2), ch(10),                    ~
           at(12,41), fac(hex(8d)), q$(3, 3), ch(10),                    ~
           at(12,54), fac(hex(8d)), q$(3, 5), ch(10),                    ~
           at(13,03), fac(hex(8d)), store$( 4), ch(3),                   ~
           at(13,09), fac(hex(8d)), lot$( 4), ch(6),                     ~
           at(13,18), fac(hex(8d)), q$(4, 1), ch(10),                    ~
           at(13,30), fac(hex(8d)), q$(4, 2), ch(10),                    ~
           at(13,41), fac(hex(8d)), q$(4, 3), ch(10),                    ~
           at(13,54), fac(hex(8d)), q$(4, 5), ch(10),                    ~
           at(14,03), fac(hex(8d)), store$( 5), ch(3),                   ~
           at(14,09), fac(hex(8d)), lot$( 5), ch(6),                     ~
           at(14,18), fac(hex(8d)), q$(5, 1), ch(10),                    ~
           at(14,30), fac(hex(8d)), q$(5, 2), ch(10),                    ~
           at(14,41), fac(hex(8d)), q$(5, 3), ch(10),                    ~
           at(14,54), fac(hex(8d)), q$(5, 5), ch(10),                    ~
           at(15,03), fac(hex(8d)), store$( 6), ch(3),                   ~
           at(15,09), fac(hex(8d)), lot$( 6), ch(6),                     ~
           at(15,18), fac(hex(8d)), q$(6, 1), ch(10),                    ~
           at(15,30), fac(hex(8d)), q$(6, 2), ch(10),                    ~
           at(15,41), fac(hex(8d)), q$(6, 3), ch(10),                    ~
           at(15,54), fac(hex(8d)), q$(6, 5), ch(10),                    ~
           at(16,03), fac(hex(8d)), store$( 7), ch(3),                   ~
           at(16,09), fac(hex(8d)), lot$( 7), ch(6),                     ~
           at(16,18), fac(hex(8d)), q$(7, 1), ch(10),                    ~
           at(16,30), fac(hex(8d)), q$(7, 2), ch(10),                    ~
           at(16,41), fac(hex(8d)), q$(7, 3), ch(10),                    ~
           at(16,54), fac(hex(8d)), q$(7, 5), ch(10),                    ~
           at(17,03), fac(hex(8d)), store$( 8), ch(3),                   ~
           at(17,09), fac(hex(8d)), lot$( 8), ch(6),                     ~
           at(17,18), fac(hex(8d)), q$(8, 1), ch(10),                    ~
           at(17,30), fac(hex(8d)), q$(8, 2), ch(10),                    ~
           at(17,41), fac(hex(8d)), q$(8, 3), ch(10),                    ~
           at(17,54), fac(hex(8d)), q$(8, 5), ch(10),                    ~
           at(18,03), fac(hex(8d)), store$( 9), ch(3),                   ~
           at(18,09), fac(hex(8d)), lot$( 9), ch(6),                     ~
           at(18,18), fac(hex(8d)), q$(9, 1), ch(10),                    ~
           at(18,30), fac(hex(8d)), q$(9, 2), ch(10),                    ~
           at(18,41), fac(hex(8d)), q$(9, 3), ch(10),                    ~
           at(18,54), fac(hex(8d)), q$(9, 5), ch(10),                    ~
           at(19,03), fac(hex(8d)), store$(10), ch(3),                   ~
           at(19,09), fac(hex(8d)), lot$(10), ch(6),                     ~
           at(19,18), fac(hex(8d)), q$(10,1), ch(10),                    ~
           at(19,30), fac(hex(8d)), q$(10,2), ch(10),                    ~
           at(19,41), fac(hex(8d)), q$(10,3), ch(10),                    ~
           at(19,54), fac(hex(8d)), q$(10,5), ch(10),                    ~
           at(21,02), fac(hex(a4)), inpmessage$                 , ch(79),~
           at(22,02), fac(hex(8c)), pf$(1)                      , ch(79),~
           at(23,02), fac(hex(8c)), pf$(2)                      , ch(79),~
           at(24,02), fac(hex(8c)), pf$(3)                      , ch(79),~
                keys(pfkeys$),  key(keyhit%)

           if keyhit% <> 13% then L42180  :  call "MANUAL" (pgmid$)
                                           goto L41040

L42180:    if keyhit% <> 15% then L42210  :  call "PRNTSCRN"
                                           goto L41040

L42210:    return

        set_pf_2

            pf$(1) = "(1)Select Another Part  (2)First Store/L" &        ~
                     "ot Info                (13)Instructions"
            pf$(2) = "                        (5)Next  Store/L" &        ~
                     "ot Info                (15)Print Screen"
            pf$(3) = "                        (8)Locations    " &        ~
                     "                       (16)Save Data   "

            pfkeys$ = hex(0102ffff05ffff08ffffffff0dff0f1000)
            if edit% = 1% then return

            str(pf$(3),64,16) = "(16)Exit Program"
            return

L50000: REM *************************************************************~
            * Validation, Screen 1                                      *~
            *************************************************************

            errormsg$ = " "

*        Test Source Part
            str(fdescr$,,) = hex(06) & "Select the 'From' Part"
            call "GETCODE" (#4, fpart$, fdescr$, 0%, 0, f1%(4))
            if f1%(4) = 1% then L50075
                 errormsg$ = "Source Part Not On File"
                 return

L50075:     if coractive$ = "N" then L50100
            init(hex(00)) readkey$
            str(readkey$,1,25) = str(fpart$,,)
            call "PLOWNEXT" (#22, readkey$, 25%, f1%(22%))
              if f1%(22%) = 0% then L50100
              errormsg$ = "Source Part is a Core Part - Conversion " &   ~
                          "Not Allowed"
              return

L50100:     call "SERENABL" (fpart$, enablef%, ll%, #1, #4)
               if enablef% = 0% then L50150
                  errormsg$ = "Source Part Has Serial #'s"
                  return

L50150:     call "LOTENABL" (fpart$, enablef%, ll%, #1, #4)
            gosub load_fpart_info

*        Test Destination Part
            str(tdescr$,,) = hex(06) & "Select the 'To' Part"
            call "GETCODE" (#4, tpart$, tdescr$, 0%, 0, f1%(4))
            if f1%(4) = 1% then L50225
                 errormsg$ = "Destination Part Not On File"
                 return

L50225:     if coractive$ = "N" then L50250
            init(hex(00)) readkey$
            str(readkey$,1,25) = str(tpart$,,)
            call "PLOWNEXT" (#22, readkey$, 25%, f1%(22%))
              if f1%(22%) = 0% then L50250

              errormsg$ = "Destination Part is a Core Part - Conversion "~
                          & "Not Allowed"
              return

L50250:     call "SERENABL" (tpart$, enablet%, ll%, #1, #4)
               if enablet% = 0% then L50300
                  errormsg$ = "Destination Part Has Serial #'s"
                  return

L50300:     call "LOTENABL" (tpart$, enablet%, ll%, #1, #4)
            gosub load_tpart_info

            return

L51000: REM *************************************************************~
            * Validation, Screen 2                                      *~
            *************************************************************

           errormsg$ = " "

           call "NUMTEST" (mq$, 0, 9e7, errormsg$, 0.2, mq)
               if errormsg$ <> " " then return
           if mq > 0 then L51140
              if keyhit% <> 0% then L51140  /* Here for Location Mgmt?  */
                 errormsg$ = "Quantity must be greater than zero."
                 return

L51140:    if fpart$  <> tpart$  then L51200    /* True ZAPP        */
           if fstore$ <> tstore$ then L51200    /* Emulates HNYMOVE */
           if flot$   <> tlot$   then L51200    /* Emulates HNYMOVE */
              errormsg$ = "Destination cannot be same as Source"
              return

L51200
*       *** NOTE: FROM Store Implicitly Validated via HNYAVAIL

            if enablef% > 0% then L51250
               flot$ = " "   /* Not Lot Tracked, Shouldn't be enabled */
               goto L51330
L51250:     if enablef% < 2% then L51330   /* Must be Memo Only */
               if flot$ <> " " then L51300
                  errormsg$ = "This is a Lot Tracked Part - 'FROM' " &   ~
                              "Lot Number is REQUIRED"
                  return
L51300:     call "LOTVALID" (fpart$, fstore$, flot$, #1, #4, #7,errormsg$)
               if errormsg$ <> " " then return

L51330:     call "READ100" (#2, tstore$, f1%(2))   /* JIC we create it */
                if f1%(2) = 1% then goto L51380
                   errormsg$ = "'TO' Store does not exist."
                   return

L51380:     if enablet% > 0% then L51410
               tlot$ = " "   /* Not Lot Tracked, Shouldn't be enabled */
               goto L51490
L51410:     if enablet% < 2% then L51490   /* Must be Memo Only */
               if tlot$ <> " " then L51460
                  errormsg$ = "This is a Lot Tracked Part - 'TO' " &     ~
                              "Lot Number is REQUIRED"
                  return
L51460:     call "LOTVALID" (tpart$, tstore$, tlot$, #1, #4, #7,errormsg$)
               if errormsg$ <> " " then return

L51490
*       *** NOTE: Implicit check of FROM store via existence of HNYQUAN
            call "HNYAVAIL" (#4, #7, fpart$, fstore$, flot$, errormsg$,  ~
                             mq, avail, return%) : avail = avail
            if errormsg$ <> " " then return
               /* ERR$ = " " - RET% =  0% -> A OK                      */
               /*            - RET% =  1% -> HNYQUAN, Neg Lot <> "Y"   */
               /*            - RET% = 99% -> No HNYQUAN, Neg Lot <> "Y"*/
            if return% = 0% then L51760
            if return% = 1% then L51620  /* Else must be 99%            */
               errormsg$ = "There is no such 'FROM' Store/Lot."
               return
*       *** NOTE: If this is relaxed, from store must be checked above

L51620:     keyhit1% = 2%
            call "ASKUSER" (keyhit1%, " *** WARNING *** ",               ~
                            "This Movement will result in a Negative " & ~
                            "Quantity in the FROM Store/Lot.",           ~
                        "Since Negative Lots are allowed on this Part,", ~
                        "Press <RETURN> to Continue, OR PF-16 to Abort.")

             if keyhit1%  =  0% then L51760
             if keyhit1% <> 16% then L51620
                errormsg$ = "Movement Exceeds Maximum Quantity Available:"
                call "CONVERT" (avail, -0.2, str(errormsg$,              ~
                                len(errormsg$)+2%, 10%))
                return

L51760
*       ** End of Testing & Ultimate return
             return

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC
        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" (" ", " ", 0%, 1%)
            call "FILEBGON" (#50)
            call "JNLCLOSE" (modno$, jnlid$, pstseq%, returncode%)
            end
