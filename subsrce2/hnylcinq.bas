        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y   Y  L       CCC   IIIII  N   N   QQQ    *~
            *  H   H  NN  N   Y Y   L      C   C    I    NN  N  Q   Q   *~
            *  HHHHH  N N N    Y    L      C        I    N N N  Q   Q   *~
            *  H   H  N  NN    Y    L      C   C    I    N  NN  Q Q Q   *~
            *  H   H  N   N    Y    LLLLL   CCC   IIIII  N   N   QQQ    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYLCINQ -  This subroutine is used to provide a specific *~
            *             display and select function for file HNYLOCNS.*~
            *             As such it is not a general purpose routine   *~
            *             though it was cloned from one which was       *~
            *             (PLOWCODE) and still shares some of the       *~
            *             characteristics of a general purpose routine. *~
            *             Through this routine a user will be able to   *~
            *             select a specific combination of store, part, *~
            *             location, and lot codes to find records in    *~
            *             this file.                                    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/27/85 ! ORIGINAL (RAPED STRAIGHT FROM PLOWCODE)  ! LDJ *~
            * 04/19/89 ! O.S. 7.20/30 Comapatability (WSXIO).     ! KAB *~
            * 10/28/92 ! Change GETUFBF1 to GETIFBS1.             ! JDH *~
            * 06/20/94 ! Platform Sensitive for Keyboard Interrupt! KAB *~
            *************************************************************

            sub "HNYLCINQ" (#1,          /* UFB of File to Search      */~
                                                                         ~
                           key$,         /* Coming In: Alternate or    */~
                                         /* Primary Key of record to   */~
                                         /* Search for.                */~
                                         /* Going Out: Alternate or    */~
                                         /* Primary Key Passed in if   */~
                                         /* found or no selection made,*/~
                                         /* Appropriate key of selected*/~
                                         /* record if selection made.  */~
                                                                         ~
                           descr$,       /* Prompt descriptn coming in.*/~
                                         /* Selected Record description*/~
                                         /* field going out.  The      */~
                                         /* description field is always*/~
                                         /* assumed to immediately     */~
                                         /* follow the primary key in  */~
                                         /* the record per standard    */~
                                         /* Caelus design.             */~
                                                                         ~
                           break%,       /* Control Break.  Not used at*/~
                                         /* present.                   */~
                                                                         ~
                           key_desclen,  /* Floating Point where the   */~
                                         /* Integer portion is the KEY */~
                                         /* number to be used (0-16).  */~
                                         /* This number indicates if   */~
                                         /* key is a primary (0) or an */~
                                         /* alternate (>0). If an alt  */~
                                         /* key is specified the file  */~
                                         /* must already have an exist-*/~
                                         /* ing keypath for that number*/~
                                         /*                            */~
                                         /* The fraction or portion to */~
                                         /* the right of the decimal   */~
                                         /* is assumed to be the length*/~
                                         /* of the description field   */~
                                         /* in the record.             */~
                                         /* For example:               */~
                                         /*   .30 or .3 means length 30*/~
                                         /*   .03 means length 3, etc. */~
                                         /* A length of 0 (.0) is      */~
                                         /* assumed to mean there is no*/~
                                         /* description & consequently */~
                                         /* one will not be displayed  */~
                                         /* or returned.               */~
                                                                         ~
                          f1%)           /* Return Status for Call.    */~
                                         /* 1 = Record Found/Selected. */~
                                         /* 0 = Record not on file OR  */~
                                         /*     file not open  OR      */~
                                         /*     no selection made OR   */~
                                         /*     invalid Call argument. */~

        REM *************************************************************~
            *                  HNYLCINQ VARIABLES                       *~
            *************************************************************

        dim                              /* MISCELLANEOUS VARIABLES    */~
            aid$1,                       /* WORKSTATION AID BYTE (PFK) */~
            answer$79,                   /* INPUT RECEIVER - LINE 21   */~
            f1%(1),                      /* FILE STATUS VARIABLE       */~
            key1$(24)100,                /* RECORD KEYS                */~
            iosw_receiver$8,             /* IO WORD STATUS BYTES       */~
            loc$8,                       /* Location Of Part           */~
            lot$6,                       /* Lot                        */~
            mask$1,                      /* 'AND' MASK VARIABLE        */~
            part$25,                     /* Part Code                  */~
            plowkey$255,                 /* MISCELLANEOUS PLOW & READ  */~
            qty$10,                      /* Quantity field             */~
            readkey$255,                 /* KEY FOR PLOW               */~
            s1$(24)80,                   /* PRIOR SCREEN CONTENTS      */~
            store$3,                     /* Warehouse (Store) Code     */~
            version$6,                   /* Test Platform              */~
            ufbkl$1,                     /* KEY LENGTH THIS FILE       */~
            ufbkd$2,                     /* KEY DISPLACEMENT REL TO 0  */~
            work$(8)256                  /* WORK AREA FOR TRANSFER     */

        dim     /* THESE VARIABLES SHOULD NOT BE DISTURBED OR MOVED !  */~
            order$4,                     /* SCREEN ORDER AREA          */~
            s$(24)80                     /* CURRENT SCREEN TEXT ARRAY  */

        dim     /* PASSED ARGUMENTS                                    */~
            descr$100,                   /* RETURNED DESCRIPTION       */~
            key$255                      /* READ KEY                   */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
        REM *************************************************************
            select #5,  "CRTFILE",                                       ~
                        consec,                                          ~
                        recsize = 1924

        REM *************************************************************~
            *     CHECK TO SEE IF THE FILE IS OPEN ON THIS CHANNEL      *~
            *              IF NOT EXIT IMMEDIATELY !                    *~
            *************************************************************
            f1% = 0%
            call "GETUFBS1" addr(#1, open%)
                 if open% = 0% then end

*        Determine Platform for this routine
            if unix% <> 0% then L09164   /* Done only once */
            call "EXTRACT" addr("S#",version$)
            convert version$ to unix%, data goto L09162
               goto L09164
L09162:           unix% = -1%
L09164
*        Now if UNIX% >= 0% this is a VS Platform, otherwise its UNIX

        REM *************************************************************~
            *       GET OR CALCULATE THE FILE PARAMETERS NEEDED         *~
            *      (KEY #, KEY LENGTH, POSITION, DESCRIPTION LENGTH)    *~
            *************************************************************
            brk% = break%
            break% = abs(break%)
            init (hex(00)) ufbkl$, ufbkd$
            call "GETUFBKL" addr(#1, ufbkl$)       /* KEY LENGTH (BIN) */
            call "GETUFBDK" addr(#1, ufbkd$)       /* KEY DISPL. (BIN) */
            disp% = 1+val(ufbkl$)+val(ufbkd$,2)
            key%    = int(abs(key_desclen))
            deslen% = abs(mod(key_desclen*100,100)+.1)

        REM *************************************************************~
            *      DOES THE PASSED KEY EXIST ON FILE AS IS ?            *~
            *IF IT'S ON FILE, THEN WIP RIGHT BACK WITH MINIMUM EFFORT   *~
            *************************************************************
            if brk% >= 0% then call "REDALT0" (#1, key$, key%, f1%)      ~
            else call "PLOWALTS" (#1, key$, key%, break%, f1%)
            if f1% = 0 then L09566
               get #1,str(work$())
               if deslen% > 0% then descr$ = str(work$(),disp%,deslen%)
               goto L65000
L09566: REM *************************************************************~
            * MAKE SURE THAT THERE'S RECORDS ON FILE WITHIN THE GIVEN   *~
            * RANGE, OR IF KEY_DESCLEN < 0 WITHIN THE ENTIRE FILE       *~
            *************************************************************
            plowkey$ = key$
            str(plowkey$,break%+1%) = all(hex(00))
            call "PLOWALTS" (#1, plowkey$, key%, break%, f1%)
            if f1% > 0% then L09620
            if key_desclen >=0 then L65000
            plowkey$ = all(hex(00))
            call "PLOWALTS" (#1, plowkey$, key%, 0%, f1%)
            if f1% = 0% then L65000

L09620: REM *************************************************************~
            * CLOSE THE WORKSTATION AND REOPEN IT USING WSXIO ROUTINE TO*~
            * ALLOW WINDOWING, ETC.                                     *~
            *************************************************************
            f1% = 0%
            close ws
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
            rlen% = 1920%
            r0% = 0% : r9% = 26%
            gosub read_screen
            r% = 2%
            r0% = 1%
            r9% = 24%
            gosub initialize

        REM *************************************************************~
            *           M A I N   P R O G R A M   B E G I N S           *~
            *************************************************************

        main_control
            gosub write_screen
            gosub read_screen
            on keyhit% + 1% gosub check_it,        /* PF 0             */~
                                  higher_level,    /* PF 1             */~
                                  first_screen,    /* PF 2             */~
                                  ,                /* PF 3             */~
                                  ,                /* PF 4             */~
                                  next_screen,     /* PF 5             */~
                                  ,                /* PF 6             */~
                                  next_line,       /* PF 7             */~
                                  find_code,       /* PF 8             */~
                                  ,                /* PF 9             */~
                                  shrink_up,       /* PF 10            */~
                                  expand_down,     /* PF 11            */~
                                  ,                /* PF 12            */~
                                  documentation,   /* PF 13            */~
                                  ,                /* PF 14            */~
                                  print_screen,    /* PF 15            */~
                                  quit,            /* PF 16            */~
                                  ,                /* PF 17            */~
                                  ,                /* PF 18            */~
                                  ,                /* PF 19            */~
                                  ,                /* PF 20            */~
                                  ,                /* PF 21            */~
                                  ,                /* PF 22            */~
                                  ,                /* PF 23            */~
                                  ,                /* PF 24            */~
                                  ,                /* PF 25            */~
                                  shrink_down,     /* PF 26            */~
                                  expand_up,       /* PF 27            */~
                                  ,                /* PF 28            */~
                                  ,                /* PF 29            */~
                                  ,                /* PF 30            */~
                                  ,                /* PF 31            */~
                                  quit             /* PF 32            */

            goto main_control

        check_it
            if r% < r0% + 2% or r% > r0% + r9% - 2% then return
            on high_level% goto L10560
            readkey$ = key1$(r%)
L10470:
            if brk% < 0% then str(readkey$,abs(brk%)+1) =all(hex(00))
            if brk% < 0% then                                            ~
               call "PLOWALTS" (#1, readkey$, key%, abs(brk%), f1%)      ~
            else call "REDALT0" (#1, readkey$, key%, f1%)
            if f1% = 0% then return
            key$ = key(#1,key%)
            get #1,str(work$())
            if deslen% > 0% then descr$ = str(work$(),disp%,deslen%)
            goto quit
L10560:     if str(s$(r%),4%,36%) = " " then return
            if brk% < 0% then L10470
            key$ = str(s$(r%),4%,36%)
            break% = save_break%
            high_level% = 0%
            gosub'106(r0%)
            gosub'105(r0%+r9%-1%)
            gosub first_screen
            return

        higher_level
            if key_desclen >= 0 then return
            if break% < 1% then return
            high_level% = 1%
            str(plowkey$,break%+1%) = all(hex(00))
            break% = 0%
            windows% = 1%
            gosub'106(r0%)
            gosub'105(r0%+r9%-1%)
            f1%(1) = 1%
            gosub next_screen
            return

        first_screen
            plowkey$=key$
            str(plowkey$,break%+1%) = all(hex(00))
            gosub'101(r0%+2%,r0%+r9%-2%)
            r% = r0% + 2%
            c% = 2%
            return

        next_screen
            if f1%(1) = 0% then return
            gosub'101 (r0%+2%,           /* FIRST ROW TO REPLACE       */~
                       r0%+r9%-2%)       /* LAST ROW TO REPLACE        */
            r% = r0%+2%
            c% = 2%
            return

        next_line
            if f1%(1) = 0% then return
            if r9% < 4% then L11000
            copy str(s$(),(r0%+2%)*80%+1%,(r9%-4%)*80%)                  ~
              to str(s$(),(r0%+1%)*80%+1%,(r9%-4%)*80%)
L11000:     gosub'101(r0%+r9%-2%,r0%+r9%-2%)
            r% = r% - 1%
            if r% < r0%+2% then r% = r0%+2%
            str(iosw_receiver$,3%,1%) = hex(00)
            gosub interrupt_check
            if aid$ <> hex(00) then return
            gosub write_screen
            goto next_line

        find_code
            answer$ = " "
            if key% = 0% then len% = val(ufbkl$)                         ~
            else len% = len(str(key(#1,key%)))
            if high_level% > 0% and save_break%>0% then len% = save_break%
L11130:     gosub'103
            if keyhit% = 1% then return
            if keyhit% <> 0% then L11130
            str(plowkey$,break%+1%)= str(answer$,,len%) addc all(hex(ff))
            f1%(1) = 1%
            gosub next_screen
            return

        shrink_up
            if r9% < 4% then return
            override% = 1%
            s$(r0%+r9%-2%) = s$(r0%+r9%-1%)
            s$(r0%+r9%-1%) = s1$(r0%+r9%-1%)
            r9% = r9% - 1%
            if r% > r0% + r9% - 2% then r% = r0% + r9% - 2%
            str(plowkey$,break%+1%)=str(s$(r0%+r9%-2%),4%,36%)
            return

        expand_down
            if r9% > 23% then return
            if r0% + r9% > 24% then return
            override% = 1%
            r9% = r9% + 1%
            s$(r0%+r9%-1%) = s$(r0%+r9%-2%)
            gosub'101(r0%+r9%-2%,r0%+r9%-2%)
            return
        REM                                                              ~
        WINDOW_FLIP
            override% = 1%
            str(plowkey$,break%+1%) = str(s$(r0%+2%),4%,36%)
            str(plowkey$,break%+1,max(1%,len(plowkey$)-break%)) =        ~
            str(plowkey$,break%+1,max(1%,len(plowkey$)-break%))          ~
                     addc all(hex(ff))
            f1%(1) = 1%
            if r9% > 23% then L11540
               s$(1%) = s$(r0%)
               s$(2%) = s$(r0%+1%)
               s$(24%)= s$(r0%+r9%-1%)
               r0% = 1%
               r9% = 24%
               gosub next_screen
               return
L11540:     s$(r10%) = s$(r0%)
            s$(r10%+1%) = s$(r0%+1%)
            s$(r10%+r19%-1%) = s$(r0%+r9%-1%)
            r0% = r10%
            r9% = r19%
            r% = r0%+2%
            c% = 2%
            if r0% < 3% then L11640
                for x% = 1% to r0%-1%
                    s$(x%) = s1$(x%)
                next x%
L11640:     if r0% + r9% > 24% then L11680
                for x% = r0% + r9% to 24%
                    s$(x%) = s1$(x%)
                next x%
L11680:     gosub next_screen
            return

        documentation
            call "WSXIO" addr("C", #5)
            call "MANUAL" ("HNYLCINQ")
            close ws
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
            override% = 1%
            gosub write_screen
            return

        print_screen
            call "WSXIO" addr("C", #5)
            call "PRNTSCRN"
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
            return

        do_windows             /* DISPLAY WINDOW PF KEY FUNCTIONS      */
            if windows% = 0% then                                        ~
               gosub'105(r0%+r9%-1%)     /* DISPLAY PROMPT MESSAGE     */~
            else                                                         ~
               gosub'104(r0%+r9%-1%)     /* DISPLAY WINDOWING PF KEYS  */
            windows% = abs(windows% - 1%)
            return

        shrink_down
            if r0% + r9% < 4% then return
            if r9% < 4% then return
            if r0% > 21% then return
            override% = 1%
            s$(r0%+2%) = s$(r0%+1%)
            s$(r0%+1%) = s$(r0%)
            s$(r0%) = s1$(r0%)
            r0% = r0% + 1%
            r9% = r9% - 1%
            if r% < r0% + 2% then r% = r0% +2%
            return

        expand_up
            if r9% > 23% then return
            if r0% < 2% then return
            override% = 1%
            r0% = r0% - 1%
            r9% = r9% + 1%
            s$(r0%) = s$(r0%+1%)
            s$(r0%+1%) = s$(r0%+2%)
            str(plowkey$,break%+1%)=str(s$(r0%+3%),4%,36%)
            str(plowkey$,break%+1,max(1%,len(plowkey$)-break%)) =        ~
            str(plowkey$,break%+1,max(1%,len(plowkey$)-break%))          ~
                     addc all(hex(ff))
            f1%(1) = 1%
            gosub next_screen
            return

        quit
            order$=hex(01000101)
            call "WSXIO" addr("X", #5, hex(80), order$, s1$(),1920%,     ~
                              iosw_receiver$)
            call "WSXIO" addr("C", #5)
            goto L65000

        deffn'101(r1%,r2%)
L12310:     call "PLOWALTS" (#1, plowkey$, key%, break%, f1%(1))
            if f1%(1) = 0% then L12480
            get #1,str(work$())
            key1$(r1%) = plowkey$
            s$(r1%)=hex(860b8c)
            get str(work$(),573,8) using L12462, qty
            convert qty to qty$, pic(##########)
            if high_level%=0% then                                       ~
              str(s$(r1%),4%)=str(work$(),1,3) & "  " & str(work$(),4,8) ~
              & " " & str(work$(),12,25) & " " & str(work$(),37,6) &     ~
                   " " & str(qty$,,10%) & " " & str(work$(),43,22)       ~
            else                                                         ~
               str(s$(r1%),4%)=str(plowkey$,,save_break%)
            if high_level% > 0% then str(plowkey$,save_break%+1%) =      ~
               all(hex(ff))
            r1% = r1% + 1%
            if r1% > r2% then return
            goto L12310

L12462:     FMT PD(14,4)

L12480:     REM *** END OF FILE ***
               for x% = r1% to r2%
                 s$(x%) = " "
                 key1$(x%) = " "
               next x%
               return

        deffn'103
        REM TRAN(ANSWER$,HEX(0B20))REPLACING
            s$(r0%+r9%-1%) = hex(84) & "WHSE:" & hex(a1) & str(store$) & ~
              hex(8c84) & "PART:" & hex(a1) & str(part$) & hex(84) &     ~
              "LOT:" & hex(a1) & str(lot$) & hex(84) & "LOC:" & hex(a1) &~
              str(loc$) & hex(8c)
            r1% = r0%+r9%-1%
            c1% = pos(s$(r0%+r9%-1%)=hex(a1)) + 1%
            order$ = bin(r1%) & hex(a4) & bin(c1%) & bin(r1%)
            call "WSXIO" addr("X", #5, hex(80), order$, s$(r1%),         ~
                               80%, iosw_receiver$)
            order$ = bin(r1%,1) & hex(000000)
            call "WSXIO" addr("X", #5, hex(40), order$, s$(r1%),80%,     ~
                                   iosw_receiver$)
            aid$ = str(iosw_receiver$,3,1)
            gosub decode_aid_byte
            store$ = str(s$(r0%+r9%-1%),8,3)
            part$  = str(s$(r0%+r9%-1%),19,25)
            lot$   = str(s$(r0%+r9%-1%),50,06)
            loc$   = str(s$(r0%+r9%-1%),62,08)
            on key% + 1% gosub L12751, L12753, L12755, L12757
        REM TRAN(ANSWER$,HEX(200B))REPLACING
            windows% = abs(windows% - 1%)
            gosub do_windows
            return

L12751:     answer$ = str(store$) & str(loc$) & str(part$) & lot$
            return
L12753:     answer$ = str(store$) & str(part$) & str(lot$) & loc$
            return
L12755:     answer$ = str(store$) & str(part$) & str(loc$) & lot$
            return
L12757:     answer$ = str(part$) & str(store$) & str(loc$) & lot$
            return

        REM *************************************************************~
            *                 W R I T E   S C R E E N                   *~
            *************************************************************
        write_screen:
            on override% goto L12850
            order$=bin(r0%+1%,1) & hex(a0) & bin(c%,1) & bin(r%,1)
            start% = r0%*80%+1%
            slength% = (r9%-2%)*80%
            goto L12880
L12850:     order$=bin(1%,1) & hex(a0) & bin(c%,1) & bin(r%,1)
            start% = 1%
            slength% = 1920%
L12880:     call "WSXIO" addr("X", #5, hex(80), order$, str(s$(), start%,~
                              slength%),slength%, iosw_receiver$)
            override% = 0%
            return

        REM *************************************************************~
            *                 R E A D   S C R E E N                     *~
            *************************************************************
        read_screen:
            order$=bin(01%,1) & hex(000000)
            call "WSXIO" addr("X", #5, hex(40), order$, s1$(), rlen%,    ~
                     iosw_receiver$)
            aid$ = str(iosw_receiver$,3,1)
            gosub decode_aid_byte
            rlen% = 0%
            c% = val(str(order$,3,1),1)
            r% = val(str(order$,4,1),1)
            if r% > r0% then L13090
L13060:        r% = r0%+1%
               c% = 2%
               return
L13090:     if r% < r0%+r9%-1% then return
               goto L13060

        interrupt_check

*       *** Platform Sensitive Code
            if unix% < 0% then L13135
            call "WSXIO" addr("W", #5, 1%, iosw_receiver$)
               goto L13138
L13135:     call "WSXIO" addr("A", #5, str(iosw_receiver$,3%,1%))
               if str(iosw_receiver$,3%,1%) < hex(40) then               ~
                  str(iosw_receiver$,3%,1%) = hex(00)
L13138
*       *** Platform Sensitive Code

            aid$ = str(iosw_receiver$,3,1)
            return

        decode_aid_byte
            tran(aid$,hex(00400141024203430444054506460747084809490a4a0b4~
        b0c4c0d4d0e4e0f4f10501161126213631464156516661767186819691a6a1b6b~
        1c6c1d6d1e6e1f6f2070))replacing
            keyhit% = val(aid$,1)
            return



        REM *************************************************************~
            *               I N I T I A L I Z A T I O N                 *~
            *************************************************************
        initialize
            r10% = r0%
            r19% = r9%
            init(" ")key1$()
            mask$ = hex(08)
            x% = 1%
L13350:     p% = pos(str(s1$(),x%) > hex(7f))
            if p% = 0% then L13410
                str(s1$(),x%-1%+p%,1%) = boole mask$
                str(s1$(),x%-1%+p%,1%) = bool8 hex(fd)
                str(s1$(),x%-1%+p%,1%) = or hex(04)
                x% = x% + p%
                if x% < 1920% then L13350

L13410:     mat s$ = s1$
            for x% = r0%+1% to r0%+r9%-2%
                s$(x%) = " "
            next x%
            rlen% = 0%
            f1%(1) = 1%
            windows% = 1%
            save_break% = break%
            high_level% = 0%
            plowkey$ = key$
            if key_desclen >= 0 or break% = 0% then L13600
            if brk% >= 0% then L13520
               gosub higher_level
               goto L13590
L13520:     call "PLOWALTS" (#1, plowkey$, key%, break%, f1%(1))
            if f1%(1) > 0% then L13590
               plowkey$ = all(hex(00))
               gosub higher_level
               plowkey$ = all(hex(00))
               str(plowkey$,break%+1%,1%) = hex(01)
               goto L13620
L13590:     plowkey$ = key$
L13600:     gosub'106(r0%)
            gosub'105(r0%+r9%-1%)
L13620: REM STR(S$(R0%+1%),4%,36%) = STR(PLOWKEY$,BREAK%+1%)
        REM GOSUB WINDOW_FLIP
            str(plowkey$,break%+1%)=str(plowkey$,break%+1%)              ~
                     addc all(hex(ff))
            gosub next_screen
            r% = r0%+2%
            c% = 2%
            return

        deffn'104(x%)
            s$(x%) = hex(a4) &                                           ~
                "(10/26)Shrink Up/Down (11/27)Expand Up/Down " &         ~
                "(12)Full Screen Flip (15)Print"
            override% = 1%
            return

        deffn'105(x%)
            if high_level% = 0% or brk% < 0% then                        ~
            s$(x%) = hex(a4) & "Position Cursor to Desired Selection and ~
        ~Press RETURN to use that Selection"                              ~
            else                                                         ~
            s$(x%) = hex(a4) & "Position Cursor to Desired Value and pres~
        ~s RETURN to explode/implode that Value"
            override% = 1%
            return

        deffn'106(x%)
            on (sgn(save_break%)+high_level%+1%) gosub L13890,L13930,L13990
            override% = 1%
            return
L13890:     REM *** LOW LEVEL, BREAK POINT = 0 ***
            s$(x%)  = hex(a4) & descr$
L13910:     str(s$(x%),36)="(2)1ST (5/7)NEXT (8)FIND (16)EXIT            "
            str(s$(x%+1%),3)=hex(ac) & "WHSE" & hex(ac) & "LOCATION" &   ~
                          hex(ac) & "PART CODE                "     &    ~
                          hex(ac) & "LOT   " & hex(ac) & "QUANTITY  " &  ~
                          hex(ac) & "LOCATION TEXT"
            return
L13930:     REM *** LOW LEVEL, BREAK POINT > 0 ***
            s$(x%) = hex(a4) & "CODES Under " & str(key$,,break%)
            str(s$(x%+1%),3)=hex(ac) & "WHSE" & hex(ac) & "LOCATION" &   ~
                          hex(ac) & "PART CODE                "    &     ~
                          hex(ac) & "LOT   " & hex(ac) & "QUANTITY  " &  ~
                          hex(ac) & "LOCATION TEXT"
            if key_desclen < 0 then str(s$(x%),21%) =                    ~
           "(1)PRIOR LEVEL (2)1ST (5/7)NEXT (8)FIND (16)EXIT            "~
            else goto L13910
            return
L13990:     REM *** HIGH LEVEL ***
            s$(x%) = hex(a4) & "HIGH LEVEL VALUES"
            goto L13910

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
