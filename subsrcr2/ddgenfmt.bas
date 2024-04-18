        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  DDDD   DDDD    GGG   EEEEE  N   N  FFFFF  M   M  TTTTT   *~
            *  D   D  D   D  G      E      NN  N  F      MM MM    T     *~
            *  D   D  D   D  G GGG  EEEE   N N N  FFFF   M M M    T     *~
            *  D   D  D   D  G   G  E      N  NN  F      M   M    T     *~
            *  DDDD   DDDD    GGG   EEEEE  N   N  F      M   M    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DDGENFMT - Generates a BASIC Format statement from the    *~
            *            passed parent structure array, starting at the *~
            *            passed basic line number.                      *~
            *            Maximum number of fields is maxfields%.        *~
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
            * 04/26/83 ! ORIGINAL                                 ! ECR *~
            * 07/24/84 ! MODIFIED FOR NEW S.E.S. FILES/UTILITIES  ! LDJ *~
            * 02/09/87 ! Modified to show POS positions in FMT    ! LDJ *~
            *          ! remarks.                                 !     *~
            * 11/13/87 ! Renamed from GENFMT to DDGENFMT.         ! LDJ *~
            * 05/03/88 ! Correct double release incryptions (CMSI)! HES *~
            * 08/25/97 ! Changed to No Line Number Version.       ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "DDGENFMT" (#1, #4, parent$,record$(),maxfields%)

        dim                                                              ~
            fmtdescr$(2)50,              /* Field description          */~
            fmt$13,                      /* Format & ","               */~
            out$80,                      /* Line buffer                */~
            outline$80,                  /* Line to write out          */~
            parent$16,                   /* Parent code                */~
            pos$10,                      /* Position Spec              */~
            record$(1)256,               /* Parent components array    */~
            work$(3)40                   /* Working Space Variable     */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01132
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
L01132: REM *************************************************************

L08990:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "FORMAT GENERATION FOR FILE: " &    ~
                            parent$,                                     ~
            "Press RETURN to Generate the Standard (Abbr.) Format, or",  ~
            "Press PF1 to Include POS Form-Specs (Field Positions), or", ~
            "Press PF2 to Include POS Form-Specs & Additional Comments/Do~
        ~cumentation.")
            if keyhit% > 2% then L08990

           REM NOW WRITE THE FORMAT STATEMENT...........................

            put out$, using L20080 , parent$ & "_FMT:", parent$, hex(7e)
            gosub'180
            p% = 1%                                /* Starting Position*/
            for f% = 1% to maxfields%              /* Formats          */
               fmt$ = str(record$(f%),176%,12%)
               fmtdescr$() = " "
               if fmt$ = " " then L10440
               call "READ100" (#1, str(record$(f%),,16), f1%)
               if f1% = 0% then L10440
                  get #1 using L10190, fmtdescr$(1)
L10190:           FMT POS(41), CH(50)
               if keyhit% = 0% then L10240
               work$(1) = "Position for Field " & str(record$(f%),,16%)
               pos$ = "POS(####),"
               convert p% to str(pos$,5%,4%) , pic(####)
               call "SPCSMASH" (pos$)
               put out$, using L20160, pos$, work$(1), hex(7e)
               gosub'180
L10240:        if f% <> maxfields% then                                  ~
                        fmt$ = fmt$ & ","
               call "TXTSMASH" (fmtdescr$(), work$())
               put out$, using L20110, fmt$, work$(1%), hex(7e)
               gosub'180
               if work$(2) = " " then L10280
               put out$, using L20190,  work$(2%), hex(7e)
               gosub'180
L10280:        if keyhit% = 0% then L10440
               l% = 1%
               convert str(record$(f%),188%,4%) to l%, data goto L10320
L10320:        p% = p% + l%
               if keyhit% = 1% then L10440
               if str(record$(f%),76%,100%) = " " then L10440
               str(fmtdescr$(),1%) = str(record$(f%),76%,100%)
               call "TXTSMASH" (fmtdescr$(), work$())
               for j% = 1% to 3%
                   if work$(j%) = " " then L10385
                   put out$, using L20190, work$(j%), hex(7e)
                   gosub'180
L10385:        next j%
L10440:     next f%

            gosub'180

            end

            REM Write the program line
            deffn'180
                 init(" ") outline$
                 str(outline$, 1) = out$
                 write #4, using L11040, outline$
L11040:          FMT CH(80)
                 out$ = " "
            return

        REM *************************************************************~
            *             F O R M A T   S T A T E M E N T S             *~
            *-----------------------------------------------------------*~
            * Format statements for all the code generation routines    *~
            * will be found here.                                       *~
            *************************************************************


L20080: %#####################FMT  /* FILE: ################                  */#

L20110: %          #############   /* ########################################*/#

L20160: %          ##########      /* ########################################*/#

L20190: %                          /* ########################################*/#
