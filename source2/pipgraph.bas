        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   IIIII  PPPP    GGG   RRRR    AAA   PPPP   H   H   *~
            *  P   P    I    P   P  G      R   R  A   A  P   P  H   H   *~
            *  PPPP     I    PPPP   G  GG  RRRR   AAAAA  PPPP   HHHHH   *~
            *  P        I    P      G   G  R  R   A   A  P      H   H   *~
            *  P      IIIII  P       GGG   R   R  A   A  P      H   H   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPGRAPH - Allows selection of a part then invokes the VB *~
            *            program of the same name to display PIP        *~
            *            data about that part.                          *~
            *-----------------------------------------------------------*~
            * This program detains valuable traded secrets and propit-  *~
            * ious asses of Caelus, Inc., Spokane, WA.  It may be em-   *~
            * boDYING substandard creative efforts and inconsequential  *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is inhibited.  *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/27/95 ! ORIGINAL (used PLNRSUB to start)         ! ERN *~
            * 12/08/95 ! CLEANED UP USER INTERFACE                ! LDJ *~
            * 01/04/96 ! Undid 'Clean up' and added VB Start form ! ERN *~
            * 09/04/96 ! Millie date conversion                   ! DER *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

          dim                                                            ~
           atc_h$4,                      /* ATC Horizon Switch         */~
           command$256,                                                  ~
           errormsg$79,                  /* ERROR MESSAGE              */~
           hex7f$,                                                       ~
           msg$255,                                                      ~
           nbr$16,                                                       ~
           partdescr$34,                                                 ~
           part$25,                      /* The Part Number            */~
           part2$25,                     /* The other Part Number      */~
	   tdate$10,udate$8,             /* temp stuff                 */~
           yymmdd$(490)6                 /* Date Reference array       */

          dim                                                            ~
           atc1%(490), atc2%(490),       /* Atc w/ and w/o Fcst        */~
           avail%(490),                  /* PIP array                  */~
           cumf%(490),                   /* Cumm Fcst                  */~
           cursor%(2),                   /* Search function receiver   */~
           f1%(64),                      /* Read status flags          */~
           work%(490)                    /* Work array                 */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #02 ! PIPMASTR !                                          *~
            * #04 ! HNYMASTR !                                          *~
            * #12 ! CALMASTR !                                          *~
            * #16 ! HNYALTRS !                                          *~
            * #41 ! SFCUM2   !                                          *~
            *************************************************************


           select # 2, "PIPMASTR", varc, indexed,                        ~
                       recsize = 2024, keypos = 2, keylen = 25,          ~
                       alt key 1,      keypos = 1, keylen = 26

            select # 4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos =  90, keylen = 4, dup

           select #12, "CALMASTR", varc, indexed,                        ~
                       recsize = 1962, keypos = 1, keylen = 2

           select #16, "HNYALTRS", varc, indexed,                        ~
                       recsize =  60, keypos = 1, keylen = 33

           select #41, "SFCUM2",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos  = 1,    keylen = 25

            call "OPENCHCK" (#02, open02%, 0%, 0%, " ")
            call "OPENCHCK" (#04, open04%, 0%, 0%, " ")
            call "OPENCHCK" (#12, open12%, 0%, 0%, " ")
            call "OPENCHCK" (#16, open00%, 0%, 0%, " ")
            call "OPENCHCK" (#41, open00%, 0%, 0%, " ")

            errormsg$ = "Unable to open required files."
            if open02% = 0% or open04% = 0% or open12% = 0%              ~
                then exit_la3lwb


        REM *************************************************************~
            *     INITIALIZATION AND ENTRY                              *~
            *************************************************************

            call "CHECKGUI" addr(gui%)
            errormsg$ = "Presentation protocol is not GUI capable."
            if gui% = 0% then exit_la3lwb

            gosub load_calendar

            errormsg$ = "Today is not in PIP Calendar."
            search str(yymmdd$(),1) = date to cursor%() step 6
            if cursor%(1) = 0% then exit_la3lwb
            today% = (cursor%(1)/6%) + 1%


        REM *************************************************************~
            *   S E E    P I P   I N F O R M A T I O N                  *~
            *************************************************************
        input_screen

*          CALL "ASKGUI" (35%, "PIP Graph Parameters",                  ~
*            "Use Variable ATC Horizon?",KH%)
*          IF KH% = 2% THEN EXIT_GRACEFULLY
*          IF KH% = 6% THEN ATC_H$ = "YES" ELSE ATC_H$ = "NO"

            call "GUIVBRUN" ("PIPGRAPH.EXE",  0%, rc%)
            if rc% < 0% then  exit_gracefully

*          ID% = 100  : MSG$ = "Howdy"     :  GOSUB SEND_COMMAND

            call "GUIFNGER" (0%, 4%, atc_h$, rc%)
            if atc_h$ = "EXIT" or rc% < 0% then  exit_gracefully
            if str(atc_h$,,1) = "Y" then atc_h$ = "YES" else             ~
                                         atc_h$ = "NO"
        input_error
            part$ = " "
            errormsg$ = hex(06) & "Select the Part to review PIP/ATC for"
            call "GETCODE" (#4, part$,errormsg$, 0%,.32,f1%(4))
            if f1%(4) = 0% then exit_gracefully


        REM *************************************************************~
           *    L O A D   PIP   I N F O R M A T I O N                   *~
           **************************************************************

            part2$      = part$

            mat atc1%   = zer
            mat atc2%   = zer
            mat avail%  = zer
            mat cumf%   = zer
            mat work%   = zer

            call "PIPFLAGS" (part$, today%, today%, 0, #2, #41)
            call "READ100"  (#2, part$, f1%(2))
            if f1%(02) <> 0% then L10440
               call "ASKGUI" (21%, "No PIP for Part",                    ~
                  "Part " & part$ & " has no PIP data.  Select Retry to S~
        ~elect another part or Cancel to exit.", kh%)
               if kh% = 4% then input_error
                goto exit_gracefully

L10440:     call "SHOSTAT" ("Gathering PIP Data, One Moment Please")

*        Get PIPMASTR data
            get #2, using L10490, str(s$,,1), oh, ss, moq, pz, type%, lt%,~
                                 atch%
L10490:         FMT  CH(01), XX(25), XX(1960), 4*PD(14,4), 3*BI(2)
            type% = type%
            call "MXFL4GT" addr(#2, 26%, avail%(1%), 490%)
            atch%     = mod(atch%, 1000%)
            if atc_h$ = "YES" then atch1% = atch% else atch1% = 999%

*        Get Part Information
            call "DESCRIBE" (#4, part$, partdescr$, 0%, f1%(4))

            init (" ") str(s$,2)
            if pos("23" = str(s$,,1)) > 0% then str(s$,2)="Surplus       "
            if pos("45" = str(s$,,1)) > 0% then str(s$,2)="Safety Stock  "
            if pos("89" = str(s$,,1)) > 0% then str(s$,2)="Critical      "

*        Read and load Forecast data
            call "READ100" (#41, part$, f1%(41))
            if f1%(41) <> 1% then L10690
                call "MXFL4GT" addr(#41, 25%, cumf%(1%), 490%)

*        Set up ATC-1 array
L10690:     mat atc1% = avail%
            for k% = min(489%, today%+atch1%) to today% step -1%
                atc1%(k%) = min(avail%(k%), atc1%(k% + 1%))
            next k%

            if today% = 1% then L10800
                for k% = today% - 1%  to  1%   step -1%
                    atc1%(k%) = min(avail%(k%), atc1%(today%))
                next k%

*        Now, ATC w/ forecast taken into account
L10800:     for k% = 1% to 490%
                work%(k%) = avail%(k%) - max(0%, cumf%(k%))
            next k%

            mat atc2% = work%
            for k% = min(489%, today%+atch1%) to today% step -1%
                atc2%(k%) = min(work%(k%), atc2%(k% + 1%))
            next k%

            if today% = 1% then send_data
                for k% = today%-1% to 1% step -1%
                     atc2%(k%) = min(work%(k%), atc2%(today%))
                next k%


*       ****************************************************************
        send_data
*       *****************************************************************
            if hex7f$ > " " then L11110    /* LJ WUZ HERE */
            hex7f$ = hex(7f)

*        close down previous graph
*          COMMAND$ = HEX7F$ & "UWVBXpipgraph,9999,TTFN" & HEX7F$
*          CALL "SENDCMD" (COMMAND$)
*          CALL "PAUSE" ADDR(200%)

*        start up graph program
*          CALL "GUIVBRUN" ("PIPGRAPH.EXE", 0%, RC%)
*          IF RC% < 0% THEN RETURN

*        Code 100 - send initializtion message
L11110:     id% = 100  : msg$ = "Howdy"     :  gosub send_command

*        10xx -- Descriptors and Codes
            id% = 1001 : msg$ = part$       :  gosub send_command
            id% = 1002 : msg$ = partdescr$  :  gosub send_command

            msg$ = "#"
            id%  = 1003 : nbr = oh          :  gosub send_command
            id%  = 1004 : nbr = ss          :  gosub send_command
            id%  = 1005 : nbr = lt%         :  gosub send_command
            id%  = 1006 : nbr = moq         :  gosub send_command
            id%  = 1007 : nbr = pz          :  gosub send_command
            id%  = 1008 : nbr = atch%       :  gosub send_command

            id%  = 1009 : msg$ = s$         :  gosub send_command

*        Arrays...
            for k% = 1% to 490%
              id%  = 2000% + k% : tdate$ = yymmdd$(k%)
	      call "DATEFMT" (tdate$,tnum%,udate$)
	      msg$ = str(udate$,3%,6%)
	      gosub send_command
              msg$ = "#"
              id%  = 3000% + k% : nbr  = atc1%(k%)   : gosub send_command
              id%  = 4000% + k% : nbr  = cumf%(k%)   : gosub send_command
              id%  = 5000% + k% : nbr  = avail%(k%)  : gosub send_command
              id%  = 6000% + k% : nbr  = atc2%(k%)   : gosub send_command
            next k%

*        Now signal end of data...
            id% = 9001 : msg$ = "Do It" : gosub send_command
            call "SHOSTAT" ("Waiting for Signal from VB Application")
L11400:     call "GUIFNGER" (0%, 0%, msg$, k%)
            if k% < 0% then L11400
            if k% = 1% then input_error
            goto exit_gracefully


*       -----------*
        send_command
*       -----------*
            convert id% to command$, pic (0000)
            if msg$ <> "#" then command$ = command$ & "," & msg$
            if msg$ <> "#" then L11540
                convert nbr to nbr$, pic (-000000000)
                command$ = command$ & "," & nbr$
L11540:     command$ = hex7f$ & "UWVBXpipgraph," & command$ & hex7f$
            call "SENDCMD" (str(command$,,len(command$)))
            return


*       ****************************************************************
        load_calendar
*       ****************************************************************

            errormsg$ = "Calendar file incomplete or damaged."

            call "READ100" (#12,"10", f1%(12))
            if f1%(12) = 0 then exit_la3lwb
                get #12, using L11680, str(yymmdd$(),,1470)
L11680:             FMT XX(2), CH(1470)

            call "READ100" (#12,"11", f1%(12))
            if f1%(12) = 0 then exit_la3lwb
                get #12, using L11680, str(yymmdd$(),1471,1470)

            return


        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_gracefully
            command$ = hex7f$ & "UWVBXpipgraph,9999,TTFN" & hex7f$
            if gui% = 1% then call "SENDCMD" (command$)
            end

        exit_la3lwb       /* Exit Like A 3-Legged Water Buffalo        */
            if gui% = 0% then                                            ~
            call "ASKUSER" (0%, "aBnOrMaL eXiT",                         ~
                    "Program is ending due to the following reason:",    ~
                    errormsg$, "Press any key to continue...")           ~
            else                                                         ~
            call "ASKGUI" (16%, "Error!", errormsg$, kh%)
            end

