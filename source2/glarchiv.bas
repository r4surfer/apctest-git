        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L       AAA   RRRR    CCC   H   H  IIIII  V   V   *~
            *  G      L      A   A  R   R  C   C  H   H    I    V   V   *~
            *  G GGG  L      AAAAA  RRRR   C      HHHHH    I    V   V   *~
            *  G   G  L      A   A  R   R  C   C  H   H    I     V V    *~
            *   GGG   LLLLL  A   A  R   R   CCC   H   H  IIIII    V     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLARCHIV - Program to allow selective (by date) archival  *~
            *            of detail from the GLDETAIL file.              *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/06/91 ! Original                                 ! SID *~
            * 09/22/92 ! Reduced # of Recs alloc at file creation.! JDH *~
            * 03/31/93 ! PRR 12792 READKEY$ Hex 00s for Details.  ! JIM *~
            * 03/31/93 ! Got rid of refs to channel #50- DUMMY.   ! JIM *~
            * 03/31/93 ! PRR 12517 (5) ASKUSER & blast ARCHVREC   ! JIM *~
            *          !  record & GLDEyyyy if nothing archived.  ! JIM *~
            * 05/06/93 ! Change due to dates change in ARCHVREC.  ! JDH *~
            * 06/17/93 ! Blast GLDEyyyy only if record count in   ! JIM *~
            *          !   in file is zero (whew! thanx, JDH).    ! JIM *~
            * 09/02/97 ! Year 2000 changes                        ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            archyear$4,                  /* Archive File Year          */~
            blankline$79,                /* Line for input screen      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dateline$79,                 /* Date line on screen        */~
            descr_m(14),                 /* Descr Map For PLOWCODE     */~
            dummydate$8,                 /* Today's Date Minus 90 Days */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            eligibles$43,                /* # of records eligibles Mtr */~
            file$8, lib$8, vol$6,        /* Get the goods on #40       */~
            fmarchdate$8,                /* Archive Date Range         */~
            getback$9,                   /* (1)Return on Safeguard Scr.*/~
            glderec$160,                 /* GLDETAIL record variable   */~
            header$42, header2$15,       /* Heading for Input Screen   */~
            header$(3)79,                /* Header for PLOWCODE call   */~
            hits$43,                     /* # of records hits Master   */~
            i$(24)80,                    /* Screen Image               */~
            inc(2),                      /* PLOWCODE Arguments or some */~
            inc$(2)8,                    /*   sort known only to LDJ   */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lfad$(2)1,                   /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mode$5,                      /* File Open Mode             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /*                            */~
            readkey$99,                  /*                            */~
            safeguard$3,                 /* Proceed to Archive (Y/N)?  */~
            toarchdate$8,                /* Archive Date Range         */~
            unffmarchdate$8,             /* Unformatted From Date Range*/~
            unftoarchdate$8,             /* Unformatted To Date Range  */~
            userid$3,                    /* Current User Id            */~
            writtens$43                  /* # of records writtens Mtr. */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20,                 /* Text from file opening     */~
            axd$4                        /* AXD Block for OPENFILE     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! GLDETAIL ! General Ledger Detailed Transaction Hist *~
            * #02 ! ARCHVREC ! Archive History Record File              *~
            * #40 ! GLDEYYYY ! Archive File For GLDETAIL                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "GLDETAIL",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  26

            select #02, "ARCHVREC",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =  1,   keylen =  30

            select #40, "GLDEYYYY",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  26


L03000:     keyhit% = 2% /* Window in the Bottom */
            call "ASKUSER" (keyhit%, "*** EXCLUSIVE FILES OPTION ***",   ~
                "Press PF1 to Archive Files in an EXCLUSIVE MODE ",      ~
                "Press PF2 to Archive Files in a SHARED MODE ",          ~
                "Press PF16 to Abort")
            if keyhit% =  1% then mode$ = "IO   "
            if keyhit% =  2% then mode$ = "SHARE"
            if keyhit% = 16% then exit_program
            if keyhit% = 1% or keyhit% = 2% then L03090 else L03000

L03090:     call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENFILE" (#01, mode$, f2%(01), rslt$(01), axd$)
              create% = val(str(rslt$(01),17,4),4) / 4%
              create% = max(100%, create%)
            call "OPENCHCK" (#02, fs%(02), f2%(02), 100%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "GLARCHIV: " & str(cms2v$,,8)
            str(header$, 1,20) = "From               "
            str(header$,21,20) = "To                 "
            header2$ = "G/L Detail File"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            gosub initialize_results

            for fieldnr% = 1% to 2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10205
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10205:               if keyhit% = 10% then gosub view_archvrec
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 10% then gosub view_archvrec
                  if keyhit% <> 16% then L11110
                     if end% = 1% then exit_program
                     gosub initialize_results
                     goto archive_data
L11110:           if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 10% then gosub view_archvrec
                  if keyhit% <>  0% then L11170
                  end% = 0%
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        archive_data

*        Get "DO YOU WANT TO PROCEED (Y/N) INFO"
            safeguard$ = " "
L19059:     gosub L42000
                if keyhit%  =  1 then editpg1
                if keyhit% <>  0 then L19059
            if safeguard$ <> "YES" then editpg1

            gosub create_arc_file
            goto do_archive

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Archive Date Range     */~
                              L20200          /* Archive File Year      */
            return

L20100: REM Def/Enable Archive Date Range          FMARCHDATE$/TOARCHDATE$
            return

L20200: REM Def/Enable Invoice Numbers Range       ARCHYEAR$
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Archive Date Range                                     ",~
         "Enter Archive File Year Name (i.e. 1988, 1989, etc...)       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$, fmarchdate$, toarchdate$,  ~
                      archyear$, glderec$
        return


        initialize_results
            hits%, eligibles%, writtens%, end% = 0%
            hits$      = "# of Records Hit      =     "
            eligibles$ = "# of Records Eligible =     "
            writtens$  = "# of Records Written  =     "
            str(hits$,33,10), str(eligibles$,33,10),                     ~
            str(writtens$,33,10) = "         0"
        return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

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
            call "OPENCHCK" (#40, fs%(40), f2%(40), 0%, rslt$(40))
                 if f2%(40) = 0% then close #40
            return clear all
            goto inputmode

        REM *************************************************************~
            *           D O   A R C H I V E                             *~
            *-----------------------------------------------------------*~
            * Perform Archiving (GLARCHIV to GLDEYYYY)                  *~
            *************************************************************

        do_archive
            call "SHOSTAT" ("Transfering Data to Archive File...")

            init(" ") glderec$, plowkey$
            init (hex(00)) readkey$

L30105:     str(readkey$,17,10) = hex(ff)
L30110:     call "READ102" (#01, readkey$, f1%(01))
                if f1%(01) = 0% then write_to_archvrec
                get #01 using L30122, str(readkey$,1,16)
L30122:                 FMT   CH(16)

            str(plowkey$,,22) = str(readkey$,1,16) & str(unffmarchdate$)
            str(plowkey$,22,1)  = addc all(hex(ff))

L30150:     call "PLOWNEXT" (#01, plowkey$, 0%, f1%(01))
                if f1%(01) = 0% then L30105

                hits% = hits% + 1%

                if str(readkey$,,16) = str(plowkey$,,16) then L30200
                   str(readkey$,,26) = str(plowkey$,,16) & hex(00000000000000)
                   goto L30110

L30200:         if str(plowkey$,17,6) > unftoarchdate$ then L30105

                eligibles% = eligibles% + 1%
                get #01, glderec$

                put #40 using L30260, str(glderec$)
L30260:                 FMT   CH(160)
                write #40, eod goto L30280

                writtens% = writtens% + 1%

L30280:         call "DELETE" (#01, str(plowkey$,1,26), 26%)
                init(" ") str(glderec$)
                goto L30150

        REM Write record to ARCHVREC file. Maybe.
        write_to_archvrec
            end% = 1%
            if writtens% > 0% then goto L30431
                call "GETNAMES" addr (#40, file$, lib$, vol$)
                call "READFDR" addr (file$, lib$, vol$, 0%, "RC", rec%,  ~
                     u3%)
                if rec% = 0% then call "FILEBGON" (#40)
                if rec% = 0%                                       then  ~
                call "ASKUSER" (0%, "*** NOTHING ARCHIVED ***",          ~
                     "No GLDETAIL records were archived per your criter"&~
                     "ia.", "No ARCHVREC record was written and " &      ~
                     gldeprname$ & " was not created.", "Press any PF k"&~
                     "ey to acknowledge and continue.")            else  ~
                call "ASKUSER" (0%, "*** NOTHING ARCHIVED ***",          ~
                     "No GLDETAIL records were archived per your criter"&~
                     "ia.", "No ARCHVREC record was written this run. " &~
                     gldeprname$ & " was not altered.", "Press any PF k"&~
                     "ey to acknowledge and continue.")
                goto editpg1

L30431
*        Something was archived. Record that fact in ARCHVREC.
            convert hits%      to str(hits$     ,33,10), pic(##########)
            convert eligibles% to str(eligibles$,33,10), pic(##########)
            convert writtens%  to str(writtens$ ,33,10), pic(##########)

            put #02 using L35070, "GLDETAIL", gldeprname$, date, time,    ~
                  unffmarchdate$, unftoarchdate$, userid$, writtens%, " "
L35070:         FMT CH(8), CH(8), CH(6), CH(8), CH(6), CH(6), CH(3),     ~
                    BI(4), CH(101)

            write #02
        goto editpg1

        view_archvrec
          header$(3) = hex(80) & "General Ledger Detail Archive History"
          header$(1) = "  File      Start Date  End Date  Archived Date" ~
                     & "  ID   Archived Name    # of Rec"


          descr_m(01) =    1.08  : descr_m(02) = 0001.0
          descr_m(03) =   31.061 : descr_m(04) = 0012.0
          descr_m(05) =   37.061 : descr_m(06) = 0023.0
          descr_m(07) =   17.061 : descr_m(08) = 0035.0
          descr_m(09) =   43.03  : descr_m(10) = 0048.0
          descr_m(11) =    9.08  : descr_m(12) = 0055.0
          descr_m(13) =   46.04  : descr_m(14) =-0068.10

          plowkey$ = "GLDETAIL" & hex(00)
          inc(1) = 1.08 : inc$(1) = "GLDETAIL"
          call "PLOWCODE" (#02, plowkey$, " ", 9000%, 0.30, f1%(02),     ~
                header$(), 0, 0, inc(), inc$(), "D", "Y", #40, descr_m())

        return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then end% = 0%
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$(), lfad$(2)      ~
                               else init(hex(86)) lfac$()

              init(hex(ac)) lfad$(1)
              if end% = 1% and fieldnr% = 0% then L40053 else L40055
L40053:            str(pf$(3),64,16) = "(16)Exit Program"
                   lfad$(1) = hex(a4) : lfad$(2) = hex(84)
L40055:       on fieldnr% gosub L40095,         /* Archive Date Range   */~
                                L40100          /* Archive File Year    */
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40100:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Archiving Selection Criteria",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),str(header$, 1, 20)     , ch(20),~
               at (06,52), fac(hex(ac)),str(header$,21, 20)     , ch(20),~
                                                                         ~
               at (07,02), "Archive Date Range",                         ~
               at (07,30), fac(lfac$( 1)), fmarchdate$          , ch(08),~
               at (07,52), fac(lfac$( 1)), toarchdate$          , ch(08),~
                                                                         ~
               at (08,02), "Archive File Year",                          ~
               at (08,30), fac(lfac$( 2)), archyear$            , ch(04),~
                                                                         ~
               at (14,29), fac(lfad$( 1)), str(header2$,,15)    ,        ~
                                                                         ~
               at (15,02), fac(lfad$( 2)), hits$                , ch(43),~
               at (16,02), fac(lfad$( 2)), eligibles$           , ch(43),~
               at (17,02), fac(lfad$( 2)), writtens$            , ch(43),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,65), fac(hex(84)),   str(pf$(3),64,)      ,        ~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40285
                  call "MANUAL" ("GLARCHIV") : goto L40110

L40285:        if keyhit% <> 15 then L40300
                  call "PRNTSCRN" : goto L40110

L40300:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if end% = 1% and fieldnr% = 0% then return
        if edit% = 2% then L40395     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field   (10" &        ~
                     ")See Archive History   (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0affff0dff0f1000)
            if fieldnr% = 1% then L40375
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40375:     if fieldnr% >= 2% then L40385
                str(pf$(2),18,17) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40385:     return

L40395: if fieldnr% > 0% then L40440  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                     (10" &        ~
                     ")See Archive History   (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Archive     "
            pfkeys$ = hex(01ffffffffffffffff0affff0dff0f1000)
            return
L40440:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                     (10" &        ~
                     ")See Archive History   (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffff0affff0dff0fff00)
            return

L42000: REM *************************************************************~
            *            S A F E G U A R D   S C R E E N   1            *~
            * --------------------------------------------------------- *~
            * Safeguards the accidental purging of data by asking if he *~
            * really wants to purge it.                                 *~
            *************************************************************
            getback$ = "(1)Return"
            str(dateline$,,60) = "Enter Safeguard Word"

L42080:     accept                                                       ~
               at (01,02),                                               ~
                  "General Ledger Detail Archive Confirmation Screen",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (08,23), "**************************************",     ~
               at (09,23), "* ARCHIVING of data is about to      *",     ~
               at (10,23), "* begin. Do you wish to PROCEED?     *",     ~
               at (11,23), "*                                    *",     ~
               at (12,23), "*                                    *",     ~
               at (13,23), "*                                    *",     ~
               at (14,23), "*     Responses other than 'YES'     *",     ~
               at (15,23), "*    Will return you to Edit Mode    *",     ~
               at (16,23), "**************************************",     ~
                                                                         ~
               at (12,40), fac(hex(81)), safeguard$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
                                                                         ~
               at (22,02), fac(hex(84)), getback$               ,        ~
               at (22,65), "(13)Instructions"                   ,        ~
               at (23,65), "(15)Print Screen"                   ,        ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L42400
                  call "MANUAL" ("HNYARCHV")
                  goto L42080

L42400:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42080

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Archive Date Range     */~
                              L50400          /* Archive File Year      */
            return

L50100: REM Test for Archive Date Range         FMARCHDATE$
            fmarchdate%, toarchdate%, r% = 0%
            init(" ") unffmarchdate$, unftoarchdate$
            call "DATEOK" (fmarchdate$, fmarchdate%, errormsg$)
                 if errormsg$ <> " " then return
            if toarchdate$ = " " then toarchdate$ = fmarchdate$
            call "DATEOK" (toarchdate$, toarchdate%, errormsg$)
                 if errormsg$ <> " " then return
            unffmarchdate$ = fmarchdate$ : unftoarchdate$ = toarchdate$
            call "DATUNFMT" (unffmarchdate$)
            call "DATUNFMT" (unftoarchdate$)
            if (unffmarchdate$ >  unftoarchdate$)                        ~
                then errormsg$ = "From may not be greater then To."
            /* TOARCHDATE$ must be less than (Today - 90 Days) */
            call "DATE" addr("G+", date, -90%, dummydate$, r%)
            if unftoarchdate$ < dummydate$ then return  /* It's Ok */
               call "DATEFMT" (dummydate$)
               errormsg$ = "To Date Must Be Less Than or Equal to "      ~
                         & dummydate$
            return
L50400: REM Test for Archive File Year            ARCHYEAR$
            for i% = 1% to 4%
            if pos(" " = str(archyear$,i%,1%)) <> 0% then                ~
               errormsg$ = "Archive File Year Must Not Have Blanks."
            if errormsg$ <> " " then return
            next i%
            return

        no_file
            ask% = 0%
            call "ASKUSER" (ask%, "***** FILE NOT OPENED *****",         ~
                 "The required file was NOT OPENED!",                    ~
                 "Either it doesn't exist or it could not be opened "  & ~
                 "EXCLUSIVELY",                                          ~
                 "Press PF-16 to end this program")
            if ask% <> 16% then no_file
            goto exit_program

        create_arc_file
            call "SHOSTAT" ("Creating or Opening Archive Files")
            gldeprname$="GLDE" & archyear$
            call "PUTPRNAM" addr(#40, gldeprname$)
            call "OPENCHCK"(#40, fs%(40), f2%(40), 0%, rslt$(40))
                 if fs%(40) = 1% then close #40
            call "OPENCHCK"(#40, fs%(40), f2%(40), create%, rslt$(40))
            close #40
            call "OPENFILE" (#40, mode$, f2%(40), rslt$(40), axd$)
        return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
