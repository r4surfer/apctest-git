        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  TTTTT  X   X  TTTTT  FFFFF  L      M   M  N   N  TTTTT   *~
            *    T     X X     T    F      L      MM MM  NN  N    T     *~
            *    T      X      T    FFFF   L      M M M  N N N    T     *~
            *    T     X X     T    F      L      M   M  N  NN    T     *~
            *    T    X   X    T    F      LLLLL  M   M  N   N    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTFLMNT - Copies TXTFILE from TXTFILE to TXTNEW, Then    *~
            *            Renames TXTFILE --> TXTOLD,                    *~
            *            Renames TXTNEW  --> TXTFILE,                   *~
            *            Optionally Scratches TXTOLD when done.         *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/20/92 ! Original                                 ! KB2 *~
            * 05/25/94 ! Added support for Clipboard Records ('U')! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            filler$(8)256,               /* 2 K of Blank filler        */~
            header$64,                   /* 64 byte txt record header  */~
            newtext$(28)70,              /* Array to hold new text     */~
            oldtext$(28)70,              /* Array to hold old text     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            newfile$8,                   /* 'New' Text File            */~
            newlib$8,                    /* 'New' Text File            */~
            newvol$6,                    /* 'New' Text File            */~
            newlines$2,                  /* 'New' Text File            */~
            newrecs$8,                   /* 'New' Text File            */~
            oldfile$8,                   /* 'Old' Text File            */~
            oldlib$8,                    /* Current Text File          */~
            oldvol$6,                    /* Current Text File          */~
            oldlines$2,                  /* 'Old' Text File            */~
            oldrecs$8,                   /* 'Old' Text File            */~
            option$(4)1,                 /* Option Selection           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rs$2,                        /* Record Size for UFBs       */~
            srces$30,                    /* Sources for 'X'            */~
            stat%(10,5),                 /* Statitistics               */~
            statl%(100),                 /* Statitistics               */~
            stata(2),                    /* Statitistics               */~
            sfac$1,                      /* Statitistics FAC           */~
            types$100,                   /* Types for 'X'              */~
            userid$3                     /* Current User Id            */~

        dim f2%(04)                      /* = 0 if the file is open    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
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
            * #01 ! TXTFILE  ! System Text File                         *~
            * #02 ! TXTFILE  ! System Text File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select #02, "TXTNEW",                                        ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

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

            str(line2$,62) = "TXTFLMNT: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  4%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% then exit_program
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
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% >  4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
                  if fieldnr% = 3% then cursor%(1) = 8%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "PUTNAMES" addr(#1, "TXTFILE ", oldlib$, oldvol$)
            call "WORKOPN2" (#1, "INPUT", 0%, f2%(1))
               if f2%(1) <> 0% then inputmode
            mat stat% = zer : mat statl% = zer : mat stata = zer
            statistics% = 0% : if option$(1) <> " " then statistics% = 1%
            if statistics% = 0% then L19075
               gosub dataput
               close #1 : gosub reset_record_size
               sfac$ = hex(84) : gosub total_records
               gosub'101(0%, 1%)        /* Display Screen - No Entry   */
               if keyhit% <> 0% then exit_program
               sfac$ = hex(9c): goto editpg1

L19075:     if option$(4) <> " " then new_lib_volume

            newfile$ = "TXTNEW"

            call "READFDR" addr("TXTOLD  ", oldlib$, oldvol$, 0%, ret%)
               if ret% <> 0% then check_txtnew

L19115:     ask% = 0%
            call "ASKUSER" ( ask%, "* * * WARNING * * *",                ~
                          "File 'TXTOLD' exists in input library/volume",~
                          "Press PF3 to SCRATCH and CONTINUE",           ~
                          "Press PF16 to EXIT PROGRAM")
            if ask% = 16% then exit_program
            if ask% <> 3% then L19115

            call "SCRATCH" addr("F", "TXTOLD  ", oldlib$, oldvol$,       ~
                                "B", " ", ret%)
               if ret% = 0% then check_txtnew

            ask% = 0%
            call "ASKUSER" ( ask%, "* * * SORRY * * *",                  ~
                          "Scratch of 'TXTOLD' Unsuccessful",            ~
                          "Check INPUT Volume and Library",              ~
                          "Press PF16 to EXIT PROGRAM")
            goto exit_program

        check_txtnew
            call "READFDR" addr("TXTNEW  ", newlib$, newvol$, 0%, ret%)
               if ret% <> 0% then do_the_work

L19270:     ask% = 0%
            call "ASKUSER" ( ask%, "* * * WARNING * * *",                ~
                         "File 'TXTNEW' exists in input library/volume", ~
                         "Press PF3 to SCRATCH and CONTINUE",            ~
                         "Press PF16 to EXIT PROGRAM")
            if ask% = 16% then exit_program
            if ask% <> 3% then L19270

            call "SCRATCH" addr("F", "TXTNEW  ", newlib$, newvol$,       ~
                                "B", " ", ret%)
            if ret% = 0% then do_the_work

            ask% = 0%
            call "ASKUSER" ( ask%, "* * * SORRY * * *",                  ~
                          "Scratch of 'TXTNEW' Unsuccessful",            ~
                          "Check INPUT Volume and Library",              ~
                          "Press PF16 to EXIT PROGRAM")
            goto exit_program

        new_lib_volume
            newfile$ = "TXTFILE"

            call "READFDR" addr("TXTFILE ", newlib$, newvol$, 0%, ret%)
               if ret% <> 0% then do_the_work
L19425:     ask% = 0%
            call "ASKUSER" ( ask%, "* * * WARNING * * *",                ~
                        "File 'TXTFILE' exists in output library/volume",~
                        "Press PF3 to SCRATCH and CONTINUE",             ~
                        "Press PF16 to EXIT PROGRAM")
            if ask% = 16% then exit_program
            if ask% <> 3% then L19425

            call "SCRATCH" addr("F", "TXTFILE ", newlib$, newvol$,       ~
                                "B", " ", ret%)
            if ret% = 0% then do_the_work

            ask% = 0%
            call "ASKUSER" ( ask%, "* * * SORRY * * *",                  ~
                          "Scratch of 'TXTFILE' Unsuccessful",           ~
                          "Check OUTPUT Volume and Library",             ~
                          "Press PF16 to EXIT PROGRAM")
            goto exit_program

        do_the_work

            newrsize% = 64% + newsize%
            str(rs$,,2) = bin(newrsize%, 2)
            call "PUTUFBRS" addr(#2, rs$)
            call "PUTNAMES" addr(#2, newfile$, newlib$, newvol$)
            call "WORKOPN2" (#2, "OUTSP", newrecs%, f2%(2))

            gosub dataput

            if option$(4) = " " then L19670
               close #1
               close #2

L19670:     sfac$ = hex(84) : gosub total_records
            gosub'101(0%, 1%)           /* Display Screen - No Entry   */

            if option$(4) <> " " then finish_up

            close #1
            close #2

            call "RENAME" addr("F", "TXTFILE ", oldlib$, oldvol$,        ~
                               "TXTOLD  ", "B", "L", " ",  ret%)
            if ret% = 0% then L19855

            ask% = 0%
            call "ASKUSER" ( ask%, "* * * WARNING * * *",                ~
                          "Rename of 'TXTFILE' to 'TXTOLD' Unsuccessful",~
                          "New file is still 'TXTNEW'",                  ~
                          "Press PF16 to EXIT PROGRAM")
            goto exit_program

L19855:     call "RENAME" addr("F", "TXTNEW  ", newlib$, newvol$,        ~
                               "TXTFILE ", "B", "L", " ",  ret%)
            if ret% = 0% then L19910

            ask% = 0%
            call "ASKUSER" ( ask%, "* * * SEVERE WARNING * * *",         ~
                          "Rename of 'TXTNEW' to 'TXTFILE' Unsuccessful",~
                          "There is currently no valid 'TXTFILE'",       ~
                          "Press PF16 to EXIT PROGRAM")
            goto exit_program

L19910:     if option$(3) <> " " then finish_up

            call "SCRATCH" addr("F", "TXTOLD  ", oldlib$, oldvol$,       ~
                                "B", " ", ret%)
               if ret% = 0% then finish_up

            ask% = 0%
            call "ASKUSER" ( ask%, "* * * WARNING * * *",                ~
                          "Scratch of 'TXTOLD' Unsuccessful",            ~
                          "Final Step of Processing",                    ~
                          "Press PF16 to EXIT PROGRAM")
            goto exit_program

        finish_up
            if keyhit% = 0% then inputmode
               goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* 'Old' Text File        */~
                              L20200,         /* 'New' Text File        */~
                              L20300,         /* Options                */~
                              L20400          /* 'New' Text File        */
            return

L20100: REM Def/Enable 'Old' Text File             OLDLIB$
            if oldlib$ <> " " then return
            call "OPENCHCK" (#01, fs%, 0%, 0%, " ")
            if fs% < 0% then return
            call "GETNAMES" addr(#1, oldfile$, oldlib$, oldvol$)
            close #1
            gosub reset_record_size
            gosub'151(1%)
            return

L20200: REM Def/Enable 'New' Text File             NEWLIB$
            if newlines$ <> " " then return
               newlines$ = oldlines$
               newlines% = oldlines%
               newrecs$  = oldrecs$
               newrecs%  = oldrecs%
            return

L20300: REM Def/Enable Options                     OPTION$
            return

L20400: REM Def/Enable 'New' Text File             NEWLIB$
            on pos(str(option$()) <> " ") goto L20450,    /* Statistics */~
                                               L20500,    /* In Place   */~
                                               L20500,    /* In Place   */~
                                               L20550     /* New Lib.   */
               return

L20450
*       ** Statisics Only ***
            newlib$, newvol$ = " "
            enabled% = 0%
            return

L20500
*       ** In Place
            newlib$ = oldlib$
            newvol$ = oldvol$
            enabled% = 0%
            return

L20550
*       ** New Target
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                if edit% <> 1% then return
                inpmessage$ =                                            ~
                  "Press ENTER to Continue, PF1 for Lines / Id Breakdown"
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter 'Old' Text File information                            ",~
         "Enter 'New' Text File Output Parameters                      ",~
         "Select Processing Option.                                    ",~
         "Enter 'New' Text File Library and Volume Information.        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, newfile$, newlib$, newvol$,~
                      newlines$, newrecs$, oldlines$, oldrecs$, option$()
            newlines% = 0% :newrecs% = 0% :oldlines% = 0% :oldrecs% = 0%
            mat stat% = zer
            sfac$ = hex(9c)
        reset_record_size
            temp% = 2024%
            str(rs$,,2) = bin(temp%, 2)
            call "PUTUFBRS" addr(#1, rs$)

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
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
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
*       DATALOAD
        total_records:

            newrecs% = 0%

            for i% = 1% to 7%
                newrecs% = newrecs% + stat%(i%, 1%)
            next i%

            newrecs% = newrecs% + stat%(10%, 1%)   /* Clip Board */

            for i% = 1% to 2%
                if stat%(i%,5%) = 0% then L30180
                   stata(i%) = stat%(i%,3%)
                   stata(i%) = stata(i%)/stat%(i%,5%)
L30180:     next i%

            convert newrecs% to newrecs$, pic(########)
            oldbytes% = oldrecs% * (oldsize% + 64%)
            newbytes% = newrecs% * (newsize% + 64%)
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            if statistics% = 0% then                                     ~
                call "SHOSTAT" ("Converting TXTFILE") else               ~
                call "SHOSTAT" ("Analyzing TXTFILE")

        read_next:
            call "READNEXT" (#1, eod%)
        process_next:
               if eod% = 0% then return

            plowkey$ = key(#1)
            on pos("BCMNSTXWU" = str(plowkey$,1,1))                      ~
                  goto    L32000,  /* 'B' - Toss Text Records           */~
                          L31300,  /* 'C' - Toss Header & Fill          */~
                          L32000,  /* 'M' - Toss Text Records           */~
                          L31300,  /* 'N' - Toss Header & Fill          */~
                          L31300,  /* 'S' - Toss Header & Fill          */~
                          L31300,  /* 'T' - Toss Header & Fill          */~
                          L31400,  /* 'X' - Toss Report Control         */~
                          L31260,  /* 'W' - Don't Toss                  */~
                          L32000   /* 'U' - Toss Clip Board Records     */
            stat%(9%,1%) = stat%(9%,1%) - 1%
            goto read_next        /* '?' - Should Not Be Here!         */

L31260
*       ** Work Area, no need to toss
            stat%(8%,1%) = stat%(8%,1%) - 1%
            goto read_next

L31300
*       ** Just need to do 64 byte header area
            get #1 using L31320, str(header$,,64)
L31320:         FMT CH(64)
            if statistics% <> 0% then L31350
            write #2, str(header$,,64), str(filler$(),,newsize%)

L31350:     stat% = 2% + pos("CNST" = str(header$,,1))
            stat%(stat%, 1%) = stat%(stat%, 1%) + 1%
            if stat% <> 4% then read_next
            stat%(4%,3%) = max(stat%(4%,3%), stat%(4%,2%))
            stat%(4%,2%) = val(str(header$,12,4), 4)
            goto read_next

L31400
*       ** Report, Need to be a little careful
            init (" ") header$, srces$, types$ : got_next% = 0%
            get #1 using L31430, str(header$,,41)
L31430:         FMT CH(41)
            if str(header$,10,1) = hex(00) then L31500
               /* Old Format */
            get #1 using L31470, str(srces$,,30), str(types$,,100)
L31470:         FMT POS(42), CH(30), CH(100)
            goto write_x

L31500:     if oldsize% < 130% then L31550
            get #1 using L31520, str(srces$,,30), str(types$,,100)
L31520:         FMT POS(65), CH(30), CH(100)
            goto write_x

L31550:     get #1 using L31560, str(srces$,,15), str(types$,,50)
L31560:         FMT POS(65), CH(15), CH(50)
            call "READNEXT" (#1, eod%)
               if eod% <> 0% then L31610
L31590:           got_next% = 1%
                  goto write_x
L31610:     plowkey$ = key(#1)
            if str(plowkey$,1,7) <> str(header$,1,7) then L31590
            get #1 using L31560, str(srces$,16,15), str(types$,51,50)

        write_x:
            str(header$,8,4) = hex(20200001)
            if newsize% < 130% then L31720
            if statistics% <> 0% then L31790
            write #2, str(header$,,64), str(srces$,,30),                 ~
                      str(types$,,100), str(filler$(),, newsize% - 130%)
            goto L31790

L31720:     if statistics% <> 0% then L31770
            write #2, str(header$,,64), str(srces$,,15),                 ~
                      str(types$,,50), str(filler$(),, newsize% - 65%)
            str(header$,8,4) = hex(20200002)
            write #2, str(header$,,64), str(srces$,16,15),               ~
                      str(types$,51,50), str(filler$(),, newsize% - 65%)
L31770:     stat%(7%,1%) = stat%(7%,1%) + 1%

L31790:     stat%(7%,1%) = stat%(7%,1%) + 1%
            stat%(7%,2%) = stat%(7%,2%) + 1%
            if got_next% <> 0% then process_next else read_next

L32000
*       ** Toss Text, a little harder
            init (" ") newtext$(), header$
            newcount% = 0% : newseq% = 0% : delete% = 0%

            get #1 using L32060, header$
L32060:         FMT CH(64)
            stat% = 1% : if str(header$,,1) = "M" then stat% = 2%
                         if str(header$,,1%) = "U" then stat% = 10%
            linecount% = val(str(header$,15,2), 2)
            stat%(stat%,3%) = stat%(stat%,3%) + linecount%
            stat%(stat%,5%) = stat%(stat%,5%) + 1%
            if linecount% > 0% then L32087
               newcount% = 1% : delete% = 1%
               goto bypass_rest
L32087:     idx% = min(linecount%, 100%)
            if stat% = 2% then statl%(idx%) = statl%(idx%) + 1%

L32090:     get #1, str(plowkey$,,64), str(oldtext$(),,oldsize%)
            if str(plowkey$,,9) <> str(header$,,9) then L32240
            how_many% = min(oldlines%, linecount%)
            if how_many% < 1% then bypass_rest      /* Nothing to do */

            for i% = 1% to how_many%
                newcount% = newcount% + 1%
                newtext$(newcount%) = oldtext$(i%)
                if newcount% >= newlines% then gosub write_new_text
            next i%
            linecount% = linecount% - how_many%

            call "READNEXT" (#1, eod%)
               if eod% <> 0% then L32090
L32240:           if newcount% > 0%  then gosub write_new_text
                  goto process_next

        bypass_rest
            if newcount% > 0% then gosub write_new_text
            call "READNEXT" (#1, eod%)
               if eod% = 0% then process_next
            plowkey$ = key(#1)
            if str(plowkey$,,9) <> str(header$,,9) then process_next
            goto bypass_rest

        write_new_text
            newseq% = newseq% + 1%
            str(header$,10,2) = bin(newseq%, 2)
            if statistics% <> 0% then L32400
            write #2, str(header$,,64), str(newtext$(),,newsize%)
L32400:     newcount% = 0%
            init (" ") newtext$()
            stat%(stat%,1%) = stat%(stat%,1%) + 1%
            stat%(stat%,2%) = stat%(stat%,2%) + newlines%
            stat%(stat%,4%) = stat%(stat%,4%) + delete%
            delete% = 0%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40085,         /* 'Old' Text File   */   ~
                                L40085,         /* 'New' Text File   */   ~
                                L40085,         /* Options           */   ~
                                L40085          /* 'New' Text File   */
              goto L40100

L40085:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */

L40100:     accept                                                       ~
               at (01,02),                                               ~
                  "Text File Reorganization Utility",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "'Old' Text - Library",                       ~
               at (04,23), fac(lfac$( 1)), oldlib$              , ch(08),~
               at (04,32), "Volume",                                     ~
               at (04,39), fac(lfac$( 1)), oldvol$              , ch(06),~
               at (04,47), fac(hex(84))  , oldlines$            , ch(02),~
               at (04,50), "Lines/Record",                               ~
               at (04,63), fac(hex(84))  , oldrecs$             , ch(08),~
               at (04,72), "Records",                                    ~
                                                                         ~
               at (05,02), "'New' Text - Output Definitions",            ~
               at (05,47), fac(lfac$( 2)), newlines$            , ch(02),~
               at (05,50), "Lines/Record",                               ~
               at (05,63), fac(lfac$( 2)), newrecs$             , ch(08),~
               at (05,72), "Records",                                    ~
                                                                         ~
               at (06,05), fac(lfac$( 3)), option$(1)           , ch(01),~
               at (06,07), "Statistics",                                 ~
                           /* ch(10) */                                  ~
               at (06,20), fac(lfac$( 3)), option$(2)           , ch(01),~
               at (06,22), "In Place",                                   ~
                           /* ch(08) */                                  ~
               at (06,33), fac(lfac$( 3)), option$(3)           , ch(01),~
               at (06,35), "In Place (Save 'Old')",                      ~
                           /* ch(21) */                                  ~
               at (06,59), fac(lfac$( 3)), option$(4)           , ch(01),~
               at (06,61), "New Library/Volume",                         ~
                           /* ch(18) */                                  ~
                                                                         ~
               at (07,36), "'New' Text - Library",                       ~
               at (07,57), fac(lfac$( 4)), newlib$              , ch(08),~
               at (07,66), "On Volume",                                  ~
               at (07,73), fac(lfac$( 4)), newvol$              , ch(06),~
                                                                         ~
               at (09,09),                                               ~
        "   Records  Line Total  Lines Used     Deleted    Text Ids    Li~
        ~nes/Id",                                                         ~
               at (10,02), "Buffer",                                     ~
               at (11,02), "Master",                                     ~
               at (12,02), "Copy  ",                                     ~
               at (13,02), "ClipBd",                                     ~
               at (14,02), "Next  ",                                     ~
               at (15,02), "Source",                                     ~
               at (16,02), "Type  ",                                     ~
               at (17,02), "X(rpt)",                                     ~
               at (18,02), "Work  ",                                     ~
               at (19,02), "?     ",                                     ~
                                                                         ~
               at (10,09), fac(sfac$), stat%(1%,1%),     pic(-#########),~
               at (10,21), fac(sfac$), stat%(1%,2%),     pic(-#########),~
               at (10,33), fac(sfac$), stat%(1%,3%),     pic(-#########),~
               at (10,45), fac(sfac$), stat%(1%,4%),     pic(-#########),~
               at (10,57), fac(sfac$), stat%(1%,5%),     pic(-#########),~
               at (10,69), fac(sfac$), stata(1%),        pic(-####.####),~
                                                                         ~
               at (11,09), fac(sfac$), stat%(2%,1%),     pic(-#########),~
               at (11,21), fac(sfac$), stat%(2%,2%),     pic(-#########),~
               at (11,33), fac(sfac$), stat%(2%,3%),     pic(-#########),~
               at (11,45), fac(sfac$), stat%(2%,4%),     pic(-#########),~
               at (11,57), fac(sfac$), stat%(2%,5%),     pic(-#########),~
               at (11,69), fac(sfac$), stata(2%),        pic(-####.####),~
                                                                         ~
               at (12,09), fac(sfac$), stat%(3%,1%),     pic(-#########),~
               at (13,09), fac(sfac$), stat%(10%,1%),    pic(-#########),~
               at (14,09), fac(sfac$), stat%(4%,1%),     pic(-#########),~
               at (15,09), fac(sfac$), stat%(5%,1%),     pic(-#########),~
               at (16,09), fac(sfac$), stat%(6%,1%),     pic(-#########),~
               at (17,09), fac(sfac$), stat%(7%,1%),     pic(-#########),~
               at (18,09), fac(sfac$), stat%(8%,1%),     pic(-#########),~
               at (19,09), fac(sfac$), stat%(9%,1%),     pic(-#########),~
                                                                         ~
               at (17,21), fac(sfac$), stat%(7%,2%),     pic(-#########),~
               at (17,33), "Reports Defined",                            ~
                                                                         ~
               at (14,21), fac(sfac$), stat%(4%,2%),     pic(-#########),~
               at (14,33), "Next Text Id",                               ~
                                                                         ~
               at (20,24), "Old Bytes:",                                 ~
               at (20,54), "New Bytes:",                                 ~
                                                                         ~
               at (20,39), fac(sfac$), oldbytes%,        pic(-#########),~
               at (20,69), fac(sfac$), newbytes%,        pic(-#########),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if edit% > 1% then L41000
               if fieldnr% > 0% then L41000
                  if keyhit% <> 1% then L41000
                     gosub L45000 : goto L40100

L41000:        if keyhit% <> 13 then L41030
                  call "MANUAL" ("TXTFLMNT") : goto L40100

L41030:        if keyhit% <> 15 then L41060
                  call "PRNTSCRN" : goto L40100

L41060:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41250     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% > 0% then L41210
                str(pf$(1),,16)   = " "  /* STR(PFKEYS$, 1,1) = HEX(FF) */
L41210:     if fieldnr% > 1% then L41230
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41230:     return

L41250: if fieldnr% > 0% then L41340  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Process     "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41340:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

L45000: REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

L45030:     accept                                                       ~
               at (01,02),                                               ~
               "Text File Reorganization Utility - Lines/Id's Breakdown",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "   1", fac(hex(84)), statl%( 1%), pic(#####),~
               at (05,02), "   2", fac(hex(84)), statl%( 2%), pic(#####),~
               at (06,02), "   3", fac(hex(84)), statl%( 3%), pic(#####),~
               at (07,02), "   4", fac(hex(84)), statl%( 4%), pic(#####),~
               at (08,02), "   5", fac(hex(84)), statl%( 5%), pic(#####),~
               at (09,02), "   6", fac(hex(84)), statl%( 6%), pic(#####),~
               at (10,02), "   7", fac(hex(84)), statl%( 7%), pic(#####),~
               at (11,02), "   8", fac(hex(84)), statl%( 8%), pic(#####),~
               at (12,02), "   9", fac(hex(84)), statl%( 9%), pic(#####),~
               at (13,02), "  10", fac(hex(84)), statl%(10%), pic(#####),~
               at (14,02), "  11", fac(hex(84)), statl%(11%), pic(#####),~
               at (15,02), "  12", fac(hex(84)), statl%(12%), pic(#####),~
               at (16,02), "  13", fac(hex(84)), statl%(13%), pic(#####),~
               at (17,02), "  14", fac(hex(84)), statl%(14%), pic(#####),~
               at (18,02), "  15", fac(hex(84)), statl%(15%), pic(#####),~
               at (19,02), "  16", fac(hex(84)), statl%(16%), pic(#####),~
               at (20,02), "  17", fac(hex(84)), statl%(17%), pic(#####),~
               at (21,02), "  18", fac(hex(84)), statl%(18%), pic(#####),~
               at (22,02), "  19", fac(hex(84)), statl%(19%), pic(#####),~
               at (23,02), "  20", fac(hex(84)), statl%(20%), pic(#####),~
                                                                         ~
               at (04,17), "  21", fac(hex(84)), statl%(21%), pic(#####),~
               at (05,17), "  22", fac(hex(84)), statl%(22%), pic(#####),~
               at (06,17), "  23", fac(hex(84)), statl%(23%), pic(#####),~
               at (07,17), "  24", fac(hex(84)), statl%(24%), pic(#####),~
               at (08,17), "  25", fac(hex(84)), statl%(25%), pic(#####),~
               at (09,17), "  26", fac(hex(84)), statl%(26%), pic(#####),~
               at (10,17), "  27", fac(hex(84)), statl%(27%), pic(#####),~
               at (11,17), "  28", fac(hex(84)), statl%(28%), pic(#####),~
               at (12,17), "  29", fac(hex(84)), statl%(29%), pic(#####),~
               at (13,17), "  30", fac(hex(84)), statl%(30%), pic(#####),~
               at (14,17), "  31", fac(hex(84)), statl%(31%), pic(#####),~
               at (15,17), "  32", fac(hex(84)), statl%(32%), pic(#####),~
               at (16,17), "  33", fac(hex(84)), statl%(33%), pic(#####),~
               at (17,17), "  34", fac(hex(84)), statl%(34%), pic(#####),~
               at (18,17), "  35", fac(hex(84)), statl%(35%), pic(#####),~
               at (19,17), "  36", fac(hex(84)), statl%(36%), pic(#####),~
               at (20,17), "  37", fac(hex(84)), statl%(37%), pic(#####),~
               at (21,17), "  38", fac(hex(84)), statl%(38%), pic(#####),~
               at (22,17), "  39", fac(hex(84)), statl%(39%), pic(#####),~
               at (23,17), "  40", fac(hex(84)), statl%(40%), pic(#####),~
                                                                         ~
               at (04,33), "  41", fac(hex(84)), statl%(41%), pic(#####),~
               at (05,33), "  42", fac(hex(84)), statl%(42%), pic(#####),~
               at (06,33), "  43", fac(hex(84)), statl%(43%), pic(#####),~
               at (07,33), "  44", fac(hex(84)), statl%(44%), pic(#####),~
               at (08,33), "  45", fac(hex(84)), statl%(45%), pic(#####),~
               at (09,33), "  46", fac(hex(84)), statl%(46%), pic(#####),~
               at (10,33), "  47", fac(hex(84)), statl%(47%), pic(#####),~
               at (11,33), "  48", fac(hex(84)), statl%(48%), pic(#####),~
               at (12,33), "  49", fac(hex(84)), statl%(49%), pic(#####),~
               at (13,33), "  50", fac(hex(84)), statl%(50%), pic(#####),~
               at (14,33), "  51", fac(hex(84)), statl%(51%), pic(#####),~
               at (15,33), "  52", fac(hex(84)), statl%(52%), pic(#####),~
               at (16,33), "  53", fac(hex(84)), statl%(53%), pic(#####),~
               at (17,33), "  54", fac(hex(84)), statl%(54%), pic(#####),~
               at (18,33), "  55", fac(hex(84)), statl%(55%), pic(#####),~
               at (19,33), "  56", fac(hex(84)), statl%(56%), pic(#####),~
               at (20,33), "  57", fac(hex(84)), statl%(57%), pic(#####),~
               at (21,33), "  58", fac(hex(84)), statl%(58%), pic(#####),~
               at (22,33), "  59", fac(hex(84)), statl%(59%), pic(#####),~
               at (23,33), "  60", fac(hex(84)), statl%(60%), pic(#####),~
                                                                         ~
               at (04,49), "  61", fac(hex(84)), statl%(61%), pic(#####),~
               at (05,49), "  62", fac(hex(84)), statl%(62%), pic(#####),~
               at (06,49), "  63", fac(hex(84)), statl%(63%), pic(#####),~
               at (07,49), "  64", fac(hex(84)), statl%(64%), pic(#####),~
               at (08,49), "  65", fac(hex(84)), statl%(65%), pic(#####),~
               at (09,49), "  66", fac(hex(84)), statl%(66%), pic(#####),~
               at (10,49), "  67", fac(hex(84)), statl%(67%), pic(#####),~
               at (11,49), "  68", fac(hex(84)), statl%(68%), pic(#####),~
               at (12,49), "  69", fac(hex(84)), statl%(69%), pic(#####),~
               at (13,49), "  70", fac(hex(84)), statl%(70%), pic(#####),~
               at (14,49), "  71", fac(hex(84)), statl%(71%), pic(#####),~
               at (15,49), "  72", fac(hex(84)), statl%(72%), pic(#####),~
               at (16,49), "  73", fac(hex(84)), statl%(73%), pic(#####),~
               at (17,49), "  74", fac(hex(84)), statl%(74%), pic(#####),~
               at (18,49), "  75", fac(hex(84)), statl%(75%), pic(#####),~
               at (19,49), "  76", fac(hex(84)), statl%(76%), pic(#####),~
               at (20,49), "  77", fac(hex(84)), statl%(77%), pic(#####),~
               at (21,49), "  78", fac(hex(84)), statl%(78%), pic(#####),~
               at (22,49), "  79", fac(hex(84)), statl%(79%), pic(#####),~
               at (23,49), "  80", fac(hex(84)), statl%(80%), pic(#####),~
                                                                         ~
               at (04,65), "  81", fac(hex(84)), statl%(81%), pic(#####),~
               at (05,65), "  82", fac(hex(84)), statl%(82%), pic(#####),~
               at (06,65), "  83", fac(hex(84)), statl%(83%), pic(#####),~
               at (07,65), "  84", fac(hex(84)), statl%(84%), pic(#####),~
               at (08,65), "  85", fac(hex(84)), statl%(85%), pic(#####),~
               at (09,65), "  86", fac(hex(84)), statl%(86%), pic(#####),~
               at (10,65), "  87", fac(hex(84)), statl%(87%), pic(#####),~
               at (11,65), "  88", fac(hex(84)), statl%(88%), pic(#####),~
               at (12,65), "  89", fac(hex(84)), statl%(89%), pic(#####),~
               at (13,65), "  90", fac(hex(84)), statl%(90%), pic(#####),~
               at (14,65), "  91", fac(hex(84)), statl%(91%), pic(#####),~
               at (15,65), "  92", fac(hex(84)), statl%(92%), pic(#####),~
               at (16,65), "  93", fac(hex(84)), statl%(93%), pic(#####),~
               at (17,65), "  94", fac(hex(84)), statl%(94%), pic(#####),~
               at (18,65), "  95", fac(hex(84)), statl%(95%), pic(#####),~
               at (19,65), "  96", fac(hex(84)), statl%(96%), pic(#####),~
               at (20,65), "  97", fac(hex(84)), statl%(97%), pic(#####),~
               at (21,65), "  98", fac(hex(84)), statl%(98%), pic(#####),~
               at (22,65), "  99", fac(hex(84)), statl%(99%), pic(#####),~
               at (23,65), "100+", fac(hex(84)), statl%(100%),pic(#####),~
                                                                         ~
               at (24,21), "* * * Press Any PF Key to Return * * *",     ~
               keys(hex(000102030405060708090a0b0c0d0e0f10)),            ~
               key(keyhit%)

               if keyhit% <> 13 then L45625
                  call "MANUAL" ("TXTFLMNT") : goto L45030

L45625:        if keyhit% <> 15 then return
                  call "PRNTSCRN" : goto L45030

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* 'Old' Text File        */~
                              L50200,         /* 'New' Text File        */~
                              L50400,         /* Options                */~
                              L50500          /* 'New' Text File        */
            return

L50100: REM Test for 'Old' Text File              OLDLIB$
            call "READFDR" addr("TXTFILE ", oldlib$, oldvol$, 0%,        ~
                                "RC", oldrecs%, "RS", oldsize%, ret%)
            if ret% <> 0% then L50155
            oldsize% = oldsize% - 64%
            oldlines% = oldsize% / 70%
            convert oldlines% to oldlines$, pic(##)
            convert oldrecs% to oldrecs$, pic(########)
            return

L50155:     errormsg$ = "'TXTFILE' not found in specified libray/volume"
            return

L50200: REM Test for 'New' Text File              NEWLIB$

            convert newlines$ to newlines%, data goto L50280
            if newlines% < 1 or newlines% > 28%  then L50280
            convert newlines% to newlines$, pic(##)
            newsize% = 70% * newlines%
            goto L50310

L50280:     errormsg$ = "New Lines Must be 1 - 28"
            return

L50310:     newrecs% = 0% : convert newrecs$ to newrecs%, data goto L50360
            newrecs% = max(newrecs%, 100%)
            convert newrecs% to newrecs$, pic(########)
            return

L50360:     errormsg$ = "Invalid Entry for Number of Records"
            return

L50400: REM Test for Options                      OPTION$
L50410:     p% = pos(str(option$()) <> " ")
            if p% <> 0% then L50490
            if cursor%(1%) <> 7% then L50484
               if cursor%(2) < 59% then L50460
                  option$(4) = "X" : goto L50410
L50460:        if cursor%(2) < 33% then L50470
                  option$(3) = "X" : goto L50410
L50470:        if cursor%(2) < 20% then L50480
                  option$(2) = "X" : goto L50410
L50480:           option$(1) = "X" : goto L50410

L50484:        errormsg$ = "Select Option for Processing"
               return

L50490:     str(option$(), p% , 1%) = "X"
            if p% < 4% then str(option$(), p% + 1%) = " "
            return

L50500: REM Test for 'New' Text File              NEWLIB$
            if option$(4) = " " then return
            if newlib$ <> oldlib$ then L50560
            if newvol$ <> oldvol$ then L50560
               errormsg$ = "'New' Library/Volume Cannot be same as 'Old'"
               return

L50560:     if newlib$ <> " " then L50600
               errormsg$ = "Output Library Cannot Be Blank"
               return

L50600:     if newvol$ = oldvol$ then return
            call "READVTOC" addr("G", newvol$, return%)
               if return% = 0% then return
            errormsg$ = "SORRY, " & newvol$ &                            ~
                           " is not a currently mounted Volume."
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
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
