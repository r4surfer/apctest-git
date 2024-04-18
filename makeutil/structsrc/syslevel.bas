        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  FFFFF  IIIII  L      EEEEE  EEEEE  X   X  TTTTT  DDDD    *~
            *  F        I    L      E      E       X X     T    D   D   *~
            *  FFFF     I    L      EEEE   EEEE     X      T    D   D   *~
            *  F        I    L      E      E       X X     T    D   D   *~
            *  F      IIIII  LLLLL  EEEEE  EEEEE  X   X    T    DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FILEEXTD -                                                *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/02/94 ! Original                                 ! KAB *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            key$1,                       /* Load by Key (0 - 2)        */~
            key3$26,                     /* Load by Key (0 - 2)        */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pg$30,                                                       ~
            plowkey$99, plowkey1$99,     /* Miscellaneous Read/Plow Key*/~
	    readkey$99, readkey1$99,     /* Miscellaneous Read/Plow Key*/~
            userid$3                     /* Current User Id            */~

        dim type$(2000)1,                /* Type                       */~
            level$(2000)3,               /* Level                      */~
            unit$(2000)8,                /* Unit                       */~
            lib$(2000)6,                 /* SES Library                */~
            ver$(2000)6,                 /* SES Version                */~
            obj$(2000)8,                                                 ~
            src$(2000)36

       dim  sr$(800)8,                                                   ~
            cd$(100)8,                                                   ~
            cm$(100)8,                                                   ~
            ac$(100)8,                                                   ~
            mc$(100)8,                                                   ~
            wa$(100)8,                                                   ~
            us$(100)8,                                                   ~
                                                                         ~
            ssr$(800)1,                                                  ~
            scd$(100)1,                                                  ~
            scm$(100)1,                                                  ~
            sac$(100)1,                                                  ~
            smc$(100)1,                                                  ~
            swa$(100)1,                                                  ~
            sus$(100)1,                                                  ~
                                                                         ~
            e2$(400)8,                                                   ~
            e3$(200)8,                                                   ~
            r2$(100)8,                                                   ~
            r3$(100)8,                                                   ~
                                                                         ~
            se2$(400)1,                                                  ~
            se3$(200)1,                                                  ~
            sr2$(100)1,                                                  ~
            sr3$(100)1,                                                  ~
                                                                         ~
            dpath$100, sendpath$200, exts$(5)8, pathc$100, pathc2$100,   ~
			slash$1, tilde$1, up$60, tolow$60, toup$60

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 12/31/99 Pre-Release Version            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! INTDOCSL ! SES Extract file                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "ANALYSIS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  28,                     ~
                        alt key  1, keypos =  2, keylen =   31,          ~
                            key  2, keypos =  1, keylen =   32,          ~
                            key  3, keypos = 33, keylen =   26, dup

            select #02, "STRUCT",                                        ~
                        varc,      indexed,  recsize =  50, ~
           		keypos =    5, keylen =  21, ~
			alt key  1, keypos =  2, keylen =   24,          ~
                            key  2, keypos =  1, keylen =   25

	    select #03, "OUTPUT",                                        ~
                        varc,      consec,  recsize =  20

            call "GETPRNAM" addr(#1, file$)
            call "EXTRACT" addr("IL", lib$, "IV", vol$)
            call "PUTNAMES" addr(#1, file$, lib$, vol$)

	Ask1:
	f1% = 0%
	call "ASKUSER" (f1%, "Select", "F1 - Rebuild", " ", "Enter to Continue")
	if f1% = 1% then L2xx
	if f1% <> 0% then Ask1
	open #1, shared
	goto L2xxx
		
	L2xx:
            open #1, output : close #1 : open nogetparm #1, shared
	    open #2, shared
	    gosub xdataput

	L2xxx:

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

            str(line2$,62) = "SYSLEVEL: " & str(cms2v$,,8)
            
            tilde$      = hex(7E)
            up$ = "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz"
			
            tolow$ =                                                     ~
                   "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"
            toup$ =                                                      ~
                   "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz"
            toup$ = toup$ & hex(2000)

	    slash$ = "/"
            str(dpath$) = hex(80) & slash$ & "work" & slash$ & "r70000" & slash$

	kh% = 0%
        call "ASKSTRNG" (kh%, "Source Path", ~
	 	"Enter path to source directories", ~
		" ", "path", dpath$, 80%)
	if kh% <> 0% then end

	call "SHOSTAT" ("Preparing Data, One Moment Please")
	if str(dpath$,1%,1%) = hex(80) then str(sendpath$) = str(dpath$,2%)
	if str(dpath$,1%,1%) = hex(80) then dpath$ = sendpath$

            sendpath$ = dpath$ & "source2"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, sr$(), sr%, 2%, exts$())

            sendpath$ = dpath$ & "sourcecd"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, cd$(), cd%, 2%, exts$())

            sendpath$ = dpath$ & "sourcecm"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, cm$(), cm%, 2%, exts$())

            sendpath$ = dpath$ & "sourceac"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, ac$(), ac%, 2%, exts$())

            sendpath$ = dpath$ & "sourcemc"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, mc$(), mc%, 2%, exts$())

            sendpath$ = dpath$ & "sourcewa"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, wa$(), wa%, 2%, exts$())

            sendpath$ = dpath$ & "subsrce2"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, e2$(), e2%, 2%, exts$())

            sendpath$ = dpath$ & "subsrce3"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, e3$(), e3%, 2%, exts$())

            sendpath$ = dpath$ & "subsrcr2"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, r2$(), r2%, 2%, exts$())

            sendpath$ = dpath$ & "subsrcr3"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, r3$(), r3%, 2%, exts$())

            sendpath$ = dpath$ & "utilsrce"
            exts$(1%) = ".bas"
            exts$(2%) = ".c.sav"
            exts$(3%) = " "
            exts$(4%) = " "
            exts$(5%) = " "
            call "GETFLIST" (sendpath$, us$(), us%, 2%, exts$())

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  1%
L11110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L11230
L11130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L11210
L11160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L11130
                         if fieldnr% = 1% then L11110
                         goto L11160
L11210:               if keyhit% = 16% then exit_program
                      if keyhit% = 14% then       L11230
                      if keyhit% <> 0% then       L11130
L11230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11130
            next fieldnr%

            gosub dataload
            if maxlines% = 0% then inputmode
            u% = 0%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1

            gosub'102                   /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then u% = 0%
                  if keyhit%  =  3% then u% = maxlines% - 16%
                  if keyhit%  =  4% then u% = u% - 16%
                  if keyhit%  =  5% then u% = u% + 16%
                  if keyhit%  =  6% then u% = u% - 1%
                  if keyhit%  =  7% then u% = u% + 1%
                  if keyhit%  = 18% then u% = u% / 2%
                  if keyhit%  = 19% then u% = (u% + maxlines%) / 2%
                                    u% = max(0%, min(u%, maxlines% - 16%))
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 30% then gosub print_report
                  if keyhit%  =  8% then gosub gen_unix_files
                  if keyhit%  = 16% then       exit_program
                  if keyhit% <>  0% then       editpg1
                     goto editpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100          /* Load by Key (0 -       */
            return
L20100: REM Def/Enable Load by Key (0 - 2)         KEY$
            if key$ = " " then key$ = "3"
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
         "Enter Sort Order (0 - 3)                                    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, key$
        initialize_variables_2
            init(" ") type$(), level$(), unit$(), lib$(), ver$(),        ~
                      src$(), obj$()
            maxlines% = 0%
            t9% = 0% : cmod$ = " "

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
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
        dataload
            if key% <> 3% then keyhit% = 0%
            if keyhit% <> 14% then L30055
               tempkey% = key%
               key% = 0%
               call "SHOSTAT" ("Setting Key . . .")
               goto L30061

L30055:     call "SHOSTAT" ("Loading Data . . .")

L30061:     init (hex(00)) plowkey$

            call "REDALT4" (#1, plowkey$, key%, f1%)
               goto L30085

L30080:     call "READNEXT" (#1, f1%)
L30085:        if f1% <> 0% then L30110
               if keyhit% = 0% then return
                  key% = tempkey%
                  keyhit% = 0%
                  gosub initialize_variables_2
                  goto dataload
L30110:     u%, maxlines% = maxlines% + 1%
            get #1 using L30125, type$(u%), level$(u%), unit$(u%),        ~
                                ver$(u%), lib$(u%), key3$
L30125:         FMT CH(1), CH(3), CH(8), XX(8), CH(6), CH(6), CH(26)

            if key% <> 3% then L30135
            if keyhit% = 14% then L30135
               if str(key3$,1%,1%) > hex(2f) then L30135
                  gosub initialize_variables_2
                  keyhit% = 14%
                  goto dataload

L30135:     search str(sr$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30285
            str(src$(u%),2%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(ssr$(),loc%,1%) = "X"

L30285:     search str(cd$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30315
            str(src$(u%),6%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(scd$(),loc%,1%) = "X"

L30315:     search str(cm$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30345
            str(src$(u%),8%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(scm$(),loc%,1%) = "X"

L30345:     search str(ac$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30375
            str(src$(u%),4%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(sac$(),loc%,1%) = "X"

L30375:     search str(mc$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30405
            str(src$(u%),10%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(smc$(),loc%,1%) = "X"

L30405:     search str(wa$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30435
            str(src$(u%),12%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(swa$(),loc%,1%) = "X"

L30435:     search str(us$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30445
            str(src$(u%),14%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(sus$(),loc%,1%) = "X"

L30445:     search str(e2$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30555
            str(src$(u%),24%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(se2$(),loc%,1%) = "X"

L30555:     search str(e3$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30585
            str(src$(u%),28%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(se3$(),loc%,1%) = "X"

L30585:     search str(r2$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30615
            str(src$(u%),26%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(sr2$(),loc%,1%) = "X"

L30615:     search str(r3$()) = str(unit$(u%)) to cursor%() step 8
             if cursor%(1%) = 0% then L30645
            str(src$(u%),30%,1%) = "X"
            loc% = (7% + cursor%(1%))/ 8%
            str(sr3$(),loc%,1%) = "X"

L30645:
            if keyhit% <> 14% then L30080
            call "READ101" (#1, key(#1), f1%)
               if f1% = 0% then L30080
            temp$ = " "
            if type$(u%) < "2" then L30715
            if type$(u%) > "8" then level$(u%) = " "
            for i% = 1% to 14%
                str(temp$,15%-i%, 1%) = str(src$(u%),22%+i%,1%)
            next i%
L30715:     put #1 using L30725, type$(u%), level$(u%),                   ~
                                str(temp$,,14%), unit$(u%)
L30725:         FMT POS(33), CH(1), CH(3), CH(14), CH(8)
            rewrite #1
            goto L30080

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        print_report

            call "SHOSTAT" ("Report In Progress. . .")

            gosub print_lib_count

            pl% = 999% : pg% = 0% : pg$ = "Level Used List"
            uct% = 0%
            for p% = 1% to maxlines%
            if type$ (p%)  = "0" then L31150
            if p%    = 1% then L31120
            if key%  = 0% then L31120
            if key%  = 1% then L31100
               if type$ (p%) <> type$ (p% - 1%) then L31101
               if type$ (p%)  = "9" then L31120
L31100:        if level$(p%)  = level$(p% - 1%) then L31120
L31101:           if uct% = 0% then L31120
                  print:print using L31110, uct% : uct% = 0% : pl% = 999%
L31110: %Total Units #####

L31120:         if pl% > 50% then gosub main_heading
            print using L31135, type$(p%), level$(p%), unit$(p%),         ~
                               lib$(p%), ver$(p%), obj$(p%), src$(p%)
L31135: %# ### ########  ######  ######  ########  ######################~
        ~################
            pl% = pl% + 1% : uct% = uct% + 1%
L31150:     next p%
            if uct% = 0% then L31165
            print:print using L31110, uct% : uct% = 0% : pl% = 999%

L31165:     if keyhit% <> 30% then print_end

L31380: %########

            pl% = 999% : pg% = 0% : pg$ = "Units in SUBSRCE2 not Found"
            for p% = 1% to e2%
                if se2$(p%) <> " " then L31975
                if pl% > 50% then gosub lib_heading
                print using L31380, e2$(p%)
            pl% = pl% + 1% : uct% = uct% + 1%
L31975:     next p%
            if uct% = 0% then L31990
            print:print using L31110, uct% : uct% = 0% : pl% = 999%

L31990:     pl% = 999% : pg% = 0% : pg$ = "Units in SUBSRCE3 not Found"
            for p% = 1% to e3%
                if se3$(p%) <> " " then L32020
                if pl% > 50% then gosub lib_heading
                print using L31380, e3$(p%)
            pl% = pl% + 1% : uct% = uct% + 1%
L32020:     next p%
            if uct% = 0% then L32035
            print:print using L31110, uct% : uct% = 0% : pl% = 999%

L32035:     pl% = 999% : pg% = 0% : pg$ = "Units in SUBSRCR2 not Found"
            for p% = 1% to r2%
                if sr2$(p%) <> " " then L32065
                if pl% > 50% then gosub lib_heading
                print using L31380, r2$(p%)
            pl% = pl% + 1% : uct% = uct% + 1%
L32065:     next p%
            if uct% = 0% then L32080
            print:print using L31110, uct% : uct% = 0% : pl% = 999%

L32080:     pl% = 999% : pg% = 0% : pg$ = "Units in SUBSRCR3 not Found"
            for p% = 1% to r3%
                if sr3$(p%) <> " " then L32110
                if pl% > 50% then gosub lib_heading
                print using L31380, r3$(p%)
            pl% = pl% + 1% : uct% = uct% + 1%
L32110:     next p%
            if uct% = 0% then print_end
            print:print using L31110, uct% : uct% = 0% : pl% = 999%

        print_end
            print
            print "* * * End of Report * * *"
            close printer
            return

        main_heading
            select printer
            print page : pg% = pg% + 1%
            print using L33130, pg$, pg%
            print
            print using L33090
            print using L33110
            pl% = 1%
            return
L33090: %T LVL PROG/SUB  SESLIB  SESVER   O U 2 3  SrAcCdCmMcWaUsUcWiIs  ~
        ~E2R2E3R3  A3VS
L33110: %- --- --------  ------  ------  --------  --------------------- ~
        ~--------  ----
L33130: %##############################                                  ~
        ~      Page ###

        lib_heading
            select printer
            print page : pg% = pg% + 1%
            print using L33290, pg$, pg%
            print
            print using L33250
            print using L33270
            pl% = 1%
            return
L33250: %    UNIT

L33270: %--------

L33290: %##############################                                  ~
        ~Page ###

        print_lib_count
            select printer
            print page
            print "Library Counts"
            print
            print "Source2", sr%
            print "Sourcecd", cd%
            print "Sourcecm", cm%
            print "Sourceac", ac%
            print "Sourcemc", mc%
            print "Sourcewa", wa%
            print "Utilsrce", us%
            print
            print "Subsrce2", e2%
            print "Subsrce3", e3%
            print "Subsrcr2", r2%
            print "Subsrcr3", r3%
            print
            return

        REM *************************************************************~
            * Generate Unix files for transport                         *~
            *************************************************************

        gen_unix_files

            t9% = 0% : cmod$ = " " : mcnt% = 0% : first% = 0%

            for m% = 1% to maxlines%
                if type$(m%) < "2" then next_m
                if str(src$(m%),24%,7%) = " " then next_m

                if type$(m%) = "9" then L35132
                   convert level$(m%) to temp%
                   t9% = max(t9%, temp%+1%)
                   goto L35141

L35132:         temp% = t9%

L35141:         file$ = " "
                if str(src$(m%),24%,1%) <> " " then str(file$,4%) = "SS2"
                if str(src$(m%),26%,1%) <> " " then str(file$,4%) = "SR2"
                if str(src$(m%),28%,1%) <> " " then str(file$,4%) = "SS3"
                if str(src$(m%),30%,1%) <> " " then str(file$,4%) = "SR3"
                if file$ = " " then next_m

                if temp% > 9% then temp1% = temp% + 55%                  ~
                              else temp1% = temp% + 48%
                str(file$,2%,1%) = bin(temp1%,1)
                str(file$,1%,1%) = "L"
                str(file$,3%,1%) = "A"
*              STR(FILE$,7%,2%) = ".O"

                if file$ = cmod$ then L35270
                   f2% = 1% : file1$ = file$ : mcnt% = 0%
L35230:            if cmod$ <> " " then gosub close_and_rename
                   cmod$ = file$
                   call "GETNAMES" addr(#3, temp$, lib$, vol$)
                   call "PUTNAMES" addr(#3, file1$, lib$, vol$)
                if first% = 0% then L35265
                   call "WORKOPN2" (#3, "OUTSP", 100%, f2%)
                   goto L35270
L35265:            open #3, output
                   first% = 1%
L35270:         if mcnt% < 30% then L35290
                   mcnt% = 0%
                   str(file1$,3%,1%) = add(hex(01))
                   goto L35230

L35290:         temp$ = unit$(m%)
                for mm% = 1% to len(temp$)
                    if str(temp$,mm%,1%) > hex(40) then                  ~
                       str(temp$,mm%,1%) = or hex(20)
                next mm%
                temp$ = temp$ & ".o"

                put #3, str(temp$)
                write #3
                mcnt% = mcnt% + 1%

        next_m
            next m%

            if cmod$ <> " " then gosub close_and_rename
            file$, lib$, vol$ = " " : t9% = 0% : cmod$ = " "
            call "PUTNAMES" addr(#3, file$, lib$, vol$)
            return

         close_and_rename

            init (" ") pathc$
            call "GETPATH" addr(#3, pathc$)
            i% = pos(pathc$ = hex(00))
            if i% > 0% then str(pathc$,i%) = " "

            close #3

            pathc2$ = pathc$ & ".idx" & hex(00)
            call "GENPUFLS" addr("D", pathc2$, pathc2$, i%)
            
            pathc2$ = pathc$ & ".lck" & hex(00)
            call "GENPUFLS" addr("D", pathc2$, pathc2$, i%)

            pathc2$ = pathc$ & ".o"  & hex(00)
            pathc$ = pathc$ & ".dat" & hex(00)
            call "GENPUFLS" addr("S", pathc$, pathc2$, i%)

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
              on fieldnr% gosub L40080          /* Load by Key (0 -  */
              goto L40090

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40080:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Display Subroutine Level Extract",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Display Order (0 - 3)",                      ~
               at (06,30), fac(lfac$( 1)), key$                 , ch(01),~
                                                                         ~
               at (08,02), "0 = Unit/Version/SES Library",               ~
               at (09,02), "1 = Level/Unit/Version/SES Library",         ~
               at (10,02), "2 = Type/Level/Unit/Version/SES Library",    ~
               at (11,02),"3 = Type/Level/Source Type/Unit [Unix Xport]",~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40200
                  call "MANUAL" ("FILEEXTD") : goto L40090

L40200:        if keyhit% <> 15 then L40215
                  call "PRNTSCRN" : goto L40090

L40215:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40310     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0d0e0f1000)
            if fieldnr% = 1% then L40290
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40290:     if fieldnr% > 2% then L40300
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40300:     return

L40310: if fieldnr% > 0% then L40355  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40355:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102
              gosub set_pf2

              inpmessage$, errormsg$ = " "
              errormsg$ = "T LVL PROG/SUB  SESLIB  SESVER   O U 2 3  SrAc~
        ~CdCmMcWaUsUcWiIs  E2R2E3R3  A3VS"
L41120:     accept                                                       ~
               at (01,02),                                               ~
                  "Display Subroutine Level Extract",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(a4)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(84)), type$ (u% + 1%)        , ch(01),~
               at (06,04), fac(hex(84)), level$(u% + 1%)        , ch(03),~
               at (06,08), fac(hex(84)), unit$ (u% + 1%)        , ch(08),~
               at (06,18), fac(hex(84)), lib$  (u% + 1%)        , ch(06),~
               at (06,26), fac(hex(84)), ver$  (u% + 1%)        , ch(06),~
               at (06,34), fac(hex(84)), obj$  (u% + 1%)        , ch(08),~
               at (06,44), fac(hex(84)), src$  (u% + 1%)        , ch(36),~
                                                                         ~
               at (07,02), fac(hex(84)), type$ (u% + 2%)        , ch(01),~
               at (07,04), fac(hex(84)), level$(u% + 2%)        , ch(03),~
               at (07,08), fac(hex(84)), unit$ (u% + 2%)        , ch(08),~
               at (07,18), fac(hex(84)), lib$  (u% + 2%)        , ch(06),~
               at (07,26), fac(hex(84)), ver$  (u% + 2%)        , ch(06),~
               at (07,34), fac(hex(84)), obj$  (u% + 2%)        , ch(08),~
               at (07,44), fac(hex(84)), src$  (u% + 2%)        , ch(36),~
                                                                         ~
               at (08,02), fac(hex(84)), type$ (u% + 3%)        , ch(01),~
               at (08,04), fac(hex(84)), level$(u% + 3%)        , ch(03),~
               at (08,08), fac(hex(84)), unit$ (u% + 3%)        , ch(08),~
               at (08,18), fac(hex(84)), lib$  (u% + 3%)        , ch(06),~
               at (08,26), fac(hex(84)), ver$  (u% + 3%)        , ch(06),~
               at (08,34), fac(hex(84)), obj$  (u% + 3%)        , ch(08),~
               at (08,44), fac(hex(84)), src$  (u% + 3%)        , ch(36),~
                                                                         ~
               at (09,02), fac(hex(84)), type$ (u% + 4%)        , ch(01),~
               at (09,04), fac(hex(84)), level$(u% + 4%)        , ch(03),~
               at (09,08), fac(hex(84)), unit$ (u% + 4%)        , ch(08),~
               at (09,18), fac(hex(84)), lib$  (u% + 4%)        , ch(06),~
               at (09,26), fac(hex(84)), ver$  (u% + 4%)        , ch(06),~
               at (09,34), fac(hex(84)), obj$  (u% + 4%)        , ch(08),~
               at (09,44), fac(hex(84)), src$  (u% + 4%)        , ch(36),~
                                                                         ~
               at (10,02), fac(hex(84)), type$ (u% + 5%)        , ch(01),~
               at (10,04), fac(hex(84)), level$(u% + 5%)        , ch(03),~
               at (10,08), fac(hex(84)), unit$ (u% + 5%)        , ch(08),~
               at (10,18), fac(hex(84)), lib$  (u% + 5%)        , ch(06),~
               at (10,26), fac(hex(84)), ver$  (u% + 5%)        , ch(06),~
               at (10,34), fac(hex(84)), obj$  (u% + 5%)        , ch(08),~
               at (10,44), fac(hex(84)), src$  (u% + 5%)        , ch(36),~
                                                                         ~
               at (11,02), fac(hex(84)), type$ (u% + 6%)        , ch(01),~
               at (11,04), fac(hex(84)), level$(u% + 6%)        , ch(03),~
               at (11,08), fac(hex(84)), unit$ (u% + 6%)        , ch(08),~
               at (11,18), fac(hex(84)), lib$  (u% + 6%)        , ch(06),~
               at (11,26), fac(hex(84)), ver$  (u% + 6%)        , ch(06),~
               at (11,34), fac(hex(84)), obj$  (u% + 6%)        , ch(08),~
               at (11,44), fac(hex(84)), src$  (u% + 6%)        , ch(36),~
                                                                         ~
               at (12,02), fac(hex(84)), type$ (u% + 7%)        , ch(01),~
               at (12,04), fac(hex(84)), level$(u% + 7%)        , ch(03),~
               at (12,08), fac(hex(84)), unit$ (u% + 7%)        , ch(08),~
               at (12,18), fac(hex(84)), lib$  (u% + 7%)        , ch(06),~
               at (12,26), fac(hex(84)), ver$  (u% + 7%)        , ch(06),~
               at (12,34), fac(hex(84)), obj$  (u% + 7%)        , ch(08),~
               at (12,44), fac(hex(84)), src$  (u% + 7%)        , ch(36),~
                                                                         ~
               at (13,02), fac(hex(84)), type$ (u% + 8%)        , ch(01),~
               at (13,04), fac(hex(84)), level$(u% + 8%)        , ch(03),~
               at (13,08), fac(hex(84)), unit$ (u% + 8%)        , ch(08),~
               at (13,18), fac(hex(84)), lib$  (u% + 8%)        , ch(06),~
               at (13,26), fac(hex(84)), ver$  (u% + 8%)        , ch(06),~
               at (13,34), fac(hex(84)), obj$  (u% + 8%)        , ch(08),~
               at (13,44), fac(hex(84)), src$  (u% + 8%)        , ch(36),~
                                                                         ~
               at (14,02), fac(hex(84)), type$ (u% + 9%)        , ch(01),~
               at (14,04), fac(hex(84)), level$(u% + 9%)        , ch(03),~
               at (14,08), fac(hex(84)), unit$ (u% + 9%)        , ch(08),~
               at (14,18), fac(hex(84)), lib$  (u% + 9%)        , ch(06),~
               at (14,26), fac(hex(84)), ver$  (u% + 9%)        , ch(06),~
               at (14,34), fac(hex(84)), obj$  (u% + 9%)        , ch(08),~
               at (14,44), fac(hex(84)), src$  (u% + 9%)        , ch(36),~
                                                                         ~
               at (15,02), fac(hex(84)), type$ (u% +10%)        , ch(01),~
               at (15,04), fac(hex(84)), level$(u% +10%)        , ch(03),~
               at (15,08), fac(hex(84)), unit$ (u% +10%)        , ch(08),~
               at (15,18), fac(hex(84)), lib$  (u% +10%)        , ch(06),~
               at (15,26), fac(hex(84)), ver$  (u% +10%)        , ch(06),~
               at (15,34), fac(hex(84)), obj$  (u% +10%)        , ch(08),~
               at (15,44), fac(hex(84)), src$  (u% +10%)        , ch(36),~
                                                                         ~
               at (16,02), fac(hex(84)), type$ (u% +11%)        , ch(01),~
               at (16,04), fac(hex(84)), level$(u% +11%)        , ch(03),~
               at (16,08), fac(hex(84)), unit$ (u% +11%)        , ch(08),~
               at (16,18), fac(hex(84)), lib$  (u% +11%)        , ch(06),~
               at (16,26), fac(hex(84)), ver$  (u% +11%)        , ch(06),~
               at (16,34), fac(hex(84)), obj$  (u% +11%)        , ch(08),~
               at (16,44), fac(hex(84)), src$  (u% +11%)        , ch(36),~
                                                                         ~
               at (17,02), fac(hex(84)), type$ (u% +12%)        , ch(01),~
               at (17,04), fac(hex(84)), level$(u% +12%)        , ch(03),~
               at (17,08), fac(hex(84)), unit$ (u% +12%)        , ch(08),~
               at (17,18), fac(hex(84)), lib$  (u% +12%)        , ch(06),~
               at (17,26), fac(hex(84)), ver$  (u% +12%)        , ch(06),~
               at (17,34), fac(hex(84)), obj$  (u% +12%)        , ch(08),~
               at (17,44), fac(hex(84)), src$  (u% +12%)        , ch(36),~
                                                                         ~
               at (18,02), fac(hex(84)), type$ (u% +13%)        , ch(01),~
               at (18,04), fac(hex(84)), level$(u% +13%)        , ch(03),~
               at (18,08), fac(hex(84)), unit$ (u% +13%)        , ch(08),~
               at (18,18), fac(hex(84)), lib$  (u% +13%)        , ch(06),~
               at (18,26), fac(hex(84)), ver$  (u% +13%)        , ch(06),~
               at (18,34), fac(hex(84)), obj$  (u% +13%)        , ch(08),~
               at (18,44), fac(hex(84)), src$  (u% +13%)        , ch(36),~
                                                                         ~
               at (19,02), fac(hex(84)), type$ (u% +14%)        , ch(01),~
               at (19,04), fac(hex(84)), level$(u% +14%)        , ch(03),~
               at (19,08), fac(hex(84)), unit$ (u% +14%)        , ch(08),~
               at (19,18), fac(hex(84)), lib$  (u% +14%)        , ch(06),~
               at (19,26), fac(hex(84)), ver$  (u% +14%)        , ch(06),~
               at (19,34), fac(hex(84)), obj$  (u% +14%)        , ch(08),~
               at (19,44), fac(hex(84)), src$  (u% +14%)        , ch(36),~
                                                                         ~
               at (20,02), fac(hex(84)), type$ (u% +15%)        , ch(01),~
               at (20,04), fac(hex(84)), level$(u% +15%)        , ch(03),~
               at (20,08), fac(hex(84)), unit$ (u% +15%)        , ch(08),~
               at (20,18), fac(hex(84)), lib$  (u% +15%)        , ch(06),~
               at (20,26), fac(hex(84)), ver$  (u% +15%)        , ch(06),~
               at (20,34), fac(hex(84)), obj$  (u% +15%)        , ch(08),~
               at (20,44), fac(hex(84)), src$  (u% +15%)        , ch(36),~
                                                                         ~
               at (21,02), fac(hex(84)), type$ (u% +16%)        , ch(01),~
               at (21,04), fac(hex(84)), level$(u% +16%)        , ch(03),~
               at (21,08), fac(hex(84)), unit$ (u% +16%)        , ch(08),~
               at (21,18), fac(hex(84)), lib$  (u% +16%)        , ch(06),~
               at (21,26), fac(hex(84)), ver$  (u% +16%)        , ch(06),~
               at (21,34), fac(hex(84)), obj$  (u% +16%)        , ch(08),~
               at (21,44), fac(hex(84)), src$  (u% +16%)        , ch(36),~
                                                                         ~
                                                                         ~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42500
                  call "MANUAL" ("FILEEXTD") : goto L41120

L42500:        if keyhit% <> 15 then L42530
                  call "PRNTSCRN" : goto L41120

L42530:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            pf$(1) = "(1)Start Over  (2)Top (4)Next (6)Down   " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(18)Down Half  (3)End (5)Prev (7)Up     " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(19)Up Half    (14)Print      (30)Print " &        ~
                     "Analysis               (16)Exit Program"
            pfkeys$ = hex(01020304050607ffffffffff0d0e0f100012131e)

            if key% <> 3% then return
               str(pf$(1),41%,20%) = "(8)Gen Unix Lists"
               str(pfkeys$,8%,1%) = hex(08)
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100          /* Load by Key (0 -       */
            return
L50100: REM Test for Load by Key (0 - 2)          KEY$
            key% = (pos("0123" = key$)) - 1%
            if key% < 0% then L50145
            if key% > 3% then L50145
               return
L50145:     errormsg$ = "Invalid Response"
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        xdataput
            call "SHOSTAT" ("Processing Programs. . .")
            plowkey$ = "0" : level% = -1% : mrk$ = " "

L31090:     call "PLOWALTS" (#2, plowkey$, 2%, 1%, f1%)
               if f1% = 0% then dataputs
            gosub write_program
			str(plowkey$,13%,1%) = "a"
            goto L31090

        write_program

            str(readkey$) = str(plowkey$,5%,8%)
            k% = 0%
            call "READ101" (#1, readkey$, f1%)
               if f1% = 0% then L31510
            get #1 using L31490, mrk$, lev$
L31490:         FMT CH(1), CH(3)
            convert lev$ to k%, data goto L31510
L31510:     k% = max(k%, level% + 1%)
            if str(plowkey$,5%,8%) = "VPRPSLCT" then  k% = min(k%, 4%)
            if str(plowkey$,5%,8%) = "VPRVSLCT" then  k% = min(k%, 4%)
            if str(plowkey$,5%,8%) = "HNYPRCSB" then  k% = min(k%, 4%)
            convert k% to lev$, pic(##0)
            put #1, using L31540, mrk$, lev$, readkey$
L31540:         FMT CH(1), CH(3), CH(28)
            if f1% = 0% then write #1 else rewrite #1
            return

        dataputs
            plowkey1$ = " "
            level% = level% + 1%
            convert level% to str(plowkey1$,,3), pic(##0)

            call "PLOWALTS" (#1, plowkey1$, 1%, 3%, f1%)
              if f1% = 0% then return
                 goto L31700

L31680:     call "PLOWALTS" (#1, plowkey1$, 1%, 3%, f1%)
              if f1% = 0% then dataputs
L31700:     readkey1$ = " "
		    get #1 using L31710, readkey1$
L31710:         FMT POS(5), CH(8)
            

            call "SHOSTAT"                                               ~
                 ("Processing Program" & " " & str(readkey1$,,8%))
            str(readkey1$,9%,1%) = "2"
			str(readkey1$,10%) = hex(0000000000000000)
            lower% = 0%
L31740:     call "PLOWALTS" (#2, readkey1$, 0%, 9%, f1%)
               if f1% = 0% then L31900
            plowkey$ = " "
            str(plowkey$) = "2   "
            str(plowkey$,5%,8%) = str(readkey1$,10%,8%)
            if str(plowkey$,5%,8%) = " " then L31740
            call "PLOWALTS" (#2, plowkey$, 2%, 12%, f1%)
               if f1% = 0% then L31740
            gosub write_program
            lower% = 1%
            goto L31740

L31900:     if lower% = 0% then L31930
               if level% = 0% then mrk$ = "1" else mrk$ = "2"
                  goto L31940
L31930:        if level% = 0% then mrk$ = "0" else mrk$ = "9"
L31940:     call "READ101" (#1, str(plowkey1$,4%), f1%)
               if f1% = 0% then L31680
            put #1 using L31970, mrk$
L31970:         FMT CH(1)
            rewrite #1
            goto L31680


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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
