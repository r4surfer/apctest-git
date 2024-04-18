        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  M   M  M   M  EEEEE  X   X  M   M   AAA   PPPP           *~
            *  MM MM  MM MM  E       X X   MM MM  A   A  P   P          *~
            *  M M M  M M M  EEE      X    M M M  AAAAA  PPPP           *~
            *  M   M  M   M  E       X X   M   M  A   A  P              *~
            *  M   M  M   M  EEEEE  X   X  M   M  A   A  P              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * MMEXMAP - Extract File Structures from SES.               *~
            *           Uses structure definitions in INTDOC02 to create*~
            *           a map file (MMDATMAP) of the date positions.    *~
            *-----------------------------------------------------------*~
            *  This program contains valuable trade secrets and         *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 2000  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/24/96 ! Original (Clone from Ken's FILEEXT2)     ! ERN *~
            * 07/02/96 ! Modifications to finish coding           ! DER *~
            * 12/05/97 ! Place PUT after Read101 &append new data.! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            a_date%(100),                /* Date format array          */~
            a_posn%(100),                /* File position array        */~
            a_fldn$(100)16,              /* Field name array           */~
            b_date%(100),                /* Date format array          */~
            b_posn%(100),                /* File position array        */~
            b_fldn$(100)16,              /* Field name array           */~
            c_fldn$16,                   /* Field name work Area       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            frfile$16,                   /* From File (or Record)      */~
            hiver$6,                     /* High Version (' ' = high)  */~
            i$(24)80,                    /* Screen Image               */~
            inp_lib$8,                   /* Input Library              */~
            inp_vol$6,                   /* Input Volume               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            nrecords$7,                  /* Text total records         */~
            out_lib$8,                   /* Output Library             */~
            out_vol$6,                   /* Output Volume              */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99, readkey$99,      /* Miscellaneous Read/Plow Key*/~
            record$(4)256,               /* Record Work Area           */~
            repl_recs$3,                 /* Replace recs in MMDATMAP   */~
            scratch$1,                   /* scratch var                */~
            seslib$6,                    /* SES Library Name           */~
            sesreclen$4,                 /* SES record len. (ch(4))    */~
            st_filenm$8,                 /* Status File name (intdoc02)*/~
            st_title$50,                 /* Status Title               */~
            tmp_time$8,                  /* Temp time to get seconds   */~
            tofile$16                    /* To   File (or Record)      */

        dim                                                              ~
            f1%(3),                      /* File handle read OK        */~	
            f2%(3),                      /* File handle open OK        */~	
            rslt$(3)20                   /* File result return         */

*  SES Types: DATE
* a_date values:
*  Type  Fmt Now     Stored  New Fmt      Stored
*  ----  ----------- ------  -----------  --------
*    1   YYMMDD      CH(6)   CCYYMMDD     PD(11,1)
*    2   CCYYMMDD    CH(8)   CCYYMMDD     PD(11,1)
*    3   YY          CH(2)   CCYY         BI(2)
*    4   YYMM        CH(4)   CCYYMM       BI(4)
*    5   MM/DD/YY    CH(8)   CCYYMMDD     PD(11,1)
*    6   YYMMDD      BI(3)   CCYYDDD      BI(3)
*    7   YY          BI(4)   CCYY         BI(4)
*    8   999999      CH(6)   99999999     PD(11,1)
*    9   YYMMDD      BI(4)   CCYYMMDD     BI(4)
*
*
* Notes:
*  - Must be run against a Millie-ized SES.
*  - Dates in date/time stamps (BI(3)) will not be changed.  The routines
*    will be changed so that 12/31/2000 will be stored as the numeric
*    1001231.
*  - Variable field dates need to be added to the mapping file as these
*    vary from site to site. They should be specified as Type "1".
*  - Record specific info (SYSFILE2, e.g.) must be manually added to the
*    mapping file.  Must either do a file record by record or all records
*    as one layout.

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! INTDOC01 ! System Element Structures Element Dictio *~
            * #2  ! INTDOC02 ! System Element Structures Relationships  *~
            * #3  ! MMDATMAP ! Specialized Element Structures Dictionary*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "INTDOC01",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =   25, keylen =  16,                     ~
                        alt key 01, keypos =   17, keylen =  24,         ~
                            key 02, keypos =    1, keylen =  40,         ~
                            key 03, keypos =   41, keylen =  50, dup

            select #2, "INTDOC02",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   45, keylen =  31,                     ~
                        alt key 01, keypos =    1, keylen =  75,         ~
                            key 02, keypos =   23, keylen =  44,         ~
                            key 03, keypos =   39, keylen =  28          ~

            select #3, "MMDATMAP",                                      ~
                        varc,     indexed,  recsize =  2000,             ~
                        keypos =    1, keylen =  46                      ~


            call "SHOSTAT" ( "Opening files, one moment please." )

            nrecords% = 0
            st_title$  = "Create MMDATMAP from INTDOC02"
            st_filenm$ = "INTDOC02"
            call "EXTRACT" addr( "OL", out_lib$ )
            call "EXTRACT" addr( "OV", out_vol$ )
            call "READFDR" addr( st_filenm$, out_lib$, out_vol$, 0%, ~
                                 "RC", nrecords%, err% )
            convert nrecords% to nrecords$, pic(#######)

            for i% = 1% to 3%
               f1%(i%)     = 0%
               f2%(i%)     = 2%
            next i%
            rslt$(1%)   = "REQUIRED"
            rslt$(2%)   = "REQUIRED"
            rslt$(3%)   = "        "

            call "OPENOLIB" (#1, "SHARE", f2%(1%), rslt$(1%), "    ")
            if f2%(1%) > 0% then exit_program

            call "OPENOLIB" (#2, "SHARE", f2%(2%), rslt$(2%), "    ")
            if f2%(2%) > 0% then exit_program

            call "OPENOLIB" (#3, "SHARE", f2%(3%), rslt$(3%), "    ")
            if f2%(3%) = 0% then goto open_okay
            call "OPENOLIB" (#3, "OUTPT", f2%(3%), rslt$(3%), "    ")
            close #3
            call "OPENOLIB" (#3, "SHARE", f2%(3%), rslt$(3%), "    ")
            if f2%(2%) > 0% then exit_program

open_okay

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            tmp_time$ = time

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "MMEXMAP: " & str(cms2v$,,8)

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
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
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
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% > 4% then fieldnr% = fieldnr% - 1%
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
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* From File              */~
                              L20300,         /* SES Library Name       */~
                              L20400,         /* High Version           */~
                              L20500          /* Replace existing recs  */
            return
L20100: REM Def/Enable From/To File (or Record)    FRFILE$, TOFILE$
            if frfile$ <> " " then return
               frfile$ = "ALL" : tofile$ = " "
               return

L20300: REM Def/Enable SES Library Name            SESLIB$
            if seslib$ = " " then seslib$ = "CAELUS"
            return

L20400: REM Def/Enable High Version (' ' = high)   HIVER$
            return

L20500: REM Def/Enable replace existing records     REPL_RECS$
            if repl_recs$ <> "YES" then repl_recs$ = "NO "
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
         "Enter From/To File                                           ",~
         "Enter SES Library Name                                       ",~
         "Enter Version (Highest Version less than or equal to Extract)",~
         "Change to YES to replace previously identified records       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      frfile$, hiver$, seslib$, tofile$
            repl_recs$ = "NO "
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1998  an unpublished work by CAELUS,       *~
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
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
dataput
            call "SHOSTAT" ("Processing. . .")
            plowkey$ = "FILE"

            cur_rec% = 0%
            user_per% = 5%
            per_loop% = ( nrecords%/100% ) * user_per%
            gosub filestatus

            if frfile$ = "ALL" then DP1090
               str(plowkey$,9%) = frfile$
               str(plowkey$,,24) = addc all(hex(ff))

DP1090:     call "PLOWALTS" (#1, plowkey$, 1%, 8%, f1%(1%))
               if f1%(1%) = 0% then DP1140
            get #1 using obsfmt, file1$, obs$
            gosub dataput2
            goto DP1090

DP1140:     gosub filestatus
            goto exit_program



dataput2
********
* Within file name range?
            if frfile$ = "ALL" then FILEOK
               if str(plowkey$,9%,16%) < frfile$ then return
               if str(plowkey$,9%,16%) <= tofile$ then FILEOK
                  init (hex(ff)) str(plowkey$,9%) : return

* Obsolete file?
FILEOK:     get #1 using obsfmt, file$, obs$
obsfmt:         FMT POS(25), CH(16), POS(936), CH(1)
            if obs$ = "Y" then return

* Get version
            readkey$, passver$ = " "
            readkey$ = str(file$,,16) & str(seslib$,,6) & str(file$,,16)

VERLOOP:    call "PLOWALTS" (#2, readkey$, 2%, 38%, f1%(2%))

* Get the record length
            if f1%(2%) = 1%  then get #2 using RECLNGTH, sesreclen$
RECLNGTH:      FMT POS(188), CH(4)

            if f1%(2%) = 0%  then VEREND
            if hiver$ = " " then VER002
            if str(readkey$,39%,6%) > hiver$ then VEREND
VER002:     passver$ = str(readkey$,39%,6%)
            goto VERLOOP

VEREND:     if passver$ = " " then return

* convert sesreclen to a numeric
            sesreclen% = 0
            convert sesreclen$ to sesreclen%, data goto GETDATES

* Now get them dates!
GETDATES:   psn%        = 1%
            count%      = 0%
            mat a_posn% = zer
            mat a_date% = zer
            mat b_posn% = zer
            mat b_date% = zer
            init(" ") a_fldn$(), b_fldn$()

            readkey$ = str(file$,,16%) & str(passver$,,6%) &             ~
                       str(seslib$,,6%) & hex(000000)

LOOP02:     call "PLOWALTS" (#2, readkey$, 0%, 28%, f1%(2%))

            gosub filestatus
            cur_rec% = cur_rec% + 1

            if f1%(2%) = 0% then LOOP02_END

            get #2 using all02, str(record$())
all02:          FMT CH(256)
            if str(record$(),18%,5%) = hex(0000000000) then LOOP02

            convert str(record$(),188%,4%) to len%, data goto LOOP02

            if str(record$(), 245%, 4%) <> "DATE"  then NEXT_FLD
            datefmt% = 1%
            mult%    = val( str( record$(), 243%, 2% ), 2% )
            inc%     = len% / mult%
            for i% = 1%  to mult%
               count%          = count% + 1%
               a_posn%(count%) = psn%
               psn%            = psn%   + inc%
               a_date%(count%) = datefmt%
               a_fldn$(count%) = str( record$(), 1%, 16% )
            next i%
            len% = 0%

NEXT_FLD:   psn% = psn% + len%
            goto LOOP02

LOOP02_END: if count% = 0 then return
            readkey$ = str( file$, 1%, 16% )
            call "READ101" ( #3, readkey$, f1%(3%) )
            if f1%(3%) = 0%        then write_data_to_file
            if repl_recs$ <> "YES" then write_data_to_file
                                /* sort & append new fields to old data */
            get #3 using datemap_get, b_posn%(), b_date%(), b_fldn$()
            last_pos% = 0%
            for u3% = 1% to 100%
                for u4% = 1% to 100%
                    if u4%          <  u3%          then       check_b
                    if a_posn%(u4%) <= last_pos%    then       check_b
                    if a_posn%(u4%) <  a_posn%(u3%) then gosub swap_a
                    if last_pos%    >  a_posn%(u3%) then gosub swap_a
check_b:            if b_posn%(u4%) <= last_pos%    then       end_append_loop
                    if b_posn%(u4%) <  a_posn%(u3%) then gosub swap_b
                    if last_pos%    >  a_posn%(u3%) then gosub swap_b
end_append_loop:next u4%
                if last_pos% < a_posn%(u3%) then last_pos% = a_posn%(u3%) ~
                    else u3%, u4% = 999%
            next u3%

            goto write_data_to_file

swap_a:
   c_posn%     =a_posn%(u4%):c_date%     =a_date%(u4%):c_fldn$     =a_fldn$(u4%)
   a_posn%(u4%)=a_posn%(u3%):a_date%(u4%)=a_date%(u3%):a_fldn$(u4%)=a_fldn$(u3%)
   a_posn%(u3%)=c_posn%     :a_date%(u3%)=c_date%     :a_fldn$(u3%)=c_fldn$
            return

swap_b:
   c_posn%     =b_posn%(u4%):c_date%     =b_date%(u4%):c_fldn$     =b_fldn$(u4%)
   b_posn%(u4%)=a_posn%(u3%):b_date%(u4%)=a_date%(u3%):b_fldn$(u4%)=a_fldn$(u3%)
   a_posn%(u3%)=c_posn%     :a_date%(u3%)=c_date%     :a_fldn$(u3%)=c_fldn$
            return

write_data_to_file
            put #3 using datemap, ~
            file1$, " ", 0%, passver$, sesreclen%, ~
            a_posn%(), a_date%(), a_fldn$(), " "
            /* Write record if it does not exist */
            if f1%(3%) = 0% then write #3
            /* If record exists and user wants to replace, do so */
            if f1%(3%) <> 0% and repl_recs$ = "YES" then rewrite #3
            return

datemap:    FMT CH(16),     /* File name                      0 1   */~
                CH(30),     /* Record Key                     0 1   */~
                BI(2),      /* Break%                               */~
                CH(6),      /* SES Version                          */~
                BI(2),      /* SES Record len                       */~
            100*BI(2),      /* Starting Position Array              */~
            100*BI(1),      /* Date Format array                    */~
            100*CH(16),     /* Date Field Name                      */~
                CH(44)      /* Filler                               */

datemap_get:fmt pos(57),    /*                                      */~
            100*bi(2),      /* Starting Position Array              */~
            100*bi(1),      /* Date Format array                    */~
            100*ch(16)      /* Date Field Name                      */

filestatus
   if mod( cur_rec%, per_loop% ) = 0% or cur_rec% = nrecords% then ~
   call "FILESTAT" ( st_title$, st_filenm$, cur_rec%, nrecords% )
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
              on fieldnr% gosub L40090,         /* From File         */   ~
                                L40090,         /* SES Library Name  */   ~
                                L40090,         /* High Version      */   ~
                                L40090          /* Replace Recs      */
              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "Extract File Structures",                             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "From and To File",                           ~
               at (06,30), fac(lfac$( 1)), frfile$              , ch(16),~
               at (06,47), "-",                                          ~
               at (06,49), fac(lfac$( 1)), tofile$              , ch(16),~
                                                                         ~
               at (07,02), "SES Library Name",                           ~
               at (07,30), fac(lfac$( 2)), seslib$              , ch(06),~
                                                                         ~
               at (08,02), "High Version (' '=highest)",                 ~
               at (08,30), fac(lfac$( 3)), hiver$               , ch(06),~
                                                                         ~
               at (10,02), "Replace existing records",                   ~
               at (10,30), fac(lfac$( 4)), repl_recs$           , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40260
                  call "MANUAL" ("FILEEXT1") : goto L40105

L40260:        if keyhit% <> 15 then L40275
                  call "PRNTSCRN" : goto L40105

L40275:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40370     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40350
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40350:     if fieldnr% > 2% then L40360
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40360:     return

L40370: if fieldnr% > 0% then L40415  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Extract Data"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40415:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* From File              */~
                              L50300,         /* SES Library Name       */~
                              L50400,         /* High Version           */~
                              L50500          /* Replace existing recs  */
            return
L50100: REM Test for From File (or Record)        FRFILE$
            if frfile$ <> " " then L50130
L50120:        frfile$ = "ALL" : tofile$ = " " : return
L50130:     if frfile$ = "ALL" then L50120
            if tofile$ = " " then tofile$ = frfile$
            if frfile$ <= tofile$ then return
               errormsg$ = "Invalid Range Selection."
            return

L50300: REM Test for SES Library Name             SESLIB$
            return

L50400: REM Test for High Version (' ' = high)    HIVER$
            return

L50500: REM Test for replace existing records     REPL_RECS$
            if repl_recs$ <> "YES" then repl_recs$ = "NO "
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
            *  Copyright (c) 1996  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
           end
