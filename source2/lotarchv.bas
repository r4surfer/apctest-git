        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  L       OOO   TTTTT   AAA   RRRR    CCC   H   H  V   V   *~
            *  L      O   O    T    A   A  R   R  C   C  H   H  V   V   *~
            *  L      O   O    T    AAAAA  RRRR   C      HHHHH  V   V   *~
            *  L      O   O    T    A   A  R   R  C   C  H   H   V V    *~
            *  LLLLL   OOO     T    A   A  R   R   CCC   H   H    V     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * LOTARCHV - Archiving of Lot Movement Data.  This program  *~
            *           will archive files from LOTMVMNT to LOTMYYYY and*~
            *           from LOTMVDTL to LOTDYYYY based on users picked *~
            *           ranges. (i.e. YYYY denotes year)                *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/25/93 ! Original - In response to PRR 12578      ! RJH *~
            * 08/14/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            archyear$4,                  /* Archive File Year          */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                /* Line for input screen      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dateline$79,                 /* Date line on screen        */~
            descr_m(14),                 /* Descr Map For PLOWCODE     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            eligibles$39,                /* # of records eligibles Mtr */~
            fmarchdate$10,               /* Archive Date Range         */~
              unffmarchdate$10,          /* Archive Date Range         */~
            getback$9,                   /* (1)Return on Safeguard Scr.*/~
            header$42, header2$23,       /* Heading for Input Screen   */~
            header$(3)79,                /* Header for PLOWCODE call   */~
            hits$39,                     /* # of records hits Master   */~
            hitsd$10,                    /* # of records hits Detail   */~
            i$(24)80,                    /* Screen Image               */~
            inc(2),                      /* PLOWCODE Arguments or some */~
            inc$(2)8,                    /*   sort known only to LDJ   */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lotdprname$8,                /* LOTD#### File Name         */~
            lotdrec$200,                 /* LOTMVDTL Record            */~
            lotmprname$8,                /* LOTM#### File Name         */~
            lotmrec$160,                 /* LOTMVMNT Record            */~
            mode$5,                      /* HNYQUAN Open mode          */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$96,                  /* Misc. Plowkey              */~
            readkey$96,                  /*                            */~
            safeguard$3,                 /* Proceed to Archive (Y/N)?  */~
            toarchdate$10,               /* Archive Date Range         */~
              unftoarchdate$10,          /* Archive Date Range         */~
            userid$3,                    /* Current User Id            */~
            writtens$39,                 /* # of records writtens Mtr. */~
            writtensd$10                 /* # of records writtens Dtl. */

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
            * #01 ! LOTMVMNT ! Lot Movement Tracking Master File        *~
            * #02 ! LOTMVDTL ! Lot Movement Detail Tracking File        *~
            * #03 ! LOTMYYYY ! Lot Movement Master Archive File         *~
            * #04 ! LOTDYYYY ! Lot Movement Detail Archive File         *~
            * #05 ! SYSFILE2 ! Caelus Management System Information     *~
            * #06 ! ARCHVREC ! Archived History File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "LOTMVMNT",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  88,                     ~
                        alt key  1, keypos =   45, keylen =  88          ~

            select #02, "LOTMVDTL",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  96,                     ~
                        alt key  1, keypos =   97, keylen =  96

            select #03, "LOTMYYYY",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  88,                     ~
                        alt key  1, keypos =   45, keylen =  88          ~

            select #04, "LOTDYYYY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  96,                     ~
                        alt key  1, keypos =   97, keylen =  96

            select #05, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =     1, keylen =  20

            select #06, "ARCHVREC",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  30

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))

            /* *** ASKUSER on File Mode *** */
L03000:     keyhit% = 2% /* Window in the Bottom */
            call "ASKUSER" (keyhit%, "*** EXCLUSIVE FILES OPTION ***",   ~
                "Press PF1 to Archive Files in an EXCLUSIVE MODE ",      ~
                "Press PF2 to Archive Files in a SHARED MODE ",          ~
                "Press PF16 to Abort")
            if keyhit% = 16% then exit_program
            if keyhit% <> 1% and keyhit% <> 2% then L03000   /* Try Again */
            if keyhit% =  1% then mode$ = "IO   "
            if keyhit% =  2% then mode$ = "SHARE"

            /* ** Check to see if LOCMVMNT/LOTMVDTL exist !!!! ** */
            lotmrec% = 160%  :   lotdrec% = 200%
            rslt$(1%), rslt$(2%) = "REQUIRED"
            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            if f2%(1%) <> 0% or f2%(2%) <> 0% then no_file
               get rslt$(1%) using L03170, lotmrec%    /* Get Record Size */
               get rslt$(2%) using L03170, lotdrec%
L03170:                     FMT POS(17), BI(4)

               close #01 : close #02
               call "OPENFILE" (#01, mode$, f2%(01%), rslt$(01%), axd$)
               call "OPENFILE" (#02, mode$, f2%(02%), rslt$(02%), axd$)
               call "OPENCHCK" (#06, fs%(06%), f2%(06%), 100%, rslt$(06%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            endfieldnr%  = 2%              /* Number of display fields */
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%)      = "LOTARCHV: " & str(cms2v$,,8%)
            str(header$, 1%,20%) = "From               "
            str(header$,21%,20%) = "To                 "
            header2$             = "Master File Detail File"
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            gosub initialize_results

            for fieldnr% = 1% to endfieldnr%
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
                  if keyhit% <> 16% then L11140
                     if end%  =  1% then exit_program
                     gosub initialize_results
                     goto archive_data
L11140:           if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  endfieldnr% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
                  end% = 0%
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  lastfieldnr% = fieldnr%
            goto L11150

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        archive_data

*        Get "DO YOU WANT TO PROCEED (Y/N) INFO"
            safeguard$ = " "
L19100:     gosub safe_guard_screen  /* Do You really want to do this ? */
                if keyhit%  =  1%  then editpg1
                if keyhit% <>  0%  then L19100
            if safeguard$ <> "YES" then editpg1

            gosub create_archive_files
            gosub loop_and_write_records
            gosub close_archive_files
            gosub records_total
            gosub write_to_archvrec
            goto  editpg1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Archive Date Range     */~
                              L20400          /* Archive File Year      */
            return

L20100: REM Def/Enable Archive Date Range          FMARCHDATE$
            if fmarchdate$ = " " or fmarchdate$ = blankdate$ then ~
               fmarchdate$ = "ALL"
            return

L20400: REM Def/Enable Archive File Year           ARCHYEAR$
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
                      archyear$

        return


        initialize_results
            hits%, eligibles%, writtens%, end% = 0%
            hitsd%, writdtls% = 0%
            hits$      = "# of Records Hit      =     " & "         0"
            eligibles$ = "# of Records Eligible =     " & "         0"
            writtens$  = "# of Records Written  =     " & "         0"
            hitsd$      = "         0"
            writtensd$  = "         0"
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
            gosub close_archive_files
            return clear all
            goto inputmode

        close_archive_files
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
                 if f2%(3%) <> 0% then L29805
            readkey$ = hex(00)
            call "READ102" (#03, readkey$, f1%(3%))
                 if f1%(3%) = 0% then  call "FILEBGON" addr(#03)         ~
                                 else  close #03

L29805:     call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
                 if f2%(04%) <> 0% then return
            readkey$ = hex(00)
            call "READ102" (#04, readkey$, f1%(4%))
                 if f1%(4%) = 0% then  call "FILEBGON" addr(#04)         ~
                                 else  close #04
            return

        REM *************************************************************~
            *           D O   A R C H I V E                             *~
            *-----------------------------------------------------------*~
            * Perform Archiving (LOTMVMNT->LOTMYYYY, LOTMVDTL->LOTDYYYY)*~
            *************************************************************

        loop_and_write_records

            call "SHOSTAT" ("Transfering Data to Archive Files")

            readkey$ = hex(00)
            call "READ104" (#1, readkey$, f1%(1%))
            goto L30160

        loop_lotmvmnt_file
            call "READNEXT" (#1, f1%(1%))
L30160:         if f1%(1%) = 0% then return

            get #1, str(lotmrec$)
            hits% = hits% + 1%

            /* Check Date Range */
            if fmarchdate$ = "ALL" then L30260           /* Don't Test */
            if str(lotmrec$, 147%, 6%) > unftoarchdate$  or              ~
               str(lotmrec$, 147%, 6%) < unffmarchdate$                  ~
                    then loop_lotmvmnt_file
L30260:     eligibles% = eligibles% + 1%

            gosub write_delete_lotmvmnt_record      /* Master Movement */
            gosub archive_pertinent_lotmvdtl_records  /* Detail Mvnmnt */

            goto loop_lotmvmnt_file                   /* Back for More */

         /* *** END of MAIN Archiving Sub *** */

         /* ***** Begin Misc. Subs to Support MAIN Archiving ***** */

        write_delete_lotmvmnt_record
            put #3, str(lotmrec$)
            write #3, eod goto L30430
            writtens% = writtens% + 1%

            /* Delete Archived Record from LOTMVMNT file */
L30430:     call "DELETE" (#01, str(lotmrec$,,88), 88%)

            return           /* Get the Next HNYMVMNT record */

        archive_pertinent_lotmvdtl_records
            plowkey$ = str(lotmrec$, , 88%)
            str(plowkey$,89%,8%) = all(hex(00))
          plowloop_on_lotmvdtl_file
            call "PLOWNEXT"(#2, plowkey$, 88%, f1%(2%))
                if f1%(2%) = 0% then reverse_plowloop_on_lotmvdtl_file
            get #2, str(lotdrec$)
            hitsd% = hitsd% + 1%
            gosub write_delete_lotmvdtl_record
            goto plowloop_on_lotmvdtl_file

          reverse_plowloop_on_lotmvdtl_file
            call "PLOWALTS"(#2, plowkey$, 1%, 88%, f1%(2%))
                if f1%(2%) = 0% then return    /* End Detail Looping */
            get #2, str(lotdrec$)
            hitsd% = hitsd% + 1%
            gosub write_delete_lotmvdtl_record
            goto  reverse_plowloop_on_lotmvdtl_file

        write_delete_lotmvdtl_record
            put #4, str(lotdrec$)
            write #4, eod goto L30720
            writdtls% = writdtls% + 1%

            /* Delete Archived Record from LOTMVMNT file */
L30720:     call "DELETE" (#02, str(lotdrec$,,96%), 96%)

            return           /* Get the Next HNYMVDTL record */

        records_total
            end% = 1%
            convert hits%      to str(hits$     ,29%,10%), pic(##########)
            convert eligibles% to str(eligibles$,29%,10%), pic(##########)
            convert writtens%  to str(writtens$ ,29%,10%), pic(##########)

            convert hitsd%      to hitsd$      , pic(##########)
            convert writdtls%   to writtensd$  , pic(##########)
            return

        write_to_archvrec   /* Write record to ARCHVREC file  */
            end% = 1%
            if writtens% = 0% then return/* No record written to archive
                                file, so don't update history file */

            /* 'DATE' gets treated as an archive date here */
            put #06 using L31040, "LOTMVMNT", lotmprname$, date, time,    ~
                  unffmarchdate$, unftoarchdate$, userid$, writtens%, " "

L31040:          FMT CH(8), CH(8), CH(6), CH(8), CH(6), CH(6), CH(3),    ~
                     BI(4), CH(101)
            write #06
            return

        view_archvrec
          header$(3) = hex(80) & "Inventory Detail Archive History"
          header$(1) = "  File      Start Date  End Date  Archived Date" ~
                     & "  ID   Archived Name    # of Rec"

          descr_m(01) =    1.08  : descr_m(02) = 0001.0
          descr_m(03) =   31.061 : descr_m(04) = 0012.0
          descr_m(05) =   37.061 : descr_m(06) = 0023.0
          descr_m(07) =   17.061 : descr_m(08) = 0035.0
          descr_m(09) =   43.03  : descr_m(10) = 0048.0
          descr_m(11) =    9.08  : descr_m(12) = 0055.0
          descr_m(13) =   46.04  : descr_m(14) =-0068.10

          plowkey$ = "LOTMVMNT" & hex(00)
          inc(1) = 1.08 : inc$(1) = "LOTMVMNT"

          call "PLOWCODE" (#06, plowkey$, " ", 9000%, 0.30, f1%(06%),    ~
                header$(), 0, 0, inc(), inc$(), "D", "Y", #03, descr_m())

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
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if end% = 1% and fieldnr% = 0%  then                       ~
                               str(pf$(3%),64%,16%) = "(16)Exit Program"

              on fieldnr% gosub L40095,         /* Archive Date Range   */~
                                L40100          /* Archive File Year    */
              goto L40115

L40095:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40100:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40115:     accept                                                       ~
               at (01,02),                                               ~
                  "Archiving selection criteria",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),str(header$, 1%, 20%)   , ch(20),~
               at (06,52), fac(hex(ac)),str(header$,21%, 20%)   , ch(20),~
                                                                         ~
               at (07,02), "Archive Date Range",                         ~
               at (07,30), fac(lfac$( 1%)), fmarchdate$         , ch(10),~
               at (07,52), fac(lfac$( 1%)), toarchdate$         , ch(10),~
                                                                         ~
               at (08,02), "Archive File Year",                          ~
               at (08,30), fac(lfac$( 2%)), archyear$           , ch(04),~
                                                                         ~
               at (14,29), fac(hex(a4)),   str(header2$, 1%,11%),        ~
               at (14,44), fac(hex(a4)),   str(header2$,13%,11%),        ~
               at (15,02), fac(hex(84)),   hits$                , ch(38),~
               at (16,02), fac(hex(84)),   eligibles$           , ch(38),~
               at (17,02), fac(hex(84)),   writtens$            , ch(38),~
               at (15,45), fac(hex(84)),   hitsd$               , ch(10),~
               at (17,45), fac(hex(84)),   writtensd$           , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,65), fac(hex(84)),   str(pf$(3%),64%,)    ,        ~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40295
                  call "MANUAL" ("LOTARCHV") : goto L40115

L40295:        if keyhit% <> 15% then L40310
                  call "PRNTSCRN" : goto L40115

L40310:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if end% = 1% and fieldnr% = 0% then return
        if edit% = 2% then L40410     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field   (10" &       ~
                     ")See Archive History   (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0affff0dff0f1000)
            if fieldnr% = 1% then L40390
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40390:     if fieldnr% >= 2% then L40400
                str(pf$(2%),18%,18%) = " ": str(pfkeys$, 4%,1%) = hex(ff)
L40400:     return

L40410: if fieldnr% > 0% then L40455  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                     (10" &       ~
                     ")See Archive History   (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Archive     "
            pfkeys$ = hex(01ffffffffffffffff0affff0dff0f1000)
            return
L40455:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                     (10" &       ~
                     ")See Archive History   (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffff0affff0dff0fff00)
            return

        REM *************************************************************~
            *            S A F E G U A R D   S C R E E N   1            *~
            * --------------------------------------------------------- *~
            * Safeguards the accidental purging of data by asking if he *~
            * really wants to purge it.                                 *~
            *************************************************************
        safe_guard_screen
            getback$            = "(1)Return"
            str(dateline$,,60%) = "Enter Safeguard Word"

L42080:     accept                                                       ~
               at (01,02),                                               ~
                  "Archiving Invoiced/Misc. Items"              ,        ~
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

               if keyhit% <> 13% then L42400
                  call "MANUAL" ("LOTARCHV")
                  goto L42080

L42400:        if keyhit% <> 15% then return
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
            fmarchdate%, toarchdate% = 0%
            if fmarchdate$ = "ALL" then return
            if fmarchdate$ <> " " and fmarchdate$ <> blankdate$ and ~
              (toarchdate$ = " " or toarchdate$ = blankdate$)  then ~
               toarchdate$ = fmarchdate$
            call "DATEOKC" (fmarchdate$, fmarchdate%, errormsg$)
                 if errormsg$ <> " " then return
            call "DATEOKC" (toarchdate$, toarchdate%, errormsg$)
                 if errormsg$ <> " " then return
            unffmarchdate$ = fmarchdate$ : unftoarchdate$ = toarchdate$
            call "DATUFMTC" (unffmarchdate$)
            call "DATUFMTC" (unftoarchdate$)
            if (unffmarchdate$ >  unftoarchdate$)                        ~
                then errormsg$ = "From may not be greater then To."
            return

L50400: REM Test for Archive File Year            ARCHYEAR$
            if archyear$ = " " then                                      ~
               errormsg$ = "Archive File Year Name is Require."
            if len(archyear$) < 4 then                                   ~
               errormsg$ = "Length may not be less than 4."
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

        create_archive_files
            call "SHOSTAT" ("Creating or Opening Archive Files")
            lotmprname$="LOTM" & archyear$
            call "PUTPRNAM" addr(#3, lotmprname$)
            lotdprname$="LOTD" & archyear$
            call "PUTPRNAM" addr(#4, lotdprname$)
            lotmrec% = max(1%, lotmrec%/2%)
            lotdrec% = max(1%, lotdrec%/2%)
            call "OPENCHCK"(#3, fs%(3%), f2%(3%), 0%      , rslt$(3%))
                 if f2%(3%) <> 0% then L60200 else L60210
L60200:     call "OPENCHCK"(#3, fs%(3%), f2%(3%), lotmrec%, rslt$(3%))
L60210:     call "OPENCHCK"(#4, fs%(4%), f2%(4%), 0%      , rslt$(4%))
                 if f2%(4%) <> 0% then L60230 else return
L60230:     call "OPENCHCK"(#4, fs%(4%), f2%(4%), lotdrec%, rslt$(4%))

            close #03 : close #04
            call "OPENFILE" (#03, mode$, f2%(03%), rslt$(03%), axd$)
            call "OPENFILE" (#04, mode$, f2%(04%), rslt$(04%), axd$)
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
