rem CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
    *                                                           *~
    *   GGG   EEEEE  N   N   SSS   L       CCC   TTTTT          *~
    *  G      E      NN  N  S      L      C   C    T            *~
    *  G GGG  EEEE   N N N   SSS   L      C        T            *~
    *  G   G  E      N  NN      S  L      C   C    T            *~
    *   GGG   EEEEE  N   N   SSS   LLLLL   CCC     T            *~
    *                                                           *~
    *-----------------------------------------------------------*~
    * GENSLCT  - Generates BASIC Code for select statements     *~
    *            and Open File CAlls.  (Used by GENPGM).        *~
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
    * 05/23/83 ! ORIGINAL                                 ! ECR *~
    * 01/07/86 ! Cosmetic Changes                         ! LDJ *~
    * 05/20/88 ! Put leading zero on file channel < 10    ! MJB *~
    * 03/12/96 ! UNIXed                                   ! ERN *~
    * 09/12/97 ! Lower Case all output, line up select out! DER *~
    CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC



        sub "GENSLCT" (#1, filenames$(), filedescr$(), fileno$(),        ~
                           maxfiles%, database$)

        dim                                                              ~
            altdup$4,                    /* string "dup(,)" if dups    */~
            altkey$(32)12,               /* ALTERNATE KEY INFORMATION  */~
            altnum$3,                    /* alternate key number       */~
            altstr$3,                    /* string "alt"               */~
            database$8,                  /* DATA LIBRARY NAME - READFDR*/~
            filedescr$(60)40,            /* FILE DESCRIPTION LIST      */~
            fileorg$8,                   /* FILE ORGANIZATION FOR SLCT */~
            filename$8,                  /* FILE NAME FOR READFDR      */~
            filenames$(60)8,             /* FILE NAME LIST             */~
            fileno$(60)2,                /* FILE CHANNEL NUMBER LIST   */~
            filetype$1,                  /* FILE TYPE FROM READFDR     */~
            filqot$11,                   /* file name in quotes        */~
            is_dup$1,                    /* blank or "D" if dup        */~
            keylen$4,                    /* KEY LENGTH                 */~
            keypos$5,                    /* KEY POSITION               */~
            keystr$3,                    /* string "key"               */~
            lend$1,                      /* Line End (tilde or space)  */~
            lspc$1,                      /* space for assign to lend$  */~
            ltil$1,                      /* tilde for assign to lend$  */~
            out$255,                     /* LINE BUFFER                */~
            outline$255,                 /* LINE TO WRITE OUT          */~
            recordtype$6,                /* RECORD TYPE FROM READFDR   */~
            recsize$5,                   /* RECORD SIZE                */~
            selnum$4,                    /* select number              */~
            temp$4,                      /* temp var                   */~
            volume$6,                    /* VOLUME  NAME FOR READFDR   */~
            vtoc$22                      /* VTOC ENTRY FOR FIND        */~


        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto cms_start
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************

cms_start
        lspc$  = hex(20)
        ltil$  = hex(7E)
        lend$  = ltil$

        put out$, using L12080, lend$          /* rem************* */
        gosub'180(0%)
        put out$, using L12110, lend$          /* * SELECT FILES * */
        gosub'180(0%)
        put out$, using L12170, lend$          /* *-+----+-------* */
        gosub'180(0%)
        put out$, using L12200, "#", lend$     /* *File#! PRName * */
        gosub'180(0%)
        put out$, using L12170, lend$          /* *-+----+-------* */
        gosub'180(0%)

        if maxfiles% = 0% then L10500

        /* Table entries  */
        for file% = 1 to maxfiles%
            tran (fileno$(file%), "0 ") replacing
            selnum$ = "#" & fileno$(file%)
            put out$, using L12260, selnum$, filenames$(file%), filedescr$(file%), lend$
            gosub'180(10%)
        next file%

L10500: put out$, using L12290, lend$    /* ***************  */
        gosub'180(0%)
        put out$, using L12320, lend$    /* *             *  */
        gosub'180(0%)
        put out$, using L12350, " "      /* *File select  *  */
        gosub'180(0%)
        put out$, using L12380, " "      /* Blank line       */
        gosub'180(0%)

        if maxfiles% = 0% then write_shostat

        /* Generate the actual select statements */
        for file% = 1% to maxfiles%
            recsize$ = "     "
            altstr$  = "   "
            keystr$  = "   "
            altnum$  = "   "
            keypos$  = "     "
            keylen$  = "    "
            altdup$  = "    "
            is_dup$  = " "

            gosub'200(filenames$(file%))   /* Get file info */

            selnum$ = "#" & fileno$(file%) & ","
            filqot$ = hex(22) & filenames$(file%) & hex(22) & ","

            if filetype$ = "I" or filetype$ = "A" then recsize$ = recsize$ & ","

            put out$, using L12420, selnum$, filqot$, lend$
            gosub'180(0%)
            if filetype$ <> "I" and filetype$ <> "A" then lend$ = lspc$
            put out$, using L12450, recordtype$, fileorg$, recsize$, lend$
            gosub'180(10%)
            lend$  = ltil$
            if filetype$ <> "I" and filetype$ <> "A" then get_next_file

            keypos$ = keypos$ & ","

            if filetype$ = "A"  then keylen$ = keylen$ & "," ~
                                else lend$   = lspc$

            put out$, using L12500, altstr$, keystr$, altnum$, ~
                                    keypos$, keylen$, altdup$, lend$
            gosub'180(10%)
            lend$  = ltil$
            if filetype$ <> "A"  then get_next_file
            if alterr% = 0% then proc_alt_keys

            lend$ = lspc$
            put out$ using L12500, "/* ", "err", "fdr", "  ", alterr%, "*/", lend$
            gosub'180(10%)
            lend$  = ltil$
            altkeycount% = altkeycount% - 1%
            if altkeycount% <= 0% then get_next_file

          proc_alt_keys
            altstr$  = "alt"
            keystr$  = "key"

            for akey% = altkeycount% to 1% step -1%
                is_dup$    = " "
                altdup$    = "    "
                altkeynr%  = val(str(altkey$(akey%), 1%, 2%), 2)
                altkeypos% = val(str(altkey$(akey%), 3%, 2%), 2)
                altkeylen% = val(str(altkey$(akey%), 5%, 2%), 2)

                convert altkeynr%  to altnum$, pic(##)
                convert altkeypos% to keypos$, pic(####)
                convert altkeylen% to keylen$, pic(###)

                is_dup$ = str(altkey$(akey%), 7%, 1%)

                altnum$ = altnum$ & ","
                keypos$ = keypos$ & ","
                if akey% > 1% or  is_dup$ = "D" then keylen$ = keylen$ & ","
                if                is_dup$ = "D" then altdup$ = "dup"
                if akey% > 1% and is_dup$ = "D" then altdup$ = altdup$ & ","
                if akey% = 1% then lend$ = lspc$

                put out$, using L12500, altstr$, keystr$, altnum$, ~
                                        keypos$, keylen$, altdup$, lend$

                gosub'180(10%)
                lend$ = ltil$
                /* alt on 1st alt key only */
                altstr$ = "   "
            next akey%

          get_next_file
            put out$, using L12380, " "  /* blank line */
            gosub'180(0%)
        next file%

      write_shostat
        /* generate the openfile calls */
        put out$, using L12580, " "
        gosub'180(10%)
        put out$, using L12380, " "
        gosub'180(10%)

        if maxfiles% = 0% then end_genslct

        for file% = 1% to maxfiles%
            selnum$ = "#" & fileno$(file%) & ","
            put out$, using L12600, selnum$, fileno$(file%), ~
                                    fileno$(file%), fileno$(file%)
            gosub'180(10%)
        next file%

      end_genslct
        put out$, using L12380, " "
        gosub'180(0%)  /* Blank Line */

    end


rem *************************************************************~
    *             F O R M A T   S T A T E M E N T S             *~
    *                                                           *~
    * FORMAT STATEMENTS FOR ALL THE CODE GENERATION ROUTINES    *~
    * WILL BE FOUND HERE.                                       *~
    *************************************************************

L12080: %rem *************************************************************#
L12110: %    *                  S E L E C T   F I L E S                  *#
L12170: %    *-----+----------+------------------------------------------*#
L12200: %    *File#!  PRName  ! File Description                         *#
       /*    *-----+----------+------------------------------------------*  */
L12260: %    * ### ! ######## ! ######################################## *#
L12290: %    *************************************************************#
L12320: %    * File Selection and Open Calls                             *#
L12350: %    *************************************************************#

L12380: %#

L12420: %    select #### ###########                                      #
L12450: %           ######  ########       recsize = #####                #

L12500: %       ### ### ### keypos = ##### keylen  =  ##### ####          #


L12580: %   call "SHOSTAT" ("Opening Files, One Moment Please")#

L12600: %   call "OPENCHCK" (#### fs%(##%), f2%(##%), 0%, rslt$(##%))


rem *************************************************************~
    *        W R I T E   O U T   T H E   L I N E                *~
    *************************************************************
deffn'180(temp%)
        put   #1  using L13130, outline$
        write #1, using L13130, out$
L13130:     fmt ch(255)
        out$  = " "
        temp% = temp%
return


rem *************************************************************~
    *        R O U T I N E   F O R   F I L E   I N F O          *~
    *                                                           *~
    * GETS  ALL SORTS OF INFORMATION ABOUT THE FILE MENTIONED IN*~
    * THE ABOVE PRNAME AND WRITES UP ABOUT IT IN THE SELECT     *~
    * STATEMENT.                                                *~
    *************************************************************
deffn'200(filename$)
    recsize$     = " "
    filetype$    = "I"
    fileorg$     = "indexed,"
    recordtype$  = "varc,"
    altkeycount% = 0%
    fcount%      = 1%
    init(" ") altkey$()

    /* First, find out the database volume */
    call "FIND" addr (filename$, database$, "?     ", 1%, fcount%, vtoc$)
    volume$      = str(vtoc$, 1%, 6%)
    if volume$ = " " then exit_200  /* No data on any disk */

    /* Next, find out if the file is there */
    temp$ = " "
    call "READFDR" addr(filename$, database$, volume$, 0%, "FT", temp$, err%)
    if err% <> 0% then exit_200     /* Out if no file */
    recordtype$ = " "
    call "READFDR" addr(filename$, database$, volume$, 0%,   ~
                            "AC", altkeycount%,              ~
                            "FT", filetype$,                 ~
                            "KP", primarykeypos%,            ~
                            "KS", primarykeylen%,            ~
                            "RS", recordsize%,               ~
                            "RT", recordtype$,               ~
                            err%)
    convert min(recordsize%,    2048%) to recsize$, pic(####)
    convert min(primarykeypos%, 2040%) to keypos$,  pic(####)
    convert min(primarykeylen%, 255% ) to keylen$,  pic(###)

    /* Handle file organization */
    if filetype$ = "C" then fileorg$ ="consec,"
    if filetype$ = "I" then fileorg$ ="indexed,"
    if filetype$ = "A" then fileorg$ ="indexed,"
    if filetype$ = "P" then fileorg$ ="printer,"
    if filetype$ = "O" then fileorg$ ="consec,"
    if filetype$ = "L" then fileorg$ ="consec,"
    if filetype$ = "W" then fileorg$ ="consec,"

    /* Handle record type field */
    recordtype$ = "fixed,"
    if fileorg$ = "indexed," then recordtype$ = "varc,"

    /* Handle key information for alt indexed file */
    if filetype$ <> "A" then exit_200

    call "READFDR" addr (filename$, database$, volume$,  ~
                         0%, "AX", altkey$(), alterr%)
  exit_200
return

