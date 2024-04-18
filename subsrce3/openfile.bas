        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   OOO   PPPP   EEEEE  N   N  FFFFF  IIIII  L      EEEEE   *~
            *  O   O  P   P  E      NN  N  F        I    L      E       *~
            *  O   O  PPPP   EEEE   N N N  FFFF     I    L      EEEE    *~
            *  O   O  P      E      N  NN  F        I    L      E       *~
            *   OOO   P      EEEEE  N   N  F      IIIII  LLLLL  EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * OPENFILE - Caelus primary file open subroutine.  Opens    *~
            *            the data file for the passed-in file channel   *~
            *            using the Select PRNAME as the filename to     *~
            *            open.  Library is extracted from the current   *~
            *            INLIB and Volume is obtained by searching the  *~
            *            currently mounted volumes for the current file *~
            *            in the current library.  Refer to the Caelus   *~
            *            Technical Reference Manual for General Purpose *~
            *            Subroutines to get more detail and calling     *~
            *            syntax.                                        *~
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
            * 01/30/83 ! ORIGINAL                                 ! GLW *~
            * 05/12/83 ! ADDED "FIND" FOR VOLUME, OUTVOL FOR OUTPT! ECR *~
            * 04/05/84 ! MODIFIED TO 'FIND' ALL VOLUMES THE INLIB ! LDJ *~
            *          !   IS ON ON THE FIRST PASS THRU.          !     *~
            *          !   SUBSEQUENT CALLS ONLY CHECK ELEGIBLE   !     *~
            *          !   VOLUMES.                               !     *~
            * 05/02/84 ! ENHANCEMENTS TO 4/5/84 MODIFICATIONS TO  ! LDJ *~
            *          !   USE THE INVOL & OUTVOL DEFAULTS FIRST  !     *~
            *          !   IF PRESENT 'AND' IF THE DESIRED INLIB  !     *~
            *          !   IS ON THOSE VOLUMES.                   !     *~
            * 10/21/85 ! Removed all code related to the AXD$ for ! LDJ *~
            *          !   stuffing alt key info back into a      !     *~
            *          !   file's UFB (no longer needed per KAB). !     *~
            * 09/24/86 ! Will no longer Open files left marked as ! LDJ *~
            *          !   being open after a system crash.       !     *~
            *          !   Instead CANCEL will be invoked if this !     *~
            *          !   or other serious errors are detected.  !     *~
            * 08/13/90 ! Basic 4.3 & SSL Compatibility            ! KAB *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 03/01/93 ! PRR 12462 Cleaned up REMs, /*'s, CMS2V$s.! JIM *~
            * 06/17/97 ! Long standing bad branch (L09900)        ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        REM *************************************************************~
            * Note- Efficient use of disk space and program execution   *~
            *        speed under various system loads requires a proper *~
            *        setting of the DPACK/IPACK variables in Output mode*~
            *************************************************************

        sub "OPENFILE" (#1, mode$, f2%, returncode$, axd$)

        dim                                                              ~
            aid$1,                       /* Aid(s) Byte Receiver       */~
            mode$5,                      /* INPUT, OUTPUT, SHARE, ETC. */~
            returncode$20,               /* Error message if any       */~
            filename$8,                  /* Filename                   */~
            library$ 8,                  /* Library                    */~
            lastlib$ 8,                  /* Libry where last open done */~
            volume$  6,                  /* Volume                     */~
            invol$6,                     /* Default input volume name  */~
            outvol$6,                    /* Default output volume name */~
            l$2,                         /* Locator string for "SEARCH"*/~
            i%(1),                       /* Locator integer ""   ""    */~
            dpack$3,                     /* Passed DPACK factor        */~
            ipack$3,                     /* Passed IPACK factor        */~
            filetype$1,                  /* File type from "READFDR"   */~
            recsize$4,                   /* Actual File Record Size    */~
            space$8,                     /* Passed file open size      */~
            status1$3,                   /* Converted F2% value        */~
            status2$3,                   /* Converted F2% value        */~
            ufbf1$1,                     /* UFB file open flag.        */~
            ufborg$1,                    /* Expected file organization */~
            ufbrecsize$2,                /* Calling Pgm record size    */~
            userfiletype$1,              /* Calling Pgm file type      */~
            userrecsize$4,               /* Program Select Record Size */~
            modes$35,                    /* List of permissable modes  */~
            axd$4,                       /* Alt Key info pointer       */~
            fgbg$1,                      /* Task type, (fore/background*/~
            vtoc$(10)22                  /* Used to find volume of file*/


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

            /************************************************************
            *     Check to See if File Already Open on this Channel     *
            ************************************************************/
           filetype$ = " "
           ex%, rc%, rs%, userrecsize% = 0%
           oldf2% = f2%

            call "GETUFBS1" addr(#1, f1%)

                 if f1% = 0% then L07675
                      /* IF UFBF1 FLAG LAST BIT = 1 THEN FILE NOW OPEN */
                 f2% = 0
                 returncode$ = "FILE CURRENTLY OPEN"
                 end                     /* File Already Open Fella!   */

            /************************************************************
            *     Get File Parameter Name & Other Info From UFB         *
            ************************************************************/

L07675:    call "GETPRNAM" addr(#1, filename$)
           call "GETUFBRS" addr(#1, ufbrecsize$)
                   userrecsize% = val(ufbrecsize$,2)
           call "GETUFBOR" addr(#1, ufborg$)

                userfiletype$=" "
                ufbf1$ = ufborg$ and hex(02)
                if ufbf1$ <> hex(02) then L07750
                   userfiletype$ = "I"
                   goto L09110
L07750:         ufbf1$ = ufborg$ and hex(01)
                if ufbf1$ <> hex(01) then L07790
                   userfiletype$ = "C"
                   goto L09110
L07790:         ufbf1$ = ufborg$ and hex(04)
                if ufbf1$ <> hex(04) then L07825
                   userfiletype$ = "W"
                   goto L09110
L07825:         ufbf1$ = ufborg$ and hex(08)
                if ufbf1$ <> hex(08) then L07870
                   userfiletype$ = "R"
                   goto L09110

L07870:            returncode$ = "WRONG FILE TYPE :XX"
                   hexunpack ufborg$ to str(returncode$,18,2)
                   f2% = 996
                   goto end_display

            /************************************************************
            *  If F2% passed in is less than zero then force reload of  *
            *  database volumes array.                                  *
            ************************************************************/
L09110:     if oldf2% < 0 then lastlib$ = " " /* force reload of array */

            /************************************************************
            *  The following steps are performed only on the first pass *
            *  through this routine (the first time the calling program *
            *  issues the call to this subroutine). The call to 'FIND'  *
            *  retrieves all the volumes where the current 'INLIB' is   *
            *  found.  There is no dependence on the INVOL being set.   *
            ************************************************************/
            call "EXTRACT" addr("IL", library$, "IV",invol$,"OV",outvol$,~
                            "TT",fgbg$)  /* Extract data library name. */
            if library$ > " " then L09420
               f2% = 990%
               returncode$ = "DATABASE NOT SET!"
               goto end_display
L09420:     if library$ = lastlib$ then L10620  /* skip reload          */
            lastlib$ = library$

            count%  = 10%                /* Find up to 10 volumes this */
            volume$ = "?"                /*   Library is on.           */
            vtoc$() = " "
            y% = 0%
            if invol$ = " " then L09830
                y% = y% + 1%
                vtoc$(y%) = invol$
L09830:     if outvol$ = " " or outvol$ = invol$ then L09900
                y% = y% + 1%
                vtoc$(y%) = outvol$

L09900:     count% = count% - y%

            call "FIND" addr ("        ", library$, volume$, 1%, count%, ~
                              str(vtoc$(), y%*22%+1%))
                                         /* Looks for all volumes with */
                                         /* this library-puts in VTOC$()*/

            if count% > 0% then L10160
               vtoc$() = " "
               vtoc$(1%) = outvol$
               goto L10520

L10160:     if invol$ = " " then L10340
                search str(vtoc$(),y%*22%+1%,) = invol$ to i%() step 22
                if i%(1) > 0% then L10280
                    vtoc$() = str(vtoc$(),23%)
                    y% = y% - 1%
                    goto L10340           /* Remove invalid IN VOLUME   */
L10280:         i%(1%) = i%(1%) + 22% * y%
                str(vtoc$(),i%(1%)) = str(vtoc$(),i%(1%)+22%)
                                         /* Remove duplicate volume    */
L10340:     if outvol$ = " " or outvol$ = invol$ then L10620
                search str(vtoc$(),y%*22%+1%) = outvol$ to i%() step 22
                if i%(1) > 0% then L10440
                str(vtoc$(),(y%-1)*22%+1%) = str(vtoc$(),y%*22%+1%)
                goto L10620               /* Remove invalid OUT VOLUME  */
L10440:         i%(1%) = i%(1%) + 22% * y%
                str(vtoc$(),i%(1%)) = str(vtoc$(),i%(1%)+22%)
                                         /* Remove duplicate volume    */

L10520:     /************************************************************
            *  Using previously retrieved list of eligible volumes find *
            *  which volume the current file is on (if any) and get it's*
            *  file type for later type/mode checking.                  *
            ************************************************************/
L10620:     for x% = 1% to count%        /* For Each Volume . . .      */
                volume$ = str(vtoc$(x%),,6%)
                if volume$ = " " then L10720
                gosub L13300              /* Do a Call to 'READFDR'     */
                if f2% = 0%  then L11080  /*If file found then Exit Loop*/
L10720:     next x%

            volume$ = "?"                /* Last gasp attempt to find  */
            x% = 1%                      /* the file if it was created */
            vtoc$(10%) = " "             /* in the last 2 - 10 seconds!*/
            call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$(10))
            if vtoc$(10%) = " " then L10940  /* File definitely not found*/
                volume$ = vtoc$(10%)
                gosub L13300              /* Call 'READFDR' as above    */
                if f2% = 0% then L11080

L10940:     volume$ = outvol$            /* File not found, retrieve   */
                                         /* outvol in prep to create   */
                                         /* new file.                  */

            /************************************************************
            *       Open the File According to Mode and File Type       *
            ************************************************************/
L11080:     if mode$ = "OUTPT" or f2% <> 0% then L11240
            if userrecsize% >= rs% then L11240
               put str(returncode$,,20%) using L11160,                    ~
                                     "RECORD SIZES", userrecsize%, rs%
L11160:            FMT CH(12), PIC(###0), PIC(###0)
               f2% = 997%
               goto end_display

L11240:     modes$ = "INPUTOUTPTIO   EXTNDSHAREVALIDSPLIN"
            search modes$ = mode$ to l$ step 5%
            mode% =(val(l$,2) + 4%)/5%   /* Which Mode was it?         */

            on mode% goto L11480,         /* INPUT                      */~
                          L11700,         /* OUTPUT                     */~
                          L12320,         /* IO                         */~
                          L12504,         /* EXTEND                     */~
                          L12560,         /* SHARED                     */~
                          L13160,         /* 'FIND'                     */~
                          L11681          /* SPECIAL INPUT              */

                returncode$ = "UNKNOWN MODE:" & filetype$ &              ~
                                                   userfiletype$ & mode$
               f2% = 998%
               goto end_display

L11480:      /* Open the file in "INPUT" mode.                        */

                gosub L12960                 /* If file doesn't exist...*/

                open nogetparm #1,                                       ~
                     input,                                              ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$
                end

L11681:      /* Open the file in "SPECIAL INPUT" mode.                 */

                gosub L12960                 /* If file doesn't exist...*/

                open nogetparm #1,                                       ~
                     spclinp,                                            ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$
                end

L11700:      /* Open the file in Output mode.
                Note that we first have to see if file exists...
                Error Exit taken if it does UNLESS key word = OUTPTN. */

                if str(returncode$,1%,6%)="OUTPTN" or f2%<>0% then L11880
                     returncode$ = "FILE ALREADY EXISTS!"
                     f2% = 995%
                     goto end_display

L11880:         get str(returncode$,7%,14%) using L11920,                 ~
                    space$, dpack$, ipack$
L11920:             FMT CH(8), CH(3), CH(3)
                dpack% = 50%:convert dpack$ to dpack%, data goto L11960
L11960:         ipack% = 50%:convert ipack$ to ipack%, data goto L11980
L11980:         space% = 100%:convert space$ to space%,data goto L12000
L12000:         dpack% = max(50%, min(100%, dpack%))
                ipack% = max(50%, min(100%, ipack%))
                space% = max(100%, min(250000%, space%))

                open nogetparm #1,                                       ~
                     output,                                             ~
                     space = space%,                                     ~
                     dpack = dpack%, ipack = ipack%,                     ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

                gosub L13380
                goto  L13160      /* Fill in the blanks .... */
            /*  END   */

L12320:     /*  Open the file in IO mode.   */

                gosub L12960    /* does file exist? check file types    */

                open nogetparm #1,                                       ~
                     io,                                                 ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$
                end

L12504:     /*  Open a Consec file in EXTEND mode.  Note it doesn't
                work for indexed files, and doesn't open a print file. */

                if userfiletype$ <> "C" then userfiletype$ = "K"
                      /* Force mismatch if not consecutive. */
                gosub L12960    /* does file exist? check file types    */

                open nogetparm #1,                                       ~
                     extend,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$
                end

L12560:     /*  Open the desired file as a shared mode file.  Note that
                BASIC doesn't allow sharing consecutive files.
                (The exception isn't worth noting).                    */

                gosub L12960    /* does file exist? check file types    */
                if userfiletype$ <> "I" then L13060

                open nogetparm #1,                                       ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

                if fs(#1) <> "01" then end
                   returncode$ = "FILE NEEDS REORG " & fs(#1)
                   close #1
                   f2% = 994%
                   goto end_display

L12960:     /*  Check to See if File Now Exists, if not then error exit.
                also checks file type against type expected by program.*/
                if f2% <> 0% then L13600
                if userfiletype$ = filetype$ then L13180
                if userfiletype$ = "I" and filetype$ = "A" then L13180
L13060:         returncode$ = "FILE TYPES!:" & filetype$ &  " " &        ~
                                                   userfiletype$ & mode$
                f2% = 999%
                goto end_display

L13160:     /*  Locate mode, set return string to actual file info  */
            gosub L13180
            end
L13180:         returncode$ = str(library$,1%,8%) &                      ~
                              str(volume$,1%,6%)  &                      ~
                              str(filetype$,1%,1%) &                     ~
                              bin(ex%,1) & bin(rc%,4)
                return

L13300:     /************************************************************
            *  Do a call to 'READFDR' to; 1) Check if File exists       *
            *                             2) If exists get file type    *
            ************************************************************/
L13380:     call "READFDR" addr (filename$, library$, volume$, 0%,       ~
                    "FT", filetype$, "EA", ex%, "RC", rc%, "RS", rs%, f2%)
                             /* We need to get the file type so that we
                                can inhibit stuff that might cause errors
                                such as opening a consec file in SH mode.
                                just to be underhanded...              */
            if f2% = 0% or f2% = 4% or f2% = 16% or f2% = 20% then return
               put returncode$, using L13540, "READFDR ERROR = ", f2%
L13540:        FMT CH(16), PIC(###)
               goto end_display

L13600:  /* Error Message Here Because File Does Not Exist   */
            if str(returncode$,1%,8%) <> "REQUIRED" then L13700
               returncode$ = "REQ'D FILE NOT FOUND"
               f2% = 900%
               goto end_display
L13700:        returncode$ = "FILE NOT FOUND"
               end

            /************************************************************
            *  End File Open with Error Display and Print               *
            ************************************************************/

        end_display

            convert userrecsize% to userrecsize$, pic(####)
            convert rs% to recsize$, pic(####)
            convert oldf2% to status1$, pic(###)
            convert f2% to status2$, pic(###)

            call "GETPARM" addr("I ",              /* 'I' or 'R'       */~
                                "A",               /* 'A'cknowlege     */~
                                "OPENERR ",        /* PRNAME           */~
                                aid$,              /* PF Key receiver  */~
                                "0001",            /* Message id       */~
                                "OPNFIL",          /* Message issuer   */~
        /*                      " ",               /* Header Msg Text  */~
                                0%,                /* Message length   */~
                                "U",               /* UNDER LINE TEXT  */~
                  "File NOT OPENED Due to Unexpected ERROR (See Below):",~
                                52%,               /* Text length      */~
                                "R",               /* Row flag (rel)   */~
                                00%,               /* Row offset       */~
                                "A",               /* Column flag (abs)*/~
                                15%,               /* Column number    */~
                                "T",               /* Regular text     */~
                    "File Name: " & str(filename$),/* File in Error    */~
                                19%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                2%,                /* ROW increment    */~
                                "A",               /* Column flag (abs)*/~
                                30%,               /* Column number    */~
                                "T",               /* Regular text     */~
                    "  Library: " & str(library$), /* File Library     */~
                                19%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                01%,               /* Row increment    */~
                                "A",               /* Column flag (abs)*/~
                                30%,               /* Column number    */~
                                "T",               /* Regular text     */~
                    "   Volume: " & str(volume$),  /* File Volume      */~
                                17%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                01%,               /* Row increment    */~
                                "A",               /* Column flag (abs)*/~
                                30%,               /* Column number    */~
                                "T",               /* Regular text     */~
              "Select Type: " & str(userfiletype$),/* Prog Select Type */~
                                14%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                01%,               /* Row increment    */~
                                "A",               /* Column flag (abs)*/~
                                28%,               /* Column number    */~
                                "T",               /* Regular text     */~
                    "File Type: " & str(filetype$),/* Actual File Type */~
                                12%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                00%,               /* Row increment    */~
                                "R",               /* Column flag (rel)*/~
                                05%,               /* Column offset    */~
                                "T",               /* Regular text     */~
        "Program Record Size: "&str(userrecsize$), /* Select Rec Size  */~
                                25%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                01%,               /* Row increment    */~
                                "A",               /* Column flag (abs)*/~
                                20%,               /* Column number    */~
                                "T",               /* Regular text     */~
          "Actual File Rec Size: " & str(recsize$),/* Actual File Size */~
                                26%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                00%,               /* Row increment    */~
                                "R",               /* Column flag (rel)*/~
                                05%,               /* Column offset    */~
                                "T",               /* Regular text     */~
                        "OPEN Mode: " & str(mode$),/* Prog Select Type */~
                                16%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                01%,               /* Row increment    */~
                                "A",               /* Column flag (abs)*/~
                                30%,               /* Column number    */~
                                "T",               /* Regular text     */~
                  "Input Status: " & str(status1$),/* Prog Select Type */~
                                17%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                01%,               /* Row increment    */~
                                "A",               /* Column flag (abs)*/~
                                27%,               /* Column number    */~
                                "T",               /* Regular text     */~
                "Current Status: " & str(status2$),/* Prog Select Type */~
                                19%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                00%,               /* Row increment    */~
                                "R",               /* Column flag (rel)*/~
                                05%,               /* Column offset    */~
                                "T",               /* Regular text     */~
             "Status Message: " & str(returncode$),/* Prog Select Type */~
                                36%,               /* Text length      */~
                                "R",               /* Row flag (relativ*/~
                                01%,               /* Row increment    */~
                                "A",               /* Column flag (rel)*/~
                                25%,               /* Column offset    */~
                                "U",               /* Underlined Text  */~
        "Press PF15 to Print an Image of this Screen, Then Press RETURN t~
        ~o Continue",                               /* Prompt Message   */~
                                74%,               /* Text length      */~
                                "A",               /* Row flag (abs)   */~
                                24%,               /* ROW number       */~
                                "C",               /* Column flag (cen)*/~
                                00%,               /* ignored          */~
                                "E")               /* ENTER KEY ALLOWED*/

            if f2% = 997% or f2% = 900% then end /* Up to Caller to Fix*/
            call "CEXIT" addr("S","N")    /* Turn Off CANCEL Screen */
            call "CANCEL" addr("0001","OPNFIL",returncode$,20%)
