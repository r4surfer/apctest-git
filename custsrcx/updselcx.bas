        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  U   U  PPPP   DDDD    SSS   EEEEE  L       CCC   TTTTT   *~
            *  U   U  P   P  D   D  S      E      L      C        T     *~
            *  U   U  PPPP   D   D   SSS   EEEE   L      C        T     *~
            *  U   U  P      D   D      S  E      L      C        T     *~
            *   UUU   P      DDDD    SSS   EEEEE  LLLLL   CCC     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * UPDSELCT - Allows specification of which sessions are to  *~
            *            be included in this update and marks those     *~
            *            session records with the User's Logon ID.      *~
            *            Program returns the following status codes-    *~
            *            0- No Update; 1- Update in foreground;         *~
            *            2- Update in background; 3- Restarting (update *~
            *            in foreground assumed.                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/22/86 ! Original                                 ! ERN *~
            * 05/08/91 ! PRR 11762  Fixed a Bad Branch            ! SID *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            hdr1$6, hdr2$20, hdr3$8,     /* Summary Screen Headings    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(100)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pf1$79, pf2$79, pf$(20)1,    /* PF Descriptors and Keys    */~
            plowkey$50,                  /* Multipurpose Plow Key      */~
            sfac$(100)1,                 /* Selection FACs             */~
            slct$(100)1,                 /* Sessions selected          */~
            status$1,                    /* Session Status             */~
            summary$(100)37,             /* Lines for Summary Screen   */~
            upd$8,                       /* Update Program Controlled  */~
            upddescr$30,                 /* Update Description         */~
            userid$3                     /* Current User Id            */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************

            dim key_data$90

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! UPDSESSN ! Update Session Control File              *~
            *************************************************************~

            select #1,  "UPDSESSN",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =  4,   keylen = 17,                      ~
                        alt key  1, keypos =     1, keylen =  20

            call "OPENCHCK" (#1,  fs%(1), f2%(1), 100%, rslt$(1))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)

            inpmessage$  = "Place a non-blank character next to Sessio" &~
                           "n(s) to Update."

*        Define Headings for Summary Screen
            hdr1$ = "  ID"
            hdr2$ = "Session Description"
            hdr3$ = "  Date"

*        Determine maximum number of sessions allowed.
            maxsessions% = dim(summary$(), 1)

*        Define what Update is being controlled here
REM         call "GETPARM" addr ("I ", "R", "UPDSELCT", " ", "0001",     ~
                                 "SELECT", "For Which Update?", 17%,     ~
                                 "K", "UPDATE  ", upd$, 8%, 0%, 0%, "A")

            upd$ = "ARIUPDTE" 

            upddescr$ = "Invoice Update"

*        Now check to see that there are no sessions queued for this
*        User/Update.  If there are, we must assume a restart condition
*        exists.
            plowkey$ = str(userid$) & str(upd$)
            call "PLOWALTS" (#1, plowkey$, 1%, 9%, f1%(1))
            if f1%(1) = 0% then L09430
L09350:         ret% = 0%
        init(" ") key_data$
        str(key_data$,01,01) = hex(22)          
        str(key_data$,02,35) = "*** ERROR *** Sessions in progress."            
        str(key_data$,78,01) = hex(22)          
        if userid$ <> "ATT" then call "ARIMAIL" (key_data$)   ~
           else call "ARIMLTX" (key_data$)
           
                goto L65000

L09430
*        Now make sure that User is not running another update.
            plowkey$ = userid$ & hex(00)
            call "PLOWALTS" (#1, plowkey$, 1%, 3%, f1%(1))
            if f1%(1) = 0% then L10000
            ret% = 0%
        init(" ") key_data$
        str(key_data$,01,01) = hex(22)          
        str(key_data$,02,39) = "*** ERROR *** Multiple updates running."      
        str(key_data$,78,01) = hex(22)          
        if userid$ <> "ATT" then call "ARIMAIL" (key_data$)   ~
           else call "ARIMLTX" (key_data$)
           
                ret% = 0%
                goto L65000

L10000: REM *************************************************************~
            *         S E L E C T I O N   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Shows sessions available for update and allows selection. *~
            *************************************************************

        summary_screen
            init(" ") errormsg$
            ret% = 0%
            gosub load_summary
            if summary% > 0% then L10150
        init(" ") key_data$
        str(key_data$,01,01) = hex(22)          
        str(key_data$,02,39) = "*** WARNING *** No sessions available."      
        str(key_data$,78,01) = hex(22)          
        if userid$ <> "ATT" then call "ARIMAIL" (key_data$)   ~
           else call "ARIMLTX" (key_data$)
           
            goto L65000

L10150:                  
*        Test that one or more sessions have been selected. If so,
*        away we go.

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Flags sessions for update and sets up for running.        *~
            *************************************************************

            gosub dataput

*          CALL "ASKUSER" (RET%, "Mode of Update",                      ~
*                          "Enter PF-1 to Update in Foreground,",       ~
*                          "      PF-2 to Update in Background.", " ")
*          IF RET% = 1% OR RET% = 2% THEN 65000 ELSE 19090
          ret% = 1%
          goto L65000

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_summary
            summary%, top% = 0%
            init(" ") summary$(), slct$()
            plowkey$ = str(upd$) & hex(00)

L30100:     call "PLOWALTS" (#1, plowkey$, 0%, 8%, f1%(1))
            if f1%(1) = 0% then return
                get #1 using L30130, temp$, status$
L30130:              FMT CH(3), XX(43), CH(1)
                if temp$ = hex(ffffff) then L30100
                if status$ <> "C"      then L30100
                s%, summary% = summary% + 1%
                get #1 using L30200,                                      ~
                          str(summary$(s%), 1, 6),       /* Session    */~
                          str(summary$(s%), 8,20),       /* Descriptn  */~
                          str(summary$(s%),29, 6)        /* Post Date  */
L30200:              FMT XX(11), CH(6), XX(3), CH(20), CH(6)
                call "DATEFMT" (str(summary$(s%),29, 8))
                goto L30100


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            for i% = 1% to summary%
                     plowkey$ = str(upd$) & str(summary$(i%),,6)
                     call "READ101" (#1, plowkey$, f1%(1))
                     if f1%(1) = 0% then L31140
                     put #1 using L31120, userid$, "U"
L31120:                   FMT CH(3), POS(47), CH(1)
                     rewrite #1
L31140:     next i%

            return

L65000: 
        exit_program

            end  ret%
