        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  U   U  PPPP   DDDD    SSS   U   U  BBBB                  *~
            *  U   U  P   P  D   D  S      U   U  B   B                 *~
            *  U   U  PPPP   D   D   SSS   U   U  BBBB                  *~
            *  U   U  P      D   D      S  U   U  B   B                 *~
            *   UUU   P      DDDD    SSS    UUU   BBBB                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * UPDSUB   - PROVIDES ACCESS TO FILE 'UPDCTL', THE UPDATE   *~
            *            CONTROL FILE.                                  *~
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
            * 04/03/84 ! ORIGINAL                                 ! ERN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "UPDSUB" (action$,           /* ACTION TO PREFORM          */~
                      #1,                /* CHANNEL OF 'UPDCTL'        */~
                      f2%,               /* FILE STATUS OF 'UPDCTL'    */~
                      procedureid$,      /* CONTROLLING PROCEDURE      */~
                      programid$,        /* ID OF RESTART PROGRAM      */~
                      subrtnid$,         /* RESTART SUB-ROUTINE        */~
                      step%,             /* RESTART STEP               */~
                      substep%,          /* RESTART SUB-STEP           */~
                      transkey$,         /* MEMO TRANSACTION KEY       */~
                      savenrs(),         /* ANY NUMBERS TO SAVE        */~
                      savestrs$(),       /* ANY STRINGS TO SAVE        */~
                      userid$,           /* USER ID                    */~
                      return%)           /* RETURN STATUS FLAG         */

        /*=============================================================*/
        /* ACTION$: "C" - Check if update was in-progress. Existance   */
        /*                of a record for UPDATE-ID/User-ID implies    */
        /*                an update was in-process.                    */
        /*          "I" - Check + inform operator. Allows abort -or-   */
        /*                proceed with restart.                        */
        /*          "W" - Write status record. Update control record   */
        /*                with restart status information.             */
        /*          "D" - Delete status record. Done to show update    */
        /*                completed.                                   */
        /*                                                             */
        /* Call/return. ACTION$ and file-channel are always            */
        /* ===========  required and remain unchanged.                 */
        /*                                                             */
        /*                          "C"      "I"      "W"      "D"     */
        /*  Linkage                in out ! in out ! in out ! in out   */
        /*  -------------------   --- --- !--- --- !--- --- !--- ---   */
        /*  F2% (file status)     rqd  0  !rqd  0  !rqd  0  !rqd  0    */
        /*   (see logic ovrvw)            !        !        !          */
        /*                                !        !        !          */
        /*  *ALL NOT SPECIFICALLY n/r rtn !n/r rtn !req n/c !n/r n/c   */
        /*   MENTIONED*                   !        !        !          */
        /*                                !        !        !          */
        /*  USERID$               n/r rtn !n/r rtn !n/r rtn !n/r rtn   */
        /*                                !        !        !          */
        /*  RETURN%- return = 0       ok  !     ok !     ok !     ok   */
        /*                  = 1       (1) !     (2)!     ---!     ---  */
        /*                  = 2       n/a !     (3)!     n/a!     n/a  */
        /*                  = 99    invalid action status flag sent    */
        /*                         -------+--------+--------+--------  */
        /*      (1) update-in-process          ! "ok" for "C" and "I"  */
        /*      (2) update-in-process: restart ! means that no record  */
        /*      (3) update-in-process: abort   ! was found.            */
        /*=============================================================*/
        /*           OVERVIEW OF PROGRAM LOGIC                         */
        /* - - - - - - - - - - - - - - - - -  - - - - - - - - - - - -  */
        /* (1) CHECK FILE EXISTANCE. If F2% = 0% (--> 'UPDCTL' open),  */
        /*     this step is skipped. Else the file is opened by the    */
        /*     sub-routine and F2% reset. If the file does not exist,  */
        /*     it is created by the sub-routine and then opened.       */
        /*                                                             */
        /* (2) Branch per action type:                                 */
        /*      "C" -or- "I". Read for status record. If not found,    */
        /*           set RETURN% = 0 and exit. If found set to 1 and   */
        /*           move record data to linkage variables for caller. */
        /*           If action = "I" display info screen and get       */
        /*           restart/abort response; set RETURN% = 2 if abort. */
        /*      "W". Write status record with data supplied.           */
        /*      "D". Delete status record.                             */
        /* (3) END.                                                    */
        /*=============================================================*/

        dim action$1,                    /* Action to preform          */~
            procedureid$8,               /* Controlling procedure      */~
            programid$8,                 /* Restart Module             */~
            savenrs(5),                  /* Some numbers to save       */~
            savestrs$(5)50,              /* Some strings to save       */~
            subrtnid$8,                  /* Restart sub-routine        */~
            transkey$100,                /* Transaction key            */~
            userid$3                     /* User ID                    */~

        dim axd$4,                       /* Alt key pointer from open  */~
            rslt$20                      /* Text from file open        */

        REM *************************************************************~
            *          I N I T I A L I Z E                              *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)

           REM  CHECK THAT FILE 'UPDCTL' EXISTS AND IS OPEN
            if f2% = 0% then goto mainlogic
            call "OPENFILE" (#1, "SHARE", f2%, rslt$, axd$)
                if f2% = 0% then goto mainlogic
                     call "OPENFILE" (#1, "OUTPT", f2%, rslt$, axd$)
                     close #1
                     call "OPENFILE" (#1, "SHARE", f2%, rslt$, axd$)

        REM *************************************************************~
            *        M A I N   P R O G R A M   L O G I C                *~
            *************************************************************
        mainlogic
            if action$ = "C" or action$ = "I" then goto check
            if action$ = "W" then goto writerecord
            if action$ = "D" then goto deleterecord
            return% = 99% : goto L65000


        check
            call "READ100" (#1, userid$, return%)

            if return% = 0% then goto L65000        /*  DONE            */

            get #1 using updctl, userid$, procedureid$, programid$,      ~
                                 subrtnid$, step%, substep%, transkey$,  ~
                                 savenrs(), savestrs$(), writecnt%

            if action$ = "C" then goto L65000       /*  DONE            */

            gosub informuser   /* DISPLAY SCREEN, GET CONT/ABORT RSPNSE*/
               if keyhit% = 16% then return% = 2%  /* DO NOT RESTART   */
               goto L65000                          /* EXIT             */


        writerecord  /* UPDATE STATUS RECORD WITH DATA SUPPLIED        */
            call "READ101" (#1, userid$, f1%)
                writecnt% = 0%
                if f1% = 0% then L11050
                     get #1 using L11035, writecnt%
L11035:                   FMT XX(425), BI(4)

L11050:     put #1 using updctl, userid$, procedureid$, programid$,      ~
                                 subrtnid$, step%, substep%, transkey$,  ~
                                 savenrs(), savestrs$(), writecnt%

            if f1% = 0% then write #1 else rewrite #1

            return% = 0%  :  goto L65000


        deleterecord
            call "DELETE" (#1, userid$, 3%)

            return% = 0%  :  goto L65000


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            *        F O R M A T   S T A T E M E N T S                  *~
            *************************************************************

        updctl:FMT                                                       ~
                CH(3),         /* USER ID (KEY)                        */~
                CH(8),         /* CONTROLLING PROCEDURE                */~
                CH(8),         /* RESTART DATA: CURRENT PROGRAM        */~
                CH(8),         /*               CURRENT SUB-ROUTINE    */~
                BI(4),         /*               NEXT STEP              */~
                BI(4),         /*               NEXT SUB-STEP          */~
                CH(100),       /* TRANSACTION KEY                      */~
                5*PD(14,4),    /* SAVE NUMERIC VALUES                  */~
                5*CH(50),      /* SAVE STRING VALUES                   */~
                BI(4)          /* RECORD WRITE COUNT                   */

        REM *************************************************************~
            *   R E S T A R T   I N F O R M A T I O N   S C R E E N     *~
            *                                                           *~
            *  INFORM OPERATOR THAT A RESTART IS REQUIRED AND GIVE      *~
            *  AN OPPURTUNITY TO ABORT                                  *~
            *************************************************************
        informuser

        accept                                                           ~
         at(01,02),"RESTART / RECOVERY",                                 ~
         at(03,02),"Your last procedure did not reach a logical",        ~
         at(04,02),"conclusion. Restarting will be done with the data",  ~
         at(05,02),"shown below. It is possible that the transaction",   ~
         at(06,02),"in-process may not update correctly.",               ~
         at(08,02),"P.F. KEYS: ( 1) CONTINUE WITH RESTART",              ~
         at(09,02),"           (15) PRINT SCREEN",                       ~
         at(10,02),"           (16) ABORT (DO NOT RESTART)",             ~
                                                                         ~
         at(14,02),"PROCEDURE: ", fac(hex(8c)), procedureid$,            ~
         at(15,02),"  PROGRAM: ", fac(hex(8c)), programid$,              ~
         at(16,02),"  SUB-RTN: ", fac(hex(8c)), subrtnid$,               ~
         at(17,02),"     STEP: ", fac(hex(8c)), step%, pic(####), ".",   ~
                                  fac(hex(8c)), substep%, pic(####),     ~
         at(18,02)," TRANS ID: ", fac(hex(8c)), transkey$,               ~
         at(19,02), fac(hex(8c)), savenrs(1), pic(#########.####),       ~
         at(19,17), fac(hex(8c)), savenrs(2), pic(#########.####),       ~
         at(19,32), fac(hex(8c)), savenrs(3), pic(#########.####),       ~
         at(19,47), fac(hex(8c)), savenrs(4), pic(#########.####),       ~
         at(19,62), fac(hex(8c)), savenrs(5), pic(#########.####),       ~
         at(20,02), fac(hex(8c)), savestrs$(1),                          ~
         at(21,02), fac(hex(8c)), savestrs$(2),                          ~
         at(22,02), fac(hex(8c)), savestrs$(3),                          ~
         at(23,02), fac(hex(8c)), savestrs$(4),                          ~
         at(24,02), fac(hex(8c)), savestrs$(5),                          ~
                keys(hex(010f10)),                                       ~
                key(keyhit%)

            if keyhit% <> 15% then return
                call "PRNTSCRN"
                goto informuser


L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
