        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   EEEEE   SSS    AAA    CCC   EEEEE   SSS    SSS    *~
            *  S      E      S      A   A  C   C  E      S      S       *~
            *   SSS   EEEE    SSS   AAAAA  C      EEEE    SSS    SSS    *~
            *      S  E          S  A   A  C   C  E          S      S   *~
            *   SSS   EEEEE   SSS   A   A   CCC   EEEEE   SSS    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SESACESS - This subroutine checks to see if the current   *~
            *            user has access rights (WRITE is assumed) to   *~
            *            the current Parent Code and/or S.E.S. Library  *~
            *            passed in to this routine.  A non-blank        *~
            *            error message passed back indicates no access. *~
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
            * 12/06/84 ! ORIGINAL                                 ! LDJ *~
            * 06/05/85 ! Changed Backdoor Value (cryptic, huh ?)  ! LDJ *~
            * 06/07/85 ! Modified Access Edit as follows; if no   ! LDJ *~
            *          !   users defined under a library then     !     *~
            *          !   universal access is assumed/allowed.   !     *~
            *          !   Otherwise users must be specifically   !     *~
            *          !   listed as components of the parent lib.!     *~
            * 07/17/85 ! Modified above edit to still require     ! LDJ *~
            *          !   user list if Library = CAELUS and user !     *~
            *          !   not a System Administrator.            !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "SESACESS"  (#1,             /* INTDOC01                   */~
                         #2,             /* INTDOC02                   */~
                         library$,       /* S.E.S. LIBRARY             */~
                         parent$,        /* STRUCTURE PARENT CODE      */~
                         errormsg$)      /* ERROR MESSAGE RETURNED     */

        dim code$16,                     /* ELEMENT DICTIONARY CODE    */~
            errormsg$79,                 /* ERROR MESSAGE RETURNED     */~
            library$6,                   /* S.E.S. LIBRARY             */~
            parent$16,                   /* STRUCTURE PARENT CODE      */~
            readkey$31,                  /* MISCEL READ & PLOW VARIABLE*/~
            userid$3,                    /* SYSTEM USER ID             */~
            usermasks$(3)4,              /* USER READ,WRITE,EXECUTE MSK*/~
            username$24                  /* CURRENT USER'S NAME        */

        dim f1%(02)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat     "
        REM *************************************************************
            errormsg$ = " "
            if userid$ > " " then L09070
            call "EXTRACT" addr("ID", userid$,                           ~
                                "NA", username$,                         ~
                                "UE", usermasks$(1),                     ~
                                "UR", usermasks$(2),                     ~
                                "UW",usermasks$(3))

L09070:     gosub check_library_access
            if errormsg$ > " " then L65000
            gosub check_parent_access
            goto L65000

        REM *************************************************************~
            *                   CHECK LIBRARY ACCESS                    *~
            *************************************************************
        check_library_access
            readkey$ = library$
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then L09420
            get #1 using L09200, type$
L09200:     FMT XX(16), CH(8)
            if type$ <> "LIBRARY" then L09460
            if usermasks$(1)=hex(ffffffff) and                           ~
               usermasks$(2)=hex(ffffffff) and                           ~
               usermasks$(3)=hex(ffffffff) then return /* SYSTEM ADMIN */
            if userid$ = "@$*" then return             /* BACK DOOR    */
            if library$ = "CAELUS" then L09260
            readkey$ = library$
            call "PLOWNEXT" (#2, readkey$, 16%, f1%(2))
            if f1%(2) = 0% then return
L09260:     errormsg$="Sorry " & username$ & ", you do not have write acc~
        ~ess to this Library"
            readkey$ = library$
L09290:     call "PLOWNEXT" (#2, readkey$, 16%, f1%(2))
            if f1%(2) = 0% then return
            get #2 using L09320, code$
L09320:     FMT CH(16)
            if len(code$) > 3% then L09290
            call "READ100" (#1, code$, f1%(1))
            if f1%(1) = 0% then L09290
            get #1 using L09200, type$
            if str(type$,,4%) <> "USER" then L09290
            if code$ <> userid$ then L09290
            errormsg$ = " "
            return

L09420:     errormsg$="Library " & library$ & " not defined in the Elemen~
        ~t Dictionary"
            return

L09460:     errormsg$=library$ & " is NOT a Library (Type = " & type$ &")"
            return

        REM *************************************************************~
            *                   CHECK PARENT ACCESS                     *~
            *************************************************************
        check_parent_access
            if parent$ = " " then return
            REM *** ADD CALL TO NEW SECURITY ROUTINE HERE ***
            return

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            *                RETURNS TO CALLING PROGRAM                 *~
            *                                                           *~
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
