        REM THISPROGRAMWASGENERATEDUSINGTHEGENMENUPROGRAMWHICHISAPROPRIET~
            *                                                           *~
            *  TTTTT    A     SSS   K  K   U   U  PPPP                  *~
            *    T     A A   S      K K    U   U  P   P                 *~
            *    T    AAAAA   SSS   KKK    U   U  PPPP                  *~
            *    T    A   A      S  K  K   U   U  P                     *~
            *    T    A   A   SSS   K   K   UUU   P                     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TASKUP   - Verify that requested task is running in       *~
            *            Background.  If not, then get it running.      *~
            *            Also used to submit programs/procedures to     *~
            *            Background without checking to see if already  *~
            *            running.                                       *~
            *-----------------------------------------------------------*~
            * This Program Contains Valuable Trade Secrets And Proprie- *~
            * tary Assets Of CAELUS INCORPORATED, Spokane, WA, Embodying*~
            * Substantial Creative Efforts  And Confidential            *~
            * Information.  Unauthorized Use, Copying, Decompiling,     *~
            * Translating, Disclosure, Or Transfer Of It Is Prohibited. *~
            * Copyright (C) 1985, An Unpublished Work By CAELUS Inc.,   *~
            * Spokane, WA.  All Rights Reserved.                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/02/85 ! ORIGINAL                                 ! HES *~
            * 03/05/86 ! Add CDATOCMS+CDAVSCOM to tasks supported ! HES *~
            * 07/18/86 ! Add ability to support reports in bckgrnd! HES *~
            * 09/21/88 ! Modified to now use CMSLINK to FIND the  ! LDJ *~
            *          ! Library & Volume of the Program/Proc to  !     *~
            *          ! Submit to Background (no longer must be  !     *~
            *          ! in same library).                        !     *~
            *          ! Fixed bug - PRINT PAGE statement caused  !     *~
            *          ! calling program to CRASH if in background!     *~
            *          ! Minor correction - now sets SPOOLSYS per !     *~
            *          ! user's defaults.                         !     *~
            *          ! Also added ZZ Identifer to tell TASKUP   !     *~
            *          ! that the Program, Library, & Volume to   !     *~
            *          ! submit were passed as additional         !     *~
            *          ! arguments to the standard argument list. !     *~
            *          ! (note that there are other, accompanying,!     *~
            *          ! arguments to those three.)               !     *~
            *          ! If ZZ will submit the specified Program  !     *~
            *          ! to background rather than using the      !     *~
            *          ! internal hardcoded list (ala JBPOST1).   !     *~
            *          ! Now uses User's Default Submittal Job    !     *~
            *          ! Class if no other available/defined      !     *~
            *          ! instead of hard-coded "A".               !     *~
            * 11/28/90 ! Modified to generate Port ID for unique  ! LDJ *~
            *          ! ZZ programs.                             !     *~
            * 07/17/91 ! PRRs 11170, 11877.  Print Mode is now set! JDH *~
            *          !   to the users useage constants except   !     *~
            *          !   when SPOOLSYS is non-blank and Print   !     *~
            *          !   Mode is 'H'.  If so change to 'K'.     !     *~
            * 11/24/93 ! New - Set Port Id & Return. Major Clean  ! KAB *~
            *          !   Up for ZZ and ME & 9997%.  Would crash !     *~
            *          !   on Write #1 for ZZ 'cause not open.    !     *~
            *          !   Won't Destroy TEMPPORT unless we were  !     *~
            *          !   the creators.                          !     *~
            *          !   Won't test port for ZZ, Unique = "N"   !     *~
            *          !   Won't fall thru ME, 9999, 9998 &       !     *~
            *          !     submit instead of cancel (now ends)  !     *~
            *          !   Alternate Method of setting file class !     *~
            *          !     for workfile.                        !     *~
            *          !   Should now be Unix transparent.        !     *~
            *          !   Unix Only (TASKCTL) can destroy orphan !     *~
            *          !     message ports.                       !     *~
            * 05/23/94 !   In the unlikely event of two tasks     ! KAB *~
            *          !     trying to bring up a task at the     !     *~
            *          !     same time, the check of TEMPPORT$    !     *~
            *          !     can return 16% for one of them,      !     *~
            *          !     resulting in (erroneously) reporting !     *~
            *          !     that the task is not up.             !     *~
            *          !   In the more unlikely event that the    !     *~
            *          !     MESSAGE routine is unable the set    !     *~
            *          !     CEXIT routine this will result in    !     *~
            *          !     a DEBUG CANCEL, which is definitely  !     *~
            *          !     not expected.                        !     *~
            *          !   Actually we need to wait longer (and   !     *~
            *          !     not check a port we didn't create).  !     *~
            * 09/11/96 ! Changes for the year 2000.               ! DXL *~
            ARYPRODUCTOFCAELUSASSOCIATESSPOKANEWAALLRIGHTSRESERVEDGENMENU

            sub "TASKUP" (id$,       /* Passed In only, not modified.  */~
                                     /* Possible values / meanings are:*/~
                                     /* CD = Verify & Submit CDATOCMS  */~
                                     /* J1 = Verify & Submit JBPOST1   */~
                                     /* J2 = Verify & Submit JBPOST2   */~
                                     /* ME = Submit the Calling Program*/~
                                     /* PO = Verify & Submit VBKUPDTE  */~
                                     /* SO = Verify & Submit BCKUPDTE  */~
                                     /* TK = Verify & Submit CDAVSCOM  */~
                                     /* ZZ = Submit the Passed In      */~
                                     /*      Program below, Verifying  */~
                                     /*      to see if already running */~
                                     /*      only if so specified.     */~
                          return%,   /* IN:  9997 = Constuct Message   */~
                                     /*             Port for INLIB     */~
                                     /*      9998 = Send ABORT Message */~
                                     /*             to Existing Task.  */~
                                     /*      9999 = Send CANCEL Message*/~
                                     /*             to Existing Task.  */~
                                     /*      8000 = Set Port Id & RTN  */~
                                     /*      8001 = Destroy (Unix)     */~
                                     /*      8999 = Submit (no wait)   */~
                                     /* OUT:   99 = SYSFILE2 not found.*/~
                                     /*        90 = SUBMIT to          */~
                                     /*             Background failed. */~
                                     /*        80 = Submitted task     */~
                                     /*             failed to respond  */~
                                     /*             (send ITM) within  */~
                                     /*             the given tim-out  */~
                                     /*             period.            */~
                                     /*        70 = Unable to FIND     */~
                                     /*             the specified      */~
                                     /*             Program to Submit. */~
                                     /*  PORTID%    IF 9997% in, port  */~
                                     /*             ID out. use BIN(,4)*/~
                                     /*             in calling program */~
                                     /*             change back PORT$  */~
                                     /*         0 = Either Task was    */~
                                     /*             Successfully       */~
                                     /*             Submitted -or- is  */~
                                     /*             already running    */~
                                     /*             -or- was signaled  */~
                                     /*             to ABORT or CANCEL.*/~
                          program$,  /* Name of Program/Proc to Submit.*/~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be both     */~
                                     /* present and non-blank.         */~
                          library$,  /* Name of Program/Proc Library.  */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                                     /* If present but blank will      */~
                                     /* search for program lib and vol.*/~
                          volume$,   /* Name of Program/Proc Volume.   */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                                     /* If present but blank will      */~
                                     /* search for program lib and vol.*/~
                          dbspec$,   /* Is the Program Specific to a   */~
                                     /* Caelus Database (INLIB)? Y/N.  */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                          unique$,   /* Must the Program be Unique?    */~
                                     /* Y/N. If Database Specific = Y  */~
                                     /* then only one copy per database*/~
                                     /* may be running.  If Database   */~
                                     /* Specific = N then only one copy*/~
                                     /* can be running at any given    */~
                                     /* time on the entire system.     */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                          showerrs$, /* Show Error messages on the     */~
                                     /* Screen? Y/blank = Yes - ASKUSER*/~
                                     /*         S       = Yes - SHOSTAT*/~
                                     /*         N       = NO           */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                          putparm$,  /* Create PUTPARM(s) to Satisfy   */~
                                     /* the submitted programs GETPARMS*/~
                                     /* Y or N.                        */~
                                     /* Ignored unless ID$ = ZZ in     */~
                                     /* which case it must be present. */~
                          prname$(), /* Name of Program GETPARM(s) to  */~
                                     /* be satisfied or PUTPARM'd.     */~
                                     /* Ignored unless ID$ = ZZ and    */~
                                     /* PUTPARM$ = "Y" in which case   */~
                                     /* it is required and must be     */~
                                     /* present.                       */~
                                     /* Number of array elements may be*/~
                                     /* 1 thru 'N' but the element     */~
                                     /* length must be 8.              */~
                          prfield$(),/* Name of Program GETPARM FIELDS */~
                                     /* be satisfied or PUTPARM'd.     */~
                                     /* Ignored unless ID$ = ZZ and    */~
                                     /* PUTPARM$ = "Y" in which case   */~
                                     /* it is required and must be     */~
                                     /* present.                       */~
                                     /* This MUST be a 2 dimensional   */~
                                     /* array whose rows correspond to */~
                                     /* the PRNAME$() above and columns*/~
                                     /* the fields for each PRNAME.    */~
                                     /* Contains the GETPARM FIELD name*/~
                                     /* to be satisfied using the      */~
                                     /* PRVALUES$() below.             */~
                                     /* Number of rows must be >= the  */~
                                     /* number of elements in PRNAME$()*/~
                                     /* and columns >= the max number  */~
                                     /* of fields to supply to a PRNAME*/~
                                     /* but the individual element     */~
                                     /* length must be 8.              */~
                          prvalue$())/* Values for GETPARM FIELDS      */~
                                     /* be satisfied or PUTPARM'd.     */~
                                     /* Ignored unless ID$ = ZZ and    */~
                                     /* PUTPARM$ = "Y" in which case   */~
                                     /* it is required and must be     */~
                                     /* present.                       */~
                                     /* This MUST be a 2 dimensional   */~
                                     /* array of the same dimensions   */~
                                     /* as PRFIELD$() above except that*/~
                                     /* the individual element length  */~
                                     /* must be only as large as the   */~
                                     /* largest field value to putparm.*/

        dim                                                              ~
            class$(2)1,                  /* Classes For Extracts       */~
            dbspec$1,                    /* Is the Program Database Spe*/~
            errormsg$(2)80,              /* For Call To Ask User +     */~
            exitmsg$24,                  /* CMSLINK Arg for PF16 prompt*/~
            id$2,                        /* Part Of Port Id            */~
            inlib$8,                     /* Data Base Name             */~
            jclass$1,                    /* Procedure Submit class     */~
            lastid$1,                    /* Work Variable              */~
            lastlib$8,                   /* Work Variable              */~
            lastport$4,                  /* Last Task Brought Up       */~
            lib$8,                       /* Library For Extract        */~
            library$8,                   /* Passed In Program Library  */~
            message$6,                   /* Wake Up/Shut Down Flag     */~
            nohelp$1,                    /* No Help 'CH' paramenter    */~
            port$4,                      /* Message Prot For Post Task */~
            post$8,                      /* Name of post program to chk*/~
            prfield$(1,1)8,              /* Passed in PR Field Names ()*/~
            prname$(1)8,                 /* Passed in PR Names ()      */~
            prtmode$1,                   /* User's Print Mode          */~
            prvalue$(1,1)1,              /* Passed in PR Field Values()*/~
            program$8,                   /* Passed In Program to Submit*/~
            putparm$1,                   /* Create Putparms? (Y/N)     */~
            rec$80,                      /* Work Variable              */~
            savefc$1, setfc$1,           /* Output File Class          */~
            search%(2),                  /* Work Variable              */~
            showerrs$1,                  /* Show Error Messages?       */~
            showerr$1,                   /* Show Error Messages?       */~
            switchs$12,                  /* Module Indictor for switchs*/~
            sysvol$6,                    /* Volume IPLed from          */~
            tid$1,                       /* Internal Task Id.          */~
            tempport$4, deport$4,        /* Port created/killed & here */~
            ttype$1,                     /* Background/Forground       */~
            unique$1,                    /* Must the Program be Unique?*/~
            unix$6,                      /* System for Platform det.   */~
            vol$6,                       /* Volume For Extract         */~
            volume$6,                    /* Passed In Program Volume   */~
            wlib$8,                      /* Library For Temp Procedure */~
            work$8,                      /* Work Variable              */~
            wvol$6                       /* Volume For Temp Procedure  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus System Parameters File            *~
            ******+**********+*******************************************~

            select #1 , "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #2 , "WORKFILE",                                      ~
                        varc,                                            ~
                        consec,                                          ~
                        recsize = 80

            select #3,  "USERLCMS",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =    4, keylen =  30, dup

        REM *************************************************************~
            * Set Function and open SYSFILE2 if needed                  *~
            *                                                           *~
            *  1 - Submit, with port                                    *~
            *  2 - Send Cancel Message (port)                           *~
            *  3 - Set Message Port in Data Base                        *~
            *  4 - Destroy Message Ports (unix)                         *~
            *  5 - Return Message Port   (same for all)                 *~
            *  6 - Submit, with no port                                 *~
            *  7 - Send Cancel Message (no port) = END                  *~
            *  8 - Set Message Port in Data Base = END                  *~
            *  9 - Destroy Message Ports (unix) (no port) = END         *~
            *                                                           *~
            *  No port is either ID$ = "ME" or                          *~
            *                   (ID$ = "ZZ" and UNIQUE$ <> "Y)          *~
            *************************************************************~
            * The flow (lumpy as it may be) -                           *~
            *                                                           *~
            *    Operation  / FN% ! 1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8 ! 9 ! *~
            * --------------------+---+---+---+---+---+---+---+---+---! *~
            * Open Sysfile2       ! X !   ! X !   !   !   !   !   !   ! *~
            * Set up              ! X ! X ! X ! X ! X ! X !   !   !   ! *~
            * Construct Port Id   ! X ! X ! X ! X ! X !   !   !   !   ! *~
            *    Check Port       ! X !   !   !   !   !   !   !   !   ! *~
            *    Issue Cancel Msg !   ! X !   !   !   !   !   !   !   ! *~
            *    Destroy Port(s)  !   !   !   ! X !   !   !   !   !   ! *~
            *    Return Port Id   !   !   !   !   ! X !   !   !   !   ! *~
            * Create Tempport     ! X !   !   !   !   !   !   !   !   ! *~
            * Write Sysfile2      ! X !   ! X !   !   !   !   !   !   ! *~
            * Find Program        ! X !   !   !   !   ! X !   !   !   ! *~
            * Create Procedure    ! X !   !   !   !   ! X !   !   !   ! *~
            * Submit Procedure    ! X !   !   !   !   ! X !   !   !   ! *~
            * Wait for Task       ! X !   !   !   !   !   !   !   !   ! *~
            * Kill Tempport       ! X !   !   !   !   !   !   !   !   ! *~
            * End [Close Sysfile2]! X ! X ! X ! X ! X ! X ! X ! X ! X ! *~
            *************************************************************~
            *                                                           *~
            * And Last a note about "ZZ" & Unique = "Y" -               *~
            *     Procedure will be on SYSVOL - (Via SET_UP_OTHERS)     *~
            *       If DBSPEC = "N" then WLIB$ = "#TEMPROC"             *~
            *          & Port Id is based on "QWERTYUI" (as INLIB$)     *~
            *       Else WLIB & Port Id based on true INLIB (somehow).  *~
            *     The Procdure will be created using those WLIB &       *~
            *     WVOL values or not at all.                            *~
            *                                                           *~
            *************************************************************

            fn% = 1%           /* Default, Submit with port            */
            message$ = "X"
            no_wait% = 0%
            nohelp$ = " "

            if return% <> 8999% then L06020
               no_wait% = 1%
L06020:     if return% <> 9999% then L06040
               fn% = 2% : message$ = "CANCEL"
L06040:     if return% <> 9998% then L06060
               fn% = 2% : message$ = "ABORT"
L06060:     if return% <> 8000% then L06080
               fn% = 3%
L06080:     if return% <> 8001% then L06100
               fn% = 4%
L06100:     if return% <> 9997% then L06130
               fn% = 5%

L06130:     if id$  = "ME" then L06170
            if id$ <> "ZZ" then L06190
               if unique$ = "Y" then L06190

L06170:     if fn% < 5% then fn% = fn% + 5%

L06190:     return% = 99%

            on fn% goto open_sysfile2,        /* Submit with port      */~
                        initialize   ,        /* Send Cancel Message   */~
                        open_sysfile2,        /* Set Port Id in SYS2   */~
                        initialize   ,        /* Destroy Ports (unix)  */~
                        initialize   ,        /* Return Port Id        */~
                        initialize            /* Submit without port   */
            /* Note the fall through for 7, 8, 9 */

                  return% = 0%
                  goto exit_routine

        open_sysfile2

            call "OPENCHCK" (#1, open%, 0%, 0%, " ")
            if open% = 1% then initialize
            errormsg$(1%) = "Required file not found (SYSFILE2)."
            errormsg$(2%) = "Background Task can't be submitted."
            goto tell_user

        REM *************************************************************~
            *              I N I T I A L I Z A T I O N                  *~
            *-----------------------------------------------------------*~
            * Initializes Information Necessary For Program.            *~
            *************************************************************
        initialize
            call "EXTRACT" addr ("IL", inlib$, "TT", ttype$, "WL", wlib$,~
                                 "WV", wvol$,  "XV", sysvol$, "S#", unix$)
            if wvol$ = " " then wvol$ = sysvol$

            unix% =  0%
            convert unix$ to unix%, data goto L09085
               goto L09090
L09085:     unix% = -1%

L09090:     REM Up Determine task to initiate/check
            REM NOTE-task ids must be CAPITOL LETTERS only(see TEMPPORT$)
            tempport$, deport$ = " "
            showerr$ = "Y"

            search "MEJ1J2SOPOCDTKZZ" = id$ to search%() step 2%
                if search%(1%) = 0% then exit_routine
            case% = (search%(1%) +1%) / 2%

            on case% gosub setup__self,    /* Submit current   program */~
                           setup__jbpost1, /* Submit JBPOST1   program */~
                           setup__jbpost2, /* Submit JBPOST2   program */~
                           setup__bckupdte,/* Submit BCKUPDTE  program */~
                           setup__vbkupdte,/* Submit VBKUPDTE  program */~
                           setup__cdatocms,/* Submit CDATOCMS  program */~
                           setup__cdavscom,/* Submit CDAVSCOM  program */~
                           setup__others   /* Submit Specified program */

*          ON FN% GOTO CONSTRUCT_MESSAGE_PORTID, /* Submit with port  */~
*                      CONSTRUCT_MESSAGE_PORTID, /* Send Cancel Mess. */~
*                      CONSTRUCT_MESSAGE_PORTID, /* Set Port Id       */~
*                      CONSTRUCT_MESSAGE_PORTID, /* Kill Ports (unix) */~
*                      CONSTRUCT_MESSAGE_PORTID, /* Return Port Id    */~
*                      FIND_PROGRAM              /* Submit w/o port   */

            if fn% = 6% then goto find_program                           ~
                        else goto construct_message_portid

        setup__self
            call "EXTRACT" addr("CF", post$)
                                tid$ = " " :switchs$ = "   " : return
        setup__jbpost1
            post$ = "JBPOST1 " :tid$ = "A" :switchs$ = "SFC" : return

        setup__jbpost2
            post$ = "JBPOST2 " :tid$ = "B" :switchs$ = "SFC" : return

        setup__bckupdte
            post$ = "BCKUPDTE" :tid$ = "C" :switchs$ = "BCK" : return

        setup__vbkupdte
            post$ = "VBKUPDTE" :tid$ = "D" :switchs$ = "VBK" : return

        setup__cdatocms
            post$ = "CDATOCMS" :tid$ = "E" :switchs$ = "CDA" : return

        setup__cdavscom
            post$ = "CDAVSCOM" :tid$ = "F" :switchs$ = "   " : return

*        Future
            post$ = "future "  :tid$ = "?" :switchs$ = "xxx" : return

        setup__others
            REM Check If Background Task Is Already Up if Unique = Y
            post$ = program$   :tid$ = "Z" :switchs$ = "   "
            showerr$ = showerrs$
            if fn% > 1% then return
            if unix% < 0% then return      /* Has to rely on msg port! */
                                           /* & Won't Create Procedure */
            wvol$ = sysvol$       /* Insures Immediate fall out below  */
            wlib$ = "#TEMPROC"    /* Now Set WLIB                      */
            if dbspec$ = "N" then L09972
               wlib$ = "#"
               REM *** Calculate WorkLib based on INLIB ***
               u3% = 0%
               for i% = 1% to len(inlib$)
                   search%(1%) = max(val(str(inlib$,i%,1%),1) - 64%, 0%)
                   if str(inlib$,i%,1%) = "#" then search%(1%) = 27%
                   if str(inlib$,i%,1%) = "$" then search%(1%) = 28%
                   u3% = u3% + i% * search%(1%)
               next i%
               convert u3% to str(wlib$,2%,7%), pic(0000000)
L09972:     call "SCRATCH" addr("F", post$, wlib$, wvol$, "B", u3%)
            if u3% = 0% or u3% = 16% or u3% = 20% then return
            return clear
            if u3% <> 32% then proc_msg
               return% = 0%
               goto exit_routine
            /* If In Use - Must be running already          */
            /* Otherwise Issue error message to User & Exit */

        REM *************************************************************~
            * Concoct Port Id using convoluted base 64 arithmetic       *~
            *************************************************************

        construct_message_portid
            REM Check If Background Task Is Already Up...
            REM (Concoct a port id unique for task/database)
            if id$ <> "ZZ" then L10100
               if dbspec$ = "N" then inlib$ = "QWERTYUI"
                                  /* ^ Can be up only once on system */

L10100:     if fn% > 1% then L10150

            if lastlib$=inlib$ and lastid$ = tid$ and inlib$ <> " "      ~
               then L10500

L10150:     work$ = inlib$
            database% = 0%

            if tid$ = "F" then L10360  /* Can be up only once on system */

            tran(work$,"< =#>$")replacing

            work$ = and all(hex(3f))

            REM since value of each byte is now less then 64 (00xxxxxx),
            REM the 8 char name can be reduced to a unique 6 char name.

            call "BITUNPK" addr (work$, errormsg$(1%), 8%)
            for i%=0% to 7% : str(errormsg$(1%),i%*8%+1%,2%)=" " : next i%
            call "SPCSMASH" (errormsg$(1%))
            call "BITPACK" addr (errormsg$(1%), work$, 64%)

            rotatec(str(work$,,8%),1%) /*Try to be as random as possible*/

            get work$, using L10330, u3%, line%
L10330:     FMT 2*BI(3)
            database% = (u3% + line%) / 2%

L10360:     put port$, using L10370, tid$, database%
L10370:     FMT CH(1), BI(3)
            lastlib$ = inlib$ : lastid$ = tid$

        REM *************************************************************~
            * Just to avoid someone else's pain. . .                    *~
            *                                                           *~
            * A3% = MOD( VAL( STR(WORK$,1,1),1), 64%)                   *~
            * A2% = MOD( VAL( STR(WORK$,2,1),1), 64%)                   *~
            * A1% = MOD( VAL( STR(WORK$,3,1),1), 64%)                   *~
            * A0% = MOD( VAL( STR(WORK$,4,1),1), 64%)                   *~
            *                                                           *~
            * B3% = MOD( VAL( STR(WORK$,5,1),1), 64%)                   *~
            * B2% = MOD( VAL( STR(WORK$,6,1),1), 64%)                   *~
            * B1% = MOD( VAL( STR(WORK$,7,1),1), 64%)                   *~
            * B0% = MOD( VAL( STR(WORK$,7,1),1), 64%)                   *~
            *                                                           *~
            * A% = A3%*(64**3) + A2%*(64**2) + A1%*(64) + A0%           *~
            * B% = B3%*(64**3) + B2%*(64**2) + B1%*(64) + B0%           *~
            *                                                           *~
            *    **** Roughly Replaces AND(HEX(3F)), BITUNPK & BITPACK  *~
            *                                                           *~
            * A% = 2% * A%                                              *~
            * B% = 2% * B%                                              *~
            * IF B% >= (64**4) THEN A0% = 1% ELSE A0% = 0%              *~
            * IF A% >= (64**4) THEN B0% = 1% ELSE B0% = 0%              *~
            * A% = A0% + MOD(A%, 64**4)                                 *~
            * B% = B0% + MOD(B%, 64**4)                                 *~
            *                                                           *~
            *    **** Roughly Replaces ROTATEC                          *~
            *                                                           *~
            * DATABASE% = (A% + B%) / 2%                                *~
            *                                                           *~
            *************************************************************

L10500: REM *************************************************************~
            * Now we either check the port, submit a cancel message,    *~
            *    Return the PORT ID, Destroy port & temp_port, or just  *~
            *    branch to update SYSFILE2                              *~
            *************************************************************

            on fn% goto check_port_id,            /* Submit with port  */~
                        check_port_id,            /* Send Cancel Mess. */~
                        write_sysfile2,           /* Set Port Id       */~
                        destroy_port_id,          /* Kill Ports (unix) */~
                        return_port_id            /* Return Port Id    */~


        return_port_id

            return% = val(port$,4)
            lastlib$ = " " : lastid$ = " " : lastport$ = " "
            goto exit_routine                /* FN% = 5% departs . . . */

        destroy_port_id
            if unix% < 0% then L10710
               goto L10745                   /* Not on Wang VS . . .    */
L10710:     deport$ = port$
            call "MESSAGE" addr("DE", deport$, u3%)
            deport$ = port$                /* Just to be safe         */
            str(deport$,,1%) = or hex(20)  /* To lower case, tempport */
            call "MESSAGE" addr("DE", deport$, u3%)
L10745:     return% = 0%
            lastlib$ = " " : lastid$ = " " : lastport$ = " "
            goto exit_routine               /* FN% = 4% departs . . . */

        check_port_id
            REM Actual Check Is Here...
            call "MESSAGE" addr ("XM", port$, message$, 6%, u3%)
                if fn% = 2% then L10828            /* Cancel Requested */
                if u3% = 4 and message$ = "X" then cms_background_tasks
                   lastport$ = port$
                     goto L10830
L10828:            lastlib$ = " " : lastid$ = " " : lastport$ = " "
L10830:            return% = 0%     /* Task is already up or won't be */
                   goto exit_routine        /* FN% = 2% departs . . . */

        REM *************************************************************~
            *                   M A I N   P R O G R A M                 *~
            *-----------------------------------------------------------*~
            * See If We Can Get It Up ...                               *~
            *************************************************************

        cms_background_tasks
            REM Task not up, let's do something about that...

            REM   Create temporary port used to keep user in this sub
            REM   until the task is completely up. (files opened, etc.)
            REM   This port also stops 2 users from attempting to bring
            REM   up same task for the same database at the same time.

            tempport$ = port$
            str(tempport$,,1%) = or hex(20)  /* To lower case */
            deport$ = tempport$
            call "MESSAGE" addr ("CR", tempport$, 200%, u3%)
                 if u3% = 0% then L11220  /* I Created It */
                    deport$ = " "        /* No, I didn't */
*                  GOTO WAIT_FOR_TASK_TO_COME_UP

L11220:     timeout% = 90%  /* Default is 1.5 minute */
            call "EXTRACT" addr("JC", jclass$)
            if jclass$ = " " then jclass$ = "A"

            if switchs$ = " " then L11320
            rec$ = "SWITCHS." & switchs$
            call "READ100" (#1, rec$, f1%)
                if f1% = 0% then L11320
            get #1, using L11310, timeout%, jclass$
L11310:     FMT XX(20), BI(2), CH(1)
L11320:     if jclass$ = "*" then lastport$ = port$  /* Don't Want Task */
            if lastport$ = port$ then get_outa_dodge /* Don't Try Again */
               lastport$ = port$
            if deport$ = " " then wait_for_task_to_come_up

        REM Up we go...

        write_sysfile2
            REM Save Port name for passing to task...
            rec$ = "PORT.ID." & post$
            call "READ101" (#1, rec$, f1%)
                if f1% <> 0% then delete #1
            write #1, using L11430, "PORT.ID.", post$, port$, " "," "," "
L11430:        FMT CH(8), CH(12), CH(4), CH(200), CH(200), CH(76)
            if fn% = 1% then find_program
               return% = 0%
               lastlib$ = " " : lastid$ = " " : lastport$ = " "
               goto exit_routine             /* FN% = 3% departs . . . */

        REM *************************************************************~
            * Find Program - here is where things come together again   *~
            *************************************************************

        find_program
            /* FN% = 6% rejoins, all thats left are FN% = 1%, 6% */

            REM *** Locate Program to Submit ***
            call "EXTRACT" addr("CL",rlib$,"CV",rvol$)
            if id$ <> "ZZ" then L11585
               if library$ <> " " then rlib$ = library$
               if volume$  <> " " then rvol$ = volume$

L11585:     if unix% < 0% then create_procedure

            call "CMSLINK" addr(#3, "   ", "F", post$, rlib$, rvol$,     ~
                 " ", "N", exitmsg$,"                ", "N", u3%, return%)
            if u3% = 0% then create_procedure
               return% = 70%
               errormsg$(1%) = "Unable to FIND " & post$ & "."
               errormsg$(2%) = "Background Task wasn't submitted."
               goto tell_user

        REM *************************************************************~
            * Create Procedure on the fly . . .                         *~
            *   Not Applicable for Unix                                 *~
            *************************************************************

        create_procedure
            REM Create submit procedure on the fly...
            call "SHOSTAT"("Submitting Background Task: " & post$)

            if unix% < 0% then submit_procedure

L12080:     next% = 0% : lib$ = wlib$
L12090:     call "SCRATCH" addr("F", post$, wlib$, wvol$, "B", u3%)
                if u3% = 0% or u3% = 16% or u3% = 20% then L12260
                if next% > 9% then L12190
                if id$ <> "ZZ" then L12140
                   if unique$ = "Y" then proc_msg
L12140:         str(wlib$,5%) = "WRK"
                convert next% to  str(wlib$,8%), pic(#)
                next% = next% + 1%
                goto L12090

L12190:         if wvol$ = sysvol$ then proc_msg
                   wvol$ = sysvol$ : wlib$ = lib$
                   goto L12080

        proc_msg
                return% = 75%
                errormsg$(1%) ="Can't scratch temporary SUBMIT procedure."
                errormsg$(2%) ="Background Task can't be submitted."
                goto tell_user

L12260:     setfc$ = " "
            call "EXTRACT" addr("FC", savefc$)
            call "SET"     addr("FC", setfc$)
            call "PUTNAMES" addr(#2, post$, wlib$, wvol$)
            call "WORKOPN2" (#2, "OUTSP", 20%, f2%) : f2% = 0%

            REM And CMS said, 'let there be a procedure', and...

            line% = 0%

            put rec$, using L13050, post$, "[]": gosub write_it
            put rec$, using L13060, "[]": gosub write_it

            REM Build Set Statements...
            rec$ = " " : gosub write_it
            call "EXTRACT" addr ("IL", lib$, "IV", vol$)
            put rec$, using L13070, lib$, vol$, "[]": gosub write_it
            call "EXTRACT" addr ("OL", lib$, "OV", vol$)
            put rec$, using L13080, lib$, vol$, "[]": gosub write_it
            call "EXTRACT" addr ("RL", lib$, "RV", vol$)
            put rec$, using L13090, lib$, vol$, "[]": gosub write_it
            call "EXTRACT" addr ("SL", lib$, "SV", vol$)
            put rec$, using L13100, lib$, vol$, "[]": gosub write_it
            call "EXTRACT" addr ("WV", vol$)
            if vol$ = " " then vol$ = sysvol$
            call "EXTSPSYS" addr(lib$)    /* SPOOLSYS */
            put rec$, using L13110, lib$, vol$, "[]": gosub write_it
            put rec$, using L13120, savefc$, "[]": gosub write_it
            call "EXTRACT" addr ("PF", class$(1), "PC", class$(2),       ~
                                                              "FN", u3%)
            call "EXTRACT" addr ("PM", prtmode$)
            if lib$ <> " " and prtmode$ = "H" then prtmode$ = "K"
            put rec$, using L13130, class$(1), prtmode$, class$(2), "#",  ~
                                   u3%, "[]"
                gosub write_it
            call "EXTRACT" addr ("PR", u3%)
            put rec$, using L13140, u3%, "[]": gosub write_it
            put rec$, using L13150, "[]": gosub write_it
            call "EXTRACT" addr ("PL", lib$, "PV", vol$)
            put rec$, using L13160, lib$, vol$, "[]": gosub write_it
            rec$ = " " : gosub write_it
            put rec$, using L13170, post$,rlib$,rvol$, "[]"
            gosub write_it : put rec$, using L13180, "[]" : gosub write_it
            if id$ <> "ZZ" then L12830
               if putparm$ <> "Y" then L12830
                  for i% = 1% to dim(prname$(),1)
                      if prname$(i%) = " " then L12820
                      put rec$, using L13190, prname$(i%), "[]"
                      gosub write_it
                      for u3% = 1% to dim(prfield$(),2)
                          if prfield$(i%,u3%) = " " then L12790
                          put rec$, using L13200, prfield$(i%,u3%),       ~
                                   "'" & prvalue$(i%,u3%) & "',",  "[]"
                          gosub write_it
L12790:               next u3%
                      put rec$, using L13210, "JUNK    ", "JUNK", "[]"
                      gosub write_it
L12820:           next i%
L12830:     put rec$, using L13220, "[]": gosub write_it

            close #2
            call "SET" addr("FC", savefc$)
            goto submit_procedure

        REM *** Change Protect Class to Blank So Others Can Scratch***  *~
*          CALL "PROTECT" ADDR("F", POST$, WLIB$, WVOL$, "FC", " ",    *~
*                              "ED", "010101", " ", U3%)               *~
*          GOTO SUBMIT_PROCEDURE                                       *~
*       ****************************************************************


        write_it
            line% = line% + 100%
            convert line% to str(rec$,75%,6%), pic(000000)
            write #2, using L12950, rec$
L12950:     FMT CH(80)
            return

        REM *************************************************************~
            * The Procedure Image Statements                            *~
            *************************************************************~


L13050: %PROCEDURE ########                                            ##
L13060: %*      Temp procedure, kill at will                           ##
L13070: %S0: SET INLIB='########', INVOL='######',                     ##
L13080: %        OUTLIB='########', OUTVOL='######',                   ##
L13090: %        RUNLIB='########', RUNVOL='######',                   ##
L13100: %        SPOOLIB='########', SPOOLVOL='######',                ##
L13110: %        SPOOLSYS='########', WORKVOL='######',                ##
L13120: %        FILECLAS='#'                                          ##
L13130: %    SET PRTFCLAS='#', PRNTMODE='#', PRTCLASS='#', FORM#=###,  ##
L13140: %        PRINTER=###, LINES=55                                 ##
L13150: %    SET JOBQUEUE='R', JOBCLASS='A', JOBLIMIT=0                ##
L13160: %    SET PROGLIB='########', PROGVOL='######'                  ##
L13170: %S1: RUN ######## IN ######## ON ###### CANCEL EXIT IS DP      ##
L13180: %ERROR EXIT IS DP                                              ##
L13190: %       ENTER ########                                         ##
L13200: %             ######## = ###################################   ##
L13210: %             ######## = ###################################   ##
L13220: %DP: DESTROY PROCEDURE                                         ##


        REM *************************************************************~
            *                       S U B M I T                         *~
            * I submit! I submit!                                       *~
            *************************************************************
        submit_procedure
            REM Let her rip...
            return% = 90%

            if unix% < 0% then L14083

            call "SUBMIT" addr (post$, wlib$, wvol$, inlib$, "R", "D",   ~
                                                            jclass$, u3%)
            goto L14090

L14083:     call "SUBMIT" addr (post$, rlib$, rvol$, inlib$, "R", "D",   ~
                                                            jclass$, u3%)

L14090:     if u3% = 0% then wait_for_task_to_come_up
            errormsg$(1%) = "SUBMIT can't get task onto Procedure Queue."
            errormsg$(2%) = "Background Task wasn't submitted."
            goto tell_user

        wait_for_task_to_come_up
            if tid$ = " " then get_outa_dodge
            if fn%  = 6%  then get_outa_dodge   /* FN% = 6% departs . .*/
            if no_wait% > 0% then get_outa_dodge

            return% = 80%
            REM Holds user here until above task establishes it's port.
            REM If no response from task in ? minutes, then assume crash.
            time% = 0%
            call "SHOSTAT"                                               ~
            ("Waiting For Background Task Initialization:" & " " & post$)
L14230:     call "MESSAGE" addr ("XM", port$, "TEST", 4%, u3%)
                if u3% = 0% or u3% = 8% then get_outa_dodge
            if time% > timeout% then get_outa_dodge

            if deport$ <> " " then L14260
               u3% = 8%
               call "PAUSE" addr(1000%)
               goto L14280
L14260:     call "MESSAGE" addr ("CH", tempport$, "T", 1000%, message$,  ~
                                 6%, nohelp$, u3%)
L14280:     time% = time% + 10%
            if u3% = 8% and time% < timeout% + 1% then L14230
            /* Return Codes of 0, 12, & 16 are NOT expected */

            errormsg$(1%) = " Background task" & hex(84) & post$ &       ~
                        " wouldn't come up. Check for Operator GETPARMs."
            errormsg$(2%) = " If none, then run task from same logon " & ~
                           "in foreground to find problem."
            tran(errormsg$(), hex(8c20))replacing
            str(errormsg$(1%),76%), str(errormsg$(2%),76%) = hex(84)

        tell_user
            if showerr$ = "N" or ttype$ <> "F" then L14490
            if showerr$ = "S" then call "SHOSTAT" (errormsg$(1%) &       ~
               " " & errormsg$(2%))                                      ~
                            else call "ASKUSER" (2%,"Please Note",       ~
            errormsg$(1%),errormsg$(2%), "Press RETURN to continue input")
            goto L14490

        get_outa_dodge
            REM All done.
            return% = 0
L14490:     if deport$ = " " then exit_routine
            call "MESSAGE" addr ("DE", deport$, u3%)

        REM THISPROGRAMWASGENERATEDBYGENMENUAPROPRIETARYPRODUCTOFCCAELUS*~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * This Program Contains Valuable Trade Secrets And Proprie- *~
            * tary Assets Of CAELUS INCORPORATED, Spokane, WA, Embodying*~
            * Substantial Creative Efforts  And Confidential            *~
            * Information.  Unauthorized Use, Copying, Decompiling,     *~
            * Translating, Disclosure, Or Transfer Of It Is Prohibited. *~
            * Copyright (C) 1985, An Unpublished Work By CAELUS Inc.,   *~
            * Spokane, WA.  All Rights Reserved.                        *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENMENUGENMENUG
        exit_routine
            call "GETUFBS1" addr(#1, u3%)
            if u3% = 1% then close #1
            end