        REM *************************************************************~
            *                                                           *~
            *    CCC   M   M   SSS    CCC   H   H  EEEEE   CCC   K  K   *~
            *   C   C  MM MM  S      C   C  H   H  E      C   C  K K    *~
            *   C      M M M   SSS   C      HHHHH  EEE    C      KKK    *~
            *   C   C  M   M      S  C   C  H   H  E      C   C  K  K   *~
            *    CCC   M   M   SSS    CCC   H   H  EEEEE   CCC   K   K  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CMSCHECK - Enters user into user list/sets user's posting *~
            *            dates to today.  Validates that today is a     *~
            *            valid posting date based on the periods open,  *~
            *            months open, and planning calendar.            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/13/80 ! ORIGINAL                                 ! GLW *~
            * 05/26/83 ! ADD DATE SETTING TO TODAY AUTOMATICALLY  ! GLW *~
            * 01/31/84 ! Change update seq on SYSFILE2 to REWRITE ! ECR *~
            * 04/12/85 ! Added Caelus Proprietary message         ! HES *~
            * 09/12/85 ! Changed to Subroutine                    ! RAC *~
            * 11/01/85 ! Add 5 more module dates                  ! HES *~
            * 12/09/85 ! DYNAMIC LINK TO DATE SETTING ROUTINE     ! KAB *~
            * 08/22/86 ! Added max # users limit, CP type check,  ! LDJ *~
            *          !   Expiration Date check, changed         !     *~
            *          !   check digit algorithm (rewrote),       !     *~
            * 05/02/88 ! Added Reentrant Check                    ! LDJ *~
            * 09/13/88 ! Added Code to support CMSUSRIN           ! LDJ *~
            *          !   enhancement for non-CMS users - bypass !     *~
            *          !   CMS specific processing.  Also changed !     *~
            *          !   a portion of the encryption algorithm  !     *~
            *          !   since it had been compromised.         !     *~
            * 10/21/88 ! Changed Return Code to caller to be      ! LDJ *~
            *          ! more specific - different code for each  !     *~
            *          ! Cancel Condition.                        !     *~
            * 05/31/89 ! Fixed double date formatting if press    ! MJB *~
            *          !  invalid key on System Inactivity Msg.   !     *~
            * 10/05/94 ! Changed to not display Copyright Message ! LDJ *~
            *          ! if running in GUI mode.                  !     *~
            * 12/14/95 ! Now uses ASKGUI if in GUI mode 4 errors. ! LDJ *~
            * 01/17/96 ! Changed SYSDATE_PRIOR_WARNING  test.     ! LDJ *~
            *          ! New users now default from first user.   !     *~
            *          ! No Longer can cause Hang on Held Record. !     *~
            * 08/20/96 ! More changes for GUI Mode                ! LDJ *~
            *          ! (copied from Q60403 unix_only_r2)        !     *~
            * 04/02/97 ! Stripped call(s) to MESSAGE for NT.      ! LDJ *~
            *          ! Pulled dead code out also.               !     *~
            * 08/22/97 ! Millie Conversion                        ! DER *~
            *************************************************************

            sub  "CMSCHECK" (company$,   /* Company Name               */~
                             division$,  /* Divisison within Co.       */~
                             dbadm$,     /* DATA BASE ADMINISTRATOR    */~
                             #3,         /* USERLCMS  (User Parameters)*/~
                             #1,         /* USERINFO  (CMS specific)   */~
                             #2,         /* SYSFILE2  (Company Name)   */~
                             reenter$,   /* Reentering CMSCONTR?       */~
                             err%)       /* Return Code                */

        dim                                                              ~
            askhdr$40,                   /* ASKUSER arguments          */~
            askpf1$80,                   /* ASKUSER arguments          */~
            askmid$80,                   /* ASKUSER arguments          */~
            askpf2$80,                   /* ASKUSER arguments          */~
            ccyymmdd$8,                  /* to extract copyright date  */~
            company$30,                  /* Company Name               */~
            copyrght$4,                  /* Copy Right date            */~
            cmsflag$1,                   /* Is User a CMS User?        */~
            date$8,                      /* SYSTEM DATE                */~
            dbase$8,                     /* User's INLIB               */~
            division$30,                 /* Division within Company    */~
            errormsg$79,                 /* Error message for screens  */~
            frst_dt$8,                   /* first date                 */~
            last_dt$8,                   /* Work                       */~
            licensee$30,                 /* LICENSEE NAME              */~
            mnth1$4,                     /* sys2-mnth open 1           */~
            mnth2$4,                     /* sys2-mnth open 2           */~
            mnth3$4,                     /* sys2-mnth open 3           */~
            name$120,                    /* Company Name               */~
            plowkey$32,                  /* PlowKey                    */~
            reenter$3,                   /* Reentrant Flag             */~
            rest$(4)128,                 /* USRINFO record             */~
            secadm$3,                    /* SYSTEM SECURITY ADMIN      */~
            screen$(24)78,               /* Copyright message          */~
            userid$,                     /* User's Logon Id.           */~
            wu$4                         /* Users write access mask    */~

        dim f2%(2),                      /* File Status Flags For      */~
            f1%(3)                       /* Record-On-File Flags       */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

            call "CHECKGUI" addr(gui%)  /* CHECK IF RUNNING GUI  */
            call "EXTRACT" addr("ID", userid$, "UW", wu$, "IL", dbase$)
            if pos(wu$ <> hex(ff)) = 0% then secadm$ = "YES"

            call "OPENCHCK" (#2, filestatus%, f2%(2%), 0%, " ")
            if filestatus% < 1% then sysfile2_error

            get #3 using L02336, cmsflag$
L02336:     FMT POS(54), CH(1)


        REM *************************************************************~
            *      Display CAI proprietary message onto the screen      *~
            *-----------------------------------------------------------*~
            *                Load user info if available                *~
            *************************************************************
            ccyymmdd% = 0%   /* no compile error */
            date$     = date
            call "DATEFMT" (date$, ccyymmdd%, ccyymmdd$)
            copyrght$ = str(ccyymmdd$, 1%, 4%)


            frst_dt$   = "19010101"
            call "DATECONV" (frst_dt$)
            mnth1$     = bin(198003,4%)
            mnth2$     = bin(198002,4%)
            mnth3$     = bin(198001,4%)

            call "READ100" (#2, "COMPANY TITLE", f1%(2%))
            if f1%(2%) <> 1% then licensee_error

            get #2 using L09090, name$, division$
L09090:     FMT XX(20), CH(120), CH(30)

            if name$  = " " then licensee_error
            licensee$ = str(name$,1%,30%)
            company$  = licensee$

            if reenter$ <> "YES" then gosub L40000


        REM *************************************************************~
            * System Activity Date Validation                           *~
            *************************************************************
            date$ = date
            if cmsflag$ <> "Y" then normal_exit
            call "READ100" (#2, "MONTHS OPEN         ", f1%(2%))
            if f1%(2%) <> 0%    then L10220
            if secadm$  = "YES" then L10110
            if dbadm$  <> "Y"   then months_open_error

L10110:     write #2, using L10190, "MONTHS OPEN",      ~
                                    mnth1$,             ~
                                    mnth2$,             ~
                                    mnth3$,             ~
                                    frst_dt$,           ~
                                    date$,              ~
                                    str(rest$(),1%,456%)

L10190:     FMT     POS(01), CH(20),   ~
                    POS(21), BI(04),   ~
                    POS(25), BI(04),   ~
                    POS(29), BI(04),   ~
                    POS(33), CH(06),   ~
                    POS(39), CH(06),   ~
                    POS(45), CH(456)
            goto L10000

L10220:     get #2, using L10230, last_dt$
L10230:     FMT POS(39), CH(6)

            call "DATEOK" (last_dt$, err%, errormsg$)
            if errormsg$ <> " " then last_dt$ = date ~
                                else call "DATUNFMT" (last_dt$)
            date$ = date

        REM Check For Inactivity...
            call "DATE" addr("G-", str(last_dt$,1%,6%), str(date$,1%,6%),   ~
                             diff%, err%)
            if diff% < 0% then gosub sysdate_prior
            if diff% > 7% then gosub sysdate_after   /*warn if > 1 week*/

        REM Update Last Activity Date...
            call "READ101" (#2, "MONTHS OPEN         ", f1%(2%))
            put #2, using L10380, date
L10380:     FMT POS(39), CH(6)
            rewrite #2


L10000: REM *************************************************************~
            *         U S E R I N F O   V A L I D A T I O N             *~
            *************************************************************

        REM If File Not Open, Ignore It (IF Not Sec. Adm., Then Exit)

            call "OPENCHCK" (#1, userstatus%, f2%(1%), 0%, " ")

            if userstatus% > 0%    then L20140
            if secadm$     = "YES" then L20110
            if dbadm$     <> "Y"   then userinfo_error

L20110:     gosub userinfo_warning

            call "OPENCHCK" (#1, userstatus%, f2%(1%), 1%, " ")
L20140:     call "READ100" (#1, userid$, f1%(1%))
            if f1%(1%) <> 0% then normal_exit

            gosub userrecord_warning
            plowkey$ = " "
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1%))
            if f1%(1%) = 1% then get #1 using L20165, str(rest$(), 1%,150%)
L20165:     FMT CH(150)
            if f1%(1%) = 0% then put #1, using L20200, userid$, ~
                                         date, date, date, date, date,  ~
                                         date, date, date, date, date,  ~
                                         str(rest$(),64%,87%)           ~
                            else put #1, using L20202, userid$, ~
                                        str(rest$(),4%,147%)
L20200:     FMT CH(3), 10*CH(6), CH(87)
L20202:     FMT CH(3), CH(147)
            write #1

            goto normal_exit

        REM *************************************************************~
            * Error, Warnings, Etc Are Displayed Here                   *~
            *************************************************************

        sysfile2_error
            askpf1$ = "The System Control File does not exist in the data~
        ~ base:" & dbase$
            err% = 201%
            gosub display_fatal_error
            goto error_exit

        licensee_error
            askpf1$ = "The Licensee Name is either missing or not authori~
        ~zed for this software."
            askmid$ = "Contact your Caelus, Inc. representative for Assis~
        ~tance."
            err% = 202%
            gosub display_fatal_error
             goto error_exit

        months_open_error
            askpf1$ = "The System Activity Record does not exist in the d~
        ~ata base:" & dbase$
            err% = 206%
            gosub display_fatal_error
            goto error_exit

        userinfo_error
            askpf1$ = "The User Control File does not exist in the data b~
        ~ase:" & dbase$
            err% = 207%
            gosub display_fatal_error
            goto error_exit

        userinfo_warning
            askhdr$ = "* * * W A R N I N G * * *"
            askpf1$ = "The User Control File does not exist " & ~
                      "in the data base:" & dbase$
            askmid$ = "- Press RETURN to Create it and Continue -"
            askpf2$ = "- Press F1 to exit immediately -"
            keyhit% = 0%
            gosub ask_user
            err% = 207%
            if keyhit%  =  1% then error_exit
            if keyhit% <>  0% then userinfo_warning
            gosub L40500
        return

        userrecord_warning
            askhdr$ = "* * * W A R N I N G * * *"
            askpf1$ = "The User Control Record does not exist " & ~
                      "in the data base:" & dbase$
            askmid$ = "- Press F1 to exit immediately -"
            askpf2$ = "- Press RETURN to Create it.- (Additional Input " & ~
                      "may be needed).  "
            keyhit% = 2%
            gosub ask_user
            err% = 208%
            if keyhit%  =  1% then error_exit
            if keyhit% <>  0% then userrecord_warning
            gosub L40500
        return

        sysdate_prior
            call "DATEFMT" (date$):call "DATEFMT" (last_dt$)
            askhdr$ = "SYSTEM DATE PRIOR TO LAST ACTIVITY"
            str(askpf1$,,40%)  = "System  Date:" & date$
            str(askpf1$,41%,40%) = "Date of Last Activity:" & last_dt$
            askmid$ = "- Press RETURN to Continue -"
            askpf2$ = "- Press F1 to exit immediately -"
            keyhit% = 0%
            gosub ask_user
            err% = 209%
            if keyhit%  =  1% then error_exit
            if keyhit% <>  0% then sysdate_prior
            gosub L40500
        return

        sysdate_after
            call "DATEFMT" (date$) : call "DATEFMT" (last_dt$)
            askhdr$ = "SYSTEM DATE SHOWS PERIOD OF INACTIVITY"
            str(askpf1$,,40%)  = "System  Date:" & date$
            str(askpf1$,41%,40%) = "Date of Last Activity:" & last_dt$
            askmid$ = "- Press RETURN to Continue -"
            askpf2$ = "- Press F1 to exit immediately -"
L31070:     keyhit% = 0%
            gosub ask_user
            err% = 210%
            if keyhit%  =  1% then error_exit
            if keyhit% <>  0% then L31070
            gosub L40500
        return


        display_fatal_error
            if askhdr$ = " " then askhdr$ = "* * * E R R O R * * *"
            if askmid$ = " " then askmid$ =                              ~
               "Contact System Administration for Assistance."
            if askpf2$ = " " then askpf2$ =                              ~
               "- Press RETURN to Acknowlege & Exit - "
            gosub tell_user
        return

        tell_user
         /*   if gui% = 0% then                                            ~
             call "ASKUSER" (keyhit%, askhdr$, askpf1$, askmid$, askpf2$)~
            else                                                         ~ */
            call "ASKGUI" (16%, askhdr$, askpf1$ & "  " & askmid$,keyhit%)
        return

        ask_user
      /*      if gui% = 0% then                                            ~
             call "ASKUSER" (keyhit%, askhdr$, askpf1$, askmid$, askpf2$)~
            else                                                         ~
      */      call "ASKGUI" (52%, askhdr$, askpf1$ &                       ~
                  "  Continue with Logon?",keyhit%)
*            if gui% = 0% then return
            if keyhit% = 6% then keyhit% = 0% else keyhit% = 1%
        return

L40000: REM *************************************************************~
            * CMS Disclaimer, Copyright, Etc. Set Up And Displayed Here *~
            *************************************************************
            if gui% = 1% then return

           screen$( 2%) = "========================================" &   ~
                         "====================================="
           screen$( 3%) = "                    Welcome To Caelus Ma" &   ~
                         "nagement Systems                     "
           screen$( 4%) = "========================================" &   ~
                         "====================================="
           screen$( 6%) = "                            Caelus Softw" &   ~
                         "are Systems                          "
           screen$( 8%) = "This software is licensed for exclusive " &   ~
                         "use by " & licensee$
           call "STRING" addr("CT",  screen$(8%), 78%)

           screen$(10%) = "     This software contains valuable tra" &   ~
                         "de secrets and proprietary assets    "
           screen$(11%) = "            of Caelus, Inc., Spokane, Wa" &   ~
                         "., embodying substantial             "
           screen$(12%) = "                creative efforts and con" &   ~
                         "fidential information.               "
           screen$(14%) = "             Unauthorized use, copying, " &   ~
                         "decompiling, translation,            "
           screen$(15%) = "                 transfer, or disclosure" &   ~
                         " of it is prohibited.                "
           screen$(17%) = "                 CMS is an unpublished w" &   ~
                         "ork by Caelus, Inc.                  "
           screen$(19%) = "Copyrights 1982, 1983, ... " & copyrght$
           call "STRING" addr("CT", screen$(19%), 78%)
           screen$(20%) = "                             All rights " &   ~
                         "reserved.                            "
           screen$(21%) = "========================================" &   ~
                         "====================================="
           screen$(22%) = "                       User Validation I" &   ~
                         "n Progress...                        "
           screen$(23%) = "========================================" &   ~
                         "====================================="

L40500: REM *************************************************************~
            * Display For Redisplay After Askuser                       *~
            *************************************************************

            display at (01,03),   screen$(1%),                           ~
                    at (02,03),   screen$(2%),                           ~
                    at (03,01),   hex(84), screen$(3%),                  ~
                    at (04,03),   screen$(4%),                           ~
                    at (05,03),   screen$(5%),                           ~
                    at (06,03),   screen$(6%),                           ~
                    at (07,03),   screen$(7%),                           ~
                    at (08,01),   hex(84), screen$(8%),                  ~
                    at (09,01),   hex(84), screen$(9%),                  ~
                    at (10,03),   screen$(10%),                          ~
                    at (11,03),   screen$(11%),                          ~
                    at (12,03),   screen$(12%),                          ~
                    at (13,03),   screen$(13%),                          ~
                    at (14,03),   screen$(14%),                          ~
                    at (15,03),   screen$(15%),                          ~
                    at (16,03),   screen$(16%),                          ~
                    at (17,03),   screen$(17%),                          ~
                    at (18,03),   screen$(18%),                          ~
                    at (19,03),   screen$(19%),                          ~
                    at (20,03),   screen$(20%),                          ~
                    at (21,03),   screen$(21%),                          ~
                    at (22,01),   hex(84), screen$(22%),                 ~
                    at (23,03),   screen$(23%),                          ~
                    at (24,03),   screen$(24%)

            return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Return To The Calling Program                            ,*~
            *************************************************************

        error_exit
            err% = max(err%,1%)
            goto L65130
        normal_exit
            err% = 0%
L65130:     if f2%(1%) = 0% then close #1
            if f2%(2%) = 0% then close #2
            end
