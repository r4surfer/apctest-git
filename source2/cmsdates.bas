        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   CCC   M   M   SSS   DDDD    AAA   TTTTT  EEEEE   SSS    *~
            *  C   C  MM MM  S      D   D  A   A    T    E      S       *~
            *  C      M M M   SSS   D   D  AAAAA    T    EEEE    SSS    *~
            *  C   C  M   M      S  D   D  A   A    T    E          S   *~
            *   CCC   M   M   SSS   DDDD   A   A    T    EEEEE   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CMSDATES - SET USER POSTING DATES.  THIS PROGRAM IS YET   *~
            *            ANOTHER DESCENDENT OF BEGIN, WITH A TWIST.  THE*~
            *            USER MUST BE ESTABLISHED IN USERINFO BY        *~
            *            CMSCONTR OR IT BOMBS WITH NON-ZERO RETURN.  ANY*~
            *            OTHER ERROR DOES LIKEWISE.                     *~
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
            * 12/16/85 ! ORIGINAL                                 ! KAB *~
            * 09/23/86 ! Changed Checks for Open SA Periods       ! ERN *~
            * 04/05/89 ! Fixed bad branch on KEYHIT% <> 1 or 0    ! MLJ *~
            * 04/03/92 ! PRR 10866 - Added calendar error codes   ! MLJ *~
            *          !   (FY or SA) to ASKUSER Posting Dates.   !     *~
            *          ! PRR 11632 - Added ASKUSER Roll Warning   !     *~
            *          !   when in last period of FY calendar.    !     *~
            * 11/05/93 ! Added warning if std cost set implementa-! JDH *~
            *          !   tion has failed.                       !     *~
            * 04/04/95 ! Changed date to test against for whether ! JDH *~
            *          !   period 17 is about to end.             !     *~
            * 12/11/96 ! Changes for System Navigator. If in GUI  ! LDJ *~
            *          ! mode uses ASKGUI for error messages.     !     *~
            * 05/20/97 ! Removed all ASKUSER calls.               ! LDJ *~
            * 05/18/97 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            askhdr$40,                                                   ~
            askpf1$80,                                                   ~
            askmid$80,                                                   ~
            askpf2$80,                                                   ~
            date$8,                      /* System date                */~
            date17$8,                    /* Period 17 start date       */~
            dbase$8,                     /* User's INLIB               */~
            dbadm$1,                     /* DATA BASE ADMINISTRATOR    */~
            saperiods$(13)6,             /* Sa Periods                 */~
            secadm$3,                    /* SECURITY                   */~
            ue$4,                        /* Users execute access mask  */~
            ur$4,                        /* Users read access mask     */~
            userid$,                     /* User's Logon Id.           */~
            uw$4,                        /* Users write access mask    */~
            why$4,                       /* Calendar error (FY or SA)  */~
            yymmdd$8                     /* Work                       */

        dim f2%(03),                     /* FILE STATUS FLAGS FOR      */~
            f1%(03)                      /* RECORD-ON-FILE FLAGS       */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! User Information file                    *~
            * # 2 ! SYSFILE2 ! System Information file                  *~
            * # 3 ! CALMASTR ! Planning Calander                        *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #2,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #3,  "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos = 1, keylen = 2

        REM STRAIGHT ON INTO INITIALIZATION AND TESTING

            call "EXTRACT" addr("ID", userid$, "UE", ue$, "UR", ur$,     ~
                                                 "UW", uw$, "IL", dbase$)
            if pos(ue$<>hex(ff)) <> 0 then L09072
               if pos(ur$<>hex(ff)) <> 0 then L09072
                  if pos(uw$<>hex(ff)) <> 0 then L09072
                     secadm$ = "YES"

L09072:        call "CMSDBADM" (dbadm$, " ")

            call "OPENCHCK" (#1, userstatus%, f2%(1), 0%, " ")
                if userstatus% < 1% then error_exit

            call "OPENCHCK" (#2, sysfilestatus%, f2%(2), 0%, " ")
                if sysfilestatus% < 1% then error_exit

            call "READ100" (#1, userid$, f1%(1))
                if f1%(1) = 0% then error_exit

        REM NOW CHECK THINGS OUT
            call "READ100" (#2, "STCIMPLE.CONTROL    ", imple_on%)
            if imple_on% = 1% then gosub std_cost_impl_warning

            date$ = date  :  date17$ = " "
            why$ = " "

            call "WHICHPER" (#2, date$, period%)
            if period% >0% and period% <17% then goto L10080
                if period% = 0% then L10035
                    call "READ100" (#2, "FISCAL DATES       ", f1%(2))
                        if f1%(2) = 0% then L10035
                    get #2 using L10025, date17$
L10025:                 FMT POS(409), CH(8)

                    call "DATE" addr( "G-", date$, date17$, dl%, ds%)
                    if dl% > 10% and ds% = 0% then L10080

                    call "DATEFMT" (date17$)
                    gosub roll_warning
                    goto L10080
L10035:         why$ = "(FY)"
                if secadm$ = "YES" then L10060
                     if dbadm$ <> "Y" then window_error
L10060:                   gosub window_warning
                          goto L10210
L10080:     call "SAPERIOD" (date, #2, saperiods$(), period%, err$)
            if err$ = " " then L10140
                why$ = "(SA)"
                if secadm$ = "YES" then L10060
                     if dbadm$ <> "Y" then window_error
                          goto L10060

L10140:     call "READ101" (#1, userid$, f1%(1))
                 if f1%(1) = 0% then error_exit
            put #1, using L10180, date, date, date, date, date,           ~
                                 date, date, date, date, date
L10180:         FMT POS(4), 10*CH(6)
            rewrite #1

L10210: REM VALIDATE PIP CALENDAR

            yymmdd$="010101"
            call "OPENCHCK" (#3, calstatus%, f2%(3), 0%, " ")
                if calstatus% < 1% then L10320

            call "READ100" (#3, "10", f1%(3))
                if f1%(3)=0 then L10320
            get #3, using L10300, yymmdd$
L10300:         FMT XX(2),CH(6)

L10320:     date$=date
            call "DATE" addr("G-",str(yymmdd$,,6), str(date$,,6),        ~
                               diff%,err%)
            if err% <> 0%   then diff% = -1%
            if diff% < 0%   then L10380
            if diff% < 490% then L10420
L10380:         if secadm$ = "YES" then L10400
                   if dbadm$ <> "Y" then calendar_error
L10400:               gosub calendar_warning

L10420:  REM CALENDAR BASE DATE
            call "READ101" (#2, "MONTHS OPEN", f1%(2))
                 if f1%(2) = 0% then error_exit
            put #2, using L10460, yymmdd$
L10460:     FMT POS(33), CH(6)
            rewrite #2
            goto normal_exit

calendar_warning

            askhdr$ = "Today's Date is outside the PIP Calendar!"
            askpf1$ = "Transaction entry is disabled until the " & ~
                      "Production Calendar is Rolled!"
            askmid$ = "  Select OK to continue Logon to correct this problem " &  ~
                       "or select CANCEL to Exit/Logoff now."
            call "ASKGUI" (49%,askhdr$,askpf1$ & askmid$,keyhit%)
            if keyhit% = 2% then error_exit
            if keyhit% <> 1% then calendar_warning
            return

calendar_error

            askhdr$ = "Today's Date is outside the PIP Calendar!"
            askpf1$ = "Logon is disabled until the problem is corrected by rolling the Calendar!"
            askmid$ = "  Please ask the CMS Administrator to correct this problem immediately!"
            call "ASKGUI" (16%,askhdr$,askpf1$ & askmid$,keyhit%)
            goto error_exit

window_warning
            askhdr$ = "Posting Date(s) Error!"
            call "ASKGUI" (49%,askhdr$,"The System is not able to set your Posting Dates " ~
                 & why$ & "  Select OK to Continue (Correct this Condition Immediately) or CANCEL to Exit/Logoff.",keyhit%)
            if keyhit% = 2% then error_exit
            if keyhit% <> 1% then window_warning
            return

roll_warning
            askhdr$ = "WARNING - Last Period of Fiscal Year!"
            askpf1$ = "Your Fiscal Year must be rolled by " & date17$ & "! "
            askmid$ = "You are in the last possible Current Period!"
            askpf2$ = "  Select OK to Continue or CANCEL to Exit/Logoff."
            call "ASKGUI" (49%,askhdr$,askpf1$ & askmid$ & askpf2$,keyhit%)
            if keyhit% = 2% then error_exit
            if keyhit% <> 1% then roll_warning
            return


window_error
            askhdr$ = "Posting Date(s) Error!"
            askpf1$ = "The System is not able to set your Posting Dates " & why$
            askmid$ = "  Contact the System Administrator for Assistance."
            call "ASKGUI" (16%,askhdr$,askpf1$ & askmid$,keyhit%)
            goto error_exit


std_cost_impl_warning
            askhdr$ = "Standard Cost Implementation Incomplete!"
            askpf1$ = "Your Last Cost Set Implementation did NOT complete!"
            askmid$ = "This MUST be corrected as soon as possible!"
            askpf2$ = "  Select OK to Continue or CANCEL to Exit/Logoff."
            call "ASKGUI" (49%,askhdr$,askpf1$ & hex(0d) & askmid$ & askpf2$,keyhit%)
            if keyhit% = 2% then error_exit
            if keyhit% <> 1% then std_cost_impl_warning
            return



        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

error_exit
                return% = 99%
                goto L65230

normal_exit
                return% = 0%

L65230:     end  return%
