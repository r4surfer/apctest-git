        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   EEEEE  TTTTT   CCC    OOO   DDDD   EEEEE          *~
            *  G      E        T    C   C  O   O  D   D  E              *~
            *  G GGG  EEEE     T    C      O   O  D   D  EEEE           *~
            *  G   G  E        T    C   C  O   O  D   D  E              *~
            *   GGG   EEEEE    T     CCC    OOO   DDDD   EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GETCODE  -  Works exactly like describe, except that if   *~
            *             the code passed is not on file, the user is   *~
            *             put into an inquiry mode that allows him to   *~
            *             search for the code he wants.                 *~
            *             Searchs on descriptions are allowed if these  *~
            *             conditions are met...                         *~
            *          A) Description must immediately follow prime key *~
            *          B) Description must be an alternate key in file  *~
            *          C) Alt key must be in the file select of caller  *~
            *          D) Description length *should* be less then 51   *~
            *          E) Description must be described correctly in    *~
            *             'CALL' statement of caller (see below).       *~
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
            * 01/18/84 ! ORIGINAL (SEMI-CONED FROM DESCRIBE)      ! HES *~
            * 03/20/85 ! Re-formatted screen, no more X box logic ! HES *~
            * 04/22/85 ! Added WSXIO                              ! HES *~
            * 05/09/85 ! Added Scroll Feature                     ! HES *~
            * 07/15/85 ! Added special provisions for GLMAIN file ! HES *~
            * 09/26/86 ! Added LDJ's Soundex Routine              ! HES *~
            * 10/04/86 ! Modified Call to SCREEN - now compatible ! LDJ *~
            *          !   with O.S. 7.10.                        !     *~
            * 03/16/87 ! Replaced call to RJuSTIFY w/ STRING      ! ERN *~
            * 05/22/87 ! Support Of Obsolete Flag On GLAMIN       ! HES *~
            * 09/10/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 04/20/89 ! If HNYMASTR, enable ALT KEY on descr.    ! JDH *~
            *          !    Done to Domestic 09/15/87 by HES      !     *~
            * 10/23/89 ! OS 7.20/30 Compatability (KAB 4-19-89)   ! MJB *~
            * 01/31/92 ! Cleaned Up GLMAIN processing in 'Search  ! KAB *~
            *          !   for Code' section.  Added immunization !     *~
            *          !   for possible returned psuedo blanks.   !     *~
            * 08/02/93 ! UNIX mod for screen printing.            ! JDH *~
            * 06/20/94 ! Platform Sensitive for Keyboard Interrupt! KAB *~
            * 04/04/95 ! Abort find not working due to disabled   ! KAB *~
            *          !   Keyboard.  (Premature termination on   !     *~
            *          !   Unix).  Write Screen @ 38700 fixes it. !     *~
            * 11/13/95 ! Added GUI Features when in CoStar env.   ! LDJ *~
            *          ! Changed numeric constants to integers.   !     *~
            * 03/28/97 ! Added ListBox for files                  ! LDJ *~
            * 06/10/97 ! Added (F4)Prev Screen to move back 1 scrn! RJH *~
            * 06/19/97 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

*        NOTE-  Description must immediately follow Primary Key, and both
*          must fall within the first *500* bytes of the record for
*          the file to work with this routine... If the descr doesn't
*          follow the prim key, customizatiom is required to this
*          subroutine in (and as shown in) the 'DESCRIBE_IT' section.


            sub "ZGETCODE" (#1,          /* FILE TO SEARCH             */~
                                                                         ~
                          key$,          /* COMING IN: RECORD TO SEARCH*/~
                                         /* FOR. GOING OUT: RECORD     */~
                                         /* SELECTED, OR UNCHANGED IF  */~
                                         /* KEY PASSED IN IS ON FILE   */~
                                                                         ~
                          descr$,        /* SELECTED RECORDS DESCRIPT  */~
                                         /* GOING OUT. SCREEN HEADER   */~
                                         /* COMING IN IF FIRST BYTE IS */~
                                         /* HEX(06).                   */~
                                                                         ~
                          flag%,         /* NON ZERO CAUSES PARENTHESIS*/~
                                         /* TO BE PUT AROUND RETURNED  */~
                                         /* DESCRIPTION                */~
                                                                         ~
                          altkeydescr,   /* FLOATING POINT WHERE THE   */~
                                         /* INTEGER PORTION IS THE ALT */~
                                         /* KEY NUMBER OF DESCRIPTION, */~
                                         /* TAKEN DIRECTLY OFF THE     */~
                                         /* FILES SELECT STATEMENT IN  */~
                                         /* THE CALLING PROGRAM.       */~
                                         /* *ZERO* INDICATES DESCRIPT  */~
                                         /* IS NOT AN ALTERNATE KEY.   */~
                                         /* FRACTION OF NUMBER IS THE  */~
                                         /* DESCRIPTION LENGTH.        */~
                                         /* .30 OR .3 MEANS THIRTY,    */~
                                         /* .03 MEANS THREE, ETC.      */~
                                         /* 30 IS THE DEFAULT, HNYMASTR*/~
                                         /* HAS A SPECIAL CHECK IN     */~
                                         /* HERE TO GET 32, BUT OTHER  */~
                                         /* FILES HAVING A DESCRIPTION */~
                                         /* NOT 30 MUST HAVE THE LENGTH*/~
                                         /* SPECIFIED HERE, OR 30 WILL */~
                                         /* BE SHOWN. THIS IS INDEPEND-*/~
                                         /* ENT OF WHETHER THE DESCR IS*/~
                                         /* AN ALT OR NOT.             */~
                                         /* SUMMARY: IF DESCRIPTION IS */~
                                         /* 30 AND NOT AN ALT, PASS IN */~
                                         /* ZERO. IF DESCR IS NOT 30   */~
                                         /* AND NOT AN ALT, PASSING IN */~
                                         /* A NUMBER LESS THEN ONE. IE.*/~
                                         /* 0.40, 0.50, 0.25, ETC.     */~
                                         /* IF DESCR IS AN ALT AND LEN */~
                                         /* IS THIRTY, PASS IN THE ALT */~
                                         /* KEY NMBR ONLY, IE. 3.0, 1.0*/~
                                         /* IF DESCR IS AN ALT, AND LEN*/~
                                         /* IS NOT THIRTY, PASS ALT KEY*/~
                                         /* NMBR & LEN, IE. 3.40, 1.25 */~
                                                                         ~
                                         /* **PASS THIS NUMBER IN AS   */~
                                         /* GREATER OR EQUAL TO 17, OR */~
                                         /* AS A NEGATIVE, AND THIS    */~
                                         /* SUBROUTINE WILL WORK       */~
                                         /* *EXACTLY* LIKE 'DESCRIBE'  */~
                                                                         ~
                          f1%)           /* RETURN STATUS FOR RECORD.  */~
                                         /* 1 = RECORD FOUND/SELECTED  */~
                                         /* 0 = REC NOT ON FILE. ONLY  */~
                                         /* POSSIBLE IF PF 16 IS PRESSD*/~
                                         /* IF -999% COMING IN, THEN   */~
                                         /* 'SKIP' LOGIC IS SHUT OFF   */~

            dim order$4,                 /* SCREEN ORDER AREA          */~
                s$(24)80,                /* CURRENT SCREEN TEXT ARRAY  */~
                aid$1,                   /* WORKSTATION AID BYTE (PFK) */~
                afac$1,                  /* SCREEN FAC                 */~
                alternate$(50,1)1,       /* KEY TO SEARCH ALTS FOR     */~
                altmsg$27,               /* SCREEN FAC                 */~
                blankdate$8,             /* Blank Date for Comparison  */~
                command$256,             /* API Commands to CoStar     */~
                descr$70,                /* RETURNED DESCRIPTION       */~
                file$8,                  /* RECEIVER FOR FILE NAME     */~
                find_code$4,             /* Soundex Searching For      */~
                iosw$8,                  /* IO WORD STATUS BYTES       */~
                key$70,                  /* READ KEY                   */~
                keys$16,                 /* Active Pf Keys             */~
                lib$8,                   /* RECEIVER FOR LIBRARY NAME  */~
                p%(1),                   /* SEARCH VARIABLE            */~
                pfac$1,                  /* SCREEN FAC                 */~
                pgmmsg$78,               /* DISPLAY MESSAGE FROM CALLER*/~
                phonic_code$4,           /* Soundex Value              */~
                primary$(30,1)1,         /* CODE TO SEARCH FOR         */~
                program$8,               /* Main Program Name          */~
                readkey$75,              /* KEY FOR PLOW               */~
                text$70,                 /* WORK AREA.                 */~
                test$70,                 /* DESCRIPTION WORK AREA.     */~
                test1$70,                /* DESCRIPTION WORK AREA.     */~
                ufbkl$1,                 /* KEY LENGTH THIS FILE       */~
                ufbkd$2,                 /* KEY DISPLACEMENT REL TO 0  */~
                uw$1,                    /* CoStar MAGIC Char - x7F    */~
                version$6,               /* Test Platform              */~
                vol$6,                   /* RECEIVER FOR VOLUME NAME   */~
                work$(5)100              /* WORK AREA FOR TRANSFER     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            select #5,  "CRTFILE",                                       ~
                        consec,                                          ~
                        recsize = 1924

*        Determine Platform for this routine
            if unix% <> 0% then L02100   /* Done only once */
            call "EXTRACT" addr("S#",version$)
            convert version$ to unix%, data goto L02090
               goto L02100
L02090:           unix% = -1%

            blankdate$ = " "
            call "DATUFMTC" ( blankdate$ )

L02100
*        Now if UNIX% >= 0% this is a VS Platform, otherwise its UNIX
            call "EXTRACT" addr("CF", program$)
*        Determine if in GUI Environment *
            call "CHECKGUI" addr(gui%)
            uw$=hex(7f)
            listbox% = 0%   /* Listbox used flag */
            REM Processing begins here.
            init (hex(00)) ufbkl$, ufbkd$ : file$ = " "
            f5% = 1% /* Screen is closed to start with */
            call "GETNAMES" addr(#1, file$, lib$, vol$)
            call "GETUFBKL" addr(#1, ufbkl$)       /* KEY LENGTH (BIN) */
            call "GETUFBDK" addr(#1, ufbkd$)       /* KEY DISPL. (BIN) */
            disp% = val(ufbkl$)+val(ufbkd$,2) + 1%
            if disp% > 468% then end
            if file$ <> "HNYMASTR" then L10800
                REM Enable searches on description if HNYMASTR file...
                if altkeydescr < 1 then altkeydescr = altkeydescr + 3
L10800:     alt%    = int(altkeydescr)
            altlen% = mod(abs(altkeydescr)*100,100)+.1
            if altlen%= 0% then altlen%= 30% /*DEFAULT DESCRIPTION LEN */
            if f1% = -999% then skip% = f1% else skip% = 0%
            if key$ = " " then L12600

            REM If it's on file, then wip right back with minimun effort
            readkey$ = " "
            if file$ = "GLMAIN"                                          ~
                then call "GLVALID" (key$,"         ", readkey$)
            if file$ = "GLMAIN2"                                         ~
                then call "GLVALD2" (key$,"         ", readkey$)
            if file$ = "GLMAIN"  and readkey$ <> " " then key$ = " "
            if file$ = "GLMAIN2" and readkey$ <> " " then key$ = " "
            if readkey$ <> " " then L12600
            readkey$ = key$
            if file$ = "GLMAIN" then call "GLUNFMT" (readkey$)
            if file$ = "GLMAIN2" then call "GLUNFM2" (readkey$)
            call "READ100" (#1, readkey$, f1%)
            if f1% = 0% then L12600
                gosub describe_it
                if skip% > 0% then L12600
                goto direct_hit

L12600: REM *************************************************************~
            *              M A I N   P L O W   L O O P                  *~
            *-----------------------------------------------------------*~
            * CODE PASSED IS NOT ON FILE, FIND & SHOW'EM WHAT IS....    *~
            *************************************************************
*           CALL "GUICHENV" ("getcode", 0%)
            REM Initialize variables for display logic...
            if altkeydescr < 0 or altkeydescr > 16 then all_done
            f1%, key%, anyhits%, c%, mode%, no_valid_data%=0%
            row% = 5%
            if gui% = 1% then col% = 4% else col% = 2%
            text$, alternate$(), altmsg$, s$() = " "
            pgmmsg$ = "Use F16 to cancel search and return for " &    ~
                      "manual entry of code."
            if str(descr$,,1%) = hex(06) then pgmmsg$ = str(descr$,2%)


            REM Split logic... try to get close to the desired record.
            adjlen%, pklen% = val(ufbkl$)
            if file$ = "GLMAIN" or file$ = "GLMAIN2" then adjlen% = 16%
            str(readkey$,,pklen%) = key$ addc all(hex(ff))
            if key$ = "?" or key$ = " " then readkey$ = all(hex(00))

            REM Redimension variables to passed/extracted key lengths
            mat redim primary$(min(30%,adjlen%), 1%)1%
            primary$() = key$
            mat redim alternate$(min(50%,altlen%),1%)1%
            if gui% = 0% then plow_codes
            rem *** Get User Specified Limit on Using Listbox from Environment
            call "CMSLBOX" addr(lb_limit%)
            rem *** get number of records in primary file ***
            call "READFDR"  addr(file$, lib$, vol$, 0%,"RC", rc%, return%)
            rem *** if number of records < $CMSLBOX, then use a GUI Listbox ***
            if rc% < lb_limit% and return% = 0% then listbox% = 1%

plow_codes
            readcount% = 0%
            if pos(readkey$<>hex(00)) = 0% then no_valid_data% = 1%
            call "PLOWALTS" (#1, readkey$, key%, 0%, f1%)
                 if f1% = 0% then end_of_data
            if listbox% =0% then L15600
               rem *** Setup Listbox ***
                rem *** Hide Screen Cursor ***
                call "SENDCMD" (uw$ & "UWSETUP3,1" & uw$)

                command$ = uw$ & "UWSCRIPTcaelus/sdk/listbox.scr,4,"
                if str(descr$,,1%) <> hex(06) then pgmmsg$ = "Select a Code"
                command$ = command$ & pgmmsg$ & ",1,04,10,22,75,CoStar" & uw$
                call "SENDCMD" (str(command$,,len(command$)))
L15600:     gosub describe_it
            if skip% > 0% then read_next
            no_valid_data% = 0%
            if mode% = 0% then L18000
                readcount% = readcount% + 1%
                if mod(readcount%, 50%) <> 0% then L16700
                REM Check For Workstation Interupts...
                iosw$ = all(hex(00))

*       *** Platform Sensitive Code
                if unix% < 0% then L16450
                   call "WSXIO" addr("W", #5, 1%, iosw$)
                      goto L16480
L16450:            call "WSXIO" addr("A", #5, str(iosw$,3%,1%))
                      if str(iosw$,3%,1%) < hex(40) then                 ~
                         str(iosw$,3%,1%) = hex(00)
L16480
*       *** Platform Sensitive Code

                if str(iosw$,3,1%) > hex(00) then show_codes

L16700:         REM In Special Search Mode...
                if mode% = 2% then L17500
                call "SOUNDEX" (text$, phonic_code$)
                if phonic_code$ = find_code$ then L18000
                test1$ = str(text$,,len(test$)) or all(hex(20))
                if test$ = test1$ then L18000
                goto read_next

L17500:         REM String Search...
                test1$ = text$ or all(hex(20))
                search test1$ = test$ to p%()
                if p%(1%) <> 0% then L18000
                goto read_next
L18000:     c% = c% + 1%
            anyhits% = 1%
            if listbox% = 0% then Build_CHUI
                rem *** send data to listbox api ***
                if file$ = "GLMAIN" then call "GLUNFMT" (readkey$)
                if file$ = "GLMAIN2" then call "GLUNFM2" (readkey$)
                tran(text$,";,'" & hex(22))replacing
                command$ = str(readkey$,,adjlen%) & "  " & text$ & "," & ~
                       str(readkey$,,adjlen%) & ","
                call "SENDCMD" (str(command$,,len(command$)))
                goto read_next

Build_CHUI: s$(c%+4%) = hex(860b8c)& str(readkey$,,adjlen%)& "   "& text$
            if file$ = "GLMAIN" then call "GLFMT" (str(s$(c%+4%),4%,16%))
            if file$ ="GLMAIN2" then call "GLFMT2" (str(s$(c%+4%),4%,16%))
            if key%<>0% then s$(c%+4%)=hex(860b8c)&str(readkey$,,altlen%)~
                          & "   " & str(work$(),1%+val(ufbkd$,2),adjlen%)
            if gui% = 0% then L18600
            str(s$(c%+4%),1%,3%) = hex(2086b4)
            if key% = 0% then                                            ~
               str(s$(c%+4%),4%+adjlen%,3%) = hex(2020b4)                ~
            else                                                         ~
               str(s$(c%+4%),4%+altlen%,3%) = hex(2020b4)
L18600:     if c% = 15% then show_codes

read_next:  read #1, eod goto end_of_data
            readkey$ = key(#1, key%)
            goto L15600

        end_of_data
            if no_valid_data% = 1% then end /* EMPTY,OR NOTHING USABLE */
            goto show_codes

        read_previous_screen
           if c% > 0% and c% < 16% then stop% = c% + 15% else stop% = 31%
           for i% = 1% to stop%    /* Backup a screen full */
               read #01, key key%  < key(#01, key%), eod goto ps_done
               readkey$ = key(#01, key%)
            next i%
            return

        ps_done  /* ended at start of file */
            readkey$ = hex(00)
            return

        describe_it
            get #1, str(work$())                  /* RECORD WORK AREA */
            text$ = str(work$(), disp%, altlen%)
            if file$ = "HNYMASTR" then text$ = str(work$(), disp%, 32%)
            if file$ <> "JBMASTR2" then L20500
                str(text$,35%)=str(work$(),153%,6%)
                if str(text$,35%) = " " or str(text$,35%) = blankdate$ ~
                   then str(text$,35%) = "  Open  " else ~
                call "DATEFMT" (str(text$,35%))
L20500:     if file$ = "PRLDDT  " then text$ = str(work$(), 351%,altlen%)

*          IF FILE$ = "XXXXXXXX" THEN TEXT$ = STR(WORK$(), X, ALTLEN%)
*          Above is how to access a file that's description does not
*          immediately follow the primary key.
*          in this case, the description shouldn't be specified as an
*          alternate key when calling GETCODE

            REM 'Skip' record checks...
            if skip% <> -999% then L21600
            if file$ <> "GLMAIN" and file$ <> "GLMAIN2" then L22000
                if str(work$(),41%,1%) = hex(01) or                      ~
                   str(work$(),41%,1%) = hex(02) then                    ~
                   str(text$,40%) = "(Obsolete)"
                goto L22000
L21600:     skip% = 0%
            if file$ <> "GLMAIN" and file$ <> "GLMAIN2" then L22000
                if str(work$(),41%,1%) = hex(01) then skip% = 1%
                if str(work$(),41%,1%) = hex(02) then skip% = 1%
L22000: return

show_codes
           on listbox% goto Show_Listbox
           if keyhit% = 7% then L31400 /* We're In Scroll Mode */
           init(hex(9c)) pfac$, afac$
           keys$ = hex(0001020405070806090a0d0f2010)
           if alt% = 0% then str(keys$,10%,1%) = hex(ff)
           if c% + anyhits% = 0% then L39600 /* DON'T WANT END OF FILE */
                             /* MESSAGE TO COME UP IF WE JUST CAME IN */

           if f5% = 0% then L23300 /* Need To Open Workstation? */
           close ws : call "WSXIO" addr("O", 255%, "Y", #5, x%)
                      f5% = 0%
L23300:    if c% > 0% then L23700
              str(s$(11%),27%) = "****  END OF FILE ****"
              row%, col% = 1%

L23700:    s$(1%) = " Position cursor (Tab) to line and press (RETURN)" &~
                    " to return with that code."
           s$(2%) = hex(208c) & str(pgmmsg$)
           if gui% = 1% then str(s$(2%),2%,1%) = hex(94)
           REM Set up title line depending on what key path we're on...
           s$(3%) = hex(ac) & "  Code"  /* SET FOR PRIMARY KEY ORDER */
           str(s$(3%),7%+adjlen%) = "Description"
           if mode% = 0% then L24900
              str(s$(4%),6%+adjlen%) = hex(ac) & "Sounds Like: " & test$
              if mode%=2% then str(s$(4%),7%+adjlen%)="Contains: "&test$
              s$(4%) = s$(4%) & hex(8c)
L24900:    if key% = 0% then L25300
           s$(3%) = hex(ac) & "  Description" /*SET FOR ALT KEY ORDER*/
           str(s$(3%),7%+altlen%) = "Code"

L25300:    REM Tack on file name for hell of it...
           if file$ = "JBMASTR2" then str(s$(3%),49%) = "Closed On"
           str(s$(3%),65%) = "FILE: " & file$
           if gui%=0% then s$(21%)=hex(ac) & "Available Functions Are :" ~
                      else s$(21%) = " "
L25900:    REM Set up bottom of screen stuff...
L26000:    s$(22%) = " (2)First (4/5)Prev/Next (7)Scroll (13)Instructi" &~
                     "ons (15)Prnt Scrn (16)Cancel"
           if c% < 15% and c% > 0% then str(s$(22%),11%,25%) = " "
           s$(23%)= " (8)Search For Code:" &pfac$&str(primary$())&hex(8c)
*         STR(S$(23%),48%) = "Cursor & RETURN: Return With Code"
           if alt% <> 0% then s$(24%) = hex(8c) & "Description Searches"&~
                    ":  (6)Sounds Like   (9)Word Search   (10)Sort Order"
           if alt% = 0% then s$(24%) = hex(8c) & "Description Searches" &~
                    ":  (6)Sounds Like   (9)Word Search"
           if gui% = 1% then str(s$(24%),,22%) = " "
           if afac$ = hex(9c) then L27500
           if mode%=0% then s$(24%) = hex(8c) & "Description Starts With:"
           if mode%=1% then s$(24%) = hex(8c) & "Description Sounds Like:"
           if mode%=2% then s$(24%) = hex(8c) & "Descriptions Containing:"
           str(s$(24%),26%) = afac$ & str(alternate$()) & hex(8c)

L27500:    REM Write screen...
           order$ = hex(01a0) & bin(col%,1) & bin(row%,1)
           call "WSXIO"addr("X", #5, hex(80), order$, s$(), 1920%, iosw$)
           on gui% + 1% goto L27800
           REM *** now draw a pretty box using VT terminal ESC codes ***
*          CALL "DRAWBOX" (1%,1%,20%,80%)
L27800:    REM Read screen...
           order$ = hex(17000000)
           call "WSXIO" addr("X", #5, hex(40), order$, str(s$(),1761%),  ~
                                                            rlen%, iosw$)
           keyhit% = val(str(iosw$,3%),1) - 64%
           if keyhit% > 16% then keyhit% = keyhit% - 16%
           if keyhit% <> 1% then L28800
              tran(s$(), hex(868d))replacing
              row% = 5%
              if gui% = 1% then col% = 4% else col% = 2%
              goto show_codes
L28800:    aid$ = bin(keyhit%,1)
           search keys$ = aid$ to p%()
               if p%(1%) = 0% then L25900
           col% = val(str(order$,3%),1)
           row% = val(str(order$,4%),1)

           REM Process keyhit...
            if keyhit% =  1% then abort_search  /* PROCESSING ABORTED */
            if keyhit% = 16% then abort_search  /* PROCESSING ABORTED */
            if keyhit% = 32% then abort_search  /* PROCESSING ABORTED */

            REM Display Instructions
            if keyhit% <> 13% then L30700
               call "WSXIO" addr("C", #5)
               close ws
               call "ZMANUAL" ("GETCODE")
               close ws
*             CALL "GUICHENV" ("getcode", 0%)
               call "WSXIO" addr("O", 255%, "Y", #5, x%)
               goto  L25900

L30700:     REM Print Screen Option
            if keyhit% <> 15% then L31400
               call "WSXIO"  addr("C", #5)
               call "ZPRNTSCR"
               call "WSXIO"  addr("O", 255%, "Y", #5, x%)
               goto  L25900

L31400:     REM Scroll Screen Option
            if keyhit% <> 7% then L32600
               if c% < 15% then L27500
               order$ = hex(05a0) & bin(col%,1) & bin(row%,1)
               call "WSXIO" addr("X",#5, hex(80), order$, str(s$(),321%),~
                                                            1200%, iosw$)
               iosw$ = all(hex(00))

*       *** Platform Sensitive Code
               if unix% < 0% then L32150
                  call "WSXIO" addr("W", #5, 1%, iosw$)
                     goto L32180
L32150:           call "WSXIO" addr("A", #5, str(iosw$,3%,1%))
                     if str(iosw$,3%,1%) < hex(40) then                  ~
                        str(iosw$,3%,1%) = hex(00)
L32180
*       *** Platform Sensitive Code

               if str(iosw$,3%,1%) > hex(00) then L27500
               copy str(s$(),401%,1120%) to str(s$(),321%,1120%)
               s$(19%) = " " : c% = 14% : goto read_next /* Repeat */

L32600:     if keyhit% <> 8% then L33500
               pfac$ = hex(81)   /* Enable For Search */
               if gui% = 1% then primary$() = " "
               tran(primary$(), hex(0b20))replacing
               s$(21%) = " Enter As Much Of The Code As Is Known, " &    ~
                         "Then Press (ENTER)"
               row% = 23% : col% = 22%
               mode% = 0%
               goto  L35100

L33500:     if keyhit% = 6% then L33900
            if keyhit% = 9% then L33900
            if keyhit% = 10% then L33900
            goto L35700
L33900:        afac$ = hex(80)   /* Enable For Search */
               mode% = 1%
               if keyhit% = 10% then mode% = 0%
               if keyhit% = 9% then mode% = 2%
               s$(21%) = " Enter As Much Of The Description As Kno" &    ~
                         "wn, In Proper Case, Then Press (RETURN)"
               if mode% = 1% then s$(21%) = " Spell Out What The Descr" &~
                            "iption Sounds Like, Then Press (RETURN)"
               if mode% = 2% then s$(21%) = " Enter String To Search D" &~
                      "escriptions For, In Any Case, Then Press (RETURN)"
               if gui% = 1% then alternate$() = " "
               tran(alternate$(), hex(0b20))replacing
               row% = 24% : col% = 27%
L35100:        rlen% = 160% /* Make read screen return last two lines */
               keys$ = hex(00010d0f10)
               tran(s$(), hex(8d86))replacing
               str(s$(21%),,1%) = hex(a4)
               goto L26000

L35700:     rlen% = 0% /* Stop 'read screen' from updating S$() */
            if keyhit% <> 0% then L39600   /* Search Mode? */
               if pfac$ <> hex(81) then L37600
                  primary$() = str(s$(23%),22%)
                  readkey$ = str(s$(23%),22%,16%)
                  key% = 0%
                  tran (str(primary$()), hex(200b)) replacing
                  tran (str(readkey$)  , hex(200b)) replacing

                  if file$ <> "GLMAIN" then L36500
                      call "GLVALID" (readkey$, str(primary$()), iosw$)
                      if iosw$ <> " " then L36600              /* Error */
                      readkey$ = " "
                      str(readkey$,,9%) = str(primary$(),,9%)            ~
                                              addc all(hex(ff))
                      call "GLFMT" (str(primary$()))
                      goto L39200  /* and Resume */

L36500:           if file$ <> "GLMAIN2" then L36700
                      call "GLVALD2" (readkey$, str(primary$()), iosw$)
                      if iosw$ <> " " then L36600              /* Error */
                      readkey$ = " "
                      str(readkey$,,9%) = str(primary$(),,9%)            ~
                                              addc all(hex(ff))
                      call "GLFMT2" (str(primary$()))
                      goto L39200  /* and Resume */

L36600:         /* Here if IOSW$ <> " " (GLVALxx Error) */
                      primary$() = " "
                      readkey$ = all(hex(00))
                      goto L39200  /* and Resume */
                        /* Note - GLFMTx should END if primary = " " */

L36700:         /* Not a GLMAIN, regular processing */
                  readkey$ = " "
                  str(readkey$,,len(str(primary$()))) = str(primary$())  ~
                      addc all(hex(ff))
                  goto L39200      /* and Resume */

L37600:        if afac$ <> hex(80) then L39600
                  readkey$ = " "
                  if str(s$(24),27%,altlen%)<>" " or mode% = 0% then L38100
                     mode% = 0%
                     goto show_codes
L38100:           alternate$() = str(s$(24%),27%)
                  tran (str(alternate$()), hex(200b)) replacing
                  test$ = str(alternate$()) or all(hex(20))
                  if mode% = 0% then L38900
                     key% = 0%
                     if mode% = 2% then L38700
                     call "SOUNDEX" (str(alternate$()), find_code$)
L38700:              readkey$ = all(hex(00))

           s$(21%) = hex(a4) & "Press Any Function Key to Terminate Search"
           order$ = hex(15a0) & bin(1%,1) & bin(21%,1)
           call "WSXIO"addr("X", #5, hex(80), order$, s$(21%), 80%, iosw$)
           s$(21%) = hex(ac) & "Available Functions Are :"

                     goto L39200
L38900:           key% = alt%
                  str(readkey$,,len(str(alternate$())))=str(alternate$())~
                                                        addc all(hex(ff))
L39200:           tran(s$(), hex(868d))replacing
                  row%, col% = 0%
                  if gui% = 0% then L40300
                      row% = 5% : col% = 4% : goto L40300

L39600:     if keyhit% = 2% or c% = 0% then readkey$ = all(hex(00))
            if keyhit% = 2% or c% = 0% then row%, col% = 0%
            if gui% = 0% or keyhit% <> 2% then L39800
               row% = 5% : col% = 4%
L39800:     if keyhit% = 2% then mode% = 0%
            if c% = 0% then s$() = " "
            if c% = 0% then plow_codes

            if keyhit% = 0% then L40700
            if keyhit% <> 4% then L40300
               /* Read Previous Screen (F4) */
               gosub read_previous_screen
               s$() = " " : c% = 0%
               goto plow_codes

L40300:        s$() = " " : c% = 0%
               if keyhit% = 5% then read_next
               goto plow_codes

L40700:     x% = row%
            row%, col% = 0%
            if gui% = 0% then L40900
               row% = 5% : col% = 4%
L40900:     if x% > c%+4% or x% < 5% then L23700

            REM Pass Selection Back
            key$ = str(s$(x%),4%,adjlen%)
            text$ = str(s$(x%),adjlen%+7%,altlen%)

            if key% = 0% then all_done
                key$ = str(s$(x%),altlen%+7%, pklen%)
                text$ = str(s$(x%),4%,altlen%)
                goto all_done

        abort_search
            f1% = 0%
            goto L42700

        all_done
            REM Now Put Parentheses Around This Thing And Exit...
                readkey$ = key$
                if file$ = "GLMAIN" then call "GLUNFMT" (readkey$)
                if file$ = "GLMAIN2" then call "GLUNFM2" (readkey$)
                call "READ100" (#1, readkey$, f1%)  /* SET POSITION */
            direct_hit
                descr$ = text$
                if descr$<>" " and flag%<>0% then descr$="("& descr$ &")"
L42700:         if f1% = 0% then descr$ = " "

               if f5% = 0% then call "WSXIO" addr("C", #5)
*              IF F5% = 0% THEN CALL "GUICHENV" (PROGRAM$, 1%)
                rem THE:end /* AH-BA-DEE AH-BA-DEE THATS ALL FOLKS! */




Show_Listbox
            call "SENDCMD" (uw$)
List_Wait:  call "GETCMD" (9999%,keyhit%,readkey$)
            if keyhit% < 0% then List_Wait /* Timed Out */
            f1% = 0%
            if keyhit% > 0% then End_ListBox     /* User Canceled */
            key$ = str(readkey$,1%,adjlen%)
            if file$ = "GLMAIN" then call "GLUNFMT" (readkey$)
            if file$ = "GLMAIN2" then call "GLUNFM2" (readkey$)
            call "READ100" (#1, readkey$, f1%)  /* SET POSITION */
            gosub describe_it
End_ListBox: REM *** Restore Screen Cursor ***
            call "SENDCMD" (uw$ & "UWSETUP3,0" & uw$)
            goto direct_hit
