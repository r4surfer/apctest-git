        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  DDDD   DDDD   IIIII  N   N  PPPP   U   U  TTTTT          *~
            *  D   D  D   D    I    NN  N  P   P  U   U    T            *~
            *  D   D  D   D    I    N N N  PPPP   U   U    T            *~
            *  D   D  D   D    I    N  NN  P      U   U    T            *~
            *  DDDD   DDDD   IIIII  N   N  P       UUU     T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DDINPUT  - Add, Change, Delete CMS Element Dictionary     *~
            *            Elements subroutine.                           *~
            *            Maintains all Element Dictionary Entries,      *~
            *            (FILES, FIELDS, PROGRAMS, SUBS, MENU'S, etc).  *~
            *            Used by the System Element Structures which    *~
            *            describe the CMS and other, user defined,      *~
            *            systems.                                       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/16/84 ! ORIGINAL                                 ! LDJ *~
            * 11/12/84 ! CORRECTED HANDLING OF 'FILLER' FIELDS TO ! LDJ *~
            *          !   PROTECT FUTURE ADDITIONS TO THE FILE.  !     *~
            * 06/14/85 ! Corrected initialization of CMS Security ! LDJ *~
            *          !   Code.                                  !     *~
            * 08/08/85 ! Disallowed deletion of a Library that's  ! LDJ *~
            *          !   in use, i.e. part of a structure.      !     *~
            * 04/14/86 ! Screen Header changes - minor bug fixes. ! LDJ *~
            *          !   Changed Field Type to Obsolete Flag.   !     *~
            * 11/12/86 ! Screen changes, more consistent PF key   ! LDJ *~
            *          !   handling, took away search by Libs and !     *~
            *          !   added multi-level where used.          !     *~
            *          !   (All above added while in the program  !     *~
            *          !   to change Obsolete flag from 'O' to 'Y'!     *~
            * 05/03/88 ! Corrected PLOWCODE calling syntax - PF14 ! LDJ *~
            *          !   "Where Used" function.                 !     *~
            * 04/19/94 ! Changed initial PLOWCODE call from anti- ! LDJ *~
            *          !   plow functionality to lesser           !     *~
            *          !   functionality provided by 2 calls to   !     *~
            *          !   PLOWCODE in order to eliminate PRR13159!     *~
	    * 05/29/96 ! Expanded var cdate_l to 8, updated to in-! DER *~
	    *          !   cluded DATEFMT and DATUFMT on cdate_l  !     *~
	    * 07/25/97 ! Disabled/removed blatant 'Wang'isms.     ! LDJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "DDINPUT" (#1,               /* INTDOC01 File channel #    */~
                       #2,               /* INTDOC02 File channel #    */~
                       pass$)            /* Code to immediately display*/~
                                         /* or blank                   */

        dim                                                              ~
            assembly$(50)22,             /* Implosion Array            */~
            cdate_i$6,                   /* Record created date        */~
            cdate_l$8,                   /* Last modified date         */~
            code$16,                     /* Elem dictionary code       */~
            cudd$16,                     /* Elem dictionary code       */~
            cde$16,                      /* Dummy ""        ""         */~
            columnhdr$(2)79,             /* Column Headers For PLOWCODE*/~
            comment$(10)79,              /* Comments                   */~
            componentdescr$99,           /* Component d.d. description */~
            count%(2),                   /* Search work variable       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            description$50,              /* 'Quick' description        */~
            descr$50,                    /* 'Quick' description save   */~
            descr_map(10),               /* Description Map - PLOWCODE */~
            document$5,                  /* Document id                */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            field_type$1,                /* Field type                 */~
            filler$50,                   /* Record filler              */~
            filler1$13,                  /* Record filler              */~
            format$12,                   /* 'BASIC' Field format       */~
            formatlist$20,               /* Valid format types         */~
            hdr$(3)79,                   /* PLOWCODE Arguments         */~
            i$(24)80,                    /* Screen image               */~
            index(1),                    /* Receiver for type search   */~
            incl(1),                     /* Include/Exclude - PLOWCODE */~
            incl$(1)1 ,                  /* Include/Exclude - PLOWCODE */~
            inpmessage$79,               /* Input message              */~
            last_type$8,                 /* Last type for default      */~
            length$4,                    /* Field or other length      */~
            lfac$(20)1,                  /* Field attribute characters */~
            line2$79,                    /* Second line of screen hdr  */~
            maxpd%(2),                   /* Work variable for edit     */~
            pass$16,                     /* Code to immediately display*/~
            pf4$20,                      /* PF  4 Prompt               */~
            pf5$20,                      /* PF  5 Prompt               */~
            pf8$20,                      /* PF  8 Prompt               */~
            pf10$20,                     /* PF 10 Prompt               */~
            pf11$20,                     /* PF 11 Prompt               */~
            pf12$20,                     /* PF 12 Prompt               */~
            pf14$20,                     /* PF 14 Prompt               */~
            pf16$20,                     /* PF 16 Prompt               */~
            plowkey$100,                 /* Miscell read key           */~
            readkey$100,                 /* Miscell read key           */~
            rel_descr$50,                /* Relationship code descript */~
            relatn$16,                   /* Relation  d.d. code        */~
            sec$6,                       /* Cms security code          */~
            temp$50,                     /* Miscell work variable      */~
            text11$79,                   /* Row 11 screen text         */~
            type$8,                      /* Elem dictionary type       */~
            typedescr$50,                /* Type description (quick)   */~
            user$3,                      /* Last modified by user id   */~
            userid$3,                    /* Current user id            */~
            version$6                    /* Version id                 */~


        dim f1%(02)                      /* = 1 If read was successful */~

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
            * #01 ! INTDOC01 ! S.E.S. Element Dictionary File           *~
            * #02 ! INTDOC02 ! S.E.S. Structure Relationships File      *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            formatlist$="CHPDFLBIchpdflbi"
            text11$ = " Comments, Notes, Special Edits, ..."
            maxpd%(1) = 14%    /* MAX NUMBER OF PACKED DECIMAL DIGITS  */
            maxpd%(2) =  9%    /* MAX NUMBER OF PD DECIMAL PLACES      */
            call "EXTRACT" addr("ID", userid$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (RETURN)."
            str(line2$,64%)="DDINPUT:" & str(cms2v$,,8%)
            code$=pass$
            if pass$ = " " then inputmode
            gosub'151(1%)
            if onfile$ <> "Y" then L10000
            call "READ100" (#1, str(type$,,8%) & "        ", f1%(1))
            if f1%(1) = 0% then L09460
            get #1 using L09475, enabled$
L09460:     call "READ100" (#1, code$, f1%(1))
            goto L11000
L09475:         FMT POS(965), CH(16)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, rel_descr$, relatn$, type$,~
                      description$, sec$, format$, length$, field_type$, ~
                      document$, comment$(), enabled$, typedescr$,       ~
                      filler$, filler1$, user$, cdate_i$, cdate_l$
            str(sec$,2%,5%) = all(hex(00))
            pf5$, pf8$, pf10$, pf11$, pf12$, pf14$ = " "
            for fieldnr% = 1% to  11%
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10330
L10131:               if fieldnr% > 1% then pf4$  = "(4)Previous Field"  ~
                                       else pf4$  = " "
                      if fieldnr% = 1% then pf16$ = "(16)Return"         ~
                                       else pf16$ = " "
                gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10210
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% > 0 then L10131
                         goto L10170
L10210:               if keyhit%  = 16 and fieldnr% = 1 then exit_routine
                      if keyhit%  = 32 then call "RETURN" addr(2%,u3%)
                      if keyhit% <>  0 then       L10131
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10131
                      if onfile$="Y" then fieldnr%=11%
L10330:     next fieldnr%

            if type$ <> "TYPE" or onfile$ = "Y" then L11000
            enabled$ = " "
            for fieldnr% = 2% to  11%
                gosub'052(fieldnr%)
                      if enabled% = 0 then L10480
L10400:         gosub'102(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10470
L10430:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% > 0 then L10400
                         goto L10430
L10470:               if keyhit% <>  0 then       L10400
L10480:     next fieldnr%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of edit mode for data entry screens     *~
            *************************************************************
        edtpg1
            pf4$ = " "
            if pass$ <> code$ then pf16$="(16)Save Data"                 ~
                              else pf16$="(16)Return"
            if type$ = "TYPE" then pf8$ = "(8)Set Defaults" else pf8$=" "
            pf5$ = "(5)Next " & type$
            pf11$ = "(11)View Doc"
            if onfile$ = "Y" then pf14$ = "(14)WhereUsed"  else pf14$=" "
            if onfile$ = "Y" then pf12$ = "(12)Delete"     else pf12$=" "
            inpmessage$ = edtmessage$
            gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  <> 8 then L11190
                     if type$ = "TYPE" then edtpg2
                     errormsg$ = "Set Defaults may only be used when D.D.~
        ~Type = TYPE"
                     goto edtpg1
L11190:           if keyhit%  = 16 then       datasave
                  if keyhit%  = 32 then call "RETURN" addr(2%,u3%)
                  if keyhit%  = 12 then       datakill
                  if keyhit%  = 14 then gosub inquiry
                  if keyhit%  = 11 then gosub view_doc
                  if keyhit%  =  5 then gosub next_code
                  if keyhit% <>  0 then       edtpg1
            oldfieldnr% = 0%
L11270:     if cursor%(1) > 8% then cursor%(1) = cursor%(1) + 1%
            if cursor%(1) >10% then cursor%(1) = cursor%(1) + 1%
            if cursor%(1) >12% then cursor%(1) = cursor%(1) + 1%
            fieldnr% = cursor%(1) - 3
            if fieldnr% = 5% and cursor%(2) > 26% then fieldnr%=fieldnr%+1
            if fieldnr% = 7% and cursor%(2) > 49% then fieldnr%=fieldnr%+1
            if fieldnr% = 9% and cursor%(2) > 49% then fieldnr%=fieldnr%+1
            if fieldnr% < 2% then edtpg1
            if fieldnr% > 11% then fieldnr% = 11%
            if fieldnr% = oldfieldnr% then edtpg1

            gosub'051(fieldnr%)
                  if enabled% = 0% then edtpg1
                  pf5$, pf8$, pf10$, pf11$, pf12$, pf14$, pf16$ = " "
L11390:     gosub'101(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 14 then gosub inquiry
                  if keyhit% <>  0 then L11390
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11390
                  oldfieldnr% = fieldnr%
            goto L11270

        edtpg2:
            inpmessage$ = edtmessage$
L11490:     gosub'102(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  8 then L11000
                  if keyhit% <>  0 then       L11490
            fieldnr% = cursor%(1) - 3
            if fieldnr% < 2% then L11490
            if fieldnr% > 11% then fieldnr% = 11%

            gosub'052(fieldnr%)
                  if enabled% = 0% then L11490
L11590:     gosub'102(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11590
            goto L11490

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after input/editing.                   *~
            *************************************************************

        datasave
            call "READ101" (#1, code$, f1%(1))
            if f1%(1) = 0% then cdate_i$ = date
            cdate_l$ = date
            put #1 using L35035, relatn$,                                 ~
                                type$,                                   ~
                                code$,                                   ~
                                description$,                            ~
                                comment$(),                              ~
                                sec$,                                    ~
                                document$,                               ~
                                format$,                                 ~
                                length$,                                 ~
                                prompt$,                                 ~
                                field_type$,                             ~
                                cdate_i$,                                ~
                                cdate_l$,                                ~
                                userid$,                                 ~
                                filler1$,                                ~
                                enabled$,                                ~
                                filler$

            last_type$ = type$
            if f1%(1) =0% then write #1
            if f1%(1) >0% then rewrite #1
            if pass$ = code$ then exit_routine
            last_code$ = code$
            goto inputmode

        datakill
            if pass$ <> code$ then L19420
               errormsg$="Delete Denied, you are currently working on thi~
        ~s code"
               goto L11000
L19420:     readkey$=code$
            call "PLOWALTS" (#1, readkey$, 2%, 16%, f1%(1))
            if f1%(1) = 0% then L19480
               errormsg$="Delete Denied, this code has a Relationship wit~
        ~h code " & str(readkey$,25,16)
               goto L11000
L19480:     if type$ <>"TYPE" then L19550
            readkey$=code$
            call "PLOWALTS" (#1, readkey$, 1%, 16%, f1%(1))
            if f1%(1) = 0% then L19550
               errormsg$="Delete Denied, this code is the TYPE definition~
        ~ for code " & str(readkey$,9,16)
               goto L11000
L19550:     readkey$=code$
            call "PLOWALTS" (#2, readkey$, 1%, 16%, f1%(2))
            if f1%(2) = 0% then L19610
               errormsg$="Delete Denied, this code is a component of " & ~
                         str(readkey$,23,28)
               goto L11000
L19610:     readkey$ = code$
            call "PLOWNEXT" (#2, readkey$, 16%, f1%(2))
            if f1%(2) = 0% then L19661
               errormsg$="Delete Denied, this code is a Parent in the Sys~
        ~tem Element Structures"
               goto L11000
L19661:     readkey$ = code$
            call "PLOWALTS" (#2, readkey$, 3%, 6%, f1%(2))
            if f1%(2) = 0% then L19695
               errormsg$="Delete Denied, this code is an active Library i~
        ~n the System Element Structures!"
               goto L11000
L19670:     call "DELETE" (#1, code$, 16%)
            goto inputmode

L19695:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** OK TO DELETE? ***",            ~
                 "Press RETURN to DELETE " & code$ & ",",                ~
                 "-OR-",                                                 ~
                 "Press PF1 to CANCEL the Delete Operation")
            if keyhit% = 0% then L19670
            if keyhit% = 1% then L11000
            goto L19695

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 1 of input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100,         /* DD Code          */~
                                    L20200,         /* Relation         */~
                                    L20300,         /* DD Type          */~
                                    L20400,         /* Description      */~
                                    L20500,         /* File class code  */~
                                    L20550,         /* CMS Security code*/~
                                    L20600,         /* Field format     */~
                                    L20650,         /* Length           */~
                                    L20700,         /* Obsolete         */~
                                    L20800,         /* Document id      */~
                                    L20900          /* Comments         */
                     return
L20100:     REM DEFAULT/ENABLE FOR ELEM DICTIONARY CODE
                enabled%=1%
                inpmessage$="Enter the Element Dictionary Code to Add, Ch~
        ~ange, or Delete"
                return
L20200:     REM DEFAULT/ENABLE FOR RELATION  D.D. CODE
                if str(enabled$,fieldnr%,1)=" "then enabled%=1%          ~
                else return
                inpmessage$="Enter an Element Code for a specific relatio~
        ~nship to this code or blank"
                return
L20300:     REM DEFAULT/ENABLE FOR ELEM DICTIONARY TYPE
                enabled% = 1%
                inpmessage$="Please enter the 'Type' of code this is, e.g~
        ~. FILE, FIELD, etc"
                if type$ = " " then type$ = last_type$
                return
L20400:     REM DEFAULT/ENABLE FOR 'QUICK' DESCRIPTION
                if str(enabled$,fieldnr%,1)=" "then enabled%=1%          ~
                else return
                inpmessage$="Please enter a one line description of this ~
        ~Element Code"
                return
L20500:     REM DEFAULT/ENABLE FOR WANG FILE CODES
                return
L20550:     REM DEFAULT/ENABLE FOR CMS SECURITY CODE
                inpmessage$="Enter the CMS access security code "
                enabled% = 0%
                return
L20600:     REM DEFAULT/ENABLE FOR 'BASIC' FIELD FORMAT
                if str(enabled$,fieldnr%,1)=" "then enabled%=1%          ~
                else return
                inpmessage$="Enter the BASIC field format describing the"~
                  & " " & type$ & " type & length"
                return
L20650:     REM DEFAULT/ENABLE FOR LENGTH
                if str(enabled$,fieldnr%,1)=" "then enabled%=1%          ~
                else return
                inpmessage$="Enter the length of this " & type$
                return
L20700:     REM DEFAULT/ENABLE FOR Obsolete Flag
                if str(enabled$,fieldnr%,1)=" "then enabled%=1%          ~
                else return
                inpmessage$="Enter 'Y' if " & code$ &                    ~
                     " is Obsolete, otherwise leave blank"
                if onfile$ <> "Y" then enabled% = 0%
                return
L20800:     REM DEFAULT/ENABLE FOR DOCUMENT ID
                return
L20900:     REM DEFAULT/ENABLE FOR COMMENTS
                if str(enabled$,fieldnr%,1)=" "then enabled%=1%          ~
                else return
                inpmessage$="Enter any applicable notes, edits, etc"
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 2 of input. *~
            *************************************************************


            deffn'052(fieldnr%)
                  enabled% = 1
                  inpmessage$="Enter a non-blank character to disable thi~
        ~s field for type " & code$
                  on fieldnr% gosub,               /* DD CODE          */~
                                   ,               /* RELATION         */~
                                   ,               /* DD TYPE          */~
                                   ,               /* DESCRIPTION      */~
                                   ,               /* FILE CLASS CODE  */~
                                   L21220           /* CMS SECURITY CODE*/~
                                                   /* FIELD FORMAT     */~
                                                   /* LENGTH           */~
                                                   /* FIELD TYPE       */~
                                                   /* DOCUMENT ID      */~
                                                   /* COMMENTS         */
                     return

L21220:     REM DEFAULT/ENABLE FOR CMS SECURITY CODE DISABLE SWITCH
                enabled% = 0%
                str(enable$,6,1) = "X"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * or will return user back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            k% = 2%
            call "STARTOVR" (k%)
            if k% <> 0% and k% <> 1% then startover
            if k% = 1% then return
            if pass$ > " " then exit_routine    /* Return to Caller  */
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *      G E T   R E C O R D   O U T   O F   B U F F E R      *~
            *                                                           *~
            *************************************************************

            get #1 using L35035, relatn$,                                 ~
                                type$,                                   ~
                                cde$,                                    ~
                                description$,                            ~
                                comment$(),                              ~
                                sec$,                                    ~
                                document$,                               ~
                                format$,                                 ~
                                length$,                                 ~
                                prompt$,                                 ~
                                field_type$,                             ~
                                cdate_i$,                                ~
                                cdate_l$,                                ~
                                user$,                                   ~
                                filler1$,                                ~
                                enable$,                                 ~
                                filler$
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * Format statements for data files.                         *~
            *************************************************************

L35035: FMT                      /* FILE: INTDOC01                     */~
            CH(16),              /* Defines similarities between D.D.  */~
            CH(08),              /* documentation type (i.e. menu,pgm, */~
            CH(16),              /* documentation code                 */~
            CH(50),              /* documentation description          */~
            10*CH(79),           /* Paragraph Description              */~
            CH(6),               /* security code                      */~
            CH(5),               /* VS Word Processing Document ID for */~
            CH(12),              /* field format                       */~
            CH(4),               /* field length                       */~
            CH(28),              /* Screen Field Prompt                */~
            CH(1),               /* field type                         */~
            CH(6),               /* originally input on (date)         */~
            CH(6),               /* last modified on (date)            */~
            CH(3),               /* Last modified by User ID           */~
            CH(13),              /* Filler (not currently used)        */~
            CH(16),              /* ENABLED FIELDS IN DDINPUT          */~
            CH(20)               /* filler for rest of record or inter */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *-----------------------------------------------------------*~
            * Inputs document for first time.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  if fieldnr% = 0%       then init(hex(86)) lfac$()      ~
                  else                        init(hex(8c)) lfac$()
                  tran(comment$(),hex(0020)) replacing
                  call "DATEFMT" (cdate_l$)
                  if cdate_l$ > " " then                                 ~
                     str(text11$,40)="Last Modified " & cdate_l$ &       ~
                        " by " & user$
                  if cdate_l$ = " " then str(text11$,40) = " "
                  call "DATUNFMT" (cdate_l$)
                  if last_code$   > " " then str(line2$,,62) =           ~
                   "Last Element Dictionary Code: "   & last_code$
                  if last_code$ = " " then  str(line2$,,62) = " "
                  on fieldnr% gosub L40330,         /* DD CODE          */~
                                    L40330,         /* RELATION         */~
                                    L40330,         /* DD TYPE          */~
                                    L40300,         /* DESCRIPTION      */~
                                    L40330,         /* WANG FILE CLASS  */~
                                    L40330,         /* CMS SECURITY CODE*/~
                                    L40330,         /* FIELD FORMAT     */~
                                    L40360,         /* FIELD LENGTH     */~
                                    L40330,         /* Obsolete         */~
                                    L40300,         /* DOCUMENT ID      */~
                                    L40300          /* COMMENTS         */
                  if fieldnr% > 0% and errormsg$ > " " then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40430

L40300:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40330:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40360:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
                  REM SET FACS FOR UNDERLINED UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(a0)
                      return

L40430: accept                                                           ~
               at (01,02),                                               ~
        "Maintain Element Dictionary Definitions",                       ~
               at (01,66),                                               ~
                  "Today:",                                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$     , ch(79),         ~
               at (04,03),                                               ~
        "Element Code        :",                                         ~
               at (04,25), fac(lfac$(1)), code$         , ch(16),        ~
               at (05,03),                                               ~
        "Relationship Code   :",                                         ~
               at (05,25), fac(lfac$(2)), relatn$       , ch(16),        ~
               at (05,42), fac(hex(8c)) , rel_descr$    , ch(39),        ~
               at (06,03),                                               ~
        "Element Type        :",                                         ~
               at (06,25), fac(lfac$(3)), type$         , ch(08),        ~
               at (06,35), fac(hex(8c)),  typedescr$    , ch(45),        ~
               at (07,03),                                               ~
        "'Quick' Description :",                                         ~
               at (07,25), fac(lfac$(4)), description$  , ch(50),        ~
               at (09,03),                                               ~
        "'BASIC' Field Format:",                                         ~
               at (09,25), fac(lfac$(7)), format$       , ch(12),        ~
               at (09,50), fac(hex(8c)),  type$         , ch(08),        ~
               at (09,59),                                               ~
        "Length:",                                                       ~
               at (09,67), fac(lfac$(8)), length$       , ch(04),        ~
               at (10,03), fac(hex(8c)),  type$         , ch(08),        ~
               at (10,12),                                               ~
        "Obsolete ? :",                                                  ~
               at (10,25), fac(lfac$(9)), field_type$   , ch(01),        ~
               at (11,02), fac(hex(ac))  , text11$      ,  ch(79),       ~
               at (12,02), fac(lfac$(11)), comment$(1)   , ch(79),       ~
               at (13,02), fac(lfac$(11)), comment$(2)   , ch(79),       ~
               at (14,02), fac(lfac$(11)), comment$(3)   , ch(79),       ~
               at (15,02), fac(lfac$(11)), comment$(4)   , ch(79),       ~
               at (16,02), fac(lfac$(11)), comment$(5)   , ch(79),       ~
               at (17,02), fac(lfac$(11)), comment$(6)   , ch(79),       ~
               at (18,02), fac(lfac$(11)), comment$(7)   , ch(79),       ~
               at (19,02), fac(lfac$(11)), comment$(8)   , ch(79),       ~
               at (20,02), fac(lfac$(11)), comment$(9)   , ch(79),       ~
               at (21,02), fac(lfac$(11)), comment$(10)  , ch(79),       ~
               at (22,02), fac(hex(a4))  , inpmessage$   , ch(79),       ~
               at (23,02), "(1)Start Over",                              ~
               at (24,02), fac(hex(8c))  , pf14$         , ch(13),       ~
               at (23,16), fac(hex(8c))  , pf4$          , ch(17),       ~
               at (24,16), fac(hex(8c))  , pf5$          , ch(17),       ~
               at (23,65), "(15)Print Screen",                           ~
               at (23,34), fac(hex(8c))  , pf8$          , ch(15),       ~
               at (24,34), fac(hex(8c))  , pf10$         , ch(14),       ~
               at (23,50), fac(hex(8c))  , pf11$         , ch(12),       ~
               at (24,50), fac(hex(84))  , pf12$         , ch(10),       ~
               at (24,65), fac(hex(84))  , pf16$         , ch(16),       ~
                                                                         ~
               keys(hex(00010405080a0b0c0d0e0f10)),                      ~
               key (keyhit%)

               tran(comment$(),hex(2000)) replacing

               if keyhit% <> 13 then L41190
                  call "MANUAL" ("DDINPUT ")
                  goto L40430

L41190:        if keyhit% <> 15 then L41891
                  call "PRNTSCRN"
                  goto L40430

L41891:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *-----------------------------------------------------------*~
            *         enable/disable selections for type = TYPE         *~
            *************************************************************

            deffn'102(fieldnr%)
                  if fieldnr% = 0%       then init(hex(86)) lfac$()      ~
                  else                        init(hex(8c)) lfac$()
                  on fieldnr% gosub L42250,         /* DD CODE          */~
                                    L42250,         /* RELATION         */~
                                    L42250,         /* DD TYPE          */~
                                    L42250,         /* DESCRIPTION      */~
                                    L42250,         /* WANG FILE CLASS  */~
                                    L42250,         /* CMS SECURITY CODE*/~
                                    L42250,         /* FIELD FORMAT     */~
                                    L42250,         /* FIELD LENGTH     */~
                                    L42250,         /* FIELD TYPE       */~
                                    L42250,         /* DOCUMENT ID      */~
                                    L42250          /* COMMENTS         */
                     goto L42310

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42250:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
L42310: accept                                                           ~
               at (01,03),                                               ~
        "Maintain Element Dictionary Definitions",                       ~
               at (01,67), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$     , ch(79),         ~
               at (04,03),                                               ~
        "Element Code        :",                                         ~
               at (04,25), fac(lfac$(1)), str(enabled$,,1), ch(1),       ~
               at (05,03),                                               ~
        "Relationship Code   :",                                         ~
               at (05,25), fac(lfac$(2)), str(enabled$,2,1), ch(1),      ~
               at (06,03),                                               ~
        "Element Type        :",                                         ~
               at (06,25), fac(lfac$(3)), str(enabled$,3,1), ch(1),      ~
               at (07,03),                                               ~
        "'Quick' Description :",                                         ~
               at (07,25), fac(lfac$(4)), str(enabled$,4,1), ch(1),      ~
               at (09,03),                                               ~
        "CMS Security Code   :",                                         ~
               at (09,25), fac(lfac$(6)), str(enabled$,6,1), ch(1),      ~
               at (10,03),                                               ~
        "'BASIC' Field Format:",                                         ~
               at (10,25), fac(lfac$(7)), str(enabled$,7,1), ch(1),      ~
               at (11,03), fac(hex(8c)),  code$         , ch(08),        ~
               at (11,12),                                               ~
        "Length     :",                                                  ~
               at (11,25), fac(lfac$(8)), str(enabled$,8,1), ch(1),      ~
               at (12,03), fac(hex(8c)),  code$         , ch(08),        ~
               at (12,12),                                               ~
        "Obsolete   :",                                                  ~
               at (12,25), fac(lfac$(9)), str(enabled$,9,1), ch(1),      ~
               at (14,02), fac(hex(8c))  , text11$      ,  ch(23),       ~
               at (14,25), fac(lfac$(11)),str(enabled$,11,1),ch(1),      ~
               at (22,02), fac(hex(a4))  , inpmessage$   , ch(79),       ~
               at (23,03),                                               ~
        "(1)Start Over    (4)Previous Field    (8)Prior Screen" ,        ~
               at (23,65), "(13)Instructions",                           ~
               at (24,65), "(15)Print Screen",                           ~
                                                                         ~
               keys(hex(000104080d0f)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L43010
                  call "MANUAL" ("DDINPUT ")
                  goto L42310

L43010:        if keyhit% <> 15 then L43050
                  call "PRNTSCRN"
                  goto L42310

L43050:        if fieldnr% > 0 then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the items on page 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* DD Code          */~
                                    L50235,         /* Relation         */~
                                    L50300,         /* DD Type          */~
                                    L50445,         /* Description      */~
                                          ,         /* Wang file class  */~
                                          ,         /* CMS Security code*/~
                                    L50585,         /* Field Format     */~
                                    L51800,         /* Length           */~
                                    L50495,         /* Obsolete Flag    */~
                                          ,         /* Document ID      */~
                                    L50575          /* Comments         */
                     return
L50100:     REM Test data for Element Dictionary Code
                onfile$="N"
                readkey$ = " "
                if code$>" " then L50153
                readkey$ = "TYPE"
L50116:         columnhdr$(2) = "Type Code    Description"
                columnhdr$(1) = " "
                description$ = hex(06) & "Select the TYPE and the " &    ~
                                         "Element Code to Edit"
                call "PLOWCODE" (#1, readkey$, description$, 6008%, 1.50,~
                                      f1%(1),columnhdr$(),0,0,           ~
                                     incl(),incl$(),"Y")
                if f1%(1) = 0% then L50220
                readkey$ = str(readkey$,9%)
                columnhdr$(2%) = str(readkey$,,8%) & "     Description"
                description$ = hex(06) & "Select the " &                 ~
                     str(readkey$,,8%) & " Code to Edit"
                call "PLOWCODE" (#1, readkey$, description$, 6008%, 1.50,~
                                      f1%(1),columnhdr$(),0,0,           ~
                                     incl(),incl$(),"Y")
                if f1%(1) = 1% then L50151
                   readkey$ = "TYPE    " & str(readkey$,,7%) & hex(00)
                   goto L50116
L50151:         code$ = str(readkey$,9%)
                if code$=" " then L50210
L50153:         call "READ100" (#1, code$, f1%(1))
                if f1%(1) = 0% then return
                gosub L30000
                onfile$="Y"
                if type$ = "TYPE" then enabled$ = enable$
                call "READ100" (#1, type$, f1%(1))
                if f1%(1) = 1% then typedescr$ = key(#1,3)               ~
                else typedescr$ = " "
                call "READ100" (#1, relatn$, f1%(1))
                if f1%(1) = 1% then rel_descr$ = key(#1,3)               ~
                else rel_descr$ = " "
                return
L50210:         errormsg$="The Element Dictionary Code cannot be blank !"
                return
L50220:         errormsg$="Enter a new Code or select an Existing one " &~
                          "please"
                return
L50235:     REM Test Data for Relation  D.D. Code
                if relatn$ = " " then return
                rel_descr$ = hex(06) & "Select an Alias for this Code"
                call "PLOWCODE" (#1, relatn$,rel_descr$,0%, 0.50, f1%(1))
                if f1%(1) = 0% then L50285
                if onfile$ = "Y" then return
                temp$=relatn$
                gosub L30000
                relatn$=temp$
                return
L50285:         errormsg$ = "Relationship Code " & relatn$ & " not define~
        ~d. Leave blank or reenter"
                return
L50300:     REM Test Data For Element Dictionary Type
                len% = 8%
                if type$ <> "TYPE" then L50330
                   if onfile$<>"Y" then enabled$="    XXX XX"
                   typedescr$="Element Dictionary Type Definition"
                   goto L50395
L50330:         readkey$ = "TYPE    " & type$
                typedescr$ = hex(06) & "Select an Element Type"
                call "PLOWCODE" (#1,readkey$, typedescr$,8%,-1.50,f1%(1))
                type$ = str(readkey$,9)
                if f1%(1) = 0% then L50420
                get #1 using L50380, temp$, typedescr$, len$, enabled$
                if temp$ = "TYPE" then L50375
                   errormsg$="Sorry, this is NOT a valid TYPE"
                   return
L50375:         convert len$ to len%, data gosub L50435
L50380:         FMT XX(16), CH(8), POS(41), CH(50), POS(904), CH(4),     ~
                    POS(965), CH(16)
                if len% = 16% or onfile$ = "Y" then return
L50395:         str(code$,len% + 1%) = " "
                descr$ = description$
                gosub L50100
                if onfile$ = "N" then description$ = descr$
                return
L50420:         errormsg$="Type " & type$ & " is not defined.  Reenter or~
        ~ Start Over & define this Type"
                return
L50435:         len% = 16%
                return
L50445:     REM TEST DATA FOR 'QUICK' DESCRIPTION
                if description$ > " " then return
                errormsg$="Please enter SOMETHING to describe this code !"
                return

L50495:     REM TEST DATA FOR Obsolete
                return

L50575:     REM TEST DATA FOR COMMENTS
                return
L50585:     REM TEST DATA FOR 'BASIC' FIELD FORMAT
            REM   [E.G. 30*PD(14,2) OR XX(120)]
            REM .....TEST FOR MULITPLE OCCURANCES
            if format$ = " " then return
            formatc$ = format$            /* ALLOWS US TO GO OFF END */
            occur%   = 1%
            star% = pos(formatc$ = "*")
            if star% = 0 then L51080
                temp$ = str(formatc$, 1, max(star% - 1%, 1%))
                convert temp$ to occur%, data goto L51050
                if occur% > 0% then L51080
L51050:             errormsg$ = "Invalid # of Occurrences: " & temp$
                    return

L51080:     REM .....TEST FOR A VALID FORMAT (UPPER OR LOWER CASE)
             format% = pos(str(formatc$, star% + 1%) > " ") + star%
             if format% <> 0% then L51140
                 temp% = star% + 1%
L51120:          errormsg$ = "Bad FORMAT: " & ">" & str(format$, temp%)
                 return
L51140:      fmt$ = str(formatc$, format%, 2)
             search formatlist$ = fmt$ to index() step 2
             if index(1)<> 0 then L51200
                 errormsg$ = "Invalid FORMAT: " & fmt$
                 return

L51200:     REM .....TEST FOR A VALID LENGTH
            delim1% = pos(str(formatc$, format% + 2%) = "(" )            ~
                      + format% + 1%
            if delim1% <> 0% then L51270
                temp% = format% + 2%
                goto L51120

L51270:     delim2% = pos(str(formatc$, delim1% + 1%) = ")" ) + delim1%
            if delim2% > delim1%+ 1% then L51310
L51290:         temp% = delim1% + 1%
                goto L51120
L51310:     if pos(str(formatc$, delim2% + 1%) > " ") <> 0 then L51290
            if fmt$ = "PD" or format$ = "pd" then L51460

            rem.....test for ch,fl,xx,bi formats (one number)
            n% = delim2% - delim1% - 1%
            if n% < 1% then L51430
                temp$ = str(formatc$, delim1% + 1%,                      ~
                            delim2% - delim1% - 1%)
                convert temp$ to count%(1), data goto L51430
                if count%(1) < 1% then L51430
                   if fmt$ <> "BI" and fmt$ <> "bi" then L51730
                       if count%(1) <= 4% then L51730
L51430:                    temp% = delim1% + 1%
                           goto L51120

L51460:     rem.....test for pd format (two numbers)
            delim3% = delim2%
            delim2% = pos(str(formatc$, delim1% + 1%) = "," ) + delim1%
            if delim2% <> 0% then L51530
                temp% = delim1% + 1%
                goto L51120

L51530:     pass% = 1%
L51540:     n% = delim2% - delim1% - 1%
            if n% < 1% then L51600
            temp$ = str(formatc$, delim1% + 1%, n%)
            convert temp$ to count%(pass%), data goto L51600
            if count%(pass%) >= 0% and count%(pass%) <= maxpd%(pass%)    ~
               then L51630
L51600:         temp% = delim1% + 1%
                goto L51120

L51630:     if pass% = 2% then L51690
                delim1% = delim2%
                delim2% = delim3%
                pass% = 2%
                goto L51540

L51690:     if count%(2%) <= count%(1%) then L51730
                errormsg$ = "Too Many Decimal Places: " & format$
                return

L51730:     REM .....NOW COMPUTE LENGTH$ FROM FORMAT
            if str(fmt$,,2) = "PD" or str(fmt$,,2) = "pd" then           ~
                count%(1) = (count%(1) / 2%) + 1%
            l% = count%(1) * occur%
L51770:     convert l% to length$, pic(####)
            return

L51800:     REM EDIT LENGTH
            if length$=" " then length$="0"
            convert length$ to l%, data goto L51840
            if type$ = "TYPE" and (l% < 1% or l% > 16%) then L51860
            goto L51770
L51840:     errormsg$="Invalid Format: " & length$
            return
L51860:     errormsg$="Length for Type=TYPE must be > 0 and < 17"
            return

        inquiry
            l% = 1%
            cudd$ = code$ : version$ = " "
        REM *************************************************************~
            *            START IMPLOSION / EXPLOSION LOOP               *~
            *************************************************************
        implosion_loop
            assembly$(l%) = str(cudd$) & version$

L52130:     REM *** ReEntry Point for Backing Up ***
            cudd$ = assembly$(l%)
            version$ = str(assembly$(l%),17%)
            hdr$(1) = "  Parent           Type    Library Version Descrip~
        ~tion"
            hdr$(3) = hex(ac) & "Select the Parent Code to Implode using ~
        ~Cursor & RETURN or PF16 to BackUp"
            descr_map(1) =  23.16 : descr_map(2) =  1   /* Parent      */
            descr_map(3) = -17.08 : descr_map(4) = 18   /* Type        */
            descr_map(5) =  39.06 : descr_map(6) = 27   /* Library     */
            descr_map(7) =  61.06 : descr_map(8) = 34   /* Version     */
            descr_map(9) = -41.50 : descr_map(10)= 42   /* Description */
            mat incl = zer
L52320:     if version$ > " " then break% = 9022% else break% = 9016%
            plowkey$ = str(cudd$) & version$
            componentdescr$=hex(06) & "Below are the Parents of: " &     ~
                            cudd$
            if version$ > " " then componentdescr$ = componentdescr$ &   ~
                "  Version: " & version$
            componentdescr$ = componentdescr$ & ".  (Level=  )"
            convert l% to str(componentdescr$,len(componentdescr$)-2,2), ~
                pic(##)
            ln  = 9066% - break%
            call "PLOWCODE" (#2, plowkey$, componentdescr$, break%, 1.5, ~
                f1%(2), hdr$(),ln, -23, incl(), incl$(), "D", "Y", #1,   ~
                  descr_map())
            if f1%(2) = 0% then L52490
               l% = l% + 1%
               cudd$ = str(plowkey$,23%,16%)
               version$ = str(plowkey$,39%)
               goto implosion_loop
L52490:     if version$ = " " then L52510
               version$ = " " : goto L52320
L52510:     l% = l%-1%
            if l% > 0% then L52130  /* Back Down 1 level */
            return   /* Exited Out */

        view_doc
            call "MANUAL" (code$)
            return

        next_code
            readkey$=str(type$) & code$
            last_code$ = code$
            call "PLOWALTS" (#1, readkey$, 1%, 0%, f1%(1))
            if f1%(1) = 0% then errormsg$="End of Dictionary, Press RETUR~
        ~N to Acknowledge & Continue"
            if f1%(1) = 0% then gosub'101(0%)
            errormsg$ = " "
            if f1%(1) = 0% then return
            code$=str(readkey$,9)
            gosub'151(1%)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            *                RETURNS TO CALLING PROGRAM.                *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

        exit_routine
            end
