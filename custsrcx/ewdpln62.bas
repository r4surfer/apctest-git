        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *************************************************************~
            * EWDPLN62 - THIS IS A COPY OF THE CAELUS ORIGINAL VERSION  *~
            *            OF GENCDSIN.  THE ONLY DIFFERENCE IS THE NAME *~
            *            AND A TEST TO VERIFY ACCESS.                   *~
            *            EWD VERSION IS NAMED GENCDSIN & REPLACES THIS. *~
            *************************************************************~
            *                                                           *~
            *   GGGG  EEEEE  N   N   CCCC  DDDD   SSSSS  IIIII  N   N   *~
            *  G      E      NN  N  C      D   D  S        I    NN  N   *~
            *  G  GG  EEE    N N N  C      D   D  SSSSS    I    N N N   *~
            *  G    G E      N  NN  C      D   D      S    I    N  NN   *~
            *   GGGG  EEEEE  N   N   CCCC  DDDD   SSSSS  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GENCDSIN - Controls System 'Standard Codes File Management*~
            *            program. Logical file 'FILE DIR' contains      *~
            *            valid code logical file IDs.                   *~
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
            * 09/14/84 ! ORIGINAL                                 ! ERN *~
            * 01/28/86 ! Hidden GETPARM for remote drivability    ! HES *~
            * 01/29/86 ! Added ASKUSER and SETPRNT routines       ! ERN *~
            * 05/03/87 ! Now properly redims CODE$() @ 10000      ! KAB *~
            * 03/23/88 ! Fixed truncation of codes after 1st run  ! RJM *~
            * 07/01/92 ! Corrected tense used in edit message.    ! MLJ *~
            * 09/28/98 ! Added access check.                EWD001! BWS *~
            *10/26/2018! (CR1768) lengthen code description       ! CMN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            aid$1,                       /* AID CHAR FOR GET PARM      */~
            cfac$(15)1,                  /* FACs FOR CRC CODES         */~
            code$(15,1)15,               /* CODES (Always w/in Table)  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
/*CR1768*/  descr$(15)60,                /* CODE DESCRIPTION           */~
            dfac$(15)1,                  /* Code description Facs      */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            eof$3,                       /* END-OF-FILE FLAG           */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            file$9,                      /* LOGICAL FILE CODE          */~
            filedescr$30,                /* LOGICAL FILE DESCRIPTION   */~
            filler$(15)72,               /* CODES (Always w/in File)   */~
            gcode$50,                    /* Caller to 'CTLGETCD'       */~
            hdr0$15,                     /* COLUMN HEADINGS            */~
            hdr1$30,                     /*                            */~
            hdr3$60,                     /* (CR1768)                   */~
            hdr2$14,                     /*                            */~
            hdr2fac$1,                   /*                            */~
            hx00$9,                      /* 9 HEX 00's for File Dir    */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            lastfile$9,                  /* Last File Managed          */~
            lfac$(15)1,                  /* Length Facs                */~
            mcl$2,                       /* Maximum Code Length        */~
            mcl$(15)2,                   /* Maximum Code Length        */~
            oldfile$9,                   /* 'MANUAL' PLOW VARIABLE     */~
            pfdescr$(3,2)79,             /* PF KEY DESCRIPTIONS        */~
            pfkeys$(2)17,                /* PF KEYS (HEX)              */~
            plowkey$50,                  /* PLOW KEY                   */~
            title$79,                    /* For Screen Titles          */~
/*EWD001*/  userid$3                     /* The Guilty Party           */

        dim f1%(5)                       /* = 1 IF READ WAS SUCCESSFUL */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Control System Codes File                *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =    1,  keylen = 24

            call "SHOSTAT" ("Opening Files, One Moment Please.")
            call "OPENCHCK" (#1, open%, 0%, 150%, " ")
                if open% <> 1 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************
/*EWD001*/  call "EXTRACT" addr("ID", userid$)

/*EWD001*/  read #1, key = ".USERACSS" & userid$ & ".USERACSS",          ~
/*EWD001*/      eod goto L65000

            date$  = date  :  call "DATEFMT" (date$)
            hx00$  = all(hex(00))

            pfdescr$(1,1) = "(ENTER)Manage code typed or at cursor   " & ~
                            "                       (13)Instructions"
            pfdescr$(2,1) = "(2)FIRST Screen        (10)MOVE Screen T" & ~
                            "o Entry Typed          (15)Print Screen"
            pfdescr$(3,1) = "(5)NEXT Screen         (12)Print Codes  " & ~
                            "                       (16)EXIT        "

            pfdescr$(1,2) = "(ENTER)SAVE DATA                        " & ~
                            "                       (13)Instructions"
            pfdescr$(2,2) = "(1)Start Line Over                (8)DEL" & ~
                            "ETE                    (15)Print Screen"
            pfdescr$(3,2) = "                                        " & ~
                            "                                       "

            pfkeys$(1) = hex(ff02ffff05ffffffff0aff0c0dff0f1000)
            pfkeys$(2) = hex(01ffffffffffff08ffffffff0dff0fff00)

            REM Determine the mode we're in via a 'hidden' GETPARM
            call "GETPARM"                                               ~
                addr ("ID", "S", "GENCDSIN", aid$, "0001", "INFILE", 0%, ~
                        "K", "CODEFILE", file$,       9%, 5%, 32%, "C",  ~
                        "K", "FILEDESC", filedescr$, 30%, 1%, 32%, "C",  ~
                        "K", "CDLENGTH", mcl$,        2%, 1%, 32%, "C")

            if file$ = " " then get_fileid

               REM We're in restricted mode, so make sure 'File' is here
                mode$ = file$
                call "READ100" (#1, str(hx00$) & file$, f1%(1))
                     if f1%(1) = 1 then L10660

               REM Not there, so we set it up...
                if len(mcl$) = 1% then mcl$ = " " & mcl$
                write #1 using L31600, hx00$, file$, filedescr$, mcl$, " "
                goto L10660

        REM *************************************************************~
            *        G E T   C O D E   F I L E   I D                    *~
            *-----------------------------------------------------------*~
            * Get Id of Logical File to be maintained.                  *~
            *************************************************************

        get_fileid  /*A little non-std. All that's needed is right here*/
            mat redim code$(15,1)15
            init (" ") errormsg$, edtmessage$, file$, code$(), descr$(), ~
                       filedescr$, filler$()
            last% = 1%
            title$ = "Last Code Table Managed: " & lastfile$
            str(title$,62%) = "GENCDSIN: " & cms2v$
            edtmessage$ = "Enter table to maintain.  Enter blank or" &   ~
                          " partial name to select from display."
L10140: accept                                                           ~
               at (01,02), "Manage General System Codes",                ~
               at (01,67), "Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "TABLE TO MANAGE:",                           ~
               at (06,20), fac(hex(81)), file$,                          ~
                                                                         ~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
               at (23,02), "(2)Manage File Directory",                   ~
               at (24,02), "(3)Print Codes",                             ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)EXIT Program",                           ~
                                                                         ~
               keys(hex(0002030d0f10)),                                  ~
               key (keyhit%)

            if keyhit% =  2 then L10530
            if keyhit% = 16 then L65000

            if keyhit% <> 3 then L10390
                gosub report_printing
                goto get_fileid

L10390:     if keyhit% <> 13 then L10430
                call "MANUAL" ("GENCDSIN")
                goto get_fileid

L10430:     if keyhit% <> 15 then L10470
                call "PRNTSCRN"
                goto get_fileid

L10470:     if keyhit% <> 0% then get_fileid
                gcode$ = str(hx00$) &  file$
                call "PLOWCODE" (#1, gcode$, filedescr$, 9%, 0.30, f1%(1))
                file$ = str(gcode$,10)
                if f1%(1) <> 1 then L10140

L10530: /* Verify File ID entered in logical file 'FILE DIR'           */
        /* If PF 2, maintain File Directory                            */
            if keyhit% = 2% then dir% = 1% else dir% = 0%
            if dir% = 0% then L10660
                file$      = hx00$
                filedescr$ = "** FILE DIRECTORY **"
                mcl%       = 9%
                mat redim code$(15,1)9
                hdr0$      = "T A B L E   I D"
                hdr1$      = "        TABLE DESCRIPTION"
                hdr2$      = "MAX CODE WIDTH"
                hdr2fac$   = hex(ac)
                goto L10780
L10660:     plowkey$ = str(hx00$) & file$
            call "READ100" (#1, plowkey$, f1%(1))
                if f1%(1) = 0% then get_fileid
            get #1 using L10700, filedescr$, mcl$(1)
L10700:     FMT XX(24), CH(30), CH(2)
            mcl% = 15% : convert mcl$(1) to mcl%, data goto L10720
L10720:     if mcl% < 0% or mcl% > 15% then mcl% = 15%
            mat redim code$(15,1)mcl%
            hdr0$      = "    C O D E    "
            hdr1$      = "        CODE DESCRIPTION"
            hdr3$      = "        CODE DESCRIPTION"
            hdr2$      = " "  :  hdr2fac$ = hex(9c)
            lastfile$ = file$
L10780:     gosub load_first

        REM *************************************************************~
            *        M A I N   P R O G R A M   L O G I C                *~
            *-----------------------------------------------------------*~
            *  Screen controls and branching.                           *~
            *************************************************************

        screen_loop
            errormsg$ = " "
L11080:     gosub'101(0%)      /* Get what is to be done               */
                if keyhit%  =  2 then gosub load_first
                if keyhit%  =  5 then gosub load_next
                if keyhit%  = 10 then gosub load_stated
                if keyhit%  = 12 then gosub report_printing_specific
                if keyhit%  = 12 then gosub load_first
                if keyhit%  = 16 and mode$ = " " then get_fileid
                if keyhit%  = 16 then L65000
                if keyhit% <>  0 then L11080
            errormsg$ = " "
            l% = cursor%(1) - 5
            if l% <> 1 then L11280
            /* Add -or- Change a specifically mentioned code.          */
                if code$(1,1) <> " " then L11240
                    errormsg$ = "Code may not be blank"
                    goto L11080
L11240:         if code$(1,1) <> "ALL" then L11270
                    errormsg$ = "Sorry, code 'ALL' is a reserved word."
                    goto L11080
L11270:         gosub load_specific
L11280:     if l% < 1 or l% > last% then L11080

            gosub'051(l%)      /* Set FACs for line entry              */
L11310:     gosub'101(l%)      /* Get field entries.                   */
                if keyhit%  =  1 then gosub startover
                if keyhit%  =  8 then gosub delete_code
                if keyhit% <>  0 then L11310
            gosub'151(l%)      /* Edit fields                          */
                if errormsg$ <> " "  then L11310
                gosub save_data
                goto screen_loop

        REM *************************************************************~
            *        S E T   F A C S   F O R   E N T R Y                *~
            *-----------------------------------------------------------*~
            *  Set FACs on for line modification.                       *~
            *************************************************************

        deffn'051(l%)
            dfac$(l%) = hex(80)
            if dir% = 1% then lfac$(l%) = hex(82)
          return

        REM *************************************************************~
            *             S T A R T   O V E R                           *~
            *-----------------------------------------------------------*~
            *  Abort line modfification. Gives operator a second chance.*~
            *  Note that if code is displayed on screen we must reload  *~
            *  so that it's values revert to what they were before they *~
            *  we modified.                                             *~
            *************************************************************
        startover
L29090:     keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "* ABORT CURRENT ENTRY *",         ~
                            "PRESS (1) TO RETURN TO DISPLAY", "- OR -",  ~
                          "PRESS (ENTER) TO ABORT CHANGES MADE TO LINE")

            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29090

            return clear        /* Enter- Abort Changes       */
            if l% <>  1% then L29210
                gosub clear_first
                goto screen_loop
L29210:     code$(1,1) = code$(2,1)
            gosub load_stated
            goto screen_loop

        REM *************************************************************~
            *           L O A D   D A T A   R O U T I N E S             *~
            *-----------------------------------------------------------*~
            *   Move a screen's worth of data into display arrays.      *~
            *************************************************************

        load_first        /* Load from top of file                     */
            plowkey$ = str(file$) & hex(00)
            goto load_data

        load_stated       /* Load from operator specified code         */
            if len(code$(1,1)) = 15 then                                 ~
             plowkey$ = str(file$)  &                                    ~
                    str(code$(1,1),,14)& bin(val(str(code$(1,1),15,1))-1)~
             else   plowkey$ = str(file$) & code$(1,1) & hex(00)
            gosub clear_first
            goto load_data

        load_next         /* Load from last code listed                */
            if eof$ <> "YES" then L30180
                errormsg$ = "ALREADY AT END OF FILE."
                return
L30180:     if last% = 1 then load_first
            plowkey$ = str(file$) & code$(last%,1) & hex(00)

        load_data
            errormsg$ = " "
            last% = 1
            eof$  = "NO"
            init (" ") code$(), descr$(), mcl$(), filler$()

          plowloop
            call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
            if f1%(1) = 1 then L30300
                eof$ = "YES"
                return
L30300:     last% = last% + 1  :  l% = last%
            gosub get_record   /* Load record and format for display   */
          if l% < 15 then plowloop else return


        load_specific     /* Get specific code from file               */
                          /* If new, supply defaults.                  */
            call "READ100" (#1, str(file$) & str(code$(l%,1)), f1%(1))
            if f1%(1) <> 1 then L30610
                gosub get_record
                return
L30610:   /* Not on file-- setup with defaults               */
            descr$(l%), mcl$(l%) = " "
            if dir% = 1% then mcl$(l%) = "15"
         return


        get_record   /* Get record from buffer and format for display  */
/* (CR1768) BEG */        
REM GET #1 USING L30850, CODE$(L%,1), DESCR$(L%), MCL$(L%),  FILLER$(L%)
               get #1 using L30850, code$(l%,1), str(descr$(l%),01%,30%),    ~
                          mcl$(l%), str(descr$(l%),31%,30%), filler$(l%)                 
/* (CR1768) END */                                
        return


L30850:         FMT  XX(09),             /* Logical File ID            */~
                     CH(15),             /* Code                       */~
                     CH(30),             /* Code Description           */~
                     CH(02),             /* Max Code Length (File Dir) */~
                     CH(30),             /* Code Description 2 (CR1768)*/~
                     CH(42)              /* Filler                     */                  
                     
REM  CH(72)              /* Filler                     */


        REM *************************************************************~
            *           S A V E   D A T A   O N    F I L E              *~
            *-----------------------------------------------------------*~
            *  Update file with code maintained (L%). After save, if    *~
            *  Code was on line 1 we redo screen with that code on top. *~
            *************************************************************
        save_data
            call "READ101" (#1, str(file$) & str(code$(l%,1)), f1%(1))
/*(CR1768) BEG */            
REM PUT #1 USING L31600, FILE$, CODE$(L%,1), DESCR$(L%), MCL$(L%), FILLER$(L%)
            put #1 using L31600, file$, code$(l%,1), str(descr$(l%),01%,30%), ~
                            mcl$(l%), str(descr$(l%),31%,30%), filler$(l%)
/*(CR1768) END */                            
            if f1%(1) = 0 then write #1 else rewrite #1

         /* Now make sure that code added/changed appears on screen.   */
         /* Also accessed by DELETE_CODE routine.                      */
         screen_align
            if l%   <> 1 then L31180
            if code$(l%,1) > code$(last%,1) and last% < 15 then L31180
            if code$(l%,1) < code$(2,1) or                               ~
               code$(l%,1) > code$(last%,1) then L31190

L31180:     code$(1,1) = code$(2,1)      /* Keeps the screen the same  */
                goto L31200
L31190:     code$(1,1) = code$(l%,1)     /* Put modified code on top   */
L31200:     gosub load_stated            /* Start display at CODE$(1)  */
          return

L31600:         FMT  CH(09),             /* Logical File ID            */~
                     CH(15),             /* Code                       */~
                     CH(30),             /* Code Description           */~
                     CH(02),             /* Max Code Width (File Dir)  */~
                     CH(30),             /* Code Description 2 (CR1768)*/~
                     CH(42)              /* Filler                     */
REM  CH(72)              /* Filler                     */

        REM *************************************************************~
            *             D E L E T E   C O D E                         *~
            *-----------------------------------------------------------*~
            *  Delete code from file after getting confirmation.        *~
            *************************************************************
        delete_code
          if dir% = 1% then delete_table
L32070:     errormsg$ = "CODE: " & code$(l%,1) & " (" & descr$(l%) & ")"
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "**** DELETE CODE ****",           ~
                            errormsg$,                                   ~
                            "(ENTER) RETURN -- DO NOT DELETE",           ~
                            "(PF 24) DELETE CODE            ")
            errormsg$ = " "
            if keyhit1%  =  0% then return
            if keyhit1% <> 24% then L32070
L32160:         call "DELETE" (#1, str(file$) & code$(l%,1), 24%)
                code$(1,1) = code$(l%,1)
                gosub screen_align
                return clear
                goto screen_loop


        delete_table
L32240:     errormsg$ = "TABLE: " & code$(l%,1) & " (" & descr$(l%) & ")"
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "**** DELETE TABLE ****",          ~
                            errormsg$,                                   ~
                            "(ENTER) RETURN -- DO NOT DELETE",           ~
                            "(PF 24) DELETE TABLE AND ALL IT'S CODES")
            errormsg$ = " "
            if keyhit1%  =  0% then return
            if keyhit1% <> 24% then L32240

                plowkey$ = code$(l%,1)
                call "DELETE" (#1, plowkey$, 9%)     /* Kills Codes */
                goto L32160     /* Delete Directory Entry & Continue */

        REM *************************************************************~
            *        R E P O R T   P R I N T I N G                      *~
            *-----------------------------------------------------------*~
            *  Print report for either ALL logical files -or- just one. *~
            *************************************************************
        report_printing

        /* First get option: All -or- one file.                        */

            errormsg$, file$, code$(), descr$(), title$, filler$() = " "
            str(title$,62%) = "GENCDSIN: " & cms2v$
            edtmessage$ = "Enter Table ID -or- 'ALL'.  Enter Blank or" & ~
                          " Partial ID for Table ID Display."
        accept                                                           ~
               at (01,02), "Print General System Codes",                 ~
               at (01,67), "Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "TABLE TO PRINT:",                            ~
               at (06,20), fac(hex(81)), file$,                          ~
                                                                         ~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
               at (24,02), "(3)Print File Directory",                    ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return      ",                           ~
                                                                         ~
               keys(hex(00030d0f10)),                                    ~
               key (keyhit%)

            errormsg$ = " "

            if keyhit%  =  3 then L33520      /* Print Directory   */
            if keyhit%  = 16 then return     /* End report option */

            if keyhit% <> 13 then L33380
                call "MANUAL" ("GENCDSIN")
                goto report_printing

L33380:     if keyhit% <> 15 then L33420
                call "PRNTSCRN"
                goto report_printing

L33420:     if keyhit% = 0 then gosub report_printing_specific
            goto report_printing

        report_printing_specific
            if file$ = "ALL" then L33520
                gcode$ = hx00$ &  file$
                call "PLOWCODE" (#1, gcode$, filedescr$, 9%, 0.30, f1%(1))
                file$ = str(gcode$,10)
                if f1%(1) = 1% then L33520
                     errormsg$ = "Table Does Not Exist: " & file$
                     return

L33520: /* Set up for printing within requested parameters.         */
            if file$ <> "ALL" then L33570
                filedescr$ = "** ALL CODE TABLES **"
                break%     = 0%
                goto L33650
L33570:     if keyhit% <> 3% then L33610
                filedescr$ = "** FILE DIRECTORY **"
                break%     = 9%
                goto L33650
L33610:     get #1 using L33620, filedescr$         /* Specific File    */
L33620:         FMT XX(24), CH(30)
            break% = 9%

L33650: /* Setups to print report                                      */
            page% = 0  :  line% = 857
            call "SETPRNT" ("SYS003", " ", 0%, 0%)
            call "SHOSTAT" ("Report for: " & filedescr$)
            select printer(134)
            oldfile$ = " "
            if file$ = "ALL" or keyhit% = 3% then file$ = hx00$
            plowkey$ = str(file$) & hex(00)

        report_loop
            call "PLOWNEXT" (#1, plowkey$, break%, f1%(1))
            if f1%(1) = 1 then L33770
                call "SETPRNT" ("SYS003", " ", 0%, 1%)
                close printer  :  select ws  :  return
REM  GET #1 USING L33780, CODE$(1,1), DESCR$(1)
L33770:     get #1 using L33780, code$(1,1), str(descr$(1),01%,30%),       ~
                       str(descr$(1),31%,30%)

L33780:         FMT XX(9), CH(15), CH(30), XX(2), CH(30)

            if oldfile$ = str(plowkey$,,9) and line% < 55 then L33990
                oldfile$ = str(plowkey$,,9)
                line% = 5%
                page% = page% + 1%
                call "READ100" (#1, hx00$ & str(oldfile$), f1%(1))
                if f1%(1) = 1 then get #1 using L33870, filedescr$ else   ~
                              filedescr$ = "?? UNDEFINED IN DIRECTORY ??"
L33870:              FMT XX(24), CH(30)
                if oldfile$ = hx00$ then filedescr$ = "FILE DIRECTORY"


                print page
                print using L34040, date$, page%
                print
                print using L34060, oldfile$, filedescr$
                print
                print using L34070, "  CODE", "   CODE DESCRIPTION "
                print using L34080

L33990:     print using L34070, code$(1,1), descr$(1)
            line% = line% + 1%
            goto report_loop


L34040: %########   SYS003         GENERAL CODES FILE LISTING            ~
        ~ ####
L34060: %TABLE: #########  ##############################
REM L34070 %             ###############  ##############################
REM L34080 %             ---------------  ------------------------------
L34070: % ###############  ##############################################~
        ~##############
L34080: % ---------------  ----------------------------------------------~
        ~--------------

        REM *************************************************************~
            *        D A T A   E N T R Y   S C R E E N                  *~
            *-----------------------------------------------------------*~
            *  Combination Display and Manage Screen.                   *~
            *                                                           *~
            *  There are essentially 2 entry points --                  *~
            *     line =  0 - Get from operator what to do. The code    *~
            *                 field on the 'wild line' is the only      *~
            *                 modifiable field.                         *~
            *     line <> 0 - Get entries for the line.                 *~
            *************************************************************
        deffn'101(l%)
            if l% <> 0 then L40260

          /* Case 1. Get what is to be done.                           */
            init (hex(84)) cfac$()  :  cfac$(1) = hex(81) /* Code FAC  */
            if file$ = "UOM" then cfac$(1) = hex(80) /* Code FAC UOM   */
            init (hex(8c)) dfac$()                        /* Line FAC  */
            if dir% = 0% then init (hex(9c)) lfac$()      /* Leng FAC  */~
                         else init (hex(8c)) lfac$()
            mode% = 1
            edtmessage$ = "Enter Code To Add Or Change, Leave Blan" &    ~
                          "k And Press ENTER to Find Existing Code"
            goto L40310

L40260:   /* Case 2. Modify line L%.                                   */
            init (hex(8c)) cfac$() : cfac$(l%) = hex(a4)
            edtmessage$ = "Modify field(s) then press (ENTER) to save."
            mode% = 2%

L40310:     title$ = filedescr$
            title$ = title$ & " (" & file$
            title$ = title$ & ")"
            str(title$,62%) = "GENCDSIN: " & cms2v$

            if dir% = 1% then goto file_directory               /* (CR1768) */

L40360: accept                                                           ~
          at(01,02),"General Codes Management System: Manage Code Table",~
          at(01,67), "Date:", fac(hex(8c)), date$               , ch(08),~
          at(02,02), fac(hex(ac)), title$                       , ch(79),~
          at(04,02), fac(hex(94)), errormsg$,                            ~
                                                                         ~
          at(05,02), fac(hex(ac)), hdr0$,                                ~
          at(05,20), fac(hex(ac)), hdr3$,                                ~
                                                                         ~
          at(06,02), fac(cfac$( 1)),     code$( 1,1),                    ~
          at(07,02), fac(cfac$( 2)),     code$( 2,1),                    ~
          at(08,02), fac(cfac$( 3)),     code$( 3,1),                    ~
          at(09,02), fac(cfac$( 4)),     code$( 4,1),                    ~
          at(10,02), fac(cfac$( 5)),     code$( 5,1),                    ~
          at(11,02), fac(cfac$( 6)),     code$( 6,1),                    ~
          at(12,02), fac(cfac$( 7)),     code$( 7,1),                    ~
          at(13,02), fac(cfac$( 8)),     code$( 8,1),                    ~
          at(14,02), fac(cfac$( 9)),     code$( 9,1),                    ~
          at(15,02), fac(cfac$(10)),     code$(10,1),                    ~
          at(16,02), fac(cfac$(11)),     code$(11,1),                    ~
          at(17,02), fac(cfac$(12)),     code$(12,1),                    ~
          at(18,02), fac(cfac$(13)),     code$(13,1),                    ~
          at(19,02), fac(cfac$(14)),     code$(14,1),                    ~
          at(20,02), fac(cfac$(15)),     code$(15,1),                    ~
                                                                         ~
          at(06,20), fac(dfac$( 1)),             descr$( 1),             ~
          at(07,20), fac(dfac$( 2)),             descr$( 2),             ~
          at(08,20), fac(dfac$( 3)),             descr$( 3),             ~
          at(09,20), fac(dfac$( 4)),             descr$( 4),             ~
          at(10,20), fac(dfac$( 5)),             descr$( 5),             ~
          at(11,20), fac(dfac$( 6)),             descr$( 6),             ~
          at(12,20), fac(dfac$( 7)),             descr$( 7),             ~
          at(13,20), fac(dfac$( 8)),             descr$( 8),             ~
          at(14,20), fac(dfac$( 9)),             descr$( 9),             ~
          at(15,20), fac(dfac$(10)),             descr$(10),             ~
          at(16,20), fac(dfac$(11)),             descr$(11),             ~
          at(17,20), fac(dfac$(12)),             descr$(12),             ~
          at(18,20), fac(dfac$(13)),             descr$(13),             ~
          at(19,20), fac(dfac$(14)),             descr$(14),             ~
          at(20,20), fac(dfac$(15)),             descr$(15),             ~
                                                                         ~
          at(21,02), fac(hex(a4)), edtmessage$,                          ~
          at(22,02), fac(hex(8c)), pfdescr$(1,mode%)            , ch(79),~
          at(23,02), fac(hex(8c)), pfdescr$(2,mode%)            , ch(79),~
          at(24,02), fac(hex(8c)), pfdescr$(3,mode%)            , ch(79),~
                                                                         ~
                keys(str(pfkeys$(mode%))),                               ~
                key (keyhit%)

               if keyhit% <> 13 then L41060
                  call "MANUAL" ("GENCDSIN")
                  goto L40360

L41060:        if keyhit% <> 15 then L41100
                  call "PRNTSCRN"
                  goto L40360

L41100:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())

               if keyhit% <> 0 then return
                if mode% <> 1 then return
                if code$(1,1) <> " " then return
                if cursor%(1) - 5 <> 1 then return
                gcode$ = code$(1,1)
                gcode$ = str(file$) & code$(1,1)
                call "PLOWCODE" (#1,gcode$,descr$(1),9%,0.3,f1%(1))
                     if f1%(1) = 0 then L40360
                code$(1,1) = gcode$
                code$(1,1) = str(gcode$,10)
                return

                                                            /* (CR1768) BEG */
file_directory:
        accept                                                           ~
          at(01,02),"General Codes Management System: Manage Code Table",~
          at(01,67), "Date:", fac(hex(8c)), date$               , ch(08),~
          at(02,02), fac(hex(ac)), title$                       , ch(79),~
          at(04,02), fac(hex(94)), errormsg$,                            ~
                                                                         ~
          at(05,12), fac(hex(ac)), hdr0$,                                ~
          at(05,30), fac(hex(ac)), hdr1$,                                ~
          at(05,65), fac(hdr2fac$),hdr2$,                                ~
                                                                         ~
          at(06,12), fac(cfac$( 1)),     code$( 1,1),                    ~
          at(07,12), fac(cfac$( 2)),     code$( 2,1),                    ~
          at(08,12), fac(cfac$( 3)),     code$( 3,1),                    ~
          at(09,12), fac(cfac$( 4)),     code$( 4,1),                    ~
          at(10,12), fac(cfac$( 5)),     code$( 5,1),                    ~
          at(11,12), fac(cfac$( 6)),     code$( 6,1),                    ~
          at(12,12), fac(cfac$( 7)),     code$( 7,1),                    ~
          at(13,12), fac(cfac$( 8)),     code$( 8,1),                    ~
          at(14,12), fac(cfac$( 9)),     code$( 9,1),                    ~
          at(15,12), fac(cfac$(10)),     code$(10,1),                    ~
          at(16,12), fac(cfac$(11)),     code$(11,1),                    ~
          at(17,12), fac(cfac$(12)),     code$(12,1),                    ~
          at(18,12), fac(cfac$(13)),     code$(13,1),                    ~
          at(19,12), fac(cfac$(14)),     code$(14,1),                    ~
          at(20,12), fac(cfac$(15)),     code$(15,1),                    ~
                                                                         ~
          at(06,30), fac(dfac$( 1)),         str(descr$( 1),01%,30%),    ~
          at(07,30), fac(dfac$( 2)),         str(descr$( 2),01%,30%),    ~
          at(08,30), fac(dfac$( 3)),         str(descr$( 3),01%,30%),    ~
          at(09,30), fac(dfac$( 4)),         str(descr$( 4),01%,30%),    ~
          at(10,30), fac(dfac$( 5)),         str(descr$( 5),01%,30%),    ~
          at(11,30), fac(dfac$( 6)),         str(descr$( 6),01%,30%),    ~
          at(12,30), fac(dfac$( 7)),         str(descr$( 7),01%,30%),    ~
          at(13,30), fac(dfac$( 8)),         str(descr$( 8),01%,30%),    ~
          at(14,30), fac(dfac$( 9)),         str(descr$( 9),01%,30%),    ~
          at(15,30), fac(dfac$(10)),         str(descr$(10),01%,30%),    ~
          at(16,30), fac(dfac$(11)),         str(descr$(11),01%,30%),    ~
          at(17,30), fac(dfac$(12)),         str(descr$(12),01%,30%),    ~
          at(18,30), fac(dfac$(13)),         str(descr$(13),01%,30%),    ~
          at(19,30), fac(dfac$(14)),         str(descr$(14),01%,30%),    ~
          at(20,30), fac(dfac$(15)),         str(descr$(15),01%,30%),    ~
                                                                         ~
          at(06,70), fac(lfac$( 1)),             mcl$( 1),               ~
          at(07,70), fac(lfac$( 2)),             mcl$( 2),               ~
          at(08,70), fac(lfac$( 3)),             mcl$( 3),               ~
          at(09,70), fac(lfac$( 4)),             mcl$( 4),               ~
          at(10,70), fac(lfac$( 5)),             mcl$( 5),               ~
          at(11,70), fac(lfac$( 6)),             mcl$( 6),               ~
          at(12,70), fac(lfac$( 7)),             mcl$( 7),               ~
          at(13,70), fac(lfac$( 8)),             mcl$( 8),               ~
          at(14,70), fac(lfac$( 9)),             mcl$( 9),               ~
          at(15,70), fac(lfac$(10)),             mcl$(10),               ~
          at(16,70), fac(lfac$(11)),             mcl$(11),               ~
          at(17,70), fac(lfac$(12)),             mcl$(12),               ~
          at(18,70), fac(lfac$(13)),             mcl$(13),               ~
          at(19,70), fac(lfac$(14)),             mcl$(14),               ~
          at(20,70), fac(lfac$(15)),             mcl$(15),               ~
                                                                         ~
          at(21,02), fac(hex(a4)), edtmessage$,                          ~
          at(22,02), fac(hex(8c)), pfdescr$(1,mode%)            , ch(79),~
          at(23,02), fac(hex(8c)), pfdescr$(2,mode%)            , ch(79),~
          at(24,02), fac(hex(8c)), pfdescr$(3,mode%)            , ch(79),~
                                                                         ~
                keys(str(pfkeys$(mode%))),                               ~
                key (keyhit%)

               if keyhit% <> 13 then L41260
                  call "MANUAL" ("GENCDSIN")
                  goto file_directory

L41260:        if keyhit% <> 15 then L42100
                  call "PRNTSCRN"
                  goto file_directory

L42100:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())

               if keyhit% <> 0 then return
                if mode% <> 1 then return
                if code$(1,1) <> " " then return
                if cursor%(1) - 5 <> 1 then return
                gcode$ = code$(1,1)
                gcode$ = str(file$) & code$(1,1)
                call "PLOWCODE" (#1,gcode$,descr$(1),9%,0.3,f1%(1))
                     if f1%(1) = 0 then file_directory
                code$(1,1) = gcode$
                code$(1,1) = str(gcode$,10)
                return
                                                            /* (CR1768) BEG */
        REM *************************************************************~
            *           E D I T   D A T A                               *~
            *-----------------------------------------------------------*~
            *  Edit data fields for line entered. If in error, set      *~
            *  message and FACs, else just return.                      *~
            *************************************************************
        deffn'151(l%)
            init (hex(8c)) dfac$(), lfac$()
            errormsg$ = " "

          /* Description                                               */
             if descr$(l%) <> " " then  L50160
                errormsg$ = "Sorry, Description Can't Be Blank"
                dfac$(l%) = hex(80)
                return

L50160:   /* Maximum Code Length                                       */
            if dir% = 0% then return
            convert mcl$(l%) to temp%, data goto L50200
            if temp% >= 1% and temp% <= 15% then L50220
L50200:         errormsg$ = "MAXIMUM CODE LENGTH: 1 - 15"
L50205:         lfac$(l%) = hex(82)
                return
L50220:     plowkey$ = code$(l%,1) : str(plowkey$,10) = hex(00)
L50230:     call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
            if f1%(1) = 0% then L50300
                get #1 using L50260, temp$
L50260:              FMT XX(9), CH(15)
                if len(temp$) <= temp% then L50230
                     errormsg$ = "Longer Codes Are Already In Table."
                     goto L50205
L50300:     convert temp% to mcl$(l%), pic(#0)
            return

        REM *************************************************************~
            *        M I S C.  S U B - R O U T I N E S                  *~
            *************************************************************

        clear_first       /* Clear array bucket 1                      */
            code$(1,1), descr$(1), mcl$(1), filler$(1) = " "
        return


L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end



