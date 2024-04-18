        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC   PPPP   RRRR    GGG    *~
            *  H   H  NN  N  Y   Y  C   C  C   C  P   P  R   R  G       *~
            *  HHHHH  N N N   YYY   C      C      PPPP   RRRR   G GGG   *~
            *  H   H  N  NN    Y    C   C  C   C  P      R R    G   G   *~
            *  H   H  N   N    Y     CCC    CCC   P      R  R    GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCRPT - This program provides the means to purge select*~
            *            ed records or all data from the Cycle Count    *~
            *            Master File (HNYCCMST). For every record purged*~
            *            there will be a hard copy record printed.      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/05/92 ! Original                                 ! RJH *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            * 01/22/93 ! Fix File pointer problems when looping   ! RJH *~
            *          !  thru Group Name purges.                 !     *~
            * 08/29/95 ! PRR 13497 - Fix group purge looping prob.! RJH *~
            *          !  when 'A'ctive record hit.               !     *~
            * 08/08/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**



        dim        /* ** Program Variables **                          */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cntr$7,                      /* # of rec deleted from file */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmccgroup$6,                 /* Cycle Count Group Name:    */~
            hiccgroup$6,                 /* Cycle Count Group Name:    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            loccgroup$6,                 /* Cycle Count Group Name:    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            r$(24)80,                    /* Screen Image from Range Sel*/~
            rangeflag$1,                 /* Range method of selection  */~
            readkey$99,                  /* Misc. Read/Plow key        */~
            reporttype$1,                /* Report Type (detl, summary)*/~
            rptid$6,                     /* Report ID                  */~
            rpttitle$60,                 /* Report Title               */~
            tempval$66,                  /* Temporary String Value     */~
            time$8,                      /* System Time                */~
            toccgroup$6,                 /* Cycle Count Group Name:    */~
            userid$3                     /* Current User Id            */



            dim     /* ** Cycle Count Variables **                     */~
            abcclass$1,                  /* ABC Class                  */~
            abcold$1,                    /* Old ABC Class              */~
            abclastdate$10,              /* Date ABC last changed      */~
            abclastuser$3,               /* Last user to change ABC    */~
            abclockflag$1,               /* ABC Lock Flag              */~
            actflag$1,                   /* Active Session Flag        */~
            ccgroup$6,                   /* Cycle Count Group Code     */~
            cntlastdate$10,              /* Date Count Period Last Chng*/~
            cntlastuser$3,               /* Last User to Change CC Prd */~
            cntnbr$10,                   /* Number of Count Sessions   */~
            cntperiod$3,                 /* Count Period               */~
            cntperiodflag$3,             /* Count Period Lock Flag     */~
            cntperiodold$3,              /* Count Period Last Value    */~
            cnttlernper$10,              /* Count Tolerance in Percent */~
            cnttlernqty$10,              /* Count Tolerance Quantity   */~
            cumbohqty$12,                /* Cumulative BOH Quantity    */~
            cumcntdltam$12,              /* Cumulative Qnty. Delta (-) */~
            cumcntdltap$12,              /* Cumulative Qnty. Delta (+) */~
            cumtlernhit$10,              /* Cumulative Tolerance Hits  */~
            datecnt$10,                  /* Last Count Date            */~
            firstcntdate$10,             /* First Count Date           */~
            lot$6,                       /* Lot Number of Part         */~
            nextcntdate$10,              /* Next Count Date            */~
            oldtransthresftr$12,         /* Old Trans. Threshold Factr */~
            part$25,                     /* Part Number                */~
            recdate$10,                  /* Date Record was created    */~
            store$3,                     /* Store/warehouse            */~
            transfreqfact$10,            /* Transaction Frequence Factr*/~
            transfreqold$10,             /* Old Transaction Freq. Factr*/~
            translastuser$3,             /* Last User to Change TranFac*/~
            translastdate$10,            /* Date TransFac Last Changed */~
            translockflag$1,             /* TransFac Lock Flag         */~
            transthresftr$12,            /* Transaction Threshold Factr*/~
            variable$200                 /* User Defined Variable Field*/

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #01 ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #03 ! HNYCCMST ! Cycle Count Master File                  *~
            * #04 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #05 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #07 ! STORNAME ! Store Names and Addresses                *~
            * #09 ! CATEGORY ! Inventory Category Descriptions          *~
            *                                                           *~
            * #50 ! WORKFIL1 ! WORKFILE for Selection Sorting Subroutine*~
            * #52 ! WORKFIL2 ! WORKFILE for Report Sort and Printing    *~
            * #54 ! WORKFIL3 ! WORKFILE for Lines to delete             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select #02, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44
                                                                         ~
            select #03, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize = 796,               ~
                        keypos =    1,  keylen = 44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup     ~

            select #04, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize = 436,               ~
                        keypos =    1,  keylen = 41,                     ~
                        alt key  1, keypos =   13, keylen =  45, dup,    ~
                            key  2, keypos =   14, keylen =  44, dup     ~

            select #05, "HNYCCGRP",                                      ~
                        varc,     indexed,  recsize =  616,              ~
                        keypos =    1, keylen =   6                      ~

            select #07, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #09, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4                      ~

            select #50, "WORKFIL1",                                      ~
                        varc,     indexed,  recsize = 400,               ~
                        keypos = 1,    keylen =   44                     ~

            select #52, "WORKFIL2",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen =58                        ~

            select #54, "WORKFIL3",                                      ~
                        varc,     indexed,  recsize =  44,               ~
                        keypos = 1,    keylen =44                        ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            rec% = val(str(rslt$(03%),17,4),4) / 2%
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            ret% = 0%  :  variable$ = " "
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "       CYCLE COUNT MASTER FILE          " &     ~
                        "                              "
            rptid$ = "HNY051"
            str(columnttl$, 1) = "From"
            str(columnttl$,10) = "To"

            str(line2$,62) = "HNYCCPRG: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  1%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 08% and fieldnr% = 1%                 ~
                                                 then  gosub  pick_range ~
                                                 else L10210
                      if rangeflag$ = "N" then L10100 else L10240

L10210:               if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
L10240:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       purge_records
                  if keyhit% <>  0% then       editpg1
L11120:     if cursor%(1%)  = 7%  then fieldnr% =  1% else fieldnr% = 0%

            if fieldnr% < 1% or fieldnr% >  1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11230:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11230
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11230
                  lastfieldnr% = fieldnr%
            goto L11120


        REM *************************************************************~
            *           P U R G E   &   R E P O R T                     *~
            *-----------------------------------------------------------*~
            * Main Section of Purge and Report                          *~
            *************************************************************
        purge_records
            keyhit% = 0%
L13060:     call "ASKUSER" (keyhit%,"** PURGE CYCLE COUNT RECORDS **  ", ~
                            "PRESS PF(16) TO ALLOW PURGE OF       " &    ~
                            " MASTER FILE RECORDS                 ",     ~
                            "             -- OR --",                     ~
                            "PRESS (RETURN) TO QUIT ")
            if keyhit% = 16% then goto L13170
            if keyhit% <> 0% then goto L13060     /* Try Again */
            goto inputmode                       /* Ok then, Start again*/

L13170:     gosub print_and_purge        /* Loop Thru File and Purge */
            gosub end_report                /* Report Ending Routine */

            convert cntr% to cntr$, pic(#######)
            keyhit% = 2% /* Window in the Bottom */
            call "ASKUSER" (keyhit%, "*** RESULTS ****",                 ~
                "There are a total of " & hex(80) & cntr$ & " deleted",  ~
                "records", "Press Any key to RETURN")

            goto inputmode

        REM *************************************************************~
            *          P I C K R A N G E  S E L E C T I O N             *~
            *-----------------------------------------------------------*~
            * Sets Report Items from Range Selection Method             *~
            *************************************************************

        pick_range
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#09, fs%(09), f2%(07), 0%, rslt$(09))
            call "FILEBGON" addr(#50)
            call "WORKOPEN" (#50, "IO", rec%, f2%(50))

            call "HNYCCRNG" ("Y", 2%, #01, #02, #09, #07, #03, #50,      ~
                             rngcnter%,                                  ~
                            "Purge Cycle Count Master File and Report" , ~
                            "(16)Continue    ",                          ~
                             r$() )
            if rngcnter% > 0%  then L16140
            rangeflag$ = "N"  :  goto L16160
L16140:     rangeflag$ = "Y"  :  fmccgroup$ = "RANGE" : toccgroup$ = " "

L16160:     return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20130          /* Cycle Count Group      */

            return

L20130: REM Def/Enable Cycle Count Group Name      FMCCGROUP$
            if fmccgroup$ = " " then fmccgroup$ = "ALL"

            return
        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Cycle Count Group Name or PF 8 to Select Ranges.       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmccgroup$, hiccgroup$, loccgroup$, toccgroup$,    ~
                      reporttype$, r$()
            call "ALLFREE"
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *       P U R G E   &     R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Work section for Purging and Reporting                    *~
            *************************************************************
        print_and_purge
            select printer(134)
            call "SHOSTAT" ("Purging Records Now")
            time$ = " "  :  call "TIME" (time$)
            cntr% = 0%
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            str(rpttitle$,33)= "PURGED RECORDS REPORT"

            if lcntr% > 56% then gosub page_head

            if pcntr% = 1% then gosub column_head

*         ** Start Purge Loop **

            if rangeflag$ = "Y" then goto range_method
            /* ELSE fall into CC GROUP Method */

            /* ** Cycle Count Group Method ** */
            if fmccgroup$ = "ALL"  or  fmccgroup$ =  "FIRST"             ~
                  then plowkey$ =  all(hex(00))                          ~
                  else plowkey$ =  fmccgroup$

            call "REDALT4" (#03, plowkey$, 1%, f1%(03))
            if  f1%(03) = 0% then  return             /* Nothing to Do */
            goto L30315
        loop_group    /* CC Group Selection Loop */
            call "READNEXT" (#03, f1%(03))
            if  f1%(03) = 0% then  goto workfile_delete /*Really delete*/
L30315:     plowkey$ = key(#03, 1%)
            if fmccgroup$ <> "ALL"  and                                  ~
                 plowkey$  > hiccgroup$ then workfile_delete
            gosub print_line
            gosub purge_group_line
            goto loop_group
            /* ** END of CC Group Method of Selection ** */

        range_method
            readkey$ = all(hex(00))
            call "READ104" (#50, readkey$, f1%(51))
            if f1%(51) = 0% then return             /* No More To Do */  ~
                            else L30470

        loop_range      /* Range Method Loop Thru Files */
            call "READNEXT" (#50, f1%(51))
            if f1%(51) = 0% then return               /* No More To Do */
L30470:     readkey$ = key(#50)
            call "READ100" (#03, readkey$, f1%(03))
            if f1%(03) = 0% then goto loop_range    /* Shouldn't Happen */
            gosub print_line
            gosub purge_range_line
            goto loop_range
            /* ** END of Range Method of Selection ** */

        print_line
            get #03  using L35060,                                        ~
                part$, store$, lot$, ccgroup$, datecnt$, cnttlernper,    ~
                cnttlernqty, cntnbr , actflag$, nextcntdate$, recdate$,  ~
                cumtlernhit, cumcntdltap, cumcntdltam, cumbohqty,        ~
                abclockflag$,abcclass$,abcold$,abclastuser$,abclastdate$,~
                translockflag$,transfreqfact,transfreqold,transthresftr, ~
                oldtransthresftr, translastuser$, translastdate$,        ~
                cntperiodflag$, cntperiod$,cntperiodold$,cntlastuser$,   ~
                cntlastdate$, firstcntdate$

            if actflag$ = "A" then return  /* Can't Purge an Active Part*/

            cntr% = cntr% + 1%
            call "CONVERT" (cnttlernper     , 2.2, cnttlernper$)
            call "CONVERT" (cnttlernqty     , 2.2, cnttlernqty$)
            call "CONVERT" (cntnbr          ,0.01, cntnbr$)
            call "CONVERT" (transfreqfact   , 2.2, transfreqfact$)
            call "CONVERT" (cumtlernhit     ,0.01, cumtlernhit$)
            call "CONVERT" (cumcntdltap     , 2.2, cumcntdltap$)
            call "CONVERT" (cumcntdltam     , 2.2, cumcntdltam$)
            call "CONVERT" (cumbohqty       , 2.2, cumbohqty$)
            call "CONVERT" (transfreqfact   , 2.2, transfreqfact$)
            call "CONVERT" (transfreqold    , 2.2, transfreqold$)
            call "CONVERT" (transthresftr   , 2.2, transthresftr$)
            call "CONVERT" (oldtransthresftr, 2.2, oldtransthresftr$)

            call "DATEFMT" (datecnt$)
            call "DATEFMT" (nextcntdate$)
            call "DATEFMT" (recdate$)
            call "DATEFMT" (abclastdate$)
            call "DATEFMT" (translastdate$)
            call "DATEFMT" (cntlastdate$)
            call "DATEFMT" (firstcntdate$)
            call "RJUSTIFY" (cntperiod$)

            lcntr% = lcntr% + 6%
            if lcntr% < 51 then L31050
                gosub page_head : gosub column_head : lcntr% = lcntr% + 6%

L31050:     print using    L60250, part$, store$, lot$, ccgroup$,abcclass$~
                             ,cntperiod$, nextcntdate$,datecnt$,         ~
                              firstcntdate$, cntnbr$,                    ~
                              cumtlernhit$, actflag$, recdate$

            print using    L60290,                                        ~
                     cnttlernper$, abcclass$, cntperiod$, transfreqfact$

            print using    L60330,                                        ~
                     cnttlernqty$, abcold$, cntperiodold$, transfreqold$

            print using    L60370,                                        ~
                     cumbohqty$,abclastdate$,cntlastdate$,translastdate$

            print using    L60410,                                        ~
                    cumcntdltap$,abclastuser$,cntlastuser$,translastuser$

            print using    L60450,                                        ~
                  cumcntdltam$,abclockflag$,cntperiodflag$,translockflag$

            print

            return
            /* *** End of Print Line Sub ** */

        purge_group_line
            if open_54% =  1%  then L31310
            call "WORKOPEN" (#54, "IO", rec%, f2%(54%))
                open_54% = f2%(54%) + 1%
L31310:     if actflag$ = "A" then return  /* Can't Purge an Active Part*/
            if actflag$ = "P" then gosub delete_from_detail

            put #54 using L31330, part$, store$, lot$
L31330:        FMT CH(25), CH(3), CH(16)

            write #54
            delete% = 1%

            return

        delete_from_detail
            readkey$ = "P" & str(part$) & str(store$) & lot$
            call "REDALT1" (#04, readkey$, 1%, f1%(04))
            if f1%(04) = 0% then return
            delete #04

            return

        purge_range_line
            if actflag$ = "A" then return  /* Can't Purge an Active Part*/
            if actflag$ = "P" then gosub delete_from_detail
            readkey$ =  str(part$) & str(store$) & lot$
            call "DELETE" (#03, readkey$, 44%)

            return

        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L64965, time$    /* End of report line */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            call "FILEBGON" (#52)   /* Close and Zap Work File */
            if rangeflag$ = "Y" then call "FILEBGON" (#50)/*Zap Range WF*/
            return

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "HNYCCPRG", rptid$
            print using L60110, rpttitle$, pcntr%
            print
            if pcntr% = 0% then gosub print_params
            lcntr% = 3%
            return

        column_head
            print using L60124
            print using L60150
            print using L60174
            lcntr% = lcntr% + 3%
            return

        print_params           /* Print Page Zero */
L34805:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34825
                str(i$(), i%, 1%) = hex(20)
                goto L34805
L34825:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            if fmccgroup$ <> "RANGE" then L34935
                print
                for x% = 4% to 20% : print tab(26); r$(x%) : next x%
                print tab(26);
                print "--------------------------------------------------~
        ~------------------------------"
L34935:     gosub page_head
            return


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: HNYCCMST                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(3),          /* Store Name                              */~
            CH(16),         /* Lot Number                              */~
            CH(6),          /* Cycle Count Group Code Name             */~
            CH(6),          /* Date Last Counted                       */~
            PD(14,4),       /* Count Tolerance Percentage              */~
            PD(14,4),       /* Count Tolerance Quantity                */~
            PD(14,4),       /* Number of Counts                        */~
            CH(1),          /* Active Session Flag                     */~
            CH(6),          /* Next Count Date                         */~
            CH(6),          /* Record Start Date                       */~
            PD(14,4),       /* Cumulative Tolerance Hits               */~
            PD(14,4),       /* Cumulative Count Delta (+)              */~
            PD(14,4),       /* Cumulative Count Delta (+)              */~
            PD(14,4),       /* Cumulative BOH Quantity                 */~
            CH(1),          /* Lock Flag for ABC Class                 */~
            CH(1),          /* ABC Class                               */~
            CH(1),          /* Old ABC Class                           */~
            CH(3),          /* User Who Last Changed ABC Class         */~
            CH(6),          /* Date ABC Class Last Changed             */~
            CH(1),          /* Lock Flag for Transaction Factor        */~
            PD(14,4),       /* Transaction Frequency Factor            */~
            PD(14,4),       /* Old Transaction Frequency Factor        */~
            PD(14,4),       /* Transaction Threshold Factor            */~
            PD(14,4),       /* Old Transaction Threshold Factor        */~
            CH(3),          /* User Last Changed Transaction Frequency */~
            CH(6),          /* Date Transaction Frequency Factor Last C*/~
            CH(1),          /* Lock Flag - Count Period                */~
            CH(3),          /* Count Period in (Days)                  */~
            CH(3),          /* Old Count Period                        */~
            CH(3),          /* User Who Last Changed Count Period      */~
            CH(6),          /* Date Count Period Last Changed          */~
            CH(6)           /* First Count Date                        */

        workfile_delete
            if delete% = 0% then return
            readkey$ = all(hex(00))
            call "READ104" (#54, readkey$, f1%(54%))
                goto L36530
L36520:     call "READNEXT" (#54, f1%(54%))
L36530:     if f1%(54%) = 0% then L36960
            get #54 using L36550, readkey$
L36550:     FMT CH(44)

            call "DELETE" (#03, readkey$, 44%)

            goto L36520  /* Readnext */

L36960:     call "FILEBGON" (#54)
            delete%, open_54% = 0%

            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40170,         /* Cycle Count Group */   ~
                                L40170,         /* Sort Criteria     */   ~
                                L40170          /* Report Type       */

              goto L40200
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Purge Cycle Count Master File and Report",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(18),~
                                                                         ~
               at (07,02), "Cycle Count Group Name ",                    ~
               at (07,30), fac(lfac$( 1)), fmccgroup$           , ch(06),~
               at (07,39), fac(lfac$( 1)), toccgroup$           , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40620
                  call "MANUAL" ("HNYCCPRG") : goto L40200

L40620:        if keyhit% <> 15 then L40650
                  call "PRNTSCRN" : goto L40200

L40650:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40840     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (8)Select Ranges       " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffffff0dff0f1000)
            if fieldnr% = 1% then L40810
                str(pf$(3),,63)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                                             str(pfkeys$, 8,1) = hex(ff)
            if fieldnr% > 1% then L40820
L40810:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
                if fmccgroup$ <> " " and fmccgroup$ <> "ALL"             ~
                                     then   str(pfkeys$, 8,1) = hex(ff)
L40820:     return

L40840: if fieldnr% > 0% then L40930  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Purge       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40930:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130          /* Cycle Count Group      */~

            return

L50130: REM Test for Cycle Count Group Name       FMCCGROUP$
            if fmccgroup$ = " " and toccgroup$ = " " then doto% = 1%     ~
                                                     else doto% = 0%
            if fmccgroup$ = "ALL" then L50200
            if fmccgroup$ = "FIRST" then L50210
                tempval$ = hex(0684) & "Select FROM Cycle Count Group"
                call "GETCODE" (#05, fmccgroup$, tempval$, 0%, 0, f1%(5))
                if fmccgroup$ <> "?" then L50190
L50185:         errormsg$ = "'?' is Not a Valid Range" :  return

L50190:     if toccgroup$ <> " "  or doto% = 1% then  L50210
L50200:         toccgroup$ = fmccgroup$     :   goto  L50260
L50210:     if toccgroup$ = "LAST" then  L50260
                tempval$ = hex(0684) & "Select TO Cycle Count Group"
                call "GETCODE" (#05, toccgroup$, tempval$, 0%, 0, f1%(5))
                if toccgroup$ <> "?" then L50260 else L50185

L50260:     call "TESTRNGE"                                              ~
                  (fmccgroup$          , toccgroup$          ,           ~
                   loccgroup$          , hiccgroup$          ,           ~
                   errormsg$)

            if fmccgroup$ = "FIRST" then  loccgroup$ =  all(hex(20))
            if fmccgroup$ = "ALL" or toccgroup$ = "LAST" then            ~
                                                hiccgroup$ = all(hex(ff))


            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

*       * Column Header Line 1
L60124: %                                        CYCLE        CNT  NEXT  ~
        ~      LAST        FIRST       NUMBER OF   NUMBER OF  SSN  RECORD

*       * Column Header Line 2
L60150: %PART NUMBER                STR  LOT     GROUP   ABC  PRD  COUNT ~
        ~DATE  COUNT DATE  COUNT DATE     COUNTS        HITS  ACT  STARTDA~
        ~TE
*       * Column Header Line 3
L60174: %-------------------------  ---  ------  ------  ---  ---  ------~
        ~----  ----------  ---------- ----------  ----------  ---  -------~
        ~--
*       * Detail Report Line 1
L60250: %#########################  ###  ######  ######  ###  ###   #####~
        ~###    ########    ########  #########  ##########   #    ########

*       * Detail Report Subline 1
L60290: %      CNT TOL PRCNT:   ##########   ABC CLASS : #            CNT~
        ~ PERIOD: ###          TRANS FREQUENCY: ############

*       * Detail Report Subline 2
L60330: %      CNT TOL QNTY :   ##########   ABC OLD   : #            CNT~
        ~ OLD   : ###            FREQUENCY OLD: ############

*       * Detail Report Subline 3
L60370: %      CUM BOH QNTY : ############    CHNG DATE: ##########    CH~
        ~NG DATE: ##########         CHNG DATE: ##########

*       * Detail Report Subline 4
L60410: %      CUM VARIANCE+: ############    LAST USER: ###           LA~
        ~ST USER: ###                LAST USER: ###

*       * Detail Report Subline 5
L60450: %      CUM VARIANCE-: ############    LOCK FLAG: #             LO~
        ~CK FLAG: #                  LOCK FLAG: #

        %** Report Title for page 0
        %############################################################

L64965:         %                          * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @   ########   * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
