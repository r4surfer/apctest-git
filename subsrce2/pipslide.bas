        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   IIIII  PPPP    SSS   L      IIIII  DDDD   EEEEE   *~
            *  P   P    I    P   P  S      L        I    D   D  E       *~
            *  PPPP     I    PPPP    SSS   L        I    D   D  EEEE    *~
            *  P        I    P          S  L        I    D   D  E       *~
            *  P      IIIII  P       SSS   LLLLL  IIIII  DDDD   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPSLIDE - For a given PIPIN, this routine will offset    *~
            *            all PIPOUT, WCOUT and misc. data based on the  *~
            *            difference between the old PIPIN due date and  *~
            *            the new date or offset entered by the user.    *~
            *            Processing is somewhat complicated by the fact *~
            *            that PIPOUTs and WCOUTs cannot be processed    *~
            *            'in place' in the file and must instead be     *~
            *            handled all at once in an array in memory.     *~
            *            Also, any similarity between parts of this     *~
            *            program and JBRETAG are entirely intentional!  *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/09/90 ! Original                                 ! WPH *~
            * ???????? !(PRR 11442) Resolved by this Program      ! WPH *~
            * 08/05/91 !(QC-FIXES) Changed to a INIT(" ") for     ! RJB *~
            *          !     RTESTEP$() & TRAVEL$(). Added call to!     *~
            *          !     'ALLFREE'.                           !     *~
            * 08/08/91 ! Minor refinements to validation section  ! WPH *~
            * 06/10/92 ! Made WCOUT step through directionally    ! KAB *~
            *          ! sensitive to avoid FS 22. (Minimal Patch)!     *~
            * 10/19/93 ! Slide Purchase Jobs, Too                 ! KAB *~
            * 06/30/94 ! Vendor Service Advice management added   ! ERN *~
            * 07/18/96 ! Changes for the year 2000.               ! DXL *~
            * 07/18/97 ! Tag No back to YYMMDD format             ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "PIPSLIDE" (tag$,            /* TAG Number of PIPIN        */~
                        #60,             /* SYSFILE2                   */~
                        #41,             /* SFCUM2                     */~
                        #9,              /* JBMASTR2                   */~
                        #11,             /* WCMASTER                   */~
                        #8,              /* JBCROSS2                   */~
                        #2,              /* PIPMASTR                   */~
                        #23,             /* WCOUT                      */~
                        #33,             /* PIPIN                      */~
                        #34,             /* PIPOUT                     */~
                        #35,             /* PIPCROSS                   */~
                        #36,             /* JBPIPXRF                   */~
                        #4,              /* HNYMASTR                   */~
                        #7,              /* RTEMASTR                   */~
                        #15,             /* BOMMASTR                   */~
                        #24)             /* ENGMASTR                   */

        dim actstdate$8,                 /* Actual Job Start Date      */~
            adj%(490),                   /* Adjusted WC usage array    */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            control$19,                  /* Control Number             */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dateclosed$8,                /* Job closed date            */~
            deletekey$100,               /*                            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fixedstep$10,                /* Fixed Step                 */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jbdescr$30,                  /* Job Description            */~
            job$8,                       /* Job Number                 */~
            jobtag$19,                   /* Job or tag number for disp */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mode$1,                      /* Slide Mode                 */~
            newduedatef$8,               /* New Job Due Date Formatted */~
            newduedateu$8,               /* New Job Due Date Unformat  */~
            newstartu$8,                 /* New Start Date unformatted */~
            offset$4,                    /* Offset Number of Days (+/-)*/~
            oldstartdate$6,              /* Old Start Date             */~
            part$25,                     /* Part to build              */~
            partdescr$32,                /* Part Description           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            plstdate$8,                  /* Job Planned Start Date     */~
            plstdateu$8,                 /* Job Planned Start Dte Unfmt*/~
            plenddate$8,                 /* Job Planned End Date       */~
            plenddateu$8,                /* Job Planned End Date Unfmt */~
            pldate$6,                    /* Based date of calendar     */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            qtymake$10,                  /* Quantity to make           */~
            qtycomp$10,                  /* Quantity complete          */~
            qtyleft$10,                  /* Quantity remaining to build*/~
            qtyorig$10,                  /* Original job quantity      */~
            qtyscrp$10,                  /* Quantity scrapped          */~
            qtyrewk$10,                  /* Quantity reworked          */~
            qtyadj$10,                   /* Quantity adjusted          */~
            step$10,                     /* Route Step                 */~
            tagdate$6,                   /* Tag No date, YYMMDD used   */~
            tagdtfull$8,                 /* Tag No date, CCYYMMDD      */~
            tagdttmp$8,                  /* temp/scratch date          */~
            temp2$10,                    /*                            */~
            time$8,                      /*                            */~
            tmpdate$8,                   /*                            */~
            travel$(256)67,              /* Formatted WCOUT data       */~
            userid$3,                    /* Current User Id            */~
            used%(490),                  /* Workcenter usage array     */~
            newtag$19,                   /* New tag number constructed */~
            readkey$200                  /*                            */


        dim ed%(1000),                   /*                            */~
            rtestep$(255)200,            /*  Built by PLNTRVLR         */~
            record$(2500)68,             /*  PIPOUT/WCOUT records array*/~
            wc$(255)4                    /* Workcenter code            */


        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

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
            *              Select and Open a Workfile                   *~
            *************************************************************
            select #10, "WORKFILE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 54,                                    ~
                        keypos = 1, keylen = 9,                          ~
                        alt key 1, keypos = 10, keylen = 43, dup

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            tagdttmp$ = date
            call "DATEFMT" ( tagdttmp$, 0%, tagdtfull$ )
            tagdate$ = str( tagdtfull$, 3%, 6% )


            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "READ100" (#60, "MONTHS OPEN", f1%(60))
            if f1%(60) <> 0% then L09067
                errormsg$ = "SYSFILE2 Months Open Record not Found"
                goto error_exit
L09067:     get #60, using L09068, pldate$
L09068:         FMT XX(32), CH(6)
            call "DATE" addr("G-", pldate$, date$, today%, err%)
                if err%<>0% then L65000
            today%=today%+1%
            if today% >= 1% or today% <= 490% then L09088
                errormsg$ = "Todays date is outside the Planing Calendar"
                goto error_exit
L09088:     call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "PIPSLIDE: " & str(cms2v$,,8)

            gosub load_pipin_data

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            edit% = 0%
            for fieldnr% = 1% to  3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            edit% = 1%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 16% then       datasave
                if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 11%
            if cursor%(1%) = 14% then fieldnr% = 2%
            if cursor%(1%) = 15% then fieldnr% = 3%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
            if mode$ <> "2" and fieldnr% = 3% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L11170
                if forcestep% <> 1% then L11220
                   fieldnr%  = 3%
                   goto L11170
L11220:         lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub pre_slide_date_check
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Slide Mode             */~
                              L20200,         /* New Due Date or Offset */~
                              L20400          /* Fixed Step             */
            return
L20100: REM Def/Enable Slide Mode                  MODE$
            if edit% = 1% then return
            mode$ = "1"
            return

L20200: REM Def/Enable New Due Date/Offset     NEWDUEDATEF$ OFFSET$
            return

L20400: REM Def/Enable Fixed Step                  FIXEDSTEP$
            if mode$ = "1" then enabled% = 0%
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a '1' to slide entire job/advice or '2' to slide from a s~
        ~tep.",                                                           ~
         "Enter New Job Due Date or Number of Days to Offset (+/-)     ",~
         "Enter Fixed Step (Slide will occur for all later steps.)     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$,                            ~
                      fixedstep$, mode$, newduedatef$, offset$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        load_pipin_data
            call "SHOSTAT" ("Loading Data for Selected PIPIN")
            if str(tag$,1,2) = "WO" then L30460
            if str(tag$,1,2) = "BW" then L30460

*        Job Master File Load if the PIPIN is a job
            job$ = str(tag$,12,8)
            jobtag$ = job$

            call "READ100" (#9, job$, f1%(9))
            if f1%(9) <> 0% then L30180
                errormsg$ = "Job Master File record not found"
                goto error_exit
L30180:     get #9, using L30220, jbdescr$, part$, qtymake, qtycomp,      ~
                                 actstdate$, dateclosed$,                ~
                                 plstdate$, plenddate$, qtyorig, qtyscrp,~
                                 qtyrewk, control$
L30220:         FMT POS(9), CH(30), POS(58), CH(25), 2*PD(14,4),         ~
                    POS(147), 2*CH(6), XX(9), 2*CH(6), PD(14,4),         ~
                    POS(212), 2*PD(14,4), POS(1120), CH(19)

            if dateclosed$ = " " or dateclosed$ = blankdate$ then L30320
                call "DATEFMT" (dateclosed$)
                errormsg$ = hex(8c) & "This Job Was Closed On"
                errormsg$ = errormsg$ & hex(84) & dateclosed$
                goto error_exit

L30320:     qtyadj = -(qtyorig - qtyscrp - qtyrewk - qtymake)
            call "CONVERT" (qtymake, 0.2, qtymake$)
            call "CONVERT" (qtycomp, 0.2, qtycomp$)
            call "CONVERT" (qtymake - qtycomp, 0.2, qtyleft$)
            call "CONVERT" (qtyorig, 0.2, qtyorig$)
            call "CONVERT" (qtyscrp, 0.2, qtyscrp$)
            call "CONVERT" (qtyrewk, 0.2, qtyrewk$)
            call "CONVERT" (qtyadj , 0.2, qtyadj$)

            call "PIPINDEX" (#60, str(plstdate$,,6) , stdate%, u3%)
            call "PIPINDEX" (#60, str(plenddate$,,6), enddate%, u3%)
            call "DATEFMT" (actstdate$)
            goto L30630

L30460
*        PIPIN File Load if the PIPIN is still an advice
            init(" ") jbdescr$, control$
            call "READ100" (#33, tag$, f1%(33))
                if f1%(33) <> 0% then L30520
                errormsg$ = "Cannot find the PIPIN file record"
                goto error_exit
L30520:     get #33 using L30530, part$, enddate%, qtyorig, stdate%
L30530:         FMT CH(25), BI(4), POS(49), PD(14,4), BI(4)

            call "CONVERT" (qtyorig, 0.2, qtyorig$)
            qtymake$ = qtyorig$
            call "DATE" addr("G+", pldate$, enddate% -1%, plenddate$,err%)
            call "DATE" addr("G+", pldate$, stdate% -1%, plstdate$, err%)

            jobtag$ = tag$  /*JOBTAG$ is clean job number or advice tag*/

*        Common formatting, etc.
L30630:     call "DESCRIBE" (#4, part$, partdescr$, 0%, f1%(4))
            if partdescr$ = jbdescr$ then partdescr$ = " "
            plenddateu$ =  plenddate$
            plstdateu$ = plstdate$
            call "DATEFMT" (plstdate$)
            call "DATEFMT" (plenddate$)


            rschdmax%, cntr2% = 0% : init(" ") rtestep$(), travel$()

            if str(tag$,,11) <> "JOB ORDER: " then L30780
            call "PLNTRVLR" (str(tag$,,19), #7, #23, #8, #15, #24,       ~
                             #60, #9, " ", " ", rtestep$(), rschdmax%,   ~
                             travel$(), cntr2%)
                goto L30860
L30780:     call "PLNTRVLR" (str(tag$,,19), #7, #23, #8, #15, #24,       ~
                             #60, #33, " ", " ", rtestep$(), rschdmax%,  ~
                             travel$(), cntr2%)

L30860:     mat ed% = zer
            if f2%(10) = 0% then call "DELETE" (#10, hex(00), 0%)
            if cntr2% < 1% then return
            if f2%(10) <> 1% then L30910
                call "WORKOPEN" (#10, "IO", 100%, f2%(10))
L30910:     for temp% = 1% to cntr2%
                get travel$(temp%), using L30930,temp2$,step%,tmpdate$
L30930:         FMT CH(7), BI(2), XX(34), CH(6)
                call "PIPINDEX" (#60, tmpdate$, index%, 0%)
                ed%(step%) = max(ed%(step%), index%)
                      plowkey$ = str(rtestep$(step%),98,30)
                      tmpdate$ = str(rtestep$(step%),94,4)
                      if tmpdate$ <> " " then call "PUTPAREN" (tmpdate$)
                      plowkey$ = plowkey$ & " " & tmpdate$
                      write #10, using L31040, temp2$, step%, ")",        ~
                                 plowkey$, step%, eod goto L31050
L31040:               FMT CH(9), PIC(###), CH(3), CH(37), BI(2)
L31050:     next temp%

            return

        REM *************************************************************~
            *              S L I D E   P R O C E S S I N G              *~
            *-----------------------------------------------------------*~
            *             Starts with Date Checking                     *~
            *  First we do the Date Checking and bail out if we find    *~
            *  activity that will fall outside the calendar.  As a      *~
            *  result of data entry testing of the new PIPIN due date   *~
            *  we know it's OK, but we need to check the PIPIN start    *~
            *  date (unless it is already released) and then search     *~
            *  the PIPOUT and WCOUT files for details that, when offset *~
            *  end up hanging off either end of the calendar.  If we    *~
            *  find any, we abort.                                      *~
            *************************************************************

        pre_slide_date_check
            call "SHOSTAT" ("Checking Dates")
            if str(tag$,1,2) = "JO" then check_earliest_wcout
            if mode$ = "2" then check_earliest_wcout

*        Check the PIPIN start date
            call "READ100" (#33, tag$, f1%(33))
                if f1%(33) <> 0% then  L31340
                errormsg$ = "Cannot read the record from the PIPIN file"
                goto error_exit

L31340:     get #33, using L31350, date%
L31350:         FMT POS(57), BI(4)
            if date% + offset% > 0% then check_earliest_wcout
                errormsg$ = "The PIPIN would have to start prior to the" ~
                           & " first date of the planning calendar."
                goto error_exit

        check_earliest_wcout
            init(hex(00)) readkey$
            str(readkey$,,19) = str(tag$)
L31440:     call "PLOWNEXT" (#23, readkey$, 19%, f1%(23))
                if f1%(23) = 0% then check_earliest_pipout
            get #23, using L31470, workcenter$, date%, step$, phantom%
L31470:         FMT CH(4), BI(2), POS(40), CH(4), BI(1)

            if mode$ <> "1" then L31570

*        First hit is earliest and that's all we need if whole job slides
            if date% + offset% > 0% then check_earliest_pipout
                errormsg$ = "A WCOUT for workcenter " & workcenter$ &    ~
                " would have to occur prior to calendar start date."
                goto error_exit

*        When a step is fixed, we've already checked the start date
*        so we dont have to worry about WCOUTs that are too early
*        we just find the date of the fixed step

L31570:    convert phantom% to str(step$,6,4), pic(####)
           str(step$,5,1) = "-"
           if step$ < fixedstep$ then L31440  /* keep trying */
           fixeddate% = date%     /* bingo, here is the magic date */

        check_earliest_pipout
            if mode$ <> "1" then slide_the_sucker

            init(hex(00)) readkey$
            str(readkey$,,19) = str(tag$)
            hits% = 0%
L31660:     call "PLOWNEXT" (#34, readkey$, 19%, f1%(34))
                if f1%(34) = 0% then L31780
            hits% = hits% + 1%
            get #34, using L31700, comp$, date%
L31700:         FMT POS(20), CH(25), POS(45), BI(4)
            if hits% = 1% then earliest% = date%
            old% = earliest%
            earliest% = min(earliest%, date%)

            if old% <> earliest% then component$ = comp$
            goto L31660

L31780:     if earliest% + offset% > 0% then slide_the_sucker
                errormsg$ = "A PIPOUT for part " & component$ &          ~
                " would have to occur prior to the calendar start date."
                goto error_exit


        REM *************************************************************~
            *         M A I N   P R O C E S S I N G   S E C T I O N     *~
            *  This is where we offset the PIPIN and all its details.   *~
            *  Since we know that the maximum dates are all within the  *~
            *  calendar, all we do is march through the detail files    *~
            *  changing dates by the amount of the offset and           *~
            *  rebalancing the master files as necessary.  Only areas   *~
            *  are that PIPOUT and WCOUT must be processed in arrays    *~
            *  else we would re-process the same records over and over  *~
            *  if we did it in place in the files.  Other weirdness     *~
            *  is if we are sliding from a fixed step, only the PIPOUTs *~
            *  and WCOUTs after that date get to slide.                 *~
            *************************************************************

        slide_the_sucker
            call "SHOSTAT" ("Sliding the PIPIN")

*        Start with the PIPIN file
            call "READ101" (#33, tag$, f1%(33))
                if f1%(33) <> 0% then L32020
                errormsg$ = "Cannot obtain the PIPIN record."
                goto error_exit

L32020:     get #33, using L32030, part$, olddue%, qty, start%
L32030:         FMT CH(25), BI(4), POS(49), PD(14,4), BI(4)

            newstart% = start% + offset%
            call "PIPINDEX" (#60, newduedateu$, newdue%, u3%)
            newtag$ = tag$
            if mode$ = "2" then newstart% = start% /* doesn't change */

            if str(tag$,,11) = "JOB ORDER: " then L32240  /*no key change*/
            if mode$ = "2" then L32240                    /* ditto       */

            call "DATE" addr("G+",pldate$, newstart% -1%, newstartu$,err%)

*        Create the new tag number and test that it isn't already used
            convert newstart% to str(newtag$,3,3), pic(###)
L32140:     call "READ100" (#33, newtag$, f1%(33))
              if f1%(33) =  0% then L32190
              newtag$ =  str(newtag$,,5) & tagdate$ & time
              goto L32140

L32190:     call "READ101" (#33, tag$, f1%(33))
            delete #33


*        And we put the data right back out there
L32240:     put #33, using L32250, newdue%, newtag$, newstart%
L32250:         FMT POS(26), BI(4), CH(19), POS(57), BI(4)
            if newtag$ = tag$ then rewrite #33 else write #33

            if olddue% = newdue% then L32300

            call "PIPFLAGS" (part$,  today%, olddue%, -qty, #2, #41)
            call "PIPFLAGS" (part$,  today%, newdue%, qty, #2, #41)

L32300
*        Now do the Job Master File if the PIPIN is a released job

            if str(tag$,1,2) = "WO" then offset_pipouts
            if str(tag$,1,2) = "BW" then offset_pipouts

            call "READ101"(#9, str(jobtag$,,8), f1%(9%))
                if f1%(9%) <> 0% then L32380
                errormsg$ = "Job Master Record Not Found"
                goto error_exit

L32380:     if mode$ = "2" then L32443

            get #9, using L32390,  oldstartdate$
L32390:         FMT POS(168), CH(6)
            call "DATE" addr("G+",oldstartdate$,offset%, newstartu$, err%)
            put #9, using L32440,  newstartu$, newduedateu$
L32440:         FMT  POS(168), CH(6), CH(6)
            goto  L32450

L32443:     put #9, using L32444, newduedateu$
L32444:         FMT  POS(174), CH(6)

L32450:     rewrite #9

        offset_pipouts
            call "SHOSTAT" ("Sliding the PIPOUTs")
*        First we build an array of all the PIPOUTs
            init(hex(00)) readkey$
            str(readkey$,1,19) = str(tag$,1,19)
            max% = 0%
L32530:     call "PLOWNEXT" (#34, readkey$,  19%, f1%(34))
               if f1%(34) = 0% then process_pipout_array
            max% = max% + 1%
            get #34, using L32570, record$(max%)
L32570:         FMT CH(64)
            goto L32530

        process_pipout_array
            if max% = 0% then offset_wcouts
            for i% = 1% to max%
                newdate% = 0%
                get str(record$(i%)) using L32660,                        ~
                                    tag$, part$, olddate%, time$,  qty
L32660:            FMT CH(19), CH(25), BI(4), CH(8), PD(14,4)

*        If only slide part of job, we only offset those after fixed date
                if mode$ <> "2" then L32720
                   if olddate% <= fixeddate% then L32820 /* skip em */

L32720:         str(deletekey$,,56) = str(record$(i%),,56)
                call "DELETE"(#34, deletekey$, 56%)
                call "PIPFLAGS"(part$, today%, olddate%, qty, #2, #41)

                newdate% = olddate% + offset%

                put #34, using L32790, newtag$, part$, newdate%,time$,qty
L32790:            FMT CH(19),  CH(25), BI(4), CH(8), PD(14,4)
                write #34
                call "PIPFLAGS"(part$, today%, newdate%, -qty, #2, #41)
L32820:     next i%

        offset_wcouts
            call "SHOSTAT" ("Sliding the WCOUTs")
*        Build an array of all the WCOUTs
            init(" ") record$()
            init(hex(00)) readkey$
            str(readkey$,1,19) = str(tag$,1,19)
            max% = 0%
L32910:     call "PLOWNEXT" (#23, readkey$,  19%, f1%(23))
               if f1%(23) = 0% then process_wc_array
            max% = max% + 1%
            get #23, using L32950, record$(max%)
L32950:         FMT CH(68)
            goto L32910

        process_wc_array
            if max% = 0% then update_jbpipxrf

*        Pass the array, process all WCOUTs for each Workcenter as we
*        encounter the first WCOUT for it, keeping usage adjustments.
*        Re-write each WCOUT and update the WCMASTR after the last WCOUT
*        for the particular Workcenter.  Also, we skip any WCOUTs that
*        occur prior to the fixed step if mode = 2

            newdate% = 0%
            call "SORT" addr(record$(), max%, 68%)
            wc$() = " "
            if offset% = 0% then update_jbpipxrf
            if offset% > 0% then L33085
               wcst% = 1% : wcend% = max% : wcstep% = 1%
               goto L33090
L33085:     wcst% = max% : wcend% = 1% : wcstep% = -1%

L33090:     for i% = wcst% to wcend% step wcstep%
                if str(record$(i%),5,2) = " " then L33560  /* next I% */
                get str(record$(i%)), using L33130,                       ~
                                     wc$(i%), date%, setup%, run%, step$
L33130:            FMT CH(4), BI(2), POS(32), BI(4), BI(4), CH(4)
                mat adj%  = zer
                if mode$ <> "2" then L33150
                   if step$ <= fixedstep$ then L33206 /* leave it alone */
L33150:         newdate% = date% + offset%
                adj%(date%) = adj%(date%) - setup% - run%
                adj%(newdate%) = adj%(newdate%) + setup% + run%

                call "READ101"(#23, str(record$(i%),9,23), f1%(23))
                put #23, using L33203, newdate%, newtag$, newdate%
L33203:               FMT POS(5), BI(2), POS(9), CH(19), BI(2)
                delete #23
                write #23
L33206:         str(record$(i%),5,2) = " " /* so don't process again */

                   if i% = wcend% then L33390
                   for j% = i% + wcstep% to wcend% step wcstep%
                      if str(record$(j%),,4) <> str(wc$(i%),,4)          ~
                                            then L33380    /* next J% * */
                      get str(record$(j%)), using L33130,                 ~
                                     wc$(j%), date%, setup%, run%, step$
                      if mode$ <> "2" then L33333
                         if step$ <= fixedstep$ then L33370
L33333:               newdate% = date% + offset%
                      adj%(date%) = adj%(date%) - setup% - run%
                      adj%(newdate%) = adj%(newdate%) + setup% + run%
                      call "READ101"(#23, str(record$(j%),9,23), f1%(23))
                      put #23, using L33353, newdate%, newtag$, newdate%
L33353:               FMT POS(5), BI(2), POS(9), CH(19), BI(2)
                      delete #23
                      write #23
L33370:               str(record$(j%),5,2) = " "
L33380:            next j%

L33390:         if wc$(i%) = "VEND" then L33560

                call "READ101" (#11, wc$(i%), f1%(11))
                get #11, using L33470, used%()
                mat used% = used% + adj%
                put #11, using L33470, used%()
L33470:            FMT POS(1040), 490*BI(2)
                rewrite #11

L33560:     next i%

*        Adjust Vendor Service Advices...
            call "JBVSASUB" (#60, #23, "RES", job$, part$, qtymake)

        update_jbpipxrf
            call "SHOSTAT" ("Adjusting Pegging Files")
            call "REDALT1" (#36, tag$, 1%, f1%(36))
                if f1%(36) = 0% then L33790
            delete #36
            put #36, using L33760, newtag$
L33760:         FMT POS(45), CH(19)
            write #36, eod goto L33790

L33790:     init(hex(00)) readkey$
            str(readkey$,1,19) = str(tag$,1,19)

L33820:     call "PLOWNXT1" (#36, readkey$, 19%, f1%(36))
                if f1%(36) = 0% then update_jbcross2
            delete #36
            put #36, using L33860, newtag$
L33860:         FMT POS(1), CH(19)
            write #36, eod goto L33820
            goto L33820

        update_jbcross2
            call "READ101" (#8, tag$, f1%(8))
                if f1%(8) = 0% then update_pipcross
            delete #8
            put #8, using L33960, newtag$
            put #8, using L33970, newtag$
L33960:         FMT POS(29), CH(19)
L33970:         FMT POS(76), CH(19)
            write #8, eod goto L34000

L34000: update_pipcross
            if str(newtag$,,19) = str(tag$,,19) then L65000 /* NADA REQD */
            init (hex(00)) readkey$
            str(readkey$,1,19) = str(tag$,1,19)

L34040:     call "PLOWAL1" (#35, readkey$, 1%, 19%, f1%)
                 if f1% = 0 then L34120
            delete #35
            put #35, using L34080, newtag$
L34080:         FMT POS(20), CH(19)
            write #35, eod goto L34040
            goto L34040

L34120:     init (hex(00)) readkey$
            str(readkey$,1,19) = str(tag$,1,19)

L34150:     call "PLOWAL1" (#35, readkey$, 2%, 19%, f1%)
                if f1% = 0 then L65000   /* that's all folks */
            delete #35
            put #35, using L34190, newtag$
L34190:         FMT POS(39), CH(19)
            write #35
            goto L34150


        error_exit
            call "ASKUSER" (2%, "* * * ERROR * * *",                     ~
                       errormsg$ ,                                       ~
                      "  ", "Press any PF key to Acknowledge and Abort")
            return clear all
            errormsg$ = " "
            goto editpg1



        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40095,         /* Slide Mode         */  ~
                                L40090,         /* New Due Date/Offset*/  ~
                                L40090          /* Fixed Step         */
              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40095:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "Slide Jobs or WO Advices",                            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Job or Advice:",                             ~
               at (06,17), fac(hex(84)), jobtag$                , ch(19),~
               at (06,40), "Descr:",                                     ~
               at (06,48), fac(hex(84)), jbdescr$               , ch(30),~
               at (07,02), "Part Number:",                               ~
               at (07,17), fac(hex(84)), part$                  , ch(25),~
               at (07,40), "Descr:",                                     ~
               at (07,48), fac(hex(84)), partdescr$             , ch(30),~
               at (08,02), "Planned Start:",                             ~
               at (08,17), fac(hex(84)), plstdate$              , ch(08),~
               at (08,27), "Planned End:" ,                              ~
               at (08,40), fac(hex(84)), plenddate$             , ch(08),~
               at (08,50), "Quantity:",                                  ~
               at (08,60), fac(hex(84)), qtymake$               , ch(10),~
               at (09,02), "Control No.:",                               ~
               at (09,17), fac(hex(84)), control$               , ch(19),~
                                                                         ~
               at (12,02), "Slide Mode",                                 ~
               at (12,30), fac(lfac$( 1)), mode$                , ch(01),~
               at (13,02), "New Job Due Date",                           ~
               at (13,30), fac(lfac$( 2)), newduedatef$         , ch(08),~
                                                                         ~
               at (14,02), "  or Number of Days (+/-)",                  ~
               at (14,30), fac(lfac$( 2)), offset$              , ch(04),~
                                                                         ~
               at (15,02), "Fixed Step",                                 ~
               at (15,30), fac(lfac$( 3)), fixedstep$           , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40260
                  call "MANUAL" ("PIPSLIDE") : goto L40105

L40260:        if keyhit% <> 15 then L40275
                  call "PRNTSCRN" : goto L40105

L40275:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40370     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40350
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40350:     if fieldnr% > 1% then L40360
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40360:     return

L40370: if fieldnr% > 0% then L40415  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Slide PIPIN "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40415:                              /*  Edit Mode - Enabled    */
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
            forcestep% = 0%
            on fieldnr% gosub L50100,         /* Slide Mode             */~
                              L50200,         /* New Due Date or Offset */~
                              L50400          /* Fixed Step             */
            return
L50100: REM Test for Slide Mode                   MODE$
            if edit% = 1% then L50145
            if mode$ = "1" or mode$ = "2" then L50145
                errormsg$ = "Enter either a '1' or a '2'"
                return
L50145:     if edit% = 0% then return
            if mode$ = "2" then L50165
            fixedstep$ = " " : fixedstep% = 0%
                return
L50165:     forcestep% = 1%
                return

L50200: REM Test for New Job Due Date             NEWDUEDATEF$ OFFSET$
            if newduedatef$ = " " or newduedatef$ = blankdate$ then L50320

*        The date is non-blank so test the date and set offset to match
            call "DATEOK"(newduedatef$, newduedate%, errormsg$)
            if errormsg$ <> " " then return
            newduedate% = newduedate%
            newduedateu$ = newduedatef$
            call "DATUNFMT" (newduedateu$)

            call "PIPINDEX" (#60, newduedateu$, newdue%, ret%)
              if ret% = 0% then L50306
              errormsg$ = "New due date would be outside the calendar"
              return

L50306:     if newduedateu$ >= plstdateu$ then L50310
              errormsg$ = "New due date would be before start date."
              return

L50310:     call "DATE" addr("G-",plenddateu$,newduedateu$, offset%, err%)
            convert  offset% to offset$, pic(-###)
            return

*        The date is blank, so test the offset and set date to match
L50320:     if offset$ = " " then L50385
            convert offset$ to offset%, data goto L50360
            if offset% > 490% or offset% < -490% then L50360

            call "DATE" addr("G+", plenddateu$,offset%,newduedateu$, err%)

            call "PIPINDEX" (#60, newduedateu$, newdue%, ret%)
              if ret% = 0 then L50348
              errormsg$ = "New due date would be outside the calendar"
              return
L50348:     if newduedateu$ >= plstdateu$ then L50351
              errormsg$ = "New due date would be before start date."
              return
L50351:     newduedatef$ = newduedateu$
            call "DATEFMT"(newduedatef$)
            return

L50360:     errormsg$ = "Enter a number of days between -490 an +490"
            return

L50385:     errormsg$ = "Both Fields Cannot be Blank, Enter a date "  &  ~
                        "or a Number of Days +/-"
            return

L50400: REM Test for Fixed Step                   FIXEDSTEP$
            fixedstep% = 0%
            if mode$ <> "1" then L50410
                fixedstep$ = " "
                return

L50410:     if cntr2% < 1% then L50500   /* no steps to see */
            call "GETCODE" (#10, fixedstep$, " ", 0%, 1.43, f1%(10))
                if f1%(10) <> 0% then L50425
                errormsg$ = "Invalid entry for step - please enter or "  ~
                  & "select a step."
                return
L50425:     get #10, using L50430, fixedstep%
L50430:         FMT POS(53), BI(2)
            if fixedstep% > 0% then L50450
                fixedday% = 0%
                goto L50455
L50450:     fixedday% = ed%(fixedstep%)
L50455:     fixedstart% = (max(fixedday%,today%) - 1%)
            fixedstart% = fixedstart%
            return

L50500:     k% = 2%
            call "ASKUSER" (k%, "* * * W A R N I N G * * *",             ~
                       "There are no WCOUTS to slide for this PIPIN",    ~
                       "Press PF 16 to set MODE = 1 and continue,",      ~
                       "or press any other key to Acknowledge and exit.")

            if k% <> 16% then exit_program
            mode$ = "1"
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
*          CALL "SHOSTAT" ("One Moment Please")
            if f2%(10) = 0% then close #10

            end
