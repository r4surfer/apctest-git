        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC   EEEEE  DDDD   TTTTT   *~
            *  H   H  NN  N  Y   Y  C   C  C   C  E      D   D    T     *~
            *  HHHHH  N N N   YYY   C      C      EEEE   D   D    T     *~
            *  H   H  N  NN    Y    C   C  C   C  E      D   D    T     *~
            *  H   H  N   N    Y     CCC    CCC   EEEEE  DDDD     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCEDT - This program provides the means to edit the    *~
            *            record values in the HNYCCMST File or to add/  *~
            *            delete individual records. Historical data in  *~
            *            the record will be displayed but not editable. *~
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
            * 02/07/92 ! Original                                 ! RJH *~
            * 09/28/92 ! Fix Record Date formate problem.         ! RJH *~
            * 05/04/93 ! Added Count Factor Field(Pieces per Hour)! RJH *~
            * 08/07/96 ! Changes for the year 2000.               ! DXL *~
            * 07/23/97 ! Bug fix for askuser arg list on delete.  ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim      /*  ** Program Variables ** */                          ~
            afac$(14)1,                  /* Field Attribute Characters */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            deletekey$99,                /* Misc. Delete Key           */~
            descr$79,                    /* Plow Code Description      */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            incl_excl$(1)79,             /* Plow code Include exclude  */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            msg$45,                      /* Plow Code Message          */~
            pc_header$(3)79,             /* Plow Code Header array     */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            temp$99,                     /* Temp Variable              */~
            userid$3                     /* Current User Id            */

            dim     /* ** Cycle Count Variables ** */                    ~
            abcclass$1,                  /* ABC Class                  */~
            abcold$1,                    /* Old ABC Class              */~
            abcorig$1,                   /* On Record ABC Class        */~
            abclastdate$10,              /* Date ABC last changed      */~
            abclastuser$3,               /* Last user to change ABC    */~
            abclockflag$1,               /* ABC Lock Flag              */~
            actflag$1,                   /* Active Sess'n Flag(P,A,C,b)*/~
            ccgroup$6,                   /* Cycle Count Group Code     */~
            cntlastuser$3,               /* Last User to Change CC Prd */~
            cntnbr$12,                   /* Number of Count Sessions   */~
            cntperiod$3,                 /* Count Period               */~
            cntperiodflag$3,             /* Count Period Lock Flag     */~
            cntperiodold$3,              /* Count Period Last Value    */~
            cntperiodorig$3,             /* Count Period On Record     */~
            cntprdlstdat$10,             /* Date Count Period Started  */~
            cnttlernper$12,              /* Count Tolerance in Percent */~
            cnttlernqty$12,              /* Count Tolerance Quantity   */~
            count_factor$10,             /* Count Rate per Part        */~
            cumbohqty$12,                /* Cumulative BOH Quantity    */~
            cumcntdltam$12,              /* Cumulative Qnty. Delta (-) */~
            cumcntdltap$12,              /* Cumulative Qnty. Delta (+) */~
            cumtlernhit$12,              /* Cumulative Tolerance Hits  */~
            datecnt$10,                  /* Last Count Date            */~
            lot$6,                       /* Lot Number of Part         */~
            lot_msg$30,                  /* Lot Number of Message      */~
            nextcntdate$10,              /* Next Count Date            */~
            oldtransthresftr$12,         /* Old Trans. Threshold Factr */~
            part$25,                     /* Part Number                */~
            partdesc$32,                 /* Part Description (accept)  */~
            partdescr$32,                /* Part Description (plow)    */~
            recdate$10,                  /* Date Record was created    */~
            scrnmsg1$35,                 /* Accept Screen Message      */~
            scrnmsg2$35,                 /* Accept Screen Message      */~
            scrnmsg3$35,                 /* Accept Screen Message      */~
            sesstatus$30,                /* Session Status             */~
            store$3,                     /* Store/warehouse            */~
            strdescr$30,                 /* Store Description (plow)   */~
            storedesc$30,                /* Store Description (accept) */~
            tempdate$8,                  /* Temporary Date             */~
            tdate$8,                     /* Temporary Date             */~
            transfreqfact$12,            /* Transaction Frequence Factr*/~
            transfreqold$12,             /* Old Transaction Freq. Factr*/~
            transfreqorig$12,            /* On Record Trans.Freq. Factr*/~
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
            rslt$(64)20,                 /* Text from file opening     */~
            descr_map(6),                /* Plow Code Description Map  */~
            incl_excl(1),                /* Plow Code Include/exclude  */~
            hdr$(3)79,                   /* PLOWCODE Headers           */~
            incl$(1)1,                   /* PLOWCODE Include/Exclude   */~
            incl(1)                      /* PLOWCODE Include/Exclude   */

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
            * #01 ! HNYMASTR ! Inventory Part Master File               *~
            * #02 ! HNYQUAN  ! Inventory Quantity File                  *~
            * #03 ! HNYCCMST ! Cycle Count Master File                  *~
            * #04 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #05 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #07 ! STORNAME ! Store Master File                        *~
            * #08 ! SYSFILE2 ! System File                              *~
            * #49 ! DUMMY    ! Dummy File for Plowcode                  *~
            *                                                           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos = 90,  keylen = 4, dup

            select #02, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

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
                        keypos =    1,  keylen =  6                      ~

            select #07, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1,  keylen =  3                      ~

            select #08, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1,  keylen =  20                     ~

            select #49, "DUMMY"   ,                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1,  keylen =  44                     ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            rec% = val(str(rslt$(02),17,4),4) / 2%
            call "OPENCHCK" (#03, fs%(03), f2%(03), rec%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            tempdate$, tdate$ = date$
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            ll% = 6%      /* Default Lot Length */
            str(line2$,62) = "HNYCCEDT: " & str(cms2v$,,8)

        REM Plowcode Screen Mapping...
            hdr$(1%)              = "  PART"
            str(hdr$(1%),30%,5%)  = "STORE"
            str(hdr$(1%),37%,3%)  = "LOT"
                descr_map(1%) =  1.250  :  descr_map(2%) =  1.0
                descr_map(3%) = 26.030  :  descr_map(4%) = 28.0
                descr_map(5%) = 29.060  :  descr_map(6%) = 35.0

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            n% = 1% :  mode% = 1%
L10100:     for fieldnr% = n% to 12%
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
                      if keyhit% =  8% and fieldnr% = 1%                 ~
                            then gosub get_cc_record  else L10220
                               if plowkey$ <= " " then L10110/*No selctn*/~
                                                  else L10250/* Got one */
L10220:               if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " "  or part$ = " "  then L10130
L10250:     next fieldnr%

            if f1%(03) = 1% then L11060  /* Existing Record */

*        Perform Input for Variable Fields
            call "VFINPSUB" ("HNYCCMST", "I", "Manage Cycle Count Recor" ~
                  & "ds", str(line2$,,60), "NN", variable$, keyhit%)
            if keyhit% = 1% then inputmode

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

L11060: editpg1
            lastfieldnr% = 0%  :  mode% = 2%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  9% then gosub edit_user_defined_fields
                  if keyhit%  = 12% then       delete_part_record
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
            if cursor%(1%) > 6% then L11114
              fieldnr% = cursor%(1%) - 3%  : goto L11130
L11114:     if cursor%(1%)  = 7% then L11120
            if cursor%(1%)  > 17 then L11120
              fieldnr% = cursor%(1%) - 4% :  goto L11130
L11120:     fieldnr% = 0%
L11130:     if fieldnr% < 3% or fieldnr% > 13% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  if fieldnr% = 3% and f1%(03) = 0% then L11214 else L11220
L11214:           n% = 4% :  goto L10100     /* Back to Input Mode */
L11220:           lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20095,         /* Part Number            */~
                              L20120,         /* Store                  */~
                              L20145,         /* Lot Number             */~
                              L20190,         /* Cycle Count Group      */~
                              L20205,         /* Record Start Date      */~
                              L20240,         /* Count Tolerance %      */~
                              L20255,         /* Count Tolerance Q      */~
                              L20270,         /* ABC Class              */~
                              L20295,         /* Transaction Freq.      */~
                              L20320,         /* Count Cycle Period     */~
                              L20350,         /* Next Count Date        */~
                              L20400          /* Count Rate             */

            return
L20095: REM Def/Enable Part Number                 PART$
            inpmessage$ = "Enter Part Number (enter partial or blank to"&~
                          " see parts on file)."
            return

L20120: REM Def/Enable Store                       STORE$
            inpmessage$ = "Enter Store Code (enter partial or blank to" &~
                          " see stores on file)."
            return

L20145: REM Def/Enable Lot Number                  LOT$
            call "LOTENABL" (part$, le%, ll%, #08, #01)
            if le% =  0% then lot_msg$ = "Part does not use lot numbers."
            if le% =  1% then lot_msg$ = "Lot number not required."
            if le% =  2% then lot_msg$ = "Part requires lot numbers."
            if le% <> 0% then return
                enabled% = 0%
                plowkey$ = str(part$) & str(store$)
                call "PLOWNEXT" (#2, plowkey$, 28%, enabled%)

            return

L20190: REM Def/Enable Cycle Count Group Name      CCGROUP$
            return

L20205: REM Def/Enable Record Start Date           RECDATE$
            if recdate$ = " " or recdate$ = blankdate$ then recdate$ = tdate$
            call "DATEOK" ( recdate$, 0%, " " )
            return

L20240: REM Def/Enable Count Tolerance(percent)    CNTTLERNPER$
            return

L20255: REM Def/Enable Count Tolerance(actual)     CNTTLERNQTY$
            return

L20270: REM Def/Enable ABC Class                   ABCCLASS$
            if abclockflag$ = " " then                                   ~
                 abclockflag$ = sysautochnglockflag$
            if abcclass$ = " " then abcclass$ = abcorig$
            return

L20295: REM Def/Enable Transaction Freq. Factor    TRANSFREQFACT$
            if translockflag$ = " " then                                 ~
                 translockflag$ = sysautochnglockflag$
            return

L20320: REM Def/Enable Count Cycle Period(days)    CNTPERIOD$
            if cntperiodflag$ = " " then                                 ~
                 cntperiodflag$ = sysautochnglockflag$
            return

L20350: REM Def/Enable Next Count Date         NEXTCNTDATE$
            if cntperiod$ <> " " then L20360
                nextcntdate$ = " " :  enabled% = 0%  :  return
L20360:     if nextcntdate$ = " " or nextcntdate$ = blankdate$ ~
                                  then  gosub check_next_date

            return

L20400: REM Def/Enable Count Rate              COUNT_FACTOR$



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
         "Enter Part Number                                            ",~
         "Enter Store/Warehouse Name                                   ",~
         "Enter Lot Number                                             ",~
         "Enter Cycle Count Group Name                                 ",~
         "Enter Record Start Date                                      ",~
         "Enter Count Tolerance(percent)                               ",~
         "Enter Count Tolerance(actual)                                ",~
         "Enter ABC Class (A, B, C, D, or X) Then Auto Lock Flag       ",~
         "Enter Transaction Freq. Factor  Then Auto Lock Flag          ",~
         "Enter Count Cycle Period (days)  Then Auto Lock Flag         ",~
         "Enter Next Count Date                                        ",~
         "Enter Count Rate (Number of Pieces per Person-hour)          ",~
         "                                                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, store$, part$, lot$,       ~
                      lot_msg$, storedesc$, partdesc$, count_factor$
            call "ALLFREE"
            gosub  init_default_vars

            scrnmsg1$ = "  Old Data (not editable)        "
            scrnmsg2$ = "Date Changed"
            scrnmsg3$ = "Changed By"

*         *** TEMP TEST LINE ***
            admin% = 1%
            sysautochnglockflag$ = "N"     /* 'Y' = Protect Field from */
*         *** END TEMP ***                /* Automatic Update Unless  */
                                           /* Field is blank.          */
            return

        init_default_vars
            init(" ")                ccgroup$, datecnt$, cnttlernper$,   ~
                cnttlernqty$, cntnbr$, actflag$, nextcntdate$, recdate$, ~
                cumtlernhit$, cumcntdltap$, cumcntdltam$, cumbohqty$,    ~
                abclockflag$,abcclass$,abcold$,abclastuser$,abclastdate$,~
              translockflag$,transfreqfact$,transfreqold$,transthresftr$,~
                oldtransthresftr$, translastuser$, translastdate$,       ~
                cntperiodflag$, cntperiod$,cntperiodold$,cntlastuser$,   ~
                cntprdlstdat$, variable$, cntperiodorig$,abcorig$,       ~
                transfreqorig$, sesstatus$, count_factor$
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            call "READ100" (#03, plowkey$, f1%(03))
            if f1%(3) = 0% then return

            get #03 using     L35030,                                     ~
            part$,          /* Part Number (Product Code)              */~
            store$,         /* Warehouse or Store                      */~
            lot$,           /* Which lot in inventory - always used wit*/~
            ccgroup$,       /* Cycle Count Group Code Name             */~
            datecnt$,       /* Date Last Counted                       */~
            cnttlernper,    /* Count Tolerance Percentage              */~
            cnttlernqty,    /* Count Tolerance Quantity                */~
            cntnbr,         /* Number of Counts                        */~
            actflag$,       /* Active Session Flag                     */~
            nextcntdate$,   /* Next Count Date                         */~
            recdate$,       /* Record Start Date                       */~
            cumtlernhit,    /* Cumulative Tolerance Hits               */~
            cumcntdltap,    /* Cumulative Count Delta (+)              */~
            cumcntdltam,    /* Cumulative Count Delta (-)              */~
            cumbohqty,      /* Cumulative BOH Quantity                 */~
            abclockflag$,   /* Lock Flag for ABC Class                 */~
            abcclass$,      /* ABC Class                               */~
            abcold$,        /* Old ABC Class                           */~
            abclastuser$,   /* User Who Last Changed ABC Class         */~
            abclastdate$,   /* Date ABC Class Last Changed             */~
            translockflag$, /* Lock Flag for Transaction Factor        */~
            transfreqfact,  /* Transaction Frequency Factor            */~
            transfreqold,   /* Old Transaction Frequency Factor        */~
            transthresftr,  /* Transaction Threshold Factor            */~
            oldtransthresftr,/*Old Transaction Threshold Factor        */~
            translastuser$, /* User Last Changed Transaction Frequency */~
            translastdate$, /* Date Transaction Frequency Factor Last C*/~
            cntperiodflag$, /* Lock Flag - Count Period                */~
            cntperiod$,     /* Count Period in (Days)                  */~
            cntperiodold$,  /* Old Count Period                        */~
            cntlastuser$,   /* User Who Last Changed Count Period      */~
            cntprdlstdat$,  /* Date Count Period Last Changed          */~
            firstcntdate$,  /* Date First Count was Made               */~
            variable$,      /* Variable Field                          */~
            count_factor    /* Count Rate                              */

            call "CONVERT" (cnttlernper     , -2.2, cnttlernper$)
            call "CONVERT" (cnttlernqty     , -2.2, cnttlernqty$)
            call "CONVERT" (cntnbr          , -2.2, cntnbr$)
            call "CONVERT" (cumtlernhit     , -2.2, cumtlernhit$)
            call "CONVERT" (cumcntdltap     , -2.2, cumcntdltap$)
            call "CONVERT" (cumcntdltam     , -2.2, cumcntdltam$)
            call "CONVERT" (cumbohqty       , -2.2, cumbohqty$)
            call "CONVERT" (transfreqfact   , -2.2, transfreqfact$)
            call "CONVERT" (transfreqold    , -2.2, transfreqold$)
            call "CONVERT" (transthresftr   , -2.2, transthresftr$)
            call "CONVERT" (oldtransthresftr, -2.2, oldtransthresftr$)
            call "CONVERT" (count_factor    , -2.2, count_factor$)

            gosub format_dates
            gosub set_session_status

            abcorig$ = abcclass$  :  cntperiodorig$ = cntperiod$
            transfreqorig$ = transfreqfact$
            return

        format_dates
            call "DATEFMT" (datecnt$)
            call "DATEFMT" (nextcntdate$)
            call "DATEFMT" (recdate$)
            call "DATEFMT" (abclastdate$)
            call "DATEFMT" (translastdate$)
            call "DATEFMT" (tempdate$)
            call "DATEFMT" (cntprdlstdat$)
            call "DATEFMT" (firstcntdate$)
            return

        set_session_status
            if actflag$ <> "P" then L30490
                sesstatus$ = "Session Status is Pre-Active" :  return
L30490:     if actflag$ <> "C" then L30510
                sesstatus$ = "Session Status is Closed"     :  return
L30510:     if actflag$ <> "A" then L30530
                sesstatus$ = "Session Status is Active"     :  return
L30530:     sesstatus$ = "Session Status is Not Set"
            return

        get_cc_record
            pc_header$(1) = "    Part Number                Store  Lot"
            pc_header$(2) = " "
            mat incl_excl = zer
            descr_map(1) =  1.25 : descr_map(2) =  3
            descr_map(3) = 26.03 : descr_map(4) = 30
            descr_map(5) = 29.06 : descr_map(6) = 37
            plowkey$ = all(hex(00))
            descr$ = hex(06) & "SELECT CYCLE COUNT PART/STORE/LOT"
            call "PLOWCODE" (#03, plowkey$, descr$, 9000%, 0.34, f1%(3), ~
                             pc_header$(), 0.0, 0.0, incl_excl(),        ~
                             incl_excl$(), "d", " ", #49, descr_map() )
            if f1%(3) = 0% then return

            part$ = str(plowkey$,,25)  : store$  = str(plowkey$,26,3)
            lot$ = str(plowkey$,29,6)
            call "DESCRIBE" (#1, part$, partdesc$, 0%, 0%)
            call "DESCRIBE" (#7, store$, storedesc$, 0%, 0%)

            gosub dataload
            if f1%(03) = 0% then init_default_vars else L30660
                return
L30660:     fieldnr% = 12%   /* Exit ForNext Loop */
            return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Saving Data")
            gosub check_changed_values
            gosub convert_values
*          GOSUB CHECK_NEXT_DATE
            gosub unformat_dates

            plowkey$ = str(part$) & str(store$) & lot$
            call "READ101" (#03, plowkey$, f1%(03))
            put #03 using    L35030,                                      ~
            part$,          /* Part Number (Product Code)              */~
            store$,         /* Warehouse or Store                      */~
            lot$,           /* Which lot in inventory - always used wit*/~
            ccgroup$,       /* Cycle Count Group Code Name             */~
            datecnt$,       /* Date Last Counted                       */~
            cnttlernper,    /* Count Tolerance Percentage              */~
            cnttlernqty,    /* Count Tolerance Quantity                */~
            cntnbr,         /* Number of Counts                        */~
            actflag$,       /* Active Session Flag                     */~
            nextcntdate$,   /* Next Count Date                         */~
            recdate$,       /* Record Start Date                       */~
            cumtlernhit,    /* Cumulative Tolerance Hits               */~
            cumcntdltap,    /* Cumulative Count Delta (+)              */~
            cumcntdltam,    /* Cumulative Count Delta (-)              */~
            cumbohqty,      /* Cumulative BOH Quantity                 */~
            abclockflag$,   /* Lock Flag for ABC Class                 */~
            abcclass$,      /* ABC Class                               */~
            abcold$,        /* Old ABC Class                           */~
            abclastuser$,   /* User Who Last Changed ABC Class         */~
            abclastdate$,   /* Date ABC Class Last Changed             */~
            translockflag$, /* Lock Flag for Transaction Factor        */~
            transfreqfact,  /* Transaction Frequency Factor            */~
            transfreqold,   /* Old Transaction Frequency Factor        */~
            transthresftr,  /* Transaction Threshold Factor            */~
            oldtransthresftr,/*Old Transaction Threshold Factor        */~
            translastuser$, /* User Last Changed Transaction Frequency */~
            translastdate$, /* Date Transaction Frequency Factor Last C*/~
            cntperiodflag$, /* Lock Flag - Count Period                */~
            cntperiod$,     /* Count Period in (Days)                  */~
            cntperiodold$,  /* Old Count Period                        */~
            cntlastuser$,   /* User Who Last Changed Count Period      */~
            cntprdlstdat$,  /* Date Count Period Last Changed          */~
            firstcntdate$,  /* Date First Count was Made               */~
            variable$,      /* Variable Field                          */~
            count_factor,   /* Count Rate                              */~
            " "             /* Filler                                  */

            if f1%(03) = 1% then rewrite #03 else write #03

            return


        check_changed_values    /* Check for Changed Values */
            if abcorig$ = " "  or  abcorig$ = abcclass$ then L31590
                abcold$ = abcorig$ : abclastdate$ = date$
                abclastuser$ = userid$
L31590:     if cntperiodorig$ = " "  or  cntperiodorig$ = cntperiod$     ~
                     then L31620
                cntperiodold$ = cntperiodorig$
                cntprdlstdat$ = date$
                firstcntdate$ = datecnt$
                if  cntnbr <> 0 then  cntnbr = 1
                cntlastuser$  = userid$
                cntnbr% = int(cntnbr)
                gosub check_next_date
L31620:     if transfreqorig$ = " "  or  transfreqorig$ = transfreqfact$ ~
                     then L31650
                transfreqold$ = transfreqorig$ : translastdate$ = date$
                translastuser$ = userid$
L31650:     return

        check_next_date  /* Calc. the Most Advanced Next Count Date. */
            gosub unformat_dates
            if cntperiod$ > " "  then L31680
                nextcntdate$ = " "   :   goto L31755
L31680:     convert cntperiod$ to cntperiod : cntperiod% = int(cntperiod)
            if recdate$ < firstcntdate$ then L31694
                days% =  int(cntperiod% / 4)
                call "DATE" addr ("G+", recdate$, days%, nextcntdate$,   ~
                                                                   err%)
                goto L31755
L31694:     /* Calc. furthest out NextCountDate */
            days% = (cntnbr%    ) * cntperiod%
            call "DATE" addr ("G+", firstcntdate$ , days% , nxdate1$,err%)
            call "DATE" addr ("G+", datecnt$  , cntperiod%, nxdate2$,err%)
            if nxdate1$ < nxdate2$ then nextcntdate$ = nxdate2$          ~
                                   else nextcntdate$ = nxdate1$
L31755:     gosub format_dates

            return

        convert_values   /* Convert Strings to Reals and Set Defaults */
            if cnttlernper$ <= " " then   cnttlernper$ = "0"

                convert cnttlernper$      to    cnttlernper
            if cnttlernqty$ <= " " then   cnttlernqty$ = "0"

                convert cnttlernqty$      to    cnttlernqty
            if transfreqfact$ <= " " then   transfreqfact$ = "0"

                convert transfreqfact$    to    transfreqfact
            if cumtlernhit$ <= " " then   cumtlernhit$ = "0"

                convert cumtlernhit$      to    cumtlernhit
            if cumcntdltap$ <= " " then   cumcntdltap$ = "0"

                convert cumcntdltap$      to    cumcntdltap
            if cumcntdltam$ <= " " then   cumcntdltam$ = "0"

                convert cumcntdltam$      to   cumcntdltam
            if cumbohqty$ <= " " then   cumbohqty$ = "0"

                convert cumbohqty$        to   cumbohqty
            if transfreqfact$ <= " " then   transfreqfact$ = "0"

                convert transfreqfact$    to   transfreqfact
            if transfreqold$ <= " " then   transfreqold$ = "0"

                convert transfreqold$     to   transfreqold
            if cntperiod$ <= " " then   cntperiod$ = "0"
                convert cntperiod$        to   cntperiod
            cntperiod% = int(cntperiod)
            cntnbr% = int(cntnbr)
            if count_factor$ <= " " then count_factor$ = "0"
                convert count_factor$     to   count_factor

            return

        unformat_dates
            call "DATUNFMT" (datecnt$)
            call "DATUNFMT" (nextcntdate$)
            call "DATUNFMT" (recdate$)
            call "DATUNFMT" (abclastdate$)
            call "DATUNFMT" (translastdate$)
            call "DATUNFMT" (tempdate$)
            call "DATUNFMT" (cntprdlstdat$)
            call "DATUNFMT" (firstcntdate$)
            return

        edit_user_defined_fields
            call "VFINPSUB" ("HNYCCMST", "E", "Manage Cycle Count Recor" ~
                     & "ds", str(line2$,,60), "YN", variable$, keyhit%)
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then editpg1
            if keyhit% = 16% then datasave
            goto editpg1


        delete_part_record
            deletekey$ = plowkey$
L33510:     key% = 0%
            call "ASKUSER" (key%, "*** DELETE CYCLE COUNT ITEM ***",     ~
                            "Press PF(16) to Delete & Continue          "~
                            ,   "-- OR --"                               ~
                           ,"Press (RETURN) to Abort Detete" )
            if key% = 16% then L33590
            if key% =  0% then  goto editpg1     else  goto L33510

L33590:     call "SHOSTAT" ("Deleting Record Now")
            call "DELETE" (#03, deletekey$,44%)
            /* Delete from the Detail file if Pre-Active */
            if actflag$ <> "P" then goto inputmode
                deletekey$ = "P" &  deletekey$
                call "REDALT1" (#04, deletekey$, 1%, f1%(4))
                if f1%(4) = 0 then goto inputmode
                delete #04

            goto inputmode

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: HNYCCMST                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(3),          /* Warehouse or Store                      */~
            CH(16),         /* Which lot in inventory - always used wit*/~
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
            CH(6),          /* Date First Count was Made               */~
            CH(200),        /* Variable Field                          */~
            PD(14,4),       /* Count Rate                              */~
            CH(387)         /* Filler                                  */

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
              lfac$(13%), lfac$(14%), lfac$(15%)  =  hex(8c)

              if fieldnr% > 0% and admin% > 0%                           ~
                               then init(hex(8c)) afac$()                ~
                               else init(hex(86)) afac$()
              on fieldnr% gosub L40134,         /* Part Number       */   ~
                                L40134,         /* Store             */   ~
                                L40134,         /* Lot Number        */   ~
                                L40134,         /* Cycle Count Group */   ~
                                L40136,         /* Record Start Date */   ~
                                L40136,         /* Count Tolerance % */   ~
                                L40136,         /* Count Tolerance Q */   ~
                                L40134,         /* ABC Class         */   ~
                                L40136,         /* Transaction Freq. */   ~
                                L40136,         /* Count Cycle Period*/   ~
                                L40136,         /* Next Count Date   */   ~
                                L40136,         /* Count Rate        */   ~
                                L40138,         /* ABC Class Old     */   ~
                                L40138,         /* Trans Freq. Old   */   ~
                                L40138          /* Count Period Old  */
              goto L40155

                  lfac$(fieldnr%) = hex(80) :  return    /* Up / Low   */
L40134:           lfac$(fieldnr%) = hex(81) : goto L40142 /* Upper Only */
L40136:           lfac$(fieldnr%) = hex(82) : goto L40142 /* Numeric    */
L40138:           lfac$(fieldnr%) = hex(8c) :  return    /* No Edit    */

L40142:         if fieldnr% > 7 and fieldnr% < 12 and admin% > 0%        ~
                     then  afac$(fieldnr%) = hex(81)                     ~
                     else  afac$(fieldnr%) = hex(8c)
                return

L40155:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter/Manage Cycle Count Master File",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part Number",                                ~
               at (04,14), fac(lfac$( 1)), part$                , ch(25),~
               at (04,46), fac(hex(8c)), partdesc$              , ch(30),~
                                                                         ~
               at (05,02), "Store      ",                                ~
               at (05,14), fac(lfac$( 2)), store$               , ch(03),~
               at (05,46), fac(hex(8c)), storedesc$             , ch(30),~
                                                                         ~
               at (06,02), "Lot Number",                                 ~
               at (06,14), fac(lfac$( 3)), lot$                 , ch(06),~
               at (06,46), fac(hex(8c)),   lot_msg$             , ch(32),~
                                                                         ~
               at (07,46), fac(hex(84)), sesstatus$             , ch(30),~
                                                                         ~
               at (08,02), "Cycle Count Group Name",                     ~
               at (08,30), fac(lfac$( 4)), ccgroup$             , ch(06),~
                                                                         ~
               at (09,02), "Record Start Date",                          ~
               at (09,30), fac(lfac$( 5)), recdate$             , ch(08),~
               at (09,42), "Date Last Counted",                          ~
               at (09,62), fac(hex(8c))  , datecnt$             , ch(08),~
                                                                         ~
               at (10,02), "Count Tolerance(percent)",                   ~
               at (10,30), fac(lfac$( 6)), cnttlernper$         , ch(10),~
                                                                         ~
               at (11,02), "Count Tolerance(actual)",                    ~
               at (11,30), fac(lfac$( 7)), cnttlernqty$         , ch(10),~
                                                                         ~
               at (12,02), "ABC Class",                                  ~
               at (12,30), fac(lfac$( 8)), abcclass$            , ch(01),~
               at (12,39), "Auto Change Lock",                           ~
               at (12,57), fac(afac$( 8)), abclockflag$         , ch(01),~
                                                                         ~
               at (13,02), "Transaction Freq. Factor",                   ~
               at (13,30), fac(lfac$( 9)), transfreqfact$       , ch(07),~
               at (13,39), "Auto Change Lock",                           ~
               at (13,57), fac(afac$( 9)), translockflag$       , ch(01),~
                                                                         ~
               at (14,02), "Count Cycle Period      ",                   ~
               at (14,30), fac(lfac$(10)), cntperiod$           , ch(03),~
               at (14,39), "Auto Change Lock",                           ~
               at (14,57), fac(afac$(10)), cntperiodflag$       , ch(01),~
                                                                         ~
               at (15,02), "Next Count Date         ",                   ~
               at (15,30), fac(lfac$(11)), nextcntdate$         , ch(08),~
               at (16,02), "Count Rate per Part     ",                   ~
               at (16,30), fac(lfac$(12%)), count_factor$       , ch(10),~
               at (17,02),fac(hex(ac)),  scrnmsg1$              , ch(33),~
               at (17,41), fac(hex(ac)), scrnmsg2$              , ch(12),~
               at (17,59), fac(hex(ac)), scrnmsg3$              , ch(10),~
                                                                         ~
               at (18,02), "ABC Class",                                  ~
               at (18,30), fac(lfac$(13)), abcold$              , ch(01),~
               at (18,42), fac(lfac$(13)), abclastdate$         , ch(10),~
               at (18,62), fac(lfac$(13)), abclastuser$         , ch(03),~
                                                                         ~
               at (19,02), "Transaction Freq. Factor",                   ~
               at (19,30), fac(lfac$(14)), transfreqold$        , ch(07),~
               at (19,42), fac(lfac$(14)), translastdate$       , ch(10),~
               at (19,62), fac(lfac$(14)), translastuser$       , ch(03),~
                                                                         ~
               at (20,02), "Count Period",                               ~
               at (20,30), fac(lfac$(15)), cntperiodold$        , ch(03),~
               at (20,42), fac(lfac$(15)), cntprdlstdat$        , ch(10),~
               at (20,62), fac(lfac$(15)), cntlastuser$         , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40460
                  call "MANUAL" ("HNYCCEDT") : goto L40155

L40460:        if keyhit% <> 15 then L40475
                  call "PRNTSCRN" : goto L40155

L40475:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40570     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (8)Select Cycle Count P" &        ~
                     "art/Store/Lot          (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffffff0dff0f1000)
            if fieldnr% = 1% then L40550
                str(pf$(3),18)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                                            str(pfkeys$,08,1) = hex(ff)
L40550:     if fieldnr% > 2% then L40560
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40560:     return

L40570: if fieldnr% > 0% then L40615  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)User Defined Fields " &        ~
                     "   (12)Delete Record   (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffff09ffff0c0dff0f1000)
            if actflag$ <> "A" and       /*If in Active Session or    */ ~
               f1%(3) = 1% then return   /*  New Record then NO Delete*/
                str(pf$(3),44,17) = " "  :  str(pfkeys$,12,1) = hex(ff)
            return
L40615:                              /*  Edit Mode - Enabled    */
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
                                             /* Record Start Date      */~
        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50250,         /* Part Number            */~
                              L50510,         /* Store                  */~
                              L50610,         /* Lot Number             */~
                              L50800,         /* Cycle Count Group      */~
                              L50850,         /* Record Start Date      */~
                              L50905,         /* Count Tolerance %      */~
                              L50925,         /* Count Tolerance Q      */~
                              L50945,         /* ABC Class              */~
                              L50990,         /* Transaction Freq.      */~
                              L51025,         /* Count Cycle Period     */~
                              L51100,         /* Next Count Date        */~
                              L51200,         /* Count Rate             */~
                              L51140,         /* Old ABC Class          */~
                              L51155,         /* Old Trans Frequency    */~
                              L51170          /* Old Count Period       */
            return

L50250: REM Test for Part Number                  PART$
            if part$ = "?" then part$ = " "
            plowkey$ = str(part$) & hex(00)
            descr$ = hex(06) & " Select Part/Store/Lot To Be Used"
            call "PLOWCODE" (#3, plowkey$, descr$, 9000%, 0.34, f1%(3%), ~
                 hdr$(),0  ,1.00010, incl(), incl$(), "D", " ", #3,      ~
                 descr_map())
            if f1%(3%) = 1% then L50290 else L50325
L50290:     part$  = str(plowkey$,1%,25%)
            call "DESCRIBE" (#01, part$, partdesc$, 0%, f1%(1%))
            store$ = str(plowkey$,26%,3%)
            lot$   = str(plowkey$,29%, 6%)
            goto L50510
            return

L50325:     abcorig$ = " "
            if part$ = "?" then part$ = " "
            partdescr$ = hex(06) & "Select a Part Number"
            call "PLOWCODE" (#01, part$, partdescr$, 0%, 0.32, f1%(1))
            if f1%(1) = 1% then L50365
                errormsg$ = "Part Number not on file."
                if part$ = " " then errormsg$ = "Please Enter a Part #"
                return
L50365:     plowkey$ = part$    :   partdesc$ =  partdescr$
            get #1 using  L50375, abcorig$  /*Get ABC Class frm HNYMASTR */
L50375:     FMT POS(111), CH(1)

            call "PLOWNEXT" (#02, plowkey$, 25%, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "No quantity records exist for this part"
                return

        REM Test for Store                        STORE$
L50510:     strdescr$ = hex(06) & "Select a Store Number"
            call "PLOWCODE" (#07, store$, strdescr$, 0%, 0.30, f1%(7))
            if f1%(7) = 1% then L50550
                errormsg$ = "Store Number not on file."  : return
L50550:     storedesc$ = strdescr$
            plowkey$ = str(part$) & str(store$) & hex(00)
            call "PLOWNEXT" (#02, plowkey$, 28%, f1%(2))
            if f1%(2) = 1% then L50574
                errormsg$ = "No quantity records exist for this" &       ~
                             " Part & Store."
                return

L50574:     if lot$ = " " then return    /* Lot Not Set Yet */
                fieldnr% = fieldnr% + 1%  :  goto  L50710

L50610: REM Test for Lot Number                   LOT$
            if lot$ = "?" then keyhit% = 8%
            if keyhit% <> 8% then L50710
                msg$ = hex(06) & "Select a Lot Number"
                plowkey$ = str(part$) & str(store$) & hex(00)
                call "PLOWCODE" (#02, plowkey$, msg$, 28%, 0, f1%(2))
                if f1%(2) = 1% then L50700
                     errormsg$ = "Please Enter a Lot Number."
                     return
L50700:         lot$ = str(plowkey$,29,6)
L50710:     plowkey$ = str(part$) & str(store$) & lot$
            call "READ100" (#02, plowkey$, f1%(02))
            if f1%(2) = 0% then L50760
                gosub dataload      /* Set Default values if in HNYCCMST*/
                if f1%(03) = 1% then fieldnr% = 12% /*Exit ForNext Loop*/~
                                else gosub init_default_vars

                return
L50760:     errormsg$ = "No quantity record found for this lot."
            return

L50800: REM Test for Cycle Count Group Name       CCGROUP$
            temp$ = hex(06) & "Select a Cycle Count Group Code."
            if ccgroup$ = "?" then ccgroup$ = hex(00)


            call "GETCODE"  (#05, ccgroup$, temp$, 0%, 0.30, f1%(05))
            if f1%(05) = 1% then  return
            if ccgroup$  = " " then return
                errormsg$ = "Group Code Not on Record"
            return

L50850: REM Test for Record Start Date            RECDATE$
            call "DATEOK" (recdate$, recdate%, errormsg$)
            recdate% = recdate%
            return

L50905: REM Test for Count Tolerance(percent)     CNTTLERNPER$
            if cnttlernper$ = " " then cnttlernper$  = "0"

            convert  cnttlernper$  to  cnttlernper
            call  "CONVERT" (cnttlernper, -2.2,  cnttlernper$)

            return

L50925: REM Test for Count Tolerance(actual)      CNTTLERNQTY$
            if  cnttlernqty$  = " " then   cnttlernqty$  = "0"

            convert  cnttlernqty$  to  cnttlernqty
            call  "CONVERT" (cnttlernqty, -2.2,  cnttlernqty$)

            return

L50945: REM Test for ABC Class                    ABCCLASS$
            p% = pos("ABCDX " = abcclass$)
            if p% <> 0% then L50965
                errormsg$ = "Select a Valid ABC Code" : return
L50965:     p% = pos("YN" = abclockflag$)
            if p% <> 0% then return
                errormsg$ = "Select 'Y' or 'N' for Lock Flag."
            return

L50990: REM Test for Transaction Freq. Factor     TRANSFREQFACT$
*          IF TRANSFREQFACT$  = " " THEN  TRANSFREQFACT$  = "0"
            p% = pos("YN" = translockflag$)
            if p% <> 0% then return
                errormsg$ = "Select 'Y' or 'N' for Lock Flag."
            return

L51025: REM Test for Count Cycle Period(days)     CNTPERIOD$
            if cntperiod$ <> " " then  convert cntperiod$ to cntperiod
            if cntperiod  <   0  then L51030
            if cntperiod$ <> " " and  cntperiod  <>  0  then L51032
                if nextcntdate$ = " " or nextcntdate$ = blankdate$ ~
                                      or mode% = 1%     then L51032
L51030:         errormsg$ = "Count Period must be Greater than Zero"
                return
L51032:     p% = pos("YN" = cntperiodflag$)
            if p% <> 0% then L51055
                errormsg$ = "Select 'Y' or 'N' for Lock Flag."
                return
            /* Check Next Count Date Calculation */
L51055:     if cntperiodorig$ = " "  or  cntperiodorig$ = cntperiod$     ~
                then return
            goto check_changed_values  /* Will Return from there */

L51100: REM Test for Next Count Date                  NEXTCNTDATE$
            if cntperiod$ <> " " then L51105
                nextcntdate$ = " "  :  return  /* Must have Cnt Period */
L51105:     call "DATEOK" (nextcntdate$, nextcntdate%, errormsg$)
            nextcntdate% = nextcntdate%
            if errormsg$ <> " " then return
              if nextcntdate$ >= datecnt$ then return
                errormsg$ = "Next Count Date Must be Greater or Equal" & ~
                            " to Last Count Date."
            return

L51140: REM Test for Old ABC Class                    ABCOLD$
            return

L51155: REM Test for Old Transaction Freq. Factor     TRANSFREQOLD$
            return

L51170: REM Test for Old Count Period                 CNTPERIODOLD$
            return


L51200: REM Test for Minimum Person-hours        COUNT_FACTOR
            convert count_factor$ to count_factor , data goto L51230
            call "CONVERT" (count_factor, -2.2, count_factor$)
            if count_factor > 0.0 then return
L51230:         errormsg$ = "Enter a Value Greater than Zero"
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
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
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
