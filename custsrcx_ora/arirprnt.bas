        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   AAA   RRRR   IIIII  RRRR   PPPP   RRRR   N   N  TTTTT   *~
            *  A   A  R   R    I    R   R  P   P  R   R  NN  N    T     *~
            *  AAAAA  RRRR     I    RRRR   PPPP   RRRR   N N N    T     *~
            *  A   A  R   R    I    R   R  P      R   R  N  NN    T     *~
            *  A   A  R   R  IIIII  R   R  P      R   R  N   N    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIRPRNT - This program uses the user inputed selection   *~
            *           criteria to search for valid invoices in the    *~
            *           ARIMASTR File and when found, prints a duplicate*~
            *           invoice using the sub-function ariprsub         *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/02/91 ! Original -- This program is in response  ! RJ1 *~
            *          ! to PRR's 10140,10201, & 10608            !     *~
            * 11/30/92 ! PRR 12708 Added option for 'Dup' literal.! JDH *~
            * 03/31/93 ! PRR 12851 Now uses Report ID 'ARI006'.   ! JDH *~
            * 07/24/96 ! Changes for the year 2000.               ! DXL *~
            * 05/08/00 ! Add sub 'ARIPRS2' for auto faxing of     ! CMG *~
            *               invoices  (EWD001)                    !     *~
            * 09/14/06 ! abend is db connection fails (AWD001)    ! DES *~
            *06/06/2011! (AWD002) - add orcl usr & pswd lookup    ! CMG *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            columnttl$51,                /* Column Title Lines         */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Range             */~
            date$8,                      /* Date for screen display    */~
            dup_msg_option$1,            /* 'Duplicate' Literal Option */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            facpf3$10,                   /* Fac variable for PF Screen */~
            fmcuscode$9,                 /* Beginning Customer Range   */~
            fmflag$1,                    /* Beginning Range Flag       */~
            fminvdate$10,                /* Beginning Invoice Date Rng.*/~
            fminvoice$8,                 /* Beginning Customer Range   */~
            fmstore$3,                   /* Beginning Store/Wrhse Range*/~
            fmuserid$3,                  /* Beginning User ID  Range   */~
            hicuscode$9,                 /* Highest Customer Range     */~
            hiinvdate$10,                /* Highest Invoice Date Range */~
            hiinvoice$8,                 /* Highest Customer Range     */~
            histore$3,                   /* Highest Store/Wrhse Range  */~
            hiuserid$3,                  /* Highest User ID  Range     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invdate$8,                   /* Invoice Date Range         */~
            invoice$8,                   /* Invoice Range              */~
            invtype$1,                   /* Invoice Type               */~
            invtypechoice$(8)1,          /* Invoice Type Choices       */~
            invtypeok$1,                 /* Invoice Type Checking Flag */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            locuscode$9,                 /* Lowest Customer Range      */~
            loinvdate$10,                /* Lowest Invoice Date Range  */~
            loinvoice$8,                 /* Lowest Customer Range      */~
            lostore$3,                   /* Lowest Store/Wrhse Range   */~
            louserid$3,                  /* Lowest User ID  Range      */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rprntflag$1,                 /* Reprint Flag               */~
            rptid$6,                     /* Report Id                  */~
            store$3,                     /* Store Range                */~
            temp$60,                     /* Temporary string           */~
            tocuscode$9,                 /* Ending Customer Range      */~
            today$8,                     /* Today's Date               */~
            toinvdate$10,                /* Ending Invoice Date Range  */~
            toinvoice$8,                 /* Ending Customer Range      */~
            tostore$3,                   /* Ending Store/Wrhse Range   */~
            touserid$3,                  /* Ending User ID  Range      */~
            userid$3                     /* Entered By Range           */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        dim fax_flag$1,                  /* Flag to fax or not         */~
            inv_no$10,                   /* Fax Number                 */~
            sav_no$10,                   /* Fax Number                 */~
            attn$60,                     /* Attention Line             */~
            ewdkey$20                    /* Update For Faxed I/C/A     */   

        dim                                                              ~
            server$25,                   /* Connection String          */~
            user$25,                     /* User name to Connect       */~
            pass$25                      /* Password to Connect        */

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
            * #01 ! ARIMASTR ! Invoice Master File                      *~
            * #02 ! CUSTOMER ! Customer Master File                     *~
            * #03 ! STORNAME ! Store Name Master File                   *~
            * #05 ! SYSFILE2 ! Caelus Management System General Informa *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  3, keypos =   34, keylen =  16, dup,    ~
                            key  2, keypos =   18, keylen =  16, dup,    ~
                            key  1, keypos =   10, keylen =   8, dup     ~

            select #02, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  5, keypos = 1049, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  1, keypos =   10, keylen =  30, dup

            select #03, "STORNAME",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =   3

            select #04, "EWDINVRF",                                      ~
                        varc,     indexed,  recsize =   20,              ~
                        keypos =    1, keylen =  20

            select #05, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

                        
            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 100%, rslt$(04))  
/*(AWD002) */          
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            today$ = date$
            call "DATEFMT" (date$)
            rptid$ = "ARI006"
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "ARIRPRNT: " & str(cms2v$,,8)

            str(columnttl$, 1) = "From"
            str(columnttl$,27) = "To"

            batch% = 0%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  9%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 12% then gosub inputmode2
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
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       main_loop /* Lets Do It */
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% > 6% then fieldnr% = fieldnr% -  1% /*Allow for*/~
                                                /* 2 Lines in Field 6% */
            if fieldnr% < 1% or fieldnr% >  9% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode2
            gosub initialize_variables2

            for fieldnr% = 1% to  2%
L10115:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10235
L10135:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10165:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10135
                         if fieldnr% = 1% then L10115
                         goto L10165
L10215:               if keyhit% = 16% and fieldnr% = 1% then inputmode
                      if keyhit% <> 0% then       L10135
L10235:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10135
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then       update_ewdinvrf
                  if keyhit%  = 16% then       inputmode
                  if keyhit% <>  0% then       editpg2
L11125:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  2% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11175:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11175
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11175
                  lastfieldnr% = fieldnr%
            goto L11125

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Customer Range:        */~
                              L20200,         /* Invoice Range:         */~
                              L20300,         /* Date Range:            */~
                              L20400,         /* Store Range:           */~
                              L20500,         /* Entered By Range:      */~
                              L20600,         /* Invoice Type:          */~
                              L20700,         /* 'Dup' Literal          */~
                              L20800,         /* 'Fax' Flag             */~
                              L20900          /* Attn: area             */
            return

L20100: REM Def/Enable Customer Range              CUSCODE$
            if fmcuscode$ = " " then fmcuscode$ = "ALL"
            return

L20200: REM Def/Enable Invoice Range               INVOICE$
            if fminvoice$ = " " then fminvoice$ = "ALL"
            return

L20300: REM Def/Enable Invoice Date Range          INVDATE$              ~
             * Default is to set From Date 30 days before Today's Date *
            if toinvdate$ = " " or toinvdate$ = blankdate$ then ~
               toinvdate$ = fminvdate$
            if fminvdate$ <> " " and fminvdate$ <> blankdate$ then return
            if toinvdate$ = " " or toinvdate$ = blankdate$ then ~
               toinvdate$ = today$ else  call "DATUFMTC" (toinvdate$)
            call "DATE" addr("G+", toinvdate$, -30%, fminvdate$, err%)
            if err% = 1% then fminvdate$ = toinvdate$
            call "DATFMTC" (fminvdate$)
            call "DATFMTC" (toinvdate$)
            return

L20400: REM Def/Enable Store Range                 STORE$
            if fmstore$ = " " then fmstore$ = "ALL"
            return

L20500: REM Def/Enable Entered By Range            USERID$
            if fmuserid$ = " " then fmuserid$ = "ALL"
            return

L20600: REM Def/Enable Invoice Type                INVTYPE$              ~
             * Default is to have All choices active *
            if str(invtypechoice$()) <> " " then return
                init ("X") invtypechoice$()
                return

L20700: REM Def/Enable 'Duplicate' Literal Option  DUP_MSG_OPTION$
            if dup_msg_option$ = " " then dup_msg_option$ = "Y"
            return

L20800: REM Def/Enable 'Fax' Flag                  FAX_FLAG$  (EWD001)
            if fax_flag$ = " " then fax_flag$ = "N"
            if fax_flag$ = "N" then inv_no$ = " "
            return

L20900: REM Def/Enable Attn area                  ATTN$  (EWD001)
            if fax_flag$ = "N" then attn$ = " "
            return

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21000,         /* Customer Code        */~
                              L21100          /* Invoice Number       */

            return

L21000: REM Def/Enable Customer Range              CUSCODE$

            return

L21100: REM Def/Enable Invoice Range               INVOICE$
            if fminvoice$ = " " then fminvoice$ = "ALL"
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
         "Enter Customer Range.                                        ",~
         "Enter Invoice Range.                                         ",~
         "Enter Invoice Date Range.                                    ",~
         "Enter Store Range.                                           ",~
         "Enter User ID Range.                                         ",~
         "Place a Non-Blank Character Next to Types to Print.          ",~
         "Enter 'Y' to Print 'DUPLICATE COPY' on Invoice; else Enter 'N'",~
         "Enter 'Y' & Fax No. to Fax or 'N' to Print Invoice.          ",~
         "Enter ATTN: informating, if faxing Invoice?                  "         

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28060
                inpmessage$ = edtmessage$
                return

L28060
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                                ~
         "Enter Customer Code.                                          ",~
         "Enter Invoice Number.                                         "
         
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, fmstore$, fmuserid$,       ~
                      fmcuscode$, fminvoice$, invtype$  , fminvdate$,    ~
                      cuscode$, invdate$, invoice$, invtype$, store$,    ~
                      tocuscode$, toinvoice$, toinvdate$, tostore$,      ~
                      touserid$, userid$, dup_msg_option$,               ~
                      invtypechoice$(), fax_flag$, inv_no$, attn$
            return

        initialize_variables2
            init(" ") errormsg$, inpmessage$, cuscode$, invoice$
            
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
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
            if batch% <> 0% then call "FILEBGON" (#04)
            return clear all
            goto inputmode

        REM *************************************************************~
            *           M A I N   P R O G R A M   L O O P               *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area and Processes data.      *~
            *************************************************************
        main_loop
            abend% = 0%                      /* Initialze for ARIPRSUB */
            gosub oracle_connect
            if dup_msg_option$ = "N" then dup% = 0% else dup% = 1%
            if str(attn$,1%,60%) <> " " and fax_flag$ = "Y" then       ~
                                 str(attn$,1%,60%) = "ATTN:"& attn$
            if batch% <> 0% then goto main_loop_batch
            
            str(plowkey$,  1,  9) = str(locuscode$,  1,  9)
            str(plowkey$, 10,  8) = all(hex(ff))
            rprntflag$ = "N"
            call "SHOSTAT" ("SEARCHING THE INVOICE FILE")
            call "READ102" (#01, plowkey$, f1%(01)) /*Get 1st Valid Rec*/
            if f1%(01) = 0% then finished_invoices
            goto L30140

        plow_loop1                             /* Process File Records */
            call "READNEXT"  (#01,  f1%(01))
                if f1%(01) = 0 then  finished_invoices /*No More Recrds*/
L30140:     get #01 using L35030,                                         ~
                       cuscode$, invoice$, invdate$, userid$, store$,    ~
                       invtype$

*           * Check for Valid Range  *
            if cuscode$ >  hicuscode$ then finished_invoices
            if fminvoice$ = "ALL" then L30225
            if invoice$ <= loinvoice$  or  invoice$ > hiinvoice$         ~
                then plow_loop1
L30225:     if fminvdate$ = "ALL" then L30245
            if invdate$ < loinvdate$  or  invdate$ > hiinvdate$          ~
                then plow_loop1
L30245:     if fmuserid$ = "ALL" then L30265
            if userid$ < fmuserid$  or  userid$ > touserid$              ~
                then plow_loop1
L30265:     if fmstore$ = "ALL" then L30290
            if store$ < fmstore$  or store$ > tostore$                  ~
                then plow_loop1
L30290:     gosub check_invtype       /* Check for Valid Invoice Types */
            if invtypeok$ <> "Y" then plow_loop1

*           * Re-print a Valid Invoice *
            gosub reprint_invoice
            goto plow_loop1

*         *** *** END MAIN_LOOP *** ***  *

        main_loop_batch 
           gosub read_ewdinvrf
           gosub read_arimastr
           gosub check_invtype       /* Check for Valid Invoice Types */
           if invtypeok$ <> "Y" then main_loop_batch

*           * Re-print a Valid Invoice *
            str(sav_no$,1%,10%) = inv_no$
            gosub reprint_invoice
            init(" ") inv_no$   :   str(inv_no$,1%,10%) = sav_no$            
            goto main_loop_batch
           
 
        reprint_invoice
           prt% = 0%                                   /* Initialize  */
           if fax_flag$ <> "Y" then goto L30400        /*  (EWD001)   */
           
           call "ARIPRSU2" (dup%, prt%, abend%, cuscode$, invoice$,~ 
                            rptid$, inv_no$, attn$)
           init(" ") inv_no$
           if fax_flag$ = "Y" then inv_no$ = "F A X E D "
  
L30400:    call "ARIPRSUB" (dup%, prt%, abend%, cuscode$, invoice$,      ~
                            rptid$, inv_no$)
                                                      /*  (EWD001)   */
           if abend% <> 0% then goto abend_program   /* Abort Program */
           if prt% = 1% then L30445              /* Re-Printed Invoice */
              call "SHOSTAT" ("INVOICE  NOT  PRINTED") : rprntflag$ = "N"
              return
L30445:    if rprntflag$ = "Y" then  return
              call "SHOSTAT" ("Printing Customer Invoices")
              rprntflag$ = "Y"

           return

        abend_program
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto exit_program

        check_invtype    /* Check for Valid Inventory Type and Expand */
           invtypeok$ = "N"
           if invtype$ = " " then return

           p% = pos("MOACDXGF" = invtype$)
           if invtypechoice$(p%) = " " then return
           invtypeok$ = "Y"
           return     /* END Sub-Function CHECK_INVTYPE  */


         finished_invoices
            call "SHOSTAT" ("FINISHED WITH RE-INVOICING")
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            if rprntflag$ = "N" then                                     ~
                call "ASKUSER" ( 0%, "** REPRINT INVOICE STATUS **",     ~
                " ", "No Invoices Found to Reprint", "Press ANY PF Key" &~
                " to Continue.")
            goto inputmode

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: ARIMASTR                          */~
            CH(9),          /* Customer Code                           */~
            CH(8),          /* Invoice Number                          */~
            POS(521),                                                    ~
            CH(6),          /* invoice date                            */~
            POS(545),                                                    ~
            CH(3),          /* User ID who entered transaction         */~
            POS(870),                                                    ~
            CH(3),          /* Store or Warehouse Code                 */~
            POS(891),                                                    ~
            CH(1)           /* Identifies source (ergo) type of Invoice*/

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
              on fieldnr% gosub L40100,         /* Customer Range:    */  ~
                                L40100,         /* Invoice  Range:   */   ~
                                L40100,         /* Date Range:       */   ~
                                L40100,         /* Store Range:      */   ~
                                L40100,         /* Entered By Range: */   ~
                                L40100,         /* Invoice Type:     */   ~
                                L40100,         /* 'Dup' Literal     */   ~
                                L40100,         /* 'Fax' Flag        */   ~
                                L40100          /* ATTN area         */                                
              goto L40115

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40100:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40115:     accept                                                       ~
               at (01,02),                                               ~
                  "Duplicate Invoice Selection Criteria",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)), columnttl$             , ch(51),~
                                                                         ~
               at (07,02), "Customer Code ",                             ~
               at (07,30), fac(lfac$( 1%)), fmcuscode$          , ch(09),~
               at (07,56), fac(lfac$( 1%)), tocuscode$          , ch(09),~
                                                                         ~
               at (08,02), "Invoice Number ",                            ~
               at (08,30), fac(lfac$( 2%)), fminvoice$          , ch(08),~
               at (08,56), fac(lfac$( 2%)), toinvoice$          , ch(08),~
                                                                         ~
               at (09,02), "Invoice Date ",                              ~
               at (09,30), fac(lfac$( 3%)), fminvdate$          , ch(10),~
               at (09,56), fac(lfac$( 3%)), toinvdate$          , ch(10),~
                                                                         ~
               at (10,02), "Store ID ",                                  ~
               at (10,30), fac(lfac$( 4%)), fmstore$            , ch(03),~
               at (10,56), fac(lfac$( 4%)), tostore$            , ch(03),~
                                                                         ~
               at (11,02), "Entered By User ",                           ~
               at (11,30), fac(lfac$( 5%)), fmuserid$           , ch(03),~
               at (11,56), fac(lfac$( 5%)), touserid$           , ch(03),~
                                                                         ~
               at (12,02), "Invoice Type ",                              ~
               at (12,32),"Manual   Sales Order    Adjust       Credit ",~
               at (13,32),"Direct   Export Order   Generated    Finance",~
               at (12,30), fac(lfac$( 6%)), invtypechoice$(1%)  , ch(01),~
               at (12,39), fac(lfac$( 6%)), invtypechoice$(2%)  , ch(01),~
               at (12,54), fac(lfac$( 6%)), invtypechoice$(3%)  , ch(01),~
               at (12,67), fac(lfac$( 6%)), invtypechoice$(4%)  , ch(01),~
               at (13,30), fac(lfac$( 6%)), invtypechoice$(5%)  , ch(01),~
               at (13,39), fac(lfac$( 6%)), invtypechoice$(6%)  , ch(01),~
               at (13,54), fac(lfac$( 6%)), invtypechoice$(7%)  , ch(01),~
               at (13,67), fac(lfac$( 6%)), invtypechoice$(8%)  , ch(01),~
                                                                         ~
               at (14,02), "Print 'DUPLICATE COPY'?",                    ~
               at (14,30), fac(lfac$( 7%)), dup_msg_option$     , ch(01),~
/*  (EWD001)   */                                                        ~
               at (15,02), "FAX Invoice?",                               ~
               at (15,30), fac(lfac$( 8%)), fax_flag$           , ch(01),~
               at (15,35), fac(lfac$( 8%)), inv_no$             , ch(10),~
                                                                         ~               
               at (16,02), "If 'Y', ATTN: ?",                            ~               
               at (16,20), fac(lfac$( 9%)), attn$               , ch(55),~               
/*  (EWD001)   */                                                        ~
               at (21,02), fac(hex(a4))  ,   inpmessage$        , ch(79),~
               at (22,02), fac(hex(8c))  ,   pf$(1%)            , ch(79),~
               at (23,02), fac(hex(8c))  ,   pf$(2%)            , ch(79),~
               at (24,02), fac(facpf3$)  ,   pf$(3%)            , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40555
                  call "MANUAL" ("ARIRPRNT") : goto L40115

L40555:        if keyhit% <> 15% then L40570
                  call "PRNTSCRN" : goto L40115

L40570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40665     /*  Input Mode             */
            facpf3$ = hex(8c)
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (12)Create Batch       " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffff0c0dff0f1000)
            if fieldnr% = 1% then L40645
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                str(pf$(3),18,26) = " "  :  str(pfkeys$,12,1) = hex(ff)                
L40645:     if fieldnr% > 1% then L40655
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40655:     return

L40665: if fieldnr% > 0% then L40710  /*  Edit Mode - Select Fld */
            facpf3$ = hex(84)
            pf$(1) = "(1)Start Over                           " &        ~
                     "                      (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                      (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                      (16)Print Invoice"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40710:                              /*  Edit Mode - Enabled    */
            facpf3$ = hex(8c)
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return


        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40100,         /* Customer Range     */  ~
                                L40100          /* Invoice  Range    */
                                
              goto L40120

L40120:     accept                                                       ~
               at (01,02),                                               ~
                  "Batch Invoice Selection Criteria",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (07,02), "Customer Code ",                             ~
               at (07,30), fac(lfac$( 1%)), cuscode$            , ch(09),~
                                                                         ~
               at (08,02), "Invoice Number ",                            ~
               at (08,30), fac(lfac$( 2%)), invoice$            , ch(08),~
               at (21,02), fac(hex(a4))  ,   inpmessage$        , ch(79),~
               at (22,02), fac(hex(8c))  ,   pf$(1%)            , ch(79),~
               at (23,02), fac(hex(8c))  ,   pf$(2%)            , ch(79),~
               at (24,02), fac(facpf3$)  ,   pf$(3%)            , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15% then L40575
                  call "PRNTSCRN" : goto L40120

L40575:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L41665     /*  Input Mode             */
            facpf3$ = hex(8c)
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Batch  "
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L41645
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41645:     if fieldnr% > 1% then L41655
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41655:     return

L41665: if fieldnr% > 0% then L41710  /*  Edit Mode - Select Fld */
            facpf3$ = hex(84)
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                      "
            pf$(2) = "                                        " &        ~
                     "                      (15)Print Screen"
            pf$(3) = "                 (12)Add to Batch       " &        ~
                     "                      (16)Print Invoice"
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            return
L41710:                              /*  Edit Mode - Enabled    */
            facpf3$ = hex(8c)
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Customer Range:        */~
                              L50200,         /* Invoice  Range:        */~
                              L50300,         /* Date Range:            */~
                              L50400,         /* Store Range:           */~
                              L50500,         /* Entered By Range:      */~
                              L50600,         /* Invoice Type:          */~
                              L50700,         /* 'Dup' Literal          */~
                              L50800,         /* 'Fax' Flag             */~
                              L50900          /* 'Fax' Flag             */
                              
            return
L50100: REM Test for Customer Range               CUSCODE$
            if fmcuscode$ = "?" then  fmcuscode$ = " "
            if fmcuscode$ = " " then   fmflag$ = " "                     ~
                else  fmflag$ = "N"
            if fmcuscode$ = "ALL" then L50120
            if fmcuscode$ = "FIRST" then L50124
                temp$ = hex(0684) & "SELECT FROM CUSTOMER"
                call "GETCODE" (#02, fmcuscode$, temp$, 0%, 0, f1%(02))
                if fmcuscode$ <> " " then L50116
                errormsg$ = "INVALID CUSTOMER CODE" : return
L50116:     if tocuscode$ <> " " then L50124
                if fmflag$ = " " then L50126
L50120:         tocuscode$ = fmcuscode$
                if f1%(2) = 0 and fmcuscode$ <> "ALL"   then L50126
                goto L50132
L50124:     if tocuscode$ = "LAST" then L50132
L50126:         temp$ = hex(0684) & "SELECT TO CUSTOMER"
                call "GETCODE" (#02, tocuscode$, temp$, 0%, 0, f1%(02))

L50132:     call "TESTRNGE"                                              ~
                 (fmcuscode$               , tocuscode$              ,   ~
                  locuscode$               , hicuscode$              ,   ~
                  errormsg$)
            if fmcuscode$ = "FIRST" then fmcuscode$ = all(hex(00))

            return

L50200: REM Test for Invoice Range                INVOICE$
            if fminvoice$ = "?" then  fminvoice$ = " "
            if fminvoice$ = " " then   fmflag$ = " "                     ~
                else  fmflag$ = "N"
            if fminvoice$ = "ALL" then L50220
            if fminvoice$ = "FIRST" then L50224
                temp$ = hex(0684) & "SELECT FROM INVOICE"
                call "PLOWCODE" (#01, fminvoice$, temp$, 0%, 1.0, f1%(01))
                if fminvoice$ <> " " then L50216
                errormsg$ = "INVALID INVOICE NUMBER" : return
L50216:     if toinvoice$ <> " " then L50224
                if fmflag$ = " " then L50226
L50220:         toinvoice$ = fminvoice$
                if f1%(01) = 0 and fminvoice$ <> "ALL"   then L50226
                goto L50232
L50224:     if toinvoice$ = "LAST" then L50232
L50226:         temp$ = hex(0684) & "SELECT TO INVOICE"
                call "PLOWCODE" (#01, toinvoice$, temp$, 0%, 1.0, f1%(01))

L50232:     call "TESTRNGE"                                              ~
                 (fminvoice$          , toinvoice$          ,            ~
                  loinvoice$          , hiinvoice$          ,            ~
                  errormsg$)
            if fminvoice$ = "FIRST" then fminvoice$ = all(hex(00))

            return

L50300: REM Test for Invoice Date Range           INVDATE$

            if fminvdate$ <> "ALL"    then L50314
                loinvdate$, fminvdate$  =  "19000101"
                call "DATFMTC" (loinvdate$): call "DATFMTC" (fminvdate$)
                hiinvdate$, toinvdate$  =  "20991231"
                call "DATFMTC" (hiinvdate$): call "DATFMTC" (toinvdate$)
                goto L50330
L50314:     call "DATEOKC" (fminvdate$, fminvdate%, errormsg$)
            if errormsg$ <> " " then  return
            if toinvdate$ = " " then toinvdate$ = fminvdate$
            call "DATEOKC" (toinvdate$, toinvdate%, errormsg$)
            if errormsg$ <> " " then return

            loinvdate$ = fminvdate$
            hiinvdate$ = toinvdate$
L50330:     call "DATUFMTC"  (loinvdate$)
            call "DATUFMTC"  (hiinvdate$)
            if loinvdate$ > hiinvdate$  then                             ~
                errormsg$ = "TO MUST BE EQUAL TO OR GREATER THAN FROM"   ~
                else  errormsg$ = " "

            return

            toinvdate%, fminvdate% = 0%  /* Do Nothing line */

L50400: REM Test for Store Range                  STORE$
            if fmstore$ = "?" then  fmstore$ = " "
            if fmstore$ = " " then   fmflag$ = " "                       ~
                else  fmflag$ = "N"
            if fmstore$ = "ALL" then L50423
            if fmstore$ = "FIRST" then L50427
                temp$ = hex(0684) & "SELECT FROM STORE"
                call "GETCODE" (#03, fmstore$, temp$, 0%, 0, f1%(03))

            if tostore$ <> " " then L50427
                if fmflag$ = " " then L50429
L50423:         tostore$ = fmstore$
                goto L50460
L50427:     if tostore$ = "LAST" then L50460
L50429:         temp$ = hex(0684) & "SELECT TO STORE"
                call "GETCODE" (#03, tostore$, temp$, 0%, 0, f1%(03))

L50460:     call "TESTRNGE"                                              ~
                 (fmstore$            , tostore$            ,            ~
                  lostore$            , histore$            ,            ~
                  errormsg$)
            if fmstore$ = "FIRST"  then  fmstore$ = all(hex(00))

            return

L50500: REM Test for Entered By Range             USERID$
            call "TESTRNGE"                                              ~
                 (fmuserid$           , touserid$           ,            ~
                  louserid$           , hiuserid$           ,            ~
                  errormsg$)
            return

L50600: REM Test for Invoice Type                 INVTYPE$
            if str(invtypechoice$()) = " "                               ~
                then errormsg$ = "AN INVOICE TYPE MUST BE SELECTED"
            return

L50700: REM Test for 'Duplicate' Literal Option   DUP_MSG_OPTION$
            if dup_msg_option$ = "Y" or dup_msg_option$ = "N" then return
                errormsg$ = "Invalid Option; Enter 'Y' or 'N'."
                return

L50800: REM Test for 'Fax' Flag Option            FAX_FLAG$ & INV_NO$   (EWD001)
            inv_no% = 0%
            if fax_flag$ <> "Y" and fax_flag$ <> "N" then goto L50810
            if fax_flag$ = "N" then inv_no$ = " "
            if fax_flag$ = "N" then return
            convert inv_no$ to inv_no%, data goto L50820
            
            str(tocuscode$,1%,9%) = fmcuscode$
            str(toinvoice$,1%,8%) = fminvoice$
        return   
L50810:         errormsg$ = "Invalid Option; Enter 'Y' or 'N'."
                return
L50820:          fax_flag$ = "N"
                 inv_no$ = " " 
                 errormsg$ = "Invalid Fax Number. "
                return

L50900: REM Test for ATTN area                   ATTN$   (EWD001)
            if fax_flag$ = "N" then attn$ = " "
            if fax_flag$ = "N" then return

        return   

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51100,         /* Customer Range         */~
                              L51200          /* Invoice  Range         */
                              
            return
L51100: REM Test for Customer Range               CUSCODE$
            if cuscode$ <> " " then L51116
                errormsg$ = "INVALID CUSTOMER CODE" : return
L51116:     
            return

L51200: REM Test for Invoice Range                INVOICE$
                if invoice$ <> " " then L51216
                errormsg$ = "INVALID INVOICE NUMBER" : return
L51216:
            return

        read_ewdinvrf
            init(" ") ewdkey$

            read #4, hold, key > ewdkey$, using L10330, userid$, cuscode$, ~
                                          invoice$, eod goto finished_invoices

                 delete #4
        return

        read_arimastr
            init(" ") ewdkey$
            str(ewdkey$,1%,9%)  = cuscode$
            str(ewdkey$,10%,8%) = invoice$

            read #1, key = ewdkey$, using L35030,                        ~
                       cuscode$, invoice$, invdate$, userid$, store$,    ~
                       invtype$, eod goto finished_invoices
        return

        
        update_ewdinvrf
            init(" ") ewdkey$
            str(ewdkey$,1%,3%)  = userid$
            str(ewdkey$,4%,9%)  = cuscode$
            str(ewdkey$,13%,8%) = invoice$

            read #4, hold, key = ewdkey$, eod goto not_there

                 delete #4
        not_there
            put #4, using L10330, userid$, cuscode$, invoice$

L10330:         FMT CH(03), CH(09), CH(08)

            write #4, eod goto ewd_error

            batch% = batch% + 1%
        goto inputmode2
        ewd_error
            call "SHOSTAT" (" Unable to update EWDINVRF : "  & invoice$) : stop
        return

        oracle_connect                                     /*(EWD005)*/
            init(" ") user$, pass$, server$
REM            user$   = "MSSQL"
REM            pass$   = "MSSQL"
            gosub get_user_pswd       /* (AWD002) */

            oci_err% = 0%
            call "CONNECT"  (user$, pass$, server$, oci_err%)
/*AWD001*/
	    if oci_err% >= 0 then return
            msg$(1) = "Database connection failed"
	    msg$(2) = "Program abending          "
            msg$(3) = "Press RETURN to continue"
	    call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
	    goto exit_program
/*AWD001*/
        return

        oracle_discnnct
            call "DISCNNCT" (oci_err%)
        return

/* (AWD002) beg */
        get_user_pswd
            call "READ100" (#05, "ORACLE PASSWORD", f1%(05%))   /* SYSFILE2 */
            if f1%(05%) <> 0% then get #05 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return

/* (AWD002) END */
        
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
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            if batch% <> 0% then call "FILEBGON" (#04) 
            gosub oracle_discnnct
            call "SHOSTAT" ("One Moment Please")

            end



