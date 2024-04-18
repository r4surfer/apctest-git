        REM *************************************************************~
            *                                                           *~
            *   SSS   H   H  PPPP   DDDD    OOO    CCC   PPPP   RRRR    *~
            *  S      H   H  P   P  D   D  O   O  C   C  P   P  R   R   *~
            *   SSS   HHHHH  PPPP   D   D  O   O  C      PPPP   RRRR    *~
            *      S  H   H  P      D   D  O   O  C   C  P      R   R   *~
            *   SSS   H   H  P      DDDD    OOO    CCC   P      R   R   *~
            *                                                           *~
            *  ( APCBLSUB - SPECIAL 'APC' VERSION FOR PRINTING BOL'S )  *~
            *  ( APCINVBO - SPECIAL VERISION FOR CREATING BOL'S/INV'S)  *~
            *               By Load with the Invoice                    *~
            *-----------------------------------------------------------*~
            * SHPDOCPR - PERMITS ENTRY OF CRITERIA -- DOCUMENT(S) TO    *~
            *            PRINT, STORE RANGE, AND SCHEDULED SHIP DATE    *~
            *            RANGE.  THEN PRINT BILLS OF LADING AND/OR PICK *~
            *            LISTS BASED ON THESE CRITERIA.  PRINTING IS    *~
            *            DRIVEN BY THE ENTRIES IN BCKPRIDX.             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/23/86 ! Original                                 ! JIM *~
            * 05/14/87 ! Std Cost Changes                         ! ERN *~
            * 08/11/87 ! HES's SO, PF8, PF9 Ploymer customizations! JIM *~
            * 08/23/88 ! Removed call to Plowcode after SO entry  ! RJM *~
            * 09/22/88 ! Added PF8 to View SO Documents on file   ! RJM *~
            * 19/18/88 ! Moved variables from COM to DIM.         ! JDH *~
            * 03/01/90 ! modified COM statement.  Changed SHIP_TO$! LAB *~
            *          ! from 30 to 31                            ! LAB *~
            * 11/09/90 ! MODIFIED FOR PRINTING DESCR (SHPBOLS)    ! RHH *~
            * 11/09/90 ! MODIFIED FOR PRINTING DESCR (SHPBOLS)    ! RHH *~
            * 05/03/94 ! Mod to Print B.O.L. by Load              ! RHH *~
            *          ! Mod also to Sub (APCBLSUB) for Print     !     *~
            * 01/16/97 ! Mods for Swithch to New Planning System. ! RHH *~
            * 11/13/97 ! Mod fo Upgrade to Release R6.04.03       ! RHH *~
            * 03/14/98 ! Y2K                                      ! LDJ *~
            * 06/17/98 ! (EWD001) Mod to print BOL's by Load and  ! RHH *~
            *          !    maintain Drop Sequence.               !     *~
            *          !                                          !     *~ 
            * 06/14/01 ! (EWD002) Mod to add new screen to allow  ! CMG *~
            *          !    to reprint BOL's.                     !     *~
            *          !                                          !     *~ 
            *************************************************************

        REM *************************************************************~
            *THIS PROGRAM CALLS TWO SUBROUTINES BASED ON OPERATOR INPUT.*~
            *THE SUBROUTINE NAMES ARE 'SHPBOLS' - PRINT BILLS OF LADING,*~
            *AND 'SHPPICKS' - PRINT PICK LISTS. APCBLSUB Replace SHPBOLS*~
            *************************************************************

        com        /* FIELDS COMMON TO ALL MODULES                     */~
            bcklines_key$19,             /* Key to BCKLINES            */~
            bckmastr_key$25,             /* Key to BCKMASTR            */~
            bol$3,                       /* Bill of lading # (BCKPRIDX)*/~
            carton$7,                    /* Edited # of cartons        */~
            cust_code$9,                 /* Customer code from BCKPRIDX*/~
            date$8,                      /* Date for screen display    */~
            from_date%,                  /* Low date for compare       */~
            from_stor$3,                 /* Low store for compare      */~
            hdr$40,                      /* ASKUSER constant           */~
            hdr_desc$30,                 /* Header description         */~
            high_date$10,                /* High date for capture               (Y2K, LDJ)*/~
            i$(24%)80,                   /* Screen Image               */~
            invoice$8,                   /* Invoice # from SHPHDRS     */~
            low_date$10,                 /* Low date for capture                (Y2K, LDJ)*/~
            msg$(3%)79,                  /* ASKUSER constant           */~
            part_nmbr$25,                /* Part # from BCKLINES       */~
            part_desc$32,                /* Part desc from BCKLINES    */~
            po$16,                       /* PO # from BCKMASTR         */~
            po_line$3,                   /* PO line #                  */~
            qtyship$10,                  /* Edited quantity shipped    */~
            schd_date$8,                 /* Sched ship date fr SHPHDRS */~
            ship_date$8,                 /* Ship date from BCKPRIDX    */~
            ship_to$(6%)31,              /* Ship to name/addr- BCKMASTR*/~
            shphdrs_key$28,              /* Key to SHPHDRS             */~
            shplines_key$22,             /* Key to SHPLINES            */~
            so$16,                       /* Sales order # from BCKPRIDX*/~
            so$(2%,2%)16,                /* Sales Order # Range For Pri*/~
            stock_uom$4,                 /* Stock UOM from BCKMASTR    */~
            stor_code$3,                 /* Store code from BCKPRIDX   */~
            to_date%,                    /* High date for compare      */~
            to_stor$3,                   /* High store for compare     */~
            type$1,                      /* 'A'=Ackn, 'B'=BOL, 'P'-Pick*/~
            userid$3,                    /* Current User Id            */~
            weight$6,                    /* Edited shipment weight     */~
                   /* COMMON DATA FILE ARRAYS                          */~
            f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

        dim        /* LOCAL VARIABLES                                  */~
            load$5, load_d$30,           /* APC Load Number            */~
            code$1,                      /* 1= BOLs, 2=Picks, 3= both  */~
            codedescr$24,                /* Description of CODE$       */~
            cursor%(2),                  /* Cursor location for edit   */~
            descr$40,                                                    ~
            descr_map(8),                                                ~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            high_stor$3,                 /* High store for capture     */~
            incl(2),                                                     ~
            incl$(2)6,                                                   ~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            low_stor$3,                  /* Low store for capture      */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18                       /* PF 4 Screen Literal        */

        dim                             /*    (EWD002)                 */~
            cust_code$9,                /* Customer Code               */~
            so_num$8,                   /* Sales Order Number          */~
            cust_key$9,                 /* Customer Readkey            */~
            shp_dte$6,                  /* SO Ship Date                */~
            or_key$8,                   /* APCPLNOR Readkey for Status */~
            or_st$2,                    /* APCPLNOR Status of Order    */~
            bckpridx_key$29,            /* BCKPRIDX Readkey            */~
            blankdate$6                 /* BlankDate                   */



        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, apc1$40, pname$21
            apc$   = "(EWD) Ver for Printing B.O.L./Pick Ticks"
            apc1$  = "   Reprint B.O.L  -  NOT CREATE B.O.L   "
            pname$ = "SHPDOCPR - Rev: R6.04"

        REM *************************************************************

            mat f2% = con : mat f1% = zer

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #3  ! BCKPRIDX ! SO Document Print Index File             *~
            * #4  ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * #5  ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #6  ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #7  ! SHPLINES ! Shipment Scheduling / Pre-Invoicing- Lin *~
            * #10 ! APCPLNLD ! (New) Planning Load Master               *~
            * #11 ! APCPLNOR ! (NEW) S.O. Header Histroy         EWD002 *~
            * #12 ! CUSTOMER ! Customer Master File              EWD002 *~
            * #64 !          ! DUMMY SELECT FOR PLOWCODE                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =   11, keylen =  29,                     ~
                        alt key  1, keypos =    1, keylen =  39

            select #4,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #5,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #6,  "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  28

            select #7,  "SHPLINES",                                      ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =   10, keylen =  22

            select #10, "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =   11, keylen =   5,                     ~
                        alt key  1, keypos =    3, keylen =  13,         ~
                            key  2, keypos =    1, keylen =  15

/*EWD002*/  select #11, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

/*EWD002*/  select #12,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #64, "DUMMY",                                         ~
                        varc,     consec,  recsize = 64


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            if fs%(3) > 0% then get rslt$(3 ) using L09110, nbr_recs%
L09110:         FMT POS(17), BI(4)
            call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 0%, rslt$(4%))
            call "OPENCHCK" (#5,  fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#6,  fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "OPENCHCK" (#7,  fs%(7%), f2%(7%), 0%, rslt$(7%))
            call "OPENCHCK" (#10,  fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11,  fs%(11%), f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12,  fs%(12%), f2%(12%), 0%, rslt$(12%))

            if nbr_recs% > 0% then L09310
L09240:         comp% = 2%
                hdr$ = "*** CANCEL REQUEST ***"
                msg$(1) = "There is no Print Index activity to analyze"
                msg$(3) = "Press RETURN to cancel program"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L09240
                goto exit_program /* NOTHING TO DO; DON'T DO ANYTHING */
L09310:     call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            blankdate$ = all(hex(00))
            call "DATUFMTC" (blankdate$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, code$, high_stor$,         ~
                high_date$, codedescr$
            low_stor$, low_date$, so$() = "ALL"
                                              /* (EWD001) - Begin     */     
            load_d$ = "(ALL)Print BOL'S for all Loads"
            load% = 0%
            load$ = "ALL  "
            load_d$ = "(ALL)Print BOL'S for all Loads"

            for fieldnr% = 1 to  5
                                              /* (EWD001) - End       */
L10130:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10250
L10150:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10230
L10180:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10150
                         if fieldnr% = 1% then L10130
                         goto L10180
L10230:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10150
L10250:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10150
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = edtmessage$
            pf4$  = " "
            pf16$ = "(16)Print Data"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
L11150:     fieldnr% = cursor%(1) - 5
                                        /* (EWD001) Mod Edit to (5)    */
            if fieldnr% < 1 or fieldnr% >  5 then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf16$ = " "
L11200:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
            if cursor%(1) - 5% <> fieldnr% then L11150
            goto edtpg1


        REM *************************************************************~
            *       I N P U T   R E P R I N T   B O L ' S               *~
            *-----------------------------------------------------------*~
            * Handles reprint of BOL's entry screens.         EWD002    *~
            *************************************************************

        inputmode_reprint
            pf4$="(4)Previous Field" : pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, cust_code$, so_num$,       ~
                      cust_key$, shp_dte$, or_key$, or_st$, bckpridx_key$

            for fieldnr% = 1% to  1%
L20130:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L20250
L20150:         gosub'102(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L20230
L20170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L20150
                         if fieldnr% = 1% then L20130
                         goto L20170
L20230:               if keyhit%  = 16 then       inputmode
                      if keyhit% <>  0 then       L20150
L20250:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L20150
            next fieldnr%

        REM *************************************************************~
            *  E D I T   R E P R I N T   M A I N   P R O G R A M  EWD002*~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg2
            inpmessage$ = edtmessage$
            pf4$  = " "
            pf16$ = "(16)Exit Update"
            gosub'102(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       inputmode
                  if keyhit% <>  0 then       edtpg2
L21150:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  1 then edtpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf16$ = " "
L21200:     gosub'102(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L21200
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L21200
            if cursor%(1) - 5% <> fieldnr% then L21150
            goto edtpg2

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            select printer
        REM if code$ = "1" then call "SHPBOLS" (#3, #4, #5, #6, #7)
            if code$ = "1" then call "APCBLSUB" (load$,#3,#4,#5,#6,#7)

            if code$ = "2" then call "SHPPICKS" (#3, #4, #5, #6, #7)

        REM IF CODE$ = "3" THEN CALL "SHPBOLS" (#3, #4, #5, #6, #7)
            if code$ = "3" then call "APCBLSUB" (load$,#3,#4,#5,#6,#7)

            if code$ = "3" then call "SHPPICKS" (#3, #4, #5, #6, #7)

            goto inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING BCKPRIDX.          *~
            *************************************************************

        update_bckpridx                       /* (EWD002) - Add routine */
          init(" ") bckpridx_key$
          str(bckpridx_key$,1%,1%) = "B" 
          str(bckpridx_key$,2%,9%) = str(cust_code$,1%,9%) 
          str(bckpridx_key$,11%,16%) = str(so_num$,1%,16%)
          str(bckpridx_key$,27%,3%) = "   "

          read #3, hold, key = bckpridx_key$, eod goto L25000

                   delete #3
L25000:
          write #3 using L25500, "B", "300", shp_dte$, "B",             ~
                                cust_code$, so_num$, " ", date, " " 

L25500:         FMT CH(1), CH(3), CH(6), CH(1), CH(9), CH(16), CH(3),    ~
                    CH(6), CH(19)     

        return clear all
        goto inputmode_reprint

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20140,        /* DOCUMENT(S) TO PR */~
                                    L20180,        /* RANGE OF STORES   */~
                                    L20220,        /* SCHEDULED SHIP DA */~
                                    L20270,        /* SO RANGE          */~
                                    L20320         /* Load Number or All*/
                     return
L20140:     REM DOCUMENT(S) TO PRINT                  CODE$
            inpmessage$ = "Enter CODE for document(s) to print: 1=BOLs,"&~
                " 2=Pick Lists, 3=Both"
                return
L20180:     REM RANGE OF STORES                       FROM_STOR$
            inpmessage$ = "Enter range of STORE CODES, or 'ALL'"
                return

L20220:     REM SCHEDULED SHIP DATE RANGE             FROM_DATE$
            inpmessage$ = "Enter range of SHIP DATES, 'FIRST', 'LAST',"& ~
                " 'ALL', or blanks (immediate)"
                return

L20270:     REM SALES ORDER NUMBER RANGE              SO$()
            inpmessage$ = "Enter range of S.O. #s, 'FIRST', 'LAST', 'AL"&~
                "L', partial, or blanks"
                return
                                              /* (EWD001) - Begin      */
L20320:     REM APC LOAD NUMBER                       LOAD$
            inpmessage$ = "Enter a Valid EWD Load Number or 'ALL  ' ?"
                return
                                              /* (EWD001) - End        */

            deffn'052(fieldnr%)               /*   (EWD002)            */
                  enabled% = 1%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "SHPDOCPR: " & "R6.04.03"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40250,         /* DOCUMENT(S) TO PR*/~
                                    L40220,         /* RANGE OF STORES  */~
                                    L40220,         /* SCHEDULED SHIP DA*/~
                                    L40220,         /* SO RANGE         */~
                                    L40220          /* Load Number/ ALL */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40290

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40250:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
                                              /* (EWD001) - Begin Mod  */   
L40290:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (06,02), "Document(s) to Print",                       ~
               at (06,28), fac(lfac$(1%)), code$                , ch(01),~
               at (06,31), fac(hex(8c)), codedescr$             , ch(24),~
                                                                         ~
               at (07,02), "Range of Stores",                            ~
               at (07,28), fac(lfac$(2%)), low_stor$            , ch(03),~
               at (07,50), fac(lfac$(2%)), high_stor$           , ch(03),~
                                                                         ~
               at (08,02), "Sched. Ship Date range",                     ~
               at (08,28), fac(lfac$(3%)), low_date$            , ch(10),       /* (Y2K, LDJ) */~
               at (08,50), fac(lfac$(3%)), high_date$           , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (09,02), "Sales Order # range",                        ~
               at (09,28), fac(lfac$(4%)), so$(1,1)             , ch(16),~
               at (09,50), fac(lfac$(4%)), so$(1,2)             , ch(16),~
                                                                         ~
               at (10,02), "Prod. Load or 'ALL'",                        ~
               at (10,28), fac(lfac$(5%)), load$                , ch(5) ,~
               at (10,50), fac(lfac$(84)), load_d$              , ch(29),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf4$                           ,~
               at (24,20), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (23,20), "(8)View SO Documents on File",               ~
/*EWD002*/     at (22,65), "(14)Reprint  BOL",                           ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104080d0e0f10)), key (keyhit%)

               if keyhit% <> 13 then L40700
                  call "MANUAL" ("SHPDOCPR")
                  goto L40290

L40700:        if keyhit% <> 14 then L40710           /* (EWD002)  */
                  goto inputmode_reprint
                   

L40710:        if keyhit% <> 15 then L40740
                  call "PRNTSCRN"
                  goto L40290

L40740:        if keyhit% <> 8 then L40780
                  gosub so_plowcode
                  goto L40290

L40780:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return
                                              /* (EWD001) - End        */

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.      EWD002 - Add Screen  *~
            *************************************************************

            deffn'102(fieldnr%)
                  str(line2$,62%) = "SHPDOCPR: " & "R6.04.03"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40255          /* Sales Order Num  */

                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40295

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40255:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
                     
L40295:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc1$                  , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Sales Order Num    :",                       ~
               at (06,28), fac(lfac$(1%)), so_num$              , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (22,65), "(14)Update  Data",                           ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(0001040e0f10)), key (keyhit%)


               if keyhit% <> 14 then L40715                             
                  gosub update_bckpridx

L40715:        if keyhit% <> 15 then L40785
                  call "PRNTSCRN"
                  goto L40290

L40785:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50150,         /* DOCUMENT(S) TO PR*/~
                                    L50260,         /* RANGE OF STORES  */~
                                    L50320,         /* SCHEDULED SHIP DA*/~
                                    L50520,         /* SO RANGE         */~
                                    L50980          /* Load No./ ALL    */
                  return

L50150: REM DOCUMENT(S) TO PRINT                      CODE$
            if code$ = " " and keyhit% = 0% then code$ = "1" /*DEFAULT*/
            if code$ <> "1" then L50190
                codedescr$ = "(Bills of Lading)" : return
L50190:     if code$ <> "2" then L50210
                codedescr$ = "(Pick Lists)" : return
L50210:     if code$ <> "3" then L50230
                codedescr$ = "(Both Documents)" : return
L50230:     errormsg$ = "You must enter either '1', '2' or '3'"
                return

L50260: REM RANGE OF STORES                           FROM_STOR$
            call "TESTRNGE" (low_stor$, high_stor$, from_stor$, to_stor$,~
                errormsg$)
            from_stor$ = addc (hex(01))
                return

L50320: REM SCHEDULED SHIP DATE RANGE                 FROM_DATE$
            from_date% = 0% :  to_date% = 99999999%                             /* (Y2K, LDJ) */
            if low_date$ = " " and high_date$ = " " then goto L50480
            if low_date$ = " " then goto L50440
            if low_date$ = "ALL" then goto L50480
            if low_date$ = "FIRST" then goto L50440
                call "DATEOKC" (low_date$, from_date%, errormsg$)               /* (Y2K, LDJ) */
                if errormsg$ <> " " then return
                if high_date$ = "LAST" then goto L50480
                if high_date$ <> " " then goto L50460
                     high_date$ = low_date$ : to_date% = from_date%
                     goto L50480
L50440:     if high_date$ = " " then high_date$ = "LAST"
            if high_date$ = "LAST" then goto L50480
L50460:     call "DATEOKC" (high_date$, to_date%, errormsg$)                     /* (Y2K, LDJ) */
            if errormsg$ <> " " then return
L50480:     if from_date% > to_date% then errormsg$ =                    ~
                "The FROM Date must be EARLIER than the TO Date"
            return

L50520: REM SO NUMBER RANGE                       SO$()
            if so$(1,1) <> "ALL" then goto L50560
                so$(1,2) = " "
                goto L50660
L50560: REM Test the beginning Sales Order number
*          IF SO$(1,1) = "FIRST" THEN GOTO 50570
*          ERRORMSG$ = HEX(06) & "Enter the beginning S.O. number"
*          X% = 1%
*          GOSUB SO_PLOWCODE
        REM Test the ending Sales Order number
*          IF SO$(1,2) = "LAST" THEN GOTO 50620
*          ERRORMSG$ = HEX(06) & "Enter the ending S.O. number"
*          X% = 2%
*          GOSUB SO_PLOWCODE
L50660: REM Test for a valid range of Sales Order numbers
            call "TESTRNGE" (so$(1,1), so$(1,2), so$(2,1), so$(2,2),     ~
                errormsg$)
            return

        so_plowcode
            plowkey$ = xor plowkey$
            mat incl = zer
            incl$() = " "
            descr_map(1) = 11.01  :  descr_map(2) = 1
            descr_map(3) = 12.09  :  descr_map(4) = 6
            descr_map(5) = 21.16  :  descr_map(6) = 16
            descr_map(7) = 37.03  :  descr_map(8) = 33
            msg$(1) = " Type  Customer  Sales Order No.  BOL"
            msg$(2) = " "
            msg$(3) = "Sales Order Documents On File."
            descr$ = hex(06) & "Press PF(16) to End Display."

            if code$ = "2"                                               ~
                then str(plowkey$,,1) = "P"                              ~
                else str(plowkey$,,1) = "B"
            incl(1) = -1.01   :   incl$(1) = "A"
            if code$ <> "1" then L50910
               incl(2) = -1.01  : incl$(2) = "P"

L50910:     call "PLOWCODE" (#3, plowkey$, descr$, 9000%, 0.39, f1%(3),  ~
                             msg$(), 0, 0, incl(), incl$(), "d", "Y",    ~
                             #64, descr_map())

*          IF F1%(3) <> 0% THEN SO$(1, X%) = STR(PLOWKEY$,11,16)
            return

L50980: REM APC Load Number or (ALL)               LOAD$
            load% = 0%
            convert load$ to load%, data goto L51080

            convert load% to load$, pic(00000)

            read #10,key = load$, using L51050, load_d$, eod goto L51080
L51050:         FMT POS(16), CH(30)

        return
L51080:     load% = 0%
            load$ = "ALL  "
            load_d$ = "(ALL)Print BOL'S for all Loads"
        return

        REM *************************************************************~
            *                     T E S T   D A T A 2                   *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.  EWD002 - Test Data2 *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52700          /* Sales Order Num  */ 
                                    REM L52500         /* Customer Code    */
                                    

REM L52500    init(" ") cust_key$
REM           str(cust_key$,1%,9%) = cust_code$
REM           read #12, key = cust_key$, eod goto L52590
           
REM                  return

REM L52590    errormsg$ = "You must enter a valid Customer Code."

REM                  return

L52700:    init(" ") or_key$
           str(or_key$,1%,8%) = so_num$
           read #11, key 4% = or_key$, using L52710, cust_code$, or_st$,  ~
                                                    eod goto L52790
L52710:        FMT POS(27), CH(9), POS(60), CH(02)

           if or_st$ < "04" then goto L52780


           init(" ") bckmastr_key$, shp_dte$
           str(bckmastr_key$,1%,9%) = cust_code$
           str(bckmastr_key$,10%,16%) = so_num$

           read #4, key = bckmastr_key$, using L52750, shp_dte$,     ~
                                                      eod goto L52790
L52750:         FMT POS(824), CH(06)

          if shp_dte$ <= blankdate$  then shp_dte$ = date

          
                 return
          
L52780:   errormsg$ = "You can not print BOL, order not Complete."
L52790:   errormsg$ = "You must enter a valid SO for the Customer."
                  
                  return


        REM *************************************************************~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
