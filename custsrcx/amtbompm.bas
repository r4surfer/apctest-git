        REM *************************************************************~
            *                                                           *~
            *   AAA   M   M  TTTTT  BBBB    OOO   M   M  PPPP   M   M   *~
            *  A   A  MM MM    T    B   B  O   O  MM MM  P   P  MM MM   *~
            *  AAAAA  M M M    T    BBBB   O   O  M M M  PPPP   M M M   *~
            *  A   A  M   M    T    B   B  O   O  M   M  P      M   M   *~
            *  A   A  M   M    T    BBBB    OOO   M   M  P      M   M   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AMTBOMPM - This program allows the User to enter the      *~
            *            Field Definition(s) associated with each field,*~
            *            which together will comprise the entire        *~
            *            (Manufactured) Part Number. The User will be   *~
            *            prompted for the Field Number, its Name,       *~
            *            Length, if it is to be Validated or Not, and   *~
            *            the Name of the Program which will contain its *~
            *            Formulas ( When Applicable ).                  *~
            *                                                           *~
            * Manufactured Part Number - Fields ( 01 thru 30 )          *~
            *          Sub Part Number - Fields ( 12 thru 31 )          *~
            *                                       (AWD001)            *~
            * Phantom Part Number      - Fields ( 32 thru 60 )          *~
            *                                                           *~
            * Raw Material Part Number - Fields ( 61 thru 89 )          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/04/89 ! Original                                 ! WKB *~
            * 11/28/89 ! Mod for Phantom Defns (flds 30+)         ! WKB *~
            * 01/09/90 ! Mod for Raw & Option Defns               ! WKB *~
            * 03/15/90 ! Complete for Installation                ! RHH *~
            * 05/02/90 ! Program Complete                         ! RHH *~
            * 10/18/90 ! Modified to create a different Report    ! RHH *~
            *          ! Format. A New Page for each Part         !     *~
            *          ! Definition.                              !     *~
            * 11/28/97 ! Mods for Upgrade to Release R6.04.03     ! RHH *~
            *03/02/2010! (AWD001) Mod for Subpart Number          ! CMG *~ 
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            rpt_hdr$20,                  /* Parameter Definition Header*/~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            file_name$(12%)8,            /* File Name                  */~
            filler$22,                   /* Spaces                     */~
            fld_lgth$2,                  /* Field Length               */~
            fld_name$8,                  /* Field Name                 */~
            fld_num$2,                   /* Field Number               */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(25%)1,                 /* Field Attribute Characters */~
            new$3,                       /* New Entry                  */~
            page$3,                      /* Page Number                */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            prt_num$2,                   /* Printed Field Number       */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            runtime$8,                   /* Time of Report             */~
            type$12,                     /* Type of Edit File          */~
            type_file$(12%)1,            /* File Type                  */~
            tot$3,                       /* Field Length Totals        */~
            userid$3,                    /* Current User Id            */~
            valid_yn$(12%)1,             /* Validate Y or N            */~
            valid_table$(12%)8           /* Validation Table Name      */~

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Part/Raw Material Definition Util."
            pname$ = "AMTBOMPM - Rev: R6.04"

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
            * #1  ! AMTBOMPM ! Bom Generator File                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AMTBOMPM",                                      ~
                        varc,     indexed,  recsize =  250,              ~
                        keypos = 1,    keylen = 2                        ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 25%, rslt$(1%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 15%
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
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then       delete_it
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     gosub set_fields
            if errormsg$ <> " " then goto L11170

            if fieldnr% < 1% or fieldnr% > 15% then editpg1
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
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            if f1%(1) = 1% then delete #1
            write #1 using L30100, fld_num$, fld_name$, fld_lgth$,        ~
                type_file$(), valid_yn$(), valid_table$(), file_name$(), ~
                filler$
            goto inputmode
        delete_it
            if f1%(1) <> 0% then call "DELETE" (#1, fld_num$, 2%)
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%

            return
        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Valid Fld-Man(1 thru 25), Ph(31 thru 55), R(61 thru 65)",~
         "Enter a Valid Field Name. ( Required Field )                 ",~
         "Enter Field Length. Max = 15                                 ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name ",~
         "TAB FIELDS - Type(S,T,C,P,R), Validate(Y/N), Table/File Name "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, filler$, type$,            ~
                      fld_lgth$, fld_name$, fld_num$, type_file$(),      ~
                      valid_yn$(), valid_table$(), file_name$()
            new$ = "Y"
            return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *                     L O A D   D A T A                     *~
            *************************************************************

        dataload
            call "READ101" (#1, fld_num$, f1%(1))
            if f1%(1) = 0% then goto L30140
            get #1 using L30100, fld_num$, fld_name$, fld_lgth$,          ~
                type_file$(), valid_yn$(), valid_table$(), file_name$(), ~
                filler$
L30100:         FMT CH(2), CH(8), CH(2), 12*CH(1), 12*CH(1),             ~
                    12*CH(8), 12*CH(8), CH(22)
            if edit% = 1%  then  fieldnr% = 15%
        return
L30140:     if f1%(1) = 0% then new$ = "Y" else new$ = "N"
        return

        REM *************************************************************~
            *                  P U T   D A T A                          *~
            *************************************************************

        dataput
            put #1 using L30100, fld_num$, fld_name$, fld_lgth$,          ~
                type_file$(), valid_yn$(), valid_table$(), file_name$(), ~
                filler$
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

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
              on  fieldnr% gosub L40280,       /* FIELD NUMBER     */     ~
                                 L40280,       /* FIELD NAME       */     ~
                                 L40280,       /* FIELD LENGTH     */     ~
                                 L40280,       /* TYPE OF FILE (4) */     ~
                                 L40280,       /* TYPE OF FILE (5) */     ~
                                 L40280,       /* TYPE OF FILE (6) */     ~
                                 L40280,       /* TYPE OF FILE (7) */     ~
                                 L40280,       /* TYPE OF FILE (8) */     ~
                                 L40280,       /* TYPE OF FILE (9) */     ~
                                 L40280,       /* TYPE OF FILE (10)*/     ~
                                 L40280,       /* TYPE OF FILE (11)*/     ~
                                 L40280,       /* TYPE OF FILE (12)*/     ~
                                 L40280,       /* TYPE OF FILE (13)*/     ~
                                 L40280,       /* TYPE OF FILE (14)*/     ~
                                 L40280        /* TYPE OF FILE (15)*/
         goto L40300
            lfac$(fieldnr%) = hex(80) : return     /* UPPER AN LOWER   */
L40280:     lfac$(fieldnr%) = hex(81) : return     /* UPPER ONLY       */
            lfac$(fieldnr%) = hex(82) : return     /* NUMERIC          */
L40300:

L40320:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Manfactured (1 thru 25)",                    ~
               at (04,28), "Phantom (32 thru 55)",  /*AWD001*/           ~
               at (04,50), "Raw Material (61 thru 85)",                  ~
                                                                         ~
               at (06,06), "------Field-------",                         ~
               at (06,43),"Type   Valid-Values   File",                  ~
               at (07,07),"Num Name     Lgth",                           ~
               at (07,43),"File    Y/N  Table    Name",                  ~
               at (08,07), fac(lfac$( 1)), fld_num$             , ch(02),~
               at (08,10), fac(lfac$( 2)), fld_name$            , ch(08),~
               at (08,21), fac(lfac$( 3)), fld_lgth$            , ch(02),~
               at (08,44), fac(lfac$( 4)), type_file$(1)        , ch(01),~
               at (08,52), fac(lfac$( 4)), valid_yn$(1)         , ch(01),~
               at (08,55), fac(lfac$( 4)), valid_table$(1)      , ch(08),~
               at (08,65), fac(lfac$( 4)), file_name$(1)        , ch(08),~
                                                                         ~
               at (09,44), fac(lfac$( 5)), type_file$(2)        , ch(01),~
               at (09,52), fac(lfac$( 5)), valid_yn$(2)         , ch(01),~
               at (09,55), fac(lfac$( 5)), valid_table$(2)      , ch(08),~
               at (09,65), fac(lfac$( 5)), file_name$(2)        , ch(08),~
                                                                         ~
               at (10,44), fac(lfac$( 6)), type_file$(3)        , ch(01),~
               at (10,52), fac(lfac$( 6)), valid_yn$(3)         , ch(01),~
               at (10,55), fac(lfac$( 6)), valid_table$(3)      , ch(08),~
               at (10,65), fac(lfac$( 6)), file_name$(3)        , ch(08),~
                                                                         ~
               at (11,44), fac(lfac$( 7)), type_file$(4)        , ch(01),~
               at (11,52), fac(lfac$( 7)), valid_yn$(4)         , ch(01),~
               at (11,55), fac(lfac$( 7)), valid_table$(4)      , ch(08),~
               at (11,65), fac(lfac$( 7)), file_name$(4)        , ch(08),~
                                                                         ~
               at (12,44), fac(lfac$( 8)), type_file$(5)        , ch(01),~
               at (12,52), fac(lfac$( 8)), valid_yn$(5)         , ch(01),~
               at (12,55), fac(lfac$( 8)), valid_table$(5)      , ch(08),~
               at (12,65), fac(lfac$( 8)), file_name$(5)        , ch(08),~
                                                                         ~
               at (13,44), fac(lfac$( 9)), type_file$(6)        , ch(01),~
               at (13,52), fac(lfac$( 9)), valid_yn$(6)         , ch(01),~
               at (13,55), fac(lfac$( 9)), valid_table$(6)      , ch(08),~
               at (13,65), fac(lfac$( 9)), file_name$(6)        , ch(08),~
                                                                         ~
               at (14,44), fac(lfac$( 10)), type_file$(7)       , ch(01),~
               at (14,52), fac(lfac$( 10)), valid_yn$(7)        , ch(01),~
               at (14,55), fac(lfac$( 10)), valid_table$(7)     , ch(08),~
               at (14,65), fac(lfac$( 10)), file_name$(7)       , ch(08),~
                                                                         ~
               at (15,44), fac(lfac$( 11)), type_file$(8)       , ch(01),~
               at (15,52), fac(lfac$( 11)), valid_yn$(8)        , ch(01),~
               at (15,55), fac(lfac$( 11)), valid_table$(8)     , ch(08),~
               at (15,65), fac(lfac$( 11)), file_name$(8)       , ch(08),~
                                                                         ~
               at (16,44), fac(lfac$( 12)), type_file$(9)       , ch(01),~
               at (16,52), fac(lfac$( 12)), valid_yn$(9)        , ch(01),~
               at (16,55), fac(lfac$( 12)), valid_table$(9)     , ch(08),~
               at (16,65), fac(lfac$( 12)), file_name$(9)       , ch(08),~
                                                                         ~
               at (17,44), fac(lfac$(13)), type_file$(10)       , ch(01),~
               at (17,52), fac(lfac$(13)), valid_yn$(10)        , ch(01),~
               at (17,55), fac(lfac$(13)), valid_table$(10)     , ch(08),~
               at (17,65), fac(lfac$(13)), file_name$(10)       , ch(08),~
                                                                         ~
               at (18,44), fac(lfac$(14)), type_file$(11)       , ch(01),~
               at (18,52), fac(lfac$(14)), valid_yn$(11)        , ch(01),~
               at (18,55), fac(lfac$(14)), valid_table$(11)     , ch(08),~
               at (18,65), fac(lfac$(14)), file_name$(11)       , ch(08),~
                                                                         ~
               at (19,44), fac(lfac$(15)), type_file$(12)       , ch(01),~
               at (19,52), fac(lfac$(15)), valid_yn$(12)        , ch(01),~
               at (19,55), fac(lfac$(15)), valid_table$(12)     , ch(08),~
               at (19,65), fac(lfac$(15)), file_name$(12)       , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 14 then L41200
                  gosub print_report : goto inputmode
L41200:
               if keyhit% <> 15 then L41260
                  call "PRNTSCRN" : goto L40320

L41260:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41450     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Report      "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L41410
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41410:     if fieldnr% > 1% then L41430
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41430:     return

L41450: if fieldnr% > 0% then L41540  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "(12)Delete             (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            return
L41540:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "(12)Delete             (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "

              on  fieldnr% gosub L50260,       /* FIELD NUMBER     */     ~
                                 L50360,       /* FIELD NAME       */     ~
                                 L50410,       /* FIELD LENGTH     */     ~
                                 L50490,       /* TYPE OF FILE (4) */     ~
                                 L50490,       /* TYPE OF FILE (5) */     ~
                                 L50490,       /* TYPE OF FILE (6) */     ~
                                 L50490,       /* TYPE OF FILE (7) */     ~
                                 L50490,       /* TYPE OF FILE (8) */     ~
                                 L50490,       /* TYPE OF FILE (9) */     ~
                                 L50490,       /* TYPE OF FILE (10)*/     ~
                                 L50490,       /* TYPE OF FILE (11)*/     ~
                                 L50490,       /* TYPE OF FILE (12)*/     ~
                                 L50490,       /* TYPE OF FILE (13)*/     ~
                                 L50490,       /* TYPE OF FILE (14)*/     ~
                                 L50490        /* TYPE OF FILE (15)*/
         return

L50260: REM Test for Field Number                 FLD_NUM$
            fld_num% = 0%
            convert fld_num$ to fld_num%, data goto L50330
            convert fld_num% to fld_num$, pic(00)
            if fld_num% < 86% then goto L50320
               errormsg$ = " Invalid Entry - Valid Entries (01 thru 85)"
L50320:
L50330:     gosub dataload
            return

L50360: REM Test for Field Name                   FLD_NAME$
            if fld_name$ <> " " then return
            errormsg$ = " Field Name is Required. "
            return

L50410: REM Test for Field Length                 FLD_LGTH$
            convert fld_lgth$ to fld_lgth%, data goto L50450
            convert fld_lgth% to fld_lgth$, pic(00)
            if fld_lgth% > 0% and fld_lgth% < 16% then goto L50470
L50450:     errormsg$ = "Maximum Field Length is 15"

L50470:     return

L50490: REM Test for Type of File                 TYPE_FILE$()
            calc_fld% = fieldnr% - 3%    
         /* CALCULATED FIELD */

            if type_file$(calc_fld%) <> " " then goto L50570
               if fieldnr% < 5% then goto L50570
                  fieldnr% = 15%
                  return

L50570:   REM  if fld_num% > 30% then goto L50690        /* (AWD001) */
          REM     if fld_num% < 26% then goto L50620
            if fld_num% > 31% then goto L50690
               if fld_num% < 32% then goto L50620
                  errormsg$ = "Invalid Field Number for Manufactured Part"
                  return

L50620:        if type_file$(calc_fld%) = "S" or                         ~
                  type_file$(calc_fld%) = "T" or                         ~
                  type_file$(calc_fld%) = "C" then goto L50880

               errormsg$ = "Invalid Entry - (S)creen, (T)able, (C)alc."
               return

L50690:     if fld_num% > 60% then goto L50790
               if fld_num% < 56% then goto L50740
                  errormsg$ = "Invalid Field Number for Phantom Part No."
                  return

L50740:        if type_file$(calc_fld%) = "P" then goto L50880

               errormsg$ = "Invalid Entry - Valid Entry (P)hantom."
               return

L50790:        if fld_num% < 86% then goto L50830
                  errormsg$ = "Invalid Field Number for Raw Material No."
                  return

L50830:        if type_file$(calc_fld%) = "R" then goto L50880

               errormsg$ = "Invalid Entry - Valid Entry (R)aw Material."
               return

L50880:     if type_file$(calc_fld%) = "C" then valid_yn$(calc_fld%) = "N"
            if type_file$(calc_fld%) = "S" then valid_yn$(calc_fld%) = "N"
            if type_file$(calc_fld%) = "R" then valid_yn$(calc_fld%) = "N"
            if type_file$(calc_fld%) = "P" then valid_yn$(calc_fld%) = "N"
            gosub L51000
            if errormsg$ <> " " then return
            gosub L51070
            if errormsg$ <> " " then return
            gosub L51150

            return

L51000: REM Test for Validate                     VALID_YN$()
            if valid_yn$(calc_fld%) <> "Y" and                           ~
               valid_yn$(calc_fld%) <> "N" then                          ~
               errormsg$ = "Answer 'Y' to Validate the choices of " &    ~
                 fld_name$ & " or 'N' if any choice is valid"
            return

L51070: REM Test for Validation File              VALID_TABLE$()
            if valid_yn$(calc_fld%) <> "Y" then                          ~
            valid_table$(calc_fld%) = " "
            if valid_yn$(calc_fld%) = "Y" and valid_table$(calc_fld%)    ~
                = " " then errormsg$ = "If " & fld_name$ &               ~
                " is Validated, what's the Validation Table Name"
            return

L51150: REM Test for File Name                    FILE_NAME$()
            if type_file$(calc_fld%) <> "P" and type_file$(calc_fld%)    ~
                <> "R" then goto L51180 else goto L51280
L51180:     if valid_yn$(calc_fld%) = "Y" then goto L51280
            if file_name$(calc_fld%) <> " " then goto L51280
            errormsg$ = "What is the"
            if type_file$(calc_fld%) = "S" then type$ = " Screen"
            if type_file$(calc_fld%) = "T" then type$ = " Table"
            if type_file$(calc_fld%) = "C" then type$ = " Calculation"
            if type_file$(calc_fld%) <> "T" then                         ~
                            errormsg$ = errormsg$ & type$ & " File Name" ~
              else errormsg$ = errormsg$ & " Name of the File containing"~
                                        & " ALL Choices of " & fld_name$
L51280:     return

        print_report
            rpt_hdr$ = "Part No. Parameters "
            call "SHOSTAT" ("Printing Report")
            call "TIME" (runtime$)
            select printer (134)
            page% = 0%
            fld_num$ = hex(00)
            tot% = 0%
            lcntr% = 99%
L60080:     call "READ102" (#1%, fld_num$, f1%(1))
            if f1%(1) = 0% then goto report_end
            get #1 using L30100, fld_num$, fld_name$, fld_lgth$,          ~
                type_file$(), valid_yn$(), valid_table$(), file_name$(), ~
                filler$
            convert fld_num$ to field%, data goto L60200
                                                /* (AWD001)*/
REM            if fld_num$ = "31" then gosub phantoms
            if fld_num$ = "32" then gosub phantoms
            if fld_num$ = "61" then gosub raw_material
            if fld_num$ < "26" then goto L60190
            if fld_num$ > "30" and fld_num$ < "60" then field%=field%- 30%
            if fld_num$ > "60" and fld_num$ < "89" then field%=field%- 60%
L60190:     convert field% to prt_num$, pic(##)
L60200:     i% = 1%
            if lcntr% > 60% then gosub report_heading
            fld_lgth% = 0%
            if field% <> 1% then goto L60215
               pos% = 1% : x% = 0%

L60215:     convert fld_lgth$ to fld_lgth%, data goto L60216
L60216:

            pos% = pos% + x%
            convert pos% to pos$, pic(00)
            print : lcntr% = lcntr% + 1%
            print using L60790, prt_num$, fld_name$, fld_lgth$, pos$,     ~
                type_file$(i%), valid_yn$(i%), valid_table$(i%),         ~
                file_name$(i%)
            x% = fld_lgth%
            tot% = tot% + fld_lgth%
L60280:     lcntr% = lcntr% + 1%
            if lcntr% > 60% then gosub report_heading
L60300:     i% = i% + 1%
            if i% < 12% then goto L60330
            goto L60080
L60330:     if type_file$(i%) <> " " or valid_yn$(i%) <> " " or          ~
               valid_table$(i%) <> " " or file_name$(i%) <> " "          ~
                     then goto L60370
            goto L60300
L60370:     print using L60800, type_file$(i%), valid_yn$(i%),            ~
                valid_table$(i%), file_name$(i%)
            goto L60280

        report_heading
            page% = page% + 1%
            convert page% to page$, pic(###)
            print page
            print using L60760, date$, runtime$, page$
            print
            print using L60810, rpt_hdr$
            print
            print
            print using L60770
            print using L60780
            lcntr% = 7%
            return

        report_end
            convert tot% to tot$, pic(###)
            print using L60820, tot$
            close printer
        return

        raw_material
            convert tot% to tot$, pic(###)
            tot% = 0%
            print using L60820, tot$
            rpt_hdr$ = "Raw Mat. Parameters "
            gosub report_heading
            return


        phantoms
            convert tot% to tot$, pic(###)
            tot% = 0%
            print using L60820, tot$
            rpt_hdr$ = " Phantom Parameters "
            gosub report_heading
            return


L60760: %Runtime: ######## @ ########                           Page: ###
L60770: %     +------Field------+ Field  File  +--Validate-+    File
L60780: %     Num  Name      Lgth  Pos   Type    Y/N Table      Name
L60790: %     ##   ########   ##    ##     #      #  ########   ########
L60800: %                                  #      #  ########   ########
L60810: %                         (####################)
L60820: %Total Field Lengths ###

        set_fields                       /* Set Field for Field Edit */
           errormsg$ = " "
           row% = cursor%(1%)
           col% = cursor%(2%)
           if row% > 7% and row% <  20% then goto L60920
              errormsg$ = "Invalid Cursor Position."
              return

L60920:    if row% > 8% then goto L60980
              if col% =  2% then fieldnr% = 1%
              if col% = 10% then fieldnr% = 2%
              if col% = 21% then fieldnr% = 3%
              if col% > 21% then fieldnr% = 4%
              return
L60980:
           fieldnr% = row% - 4%
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
