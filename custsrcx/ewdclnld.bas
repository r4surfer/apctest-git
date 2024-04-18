        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDCLNLD                             *~
            *  Creation Date     - 01/07/02                             *~
            *  Last Modified Date- 04/05/05                             *~
            *  Written By        - Christie M. Gregory                  *~
            *  Modifications By  -                                      *~ 
            *                                                           *~
            *  Description       - Master Purge Utility for resetting   *~
            *                      load numbers and Stock Sales Order No*~
            *                                                           *~
            *  DATABASE Files    - (1) APCPLNOR                         *~
            *                      (2) APCPLNSC                         *~
            *                      (3) APCPLNLD                         *~
            *                      (4) APCPLNDT                         *~
            *                      (5) APCPLNSD                         *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/07/02 ! (New) Program -                          ! CMG *~
            * 04/05/05 ! (AWD001) - Mod to reclen of APCPLNSD     ! CMG *~
            * 06/15/21 ! (CR2848) - Add awdappld to purge         ! RDB *~
            *************************************************************

        dim                                                              ~
            beg_num$5, end_num$5,        /* Beg and End Numeric Load   */~
            beg_alp$5, end_alp$5,        /* Beg and End Alpha   Load   */~
            beg_sto$5, end_sto$5,        /* Beg and End Stock   Load   */~
            beg_sso$8, end_sso$8,        /* Beg and End Stock So Num   */~
            beg_aso$8, end_aso$8,        /* Beg and End So Num         */~            
            cnt$28,                      /* Counter                    */~
            purge_key$60,                /* For Purge Utilities        */~
            purge_end$60,                /* For End Purge Utilities    */~
            sel$(15%)1, sel_d$(15%)40,   /* Purge Selection Data       */~
            filename$8,                  /* Filename                   */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Master Load Re-set Utility  "
            pname$ = "EWDCLNLD - Rev: R7.20"

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNOR ! Lowe's Special Label Database            *~
            * #2  ! APCPLNSC ! Master DATABASE for Production Labels    *~
            * #3  ! APCPLNLD ! Store Associated P.O. Totals             *~
            * #4  ! APCPLNDT ! Master DATABASE for Glass Rack Labels    *~
            * #5  ! APCPLNSD ! Master Remake Label Database             *~
            * #6  ! AWDAPPLD ! Appian Load File                (CR2848) *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNOR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  170,                                  ~
                        keypos =     1, keylen = 51,                     ~
                        alt key  1, keypos =  27, keylen =  25,          ~
                            key  2, keypos =  70, keylen =   8, dup,     ~
                            key  3, keypos =  78, keylen =   8, dup,     ~
                            key  4, keypos =  52, keylen =   8,          ~
                            key  5, keypos =  36, keylen =  16, dup   

            select #2,  "APCPLNSC",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  128,                                  ~
                        keypos =    24, keylen = 10,                     ~
                        alt key  1, keypos =   7, keylen =  27,          ~
                            key  2, keypos =   1, keylen =  33 


            select #3,  "APCPLNLD",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  128,                                  ~
                        keypos =    11, keylen =  5,                     ~
                        alt key  1, keypos =   3, keylen =  13,          ~
                            key  2, keypos =   1, keylen =  15      

            select #4  "APCPLNDT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  256,                                  ~
                        keypos =    24, keylen = 23,                     ~
                        alt key  1, keypos =  47, keylen =  57,          ~
                            key  2, keypos =  53, keylen =  51,          ~
                            key  3, keypos =   1, keylen =  23,          ~
                            key  4, keypos =  96, keylen =   8           
/* (AWD001) - Mod to key and reclen */
            select #5,  "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =    1, keylen =  23
/* CR2848 */
            select #6, "AWDAPPLD",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =    5,                    ~
                        alt key 1,  keypos =  1,   keylen =  16,         ~
                            key 2,  keypos =  2,   keylen =  15,         ~
                            key 3,  keypos = 17,   keylen =  15

            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "APCPLNOR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPLD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            
        REM    call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),  0%,  rslt$(1%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            if userid$ <> "CMG" and userid$ <> "RHH" and userid$ <> "CG1"  ~
               and userid$ <> "RDB" and userid$ <> "RBN"                   ~
                    then goto exit_program

            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            ss$(1%)  = "Enter Beginning and Ending Numeric Load "
            ss$(2%)  = "Enter Beginning and Ending Alpha   Load "
            ss$(3%)  = "Enter Beginning and Ending Stock   Load "
            ss$(4%)  = "Enter Beginning and Ending Stock So Num "
            ss$(5%)  = "Enter Beginning and Ending So Num       " 
            
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:               gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 14% then gosub purge_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  5% then editpg1
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
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
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

        scrn1_msg  :  data                                                   ~
         "Enter Beginning (One Less - Read Greater Than) and Ending Numeric Load ?",~
         "Enter Beginning (One Less - Read Greater Than) and Ending Alpha   Load ?",~
         "Enter Beginning (One Less - Read Greater Than) and Ending Stock   Load ?",~
         "Enter Beginning (One Less - Read Greater Than) and Ending Stock So Num ?",~
         "Enter Beginning (One Less - Read Greater Than) and Ending So Num ?      "
         
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
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sel$(), sel_d$(),          ~
                      purge_key$, beg_num$, end_num$, beg_alp$, end_alp$,~
                      beg_sto$, end_sto$, beg_sso$, end_sso$, purge_end$,~
                      beg_aso$, end_aso$
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
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
              gosub L40170

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,23), fac(hex(a4)), apc$                   , ch(35),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Beginning & Ending Num  :",                  ~
               at (05,30), fac(lfac$(1%)), beg_num$             , ch(05),~
               at (05,45), fac(lfac$(1%)), end_num$             , ch(05),~
                                                                         ~   
               at (06,02), "Beginning & Ending Alpha:",                  ~
               at (06,30), fac(lfac$(2%)), beg_alp$             , ch(05),~
               at (06,45), fac(lfac$(2%)), end_alp$             , ch(05),~
                                                                         ~   
               at (07,02), "Beginning & Ending Stock:",                  ~
               at (07,30), fac(lfac$(3%)), beg_sto$             , ch(05),~
               at (07,45), fac(lfac$(3%)), end_sto$             , ch(05),~
                                                                         ~   
               at (08,02), "Beginning & Ending St SO:",                  ~
               at (08,30), fac(lfac$(4%)), beg_sso$             , ch(08),~
               at (08,45), fac(lfac$(4%)), end_sso$             , ch(08),~
                                                                         ~   
               at (09,02), "Beginning & Ending A Load SO:",              ~
               at (09,35), fac(lfac$(5%)), beg_aso$             , ch(08),~
               at (09,50), fac(lfac$(5%)), end_aso$             , ch(08),~
                                                                         ~ 
                                                                         ~   
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

 
               if keyhit% <> 15 then goto L40200
                  call "PRNTSCRN"
                  goto L40190

L40200: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

set_pf1:
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                      (14)Purge Data   "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)

            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50010,       /* Beg & End Numeric Load */ ~
                              L50100,       /* Beg & End Alpha Load   */ ~
                              L50200,       /* Beg & End Stock Load   */ ~
                              L50300,       /* Beg & End Stock So Num */ ~
                              L50400        /* Beg & End SO Num A Load*/

            return

L50010: REM Beg & End Numeric Load            beg_num$, end_num$
            beg_num%, end_num% = 0%
            if beg_num$ <> " " and end_num$ <> " " then goto L50020
               beg_num$, end_num$ = "NA   "
            return

L50020:     convert beg_num$ to beg_num%, data goto L50030

            convert end_num$ to end_num%, data goto L50030

            convert beg_num% to str(beg_num$,1%,5%), pic(00000)

            convert end_num% to str(end_num$,1%,5%), pic(00000)
        return
L50030:     errormsg$ = "(Error) Numeric Load Number?"
            gosub error_prompt
            init(" ") beg_num$, end_num$
        return

L50100: REM Beg & End Alpha   Load            beg_alp$, end_alp$
            beg_alp%, end_alp% = 0%
            if beg_alp$ <> " " and end_alp$ <> " " then goto L50120
               beg_alp$, end_alp$ = "NA   "
            return

L50120:     convert str(beg_alp$,2%,4%) to beg_alp%, data goto L50130

            convert str(end_alp$,2%,4%) to end_alp%, data goto L50130

            convert beg_alp% to str(beg_alp$,2%,4%), pic(0000)

            convert end_alp% to str(end_alp$,2%,4%), pic(0000)

            if str(beg_alp$,1%,1%) <> "A" or str(end_alp$,1%,1%) <> "A" ~
                         then goto L50130
        return
L50130:     errormsg$ = "(Error) Alpha Load Number?"
            gosub error_prompt
            init(" ") beg_alp$, end_alp$
        return

L50200: REM Beg & End Stock   Load            beg_sto$, end_sto$
            beg_sto%, end_sto% = 0%
            if beg_sto$ <> " " and end_sto$ <> " " then goto L50220
               beg_sto$, end_sto$ = "NA   "
            return

L50220:     convert str(beg_sto$,2%,4%) to beg_sto%, data goto L50230

            convert str(end_sto$,2%,4%) to end_sto%, data goto L50230

            convert beg_sto% to str(beg_sto$,2%,4%), pic(0000)

            convert end_sto% to str(end_sto$,2%,4%), pic(0000)

            if str(beg_sto$,1%,1%) <> "S" or str(end_sto$,1%,1%) <> "S" ~
                         then goto L50230
        return
L50230:     errormsg$ = "(Error) Stock Load Number?"
            gosub error_prompt
            init(" ") beg_sto$, end_sto$
        return

L50300: REM Beg & End Stock Sales Order Num   beg_sso$, end_sso$
            beg_sso%, end_sso% = 0%
            if beg_sso$ <> " " and end_sso$ <> " " then goto L50320
               beg_sso$, end_sso$ = "NA      "
            return

L50320:     convert str(beg_sso$,2%,7%) to beg_sso%, data goto L50330

            convert str(end_sso$,2%,7%) to end_sso%, data goto L50330

            convert beg_sso% to str(beg_sso$,2%,7%), pic(0000000)

            convert end_sso% to str(end_sso$,2%,7%), pic(0000000)

            if str(beg_sso$,1%,1%) <> "S" or str(end_sso$,1%,1%) <> "S" ~
                         then goto L50330
        return
L50330:     errormsg$ = "(Error) Stock Sales Order Number?"
            gosub error_prompt
            init(" ") beg_sso$, end_sso$
        return

L50400: REM Beg & End Sales Order Num   beg_aso$, end_aso$
/* CR2848 */
            if beg_aso$ <> " " and end_aso$ <> " " then goto L50420
               beg_aso$, end_aso$ = "NA      "
            return
L50420:
        return

   
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        purge_data
            cnt% = 0%
            cnt$ = "Records Deleted = [xxxxxxxx]"
            if str(beg_num$,1%,2%)  <>  "NA" and                       ~
               str(end_num$,1%,2%)  <>  "NA" then gosub purge_numeric
            if str(beg_alp$,1%,2%)  <>  "NA" and                       ~
               str(end_alp$,1%,2%)  <>  "NA" then gosub purge_alpha
            if str(beg_sto$,1%,2%)  <>  "NA" and                       ~
               str(end_sto$,1%,2%)  <>  "NA" then gosub purge_stock
            if str(beg_sso$,1%,2%)  <>  "NA" and                       ~
               str(end_sso$,1%,2%)  <>  "NA" then gosub purge_stock_so
/* CR2848 */               
            if str(beg_aso$,1%,2%)  <>  "NA" and                       ~
               str(end_aso$,1%,2%)  <>  "NA" then gosub purge_aload_so
            gosub initialize_variables
        return clear all
        goto editpg1


        purge_numeric
            purge_key$ = all(hex(00))
            str(purge_key$,1%,5%) = beg_num$
            str(purge_end$,1%,5%) = end_num$
            gosub purge_apcplnsc
            
            purge_key$ = all(hex(00))
            str(purge_key$,1%,5%) = beg_num$
            gosub purge_apcplnld

            purge_key$ = all(hex(00))
            str(purge_key$,1%,5%) = beg_num$
            gosub purge_apcplndt

        return

        purge_alpha
            purge_key$ = all(hex(00))
            str(purge_key$,1%,5%) = beg_alp$
            str(purge_end$,1%,5%) = end_alp$
            gosub purge_apcplnsc
            
            purge_key$ = all(hex(00))
            str(purge_key$,1%,5%) = beg_alp$
            gosub purge_apcplnld

            purge_key$ = all(hex(00))
            str(purge_key$,1%,5%) = beg_alp$
            gosub purge_apcplndt
/* CR2848 */            
            purge_key$ = all(hex(00))
            str(purge_key$,1%,5%) = beg_alp$
            gosub purge_awdappld
            
        return

        purge_stock
            purge_key$ = all(hex(00))
            str(purge_key$,1%,5%) = beg_sto$
            str(purge_end$,1%,5%) = end_sto$
            gosub purge_apcplnsc
            
            purge_key$ = all(hex(00))
            str(purge_key$,1%,5%) = beg_sto$
            gosub purge_apcplnld

            purge_key$ = all(hex(00))
            str(purge_key$,1%,5%) = beg_sto$
            gosub purge_apcplndt

        return

        purge_stock_so
            purge_key$ = all(hex(00))
            str(purge_key$,1%,8%) = beg_sto$
            str(purge_end$,1%,8%) = end_sto$
            gosub purge_apcplnor

            purge_key$ = all(hex(00))
            str(purge_key$,1%,8%) = beg_sto$
            str(purge_key$,9%,2%) = "99"
            gosub purge_apcplnsc_stock
            
            purge_key$ = all(hex(00))
            str(purge_key$,1%,8%) = beg_sto$
            str(purge_key$,9%,2%) = "99"
            gosub purge_apcplndt_stock

            purge_key$ = all(hex(00))
            str(purge_key$,1%,8%) = beg_sto$
            str(purge_key$,9%,2%) = "99"
            gosub purge_apcplnsd
        return

/* CR2848 + */
        purge_aload_so
            purge_key$ = all(hex(00))
            str(purge_key$,1%,8%) = beg_aso$
            str(purge_end$,1%,8%) = end_aso$
            gosub purge_aload_apcplnor

        return
        
        purge_aload_apcplnor
            call "SHOSTAT" ("Purging APCPLNOR Data ")
            
            str(purge_key$,1%,8%) = beg_aso$
            
        purge_aload_apcplnor_nxt 
            read #1,hold,key 4% > purge_key$, using L51000,          ~
                                  purge_key$, or_load$,              ~           
                        eod goto purge_aload_apcplnor_done
L51000:        FMT POS(52), CH(8), POS(94), CH(5)
            if mod(cnt%,50%) <> 0 then goto L51010
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(03,26);hex(84);cnt$;
                                               
L51010:     if str(purge_key$,1%,8%) > str(purge_end$,1%,8%) then    ~
                 goto purge_aload_apcplnor_done
                                                
            if str(or_load$,1%,1%) <> "A" then goto purge_aload_apcplnor_nxt

            call "SHOSTAT" (" DELETE KEY OR ==>  " & str(purge_key$,1%,8%))

                delete #1
                cnt% = cnt% + 1% 

        goto purge_aload_apcplnor_nxt
        purge_aload_apcplnor_done
           sel_d$(1%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(1%),21%,8%), pic(########)

        return
/* CR2848 - */        
        
        purge_apcplnor
            call "SHOSTAT" ("Purging APCPLNOR Data ")
            
            str(purge_key$,1%,8%) = beg_sso$
            
        purge_apcplnor_nxt
            read #1,hold,key 4% > purge_key$, using L60000,          ~
                                  purge_key$, eod goto purge_apcplnor_done
L60000:        FMT POS(52), CH(8)
            if mod(cnt%,50%) <> 0 then goto L60010
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(03,26);hex(84);cnt$;
                                                /* Only Keep (14) Days */
L60010:     if str(purge_key$,1%,8%) > str(purge_end$,1%,8%) then    ~
                                                goto purge_apcplnor_done

            call "SHOSTAT" (" DELETE KEY OR ==>  " & str(purge_key$,1%,5%))

                delete #1
                cnt% = cnt% + 1% 

        goto purge_apcplnor_nxt
        purge_apcplnor_done
           sel_d$(1%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(1%),21%,8%), pic(########)

        return

        purge_apcplnsc   
            call "SHOSTAT" ("Purging APCPLNSC Data")
        purge_apcplnsc_nxt
            read #2, hold, key 1% > purge_key$, using L60100,        ~
                                 purge_key$, eod goto purge_apcplnsc_done

L60100:        FMT POS(7), CH(27)

               if mod(cnt%,50%) <> 0 then goto L60110
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(03,26);hex(84);cnt$;
L60110:
               if str(purge_key$,1%,5%) > str(purge_end$,1%,5%) then ~
                                                 goto purge_apcplnsc_done

               call "SHOSTAT" (" DELETE KEY SC ==>  " & str(purge_key$,1%,5%))
               
               delete #2
               cnt% = cnt% + 1%       

        goto purge_apcplnsc_nxt
        purge_apcplnsc_done
           sel_d$(1%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(1%),21%,8%), pic(########)

        return

        purge_apcplnld
            call "SHOSTAT" ("Purging APCPLNLD Data")
        purge_apcplnld_nxt
            read #3, hold, key > purge_key$, using L60200,           ~
                                 purge_key$, eod goto purge_apcplnld_done

L60200:        FMT POS(11), CH(5)

               cnt% = cnt% + 1%
               if mod(cnt%,50%) <> 0 then goto L60210
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(03,26);hex(84);cnt$;
L60210:
               if str(purge_key$,1%,5%) > str(purge_end$,1%,5%) then ~
                                                 goto purge_apcplnld_done
               call "SHOSTAT" (" DELETE KEY LD ==>  " & str(purge_key$,1%,5%))
               
               delete #3

        goto purge_apcplnld_nxt
        purge_apcplnld_done
           sel_d$(1%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(1%),21%,8%), pic(########)

        return

        purge_apcplndt
            call "SHOSTAT" ("Purging APCPLNDT Data")
        purge_apcplndt_nxt
            read #4, hold, key 3% > purge_key$, using L60300,        ~
                                 purge_key$, eod goto purge_apcplndt_done

L60300:        FMT POS(1), CH(5)

               cnt% = cnt% + 1%
               if mod(cnt%,50%) <> 0 then goto L60310
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(03,26);hex(84);cnt$;
L60310:
               if str(purge_key$,1%,5%) > str(purge_end$,1%,5%) then ~
                                                 goto purge_apcplndt_done
               call "SHOSTAT" (" DELETE KEY DT ==>  " & str(purge_key$,1%,5%))
               
               delete #4

        goto purge_apcplndt_nxt
        purge_apcplndt_done
           sel_d$(1%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(1%),21%,8%), pic(########)

        return

        purge_apcplnsc_stock   
            call "SHOSTAT" ("Purging APCPLNSC STOCK Data")
        purge_apcplnsc_nxt_stock
            read #2, hold, key > purge_key$, using L60400,        ~
                                 purge_key$, eod goto purge_apcplnsc_stock_done

L60400:        FMT POS(24), CH(10)

               if mod(cnt%,50%) <> 0 then goto L60410
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(03,26);hex(84);cnt$;
L60410:
               if str(purge_key$,1%,8%) > str(purge_end$,1%,8%) then ~
                                                 goto purge_apcplnsc_stock_done

               call "SHOSTAT" (" DELETE KEY SC STOCK ==>  " & str(purge_key$,1%,5%))
               
               delete #2
               cnt% = cnt% + 1%       

        goto purge_apcplnsc_nxt_stock
        purge_apcplnsc_stock_done
           sel_d$(1%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(1%),21%,8%), pic(########)

        return

        purge_apcplndt_stock
            call "SHOSTAT" ("Purging APCPLNDT STOCK Data")
        purge_apcplndt_nxt_stock
            read #4, hold, key > purge_key$, using L60500,        ~
                                 purge_key$, eod goto purge_apcplndt_stock_done

L60500:        FMT POS(24), CH(23)

               cnt% = cnt% + 1%
               if mod(cnt%,50%) <> 0 then goto L60510
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(03,26);hex(84);cnt$;
L60510:
               if str(purge_key$,1%,8%) > str(purge_end$,1%,8%) then ~
                                                 goto purge_apcplndt_stock_done
               call "SHOSTAT" (" DELETE KEY DT STOCK ==>  " & str(purge_key$,1%,8%))
               
               delete #4

        goto purge_apcplndt_nxt_stock
        purge_apcplndt_stock_done
           sel_d$(1%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(1%),21%,8%), pic(########)

        return

        purge_apcplnsd
            call "SHOSTAT" ("Purging APCPLNSD Data")
        purge_apcplnsd_nxt
            read #5, hold, key > purge_key$, using L60600,        ~
                                 purge_key$, eod goto purge_apcplnsd_done

L60600:        FMT CH(23)

               cnt% = cnt% + 1%
               if mod(cnt%,50%) <> 0 then goto L60610
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(03,26);hex(84);cnt$;
L60610:
               if str(purge_key$,1%,8%) > str(purge_end$,1%,8%) then ~
                                                 goto purge_apcplnsd_done
               call "SHOSTAT" (" DELETE KEY SD ==>  " & str(purge_key$,1%,8%))
               
               delete #5

        goto purge_apcplnsd_nxt
        purge_apcplnsd_done
           sel_d$(6%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(6%),21%,8%), pic(########)

        return

REM--------------------------------------------------
REM Purge awdappld   CR2848
REM--------------------------------------------------
        purge_awdappld                            
            cnt6% = 0%
            
        purge_awdappld_nxt         
            read #6,hold,key > purge_key$, using L71000, purge_key$,~
                                                      eod goto L71200
L71000:        FMT POS(12), CH(05)  
            
            cnt6% = cnt6% + 1%
            if mod(cnt%,50%) <> 0 then goto L71610
            convert cnt% to str(cnt$,20%,8%), pic(########)
            print at(03,26);hex(84);cnt$;
L71610:
            if str(purge_key$,1%,8%) > str(purge_end$,1%,8%) then ~
                                                 goto purge_awdappld_done
            call "SHOSTAT" ("DELETE KEY AWDAPPLD ==>  " & str(purge_key$,1%,5%))
            
            delete #6
        goto purge_awdappld_nxt
        purge_awdappld_done
           sel_d$(1%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt6% to str(sel_d$(1%),21%,8%), pic(########)  
           call "SHOSTAT" (sel_d$(1%)) : STOP

L71200:     return
        
        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end


