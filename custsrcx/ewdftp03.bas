        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDFTP03                             *~
            *  Creation Date     - 07/18/01                             *~
            *  Last Modified Date-                                      *~
            *  Description       - This Program Allows you to Input     *~
            *                      a Customer and Invoice No. and       *~
            *                      put the approp. Send Info Into the   *~
            *                      EDI Master Control File (EWDFTPMR)   *~
            *                      for Re-Transmiting an Invoice.       *~
            *                                                           *~
            *  Code Tables Used  - (XXXXX XXX) - Code Table             *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/18/01 ! New Program for (EWD) -                  ! CMG *~
            *************************************************************

        dim                                                              ~
            edi_cust$9, edi_cust_nam$30, /* CUSTOMER CODE              */~
            edi_inv$8, sku_code$3,       /* INVOICE NUMBER             */~
            edi_seq$3,                   /* LINE ITEM SEQUENCE NUMBER  */~
            terms_disc$6,                /* Terms Discount Date        */~
            terms_net$6,                 /* Terms Net Date             */~
            ar_key$17, ar_key1$20,       /* ARIMASTR, ARILINES         */~
            edi_key$22, sav_key$20,      /* EWDFTPMR - KEY's           */~
            readkey$24, desc$32,         /* GENCODES Key               */~
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

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97 APC EDI Re-Trans. Invoice (Y2K)"
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
            * #1  ! ARIMASTR ! Master Invoice Header File               *~
            * #2  ! ARILINES ! Master Invoice Line Items                *~
            * #3  ! CUSTOMER ! Master Customer File                     *~
            * #4  ! GENCODES ! System Master Code Tables                *~
            * #5  ! BCKMASTR ! Sales Order Header File                  *~
            * #6  ! EWDFTPMR ! EDI Master Control File Norandex         *~
            * #7  ! APCSKUNO ! Sku Number Master File                   *~
            * #8  ! ARMTERMS ! AR Invoice Terms Def. File               *~
            * #9  ! HNYMASTR ! Master Inventory File                    *~  
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize =  2000,             ~
                        keypos =  1,   keylen =  17,                     ~
                        alt key  1, keypos  =    10, keylen =  8, dup,   ~
                            key  2, keypos  =    18, keylen = 16, dup,   ~
                            key  3, keypos  =    34, keylen = 16, dup,   ~
                            key  4, keypos  =  1783, keylen = 26, dup  

            select #2,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =   750,             ~
                        keypos =  1,   keylen =  20

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #5,  "BCKLINES",                                      ~
                        varc, indexed, recsize =  300,                   ~
                        keypos = 10, keylen = 19

            select #6,  "EWDFTPMR",                                      ~
                        varc,     indexed,  recsize = 224,               ~
                        keypos =   7 , keylen =  22,                     ~
                        alt key    1 , keypos =   1, keylen =  28

            select #7,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =  1,   keylen =  32                        

            select #8,  "TXTFILE",                                       ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 1, keylen =  11

            select #9,  "HNYMASTR",                                      ~
                        varc, indexed, recsize =  900,                   ~
                        keypos = 1, keylen =  25,                        ~
                        alt key  1, keypos = 102, keylen = 9, dup,       ~
                            key  2, keypos =  90, keylen = 4, dup



            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1,  fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5,  fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6,  fs%(6%), f2%(6%),100%, rslt$(6%))
            call "OPENCHCK" (#7,  fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#8,  fs%(8%), f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9,  fs%(9%), f2%(9%),  0%, rslt$(9%))
                                                   
            mat f1% = zer
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
            for fieldnr% = 1% to  2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10220
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10220:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 16% then gosub create_edi
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        create_edi
            call "SHOSTAT" ("Purging Old Invoice Data")
            gosub purge_old
            call "SHOSTAT" ("Creating Invoice Line Data")
            gosub process_line
REM            call "SHOSTAT" ("Creating Invoice Header Data")
REM            gosub process_header
        return clear all
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
         "Enter a Valid APC Customer Code Assoc. With Invoice?         ",~
         "Enter a Valid APC Invoice Number?                            "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, desc$,           ~
                      edi_cust$, edi_cust_nam$, edi_inv$,                ~
                      edi_seq$, ar_key$, ar_key1$,                       ~  
                      edi_key$, sav_key$, terms_disc$, terms_net$,       ~
                      sku_code$
            return

        REM *************************************************************~
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
        REM DATALOAD
        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        dataput
        call "EWDFTP01" (#1,               /* ARIBUFFR File - Headers */ ~
                        #2,                /* ARIBUFF2 File - Details */ ~
                        #3,                /* CUSTOMER Master File    */ ~
                        #4,                /* GENCODES File           */ ~
                        #5,                /* BCKLINES File           */ ~
                        #6,                /* EWDFTPMR File           */ ~
                        #9,                /* HNYMASTR File           */ ~ 
                        #7,                /* AMTBOMIF File           */ ~
                        #8,                /* TXTFILE File            */ ~
                        edi_cust$,         /* Customer Number         */ ~
                        edi_inv$,          /* Invoice Number          */ ~
                        edi_seq$)          /* Invoice Sequence Number */  
        return

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
              on fieldnr% gosub L40170,         /* Customer Code     */   ~
                                L40170          /* Invoice Number    */

              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "EWD EDI Re-Transmit Invoice Utility - Norandex",      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Customer Code:",                             ~
               at (06,20), fac(lfac$(1%)), edi_cust$            , ch(09),~
               at (06,40), fac(hex(84)),   edi_cust_nam$        , ch(30),~
               at (07,02), "Invoice No.  :",                             ~
               at (07,20), fac(lfac$(2%)), edi_inv$             , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40440
                  call "PRNTSCRN"
                  goto L40200

L40440:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40630     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40590
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40590:     if fieldnr% > 1% then L40610
                str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40610:     return

L40630: if fieldnr% > 0% then L40720  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Create Edi  "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L40720:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
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
            on fieldnr% gosub L50120,         /* Customer Code         */ ~
                              L50300          /* Invoice Number        */
            return

L50120: REM APC Customer Code                     EDI_CUST$
           if edi_cust$ <> " " then goto L50180
              edi_cust_nam$ = hex(06) & "Select Customer Code"
          call "PLOWCODE" (#3, edi_cust$,edi_cust_nam$, 0%, .30, f1%(3))
              if f1%(3) = 0 then goto L50230
              goto L50190
L50180:     read #3,key = edi_cust$, eod goto L50230
L50190:        get #3, using L50200, edi_cust_nam$, sku_code$
L50200:       FMT POS(10), CH(30), POS(1000), CH(3)
            if len(sku_code$) < 3 then goto L50260
        return
L50230:     errormsg$ = "(Error) - Invalid Customer Code? Required"
            init(" ")  edi_cust$, edi_cust_nam$, sku_code$
        return
L50260:     errormsg$ = "(Error) - Customer Not Defined for EDI?"
            init(" ")  edi_cust$, edi_cust_nam$, sku_code$
        return

L50300: REM APC Invoice Number                    EDI_INV$
           init(" ") ar_key$
           str(ar_key$,1%,9%)  = edi_cust$
           str(ar_key$,10%,8%) = edi_inv$
           read #1,key = ar_key$, eod goto L50360
        return
L50360:    errormsg$ = "(Error) - Invalid Invoice Number ?"
           init(" ") edi_inv$, ar_key$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        purge_old                        /* Clear all Data for Invoice */
            pass% = 0%
            edi_key$, sav_key$ = " "
            str(edi_key$,1%,1%) = "S"     
            str(edi_key$,2%,1%) = "I"
            str(edi_key$,3%,9%) = edi_cust$
            str(edi_key$,12%,8%) = edi_inv$
            str(sav_key$,1%,19%) = str(edi_key$,1%,19%)
        purge_next
            read #6,hold,key  > edi_key$,using L60100, edi_key$,        ~
                                                      eod goto purge_done
L60100:        FMT POS(7), CH(22)
            if str(edi_key$,1%,19%) <> str(sav_key$,1%,19%) then goto purge_done
               delete #6
               goto purge_next
        purge_done
           if pass% <> 0% then return
            edi_key$, sav_key$ = " "
            str(edi_key$,1%,1%) = "T"     
            str(edi_key$,2%,1%) = "I"
            str(edi_key$,3%,9%) = edi_cust$
            str(edi_key$,12%,8%) = edi_inv$
            str(sav_key$,1%,19%) = str(edi_key$,1%,19%)
            pass% = 1%
            goto purge_next


        process_line
            init(" ") ar_key1$ , sav_key$
            str(ar_key1$,1%,9%)  = edi_cust$
            str(ar_key1$,10%,8%) = edi_inv$
        process_line_next
            read #2,key > ar_key1$, using L60230, sav_key$, eod goto L60330
L60230:        FMT CH(20)
            if str(sav_key$,1%,17%) <> str(ar_key$,1%,17%) then          ~
                                              goto process_line_done
            ar_key1$ = sav_key$
            edi_seq$ = str(sav_key$,18%,3%)
            gosub dataput
            goto process_line_next
L60330: process_line_done
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
