        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN10                             *~
            *  Creation Date     - 05/06/96                             *~
            *  Last Modified Date- 11/14/97                             *~
            *  Written By        - J. Browning Fields                   *~
            *  Description       - This Program Rewrites the Region/    *~
            *                      Route code for APCPLNOR from         *~
            *                      CUSTOMER data.                       *~
            *                                                           *~
            *  Special Comments  - Select either Route, Region or both  *~
            *                      from CUSTOMER.  Status in APCPLNOR   *~
            *                      must be '00' or '01'.                *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/06/96 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 03/13/97 ! Mods to add logic for Updating the       ! RHH *~
            *          !   Customer Master with New Route Codes.  !     *~
            *          !   Also ad the logic for a Process        !     *~
            *          !   Selection.                             !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 12/20/18 ! CR1829 change cust len from 6 to 9 char  ! DES *~
            *************************************************************
        dim                                                              ~
            selection$1, sel_d$30,       /* SELECTION CODE AND DESCR   */~
            option$1, option_d$30,       /* PROCESS OPTION CODE 1 OR 2 */~
            or_region$2,                 /* Customer Region Code       */~
            or_route$5,                  /* Customer Route Code        */~
            or_cuscode$9,                /* Customer Number            */~
            or_status$2,                 /* APCPLNOR S.O. Status       */~
            or_rec$170,                  /* APCPLNOR Record            */~
            or_key$51,                   /* APCPLNOR Read Key          */~
            cus_region$2,                /* Customer Region            */~
            cus_route$5, xxx_route$5,    /* Customer Route             */~
            readkey$50, desc$,           /* GENCODES KEY               */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */~

        dim f2%(3%),                     /* = 0 if the file is open    */~
            f1%(3%),                     /* = 1 if READ was successful */~
            fs%(3%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(3%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Region/Route Re-Assign Utility    "
            pname$ = "APCPLN10 - Rev: R6.04"


        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNOR ! Planning S.O. Header History             *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! GENCODES ! CAELUS MASTER CODE TABLE FILE            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =  27, keylen =  25,          ~
                            key  2, keypos =  70, keylen =   8, dup,     ~
                            key  3, keypos =  78, keylen =   8, dup,     ~
                            key  4, keypos =  52, keylen =   8,          ~
                            key  5, keypos =  36, keylen =  16, dup

            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =    1, keylen =    9,                    ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62%) = "APCPLN10: " & "R6.04.03"
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
L10090:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then L10250

L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then L10220

L10160:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                               if enabled% = 1% then L10120
                               if fieldnr% = 1% then L10090

                          goto L10160
L10220:              if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0%                   then L10120

L10250:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10120

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
                if keyhit%  = 14% then gosub dataput
                if keyhit%  = 16% then exit_program
                if keyhit% <>  0% then editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% = 0%  then editpg1

L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  = 1%  then gosub startover
                if keyhit% <> 0%  then L11180

            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L11180
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
            if fieldnr% <> 0% then L28100
            inpmessage$ =  edtmessage$
        return

L28100
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter the Applicable Process Selection 1=Customers,2=Schd Ord",~
         "Enter Selection: (1)=Region's, (2)=Route's, (3)=Both?        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, selection$, or_key$,       ~
                      sel_d$, option$, option_d$, xxx_route$
        return

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
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            if option$ = "1" then goto dataput_customer
               if selection$ = "1" then                                  ~
                  call "SHOSTAT" ("Re-Assigning Region Codes")
               if selection$ = "2" then                                  ~
                  call "SHOSTAT" ("Re-Assigning Route Codes")
               if selection$ = "3" then                                  ~
                  call "SHOSTAT" ("Re-Assigning Region and Route Codes")

            or_key$ = all(hex(00))
        read_next
            read #1,key > or_key$, using L31170, or_rec$, eod goto L31340
L31170:         FMT CH(170)
               or_key$       = str(or_rec$, 1%,51%)
               or_status$    = str(or_rec$,60%, 2%)
               or_region$    = str(or_rec$, 9%, 2%)
               or_route$     = str(or_rec$,11%, 5%)
               or_cuscode$   = str(or_rec$,27%, 9%)
               if or_status$ <> "00" and or_status$ <> "01" then         ~
                                                           goto read_next
               gosub lookup_customer
               if rec% = 0% then goto read_next

               if selection$ = "1" or selection$ = "3" then              ~
                                                 or_region$ = cus_region$
               if selection$ = "2" or selection$ = "3" then              ~
                                                 or_route$  = cus_route$
               gosub update_or
            goto read_next
L31340: return clear all
        goto inputmode

        update_or
            read #1,hold,key = or_key$, eod goto L31480
                delete #1
            str(or_rec$,  9%,2%) = or_region$
            str(or_rec$, 11%,5%) = or_route$
            str(or_rec$,135%,8%) = date$
            str(or_rec$,143%,3%) = userid$
            put #1, using L31170, or_rec$
            write  #1, eod goto L31480
        return
L31480:     call "SHOSTAT" ("Error-Updating S.O. -- "&or_key$)
            stop
        return

        dataput_customer
            call "SHOSTAT" ("Updating Customer Route Codes")

            init(" ") readkey$, desc$
            str(readkey$,1%,9%) = "PLAN RTE "
        dataput_nxt
            read #3,key > readkey$, using L31600, readkey$, desc$,        ~
                                                 eod goto dataput_done
L31600:        FMT CH(24), CH(30)
            if str(readkey$,1%,8%) <> "PLAN RTE" then goto dataput_done
               init(" ") or_cuscode$, cus_route$
               or_cuscode$ = str(desc$,1%,9%)
               cus_route$  = str(readkey$,10%,5%)
               gosub update_customer
               goto dataput_nxt
        dataput_done
        return clear all
        goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1

            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L40160,         /* Process Option    */     ~
                              L40160          /* Selection         */
            goto L40180

                lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40160:         lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40180:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                at (07,02), "Process Option    :",                       ~
                at (07,24), fac(lfac$(1%)), option$             , ch(01),~
                at (07,40), fac(hex(84)),   option_d$           , ch(30),~
                                                                         ~
                at (08,02), "Reassign Selection:",                       ~
                at (08,24), fac(lfac$(2%)), selection$          , ch(01),~
                at (08,40), fac(hex(84)),   sel_d$              , ch(30),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15% then L40390
                     call "PRNTSCRN" : goto L40180

L40390:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

        return

        set_pf1
        if edit% = 2% then L40600     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)

            if fieldnr% = 1% then L40560
                str(pf$(3%),64%) = " "     : str(pfkeys$,16%,1%) = hex(ff)
L40560:     if fieldnr% > 1% then L40580
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%)  = hex(ff)
L40580: return

L40600: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Update      "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
        return

L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
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
            on fieldnr% gosub L50110,                /* Process Option  */~
                              L50220                 /* Selection       */
        return

L50110: REM Process Option                          OPTION$, OPTION_D$
            init(" ") option_d$
            if option$ <> "1" and option$ <> "2" then goto L50180
               option_d$ = "Update Customer Route'S Only  "
               if option$ = "2" then                                     ~
                            option_d$ = "Update Scheduled Sales Orders "
        return
L50180:     errormsg$ = "(Error) - Invalid Process Selection (1 or 2)?"
            init(" ") option$, option_d$
        return

L50220: REM Selection                               SELECTION$, SEL_D$
            init(" ") sel_d$
            if selection$ <> "1" and selection$ <> "2" and               ~
               selection$ <> "3" then goto L50320
            sel_d$ = "Update Scheduled Region's Only"
            if selection$ = "2" then                                     ~
                            sel_d$ = "Update Scheduled Route's Only "
            if selection$ = "3" then                                     ~
                            sel_d$ = "Update Both Regions & Routes  "
        return
L50320:     errormsg$ = "(Error) - Invalid Selection (1, 2, or 3) ?"
            init(" ") selection$, sel_d$
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        lookup_customer
            rec% = 0%
            read #2,key = or_cuscode$, using L60070, cus_region$,         ~
                cus_route$, eod goto L60100
L60070:         FMT POS(940), CH(02), XX(38), CH(05)

            rec% = 1%
L60100: return

        update_customer
            read #2,hold,key = or_cuscode$, using L60160, xxx_route$,     ~
                                                         eod goto L60200
L60160:         FMT POS(980), CH(05)
            put #2, using L60160, cus_route$
            rewrite #2
        return
L60200:     call "SHOSTAT" ("Error-Updating Customer Route-"&or_cuscode$)
            stop
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************
        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
