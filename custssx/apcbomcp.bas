        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   BBBB    OOO   M   M   CCC   PPPP    *~
            *  A   A  P   P  C   C  B   B  O   O  MM MM  C   C  P   P   *~
            *  AAAAA  PPPP   C      BBBB   O   O  M M M  C      PPPP    *~
            *  A   A  P      C   C  B   B  O   O  M   M  C   C  P       *~
            *  A   A  P       CCC   BBBB    OOO   M   M   CCC   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCBOMCP - Multiple Model and Field Copy and Delete.      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/10/02 ! Original                                 ! CMG *~
            *************************************************************

            sub  "APCBOMCP" ( #1,             /* (AMTBOMIF) - FILE     */~
                              #2,             /* (AMTBOMPM) - FILE     */~
                              #3,             /* (GENCODES) - FILE     */~
                              err% )          /* 0% = OK               */
        dim                                                              ~
            hdr$40, msg$(3%)79,          /* Screen Status Text         */~
            cnt1$5, cnt2$5,              /* Status Counters            */~
            field$2,                     /* Field Number               */~
            model$15,                    /* Model Code                 */~
            value$15,                    /* Code Value                 */~
            include$1,                   /* Include Value              */~
            screen$20, printer$30,       /* Screen and Print Text      */~
            phantom$25, bom$3, fil$9,    /* Phantom/Bom ID = N/A       */~
            beg_mod$3, beg_mdesc$30,     /* Beginning Model Code       */~
            end_mod$3, end_mdesc$30,     /* Ending Model Code          */~
            beg_fld$3, beg_fdesc$30,     /* Beginning Field Code       */~
            end_fld$3, end_fdesc$30,     /* Ending Field Code          */~
            mod_code$, desc$32,          /* Model Lookup Code          */~
            scan_key$32, put_key$32,     /* Scan Key                   */~
            match_key$32,                /* Match Key                  */~
            readkey$50,                  /* TABLE KEY                  */~
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
            apc$   = "(EWD) Record Copy / Delete Utility      "
            pname$ = "APCBOMED - Rev: R6.04"

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
            * #1  ! AMTBOMIF ! Field Definitions for Item Number        *~
            * #3  ! GENCODES ! Control System Codes File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            mat f1% = zer
            mat fs% = zer
            init(" ") rslt$()
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
            
        gosub check_security
        if security% <> 0% then gosub inputmode
           gosub security_error

           goto exit_program        

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  4%
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
                  if keyhit%  = 12% then gosub delete_data
                  if keyhit%  = 14% then gosub copy_data
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if cursor%(1%) > 6% then fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

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
         "Enter Beginning Model (ALL) Number for Delete/Copy?(Required)",~
         "Enter Ending Model Number for Delete/Copy?                   ",~
         "Enter Beginning Field (AL) Number for Delete/Copy? (Required)",~
         "Enter Ending Field Number for Delete/Copy?                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_mod$, beg_mdesc$,      ~
                      end_mod$, end_mdesc$, scan_key$, fil$, model$,     ~
                      match_key$, mod_code$, desc$, cnt1$, cnt2$, field$,~
                      beg_fld$, beg_fdesc$, end_fld$, end_fdesc$, value$,~
                      put_key$
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
            dataload
            rec% = 0%
            read #1,key = scan_key$, eod goto L30160
                get #1 using L35060, model$, field$, value$, include$,    ~
                                 screen$, printer$, phantom$, bom$, fil$

            rec% = 1%
L30160: return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
REM            put_key$ = scan_key$
REM            str(put_key$,16%,17%) = str(match_key$,16%,17%)
REM            model$ = str(put_key$,1%,15%)
            read #1,hold,key = put_key$, eod goto L31120
               delete #1
L31120:     put #1, using L35060, model$, field$, value$, include$,       ~
                                 screen$, printer$, phantom$, bom$, fil$

              write #1, eod goto L31170
        return
L31170:     stop "(ERROR) - UPDATE ERROR "
            close ws
        return

        REM *************************************************************~
            *              F O R M A T   S T A T E M E N T S            *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

L35060: FMT                 /* FILE: AMTBOMIF                          */~
            CH(15),         /* Model Number                            */~
            CH(2),          /* Definition for elements whose Type = FIE*/~
            CH(15),         /* Value of the field of the Model         */~
            CH(1),          /* Include / Exclude the Value             */~
            CH(20),         /* Verbage to appear on the Screen Displays*/~
            CH(30),         /* Verbage to print on the Hardcopy        */~
            CH(25),         /* Phantom Assembly Part Number            */~
            CH(03),         /* Bom Id Associated with Phantom          */~
            CH(09)          /* Filler Area                             */

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
              on fieldnr% gosub L40170,         /* Beg Model Code    */   ~
                                L40170,         /* End Model Code    */   ~
                                L40170,         /* Beg Field Code    */   ~
                                L40170          /* End Field Code    */

              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Frm Delete/Copy Model Code:",                ~
               at (04,30), fac(lfac$(1%)), beg_mod$             , ch(03),~
               at (04,40), fac(hex(8c)), beg_mdesc$             , ch(30),~
                                                                         ~
               at (05,02), "To  Delete/Copy Model Code:",               ~
               at (05,30), fac(lfac$(2%)), end_mod$             , ch(03),~
               at (05,40), fac(hex(8c)), end_mdesc$             , ch(30),~
                                                                         ~
                                                                         ~
               at (07,02), "Frm Delete/Copy Field Code:",                ~
               at (07,30), fac(lfac$(3%)), beg_fld$             , ch(02),~
               at (07,40), fac(hex(8c)), beg_fdesc$             , ch(30),~
                                                                         ~
               at (08,02), "To  Delete/Copy Field Code:",                ~
               at (08,30), fac(lfac$(4%)), end_fld$             , ch(02),~
               at (08,40), fac(hex(8c)), end_fdesc$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then L40690
                  call "PRNTSCRN" : goto L40200

L40690:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40880     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffffff0f1000)
            if fieldnr% = 1% then L40840
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40840:     if fieldnr% > 1% then L40860
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40860:     return

L40880: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (12)Delete Range       " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (14)Copy Range         " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffff0cff0e0f1000)
            return
L40970:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
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
            on fieldnr% gosub L50130,         /* BEG MODEL CODE         */~
                              L50310,         /* END MODEL CODE         */~
                              L50550,         /* BEG FIELD CODE         */~
                              L50700          /* END FIELD CODE         */

            return

L50130: REM Test for Beginning Model Code         BEG_MOD$, BEG_MDESC$
            if beg_mod$ <> " " then goto L50200
L50150:        beg_mod$, end_mod$     = "ALL"
               beg_mdesc$, end_mdesc$ = "(A)ll Model Codes "
               end_mod$ = "ALL"
               end_mdesc$ = beg_mdesc$
               return
L50200:     if str(beg_mod$,1%,3%) = "ALL" then goto L50150
               mod_code$ = beg_mod$
               gosub model_lookup
               if mod% = 0% then goto L50270
                  beg_mod$  = mod_code$
                  beg_mdesc$ = desc$
        return
L50270:     errormsg$ = "(Error) Invalid Beginning Model Code ?"
            init(" ") beg_mod$, beg_mdesc$
        return

L50310: REM Test for Ending Model Code            END_MOD$, END_MDESC$
            if end_mod$ <> " " then goto L50360
L50330:        end_mod$, beg_mod$     = "ALL"
               end_mdesc$, beg_mdesc$ = "(A)ll Model Codes "
               return
L50360:     if str(end_mod$,1%,3%) = "ALL" then goto L50330
            mod_code$ = end_mod$
            gosub model_lookup
            if mod% = 0% then goto L50430
               end_mod$  = mod_code$
               end_mdesc$ = desc$
        return
L50430:     errormsg$ = "(Error) Invalid Ending Model Code ?"
            init(" ") end_mod$, end_mdesc$
        return

L50550: REM Test for Beginning Field Code         BEG_FLD$, BEG_FDESC$
            fld% = 99%
            if beg_fld$ <> " " then goto L50560
L50570:        beg_fld$, end_fld$     = "AL"
               beg_fdesc$, end_fdesc$ = "All Field Codes"
            return
REM            call "GETCODE" (#2, beg_fld$, bef_fdesc$, 0%, .08, f1%(2))
REM            if f1%(2) = 0% then goto L50590

L50560:     if beg_fld$ = "AL" then L50570
            convert beg_fld$ to fld%, data goto L50590

            convert fld% to beg_fld$, pic(00)

            if beg_fld$ > "36" then goto L50590

            read #2,key = beg_fld$, eod goto L50590

            get #2 using L50580, beg_fdesc$
L50580:        FMT XX(2), CH(08)

        return
L50590:     errormsg$ = "(Error) Invalid Beginning Field Code ?"
            init(" ") beg_fld$, beg_fdesc$
        return

L50700: REM Test for Ending Field Code            END_FLD$, END_FDESC$
            fld% = 99%
            if end_fld$ <> " " then goto L50710
L50720:        end_fld$, beg_fld$     = "AL"
               end_fdesc$, beg_fdesc$ = "All Field Codes"
            return
REM               call "GETCODE" (#2, end_fld$, end_fdesc$, 0%, .08, f1%(2))
REM               if f1%(2) = 0% then goto L50790

L50710:     if end_fld$ = "AL" then L50720

            convert end_fld$ to fld%, data goto L50790

            convert fld% to end_fld$, pic(00)

            if end_fld$ > "36" then goto L50790

            read #2,key = end_fld$, eod goto L50790

            get #2 using L50580, end_fdesc$


        return
L50790:     errormsg$ = "(Error) Invalid Ending Field Code ?"
            init(" ") end_fld$, end_fdesc$
        return

        REM *************************************************************~
            *                                                           *~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *                                                           *~
            *                                                           *~
            *************************************************************

        model_lookup                          /* Model Description     */
            mod% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "MODEL    "
            str(readkey$,10%,15%) = mod_code$
            read #3,key = readkey$, using L60130, desc$, eod goto L60150
L60130:       FMT POS(25), CH(32)
            mod% = 1%
L60150: return


        delete_data
            call "SHOSTAT" ("Deleting Specified Data for Model(s)")
            cnt1%, cnt2% = 0%
            gosub delete_prompt
            if comp% <> 30% then goto delete_done
            scan_key$ = all(hex(00))
            if str(beg_mod$,1%,3%) = "ALL" then goto delete_next
               str(scan_key$,1%,3%) = beg_mod$
        delete_next
            read #1,hold,key > scan_key$, using L60260, scan_key$,        ~
                                                    eod goto delete_done
L60260:        FMT CH(32)
            cnt1% = cnt1% + 1%
            print at(3,35), hex(84), "["; cnt1% ; "]"

            if str(beg_mod$,1%,3%) = "ALL" then goto L60330
               if str(scan_key$,1%,3%) > end_mod$ then                   ~
                                                  goto delete_done
L60330:     if str(beg_fld$,1%,2%) = "AL" then goto L60340
               if str(scan_key$,16%,2%) < beg_fld$ then                   ~
                                                  goto delete_next
               if str(scan_key$,16%,2%) > end_fld$ then                   ~
                                                  goto delete_next
L60340:
REM            if str(scan_key$,16%,17%) <> str(match_key$,16%,17%) then    ~
REM                                                         goto delete_next
               cnt2% = cnt2% + 1%
               delete #1
               goto delete_next
        delete_done
            gosub status_screen
        return clear all
        goto exit_program

        copy_data
            call "SHOSTAT" ("Copying Specified Data for Model(s)")
            cnt1%, cnt2% = 0%
            scan_key$ = all(hex(00))
            if str(beg_mod$,1%,3%) = "ALL" then goto copy_next
               str(scan_key$,1%,3%) = beg_mod$
        copy_next
            read #1,key > scan_key$, using L60520, scan_key$,             ~
                                                  eod goto copy_done
L60520:        FMT CH(32)
            cnt1% = cnt1% + 1%
            print at(3,35), hex(84), "["; cnt1% ; "]"

            if str(beg_mod$,1%,3%) = "ALL" then goto L60580
               if str(scan_key$,1%,3%) <> beg_mod$ then              ~
                                                  goto copy_done
L60580:
            if str(end_fld$,1%,2%) = "AL" then  goto L60590
               if str(scan_key$,16%,2%) < beg_fld$ or                ~
                    str(scan_key$,16%,2%) > end_fld$ then            ~
                                                  goto copy_next
L60590:                                   /* DELETE AND REPLACE OR ADD */
               gosub dataload
               if rec% <> 1% then copy_next
               put_key$ = all(hex(00))
               put_key$ = scan_key$
               if str(end_mod$,1%,3%) = "ALL" then L60600
                  if field$ = "01" then value$ = str(end_mod$,1%,3%)
                  if field$ = "01" then                                  ~
                            str(put_key$,18%,3%) = str(end_mod$,1%,3%)
                  str(model$,1%,3%)    = str(end_mod$,1%,3%)
                  str(put_key$,1%,3%) = str(end_mod$,1%,3%)
L60600:
               
                goto L60610

                if str(end_fld$,1%,2%) = "AL"  then L60610
                  str(field$,1%,2%)     = str(end_fld$,1%,2%)
                  str(put_key$,16%,2%) = str(end_fld$,1%,2%)
L60610:
               gosub dataput
               cnt2% = cnt2% + 1%
               goto copy_next
        copy_done
            gosub status_screen
        return clear all
        goto exit_program

        status_screen
            convert cnt1% to cnt1$, pic(#####)

            convert cnt2% to cnt2$, pic(#####)

            comp% = 2%
            hdr$    = "**** Delete/Copy Statistics ****"
            msg$(1) = " ***** Records Scanned        :"&cnt1$&" ******* "
            msg$(2) = " ***** Records Deleted/Copied :"&cnt2$&" ******* "
            msg$(3) = "         Press any Key To Continue......         "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        delete_prompt
           comp% = 2%
           hdr$ = "******* Deleting Validation Data *******"
           msg$(1%) = " - - - -  A r e   Y o u   S u r e  - - - - "
           msg$(2%) = "   Press Any (PF) Key to Abort, or Press   "
           msg$(3%) = "    <PF30> to Delete Validation Data?      "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
        
        check_security
           security% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%) = "VAL SECUR"
           str(readkey$,10%,3%) = userid$
           read #3,key = readkey$, eod goto not_valid
              security% = 1%
        not_valid
        return

        security_error
            comp% = 2%
            hdr$ = "******** Access Denied ********"
            msg$(1) = "Currently 'You' do not have Access To Selection!"
            msg$(2) = "           A c c e s s   D e n i e d            "
            msg$(3) = "       Press <RETURN> To Continue !!!!          "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return        

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end

