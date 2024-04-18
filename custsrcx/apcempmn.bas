        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCEMPMN                             *~
            *  Creation Date     - 08/01/96                             *~
            *  Last Modified Date- 03/17/2000                           *~
            *  Written By        - Roy H. Hoffman                       *~
            *                                                           *~
            *  Description       - Employee Barcode label Printing.     *~
            *                                                           *~
            *  Subroutines       - APCBAREM - Mo Longer used            *~
            *                      EWDBAREM - New Subroutine            *~ 
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/01/96 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 03/17/00 ! (EWD001) Switch Employee Labels to the   ! RHH *~
            *          !   Zebra Printer.                         !     *~
            *02/22/2019! CR-1894 Increase size of EMP DEPT to 3   ! DES *~
            *05/05/2020! CR2490  Increase Employee Number size    ! RDB *~
            *************************************************************
        dim                                                              ~
            access$30, hdr$40, msg$(3)79,/* Security Access Check      */~
            empno$8,                     /* Employee Number            */~
            beg_no$8, beg_desc$30,       /* Beginning Emp Number       */~
            end_no$8, end_desc$30,       /* Ending Emp Number          */~
            e_lname$15,                  /* Last Name                  */~
            e_fname$10,                  /* First Name                 */~
            x_fname$15,                  /* First Name                 */~
            e_fnamex$5,                  /* First Name                 */~
            e_init$1,                    /* Middle Init                */~
            e_status$1,                  /* STATUS (A,I,T)             */~
            readkey$50,                  /* Generic Key                */~
            emp_key$26,                  /* Employee Primary Key       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            filename$8,                  /* Used by EWDOPEN            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            error$1,                     /* Error Code                 */~
            userid$3                     /* Current User Id            */

        dim f2%(2),                      /* = 0 if the file is open    */~
            f1%(2),                      /* = 1 if READ was successful */~
            fs%(2),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(2)20                   /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 03/17/00 Employee Barcode Labels Print  "

            mat f2% = con
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCEMPLY ! Employee Master File                     *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "APCEMPLY",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    7, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos =   12, keylen =  26, dup

            select #2, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "APCEMPLY" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            date$       = date
            call "DATEFMT"     (date$)
            call "EXTRACT" addr("ID", userid$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor "&~
                          "to Desired Value & Press (RETURN)."
            gosub load_access
                if access% = 0%                 then exit_program

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 2%
L10090:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0%           then L10240

L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1%          then gosub startover
                     if keyhit% <>  4%          then L10210

L10160:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                               if enabled% = 1% then L10120
                               if fieldnr% = 1% then L10090
                               goto L10160
L10210:              if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0%           then L10120

L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " "        then L10120

            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            lastfieldnr%      =  0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%    =  1%             then gosub startover
                if keyhit%    = 14%             then gosub begin_process
                if keyhit%    = 16%             then       exit_program
                if keyhit%   <>  0%             then       editpg1

L11130:     fieldnr%          =  cursor%(1%) - 5%
            if fieldnr%       <  1%                                      ~
            or fieldnr%       >  2%             then       editpg1
            if fieldnr%       =  lastfieldnr%   then       editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled%   =  0%             then       editpg1

L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%    =  1%             then gosub startover
                if keyhit%   <>  0%             then L11200

            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " "             then L11200
                lastfieldnr%  =  fieldnr%

            goto L11130

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            *************************************************************
        begin_process
            call "SHOSTAT" ("Creating Barcode Labels")
            cnt%        = 0%
            emp_key$    = all(hex(00))
            read #1,key > emp_key$, using L19150, emp_key$,               ~
                eod goto process_done

            goto L19170

        begin_next
            read #1, using L19150, emp_key$, eod goto process_done
L19150:         FMT POS(7), CH(5)

L19170:     if beg_no$    = "ALL  "             then L19200     
            get str(emp_key$,1%,4%), using L12345, e_no%        /* CR2490 */
            if e_no%   <  beg_no%           then begin_next
            if e_no%   >  end_no%           then process_done
L19200:     get #1, using L19210, e_lname$, e_fname$, e_init$, e_status$
L19210:  FMT POS(12), CH(15), CH(10), CH(1), POS(152), CH(1),POS(844), CH(5)

            if e_status$ <> "A"                 then begin_next
            cnt%          =  cnt% + 1%
REM            empno$        =  emp_key$
            convert e_no% to empno$, pic(#######0)
                                                /* (EWD001)           */             
            x_fname$ = e_fname$ & e_fnamex$
            call "EWDBAREM" (empno$,    /* Employee Number            */~
                             e_lname$,  /* Employee Last Name         */~
                             x_fname$,  /* Employee First Name        */~
                             e_init$,   /* Employee Middle initial    */~
                             #2,        /* Gencodes                   */~ 
                             err%)      /* Return Code                */
            if err% <> 0% then gosub print_error 

                                                /* (EWD001)           */
            init(" ") empno$, e_lname$, e_fname$, e_init$, e_fnamex$
            goto begin_next

        process_done
            if cnt%       =  0%                 then L19350
L19350: return clear all
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
            if fieldnr%    <> 0% then L28100
                inpmessage$ = edtmessage$
            return

L28100
*        Define the Input Message for the Screen/Field Indicated
            if scrnr%       = 1% then restore line = scrn1_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Beginning Employee Number or (ALL).            ",~
         "Enter a Valid Ending Employee Number or Leave Blank.         "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, empno$, beg_no$, end_no$,  ~
                      beg_desc$, end_desc$, e_lname$, e_fname$, e_init$, ~
                      readkey$,  emp_key$,  e_status$, e_fnamex$
        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************
        startover
            u3%        = 2%
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
            read #1,key = emp_key$, using L30110, e_lname$, e_fname$,     ~
                e_init$, eod goto L30130
L30110:         FMT POS(12), CH(15), CH(10), CH(1)

            rec% = 1%
L30130: return

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
                if fieldnr% > 0% then init(hex(8c)) lfac$()              ~
                                 else init(hex(86)) lfac$()

            on fieldnr%          gosub L40160,  /* Beginning Emp No.    */~
                                       L40160   /* Ending Employee No.  */
            goto L40190

            lfac$(fieldnr%) = hex(80) : return /* Up / Low   */
L40160:     lfac$(fieldnr%) = hex(81) : return /* Upper Only */
            lfac$(fieldnr%) = hex(82) : return /* Numeric    */

L40190:     accept                                                       ~
                at (01,02),                                              ~
                   "Create Employee Barcode Labels ",                    ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                at (04,02), fac(hex(94)),   errormsg$           , ch(79),~
                                                                         ~
                at (06,02), "Beginning Employee No:",                    ~
                at (06,25), fac(lfac$( 1)), beg_no$             , ch(08),~
                at (06,40), fac(hex(84)),   beg_desc$           , ch(30),~
                                                                         ~
                at (07,02), "Ending Employee No.  :",                    ~
                at (07,25), fac(lfac$( 2)), end_no$             , ch(08),~
                at (07,40), fac(hex(84)),   end_desc$           , ch(30),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15                then L40450
                     call "PRNTSCRN"
                     goto L40190

L40450:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
        if edit% = 2% then L40640     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40600
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40600:     if fieldnr% > 1% then L40620
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40620: return

L40640: if fieldnr% > 0% then L40730  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Labels"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
        return
L40730:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50110,         /* Beginning Employee No.*/ ~
                              L50370          /* Ending Employee No.   */
        return

L50110: REM Beg Employee Number                   BEG_NO$
            if  beg_no$  <> " "                         then L50180
                beg_no$   = "ALL  "
                beg_desc$ = "Print Labels for All Employees"
                init(" ")    end_no$, end_desc$
                fieldnr%  =  2%
            return
L50180:     
REM         convert beg_no$ to beg_no%,            data goto L50230
            convert beg_no$ to beg_no%, data goto L50330          /* CR2490 */
L12345:             FMT BI(4)

            convert beg_no% to beg_no$,            pic(#####000)  /* CR2490 */

            goto L50270
L50230:     convert str(beg_no$,2%,7%) to beg_no%, data goto L50330  /*CR2490 */

            convert beg_no% to str(beg_no$,2%,7%), pic(#####000)   /* CR2490 */

L50270:     
REM         emp_key$      =  beg_no$
            put str(emp_key$,1%,4%), using L12345, beg_no%      /* CR2490 */
            str(emp_key$,5%,1%) = " "
            gosub dataload
                if rec%   =  0%                         then L50330

            beg_desc$     =  e_lname$ & ", " & e_fname$ & ", " & e_init$
        return
L50330:     errormsg$     = "Invalid Beginning Employee Number?"
            gosub error_prompt
            init(" ")        beg_no$, beg_desc$
        return

L50370: REM Ending Employee Number                END_NO$
            if end_no$   <> " "                         then L50410
            if beg_no$    = "ALL  "                     then return
            end_no$       =  beg_no$
L50410:     
REM         convert end_no$ to end_no%,            data goto L50450
            convert end_no$ to end_no%, data goto L50550          /* CR2490 */
            convert end_no% to end_no$,            pic(#####000)
            goto L50490
L50450:     convert str(end_no$,2%,7%) to end_no%, data goto L50550  /*CR2490*/

            convert end_no% to str(end_no$,2%,7%), pic(#####000)

L50490:     
REM         emp_key$      =  end_no$
            put str(emp_key$,1%,4%), using L12345, end_no%      /* CR2490 */
            str(emp_key$,5%,1%) = " "
            gosub dataload
                if rec%   =  0%                         then L50550

            end_desc$     =  e_lname$ & ", " & e_fname$ & ", " & e_init$
        return
L50550:     errormsg$     = "Invalid Ending Employee Number?"
            gosub error_prompt
            init(" ")        end_no$, end_desc$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        load_access
            init(" ") readkey$, access$
            access%               =  0%
            str(readkey$,1%,9%)   = "EMP SECUR"
            str(readkey$,10%,15%) =  userid$
            read #2,key = readkey$,  using L60090, access$, eod goto L60130
L60090:         FMT POS(25), CH(30)

            access%               =  1%
        return
L60130:     comp%   =  2%
            hdr$    = "*** Access Denied to User ***"
            msg$(1) = "   You Do Not Have Access to Option Selected??   "
            msg$(2) = "            A c c e s s   S e c u r i t y        "
            msg$(3) = "   Press <RETURN> or Any (PF) Key To Continue.   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        print_error
           convert err% to error$, pic(#)

           errormsg$ = "(Error) Printing Employee Label? " &           ~
                                          " (" & error$ & ")"

           gosub error_prompt
        return
                                                  /* (EWD001)          */      
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
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
