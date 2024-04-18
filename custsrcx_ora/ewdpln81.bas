        REM *************************************************************~
            *                                                           *~
            *  EEEE   W   W  DDDD   PPPP   L      N   N   888       1   *~
            *  E      W   W  D   D  P   P  L      NN  N  8   8     11   *~
            *  EEE    W W W  D   D  PPPP   L      N N N   888     1 1   *~
            *  E      W W W  D   D  P      L      N  NN  8   8      1   *~
            *  EEEE    W W   DDDD   P      LLLLL  N   N   888       1   *~
            *                                                           *~
            *  ( APCFAXSD - Faxing of Sales Order Acknowledgements.  )  *~
            *-----------------------------------------------------------*~
            * EWDPLN81 - PERMITS REFAXING OF SALES ORDER                *~
            *            ACKNOWLEDGEMENTS.                              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/28/02 ! Original                                 ! CMG *~
            *06/06/2011! (AWD001) - add orcl usr & pswd lookup    ! CMG *~
            *************************************************************


        dim f1%(25%),                    /* = 1 if READ was successful */~
            errormsg$79,                 /* Error message              */~
            edtmessage$79,               /* Edit screen message        */~
            hdr$40, msg$(3%)79,          /* ASKUSER                    */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            cuscode$9,                   /* Customer Code              */~
            so$8,                        /* S.O. Number                */~
            readkey$19,                  /* BCKLINES Readkey           */~
            userid$3                     /* UserID                     */

        dim                              /*  (EWD013)                  */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User Name to Connect       */~
            pass$25                      /* Password to Connect        */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$45, pname$10
            apc$   = "(EWD) Ver for Printing S.O. Acknowledgements"
            pname$ = " EWDPLN81 "

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
            * #1  ! HNYMASTR ! Inventory Master File                    *~
            * #2  ! BCKMASTR ! Backlog master file                      *~
            * #3  ! BCKLINES ! Back Log Line Item File                  *~
            * #4  ! BOMSPEC  ! options selected file                    *~
            * #5  ! CUSTOMER ! Customer Master File                     *~
            * #6  ! GENCODES ! Master Code Table Files                  *~
            * #7  ! TXTFILE  ! Text File        		        *~
    	    * #8  ! ARMTERMS ! Terms File                               *~
            * #9  ! SYSFILE2 ! Caelus Management System General Informa *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #2,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #3,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #4,  "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #5,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #6,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #7,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #8,  "ARMTERMS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  20

           select #9,   "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "SHOSTAT" ("Opening Files, One Moment Please")

            err% = 0%

            filename$ = "HNYMASTR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BOMSPEC"  : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "TXTFILE"  : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARMTERMS" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
/*(AWD001) */
            filename$ = "SYSFILE2" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error


            call "EXTRACT" addr("ID", userid$)
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
            gosub initialize_variables

            for fieldnr% = 1% to   1%
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
                  if keyhit%  = 14% then gosub datasave   
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
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

        call "SHOSTAT" (" Faxing Please Wait")
        gosub oracle_connect

        error% = 0%
	call "APCFAXSD" (cuscode$     , /* Customer Code  */    ~
                         so$          , /* Sales Order No */    ~
                         userid$      , /* User ID        */    ~
			 error%	      ,	/* Error Flag     */    ~
			 #6           , /* Gencodes File  */    ~
			 #2           , /* BCKMASTR File  */    ~
		 	 #3           , /* BCKLINES File  */    ~
			 #7           , /* TXTFILE  File  */    ~
			 #5           , /* CUSTOMER File  */    ~
			 #8           , /* ARMTERMS File  */    ~
			 #1 	      , /* HNYMASTR File  */	~
			 #4 )           /* BOMSPEC  File  */

            if error% = 0% then return

            errormsg$ = "Sales Order " & so$ & " did not fax!!"
            gosub error_prompt
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
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
         "Enter a Sales Order Number to Fax?"

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
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************


        initialize_variables
            init(" ") errormsg$, inpmessage$, hdr$, msg$(), cuscode$,    ~
                      so$, readkey$

        return


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
               at (01,02), fac(hex(8c)), pname$                 , ch(10),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,17), fac(hex(a4)), apc$                   , ch(45),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Sales Order Num    :",                       ~
               at (06,28), fac(lfac$(1%)), so$                  , ch(08),~
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
                      "                      (14)Fax Ack      "
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
                  on fieldnr% gosub L52700          /* Sales Order Num  */ 

            return
                                    

L52700:    init(" ") readkey$
           str(readkey$,1%,8%) = so$
           read #3 , key > readkey$, using L52710, cuscode$, readkey$,   ~
                                                           eod goto L52790
L52710:        FMT CH(9), CH(19)

               if str(readkey$,1%,8%) <> so$ then goto L52790

          return

L52790:   errormsg$ = "You must enter a valid SO for the Customer."
          return


        open_error                                  
            err% = 0%
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        oracle_connect
            init(" ") user$, pass$, server$
REM            user$   = "MSSQL"
REM            pass$   = "MSSQL"
            gosub get_user_pswd       /* (AWD001) */

            oci_err% = 0%
            call "CONNECT" (user$, pass$, server$, oci_err%)
        return
 
/* (AWD001) beg */
        get_user_pswd
            call "READ100" (#9, "ORACLE PASSWORD", f1%(9%))   /* SYSFILE2 */
            if f1%(9%) <> 0% then get #9 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return

/* (AWD001) END */
                          

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end


