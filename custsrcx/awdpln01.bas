        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN01                             *~
            *  Creation Date     - 03/07/03                             *~
            *  Last Modified Date- 09/22/06                             *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New program to build an Appian       *~
            *                      Customer Extract File.               *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Spec. Comm (Screen 1) -                                  *~
            *                         PF(10) Create Customer File.      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/07/03 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 07/10/03 ! (EWD001) - Mod for twice week customers. ! CMG *~
            * 03/16/06 ! (AWD002) - modification for North East   ! CMG *~
            * 09/22/06 ! (AWD003) - add region to pull criteria   ! CMG *~
            *11/10/2010! (AWD004) - changes for Appian9           ! DES *~
            *12/20/2018! (CR1829) - cust 6 -> 9 char for Dallas   ! DES *~
            *************************************************************

        dim                              /* (CUSTOMER) - FILE          */~
            exc_cust$(900%)9,            /* Customers to Exclude       */~
            genkey$24,                   /* Gencodes Readkey           */~
            gen_desc$30,                 /* Gencodes Description       */~
            readkey$9,                   /* Customer Readkey           */~
            custcode$9,                  /* Customer Code              */~
            custname$30,                 /* Customer Name              */~
            cutoff$2,                    /* Customer Cutoff            */~
            cutoff1$2,                   /* Customer Cutoff  1         */~
            cutoff_twice$2,              /* Customer Cutoff  Twice     */~
            routecode$5,                 /* Customer Route Code        */~
            route$5,                     /* Customer Route for Test    */~
            address$(5%)30,              /* Customer Address           */~
            city$18,                     /* Customer City              */~
            state$2,                     /* Customer State             */~
            zip$9,                       /* Customer Zip               */~
            contact$20,                  /* Customer Contact Name      */~
            contphone$10,                /* Customer Contact Phone     */~
            varfields$200,               /* Customer Variable Fields   */~
            status$1,                    /* Customer Status            */~
            sunet1$4,                    /* Sun Early Time 1           */~
            sunlt1$4,                    /* Sun Late Time  1           */~
            sunet2$4,                    /* Sun Early Time 2           */~
            sunlt2$4,                    /* Sun Late Time  2           */~
            monet1$4,                    /* Mon Early Time 1           */~
            monlt1$4,                    /* Mon Late Time  1           */~
            monet2$4,                    /* Mon Early Time 2           */~
            monlt2$4,                    /* Mon Late Time  2           */~
            tueet1$4,                    /* Tue Early Time 1           */~
            tuelt1$4,                    /* Tue Late Time  1           */~
            tueet2$4,                    /* Tue Early Time 2           */~
            tuelt2$4,                    /* Tue Late Time  2           */~
            wedet1$4,                    /* Wed Early Time 1           */~
            wedlt1$4,                    /* Wed Late Time  1           */~
            wedet2$4,                    /* Wed Early Time 2           */~
            wedlt2$4,                    /* Wed Late Time  2           */~
            thret1$4,                    /* Thr Early Time 1           */~
            thrlt1$4,                    /* Thr Late Time  1           */~
            thret2$4,                    /* Thr Early Time 2           */~
            thrlt2$4,                    /* Thr Late Time  2           */~
            friet1$4,                    /* Fri Early Time 1           */~
            frilt1$4,                    /* Fri Late Time  1           */~
            friet2$4,                    /* Fri Early Time 2           */~
            frilt2$4,                    /* Fri Late Time  2           */~
            satet1$4,                    /* Sat Early Time 1           */~
            satlt1$4,                    /* Sat Late Time  1           */~
            satet2$4,                    /* Sat Early Time 2           */~
            satlt2$4,                    /* Sat Late Time  2           */~
            time1$4,                     /* Time 1                     */~
            time2$4,                     /* Time 2                     */~
            time3$4,                     /* Time 3                     */~
            time4$4                      /* Time 4                     */

        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            cnt$28,                      /* Screen Display             */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* REPORT TITLE               */~
            wk_date$8,                   /* Work Date                  */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            message$256,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */



        dim bg_cust$9,                   /* Screen Beg Customer Code   */~
            ed_cust$9                    /* Screen End Customer Code   */

        dim comma$1,                     /* Comma for EWDAPPSN File    */~
            file$8,                      /* File Name                  */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim schema$8                     /* SCHEMA          (AWD002)   */

        dim region$2,                    /* (AWD003) customer region   */~ 
            beg_reg$2,                   /* (AWD003) beg screen region */~ 
            end_reg$2,                   /* (AWD003) end screen region */~
            plant$1                      /* (AWD003) customer plant    */

        dim     rec$(2)256                                             ~

        dim     open$(7)4,                                             ~
                close$(7)4,                                             ~
                pattern$(7)7                           

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Appian Customer Extract Utility  "
            pname$ = "AWDPLN01 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CUSTOMER ! Customer Master Schedule File            *~
            * #2  ! GENCODES ! Master Code Tables File                  *~
            * #3  ! AWDAPPCU ! Appian Customer Master File              *~
            * #4  ! AWDAPPC9 ! Appian 9 Customer Master File            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key 1, keypos = 10, keylen = 30, dup,        ~
                            key 2, keypos =424, keylen =  9, dup,        ~
                            key 3, keypos =771, keylen =  9, dup,        ~
                            key 4, keypos =780, keylen =  9, dup,        ~
                            key 5, keypos = 1049, keylen = 9, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #3, "AWDAPPCU",                                       ~
                        varc,     indexed, recsize = 384,                ~
                        keypos = 1,    keylen = 9

            select #4, "AWDAPPCU",                                       ~
                        varc,     indexed, recsize = 512,                ~
                        keypos = 1,    keylen = 9

            call "SHOSTAT" ("Initialization")

            filename$ = "CUSTOMER" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            wk_date$ = date
            call "DATEFMT" (date$)

* (AWD002) Next 3 lines
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #2, schema_err%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 4%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  =  9% then gosub load_analysis
                  if keyhit%  = 10% then gosub load_analysis /* AWD004 */
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  lastfieldnr% = fieldnr%
            goto L11150

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        load_analysis
/* <AWD004> */ 
            appian% = 8%
	    if keyhit% = 10% then appian% = 9%
/* </AWD004> */ 
            call "SHOSTAT" ("Loading Appian Customer Data")
            comma$ = "|"
            gosub open_file

            cnt% = 0%
            cnt$ = "Records Scanned [ xxxxxxxx ]"
            gosub load_exclude_customers
            gosub build_customer_file

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
         "Enter a Valid Beginning Customer Code ?                      ",~
         "Enter a Valid Ending    Customer Code ?                      ",~ 
         "Enter a Valid Beginning Region Code or 'AL'                  ",~
         "Enter a Valid Ending    Region Code or 'AL'                  "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") custcode$, custname$, cutoff$, routecode$,         ~
                      address$(), city$, state$, zip$, contact$,         ~
                      contphone$, status$, bg_cust$, ed_cust$, route$,   ~
/*(AWD003)*/          cutoff1$, cutoff_twice$, region$, beg_reg$, end_reg$

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload                         /* (CUSTOMER) - File          */
            init(" ") custcode$,  custname$,  address$(), city$, state$, ~
                      zip$, contact$, contphone$, status$, varfields$,   ~
                      cutoff$, routecode$, cutoff1$, cutoff_twice$,      ~
                      region$

        get #1, using L35040  ,                                           ~
            custcode$,                   /* Customer Code                */~
            custname$,                   /* Sort Name                    */~
            address$(),                  /* Ship-to Name and Address     */~
            city$,                       /* Cust Ship to Address - City  */~
            state$,                      /* Cust Ship to Address - State */~
            zip$,                        /* Cust Ship to Address - Zip   */~
            contact$,                    /* Contact                      */~
            contphone$,                  /* Phone number                 */~
            status$,                     /* Customer Status              */~
            varfields$                   /* Variable Fields Data Area    */

            p% = 0%
            for k% = 1% to 30%
                p% = pos(custname$ = ",")
                if p% = 0% then goto L20095
                   str(custname$,p%,1%) = " "
                   k% = p%
L20095:     next k%

            p% = 0%
            for k% = 1% to 150%
                p% = pos(address$() = ",")
                if p% = 0% then goto L30015
                   str(address$(),p%,1%) = " "
                   k% = p%
L30015:     next k%

            for k% = 1% to 18%
                p% = pos("," = city$)
                if p% = 0% then goto L30025
                   str(city$,p%,1%) = " "
                   k% = p%
L30025:     next k%



            for k% = 1% to 20%
                if str(contact$,k%,1%) = "," then ~
                   str(contact$,k%,1%) = " "
            next k%

            cutoff$    = str(varfields$,41%,2%)
            cutoff1$   = str(varfields$,81%,2%)               /*  (EWD001)  */
            routecode$ = str(varfields$,161%,5%)
            region$    = str(varfields$,121%,5%)              /* (AWD004) */


        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                         /* CUSTOMER - New File Layout */
L35040: FMT CH(9),          /* Customer Code                           */~
            CH(30),         /* Sort Name                               */~
            POS(253),       /*                                         */~
            5*CH(30),       /* Ship-to Name and Address                */~
            CH(18),         /* Customers Ship to Address - City        */~
            CH(2),          /* Customers Ship to Address - State       */~
            XX(1),          /*                                         */~
            CH(9),          /* Customers Ship to Address - Zip         */~
            CH(20),         /* Contact                                 */~
            CH(10),         /* Phone number                            */~
            POS(793),       /*                                         */~
            CH(1),          /* Customer Status                         */~
            XX(26),         /*                                         */~
            CH(200)         /* Variable Fields Data Area               */


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
REM L40070:   gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200,         /* Beg Customer Code    */~
	                        L40200,         /* End Customer Code    */~
                                L40200,         /* Beg Region code (AWD003)*/~
                                L40200          /* End Region code (AWD003)*/


              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Beginning Customer Cd:",                     ~
               at (03,25), fac(lfac$(1%)), bg_cust$             , ch(09),~
                                                                         ~
               at (04,02), "Ending    Customer Cd:",                     ~
               at (04,25), fac(lfac$(2%)), ed_cust$             , ch(09),~
/*(AWD003)*/                                                             ~
               at (05,02), "Beginning Region:     ",                     ~
               at (05,25), fac(lfac$(3%)), beg_reg$             , ch(02),~
                                                                         ~
               at (06,02), "Ending    Region:",                          ~
               at (06,25), fac(lfac$(4%)), end_reg$             , ch(02),~
/*(AWD003)*/                                                             ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then goto L40790
                  call "PRNTSCRN"


L40790:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40990     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            return

L40990: if fieldnr% > 0% then L41100  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                (10)Appian9" &       ~
                      " Customer Analysis                     "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(9)Appian Customer Analysis             " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff090affffffff0f1000)
            return
L41100:
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
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
            on fieldnr% gosub L50150,         /* Beg Customer Code     */~
                              L50500,         /* End Customer Code     */~
                              L51100,         /* Beg Region Code       */~
                              L51500          /* End Region Code       */

            return

L50150: REM Beginning Customer Code               BG_CUST$              
            if bg_cust$  > " " then goto L50190
            all_cust
               str(bg_cust$,1%,9%) = "ALL"
               str(ed_cust$,1%,9%) = "ALL"
            return
L50190:     custcode$ = bg_cust$
            gosub lookup_customer
            if cus% = 0% then goto L50330
        return
L50330:     errormsg$ = "(Error) - Invalid Customer Range??"
            gosub error_prompt
            init(" ") bg_cust$, ed_cust$
        return

L50500: REM Ending     Customer Code              ED_CUST$       
            if ed_cust$  > " " then goto L50530
               str(bg_cust$,1%,9%) = "ALL"
               str(ed_cust$,1%,9%) = "ALL"
            return
L50530:     if str(bg_cust$,1%,3%) = "ALL" or              ~
               str(ed_cust$,1%,3%) = "ALL" then goto all_cust
            custcode$ = ed_cust$
            gosub lookup_customer
            if cus% = 0% then goto L50590
        return
L50590:     errormsg$ = "(Error) - Invalid Customer Range??"
            gosub error_prompt
            init(" ") bg_cust$, ed_cust$
        return
L51100: REM Beginning Region Code                 BEG_REG$
            if beg_reg$ = " " then beg_reg$ = "AL"
            if beg_reg$ = "AL" then return

            init(" ") region$ 
            region$ = beg_reg$
             gosub lookup_region
             if region% = 0% then goto L51190

 
        return
L51190:      errormsg$ = "(Error) - Invalid Region Code??"
             gosub error_prompt
             init(" ") beg_reg$
        return
L51500: REM Ending Region Code
            if end_reg$ = " " then end_reg$ = beg_reg$
            if end_reg$ = "AL" then return

            init(" ") region$ 
            region$ = end_reg$
             gosub lookup_region
             if region% = 0% then goto L51590

              if end_reg$ < beg_reg$ then goto L51595

        return
L51590:      errormsg$ = "(Error) - Invalid Region Code??"
             gosub error_prompt
             init(" ") end_reg$
        return
L51595:      errormsg$ = "(Error) - End Region can not be less than beginning region??"
             gosub error_prompt
             init(" ") end_reg$
        return




        lookup_customer
           cus% = 0%
           read #1,key = custcode$, eod goto no_customer
           cus% = 1%
        no_customer
        return


        lookup_region
           region% = 0%
           init(" ") genkey$
           str(genkey$,1%,9%)   = "PLAN REGN"
           str(genkey$,10%,15%) = region$
           
           read #2, key = genkey$, eod goto region_done
              region% = 1%
        return
        region_done
        return


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************


        build_customer_file
            call "SHOSTAT" ("Loading Customer Information ")  

            gosub write_header
            init(" ") readkey$, route$
            if str(bg_cust$,1%,3%) = "ALL" then goto build_cust_first
               str(readkey$,1%,9%) = str(bg_cust$,1%,9%)
               str(readkey$,9%,1%) = hex(00)
        build_cust_first
            read #1,key > readkey$, using L61120, readkey$, region$, route$,~
                                       plant$, eod goto scan_cust_done
L61120:         FMT CH(9), POS(940), CH(02),  POS(980), CH(5), POS(1094), CH(01)
            goto read_cust_first
        build_cus_next 
            init(" ") route$
            read #1, using L61120, readkey$, region$, route$, ~
                                 plant$, eod goto scan_cust_done

        read_cust_first

            cnt% = cnt% + 1%
            if mod(cnt%,50%) <> 0% then goto L61155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L61155:    if str(route$,1%,5%) = " " then goto build_cus_next 
/* (AWD003) beg */

           convert plant$ to plant%, data goto build_cus_next
    
REM        if plant% <> schema% then goto build_cus_next
/* change for plantid */
           if schema% = 2% and plant% = 1% then goto build_cus_next
           if schema% = 1% and plant% = 4% then goto build_cus_next
            
/* (AWD003) end */
           for i% = 1% to cnt_max%
                if str(readkey$,1%,9%) = str(exc_cust$(i%),1%,9%) then     ~
                                                     goto build_cus_next
            next i%

            if str(bg_cust$,1%,3%) = "ALL" then goto L61170
               if str(readkey$,1%,9%) < str(bg_cust$,1%,9%) or             ~
                     str(readkey$,1%,9%) > str(ed_cust$,1%,9%) then goto   ~
                                                             build_cus_next
L61170:
/* (AWD003) beg */

            if str(beg_reg$,1%,2%) = "AL" then goto L61175
            if str(region$,1%,2%) < str(beg_reg$,1%,2%) or               ~
                      str(region$,1%,2%) > str(end_reg$,1%,2%)           ~
                                                  then goto build_cus_next

L61175:
/* (AWD003) end */
            gosub dataload
REM            if status$ = "I" or status$ = "D" then goto build_cus_next
            if status$ = "I" then goto build_cus_next
            if status$ = "D" then goto build_cus_next
	    if status$ = "O" then goto build_cus_next

REM            for j% = 1% to 7% /* AWD004 */
            gosub lookup_days
REM            next j%
            goto build_cus_next
        scan_cust_done
        return


        write_upload_file
REM            convert j% to str(custcode$,7%,1%), pic(0)
/* <AWD004> */ 
	    if appian% = 9% then goto write_upload_file9
/* </AWD004> */ 

            write #3, using L63400, custcode$, comma$, " ", comma$,        ~
                      custname$, comma$, cutoff$, comma$, routecode$,      ~
                      comma$,  address$(2%), comma$, address$(3%), comma$, ~
                      city$, comma$, state$, comma$, zip$, comma$, sunet1$,~
                      comma$, sunlt1$, comma$, sunet2$, comma$, sunlt2$,   ~
                      comma$,monet1$,comma$,monlt1$,comma$,monet2$, comma$,~
                      monlt2$, comma$, tueet1$, comma$, tuelt1$, comma$,   ~
                      tueet2$, comma$, tuelt2$, comma$, wedet1$, comma$,   ~
                      wedlt1$, comma$, wedet2$, comma$, wedlt2$, comma$,   ~
                      thret1$, comma$, thrlt1$, comma$, thret2$, comma$,   ~
                      thrlt2$, comma$, friet1$, comma$, frilt1$, comma$,   ~
                      friet2$, comma$, frilt2$, comma$, satet1$, comma$,   ~
                      satlt1$, comma$, satet2$, comma$, satlt2$, comma$,   ~
                      contact$, comma$, contphone$, comma$, "TRUE", comma$, " "


L63400:               FMT CH(9), CH(1), CH(8), CH(1), CH(30), CH(1), CH(2), ~
                          CH(1), CH(5), CH(1), CH(30), CH(1), CH(30), CH(1),~
                          CH(18), CH(1), CH(2), CH(1), CH(9), CH(1), CH(4), ~
                          CH(1), CH(4), CH(1), CH(4), CH(1), CH(4), CH(1),  ~ 
                          CH(4), CH(1), CH(4), CH(1), CH(4), CH(1), CH(4),  ~
                          CH(1), CH(4), CH(1), CH(4), CH(1), CH(4), CH(1),  ~
                          CH(4), CH(1), CH(4), CH(1), CH(4), CH(1), CH(4),  ~
                          CH(1), CH(4), CH(1), CH(4), CH(1), CH(4), CH(1),  ~
                          CH(4), CH(1), CH(4), CH(1), CH(4), CH(1), CH(4),  ~
                          CH(1), CH(4), CH(1), CH(4), CH(1), CH(4), CH(1),  ~
                          CH(4), CH(1), CH(4), CH(1), CH(4), CH(1), CH(20), ~
                          CH(1), CH(10), CH(1), CH(5), CH(1), CH(52)
REM         str(custcode$,7%,1%) = " "
REM don't think this is needed when using 9 char customer
        return

/* <AWD004> */ 
write_upload_file9
/*
order$ = order #
fixtime$ = fixed time
rt$ = 
seq$ = sequence
szrest$ = size restriction
eqcode$ = eqcode
ldunits$ = load units
goamt$ = gross open amount
totqty$ = tot qty
ulunits$ = unld units
ugoamt$ = unldgross open amount
utotqty$ = unldtot qty
zone$ = zone
long$ = longitude
lat$  = latitude
sym$  = symbol
size$ = size
clr$  = colour
sel$  = selected
early$ = earliestdate
late$ = latestdate
earlyb$ = earlybuffer
lateb$  = latebuffer
penalty$ = penalty cost 
loadnum$ = load number
cutoff$ = cut off
po$     = PO Num
Shipto$ = ship to
Open1$  = open time
Close1$ = close time
Pattern1$ = days of week
Open2$  = open time
Close2$ = close time
Pattern2$ = days of week
Open3$  = open time
Close3$ = close time
Pattern3$ = days of week
Open4$  = open time
Close4$ = close time
Pattern4$ = days of week
Open5$  = open time
Close5$ = close time
Pattern5$ = days of week
Open6$  = open time
Close6$ = close time
Pattern6$ = days of week
Open7$  = open time
Close7$ = close time
Pattern7$ = days of week
@@@
*/

init(" ") filler$
order$ = "        "
fixtime$ = "     "  
rt$ =    "      " 
seq$ = "   "      
szrest$ = "        "         
eqcode$ = "        "
ldunits$ = "        "
goamt$ = "        "
totqty$ = "        "
ulunits$ = "        "
ugoamt$ = "        "              
utotqty$ = "        " 
zone$ = "    "
long$ = "        "
lat$  = "        "
sym$  = "        "
size$ = "        "
clr$  = "        "
sel$  = "        "
early$ = "        "  
late$ = "        "
earlyb$ = "        "  
lateb$  = "        "
penalty$ = "        "   
loadnum$ = "        " 
cutoff$ = "        "
po$     = "        "
Shipto$ = "        "
Open1$  = "        "
Close1$ = "        "
Pattern1$ = "        "  
Open2$  = "        "
Close2$ = "        "
Pattern2$ = "        "  
Open3$  = "        "
Close3$ = "        "
Pattern3$ = "        "  
Open4$  = "        "
Close4$ = "        "
Pattern4$ = "        "  
Open5$  = "        "
Close5$ = "        " 
Pattern5$ = "        "  
Open6$  = "        "
Close6$ = "        "
Pattern6$ = "        "  
Open7$  = "        "
Close7$ = "        "
Pattern7$ = "        "  
init(" ") rec$()

rec$() =  custcode$ &  comma$ &  order$ &  comma$ & custname$ &  comma$ &  ~
	  cutoff$ &  comma$ &  routecode$ & comma$ &   address$(2%) &      ~
	  comma$ &  address$(3%) &  comma$ & city$ &  comma$ &  state$ &   ~
	  comma$ &  zip$ &  comma$ &  contact$ & comma$ &  contphone$ &    ~
	  comma$ &  "TRUE" &  comma$ & fixtime$ & comma$ & rt$ & comma$ &  ~
	  seq$ & comma$ & szrest$ & comma$ & eqcode$ & comma$ &   ~
	  ldunits$ & comma$ & goamt$ & comma$ & totqty$ & comma$ &         ~
	  ulunits$ & comma$ &       ~
	  ugoamt$ & comma$ & utotqty$ & comma$ & zone$ & comma$ & long$ &  ~
	  comma$ & lat$ & comma$ & sym$ & comma$ & size$ & comma$ & clr$ & ~
	  comma$ & sel$ & comma$ & early$ & comma$ & late$ & comma$ &      ~
	  earlyb$ & comma$ & lateb$ & comma$ & penalty$ & comma$ &         ~
	  loadnum$ & comma$ & cutoff$ & comma$ & po$ & comma$ & Shipto$ &  ~
	  comma$ & Open$(1) & comma$ & Close$(1) & comma$ & Pattern$(1) &  ~
	  comma$ & Open$(2) & comma$ & Close$(2) & comma$ & Pattern$(2) &  ~
	  comma$ & Open$(3) & comma$ & Close$(3) & comma$ & Pattern$(3) &  ~
	  comma$ & Open$(4) & comma$ & Close$(4) & comma$ & Pattern$(4) &  ~
	  comma$ & Open$(5) & comma$ & Close$(5) & comma$ & Pattern$(5) &  ~
	  comma$ & Open$(6) & comma$ & Close$(6) & comma$ & Pattern$(6) &  ~
	  comma$ & Open$(7) & comma$ & Close$(7) & comma$ & Pattern$(7) &  ~
	  comma$ & filler$                                  
          write #4, using L63405, rec$()
L63405:  FMT 2*CH(256)

/*
            write #4, using L63405, custcode$, comma$, order$, comma$,       ~
                  custname$, comma$, cutoff$, comma$, routecode$,        ~
                  comma$,  address$(2%), comma$, address$(3%), comma$,   ~
                  city$, comma$, state$, comma$, zip$, comma$, contact$, ~
                  comma$, contphone$, comma$, "TRUE", comma$,fixtime$,   ~
                  comma$,rt$,comma$,seq$,comma$,szrest$,comma$,          ~
                  comma$,eqcode$,comma$,ldunits$,comma$,goamt$,          ~
                  comma$,totqty$,comma$,ulunits$,comma$,totqty$,         ~
	          comma$,ulunits$,comma$,ugoamt$,comma$,utotqty$,        ~
		  comma$,zone$,comma$,long$,comma$,lat$,comma$,sym$,     ~
		  comma$,size$,comma$,clr$,comma$,sel$,comma$,early$,    ~ 
		  comma$,late$,comma$,earlyb$,comma$,lateb$,comma$,      ~ 
                  penalty$,comma$,loadnum$,comma$,cutoff$,comma$,po$,    ~
		  comma$,Shipto$,comma$,Open$(1),comma$,Close$(1),comma$,~ 
                  Pattern$(1),comma$,Open$(2),comma$,Close$(2),comma$,   ~  
                  Pattern$(2),comma$,Open$(3),comma$,Close$(3),comma$,   ~  
                  Pattern$(3),comma$,Open$(4),comma$,Close$(4),comma$,   ~  
                  Pattern$(4),comma$,Open$(5),comma$,Close$(5),comma$,   ~  
                  Pattern$(5),comma$,Open$(6),comma$,Close$(6),comma$,   ~  
                  Pattern$(6),comma$,Open$(7),comma$,Close$(7),comma$,   ~  
                  Pattern$(7),comma$,filler$                                  

L63405:               FMT CH(9), CH(1), CH(8), CH(1), CH(30), CH(1), CH(2), ~
                          CH(1), CH(5), CH(1), CH(30), CH(1), CH(30), CH(1),~
                          CH(18), CH(1), CH(2), CH(1), CH(9), CH(1), CH(20), ~
                          CH(1), CH(10), CH(1), CH(5), CH(1), CH(8), CH(1),  ~
			  CH(4), CH(1), CH(6), CH(1), CH(6),                ~
			  CH(1), CH(6), CH(1), CH(6), CH(1), CH(6), CH(1),  ~
			  CH(6), CH(1), CH(6), CH(1), CH(6), CH(1), CH(6),  ~
			  CH(1), CH(6), CH(1), CH(6), CH(1), CH(6), CH(1),  ~
			  CH(6), CH(1), CH(6), CH(1), CH(6), CH(1), CH(6),  ~
			  CH(1), CH(6), CH(1), CH(6), CH(1), CH(6), CH(1),  ~
			  CH(6), CH(1), CH(6), CH(1), CH(6), CH(1), CH(6),  ~
			  CH(1), CH(6), CH(1), CH(4), CH(1), CH(4), CH(1),  ~
			  CH(7), CH(1), CH(4), CH(1), CH(4), CH(1), CH(7),  ~
			  CH(1), CH(4), CH(1), CH(4), CH(1), CH(7), CH(1),  ~
			  CH(4), CH(1), CH(4), CH(1), CH(7), CH(1), CH(4),  ~
			  CH(1), CH(4), CH(1), CH(7), CH(1), CH(4), CH(1),  ~
			  CH(4), CH(1), CH(7), CH(1), CH(4), CH(1), CH(4),  ~
			  CH(1), CH(7), CH(1), CH(33)                         
*/
REM         str(custcode$,7%,2%) = " "
        return
/* </AWD004> */ 

        write_header
/* <AWD004> */ 
	    if appian% = 9% then goto write_header9
/* </AWD004> */ 

            write #3, using L63410, "Acct#", comma$, "Order#", comma$,      ~
                      "Name", comma$, "Cutoff", comma$, "RtCode", comma$,   ~
                      "Address", comma$, "Address2", comma$, "City", comma$,~
                      "State", comma$, "Zip", comma$, "SUNET1", comma$,     ~
                      "SUNLT1", comma$, "SUNET2", comma$, "SUNLT2", comma$, ~
                      "MONET1", comma$, "MONLT1", comma$, "MONET2", comma$, ~
                      "MONLT2", comma$, "TUEET1", comma$, "TUELT1", comma$, ~
                      "TUEET2", comma$, "TUELT2", comma$, "WEDET1", comma$, ~
                      "WEDLT1", comma$, "WEDET2", comma$, "WEDLT2", comma$, ~
                      "THUET1", comma$, "THULT1", comma$, "THUET2", comma$, ~
                      "THULT2", comma$, "FRIET1", comma$, "FRILT1", comma$, ~
                      "FRIET2", comma$, "FRILT2", comma$, "SATET1", comma$, ~
                      "SATLT1", comma$, "SATET2", comma$, "SATLT2", comma$, ~
                      "Contact", comma$, "Phone", comma$, "CloseTW", comma$," "


L63410:               FMT CH(5), CH(1), CH(6), CH(1), CH(4), CH(1), CH(6),  ~
                          CH(1), CH(6), CH(1), CH(7), CH(1), CH(8), CH(1),  ~
                          CH(4), CH(1), CH(5), CH(1), CH(3), CH(1), CH(6),  ~
                          CH(1), CH(6), CH(1), CH(6), CH(1), CH(6), CH(1),  ~ 
                          CH(6), CH(1), CH(6), CH(1), CH(6), CH(1), CH(6),  ~
                          CH(1), CH(6), CH(1), CH(6), CH(1), CH(6), CH(1),  ~
                          CH(6), CH(1), CH(6), CH(1), CH(6), CH(1), CH(6),  ~
                          CH(1), CH(6), CH(1), CH(6), CH(1), CH(6), CH(1),  ~
                          CH(6), CH(1), CH(6), CH(1), CH(6), CH(1), CH(6),  ~
                          CH(1), CH(6), CH(1), CH(6), CH(1), CH(6), CH(1),  ~
                          CH(6), CH(1), CH(6), CH(1), CH(6), CH(1), CH(7),  ~
                          CH(1), CH(5), CH(1), CH(7), CH(1), CH(106)

        return

write_header9
            write #4, using L63420,"Acct#",comma$,"Order#",comma$,    ~
		  "Name",comma$,"Cutoff",comma$,"RtCode",comma$,      ~
		  "Address",comma$,"Address2",comma$,"City",comma$,   ~
		  "State",comma$,"Zip",comma$,"Contact",comma$,       ~
		  "Phone",comma$,"CloseTW",comma$,"FixedTime",comma$, ~
		  "Rt",comma$,"Seq",comma$,"SzRestriction",comma$,    ~
		  "EqCode",comma$,"Load Units",comma$,                ~
		  "Gross Open Amount",comma$,"Tot Qty",comma$,        ~
		  "UnldLoad Units",comma$,"UnldGross Open Amount",comma$, ~
		  "UnldTot Qty",comma$,"Zone",comma$,"Longitude",comma$,  ~
		  "Latitude",comma$,"Symbol",comma$,"Size",comma$,        ~
		  "Color",comma$,"Selected",comma$,"EarliestDate",comma$, ~
		  "LatestDate",comma$,"EarlyBuffer",comma$,               ~
		  "LateBuffer",comma$,"PenaltyCost",comma$,               ~
		  "Load Num",comma$,"Cut Off",comma$,"PO Num",comma$,     ~
		  "Ship To",comma$,"Open1",comma$,"Close1",comma$,        ~
		  "Pattern1",comma$,"Open2",comma$,"Close2",comma$,       ~
		  "Pattern2",comma$,"Open3",comma$,"Close3",comma$,       ~
		  "Pattern3",comma$,"Open4",comma$,"Close4",comma$,       ~
		  "Pattern4",comma$,"Open5",comma$,"Close5",comma$,       ~
		  "Pattern5",comma$,"Open6",comma$,"Close6",comma$,       ~
		  "Pattern6",comma$,"Open7",comma$,"Close7",comma$,       ~
		  "Pattern7",comma$," "

L63420:  FMT CH(5), CH(1), CH(6), CH(1), CH(4), CH(1), CH(6), CH(1),    ~
	     CH(6), CH(1), CH(7), CH(1), CH(8), CH(1), CH(4), CH(1),    ~
	     CH(5), CH(1), CH(3), CH(1), CH(7), CH(1), CH(5), CH(1),    ~
	     CH(7), CH(1), CH(9), CH(1), CH(2), CH(1), CH(3), CH(1),    ~
	     CH(13), CH(1), CH(6), CH(1), CH(10), CH(1), CH(17), CH(1), ~
	     CH(7), CH(1), CH(14), CH(1), CH(21), CH(1), CH(11), CH(1), ~
	     CH(4), CH(1), CH(9), CH(1), CH(8), CH(1), CH(6), CH(1),    ~
	     CH(4), CH(1), CH(5), CH(1), CH(8), CH(1), CH(12), CH(1),   ~
	     CH(10), CH(1), CH(11), CH(1), CH(10), CH(1), CH(11), CH(1), ~
	     CH(8), CH(1), CH(7), CH(1), CH(6), CH(1), CH(7), CH(1),    ~
	     CH(5), CH(1), CH(6), CH(1), CH(8), CH(1), CH(5), CH(1),   ~
	     CH(6), CH(1), CH(8), CH(1), CH(5), CH(1), CH(6), CH(1),   ~
	     CH(8), CH(1), CH(5), CH(1), CH(6), CH(1), CH(8), CH(1),   ~
	     CH(5), CH(1), CH(6), CH(1), CH(8), CH(1), CH(5), CH(1),   ~
	     CH(6), CH(1), CH(8), CH(1), CH(5), CH(1), CH(6), CH(1),   ~
	     CH(8), CH(1), CH(6) 

        return

        lookup_days
            cust_cnt% = 0%
            init(" ") sunet1$, sunlt1$, sunet2$, sunlt2$, monet1$, monlt1$,~
            monet2$, monlt2$, tueet1$, tuelt1$, tueet2$, tuelt2$, wedet1$, ~
	    wedlt1$, wedet2$, wedlt2$, thret1$, thrlt1$, thret2$, thrlt2$, ~
            friet1$, frilt1$, friet2$, frilt2$, satet1$, satlt1$, satet2$, ~
            satlt2$

            twice_wk% = 0%                                     /*  (EWD001)  */
            if cutoff1$ = " " then goto not_twice_wk
               call "DAY" addr(wk_date$,day%)
               day% = day% - 1%
REM               day% = j%
               if day% = 0% then day% = 7%
               convert cutoff$ to cutoff%, data goto no_cutoff

no_cutoff:     if cutoff% > 5% then cutoff% = cutoff% - 5%
               convert cutoff1$ to cutoff1%, data goto no_cutoff1

no_cutoff1:    convert cutoff% to cutoff$, pic(00)
               if cutoff1% > 5% then cutoff1% = cutoff1% - 5%
               convert cutoff1% to cutoff1$,pic(00)

               cutoff_twice$ = cutoff$

REM               if day% > cutoff% and day% <= cutoff1%            ~
                         then cutoff_twice$ = cutoff1$
REM              if day% = 3% or day% = 4% then cutoff_twice$ = cutoff1$
REM              if day% = 2% then cutoff_twice$ = cutoff1$

              if day% = 3% or day% = 4% then cutoff_twice$ = cutoff1$
              if day% = 5% then cutoff_twice$ = cutoff1$

               twice_wk% = 1%

not_twice_wk:                                                /*  (EWD001)  */
	    if appian% = 9% then gosub lookup_days9      
	    if appian% = 9% then goto  L63465            
            init(" ") genkey$
            str(genkey$,1%,9%) = "EWDAPPCUS"
            str(genkey$,10%,9%) = custcode$
            for i% = 1% to 7%
                convert i% to str(genkey$,19%,2%), pic(00)
                read #2, key = genkey$, using L63450, gen_desc$,            ~
                                                       eod goto L63460           
L63450:                  FMT POS(25), CH(30)
                if str(gen_desc$,1%,4%) = "0000" then                     ~ 
                                          str(gen_desc$,1%,4%) = " "
                if str(gen_desc$,5%,4%) = "0000" then                     ~ 
                                          str(gen_desc$,5%,4%) = " "
                if str(gen_desc$,10%,4%) = "0000" then                    ~ 
                                          str(gen_desc$,10%,4%) = " "
                if str(gen_desc$,14%,4%) = "0000" then                    ~ 
                                          str(gen_desc$,14%,4%) = " "

                


                gosub load_cust_data
L63460:     next i% 
L63465:     gosub write_upload_file
	    if appian% = 9% then return                  
            if cust_cnt% = 0% then goto no_gen_customer
        return
        no_gen_customer
          errormsg$ = "No Data in EWDAPPCUS for Customer " & str(genkey$,10%,9%)
          gosub error_prompt
        return

        load_cust_data                                          /*  (EWD001)  */
           cust_cnt% = cust_cnt% + 1%
           if twice_wk% = 0% then goto not_twice 
              if (cutoff$ = "01" )   and i% <= 2% then goto not_twice
              if (cutoff1$ = "04" ) and i% >  3% then goto not_twice

               if cutoff_twice$ = "01" and i% > 2% then return
               if cutoff_twice$ = "04" and                               ~
                          (i% <> 3% and i% <> 4% and i% <> 5%) then return


REM               if cutoff_twice$ = "01" and i% < 4% then return
REM               if cutoff_twice$ = "04" and                            ~
                          (i% <> 1% and i% <> 2% and i% <> 3%) then return

REM               if cutoff_twice$ = "01" and i% > 2% then return
REM               if cutoff_twice$ = "04" and i% < 3% then return


not_twice:
                                                                /*  (EWD001)  */
            if i% <> 1% then goto not_one
               if str(gen_desc$,1%,4%) <> " " then ~
               monet1$ = str(gen_desc$,1%,4%)
               if str(gen_desc$,5%,4%) <> " " then ~
               monlt1$ = str(gen_desc$,5%,4%)
               if str(gen_desc$,10%,4%) <> " " then ~
               monet2$ = str(gen_desc$,10%,4%)
               if str(gen_desc$,14%,4%) <> " " then ~
               monlt2$ = str(gen_desc$,14%,4%)
               gosub'100(monet1$,monlt1$,monet2$,monlt2$)
        not_one
            if i% <> 2% then goto not_two
               tueet1$ = str(gen_desc$,1%,4%)
               tuelt1$ = str(gen_desc$,5%,4%)
               tueet2$ = str(gen_desc$,10%,4%)
               tuelt2$ = str(gen_desc$,14%,4%)
               gosub'200(tueet1$,tuelt1$,tueet2$,tuelt2$)
        not_two
            if i% <> 3% then goto not_three
               wedet1$ = str(gen_desc$,1%,4%)
               wedlt1$ = str(gen_desc$,5%,4%)
               wedet2$ = str(gen_desc$,10%,4%)
               wedlt2$ = str(gen_desc$,14%,4%)
               gosub'300(wedet1$,wedlt1$,wedet2$,wedlt2$)
        not_three
            if i% <> 4% then goto not_four
               thret1$ = str(gen_desc$,1%,4%)
               thrlt1$ = str(gen_desc$,5%,4%)
               thret2$ = str(gen_desc$,10%,4%)
               thrlt2$ = str(gen_desc$,14%,4%)
               gosub'400(thret1$,thrlt1$,thret2$,thrlt2$)
        not_four
            if i% <> 5% then goto not_five
               friet1$ = str(gen_desc$,1%,4%)
               frilt1$ = str(gen_desc$,5%,4%)
               friet2$ = str(gen_desc$,10%,4%)
               frilt2$ = str(gen_desc$,14%,4%)
               gosub'500(friet1$,frilt1$,friet2$,frilt2$)
        not_five
            if i% <> 6% then goto not_six
               satet1$ = str(gen_desc$,1%,4%)
               satlt1$ = str(gen_desc$,5%,4%)
               satet2$ = str(gen_desc$,10%,4%)
               satlt2$ = str(gen_desc$,14%,4%)
               gosub'600(satet1$,satlt1$,satet2$,satlt2$)
        not_six
            if i% <> 7% then goto not_seven
               sunet1$ = str(gen_desc$,1%,4%)
               sunlt1$ = str(gen_desc$,5%,4%)
               sunet2$ = str(gen_desc$,10%,4%)
               sunlt2$ = str(gen_desc$,14%,4%)
               gosub'700(sunet1$,sunlt1$,sunet2$,sunlt2$)
        not_seven
        return

lookup_days9                                        
        order$ = "        "
        fixtime$ = "     "  
        rt$ =    "      " 
        seq$ = "   "      
        szrest$ = "        "         
        eqcode$ = "        "
        ldunits$ = "        "
        goamt$ = "        "
        totqty$ = "        "
        ulunits$ = "        "
        ugoamt$ = "        "              
        utotqty$ = "        " 
        zone$ = "    "
        long$ = "        "
        lat$  = "        "
        sym$  = "        "
        size$ = "        "
        clr$  = "        "
        sel$  = "        "
        early$ = "        "  
        late$ = "        "
        earlyb$ = "        "  
        lateb$  = "        "
        penalty$ = "        "   
        loadnum$ = "        " 
        cutoff$ = "        "
        po$     = "        "
        Shipto$ = "        "
        Open1$  = "        "
        Close1$ = "        "
        Pattern1$ = "        "  
        Open2$  = "        "
        Close2$ = "        "
        Pattern2$ = "        "  
        Open3$  = "        "
        Close3$ = "        "
        Pattern3$ = "        "  
        Open4$  = "        "
        Close4$ = "        "
        Pattern4$ = "        "  
        Open5$  = "        "
        Close5$ = "        " 
        Pattern5$ = "        "   
        Open6$  = "        "
        Close6$ = "        "
        Pattern6$ = "        "  
        Open7$  = "        "
        Close7$ = "        "
        Pattern7$ = "        "  

            init(" ") genkey$, open$(), close$(), pattern$()
            str(genkey$,1%,9%) = "AWDAPPCUS"
            str(genkey$,10%,1%) = "0"         
            if twice_wk% = 1% then str(genkey$,10%,1%) = cutoff1$ 

            str(genkey$,11%,9%) = custcode$
            for i% = 1% to 7%
                convert i% to str(genkey$,20%,1%), pic(0)
                read #2, key = genkey$, using L63450, gen_desc$,            ~
                                                       eod goto L63462           
                if str(gen_desc$,1%,4%) = "0000" then                     ~ 
                                          str(gen_desc$,1%,4%) = " "
                if str(gen_desc$,5%,4%) = "0000" then                     ~ 
                                          str(gen_desc$,5%,4%) = " "
                  
                time1$   = str(gen_desc$,1,4) 
                convert time1$ to field1%, data goto app9_1  
                convert field1% to time1$, pic(####)
                call "SPCESMSH" (time1$, 0%)
                open$(i%) = time1$              
app9_1:         time1$   = str(gen_desc$,5,4) 
                convert time1$ to field1%, data goto app9_2  
                convert field1% to time1$, pic(####)
                call "SPCESMSH" (time1$, 0%)
                close$(i%) = time1$  
app9_2:         pattern$(i%) = str(gen_desc$,10,7) 

                gosub load_cust_data
L63462:     next i% 
            if cust_cnt% = 0% then goto no_gen_customer9
        return
        no_gen_customer9
          errormsg$ = "No Data in AWDAPPCUS for Customer " & str(genkey$,10%,9%)
          gosub error_prompt
        return


        deffn'100(monet1$,monlt1$,monet2$,monlt2$)
              convert monet1$ to field1%, data goto field_1
        
              convert field1% to monet1$, pic(####)
field_1
              convert monlt1$ to field2%, data goto field_2
        
              convert field2% to monlt1$, pic(####)
field_2
              convert monet2$ to field3%, data goto field_3
        
              convert field3% to monet2$, pic(####)
field_3
              convert monlt2$ to field4%, data goto field_4
        
              convert field4% to monlt2$, pic(####)
field_4

               gosub '900(monet1$,monlt1$,monet2$,monlt2$)
        return

        deffn'200(tueet1$,tuelt1$,tueet2$,tuelt2$)
              convert tueet1$ to field1%, data goto field_1b
        
              convert field1% to tueet1$, pic(####)
field_1b
              convert tuelt1$ to field2%, data goto field_2b
        
              convert field2% to tuelt1$, pic(####)
field_2b
              convert tueet2$ to field3%, data goto field_3b
        
              convert field3% to tueet2$, pic(####)
field_3b
              convert tuelt2$ to field4%, data goto field_4b
        
              convert field4% to tuelt2$, pic(####)
field_4b

               gosub '900(tueet1$,tuelt1$,tueet2$,tuelt2$)
        return

        deffn'300(wedet1$,wedlt1$,wedet2$,wedlt2$)
              convert wedet1$ to field1%, data goto field_1c
        
              convert field1% to wedet1$, pic(####)
field_1c
              convert wedlt1$ to field2%, data goto field_2c
        
              convert field2% to wedlt1$, pic(####)
field_2c
              convert wedet2$ to field3%, data goto field_3c
        
              convert field3% to wedet2$, pic(####)
field_3c
              convert wedlt2$ to field4%, data goto field_4c
        
              convert field4% to wedlt2$, pic(####)
field_4c

               gosub '900(wedet1$,wedlt1$,wedet2$,wedlt2$)
        return

        deffn'400(thret1$,thrlt1$,thret2$,thrlt2$)
              convert thret1$ to field1%, data goto field_1d
        
              convert field1% to thret1$, pic(####)
field_1d
              convert thrlt1$ to field2%, data goto field_2d
        
              convert field2% to thrlt1$, pic(####)
field_2d
              convert thret2$ to field3%, data goto field_3d
        
              convert field3% to thret2$, pic(####)
field_3d
              convert thrlt2$ to field4%, data goto field_4d
        
              convert field4% to thrlt2$, pic(####)
field_4d
               gosub '900(thret1$,thrlt1$,thret2$,thrlt2$)

        return

        deffn'500(friet1$,frilt1$,friet2$,frilt2$)
              convert friet1$ to field1%, data goto field_1e
        
              convert field1% to friet1$, pic(####)
field_1e
              convert frilt1$ to field2%, data goto field_2e
        
              convert field2% to frilt1$, pic(####)
field_2e
              convert friet2$ to field3%, data goto field_3e
        
              convert field3% to friet2$, pic(####)
field_3e
              convert frilt2$ to field4%, data goto field_4e
        
              convert field4% to frilt2$, pic(####)
field_4e

               gosub '900(friet1$,frilt1$,friet2$,frilt2$)
        return

        deffn'600(satet1$,satlt1$,satet2$,satlt2$)
              convert satet1$ to field1%, data goto field_1f
        
              convert field1% to satet1$, pic(####)
field_1f
              convert satlt1$ to field2%, data goto field_2f
        
              convert field2% to satlt1$, pic(####)
field_2f
              convert satet2$ to field3%, data goto field_3f
        
              convert field3% to satet2$, pic(####)
field_3f
              convert satlt2$ to field4%, data goto field_4f
        
              convert field4% to satlt2$, pic(####)
field_4f

               gosub '900(satet1$,satlt1$,satet2$,satlt2$)
        return

        deffn'700(sunet1$,sunlt1$,sunet2$,sunlt2$)
              convert sunet1$ to field1%, data goto field_1g
        
              convert field1% to sunet1$, pic(####)
field_1g
              convert sunlt1$ to field2%, data goto field_2g
        
              convert field2% to sunlt1$, pic(####)
field_2g
              convert sunet2$ to field3%, data goto field_3g
        
              convert field3% to sunet2$, pic(####)
field_3g
              convert sunlt2$ to field4%, data goto field_4g
        
              convert field4% to sunlt2$, pic(####)
field_4g

               gosub '900(sunet1$,sunlt1$,sunet2$,sunlt2$)
        return


        deffn'900(time1$,time2$,time3$,time4$)

rem               call "STRING" addr("LJ", time1$, 4%)

rem               call "STRING" addr("LJ", time2$, 4%)

rem               call "STRING" addr("LJ", time3$, 4%)

rem               call "STRING" addr("LJ", time4$, 4%)

              call "SPCESMSH" (time1$, 0%)
              call "SPCESMSH" (time2$, 0%)
              call "SPCESMSH" (time3$, 0%)
              call "SPCESMSH" (time4$, 0%)


        return

        load_exclude_customers
            cnt% = 0%
            init(" ") genkey$
            str(genkey$,1%,9%) = "EWDAPPEXC"
        exclude_nxt
            read #2, key > genkey$, using L63550, genkey$,               ~
                                              eod goto exclude_done
L63550:                  FMT CH(24)

            if str(genkey$,1%,9%) <> "EWDAPPEXC" then goto exclude_done
            cnt% = cnt% + 1%
            str(exc_cust$(cnt%),1%,9%) = str(genkey$,10%,9%)
            goto exclude_nxt
        exclude_done
            cnt_max% = cnt%
        return

        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_file
            init(" ") library$, volume$, file$
            library$        = "APPIAN  "
* (AWD002) - Next two lines
            volume$         = "DEVEL" 
            if schema% = 1% then volume$         = "CARLO2"
            if schema% = 2% then volume$         = "NE2"
/* <AWD004> */ 
            if schema% = 0% then schema% = 1%               
	    if appian% = 9% then goto open_file9        
/* </AWD004> */ 
            file$   = "AWDAPPCU"
             open nodisplay #3, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return
       
/* <AWD004> */ 
open_file9
	    file$   = "AWDAPPCU"
             open nodisplay #4, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return
/* </AWD004> */ 

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
