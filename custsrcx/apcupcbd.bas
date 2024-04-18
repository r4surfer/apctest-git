        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   U   U  PPPP    CCC   BBBB   DDDDD   *~
            *  A   A  P   P  C   C  U   U  P   P  C   C  B   B  D    D  *~
            *  AAAAA  PPPP   C      U   U  PPPP   C      BBBB   D    D  *~
            *  A   A  P      C   C  U   U  P      C   C  B   B  D    D  *~
            *  A   A  P       CCC    UUU   P       CCC   BBBB   DDDDD   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCUPCBD - Build UPC Bar Code for Finished Goods Products *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/05/90 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/12/97 ! Modified For Revision 60403              ! DJD *~
            *          !                                          !     *~
            * 01/30/06 ! (PAR000) CR347 Mod for new Sub Part No.  ! RHH *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            part_key$45,                 /* Primary Key        (PAR000)*/~
            partno$25,                   /* Part Number                */~
            sub_partno$20,               /* New Part Number    (PAR000)*/~ 
            desc$32,                     /* Part Number Description    */~
            upccode$20,                  /* UPC Code                   */~
            code$12,                     /* Actual Bar Code            */~
            nxtupc$8,                    /* Next Code To Assign        */~
            check$4,                     /* Calc Check Digit           */~
            upc$5,                       /* Assigned Code              */~
            storeno$3,                   /* Store Number               */~
            readkey$24,                  /* Gencodes Key               */~
            up%(11%),                    /* CALC ARRAY                 */~
            company$60,                  /* For Report Company Name    */~
            print_title$60,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(4%),                     /* = 0 if the file is open    */~
            f1%(4%),                     /* = 1 if READ was successful */~
            fs%(4%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(4%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "REV: 01.00 01/30/06 AWD New Program              "
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
            * #01 ! STORNAME ! Master Store File                        *~
            * #02 ! INVMASTR ! Part Master File New File        (PAR000)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3
                                                           /* (PAR000) */
            select #2,  "INVMASTR",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =  45,                     ~
                        alt key  1, keypos =  122, keylen =   9, dup,    ~
                            key  2, keypos =  110, keylen =   4, dup,    ~
                            key  3, keypos =   46, keylen =  32, dup
                                                           /* (PAR000) */         

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),  0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))

            f1%(1), f1%(2) = 0%

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

            for fieldnr% = 1% to  1%
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
L10210:               if keyhit% = 14% then gosub print_report
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub build_upc
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub select_printer
            part_key$ = all(hex(00))                       /* (PAR000) */
        next_part
            read #2,key > part_key$,using L19110,partno$, sub_partno$,   ~
                                                         eod goto L19170
L19110:       FMT CH(25), CH(20)
            str(part_key$,1%,25%)  = partno$
            str(part_key$,26%,20%) = sub_partno$
                                                           /* (PAR000) */
            if len(partno$) < 19% then goto next_part
            get #2, using L35180, desc$, upccode$
            if len(upccode$) <> 12% then upccode$ = "<---BLANK-->"
            if len(upccode$) = 12% then gosub print_detail
               goto next_part

L19170:     gosub close_printer
        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20190          /* Part Number           */

         return

L20190: REM Part Number                            PARTNO$
        REM PARTNO$ = " "
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
         "Enter Valid Finished Goods Part Number.                       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, partno$, storeno$,         ~
                      upccode$, readkey$, desc$, nxtupc$, code$, check$, ~
                      upc$, part_key$, sub_partno$
                                                           /* (PAR000) */  
        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
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
        REM  DATALOAD
        REM  RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        dataput
          rec% = 0%
          read #1,hold,key = storeno$, eod goto L31210
            put #1, using L35040, nxtupc$
            rewrite #1
                                                           /* (PAR000) */
          read #2,hold,key = part_key$, eod goto L31210
            put #2, using L35180, desc$, upccode$
            rewrite #2
        return
L31210:   rec% = 1%
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* STORNAME - Store File      */
L35040:     FMT POS(186),                /* Skip                       */~
                CH(08)                   /* Next Bar Code Number       */
                                         /* From Next S. O. Field      */

                                         /* HNYMASTR - Part Master     */
L35180:     FMT POS(46),                 /* Skip                       */~
                CH(32),                  /* Part Description           */~
                POS(586),                /* Skip to UPC Code           */~
                CH(20)                   /* UPC Bar Code               */

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
              on fieldnr% gosub L40150          /* Part Number       */

              goto L40190

L40150:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02),                                               ~
                  "UPC Bar Code - Build Utility ",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number :",                              ~
               at (06,20), fac(lfac$(1%)), partno$              , ch(25),~
               at (06,47), fac(lfac$(1%)), sub_partno$          , ch(20),~ 
                                                                         ~
               at (07,02), "Description :",                              ~
               at (07,20), fac(hex(84)),   desc$                , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40400
                  call "PRNTSCRN"
                  goto L40190

L40400:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40590     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40550
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40550:     if fieldnr% > 1% then L40570
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40570:     return

L40590: if fieldnr% > 0% then L40680  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Create Code "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40680:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50120          /* Part Number           */

            return

L50120: REM Part Number                           Part Number
            if partno$ <> " " then goto L50200
               desc$ = hex(06) & "Select Part Number "
               call "GETCODE" (#2, part_key$, desc$, 0%, 1.32, f1%(2%))
               partno$     = str(part_key$,1%,25%)
               sub_partno$ = str(part_key$,26%,20%)
               desc$ = " "
               if f1%(2%) <> 0 then goto L50200
L50180:           errormsg$ = "Invalid Part Number Selection"
                  gosub error_prompt
                  return
L50200:     part_key$ = all(hex(00))
            str(part_key$,1%,25%)  = partno$
            str(part_key$,26%,20%) = sub_partno$
            if len(partno$) < 19% then goto L50180

            read #2,hold,key = part_key$,using L35180, desc$, upccode$,     ~
                                                   eod goto L50180
            if upccode$ <> " " then goto L50260
               partno$     = str(part_key$,1%,25%)
               sub_partno$ = str(part_key$,26%,20%)
               return

L50260:     errormsg$ = "UPC Code already Defined ( " & upccode$ & " )"
            gosub error_prompt
         return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %######## ########                   ############################~
        ~################################                        APCUPCBD:

L55080: %USER ID: ###                        ############################~
        ~################################                      PAGE: #####
                                                   /* COLUMN HEADER   */
L55110: % <--------------- PART NUMBER --------------->    <-------- DESC~
        ~RIPTION --------->            <-- UPC BAR CODE -->

L55140: % ---------------------------------------------    --------------~
        ~------------------            --------------------

                                                   /* DETAIL        */
L55180: % #############################################    ##############~
        ~##################            ####################

        print_header

          page_no% = page_no% + 1%
          print page
          print using L55050, date$, rpt_time$, company$
          print using L55080, userid$, print_title$, page_no%
          print
          print using L55110
          print using L55140
          lcnt% = 5%
        return

        print_detail
          if lcnt% > 60% then gosub print_header
           print using L55180, part_key$, desc$, upccode$
          lcnt% = lcnt% + 1%
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SHOSTAT" ("Creating UPC Code's Report")
            page_no% = 0%
            lcnt%    = 99%
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCUPC", " ", 0%, 0%)
            print_title$ = "Parts Assigned UPC Codes"
            call "FMTTITLE" (print_title$, " ", 12%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCUPC", " ", 0%, 1%)
        return

        build_upc
          storeno$ = "000"
                                             /* Next Code from S.O. */
          read #1,hold,key = storeno$,using L35040,nxtupc$,eod goto L60560
          convert nxtupc$ to nxtupc%, data goto L60540

          upc% = nxtupc%                     /* Save Number         */
          nxtupc% = nxtupc% + 1%             /* Set Next Number     */
          convert nxtupc% to nxtupc$, pic(00000000) /* Reset No.    */
          convert upc% to upc$, pic(00000)

          code$ = "719801" & upc$ & "0"      /* Build Code          */
          for i% = 1% to 11%                 /* Convert Digits      */
            up%(i%) = 0%
            convert str(code$,i%,1%) to up%(i%),data goto L60540

          next i%
          x%, y%, z% = 0%
          x% = up%(1%) + up%(3%) + up%(5%) + up%(7%) + up%(9%) + up%(11%)
          y% = up%(2%) + up%(4%) + up%(6%) + up%(8%) + up%(10%)
          z% = (3% * x%) + y%
          convert z% to check$,pic(0000)
          convert str(check$,4%,1%) to z%, data goto L60540

          z% = 10% - z%                      /* Calculate Check Digit */
          if z% = 10% then z% = 0%
          convert z% to str(code$,12%,1%), pic(0)
          str(upccode$,1%,12%) = code$
          gosub dataput
          if rec% <> 0% then goto L60580
          hdr$     = "*********** UPC Code Updated ***********"
          msg$(1%) = " - - - - - - - " & str(upccode$,1%,12%) & "- - - - - - - - "
          init (" ") errormsg$ 
          gosub error_prompt_1 
        return clear all
        goto inputmode

L60540:   errormsg$ = "Build Error - Data Conversion Error"
        gosub error_prompt
        return
L60560:   errormsg$ = "Build Error - Unable to Read Store (000)"
        gosub error_prompt 
        return
L60580:   errormsg$ = "Update Error - Unable to Update Data"
        gosub error_prompt
        return

        error_prompt
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
        error_prompt_1
           comp% = 2%   
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
