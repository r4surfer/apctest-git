        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCEQUIP                             *~
            *  Creation Date     - 02/20/95                             *~
            *  Last Modified Date- 11/10/97                             *~
            *  Description       - This Program Creates a Computer      *~
            *                      Equiptment Data Base for Record      *~
            *                      Keeping and Maintenance.             *~
            *                                                           *~
            *  Code Tables Used  - (APC  EQ01) - OFFICE LOCATION CODES  *~
            *                      (APC  EQ02) - PATCH LOCATION CODES   *~
            *                      (APC  EQ03) - DTC LOCATION CODES     *~
            *                                                           *~
            *  Special Comments  - PF(3) Print User ID Report           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/20/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 08/25/95 ! Mod for printer user id report PF(3)     ! RHH *~
            *          !                                          !     *~
            * 11/10/97 ! Change revision number to 60403          ! DJD *~
            *          !                                          !     *~
            *************************************************************

        dim eq_key$30,                         /* LOOKUP KEY        */   ~
            eq_ctr$4, eq_ctr_t$4,              /* APC Control Number*/   ~
            eq_name$20,                        /* System Name       */   ~
            eq_off_loc$2, eq_off_desc$25,      /* Office Location   */   ~
            eq_off_jack$5,                     /* Office Data Jack  */   ~
            eq_pat_loc$2, eq_pat_desc$25,      /* Patch Location    */   ~
            eq_pat_jack$5,                     /* Patch Data Jack   */   ~
            eq_dtc_loc$2, eq_dtc_desc$25,      /* DTC Location      */   ~
            eq_dtc_jack$2,                     /* DTC Port Number   */   ~
            eq_dtc_baud$5,                     /* DTC Port Baud Rate*/   ~
            eq_com_ser$15,                     /* Computer Serial No*/   ~
            eq_mon_ser$15,                     /* Monitor Serial No.*/   ~
            eq_prt_ser$15,                     /* Printer Serial No.*/   ~
            eq_proc$40,                        /* Processor Info.   */   ~
            eq_disk$40,                        /* Hard Disk Info.   */   ~
            eq_ext_disk$40,                    /* Removable Disk Inf*/   ~
            eq_monitor$40,                     /* Monitor Info      */   ~
            eq_network$40,                     /* Network Info      */   ~
            eq_software$40,                    /* Software Info     */   ~
            eq_date$8, eq_dte$6,               /* Last Mod Date     */   ~
            eq_usr$3,                          /* Last User Mod      */  ~
            eq_fil$56,                         /* Filler Area        */  ~
            readkey$24, desc$32,         /* GENCODES                   */~
            tab_hdr$30,                  /* DISPLAY SCREEN HEADER      */~
            cc$(35%)4,                   /* CODE DISPLAY               */~
            dd$(35)32,                   /* DISCRIPTION DISPLAY        */~
            sav_key$9,                   /* SAVE TABLE NAME            */~
            user_id$3,                   /* User Id                    */~
            user_name$30,                /* User Name                  */~
            user_menu$16,                /* User Menu Name             */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8, title$40,            /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim dtc_no$2, dtc_desc$30,       /* DTC Number                 */~
            dtc_baud$(16%)5, dtc_port$2, /* DTC Baud Rate - Ports      */~
            dtc_type$(16%)1,             /* DTC Types - Ports          */~
            dtc_conn$(16%)5,             /* DTC Connections - Port     */~
            dtc_comm$(16%)20,            /* DTC Comments - Ports       */~
            dtc_fil$15                   /* DTC Filler Area            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/10/97 APC Computer Equipt Doc.       "
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
            * #1  ! APCEQUIP ! APC Computer Equiptment Master           *~
            * #2  ! GENCODES ! System Code Table File                   *~
            * #3  ! APCEQDTC ! APC Computer DTC Master                  *~
            * #4  ! APCEQUIP ! APC Computer Equiptment Master(LOOKUP)   *~
            * #5  ! USERLCMS ! CAELUS MASTER USER ID FILE               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCEQUIP",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =   4,                     ~
                        alt key  1, keypos =   5, keylen = 20,           ~
                            key  2, keypos =  25, keylen =  7, dup,      ~
                            key  3, keypos =  32, keylen =  7, dup,      ~
                            key  4, keypos =  39, keylen =  4, dup,      ~
                            key  5, keypos =  48, keylen = 15, dup,      ~
                            key  6, keypos =  63, keylen = 15, dup,      ~
                            key  7, keypos =  78, keylen = 15, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #3,  "APCEQDTC",                                      ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos = 1,    keylen =  2

            select #4,  "APCEQUIP",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =   4,                     ~
                        alt key  1, keypos =   5, keylen = 20,           ~
                            key  2, keypos =  25, keylen =  7, dup,      ~
                            key  3, keypos =  32, keylen =  7, dup,      ~
                            key  4, keypos =  39, keylen =  4, dup,      ~
                            key  5, keypos =  48, keylen = 15, dup,      ~
                            key  6, keypos =  63, keylen = 15, dup,      ~
                            key  7, keypos =  78, keylen = 15, dup

            select #5,  "USERLCMS",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen = 30, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 100%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(3%),   0%, rslt$(4%))
        REM CALL "OPENCHCK" (#5, FS%(5%), F2%(5%),   0%, RSLT$(5%))
            call "OPENOLIB" (#5,"SHARE", f2%(5%), rslt$(5%), axd$ )
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

            str(line2$,62) = "APCEQUIP: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 18%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% =  6% and fieldnr% = 1% then inp_dtc
                      if keyhit% = 14% then gosub gen_eq
                      if keyhit% <> 0% then       L10120
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 12% then gosub delete_rec
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% <> 1% then goto L11170
               if cursor%(2%) > 30% then fieldnr% = 2%
               goto L11220
L11170:     fieldnr% = fieldnr% + 1%
            if fieldnr% > 8% then fieldnr% = fieldnr% + 1%
            if fieldnr% = 8% and cursor%(2%) > 30% then                  ~
                                                fieldnr% = fieldnr% + 1%

L11220:     if fieldnr% < 1% or fieldnr% > 18% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11260:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11260
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11260
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *       I N P U T   M O D E   R E P O R T                    ~
            *************************************************************

        inp_dtc
            gosub initialize_variables
            for fieldnr% = 1% to  3%
L12070:         gosub'061(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12200
L12090:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12170
L12120:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'061(fieldnr%)
                         if enabled% = 1% then L12090
                         if fieldnr% = 1% then L12070
                         goto L12120
L12170:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% then gosub gen_dtc
                      if keyhit% <> 0% then       L12090
L12200:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12090
            next fieldnr%

        REM *************************************************************~
            *   E D I T   M O D E   D T C   C O N F I G U R A T I O N   *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then gosub delete_rec_2
                  if keyhit%  = 16% then gosub dataput_2
                  if keyhit% <>  0% then       editpg2
L13110:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  3% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'061(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13160:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13160
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13160
                  lastfieldnr% = fieldnr%
            goto L13110

        REM *************************************************************~
            *             P r o c e s s   D a t a                       *~
            *************************************************************

        gen_dtc
            gosub select_dtc
            gosub generate_dtc
            close printer
        return clear all
        goto inputmode

        select_dtc
            title$ = "***** APC DTC Configuration Report *****"
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            call "DATEFMT" (date$)
            call "TIME" (xtime$)
            call "SETPRNT" (" ","DTCR",0%,0%)
            select printer(134)
        return

        gen_eq
            gosub select_eq
            gosub generate_eq
            close printer
        return clear all
        goto inputmode

        select_eq
            title$ = "***** APC Computer Equipment Report ****"
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            call "DATEFMT" (date$)
            call "TIME" (xtime$)
            call "SETPRNT" (" ","EQPR",0%,0%)
            select printer(134)
        return

        select_user
            title$ = "********** APC User Id Report **********"
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            call "DATEFMT" (date$)
            call "TIME" (xtime$)
            call "SETPRNT" (" ","USER",0%,0%)
            select printer(134)
        return

        gen_user
            call "SHOSTAT" ("Creating User Id Report")
            gosub select_user
            gosub generate_user
            close printer
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

        deffn'061(fieldnr%)
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
         "Enter a Valid Control Number or <Return> to Assign One?      ",~
         "Enter a Valid Name? Either an Individual or Department?<Req.>",~
         "Enter a Valid Office Location Code?                          ",~
         "Enter a Valid Office Data Jack Code?                         ",~
         "Enter a Valid Patch Location Code?                           ",~
         "Enter a Valid Patch Data Jack Code?                          ",~
         "Enter a Valid DTC Location Code?                             ",~
         "Enter a Valid DTC Data Jack Code?                            ",~
         "Enter a Valid DTC Baud Rate?                                 ",~
         "Enter a Valid Computer Serial Number?                        ",~
         "Enter a Valid Monitor Serial Number?                         ",~
         "Enter a Valid Printer Serial Number?                         ",~
         "Enter Applicable Processor Info. Processor, Memory, Etc.?    ",~
         "Enter Applicable Hard Disk Info. Size, Type, No., Etc.?      ",~
         "Enter Applicable External Disk Info. Type, Size, Etc.?       ",~
         "Enter Applicable Monitor Info. Mon or Color, CGA,VGA,EGA,Etc.",~
         "Enter Applicable Network Info. (Primary)?                    ",~
         "Enter Applicable Software Info. Type, ID, Network Node, Etc. "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28410
                inpmessage$ = edtmessage$
                return

L28410
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid DTC Number? ( 00 - XX )                        ",~
         "Enter a Valid DTC Port Number? ( 00 thru 15 )                ",~
         "Enter a Applicable Information?                              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, eq_ctr$, eq_name$,         ~
                      eq_off_loc$, eq_off_desc$, eq_off_jack$,           ~
                      eq_pat_loc$, eq_pat_desc$, eq_pat_jack$,           ~
                      eq_dtc_loc$, eq_dtc_desc$, eq_dtc_jack$,           ~
                      eq_dtc_baud$, eq_com_ser$, eq_mon_ser$,            ~
                      eq_prt_ser$, eq_proc$, eq_disk$, eq_ext_disk$,     ~
                      eq_monitor$, eq_network$, eq_software$, eq_date$,  ~
                      eq_dte$, eq_fil$, dtc_no$, dtc_desc$, dtc_port$,   ~
                      dtc_baud$(), dtc_type$(), dtc_conn$(), dtc_comm$(),~
                      dtc_fil$

        dtc_rec%, lookup%, rec% = 0%
        dtc% = 1%
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            rec% = 0%
            eq_key$ = " "
            if lookup% <> 0% then goto L30130
L30100:        str(eq_key$,1%,4%) = eq_ctr$
               read #1,key = eq_key$,eod goto L30460
               goto L30170
L30130:     if lookup% <> 1% then goto L30100
               str(eq_key$,1%,20%) = eq_name$
               read #1,key 1% = eq_key$,eod goto L30460

        get_eq_data
L30170:        get #1, using L35040, eq_ctr$,       /*APC CONTROL NUMBER*/~
                                    eq_name$,      /* SYSTEM NAME      */~
                                    eq_off_loc$,   /* OFFICE LOC CODE  */~
                                    eq_off_jack$,  /* OFFICE JACK      */~
                                    eq_pat_loc$,   /* PATCH LOC CODE   */~
                                    eq_pat_jack$,  /* PATCH JACK       */~
                                    eq_dtc_loc$,   /* DTC LOC CODE     */~
                                    eq_dtc_jack$,  /* DTC JACK         */~
                                    eq_dtc_baud$,  /* DTC BAUD RATE    */~
                                    eq_com_ser$,   /* COMPUTER SER. NO.*/~
                                    eq_mon_ser$,   /* MONITOR SER. NO. */~
                                    eq_prt_ser$,   /* PRINTER SER. NO. */~
                                    eq_proc$,      /* PROCESSOR INFO   */~
                                    eq_disk$,      /* HARD DISK INFO   */~
                                    eq_ext_disk$,  /* EXT. DISK INFO   */~
                                    eq_monitor$,   /* MONITOR INFO     */~
                                    eq_network$,   /* NETWORK INFO     */~
                                    eq_software$,  /* SOFTWARE INFO    */~
                                    eq_dte$,       /* LAST MOD DATE    */~
                                    eq_usr$,       /* LAST USER MOD    */~
                                    eq_fil$        /* FILLER AREA      */

               gosub lookup_office
               gosub lookup_patch
               gosub lookup_dtc
               eq_date$ = eq_dte$
               call "DATEFMT" (eq_date$)

        rec% = 1%
L30460: return

        dataload_2
            dtc_rec% = 0%
            init(" ") dtc_baud$(), dtc_type$(), dtc_conn$(),dtc_comm$(), ~
                      dtc_fil$

            read #3,key = dtc_no$, using L35260, dtc_no$, dtc_baud$(),    ~
                                                dtc_type$(), dtc_conn$(),~
                                                dtc_comm$(), dtc_fil$,   ~
                                                eod goto L30590

            dtc_rec% = 1%
L30590: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_rec
        dataput
            call "SHOSTAT" ( "Updating Data" )
            if assign% = 1% then goto L31150
               gosub assign_ctr
               if assign% = 0% then return

            eq_usr$ = userid$
            eq_dte$ = date
L31150:     eq_key$ = " "
            str(eq_key$,1%,4%) = eq_ctr$
            read #1,hold,key = eq_key$,eod goto L31200
                delete #1
                if keyhit% = 12% then goto L31420
L31200:     put #1, using L35040,    eq_ctr$,       /*APC CONTROL NUMBER*/~
                                    eq_name$,      /* SYSTEM NAME      */~
                                    eq_off_loc$,   /* OFFICE LOC CODE  */~
                                    eq_off_jack$,  /* OFFICE JACK      */~
                                    eq_pat_loc$,   /* PATCH LOC CODE   */~
                                    eq_pat_jack$,  /* PATCH JACK       */~
                                    eq_dtc_loc$,   /* DTC LOC CODE     */~
                                    eq_dtc_jack$,  /* DTC JACK         */~
                                    eq_dtc_baud$,  /* DTC BAUD RATE    */~
                                    eq_com_ser$,   /* COMPUTER SER. NO.*/~
                                    eq_mon_ser$,   /* MONITOR SER. NO. */~
                                    eq_prt_ser$,   /* PRINTER SER. NO. */~
                                    eq_proc$,      /* PROCESSOR INFO   */~
                                    eq_disk$,      /* HARD DISK INFO   */~
                                    eq_ext_disk$,  /* EXT. DISK INFO   */~
                                    eq_monitor$,   /* MONITOR INFO     */~
                                    eq_network$,   /* NETWORK INFO     */~
                                    eq_software$,  /* SOFTWARE INFO    */~
                                    eq_dte$,       /* LAST MOD DATE    */~
                                    eq_usr$,       /* LAST USER MOD    */~
                                    eq_fil$        /* FILLER AREA      */
            write #1, eod goto L31440
L31420: return clear all
        goto inputmode
L31440:     call "SHOSTAT" ("ERROR - UNABLE TO UPDATE") : stop
        return

        assign_ctr
            readkey$ = " "
            str(readkey$,1%,9%) = "APC  EQ01"
            str(readkey$,10%,15%) = "00"
            read #2,hold,key = readkey$, using L31530, desc$,             ~
                                                           eod goto L31650
L31530:        FMT POS(25), CH(32)
            convert str(desc$,1%,4%) to eq_ctr%, data goto L31650

            eq_ctr% = eq_ctr% + 1%
            convert eq_ctr% to eq_ctr$,pic(0000)

            str(desc$,1%,4%) = eq_ctr$
            put #2, using L31530, desc$

            rewrite #2
            assign% = 1%
        return
L31650:     call "SHOSTAT" ("ERROR - ASSIGNING CONTROL NO") : stop
        return

        REM *************************************************************~
            *     S T U F F   D A T A   I N T O   D T C   F I L E       *~
            *************************************************************

        delete_rec_2
        dataput_2
            call "SHOSTAT" ( "Updating DTC Data" )

            read #3,hold,key = dtc_no$,eod goto L32200
                delete #3
                if keyhit% = 12% then goto L32420
L32200:     put #3, using L35260,    dtc_no$,       /* DTC Number       */~
                                    dtc_baud$(),   /* DTC Baud Rate    */~
                                    dtc_type$(),   /* DTC Type Code T/P*/~
                                    dtc_conn$(),   /* DTC Connextion   */~
                                    dtc_comm$(),   /* DTC Comment      */~
                                    dtc_fil$       /* Filler Area      */

            write #3, eod goto L32440
L32420: return clear all
        goto inputmode
L32440:     call "SHOSTAT" ("Error - Unable to Update DTC Data") : stop
        return

        REM *************************************************************~
            *            D a t a   F o r m a t                          *~
            *************************************************************

L35040:     FMT CH(04),                        /* APC Control Number*/   ~
                CH(20),                        /* System Name       */   ~
                CH(02),                        /* Office Location   */   ~
                CH(05),                        /* Office Data Jack  */   ~
                CH(02),                        /* Patch Location    */   ~
                CH(05),                        /* Patch Data Jack   */   ~
                CH(02),                        /* DTC Location      */   ~
                CH(02),                        /* DTC Port Number   */   ~
                CH(05),                        /* DTC Port Baud Rate*/   ~
                CH(15),                        /* Computer Serial No*/   ~
                CH(15),                        /* Monitor Serial No.*/   ~
                CH(15),                        /* Printer Serial No.*/   ~
                CH(40),                        /* Processor Info.   */   ~
                CH(40),                        /* Hard Disk Info.   */   ~
                CH(40),                        /* Removable Disk Inf*/   ~
                CH(40),                        /* Monitor Info      */   ~
                CH(40),                        /* Network Info      */   ~
                CH(40),                        /* Software Info     */   ~
                CH(06),                        /* Last Mode Date    */   ~
                CH(03),                        /* Last Mode User    */   ~
                CH(59)                         /* Filler Area (400) */

L35260: FMT     CH(02),                        /* DTC Number        */   ~
                16*CH(05),                     /* DTC Baud Rates    */   ~
                16*CH(01),                     /* DTC Types         */   ~
                16*CH(05),                     /* DTC Connections   */   ~
                16*CH(20),                     /* DTC Comments      */   ~
                CH(14)                         /* Filler Area (512) */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40340,         /* APC Control Number*/   ~
                                L40320,         /* System Name       */   ~
                                L40340,         /* Office Location   */   ~
                                L40330,         /* Office Data Jack  */   ~
                                L40340,         /* Patch Location    */   ~
                                L40330,         /* Patch Data Jack   */   ~
                                L40340,         /* DTC Location      */   ~
                                L40340,         /* DTC Port Number   */   ~
                                L40340,         /* DTC Port Baud Rate*/   ~
                                L40330,         /* Computer Serial No*/   ~
                                L40330,         /* Monitor Serial No.*/   ~
                                L40330,         /* Printer Serial No.*/   ~
                                L40320,         /* Processor Info.   */   ~
                                L40320,         /* Hard Disk Info.   */   ~
                                L40320,         /* Removable Disk Inf*/   ~
                                L40320,         /* Monitor Info      */   ~
                                L40320,         /* Network Info      */   ~
                                L40320          /* Software Info     */

              goto L40360

L40320:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40330:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40340:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40360:     accept                                                       ~
               at (01,02),                                               ~
                  "(APC) Computer Equiptment Master Data Information",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Control Number  :",                          ~
               at (05,20), fac(lfac$(1%)), eq_ctr$              , ch(04),~
                                                                         ~
               at (05,40), "System Name  :",                             ~
               at (05,55), fac(lfac$(2%)), eq_name$             , ch(20),~
                                                                         ~
               at (06,02), "Office Loc Code :",                          ~
               at (06,20), fac(lfac$(3%)), eq_off_loc$          , ch(02),~
               at (06,40), fac(hex(84)),   eq_off_desc$         , ch(25),~
                                                                         ~
               at (07,02), "Office Data Jack:",                          ~
               at (07,20), fac(lfac$(4%)), eq_off_jack$         , ch(05),~
                                                                         ~
               at (08,02), "Patch Loc Code  :",                          ~
               at (08,20), fac(lfac$(5%)), eq_pat_loc$          , ch(02),~
               at (08,40), fac(hex(84)),   eq_pat_desc$         , ch(25),~
                                                                         ~
               at (09,02), "Patch Data Jack :",                          ~
               at (09,20), fac(lfac$(6%)), eq_pat_jack$         , ch(05),~
                                                                         ~
               at (10,02), "DTC Loc Code    :",                          ~
               at (10,20), fac(lfac$(7%)), eq_dtc_loc$          , ch(02),~
               at (10,40), fac(hex(84)),   eq_dtc_desc$         , ch(25),~
                                                                         ~
               at (11,02), "DTC Port Number :",                          ~
               at (11,20), fac(lfac$(8%)), eq_dtc_jack$         , ch(02),~
                                                                         ~
               at (11,40), "DTC Baud Rate:",                             ~
               at (11,55), fac(lfac$(9%)), eq_dtc_baud$         , ch(05),~
                                                                         ~
               at (12,02), "Computer Ser. No:",                          ~
               at (12,20), fac(lfac$(10%)), eq_com_ser$         , ch(15),~
                                                                         ~
               at (13,02), "Monitor Ser. No.:",                          ~
               at (13,20), fac(lfac$(11%)), eq_mon_ser$         , ch(15),~
                                                                         ~
               at (14,02), "Printer Ser. No.:",                          ~
               at (14,20), fac(lfac$(12%)), eq_prt_ser$         , ch(15),~
                                                                         ~
               at (15,02), "Processor Info. :",                          ~
               at (15,20), fac(lfac$(13%)), eq_proc$            , ch(40),~
                                                                         ~
               at (16,02), "Hard Disk Info. :",                          ~
               at (16,20), fac(lfac$(14%)), eq_disk$            , ch(40),~
                                                                         ~
               at (17,02), "Ext. Disk Info. :",                          ~
               at (17,20), fac(lfac$(15%)), eq_ext_disk$        , ch(40),~
                                                                         ~
               at (18,02), "Monitor Info.   :",                          ~
               at (18,20), fac(lfac$(16%)), eq_monitor$         , ch(40),~
                                                                         ~
               at (19,02), "Network Info.  .:",                          ~
               at (19,20), fac(lfac$(17%)), eq_network$         , ch(40),~
                                                                         ~
               at (20,02), "Software Info.  :",                          ~
               at (20,20), fac(lfac$(18%)), eq_software$        , ch(40),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 3% then L41090
                  gosub gen_user
                  goto L40070

L41090:        if keyhit% <> 7% then L41130
                  tab_1% = 1% : gosub display_codes
                  goto L40070

L41130:        if keyhit% <> 8% then L41170
                  tab_1% = 2% : gosub display_codes
                  goto L40070

L41170:        if keyhit% <> 9% then L41210
                  tab_1% = 3% : gosub display_codes
                  goto L40070

L41210:        if keyhit% <> 15% then L41240
                  call "PRNTSCRN" : goto L40360

L41240:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1
        if edit% = 2% then L41440     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (3)User Id Report      " &        ~
                     "(7)Dsp Office Codes    (14)Report      "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "(8)Dsp Patch Codes     (15)Print Screen"
            pf$(3) = "                 (6)Update DTC Data     " &        ~
                     "(9)Dsp DTC Codes       (16)Exit Program"
            pfkeys$ = hex(01ff0304ff06070809ffffffff0e0f1000)
            if fieldnr% = 1% then L41400
               str(pf$(1%),18%,20%) = " " : str(pfkeys$,3%,1%) = hex(ff)
               str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
               str(pf$(3%),18%,20%) = " " : str(pfkeys$,6%,1%) = hex(ff)
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41400:     if fieldnr% > 2% then L41420
               str(pf$(2%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41420:     return

L41440: if fieldnr% > 0% then L41550  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(7)Dsp Office Codes                    "
            pf$(2) = "                 (12)Delete Record      " &        ~
                     "(8)Dsp Patch Codes     (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "(9)Dsp DTC Codes       (16)Save Data   "
            pfkeys$ = hex(01ffffffffff070809ffff0cffff0f1000)
            if rec% <> 0% then goto L41540
               str(pf$(2%),18%,20%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41540:     return
L41550:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(7)Dsp Office Codes                    "
            pf$(2) = "                                        " &        ~
                     "(8)Dsp Patch Codes     (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "(9)Dsp DTC Codes                       "
            pfkeys$ = hex(01ffffffffff070809ffffff0dff0fff00)
            return

        REM *************************************************************~
            *               D T C   E N T R Y   S C R E E N             *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42180,         /* DTC Number        */   ~
                                L42180,         /* DTC Port No       */   ~
                                L42180,         /* DTC Baud          */   ~
                                L42180,         /* DTC Type T/P/O    */   ~
                                L42180,         /* DTC Connection    */   ~
                                L42170          /* DTC Comment       */
              goto L42210

L42170:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42210:     accept                                                       ~
               at (01,02),                                               ~
                  "(APC) DTC Port Configuration Screen",                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "DTC Assigned Number:",                       ~
               at (05,25), fac(lfac$(1%)), dtc_no$              , ch(02),~
               at (05,40), fac(hex(84)),   dtc_desc$            , ch(30),~
                                                                         ~
               at (06,02), "DTC Port Number    :",                       ~
               at (06,25), fac(lfac$(2%)), dtc_port$            , ch(02),~
                                                                         ~
               at (07,02), "DTC Port Baud Rate :",                       ~
               at (07,25), fac(lfac$(3%)), dtc_baud$(dtc%)      , ch(05),~
                                                                         ~
               at (08,02), "DTC Port Type T/P/O:",                       ~
               at (08,25), fac(lfac$(3%)), dtc_type$(dtc%)      , ch(01),~
                                                                         ~
               at (09,02), "DTC Connection     :",                       ~
               at (09,25), fac(lfac$(3%)), dtc_conn$(dtc%)      , ch(05),~
                                                                         ~
               at (10,02), "DTC Comments       :",                       ~
               at (10,25), fac(lfac$(3%)), dtc_comm$(dtc%)      , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then L42580
                  call "PRNTSCRN" : goto L42210

L42580:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf2
        if edit% = 2% then L42780     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Print Report"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L42740
               str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42740:     if fieldnr% > 2% then L42760
               str(pf$(2%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42760:     return

L42780: if fieldnr% > 0% then L42890  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (12)Delete Info "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            if dtc_rec% = 1% then L42880
               str(pf$(1%),64%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L42880:     return
L42890:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L50145,         /* APC Control Number*/   ~
                                L50230,         /* System Name       */   ~
                                L50295,         /* Office Location   */   ~
                                L50360,         /* Office Data Jack  */   ~
                                L50425,         /* Patch Location    */   ~
                                L50485,         /* Patch Data Jack   */   ~
                                L51060,         /* DTC Location      */   ~
                                L51180,         /* DTC Port Number   */   ~
                                L51290,         /* DTC Port Baud Rate*/   ~
                                L51320,         /* Computer Serial No*/   ~
                                L51420,         /* Monitor Serial No.*/   ~
                                L51520,         /* Printer Serial No.*/   ~
                                L51620,         /* Processor Info.   */   ~
                                L51650,         /* Hard Disk Info.   */   ~
                                L51680,         /* Removable Disk Inf*/   ~
                                L51710,         /* Monitor Info      */   ~
                                L51740,         /* Network Info      */   ~
                                L51770          /* Software Info     */

            return

L50145: REM APC Control Number                 EQ_CTR$
          assign% = 0%
          if eq_ctr$ <> " " then goto L50165
             return
L50165:   convert eq_ctr$ to eq_ctr%, data goto L50215

          convert eq_ctr% to eq_ctr$,pic(0000)

          lookup% = 0%
          gosub dataload
          if rec% = 0% then goto L50215
             assign%  = 1%                         /* DO NOT ASSIGN NO. */
             fieldnr% = 18%
        return
L50215:   errormsg$ = "(Error) - Invalid Control Number?"
        return

L50230: REM System Name                        EQ_NAME$
          if eq_name$ <> " " then goto L50245
             goto L50280
L50245:   lookup% = 1%
          gosub dataload
          lookup% = 0%
          if rec% = 0% then return
             assign%  = 1%
             fieldnr% = 18%
        return
L50280:   errormsg$ = "(Error) - A Unique System Name is Required?"
        return

L50295: REM Office Location                    EQ_OFF_LOC$,EQ_OFF_DESC$
           if eq_off_loc$ <> " " then goto L50310
              return
L50310:    convert eq_off_loc$ to x%, data goto L50345

           convert x% to eq_off_loc$,pic(00)

           if x% = 0% then goto L50345
           gosub lookup_office
        return
L50345:    errormsg$ = "(Error) - Invalid Office Location Code?"
        return

L50360: REM Office Data Jack                   EQ_OFF_JACK$
           if eq_off_jack$ <> " " then goto L50375
              return
L50375:    readkey$ = " "
           str(readkey$,1%,2%) = eq_off_loc$
           str(readkey$,3%,5%) = eq_off_jack$
           read #4,key 2% = readkey$,using L50395,eq_ctr_t$,eod goto L50405
L50395:       FMT CH(04)
           if eq_ctr$ <> eq_ctr_t$ then goto L50410
L50405: return
L50410:    errormsg$ = "(Error) - Invalid Office Data Jack Code?"
        return

L50425: REM Patch Location                     EQ_PAT_LOC$,EQ_PAT_DESC$
           if eq_pat_loc$ <> " " then goto L50440
              return
L50440:    convert eq_pat_loc$ to x%, data goto L50470

           convert x% to eq_pat_loc$,pic(00)

           gosub lookup_patch
        return
L50470:    errormsg$ = "(Error) - Invalid Patch Location Code?"
        return

L50485: REM Patch Data Jack                    EQ_PAT_JACK$
           if eq_pat_jack$ <> " " then goto L50500
              return
L50500:    readkey$ = " "
           str(readkey$,1%,2%) = eq_pat_loc$
           str(readkey$,3%,5%) = eq_pat_jack$
           read #4,key 3% = readkey$,using L51000,eq_ctr_t$,eod goto L51020
L51000:       FMT CH(04)
           if eq_ctr$ <> eq_ctr_t$ then goto L51030
L51020: return
L51030:    errormsg$ = "(Error) - Invalid Patch Jack Code?"
        return

L51060: REM DTC Location                       EQ_DTC_LOC$,EQ_DTC_DESC$
           if eq_dtc_loc$ <> " " then goto L51090
              return
L51090:    convert eq_dtc_loc$ to x%, data goto L51150

           convert x% to eq_dtc_loc$,pic(00)

           gosub lookup_dtc
        return
L51150:    errormsg$ = "(Error) - Invalid DTC Location Code?"
        return

L51180: REM DTC Data Jack                      EQ_DTC_JACK$
           if eq_dtc_jack$ <> " " then goto L51190
              return
L51190:    readkey$ = " "
           str(readkey$,1%,2%) = eq_dtc_loc$
           str(readkey$,3%,2%) = eq_dtc_jack$
           read #4,key 4% = readkey$,using L51230,eq_ctr_t$,eod goto L51250
L51230:       FMT CH(04)
           if eq_ctr$ <> eq_ctr_t$ then goto L51260
L51250: return
L51260:    errormsg$ = "(Error) - Invalid DTC Port Code?"
        return

L51290: REM DTC Baud Rate                      EQ_DTC_BAUD$
        return

L51320: REM Computer Serial Number             EQ_COM_SER$
           if eq_com_ser$ <> " " then goto L51330
              return
L51330:    readkey$ = " "
           str(readkey$,1%,15%) = eq_com_ser$
           read #4,key 5% = readkey$,using L51360,eq_ctr_t$,eod goto L51380
L51360:       FMT CH(04)
           if eq_ctr$ <> eq_ctr_t$ then goto L51390
L51380: return
L51390:    errormsg$ = "(Error) - Invalid Computer Serial Number?"
        return

L51420: REM Monitor  Serial Number             EQ_MON_SER$
           if eq_mon_ser$ <> " " then goto L51430
              return
L51430:    readkey$ = " "
           str(readkey$,1%,15%) = eq_mon_ser$
           read #4,key 6% = readkey$,using L51460,eq_ctr_t$,eod goto L51480
L51460:       FMT CH(04)
           if eq_ctr$ <> eq_ctr_t$ then goto L51490
L51480: return
L51490:    errormsg$ = "(Error) - Invalid Monitor Serial Number?"
        return

L51520: REM Printer Serial Number              EQ_PRT_SER$
           if eq_prt_ser$ <> " " then goto L51530
              return
L51530:    readkey$ = " "
           str(readkey$,1%,15%) = eq_prt_ser$
           read #4,key 7% = readkey$,using L51560,eq_ctr_t$,eod goto L51580
L51560:       FMT CH(04)
           if eq_ctr$ <> eq_ctr_t$ then goto L51590
L51580: return
L51590:    errormsg$ = "(Error) - Invalid Printer Serial Number?"
        return

L51620: REM Processor Information              EQ_PROC$
        return

L51650: REM Hard Disk Information              EQ_DISK$
        return

L51680: REM External Disk Information          EQ_EXT_DISK$
        return

L51710: REM Monitor Information                EQ_MONITOR$
        return

L51740: REM Network Information                EQ_NETWORK$
        return

L51770: REM Software Information               EQ_SOFTWARE$
        return

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52070,         /* DTC Number             */~
                              L52250,         /* DTC Port Number        */~
                              L52370          /* DTC Field Info.        */
            return

L52070: REM DTC Number                            DTC_NO$
           init(" ") eq_dtc_loc$, dtc_desc$
           convert dtc_no$ to dtc_no%, data goto L52340

           convert dtc_no% to dtc_no$, pic(00)

           eq_dtc_loc$ = dtc_no$
           gosub lookup_dtc
           if errormsg$ <> " " then return
              dtc_desc$ = eq_dtc_desc$
              init(" ") eq_dtc_loc$, eq_dtc_desc$
              gosub dataload_2
              if dtc_rec% = 0% then return
                 dtc% = 1%
                 dtc_port$ = "00"
                 fieldnr% = 3%
        return

L52250: REM DTC Port Number                       DTC_PORT$
           dtc% = 0%
           convert dtc_port$ to dtc%, data goto L52340

           convert dtc% to dtc_port$, pic(00)

           dtc% = dtc% + 1%
           if dtc% < 1% or dtc% > 16% then goto L52340
        return
L52340:    errormsg$ = "(Error) - Invalid DTC Port Number?"
           dtc% = 1%
        return

L52370: REM DTC Information                       DTC_(Fields)
        return

        REM *************************************************************~
            *      R e p o r t   F o r m a t   S t a t e m e n t s      *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~-------------+
L55060: %!---------------------------------------------------------------~
        ~-------------!
L55080: %!                                                               ~
        ~             !
                                                      /* Header Format */
L55110: %! ######## @ ########   ########################################~
        ~   Page: ### !
                                                      /* Detail Header */
L55140: %!DTC!Port!Baud Rate!Type!Connection!<----- Comment ------>!<----~
        ~ Notes ----->!
L55160: %!## ! ## !  #####  !  # !   #####  ! #################### !     ~
        ~             !
L55180: %!---!----!---------!----!----------!----------------------!-----~
        ~-------------!

L55210: %!           Control No.: ####    System Name: ##################~
        ~##           !
L55230: %!Office Loc Code: ## #########################  Office Jack: ###~
        ~##           !
L55250: %!Patch  Loc Code: ## #########################  Patch  Jack: ###~
        ~##           !
L55270: %!DTC    Loc Code: ## #########################  DTC    Jack:    ~
        ~## Baud:#####!
L55290: %!Comp Ser: ############### Mon Ser: ############### Prt Ser: ###~
        ~############ !
L55310: %!Computer Processor Info. : ####################################~
        ~####         !
L55330: %!Computer Hard Disk Info. : ####################################~
        ~####         !
L55350: %!Computer External Disk   : ####################################~
        ~####         !
L55370: %!Computer Monitor Info.   : ####################################~
        ~####         !
L55390: %!Computer Network Info.   : ####################################~
        ~####         !
L55410: %!Computer Software Info.  : ####################################~
        ~####         !
L55430: %!---------------------------------------------------------------~
        ~-------------!
                                                /* User Report Columns */
L55460: %! User Id !<---- Employee Name ----------->!<-Assigned Menu ->! ~
        ~Remarks ! NO.!

L55490: %!  ###    ! ############################## ! ################ ! ~
        ~        !####!

L55520: %!---------!--------------------------------!------------------!-~
        ~--------!----!

        REM *************************************************************~
            *            S p e c i a l   S u b r o u t i n e s          *~
            *************************************************************

        header_user
            if lcnt% <> 99% then print using L55040
            pageno% = pageno% + 1%
            print page
            print using L55040
            print using L55110, date$, xtime$, title$, pageno%
            print using L55060
            print using L55460
            lcnt% = 5%
        return

        header_dtc
            if lcnt% <> 99% then print using L55040
            pageno% = pageno% + 1%
            print page
            print using L55040
            print using L55110, date$, xtime$, title$, pageno%
            print using L55060
            print using L55140
            lcnt% = 5%
        return

        header_eq
            if lcnt% <> 99% then print using L55040
            pageno% = pageno% + 1%
            print page
            print using L55040
            print using L55110, date$, xtime$, title$, pageno%
            lcnt% = 3%
        return

        detail_dtc
            gosub header_dtc
            for i% = 1% to 16%
              print using L55180
              print using L55160, dtc_no$, i%-1% ,dtc_baud$(i%),          ~
                              dtc_type$(i%), dtc_conn$(i%), dtc_comm$(i%)
              dtc_no$ = "  "
            next i%
        return

        detail_eq
               gosub header_eq
               print using L55060
               print using L55210,eq_ctr$, eq_name$
               print using L55060
               print using L55230,eq_off_loc$, eq_off_desc$, eq_off_jack$
               print using L55080
               print using L55250,eq_pat_loc$, eq_pat_desc$, eq_pat_jack$
               print using L55080
               print using L55270,eq_dtc_loc$, eq_dtc_desc$, eq_dtc_jack$
               print using L55080
               print using L55290,eq_com_ser$, eq_mon_ser$, eq_prt_ser$
               print using L55080
               print using L55310,eq_proc$
               print using L55080
               print using L55330,eq_disk$
               print using L55080
               print using L55350,eq_ext_disk$
               print using L55080
               print using L55370,eq_monitor$
               print using L55080
               print using L55390,eq_network$
               print using L55080
               print using L55410,eq_software$
               for i% = 1% to 16%
                   print using L55080
                   print using L55430
               next i%
        return

        detail_user
            if lcnt% > 57% then gosub header_user
               print using L55520
               print using L55490, user_id$, user_name$, user_menu$, cnt%
               lcnt% = lcnt% + 2%
        return

        generate_dtc
            call "SHOSTAT" ("Creating (DTC) Report")
            init(" ") readkey$
        generate_next_dtc
            read #3,key > readkey$,using L35260, dtc_no$, dtc_baud$(),    ~
                                                dtc_type$(), dtc_conn$(),~
                                                dtc_comm$(), dtc_fil$,   ~
                                               eod goto generate_done_dtc
            readkey$ = dtc_no$
            gosub detail_dtc
            goto generate_next_dtc
        generate_done_dtc
            print using L55040
        return

        generate_eq
            call "SHOSTAT" ("Creating (Equipment) Report")
            init(" ") readkey$
        generate_next_eq
            read #1,key > readkey$, eod goto generate_done_eq
               gosub get_eq_data
               readkey$ = eq_ctr$
               gosub detail_eq
               goto generate_next_eq
        generate_done_eq
            print using L55080
            print using L55040
        return

        lookup_codes                   /* Load Data for Display Screen */
            on tab_1% gosub p_1, p_2, p_3
               return
        p_1                                /* Lookup Office Loc Codes  */
            tab_hdr$ = " Master Office Location Codes "
            sav_key$ = "APC  EQ01" : t_len% = 2%
            goto L61260
        p_2                                /* Lookup Patch Loc Codes   */
            tab_hdr$ = " Master Patch Location Codes  "
            sav_key$ = "APC  EQ02" : t_len% = 2%
            goto L61260
        p_3                                /* Lookup DTC Loc Codes     */
            tab_hdr$ = "  Master DTC Location Codes   "
            sav_key$ = "APC  EQ03" : t_len% = 2%

L61260:     i% = 0% : max_code% = 0%
            init(" ") cc$(), dd$(), readkey$
            str(readkey$,1%,9%) = sav_key$
L61290:     read #2,key > readkey$, using L61310, readkey$, desc$,        ~
                                                         eod goto L61390
L61310:       FMT CH(24), CH(32)
            if sav_key$ <> str(readkey$,1%,9%) then goto L61390

               i% = i% + 1%
               cc$(i%) = str(readkey$,10%,t_len%)
               dd$(i%) = desc$
               if i% = 34 then goto L61390
               goto L61290
L61390:     max_code% = i%
        return

        display_codes
            gosub lookup_codes
            gosub set_keys
L61450:     accept                                                       ~
               at (02,26), fac(hex(84)), tab_hdr$               , ch(30),~
                                                                         ~
               at (04,02), "Code Description                     ",      ~
               at (05,02), "---- --------------------------------",      ~
                                                                         ~
               at (04,40), "Code Description                     ",      ~
               at (05,40), "---- --------------------------------",      ~
                                                                         ~
               at (06,02), fac(hex(84))  , cc$( 1%)             , ch(04),~
               at (06,07), fac(hex(84))  , dd$( 1%)             , ch(32),~
                                                                         ~
               at (06,40), fac(hex(84))  , cc$(18%)             , ch(04),~
               at (06,45), fac(hex(84))  , dd$(18%)             , ch(32),~
                                                                         ~
               at (07,02), fac(hex(84))  , cc$( 2%)             , ch(04),~
               at (07,07), fac(hex(84))  , dd$( 2%)             , ch(32),~
                                                                         ~
               at (07,40), fac(hex(84))  , cc$(19%)             , ch(04),~
               at (07,45), fac(hex(84))  , dd$(19%)             , ch(32),~
                                                                         ~
               at (08,02), fac(hex(84))  , cc$( 3%)             , ch(04),~
               at (08,07), fac(hex(84))  , dd$( 3%)             , ch(32),~
                                                                         ~
               at (08,40), fac(hex(84))  , cc$(20%)             , ch(04),~
               at (08,45), fac(hex(84))  , dd$(20%)             , ch(32),~
                                                                         ~
               at (09,02), fac(hex(84))  , cc$( 4%)             , ch(04),~
               at (09,07), fac(hex(84))  , dd$( 4%)             , ch(32),~
                                                                         ~
               at (09,40), fac(hex(84))  , cc$(21%)             , ch(04),~
               at (09,45), fac(hex(84))  , dd$(21%)             , ch(32),~
                                                                         ~
               at (10,02), fac(hex(84))  , cc$( 5%)             , ch(04),~
               at (10,07), fac(hex(84))  , dd$( 5%)             , ch(32),~
                                                                         ~
               at (10,40), fac(hex(84))  , cc$(22%)             , ch(04),~
               at (10,45), fac(hex(84))  , dd$(22%)             , ch(32),~
                                                                         ~
               at (11,02), fac(hex(84))  , cc$( 6%)             , ch(04),~
               at (11,07), fac(hex(84))  , dd$( 6%)             , ch(32),~
                                                                         ~
               at (11,40), fac(hex(84))  , cc$(23%)             , ch(04),~
               at (11,45), fac(hex(84))  , dd$(23%)             , ch(32),~
                                                                         ~
               at (12,02), fac(hex(84))  , cc$( 7%)             , ch(04),~
               at (12,07), fac(hex(84))  , dd$( 7%)             , ch(32),~
                                                                         ~
               at (12,40), fac(hex(84))  , cc$(24%)             , ch(04),~
               at (12,45), fac(hex(84))  , dd$(24%)             , ch(32),~
                                                                         ~
               at (13,02), fac(hex(84))  , cc$( 8%)             , ch(04),~
               at (13,07), fac(hex(84))  , dd$( 8%)             , ch(32),~
                                                                         ~
               at (13,40), fac(hex(84))  , cc$(25%)             , ch(04),~
               at (13,45), fac(hex(84))  , dd$(25%)             , ch(32),~
                                                                         ~
               at (14,02), fac(hex(84))  , cc$( 9%)             , ch(04),~
               at (14,07), fac(hex(84))  , dd$( 9%)             , ch(32),~
                                                                         ~
               at (14,40), fac(hex(84))  , cc$(26%)             , ch(04),~
               at (14,45), fac(hex(84))  , dd$(26%)             , ch(32),~
                                                                         ~
               at (15,02), fac(hex(84))  , cc$(10%)             , ch(04),~
               at (15,07), fac(hex(84))  , dd$(10%)             , ch(32),~
                                                                         ~
               at (15,40), fac(hex(84))  , cc$(27%)             , ch(04),~
               at (15,45), fac(hex(84))  , dd$(27%)             , ch(32),~
                                                                         ~
               at (16,02), fac(hex(84))  , cc$(11%)             , ch(04),~
               at (16,07), fac(hex(84))  , dd$(11%)             , ch(32),~
                                                                         ~
               at (16,40), fac(hex(84))  , cc$(28%)             , ch(04),~
               at (16,45), fac(hex(84))  , dd$(28%)             , ch(32),~
                                                                         ~
               at (17,02), fac(hex(84))  , cc$(12%)             , ch(04),~
               at (17,07), fac(hex(84))  , dd$(12%)             , ch(32),~
                                                                         ~
               at (17,40), fac(hex(84))  , cc$(29%)             , ch(04),~
               at (17,45), fac(hex(84))  , dd$(29%)             , ch(32),~
                                                                         ~
               at (18,02), fac(hex(84))  , cc$(13%)             , ch(04),~
               at (18,07), fac(hex(84))  , dd$(13%)             , ch(32),~
                                                                         ~
               at (18,40), fac(hex(84))  , cc$(30%)             , ch(04),~
               at (18,45), fac(hex(84))  , dd$(30%)             , ch(32),~
                                                                         ~
               at (19,02), fac(hex(84))  , cc$(14%)             , ch(04),~
               at (19,07), fac(hex(84))  , dd$(14%)             , ch(32),~
                                                                         ~
               at (19,40), fac(hex(84))  , cc$(31%)             , ch(04),~
               at (19,45), fac(hex(84))  , dd$(31%)             , ch(32),~
                                                                         ~
               at (20,02), fac(hex(84))  , cc$(15%)             , ch(04),~
               at (20,07), fac(hex(84))  , dd$(15%)             , ch(32),~
                                                                         ~
               at (20,40), fac(hex(84))  , cc$(32%)             , ch(04),~
               at (20,45), fac(hex(84))  , dd$(32%)             , ch(32),~
                                                                         ~
               at (21,02), fac(hex(84))  , cc$(16%)             , ch(04),~
               at (21,07), fac(hex(84))  , dd$(16%)             , ch(32),~
                                                                         ~
               at (21,40), fac(hex(84))  , cc$(33%)             , ch(04),~
               at (21,45), fac(hex(84))  , dd$(33%)             , ch(32),~
                                                                         ~
               at (22,02), fac(hex(84))  , cc$(17%)             , ch(04),~
               at (22,07), fac(hex(84))  , dd$(17%)             , ch(32),~
                                                                         ~
               at (22,40), fac(hex(84))  , cc$(34%)             , ch(04),~
               at (22,45), fac(hex(84))  , dd$(34%)             , ch(32),~
                                                                         ~
               at (23,02), "Hit <Return> To Continue!",                  ~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L62640
                  call "PRNTSCRN"
                  goto L61450

L62640:        init(" ") cc$(), dd$()
               close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_keys
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return

        lookup_office
            readkey$ = " "
            str(readkey$,1%,9%)   = "APC  EQ01"
            str(readkey$,10%,15%) = eq_off_loc$
            read #2,key = readkey$, using L62780, desc$,eod goto L62810
L62780:         FMT POS(25), CH(25)
            eq_off_desc$ = str(desc$,1%,25%)
        return
L62810:     errormsg$ = "(Error) - Invalid Office Location Code?"
        return

        lookup_patch
            readkey$ = " "
            str(readkey$,1%,9%)   = "APC  EQ02"
            str(readkey$,10%,15%) = eq_pat_loc$
            read #2,key = readkey$, using L62890, desc$,eod goto L62920
L62890:         FMT POS(25), CH(25)
            eq_pat_desc$ = str(desc$,1%,25%)
        return
L62920:     errormsg$ = "(Error) - Invalid Patch Location Code?"
        return

        lookup_dtc
            readkey$ = " "
            str(readkey$,1%,9%)   = "APC  EQ03"
            str(readkey$,10%,15%) = eq_dtc_loc$
            read #2,key = readkey$, using L63000, desc$,eod goto L63030
L63000:         FMT POS(25), CH(25)
            eq_dtc_desc$ = str(desc$,1%,25%)
        return
L63030:     errormsg$ = "(Error) - Invalid DTC Location Code?"
        return

        generate_user
            user_id$ = " " : cnt% = 0%
        gen_user_next
            read #5,key > user_id$, using L63110, user_id$, user_name$,   ~
                                       user_menu$, eod goto gen_user_done
L63110:        FMT CH(3), CH(30), POS(74), CH(30)
            cnt% = cnt% + 1%
            gosub detail_user
            goto gen_user_next
        gen_user_done
            print using L55040
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

        end
