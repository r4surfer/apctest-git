        REM *************************************************************~
            *      ( Replaced - APCBOMSB ) - As of (02/28/2020)         *~
            *   AAA   PPPP    CCC   PPPP   RRRR    000    SSS   BBBB    *~
            *  A   A  P   P  C   C  P   P  R   R  0   0  S      B   B   *~
            *  AAAAA  PPPP   C      PPPP   RRRR   0   0   SSS   BBBB    *~
            *  A   A  P      C   C  P      R R    0   0      S  B   B   *~
            *  A   A  P       CCC   P      R  R    000    SSS   BBBB    *~
            *                                                           *~
            *       ( Programs -->  APCPRCQT and BCKFASTR )             *~
            *-----------------------------------------------------------*~
            *                                                           *~
            * APCPR0SB - This program builds the (Manufactured) Part    *~
            *            Number Based on the 'FIELDS' Valid Data        *~
            *            from the Validity File (AMTBOMIF).             *~
            *                                                           *~
            *            When Building the Part Number from 'Scratch',  *~
            *            the Program will Display the Acceptable Values *~
            *            for Each Field being entered along with the    *~
            *            applicable Description found in the associated *~
            *            'Table File' for Each Field.                   *~
            *                                                           *~
            * (APCPRSUB) - Used to Calculate the Following.             *~
            *              (1) Calc Standard Deduction                  *~
            *              (2) Calculate the APC Catalog Price          *~
            *              (3) When Applicable, Calculate a Customers   *~
            *                  Special Price.                           *~
            *                                                           *~
            * Notes (1) '!' As First Character of Part Number Sets      *~
            *               Component Part Flag and Exits Routine.      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/24/94 ! New Subroutine for Building Valid Parts  ! RHH *~
            *          ! Currently there are three (3) versions.  !     *~
            *          ! SIZE$ = "X" - (APCPRCQT) - Program% = 1% !     *~
            *          !       = "Y" - (BCKFASTR) - Program% = 2% !     *~
            *          !       = "Z" - (AMTBOMPB) - Program% = 0% !     *~
            *          !       = "O" - Calc Price using Opening   !     *~
            *          !       = "E" - Calc Price Using Exact Size!     *~
            *          !       = "F" - Calc Opening/No Std Deduct !     *~
            *          !       = "C" - No Calc, Component Part    !     *~
            *          ! PART  = (19) Characters Standard Parts   !     *~
            *          !         (22) Characters Center Line/Cott !     *~
            *          !         (25) Characters Bay/Bow Windows  !     *~
            *          ! Load Part Definitions  - (GATHER_FIELDS) !     *~
            *          !   Subroutine. Lines ( 9250 thru 9520 )   !     *~
            *          !                                          !     *~
            *          ! Control Screen Prompts - (CHOOSE_OPTION) !     *~
            *          !   Subroutine. Lines ( 51010 thru 51850 ) !     *~
            *          !                                          !     *~
            *          ! CONTROL PRICE BUILD    - (BUILD_PRICE)   !     *~
            *          !   Subroutine. Lines ( 31210 thru 31480 ) !     *~
            *          !   (BUILD_DESCRIPTION) (60160 thru 60310) !     *~
            * 04/20/95 ! New Mod to Support Wood Surround and     ! RHH *~
            *          !   Factory Mulled Products. Key Flag is   !     *~
            *          !   'WOOD_MULL%'                           !     *~
            * 03/24/97 ! Mods to Support New Pricing Modifications! RHH *~
            *          !   and condition for the HP Unix Box.     !     *~
            * 04/03/97 ! Mod for New Series Prompt to Connect with! RHH *~
            *          !   new Sub (APCPRXSB) - at Line (50220)   !     *~
            * 10/31/97 ! Check for Upgrade to R6.04.03            ! RHH *~
            * 11/06/97 ! Set Hinge Code Limit for Cottage/Oriel   ! RHH *~
            *          ! to between (70 to 95)                    !     *~
            * 01/20/98 ! Mod for New Wood Surround Codes, Range   ! RHH *~
            *          !   A00 thru Z00. 1st Character must be an !     *~
            *          !   Alpha Character 'A' thru 'Z' followed  !     *~
            *          !   by Two (2) numeric digits.             !     *~
            * 10/02/98 ! (EWD001) Mods to change FACs for options;! BWS *~
            *          !   add PFKey to view table APC WOOD; add  !     *~
            *          !   logic for pricing non-item parts.      !     *~
            *          !   Part No. Preceded with '!'&Part No.    ! RHH *~ 
            * 05/27/99 ! (EWD002) Review promotion's logic for New! RHH *~
            *          !   'Grid' promotion.                      !     *~
            * 01/20/00 ! (EWD003) Mods for Samp/Disp/Lit          ! RHH *~ 
            * 06/01/00 ! (EWD004) Mods to set limit for Cottage/  ! CMG *~ 
            *          !          Oriel                           !     *~ 
            * 09/06/02 ! (EWD005) Fix for Special Shapes Grid Code! CMG *~
            * 10/29/05 ! (AWD006) Mods for new part number CR347  ! CMG *~
            *04/22/2013! (AWD007) mod for 5/0 sash                ! CMG *~
            *12/22/2018! (SR67154) mods for NFRC 2016             ! CMG *~
            *12/22/2018! (SR71583) mods for TriplePlus            ! CMG *~
            *02/28/2020! (CR2444) initalization issue with model$ ! CMN *~
            *03/04/2020! (CR2451) 3900 PSE Price                  ! CMN *~            
            *************************************************************

            sub  "APCPR0SB" ( model$,     /* Virtual Model (SR67154) */  ~
                              partno$,    /* Part Number to Build    */  ~
                              subpart$,   /* New part number (AWD006)*/  ~
                              prt_desc$,  /* Part Description        */  ~
                              size$,      /* (O)pen,(E)xact,(F)No Ded*/  ~
                              upd$,       /* Update Prices (Y)es,(N)o*/  ~
                              pc(),       /* Calculated Prices       */  ~
                              p1,         /* Special Customer Price  */  ~
                              sp%,        /* Special Prc Cd 0,1,2    */  ~
                              cuscode$,   /* Customer Code           */  ~
                              ref$(),     /* Ref. Type Codes Catalog */  ~
                              ref1$(),    /* Ref. Type Codes Spec Cat*/  ~
                              ref_p(),    /* Ref Prices APC Catalog  */  ~
                              ref_p1(),   /* Ref Prices Special Cat. */  ~
                              #8,         /* CPRPRICE File           */  ~
                              #4,         /* GENCODES File           */  ~
                              #10,        /* CUSTOMER FILE           */  ~
                              #3,         /* HNYMASTR File           */  ~
                              #9,         /* APCPCMST File           */  ~
                              #14,        /* AWDPCMST File           */  ~
                              #11,        /* APCSKUNO FILE           */  ~
                              #12,        /* APCPCMSK File           */  ~
                              #13,        /* APCPCMSD File           */  ~
                              err% )      /* Error Code                */
                                          /* 0% = Part and Catalog Prc */
                                          /* 1% = Part Cat. an Spc Prc */
                                          /* 2% = Edi Price            */
                                          /* 3% = No Price Error Cat.  */
                                          /* 4% = Catalog Error Special*/
                                          /* 5% = Invalid Part Price   */
                                          /* 6% = Invalid Part Build   */
        dim                                                              ~
            apc_scr$120, wood_desc$6,    /* Screen Display Text        */~
            apc_prt$60, wood$4,          /* Print Display Text         */~
            sub_scr$120,                 /* Sub Part Screen            */~
            sub_prt$60,                  /* Sub Part Print             */~ 
            apc_sze$20,                  /* Long Form of Size          */~
            lit_flg$1,                   /* Literature Flag    (EWD003)*/~   
            pc(36%),                     /* APC (35) PRICE CODES       */~
            size$1,                      /* (O)PENING OR (E)XACT SIZE  */~
            upd$1,                       /* UPDATE PRICES (Y)ES, (N)O  */~
            size_msg$18,                 /* (O)pen,(E)xact,(F)No Deduct*/~
/*AWD006*/  ref$(35%)2,                  /* Ref Type Codes Catalog     */~
/*AWD006*/  ref1$(35%)2,                 /* Ref Type Codes Spec Cat.   */~
/*AWD006*/  ref_p(35%),                  /* Ref Type Price APC Catalog */~
/*AWD006*/  ref_p1(35%),                 /* Ref Type Price Special Cat.*/~
            bom_id$3,                    /* Bom ID                     */~
            bom_part$25,                 /* Bom Part Number            */~
            cuscode$9,                   /* CUSTOMER CODE              */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            desc$(22%)20,                /* Option Description         */~
            desc_1$20,                   /* Screen Text - Description  */~
            defkey$32,                   /* Field Definition Key       */~
            savkey$(20%)32,              /* Save End of Previous Page  */~
            edtmessage$79,               /* Edit screen message        */~
            exclude$(35%)15,             /* Excluded Values            */~
            errormsg$79,                 /* Error Message              */~
            field$(35%)2,                /* Field Number               */~
            fld_lgth$2,                  /* Field Length               */~
            fld_name$8,                  /* Field Name                 */~
            file_name$(12%)8,            /* File Name                  */~
            i$(24)80,                    /* Screen Image               */~
            gencode$24,                  /* Gencode File Key           */~
/*EWD001*/  gen_desc$30,                 /* Gencode Description        */~
            gen_table$9,                 /* Gencode Table              */~
            gen_value$15,                /* Gencode Table              */~
            inpmessage$79,               /* Informational Message      */~
            incl$1,                      /* Include Y/N                */~
/*EWD001*/  lfac$(32%)1,                 /* Field Attribute Characters */~
            name$(35%)8,                 /* Field Name                 */~
            opt$(22%)15,                 /* Options                    */~
            option$22,                   /* Chosen Option Indicatior   */~
            partno$25,                   /* Part Number                */~
            model$16,                    /* Model Number               */~
            subpart$20,                  /* Part Number 1      (AWD006)*/~
            mscdescr$64,                 /* LOOKUP DESCRIPTION         */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prt_desc$32,                 /* Printer Description        */~
            printer$(35%)30,             /* Printer Description Optns  */~
            printer_desc$30,             /* Printer Description        */~
            rte_id$3,                    /* Route ID                   */~
            screen_desc$20,              /* Screen Description         */~
            screen$(35%)20,              /* Screen Description Optns   */~
            scr_desc$120,                /* Screen Description         */~
            table_file$(35%)9,           /* Table File Name            */~
            temp_flg$1,                  /* TEMP OR PERMENANT (P/T)    */~
            type_file$(12%)1,            /* File Type                  */~
            userid$3,                    /* Current User Id            */~
            valid_yn$(12%)1,             /* Validate ?                 */~
            valid_table$(12%)9,          /* Validation Table Name      */~
            value$15                     /* Values                     */~

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20,                /* Text from file opening     */~
            pos%(12%),                   /* Starting Field Positions   */~
            length%(35%)                 /* Field Length               */
            
        dim sash_desc$(3%)18,            /* (AWD007) 5/0 sash desc     */~
            triple_desc$(3%)18,          /* (SR71583) Triple Plus      */~
            north_desc$(3%)18 ,          /* (SR67154) Northern NFRC    */~
            ffoam$(3%)18                 /* Forced Form                */

        dim logmsg$256
        
        dim sash5_0$2,                   /* 5/0 Sash  (AWD007)         */~
            triple$2,                    /* Triple Plus (SR71583)      */~
            northern$2,                  /* NorthernNFRC (SR67154)     */~
            forcedfoam$2                 /* Forced Foam                */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(Configurator for Manufactured Products)"
            pname$ = "APCPR0SB - Rev: R6.04"

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
            * #1  ! AMTBOMPM ! Bom Gen. Field Parameter Definitions     *~
            * #2  ! AMTBOMIF ! BOM Gen. Assoc. Valid Data Elements File *~
            * #3  ! HNYMASTR ! Parts Master File             (Passed In)*~
            * #4  ! GENCODES ! Gencodes                      (Passed In)*~
            * #8  ! CPRPRICE ! CUSTOMER PRICE CODES FILE     (Passed In)*~
            * #9  ! APCPCMST ! Pricing Master Definition File(Passed In)*~
            * #10 ! CUSTOMER ! Customer Master File          (Passed In)*~
            * #11 ! APCSKUNO ! APC SKU Nunber File           (Passed In)*~
            * #12 ! APCPCMSK ! Pricing (Key) and (Value) Def.(Passed In)*~
            * #13 ! APCPCMSD ! Pricing Calc Definition File  (Passed In)*~
            * #14 ! AWDPCMST ! Pricing Master Definition File(Passed In)*~            
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AMTBOMPM",                                      ~
                        varc,     indexed,  recsize =  250,              ~
                        keypos =    1, keylen =   2                      ~

            select #2,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            
                                                  /* (EWD003)          */
            lit_flg$ = "N"
            if str(partno$,1%,3%) = "003" then lit_flg$ = "Y"
                                                  /* (EWD003)          */

            desc_1$ = "Description         "
            program% = 0%                         /* (AMTBOMPB) - Call */
            if size$ = "X" then program% = 1%     /* (APCPRCQT) - Call */
            if size$ = "Y" then program% = 2%     /* (BCKFASTR) - Call */
                                                  /* Check Opening Size*/
            err% = 0%  : size$ = "O"              /* Default Opening   */
            if str(partno$,1%,1%) <> "!" then L09160
                                                  /* (EWD002) Precede  */
                                                  /* Part No. with '!' */
               str(partno$,1%,24%) = str(partno$,2%,24%)
/*EWD001*/     gosub build_price        /* See if it can be priced.... */
/* Begin*/     if err% = 0% then goto L09150
                 size$ = "C" : err% = 6%          /* Component Part    */
                 goto exit_program  
                                                  /* (EWD002) Check    */
                                                  /* Glass Warranty Info*/
L09150:        gencode$ = "PRICE 022" & str(partno$,5%,8%)
               read #4, key = gencode$, using L09155, gen_desc$,          ~
                    eod goto exit_program
L09155:          fmt pos(25), ch(20)
               x% = pos(gen_desc$ = "-")
/*EWD001*/     if x% <> 0% then str(prt_desc$,13%) = str(gen_desc$,x%+2%)
/* End  */     goto exit_program

L09160:     if program% = 0% then size$ = "E"
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            plowkey$ = hex(00) : x% = 0%
            database% = 0% : pos%(1%) = 1%
        gather_fields                      /* Obtain Field Definitions */
            read #1,key > plowkey$, using L35030, fld_num$, fld_name$,    ~
                    fld_lgth$, type_file$(), valid_yn$(), valid_table$(),~
                    file_name$(), eod goto gather_done
REM adjust when add to subpart.  field number + 1                    
              if fld_num$ > "30" then goto gather_done
              plowkey$ = fld_num$
              convert fld_lgth$ to fld_lgth%, data goto L09340
L09340:
              x% = x% + 1%
              field$(x%)  = fld_num$             /* Save Field Number  */
              name$(x%)   = fld_name$            /* Save Field Name    */
              length%(x%) = fld_lgth%            /* Save Field Length  */
              pos%(x%+1%) = pos%(x%) + fld_lgth% /* Save Start Position*/
              i% = 0%                            /* for Next Field     */
L09410:       i% = i% + 1%
              if i% > 12% then goto gather_fields
                 if type_file$(i%) <> "T" then goto L09410
                    if valid_yn$(i%) <> "Y" then goto L09470
                       table_file$(x%) = valid_table$(i%) /* TABLE NAME*/
                                                       /* FOR VALIDITY */
L09470:       goto gather_fields
        gather_done
        database% = x%                     /* X% > 0% Correct Database */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  1%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0% : table$ = "Option"
            desc_1$ = "Description         "
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 4%
                   goto inputmode

            fieldnr% = 1%
            if fieldnr% < 1% or fieldnr% >  1% then editpg1
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
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if fieldnr% <> 1% then goto L28160
            inpmessage$ = "Enter Part Number,<Return> for Opt's," &      ~
                     "'+' for <Series>,or '?' for Existing Parts"
            return
L28160:     if fieldnr% <> 2% then goto L28230
            if more_options% <> 1% then goto L28210
            inpmessage$ = "Place 'X' by appropriate selection or"        ~
                & " <PF5> for More Options."        /*EWD001*/
            return
L28210:     inpmessage$ = "Place 'X' by appropriate selection."
            return
L28230:     if fieldnr% <> 3% then goto L28270      /* NOT WIDTH */
               inpmessage$ = "Valid Width in Inches.(Leading Zero's, "   ~
                           & "the Last Digit is Eight's of an Inch.)"
               return
L28270:     if fieldnr% <> 4% then goto L28310      /* NOT HEIGHT */
               inpmessage$ = "Valid Height in Inches.(Leading Zero's, "  ~
                           & "the Last Digit is Eight's of an Inch.)"
               return
L28310:     if fieldnr% <> 5% then goto L28360
               inpmessage$ = "Valid Ctr Line Meeting Rail in Inches. "   ~
                           & "Leading Zero, Last Digit Eight's Inch."
               if wood_mull% = 1% then goto L28390
               return

L28360:     if fieldnr% <> 6% then goto L28400
            
            inpmessage$ = "Valid Wall Width in Inches. Leading Zero"     ~
                        & ", Last Digit is Eight's of an Inch."
            if wood_mull% = 1% then goto L28390
            return
L28390: REM - WOOD SURROUND/FACTORY MULL
               inpmessage$ = "Enter a Valid Wood/Factory Mull Code. 'A"  ~
                           & "00' thru 'Z99' and Blank are Valid."
        return

L28400:
           inpmessage$ = "Enter Part Number or <Return> for Opt's, " ~
                        & "place 'X' by appropriate selection."

        return


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, opt$(), desc$(),           ~
                    prt_desc$, scr_desc$, option$, printer$(), screen$(),~
                    screen_desc$, printer_desc$, bom_part$, bom_id$,     ~
                    rte_id$


            err_flag% = 0% : table$ = "Option" : temp_flg$ = "P"
            size_msg$ = "Exact             "
            if size$ = "O" then size_msg$ = "Opening           "
            if size$ = "F" then size_msg$ = "Exact (No Deduct) "
            x%, i% = 0%
            x1% = 0%
/* (AWD007) */            
            sash5_0% = 1%
            sash_desc$(1%) = "N/A"
            sash_desc$(2%) = "5/0 Sash"    
/* (\AWD007) */                        

/* (SR71583) */            
            tripleplus% = 1%
            triple_desc$(1%) = "N/A"
            triple_desc$(2%) = "TriplePlus"    
/* (\SR71583) */  

/* (SR67154) */            
            northern% = 1%
            north_desc$(1%) = "N/A"
            north_desc$(2%) = "NorthernNFRC"    
/* (\SR67154) */  
            ffoam% = 1%
            ffoam$(1%) = "N/A"
            ffoam$(2%) = "ForcedForam "    

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
            model$  = " "              
            partno$ = " "
            subpart$ = " "                 /* CR347 */
            x% = 0%
            return clear all
            goto inputmode

        REM *************************************************************~
            *                   S A V E   D A T A                       *~
            *-----------------------------------------------------------*~
            * Verify Template. For Valid Template Update 'HNYMASTR' and *~
            *                  'AMTBOMPM' Buffer File.                  *~
            *************************************************************

        datasave
            err_flag%, err% = 0%
            gosub build_price             /* ERR% - Values             */
                                          /* 0% = Part and Catalog Prc */
                                          /* 1% = Part Cat. an Spc Prc */
                                          /* 2% = Edi Price            */
                                          /* 3% = No Price Error Cat.  */
                                          /* 4% = Catalog Error Special*/
                                          /* 5% = Invalid Part Price   */
                                          /* 6% = Invalid Part Build   */

        gosub build_description
        goto exit_program

        build_price                 /* Get Correct Size and Price      */
                           /* (EWD002) No logic found for Price        */
                           /* Promotions.                              */ 
            p1    = 0.0    /* Spc Customer Price, Dealer Catalog Always*/
            sp%   = 0%     /* (0%) Dealer Price Catalog (Only)         */
                           /* (1%) Spc Customer Catalog Price in (P1)  */
                           /* (2%) Spc Customer EDI Price in     (P1)  */
                           /* Note - Information from 'CUS PRICE' Table*/
REM          partno$ = "C122WARRLAB"    
REM          subpart$ = "           "
/* (AWD007) */
             sash5_0% = sash5_0% - 1%        /* zero (0) means n/a (AWD007) */
             tripleplus% = tripleplus% - 1%  /* zero (0) means n/a (SR71583)*/
             northern% = northern% - 1%      /* zero (0) means n/a (SR67154)*/
             ffoam% = ffoam% - 1%            /* zero (0) means n/a */
             
             convert sash5_0% to sash5_0$, pic(00)       /* (AWD007)  */
             convert tripleplus% to triple$, pic(00)     /* (SR71583) */
             convert northern% to northern$, pic(00)   
             convert ffoam% to forcedfoam$, pic(00)   

REM             MODEL$ = "E211"
             
             call "APCPRSUB" ( partno$,     /* Part Number                */~
                            subpart$,    /* Sub part         (AWD006)  */~
                            size$,       /* (O)pen,(E)xact,(F)No Deduct*/~
                            pc(),        /* Calc Dealer Price Catalog  */~
                            p1,          /* Special Customer Price     */~
                            sp%,         /* Special Price Code 0,1,2   */~
                            upd$,        /* Update Price Sheet Y or N  */~
                            cuscode$,    /* Customer Code              */~
                            err%,        /* Error Return Codes         */~
                            ref$(),      /* Ref. Type Codes Catalog    */~
                            ref1$(),     /* Ref. Type Codes Spec Cat.  */~
                            ref_p(),     /* Ref Prices APC Catalog     */~
                            ref_p1(),    /* Ref Prices Special Cat.    */~
                            sash5_0$,    /* (AWD007) 5/0 Sash Option   */~
                            model$,      /* Model                      */~
                            triple$,     /* Triple Plus (SR71583)      */~
                            northern$,   /* Northern NFRC (SR67154)    */~
                            forcedfoam$, /* Forced Foam                */~
                            #9,          /* Channel of (APCPCMST) File */~
                            #14,         /* Channel of (AWDPCMST) File */~
                            #12,         /* Channel of (APCPCMSK) File */~
                            #13,         /* Channel of (APCPCMSD) File */~
                            #4,          /* Channel of (GENCODES) File */~
                            #8,          /* Channel of (CPRPRICE) File */~
                            #10,         /* Channel of (CUSTOMER) File */~
                            #11 )        /* Channel of (APCSKUNO) File */
                                         /* After Call PARTNO$ Always  */
                                         /* has Exact Size             */
        return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: AMTBOMPM                          */~
            CH(2),          /* Field Number                            */~
            CH(8),          /* Field Name                              */~
            CH(2),          /* Field Length                            */~
            12*CH(1),       /* Type of File                            */~
            12*CH(1),       /* Validated Entries                       */~
            12*CH(8),       /* Validation Table                        */~
            12*CH(8),       /* File Name for all Possible Choices      */~
            CH(22)          /* Unused Space                            */~

        FMT                 /* FILE: AMTBOMIF                          */~
            CH(15),         /* Model Number                            */~
            CH(2),          /* Definition for elements whose Type = FIE*/~
            CH(15),         /* Value of the field of the Model         */~
            CH(1),          /* Include / Exclude the Value             */~
            CH(20),         /* Verbage to appear on the Screen Displays*/~
            CH(30),         /* Verbage to print on the Hardcopy        */~
            PD(14,4),       /* Price of the Component                  */~
            CH(29)          /* Unused Space                            */~

        FMT                 /* FILE: AMTBOMPB     ( OLD BOM GENERATOR) */~
            CH(25),         /* Build Part Number                       */~
            CH(32),         /* Print Description                       */~
            CH(75),         /* Screen Description                      */~
            CH(25),         /* Template Part Number to Use             */~
            CH(3),          /* Template BOM Id to Use                  */~
            CH(3),          /* Template Route Id to Use                */~
            CH(1),          /* (T)emporary or (P)ermenant Flag         */~
            CH(11)          /* Filler Area                             */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              if fieldnr% = 0% then goto L40140
              if calc% <> 1% then goto L40140
              if x% > 11% then goto define_second_part        /* (AWD006) */
              fieldnr% = 1%
              if x% = 8% then fieldnr% = 3%        /* (SR67154) Width       */
              if x% = 9% then fieldnr% = 4%        /* (SR67154) Height      */
              if x% =10% then fieldnr% = 5%        /* (SR67154) Center Line */
              if x% =11% then fieldnr% = 6%        /* (SR67154) Wall Width  */
L40140:       if x% < 12% then goto L40160
                 init(" ") opt$(),desc$()
                 goto L40160
define_second_part:                             /* (AWD006) - BEG */
              if calc_part1% <> 1% then goto L40145
              fieldnr% = 7%


REM           Six Fields....
L40145:       if x1% < 6% then goto L40160
                 init(" ") opt$(),desc$()

                                               /* (AWD006) - END */
L40160:       gosub'050(fieldnr%)
              gosub set_pf1
              init(hex(8c)) lfac$()
              on fieldnr% gosub L40280,         /* Part Number       */   ~
/*EWD001*/                      L40295,         /* Option Chosen     */   ~
                                L40290,         /* Width             */   ~
                                L40290,         /* Height            */   ~
                                L40280,         /* Center Line Meetin*/   ~
                                L40280,         /* Wall Width        */   ~
                                L40280,         /* Set Up Part 1 (AWD006)*/   ~
                                L40280,         /* Set Up Part 1 (AWD006)*/   ~
                                L40295          /* Options           */
              goto L40310

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40280:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40290:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40295:     for lfac% = 1% to 22%  /* Set FACs for Options (EWD001-New)*/
                if opt$(lfac%) <> " " then lfac$(lfac% + 10%) = hex(81)
            next lfac%
            return



L40310:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,14), fac(hex(a4)), apc$                   , ch(40),~
/* (SR67154) (SR71583) */                                                ~               
               at (03,02), fac(hex(94)), errormsg$              , ch(57),~
                                                                         ~
               at (02,60), "FFM",                                        ~
               at (02,66), fac(hex(a4)), ffoam$(ffoam%)         ,ch(12) ,~
                                                                         ~               
/* (SR71583)*/ at (03,60), "Trpl",                                       ~
/* (SR71583)*/ at (03,66), fac(hex(a4)), triple_desc$(tripleplus%),ch(12),~
                                                                         ~
/*  (SR67154)    AT (04,02), "MDL ",                                        */ ~
/*  (SR67154)    AT (04,06), FAC(LFAC$(1%)), MODEL$               , CH(16), */ ~
                                                                         ~                
/*(AWD006)*/   at (04,02), "Mfg Part Num ",                              ~
/*(AWD006)*/   at (04,15), fac(lfac$(1%)), str(partno$,1%,12%)  , ch(12),~
/*(AWD006)*/   at (04,28), fac(lfac$(3%)), str(partno$,13%,4%)  , ch(04),~
/*(AWD006)*/   at (04,33), fac(lfac$(4%)), str(partno$,17%,3%)  , ch(03),~
/*(AWD006)*/   at (04,37), fac(lfac$(5%)), str(partno$,20%,3%)  , ch(03),~
/*(AWD006)*/   at (04,41), fac(lfac$(6%)), str(partno$,23%,3%)  , ch(03),~
/*(SR67154)*/  at (04,60), "NFRC",                                       ~
/*(SR67154)*/  at (04,66), fac(hex(a4)), north_desc$(northern%) ,ch(12) ,~
                                                                         ~
               at (05,02), "Subpart ",                                   ~
/*(AWD006)*/   at (05,15), fac(lfac$(7%)), subpart$             , ch(20),~
               at (05,60), "Sash",                                       ~
               at (05,66), fac(hex(a4)),   sash_desc$(sash5_0%) , ch(12),~
                                                                         ~
               at (06,02), "Part Description",                           ~
               at (06,20), fac(hex(a4)),   prt_desc$            , ch(32),~
                                                                         ~
               at (06,60), "Size ",                                      ~
               at (06,66), fac(hex(a4)),   size_msg$            , ch(12),~
/*(CR2451)*/   at (05,38), fac(hex(8c)),   model$               , ch(16),~               
                                                                         ~
               at (07,02), "Screen Desc. (1)",                           ~
               at (07,20), fac(hex(8c)),   str(scr_desc$,1%,60%), ch(60),~
               at (08,02), "Screen Desc. (2)",                           ~
               at (08,20), fac(hex(8c)),  str(scr_desc$,61%,60%), ch(60),~
                                                                         ~
               at (09,04), fac(hex(ac)),   table$               , ch(8) ,~
               at (09,20), fac(hex(ac)),   desc_1$              , ch(20),~
               at (09,43), fac(hex(ac)),   table$               , ch(8) ,~
               at (09,59), fac(hex(ac)),   desc_1$              , ch(20),~
/*EWD001*/     at (10,02), fac(lfac$(11)), str(option$,1%,1%)   , ch(01),~
               at (10,04), fac(hex(8c)),   opt$(1%)             , ch(15),~
               at (10,20), fac(hex(8c)),   desc$(1%)            , ch(20),~
/*EWD001*/     at (10,41), fac(lfac$(22)), str(option$,12%,1%)  , ch(01),~
               at (10,43), fac(hex(8c)),   opt$(12%)            , ch(15),~
               at (10,59), fac(hex(8c)),   desc$(12%)           , ch(20),~
/*EWD001*/     at (11,02), fac(lfac$(12)), str(option$,2%,1%)   , ch(01),~
               at (11,04), fac(hex(8c)),   opt$(2%)             , ch(15),~
               at (11,20), fac(hex(8c)),   desc$(2%)            , ch(20),~
/*EWD001*/     at (11,41), fac(lfac$(23)), str(option$,13%,1%)  , ch(01),~
               at (11,43), fac(hex(8c)),   opt$(13%)            , ch(15),~
               at (11,59), fac(hex(8c)),   desc$(13%)           , ch(20),~
/*EWD001*/     at (12,02), fac(lfac$(13)), str(option$,3%,1%)   , ch(01),~
               at (12,04), fac(hex(8c)),   opt$(3%)             , ch(15),~
               at (12,20), fac(hex(8c)),   desc$(3%)            , ch(20),~
/*EWD001*/     at (12,41), fac(lfac$(24)), str(option$,14%,1%)  , ch(01),~
               at (12,43), fac(hex(8c)),   opt$(14%)            , ch(15),~
               at (12,59), fac(hex(8c)),   desc$(14%)           , ch(20),~
/*EWD001*/     at (13,02), fac(lfac$(14)), str(option$,4%,1%)   , ch(01),~
               at (13,04), fac(hex(8c)),   opt$(4%)             , ch(15),~
               at (13,20), fac(hex(8c)),   desc$(4%)            , ch(20),~
/*EWD001*/     at (13,41), fac(lfac$(25)), str(option$,15%,1%)  , ch(01),~
               at (13,43), fac(hex(8c)),   opt$(15%)            , ch(15),~
               at (13,59), fac(hex(8c)),   desc$(15%)           , ch(20),~
/*EWD001*/     at (14,02), fac(lfac$(15)), str(option$,5%,1%)   , ch(01),~
               at (14,04), fac(hex(8c)),   opt$(5%)             , ch(15),~
               at (14,20), fac(hex(8c)),   desc$(5%)            , ch(20),~
/*EWD001*/     at (14,41), fac(lfac$(26)), str(option$,16%,1%)  , ch(01),~
               at (14,43), fac(hex(8c)),   opt$(16%)            , ch(15),~
               at (14,59), fac(hex(8c)),   desc$(16%)           , ch(20),~
/*EWD001*/     at (15,02), fac(lfac$(16)), str(option$,6%,1%)   , ch(01),~
               at (15,04), fac(hex(8c)),   opt$(6%)             , ch(15),~
               at (15,20), fac(hex(8c)),   desc$(6%)            , ch(20),~
/*EWD001*/     at (15,41), fac(lfac$(27)), str(option$,17%,1%)  , ch(01),~
               at (15,43), fac(hex(8c)),   opt$(17%)            , ch(15),~
               at (15,59), fac(hex(8c)),   desc$(17%)           , ch(20),~
/*EWD001*/     at (16,02), fac(lfac$(17)), str(option$,7%,1%)   , ch(01),~
               at (16,04), fac(hex(8c)),   opt$(7%)             , ch(15),~
               at (16,20), fac(hex(8c)),   desc$(7%)            , ch(20),~
/*EWD001*/     at (16,41), fac(lfac$(28)), str(option$,18%,1%)  , ch(01),~
               at (16,43), fac(hex(8c)),   opt$(18%)            , ch(15),~
               at (16,59), fac(hex(8c)),   desc$(18%)           , ch(20),~
/*EWD001*/     at (17,02), fac(lfac$(18)), str(option$,8%,1%)   , ch(01),~
               at (17,04), fac(hex(8c)),   opt$(8%)             , ch(15),~
               at (17,20), fac(hex(8c)),   desc$(8%)            , ch(20),~
/*EWD001*/     at (17,41), fac(lfac$(29)), str(option$,19%,1%)  , ch(01),~
               at (17,43), fac(hex(8c)),   opt$(19%)            , ch(15),~
               at (17,59), fac(hex(8c)),   desc$(19%)           , ch(20),~
/*EWD001*/     at (18,02), fac(lfac$(19)), str(option$,9%,1%)   , ch(01),~
               at (18,04), fac(hex(8c)),   opt$(9%)             , ch(15),~
               at (18,20), fac(hex(8c)),   desc$(9%)            , ch(20),~
/*EWD001*/     at (18,41), fac(lfac$(30)), str(option$,20%,1%)  , ch(01),~
               at (18,43), fac(hex(8c)),   opt$(20%)            , ch(15),~
               at (18,59), fac(hex(8c)),   desc$(20%)           , ch(20),~
/*EWD001*/     at (19,02), fac(lfac$(20)), str(option$,10%,1%)  , ch(01),~
               at (19,04), fac(hex(8c)),   opt$(10%)            , ch(15),~
               at (19,20), fac(hex(8c)),   desc$(10%)           , ch(20),~
/*EWD001*/     at (19,41), fac(lfac$(31)), str(option$,21%,1%)  , ch(01),~
               at (19,43), fac(hex(8c)),   opt$(21%)            , ch(15),~
               at (19,59), fac(hex(8c)),   desc$(21%)           , ch(20),~
/*EWD001*/     at (20,02), fac(lfac$(21)), str(option$,11%,1%)  , ch(01),~
               at (20,04), fac(hex(8c)),   opt$(11%)            , ch(15),~
               at (20,20), fac(hex(8c)),   desc$(11%)           , ch(20),~
/*EWD001*/     at (20,41), fac(lfac$(32)), str(option$,22%,1%)  , ch(01),~
               at (20,43), fac(hex(8c)),   opt$(22%)            , ch(15),~
               at (20,59), fac(hex(8c)),   desc$(22%)           , ch(20),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 3% then L41420
                  if program% = 0% then goto L41390
                  if size$ <> "E" then goto L41390
                     size$ = "O" : size_msg$ = "Opening           "
                     goto L40310
L41390:           size$ = "E" : size_msg$ = "Exact             "
                  goto L40310

L41420:        if keyhit% <> 7% then L41480                 /*EWD001*/
                  if program% = 0% then goto L41460
                     size$ = "F" : size_msg$ = "Exact (No Deduct) "
                     goto L40310
L41460:           size$ = "E" : size_msg$ = "Exact             "
                  goto L40310

L41480:        if keyhit% <>  6% then L41500        /*EWD001 - New*/
                  call "EWDPLA63" (0%, #4, "APC WOOD ", 0%, "A00")
                  goto L40310

L41500:        if keyhit% <> 9% then goto L41510
                  tripleplus% = tripleplus% + 1%
                  if tripleplus% > 2% then tripleplus% = 1%
                  goto L40310
                  
L41510:        if keyhit% <> 10% then goto L41520
                  northern% = northern% + 1%
                  if northern% > 2% then northern% = 1%
                  goto L40310
                  
L41520:        if keyhit% <> 11% then goto L41530
                  sash5_0% = sash5_0% + 1%
                  if sash5_0% > 2% then sash5_0% = 1%
                  goto L40310
                  
L41530:        if keyhit% <> 12% then goto L41540
                  ffoam% = ffoam% + 1%
                  if ffoam% > 2% then ffoam% = 1%
                  goto L40310                  
                                                      
L41540:        if keyhit% <> 15% then L41560
                  call "PRNTSCRN" : goto L40310

L41560:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               if x% > 7% then fieldnr% = 1%
/* (SR67154) */               	
               if x% > 11% then fieldnr% = 7%
/* (AWD006) */   /* (SR67154) */
               if fieldnr% >= 7% then     subpart% = 1%

               return

        set_pf1
        if edit% = 2% then L41720     /*  Input Mode             */
            pf$(1%) = "(1)Start Over           (09)TriplePlus  " &       ~
                      "(7)Exact No Deduction                  "
            pf$(2%) = "                        (10)NorthernNFRC" &       ~
                      "(12)ForcedFoam         (15)Print Screen"
            pf$(3%) = "(3)Toggle Size          (11)Sash Option " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ff0304ffff07ff090a0b0cffff0f1000)
                                         /* Turn-off PF(14) and PF(16)*/
            if fieldnr% = 1% then L41700
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41700:     return

L41720: if fieldnr% > 0% then L41830  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over           (09)TriplePlus  " &       ~
                      "                                       "
            pf$(2%) = "                        (10)NortherNFRC " &       ~
                      "(12)ForcedFoam         (15)Print Screen"
            pf$(3%) = "(3)Toggle Size          (11)Sash Option " &       ~
                      "(7)Exact No Deduction  (16)Finished    "
            pfkeys$ = hex(01ff03ffffff07ff090a0b0cffff0f1000)
            if errormsg$ = " " then goto L41820
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41820:     return
L41830:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over    (4)Prev(09)TriplePlus  " &       ~
                      "(6)View WS/FM Codes                     "
            pf$(2%) = "(2)First Page    (5)Next(10)NorthernNFRC" &       ~
                      "(12)ForcedFoam         (15)Print Screen"
            pf$(3%) = "(3)Toggle Size          (11)Sash Option " &       ~
/*EWD001*/            "(7)Exact No Deduction                  "
/*EWD001*/  pfkeys$ = hex(0102030405060708090a0b0cffff0fff00)
            if savkey% > 2% then goto L41930
               str(pf$(1%),18%,7%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41930:     if more_options% <> 0% then goto L41950
               str(pf$(2%),18%,7%) = " " : str(pfkeys$,5%,1%) = hex(ff)
L41950:     if fieldnr% >= 5% and wood_mull% = 1% then goto L41970 
 /*^EWD001->*/ str(pf$(3%),41%,21%) = " " : str(pfkeys$,6%,1%) = hex(ff)
L41970:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50110          /* Part Number            */
            return

L50110: REM Test for Part Number                  PARTNO$
                                                        /* (EWD003)   */
            lit_flg$ = "N"
            if str(partno$,1%,3%) = "003" then lit_flg$ = "Y"
                                                        /* (EWD003)   */
            if database% <> 0% then goto L50160
               errormsg$ = "Part Field Definitions are Un-defined, you"  ~
                           & " are in the Wrong Database."
               return
L50160:     REM IF MODEL$ <> " " THEN GOSUB CHECKMODEL /* (SR67154) */
	          

            if str(partno$,1%,1%) = " " then partno$ = " "
            if str(partno$,1%,1%) <> "?" then goto L50220
               partno$ = " "
               mscdescr$ = hex(06) & "Select MFG Part Number"
               call "PLOWCODE" (#3, partno$, mscdescr$, 0%, .30, f1%(3))

L50220: REM Test for Series Prompt
            if str(partno$,1%,1%) <> "+" then goto L50260
               goto exit_program

L50260: REM Begin Scan of Fields
            chosen%, exclude%, i%, x% = 0% : scr_desc$, prt_desc$ = " "
            value%, upper%, lower%, calc%, more_options% = 0%
            calc_part1% = 0%                    /* (AWD006) */
            subpart%    = 0%                    /* (AWD006) */
                                           /* Set Primary Fld - KEY   */
L50300:     str(defkey$,1%,15%) = str(partno$,1%,pos%(2%) - 1%)
            x% = x% + 1%
            wood_mull% = 0%
/* (CR2444) */
/* this caused issue with model. model not initalized back to " "   */
/* Patio 374 caught pricing as 314                                  */
REM            IF MODEL$ = " " THEN MODEL$ = STR(PARTNO$,1%,3%)
REM            MODEL$ = STR(PARTNO$,1%,3%)            /* (CR2451) */
REM            This ends the loop through fields
            if length%(x%) = 0% then return           /* Check Length  */

REM (AWD006)  BEGIN
REM         This is to set up subpart$ screen to open field for first time
            if x% > 11% and subpart% <> 1% then goto L55000
REM         This is so it will skip the mull and clmr
            if x% > 11% and subpart% =  1% then goto L50500

REM         10th fields is mull and clmr
REM (AWD006)  END
            if x% < 10% then goto L50500       /* Control Screen Prompts*/
                  if x% = 10% and lit_flg$ = "Y" then return /* (EWD003) */

                  if x% <> 10% then goto L50440
                     str(partno$,pos%(x%),length%(x%)) = "000"
                     if str(partno$,9%,2%) >= "70" and                   ~
                        str(partno$,9%,2%) <= "97" then goto L50500  /* (EWD004)*/
                     if str(partno$,1%,1%) = "9" then goto L50300
                        str(partno$,pos%(x%),length%(x%)) = "   "
                        wood_mull% = 1%
                        i% = 1%
                        goto L50500

L50440:           if str(partno$,1%,1%) = "9" then goto L50500
                     wood_mull% = 1%
                  if x% = 11% and str(partno$,9%,2%) >= "70" and      ~
                       str(partno$,9%,2%) <= "97" then goto next_option
                  if str(partno$,9%,2%) < "70" then goto L50300
                  if str(partno$,9%,2%) >= "A1" and                  ~
                         str(partno$,9%,2%) <= "A5" then goto L50300
                     wood_mull% = 1%
                     i% = 1%
                     goto L50830

L50500:        table$ = table_file$(x%)               /* SET TABLE NAME*/
               if table$ = " " then table$ = name$(x%)    /*Field Name */
               desc_1$ = "Description         "
               str(defkey$,16%,2%) = field$(x%)       /* SET FIELD NO. */
               
REM (AWD006)
               if subpart% = 1% then goto set_value_1
                                            /* SET VALID KEY VALUE     */
               str(defkey$,18%,15%) = str(partno$, pos%(x%), length%(x%))
                                            /* APC MOD - Check Min/Max */
               if x% > 7% then str(defkey$,18%,15%) = " "   /* Min/Max */
 /* (AWD006) - BEGIN */
                     goto read_value                          
set_value_1:
               str(defkey$,18%,15%) = str(subpart$,(pos%(x%)-25%), length%(x%))
               if x1% > 6% then str(defkey$,18%,15%) = " "  
read_value:
 /* (AWD006) - END */
               read #2,key = defkey$, eod goto L50610 /*Check for Match */
                  chosen% = 1%                      /* Valid Option    */
                  goto L50670                        /* Found           */
L50610:        savkey% = 1%                         /* Build Options   */
               savkey$(savkey%) = defkey$

        next_option                                 /* Loop for Options*/
               more_options% = 0%                   /* Set End of File */
               read #2,key > defkey$, eod goto choose_option
L50670:          get #2 using L50690, def_mod$, fld_num$, value$, incl$,  ~
                                     screen_desc$, printer_desc$
L50690:            FMT CH(15), CH(2), CH(15), CH(1), CH(20), CH(30)
               str(defkey$,1%,15%)  = def_mod$
               str(defkey$,16%,2%)  = fld_num$
               str(defkey$,18%,15%) = value$
               if str(partno$,1%,pos%(2%)-1%) <> " " then goto L50760
                  goto L50780

L50760:        if str(partno$,1%,pos%(2%)-1%) <> def_mod$ then           ~
                                                 goto choose_option
L50780:        if fld_num$ = field$(x%) then L50830 /*PUT IN BUFF TO SEL*/
                  str(defkey$,16%,2%) = "99"
                  goto next_option              /* SKIP ALL OTHER FLDS */


L50830:        more_options% = 1%             /* Continue Scan for More*/
               i% = i% + 1%
               if i% <> 22% then goto L50890    /* NOT LAST ONE         */
                  savkey% = savkey% + 1%
                  savkey$(savkey%) = defkey$
REM Display first 22 items on screen from validation
L50890:        if i% < 23% then goto L50940     /* STILL ROOM ON SCREEN */
                  goto choose_option           /* SCREEN FULL - CHOOSE */

                                               /* NO SELECTION MADE    */
               init (" ") opt$(), screen$(), printer$(), desc$()
L50940:        if incl$ <> "E" then goto L50970
                  exclude% = exclude% + 1% : exclude$(exclude%) = value$
                  goto next_option                  /* Get Next Option */
L50970:        opt$(i%)     = value$           /* Load for Selection   */
               screen$(i%)  = screen_desc$
               printer$(i%) = printer_desc$
                                               /* Check for Min/Max    */
               if wood_mull% = 1% then goto L51030
               if incl$ <> "U" and incl$ <> "L" then goto L51190
L51030:           if incl$ = "U" then desc$(i%) = "Upper Limit"          ~
                                 else desc$(i%) = "Lower Limit"
                  calc% = 1%
                  if incl$ = "U" then convert value$ to upper%           ~
                                 else convert value$ to lower%
                  if x% < 10% then goto L51180     /* SKIP WIDTH/HEIGHT */
                  if x% > 11% then goto L51180          /* (AWD006)  */
                  if x% = 10% and str(partno$,9%,2%) >= "70"             ~
/* (EWD004) */                and str(partno$,9%,2%) <= "97" then L51180
                  if x% = 11% and str(partno$,1%,1%) = "9" then L51180

                  table$ = "APC Wood" 
                                                    /* (EWD003)         */      
                  desc_1$ = "Wood/Sample Code   "
/*EWD001*/        opt$(i%), desc$(i%) = " "     /* Blank Out Extraneous */
                  upper% = 80% : opt$(2%) = "Z99"
                  desc$(2%) = "Max Wood/Samp Code"
                  lower% =  1% : opt$(1%) = "001"
                  desc$(1%) = "Min Wood/Samp Code"
                  if lit_flg$ = "N" then goto L51180 /* (EWD003)       */
                     desc$(2%) = "Max Literature Code"
                     desc$(1%) = "Min Literature Code"
                     opt$(1%) = "A00"
                                                    /* (EWD003)        */
L51180:           goto next_option                  /* Get Next Option */
L51190:        str(gencode$,1%,9%)   = table_file$(x%)
               str(gencode$,10%,15%) = value$
               read #4,key = gencode$, using L51230, desc$(i%),           ~
                                                           eod goto L51240
L51230:           FMT POS(25), CH(20)
L51240:        goto next_option                     /* Get Next Option */

        choose_option
            option$ = " "
            if exclude% > 0% then gosub exclude
               if chosen% <> 1% then goto L51320   /* Choose an Option  */
                  i% = 1%                         /* Option Selected   */
                  goto L51740
L51320:        
REM (AWD006) - BEG
               if subpart% <> 0% then goto set_subpart

               if str(partno$, pos%(x%), length%(x%)) = " " then         ~
                               goto choose_screen /* Make a Selection  */
                      goto finish_option
set_subpart:
               
               if str(subpart$, (pos%(x%)-25%), length%(x%)) = " " then         ~
                               goto choose_screen /* Make a Selection  */
finish_option:
REM (AWD006) - END
               if calc% = 1% then goto choose_screen


               if str(partno$, pos%(x%), length%(x%)) <> opt$(1%) then   ~
                  str(partno$, pos%(x%), length%(x%)) = " "
        choose_screen                         /* Display Options Screen*/
               scrn_field% = 2%                  /* (AWD006) */
               if subpart%  = 1% then scrn_field% = 9%      /* (AWD006) */

REM            gosub'101(2%, 2%)              /* Display Valid Options */
               gosub'101(scrn_field%, 2%)     /* (AWD006) */
                 if keyhit% = 1% then goto startover
                 if keyhit% <> 2% then goto L51450             /* FIRST */
                    savkey% = 1%
                    goto L51520
L51450:          if keyhit% <> 4% then goto L51510          /* PREVIOUS */
                    if more_options% = 1% then savkey% = savkey% - 2%    ~
                                          else savkey% = savkey% - 1%

                    if savkey% < 1% then savkey% = 1%
                    goto L51520
L51510:          if keyhit% <> 5% then goto L51570          /* NEXT PG  */
L51520:             defkey$ = savkey$(savkey%)
                    init (" ") opt$(), screen$(), printer$(), desc$()
                    i% = 0%
                    goto next_option
REM (AWD006)   - BEG
L51570:        if calc% <> 1% and subpart% = 0% then goto L51660
               if calc_part1% <> 1% and subpart% = 1% then goto L51660
                   if subpart% <> 0% then goto L51660
REM (AWD006)   - END
                  gosub check_wd_ln          /* Verify Width an Height */
                  if errormsg$ <> " " then goto choose_option
                     printer$(i%) = str(partno$, pos%(x%), length%(x%))
                     screen$(i%) = str(partno$, pos%(x%), length%(x%))
                     if wood_mull% <> 1% then goto L51740
                        prt_desc$ = prt_desc$ & " " & wood_desc$
                        scr_desc$ = scr_desc$ & " " & wood_desc$
                     goto L51740
L51660:        i% = pos(option$ = "X")          /* CHECK FOR SELECTION */
               if i% = 0% then goto choose_option /* NO SELECTION MADE */
               if opt$(i%) = " " then goto choose_option  /* NOT VALID */

               if subpart% = 0%               then      /*(AWD006) */  ~
               str(partno$, pos%(x%), length%(x%)) = opt$(i%)
               if subpart% <> 0%              then     /* (AWD006) */  ~
               str(subpart$,(pos%(x%)-25%),length%(x%)) = opt$(i%)

               if prt_desc$ <> " " then goto L51740
                  if printer$(i%) <> " " then prt_desc$ = printer$(i%)
                  if screen$(i%) <> " "  then scr_desc$ = screen$(i%)
                  goto L51780
L51740:        if printer$(i%) <> " " then                               ~
                     prt_desc$ = prt_desc$ & " " & printer$(i%)
               if screen$(i%) <> " " and x% < 8% then                    ~
                                scr_desc$ = scr_desc$ & " " & screen$(i%)
L51780:        init (" ") desc$(), opt$(), option$, printer$(), screen$()
               chosen%, exclude%, i% = 0%
               goto L50300

        exclude
            i% = i% - 1% : exclude% = 0%
            str(gencode$,1%,9%)   = table_file$(x%)
            str(gencode$,10%,15%) = all(hex(00))
L51860: REM CALL "READ102" (#4, GENCODE$, F1%(4))
            read #4,key > gencode$, using L51880, gencode$, eod goto L51900
L51880:         FMT CH(24)
                goto L51920
L51900:     return

L51920:     get #4 using L51930, gen_table$, gen_value$, gen_desc$
L51930:         FMT CH(9), CH(15), CH(20)
            if gen_table$ <> table_file$(x%) then return
            str(gencode$,10,15) = gen_value$
L51960:     exclude% = exclude% + 1%
            if exclude% > 28% then goto L52050
            if exclude$(exclude%) = " " then goto L52050
            if gen_value$ = exclude$(exclude%) then goto L51860
            plus% = pos(exclude$(exclude%) = "+")
            if plus% = 0% then goto L51960
            if str(gen_value$,1,plus% - 1%) =                            ~
                     str(exclude$(exclude%),1,plus% - 1%) then goto L52050
            goto L51960
L52050:     if str(partno$, pos%(x%), length%(x%)) = " " then goto L52080
            chosen% = 1%
            if gen_value$ <> str(partno$,pos%(x%),length%(x%)) then L51860
L52080:     i% = i% + 1%
            opt$(i%) = gen_value$
            desc$(i%) = gen_desc$
            exclude% = 0%
            goto L51860

REM            Second Part Number     Loop                  /*(AWD006) - BEG */
L55000:
            x1%, chosen%, exclude%, i% = 0%
REM            SUBPART% = 1%

            gosub'101(7%,2%)

            goto L50500
            gosub choose_screen

            x1% = x1% + 1%
REM            x%  = x%  + 1%

            str(defkey$,1%,15%) = str(subpart$,1%,(pos%(12%)-25%) - 1%)
            calc_part1% = 1%
            gosub L50500

REM            Second Part Number     Loop                  /*(AWD006) - END */
       return

REM        CHECKMODEL
REM          INIT(" ") GENCODE$
REM          GENCODE$ = "VIRTUALMD" & STR(MODEL$,1%,15%)
REM          CALL "DESCRIBE" (#4, GENCODE$, " ", 0%, F1%(4))        
REM          STR(PARTNO$,1%,3%) = MODEL$
REM        RETURN
        
        REM *************************************************************~
            *          S P E C I A L   R O U T I N E S                  *~
            *************************************************************

        check_wd_ln
           if subpart% <> 0% then return          /*(AWD006) */
           init(" ") wood_desc$, wood$    /* Wood Surround Description */
           errormsg$ = " " : value% = 0% : wood% = 0% /* and Code.     */
           wood$ = str(partno$,pos%(x%),length%(x%))  /* Store Input   */
           if lit_flg$ = "Y" then return              /* (EWD003)      */

           if wood_mull% <> 1% then goto L60110  /* Only Check WoodSurr*/
              if pos("0ABCDEFGHIJKLMNOPQRTSUVWXYZ" = str(wood$,1%,1%))    ~
                                                  <> 0 then goto L60130
L60110:    convert wood$ to value%, data goto L60170

L60130:    if wood_mull% = 1% then goto L60210
           if wood_mull% = 0% and value% = 0% then return

           if value% < lower% or value% > upper% then                    ~
                                       errormsg$ = "Invalid " & name$(x%)
        return
L60170:    if wood_mull% = 1% then return        /* BLANK VALID */
           errormsg$ = "Invalid " & name$(x%)
           str(partno$,pos%(x%),length%(x%)) = "   "
        return
L60210:    convert wood$ to wood%, data goto L60250
                                                       /* (EWD003)      */
              if wood% > 0% and wood% < 81% then goto L60250
                                                       /* (EWD003)      */ 
                                                       /* (EWD005)      */ 
              if str(partno$,7%,2%) > "99" and wood$ < "A00" then return

              goto L60310

L60250:    str(gencode$,1%,9%) = "APC WOOD "
           str(gencode$,10%,15%) = wood$
           read #4,key = gencode$,using L60280,wood_desc$, eod goto L60310
L60280:       FMT POS(25), CH(6)
           if str(partno$,pos%(x%),length%(x%)) = "000" then goto L60310
        return
L60310:    errormsg$ = "Invalid " & "WOOD/MULL CODE"
           str(partno$,pos%(x%),length%(x%)) = "   "
 
        return

        build_description                     /* CUSTOMIZED FOR 'APC' */
                              /* PARTNO$   - NEW BUILD PART NUMBER(25)*/
                              /* PRT_DESC$ - NEW BUILD DESCRIPTION(32)*/
                              /* APC_SCR$  - SCREEN DESCRIPTION  (120)*/
                              /* APC_PRT$  - PRINT DESCRIPTION   ( 60)*/
                              /* APC_SZE$  - SIZE DESCRIPTION    ( 20)*/

           str(prt_desc$,17%,16%) = "<< E R R O R >> "
/* (AWD006)  */
REM           call "APCDESCR" (partno$,apc_scr$,apc_prt$,apc_sze$,#2,er%)

           call "AWDDESCR" (partno$, subpart$, apc_scr$, apc_prt$,        ~
                             sub_scr$, sub_prt$, apc_sze$, #2, er%)
           if er% <> 0% then goto L60480
              str(prt_desc$,1%,16%)  = str(apc_prt$,1%,16%)
              str(prt_desc$,17%,16%) = str(apc_sze$,1%,16%)
              scr_desc$ = apc_scr$ & sub_scr$

REM           call "SHOSTAT" ("DESCR " )   stop
L60480: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        exit_program
                                    /* EITHER COMPONENT OR NON-STOCK */
/*EWD001*/  if len(partno$) < 19% and pc(1) + p1 = 0 then err% = 6%
            if pc(1) + p1 = 0 and str(partno$,5%,4%) = "WARR" then err% = 0%
            if pc(1) + p1 = 0 and str(partno$,5%,5%) = "LABOR" then err% = 0%
            end





