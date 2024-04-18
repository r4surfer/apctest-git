        REM *************************************************************~ 
            *             (RHHTEST)         Five Label test turned (OFF)*~ 
            *  Program Name      - EWDPLN71                             *~  
            *  Creation Date     - 04/28/99                             *~  
            *  Last Modified Date- 10/17/07                             *~ 
            *  Written By        - Roy H. Hoffman                       *~  
            *  Modifications By  - Roy H. Hoffman                       *~ 
            *                                                           *~
            *  Description       - Production Labels Printing.          *~
            *                      Prod. Date, Dept. No., Shift & Load  *~ 
            *                      Range Specified.                     *~
            *                      Barcode Range Specific               *~
            *                                                           *~  
            *  Code Tables Used  - PLAN DEPT, PLAN 100, PLANPARTS,      *~     
            *                      SKIP PRT                             *~
            *                                                           *~  
            *  Subroutine Used   - EWDPLA71 (Print Production Labels)   *~
            *                      Changed for new Sub Part Number      *~
            *                                              (PAR000)     *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/28/99 ! (New) Program - Copied & Mod EWDPLN65.   ! BWS *~
            * 06/30/99 ! (EWD001) - Mods for new production Labels! RHH *~
            * 07/07/99 ! (EWD002) - Mod for Barcode Range's       ! RHH *~  
            * 07/19/99 ! (EWD003) - Mods for Documentation        ! RHH *~
            * 08/16/99 ! (EWD004) - Mods to (EWDPLA71) to high Lit! RHH *~ 
            *          !    Exact Size both Text and Size.        !     *~
            * 09/22/99 ! (EWD005) - Mod to Label print to High Lit! RHH *~
            *          !    Bay-Bow                               !     *~
            * 11/12/99 ! (EWD006) - Mod To label to print 'SAMPLE'! RHH *~
            * 02/23/00 ! (EWD007) - Mod to print two sample Labels! RHH *~
            *          !            for Sample Code (011)         !     *~
            * 07/10/01 ! (EWD008) - Mod to label selection for    ! RHH *~
            *          !            Special Shapes. Special Shapes!     *~
            *          !            are in department '104'       !     *~
            *          !            Frames are flagged at position!     *~
            *          !            pd_lab$(),592%,1%) = 'Y'      !     *~
            * 08/30/02 ! (EWD009) - Mod to label printing. New    ! RHH *~
            *          !            field on the label showing the!     *~
            *          !            S.O line item number.         !     *~
            * 06/04/03 ! (EWD010) - Mod to display 'P' for all    ! RHH *~
            *          !            parts. Also display '@@@@@@@' !     *~
            *          !            for Customers requiring 100%  !     *~
            *          !            Inspection. Replace drop with !     *~
            *          !            Customer Code.                !     *~
            * 06/09/03 ! (EWD011) - Put a range on Production     ! RHH *~
            *          !            Date                          !     *~
            * 06/11/03 ! (EWD012) - Improve print speed           ! RHH *~
            * 06/12/03 ! (EWD014) - Skip Repair for Printing for  ! RHH *~
            *          !            special test                  !     *~
            * 06/16/03 ! (EWD015) - Remove Contractor, Job, Room, ! RHH *~
            *          !            and Nominal Size from Label.  !     *~
            *          !            Put Last 3 digits of S.O. '-' !     *~
            *          !            and WW Confix Line No. SSS-WW !     *~
            * 06/17/03 ! (EWD016) - Put Wood Surround Sequence No ! RHH *~
            *          !            on production label.          !     *~
            *          !                                          !     *~
            * 06/26/03 ! (EWD017) - Fix Sample Parts Problem      ! RHH *~ 
            * 06/30/03 ! (EWD018) - Mods for Label Changes        ! RHH *~
            * 02/02/04 ! (AWD019) - Mods for Continous Head and   ! RHH *~
            *          !            Mull Codes 'A01' 'A16' 'A17'  !     *~
            *          !            and Screens                   !     *~
            * 05/21/04 ! (AWD020) - Mod for Special Mull codes,   ! RHH *~
            *          !            for specific Codes put an "A" !     *~
            *          !            or put a "D" use a new table  !     *~
            *          !            'PROD MULL'                   !     *~
            * 06/29/04 ! (AWD021) - Mod to label. Two Changes.    ! RHH *~
            *          !            1. Seq printed White on Black !     *~
            *          !            2. For Specials 'SP' printed  !     *~
            *          !               in Box Right of the P.O No.!     *~
            * 06/30/04 ! (AWD022) - Replace UPC with Larger Prod  ! RHH *~
            *          !            Sequence Number.              !     *~
            * 08/11/04 ! (AWD023) - Mod for Specials made         ! RHH *~
            *          !            New Table SPECCHECK'          !     *~
            * 05/06/05 ! (AWD024) - Mod to Screens that print on  ! RHH *~
            *          !            on label. '389DHNR' Half Screen!    *~
            * 06/28/05 ! (AWD025) - Mod to put UPS on Prod. Label ! RHH *~
            * 08/02/05 ! (AWD026) - Mod to allow production Mgr's ! RHH *~
            *          !            print single label when logged!     *~
            *          !            using 'WWW' User Id.          !     *~   
            * 09/29/05 ! (AWD027) - Mod to put Big 'B' on label   ! RHH *~
            *          !            for Bay/Bow                   !     *~
            * 01/01/06 ! (PAR000) - CR347 Mod for New Sub Part No.! RHH *~
            * 03/03/06 ! (PAR001) - Mod to production label for   ! RHH *~
            *          !            the New Sub Part Number.      !     *~
            * 03/20/06 ! (PAR002) - Mod to Print Labels for North ! RHH *~
            *          !            East.                         !     *~   
            * 0412/06  ! (PAR003) Mod to move the 100% Inspection ! RHH *~
            *          !          to the right about 1/4 inch. If !     *~
            *          !          move too far will conflict with !     *~
            *          !          the printing of 'SAMPLE'        !     *~
            * 07/27/06 ! (PAR004) Mod for special Mull code added ! RHH *~
            *          !          to the production label file at !     *~
            *          !          position (687%,1%)              !     *~
            * 10/17/07 ! (AWD028) mod for 100% bay bow            ! CMG *~ 
            * 08/28/15 ! SR68332  Mod to add new fields from      ! PWW *~
            *          !          oradesc2 if it is a part.       !     *~
            * 01/04/16 ! SR67154  2016 NFRC reg changes for foam. ! PWW *~
            * 05/26/17 ! SR99999  For dept ALL no print dept 041  ! RDB *~
            *          !          Add sequence range              !     *~
            * 08/10/17 ! CR1073   Change layout for AD LANE       ! RDB *~
            * 01/11/18 ! SR83259  Allow dept 041 to print with all! RDB *~
            * 04/26/18 ! CR1478   Repeat - Stop dept 041 print    ! RDB *~
            * 07/29/18 ! CR1612   Allow dept 041 to print with all! RDB *~
            * 11/09/18 ! CR1790   Add schema; stop dept 101 print ! RDB *~
            *          !          only when entry dept and brcde ALL    *~      
            * 04/26/18 ! CR1895   Repeat - Stop dept 041 print    ! RDB *~
            *01/30/2019! CR-1896  Add GENCODES table "SKIP PRT"   ! DES *~
	        *          !          to skip printing (see CR-1895)  !     *~            
            * 02/21/19 ! CR1918   New PlyGem PO and SKU           ! RDB *~
            * 05/01/19 ! CR2001   Add Oradesc2 PG Series to label ! RDB *~
            *12/04/2019! CR2219 Allow A loads in Dept 104 to print! RDB *~
            *          ! production labels                        !     *~  
            *10/02/2020! CR2690 Validate SKU in awdskuxr.         ! RDB *~         
            *07/28/2022! CR3143 Work file to sort dept 101 NC     ! RDB *~			
            *************************************************************
           
        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            count$22,                    /* Display                    */~ 
            sc_dept$3, sc_dept_d$30,     /* Department Code & Descr    */~
            sc_prddate$10,               /* Production Date            */~
            sc_end_prddate$10,           /* Ending Production date     */~
            sc_load_fr$5, sc_load_to$5,  /* Load Range                 */~
            sc_shift$2,                  /* Shift Code                 */~
            sc_barcode_fr$18,            /* Beginning Barcode  (EWD002)*/~
            sc_barcode_to$18,            /* Ending Barcode     (EWD002)*/~       
            sc_seq_fr$5,                 /* Beginning Sequence (SR9999)*/~
            sc_seq_to$5,                 /* Ending Barcode     (SR9999)*/~       
            lb_key$35, lb_key1$23,       /* Record Key from LB (PAR001)*/~
            lb_rec$(4%)256,              /* LB Record Data     (PAR001)*/~
			lb2_rec$(4%)256,             /* LB Sort File               */~
            lb_prddate$10,               /* Prod. Date from LB         */~
            lb_dept$3,                   /* Dept. No. from LB          */~
            lb_shift$2,                  /* Shift Code from LB         */~
            lb_load$5,                   /* Load Number from LB        */~
            lb_barcode$18,               /* Production Barcode (EWD002)*/~
            lb_foam$1,                   /* Foam Flag Y,N,0,1  (EWD014)*/~ 
            lb_seq$5,                    /* Sequence number            */~
            filename$8,                  /* Used by EWDOPEN            */~ 
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~ 
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            schema$8,                    /* Schema Switch              */~
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
            dim apc$40, pname$21                            /* (AWD023) */
                                                            /* (AWD025) */
                                                            /* (AWD026) */
                                                            /* (AWD027) */
                                                            /* (PAR001) */
                                                            /* (PAR002) */
                                                            /* (PAR003) */
                                                            /* (PAR004) */
            apc$   = "Generate Production Labels- 07-27-2006"
            pname$ = "EWDPLN71 - Rev: R1.00"

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
            * #1  ! APCPLNDT ! Production Master Detail File            *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! EWDPRDLB ! Production Label Data File       (PAR001)*~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #8  ! AWDAPPLS ! Appian New Shipping Label File  (CR1073) *~
            * #9  ! AWDSKUXR ! Lowes sku cross refference file (CR2690) *~
            * #63 ! BCKSUBPT ! New Sub Prt Number File                  *~
            * #16 ! BCKMASTR ! Backlog master file                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
                                                    /* (EWD016)         */
            select #1,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
                                                    /* (EWD016)         */

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

                                                    /* (EWD001) - New   */
                                                    /* (PAR001)         */  
            select #5, "EWDPRDLB",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23 
                                                    /* (PAR001)         */
                                                    /* (EWD001) - File  */
/*(AWD028) */    
            select #6, "BCKLINES",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19
/* CR1073 */                        
            select #8, "AWDAPPLS",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   20,                    ~
                        alt key 1,  keypos = 21,   keylen =  34, dup ,   ~
                            key 2,  keypos = 23,   keylen =  32, dup ,   ~
                            key 3,  keypos = 56,   keylen =  10, dup
/* CR2690 */
            select #9,   "AWDSKUXR",                                     ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =    1, keylen =  16,                    ~
                        alt key  1, keypos =  17, keylen =  20,         ~
                            key  2, keypos =  37, keylen =  45, dup
                            
                                                        /* (PAR000)     */
            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                                       /* (PAR000)     */
                                                       /* (SR68332)    */
            select #58, "ORADESC2",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =   1,   keylen = 11
/* CR1918 */ 
            select #16, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup
						
            select #12, "WRKPRDLB",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
						keypos = 1, keylen = 40				
                        
            call "SHOSTAT" ("Opening Files, One Moment Please")
                                                    /* (EWD016)         */
            filename$ = "APCPLNDT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
                                                    /* (EWD004)         */
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
/* (AWD028) */
            filename$ = "BCKLINES" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
/* CR1073 */
            filename$ = "AWDAPPLS" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
/* CR2690 */
            filename$ = "AWDSKUXR" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
                                                    /* (PAR000)         */
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
/*SR68332 */
            filename$ = "ORADESC2" : call "EWDOPEN" (#58, filename$, err%)
            if err% <> 0% then gosub open_error
               
            filename$ = "BCKMASTR" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
			
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            if fs%(12%) < 0% then goto create_sort_file
                 call "FILEBGON" (#12)
create_sort_file:
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),500%, rslt$(12%))
			
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
            call "TIME" (time$)
            
            err%    = 0%
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #4,           /* GENCODES                   */~
                           err% )        /* error                      */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
 
        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   6%
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
                  if keyhit%  = 16% then       dataload
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 6% then editpg1
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

        scrn1_msg  :  data                                               ~
         "Enter a Production Date Beginning and Ending?                ",~
         "Enter a Specific Department Code or 'ALL'?                   ",~
         "Enter a Specific Shift or 'AA' for All?                      ",~
         "Enter a Load Range or 'ALL'?                                 ",~
         "Enter a Barcode Range or 'ALL'?                              ",~  
         "Enter a Sequence Range or 'ALL'?                             "  

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
            init(" ") errormsg$, inpmessage$, sc_dept$, sc_dept_d$,      ~
                      sc_prddate$, sc_load_fr$, sc_load_to$, sc_shift$,  ~
                      sc_barcode_fr$, sc_barcode_to$, lb_barcode$,       ~
                      sc_end_prddate$, sc_seq_fr$, sc_seq_to$
                                                       /* (EWD011)     */
           lbl% = 0%

        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload                                         /* (EWD001)   */
            call "SHOSTAT" ("Printing Production Labels...")
            count$ = "Labels Printed (xxxxx)"
            call "DATUFMTC" (sc_prddate$)

            call "DATUFMTC" (sc_end_prddate$)            /* (EWD011)   */

            been_here% = 0%

            lb_key$ = all(hex(00))
            str(lb_key$,1%,6%) = sc_prddate$
          load_next_rec
            read #5, key > lb_key$, using L35040, lb_rec$(),            ~
                                                      eod goto load_done
                                                     /* (AWD025)       */
                                                     /* (PAR001)       */
            lb_key$ = str(lb_rec$(),1%,35%)
                                                     /* (PAR001)       */
            lb_prddate$ = str(lb_key$,1%,6%)
            lb_dept$    = str(lb_key$,12%,3%)
    
            lb_shift$   = str(lb_key$,15%,2%)
            lb_load$    = str(lb_key$,22%,5%)
                                                     /* (PAR001)       */
            lb_barcode$ = str(lb_rec$(),278%,18%)
                                                     /* (PAR001)       */
            lb_foam$    = str(lb_rec$(),601%,1%)     /* (EWD014)       */
                                                     /* (PAR001)       */
                                                     /* (EWD011)       */
            lb_seq$     = str(lb_rec$(),311%,5%)     /* SR9999         */
         		
            if lb_prddate$ > sc_end_prddate$ then goto load_done

            if lb_shift$ <> sc_shift$ and sc_shift$ <> "AA"              ~
                then goto load_next_rec

            if sc_dept$ = "ALL" then goto L30000
               if lb_dept$ <> sc_dept$ then goto load_next_rec

L30000:    
/* CR1895 */ /* if sc_dept$ = "ALL" and lb_dept$ = "041" and schema% = 2% ~    
                then goto load_next_rec  */   /* TX not print dept 041 */
/* <CR-1896> */
            if sc_dept$ <> "ALL" then goto skip_print_test                  
            if sc_shift$ <> "AA" then goto skip_print_test              
            if str(sc_load_fr$,1%,3%) <> "ALL" then goto skip_print_test 
            if str(sc_barcode_fr$,1%,3%) <> "ALL" then goto skip_print_test    
            if str(sc_seq_fr$,1%,3%) <> "ALL" then goto skip_print_test        
            init(" ") readkey$
            str(readkey$,1%,9%)   = "SKIP PRT "
            str(readkey$,10%,15%) = lb_dept$
            read #4,key = readkey$, eod goto skip_print_test
            goto load_next_rec 

skip_print_test: 
/* </CR-1896> */
            
/* CR1790 */
            if sc_dept$ = "ALL" and sc_barcode_fr$ = "ALL"    ~
              and lb_dept$ = "101" and schema% = 2%  ~
                then goto load_next_rec  
            
            if str(sc_load_fr$,1%,3%) <> "ALL" then goto L30010
/* CR2219 */
               if (schema% = 2%) and pos("AS" = str(lb_load$,1%,1%)) > 0 then  ~
                                            goto load_next_rec
               if (schema% = 1%) and pos("S" = str(lb_load$,1%,1%)) > 0 then   ~
                                            goto load_next_rec
               if (schema% = 1%) and pos("A" = str(lb_load$,1%,1%)) > 0 and    ~
                  lb_dept$ <> "104" then goto load_next_rec
               goto L30020
  
L30010:     if (lb_load$ < sc_load_fr$ or lb_load$ > sc_load_to$)        ~ 
                                                  then goto load_next_rec
                                                /* (EWD002) - 07/07/99 */
L30020:     if (lb_barcode$ < sc_barcode_fr$ or lb_barcode$ > sc_barcode_to$)~
                and str(sc_barcode_fr$,1%,3%) <> "ALL" then              ~
                           goto load_next_rec
            goto L30025

L30025:     if (lb_seq$ < sc_seq_fr$ or lb_seq$ > sc_seq_to$)~
                and str(sc_seq_fr$,1%,3%) <> "ALL" then              ~
                goto load_next_rec 
                        
                                                /* (EWD014) - 06/12/03 */
                                                /* Check for Repair    */
               gosub check_repair
               if lb_foam% = 1% then goto load_next_rec  /* Skip       */
 
                                                /* (EWD004) - 08/16/99 */
                                                /* (EWD005) - 09/22/99 */
                                                /* (EWD009) - 08/30/02 */
                                                /* (EWD010) - 06/04/03 */
                                                /* (EWD015) - 06/16/03 */
            err% = 0%                           /* (EWD016) - 06/17/03 */
                                                /* (EWD017) - 06/26/03 */
                                                /* (EWD018) - 06/30/03 */
                                                /* (AWD019) - 02/02/04 */
                                                /* (AWD020) - 05/21/04 */
                                                /* (AWD021) - 06/29/04 */
                                                /* (AWD025) - 06/15/05 */
                                                /* (PAR000) - 01/01/06 */
                                                /* (PAR002) - 03/20/06 */
                                                /* (PAR003) - 04/10/06 */
                                                /* (PAR004) - 07/27/06 */  
/*  CR3143 */											
          if sc_dept$ = "102" and schema% = 1% then goto skipprint											
       
/*(AWD028) SR68332 #58 CR1073 #8 */       
/* CR1918 at #16 */ 
/* CR2690 add #9 */              
         call "EWDPLA71" (been_here%, lb_rec$(), #4, #1, #6, #63, #58, #8, #9, ~
                          #16, err%)   
                if err% <> 0% then gosub print_error
				
skipprint:
            lbl% = lbl% + 1%                     /* (EWD015)           */
                                                 /* (RHHTEST)          */
REM    if lbl% > 5 then goto Load_done
/* CR3143             */
            init(" ") lb2_rec$()
            str(lb2_rec$(), 1%, 5%) = lb_load$
			str(lb2_rec$(), 6%, 1019%) = lb_rec$()
            write #12, using L35040, lb2_rec$()
L30065:        FMT 4*CH(256)
                                                 /* (RHHTEST)          */
            if mod(lbl%,25%) <> 0 then goto load_next_rec
               convert lbl% to str(count$,17%,5%), pic(#####)

               call "SHOSTAT" (count$)
            goto load_next_rec 
 
        load_done
                                                /* (EWD010) - 06/04/03 */
                                                /* (EWD016) - 06/17/03 */
                                                /* (EWD018) - 06/30/03 */
                                                /* (AWD019) - 02/02/04 */
                                                /* (AWD020) - 05/21/04 */
                                                /* (AWD021) - 06/29/04 */
                                                /* (AWD024) - 05/06/05 */
                                                /* (AWD025) - 06/15/05 */
                                                /* (PAR000) - 01/01/06 */
                                                /* (PAR002) - 03/20/06 */
                                                /* (PAR004) - 06/23/06 */
                                                /* Finished            */   
/* CR1918 add #16 */                                      
/* CR2690 add #9  */      
/* CR3143 */
         if sc_dept$ = "102" and schema% = 1% then gosub process_inventory_print
            
         call "EWDPLA71" (been_here%, lb_rec$(), #4, #1, #6, #63, #58, #8, #9, ~
                          #16, 99%)
                if err% <> 0% then gosub print_error
            gosub load_results
			
            gosub clear_work_file		  /* CR3143 */	
            goto inputmode

        check_repair                            /* (EWD014)             */
            lb_foam% = 0%
            if lb_foam$ = "Y" or lb_foam$ = "N" then goto L30110

            p% =  pos("AS" = str(lb_load$,1%,1%))

            if str(sc_barcode_fr$,1%,3%) <> "ALL" then goto L30110

            if p% <> 0% then goto L30110

            lb_foam% = 1%                       /* Skip Label Print     */
L30110: return    
                                                /* (EWd014)             */
  
        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                                         /* (EWDPRDLB) */
                                                         /* (PAR001)   */
L35040:     FMT 4*CH(256)
                                                         /* (PAR001)   */

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
              on fieldnr% gosub L40170,          /* sc_prddate$        */~
                                L40160,          /* sc_dept$           */~
                                L40160,          /* sc_shift$          */~
                                L40160,     /* sc_load_fr$,sc_load_to$ */~
                                L40160,     /* sc_barcode_fr$ & _to$   */~
                                L40170      /* sc_seq_fr$, sc_seq_to$  */
                                

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Prod Beg/End Date:",                         ~
               at (03,25), fac(lfac$(1%)), sc_prddate$          , ch(10),~
               at (03,40), fac(lfac$(1%)), sc_end_prddate$      , ch(10),~
                                                                         ~
               at (04,02), "Dept. Code       :",                         ~
               at (04,25), fac(lfac$(2%)), sc_dept$             , ch(03),~
               at (04,40), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (05,02), "Shift Code       :",                         ~
               at (05,25), fac(lfac$(3%)), sc_shift$            , ch(02),~
                                                                         ~
               at (06,02), "Load No. Range   :",                         ~
               at (06,25), fac(lfac$(4%)), sc_load_fr$          , ch(05),~
               at (06,45), fac(lfac$(4%)), sc_load_to$          , ch(05),~
                                                                         ~
               at (07,02), "Barcode No. Range:",                         ~
               at (07,25), fac(lfac$(5%)), sc_barcode_fr$       , ch(18),~
               at (07,45), fac(lfac$(5%)), sc_barcode_to$       , ch(18),~
                                                                         ~
               at (08,02), "Sequence Range   :",                         ~
               at (08,25), fac(lfac$(6%)), sc_seq_fr$           , ch(05),~
               at (08,45), fac(lfac$(6%)), sc_seq_to$           , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
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
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)PRINT LABELS "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
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
            on fieldnr% gosub L50010,        /* Prod Beg/End Date      */~
                              L50050,        /* Department Code        */~
                              L50080,        /* Shift Code             */~
                              L50090,        /* Load No. Range         */~
                              L50200,        /* Barcode Range          */~
                              L50300         /* Sequence Range         */

            return
                                                  /* (EWD011)         */ 
L50010: Rem Enter a Production Date                sc_prddate$
            call "DATEOKC" (sc_prddate$, 0%, errormsg$)

            if len(sc_end_prddate$) < 5 then                  ~
                                  sc_end_prddate$ = sc_prddate$

            call "DATEOKC" (sc_end_prddate$, 0%, errormsg$)
   
            if sc_prddate$ > sc_end_prddate$ then goto L50015

            if sc_end_prddate$ < sc_prddate$ then goto L50020   
            return
L50015:    errormsg$ = "(Error) Invalid Beginning Production date?"
           gosub error_prompt
           init(" ") sc_prddate$, sc_end_prddate$
        return 
L50020:    errormsg$ = "(Error) Invalid Ending Production date?"
           gosub error_prompt
           init(" ") sc_prddate$, sc_end_prddate$
        return 


L50050: Rem Enter a Valid Department Code          sc_dept$, sc_dept_d$
            init(" ") sc_dept_d$
            if sc_dept$ <> " " then goto L50055
               sc_dept$ = "ALL"

L50055:     if sc_dept$ <> "ALL" then goto L50060
                sc_dept_d$ = "*** All Departments"
                return
L50060:     gosub check_dept
            if dept% = 0% then goto L50070
               sc_dept_d$ = desc$
        return

L50070:     errormsg$ = "(Error) Invalid Department Code"
            gosub error_prompt
            init(" ") sc_dept$, sc_dept_d$
        return

L50080: Rem Enter a Shift Code                      sc_shift$
            if sc_shift$ = " " then sc_shift$ = "AA"
            return

L50090: Rem Enter a Load No. Range               sc_load_fr$, sc_load_to$
            if str(sc_load_fr$,1%,1%) <> " " then goto L50095
               sc_load_fr$ = "ALL  "
               sc_load_to$ = sc_load_fr$

L50095:     if str(sc_load_fr$,1%,3%) <> "ALL" then goto L50098
               sc_load_to$ = "ALL  "
               return
L50098:     if sc_load_to$ = " " then sc_load_to$ = sc_load_fr$
            if sc_load_fr$ > sc_load_to$ then goto L50100 
        return
L50100:     errormsg$ = "'TO' Load No. must be > or = 'FROM' Load No."
            gosub error_prompt
            init(" ") sc_load_fr$, sc_load_to$
        return

L50200: Rem Enter a Barcode Range             sc_barcode_fr$, sc_barcode_to$
            if str(sc_barcode_fr$,1%,1%) <> " " then goto L50205
               sc_barcode_fr$ = "ALL Barcodes      "
               sc_barcode_to$ = sc_barcode_fr$

L50205:     if str(sc_barcode_fr$,1%,3%) <> "ALL" then goto L50210
               sc_barcode_to$ = sc_barcode_fr$
                                                   /* (AWD026)        */
               if userid$ = "WWW" then goto L50215  
               return

L50210:     if len(sc_barcode_to$) < 3 then                         ~
               sc_barcode_to$ = sc_barcode_fr$

            lb_barcode$ = sc_barcode_fr$
            gosub check_barcode
            if barcode% = 0% then goto L50230

            lb_barcode$ = sc_barcode_to$
            gosub check_barcode
            if barcode% = 0% then goto L50230
  
            if sc_barcode_fr$ > sc_barcode_to$ then goto L50220
                                                    /* (AWD026)          */
L50215:
            if userid$ <> "WWW" then return
               if str(sc_barcode_fr$,1%,3%) = "ALL" then goto L50250
               if sc_barcode_to$ <> sc_barcode_fr$  then goto L50250
                                                    /* (AWD026)          */ 
        return
L50220:     errormsg$ = "'TO' Barcode No. must be > or = 'FROM' Barcode No."
            gosub error_prompt
            init(" ") sc_barcode_fr$, sc_barcode_to$
        return
L50230:     errormsg$ = "(Error) Invalid Barcode, or Not on File"
            gosub error_prompt
            init(" ") sc_barcode_fr$, sc_barcode_to$
        return
                                                    /* (AWD026)          */
L50250:     errormsg$ = "(Error) Invalid Barcode, Can Only Print Single Label?"
            gosub error_prompt
            init(" ") sc_barcode_fr$, sc_barcode_to$
        return
        
L50300: Rem Enter a Sequence Range             sc_seq_fr$, sc_seq_to$
            if str(sc_seq_fr$,1%,1%) <> " " then goto L50305
               sc_seq_fr$ = "ALL "
               sc_seq_to$ = sc_seq_fr$

L50305:     if str(sc_seq_fr$,1%,3%) <> "ALL" then goto L50310
               sc_seq_to$ = sc_seq_fr$
        return
        
L50310:     if sc_seq_fr$ > sc_seq_to$  then  goto L50320
            
            convert sc_seq_to$ to scseq%, data goto L50330
            convert sc_seq_fr$ to scseq%, data goto L50330
        return
                                                   
L50320:     errormsg$ = "'TO' Sequence No. must be > or = 'FROM' Sequence No."
            gosub error_prompt
            init(" ") sc_seq_fr$, sc_seq_to$
        return              
 
L50330:     errormsg$ = "Sequence is not numeric"
            gosub error_prompt
            init(" ") sc_seq_fr$, sc_seq_to$
        return              
                                                    
        check_dept
            dept% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = sc_dept$
            read #4,key = readkey$, using L51000, desc$, eod goto L51010
L51000:        FMT POS(25), CH(30)
            dept% = 1%
L51010: return
                                                    /* (EWD002) -     */
        check_barcode
            barcode% = 0%                           /* (PAR001)       */
            init(" ") lb_key1$
            str(lb_key1$,1%,18%) = lb_barcode$
            read #5,key 1% > lb_key1$, using L52000, lb_key1$, eod goto L52010
L52000:        FMT POS(278), CH(23)
            if str(lb_key1$,1%,18%) <> lb_barcode$ then goto L52010
               barcode% = 1%
L52010: return                                      /* (PAR001)       */

/* CR3143 */
        REM *************************************************************~
            *  Print by key (load #) from work file - dept 102 NC       *~
            *************************************************************
        process_inventory_print
		
		   close #12
		   been_here% = 0%
           init(" ") lb_rec$()
           lb_key$ = all(hex(00))
           call "OPENCHCK" (#12, fs%(12%), f2%(12%),500%, rslt$(12%))		   

          work_next_rec
            read #12, key > lb_key$, using L30065, lb2_rec$(),   ~
                                                eod goto L55000
	
	         lb_rec$() = str(lb2_rec$(),6%, 1019%)
             call "EWDPLA71" (been_here%, lb_rec$(), #4, #1, #6, #63,   ~
                          #58, #8, #9, #16, err%)   
             if err% <> 0% then gosub print_error
			 
			 lb_key$ = str(lb2_rec$(),1%,40%)
			 
		  goto work_next_rec
L55000:		
		return
		
/* CR3143 */    
        REM *************************************************************~
            *   Delete and recreate work file after each run            *~
            *************************************************************
	    clear_work_file
            call "FILEBGON" (#12)

            call "OPENCHCK" (#12, fs%(12%), f2%(12%),500%, rslt$(12%))
		return 
	  
		
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error                                    /* (EWD004)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD004)        */
        load_results
           k% = 2%
           hdr$     = "***** Label Generation Results *****"
           msg$(1%) = "This run generated xxxxx label(s)."
           msg$(2%) = "---"
           msg$(3%) = "Press <ENTER> to Acknowledge & Continue"
           convert lbl% to str(msg$(1%),20%,5%), pic(####0)
           if lbl% <> 0% then L64100
               msg$(1%) = "NO LABELS GENERATED!!!"
               str(msg$(3%),,13%) = " Press <PF16>"
L64100:    call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if lbl% <> 0% and k% <>  0% then load_results
           if lbl%  = 0% and k% <> 16% then load_results
           lbl% = 0%
        return  

        print_error
            hdr$     = "***** Label Printing Error *****"
            msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABELS!!!"
            msg$(2%) = "Return Code (EWDPLA71) = "    
            msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

            convert err% to str(msg$(2%),26%,2%), pic(#0)    
L64550:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then return
                if k% <> 16% then goto L64550
            return clear all
            goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

        end

