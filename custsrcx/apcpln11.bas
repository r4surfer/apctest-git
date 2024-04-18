        REM *************************************************************~  
            *                                                           *~
            *  Program Name      - APCPLN11 - Subroutine (APCPL11B)     *~  
            *  Creation Date     - 07/02/96                             *~
            *  Last Modified Date- 10/16/2014                           *~
            *  Written By        - J. Browning Fields                   *~
            *  Last Modified By  - Christie Sanders                     *~
            *                                                           *~
            *  Description       - Utility Program used to Resequence   *~
            *                      Planning File APCPLNDT.  Option to   *~
            *                      resequence an entire day or one      *~
            *                      department.  Append or recreate.     *~
            *                                                           *~
            *  Code Tables Used  - PLAN_DEPT                            *~
            *                                                           *~
            *  Subroutine Used   - APCPLN9B, EXPAND                     *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/15/96 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 03/14/98 ! Y2K                                      ! LDJ *~
            * 05/14/98 ! Added edits to insure data exists for    ! ERN *~
            *          !   beginning and ending production dates. !     *~
            * 10/29/98 ! (EWD001) - Special Fix related to Warranty!RHH *~
            * 12/18/98 ! (EWD002) - Rebuild Sort When Sequencing  ! RHH *~
            *          !    Disable Debug File Update. Only for   !     *~
            *          !    testing. 'expand_sub' Remove          !     *~
            * 12/06/99 ! (EWD003) - Modification to resequence    ! RHH *~
            *          !    whole day.                            !     *~            
            * 05/18/04 ! (EWD004) - Mod so when planning reseq    ! CMG *~
            *          !    for a single department all of the    !     *~   
            *          !    remaining departments sequence no do  !     *~
            *          !    not get reset to zero.                !     *~
            * 05/18/04 ! (EWD005) - Mod for new shapes sort       ! CMG *~
            * 09/01/04 ! (AWD006) - Modification to fix resequence! CMG *~
            *          !     when seq number is part of sort.     !     *~
            *          !     Sort code '3'                        !     *~
            * 03/19/08 ! (AWD007) - mod for casing sort           ! CMG *~
            *10/23/2009! (AWD008) - mod to capture seq data in    ! CMG *~
            *          !           AWDBAYBW                       !     *~
            *02/18/2013! (AWD009) - mod for foam sort             ! CMG *~
            *10/16/2014! (AWD010) - add schema logic              ! CMG *~
            *05/31/2019! (CR2052) - trigger attr and schd for Atla! RDB *~
            *************************************************************

        dim                              /* Record/Screen Variables    */~
            pl_sort$15,sort_key$60,      /* New Sort Index     (EWD002)*/~
            pl_sort1$3,                  /* Expanded Sort Index(EWD002)*/~
            pl_key$11, ld_yr$2, ld_wk$2, /* Use find Sort Code (EWD002)*/~
            ld_load$5,                   /*                    (EWD002)*/~ 
            sq_rec$20,                   /* APCPLNSQ Record            */~
            sq_key$11,                   /* APCPLNSQ Read Key          */~
            readkey$24,                  /* GENCODES Read Key          */~
            dt_key$23,                   /* APCPLNDT Read Key          */~
            dt_key1$57,                  /* APCPLNDT Read Key (Alt 1)  */~
            dt_rec$256,                  /* APCPLNDT Record            */~
            genc$128,                    /* GENCODES Record            */~
            dept$(60%)5,                 /* Department/Process Array   */~
            hdr$40, msg$(3%)79,          /* Shostat Error Messages     */~
            seq$5, seq%(60%),            /* Sequence Number Array      */~
            scr_dte_b$10,                /* Screen Begin  Prod.        */~
            beg_dte$6,                   /* Save Begin  Date           */~
            sav_dte$6,                   /* Save Date for Key          */~
            scr_dept$3,                  /* Screen Department Code     */~
            sav_dept$5,                  /* Save Department Code       */~
            sv_dept$3,                   /* Save Dept for sort (AWD006)*/~
            dept_desc$30,                /* Department Description     */~
            dept_proc$1,                 /* Departments to process     */~
            dept_sort$(60%)15,           /* Department sort Codes      */~
            scr_proc$2,                  /* Screen Process Code        */~
            proc_desc$30,                /* Process Description        */~
            temp_date$10,                /* ShoStat Display Date       */~
            filename$8,                  /* Used by EWD_OPEN   (EWD003)*/~
            reseq$1,                     /* Resequence Select  (A),(R) */~
            reseq_desc$30,               /* Resequence Select Descript.*/~
            specialmull$1                /* (AWD007) casing sort       */

        dim                              /* (Program) - Variables      */~
            desc$30,                     /* Generic Description        */~
            cursor%(2%),                 /* Cursor location for edit   */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            edtmessage$79,               /* Edit Message               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */
            
        dim                              /* (AWD009) */                 ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            so_item$11,                  /* SO & item number           */~
            sav_so_inv$11,               /* Save Saleorder             */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            dt_sub_part$20,              /* New Sub Part No.           */~
            dt_sub_info$20               /* New Sub Info Fields (9)+11 */~
                                         /* (AWD009\)                  */            

        dim schema$8                     /* Schema  (AWD010)           */

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
            apc$   = "(EWD) Planning Assign Sequence Numbers  "
            pname$ = "APCPLN11 - Rev: R7.00"

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
            * #1  ! APCPLNSQ ! (NEW) Planning Sequence No. File         *~
            * #2  ! APCPLNDT ! (NEW) Planning Detail File               *~
            * #3  ! GENCODES ! Master Code Tables File                  *~
            * #4  ! APCPLNUC ! (NEW) Planning Master Units Capacity     *~
            * #5  ! APCPLNLD ! (New) Planning/Scheduling Load Master    *~
            * #6  ! AWDBAYBW ! File to capture DT Data          (AWD008)*~
            * #7  ! BCKSUBPT ! New Sub Part Number File         (AWD009)*~ 
/*CR2052*/  * #26 ! PGSCHDTR ! Ply Gem Schedule Trigger file            *~
/*CR2052*/  * #28 ! PGORATTR ! Ply Gem Attribute Trigger file           *~            
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCPLNSQ",                                      ~
                        varc,     indexed,  recsize =   20,              ~
                        keypos =  1,   keylen = 11

            select #2,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24
                                                    /* (EWD002) Begin   */
            select #4,  "APCPLNUC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =  1,   keylen = 11,                      ~
                        alt key 1, keypos =  7, keylen = 11

            select #5,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen = 5,                       ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15
                                                    /* (EWD002) - End   */
/* (AWD008) */
            select #6, "AWDBAYBW",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   29
/* (AWD009) */
            select #7, "BCKSUBPT",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup                        
 /*CR2052 */
            select #26, "PGSCHDTR",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    21, keylen =   44,                   ~
                        alt key 1, keypos = 1, keylen = 64,              ~
                            key 2, keypos = 54, keylen = 11, dup  
/*CR2052 */
            select #28, "PGORATTR",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    21, keylen =   44,                   ~
                        alt key 1, keypos = 1, keylen = 64,              ~
                            key 2, keypos = 54, keylen = 11, dup  
                       
                        

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
                                                    /* (EWD003)         */
            filename$ = "APCPLNDT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNUC" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
/*(AWD008)*/
            filename$ = "AWDBAYBW" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
/*(AWD009)*/
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error            

/* CR2052*/ filename$ = "PGSCHDTR" : call "EWDOPEN" (#26, filename$, err%)
            if err% <> 0% then gosub open_error

/* CR2052*/ filename$ = "PGORATTR" : call "EWDOPEN" (#28, filename$, err%)
            if err% <> 0% then gosub open_error
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            date$       =   date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor"& ~
                          " to Desired Value & Press (RETURN)."
/* (AWD010) */
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 4%
L10090:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0%    then L10240

L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  = 1%    then gosub startover
                     if keyhit% <> 4%    then L10220

L10160:         fieldnr%         = max(1%, fieldnr% - 1%)
                gosub'051(fieldnr%)
                     if enabled% = 1%    then L10120
                     if fieldnr% = 1%    then L10090

                goto L10160
L10220:         if keyhit%  = 16% and fieldnr% = 1% then exit_program
                if keyhit% <>  0%        then L10120
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10120

            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            lastfieldnr%    = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  = 1%         then gosub startover
                if keyhit%  = 14%        then gosub dataload
                if keyhit%  = 16%        then exit_program
                if keyhit% <> 0%         then editpg1

L11130:     fieldnr%        = cursor%(1%)    - 5%
            if fieldnr%     < 1% or fieldnr% > 4% then editpg1
            if fieldnr%     = lastfieldnr%        then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% = 0%         then editpg1

L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  = 1%         then gosub startover
                if keyhit% <> 0%         then L11190

            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " "      then L11190
                lastfieldnr%  = fieldnr%
            goto L11130

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************
        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *     I N I T I A L I Z E   I N P U T   M E S S A G E S     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************
        deffn'050(scrnr%, fieldnr%)
            if fieldnr%    <> 0%         then L28110
                inpmessage$ = edtmessage$
            return

*        Define the Input Message for the Screen/Field Indicated
L28110:     if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Beginning Production Date?                           ",~
         "Enter a Valid Department?                                    ",~
         "Enter a Valid Process?                                       ",~
         "Enter a Resequence Selection - (A)ppend or (R)esequence?     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, desc$, dept$(),  ~
                      scr_dte_b$, sav_dte$, scr_dept$, sav_dept$, reseq$,~
                      dept_desc$, scr_proc$, proc_desc$, reseq_desc$,    ~
                      dept_proc$, beg_dte$, temp_date$, dept_sort$()
            gosub load_dept

        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************
        startover
            u3%                      =  2%
            call "STARTOVR" (u3%)
            if u3%                   =  1%        then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            pl_sort% = 0%                                   /* (EWD003)  */
            prime%   = 0% 
            update%  = 0% 
            if reseq$ = "R" then gosub clear_seq_nos
            gosub data_prime 
            dt_key1$                 =  all(hex(00))
            str(dt_key1$, 1%, 6%)    =  beg_dte$
        dataload_next
            read #2,key 1% > dt_key1$, using L35070, dt_rec$,             ~
                                                     eod goto end_dataload
            dt_key1$ = str(dt_rec$,47%,57%)       /* Save Alt Key (1)   */
            if prime% = 0% then gosub data_prime

            if str(dt_rec$,47%,6%)  <>  sav_dte$ then goto end_dataload
            if str(dt_rec$,59%,5%)   =  sav_dept$ then goto reseq_check
            if dept_proc$           <> "A"        then goto dataload_next
               sav_dept$             =  str(dt_rec$,59%,5%)
                                                /* (EWD001) - Fix       */
            if str(sav_dept$,4%,2%) <> "01" then                          ~
                                            str(sav_dept$,4%,2%) = "01"
            gosub find_dept                          /* (EWD003)        */
            so_item$ = so_inv$ & item_no$
                         
            if reseq$ = "R" then                                             ~
               call "SHOSTAT" ("Resequence for " & temp_date$ & " " &        ~
                                                  str(sav_dept$,1%,3%) )
            if reseq$ = "A" then                                             ~
               call "SHOSTAT" ("Appending Seq. No's for " & temp_date$ &     ~
                                            " " & str(sav_dept$,1%,3%) )

        reseq_check                                /* (EWD003) - Remove */
                                                   /*   reseq$ test     */
            if str(dt_rec$,111%,5%) <> "00000"    then goto dataload_next
               if pl_sort% = 0% then gosub load_sort_codes
               so_inv$, item_no$ = " "
               so_inv$  = str(dt_rec$,24%,8%)
               item_no$ = str(dt_rec$,32%,2%)               
               if sav_so_inv$ <> so_item$ then gosub lookup_sub_part               
               gosub dataput
/*(AWD002)*/
               gosub awdbaybw_updte
               goto dataload_next

        data_prime
            mat seq% = zer
            if reseq$                = "A"        then gosub load_plnsq
            if dept_proc$            = "A"        then                   ~
                sav_dept$            =  str(dt_rec$,59%,5%)
                                               /* (EWD001) - Fix       */
            if str(sav_dept$,4%,2%) <> "01" then                         ~
                                            str(sav_dept$,4%,2%) = "01"
            gosub find_dept                      /* (EWD003)           */
            prime% = 1% 
            if reseq$ = "R" then                                             ~
               call "SHOSTAT" ("Resequence for " & temp_date$ & " " &        ~
                                                  str(sav_dept$,1%,3%) )
            if reseq$ = "A" then                                             ~
               call "SHOSTAT" ("Appending Seq. No's for " & temp_date$ &     ~
                                            " " & str(sav_dept$,1%,3%) )
            so_inv$, item_no$ = " "
            so_inv$  = str(dt_rec$,24%,8%)
            item_no$ = str(dt_rec$,33%,2%)                                            
            gosub lookup_sub_part
        return

        end_dataload
        if update% = 1% then gosub update_plnsq
        return clear all
        goto inputmode

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            seq%(dept%) = seq%(dept%) + 1%             /* (EWD003)     */

            dt_key$                  =  str(dt_rec$,24%,23%)
            read   #2,hold,key = dt_key$, using L35070, dt_rec$,         ~
                                                        eod goto L31215
                delete #2 
                                             /* (EWD002) - Begin       */   
            convert seq%(dept%) to str(dt_rec$,111%,5%), pic(00000)

                                             /* (EWD005)For Spec Shapes*/

              pl_sort$ = dept_sort$(dept%)
/* (AWD010) */
              if schema% = 1% and str(sav_dept$,1%,3%) <> "043" then goto sort_all
              if schema% = 2% and (str(sav_dept$,1%,3%) <> "002" and ~
                                str(sav_dept$,1%,3%) <> "003") then goto sort_all

                 specialmull$ = " "          /* (AWD007) */
                 call "AWDPLN9B" (dt_rec$,   /* (APCPLNDT) Record      */~
                                  sort_key$, /* Output Index Built     */~
                                  specialmull$, /* Not used here (AWD007) */~
                                  #3 )       /* (GENCODES)             */
                 goto sort_shapes

sort_all:
                                             /*  (EWD005)  - For Shapes*/
        REM - Build Index (DT_INDEX$)
REM           pl_sort$ = dept_sort$(dept%)
              call "APCPLN9B" ( "0",         /* Set Flag for Data File */~
                                pl_sort$,    /* Sort Code From APCPLNUC*/~
                                dt_rec$,     /* (APCPLNDT) Record      */~
                                bcksubpt_rec$, /*(BCKSUBPT)Rec (AWD009)*/~
                                sort_key$,   /* Output Index Built     */~
                                #3 )         /* (GENCODES)             */

sort_shapes:                                   /*  (EWD005) - For Shapes */
              str(dt_rec$,66%,30%) = str(sort_key$,1%,30%)
                                             
            write #2, using L35070, dt_rec$, eod goto L31210
            update% = 1%
/* CR2052 */
            if schema% = 2% then gosub load_pg_trigger
            if schema% = 2% then gosub load_pg_attr_trigger
            
        return
L31210:     errormsg$ = "Error - Dept = ("& sav_dept$&") Sales Order = (" ~
                           & str(dt_key$,1%,8%) & "-" & str(dt_key$,9%,2%)~
                           & ")"
            gosub error_prompt
        return
L31215:     errormsg$ = "Error - Sales Order = ("                         ~
                           & str(dt_key$,1%,8%) & "-" & str(dt_key$,9%,2%)~
                           & ")"
            gosub error_prompt
        return

        update_plnsq
            call "SHOSTAT" ("Updating PLNSQ for " & temp_date$)
            for i%                   =  1% to dept_max%
                                                      /*  (EWD004) - Begin */

                if scr_dept$ = "ALL" then goto update_all
                   if str(dept$(i%),1%,3%) <> scr_dept$ then goto L31410
update_all:
                                                      /*  (EWD004) - END   */
                init(" ") sq_key$, sq_rec$
                str(sq_key$, 1%,6%)  =  sav_dte$
                str(sq_key$, 7%,5%)  =  dept$(i%)
                read   #1,hold,key = sq_key$, eod goto L31340
                   delete #1
L31340:         str(sq_rec$, 1%,6%)  =  sav_dte$
                str(sq_rec$, 7%,5%)  =  dept$(i%)
                str(sq_rec$,12%,2%)  = "01"
                convert seq%(i%) to str(sq_rec$,14%,5%), pic(00000)
                str(sq_rec$,19%,2%)  = "  "
                write  #1, using L35040, sq_rec$,  eod goto L31410
L31410:     next i%
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* (APCPLNSQ) - FILE          */
L35040:     FMT CH(20)                   /* Sequence Record      - #1  */

                                         /* (APCPLNDT) - FILE          */
L35070:     FMT CH(256)                  /* Plan Detail Record   - #2  */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40060:     gosub'050(1%, fieldnr%)
            gosub set_pf1
                if fieldnr% > 0% then init(hex(8c)) lfac$()              ~
                                 else init(hex(86)) lfac$()

            on fieldnr% gosub L40180,      /* Begin  Prod. Date      */   ~
                              L40180,      /* Department             */   ~
                              L40180,      /* Process                */   ~
                              L40180       /* Append or Resequence   */
            goto L40210
                lfac$(fieldnr%) = hex(80) : return  /* Up / Low   */
L40180:         lfac$(fieldnr%) = hex(81) : return  /* Upper Only */
                lfac$(fieldnr%) = hex(82) : return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
                at (06,02), "Begin Production Date:"            ,        ~
                at (06,25), fac(lfac$(1%)), scr_dte_b$          , ch(10),~
                                                                         ~
                at (07,02), "Department Code      :"            ,        ~
                at (07,25), fac(lfac$(2%)), scr_dept$           , ch(03),~
                at (07,35), fac(hex(84)),   dept_desc$          , ch(30),~
                                                                         ~
                at (08,02), "Process    Code      :"            ,        ~
                at (08,25), fac(lfac$(3%)), scr_proc$           , ch(02),~
                at (08,35), fac(hex(84)),   proc_desc$          , ch(30),~
                                                                         ~
                at (09,02), "(A)ppend/(R)esequence:"            ,        ~
                at (09,25), fac(lfac$(4%)), reseq$              , ch(01),~
                at (09,35), fac(hex(84)),   reseq_desc$         , ch(30),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15 then goto L40570
                     call "PRNTSCRN"
                     goto L40060
L40570:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
        if edit%    = 2%      then L40760     /*  Input Mode            */
            pf$(1)  = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2)  = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3)  = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1%  then L40720
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%)= hex(ff)
L40720:     if fieldnr% > 1%  then L40740
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40740:     return

L40760: if fieldnr% > 0%      then L40850  /*  Edit Mode - Select Fld */
            pf$(1)  = "(1)Start Over                           " &       ~
                      "                       (14)Resequence  "
            pf$(2)  = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3)  = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40850:                                  /*  Edit Mode - Enabled    */
            pf$(1)  = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2)  = "                                        " &       ~
                      "                                       "
            pf$(3)  = "                                        " &       ~
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
            on fieldnr% gosub L50140,           /* Begin  Prod. Date */   ~
                              L50430,           /* Department Code   */   ~
                              L50670,           /* Process    Code   */   ~
                              L50810            /* Append or Reseq.  */
        return

L50140: REM Verify Begin Production Date           SCR_DTE_B$
            if scr_dte_b$         > " "   then L50170
            scr_dte_b$ = date 
            call "DATFMTC" (scr_dte_b$)   
       
L50170:     date%                 = 0%
            call "DATEOKC"   (scr_dte_b$, date%, errormsg$)
            if date%              = 0%    then L50250
            call "DATUFMTC" (scr_dte_b$)
            beg_dte$              = str(scr_dte_b$,1%,6%)
            sav_dte$              = beg_dte$
            str(temp_date$,1%,6%) = sav_dte$
            call "DATFMTC" (temp_date$)
            call "DATFMTC"  (scr_dte_b$)     

            dt_key1$                 =  all(hex(00))
            str(dt_key1$, 1%, 6%)    =  beg_dte$
            read #2, key 1% > dt_key1$, using L35070, dt_rec$, eod goto L50220
            if str(dt_rec$,47,6) = beg_dte$ then return
L50220:         errormsg$ = "(Error) - No records for BEGIN Prod. Date " & scr_dte_b$
                goto L50260

L50250:     errormsg$ = "(Error) - Invalid BEGIN Prod. Date?"
L50260:     gosub error_prompt
            init(" ") scr_dte_b$, beg_dte$, sav_dte$, temp_date$
        return

L50430: REM Verify Department Code                 SCR_DEPT$
            init(" ") dept_proc$
            if scr_dept$          = " "   then scr_dept$ = "ALL"
            if scr_dept$         <> "ALL" then L50500
                dept_desc$        = "All Departments"
                dept_proc$        = "A"
                return

L50500:     convert scr_dept$ to scr_dept%, data goto L50630

            convert scr_dept% to scr_dept$, pic(000)

            readkey$              = " "
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) =  scr_dept$
            read #3,key = readkey$, using L50580, desc$, eod goto L50630
L50580:         FMT POS(25), CH(30)

            dept_desc$            =  str(desc$,1%,30%)
            sav_dept$             =  scr_dept$ & "01"
            dept_proc$            = " " 
        return
L50630:     errormsg$ = "(Error) - Invalid DEPARTMENT Lookup ?"
            init(" ") dept_desc$, scr_dept$, sav_dept$, dept_proc$
        return

L50670: REM Verify Process Code                    SCR_PROC$
            scr_proc$             = "01"
            readkey$              = " "
            str(readkey$,1%,9%)   = "PLAN PROC"
            str(readkey$,10%,15%) =  scr_proc$
            read #3,key = readkey$, using L50730, desc$, eod goto L50770
L50730:         FMT POS(25), CH(30)

            proc_desc$            =  str(desc$,1%,30%)
        return
L50770:     errormsg$ = "(Error) - Invalid PROCESS Lookup ?"
            init(" ") proc_desc$, scr_proc$
        return

L50810: REM Verify Resequence Selection            RESEQ$
            if reseq$             = " "   then reseq$ = "A"
            if reseq$             = "R"   then                           ~
               reseq_desc$        = "Resequence Department(s)"
            if reseq$             = "A"   then                           ~
               reseq_desc$        = "Append Department(s) Sequence"
            if reseq$             = "R"   then return
            if reseq$             = "A"   then return
            errormsg$ = "(Error) - Invalid RESEQUENCE Selection?"
            init(" ") reseq_desc$, reseq$
        return

        REM *************************************************************~
            *               S P E C I A L   R O U T I N E S             *~
            *-----------------------------------------------------------*~
            * Subroutines for Edit or Update Processes                  *~
            *************************************************************

        load_dept
            call "SHOSTAT" ("Loading Department Array")
            i%                       =  0%
            readkey$                 =  all(hex(20))
            str(readkey$,1%,9%)      = "PLAN DEPT"
        dept_next
            read #3,key > readkey$, using L60130, readkey$,               ~
                                                        eod goto dept_max
L60130:        FMT CH(24)
            if str(readkey$,1%,9%)  <>  "PLAN DEPT" then goto dept_max
            if str(readkey$,10%,3%)  = "100"        then goto dept_next
            if str(genc$,10%,3%)     > "104"        then goto dept_next
            i%                       =  i% + 1% 
            dept$(i%)                =  str(readkey$,10%,3%) & "01"
            goto dept_next
        dept_max
            dept_max%                =  i%
        return

        find_dept                                       /* (EWD003)      */
            dept% = 0%
            for k%                   =  1% to dept_max%
                if dept$(k%)        <>  sav_dept$ then find_dept_next
                   goto find_dept_done
        find_dept_next
            next k%
        find_dept_done
            dept% = k%
        return

        load_plnsq
            call "SHOSTAT" ("Loading PLNSQ for " & temp_date$)
            for i%                   =  1% to dept_max%
                str(sq_key$, 1%,6%)  =  beg_dte$
                str(sq_key$, 7%,5%)  =  dept$(i%)
                read #1,key = sq_key$, using L60310, seq$, eod goto L60350
L60310:            FMT POS(14), CH(5)
                convert seq$ to seq%(i%), data goto L60350

L60350:     next i%
        return

        clear_seq_nos                                 /* (EWD003)  */
            call "SHOSTAT" ("Clearing Seq. No's for " & temp_date$ & ~
                                          " " & str(sav_dept$,1%,3%))
            init(" ") sv_dept$                        /* (AWD006)  */
            dt_key1$                 =  all(hex(00))
            str(dt_key1$, 1%, 6%)    =  beg_dte$
        clear_seq_next
            read #2,hold,key 1% > dt_key1$, using L35070, dt_rec$,   ~
                                              eod goto clear_seq_done
            dt_key1$ = str(dt_rec$,47%,57%)      /* Set Alt Key 1  */
            if str(dt_rec$,47%,6%) <> beg_dte$ then goto clear_seq_done
            if dept_proc$ = "A" then goto L60500
               if str(sav_dept$,1%,3%) <> str(dt_rec$,59%,3%) then   ~
                                                  goto clear_seq_next

L60500:           str(dt_rec$,111%,5%) = "00000"
                  gosub check_sort                    /* (AWD006)   */
    
                  delete #2                           /* (AWD006)   */
                  put #2, using L35070, dt_rec$       /* (AWD006)   */
                  write #2, eod goto L61000           /* (AWD006)   */

REM                  put #2, using L35070, dt_rec$    /* (AWD006)   */
REM                  rewrite #2, eod goto L61000      /* (AWD006)   */
                  goto clear_seq_next
        clear_seq_done
        return
L61000:    errormsg$ = "(Error)-Unable to clear Seq No. for " & dt_key1$
           gosub error_prompt
           goto clear_seq_next                    /* (EWD003)          */

        load_sort_codes                           /* (EWD003)          */
            init(" ") ld_load$, ld_yr$, ld_wk$, pl_key$, pl_sort$,       ~
                      dept_sort$(), pl_sort$
            ld_load$ = str(dt_rec$,1%,5%)
            read #5,key = ld_load$, using L61100, ld_Yr$, ld_wk$,        ~
                                                  eod goto L61130
L61100:        FMT POS(104), 2*CH(2)
            for i% = 1% to dept_max% 
                init(" ") pl_key$
                str(pl_key$,1%,2%) = ld_yr$            /* Prod Year     */
                str(pl_key$,3%,2%) = ld_wk$            /* Prod Week     */
                str(pl_key$,5%,2%) = "01"              /* Shift Code    */
                str(pl_key$,7%,5%) = dept$(i%)         /* Dept and Proc */
                read #4,key = pl_key$, using L61110, str(pl_sort$,1%,12%),~
                                              pl_sort1$, eod goto L61120 
L61110:             FMT POS(18), CH(12), POS(123), CH(3)
                str(pl_sort$,13%,3%) = pl_sort1$
                dept_sort$(i%) = pl_sort$
L61120:     next i%
            pl_sort% = 1%
L61130: return
                                                  /* (EWD003) - end    */   

                                                      /*  (AWD006) - BEG */
        check_sort
            if sv_dept$ = str(dt_rec$,59%,3%) then goto already_sort
            init(" ") ld_load$, ld_yr$, ld_wk$, pl_key$, pl_sort$,       ~
                      dept_sort$(), pl_sort$
            length% = 0%
            ld_load$ = str(dt_rec$,1%,5%)
            read #5,key = ld_load$, using L61100, ld_yr$, ld_wk$,        ~
                                                  eod goto no_sort

                init(" ") pl_key$
                str(pl_key$,1%,2%) = ld_yr$            /* Prod Year     */
                str(pl_key$,3%,2%) = ld_wk$            /* Prod Week     */
                str(pl_key$,5%,2%) = "01"              /* Shift Code    */
                str(pl_key$,7%,5%) = str(dt_rec$,59%,5%)/* Dept and Proc */
                read #4,key = pl_key$, using L61110, str(pl_sort$,1%,12%),~
                                              pl_sort1$, eod goto L61120 

                str(pl_sort$,13%,3%) = pl_sort1$
                goto already_sort


        no_sort
        return
        already_sort
             for sort% = 1% to 15%
                 gosub lookup_sort_length
                 if str(pl_sort$,sort%,1%) <> "3" then goto L61300
                    goto clear_dt_seq
L61300:      next sort%
        return
        clear_dt_seq
                            /* Add 66 to get to correct postion in DT record */
             str(dt_rec$,(length% + 66%), 5%) = "00000"   

        return

        lookup_sort_length
            if str(pl_sort$,sort%,1%) = "3" then return
            readkey$, desc$       = " "
            sort_len%             = 0%
            str(readkey$,1%,9%)   = "PLAN SORT"
            str(readkey$,10%,15%) =  str(pl_sort$,sort%,1%)
            read #3,key = readkey$, using L50580, desc$, eod goto not_length


            convert str(desc$,16%,1%) to sort_len%, data goto not_length

            length% = length% + sort_len%
        not_length
        return

                                                      /*  (AWD006) - END */
        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error                                    /* (EWD003)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
/*(AWD008)*/
        awdbaybw_updte
           call "AWDCOMSB" (dt_rec$,#6," "," ")
        return
/*(AWD009) */        
        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$(),           ~
                      dt_sub_part$, dt_sub_info$
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1"
            err1% = 0%

            convert so_inv$ to so_inv%, data goto convert_alpha

            convert so_inv% to so_inv$, pic(00000000)

            goto order_converted

convert_alpha:
            convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)

order_converted:
            convert item_no$ to item_no%, data goto sub_part2
sub_part2:
            convert item_no% to item_no$, pic(###)
            
            sav_so_inv$ = str(dt_rec$,24%,11%)

        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #7,            /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */

            if err1% <> 0% then str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
            if err1% <> 0% then str(bcksubpt_rec$,132%,9%) = "         "

               dt_sub_part$ = str(bcksubpt_rec$,48%,20%)

               dt_sub_info$ = str(bcksubpt_rec$,132%,20%)

               if err1% = 0% then return

            return        
/*(AWD009\) */    
   
/************************************************************************/
/* Load the Schedule Header trigger for ATLas extract later.  Only 1    */
/* record in the trigger file with 0 transmit status needed.  CR2052    */
/************************************************************************/
        load_pg_trigger
            init(" ") filetype$, transmit$, filetype2$, salesorder$, linenbr$, ~
                      pgdate$, time$, upddte$, updtime$, filler1$            
                                 
            str(tr_key$,1%,8%) = str(dt_key$,1%,8%)
            str(tr_key$,9%,2%) = str(dt_key$,9%,2%)

            read #26, key 2% >= tr_key$,   ~
                   using L62700, filetype$, transmit$, pgdate$, time$,       ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,      eod goto new_trigger

              goto trigger_process
               
            nxt_trigger   
               read #26, using L62700, filetype$, transmit$, pgdate$, time$, ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,      eod goto new_trigger
trigger_process:                     
                  if salesorder$ <> str(dt_key$,1%,8%) or      ~
                     linenbr$ <> str(dt_key$,9%,2%)  then goto new_trigger
                     
                  if transmit$ = "0"  then  goto trigger_done
               goto nxt_trigger      
                  
            new_trigger
               filetype$ = "SCHDHDR"
               transmit$ = "0"
               str(pgdate$,1%,6%) = date
               time$ = time
               filetype2$ = "SCHDHDR"
               salesorder$ = str(dt_key$,1%,8%)
               linenbr$  = str(dt_key$,9%,2%)
               upddte$ = " "
               updtime$ = " "
               str(filler1$,1%,2%) = "04"
               str(filler1$,3%,10%) = "APCPLN11"
               put #26, using L62700, filetype$, transmit$, pgdate$, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,      ~
                            updtime$, filler1$
            
L62700:           FMT CH(20), CH(1), CH(6), CH(6), CH(20), CH(8), ~
                           CH(3), CH(6), CH(6), CH(180)
                           
               write #26
               
        trigger_done
        return
        
/************************************************************************/
/* Load the Order Attributes trigger for ATLas extract later.  Only 1   */
/* record in the trigger file with 0 transmit status needed.  CR2052    */
/************************************************************************/
        load_pg_attr_trigger
            init(" ") filetype$, transmit$, filetype2$, salesorder$, linenbr$, ~
                      pgdate$, time$, upddte$, updtime$, filler1$    
            
            str(tr_key$,1%,8%) = str(dt_key$,1%,8%)
            str(tr_key$,9%,2%) = str(dt_key$,9%,2%)

            read #28, key 2% >= tr_key$,   ~
                   using L62710, filetype$, transmit$, pgdate$, time$,       ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,   eod goto new_attr_trigger

              goto trigger_attr_process
               
            nxt_attr_trigger   
               read #28, using L62710, filetype$, transmit$, pgdate$, time$, ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,   eod goto new_attr_trigger
trigger_attr_process:                     
                  if salesorder$ <> str(dt_key$,1%,8%) or      ~
                     linenbr$ <> str(dt_key$,9%,2%)  then goto new_attr_trigger
                     
                  if transmit$ = "0"  then  goto trigger_attr_done
               goto nxt_attr_trigger      
                  
            new_attr_trigger
               filetype$ = "ORDERATTR"
               transmit$ = "0"
               str(pgdate$,1%,6%) = date
               time$ = time
               filetype2$ = "ORDERATTR"
               salesorder$ = str(dt_key$,1%,8%)
               linenbr$  = str(dt_key$,9%,2%)
               upddte$ = " "
               updtime$ = " "
               str(filler1$,1%,2%) = "04"
               str(filler1$,3%,10%) = "APCPLN11"
               put #28, using L62710, filetype$, transmit$, pgdate$, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,      ~
                            updtime$, filler1$
            
L62710:           FMT CH(20), CH(1), CH(6), CH(6), CH(20), CH(8), ~
                           CH(3), CH(6), CH(6), CH(180)
                           
               write #28
               
        trigger_attr_done
        return  
        
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_program
            call "SHOSTAT" ("One Moment Please")

            end


