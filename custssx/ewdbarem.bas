        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDBAREM                             *~
            *  Creation Date     - 03/17/00                             *~
            *  Last Modified Date- 03/17/00                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Generate Employee Barcode Labels     *~
            *                                                           *~
            *               Note - PLAN LB03 is used for Production     *~
            *                      Glass and Employee labels            *~               
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDBAREM - Generates the label format and data to print   *~
            *            Employee Barcode Labels. The resulting         *~
            *            file is routed to the label printer via a      *~
            *            script file.                                   *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                                                           *~
            * Subroutine - Called by APCEMPMN                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/17/00 ! Original - New Program                   ! RHH *~
            * 07/12/06 ! Add NE printing                AWD001    ! DES *~
            * 07/27/15 ! SR67069 Created 2 new scripts- EWDGPS00, ! PWW *~
            *          ! NTXGPS00. New Filename is LBLGLP00.      !     *~
            *************************************************************

        sub "EWDBAREM" (empno$,          /* Employee Number            */~
                        e_lname$,        /* Employee Last Name         */~
                        e_fname$,        /* Employee First Name        */~
                        e_init$,         /* Employee Middle initial    */~
                        #1,              /* Gencodes                   */~ 
                        error%)          /* Return Code                */

        dim                                                              ~
            empno$5,                     /* Employee Number            */~
            e_lname$15,                  /* Employee Last Name         */~
            e_fname$15,                  /* employee First Name        */~
            e_init$1,                    /* Employee Middle Initial    */~   
            a$72, b$72,                  /* Print Lines for Label      */~
            desc$64,                     /* Descriptions for Scripts   */~
            lbl$(10%)60,                 /* Label Data Array           */~
            xx$(20%)80,                  /* Label Buffer               */~
            lbl_key$24,                  /* Save File Key              */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */


        dim f2%(2%),                     /* = 0 if the file is open    */~
            f1%(2%)                      /* = 1 if READ was successful */
 
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD)Generate Employee Barcode Labels    "
            pname$ = "EWDBAREM - Rev: R7.00"

        REM *************************************************************
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Master Code Table File                   *~
            *************************************************************~
            * #5  ! LBLGLR?? ! Print File For Glass Remake Labels       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "ROYAL", varc, consec, recsize =   72

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            error%          =  0%  :  counter% = 0%  :  nbr_lines% = 0%
            library$        = "APCDATA "
            volume$         = "CARLOS"
            fs$ = "^FS"

            xx$(1%) = "^JO"
            xx$(2%) = "^XA^EG^XZ"
            xx$(3%) = "^XA"
            xx$(4%) = "^PMN"
            xx$(5%) = "^MNY"
            xx$(6%) = "^MMR"
            xx$(7%) = "^MTT"
            xx$(8%) = "^MD0"
            xx$(9%) = "^LH0,0"
            xx$(10%) = "^LL203"
            xx$(11%)= "^PR4"
            xx$(12%)= "^JMA"

            xx$(13%)= "01^FO28,35^CI0^A0N,59,124^FR^FD"
            xx$(14%)= "02^FO37,100^CI0^A0N,24,24^FR^FD"
            xx$(15%)= "03^FO224,100^CI0^A0N,24,24^FR^FD"
            xx$(16%)= "04^FO354,100^CI0^A0N,24,24^FR^FD"
            xx$(17%)= "01^FO355,39^BY3,2.5,71^BCN,71,N,N,N^FR^FD>:"
            xx$(18%)= "^PQ1"
            xx$(19%)= "^XZ"


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        REM INPUTMODE
/*SR67069   gosub set_file_name                                    */
/*SR67069*/ gosub set_file_destination          /* AWD001          */
                if error% <> 0%               then exit_sub
            gosub open_file
            gosub begin_process

            goto exit_sub

        set_file_name
            init(" ") lbl_key$, desc$, file$, script$
            str(lbl_key$,1%,9%)   = "PLAN LB03"
            str(lbl_key$,10%,15%) = "LBLGLP"

        check_next_file
            read #1,hold,key > lbl_key$, using L10210, lbl_key$, desc$,   ~
                eod goto check_next_done
L10210:         FMT CH(24), CH(30)
            if str(lbl_key$,1%,15%) <> "PLAN LB03LBLGLP" then            ~
                    goto check_next_done
            if str(desc$,1%,1%) <> "-"                   then            ~
                    goto check_next_file
            put #1, using L10280, "*"
            rewrite #1
L10280:         FMT POS(25), CH(1)

            file$       = str(lbl_key$,10%,8%)  /* Print Filename  */
                                                /* LBLGLR??        */
            script$     = str(desc$,3%,8%)      /* Script Program  */
                                                /* Name EWDGRS??   */ 
            gosub set_file_destination          /* AWD001          */

        return

        check_next_done
            counter%    = counter% + 1%
            if counter% < 4% then goto set_file_name
            error%      = 1%                       /* EXIT TRY LATER */
        return

        set_file_destination                    /* (RHHTEST)       */
            err%    = 0%
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #1,           /* GENCODES                   */~
                           err% )        /* error                      */

            if err% = 0% then goto SS_1
               errormsg$ = "(Error) Schema Lookup Error)"
               gosub error_prompt
               error% = 9%
               end

SS_1:                                                /* (PAR002)        */
                                                     /* (EWD014)        */
                                                     /* (RHHTEST)       */
            if schema% <> 1% then goto SS_2
                                                     /* North Caolina   */
/*SR67069*/    file$    = "LBLGLP00"
/*SR67069*/    script$  = "EWDGPS00"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
                                                     /* North East      */
SS_2:
/*SR67069*/    file$    = "LBLGLP00"
/*SR67069*/    script$  = "NTXGPS00"

               library$ = "NEDATA  "
               volume$  = "NE    "

SS_3:
        return                                    /* (PAR002)        */

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_file
            open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process
            init (" ") lbl$()
            gosub format_employee_label
 
    read_loop
        a$ = " "
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)

        if a$ = " " then return
        a_len% = len(a$)                    /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$   */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                            /* Look for a Field Number    */ 
        l_len% = len(lbl$(ln%))             /* Find Length of Data Element*/
                                            /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%     /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */    
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),1%,l_len%)

      skip_data
        if nbr_lines% = 1% then b$ = bin(126,1) & b$
        gosub print_line
        if a$ = "^XZ" then return       /* Last Line */
        goto read_loop


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

        format_employee_label                    /* Format             */
            lbl$(1%) = empno$       & fs$        /* Employee No.       */
  
            lbl$(2%) = e_lname$     & fs$        /* Employee Last Name */

            lbl$(3%) = e_fname$     & fs$        /* Employee First Name*/
                                         
            lbl$(4%) = e_init$      & fs$        /* Middle Initial     */
 
        return
    
        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(72)

        REM *************************************************************~
            *           S P E C I A L   R O U T I N E S                 *~
            *************************************************************

        print_line
            write #5, using L55030, b$, eod goto L61550
        return

L61550:     error% = 5%
        return clear all

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub
            counter%       = 0% : lb1% = 0% : lb2% = 0%
            if error%      = 1% then end
            close #5
                if error% <> 0% then delete_lbl

            call "LINK" addr(script$, lb1%, lb2%)
                if lb1%    > 0% then error% = 4%

        delete_lbl
            call "FILEBGON" (#5)          /* Scratch Lbl File        */
/*SR67069*/ goto delete_lbl_end
L65140:     read    #1,hold,key = lbl_key$, eod goto L65190
            put     #1, using L10280, "-"
            rewrite #1
        delete_lbl_end                    /*SR67069 */
        end
L65190:     counter%       = counter% + 1%
            if counter%    < 4% then L65140
            error%         = 6%
        end

