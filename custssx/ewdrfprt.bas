        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDRFPRT                             *~
            *  Creation Date     - 10/16/2020                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Ricky Beane                          *~
            *                                                           *~
            *  Description       - RFID Tag Printing Test               *~
            *                                                           *~            
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDRFPRT - Print the barcode within the RFID tag as well  *~
            *            as readable and scanable barcode. This is only *~
            *            for Beta Testing.                              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN-----+----------------WHAT--------------------+-WHO-*~
            * 10/16/2020 ! Original - New Program                 ! RDB *~
            * 02/26/2021 ! CR2782 Heading jsn, seq# moved         ! RDB *~
            *************************************************************
        sub "EWDRFPRT" (lb_barcode$,         /* Barcode to print           */~
                        lb_dept$,            /* Department number          */~
                        #1,                  /* GENCODES                   */~
                        #2,                  /* EWDPRDLB                   */~
                        error_return%)
                         
        dim                                                              ~
            a$72, b$72,                  /* Print Lines for Label      */~
            lbl$(10%)60,                 /* Label Data Array           */~
            xx$(70%)80,                  /* Label Buffer               */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim                                                              ~
            lb_key$35, lb_key1$23,       /* Record Key from LB         */~
            lb_rec$(4%)256,              /* LB Record Data     (PAR001)*/~
            lb_prddate$10,               /* Prod. Date from LB         */~
            lb_dept$3,                   /* Dept. No. from LB          */~
            lb_shift$2,                  /* Shift Code from LB         */~
            lb_load$5,                   /* Load Number from LB        */~
            lb_barcode$18,               /* Production Barcode         */~
            lb_seq$5,                    /* Sequence Number            */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            userid$3,                    /* Current User Id            */~
            count$22,                    /* Display                    */~ 
            hold$5,                      /* Testing                    */~
            testdate$10,                 /* Printable production date  */~
            schema$8                     /* Schema Switch              */
            
        dim f2%(2%),                     /* = 0 if the file is open    */~
            f1%(2%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */
 
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD)Generate RFID Barcode Labels    "
            pname$ = "EWDRFPRT - Rev: R1.00"

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
            * #2  ! EWDPRDLB ! Production Label Data File               *~
            *************************************************************~
            * #5  ! LBLRFID1 ! Print File For RFID Labels               *~
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
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            fs$ = "^FS"

            xx$(1%)  = "^JO"
            xx$(2%)  = "^XA"
            xx$(3%)  = "^RS,,,3,N,,,2"
            xx$(4%)  = "^RR3"
            xx$(5%)  = "^XZ"
            xx$(6%)  = "^XA"
            xx$(7%)  = "^SZ2^JMA"
            xx$(8%)  = "^MCY^PMN"
            xx$(9%)  = "^PW715~JSN"
REM            xx$(10%) = "^JZY"     /* CR2782 */
            xx$(10%) = "^JZY"
            xx$(11%) = "^LH0,0^LRN"
            xx$(12%) = "^XZ"

            xx$(13%) = "^XA"
            xx$(14%) = "^FO156,35"
            xx$(15%) = "01^BY2^BCI,89,N,N^FR^FD"

            xx$(16%) = "01^FT120,150^CIO^AON,24,27^FR^FD"
            xx$(17%) = "^RFW,H,1,2,1^FD3400^FS"     /* GED code */
            xx$(18%) = "05^RFW,H,2,12,1^FR^FD"                                    
            xx$(19%) = "02^FT108,178^A0N,31,24^FR^FD"     /* CR2782 */
            xx$(20%) = "03^FT590,178^A0N,31,24^FR^FD"     /* CR2782 */
            xx$(21%) = "04^FT349,181^A0N,34,43^FR^FD"     /* CR2782 */
               
            xx$(22%) = "^PQ1,0,1,Y"
            xx$(23%) = "^XZ"
            xx$(24%) = " "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        REM INPUTMODE

            gosub set_file_destination   
                if error% <> 0%             then exit_pgm
            gosub open_file
            gosub initialize_variables
            gosub dataload
            goto  exit_pgm

        set_file_destination 
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

SS_1:                                               
                                                    
                                                    
            if schema% <> 1% then goto SS_2
                                                     /* North Caolina   */
               file$    = "LBLRFID1"
               script$  = "NCWRFID1"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
                                                     /* Texas           */
SS_2:
               file$    = "LBLRFIDZ"
               script$  = "NTXRFIDZ"

               library$ = "NEDATA  "
               volume$  = "NE    "

SS_3:
        return                                     

        error_prompt
           error_return% = error%
        return

        open_file
            open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L01100
                  call "FILEBGON" (#5)

L01100:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return
      
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables

            init(" ") lb_rec$()
            lb_key1$ = all(hex(00))
            lbl% = 0%
 
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            call "SHOSTAT" ("Printing RFID Tag Labels...")

            lb_key1$ = all(hex(00))
            str(lb_key1$,1%,18%) = lb_barcode$
            str(lb_key1$,19%,3%) = "   "
            str(lb_key1$,22%,2%) = "  "
            
/* CR2782 Change key read for support departments */                                     
            read #2, key 1% > lb_key1$, using L35040, lb_rec$(),             ~
                                                       eod goto load_done
L35040:     FMT 4*CH(256)                   /* Label Data     EWDPRDLB */

            lb_key$     = str(lb_rec$(),1%,35%)
            
/* CR2782 */            
            if lb_barcode$ <> str(lb_rec$(),278%,18%) then load_done  
                                                            
            lb_prddate$ = str(lb_key$,1%,6%)
            lb_dept$    = str(lb_key$,12%,3%)
            lb_shift$   = str(lb_key$,15%,2%)
            lb_load$    = str(lb_key$,22%,5%)
            lb_seq$     = str(lb_rec$(),311%,5%)     
                                                           
            lb_barcode$ = str(lb_rec$(),278%,18%)

            gosub format_barcode_label

            lbl% = lbl% + 1%

            no_label% = no_label% - 1%

            convert lbl% to str(count$,17%,5%), pic(#####)

            call "SHOSTAT" (count$)

        load_done

        return 
        

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        format_barcode_label                       /* Format            */
            init (" ") lbl$()
            nbr_lines% = 0%
            lbl$(1%) = lb_barcode$  & fs$          /* Barcode No.       */
            testdate$ =  lb_prddate$
            call "DATFMTC" (testdate$)
            lbl$(2%) = testdate$    & fs$          /* Production Date   */
            lbl$(3%) = lb_dept$     & fs$          /* Department        */
            lbl$(4%) = lb_seq$      & fs$          /* Sequence Number   */
            lbl$(5%) = "000000" & lbl$(1%)         /* Set tag with leading 0 */
            
    read_loop
        a$ = " "
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        hold$ = str(a$,1%,5%)

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
        if a$ = " " then return       /* Last Line */
        goto read_loop

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


        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_pgm
            counter%       = 0% : lb1% = 0% : lb2% = 0%
            if error%      = 1% then end
            close #5
                if error% <> 0% then delete_lbl

            call "LINK" addr(script$, lb1%, lb2%)
                if lb1%    > 0% then error% = 4%

        delete_lbl
            call "FILEBGON" (#5)          /* Scratch Lbl File        */
                   
        end
