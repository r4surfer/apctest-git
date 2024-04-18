        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - AWDPLB05                             *~
            *  Creation Date     - 06/09/04                             *~
            *  Last Modified Date- 07/28/2014                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modifications By  - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Create New Label for Wood Surrround. *~
            *                                                           *~
            *             Note-    Called from 'APCSCANN'               *~ 
            *                                                           *~
            *                      Print File  = MFGWOOD                *~
            *                      Script File = MFGWOOD                *~
            *                                                           *~
            *                      Print File  = NEAWOOD   North East   *~
            *                      Script File = NEAWOOD   North East   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDPLB05 - Generates the label format and data to print   *~
            *            the new Wood Surround Label. Label created     *~
            *            when production label Barcode Scanned.         *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *                     9% - Schema Lookup Error              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/09/04 ! Original - Copied & Mod (sub) AWDPLA05.  ! RHH *~
            * 09/29/05 ! (AWD001) - Mod to print Arg #6           !     *~
            * 12/28/05 ! (AWD002) - No Change for New Printers    ! RHH *~
            * 01/01/06 ! (PAR000) - CR347 No Change               ! RHH *~
            * 04/10/06 ! (PAR001) - Mods to print labels for N.E  ! RHH *~
            * 09/25/06 ! (PAR002) - No change for The room locatio! RHH *~
            *          !               change.                    !     *~
            *03/24/2009! (AWD003) - Mods to WS label              ! DES *~
            *05/21/2009! (AWD004) - Mods for Florida Windows      ! DES *~
            *08/20/2009! (AWD005) - Mods for Sill Angles          ! DES *~
            *07/27/2014! (AWD006) - Mods for new tube mull        ! CMG *~
            *07/28/2014! (AWD007) - mod for new arguments         ! CMG *~
            *************************************************************

        sub "AWDPLB05" (been_here%,      /* Zero (Only 1st Time)       */~
                        arg1$,           /* Load Number             (5)*/~
                        arg2$,           /* Wood Surround Code      (7)*/~
                        arg3$,           /* Family Number           (7)*/~
                        arg4$,           /* Day code                (7)*/~
                        arg5$,           /* Hinge Description      (12)*/~
                        arg6$,           /* Growth                  (7)*/~ 
                        arg7$,           /* WS label                (4)*/~ 
                        arg8$,           /* Sub Part               (20)*/~
                        arg9$,           /* Series                 (20)*/~
                        arg10$,          /* Open                   (20)*/~
                        arg11$,          /* Open                   (20)*/~
                        arg12$,          /* Open                   (20)*/~
		                  	#1,              /* GENCODES                   */~
                        error%)          /* Return Code                */

        dim                                                              ~
            schema$8,                    /* (PAR002) Schema Switch     */~
            a$60, b$60,                  /* Print Lines for Label      */~
            lbl$(40%)30,                 /* Label Data Array           */~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim yy$(30%)60,                  /* Buffer                     */~
            xx$(30%)60,                  /* Buffer                     */~
            arg1$5,                      /* Load Number                */~
            arg2$7,                      /* Wood surround Code         */~
            arg3$7,                      /* Family Number              */~
            arg4$7,                      /* Day Number                 */~
            arg5$12,                     /* Hinge                      */~
            arg6$7,                      /* DryWall                    */~
            arg7$4,                      /* Mull Config                */~
            arg8$20,                     /* Subpart (AWD007)           */~
            arg9$20,                     /* Series (AWD007)            */~
            arg10$20,                    /* Open (AWD007)              */~
            arg11$20,                    /* Open (AWD007)              */~
            arg12$20                     /* Open (AWD007)              */


        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Generate Wood Surround Labels     "
            pname$ = "AWDPLB05 - Rev: R1.00"

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
              * #5  ! MFGWOOD  ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "ROYAL", varc, consec, recsize =   60


                                             
                                                              
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************




            sel% = error%
            error%     = 0%  
            nbr_lines% = 0%
            fs$ = "^FS"


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
            init(" ") xx$()
            if been_here% > 0% then goto L01000
               gosub load_label           /* Build Label Format yy$() */ 
               gosub set_file_name        /* Create Label Print File  */
               gosub open_file            /* Open Label Print file    */

L01000:        if sel% = 99% then goto exit_print
                  been_here% = been_here% + 1%
                  copy yy$() to xx$()
                  gosub begin_process

                  goto exit_sub

        set_file_name                             /* (PAR001)       */
            init(" ") file$, script$
                                                
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

SS_1:                                                /* (PAR001)        */
            if schema% <> 1% then goto SS_2
                                                     /* North Caolina   */
               file$    = "MFGWOOD"
               script$  = "MFGWOOD"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
                                                     /* North East      */
SS_2:
               file$    = "NEAWOOD"
               script$  = "NEAWOOD"

               library$ = "NEDATA  "
               volume$  = "NE    "

SS_3:
        REM    file$   = "MFGTEST"
        REM    script$ = "MFGTEST"
 
        REM    file$   = "NEATEST"
        REM    script$ = "NEATEST"
        return                                     /* (PAR001)        */

        open_file
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L01100
               gosub file_exists         
               if comp% <> 16% then goto exit_sub
                  call "FILEBGON" (#5)

L01100:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return


        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process  
                                         
            init (" ") lbl$()
                                          /* Load Digit (1)            */ 
            lbl$(01%) = str(arg1$,1%,1%)      & fs$
                                          /* Load Digit (2)            */  
            lbl$(02%) = str(arg1$,2%,1%)     & fs$
                                          /* load Digit (3)            */
            lbl$(03%) = str(arg1$,3%,1%)     & fs$
                                          /* Load Digit (4)            */
            lbl$(04%) = str(arg1$,4%,1%)     & fs$
                                          /* Load Digit (5)            */
            lbl$(05%) = str(arg1$,5%,1%)     & fs$
                                          /* Wood Surround Code        */
            lbl$(06%) = arg2$                & fs$
                                          /* Family Number             */
            lbl$(07%) = arg3$               & fs$
                                          /* Production Day Number     */
            lbl$(08%) = arg4$               & fs$
                                          /* Hinge Code                */
            lbl$(09%) = arg5$               & fs$
                                          /* Growth                    */
                                          /* (AWD001)                  */
            lbl$(10%) = arg6$               & fs$
                                          /* (AWD003)                  */
            lbl$(11%) = arg7$               & fs$

                                          /* (AWD004)                  */
            lbl$(12%) = "         " & fs$
            if str(arg8$,6,1) = "3" then                       ~
                lbl$(12%) = "MULL CLIP"     & fs$
/* @@@ */
            if str(arg8$,13,1) = "1" then                       ~
                lbl$(13%) = "NEW SILL"     & fs$

            if str(arg8$,13,1) = "1" then                       ~
                lbl$(14%) = "ANGLE"        & fs$
/*(AWD006)*/   /* (AWD007) */
            if str(arg9$,1,4) = "35  " then                       ~
                lbl$(12%) = "1 TUBE"     & fs$
            if str(arg9$,1,4) = "130 " then                       ~
                lbl$(12%) = "1-1/4TUBE"     & fs$
/*(\AWD006)*/
    read_loop
        init(" ") a$
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        if a$ = " " then end_process
        a_len% = len(a$)                   /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$ */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                           /* Look for a Field Number    */ 
        l_len% = len(lbl$(ln%))            /* Find Length of Data Element*/
                                           /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%    /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),1%,l_len%)
      skip_data
                                           
        if nbr_lines% = 1% and been_here% = 1% then                        ~
                               b$ = hex(7e) & str(a$,2%,a_len%)

        if nbr_lines% = 1% and been_here% > 1% then                        ~
                               goto read_loop

  
        gosub print_line
        if a$ = "^XZ" then end_process       /* Last Line */
        goto read_loop

    end_process
        if nbr_lines% = 0% then error% = 8%
        return

 

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                          

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(60)

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


        end

        exit_print
            lb1% = 0% : lb2% = 0%

            close #5

            call "LINK" addr(script$, lb1%, lb2%)
            if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#5)          /* Scratch 'MFGWOOD'        */
        end

        file_exists
          comp% = 2% 
          hdr$ = "*  New Wood Surround Label   *"
          msg$(1%) = "       The File (MFGWOOD) Already Exists.        "
          msg$(2%) = " New  W O O D   S U R R O U N D   L a b e l s    "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                      /* (PAR001)     */
        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                      /* (PAR001)     */
        load_label
        init(" ") yy$() 

           yy$( 1%) = "^JO"
           yy$( 2%) = "^XA^EG^XZ"
 
           yy$( 3%) = "^XA"
           yy$( 4%) = "^PMN"
           yy$( 5%) = "^MNY"
           yy$( 6%) = "^MMT"                          /* Back Feed Off */
                                                      /* R=Off, T=On   */
           yy$( 7%) = "^MTT"
           yy$( 8%) = "^MD0"
           yy$( 9%) = "^LH0,0"
           yy$(10%) = "^LL203"
           yy$(11%) = "^PR3,4,8"                      /* PR Label Print Speed*/
                                                      /* a = 3 Print Speed   */
                                                      /* b = 4 Slew  SPeed   */
                                                      /* c = 8 Back Feed Spee*/
           yy$(12%) = "^JMA"

           yy$(13%) = "^COY,362"                      /* 256 + 128 Mem */
                                                      /* (-) 22k Intern*/
                                           /* 1st Digit of Load Number */
           yy$(14%) = "01^FO24,89^CI0^A0B,81,81^FR^FD"
                                           /* 2nd digit of Load Number */
           yy$(15%) = "02^FO104,89^CI0^A0B,81,81^FR^FD"
                                           /* 3rd Digit of Load Number */
           yy$(16%) = "03^FO194,89^CI0^A0B,81,81^FR^FD"
                                           /* 4th Digit of Load Number */
           yy$(17%) = "04^FO263,89^CI0^A0B,81,81^FR^FD"
                                           /* 5th Digit of Load Number */
           yy$(18%) = "05^FO343,89^CI0^A0B,81,81^FR^FD"
                                           /* Hinge Code               */
                                           /* (AWD001)                 */
           yy$(19%) = "09^FO746,20^CI0^A0B,30,30^FR^FD"
                                           /* Production Day           */
           yy$(20%) = "08^FO646,12^CI0^A0B,41,41^FR^FD"
                                           /* Family Number            */
           yy$(21%) = "07^FO579,12^CI0^A0B,41,41^FR^FD"
                                           /* Wood Surround Code       */
           yy$(22%) = "06^FO520,12^CI0^A0B,41,41^FR^FD"
                                           /* Arg#6 Print              */
           yy$(23%) = "10^FO683,33^CI0^A0B,41,41^FR^FD"
                                           /* (AWD001)                 */
					   /* <AWD003> */
           yy$(24%) = "11^FO777,20^CI0^A0B,30,30^FR^FD"
					   /* </AWD003> */
					   /* <AWD004> */
           yy$(25%) = "12^FO720,10^CI0^A0B,30,30^FR^FD"
					   /* </AWD004> */
					   /* <AWD005> */
           yy$(26%) = "13^FO430,10^CI0^A0B,30,30^FR^FD"
           yy$(27%) = "14^FO470,10^CI0^A0B,30,30^FR^FD"
					   /* </AWD005> */
           yy$(28%) = "^PQ1"

           yy$(29%) = "^XZ"

        return

