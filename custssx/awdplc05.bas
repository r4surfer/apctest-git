        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - AWDPLC05                             *~
            *  Creation Date     - 04/04/05                             *~
            *  Last Modified Date- 01/20/2016                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modifications By  - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      new Trailer Label with Load No.      *~
            *                                                           *~
            *                      Print File  = MFGSTAG,MFGSHIP,MFGTEST*~
            *                      Script File = MFGSTAG,MFGSHIP,MFGTEST*~
            *                                   (Print File and Script) *~
            *                      switch% = 1%    MFGSTAG NESTAG   'A' *~
            *                      switch% = 2%    MFGSHIP NESHIP   'B' *~
            *                      switch% = 3%    MFGTEST NETEST   'T' *~
            *                      switch% = 4%    MFGRGA  NERGA    'C' *~
            *                      switch% = 5%    MFGUPS  NEUPS    'D' *~
            *                      switch% = 6%    MFGNEA  MFGNEA   'Z' *~
            *                      switch% = 7%    MFGFLB  NEFLB    'E' *~
            *                      switch% = 8%    MFGCROSD  NECROSD'G' *~
            *-----------------------------------------------------------*~
            * AWDPLC05 - Generates the label format and data to print   *~
            *            Label for the Trailer. The resulting file is   *~
            *            routed to the label printer via a script.      *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *                     9% - Schema Looku Error               *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/04/05 ! Original - Copied & Mod (sub) AWDPLA05.  ! RHH *~
            * 12/28/05 ! (AWD010) - Mod for Two new printers in   ! RHH *~
            *          !            Shipping.                     !     *~
            * 01/01/06 ! (PAR000) CR347 No Change                 ! RHH *~
            * 03/20/06 ! (PAR001) Mod for printing Shipping labels! RHH *~
            *          !    at North East.                        !     *~
            * 04/20/06 ! (PAR002) Mod to add new Printer code 'Z' ! RHH *~
            *          !    for the North East But Print Here at  !     *~
            *          !    Atrium.                               !     *~
            * 09/25/06 ! (PAR003) No change required for room     ! RHH *~
            *          !    location change.                      !     *~
            *10/29/2009! (PAR004) Mod to add new Printer code 'E' ! DES *~
            *01/20/2016! (SR70701) add option for printer G       ! CMG *~
            *************************************************************

        sub "AWDPLC05" (switch%,         /* 0% thru 5%                 */~
                        been_here%,      /* Zero (Only 1st Time)       */~
                        tr_trailer$,     /* Trailer Number             */~
                        tr_load$,        /* Load Number                */~
                        #1,              /* GENCODES                   */~
                        error%)          /* Return Code                */

        dim                                                              ~
            schema$8,                    /* (PAR001) Schema Switch     */~
            a$90, b$90,                  /* Print Lines for Label      */~
            lbl$(40%)60,                 /* Label Data Array           */~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */~

        dim yy$(90%)90,                  /* Buffer                     */~
            xx$(90%)90,                  /* Buffer                     */~
            tr_trailer$8,                /* Trailer Number             */~
            tr_load$5                    /* Load Number                */~


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
            apc$   = "(AWD) Generate Loading Labels Trailer  "
            pname$ = "AWDPLC05 - Rev: 01.00"

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
            * #5  ! MFGSHIP  ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "ROYAL", varc, consec, recsize =   90




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

        set_file_name                           /* (AWD004)              */
            init(" ") file$, script$
                                                /* (PAR002)              */
            err%    = 0%
            call "SCHEMA" (schema$,             /* What switch 1-NC 2-NE */~
                           schema%,             /* Schema                */~
                           #1,                  /* GENCODES              */~
                           err% )               /* error                 */

            if err% = 0% then goto SS_1
               error% = 9%
               end

SS_1:                                    /* (PAR002)                     */
                                         /* (AWD010)                     */
            on switch% gosub L01010,     /* 'MFGSTAG' Staging Printer 'A'*/~
                             L01020,     /* 'MFGSHIP' Staging Printer 'B'*/~
                             L01030,     /* 'MFGTEST' Label Test Printer */~
                             L01040,     /* 'MFGRGA'  Staging Printer 'C'*/~
                             L01050,     /* 'MFGUPS'  Staging Printer 'D'*/~
                             L01060,     /* 'MFGNEA'  Staging Prt NE  'Z'*/~
                             L01070,     /* 'MFGNEA'  Staging Prt NE  'Z'*/~
                             L01080,     /* 'MFGCROSD' (SR70701) 'G'     */~
                             L01090,     /* 'MFGNECD' Cross Dock      'G'*/~
/*(CR456)*/                  L01150,     /* 'MFGDCZA' Cross Dock      'H'*/~
/*(CR456)*/                  L01170,     /* 'MFGDCZB' Cross Dock      'I'*/~
/*(CR456)*/                  L01250      /* 'MFGDCZC' Cross Dock      'J'*/
                                         /* (PAR002)                     */
                                         /* (AWD010)                     */

        return

L01010:                                         /* 'MFGSTAG'-1%*/
        if schema% <> 1% then goto L01015
            file$   = "MFGSTAG"
            script$ = "MFGSTAG"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01015:
            file$   = "NESTAG"
            script$ = "NESTAG"
            library$= "NEDATA  "
            volume$ = "NE    "
        return

L01020:                                         /* 'MFGSHIP'-2%*/
        if schema% <> 1% then goto L01025
            file$   = "MFGSHIP"
            script$ = "MFGSHIP"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01025:
            file$   = "NESHIP"
            script$ = "NESHIP"
            library$= "NEDATA  "
            volume$ = "NE    "
        return

L01030:                                         /* 'MFGTEST'-3%*/
        if schema% <> 1% then goto L01035
            file$   = "MFGTEST"
            script$ = "MFGTEST"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01035:
            file$   = "NETEST"
            script$ = "NETEST"
            library$= "NEDATA  "
            volume$ = "NE    "
        return
L01040:                                         /* 'MFGSHIP'-4%*/
        if schema% <> 1% then goto L01045
            file$   = "MFGRGA"
            script$ = "MFGRGA"
            library$= "APCDATA "
            volume$ = "CARLOS"
            return
L01045:
            file$   = "NERGA"
            script$ = "NERGA"
            library$= "NEDATA  "
            volume$ = "NE    "
        return

L01050:                                         /* 'MFGTEST'-5%*/
        if schema% <> 1% then goto L01055
            file$   = "MFGUPS"
            script$ = "MFGUPS"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01055:
            file$   = "NEUPS"
            script$ = "NEUPS"
            library$= "NEDATA  "
            volume$ = "NE    "
        return
                                                /* (PAR002)   */
L01060:                                         /* 'MFGNEA'-6%*/
        if schema% <> 1% then goto L01065
            file$   = "NEASTAG"
            script$ = "NEASTAG"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01065:
            file$   = "MFGNEA"                 /* NE but Print at Atrium */
            script$ = "MFGNEA"
            library$= "NEDATA  "
            volume$ = "NE    "
        return

L01070:                                         /* 'MFGNEA'-6%*/
        if schema% <> 1% then goto L01075
            file$   = "MFGFLB"
            script$ = "MFGFLB"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
L01075:
            file$   = "NEFLB"                  /* NE but Print at Atrium */
            script$ = "NEFLB"
            library$= "NEDATA  "
            volume$ = "NE    "
        return
L01080:
        if schema% = 1% then goto L01090
REM mfgcrosd
             file$   = "MFGCROSD"              /* NE but Print at Atrium */
             script$ = "MFGCROSD"
             library$= "NEDATA  "
             volume$ = "NE    "
        return
L01090:                /* SR70180                  'MFGNECD'-9%*/
REM     if schema% <> 1% then goto L01150
            file$   = "NECROSD"
            script$ = "NECROSD"
            library$= "NEDATA  "
            volume$ = "NE    "
        return

/* (CR456) + */
L01150:
        if schema% <> 1% then return           /* switch% = 10% */
            file$   = "MFGDCZA"
            script$ = "MFGDCZA"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return


L01170:
        if schema% <> 1% then return           /* switch% = 11% */
            file$   = "MFGDCZB"
            script$ = "MFGDCZB"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return

L01250:
        if schema% <> 1% then return           /* switch% = 12% */
            file$   = "MFGDCZC"
            script$ = "MFGDCZC"
            library$= "APCDATA "
            volume$ = "CARLOS"
        return
/* (CR456) - */




                                                /* (PAR002)    */
                                                /* (PAR001)    */
                                                /* (AWD010)    */
                                                /* (AWD004)    */
        open_file
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L01100
               gosub file_exists
               if comp% <> 16% then goto exit_sub
                  call "FILEBGON" (#5)

L01100:    open nodisplay #5, output, space = 100%,                      ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return


        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
      begin_process
                                          /* Load data from (AWDAPPLB) */
        init (" ") lbl$()
                                          /* Trailer Number            */
        lbl$(01%) = tr_trailer$  & fs$
                                          /* Appian Load No.           */
        lbl$(02%) = tr_load$     & fs$


      read_loop
        init(" ") a$
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        if a$ = " " then end_process
        a_len% = len(a$)                   /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /*Put into b$ Data from a$ */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                           /* Look for a Field Number    */
        l_len% = len(lbl$(ln%))            /* Find Length of Data Element*/
                                           /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%    /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                          ~
            & str(lbl$(ln%),1%,l_len%)
      skip_data
                                           /* (AWD002)                  */
        if nbr_lines% = 1% and been_here% = 1% then                       ~
                               b$ = hex(7e) & str(a$,2%,a_len%)

        if nbr_lines% = 1% and been_here% > 1% then                       ~
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
                                                       /* (EWD001)     */

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(90)

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

            call "FILEBGON" (#5)          /* Scratch 'MFGSHIP'        */
        end

        file_exists
          comp% = 2%
          hdr$ = "***    New Loading Label   ***"
          msg$(1%) = "       The File (MFGSHIP) Already Exists.       "
          msg$(2%) = "        New  L O A D I N G   L A B E L           "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

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
           yy$(10%) = "^LL813"
           yy$(11%) = "^PR3,4,8"                  /* PR Label Print Speed*/
                                                  /* a = 3 Print Speed   */
                                                  /* b = 4 Slew  SPeed   */
                                                  /* c = 8 Back Feed Spee*/
           yy$(12%) = "^JMA"

           yy$(13%) = "^COY,362"                      /* 256 + 128 Mem */
                                                      /* (-) 22k Intern*/

           yy$(14%) = "01^FO431,45^BY4,2.0,132^BCN,132,N,N,N^FR^FD>:"

           yy$(15%) = "02^FO240,343^CI0^A0N,264,264^FR^FD"

           yy$(16%) = "01^FO536,197^CI0^A0N,61,61^FR^FD"

           yy$(17%) = "^PQ1"

           yy$(18%) = "^XZ"

        return
