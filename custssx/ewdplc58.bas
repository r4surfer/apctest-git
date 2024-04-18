        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLC58                             *~
            *  Creation Date     - 09/22/99                             *~
            *  Last Modified Date- 04/06/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Generate Glass Barcode Label         *~
            *                                                           *~
            *  Process           - Glass Labels for Tempered and        *~
            *                      Special Shapes                       *~
            *               Note - PLAN LB03 is used for Production     *~
            *                      Glass - TP5                          *~               
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLC58 - Generates the label format and data to print   *~
            *            Tempered and Special Shapes Glass Labels.      *~
            *            The resulting file is routed to the label      *~
            *            printer via a script file.                     *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                                                           *~
            * Subroutine - Called by EWDPLN72                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/22/99 ! Original - New Program                   ! RHH *~
            * 04/05/06 ! (AWD001) - mods for NE                   ! CMG *~
            *          ! (AWD002) - mods for Ultra                !     *~    
            *09/20/2011! (AWD003) - mods for the warranty number  ! CMG *~
            *10/12/2014! (CR2190) - add t12 glass text            ! RDB *~
            *************************************************************
 
        sub "EWDPLC58" ( t1$,                      /* Model        (03)*/~
                         t2$,                      /* Color Descr  (06)*/~
                         t3$,                      /* Glass Type   (15)*/~
                         t4$,                      /* Litin/Grid   (06)*/~
                         t5$,                      /* Width & Heig (20)*/~
                         t6$,                      /* Top/Bot      (03)*/~
                         t7$,                      /* SO/Ln        (11)*/~
                         t8$,                      /* SO Due Date  (08)*/~
                         t9$,                      /* Contor Grid  (01)*/~
                         t10$,                     /* Ultra (AWD002)   */~
                         t11$,                     /* Warranty (AWD003)*/~
                         t12$,                     /* Glass Text CR2190*/~
                         #1,                       /* (GENCODES)       */~   
                         error%)                   /* Return Code      */

        dim                                                              ~
            t1$3,                        /* Model Code                 */~
            t2$6,                        /* Color Code                 */~
            t3$15,                       /* Glass Description          */~
            t4$6,                        /* Liting                     */~
            t5$20,                       /* Width and Height           */~
            t6$3,                        /* View Top/Bot               */~
            t7$11,                       /* Sales Order Line Item      */~
            t8$8,                        /* Due Date                   */~
            t9$1,                        /* Contour Grid               */~
            t10$5,                       /* Ultra (AWD002)             */~
            t11$9,                       /* Warranty                   */~
            t12$40,                      /* Glass text  CR2190         */~
            a$72, b$72,                  /* Print Lines for Label      */~
            desc$64,                     /* Descriptions for Scripts   */~
            lbl$(20%)60,                 /* Label Data Array           */~
            xx$(100%)80,                 /* Label Buffer               */~
            lbl_key$24,                  /* Save File Key              */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */


        dim f2%(2%),                     /* = 0 if the file is open    */~
            f1%(2%)                      /* = 1 if READ was successful */

        dim schema$8                     /* Schema (AWD088)            */
 
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD)Generate Tempered/Special Shapes Lables"
            pname$ = "EWDPLC58 - Rev: R7.00"

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
            *     !          !                                          *~
            *************************************************************~
            * #5  ! LBLGLR?? ! Print File For Glass Remake Labels       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "ROYAL", varc, consec, recsize =   72

            mat f1% = zer

/*(AWD001)*/  
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #1, schema_err%)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            error%          =  0%  :  counter% = 0%  :  nbr_lines% = 0%
/*(AWD001) */
            if schema% <> 1% then goto not_nc
            library$        = "APCDATA "
            volume$         = "CARLOS"

not_nc
            if schema% <> 2% then not_ne
            library$        = "NEDATA "
            volume$         = "NE"
not_ne

            fs$ = "^FS"


            xx$(1%) = "^JO"
            xx$(2%) = "^XA"
            xx$(3%) = "^PMN"
            xx$(4%) = "^MNY"
            xx$(5%) = "^MMR"
            xx$(6%) = "^MTT"
            xx$(7%) = "^MD0"
            xx$(8%) = "^LH0,0"
            xx$(9%) = "^LL203"
            xx$(10%)= "^PR4"
            xx$(11%)= "^JMA"

            xx$(12%)= "^FO732,134^FR^GB45,39,39^FS"
            xx$(13%)= "^FO467,71^FR^GB85,39,39^FS"

            xx$(14%)= "01^FO39,18^CI0^A0N,39,39^FR^FD"   /* Model Code   */
            xx$(15%)= "02^FO134,18^CI0^A0N,39,39^FR^FD"  /* Color Desc   */
            xx$(16%)= "03^FO293,18^CI0^A0N,39,39^FR^FD"  /* Glass Type   */
            xx$(17%)= "04^FO654,18^CI0^A0N,39,39^FR^FD"  /* Liting/Grid  */
            xx$(18%)= "05^FO39,71^CI0^A0N,39,39^FR^FD"   /* Width & Height*/
            xx$(19%)= "06^FO697,71^CI0^A0N,39,39^FR^FD"  /* View Top/Bot */
            xx$(23%)= "10^FO467,71^CI0^A0N,39,39^FR^FD"  /* ULTRA (AWD002) */
            xx$(20%)= "07^FO39,132^CI0^A0N,39,39^FR^FD"  /* S.O. and Line*/
            xx$(21%)= "08^FO467,132^CI0^A0N,39,39^FR^FD" /* SO Due Date  */
            xx$(22%)= "09^FO732,132^CI0^A0N,39,39^FR^FD" /* Contour Grid */
            xx$(24%)= "11^FO39,172^CI0^A0N,39,39^FR^FD"  /* WarrantyID   */
            xx$(25%)= "12^FO39,235^CI0^A0N,39,39^FR^FD"  /* Glass Text CR2190*/
            
            xx$(26%)= "^PQ1"
            xx$(27%)= "^XZ"


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        REM INPUTMODE
            gosub set_file_name
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
        return

        check_next_done
            counter%    = counter% + 1%
            if counter% < 4% then goto set_file_name
            error%      = 1%                       /* EXIT TRY LATER */
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
            gosub format_tempered_label
 
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

format_tempered_label                             /* Format            */
        lbl$(1%) = t1$ & fs$                      /* Model Code        */
        lbl$(2%) = t2$ & fs$                      /* Color Descr       */
        lbl$(3%) = t3$ & fs$                      /* Glass Type Code   */  
        lbl$(4%) = t4$ & fs$                      /* Liting / Grid     */
        lbl$(5%) = t5$ & fs$                      /* Width and Height  */   
        lbl$(6%) = t6$ & fs$                      /* View Top/Bot      */ 
        lbl$(7%) = t7$ & fs$                      /* S.O. and Line Item*/ 
        lbl$(8%) = t8$ & fs$                      /* S.O. Due Date     */
        lbl$(9%) = " " & t9$ & " " & fs$          /* Contour Grid      */
        lbl$(10%) = t10$ & fs$                    /* Ultra (AWD002)    */
        lbl$(11%) = t11$ & fs$                    /* WarrantyID        */
        lbl$(12%) = t12$ & fs$                    /* Glass Text CR2190 */
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
L65140:     read    #1,hold,key = lbl_key$, eod goto L65190
            put     #1, using L10280, "-"
            rewrite #1

        end
L65190:     counter%       = counter% + 1%
            if counter%    < 4% then L65140
            error%         = 6%
        end



