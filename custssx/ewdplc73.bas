        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLC73                             *~
            *  Creation Date     - 11/08/99                             *~
            *  Last Modified Date- 08/12/02                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modifications By  - Royal H. Hoffman                     *~ 
            *                                                           *~
            *  Description       - Print the New Lowe's Color Labels    *~
            *                                                           *~
            *                      Print File  - MFGLOWES               *~
            *                      Script File - MFGLOWES               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLC73 - Generates the label format and data to print   *~
            *            Lowe's Special Labels.                         *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/08/99 ! Original - Copied & Mod (sub) EWDPLA71   ! RHH *~
            * 04/12/00 ! (EWD001) - Mods to put Labels into one   ! RHH *~
            *          !    file.                                 !     *~ 
            * 05/12/00 ! (EWD002) - Sku number changed to six     ! RHH *~
            *          !    Digits                                !     *~   
            * 08/12/02 ! (EWD003) - Mod to put UPC on label       ! RHH *~    
            *************************************************************

        sub "EWDPLC73" (type%,      /* type% = 0%-(Label), 1%-(Header) */~
                        sp_dept$,        /* Department Code            */~
                        sp_seq$,         /* Production Sequence Number */~
                        sp_sku$,         /* SKU Number                 */~
                        sp_load$,        /* Load Number                */~
                        sp_part$,        /* MFG Part Number            */~
                        sp_mon$,         /* Production Month           */~
                        sp_day$,         /* Production Day             */~
                        p_wd1$,          /* Nom Width in Feet          */~
                        p_wd2$,          /* Nom Width in Inches        */~
                        p_ht1$,          /* Nom Height in Feet         */~
                        p_ht2$,          /* Nom Height in Inches       */~ 
                        wd$,             /* Opening Width  - Inches    */~
                        ht$,             /* Opening Height - Inches    */~
                        sp_upc$,         /* (EWD003) UPC Code Label    */~ 
                        been_here%,      /* Zero (Only 1st Time)       */~
                        #5,              /* (MFGLOWES)                 */~
                        err%)            /* Error Code 0% = Ok         */

        dim                                                              ~
            sp_dept$3,                   /* Department Code            */~
            sp_seq$5,                    /* Prod Seq. Number           */~
            sp_sku$6,                    /* Product SKU No.            */~
            sp_load$5,                   /* Load Number                */~
            sp_part$25,                  /* EWD Part Number - Exact    */~
            sp_mon$2,                    /* Production Month           */~
            sp_day$2,                    /* Production Day             */~
            sp_upc$11,                   /* (EWD003) UPC Code Lowe's   */~
            p_wd1$1, p_wd2$2,            /* Width Feet and Inches      */~
            p_ht1$1, p_ht2$2,            /* Height Feet and Inches     */~
            wd$2, ht$2,                  /* Width and Height Tot Inches*/~
            a$72, b$72,                  /* Print Lines for Label      */~
            lbl$(45%)55,                 /* Label Data Array   (EWD001)*/~
            fs$3                         /* End of Lbl Records         */

        dim xx$(70%)72,                  /* Buffer                     */~
            yy$(70%)72                   /* Buffer                     */ 

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Generate Lowe's Special Labels"
            pname$ = "EWDPLC73 - Rev: R7.00"

        REM *************************************************************

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #5  ! MFGLOWES ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            nbr_lines% = 0% : err% = 0%
            fs$ = "^FS"
            q$  = hex(22)
            call "RJUSTIFY" (p_wd2$)
            call "RJUSTIFY" (p_ht2$)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
            init(" ") xx$()
            if been_here% = 1% then goto L01000
               gosub load_label           /* Build Label Format yy$() */ 
               been_here% = 1%

L01000:     copy yy$() to xx$()
            gosub begin_process

            goto exit_sub

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process
            if type% = 0% then gosub format_label                        ~
                          else gosub format_header                     
                                          /* Load Label Array Elements */
 
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
                                           /* (EWD006) - 09/09/1999      */
        if nbr_lines% = 1% then b$ = hex(7e) & str(a$,1%,a_len%)
  
        gosub print_line
        if a$ = "^XZ" then end_process       /* Last Line */
        goto read_loop


    end_process
        if nbr_lines% = 0% then error% = 8%
        return

        format_label
            init (" ") lbl$()          
            lbl$(01%) = p_wd1$               & fs$  /* Width in Feet     1*/
  
            lbl$(02%) = p_wd2$               & fs$  /* Width in Inches   2*/
 
            lbl$(03%) = p_ht1$               & fs$  /* Height in Feet    1*/ 

            lbl$(04%) = p_ht2$               & fs$  /* Height in Inche   2*/
    
            lbl$(05%) = P_wd1$               & fs$  /* Width in Feet     1*/

            lbl$(06%) = p_wd2$               & fs$  /* Width in Inches   2*/

            lbl$(07%) = p_ht1$               & fs$  /* Height in Feet    1*/

            lbl$(08%) = p_ht2$               & fs$  /* Height in Inches  2*/ 

            lbl$(09%) = wd$                  & fs$  /* Open Width Inches 2*/

            lbl$(10%) = ht$                  & fs$  /* Open Height Inches2*/

            lbl$(11%) = wd$                  & fs$  /* Open Width Inches 2*/
 
            lbl$(12%) = ht$                  & fs$  /* Open Height Inches2*/

            lbl$(13%) = str(sp_sku$,1%,1%)   & fs$  /* Sku 1st Digit     1*/

            lbl$(14%) = str(sp_sku$,2%,1%)   & fs$  /* Sku 2nd Digit     1*/

            lbl$(15%) = str(sp_sku$,3%,1%)   & fs$  /* Sku 3rd Digit     1*/

            lbl$(16%) = str(sp_sku$,4%,1%)   & fs$  /* Sku 4th Digit     1*/ 
 
            lbl$(17%) = str(sp_sku$,5%,1%)   & fs$  /* Sku 5th Digit     1*/

            lbl$(18%) = str(sp_sku$,1%,1%)   & fs$  /* Sku 1st Digit     1*/

            lbl$(19%) = str(sp_sku$,2%,1%)   & fs$  /* Sku 2nd Digit     1*/

            lbl$(20%) = str(sp_sku$,3%,1%)   & fs$  /* Sku 3rd Digit     1*/

            lbl$(21%) = str(sp_sku$,4%,1%)   & fs$  /* Sku 4th Digit     1*/ 
 
            lbl$(22%) = str(sp_sku$,5%,1%)   & fs$  /* Sku 5th Digit     1*/

            lbl$(23%) = sp_seq$              & fs$  /* Prod Seq. No.     5*/

            lbl$(24%) = sp_mon$& "/" &sp_day$& fs$  /* Month an Day      5*/

            lbl$(25%) = sp_mon$& "/" &sp_day$& fs$  /* Month an Day      5*/

            lbl$(26%) = sp_seq$              & fs$  /* Prod Seq. No.     5*/

            lbl$(27%) = sp_load$             & fs$  /* Load Number       5*/

            lbl$(28%) = sp_load$             & fs$  /* Load Number       5*/

            lbl$(29%) = q$                   & fs$  /* Quote             1*/
          
            lbl$(30%) = q$ & ")"             & fs$  /* Quote & ")"       2*/

            lbl$(31%) = q$ & " x"            & fs$  /* Quote & " x"      3*/
     
            lbl$(32%) = str(sp_sku$,6%,1%)   & fs$  /* Sku 6th Digit Left1*/ 
 
            lbl$(33%) = str(sp_sku$,6%,1%)   & fs$  /* Sku 6th Digit Righ1*/

                                                    /* (EWD003) UPC Code  */
            lbl$(34%) = sp_upc$              & fs$ 
        return

        format_header
            init (" ") lbl$()          
            lbl$(01%) = "0"                  & fs$  /* Width in Feet     1*/
  
            lbl$(02%) = "00"                 & fs$  /* Width in Inches   2*/
 
            lbl$(03%) = "0"                  & fs$  /* Height in Feet    1*/ 

            lbl$(04%) = "00"                 & fs$  /* Height in Inche   2*/
    
            lbl$(05%) = "0"                  & fs$  /* Width in Feet     1*/

            lbl$(06%) = "00"                 & fs$  /* Width in Inches   2*/

            lbl$(07%) = "0"                  & fs$  /* Height in Feet    1*/

            lbl$(08%) = "00"                 & fs$  /* Height in Inches  2*/ 

            lbl$(09%) = "DE"                 & fs$  /* Open Width Inches 2*/

            lbl$(10%) = "PT"                 & fs$  /* Open Height Inches2*/

            lbl$(11%) = "DE"                 & fs$  /* Open Width Inches 2*/
 
            lbl$(12%) = "PT"                  & fs$  /* Open Height Inches2*/

            lbl$(13%) = "D"                  & fs$  /* Sku 1st Digit     1*/

            lbl$(14%) = "P"                  & fs$  /* Sku 2nd Digit     1*/

            lbl$(15%) = str(sp_dept$,1%,1%)  & fs$  /* Sku 3rd Digit     1*/

            lbl$(16%) = str(sp_dept$,2%,1%)  & fs$  /* Sku 4th Digit     1*/ 
 
            lbl$(17%) = str(sp_dept$,3%,1%)  & fs$  /* Sku 5th Digit     1*/

            lbl$(18%) = "D"                  & fs$  /* Sku 1st Digit     1*/

            lbl$(19%) = "P"                  & fs$  /* Sku 2nd Digit     1*/

            lbl$(20%) = str(sp_dept$,1%,1%)  & fs$  /* Sku 3rd Digit     1*/

            lbl$(21%) = str(sp_dept$,2%,1%)  & fs$  /* Sku 4th Digit     1*/ 
 
            lbl$(22%) = str(sp_dept$,3%,1%)  & fs$  /* Sku 5th Digit     1*/

            lbl$(23%) = "DEPT."              & fs$  /* Prod Seq. No.     5*/

            lbl$(24%) = "XX"& sp_dept$ &"XX" & fs$  /* Month an Day      5*/

            lbl$(25%) = "XX"& sp_dept$ &"XX" & fs$  /* Month an Day      5*/

            lbl$(26%) = "DEPT."              & fs$  /* Prod Seq. No.     5*/ 

            lbl$(27%) = sp_mon$&"/"&sp_day$  & fs$  /* Load Number       5*/

            lbl$(28%) = sp_mon$&"/"&sp_day$  & fs$  /* Load Number       5*/

            lbl$(29%) = q$                   & fs$  /* Quote             1*/
          
            lbl$(30%) = q$ & ")"             & fs$  /* Quote & ")"       2*/

            lbl$(31%) = q$ & " x"            & fs$  /* Quote & " x"      3*/

            lbl$(32%) = " "                  & fs$  /* Sku 6th Digit Left1*/ 
 
            lbl$(33%) = " "                  & fs$  /* Sku 6th Digit Righ1*/

            lbl$(34%) = "00000000000"        & fs$  /* (EWD003) UPC Code  */

        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

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

        end

        load_label
           init(" ") yy$()

           yy$( 1%) = "^JO"
           yy$( 2%) = "^XA"
           yy$( 3%) = "^PMN"
           yy$( 4%) = "^MNY"
           yy$( 5%) = "^MMR"
           yy$( 6%) = "^MTT"
           yy$( 7%) = "^MD0"
           yy$( 8%) = "^LH0,0"
           yy$( 9%) = "^LL4877"
           yy$(10%) = "^PR4"
           yy$(11%) = "^JMA"

           yy$(12%) = "01^FO136,1471^CI0^A0N,182,183^FR^FD"        /* (01)  */

           yy$(13%) = "02^FO358,1471^CI0^A0N,183,183^FR^FD"        /* (02)  */

           yy$(14%) = "29^FO536,1471^CI0^A0N,138,97^FR^FD"         /* (29)  */

           yy$(15%) = "^FO284,1697^CI0^A0N,183,183^FR^FDX^FS"

           yy$(16%) = "03^FO140,1930^CI0^A0N,183,183^FR^FD"        /* (03)  */

           yy$(17%) = "^FO230,1930^CI0^A0N,102,102^FR^FD'^FS"

           yy$(18%) = "04^FO364,1930^CI0^A0N,183,183^FR^FD"        /* (04)  */

           yy$(19%) = "29^FO536,1930^CI0^A0N,102,81^FR^FD"         /* (29)  */

           yy$(20%) = "05^FO831,1473^CI0^A0N,202,171^FR^FD"        /* (05)  */

           yy$(21%) = "^FO904,1473^CI0^AFN,130,65^FR^FD'^FS"

           yy$(22%) = "06^FO1016,1473^CI0^A0N,202,171^FR^FD"       /* (06)  */

           yy$(23%) = "29^FO1199,1473^CI0^A0N,120,68^FR^FD"        /* (29)  */

           yy$(24%) = "^FO965,1697^CI0^A0N,202,166^FR^FDX^FS"

           yy$(25%) = "07^FO831,1920^CI0^A0N,202,171^FR^FD"        /* (07)  */

           yy$(26%) = "^FO925,1920^CI0^A0N,120,81^FR^FD'^FS"

           yy$(27%) = "08^FO1016,1920^CI0^A0N,202,171^FR^FD"       /* (08)  */

           yy$(28%) = "29^FO1189,1930^CI0^A0N,120,68^FR^FD"        /* (29)  */

           yy$(29%) = "^FO152,2611^CI0^A0N,183,163^FR^FDItem #^FS"

           yy$(30%) = "^FO823,2611^CI0^A0N,183,163^FR^FDItem #^FS"

           yy$(31%) = "13^FO244,2881^CI0^A0N,254,488^FR^FD"        /* (13)  */

           yy$(32%) = "14^FO232,3137^CI0^A0N,254,488^FR^FD"        /* (14)  */

           yy$(33%) = "15^FO228,3393^CI0^A0N,254,488^FR^FD"        /* (15)  */

           yy$(34%) = "16^FO207,3649^CI0^A0N,254,488^FR^FD"        /* (16)  */

           yy$(35%) = "18^FO870,2849^CI0^A0N,273,467^FR^FD"        /* (18)  */

           yy$(36%) = "19^FO882,3113^CI0^A0N,273,469^FR^FD"        /* (19)  */

           yy$(37%) = "20^FO894,3377^CI0^A0N,273,468^FR^FD"        /* (20)  */

           yy$(38%) = "21^FO890,3641^CI0^A0N,273,464^FR^FD"        /* (21)  */

           yy$(39%) = "22^FO878,3903^CI0^A0N,273,476^FR^FD"        /* (22)  */

           yy$(40%) = "23^FO163,4552^CI0^A0N,61,61^FR^FD"          /* (23)  */

           yy$(41%) = "24^FO376,4562^CI0^A0N,61,61^FR^FD"          /* (24)  */

           yy$(42%) = "27^FO264,4623^CI0^A0N,61,61^FR^FD"          /* (27)  */

           yy$(43%) = "^FO226,1471^CI0^ADN,90,60^FR^FD'^FS"

           yy$(44%) = "17^FO215,3903^CI0^A0N,254,488^FR^FD"        /* (17)  */

           yy$(45%) = "30^FO1189,2144^CI0^A0N,152,81^FR^FD"        /* (30)  */

           yy$(46%) = "12^FO1067,2144^CI0^A0N,163,122^FR^FD"       /* (12)  */

           yy$(47%) = "31^FO955,2144^CI0^A0N,120,76^FR^FD"         /* (31)  */

           yy$(48%) = "11^FO843,2144^CI0^A0N,163,122^FR^FD"        /* (11)  */

           yy$(49%) = "^FO813,2144^CI0^A0N,143,80^FR^FD(^FS"

           yy$(50%) = "30^FO528,2154^CI0^A0N,132,91^FR^FD"         /* (30)  */

           yy$(51%) = "10^FO406,2154^CI0^A0N,163,122^FR^FD"        /* (10)  */

           yy$(52%) = "31^FO305,2154^CI0^A0N,102,81^FR^FD"         /* (31)  */

           yy$(53%) = "09^FO173,2154^CI0^A0N,163,122^FR^FD"        /* (09)  */

           yy$(54%) = "^FO132,2154^CI0^A0N,142,132^FR^FD(^FS"

           yy$(55%) = "26^FO843,4542^CI0^A0N,61,61^FR^FD"          /* (26)  */

           yy$(56%) = "25^FO1067,4552^CI0^A0N,61,61^FR^FD"         /* (25)  */

           yy$(57%) = "28^FO945,4613^CI0^A0N,61,61^FR^FD"          /* (28)  */

                                                                   /* (EWD002) */
           yy$(58%) = "32^FO213,4176^CI0^A0N,254,488^FR^FD"        /* (32)  */

           yy$(59%) = "33^FO874,4166^CI0^A0N,272,475^FR^FD"       /* (33)  */
                                                                  /* (EWD003) */
           yy$(60%) = "34^FO102,4212^BY3,2.0,102^BUB,102,Y,N,Y^FR^FD"

           yy$(61%) = "34^FO788,4192^BY3,2.0,102^BUB,102,Y,N,Y^FR^FD"  


           yy$(62%) = "^PQ1"
           yy$(63%) = "^XZ"



        return

 
