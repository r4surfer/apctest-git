        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPARTN                             *~
            *  Creation Date     - 01/09/06                             *~
            *  Last Modified Date- 07/17/2013                           *~
            *  Description       - New Part Number Explosion            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/09/06 ! New Program for (AWD) - Part Number      ! RHH *~
            *          !   Explosion with Old and New             !     *~
            * 06/02/06 ! (AWD001) Mod to Add new Fields to the    ! RHH *~
            *          !   Explosion Screen.                      !     *~  
            *07/17/2013! (AWD002) add dimensions to screen        ! CMG *~
            *07/18/2014! (AWD003) add complete sub part no        ! PWW *~
            *************************************************************

            sub "AWDPARTN" ( switch%,    /* Current Planning Tables    */~
                             so_ln$,     /* Sales Order Line Item      */~
                             so_no$,     /* Sales Order Number         */~
                             so_part$,   /* Old Part Number            */~
                             sub_part$,  /* Sub Part Number            */~
                             info_part$, /* Information Part Number    */~
                             dim1es,     /* (AWD002) dim1es            */~
                             dim2es,     /* (AWD002) dim2es            */~
                             dim3es,     /* (AWD002) dim3es            */~
                             #1,         /* (BCKSUBPT) New Sub Part No */~         
                             #2 )        /* FILE = (GENCODES)          */

        dim                              /* Subroutine - Variables     */~
            so_ln$3,                     /* S.O. Line Item Blank Filled*/~
            so_no$8,                     /* S.O. No. Leading Zero's    */~
            so_part$25,                  /* Old Part Number            */~
            sub_part$20,                 /* New Sub Part Number        */~
            info_part$9,                 /* New Information Part Number*/~ 
            tab$(34%)9,                  /* Code Table Names           */~
            code$3,                      /* Test Value                 */~
            title$25,                    /* Screen Title               */~
/*<AWD003>*/header$79,header2$79,        /* Screen Header              */~ 
            val$(34%)4, descr$30,        /* Table Value and Description*/~
            vald$(34%)24,                /* Table Descriptions         */~
            fract$(8%)12,                /* Fraction Discription       */~
            readkey$24,                  /* Table Lookup Key           */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            time$10, timed$8,            /* Time of Day                */~
            hr$2, mm$2, a_m$2,           /* Time Calc                  */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            cursor%(2%),                 /* Cursor Location for Edit   */~
            i$(24%)80,                   /* SCREEN IMAGE               */~
            pf$(3%)79,                   /* PF Key Description         */~
            pfkeys$32                    /* PF Key Values              */

        dim                                                              ~
            err1$2,                      /* Error Code String          */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */
 
            fract$(1%) = "-    Inches"
            fract$(2%) = "-1/8 Inch"
            fract$(3%) = "-1/4 Inch"
            fract$(4%) = "-3/8 Inch"
            fract$(5%) = "-1/2 Inch"
            fract$(6%) = "-5/8 Inch"
            fract$(7%) = "-3/4 Inch"
            fract$(8%) = "-7/8 Inch"
                                         /* Switch% = 0% History Call  */
                                         /* Switch% = 1% Caelus Call   */

            tab$(1%)  = "MODEL    " : tab$(11%)  = "WALLWIDT "
            tab$(2%)  = "COLOR    " : tab$(12%)  = "Dim1es   "
            tab$(3%)  = "GLASS    " : tab$(13%)  = "Dim2es   "
            tab$(4%)  = "LITING   " : tab$(14%)  = "         "
            tab$(5%)  = "HINGE    " : tab$(15%)  = "         "
            tab$(6%)  = "SCREEN   " : tab$(16%)  = "         "
            tab$(7%)  = "LOCKS    " : tab$(17%)  = "         "
            tab$(8%)  = "WIDTH    " : tab$(18%)  = "GRDTYPE  "
            tab$(9%)  = "HEIGHT   " : tab$(19%)  = "GRDSIZE  "
            tab$(10%) = "APC WOOD " : tab$(20%)  = "GRDCOLOR "

            tab$(21%) = "HARDWARE " : tab$(33%)  = "PLANTACT "
            tab$(22%) = "FOAM     " : tab$(34%)  = "ELLISON01"
            tab$(23%) = "ACCESSOR "                      /* (AWD001) */
            tab$(24%) = "SPACER   "
            tab$(25%) = " - - - - "
            tab$(26%) = "(Table)  "
            tab$(27%) = "INTCOLOR "
            tab$(28%) = "EXTCOLOR "
            tab$(29%) = "SEAT     "
            tab$(30%) = "FIN      "
            tab$(31%) = "PLANTINV "
            tab$(32%) = "PLANTSTD "

            u3% = 0%
                                                    /* Not Called From  */
                                                    /* History          */
            if switch% <> 0% then gosub lookup_part_number  

            gosub build_header

            gosub decode_part_number

            gosub display_codes

        end

        decode_part_number
           call "SHOSTAT" ("Decoding Part Number")

           gosub calc_date_time

           init(" ") val$(), vald$()
                                                    /* Model Code       */
           val%       = 1%
           val$(val%) = str(so_part$,1%,3%)
           gosub table_lookup
                                                    /* Color Code      */
           val%       = 2%
           val$(val%) = str(so_part$,4%,1%)
           gosub table_lookup
                                                    /* Glass Code      */
           val%       = 3%
           val$(val%) = str(so_part$,5%,2%)
           gosub table_lookup
                                                    /* Liting Code     */
           val%       = 4%
           val$(val%) = str(so_part$,7%,2%)
           gosub table_lookup
                                                    /* Hinge Code      */
           val%       = 5%
           val$(val%) = str(so_part$,9%,2%)
           gosub table_lookup
                                                    /* Screen Code     */
           val%       = 6%
           val$(val%) = str(so_part$,11%,1%)
           gosub table_lookup
                                                    /* Lock Code       */
           val%       = 7%
           val$(val%) = str(so_part$,12%,1%)
           gosub table_lookup
                                                    /* Width Value     */
           val%       = 8%
           val$(val%) = str(so_part$,13%,4%)
           gosub build_width_height
                                                    /* Height Value    */
           val%       = 9%
           val$(val%) = str(so_part$,17%,3%)
           gosub build_width_height
           
/* (AWD002) */
           val% = 12%
           convert dim1es to vald$(val%), pic(###0.0000)

           val% = 13%
           convert dim2es to vald$(val%), pic(###0.0000)

/* (\AWD002) */           
                                                    /* Center Line Meet*/
                                                    /* Special Logic   */
                                                    /* 'CLMR     '     */
           if len(so_part$) < 22% then goto CLMR_2  /* Skip Decode     */ 
           val%       = 10%
           val$(val%) = str(so_part$,20%,3%)
           init(" ") code$
           code% = 0%
           code$ = val$(val%)
           convert code$ to code%, data goto CLMR_1

           if code% > 70% then tab$(val%) = "CLMR     "
CLMR_1:
           if str(tab$(val%),1%,4%) = "CLMR" then gosub build_clmr   ~
                                             else gosub table_lookup

CLMR_2:
                                                    /* Wall Width      */
                                                    /* Special Logic   */
           if len(so_part$) < 25% then goto WALLWIDT_2
           val%       = 11%
           val$(val%) = str(so_part$,23%,3%)
           init(" ") code$
           code% = 0%
           code$ = val$(val%)
           convert code$ to code%, data goto WALLWIDT_1

           tab$(val%) = "WALLWIDT "
WALLWIDT_1:

           if str(tab$(val%),1%,8%) = "WALLWIDT" then gosub build_wallwidt ~
                                                 else gosub table_lookup


WALLWIDT_2:
                                                    /* New Part No.    */
                                                    /* Begin with 18   */
                                                    /* Grid Type       */
           val%       = 18%
           val$(val%) = str(sub_part$,1%,1%)
           gosub table_lookup
                                                    /* Grid Size       */
           val%       = 19%
           val$(val%) = str(sub_part$,2%,1%)
           gosub table_lookup
                                                    /* Grid Color      */
           val%       = 20%
           val$(val%) = str(sub_part$,3%,1%)
           gosub table_lookup
                                                    /* Hardware        */
           val%       = 21%
           val$(val%) = str(sub_part$,4%,1%)
           gosub table_lookup
                                                    /* Foam            */
           val%       = 22%
           val$(val%) = str(sub_part$,5%,1%)
           gosub table_lookup
                                                    /* (AWD001) New Fld*/
           val%       = 23%
           val$(val%) = str(sub_part$,6%,1%)
           gosub table_lookup

           val%       = 24%
           val$(val%) = str(sub_part$,17%,1%)
           gosub table_lookup
REM           VALD$(VAL%)= "A V A I L A B L E"

           val%       = 25%
           val$(val%) = str(sub_part$,8%,1%)
        REM   gosub table_lookup
           vald$(val%)= "A V A I L A B L E"
                                                    /* (AWD001) Fields */      
                                                    /* New Info Part   */
                                                    /* Internal Color  */
           val$(26%) = "Code"                       /* Temporary       */
           vald$(26%) = "Information Part Number Fields"
                                                    /* Temporary       */
           val%       = 27%
           val$(val%) = str(info_part$,1%,1%)
           gosub table_lookup
                                                    /* External Color  */
           val%       = 28%
           val$(val%) = str(info_part$,2%,1%)
           gosub table_lookup
                                                    /* Seat            */
           val%       = 29%
           val$(val%) = str(info_part$,3%,1%)
           gosub table_lookup
                                                    /* Fin             */
           val%       = 30%
           val$(val%) = str(info_part$,4%,1%)
           gosub table_lookup
                                                    /* Plant Invoice   */
           val%       = 31%
           val$(val%) = str(info_part$,5%,1%)
           gosub table_lookup
                                                    /* Plant Standard  */
           val%       = 32%
           val$(val%) = str(info_part$,6%,1%)
           gosub table_lookup
                                                    /* Plant Account  */
           val%       = 33%
           val$(val%) = str(info_part$,7%,1%)
           gosub table_lookup
                                                    /* Private Label  */
           val%       = 34%
           val$(val%) = str(info_part$,8%,2%)
           gosub table_lookup

        return

        table_lookup
           err% = 0%
           init(" ") readkey$, descr$, errormsg$
           str(readkey$,1%,9%)   = tab$(val%)
           str(readkey$,10%,15%) = val$(val%)
           read #2,key = readkey$, using TABLE_1, descr$, eod goto TABLE_2
TABLE_1:      FMT POS(25), CH(30)

              vald$(val%) = str(descr$,1%,24%)
              err% = 1%
TABLE_2:
              if err% = 1% then return

                 errormsg$ = "Lookup Error for Table (" & tab$(val%) & ") Value = " ~
                             & val$(Val%)
                 gosub error_prompt
        return

        lookup_part_number
            err1% = 0%
            init(" ") bcksubpt_rec$, flds$(), info_flds$(), errormsg$
            dim1es, dim2es, dim3es = 0.00
            flag$ = "0"                  /* Sales Order Info           */
            pgm$  = "1" 

            so_inv$  = so_no$
            item_no$ = so_ln$ 

            call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                               pgm$,         /* Calling Program 0=BCKUPDTE */~
                                             /* 1=Any Other 2=Delete       */~
                                             /* 3=Invoice                  */~
                               so_inv$,      /* SO or Invoice Num to lookup*/~
                               item_no$,     /* Item Number                */~
                               bcksubpt_rec$,/* Record If BCKUPDTE then    */~
                                             /* pass in else pass out      */~
                               flds$(),      /* Part Number Fields         */~
                               info_flds$(), /* Information Fields         */~
                               #1,           /* BCKSUBPT File              */~
                               err1%)        /* Error Code                 */

            so_part$   = str(bcksubpt_rec$,23%,25%)    /* Old Part Number  */           
            sub_part$  = str(bcksubpt_rec$,48%,20%)    /* New Sub Part     */
            info_part$ = str(bcksubpt_rec$,132%,9%)    /* Info Part Number */

            get bcksubpt_rec$ using dimFmt, dim1es, dim2es
dimFMT:               FMT POS(153), PD(14,4), PD(14,4)

            
            if err1% = 0% then return

            convert err1% to err1$, pic(##)

            errormsg$ = "Part Number Lookup Error (" & err1$ & ")???"
            gosub error_prompt

        return

        build_header
            init(" ") title$, header$,header2$            /*<AWD003>*/
            title$ = "New Part Number Explosion"

            header$ = "                                        ~
                      ~                                       "
                                         /* Old Part Number   */  
            str(header$,1%,7%)  = "Current"
            str(header$,9%,25%) = so_part$
                                         /* New Part Number   */
/*          str(header$,36%,9%) = "New Part:"                    <AWD003>+*/
/*          str(header$,46%,12%) = str(sub_part$,1%,12%)      */
/*                                          Info Part Number  */
/*          str(header$,60%,10%) = "Info Part:"               */
/*          str(header$,71%,9%) = info_part$                  */
            str(header$,49%,10%) = "Info Part:"
            str(header$,60%,9%) = info_part$
            str(header2$,1%,9%) = "New Part:"
            str(header2$,11%,20%) = str(sub_part$,1%,20%)      /*<AWD003>-*/

        return

        build_width_height
            if val% <> 8% then goto BUILD_1
               if str(so_part$,13%,1%) = "0" then vald$(val%) = str(so_part$,14%,2%)~
                                             else vald$(val%) = str(so_part$,13%,3%)
               fract$ = str(so_part$,16%,1%)
               goto BUILD_2
BUILD_1:
               vald$(val%) = str(so_part$,17%,2%)
               fract$ = str(so_part$,19%,1%)
BUILD_2:
            fract% = 0%
            convert fract$ to fract%, data goto BUILD_3

BUILD_3:
            for i% = 1% to 8%
                if (fract% + 1%) = i% then goto BUILD_4
            next i%
BUILD_4:
            vald$(val%) = vald$(val%) & fract$(i%) 
        return

        build_clmr
            if str(so_part$,20%,1%) = "0" then vald$(val%) = str(so_part$,21%,1%)~
                                          else vald$(val%) = str(so_part$,20%,2%)
            
            fract$ = str(so_part$,22%,1%)

            fract% = 0%
            convert fract$ to fract%, data goto BUILD_CLMR_1

BUILD_CLMR_1:
            for i% = 1% to 8%
                if (fract% + 1%) = i% then goto BUILD_CLMR_2
            next i%
BUILD_CLMR_2:
            vald$(val%) = vald$(val%) & fract$(i%) 
        return

        build_wallwidt
            if str(so_part$,23%,1%) = "0" then vald$(val%) = str(so_part$,24%,1%)~
                                          else vald$(val%) = str(so_part$,23%,2%)
            fract$ = str(so_part$,25%,1%)

            fract% = 0%
            convert fract$ to fract%, data goto BUILD_WALLWIDT_1

BUILD_WALLWIDT_1:
            for i% = 1% to 8%
                if (fract% + 1%) = i% then goto BUILD_WALLWIDT_2
            next i%
BUILD_WALLWIDT_2:
            vald$(val%) = vald$(val%) & fract$(i%) 
        return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        calc_date_time
           date$ = date
           call "DATEFMT" (date$)

           time$ = time
           hr$ = str(time$,1%,2%) 
           mm$ = str(time$,3%,2%)
           a_m$ = "AM"
           hr% = 0%
           convert hr$ to hr%, data goto t_1
T_1:
           if hr% >= 12% then a_m$ = "PM"
           if hr% >= 12% then hr% = hr% - 12%
           convert hr% to hr$, pic(00)

           timed$ = hr$ & ":" & mm$ & " " & a_m$
        return

        REM *************************************************************~
            *           D I S P L A Y   C O D E   T A B L E             *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_codes
L01480:     gosub set_pf1
            accept                                                       ~
               at (01,02), "Time:",                                      ~
               at (01,08), fac(hex(8c)), timed$                 , ch(08),~   
               at (01,28), fac(hex(a4)), title$                 , ch(25),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (02,02), fac(hex(84)), header$                , ch(79),~
/*<AWD003>*/   at (03,02), fac(hex(84)), header2$               , ch(79),~
                                                                         ~
               at (04,02), "(Table)   Code Description(Part No)",        ~
               at (05,02), "--------- ---- ------------------------",    ~
                                                                         ~
               at (04,42), "(Table)   Code Description(Sub Part No)",    ~
               at (05,42), "--------- ---- ------------------------",    ~
                                                                         ~
               at (06,02), fac(hex(84))  , tab$(k% + 1%)        , ch(09),~
               at (06,12), fac(hex(84))  , val$(k% + 1%)        , ch(04),~
               at (06,17), fac(hex(84))  , vald$(k% + 1%)       , ch(24),~
                                                                         ~
               at (06,42), fac(hex(84))  , tab$(k% + 18%)       , ch(09),~
               at (06,52), fac(hex(84))  , val$(k% + 18%)       , ch(04),~
               at (06,57), fac(hex(84))  , vald$(k% + 18%)      , ch(24),~
                                                                         ~
               at (07,02), fac(hex(84))  , tab$(k% + 2%)        , ch(09),~
               at (07,12), fac(hex(84))  , val$(k% + 2%)        , ch(04),~
               at (07,17), fac(hex(84))  , vald$(k% + 2%)       , ch(24),~
                                                                         ~
               at (07,42), fac(hex(84))  , tab$(k% + 19%)       , ch(09),~
               at (07,52), fac(hex(84))  , val$(k% + 19%)       , ch(04),~
               at (07,57), fac(hex(84))  , vald$(k% + 19%)      , ch(24),~
                                                                         ~
               at (08,02), fac(hex(84))  , tab$(k% + 3%)        , ch(09),~
               at (08,12), fac(hex(84))  , val$(k% + 3%)        , ch(04),~
               at (08,17), fac(hex(84))  , vald$(k% + 3%)       , ch(24),~
                                                                         ~
               at (08,42), fac(hex(84))  , tab$(k% + 20%)       , ch(09),~
               at (08,52), fac(hex(84))  , val$(k% + 20%)       , ch(04),~
               at (08,57), fac(hex(84))  , vald$(k% + 20%)      , ch(24),~
                                                                         ~
               at (09,02), fac(hex(84))  , tab$(k% + 4%)        , ch(09),~
               at (09,12), fac(hex(84))  , val$(k% + 4%)        , ch(04),~
               at (09,17), fac(hex(84))  , vald$(k% + 4%)       , ch(24),~
                                                                         ~
               at (09,42), fac(hex(84))  , tab$(k% + 21%)       , ch(09),~
               at (09,52), fac(hex(84))  , val$(k% + 21%)       , ch(04),~
               at (09,57), fac(hex(84))  , vald$(k% + 21%)      , ch(24),~
                                                                         ~
               at (10,02), fac(hex(84))  , tab$(k% + 5%)        , ch(09),~
               at (10,12), fac(hex(84))  , val$(k% + 5%)        , ch(04),~
               at (10,17), fac(hex(84))  , vald$(k% + 5%)       , ch(24),~
                                                                         ~
               at (10,42), fac(hex(84))  , tab$(k% + 22%)       , ch(09),~
               at (10,52), fac(hex(84))  , val$(k% + 22%)       , ch(04),~
               at (10,57), fac(hex(84))  , vald$(k% + 22%)      , ch(24),~
                                                                         ~
               at (11,02), fac(hex(84))  , tab$(k% + 6%)        , ch(09),~
               at (11,12), fac(hex(84))  , val$(k% + 6%)        , ch(04),~
               at (11,17), fac(hex(84))  , vald$(k% + 6%)       , ch(24),~
                                                                         ~
               at (11,42), fac(hex(84))  , tab$(k% + 23%)       , ch(09),~
               at (11,52), fac(hex(84))  , val$(k% + 23%)       , ch(04),~
               at (11,57), fac(hex(84))  , vald$(k% + 23%)      , ch(24),~
                                                                         ~
               at (12,02), fac(hex(84))  , tab$(k% + 7%)        , ch(09),~
               at (12,12), fac(hex(84))  , val$(k% + 7%)        , ch(04),~
               at (12,17), fac(hex(84))  , vald$(k% + 7%)       , ch(24),~
                                                                         ~
               at (12,42), fac(hex(84))  , tab$(k% + 24%)       , ch(09),~
               at (12,52), fac(hex(84))  , val$(k% + 24%)       , ch(04),~
               at (12,57), fac(hex(84))  , vald$(k% + 24%)      , ch(24),~
                                                                         ~
               at (13,02), fac(hex(84))  , tab$(k% + 8%)        , ch(09),~
               at (13,12), fac(hex(84))  , val$(k% + 8%)        , ch(04),~
               at (13,17), fac(hex(84))  , vald$(k% + 8%)       , ch(24),~
                                                                         ~
               at (13,42), fac(hex(84))  , tab$(k% + 25%)       , ch(09),~
               at (13,52), fac(hex(84))  , val$(k% + 25%)       , ch(04),~
               at (13,57), fac(hex(84))  , vald$(k% + 25%)      , ch(24),~
                                                                         ~
               at (14,02), fac(hex(84))  , tab$(k% + 9%)        , ch(09),~
               at (14,12), fac(hex(84))  , val$(k% + 9%)        , ch(04),~
               at (14,17), fac(hex(84))  , vald$(k% + 9%)       , ch(24),~
                                                                         ~
               at (14,42), fac(hex(84))  , tab$(k% + 26%)       , ch(09),~
               at (14,52), fac(hex(84))  , val$(k% + 26%)       , ch(04),~
               at (14,57), fac(hex(84))  , vald$(k% + 26%)      , ch(24),~
                                                                         ~
               at (15,02), fac(hex(84))  , tab$(k% + 10%)       , ch(09),~
               at (15,12), fac(hex(84))  , val$(k% + 10%)       , ch(04),~
               at (15,17), fac(hex(84))  , vald$(k% + 10%)      , ch(24),~
                                                                         ~
               at (15,42), fac(hex(84))  , tab$(k% + 27%)       , ch(09),~
               at (15,52), fac(hex(84))  , val$(k% + 27%)       , ch(04),~
               at (15,57), fac(hex(84))  , vald$(k% + 27%)      , ch(24),~
                                                                         ~
               at (16,02), fac(hex(84))  , tab$(k% + 11%)       , ch(09),~
               at (16,12), fac(hex(84))  , val$(k% + 11%)       , ch(04),~
               at (16,17), fac(hex(84))  , vald$(k% + 11%)      , ch(24),~
                                                                         ~
               at (16,42), fac(hex(84))  , tab$(k% + 28%)       , ch(09),~
               at (16,52), fac(hex(84))  , val$(k% + 28%)       , ch(04),~
               at (16,57), fac(hex(84))  , vald$(k% + 28%)      , ch(24),~
                                                                         ~
               at (17,02), fac(hex(84))  , tab$(k% + 12%)       , ch(09),~
               at (17,12), fac(hex(84))  , val$(k% + 12%)       , ch(04),~
               at (17,17), fac(hex(84))  , vald$(k% + 12%)      , ch(24),~
                                                                         ~
               at (17,42), fac(hex(84))  , tab$(k% + 29%)       , ch(09),~
               at (17,52), fac(hex(84))  , val$(k% + 29%)       , ch(04),~
               at (17,57), fac(hex(84))  , vald$(k% + 29%)      , ch(24),~
                                                                         ~
               at (18,02), fac(hex(84))  , tab$(k% + 13%)       , ch(09),~
               at (18,12), fac(hex(84))  , val$(k% + 13%)       , ch(04),~
               at (18,17), fac(hex(84))  , vald$(k% + 13%)      , ch(24),~
                                                                         ~
               at (18,42), fac(hex(84))  , tab$(k% + 30%)       , ch(09),~
               at (18,52), fac(hex(84))  , val$(k% + 30%)       , ch(04),~
               at (18,57), fac(hex(84))  , vald$(k% + 30%)      , ch(24),~
                                                                         ~
               at (19,02), fac(hex(84))  , tab$(k% + 14%)       , ch(09),~
               at (19,12), fac(hex(84))  , val$(k% + 14%)       , ch(04),~
               at (19,17), fac(hex(84))  , vald$(k% + 14%)      , ch(24),~
                                                                         ~
               at (19,42), fac(hex(84))  , tab$(k% + 31%)       , ch(09),~
               at (19,52), fac(hex(84))  , val$(k% + 31%)       , ch(04),~
               at (19,57), fac(hex(84))  , vald$(k% + 31%)      , ch(24),~
                                                                         ~
               at (20,02), fac(hex(84))  , tab$(k% + 15%)       , ch(09),~
               at (20,12), fac(hex(84))  , val$(k% + 15%)       , ch(04),~
               at (20,17), fac(hex(84))  , vald$(k% + 15%)      , ch(24),~
                                                                         ~
               at (20,42), fac(hex(84))  , tab$(k% + 32%)       , ch(09),~
               at (20,52), fac(hex(84))  , val$(k% + 32%)       , ch(04),~
               at (20,57), fac(hex(84))  , vald$(k% + 32%)      , ch(24),~
                                                                         ~
               at (21,02), fac(hex(84))  , tab$(k% + 16%)       , ch(09),~
               at (21,12), fac(hex(84))  , val$(k% + 16%)       , ch(04),~
               at (21,17), fac(hex(84))  , vald$(k% + 16%)      , ch(24),~
                                                                         ~
               at (21,42), fac(hex(84))  , tab$(k% + 33%)       , ch(09),~
               at (21,52), fac(hex(84))  , val$(k% + 33%)       , ch(04),~
               at (21,57), fac(hex(84))  , vald$(k% + 33%)      , ch(24),~
                                                                         ~
               at (22,02), fac(hex(84))  , tab$(k% + 17%)       , ch(09),~
               at (22,12), fac(hex(84))  , val$(k% + 17%)       , ch(04),~
               at (22,17), fac(hex(84))  , vald$(k% + 17%)      , ch(24),~
                                                                         ~
               at (22,42), fac(hex(84))  , tab$(k% + 34%)       , ch(09),~
               at (22,52), fac(hex(84))  , val$(k% + 34%)       , ch(04),~
               at (22,57), fac(hex(84))  , vald$(k% + 34%)      , ch(24),~
                                                                         ~
               at (24,02), fac(hex(a4)), pf$(1%)                , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L02710             /* First    */
L02680:           k% = 0%
                  goto L01480

L02710:        if keyhit% <> 3% then goto L02760             /* Last      */
L02720:           x% = int(val_max% / 34%)
                  k% = (x%*34%)
                  goto L01480

L02760:        if keyhit% <> 4% then goto L02820             /* Previous */
                  if k% < 35% then goto L02680
                  k% = k% - 34%
                  if k% <= 1% then goto L02680
                  goto L01480

L02820:        if keyhit% <> 5% then goto L02870             /* Next     */
                  k% = k% + 34%
                  if k% < val_max% then goto L01480
                  goto L02720

L02870:        if keyhit% <> 15 then goto L02910
                  call "PRNTSCRN"
                  goto L01480

L02910:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            pf$(1) = "(2)First     (3)Last     (4)Previous    " &        ~
                     " (5)Next (15)Print Screen <Return> Cont"
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            gosub check_screen
            return

        check_screen
            if val_max% > 34% then goto L03120
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L03120:      if k% >= 34% then goto L03150
                gosub no_first
                gosub no_prev
L03150:      if (k% + 34%) <= val_max% then goto L03170
                gosub no_last
L03170:      if k% <= (val_max% - 34%) then goto L03190
                gosub no_next
L03190: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),41%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(1%),14%,9%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(1%),26%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

