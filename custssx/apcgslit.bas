*       ****************************************************************~
*                            ( As Of 11/11/97 )                        *~
*        APCGSLIT - Calculate Vertical and Horizontal Grid Pattern     *~
*                                                                      *~
* 09/16/04 ! AWD001  -  Mod for Cont Head Sill                  * CMG  *~
*       ****************************************************************

        sub "APCGSLIT" (part$,           /* MFG Part Number           */ ~
                        muttin$,         /* Grid Vert/Horiz Output    */ ~
                        lt$,             /* Number of Lits            */ ~
                        tb$,             /* Top or Bottom Glass       */ ~
                        vert%,           /* Number of Verticals       */ ~
                        horz%,           /* Number of Horizontols     */ ~
                        #1,              /* (GENCODES) Code Table     */ ~
                        err% )           /* ERROR CODES 0% = OK       */

        dim readkey$24,                  /* GENCODES Primary Key      */ ~
            desc$32,                     /* GENCODES Description      */ ~
            part$25,                     /* Completion Date           */ ~
            muttin$8,                    /* Grid Code for GED         */ ~
            hng$2, flg$3,                /* Hinge Code AND DESC FLAG  */ ~
            lit$3,                       /* Liting Code               */ ~
            tb$1,                        /* Top or Bottom, T or B     */ ~
            lt$1,                        /* Number of Lits            */ ~
            model$3,                     /* Model Code                */ ~
            c_o$2,                       /* Cottage or Oriel          */ ~
            table$(10%)9                 /* Code Tables               */

            table$( 1%) = "GED 006  "    /* Standard 1 Lite            */
            table$( 2%) = "GED 007  "    /* Oriel - 2 Lite             */
            table$( 3%) = "GED 006  "    /* Standard - 2 Lite (GED 006)*/
            table$( 4%) = "GED 009  "    /* Cottage - 2 Lite           */
            table$( 5%) = "GED 006  "    /* Multi-Lite        (GED 006)*/
            table$( 6%) = "GED 011  "    /* 1/4-1/2-1/4 - 3 Lite       */
            table$( 7%) = "GED 006  "    /* 1/3-1/3-1/3 - 3 LiteGED 006*/
            model$ = str(part$,1%,3%)
            hng$   = str(part$,9%,2%)
            lit$   = "0" & str(part$,7%,2%)
            err% = 0%                    /* (0%) = OK             */
                                         /* (1%) = NO LITING CODE */
                                         /* (2%) = NO HINGE CODE  */
                                         /* (3%) = NO MUTTIN CODE */
            gosub lookup_liting
            if err% <> 0% then goto exit_program
            gosub lookup_hinge                 /* CO% = 0%,1%,2%,3%,4% */
            if err% <> 0% then goto exit_program

            gosub lookup_cont                 /*  (AWD001)        */

        REM - Main Line Processing
            if lt% <> 1% then goto L00520
               table% = 1%                     /* (1) - LITE STANDARD  */
               goto lookup_muttin

L00520:     if lt% <> 2% then goto L00580
               table% = 3%                     /* (2) - LITE STANDARD  */
               if co% = 1% then table% = 4%    /*            COTTAGE   */
               if co% = 2% then table% = 2%    /*            ORIEL     */
               goto lookup_muttin

L00580:     if lt% <> 3% then goto L00630
               table% = 6%                     /* (3) - LIT 1/4,1/2,1/4*/
               if co% = 3% then table% = 7%    /*           1/3,1/3,1/3*/
               goto lookup_muttin

L00630:     if cont% <> 1% then goto L00650        /*  (AWD001)  */
               table% = 3%
               if co% = 1% then table% = 4%    /*            COTTAGE   */
               if co% = 2% then table% = 2%    /*            ORIEL     */
               goto lookup_muttin

L00650:       table% = 5%                       /* (*) - LIT MULTIPLE   */
        lookup_muttin                              /* (AWD001)      */
             gosub get_muttin
             gosub vert_horz
        goto exit_program

        get_muttin
            init(" ") muttin$, readkey$
            str(readkey$,1%,9%)   = table$(table%)
            str(readkey$,10%,15%) = lit$
            read #1,key = readkey$, using L00870, desc$, eod goto L00790
               p% = pos(desc$ = "-")
               if p% = 0% then goto L00790
               muttin$ = str(desc$,1%,p%-2%)
               if tb$ = "B" then muttin$ = str(desc$,p%+2%)
        return
L00790:   err% = 3%                                       /* NO MUTTIN */
        return

        lookup_liting                    /* Find No. of Lits (GED LIT) */
            readkey$ = " " : lt% = 0%
            str(readkey$,1%,9%)   = "GED LIT  "
            str(readkey$,10%,15%) = model$
            read #1,key = readkey$, using   L00870, desc$, eod goto L00930
L00870:        FMT POS(25), CH(30)
            lt$ = str(desc$,1%,1%)
            lt% = 4%                                  /* Multiple Lits */
            convert lt$ to lt%, data goto L00910
L00910:
        return
L00930:     err% = 1%                               /* No Liting Found */
        return

        lookup_hinge
           init(" ") c_o$, readkey$, flg$        /* 0% = STANDARD      */
           co% = 0%                              /* 1% = COTTAGE CODE  */
           str(readkey$,1%,9%)   = "HINGE    "   /* 2% = ORIEL CODE    */
           str(readkey$,10%,15%) = hng$          /* 3% = 1/3,1/3,1/3   */
                                                 /* 4% = 1/4,1/2,1/4   */
           read #1,key = readkey$,using   L00870, desc$,eod goto L01120

              c_o$ = str(desc$,1%,2%)
              if c_o$ = "CO" then co% = 1%
              if c_o$ = "OR" then co% = 2%
              p% = pos(desc$ = "-")
              if (p%-4%) > 0% then flg$ = str(desc$,p%-4%,3%)
              if flg$ = "1/3" then co% = 3%             /* 1/3,1/3,1/3 */
              if flg$ = "1/2" then co% = 4%             /* 1/4,1/2,1/4 */
        return
L01120:    err% = 2%                                  /* NO HINGE CODE */
        return

        vert_horz
            vert%, horz% = 0%
            vv% = pos(muttin$ = "V")
            xx% = pos(muttin$ = "x")
            hh% = pos(muttin$ = "H")
            if vv% = 0% or xx% = 0% or hh% = 0% then return
            convert str(muttin$,1%,vv%-1%) to vert%, data goto L01220
L01220:
            hor%  = (hh% - 1%) - xx%
            convert str(muttin$,xx%+1%,hor%) to horz%, data goto L01250
L01250:
        return

        lookup_cont                                   /*  (AWD001)  */
            readkey$ = " "  : cont% = 0%
            str(readkey$,1%,9%)   = "PLAN CONT"
            str(readkey$,10%,15%) = model$
            read #1,key = readkey$, eod goto not_cont
                  cont% = 1%
        not_cont

        return                                        /*  (AWD001)  */

        exit_program
        end


