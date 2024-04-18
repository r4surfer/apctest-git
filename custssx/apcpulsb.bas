*       ****************************************************************~
*                           ( As of 01/25/06 - cmg )                   *~
*        APCPULSB - Calculate the Current Pull Quantity for Specified  *~
*                   Part                                               *~
*                                                                      *~
*                   Note - Quantitiy is Total as of Call               *~
*                          Mod for Upgrade to Release R6.04.03         *~
*       ****************************************************************

        sub "APCPULSB" (sel%,            /* Selection 0% - Current Qty / ~
                                         /*           1% - Add Record */ ~
                                         /*           2% - Delete Rec.*/ ~
                        pull_part$,      /*                           */ ~
/*PAR000*/              pull_subp$,      /* Pull Subpart number       */ ~
                        pull_cust$,      /* S.O. Cutomer Code         */ ~
                        pull_so$,        /* Sales Order               */ ~
                        pull_ln$,        /* Sales Order Line Item     */ ~
                        pull_date$,      /* Date Record Created       */ ~
                        pull_load$,      /* Reference Load Number     */ ~
                        pull_qty%,       /* S.O. Qty to Pull from Stk */ ~
                        pull_oh%,        /* Curr On-Hand At Create    */ ~
                        #1,              /* (APCPULLS) - APC PULL Mast*/ ~
                        pull% )          /* 0% = Ok, <> 0% = Error%   */

        dim pull_part$25,                /* Glass Completion Date     */ ~
/*PAR000*/  pull_subp$20,                /* PUll subpart number       */ ~
            pull_cust$9,                 /* Glass Production Date     */ ~
            pull_so$8,                   /* Glass Prod. Date UNFORMAT */ ~
            pull_ln$2,                   /* Batch Identifiers ( A-Z ) */ ~
            pull_date$6,                 /* Batch Record              */ ~
            pull_load$5,                 /* Batch Record              */ ~
            pull_key$45,                 /* Generic Pull Key          */ ~
            pull_rec$128,                /* Pull Record               */ ~
            pull_fil$49,                 /* Filler Area               */ ~
            date$8                       /* TODAYS DATE               */

            init(" ") pull_key$, pull_rec$, pull_fil$
            date$ = date
            call "DATEFMT" (date$)
            call "DATUNFMT" (date$)
            pull% = 0%
            p% = sel%
            p% = p% + 1%
            on p% gosub pull_scan, pull_add, pull_delete

            goto exit_program

        pull_scan              /* FIND THE CURRENT APC PULL STK QUANTITY*/
            qty%, pull_qty% = 0%
            str(pull_key$, 1%,25%) = pull_part$
* PAR000
            str(pull_key$,26%,20%) = pull_subp$
            read #1,key 2% = pull_key$, using   L00500, pull_key$, qty%,    ~
                                                  eod goto L00550
            goto L00520
        pull_scan_next
            read #1, using   L00500, pull_key$, qty%, eod goto L00550
* PAR000
L00500:       FMT POS(20), CH(45), POS(76), BI(2)

L00520:     if str(pull_key$, 1%,25%)  <> pull_part$ then return
            if str(pull_key$,26%,20%)  <> pull_subp$ then return
               pull_qty% = pull_qty% + qty%
               goto pull_scan_next
L00550: return

        pull_add
            pull_date$ = str(date$,1%,6%)
            pull_key$ = " "
            str(pull_key$,1%,8%) = pull_so$
            str(pull_key$,9%,2%) = pull_ln$
            read #1,hold,key = pull_key$, eod goto L00650
               delete #1
L00650:     put #1, using   L00680, pull_cust$, pull_so$, pull_ln$,         ~
                                 pull_part$, pull_subp$, pull_date$,        ~
                                 pull_load$, pull_qty%, pull_oh%, pull_fil$
* PAR000
L00680:       FMT CH(9), CH(8), CH(2), CH(25), CH(20), CH(6), CH(5), BI(2),~
                  BI(2), CH(49)
           write #1, eod goto L00730

        return
L00730:    pull% = 1%                    /* (ERROR) - NO UPDATE */
        return

        pull_delete
            pull_key$ = " "
            str(pull_key$,1%,8%) = pull_so$
            str(pull_key$,9%,2%) = pull_ln$
            read #1,hold,key = pull_key$, eod goto L00830
               delete #1
        return
L00830:     pull% = 1%
        return

        exit_program

        end

