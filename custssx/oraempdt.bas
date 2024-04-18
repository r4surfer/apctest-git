        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - ORAEMPDT                             *~
            *  Creation Date     - 07/25/2017                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Norman                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *07/25/2017! Original                                 ! CMN *~
            *01/29/2019! CR-1821 ADP Increase Emp ID field length ! DES *~
            *03/01/2019! CR-1894 Increase EMP Dept from 2 to 3 CH ! DES *~
            *************************************************************


        sub "ORAEMPDT" (#1,              /* APCEMPDT                   */~
                        #2,              /* GENCODES                   */~
                        jobid$,                                         ~
                        trans$,                                         ~
                        action$,                                        ~
                        file$,                                          ~
                        division$,                                      ~
                        location$,                                      ~
                        department$,                                    ~
                        mfgDept$,                                       ~
                        fields$(),                                      ~
                        error%)          /* Error Flag from File Open  */

        dim jobid$10,                    /* Job ID                     */~
            trans$10,                    /* Transaction Number         */~
            key$100,                     /* Read Key                   */~
            action$1,                    /* Record Action              */~
            file$,                       /* File Name                  */~
            division$3,                  /* Division Number            */~
            location$3,                  /* Location Number            */~
            department$4,                /* Corporate Department       */~
            mfgDept$3,                   /* Mfg Department             */~
            badgeID$5,                   /* Badge ID                   */~
            field$256,                   /* Query Return Field         */~
            fields$(100)64,              /* Oracle Fields              */~
            rec$128,                     /* APCEMPDT file record       */~
            appliedDate$10,              /* Applied Date               */~
            appliedDte$6,                /* Unformatted Applied Date   */~
            dte$,                        /* System Date                */~
            dec$20                       /* Decimal                    */

        dim clockCde$1,                  /* Clock Code                 */~
            ent_yr$4,                    /* Entry Production Year      */~
            ent_bi_yr$2,                 /* Entry Prod Binary Year     */~
            ent_wk$2,                    /* Entry Prod Week       (OUT)*/~
            ent_dy$1,                    /* Entry Production Day  (OUT)*/~
            inDate$10,                   /* Punch In Date              */~
            inDte$6,                     /* Punch In Date Unformatted  */~
            outDate$10,                  /* Punch Out Date             */~
            outDte$6,                    /* Punch Out Date Unformatted */~
            punchTme$5                   /* Time Clock Punch Time      */~


        init(" ") ent_yr$, ent_bi_yr$, ent_wk$, ent_dy$, appliedDate$,~
                  appliedDte$, inDate$, inDte$, outDate$, outDte$,    ~
                  punchTme$, clockCde$, dec$, badgeID$


        error%, pl_e% = 0%
        dte$ = date
        badgeID$ = fields$(01%)

REM ================================
REM   Convert Dates
REM ================================

        appliedDate$  = fields$(03%)
        field$        = fields$(03%)
        gosub convert_date
        appliedDte$   = field$

        inDate$  = fields$(04%)
        field$   = fields$(04%)
        gosub convert_date
        inDte$   = field$

        outDate$ = fields$(06%)
        field$   = fields$(06%)
        gosub convert_date
        outDte$  = field$

REM =========================================
REM   Check for say date and time punch
REM =========================================
        if inDte$ <> OutDte$ then goto NotSameDte
          if fields$(05%) <> fields$(07%) then goto NotSameDte
                goto FINI
NotSameDte:

        call "AWDEMP1B" (ent_yr$,     /* Entry Production Year (OUT)*/~
                         ent_bi_yr$,  /* Entry Prod Binary Year(OUT)*/~
                         ent_wk$,     /* Entry Prod Week       (OUT)*/~
                         ent_dy$,     /* Entry Production Day  (OUT)*/~
                         appliedDte$, /* Entry Production Date (6)  */~
                         appliedDate$,/* Entry Production Date (10) */~
                         #2,          /* GENCODES                   */~
                         pl_e%    )   /* 0% = Ok, Not Zero error    */
REM            if pl_e% <> 0% then goto FINI

        init(" ") dec$
        dec$ = fields$(10%)
        gosub convertDec
        fields$(10%) = dec$
REM ===================================
REM    Clock In
REM ===================================
        if inDate$ = " " then goto clockOut

        init(" ") ent_yr$, ent_bi_yr$, ent_wk$, ent_dy$
        call "AWDEMP1B" (ent_yr$,     /* Entry Production Year (OUT)*/~
                         ent_bi_yr$,  /* Entry Prod Binary Year(OUT)*/~
                         ent_wk$,     /* Entry Prod Week       (OUT)*/~
                         ent_dy$,     /* Entry Production Day  (OUT)*/~
                         inDte$,      /* Entry Production Date (6)  */~
                         inDate$,     /* Entry Production Date (10) */~
                         #2,          /* GENCODES                   */~
                         pl_e%    )   /* 0% = Ok, Not Zero error    */
         if pl_e% <> 0% then goto clockOut

         punchtme% = 0%
         convert fields$(05%) to punchTme%, data goto badInTime
         
badInTime:

         convert punchTme% to punchTme$, pic(###0)
         gosub checkRecord
         if rec% = 1% then goto clockOut
         clockCde$ = "0"
         if badgeID$ <= " " then badgeID$ = "00000"

         str(rec$,01%,02%)   = ent_bi_yr$     /* Emp Yr Bin  */
         str(rec$,03%,02%)   = ent_wk$        /* Emp Wk      */
         str(rec$,05%,01%)   = ent_dy$        /* Emp Day     */
         str(rec$,06%,05%)   = badgeID$       /* Emp No      */
         str(rec$,11%,03%)   = mfgDept$       /* Department  */
         str(rec$,14%,05%)   = punchTme$      /* Punch In 24 */
         str(rec$,19%,01%)   = clockCde$      /* Clock Code  */
         str(rec$,20%,02%)   = " "            /* Filler      */
         str(rec$,22%,03%)   = fields$(09%)   /* User ID     */
         str(rec$,25%,06%)   = inDte$         /* Dte Adj for Time Zone */
         str(rec$,31%,08%)   = time           /* System Time */
         str(rec$,39%,06%)   = dte$           /* System Date */
         str(rec$,45%,08%)   = time           /* Time        */
         str(rec$,53%,08%)   = fields$(10%)   /* Payrate     */
         str(rec$,61%,10%)   = jobid$         /* ADP Job ID  */
         str(rec$,71%,20%)   = fields$(11%)   /* ADP EMP ID  */
         str(rec$,91%,10%)   = fields$(02%)   /* ADP EV5 Dept*/
         str(rec$,101%,03%)  = fields$(08%)   /* ADP Pay Grp */
         str(rec$,104%,03%)  = division$      /* ADP Division*/
         str(rec$,107%,03%)  = location$      /* ADP Location*/
         str(rec$,110%,04%)  = department$    /* ADP Dept    */
         str(rec$,114%,06%)  = appliedDte$    /* ADP Appl Dte*/
         str(rec$,120%,01%)  = fields$(12%)   /* Shft Diff   */
        gosub writeData

REM ===================================
REM    Clock Out
REM ===================================
clockOut:
        if outDate$ = " " then goto FINI
        init(" ") ent_yr$, ent_bi_yr$, ent_wk$, ent_dy$
        call "AWDEMP1B" (ent_yr$,     /* Entry Production Year (OUT)*/~
                         ent_bi_yr$,  /* Entry Prod Binary Year(OUT)*/~
                         ent_wk$,     /* Entry Prod Week       (OUT)*/~
                         ent_dy$,     /* Entry Production Day  (OUT)*/~
                         outDte$,     /* Entry Production Date (6)  */~
                         outDate$,    /* Entry Production Date (10) */~
                         #2,          /* GENCODES                   */~
                         pl_e%    )   /* 0% = Ok, Not Zero error    */

         if pl_e% <> 0% then goto FINI

         gosub updteOutTmePunch
         gosub checkRecord
         if rec% = 1% then goto FINI
         clockCde$ = "1"
         str(rec$,01%,02%)   = ent_bi_yr$     /* Emp Yr Bin  */
         str(rec$,03%,02%)   = ent_wk$        /* Emp Wk      */
         str(rec$,05%,01%)   = ent_dy$        /* Emp Day     */
         str(rec$,06%,05%)   = badgeID$       /* Emp No      */
         str(rec$,11%,03%)   = mfgDept$       /* Department  */
         str(rec$,14%,05%)   = punchTme$      /* Punch In 24 */
         str(rec$,19%,01%)   = clockCde$      /* Clock Code  */
         str(rec$,20%,02%)   = " "            /* Filler      */
         str(rec$,22%,03%)   = fields$(09%)   /* User ID     */
         str(rec$,25%,06%)   = outDte$        /* Dte Adj for Time Zone */
         str(rec$,31%,08%)   = time           /* System Time */
         str(rec$,39%,06%)   = dte$           /* System Date */
         str(rec$,45%,08%)   = time           /* Time        */
         str(rec$,53%,08%)   = fields$(10%)   /* Payrate     */
         str(rec$,61%,10%)   = jobid$         /* ADP Job ID  */
         str(rec$,71%,20%)   = fields$(11%)   /* ADP EMP ID  */
         str(rec$,91%,10%)   = fields$(02%)   /* ADP EV5 Dept*/
         str(rec$,101%,03%)   = fields$(08%)   /* ADP Pay Grp */
         str(rec$,104%,03%)   = division$      /* ADP Division*/
         str(rec$,107%,03%)   = location$      /* ADP Location*/
         str(rec$,110%,04%)   = department$    /* ADP Dept    */
         str(rec$,114%,06%)  = appliedDte$    /* ADP Appl Dte*/

REM ===================================
REM    Write Data
REM ===================================

writeData:
        put #1, using APCEMPDT_FMT, rec$
        write #1
APCEMPDT_FMT:  FMT CH(128)

        if clockCde$ = "0" then return

        goto FINI



REM ==============================
REM  Column format Routines
REM ==============================



checkRecord
  rec% = 0%
  key$ = all(hex(00))
  str(key$,01%,02%) = ent_bi_yr$
  str(key$,03%,02%) = ent_wk$
  str(key$,05%,01%) = ent_dy$
  str(key$,06%,05%) = badgeID$    
  str(key$,11%,03%) = mfgDept$
  str(key$,14%,05%) = punchTme$

  read #1, key = key$, eod goto noRec

  rec% = 1%
NoRec:
return



convert_date:
   call "DATUFMTC" (field$)
return

updteOutTmePunch
  punchtme% = 0%
  convert fields$(07%) to punchTme%, data goto badOutTime

badOutTime:

  if outDte$ > inDte$ then punchTme% = punchTme% + 2400%

  convert punchTme% to punchTme$, pic(###0)
return

convertDec:
  init(hex(00)) dec$
  colval = 0.0
  convert fields$(10%) to colval, data goto bad_dec
  put dec$, using dec_fmt, colval
dec_fmt:     FMT PD(14,4)
bad_dec:
return

FINI:
        end







        
