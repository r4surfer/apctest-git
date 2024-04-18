        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - ORAEMPMT                             *~
            *  Creation Date     - 07/27/2017                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Norman                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *07/27/2017! Original                                 ! CMN *~
            *01/29/2019! CR-1821 ADP Increase Emp ID field length ! DES *~
            *03/01/2019! CR-1894 Increase EMP DEPT size to 3 bytes! DES *~
            *03/09/2020! CR-2506 Ignore negative hours            ! RDB *~
            *************************************************************


        sub "ORAEMPMT" (#1,              /* APCEMPMT                   */~
                        #2,              /* GENCODES                   */~
                        #3,              /* ADPEMPMT  ADP              */~                        
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
            file$8,                      /* File Name                  */~
            division$3,                  /* Division Number            */~
            location$3,                  /* Location Number            */~
            department$4,                /* Corporate Department       */~
            mfgDept$3,                   /* Mfg Department             */~
            badgeID$5,                   /* Badge ID                   */~             
            newbadge$8,                  /* New Badge ID format CR2490 */~           
            field$256,                   /* Query Return Field         */~
            fields$(100)64,              /* Oracle Fields              */~
            rec$128,                     /* APCEMPDT file record       */~
            appliedDate$10,              /* Applied Date               */~
            appliedDte$6,                /* Unformatted Applied Date   */~
            wkStartDte$6,                /* Beginning Week Date        */~
            dte$,                        /* System Date                */~
            ent_yr$4,                    /* Entry Production Year      */~
            ent_bi_yr$2,                 /* Entry Prod Binary Year     */~
            ent_wk$2,                    /* Entry Prod Week       (OUT)*/~
            ent_dy$1,                    /* Entry Production Day  (OUT)*/~
            dec$8,                       /* Decimal Number Field       */~
            bin$4,                       /* Binary Number Field        */~
            shftDiff$8,                  /* ADP Shift Differential     */~    
            hrs$2,                       /* Hours                      */~
            min$2,                       /* Minutes                    */~            
            adphrs$2,                    /* ADP Total Hrs              */~
            adpmin$2,                    /* ADP Total Min              */~
            tothrs$2,                    /* Total Hrs                  */~
            totmin$2,                    /* Total Min                  */~
            table$9,                     /* Table To Read              */~
            genkey$15,                   /* GENCODES Key to Read       */~
            descr1$30,                   /* Description                */~
            codeLen$2,                   /* Code Length                */~
            descr2$30,                   /* Description                */~
            descr3$30,                   /* Description                */~
            mt_pay_code$10               /* ADP Pay Code               */~            


        init(" ") ent_yr$, ent_bi_yr$, ent_wk$, ent_dy$, appliedDate$, ~
                  appliedDte$, dte$, bin$, badgeID$, table$, genkey$,  ~
                  descr1$, codeLen$, descr2$, descr3$, shftDiff$,      ~
                  mt_pay_code$, newbadge$
                  
          error%, pl_e%, rec% = 0%
          newbadge% = 0%                        /* CR2490 */
          dte$ = date
/* CR2490 */
L12345:         FMT BI(4)
            newbadge$ = fields$(01%)
            convert newbadge$ to newbadge%, data goto L00001
            put str(badgeID$,1%,4%), using L12345, newbadge%
            str(badgeID$,5%,1%) = " "
REM          badgeID$ = fields$(01%)
L00001:
          
          init(" ") shftDiff$
          colval, shifDiff = 0.00
          table$ = "EMPSHFDIF"
          genkey$ = division$ & "0" & str(fields$(10%),1%,1%)
          gosub genRead
            if genErr% = 0% then shftDiff$ = str(descr1$,1%,8%)
            convert shftDiff$ to colval, data goto convertDiff
          
convertDiff:            
          gosub convertDec
          shifDiff = colval
          shftDiff$ = dec$              
          
          
          colval = 0.0
          convert fields$(09%) to colval, data goto convertRate
        
convertRate:            
          colval = colval + shifDiff
          gosub convertDec
          fields$(09%) = dec$
                  
          
          gosub convertADPHrs
          
REM ================================
REM   Convert Dates
REM ================================

        appliedDate$  = fields$(03%)
        field$        = fields$(03%)
        gosub convert_date
        appliedDte$   = field$
        call "AWDEMP1B" (ent_yr$,     /* Entry Production Year (OUT)*/~
                         ent_bi_yr$,  /* Entry Prod Binary Year(OUT)*/~
                         ent_wk$,     /* Entry Prod Week       (OUT)*/~
                         ent_dy$,     /* Entry Production Day  (OUT)*/~
                         appliedDte$, /* Entry Production Date (6)  */~
                         appliedDate$,/* Entry Production Date (10) */~
                         #2,          /* GENCODES                   */~
                         pl_e%    )   /* 0% = Ok, Not Zero error    */
                         
                         

        wkStartDte$ = appliedDte$
        if ent_dy$ <> "1" then gosub updateStartDte                         
        if pl_e% <> 0% then goto FINI

        mt_pay_code$ = fields$(06%)
REM ===================================
REM    APCEMPMT Record
REM ===================================
        key$, rec$ = all(hex(00))
        str(key$,01%,03%) = mfgDept$
        str(key$,04%,02%) = ent_bi_yr$ 
        str(key$,06%,05%) = badgeID$
        str(key$,11%,02%) = ent_wk$
        str(key$,13%,01%) = ent_dy$
         
        read #1, hold, key = key$, using APCEMPMT_FMT, rec$, ~
                                                  eod goto addRec
            rec% = 1%
            gosub updateRec              
            delete #1
            goto writeUpdtRec
            
addRec:

REM CALL "SHOSTAT" ("ADD REC MT")
REM STOP
REM GOSUB CONVERTORGINALHRS
REM TOTHRS$ = FIELDS$(04%)
REM TOTMIN$ = FIELDS$(05%)

        gosub updateRec

        
REM ===================================
REM    Write Data
REM ===================================
writeUpdtRec:        

        str(rec$,01%,03%)  = mfgDept$       /* Mfg Depart    */
        str(rec$,04%,02%)  = ent_bi_yr$     /* Punch BI Yr   */
        str(rec$,06%,05%)  = badgeID$       /* Emp No        */
        str(rec$,11%,02%)  = ent_wk$        /* Punch Week    */
        str(rec$,13%,01%)  = ent_dy$        /* Punch Day     */
        str(rec$,14%,06%)  = wkStartDte$    /* Begin Wk Dte  */
        str(rec$,20%,02%)  = tothrs$        /* Hours         */
        str(rec$,22%,02%)  = totmin$        /* Mins          */
        str(rec$,24%,01%)  = "0"            /* Var In        */
        str(rec$,25%,01%)  = "0"            /* Var Out       */
        str(rec$,26%,01%)  = "0"            /* Var Day       */
        str(rec$,27%,01%)  = "0"            /* Proc Code     */
        str(rec$,28%,03%)  = "ADP"          /* User ID       */
        str(rec$,31%,10%)  = mt_pay_code$   /* ADP Pay Code  */
        str(rec$,41%,10%)  = fields$(07%)   /* ADP Paid      */
        str(rec$,42%,10%)  = jobid$         /* ADP JOBID     */
        str(rec$,52%,20%)  = fields$(08%)   /* ADP EMPID     */
        str(rec$,72%,10%)  = fields$(02%)   /* ADP EV5 Dept  */
        str(rec$,82%,06%)  = appliedDte$    /* ADP Appl Dte  */
        str(rec$,88%,08%)  = fields$(09%)   /* Pay Rate      */
        str(rec$,96%,02%)  = fields$(10%)   /* Shift Diff    */
        str(rec$,98%,08%)  = shftDiff$      /* shift dif rate*/



        put #1, using APCEMPMT_FMT, rec$
        write #1
            
APCEMPMT_FMT:  FMT CH(128)
        
        
REM ===================================
REM    ADPEMPMT Record
REM ===================================
        key$, rec$ = all(hex(00))
        str(key$,01%,03%) = mfgDept$
        str(key$,04%,02%) = ent_bi_yr$ 
        str(key$,06%,02%) = ent_wk$
        str(key$,08%,05%) = badgeID$
        str(key$,13%,01%) = ent_dy$
        str(key$,14%,10%) = mt_pay_code$
         
        read #3, hold, key = key$, using ADPEMPMT_FMT, rec$,        ~
                                                  eod goto addADPRec
            delete #3

addADPRec:
        str(rec$,01%,06%)   = wkStartDte$    /* Begin Wk Dte  */
        str(rec$,07%,03%)   = mfgDept$       /* Mfg Depart    */
        str(rec$,10%,02%)   = ent_bi_yr$     /* Punch BI Yr   */
        str(rec$,12%,02%)   = ent_wk$        /* Punch Week    */
        str(rec$,14%,05%)   = badgeID$       /* Emp No        */
        str(rec$,19%,01%)   = ent_dy$        /* Punch Day     */
        str(rec$,20%,10%)   = mt_pay_code$   /* Pay Code      */
        str(rec$,30%,03%)   = mfgDept$       /* Mfg Depart    */
        str(rec$,33%,02%)   = adphrs$        /* HRS           */
        str(rec$,35%,02%)   = adpmin$        /* MINS          */
        str(rec$,37%,03%)   = "ADP"          /* User ID       */
        str(rec$,40%,10%)   = fields$(07%)   /* ADP Paid      */
        str(rec$,50%,10%)   = jobid$         /* ADP JOB ID    */
        str(rec$,60%,20%)   = fields$(08%)   /* ADP EMPID     */
        str(rec$,80%,10%)   = fields$(02%)   /* ADP EV5 Dept  */
        str(rec$,90%,06%)   = appliedDte$    /* ADP Applied Dt*/
        str(rec$,96%,08%)   = fields$(09%)   /* ADP Pay Rate  */
        str(rec$,104%,02%)   = fields$(10%)   /* ADP Shft Diff */
        str(rec$,106%,08%)   = shftDiff$      /* ADP SftDiffR  */
        

REM ===================================
REM    Write Data
REM ===================================

        put #3, using ADPEMPMT_FMT, rec$
        write #3
            
ADPEMPMT_FMT:  FMT CH(128)

        goto FINI

REM =====================================
REM  Update Employee Total Time for a Day
REM =====================================
            
updateRec  
  init(" ") hrs$, min$, tothrs$, totmin$
  addHrs%, hrs%, addMin%, min%, totHrs%, totmin% = 0%
   
REM===========================================================
REM If same JobID and *rg hours something must have ran twice 
REM===========================================================
REM IF JOBID$ = STR(REC$,41%,10%) AND STR(REC$,41%,3%) = "*RG" THEN GOTO ZEROHRS
REM IF STR(REC$,30%,3%) = "*RG" THEN GOTO ZEROHRS
   if str(fields$(06%),1%,3%) = "*rg" then goto zeroHrs
   if str(fields$(06%),1%,3%) = "*OT" and jobid$ <> str(rec$,41%,10%)  ~
                                                    then goto zeroHrs
   if str(fields$(06%),1%,3%) = "*OT" and str(rec$,30%,3%) = "*OT"     ~
                                                    then goto zeroHrs
   
    get str(rec$,20%,02%) using BI_FMT, hrs%
    get str(rec$,22%,02%) using BI_FMT, min%
BI_FMT:   FMT BI(02)


 
zeroHrs:  
    hrs$ = fields$(04%)
    convert fields$(04%) to addHrs%, data goto badAddHrs
    
badAddHrs:  

    min$ = fields$(05%)
    convert fields$(05%) to addMin%, data goto badAddMins
  
badAddMins:  
/* CR2506 */
    if hrs% < 0% then hrs% = 0%
    if min% < 0% then min% = 0%
    if addHrs% < 0% then addHrs% = 0%
    if addMin% < 0% then addMin% = 0%   
    if hrs% = 0% and min% = 0% and addHrs% = 0% and addMin% = 0% then goto FINI
    
    hrs%  = (hrs% + addHrs%) * 60
    min% = min% + addMin%
    totHrs%  = int((hrs% + min%) / 60%)
    totmin% = mod((hrs% + min%),60%) 
   
    if totHrs% > 100 then goto resetHrs
    
    colval% = totHrs%
    gosub convertBinary
    tothrs$ = bin$
  
    colval% = totmin%
    gosub convertBinary
    totmin$ = bin$
return
resetHrs
  init(" ") hrs$, min$, tothrs$, totmin$
  addHrs%, hrs%, addMin%, min%, totHrs%, totmin% = 0%
goto zeroHrs

REM ==============================
REM  Column format Routines
REM ==============================
updateStartDte:
  day%, err% = 0%
  convert ent_dy$ to day%, data goto badDay

  day% = ((day% -1%) * -1)
  call "DATE" addr("G+",appliedDte$,day%,wkStartDte$,err%)

badDay
return

convertBinary:
  init(hex(00)) bin$
  put bin$, using bin_fmt, colval%
bin_fmt:      FMT BI(02)
return

convertDec:
  init(hex(00)) dec$
  put dec$, using dec_fmt, colval
dec_fmt:     FMT PD(14,4)
return


convert_date:
  call "DATUFMTC" (field$)
return

genRead
  init(" ") codeLen$, descr1$, descr2$, descr3$
  generr% = 0%
  call "GENREAD" (table$, genkey$, descr1$, codeLen$,       ~
                                 descr2$, descr3$, generr%)
return


convertADPHrs
    init(" ") adphrs$, adpmin$
    colval% = 0            
    convert fields$(04%) to colval%, data goto badField4
    gosub convertBinary
    adphrs$ = bin$

badField4:

    colval% = 0
    convert fields$(05%) to colval%, data goto badField5
    gosub convertBinary
    adpmin$ = bin$

badField5:
return
   
FINI:
        end
        
        
        
        

