        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - ORADFLY                              *~
            *  Creation Date     - 06/20/2020                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Norman                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *06/20/2020! Original                                 ! CMN *~
            *************************************************************/


        sub "ORADFLY"  (#1,              /* APCDFLY                    */~
                        jobid$,                                         ~
                        trans$,                                         ~
                        action$,                                        ~
                        file$,                                          ~
                        division$,                                         ~
                        location$,                                      ~
                        department$,                                    ~
                        mfgDept$,                                       ~
                        fields$(),                                      ~
                        error%)          /* Error Flag from File Open  */

        dim jobid$10,                    /* Job ID                     */~
            trans$,                      /* Transaction Number         */~
            key$100,                     /* Read Key                   */~
            action$1,                    /* Record Action              */~
            file$,                       /* File Name                  */~
            division$3,                  /* Division Number            */~
            location$6,                  /* Location Number            */~
            department$6,                /* Corporate Department       */~
            mfgDept$3,                   /* Mfg Department             */~
            badgeID$5,                   /* Badge ID                   */~
            newbadge$8,                  /* New Badge ID format        */~
            field$256,                   /* Query Return Field         */~
            fields$(100)64,                                              ~
            rec$(4)256,                  /* APCEMPLY file record       */~
            blankdte$6,                  /* Blank Date                 */~
            bin$2,                       /* Binary 0                   */~
            zerobin$2,                   /* Binary 0                   */~
            dec$8,                       /* Packed Decimal 0.00        */~
            zerodec$8,                   /* Packed Decimal 0.00        */~
            shftDiff$8,                  /* DF Shift Differential      */~
            shft$2,                      /* Employee Shift             */~
            shftInfo$25,                 /* Shift Info                 */~
            table$9,                     /* Table To Read              */~
            genkey$15,                   /* GENCODES Key to Read       */~
            descr1$30,                   /* Description                */~
            codeLen$2,                   /* Code Length                */~
            descr2$30,                   /* Description                */~
            descr3$30                    /* Description                */                  

            error% = 0%
            init(" ") table$, genkey$, descr1$, codeLen$, descr2$, descr3$, ~
                      shftDiff$
            payrate = 0.00
            init(hex(00)) zerobin$, zerodec$
            gosub setDefaults
            zerobin$ = bin$
            zerodec$ = dec$
            if action$   = "A" then goto addRecord
            if action$   = "C" then goto changeRecord
REM IF ACTION$   = "D" THEN GOTO DELETERECORD
            goto FINI



REM ==========
REM   add 5% to fields$() = oe_dfbuf field number
REM ==========
changeRecord:
addRecord:
REM KEY$ = FIELDS$(01%) 20170918
            init(" ") key$, badgeID$, newbadge$
            newbadge% = 0%
REM            badgeID$ = fields$(01%)
REM            key$ = badgeID$

L12345:         FMT BI(4)
            newbadge$ = fields$(01%)
            convert newbadge$ to newbadge%, data goto L00001
            put str(key$,1%,4%), using L12345, newbadge%
            str(key$,5%,1%) = " "
L00001:
            read #1, key = key$, hold, eod goto createRec
              get #1, using PAYRATE_FMT, payrate
             delete #1
             goto createRec

PAYRATE_FMT:  FMT POS(250), PD(14,4)

createRec:

REM FOR L% = 1% TO NO_FIELDS%          /* CLEAR NULLS */
REM  IF FIELDS$(L%) = "NULL" THEN FIELDS$(L%) = "    "
REM NEXT L%
            gosub convertShift
            
            goto createRec1
            
REM            init(" ") field$
REM            field$ = fields$(12%)         /* Birthdate */
REM            gosub convert_date
REM            fields$(12%) = field$

REM            init(" ") field$
REM            field$ = fields$(17%)         /* Hire Date */
REM            gosub convert_date
REM            fields$(17%) = field$

REM            init(" ") field$
REM            field$ = fields$(18%)         /* Term Date */
REM            gosub convert_date
REM            fields$(18%) = field$
            
           
REM            INIT(" ") SHFTDIFF$           /* SHIFT DIFF */
REM            COLVAL, SHFTDIFF = 0.00
REM            TABLE$ = "EMPSHFDIF"
REM            GENKEY$ = DIVISION$ & "0" & STR(FIELDS$(27%),1%,1%)
REM            GENKEY$ = DIVISION$ & "0" & STR(FIELDS$(27%),1%,1%)
REM            GOSUB GENREAD
REM              IF GENERR% = 0% THEN SHFTDIFF$ = STR(DESCR1$,1%,8%)
REM              CONVERT SHFTDIFF$ TO COLVAL, DATA GOTO CONVERTDIFF
              
REM CONVERTDIFF            
REM            GOSUB CONVERTDEC
REM            SHFTDIFF = COLVAL
REM            SHFTDIFF$ = DEC$              
            
createRec1:            
            colval = 0.0

            if payrate > 9999 then payrate = 0.0
            
REM         CONVERT FIELDS$(26%) TO COLVAL, DATA GOTO CONVERTRATE
            convert fields$(25%) to colval, data goto convertRate
            
convertRate:                              /* Pay Rate */
REM IF COLVAL <= 0 THEN COLVAL = PAYRATE
REM COLVAL = COLVAL + SHFTDIFF
REM            IF COLVAL > 0 THEN COLVAL = COLVAL + SHFTDIFF ~
                          ELSE COLVAL = PAYRATE

            gosub convertDec
            fields$(25%) = dec$            

            init(" ") rec$()
REM STR(REC$(),01%,02%)  = FIELDS$(24%)   20170918/* EMP DEPARTMENT*/
            str(rec$(),01%,03%)  = mfgDept$       /* Emp Department*/
            str(rec$(),04%,03%)  = "   "          /* Emp Pay Grade */
            str(rec$(),07%,04%)  = str(key$,01%,04%)  
            str(rec$(),11%,01%)  = " "
            str(rec$(),12%,15%)  = str(fields$(02%),1%,15%)   /*Last Name*/
            str(rec$(),27%,10%)  = str(fields$(03%),1%,10%)   /*First Name*/
            str(rec$(),37%,01%)  = str(fields$(04%),1%,1%)    /*Middle Init*/
REM            str(rec$(),38%,30%)  = fields$(05%)   /* Address 1     */
REM            str(rec$(),68%,30%)  = fields$(06%)   /* Address 2     */
REM            str(rec$(),98%,18%)  = fields$(07%)   /* City          */
REM            str(rec$(),116%,02%) = fields$(08%)   /* State         */
REM            str(rec$(),118%,09%) = str(fields$(09%),1%,5%) /* Zip  */
REM            str(rec$(),127%,03%) = fields$(10%)   /* Home Area Cd  */
REM            str(rec$(),130%,07%) = fields$(11%)   /* Home Phone    */
REM            str(rec$(),137%,06%) = fields$(12%)   /* Birthdate  !! */

            str(rec$(),152%,01%) = fields$(13%)   /* Status        */
REM            str(rec$(),154%,30%) = fields$(14%)   /* Emerg Cont    */
REM            str(rec$(),184%,03%) = fields$(15%)   /* ECont AreaCd  */
REM            str(rec$(),187%,07%) = fields$(16%)   /* EContHomePhn  */
REM            str(rec$(),194%,06%) = fields$(17%)   /* Hire Date  !! */
REM            str(rec$(),200%,06%) = fields$(18%)   /* Term Date  !! */
REM            STR(REC$(),250%,08%) = FIELDS$(26%)   /* PAY RATE      */
            str(rec$(),250%,08%) = fields$(25%)   /* Pay Rate      */
REM            str(rec$(),294%,40%) = fields$(25%)   /* Pay Rule ???  */          
/* cols 294-333 overwritten in completeAPCEMPLT                    */
REM            str(rec$(),738%,06%) = fields$(19%)   /* Job Code      */

            str(rec$(),744%,04%) = str(department$,3%,4%)  /* CorpDept*/
 
REM            str(rec$(),773%,03%) = fields$(22%)   /* CompCde PayGrp*/
REM            str(rec$(),776%,03%) = location$      /* location      */
               str(rec$(),779%,03%) = division$      /* division      */
REM         STR(REC$(),782%,20%) = FIELDS$(23%)   /* EMP ID        */
REM            str(rec$(),782%,20%) = fields$(24%)   /* emp ID        */
	     
            str(rec$(),802%,10%) = jobid$         /* Job ID        */
REM            str(rec$(),812%,10%) = fields$(20%)   /* ev5 dept      */
REM            str(rec$(),822%,02%) = fields$(27%)   /* shift dif     */
            str(rec$(),824%,08%) = shftDiff$      /* shift dif rate*/

            str(rec$(),841%,02%) = shft$          /* home shift    */
REM            str(rec$(),844%,01%) = fields$(25%)   /* Emp Type E,N,H*/
REM         STR(REC$(),853%,40%) = SHFTINFO$      /* SHIFT INFO    */
            str(rec$(),853%,40%) = shft$ & "-" & shftInfo$ /* Shift*/
            str(rec$(),893%,40%) = fields$(20%)   /*DF CHAR20 Dept*/
REM            str(rec$(),933%,40%) = fields$(23%)   /* Pay Rule      */
REM            str(rec$(),973%,40%) = fields$(24%)   /* ADP EMPID     */
            
REM==================================================================
REM Clean up reminder of APCEMPLY RECORD
REM==================================================================
            gosub completeAPCEMPLY


            put #1, using APCEMPLY_FMT, rec$()
            write #1

            goto FINI
            
APCEMPLY_FMT:  FMT 4*CH(256)            

REM==================================================================
REM completeAPCEMPLY
REM==================================================================
completeAPCEMPLY
  str(rec$(),143%,09%) = " "
  str(rec$(),153%,01%) = " "
  str(rec$(),206%,06%) = blankdte$
  str(rec$(),212%,06%) = blankdte$
  str(rec$(),218%,02%) = zerobin$
  str(rec$(),220%,06%) = blankdte$
  str(rec$(),226%,02%) = zerobin$
  str(rec$(),228%,06%) = blankdte$
  str(rec$(),234%,02%) = zerobin$
  str(rec$(),236%,06%) = blankdte$
  str(rec$(),242%,02%) = zerobin$
  str(rec$(),244%,06%) = blankdte$
  str(rec$(),258%,06%) = blankdte$
  str(rec$(),264%,08%) = zerodec$
  str(rec$(),272%,06%) = blankdte$
  str(rec$(),278%,06%) = blankdte$
  str(rec$(),284%,08%) = zerodec$
  str(rec$(),292%,08%) = zerodec$
  str(rec$(),300%,08%) = zerodec$
  str(rec$(),308%,08%) = zerodec$
  str(rec$(),316%,06%) = blankdte$
  str(rec$(),322%,416%) = " "    /* E_WK_HRS() - turn to filler not used */
  str(rec$(),748%,09%) = " "
  str(rec$(),757%,02%) = zerobin$
  str(rec$(),759%,06%) = blankdte$
  str(rec$(),765%,02%) = zerobin$
  str(rec$(),767%,06%) = blankdte$
  str(rec$(),832%,09%) = " "
  str(rec$(),1013%,12%) = " "
return

REM ==============================
REM  Column format Routines
REM ==============================
setDefaults
  colval% = 0%
  gosub convertBinary

  colval = 0.0
  gosub convertDec

  init(" ") field$
  field$ = blankdte$            /* BlankDate */
  gosub convert_date
  blankdte$ = field$
return

convert_date:
   call "DATUFMTC" (field$)
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

convertShift
REM 01-1st Shift
REM p% = 3%
  init(" ") shft$, shftInfo$
  shift% = 0%
  p%,q% = 0%
  p% = pos(fields$(21%) = "-" )
   if p% = 0% then goto badShift

   q% = p% - 1%
   convert str(fields$(21%),1%,q%) to shift%,data goto badShift

badShift:

   q% = p% + 1%
   convert shift% to shft$, pic(00)
   if p% = 0% then shftInfo$ = fields$(21%)                ~
              else shftInfo$ = str(fields$(21%),q%,(25-q%))
return


genRead
  init(" ") codeLen$, descr1$, descr2$, descr3$
  generr% = 0%
  call "GENREAD" (table$, genkey$, descr1$, codeLen$,       ~
                                 descr2$, descr3$, generr%)
return

FINI:
        end





