        REM *************************************************************~
            *  (APCTIME) - Calculate the Elapsed Time for an Event.     *~
            *                                                           *~
            *      ( As Of 11/17/97 - RHH Check for R6.04.03 )          *~
            *************************************************************

            sub "APCTIME" (s_t$,         /* Starting Time of Event     */~
                           e_t$,         /* Ending Time of Event       */~
                           tt$,          /* Elapsed Time - xx-xx-xx.xx */~
                           elapse$)      /* Display Text Results       */

           dim elapse$35, s_t$8, e_t$8, tt$11
           init(" ") elapse$, tt$

           sh, sm, ss, eh, em, es, elapse = 0.0
           convert str(s_t$,1%,2%) to sh, data goto L00170
L00170:
           convert str(s_t$,3%,2%) to sm, data goto L00190
L00190:
           convert str(s_t$,5%,4%) to ss, data goto L00210
L00210:
L00220:    convert str(e_t$,1%,2%) to eh, data goto L00220

           convert str(e_t$,3%,2%) to em, data goto L00250
L00250:
           convert str(e_t$,5%,4%) to es, data goto L00290
           ss = round(ss/100.0,2)
           es = round(es/100.0,2)
L00290:    s =  round( ((sh * 60.0) * 60.0) + (sm * 60.0) + ss, 2)
           e =  round( ((eh * 60.0) * 60.0) + (em * 60.0) + es, 2)
           if s > e then e = e + 86400.0
           elapse = round(e - s, 2)
           hours% = int(elapse/3600.0)
           if hours% = 0% then goto L00360
              elapse = round( mod(elapse,3600.0), 2)
L00360:    min% = int(elapse/60.0)
           if min% = 0% then goto L00390
              elapse = round( mod(elapse,60.0), 2)
L00390:    sec = elapse
           elapse$ = "Elapsed Time - HH:XX MM:XX SS:XXXXX"
           convert hours% to str(elapse$,19%,2%), pic(00)
           convert min% to str(elapse$,25%,2%), pic(00)
           convert sec to str(elapse$,31%,5%), pic(00.00)

           tt$ = str(elapse$,19%,2%) & "-" & str(elapse$,25%,2%) & "-" & ~
                 str(elapse$,31%,5%)
        end
