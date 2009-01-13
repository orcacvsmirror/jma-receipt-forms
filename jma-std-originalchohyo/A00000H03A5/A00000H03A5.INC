        01  HCM03V02.
            02  HCM03V02-TYPE.
              03  HCM03V02-NO             PIC X(7).
              03  HCM03V02-LBLSRYYMD      PIC X(10).
              03  HCM03V02-SRYYMD         PIC X(50).
              03  HCM03V02-KNJKANANAME    PIC X(50).
              03  HCM03V02-KNJNAMW    PIC X(30).
              03  HCM03V02-KNJNO  PIC X(20).
              03  HCM03V02-MIDASI PIC X(40).
              03  HCM03V02-HAKYMD PIC X(22).
              03  HCM03V02-HOKEN  PIC X(20).
              03  HCM03V02-FUTAN  PIC X(3).
              03  HCM03V02-MEISAI
                      PIC X(20)   OCCURS  16 TIMES.
      *--- 2008/10/21  ADD  START --------------------------------------
              03  HCM03V02-HONKZKKBN  PIC X(04).
              03  HCM03V02-TANJYOYMD  PIC X(24).
              03  HCM03V02-SEX        PIC X(02).
      *--- 2008/10/21  ADD  END ----------------------------------------
              03  HCM03V02-HKNRYO1    PIC X(18).
              03  HCM03V02-HKNRYO2    PIC X(18).
              03  HCM03V02-HKNRYO3    PIC X(18).
              03  HCM03V02-HKNRYO4    PIC X(18).
              03  HCM03V02-HKNRYO5    PIC X(18).
              03  HCM03V02-HKNRYO6    PIC X(18).
              03  HCM03V02-HKNRYO7    PIC X(18).
              03  HCM03V02-HKNRYO8    PIC X(18).
              03  HCM03V02-HKNRYO9    PIC X(18).
              03  HCM03V02-HKNRYO10   PIC X(18).
              03  HCM03V02-HKNRYO11   PIC X(18).
              03  HCM03V02-HKNRYO12   PIC X(18).
              03  HCM03V02-HKNRYO13   PIC X(18).
              03  HCM03V02-HKNRYO14   PIC X(18).
              03  HCM03V02-HKNRYO15   PIC X(18).
              03  HCM03V02-HKNRYO16   PIC X(18).
              03  HCM03V02-GOKTEN PIC X(18).
              03  HCM03V02-JIHIBUN
                     PIC X(18)   OCCURS  16 TIMES.
              03  HCM03V02-HOKENBUN   PIC X(18).
              03  HCM03V02-HOKENGAI   PIC X(18).
              03  HCM03V02-HOKENZEI   PIC X(18).
              03  HCM03V02-JIHIMSG1   PIC X(14).
              03  HCM03V02-JIHIMSG2   PIC X(14).
              03  HCM03V02-JIHIMSG3   PIC X(14).
              03  HCM03V02-JIHIMSG4   PIC X(14).
              03  HCM03V02-JIHIMSG5   PIC X(14).
              03  HCM03V02-JIHIMSG6   PIC X(14).
              03  HCM03V02-JIHIMSG7   PIC X(14).
              03  HCM03V02-JIHIMSG8   PIC X(14).
              03  HCM03V02-JIHIMSG9   PIC X(14).
              03  HCM03V02-JIHIMSG10  PIC X(14).
              03  HCM03V02-JIHI
                      PIC X(18)   OCCURS  12 TIMES.
              03  HCM03V02-YAKUZAI    PIC X(18).
              03  HCM03V02-ROUJIN PIC X(18).
              03  HCM03V02-KOUHI  PIC X(18).
              03  HCM03V02-CHOSEI PIC X(18).
              03  HCM03V02-KONSEIKYU  PIC X(18).
              03  HCM03V02-SEIKYUZEI  PIC X(18).
              03  HCM03V02-ZENMISYU   PIC X(18).
              03  HCM03V02-GOUKEIGAK  PIC X(18).
              03  HCM03V02-KONNYUKIN  PIC X(18).
              03  HCM03V02-IRYOADRS   PIC X(100).
              03  HCM03V02-IRYOMEI    PIC X(100).
              03  HCM03V02-IRYOTEL    PIC X(15).
              03  HCM03V02-MIDASHI    PIC X(10).
      *--- 2008/10/21  ADD  START --------------------------------------
              03  HCM03V02-RSIBUN      PIC X(14).
              03  HCM03V02-RSIEN       PIC X(02).
              03  HCM03V02-RSIMIDASI   PIC X(12).
              03  HCM03V02-RSIMIDASI2  PIC X(02).
              03  HCM03V02-RSIMIDASI3  PIC X(02).
      *--- 2008/10/21  ADD  END ----------------------------------------
