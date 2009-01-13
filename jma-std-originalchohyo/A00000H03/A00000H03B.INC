        01  HCM03V02B.
            02  HCM03V02B-TYPE-A.
              03  HCM03V02B-NO         PIC X(7).
              03  HCM03V02B-LBLSRYYMD  PIC X(10).
              03  HCM03V02B-SRYYMD     PIC X(50).
              03  HCM03V02B-KNJNAMW    PIC X(30).
              03  HCM03V02B-KNJKANANAME    PIC X(50).
              03  HCM03V02B-KNJNO  PIC X(20).
              03  HCM03V02B-MIDASI PIC X(40).
              03  HCM03V02B-HAKYMD PIC X(22).
              03  HCM03V02B-HOKEN  PIC X(20).
              03  HCM03V02B-FUTAN  PIC X(3).
              03  HCM03V02B-MEISAI
                      PIC X(20)   OCCURS  16 TIMES.
      *--- 2008/10/17  ADD  START --------------------------------------
              03  HCM03V02B-HONKZKKBN  PIC X(04).
              03  HCM03V02B-TANJYOYMD  PIC X(24).
              03  HCM03V02B-SEX        PIC X(02).
      *--- 2008/10/17  ADD  END ----------------------------------------
              03  HCM03V02B-HKNRYO1    PIC X(18).
              03  HCM03V02B-HKNRYO2    PIC X(18).
              03  HCM03V02B-HKNRYO3    PIC X(18).
              03  HCM03V02B-HKNRYO4    PIC X(18).
              03  HCM03V02B-HKNRYO5    PIC X(18).
              03  HCM03V02B-HKNRYO6    PIC X(18).
              03  HCM03V02B-HKNRYO7    PIC X(18).
              03  HCM03V02B-HKNRYO8    PIC X(18).
              03  HCM03V02B-HKNRYO9    PIC X(18).
              03  HCM03V02B-HKNRYO10   PIC X(18).
              03  HCM03V02B-HKNRYO11   PIC X(18).
              03  HCM03V02B-HKNRYO12   PIC X(18).
              03  HCM03V02B-HKNRYO13   PIC X(18).
              03  HCM03V02B-HKNRYO14   PIC X(18).
              03  HCM03V02B-HKNRYO15   PIC X(18).
              03  HCM03V02B-HKNRYO16   PIC X(18).
              03  HCM03V02B-GOKTEN PIC X(18).
              03  HCM03V02B-JIHIBUN
                      PIC X(18)   OCCURS  16 TIMES.
              03  HCM03V02B-HOKENBUN   PIC X(18).
              03  HCM03V02B-HOKENGAI   PIC X(18).
              03  HCM03V02B-HOKENZEI   PIC X(18).
              03  HCM03V02B-JIHIMSG1   PIC X(20).
              03  HCM03V02B-JIHIMSG2   PIC X(20).
              03  HCM03V02B-JIHIMSG3   PIC X(20).
              03  HCM03V02B-JIHIMSG4   PIC X(20).
              03  HCM03V02B-JIHIMSG5   PIC X(20).
              03  HCM03V02B-JIHIMSG6   PIC X(20).
              03  HCM03V02B-JIHIMSG7   PIC X(20).
              03  HCM03V02B-JIHIMSG8   PIC X(20).
              03  HCM03V02B-JIHIMSG9   PIC X(20).
              03  HCM03V02B-JIHIMSG10  PIC X(20).
              03  HCM03V02B-JIHI
                      PIC X(18)   OCCURS  12 TIMES.
              03  HCM03V02B-YAKUZAI    PIC X(18).
              03  HCM03V02B-ROUJIN PIC X(18).
              03  HCM03V02B-KOUHI  PIC X(18).
              03  HCM03V02B-CHOSEI PIC X(18).
              03  HCM03V02B-KONSEIKYU  PIC X(18).
              03  HCM03V02B-SEIKYUZEI  PIC X(18).
              03  HCM03V02B-ZENMISYU   PIC X(18).
              03  HCM03V02B-GOUKEIGAK  PIC X(18).
              03  HCM03V02B-KONNYUKIN  PIC X(18).
              03  HCM03V02B-IRYOADRS   PIC X(100).
              03  HCM03V02B-IRYOMEI    PIC X(100).
              03  HCM03V02B-IRYOTEL    PIC X(15).
              03  HCM03V02B-MIDASHI    PIC X(10).
      *--- 2008/10/17  ADD  START --------------------------------------
              03  HCM03V02B-RSIBUN     PIC X(14).
              03  HCM03V02B-RSIEN      PIC X(02).
              03  HCM03V02B-RSIMIDASI  PIC X(12).
              03  HCM03V02B-RSIMIDASI2  PIC X(02).
              03  HCM03V02B-RSIMIDASI3  PIC X(02).
      *--- 2008/10/17  ADD  END ----------------------------------------
            02  HCM03V02B-TYPE-B.
              03  HCM03V02B-NO         PIC X(7).
              03  HCM03V02B-LBLSRYYMD  PIC X(10).
              03  HCM03V02B-SRYYMD     PIC X(50).
              03  HCM03V02B-KNJNAMW    PIC X(30).
              03  HCM03V02B-KNJKANANAME    PIC X(50).
              03  HCM03V02B-KNJNO  PIC X(20).
              03  HCM03V02B-MIDASI PIC X(40).
              03  HCM03V02B-HAKYMD PIC X(22).
              03  HCM03V02B-HOKEN  PIC X(20).
              03  HCM03V02B-FUTAN  PIC X(3).
              03  HCM03V02B-MEISAI
                      PIC X(20)   OCCURS  16 TIMES.
      *--- 2008/10/17  ADD  START --------------------------------------
              03  HCM03V02B-HONKZKKBN  PIC X(04).
              03  HCM03V02B-TANJYOYMD  PIC X(24).
              03  HCM03V02B-SEX        PIC X(02).
      *--- 2008/10/17  ADD  END ----------------------------------------
              03  HCM03V02B-HKNRYO1    PIC X(18).
              03  HCM03V02B-HKNRYO2    PIC X(18).
              03  HCM03V02B-HKNRYO3    PIC X(18).
              03  HCM03V02B-HKNRYO4    PIC X(18).
              03  HCM03V02B-HKNRYO5    PIC X(18).
              03  HCM03V02B-HKNRYO6    PIC X(18).
              03  HCM03V02B-HKNRYO7    PIC X(18).
              03  HCM03V02B-HKNRYO8    PIC X(18).
              03  HCM03V02B-HKNRYO9    PIC X(18).
              03  HCM03V02B-HKNRYO10   PIC X(18).
              03  HCM03V02B-HKNRYO11   PIC X(18).
              03  HCM03V02B-HKNRYO12   PIC X(18).
              03  HCM03V02B-HKNRYO13   PIC X(18).
              03  HCM03V02B-HKNRYO14   PIC X(18).
              03  HCM03V02B-HKNRYO15   PIC X(18).
              03  HCM03V02B-HKNRYO16   PIC X(18).
              03  HCM03V02B-GOKTEN PIC X(18).
              03  HCM03V02B-JIHIBUN
                      PIC X(18)   OCCURS  16 TIMES.
              03  HCM03V02B-HOKENBUN   PIC X(18).
              03  HCM03V02B-HOKENGAI   PIC X(18).
              03  HCM03V02B-HOKENZEI   PIC X(18).
              03  HCM03V02B-JIHIMSG1   PIC X(20).
              03  HCM03V02B-JIHIMSG2   PIC X(20).
              03  HCM03V02B-JIHIMSG3   PIC X(20).
              03  HCM03V02B-JIHIMSG4   PIC X(20).
              03  HCM03V02B-JIHIMSG5   PIC X(20).
              03  HCM03V02B-JIHIMSG6   PIC X(20).
              03  HCM03V02B-JIHIMSG7   PIC X(20).
              03  HCM03V02B-JIHIMSG8   PIC X(20).
              03  HCM03V02B-JIHIMSG9   PIC X(20).
              03  HCM03V02B-JIHIMSG10  PIC X(20).
              03  HCM03V02B-JIHI
                      PIC X(18)   OCCURS  12 TIMES.
              03  HCM03V02B-YAKUZAI    PIC X(18).
              03  HCM03V02B-ROUJIN PIC X(18).
              03  HCM03V02B-KOUHI  PIC X(18).
              03  HCM03V02B-CHOSEI PIC X(18).
              03  HCM03V02B-KONSEIKYU  PIC X(18).
              03  HCM03V02B-SEIKYUZEI  PIC X(18).
              03  HCM03V02B-ZENMISYU   PIC X(18).
              03  HCM03V02B-GOUKEIGAK  PIC X(18).
              03  HCM03V02B-KONNYUKIN  PIC X(18).
              03  HCM03V02B-IRYOADRS   PIC X(100).
              03  HCM03V02B-IRYOMEI    PIC X(100).
              03  HCM03V02B-IRYOTEL    PIC X(15).
              03  HCM03V02B-MIDASHI    PIC X(10).
      *--- 2008/10/17  ADD  START --------------------------------------
              03  HCM03V02B-RSIBUN     PIC X(14).
              03  HCM03V02B-RSIEN      PIC X(02).
              03  HCM03V02B-RSIMIDASI  PIC X(12).
              03  HCM03V02B-RSIMIDASI2  PIC X(02).
              03  HCM03V02B-RSIMIDASI3  PIC X(02).
      *--- 2008/10/17  ADD  END ----------------------------------------
