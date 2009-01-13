        01  HCN03V02.
            02  HCN03V02-TYPE.
              03  HCN03V02-NO             PIC X(7).
              03  HCN03V02-NYUINSTRYMD    PIC X(22).
              03  HCN03V02-NYUINENDYMD    PIC X(22).
              03  HCN03V02-HAKYMD         PIC X(22).
              03  HCN03V02-KNJKANANAME    PIC X(50).
              03  HCN03V02-KNJNAMW        PIC X(30).
              03  HCN03V02-KNJNO          PIC X(20).
              03  HCN03V02-ROOM           PIC X(50).
              03  HCN03V02-HOKEN          PIC X(20).
              03  HCN03V02-FUTAN  PIC X(6).
              03  HCN03V02-MIDASI PIC X(20).
              03  HCN03V02-MEISAI
      * 20080317 ïaóùêfífÇÃí«â¡START tanaka
      *                PIC X(20)   OCCURS  15 TIMES.
                      PIC X(20)   OCCURS  16 TIMES.
      * 20080317 ïaóùêfífÇÃí«â¡END
      *--- 2008/10/22  ADD  START MONMA --------------------------------
              03  HCN03V02-HONKZKKBN  PIC X(04).
              03  HCN03V02-TANJYOYMD  PIC X(24).
              03  HCN03V02-SEX        PIC X(02).
      *--- 2008/10/22  ADD  END MONMA ----------------------------------
              03  HCN03V02-HKNRYO
      * 20080317 ïaóùêfífÇÃí«â¡START tanaka
      *                PIC X(18)   OCCURS  16 TIMES.
                      PIC X(18)   OCCURS  17 TIMES.
      * 20080317 ïaóùêfífÇÃí«â¡END
              03  HCN03V02-JIHIBUN
      * 20080317 ïaóùêfífÇÃí«â¡START tanaka
      *                PIC X(18)   OCCURS  15 TIMES.
                      PIC X(18)   OCCURS  16 TIMES.
      * 20080317 ïaóùêfífÇÃí«â¡END
              03  HCN03V02-SKJRYO     PIC X(18).
              03  HCN03V02-LIFERYO    PIC X(18).
              03  HCN03V02-HOKENBUN   PIC X(18).
              03  HCN03V02-HOKENGAI   PIC X(18).
              03  HCN03V02-HOKENZEI   PIC X(18).
              03  HCN03V02-JIHI
                      PIC X(18)   OCCURS  12 TIMES.
              03  HCN03V02-JIHIMSG
                      PIC X(14)   OCCURS  10 TIMES.
              03  HCN03V02-SYOKUJI    PIC X(18).
              03  HCN03V02-ROUJIN     PIC X(18).
              03  HCN03V02-KOUHI      PIC X(18).
              03  HCN03V02-RMSAGAKU   PIC X(18).
              03  HCN03V02-RMZEI      PIC X(18).
              03  HCN03V02-CHOSEI     PIC X(18).
              03  HCN03V02-KONSEIKYU  PIC X(18).
              03  HCN03V02-KONZEI     PIC X(18).
              03  HCN03V02-KONNYUKIN  PIC X(18).
              03  HCN03V02-IRYOADRS   PIC X(100).
              03  HCN03V02-IRYOMEI    PIC X(100).
              03  HCN03V02-IRYOTEL    PIC X(15).
      *--- 2008/10/22  ADD  START MONMA --------------------------------
              03  HCN03V02-RSIBUN      PIC X(18).
              03  HCN03V02-RSIEN       PIC X(02).
              03  HCN03V02-RSIMIDASI   PIC X(16).
      *--- 2008/10/22  ADD  END MONMA ----------------------------------
            02  HCN03V02-TYPE-C.
              03  HCN03V02-LBL-G-MISYU    PIC X(20).
              03  HCN03V02-LBL-G-MISYU-EN PIC X(2).
              03  HCN03V02-GAI-MISYUKIN   PIC X(18).
              03  HCN03V02-LBL-N-MISYU    PIC X(20).
              03  HCN03V02-LBL-N-MISYU-EN PIC X(2).
              03  HCN03V02-NYU-MISYUKIN   PIC X(18).
