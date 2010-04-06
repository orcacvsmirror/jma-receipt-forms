        01  HCM19.
            02  HCM19-FTNJA       PIC X(16).
            02  HCM19-FTNJAN      PIC X(20).
            02  HCM19-JKYSNUM     PIC X(14).
            02  HCM19-JKYSNUMN    PIC X(20).
            02  HCM19-HKNJA       PIC X(16).
            02  HCM19-KIGONUM     PIC X(32).
            02  HCM19-KIGO        PIC X(32).
            02  HCM19-NUM         PIC X(32).
            02  HCM19-BIRTH-G1    PIC X(2).
            02  HCM19-BIRTH-G2    PIC X(2).
            02  HCM19-BIRTH-G3    PIC X(2).
            02  HCM19-BIRTH-G4    PIC X(2).
            02  HCM19-BIRTH-YY    PIC X(4).
            02  HCM19-BIRTH-MM    PIC X(4).
            02  HCM19-BIRTH-DD    PIC X(4).
            02  HCM19-KANANAME    PIC X(40).
            02  HCM19-NAME        PIC X(30).
            02  HCM19-SEX1        PIC X(2).
            02  HCM19-SEX2        PIC X(2).
            02  HCM19-HONKZKKBN1  PIC X(12).
            02  HCM19-HONKZKKBN2  PIC X(12).
            02  HCM19-KOFYY       PIC X(4).
            02  HCM19-KOFMM       PIC X(4).
            02  HCM19-KOFDD       PIC X(4).
            02  HCM19-KIKYY       PIC X(4).
            02  HCM19-KIKMM       PIC X(4).
            02  HCM19-KIKDD       PIC X(4).
            02  HCM19-HOSPADRS    PIC X(80).
            02  HCM19-HOSPNAME    PIC X(80).
            02  HCM19-HOSPTEL     PIC X(15).
            02  HCM19-DRNAME      PIC X(26).
            02  HCM19-FTNJA2      PIC X(16).
            02  HCM19-FTNJA2N     PIC X(20).
            02  HCM19-JKYSNUM2    PIC X(14).
            02  HCM19-JKYSNUM2N   PIC X(20).
            02  HCM19-NAIYO       PIC X(86)   OCCURS 20 TIMES.
            02  HCM19-MENJYO      PIC X(6).
            02  HCM19-BIKOH       PIC X(120)  OCCURS  4 TIMES.
      ****20030616 追加
            02  HCM19-JKYSNUMB    PIC X(20)   OCCURS  2 TIMES.
      ****20030616 追加
      *--- 2010/04/05 ADD START ----------------------------------------
            02  HCM19-HKNINAME       PIC X(16).
            02  HCM19-PREFNUM.
              05  HCM19-PREFNUM-G    PIC X(02)   OCCURS  2 TIMES.
            02  HCM19-TENHYOKBN      PIC X(02).
            02  HCM19-HOSPCD.
              05  HCM19-HOSPCD-G     PIC X(02)   OCCURS  7 TIMES.
      *--- 2010/04/05 ADD END ------------------------------------------
