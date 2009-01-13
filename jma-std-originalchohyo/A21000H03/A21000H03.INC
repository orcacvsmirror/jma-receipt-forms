      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      *  プログラム修正履歴
      *              @@@@-高嶋    06/11/22  基本診療項目追加対応
      *              @@@@-田中    08/02/29  再印刷処理の追加
      ********************************************************************
        01  HC03.
            02  HC03-TYPE1.
              03  HC03-NO PIC X(14).
              03  HC03-HAKYYYY    PIC X(4).
              03  HC03-HAKMM  PIC X(2).
              03  HC03-HAKDD  PIC X(2).
              03  HC03-KNJNO  PIC X(20).
              03  HC03-HKNJANO    PIC X(16).
              03  HC03-KNJNAMW    PIC X(20).
      * 20080229 再印刷処理の追加START t.tanaka
      *        03  HC03-HKNRYO1    PIC X(14).
              03  HC03-HKNRYO1    PIC X(07).
      * 20080229 再印刷処理の追加END
              03  HC03-HKNRYO2    PIC X(14).
              03  HC03-HKNRYO3    PIC X(14).
              03  HC03-HKNRYO4    PIC X(14).
              03  HC03-HKNRYO5    PIC X(14).
              03  HC03-HKNRYO6    PIC X(14).
      * 20080229 再印刷処理の追加START t.tanaka
      *        03  HC03-HKNRYO7    PIC X(14).
              03  HC03-HKNRYO7    PIC X(07).
      * 20080229 再印刷処理の追加END
              03  HC03-HKNRYO8    PIC X(14).
              03  HC03-HKNRYO9    PIC X(14).
              03  HC03-HKNRYO10   PIC X(14).
              03  HC03-HKNRYO11   PIC X(14).
              03  HC03-HKNRYO12   PIC X(14).
      * 20080229 再印刷処理の追加START t.tanaka
      *        03  HC03-HKNRYO13    PIC X(14).
              03  HC03-HKNRYO13    PIC X(07).
      * 20080229 再印刷処理の追加END
              03  HC03-GOKTEN PIC X(14).
              03  HC03-FUTAN  PIC X(6).
              03  HC03-FUTANKIN   PIC X(18).
              03  HC03-HOKENGAI   PIC X(18).
              03  HC03-ZENMISYU   PIC X(18).
      * 20080229 再印刷処理の追加START t.tanaka
      *        03  HC03-KONSEIKYU    PIC X(18).
              03  HC03-KONSEIKYU    PIC X(09).
      * 20080229 再印刷処理の追加START t.tanaka
              03  HC03-KONMISYU   PIC X(18).
              03  HC03-KONNYUKIN  PIC X(18).
              03  HC03-IRYOADRS   PIC X(100).
              03  HC03-IRYOMEI    PIC X(100).
              03  HC03-IRYOTEL    PIC X(30).
            02  HC03-TYPE2.
              03  HC03-B-NO   PIC X(14).
              03  HC03-B-HAKYYYY  PIC X(4).
              03  HC03-B-HAKMM    PIC X(2).
              03  HC03-B-HAKDD    PIC X(2).
              03  HC03-B-KNJNO    PIC X(20).
              03  HC03-B-HKNJANO  PIC X(16).
              03  HC03-B-KNJNAMW  PIC X(20).
      * 20080229 再印刷処理の追加START t.tanaka
      *        03  HC03-B-HKNRYO1  PIC X(14).
              03  HC03-B-HKNRYO1  PIC X(07).
      * 20080229 再印刷処理の追加END
              03  HC03-B-HKNRYO2  PIC X(14).
              03  HC03-B-HKNRYO3  PIC X(14).
              03  HC03-B-HKNRYO4  PIC X(14).
              03  HC03-B-HKNRYO5  PIC X(14).
              03  HC03-B-HKNRYO6  PIC X(14).
      * 20080229 再印刷処理の追加START t.tanaka
      *        03  HC03-B-HKNRYO7  PIC X(14).
              03  HC03-B-HKNRYO7  PIC X(07).
      * 20080229 再印刷処理の追加END
              03  HC03-B-HKNRYO8  PIC X(14).
              03  HC03-B-HKNRYO9  PIC X(14).
              03  HC03-B-HKNRYO10 PIC X(14).
              03  HC03-B-HKNRYO11 PIC X(14).
              03  HC03-B-HKNRYO12 PIC X(14).
      * 20080229 再印刷処理の追加START t.tanaka
      *        03  HC03-B-HKNRY13  PIC X(14).
              03  HC03-B-HKNRY13  PIC X(07).
      * 20080229 再印刷処理の追加END
              03  HC03-B-GOKTEN   PIC X(14).
              03  HC03-B-FUTAN    PIC X(6).
              03  HC03-B-FUTANKIN PIC X(18).
              03  HC03-B-HOKENGAI PIC X(18).
              03  HC03-B-ZENMISYU PIC X(18).
      * 20080229 再印刷処理の追加START t.tanaka
      *        03  HC03-B-KONSEIKYU    PIC X(18).
              03  HC03-B-KONSEIKYU    PIC X(09).
      * 20080229 再印刷処理の追加START t.tanaka
              03  HC03-B-KONMISYU PIC X(18).
              03  HC03-B-KONNYUKIN    PIC X(18).
              03  HC03-B-IRYOADRS PIC X(100).
              03  HC03-B-IRYOMEI  PIC X(100).
              03  HC03-B-IRYOTEL  PIC X(30).
