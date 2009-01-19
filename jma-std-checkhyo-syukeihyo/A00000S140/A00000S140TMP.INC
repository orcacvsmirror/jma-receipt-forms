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
      ********************************************************************
       01  S140TMP-REC.
              03  S140TMP-KEY.
                  05  S140TMP-NYUGAIKBN       PIC  X(01).
                  05  S140TMP-SRYKA           PIC  X(02).
              03  S140TMP-KOKUHO-TBL OCCURS 14.
                  05  S140TMP-KENSU      PIC  9(04).
                  05  S140TMP-NISSU      PIC  9(04).
                  05  S140TMP-TENSU      PIC  9(09).
      *+
      *            05  S140TMP-MADOGK     PIC  9(09).
                  05  S140TMP-MADOGK     PIC  S9(09).
      *+
      *--- (2008/12/12) line add start monma ---------------------------
                  05  S140TMP-JIHIGK     PIC  9(09).
      *--- (2008/12/12) line add end   monma ---------------------------
                  05  S140TMP-HKNGK      PIC  9(09).
                  05  S140TMP-KOHGK      PIC  9(09).
                  05  S140TMP-SYUEKI     PIC  9(09).
                  05  S140TMP-SNISSU     PIC  9(04).
                  05  S140TMP-SSEIKYU    PIC  9(09).
                  05  S140TMP-SFTNGK     PIC  9(09).
              03  S140TMP-KOH1-MAX       PIC  9(02).
      * 20081014 市町村別の集計を追加START
      *        03  S140TMP-KOHI1-TBL OCCURS 100.
              03  S140TMP-KOHI1-TBL OCCURS 300.
      * 20081014 市町村別の集計を追加END
                  05  S140TMP-KOH1-KEY.
                      07  S140TMP-RJNKBN       PIC  X(03).
                      07  S140TMP-KOHNUM       PIC  X(03).
                  05  S140TMP-KKENSU      PIC  9(04).
                  05  S140TMP-KNISSU      PIC  9(04).
                  05  S140TMP-KTENSU      PIC  9(09).
      * 20081014 市町村別の集計を追加START
      *        03  S140TMP-HKNJA1-TBL OCCURS 100.
              03  S140TMP-HKNJA1-TBL OCCURS 300.
      *            05  S140TMP-HKNJANUM    PIC  X(06).
                  05  S140TMP-HKNJANUM    PIC  X(08).
      * 20081014 市町村別の集計を追加END
                  05  S140TMP-HKENSU      PIC  9(04).
                  05  S140TMP-HNISSU      PIC  9(04).
                  05  S140TMP-HTENSU      PIC  9(09).
      *+
      *            05  S140TMP-HMADOGK     PIC  9(09).
                  05  S140TMP-HMADOGK     PIC  S9(09).
      *+
      *--- (2008/12/12) line add start monma ---------------------------
                  05  S140TMP-HJIHIGK     PIC  9(09).
      *--- (2008/12/12) line add end   monma ---------------------------
                  05  S140TMP-HHKNGK      PIC  9(09).
                  05  S140TMP-HKOHGK      PIC  9(09).
                  05  S140TMP-HSYUEKI     PIC  9(09).
                  05  S140TMP-HSNISSU     PIC  9(04).
                  05  S140TMP-HSSEIKYU    PIC  9(09).
                  05  S140TMP-HSFTNGK     PIC  9(09).
              03  S140TMP-KUMIAI-TBL OCCURS 11.
                  05  S140TMP-K-KENSU      PIC  9(04).
                  05  S140TMP-K-NISSU      PIC  9(04).
                  05  S140TMP-K-TENSU      PIC  9(09).
      *+
      *            05  S140TMP-K-MADOGK     PIC  9(09).
                  05  S140TMP-K-MADOGK     PIC  S9(09).
      *+
      *--- (2008/12/12) line add start monma ---------------------------
                  05  S140TMP-K-JIHIGK     PIC  9(09).
      *--- (2008/12/12) line add end   monma ---------------------------
                  05  S140TMP-K-HKNGK      PIC  9(09).
                  05  S140TMP-K-KOHGK      PIC  9(09).
                  05  S140TMP-K-SYUEKI     PIC  9(09).
                  05  S140TMP-K-SNISSU     PIC  9(04).
                  05  S140TMP-K-SSEIKYU    PIC  9(09).
                  05  S140TMP-K-SFTNGK     PIC  9(09).
              03  S140TMP-KOH2-MAX       PIC  9(02).
      * 20081014 市町村別の集計を追加START
      *        03  S140TMP-KOHI2-TBL OCCURS 100.
              03  S140TMP-KOHI2-TBL OCCURS 300.
      * 20081014 市町村別の集計を追加END
                  05  S140TMP-KOH2-KEY.
                      07  S140TMP-K-RJNKBN       PIC  X(03).
                      07  S140TMP-K-KOHNUM       PIC  X(03).
                  05  S140TMP-K-KKENSU      PIC  9(04).
                  05  S140TMP-K-KNISSU      PIC  9(04).
                  05  S140TMP-K-KTENSU      PIC  9(09).
      *+
      *            05  S140TMP-K-KMADOGK     PIC  9(09).
                  05  S140TMP-K-KMADOGK     PIC S9(09).
      *+
      *--- (2008/12/12) line add start monma ---------------------------
                  05  S140TMP-K-KJIHIGK     PIC  9(09).
      *--- (2008/12/12) line add end   monma ---------------------------
                  05  S140TMP-K-KHKNGK      PIC  9(09).
                  05  S140TMP-K-KKOHGK      PIC  9(09).
                  05  S140TMP-K-KSYUEKI     PIC  9(09).
      * 20081014 市町村別の集計を追加START
      *        03  S140TMP-HKNJA2-TBL OCCURS 100.
              03  S140TMP-HKNJA2-TBL OCCURS 300.
      *            05  S140TMP-K-HKNJANUM    PIC  X(06).
                  05  S140TMP-K-HKNJANUM    PIC  X(08).
      * 20081014 市町村別の集計を追加END
                  05  S140TMP-K-HKENSU      PIC  9(04).
                  05  S140TMP-K-HNISSU      PIC  9(04).
                  05  S140TMP-K-HTENSU      PIC  9(09).
      *+
      *            05  S140TMP-K-HMADOGK     PIC  9(09).
                  05  S140TMP-K-HMADOGK     PIC S9(09).
      *+
      *--- (2008/12/12) line add start monma ---------------------------
                  05  S140TMP-K-HJIHIGK     PIC  9(09).
      *--- (2008/12/12) line add end   monma ---------------------------
                  05  S140TMP-K-HHKNGK      PIC  9(09).
                  05  S140TMP-K-HKOHGK      PIC  9(09).
                  05  S140TMP-K-HSYUEKI     PIC  9(09).
                  05  S140TMP-K-HSNISSU     PIC  9(04).
                  05  S140TMP-K-HSSEIKYU    PIC  9(09).
                  05  S140TMP-K-HSFTNGK     PIC  9(09).
              03  S140TMP-GOKEI-TBL.
                  05  S140TMP-GKENSU      PIC  9(04).
                  05  S140TMP-GNISSU      PIC  9(04).
                  05  S140TMP-GTENSU      PIC  9(09).
      *+
      *            05  S140TMP-GMADOGK     PIC  9(09).
                  05  S140TMP-GMADOGK     PIC  S9(09).
      *+
      *--- (2008/12/12) line add start monma ---------------------------
                  05  S140TMP-GJIHIGK     PIC  9(09).
      *--- (2008/12/12) line add end   monma ---------------------------
                  05  S140TMP-GHKNGK      PIC  9(09).
                  05  S140TMP-GKOHGK      PIC  9(09).
                  05  S140TMP-GSYUEKI     PIC  9(09).
                  05  S140TMP-SGNISSU     PIC  9(04).
                  05  S140TMP-SGSEIKYU    PIC  9(09).
                  05  S140TMP-SGFTNGK     PIC  9(09).
