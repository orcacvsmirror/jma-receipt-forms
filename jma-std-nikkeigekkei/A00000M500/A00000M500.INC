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
      *              @@@@-高嶋    06/10/30  自費、基本診療項目追加対応
      *              @@@@-田中    08/03/26  H20年4月改正対応
      ********************************************************************
       01  HC90.
              03  HC90-SYSYMD        PIC X(22).
              03  HC90-PAGE          PIC X(3).
              03  HC90-SRYYMD        PIC X(22).
              03  HC90-CMSG1         PIC X(60).
              03  HC90-MIDASI        PIC X(10).
              03  HC90-NYUGAIKBN     PIC X(4).
              03  HC90-TSRYKA        PIC X(20).
              03  HC90-TKENSU1       PIC X(20).
              03  HC90-TKENSU2       PIC X(20).
              03  HC90-TKENSU3       PIC X(20).
              03  HC90-THKN1         PIC X(20).
              03  HC90-TRYO          PIC X(20).
              03  HC90-TFUTAN1       PIC X(22).
              03  HC90-TFUTAN2       PIC X(22).
              03  HC90-TFUTAN3       PIC X(20).
              03  HC90-TJIHI-G                   OCCURS  10 TIMES.
                  05  HC90-TJIHI     PIC X(8).
      * 20080326 H20年4月改正対応START tanaka
      *        03  HC90-YOSO-G                    OCCURS  10 TIMES.
              03  HC90-YOSO-G                    OCCURS  11 TIMES.
      *            05  HC90-YOSO-OCC              OCCURS  34 TIMES.
                  05  HC90-YOSO-OCC              OCCURS  35 TIMES.
      * 20080326 H20年4月改正対応END
                      07  HC90-YOSO  PIC X(12).
