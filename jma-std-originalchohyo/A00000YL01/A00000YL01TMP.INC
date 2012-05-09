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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  Maj/Min/Rev  èCê≥é“  ì˙ït       ì‡óe
      *   01.00.01    ñÂä‘    12/05/01   êVãKçÏê¨
      ******************************************************************
       01  YL01TMP.
           02  YL01TMP-KEY.
             03  YL01TMP-KEY-TBL      OCCURS 5.
               04  YL01TMP-KEY-D        PIC X(64).
             03  YL01TMP-NAME         PIC X(64).
           02  YL01TMP-SRYCD        PIC X(9).
           02  YL01TMP-INPUTCD      PIC X(9).
           02  YL01TMP-YKZKBN       PIC X(2).
           02  YL01TMP-KISAIKBN     PIC X(2).
           02  YL01TMP-GNAME        PIC X(64).
           02  YL01TMP-YAKKA        PIC X(9).
           02  YL01TMP-SAIYAKKA     PIC X(9).
