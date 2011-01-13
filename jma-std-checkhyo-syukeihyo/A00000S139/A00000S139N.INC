      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
      *******************************************************************
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      *  Maj/Min/Rev  ½¤Àµ¼Ô      ÆüÉÕ      ÆâÍÆ
      *   01.00.01    @@@@-ÅÄÃæ   11/01/05  ¥³¥á¥ó¥ÈÍúÎòºï½ü
      *******************************************************************
       01  S139N.
           03  S139N-PAGE                  PIC  X(03).
           03  S139N-SRYYM                 PIC  X(16).
           03  S139N-SYSYMD                PIC  X(22).
           03  S139N-SRYKANAME             PIC  X(20).
           03  S139N-MEISAI-AREA.
               05  S139N-NYUGAI-TBL        OCCURS 1.
                   07  S139N-HKN-TBL       OCCURS  18.
                       09  S139N-KENSU     PIC  X(04).
                       09  S139N-NISSU     PIC  X(04).
                       09  S139N-TENSU     PIC  X(09).
                       09  S139N-MADOGK    PIC  X(09).
                       09  S139N-JIHIGK    PIC  X(09).
                       09  S139N-HKNGK     PIC  X(09).
                       09  S139N-KOHGK     PIC  X(09).
                       09  S139N-SYUEKI    PIC  X(09).
                       09  S139N-SNISSU    PIC  X(05).
                       09  S139N-SSEIKYU   PIC  X(09).
                       09  S139N-SFTNGK    PIC  X(09).
      *    £±¡¢¼ÒÊÝ £²¡¢¹ñÊÝ ¡¢£³¡¢¸øÈñ £´¡¢ÊÝ¸±¹ç·× £µ¡¢¸å´ü¹âÎð¼Ô
                   07  S139N-SYOKEI-TBL         OCCURS 5.
                       09  S139N-S-KENSU        PIC  X(04).
                       09  S139N-S-NISSU        PIC  X(04).
                       09  S139N-S-TENSU        PIC  X(09).
                       09  S139N-S-MADOGK       PIC  X(09).
                       09  S139N-S-JIHIGK       PIC  X(09).
                       09  S139N-S-HKNGK        PIC  X(09).
                       09  S139N-S-KOHGK        PIC  X(09).
                       09  S139N-S-SYUEKI       PIC  X(09).
                       09  S139N-S-SNISSU       PIC  X(05).
                       09  S139N-S-SSEIKYU      PIC  X(09).
                       09  S139N-S-SFTNGK       PIC  X(09).
      *    £±¡¢¼«Èñ £²¡¢Ï«ºÒ £³¡¢¼«ÇåÀÕ £´¡¢¸ø³²  £µ¡¢¹ç·×
                   07  S139N-ETC-TBL            OCCURS 5.
                       09  S139N-ETC-KENSU      PIC  X(04).
                       09  S139N-ETC-NISSU      PIC  X(04).
                       09  S139N-ETC-TENSU      PIC  X(09).
                       09  S139N-ETC-MADOGK     PIC  X(09).
                       09  S139N-ETC-JIHIGK     PIC  X(09).
                       09  S139N-ETC-HKNGK      PIC  X(09).
                       09  S139N-ETC-KOHGK      PIC  X(09).
                       09  S139N-ETC-SYUEKI     PIC  X(09).
                       09  S139N-ETC-SNISSU     PIC  X(05).
                       09  S139N-ETC-SSEIKYU    PIC  X(09).
                       09  S139N-ETC-SFTNGK     PIC  X(09).
      *    Áí¹ç·×¥¨¥ê¥¢ £± ÊÝ¸± £² ¼«Èñ £³ Ï«ºÒ £´ ¼«ÇåÀÕ £µ ¸ø³² £¶ Áí¹ç·×
               05  S139N-GOKEI-TBL              OCCURS 6.
                   07  S139N-GKENSU             PIC  X(04).
                   07  S139N-GNISSU             PIC  X(04).
                   07  S139N-GTENSU             PIC  X(09).
                   07  S139N-GMADOGK            PIC  X(09).
                   07  S139N-GJIHIGK            PIC  X(09).
                   07  S139N-GHKNGK             PIC  X(09).
                   07  S139N-GKOHGK             PIC  X(09).
                   07  S139N-GSYUEKI            PIC  X(09).
