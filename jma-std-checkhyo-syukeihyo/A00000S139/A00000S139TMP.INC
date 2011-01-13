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
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      *  Maj/Min/Rev  ½¤Àµ¼Ô      ÆüÉÕ      ÆâÍÆ
      *   01.00.01    @@@@-ÅÄÃæ   11/01/05  ¥³¥á¥ó¥ÈÍúÎòºï½ü
      *******************************************************************
       01  S139TMP-REC.
           03  S139TMP-KEY.
               05  S139TMP-SRYKA              PIC  X(02).
               05  S139TMP-SRYYMKBN           PIC  X(01).
           03  S139TMP-MEISAI-AREA.
               05  S139TMP-NYUGAI-TBL         OCCURS 2.
                   07  S139-HKNTMP-TBL        OCCURS 18.
                       09  S139TMP-KENSU      PIC  9(04).
                       09  S139TMP-NISSU      PIC  9(04).
                       09  S139TMP-TENSU      PIC  9(09).
                       09  S139TMP-MADOGK     PIC  S9(09).
                       09  S139TMP-JIHIGK     PIC  9(09).
                       09  S139TMP-HKNGK      PIC  9(09).
                       09  S139TMP-KOHGK      PIC  S9(09).
                       09  S139TMP-SYUEKI     PIC  9(09).
                       09  S139TMP-SNISSU     PIC  9(05).
                       09  S139TMP-SSEIKYU    PIC  9(09).
                       09  S139TMP-SFTNGK     PIC  9(09).
      *    £±¡¢¼ÒÊÝ £²¡¢¹ñÊÝ £³¡¢¸øÈñ £´¡¢ÊÝ¸±¹ç·× £µ¡¢¸å´ü¹âÎð¼Ô
                   07  S139TMP-SYOKEI-TBL       OCCURS 5.
                       09  S139TMP-S-KENSU      PIC  9(04).
                       09  S139TMP-S-NISSU      PIC  9(04).
                       09  S139TMP-S-TENSU      PIC  9(09).
                       09  S139TMP-S-MADOGK     PIC  S9(09).
                       09  S139TMP-S-JIHIGK     PIC  9(09).
                       09  S139TMP-S-HKNGK      PIC  9(09).
                       09  S139TMP-S-KOHGK      PIC  S9(09).
                       09  S139TMP-S-SYUEKI     PIC  9(09).
                       09  S139TMP-S-SNISSU     PIC  9(05).
                       09  S139TMP-S-SSEIKYU    PIC  9(09).
                       09  S139TMP-S-SFTNGK     PIC  9(09).
      *    £±¡¢¼«Èñ £²¡¢Ï«ºÒ £³¡¢¼«ÇåÀÕ £´¡¢¸ø³² £µ¡¢¹ç·×
                   07  S139TMP-ETC-TBL            OCCURS 5.
                       09  S139TMP-ETC-KENSU      PIC  9(04).
                       09  S139TMP-ETC-NISSU      PIC  9(04).
                       09  S139TMP-ETC-TENSU      PIC  9(09).
                       09  S139TMP-ETC-MADOGK     PIC  S9(09).
                       09  S139TMP-ETC-JIHIGK     PIC  9(09).
                       09  S139TMP-ETC-HKNGK      PIC  9(09).
                       09  S139TMP-ETC-SYUEKI     PIC  9(09).
                       09  S139TMP-ETC-SNISSU     PIC  9(05).
                       09  S139TMP-ETC-SSEIKYU    PIC  9(09).
                       09  S139TMP-ETC-SFTNGK     PIC  9(09).
      *    Áí¹ç·×¥¨¥ê¥¢ £± ÊÝ¸± £² ¼«Èñ £³ Ï«ºÒ £´ ¼«ÇåÀÕ £µ ¸ø³² £¶ Áí¹ç·×
               05  S139TMP-GOKEI-TBL       OCCURS 6.
                   07  S139TMP-GKENSU      PIC  9(04).
                   07  S139TMP-GNISSU      PIC  9(04).
                   07  S139TMP-GTENSU      PIC  9(09).
                   07  S139TMP-GMADOGK     PIC  S9(09).
                   07  S139TMP-GJIHIGK     PIC  9(09).
                   07  S139TMP-GHKNGK      PIC  9(09).
                   07  S139TMP-GKOHGK      PIC  S9(09).
                   07  S139TMP-GSYUEKI     PIC  9(09).
                   07  S139TMP-SGNISSU     PIC  9(05).
                   07  S139TMP-SGSEIKYU    PIC  9(09).
                   07  S139TMP-SGFTNGK     PIC  9(09).
