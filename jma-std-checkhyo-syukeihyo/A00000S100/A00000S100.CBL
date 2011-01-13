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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000S100.
      *******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 保険別集計チェック表 起動PG
      *  管理者            :
      *  作成日付    作業者        記述
      *  04/03/22    @@@@-高橋     新規作成
      *******************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者      日付      内容
      *   01.00.01    @@@@-田中   11/01/05  open-cobol1.0対応
      *   01.00.01    @@@@-田中   11/01/05  コメント履歴削除
      *******************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-PARA            PIC 9(02).
           03  STS-PARA2           PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                  PIC 9(01).
           03  FLG-SYSKANRI             PIC 9(01).
           03  FLG-SYSKANRI3            PIC 9(01).
           03  FLG-HKNCOMBI             PIC 9(01).
           03  FLG-PTBYOMEI             PIC 9(01).
           03  FLG-PTINF                PIC 9(01).
           03  FLG-PTNUM                PIC 9(01).
           03  FLG-PTHKNINF             PIC 9(01).
           03  FLG-SRYACCT              PIC 9(01).
           03  FLG-JYURRK               PIC 9(01).
           03  FLG-PTNYUINRRK           PIC 9(01).
           03  FLG-SRYACT               PIC 9(01).
           03  FLG-TENSU                PIC 9(01).
           03  FLG-SRYKA                PIC 9(01).
           03  FLG-SRYKBN               PIC 9(01).
           03  FLG-OK                   PIC 9(01).
           03  FLG-NG                   PIC 9(01).
           03  FLG-ATEND                PIC 9(01).
           03  FLG-GAISW                PIC 9(01).
           03  FLG-NYUSW                PIC 9(01).
           03  FLG-SRYKA-ALL            PIC 9(01).
           03  INP-STS                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0               PIC 9(04).
           03  IDX1               PIC 9(02).
           03  IDX2               PIC 9(04).
           03  IDX3               PIC 9(04).
           03  IDX4               PIC 9(02).
           03  IDX5               PIC 9(04).
           03  IDX                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PRTYPE       PIC X(01).
           03  WRK-PARA-YM           PIC X(06).
           03  WRK-PARA-NYUGAIKBN    PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-PRDTLCNT          PIC 9(06).
           03  WRK-RECCNT            PIC 9(06).
           03  WRK-HONKZKKBN         PIC X(04).
           03  WRK-CHK-SRYKA         PIC X(02).
      *
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06).
           03  WRK-SYUHKNNUM         PIC X(03).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
      *
           03  WRK-Z3                PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z9                PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZZZZZZ.
           03  WRK-S9                PIC -----------9.
           03  WRK-SZ                PIC -----------Z.
           03  DEBUG-CNT             PIC 9(03).
      *
       01  WRK-SRYKA-AERA.
           03  WRK-SRYKAMAX        PIC    9(03).
           03  WRK-SRYKA-TBL       OCCURS 100.
               05  WRK-SRYKA       PIC    X(02).
               05  WRK-SRYKANAME   PIC    X(20).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "A00000S100LNK.INC".
      *
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM  100-INIT-SEC
      *
           PERFORM  200-MAIN-SEC
      *
           PERFORM  300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-PARA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  WRK-SRYKA-AERA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-PRTYPE
                                               WRK-PARA-YM
                                               WRK-PARA-NYUGAIKBN
      *
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE  LNK-PRTKANRI-RENNUM    TO
                              S100LNK-PRTKANRI-RENNUM
           MOVE  LNK-PRTKANRI-TBL-KEY   TO
                              S100LNK-PRTKANRI-TBL-KEY
           MOVE  LNK-PRTKANRI-TBL-GROUP TO
                              S100LNK-PRTKANRI-TBL-GROUP
           MOVE  LNK-PRTKANRI-SHORI-RENNUM      TO
                              S100LNK-PRTKANRI-SHORI-RENNUM
           MOVE  LNK-PRTKANRI-SRYYM     TO
                              S100LNK-PRTKANRI-SRYYM
           MOVE  LNK-PRTKANRI-SKYYMD    TO
                              S100LNK-PRTKANRI-SKYYMD
           MOVE  LNK-PRTKANRI-SHELLID   TO
                              S100LNK-PRTKANRI-SHELLID
           MOVE  LNK-PRTKANRI-PRIORITY  TO
                              S100LNK-PRTKANRI-PRIORITY
           MOVE  LNK-PRTKANRI-TERMID    TO
                              S100LNK-PRTKANRI-TERMID
           MOVE  LNK-PRTKANRI-OPID      TO
                              S100LNK-PRTKANRI-OPID
           MOVE  LNK-PRTKANRI-PRTNM     TO
                              S100LNK-PRTKANRI-PRTNM
      *
           MOVE  WRK-PARA-JOBID   TO    S100LNK-JOBID
           MOVE  WRK-PARA-SHELLID TO    S100LNK-SHELLID
           MOVE  WRK-PARA-HOSPNUM  TO    S100LNK-HOSPNUM
           MOVE  WRK-PARA-YM      TO    S100LNK-YM
           MOVE  WRK-PARA-NYUGAIKBN  TO   S100LNK-NYUGAIKBN
      *
           MOVE    "保険別請求チェック表"
                                  TO    S100LNK-SPRT-TITLE
      *    ステップ管理開始処理
           MOVE    "STS"              TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    S100LNK-JOBID      TO  JOB-JOBID
           MOVE    S100LNK-SHELLID    TO  JOB-SHELLID
           MOVE    "A00000S100"       TO  JOB-PGID
           MOVE    S100LNK-SPRT-TITLE TO  JOB-SHELLMSG
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
      *
      *    帳票別サブプログラムCALL
           EVALUATE  WRK-PARA-PRTYPE
      *    (全出力)
             WHEN  ""
      *
               CALL  "A00000S137" USING  A00000S100-LNK
      *
               CALL  "A00000S138" USING  A00000S100-LNK
      *
               CALL  "A00000S139" USING  A00000S100-LNK
      *
               CALL  "A00000S140" USING  A00000S100-LNK
      *
      *    (社保)
             WHEN  "0"
               MOVE    "保険別請求チェック表(社保)"
                                  TO    S100LNK-SPRT-TITLE
               CALL  "A00000S137" USING  A00000S100-LNK
      *    (国保)
             WHEN  "1"
               MOVE    "保険別請求チェック表(国保)"
                                  TO    S100LNK-SPRT-TITLE
               CALL  "A00000S138" USING  A00000S100-LNK
      *    (総括)
             WHEN  "2"
               MOVE    "保険別請求チェック表(総括)"
                                  TO    S100LNK-SPRT-TITLE
               CALL  "A00000S139" USING  A00000S100-LNK
             WHEN  "3"
               MOVE    "保険別請求チェック表(後期高齢者)"
                                  TO    S100LNK-SPRT-TITLE
               CALL  "A00000S140" USING  A00000S100-LNK
           END-EVALUATE
      *
      *    ステップ管理終了処理
           MOVE    "STE"             TO  SJOBKANRI-MODE
           INITIALIZE                JOBKANRI-REC
           MOVE    SPA-HOSPNUM       TO  JOB-HOSPNUM
           MOVE    S100LNK-JOBID     TO  JOB-JOBID
           MOVE    S100LNK-SHELLID   TO  JOB-SHELLID
           MOVE    0                 TO  JOB-UPDCNT
           CALL    "ORCSJOB"         USING
                                     ORCSJOBKANRIAREA
                                     JOBKANRI-REC
                                     SPA-AREA
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
