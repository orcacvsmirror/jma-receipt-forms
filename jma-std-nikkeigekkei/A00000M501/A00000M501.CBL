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
       PROGRAM-ID.             A00000M501.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 月計表（ＨＣ１７）
      *  管理者            :
      *  作成日付    作業者       記述
      *  03/12/24    @@@@-半田    新規再作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/07  コメント履歴削除
      *                                     初診、再診、その他の判定条件を変更
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-門間    11/04/18  処理速度の改善、コメント履歴削除
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
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
           COPY    "A00000M501.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-JYURRK2         PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  DEBUG-SW            PIC 9(01).
           03  FLG-ACCTEND         PIC 9(01).
      *    件数用フラグ領域
           03  FLG-KENSUU.
             05  FLG-HKN           PIC 9(01).
             05  FLG-SYUEKI        PIC 9(01).
             05  FLG-SEIKYU        PIC 9(01).
             05  FLG-RYOSYU        PIC 9(01).
             05  FLG-JIHI-G.
               07  FLG-JIHI        PIC 9(01)  OCCURS  10.
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-SRYACT             PIC 9(01).
           03  FLG-JYURRK3         PIC 9(01).
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
           03  IDX                 PIC 9(10).
           03  IDY                 PIC 9(10).
           03  IDZ                 PIC 9(10).
           03  IDX-S               PIC 9(10).
           03  IDX-N               PIC 9(10).
           03  IDX-MAX             PIC 9(10).
           03  IDX7G.
              05  IDX7             PIC 9(02).
           03  IDX8                PIC 9(04).
      *
           03  IDX2                PIC 9(10).
           03  IDX3                PIC 9(10).
           03  JIHIIDX             PIC 9(10).
           03  IDX10                  PIC 9(02).
           03  IDX11                  PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PRTYM      PIC X(06).
           03  WRK-PARA-PRTSYMD    PIC X(08).
           03  WRK-PARA-PRTLYMD    PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
      *
           03  WRK-RECEERR           PIC X(200).
           03  WRK-PRTYMWH           PIC X(16).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06).
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
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-SRYKA             PIC X(02).
           03  WRK-PTID              PIC 9(10).
      *
           03  WRK-MONEY             PIC S9(08).
           03  WRK-JIHI              PIC 9(08).
           03  WRK-JIHI2             PIC 9(08).
           03  WRK-JIHI3             PIC 9(08).
      *
           03  WRK-CREYMD            PIC X(08).
           03  WRK-SYUEKI            PIC S9(08).
           03  WRK-TJIHI-G       OCCURS  10.
               05  WRK-TJIHI         PIC X(08).
           03  WRK-TJIHIN-G          OCCURS  10.
               05  WRK-TJIHIN        PIC X(08).
           03  WRK-4001-TENTANKA PIC 9(02)V9(01).
      *
      *    入院用印刷診療科件数
           03  WRK-PRNSRKA-CNT       PIC 9(03).
      *     外来用印刷診療科件数
           03  WRK-PRGSRKA-CNT       PIC 9(03).
      *
           03  WRK-RENNUM-X          PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z9                PIC ZZZZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZZZZ.
           03  WRK-S9                PIC ---------9.
           03  WRK-SZ                PIC ---------Z.
           03  WRK-TMP-K             PIC X(08).
           03  WRK-COUNTS            PIC 9(04).
           03  WRK-COMPFTN           PIC S9(10).
           03  WRK-SYOSINYMD         PIC 9(08).
           03  WRK-DENPNUM           PIC 9(07).
      *
      *    診療科別加算エリア
       01  SUM-AREA                 VALUE   ZERO.
           03  SUM-HKNTEN           PIC 9(10).
           03  SUM-SHKNKEN          PIC 9(10).
           03  SUM-RHKNKEN          PIC 9(10).
           03  SUM-OHKNKEN          PIC 9(10).
           03  SUM-SAHKNKEN         PIC 9(10).
           03  SUM-OCC              OCCURS  20.
               05  SUM-MONEY        PIC S9(10).
               05  SUM-OKEN         PIC 9(10).
               05  SUM-RKEN         PIC 9(10).
               05  SUM-SKEN         PIC 9(10).
               05  SUM-SAKEN        PIC 9(10).
           03  SUM-SYUEKI           PIC S9(10).
           03  SUM-SEIKYU           PIC S9(10).
           03  SUM-RYOSYU           PIC S9(10).
           03  SUM-SSYUEKIKEN       PIC 9(10).
           03  SUM-SSEIKYUKEN       PIC 9(10).
           03  SUM-SRYOSYUKEN       PIC 9(10).
           03  SUM-RSYUEKIKEN       PIC 9(10).
           03  SUM-RSEIKYUKEN       PIC 9(10).
           03  SUM-RRYOSYUKEN       PIC 9(10).
           03  SUM-OSYUEKIKEN       PIC 9(10).
           03  SUM-OSEIKYUKEN       PIC 9(10).
           03  SUM-ORYOSYUKEN       PIC 9(10).
           03  SUM-JYUSHINKEN       PIC 9(10).
           03  SUM-SASYUEKIKEN      PIC 9(10).
           03  SUM-SASEIKYUKEN      PIC 9(10).
           03  SUM-SARYOSYUKEN      PIC 9(10).
      *
      *    入外区分別加算エリア
       01  SUM-AREA-GK              VALUE   ZERO.
           03  SUM-HKNTEN-GK        PIC 9(10).
           03  SUM-SHKNKEN-GK       PIC 9(10).
           03  SUM-RHKNKEN-GK       PIC 9(10).
           03  SUM-OHKNKEN-GK       PIC 9(10).
           03  SUM-SAHKNKEN-GK      PIC 9(10).
           03  SUM-OCC-GK           OCCURS  20.
               05  SUM-MONEY-GK     PIC S9(10).
               05  SUM-OKEN-GK      PIC 9(10).
               05  SUM-RKEN-GK      PIC 9(10).
               05  SUM-SKEN-GK      PIC 9(10).
               05  SUM-SAKEN-GK     PIC 9(10).
           03  SUM-SYUEKI-GK        PIC S9(10).
           03  SUM-SEIKYU-GK        PIC S9(10).
           03  SUM-RYOSYU-GK        PIC S9(10).
           03  SUM-SSYUEKIKEN-GK    PIC 9(10).
           03  SUM-SSEIKYUKEN-GK    PIC 9(10).
           03  SUM-SRYOSYUKEN-GK    PIC 9(10).
           03  SUM-RSYUEKIKEN-GK    PIC 9(10).
           03  SUM-RSEIKYUKEN-GK    PIC 9(10).
           03  SUM-RRYOSYUKEN-GK    PIC 9(10).
           03  SUM-OSYUEKIKEN-GK    PIC 9(10).
           03  SUM-OSEIKYUKEN-GK    PIC 9(10).
           03  SUM-ORYOSYUKEN-GK    PIC 9(10).
           03  SUM-JYUSHINKEN-GK    PIC 9(10).
           03  SUM-SASYUEKIKEN-GK   PIC 9(10).
           03  SUM-SASEIKYUKEN-GK   PIC 9(10).
           03  SUM-SARYOSYUKEN-GK   PIC 9(10).
      *
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL      OCCURS  300000.
               05  WRK-SMEI-HOSPNUM         PIC 9(02).
               05  WRK-SMEI-SORT-KEY.
                   07  WRK-SMEI-NYUGAIKBN   PIC X(01).
                   07  WRK-SMEI-SRYKA       PIC X(02).
                   07  WRK-SMEI-PTID        PIC 9(10).
                   07  WRK-SMEI-DENPNUM     PIC 9(07).
                   07  WRK-SMEI-DENPEDANUM  PIC 9(02).
               05  WRK-SMEI-SKYMONEY        PIC S9(07).
               05  WRK-SMEI-NYUHEN-MONEY    PIC S9(07).
               05  WRK-SMEI-NYUHEN-YMD      PIC X(08).
               05  WRK-SMEI-PTINFCRE-YMD    PIC X(08).
      *
           03  WRK-SV-SMEI-TBL              PIC X(54).
      *
           03  WRK-SV-SMEI-DENPNUM          PIC 9(07).
      *
       01   WRK-JYUSHIN-AREA.
           03   WRK-JYUSHIN-PTID-TBL     OCCURS 10000.
               05   WRK-JYUSHIN-PTID        PIC  9(10).
               05   WRK-NYUHEN-YMD          PIC  X(02).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980".
      *    労災保険番号
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    自賠責保険番号
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *    公害保険番号
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975".
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    自費管理情報
           COPY  "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    労災医療機関情報
           COPY  "CPSK4001.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
       01  JYURRK2-REC.
           COPY    "CPJYURRK.INC"      REPLACING  //JYURRK//
                                       BY         //JYURRK2//.
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
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
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
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
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END   =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  SUM-AREA-GK
           INITIALIZE                  WRK-SMEI-REC
           INITIALIZE                  WRK-JYUSHIN-AREA
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
                                               WRK-PARA-PRTYM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO   SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"              TO   SJOBKANRI-MODE
           INITIALIZE                 JOBKANRI-REC
           MOVE    SPA-HOSPNUM        TO   JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID     TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID   TO   JOB-SHELLID
           MOVE    "A00000M501"       TO   JOB-PGID
           MOVE    "月計表合計（窓口領収金）"  TO   JOB-SHELLMSG
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD   TO   WRK-SYMD
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO   WRK-SYSYMDWH
      *
      *    対象年月最初日設定
           MOVE    WRK-PARA-PRTYM        TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                  TO   WRK-PARA-PRTSYMD(7:2)
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTYM        TO   WRK-SYMD(1:6)
           PERFORM   31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD              TO   WRK-PARA-PRTLYMD
      *
      *    対象年月日編集
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTLYMD      TO   WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)     TO   WRK-PRTYMWH
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                 TO   SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO   SYS-1001-KANRICD
           MOVE    "*"                   TO   SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM           TO   SYS-1001-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD      TO   SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD      TO   SYS-1001-STYUKYMD
           MOVE    SYS-1001-REC          TO   MCPDATA-REC
           PERFORM   800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI   =   ZERO
             MOVE    MCPDATA-REC         TO   SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO   WRK-PARA-HOSPNUM
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO   WRK-RECEERR
             PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
      *    自費情報取得
      *    区分11
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "11"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(1)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHIN(1)
           END-IF
      *
      *    区分12
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "12"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(2)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(2)
           END-IF
      *
      *    区分13
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "13"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE  WRK-PARA-PRTSYMD      TO   SYS-1013-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD      TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(3)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(3)
           END-IF
      *
      *    区分14
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "14"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(4)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(4)
           END-IF
      *
      *    区分15
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "15"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2    =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(5)
           ELSE
             MOVE   SPACE                  TO   WRK-TJIHIN(5)
           END-IF
      *
      *    区分16
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "16"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC         TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME   TO   WRK-TJIHIN(6)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHIN(6)
           END-IF
      *
      *    区分17
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "17"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(7)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(7)
           END-IF
      *
      *    区分18
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "18"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE  WRK-PARA-PRTSYMD      TO   SYS-1013-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD      TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(8)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(8)
           END-IF
      *
      *    区分19
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "19"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(9)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(9)
           END-IF
      *
      *    区分10
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "10"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(10)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(10)
           END-IF
      *
      *    自費情報取得
      *    区分21
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "21"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(1)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(1)
           END-IF
      *
      *    区分22
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "22"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(2)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(2)
           END-IF
      *
      *    区分23
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "23"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(3)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(3)
           END-IF
      *
      *    区分24
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "24"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(4)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(4)
           END-IF
      *
      *    区分25
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "25"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(5)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(5)
           END-IF
      *
      *    区分26
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "26"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(6)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(6)
           END-IF
      *
      *    区分27
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "27"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2    =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(7)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(7)
           END-IF
      *
      *    区分28
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "28"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(8)
           ELSE
             MOVE  SPACE               TO  WRK-TJIHI(8)
           END-IF
      *
      *    区分29
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "29"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(9)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHI(9)
           END-IF
      *
      *    区分20
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                       SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "20"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2       =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(10)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(10)
           END-IF
      *
      *    労災医療機関情報
           MOVE    SPACE               TO   SYS-4001-REC
           INITIALIZE                       SYS-4001-REC
           MOVE    "4001"              TO   SYS-4001-KANRICD
           MOVE    "*"                 TO   SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-4001-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-4001-STYUKYMD
           MOVE    SYS-4001-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
               MOVE  MCPDATA-REC    TO  SYS-4001-REC
             EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                 MOVE    11.5    TO   WRK-4001-TENTANKA
               WHEN  "2"
                 MOVE    12      TO   WRK-4001-TENTANKA
               WHEN  OTHER
                 MOVE    10      TO   WRK-4001-TENTANKA
             END-EVALUATE
           ELSE
             MOVE  10   TO  WRK-4001-TENTANKA
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM  310-DATA-SELECT-SEC
      *
           PERFORM  UNTIL   FLG-END   =   1
      *    収納情報集計処理
             PERFORM 320-BODY-HEN-SEC
           END-PERFORM
      *
      *    帳票編集<診療科合計ページ>印刷処理
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               IF   WRK-PRNSRKA-CNT   >   1
                 PERFORM 330-TERM-KA-PRINT-SEC
               END-IF
             WHEN "2"
               IF   WRK-PRGSRKA-CNT   >   1
                 PERFORM 330-TERM-KA-PRINT-SEC
               END-IF
           END-EVALUATE
      *
      *    帳票編集<合計ページ>印刷処理
           PERFORM 330-TERM-GK-PRINT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データ選定処理
      *****************************************************************
       310-DATA-SELECT-SEC          SECTION.
      *
           MOVE    0     TO    IDX-S
      *
           PERFORM   910-DBSELECT-SEC
           PERFORM   UNTIL   FLG-SYUMEI   =   1
             PERFORM   900-SYUNOU-READ-SEC
      *
             IF      FLG-SYUNOU             =   ZERO
               AND   SYU-DENPJTIKBN     NOT =  "3"
               AND ((SMEI-JOUTAIKBN     NOT =  "8"
               AND   SMEI-JOUTAIKBN     NOT =  "3")
               OR    SMEI-SKYMONEY      NOT =  0
               OR    SMEI-NYUHEN-MONEY  NOT =  0)
      *
               ADD   1                  TO  IDX-S
               MOVE  SMEI-HOSPNUM       TO  WRK-SMEI-HOSPNUM(IDX-S)
               MOVE  SMEI-NYUGAIKBN     TO  WRK-SMEI-NYUGAIKBN(IDX-S)
               MOVE  SMEI-SRYKA         TO  WRK-SMEI-SRYKA(IDX-S)
               MOVE  SMEI-PTID          TO  WRK-SMEI-PTID(IDX-S)
               MOVE  SMEI-DENPNUM       TO  WRK-SMEI-DENPNUM(IDX-S)
               MOVE  SMEI-DENPEDANUM    TO  WRK-SMEI-DENPEDANUM(IDX-S)
               MOVE  SMEI-SKYMONEY      TO  WRK-SMEI-SKYMONEY(IDX-S)
               MOVE  SMEI-NYUHEN-MONEY  TO  WRK-SMEI-NYUHEN-MONEY(IDX-S)
               MOVE  SMEI-NYUHEN-YMD    TO  WRK-SMEI-NYUHEN-YMD(IDX-S)
               MOVE  PTINF-CREYMD       TO  WRK-SMEI-PTINFCRE-YMD(IDX-S)
             END-IF
             PERFORM   910-DBFETCH-SEC
           END-PERFORM
           PERFORM   910-DBCLOSE-SEC
      *
      *    ソート処理
           MOVE IDX-S TO IDX-MAX
      *
           IF   IDX-MAX   >    1
             MOVE    1        TO   FLG-GET
             MOVE    ZERO     TO   FLG-END
             MOVE 1  TO  IDX-S
             MOVE 2  TO  IDX-N
             PERFORM UNTIL IDX-S NOT  <  IDX-MAX
               PERFORM  UNTIL IDX-N  >  IDX-MAX
                 IF WRK-SMEI-SORT-KEY(IDX-S)
                     >  WRK-SMEI-SORT-KEY(IDX-N)
                   MOVE   WRK-SMEI-TBL(IDX-N)   TO   WRK-SV-SMEI-TBL
                   MOVE   WRK-SMEI-TBL(IDX-S)   TO   WRK-SMEI-TBL(IDX-N)
                   MOVE   WRK-SV-SMEI-TBL       TO   WRK-SMEI-TBL(IDX-S)
                 END-IF
                 ADD    1      TO   IDX-N
               END-PERFORM
               ADD 1  TO  IDX-S
               COMPUTE IDX-N = IDX-S + 1
             END-PERFORM
           ELSE
             IF   IDX-MAX    =   ZERO
               MOVE   ZERO         TO   FLG-GET
               MOVE   1            TO   FLG-END
             ELSE
               MOVE   1            TO   FLG-GET
               MOVE   ZERO         TO   FLG-END
             END-IF
           END-IF
      *
           MOVE     1       TO   IDX-S
      *
           .
       310-DATA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計カウント>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO     TO  CNT-LINE
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO  WRK-NYUGAIKBN
           MOVE    WRK-SMEI-SRYKA(IDX-S)       TO  WRK-SRYKA
      *
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
      *    入院診療科数カウントアップ
               MOVE   1    TO WRK-PRNSRKA-CNT
             WHEN "2"
      *    外来診療科数カウントアップ
               MOVE   1    TO WRK-PRGSRKA-CNT
           END-EVALUATE
      *
           PERFORM   UNTIL   FLG-END     =   1
      *    件数用フラグの初期化
             INITIALIZE   FLG-KENSUU
      *
      *    収納テーブル再読み込み
             PERFORM 901-SYUNOU-READ-SEC
             IF    FLG-SYUNOU   =   ZERO
      *
      *    入外区分ブレイクチェック処理
             IF  WRK-SMEI-NYUGAIKBN(IDX-S)  NOT = WRK-NYUGAIKBN
      *    合計カウンターに加算
               ADD    SUM-HKNTEN      TO   SUM-HKNTEN-GK
               ADD    SUM-SHKNKEN     TO   SUM-SHKNKEN-GK
               ADD    SUM-RHKNKEN     TO   SUM-RHKNKEN-GK
               ADD    SUM-OHKNKEN     TO   SUM-OHKNKEN-GK
               ADD  SUM-SAHKNKEN      TO   SUM-SAHKNKEN-GK
               PERFORM  VARYING   IDX2   FROM  1  BY 1
                        UNTIL   IDX2   >   20
                 ADD   SUM-MONEY(IDX2)   TO   SUM-MONEY-GK(IDX2)
                 ADD   SUM-RKEN(IDX2)    TO   SUM-RKEN-GK(IDX2)
                 ADD   SUM-SKEN(IDX2)    TO   SUM-SKEN-GK(IDX2)
                 ADD   SUM-OKEN(IDX2)    TO   SUM-OKEN-GK(IDX2)
                 ADD   SUM-SAKEN(IDX2)   TO   SUM-SAKEN-GK(IDX2)
               END-PERFORM
               ADD    SUM-SYUEKI         TO   SUM-SYUEKI-GK
               ADD    SUM-SEIKYU         TO   SUM-SEIKYU-GK
               ADD    SUM-RYOSYU         TO   SUM-RYOSYU-GK
               ADD    SUM-SSYUEKIKEN     TO   SUM-SSYUEKIKEN-GK
               ADD    SUM-SSEIKYUKEN     TO   SUM-SSEIKYUKEN-GK
               ADD    SUM-SRYOSYUKEN     TO   SUM-SRYOSYUKEN-GK
               ADD    SUM-RSYUEKIKEN     TO   SUM-RSYUEKIKEN-GK
               ADD    SUM-RSEIKYUKEN     TO   SUM-RSEIKYUKEN-GK
               ADD    SUM-RRYOSYUKEN     TO   SUM-RRYOSYUKEN-GK
               ADD    SUM-OSYUEKIKEN     TO   SUM-OSYUEKIKEN-GK
               ADD    SUM-OSEIKYUKEN     TO   SUM-OSEIKYUKEN-GK
               ADD    SUM-ORYOSYUKEN     TO   SUM-ORYOSYUKEN-GK
               ADD    SUM-SASYUEKIKEN    TO   SUM-SASYUEKIKEN-GK
               ADD    SUM-SASEIKYUKEN    TO   SUM-SASEIKYUKEN-GK
               ADD    SUM-SARYOSYUKEN    TO   SUM-SARYOSYUKEN-GK
               IF   WRK-NYUGAIKBN   =      "1"
                AND WRK-PRNSRKA-CNT >      1
                    PERFORM 330-TERM-KA-PRINT-SEC
               END-IF
               PERFORM 330-TERM-GK-PRINT-SEC
               INITIALIZE           SUM-AREA-GK
               INITIALIZE           SUM-AREA
      *
      *    ブレイクキー項目設定
               MOVE   WRK-SMEI-NYUGAIKBN(IDX-S)   TO   WRK-NYUGAIKBN
               MOVE   WRK-SMEI-SRYKA(IDX-S)       TO   WRK-SRYKA
      *
      *    PTIDブレイクキー項目の設定
               INITIALIZE  WRK-PTID
      *
               EVALUATE WRK-NYUGAIKBN
                 WHEN "1"
      *    入院診療科数カウントアップ
                   ADD   1    TO   WRK-PRNSRKA-CNT
                 WHEN "2"
      *    外来診療科数カウントアップ
                   ADD   1    TO   WRK-PRGSRKA-CNT
               END-EVALUATE
      *
             END-IF
      *
      *    診療科ブレイクチェック処理
             IF  WRK-SMEI-SRYKA(IDX-S)  NOT = WRK-SRYKA
               PERFORM 330-TERM-KA-PRINT-SEC
      *    合計カウンターに加算
               ADD   SUM-HKNTEN       TO   SUM-HKNTEN-GK
               ADD   SUM-SHKNKEN      TO   SUM-SHKNKEN-GK
               ADD   SUM-RHKNKEN      TO   SUM-RHKNKEN-GK
               ADD   SUM-OHKNKEN      TO   SUM-OHKNKEN-GK
               ADD   SUM-SAHKNKEN     TO   SUM-SAHKNKEN-GK
               PERFORM   VARYING   IDX2   FROM  1  BY 1
                         UNTIL   IDX2   >   20
                 ADD   SUM-MONEY(IDX2)    TO   SUM-MONEY-GK(IDX2)
                 ADD   SUM-RKEN(IDX2)     TO   SUM-RKEN-GK(IDX2)
                 ADD   SUM-SKEN(IDX2)     TO   SUM-SKEN-GK(IDX2)
                 ADD   SUM-OKEN(IDX2)     TO   SUM-OKEN-GK(IDX2)
                 ADD   SUM-SAKEN(IDX2)    TO   SUM-SAKEN-GK(IDX2)
               END-PERFORM
               ADD   SUM-SYUEKI          TO   SUM-SYUEKI-GK
               ADD   SUM-SEIKYU          TO   SUM-SEIKYU-GK
               ADD   SUM-RYOSYU          TO   SUM-RYOSYU-GK
               ADD   SUM-SSYUEKIKEN      TO   SUM-SSYUEKIKEN-GK
               ADD   SUM-SSEIKYUKEN      TO   SUM-SSEIKYUKEN-GK
               ADD   SUM-SRYOSYUKEN      TO   SUM-SRYOSYUKEN-GK
               ADD   SUM-RSYUEKIKEN      TO   SUM-RSYUEKIKEN-GK
               ADD   SUM-RSEIKYUKEN      TO   SUM-RSEIKYUKEN-GK
               ADD   SUM-RRYOSYUKEN      TO   SUM-RRYOSYUKEN-GK
               ADD   SUM-OSYUEKIKEN      TO   SUM-OSYUEKIKEN-GK
               ADD   SUM-OSEIKYUKEN      TO   SUM-OSEIKYUKEN-GK
               ADD   SUM-ORYOSYUKEN      TO   SUM-ORYOSYUKEN-GK
               ADD   SUM-SASYUEKIKEN     TO   SUM-SASYUEKIKEN-GK
               ADD   SUM-SASEIKYUKEN     TO   SUM-SASEIKYUKEN-GK
               ADD   SUM-SARYOSYUKEN     TO   SUM-SARYOSYUKEN-GK
               INITIALIZE                SUM-AREA
      *
      *    ブレイクキー項目設定
               MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO   WRK-NYUGAIKBN
               MOVE    WRK-SMEI-SRYKA(IDX-S)       TO   WRK-SRYKA
      *
      *    PTIDブレイクキー項目の設定
               INITIALIZE  WRK-PTID
               EVALUATE WRK-NYUGAIKBN
                 WHEN "1"
      *    入院診療科数カウントアップ
                   ADD    1    TO   WRK-PRNSRKA-CNT
                 WHEN  "2"
      *    外来診療科数カウントアップ
                   ADD    1    TO   WRK-PRGSRKA-CNT
               END-EVALUATE
      *
             END-IF
      *
      *    伝票番号枝番が１の場合は保険点数、保険別請求額の明細を印字(開始)
             IF   WRK-SMEI-DENPEDANUM(IDX-S)   =     1
               IF   FLG-SYUNOU  = ZERO
                 IF  WRK-PARA-PRTYM  <  "200804"
                   PERFORM   320-BODY-HEN-SUB-SEC
                 ELSE
                   PERFORM   330-BODY-HEN-SUB-SEC
                 END-IF
      *    受診履歴集計
                 IF SYU-NYUGAIKBN = "1"
      *    入院の場合は初診、再診別に件数を集計する。
                   PERFORM   320-CNT-SEC
                 ELSE
      *    外来の場合は初診、再診別に件数を集計する。
                   MOVE   WRK-SMEI-PTINFCRE-YMD(IDX-S)  TO   WRK-CREYMD
                   PERFORM  900-JYURRK-SELECT-SEC
                   PERFORM  UNTIL  FLG-JYURRK  =  1
                     IF   JYURRK-DENPNUM  NOT =  WRK-DENPNUM
                       PERFORM  320-JYURRK-CNT-SEC
                     END-IF
                     PERFORM  900-JYURRK-FETCH-SEC
                   END-PERFORM
                   PERFORM  900-JYURRK-CLOSE-SEC
                 END-IF
               END-IF
             END-IF
      *
      *    請求金額
             INITIALIZE  WRK-SZ
             MOVE WRK-SMEI-SKYMONEY(IDX-S)   TO   WRK-SZ
             IF   WRK-SMEI-SKYMONEY(IDX-S)  NOT =   0
               ADD   WRK-SMEI-SKYMONEY(IDX-S)   TO   SUM-SEIKYU
               MOVE    1           TO    FLG-SEIKYU
             END-IF
      *    領収金額
             INITIALIZE  WRK-SZ
             MOVE WRK-SMEI-NYUHEN-MONEY(IDX-S) TO WRK-SZ
             IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
               ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)  TO   SUM-RYOSYU
               MOVE      1        TO   FLG-RYOSYU
             END-IF
      *
      *    受診履歴集計
           END-IF
      *    収納テーブル収納テーブル再読み込み
      *
      *    PTIDブレイクキー項目の設定
           ADD  1     TO  IDX-S
      *    配列のデータを全件出力した場合は合計表の印刷を行う為にループより脱出
           IF IDX-S   >   IDX-MAX
             MOVE    1       TO   FLG-END
           END-IF
      *
           END-PERFORM
      *
      *    合計カウンターに加算
           ADD   SUM-HKNTEN      TO   SUM-HKNTEN-GK
           ADD   SUM-SHKNKEN     TO   SUM-SHKNKEN-GK
           ADD   SUM-RHKNKEN     TO   SUM-RHKNKEN-GK
           ADD   SUM-OHKNKEN     TO   SUM-OHKNKEN-GK
           ADD   SUM-SAHKNKEN    TO   SUM-SAHKNKEN-GK
           PERFORM   VARYING  IDX2   FROM  1  BY 1
                     UNTIL        IDX2   >     20
             ADD    SUM-MONEY(IDX2)     TO   SUM-MONEY-GK(IDX2)
             ADD    SUM-RKEN(IDX2)      TO   SUM-RKEN-GK(IDX2)
             ADD    SUM-SKEN(IDX2)      TO   SUM-SKEN-GK(IDX2)
             ADD    SUM-OKEN(IDX2)      TO   SUM-OKEN-GK(IDX2)
             ADD    SUM-SAKEN(IDX2)     TO   SUM-SAKEN-GK(IDX2)
           END-PERFORM
           ADD    SUM-SYUEKI            TO   SUM-SYUEKI-GK
           ADD    SUM-SEIKYU            TO   SUM-SEIKYU-GK
           ADD    SUM-RYOSYU            TO   SUM-RYOSYU-GK
           ADD    SUM-SSYUEKIKEN        TO   SUM-SSYUEKIKEN-GK
           ADD    SUM-SSEIKYUKEN        TO   SUM-SSEIKYUKEN-GK
           ADD    SUM-SRYOSYUKEN        TO   SUM-SRYOSYUKEN-GK
           ADD    SUM-RSYUEKIKEN        TO   SUM-RSYUEKIKEN-GK
           ADD    SUM-RSEIKYUKEN        TO   SUM-RSEIKYUKEN-GK
           ADD    SUM-RRYOSYUKEN        TO   SUM-RRYOSYUKEN-GK
           ADD    SUM-OSYUEKIKEN        TO   SUM-OSYUEKIKEN-GK
           ADD    SUM-OSEIKYUKEN        TO   SUM-OSEIKYUKEN-GK
           ADD    SUM-ORYOSYUKEN        TO   SUM-ORYOSYUKEN-GK
           ADD    SUM-SASYUEKIKEN       TO   SUM-SASYUEKIKEN-GK
           ADD    SUM-SASEIKYUKEN       TO   SUM-SASEIKYUKEN-GK
           ADD    SUM-SARYOSYUKEN       TO   SUM-SARYOSYUKEN-GK
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計カウント>サブ処理
      *****************************************************************
       320-BODY-HEN-SUB-SEC      SECTION.
      *
      *    保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =   1
             MOVE     10     TO   HKN-TENTNK
           END-IF
      *
      *    保険点数
           MOVE     ZERO      TO   SYU-TOTAL-HKNTEN
           EVALUATE    SYU-SYUHKNNUM
             WHEN    OTHER
               PERFORM  VARYING IDZ  FROM  1   BY  1
                        UNTIL   IDZ   >   16
                 ADD    SYU-HKNTEN(IDZ)     TO   SYU-TOTAL-HKNTEN
               END-PERFORM
               MOVE    SYU-TOTAL-HKNTEN     TO   WRK-Z9
               ADD     SYU-TOTAL-HKNTEN     TO   SUM-HKNTEN
               MOVE     1                   TO   FLG-HKN
           END-EVALUATE
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                     TO   IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6                    TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19                   TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18                   TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    17                   TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                   TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    5                  TO   IDX
               ELSE
                 MOVE    4                  TO   IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    3                  TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1              TO   IDX
                   WHEN    "2"
                     MOVE    2              TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      * 保険別区分例外条件(自費条件拡張)
           IF   SYU-SYUHKNNUM(1:2) = "98"
             MOVE    17          TO    IDX
           END-IF
      *
           IF   IDX   >   ZERO
      *
      *    自費以外のとき自費分だけ集計
             INITIALIZE WRK-JIHI
             INITIALIZE WRK-JIHI3
               IF   SYU-SRYYMD  <=  "20070331"
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
               ELSE
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
               END-IF
      *

             COMPUTE WRK-MONEY   =   SYU-SKYMONEY   -   WRK-JIHI3
      *
             MOVE    WRK-MONEY     TO   WRK-Z9
             ADD     WRK-MONEY     TO   SUM-MONEY(IDX)
               IF   WRK-JIHI   >   ZERO
                 PERFORM  VARYING IDX2  FROM 1  BY  1
                          UNTIL    IDX2    >    10
                   INITIALIZE   WRK-JIHI2
                   COMPUTE   WRK-JIHI2   =   SYU-JIHI-TAXNASI(IDX2)
                                         +   SYU-JIHI-TAXARI(IDX2)
                   IF   WRK-JIHI2  >  ZERO
                     MOVE   WRK-JIHI2    TO   WRK-ZZ
                     COMPUTE  JIHIIDX  =  IDX2  +  6
                     ADD   WRK-JIHI2     TO   SUM-MONEY(JIHIIDX)
                     MOVE    1      TO   FLG-JIHI(IDX2)
                   END-IF
                 END-PERFORM
               END-IF
           END-IF
      *
           EVALUATE IDX
             WHEN 17
      *    自費の場合
               MOVE  SYU-SKYMONEY  TO WRK-SYUEKI
             WHEN 18
      *    労災の場合
               COMPUTE  WRK-SYUEKI ROUNDED =
                          (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
                           +  WRK-JIHI
                           +  SYU-RSISHOSHIN-MONEY
                           +  SYU-RSISAISHIN-MONEY
                           +  SYU-RSISDO-MONEY
                           +  SYU-RSIETC-MONEY
                           +  SYU-CHOSEI
                           +  SYU-YKZ-FTN
                           +  SYU-RYOYOHI-SKJ
                           +  SYU-RYOYOHI-SKJ-JIHI
                           +  SYU-RYOYOHI-LIFE
                           +  SYU-RYOYOHI-LIFE-JIHI
                           +  SYU-RMSAGAKU
             WHEN 19
      *    自賠責の場合
               MOVE    SYU-RSIJIBAI-SKYMONEY     TO   WRK-SYUEKI
      *    公害の場合
             WHEN 20
               COMPUTE  WRK-SYUEKI   =   SYU-RSIJIBAI-SKYMONEY
             WHEN OTHER
      *    労災、自賠責以外の場合
               COMPUTE  WRK-SYUEKI   =
                             (SYU-TOTAL-HKNTEN * HKN-TENTNK)
                              +  WRK-JIHI
                              +  SYU-CHOSEI
                              +  SYU-YKZ-FTN
                              +  SYU-RYOYOHI-SKJ
                              +  SYU-RYOYOHI-SKJ-JIHI
                              +  SYU-RYOYOHI-LIFE
                              +  SYU-RYOYOHI-LIFE-JIHI
                              +  SYU-RMSAGAKU
               IF  SYU-DISCOUNT-MONEY  >  0
                 COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI
                                      -  SYU-DISCOUNT-MONEY
                 EVALUATE  SYS-1001-DISCOUNT-HASU-KBN
                   WHEN  1
                     COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  2
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  3
                     IF   WRK-SYUEKI(8:1)  NOT =  0
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10  +  10
                     END-IF
                 END-EVALUATE
               END-IF
           END-EVALUATE
           ADD     WRK-SYUEKI   TO   SUM-SYUEKI
           MOVE    1            TO   FLG-SYUEKI
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<合計カウント>サブ処理(H20年4月以降)
      *****************************************************************
       330-BODY-HEN-SUB-SEC      SECTION.
      *
      *    保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM   =   1
             MOVE   10     TO  HKN-TENTNK
           END-IF
      *
      *    保険点数
           MOVE    ZERO  TO  SYU-TOTAL-HKNTEN
           EVALUATE    SYU-SYUHKNNUM
             WHEN    OTHER
               PERFORM  VARYING IDZ   FROM  1   BY  1
                        UNTIL   IDZ   >  16
                 ADD   SYU-HKNTEN(IDZ)  TO   SYU-TOTAL-HKNTEN
               END-PERFORM
               MOVE    SYU-TOTAL-HKNTEN   TO   WRK-Z9
               ADD     SYU-TOTAL-HKNTEN   TO   SUM-HKNTEN
               MOVE    1                  TO   FLG-HKN
           END-EVALUATE
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO  IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6                    TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19                   TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18                   TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    17                   TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                   TO   IDX
             WHEN    "039"
             WHEN    "040"
               MOVE    4                    TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF      SYU-KOH1HKNNUM    =   "027"
                 MOVE    4                  TO   IDX
               ELSE
                 MOVE    3                  TO   IDX
               END-IF
             WHEN    OTHER
               IF      SYU-KOH1HKNNUM    =   "027"
                 MOVE    4                  TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1           TO   IDX
                   WHEN    "2"
                     MOVE    2           TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           IF   SYU-SYUHKNNUM(1:2)  =  "98"
             MOVE    17    TO      IDX
           END-IF
      *
           IF    IDX   >   ZERO
      *
             INITIALIZE WRK-JIHI
             INITIALIZE WRK-JIHI3
               IF  SYU-SRYYMD  <=  "20070331"
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
                                     +   SYU-JIHI-TAX
               ELSE
                 COMPUTE WRK-JIHI    =   SYU-TOTAL-TGMONEY
                                     +   SYU-TOTAL-TGMONEY-TAX
                                     +   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
      *
                 COMPUTE WRK-JIHI3   =   SYU-JIHI-TOTAL
                                     +   SYU-JIHI-TOTAL-TAX
               END-IF
      *
             COMPUTE WRK-MONEY       =   SYU-SKYMONEY
                                     -   WRK-JIHI3
      *
             MOVE    WRK-MONEY     TO  WRK-Z9
             ADD     WRK-MONEY     TO  SUM-MONEY (IDX)
               IF   WRK-JIHI    >   ZERO
                 PERFORM  VARYING IDX2  FROM 1  BY  1
                          UNTIL    IDX2    >   10
                   INITIALIZE WRK-JIHI2
                   COMPUTE WRK-JIHI2  =
                               SYU-JIHI-TAXNASI(IDX2)
                            +  SYU-JIHI-TAXARI(IDX2)
                   IF  WRK-JIHI2  >  ZERO
                     MOVE WRK-JIHI2  TO  WRK-ZZ
                     COMPUTE  JIHIIDX  =  IDX2  +  6
                     ADD    WRK-JIHI2  TO     SUM-MONEY(JIHIIDX)
                     MOVE    1  TO   FLG-JIHI(IDX2)
                   END-IF
                 END-PERFORM
               END-IF
           END-IF
      *
           EVALUATE IDX
             WHEN 17
      *    自費の場合
               MOVE  SYU-SKYMONEY    TO   WRK-SYUEKI
             WHEN 18
      *    労災の場合
               COMPUTE  WRK-SYUEKI ROUNDED =
                        (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
                            +  WRK-JIHI
                            +  SYU-RSISHOSHIN-MONEY
                            +  SYU-RSISAISHIN-MONEY
                            +  SYU-RSISDO-MONEY
                            +  SYU-RSIETC-MONEY
                            +  SYU-CHOSEI
                            +  SYU-YKZ-FTN
                            +  SYU-RYOYOHI-SKJ
                            +  SYU-RYOYOHI-SKJ-JIHI
                            +  SYU-RYOYOHI-LIFE
                            +  SYU-RYOYOHI-LIFE-JIHI
                            +  SYU-RMSAGAKU
             WHEN 19
      *    自賠責の場合
               MOVE  SYU-RSIJIBAI-SKYMONEY  TO  WRK-SYUEKI
             WHEN 20
      *    公害の場合
               COMPUTE   WRK-SYUEKI   =   SYU-RSIJIBAI-SKYMONEY
             WHEN OTHER
      *    労災、自賠責以外の場合
               COMPUTE  WRK-SYUEKI =
                  (SYU-TOTAL-HKNTEN * HKN-TENTNK)
                            +  WRK-JIHI
                            +  SYU-CHOSEI
                            +  SYU-YKZ-FTN
                            +  SYU-RYOYOHI-SKJ
                            +  SYU-RYOYOHI-SKJ-JIHI
                            +  SYU-RYOYOHI-LIFE
                            +  SYU-RYOYOHI-LIFE-JIHI
                            +  SYU-RMSAGAKU
               IF  SYU-DISCOUNT-MONEY  >  0
                 COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI
                                      -  SYU-DISCOUNT-MONEY
                 EVALUATE  SYS-1001-DISCOUNT-HASU-KBN
                   WHEN  1
                     COMPUTE  WRK-SYUEKI  ROUNDED  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  2
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                     COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10
                   WHEN  3
                     IF   WRK-SYUEKI(8:1)  NOT =  0
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  /  10
                       COMPUTE  WRK-SYUEKI  =  WRK-SYUEKI  *  10  +  10
                     END-IF
                 END-EVALUATE
               END-IF
           END-EVALUATE
           ADD     WRK-SYUEKI   TO   SUM-SYUEKI
           MOVE      1          TO   FLG-SYUEKI
      *
           .
       330-BODY-HEN-SUB-EXT.
           EXIT.
      *****************************************************************
      *    件数集計サブ処理
      *****************************************************************
       320-CNT-SEC                    SECTION.
      *
      *    保険別件数カウント
           ADD     1   TO   SUM-RKEN(IDX)
      *    自費分件数カウント
           PERFORM  VARYING IDX2  FROM 1  BY  1
                     UNTIL  IDX2   >  10
             IF  FLG-JIHI(IDX2)  =  1
               COMPUTE  JIHIIDX  =  IDX2  +  6
               ADD    1   TO  SUM-RKEN(JIHIIDX)
             END-IF
           END-PERFORM
      *    保険点数件数カウント
           IF  FLG-HKN = 1
               ADD   1  TO   SUM-RHKNKEN
           END-IF
      *    医療収益計件数カウント
           IF  FLG-SYUEKI = 1
               ADD   1  TO   SUM-RSYUEKIKEN
           END-IF
      *    窓口請求金額件数カウント
           IF  FLG-SEIKYU = 1
               ADD   1  TO   SUM-RSEIKYUKEN
           END-IF
      *    窓口領収金額件数カウント
           IF  FLG-RYOSYU = 1
               ADD   1  TO   SUM-RRYOSYUKEN
           END-IF
           .
       320-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報集計サブ処理
      *****************************************************************
       320-JYURRK-CNT-SEC      SECTION.
      *
           MOVE JYURRK-SRYYMD(7:2) TO  IDX7G
           IF WRK-PARA-PRTYM  <  "200804"
             PERFORM 320-HOKEN-GROUPING-SEC
           ELSE
             PERFORM 330-HOKEN-GROUPING-SEC
           END-IF
      *    患者初診療日取得
           PERFORM 900-JYURRK2-READ-SEC
      *
           INITIALIZE               SRYACCT-REC
           MOVE  JYURRK-HOSPNUM         TO   ACCT-HOSPNUM
           MOVE  JYURRK-NYUGAIKBN       TO   ACCT-NYUGAIKBN
           MOVE  JYURRK-PTID            TO   ACCT-PTID
           MOVE  JYURRK-SRYKA           TO   ACCT-SRYKA
           MOVE  JYURRK-SRYYMD(1:6)     TO   ACCT-SRYYM
           MOVE  SRYACCT-REC            TO   MCPDATA-REC
           MOVE  "DBSELECT"             TO   MCP-FUNC
           MOVE  "tbl_sryacct"          TO   MCP-TABLE
           MOVE  "key2"                 TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE  "DBFETCH"            TO   MCP-FUNC
             MOVE  "tbl_sryacct"        TO   MCP-TABLE
             MOVE  "key2"               TO   MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE  MCPDATA-REC        TO   SRYACCT-REC
               MOVE  ZERO               TO   FLG-SRYACCT
      *+
      *    同月に初診が2回目以降の時のデータを取得
               INITIALIZE  FLG-ACCTEND
               PERFORM  UNTIL  FLG-ACCTEND  =  1
                 IF  JYURRK-ZAINUM(1)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(2)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(3)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(4)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(5)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(6)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(7)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(8)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(9)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(10)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(11)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(12)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(13)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(14)  =  ACCT-ZAINUM
                     OR  JYURRK-ZAINUM(15)  =  ACCT-ZAINUM
                   MOVE    MCPDATA-REC       TO   SRYACCT-REC
                   MOVE    ZERO              TO   FLG-SRYACCT
                   MOVE    1                 TO   FLG-ACCTEND
                 ELSE
                   MOVE    "DBFETCH"         TO   MCP-FUNC
                   MOVE    "tbl_sryacct"     TO   MCP-TABLE
                   MOVE    "key2"            TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"       USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
                   IF   MCP-RC  =  ZERO
                     MOVE    MCPDATA-REC     TO   SRYACCT-REC
                     MOVE    ZERO            TO   FLG-SRYACCT
                   ELSE
                     MOVE    1               TO   FLG-ACCTEND
                   END-IF
                 END-IF
               END-PERFORM
      *+
             ELSE
               MOVE  1                  TO   FLG-SRYACCT
               INITIALIZE                    SRYACCT-REC
             END-IF
           ELSE
             MOVE  1                    TO   FLG-SRYACCT
             INITIALIZE                      SRYACCT-REC
           END-IF
           MOVE  "DBCLOSECURSOR"        TO   MCP-FUNC
           MOVE  "tbl_sryacct"          TO   MCP-TABLE
           MOVE  "key2"                 TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   FLG-SRYACCT  =  ZERO
               AND   SYU-SRYYMD  =  JYURRK2-SRYYMD
               AND   ACCT-DAY(1 IDX7)   >   ZERO
               AND   JYURRK-RENNUM   =   1
               AND   (JYURRK-DOUJI-RENNUM  =  1
               OR   JYURRK-DOUJI-RENNUM  =  ZERO)
               AND   (JYURRK-GRP-RENNUM  =  1
               OR   JYURRK-GRP-RENNUM  =  ZERO)
      *    新患件数としてカウントする。
             INITIALIZE                 WRK-SYOSINYMD
                                        FLG-SRYKARRK
             MOVE    SYU-SRYYMD         TO   WRK-SYOSINYMD
             PERFORM  900-SRYKARRKA-SELECT-SEC
             PERFORM  UNTIL  FLG-SRYKARRK  =  1
               IF   KARRK-SYOSINYMD1  <  WRK-SYOSINYMD
                   AND   KARRK-SYOSINYMD1  NOT =  SPACE
                 MOVE    KARRK-SYOSINYMD1     TO   WRK-SYOSINYMD
               END-IF
               PERFORM  900-SRYKARRKA-FETCH-SEC
             END-PERFORM
             PERFORM  900-SRYKARRKA-CLOSE-SEC
             IF   WRK-SYOSINYMD  =  SYU-SRYYMD
      *    保険別件数カウント
               ADD    1   TO   SUM-SKEN(IDX)
      *    自費分件数カウント
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL   IDX2   >  10
                 IF  FLG-JIHI(IDX2)  = 1
                   COMPUTE  JIHIIDX  =  IDX2  +  6
                   ADD   1   TO  SUM-SKEN(JIHIIDX)
                 END-IF
               END-PERFORM
      *    保険点数件数カウント
               IF   FLG-HKN = 1
                 ADD    1    TO   SUM-SHKNKEN
               END-IF
      *    医療収益計件数カウント
               IF   FLG-SYUEKI = 1
                 ADD    1    TO   SUM-SSYUEKIKEN
               END-IF
      *    窓口請求金額件数カウント
               IF   FLG-SEIKYU = 1
                 ADD    1    TO   SUM-SSEIKYUKEN
               END-IF
      *    窓口領収金額件数カウント
               IF   FLG-RYOSYU = 1
                 ADD    1    TO   SUM-SRYOSYUKEN
               END-IF
             END-IF
           END-IF
      *
           COMPUTE  JYURRK-EDANUM  =  JYURRK-EDANUM  +  1
           MOVE    JYURRK-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key22"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO             TO   FLG-JYURRK3
             MOVE    "DBFETCH"        TO   MCP-FUNC
             MOVE    "tbl_jyurrk"     TO   MCP-TABLE
             MOVE    "key22"          TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"      USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    ZERO            TO   FLG-JYURRK3
             ELSE
               MOVE    1               TO   FLG-JYURRK3
             END-IF
           ELSE
             MOVE    1                 TO   FLG-JYURRK3
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key22"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF   FLG-SRYACCT  =  ZERO
               AND   ACCT-DAY(1 IDX7)   >   ZERO
               AND   ACCT-SRYKBN  =  "11"
               AND   (JYURRK-DOUJI-RENNUM  =  1
               OR   JYURRK-DOUJI-RENNUM  =  ZERO)
      *    初診件数としてカウントする。
      *    保険別件数カウント
             ADD    1   TO   SUM-SAKEN(IDX)
             MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *    自費分件数カウント
             PERFORM  VARYING IDX2  FROM 1  BY  1
                      UNTIL    IDX2  >  10
               IF  FLG-JIHI(IDX2)  =  1
                 COMPUTE  JIHIIDX  =  IDX2  +  6
                 ADD   1   TO  SUM-SAKEN(JIHIIDX)
               END-IF
             END-PERFORM
      *    保険点数件数カウント
             IF  FLG-HKN = 1
               ADD   1   TO   SUM-SAHKNKEN
             END-IF
      *    医療収益計件数カウント
             IF  FLG-SYUEKI = 1
               ADD   1   TO   SUM-SASYUEKIKEN
             END-IF
      *    窓口請求金額件数カウント
             IF  FLG-SEIKYU = 1
               ADD   1   TO   SUM-SASEIKYUKEN
             END-IF
      *    窓口領収金額件数カウント
             IF  FLG-RYOSYU = 1
               ADD   1   TO   SUM-SARYOSYUKEN
             END-IF
           ELSE
      *
             PERFORM  VARYING  IDX10 FROM  1  BY  1
                      UNTIL  IDX10  >  15
               MOVE    JYURRK-ZAINUM(IDX10)  TO   ACCT-ZAINUM
               MOVE  JYURRK-HOSPNUM     TO   ACCT-HOSPNUM
               MOVE  JYURRK-NYUGAIKBN   TO   ACCT-NYUGAIKBN
               MOVE  JYURRK-PTID        TO   ACCT-PTID
               MOVE  JYURRK-SRYKA       TO   ACCT-SRYKA
               MOVE  JYURRK-SRYYMD(1:6) TO   ACCT-SRYYM
               MOVE  SRYACCT-REC        TO   MCPDATA-REC
               MOVE  "DBSELECT"         TO   MCP-FUNC
               MOVE  "tbl_sryacct"      TO   MCP-TABLE
               MOVE  "key5"             TO   MCP-PATHNAME
               CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
               IF     MCP-RC  =  ZERO
                 MOVE   "DBFETCH"       TO   MCP-FUNC
                 MOVE   "tbl_sryacct"   TO   MCP-TABLE
                 MOVE   "key5"          TO   MCP-PATHNAME
                 CALL   "ORCDBMAIN"     USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                 IF   MCP-RC   =  ZERO
                   MOVE MCPDATA-REC     TO   SRYACCT-REC
                   MOVE ZERO            TO   FLG-SRYACCT
                 ELSE
                   MOVE   1             TO   FLG-SRYACCT
                   INITIALIZE                SRYACCT-REC
                 END-IF
               ELSE
                 MOVE    1              TO   FLG-SRYACCT
                 INITIALIZE                  SRYACCT-REC
               END-IF
      *
               MOVE  "DBCLOSECURSOR"    TO   MCP-FUNC
               MOVE   "tbl_sryacct"     TO   MCP-TABLE
               MOVE   "key5"            TO   MCP-PATHNAME
               CALL   "ORCDBMAIN"       USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
               IF  IDX10  <  15
                 COMPUTE   IDX11   =   IDX10  +   1
                 IF   JYURRK-ZAINUM(IDX11)  =  ZERO
                   MOVE    16               TO   IDX10
                 END-IF
               END-IF
               IF   ACCT-SRYKBN   =  "12"
                 MOVE    16               TO   IDX10
               END-IF
             END-PERFORM
      *
             IF     FLG-SRYACCT    =  ZERO
                AND   ACCT-DAY(1 IDX7)  >  ZERO
                AND   ACCT-SRYKBN   =  "12"
      *
      *    再診の件数としてカウントする。
      *    保険別件数カウント
               ADD     1   TO   SUM-RKEN(IDX)
               MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *    自費分件数カウント
               PERFORM  VARYING IDX2  FROM 1  BY  1
                        UNTIL    IDX2  >  10
                 IF  FLG-JIHI(IDX2)  = 1
                   COMPUTE   JIHIIDX  =  IDX2  +  6
                   ADD     1      TO    SUM-RKEN(JIHIIDX)
                 END-IF
               END-PERFORM
      *    保険点数件数カウント
               IF  FLG-HKN = 1
                    ADD   1  TO   SUM-RHKNKEN
               END-IF
      *    医療収益計件数カウント
               IF  FLG-SYUEKI = 1
                    ADD   1  TO   SUM-RSYUEKIKEN
               END-IF
      *    窓口請求金額件数カウント
               IF  FLG-SEIKYU = 1
                    ADD   1  TO   SUM-RSEIKYUKEN
               END-IF
      *    窓口領収金額件数カウント
               IF  FLG-RYOSYU = 1
                    ADD   1  TO   SUM-RRYOSYUKEN
               END-IF
      *
             ELSE
               IF   FLG-JYURRK3  =  1
      *    その他の件数としてカウントする。
      *    保険別件数カウント
                 ADD     1   TO   SUM-OKEN(IDX)
      *    自費分件数カウント
                 PERFORM  VARYING IDX2  FROM 1  BY  1
                          UNTIL    IDX2       >    10
                 IF  FLG-JIHI(IDX2)  = 1
                   COMPUTE  JIHIIDX  =  IDX2  +  6
                   ADD   1   TO  SUM-OKEN(JIHIIDX)
                 END-IF
                          END-PERFORM
      *    保険点数件数カウント
                 IF  FLG-HKN = 1
                   ADD   1  TO   SUM-OHKNKEN
                 END-IF
      *    医療収益計件数カウント
                 IF  FLG-SYUEKI = 1
                   ADD   1  TO   SUM-OSYUEKIKEN
                 END-IF
      *    窓口請求金額件数カウント
                 IF  FLG-SEIKYU = 1
                   ADD   1  TO   SUM-OSEIKYUKEN
                 END-IF
      *    窓口領収金額件数カウント
                 IF  FLG-RYOSYU = 1
                   ADD   1  TO   SUM-ORYOSYUKEN
                 END-IF
               END-IF
             END-IF
      *
           END-IF
      *
           MOVE     1        TO      IDX3
           PERFORM  UNTIL    IDX3    >      10000
             IF  (WRK-SMEI-PTID(IDX-S) =  WRK-JYUSHIN-PTID(IDX3))
                 AND (WRK-SMEI-NYUHEN-YMD(IDX-S)(7:2)
                                =  WRK-NYUHEN-YMD(IDX3))
      *
               MOVE   10000     TO      IDX3
             END-IF
             IF  WRK-JYUSHIN-PTID(IDX3)  =  ZERO
                 AND  WRK-NYUHEN-YMD(IDX3)  =  SPACE
                 AND  (IDX3 NOT = 10000)
               MOVE   WRK-SMEI-PTID(IDX-S)   TO  WRK-JYUSHIN-PTID(IDX3)
               MOVE   WRK-SMEI-NYUHEN-YMD(IDX-S)(7:2)
                                             TO  WRK-NYUHEN-YMD(IDX3)
               ADD    1         TO    SUM-JYUSHINKEN-GK
               MOVE   10000     TO    IDX3
             END-IF
             ADD   1          TO     IDX3
           END-PERFORM
      *
           .
       320-JYURRK-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険グループ判別処理
      *****************************************************************
       320-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO  IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6               TO  IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                   TO  IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19                   TO  IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18                   TO  IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    17                   TO  IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF      SYU-KOH1HKNNUM   =   "027"
                 MOVE    5                   TO  IDX
               ELSE
                 MOVE    4                   TO  IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    3                   TO  IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1                   TO  IDX
                   WHEN    "2"
                     MOVE    2                   TO  IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      *    保険別区分例外条件(自費条件拡張)
           IF   SYU-SYUHKNNUM(1:2) = "98"
             MOVE    17    TO      IDX
           END-IF
      *
           .
       320-HOKEN-GROUPING-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険グループ判別処理(H20年4月以降)
      *****************************************************************
       330-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                     TO   IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6                    TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    20                   TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    19                   TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    18                   TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    17                   TO   IDX
             WHEN    "039"
             WHEN    "040"
               MOVE    4                    TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF      SYU-KOH1HKNNUM      =   "027"
                 MOVE    4                  TO   IDX
               ELSE
                 MOVE    3                  TO   IDX
               END-IF
             WHEN    OTHER
               IF      SYU-KOH1HKNNUM      =   "027"
                 MOVE    4                  TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1              TO   IDX
                   WHEN    "2"
                     MOVE    2              TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           IF   SYU-SYUHKNNUM(1:2) = "98"
             MOVE      17      TO      IDX
           END-IF
      *
           .
       320-HOKEN-GROUPING-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *    保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYU-HOSPNUM         TO   COMB-HOSPNUM
           MOVE    SYU-PTID            TO   COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO   COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF   FLG-HKNCOMBI   =   ZERO
      *    患者保険情報読み込み
             INITIALIZE              PTHKNINF-REC
             MOVE    SYU-HOSPNUM       TO  PTHKN-HOSPNUM
             MOVE    SYU-PTID          TO  PTHKN-PTID
             MOVE    COMB-HKNID        TO  PTHKN-HKNID
             PERFORM   900-PTHKNINF-READ-SEC
           END-IF
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票<診療科合計>印刷処理
      *****************************************************************
      *
       330-TERM-KA-PRINT-SEC         SECTION.
      *
           INITIALIZE                HC17B
      *
           MOVE    WRK-SYSYMDWH      TO    HC17B-SYSYMD
           MOVE    WRK-PRTYMWH       TO    HC17B-SRYYM
           MOVE    "対象年月"        TO    HC17B-MIDASI
      *
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               MOVE "入院"         TO  HC17B-NYUGAIKBN
      *    入院自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10
                 MOVE   WRK-TJIHIN(IDX2)    TO   HC17B-TJIHI(IDX2)
               END-PERFORM
               MOVE   "請求件数"    TO   HC17B-KENSU1(1)
               MOVE   "請求件数"    TO   HC17B-KENSU1(2)
             WHEN "2"
               MOVE   "外来"        TO  HC17B-NYUGAIKBN
      *    外来自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10
                 MOVE   WRK-TJIHI(IDX2)  TO   HC17B-TJIHI(IDX2)
               END-PERFORM
               MOVE   "再診件数"    TO   HC17B-KENSU1(1)
               MOVE   "初診件数"    TO   HC17B-KENSU2(1)
               MOVE   "その他件数"  TO   HC17B-KENSU4(1)
               MOVE   "(新患数)"    TO   HC17B-KENSU3(1)
               MOVE   "再診件数"    TO   HC17B-KENSU1(2)
               MOVE   "初診件数"    TO   HC17B-KENSU2(2)
               MOVE   "その他件数"  TO   HC17B-KENSU4(2)
               MOVE   "(新患数)"    TO   HC17B-KENSU3(2)
           END-EVALUATE
      *
      *    診療科名称取得
           INITIALIZE                   SYS-1005-REC
           MOVE    "1005"               TO   SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO   SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM          TO   SYS-1005-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD     TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD     TO   SYS-1005-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD     TO   SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC         TO   MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2    =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1005-REC
             MOVE   SYS-1005-SRYKANAME  TO   HC17B-SRYKA
           ELSE
             MOVE   SPACE               TO   HC17B-SRYKA
           END-IF
      *
           IF  FLG-GET = ZERO
      *    対象データが１件も存在しなかった場合
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                                   TO  HC17B-JOBMSG
             MOVE    1         TO   WRK-PAGE
             MOVE    WRK-PAGE  TO   HC17B-PAGE
           ELSE
      *    対象データが１件以上存在した場合
             ADD     1         TO   CNT-PAGE
             MOVE    CNT-PAGE  TO   WRK-PAGE
             MOVE    WRK-PAGE  TO   HC17B-PAGE
      *    保険点数、件数合計
             MOVE    SUM-HKNTEN   TO  WRK-Z9
             MOVE    WRK-Z9   TO  HC17B-GHKNTEN
             IF   WRK-NYUGAIKBN   =   "2"
               MOVE    SUM-SAHKNKEN        TO   WRK-Z9
               MOVE    WRK-Z9              TO   HC17B-SHKNKEN
               MOVE    SUM-SHKNKEN         TO   WRK-Z9
               MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
               MOVE    1                   TO   WRK-COUNTS
               INSPECT WRK-TMP-K  TALLYING
                       WRK-COUNTS FOR LEADING SPACE
               MOVE    WRK-TMP-K      TO   HC17B-SAHKNKEN(2:8)
               MOVE    "("            TO   HC17B-SAHKNKEN(WRK-COUNTS:1)
               MOVE    ")"            TO   HC17B-SAHKNKEN(10:1)
               MOVE    SUM-OHKNKEN    TO   WRK-Z9
               MOVE    WRK-Z9         TO   HC17B-OHKNTEN
             END-IF
             MOVE    SUM-RHKNKEN   TO   WRK-Z9
             MOVE    WRK-Z9        TO   HC17B-RHKNKEN
      *
      *    保険別金額、件数合計
             PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL    IDX   >    20
               IF    IDX = 7
                   OR  IDX = 8
                   OR  IDX = 9
                   OR  IDX = 10
                   OR  IDX = 11
                   OR  IDX = 12
                   OR  IDX = 13
                   OR  IDX = 14
                   OR  IDX = 15
                   OR  IDX = 16
                 COMPUTE IDX8 = IDX - 6
                 EVALUATE WRK-NYUGAIKBN
                   WHEN "1"
                     MOVE   SUM-MONEY(IDX)    TO   WRK-S9
                     MOVE   WRK-S9            TO   HC17B-GMONEY(IDX)
                     MOVE   SUM-RKEN(IDX)     TO   WRK-Z9
                     MOVE   WRK-Z9            TO   HC17B-RKENSU(IDX)
                   WHEN "2"
                     MOVE   SUM-MONEY(IDX)   TO   WRK-S9
                     MOVE   WRK-S9           TO   HC17B-GMONEY(IDX)
                     MOVE   SUM-RKEN(IDX)    TO   WRK-Z9
                     MOVE   WRK-Z9           TO   HC17B-RKENSU(IDX)
                     MOVE   SUM-OKEN(IDX)    TO   WRK-Z9
                     MOVE   WRK-Z9           TO   HC17B-OKENSU(IDX)
                    MOVE    SUM-SAKEN(IDX)    TO   WRK-Z9
                     MOVE    WRK-Z9            TO   HC17B-SKENSU (IDX)
                     MOVE    SUM-SKEN (IDX)    TO   WRK-Z9
                     MOVE    WRK-Z9(3:8)       TO   WRK-TMP-K
                     MOVE    1                 TO   WRK-COUNTS
                     INSPECT WRK-TMP-K  TALLYING
                             WRK-COUNTS FOR LEADING SPACE
                     MOVE    WRK-TMP-K   TO   HC17B-SAKENSU(IDX)(2:8)
                     MOVE    "("   TO   HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                     MOVE    ")"   TO   HC17B-SAKENSU(IDX)(10:1)
                 END-EVALUATE
               ELSE
                 MOVE    SUM-MONEY(IDX)    TO   WRK-S9
                 MOVE    WRK-S9            TO   HC17B-GMONEY(IDX)
                 MOVE    SUM-RKEN(IDX)     TO   WRK-Z9
                 MOVE    WRK-Z9            TO   HC17B-RKENSU(IDX)
                 IF WRK-NYUGAIKBN =   "2"
                  MOVE    SUM-SAKEN(IDX)      TO   WRK-Z9
                   MOVE    WRK-Z9              TO   HC17B-SKENSU(IDX)
                   MOVE    SUM-SKEN (IDX)      TO   WRK-Z9
                   MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
                   MOVE    1                   TO   WRK-COUNTS
                   INSPECT WRK-TMP-K  TALLYING
                           WRK-COUNTS FOR LEADING SPACE
                   MOVE    WRK-TMP-K  TO  HC17B-SAKENSU(IDX)(2:8)
                   MOVE    "("     TO  HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                   MOVE    ")"     TO  HC17B-SAKENSU(IDX)(10:1)
                   MOVE    SUM-OKEN (IDX)       TO  WRK-Z9
                   MOVE    WRK-Z9   TO  HC17B-OKENSU  (IDX)
                 END-IF
               END-IF
             END-PERFORM
      *
      *    医療収益計、窓口請求金額合計、窓口領収金額合計
      *    金額合計
             MOVE   SUM-SYUEKI     TO   WRK-S9
             MOVE   WRK-S9         TO   HC17B-GSYUEKI
             MOVE   SUM-SEIKYU     TO   WRK-S9
             MOVE   WRK-S9         TO   HC17B-GSEIKYU
             MOVE   SUM-RYOSYU     TO   WRK-S9
             MOVE   WRK-S9         TO   HC17B-GRYOSYU
      *
      *    再診
             MOVE  SUM-RSYUEKIKEN    TO   WRK-Z9
             MOVE  WRK-Z9            TO   HC17B-RSYUEKIKEN
      *
      *    初診
             IF WRK-NYUGAIKBN   =  "2"
               MOVE    SUM-SASYUEKIKEN     TO  WRK-Z9
               MOVE    WRK-Z9              TO  HC17B-SSYUEKIKEN
               MOVE    SUM-SSYUEKIKEN      TO  WRK-Z9
               MOVE    WRK-Z9(3:8)         TO  WRK-TMP-K
               MOVE    1                   TO  WRK-COUNTS
               INSPECT WRK-TMP-K  TALLYING
                       WRK-COUNTS FOR LEADING SPACE
               MOVE    WRK-TMP-K  TO  HC17B-SASYUEKIKEN(2:8)
               MOVE    "("        TO  HC17B-SASYUEKIKEN(WRK-COUNTS:1)
               MOVE    ")"        TO  HC17B-SASYUEKIKEN(10:1)
      *    その他
               MOVE  SUM-OSYUEKIKEN    TO   WRK-Z9
               MOVE  WRK-Z9            TO   HC17B-OSYUEKIKEN
             END-IF
      *
           END-IF
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                   TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM   TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
           IF  WRK-PARA-PRTYM   <  "200804"
             MOVE   "A00000M501O.red"      TO   SPRT-PRTID
           ELSE
             MOVE   "A00000M501N.red"      TO   SPRT-PRTID
           END-IF
           MOVE    "月計表合計（窓口領収金）"  TO   SPRT-TITLE
           MOVE    HC17B   TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID     TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM      TO   SPRT-PRTNM
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF   SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
             PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
           .
       330-TERM-KA-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票<入外区分別全科合計>印刷処理
      *****************************************************************
      *
       330-TERM-GK-PRINT-SEC         SECTION.
      *
           INITIALIZE            HC17B
      *
           MOVE    WRK-SYSYMDWH      TO   HC17B-SYSYMD
           MOVE    WRK-PRTYMWH       TO   HC17B-SRYYM
           MOVE    "対象年月"        TO   HC17B-MIDASI
      *
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               MOVE   "入院"         TO  HC17B-NYUGAIKBN
      *    入院自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10
                 MOVE   WRK-TJIHIN(IDX2)   TO   HC17B-TJIHI(IDX2)
               END-PERFORM
               MOVE   "請求件数"            TO  HC17B-KENSU1(1)
               MOVE   "請求件数"            TO  HC17B-KENSU1(2)
             WHEN "2"
               MOVE "外来"         TO  HC17B-NYUGAIKBN
      *    外来自費名称設定
               PERFORM  VARYING  IDX2   FROM 1  BY  1
                        UNTIL    IDX2   >  10
                 MOVE     WRK-TJIHI(IDX2)   TO   HC17B-TJIHI(IDX2)
               END-PERFORM
               MOVE    "再診件数"    TO   HC17B-KENSU1(1)
               MOVE    "初診件数"    TO   HC17B-KENSU2(1)
               MOVE    "その他件数"  TO   HC17B-KENSU4(1)
               MOVE    "(新患数)"    TO   HC17B-KENSU3(1)
               MOVE    "再診件数"    TO   HC17B-KENSU1(2)
               MOVE    "初診件数"    TO   HC17B-KENSU2(2)
               MOVE    "その他件数"  TO   HC17B-KENSU4(2)
               MOVE    "(新患数)"    TO   HC17B-KENSU3(2)
               MOVE    "受診者数"    TO   HC17B-TJYUSHIN
           END-EVALUATE
      *    診療科名称
           EVALUATE WRK-NYUGAIKBN
             WHEN "1"
               IF   WRK-PRNSRKA-CNT  >   1
      *    診療科が２つ以上の場合は全科合計を設定する。
                 MOVE   "全科合計"    TO  HC17B-SRYKA
               ELSE
      *    診療科名称取得
      *    診療科が１つの場合は診療科名称を設定する。
                 INITIALIZE                 SYS-1005-REC
                 MOVE  "1005"               TO   SYS-1005-KANRICD
                 MOVE  WRK-SRYKA            TO   SYS-1005-KBNCD
                 MOVE  SPA-HOSPNUM          TO   SYS-1005-HOSPNUM
                 MOVE  WRK-PARA-PRTLYMD     TO   WRK-SYMD
                 MOVE  WRK-PARA-PRTSYMD     TO   SYS-1005-EDYUKYMD
                 MOVE  WRK-PARA-PRTLYMD     TO   SYS-1005-STYUKYMD
                 MOVE  SYS-1005-REC         TO   MCPDATA-REC
                 PERFORM 820-SYSKANRI-READ-SEC
                 IF    FLG-SYSKANRI2    =   ZERO
                   MOVE   MCPDATA-REC        TO   SYS-1005-REC
                   MOVE   SYS-1005-SRYKANAME TO   HC17B-SRYKA
                 ELSE
                   MOVE  SPACE               TO   HC17B-SRYKA
                 END-IF
               END-IF
             WHEN "2"
               IF  WRK-PRGSRKA-CNT  >   1
      *    診療科が２つ以上の場合は全科合計を設定する。
                 MOVE  "全科合計"    TO  HC17B-SRYKA
               ELSE
      *    診療科名称取得
      *    診療科が１つの場合は診療科名称を設定する。
                 INITIALIZE                SYS-1005-REC
                 MOVE  "1005"              TO   SYS-1005-KANRICD
                 MOVE  WRK-SRYKA           TO   SYS-1005-KBNCD
                 MOVE  SPA-HOSPNUM         TO   SYS-1005-HOSPNUM
                 MOVE  WRK-PARA-PRTLYMD    TO   WRK-SYMD
                 MOVE  WRK-PARA-PRTSYMD    TO   SYS-1005-EDYUKYMD
                 MOVE  WRK-PARA-PRTLYMD    TO   SYS-1005-STYUKYMD
                 MOVE  SYS-1005-REC        TO  MCPDATA-REC
                 PERFORM 820-SYSKANRI-READ-SEC
                 IF    FLG-SYSKANRI2        =   ZERO
                   MOVE  MCPDATA-REC       TO   SYS-1005-REC
                   MOVE  SYS-1005-SRYKANAME TO   HC17B-SRYKA
                 ELSE
                   MOVE  SPACE             TO   HC17B-SRYKA
                 END-IF
               END-IF
           END-EVALUATE
      *
           IF  FLG-GET = ZERO
      *    対象データが１件も存在しなかった場合
             MOVE  "＊＊＊  対象データが１件も存在しません。＊＊＊"
                     TO  HC17B-JOBMSG
             MOVE    1         TO   WRK-PAGE
             MOVE    WRK-PAGE  TO   HC17B-PAGE
           ELSE
      *
      *    対象データが１件以上存在した場合
             ADD     1         TO   CNT-PAGE
             MOVE    CNT-PAGE  TO   WRK-PAGE
             MOVE    WRK-PAGE  TO   HC17B-PAGE
      *    保険点数、件数合計
             MOVE    SUM-HKNTEN-GK    TO   WRK-Z9
             MOVE    WRK-Z9           TO   HC17B-GHKNTEN
             IF WRK-NYUGAIKBN   =  "2"
               MOVE    SUM-SAHKNKEN-GK     TO   WRK-Z9
               MOVE    WRK-Z9              TO   HC17B-SHKNKEN
               MOVE    SUM-SHKNKEN-GK      TO   WRK-Z9
               MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
               MOVE    1                   TO   WRK-COUNTS
               INSPECT WRK-TMP-K  TALLYING
                       WRK-COUNTS FOR LEADING SPACE
               MOVE    WRK-TMP-K  TO   HC17B-SAHKNKEN(2:8)
               MOVE    "("        TO   HC17B-SAHKNKEN(WRK-COUNTS:1)
               MOVE    ")"        TO   HC17B-SAHKNKEN(10:1)
               MOVE    SUM-OHKNKEN-GK   TO   WRK-Z9
               MOVE    WRK-Z9           TO   HC17B-OHKNTEN
             END-IF
             MOVE    SUM-RHKNKEN-GK   TO   WRK-Z9
             MOVE    WRK-Z9           TO   HC17B-RHKNKEN
      *
      *    保険別金額、件数合計
             PERFORM  VARYING IDX FROM    1   BY  1
                      UNTIL    IDX   >    20
               IF    IDX = 7
                 OR  IDX = 8
                 OR  IDX = 9
                 OR  IDX = 10
                 OR  IDX = 11
                 OR  IDX = 12
                 OR  IDX = 13
                 OR  IDX = 14
                 OR  IDX = 15
                 OR  IDX = 16
                 COMPUTE IDX8 = IDX - 6
                 EVALUATE WRK-NYUGAIKBN
                   WHEN "1"
                     MOVE   SUM-MONEY-GK(IDX)  TO   WRK-S9
                     MOVE   WRK-S9             TO   HC17B-GMONEY(IDX)
                     MOVE   SUM-RKEN-GK(IDX)   TO   WRK-Z9
                     MOVE   WRK-Z9             TO   HC17B-RKENSU(IDX)
                   WHEN "2"
                     MOVE   SUM-MONEY-GK (IDX) TO   WRK-S9
                     MOVE   WRK-S9             TO   HC17B-GMONEY(IDX)
                     MOVE   SUM-RKEN-GK (IDX)  TO   WRK-Z9
                     MOVE   WRK-Z9             TO   HC17B-RKENSU(IDX)
                     MOVE   SUM-OKEN-GK (IDX)  TO   WRK-Z9
                     MOVE   WRK-Z9   TO  HC17B-OKENSU(IDX)
                     MOVE    SUM-SAKEN-GK(IDX)   TO   WRK-Z9
                     MOVE    WRK-Z9              TO   HC17B-SKENSU(IDX)
                     MOVE    SUM-SKEN-GK (IDX)   TO   WRK-Z9
                     MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
                     MOVE    1                   TO   WRK-COUNTS
                     INSPECT WRK-TMP-K  TALLYING
                             WRK-COUNTS FOR LEADING SPACE
                     MOVE    WRK-TMP-K  TO   HC17B-SAKENSU(IDX)(2:8)
                     MOVE    "("   TO   HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                     MOVE    ")"   TO   HC17B-SAKENSU(IDX)(10:1)
                 END-EVALUATE
               ELSE
                 MOVE    SUM-MONEY-GK(IDX)   TO   WRK-S9
                 MOVE    WRK-S9              TO   HC17B-GMONEY(IDX)
                 MOVE    SUM-RKEN-GK(IDX)    TO   WRK-Z9
                 MOVE    WRK-Z9              TO   HC17B-RKENSU(IDX)
                 IF   WRK-NYUGAIKBN   =   "2"
                   MOVE    SUM-SAKEN-GK(IDX)   TO   WRK-Z9
                   MOVE    WRK-Z9              TO   HC17B-SKENSU(IDX)
                   MOVE    SUM-SKEN-GK (IDX)   TO   WRK-Z9
                   MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
                   MOVE    1                   TO   WRK-COUNTS
                   INSPECT WRK-TMP-K  TALLYING
                           WRK-COUNTS FOR LEADING SPACE
                   MOVE    WRK-TMP-K  TO  HC17B-SAKENSU(IDX)(2:8)
                   MOVE    "("    TO  HC17B-SAKENSU(IDX)(WRK-COUNTS:1)
                   MOVE    ")"    TO  HC17B-SAKENSU(IDX)(10:1)
                   MOVE    SUM-OKEN-GK(IDX)       TO  WRK-Z9
                   MOVE    WRK-Z9    TO  HC17B-OKENSU(IDX)
                 END-IF
               END-IF
             END-PERFORM
      *
      *    医療収益計、窓口請求金額合計、窓口領収金額合計
      *    金額合計
             MOVE  SUM-SYUEKI-GK      TO   WRK-S9
             MOVE  WRK-S9             TO   HC17B-GSYUEKI
             MOVE  SUM-SEIKYU-GK      TO   WRK-S9
             MOVE  WRK-S9             TO   HC17B-GSEIKYU
             MOVE  SUM-RYOSYU-GK      TO   WRK-S9
             MOVE  WRK-S9             TO   HC17B-GRYOSYU
      *
      *    再診
             MOVE  SUM-RSYUEKIKEN-GK    TO   WRK-Z9
             MOVE  WRK-Z9               TO   HC17B-RSYUEKIKEN
      *
      *    初診
             IF WRK-NYUGAIKBN   =   "2"
               MOVE    SUM-SASYUEKIKEN-GK  TO   WRK-Z9
               MOVE    WRK-Z9              TO   HC17B-SSYUEKIKEN
               MOVE    SUM-SSYUEKIKEN-GK   TO   WRK-Z9
               MOVE    WRK-Z9(3:8)         TO   WRK-TMP-K
               MOVE    1                   TO   WRK-COUNTS
               INSPECT WRK-TMP-K  TALLYING
                       WRK-COUNTS FOR LEADING SPACE
               MOVE    WRK-TMP-K   TO  HC17B-SASYUEKIKEN(2:8)
               MOVE    "("         TO  HC17B-SASYUEKIKEN(WRK-COUNTS:1)
               MOVE    ")"         TO  HC17B-SASYUEKIKEN(10:1)
      *
      *    その他
               MOVE  SUM-OSYUEKIKEN-GK    TO   WRK-Z9
               MOVE  WRK-Z9               TO   HC17B-OSYUEKIKEN
      *
               MOVE  SUM-JYUSHINKEN-GK TO   WRK-Z9
               MOVE  WRK-Z9            TO   HC17B-JYUSHINCNT
      *
             END-IF
           END-IF
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                   TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
           IF  WRK-PARA-PRTYM  <  "200804"
             MOVE   "A00000M501O.red"      TO   SPRT-PRTID
           ELSE
             MOVE   "A00000M501N.red"      TO   SPRT-PRTID
           END-IF
           MOVE    "月計表合計（窓口領収金）"    TO   SPRT-TITLE
           MOVE    HC17B                   TO   SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID     TO   SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO   SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM      TO   SPRT-PRTNM
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF    SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"   TO  WRK-RECEERR
             PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
           .
       330-TERM-GK-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF    STS-RECEERR    =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT              RECEERR-FILE
      *
             MOVE    WRK-RECEERR     TO   RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"           TO   SJOBKANRI-MODE
             INITIALIZE                   JOBKANRI-REC
             MOVE    SPA-HOSPNUM     TO   JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID  TO   JOB-JOBID
             MOVE    WRK-PARA-SHELLID
                                     TO   JOB-SHELLID
             MOVE    WRK-RECEERR     TO   JOB-YOBI
             MOVE    "9999"          TO   JOB-ERRCD
             CALL    "ORCSJOB"       USING
                                     ORCSJOBKANRIAREA
                                     JOBKANRI-REC
                                     SPA-AREA
           END-IF
      *
           MOVE    1                 TO   FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"                TO   SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM          TO   JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID       TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID     TO   JOB-SHELLID
           MOVE    CNT-PAGE             TO   JOB-UPDCNT
           CALL    "ORCSJOB"            USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE               STS-AREA-DAY
           INITIALIZE               LNK-DAY7-AREA
           MOVE    "71"             TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD         TO   LNK-DAY7-YM
           CALL    "ORCSDAY"        USING
                                    STS-AREA-DAY
                                    LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI  TO   WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細情報
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  SYUMEI-REC
      *
           MOVE    WRK-PARA-HOSPNUM   TO   SMEI-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD   TO   SMEI-NYUHEN-YMD
           MOVE    WRK-PARA-PRTLYMD   TO   SMEI-UPNYUHEN-YMD
      *
           MOVE    SYUMEI-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE   "tbl_syumei"        TO  MCP-TABLE
           MOVE   "st_key3"           TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"         USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF    MCP-RC   =   ZERO
             PERFORM 910-DBFETCH-SEC
           ELSE
             MOVE    1           TO  FLG-SYUMEI
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細情報検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO  MCP-FUNC
           MOVE   "tbl_syumei"      TO  MCP-TABLE
           MOVE   "st_key3"         TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"       USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF    MCP-RC  =   ZERO
             MOVE    ZERO           TO   FLG-SYUMEI
             MOVE    MCPDATA-REC    TO   SYUMEI-REC
           ELSE
             MOVE    1              TO   FLG-SYUMEI
           END-IF
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE   "tbl_syumei"      TO  MCP-TABLE
           MOVE   "st_key3"         TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"       USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      ******************************************************************
      *    受診履歴SELECT
      ******************************************************************
       900-JYURRK-SELECT-SEC                 SECTION.

           INITIALIZE                         JYURRK-REC
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)    TO   JYURRK-HOSPNUM
           MOVE    WRK-SMEI-PTID(IDX-S)       TO   JYURRK-PTID
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)  TO   JYURRK-NYUGAIKBN
           MOVE    WRK-SMEI-DENPNUM(IDX-S)    TO   JYURRK-DENPNUM
           MOVE    JYURRK-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"             TO   MCP-FUNC
           MOVE    "tbl_jyurrk"           TO   MCP-TABLE
           MOVE    "key6 "                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  900-JYURRK-FETCH-SEC
           ELSE
             MOVE    1                    TO   FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.

      ******************************************************************
      *    受診履歴FETCH
      ******************************************************************
       900-JYURRK-FETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key6"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC       TO   JYURRK-REC
             MOVE    ZERO              TO   FLG-JYURRK
           ELSE
             MOVE    1                 TO   FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-FETCH-EXT.
           EXIT.

      ******************************************************************
      *    受診履歴CLOSE
      ******************************************************************
       900-JYURRK-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key6"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       900-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE               SYUNOU-REC
           MOVE    SMEI-HOSPNUM     TO    SYU-HOSPNUM
           MOVE    SMEI-NYUGAIKBN   TO   SYU-NYUGAIKBN
           MOVE    SMEI-PTID        TO   SYU-PTID
           MOVE    SMEI-DENPNUM     TO   SYU-DENPNUM
      *
           MOVE    SYUNOU-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"       TO   MCP-FUNC
           MOVE   "tbl_syunou"      TO  MCP-TABLE
           MOVE   "key"             TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"       USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF    MCP-RC  =   ZERO
             MOVE    "DBFETCH"      TO   MCP-FUNC
             MOVE   "tbl_syunou"    TO   MCP-TABLE
             MOVE   "key"           TO   MCP-PATHNAME
             CALL   "ORCDBMAIN"     USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC    TO   SYUNOU-REC
               MOVE    ZERO           TO   FLG-SYUNOU
             ELSE
               MOVE    1              TO   FLG-SYUNOU
             END-IF
           ELSE
               MOVE    1              TO   FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE   "tbl_syunou"        TO  MCP-TABLE
           MOVE   "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"         USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       901-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                   SYUNOU-REC
      *
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)     TO   SYU-HOSPNUM
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO   SYU-NYUGAIKBN
           MOVE    WRK-SMEI-PTID(IDX-S)        TO   SYU-PTID
           MOVE    WRK-SMEI-DENPNUM(IDX-S)     TO   SYU-DENPNUM
      *
           MOVE    SYUNOU-REC                  TO   MCPDATA-REC
           MOVE    "DBSELECT"                  TO   MCP-FUNC
           MOVE    "tbl_syunou"                TO   MCP-TABLE
           MOVE    "key"                       TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"                 USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF  MCP-RC    =   ZERO
             MOVE    "DBFETCH"              TO   MCP-FUNC
             MOVE   "tbl_syunou"            TO   MCP-TABLE
             MOVE   "key"                   TO   MCP-PATHNAME
             CALL   "ORCDBMAIN"             USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
             IF   MCP-RC    =   ZERO
               MOVE    MCPDATA-REC          TO   SYUNOU-REC
               MOVE    ZERO                 TO   FLG-SYUNOU
             ELSE
               MOVE    1                    TO   FLG-SYUNOU
             END-IF
           ELSE
             MOVE    1                      TO   FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"          TO   MCP-FUNC
           MOVE   "tbl_syunou"              TO   MCP-TABLE
           MOVE   "key"                     TO   MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
           .
       901-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報検索
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                      HKNNUM-REC
           MOVE    SYU-HOSPNUM         TO   HKN-HOSPNUM
           MOVE    SYU-SYUHKNNUM       TO   HKN-HKNNUM
           MOVE    WRK-PARA-PRTSYMD    TO   HKN-TEKSTYMD
           MOVE    ALL "9"             TO   HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC    =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE   "tbl_hknnum"       TO  MCP-TABLE
             MOVE   "key5"             TO  MCP-PATHNAME
             CALL   "ORCDBMAIN"        USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF      MCP-RC          =  ZERO
               MOVE    MCPDATA-REC     TO   HKNNUM-REC
               MOVE    ZERO            TO   FLG-HKNNUM
             ELSE
               MOVE    1               TO   FLG-HKNNUM
             END-IF
           ELSE
             MOVE    1                 TO   FLG-HKNNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_hknnum"        TO   MCP-TABLE
           MOVE    "key5"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key10"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_syskanri"    TO   MCP-TABLE
             MOVE    "key10"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF    MCP-RC   =   ZERO
               MOVE    ZERO            TO   FLG-SYSKANRI
             ELSE
               MOVE    1               TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                 TO   FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key10"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_syskanri"     TO   MCP-TABLE
             MOVE    "key10"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-SYSKANRI2
             ELSE
               MOVE    1                TO   FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       820-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_syskanri"     TO   MCP-TABLE
             MOVE    "key10"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    ZERO             TO   FLG-SYSKANRI2
             ELSE
               MOVE    1                TO   FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       820-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_ptinf"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_ptinf"       TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC    =   ZERO
               MOVE    MCPDATA-REC     TO   PTINF-REC
               MOVE    ZERO            TO   FLG-PTINF
             ELSE
               MOVE    1               TO   FLG-PTINF
               INITIALIZE              PTINF-REC
             END-IF
           ELSE
             MOVE    1                 TO   FLG-PTINF
             INITIALIZE                PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_ptinf"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
            MOVE    "tbl_pthkninf"     TO   MCP-TABLE
            MOVE    "key"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_pthkninf"    TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO   PTHKNINF-REC
               MOVE    ZERO            TO   FLG-PTHKNINF
             ELSE
               MOVE    1               TO   FLG-PTHKNINF
               INITIALIZE              PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1                 TO   FLG-PTHKNINF
             INITIALIZE                PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_pthkninf"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_hkncombi"     TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF    MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   HKNCOMBI-REC
               MOVE    ZERO             TO   FLG-HKNCOMBI
             ELSE
               MOVE    1                TO   FLG-HKNCOMBI
               INITIALIZE               HKNCOMBI-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-HKNCOMBI
             INITIALIZE                 HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_hkncombi"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_sryacct"        TO   MCP-TABLE
           MOVE    "key3"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_sryacct"      TO   MCP-TABLE
             MOVE    "key3"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   SRYACCT-REC
               MOVE    ZERO             TO   FLG-SRYACCT
             ELSE
               MOVE    1                TO   FLG-SRYACCT
               INITIALIZE               SRYACCT-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SRYACCT
             INITIALIZE                 SRYACCT-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_sryacct"        TO   MCP-TABLE
           MOVE    "key3"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報検索処理
      *****************************************************************
       900-JYURRK2-READ-SEC                SECTION.
      *
           INITIALIZE                  JYURRK2-REC
      *
           MOVE    JYURRK-HOSPNUM      TO   JYURRK2-HOSPNUM
           MOVE    JYURRK-PTID         TO   JYURRK2-PTID
           MOVE    JYURRK-NYUGAIKBN    TO   JYURRK2-NYUGAIKBN
           MOVE    "00000000"          TO   JYURRK2-SRYYMD
           MOVE    "99999999"          TO   JYURRK2-UPSRYYMD
      *
           MOVE    JYURRK2-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key14"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_jyurrk"      TO   MCP-TABLE
             MOVE    "key14"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO   JYURRK2-REC
               MOVE    ZERO            TO   FLG-JYURRK2
             ELSE
               MOVE    1               TO   FLG-JYURRK2
             END-IF
           ELSE
             MOVE    1                 TO   FLG-JYURRK2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key14"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-JYURRK2-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴情報検索処理
      *****************************************************************
       900-SRYKARRKA-SELECT-SEC            SECTION.
      *
           INITIALIZE               SRYKARRK-REC
      *
           MOVE    SYU-HOSPNUM      TO   KARRK-HOSPNUM
           MOVE    SYU-PTID         TO   KARRK-PTID
      *
           MOVE    SRYKARRK-REC     TO   MCPDATA-REC
           MOVE    "DBSELECT"       TO   MCP-FUNC
           MOVE    "tbl_srykarrk"  TO   MCP-TABLE
           MOVE    "key2"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM  900-SRYKARRKA-FETCH-SEC
           ELSE
             MOVE    1               TO   FLG-SRYKARRK
           END-IF
      *
           .
       900-SRYKARRKA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴情報検索処理
      *****************************************************************
       900-SRYKARRKA-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"         TO   MCP-FUNC
           MOVE    "tbl_srykarrk"   TO   MCP-TABLE
           MOVE    "key2"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC     TO   SRYKARRK-REC
             MOVE    ZERO            TO   FLG-SRYKARRK
           ELSE
             MOVE    1               TO   FLG-SRYKARRK
           END-IF
      *
           .
       900-SRYKARRKA-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴情報検索処理
      *****************************************************************
       900-SRYKARRKA-CLOSE-SEC            SECTION.
      *
           MOVE    SRYKARRK-REC     TO   MCPDATA-REC
           MOVE    "DBCLOSECURSOR"  TO   MCP-FUNC
           MOVE    "tbl_srykarrk"  TO   MCP-TABLE
           MOVE    "key2"           TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       900-SRYKARRKA-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"             TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
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
           MOVE    "DBCOMMIT"           TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO   MCP-FUNC.
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
