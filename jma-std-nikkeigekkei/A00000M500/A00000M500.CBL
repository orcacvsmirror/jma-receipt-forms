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
       PROGRAM-ID.             A00000M500.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 月計表
      *  管理者            :
      *  作成日付    作業者       記述
      *  03/09/29    @@@@-有村    新規作成(ORCBM500のID変更Ver)
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/07  コメント履歴削除
      *                                     初診、再診、その他の判定条件を変更
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-門間    11/04/18  処理速度の改善、コメント履歴削除
      *****************************************************************
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
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
           COPY    "A00000M500.INC".
           COPY    "A00000M5001.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET                PIC 9(01).
           03  FLG-END                PIC 9(01).
           03  FLG-SYSKANRI           PIC 9(01).
           03  FLG-SYSKANRI2          PIC 9(01).
           03  FLG-SYSKANRI3          PIC 9(01).
           03  FLG-INPUT              PIC 9(01).
           03  FLG-HKNCOMBI           PIC 9(01).
           03  FLG-PTINF              PIC 9(01).
           03  FLG-PTHKNINF           PIC 9(01).
           03  FLG-SYUMEI             PIC 9(01).
           03  FLG-SYUNOU             PIC 9(01).
           03  FLG-SRYACCT            PIC 9(01).
           03  FLG-JYURRK             PIC 9(01).
           03  FLG-HKNNUM             PIC 9(01).
           03  FLG-PTNYUINRRK         PIC 9(01).
           03  FLG-SRYKA-ALL          PIC 9(01).
           03  FLG-SRYKAGK            PIC 9(01).
           03  FLG-SRYACT             PIC 9(01).
           03  FLG-JYURRK2            PIC 9(01).
           03  FLG-ACCTEND            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE               PIC 9(02).
           03  CNT-PAGE               PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY             PIC 9(02).
               05  SYS-MM             PIC 9(02).
               05  SYS-DD             PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                   PIC 9(04).
           03  IDX2                   PIC 9(04).
           03  IDX3                   PIC 9(04).
           03  IDX4                   PIC 9(04).
           03  IDX5                   PIC 9(04).
           03  IDX6                   PIC 9(04).
           03  IDX7G.
              05  IDX7                PIC 9(02).
           03  IDX8                   PIC 9(04).
           03  IDX9                   PIC 9(04).
           03  IDX                    PIC 9(04).
           03  IDY                    PIC 9(04).
           03  IDZ                    PIC 9(04).
           03  IDX-S                  PIC 9(07).
           03  IDX-N                  PIC 9(07).
           03  IDX-MAX                PIC 9(07).
           03  IDX10                  PIC 9(02).
           03  IDX11                  PIC 9(02).
      *
           03  JIHIIDX                PIC 9(04).
           03  NG                     PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID         PIC 9(07).
           03  WRK-PARA-SHELLID       PIC X(08).
           03  WRK-PARA-HOSPNUM       PIC 9(02).
           03  WRK-PARA-PRTYM         PIC X(06).
           03  WRK-PARA-PRTSYMD       PIC X(08).
           03  WRK-PARA-PRTLYMD       PIC X(08).
           03  WRK-PARA-NYUGAIKBN     PIC X(01).
           03  WRK-PARA-SRYKA         PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH               PIC X(64).
      *
           03  WRK-RECEERR            PIC X(200).
      *
           03  WRK-PARA-DENPPRTYMDWH  PIC X(22).
           03  WRK-SYSYMDWH           PIC X(22).
           03  WRK-RENNUM             PIC 9(03).
           03  WRK-PTNUM              PIC X(20).
           03  WRK-PTNUM6             PIC 9(06).
           03  WRK-SYUHKNNUM          PIC X(03).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY          PIC 9(04).
               05  WRK-SYSMM          PIC 9(02).
               05  WRK-SYSDD          PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY            PIC 9(04).
               05  WRK-SMM            PIC 9(02).
               05  WRK-SDD            PIC 9(02).
           03  WRK-HENYMDG            PIC X(22).
      *
           03  WRK-MONEY              PIC 9(08).
           03  WRK-JIHI               PIC 9(08).
           03  WRK-SYUEKI             PIC S9(08).
      *
           03  WRK-JIHI2              PIC 9(08).
           03  WRK-JIHI4              PIC 9(08).
           03  WRK-TJIHI-G       OCCURS  10.
               05  WRK-TJIHI          PIC X(08).
           03  WRK-TJIHIN-G       OCCURS  10.
               05  WRK-TJIHIN         PIC X(08).
      *
           03  WRK-4001-TENTANKA      PIC 9(02)V9(01).
      *
           03  WRK-RENNUM-X           PIC ZZ9.
           03  WRK-PAGE               PIC ZZ9.
           03  WRK-Z9                 PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ                 PIC ZZZZZZZZZZZZ.
           03  WRK-S9                 PIC -----------9.
           03  WRK-SZ                 PIC -----------Z.
           03  WRK-NYUGAIKBN          PIC X(01).
           03  WRK-SRYKA              PIC X(02).
           03  DEBUG-CNT              PIC 9(03).
      *
           03  WRK-SYS-BEDSU          PIC 9(04).
           03  WRK-SYS-BEDSUIPN       PIC 9(04).
           03  WRK-SYS-NYUGAIKBN      PIC 9(04).
           03  WRK-COMPFTN            PIC S9(10).
           03  WRK-DENPNUM            PIC 9(07).
      *
       01  SUM-AREA                VALUE   ZERO.
           03  SRYKAMAX               PIC   9(03).
           03  NYUGAI-TBL          OCCURS  2.
             05  NYUGAIKBN            PIC   X(01).
             05  NYUGAINAME           PIC   X(04).
             05  SRYKA-TBL            OCCURS  100.
               07  SRYKA              PIC   X(02).
               07  SRYKANAME          PIC   X(20).
               07  HKNTYPE-TBL     OCCURS  11.
                 09  ITEM-TBL      OCCURS  40.
                   11  YOSO           PIC S9(11).
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
      *
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
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    自費管理情報
           COPY    "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    労災情報取得
           COPY    "CPSK4001.INC".
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
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
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
      *
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
           PERFORM   100-INIT-SEC
      *
           PERFORM   200-MAIN-SEC
                     UNTIL   FLG-END  =  1
      *
           PERFORM   300-END-SEC
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
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-SRYKA
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO   SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"              TO   SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM        TO   JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID     TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID   TO   JOB-SHELLID
           MOVE    "A00000M500"       TO   JOB-PGID
           MOVE    "保険別診療点数 月計表"  TO  JOB-SHELLMSG
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
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
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTYM        TO   WRK-SYMD(1:6)
           PERFORM   31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD              TO   WRK-PARA-PRTLYMD
      *
      *    対象年月初日
           MOVE    WRK-PARA-PRTYM        TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                  TO   WRK-PARA-PRTSYMD(7:2)
      *
      *    対象年月日編集
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTLYMD      TO   WRK-SYMD
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO   WRK-PARA-DENPPRTYMDWH
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
             MOVE    SYS-1001-BEDSU      TO   WRK-SYS-BEDSU
             MOVE    SYS-1001-BEDSUIPN   TO   WRK-SYS-BEDSUIPN
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO   WRK-RECEERR
             PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
      *    診療科情報
           IF   WRK-PARA-SRYKA  =  "00"
               OR    WRK-PARA-SRYKA  =  SPACE
             IF   WRK-PARA-SRYKA  =  "00"
               MOVE    1     TO   FLG-SRYKAGK
             END-IF
             IF   WRK-PARA-SRYKA  =  SPACE
               MOVE    1     TO   FLG-SRYKA-ALL
             END-IF
           ELSE
	         IF   WRK-PARA-SRYKA(2:1) =   SPACE
               MOVE    "診療科コードが不正です。"   TO   WRK-RECEERR
               PERFORM   500-ERR-HENSYU-SEC
             ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO   SYS-1005-KANRICD
               MOVE    WRK-PARA-SRYKA      TO   SYS-1005-KBNCD
               MOVE    SPA-HOSPNUM         TO   SYS-1005-HOSPNUM
               MOVE    WRK-PARA-PRTSYMD    TO   SYS-1005-EDYUKYMD
               MOVE    WRK-PARA-PRTLYMD    TO   SYS-1005-STYUKYMD
               MOVE    SYS-1005-REC        TO   MCPDATA-REC
               PERFORM   800-SYSKANRI-READ-SEC
               IF   FLG-SYSKANRI   =   ZERO
                 MOVE    MCPDATA-REC       TO   SYS-1005-REC
               ELSE
      *    診療科コード入力値エラーの場合
                 MOVE    "指定された診療科コードの値はありません。"
                                                     TO   WRK-RECEERR
                 PERFORM   500-ERR-HENSYU-SEC
               END-IF
             END-IF
           END-IF
      *
      *    入院自費情報取得
      *    区分11
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "11"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =  ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(1)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHIN(1)
           END-IF
      *
      *    区分12
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "12"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(2)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(2)
           END-IF
      *
      *    区分13
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "13"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(3)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(3)
           END-IF
      *
      *    区分14
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "14"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(4)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(4)
           END-IF
      *
      *    区分15
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "15"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(5)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(5)
           END-IF
      *
      *    区分16
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "16"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(6)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(6)
           END-IF
      *
      *    区分17
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "17"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(7)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(7)
           END-IF
      *
      *    区分18
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "18"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC         TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME   TO   WRK-TJIHIN(8)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHIN(8)
           END-IF
      *
      *    区分19
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "19"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC         TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME   TO   WRK-TJIHIN(9)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHIN(9)
           END-IF
      *
      *    区分10
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "10"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHIN(10)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHIN(10)
           END-IF
      *
      *    外来自費情報取得
      *    区分21
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "21"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(1)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHI(1)
           END-IF
      *
      *    区分22
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "22"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(2)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(2)
           END-IF
      *
      *    区分23
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "23"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(3)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(3)
           END-IF
      *
      *    区分24
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "24"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(4)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(4)
           END-IF
      *
      *    区分25
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "25"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC         TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME   TO   WRK-TJIHI(5)
           ELSE
             MOVE  SPACE               TO   WRK-TJIHI(5)
           END-IF
      *
      *    区分26
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "26"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE  WRK-PARA-PRTSYMD      TO   SYS-1013-EDYUKYMD
           MOVE  WRK-PARA-PRTLYMD      TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE  MCPDATA-REC       TO   SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO   WRK-TJIHI(6)
           ELSE
               MOVE  SPACE             TO   WRK-TJIHI(6)
           END-IF
      *
      *    区分27
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "27"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(7)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(7)
           END-IF
      *
      *    区分28
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "28"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(8)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(8)
           END-IF
      *
      *    区分29
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "29"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(9)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(9)
           END-IF
      *
      *    区分20
           MOVE    SPACE               TO   SYS-1013-REC
           INITIALIZE                  SYS-1013-REC
           MOVE    "1013"              TO   SYS-1013-KANRICD
           MOVE    "20"                TO   SYS-1013-KBNCD
           MOVE    SPA-HOSPNUM         TO   SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD    TO   SYS-1013-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO   SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE   MCPDATA-REC        TO   SYS-1013-REC
             MOVE   SYS-1013-JIHINAME  TO   WRK-TJIHI(10)
           ELSE
             MOVE   SPACE              TO   WRK-TJIHI(10)
           END-IF
      *
      *    労災医療機関情報
           MOVE    SPACE                TO   SYS-4001-REC
           INITIALIZE                   SYS-4001-REC
           MOVE    "4001"               TO   SYS-4001-KANRICD
           MOVE    "*"                  TO   SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM          TO   SYS-4001-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD     TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD     TO   SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD     TO   SYS-4001-STYUKYMD
           MOVE    SYS-4001-REC         TO   MCPDATA-REC
           PERFORM   810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2   =   ZERO
             MOVE    MCPDATA-REC        TO   SYS-4001-REC
             EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                 MOVE    11.5           TO   WRK-4001-TENTANKA
               WHEN  "2"
                 MOVE    12             TO   WRK-4001-TENTANKA
               WHEN  OTHER
                 MOVE    10             TO   WRK-4001-TENTANKA
             END-EVALUATE
           ELSE
             MOVE    10                 TO   WRK-4001-TENTANKA
           END-IF
      *
           MOVE    "1005"               TO   SYS-1005-KANRICD
           MOVE    SPA-HOSPNUM          TO   SYS-1005-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD     TO   WRK-SYMD
           MOVE    WRK-PARA-PRTSYMD     TO   SYS-1005-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD     TO   SYS-1005-STYUKYMD
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           MOVE    1                    TO   IDX1
           PERFORM  UNTIL  FLG-SYSKANRI3   =  1
             IF   FLG-SYSKANRI3 = ZERO
               MOVE   SYS-1005-KBNCD      TO   SRYKA(1 IDX1)
               MOVE   SYS-1005-SRYKANAME1 TO   SRYKANAME(1 IDX1)
               MOVE   SYS-1005-KBNCD      TO   SRYKA(2 IDX1)
               MOVE   SYS-1005-SRYKANAME1 TO   SRYKANAME(2 IDX1)
               ADD     1                  TO   IDX1
             END-IF
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
           MOVE    "全科合計"             TO   SRYKANAME(1 IDX1)
           MOVE    "全科合計"             TO   SRYKANAME(2 IDX1)
           MOVE    IDX1                   TO   SRYKAMAX
      *
           MOVE    "1"                    TO   NYUGAIKBN(1)
           MOVE    "入院"                 TO   NYUGAINAME(1)
           MOVE    "2"                    TO   NYUGAIKBN(2)
           MOVE    "外来"                 TO   NYUGAINAME(2)
      *
           IF   WRK-PARA-NYUGAIKBN  NOT =  "1"  AND  "2"
             MOVE    SPACE                TO   WRK-PARA-NYUGAIKBN
           END-IF
           MOVE   WRK-PARA-NYUGAIKBN      TO   WRK-SYS-NYUGAIKBN
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
      *    収納情報集計処理
           PERFORM   910-DBSELECT-SEC
           PERFORM   320-SYUNOU-CNT-SEC
      *    入院履歴情報集計処理
           PERFORM   910-DBSELECT2-SEC
           PERFORM   320-PTNYUINRRK-CNT-SEC
      *    帳票編集印刷処理
           PERFORM   330-TERM-HEN-SEC
           MOVE   1        TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報集計処理
      *****************************************************************
       320-SYUNOU-CNT-SEC      SECTION.
      *
           PERFORM  UNTIL   FLG-SYUNOU  = 1
      *
      *    集計対象年月のデータであるかをチェックする。
             IF   SYU-SRYYMD(1:6)  =  WRK-PARA-PRTYM
                 AND  SYU-HOSPNUM   =  WRK-PARA-HOSPNUM
                 AND  SYU-CREATEKBN   NOT =  "3"
                 AND  SYU-DENPJTIKBN  NOT =  "3"
      *
               EVALUATE SYU-NYUGAIKBN
                 WHEN "1"
                   MOVE     1           TO   NG
                 WHEN "2"
                   MOVE     2           TO   NG
                 WHEN OTHER
                   MOVE     2           TO   NG
               END-EVALUATE
      *
               MOVE   SYU-NYUGAIKBN     TO   WRK-NYUGAIKBN
               MOVE   SYU-SRYKA         TO   WRK-SRYKA
               PERFORM   320-SRYKA-GROUPING-SEC
               IF  WRK-PARA-PRTYM  <  "200804"
                 PERFORM   320-HOKEN-GROUPING-SEC
               ELSE
                 PERFORM   330-HOKEN-GROUPING-SEC
               END-IF
      *
               IF   IDX  >  ZERO
                 IF  FLG-GET  =  ZERO
                   MOVE   1   TO   FLG-GET
                 END-IF
                 PERFORM   320-SYUNOU-CNT-SUB-SEC
               END-IF
             END-IF
             PERFORM   910-DBFETCH-SEC
           END-PERFORM
      *
           .
       320-SYUNOU-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報集計サブ処理
      *****************************************************************
       320-SYUNOU-CNT-SUB-SEC      SECTION.
      *
      *    保険番号情報読み込み
           PERFORM   900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =  1
      *    該当レコードが存在しない場合は単価を10円に設定する。
             MOVE 10     TO HKN-TENTNK
           END-IF
      *
      *    自費以外のとき自費分だけ集計
           IF   IDX   =   7
             MOVE    ZERO            TO    WRK-JIHI
           ELSE
             IF  SYU-SRYYMD  <=  "20070331"
               COMPUTE  WRK-JIHI   =   SYU-TOTAL-TGMONEY
                                   +   SYU-TOTAL-TGMONEY-TAX
                                   +   SYU-JIHI-TOTAL
                                   +   SYU-JIHI-TOTAL-TAX
                                   +   SYU-JIHI-TAX
             ELSE
               COMPUTE  WRK-JIHI   =   SYU-TOTAL-TGMONEY
                                   +   SYU-TOTAL-TGMONEY-TAX
                                   +   SYU-JIHI-TOTAL
                                   +   SYU-JIHI-TOTAL-TAX
             END-IF
           END-IF
           COMPUTE  WRK-MONEY   =   SYU-SKYMONEY
                                -   WRK-JIHI
      *
           MOVE    SRYKAMAX          TO   IDX3
      *
           MOVE    ZERO              TO   SYU-TOTAL-HKNTEN
      *    診療科保険点数
           ADD    SYU-SSU-HKNTEN     TO   YOSO (NG IDX1 IDX 5)
           ADD    SYU-SSU-HKNTEN     TO   YOSO (NG IDX1 10 5)
           ADD    SYU-SSU-HKNTEN     TO   YOSO (NG IDX3 IDX 5)
           ADD    SYU-SSU-HKNTEN     TO   YOSO (NG IDX3 10 5)
           ADD    SYU-SSU-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    指導料保険点数
           ADD    SYU-SDO-HKNTEN     TO   YOSO (NG IDX1 IDX 6)
           ADD    SYU-SDO-HKNTEN     TO   YOSO (NG IDX1 10 6)
           ADD    SYU-SDO-HKNTEN     TO   YOSO (NG IDX3 IDX 6)
           ADD    SYU-SDO-HKNTEN     TO   YOSO (NG IDX3 10 6)
           ADD    SYU-SDO-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    在宅料保険点数
           ADD    SYU-ZTK-HKNTEN     TO   YOSO (NG IDX1 IDX 7)
           ADD    SYU-ZTK-HKNTEN     TO   YOSO (NG IDX1 10 7)
           ADD    SYU-ZTK-HKNTEN     TO   YOSO (NG IDX3 IDX 7)
           ADD    SYU-ZTK-HKNTEN     TO   YOSO (NG IDX3 10 7)
           ADD    SYU-ZTK-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    投薬料保険点数
           ADD    SYU-TYK-HKNTEN     TO   YOSO (NG IDX1 IDX 8)
           ADD    SYU-TYK-HKNTEN     TO   YOSO (NG IDX1 10 8)
           ADD    SYU-TYK-HKNTEN     TO   YOSO (NG IDX3 IDX 8)
           ADD    SYU-TYK-HKNTEN     TO   YOSO (NG IDX3 10 8)
           ADD    SYU-TYK-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    注射料保険点数
           ADD    SYU-CSY-HKNTEN     TO   YOSO (NG IDX1 IDX 9)
           ADD    SYU-CSY-HKNTEN     TO   YOSO (NG IDX1 10 9)
           ADD    SYU-CSY-HKNTEN     TO   YOSO (NG IDX3 IDX 9)
           ADD    SYU-CSY-HKNTEN     TO   YOSO (NG IDX3 10 9)
           ADD    SYU-CSY-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    処置料保険点数
           ADD    SYU-SYC-HKNTEN     TO   YOSO (NG IDX1 IDX 10)
           ADD    SYU-SYC-HKNTEN     TO   YOSO (NG IDX1 10 10)
           ADD    SYU-SYC-HKNTEN     TO   YOSO (NG IDX3 IDX 10)
           ADD    SYU-SYC-HKNTEN     TO   YOSO (NG IDX3 10 10)
           ADD    SYU-SYC-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    手術料保険点数
           ADD    SYU-SJT-HKNTEN     TO   YOSO (NG IDX1 IDX 11)
           ADD    SYU-SJT-HKNTEN     TO   YOSO (NG IDX1 10 11)
           ADD    SYU-SJT-HKNTEN     TO   YOSO (NG IDX3 IDX 11)
           ADD    SYU-SJT-HKNTEN     TO   YOSO (NG IDX3 10 11)
           ADD    SYU-SJT-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    麻酔点数
           ADD    SYU-MSI-HKNTEN     TO   YOSO (NG IDX1 IDX 12)
           ADD    SYU-MSI-HKNTEN     TO   YOSO (NG IDX1 10 12)
           ADD    SYU-MSI-HKNTEN     TO   YOSO (NG IDX3 IDX 12)
           ADD    SYU-MSI-HKNTEN     TO   YOSO (NG IDX3 10 12)
           ADD    SYU-MSI-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    検査料保険点数
           ADD    SYU-KNS-HKNTEN     TO   YOSO (NG IDX1 IDX 13)
           ADD    SYU-KNS-HKNTEN     TO   YOSO (NG IDX1 10 13)
           ADD    SYU-KNS-HKNTEN     TO   YOSO (NG IDX3 IDX 13)
           ADD    SYU-KNS-HKNTEN     TO   YOSO (NG IDX3 10 13)
           ADD    SYU-KNS-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    X線料保険点数
           ADD    SYU-GZU-HKNTEN     TO   YOSO (NG IDX1 IDX 14)
           ADD    SYU-GZU-HKNTEN     TO   YOSO (NG IDX1 10 14)
           ADD    SYU-GZU-HKNTEN     TO   YOSO (NG IDX3 IDX 14)
           ADD    SYU-GZU-HKNTEN     TO   YOSO (NG IDX3 10 14)
           ADD    SYU-GZU-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    リハ点数
           ADD    SYU-ETC-HKNTEN     TO   YOSO (NG IDX1 IDX 15)
           ADD    SYU-ETC-HKNTEN     TO   YOSO (NG IDX1 10 15)
           ADD    SYU-ETC-HKNTEN     TO   YOSO (NG IDX3 IDX 15)
           ADD    SYU-ETC-HKNTEN     TO   YOSO (NG IDX3 10 15)
           ADD    SYU-ETC-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    精神科専門点数
           ADD    SYU-SSN-HKNTEN     TO   YOSO (NG IDX1 IDX 16)
           ADD    SYU-SSN-HKNTEN     TO   YOSO (NG IDX1 10 16)
           ADD    SYU-SSN-HKNTEN     TO   YOSO (NG IDX3 IDX 16)
           ADD    SYU-SSN-HKNTEN     TO   YOSO (NG IDX3 10 16)
           ADD    SYU-SSN-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    放射線治療点数
           ADD    SYU-HOU-HKNTEN     TO   YOSO (NG IDX1 IDX 17)
           ADD    SYU-HOU-HKNTEN     TO   YOSO (NG IDX1 10 17)
           ADD    SYU-HOU-HKNTEN     TO   YOSO (NG IDX3 IDX 17)
           ADD    SYU-HOU-HKNTEN     TO   YOSO (NG IDX3 10 17)
           ADD    SYU-HOU-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    病理診断点数
           ADD    SYU-BYR-HKNTEN     TO   YOSO (NG IDX1 IDX 40)
           ADD    SYU-BYR-HKNTEN     TO   YOSO (NG IDX1 10 40)
           ADD    SYU-BYR-HKNTEN     TO   YOSO (NG IDX3 IDX 40)
           ADD    SYU-BYR-HKNTEN     TO   YOSO (NG IDX3 10 40)
           ADD    SYU-BYR-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    入院料保険点数
           ADD    SYU-NYN-HKNTEN   TO   YOSO (NG IDX1 IDX 18)
           ADD    SYU-NYN-HKNTEN   TO   YOSO (NG IDX1 10 18)
           ADD    SYU-NYN-HKNTEN   TO   YOSO (NG IDX3 IDX 18)
           ADD    SYU-NYN-HKNTEN   TO   YOSO (NG IDX3 10 18)
           ADD    SYU-NYN-HKNTEN   TO   SYU-TOTAL-HKNTEN
      *
      *    療養担当手当点数
           ADD    SYU-RYO-HKNTEN     TO   YOSO (NG IDX1 IDX 19)
           ADD    SYU-RYO-HKNTEN     TO   YOSO (NG IDX1 10 19)
           ADD    SYU-RYO-HKNTEN     TO   YOSO (NG IDX3 IDX 19)
           ADD    SYU-RYO-HKNTEN     TO   YOSO (NG IDX3 10 19)
           ADD    SYU-RYO-HKNTEN     TO   SYU-TOTAL-HKNTEN
      *
      *    合計保険点数
           ADD    SYU-TOTAL-HKNTEN   TO   YOSO (NG IDX1 IDX 20)
           ADD    SYU-TOTAL-HKNTEN   TO   YOSO (NG IDX1 10 20)
           ADD    SYU-TOTAL-HKNTEN   TO   YOSO (NG IDX3 IDX 20)
           ADD    SYU-TOTAL-HKNTEN   TO   YOSO (NG IDX3 10 20)
      *
      *    保険適用外金額
           ADD    SYU-TOTAL-TGMONEY       TO   YOSO (NG IDX1 IDX 21)
           ADD    SYU-TOTAL-TGMONEY       TO   YOSO (NG IDX1 10  21)
           ADD    SYU-TOTAL-TGMONEY-TAX   TO   YOSO (NG IDX1 IDX 21)
           ADD    SYU-TOTAL-TGMONEY-TAX   TO   YOSO (NG IDX1 10  21)
           ADD    SYU-TOTAL-TGMONEY       TO   YOSO (NG IDX3 IDX 21)
           ADD    SYU-TOTAL-TGMONEY       TO   YOSO (NG IDX3 10  21)
           ADD    SYU-TOTAL-TGMONEY-TAX   TO   YOSO (NG IDX3 IDX 21)
           ADD    SYU-TOTAL-TGMONEY-TAX   TO   YOSO (NG IDX3 10  21)
      *
      *    負担金の集計
           IF   NG   =   1
      *    入院の場合
      *    食事療養費
             COMPUTE   YOSO (NG IDX1 IDX 22)  =  YOSO (NG IDX1 IDX 22)
                                              +  SYU-RYOYOHI-SKJ
                                              +  SYU-RYOYOHI-SKJ-JIHI
                                              +  SYU-RYOYOHI-LIFE
             COMPUTE   YOSO (NG IDX1 10 22)   =  YOSO (NG IDX1 10 22)
                                              +  SYU-RYOYOHI-SKJ
                                              +  SYU-RYOYOHI-SKJ-JIHI
                                              +  SYU-RYOYOHI-LIFE
             COMPUTE   YOSO (NG IDX3 IDX 22)  =  YOSO (NG IDX3 IDX 22)
                                              +  SYU-RYOYOHI-SKJ
                                              +  SYU-RYOYOHI-SKJ-JIHI
                                              +  SYU-RYOYOHI-LIFE
             COMPUTE   YOSO (NG IDX3 10 22)   =  YOSO (NG IDX3 10 22)
                                              +  SYU-RYOYOHI-SKJ
                                              +  SYU-RYOYOHI-SKJ-JIHI
                                              +  SYU-RYOYOHI-LIFE
      *    食事負担額
             COMPUTE   YOSO (NG IDX1 IDX 23)  =  YOSO (NG IDX1 IDX 23)
                                            +  SYU-SKYMONEY-SKJ-KEI
                                            +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                            +  SYU-SKYMONEY-LIFE-KEI
             COMPUTE   YOSO (NG IDX1 10 23)  =  YOSO (NG IDX1 10 23)
                                            +  SYU-SKYMONEY-SKJ-KEI
                                            +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                            +  SYU-SKYMONEY-LIFE-KEI
             COMPUTE   YOSO (NG IDX3 IDX 23)  =  YOSO (NG IDX3 IDX 23)
                                            +  SYU-SKYMONEY-SKJ-KEI
                                            +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                            +  SYU-SKYMONEY-LIFE-KEI
             COMPUTE   YOSO (NG IDX3 10 23)  =  YOSO (NG IDX3 10 23)
                                            +  SYU-SKYMONEY-SKJ-KEI
                                            +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                            +  SYU-SKYMONEY-LIFE-KEI
      *    室料差額
             ADD SYU-RMSAGAKU        TO   YOSO (NG IDX1 IDX 24)
             ADD SYU-RMSAGAKU        TO   YOSO (NG IDX1 10 24)
             ADD SYU-RMSAGAKU        TO   YOSO (NG IDX3 IDX 24)
             ADD SYU-RMSAGAKU        TO   YOSO (NG IDX3 10 24)
           ELSE
      *    外来の場合
      *    薬剤一部負担金
             ADD    SYU-YKZ-FTN      TO   YOSO (NG IDX1 IDX 24)
             ADD    SYU-YKZ-FTN      TO   YOSO (NG IDX1 10 24)
             ADD    SYU-YKZ-FTN      TO   YOSO (NG IDX3 IDX 24)
             ADD    SYU-YKZ-FTN      TO   YOSO (NG IDX3 10 24)
           END-IF
      *    自費1
           ADD    SYU-JIHI-1         TO   YOSO (NG IDX1 IDX 25)
           ADD    SYU-JIHI-1         TO   YOSO (NG IDX1 10 25)
           ADD    SYU-JIHI-1-TAX     TO   YOSO (NG IDX1 IDX 25)
           ADD    SYU-JIHI-1-TAX     TO   YOSO (NG IDX1 10 25)
           ADD    SYU-JIHI-1         TO   YOSO (NG IDX3 IDX 25)
           ADD    SYU-JIHI-1         TO   YOSO (NG IDX3 10 25)
           ADD    SYU-JIHI-1-TAX     TO   YOSO (NG IDX3 IDX 25)
           ADD    SYU-JIHI-1-TAX     TO   YOSO (NG IDX3 10 25)
      *
      *    自費2
           ADD    SYU-JIHI-2         TO   YOSO (NG IDX1 IDX 26)
           ADD    SYU-JIHI-2         TO   YOSO (NG IDX1 10 26)
           ADD    SYU-JIHI-2-TAX     TO   YOSO (NG IDX1 IDX 26)
           ADD    SYU-JIHI-2-TAX     TO   YOSO (NG IDX1 10 26)
           ADD    SYU-JIHI-2         TO   YOSO (NG IDX3 IDX 26)
           ADD    SYU-JIHI-2         TO   YOSO (NG IDX3 10 26)
           ADD    SYU-JIHI-2-TAX     TO   YOSO (NG IDX3 IDX 26)
           ADD    SYU-JIHI-2-TAX     TO   YOSO (NG IDX3 10 26)
      *
      *    自費3
           ADD    SYU-JIHI-3         TO   YOSO (NG IDX1 IDX 27)
           ADD    SYU-JIHI-3         TO   YOSO (NG IDX1 10 27)
           ADD    SYU-JIHI-3-TAX     TO   YOSO (NG IDX1 IDX 27)
           ADD    SYU-JIHI-3-TAX     TO   YOSO (NG IDX1 10 27)
           ADD    SYU-JIHI-3         TO   YOSO (NG IDX3 IDX 27)
           ADD    SYU-JIHI-3         TO   YOSO (NG IDX3 10 27)
           ADD    SYU-JIHI-3-TAX     TO   YOSO (NG IDX3 IDX 27)
           ADD    SYU-JIHI-3-TAX     TO   YOSO (NG IDX3 10 27)
      *
      *    自費4
           ADD    SYU-JIHI-4         TO   YOSO (NG IDX1 IDX 28)
           ADD    SYU-JIHI-4         TO   YOSO (NG IDX1 10 28)
           ADD    SYU-JIHI-4-TAX     TO   YOSO (NG IDX1 IDX 28)
           ADD    SYU-JIHI-4-TAX     TO   YOSO (NG IDX1 10 28)
           ADD    SYU-JIHI-4         TO   YOSO (NG IDX3 IDX 28)
           ADD    SYU-JIHI-4         TO   YOSO (NG IDX3 10 28)
           ADD    SYU-JIHI-4-TAX     TO   YOSO (NG IDX3 IDX 28)
           ADD    SYU-JIHI-4-TAX     TO   YOSO (NG IDX3 10 28)
      *
      *    自費5
           ADD    SYU-JIHI-5         TO   YOSO (NG IDX1 IDX 29)
           ADD    SYU-JIHI-5         TO   YOSO (NG IDX1 10 29)
           ADD    SYU-JIHI-5-TAX     TO   YOSO (NG IDX1 IDX 29)
           ADD    SYU-JIHI-5-TAX     TO   YOSO (NG IDX1 10 29)
           ADD    SYU-JIHI-5         TO   YOSO (NG IDX3 IDX 29)
           ADD    SYU-JIHI-5         TO   YOSO (NG IDX3 10 29)
           ADD    SYU-JIHI-5-TAX     TO   YOSO (NG IDX3 IDX 29)
           ADD    SYU-JIHI-5-TAX     TO   YOSO (NG IDX3 10 29)
      *
      *    自費6
           ADD    SYU-JIHI-6         TO   YOSO (NG IDX1 IDX 30)
           ADD    SYU-JIHI-6         TO   YOSO (NG IDX1 10 30)
           ADD    SYU-JIHI-6-TAX     TO   YOSO (NG IDX1 IDX 30)
           ADD    SYU-JIHI-6-TAX     TO   YOSO (NG IDX1 10 30)
           ADD    SYU-JIHI-6         TO   YOSO (NG IDX3 IDX 30)
           ADD    SYU-JIHI-6         TO   YOSO (NG IDX3 10 30)
           ADD    SYU-JIHI-6-TAX     TO   YOSO (NG IDX3 IDX 30)
           ADD    SYU-JIHI-6-TAX     TO   YOSO (NG IDX3 10 30)
      *
      *    自費7
           ADD    SYU-JIHI-7         TO   YOSO (NG IDX1 IDX 31)
           ADD    SYU-JIHI-7         TO   YOSO (NG IDX1 10 31)
           ADD    SYU-JIHI-7-TAX     TO   YOSO (NG IDX1 IDX 31)
           ADD    SYU-JIHI-7-TAX     TO   YOSO (NG IDX1 10 31)
           ADD    SYU-JIHI-7         TO   YOSO (NG IDX3 IDX 31)
           ADD    SYU-JIHI-7         TO   YOSO (NG IDX3 10 31)
           ADD    SYU-JIHI-7-TAX     TO   YOSO (NG IDX3 IDX 31)
           ADD    SYU-JIHI-7-TAX     TO   YOSO (NG IDX3 10 31)
      *
      *    自費8
           ADD    SYU-JIHI-8         TO   YOSO (NG IDX1 IDX 32)
           ADD    SYU-JIHI-8         TO   YOSO (NG IDX1 10 32)
           ADD    SYU-JIHI-8-TAX     TO   YOSO (NG IDX1 IDX 32)
           ADD    SYU-JIHI-8-TAX     TO   YOSO (NG IDX1 10 32)
           ADD    SYU-JIHI-8         TO   YOSO (NG IDX3 IDX 32)
           ADD    SYU-JIHI-8         TO   YOSO (NG IDX3 10 32)
           ADD    SYU-JIHI-8-TAX     TO   YOSO (NG IDX3 IDX 32)
           ADD    SYU-JIHI-8-TAX     TO   YOSO (NG IDX3 10 32)
      *
      *    自費9
           ADD    SYU-JIHI-9         TO   YOSO (NG IDX1 IDX 33)
           ADD    SYU-JIHI-9         TO   YOSO (NG IDX1 10 33)
           ADD    SYU-JIHI-9-TAX     TO   YOSO (NG IDX1 IDX 33)
           ADD    SYU-JIHI-9-TAX     TO   YOSO (NG IDX1 10 33)
           ADD    SYU-JIHI-9         TO   YOSO (NG IDX3 IDX 33)
           ADD    SYU-JIHI-9         TO   YOSO (NG IDX3 10 33)
           ADD    SYU-JIHI-9-TAX     TO   YOSO (NG IDX3 IDX 33)
           ADD    SYU-JIHI-9-TAX     TO   YOSO (NG IDX3 10 33)
      *
      *    自費10
           ADD    SYU-JIHI-10        TO   YOSO (NG IDX1 IDX 34)
           ADD    SYU-JIHI-10        TO   YOSO (NG IDX1 10 34)
           ADD    SYU-JIHI-10-TAX    TO   YOSO (NG IDX1 IDX 34)
           ADD    SYU-JIHI-10-TAX    TO   YOSO (NG IDX1 10 34)
           ADD    SYU-JIHI-10        TO   YOSO (NG IDX3 IDX 34)
           ADD    SYU-JIHI-10        TO   YOSO (NG IDX3 10 34)
           ADD    SYU-JIHI-10-TAX    TO   YOSO (NG IDX3 IDX 34)
           ADD    SYU-JIHI-10-TAX    TO   YOSO (NG IDX3 10 34)
      *
      *    自費消費税
           ADD    SYU-JIHI-TAX       TO   YOSO (NG IDX1 IDX 35)
           ADD    SYU-JIHI-TAX       TO   YOSO (NG IDX1 10 35)
           ADD    SYU-JIHI-TAX       TO   YOSO (NG IDX3 IDX 35)
           ADD    SYU-JIHI-TAX       TO   YOSO (NG IDX3 10 35)
      *
      *    自費合計
           IF   SYU-SRYYMD  <=  "20070331"
             COMPUTE  YOSO (NG IDX1 IDX 36)  =  YOSO (NG IDX1 IDX 25)
                                             +  YOSO (NG IDX1 IDX 26)
                                             +  YOSO (NG IDX1 IDX 27)
                                             +  YOSO (NG IDX1 IDX 28)
                                             +  YOSO (NG IDX1 IDX 29)
                                             +  YOSO (NG IDX1 IDX 30)
                                             +  YOSO (NG IDX1 IDX 31)
                                             +  YOSO (NG IDX1 IDX 32)
                                             +  YOSO (NG IDX1 IDX 33)
                                             +  YOSO (NG IDX1 IDX 34)
                                             +  YOSO (NG IDX1 IDX 35)
             COMPUTE  YOSO (NG IDX1 10  36)  =  YOSO (NG IDX1 10  25)
                                             +  YOSO (NG IDX1 10  26)
                                             +  YOSO (NG IDX1 10  27)
                                             +  YOSO (NG IDX1 10  28)
                                             +  YOSO (NG IDX1 10  29)
                                             +  YOSO (NG IDX1 10  30)
                                             +  YOSO (NG IDX1 10  31)
                                             +  YOSO (NG IDX1 10  32)
                                             +  YOSO (NG IDX1 10  33)
                                             +  YOSO (NG IDX1 10  34)
                                             +  YOSO (NG IDX1 10  35)
             COMPUTE  YOSO (NG IDX3 IDX 36)  =  YOSO (NG IDX3 IDX 25)
                                             +  YOSO (NG IDX3 IDX 26)
                                             +  YOSO (NG IDX3 IDX 27)
                                             +  YOSO (NG IDX3 IDX 28)
                                             +  YOSO (NG IDX3 IDX 29)
                                             +  YOSO (NG IDX3 IDX 30)
                                             +  YOSO (NG IDX3 IDX 31)
                                             +  YOSO (NG IDX3 IDX 32)
                                             +  YOSO (NG IDX3 IDX 33)
                                             +  YOSO (NG IDX3 IDX 34)
                                             +  YOSO (NG IDX3 IDX 35)
             COMPUTE  YOSO (NG IDX3 10  36)  =  YOSO (NG IDX3 10  25)
                                             +  YOSO (NG IDX3 10  26)
                                             +  YOSO (NG IDX3 10  27)
                                             +  YOSO (NG IDX3 10  28)
                                             +  YOSO (NG IDX3 10  29)
                                             +  YOSO (NG IDX3 10  30)
                                             +  YOSO (NG IDX3 10  31)
                                             +  YOSO (NG IDX3 10  32)
                                             +  YOSO (NG IDX3 10  33)
                                             +  YOSO (NG IDX3 10  34)
                                             +  YOSO (NG IDX3 10  35)
           ELSE
             COMPUTE  YOSO (NG IDX1 IDX 36)  =  YOSO (NG IDX1 IDX 25)
                                             +  YOSO (NG IDX1 IDX 26)
                                             +  YOSO (NG IDX1 IDX 27)
                                             +  YOSO (NG IDX1 IDX 28)
                                             +  YOSO (NG IDX1 IDX 29)
                                             +  YOSO (NG IDX1 IDX 30)
                                             +  YOSO (NG IDX1 IDX 31)
                                             +  YOSO (NG IDX1 IDX 32)
                                             +  YOSO (NG IDX1 IDX 33)
                                             +  YOSO (NG IDX1 IDX 34)
             COMPUTE  YOSO (NG IDX1 10  36)  =  YOSO (NG IDX1 10  25)
                                             +  YOSO (NG IDX1 10  26)
                                             +  YOSO (NG IDX1 10  27)
                                             +  YOSO (NG IDX1 10  28)
                                             +  YOSO (NG IDX1 10  29)
                                             +  YOSO (NG IDX1 10  30)
                                             +  YOSO (NG IDX1 10  31)
                                             +  YOSO (NG IDX1 10  32)
                                             +  YOSO (NG IDX1 10  33)
                                             +  YOSO (NG IDX1 10  34)
             COMPUTE  YOSO (NG IDX3 IDX 36)  =  YOSO (NG IDX3 IDX 25)
                                             +  YOSO (NG IDX3 IDX 26)
                                             +  YOSO (NG IDX3 IDX 27)
                                             +  YOSO (NG IDX3 IDX 28)
                                             +  YOSO (NG IDX3 IDX 29)
                                             +  YOSO (NG IDX3 IDX 30)
                                             +  YOSO (NG IDX3 IDX 31)
                                             +  YOSO (NG IDX3 IDX 32)
                                             +  YOSO (NG IDX3 IDX 33)
                                             +  YOSO (NG IDX3 IDX 34)
             COMPUTE  YOSO (NG IDX3 10  36)  =  YOSO (NG IDX3 10  25)
                                             +  YOSO (NG IDX3 10  26)
                                             +  YOSO (NG IDX3 10  27)
                                             +  YOSO (NG IDX3 10  28)
                                             +  YOSO (NG IDX3 10  29)
                                             +  YOSO (NG IDX3 10  30)
                                             +  YOSO (NG IDX3 10  31)
                                             +  YOSO (NG IDX3 10  32)
                                             +  YOSO (NG IDX3 10  33)
                                             +  YOSO (NG IDX3 10  34)
           END-IF
      *
           ADD    SYU-SKYMONEY        TO   YOSO (NG IDX1 IDX 38)
           ADD    SYU-SKYMONEY        TO   YOSO (NG IDX1 10 38)
           ADD    SYU-SKYMONEY        TO   YOSO (NG IDX3 IDX 38)
           ADD    SYU-SKYMONEY        TO   YOSO (NG IDX3 10 38)
      *
           EVALUATE IDX
             WHEN 7
      *    自費の場合
               MOVE    SYU-SKYMONEY      TO   WRK-SYUEKI
             WHEN 8
      *    労災の場合
               COMPUTE  WRK-SYUEKI ROUNDED  =
                               (SYU-TOTAL-HKNTEN  *  WRK-4001-TENTANKA)
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
      *
             WHEN 9
      *    自賠責の場合
               MOVE   SYU-RSIJIBAI-SKYMONEY     TO   WRK-SYUEKI
             WHEN 11
      *    公害の場合
               COMPUTE WRK-JIHI4  =  SYU-TOTAL-TGMONEY
                                  +  SYU-TOTAL-TGMONEY-TAX
                                  +  SYU-JIHI-TOTAL
                                  +  SYU-JIHI-TOTAL-TAX
                                  +  SYU-RMSAGAKU
                                  +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                  +  SYU-SKYMONEY-LIFE-JIHI-KEI
               IF   SYU-SRYYMD  <  "20070401"
                 COMPUTE   WRK-JIHI4    =   WRK-JIHI4
                                        +   SYU-JIHI-TAX
               END-IF
                 COMPUTE   WRK-SYUEKI   =   SYU-RSIJIBAI-SKYMONEY
             WHEN OTHER
      *    自費、労災、自賠責以外の場合
               COMPUTE  WRK-SYUEKI   =  (SYU-TOTAL-HKNTEN * HKN-TENTNK)
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
      *
           END-EVALUATE
      *
           COMPUTE   YOSO (NG IDX1 IDX 39)  =  YOSO (NG IDX1 IDX 39)
                                            +  WRK-SYUEKI
           COMPUTE   YOSO (NG IDX1 10 39)   =  YOSO (NG IDX1 10 39)
                                            +  WRK-SYUEKI
           COMPUTE   YOSO (NG IDX3 IDX 39)  =  YOSO (NG IDX3 IDX 39)
                                            +  WRK-SYUEKI
           COMPUTE   YOSO (NG IDX3 10 39)   =  YOSO (NG IDX3 10 39)
                                            +  WRK-SYUEKI
      *
           COMPUTE   YOSO (NG IDX1 IDX 37)  =  YOSO (NG IDX1 IDX 39)
                                            -  YOSO (NG IDX1 IDX 38)
           COMPUTE   YOSO (NG IDX1 10 37)   =  YOSO (NG IDX1 10 39)
                                            -  YOSO (NG IDX1 10 38)
           COMPUTE   YOSO (NG IDX3 IDX 37)  =  YOSO (NG IDX3 IDX 39)
                                            -  YOSO (NG IDX3 IDX 38)
           COMPUTE   YOSO (NG IDX3 10 37)   =  YOSO (NG IDX3 10 39)
                                            -  YOSO (NG IDX3 10 38)
      *
           IF   NG  =  2
      *    外来の場合は初診、再診件数を取得する。
             PERFORM  900-JYURRK-SELECT-SEC
             PERFORM  UNTIL  FLG-JYURRK  =  1
               IF   JYURRK-DENPNUM  NOT =  WRK-DENPNUM
                 PERFORM   320-JYURRK-CNT-SUB-SEC
               END-IF
               PERFORM  900-JYURRK-FETCH-SEC
             END-PERFORM
             PERFORM  900-JYURRK-CLOSE-SEC
           END-IF
      *
           .
       320-SYUNOU-CNT-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報集計サブ処理
      *****************************************************************
       320-JYURRK-CNT-SUB-SEC      SECTION.
      *
           MOVE   JYURRK-SRYYMD(7:2)     TO   IDX7G
           PERFORM  VARYING  IDX6  FROM 1  BY  1
                    UNTIL    IDX6  >  SRYKAMAX
             IF  (JYURRK-NYUGAIKBN  =  "1"
                 AND  SRYKA(1 IDX6)  =  JYURRK-SRYKA)
                 OR  (JYURRK-NYUGAIKBN  =  "2"
                 AND  SRYKA(2 IDX6)  =  JYURRK-SRYKA)
      *
               MOVE   JYURRK-NYUGAIKBN    TO   WRK-NYUGAIKBN
               MOVE   JYURRK-SRYKA        TO   WRK-SRYKA
               PERFORM   320-SRYKA-GROUPING-SEC
               IF  WRK-PARA-PRTYM  <  "200804"
                 PERFORM  320-HOKEN-GROUPING-SEC
               ELSE
                 PERFORM  330-HOKEN-GROUPING-SEC
               END-IF
               MOVE   JYURRK-HOSPNUM      TO   ACCT-HOSPNUM
               MOVE   JYURRK-NYUGAIKBN    TO   ACCT-NYUGAIKBN
               MOVE   JYURRK-PTID         TO   ACCT-PTID
               MOVE   JYURRK-SRYKA        TO   ACCT-SRYKA
               MOVE   JYURRK-SRYYMD(1:6)  TO   ACCT-SRYYM
               MOVE   SRYACCT-REC         TO   MCPDATA-REC
               MOVE   "DBSELECT"          TO   MCP-FUNC
               MOVE   "tbl_sryacct"       TO   MCP-TABLE
               MOVE   "key2"              TO   MCP-PATHNAME
               CALL   "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF   MCP-RC   =  ZERO
                 MOVE   "DBFETCH"         TO   MCP-FUNC
                 MOVE   "tbl_sryacct"     TO   MCP-TABLE
                 MOVE   "key2"            TO   MCP-PATHNAME
                 CALL   "ORCDBMAIN"       USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
                 IF   MCP-RC   =  ZERO
                   MOVE MCPDATA-REC       TO   SRYACCT-REC
                   MOVE ZERO              TO   FLG-SRYACCT
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
                       MOVE   "key2"             TO   MCP-PATHNAME
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
                 ELSE
                   MOVE    1              TO   FLG-SRYACCT
                   INITIALIZE             SRYACCT-REC
                 END-IF
               ELSE
                 MOVE    1                TO   FLG-SRYACCT
                 INITIALIZE               SRYACCT-REC
               END-IF
      *
      *    診療会計情報CLOSE
               MOVE "DBCLOSECURSOR"     TO   MCP-FUNC
               MOVE "tbl_sryacct"       TO   MCP-TABLE
               MOVE   "key2"            TO   MCP-PATHNAME
               CALL "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           COMPUTE  JYURRK-EDANUM  =  JYURRK-EDANUM  +  1
           MOVE    JYURRK-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key22"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO              TO   FLG-JYURRK2
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_jyurrk"      TO   MCP-TABLE
             MOVE    "key22"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
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
           MOVE    "key22"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
               IF   FLG-SRYACCT  =  ZERO
                   AND   ACCT-SRYKBN  =  "11"
                   AND   (JYURRK-DOUJI-RENNUM  =  1
                   OR   JYURRK-DOUJI-RENNUM  =  ZERO)
      *    初診の件数としてカウントする。
                 IF   JYURRK-NYUGAIKBN = 1
                   ADD     1            TO   YOSO (1 IDX6 IDX 1)
                   ADD     1            TO   YOSO (1 IDX6 10 1)
                   ADD     1            TO   YOSO (1 IDX3 IDX 1)
                   ADD     1            TO   YOSO (1 IDX3 10 1)
                 ELSE
                   ADD     1            TO   YOSO (2 IDX6 IDX 1)
                   ADD     1            TO   YOSO (2 IDX6 10 1)
                   ADD     1            TO   YOSO (2 IDX3 IDX 1)
                   ADD     1            TO   YOSO (2 IDX3 10 1)
                 END-IF
                 MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
               ELSE
      *
                 PERFORM  VARYING  IDX10 FROM  1  BY  1
                          UNTIL  IDX10  >  15
                   MOVE    JYURRK-ZAINUM(IDX10)  TO   ACCT-ZAINUM
                 MOVE    JYURRK-HOSPNUM      TO   ACCT-HOSPNUM
                 MOVE    JYURRK-NYUGAIKBN    TO   ACCT-NYUGAIKBN
                 MOVE    JYURRK-PTID         TO   ACCT-PTID
                 MOVE    JYURRK-SRYKA        TO   ACCT-SRYKA
                 MOVE    JYURRK-SRYYMD(1:6)  TO   ACCT-SRYYM
      *    診療会計情報SELECT(KEY=5)
                 MOVE    SRYACCT-REC         TO   MCPDATA-REC
                 MOVE    "DBSELECT"          TO   MCP-FUNC
                 MOVE    "tbl_sryacct"       TO   MCP-TABLE
                 MOVE    "key5"              TO   MCP-PATHNAME
                 CALL    "ORCDBMAIN"         USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
                 IF   MCP-RC   =  ZERO
                   MOVE    "DBFETCH"         TO   MCP-FUNC
                   MOVE    "tbl_sryacct"     TO   MCP-TABLE
                   MOVE    "key5"            TO   MCP-PATHNAME
                   CALL    "ORCDBMAIN"       USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
                   IF   MCP-RC   =  ZERO
                     MOVE    MCPDATA-REC     TO   SRYACCT-REC
                     MOVE    ZERO            TO   FLG-SRYACCT
                   ELSE
                     MOVE    1               TO   FLG-SRYACCT
                     INITIALIZE              SRYACCT-REC
                   END-IF
                 ELSE
                   MOVE    1                 TO   FLG-SRYACCT
                   INITIALIZE                SRYACCT-REC
                 END-IF
      *    診療会計情報CLOSE
                 MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
                 MOVE    "tbl_sryacct"       TO   MCP-TABLE
                 MOVE    "key5"              TO   MCP-PATHNAME
                 CALL    "ORCDBMAIN"         USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *
                  IF   IDX10  <  15
                    COMPUTE   IDX11  =  IDX10  +  1
                    IF   JYURRK-ZAINUM(IDX11)  =  ZERO
                      MOVE    16         TO   IDX10
                    END-IF
                  END-IF
                  IF   ACCT-SRYKBN   =  "12"
                    MOVE    16           TO   IDX10
                  END-IF
                END-PERFORM
      *
                 IF   FLG-SRYACCT  =  ZERO
                     AND   ACCT-DAY(1 IDX7)  >  ZERO
                     AND   ACCT-SRYKBN   =  "12"
      *
                   IF   JYURRK-NYUGAIKBN  =  1
                     ADD    1            TO   YOSO(1 IDX6 IDX 2)
                     ADD    1            TO   YOSO(1 IDX6 10 2)
                     ADD    1            TO   YOSO(1 IDX3 IDX 2)
                     ADD    1            TO   YOSO(1 IDX3 10 2)
                   ELSE
                     ADD    1            TO   YOSO(2 IDX6 IDX 2)
                     ADD    1            TO   YOSO(2 IDX6 10 2)
                     ADD    1            TO   YOSO(2 IDX3 IDX 2)
                     ADD    1            TO   YOSO(2 IDX3 10 2)
                   END-IF
                   MOVE    JYURRK-DENPNUM   TO   WRK-DENPNUM
      *
                 ELSE
                   IF   FLG-JYURRK2  =  1
      *    その他の件数としてカウントする。
                     IF   JYURRK-NYUGAIKBN  =  1
                       ADD    1           TO   YOSO(1 IDX6 IDX 3)
                       ADD    1           TO   YOSO(1 IDX6 10 3)
                       ADD    1           TO   YOSO(1 IDX3 IDX 3)
                       ADD    1           TO   YOSO(1 IDX3 10 3)
                     ELSE
                       ADD    1           TO   YOSO(2 IDX6 IDX 3)
                       ADD    1           TO   YOSO(2 IDX6 10 3)
                       ADD    1           TO   YOSO(2 IDX3 IDX 3)
                       ADD    1           TO   YOSO(2 IDX3 10 3)
                     END-IF
                   END-IF
                 END-IF
      *
               END-IF
               MOVE     SRYKAMAX          TO   IDX6
             END-IF
           END-PERFORM
           .
       320-JYURRK-CNT-SUB-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報集計処理
      *****************************************************************
       320-PTNYUINRRK-CNT-SEC      SECTION.
      *
           PERFORM  UNTIL      FLG-PTNYUINRRK  = 1
             INITIALIZE                 PTINF-REC
             MOVE   PTNYUINRRK-HOSPNUM  TO   PTINF-HOSPNUM
             MOVE   PTNYUINRRK-PTID     TO   PTINF-PTID
             PERFORM   900-PTINF-READ-SEC
      *
      *    集計対象年月のデータであるかをチェックする。
             IF (( PTNYUINRRK-NYUINYMD(1:6)  =  WRK-PARA-PRTYM
                 AND  PTNYUINRRK-RRKEDANUM   =  1)
                 OR  ( PTNYUINRRK-TAIINYMD(1:6) =   WRK-PARA-PRTYM
                 AND  PTNYUINRRK-RRKEDANUM   =  1))
                 AND  FLG-PTINF   =   ZERO
                 AND  PTINF-TSTPTNUMKBN   NOT =  "1"
               MOVE    "1"                  TO   WRK-NYUGAIKBN
               MOVE    PTNYUINRRK-NYUINKA   TO   WRK-SRYKA
               PERFORM  320-SRYKA-GROUPING-SEC
               MOVE    PTNYUINRRK-HOSPNUM   TO   COMB-HOSPNUM
               MOVE    PTNYUINRRK-PTID      TO   COMB-PTID
               MOVE    PTNYUINRRK-HKNCOMBINUM   TO   COMB-HKNCOMBINUM
               PERFORM   900-HKNCOMBI-READ-SEC
               IF  WRK-PARA-PRTYM  <  "200804"
                 PERFORM  321-HOKEN-GROUPING-SEC
               ELSE
                 PERFORM  331-HOKEN-GROUPING-SEC
               END-IF
               IF  PTNYUINRRK-NYUINYMD(1:6)  =   WRK-PARA-PRTYM
                 ADD     1        TO   YOSO (1 IDX1 IDX 1)
                 ADD     1        TO   YOSO (1 IDX1 10 1)
                 ADD     1        TO   YOSO (1 IDX3 IDX 1)
                 ADD     1        TO   YOSO (1 IDX3 10 1)
               END-IF
               IF  PTNYUINRRK-TAIINYMD(1:6) =   WRK-PARA-PRTYM
                 ADD     1        TO   YOSO (1 IDX1 IDX 2)
                 ADD     1        TO   YOSO (1 IDX1 10 2)
                 ADD     1        TO   YOSO (1 IDX3 IDX 2)
                 ADD     1        TO   YOSO (1 IDX3 10 2)
               END-IF
             END-IF
             PERFORM   910-DBFETCH2-SEC
           END-PERFORM
      *
           .
       320-PTNYUINRRK-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科グループ判別処理
      *****************************************************************
       320-SRYKA-GROUPING-SEC      SECTION.
      *
           MOVE    1            TO   IDX1
           MOVE    SRYKAMAX     TO   IDX2
           PERFORM  UNTIL  IDX1   >   IDX2
             IF  (WRK-NYUGAIKBN   =  "1"
                 AND  SRYKA(1 IDX1)  =  WRK-SRYKA)
                 OR   (WRK-NYUGAIKBN   =  "2"
                 AND  SRYKA(2 IDX1)  =  WRK-SRYKA)
               MOVE    ZERO     TO   IDX2
             ELSE
               ADD     1        TO   IDX1
             END-IF
           END-PERFORM
      *
           .
       320-SRYKA-GROUPING-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険グループ判別処理
      *****************************************************************
       320-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO   IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6               TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    9               TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    8               TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    7               TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    11              TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    5             TO   IDX
               ELSE
                 MOVE    4             TO   IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    3             TO   IDX
               ELSE
                 PERFORM 3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1         TO   IDX
                   WHEN    "2"
                     MOVE    2         TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      *    保険別区分例外条件(自費条件拡張)
           IF   SYU-SYUHKNNUM(1:2)  =  "98"
             MOVE      7               TO   IDX
           END-IF
      *
           .
       320-HOKEN-GROUPING-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険グループ判別処理2
      *****************************************************************
       321-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE     ZERO               TO   IDX
           EVALUATE   COMB-HKNNUM
             WHEN    SPACE
               MOVE    6               TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    9               TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    8               TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    7               TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    11      TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF   COMB-KOH1HKNNUM   =   "027"
                 MOVE    5             TO   IDX
               ELSE
                 MOVE    4             TO   IDX
               END-IF
             WHEN    OTHER
               IF   COMB-KOH1HKNNUM   =   "027"
                 MOVE    3             TO   IDX
               ELSE
                 PERFORM 3202-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1         TO   IDX
                   WHEN    "2"
                     MOVE    2         TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
      *    保険別区分例外条件(自費条件拡張)
           IF   COMB-HKNNUM(1:2)  =  "98"
             MOVE     7        TO    IDX
           END-IF
      *
           .
       321-HOKEN-GROUPING-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険グループ判別処理(H20年4月以降)
      *****************************************************************
       330-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO   IDX
           EVALUATE    SYU-SYUHKNNUM
             WHEN    SPACE
               MOVE    6               TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
               MOVE    9               TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    8               TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    7               TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    11              TO   IDX
             WHEN    "039"
             WHEN    "040"
               MOVE    5               TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF   SYU-KOH1HKNNUM   =   "027"
                   MOVE    5           TO   IDX
               ELSE
                   MOVE    4           TO   IDX
               END-IF
             WHEN    OTHER
               IF   SYU-KOH1HKNNUM   =   "027"
                 MOVE    5             TO   IDX
               ELSE
                 PERFORM   3201-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1         TO   IDX
                   WHEN    "2"
                     MOVE    2         TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           IF   SYU-SYUHKNNUM(1:2) = "98"
             MOVE     7        TO   IDX
           END-IF
      *
           .
       320-HOKEN-GROUPING-EXT.
           EXIT.
      *****************************************************************
      *    保険グループ判別処理2(H20年4月以降)
      *****************************************************************
       331-HOKEN-GROUPING-SEC      SECTION.
      *
      *    保険別金額（社保本人〜自賠責）
           MOVE    ZERO                TO  IDX
           EVALUATE    COMB-HKNNUM
             WHEN    SPACE
               MOVE    6                   TO   IDX
             WHEN    WRK-CONS-JIBAI-HKNNUM
              MOVE     9                   TO   IDX
             WHEN    WRK-CONS-ROUSAI-HKNNUM
               MOVE    8                   TO   IDX
             WHEN    WRK-CONS-JIHI-HKNNUM
             WHEN    "900"  THRU  "919"
               MOVE    7                   TO   IDX
             WHEN    WRK-CONS-KOGAI-HKNNUM
               MOVE    11                  TO   IDX
             WHEN    "039"
             WHEN    "040"
               MOVE    5                   TO   IDX
             WHEN    "060"
             WHEN    "067"
             WHEN    "068"
             WHEN    "069"
               IF   COMB-KOH1HKNNUM   =   "027"
                 MOVE    5                 TO   IDX
               ELSE
                 MOVE    4                 TO   IDX
               END-IF
             WHEN    OTHER
               IF   COMB-KOH1HKNNUM   =   "027"
                 MOVE    5                 TO   IDX
               ELSE
                 PERFORM 3202-PTHKNINF-HENSYU-SEC
                 EVALUATE    PTHKN-HONKZKKBN
                   WHEN    "1"
                     MOVE    1             TO   IDX
                   WHEN    "2"
                     MOVE    2             TO   IDX
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           IF   COMB-HKNNUM(1:2) = "98"
             MOVE    7    TO      IDX
           END-IF
      *
           .
       321-HOKEN-GROUPING-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *    保険組合せ読み出し
           INITIALIZE                 HKNCOMBI-REC
           MOVE    SYU-HOSPNUM        TO   COMB-HOSPNUM
           MOVE    SYU-PTID           TO   COMB-PTID
           MOVE    SYU-HKNCOMBINUM    TO   COMB-HKNCOMBINUM
           PERFORM   900-HKNCOMBI-READ-SEC
           IF   FLG-HKNCOMBI   =   ZERO
      *    患者保険情報読み込み
             INITIALIZE                    PTHKNINF-REC
             MOVE    SYU-HOSPNUM      TO   PTHKN-HOSPNUM
             MOVE    SYU-PTID         TO   PTHKN-PTID
             MOVE    COMB-HKNID       TO   PTHKN-HKNID
             PERFORM   900-PTHKNINF-READ-SEC
           END-IF
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3202-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *    患者保険情報読み込み
           INITIALIZE                    PTHKNINF-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO   PTHKN-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO   PTHKN-PTID
           MOVE    COMB-HKNID          TO   PTHKN-HKNID
           PERFORM   900-PTHKNINF-READ-SEC
           .
       3202-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
      *
       330-TERM-HEN-SEC         SECTION.
      *
           IF   WRK-SYS-BEDSU     >  ZERO
               OR  WRK-SYS-BEDSUIPN  >  ZERO
      *    病床数が１つ以上ある場合は入院のデータも印字する。
             MOVE    1      TO    NG
           ELSE
      *    病床数がゼロの場合は外来のデータのみ印字する。
             MOVE    2      TO    NG
           END-IF
      *
           PERFORM    UNTIL   NG   >   2
             PERFORM    VARYING  IDX1    FROM  1   BY  1
                        UNTIL   IDX1  >  SRYKAMAX
      *    帳票パラメタ診療科,入外区分の追加
               IF   SRYKA(NG IDX1)  =  WRK-PARA-SRYKA
                   OR   WRK-PARA-SRYKA  =  SPACE
                   OR   FLG-SRYKAGK   =  1
      *    パラメタ診療科に”00”が入力された場合は合計項のみ印刷する
                 IF   FLG-SRYKAGK   =  1
                   MOVE    SRYKAMAX        TO   IDX1
                 END-IF
      *    パラメタ入外区分の追加。
                 IF   WRK-SYS-NYUGAIKBN    =   NG
                     OR   WRK-PARA-NYUGAIKBN   =   SPACE
                   INITIALIZE       HC90
      *
                   MOVE    "対象年月"        TO   HC90-MIDASI
                   MOVE    "対象年月"        TO   HC901-MIDASI
                   INSPECT    WRK-PARA-DENPPRTYMDWH
                              REPLACING  CHARACTERS  BY " "
                              AFTER  "月"
                   MOVE    WRK-PARA-DENPPRTYMDWH  TO   HC90-SRYYMD
                   MOVE    WRK-PARA-DENPPRTYMDWH  TO   HC901-SRYYMD
                   MOVE    WRK-SYSYMDWH           TO   HC90-SYSYMD
                   MOVE    WRK-SYSYMDWH           TO   HC901-SYSYMD
      *
                   MOVE    NYUGAINAME(NG)         TO   HC90-NYUGAIKBN
                   MOVE   NYUGAINAME(NG)          TO   HC901-NYUGAIKBN
      *
                   IF   NYUGAIKBN(NG) = "1"
                     MOVE   SRYKANAME(1 IDX1)     TO   HC90-TSRYKA
                     MOVE   SRYKANAME(1 IDX1)     TO   HC901-TSRYKA
                   ELSE
                     MOVE   SRYKANAME(2 IDX1)     TO   HC90-TSRYKA
                     MOVE   SRYKANAME(2 IDX1)     TO   HC901-TSRYKA
                   END-IF
      *
                   IF   NYUGAIKBN(NG)  =  "1"
      *    注意書メッセージの設定
                     MOVE  SPACE            TO   HC90-CMSG1
      *    入院の件数タイトル設定
                     MOVE  "入院件数"       TO   HC90-TKENSU1
                     MOVE  "退院件数"       TO   HC90-TKENSU2
      *    入院の負担金タイトル設定
                     MOVE  "食事療養費・生活療養費"   TO   HC90-TFUTAN1
                     MOVE  "食事負担額・生活負担額"   TO   HC90-TFUTAN2
      *
                     MOVE  "室料差額"       TO   HC90-TFUTAN3
      *    入院の自費タイトル設定
                     PERFORM  VARYING  IDX2   FROM 1  BY  1
                              UNTIL    IDX2   >    10
                       MOVE   WRK-TJIHIN(IDX2)   TO   HC90-TJIHI(IDX2)
                     END-PERFORM
                   ELSE
      *    注意書メッセージの設定
                     MOVE  "※(注１) 同日再診も１件毎にカウントする。"
                                                 TO   HC90-CMSG1
      *    外来の件数タイトル設定
                     MOVE  "初診件数"           TO   HC90-TKENSU1
                     MOVE  "再診件数 (注１)"    TO   HC90-TKENSU2
                     MOVE  "その他件数 (注１)"  TO   HC90-TKENSU3
      *    外来の負担金タイトル設定
      *    外来の自費タイトル設定
                     PERFORM  VARYING  IDX2   FROM 1  BY  1
                              UNTIL    IDX2   >   10
                       MOVE  WRK-TJIHI(IDX2)   TO   HC90-TJIHI(IDX2)
                     END-PERFORM
                   END-IF
                   MOVE    "入院料(点数)"   TO   HC90-THKN1
      *
                   ADD     1            TO   CNT-PAGE
                   MOVE    CNT-PAGE     TO   WRK-PAGE
                   MOVE    WRK-PAGE     TO   HC90-PAGE
                   ADD     1            TO   CNT-PAGE
                   MOVE    CNT-PAGE     TO   WRK-PAGE
                   MOVE    WRK-PAGE     TO   HC901-PAGE
      *
                   PERFORM  VARYING  IDX2   FROM  1 BY 1
                            UNTIL    IDX2  >  11
                     PERFORM   VARYING  IDX3   FROM  1 BY 1
                               UNTIL    IDX3   >  40
                       IF  YOSO(NG IDX1 IDX2 IDX3)  >  0
                           AND  IDX3  =  19
                         MOVE     "療養担当手当"    TO   HC90-TRYO
                       END-IF
      *    予備(空き)行以外は数値を転送する。
                       EVALUATE  NYUGAIKBN(NG)
                         WHEN "1"
      *    入院集計情報のデータ転送
                           IF   IDX3   NOT =   3
                               AND  IDX3   NOT =   4
      *    自費行は有効である場合に数値を転送する。
                             IF  IDX3 = 25
                                 OR   IDX3  =  26
                                 OR   IDX3  =  27
                                 OR   IDX3  =  28
                                 OR   IDX3  =  29
                                 OR   IDX3  =  30
                                 OR   IDX3  =  31
                                 OR   IDX3  =  32
                                 OR   IDX3  =  33
                                 OR   IDX3  =  34
                               COMPUTE   IDX8   =   IDX3   -   24
                               MOVE YOSO(NG IDX1 IDX2 IDX3)  TO   WRK-S9
                               MOVE WRK-S9   TO   HC90-YOSO(IDX2 IDX3)
                             ELSE
      *    自費行以外の場合は数値を転送する。
                               MOVE YOSO(NG IDX1 IDX2 IDX3)  TO  WRK-S9
                               IF  IDX3  >  34
                                 IF   IDX3  =  40
                                   COMPUTE   IDX9   =   IDX3   -   5
                                   MOVE  WRK-S9 TO HC90-YOSO(IDX2 IDX9)
                                 ELSE
                                   COMPUTE IDX9 = IDX3 - 34
                                   MOVE  WRK-S9 TO HC901-YOSO(IDX2 IDX9)
                                 END-IF
                               ELSE
                                 MOVE WRK-S9  TO   HC90-YOSO(IDX2 IDX3)
                               END-IF
                             END-IF
                           END-IF
                         WHEN "2"
      *    外来集計情報のデータ転送
                           IF  IDX3   NOT = 4
                               AND  IDX3   NOT =   22
                               AND  IDX3   NOT =   23
                               AND  IDX3   NOT =   24
      *    自費行は有効である場合に数値を転送する。
                             IF  IDX3  =  25
                                 OR  IDX3  =  26
                                 OR  IDX3  =  27
                                 OR  IDX3  =  28
                                 OR  IDX3  =  29
                                 OR  IDX3  =  30
                                 OR  IDX3  =  31
                                 OR  IDX3  =  32
                                 OR  IDX3  =  33
                                 OR  IDX3  =  34
                               COMPUTE  IDX8  =  IDX3  -  24
                               MOVE   YOSO(NG IDX1 IDX2 IDX3) TO  WRK-S9
                               MOVE   WRK-S9   TO   HC90-YOSO(IDX2 IDX3)
                             ELSE
      *    自費行以外の場合は数値を転送する。
                               MOVE   YOSO(NG IDX1 IDX2 IDX3) TO  WRK-S9
                               IF  IDX3  >  34
                                 IF  IDX3  =  40
                                   COMPUTE   IDX9   =   IDX3   -   5
                                   MOVE  WRK-S9 TO  HC90-YOSO(IDX2 IDX9)
                                 ELSE
                                   COMPUTE   IDX9   =   IDX3   -   34
                                   MOVE  WRK-S9 TO HC901-YOSO(IDX2 IDX9)
                                 END-IF
                               ELSE
                                 MOVE  WRK-S9  TO  HC90-YOSO(IDX2 IDX3)
                               END-IF
                             END-IF
                           END-IF
                       END-EVALUATE
                     END-PERFORM
                   END-PERFORM
      *
                   INITIALIZE                      ORCSPRTAREA
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
                   IF    WRK-PARA-PRTYM  <  "200804"
                     MOVE    "A00000M500O.red"     TO   SPRT-PRTID
                   ELSE
                     MOVE    "A00000M500N.red"     TO   SPRT-PRTID
                   END-IF
                   MOVE    "保険別診療点数 月計表"   TO   SPRT-TITLE
                   MOVE    HC90                  TO   SPRT-PRTDATA
                   MOVE    LNK-PRTKANRI-TERMID   TO   SPRT-TERMID
                   MOVE    LNK-PRTKANRI-OPID     TO   SPRT-OPID
                   MOVE    LNK-PRTKANRI-PRTNM    TO   SPRT-PRTNM
                   CALL    "ORCSPRT"             USING
                                                 ORCSPRTAREA
                                                 SPA-AREA
                   IF   SPRT-RETURN   =   ZERO
                     IF  WRK-PARA-PRTYM  <  "200804"
                       MOVE   "A00000M5001O.red"   TO   SPRT-PRTID
                     ELSE
                       MOVE   "A00000M5001N.red"   TO   SPRT-PRTID
                     END-IF
                     MOVE    "保険別診療点数 月計表"
                                                   TO   SPRT-TITLE
                     MOVE    HC901                 TO   SPRT-PRTDATA
                     MOVE    LNK-PRTKANRI-TERMID   TO   SPRT-TERMID
                     MOVE    LNK-PRTKANRI-OPID     TO   SPRT-OPID
                     MOVE    LNK-PRTKANRI-PRTNM    TO   SPRT-PRTNM
                     CALL    "ORCSPRT"             USING
                                                   ORCSPRTAREA
                                                    SPA-AREA
                     IF   SPRT-RETURN   =   ZERO
                       CONTINUE
                     ELSE
                       MOVE    "印刷ＤＢに更新できませんでした"
                                           TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                     END-IF
                     CONTINUE
                   ELSE
                     MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      *    診療科が単科の場合は合計頁のみ印刷する.
                   IF   SRYKAMAX   =   2
                     MOVE    2       TO      IDX1
                   END-IF
      *
      *    診療科、入外区分判別終わり
                 END-IF
               END-IF
             END-PERFORM
      *
      *      入外用インデックスカウントアップ
             ADD    1      TO    NG
      	   END-PERFORM
      *
           .
       330-TERM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT           RECEERR-FILE
      *
             MOVE    WRK-RECEERR      TO   RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"            TO   SJOBKANRI-MODE
             INITIALIZE                    JOBKANRI-REC
             MOVE    SPA-HOSPNUM      TO   JOB-HOSPNUM
             MOVE    WRK-PARA-JOBID   TO   JOB-JOBID
             MOVE    WRK-PARA-SHELLID TO   JOB-SHELLID
             MOVE    WRK-RECEERR      TO   JOB-YOBI
             MOVE    "9999"           TO   JOB-ERRCD
             CALL    "ORCSJOB"        USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
           END-IF
      *
           MOVE      1                TO   FLG-END
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
           MOVE    "STE"           TO   SJOBKANRI-MODE
           INITIALIZE                   JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO   JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO   JOB-SHELLID
           MOVE    CNT-PAGE        TO   JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM   900-DBDISCONNECT-SEC
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
           MOVE    "21"                TO   LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO   LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO   WRK-HENYMDG
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
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD            TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO   WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  SYUNOU-REC
      *
           MOVE    WRK-PARA-HOSPNUM     TO   SYU-HOSPNUM
           EVALUATE WRK-PARA-NYUGAIKBN
             WHEN  "1"
               MOVE    "1"              TO   SYU-NYUGAIKBN
             WHEN  "2"
               MOVE    "2"              TO   SYU-NYUGAIKBN
             WHEN  SPACE
               MOVE    "%"              TO   SYU-NYUGAIKBN
           END-EVALUATE
           MOVE    WRK-PARA-PRTYM       TO   SYU-SRYYMD
           MOVE    "01"                 TO   SYU-SRYYMD(7:2)
           MOVE    WRK-PARA-PRTLYMD     TO   SYU-UPSRYYMD
      *
           MOVE    SYUNOU-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM   910-DBFETCH-SEC
           ELSE
             MOVE      1           TO   FLG-SYUNOU
             PERFORM   910-DBCLOSE-SEC
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "st_key1"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO               TO   FLG-SYUNOU
             MOVE    MCPDATA-REC        TO   SYUNOU-REC
           ELSE
             MOVE    1                  TO   FLG-SYUNOU
             PERFORM   910-DBCLOSE-SEC
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
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "st_key1"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報検索
      *****************************************************************
       910-DBSELECT2-SEC                 SECTION.
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO   PTNYUINRRK-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD    TO   PTNYUINRRK-NYUINYMD
           MOVE    WRK-PARA-PRTYM      TO   PTNYUINRRK-TAIINYMD(1:6)
           MOVE    "00"                TO   PTNYUINRRK-TAIINYMD(7:2)
      *
           MOVE    PTNYUINRRK-REC      TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"    TO   MCP-TABLE
           MOVE    "key14"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    MCP-RC   =   ZERO
             PERFORM   910-DBFETCH2-SEC
           ELSE
             MOVE     1           TO    FLG-PTNYUINRRK
             MOVE     1           TO    FLG-END
             PERFORM   910-DBCLOSE2-SEC
           END-IF
           .
       910-DBSELECT2-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴情報検索
      *****************************************************************
       910-DBFETCH2-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO   MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO   MCP-TABLE
           MOVE    "key14"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO               TO   FLG-PTNYUINRRK
             MOVE    MCPDATA-REC        TO   PTNYUINRRK-REC
           ELSE
             MOVE    1                  TO   FLG-PTNYUINRRK
             MOVE    1                  TO   FLG-END
             PERFORM   910-DBCLOSE2-SEC
           END-IF
           .
       910-DBFETCH2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE2-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"   TO   MCP-TABLE
           MOVE    "key14"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           .
       910-DBCLOSE2-EXT.
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
             IF   MCP-RC   =   ZERO
               MOVE      ZERO          TO   FLG-SYSKANRI
             ELSE
               MOVE      1             TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE      1               TO   FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
      *    管理マスタ読み込み(自費情報)
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
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYS-1005-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             PERFORM   820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE      1                TO    FLG-SYSKANRI3
             PERFORM   820-SYSKANRI-DBCLOSE-SEC
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             MOVE    ZERO               TO   FLG-SYSKANRI3
             MOVE    MCPDATA-REC        TO   SYS-1005-REC
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI3
             PERFORM   820-SYSKANRI-DBCLOSE-SEC
           END-IF
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC            TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_ptinf"          TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_ptinf"        TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   PTINF-REC
               MOVE    ZERO             TO   FLG-PTINF
             ELSE
               MOVE    1                TO   FLG-PTINF
               INITIALIZE               PTINF-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-PTINF
             INITIALIZE                 PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_ptinf"          TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
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
           MOVE    "tbl_pthkninf"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
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
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    受診履歴SELECT
      ******************************************************************
       900-JYURRK-SELECT-SEC                 SECTION.

           INITIALIZE                  JYURRK-REC
           MOVE    SYU-HOSPNUM         TO   JYURRK-HOSPNUM
           MOVE    SYU-PTID            TO   JYURRK-PTID
           MOVE    SYU-NYUGAIKBN       TO   JYURRK-NYUGAIKBN
           MOVE    SYU-DENPNUM         TO   JYURRK-DENPNUM
           MOVE    JYURRK-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_jyurrk"        TO   MCP-TABLE
           MOVE    "key6 "             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  900-JYURRK-FETCH-SEC
           ELSE
             MOVE    1                 TO   FLG-JYURRK
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
           MOVE    "key6"              TO   MCP-PATHNAME
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
           MOVE    "key6"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_hkncombi"    TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC     TO   HKNCOMBI-REC
               MOVE    ZERO            TO   FLG-HKNCOMBI
             ELSE
               MOVE    1               TO   FLG-HKNCOMBI
               INITIALIZE              HKNCOMBI-REC
             END-IF
           ELSE
             MOVE    1                 TO   FLG-HKNCOMBI
             INITIALIZE                HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
           MOVE    "tbl_hkncombi"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報検索
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SYU-HOSPNUM         TO   HKN-HOSPNUM
           MOVE    SYU-SYUHKNNUM       TO   HKN-HKNNUM
           MOVE    WRK-PARA-PRTLYMD    TO   HKN-TEKSTYMD
           MOVE    ALL "9"             TO   HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_hknnum"        TO   MCP-TABLE
           MOVE    "key5"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
             MOVE    "tbl_hknnum"      TO   MCP-TABLE
             MOVE    "key5"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC   =  ZERO
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
      *    収納情報読込処理
      *****************************************************************
       900-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    SYUNOU-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_syunou"       TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   SYUNOU-REC
               MOVE    ZERO             TO   FLG-SYUNOU
             ELSE
               MOVE    1                TO   FLG-SYUNOU
               INITIALIZE               SYUNOU-REC
             END-IF
           ELSE
             MOVE    1                  TO   FLG-SYUNOU
             INITIALIZE                 SYUNOU-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_syunou"         TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_sryacct"         TO   MCP-TABLE
           MOVE    "key3"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_sryacct"       TO   MCP-TABLE
             MOVE    "key3"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF      MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   SRYACCT-REC
               MOVE    ZERO              TO   FLG-SRYACCT
             ELSE
               MOVE    1                 TO   FLG-SRYACCT
               INITIALIZE                SRYACCT-REC
             END-IF
           ELSE
             MOVE    1                   TO   FLG-SRYACCT
             INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_sryacct"         TO   MCP-TABLE
           MOVE    "key3"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-SRYACCT-READ-EXT.
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
