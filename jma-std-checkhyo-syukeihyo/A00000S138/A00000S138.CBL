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
       PROGRAM-ID.             A00000S138.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 保険別総括表
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/12/10    @@@@-有村     新規作成
      *******************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者      日付      内容
      *   01.00.01    @@@@-田中   11/01/06  open-cobol1.0対応
      *   01.00.01    @@@@-田中   11/01/06  コメント履歴削除
      *******************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    国保用ファイル
           SELECT  S138TMP-FILE    ASSIGN    S138TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  S138TMP-KEY
                                   FILE    STATUS  IS  STS-S138TMP.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    国保一時ファイル
       FD  S138TMP-FILE.
           COPY    "A00000S138TMP.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    印刷用データ 名称領域
           COPY    "CPCOMMONPRT.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *
      *    国保一時ファイル 名称領域
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //S138TP//.
      *
           COPY    "A00000S138G1.INC".
           COPY    "A00000S138G2.INC".
           COPY    "A00000S138N1.INC".
           COPY    "A00000S138N2.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                 PIC X(02).
           03  STS-RECE                 PIC X(02).
           03  STS-RECEERR              PIC X(02).
           03  STS-RECEUPD              PIC X(02).
           03  STS-S138TMP              PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI              PIC 9(01).
           03  FLG-SYSKANRI3             PIC 9(01).
           03  FLG-JYURRK                PIC 9(01).
           03  FLG-PASS                  PIC 9(01).
           03  FLG-TOKUTEI               PIC 9(01).
           03  FLG-TYPE                  PIC 9(01).
           03  FLG-PTINF                 PIC 9(01).
           03  FLG-HKNJAINF              PIC 9(01).
           03  FLG-SYUNOU                PIC 9(01).
           03  FLG-TENSU                 PIC 9(01).
           03  FLG-S138TMP               PIC 9(01).
           03  FLG-PRINT                 PIC 9(01).
           03  INP-STS                   PIC 9(04).
           03  FLG-KUMIAI                PIC 9(01).
           03  FLG-HKNNUM                PIC 9(01).
           03  FLG-PTHKNINF              PIC 9(01).
           03  FLG-HEIYOU                PIC 9(01).
           03  FLG-RJNHKN                PIC 9(01).
           03  FLG-END                   PIC 9(01).
           03  FLG-HKNCOMBI              PIC 9(01).
           03  FLG-TYOKI                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                  PIC 9(02).
           03  CNT-PAGE                  PIC 9(06).
           03  CNT-KOHI                  PIC 9(01).
           03  CNT-NINZU                 PIC 9(06).
           03  CNT-JYURRK                PIC 9(06).
           03  CNT-DATA                  PIC 9(06).
           03  CNT-READ                  PIC 9(06).
           03  CNT-KOH                   PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                PIC 9(02).
               05  SYS-MM                PIC 9(02).
               05  SYS-DD                PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                       PIC 9(01).
           03  IDX1                      PIC 9(04).
           03  IDX2                      PIC 9(04).
           03  IDX3                      PIC 9(04).
           03  IDX4                      PIC 9(04).
           03  IDX5                      PIC 9(04).
           03  IDX6                      PIC 9(03).
           03  IDX7                     PIC 9(01).
           03  IDA-G.
               05  IDA                   PIC 9(02).
           03  IDB-G.
               05  IDB                   PIC 9(02).
           03  IDC-G.
               05  IDC                   PIC 9(04).
           03  IDD-G.
               05  IDD                   PIC 9(04).
           03  IDY                       PIC 9(04).
           03  IDZ                       PIC 9(04).
           03  IDM                       PIC 9(04).
           03  IDX-MAX                   PIC 9(04).
           03  IDX-S                     PIC 9(03).
           03  IDX-N                     PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID            PIC 9(07).
           03  WRK-PARA-SHELLID          PIC X(08).
           03  WRK-PARA-HOSPNUM          PIC 9(02).
      *    対象診療年月
           03  WRK-PARA-SRYYM            PIC X(06).
      *    対象年月最終日・初日
           03  WRK-PARA-PRTLYMD          PIC X(08).
           03  WRK-PARA-PRTSYMD          PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY             PIC 9(04).
               05  WRK-SYSMM             PIC 9(02).
               05  WRK-SYSDD             PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY               PIC 9(04).
               05  WRK-SMM               PIC 9(02).
               05  WRK-SDD               PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY             PIC 9(04).
               05  WRK-SRYMM             PIC 9(02).
               05  WRK-SRYDD             PIC 9(02).
           03  WRK-HENYMDG               PIC X(22).
           03  WRK-SRYYM                 PIC X(22).
           03  WRK-SEIYMD                PIC X(22).
           03  WRK-RECEERR               PIC X(200).
           03  WRK-SRYKA                 PIC X(02).
           03  WRK-NYUGAIKBN             PIC X(01).
           03  WRK-PARA-NYUGAIKBN        PIC X(01).
           03  WRK-CITYNAME              PIC X(12).
           03  WRK-PTID                  PIC 9(10).
           03  WRK-HKNNUM                PIC X(03).
           03  WRK-KOHNUM                PIC X(03).
           03  WRK-TENTNK                PIC 9(04).
           03  WRK-JIBAITNK              PIC 99.9.
           03  WRK-HKNJANUM              PIC X(06).
           03  WRK-HKNJANAME             PIC X(40).
           03  WRK-KYURATE               PIC 9(02).
           03  WRK-KASAN-TBL.
               05  WRK-KENSU             PIC 9(04).
               05  WRK-NISSU             PIC 9(04).
               05  WRK-TENSU             PIC 9(09).
               05  WRK-MADOGK            PIC S9(09).
               05  WRK-JIHIGK            PIC 9(09).
               05  WRK-HKNGK             PIC 9(09).
               05  WRK-HKNGK2            PIC 9(09).
               05  WRK-KOHGK             PIC S9(09).
               05  WRK-SYUEKI            PIC 9(09).
               05  WRK-SNISSU            PIC 9(05).
               05  WRK-SSEIKYU           PIC 9(09).
               05  WRK-SFTNGK            PIC 9(09).
               05  WRK-KKENSU            PIC 9(04).
               05  WRK-KNISSU            PIC 9(04).
               05  WRK-KTENSU            PIC 9(09).
           03  WRK-KOHI-GOKEI-TBL.
               05  WRK-GKKENSU           PIC 9(04).
               05  WRK-GKNISSU           PIC 9(04).
               05  WRK-GKTENSU           PIC 9(09).
           03  WRK-MADOSEIKYUGK-G        PIC S9(09).
           03  WRK-JIHIGK-G              PIC 9(09).
           03  WRK-KOHSEIKYUGK          PIC S9(09).
           03  WRK-KOHSEIKYUGK-G         PIC S9(09).
           03  WRK-ZENCOMPFTN            PIC 9(09).
           03  WRK-KOHSKJSEIKYUGK        PIC 9(09).
           03  WRK-PATH                  PIC X(64).
           03  WRK-KOHTMP-KEY.
               05  WRK-KOHTMP-RJNKBN     PIC  X(03).
               05  WRK-KOHTMP-KOHNUM     PIC  X(03).
           03  WRK-CHK-TBL.
               05  WRK-CHK-PTID2         PIC 9(10).
               05  WRK-CHK-SRYKA-TBL OCCURS 20.
                   07  WRK-CHK-PTID      PIC 9(10).
                   07  WRK-CHK-DAY-TBL.
                       09  WRK-CHK-DAY   PIC 9(01) OCCURS 31.
           03  WRK-CNT1                  PIC 9(03).
           03  WRK-CNT2                  PIC 9(03).
           03  WRK-CNT3                  PIC 9(03).
           03  WRK-CNT4                  PIC 9(03).
           03  WRK-KOHNAME-G             PIC X(24).
           03  WRK-KOHNAME-G2            PIC X(24).
           03  WRK-JIHI                 PIC S9(07).
           03  WRK-TGMONEY              PIC S9(07).
           03  WRK-YY1                   PIC 9(04).
           03  WRK-YY2                   PIC 9(04).
           03  WRK-NENREI                PIC 9(03).
           03  WRK-JIHI-TBL        OCCURS 4.
               05  WRK-JIHI-HKNNUM       PIC X(03).
      *
           03  WRK-MAE-INPUT             PIC X(200).
           03  WRK-OUT-INPUT             PIC X(200).
      *
           03  WRK-PAGE                  PIC ZZ9.
           03  WRK-Z3                    PIC ZZ9.
           03  WRK-Z4                    PIC ZZZZ.
           03  WRK-Z5                    PIC Z,ZZZ.
           03  WRK-Z6                    PIC ZZ,ZZ9.
           03  WRK-Z7                    PIC ZZZ,ZZZ.
           03  WRK-Z9                    PIC Z,ZZZ,ZZZ.
           03  WRK-Z92                   PIC ZZZZZZZZZ.
           03  WRK-Z10                   PIC ZZ,ZZZ,ZZ9.
           03  WRK-Z11                   PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13                   PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-Z14                   PIC ZZ,ZZZ,ZZZ,ZZZ.
           03  WRK-Z93                   PIC --------Z.
      *
           03  WRK-Z52                  PIC ZZZZZ.
           03  WRK-SYS-BEDSU             PIC 9(04).
           03  WRK-SYS-BEDSUIPN          PIC 9(04).
           03  WRK-FTNFLG               PIC 9(01).
           03  WRK-HKNFTN               PIC 9(09).
           03  WRK-KOHFTN               PIC 9(09).
           03  WRK-HKNSKYMONEY          PIC 9(09).
           03  WRK-HKNJA-TBL            PIC X(100).
           03  WRK-WARI                  PIC 9(02)V9.
           03  WRK-TYOKIFTN            PIC 9(09).
      *
      *    診療科名退避エリア
       01  SUM-AREA.
           03  SUM-SRYKAMAX              PIC 9(03).
           03  SUM-SRYDR-TBL       OCCURS 300.
               05  SUM-SRYKA             PIC X(02).
               05  SUM-SRYKANAME         PIC X(24).
      *    入院履歴情報退避エリア
       01  WRK-NYUIN-AREA.
           03  WRK-NYUIN-TBL.
               05  WRK-NYUIN-DAY         PIC 9(01) OCCURS 31.
      *
      *    室料差額退避エリア
       01  WRK-ROOM-SAGAKU-TBL.
           03  WRK-ROOM-MAX              PIC 9(02).
           03  WRK-ROOM-DATA     OCCURS 50.
               05  WRK-ROOM-CD           PIC 9(03).
               05  WRK-ROOM-SGK          PIC 9(05).
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
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    職員情報情報
           COPY    "CPSK1010.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    主科設定情報
           COPY    "CPSK2009.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    室料差額情報
           COPY    "CPSK5113.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    請求管理マスタ
           COPY    "CPRCF010.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "A00000S100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000S100-LNK.
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
           .
       000-PROC-EXT.
           EXIT PROGRAM
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
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-PARA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  WRK-ROOM-SAGAKU-TBL
           INITIALIZE                  WRK-NYUIN-AREA
           INITIALIZE                  SPA-AREA
      *
           MOVE    S100LNK-HOSPNUM    TO   WRK-PARA-HOSPNUM
           MOVE    WRK-PARA-HOSPNUM   TO   SPA-HOSPNUM
      *    ステップ管理開始処理
           MOVE    "STS"              TO   SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM        TO   JOB-HOSPNUM
           MOVE    S100LNK-JOBID      TO   JOB-JOBID
           MOVE    S100LNK-SHELLID    TO   JOB-SHELLID
           MOVE    "A00000S100"       TO   JOB-PGID
           MOVE    "保険別請求チェック表(国保)"
                                      TO   JOB-SHELLMSG
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
      *    システム日付
           MOVE  S100LNK-PRTKANRI-SKYYMD   TO   WRK-SYMD
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG               TO   WRK-SEIYMD
           MOVE  S100LNK-PRTKANRI-SKYYMD   TO   SPA-SYSYMD
      *
           MOVE   S100LNK-NYUGAIKBN        TO   WRK-PARA-NYUGAIKBN
      *
      *    対象日付処理
           MOVE    S100LNK-YM              TO   WRK-SRYYMD (1:6)
           MOVE    "01"                    TO   WRK-SRYYMD (7:2)
           MOVE    S100LNK-YM              TO   WRK-SYMD (1:6)
           MOVE    "01"                    TO   WRK-SYMD (7:2)
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)       TO   WRK-SRYYM
           MOVE    S100LNK-YM              TO   WRK-PARA-SRYYM
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    S100LNK-YM              TO   WRK-SYMD(1:6)
           PERFORM   31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD                TO   WRK-PARA-PRTLYMD
      *
      *    対象年月初日取得
           INITIALIZE  WRK-SYMD
           MOVE    S100LNK-YM              TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                    TO   WRK-PARA-PRTSYMD(7:2)
      *
      *    医療機関ＩＤ編集^M
           MOVE    SPACE                   TO   SYS-1001-REC
           INITIALIZE                           SYS-1001-REC
           MOVE    "1001"                  TO   SYS-1001-KANRICD
           MOVE    "*"                     TO   SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM             TO   SYS-1001-HOSPNUM
           MOVE    SPA-SYSYMD              TO   SYS-1001-STYUKYMD
                                                SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC            TO   MCPDATA-REC
           PERFORM   800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI   =   ZERO
             MOVE    MCPDATA-REC        TO   SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM   TO   WRK-PARA-HOSPNUM
             MOVE    SYS-1001-BEDSU     TO   WRK-SYS-BEDSU
             MOVE    SYS-1001-BEDSUIPN  TO   WRK-SYS-BEDSUIPN
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                        TO   WRK-RECEERR
             PERFORM   500-ERR-HENSYU-SEC
           END-IF
      *
      *    自賠責点数単価取得
           MOVE     SPACE              TO   SYS-4001-REC
           INITIALIZE                       SYS-4001-REC
           MOVE     "4001"             TO   SYS-4001-KANRICD
           MOVE     "*"                TO   SYS-4001-KBNCD
           MOVE     SPA-HOSPNUM        TO   SYS-4001-HOSPNUM
           MOVE     SPA-SYSYMD         TO   SYS-4001-STYUKYMD
                                            SYS-4001-EDYUKYMD
           MOVE     SYS-4001-REC  TO        MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF   FLG-SYSKANRI        =   ZERO
             MOVE    MCPDATA-REC TO  SYS-4001-REC
           ELSE
             INITIALIZE              SYS-4001-REC
           END-IF
           IF   SYS-4001-TENKBN  =  "1"
             MOVE    11.5              TO   WRK-JIBAITNK
           ELSE
             MOVE    12                TO   WRK-JIBAITNK
           END-IF
      *
      *    診療科情報取得
           MOVE    SPACE                  TO   SYS-1005-REC
           INITIALIZE                          SYS-1005-REC
           MOVE    "1005"                 TO   SYS-1005-KANRICD
           MOVE    SPA-SYSYMD             TO   SYS-1005-STYUKYMD
                                               SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM            TO   SYS-1005-HOSPNUM
           MOVE    WRK-PARA-PRTLYMD       TO   WRK-SYMD
           PERFORM   820-SYSKANRI-DBSELECT-SEC
           MOVE     1        TO     IDX1
           PERFORM  UNTIL  FLG-SYSKANRI3 = 1
             IF  FLG-SYSKANRI3  =  ZERO
               MOVE   SYS-1005-KBNCD        TO   SUM-SRYKA(IDX1)
               MOVE   SYS-1005-SRYKANAME1   TO   SUM-SRYKANAME(IDX1)
               ADD    1            TO   IDX1
             END-IF
             PERFORM   820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
           SUBTRACT    1      FROM   IDX1
           MOVE    IDX1          TO   SUM-SRYKAMAX
           PERFORM   820-SYSKANRI-DBCLOSE-SEC
      *
           MOVE    "S138TP"                 TO   S138TP-FILE-ID
           MOVE    S100LNK-PRTKANRI-TERMID  TO   S138TP-TERMID
           MOVE    WRK-PARA-HOSPNUM         TO   S138TP-HOSPNUM
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
      *    データ集計
           PERFORM  2001-SYUKEI-SEC
      *
      *    印刷処理
           IF    CNT-DATA  >  ZERO
             PERFORM  2002-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ抽出処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      *
      *
           OPEN     OUTPUT  S138TMP-FILE
           CLOSE            S138TMP-FILE
           OPEN     I-O     S138TMP-FILE
      *    自費データ集計
           IF  WRK-PARA-NYUGAIKBN  =  2
             OR  WRK-PARA-NYUGAIKBN  =  SPACE
             PERFORM  2001-JIHI-SYUKEI-SEC
           END-IF
      *    国保データ集計
           PERFORM  2001-KOKUHO-SYUKEI-SEC
      *
           CLOSE   S138TMP-FILE
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費データ抽出処理１
      *****************************************************************
       2001-JIHI-SYUKEI-SEC                SECTION.
      *
           MOVE    "2"        TO   WRK-NYUGAIKBN
           PERFORM  920-DBSELECT3-SEC
             PERFORM  UNTIL   FLG-JYURRK   =   1
      *    データ集計
             PERFORM  2001-JIHI-G-SEC
             PERFORM  920-DBFETCH3-SEC
           END-PERFORM
      *
           PERFORM  920-DBCLOSE3-SEC
      *
           .
       2001-JIHI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費データ抽出処理２
      *****************************************************************
        2001-JIHI-G-SEC                SECTION.
      *
      *    収納情報よりデータ抽出
           PERFORM  940-SYUNOU-READ-SEC
           PERFORM  910-PTHKNINF-READ-SEC
           IF  SYU-SYUHKNNUM  =  "060"
               OR  SYU-SYUHKNNUM  =  "067"
             IF    SYU-TOTAL-HKNTEN            =  0
                 AND    SYU-TOTAL-MONEY             =  0
                 AND  ( SYU-JIHI-TOTAL         NOT  =  0
                 OR     SYU-JIHI-TOTAL-TAX     NOT  =  0
                 OR     SYU-TOTAL-TGMONEY      NOT  =  0
                 OR     SYU-TOTAL-TGMONEY-TAX  NOT  =  0 )
               COMPUTE  WRK-JIHI     =  WRK-JIHI
                                     +  SYU-JIHI-TOTAL
                                     +  SYU-JIHI-TOTAL-TAX
               COMPUTE  WRK-TGMONEY  =  WRK-TGMONEY
                                     +  SYU-TOTAL-TGMONEY
                                     +  SYU-TOTAL-TGMONEY-TAX
                                     +  SYU-CHOSEI
               PERFORM  2001-KASAN-JIHI-G-SEC
               MOVE     0   TO   WRK-JIHI
                                 WRK-TGMONEY
             END-IF
           END-IF
      *
           .
        2001-JIHI-G-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費集計処理１
      *****************************************************************
        2001-KASAN-JIHI-G-SEC               SECTION.
      *
           MOVE     SYU-PTID                TO   WRK-PTID
           PERFORM  900-PTINF-READ-SEC
           PERFORM  930-HKNCOMBI-SET-SEC
      *
      *    保険算定年齢チェック等
           INITIALIZE                       ORCSAGECHKAREA
           MOVE     SYU-HOSPNUM             TO  AGECHK-HOSPNUM
           MOVE     SYU-PTID                TO  AGECHK-PTID
           MOVE     S100LNK-YM              TO  AGECHK-SRYYMD(1:6)
           MOVE     "01"                    TO  AGECHK-SRYYMD(7:2)
           MOVE     COMB-HKNCOMBINUM        TO  AGECHK-HKNCOMBINUM
           CALL     "ORCSAGECHK"            USING
                                            ORCSAGECHKAREA
                                            SPA-AREA
      *
           MOVE     PTINF-BIRTHDAY(1:4)     TO    WRK-YY1
           MOVE     S100LNK-YM(1:4)         TO    WRK-YY2
           COMPUTE  WRK-NENREI   =   WRK-YY2   -   WRK-YY1
           IF  PTINF-BIRTHDAY(5:2)  >  S100LNK-YM(5:2)
               SUBTRACT   1   FROM   WRK-NENREI
           END-IF
      *
      *    キー設定
           INITIALIZE    S138TMP-REC
      *
           MOVE     SYU-NYUGAIKBN       TO    S138TMP-NYUGAIKBN
      *
           IF    SYS-1001-HOSPSBT1  =  1
             OR  SYS-2009-SYUKA-KBN = 1
             MOVE    SYU-SRYKA        TO    S138TMP-SRYKA
           ELSE
             MOVE    "00"               TO    S138TMP-SRYKA
           END-IF
      *
           MOVE    "9"       TO   S138TMP-SRYYMKBN
      *    TMPファイル読み込み
           MOVE    2                    TO   FLG-TYPE
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    国保組合判定
           MOVE    ZERO     TO    FLG-KUMIAI
           IF    PTHKN-HKNJANUM(7:2)  =  SPACE
             IF    PTHKN-HKNJANUM(3:1)  =  "3"
               MOVE    1    TO    FLG-KUMIAI
             END-IF
             MOVE  PTHKN-HKNJANUM(1:6)    TO   WRK-HKNJANUM
           ELSE
             IF    PTHKN-HKNJANUM(5:1)  =  "3"
               MOVE    1    TO    FLG-KUMIAI
             END-IF
             MOVE  PTHKN-HKNJANUM(3:6)    TO   WRK-HKNJANUM
           END-IF
      *
      *    公費設定
           MOVE   SYU-KOH1HKNNUM   TO   WRK-JIHI-HKNNUM(1)
           MOVE   SYU-KOH2HKNNUM   TO   WRK-JIHI-HKNNUM(2)
           MOVE   SYU-KOH3HKNNUM   TO   WRK-JIHI-HKNNUM(3)
           MOVE   SYU-KOH4HKNNUM   TO   WRK-JIHI-HKNNUM(4)
      *
      *    明細集計場所設定
           IF   FLG-KUMIAI   =   ZERO
      *    公費集計処理
             IF    SYU-KOH1HKNNUM  NOT = SPACE
               PERFORM  2001-JIHI-IK-DATA-SET-SEC
             END-IF
             PERFORM 2001-JIHI-IDX-SET-SEC
      *    一般国保集計
             PERFORM 2001-JIHI-DATA-SET-SEC
           ELSE
      *    公費集計処理
             IF    SYU-KOH1HKNNUM  NOT = SPACE
               PERFORM  2001-JIHI-KK-DATA-SET-SEC
             END-IF
             PERFORM 2001-JIHI-IDX-SET-K-SEC
      *    国保組合集計
             PERFORM 2001-JIHI-DATA-SET-K-SEC
           END-IF
      *
      *    書き出し
           IF  FLG-S138TMP   =  1
             ADD      1             TO   CNT-DATA
             WRITE    S138TMP-REC
           ELSE
             REWRITE  S138TMP-REC
           END-IF
      *
           .
        2001-KASAN-JIHI-G-EXT.
           EXIT.
      *****************************************************************
      *    自費集計場所設定（一般国保）
      *****************************************************************
       2001-JIHI-IDX-SET-SEC                SECTION.
      *
           IF    SYU-SYUHKNNUM = "060"
             IF    WRK-NENREI  >= 70
               IF    SYU-PTFTNRATE  =  10
                 MOVE      1         TO    IDX1
               ELSE
                 MOVE      2         TO    IDX1
               END-IF
             ELSE
               IF    AGECHK-SYUGAKUKBN  =  1
                 MOVE      4       TO    IDX1
               ELSE
                 MOVE      3       TO    IDX1
               END-IF
             END-IF
      *    小計集計場所設定
             MOVE          5         TO    IDX2
           ELSE
             IF    WRK-NENREI    >= 70
               IF    SYU-PTFTNRATE  =  10
                 MOVE      6         TO    IDX1
               ELSE
                 MOVE      7         TO    IDX1
               END-IF
             ELSE
               IF    AGECHK-SYUGAKUKBN  =  1
                 MOVE      10      TO    IDX1
               ELSE
                 IF    COMB-HONKZKKBN  =  "1"
                   MOVE    8       TO    IDX1
                 ELSE
                   MOVE    9       TO    IDX1
                 END-IF
               END-IF
             END-IF
      *    小計集計場所設定
             MOVE          11        TO    IDX2
           END-IF
           .
       2001-JIHI-IDX-SET-EXT.
           EXIT.
      *****************************************************************
      *    自費集計処理２（一般国保）
      *****************************************************************
       2001-JIHI-DATA-SET-SEC                SECTION.
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
             ADD    WRK-TGMONEY          TO    S138TMP-MADOGK(IDX1)
                                               S138TMP-MADOGK(IDX2)
                                               S138TMP-GMADOGK
      *
      *    自費計加算
             ADD    WRK-JIHI             TO    S138TMP-JIHIGK(IDX1)
                                               S138TMP-JIHIGK(IDX2)
                                               S138TMP-GJIHIGK
      *
      *    医療収益
             ADD    WRK-JIHI             TO    S138TMP-SYUEKI(IDX1)
                                               S138TMP-SYUEKI(IDX2)
                                               S138TMP-GSYUEKI
             ADD    WRK-TGMONEY          TO    S138TMP-SYUEKI(IDX1)
                                               S138TMP-SYUEKI(IDX2)
                                               S138TMP-GSYUEKI
      *
             MOVE    1             TO    IDC
             MOVE    300           TO    IDX-MAX
             PERFORM    UNTIL   IDC   >    IDX-MAX
               IF   S138TMP-HKNJANUM(IDC)   =  SPACE  OR  WRK-HKNJANUM
                 MOVE    WRK-HKNJANUM  TO   S138TMP-HKNJANUM(IDC)
                 MOVE    ZERO      TO    IDX-MAX
               ELSE
                 ADD     1         TO    IDC
               END-IF
             END-PERFORM
      *
             IF    IDC    <=  300
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
             ADD    WRK-TGMONEY          TO    S138TMP-HMADOGK(IDC)
      *
      *    自費計加算
             ADD    WRK-JIHI             TO    S138TMP-HJIHIGK(IDC)
      *
      *    医療収益
             ADD    WRK-JIHI             TO    S138TMP-HSYUEKI(IDC)
             ADD    WRK-TGMONEY          TO    S138TMP-HSYUEKI(IDC)
      *
           END-IF
           .
       2001-JIHI-DATA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計処理（一般国保）
      *****************************************************************
       2001-JIHI-IK-DATA-SET-SEC                SECTION.
      *
           PERFORM VARYING IDX5     FROM    1   BY  1
                   UNTIL   IDX5   >   4
                   OR  WRK-JIHI-TBL(IDX5) =   SPACE
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     300
                      OR       S138TMP-KOH1-KEY (IDX4) = SPACE
               IF  WRK-JIHI-TBL(IDX5)  =  S138TMP-KOHNUM (IDX4)
                   MOVE  IDX4  TO  CNT-KOH
                   MOVE  300    TO  IDX4
               END-IF
             END-PERFORM
      *
             IF  IDX4  < 300
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    公費名
      *    ０９１と０５１を同じ公費として扱う
             IF   WRK-JIHI-TBL(IDX5)  =  "091"
               MOVE  "051"               TO   S138TMP-KOHNUM(CNT-KOH)
             ELSE
               MOVE  WRK-JIHI-TBL(IDX5)  TO   S138TMP-KOHNUM(CNT-KOH)
             END-IF
      *    自費・保険外のみの場合、件数・日数は集計しない
      *
           END-PERFORM
           .
       2001-JIHI-IK-DATA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費集計場所設定（国保組合）
      *****************************************************************
       2001-JIHI-IDX-SET-K-SEC                SECTION.
      *
           IF    COMB-HONKZKKBN    =    "1"
             IF    WRK-NENREI       >=    70
               IF    SYU-PTFTNRATE  =  10
                 MOVE      1         TO    IDX1
               ELSE
                 MOVE      2         TO    IDX1
               END-IF
             ELSE
               MOVE        3         TO    IDX1
             END-IF
           ELSE
             IF    WRK-NENREI       >=    70
               IF    SYU-PTFTNRATE  =  10
                 MOVE      4         TO    IDX1
               ELSE
                 MOVE      5         TO    IDX1
               END-IF
             ELSE
               IF    AGECHK-SYUGAKUKBN  =  1
                 MOVE      7         TO    IDX1
               ELSE
                 MOVE      6         TO    IDX1
               END-IF
             END-IF
           END-IF
      *    小計集計場所設定
             MOVE    8         TO    IDX2
           .
       2001-JIHI-IDX-SET-K-EXT.
           EXIT.
      *****************************************************************
      *    自費集計処理２（国保組合）
      *****************************************************************
       2001-JIHI-DATA-SET-K-SEC                SECTION.
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
           ADD    WRK-TGMONEY      TO    S138TMP-K-MADOGK(IDX1)
                                         S138TMP-K-MADOGK(IDX2)
                                         S138TMP-GMADOGK
      *
      *    自費計加算
           ADD    WRK-JIHI         TO    S138TMP-K-JIHIGK(IDX1)
                                         S138TMP-K-JIHIGK(IDX2)
                                         S138TMP-GJIHIGK
      *
      *    医療収益
           ADD    WRK-JIHI         TO    S138TMP-K-SYUEKI(IDX1)
                                         S138TMP-K-SYUEKI(IDX2)
                                         S138TMP-GSYUEKI
           ADD    WRK-TGMONEY      TO    S138TMP-K-SYUEKI(IDX1)
                                         S138TMP-K-SYUEKI(IDX2)
                                         S138TMP-GSYUEKI
      *
           MOVE    1             TO    IDC
           MOVE    300            TO    IDX-MAX
           PERFORM    UNTIL   IDC   >    IDX-MAX
             IF   S138TMP-K-HKNJANUM(IDC)   =  SPACE  OR  WRK-HKNJANUM
               MOVE    WRK-HKNJANUM  TO   S138TMP-K-HKNJANUM(IDC)
               MOVE    ZERO      TO    IDX-MAX
             ELSE
               ADD     1         TO    IDC
             END-IF
           END-PERFORM
           IF    IDC    <=  300
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
             ADD    WRK-TGMONEY          TO    S138TMP-K-HMADOGK(IDC)
      *
      *    自費計加算
             ADD    WRK-JIHI             TO    S138TMP-K-HJIHIGK(IDC)
      *
      *    医療収益
             ADD    WRK-JIHI             TO    S138TMP-K-HSYUEKI(IDC)
             ADD    WRK-TGMONEY          TO    S138TMP-K-HSYUEKI(IDC)
      *
           END-IF
           .
       2001-JIHI-DATA-SET-K-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費集計処理(国保組合)
      *****************************************************************
       2001-JIHI-KK-DATA-SET-SEC               SECTION.
      *
           PERFORM VARYING IDX5     FROM    1   BY  1
                   UNTIL   IDX5     >       4
                   OR  WRK-JIHI-TBL(IDX5) =   SPACE
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     300
                      OR       S138TMP-KOH2-KEY (IDX4) = SPACE
               IF  WRK-JIHI-TBL(IDX5)  =  S138TMP-K-KOHNUM(IDX4)
                 MOVE    IDX4      TO   CNT-KOH
                 MOVE    300       TO   IDX4
               END-IF
             END-PERFORM
      *
             IF  IDX4  < 300
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    公費名
             MOVE  WRK-JIHI-TBL(IDX5)  TO  S138TMP-K-KOHNUM(CNT-KOH)
      *    自費・保険外のみの場合、件数・日数は集計しない
      *
           END-PERFORM
           .
       2001-JIHI-KK-DATA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保データ抽出処理
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC                SECTION.
      *
           PERFORM  900-SEIKYU-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-END   =   1
      *
      *    集計場所設定
             PERFORM   2001-IDX-SET2-SEC
      *
      *    国保データ集計
             IF    FLG-KUMIAI  =  ZERO
               PERFORM   2001-DATA-SET2-SEC
             ELSE
               PERFORM   2001-DATA-SET2-K-SEC
             END-IF
      *
             PERFORM   900-SEIKYU-READ-SEC
           END-PERFORM
      *
           PERFORM   900-SEIKYU-CLOSE-SEC
      *
           .
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保集計場所設定
      *****************************************************************
       2001-IDX-SET2-SEC                SECTION.
      *
      *    国保組合判定
           MOVE    ZERO    TO    FLG-KUMIAI
           IF    RECE10-HKNJANUM(7:2)  =  SPACE
             IF    RECE10-HKNJANUM(3:1)  =  "3"
               MOVE    1    TO    FLG-KUMIAI
             END-IF
             MOVE  RECE10-HKNJANUM(1:6)    TO   WRK-HKNJANUM
           ELSE
             IF    RECE10-HKNJANUM(5:1)  =  "3"
               MOVE    1    TO    FLG-KUMIAI
             END-IF
             MOVE  RECE10-HKNJANUM(3:6)    TO   WRK-HKNJANUM
           END-IF
      *
      *    明細集計場所設定
           IF    FLG-KUMIAI   =   ZERO
      *    一般国保
             IF    RECE10-RJNHKNNUM NOT = SPACE
               IF    RECE10-KYURATE  =  9
                 MOVE          12      TO    IDX1
               ELSE
                 MOVE          13      TO    IDX1
               END-IF
      *    小計集計場所設定
               MOVE            14      TO    IDX2
             ELSE
               IF    RECE10-HKNNUM = "060"
                 IF    RECE10-AGE  >= 70
                   IF    RECE10-KYURATE  =  9
                     MOVE      1       TO    IDX1
                   ELSE
                     MOVE      2       TO    IDX1
                   END-IF
                 ELSE
                   IF S100LNK-YM < "200804"
                     IF    RECE10-AGE  <  3
                       MOVE      4       TO    IDX1
                     ELSE
                       MOVE      3       TO    IDX1
                     END-IF
                   ELSE
                     IF    RECE10-FLG(1)  =  1
                       MOVE      4       TO    IDX1
                     ELSE
                       MOVE      3       TO    IDX1
                     END-IF
                   END-IF
                 END-IF
      *    小計集計場所設定
                 MOVE          5       TO    IDX2
               ELSE
                 IF    RECE10-AGE    >= 70
                   IF    RECE10-KYURATE  =  9
                     MOVE      6       TO    IDX1
                   ELSE
                     MOVE      7       TO    IDX1
                   END-IF
                 ELSE
                   IF S100LNK-YM < "200804"
                     IF    RECE10-AGE  <  3
                       MOVE      10       TO    IDX1
                     ELSE
                       IF    RECE10-HONKZKKBN  =  "1"
                         MOVE    8       TO    IDX1
                       ELSE
                         MOVE    9       TO    IDX1
                       END-IF
                     END-IF
                   ELSE
                     IF    RECE10-FLG(1)  =  1
                       MOVE      10       TO    IDX1
                     ELSE
                       IF    RECE10-HONKZKKBN  =  "1"
                         MOVE    8       TO    IDX1
                       ELSE
                         MOVE    9       TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
      *    小計集計場所設定
                 MOVE          11      TO    IDX2
               END-IF
             END-IF
           ELSE
      *    国保組合
             IF    RECE10-RJNHKNNUM  NOT  =  SPACE
               IF    RECE10-KYURATE    =    9
                 MOVE     9       TO    IDX1
               ELSE
                 MOVE     10      TO    IDX1
               END-IF
      *    小計集計場所設定
               MOVE     11      TO    IDX2
             ELSE
               IF    RECE10-HONKZKKBN    =    "1"
                 IF    RECE10-AGE       >=    70
                   IF    RECE10-KYURATE  =    9
                     MOVE      1       TO    IDX1
                   ELSE
                     MOVE      2       TO    IDX1
                   END-IF
                 ELSE
                   MOVE     3       TO    IDX1
                 END-IF
               ELSE
                 IF    RECE10-AGE       >=    70
                   IF    RECE10-KYURATE  =    9
                     MOVE      4       TO    IDX1
                   ELSE
                     MOVE      5       TO    IDX1
                   END-IF
                 ELSE
                   IF S100LNK-YM < "200804"
                     IF    RECE10-AGE  <  3
                       MOVE      7       TO    IDX1
                     ELSE
                       MOVE      6       TO    IDX1
                     END-IF
                   ELSE
                     IF    RECE10-FLG(1)  =  1
                       MOVE      7       TO    IDX1
                     ELSE
                       MOVE      6       TO    IDX1
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *    小計集計場所設定
               MOVE     8       TO    IDX2
             END-IF
           END-IF
      *
           .
       2001-IDX-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保データ集計処理
      *****************************************************************
       2001-DATA-SET2-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE    S138TMP-REC
      *
           MOVE    RECE10-NYUGAIKBN    TO    S138TMP-NYUGAIKBN
           MOVE    RECE10-RECEKA       TO    S138TMP-SRYKA
           IF  RECE10-SRYYM < "200804"
             MOVE    "0"       TO    S138TMP-SRYYMKBN
           ELSE
             MOVE    "9"       TO    S138TMP-SRYYMKBN
           END-IF
      *    TMPファイル読み込み
           MOVE    2                   TO    FLG-TYPE
           PERFORM 800-SKTTMP-READ-SEC
      *
      *    加算データ設定
           PERFORM  2001-KASAN-SEC
      *
      *    件数加算
           ADD    1                    TO    S138TMP-KENSU(IDX1)
                                             S138TMP-KENSU(IDX2)
                                             S138TMP-GKENSU
      *
      *    日数加算
           ADD    WRK-NISSU            TO    S138TMP-NISSU(IDX1)
                                             S138TMP-NISSU(IDX2)
                                             S138TMP-GNISSU
      *
      *    点数加算
           ADD    WRK-TENSU            TO    S138TMP-TENSU(IDX1)
                                             S138TMP-TENSU(IDX2)
                                             S138TMP-GTENSU
      *
      *    窓口請求額加算
           ADD    WRK-MADOGK           TO    S138TMP-MADOGK(IDX1)
                                             S138TMP-MADOGK(IDX2)
                                             S138TMP-GMADOGK
      *
      *    自費計加算
           ADD    WRK-JIHIGK           TO    S138TMP-JIHIGK(IDX1)
                                             S138TMP-JIHIGK(IDX2)
                                             S138TMP-GJIHIGK
      *
      *    保険請求額加算
           ADD    WRK-HKNGK            TO    S138TMP-HKNGK(IDX1)
                                             S138TMP-HKNGK(IDX2)
                                             S138TMP-GHKNGK
      *
      *    公費請求額
           ADD    WRK-KOHGK            TO    S138TMP-KOHGK(IDX1)
                                             S138TMP-KOHGK(IDX2)
                                             S138TMP-GKOHGK
      *
      *    医療収益
           ADD    WRK-SYUEKI           TO    S138TMP-SYUEKI(IDX1)
                                             S138TMP-SYUEKI(IDX2)
                                             S138TMP-GSYUEKI
      *
      *      日数加算
             ADD    WRK-SNISSU         TO    S138TMP-SNISSU(IDX1)
                                             S138TMP-SNISSU(IDX2)
                                             S138TMP-SGNISSU
      *      請求額加算
             ADD    WRK-SSEIKYU        TO    S138TMP-SSEIKYU(IDX1)
                                             S138TMP-SSEIKYU(IDX2)
                                             S138TMP-SGSEIKYU
      *      標準負担額加算
             ADD    WRK-SFTNGK         TO    S138TMP-SFTNGK(IDX1)
                                             S138TMP-SFTNGK(IDX2)
                                             S138TMP-SGFTNGK
      *
      *    公費集計処理
           IF    RECE10-KOHNUM(1)  NOT = SPACE
             PERFORM  2001-DATA-SET21-SEC
           END-IF
      *
      *    保険者毎の集計処理
           IF    RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)    TO    WRK-HKNJANUM
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)    TO    WRK-HKNJANUM
           END-IF
      *
           PERFORM  2001-DATA-SET22-SEC
      *
      *    書き出し
           IF  FLG-S138TMP   =  1
             ADD      1                    TO    CNT-DATA
             WRITE    S138TMP-REC
           ELSE
             REWRITE  S138TMP-REC
           END-IF
      *
           .
       2001-DATA-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保公費集計処理
      *****************************************************************
       2001-DATA-SET21-SEC                SECTION.
      *
           PERFORM VARYING IDX5     FROM    1   BY  1
                   UNTIL   IDX5   >   4
                   OR  RECE10-KOHNUM (IDX5) =   SPACE
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     300
                      OR       S138TMP-KOH1-KEY (IDX4) = SPACE
               IF  RECE10-KOHNUM (IDX5)  =  S138TMP-KOHNUM (IDX4) AND
                   RECE10-RJNHKNNUM      =  S138TMP-RJNKBN(IDX4)
                   MOVE  IDX4  TO  CNT-KOH
                   MOVE  300    TO  IDX4
               END-IF
             END-PERFORM
      *
      *    該当公費がなかった場合
             IF  IDX4  < 300
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    老人併用区分設定
             MOVE  RECE10-RJNHKNNUM     TO  S138TMP-RJNKBN(CNT-KOH)
      *    公費名
             MOVE  RECE10-KOHNUM(IDX5)  TO  S138TMP-KOHNUM(CNT-KOH)
      *    療養の給付：件数
             ADD   1  TO  S138TMP-KKENSU(CNT-KOH)
      *    療養の給付:診療実日数
             ADD  RECE10-JNISSU(IDX5 + 1)  TO
                               S138TMP-KNISSU(CNT-KOH)
      *    療養の給付：点数
             ADD  RECE10-TOTALTEN(IDX5 + 1)  TO
                               S138TMP-KTENSU(CNT-KOH)
      *
             ADD     1                          TO  WRK-KKENSU
             ADD     RECE10-JNISSU(IDX5 + 1)    TO  WRK-KNISSU
             ADD     RECE10-TOTALTEN(IDX5 + 1)  TO  WRK-KTENSU
      *
           END-PERFORM
           .
       2001-DATA-SET21-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保保険者毎の処理
      *****************************************************************
       2001-DATA-SET22-SEC                SECTION.
      *
           MOVE    1             TO    IDC
           MOVE    300            TO    IDX-MAX
           PERFORM    UNTIL   IDC   >    IDX-MAX
             IF   S138TMP-HKNJANUM(IDC)   =  SPACE  OR WRK-HKNJANUM
               MOVE    WRK-HKNJANUM  TO   S138TMP-HKNJANUM(IDC)
               MOVE    ZERO      TO    IDX-MAX
             ELSE
               ADD     1         TO    IDC
             END-IF
           END-PERFORM
      *
      *    保険者加算
           IF    IDC    <=  300
      *    件数加算
             ADD    1                    TO    S138TMP-HKENSU(IDC)
      *
      *    日数加算
             ADD    WRK-NISSU            TO    S138TMP-HNISSU(IDC)
      *
      *    点数加算
             ADD    WRK-TENSU            TO    S138TMP-HTENSU(IDC)
      *
      *    窓口請求額加算
             ADD    WRK-MADOGK           TO    S138TMP-HMADOGK(IDC)
      *
      *    自費計加算
             ADD    WRK-JIHIGK           TO    S138TMP-HJIHIGK(IDC)
      *
      *    保険請求額加算
             ADD    WRK-HKNGK            TO    S138TMP-HHKNGK(IDC)
      *
      *    公費請求額
             ADD    WRK-KOHGK            TO    S138TMP-HKOHGK(IDC)
      *
      *    医療収益
             ADD    WRK-SYUEKI           TO    S138TMP-HSYUEKI(IDC)
      *
      *    日数加算
             ADD    WRK-SNISSU         TO    S138TMP-HSNISSU(IDC)
      *
      *    請求額加算
             ADD    WRK-SSEIKYU        TO    S138TMP-HSSEIKYU(IDC)
      *
      *    標準負担額加算
             ADD    WRK-SFTNGK         TO    S138TMP-HSFTNGK(IDC)
      *
           END-IF
      *
           .
       2001-DATA-SET22-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保データ集計処理(国保組合)
      *****************************************************************
       2001-DATA-SET2-K-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE    S138TMP-REC
      *
           MOVE    RECE10-NYUGAIKBN    TO    S138TMP-NYUGAIKBN
           MOVE    RECE10-RECEKA       TO    S138TMP-SRYKA
           IF  RECE10-SRYYM < "200804"
             MOVE    "0"       TO    S138TMP-SRYYMKBN
           ELSE
             MOVE    "9"       TO    S138TMP-SRYYMKBN
           END-IF
      *    TMPファイル読み込み
           MOVE    2                   TO    FLG-TYPE
           PERFORM 800-SKTTMP-READ-SEC
      *
      *    加算データ設定
           PERFORM  2001-KASAN-SEC
      *
      *    件数加算
           ADD    1                    TO    S138TMP-K-KENSU(IDX1)
                                             S138TMP-K-KENSU(IDX2)
                                             S138TMP-GKENSU
      *
      *    日数加算
           ADD    WRK-NISSU            TO    S138TMP-K-NISSU(IDX1)
                                             S138TMP-K-NISSU(IDX2)
                                             S138TMP-GNISSU
      *
      *    点数加算
           ADD    WRK-TENSU            TO    S138TMP-K-TENSU(IDX1)
                                             S138TMP-K-TENSU(IDX2)
                                             S138TMP-GTENSU
      *
      *    窓口請求額加算
           ADD    WRK-MADOGK           TO    S138TMP-K-MADOGK(IDX1)
                                             S138TMP-K-MADOGK(IDX2)
                                             S138TMP-GMADOGK
      *
      *    自費計加算
           ADD    WRK-JIHIGK           TO    S138TMP-K-JIHIGK(IDX1)
                                             S138TMP-K-JIHIGK(IDX2)
                                             S138TMP-GJIHIGK
      *
      *    保険請求額加算
           ADD    WRK-HKNGK            TO    S138TMP-K-HKNGK(IDX1)
                                             S138TMP-K-HKNGK(IDX2)
                                             S138TMP-GHKNGK
      *
      *    公費請求額
           ADD    WRK-KOHGK            TO    S138TMP-K-KOHGK(IDX1)
                                             S138TMP-K-KOHGK(IDX2)
                                             S138TMP-GKOHGK
      *
      *    医療収益
           ADD    WRK-SYUEKI           TO    S138TMP-K-SYUEKI(IDX1)
                                             S138TMP-K-SYUEKI(IDX2)
                                             S138TMP-GSYUEKI
      *
      *      日数加算
             ADD    WRK-SNISSU         TO    S138TMP-K-SNISSU(IDX1)
                                             S138TMP-K-SNISSU(IDX2)
                                             S138TMP-SGNISSU
      *      請求額加算
             ADD    WRK-SSEIKYU        TO    S138TMP-K-SSEIKYU(IDX1)
                                             S138TMP-K-SSEIKYU(IDX2)
                                             S138TMP-SGSEIKYU
      *      標準負担額加算
             ADD    WRK-SFTNGK         TO    S138TMP-K-SFTNGK(IDX1)
                                             S138TMP-K-SFTNGK(IDX2)
                                             S138TMP-SGFTNGK
      *
      *    公費集計処理
           IF    RECE10-KOHNUM(1)  NOT = SPACE
             PERFORM  2001-DATA-SET21-K-SEC
           END-IF
      *
      *    保険者毎の集計処理
           IF    RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)    TO    WRK-HKNJANUM
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)    TO    WRK-HKNJANUM
           END-IF
      *
           PERFORM  2001-DATA-SET22-K-SEC
      *
      *    書き出し
           IF  FLG-S138TMP   =  1
             ADD      1                  TO    CNT-DATA
             WRITE    S138TMP-REC
           ELSE
             REWRITE  S138TMP-REC
           END-IF
      *
           .
       2001-DATA-SET2-K-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保公費集計処理(国保組合)
      *****************************************************************
       2001-DATA-SET21-K-SEC                SECTION.
      *
           PERFORM VARYING IDX5     FROM    1   BY  1
                   UNTIL   IDX5     >       4
                   OR  RECE10-KOHNUM (IDX5) =   SPACE
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     300
                      OR       S138TMP-KOH2-KEY (IDX4) = SPACE
               IF  RECE10-KOHNUM(IDX5)  =  S138TMP-K-KOHNUM(IDX4)
                   AND  RECE10-RJNHKNNUM  =  S138TMP-K-RJNKBN(IDX4)
                 MOVE    IDX4      TO   CNT-KOH
                 MOVE    300       TO   IDX4
               END-IF
             END-PERFORM
      *
      *    該当公費がなかった場合
             IF  IDX4  < 300
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    老人併用区分設定
             MOVE  RECE10-RJNHKNNUM     TO  S138TMP-K-RJNKBN(CNT-KOH)
      *    公費名
             MOVE  RECE10-KOHNUM(IDX5)  TO  S138TMP-K-KOHNUM(CNT-KOH)
      *    療養の給付：件数
             ADD   1  TO  S138TMP-K-KKENSU(CNT-KOH)
      *    療養の給付:診療実日数
             ADD  RECE10-JNISSU(IDX5 + 1)  TO
                               S138TMP-K-KNISSU(CNT-KOH)
      *    療養の給付：点数
             ADD  RECE10-TOTALTEN(IDX5 + 1)  TO
                               S138TMP-K-KTENSU(CNT-KOH)
      *
             ADD     1                          TO  WRK-KKENSU
             ADD     RECE10-JNISSU(IDX5 + 1)    TO  WRK-KNISSU
             ADD     RECE10-TOTALTEN(IDX5 + 1)  TO  WRK-KTENSU
      *
           END-PERFORM
           .
       2001-DATA-SET21-K-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保保険者毎の処理(国保組合)
      *****************************************************************
       2001-DATA-SET22-K-SEC                SECTION.
      *
           MOVE    1             TO    IDC
           MOVE    300            TO    IDX-MAX
           PERFORM    UNTIL   IDC   >    IDX-MAX
             IF   S138TMP-K-HKNJANUM(IDC)   =  SPACE  OR WRK-HKNJANUM
               MOVE    WRK-HKNJANUM  TO   S138TMP-K-HKNJANUM(IDC)
               MOVE    ZERO      TO    IDX-MAX
             ELSE
               ADD     1         TO    IDC
             END-IF
           END-PERFORM
      *
      *    保険者加算
           IF    IDC    <=  300
      *    件数加算
             ADD    1                    TO    S138TMP-K-HKENSU(IDC)
      *
      *    日数加算
             ADD    WRK-NISSU            TO    S138TMP-K-HNISSU(IDC)
      *
      *    点数加算
             ADD    WRK-TENSU            TO    S138TMP-K-HTENSU(IDC)
      *
      *    窓口請求額加算
             ADD    WRK-MADOGK           TO    S138TMP-K-HMADOGK(IDC)
      *
      *    自費計加算
             ADD    WRK-JIHIGK           TO    S138TMP-K-HJIHIGK(IDC)
      *
      *    保険請求額加算
             ADD    WRK-HKNGK            TO    S138TMP-K-HHKNGK(IDC)
      *
      *    公費請求額
             ADD    WRK-KOHGK            TO    S138TMP-K-HKOHGK(IDC)
      *
      *    医療収益
             ADD    WRK-SYUEKI           TO    S138TMP-K-HSYUEKI(IDC)
      *
      *    日数加算
             ADD    WRK-SNISSU         TO    S138TMP-K-HSNISSU(IDC)
      *
      *    請求額加算
             ADD    WRK-SSEIKYU        TO    S138TMP-K-HSSEIKYU(IDC)
      *
      *    標準負担額加算
             ADD    WRK-SFTNGK         TO    S138TMP-K-HSFTNGK(IDC)
      *
           END-IF
      *
      *
           .
       2001-DATA-SET22-K-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算データ設定処理
      *****************************************************************
       2001-KASAN-SEC                SECTION.
      *
      *
           INITIALIZE    WRK-KASAN-TBL
      *
           MOVE    RECE10-JNISSU(1)    TO    WRK-NISSU
           MOVE    RECE10-TOTALTEN(1)  TO    WRK-TENSU
           MOVE    ZERO                TO   FLG-TYOKI
      *
           IF   (RECE10-TOKKI(1)  =  "02"
               OR  RECE10-TOKKI(2)  =  "02"
               OR  RECE10-TOKKI(3)  =  "02"
               OR  RECE10-TOKKI(4)  =  "02"
               OR  RECE10-TOKKI(5)  =  "02")
             MOVE    1        TO   FLG-TYOKI
             MOVE    10000    TO   WRK-TYOKIFTN
           ELSE
             IF  (RECE10-TOKKI(1)  =  "16"
                 OR  RECE10-TOKKI(2)  =  "16"
                 OR  RECE10-TOKKI(3)  =  "16"
                 OR  RECE10-TOKKI(4)  =  "16"
                 OR  RECE10-TOKKI(5)  =  "16")
               MOVE    2        TO   FLG-TYOKI
               MOVE    20000    TO   WRK-TYOKIFTN
             END-IF
           END-IF
      *    限度額が有る場合は(点数×１０) − 患者負担 で算出する
           IF    RECE10-FTNMONEY(1)  =  ZERO
               AND  RECE10-KYURATE  NOT =  ZERO
             IF    FLG-TYOKI  NOT =  1
                 AND   FLG-TYOKI  NOT =  2
               COMPUTE   WRK-HKNGK  =  RECE10-TOTALTEN(1)
                                    *  RECE10-KYURATE
             END-IF
           ELSE
             COMPUTE   WRK-HKNGK  =  (RECE10-TOTALTEN(1)  *  10)
                                  -  RECE10-FTNMONEY(1)
           END-IF
      *    収納情報から窓口請求額、公費請求額を集計
           MOVE    ZERO              TO    WRK-MADOSEIKYUGK-G
                                           WRK-KOHSEIKYUGK-G
                                           WRK-JIHIGK-G
      *
           PERFORM  920-SYUNOU-SET-SEC
      *    その他自費を別集計
           MOVE    WRK-JIHIGK-G             TO    WRK-JIHIGK
           COMPUTE WRK-MADOGK  =  WRK-MADOSEIKYUGK-G  -  WRK-JIHIGK-G
      *
           MOVE    WRK-KOHSEIKYUGK-G        TO    WRK-KOHGK
      *
      *    公費単独の場合の編集
           IF    RECE10-HKNNUM  =  SPACE
             MOVE    RECE10-JNISSU(2)    TO    WRK-NISSU
             MOVE    RECE10-TOTALTEN(2)  TO    WRK-TENSU
             COMPUTE  WRK-HKNGK  = (RECE10-TOTALTEN(2) * 10)
                                 + RECE10-SHOKUJIRYOYOHI(2)
             ADD     WRK-HKNGK           TO    WRK-KOHGK
             MOVE    ZERO                TO    WRK-HKNGK
           END-IF
      *
      *    日数
           MOVE  RECE10-SHOKUJINISSU(1)     TO    WRK-SNISSU
      *    請求額
           MOVE  RECE10-SHOKUJIRYOYOHI(1)   TO    WRK-SSEIKYU
      *    標準負担額
           MOVE  RECE10-SHOKUJIFTN(1)       TO    WRK-SFTNGK
      *
      *    医療収益 食事収益加算
           COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
                                +  WRK-KOHGK  +  WRK-JIHIGK
                                +  (WRK-SSEIKYU - WRK-SFTNGK)
      *
           .
       2001-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
      *    国保データ印刷処理
           PERFORM 2002-PRINT-TYPE2-SEC
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保データ印刷処理
      *****************************************************************
       2002-PRINT-TYPE2-SEC                SECTION.
      *
           OPEN     INPUT        S138TMP-FILE
           MOVE     ZERO    TO   FLG-PRINT
           INITIALIZE            S138TMP-REC
      *
           MOVE    2        TO   FLG-TYPE
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL  FLG-PRINT   =    1
      *
      *    国保保険者並び替え処理
             IF   S138TMP-HKNJANUM(1)  NOT =  SPACE
               MOVE    1        TO   IDX-S
               MOVE    2        TO   IDX-N
               PERFORM  UNTIL  IDX-S  NOT <  300
                        OR  S138TMP-HKNJANUM(IDX-S)  =  SPACE
                 PERFORM  UNTIL  IDX-N  >  300
                          OR  S138TMP-HKNJANUM(IDX-N)  =  SPACE
                   IF  S138TMP-HKNJANUM(IDX-S) > S138TMP-HKNJANUM(IDX-N)
                     MOVE   S138TMP-HKNJA1-TBL(IDX-N)  TO  WRK-HKNJA-TBL
                     MOVE   S138TMP-HKNJA1-TBL(IDX-S)
                                           TO  S138TMP-HKNJA1-TBL(IDX-N)
                     MOVE   WRK-HKNJA-TBL  TO  S138TMP-HKNJA1-TBL(IDX-S)
                   END-IF
                   ADD    1      TO   IDX-N
                 END-PERFORM
      *    N番目の値を決定する為にインデックスの再設定
                 ADD    1        TO   IDX-S
                 COMPUTE  IDX-N  =  IDX-S  +  1
               END-PERFORM
             END-IF
      *    国保組合保険者並び替え処理
             IF   S138TMP-K-HKNJANUM(1)  NOT =  SPACE
               MOVE    1        TO   IDX-S
               MOVE    2        TO   IDX-N
               PERFORM  UNTIL  IDX-S  NOT <  300
                        OR  S138TMP-K-HKNJANUM(IDX-S)  =  SPACE
                 PERFORM  UNTIL  IDX-N  >  300
                          OR  S138TMP-K-HKNJANUM(IDX-N)  =  SPACE
                   IF   S138TMP-K-HKNJANUM(IDX-S)
                                     >  S138TMP-K-HKNJANUM(IDX-N)
                     MOVE   S138TMP-HKNJA2-TBL(IDX-N)  TO  WRK-HKNJA-TBL
                     MOVE   S138TMP-HKNJA2-TBL(IDX-S)
                                           TO  S138TMP-HKNJA2-TBL(IDX-N)
                     MOVE   WRK-HKNJA-TBL  TO  S138TMP-HKNJA2-TBL(IDX-S)
                   END-IF
                   ADD    1      TO   IDX-N
                 END-PERFORM
      *    N番目の値を決定する為にインデックスの再設定
                 ADD    1        TO   IDX-S
                 COMPUTE  IDX-N  =  IDX-S  +  1
               END-PERFORM
             END-IF
             IF    WRK-PARA-NYUGAIKBN   =  "1"  OR  SPACE
               IF    S138TMP-NYUGAIKBN   =   "1"
      *    入院フォーム
      *    １枚目
      *    見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
                 PERFORM  UNTIL  FLG-PASS   =   1
                          AND    S138TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
                          AND    S138TMP-KOH2-KEY(WRK-CNT2)  =  SPACE
      *
                   PERFORM  3104-HEAD-HEN-SEC
      *
      *    明細編集
                   PERFORM  3205-BODY-HEN-SEC
      *
      *    印刷処理
                   PERFORM  350-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
      *
      *    ２枚目（国保分）
      *    見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
                 PERFORM  UNTIL  FLG-PASS   =   1
                        AND (   S138TMP-HKNJANUM(WRK-CNT3)  =  '000000'
                           OR    S138TMP-HKNJANUM(WRK-CNT3)  =  SPACE )
                   MOVE   "（市町村）"  TO      WRK-CITYNAME
                   PERFORM  3105-HEAD-HEN-SEC
      *
      *    明細編集
                   PERFORM  3206-BODY-HEN-SEC
      *
      *    印刷処理
                   PERFORM  360-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
      *
      *          ３枚目（国保組合分）
      *          見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
                 PERFORM  UNTIL  FLG-PASS   =   1
                          AND    S138TMP-K-HKNJANUM(WRK-CNT4)  =  SPACE
                   MOVE   "（国保組合）"  TO      WRK-CITYNAME
                   PERFORM  3105-HEAD-HEN-SEC
      *
      *            明細編集
                   PERFORM  3207-BODY-HEN-SEC
      *
      *            印刷処理
                   PERFORM  360-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
               END-IF
             END-IF
      *
             IF    WRK-PARA-NYUGAIKBN     =  "2"  OR  SPACE
               IF    S138TMP-NYUGAIKBN      =    "2"
      *
      *    外来フォーム
      *    １枚目
      *    見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
                 PERFORM  UNTIL  FLG-PASS   =   1
                          AND    S138TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
                          AND    S138TMP-KOH2-KEY(WRK-CNT2)  =  SPACE
      *
                   PERFORM  3102-HEAD-HEN-SEC
      *
      *    明細編集
                   PERFORM  3202-BODY-HEN-SEC
      *
      *    印刷処理
                   PERFORM  330-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
      *
      *    ２枚目（国保分）
      *    見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
                 PERFORM  UNTIL  FLG-PASS   =   1
                        AND (   S138TMP-HKNJANUM(WRK-CNT3)  =  '000000'
                           OR    S138TMP-HKNJANUM(WRK-CNT3)  =  SPACE )
                   MOVE   "（市町村）"  TO      WRK-CITYNAME
                   PERFORM  3103-HEAD-HEN-SEC
      *
      *    明細編集
                   PERFORM  3203-BODY-HEN-SEC
      *
      *    印刷処理
                   PERFORM  340-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
      *
      *    ３枚目（国保組合分）
      *    見出し編集
                 MOVE     1       TO   WRK-CNT1
                                       WRK-CNT2
                                       WRK-CNT3
                                       WRK-CNT4
                 MOVE     ZERO    TO   FLG-PASS
                 PERFORM  UNTIL  FLG-PASS   =   1
                          AND    S138TMP-K-HKNJANUM(WRK-CNT4)  =  SPACE
                   MOVE   "（国保組合）"  TO      WRK-CITYNAME
                   PERFORM  3103-HEAD-HEN-SEC
      *
      *    明細編集
                   PERFORM  3204-BODY-HEN-SEC
      *
      *    印刷処理
                   PERFORM  340-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
               END-IF
             END-IF
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    S138TMP-FILE
      *
           .
       2002-PRINT-TYPE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<ヘッダー部>処理
      *****************************************************************
       3102-HEAD-HEN-SEC                 SECTION.
      *
           INITIALIZE                      S138G1
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S138G1-PAGE
             MOVE     1               TO    IDX4
             MOVE     SUM-SRYKAMAX    TO    IDX-MAX
             PERFORM  UNTIL    IDX4   >     IDX-MAX
               IF  S138TMP-SRYKA       =     SUM-SRYKA(IDX4)
                 MOVE    ZERO                   TO    IDX-MAX
                 MOVE    SUM-SRYKANAME(IDX4)    TO    S138G1-SRYKANAME
               ELSE
                 ADD     1                      TO    IDX4
               END-IF
             END-PERFORM
      *
           IF    S138TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   S138G1-NYUGAI
           ELSE
             MOVE     "外来"        TO   S138G1-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S138G1-SRYYM
           MOVE    WRK-SEIYMD       TO   S138G1-SYSYMD
      *
           .
       3102-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<ヘッダー部>処理
      *****************************************************************
       3103-HEAD-HEN-SEC                 SECTION.
      *
           INITIALIZE                      S138G2
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S138G2-PAGE
           MOVE     1               TO    IDX4
           MOVE     SUM-SRYKAMAX    TO    IDX-MAX
           PERFORM  UNTIL    IDX4   >     IDX-MAX
             IF  S138TMP-SRYKA       =     SUM-SRYKA(IDX4)
               MOVE    ZERO                   TO    IDX-MAX
               MOVE    SUM-SRYKANAME(IDX4)    TO    S138G2-SRYKANAME
             ELSE
               ADD     1                      TO    IDX4
             END-IF
           END-PERFORM
      *
           IF    S138TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   S138G2-NYUGAI
           ELSE
             MOVE     "外来"        TO   S138G2-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S138G2-SRYYM
           MOVE    WRK-SEIYMD       TO   S138G2-SYSYMD
      *
           MOVE    WRK-CITYNAME     TO   S138G2-CITYKBN
      *
           .
       3103-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<ヘッダー部>処理
      *****************************************************************
       3104-HEAD-HEN-SEC                 SECTION.
      *
           INITIALIZE                      S138N1
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S138N1-PAGE
           MOVE     1               TO    IDX4
           MOVE     SUM-SRYKAMAX    TO    IDX-MAX
           PERFORM  UNTIL    IDX4   >     IDX-MAX
             IF  S138TMP-SRYKA       =     SUM-SRYKA(IDX4)
               MOVE    ZERO                   TO    IDX-MAX
               MOVE    SUM-SRYKANAME(IDX4)    TO    S138N1-SRYKANAME
             ELSE
               ADD     1                      TO    IDX4
             END-IF
           END-PERFORM
      *
           IF    S138TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   S138N1-NYUGAI
           ELSE
             MOVE     "外来"        TO   S138N1-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S138N1-SRYYM
           MOVE    WRK-SEIYMD       TO   S138N1-SYSYMD
      *
           .
       3104-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<ヘッダー部>処理
      *****************************************************************
       3105-HEAD-HEN-SEC                 SECTION.
      *
           INITIALIZE                      S138N2
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S138N2-PAGE
           MOVE     1               TO    IDX4
           MOVE     SUM-SRYKAMAX    TO    IDX-MAX
           PERFORM  UNTIL    IDX4   >     IDX-MAX
             IF  S138TMP-SRYKA       =     SUM-SRYKA(IDX4)
               MOVE    ZERO                   TO    IDX-MAX
               MOVE    SUM-SRYKANAME(IDX4)    TO    S138N2-SRYKANAME
             ELSE
               ADD     1                      TO    IDX4
             END-IF
           END-PERFORM
      *
           IF    S138TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   S138N2-NYUGAI
           ELSE
             MOVE     "外来"        TO   S138N2-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S138N2-SRYYM
           MOVE    WRK-SEIYMD       TO   S138N2-SYSYMD
      *
           MOVE    WRK-CITYNAME     TO   S138N2-CITYKBN
      *
           .
       3105-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(外来：１枚目)
      *****************************************************************
       3202-BODY-HEN-SEC               SECTION.
      *
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     14
                      OR       FLG-PASS  = 1
      *    国保分
      *    件数
             MOVE     S138TMP-KENSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G1-KENSU(IDX1)
      *
      *    日数
             MOVE     S138TMP-NISSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G1-NISSU(IDX1)
      *
      *    点数
             MOVE     S138TMP-TENSU(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G1-TENSU(IDX1)
      *
      *    窓口請求額
             MOVE     S138TMP-MADOGK(IDX1)   TO    WRK-Z93
             MOVE     WRK-Z93               TO    S138G1-MADOGK(IDX1)
      *
      *    自費
             MOVE     S138TMP-JIHIGK(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G1-JIHIGK(IDX1)
      *
      *    保険請求額
             MOVE     S138TMP-HKNGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G1-HKNGK(IDX1)
      *
      *    公費請求額
             MOVE     S138TMP-KOHGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G1-KOHGK(IDX1)
      *
      *    医療収益
             MOVE     S138TMP-SYUEKI(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G1-SYUEKI(IDX1)
      *
      *    国保組合分
      *    件数
             IF    IDX1    <=    11
               MOVE     S138TMP-K-KENSU(IDX1)    TO    WRK-Z4
               MOVE     WRK-Z4                TO    S138G1-K-KENSU(IDX1)
      *
      *    日数
               MOVE     S138TMP-K-NISSU(IDX1)    TO    WRK-Z4
               MOVE     WRK-Z4                TO    S138G1-K-NISSU(IDX1)
      *
      *    点数
               MOVE     S138TMP-K-TENSU(IDX1)    TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138G1-K-TENSU(IDX1)
      *
      *    窓口請求額
               MOVE     S138TMP-K-MADOGK(IDX1)   TO    WRK-Z93
               MOVE     WRK-Z93              TO    S138G1-K-MADOGK(IDX1)
      *
      *    自費
               MOVE     S138TMP-K-JIHIGK(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92              TO    S138G1-K-JIHIGK(IDX1)
      *
      *    保険請求額
               MOVE     S138TMP-K-HKNGK(IDX1)    TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138G1-K-HKNGK(IDX1)
      *
      *    公費請求額
               MOVE     S138TMP-K-KOHGK(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138G1-K-KOHGK(IDX1)
      *
      *    医療収益
               MOVE     S138TMP-K-SYUEKI(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92              TO    S138G1-K-SYUEKI(IDX1)
             END-IF
           END-PERFORM
      *
      *    合計
           IF    FLG-PASS  =  ZERO
      *
      *    件数
             MOVE     S138TMP-GKENSU          TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-GKENSU
      *
      *    日数
             MOVE     S138TMP-GNISSU          TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-GNISSU
      *
      *    点数
             MOVE     S138TMP-GTENSU          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-GTENSU
      *
      *    窓口請求額
             MOVE     S138TMP-GMADOGK         TO    WRK-Z93
             MOVE     WRK-Z93                TO    S138G1-GMADOGK
      *
      *    自費
             MOVE     S138TMP-GJIHIGK         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-GJIHIGK
      *
      *    保険請求額
             MOVE     S138TMP-GHKNGK          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-GHKNGK
      *
      *    公費請求額
             MOVE     S138TMP-GKOHGK          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-GKOHGK
      *
      *    医療収益
             MOVE     S138TMP-GSYUEKI         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-GSYUEKI
           END-IF
      *
      *    国保・公費分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     20
                    OR       S138TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
                    OR       WRK-CNT1  >  300
      *
      *    公費名
             INITIALIZE                            WRK-KOHTMP-KEY
             MOVE     S138TMP-KOH1-KEY(WRK-CNT1)  TO WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE     WRK-KOHNAME-G  TO
                                                   S138G1-KOHNAME(IDX3)
      *    件数
             MOVE     S138TMP-KKENSU(WRK-CNT1)   TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-KKENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-KNISSU(WRK-CNT1)      TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-KNISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-KTENSU(WRK-CNT1)      TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-KTENSU(IDX3)
             ADD      1                      TO    WRK-CNT1
           END-PERFORM
      *
      *
      *    組合・公費分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     20
                    OR       S138TMP-KOH2-KEY(WRK-CNT2)  =  SPACE
                    OR       WRK-CNT2  >  300
      *
      *    公費名
             INITIALIZE                            WRK-KOHTMP-KEY
             MOVE     S138TMP-KOH2-KEY(WRK-CNT2)  TO WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE     WRK-KOHNAME-G  TO
                                                  S138G1-K-KOHNAME(IDX3)
      *    件数
             MOVE     S138TMP-K-KKENSU(WRK-CNT2)   TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-K-KKENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-K-KNISSU(WRK-CNT2)      TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138G1-K-KNISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-K-KTENSU(WRK-CNT2)      TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138G1-K-KTENSU(IDX3)
             ADD      1                      TO    WRK-CNT2
           END-PERFORM
      *
           .
       3202-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(外来：２枚目（国保分）)
      *****************************************************************
       3203-BODY-HEN-SEC               SECTION.
      *
      *
      *    国保・保険者分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     55
                    OR  S138TMP-HKNJANUM(WRK-CNT3)  =  SPACE
                    OR  WRK-CNT3  >  300
      *
      *    保険者名
             INITIALIZE                             WRK-HKNJANUM
             MOVE     S138TMP-HKNJANUM(WRK-CNT3)  TO WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME  TO
                                                  S138G2-HKNJANAME(IDX3)
      *    件数
             MOVE     S138TMP-HKENSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G2-KENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-HNISSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G2-NISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-HTENSU(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-TENSU(IDX3)
      *
      *    窓口請求額
             MOVE     S138TMP-HMADOGK(WRK-CNT3)   TO    WRK-Z93
             MOVE     WRK-Z93               TO    S138G2-MADOGK(IDX3)
      *
      *    自費
             MOVE     S138TMP-HJIHIGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-JIHIGK(IDX3)
      *
      *    保険請求額
             MOVE     S138TMP-HHKNGK(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-HKNGK(IDX3)
      *
      *    公費請求額
             MOVE     S138TMP-HKOHGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-KOHGK(IDX3)
      *
      *    医療収益
             MOVE     S138TMP-HSYUEKI(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-SYUEKI(IDX3)
             ADD      1                      TO    WRK-CNT3
           END-PERFORM
      *
           .
       3203-BODY-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(外来：３枚目（国保組合分）)
      *****************************************************************
       3204-BODY-HEN-SEC               SECTION.
      *
      *
      *    組合・保険者分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     55
                    OR       S138TMP-K-HKNJANUM(WRK-CNT4)  =  SPACE
                    OR       WRK-CNT4  >  300
      *
      *    保険者名
             INITIALIZE                             WRK-HKNJANUM
             MOVE     S138TMP-K-HKNJANUM(WRK-CNT4)  TO WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME  TO
                                               S138G2-HKNJANAME(IDX3)
      *    件数
             MOVE     S138TMP-K-HKENSU(WRK-CNT4)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G2-KENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-K-HNISSU(WRK-CNT4)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138G2-NISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-K-HTENSU(WRK-CNT4)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-TENSU(IDX3)
      *
      *    窓口請求額
             MOVE     S138TMP-K-HMADOGK(WRK-CNT4)   TO    WRK-Z93
             MOVE     WRK-Z93               TO   S138G2-MADOGK(IDX3)
      *
      *    自費
             MOVE     S138TMP-K-HJIHIGK(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO   S138G2-JIHIGK(IDX3)
      *
      *    保険請求額
             MOVE     S138TMP-K-HHKNGK(WRK-CNT4)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-HKNGK(IDX3)
      *
      *    公費請求額
             MOVE     S138TMP-K-HKOHGK(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-KOHGK(IDX3)
      *
      *    医療収益
             MOVE     S138TMP-K-HSYUEKI(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138G2-SYUEKI(IDX3)
             ADD      1                      TO    WRK-CNT4
           END-PERFORM
      *
           .
       3204-BODY-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(入院：１枚目)
      *****************************************************************
       3205-BODY-HEN-SEC               SECTION.
      *
      *
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     14
                      OR       FLG-PASS  = 1
      *    国保分
      *    件数
             MOVE     S138TMP-KENSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N1-KENSU(IDX1)
      *
      *    日数
             MOVE     S138TMP-NISSU(IDX1)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N1-NISSU(IDX1)
      *
      *    点数
             MOVE     S138TMP-TENSU(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-TENSU(IDX1)
      *
      *    窓口請求額
             MOVE     S138TMP-MADOGK(IDX1)   TO    WRK-Z93
             MOVE     WRK-Z93               TO    S138N1-MADOGK(IDX1)
      *
      *    自費
             MOVE     S138TMP-JIHIGK(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-JIHIGK(IDX1)
      *
      *    保険請求額
             MOVE     S138TMP-HKNGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-HKNGK(IDX1)
      *
      *    公費請求額
             MOVE     S138TMP-KOHGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-KOHGK(IDX1)
      *
      *    医療収益
             MOVE     S138TMP-SYUEKI(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-SYUEKI(IDX1)
      *
      *    食事療養
      *    日数
             MOVE     S138TMP-SNISSU(IDX1)    TO    WRK-Z52
             MOVE     WRK-Z52                TO    S138N1-SNISSU(IDX1)
      *
      *    請求額
             MOVE     S138TMP-SSEIKYU(IDX1)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-SSEIKYU(IDX1)
      *
      *    標準負担額
             MOVE     S138TMP-SFTNGK(IDX1)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-SFTNGK(IDX1)
           END-PERFORM
      *
      *    国保・公費分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     20
                    OR       S138TMP-KOH1-KEY(WRK-CNT1)  =  SPACE
                    OR       WRK-CNT1  >  300
      *
      *    公費名
             INITIALIZE                            WRK-KOHTMP-KEY
             MOVE     S138TMP-KOH1-KEY(WRK-CNT1)  TO WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE     WRK-KOHNAME-G  TO
                                                   S138N1-KOHNAME(IDX3)
      *    件数
             MOVE     S138TMP-KKENSU(WRK-CNT1)   TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-KKENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-KNISSU(WRK-CNT1)      TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-KNISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-KTENSU(WRK-CNT1)      TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-KTENSU(IDX3)
             ADD      1                      TO    WRK-CNT1
           END-PERFORM
      *
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     14
                      OR       FLG-PASS  = 1
      *
      *    国保組合分
      *    件数
             IF    IDX1    <=    11
               MOVE     S138TMP-K-KENSU(IDX1)    TO    WRK-Z4
               MOVE     WRK-Z4                TO    S138N1-K-KENSU(IDX1)
      *
      *    日数
               MOVE     S138TMP-K-NISSU(IDX1)    TO    WRK-Z4
               MOVE     WRK-Z4                TO    S138N1-K-NISSU(IDX1)
      *
      *    点数
               MOVE     S138TMP-K-TENSU(IDX1)    TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138N1-K-TENSU(IDX1)
      *
      *    窓口請求額
               MOVE     S138TMP-K-MADOGK(IDX1)   TO    WRK-Z93
               MOVE     WRK-Z93               TO   S138N1-K-MADOGK(IDX1)
      *
      *    自費計
               MOVE     S138TMP-K-JIHIGK(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92               TO   S138N1-K-JIHIGK(IDX1)
      *
      *    保険請求額
               MOVE     S138TMP-K-HKNGK(IDX1)    TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138N1-K-HKNGK(IDX1)
      *
      *    公費請求額
               MOVE     S138TMP-K-KOHGK(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92               TO    S138N1-K-KOHGK(IDX1)
      *
      *    医療収益
               MOVE     S138TMP-K-SYUEKI(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92               TO   S138N1-K-SYUEKI(IDX1)
      *
      *    食事療養
      *    日数
               MOVE     S138TMP-K-SNISSU(IDX1)    TO    WRK-Z52
               MOVE     WRK-Z52               TO   S138N1-K-SNISSU(IDX1)
      *
      *    請求額
               MOVE     S138TMP-K-SSEIKYU(IDX1)   TO    WRK-Z92
               MOVE     WRK-Z92               TO  S138N1-K-SSEIKYU(IDX1)
      *
      *    標準負担額
               MOVE     S138TMP-K-SFTNGK(IDX1)    TO    WRK-Z92
               MOVE     WRK-Z92               TO   S138N1-K-SFTNGK(IDX1)
             END-IF
           END-PERFORM
      *
      *    組合・公費分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     20
                    OR       S138TMP-KOH2-KEY(WRK-CNT2)  =  SPACE
                    OR       WRK-CNT2  >  300
      *
      *    公費名
             INITIALIZE                            WRK-KOHTMP-KEY
             MOVE     S138TMP-KOH2-KEY(WRK-CNT2)  TO WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE     WRK-KOHNAME-G  TO
                                                  S138N1-K-KOHNAME(IDX3)
      *    件数
             MOVE     S138TMP-K-KKENSU(WRK-CNT2)   TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-K-KKENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-K-KNISSU(WRK-CNT2)      TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-K-KNISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-K-KTENSU(WRK-CNT2)      TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-K-KTENSU(IDX3)
             ADD      1                      TO    WRK-CNT2
           END-PERFORM
      *
      *    合計
           IF    FLG-PASS  =  ZERO
      *
      *    件数
             MOVE     S138TMP-GKENSU          TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-GKENSU
      *
      *    日数
             MOVE     S138TMP-GNISSU          TO    WRK-Z4
             MOVE     WRK-Z4                 TO    S138N1-GNISSU
      *
      *    点数
             MOVE     S138TMP-GTENSU          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-GTENSU
      *
      *    窓口請求額
             MOVE     S138TMP-GMADOGK         TO    WRK-Z93
             MOVE     WRK-Z93                TO    S138N1-GMADOGK
      *
      *    自費
             MOVE     S138TMP-GJIHIGK         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-GJIHIGK
      *
      *    保険請求額
             MOVE     S138TMP-GHKNGK          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-GHKNGK
      *
      *    公費請求額
             MOVE     S138TMP-GKOHGK          TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-GKOHGK
      *
      *    医療収益
             MOVE     S138TMP-GSYUEKI         TO    WRK-Z92
             MOVE     WRK-Z92                TO    S138N1-GSYUEKI
      *
      *    食事療養
      *    日数
             MOVE     S138TMP-SGNISSU    TO    WRK-Z52
             MOVE     WRK-Z52                TO    S138N1-SGNISSU
      *
      *    請求額
             MOVE     S138TMP-SGSEIKYU   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-SGSEIKYU
      *
      *    標準負担額
             MOVE     S138TMP-SGFTNGK    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N1-SGFTNGK
           END-IF
      *
           .
       3205-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(入院：２枚目（国保分）)
      *****************************************************************
       3206-BODY-HEN-SEC               SECTION.
      *
      *    国保・保険者分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     55
                    OR       S138TMP-HKNJANUM(WRK-CNT3)  =  SPACE
                    OR       WRK-CNT3  >  300
      *
      *    保険者名
             INITIALIZE                             WRK-HKNJANUM
             MOVE     S138TMP-HKNJANUM(WRK-CNT3)  TO WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME  TO
                                                  S138N2-HKNJANAME(IDX3)
      *    件数
             MOVE     S138TMP-HKENSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N2-KENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-HNISSU(WRK-CNT3)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N2-NISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-HTENSU(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-TENSU(IDX3)
      *
      *    窓口請求額
             MOVE     S138TMP-HMADOGK(WRK-CNT3)   TO    WRK-Z93
             MOVE     WRK-Z93               TO    S138N2-MADOGK(IDX3)
      *
      *    自費
             MOVE     S138TMP-HJIHIGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-JIHIGK(IDX3)
      *
      *    保険請求額
             MOVE     S138TMP-HHKNGK(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-HKNGK(IDX3)
      *
      *    公費請求額
             MOVE     S138TMP-HKOHGK(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-KOHGK(IDX3)
      *
      *    医療収益
             MOVE     S138TMP-HSYUEKI(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-SYUEKI(IDX3)
      *
      *    食事療養
      *    日数
             MOVE     S138TMP-HSNISSU(WRK-CNT3)    TO    WRK-Z52
             MOVE     WRK-Z52                TO    S138N2-SNISSU(IDX3)
      *
      *    請求額
             MOVE     S138TMP-HSSEIKYU(WRK-CNT3)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-SSEIKYU(IDX3)
      *
      *    標準負担額
             MOVE     S138TMP-HSFTNGK(WRK-CNT3)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-SFTNGK(IDX3)
             ADD      1                      TO    WRK-CNT3
           END-PERFORM
      *
           .
       3206-BODY-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    国保帳票編集<明細>処理(入院：３枚目（国保組合分）)
      *****************************************************************
       3207-BODY-HEN-SEC               SECTION.
      *
      *
      *    組合・保険者分
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     55
                    OR       S138TMP-K-HKNJANUM(WRK-CNT4)  =  SPACE
                    OR       WRK-CNT4  >  300
      *
      *    保険者名
             INITIALIZE                             WRK-HKNJANUM
             MOVE     S138TMP-K-HKNJANUM(WRK-CNT4)  TO WRK-HKNJANUM
             PERFORM  920-HKNJAINF-SET-SEC
             MOVE     WRK-HKNJANAME  TO
                                               S138N2-HKNJANAME(IDX3)
      *    件数
             MOVE     S138TMP-K-HKENSU(WRK-CNT4)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N2-KENSU(IDX3)
      *
      *    日数
             MOVE     S138TMP-K-HNISSU(WRK-CNT4)    TO    WRK-Z4
             MOVE     WRK-Z4                TO    S138N2-NISSU(IDX3)
      *
      *    点数
             MOVE     S138TMP-K-HTENSU(WRK-CNT4)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-TENSU(IDX3)
      *
      *    窓口請求額
             MOVE     S138TMP-K-HMADOGK(WRK-CNT4)   TO    WRK-Z93
             MOVE     WRK-Z93               TO   S138N2-MADOGK(IDX3)
      *
      *    自費
             MOVE     S138TMP-K-HJIHIGK(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO   S138N2-JIHIGK(IDX3)
      *
      *    保険請求額
             MOVE     S138TMP-K-HHKNGK(WRK-CNT4)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-HKNGK(IDX3)
      *
      *    公費請求額
             MOVE     S138TMP-K-HKOHGK(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-KOHGK(IDX3)
      *
      *    医療収益
             MOVE     S138TMP-K-HSYUEKI(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-SYUEKI(IDX3)
      *
      *    食事療養
      *    日数
             MOVE     S138TMP-K-HSNISSU(WRK-CNT4)    TO    WRK-Z52
             MOVE     WRK-Z52                TO    S138N2-SNISSU(IDX3)
      *
      *    請求額
             MOVE     S138TMP-K-HSSEIKYU(WRK-CNT4)   TO    WRK-Z92
             MOVE     WRK-Z92               TO   S138N2-SSEIKYU(IDX3)
      *
      *    標準負担額
             MOVE     S138TMP-K-HSFTNGK(WRK-CNT4)    TO    WRK-Z92
             MOVE     WRK-Z92               TO    S138N2-SFTNGK(IDX3)
             ADD      1                      TO    WRK-CNT4
           END-PERFORM
      *
           .
       3207-BODY-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       330-PRINT-OUT-SEC         SECTION.
      *
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           IF  S138TMP-SRYYMKBN  =  "0"
             MOVE    "A00000S138G1O.red"        TO  SPRT-PRTID
           ELSE
             MOVE    "A00000S138G1N.red"        TO  SPRT-PRTID
           END-IF
           MOVE    S100LNK-SPRT-TITLE  TO  SPRT-TITLE
           MOVE    S138G1                TO  SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF  SPRT-RETURN  =   ZERO
             CONTINUE               ELSE
             MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       340-PRINT-OUT-SEC         SECTION.
      *
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "A00000S138G2.red"         TO  SPRT-PRTID
           MOVE    S100LNK-SPRT-TITLE  TO  SPRT-TITLE
           MOVE    S138G2                TO  SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF  SPRT-RETURN         =   ZERO
             CONTINUE               ELSE
             MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       350-PRINT-OUT-SEC         SECTION.
      *
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           IF  S138TMP-SRYYMKBN  =  "0"
               MOVE    "A00000S138N1O.red"        TO  SPRT-PRTID
           ELSE
               MOVE    "A00000S138N1N.red"        TO  SPRT-PRTID
           END-IF
           MOVE    S100LNK-SPRT-TITLE  TO  SPRT-TITLE
           MOVE    S138N1                  TO  SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF  SPRT-RETURN   =   ZERO
             CONTINUE               ELSE
             MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       350-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       360-PRINT-OUT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "A00000S138N2.red"         TO  SPRT-PRTID
           MOVE    S100LNK-SPRT-TITLE  TO  SPRT-TITLE
           MOVE    S138N2                TO  SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF  SPRT-RETURN         =   ZERO
             CONTINUE               ELSE
             MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       360-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    公費名編集処理
      *****************************************************************
       333-KOHNAME-HEN-SEC               SECTION.
      *
           MOVE    SPACE       TO    WRK-KOHNAME-G
                                     WRK-KOHNAME-G2
      *
           IF    WRK-KOHTMP-RJNKBN  NOT = SPACE
             MOVE  "２７＋"    TO    WRK-KOHNAME-G
           END-IF
      *
           MOVE    WRK-KOHTMP-KOHNUM    TO  WRK-KOHNUM
           PERFORM  950-HKNNUM-SET-SEC
           IF    WRK-KOHNUM    =    "051"
             MOVE    "特定疾患"    TO    HKN-TANSEIDONAME
           END-IF
           STRING  WRK-KOHNAME-G      DELIMITED BY SPACE
                   HKN-HBTNUM         DELIMITED BY SPACE
                   "("                DELIMITED BY SIZE
                   HKN-TANSEIDONAME   DELIMITED BY SPACE
                   ")"                DELIMITED BY SIZE
                   INTO               WRK-KOHNAME-G2
           END-STRING
      *
           MOVE    WRK-KOHNAME-G2        TO    WRK-KOHNAME-G
      *
           MOVE   WRK-KOHNAME-G          TO    WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE   WRK-OUT-INPUT          TO    WRK-KOHNAME-G
      *
           .
       333-KOHNAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
             ELSE
               MOVE    1                   TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    SYS-1005-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1           TO    FLG-SYSKANRI3
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
           MOVE    "DBFETCH"                  TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC           =   ZERO
             MOVE    ZERO                   TO  FLG-SYSKANRI3
             MOVE    MCPDATA-REC  TO  SYS-1005-REC
           ELSE
             MOVE    1                           TO  FLG-SYSKANRI3
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
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "  "
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                  LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
             MOVE    KANACHK-OUT-INPUT  TO  WRK-OUT-INPUT
           ELSE
             MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT              RECEERR-FILE
      *
             MOVE    WRK-RECEERR         TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ終了処理
             MOVE    "JBE"             TO  SJOBKANRI-MODE
             INITIALIZE                  JOBKANRI-REC
             MOVE    SPA-HOSPNUM       TO  JOB-HOSPNUM
             MOVE    S100LNK-JOBID     TO  JOB-JOBID
             MOVE    S100LNK-SHELLID   TO  JOB-SHELLID
             MOVE    WRK-RECEERR       TO  JOB-YOBI
             MOVE    "9999"            TO  JOB-ERRCD
             CALL    "ORCSJOB"       USING
                                     ORCSJOBKANRIAREA
                                     JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    S138TMP-FILE
             INVALID    KEY
               MOVE    1           TO  FLG-S138TMP
             NOT  INVALID  KEY
               MOVE    ZERO        TO  FLG-S138TMP
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************^M
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
             ELSE
               MOVE    1               TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
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
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  S138TMP-FILE   NEXT
             AT  END
               MOVE  1  TO  FLG-PRINT
             NOT  AT  END
               ADD   1   TO    CNT-READ
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
           MOVE   WRK-PARA-HOSPNUM      TO    RECE10-HOSPNUM.
           MOVE   "0"                   TO    RECE10-XXSTHKNJANUM
           MOVE   "9999999"             TO    RECE10-XXEDHKNJANUM
           MOVE   WRK-PARA-SRYYM        TO    RECE10-SKYYM.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC   =   ZERO
             PERFORM 900-SEIKYU-READ-SEC
           ELSE
             MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC  =   ZERO
             MOVE    ZERO            TO  FLG-END
             MOVE    MCPDATA-REC         TO  RECE10-REC

      *    テスト患者のときは読み飛ばす
             MOVE    SPACE               TO  PTINF-REC
             INITIALIZE                      PTINF-REC
             MOVE    RECE10-HOSPNUM       TO  WRK-PARA-HOSPNUM
             MOVE    RECE10-PTID         TO  WRK-PTID
             PERFORM 900-PTINF-READ-SEC
             IF      FLG-PTINF           =   ZERO
             AND     PTINF-TSTPTNUMKBN   =   "1"
                 GO  TO  900-SEIKYU-READ-SEC
             END-IF
      *    ０９１と０５１を同じ公費として扱う
             PERFORM  VARYING  IDX5  FROM  1  BY  1
                      UNTIL    IDX5  >     4  OR
                      RECE10-KOHNUM(IDX5)  =  SPACE
               IF    RECE10-KOHNUM(IDX5)  =  "091"
                 MOVE    "051"      TO      RECE10-KOHNUM(IDX5)
               END-IF
             END-PERFORM
           ELSE
             MOVE    1               TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求管理情報クローズ処理
      *****************************************************************
       900-SEIKYU-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_seikyu"       TO  MCP-TABLE
           MOVE    "key4"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC              =   ZERO
               MOVE    MCPDATA-REC      TO  PTINF-REC
               MOVE    ZERO             TO  FLG-PTINF
             ELSE
               MOVE    1                TO  FLG-PTINF
               INITIALIZE               PTINF-REC
             END-IF
           ELSE
             MOVE    1                   TO  FLG-PTINF
             INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスター読込処理
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    SYU-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SYU-PTID            TO  PTHKN-PTID
      *
           MOVE    SYU-SYUHKNNUM       TO  PTHKN-HKNNUM
           MOVE    SYU-SRYYMD          TO  PTHKN-TEKSTYMD
           MOVE    SYU-SRYYMD          TO  PTHKN-TEKEDYMD
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF    MCP-RC     =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_pthkninf"    TO  MCP-TABLE
             MOVE    "key9"            TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF   MCP-RC            =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
             ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTHKNINF
             INITIALIZE                PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴情報検索
      *****************************************************************
       920-DBSELECT3-SEC                 SECTION.
      *
           INITIALIZE                        JYURRK-REC
           MOVE    WRK-PARA-HOSPNUM     TO   JYURRK-HOSPNUM
           MOVE    WRK-NYUGAIKBN        TO   JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD     TO   JYURRK-SRYYMD
           MOVE    WRK-PARA-PRTLYMD     TO   JYURRK-UPSRYYMD
           MOVE    JYURRK-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_jyurrk"         TO   MCP-TABLE
           MOVE    "key21"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM 920-DBFETCH3-SEC
           ELSE
             MOVE    1           TO    FLG-JYURRK
           END-IF
           .
       920-DBSELECT3-EXT.
           EXIT.
      *
      *****************************************************************
      *     受診履歴情報フェッチ
      *****************************************************************
       920-DBFETCH3-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key21"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF    MCP-RC   =   ZERO
             MOVE    ZERO            TO   FLG-JYURRK
             MOVE    MCPDATA-REC  TO  JYURRK-REC
      *
      *    テスト患者は対象外
             MOVE    JYURRK-PTID     TO   WRK-PTID
             PERFORM  900-PTINF-READ-SEC
             IF   FLG-PTINF   =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  920-DBFETCH3-SEC
             END-IF
      *
           ELSE
             MOVE    1            TO  FLG-JYURRK
           END-IF
           .
       920-DBFETCH3-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報クローズ処理
      *****************************************************************
       920-DBCLOSE3-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key21"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           .
       920-DBCLOSE3-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスター読込処理
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    SYU-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SYU-PTID            TO  PTHKN-PTID
           MOVE    SYU-SYUHKNNUM       TO  PTHKN-HKNNUM
           MOVE    SYU-SRYYMD          TO  PTHKN-TEKSTYMD
           MOVE    SYU-SRYYMD          TO  PTHKN-TEKEDYMD
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_pthkninf"    TO  MCP-TABLE
             MOVE    "key9"            TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF      MCP-RC    =   ZERO
               MOVE    MCPDATA-REC   TO  PTHKNINF-REC
               MOVE    ZERO          TO  FLG-PTHKNINF
             ELSE
               MOVE    1             TO  FLG-PTHKNINF
               INITIALIZE            PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1               TO  FLG-PTHKNINF
             INITIALIZE              PTHKNINF-REC
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  WRK-PARA-HOSPNUM     TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM         TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknjainf"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  925-HKNJAINF-READ-SEC
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE  "tbl_hknjainf"   TO  MCP-TABLE
           MOVE  "key"            TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"      USING
                                  MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
      *
           .
       920-HKNJAINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       925-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           MOVE  "tbl_hknjainf" TO  MCP-TABLE
           MOVE  "key"          TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"    USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
             MOVE  HKNJA-HKNJANAME   TO  WRK-HKNJANAME
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1              TO  FLG-HKNJAINF
             MOVE  WRK-HKNJANUM   TO  WRK-HKNJANAME
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO    TO    WRK-KOHSEIKYUGK
                                 WRK-KOHSKJSEIKYUGK
                                 WRK-ZENCOMPFTN
                                 WRK-FTNFLG
      *
           IF   RECE10-FTNMONEY(1)  =  ZERO
               AND   RECE10-KYURATE  NOT =  ZERO
             PERFORM  VARYING  IDX7 FROM  1  BY  1
                      UNTIL  IDX7  >  4
                      OR  SYU-KOH-HKNNUM(IDX7)  =  SPACE
               IF   FLG-TYOKI  =  1
                   AND  SYU-KOH-HKNNUM(IDX7)  =  "972"
                 COMPUTE  WRK-HKNSKYMONEY  =  (SYU-TOTAL-HKNTEN  *  10)
                                         -  SYU-KOH-COMPFTN-ENTANI(IDX7)
                 ADD    WRK-HKNSKYMONEY   TO   WRK-HKNGK
                 MOVE    SYU-KOH-COMPFTN-ENTANI(IDX7)
                                              TO   SYU-SYUCOMPFTN-ENTANI
                 MOVE    SYU-KOH-COMPFTN(IDX7)  TO   SYU-SYUCOMPFTN
               END-IF
               IF   FLG-TYOKI  =  2
                   AND  SYU-KOH-HKNNUM(IDX7)  =  "974"
                 COMPUTE  WRK-HKNSKYMONEY  =  (SYU-TOTAL-HKNTEN  *  10)
                                         -  SYU-KOH-COMPFTN-ENTANI(IDX7)
                 ADD    WRK-HKNSKYMONEY   TO   WRK-HKNGK
                 MOVE    SYU-KOH-COMPFTN-ENTANI(IDX7)
                                              TO   SYU-SYUCOMPFTN-ENTANI
                 MOVE    SYU-KOH-COMPFTN(IDX7)  TO   SYU-SYUCOMPFTN
               END-IF
             END-PERFORM
             IF   ( FLG-TYOKI  =  1  OR  2 )
                 AND  ( SYU-KOH-HKNNUM(1)  NOT =  "972" AND "974" )
                 AND  ( SYU-KOH-HKNNUM(2)  NOT =  "972" AND "974" )
                 AND  ( SYU-KOH-HKNNUM(3)  NOT =  "972" AND "974" )
                 AND  ( SYU-KOH-HKNNUM(4)  NOT =  "972" AND "974" )
                COMPUTE  WRK-HKNGK2  =  SYU-TOTAL-HKNTEN
                                     *  RECE10-KYURATE
                ADD    WRK-HKNGK2    TO   WRK-HKNGK
             END-IF
           END-IF
      *
           IF  RECE10-RJNHKNNUM NOT = SPACE
             MOVE   SYU-KOH-COMPFTN(1) TO  WRK-ZENCOMPFTN
           ELSE
             MOVE   SYU-SYUCOMPFTN  TO  WRK-ZENCOMPFTN
           END-IF
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    4 OR
                    SYU-KOH-HKNNUM(IDX)  =  SPACE
             IF    SYU-KOH-HKNNUM(IDX) NOT  =  "027"
                 AND  SYU-KOH-HKNNUM(IDX)  NOT =  "972"
                 AND  SYU-KOH-HKNNUM(IDX)  NOT =  "974"
      *    差額から助成額(公費請求額)を算出
               IF    RECE10-RJNHKNNUM  NOT = SPACE
      *
                 IF  SYU-KOH-COMPFTN(IDX) < WRK-ZENCOMPFTN
                   COMPUTE  WRK-KOHSEIKYUGK  =  SYU-KOH-COMPFTN(1)
                                             -  SYU-KOH-COMPFTN(IDX)
                   COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SKJ-FTN(1)
                                                -  SYU-SKJ-FTN(IDX)
                   MOVE   SYU-KOH-COMPFTN(IDX)  TO  WRK-ZENCOMPFTN
                 END-IF
               ELSE
      *
                   IF    RECE10-FTNMONEY(1)  =  ZERO
                       AND  RECE10-KYURATE  NOT = ZERO
                     MOVE  SYU-KOH-HKNNUM(IDX)   TO  WRK-KOHNUM
      *    限度なし
                     PERFORM   950-HKNNUM-SET-SEC
                       COMPUTE  WRK-KOHSEIKYUGK = SYU-SYUCOMPFTN-ENTANI
                                          -  SYU-KOH-COMPFTN-ENTANI(IDX)
                       COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
                                                    -  SYU-SKJ-FTN(IDX)
                   ELSE
      *    限度あり
                     MOVE    SYU-KOH-COMPFTN(IDX)  TO    WRK-KOHFTN
                     MOVE   1   TO    WRK-FTNFLG
                     COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
                                                 -  SYU-SKJ-FTN(IDX)
                   END-IF
                   MOVE   SYU-KOH-COMPFTN(IDX)  TO  WRK-ZENCOMPFTN
               END-IF
      *
             ELSE
               IF   RECE10-FLG(2)  =  1
                   AND  ( FLG-TYOKI  =  1  OR  2 )
                   AND  ( SYU-KOH-HKNNUM(2)  =  SPACE
                   AND  SYU-KOH-HKNNUM(3)  =  SPACE
                   AND  SYU-KOH-HKNNUM(4)  =  SPACE )
      *
                 COMPUTE  WRK-HKNGK       =  WRK-HKNGK
                                          +  SYU-SYUCOMPFTN-ENTANI
                                          -  SYU-SKYMONEY
               END-IF
             END-IF
           END-PERFORM
      *
           IF  WRK-FTNFLG  =  1
             COMPUTE  WRK-HKNFTN  =  WRK-HKNFTN  -  WRK-KOHFTN
             IF  WRK-HKNFTN  <=  10
               MOVE    ZERO      TO   WRK-HKNFTN
             END-IF
             INITIALIZE    WRK-KOHFTN
           ELSE
             ADD    WRK-KOHSEIKYUGK     TO   WRK-KOHSEIKYUGK-G
           END-IF
           COMPUTE  WRK-JIHIGK-G  =  WRK-JIHIGK-G
                                  +  SYU-JIHI-TOTAL
                                  +  SYU-JIHI-TOTAL-TAX
      *
           ADD    WRK-KOHSKJSEIKYUGK TO  WRK-KOHSEIKYUGK-G
           ADD    SYU-SKYMONEY     TO    WRK-MADOSEIKYUGK-G
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
           INITIALIZE  SYUNOU-REC
                       WRK-HKNFTN
           MOVE    RECE10-FTNMONEY(1)  TO   WRK-HKNFTN
           MOVE    RECE10-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    RECE10-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    RECE10-PTID        TO  SYU-PTID
           MOVE    SYUNOU-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "tbl_syunou"     TO  MCP-TABLE
           MOVE    "key2"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  925-SYUNOU-READ-SEC
      *
             PERFORM  UNTIL  FLG-SYUNOU  =  1
               IF       SYU-TOTAL-HKNTEN =  0
                   AND    SYU-TOTAL-MONEY             =  0
                   AND  ( SYU-JIHI-TOTAL         NOT  =  0
                   OR     SYU-JIHI-TOTAL-TAX     NOT  =  0
                   OR     SYU-TOTAL-TGMONEY      NOT  =  0
                   OR     SYU-TOTAL-TGMONEY-TAX  NOT  =  0 )
                 CONTINUE
               ELSE
                 PERFORM  920-PTHKNINF-READ-SEC
                 IF    RECE10-KOHNUM(1)  NOT =   SPACE
                     AND RECE10-HKNNUM   =   SPACE
                   MOVE    ZERO     TO   PTHKN-HKNJANUM
                 END-IF
                 IF  RECE10-SRYYM  =  SYU-SRYYMD(1:6)
                     AND  SYU-DENPJTIKBN   NOT = "3"
      *
                     AND  SYU-SYUHKNNUM  =  RECE10-HKNNUM
                     AND  PTHKN-HKNJANUM  =  RECE10-HKNJANUM
      *
      *    旧総合病院の時は該当する診療科のみ対象にする
                   IF    SYS-1001-HOSPSBT1 NOT =  1  OR
                         RECE10-NYUGAIKBN      = "1" OR
                         RECE10-RECEKA         = SYU-SRYKA
                     PERFORM  800-SEIKYU-SEC
                   END-IF
                 END-IF
               END-IF
      *
               PERFORM   925-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE  "tbl_syunou" TO  MCP-TABLE
           MOVE  "key2"       TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"  USING
                              MCPAREA
                              MCPDATA-REC
                              SPA-AREA
      *
           IF  WRK-FTNFLG  =  1
             ADD    WRK-HKNFTN     TO   WRK-KOHSEIKYUGK-G
           END-IF
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"     TO  MCP-FUNC
           MOVE    "tbl_syunou"  TO  MCP-TABLE
           MOVE    "key2"        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"   USING
                                 MCPAREA
                                 MCPDATA-REC
                                 SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SYUNOU-REC
             MOVE  ZERO         TO  FLG-SYUNOU
           ELSE
             INITIALIZE  SYUNOU-REC
             MOVE  1  TO  FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報READ自費・自賠・労災用(外来)
      *****************************************************************
       940-SYUNOU-READ-SEC         SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE  JYURRK-HOSPNUM       TO  SYU-HOSPNUM
           MOVE  JYURRK-NYUGAIKBN     TO  SYU-NYUGAIKBN
           MOVE  JYURRK-PTID          TO  SYU-PTID
           MOVE  JYURRK-DENPNUM       TO  SYU-DENPNUM
           MOVE  SYUNOU-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE  "tbl_syunou"         TO  MCP-TABLE
           MOVE  "key"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"  TO  MCP-FUNC
             MOVE  "tbl_syunou"         TO  MCP-TABLE
             MOVE  "key"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC  TO  SYUNOU-REC
               MOVE  ZERO         TO  FLG-SYUNOU
             ELSE
               INITIALIZE  SYUNOU-REC
               MOVE  1            TO  FLG-SYUNOU
             END-IF
      *
           ELSE
             MOVE    1            TO  FLG-SYUNOU
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE  "tbl_syunou"     TO  MCP-TABLE
           MOVE  "key"            TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"      USING
                                  MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
      *
           .
       940-SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納情報SELECT(入院データ用)
      *****************************************************************
       940-SYUNOU-SELECT-SEC         SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    JYURRK-HOSPNUM       TO  SYU-HOSPNUM
           MOVE    JYURRK-PTID          TO  SYU-PTID
           MOVE    JYURRK-NYUGAIKBN     TO  SYU-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD     TO  SYU-SKYSTYMD
           MOVE    WRK-PARA-PRTLYMD     TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM 940-SYUNOU-FETCH-SEC
           ELSE
             MOVE   1               TO  FLG-SYUNOU
           END-IF
           .
       940-SYUNOU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    収納情報１件読み込み
      *****************************************************************
       940-SYUNOU-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE    MCPDATA-REC     TO  SYUNOU-REC
             MOVE    ZERO            TO  FLG-SYUNOU
             IF    SYU-DENPJTIKBN    =   "3"
               GO    TO    940-SYUNOU-FETCH-SEC
             END-IF
      *
             IF    SYU-SYUHKNNUM(1:2) NOT  =  "97" AND "98"
               GO    TO    940-SYUNOU-FETCH-SEC
             END-IF
      *
           ELSE
             MOVE    1               TO  FLG-SYUNOU
           END-IF
           .
       940-SYUNOU-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    収納情報クローズ
      *****************************************************************
       940-SYUNOU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       940-SYUNOU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せ情報セット
      *****************************************************************
       930-HKNCOMBI-SET-SEC  SECTION.
      *
           INITIALIZE  HKNCOMBI-REC
           MOVE  SYU-HOSPNUM          TO  COMB-HOSPNUM
           MOVE  SYU-PTID             TO  COMB-PTID
           MOVE  SYU-HKNCOMBINUM      TO  COMB-HKNCOMBINUM
           MOVE  HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hkncombi"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"  TO  MCP-FUNC
             MOVE  "tbl_hkncombi" TO  MCP-TABLE
             MOVE  "key"          TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"  USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC  TO  HKNCOMBI-REC
               MOVE  ZERO         TO  FLG-HKNCOMBI
             ELSE
               INITIALIZE  HKNCOMBI-REC
               MOVE  1            TO  FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1            TO  FLG-HKNCOMBI
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "tbl_hkncombi"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       930-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE  WRK-PARA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE  WRK-KOHNUM           TO  HKN-HKNNUM
           MOVE  HKNNUM-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE  "tbl_hknnum"          TO  MCP-TABLE
           MOVE  "key2"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  955-HKNNUM-READ-SEC
           ELSE
             MOVE    1    TO    FLG-HKNNUM
           END-IF
      *
           PERFORM UNTIL   FLG-HKNNUM  =   1
             IF   HKN-TEKSTYMD   <=  WRK-SRYYMD
                 AND   HKN-TEKEDYMD   >=  WRK-SRYYMD
               MOVE  1                  TO  FLG-HKNNUM
             END-IF
      *
             IF      FLG-HKNNUM    =   ZERO
               MOVE  "tbl_hknnum"       TO  MCP-TABLE
               MOVE  "key2"             TO  MCP-PATHNAME
               PERFORM  955-HKNNUM-READ-SEC
             END-IF
           END-PERFORM
      *
           INITIALIZE             MCPAREA
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE  "tbl_hknnum"       TO  MCP-TABLE
           MOVE  "key2"            TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"       USING
                                  MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
      *
           .
       950-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報読込
      *****************************************************************
       955-HKNNUM-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           MOVE  "tbl_hknnum" TO  MCP-TABLE
           MOVE  "key2"       TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"  USING
                              MCPAREA
                              MCPDATA-REC
                              SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE  1  TO  FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM       TO  JOB-HOSPNUM
           MOVE    S100LNK-JOBID     TO  JOB-JOBID
           MOVE    S100LNK-SHELLID   TO  JOB-SHELLID
           MOVE    CNT-PAGE          TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       300-END-EXT.
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
