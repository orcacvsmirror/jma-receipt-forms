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
      *******************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000K144.
      *******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : (保険別)診療行為区分別集計表
      *  管理者            :
      *  作成日付    作業者        記述
      *  04/03/26    @@@@-半田     新規作成
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
      *    診療行為受診対象患者一時ファイル
           SELECT  K144T-FILE      ASSIGN              K144TPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  K144TMP-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    診療行為受診対象患者一時ファイル
       FD  K144T-FILE.
           COPY    "A00000K144TMP.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域
           COPY    "CPCOMMONPRT.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *
      *    診療行為受診対象患者一時ファイル(外部ファイル名称)
       01  K144TPARA.
           03  FILLER              PIC  X(05) VALUE "/tmp/".
           03  K144TPARA-FILENAME  PIC  X(11) VALUE "A00000K144T".
           03  FILLER              PIC  X(04) VALUE ".TMP".
      *
      *
           COPY    "A00000K100.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-RECE            PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-SYUBETU         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-M01TMP          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-SYSKANRI3       PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-TOKUTEI         PIC 9(01).
           03  FLG-TYPE            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACCT2        PIC 9(01).
           03  FLG-NSRYACCT        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-NYUIN           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-K144NPT         PIC 9(01).
           03  FLG-M01TMP          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-KENSASORT       PIC 9(01).
           03  INP-STS             PIC 9(04).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-ROOM            PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNTG           PIC 9(01).
           03  GET-OFF             PIC 9(01).
           03  GET-TENSU           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-ACCT            PIC 9(06).
           03  CNT-ACT             PIC 9(06).
           03  CNT-NINZU           PIC 9(06).
           03  CNT-JYURRK          PIC 9(06).
           03  CNT-DATA            PIC 9(06).
           03  CNT-K144TMP         PIC 9(06).
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
           03  IDX                PIC 9(01).
           03  IDX1               PIC 9(04).
           03  IDX2               PIC 9(04).
           03  IDX3               PIC 9(04).
           03  IDX4               PIC 9(04).
           03  IDX5               PIC 9(04).
           03  IDX6               PIC 9(03).
           03  IDX7G.
               05  IDX7           PIC 9(04).
           03  IDA-G.
               05  IDA            PIC 9(02).
           03  IDB-G.
               05  IDB            PIC 9(02).
           03  IDC                PIC 9(04).
           03  IDY                PIC 9(04).
           03  IDZ                PIC 9(04).
           03  IDM                PIC 9(04).
           03  IDX-MAX            PIC 9(04).
           03  IDX-TEN            PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
      *    対象年月最終日・初日
           03  WRK-PARA-PRTLYMD      PIC X(08).
           03  WRK-PARA-PRTSYMD      PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DENPJTIKBN        PIC 9(01).
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY         PIC 9(04).
               05  WRK-SRYMM         PIC 9(02).
               05  WRK-SRYDD         PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-SRYYM             PIC X(22).
           03  WRK-SEIYMD            PIC X(22).
           03  WRK-RECEERR           PIC X(200).
           03  WRK-DRCD              PIC X(05).
           03  WRK-CITYNUM-G.
               05  WRK-CITYNUM       PIC X(02).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-PTID              PIC 9(10).
           03  WRK-NINZU             PIC 9(01).
           03  WRK-1-31YMD-G.
               05  WRK-1             PIC X(01).
               05  WRK-2             PIC X(02).
               05  WRK-3             PIC X(02).
               05  WRK-4             PIC X(04).
           03  WRK-MONEY             PIC 9(08).
           03  WRK-JIHIKBN           PIC X(02).
           03  WRK-CHK-PTID          PIC 9(10).
           03  WRK-CHK-SRYKA         PIC X(02).
           03  WRK-CHK-DRCD          PIC X(05).
           03  WRK-CHK-KBN1          PIC X(01).
           03  WRK-CHK-KBN2          PIC X(02).
           03  WRK-CHK-KBN3          PIC X(03).
           03  WRK-BUNRUI-1          PIC X(01).
           03  WRK-BUNRUI-2          PIC X(02).
           03  WRK-BUNRUI-3          PIC X(03).
           03  WRK-WARIAI            PIC 9(03).
           03  WRK-HENKAN-G.
               05  WRK-HENKAN-1      PIC X(03).
               05  WRK-HENKAN-2      PIC X(02).
           03  WRK-KAISU             PIC 9(09).
           03  WRK-NISSU             PIC 9(03).
           03  WRK-TENSU             PIC S9(09).
           03  WRK-PTID2             PIC 9(10).
           03  WRK-TENSU2            PIC 9(09).
           03  WRK-SYUEKI            PIC 9(09).
           03  WRK-SYUEKI2           PIC 9(09).
           03  WRK-SYUEKI3           PIC 9(09).
           03  WRK-HKNNUM            PIC X(03).
           03  WRK-TENTNK            PIC 9(03).
           03  WRK-NENREI            PIC 9(03).
           03  WRK-KNSBUNRUI         PIC 9(02).
           03  WRK-TMP-NYUGAIKBN     PIC X(01).
           03  WRK-CNV-TEN-V         PIC S9(11)V9(02).
           03  WRK-CNV-TEN           PIC S9(11).
           03  WRK-YY1-G.
               05  WRK-YY1           PIC 9(04).
           03  WRK-YY2-G.
               05  WRK-YY2           PIC 9(04).
           03  WRK-TJIHI-G           OCCURS 10.
               05  WRK-TJIHI         PIC X(20).
               05  WRK-TJIHI-FLG     PIC 9(01).
           03  WRK-TJIHIN-G          OCCURS 10.
               05  WRK-TJIHIN        PIC X(20).
               05  WRK-TJIHIN-FLG    PIC 9(01).
           03  WRK-SKYSTYMD.
               05  WRK-SKYSTY        PIC 9(04).
               05  WRK-SKYSTM        PIC 9(02).
               05  WRK-SKYSTD        PIC 9(02).
           03  WRK-SKYEDYMD.
               05  WRK-SKYEDY        PIC 9(04).
               05  WRK-SKYEDM        PIC 9(02).
               05  WRK-SKYEDD        PIC 9(02).
           03  WRK-N-NISSU           PIC 9(02).
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z3                PIC ZZ9.
           03  WRK-Z4                PIC ZZZZ.
           03  WRK-Z5                PIC Z,ZZZ.
           03  WRK-Z6                PIC ZZ,ZZ9.
           03  WRK-Z62               PIC ZZZZZZ.
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z9                PIC Z,ZZZ,ZZZ.
           03  WRK-Z92               PIC ZZZZZZZZZ.
           03  WRK-Z10               PIC ZZ,ZZZ,ZZ9.
           03  WRK-Z11               PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13               PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-Z14               PIC ZZ,ZZZ,ZZZ,ZZZ.
           03  WRK-TENSU-HEN         PIC ZZZZZZZZZZZ.
           03  WRK-KAISU-HEN         PIC ZZZZZZZZZZZ.
           03  WRK-HKNCOMBI          PIC X(04).
           03  WRK-HKN-INDEX         PIC 9(04).
           03  WRK-BIRTHDAY.
               05 WRK-BIRTHY         PIC 9(04).
               05 WRK-BIRTHM         PIC 9(02).
               05 WRK-BIRTHD         PIC 9(02).
           03  WRK-ZAINUM            PIC 9(08).
           03  WRK-ZENPTID           PIC 9(10).
           03  WRK-ZENPTID2          PIC 9(10).
           03  WRK-TENSU3            PIC 9(09).
           03  WRK-TENYMD            PIC X(08).
      *
      *    年齢名退避エリア
       01  WRK-HKN-AREA.
           03  WRK-HKNMAX            PIC 9(03).
           03  WRK-HKN-TBL           OCCURS 100.
               05  WRK-HKNNAME       PIC X(20).
      *
      *    入院履歴情報退避エリア
       01  WRK-NYUIN-AREA.
           03  WRK-NYUIN-TBL.
               05  WRK-NYUIN-DAY     PIC 9(01)  OCCURS 31.
      *
      *    室料差額退避エリア
       01  WRK-ROOM-SAGAKU-AREA.
           03  WRK-ROOM-MAX           PIC 9(02).
           03  WRK-ROOM-DATA          OCCURS 50.
               05  WRK-ROOM-CD        PIC 9(03).
               05  WRK-ROOM-SGK       PIC 9(05).
      *
      *   入外別合計加算エリア
       01 WRK-K144ALL-AREA.
           03  WRK-K144ALL-SRY-TBL    OCCURS 56.
               05  WRK-K144ALL-TENSU  PIC  9(11).
               05  WRK-K144ALL-KAISU  PIC  9(11).
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
      *    請求自費名称情報
           COPY    "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
       01  SRYACCT-REC2.
           COPY    "CPSRYACCT.INC"   REPLACING  //ACCT-//
                                     BY         //ACCT2-//.
      *
      *    入院会計マスタ
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院会計マスタ２
       01  NYUINACCT2-REC.
           COPY    "CPNYUINACCT.INC"   REPLACING  //NACCT-//
                                       BY         //NACCT2-//.
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
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
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    検査ソートマスタ
       01  KENSASORT-REC.
           COPY    "CPKENSASORT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
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
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブGET-OFF
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
           COPY    "A00000K100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000K100-LNK.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF  INP-STS   =    ZERO
      *    正常主処理
             PERFORM 200-MAIN-SEC
           ELSE
      *    エラー処理
             PERFORM 210-ERR-SEC
           END-IF
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
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-PARA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-K144ALL-AREA
           INITIALIZE                  WRK-HKN-AREA
           INITIALIZE                  WRK-ROOM-SAGAKU-AREA
           INITIALIZE                  WRK-NYUIN-AREA
           INITIALIZE                  SPA-AREA
      *
           MOVE    K100LNK-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    K100LNK-JOBID       TO  JOB-JOBID
           MOVE    K100LNK-SHELLID     TO  JOB-SHELLID
           MOVE    "A00000K144"        TO  JOB-PGID
           MOVE    "(保険別)診療区分別集計表"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    K100LNK-SRYYM       TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    K100LNK-SRYYM       TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYM
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    K100LNK-SRYYM       TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD            TO    WRK-PARA-PRTLYMD
      *
      *    対象年月初日取得
           INITIALIZE  WRK-SYMD
           MOVE    K100LNK-SRYYM       TO    WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                TO    WRK-PARA-PRTSYMD(7:2)
      *
      *    医療機関ＩＤ編集
           INITIALIZE                     SYS-1001-REC
           MOVE    "1001"         TO      SYS-1001-KANRICD
           MOVE    "*"            TO      SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM    TO      SYS-1001-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD   TO  SYS-1001-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD   TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC   TO      MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC     TO    SYS-1001-REC
             IF      SYS-1001-HOSPNAME  =  LOW-VALUE
               MOVE    SPACE      TO    SYS-1001-HOSPNAME
             END-IF
             IF      SYS-1001-KAISETUNAME  =   LOW-VALUE
               MOVE    SPACE      TO    SYS-1001-KAISETUNAME
             END-IF
           ELSE
             MOVE    "医療機関情報が取得できませんでした"
                                     TO    WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE    SPACE              TO   SYS-1031-REC
           INITIALIZE                 SYS-1031-REC
           MOVE    "1031"             TO   SYS-1031-KANRICD
           MOVE    "*"                TO   SYS-1031-KBNCD
           MOVE    SPA-HOSPNUM        TO   SYS-1031-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD   TO   SYS-1031-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD   TO   SYS-1031-EDYUKYMD
           MOVE    SYS-1031-REC       TO   MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF    FLG-SYSKANRI  =   ZERO
             MOVE    MCPDATA-REC      TO   SYS-1031-REC
           ELSE
             INITIALIZE               SYS-1031-REC
             MOVE    01               TO   INP-STS
           END-IF
      *
      *    入外区分のチェック
           IF   K100LNK-NYUGAIKBN   NOT =   SPACE
               AND  K100LNK-NYUGAIKBN   NOT =   "1"
               AND  K100LNK-NYUGAIKBN   NOT =   "2"
             MOVE    10      TO  INP-STS
             GO              TO  100-INIT-EXT
           END-IF
      *
      *    保険外区分のチェック
           IF   K100LNK-HKNTG   NOT =   SPACE
               AND K100LNK-HKNTG       NOT =   "0"
             MOVE    20      TO  INP-STS
             GO              TO  100-INIT-EXT
           END-IF
      *
      *    保険別印刷
             PERFORM  110-HKN-PARA-HENSYU-SEC
      *
      *    入院自費情報取得
      *    区分11
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "11"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(1)
             MOVE  1                 TO  WRK-TJIHIN-FLG(1)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(1)
           END-IF
      *
      *    区分12
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "12"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(2)
             MOVE  1                 TO  WRK-TJIHIN-FLG(2)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(2)
           END-IF
      *
      *    区分13
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "13"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(3)
             MOVE  1                 TO  WRK-TJIHIN-FLG(3)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(3)
           END-IF
      *
      *    区分14
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "14"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(4)
             MOVE  1                 TO  WRK-TJIHIN-FLG(4)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(4)
           END-IF
      *
      *    区分15
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "15"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(5)
             MOVE  1                 TO  WRK-TJIHIN-FLG(5)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHIN(5)
           END-IF
      *
      *    区分16
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "16"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(6)
             MOVE  1                 TO  WRK-TJIHIN-FLG(6)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHIN(6)
           END-IF
      *
      *    区分17
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "17"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(7)
             MOVE  1                 TO  WRK-TJIHIN-FLG(7)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(7)
           END-IF
      *
      *    区分18
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "18"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(8)
             MOVE  1                 TO  WRK-TJIHIN-FLG(8)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(8)
           END-IF
      *
      *    区分19
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "19"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(9)
             MOVE  1                 TO  WRK-TJIHIN-FLG(9)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHIN(9)
           END-IF
      *
      *    区分10
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "10"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(10)
               MOVE  1                 TO  WRK-TJIHIN-FLG(10)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHIN(10)
           END-IF
      *
      *    外来自費情報取得
      *    区分21
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "21"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(1)
             MOVE  1                 TO  WRK-TJIHI-FLG(1)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(1)
           END-IF
      *
      *    区分22
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "22"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(2)
             MOVE  1                 TO  WRK-TJIHI-FLG(2)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(2)
           END-IF
      *
      *    区分23
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "23"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(3)
             MOVE  1                 TO  WRK-TJIHI-FLG(3)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(3)
           END-IF
      *
      *    区分24
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "24"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(4)
             MOVE  1                 TO  WRK-TJIHI-FLG(4)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(4)
           END-IF
      *
      *    区分25
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "25"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(5)
             MOVE  1                 TO  WRK-TJIHI-FLG(5)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(5)
           END-IF
      *
      *    区分26
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "26"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(6)
             MOVE  1                 TO  WRK-TJIHI-FLG(6)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(6)
           END-IF
      *
      *    区分27
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "27"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(7)
             MOVE  1                 TO  WRK-TJIHI-FLG(7)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(7)
           END-IF
      *
      *    区分28
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "28"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF    FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(8)
             MOVE  1                 TO  WRK-TJIHI-FLG(8)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(8)
           END-IF
      *
      *    区分29
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "29"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(9)
             MOVE  1                 TO  WRK-TJIHI-FLG(9)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(9)
           END-IF
      *
      *    区分20
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "20"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC       TO  SYS-1013-REC
             MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(10)
             MOVE  1                 TO  WRK-TJIHI-FLG(10)
           ELSE
             MOVE  SPACE             TO  WRK-TJIHI(10)
           END-IF
      *
      *    全室料差額取得
           INITIALIZE                      SYS-5113-REC
           MOVE    "5113"              TO  SYS-5113-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-5113-HOSPNUM
           PERFORM  900-SYS-SELECT-SEC
           PERFORM  900-SYS-FETCH-SEC
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >     50
                    OR       FLG-ROOM  =  1
             IF  FLG-SYSKANRI  =  ZERO
               MOVE  SYS-5113-BRM-SAGAKU-NM  TO WRK-ROOM-SGK(IDX4)
               MOVE  IDX4                    TO WRK-ROOM-CD(IDX4)
             ELSE
               MOVE  1  TO  FLG-ROOM
             END-IF
             PERFORM 900-SYS-FETCH-SEC
           END-PERFORM
           PERFORM   900-SYS-CLOSE-SEC
           IF    FLG-ROOM     =   ZERO
             MOVE  IDX4    TO    WRK-ROOM-MAX
           ELSE
             COMPUTE    WRK-ROOM-MAX  =    IDX4  -  1
           END-IF
      *
      *    システム日付セット
           MOVE  K100LNK-PRTKANRI-SKYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE  WRK-HENYMDG  TO  WRK-SEIYMD
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    年齢編集処理
      *****************************************************************
       110-HKN-PARA-HENSYU-SEC              SECTION.
      *
      * 保険別カラム作成
           IF   K100LNK-SRYYM  <  "200804"
             MOVE   "社保本人"        TO  WRK-HKNNAME(1)
             MOVE   "社保家族"        TO  WRK-HKNNAME(2)
             MOVE   "社保老人"        TO  WRK-HKNNAME(3)
             MOVE   "国保一般・退職"  TO  WRK-HKNNAME(4)
             MOVE   "国保老人"        TO  WRK-HKNNAME(5)
             MOVE   "公費単独"        TO  WRK-HKNNAME(6)
             MOVE   "自費"            TO  WRK-HKNNAME(7)
             MOVE   "労災・自賠責"    TO  WRK-HKNNAME(8)
             MOVE   "包括"            TO  WRK-HKNNAME(9)
           ELSE
             MOVE   "社保本人"        TO  WRK-HKNNAME(1)
             MOVE   "社保家族"        TO  WRK-HKNNAME(2)
             MOVE   "国保一般・退職"  TO  WRK-HKNNAME(3)
             MOVE   "後期高齢者"      TO  WRK-HKNNAME(4)
             MOVE   "公費単独"        TO  WRK-HKNNAME(5)
             MOVE   "自費"            TO  WRK-HKNNAME(6)
             MOVE   "労災・自賠責"    TO  WRK-HKNNAME(7)
             MOVE   "公害"            TO  WRK-HKNNAME(8)
             MOVE   "包括"            TO  WRK-HKNNAME(9)
           END-IF
           MOVE     10               TO  WRK-HKNMAX
      *
           .
       110-HKN-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *
           OPEN     OUTPUT  K144T-FILE
           CLOSE            K144T-FILE
           OPEN     I-O     K144T-FILE
      *
           MOVE     ZERO    TO        GET-OFF
           EVALUATE  K100LNK-NYUGAIKBN
             WHEN  SPACE
             WHEN  "1"
               MOVE    "1"       TO     WRK-NYUGAIKBN
             WHEN  "2"
               MOVE    "2"       TO     WRK-NYUGAIKBN
           END-EVALUATE
      *
      *    空データ作成
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL    IDX4  >     8
      *    キー設定
             INITIALIZE                      K144TMP-REC
      *
             MOVE    WRK-NYUGAIKBN     TO    K144TMP-NYUGAIKBN
             MOVE    IDX4              TO    K144TMP-HKNINDEX
      *    TMPファイル読み込み
             PERFORM  800-SKTTMP-READ-SEC
      *
             IF  FLG-M01TMP   =  1
               WRITE    K144TMP-REC
             ELSE
               REWRITE  K144TMP-REC
             END-IF
      *
           END-PERFORM
      *    データ集計処理
           PERFORM  UNTIL   GET-OFF   =      1
      *    集計処理
             PERFORM  2001-SYUKEI-SEC
             EVALUATE  K100LNK-NYUGAIKBN
               WHEN  SPACE
                 IF  WRK-NYUGAIKBN  =   "1"
                   MOVE   "2"    TO     WRK-NYUGAIKBN
                 ELSE
                   MOVE 1      TO     GET-OFF
                 END-IF
               WHEN  "1"
               WHEN  "2"
                 MOVE     1      TO     GET-OFF
             END-EVALUATE
           END-PERFORM
      *
      *
           CLOSE   K144T-FILE
      *
      *    帳票印刷処理
           PERFORM  2002-PRINT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE             K100
      *
           MOVE    "(保険別)"      TO    K100-SUBTITLE
      *
           EVALUATE  INP-STS
           WHEN  01
             MOVE   "プリンタ情報が取得できませんでした"
                                  TO    K100-JOBMSG
           WHEN  10
             MOVE   "入力された入外区分の値に誤りがあります"
                                  TO    K100-JOBMSG
           WHEN  20
             MOVE   "入力された保険外区分の値に誤りがあります"
                                    TO    K100-JOBMSG
           END-EVALUATE
      *    ページ数
           ADD    1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    K100-PAGE
      *
           MOVE    WRK-SRYYM        TO    K100-SRYYM
           MOVE    WRK-SEIYMD       TO    K100-SYSYMD
      *
           PERFORM 330-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      * 保険インデックス判別処理
      *****************************************************************
       220-HKN-GROUPING-SEC      SECTION.
      *
      *   保険組合情報
           INITIALIZE                           HKNCOMBI-REC
           MOVE    PTINF-HOSPNUM            TO  COMB-HOSPNUM
           MOVE    PTINF-PTID               TO  COMB-PTID
           MOVE    WRK-HKNCOMBI             TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
      *    年齢計算
           MOVE    PTINF-BIRTHDAY           TO  WRK-BIRTHDAY
           MOVE    WRK-PARA-PRTSYMD         TO  WRK-SYMD
           COMPUTE  WRK-NENREI              =   WRK-SYY
                                            -   WRK-BIRTHY
           IF    WRK-SMM  <   WRK-BIRTHM
             COMPUTE  WRK-NENREI            =   WRK-NENREI - 1
           END-IF
      *
           EVALUATE  COMB-HKNNUM
               WHEN  "001"
               WHEN  "002"
               WHEN  "003"
               WHEN  "004"
               WHEN  "006"
               WHEN  "007"
               WHEN  "031"
               WHEN  "032"
               WHEN  "033"
               WHEN  "034"
                 IF   COMB-KOHHKNNUM(1)    =   "027"
                     OR  COMB-KOHHKNNUM(2)    =   "027"
                     OR  COMB-KOHHKNNUM(3)    =   "027"
                     OR  COMB-KOHHKNNUM(4)    =   "027"
                   MOVE     3           TO  IDX3
                 ELSE
                   IF     WRK-NENREI    <   3
                     MOVE     2           TO  IDX3
                   ELSE
                     IF    COMB-HONKZKKBN   =   "1"
                       MOVE     1           TO  IDX3
                     ELSE
                       MOVE     2           TO  IDX3
                     END-IF
                   END-IF
                 END-IF
      *
               WHEN  "060"
                 IF    COMB-KOHHKNNUM(1)    =   "027"
                     OR  COMB-KOHHKNNUM(2)    =   "027"
                     OR  COMB-KOHHKNNUM(3)    =   "027"
                     OR  COMB-KOHHKNNUM(4)    =   "027"
                   MOVE         5           TO  IDX3
                 ELSE
                   IF     WRK-NENREI        <   3
                     MOVE       4           TO  IDX3
                   ELSE
                     MOVE       4           TO  IDX3
                   END-IF
                 END-IF
      *
               WHEN  "063"
               WHEN  "067"
               WHEN  "072"
               WHEN  "073"
               WHEN  "074"
               WHEN  "075"
                 IF    COMB-KOHHKNNUM(1)    =   "027"
                     OR  COMB-KOHHKNNUM(2)    =   "027"
                     OR  COMB-KOHHKNNUM(3)    =   "027"
                     OR  COMB-KOHHKNNUM(4)    =   "027"
                   MOVE    4            TO  IDX3
                 ELSE
                   IF     WRK-NENREI        <   3
                     MOVE     4            TO  IDX3
                   ELSE
                     IF    COMB-HONKZKKBN   =   "1"
                       MOVE     4           TO  IDX3
                     ELSE
                       MOVE     4           TO  IDX3
                     END-IF
                   END-IF
                 END-IF
      *
               WHEN  "971"
               WHEN  "973"
                   MOVE            8          TO  IDX3
      *
               WHEN  SPACE
                 IF  WRK-HKNCOMBI  =  "9999"
                   MOVE     9          TO  IDX3
                 ELSE
                   MOVE     6          TO  IDX3
                 END-IF
      *
               WHEN  OTHER
                 MOVE           7           TO  IDX3
           END-EVALUATE
      *
           MOVE   IDX3             TO   WRK-HKN-INDEX
      *
           .
       220-HKN-GROUPING-EXT.
           EXIT.
      *****************************************************************
      * 保険インデックス判別処理(H20年4月以降)
      *****************************************************************
       230-HKN-GROUPING-SEC      SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    PTINF-HOSPNUM        TO  AGECHK-HOSPNUM
           MOVE    PTINF-PTID           TO  AGECHK-PTID
           MOVE    K100LNK-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"       TO  AGECHK-SRYYMD(7:2)
           MOVE    COMB-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *    保険組合情報
           INITIALIZE                           HKNCOMBI-REC
           MOVE    PTINF-HOSPNUM            TO  COMB-HOSPNUM
           MOVE    PTINF-PTID               TO  COMB-PTID
           MOVE    WRK-HKNCOMBI             TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
      *    年齢計算
           MOVE    PTINF-BIRTHDAY           TO  WRK-BIRTHDAY
           MOVE    WRK-PARA-PRTSYMD         TO  WRK-SYMD
           COMPUTE  WRK-NENREI              =   WRK-SYY
                                            -   WRK-BIRTHY
           IF   WRK-SMM  <   WRK-BIRTHM
             COMPUTE      WRK-NENREI      =   WRK-NENREI - 1
           END-IF
      *
           EVALUATE  COMB-HKNNUM
             WHEN  "001"
             WHEN  "002"
             WHEN  "003"
             WHEN  "004"
             WHEN  "006"
             WHEN  "007"
             WHEN  "009"
             WHEN  "031"
             WHEN  "032"
             WHEN  "033"
             WHEN  "034"
                 IF    COMB-KOHHKNNUM(1)    =   "027"  OR
                       COMB-KOHHKNNUM(2)    =   "027"  OR
                       COMB-KOHHKNNUM(3)    =   "027"  OR
                       COMB-KOHHKNNUM(4)    =   "027"
                     MOVE         3           TO  IDX3
                 ELSE
                     IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE       2           TO  IDX3
                     ELSE
                         IF    COMB-HONKZKKBN   =   "1"
                             MOVE     1           TO  IDX3
                         ELSE
                             MOVE     2           TO  IDX3
                         END-IF
                     END-IF
                 END-IF
      *
             WHEN  "060"
                 IF    COMB-KOHHKNNUM(1)    =   "027"  OR
                       COMB-KOHHKNNUM(2)    =   "027"  OR
                       COMB-KOHHKNNUM(3)    =   "027"  OR
                       COMB-KOHHKNNUM(4)    =   "027"
                     MOVE         4           TO  IDX3
                 ELSE
                     MOVE       3           TO  IDX3
                 END-IF
      *
             WHEN  "063"
             WHEN  "067"
             WHEN  "072"
             WHEN  "073"
             WHEN  "074"
             WHEN  "075"
               IF    COMB-KOHHKNNUM(1)    =   "027"
                   OR  COMB-KOHHKNNUM(2)    =   "027"
                   OR  COMB-KOHHKNNUM(3)    =   "027"
                   OR  COMB-KOHHKNNUM(4)    =   "027"
                 MOVE     4              TO  IDX3
               ELSE
                 MOVE     3              TO  IDX3
               END-IF
      *
             WHEN  "971"
             WHEN  "973"
               MOVE     7          TO  IDX3
             WHEN  "975"
               MOVE     8          TO  IDX3
             WHEN  "039"
             WHEN  "040"
                  MOVE           4          TO  IDX3
      *
             WHEN  SPACE
               IF  WRK-HKNCOMBI  =  "9999"
                 MOVE     9          TO  IDX3
               ELSE
                 MOVE     5          TO  IDX3
               END-IF
      *
             WHEN  OTHER
               MOVE       6          TO  IDX3
           END-EVALUATE
      *
           MOVE   IDX3               TO   WRK-HKN-INDEX
      *
           .
       220-HKN-GROUPING-EXT.
           EXIT.
      *****************************************************************
      *    診療行為データ集計処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      *
      *    入院診療行為情報集計
           PERFORM  900-NYUINACCT-SELECT-SEC
           PERFORM  UNTIL    FLG-NSRYACCT   =   1
             IF  WRK-NYUGAIKBN  =   "1"
               IF   NACCT-PTID  NOT =  WRK-ZENPTID2
                 PERFORM  20011-NYUIN-SEC
                 MOVE   NACCT-PTID   TO    WRK-ZENPTID2
               END-IF
      *    入院件数カウント処理
               PERFORM   20000-NYUINKENSU-SEC
             END-IF
             PERFORM  900-NYUINACCT-FETCH-SEC
           END-PERFORM
           PERFORM  900-NYUINACCT-CLOSE-SEC
      *
           PERFORM  900-SRYACCT-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-SRYACCT   =   1
      *
             INITIALIZE   WRK-TENYMD
             PERFORM   VARYING   IDX-TEN   FROM   1   BY   1
                       UNTIL     IDX-TEN   >   31
                       OR        WRK-TENYMD   NOT =   SPACE
               IF   ACCT-DAY(1 IDX-TEN)   >   ZERO
                 MOVE   ACCT-SRYYM   TO   WRK-TENYMD(1:6)
                 MOVE   IDX-TEN      TO   WRK-TENYMD(7:2)
               END-IF
             END-PERFORM
      *    年齢インデックス
             MOVE    ACCT-HKNCOMBI     TO  WRK-HKNCOMBI
             IF K100LNK-SRYYM  <  "200804"
               PERFORM  220-HKN-GROUPING-SEC
             ELSE
               PERFORM  230-HKN-GROUPING-SEC
             END-IF
      *
      *    診療行為情報集計
             IF  K100LNK-HOUKATU  NOT =  "0"
               IF ACCT-HKNCOMBI NOT = "9999"
                   AND  ACCT-JIHOKBN  NOT =  "1"
                 PERFORM  20012-SRY-SET
               END-IF
             ELSE
               IF  ACCT-JIHOKBN  =  "1"
                 MOVE   9   TO   WRK-HKN-INDEX
               END-IF
               PERFORM  20012-SRY-SET
             END-IF
             PERFORM  900-SRYACCT-FETCH-SEC
           END-PERFORM
      *
           PERFORM  900-SRYACCT-CLOSE-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料設定
      *****************************************************************
       20011-NYUIN-SEC                SECTION.
      *
      *    収納情報より、入院料・室料差額・食事取得
      *    収納情報検索
           PERFORM  940-SYUNOU-SELECT-SEC
           PERFORM UNTIL   FLG-SYUNOU  =  1
             IF  SYU-NYUGAIKBN   =  "1"
      *
      *    テスト患者は対象外
               MOVE    SYU-PTID    TO  WRK-PTID
               PERFORM  900-PTINF-READ-SEC
               IF   FLG-PTINF   =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "0"
                 MOVE   SYU-HKNCOMBINUM  TO  WRK-HKNCOMBI
                 IF   K100LNK-SRYYM  <  "200804"
                   PERFORM  220-HKN-GROUPING-SEC
                 ELSE
                   PERFORM  230-HKN-GROUPING-SEC
                 END-IF
                 PERFORM  20013-NYUIN-SET-SEC
               END-IF
             END-IF
             PERFORM  940-SYUNOU-FETCH-SEC
           END-PERFORM
           PERFORM  940-SYUNOU-CLOSE-SEC
      *
           .
       20011-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院点数集計処理
      *****************************************************************
       20013-NYUIN-SET-SEC                 SECTION.
      *
      *    キー設定診療行為受診対象患者一時ファイル
           INITIALIZE                      K144TMP-REC
      *
           MOVE    SYU-NYUGAIKBN         TO    K144TMP-NYUGAIKBN
           MOVE    WRK-HKN-INDEX         TO    K144TMP-HKNINDEX
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    入院料加算
           ADD    SYU-NYN-HKNTEN         TO    K144TMP-TENSU(42)
      *
      *    室料差額加算
           IF   SYU-RMSAGAKU  NOT =  ZERO
             ADD    SYU-RMSAGAKU         TO    K144TMP-TENSU(41)
           END-IF
      *
      *    食事加算
           ADD    SYU-RYOYOHI-SKJ        TO    K144TMP-TENSU(41)
           ADD    SYU-RYOYOHI-LIFE       TO    K144TMP-TENSU(41)
      *
           IF  FLG-M01TMP   =  1
             WRITE  K144TMP-REC
           ELSE
             REWRITE  K144TMP-REC
           END-IF
      *
           .
       20013-NYUIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院件数集計処理
      *****************************************************************
       20000-NYUINKENSU-SEC                SECTION.
      *
      *    キー設定一時ファイル
           INITIALIZE                 K144TMP-REC
      *
           MOVE    SYU-NYUGAIKBN      TO    K144TMP-NYUGAIKBN
           MOVE    WRK-HKN-INDEX      TO    K144TMP-HKNINDEX
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
           PERFORM  VARYING  IDA  FROM  1  BY  1
                    UNTIL  IDA  >  31
             PERFORM   920-NYUINACCT-READ-SEC
             IF  NACCT-DAY(IDA)  >  0
      *
               EVALUATE  NACCT-ZAISKBKBN
                 WHEN  "1"
      *    入院料件数加算
                   ADD     1          TO   K144TMP-KAISU(42)
                 WHEN  "3"
                 WHEN  "4"
      *    室料差額件数、食事件数加算
                   IF  NACCT-SRYCDTOTAL  NOT =  "00000197000570"
                     ADD    1         TO   K144TMP-KAISU(41)
                   END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
      *
           IF  FLG-M01TMP   =  1
             WRITE    K144TMP-REC
           ELSE
             REWRITE  K144TMP-REC
           END-IF
           .
       20000-NYUINKENSU-EXT.
           EXIT.
      *****************************************************************
      *    入院診療会計情報検索
      *****************************************************************
       920-NYUINACCT-READ-SEC           SECTION.
      *
           INITIALIZE                   NYUINACCT2-REC
           MOVE    NACCT-HOSPNUM        TO   NACCT2-HOSPNUM
           MOVE    NACCT-PTID           TO   NACCT2-PTID
           MOVE    NACCT-SRYYM          TO   NACCT2-SRYYM
           MOVE    "2"                  TO   NACCT2-ZAISKBKBN
           MOVE    NYUINACCT2-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO   MCP-TABLE
           MOVE    "key17"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             PERFORM   920-NYUINACCT-FETCH-SEC
             EVALUATE  NACCT2-DAY(IDA)
               WHEN  1
               WHEN  2
               WHEN  3
                 IF   NACCT-ZAISKBKBN  =  "4"  OR  "6"
                   MOVE    0            TO   NACCT-DAY(IDA)
                 END-IF
             END-EVALUATE
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO   MCP-TABLE
           MOVE    "key17"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院診療会計情報検索
      *****************************************************************
       920-NYUINACCT-FETCH-SEC           SECTION.
      *
           MOVE    "DBFETCH"          TO   MCP-FUNC
           MOVE    "tbl_nyuinacct"    TO   MCP-TABLE
           MOVE    "key17"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC      TO   NYUINACCT2-REC
           ELSE
             INITIALIZE                    NYUINACCT2-REC
           END-IF
           .
       920-NYUINACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報取得設定
      *****************************************************************
       20012-SRY-SET                SECTION.
      *
      *    キー設定
           INITIALIZE                      K144TMP-REC
      *
           MOVE    ACCT-NYUGAIKBN    TO    K144TMP-NYUGAIKBN
           MOVE    WRK-HKN-INDEX     TO    K144TMP-HKNINDEX
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    診療行為情報読み込み
      *    (14)往診・在宅,(60)検査,(95)非課税自費,(96)課税自費は
      *    診療コード毎に分類する
           IF    ACCT-SRYKBN  =  "14"
               OR   ACCT-SRYKBN  =  "60"
               OR   ACCT-SRYKBN  =  "64"
               OR   ACCT-SRYKBN  =  "95"
               OR   ACCT-SRYKBN  =  "96"
             PERFORM  200121-SRYCD-CHK-SEC
           ELSE
             PERFORM  200122-ETC-SEC
           END-IF
      *
           IF  FLG-M01TMP  =  1
             WRITE  K144TMP-REC
           ELSE
             REWRITE  K144TMP-REC
           END-IF
      *
           .
       20012-SRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードチェック処理
      ****************************************************************
       200121-SRYCD-CHK-SEC                SECTION.
      *
           PERFORM  900-SRYACT-SELECT-SEC
      *
           PERFORM  UNTIL    FLG-SRYACT   =   1
      *
             PERFORM  VARYING  IDZ  FROM  1  BY  1
                      UNTIL  IDZ  >     5
                      OR  SRY-SRYCD(IDZ)    =   SPACE
               EVALUATE  SRY-SRYKBN
                 WHEN  "14"
                   PERFORM  200121-ZAITAKU-SET-SEC
                 WHEN  "60"
                 WHEN  "64"
                   PERFORM  200121-KENSA-SET-SEC
                 WHEN  "95"
                 WHEN  "96"
                   PERFORM  200121-JIHI-SET-SEC
               END-EVALUATE
             END-PERFORM
      *
             PERFORM  900-SRYACT-FETCH-SEC
           END-PERFORM
      *
           PERFORM  900-SRYACT-CLOSE-SEC
      *
           .
       200121-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅点数設定処理
      *****************************************************************
       200121-ZAITAKU-SET-SEC                SECTION.
      *
           PERFORM  900-TENSU-READ-SEC
      *
           IF    TNS-CDKBN-KBN     =  "C"
               AND   TNS-CDKBN-KBNNUM  =  ZERO
             MOVE    12    TO   IDX1
           ELSE
             MOVE    13    TO   IDX1
           END-IF
      *
      *    点数加算
           IF   ACCT-ZAINUM  NOT =  WRK-ZAINUM
               OR  ( ACCT-ZAINUM  NOT =  WRK-ZAINUM
               AND   ACCT-PTID  NOT =  WRK-PTID2 )
      *
             COMPUTE  WRK-TENSU  =  ACCT-ZAITEN  *  ACCT-ZAIKAISU
             IF   TNS-TENSIKIBETU  =  7  OR  8
               COMPUTE  WRK-TENSU  =  WRK-TENSU  *  -1
             END-IF
      *
             ADD    WRK-TENSU        TO   K144TMP-TENSU(IDX1)
                                          K144TMP-TENSU(14)
      *
      *    回数加算
             ADD    ACCT-ZAIKAISU    TO   K144TMP-KAISU(IDX1)
                                          K144TMP-KAISU(14)
           END-IF
      *
           MOVE    ACCT-ZAINUM      TO   WRK-ZAINUM
           MOVE    ACCT-PTID        TO   WRK-PTID2
      *
           .
       200121-ZAITAKU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査点数設定処理
      *****************************************************************
       200121-KENSA-SET-SEC                SECTION.
      *
      *    採血チェック
           IF   SRY-SRYKBN  =  "60"
             MOVE    2            TO   WRK-KNSBUNRUI
             PERFORM  900-KENSA-READ-SEC
             IF  FLG-KENSASORT  =  ZERO
               MOVE    33         TO   IDX1
             ELSE
      *    判断料チェック
               MOVE    99         TO   WRK-KNSBUNRUI
               PERFORM  900-KENSA-READ-SEC
               IF    FLG-KENSASORT  =  ZERO
                 MOVE    34       TO   IDX1
               ELSE
                 MOVE    32       TO   IDX1
               END-IF
             END-IF
           ELSE
             MOVE    54           TO   IDX1
           END-IF
      *
      *    点数取得
           PERFORM  900-TENSU-READ-SEC
      *
      *    点数加算
           IF   ACCT-ZAINUM  NOT =  WRK-ZAINUM
               OR  ( ACCT-ZAINUM  =  WRK-ZAINUM
               AND  ACCT-PTID  NOT =  WRK-PTID2 )
      *
             COMPUTE  WRK-TENSU  =  ACCT-ZAITEN  *  ACCT-ZAIKAISU
             IF   TNS-TENSIKIBETU  =  7  OR  8
               COMPUTE  WRK-TENSU  =  WRK-TENSU  *  -1
             END-IF
      *
             ADD    WRK-TENSU        TO   K144TMP-TENSU(IDX1)
                                          K144TMP-TENSU(35)
      *
      *    回数加算
             ADD    ACCT-ZAIKAISU    TO   K144TMP-KAISU(IDX1)
                                          K144TMP-KAISU(35)
           END-IF
           MOVE    ACCT-ZAINUM       TO   WRK-ZAINUM
           MOVE    ACCT-PTID         TO   WRK-PTID2
      *
           .
       200121-KENSA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険適用外点数、自費項目金額設定処理
      *****************************************************************
       200121-JIHI-SET-SEC                SECTION.
      *
           PERFORM  900-TENSU-READ-SEC
           IF    SRY-JIHIMONEY(IDZ)     =     ZERO
      *    点数、金額がゼロの場合は加算処理は行わない。
             GO          TO    200121-JIHI-SET-EXT
           END-IF
      *
           INITIALIZE   FLG-HKNTG
           INITIALIZE   IDX1
           INITIALIZE   IDX2
      *
           IF    TNS-SRYCD(1:3)    =     "095"
               OR   TNS-SRYCD(1:3)    =     "096"
             IF  WRK-NYUGAIKBN      =     "1"
               EVALUATE  TNS-NYUTENTTLKBN
      *
                 WHEN  1
                   MOVE    43     TO   IDX1
                 WHEN  2
                   MOVE    44     TO   IDX1
                 WHEN  3
                   MOVE    45     TO   IDX1
                 WHEN  4
                   MOVE    46     TO   IDX1
                 WHEN  5
                   MOVE    47     TO   IDX1
                 WHEN  6
                   MOVE    48     TO   IDX1
                 WHEN  7
                   MOVE    49     TO   IDX1
                 WHEN  8
                   MOVE    50     TO   IDX1
                 WHEN  9
                   MOVE    51     TO   IDX1
                 WHEN  10
                   MOVE    52     TO   IDX1
                 WHEN  OTHER
                   MOVE    53     TO   IDX1
      *
               END-EVALUATE
             ELSE
               EVALUATE  TNS-GAITENTTLKBN
      *
                 WHEN  1
                   MOVE    43     TO   IDX1
                 WHEN  2
                   MOVE    44     TO   IDX1
                 WHEN  3
                   MOVE    45     TO   IDX1
                 WHEN  4
                   MOVE    46     TO   IDX1
                 WHEN  5
                   MOVE    47     TO   IDX1
                 WHEN  6
                   MOVE    48     TO   IDX1
                 WHEN  7
                   MOVE    49     TO   IDX1
                 WHEN  8
                   MOVE    50     TO   IDX1
                 WHEN  9
                   MOVE    51     TO   IDX1
                 WHEN  10
                   MOVE    52     TO   IDX1
                 WHEN  OTHER
                   MOVE    53     TO   IDX1
      *
               END-EVALUATE
             END-IF
           ELSE
      *
      *    保険適用外フラグON
             MOVE  1     TO   FLG-HKNTG
             IF    K100LNK-HKNTG   =    SPACE
      *    保険適用外取扱区分がSPACEの場合は加算処理は行わない。
               GO      TO     200121-JIHI-SET-EXT
             END-IF
             IF  TNS-DATAKBN  =  "1"
               PERFORM   20013-IDXSET2-SEC
             ELSE
               EVALUATE  TNS-SRYCD
                 WHEN  "610000000"  THRU  "619999999"
                   PERFORM   20013-IDXSET2-SEC
                 WHEN  "660000000"  THRU  "669999999"
                   PERFORM   20013-IDXSET2-SEC
                 WHEN  OTHER
                   GO    TO  200121-JIHI-SET-EXT
               END-EVALUATE
             END-IF
           END-IF
      *
      *    点数加算
           IF   FLG-HKNTG   =   1
      *
      *    保険適用外の場合
      *
             EVALUATE  TNS-TENSIKIBETU
               WHEN  1
      *    金額項目の場合は点数に変換してデータ化する。
                 COMPUTE   WRK-CNV-TEN-V   =
                      (TNS-TEN  * SRY-SRYSURYO(IDZ)) / 10
                 COMPUTE   WRK-CNV-TEN     ROUNDED  =
                      WRK-CNV-TEN-V  *  1
               WHEN  3
               WHEN  8
      *    点数項目の場合は回数をデータ化する。
                 MOVE   TNS-TEN  TO     WRK-CNV-TEN
               WHEN  OTHER
      *    点数、金額以外の場合
                       GO              TO     200121-JIHI-SET-EXT
             END-EVALUATE
             COMPUTE  WRK-TENSU   =   WRK-CNV-TEN  *  ACCT-ZAIKAISU
             ADD    WRK-TENSU     TO     K144TMP-TENSU(IDX1)
           ELSE
      *
      *    自費項目の場合
             COMPUTE  WRK-TENSU  =  SRY-JIHIMONEY(IDZ)  *  ACCT-ZAIKAISU
             ADD      WRK-TENSU     TO   K144TMP-TENSU(IDX1)
           END-IF
      *
      *    回数加算
           ADD    ACCT-ZAIKAISU     TO   K144TMP-KAISU(IDX1)
      *
           .
       200121-JIHI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他診療区分別設定処理
      *****************************************************************
       200122-ETC-SEC                SECTION.
      *
      *    診療行為情報より診療種別区分取得
           PERFORM  900-SRYACT-SELECT-SEC
           PERFORM  900-SRYACT-CLOSE-SEC
      *
           IF    ACCT-ZAITEN  =        ZERO
      *    点数、金額がゼロの場合は加算設定処理は行わない。
             GO           TO       200122-ETC-EXT
           END-IF
      *
      *    集計インデックス設定
           PERFORM  20013-IDXSET-SEC
      *
           IF  IDX1  NOT =  42
      *    点数加算
             PERFORM  900-TENSU-READ-SEC
      *
             COMPUTE  WRK-TENSU  =  ACCT-ZAITEN  *  ACCT-ZAIKAISU
             IF   SRY-SRYCD(1)  =  "630010002"
                 OR  TNS-TENSIKIBETU  =  7  OR  8
               COMPUTE  WRK-TENSU  =  WRK-TENSU  *  -1
             END-IF
             ADD      WRK-TENSU       TO     K144TMP-TENSU(IDX1)
      *
      *    回数加算
             ADD    ACCT-ZAIKAISU  TO     K144TMP-KAISU(IDX1)
           END-IF
      *
      *
           .
       200122-ETC-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算インデックス設定処理
      *****************************************************************
       20013-IDXSET-SEC                SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    ACCT-HOSPNUM        TO  AGECHK-HOSPNUM
           MOVE    ACCT-PTID           TO  AGECHK-PTID
           MOVE    K100LNK-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"       TO  AGECHK-SRYYMD(7:2)
           MOVE    COMB-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    ZERO    TO    IDX1
           EVALUATE  SRY-SRYKBN
             WHEN  "11" THRU "14"
               EVALUATE  SRY-SRYSYUKBN
                 WHEN  "110"
                   MOVE     PTINF-BIRTHDAY(1:4)  TO  WRK-YY1-G
                   MOVE     K100LNK-SRYYM(1:4)   TO  WRK-YY2-G
                   COMPUTE  WRK-NENREI   =  WRK-YY2  -  WRK-YY1
                   IF  PTINF-BIRTHDAY(5:2) > K100LNK-SRYYM(5:2)
                     SUBTRACT  1     FROM WRK-NENREI
                   END-IF
                   IF  COMB-KOH1HKNNUM      =     "027"
                       OR   COMB-HKNNUM  =  "039"
                       OR   COMB-HKNNUM  =  "040"
                     MOVE      4    TO    IDX1
                   ELSE
                     IF  WRK-NENREI   >= 70
                       MOVE    3    TO    IDX1
                     ELSE
                       IF   K100LNK-SRYYM < "200804"
                         IF  WRK-NENREI  <  3
                           MOVE    1    TO    IDX1
                         ELSE
                           MOVE    2    TO    IDX1
                         END-IF
                       ELSE
                         IF  AGECHK-SYUGAKUKBN   =  1
                           MOVE    1    TO    IDX1
                         ELSE
                           MOVE    2    TO    IDX1
                         END-IF
                       END-IF
                     END-IF
                   END-IF
                 WHEN  "120"
                   MOVE    PTINF-BIRTHDAY(1:4)  TO  WRK-YY1-G
                   MOVE    K100LNK-SRYYM(1:4)  TO  WRK-YY2-G
                   COMPUTE  WRK-NENREI   =  WRK-YY2  -  WRK-YY1
                   IF   PTINF-BIRTHDAY(5:2) > K100LNK-SRYYM(5:2)
                     SUBTRACT  1    FROM  WRK-NENREI
                   END-IF
                   IF   COMB-KOH1HKNNUM      =  "027"
                       OR   COMB-HKNNUM  =  "039"
                       OR   COMB-HKNNUM  =  "040"
                     MOVE      8    TO    IDX1
                   ELSE
                     IF   WRK-NENREI   >= 70
                       MOVE    7    TO    IDX1
                     ELSE
                       IF   K100LNK-SRYYM < "200804"
                         IF   WRK-NENREI  <  3
                           MOVE    5    TO    IDX1
                         ELSE
                           MOVE    6    TO    IDX1
                         END-IF
                       ELSE
                         IF  AGECHK-SYUGAKUKBN   =  1
                           MOVE    5    TO    IDX1
                         ELSE
                           MOVE    6    TO    IDX1
                         END-IF
                       END-IF
                     END-IF
                   END-IF
                 WHEN "130"
                     MOVE    9     TO    IDX1
                 WHEN OTHER
                     MOVE    10    TO    IDX1
               END-EVALUATE
             WHEN  "21" THRU "27"
               EVALUATE  SRY-SRYSYUKBN
                 WHEN  "210" THRU "213"
                 WHEN  "290" THRU "292"
                   MOVE    15     TO    IDX1
                 WHEN  "220" THRU "223"
                   MOVE    16     TO    IDX1
                 WHEN  "230" THRU "233"
                   MOVE    17     TO    IDX1
                 WHEN  "240" THRU "242"
                   MOVE    18     TO    IDX1
                 WHEN  "250"
                   MOVE    19     TO    IDX1
                 WHEN  "260"
                   MOVE    20     TO    IDX1
                 WHEN  "270"
                   MOVE    21     TO    IDX1
               END-EVALUATE
             WHEN  "31" THRU "34"
               EVALUATE  SRY-SRYSYUKBN
                 WHEN  "310" THRU "311"
                   MOVE   23      TO    IDX1
                 WHEN  "320" THRU "321"
                   MOVE   24      TO    IDX1
                 WHEN  "330" THRU "331"
                 WHEN  "340"
                   MOVE   25      TO    IDX1
               END-EVALUATE
             WHEN "40"
                 MOVE     27      TO    IDX1
             WHEN "50" THRU "54"
               EVALUATE  SRY-SRYSYUKBN
                 WHEN  "500" THRU "502"
                   MOVE   28      TO    IDX1
                 WHEN "510"
                   MOVE   29      TO    IDX1
                 WHEN  "540" THRU "542"
                   MOVE   30      TO    IDX1
               END-EVALUATE
             WHEN "70"
               MOVE     36      TO    IDX1
             WHEN "80"
               EVALUATE  SRY-SRYSYUKBN
                 WHEN  "820"
                   MOVE   37      TO    IDX1
                 WHEN  "800"
                 WHEN  "810"
                   MOVE   38      TO    IDX1
                 WHEN  "840"
                 WHEN  "830"
                 WHEN  "850"
                   MOVE   39      TO    IDX1
               END-EVALUATE
      *
             WHEN  "90"
             WHEN  "92"
             WHEN  "97"
               EVALUATE SRY-SRYSYUKBN
                 WHEN  "900"
                 WHEN  "920"
                   MOVE   42      TO    IDX1
                 WHEN  "970"
                 WHEN  "971"
                   MOVE   41      TO    IDX1
               END-EVALUATE
             WHEN  "95"  THRU  "96"
             MOVE     53        TO    IDX1
      *
           END-EVALUATE
      *
      *    診療種別がスペースだったら診療区分で分類する
           IF   IDX1   =      ZERO
             EVALUATE  SRY-SRYKBN
               WHEN "11"
                 MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                  MOVE    K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                  COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                  IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                      SUBTRACT  1       FROM  WRK-NENREI
                  END-IF
                  IF  COMB-KOH1HKNNUM    =  "027"
                      OR   COMB-HKNNUM  =  "039"
                      OR   COMB-HKNNUM  =  "040"
                    MOVE      4          TO    IDX1
                  ELSE
                    IF  WRK-NENREI   >=    70
                      MOVE    3      TO    IDX1
                    ELSE
                      IF K100LNK-SRYYM < "200804"
                        IF  WRK-NENREI  <  3
                          MOVE    1    TO    IDX1
                        ELSE
                          MOVE    2    TO    IDX1
                        END-IF
                      ELSE
                        IF  AGECHK-SYUGAKUKBN   =  1
                          MOVE    1    TO    IDX1
                        ELSE
                          MOVE    2    TO    IDX1
                        END-IF
                      END-IF
                    END-IF
                  END-IF
               WHEN "12"
                 MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                 MOVE    K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                 COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                 IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                   SUBTRACT       1     FROM  WRK-NENREI
                 END-IF
                 IF  COMB-KOH1HKNNUM    = "027"
                     OR   COMB-HKNNUM  =  "039"
                     OR   COMB-HKNNUM  =  "040"
                   MOVE    8      TO    IDX1
                 ELSE
                   IF  WRK-NENREI   >= 70
                     MOVE    7      TO    IDX1
                   ELSE
                     IF K100LNK-SRYYM < "200804"
                       IF  WRK-NENREI  <  3
                         MOVE    5    TO    IDX1
                       ELSE
                         MOVE    6    TO    IDX1
                       END-IF
                     ELSE
                       IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE    5    TO    IDX1
                       ELSE
                         MOVE    6    TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               WHEN  "13"
                 MOVE     9         TO    IDX1
               WHEN  "14"
                 MOVE     13        TO    IDX1
               WHEN  "21"
                 MOVE     15        TO    IDX1
               WHEN  "22"
                 MOVE     16        TO    IDX1
               WHEN  "23"
                 MOVE     17        TO    IDX1
               WHEN  "24"
                 MOVE     18        TO    IDX1
               WHEN  "25"
                 MOVE     19        TO    IDX1
               WHEN  "26"
                 MOVE     20        TO    IDX1
               WHEN  "27"
                 MOVE     21        TO    IDX1
               WHEN  "31"
                 MOVE     23        TO    IDX1
               WHEN  "32"
                 MOVE     24        TO    IDX1
               WHEN  "33"
                 MOVE     25        TO    IDX1
               WHEN  "40"
                 MOVE     27        TO    IDX1
               WHEN  "50"
                 MOVE     28        TO    IDX1
               WHEN  "51"
                 MOVE     29        TO    IDX1
               WHEN  "54"
                 MOVE     30        TO    IDX1
               WHEN  "70"
                 MOVE     36        TO    IDX1
               WHEN  "80"
               WHEN  "93"
                 MOVE     37        TO    IDX1
      *
               WHEN  "95"
               WHEN  "96"
                 MOVE     53        TO    IDX1
               WHEN  "97"
                 MOVE     41        TO    IDX1
               WHEN  "90"
                 MOVE     42        TO    IDX1
               WHEN  "92"
                 MOVE     42        TO    IDX1
               WHEN  OTHER
                 MOVE     39        TO    IDX1
      *
             END-EVALUATE
           END-IF
      *
           .
       20013-IDXSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算インデックス設定処理(点数マスタの診療区分と診療種別により判定)
      *****************************************************************
       20013-IDXSET2-SEC                SECTION.
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    ACCT-HOSPNUM        TO  AGECHK-HOSPNUM
           MOVE    ACCT-PTID           TO  AGECHK-PTID
           MOVE    K100LNK-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    COMB-HKNCOMBINUM    TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
           MOVE      ZERO    TO    IDX1
           EVALUATE  TNS-SRYKBN
             WHEN  "11" THRU "14"
               EVALUATE  TNS-SRYSYUKBN
               WHEN  "110"
                 MOVE     PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                 MOVE     K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                 COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                 IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                   SUBTRACT  1     FROM WRK-NENREI
                 END-IF
                 IF  COMB-KOH1HKNNUM      =     "027"
                     OR   COMB-HKNNUM  =  "039"
                     OR   COMB-HKNNUM  =  "040"
                   MOVE      4    TO    IDX1
                 ELSE
                   IF  WRK-NENREI   >= 70
                     MOVE    3    TO    IDX1
                   ELSE
                     IF K100LNK-SRYYM < "200804"
                       IF  WRK-NENREI  <  3
                         MOVE    1    TO    IDX1
                       ELSE
                         MOVE    2    TO    IDX1
                       END-IF
                     ELSE
                       IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE    1    TO    IDX1
                       ELSE
                         MOVE    2    TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               WHEN  "120"
                   MOVE    PTINF-BIRTHDAY(1:4)   TO    WRK-YY1-G
                   MOVE    K100LNK-SRYYM(1:4)    TO    WRK-YY2-G
                   COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                   IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                     SUBTRACT  1    FROM  WRK-NENREI
                   END-IF
                   IF  COMB-KOH1HKNNUM  =  "027"
                       OR   COMB-HKNNUM  =  "039"
                       OR   COMB-HKNNUM  =  "040"
                     MOVE      8    TO    IDX1
                   ELSE
                     IF  WRK-NENREI   >=  70
                       MOVE    7    TO    IDX1
                     ELSE
                       IF   K100LNK-SRYYM < "200804"
                         IF   WRK-NENREI  <  3
                           MOVE    5    TO    IDX1
                         ELSE
                           MOVE    6    TO    IDX1
                         END-IF
                       ELSE
                         IF  AGECHK-SYUGAKUKBN   =  1
                           MOVE    5    TO    IDX1
                         ELSE
                           MOVE    6    TO    IDX1
                         END-IF
                       END-IF
                     END-IF
                   END-IF
               WHEN  "130"
                   MOVE    9     TO    IDX1
               WHEN  OTHER
                   MOVE    10    TO    IDX1
               END-EVALUATE
             WHEN  "21" THRU "27"
               EVALUATE  TNS-SRYSYUKBN
                 WHEN  "210" THRU "213"
                 WHEN  "290" THRU "292"
                   MOVE    15     TO    IDX1
                 WHEN  "220" THRU "223"
                   MOVE    16     TO    IDX1
                 WHEN  "230" THRU "233"
                   MOVE    17     TO    IDX1
                 WHEN  "240" THRU "242"
                   MOVE    18     TO    IDX1
                 WHEN  "250"
                   MOVE    19     TO    IDX1
                 WHEN  "260"
                   MOVE    20     TO    IDX1
                 WHEN  "270"
                   MOVE    21     TO    IDX1
               END-EVALUATE
             WHEN  "31" THRU "34"
               EVALUATE  TNS-SRYSYUKBN
               WHEN "310" THRU "311"
                 MOVE   23      TO    IDX1
               WHEN "320" THRU "321"
                 MOVE   24      TO    IDX1
               WHEN "330" THRU "331"
               WHEN "340"
                 MOVE   25      TO    IDX1
               END-EVALUATE
             WHEN  "40"
               MOVE     27      TO    IDX1
             WHEN  "50" THRU "54"
               EVALUATE  TNS-SRYSYUKBN
                 WHEN "500" THRU "502"
                   MOVE   28      TO    IDX1
                 WHEN "510"
                   MOVE   29      TO    IDX1
                 WHEN "540" THRU "542"
                   MOVE   30      TO    IDX1
               END-EVALUATE
             WHEN  "70"
               MOVE     36      TO    IDX1
             WHEN  "80"
               EVALUATE  TNS-SRYSYUKBN
                 WHEN  "820"
                   MOVE   37      TO    IDX1
                 WHEN  "800"
                 WHEN  "810"
                   MOVE   38      TO    IDX1
                 WHEN  "840"
                 WHEN  "830"
                 WHEN  "850"
                   MOVE   39      TO    IDX1
               END-EVALUATE
      *
             WHEN  "90"
             WHEN  "92"
             WHEN  "97"
               EVALUATE TNS-SRYSYUKBN
                 WHEN  "900"
                 WHEN  "920"
                   MOVE   42      TO    IDX1
                 WHEN  "970"
                 WHEN  "971"
                   MOVE   41      TO    IDX1
               END-EVALUATE
      *
           END-EVALUATE
      *
      *    診療種別がスペースだったら診療区分で分類する
           IF   IDX1   =      ZERO
             EVALUATE  TNS-SRYKBN
               WHEN  "11"
                 MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                 MOVE    K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                 COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                 IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                   SUBTRACT  1       FROM  WRK-NENREI
                 END-IF
                 IF  COMB-KOH1HKNNUM    =  "027"
                     OR   COMB-HKNNUM  =  "039"
                     OR   COMB-HKNNUM  =  "040"
                   MOVE      4          TO    IDX1
                 ELSE
                   IF  WRK-NENREI   >=    70
                     MOVE    3      TO    IDX1
                   ELSE
                     IF K100LNK-SRYYM < "200804"
                       IF  WRK-NENREI  <  3
                         MOVE    1    TO    IDX1
                       ELSE
                         MOVE    2    TO    IDX1
                       END-IF
                     ELSE
                       IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE    1    TO    IDX1
                       ELSE
                         MOVE    2    TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               WHEN  "12"
                 MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                 MOVE    K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                 COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                 IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                   SUBTRACT       1     FROM  WRK-NENREI
                 END-IF
                 IF  COMB-KOH1HKNNUM    = "027"
                     OR   COMB-HKNNUM  =  "039"
                     OR   COMB-HKNNUM  =  "040"
                   MOVE    8      TO    IDX1
                 ELSE
                   IF   WRK-NENREI   >= 70
                     MOVE    7      TO    IDX1
                   ELSE
                     IF K100LNK-SRYYM < "200804"
                       IF  WRK-NENREI  <  3
                         MOVE    5    TO    IDX1
                       ELSE
                         MOVE    6    TO    IDX1
                       END-IF
                     ELSE
                       IF  AGECHK-SYUGAKUKBN   =  1
                         MOVE    5    TO    IDX1
                       ELSE
                         MOVE    6    TO    IDX1
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               WHEN  "13"
                 MOVE     9         TO    IDX1
               WHEN  "14"
                 MOVE     13        TO    IDX1
               WHEN  "21"
                 MOVE     15        TO    IDX1
               WHEN  "22"
                 MOVE     16        TO    IDX1
               WHEN  "23"
                 MOVE     17        TO    IDX1
               WHEN  "24"
                 MOVE     18        TO    IDX1
               WHEN  "25"
                 MOVE     19        TO    IDX1
               WHEN  "26"
                 MOVE     20        TO    IDX1
               WHEN  "27"
                 MOVE     21        TO    IDX1
               WHEN  "31"
                 MOVE     23        TO    IDX1
               WHEN  "32"
                 MOVE     24        TO    IDX1
               WHEN  "33"
                 MOVE     25        TO    IDX1
               WHEN  "40"
                 MOVE     27        TO    IDX1
               WHEN  "50"
                 MOVE     28        TO    IDX1
               WHEN  "51"
                 MOVE     29        TO    IDX1
               WHEN  "54"
                 MOVE     30        TO    IDX1
               WHEN  "70"
                 MOVE     36        TO    IDX1
               WHEN  "80"
               WHEN  "93"
                 MOVE     37        TO    IDX1
               WHEN  "97"
                 MOVE     41        TO    IDX1
               WHEN  "90"
                 MOVE     42        TO    IDX1
               WHEN  "92"
                 MOVE     42        TO    IDX1
      *
               WHEN OTHER
                 EVALUATE TNS-SRYCD
                   WHEN  "610000000"  THRU  "619999999"
      *    内服薬
                     MOVE     15   TO    IDX1
                   WHEN  "660000000"  THRU  "669999999"
      *    外用薬
                     MOVE     17   TO    IDX1
                   WHEN  OTHER
                     MOVE     39   TO    IDX1
                 END-EVALUATE
             END-EVALUATE
           END-IF
      *
           .
       20013-IDXSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN    INPUT       K144T-FILE
           MOVE    ZERO    TO  FLG-PRINT
           INITIALIZE          K144TMP-REC
      *
      *    一時ファイル読込
           PERFORM 900-TMP-READ-SEC
           INITIALIZE                       WRK-K144ALL-AREA
           MOVE    ZERO                TO   IDX2
           MOVE    K144TMP-NYUGAIKBN   TO   WRK-TMP-NYUGAIKBN
      *    見出し編集
           PERFORM  310-HEAD-HEN-SEC
      *
           PERFORM  UNTIL  FLG-PRINT   =    1
      *
             IF   K144TMP-NYUGAIKBN   NOT =   WRK-TMP-NYUGAIKBN
      *    全年齢合計明細の印字
      *    印刷処理
               IF   IDX2  >=   6
      *    印刷処理
                 PERFORM  330-PRINT-OUT-SEC
                 MOVE  ZERO         TO  IDX2
               END-IF
      *    全年齢明細編集
               ADD      1              TO  IDX2
               PERFORM  320-BODY-HEN2-SEC
      *    印刷処理
               PERFORM  330-PRINT-OUT-SEC
               INITIALIZE                  WRK-K144ALL-AREA
               MOVE  ZERO              TO  IDX2
               MOVE  K144TMP-NYUGAIKBN TO  WRK-TMP-NYUGAIKBN
      *    見出し編集
               PERFORM  310-HEAD-HEN-SEC
             END-IF
      *
      *    １頁内で年齢のMAXに達している場合
             IF   IDX2  >=     6
      *    印刷処理
               PERFORM  330-PRINT-OUT-SEC
               MOVE  ZERO              TO  IDX2
      *    見出し編集
               PERFORM  310-HEAD-HEN-SEC
             END-IF
      *
      *    保険別明細編集
             ADD    1     TO   IDX2
             PERFORM  320-BODY-HEN-SEC
      *
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
      *    全年齢合計明細の印字
           IF   IDX2   >=   6
      *    印刷処理
             PERFORM  330-PRINT-OUT-SEC
             MOVE  ZERO         TO  IDX2
      *    見出し編集
             PERFORM  310-HEAD-HEN-SEC
           END-IF
      *    全年齢明細編集
           ADD      1              TO  IDX2
           PERFORM  320-BODY-HEN2-SEC
      *
           IF   CNT-K144TMP   =    ZERO
             MOVE   "対象データが存在しません"    TO  K100-JOBMSG
             MOVE   SPACE          TO  K100-NYUGAIKBN
           END-IF
      *
      *    印刷処理
           PERFORM  330-PRINT-OUT-SEC
      *
           CLOSE    K144T-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC                 SECTION.
      *
           INITIALIZE                     K100
      *
           MOVE   "(保険別)"      TO    K100-SUBTITLE
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    K100-PAGE
      *
           IF    K144TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   K100-NYUGAIKBN
             PERFORM  VARYING  IDX3  FROM 1  BY  1
                      UNTIL    IDX3  >    10
               MOVE  WRK-TJIHIN(IDX3) TO  K100-TJIHI(IDX3)
             END-PERFORM
           ELSE
             MOVE     "外来"        TO   K100-NYUGAIKBN
             PERFORM  VARYING  IDX3  FROM 1  BY  1
                      UNTIL    IDX3  >    10 
               MOVE  WRK-TJIHI(IDX3)  TO  K100-TJIHI(IDX3)
             END-PERFORM
           END-IF
      *
           MOVE    WRK-SRYYM        TO   K100-SRYYM
           MOVE    WRK-SEIYMD       TO   K100-SYSYMD
      *
           IF   K100LNK-SRYYM  <   "200804"
             MOVE   "(３歳未満)"    TO  K100-NENREIKBN(1)
             MOVE   "(老人)"        TO  K100-NENREIKBN2(1)
             MOVE   "(３歳未満)"    TO  K100-NENREIKBN(2)
             MOVE   "(老人)"        TO  K100-NENREIKBN2(2)
           ELSE
             MOVE   "(６歳)"        TO  K100-NENREIKBN(1)
             MOVE   "(後期高齢者)"       TO  K100-NENREIKBN2(1)
             MOVE   "(６歳)"        TO  K100-NENREIKBN(2)
             MOVE   "(後期高齢者)"       TO  K100-NENREIKBN2(2)
           END-IF
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<保険別明細>処理
      *****************************************************************
       320-BODY-HEN-SEC               SECTION.
      *
           MOVE    K144TMP-HKNINDEX     TO    IDX4
           MOVE    WRK-HKNNAME(IDX4)    TO    K100-TNAME(IDX2)
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >   56
      *
             EVALUATE IDX1
               WHEN  11
                 COMPUTE     K144TMP-TENSU(11)    =
                             K144TMP-TENSU(01)
                       +     K144TMP-TENSU(02)
                       +     K144TMP-TENSU(03)
                       +     K144TMP-TENSU(04)
                       +     K144TMP-TENSU(05)
                       +     K144TMP-TENSU(06)
                       +     K144TMP-TENSU(07)
                       +     K144TMP-TENSU(08)
                       +     K144TMP-TENSU(09)
                       +     K144TMP-TENSU(10)
                 COMPUTE     K144TMP-KAISU(11)    =
                             K144TMP-KAISU(01)
                       +     K144TMP-KAISU(02)
                       +     K144TMP-KAISU(03)
                       +     K144TMP-KAISU(04)
                       +     K144TMP-KAISU(05)
                       +     K144TMP-KAISU(06)
                       +     K144TMP-KAISU(07)
                       +     K144TMP-KAISU(08)
                       +     K144TMP-KAISU(09)
                       +     K144TMP-KAISU(10)
      *
               WHEN  22
                 COMPUTE     K144TMP-TENSU(22)    =
                             K144TMP-TENSU(15)
                       +     K144TMP-TENSU(16)
                       +     K144TMP-TENSU(17)
                       +     K144TMP-TENSU(18)
                       +     K144TMP-TENSU(19)
                       +     K144TMP-TENSU(20)
                       +     K144TMP-TENSU(21)
                 COMPUTE     K144TMP-KAISU(22)    =
                             K144TMP-KAISU(15)
                       +     K144TMP-KAISU(16)
                       +     K144TMP-KAISU(17)
                       +     K144TMP-KAISU(18)
                       +     K144TMP-KAISU(19)
                       +     K144TMP-KAISU(20)
                       +     K144TMP-KAISU(21)
               WHEN  26
                 COMPUTE     K144TMP-TENSU(26)    =
                             K144TMP-TENSU(23)
                       +     K144TMP-TENSU(24)
                       +     K144TMP-TENSU(25)
                 COMPUTE     K144TMP-KAISU(26)    =
                             K144TMP-KAISU(23)
                       +     K144TMP-KAISU(24)
                       +     K144TMP-KAISU(25)
               WHEN  31
                 COMPUTE     K144TMP-TENSU(31)    =
                             K144TMP-TENSU(28)
                       +     K144TMP-TENSU(29)
                       +     K144TMP-TENSU(30)
                 COMPUTE     K144TMP-KAISU(31)    =
                             K144TMP-KAISU(28)
                       +     K144TMP-KAISU(29)
                      +     K144TMP-KAISU(30)
               WHEN  40
                 COMPUTE     K144TMP-TENSU(40)    =
                             K144TMP-TENSU(37)
                       +     K144TMP-TENSU(38)
                       +     K144TMP-TENSU(39)
                 COMPUTE     K144TMP-KAISU(40)    =
                             K144TMP-KAISU(37)
                       +     K144TMP-KAISU(38)
                       +     K144TMP-KAISU(39)
               WHEN  55
                 COMPUTE     K144TMP-TENSU(55)    =
                             K144TMP-TENSU(43)
                       +     K144TMP-TENSU(44)
                       +     K144TMP-TENSU(45)
                       +     K144TMP-TENSU(46)
                       +     K144TMP-TENSU(47)
                       +     K144TMP-TENSU(48)
                       +     K144TMP-TENSU(49)
                       +     K144TMP-TENSU(50)
                       +     K144TMP-TENSU(51)
                       +     K144TMP-TENSU(52)
                       +     K144TMP-TENSU(53)
                 COMPUTE     K144TMP-KAISU(55)    =
                             K144TMP-KAISU(43)
                       +     K144TMP-KAISU(44)
                       +     K144TMP-KAISU(45)
                       +     K144TMP-KAISU(46)
                       +     K144TMP-KAISU(47)
                       +     K144TMP-KAISU(48)
                       +     K144TMP-KAISU(49)
                       +     K144TMP-KAISU(50)
                       +     K144TMP-KAISU(51)
                       +     K144TMP-KAISU(52)
                       +     K144TMP-KAISU(53)
               WHEN  56
                 COMPUTE     K144TMP-TENSU(56)    =
                             K144TMP-TENSU(11)
                       +     K144TMP-TENSU(14)
                       +     K144TMP-TENSU(22)
                       +     K144TMP-TENSU(26)
                       +     K144TMP-TENSU(27)
                       +     K144TMP-TENSU(31)
                       +     K144TMP-TENSU(35)
                       +     K144TMP-TENSU(36)
                       +     K144TMP-TENSU(40)
                       +     K144TMP-TENSU(42)
                 COMPUTE     K144TMP-KAISU(56)    =
                             K144TMP-KAISU(11)
                       +     K144TMP-KAISU(14)
                       +     K144TMP-KAISU(22)
                       +     K144TMP-KAISU(26)
                       +     K144TMP-KAISU(27)
                       +     K144TMP-KAISU(31)
                       +     K144TMP-KAISU(35)
                       +     K144TMP-KAISU(36)
                       +     K144TMP-KAISU(40)
                       +     K144TMP-KAISU(42)
             END-EVALUATE
             MOVE     K144TMP-TENSU(IDX1) TO  WRK-TENSU-HEN
             MOVE     WRK-TENSU-HEN       TO  K100-TENSU(IDX2 IDX1)
      *
             MOVE     K144TMP-KAISU(IDX1) TO  WRK-KAISU-HEN
             MOVE     WRK-KAISU-HEN       TO  K100-KAISU(IDX2 IDX1)
      *
      *    全年齢加算エリアに加算する。
      *
             COMPUTE  WRK-K144ALL-TENSU(IDX1) =
                          WRK-K144ALL-TENSU(IDX1)
                      +   K144TMP-TENSU(IDX1)
             COMPUTE  WRK-K144ALL-KAISU(IDX1) =
                          WRK-K144ALL-KAISU(IDX1)
                      +   K144TMP-KAISU(IDX1)
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<全年齢明細>処理
      *****************************************************************
       320-BODY-HEN2-SEC               SECTION.
      *
           MOVE       "合計"          TO    K100-TNAME(IDX2)
      *
           PERFORM  VARYING   IDX1    FROM  1  BY  1
                    UNTIL     IDX1    >     56
      *
             MOVE    WRK-K144ALL-TENSU(IDX1)
                                         TO  WRK-TENSU-HEN
             MOVE    WRK-TENSU-HEN       TO  K100-TENSU(IDX2 IDX1)
      *
             MOVE    WRK-K144ALL-KAISU(IDX1)
                                         TO  WRK-KAISU-HEN
             MOVE    WRK-KAISU-HEN       TO  K100-KAISU(IDX2 IDX1)
      *
           END-PERFORM
      *
           .
       320-BODY-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<印刷>処理
      *****************************************************************
       330-PRINT-OUT-SEC         SECTION.
      *
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                   TO  SPRT-MODE
           MOVE    K100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    K100LNK-PRTKANRI-TBL-KEY
                                           TO  SPRT-TBL-KEY
           MOVE    K100LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    K100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    K100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    K100LNK-PRTKANRI-SHELLID
                                           TO  SPRT-SHELLID
           MOVE    K100LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SPRT-SHORI-RENNUM
           MOVE    K100LNK-PRTKANRI-PRIORITY
                                           TO  SPRT-PRIORITY
           MOVE   "A00000K100V02.red"             TO  SPRT-PRTID
           MOVE    "(保険別)診療行為区分別集計表"
                                           TO  SPRT-TITLE
           MOVE    K100                    TO  SPRT-PRTDATA
           MOVE    K100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    K100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    K100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
             CONTINUE
           END-IF
      *
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    K100LNK-PRTKANRI-SKYYMD
                                       TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC     =   ZERO
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
           MOVE    "key10"             TO  MCP-PATHNAME
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
      *    管理マスタ読み込み(自費情報)
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI2
             ELSE
               MOVE    1               TO  FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1               TO  FLG-SYSKANRI2
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
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    WRK-SYSYMD            TO  ORC-DBYMD
           MOVE    SYS-1005-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC           =   ZERO
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
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
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
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(職員情報)
      *****************************************************************
       821-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    WRK-SYSYMD            TO  ORC-DBYMD
           MOVE    SYS-1010-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
             PERFORM 821-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1           TO    FLG-SYSKANRI3
           END-IF
           .
       821-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(職員情報)
      *****************************************************************
       821-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO                   TO  FLG-SYSKANRI3
             MOVE    MCPDATA-REC  TO  SYS-1010-REC
           ELSE
             MOVE    1                           TO  FLG-SYSKANRI3
           END-IF
           .
       821-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       821-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       821-SYSKANRI-DBCLOSE-EXT.
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
           INITIALIZE                    STS-AREA-DAY
           INITIALIZE                    LNK-DAY7-AREA
           MOVE    "71"          TO      LNK-DAY7-IRAI
           MOVE    WRK-SYMD      TO      LNK-DAY7-YM
           CALL    "ORCSDAY"     USING   STS-AREA-DAY
                                         LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO      WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF   STS-RECEERR         =   ZERO
             CONTINUE
           ELSE
             OPEN    OUTPUT              RECEERR-FILE
      *
             MOVE    WRK-RECEERR         TO  RECEERR-REC
             WRITE   RECEERR-REC
      *
      *      ジョブ終了処理
             MOVE    "JBE"              TO  SJOBKANRI-MODE
             INITIALIZE                     JOBKANRI-REC
             MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
             MOVE    K100LNK-JOBID      TO  JOB-JOBID
             MOVE    K100LNK-SHELLID    TO  JOB-SHELLID
             MOVE    WRK-RECEERR        TO  JOB-YOBI
             MOVE    "9999"             TO  JOB-ERRCD
             CALL    "ORCSJOB"          USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
           END-IF
      *
           CLOSE   RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為受診対象患者一時ファイル読込(KEY READ)
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    K144T-FILE
             INVALID    KEY
               MOVE    1           TO  FLG-M01TMP
             NOT  INVALID  KEY
               MOVE    ZERO        TO  FLG-M01TMP
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為受診対象患者一時ファイル読込(NEXT READ)
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  K144T-FILE   NEXT
             AT  END
               MOVE  1   TO  FLG-PRINT
             NOT  AT  END
               ADD   1   TO  CNT-K144TMP
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    K100LNK-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_ptinf"         TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-PTINF
             ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
             END-IF
           ELSE
             MOVE    1                   TO  FLG-PTINF
             INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
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
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC   SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SRY-SRYCD(IDZ)      TO  TNS-SRYCD
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC  =   ZERO
             PERFORM 900-TENSU-READ-CHK-SEC
           ELSE
             MOVE    1                   TO  FLG-TENSU
             INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ対象期間チェック(複数レコードの読込み)
      *****************************************************************
       900-TENSU-READ-CHK-SEC   SECTION.
      *
           INITIALIZE GET-TENSU
           PERFORM    UNTIL    GET-TENSU   =   1
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_tensu"         TO  MCP-TABLE
             MOVE    "key21"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO  TNS-TENSU-REC
               MOVE    ZERO             TO  FLG-TENSU
             ELSE
               MOVE    1                TO  FLG-TENSU
               INITIALIZE               TNS-TENSU-REC
               MOVE    1                TO  GET-TENSU
               PERFORM 900-TENSU-READ2-SEC
             END-IF
             IF    TNS-YUKOSTYMD  <=      WRK-PARA-PRTLYMD
                 AND  TNS-YUKOEDYMD  >=      WRK-PARA-PRTLYMD
               MOVE  1                TO  GET-TENSU
             END-IF
           END-PERFORM
      *
           .
       900-TENSU-READ-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読込処理２(単一レコードの読込み)
      *****************************************************************
       900-TENSU-READ2-SEC   SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SRY-SRYCD(IDZ)      TO  TNS-SRYCD
           MOVE    WRK-TENYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-TENYMD          TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF    MCP-RC  =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_tensu"         TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC  =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
             ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
             END-IF
           ELSE
             MOVE    1                   TO  FLG-TENSU
             INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-TENSU-READ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査ソートマスタ読込処理
      *****************************************************************
       900-KENSA-READ-SEC   SECTION.
      *
           INITIALIZE                      KENSASORT-REC
           MOVE    SRY-SRYCD(IDZ)      TO  KENSASORT-SRYCD
           MOVE    WRK-KNSBUNRUI       TO  KENSASORT-KNSBUNRUI
           MOVE    KENSASORT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_kensasort"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_kensasort"     TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF   MCP-RC  =   ZERO
               MOVE    MCPDATA-REC         TO  KENSASORT-REC
               MOVE    ZERO                TO  FLG-KENSASORT
             ELSE
               MOVE    1                   TO  FLG-KENSASORT
               INITIALIZE                  KENSASORT-REC
             END-IF
           ELSE
             MOVE    1                   TO  FLG-KENSASORT
             INITIALIZE                  KENSASORT-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_kensasort"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
      *
           .
       900-KENSA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報SELECT(抽出)
      *****************************************************************
       900-SRYACT-SELECT-SEC         SECTION.
      *
           INITIALIZE                       SRYACT-REC
           MOVE    ACCT-HOSPNUM          TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO  SRY-PTID
           MOVE    ACCT-SRYKA           TO  SRY-SRYKA
           MOVE    ACCT-SRYYM           TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO  SRY-ZAINUM
           MOVE    SRYACT-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE  ZERO               TO FLG-SRYACT
             PERFORM   900-SRYACT-FETCH-SEC
           ELSE
               MOVE   1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報１件読み込み(抽出)
      *****************************************************************
       900-SRYACT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE   1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報クローズ(抽出)
      *****************************************************************
       900-SRYACT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報SELECT
      *****************************************************************
       900-SRYACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   SRYACCT-REC
           MOVE    K100LNK-HOSPNUM       TO  ACCT-HOSPNUM
           MOVE    K100LNK-SRYYM        TO  ACCT-SRYYM
           MOVE    WRK-NYUGAIKBN        TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC               =   ZERO
             MOVE    ZERO         TO  FLG-SRYACCT
             PERFORM    900-SRYACCT-FETCH-SEC
           ELSE
             MOVE   1             TO  FLG-SRYACCT
           END-IF
           .
       900-SRYACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報１件読み込み
      *****************************************************************
       900-SRYACCT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC   =   ZERO
             MOVE    MCPDATA-REC     TO  SRYACCT-REC
             MOVE    ZERO            TO  FLG-SRYACCT
      *    テスト患者は対象外
             MOVE    ACCT-PTID    TO  WRK-PTID
             PERFORM  900-PTINF-READ-SEC
             IF   FLG-PTINF  =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  900-SRYACCT-FETCH-SEC
             END-IF
      *    コメントは対象外
             IF    ACCT-SRYKBN    =    "99"
               GO    TO    900-SRYACCT-FETCH-SEC
             END-IF
             ADD    1               TO  CNT-DATA
           ELSE
             MOVE   1               TO  FLG-SRYACCT
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報クローズ
      *****************************************************************
       900-SRYACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報SELECT
      *****************************************************************
       900-NYUINACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   NYUINACCT-REC
           MOVE    K100LNK-HOSPNUM      TO  NACCT-HOSPNUM
           MOVE    K100LNK-SRYYM        TO  NACCT-SRYYM
           MOVE    WRK-NYUGAIKBN        TO  NACCT-NYUGAIKBN
           MOVE    NYUINACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC               =   ZERO
             MOVE    ZERO         TO  FLG-NSRYACCT
             PERFORM    900-NYUINACCT-FETCH-SEC
           ELSE
             MOVE   1             TO  FLG-NSRYACCT
           END-IF
           .
       900-NYUINACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報１件読み込み
      *****************************************************************
       900-NYUINACCT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC   =   ZERO
             MOVE    MCPDATA-REC     TO  NYUINACCT-REC
             MOVE    ZERO            TO  FLG-NSRYACCT
      *    テスト患者は対象外
             MOVE    NACCT-PTID    TO  WRK-PTID
             PERFORM  900-PTINF-READ-SEC
             IF      FLG-PTINF             =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  900-NYUINACCT-FETCH-SEC
             END-IF
      *    コメントは対象外
             IF    ACCT-SRYKBN    =    "99"
               GO    TO    900-NYUINACCT-FETCH-SEC
             END-IF
             ADD    1               TO  CNT-DATA
           ELSE
             MOVE   1               TO  FLG-NSRYACCT
           END-IF
           .
       900-NYUINACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報クローズ
      *****************************************************************
       900-NYUINACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SRYACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    収納情報SELECT
      *****************************************************************
       940-SYUNOU-SELECT-SEC         SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    K100LNK-HOSPNUM       TO  SYU-HOSPNUM
           MOVE    "1"                  TO  SYU-NYUGAIKBN
           MOVE    NACCT-PTID            TO  SYU-PTID
           MOVE    WRK-PARA-PRTSYMD     TO  SYU-SKYSTYMD
           MOVE    WRK-PARA-PRTLYMD     TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
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
           IF  MCP-RC              =   ZERO
             MOVE    MCPDATA-REC     TO  SYUNOU-REC
             MOVE    ZERO            TO  FLG-SYUNOU
             IF    SYU-DENPJTIKBN    =   "3"
               GO    TO    940-SYUNOU-FETCH-SEC
             END-IF
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
           MOVE  ACCT-HOSPNUM          TO  COMB-HOSPNUM
           MOVE  ACCT-PTID            TO  COMB-PTID
           MOVE  ACCT-HKNCOMBI        TO  COMB-HKNCOMBINUM
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
             MOVE  "tbl_hkncombi"   TO  MCP-TABLE
             MOVE  "key"            TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"      USING
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
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE    "tbl_hkncombi"   TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
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
       930-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE  ACCT-HOSPNUM          TO  HKN-HOSPNUM
           MOVE  WRK-HKNNUM           TO  HKN-HKNNUM
           MOVE  "00"                 TO  HKN-PAYKBN
           MOVE  ALL "9"              TO  HKN-TEKSTYMD
           MOVE  ZERO                 TO  HKN-TEKEDYMD
           MOVE  HKNNUM-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"  TO  MCP-FUNC
             MOVE  "tbl_hknnum" TO  MCP-TABLE
             MOVE  "key"        TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"  USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC  TO  HKNNUM-REC
               MOVE  ZERO         TO  FLG-HKNNUM
             ELSE
               INITIALIZE  HKNCOMBI-REC
               MOVE  1            TO  FLG-HKNNUM
             END-IF
           ELSE
             MOVE    1            TO  FLG-HKNNUM
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
           MOVE    "tbl_hknnum"     TO  MCP-TABLE
           MOVE    "key"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       930-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報SELECT
      *****************************************************************
       930-PTNYUINRRK-SELECT-SEC         SECTION.
      *
           INITIALIZE                       PTNYUINRRK-REC
           MOVE    SYU-HOSPNUM           TO  PTNYUINRRK-HOSPNUM
           MOVE    SYU-PTID             TO  PTNYUINRRK-PTID
           MOVE    WRK-PARA-PRTLYMD     TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-PARA-PRTSYMD     TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC   =   ZERO
             PERFORM 930-PTNYUINRRK-FETCH-SEC
           ELSE
             MOVE   1               TO  FLG-PTNYUINRRK
           END-IF
           .
       930-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報１件読み込み
      *****************************************************************
       930-PTNYUINRRK-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC  =   ZERO
             MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
             IF    PTNYUINRRK-TENSTUYMD  < WRK-PARA-PRTSYMD
               GO    TO    930-PTNYUINRRK-FETCH-SEC
             END-IF
             IF    PTNYUINRRK-TENNYUYMD  > WRK-PARA-PRTLYMD
               GO    TO    930-PTNYUINRRK-FETCH-SEC
             END-IF
      *
             MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
             MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           .
       930-PTNYUINRRK-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報クローズ
      *****************************************************************
       930-PTNYUINRRK-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       930-PTNYUINRRK-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理SELECT(室料差額用)
      *****************************************************************
       900-SYS-SELECT-SEC         SECTION.
      *
           MOVE    SYS-5113-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC         NOT  =   ZERO
               MOVE    1               TO  FLG-ROOM
           END-IF
           .
       900-SYS-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理１件読み込み(室料差額用)
      *****************************************************************
       900-SYS-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC     TO  SYS-5113-REC
             MOVE    ZERO            TO  FLG-ROOM
           ELSE
             MOVE    1               TO  FLG-ROOM
           END-IF
           .
       900-SYS-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    システム管理クローズ(室料差額用)
      *****************************************************************
       900-SYS-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SYS-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
             MOVE    "tbl_hkncombi"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
             IF    MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
             ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
             END-IF
           ELSE
             MOVE    1                   TO  FLG-HKNCOMBI
             INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
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
      *    診療会計情報SELECT
      *****************************************************************
       910-SRYACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   SRYACCT-REC2
           MOVE    SPA-HOSPNUM         TO  ACCT2-HOSPNUM
           MOVE    K100LNK-SRYYM       TO  ACCT2-SRYYM
           MOVE    SYU-NYUGAIKBN       TO  ACCT2-NYUGAIKBN
           MOVE    SYU-PTID            TO  ACCT2-PTID
           MOVE    SRYACCT-REC2        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF    MCP-RC          =   ZERO
             MOVE    ZERO         TO  FLG-SRYACCT2
             PERFORM    910-SRYACCT-FETCH-SEC
           ELSE
             MOVE   1             TO  FLG-SRYACCT2
           END-IF
           .
       900-SRYACCT-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報１件読み込み
      *****************************************************************
       910-SRYACCT-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =   ZERO
             MOVE    MCPDATA-REC     TO  SRYACCT-REC2
             MOVE    ZERO            TO  FLG-SRYACCT2
      *    テスト患者は対象外
             MOVE    ACCT2-PTID    TO  WRK-PTID
             PERFORM  900-PTINF-READ-SEC
             IF    FLG-PTINF             =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  900-SRYACCT-FETCH-SEC
             END-IF
      *    コメントは対象外
             IF    ACCT2-SRYKBN    =    "99"  OR  "98"
               GO    TO    910-SRYACCT-FETCH-SEC
             END-IF
             ADD    1               TO  CNT-DATA
           ELSE
             MOVE   1               TO  FLG-SRYACCT2
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報クローズ
      *****************************************************************
       910-SRYACCT-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-SRYACCT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    K100LNK-JOBID   TO  JOB-JOBID
           MOVE    K100LNK-SHELLID TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
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
