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
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     A00000KOHS.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 償還払い一覧表
      *  管理者            :
      *  作成日付    作業者        記述
      *  04/04/07    有村        新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    11/01/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *****************************************************************
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF100-FILE    ASSIGN    MF100PARA
                                 ORGANIZATION    IS  INDEXED
                                 ACCESS  MODE    IS  DYNAMIC
                                 RECORD  KEY     IS  MF100-KEY
                                 FILE    STATUS  IS  STS-MF100.
      *    ファイル出力用
           SELECT  A00000KOHS-FILE   ASSIGN   A00000KOHSCSV
                                     ORGANIZATION    IS  LINE
                                                         SEQUENTIAL
                                     FILE    STATUS  IS  STS-CSV.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE  ASSIGN  RECEERR
                                 FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC. 
           COPY  "A00000KOHSTMP.INC".
      *
      *    ＣＳＶファイル出力用
       FD  A00000KOHS-FILE.
       01  A00000KOHS-REC            PIC X(400).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *     レセプト明細ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *
      *帳票コピー句
           COPY    "A00000KOHSV02.INC".
      *
      *    ＣＳＶファイル出力用
       01  A00000KOHSCSV.
           03  FILLER               PIC X(11)   VALUE   "/home/orca/".
           03  CSV-DENPPRTNAME      PIC X(10)   VALUE   "A00000KOHS".
           03  FILLER               PIC X(01)   VALUE   "-".
           03  CSV-DENPPRTYMDHM     PIC 9(12).
           03  FILLER               PIC X(04)   VALUE   ".csv".
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100            PIC X(02).
           03  STS-RECE02           PIC X(02).
           03  STS-RECEERR          PIC X(02).
           03  STS-CITY             PIC X(02).
           03  STS-KOHI             PIC X(02).
           03  STS-KOHS             PIC X(01).
      *
           03  STS-CSV              PIC X(02).
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY           PIC 9(02).
               05  SYS-MM           PIC 9(02).
               05  SYS-DD           PIC 9(02).
           03  SYS-TIME             PIC 9(08).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END              PIC 9(01).
           03  FLG-PRINT            PIC 9(01).
           03  FLG-SYSKANRI         PIC 9(01).
           03  FLG-SYUNOU           PIC 9(01).
           03  FLG-HKNNUM           PIC 9(01).
           03  FLG-PTINF            PIC 9(01).
           03  FLG-PTKOHINF         PIC 9(01).
           03  FLG-PTHKNINF         PIC 9(01).
           03  FLG-RECE10           PIC 9(01).
           03  FLG-OK               PIC 9(01).
           03  FLG-KBN              PIC 9(01).
           03  FLG-KOHCHK           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                 PIC 9(04).
           03  IDX2                 PIC 9(04).
           03  IDX3                 PIC 9(04).
           03  IDX4                 PIC 9(04).
           03  IDX5                 PIC 9(04).
           03  IDX                  PIC 9(04).
           03  IDX6                 PIC 9(04).
           03  IDY                  PIC 9(04).
           03  IDZ                  PIC 9(04).
           03  IDXP                 PIC 9(02).
           03  IDX7                 PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID       PIC 9(07).
           03  WRK-PARA-SHELLID     PIC X(08).
           03  WRK-PARA-HOSPNUM     PIC 9(02).
      *--- 20110107 DELETE START ---------------------------------------
      *     03  RECEERR              PIC X(100).
      *--- 20110107 DELETE END   ---------------------------------------
           03  WRK-PARA-PAGE        PIC 9(10).
           03  WRK-PARA-SRYYM       PIC X(06).
           03  WRK-PARA-STUKBN      PIC X(01).
           03  WRK-PARA-PRTSYMD     PIC X(08).
           03  WRK-PARA-PRTLYMD     PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR   PIC X(200).
           03  WRK-SYMD.
               05  WRK-SYY          PIC 9(04).
               05  WRK-SMM          PIC 9(02).
               05  WRK-SDD          PIC 9(02).
           03  WRK-SRYYMWH          PIC X(16).
           03  WRK-HENYMDG          PIC X(22).
           03  WRK-HENYMDG1         PIC X(09).
           03  WRK-KOHNUM           PIC X(03).
           03  WRK-TAHO             PIC X(20).
      *
           03  WRK-CITYNAME1        PIC X(60).
           03  WRK-CITYNAME2        PIC X(80).
           03  WRK-MOJI1            PIC 9(02).
      *
           03  WRK-MAE-INPUT        PIC X(200).
           03  WRK-OUT-INPUT        PIC X(200).
      *
           03  FILE-NAME            PIC X(512).
           03  WRK-PATH             PIC X(64).
      *
           03  WRK-Z2               PIC ZZ.
           03  WRK-Z9               PIC Z,ZZZ,ZZZ. 
           03  WRK-SRYYMD           PIC 9(08). 
           03  FLG-KOHSKY           PIC 9(01).
           03  WRK-SRYKANAME        PIC X(10).
           03  WRK-REC-LENGTH       PIC 9(04).
           03  IDW                  PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA         PIC X(100).
               05  WRK-NAGASA       PIC 9(04).
           03  WRK-SKYMONEY         PIC 9(09).
           03  WRK-NISSU1           PIC 9(02).
           03  WRK-NISSU2           PIC 9(02).
      *
       01  PGOPTION-AREA.
           03  CITY-CITYNAME        PIC X(40).
           03  KOHS-KAIPAGE         PIC 9(01).
           03  KOHI-DATA-TBL        OCCURS 40.
               05  KOHI-KOHNUM      PIC X(03). 
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE10           PIC 9(06).
           03  CNT-MF100            PIC 9(06).
           03  CNT-PAGE             PIC 9(06).
      *    改ページKEY領域
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-KOHNUM     PIC X(03).
           03  KEY-OLD.
               05  KEY-O-KOHNUM     PIC X(03).
      *
      *--- 20110107 DELETE START ---------------------------------------
      *     COPY    "CPSHELLTBL.INC".
      *--- 20110107 DELETE END   ---------------------------------------
      *     
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY  "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY  "CPSK1005.INC".
      *
      *    請求管理
           COPY  "CPRCF010.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY  "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY  "CPPTINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY  "CPPTHKNINF.INC".
           COPY  "CPKOHSKY.INC"
                 REPLACING  //KOHSKY//
                 BY         //RECE02//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ"
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
           COPY    "COMMON-SPA".
           COPY    "CPORCSPRGOPTION.INC".
      *
      *****************************************************************
       LINKAGE  SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256). 
      *
      ****************************************************************
       PROCEDURE  DIVISION
                  USING COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC  SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
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
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  KEY-AREA
           INITIALIZE  SPA-AREA
      *
           ACCEPT    SYS-YMD       FROM    DATE
           ACCEPT    SYS-TIME      FROM    TIME
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
      *
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-SRYYM
                                               WRK-PARA-STUKBN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
           PERFORM  110-INI-FILE-SEC
      *
      *    ステップ開始処理
           MOVE    "STS"              TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID     TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID   TO  JOB-SHELLID
           MOVE    "A00000KOHS"       TO  JOB-PGID
           MOVE    "償還払公費一覧表"      TO   JOB-SHELLMSG
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
                                      SPA-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO   MF100PARA-HOSPNUM
           MOVE  "MF100ZZ"            TO   MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)
                                      TO   MF100PARA-TERMID
      *
      *    請求年月の和暦を取得
           MOVE    WRK-PARA-SRYYM     TO   WRK-SYMD (1:6)
           MOVE    "01"               TO   WRK-SYMD (7:2)
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)  TO   WRK-SRYYMWH
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-SRYYM     TO   WRK-SYMD(1:6)
           PERFORM   31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD           TO   WRK-PARA-PRTLYMD
      *
      *    対象年月初日
           MOVE    WRK-PARA-SRYYM     TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"               TO   WRK-PARA-PRTSYMD(7:2)
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE    "1001"             TO   SYS-1001-KANRICD
           MOVE    "*"                TO   SYS-1001-KBNCD
           MOVE    WRK-PARA-PRTSYMD   TO   SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-PRTLYMD   TO   SYS-1001-STYUKYMD
           MOVE    SPA-HOSPNUM        TO   SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC       TO   MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC        TO   SYS-1001-REC
           ELSE
             DISPLAY "*** A00000KOHS SYS 1001 ERR ***" 
             MOVE  1  TO  FLG-END
           END-IF 
      *
           OPEN  OUTPUT MF100-FILE
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    INIファイル読み込み処理
      *****************************************************************
       110-INI-FILE-SEC                SECTION.
      *
      *    プログラムオプション取込み
           INITIALIZE                        ORCSPRGOPTIONAREA
                                             PGOPTION-AREA
           MOVE    "A00000KOHS"      TO      ORCSPRGOPTION-PRGID
      *    自治体名
           MOVE    "JICHI"           TO      ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"   USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           IF  ORCSPRGOPTION-OT-PARA  NOT =  SPACE
             MOVE  ORCSPRGOPTION-OT-PARA  TO  CITY-CITYNAME
           END-IF
      *
      *    改ページ
           MOVE    "PAGE"            TO      ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"   USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           IF  ORCSPRGOPTION-OT-PARA  NOT =  SPACE
             MOVE  ORCSPRGOPTION-OT-PARA  TO  KOHS-KAIPAGE
           END-IF
      *
      *    償還払公費
           MOVE    "KOH"             TO      ORCSPRGOPTION-IN-DATA
           PERFORM  VARYING  IDXP  FROM  1  BY  1
                    UNTIL    IDXP  >    40
             MOVE   IDXP             TO      ORCSPRGOPTION-IN-DATA(4:2)
             CALL   "ORCSPRGOPTION"  USING   SPA-AREA
                                                 ORCSPRGOPTIONAREA
             IF  ORCSPRGOPTION-OT-PARA  NOT =  SPACE
               MOVE  ORCSPRGOPTION-OT-PARA  TO  KOHI-KOHNUM(IDXP)
             ELSE
               MOVE  41                     TO  IDXP
             END-IF
           END-PERFORM
      *
           .
       110-INI-FILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
      *    データ抽出処理
           PERFORM  2001-SYUKEI-SEC
      *
      *    データ印刷処理
           IF    CNT-MF100    >    ZERO
             PERFORM  2003-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ抽出処理
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
           OPEN     OUTPUT  MF100-FILE
           CLOSE            MF100-FILE
           OPEN     I-O     MF100-FILE
      *
           PERFORM  900-KOHSKY-SELECT-SEC
           PERFORM  UNTIL  FLG-KOHSKY  =  1
      *
             PERFORM  2002-HENSYU-SEC
             PERFORM  900-KOHSKY-FETCH-SEC
      *
           END-PERFORM
      *
           PERFORM  900-KOHSKY-CLOSE-SEC
      *
           CLOSE   MF100-FILE
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ編集処理
      *****************************************************************
       2002-HENSYU-SEC  SECTION.
      *
           INITIALIZE          FLG-KOHCHK
           PERFORM  20021-KOHCHK-SELECT-SEC
      *
           IF   FLG-KOHCHK  =  1
             PERFORM  900-SEIKYU-SELECT-SEC
             PERFORM  910-SYUNOU-SELECT-SEC
             PERFORM  UNTIL  FLG-SYUNOU  =  1
      *
               IF  SYU-DENPJTIKBN  NOT =  "3"
                 AND  (( RECE02-KOHNUM(IDX7 - 1)  =  SYU-KOH1HKNNUM
                 AND     SYU-KOH1HKNNUM       NOT =  SPACE )
                 OR    ( RECE02-KOHNUM(IDX7 - 1)  =  SYU-KOH2HKNNUM
                 AND     SYU-KOH2HKNNUM       NOT =  SPACE )
                 OR    ( RECE02-KOHNUM(IDX7 - 1)  =  SYU-KOH3HKNNUM
                 AND     SYU-KOH3HKNNUM       NOT =  SPACE )
                 OR    ( RECE02-KOHNUM(IDX7 - 1)  =  SYU-KOH4HKNNUM
                 AND     SYU-KOH4HKNNUM       NOT =  SPACE ))
      *
      *    データ設定
                 MOVE    RECE10-HOSPNUM          TO   MF100-HOSPNUM
                 MOVE    RECE10-PTNUM            TO   MF100-PTNUM
                 MOVE    RECE10-PTID             TO   MF100-PTID
                 MOVE    RECE10-SRYYM            TO   MF100-SRYYM
                 MOVE    RECE10-NYUGAIKBN        TO   MF100-NYUGAIKBN
                 MOVE    RECE10-RECEKA           TO   MF100-SRYKA
                 MOVE    RECE10-TEISYUTUSAKI     TO   MF100-TEISYUTUSAKI
                 MOVE    RECE10-RECESYUBETU      TO   MF100-RECESYUBETU
                 MOVE    RECE10-HKNJANUM         TO   MF100-HKNJANUM
                 MOVE    RECE10-TEKSTYMD         TO   MF100-TEKSTYMD
                 MOVE    PTINF-NAME              TO   MF100-NAME
                 MOVE    PTINF-KANANAME          TO   MF100-FURIGANA
                 MOVE    PTINF-SEX               TO   MF100-SEX
                 MOVE    RECE10-KIGO             TO   MF100-KIGO
                 MOVE    RECE10-NUM              TO   MF100-NUM
                 MOVE    RECE10-KYURATE          TO   MF100-KYURATE
                 MOVE    RECE10-RJNHKNNUM        TO   MF100-RJNHKNNUM
      *
                 IF  RECE10-NYUGAIKBN  =  "1"
      *
                   MOVE  SYU-SKYSTYMD(7:2)    TO  WRK-NISSU1
                   MOVE  SYU-SKYEDYMD(7:2)    TO  WRK-NISSU2
                   COMPUTE  WRK-NISSU1  =  (WRK-NISSU2  -  WRK-NISSU1)
                                        +  1
                   ADD    WRK-NISSU1          TO  MF100-JNISSU
                   ADD    SYU-TOTAL-HKNTEN    TO  MF100-TOTALTEN
      *
                   IF   SYU-KOH1HKNNUM  =  MF100-KOHNUM
      *    日数加算
                     ADD   SYU-KOH1SKJ-NISSU    TO  MF100-SKJKAISU
      *    請求額加算
                     ADD   SYU-KOH1SKJ-RYOYOHI  TO  MF100-SKJSKY
      *    標準負担額加算
                     ADD   SYU-KOH1SKJ-FTN      TO  MF100-SKJFTN
                   ELSE
                     IF  SYU-KOH2HKNNUM  =  MF100-KOHNUM
      *    日数加算
                       ADD   SYU-KOH2SKJ-NISSU    TO  MF100-SKJKAISU
      *    請求額加算
                       ADD   SYU-KOH2SKJ-RYOYOHI  TO  MF100-SKJSKY
      *    標準負担額加算
                       ADD   SYU-KOH2SKJ-FTN      TO  MF100-SKJFTN
                     ELSE
                       IF  SYU-KOH3HKNNUM  =  MF100-KOHNUM
      *    日数加算
                         ADD   SYU-KOH3SKJ-NISSU    TO  MF100-SKJKAISU
      *    請求額加算
                         ADD   SYU-KOH3SKJ-RYOYOHI  TO  MF100-SKJSKY
      *    標準負担額加算
                         ADD   SYU-KOH3SKJ-FTN      TO  MF100-SKJFTN
                       ELSE
                         IF  SYU-KOH4HKNNUM  =  MF100-KOHNUM
      *    日数加算
                           ADD   SYU-KOH4SKJ-NISSU    TO  MF100-SKJKAISU
      *    請求額加算
                           ADD   SYU-KOH4SKJ-RYOYOHI  TO  MF100-SKJSKY
      *    標準負担額加算
                           ADD   SYU-KOH4SKJ-FTN      TO  MF100-SKJFTN
                         ELSE
      *    日数加算
                           ADD   SYU-SYUSKJ-NISSU     TO  MF100-SKJKAISU
      *    請求額加算
                           ADD   SYU-SYUSKJ-RYOYOHI   TO  MF100-SKJSKY
      *    標準負担額加算
                           ADD   SYU-SYUSKJ-FTN       TO  MF100-SKJFTN
                         END-IF
                       END-IF
                     END-IF
                   END-IF
      *
                 ELSE
      *
                   ADD    1                        TO  MF100-JNISSU
                   ADD    SYU-TOTAL-HKNTEN         TO  MF100-TOTALTEN
                 END-IF
      *
      *    公費名取得
                 MOVE    MF100-KOHNUM              TO   WRK-KOHNUM
                 PERFORM  950-HKNNUM-SET-SEC
                 MOVE    HKN-TANSEIDONAME          TO   MF100-KOHNAME
      *
      *    他法公費情報設定
                 MOVE    1                         TO   IDX1
                 IF   RECE10-RJNHKNNUM  NOT =  SPACE
                   MOVE   "27"                     TO   MF100-TAHO(IDX1)
                   ADD    1                        TO   IDX1
                 END-IF
      *
                 IF   RECE10-ETC-KOHNUM(1)   =   "972"
                     OR  RECE10-ETC-KOHNUM(2)   =   "972"
                     OR  RECE10-ETC-KOHNUM(3)   =   "972"
                     OR  RECE10-ETC-KOHNUM(4)   =   "972"
                   MOVE  "長"                      TO   MF100-TAHO(IDX1)
                   ADD   1                         TO   IDX1
                 END-IF
      *
                 PERFORM  VARYING  IDX2   FROM    1   BY  1
                          UNTIL    IDX2   >   4
                          OR   IDX1   >   4
                          OR   RECE10-KOHNUM(IDX2)  =  SPACE
                   IF     RECE10-KOHNUM(IDX2)(1:1)  =  "0"
                     MOVE     RECE10-KOHNUM(IDX2)  TO   WRK-KOHNUM
                     PERFORM  950-HKNNUM-SET-SEC
                     MOVE     HKN-HBTNUM           TO   MF100-TAHO(IDX1)
                     ADD      1                    TO   IDX1
                   END-IF
                 END-PERFORM
      *
      *    収納情報より、本人負担額を集計
                 COMPUTE  WRK-SKYMONEY  =  SYU-SKYMONEY
                                        -  SYU-JIHI-TOTAL
                                        -  SYU-JIHI-TOTAL-TAX
                 ADD   WRK-SKYMONEY     TO   MF100-FTNMONEY
      *
      *    公費番号でソートする
                 MOVE     MF100-KOHNUM      TO   MF100-KOHNUM-KEY
      *
               END-IF
               PERFORM  910-SYUNOU-FETCH-SEC
             END-PERFORM
      *
             PERFORM  910-SYUNOU-CLOSE-SEC
             IF  FLG-KBN   =   ZERO
               WRITE    MF100-REC
               ADD     1        TO   CNT-MF100
             ELSE
               REWRITE  MF100-REC
             END-IF
           END-IF
      *
           .
       2002-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費チェック処理
      *****************************************************************
       20021-KOHCHK-SELECT-SEC             SECTION.
      *
      *    対象公費所持者判定
           INITIALIZE  MF100-REC
      *
           PERFORM  VARYING  IDX7   FROM    1   BY  1
                    UNTIL    IDX7   >   4
                    OR       RECE02-KOHNUM(IDX7)  =  SPACE
             IF  RECE02-KOHNUM(IDX7)  =  KOHI-KOHNUM(1)
               OR  KOHI-KOHNUM(2)    OR  KOHI-KOHNUM(3)
               OR  KOHI-KOHNUM(4)    OR  KOHI-KOHNUM(5)
               OR  KOHI-KOHNUM(6)    OR  KOHI-KOHNUM(7)
               OR  KOHI-KOHNUM(8)    OR  KOHI-KOHNUM(9)
               OR  KOHI-KOHNUM(10)   OR  KOHI-KOHNUM(11)
               OR  KOHI-KOHNUM(12)   OR  KOHI-KOHNUM(13)
               OR  KOHI-KOHNUM(14)   OR  KOHI-KOHNUM(15)
               OR  KOHI-KOHNUM(16)   OR  KOHI-KOHNUM(17)
               OR  KOHI-KOHNUM(18)   OR  KOHI-KOHNUM(19)
               OR  KOHI-KOHNUM(20)   OR  KOHI-KOHNUM(21)
               OR  KOHI-KOHNUM(22)   OR  KOHI-KOHNUM(23)
               OR  KOHI-KOHNUM(24)   OR  KOHI-KOHNUM(25)
               OR  KOHI-KOHNUM(26)   OR  KOHI-KOHNUM(27)
               OR  KOHI-KOHNUM(28)   OR  KOHI-KOHNUM(29)
               OR  KOHI-KOHNUM(30)   OR  KOHI-KOHNUM(31)
               OR  KOHI-KOHNUM(32)   OR  KOHI-KOHNUM(33)
               OR  KOHI-KOHNUM(34)   OR  KOHI-KOHNUM(35)
               OR  KOHI-KOHNUM(36)   OR  KOHI-KOHNUM(37)
               OR  KOHI-KOHNUM(38)   OR  KOHI-KOHNUM(39)
               OR  KOHI-KOHNUM(40)
               PERFORM  9001-CHK-SEC
             END-IF
           END-PERFORM
      *
           .
       20021-KOHCHK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ印刷処理
      *****************************************************************
       2003-PRINT-SEC  SECTION.
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO   FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM   900-TMP-READ-SEC
      *
           PERFORM   UNTIL  FLG-PRINT   =  1
      *
      *    帳票編集<見出し>処理
             PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
             PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
             PERFORM  330-TERM-HEN-SEC
      *
           END-PERFORM
           CLOSE    MF100-FILE
           IF   WRK-PARA-STUKBN  =  "1"
      *    CSV頭書き出力    
             PERFORM  210-CSV-HED-SEC
             OPEN     INPUT       MF100-FILE
             MOVE     ZERO        TO   FLG-PRINT
      *    一時ファイル読込
             PERFORM   900-TMP-READ-SEC
                        
             PERFORM   UNTIL  FLG-PRINT   =  1
      *    CSV書き出し
               PERFORM   210-CSV-WRITE-SEC
      *    一時ファイル読込
               PERFORM   900-TMP-READ-SEC
             END-PERFORM
      *    ファイルクローズ
             CLOSE  A00000KOHS-FILE
             CLOSE  MF100-FILE
           END-IF
      *
           .
       2003-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           ADD   1     TO  CNT-PAGE
           INITIALIZE  KOHS-DATA
      *
      *    自治体名
           INITIALIZE                       WRK-CITYNAME1
           STRING   CITY-CITYNAME           DELIMITED BY SPACE
                   "償還払公費患者一覧表"   DELIMITED BY SIZE
                   INTO                     WRK-CITYNAME1
           END-STRING
      *
      *    文字数計算
           MOVE  ZERO                      TO    WRK-MOJI1
           PERFORM  VARYING  IDX1  FROM  1  BY  2
                    UNTIL    WRK-CITYNAME1(IDX1:2)  =  SPACE
                    OR       IDX1  >     60
             ADD    2                      TO    WRK-MOJI1
           END-PERFORM
      *    診療年月分の文字数加算
           ADD      20                     TO    WRK-MOJI1
      *    空白のバイト数計算
           COMPUTE  IDX1   = ((80 - WRK-MOJI1) / 2) + 1
           STRING   WRK-CITYNAME1           DELIMITED BY SPACE
                    "（" WRK-SRYYMWH "）"   DELIMITED BY SIZE
                    INTO                    WRK-CITYNAME2
           END-STRING
           MOVE  WRK-CITYNAME2(1:WRK-MOJI1)
                                  TO   KOHS-CITYNAME(IDX1:WRK-MOJI1)
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE     KEY-NEW        TO    KEY-OLD
      *
           PERFORM  VARYING  IDX1  FROM  1   BY   1 
                    UNTIL    IDX1  >  15
                    OR    FLG-PRINT   =   1
                    OR    KEY-NEW  NOT =  KEY-OLD
      *    公費名
             MOVE   MF100-KOHNAME          TO   KOHS-KOHNAME(IDX1)
      *
      *    ID(患者番号)
             MOVE   MF100-PTNUM            TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             IF    WRK-OUT-INPUT(21:)  =  SPACE
               MOVE   WRK-OUT-INPUT          TO   KOHS-PTNUM(IDX1)
             ELSE
               MOVE   WRK-OUT-INPUT          TO   KOHS-PTNUM2(IDX1)
             END-IF
      *
      *    氏名
             MOVE   MF100-NAME             TO   KOHS-NAME(IDX1)
             MOVE   MF100-FURIGANA         TO   KOHS-FURIGANA(IDX1)
      *
      *    負担者番号
             MOVE   MF100-FTNJANUM         TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE   WRK-OUT-INPUT          TO   KOHS-FTNJANUM(IDX1)
      *
      *    受給者番号
             MOVE   MF100-JKYSNUM          TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE   WRK-OUT-INPUT          TO   KOHS-JKYSNUM(IDX1)
      *
      *    診療年月(請求年月と異なる時のみ)
             IF     MF100-SRYYM  NOT =   WRK-PARA-SRYYM
               MOVE    MF100-SRYYM         TO   WRK-SYMD (1:6)
               MOVE    "01"                TO   WRK-SYMD (7:2)
               PERFORM   31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG1        TO   WRK-MAE-INPUT
               PERFORM   2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT       TO   KOHS-SRYYM(IDX1)
             END-IF
      *
      *    性別
             EVALUATE  MF100-SEX
             WHEN  1
               MOVE    "男"                TO   KOHS-SEX(IDX1)
             WHEN  2
               MOVE    "女"                TO   KOHS-SEX(IDX1)
             END-EVALUATE
      *
      *    入外区分
             EVALUATE  MF100-NYUGAIKBN
             WHEN  "1"
               MOVE    "入"                TO   KOHS-NYUGAIKBN(IDX1)
             WHEN  "2"
               MOVE    "外"                TO   KOHS-NYUGAIKBN(IDX1)
             END-EVALUATE
      *
      *    保険者番号
             MOVE    MF100-HKNJANUM        TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-HKNJANUM(IDX1)
      *
      *    記号
             MOVE      MF100-KIGO          TO   KOHS-KIGO(IDX1)
      *
      *    番号
             MOVE      MF100-NUM           TO   KOHS-NUM(IDX1)
      *
      *    実日数
             MOVE    MF100-JNISSU          TO   WRK-Z2
             MOVE    WRK-Z2                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-NISSU(IDX1)
      *
      *    点数
             MOVE    MF100-TOTALTEN        TO   WRK-Z9
             MOVE    WRK-Z9                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-TENSU(IDX1)
      *
      *    本人負担額
             MOVE    MF100-FTNMONEY        TO   WRK-Z9
             MOVE    WRK-Z9                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-ITBFTN(IDX1)
      *
      *    食事療養費-日数
             MOVE    MF100-SKJKAISU        TO   WRK-Z2
             MOVE    WRK-Z2                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-SKJKAISU(IDX1)
      *    食事療養費-請求額
             MOVE    MF100-SKJSKY          TO   WRK-Z9
             MOVE    WRK-Z9                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-SKJSKY(IDX1)
      *    食事療養費-標準負担額
             MOVE    MF100-SKJFTN          TO   WRK-Z9
             MOVE    WRK-Z9                TO   WRK-MAE-INPUT
             PERFORM   2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT         TO   KOHS-SKJFTN(IDX1)
      *
      *    診療科名
             IF   MF100-SRYKA   NOT =   "00"
               INITIALIZE                  SYS-1005-REC
               MOVE    SPA-HOSPNUM         TO   SYS-1005-HOSPNUM
               MOVE    "1005"              TO   SYS-1005-KANRICD
               MOVE    MF100-SRYKA         TO   SYS-1005-KBNCD
               MOVE  WRK-PARA-PRTSYMD      TO   SYS-1005-EDYUKYMD
               MOVE  WRK-PARA-PRTLYMD      TO   SYS-1005-STYUKYMD
               MOVE    SYS-1005-REC        TO   MCPDATA-REC
               PERFORM 800-SYSKANRI-READ-SEC
               IF  FLG-SYSKANRI  =  ZERO
                 MOVE  MCPDATA-REC         TO   SYS-1005-REC
                 MOVE  SYS-1005-SRYKANAME  TO   KOHS-SRYKANAME(IDX1)
               END-IF
             END-IF
      *
      *    給付割合
             MOVE     MF100-KYURATE        TO   WRK-Z2
             MOVE     WRK-Z2               TO   WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT        TO   KOHS-KYURATE(IDX1)
      *
      *    他法公費
             INITIALIZE                         WRK-TAHO
             MOVE     MF100-TAHO(1)        TO   WRK-TAHO(1:2)
             PERFORM  VARYING    IDX2  FROM  2  BY  1
                      UNTIL   IDX2   >   4
                      OR    MF100-TAHO(IDX2)   =   SPACE
               STRING  WRK-TAHO            DELIMITED  BY  SPACE
                       ","                 DELIMITED  BY  SIZE
                       MF100-TAHO(IDX2)    DELIMITED  BY  SIZE
                       INTO                WRK-TAHO
               END-STRING
             END-PERFORM
             MOVE     WRK-TAHO             TO   WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT        TO   KOHS-TAHO(IDX1)
      *
             PERFORM  900-TMP-READ-SEC
      *      改ページフラグが０の時は公費番号で改ページしない
             IF  KOHS-KAIPAGE              =   ZERO
               MOVE     KEY-NEW            TO    KEY-OLD
             END-IF
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       330-TERM-HEN-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                   TO   SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      *
           MOVE   "A00000KOHSV02.red"         TO    SPRT-PRTID
           MOVE    "償還払公費患者一覧表"  TO    SPRT-TITLE
           MOVE    KOHS-DATA               TO    SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID     TO    SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO    SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM      TO    SPRT-PRTNM
      *
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN   =   ZERO
             CONTINUE
           END-IF
      *
           .
       330-TERM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
             AT  END
               MOVE    1                   TO   FLG-PRINT
             NOT  AT  END
               MOVE    MF100-KOHNUM-KEY    TO   KEY-N-KOHNUM
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC  SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF   STS-RECEERR  =  ZERO
             CONTINUE
           ELSE
             OPEN   OUTPUT  RECEERR-FILE
      *
             MOVE   WRK-RECEERR     TO   RECEERR-REC
             WRITE  RECEERR-REC
           END-IF
      *
           CLOSE  RECEERR-FILE
           .
       500-ERR-HENSYU-EXT.
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
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
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
           MOVE LNK-DAY7-KOYOMI     TO   WRK-SYMD
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
           MOVE    "2"                 TO   KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO   KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF   KANACHK-RC   =   ZERO
             MOVE   KANACHK-OUT-INPUT  TO   WRK-OUT-INPUT
           ELSE
               MOVE   WRK-MAE-INPUT    TO   WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理セレクト
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
           MOVE    SPA-HOSPNUM           TO   RECE10-HOSPNUM
           MOVE    RECE02-SRYYM          TO   RECE10-SRYYM
           MOVE    RECE02-NYUGAIKBN      TO   RECE10-NYUGAIKBN
           MOVE    RECE02-PTID           TO   RECE10-PTID
           MOVE    RECE02-RECEKA         TO   RECE10-RECEKA
           MOVE    RECE02-TEISYUTUSAKI   TO   RECE10-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU    TO   RECE10-RECESYUBETU
           MOVE    RECE02-HKNJANUM       TO   RECE10-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY    TO   RECE10-HOJOKBN-KEY
           MOVE    RECE02-TEKSTYMD       TO   RECE10-TEKSTYMD
      *
           MOVE    RECE10-REC          TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
           MOVE    "tbl_seikyu"        TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM 900-SEIKYU-READ-SEC
           ELSE
             MOVE    1                 TO   FLG-END
           END-IF
           PERFORM  900-SEIKYU-CLOSE-SEC
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
           MOVE    "tbl_seikyu"        TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO              TO   FLG-END
             MOVE    MCPDATA-REC       TO   RECE10-REC
      *
      *        テスト患者のときは読み飛ばす
             MOVE    SPACE             TO   PTINF-REC
             INITIALIZE                     PTINF-REC
             MOVE    RECE10-HOSPNUM    TO   PTINF-HOSPNUM
             MOVE    RECE10-PTID       TO   PTINF-PTID
             MOVE    PTINF-REC         TO   MCPDATA-REC
             PERFORM 800-PTINF-READ-SEC
             IF   FLG-PTINF   =   ZERO
                 AND   PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  900-SEIKYU-READ-SEC
             END-IF
      *
             ADD    1                  TO   CNT-RECE10
           ELSE
             MOVE    1                 TO   FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       900-SEIKYU-CLOSE-SEC  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      ******************************************************************
      *    公費請求マスタ読込
      ******************************************************************
       900-KOHSKY-SELECT-SEC            SECTION.
      *
           INITIALIZE                   RECE02-REC
           MOVE    SPA-HOSPNUM          TO   RECE02-HOSPNUM
           MOVE    WRK-PARA-SRYYM       TO   RECE02-SKYYM
           MOVE    RECE02-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_kohsky"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             PERFORM  900-KOHSKY-FETCH-SEC
           END-IF
           .
       900-KOHSKY-SELECT-EXT.
           EXIT.
      ******************************************************************
      *    公費請求マスタ
      ******************************************************************
       900-KOHSKY-FETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"            TO   MCP-FUNC
           MOVE    "tbl_kohsky"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC        TO   RECE02-REC
           ELSE
             MOVE    1                  TO   FLG-KOHSKY
             INITIALIZE                 RECE02-REC
           END-IF
           .
       900-KOHSKY-FETCH-EXT.
           EXIT.
      ******************************************************************
      *    公費請求マスタ
      ******************************************************************
       900-KOHSKY-CLOSE-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_kohsky"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    対象公費所持者判定処理
      *****************************************************************
       9001-CHK-SEC                    SECTION.
      *
           INITIALIZE  PTKOHINF-REC
      *
           MOVE    RECE02-HOSPNUM       TO   PTKOH-HOSPNUM
           MOVE    RECE02-PTID          TO   PTKOH-PTID
           MOVE    RECE02-KOHID(IDX7)   TO   PTKOH-KOHID
           MOVE    PTKOHINF-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_ptkohinf"       TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_ptkohinf"     TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             PERFORM  935-PTKOHINF-READ-SEC
      *
                 IF  (PTKOH-TEKSTYMD(1:6)  <=  WRK-PARA-SRYYM
                     AND   PTKOH-TEKEDYMD(1:6)  >=  WRK-PARA-SRYYM)
                     AND   PTKOH-SAKJOKBN   NOT =  "1"
                   MOVE  PTKOH-KOHNUM        TO   MF100-KOHNUM
                   MOVE  PTKOH-FTNJANUM      TO   MF100-FTNJANUM
                   MOVE  PTKOH-JKYSNUM       TO   MF100-JKYSNUM
                 END-IF
                 MOVE    1     TO   FLG-KOHCHK
           ELSE
             DISPLAY  "PTKOHINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"          TO   MCP-FUNC
           MOVE    "tbl_ptkohinf"           TO   MCP-TABLE
           MOVE    "key"                    TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *
           .
       9001-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       935-PTKOHINF-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"      TO   MCP-FUNC
           CALL    "ORCDBMAIN"    USING
                                  MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE  MCPDATA-REC    TO   PTKOHINF-REC
             MOVE  ZERO           TO   FLG-PTKOHINF
           ELSE
             INITIALIZE   PTKOHINF-REC
             MOVE    1            TO   FLG-PTKOHINF
           END-IF
      *
           .
       935-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
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
               MOVE    ZERO             TO   FLG-PTINF
               MOVE    MCPDATA-REC      TO   PTINF-REC
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
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      **    収納情報検索
      ******************************************************************
       910-SYUNOU-SELECT-SEC             SECTION.
      *
           INITIALIZE                    SYUNOU-REC
      *
           MOVE    RECE10-HOSPNUM    TO  SYU-HOSPNUM
           MOVE    RECE10-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE    RECE10-PTID       TO  SYU-PTID
           MOVE    RECE10-SRYYM      TO  SYU-SKYSTYMD(1:6)
           MOVE    "01"              TO  SYU-SKYSTYMD(7:2)
           MOVE    "99999999"        TO  SYU-SKYEDYMD
      *
           MOVE    SYUNOU-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"        TO  MCP-FUNC
           MOVE    "tbl_syunou"      TO  MCP-TABLE
           MOVE    "key5"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 910-SYUNOU-FETCH-SEC
           ELSE
               MOVE    1             TO  FLG-SYUNOU
           END-IF        
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       910-SYUNOU-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
           MOVE    "tbl_syunou"      TO  MCP-TABLE
           MOVE    "key5"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO  FLG-SYUNOU
               MOVE    MCPDATA-REC   TO  SYUNOU-REC
           ELSE
               MOVE    1             TO  FLG-SYUNOU
           END-IF  
           .
       910-SYUNOU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-SYUNOU-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
           MOVE    "tbl_syunou"      TO  MCP-TABLE
           MOVE    "key5"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           .
       910-SYUNOU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
           INITIALIZE  SYUNOU-REC
           MOVE  RECE10-HOSPNUM       TO   SYU-HOSPNUM
           MOVE  RECE10-NYUGAIKBN     TO   SYU-NYUGAIKBN
           MOVE  RECE10-PTID          TO   SYU-PTID
           MOVE  SYUNOU-REC           TO   MCPDATA-REC
           MOVE  "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syunou"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  925-SYUNOU-READ-SEC
      *
             PERFORM  UNTIL  FLG-SYUNOU  =  1
               IF  RECE10-SRYYM  =  SYU-SRYYMD(1:6)
                 IF  SYU-DENPJTIKBN  NOT =  3
                   COMPUTE  WRK-SKYMONEY  =  SYU-SKYMONEY
                                          -  SYU-JIHI-TOTAL
                                          -  SYU-JIHI-TOTAL-TAX
                   ADD   WRK-SKYMONEY     TO   MF100-FTNMONEY
                 END-IF
               END-IF
      *
               PERFORM   925-SYUNOU-READ-SEC
             END-PERFORM
           ELSE
             DISPLAY  "SYUNOU SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
           MOVE    "tbl_syunou"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           MOVE    "tbl_syunou"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF    MCP-RC  =  ZERO
             MOVE    MCPDATA-REC      TO   SYUNOU-REC
             MOVE    ZERO             TO   FLG-SYUNOU
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
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           MOVE    WRK-PARA-SRYYM       TO   WRK-SRYYMD (1:6)
           MOVE    "01"                 TO   WRK-SRYYMD (7:2)
           INITIALIZE  HKNNUM-REC
           MOVE    SYS-1001-HOSPNUM     TO   HKN-HOSPNUM
           MOVE    WRK-KOHNUM           TO   HKN-HKNNUM
           MOVE    HKNNUM-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_hknnum"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknnum"       TO   MCP-TABLE
             MOVE    "key2"             TO   MCP-PATHNAME
             PERFORM  955-HKNNUM-READ-SEC
           ELSE
             MOVE      1      TO       FLG-HKNNUM
           END-IF
           PERFORM    UNTIL  FLG-HKNNUM   =   1
             IF   HKN-TEKSTYMD   <=  WRK-SRYYMD
                 AND   HKN-TEKEDYMD   >=  WRK-SRYYMD
               MOVE     1               TO   FLG-HKNNUM
             END-IF
      *
             IF   FLG-HKNNUM   =   ZERO
               MOVE  "tbl_hknnum"       TO   MCP-TABLE
               MOVE  "key2"             TO   MCP-PATHNAME
               PERFORM  955-HKNNUM-READ-SEC
             END-IF
           END-PERFORM
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
           MOVE    "tbl_hknnum"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBFETCH"        TO   MCP-FUNC
           CALL    "ORCDBMAIN"      USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    MCPDATA-REC    TO   HKNNUM-REC
             MOVE    ZERO           TO   FLG-HKNNUM
      *
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE    1              TO   FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC  SECTION.
      *
           MOVE    CNT-PAGE          TO   WRK-PARA-PAGE
      *
      *    ステップ終了処理
           MOVE    "STE"             TO   SJOBKANRI-MODE
           INITIALIZE                     JOBKANRI-REC
           MOVE    SPA-HOSPNUM       TO   JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE     TO   JOB-UPDCNT
           MOVE    WRK-PARA-JOBID    TO   JOB-JOBID
           MOVE    WRK-PARA-SHELLID  TO   JOB-SHELLID
           CALL    "ORCSJOB"         USING
                                     ORCSJOBKANRIAREA
                                     JOBKANRI-REC
                                     SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE    "DBSELECT"           TO   MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
             MOVE    "tbl_syskanri"     TO   MCP-TABLE
             MOVE    "key10"            TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    ZERO  TO  FLG-SYSKANRI
             ELSE
               MOVE    1                TO   FLG-SYSKANRI
             END-IF
           ELSE
               MOVE    1                TO   FLG-SYSKANRI
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
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV頭書き出力　　処理
      *****************************************************************
       210-CSV-HED-SEC                SECTION.
      *
      *
      *    ＣＳＶファイル名称
           IF  CNT-PAGE  >  0
             MOVE    "20"                TO   CSV-DENPPRTYMDHM(1:2)
             MOVE    SYS-YMD             TO   CSV-DENPPRTYMDHM(3:6)
             MOVE    SYS-TIME(1:4)       TO   CSV-DENPPRTYMDHM(9:4)
           END-IF
           OPEN  OUTPUT  A00000KOHS-FILE
      *
           INITIALIZE  A00000KOHS-REC
           STRING   "償還払公費患者一覧"   DELIMITED  BY  SPACE
                                         INTO    A00000KOHS-REC
           END-STRING
           WRITE  A00000KOHS-REC
      *
           INITIALIZE  A00000KOHS-REC
           MOVE  WRK-PARA-SRYYM(1:4)     TO   A00000KOHS-REC(1:4)
           MOVE  "年"                    TO   A00000KOHS-REC(5:2)
           MOVE  WRK-PARA-SRYYM(5:2)     TO   A00000KOHS-REC(7:2)
           MOVE  "月"                    TO   A00000KOHS-REC(9:2)
           WRITE  A00000KOHS-REC
      *
           INITIALIZE  A00000KOHS-REC
           STRING   "公費名"              DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "ＩＤ"                DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "氏名"                DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "フリガナ"            DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "負担者番号"          DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "受給者番号"          DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "診療年月"            DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "性別"                DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "入外区分"            DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "保険者番号"          DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "記号・番号"          DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "実日数"              DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "点数（点）"          DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "本人負担額（円）"    DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "回数（食事日数）"    DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "請求額（食事日数）"  DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "標準負担額（食事日数）"    DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "診療科名"            DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "給付割合"            DELIMITED  BY  SPACE
                    ","                   DELIMITED  BY  SIZE
                    "他方公費"            DELIMITED  BY  SPACE
                                          INTO   A00000KOHS-REC
           END-STRING
           WRITE  A00000KOHS-REC
           .
       210-CSV-HED-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＶＳ書き出し処理
      *****************************************************************
       210-CSV-WRITE-SEC                SECTION.
      *
      *    ＣＳＶ書き出し
      *    
           INITIALIZE  A00000KOHS-REC
                       WRK-REC-LENGTH
      *    公費名
           MOVE    MF100-KOHNAME      TO   WRK-DATA
           MOVE    20                 TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    患者番号 
           MOVE    MF100-PTNUM        TO   WRK-DATA
           MOVE    20                 TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    患者氏名 
           MOVE    MF100-NAME         TO   WRK-DATA
           MOVE    100                TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    患者フリガナ 
           MOVE    MF100-FURIGANA     TO   WRK-DATA
           MOVE    100                TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    負担者番号
           MOVE    MF100-FTNJANUM     TO   WRK-DATA
           MOVE    20                 TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    受給者番号 
           MOVE    MF100-JKYSNUM      TO   WRK-DATA
           MOVE    20                 TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療年月(請求年月と異なる時のみ)
           IF   MF100-SRYYM   NOT =   WRK-PARA-SRYYM
             MOVE    MF100-SRYYM      TO   WRK-SYMD(1:6)
             MOVE    "01"             TO   WRK-SYMD(7:2)
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE    WRK-HENYMDG1(1:6)   TO   WRK-DATA
             MOVE   6                  TO   WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           END-IF
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    性別
           IF      MF100-SEX  =  1
             MOVE    "男"             TO   WRK-DATA
           ELSE
             MOVE    "女"             TO   WRK-DATA
           END-IF
           MOVE    2                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    入外区分 
           IF    MF100-NYUGAIKBN   =   1
             MOVE    "入"             TO   WRK-DATA
           ELSE 
             MOVE    "外"             TO   WRK-DATA
           END-IF
           MOVE    2                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    保険者番号
           MOVE    MF100-HKNJANUM     TO   WRK-DATA
           MOVE    8                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    記号・番号
           MOVE    MF100-KIGO         TO   WRK-DATA
           MOVE    80                 TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    "・"               TO   WRK-DATA
           MOVE    2                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    MF100-NUM          TO   WRK-DATA
           MOVE    80                 TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    実日数 
           MOVE    MF100-JNISSU       TO   WRK-DATA
           MOVE    2                  TO   WRK-NAGASA
           PERFORM 5005-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    合計点数 
           MOVE    MF100-TOTALTEN     TO   WRK-DATA
           MOVE    8                  TO   WRK-NAGASA
           PERFORM 5005-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    負担金額 
           MOVE    MF100-FTNMONEY     TO   WRK-DATA
           MOVE    9                  TO   WRK-NAGASA
           PERFORM 5005-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    回数（食事療養費）
           MOVE    MF100-SKJKAISU     TO   WRK-DATA
           MOVE    2                  TO   WRK-NAGASA
           PERFORM 5005-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    請求額（食事療養費）
           MOVE    MF100-SKJSKY       TO   WRK-DATA
           MOVE    9                  TO   WRK-NAGASA
           PERFORM 5005-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    標準負担額（食事療養費）
           MOVE    MF100-SKJFTN       TO   WRK-DATA
           MOVE    9                  TO   WRK-NAGASA
           PERFORM 5005-MOJI-HENSYU-SEC
           MOVE    ","                TO   WRK-DATA
           MOVE    1                  TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療科名
           IF   MF100-SRYKA  NOT =  "00"
             INITIALIZE                  SYS-1005-REC
             MOVE    SPA-HOSPNUM         TO   SYS-1005-HOSPNUM
             MOVE    "1005"              TO   SYS-1005-KANRICD
             MOVE    MF100-SRYKA         TO   SYS-1005-KBNCD
             MOVE    WRK-PARA-PRTSYMD    TO   SYS-1005-EDYUKYMD
             MOVE    WRK-PARA-PRTLYMD    TO   SYS-1005-STYUKYMD
             MOVE    SYS-1005-REC        TO   MCPDATA-REC
             PERFORM 800-SYSKANRI-READ-SEC
             IF   FLG-SYSKANRI  =  ZERO
               MOVE  MCPDATA-REC         TO   SYS-1005-REC
               MOVE  SYS-1005-SRYKANAME  TO   WRK-SRYKANAME
             END-IF
           END-IF
           MOVE    WRK-SRYKANAME         TO   WRK-DATA
           MOVE    10                    TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    ","                   TO   WRK-DATA
           MOVE    1                     TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    給付割合
           MOVE    MF100-KYURATE         TO   WRK-DATA
           MOVE    3                     TO   WRK-NAGASA
           PERFORM 5005-MOJI-HENSYU-SEC
           MOVE    ","                   TO   WRK-DATA
           MOVE    1                     TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    他方公費
      *--- 20110107 CHANGE START ---------------------------------------
      *     MOVE    MF100-TAHO            TO   WRK-DATA
           MOVE    MF100-TAHO(1)            TO   WRK-DATA
      *--- 20110107 CHANGE END   ---------------------------------------
           MOVE    3                     TO   WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           
           WRITE   A00000KOHS-REC
      *
           .
       210-CSV-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF   WRK-DATA   =   SPACE
             CONTINUE
           ELSE
             PERFORM VARYING IDW  FROM  WRK-NAGASA  BY  -1
                     UNTIL   IDW   <   1
               IF   WRK-DATA(IDW:1)   NOT =   SPACE
                 STRING  A00000KOHS-REC(1:WRK-REC-LENGTH) 
                                            DELIMITED  BY  SIZE
                         WRK-DATA(1:IDW)    DELIMITED  BY  SIZE
                         INTO     A00000KOHS-REC
                 END-STRING
                 ADD     IDW             TO   WRK-REC-LENGTH
                 MOVE     1              TO   IDW
               END-IF
             END-PERFORM
           END-IF
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    出力レコード処理（数字）
      *****************************************************************
       5005-MOJI-HENSYU-SEC          SECTION.
      *
           INITIALIZE    IDX6
           IF    WRK-DATA   =   SPACE
               CONTINUE
           ELSE
             PERFORM VARYING IDW    FROM    1   BY  1
                   UNTIL   IDW    >     WRK-NAGASA
               IF      WRK-DATA (IDW:1)   NOT =   0
                 STRING  A00000KOHS-REC(1:WRK-REC-LENGTH) 
                                              DELIMITED  BY  SIZE
                         WRK-DATA(IDW:)       DELIMITED  BY  SIZE
                         INTO     A00000KOHS-REC
                 END-STRING
                 COMPUTE  IDW  =   WRK-NAGASA   -   IDX6
                 ADD      IDW             TO   WRK-REC-LENGTH
                 MOVE     9999            TO   IDW
               END-IF
               ADD    1                   TO   IDX6
             END-PERFORM
           END-IF
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE     "DBOPEN"       TO   MCP-FUNC.
           MOVE    LOW-VALUE       TO   MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
                                   .
      *
           MOVE    "DBSTART"       TO   MCP-FUNC.
           MOVE    LOW-VALUE       TO   MCP-TABLE
                                        MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
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
       900-DBDISCONNECT-SEC  SECTION.
      *
           MOVE    "DBCOMMIT"         TO   MCP-FUNC.
           MOVE    LOW-VALUE          TO   MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
                                      .
      *
           MOVE    "DBDISCONNECT"     TO   MCP-FUNC.
           MOVE    LOW-VALUE          TO   MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
                                      .
           .
       900-DBCLOSE-EXT.
           EXIT.
