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
       PROGRAM-ID.             A00000A010.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次／月次帳票
      *  コンポーネント名  : 診療行為別使用頻度一覧表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/12/25    @@@@-半田     新規作成
      *  04/01/30    @@@@-半田     「印字順序」の仕様変更対応
      *  04/07/14    @@@@-本橋     入院診療行為情報抽出処理の不具合修正
      *  04/07/23    @@@@-本橋     食事療養費、室料差額の回数集計処理を修正
      *  04/07/27    @@@@-本橋     入院、外来の合計数量の集計処理を追加
      *  04/09/14    @@@@-本橋     システム管理からの室料差額算出KEY変更
      *                            MONPE化
      *  04/11/08    @@@@-本橋     外泊(099999911)、室料差額(099999912)、
      *                            食事療養費(099999913)、老人特定入院料
      *                            (099999915)、選定入院料(099999916)を
      *                            "90:入院"に集計するように修正
      *                            095000000を"95:自費(税無)"、096000000を
      *                             "96:自費(税有)"に集計するように修正
      *  05/02/08    @@@@-鈴木     回数(量)小数点以下の桁を印字するように修正 
      *  05/10/17    @@@@-呉秀凡   DBアクセスMCPSUBへの変更
      *  06/01/06    @@@@-呉秀凡   室料差額の設定
      *  06/08/17    @@@@-高嶋     sarge対応
      *  07/07/17    @@@@-田中     食事療養費の対応(099999918〜099999920)
      *  07/08/28    @@@@-長屋     診療行為振り分け修正
      *  07/09/21    @@@@-田中     4.0対応
      *  07/11/05    @@@@-田中     きざみ値による点数の不具合を修正
      *  08/01/21    @@@@-田中     入院診療行為の点数、回数の不具合を修正
      *  08/04/02    @@@@-田中     H20年4月改正対応
      *  08/04/18    @@@@-田中     診療行為区分の振り分けを修正
      *  08/05/26    @@@@-田中     診療行為区分の振り分けを修正
      *  08/09/02    @@@@-田中     ０回の診療行為時の不具合の修正
      *  08/11/20    @@@@-田中     自費のフリー金額を集計するように変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN TO RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    診療行為別使用頻度一覧表ファイル
           SELECT  A010T-FILE      ASSIGN  A010TPARA
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     A010T-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
      *    診療行為別使用頻度一覧表ファイル２
           SELECT  A010T2-FILE     ASSIGN TO A010T2PARA
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     A010T2-KEY
                                   FILE    STATUS  IS  STS-PARA2.
      *
      * 20080902 ０回の診療行為時の不具合の修正START
      *    診療行為別使用頻度一覧表ファイル３
           SELECT  A010T3-FILE     ASSIGN TO A010T3PARA
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     A010T3-KEY
                                   FILE    STATUS  IS  STS-PARA3.
      * 20080902 ０回の診療行為時の不具合の修正END
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).

      *    診療行為別使用頻度一覧表ファイル
       FD  A010T-FILE.
           COPY    "A00000A010TMP.INC".
      *
      *    診療行為別使用頻度一覧表ファイル２
       FD  A010T2-FILE.
           COPY    "A00000A010TMP.INC"
                                   REPLACING  //A010T//
                                   BY         //A010T2//.
      *
      * 20080902 ０回の診療行為時の不具合の修正START
      *    診療行為別使用頻度一覧表ファイル２
       FD  A010T3-FILE.
           COPY    "A00000A010TMP2.INC".
      *
      * 20080902 ０回の診療行為時の不具合の修正END
      *
       WORKING-STORAGE             SECTION.
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域 
      * 20070921 4.0対応START t.tanaka
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      * 20070921 4.0対応END
      *    診療行為別使用頻度一覧表ファイル
       01  A010TPARA.
           03  FILLER   PIC  X(05) VALUE "/tmp/".
           03  A010TPARA-FILENAME  PIC  X(13) VALUE "A00000A010-IN".
           03  FILLER              PIC  X(04) VALUE ".TMP".
      *
       01  A010T2PARA.
           03  FILLER   PIC  X(05) VALUE "/tmp/".
           03  A010T2PARA-FILENAME PIC  X(14) VALUE "A00000A010-OUT".
           03  FILLER              PIC  X(04) VALUE ".TMP".
      *
      * 20080902 ０回の診療行為時の不具合の修正START
       01  A010T3PARA.
           03  FILLER   PIC  X(05) VALUE "/tmp/".
           03  A010T3PARA-FILENAME PIC  X(14) VALUE "A00000A010-OUT".
           03  FILLER              PIC  X(04) VALUE ".TMP".
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    診療行為別使用頻度一覧表 diaファイル
           COPY    "A00000A010.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-PARA            PIC 9(02).
           03  STS-PARA2           PIC 9(02).
      * 20080902 ０回の診療行為時の不具合の修正START
           03  STS-PARA3           PIC 9(02).
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                  PIC 9(01).
           03  FLG-SYSKANRI             PIC 9(01).
           03  FLG-HKNCOMBI             PIC 9(01).
           03  FLG-PTBYOMEI             PIC 9(01).
           03  FLG-PTINF                PIC 9(01).
           03  FLG-PTNUM                PIC 9(01).
           03  FLG-PTHKNINF             PIC 9(01).
           03  FLG-SRYACCT              PIC 9(01).
           03  FLG-NYUINACCT            PIC 9(01).
           03  FLG-JYURRK               PIC 9(01).
           03  FLG-PTNYUINRRK           PIC 9(01).
           03  FLG-SRYACT               PIC 9(01).
           03  FLG-NYUINACT             PIC 9(01).
           03  FLG-TENSU                PIC 9(01).
           03  FLG-KENSASORT            PIC 9(01).
           03  FLG-SRYKA                PIC 9(01).
           03  FLG-SRYKBN               PIC 9(01).
           03  FLG-INPUTCD              PIC 9(01).
           03  FLG-OK                   PIC 9(01).
           03  FLG-NG                   PIC 9(01).
           03  FLG-ATEND                PIC 9(01).
           03  FLG-INVALID              PIC 9(01).
      * 20080902 ０回の診療行為時の不具合の修正START
           03  FLG-INVALID2              PIC 9(01).
           03  FLG-ATEND2                PIC 9(01).
      * 20080902 ０回の診療行為時の不具合の修正END
           03  FLG-GAISW                PIC 9(01).
           03  FLG-NYUSW                PIC 9(01).
           03  FLG-OUTALL               PIC 9(01).
           03  FLG-NYUGAIALL            PIC 9(01).
           03  INP-STS                  PIC 9(01).
           03  FLG-KAIKEI               PIC 9(01).
           03  FLG-NYUKAIKEI            PIC 9(01).
           03  FLG-NICDINV              PIC 9(01).
           03  FLG-GICDINV              PIC 9(01).
           03  FLG-SPACE                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-A010T           PIC 9(08).
           03  CNT-A010T2          PIC 9(08).
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
           03  IDX3               PIC 9(02).
           03  IDX4               PIC 9(01).
           03  IDX5               PIC 9(02).
           03  IDX6               PIC 9(02).
           03  IDX                PIC 9(04).
      ****2005/10/17  @@@@-BRAND  START
           03  IDX7               PIC 9(04).
      ****2005/10/17  @@@@-BRAND  END
      ****2006/01/07  @@@@-BRAND  START
           03  IIII               PIC 9(03).
           03  SSSS               PIC 9(03).
           03  ZZZZ               PIC 9(03).
      ****2006/01/07  @@@@-BRAND  END
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID         PIC 9(07).
           03  WRK-PARA-SHELLID       PIC X(08).
      * 20070921 4.0対応START t.tanaka
      *     03  WRK-PARA-HOSPID        PIC X(24).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      * 20070921 4.0対応END
           03  WRK-PARA-PRTYM         PIC X(06).
           03  WRK-PARA-OUTTYPE       PIC X(02).
           03  WRK-PARA-NYUGAIKBN     PIC X(01). 
           03  WRK-PARA-PRSORT        PIC X(01). 
           03  WRK-PARA-PRTLYMD       PIC X(08).
           03  WRK-PARA-PRTSYMD       PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA-DENPPRTYMDWH PIC X(22).
           03  WRK-SYSYMDWH          PIC X(22).
      *  
           03  WRK-ADRS              PIC X(100).
           03  WRK-NAME              PIC X(40).
           03  WRK-PTID-G.
               05  WRK-PTID          PIC 9(10).
           03  WRK-PRDTLCNT          PIC 9(010).
           03  WRK-BYOMEI            PIC X(100).
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
           03  WRK-BIRTHYMD.
               05  WRK-BIRTHYY       PIC 9(04).
               05  WRK-BIRTHMM       PIC 9(02).
               05  WRK-BIRTHSYSDD    PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-FROMYMD           PIC X(22).
           03  WRK-TOYMD             PIC X(22).
      *    郵便番号変換
        03  WRK-POSTMAEG.
               05  WRK-POSTMAE1      PIC X(03).
               05  WRK-POSTMAE2      PIC X(04).
           03  WRK-POSTATOG.
               05  WRK-POSTATO1      PIC X(03).
               05  WRK-POSTNAKA      PIC X(01)   VALUE  "-".
               05  WRK-POSTATO2      PIC X(04).
      *
           03  WRK-PTID2             PIC X(10).
           03  WRK-SRYCD2            PIC X(10).
           03  WRK-RENNUM            PIC 9(05).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-NENREI            PIC 9(03).
      ****2005/10/17 @@@@-BRAND  START
      *     03  WRK-ADRS              PIC X(200).
      ****2005/10/17 @@@@-BRAND  END
      *
      *  2005/02/08 @@@@-SUZUKI 変更 
      *     03  WRK-ZAIKAISU          PIC 9(09).
           03  WRK-ZAIKAISU          PIC 9(09)V9(03).
      *  2005/02/08 ここまで    
           03  WRK-PRSORTNUM         PIC 9(09)V9(02).
           03  WRK-TEN               PIC 9(09)V9(02).
           03  WRK-TEN2              PIC 9(09)V9(02).
           03  WRK-KAI               PIC 9(09).
           03  WRK-Z9TENGK           PIC ZZZZZZZZZ9.99.
           03  WRK-Z9TEN             PIC ZZZZZZZZ9.99.
           03  WRK-Z9TEN2            PIC ZZZZZZZ9.99.
           03  WRK-Z9KAI             PIC ZZZZZZZ9.
      *  2005/02/08 @@@@SUZUKI 小数点以下の桁を印字するために追加
           03  WRK-Z9KAI-1           PIC 9(09).
        03  WRK-Z9KAI-1Z          PIC ZZZZZZZZ9.
           03  WRK-Z9KAI-2           PIC 9(09).
           03  WRK-Z9KAI-2Z          PIC ZZZZ9.999.
           03  WRK-Z9KAI-2ZZ         PIC ZZZZZZ9.9.
           03  WRK-Z9KAI-2ZZZ        PIC ZZZZZ9.99.
      *  2005/02/08 ここまで
           03  WRK-Z7                PIC ZZZ,ZZZ.
           03  WRK-Z5                PIC ZZZZ9.
           03  WRK-Z3                PIC ZZ9.
           03  WRK-PAGE              PIC 9(03).
           03  WRK-Z9                PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZZZZZZ.
           03  WRK-S9                PIC -----------9.
           03  WRK-SZ                PIC -----------Z.
           03  WRK-SRYKA             PIC X(02).
           03  DEBUG-CNT             PIC 9(03).
           03  WRK-OUTTYPE           PIC X(02).
           03  WRK-PRT-OUTTYPE       PIC X(02).
           03  WRK-SYUKBN            PIC X(03).
      *
      ***  20040723  @@@@-MOTOHASI
           03  WRK-SYOKUJIKAISU       PIC 9(09).
           03  WRK-SYOKUJIKAI.
               05  WRK-SYOKUJIKAISU1 PIC 9(09).
               05  WRK-SYOKUJIKAISU2 PIC 9(09).
               05  WRK-SYOKUJIKAISU3 PIC 9(09).
               05  WRK-SYOKUJIKAISU4 PIC 9(09).
           03  WRK-SYOKUJITEN.
               05  WRK-SYOKUJITEN1   PIC 9(08)V9(02).
               05  WRK-SYOKUJITEN2   PIC 9(08)V9(02).
               05  WRK-SYOKUJITEN3   PIC 9(08)V9(02).
               05  WRK-SYOKUJITEN4   PIC 9(08)V9(02).
           03  WRK-SITURYOKAI-TBL.
      ****2005/12/27 @@@@-BRAND  START
      *        05  WRK-SITURYOKAISU      OCCURS 20.
              05  WRK-SITURYOKAISU      OCCURS 30.
      ****2005/12/27 @@@@-BRAND  END
                   07  WRK-SITURYOKAI    PIC 9(09).
      ****2005/12/27 @@@@-BRAND  START
      *     03  WRK-SITURYOYEN-TBL    OCCURS 20.
           03  WRK-SITURYOYEN-TBL    OCCURS 30.
      ****2005/12/27 @@@@-BRAND  END
               05  WRK-SITURYOYEN    PIC 9(08)V9(02).
      ***  20040723 
      * 20071105 きざみ値による点数の不具合を修正START t.tanaka
           03  WRK-KIZAMI             PIC S9(09).
      * 20071105 きざみ値による点数の不具合を修正END
      *  
       01  SUM-AREA.
           03  SUM-T-AREA.
               05  SUM-T-TTEN        PIC 9(12)V9(02).
               05  SUM-T-TKIN        PIC 9(12)V9(02).
      *
           COPY    "CPSHELLTBL.INC".
      *     
      * 20070921 4.0対応START t.tanaka
      *     COPY    "ORCA-DBPATH".         
      * 20070921 4.0対応END
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
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *    入院基本情報
           COPY  "CPSK5000.INC".
      *
      *    室料差額
           COPY  "CPSK5113.INC".
      *
      *    患者病名情報
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
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
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    入院診療会計マスタ
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為マスタ−
       01  NYUIN-SRYACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力マスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    検査ソート情報
       01  KENSASORT-REC.
           COPY    "CPKENSASORT.INC".
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".

      *    ＤＢ検索
           COPY    "MCPAREA".
      *
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
      *    処理プロセス
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF  INP-STS =  ZERO  
      *    正常処理
             PERFORM 200-MAIN-SEC
           ELSE
      *    エラー処理
             PERFORM 200-ERR-SEC
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
           INITIALIZE                  WRK-PARA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  A010T
           INITIALIZE                  A010
      * 20070921 4.0対応START t.tanaka
           INITIALIZE                  SPA-AREA
      * 20070921 4.0対応END
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
      * 20070921 4.0対応START t.tanaka
      *                                         WRK-PARA-HOSPID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
      * 20070921 4.0対応END
                                               WRK-PARA-OUTTYPE
                                               WRK-PARA-PRTYM
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-PRSORT
      * 20070921 4.0対応START t.tanaka
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      * 20070921 4.0対応END
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      * 20070921 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      * 20070921 4.0対応END
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "A00000A010"    TO  JOB-PGID
           MOVE    "診療行為別使用頻度一覧"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      * 20070921 4.0対応START t.tanaka
                                   SPA-AREA
      *
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      * 20070921 4.0対応END
      *  
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD   TO   WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO   WRK-SYSYMDWH 
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTYM        TO   WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD              TO   WRK-PARA-PRTLYMD
      *
      *    対象年月初日取得
           INITIALIZE  WRK-SYMD
           MOVE    WRK-PARA-PRTYM        TO   WRK-PARA-PRTSYMD(1:6)
           MOVE    "01"                  TO   WRK-PARA-PRTSYMD(7:2)
      *
      *    対象年月日編集
           INITIALIZE  WRK-SYMD
      ***     MOVE    WRK-PARA-PRTYM  TO    WRK-SYMD
      ***     MOVE    "01"  TO  WRK-SYMD(7:2)     
           MOVE    WRK-PARA-PRTLYMD  TO    WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PARA-DENPPRTYMDWH 
      *
      *    診療分類指定のチェック
           IF   WRK-PARA-OUTTYPE       =  SPACE
               OR  WRK-PARA-OUTTYPE       =  "0"
             MOVE   1       TO   FLG-OUTALL    
           ELSE
             IF   WRK-PARA-OUTTYPE   NOT =  "10"
                 AND WRK-PARA-OUTTYPE   NOT =  "21"
                 AND WRK-PARA-OUTTYPE   NOT =  "22"
                 AND WRK-PARA-OUTTYPE   NOT =  "23"
                 AND WRK-PARA-OUTTYPE   NOT =  "29"
                 AND WRK-PARA-OUTTYPE   NOT =  "30"
                 AND WRK-PARA-OUTTYPE   NOT =  "40"
                 AND WRK-PARA-OUTTYPE   NOT =  "50"
                 AND WRK-PARA-OUTTYPE   NOT =  "60"
      * 20080402 H20年4月改正対応START tanaka
                 AND WRK-PARA-OUTTYPE   NOT =  "64"
      * 20080402 H20年4月改正対応END
                 AND WRK-PARA-OUTTYPE   NOT =  "70"
                 AND WRK-PARA-OUTTYPE   NOT =  "71"
                 AND WRK-PARA-OUTTYPE   NOT =  "80"
                 AND WRK-PARA-OUTTYPE   NOT =  "90"
                 AND WRK-PARA-OUTTYPE   NOT =  "91"
                 AND WRK-PARA-OUTTYPE   NOT =  "92"
                 AND WRK-PARA-OUTTYPE   NOT =  "95"
                 AND WRK-PARA-OUTTYPE   NOT =  "96"
                 AND WRK-PARA-OUTTYPE   NOT =  "99"
      ***      出力区分入力値エラーの場合
               MOVE   1       TO    INP-STS
             END-IF
           END-IF
      *
      *    入外区分のチェック
      *
           IF  WRK-PARA-NYUGAIKBN   =  SPACE
      ***  20040727  @@@@-MOTOHASI  追加
      ****2005/12/27 @@@@-BRAND  START
      *         OR  WRK-PARA-NYUGAIKBN   =  3
               OR  WRK-PARA-NYUGAIKBN   =  "3"
      ****2005/12/27 @@@@-BRAND  END
      ***  ここまで
             MOVE   1       TO    FLG-NYUGAIALL
           ELSE
             IF   WRK-PARA-NYUGAIKBN NOT =  "1"
                 AND WRK-PARA-NYUGAIKBN NOT =  "2"
      ***  20040727  @@@@-MOTOHASI  追加
                 AND WRK-PARA-NYUGAIKBN NOT =  "3"
      ***  ここまで
      *    入外区分入力値エラーの場合
               MOVE   4       TO    INP-STS
             END-IF
           END-IF
      *
      *    出力順序のチェック
      *
      * 20040129変更
      ***     IF   WRK-PARA-PRSORT NOT =  SPACE
      ***      AND WRK-PARA-PRSORT NOT =  "0"
      ***      AND WRK-PARA-PRSORT NOT =  "1"
           IF   WRK-PARA-PRSORT NOT =  SPACE
               AND WRK-PARA-PRSORT NOT =  "1"
               AND WRK-PARA-PRSORT NOT =  "2"
               AND WRK-PARA-PRSORT NOT =  "8"
               AND WRK-PARA-PRSORT NOT =  "9"
      * 20040129変更
      *    印刷順序入力値エラーの場合
             MOVE   5      TO    INP-STS
           END-IF
      *
      ***  20040723  @@@@-MOTOHASI  食事療養費点数算出
           INITIALIZE                SYS-5000-REC
           MOVE    "5000"            TO   SYS-5000-KANRICD
      ****2005/10/17 @@@@-BRAND  START
      *     MOVE    WRK-PARA-PRTLYMD  TO   WRK-SYMD
           MOVE  WRK-PARA-PRTLYMD    TO  SYS-5000-STYUKYMD
           MOVE  WRK-PARA-PRTSYMD    TO  SYS-5000-EDYUKYMD
      ****2005/10/17 @@@@-BRAND  END
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           IF   FLG-SYSKANRI = ZERO
      *    食事療養費
             MOVE  SYS-5000-SHOKUJIRYOYOCD TO    TNS-SRYCD
             PERFORM   910-TENSU-READ-SEC
             MOVE  TNS-TEN    TO   WRK-SYOKUJITEN1
                                   WRK-SYOKUJITEN2
                                   WRK-SYOKUJITEN3
                                   WRK-SYOKUJITEN4
      *    食事療養費(特別管理加算)の算定判定
             IF  SYS-5000-SHOKUKBN(1)  =  "2"
               MOVE  SYS-5000-SHOKUCD(1) TO    TNS-SRYCD
               PERFORM   910-TENSU-READ-SEC
               ADD   TNS-TEN  TO   WRK-SYOKUJITEN1
                                   WRK-SYOKUJITEN2
                                   WRK-SYOKUJITEN3
                                   WRK-SYOKUJITEN4
             END-IF  
      *    食事療養費(食堂加算)の算定判定
             IF  SYS-5000-SHOKUKBN(2)  =  "2"
               MOVE  SYS-5000-SHOKUCD(2) TO    TNS-SRYCD
               PERFORM   910-TENSU-READ-SEC
               ADD   TNS-TEN  TO   WRK-SYOKUJITEN1
                                   WRK-SYOKUJITEN2
                                   WRK-SYOKUJITEN3
                                   WRK-SYOKUJITEN4
             END-IF  
             PERFORM 820-SYSKANRI-DBCLOSE-SEC
           END-IF  
      ***  1:食事療養費のみ(WRK-SYOKUJITEN1)
      ***  2:食事療養費＋特別食加算(WRK-SYOKUJITEN2)
      ***  3:食事療養費＋複数献立加算(WRK-SYOKUJITEN3)
      ***  4:食事療養費＋特別食加算＋複数献立加算(WRK-SYOKUJITEN4)
      *
      ***  特別食加算の算出
           MOVE  "197000470"   TO    TNS-SRYCD
           PERFORM   910-TENSU-READ-SEC
      *
           ADD    TNS-TEN      TO    WRK-SYOKUJITEN2
                                     WRK-SYOKUJITEN4
      ***  複数献立加算の算出
           MOVE  "197000670"   TO    TNS-SRYCD
           PERFORM   910-TENSU-READ-SEC
      *
           ADD    TNS-TEN      TO    WRK-SYOKUJITEN3
                                     WRK-SYOKUJITEN4
      ***  20040723  ここまで
      *
      ***  20040723  @@@@-MOTOHASI  室料差額算出
           INITIALIZE         SYS-5113-REC
           MOVE    "5113"      TO   SYS-5113-KANRICD
      ****2005/10/17 @@@@-BRAND  START
      *     MOVE    WRK-PARA-PRTLYMD  TO   WRK-SYMD
      * 20080121 入院診療行為の点数、回数の不具合を修正START t.tanaka
           MOVE  SPA-HOSPNUM      TO  SYS-5113-HOSPNUM
      * 20080121 入院診療行為の点数、回数の不具合を修正START t.tanaka
           MOVE  WRK-PARA-PRTLYMD    TO  SYS-5113-STYUKYMD
           MOVE  WRK-PARA-PRTSYMD    TO  SYS-5113-EDYUKYMD

      ****2005/10/17 @@@@-BRAND  END
           PERFORM 830-SYSKANRI-DBSELECT-SEC
           PERFORM   UNTIL   FLG-SYSKANRI =   1
      ****2005/12/27 @@@@-BRAND  START
      *                 OR    IDX3  >= 20
                       OR    IDX3 >= 30
      ****2005/12/27 @@@@-BRAND  END
             ADD   1        TO    IDX3
             MOVE  SYS-5113-BRM-SAGAKU-NM    TO   WRK-SITURYOYEN(IDX3)
             PERFORM 830-SYSKANRI-DBFETCH-SEC
           END-PERFORM
      ***  20040723  ここまで
      *=== sarge対応 ADD By Takashima(2006/09/16) start
           PERFORM 830-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/09/16) end
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    診療行為情報抽出処理
           PERFORM 2001-SRYGET-SEC
      *    入院診療行為情報抽出処理
           PERFORM 2001-NSRYGET-SEC
      * 20040129変更
      ***     PERFORM 2001-ICDGET-SEC
      *    未使用分入力コード情報抽出処理
      *    (未使用分は印刷順序指定が"1"の場合に限りデータ抽出を行う。)
           IF    WRK-PARA-PRSORT  = "8"
               OR   WRK-PARA-PRSORT  = "9"
             PERFORM 2001-ICDGET-SEC
           END-IF
      * 20040129変更
      *    帳票明細印字順番号設定処理
           PERFORM 2001-SET-PRSORTNUM-SEC
      *    帳票印字処理
           PERFORM 2001-PRT-SEC
      * 
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値エラー処理
      *****************************************************************
       200-ERR-SEC                SECTION.
      *
           EVALUATE  INP-STS
             WHEN  1
               MOVE    "入力された診療分類指定に誤りがあります"
                           TO   A010-JOBMSG
             WHEN  4
               MOVE    "入力された入外区分に誤りがあります"
                           TO   A010-JOBMSG
             WHEN  5
               MOVE    "入力された印字順序に誤りがあります"
                           TO   A010-JOBMSG
           END-EVALUATE
      *
           PERFORM 330-TERM-HEN-SEC
      *
           .
       200-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為回数情報抽出処理
      *****************************************************************
       2001-SRYGET-SEC                SECTION.
      * 
           OPEN     OUTPUT             A010T-FILE
           CLOSE                       A010T-FILE
           OPEN     I-O                A010T-FILE
      * 20080902 ０回の診療行為時の不具合の修正START
           OPEN     OUTPUT             A010T3-FILE
           CLOSE                       A010T3-FILE
           OPEN     I-O                A010T3-FILE
      * 20080902 ０回の診療行為時の不具合の修正END
      *    入院患者読み込み
           INITIALIZE   SRYACT-REC
      * 20070921 4.0対応START t.tanaka
      *     MOVE WRK-PARA-HOSPID  TO   SRY-HOSPID 
           MOVE WRK-PARA-HOSPNUM  TO   SRY-HOSPNUM 
           MOVE WRK-PTID  TO   SRY-PTID
           MOVE WRK-SRYKA  TO   SRY-SRYKA
      * 20070921 4.0対応END
           MOVE WRK-PARA-PRTYM   TO    SRY-SRYYM 
           PERFORM   900-SRYACT-SELECT-SEC
           PERFORM   900-SRYACT-FETCH-SEC
           PERFORM   UNTIL   FLG-SRYACT =   1
      *
             PERFORM  2003-SYORI-SEC
             PERFORM  900-SRYACT-FETCH-SEC
           END-PERFORM
           PERFORM   900-SRYACT-CLOSE-SEC
      *
           .
       2001-SRYGET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為回数一時ファイルヘの書込
      *****************************************************************
       2003-SYORI-SEC                SECTION.
      *
           MOVE     ZERO         TO    FLG-KAIKEI
      *
           PERFORM  VARYING      IDX1  FROM  1  BY   1
                    UNTIL        IDX1  >  5 
              IF       SRY-SRYCD(IDX1)   NOT =  SPACE
                  AND ((  FLG-NYUGAIALL         =  1 )
                  OR  (  FLG-NYUGAIALL     NOT =  1
                  AND   WRK-PARA-NYUGAIKBN    =  SRY-NYUGAIKBN ))
      ***       点数マスタ読込み
                MOVE  SRY-SRYCD(IDX1) TO    TNS-SRYCD
      * 20070921 4.0対応START t.tanaka
                MOVE  SPA-HOSPNUM     TO    TNS-HOSPNUM
      * 20070921 4.0対応END
                PERFORM   910-TENSU-READ-SEC
      *
                IF     FLG-TENSU       =     ZERO      
      ***             AND ( TNS-DATAKBN    =     1
      ***               OR  TNS-DATAKBN    =     2 )

                  IF     FLG-KAIKEI      =     ZERO
      ***             診療会計情報読込み
      * 20070921 4.0対応START t.tanaka
      *                MOVE  SRY-HOSPID      TO    ACCT-HOSPID
                    MOVE  SRY-HOSPNUM      TO    ACCT-HOSPNUM
      * 20070921 4.0対応END
                    MOVE  SRY-NYUGAIKBN   TO    ACCT-NYUGAIKBN
                    MOVE  SRY-PTID        TO    ACCT-PTID
                    MOVE  SRY-SRYKA       TO    ACCT-SRYKA
                    MOVE  SRY-SRYYM       TO    ACCT-SRYYM
                    MOVE  SRY-SRYKBN      TO    ACCT-SRYKBN
                    MOVE  SRY-ZAINUM      TO    ACCT-ZAINUM
                    PERFORM   910-SRYACCT-READ-SEC
      *
                    MOVE      1           TO    FLG-KAIKEI
                  END-IF
      *
                  PERFORM 2003-A010PUT-SEC
                END-IF
              END-IF
           END-PERFORM
      *
           .
       2003-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院 診療行為回数情報抽出処理
      *****************************************************************
       2001-NSRYGET-SEC                SECTION.
      * 
      *    入院患者読み込み
           INITIALIZE                  NYUIN-SRYACT-REC
      * 20070921 4.0対応START t.tanaka
      *     MOVE WRK-PARA-HOSPID   TO   NSRY-HOSPID 
           MOVE WRK-PARA-HOSPNUM   TO   NSRY-HOSPNUM
      * 20070921 4.0対応END
           MOVE WRK-PARA-PRTYM   TO    NSRY-SRYYM 
           PERFORM   900-NYUINACT-SELECT-SEC
           PERFORM   900-NYUINACT-FETCH-SEC
           PERFORM   UNTIL   FLG-NYUINACT =   1
                     PERFORM  2003-NSYORI-SEC
                     PERFORM  900-NYUINACT-FETCH-SEC
           END-PERFORM
           PERFORM   900-NYUINACT-CLOSE-SEC
      *
           .
       2001-NSRYGET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院 診療行為回数一時ファイルヘの書込
      *****************************************************************
       2003-NSYORI-SEC                SECTION.
      *
           MOVE     ZERO         TO    FLG-NYUKAIKEI
      *
           PERFORM  VARYING      IDX1  FROM  1  BY   1
                    UNTIL        IDX1  >  5 
             IF       NSRY-SRYCD(IDX1)   NOT =  SPACE
                 AND ((  FLG-NYUGAIALL          =  1 )
                 OR  (  FLG-NYUGAIALL      NOT =  1  
                 AND   WRK-PARA-NYUGAIKBN     =  NSRY-NYUGAIKBN ))
      *    点数マスタ読込み
               MOVE  NSRY-SRYCD(IDX1) TO    TNS-SRYCD
               PERFORM   910-TENSU-READ-SEC
      *
               IF     FLG-TENSU       =     ZERO 
      ***          AND ( TNS-DATAKBN    =     1
      ***            OR  TNS-DATAKBN    =     2 )
                 IF     FLG-NYUKAIKEI      =     ZERO
      ***         診療会計情報読込み
      * 20070921 4.0対応START t.tanaka
      *                MOVE  NSRY-HOSPID      TO    NACCT-HOSPID
                    MOVE  NSRY-HOSPNUM      TO    NACCT-HOSPNUM
      * 20070921 4.0対応END
                    MOVE  NSRY-NYUGAIKBN    TO    NACCT-NYUGAIKBN
                    MOVE  NSRY-PTID         TO    NACCT-PTID
                    MOVE  NSRY-SRYKA        TO    NACCT-SRYKA
                    MOVE  NSRY-SRYYM        TO    NACCT-SRYYM
                    MOVE  NSRY-SRYKBN       TO    NACCT-SRYKBN
                    MOVE  NSRY-ZAINUM       TO    NACCT-ZAINUM
                    PERFORM   910-NYUINACCT-READ-SEC
                    MOVE      1             TO    FLG-NYUKAIKEI
                 END-IF
      *
      ***  20040723  @@@@-MOTOHASI  食事療養費、室料差額を区分けして
      ***                           正確な回数、点数、金額を算出する
      ***             PERFORM 2003-NA010PUT-SEC
                 IF  NSRY-SRYKBN = "97" 
      *** 20070717食事療養費の修正START　T.Tanaka
      *                 AND NSRY-SRYCD(IDX1) = "099999913"
                     AND (NSRY-SRYCD(IDX1) = "099999913"
                     OR NSRY-SRYCD(IDX1) = "099999918" 
                     OR NSRY-SRYCD(IDX1) = "099999919"
                     OR NSRY-SRYCD(IDX1) = "099999920")
      *** 20070717食事療養費の修正END
      *    食事療養費の回数算出
                     PERFORM 2004-NA010PUT-SEC
                 ELSE
                   IF  NSRY-SRYKBN = "90" 
                       AND NSRY-SRYCD(IDX1) = "099999912"
      *    室料差額の回数算出
                     PERFORM 2005-NA010PUT-SEC
                   ELSE
                     PERFORM 2003-NA010PUT-SEC
                   END-IF
                 END-IF
      ***  20040723  ここまで
      *
               END-IF
             END-IF
           END-PERFORM
      *
           .
       2003-NSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    未使用分入力コード情報抽出処理
      *****************************************************************
       2001-ICDGET-SEC                SECTION.
      * 
      *    入院患者読み込み
           INITIALIZE                   INPUTCD-REC
           PERFORM   900-INPUTCD-SELECT-SEC
           PERFORM   900-INPUTCD-FETCH-SEC
           PERFORM   UNTIL    FLG-INPUTCD  =  1
                     PERFORM  2003-ICDSYORI-SEC
                     PERFORM  900-INPUTCD-FETCH-SEC
           END-PERFORM
           PERFORM   900-INPUTCD-CLOSE-SEC
      *
           .
       2001-ICDGET-EXT.
           EXIT.
      *
      *****************************************************************
      *    未使用分入力コード情報  診療行為回数一時ファイルヘの書込
      *****************************************************************
       2003-ICDSYORI-SEC                SECTION.
      *
           IF  ICD-SRYCD NOT =     SPACE
      ***   点数マスタ読込み
             MOVE      ICD-SRYCD    TO    TNS-SRYCD
             PERFORM   910-TENSU-READ-SEC
      *
             IF      FLG-TENSU      =     ZERO
      ***     AND ( TNS-DATAKBN    =     1
      ***       OR  TNS-DATAKBN    =     2 )
               PERFORM 2003-ICDA010PUT-SEC
             END-IF
           END-IF
      *
           .
       2003-ICDSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為回数回数一時ファイルヘの書込
      *****************************************************************
       2003-A010PUT-SEC                SECTION.
      *
           INITIALIZE   A010T
           INITIALIZE   FLG-INVALID
      * 20080902 ０回の診療行為時の不具合の修正START
           INITIALIZE   A010T3
           INITIALIZE   FLG-INVALID2
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    出力区分の設定
           PERFORM   2003-SET-OUTTYPE-SEC
      *
      *    入外区分の設定
           MOVE SRY-NYUGAIKBN   TO    A010T-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE SRY-NYUGAIKBN   TO    A010T3-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      ***  20040727  @@@@-MOTOHASI  入外合計
           IF  WRK-PARA-NYUGAIKBN  =  "3"
             MOVE "0"             TO    A010T-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正START
             MOVE "0"             TO    A010T3-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正END
           END-IF
      ***  ここまで
      *
      * 2004/01/29追加 
      *    カナ名称
           IF   WRK-PARA-PRSORT  =     "2"
               OR  WRK-PARA-PRSORT  =     "9"
             MOVE TNS-KANANAME      TO    A010T-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正START
             MOVE TNS-KANANAME      TO    A010T3-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正END
           END-IF
      * 2004/01/29追加 
      *
           MOVE SRY-SRYCD(IDX1) TO    A010T-SRYCD
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE SRY-SRYCD(IDX1) TO    A010T3-SRYCD
      * 20080902 ０回の診療行為時の不具合の修正END
      * 20071105 きざみ値による点数の不具合を修正START t.tanaka
      * 点数の設定
           MOVE TNS-TEN         TO    A010T-TEN
           MOVE TNS-TEN         TO    A010T3-TEN
           DISPLAY SRY-SRYCD(IDX1) "   |   "A010T-OUTTYPE
      * --- 2008/11/20  ADD  START -------------------------------------
           IF   A010T-OUTTYPE         =    "95"
             OR  A010T-OUTTYPE         =    "96"
              MOVE SRY-JIHIMONEY(IDX1)   TO    A010T-TEN
              MOVE SRY-JIHIMONEY(IDX1)   TO    A010T-TEN2
              MOVE SRY-JIHIMONEY(IDX1)   TO    A010T3-TEN
           END-IF
      * --- 2008/11/20  ADD  END ---------------------------------------
           IF   TNS-KZMCOMPSIKIBETU   = 1
             MOVE TNS-TEN         TO    A010T-TEN2
             IF  SRY-SRYSURYO(IDX1)  >=  TNS-KZMJGN
               COMPUTE  WRK-KIZAMI  =  TNS-KZMJGN - TNS-KZMKGN
             ELSE
               COMPUTE  WRK-KIZAMI  =  SRY-SRYSURYO(IDX1) - TNS-KZMKGN
             END-IF
             MOVE  1  TO  A010T-KZMWAKE
             
             PERFORM    UNTIL WRK-KIZAMI <= 0
               COMPUTE  WRK-KIZAMI  =  WRK-KIZAMI - TNS-KZM
               COMPUTE  A010T-TEN2  =  A010T-TEN2 + TNS-KZMTEN
             END-PERFORM
           END-IF
      * 20071105 きざみ値による点数の不具合を修正END
           READ A010T-FILE
                KEY      IS      A010T-KEY  
                INVALID  KEY     MOVE  1  TO FLG-INVALID
           END-READ
      * 20080902 ０回の診療行為時の不具合の修正START
           READ A010T3-FILE
                KEY      IS      A010T3-KEY  
                INVALID  KEY     MOVE  1  TO FLG-INVALID2
           END-READ
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    (点数マスタ)点識別の設定
           MOVE TNS-TENSIKIBETU TO    A010T-TENSIKIBETU
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE TNS-TENSIKIBETU TO    A010T3-TENSIKIBETU
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      * 診療行為名称の設定
           MOVE TNS-NAME        TO    A010T-NAME
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE TNS-NAME        TO    A010T3-NAME
      * 20080902 ０回の診療行為時の不具合の修正END
      * 20071105 きざみ値による点数の不具合を修正START t.tanaka
      * 点数の設定
      *     MOVE TNS-TEN         TO    A010T-TEN
      * 20071105 きざみ値による点数の不具合を修正END
      *
      * 20040129 変更
      *
      ***     IF   TNS-TENSIKIBETU       =    1
      ***      OR  A010T-OUTTYPE         =    "95"
      ***      OR  A010T-OUTTYPE         =    "96"
      *    金額項目の場合は数量×回数の値を総数量としてデータ化する。
      ***          COMPUTE  WRK-ZAIKAISU =
      ***          SRY-SRYSURYO(IDX1)    *    ACCT-ZAIKAISU
      ***          ADD  WRK-ZAIKAISU     TO   A010T-KAISU
      ***     ELSE  
      ***       点数項目の場合は回数をデータ化する。
      ***          ADD  ACCT-ZAIKAISU    TO   A010T-KAISU
      ***     END-IF  
      *
           IF   A010T-OUTTYPE         =    "95"
               OR  A010T-OUTTYPE         =    "96"
      *    金額項目の場合は数量×回数の値を総数量としてデータ化する。
             COMPUTE  WRK-ZAIKAISU =
             SRY-SRYSURYO(IDX1)    *    ACCT-ZAIKAISU
             ADD  WRK-ZAIKAISU     TO   A010T-KAISU
             MOVE 1                TO   A010T-TANI
      * 20080902 ０回の診療行為時の不具合の修正START
             ADD  WRK-ZAIKAISU     TO   A010T3-KAISU
             MOVE 1                TO   A010T3-TANI
      * 20080902 ０回の診療行為時の不具合の修正END
           ELSE
             EVALUATE  TNS-TENSIKIBETU
               WHEN  1
      *    金額項目の場合は数量×回数の値を総数量としてデータ化する。
                 COMPUTE  WRK-ZAIKAISU =
                         SRY-SRYSURYO(IDX1)  *  ACCT-ZAIKAISU
                 ADD  WRK-ZAIKAISU     TO   A010T-KAISU
                 MOVE 1                TO   A010T-TANI
      * 20080902 ０回の診療行為時の不具合の修正START
                 ADD  WRK-ZAIKAISU     TO   A010T3-KAISU
                 MOVE 1                TO   A010T3-TANI
      * 20080902 ０回の診療行為時の不具合の修正END
               WHEN  3
               WHEN  8
      *    点数項目の場合は回数をデータ化する。
                 ADD  ACCT-ZAIKAISU    TO   A010T-KAISU
                 MOVE ZERO             TO   A010T-TANI
      * 20080902 ０回の診療行為時の不具合の修正START
                 ADD  ACCT-ZAIKAISU    TO   A010T3-KAISU
                 MOVE ZERO             TO   A010T3-TANI
      * 20080902 ０回の診療行為時の不具合の修正END
               WHEN  0
               WHEN  4
               WHEN  5
               WHEN  6
               WHEN  7
      *    点数、金額以外の場合
                 MOVE ZERO             TO   A010T-TEN
                 ADD  ACCT-ZAIKAISU    TO   A010T-KAISU
                 MOVE 9                TO   A010T-TANI
      * 20080902 ０回の診療行為時の不具合の修正START
                     MOVE ZERO             TO   A010T3-TEN
                     ADD  ACCT-ZAIKAISU    TO   A010T3-KAISU
                     MOVE 9                TO   A010T3-TANI
      * 20080902 ０回の診療行為時の不具合の修正END
               END-EVALUATE
           END-IF   
      * 20040129 変更
      *
           IF   FLG-INVALID      =    ZERO 
             INITIALIZE       FLG-INVALID
             REWRITE A010T
                     INVALID  KEY
                     MOVE  1  TO FLG-INVALID
             END-REWRITE
           ELSE
             INITIALIZE       FLG-INVALID
             WRITE A010T
                     INVALID  KEY
                     MOVE  1  TO FLG-INVALID
             END-WRITE
           END-IF
      * 20080902 ０回の診療行為時の不具合の修正START
           IF   FLG-INVALID2      =    ZERO 
             INITIALIZE       FLG-INVALID2
             REWRITE A010T3
               INVALID  KEY  
               MOVE  1  TO FLG-INVALID2
             END-REWRITE
           ELSE
             INITIALIZE       FLG-INVALID2
             WRITE A010T3
               INVALID  KEY  
               MOVE  1  TO FLG-INVALID2
             END-WRITE
           END-IF
      * 20080902 ０回の診療行為時の不具合の修正END
      *
           .
       2003-A010PUT-EXT.
           EXIT.
      *****************************************************************
      *    (入院データ) 診療行為回数一時ファイルヘの書込
      *****************************************************************
       2003-NA010PUT-SEC                SECTION.
      *
           INITIALIZE   A010T
           INITIALIZE   FLG-INVALID
      * 20080902 ０回の診療行為時の不具合の修正START
           INITIALIZE   A010T3
           INITIALIZE   FLG-INVALID2
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    出力区分の設定
           PERFORM   2003-SET-OUTTYPE-SEC
      *
      *    入外区分の設定
           MOVE NSRY-NYUGAIKBN   TO    A010T-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE NSRY-NYUGAIKBN   TO    A010T3-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      ***  20040727  @@@@-MOTOHASI  入外合計
           IF  WRK-PARA-NYUGAIKBN  =  "3"
             MOVE "0"             TO    A010T-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正START
             MOVE "0"             TO    A010T3-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正END
           END-IF
      ***  ここまで
      *
      * 2004/01/29追加 
      *    カナ名称
           IF   WRK-PARA-PRSORT  =     "2"
               OR  WRK-PARA-PRSORT  =     "9"
             MOVE TNS-KANANAME      TO    A010T-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正START
             MOVE TNS-KANANAME      TO    A010T3-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正END
           END-IF
      * 2004/01/29追加 
      *
           MOVE NSRY-SRYCD(IDX1) TO    A010T-SRYCD
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE NSRY-SRYCD(IDX1) TO    A010T3-SRYCD
      * 20080902 ０回の診療行為時の不具合の修正END
      *
           READ A010T-FILE
                KEY      IS      A010T-KEY  
                INVALID  KEY     MOVE  1  TO FLG-INVALID
           END-READ
      * 20080902 ０回の診療行為時の不具合の修正START
           READ A010T3-FILE
                KEY      IS      A010T3-KEY  
                INVALID  KEY     MOVE  1  TO FLG-INVALID2
           END-READ
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      * (点数マスタ)点識別の設定
           MOVE TNS-TENSIKIBETU  TO    A010T-TENSIKIBETU
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE TNS-TENSIKIBETU  TO    A010T3-TENSIKIBETU
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    診療行為名称の設定
           MOVE TNS-NAME         TO    A010T-NAME
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE TNS-NAME         TO    A010T3-NAME
      * 20080902 ０回の診療行為時の不具合の修正END
      * 点数の設定
           MOVE TNS-TEN          TO    A010T-TEN
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE TNS-TEN          TO    A010T3-TEN
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      * 20040129 変更
      ***     IF   TNS-TENSIKIBETU       =    1
      ***      OR  A010T-OUTTYPE         =    "95"
      ***      OR  A010T-OUTTYPE         =    "96"
      ***     金額項目の場合は数量×回数の値を総数量としてデータ化する。
      ***          COMPUTE  WRK-ZAIKAISU =
      ***          NSRY-SRYSURYO(IDX1)   *    NACCT-ZAIKAISU
      ***          ADD  WRK-ZAIKAISU     TO   A010T-KAISU
      ***     ELSE  
      ******       点数項目の場合は回数をデータ化する。
      ***          ADD  NACCT-ZAIKAISU   TO   A010T-KAISU
      ***     END-IF  
      *
      ***  20040714  @@@@-MOTOHASI  SRYACT、SRYACCTを参照している不具合修正
      ***     IF   A010T-OUTTYPE         =    "95"
      ***      OR  A010T-OUTTYPE         =    "96"
      *    金額項目の場合は数量×回数の値を総数量としてデータ化する。
      ***         COMPUTE  WRK-ZAIKAISU =
      ***         SRY-SRYSURYO(IDX1)    *    ACCT-ZAIKAISU
      ***         ADD  WRK-ZAIKAISU     TO   A010T-KAISU
      ***         MOVE 1                TO   A010T-TANI
      ***     ELSE
      ***         EVALUATE  TNS-TENSIKIBETU
      ***         WHEN  1
      *    金額項目の場合は数量×回数の値を総数量としてデータ化する。
      ***               COMPUTE  WRK-ZAIKAISU =
      ***               SRY-SRYSURYO(IDX1)    *    ACCT-ZAIKAISU
      ***               ADD  WRK-ZAIKAISU     TO   A010T-KAISU
      ***               MOVE 1                TO   A010T-TANI
      ***         WHEN  3
      ***         WHEN  8
      ******            点数項目の場合は回数をデータ化する。
      ***               ADD  ACCT-ZAIKAISU    TO   A010T-KAISU
      ***               MOVE ZERO             TO   A010T-TANI
      ***         WHEN  0
      ***         WHEN  4
      ***         WHEN  5
      ***         WHEN  6
      ***         WHEN  7
      ******           点数、金額以外の場合
      ***               MOVE ZERO             TO   A010T-TEN
      ***               ADD  ACCT-ZAIKAISU    TO   A010T-KAISU
      ***               MOVE 9                TO   A010T-TANI
      ***         END-EVALUATE
      ***     END-IF   
      *
           IF   A010T-OUTTYPE         =    "95"
               OR  A010T-OUTTYPE         =    "96"
      *    金額項目の場合は数量×回数の値を総数量としてデータ化する。
             COMPUTE  WRK-ZAIKAISU =
             NSRY-SRYSURYO(IDX1)   *    NACCT-ZAIKAISU
             ADD  WRK-ZAIKAISU     TO   A010T-KAISU
             MOVE 1                TO   A010T-TANI
      * 20080902 ０回の診療行為時の不具合の修正START
             ADD  WRK-ZAIKAISU     TO   A010T3-KAISU
             MOVE 1                TO   A010T3-TANI
      * 20080902 ０回の診療行為時の不具合の修正END
           ELSE
             EVALUATE  TNS-TENSIKIBETU
               WHEN  1
      *    金額項目の場合は数量×回数の値を総数量としてデータ化する。
                 COMPUTE  WRK-ZAIKAISU =
                        NSRY-SRYSURYO(IDX1)   *    NACCT-ZAIKAISU
                 ADD  WRK-ZAIKAISU     TO   A010T-KAISU
                 MOVE 1                TO   A010T-TANI
      * 20080902 ０回の診療行為時の不具合の修正START
                 ADD  WRK-ZAIKAISU     TO   A010T3-KAISU
                 MOVE 1                TO   A010T3-TANI
      * 20080902 ０回の診療行為時の不具合の修正END
               WHEN  3
               WHEN  8
      ***         点数項目の場合は回数をデータ化する。
                 ADD   NACCT-ZAIKAISU   TO   A010T-KAISU
                 MOVE       ZERO       TO   A010T-TANI
      * 20080902 ０回の診療行為時の不具合の修正START
                 ADD   NACCT-ZAIKAISU   TO   A010T3-KAISU
                 MOVE       ZERO       TO   A010T3-TANI
      * 20080902 ０回の診療行為時の不具合の修正END
               WHEN  0
               WHEN  4
               WHEN  5
               WHEN  6
               WHEN  7
      ***           点数、金額以外の場合
                 MOVE  ZERO             TO   A010T-TEN
                 ADD   NACCT-ZAIKAISU   TO   A010T-KAISU
                 MOVE  9                TO   A010T-TANI
      * 20080902 ０回の診療行為時の不具合の修正START
                 MOVE  ZERO             TO   A010T3-TEN
                 ADD   NACCT-ZAIKAISU   TO   A010T3-KAISU
                 MOVE  9                TO   A010T3-TANI
      * 20080902 ０回の診療行為時の不具合の修正END
             END-EVALUATE
           END-IF   
      ***  20040714  ここまで
      *
      * 20040129 変更
      *
           IF   FLG-INVALID      =    ZERO
                INITIALIZE       FLG-INVALID
                REWRITE A010T
                        INVALID  KEY  
                        MOVE  1  TO FLG-INVALID
                END-REWRITE
           ELSE
                INITIALIZE       FLG-INVALID
                WRITE A010T
                        INVALID  KEY  
                        MOVE  1  TO FLG-INVALID
                END-WRITE
           END-IF
      * 20080902 ０回の診療行為時の不具合の修正START
           IF   FLG-INVALID2      =    ZERO 
                INITIALIZE       FLG-INVALID2
                REWRITE A010T3
                        INVALID  KEY
                        MOVE  1  TO FLG-INVALID2
                END-REWRITE
           ELSE
                INITIALIZE       FLG-INVALID2
                WRITE A010T3
                        INVALID  KEY
                        MOVE  1  TO FLG-INVALID2
                END-WRITE
           END-IF
      * 20080902 ０回の診療行為時の不具合の修正END
      *
           .
       2003-NA010PUT-EXT.
           EXIT.
      *****************************************************************
      *    食事療養費 診療行為回数一時ファイルヘの書込 2004/07/23
      *****************************************************************
       2004-NA010PUT-SEC                SECTION.
      *
      *    区分けの設定(1:食事療養のみ、2:食事療養＋特食、
      *              3:食事療養＋選択食、4:食事療養＋特食＋選択食)
           INITIALIZE   WRK-SYOKUJIKAISU
           INITIALIZE   WRK-SYOKUJIKAI
      ****2006/01/07 @@@@-BRAND  START
      *     PERFORM   2004-SET-SYOKUJITYPE-SEC
           PERFORM VARYING IIII FROM 1 BY 1
	               UNTIL  IIII > 31
             PERFORM VARYING ZZZZ FROM 1 BY 1
                      UNTIL  ZZZZ > 4
               COMPUTE   SSSS = IIII * 3 - 2
               IF ZZZZ = NACCT-DAY-TBL(SSSS:3)
                 IF  ZZZZ = 1
                   ADD 1 TO WRK-SYOKUJIKAISU1
                 ELSE 
                   IF ZZZZ = 2
                     ADD 1 TO WRK-SYOKUJIKAISU2
                   ELSE
                     IF ZZZZ = 3
                       ADD 1  TO WRK-SYOKUJIKAISU3
                     ELSE
                       IF ZZZZ = 4
                         ADD 1  TO WRK-SYOKUJIKAISU4
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-PERFORM
           END-PERFORM
      ****2006/01/07 @@@@-BRAND  START
      *
           PERFORM  VARYING      IDX4  FROM  1  BY   1
                    UNTIL        IDX4  >  4
      *
             EVALUATE  IDX4
               WHEN  1
                 MOVE WRK-SYOKUJIKAISU1   TO    WRK-SYOKUJIKAISU
               WHEN  2
                 MOVE WRK-SYOKUJIKAISU2   TO    WRK-SYOKUJIKAISU
               WHEN  3
                 MOVE WRK-SYOKUJIKAISU3   TO    WRK-SYOKUJIKAISU
               WHEN  4
                 MOVE WRK-SYOKUJIKAISU4   TO    WRK-SYOKUJIKAISU
             END-EVALUATE
      *
      *    回数が0の場合は書き込まない
             IF  WRK-SYOKUJIKAISU  NOT = 0
               INITIALIZE   A010T
               INITIALIZE   FLG-INVALID
      * 20080902 ０回の診療行為時の不具合の修正START
               INITIALIZE   A010T3
               INITIALIZE   FLG-INVALID2
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    出力区分の設定
               PERFORM   2003-SET-OUTTYPE-SEC
      *
      *    入外区分の設定
               MOVE NSRY-NYUGAIKBN   TO    A010T-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE NSRY-NYUGAIKBN   TO    A010T3-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      ***  20040727  @@@@-MOTOHASI  入外合計
               IF  WRK-PARA-NYUGAIKBN  =  "3"
                 MOVE "0"             TO    A010T-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正START
                 MOVE "0"             TO    A010T3-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正END
               END-IF
      ***  ここまで
      *
      *    カナ名称
               IF   WRK-PARA-PRSORT  =     "2"
                   OR  WRK-PARA-PRSORT  =     "9"
                 MOVE TNS-KANANAME      TO    A010T-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正START
                 MOVE TNS-KANANAME      TO    A010T3-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正END
               END-IF
      *
               MOVE NSRY-SRYCD(IDX1) TO    A010T-SRYCD
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE NSRY-SRYCD(IDX1) TO    A010T3-SRYCD
      * 20080902 ０回の診療行為時の不具合の修正END
      *
               MOVE IDX4             TO    A010T-BUNRUICD
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE IDX4             TO    A010T3-BUNRUICD
      * 20080902 ０回の診療行為時の不具合の修正END
      *
               READ A010T-FILE
                    KEY      IS      A010T-KEY
                    INVALID  KEY     MOVE  1  TO FLG-INVALID
               END-READ
      * 20080902 ０回の診療行為時の不具合の修正START
               READ A010T3-FILE
                    KEY      IS      A010T3-KEY
                    INVALID  KEY     MOVE  1  TO FLG-INVALID2
               END-READ
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    (点数マスタ)点識別の設定
               MOVE TNS-TENSIKIBETU  TO    A010T-TENSIKIBETU
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE TNS-TENSIKIBETU  TO    A010T3-TENSIKIBETU
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    診療行為名称の設定
               EVALUATE  IDX4
                 WHEN  1
                   MOVE "01：食事療養のみ"            TO  A010T-NAME
      *** 20070717食事療養費の修正START　T.Tanaka
                   EVALUATE NSRY-SRYCD(IDX1) 
                     WHEN 099999918
                       MOVE "01：食事療養のみ(朝)"    TO  A010T-NAME
                     WHEN 099999919
                       MOVE "01：食事療養のみ(昼)"    TO  A010T-NAME
                     WHEN 099999920
                       MOVE "01：食事療養のみ(夕)"    TO  A010T-NAME
                   END-EVALUATE
      *** 20070717食事療養費の修正END
                 WHEN  2
                   MOVE "02：食事療養＋特食"          TO  A010T-NAME
                 WHEN  3
                   MOVE "03：食事療養＋選択食"        TO  A010T-NAME
                 WHEN  4
                   MOVE "04：食事療養＋特食＋選択食"  TO  A010T-NAME
               END-EVALUATE


      *    点数の設定
               EVALUATE  IDX4
                 WHEN  1
                   MOVE WRK-SYOKUJITEN1     TO    A010T-TEN
                 WHEN  2
                   MOVE WRK-SYOKUJITEN2     TO    A010T-TEN
                 WHEN  3
                   MOVE WRK-SYOKUJITEN3     TO    A010T-TEN
                 WHEN  4
                   MOVE WRK-SYOKUJITEN4     TO    A010T-TEN
               END-EVALUATE
      *    回数の設定
      *
               EVALUATE  IDX4
                 WHEN  1
                   ADD  WRK-SYOKUJIKAISU1   TO   A010T-KAISU
                   MOVE ZERO                TO   A010T-TANI
                 WHEN  2
                   ADD  WRK-SYOKUJIKAISU2   TO   A010T-KAISU
                   MOVE ZERO                TO   A010T-TANI
                 WHEN  3
                   ADD  WRK-SYOKUJIKAISU3   TO   A010T-KAISU
                   MOVE ZERO                TO   A010T-TANI
                 WHEN  4
                   ADD  WRK-SYOKUJIKAISU4   TO   A010T-KAISU
                   MOVE ZERO                TO   A010T-TANI
               END-EVALUATE
      *
               IF   FLG-INVALID      =    ZERO
                 INITIALIZE       FLG-INVALID
                 REWRITE A010T
                         INVALID  KEY
                         MOVE  1  TO FLG-INVALID
                 END-REWRITE
               ELSE
                 INITIALIZE       FLG-INVALID
                 WRITE   A010T
                         INVALID  KEY
                         MOVE  1  TO FLG-INVALID
                 END-WRITE
               END-IF
      * 20080902 ０回の診療行為時の不具合の修正START
               IF   FLG-INVALID2      =    ZERO 
                 INITIALIZE       FLG-INVALID2 
                 REWRITE A010T3
                         INVALID  KEY
                         MOVE  1  TO FLG-INVALID2
                 END-REWRITE
               ELSE
                 INITIALIZE       FLG-INVALID2
                 WRITE   A010T3
                         INVALID  KEY
                         MOVE  1  TO FLG-INVALID2
                 END-WRITE
               END-IF
      * 20080902 ０回の診療行為時の不具合の修正END
             END-IF
           END-PERFORM
      *
           .
       2004-NA010PUT-EXT.
           EXIT.
      *****************************************************************
      *    室料差額 診療行為回数一時ファイルヘの書込 2004/07/23
      *****************************************************************
       2005-NA010PUT-SEC                SECTION.
      *
      *    区分けの設定(1〜30)
      *              
           INITIALIZE   WRK-SITURYOKAI-TBL
      ****2006/01/07 @@@@-BRAND  START
      *     PERFORM   2005-SET-SITURYOTYPE-SEC
           PERFORM VARYING IIII FROM 1 BY 1
	               UNTIL  IIII > 31
             PERFORM VARYING ZZZZ FROM 1 BY 1
                     UNTIL  ZZZZ > 30
               COMPUTE   SSSS = IIII * 3 - 2
               IF  NACCT-DAY-TBL(SSSS:3) = ZZZZ
                 ADD 1 TO  WRK-SITURYOKAI(ZZZZ)
	           END-IF
	         END-PERFORM
           END-PERFORM
      *
      ****2006/01/07 @@@@-BRAND  END
      *
           PERFORM  VARYING      IDX5  FROM  1  BY   1
      ****2005/12/27 @@@@-BRAND  START
      *             UNTIL        IDX5  >  20
                    UNTIL        IDX5 > 30
      ****2005/12/27 @@@@-BRAND  END
      *
      *    回数が0の場合は書き込まない
             IF  WRK-SITURYOKAI(IDX5)  NOT = 0
               INITIALIZE   A010T
               INITIALIZE   FLG-INVALID
      * 20080902 ０回の診療行為時の不具合の修正START
               INITIALIZE   A010T3
               INITIALIZE   FLG-INVALID2
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    出力区分の設定
               PERFORM   2003-SET-OUTTYPE-SEC
      *
      *    入外区分の設定
               MOVE NSRY-NYUGAIKBN   TO    A010T-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE NSRY-NYUGAIKBN   TO    A010T3-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      ***  20040727  @@@@-MOTOHASI  入外合計
               IF  WRK-PARA-NYUGAIKBN  =  "3"
                 MOVE "0"             TO    A010T-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正START
                 MOVE "0"             TO    A010T3-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正END
               END-IF
      ***  ここまで
      *
      *    カナ名称
               IF   WRK-PARA-PRSORT  =     "2"
                   OR  WRK-PARA-PRSORT  =     "9"
                 MOVE TNS-KANANAME      TO    A010T-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正START
                 MOVE TNS-KANANAME      TO    A010T-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正END
               END-IF
      *
               MOVE NSRY-SRYCD(IDX1) TO    A010T-SRYCD
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE NSRY-SRYCD(IDX1) TO    A010T3-SRYCD
      * 20080902 ０回の診療行為時の不具合の修正END
      *
               MOVE IDX5             TO    A010T-BUNRUICD
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE IDX5             TO    A010T3-BUNRUICD
      * 20080902 ０回の診療行為時の不具合の修正END
      *
               READ A010T-FILE
                 KEY      IS      A010T-KEY  
                 INVALID  KEY     MOVE  1  TO FLG-INVALID
               END-READ
      * 20080902 ０回の診療行為時の不具合の修正START
               READ A010T3-FILE
                 KEY      IS      A010T3-KEY  
                 INVALID  KEY     MOVE  1  TO FLG-INVALID2
               END-READ
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    (点数マスタ)点識別の設定
               MOVE TNS-TENSIKIBETU  TO    A010T-TENSIKIBETU
      *
      *    診療行為名称の設定
               EVALUATE  IDX5
                 WHEN  1
                   MOVE "01：室料差額"            TO  WRK-NAME
                 WHEN  2
                   MOVE "02：室料差額"            TO  WRK-NAME
                 WHEN  3
                   MOVE "03：室料差額"            TO  WRK-NAME
                 WHEN  4
                   MOVE "04：室料差額"            TO  WRK-NAME
                 WHEN  5
                   MOVE "05：室料差額"            TO  WRK-NAME
                 WHEN  6
                   MOVE "06：室料差額"            TO  WRK-NAME
                 WHEN  7
                   MOVE "07：室料差額"            TO  WRK-NAME
                 WHEN  8
                   MOVE "08：室料差額"            TO  WRK-NAME
                 WHEN  9
                   MOVE "09：室料差額"            TO  WRK-NAME
                 WHEN  10
                   MOVE "10：室料差額"            TO  WRK-NAME
                 WHEN  11
                   MOVE "11：室料差額"            TO  WRK-NAME
                 WHEN  12
                   MOVE "12：室料差額"            TO  WRK-NAME
                 WHEN  13
                   MOVE "13：室料差額"            TO  WRK-NAME
                 WHEN  14
                   MOVE "14：室料差額"            TO  WRK-NAME
                 WHEN  15
                   MOVE "15：室料差額"            TO  WRK-NAME
                 WHEN  16
                   MOVE "16：室料差額"            TO  WRK-NAME
                 WHEN  17
                   MOVE "17：室料差額"            TO  WRK-NAME
                 WHEN  18
                   MOVE "18：室料差額"            TO  WRK-NAME
                 WHEN  19
                   MOVE "19：室料差額"            TO  WRK-NAME
                 WHEN  20 
                   MOVE "20：室料差額"            TO  WRK-NAME
      ****2005/12/27 @@@@-BRAND  START
                 WHEN  21
                   MOVE "21：室料差額"            TO  WRK-NAME
                 WHEN  22
                   MOVE "22：室料差額"            TO  WRK-NAME
                 WHEN  23
                   MOVE "23：室料差額"            TO  WRK-NAME
                 WHEN  24
                   MOVE "24：室料差額"            TO  WRK-NAME
                 WHEN  25
                   MOVE "25：室料差額"            TO  WRK-NAME
                 WHEN  26
                   MOVE "26：室料差額"            TO  WRK-NAME
                 WHEN  27
                   MOVE "27：室料差額"            TO  WRK-NAME
                 WHEN  28
                   MOVE "28：室料差額"            TO  WRK-NAME
                 WHEN  29
                   MOVE "29：室料差額"            TO  WRK-NAME
                 WHEN  30
                   MOVE "30：室料差額"            TO  WRK-NAME
      ****2005/12/27 @@@@-BRAND  END
               END-EVALUATE
      *
               MOVE WRK-SITURYOYEN(IDX5)  TO  WRK-Z7
               INITIALIZE        FLG-SPACE
               INITIALIZE        IDX6
      ****2005/10/17  @@@@-BRAND START
      *        PERFORM   UNTIL   FLG-SPACE =   1
      *                    OR    IDX6  >= 20
               PERFORM   UNTIL   FLG-SPACE =   1
                         OR    IDX6  >= 7 
      ****2005/10/17  @@@@-BRAND END
                 ADD  1       TO    IDX6
                 IF   WRK-Z7(IDX6:1)   =  "0" OR "1" OR "2" OR "3" OR
                                          "4" OR "5" OR "6" OR "7" OR
                                          "8" OR "9" 
                   MOVE    1    TO    FLG-SPACE
                 END-IF
               END-PERFORM
      ****2005/10/17  @@@@-BRAND START
               INITIALIZE        IDX7
               COMPUTE  IDX7 = 8 - IDX6
      *          MOVE WRK-Z7(IDX6:)         TO  WRK-NAME(13:)
               MOVE WRK-Z7(IDX6:IDX7)         TO  WRK-NAME(13:IDX7)
      ****2005/10/17  @@@@-BRAND END
               MOVE WRK-NAME              TO  A010T-NAME
      *
               INITIALIZE        FLG-SPACE
               MOVE      1       TO     IDX6
               PERFORM   UNTIL   FLG-SPACE =   1
                         OR    IDX6  >= 30
                 IF   A010T-NAME(IDX6:1)  =  SPACE
                   MOVE    1    TO    FLG-SPACE
                 END-IF
                 ADD  2       TO    IDX6
               END-PERFORM
               SUBTRACT      3        FROM IDX6  
               IF  A010T-NAME(IDX6:1) = SPACE
                 ADD  1       TO    IDX6
                 MOVE "円"              TO   A010T-NAME(IDX6:)
               ELSE
                 ADD  2       TO    IDX6
                 MOVE "円"              TO   A010T-NAME(IDX6:)
               END-IF
      *    点数の設定
               MOVE WRK-SITURYOYEN(IDX5)    TO   A010T-TEN
      *    回数の設定
               ADD  WRK-SITURYOKAI(IDX5)    TO   A010T-KAISU
               MOVE 1                       TO   A010T-TANI
      *
               IF   FLG-INVALID      =    ZERO 
                 INITIALIZE       FLG-INVALID
                 REWRITE A010T
                         INVALID  KEY
                         MOVE  1  TO FLG-INVALID
                 END-REWRITE
               ELSE
                 INITIALIZE       FLG-INVALID
                 WRITE   A010T
                         INVALID  KEY  
                         MOVE  1  TO  FLG-INVALID
                 END-WRITE
               END-IF
      * 20080902 ０回の診療行為時の不具合の修正START
               IF   FLG-INVALID2      =    ZERO 
                 INITIALIZE       FLG-INVALID2
                 REWRITE A010T3
                          INVALID  KEY  
                          MOVE  1  TO FLG-INVALID2
                 END-REWRITE
               ELSE
                 INITIALIZE       FLG-INVALID2     
                 WRITE A010T3
                       INVALID  KEY  
                       MOVE  1  TO FLG-INVALID2
                 END-WRITE
               END-IF
      * 20080902 ０回の診療行為時の不具合の修正END
             END-IF
           END-PERFORM
      *
           .
       2005-NA010PUT-EXT.
           EXIT.
      *****************************************************************
      *    (未使用分入力コード)診療行為回数一時ファイルヘの書込
      *****************************************************************
       2003-ICDA010PUT-SEC                SECTION.
      *
           INITIALIZE   A010T
           INITIALIZE   FLG-INVALID
      * 20080902 ０回の診療行為時の不具合の修正START
           INITIALIZE   A010T3
           INITIALIZE   FLG-INVALID2
      * 20080902 ０回の診療行為時の不具合の修正END
      *  
      *    出力区分の設定
           PERFORM   2003-SET-OUTTYPE-SEC
      *
      *    診療行為コードの設定
           MOVE ICD-SRYCD        TO    A010T-SRYCD
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE ICD-SRYCD        TO    A010T3-SRYCD
      * 20080902 ０回の診療行為時の不具合の修正END
      *
           INITIALIZE    FLG-NICDINV
           INITIALIZE    FLG-GICDINV
      *
      *    入院データの未入力チェック
           MOVE "1"              TO    A010T-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE "1"              TO    A010T3-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      * 2004/01/29追加 
      *    カナ名称
           IF   WRK-PARA-PRSORT  =     "2"
               OR   WRK-PARA-PRSORT  =     "9"
             MOVE TNS-KANANAME      TO    A010T-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正START
             MOVE TNS-KANANAME      TO    A010T3-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正END
           END-IF
      * 2004/01/29追加 
      *
           READ A010T-FILE
                KEY      IS      A010T-KEY  
      * 20080902 ０回の診療行為時の不具合の修正START
      *          INVALID  KEY     MOVE  1  TO FLG-NICDINV
      * 20080902 ０回の診療行為時の不具合の修正END
           END-READ
      * 20080902 ０回の診療行為時の不具合の修正START
           READ A010T3-FILE
                KEY      IS      A010T3-KEY  
                INVALID  KEY     MOVE  1  TO FLG-NICDINV
           END-READ
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      *    外来データの未入力チェック
           MOVE "2"              TO    A010T-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正START
           MOVE "2"              TO    A010T3-NYUGAIKBN
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      * 2004/01/29追加 
      *    カナ名称
           IF   WRK-PARA-PRSORT  =     "2"
               OR  WRK-PARA-PRSORT  =     "9"
             MOVE TNS-KANANAME      TO    A010T-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正START
             MOVE TNS-KANANAME      TO    A010T3-KANANAME
      * 20080902 ０回の診療行為時の不具合の修正END
           END-IF
      * 2004/01/29追加 
      *
           READ A010T-FILE
                KEY      IS      A010T-KEY  
      * 20080902 ０回の診療行為時の不具合の修正START
      *          INVALID  KEY     MOVE  1  TO FLG-GICDINV
      * 20080902 ０回の診療行為時の不具合の修正END
           END-READ
      * 20080902 ０回の診療行為時の不具合の修正START
           READ A010T3-FILE
                KEY      IS      A010T3-KEY  
                INVALID  KEY     MOVE  1  TO FLG-GICDINV
           END-READ
      * 20080902 ０回の診療行為時の不具合の修正END
      *
           IF  FLG-NICDINV      =   1
               AND FLG-GICDINV      =   1
      * 
      *    入院、外来共に未使用の場合は未使用としてデータを抽出する。
      *
      *        入外区分の設定
             MOVE "9"             TO    A010T-NYUGAIKBN
      *
      * 2004/01/29追加 
      *    カナ名称の設定
             IF   WRK-PARA-PRSORT  =    "2"
                 OR WRK-PARA-PRSORT  =    "9"
               MOVE TNS-KANANAME     TO    A010T-KANANAME
             END-IF
      * 2004/01/29追加 
      *
      *    (点数マスタ)点識別の設定
             MOVE TNS-TENSIKIBETU TO    A010T-TENSIKIBETU
      *
      *    診療行為名称の設定
             MOVE TNS-NAME        TO    A010T-NAME
      *
      *    点数の設定
             MOVE TNS-TEN         TO    A010T-TEN
      *
      * 2004/01/29追加 
             IF   A010T-OUTTYPE      =     "95"
                 OR  A010T-OUTTYPE      =     "96"
               MOVE 1             TO    A010T-TANI
             ELSE
               EVALUATE  TNS-TENSIKIBETU
                 WHEN  1
      *    金額の場合
                   MOVE 1          TO    A010T-TANI
                 WHEN  3
                 WHEN  8
      *    点数の場合
                     MOVE ZERO     TO    A010T-TANI
                 WHEN  0
                 WHEN  4
                 WHEN  5
                 WHEN  6
                 WHEN  7
      *    点数、金額以外の場合
                   MOVE ZERO       TO    A010T-TEN
                   MOVE 9          TO    A010T-TANI
               END-EVALUATE  
             END-IF   
      * 2004/01/29追加 
      *
      *    回数の設定
             MOVE ZERO             TO    A010T-KAISU 
      *
             INITIALIZE            FLG-INVALID
             WRITE    A010T
                      INVALID  KEY  
               MOVE 1  TO    FLG-INVALID
             END-WRITE
           END-IF
      *
           .
       2003-ICDA010PUT-EXT.
           EXIT.
      *****************************************************************
      *    出力区分判定処理
      *****************************************************************
       2003-SET-OUTTYPE-SEC                SECTION.
      *
           EVALUATE  TNS-SRYCD
             WHEN  "101000000"   THRU  "114999999"
      *    初診、再診、指導、在宅
               MOVE  "10"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "10"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      * 20080526 診療行為区分の振り分けを修正STRAT tanaka
               EVALUATE   TNS-SRYKBN
                 WHEN  "80" 
                   MOVE  "80"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "80"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
               END-EVALUATE
      * 20080526 診療行為区分の振り分けを修正END
             WHEN  "610000000"   THRU  "619999999"
      *    内服薬
               MOVE  "21"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "21"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
             WHEN  "640000000"   THRU  "649999999"
      *    注射薬
               MOVE  "22"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "22"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
             WHEN  "660000000"   THRU  "669999999"
      *    外用薬
               MOVE  "23"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "23"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
             WHEN  "120000000"   THRU  "129999999"
      *    投薬
               MOVE  "29"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "29"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      * 20080526 診療行為区分の振り分けを修正STRAT tanaka
               EVALUATE   TNS-SRYKBN
                 WHEN    "11"
                 WHEN    "12"
                 WHEN    "13"
                 WHEN    "14"
                   MOVE   "10"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE   "10"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
               END-EVALUATE
      * 20080526 診療行為区分の振り分けを修正END
             WHEN  "130000000"   THRU  "139999999"
      *    注射、点滴
               MOVE  "30"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "30"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      * 20080418 診療行為区分の振り分けを修正START tanaka
               IF  TNS-SRYKBN = "50"
                 MOVE  "50"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                 MOVE  "50"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
               END-IF
      * 20080418 診療行為区分の振り分けを修正END
             WHEN  "140000000"   THRU  "149999999"
      *    処置
               MOVE  "40"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "40"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
             WHEN  "150000000"   THRU  "159999999"
      *    手術、麻酔
               MOVE  "50"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "50"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      * 20080418 診療行為区分の振り分けを修正START tanaka
               IF  TNS-SRYKBN = "40"
                 MOVE  "40"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                 MOVE  "40"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
               END-IF
      * 20080418 診療行為区分の振り分けを修正END
             WHEN  "160000000"   THRU  "169999999"
      * 20080402 H20年4月改正対応START tanaka
      *               MOVE  "60"    TO    A010T-OUTTYPE
      *    病理診断
               IF WRK-PARA-PRTYM  <  "200804"
                 MOVE  "60"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                 MOVE  "60"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
               ELSE
                 IF  TNS-KNSJISGRPKBN = 7
                   MOVE  "64"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "64"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 ELSE
      *    検査
                   MOVE  "60"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "60"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 END-IF
               END-IF
      * 20080402 H20年4月改正対応END
             WHEN  "170000000"   THRU  "179999999"
      *    画像診断
               MOVE  "70"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "70"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
             WHEN  "700000000"   THRU  "700999999"
      *    フィルム
               MOVE  "71"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "71"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
             WHEN  "180000000"   THRU  "189999999"
      *    理学療法、リハビリ
               MOVE  "80"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "80"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      *20070828 診療行為振り分け修正 START
               IF  TNS-SRYKBN = "13"
                 MOVE  "10"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                 MOVE  "10"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
               END-IF
      *20070828 診療行為振り分け修正 END
             WHEN  "190000000"   THRU  "199999999"
      *    入院
               MOVE  "90"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "90"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      *20070828 診療行為振り分け修正 START
               IF  TNS-SRYKBN = "13"
                 MOVE  "10"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                 MOVE  "10"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
               END-IF
      *20070828 診療行為振り分け修正 END
             WHEN  "705000000"   THRU  "799999999"
      *    器材
               MOVE  "91"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "91"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
             WHEN  "059000001"   THRU  "059999999"
      *    その他材料
               MOVE  "92"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "92"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      ***  20041108  @@@@-MOTOHASI  変更
      ***         WHEN  "095000001"   THRU  "095999999"
             WHEN  "095000000"   THRU  "095999999"
      *    自費消費税なし
               MOVE  "95"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "95"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      ***  20041108  @@@@-MOTOHASI  変更
      ***         WHEN  "096000001"   THRU  "096999999"
             WHEN  "096000000"   THRU  "096999999"
      *    自費消費税あり
               MOVE  "96"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "96"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      ***         WHEN  "001000000"   THRU  "001000999"
             WHEN  "001000000"   THRU  "001999999"
             WHEN  "002000001"   THRU  "002999999"
             WHEN  "810000000"   THRU  "899999999"
      *              用法、画像診断、撮影部位、コメント
               MOVE  "99"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "99"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      *
      ***  20041108  @@@@-MOTOHASI  追加
      ***            外泊(099999911)、室料差額(099999912)、食事(099999913)
      ***            老人特定入院料(099999915)、選定入院料(099999916)
             WHEN  "099999911"   THRU  "099999913"
             WHEN  "099999915"   THRU  "099999916"
      *** 20070717食事療養費の修正START　T.Tanaka
             WHEN  "099999918"   THRU  "099999920"
      *** 20070717食事療養費の修正END
      *              入院WRK-PRT-OUTTYPE
               MOVE  "90"    TO    A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
               MOVE  "90"    TO    A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END

      ***  20041108  @@@@-MOTOHASI  追加
      *
             WHEN  OTHER
      *    その他分類
               EVALUATE    TNS-SRYKBN
                 WHEN    "11"
                 WHEN    "12"
                 WHEN    "13"
                 WHEN    "14"
                   MOVE  "10"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "10"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 WHEN    "40"
                   MOVE  "40"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "40"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 WHEN    "50"
                 WHEN    "54"
                   MOVE  "50"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "50"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 WHEN    "60"
                   MOVE  "60"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "60"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      * 20080402 H20年4月改正対応START tanaka
                 WHEN    "64"
                   MOVE  "64"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "64"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
      * 20080402 H20年4月改正対応END
                 WHEN    "70"
                   MOVE  "70"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "70"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 WHEN    "80"
                   MOVE  "80"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "80"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 WHEN    "90"
                   MOVE  "90"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "90"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 WHEN    "95"
                   MOVE  "95"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "95"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 WHEN    "96"
                   MOVE  "96"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "96"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 WHEN    "99"
                   MOVE  "99"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                   MOVE  "99"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                 WHEN    OTHER
                   EVALUATE  TNS-YKZKBN
                     WHEN  1
                       MOVE  "21"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                       MOVE  "21"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                     WHEN  4
                       MOVE  "22"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                       MOVE  "22"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                     WHEN  6
                       MOVE  "23"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                       MOVE  "23"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                     WHEN  OTHER
                       MOVE  "99"  TO  A010T-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正START
                       MOVE  "99"  TO  A010T3-OUTTYPE
      * 20080902 ０回の診療行為時の不具合の修正END
                   END-EVALUATE 
               END-EVALUATE 
           END-EVALUATE
      *
           .
       2003-SET-OUTTYPE-EXT.
           EXIT.
      *****************************************************************
      *    食事療養区分判定処理
      *****************************************************************
       2004-SET-SYOKUJITYPE-SEC                SECTION.
      *
           EVALUATE  NACCT-DAY-1
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-2
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-3
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
              ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-4
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-5
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      * 
           EVALUATE  NACCT-DAY-6
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-7
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-8
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-9
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-10
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-11
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      * 
     
           EVALUATE  NACCT-DAY-12
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-13
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *  
           EVALUATE  NACCT-DAY-14
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-15
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *       
           EVALUATE  NACCT-DAY-16
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-17
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-18
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-19
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-20
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-21
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-22
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-23
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-24
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-25
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-26
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-27
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-28
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-29
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-30
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-31
             WHEN 1
               ADD  1     TO   WRK-SYOKUJIKAISU1
             WHEN 2
               ADD  1     TO   WRK-SYOKUJIKAISU2
             WHEN 3
               ADD  1     TO   WRK-SYOKUJIKAISU3
             WHEN 4
               ADD  1     TO   WRK-SYOKUJIKAISU4
           END-EVALUATE
      *
           .
       2004-SET-SYOKUJITYPE-EXT.
           EXIT.

      *****************************************************************
      *    室料差額区分判定処理
      *****************************************************************
       2005-SET-SITURYOTYPE-SEC                SECTION.
      *
           EVALUATE  NACCT-DAY-1
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
      
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-2
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-3
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-4
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-5
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-6
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
             ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-7
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-8
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-9
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-10
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-11
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-12
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-13
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-14
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-15
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-16
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-17
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-18
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-19
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24 
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-20
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-21
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-22
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-23
             WHEN 1
                ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
                ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
                ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
                ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
                ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
                ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
                ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
                ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
                ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
                ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
                ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
                ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
                ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
                ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
                ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
                ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
                ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
                ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
                ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
                ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-24
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-25
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-26
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-27
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-28
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-29
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-30
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           EVALUATE  NACCT-DAY-31
             WHEN 1
               ADD  1     TO   WRK-SITURYOKAI(1)
             WHEN 2
               ADD  1     TO   WRK-SITURYOKAI(2)
             WHEN 3
               ADD  1     TO   WRK-SITURYOKAI(3)
             WHEN 4
               ADD  1     TO   WRK-SITURYOKAI(4)
             WHEN 5
               ADD  1     TO   WRK-SITURYOKAI(5)
             WHEN 6
               ADD  1     TO   WRK-SITURYOKAI(6)
             WHEN 7
               ADD  1     TO   WRK-SITURYOKAI(7)
             WHEN 8
               ADD  1     TO   WRK-SITURYOKAI(8)
             WHEN 9
               ADD  1     TO   WRK-SITURYOKAI(9)
             WHEN 10
               ADD  1     TO   WRK-SITURYOKAI(10)
             WHEN 11
               ADD  1     TO   WRK-SITURYOKAI(11)
             WHEN 12
               ADD  1     TO   WRK-SITURYOKAI(12)
             WHEN 13
               ADD  1     TO   WRK-SITURYOKAI(13)
             WHEN 14
               ADD  1     TO   WRK-SITURYOKAI(14)
             WHEN 15
               ADD  1     TO   WRK-SITURYOKAI(15)
             WHEN 16
               ADD  1     TO   WRK-SITURYOKAI(16)
             WHEN 17
               ADD  1     TO   WRK-SITURYOKAI(17)
             WHEN 18
               ADD  1     TO   WRK-SITURYOKAI(18)
             WHEN 19
               ADD  1     TO   WRK-SITURYOKAI(19)
             WHEN 20
               ADD  1     TO   WRK-SITURYOKAI(20)
      ****2006/01/06 @@@@-BRAND  START
             WHEN 21       
               ADD  1     TO   WRK-SITURYOKAI(21)
             WHEN 22  
               ADD  1     TO   WRK-SITURYOKAI(22)
             WHEN 23 
               ADD  1     TO   WRK-SITURYOKAI(23)
             WHEN 24       
               ADD  1     TO   WRK-SITURYOKAI(24)
             WHEN 25  
               ADD  1     TO   WRK-SITURYOKAI(25)
             WHEN 26 
               ADD  1     TO   WRK-SITURYOKAI(26)
             WHEN 27       
               ADD  1     TO   WRK-SITURYOKAI(27)
             WHEN 28  
               ADD  1     TO   WRK-SITURYOKAI(28)
             WHEN 29
               ADD  1     TO   WRK-SITURYOKAI(29)
             WHEN 30 
               ADD  1     TO   WRK-SITURYOKAI(30)
      ****2006/01/06 @@@@-BRNAD  END
           END-EVALUATE
      *
           .
       2005-SET-SITURYOTYPE-EXT.
           EXIT.
      *****************************************************************
      *    検査分類コードの判定(現在未使用)
      *****************************************************************
       2003-KENSABUNRUI-SEC                SECTION.

      *
           INITIALIZE             KENSASORT-REC
           MOVE  SRY-SRYCD(IDX1)  TO  KENSASORT-SRYCD
           PERFORM 940-KENSASORT-READ-SEC

           IF    FLG-KENSASORT    =   ZERO
             EVALUATE  KENSASORT-KNSBUNRUI
               WHEN 1
      *    尿糞便
                 MOVE  "01"       TO  A010T-BUNRUISUB
               WHEN 2
      *    血液
                 MOVE  "02"       TO  A010T-BUNRUISUB
               WHEN 31
      *    生化１
                 MOVE  "03"       TO  A010T-BUNRUISUB
               WHEN 32
      *    生化１
                 MOVE  "04"       TO  A010T-BUNRUISUB
               WHEN 41
      *    生化２
                 MOVE  "05"       TO  A010T-BUNRUISUB
               WHEN 42
      *    生化２
                 MOVE  "06"       TO  A010T-BUNRUISUB
               WHEN 5
      *    免疫
                 MOVE  "07"       TO  A010T-BUNRUISUB
               WHEN 6
      *    微生物
                 MOVE  "08"       TO  A010T-BUNRUISUB
               WHEN 7
      *    病理
                 MOVE  "09"       TO  A010T-BUNRUISUB
               WHEN 8
      *    病理
                 MOVE  "10"       TO  A010T-BUNRUISUB
               WHEN 11
      *    生体
                 MOVE  "11"       TO  A010T-BUNRUISUB
               WHEN 12
      *    生体
                 MOVE  "12"       TO  A010T-BUNRUISUB
               WHEN 13
      *    生体
                 MOVE  "13"       TO  A010T-BUNRUISUB
               WHEN 14
      *    生体
                 MOVE  "14"       TO  A010T-BUNRUISUB
               WHEN 15
      *    生体
                 MOVE  "15"       TO  A010T-BUNRUISUB
               WHEN 16
      *    生体
                 MOVE  "16"       TO  A010T-BUNRUISUB
               WHEN 17
      *    生体
                 MOVE  "17"       TO  A010T-BUNRUISUB
               WHEN 18
      *    生体
                 MOVE  "18"       TO  A010T-BUNRUISUB
               WHEN 19
      *    生体
                 MOVE  "19"       TO  A010T-BUNRUISUB
               WHEN 20
      *    検体採取
                 MOVE  "20"       TO  A010T-BUNRUISUB
               WHEN 21
      *    検体採取
                 MOVE  "21"       TO  A010T-BUNRUISUB
               WHEN 22
      *    検体採取
                 MOVE  "22"       TO  A010T-BUNRUISUB
               WHEN 99
      *    判断材料・その他
                 MOVE  "23"       TO  A010T-BUNRUISUB
               WHEN 98
      *    判断材料・その他
                 MOVE  "24"       TO  A010T-BUNRUISUB
               WHEN 0
      *    判断材料・その他
                 MOVE  "25"       TO  A010T-BUNRUISUB
               WHEN OTHER
      *    分類不可能の場合
                 MOVE  "26"       TO  A010T-BUNRUISUB
             END-EVALUATE
           ELSE
      *    分類不可能の場合
             MOVE  "26"    TO     A010T-BUNRUISUB
           END-IF
      *
           .
       2003-KENSABUNRUI-EXT.
           EXIT.
      *****************************************************************
      *    帳票明細印字順番号設定処理
      *****************************************************************
       2001-SET-PRSORTNUM-SEC      SECTION.
      *
           CLOSE           A010T-FILE
           INITIALIZE      FLG-ATEND
           INITIALIZE      A010T
           INITIALIZE      A010T2
           OPEN    INPUT   A010T-FILE
           OPEN    OUTPUT  A010T2-FILE
           CLOSE           A010T2-FILE
           OPEN    I-O     A010T2-FILE
      * 
           IF  FLG-OUTALL       =  1
               MOVE  "10"               TO  A010T-OUTTYPE
           ELSE
               MOVE  WRK-PARA-OUTTYPE   TO  A010T-OUTTYPE
           END-IF
      * 2004/01/29変更 
      ***     IF  WRK-PARA-PRSORT  =  1
           IF   WRK-PARA-PRSORT  =  "8"
               AND WRK-PARA-PRSORT  =  "9"
      * 2004/01/29変更 
             MOVE  "9"                TO  A010T-NYUGAIKBN
           END-IF
      *    抽出患者情報読み込み
      *
           START   A010T-FILE
                   KEY IS      >=  A010T-KEY  
           END-START
           PERFORM UNTIL   FLG-ATEND =   1
             PERFORM 910-A010-READ-SEC
      * 2004/01/29変更 
      ***         IF      FLG-ATEND       NOT =   1
      ***          AND (( WRK-PARA-PRSORT NOT =  "1" )
      ***           OR  ( WRK-PARA-PRSORT     =  "1" 
      ***            AND  A010T-NYUGAIKBN     =  "9" )) 
             IF      FLG-ATEND       NOT =   1
                 AND (( WRK-PARA-PRSORT NOT =  "8" )
                 OR  ( WRK-PARA-PRSORT     =  "8" 
                 AND  A010T-NYUGAIKBN     =  "9" )) 
                 AND (( WRK-PARA-PRSORT NOT =  "9" )
                 OR  ( WRK-PARA-PRSORT     =  "9" 
                 AND  A010T-NYUGAIKBN     =  "9" )) 
      * 2004/01/29変更 
      *            レコード転送
               MOVE  A010T   TO  A010T2
      * 
      *            点数 × 回数
      * 20080121 入院行為の不具合をを修正START  t.tanaka 
               IF A010T-KZMWAKE = ZERO
      * 20080121 入院行為の不具合をを修正END
                 COMPUTE    WRK-TEN =
                              A010T-TEN    *      A010T-KAISU
                 MOVE WRK-TEN            TO     A010T2-TTEN
      * 20080121 入院行為の不具合をを修正START  t.tanaka 
               ELSE
                 COMPUTE    WRK-TEN =
                            A010T-TEN2    *     A010T-KAISU  
                 MOVE WRK-TEN            TO     A010T2-TTEN 
               END-IF     
      * 20080121 入院行為の不具合をを修正END
      *            明細印字順番号
               EVALUATE   WRK-PARA-PRSORT
                 WHEN   SPACE
                   COMPUTE     WRK-PRSORTNUM   =
                                999999999.99   -   A010T-KAISU
                   MOVE WRK-PRSORTNUM  TO   A010T2-PRSORTNUM
      * 2004/01/29変更 
      ***                 WHEN   "0" 
                 WHEN   "1" 
      * 2004/01/29変更 
                   COMPUTE    WRK-PRSORTNUM   =
                              999999999.99  -  WRK-TEN
                   MOVE WRK-PRSORTNUM  TO   A010T2-PRSORTNUM
               END-EVALUATE
               INITIALIZE       FLG-INVALID
               WRITE A010T2
                    INVALID  KEY  
                    MOVE  1  TO FLG-INVALID
               END-WRITE
             END-IF
           END-PERFORM
           CLOSE           A010T-FILE
           CLOSE           A010T2-FILE
           .
       2001-SET-PRSORTNUM-EXT.
           EXIT.
      *****************************************************************
      *    診療行為別使用頻度一覧   出力処理
      *****************************************************************
       2001-PRT-SEC                SECTION.
      *
           INITIALIZE      FLG-ATEND
           INITIALIZE      A010T2
           INITIALIZE      A010T
           INITIALIZE      A010
      *
           OPEN    INPUT   A010T2-FILE
      *
      *    抽出患者情報読み込み
      *
           START   A010T2-FILE
                   KEY IS      >=  A010T2-KEY  
           END-START
      *
           PERFORM 910-A010-READ2-SEC
           MOVE    A010T2-OUTTYPE    TO  WRK-OUTTYPE
           MOVE    A010T2-NYUGAIKBN  TO  WRK-NYUGAIKBN
           MOVE    1                 TO  IDX2  
           PERFORM UNTIL    FLG-ATEND =   1
             IF  FLG-ATEND NOT = 1
                 PERFORM 2007-SYORI-SEC
             END-IF
             PERFORM 910-A010-READ2-SEC
           END-PERFORM
      *
      *合計総点数(総金額)
      *
           MOVE  SUM-T-TTEN      TO    WRK-Z9TENGK
           MOVE  WRK-Z9TENGK     TO    A010-GKTTEN
           MOVE  SUM-T-TKIN      TO    WRK-Z9TENGK
           MOVE  WRK-Z9TENGK     TO    A010-GKTKIN
      *   点数単位
           MOVE  "点"            TO    A010-GKTENTANI
           MOVE  "円"            TO    A010-GKYENTANI
      *

           PERFORM  330-TERM-HEN-SEC
      *
           CLOSE           A010T2-FILE
      *
           .
       2001-PRT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為回数明細転送
      *****************************************************************
       2007-SYORI-SEC                SECTION.
      *
      * 診療行為分類のブレイクチェック
           IF    A010T2-OUTTYPE      NOT =  WRK-OUTTYPE
             PERFORM  330-TERM-HEN-SEC
             MOVE     1             TO  IDX2
             INITIALIZE                 A010
             MOVE  A010T2-OUTTYPE   TO  WRK-OUTTYPE
             MOVE  A010T2-NYUGAIKBN TO  WRK-NYUGAIKBN
           END-IF
      *
      * 入外区分のブレイクチェック
           IF    A010T2-NYUGAIKBN   NOT =   WRK-NYUGAIKBN
             PERFORM  330-TERM-HEN-SEC
             MOVE     1             TO IDX2
             INITIALIZE                A010
             MOVE  A010T2-NYUGAIKBN TO WRK-NYUGAIKBN
           END-IF
      *
      * 明細数オーバーのチェック
           IF  IDX2     >          38
             PERFORM  330-TERM-HEN-SEC
             MOVE     1     TO   IDX2
             INITIALIZE          A010
           END-IF
      *
      * 診療コード
           MOVE A010T2-SRYCD     TO   A010-SRYCD(IDX2)
      * 入力コード
      *
      * 20070921 4.0対応START t.tanaka
      *     MOVE     WRK-PARA-HOSPID  TO  ICD-HOSPID
           MOVE     WRK-PARA-HOSPNUM  TO  ICD-HOSPNUM
      * 20070921 4.0対応END
           MOVE     A010T2-SRYCD     TO  ICD-SRYCD
           PERFORM  930-INPUTCD-SELECT-SEC
           PERFORM  930-INPUTCD-FETCH-SEC
           IF  FLG-INPUTCD    =      ZERO
             MOVE ICD-INPUTCD TO   A010-INPCD(IDX2)
             PERFORM  930-INPUTCD-FETCH-SEC
             IF  FLG-INPUTCD  =    ZERO
                 MOVE "※"    TO   A010-INPMK(IDX2)
             END-IF
           END-IF
      *=== sarge対応 ADD By Takashima(2006/09/16) start
           PERFORM  930-INPUTCD-CLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/09/16) end
      * 診療行為名称
           MOVE  A010T2-NAME         TO  A010-SRYNM(IDX2)
      * 点数
      * 20080121 入院診療行為の不具合を修正START t.tanaka
           IF A010T2-KZMWAKE = ZERO
             MOVE  A010T2-TEN          TO  WRK-Z9TEN2
           ELSE 
             MOVE  A010T2-TEN2          TO  WRK-Z9TEN2
           END-IF
     
           MOVE  WRK-Z9TEN2          TO  A010-TEN(IDX2)
      * 20080121 入院診療行為の不具合を修正END
      * 回数
      ****
      *  2005/02/08 @@@@-SUZUKI 小数点以下の桁を印字するため追加
      *     MOVE  A010T2-KAISU        TO  WRK-Z9KAI
      *     MOVE  WRK-Z9KAI           TO  A010-KAISU(IDX2)
           IF    A010T2-KAISU(10:3)    = "000"
             MOVE  A010T2-KAISU(1:9)   TO  WRK-Z9KAI-1
             MOVE  WRK-Z9KAI-1         TO  WRK-Z9KAI-1Z
             MOVE  WRK-Z9KAI-1Z        TO  A010-KAISU(IDX2)
           ELSE
             IF   A010T2-KAISU(11:2)         = "00"
               MOVE A010T2-KAISU         TO  WRK-Z9KAI-2ZZ
               MOVE WRK-Z9KAI-2ZZ        TO  A010-KAISU(IDX2)
             ELSE
               IF A010T2-KAISU(12:1)         = "0"
                 MOVE A010T2-KAISU         TO  WRK-Z9KAI-2ZZZ
                 MOVE WRK-Z9KAI-2ZZZ       TO  A010-KAISU(IDX2)
               ELSE
                 MOVE A010T2-KAISU         TO  WRK-Z9KAI-2Z
                 MOVE WRK-Z9KAI-2Z         TO  A010-KAISU(IDX2)
               END-IF
             END-IF
           END-IF
      *
      *  2005/02/08 ここまで
      ***
      * 点数 × 回数
           MOVE  A010T2-TTEN         TO  WRK-Z9TEN
           MOVE  WRK-Z9TEN           TO  A010-TTEN(IDX2)
      * 合計総点数(総金額)の加算処理
      * 20040129 変更
      ***     IF   A010T2-TENSIKIBETU   =   1
      ***      OR  A010T2-OUTTYPE       =   "95"
      ***      OR  A010T2-OUTTYPE       =   "96"
      ***          ADD   A010T2-TTEN    TO  SUM-T-TKIN
      * 点数単位
      ***          MOVE  "円"           TO  A010-TENTANI(IDX2)
      ***     ELSE
      ***          ADD   A010T2-TTEN    TO  SUM-T-TTEN
      ***     END-IF
           EVALUATE  A010T2-TANI
             WHEN  ZERO
      *    単位が点数の場合
               ADD   A010T2-TTEN    TO  SUM-T-TTEN
             WHEN  1
      *    単位が金額の場合
               ADD   A010T2-TTEN    TO  SUM-T-TKIN
               MOVE  "円"           TO  A010-TENTANI(IDX2)

             WHEN  9
      *    単位が点数、金額以外の場合
               MOVE  "＠"           TO  A010-TENTANI(IDX2)
           END-EVALUATE  
      * 20040129 変更
      * 連番
           ADD   1                   TO  WRK-PRDTLCNT 
           MOVE  WRK-PRDTLCNT        TO  WRK-Z5 
           MOVE  WRK-Z5              TO  A010-NUM(IDX2)
      *
      * 印字明細数カウントアップ
           ADD   1                   TO  IDX2  
      *
           .
       2007-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
      *    
       330-TERM-HEN-SEC         SECTION.
      * 
      *
           INSPECT  WRK-PARA-DENPPRTYMDWH
                    REPLACING  CHARACTERS  BY  " "
                    AFTER  "月"
           MOVE    WRK-PARA-DENPPRTYMDWH   TO  A010-SRYYM
           MOVE    WRK-SYSYMDWH            TO  A010-SYSYMD
      *
      *    ページ数
           ADD     1                       TO  CNT-PAGE
           MOVE    CNT-PAGE                TO  WRK-Z5
           MOVE    WRK-Z5                  TO  A010-PAGE
      *    メッセージ
           IF      WRK-PRDTLCNT =      ZERO
               AND    INP-STS      =      ZERO
             MOVE "＊＊＊  対象データが存在しません  ＊＊＊"
                 TO       A010-JOBMSG
           END-IF
      *    出力区分
           IF  FLG-OUTALL  =  1
             MOVE  WRK-OUTTYPE        TO  WRK-PRT-OUTTYPE
           ELSE
             MOVE  WRK-PARA-OUTTYPE   TO  WRK-PRT-OUTTYPE
           END-IF 
           EVALUATE WRK-PRT-OUTTYPE
             WHEN  "10"
               MOVE  "初診・再診・指導・在宅"  TO  A010-OUTTYPE
             WHEN  "21"
               MOVE  "内服薬"     TO  A010-OUTTYPE
             WHEN  "22"
               MOVE  "注射料"     TO  A010-OUTTYPE
             WHEN  "23"
               MOVE  "外用薬"     TO  A010-OUTTYPE
             WHEN  "29"
               MOVE  "投薬"       TO  A010-OUTTYPE
             WHEN  "30"
               MOVE  "注射・点滴" TO  A010-OUTTYPE
             WHEN  "40"
               MOVE  "処置"       TO  A010-OUTTYPE
             WHEN  "50"
               MOVE  "手術・麻酔" TO  A010-OUTTYPE
             WHEN  "60"
               MOVE  "検査"       TO  A010-OUTTYPE
      * 20080402 H20年4月改正対応START tanaka
             WHEN  "64"
               MOVE  "病理診断"   TO  A010-OUTTYPE
      * 20080402 H20年4月改正対応END
             WHEN  "70"
               MOVE  "画像診断"   TO  A010-OUTTYPE
             WHEN  "71"
               MOVE  "フィルム"   TO  A010-OUTTYPE
             WHEN  "80"
               MOVE  "理学療法・リハビリ・指導・在宅"
                                      TO  A010-OUTTYPE
             WHEN  "90"
               MOVE  "入院"       TO  A010-OUTTYPE
             WHEN  "91"
               MOVE  "器材"       TO  A010-OUTTYPE
             WHEN  "92"
               MOVE  "その他材料" TO  A010-OUTTYPE
             WHEN  "95"
               MOVE  "自費(消費税無し)"  TO  A010-OUTTYPE
             WHEN  "96"
               MOVE  "自費(消費税有り)"  TO  A010-OUTTYPE
             WHEN  "99"
               MOVE  "用法・画像診断撮影部位・コメント" TO A010-OUTTYPE
             WHEN  "ZZ"
               MOVE  "その他分類" TO  A010-OUTTYPE
           END-EVALUATE
      * 
      *    入外区分
           EVALUATE  WRK-NYUGAIKBN
             WHEN  "1"
               MOVE  "入院"      TO  A010-NYUGAIKBN
             WHEN  "2"
               MOVE  "外来"      TO  A010-NYUGAIKBN
             WHEN  "9"
               MOVE  "未使用入力CD"  TO  A010-NYUGAIKBN
      *
      ***  20040727  @@@@-MOTOHASI  入外合計
             WHEN  "0"
               MOVE  "入外合計"  TO  A010-NYUGAIKBN
      ***  ここまで
      *
           END-EVALUATE
      * 
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                 TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY  TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM    TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID  TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY  TO  SPRT-PRIORITY
      ***  20040914 MOTOHASI MONPEに変更
      ***     MOVE   "A00000A010"          TO  SPRT-PRTID
           MOVE   "A00000A010.red"          TO  SPRT-PRTID
      ***  20040914 MOTOHASI MONPEに変更
           MOVE    "診療行為別使用頻度一覧"  TO  SPRT-TITLE
           MOVE    A010                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***     MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END

           IF      SPRT-RETURN         =   ZERO
             CONTINUE
           ELSE
             CONTINUE
           END-IF
      *
           .
       330-TERM-HEN-EXT.
           EXIT.
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
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.  
      *
      ****2005/10/17 @@@@-BRAND  START
      *     MOVE    WRK-SYSYMD          TO  ORC-DBYMD
      ****2005/10/17 @@@@-BRAND  END
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      *
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"         USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"         USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
      *
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *        MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *        CALL    "ORCMCPSUB"          USING
      *         CALL     "MCPSUB"          USING
             MOVE    "tbl_syskanri"       TO  MCP-TABLE
             MOVE    "key10"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING
      ****2005/10/17 @@@@-BRAND  END
                                          MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                     ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                          MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                          SPA-AREA
      * 20070921 4.0対応END
             IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
             ELSE
               MOVE    1               TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
      ****2005/10/17 @@@@-BRAND  START
      *     MOVE    WRK-SYSYMD           TO  ORC-DBYMD
      ****2005/10/17 @@@@-BRAND  END
      * 20080121 入院診療行為の点数、回数の不具合を修正START t.tanaka
           MOVE    SPA-HOSPNUM          TO  SYS-5000-HOSPNUM
      * 20080121 入院診療行為の点数、回数の不具合を修正END
           MOVE    SYS-5000-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY2      TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *    CALL     "MCPSUB"          USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC           =   ZERO
             PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1               TO   FLG-SYSKANRI
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY2      TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL     "ORCMCPSUB"         USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"         USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC           =   ZERO
             MOVE    ZERO             TO  FLG-SYSKANRI
             MOVE    MCPDATA-REC      TO  SYS-5000-REC
           ELSE
             MOVE    1                TO  FLG-SYSKANRI
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
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
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE  "DBCLOSE"          TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY2  TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    管理マスタ検索
      *****************************************************************
       830-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
      ****2005/10/17 @@@@-BRAND  START
      *     MOVE    WRK-SYSYMD           TO  ORC-DBYMD
      ****2005/10/17 @@@@-BRAND  END
           MOVE    SYS-5113-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
      ***  20040914  @@@@-MOTOHASI  
      ***     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY5"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY5      TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC     =   ZERO
             PERFORM 830-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1               TO   FLG-SYSKANRI
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 830-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       830-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索
      *****************************************************************
       830-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
      ***  20040914  @@@@-MOTOHASI  
      ***     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY5"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY5      TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL     "ORCMCPSUB"         USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"         USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC           =   ZERO
             MOVE    ZERO             TO  FLG-SYSKANRI
             MOVE    MCPDATA-REC      TO  SYS-5113-REC
           ELSE
             MOVE    1                TO  FLG-SYSKANRI
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 830-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       830-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       830-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE  "DBCLOSE"          TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      ***  20040914  @@@@-MOTOHASI  
      ***     MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY5"  TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY5  TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
           .
       830-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-SELECT-SEC               SECTION.
      *
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACT-ST-KEY4     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "st_key4"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
      * 20070921 4.0対応END
                                   MCPAREA
                                   MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                   SPA-AREA
      * 20070921 4.0対応END

           IF      MCP-RC           =   ZERO
             MOVE    ZERO          TO  FLG-SRYACT
           ELSE
             MOVE    1             TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO   MCP-FUNC
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACT-ST-KEY4     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryact"  TO  MCP-TABLE
           MOVE    "st_key4"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
      * 20070921 4.0対応END
                                   MCPAREA
                                   MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                   SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC           =    ZERO
             MOVE    ZERO         TO   FLG-SRYACT
             MOVE    MCPDATA-REC  TO   SRYACT-REC
           ELSE
             MOVE    1            TO   FLG-SRYACT
             INITIALIZE                SRYACT-REC
           END-IF
           .
       900-SRYACT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報ＤＢクローズ処理
      *****************************************************************
       900-SRYACT-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE     "DBCLOSE"            TO  MCP-FUNC
           MOVE     "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACT-ST-KEY4     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryact"  TO  MCP-TABLE
           MOVE    "st_key4"     TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"   USING
      * 20070921 4.0対応END
                                 MCPAREA
                                 MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                 SPA-AREA
      * 20070921 4.0対応END
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為情報検索
      *****************************************************************
       900-NYUINACT-SELECT-SEC               SECTION.
      *
           MOVE    NYUIN-SRYACT-REC TO  MCPDATA-REC
           MOVE    "DBSELECT"       TO  MCP-FUNC
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-NYUINACT-ST-KEY1  TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_nyuinact"  TO  MCP-TABLE
           MOVE    "st_key1"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
      * 20070921 4.0対応END
                                       MCPAREA
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC           =   ZERO
             MOVE    ZERO        TO  FLG-NYUINACT
           ELSE
             MOVE    1           TO  FLG-NYUINACT
           END-IF
           .
       900-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為情報検索
      *****************************************************************
       900-NYUINACT-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO  MCP-FUNC
      *     MOVE    PATH-TBL-NYUINACT-ST-KEY1  TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_nyuinact"   TO  MCP-TABLE
           MOVE    "st_key1"        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
      * 20070921 4.0対応END
                                    MCPAREA
                                    MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                    SPA-AREA
      * 20070921 4.0対応END

           IF      MCP-RC           =   ZERO
             MOVE    ZERO         TO  FLG-NYUINACT
             MOVE    MCPDATA-REC  TO  NYUIN-SRYACT-REC
           ELSE
             MOVE    1            TO  FLG-NYUINACT
             INITIALIZE               NYUIN-SRYACT-REC
           END-IF  
           .
       900-NYUINACT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為情報ＤＢクローズ処理
      *****************************************************************
       900-NYUINACT-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE  "DBCLOSE"            TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      *     MOVE    PATH-TBL-NYUINACT-ST-KEY1  TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_nyuinact"     TO  MCP-TABLE
           MOVE    "st_key1"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      * 20070921 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                          SPA-AREA
      * 20070921 4.0対応END
           .
       900-NYUINACT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスタ検索
      *****************************************************************
       900-INPUTCD-SELECT-SEC               SECTION.
      *
           MOVE    SPA-HOSPNUM      TO  ICD-HOSPNUM
           MOVE    INPUTCD-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"       TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "INPUTCD-ALL"    TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-INPUTCD-ALL    TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL     "ORCMCPSUB"         USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"         USING
           MOVE    "tbl_inputcd"    TO  MCP-TABLE
           MOVE    "all"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
           IF   MCP-RC             =    ZERO
             MOVE    ZERO        TO  FLG-INPUTCD
           ELSE
             MOVE    1           TO  FLG-INPUTCD
           END-IF        
           .
       900-INPUTCD-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスタ検索
      *****************************************************************
       900-INPUTCD-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "INPUTCD-ALL"    TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-INPUTCD-ALL    TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL     "ORCMCPSUB"         USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"         USING
           MOVE    "tbl_inputcd"        TO  MCP-TABLE
           MOVE    "all"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC       =   ZERO
             MOVE    ZERO         TO  FLG-INPUTCD
             MOVE    MCPDATA-REC  TO  INPUTCD-REC
           ELSE
             MOVE    1            TO  FLG-INPUTCD
             INITIALIZE               INPUTCD-REC
           END-IF
           .
       900-INPUTCD-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスタＤＢクローズ処理
      *****************************************************************
       900-INPUTCD-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE  "DBCLOSE"            TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE  "INPUTCD-ALL"        TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-INPUTCD-ALL    TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL  "ORCMCPSUB"              USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"         USING
           MOVE    "tbl_inputcd"        TO  MCP-TABLE
           MOVE    "all"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                   ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
           .
       900-INPUTCD-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療行為回数一時ファイルREAD
      *****************************************************************
       910-A010-READ-SEC                SECTION.
      *
           READ A010T-FILE   NEXT
             AT  END
               MOVE  1  TO  FLG-ATEND
           END-READ
           IF  FLG-OUTALL     =     1
             IF  WRK-PARA-OUTTYPE =      SPACE
      *    診療分類指定が未入力の場合は分類"99"を印字対象外とする。
      *    (診療分類指定が"0"の場合は分類"99"を印字対象とする。)
      *
               IF    A010T-OUTTYPE     =   "99"
                 MOVE 1     TO     FLG-ATEND
               END-IF
             END-IF
           ELSE
      *    診療分類コード指定の場合は指定された分類コードのみ印字する。
             IF  A010T-OUTTYPE    NOT =  WRK-PARA-OUTTYPE
               MOVE  1    TO    FLG-ATEND
             END-IF
           END-IF
      *
      *    件数集計
      *
           IF   FLG-ATEND     NOT = 1
             ADD      1       TO  CNT-A010T
           END-IF

           .
       910-A010-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為回数一時ファイルREAD
      *****************************************************************
       910-A010-READ2-SEC                SECTION.
      *
           READ A010T2-FILE   NEXT
             AT  END
               MOVE  1  TO  FLG-ATEND
           END-READ
      *
      *    件数集計
           IF   FLG-ATEND     NOT = 1
             ADD      1     TO  CNT-A010T2
           END-IF 
           .
       910-A010-READ2-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報検索
      *****************************************************************
       910-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACCT-KEY       TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
           IF  MCP-RC              =   ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *        MOVE    "SRYACCT-KEY"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACCT-KEY       TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *        CALL    "ORCMCPSUB"      USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
             MOVE    "tbl_sryacct"      TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                          MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                   ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                          MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                          SPA-AREA
      * 20070921 4.0対応END
             IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
             ELSE
               INITIALIZE                  SRYACCT-REC
      ****2005/10/17 @@@@-BRAND  START
      *          MOVE    "1"             TO  FLG-SRYACCT
                MOVE     1             TO  FLG-SRYACCT
      ****2005/10/17 @@@@-BRAND  END
             END-IF
           ELSE
      ****2005/10/17 @@@@-BRAND  START
      *       MOVE   "1"          TO  FLG-SRYACCT
             MOVE    1           TO  FLG-SRYACCT
      ****2005/10/17 @@@@-BRAND  END
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SRYACCT-KEY"       TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACCT-KEY       TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"             USING
           MOVE    "tbl_sryacct"      TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                      MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                      MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                      SPA-AREA
      * 20070921 4.0対応END
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院診療会計情報検索
      *****************************************************************
       910-NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "NYUINACCT-KEY"     TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-NYUINACCT-KEY     TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
           IF  MCP-RC              =   ZERO
             MOVE    "DBFETCH"        TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *        MOVE    "NYUINACCT-KEY"  TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *         MOVE    PATH-TBL-NYUINACCT-KEY  TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *        CALL    "ORCMCPSUB"      USING
      * 20070921 4.0対応START t.tanaka
      *         CALL     "MCPSUB"      USING
             MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
             IF      MCP-RC           =   ZERO
               MOVE    MCPDATA-REC  TO  NYUINACCT-REC
               MOVE    ZERO         TO  FLG-NYUINACCT
             ELSE
               INITIALIZE               NYUINACCT-REC
      ****2005/10/17 @@@@-BRAND  START
      *            MOVE    "1"          TO  FLG-NYUINACCT
               MOVE     1          TO  FLG-NYUINACCT
      ****2005/10/17 @@@@-BRAND  END
             END-IF
           ELSE
      ****2005/10/17 @@@@-BRAND  START
      *       MOVE   "1"           TO  FLG-NYUINACCT
             MOVE    1            TO  FLG-NYUINACCT
      ****2005/10/17 @@@@-BRAND  END
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"            TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "NYUINACCT-KEY"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-NYUINACCT-KEY      TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
      ****2006/01/06 @@@@-BRAND  START
      *     MOVE    ZERO                TO  TNS-YUKOSTYMD
      *     MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      * 20080121 入院行為の不具合をを修正START t.tanaka
           MOVE    SPA-HOSPNUM     TO    TNS-HOSPNUM
      * 20080121 入院行為の不具合をを修正END
           MOVE    WRK-PARA-PRTLYMD    TO  TNS-YUKOSTYMD
           MOVE    WRK-PARA-PRTSYMD    TO  TNS-YUKOEDYMD
      ***2006/01/06 @@@@-BRAND  END
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-TENSU-KEY         TO  MCP-PATH
      * 20070921 4.0対応END
      *     MOVE    LNK-PRTKANRI-SKYYMD TO  ORC-DBYMD
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *        MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *         MOVE    PATH-TBL-TENSU-KEY         TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      * 20070921 4.0対応START t.tanaka
      *        CALL    "ORCMCPSUB"          USING
      *         CALL     "MCPSUB"          USING
             MOVE    "tbl_tensu"        TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                     ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070921 4.0対応END
             IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC      TO  TNS-TENSU-REC
               MOVE    ZERO             TO  FLG-TENSU
             ELSE
               INITIALIZE               TNS-TENSU-REC
               MOVE    1                TO  FLG-TENSU
             END-IF
           ELSE
             INITIALIZE                 TNS-TENSU-REC
             MOVE    1                  TO  FLG-TENSU
           END-IF
      
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-TENSU-KEY     TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      * 20070921 4.0対応START t.tanaka
      *    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター SELECT
      *****************************************************************
       930-INPUTCD-SELECT-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "INPUTCD-KEY4"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-INPUTCD-KEY4      TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC              =   ZERO
             MOVE    ZERO            TO  FLG-INPUTCD
           ELSE
             INITIALIZE                  INPUTCD-REC
             MOVE    1               TO  FLG-INPUTCD
           END-IF
      *
           .
       930-INPUTCD-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター FETCH
      *****************************************************************
       930-INPUTCD-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "INPUTCD-KEY4"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-INPUTCD-KEY4      TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"         USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                           MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                    ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                           MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                           SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC              =   ZERO
             MOVE    MCPDATA-REC TO  INPUTCD-REC
             MOVE    ZERO        TO  FLG-INPUTCD
           ELSE
             INITIALIZE              INPUTCD-REC
             MOVE    1           TO  FLG-INPUTCD
           END-IF
      *
           .
       930-INPUTCD-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター CLOSE
      *****************************************************************
       930-INPUTCD-CLOSE-SEC           SECTION.
      *
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "INPUTCD-KEY4"      TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-INPUTCD-KEY4      TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
      *
           .
       930-INPUTCD-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    検査ソート情報読込
      *****************************************************************
       940-KENSASORT-READ-SEC         SECTION.
      *
           MOVE    KENSASORT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-KENSASORT-ST-KEY1     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_kensasort"     TO  MCP-TABLE
           MOVE    "st_key1"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
                                       MCPAREA
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-KENSASORT-ST-KEY1     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
             MOVE    "tbl_kensasort"     TO  MCP-TABLE
             MOVE    "st_key1"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
                                         MCPAREA
                                         MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                         SPA-AREA
      * 20070921 4.0対応END

             IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  KENSASORT-REC
               MOVE    ZERO            TO  FLG-KENSASORT
             ELSE
               MOVE    1               TO  FLG-KENSASORT
             END-IF
           ELSE
             MOVE    1                 TO  FLG-KENSASORT
           END-IF
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-KENSASORT-ST-KEY1     TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_kensasort"     TO  MCP-TABLE
           MOVE    "st_key1"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      * 20070921 4.0対応END
                                       MCPAREA
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
           .
       940-KENSASORT-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報読み込み
      *****************************************************************
       940-PTNUM-READ-SEC         SECTION.
      *
           MOVE    PTNUM-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"        TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTNUM-KEY       TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_ptnum"       TO  MCP-TABLE
           MOVE    "key"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                     MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                     MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                     SPA-AREA
      * 20070921 4.0対応END
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"           TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *        MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *        MOVE    PATH-TBL-PTNUM-KEY       TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *        CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *         CALL     "MCPSUB"          USING
             MOVE    "tbl_ptnum"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                   ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
                                       MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070921 4.0対応END
             IF   MCP-RC       =   ZERO
               MOVE    MCPDATA-REC   TO  PTNUM-REC
               MOVE    ZERO          TO  FLG-PTNUM
             ELSE
               MOVE    1             TO  FLG-PTNUM
             END-IF
           ELSE
             MOVE    1               TO  FLG-PTNUM
           END-IF
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      * 20070921 4.0対応START t.tanaka
      *    MOVE    PATH-TBL-PTNUM-KEY       TO  MCP-PATH
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL     "MCPSUB"          USING
           MOVE    "tbl_ptnum"       TO  MCP-TABLE
           MOVE    "key"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                     MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                     MCPDATA-REC
      * 20070921 4.0対応START t.tanaka
                                     SPA-AREA
      * 20070921 4.0対応END
           .
       940-PTNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      * 20070921 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      * 20070921 4.0対応END

           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      * 20070921 4.0対応START t.tanaka
                                   SPA-AREA
      * 20070921 4.0対応END
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
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           CALL   "ORCDBMAIN"     USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
      * 20070921 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
      * 20070921 4.0対応END
           MOVE    "DBSTART"           TO  MCP-FUNC.
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"         USING
      * 20070921 4.0対応START t.tanaka
      *
      *     CALL    "MCPSUB"           USING
           MOVE    LOW-VALUE           TO     MCP-TABLE
                                              MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                ORCMCPAREA

      ****2005/10/17 @@@@-BRAND  END
      * 20070921 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                       MCPDATA-REC
                                       SPA-AREA
                                       .
      * 20070921 4.0対応END
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
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE           TO     MCP-TABLE
                                              MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
      * 20070921 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      * 20070921 4.0対応END
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      * 20070921 4.0対応START t.tanaka
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070921 4.0対応END
      ****2005/10/17 @@@@-BRAND  END
                                       MCPAREA
      ****2005/10/17 @@@@-BRAND  START 
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
      * 20070921 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                       MCPDATA-REC
                                       SPA-AREA
                                        .
      * 20070921 4.0対応END
           .
       900-DBCLOSE-EXT.
           EXIT.
