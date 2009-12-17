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
       PROGRAM-ID.             A00000K140.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : (診療科別)診療行為区分別集計表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/03/26    @@@@-半田     新規作成
      *  04/05/17    @@@@-有村     検査・在宅の点数集計方法修正
      *                            テスト患者は対象外に変更
      *  04/07/07    @@@@-本橋     入院基本料・特定基本料項目欄を削除
      *  04/09/15    @@@@-本橋     MONPE化
      *  06/08/08    @@@@-高嶋     sarge対応
      *  07/05/15    @@@@-内田     療養担当手当の集計位置修正
      *  07/06/28    @@@@-佐藤     自費10項目修正
      *  07/09/18    @@@@-長屋     4.0対応
      *  07/11/13    @@@@-田中     老人の値が前期高齢者に集計される不具合を修正
      *  07/12/13    @@@@-田中     往診・在宅の判定条件を修正
      *  08/01/28    @@@@-田中     包括分を対象外にする
      *  08/03/03    @@@@-田中     精神科専門療法をその他に集計するよう修正
      *  08/04/17    @@@@-田中     H20年4月改正対応(後期高齢者、義務教育就学前、病理診断)
      *  08/06/18    @@@@-田中     生活療養費を室料差額・食事欄への追加修正
      *  08/08/01    @@@@-田中     検査と在宅の点数と回数の計算式を修正
      *                            自費の都度入力が印字しない不具合を修正
      *  08/08/25    @@@@-田中     診療行為画面から入院料を入力すると重複してしまう不具合を修正
      *  08/09/30    @@@@-田中     入院のみの患者が集計されない不具合を修正
      *  09/02/10    @@@@-田中     入院料・室料差額・食事の件数を修正
      *  09/12/14    @@@@-田中     病理診断をその他欄から病理診断欄に変更
      *                            放射線治療をその他欄に集計
      *                            その他（リハビリ）の診療をリハビリに集計
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    診療行為受診対象患者一時ファイル
           SELECT  K140T-FILE      ASSIGN              K140TPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  K140TMP-KEY
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
       FD  K140T-FILE.
           COPY    "A00000K140TMP.INC".
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    エラーファイル 名称領域 
      ***07/09/18 nagaya 4.0対応 start
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      ***07/09/18 nagaya 4.0対応 end
      *
      *    診療行為受診対象患者一時ファイル(外部ファイル名称)
       01  K140TPARA.
           03  FILLER              PIC  X(05) VALUE "/tmp/".
           03  K140TPARA-FILENAME  PIC  X(11) VALUE "A00000K140T".
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
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
           03  FLG-SRYACCT2        PIC 9(01).
           03  FLG-NSRYACCT        PIC 9(01).
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-NYUIN           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-K140NPT         PIC 9(01).
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
           03  CNT-K140TMP         PIC 9(06).
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
      *  
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
           03  WRK-TENSU             PIC 9(09).
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
      *=== 自費10項目修正 Satoh (2007/06/28) START
      *     03  WRK-TJIHI-G       OCCURS  5.
           03  WRK-TJIHI-G       OCCURS  10.
      *=== 自費10項目修正 Satoh (2007/06/28) END
               05  WRK-TJIHI         PIC X(20).
               05  WRK-TJIHI-FLG     PIC 9(01).
      *=== 自費10項目修正 Sato (2007/06/28) START
      *     03  WRK-TJIHIN-G       OCCURS  5.
           03  WRK-TJIHIN-G       OCCURS  10.
      *=== 自費10項目修正 Satoh (2007/06/28) END
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
      *
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
      **     03  WRK-TENSU-HEN         PIC 99999999999.
      **     03  WRK-KAISU-HEN         PIC 99999999999.
      * 20080801 検査と在宅の点数と回数の計算式を修正START
           03  WRK-ZAINUM            PIC 9(08).
      * 20080801 検査と在宅の点数と回数の計算式を修正END
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
           03  WRK-ZENPTID           PIC 9(10).
           03  WRK-ZENPTID2          PIC 9(10).
           03  WRK-TENSU3            PIC 9(09).
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
      *
      *    診療科名退避エリア
       01  WRK-SRYKA-AREA.
           03  WRK-SRYKAMAX                         PIC 9(03).
           03  WRK-SRYKA-TBL         OCCURS 100.
               05  WRK-SRYKA                        PIC X(02).
               05  WRK-SRYKANAME                    PIC X(20).
      *
      *    入院履歴情報退避エリア
       01  WRK-NYUIN-AREA.
           03  WRK-NYUIN-TBL.
               05  WRK-NYUIN-DAY    PIC  9(01) OCCURS 31.
      *
      *    室料差額退避エリア
       01  WRK-ROOM-SAGAKU-AREA.
          03  WRK-ROOM-MAX           PIC 9(02).
          03  WRK-ROOM-DATA     OCCURS 50.
             05  WRK-ROOM-CD         PIC 9(03).
             05  WRK-ROOM-SGK        PIC 9(05).
      *
      *   入外別全診療科加算エリア
       01 WRK-K140ALL-AREA.
          03  WRK-K140ALL-SRY-TBL OCCURS 56.
              05  WRK-K140ALL-TENSU  PIC  9(11).
              05  WRK-K140ALL-KAISU  PIC  9(11).
      *
           COPY    "CPSHELLTBL.INC".
      *
      ***07/09/18 nagaya 4.0対応 start
      *     COPY    "ORCA-DBPATH".
      ***07/09/18 nagaya 4.0対応 end
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
           COPY  "CPSK1031.INC".
      *
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
       01  SRYACCT-REC2.
           COPY    "CPSRYACCT.INC"   REPLACING  //ACCT-//
                                     BY         //ACCT2-//.
      *    入院会計マスタ
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
      *
      *--- 2009/02/10 ADD START ----------------------------------------
      *    入院会計マスタ２
       01  NYUINACCT2-REC.
           COPY    "CPNYUINACCT.INC"   REPLACING  //NACCT-//
                                       BY         //NACCT2-//.
      *--- 2009/02/10 ADD END ------------------------------------------
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
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
           COPY  "CPSK5113.INC".
      *
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    点数マスタ
      * 01  TNS-TENSU-REC.
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
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブGET-OFF
           COPY    "CPORCSPRT.INC".          
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      
      * 20080417 H20年4月改正対応START tanaka
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      * 20080417 H20年4月改正対応END
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
      ***      正常主処理
               PERFORM 200-MAIN-SEC
           ELSE
      ***      エラー処理
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
           INITIALIZE                  WRK-K140ALL-AREA
           INITIALIZE                  WRK-SRYKA-AREA
           INITIALIZE                  WRK-ROOM-SAGAKU-AREA
           INITIALIZE                  WRK-NYUIN-AREA
      ***07/09/18 nagaya 4.0対応 start
           INITIALIZE                  SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
      *
      ***07/09/18 nagaya 4.0対応 start
           MOVE    K100LNK-HOSPNUM    TO  SPA-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE    K100LNK-JOBID   TO  JOB-JOBID
           MOVE    K100LNK-SHELLID TO  JOB-SHELLID
           MOVE    "A00000K140"    TO  JOB-PGID
           MOVE    "(診療科別)診療区分別集計表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      ***07/09/18 nagaya 4.0対応 start
                                       SPA-AREA
      *
      *     MOVE    "RECEERR"           TO  RECEERR-FILE-ID
      *     MOVE    K100LNK-PRTKANRI-TERMID
      *                                 TO  RECEERR-TERMID
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM    TO      SYS-1001-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD   TO  SYS-1001-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD   TO  SYS-1001-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1001-REC   TO      MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF     FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO    SYS-1001-REC
               IF      SYS-1001-HOSPNAME  =  LOW-VALUE
                   MOVE    SPACE   TO    SYS-1001-HOSPNAME
               END-IF
               IF      SYS-1001-KAISETUNAME  =   LOW-VALUE
                   MOVE    SPACE   TO    SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                     TO    WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE     SPACE         TO        SYS-1031-REC
           INITIALIZE                       SYS-1031-REC
           MOVE     "1031"        TO        SYS-1031-KANRICD
           MOVE     "*"           TO        SYS-1031-KBNCD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM        TO    SYS-1031-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD   TO    SYS-1031-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD   TO    SYS-1031-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE     SYS-1031-REC  TO        MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC TO  SYS-1031-REC
           ELSE
                    INITIALIZE              SYS-1031-REC
                    MOVE    01    TO        INP-STS 
           END-IF
      *
      *    入外区分のチェック
           IF   K100LNK-NYUGAIKBN   NOT =   SPACE
               AND K100LNK-NYUGAIKBN   NOT =   "1"
               AND K100LNK-NYUGAIKBN   NOT =   "2"
               MOVE    10      TO  INP-STS 
               GO              TO  100-INIT-EXT
           END-IF
      *
      *    保険外区分のチェック
           IF   K100LNK-HKNTG       NOT =   SPACE
                AND K100LNK-HKNTG       NOT =   "0"
               MOVE    20      TO  INP-STS 
               GO              TO  100-INIT-EXT
           END-IF
      *
      *    診療科別印刷K140NPT
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM       TO   SYS-1005-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD   TO  SYS-1005-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD   TO  SYS-1005-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    "1005"            TO   SYS-1005-KANRICD
           MOVE    WRK-PARA-PRTLYMD  TO   WRK-SYMD
           PERFORM 820-SYSKANRI-DBSELECT-SEC
           MOVE   1        TO     IDX1 
           PERFORM  UNTIL  FLG-SYSKANRI3 = 1
               IF   FLG-SYSKANRI3 = ZERO
                   MOVE SYS-1005-KBNCD      TO WRK-SRYKA(IDX1)
                   MOVE SYS-1005-SRYKANAME1 TO WRK-SRYKANAME(IDX1)
                   ADD  1                   TO IDX1 
               END-IF  
               PERFORM 820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
           SUBTRACT  1        FROM IDX1  
           MOVE  IDX1         TO   WRK-SRYKAMAX
      *=== sarge対応 ADD By Takashima(2006/09/17) start
           PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/09/17) end
      *
      *    入院自費情報取得
      *    区分11
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "11"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(5)
               MOVE  1                 TO  WRK-TJIHIN-FLG(5)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHIN(5)
           END-IF                                     

      *=== 自費10項目修正 Satoh (2007/06/28) START
      *    区分16
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "16"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(6)
               MOVE  1                 TO  WRK-TJIHIN-FLG(6)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHIN(6)
           END-IF                                     

      *    区分17
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "17"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(7)
               MOVE  1                 TO  WRK-TJIHIN-FLG(7)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHIN(7)
           END-IF                                     

      *    区分18
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "18"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(8)
               MOVE  1                 TO  WRK-TJIHIN-FLG(8)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHIN(8)
           END-IF                                     

      *    区分19
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "19"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(9)
               MOVE  1                 TO  WRK-TJIHIN-FLG(9)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHIN(9)
           END-IF                                     

      *    区分10
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "10"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHIN(10)
               MOVE  1                 TO  WRK-TJIHIN-FLG(10)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHIN(10)
           END-IF                                     
      *=== 自費10項目修正 Satoh (2007/06/28) END
      *
      *    外来自費情報取得
      *    区分21
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "21"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(5)
               MOVE  1                 TO  WRK-TJIHI-FLG(5)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(5)
           END-IF                                     

      *=== 自費10項目修正 Satoh (2007/06/28) START
      *    区分26
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "26"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(6)
               MOVE  1                 TO  WRK-TJIHI-FLG(6)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(6)
           END-IF                                     

      *    区分27
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "27"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(7)
               MOVE  1                 TO  WRK-TJIHI-FLG(7)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(7)
           END-IF                                     

      *    区分28
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "28"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(8)
               MOVE  1                 TO  WRK-TJIHI-FLG(8)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(8)
           END-IF                                     

      *    区分29
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "29"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(9)
               MOVE  1                 TO  WRK-TJIHI-FLG(9)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(9)
           END-IF                                     

      *    区分20
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "20"                TO  SYS-1013-KBNCD
           MOVE    WRK-PARA-PRTLYMD    TO  WRK-SYMD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    WRK-PARA-PRTSYMD    TO  SYS-1013-STYUKYMD
           MOVE    WRK-PARA-PRTLYMD    TO  SYS-1013-EDYUKYMD
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(10)
               MOVE  1                 TO  WRK-TJIHI-FLG(10)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(10)
           END-IF                                     
      *=== 自費10項目修正 Satoh (2007/06/28) END
      *
      *    全室料差額取得
           INITIALIZE                      SYS-5113-REC
           MOVE    "5113"              TO  SYS-5113-KANRICD
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  SYS-5113-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
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
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *
           OPEN     OUTPUT  K140T-FILE
           CLOSE            K140T-FILE
           OPEN     I-O     K140T-FILE
      *
           MOVE     ZERO    TO        GET-OFF
           EVALUATE K100LNK-NYUGAIKBN
               WHEN SPACE
               WHEN "1"
                   MOVE    "1"       TO     WRK-NYUGAIKBN
               WHEN "2"
                   MOVE    "2"       TO     WRK-NYUGAIKBN
           END-EVALUATE
      *
      *    データ集計処理
           PERFORM  UNTIL   GET-OFF   =      1
      ***           集計処理
               PERFORM 2001-SYUKEI-SEC
               EVALUATE K100LNK-NYUGAIKBN
                   WHEN SPACE
                       IF  WRK-NYUGAIKBN  =   "1"
                           MOVE "2"    TO     WRK-NYUGAIKBN
                       ELSE    
                          MOVE 1      TO     GET-OFF
                       END-IF    
                   WHEN "1"
                   WHEN "2"
                       MOVE     1      TO     GET-OFF
                END-EVALUATE
           END-PERFORM
      *
      *
           CLOSE   K140T-FILE
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
           MOVE   "(診療科別)"      TO    K100-SUBTITLE
      *
           EVALUATE INP-STS 
               WHEN 01
                   MOVE   "プリンタ情報が取得できませんでした"
                                    TO    K100-JOBMSG
               WHEN 10
                   MOVE  "入力された入外区分の値に誤りがあります"
                                    TO    K100-JOBMSG
               WHEN 20
                   MOVE  "入力された保険外区分の値に誤りがあります"
                                    TO    K100-JOBMSG
           END-EVALUATE
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    K100-PAGE
      *
           MOVE    WRK-SRYYM        TO    K100-SRYYM
           MOVE    WRK-SEIYMD       TO    K100-SYSYMD

           PERFORM 330-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為データ集計処理
      *****************************************************************
       2001-SYUKEI-SEC                SECTION.
      *
      * 20080930 入院のみの患者が集計されない不具合を修正START
      *     IF  WRK-NYUGAIKBN  =   "1"
      *         PERFORM  20011-NYUIN-SEC
      *     END-IF
      *      入院診療行為情報集計
           PERFORM  900-NYUINACCT-SELECT-SEC
           PERFORM  UNTIL    FLG-NSRYACCT   =   1
             IF  WRK-NYUGAIKBN  =   "1"
      *--- 2009/02/10 CHANGE START -------------------------------------
      *        AND  NACCT-PTID NOT =  WRK-ZENPTID2
               IF   NACCT-PTID  NOT =  WRK-ZENPTID2
      *--- 2009/02/10 CHANGE END ---------------------------------------
                 PERFORM  20011-NYUIN-SEC
                 MOVE   NACCT-PTID   TO    WRK-ZENPTID2
      *--- 2009/02/10 ADD START ----------------------------------------
               END-IF
      *    入院件数カウント処理
               PERFORM   20000-NYUINKENSU-SEC
      *--- 2009/02/10 ADD END ------------------------------------------
             END-IF
             PERFORM  900-NYUINACCT-FETCH-SEC
           END-PERFORM
           PERFORM  900-NYUINACCT-CLOSE-SEC

      * 20080930 入院のみの患者が集計されない不具合を修正END
      *
           PERFORM  900-SRYACCT-SELECT-SEC
      *
           PERFORM  UNTIL    FLG-SRYACCT   =   1
      *      診療行為情報集計
               PERFORM  20012-SRY-SET
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
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
      *    収納情報より、入院料・室料差額・食事取得
      *     PERFORM   VARYING    WRK-DENPJTIKBN  FROM  1  BY  1
      *               UNTIL      WRK-DENPJTIKBN  >     3       
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
      *     収納情報検索
               PERFORM  940-SYUNOU-SELECT-SEC
      *
               PERFORM UNTIL   FLG-SYUNOU  =  1
                   IF  SYU-NYUGAIKBN          =  "1"
      ***** 04/05/17 UPD
      **           PERFORM  20013-NYUIN-SET-SEC
      *     テスト患者は対象外
                       MOVE    SYU-PTID    TO  WRK-PTID  
                       PERFORM  900-PTINF-READ-SEC
                       IF      FLG-PTINF             =   ZERO
                           AND     PTINF-TSTPTNUMKBN   =   "0"
                           PERFORM  20013-NYUIN-SET-SEC
                       END-IF
      *****
                   END-IF
                   PERFORM  940-SYUNOU-FETCH-SEC
               END-PERFORM
               PERFORM  940-SYUNOU-CLOSE-SEC
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
      *     END-PERFORM
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
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
           INITIALIZE                            K140TMP-REC
      *
           MOVE    SYU-NYUGAIKBN           TO    K140TMP-NYUGAIKBN
           MOVE    SYU-SRYKA               TO    K140TMP-SRYKA
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
      *--- 2009/02/10 DELETE START -------------------------------------
      ** 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
      *     IF SYU-PTID  NOT =   WRK-ZENPTID
      *       PERFORM  910-SRYACCT-SELECT-SEC
      ** 
      *       PERFORM  UNTIL    FLG-SRYACCT2   =   1
      *         IF ACCT2-SRYKBN  =  "90" OR  "92"
      *           COMPUTE  WRK-TENSU3  =  ACCT2-ZAITEN  *  ACCT2-ZAIKAISU
      *           COMPUTE  SYU-NYN-HKNTEN  =  SYU-NYN-HKNTEN - WRK-TENSU3
      *           MOVE     ACCT2-PTID  TO   WRK-ZENPTID
      *         END-IF
      *         PERFORM  910-SRYACCT-FETCH-SEC
      *       END-PERFORM
      *     PERFORM  910-SRYACCT-CLOSE-SEC
      *     END-IF
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
      *    日数の算出
      *     MOVE    SYU-SKYSTYMD            TO    WRK-SKYSTYMD
      *     MOVE    SYU-SKYEDYMD            TO    WRK-SKYEDYMD
      *     COMPUTE WRK-N-NISSU             =
      *             WRK-SKYEDD              -     WRK-SKYSTD  +  1    
      *--- 2009/02/10 DELETE END ---------------------------------------
      *
      *
      ***  20040707  @@@@-MOTOHASI  変更
      ****    入院料加算
      ***     MOVE     SYU-SKYEDYMD(7:2)      TO    IDA-G
      ***     IF       WRK-NYUIN-DAY(IDA)     =     1
      ***       ADD    SYU-NYN-HKNTEN         TO    K140TMP-TENSU(42)
      ***       ADD    WRK-N-NISSU            TO    K140TMP-KAISU(42)
      ***     ELSE
      ***       ADD    SYU-NYN-HKNTEN         TO    K140TMP-TENSU(41)
      ***       ADD    WRK-N-NISSU            TO    K140TMP-KAISU(41)
      ***     END-IF
      *    入院料加算
           ADD    SYU-NYN-HKNTEN         TO    K140TMP-TENSU(42)
      *--- 2009/02/10 DELETE START -------------------------------------
      *     ADD    WRK-N-NISSU            TO    K140TMP-KAISU(42)
      *--- 2009/02/10 DELETE END ---------------------------------------
      ***  20040707  ここまで
      *
      ***  20040707  @@@@-MOTOHASI  変更
      ****    室料差額加算
      ***     IF       SYU-RMSAGAKU           NOT = ZERO
      ***       ADD    SYU-RMSAGAKU           TO    K140TMP-TENSU(43)
      ***       ADD    WRK-N-NISSU            TO    K140TMP-KAISU(43)
      ***     END-IF
      ****
      ****    食事加算
      ***     ADD      SYU-RYOYOHI-SKJ        TO    K140TMP-TENSU(43)
      ***     ADD      WRK-N-NISSU            TO    K140TMP-KAISU(43)
      *    室料差額加算
           IF       SYU-RMSAGAKU           NOT = ZERO
               ADD    SYU-RMSAGAKU           TO    K140TMP-TENSU(41)
      *--- 2009/02/10 DELETE START -------------------------------------
      *         ADD    WRK-N-NISSU            TO    K140TMP-KAISU(41)
      *--- 2009/02/10 DELETE END ---------------------------------------
           END-IF
      *
      *    食事加算
           ADD      SYU-RYOYOHI-SKJ        TO    K140TMP-TENSU(41)
      * 20080618 生活療養費を室料差額・食事欄への追加修正START tanaka
           ADD      SYU-RYOYOHI-LIFE       TO    K140TMP-TENSU(41)
      * 20080618 生活療養費を室料差額・食事欄への追加修正END
      *--- 2009/02/10 DELETE START -------------------------------------
      *     ADD      WRK-N-NISSU            TO    K140TMP-KAISU(41)
      *--- 2009/02/10 DELETE END ---------------------------------------
      ***  20040707  ここまで
      *
           IF  FLG-M01TMP   =  1
               WRITE    K140TMP-REC
           ELSE
               REWRITE  K140TMP-REC
           END-IF
      *
           .
       20013-NYUIN-SET-EXT.
           EXIT.
      *--- 2009/02/10 ADD START ----------------------------------------
      *****************************************************************
      *    入院件数集計処理
      *****************************************************************
       20000-NYUINKENSU-SEC                SECTION.
      *
      *    キー設定一時ファイル
           INITIALIZE                 K140TMP-REC
      *
           MOVE    SYU-NYUGAIKBN      TO    K140TMP-NYUGAIKBN
           MOVE    SYU-SRYKA          TO    K140TMP-SRYKA
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
           PERFORM  VARYING  IDA  FROM 1  BY 1
                    UNTIL  IDA  >  31
             PERFORM   920-NYUINACCT-READ-SEC
             IF  NACCT-DAY(IDA)  >  0
      *
               EVALUATE  NACCT-ZAISKBKBN
                 WHEN  "1"
      *    入院料件数加算
                   ADD     1          TO   K140TMP-KAISU(42)
                 WHEN  "3"
                 WHEN  "4"
      *    室料差額件数、食事件数加算
                   IF  NACCT-SRYCDTOTAL  NOT =  "00000197000570"
                     ADD    1         TO   K140TMP-KAISU(41)
                   END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
      *
           IF  FLG-M01TMP   =  1
               WRITE    K140TMP-REC
           ELSE
               REWRITE  K140TMP-REC
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
      *--- 2009/02/10 ADD END ------------------------------------------
      *****************************************************************
      *    診療行為情報取得設定
      *****************************************************************
       20012-SRY-SET                SECTION.
      *
      *    キー設定
           INITIALIZE                      K140TMP-REC
      *
           MOVE    ACCT-NYUGAIKBN    TO    K140TMP-NYUGAIKBN
           MOVE    ACCT-SRYKA        TO    K140TMP-SRYKA
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      * 20071113 老人の値が前期高齢者に集計される不具合を修正START t.tanaka
           PERFORM  930-HKNCOMBI-SET-SEC
      * 20071113 老人の値が前期高齢者に集計される不具合を修正END
      *
      * 20080128 保険組合せ番号９９９９(包括分)は対象外にするSTART t.tanaka
           IF ACCT-HKNCOMBI NOT = "9999"
      * 20080128 保険組合せ番号９９９９(包括分)は対象外にするEND
      *
      *    診療行為情報読み込み
      *    (14)往診・在宅,(60)検査,(95)非課税自費,(96)課税自費は
      *    診療コード毎に分類する
               IF    ACCT-SRYKBN  =  "14"
                  OR   ACCT-SRYKBN  =  "60"
      *--- 2009/12/14 ADD START ----------------------------------------
                  OR   ACCT-SRYKBN  =  "64"
      *--- 2009/12/14 ADD END ------------------------------------------
                  OR   ACCT-SRYKBN  =  "95"
                  OR   ACCT-SRYKBN  =  "96"
                   PERFORM  200121-SRYCD-CHK-SEC
      ***** 04/05/17 INS
      *    検査・在宅の合計点数集計方法修正
                   IF     ACCT-SRYKBN  =  "14" OR "60"
      *--- 2009/12/14 ADD START ----------------------------------------
                                           OR  "64"
      *--- 2009/12/14 ADD END ------------------------------------------
                       PERFORM  2001211-SYUKEI-SEC
                   END-IF
      *****
               ELSE
                       PERFORM  200122-ETC-SEC
               END-IF
      *    
      * 20080128 保険組合せ番号９９９９(包括分)は対象外にするSTART t.tanaka
           END-IF
      * 20080128 保険組合せ番号９９９９(包括分)は対象外にするEND
      *
           IF   K140TMP-TENSU(IDX1)  NOT = ZERO
               AND K140TMP-KAISU(IDX1)  NOT = ZERO
               IF  FLG-M01TMP   =  1
                   WRITE    K140TMP-REC
               ELSE
                   REWRITE  K140TMP-REC
               END-IF
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
                        UNTIL    IDZ  >     5  OR
                        SRY-SRYCD(IDZ)    =   SPACE
                   EVALUATE   SRY-SRYKBN
                       WHEN   "14"
                           PERFORM  200121-ZAITAKU-SET-SEC
                       WHEN   "60"
      *--- 2009/12/14 ADD START ----------------------------------------
                       WHEN   "64"
      *--- 2009/12/14 ADD END ------------------------------------------
                           PERFORM  200121-KENSA-SET-SEC
                       WHEN   "95"
                       WHEN   "96"
                           PERFORM  200121-JIHI-SET-SEC
                   END-EVALUATE
               END-PERFORM
               PERFORM  900-SRYACT-FETCH-SEC
           END-PERFORM  
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
      * 20071213 往診・在宅の判定条件を修正START t.tanaka 
      *     IF    TNS-OSINKBN   =    1
           IF    TNS-CDKBN-KBN     =  "C"
               AND   TNS-CDKBN-KBNNUM  =  ZERO
      * 20071213 往診・在宅の判定条件を修正END 
               MOVE    12    TO   IDX1
           ELSE
               MOVE    13    TO   IDX1
           END-IF
      *
      *    点数加算
      * 20080801 検査と在宅の点数と回数の計算式を修正START
      *    COMPUTE   WRK-TENSU  =
      *               TNS-TEN    *      ACCT-ZAIKAISU
      *    ADD      WRK-TENSU      TO     K140TMP-TENSU(IDX1)
           IF ACCT-ZAINUM NOT =  WRK-ZAINUM
               COMPUTE  WRK-TENSU  =  
                        ACCT-ZAITEN  * ACCT-ZAIKAISU
               ADD      WRK-TENSU        TO     K140TMP-TENSU(IDX1)
      * 
      *    回数加算
               ADD      ACCT-ZAIKAISU    TO     K140TMP-KAISU(IDX1)
           END-IF
           MOVE   ACCT-ZAINUM TO  WRK-ZAINUM
      * 20080801 検査と在宅の点数と回数の計算式を修正END
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
      *--- 2009/12/14 ADD START ----------------------------------------
           IF   SRY-SRYKBN  =  "60"
      *--- 2009/12/14 ADD END ------------------------------------------
      *    採血チェック
             MOVE    2              TO   WRK-KNSBUNRUI
             PERFORM  900-KENSA-READ-SEC
             IF   FLG-KENSASORT  =  ZERO
               MOVE    33           TO   IDX1
             ELSE
      *    判断料チェック
               MOVE    99           TO   WRK-KNSBUNRUI
               PERFORM  900-KENSA-READ-SEC
               IF   FLG-KENSASORT  =  ZERO
                 MOVE    34         TO   IDX1
               ELSE
                 MOVE    32         TO   IDX1
               END-IF
             END-IF
      *--- 2009/12/14 ADD START ----------------------------------------
           ELSE
             MOVE    54          TO   IDX1
           END-IF
      *--- 2009/12/14 ADD END ------------------------------------------
      *
      * 20080801 検査と在宅の点数と回数の計算式を修正START
      *    点数取得
      *     PERFORM  900-TENSU-READ-SEC
      *
      *    点数加算
      *     COMPUTE   WRK-TENSU  =
      *               TNS-TEN    *      ACCT-ZAIKAISU
      *     ADD      WRK-TENSU      TO     K140TMP-TENSU(IDX1)
           IF ACCT-ZAINUM NOT =  WRK-ZAINUM
               COMPUTE  WRK-TENSU  =  
                        ACCT-ZAITEN  * ACCT-ZAIKAISU
               ADD      WRK-TENSU      TO    K140TMP-TENSU(IDX1)
      *
      *    回数加算
               ADD      ACCT-ZAIKAISU  TO    K140TMP-KAISU(IDX1)
           END-IF
           MOVE   ACCT-ZAINUM TO  WRK-ZAINUM
      * 20080801 検査と在宅の点数と回数の計算式を修正END
      *
           .
       200121-KENSA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査・在宅合計点数設定処理
      *****************************************************************
       2001211-SYUKEI-SEC                SECTION.
      *
           IF     ACCT-SRYKBN       =     "14"
               MOVE     14            TO    IDX1
           ELSE
               MOVE     35            TO    IDX1
           END-IF
      *
      *
      *    点数加算
           COMPUTE  WRK-TENSU       =
                    ACCT-ZAITEN     *      ACCT-ZAIKAISU
           ADD      WRK-TENSU       TO     K140TMP-TENSU(IDX1)
      *
      *    回数加算
           ADD    ACCT-ZAIKAISU  TO     K140TMP-KAISU(IDX1)
      *
      *
           .
       2001211-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険適用外点数、自費項目金額設定処理
      *****************************************************************
       200121-JIHI-SET-SEC                SECTION.
      *
           PERFORM  900-TENSU-READ-SEC
      *    点数、金額がゼロの場合は加算処理は行わない。
      * 20080801 自費の都度入力が印字しない不具合を修正START
      *     IF       TNS-TEN     =     ZERO
           IF       SRY-JIHIMONEY(IDZ)     =     ZERO
      * 20080801 自費の都度入力が印字しない不具合を修正END
      
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
      ***  20040707  @@@@-MOTOHASI  変更
                       WHEN  1     
      ***                     MOVE    45     TO   IDX1
                           MOVE    43     TO   IDX1
                       WHEN  2     
      ***                     MOVE    46     TO   IDX1
                           MOVE    44     TO   IDX1
                       WHEN  3     
      ***                     MOVE    47     TO   IDX1
                           MOVE    45     TO   IDX1
                       WHEN  4     
      ***                     MOVE    48     TO   IDX1
                           MOVE    46     TO   IDX1
                       WHEN  5     
      ***                     MOVE    49     TO   IDX1
                           MOVE    47     TO   IDX1
      ***  20040707  ここまで

      *=== 自費10項目修正 Satoh (2007/06/28) START
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
      *=== 自費10項目修正 Satoh (2007/06/28) END
                       WHEN  OTHER     
      ***                     MOVE    49     TO   IDX1
      *=== 自費10項目修正 Satoh (2007/06/28) START
      *                     MOVE    48     TO   IDX1
                           MOVE    53     TO   IDX1
      *=== 自費10項目修正 Satoh (2007/06/28) END
      *
                   END-EVALUATE
               ELSE
                   EVALUATE  TNS-GAITENTTLKBN
      *
      ***  20040707  @@@@-MOTOHASI  変更
      ***  20040707  @@@@-MOTOHASI  変更
                       WHEN  1     
      ***                     MOVE    45     TO   IDX1
                           MOVE    43     TO   IDX1
                       WHEN  2     
      ***                     MOVE    46     TO   IDX1
                           MOVE    44     TO   IDX1
                       WHEN  3     
      ***                     MOVE    47     TO   IDX1
                           MOVE    45     TO   IDX1
                       WHEN  4     
      ***                     MOVE    48     TO   IDX1
                           MOVE    46     TO   IDX1
                       WHEN  5     
      ***                     MOVE    49     TO   IDX1
                           MOVE    47     TO   IDX1
      ***  20040707  ここまで

      *=== 自費10項目修正 Satoh (2007/06/28) START
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
      *=== 自費10項目修正 Satoh (2007/06/28) END
                       WHEN  OTHER     
      ***                     MOVE    49     TO   IDX1
      *=== 自費10項目修正 Satoh (2007/06/28) START
      *                     MOVE    48     TO   IDX1
                           MOVE    53     TO   IDX1
      *=== 自費10項目修正 Satoh (2007/06/28) END
      *
                   END-EVALUATE
               END-IF
           ELSE
      *
      *      保険適用外フラグON
               MOVE  1     TO   FLG-HKNTG
               IF    K100LNK-HKNTG   =      SPACE
      ***          保険適用外取扱区分がSPACEの場合は加算処理は行わない。
                   GO              TO     200121-JIHI-SET-EXT
               END-IF
               IF    TNS-DATAKBN     =      "1"
                   PERFORM   20013-IDXSET2-SEC
               ELSE 
                   EVALUATE  TNS-SRYCD 
                       WHEN  "610000000" THRU "619999999"  
                           PERFORM   20013-IDXSET2-SEC
                       WHEN  "660000000" THRU "669999999"  
                           PERFORM   20013-IDXSET2-SEC
                       WHEN  OTHER  
                           GO    TO  200121-JIHI-SET-EXT     
                   END-EVALUATE 
               END-IF    
           END-IF
      *
      *    点数加算
           IF     FLG-HKNTG      =      1
      *
      * 保険適用外の場合
      *
               EVALUATE  TNS-TENSIKIBETU
                   WHEN  1
      ***            金額項目の場合は点数に変換してデータ化する。
                       COMPUTE   WRK-CNV-TEN-V   =
                         (TNS-TEN  * SRY-SRYSURYO(IDZ)) / 10
                       COMPUTE   WRK-CNV-TEN     ROUNDED  =
                         WRK-CNV-TEN-V  *  1
                   WHEN  3
                   WHEN  8
      ***            点数項目の場合は回数をデータ化する。
                       MOVE   TNS-TEN  TO     WRK-CNV-TEN
                   WHEN  OTHER
      ***           点数、金額以外の場合
                       GO              TO     200121-JIHI-SET-EXT     
               END-EVALUATE
               COMPUTE  WRK-TENSU   =
                        WRK-CNV-TEN *      ACCT-ZAIKAISU
               ADD      WRK-TENSU   TO     K140TMP-TENSU(IDX1)
           ELSE
      * 自費項目の場合
               COMPUTE  WRK-TENSU   =
      * 20080801 自費の都度入力が印字しない不具合を修正START
      *                 TNS-TEN     *      ACCT-ZAIKAISU
                        SRY-JIHIMONEY(IDZ)  *  ACCT-ZAIKAISU
      * 20080801 自費の都度入力が印字しない不具合を修正END
               ADD      WRK-TENSU   TO     K140TMP-TENSU(IDX1)
           END-IF
      *
      *    回数加算
           ADD    ACCT-ZAIKAISU   TO     K140TMP-KAISU(IDX1)
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
      ***        点数、金額がゼロの場合は加算設定処理は行わない。
               GO           TO       200122-ETC-EXT 
           END-IF
      *
      *    集計インデックス設定
           PERFORM  20013-IDXSET-SEC
      *
      *--- 2009/02/10 ADD START ----------------------------------------
           IF  IDX1  NOT =  42 
      *--- 2009/02/10 ADD END ------------------------------------------
      *    点数加算
             COMPUTE  WRK-TENSU       =
                      ACCT-ZAITEN     *      ACCT-ZAIKAISU
             ADD      WRK-TENSU       TO     K140TMP-TENSU(IDX1)
      *
      *    回数加算
             ADD    ACCT-ZAIKAISU  TO     K140TMP-KAISU(IDX1)
      *--- 2009/02/10 ADD START ----------------------------------------
           END-IF
      *--- 2009/02/10 ADD END ------------------------------------------
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
      * 20080417 H20年4月改正対応START tanaka
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
      * 20080417 H20年4月改正対応END
           MOVE    ZERO    TO    IDX1
           EVALUATE  SRY-SRYKBN
           WHEN "11" THRU "14"
               EVALUATE  SRY-SRYSYUKBN
                   WHEN "110"
                       MOVE     PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                       MOVE     K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                       COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                       IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                           SUBTRACT  1     FROM WRK-NENREI  
                       END-IF
                       IF  COMB-KOH1HKNNUM      =     "027"
      * 20080417 H20年4月改正対応START tanaka
                           OR   COMB-HKNNUM  =  "039"
                           OR   COMB-HKNNUM  =  "040"
      * 20080417 H20年4月改正対応END
                           MOVE      4    TO    IDX1
                       ELSE
                           IF  WRK-NENREI   >= 70 AND WRK-NENREI  < 75
                               MOVE    3    TO    IDX1
                           ELSE
      * 20080417 H20年4月改正対応START tanaka
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
      * 20080417 H20年4月改正対応END
                           END-IF
                       END-IF
                   WHEN "120"
                       MOVE    PTINF-BIRTHDAY(1:4)   TO    WRK-YY1-G
                       MOVE    K100LNK-SRYYM(1:4)    TO    WRK-YY2-G
                       COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                       IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                           SUBTRACT  1    FROM  WRK-NENREI  
                       END-IF
                       IF  COMB-KOH1HKNNUM      =  "027"
      * 20080417 H20年4月改正対応START tanaka
                           OR   COMB-HKNNUM  =  "039"
                           OR   COMB-HKNNUM  =  "040"
      * 20080417 H20年4月改正対応END
                           MOVE      8    TO    IDX1
                       ELSE
                           IF  WRK-NENREI   >= 70 AND WRK-NENREI  < 75
                               MOVE    7    TO    IDX1
                           ELSE
      * 20080417 H20年4月改正対応START tanaka
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
      * 20080417 H20年4月改正対応END
                           END-IF
                       END-IF
                   WHEN "130"
                       MOVE    9      TO    IDX1
                   WHEN OTHER
                       MOVE    10     TO    IDX1
               END-EVALUATE
               WHEN "21" THRU "27"
               EVALUATE  SRY-SRYSYUKBN
                   WHEN "210" THRU "213"
                   WHEN "290" THRU "292"
                       MOVE    15     TO    IDX1
                   WHEN "220" THRU "223"
                       MOVE    16     TO    IDX1
                   WHEN "230" THRU "233"
                       MOVE    17     TO    IDX1
                   WHEN "240" THRU "242"
                       MOVE    18     TO    IDX1
                   WHEN "250"
                       MOVE    19     TO    IDX1
                   WHEN "260"
                       MOVE    20     TO    IDX1
                   WHEN "270"
                       MOVE    21     TO    IDX1
               END-EVALUATE
           WHEN "31" THRU "34"
               EVALUATE  SRY-SRYSYUKBN
                   WHEN "310" THRU "311"
                       MOVE   23      TO    IDX1
                   WHEN "320" THRU "321"
                       MOVE   24      TO    IDX1
                   WHEN "330" THRU "331"
                   WHEN "340"
                       MOVE   25      TO    IDX1
               END-EVALUATE
           WHEN "40"
               MOVE     27      TO    IDX1
           WHEN "50" THRU "54"
               EVALUATE  SRY-SRYSYUKBN
                   WHEN "500" THRU "502"
                       MOVE   28      TO    IDX1
                   WHEN "510"
                       MOVE   29      TO    IDX1
                   WHEN "540" THRU "542"
                       MOVE   30      TO    IDX1
               END-EVALUATE
           WHEN "70"
               MOVE     36      TO    IDX1
           WHEN "80"
      * 20080417 H20年4月改正対応START tanaka
      *--- 2009/12/14 DELETE START -------------------------------------
      *     WHEN "64"
      *--- 2009/12/14 DELETE END ---------------------------------------
      * 20080417 H20年4月改正対応END
               EVALUATE  SRY-SRYSYUKBN
                  WHEN "820"
                      MOVE   37      TO    IDX1
      *--- 2009/12/14 ADD START ----------------------------------------
                  WHEN "800"
      *--- 2009/12/14 ADD END ------------------------------------------
                  WHEN "810"
                      MOVE   38      TO    IDX1
      * 20080417 H20年4月改正対応START tanaka
      *--- 2009/12/14 ADD + DELETE START -------------------------------
      *                 WHEN "640"
                  WHEN "840"
      * 20080417 H20年4月改正対応END
      *            WHEN "800"
      *--- 2009/12/14 ADD + DELETE END ---------------------------------
      * 20080303 精神科専門療法をその他に集計するよう修正START tanaka
                  WHEN "830"
      * 20080303 精神科専門療法をその他に集計するよう修正END
      *=== 療養担当手当対応 UP By Uchida(2007/05/15) start
                  WHEN "850"
      *=== 療養担当手当対応 UP By Uchida(2007/05/15) end
                      MOVE   39      TO    IDX1
              END-EVALUATE
      *
      ***  20040707  @@@@-MOTOHASI  変更
      ***     WHEN "90"
      ***     WHEN "92"
      ***     WHEN "97"
      ***       EVALUATE SRY-SRYSYUKBN
      ***       WHEN "900"
      ***         MOVE   41      TO    IDX1
      ***       WHEN "920"
      ***         MOVE   42      TO    IDX1
      ***       WHEN "970"
      ***       WHEN "971"
      ***         MOVE   43      TO    IDX1
      ***       END-EVALUATE
      ***     WHEN "95" THRU "96"
      ***         MOVE   50      TO    IDX1
           WHEN "90"
           WHEN "92"
           WHEN "97"
               EVALUATE SRY-SRYSYUKBN
                   WHEN "900"
                   WHEN "920"
                       MOVE   42      TO    IDX1
                   WHEN "970"
                   WHEN "971"
                       MOVE   41      TO    IDX1
               END-EVALUATE
           WHEN "95" THRU "96"
      *=== 自費10項目修正 Satoh (2007/06/28) START
      *         MOVE     48        TO    IDX1
      ****  20040707  ここまで
               MOVE     53        TO    IDX1
      *=== 自費10項目修正 Satoh (2007/06/28) END
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
                           IF  PTINF-BIRTHDAY(5:2) > K100LNK-SRYYM(5:2)
                               SUBTRACT  1       FROM  WRK-NENREI  
                           END-IF
                           IF  COMB-KOH1HKNNUM    =  "027"
      * 20080417 H20年4月改正対応START tanaka
                               OR   COMB-HKNNUM  =  "039"
                               OR   COMB-HKNNUM  =  "040"
      * 20080417 H20年4月改正対応END
                             MOVE      4          TO    IDX1
                           ELSE
                               IF  WRK-NENREI   >=    70
                                 MOVE    3      TO    IDX1
                               ELSE
      * 20080417 H20年4月改正対応START tanaka
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
      * 20080417 H20年4月改正対応END
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
      * 20080417 H20年4月改正対応START tanaka
                       OR   COMB-HKNNUM  =  "039"
                       OR   COMB-HKNNUM  =  "040"
      * 20080417 H20年4月改正対応END
                       MOVE    8      TO    IDX1
                   ELSE
                       IF  WRK-NENREI   >= 70 AND WRK-NENREI   < 75
                         MOVE    7      TO    IDX1
                       ELSE
      * 20080417 H20年4月改正対応START tanaka
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
      * 20080417 H20年4月改正対応END
                       END-IF
                   END-IF
                   WHEN "13"
                       MOVE     9         TO    IDX1 
                   WHEN "14"
                       MOVE     13        TO    IDX1
                   WHEN "21"
                       MOVE     15        TO    IDX1
                   WHEN "22"
                       MOVE     16        TO    IDX1
                   WHEN "23"
                       MOVE     17        TO    IDX1
                   WHEN "24"
                       MOVE     18        TO    IDX1
                   WHEN "25"
                       MOVE     19        TO    IDX1
                   WHEN "26"
                       MOVE     20        TO    IDX1
                   WHEN "27"
                       MOVE     21        TO    IDX1
                   WHEN "31"
                       MOVE     23        TO    IDX1
                   WHEN "32"
                       MOVE     24        TO    IDX1
                   WHEN "33"
                       MOVE     25        TO    IDX1
                   WHEN "40"
                       MOVE     27        TO    IDX1
                   WHEN "50"
                       MOVE     28        TO    IDX1
                   WHEN "51"
                       MOVE     29        TO    IDX1
                   WHEN "54"
                       MOVE     30        TO    IDX1
                   WHEN "70"
                       MOVE     36        TO    IDX1
                   WHEN "80"
                   WHEN "93"
                       MOVE     37        TO    IDX1
      *
      ***  20040707  @@@@-MOTOHASI  変更
      ***       WHEN "95"
      ***       WHEN "96"
      ***         MOVE     50        TO    IDX1
      ***       WHEN "97"
      ***         MOVE     43        TO    IDX1
      ***       WHEN "90"
      ***         MOVE     41        TO    IDX1
      ***       WHEN "92"
      ***         MOVE     42        TO    IDX1
      ***       WHEN OTHER
      ***         MOVE     39        TO    IDX1
                   WHEN "95"
                   WHEN "96"
      *=== 自費10項目修正 Satoh (2007/06/28) START
      *         MOVE     48        TO    IDX1
                       MOVE     53        TO    IDX1
      *=== 自費10項目修正 Satoh (2007/06/28) END
                   WHEN "97"
                       MOVE     41        TO    IDX1
                   WHEN "90"
                       MOVE     42        TO    IDX1
                   WHEN "92"
                       MOVE     42        TO    IDX1
                   WHEN OTHER
                       MOVE     39        TO    IDX1
      ***  20040707  ここまで
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
      * 20080417 H20年4月改正対応START tanaka
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    ACCT-HOSPNUM        TO  AGECHK-HOSPNUM
           MOVE    ACCT-PTID           TO  AGECHK-PTID
           MOVE    K100LNK-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"       TO  AGECHK-SRYYMD(7:2)
           MOVE    COMB-HKNCOMBINUM    TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      
      * 20080417 H20年4月改正対応END

           MOVE      ZERO    TO    IDX1
           EVALUATE  TNS-SRYKBN
           WHEN "11" THRU "14"
               EVALUATE  TNS-SRYSYUKBN
                   WHEN "110"
                       MOVE     PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                       MOVE     K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                       COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                       IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                           SUBTRACT  1     FROM WRK-NENREI  
                       END-IF
                       IF  COMB-KOH1HKNNUM      =     "027"
      * 20080417 H20年4月改正対応START tanaka
                           OR   COMB-HKNNUM  =  "039"
                           OR   COMB-HKNNUM  =  "040"
      * 20080417 H20年4月改正対応END
                             MOVE      4    TO    IDX1
                       ELSE
                           IF  WRK-NENREI   >= 70 AND WRK-NENREI   < 75
                               MOVE    3    TO    IDX1
                           ELSE
      * 20080417 H20年4月改正対応START tanaka
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
      * 20080417 H20年4月改正対応END
                       END-IF
                   END-IF
                   WHEN "120"
                   MOVE    PTINF-BIRTHDAY(1:4)   TO    WRK-YY1-G
                   MOVE    K100LNK-SRYYM(1:4)    TO    WRK-YY2-G
                   COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                   IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                       SUBTRACT  1    FROM  WRK-NENREI  
                   END-IF
                   IF  COMB-KOH1HKNNUM      =  "027"
      * 20080417 H20年4月改正対応START tanaka
                       OR   COMB-HKNNUM  =  "039"
                       OR   COMB-HKNNUM  =  "040"
      * 20080417 H20年4月改正対応END
                       MOVE      8    TO    IDX1
                   ELSE
                       IF  WRK-NENREI   >= 70 AND WRK-NENREI   < 75
                           MOVE    7    TO    IDX1
                       ELSE
      * 20080417 H20年4月改正対応START tanaka
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
      * 20080417 H20年4月改正対応END
                       END-IF
                   END-IF
                   WHEN "130"
                       MOVE    9     TO    IDX1
                   WHEN OTHER
                       MOVE    10    TO    IDX1
               END-EVALUATE
           WHEN "21" THRU "27"
               EVALUATE  TNS-SRYSYUKBN
                   WHEN "210" THRU "213"
                   WHEN "290" THRU "292"
                       MOVE    15     TO    IDX1
                   WHEN "220" THRU "223"
                       MOVE    16     TO    IDX1
                   WHEN "230" THRU "233"
                       MOVE    17     TO    IDX1
                   WHEN "240" THRU "242"
                       MOVE    18     TO    IDX1
                   WHEN "250"
                       MOVE    19     TO    IDX1
                   WHEN "260"
                       MOVE    20     TO    IDX1
                   WHEN "270"
                       MOVE    21     TO    IDX1
               END-EVALUATE
           WHEN "31" THRU "34"
               EVALUATE  TNS-SRYSYUKBN
                   WHEN "310" THRU "311"
                       MOVE   23      TO    IDX1
                   WHEN "320" THRU "321"
                       MOVE   24      TO    IDX1
                   WHEN "330" THRU "331"
                   WHEN "340"
                       MOVE   25      TO    IDX1
               END-EVALUATE
           WHEN "40"
               MOVE     27      TO    IDX1
           WHEN "50" THRU "54"
               EVALUATE  TNS-SRYSYUKBN
                   WHEN "500" THRU "502"
                       MOVE   28      TO    IDX1
                   WHEN "510"
                       MOVE   29      TO    IDX1
                   WHEN "540" THRU "542"
                       MOVE   30      TO    IDX1
               END-EVALUATE
           WHEN "70"
               MOVE     36      TO    IDX1
           WHEN "80"
      * 20080417 H20年4月改正対応START tanaka
      *--- 2009/12/14 DELETE START -------------------------------------
      *     WHEN "64"
      *--- 2009/12/14 DELETE END ---------------------------------------
      * 20080417 H20年4月改正対応END
               EVALUATE  TNS-SRYSYUKBN
                   WHEN "820"   
                       MOVE   37      TO    IDX1
      *--- 2009/12/14 ADD START ----------------------------------------
                   WHEN "800"
      *--- 2009/12/14 ADD END ------------------------------------------
                   WHEN "810"
                       MOVE   38      TO    IDX1
      * 20080417 H20年4月改正対応START tanaka
      *--- 2009/12/14 ADD + DELETE START -------------------------------
      *                 WHEN "640"
                   WHEN "840"
      * 20080417 H20年4月改正対応END
      *             WHEN "800"
      *--- 2009/12/14 ADD + DELETE END ---------------------------------
      * 20080303 精神科専門療法をその他に集計するよう修正START tanaka
                   WHEN "830"
      * 20080303 精神科専門療法をその他に集計するよう修正END
      *=== 療養担当手当対応 UP By Uchida(2007/05/15) start
                   WHEN "850"
      *=== 療養担当手当対応 UP By Uchida(2007/05/15) end
                       MOVE   39      TO    IDX1
               END-EVALUATE
      *
      ***  20040707  @@@@-MOTOHASI  変更
      ***     WHEN "90"
      ***     WHEN "92"
      ***     WHEN "97"
      ***       EVALUATE TNS-SRYSYUKBN
      ***       WHEN "900"
      ***         MOVE   41      TO    IDX1
      ***       WHEN "920"
      ***         MOVE   42      TO    IDX1
      ***       WHEN "970"
      ***       WHEN "971"
      ***         MOVE   43      TO    IDX1
      ***       END-EVALUATE
           WHEN "90"
           WHEN "92"
           WHEN "97"
               EVALUATE TNS-SRYSYUKBN
                   WHEN "900"
                   WHEN "920"
                       MOVE   42      TO    IDX1
                   WHEN "970"
                   WHEN "971"
                       MOVE   41      TO    IDX1
               END-EVALUATE
      ***  20040707  ここまで
      *
           END-EVALUATE
      *
      *    診療種別がスペースだったら診療区分で分類する
           IF   IDX1   =      ZERO
               EVALUATE  TNS-SRYKBN
                   WHEN "11"
                       MOVE    PTINF-BIRTHDAY(1:4)    TO    WRK-YY1-G
                       MOVE    K100LNK-SRYYM(1:4)     TO    WRK-YY2-G
                       COMPUTE  WRK-NENREI   =  WRK-YY2   -    WRK-YY1
                       IF  PTINF-BIRTHDAY(5:2)  >  K100LNK-SRYYM(5:2)
                           SUBTRACT  1       FROM  WRK-NENREI  
                       END-IF
                       IF  COMB-KOH1HKNNUM    =  "027"
      * 20080417 H20年4月改正対応START tanaka
                           OR   COMB-HKNNUM  =  "039"
                           OR   COMB-HKNNUM  =  "040"
      * 20080417 H20年4月改正対応END
                           MOVE      4          TO    IDX1
                       ELSE
                           IF  WRK-NENREI   >=    70
                               MOVE    3      TO    IDX1
                           ELSE
      * 20080417 H20年4月改正対応START tanaka
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
      * 20080417 H20年4月改正対応END
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
      * 20080417 H20年4月改正対応START tanaka
                           OR   COMB-HKNNUM  =  "039"
                           OR   COMB-HKNNUM  =  "040"
      * 20080417 H20年4月改正対応END
                           MOVE    8      TO    IDX1
                       ELSE
                           IF  WRK-NENREI   >= 70 AND WRK-NENREI   < 75
                               MOVE    7      TO    IDX1
                           ELSE
      * 20080417 H20年4月改正対応START tanaka
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
      * 20080417 H20年4月改正対応END
                           END-IF
                       END-IF
                   WHEN "13"
                       MOVE     9         TO    IDX1 
                   WHEN "14"
                       MOVE     13        TO    IDX1
                   WHEN "21"
                       MOVE     15        TO    IDX1
                   WHEN "22"
                       MOVE     16        TO    IDX1
                   WHEN "23"
                       MOVE     17        TO    IDX1
                   WHEN "24"
                       MOVE     18        TO    IDX1
                   WHEN "25"
                       MOVE     19        TO    IDX1
                   WHEN "26"
                       MOVE     20        TO    IDX1
                   WHEN "27"
                       MOVE     21        TO    IDX1
                   WHEN "31"
                       MOVE     23        TO    IDX1
                   WHEN "32"
                       MOVE     24        TO    IDX1
                   WHEN "33"
                       MOVE     25        TO    IDX1
                   WHEN "40"
                       MOVE     27        TO    IDX1
                   WHEN "50"
                       MOVE     28        TO    IDX1
                   WHEN "51"
                       MOVE     29        TO    IDX1
                   WHEN "54"
                       MOVE     30        TO    IDX1
                   WHEN "70"
                       MOVE     36        TO    IDX1
                   WHEN "80"
                   WHEN "93"
                       MOVE     37        TO    IDX1
      *
      ***  20040707  @@@@-MOTOHASI  変更
      ***       WHEN "97"
      ***         MOVE     43        TO    IDX1
      ***       WHEN "90"
      ***         MOVE     41        TO    IDX1
      ***       WHEN "92"
      ***         MOVE     42        TO    IDX1
                   WHEN "97"
                       MOVE     41        TO    IDX1
                   WHEN "90"
                       MOVE     42        TO    IDX1
                   WHEN "92"
                       MOVE     42        TO    IDX1
      ***  20040707  ここまで
      *
                   WHEN OTHER
                       EVALUATE TNS-SRYCD
                           WHEN "610000000"  THRU  "619999999"
      *              内服薬
                               MOVE     15   TO    IDX1
                           WHEN "660000000"  THRU  "669999999"
      *              外用薬
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
           OPEN    INPUT       K140T-FILE
           MOVE    ZERO    TO  FLG-PRINT
           INITIALIZE          K140TMP-REC
      *
      *    一時ファイル読込
           PERFORM 900-TMP-READ-SEC
           INITIALIZE                       WRK-K140ALL-AREA
           MOVE    ZERO                TO   IDX2
           MOVE    K140TMP-NYUGAIKBN   TO   WRK-TMP-NYUGAIKBN
      *    見出し編集
           PERFORM  310-HEAD-HEN-SEC
      *
           PERFORM  UNTIL  FLG-PRINT   =    1
      *
               IF K140TMP-NYUGAIKBN   NOT =   WRK-TMP-NYUGAIKBN  
      *         全診療科合計明細の印字
      *         印刷処理
                   IF   IDX2              >=   6
      *              印刷処理
                       PERFORM  330-PRINT-OUT-SEC
                       MOVE  ZERO         TO  IDX2
                   END-IF
      *         全診療科明細編集
                   ADD      1              TO  IDX2
                   PERFORM  320-BODY-HEN2-SEC
      *         印刷処理
                   PERFORM  330-PRINT-OUT-SEC
                   INITIALIZE                  WRK-K140ALL-AREA
                   MOVE  ZERO              TO  IDX2
                   MOVE  K140TMP-NYUGAIKBN TO  WRK-TMP-NYUGAIKBN
      *         見出し編集
                   PERFORM  310-HEAD-HEN-SEC
               END-IF
      *
      *      １頁内で診療科のMAXに達している場合
               IF IDX2        >=     6
      *         印刷処理
                   PERFORM  330-PRINT-OUT-SEC
                   MOVE  ZERO              TO  IDX2
      *         見出し編集
                   PERFORM  310-HEAD-HEN-SEC
               END-IF
      *
      *    診療科別明細編集
               ADD      1   TO   IDX2
               PERFORM  320-BODY-HEN-SEC
      *
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
      *    全診療科合計明細の印字
           IF   IDX2              >=   6
      *         印刷処理
                PERFORM  330-PRINT-OUT-SEC
                MOVE  ZERO         TO  IDX2
      *         見出し編集
                PERFORM  310-HEAD-HEN-SEC
           END-IF
      *    全診療科明細編集
           ADD      1              TO  IDX2
           PERFORM  320-BODY-HEN2-SEC
      *
           IF   CNT-K140TMP   =    ZERO
               MOVE   "対象データが存在しません"
                                   TO  K100-JOBMSG
               MOVE   SPACE          TO  K100-NYUGAIKBN
           END-IF    
      *
      *    印刷処理
           PERFORM  330-PRINT-OUT-SEC
      *
           CLOSE    K140T-FILE
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
           MOVE   "(診療科別)"      TO    K100-SUBTITLE
      *
      *    ページ数
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    K100-PAGE
      *
           IF    K140TMP-NYUGAIKBN      =    "1"
               MOVE     "入院"        TO   K100-NYUGAIKBN
               PERFORM VARYING  IDX3  FROM 1  BY  1
      *=== 自費10項目修正 Satoh (2007/06/28) START
      *               UNTIL    IDX3  >    5 
                       UNTIL    IDX3  >    10 
      *=== 自費10項目修正 Satoh (2007/06/28) START

                   MOVE  WRK-TJIHIN(IDX3) TO  K100-TJIHI(IDX3)
               END-PERFORM
           ELSE
               MOVE     "外来"        TO   K100-NYUGAIKBN
               PERFORM VARYING  IDX3  FROM 1  BY  1

      *=== 自費10項目修正 Satoh (2007/06/28) START
      *               UNTIL    IDX3  >    5 
                       UNTIL    IDX3  >    10 
      *=== 自費10項目修正 Satoh (2007/06/28) START

                   MOVE  WRK-TJIHI(IDX3)  TO  K100-TJIHI(IDX3)
               END-PERFORM
           END-IF
      *
           MOVE    WRK-SRYYM        TO   K100-SRYYM
           MOVE    WRK-SEIYMD       TO   K100-SYSYMD
      * 20080417 H20年4月改正対応START tanaka
           IF   K100LNK-SRYYM  <   "200804"
               MOVE   "(３歳未満)"    TO  K100-NENREIKBN(1)
               MOVE   "(老人)"        TO  K100-NENREIKBN2(1)
               MOVE   "(３歳未満)"    TO  K100-NENREIKBN(2)
               MOVE   "(老人)"        TO  K100-NENREIKBN2(2)
           ELSE
               MOVE   "(６歳)"   TO  K100-NENREIKBN(1)
               MOVE   "(後期高齢者)"       TO  K100-NENREIKBN2(1)
               MOVE   "(６歳)"   TO  K100-NENREIKBN(2)
               MOVE   "(後期高齢者)"       TO  K100-NENREIKBN2(2)
           END-IF
      * 20080417 H20年4月改正対応END
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<診療科別明細>処理
      *****************************************************************
       320-BODY-HEN-SEC               SECTION.
      *
           MOVE     1               TO    IDX4
           MOVE     WRK-SRYKAMAX    TO    IDX-MAX
           PERFORM  UNTIL    IDX4   >     IDX-MAX
               IF  K140TMP-SRYKA      =     WRK-SRYKA(IDX4)
                   MOVE    ZERO                   TO    IDX-MAX
                   MOVE    WRK-SRYKANAME(IDX4)    TO    K100-TNAME(IDX2)
               ELSE
                   ADD     1                      TO    IDX4
               END-IF
           END-PERFORM
      *
           PERFORM    VARYING     IDX1    FROM  1  BY  1
                      UNTIL       IDX1    >     56
      *
               EVALUATE IDX1
                   WHEN 11
                       COMPUTE     K140TMP-TENSU(11)    =
                                   K140TMP-TENSU(01)
                             +     K140TMP-TENSU(02)
                             +     K140TMP-TENSU(03)
                             +     K140TMP-TENSU(04)
                             +     K140TMP-TENSU(05)
                             +     K140TMP-TENSU(06)
                             +     K140TMP-TENSU(07)
                             +     K140TMP-TENSU(08)
                             +     K140TMP-TENSU(09)
                             +     K140TMP-TENSU(10)
                       COMPUTE     K140TMP-KAISU(11)    =
                                   K140TMP-KAISU(01)
                             +     K140TMP-KAISU(02)
                             +     K140TMP-KAISU(03)
                             +     K140TMP-KAISU(04)
                             +     K140TMP-KAISU(05)
                             +     K140TMP-KAISU(06)
                             +     K140TMP-KAISU(07)
                             +     K140TMP-KAISU(08)
                             +     K140TMP-KAISU(09)
                             +     K140TMP-KAISU(10)
      ***** 04/05/17 DEL 在宅の合計は、診療会計より、算出 
      *           WHEN 14
      *                COMPUTE     K140TMP-TENSU(14)    =
      *                            K140TMP-TENSU(12)
      *                      +     K140TMP-TENSU(13)
      *                COMPUTE     K140TMP-KAISU(14)    =
      *                            K140TMP-KAISU(12)
      *                      +     K140TMP-KAISU(13)
      *****
                   WHEN 22
                       COMPUTE     K140TMP-TENSU(22)    =
                                   K140TMP-TENSU(15)
                             +     K140TMP-TENSU(16)
                             +     K140TMP-TENSU(17)
                             +     K140TMP-TENSU(18)
                             +     K140TMP-TENSU(19)
                             +     K140TMP-TENSU(20)
                             +     K140TMP-TENSU(21)
                       COMPUTE     K140TMP-KAISU(22)    =
                                   K140TMP-KAISU(15)
                             +     K140TMP-KAISU(16)
                             +     K140TMP-KAISU(17)
                             +     K140TMP-KAISU(18)
                             +     K140TMP-KAISU(19)
                             +     K140TMP-KAISU(20)
                             +     K140TMP-KAISU(21)
                   WHEN 26
                       COMPUTE     K140TMP-TENSU(26)    =
                                   K140TMP-TENSU(23)
                             +     K140TMP-TENSU(24)
                             +     K140TMP-TENSU(25)
                       COMPUTE     K140TMP-KAISU(26)    =
                                   K140TMP-KAISU(23)
                             +     K140TMP-KAISU(24)
                             +     K140TMP-KAISU(25)
                   WHEN 31
                       COMPUTE     K140TMP-TENSU(31)    =
                                   K140TMP-TENSU(28)
                             +     K140TMP-TENSU(29)
                             +     K140TMP-TENSU(30)
                       COMPUTE     K140TMP-KAISU(31)    =
                                   K140TMP-KAISU(28)
                             +     K140TMP-KAISU(29)
                             +     K140TMP-KAISU(30)
      ***** 04/05/17 DEL 検査の合計は、診療会計より、算出 
      *           WHEN 35
      *                COMPUTE     K140TMP-TENSU(35)    =
      *                            K140TMP-TENSU(32)
      *                      +     K140TMP-TENSU(33)
      *                      +     K140TMP-TENSU(34)
      *                COMPUTE     K140TMP-KAISU(35)    =
      *                            K140TMP-KAISU(32)
      *                      +     K140TMP-KAISU(33)
      *                      +     K140TMP-KAISU(34)
      *****
                   WHEN 40
                       COMPUTE     K140TMP-TENSU(40)    =
                                   K140TMP-TENSU(37)
                             +     K140TMP-TENSU(38)
                             +     K140TMP-TENSU(39)
                       COMPUTE     K140TMP-KAISU(40)    =
                                   K140TMP-KAISU(37)
                             +     K140TMP-KAISU(38)
                             +     K140TMP-KAISU(39)
      *
      ***  20040707  @@@@-MOTOHASI  削除
      ***           WHEN 44
      ***                COMPUTE     K140TMP-TENSU(44)    =
      ***                            K140TMP-TENSU(41)
      ***                      +     K140TMP-TENSU(42)
      ***                COMPUTE     K140TMP-KAISU(44)    =
      ***                            K140TMP-KAISU(41)
      ***                      +     K140TMP-KAISU(42)
      ***  20040707  ここまで
      *
      *
      ***  20040707  @@@@-MOTOHASI  変更
      ***           WHEN 55
      ***                COMPUTE     K140TMP-TENSU(55)    =
      ***                            K140TMP-TENSU(45)
      ***                      +     K140TMP-TENSU(46)
      ***                      +     K140TMP-TENSU(47)
      ***                      +     K140TMP-TENSU(48)
      ***                      +     K140TMP-TENSU(49)
      ***                      +     K140TMP-TENSU(50)
      ***                      +     K140TMP-TENSU(51)
      ***                      +     K140TMP-TENSU(52)
      ***                      +     K140TMP-TENSU(53)
      ***                      +     K140TMP-TENSU(54)
      ***                COMPUTE     K140TMP-KAISU(55)    =
      ***                            K140TMP-KAISU(45)
      ***                      +     K140TMP-KAISU(46)
      ***                      +     K140TMP-KAISU(47)
      ***                      +     K140TMP-KAISU(48)
      ***                      +     K140TMP-KAISU(49)
      ***                      +     K140TMP-KAISU(50)
      ***                      +     K140TMP-KAISU(51)
      ***                      +     K140TMP-KAISU(52)
      ***                      +     K140TMP-KAISU(53)
      ***                      +     K140TMP-KAISU(54)
      ***           WHEN 56
      ***                COMPUTE     K140TMP-TENSU(56)    =
      ***                            K140TMP-TENSU(11)
      ***                      +     K140TMP-TENSU(14)
      ***                      +     K140TMP-TENSU(22)
      ***                      +     K140TMP-TENSU(26)
      ***                      +     K140TMP-TENSU(27)
      ***                      +     K140TMP-TENSU(31)
      ***                      +     K140TMP-TENSU(35)
      ***                      +     K140TMP-TENSU(36)
      ***                      +     K140TMP-TENSU(40)
      ***                      +     K140TMP-TENSU(44)
      ***                COMPUTE     K140TMP-KAISU(56)    =
      ***                            K140TMP-KAISU(11)
      ***                      +     K140TMP-KAISU(14)
      ***                      +     K140TMP-KAISU(22)
      ***                      +     K140TMP-KAISU(26)
      ***                      +     K140TMP-KAISU(27)
      ***                      +     K140TMP-KAISU(31)
      ***                      +     K140TMP-KAISU(35)
      ***                      +     K140TMP-KAISU(36)
      ***                      +     K140TMP-KAISU(40)
      ***                      +     K140TMP-KAISU(44)
      *
                   WHEN 55
                       COMPUTE     K140TMP-TENSU(55)    =
                                   K140TMP-TENSU(43)
                             +     K140TMP-TENSU(44)
                             +     K140TMP-TENSU(45)
                             +     K140TMP-TENSU(46)
                             +     K140TMP-TENSU(47)
                             +     K140TMP-TENSU(48)
                             +     K140TMP-TENSU(49)
                             +     K140TMP-TENSU(50)
                             +     K140TMP-TENSU(51)
                             +     K140TMP-TENSU(52)
      *=== 自費10項目修正 Satoh (2007/06/28) START
                             +     K140TMP-TENSU(53)
      *--- 2009/12/14 DELETE START -------------------------------------
      *                       +     K140TMP-TENSU(54)
      *--- 2009/12/14 DELETE END ---------------------------------------
      *=== 自費10項目修正 Satoh (2007/06/28) END
                       COMPUTE     K140TMP-KAISU(55)    =
                                   K140TMP-KAISU(43)
                             +     K140TMP-KAISU(44)
                             +     K140TMP-KAISU(45)
                             +     K140TMP-KAISU(46)
                             +     K140TMP-KAISU(47)
                             +     K140TMP-KAISU(48)
                             +     K140TMP-KAISU(49)
                             +     K140TMP-KAISU(50)
                             +     K140TMP-KAISU(51)
                             +     K140TMP-KAISU(52)
      *=== 自費10項目修正 Satoh (2007/06/28) START
                             +     K140TMP-KAISU(53)
      *--- 2009/12/14 DELETE START -------------------------------------
      *                       +     K140TMP-TENSU(54)
      *--- 2009/12/14 DELETE END ---------------------------------------
      *=== 自費10項目修正 Satoh (2007/06/28) END
                   WHEN 56
                       COMPUTE     K140TMP-TENSU(56)    =
                                   K140TMP-TENSU(11)
                             +     K140TMP-TENSU(14)
                             +     K140TMP-TENSU(22)
                             +     K140TMP-TENSU(26)
                             +     K140TMP-TENSU(27)
                             +     K140TMP-TENSU(31)
                             +     K140TMP-TENSU(35)
                             +     K140TMP-TENSU(36)
                             +     K140TMP-TENSU(40)
                             +     K140TMP-TENSU(42)
                       COMPUTE     K140TMP-KAISU(56)    =
                                   K140TMP-KAISU(11)
                             +     K140TMP-KAISU(14)
                             +     K140TMP-KAISU(22)
                             +     K140TMP-KAISU(26)
                             +     K140TMP-KAISU(27)
                             +     K140TMP-KAISU(31)
                             +     K140TMP-KAISU(35)
                             +     K140TMP-KAISU(36)
                             +     K140TMP-KAISU(40)
                             +     K140TMP-KAISU(42)
      ***  20040707  ここまで
               END-EVALUATE
               MOVE     K140TMP-TENSU(IDX1) TO  WRK-TENSU-HEN
               MOVE     WRK-TENSU-HEN       TO  K100-TENSU(IDX2 IDX1)
      *     
               MOVE     K140TMP-KAISU(IDX1) TO  WRK-KAISU-HEN
               MOVE     WRK-KAISU-HEN       TO  K100-KAISU(IDX2 IDX1)
      *
      *      全診療科加算エリアに加算する。
      *
               COMPUTE  WRK-K140ALL-TENSU(IDX1) =
                        WRK-K140ALL-TENSU(IDX1)
                    +   K140TMP-TENSU(IDX1) 
               COMPUTE  WRK-K140ALL-KAISU(IDX1) =
                        WRK-K140ALL-KAISU(IDX1)
                    +   K140TMP-KAISU(IDX1)
      *
           END-PERFORM
      *     
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<全診療科明細>処理
      *****************************************************************
       320-BODY-HEN2-SEC               SECTION.
      *
           MOVE       "全診療科"          TO    K100-TNAME(IDX2)
      *
           PERFORM    VARYING     IDX1    FROM  1  BY  1
                      UNTIL       IDX1    >     56
      *
               MOVE     WRK-K140ALL-TENSU(IDX1)
                                            TO  WRK-TENSU-HEN
               MOVE     WRK-TENSU-HEN       TO  K100-TENSU(IDX2 IDX1)
      *
               MOVE     WRK-K140ALL-KAISU(IDX1)
                                            TO  WRK-KAISU-HEN
               MOVE     WRK-KAISU-HEN       TO  K100-KAISU(IDX2 IDX1)
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
      ***  20040915 MOTOHASI MONPEに変更
      ***     MOVE   "A00000K100"             TO  SPRT-PRTID
      *--- 2009/12/14 CHANGE START -------------------------------------
      *     MOVE   "A00000K100.red"             TO  SPRT-PRTID
           MOVE   "A00000K100V02.red"      TO  SPRT-PRTID
      *--- 2009/12/14 CHANGE END ---------------------------------------
      ***  20040915 MOTOHASI MONPEに変更
           MOVE    "(診療科別)診療行為区分別集計表"
                                           TO  SPRT-TITLE
           MOVE    K100                    TO  SPRT-PRTDATA
           MOVE    K100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    K100LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    K100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***** site-libで動かすためにコメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      *****
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      ***07/09/18 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *     MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "key10"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *         MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *         MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "key10"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                            MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                      ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                            MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                            SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *     MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "key10"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *     MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "key10"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *         MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *         MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 start
                                            MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                      ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                            MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                            SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *     MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC           =   ZERO
               PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
      *=== sarge対応 DEL By Takashima(2006/09/17) start
      *         PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/17) end
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-1005-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
      *=== sarge対応 DEL By Takashima(2006/09/17) start
      *         PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/17) end
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE  "DBCLOSE"                TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC           =   ZERO
               PERFORM 821-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
      *=== sarge対応 DEL By Takashima(2006/09/17) start
      *         PERFORM 821-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/17) end
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-1010-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
      *=== sarge対応 DEL By Takashima(2006/09/17) start
      *         PERFORM 821-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/17) end
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE  "DBCLOSE"                TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
      *
      *        ジョブ終了処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
      ***07/09/18 nagaya 4.0対応 start
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
               MOVE    K100LNK-JOBID   TO  JOB-JOBID
               MOVE    K100LNK-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      ***07/09/18 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
           READ    K140T-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-M01TMP
               NOT INVALID  KEY
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
           READ  K140T-FILE   NEXT
               AT  END
                   MOVE  1   TO  FLG-PRINT
               NOT AT END
                   ADD   1   TO  CNT-K140TMP
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    K100LNK-HOSPID      TO  PTINF-HOSPID
           MOVE    K100LNK-HOSPNUM     TO  PTINF-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE    WRK-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *         MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                            MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                      ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                            MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                            SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
               IF      MCP-RC              =   ZERO
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SRY-SRYCD(IDZ)      TO  TNS-SRYCD
           MOVE    ZERO                TO  TNS-YUKOSTYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "TENSU-KEY21"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC              =   ZERO
               PERFORM 900-TENSU-READ-CHK-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "TENSU-KEY21"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *         MOVE    "TENSU-KEY21"       TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key21"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                            MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                      ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                            MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                            SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
               IF      MCP-RC               =   ZERO
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
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SRY-SRYCD(IDZ)      TO  TNS-SRYCD
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL 9               TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *         MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                            MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                      ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                            MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                            SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
               IF      MCP-RC              =   ZERO
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "TENSU-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "KENSASORT-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_kensasort"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *         MOVE    "KENSASORT-KEY"     TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_kensasort"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                            MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                      ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                            MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                            SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
               IF      MCP-RC              =   ZERO
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "KENSASORT-KEY"     TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_kensasort"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    ACCT-HOSPID          TO  SRY-HOSPID
           MOVE    ACCT-HOSPNUM         TO  SRY-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE    ACCT-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO  SRY-PTID
           MOVE    ACCT-SRYKA           TO  SRY-SRYKA
           MOVE    ACCT-SRYYM           TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO  SRY-ZAINUM
           MOVE    SRYACT-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SRYACT-KEY2"        TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC               =   ZERO
               MOVE  ZERO               TO FLG-SRYACT
               PERFORM   900-SRYACT-FETCH-SEC
           ELSE
               MOVE   1                 TO  FLG-SRYACT
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SRYACT-KEY2"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SRYACT-KEY2"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報SELECT
      *****************************************************************
       900-SRYACCT-SELECT-SEC         SECTION.
      *
           INITIALIZE                   SRYACCT-REC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    K100LNK-HOSPID       TO  ACCT-HOSPID
           MOVE    K100LNK-HOSPNUM      TO  ACCT-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE    K100LNK-SRYYM        TO  ACCT-SRYYM
           MOVE    WRK-NYUGAIKBN        TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SRYACCT-KEY8"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC               =   ZERO
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SRYACCT-KEY8"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_sryacct"      TO  MCP-TABLE
           MOVE    "key8"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
      *    テスト患者は対象外
               MOVE    ACCT-PTID    TO  WRK-PTID  
               PERFORM  900-PTINF-READ-SEC
               IF      FLG-PTINF             =   ZERO
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SRYACCT-KEY8"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           .
       900-SRYACCT-CLOSE-EXT.
           EXIT.
      * 20080930 入院のみの患者が集計されない不具合を修正START
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
           IF    MCP-RC               =   ZERO
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
           IF    MCP-RC              =   ZERO
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
      * 20080930 入院のみの患者が集計されない不具合を修正END
      *****************************************************************
      *    収納情報SELECT
      *****************************************************************
       940-SYUNOU-SELECT-SEC         SECTION.
      *
           INITIALIZE                  SYUNOU-REC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    K100LNK-HOSPID       TO  SYU-HOSPID
           MOVE    K100LNK-HOSPNUM       TO  SYU-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE    "1"                  TO  SYU-NYUGAIKBN
      * 20080930 入院のみの患者が集計されない不具合を修正START
           MOVE    NACCT-PTID            TO  SYU-PTID
      *     MOVE    WRK-DENPJTIKBN       TO  SYU-DENPJTIKBN 
      * 20080930 入院のみの患者が集計されない不具合を修正END
           MOVE    WRK-PARA-PRTSYMD     TO  SYU-SKYSTYMD
           MOVE    WRK-PARA-PRTLYMD     TO  SYU-SKYEDYMD
      ****2006/04/11 @@@@-BRAND START
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
      *     IF  WRK-DENPJTIKBN  = 3
      *         MOVE  " "   TO  SYU-DENPJTIKBN
      *     END-IF
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
      ****2006/04/11 @@@@-BRAND END
           MOVE    SYUNOU-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYUNOU-KEY7"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syunou"         TO  MCP-TABLE
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
      *     MOVE    "key7"               TO  MCP-PATHNAME
           MOVE    "key5"               TO  MCP-PATHNAME
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC           =   ZERO
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYUNOU-KEY7"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syunou"         TO  MCP-TABLE
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
      *     MOVE    "key7"               TO  MCP-PATHNAME
           MOVE    "key5"               TO  MCP-PATHNAME
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC              =   ZERO
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"            TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08)end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYUNOU-KEY7"        TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syunou"         TO  MCP-TABLE
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
      *     MOVE    "key7"               TO  MCP-PATHNAME
           MOVE    "key5"               TO  MCP-PATHNAME
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           .
       940-SYUNOU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せ情報セット
      *****************************************************************
       930-HKNCOMBI-SET-SEC  SECTION.
      *
           INITIALIZE  HKNCOMBI-REC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE  ACCT-HOSPID          TO  COMB-HOSPID
           MOVE  ACCT-HOSPNUM         TO  COMB-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE  ACCT-PTID            TO  COMB-PTID
           MOVE  ACCT-HKNCOMBI        TO  COMB-HKNCOMBINUM
           MOVE  HKNCOMBI-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE  "HKNCOMBI-KEY"      TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE    "tbl_hkncombi"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/18 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                      MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF  MCP-RC  =  ZERO
      ***07/09/18 nagaya 4.0対応 start
      *       MOVE  "HKNCOMBI-KEY"  TO  ORC-DBPATH
      ***07/09/18 nagaya 4.0対応 end
               MOVE  "DBFETCH"       TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *       CALL  "ORCMCPSUB"  USING
               MOVE  "tbl_hkncombi"  TO  MCP-TABLE
               MOVE  "key"           TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"     USING
      ***07/09/18 nagaya 4.0対応 end
                                     MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                          ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                     MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                     SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE  "HKNCOMBI-KEY"  TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE  "tbl_hkncombi"   TO  MCP-TABLE
           MOVE  "key"            TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"      USING
      ***07/09/18 nagaya 4.0対応 end
                                  MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                  MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                  SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE  ACCT-HOSPID          TO  HKN-HOSPID
           MOVE  ACCT-HOSPNUM         TO  HKN-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE  WRK-HKNNUM           TO  HKN-HKNNUM
           MOVE  "00"                 TO  HKN-PAYKBN
           MOVE  ALL 9                TO  HKN-TEKSTYMD
           MOVE  ZERO                 TO  HKN-TEKEDYMD
           MOVE  HKNNUM-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE  "HKNNUM-KEY"      TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE  "tbl_hknnum"         TO  MCP-TABLE
           MOVE  "key"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                      MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF  MCP-RC  =  ZERO
      ***07/09/18 nagaya 4.0対応 start
      *       MOVE  "HKNNUM-KEY"  TO  ORC-DBPATH
      ***07/09/18 nagaya 4.0対応 end
               MOVE  "DBFETCH"  TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *       CALL  "ORCMCPSUB"  USING
               MOVE  "tbl_hknnum"   TO  MCP-TABLE
               MOVE  "key"          TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"    USING
      ***07/09/18 nagaya 4.0対応 end
                                MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                          ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE  "HKNNUM-KEY"  TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE  "tbl_hknnum"     TO  MCP-TABLE
           MOVE  "key"            TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"      USING
      ***07/09/18 nagaya 4.0対応 end
                                  MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                  MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                  SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    SYU-HOSPID           TO  PTNYUINRRK-HOSPID
           MOVE    SYU-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE    SYU-PTID             TO  PTNYUINRRK-PTID
           MOVE    SYU-SKYSTYMD     TO  PTNYUINRRK-NYUINYMD
           MOVE    SYU-SKYEDYMD     TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "PTNYUINRRK-KEY11"     TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC           =   ZERO
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
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "PTNYUINRRK-KEY11"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC              =   ZERO
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"            TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "PTNYUINRRK-KEY11"     TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           .
       930-PTNYUINRRK-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理SELECT(室料差額用)
      *****************************************************************
       900-SYS-SELECT-SEC         SECTION.
      *
           MOVE    SYS-5113-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY5"   TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
           MOVE    "DBFETCH"            TO  MCP-FUNC
      ***07/09/18 nagaya 4.0対応 start
      *         MOVE    "SYSKANRI-KEY5"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                            MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                      ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                            MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                            SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           IF      MCP-RC              =   ZERO
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
      *=== sarge対応 UP By Takashima(2006/08/08) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/08) end
      ***07/09/18 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY5" TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                                  ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
                                        MCPDATA-REC
      ***07/09/18 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
           .
       900-SYS-CLOSE-EXT.
           EXIT.
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正START
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
           IF  MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC2
               MOVE    ZERO            TO  FLG-SRYACCT2
      *    テスト患者は対象外
               MOVE    ACCT2-PTID    TO  WRK-PTID  
               PERFORM  900-PTINF-READ-SEC
               IF      FLG-PTINF             =   ZERO
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
      ****2006/03/09 @@@@-BRAND  START
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
      * 20080825 診療行為画面から入院料を入力すると重複してしまう不具合を修正END
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      ***07/09/18 nagaya 4.0対応 start
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      ***07/09/18 nagaya 4.0対応 end
           MOVE    K100LNK-JOBID   TO  JOB-JOBID
           MOVE    K100LNK-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT                        
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      ***07/09/18 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
      ***07/09/18 nagaya 4.0対応 start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      ***07/09/18 nagaya 4.0対応 end
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
      ***07/09/18 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
      ***07/09/18 nagaya 4.0対応 start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      ***07/09/18 nagaya 4.0対応 end
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
      ***07/09/18 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
      ***07/09/18 nagaya 4.0対応 start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      ***07/09/18 nagaya 4.0対応 end
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      ***07/09/18 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/18 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/18 nagaya 4.0対応 start
      *                            ORCMCPAREA
      ***07/09/18 nagaya 4.0対応 end
      ***07/09/18 nagaya 4.0対応 start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      ***07/09/18 nagaya 4.0対応 end
           .
       900-DBCLOSE-EXT.
           EXIT.
