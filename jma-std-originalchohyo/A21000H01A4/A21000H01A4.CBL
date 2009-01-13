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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A21000H01A4.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : カルテ（診療録）印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/03/31    @@@@−有村　  新規作成(ORCHC01 V1.0.6カスタマイズ)
      *  03/10/07    @@@@-本橋     実行ファイル読み込み先ディレクトリ変更
      *  04/09/09    @@@@-本橋     MONPE化
      *  04/09/09    @@@@-本橋     国保の場合、被保険者→世帯主の印字へ変更
      *  04/11/26    @@@@-本橋     傷病名全印字/継続印字の対応
      *  05/01/05    @@@@-本橋     ３歳未満の負担割合修正
      *  05/01/17    @@@@-本橋     患者名の１１文字以上時の折返し差込みを追加
      *  05/09/15    @@@@-本橋     傷病名未印字の対応
      *  06/08/09    @@@@-内田     sarge対応
      *  07/02/21    @@@@-内田     出力先プリンタサブ追加
      *  07/09/21    @@@@-大竹     Ver4.0対応
      *  08/02/06    @@@@-田中     続紙に病名が１件しか表示されない不具合を修正
      *                            転帰した病名が表示されてしまう不具合を修正
      *  08/03/07    @@@@-田中     公害保険対応
      *  08/03/14    @@@@-田中     H20年4月改正対応
      *  08/04/04    @@@@-田中     義務教育就学前の負担割合を修正
      *  08/06/03    @@@@-田中     公費を３つ使うとﾌﾘｶﾞﾅが印字されない不具合を修正
      *  08/06/24    @@@@-田中     後期高齢者対応
      *  08/08/14    @@@@-門間     記号・番号の１５、１６文字目が印字されない不具合を修正
      *  08/09/30    @@@@-田中     ｼｽﾃﾑ管理ﾏｽﾀ1017の情報を取得できない不具合を修正
      *  08/10/28    @@@@-門間     ver4.3対応(外来カルテ一括発行)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/04/10  保険者番号の編集
      *  01.00.02    MCC-藤原     01/04/11  病名・公費のテーブル化
      *  01.00.03    MCC-藤原     01/04/19  パラメタの変更・公費の編集  
      *  01.00.04    MCC-藤原     01/05/20  住所編集の変更
      *  01.00.05    MCC-森脇     01/09/10  続紙の処理追加
      *  01.00.06    MCC-藤原     01/10/03  出力先プリンタ名編集
      *  01.00.07    MCC-多々納   01/12/11  カルテ病名追加
      *  01.00.08    MCC-藤原     02/03/18  帳票ファイル名の変更
      *  01.00.09    MCC-多々納   02/03/27  保険組合せなし追加
      *  01.00.10    MCC-多々納   02/05/07  全角変換の追加
      *  01.00.11    MCC-多々納   02/05/23  労災の追加
      *  01.00.12    NACL-多々納  02/09/13  自賠責・印刷サプの追加
      *  01.00.13    NACL-多々納  02/11/01  受給者番号の全角８文字以上対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    国保印刷用データ
           SELECT  PRT-FILE       ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *    健保印刷用データ
           SELECT  PRT-FILE2      ASSIGN  HC01PARA2
                                   FILE    STATUS  IS  STS-PARA2.
      *---- (01.00.05) LINE ADD START ----------------------------       
           SELECT  PRTX-FILE      ASSIGN  HC011PARA
                                   FILE    STATUS  IS  STSX-PARA.
      *---- (01.00.05) LINE ADD END   ----------------------------
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    国保印刷用データ
       FD  PRT-FILE.
           COPY    "A21000H01KA4.INC".
      *
      *    健保印刷用データ
       FD  PRT-FILE2.
           COPY    "A21000H01SA4.INC".
      *---- (01.00.05) LINE ADD START ----------------------------
      *    印刷用データ続紙
       FD  PRTX-FILE.
           COPY    "HC011.INC".
      *---- (01.00.05) LINE ADD END   ----------------------------
      *
       WORKING-STORAGE             SECTION.
      *     国保印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *     健保印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC"
                                     REPLACING  //HC01PARA//
                                     BY         //HC01PARA2//.
      *---- (01.00.05) LINE ADD START ----------------------------
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT1.INC".
      *---- (01.00.05) LINE ADD END   ----------------------------
      *    シェル用領域
      *    COPY    "CPCOMMONSHELL.INC".
      *
      *    シェル用領域
      *    COPY    "CPSHELLTBL.INC".
      **** COPY    "ORCA-DBPATH".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-PARA2            PIC X(02).
       01  STSX-AREA.
           03  STSX-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
      *---- (01.00.01) LINE ADD START ----------------------------
           03  FLG-OK              PIC 9(01).
           03  FLG-ZOKUSHI         PIC 9(01).
      *---- (01.00.01) LINE ADD END   ----------------------------
           03  FLG-PTRSIINF        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *****03  SYS-TIME.
      *        05  SYS-THH         PIC 9(02).
      *        05  SYS-TMM         PIC 9(02).
      *        05  SYS-TSS         PIC 9(02).
      *********05  SYS-TDD         PIC 9(02).
      *    システム領域
      *01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
      *---- (01.00.01) LINE ADD START ----------------------------
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
      *---- (01.00.01) LINE ADD END   ----------------------------  
           03  IDX2-1              PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *
           03  WRK-WSYSYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-PAGE            PIC 9(03).
      *
           03  WRK-ADRS          PIC X(200).
           03  WRK-NENREI-X.
               05  WRK-NENREI        PIC 9(03).
      *
      *---- (01.00.01) LINE ADD START ----------------------------                          
           03  WRK-HKNJA           PIC X(08).
           03  WRK-HKNJA-G         PIC X(16).
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)   OCCURS   10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
      *---- (01.00.11) LINE ADD START ----------------------------
           03  WRK-HEN-KIGONUM         PIC X(80).
      *---- (01.00.11) LINE ADD END   ----------------------------
           03  WRK-KIGONUM             PIC X(80).
           03  WRK-NUM                 PIC X(80).
      *
           03  WRK-WARI            PIC 9(02)V9.
           03  WRK-Z6              PIC ZZZZ.
           03  WRK-Z62             PIC Z9.9.
           03  WRK-Z3              PIC ZZ9.
      *** 20030516追加 
           03  WRK-Z9V9            PIC Z9.9.
           03  WRK-Z9              PIC Z9.
      *** 20030516追加 
      *
           03  WRK-HKNCOMBI        PIC X(40).
      *---- (01.00.01) LINE ADD END   ----------------------------                          
           03  WRK-HPTNUM          PIC X(08). 
           03  WRK-FTNSNO          PIC X(16).
      *****03  WRK-JKYSNO          PIC X(16).
           03  WRK-JKYSNO          PIC X(20).
           03  WRK-TEKSTYMD        PIC X(22).
           03  WRK-TEKEDYMD        PIC X(22).
      *
           03  WRK-STRYMD.
               05  WRK-STRGO       PIC X(04).
               05  WRK-STRYY       PIC X(04).
               05  WRK-STR1        PIC X(02).
               05  WRK-STRMM       PIC X(04).
               05  WRK-STR2        PIC X(02).
               05  WRK-STRDD       PIC X(04).
               05  WRK-STR3        PIC X(02).
           03  WRK-ENDYMD.
               05  WRK-ENDGO       PIC X(04).
               05  WRK-ENDYY       PIC X(04).
               05  WRK-END1        PIC X(02).
               05  WRK-ENDMM       PIC X(04).
               05  WRK-END2        PIC X(02).
               05  WRK-ENDDD       PIC X(04).
               05  WRK-END3        PIC X(02).
      *
           03  WRK-STRYMDW.
               05  WRK-STRGOW      PIC X(01).
               05  WRK-STRYYW      PIC X(02).
               05  WRK-STR1W       PIC X(01).
               05  WRK-STRMMW      PIC X(02).
               05  WRK-STR2W       PIC X(01).
               05  WRK-STRDDW      PIC X(02).
           03  WRK-ENDYMDW.
               05  WRK-ENDGOW      PIC X(01).
               05  WRK-ENDYYW      PIC X(02).
               05  WRK-END1W       PIC X(01).
               05  WRK-ENDMMW      PIC X(02).
               05  WRK-END2W       PIC X(01).
               05  WRK-ENDDDW      PIC X(02).
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-MSG             PIC X(200).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC X(02).
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
           03  WRK-SPA-SYSYYYY.
               05  WRK-SPA-SYSYY   PIC  9(04).
           03  WRK-POSTG.
               05  WRK-POST1       PIC  X(03).
               05  FILLER          PIC  X(01)  VALUE  "-".
               05  WRK-POST2       PIC  X(04).
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
       01  DUMMY PIC X(03).
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *    患者病名マスタ−
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    患者登録機能制御情報
           COPY  "CPSK1017.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY  "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY  "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *=== 出力先プリンタサブ追加 By Uchida(2007/02/21) start
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *=== 出力先プリンタサブ追加 By Uchida(2007/02/21) end
      *
      *---- (2008/10/28) LINE ADD START MONMA --------------------------
      *    一括用
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *---- (2008/10/28) LINE ADD ENC   MONMA --------------------------
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *=== 4.0対応 By Otake(2007/09/21) start
      *     COPY    "CPORCMCP.INC".
      *=== 4.0対応 By Otake(2007/09/21) end
      *
      * 20080404 義務教育就学前の負担割合を修正START tanaka
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      * 20080404 義務教育就学前の負担割合を修正END
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
      *
      *---- (2008/10/28) LINE ADD START MONMA --------------------------
      *    カルテ出力パラメタ
           COPY    "CPORCHC01.INC".
      *---- (2008/10/28) LINE ADD END   MONMA --------------------------
      *
      *---- (01.00.03) LINE DEL START ----------------------------                          
      *01   HC01LNK-PTNUM              PIC X(20).
      *01   HC01LNK-PTID               PIC 9(10).
      *01   HC01LNK-COMBINUM           PIC X(04).
      *01   HC01LNK-SRYKA              PIC X(17).
      *---- (01.00.03) LINE DEL END ------------------------------                          
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
      *
      *---- (2008/10/28) LINE ADD START MONMA --------------------------
           ORCHC01AREA
      *---- (2008/10/28) LINE ADD END   MONMA --------------------------
      *
      *---- (01.00.03) LINE DEL START ----------------------------                          
      **   HC01LNK-PTNUM
      **   HC01LNK-PTID
      **   HC01LNK-COMBINUM
      **   HC01LNK-SRYKA
      *---- (01.00.03) LINE DEL END ------------------------------
           .                          
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END  =   1
      *
           PERFORM 300-END-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCHC01    START  *** = "  SPA-TERMID
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  H01KA4
           INITIALIZE                  H01SA4
           MOVE    ZERO                TO  WRK-PAGE
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    ファイルＯＰＥＮ
      *=== 4.0対応 By Otake(2007/09/21) start
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    SPA-HOSPNUM         TO  HC01PARA2-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    "H01SA4"            TO  HC01PARA2-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
                                           HC01PARA2-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
                                           HC01PARA2-SYOHMS
      *    変更★★★★★★
      *****MOVE    "001"               TO  HC01PARA-CNT
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
                                           HC01PARA2-CNT
           OPEN    OUTPUT               PRT-FILE
                                        PRT-FILE2
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *=== 出力先プリンタサブ追加 By Uchida(2007/02/21) start
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
      *
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    1                   TO  FLG-END
           END-IF 
      *
      *    出力先プリンタ名割り当て情報編集
      *     MOVE    SPACE               TO  SYS-1031-REC
      *     INITIALIZE                  SYS-1031-REC
      *     MOVE    "1031"              TO  SYS-1031-KANRICD
      *     MOVE    "*"                 TO  SYS-1031-KBNCD
      *     MOVE    SYS-1031-REC        TO  MCPDATA-REC
      *     PERFORM 800-SYSKANRI-READ-SEC
      *     IF      FLG-SYSKANRI        =   ZERO
      *         MOVE    MCPDATA-REC         TO  SYS-1031-REC
      *     ELSE
      ***      MOVE    "出力先プリンタ名割り当て情報が取得できませんでした。"
      ***                                  TO  WRK-RECEERR
      ***      PERFORM 500-ERR-HENSYU-SEC
      *         MOVE    1                   TO  FLG-END
      *     END-IF 
      *=== 出力先プリンタサブ追加 By Uchida(2007/02/21) end
      *
      ***   20041126 MOTOHASI 
      *    患者登録機能制御情報
           MOVE    SPACE               TO  SYS-1017-REC
           INITIALIZE                  SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
      *=== 4.0対応 By Otake(2007/09/21) start
      *20080930 ｼｽﾃﾑ管理ﾏｽﾀ1017の情報を取得できない不具合を修正START
      *     MOVE    SPA-SRYYMD          TO  SYS-1017-STYUKYMD
      *     MOVE    SPA-SRYYMD          TO  SYS-1017-EDYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1017-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1017-EDYUKYMD
      *20080930 ｼｽﾃﾑ管理ﾏｽﾀ1017の情報を取得できない不具合を修正START
           MOVE    SPA-HOSPNUM         TO  SYS-1017-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1017-REC
      *--- (2008/10/28) LINE ADD START MONMA ---------------------------
               IF   ORCHC01-SYORIKBN  =   "3"
                 MOVE  ORCHC01-BYOMEIFLG  TO  SYS-1017-KARUTE-BYOMEI
               END-IF
      *--- (2008/10/28) LINE ADD END   MONMA ---------------------------
           ELSE
               INITIALIZE                  SYS-1017-REC
           END-IF
      ***   20041126 MOTOHASI 
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *　　印刷項目編集
            PERFORM 2001-SYORI-SEC
      *　　印刷用データ出力
      *
            WRITE  H01KA4
            WRITE  H01SA4
      *
      ***** MOVE   "HC01.sh"           TO  SHELL-ID
      *     MOVE   "HC01.dia"          TO  SHELL-DIA
      *     MOVE   "HC01.def"          TO  SHELL-DEF
      *     MOVE   HC01PARA            TO  SHELL-DAT
      ***** MOVE   SYS-1031-PRTNM(1)    TO  SHELL-PRTNM
            CLOSE                       PRT-FILE
                                        PRT-FILE2
      *
      *---- (2008/10/28) LINE ADD START MONMA --------------------------
           IF ORCHC01-SYORIKBN   NOT = "3"
      *---- (2008/10/28) LINE ADD END   MONMA --------------------------
      *
      *     国保・健保印刷判別
             EVALUATE   TRUE
             WHEN  PTHKN-HKNNUM   = "060"  OR   "067"
      * 20080624 後期高齢者対応START
                                   OR  "039" OR  "040"
      * 20080624 後期高齢者対応END
               INITIALIZE                 ORCSMKPRTAREA
               MOVE   "HC01.sh"           TO  MKPRT-ID
      ***   20040909 MOTOHASI MONPEに変更
      ***          MOVE   "A21000H01KA4.dia"          TO  MKPRT-DIA
      ***          MOVE   "A21000H01KA4.def"          TO  MKPRT-DEF
               MOVE   "A21000H01KA4.red"          TO  MKPRT-DIA
               MOVE   SPACE                       TO  MKPRT-DEF
      ***   20040909 MOTOHASI MONPEに変更
               MOVE   HC01PARA            TO  MKPRT-DAT
               MOVE   SYS-1031-PRTNM(1)   TO  MKPRT-PRTNM
      ***   20031007  変更 @@@@-MOTOHASI
      ***      CALL   "ORCSMKPRT"         USING
      ***                                 ORCSMKPRTAREA
               CALL   "ORCSMKPRTSITE"      USING
                                           ORCSMKPRTAREA
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
             WHEN   OTHER
               INITIALIZE                 ORCSMKPRTAREA
               MOVE   "HC01.sh"           TO  MKPRT-ID
      ***   20040909 MOTOHASI MONPEに変更
      ***          MOVE   "A21000H01SA4.dia"          TO  MKPRT-DIA
      ***          MOVE   "A21000H01SA4.def"          TO  MKPRT-DEF
               MOVE   "A21000H01SA4.red"          TO  MKPRT-DIA
               MOVE   SPACE                       TO  MKPRT-DEF
      ***   20040909 MOTOHASI MONPEに変更
               MOVE   HC01PARA2            TO  MKPRT-DAT
      *          MOVE   HC01PARA            TO  MKPRT-DAT
               MOVE   SYS-1031-PRTNM(1)   TO  MKPRT-PRTNM
      ***   20031007  変更 @@@@-MOTOHASI
      ***      CALL   "ORCSMKPRT"         USING
      ***                                 ORCSMKPRTAREA
              CALL   "ORCSMKPRTSITE"      USING
                                           ORCSMKPRTAREA
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
             END-EVALUATE
      *
      *---- (2008/10/28) LINE ADD START MONMA --------------------------
           ELSE
      *
      *     国保・健保印刷判別
             EVALUATE   TRUE
               WHEN  PTHKN-HKNNUM   = "060"  OR   "067"
                                       OR  "039" OR  "040"
      *
               MOVE    ORCHC01-PRTAREA     TO  ORCSPRTAREA
      *
               MOVE    H01KA4              TO  SPRT-PRTDATA
               MOVE    "2"                 TO  SPRT-SITEKBN
               MOVE    "A21000H01KA4.red"         TO  SPRT-PRTID
               MOVE    "診療録（カルテ１号紙）（外来）一括"
                                           TO  SPRT-TITLE
               CALL    "ORCSPRT"           USING
                                           ORCSPRTAREA
                                           SPA-AREA
               IF      SPRT-RETURN         =   ZERO
                       ADD     1               TO  ORCHC01-CNT
               ELSE
                   MOVE    9               TO  ORCHC01-RC
               END-IF
             WHEN   OTHER
      *
               MOVE    ORCHC01-PRTAREA     TO  ORCSPRTAREA
      *
               MOVE    H01SA4              TO  SPRT-PRTDATA
               MOVE    "2"                 TO  SPRT-SITEKBN
               MOVE    "A21000H01SA4.red"         TO  SPRT-PRTID
               MOVE    "診療録（カルテ１号紙）（外来）一括"
                                           TO  SPRT-TITLE
               CALL    "ORCSPRT"           USING
                                           ORCSPRTAREA
                                           SPA-AREA
               IF      SPRT-RETURN         =   ZERO
                   ADD     1               TO  ORCHC01-CNT
               ELSE
                   MOVE    9               TO  ORCHC01-RC
               END-IF
             END-EVALUATE
           END-IF
      *---- (2008/10/28) LINE ADD END   MONMA --------------------------
      *
      *   印刷用スクリプト処理
      *★    ETC-EXEC SHELL   SHELL-AREA 10
      *    MOVE    SHELL-NAME          TO  SHELLTBL-NAME
      *    MOVE    SHELL-ARG1          TO  SHELLTBL-ARG1
      *    MOVE    SHELL-ARG2          TO  SHELLTBL-ARG2
      *    MOVE    SHELL-ARG3          TO  SHELLTBL-ARG3
      *    MOVE    SHELL-ARG4          TO  SHELLTBL-ARG4
      *    MOVE    SHELL-ARG5          TO  SHELLTBL-ARG5
      *    MOVE    SHELLTBL            TO  MCPDATA-REC
      *    MOVE    "SHELL"             TO  MCP-FUNC
      *    MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
      *    CALL    "MCPSUB"            USING
      *            MCPAREA
      ******       MCPDATA-REC
      *
      *---- (01.00.05) LINE MOD START ----------------------------
      *    続紙あり
           IF      FLG-ZOKUSHI         =   1  
              AND  FLG-PTBYOMEI        =   ZERO
               PERFORM 2002-ZOKUSHI-SEC
               CLOSE                       PRT-FILE
                                           PRT-FILE2
           END-IF
      *---- (01.00.05) LINE MOD END ------------------------------
      *
      *--- (2008/10/28) LINE ADD START MONMA ---------------------------
               MOVE    "DBCLOSECURSOR"     TO      MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *--- (2008/10/28) LINE ADD END   MONMA ---------------------------
      *
           MOVE    1                   TO  FLG-END 
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       2001-SYORI-SEC                SECTION.
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    SPA-HOSPID        TO  PTINF-HOSPID
           MOVE    SPA-HOSPNUM       TO  PTINF-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
      *---- (01.00.03) LINE MOD START ---------------------------- 
      **   MOVE    HC01LNK-PTID      TO  PTINF-PTID
           MOVE    SPA-PTID          TO  PTINF-PTID
      *---- (01.00.03) LINE MOD END ------------------------------ 
           PERFORM 900-PTINF-READ-SEC
      *
           IF      FLG-PTINF           =   1
               DISPLAY "*** PTINF  INVALIE:"   PTINF-KEY
               GO      TO      2001-SYORI-EXT
           END-IF
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
      *---- (01.00.03) LINE MOD START ----------------------------
      **   MOVE    HC01LNK-PTID        TO  COMB-PTID
      **   MOVE    HC01LNK-COMBINUM    TO  COMB-HKNCOMBINUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SPA-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
      *---- (01.00.03) LINE MOD END ------------------------------
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
      ********* DISPLAY "*** HKNCOMBI  エラー"
      *********GO      TO      2001-SYORI-EXT
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
      *    患者保険情報検索
           INITIALIZE                      PTHKNINF-REC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    SPA-HOSPID          TO  PTHKN-HOSPID
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
      *---- (01.00.03) LINE MOD START ---------------------------- 
      **   MOVE    HC01LNK-PTID        TO  PTHKN-PTID
           MOVE    SPA-PTID            TO  PTHKN-PTID
      *---- (01.00.03) LINE MOD END ------------------------------
      *    主保険−ＩＤ
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           PERFORM 920-PTHKNINF-READ-SEC
           IF      FLG-PTHKNINF      NOT =   ZERO
               INITIALIZE              PTHKNINF-REC
           END-IF
      *
      *---- (01.00.01) LINE ADD START ---------------------------- 
      *    保険者番号変換処理
           MOVE    SPACE               TO  WRK-HKNJA
           MOVE    9                   TO  IDY
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    8   BY  -1
                   UNTIL       IDX     <   1
               IF      PTHKN-HKNJANUM(IDX:1)   NOT =   SPACE
                   MOVE    1               TO  FLG-OK
               END-IF
               IF      FLG-OK              =   1
                   COMPUTE IDY         =   IDY     -   1
                   MOVE    PTHKN-HKNJANUM(IDX:1)   TO  WRK-HKNJA (IDY:1)
               END-IF
           END-PERFORM
      **** MOVE    WRK-HKNJA           TO  WRK-MOJI
      *    MOVE    8                   TO  WRK-MOJISU
      *    PERFORM 2009-HENKAN-SEC
      *****MOVE    WRK-ZENKAKU-G(1:16) TO  WRK-HKNJA-G
           MOVE    16                  TO  WRK-MOJISU
           MOVE    WRK-HKNJA           TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:16) TO  WRK-HKNJA-G
      *---- (01.00.01) LINE ADD END ------------------------------- 
      *
      *    保険番号マスター検索
           INITIALIZE                  HKNNUM-REC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    SPA-HOSPID    TO  HKN-HOSPID
           MOVE    SPA-HOSPNUM   TO  HKN-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
           MOVE    COMB-HKNNUM         TO  HKN-HKNNUM
           MOVE    COMB-TEKSTYMD       TO  HKN-TEKSTYMD
           PERFORM 930-HKNNUM-READ-SEC
           IF      FLG-HKNNUM      NOT =   ZERO
               INITIALIZE              HKNNUM-REC
           END-IF
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    SPA-HOSPID            TO  HKNJA-HOSPID
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
           MOVE    PTHKN-HKNJA-G         TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "HKNJAINF-KEY"        TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"           USING
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN   "       USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                        MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                        MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                        SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
      *=== 4.0対応 By Otake(2007/09/21) start
      *         MOVE    "HKNJAINF-KEY"      TO  ORC-DBPATH
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
      *=== 4.0対応 By Otake(2007/09/21) end
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
      *---- (06/08/09) LINE ADD START ---------------------------- 
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---- (06/08/09) LINE ADD END   ---------------------------- 
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "HKNJAINF-KEY"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN   "       USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                        MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                        MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                        SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
      *
      *    帳票編集処理
           PERFORM 310-HC01-HEN-SEC
      *    患者病名編集
           PERFORM  311-HC01-HEN-SEC
      
      *
           .
       2001-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       310-HC01-HEN-SEC                SECTION.
      *
           INITIALIZE  H01KA4
           INITIALIZE  H01SA4
           INITIALIZE  H01SA4-KANANAME
      *
      *    患者マスターより
      *---- (01.00.03) LINE MOD START ----------------------------- 
      *    MOVE    HC01LNK-PTNUM       TO  HC01-PTNUM
           MOVE    SPA-PTNUM           TO  H01KA4-PTNUM
                                           H01SA4-PTNUM

      *---- (01.00.03) LINE MOD END -------------------------------
      * 20050117  MOTO  追加
      ***     MOVE    PTINF-NAME          TO  H01KA4-NAME
      ***                                     H01SA4-NAME
           IF  PTINF-NAME(21:)  =  SPACE
               MOVE    PTINF-NAME          TO  H01KA4-NAME
                                               H01SA4-NAME
           ELSE
               MOVE    PTINF-NAME          TO  H01KA4-NAME2
                                               H01SA4-NAME2
           END-IF
      * 20050117  MOTO  追加
           MOVE    PTINF-KANANAME      TO  H01KA4-KANANAME
                                           H01SA4-KANANAME
      *
      *---- (01.00.01) LINE ADD START ----------------------------- 
      *    診療科
      *****MOVE    SPA-SRYKAMEI        TO  HC01-SRYKA
      *---- (01.00.01) LINE ADD END -------------------------------
      *
           EVALUATE    PTINF-SEX
               WHEN    "1"
      ************ MOVE    NC"男"                TO  HC01-SEX
                   MOVE    "○"                  TO  H01KA4-OTOKO
                                                     H01SA4-OTOKO
               WHEN    "2"
      ************ MOVE    NC"女"                TO  HC01-SEX
                   MOVE    "○"                  TO  H01KA4-ONNA
                                                     H01SA4-ONNA
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY         TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
      *    MOVE    WRK-HENYMDG         TO  SCNV-INCODE
      *    CALL    "ORCSCNV"           USING
      *                                ORCSCNVAREA
      *****MOVE    WRK-HENYMDG         TO  HC01-BIRTH
      *****MOVE    WRK-HENYMDG(1:4)    TO  HC01-TANJYOG
           EVALUATE  LNK-DAY2-EDTYMD1-G
           WHEN  "M"
               MOVE    "○"           TO     H01KA4-MEIJI
                                             H01SA4-MEIJI
           WHEN  "T"
               MOVE    "○"           TO     H01KA4-TAISYOU
                                             H01SA4-TAISYOU
           WHEN  "S"
               MOVE    "○"           TO     H01KA4-SYOUWA
                                             H01SA4-SYOUWA
           WHEN  "H"
               MOVE    "○"           TO     H01KA4-HEISEI
                                             H01SA4-HEISEI
           END-EVALUATE
           MOVE    WRK-HYY             TO  H01KA4-TANJYOY
                                           H01SA4-TANJYOY
           MOVE    WRK-HMM             TO  H01KA4-TANJYOM
                                           H01SA4-TANJYOM
           MOVE    WRK-HDD             TO  H01KA4-TANJYOD
                                           H01SA4-TANJYOD
      *　　年齢
           MOVE    SPA-SYSYMD(1:4)     TO  WRK-SPA-SYSYYYY
           COMPUTE WRK-NENREI        =   WRK-SPA-SYSYY
                                       -   WRK-SYY
      * 20030516変更 
      ***    IF      SPA-SYSYMD(5:4)     <   WRK-SYMD(5:4)
      ***         COMPUTE WRK-NENREI   =   WRK-NENREI   -   1
      ***     END-IF
           IF      SPA-SYSYMD(5:2)     <   WRK-SYMD(5:2)
      ***        現在日付が誕生月に達していない場合 
                   COMPUTE WRK-NENREI   =   WRK-NENREI   -   1
           ELSE
      ***        現在日付と誕生月が一致して誕生日が１日以降の場合 
                IF   SPA-SYSYMD(5:2)   =   WRK-SYMD(5:2)   AND
                      WRK-SYMD(7:2)      >   01 
                      COMPUTE WRK-NENREI   =   WRK-NENREI   -   1
                END-IF 
           END-IF
      * 20030516変更 
      *---- (01.00.01) LINE MOD START ---------------------------------
      *    MOVE   WRK-NENREI        TO  HC01-NENREI
           MOVE    WRK-NENREI       TO  WRK-Z3
      *****MOVE    WRK-Z3           TO  HC01-NENREI 
      *---- (01.00.01) LINE MOD END -----------------------------------
      *
      *    自宅−住所
           MOVE    SPACE               TO  WRK-ADRS
      *---- (01.00.04) LINE MOD START ---------------------------------
      *    STRING  PTINF-HOME-ADRS     DELIMITED   BY  "　　"
      *            PTINF-HOME-BANTI    DELIMITED   BY  "　　"
      *            INTO                    WRK-ADRS
      *    END-STRING
      *    MOVE    WRK-ADRS(1:36)      TO  HC01-ADRS1
      *    MOVE    WRK-ADRS(37:)       TO  HC01-ADRS2
           STRING  PTINF-HOME-ADRS     DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED   BY  SPACE
                   INTO                    WRK-ADRS
           END-STRING
           MOVE    WRK-ADRS            TO  H01KA4-ADRS
                                           H01SA4-ADRS
      *    自宅郵便番号
           MOVE    PTINF-HOME-POST(1:3) TO  WRK-POST1
           MOVE    PTINF-HOME-POST(4:4) TO  WRK-POST2
           MOVE    WRK-POSTG            TO  H01KA4-ADRSPOST
                                            H01SA4-ADRSPOST
      *---- (01.00.04) LINE MOD END -----------------------------------
      *
      *    自宅−昼電話番号
           MOVE    PTINF-HOME-TEL1     TO  H01KA4-ADRSTEL
                                           H01SA4-ADRSTEL
      *    続柄
           MOVE    PTINF-ZOKUGARA      TO  H01KA4-TUZUKI
                                           H01SA4-TUZUKI
      * 20080624 後期高齢者対応START
           IF   PTHKN-HKNNUM   =       "039" OR  "040"
               MOVE     SPACE          TO  H01KA4-TUZUKI
           END-IF
      * 20080624 後期高齢者対応END
      *    職業
           MOVE    PTINF-JOB           TO  H01KA4-WORK
                                           H01SA4-WORK
      *
      *事業所
      *    名称
           MOVE    PTINF-OFFICE-NAME   TO  H01SA4-KINNAME
      *    勤務先−住所,番地方書
           MOVE    SPACE               TO  WRK-ADRS
           STRING  PTINF-OFFICE-ADRS   DELIMITED   BY  SPACE
                   PTINF-OFFICE-BANTI  DELIMITED   BY  SPACE
                   INTO                    WRK-ADRS
           END-STRING
           MOVE    WRK-ADRS            TO  H01SA4-KINADRS
      *    MOVE    WRK-ADRS(1:36)      TO  HC01-KINADRS1
      *    MOVE    WRK-ADRS(37:)       TO  HC01-KINADRS2
      *    電話番号
           MOVE    PTINF-OFFICE-TEL    TO  H01SA4-KINADRSTEL
      * 20080307 公害保険対応START tanaka
           IF      COMB-HKNNUM         =  "975"
               MOVE    COMB-SYU-TANSEIDONAME   TO  WRK-HKNJA-G
           END-IF
      * 20080307 公害保険対応END
           
      *
      *    保険者情報
      *    保険者番号
      *---- (01.00.01) LINE MOD START ---------------------------------
      **   MOVE    PTHKN-HKNJANUM      TO  HC01-HPTNUM
           MOVE    WRK-HKNJA-G         TO  H01KA4-HPTNUM
                                           H01SA4-HPTNUM
      *
      *    記号
      **   MOVE    PTHKN-KIGO          TO  HC01-KIGO
      *    番号
      **   MOVE    PTHKN-NUM           TO  HC01-NO
      *    STRING  PTHKN-KIGO          DELIMITED   BY  SPACE
      *            "・"                DELIMITED   BY  SIZE
      *            PTHKN-NUM           DELIMITED   BY  SPACE
      *            INTO                    HC01-KIGONUM
      *****END-STRING
      *    全角変換
           MOVE    PTHKN-KIGO          TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-KIGONUM
           MOVE    PTHKN-NUM           TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-NUM
      *
           IF      (WRK-KIGONUM        =   SPACE )  AND
                   (WRK-NUM            =   SPACE )
               MOVE    SPACE               TO  WRK-HEN-KIGONUM
           ELSE
               STRING  WRK-KIGONUM         DELIMITED   BY  SPACE
                       "・"                DELIMITED   BY  SIZE
                       WRK-NUM             DELIMITED   BY  SPACE
      *---- (01.00.11) LINE UPD START ----------------------------
      ***********  INTO                    HC01-KIGONUM
                   INTO                    WRK-HEN-KIGONUM
               END-STRING
           END-IF
      *                                           
      * 20080314 H20年4月改正対応START tanaka
      *    後期高齢者は番号のみ
           MOVE    WRK-HEN-KIGONUM     TO  H01KA4-KIGONUM
                                           H01SA4-KIGONUM
           IF     (COMB-HKNNUM         =  "039" )
              AND (WRK-KIGONUM         =   SPACE )
              MOVE   WRK-NUM     TO     H01KA4-KIGONUM
                                         H01SA4-KIGONUM
           END-IF
      *
      * 20080814 記号・番号が１５文字以上は別行へ START monma
           IF      WRK-HEN-KIGONUM(29:)    NOT =   SPACE
                   MOVE    SPACE         TO     H01KA4-KIGONUM
                                                H01SA4-KIGONUM
                   MOVE    WRK-KIGONUM   TO     H01KA4-KIGO
                                                H01SA4-KIGO
                   MOVE    WRK-NUM       TO     H01KA4-NUM
                                                H01SA4-NUM
           END-IF
      * 20080814 記号・番号が１５文字以上は別行へ END   monma
      * 
      *    記号・番号が１６文字以上は別行へ
      *    IF      WRK-HEN-KIGONUM(33:)    NOT =   SPACE
      *            MOVE    SPACE         TO     H01KA4-KIGONUM
      *                                         H01SA4-KIGONUM
      *            MOVE    WRK-KIGONUM   TO     H01KA4-KIGO
      *                                         H01SA4-KIGO
      *            MOVE    WRK-NUM       TO     H01KA4-NUM
      *                                         H01SA4-NUM
      *   END-IF
      *
      * 20080314 H20年4月改正対応END
      *---- (01.00.11) LINE UPD START ----------------------------
      *
      *---- (01.00.01) LINE MOD END   ---------------------------------
      *    被保険者名
      ***  20040927 国保の場合、世帯主に変更
      ***     MOVE    PTHKN-HIHKNJANAME   TO  H01KA4-HIHKNJNAME
      ***                                     H01SA4-HIHKNJNAME
           MOVE    PTINF-SETAINUSI   TO  H01KA4-HIHKNJNAME
           MOVE    PTHKN-HIHKNJANAME   TO  H01SA4-HIHKNJNAME
      ***  20040927 国保の場合、世帯主に変更
      *
      *    適用開始年月日（資格取得日へ）
      *****MOVE    PTHKN-TEKSTYMD      TO  WRK-SYMD
           IF      PTHKN-SKKGETYMD     =   SPACE
               CONTINUE
           ELSE
               MOVE    PTHKN-SKKGETYMD     TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
      *@@@@@@@@MOVE    WRK-HENYMDG(1:4)    TO  HC01-SIKAKUG
               EVALUATE  LNK-DAY2-EDTYMD1-G
               WHEN  "S"
                   MOVE    "○"            TO    H01KA4-SKKSYOWA
                                                 H01SA4-SKKSYOWA
               WHEN  "H"
                   MOVE    "○"            TO    H01KA4-SKKHEISEI
                                                 H01SA4-SKKHEISEI
               END-EVALUATE
               MOVE    WRK-HYY             TO  H01KA4-SIKAKUY
                                               H01SA4-SIKAKUY
               MOVE    WRK-HMM             TO  H01KA4-SIKAKUM
                                               H01SA4-SIKAKUM
               MOVE    WRK-HDD             TO  H01KA4-SIKAKUD
                                               H01SA4-SIKAKUD
           END-IF
      *    適用終了年月日
           IF      PTHKN-TEKEDYMD      =   SPACE  OR  ALL "9"
      *---- (01.00.01) LINE MOD START ---------------------------------
               CONTINUE
      **       MOVE    "平成　　年　　月　　日"
      **                                   TO  HC01-TEKEDYMD
      *---- (01.00.01) LINE MOD END -----------------------------------
           ELSE
               MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
      *********MOVE    WRK-HENYMDG         TO  HC01-TEKEDYMD
      *********MOVE    WRK-HENYMDG(1:4)    TO  HC01-YUKOG
               MOVE    WRK-HYY             TO  H01KA4-YUKOY
                                               H01SA4-YUKOY
               MOVE    WRK-HMM             TO  H01KA4-YUKOM
                                               H01SA4-YUKOM
               MOVE    WRK-HDD             TO  H01KA4-YUKOD
                                               H01SA4-YUKOD
           END-IF
      *
      *    保険者情報
      **** MOVE    HKNJA-HKNJANAME-TAN1    TO  HC01-HKNNAME
           MOVE    HKNJA-HKNJANAME         TO  H01KA4-HKNJANAME
                                               H01SA4-HKNNAME
           STRING  HKNJA-ADRS        DELIMITED  BY  SPACE
                   HKNJA-BANTI       DELIMITED  BY  SPACE
                   INTO                    H01SA4-HKNADRS
           END-STRING
      * 20080624 後期高齢者対応START
           IF   PTHKN-HKNNUM   =   "039" OR  "040"
               STRING  HKNJA-ADRS        DELIMITED  BY  SPACE
                   HKNJA-BANTI       DELIMITED  BY  SPACE
                   INTO                    H01KA4-HKNJAADR
               END-STRING
           END-IF
      * 20080624 後期高齢者対応END
      *    MOVE    HKNJA-ADRS              TO  HC01-HKNADRS
      *    MOVE    HKNJA-BANTI         TO  SPA-GMN2B-HKNJABANTI
           MOVE    HKNJA-TEL           TO  H01SA4-HKNADRSTEL
      *
      *    負担割合
      *    本人−外来−負担区分 
      *---- (01.00.01) LINE MOD START ------------------------------
      **   IF      PTHKN-HONKZKKBN     =   "1"
      **       MOVE    HKN-HON-GAIKFTNRATE     TO  HC01-FTNWARI
      **   ELSE
      **       MOVE    HKN-KZK-GAIKFTNRATE     TO  HC01-FTNWARI
      **   END-IF
      *****IF      COMB-GAI-KFTNRATE >   ZERO
      *        COMPUTE WRK-WARI        =   COMB-GAI-KFTNRATE   /   10
      *        IF      COMB-GAI-KFTNRATE(3:1)  =   ZERO
      *            MOVE    WRK-WARI            TO  WRK-Z6
      *            MOVE    WRK-Z6              TO  HC01-FTNWARI
      *        ELSE
      *            MOVE    WRK-WARI            TO  WRK-Z62
      *            MOVE    WRK-Z62             TO  HC01-FTNWARI
      *        END-IF
      *****END-IF
      *
      * 20030516 負担割合ロジックの追加
      * 20050105 ３歳未満の修正
      ***   IF   WRK-NENREI  <  3   
      * 20080404 義務教育就学前の負担割合を修正START tanaka
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    COMB-HKNCOMBINUM    TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF SPA-SRYYMD  <  "20080401"
      * 20080404 義務教育就学前の負担割合を修正END
               IF    WRK-NENREI  <  3   AND  COMB-GAI-KFTNRATE = 30
                      MOVE   2            TO    WRK-Z9
                      MOVE   WRK-Z9       TO    H01KA4-FTNRITU
                                                H01SA4-FTNRITU
               ELSE 
                  COMPUTE WRK-WARI  =   COMB-GAI-KFTNRATE   /   10
                  IF      WRK-WARI(3:1) =   ZERO
                      MOVE    WRK-WARI    TO    WRK-Z9
                      MOVE    WRK-Z9      TO    H01KA4-FTNRITU
                                                H01SA4-FTNRITU
                  ELSE
                      MOVE    WRK-WARI    TO    WRK-Z9V9
                      MOVE    WRK-Z9V9    TO    H01KA4-FTNRITU
                                                H01SA4-FTNRITU
                  END-IF
               END-IF
      * 20080404 義務教育就学前の負担割合を修正START tanaka
           ELSE
               IF  AGECHK-SYUGAKUKBN = 1  AND  COMB-GAI-KFTNRATE = 30
                      MOVE   2            TO    WRK-Z9
                      MOVE   WRK-Z9       TO    H01KA4-FTNRITU
                                                H01SA4-FTNRITU
               ELSE 
                  COMPUTE WRK-WARI  =   COMB-GAI-KFTNRATE   /   10
                  IF      WRK-WARI(3:1) =   ZERO
                      MOVE    WRK-WARI    TO    WRK-Z9
                      MOVE    WRK-Z9      TO    H01KA4-FTNRITU
                                                H01SA4-FTNRITU
                  ELSE
                      MOVE    WRK-WARI    TO    WRK-Z9V9
                      MOVE    WRK-Z9V9    TO    H01KA4-FTNRITU
                                                H01SA4-FTNRITU
                  END-IF
               END-IF
           END-IF
      * 20080404 義務教育就学前の負担割合を修正END
      * 20080703 保険を未選択時に割合を表示させないSTART
           IF PTHKN-HKNNUM = SPACE
               MOVE    SPACE    TO    H01KA4-FTNRITU
                                      H01SA4-FTNRITU
           END-IF
      * 20080703 保険を未選択時に割合を表示させないEND
      
      * 20030516 負担割合ロジックの追加
      *
      *    保険区分　主保険−短縮制度名
      **   MOVE    COMB-SYU-TANSEIDONAME   TO  HC01-KUMI
           MOVE    SPACE               TO  WRK-HKNCOMBI
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
           END-STRING
      *
      *    公費情報
      * 20080603 公費を３つ使うとﾌﾘｶﾞﾅが印字されない不具合を修正START tanaka
      *     PERFORM VARYING IDY FROM    1   BY  1
      *             UNTIL   IDY >       3
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       2
      * 20080603 公費を３つ使うとﾌﾘｶﾞﾅが印字されない不具合を修正END
               INITIALIZE                  PTKOHINF-REC
      *=== 4.0対応 By Otake(2007/09/21) start
      *         MOVE    SPA-HOSPID          TO  PTKOH-HOSPID
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
               MOVE    SPA-PTID            TO  PTKOH-PTID
               MOVE    COMB-KOHID (IDY)    TO  PTKOH-KOHID
               PERFORM 940-PTKOHINF-READ-SEC
               IF      FLG-PTKOHINF          =   ZERO
      * 20031031 追加 岐阜県福祉医療費 受給者番号印字対応
                 IF    PTKOH-KOHNUM        =   "141"
                  OR   PTKOH-KOHNUM        =   "143"
                  OR   PTKOH-KOHNUM        =   "144"
                  OR   PTKOH-KOHNUM        =   "145"
                  OR   PTKOH-KOHNUM        =   "146"
                  OR   PTKOH-KOHNUM        =   "147"
      * 受給者番号
                       MOVE  PTKOH-JKYSNUM TO  H01KA4-FK-JKYSNO
                                               H01SA4-FK-JKYSNO 
      * 福祉医療費名称
                   EVALUATE  PTKOH-KOHNUM
                       WHEN  "141"
                       WHEN  "147"
                             MOVE  "老人"  TO  H01KA4-FK-NAME
                                               H01SA4-FK-NAME 
                       WHEN  "145"
                             MOVE  "乳児"  TO  H01KA4-FK-NAME
                                               H01SA4-FK-NAME 
                       WHEN  "144"
                             MOVE  "重老"  TO  H01KA4-FK-NAME
                                               H01SA4-FK-NAME 
                       WHEN  "146"
                             MOVE  "重度"  TO  H01KA4-FK-NAME
                                               H01SA4-FK-NAME 
                       WHEN  "143"
                             MOVE  "母子"  TO  H01KA4-FK-NAME
                                               H01SA4-FK-NAME 
                   END-EVALUATE
                 ELSE
      * 20031031 追加 岐阜県福祉医療費 受給者番号印字対応
                   PERFORM 3102-PTKOHINF-HEN-SEC
                   MOVE    WRK-FTNSNO      TO  H01KA4-FTNSNO (IDY)
                                               H01SA4-FTNSNO (IDY)
      *************MOVE    WRK-JKYSNO      TO  HC01-JKYSNO (IDY)
                   IF      WRK-JKYSNO(15:)     =   SPACE
                       MOVE    WRK-JKYSNO      TO  H01KA4-JKYSNO(IDY)
                                                   H01SA4-JKYSNO(IDY)
                   ELSE
                       MOVE    WRK-JKYSNO      TO  H01KA4-JKYSNON(IDY)
                                                   H01SA4-JKYSNON(IDY)
                   END-IF
      * 20031031 追加 岐阜県福祉医療費 受給者番号印字対応
                 END-IF
      * 20031031 追加 岐阜県福祉医療費 受給者番号印字対応
               ELSE
                   MOVE    "平成　　年　　月　　日"
                                           TO  WRK-TEKSTYMD
                                               WRK-TEKEDYMD
               END-IF
      ****     MOVE    WRK-TEKSTYMD    TO  HC01-TTEKSTYMD (IDY)
      ****     MOVE    WRK-TEKEDYMD    TO  HC01-TTEKEDYMD (IDY)
           END-PERFORM
      *---- (01.00.03) LINE MOD END ------------------------------ 
      *---- (01.00.11) LINE ADD START ----------------------------
      *
      *    労災の時、保険者番号・記号の変更
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
      *=== 4.0対応 By Otake(2007/09/21) start
      *         MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) start
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *         MOVE    "PTRSIINF-KEY"       TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                     ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
               IF      MCP-RC              =   ZERO
      *=== 4.0対応 By Otake(2007/09/21) start
      *             MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
      *=== 4.0対応 By Otake(2007/09/21) end
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
      *=== 4.0対応 By Otake(2007/09/21) start
               MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"           USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
               IF      FLG-PTRSIINF        =   ZERO
      *            保険者番号へ記号（労災区分名）
                   MOVE    WRK-HEN-KIGONUM     TO  H01KA4-HPTNUM
                                                   H01SA4-HPTNUM
      *            記号へ患者交付番号
                   MOVE    PTRSI-KOFUNUM       TO  WRK-MAE-INPUT
                   PERFORM 2009-HENKAN-SEC
                   MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  H01KA4-KIGONUM
                                                   H01SA4-KIGONUM
      *
               END-IF
           END-IF
      *---- (01.00.11) LINE ADD END   ----------------------------
           .
       310-HC01-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者病名編集処理
      *****************************************************************
       311-HC01-HEN-SEC        SECTION.
      *
      *---(01.00.01) LINE ADD START ------------------------------------
      *
      *    患者病名マスタより編集
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-ZOKUSHI
           INITIALIZE                      PTBYOMEI-REC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
      *---- (01.00.03) LINE MOD START ---------------------------- 
      **   MOVE    HC01LNK-PTID        TO  PTBYO-PTID
      **   MOVE    HC01LNK-SRYKA       TO  PTBYO-SRYKA
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    SPA-SRYKA           TO  PTBYO-SRYKA
      *---- LINE MOD END   ----------------------------------------
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "PTBYOMEI-KEY2"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
      *=== 4.0対応 By Otake(2007/09/21) start
      *         MOVE    "PTBYOMEI-KEY2"     TO  ORC-DBPATH
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
      *=== 4.0対応 By Otake(2007/09/21) end
               PERFORM 950-PTBYOMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           PERFORM 
                   UNTIL      (FLG-PTBYOMEI    NOT =   ZERO  )
                        OR    (FLG-ZOKUSHI     NOT =   ZERO  )
      ***   20041126 MOTOHASI 
      **      継続病名のみの時、転帰日のあるものを対象外とする
             IF     (SYS-1017-KARUTE-BYOMEI    =   "1"  )  AND
                    (PTBYO-TENKIYMD        NOT =   SPACE)
               CONTINUE
             ELSE
      ***   20041126 MOTOHASI 
      **** 20050915 カルテの病名対象:２の場合、病名未印字 MOTO
	           IF     SYS-1017-KARUTE-BYOMEI    =   "2"
                 CONTINUE
               ELSE
      **** 20050915 ここまで MOTO
      *          病名編集処理
                 PERFORM 3103-BYOMEI-SET-SEC
      *---- (01.00.05) LINE MOD START ---------------------------------
      *********MOVE    PTBYO-BYOMEI        TO  HC01-BYOMEIG (IDX)  
      *          カルテ病名がある時はカルテ病名
                 IF      PTBYO-CHARTBYOMEI   =   SPACE
                   MOVE    PTBYO-BYOMEI        TO  H01KA4-BYOMEI (IDX)
                                                   H01SA4-BYOMEI (IDX)
                 ELSE
                   MOVE    PTBYO-CHARTBYOMEI   TO  H01KA4-BYOMEI (IDX)
                                                   H01SA4-BYOMEI (IDX)
                 END-IF
                 MOVE    WRK-STRYYW          TO  H01KA4-STRYMDY  (IDX)
                                                 H01SA4-STRYMDY  (IDX)
                 MOVE    WRK-STRMMW          TO  H01KA4-STRYMDM  (IDX)
                                                 H01SA4-STRYMDM  (IDX)
                 MOVE    WRK-STRDDW          TO  H01KA4-STRYMDD  (IDX)
                                                 H01SA4-STRYMDD  (IDX)
                 MOVE    WRK-ENDYYW          TO  H01KA4-ENDYMDY  (IDX)
                                                 H01SA4-ENDYMDY  (IDX)
                 MOVE    WRK-ENDMMW          TO  H01KA4-ENDYMDM  (IDX)
                                                 H01SA4-ENDYMDM  (IDX)
                 MOVE    WRK-ENDDDW          TO  H01KA4-ENDYMDD  (IDX)
                                                 H01SA4-ENDYMDD  (IDX)
      *         IF      IDX2                >   ZERO
      *             MOVE    "○"            TO  HC01-TENKI   (IDX IDX2)
      *         END-IF
                 EVALUATE IDX2
                 WHEN    1
                   MOVE    "治癒"          TO    H01KA4-TENKI(IDX)
                                                 H01SA4-TENKI(IDX)
                 WHEN    2
                   MOVE    "死亡"          TO    H01KA4-TENKI(IDX)
                                                 H01SA4-TENKI(IDX)
                 WHEN    3
                   MOVE    "中止"          TO    H01KA4-TENKI(IDX)
                                                 H01SA4-TENKI(IDX)
                 END-EVALUATE
      ***   20050915 MOTO 
               END-IF
      ***   20050915 MOTO 
      ***   20041126 MOTOHASI 
             END-IF
      ***   20041126 MOTOHASI 
      *
      *=== 4.0対応 By Otake(2007/09/21) end
      *         MOVE    "PTBYOMEI-KEY2"     TO  ORC-DBPATH
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
      *=== 4.0対応 By Otake(2007/09/21) end
               PERFORM 950-PTBYOMEI-READ-SEC
      *
      *********IF      IDX             >=  6
               IF      IDX             >=  17
      *@@@@@@@@  続紙無し
                   MOVE      1                 TO  FLG-ZOKUSHI
      ******** ELSE
      *            MOVE    "PTBYOMEI-KEY2"     TO  ORC-DBPATH
      ***********  PERFORM 950-PTBYOMEI-READ-SEC
               END-IF
           END-PERFORM
      *=== 4.0対応 By Otake(2007/09/21) start
      * 20080206 続紙に病名が１件しか表示されない不具合を修正START t.tanaka
      *     MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
      *     MOVE    "tbl_ptbyomei"        TO  MCP-TABLE
      *     MOVE    "key2"                TO  MCP-PATHNAME
      *     CALL    "ORCDBMAIN"           USING
      *                                   MCPAREA
      *                                   MCPDATA-REC
      *                                   SPA-AREA
      * 20080206 続紙に病名が１件しか表示されない不具合を修正END
      *=== 4.0対応 By Otake(2007/09/21) end
      *
      *---- (01.00.05) LINE MOD END -----------------------------------
      *
      *--- (2008/10/28) LINE ADD START MONMA ---------------------------
      *         MOVE    "DBCLOSECURSOR"     TO      MCP-FUNC
      *         CALL    "ORCDBMAIN"         USING   MCPAREA
      *                                             MCPDATA-REC
      *                                             SPA-AREA
      *--- (2008/10/28) LINE ADD END   MONMA ---------------------------
           .
       311-HC01-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費変換処理
      *****************************************************************
       3102-PTKOHINF-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-TEKSTYMD
           MOVE    SPACE               TO  WRK-TEKEDYMD
           MOVE    SPACE               TO  WRK-FTNSNO
           MOVE    SPACE               TO  WRK-JKYSNO
      *    保険者情報
      *****保険者番号　漢字変換
      *    MOVE    1                   TO  IDX3
      *    MOVE    PTKOH-FTNJANUM      TO  WRK-HPTNUM
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   8
      *        IF      WRK-HPTNUM (IDX:1)  NUMERIC
      *            MOVE    ZERO                    TO  IDX2
      *            MOVE    WRK-HPTNUM (IDX:1)      TO  IDX2-1
      *            IF      IDX2-1          =   ZERO
      *                MOVE    10                  TO  IDX2
      *            ELSE
      *                MOVE    IDX2-1              TO  IDX2
      *            END-IF
      *            MOVE    TBL-SUJI (IDX2)     TO  WRK-FTNSNO(IDX3:2)
      *        END-IF
      *        ADD     2                   TO  IDX3
      **** END-PERFORM
      *    全角変換
           MOVE    PTKOH-FTNJANUM      TO  WRK-MAE-INPUT
           MOVE    16                  TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:16) TO  WRK-FTNSNO
      *    受給者番号１　漢字変換
      *****MOVE    1                   TO  IDX3
      *    MOVE    PTKOH-JKYSNUM       TO  WRK-HPTNUM
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   8
      *        IF      WRK-HPTNUM (IDX:1)  NUMERIC
      *            MOVE    WRK-HPTNUM (IDX:1)      TO  IDX2-1
      *            IF      IDX2-1          =   ZERO
      *                MOVE    10                  TO  IDX2
      *            ELSE
      *                MOVE    IDX2-1              TO  IDX2
      *            END-IF
      *            MOVE    TBL-SUJI (IDX2)     TO  WRK-JKYSNO(IDX3:2)
      *        END-IF
      *        ADD     2                   TO  IDX3
      *****END-PERFORM
      *    全角変換
           MOVE    PTKOH-JKYSNUM       TO  WRK-MAE-INPUT
           MOVE    14                  TO  WRK-MOJISU
           PERFORM 2009-HENKAN-SEC
      **** IF      KANACHK-MAX         >   14
      *        COMPUTE WRK-MOJISU      =   KANACHK-MAX   -   14
      *                                +   1
      *        MOVE    WRK-OUT-INPUT(WRK-MOJISU:14) TO  WRK-JKYSNO
      *    ELSE
      *        MOVE    WRK-OUT-INPUT(1:14)        TO  WRK-JKYSNO
      *****END-IF
           MOVE    SPACE               TO  WRK-JKYSNO
           MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-JKYSNO
      *
      *    適用開始年月日
           MOVE    PTKOH-TEKSTYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-TEKSTYMD
      *    適用終了年月日
           IF      PTKOH-TEKEDYMD      =   SPACE  OR  ALL "9"
      *---- (01.00.01) LINE MOD START ---------------------------------
      **       MOVE    SPACE               TO  WRK-TEKEDYMD
               MOVE    "平成　　年　　月　　日"
                                           TO  WRK-TEKEDYMD
      *---- (01.00.01) LINE MOD END -----------------------------------
           ELSE
               MOVE    PTKOH-TEKEDYMD      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-TEKEDYMD
           END-IF
      *
      **********
      **********
           .
       3102-PTKOHINF-HEN-EXT.
           EXIT.
      *---- (01.00.05) LINE MOD START ----------------------------
      *****************************************************************
      *    続紙印刷処理
      *****************************************************************
       2002-ZOKUSHI-SEC          SECTION.
      *
      *    ファイルＯＰＥＮ
      *=== 4.0対応 By Otake(2007/09/21) start
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    SPA-HOSPNUM         TO  HC01PARA2-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    "H01SA4"             TO  HC01PARA2-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
                                           HC01PARA2-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
                                           HC01PARA2-SYOHMS
      *    変更★★★★★★
      *****MOVE    "001"               TO  HC01PARA-CNT
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
                                           HC01PARA2-CNT
           OPEN    OUTPUT               PRT-FILE
                                        PRT-FILE2
      *
           INITIALIZE  H01KA4
           INITIALIZE  H01SA4
           MOVE    ZERO                TO  IDX
           PERFORM 310-HC01-HEN-SEC
      *
      * 　 印刷項目編集
           PERFORM  UNTIL      FLG-PTBYOMEI    NOT =   ZERO
      *
               IF      IDX             >=  17
                   PERFORM 20021-HC011-PRINT-SEC
                   INITIALIZE  H01KA4
                   INITIALIZE  H01SA4
                   MOVE    ZERO                TO  IDX
                   PERFORM 310-HC01-HEN-SEC
               END-IF
      *
      ***   20041126 MOTOHASI 
      **      継続病名のみの時、転帰日のあるものを対象外とする
             IF     (SYS-1017-KARUTE-BYOMEI    =   "1"  )  AND
                    (PTBYO-TENKIYMD        NOT =   SPACE)
               CONTINUE
             ELSE
      ***   20041126 MOTOHASI 
      **** 20050915 カルテ病名対象:２の場合、病名未印字 MOTO
	       IF     SYS-1017-KARUTE-BYOMEI   =  "2"
                 CONTINUE
	       ELSE
      **** 20050915 ここまで MOTO
      *
      *          病名編集処理
                 PERFORM 3103-BYOMEI-SET-SEC
      *********MOVE    PTBYO-BYOMEI        TO  HC011-BYOMEIG (IDX)
      *          カルテ病名がある時はカルテ病名
                 IF      PTBYO-CHARTBYOMEI   =   SPACE
                   MOVE    PTBYO-BYOMEI        TO  H01KA4-BYOMEI (IDX)
                                                   H01SA4-BYOMEI (IDX)
                 ELSE
                   MOVE    PTBYO-CHARTBYOMEI   TO  H01KA4-BYOMEI (IDX)
                                                   H01SA4-BYOMEI (IDX)
                 END-IF
                 MOVE    WRK-STRYYW          TO  H01KA4-STRYMDY  (IDX)
                                                 H01SA4-STRYMDY  (IDX)
                 MOVE    WRK-STRMMW          TO  H01KA4-STRYMDM  (IDX)
                                                 H01SA4-STRYMDM  (IDX)
                 MOVE    WRK-STRDDW          TO  H01KA4-STRYMDD  (IDX)
                                                 H01SA4-STRYMDD  (IDX)
                 MOVE    WRK-ENDYYW          TO  H01KA4-ENDYMDY  (IDX)
                                                 H01SA4-ENDYMDY  (IDX)
                 MOVE    WRK-ENDMMW          TO  H01KA4-ENDYMDM  (IDX)
                                                 H01SA4-ENDYMDM  (IDX)
                 MOVE    WRK-ENDDDW          TO  H01KA4-ENDYMDD  (IDX)
                                                 H01SA4-ENDYMDD  (IDX)
      *
                 EVALUATE IDX2
                 WHEN    1
                   MOVE    "治癒"          TO    H01KA4-TENKI(IDX)
                                                 H01SA4-TENKI(IDX)
                 WHEN    2
                   MOVE    "死亡"          TO    H01KA4-TENKI(IDX)
                                                 H01SA4-TENKI(IDX)
                 WHEN    3
                   MOVE    "中止"          TO    H01KA4-TENKI(IDX)
                                                 H01SA4-TENKI(IDX)
                 END-EVALUATE
      ***   20050915 MOTO
               END-IF
      ***   20050915 MOTO 
      ***   20041126 MOTOHASI 
             END-IF
      ***   20041126 MOTOHASI 
      *
      *=== 4.0対応 By Otake(2007/09/21) start
      * 20080206 続紙に病名が１件しか表示されない不具合を修正START t.tanaka
      *         MOVE    "DBSELECT"          TO  MCP-FUNC
      *         MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
      *         MOVE    "key2"              TO  MCP-PATHNAME
      *         CALL    "ORCDBMAIN"         USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
      *                                     SPA-AREA
      * 20080206 続紙に病名が１件しか表示されない不具合を修正END
      *         MOVE    "PTBYOMEI-KEY2"      TO  ORC-DBPATH
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
      *=== 4.0対応 By Otake(2007/09/21) end
               PERFORM 950-PTBYOMEI-READ-SEC
           END-PERFORM
      *=== 4.0対応 By Otake(2007/09/21) start
      * 20080206 続紙に病名が１件しか表示されない不具合を修正START t.tanaka
      *         MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
      *         MOVE    "tbl_ptrsiinf"        TO  MCP-TABLE
      *         MOVE    "key"                 TO  MCP-PATHNAME
      *         CALL    "ORCDBMAIN"           USING
      *                                       MCPAREA
      *                                       MCPDATA-REC
      *                                       SPA-AREA
      * 20080206 続紙に病名が１件しか表示されない不具合を修正END
      *=== 4.0対応 By Otake(2007/09/21) end
      *
           IF      IDX                 >   ZERO
               PERFORM 20021-HC011-PRINT-SEC
           END-IF
      *
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      *    MOVE    "PTBYOMEI-KEY2"     TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      ********************************* MCPDATA-REC
           .
       2002-ZOKUSHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    続紙印刷　処理
      *****************************************************************
       20021-HC011-PRINT-SEC                SECTION.
      *
      *    印刷用データ出力
            WRITE  H01KA4
            WRITE  H01SA4
      *
            CLOSE                       PRT-FILE
                                        PRT-FILE2
      *
      *---- (2008/10/28) LINE ADD START MONMA --------------------------
           IF ORCHC01-SYORIKBN   NOT = "3"
      *---- (2008/10/28) LINE ADD END   MONMA --------------------------
      *
      *     国保・健保印刷判別
             EVALUATE   TRUE
             WHEN  PTHKN-HKNNUM   = "060"  OR   "067"
      * 20080624 後期高齢者対応START
                                   OR  "039" OR  "040"
      * 20080624 後期高齢者対応END
               INITIALIZE                 ORCSMKPRTAREA
               MOVE   "HC01.sh"           TO  MKPRT-ID
      ***   20040909 MOTOHASI MONPEに変更
      ***          MOVE   "A21000H01KA4.dia"          TO  MKPRT-DIA
      ***          MOVE   "A21000H01KA4.def"          TO  MKPRT-DEF
               MOVE   "A21000H01KA4.red"          TO  MKPRT-DIA
               MOVE   SPACE                       TO  MKPRT-DEF
      ***   20040909 MOTOHASI MONPEに変更
               MOVE   HC01PARA            TO  MKPRT-DAT
               MOVE   SYS-1031-PRTNM(1)   TO  MKPRT-PRTNM
      ***   20031007  変更 @@@@-MOTOHASI
      ***      CALL   "ORCSMKPRT"         USING
      ***                                 ORCSMKPRTAREA
               CALL   "ORCSMKPRTSITE"      USING
                                           ORCSMKPRTAREA
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
             WHEN   OTHER
               INITIALIZE                 ORCSMKPRTAREA
               MOVE   "HC01.sh"           TO  MKPRT-ID
      ***   20040909 MOTOHASI MONPEに変更
      ***          MOVE   "A21000H01SA4.dia"          TO  MKPRT-DIA
      ***          MOVE   "A21000H01SA4.def"          TO  MKPRT-DEF
               MOVE   "A21000H01SA4.red"          TO  MKPRT-DIA
               MOVE   SPACE                       TO  MKPRT-DEF
      ***   20040909 MOTOHASI MONPEに変更
               MOVE   HC01PARA2            TO  MKPRT-DAT
      *          MOVE   HC01PARA            TO  MKPRT-DAT
               MOVE   SYS-1031-PRTNM(1)   TO  MKPRT-PRTNM
      ***   20031007  変更 @@@@-MOTOHASI
      ***      CALL   "ORCSMKPRT"         USING
      ***                                 ORCSMKPRTAREA
               CALL   "ORCSMKPRTSITE"      USING
                                           ORCSMKPRTAREA
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
             END-EVALUATE
      *
      *---- (2008/10/28) LINE ADD START MONMA --------------------------
      *    一括印刷
           ELSE
      *
      *     国保・健保印刷判別
             EVALUATE   TRUE
             WHEN  PTHKN-HKNNUM   = "060"  OR   "067"
                                    OR  "039" OR  "040"
      *
               MOVE    ORCHC01-PRTAREA     TO  ORCSPRTAREA
      *
               MOVE    H01KA4              TO  SPRT-PRTDATA
               MOVE    "2"                 TO  SPRT-SITEKBN
               MOVE    "A21000H01KA4.red"  TO  SPRT-PRTID
               MOVE    "診療録（カルテ１号紙）（外来）一括"
                                           TO  SPRT-TITLE
               CALL    "ORCSPRT"           USING
                                           ORCSPRTAREA
                                           SPA-AREA
               IF      SPRT-RETURN         =   ZERO
                   ADD     1               TO  ORCHC01-CNT
               ELSE
                   MOVE    9               TO  ORCHC01-RC
               END-IF
             WHEN   OTHER
      *
               MOVE    ORCHC01-PRTAREA     TO  ORCSPRTAREA
      *
               MOVE    H01SA4              TO  SPRT-PRTDATA
               MOVE    "2"                 TO  SPRT-SITEKBN
               MOVE    "A21000H01SA4.red"  TO  SPRT-PRTID
               MOVE    "診療録（カルテ１号紙）（外来）一括"
                                          TO  SPRT-TITLE
               CALL    "ORCSPRT"           USING
                                           ORCSPRTAREA
                                           SPA-AREA
               IF      SPRT-RETURN         =   ZERO
                   ADD     1               TO  ORCHC01-CNT
               ELSE
                   MOVE    9               TO  ORCHC01-RC
               END-IF
             END-EVALUATE
           END-IF
      *---- (2008/10/28) LINE ADD END   MONMA --------------------------
      *
      *    ファイルＯＰＥＮ
      *=== 4.0対応 By Otake(2007/09/21) start
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    SPA-HOSPNUM         TO  HC01PARA2-HOSPNUM
      *=== 4.0対応 By Otake(2007/09/21) end
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    "H01SA4"             TO  HC01PARA2-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
                                           HC01PARA2-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
                                           HC01PARA2-SYOHMS
      *    変更★★★★★★
      *****MOVE    "001"               TO  HC01PARA-CNT
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
                                           HC01PARA2-CNT
           OPEN    OUTPUT               PRT-FILE
                                        PRT-FILE2

      *
           .
       20021-HC011-PRINT-EXT.
           EXIT.
      *
      *---- (01.00.05) LINE MOD END ------------------------------ 
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
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
      *---- (01.00.01) LINE INS START ---------------------------------
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
      *---- (01.00.01) LINE INS END -----------------------------------
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *        病名編集処理
      *****************************************************************
       3103-BYOMEI-SET-SEC        SECTION.
      *
           ADD     1                   TO  IDX
      *    開始年月日（診療年月日）
           MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
      *****MOVE    WRK-HENYMDG         TO  WRK-STRYMD
           MOVE    WRK-HENYMD          TO  WRK-STRYMDW
      *
      *    終了年月日（転帰年月日）
           MOVE    SPACE               TO  WRK-ENDYMD
           MOVE    SPACE               TO  WRK-ENDYMDW
           IF      PTBYO-TENKIYMD  NOT =   SPACE
               MOVE    PTBYO-TENKIYMD      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
      *********MOVE    WRK-HENYMDG         TO  WRK-ENDYMD
               MOVE    WRK-HENYMD          TO  WRK-ENDYMDW
           END-IF
      *
           EVALUATE    PTBYO-TENKIKBN
               WHEN    "1"
                   MOVE    1               TO  IDX2
               WHEN    "2"
                   MOVE    2               TO  IDX2
               WHEN    "3"
                   MOVE    3               TO  IDX2
               WHEN    OTHER
                   MOVE    ZERO            TO  IDX2
           END-EVALUATE
      *
      *---- (01.00.02) LINE MOD START ---------------------------------
      **   EVALUATE    IDX
      **     WHEN    1
      **           MOVE    PTBYO-BYOMEI        TO  HC01-BYOMEI1G
      **           MOVE    WRK-STRYY           TO  HC01-STRYMD1 (01)
      **           MOVE    WRK-STRMM           TO  HC01-STRYMD1 (02)
      **           MOVE    WRK-STRDD           TO  HC01-STRYMD1 (03)
      **           MOVE    WRK-ENDYY           TO  HC01-ENDYMD1 (01)
      **           MOVE    WRK-ENDMM           TO  HC01-ENDYMD1 (02)
      **           MOVE    WRK-ENDDD           TO  HC01-ENDYMD1 (03)
      **           IF      IDX2                >   ZERO
      **               MOVE    "○"          TO  HC01-TENKI1 (IDX2)
      **           END-IF
      *
      **       WHEN    2
      **           MOVE    PTBYO-BYOMEI        TO  HC01-BYOMEI2G
      **           MOVE    WRK-STRYY           TO  HC01-STRYMD2 (01)
      **           MOVE    WRK-STRMM           TO  HC01-STRYMD2 (02)
      **           MOVE    WRK-STRDD           TO  HC01-STRYMD2 (03)
      **           MOVE    WRK-ENDYY           TO  HC01-ENDYMD2 (01)
      **           MOVE    WRK-ENDMM           TO  HC01-ENDYMD2 (02)
      **           MOVE    WRK-ENDDD           TO  HC01-ENDYMD2 (03)
      **           IF      IDX2                >   ZERO
      **               MOVE    "○"          TO  HC01-TENKI2 (IDX2)
      **           END-IF
      *
      **       WHEN    3
      **           MOVE    PTBYO-BYOMEI        TO  HC01-BYOMEI3G
      **           MOVE    WRK-STRYY           TO  HC01-STRYMD3 (01)
      **           MOVE    WRK-STRMM           TO  HC01-STRYMD3 (02)
      **           MOVE    WRK-STRDD           TO  HC01-STRYMD3 (03)
      **           MOVE    WRK-ENDYY           TO  HC01-ENDYMD3 (01)
      **           MOVE    WRK-ENDMM           TO  HC01-ENDYMD3 (02)
      **           MOVE    WRK-ENDDD           TO  HC01-ENDYMD3 (03)
      **           IF      IDX2                >   ZERO
      **               MOVE    "○"          TO  HC01-TENKI3 (IDX2)
      **           END-IF
      *
      **       WHEN    4
      **           MOVE    PTBYO-BYOMEI        TO  HC01-BYOMEI4G
      **           MOVE    WRK-STRYY           TO  HC01-STRYMD4 (01)
      **           MOVE    WRK-STRMM           TO  HC01-STRYMD4 (02)
      **           MOVE    WRK-STRDD           TO  HC01-STRYMD4 (03)
      **           MOVE    WRK-ENDYY           TO  HC01-ENDYMD4 (01)
      **           MOVE    WRK-ENDMM           TO  HC01-ENDYMD4 (02)
      **           MOVE    WRK-ENDDD           TO  HC01-ENDYMD4 (03)
      **           IF      IDX2                >   ZERO
      **               MOVE    "○"          TO  HC01-TENKI4 (IDX2)
      **           END-IF
      *
      **       WHEN    5
      **           MOVE    PTBYO-BYOMEI        TO  HC01-BYOMEI5G
      **           MOVE    WRK-STRYY           TO  HC01-STRYMD5 (01)
      **           MOVE    WRK-STRMM           TO  HC01-STRYMD5 (02)
      **           MOVE    WRK-STRDD           TO  HC01-STRYMD5 (03)
      **           MOVE    WRK-ENDYY           TO  HC01-ENDYMD5 (01)
      **           MOVE    WRK-ENDMM           TO  HC01-ENDYMD5 (02)
      **           MOVE    WRK-ENDDD           TO  HC01-ENDYMD5 (03)
      **           IF      IDX2                >   ZERO
      **               MOVE    "○"          TO  HC01-TENKI5 (IDX2)
      **           END-IF
      *
      **       WHEN    6
      **           MOVE    PTBYO-BYOMEI        TO  HC01-BYOMEI6G
      **           MOVE    WRK-STRYY           TO  HC01-STRYMD6 (01)
      **           MOVE    WRK-STRMM           TO  HC01-STRYMD6 (02)
      **           MOVE    WRK-STRDD           TO  HC01-STRYMD6 (03)
      **           MOVE    WRK-ENDYY           TO  HC01-ENDYMD6 (01)
      **           MOVE    WRK-ENDMM           TO  HC01-ENDYMD6 (02)
      **           MOVE    WRK-ENDDD           TO  HC01-ENDYMD6 (03)
      **           IF      IDX2                >   ZERO
      **               MOVE    "○"          TO  HC01-TENKI6 (IDX2)
      **           END-IF
      *
      **   END-EVALUATE
      *---- (01.00.05) LINE ADD START ---------------------------------
      *---- (01.00.05) LINE MOD END ------------------------------------
      *---- (01.00.01) LINE MOD END ------------------------------------
      *
           .
       3103-BYOMEI-SET-EXT.
           EXIT.
      *---- (01.00.01) LINE ADD START ---------------------------------
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
     *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
      *    MOVE    ALL "　"            TO  WRK-ZENKAKU-G
      *
      *    PERFORM VARYING     IDK     FROM    1   BY  1
      *            UNTIL       IDK     >   WRK-MOJISU
      *        IF      WRK-MOJI (IDK:1)    NUMERIC
      *            MOVE    WRK-MOJI(IDK:1)     TO  IDX2-1
      *            IF      IDX2-1              =   ZERO
      *                MOVE    10                  TO  IDX2
      *            ELSE
      *                MOVE    IDX2-1              TO  IDX2
      *            END-IF
      *            MOVE    TBL-SUJI (IDX2)     TO  WRK-ZENKAKU(IDK)
      *        ELSE
      *            IF      WRK-MOJI (IDK:1)    =   "."
      *                MOVE    "．"            TO  WRK-ZENKAKU(IDK)
      *            END-IF
      *        END-IF
      *****END-PERFORM
           .
       2009-HENKAN-EXT.
           EXIT.
      *---- (01.00.01) LINE ADD END -----------------------------------
      * 
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "PTINF-KEY"          TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *         MOVE    "PTINF-KEY"          TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                      ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *---- (06/08/09) LINE ADD START ---------------------------- 
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---- (06/08/09) LINE ADD END   ---------------------------- 
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "PTINF-KEY"          TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) strat
      *     MOVE    "HKNCOMBI-KEY"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) strat
      *         MOVE    "HKNCOMBI-KEY"       TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                     ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
      *---- (06/08/09) LINE ADD START ---------------------------- 
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---- (06/08/09) LINE ADD END   ---------------------------- 
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "HKNCOMBI-KEY"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "PTHKNINF-KEY"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *         MOVE    "PTHKNINF-KEY"       TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                     ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
      *---- (06/08/09) LINE ADD START ---------------------------- 
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---- (06/08/09) LINE ADD END   ---------------------------- 
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "PTHKNINF-KEY"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
      *
           .
       920-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読み込み
      *****************************************************************
       930-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "HKNNUM-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *         MOVE    "HKNNUM-KEY"         TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                     ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNNUM-REC
                   MOVE    ZERO            TO  FLG-HKNNUM
               ELSE
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
      *---- (06/08/09) LINE ADD START ---------------------------- 
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---- (06/08/09) LINE ADD END   ---------------------------- 
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "HKNNUM-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
      *
           .
       930-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       940-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "PTKOHINF-KEY"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *         MOVE    "PTKOHINF-KEY"       TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                     ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
      *---- (06/08/09) LINE ADD START ---------------------------- 
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---- (06/08/09) LINE ADD END   ---------------------------- 
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "PTKOHINF-KEY"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読み込み
      *****************************************************************
       950-PTBYOMEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     CALL    "ORCMCPSUB"          USING
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                        MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                        MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                        SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       950-PTBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
      *
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    SPA-SYSYMD     TO  ORC-DBYMD
      *=== 4.0対応 By Otake(2007/09/21) end
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      * 20080206 転帰した病名が表示されてしまう不具合を修正START t.tanaka
      *     MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "key10"               TO  MCP-PATHNAME
      * 20080206 転帰した病名が表示されてしまう不具合を修正END
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *         MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
      * 20080206 転帰した病名が表示されてしまう不具合を修正START t.tanaka
      *     MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "key10"               TO  MCP-PATHNAME
      * 20080206 転帰した病名が表示されてしまう不具合を修正END
               CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                     ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                           MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                           SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *---- (06/08/09) LINE ADD START ---------------------------- 
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---- (06/08/09) LINE ADD END   ---------------------------- 
      *=== 4.0対応 By Otake(2007/09/21) start
      *     MOVE    "SYSKANRI-KEY"       TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      * 20080206 転帰した病名が表示されてしまう不具合を修正START t.tanaka
      *     MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "key10"               TO  MCP-PATHNAME
      * 20080206 転帰した病名が表示されてしまう不具合を修正END
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                       MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                       SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     CALL    "ORCMCPSUB"          USING
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                        MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                        MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                        SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO            TO  FLG-HKNJAINF
           ELSE
               INITIALIZE              HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Otake(2007/09/21) start
      *     CALL    "ORCMCPSUB"          USING
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Otake(2007/09/21) end
                                        MCPAREA
      *=== 4.0対応 By Otake(2007/09/21) start
      *                                  ORCMCPAREA
      *=== 4.0対応 By Otake(2007/09/21) end
                                        MCPDATA-REC
      *=== 4.0対応 By Otake(2007/09/21) start
                                        SPA-AREA
      *=== 4.0対応 By Otake(2007/09/21) end
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
