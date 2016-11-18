#!/bin/sh

. /etc/jma-receipt/jma-receipt.env

PARAMFILE=/tmp/a00000ra03.txt
#-------------------------------------------#
#        $1    処理ＰＧ（画面名）
#        $2    処理状況
#              1:追加  2:更新  3:削除
#        $3    システム日付
#        $4    診療日
#        $5    患者ＩＤ
#        $6    患者番号
#        $7    診療科
#        $8    ドクター
#        $9    保険組合せ
#        ${10} 処理・帳票名
#        ${11} プリンタ名
#        ${12} 医療機関番号
#        ${13} 伝票番号
#-------------------------------------------#
#

	echo	"処理ＰＧ（画面名）："$1 > $PARAMFILE
	echo	"処理状況："$2 >> $PARAMFILE
	echo    "システム日付："$3 >> $PARAMFILE
	echo    "診療日："$4 >> $PARAMFILE
	echo    "患者ＩＤ："$5 >> $PARAMFILE
	echo    "患者番号："$6 >> $PARAMFILE
	echo    "診療科："$7 >> $PARAMFILE
	echo    "ドクター："$8 >> $PARAMFILE
	echo    "保険組合せ："$9 >> $PARAMFILE
	echo    "処理・帳票名："${10} >> $PARAMFILE
	echo    "プリンタ名："${11} >> $PARAMFILE
	echo    "医療機関番号："${12} >> $PARAMFILE
	echo    "伝票番号："${13} >> $PARAMFILE

#
       $DBSTUB -dir $LDDEFDIR/directory -bd a00000ra03 A00000RA03 -parameter $1,$2,$3,$4,$5,$6,$7,$8,$9,${10},${11},${12},${13} > /home/orca/a00000ra03.log

#
	exit
