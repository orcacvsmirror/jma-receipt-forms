#!/bin/sh
#########################################################################
#
# Ģɼ���󥹥ȡ���ġ���
# (�Ť�Ģɼ�κƥ��󥹥ȡ��롦���ԡ���������ݤ��ʤ��ġ���)
#
# update
# 	2003.09.27
# 		�������� (K.Togazono)
# 	2003.10.07
#		CBL�ե�����򥳥ԡ����ʤ��Х�������
# 		/usr/lib/jma-receipt/site-lib�����
# 		�ե����륳�ԡ��򥳥��ȥ����ȡ�(T.Onuki)
#
#########################################################################


#----------------
# ��� 
#----------------

DIRTYOHYO=$1

DIRCBL=cobol
DIRCPY=${DIRCBL}/copy
DIRFORM=form
DIRREC=record

SRCBASE=/usr/local/site-jma-receipt
INSBASE=/usr/lib/jma-receipt/site-lib
INSCOPY=/usr/lib/jma-receipt/

PRGNAME=

FILESO=
FILECBL=
FILEINC=
#(��.red��ޤ�)
FILERED=
#(��.red�ξ��϶�)
FILEDEF=


#----------------
# ����
#----------------

# Etch�ǻ��Ѥ��륳����UTF-8�ؤ��Ѵ�
# echo message (default character-code EUC)
function echomsg() {
  if [ "${LANG}" = "ja_JP.UTF-8" ] || [ "${LANG}" = "ja_JP.UTF8" ]; then
    if [ -z $2 ]; then
      echo `echo $1 | nkf -wE`
    else
      echo -n `echo $1 | nkf -wE`
    fi 
  else
    if [ -z $2 ]; then
      echo $1
    else
      echo -n $1
    fi
  fi
  return 0
}

# root�桼���Ǥʤ��Ȥ���
if test `whoami` != "root"
then
	echomsg "root�桼���Ǽ¹Ԥ��Ƥ�������"
	exit
fi

# Ģɼ�ץ����Ϥɤ���
if test "$DIRTYOHYO" = ""
then
#	echo "�裱������Ģɼ�ץ��������Хѥ�����ꤷ�Ƥ�������"
#	exit
        DIRTYOHYO=./
fi

if ! test -d $DIRTYOHYO
then
	echomsg "Ģɼ�ץ���ब���դ���ޤ��󡣥ѥ����ְ�äƤޤ��󤫡�"
	exit
fi
# (�ѥ��κǸ��"/"�򤯤äĤ��Ƥ���������)
DIRTYOHYO=`echo "$DIRTYOHYO" | sed -e "s/\/$//"`

# "*.CBL"�Ϥ��뤫�� 
FILECBL=`find $DIRTYOHYO -name "*.CBL" -print 2>/dev/null`
if test "$FILECBL" = ""
then
	echomsg "\"*.CBL\"�ե����뤬���դ���ޤ���"
	exit
fi
FILECBL=`echo $FILECBL | sed -e "s/.*\///"`

# �ץ����̾�򽦤äƤ���
PRGNAME=`echo $FILECBL | sed -e "s/\.CBL//"`

# "*.INC"�Ϥ��뤫�� 
FILEINC=`find $DIRTYOHYO -name "*.INC" -print 2>/dev/null`
if test "$FILEINC" = ""
then
	echo "\"*.INC\"�ե����뤬���դ���ޤ���"
	exit
fi
FILEINC=`echo $FILEINC | sed -e "s/.*\///"`

# "*.dia"�ޤ���"*.red"�Ϥ��뤫�� 
FILERED=`find $DIRTYOHYO -name "*.red" -print 2>/dev/null`
if test "$FILERED" = ""
then
	FILERED=`find $DIRTYOHYO -name "*.dia" -print 2>/dev/null`
	if test "$FILERED" = ""
	then
		echo "\"*.dia\"�ޤ���\"*.red\"�ե����뤬���դ���ޤ���"
		exit
	fi
	# "*.dia"�ϡ�"*.def"�ե��������äƤ���
	FILEDEF=`find $DIRTYOHYO -name "*.def" -print 2>/dev/null`
	FILEDEF=`echo $FILEDEF | sed -e "s/.*\///"`
fi
FILERED=`echo $FILERED | sed -e "s/.*\///"`


# �ե�����ι���������ǧ����
# (���졩���ԡ����Ƥ롩�ޤ���������)
if ! find ${SRCBASE}/${DIRCBL}/${FILECBL} 2>/dev/null -o \
	find ${DIRTYOHYO}/${FILECBL} -newer ${SRCBASE}/${DIRCBL}/${FILECBL} 2>/dev/null
then
	echo "cp ${DIRTYOHYO}/${FILECBL} ${SRCBASE}/${DIRCBL}"
	if ! cp ${DIRTYOHYO}/${FILECBL} ${SRCBASE}/${DIRCBL}
        then
             exit
        fi
fi

if ! find ${SRCBASE}/${DIRCPY}/${FILEINC} 2>/dev/null -o \
	find ${DIRTYOHYO}/${FILEINC} -newer ${SRCBASE}/${DIRCPY}/${FILEINC} 2>/dev/null
then
	echo "cp ${DIRTYOHYO}/${FILEINC} ${SRCBASE}/${DIRCPY}"
	if ! cp ${DIRTYOHYO}/*.INC ${SRCBASE}/${DIRCPY}
        then
             exit
        fi
fi

if ! find ${SRCBASE}/${DIRFORM}/${FILERED} 2>/dev/null -o \
	find ${DIRTYOHYO}/${FILERED} -newer ${SRCBASE}/${DIRFORM}/${FILERED} 2>/dev/null
then
	echo "cp ${DIRTYOHYO}/${FILERED} ${SRCBASE}/${DIRFORM}"
	if ! cp ${DIRTYOHYO}/*.red ${SRCBASE}/${DIRFORM}
        then
             exit
        fi
fi

if test "$FILEDEF" != ""
then
	if ! find ${SRCBASE}/${DIRREC}/${FILEDEF} 2>/dev/null -o \
		find ${DIRTYOHYO}/${FILEDEF} -newer ${SRCBASE}/${DIRREC}/${FILEDEF} 2>/dev/null
	then
		echo "cp ${DIRTYOHYO}/${FILEDEF} ${SRCBASE}/${DIRREC}"
		if ! cp ${DIRTYOHYO}/*.def ${SRCBASE}/${DIRREC}
                then
                     exit
                fi
	fi
fi


# �Ť��ե������������
rm -f ${INSCOPY}/${PRGNAME}.so
# (��ʸ���äƤ��ȤϤʤ����ɡ�����Ȥϸ¤�ʤ�������)
rm -f ${INSCOPY}/${PRGNAME}.SO
# (�Ť��С������ϡ�*.red����ʤ��ơ�*.dia�����äƤ����ǽ�������롩)
# (�� *.red/*.dia�γ�ĥ�Ҥ���ʧ�äƤ����)
OLDREDNAME=`echo $FILERED | sed -e "s/\..*" 2>/dev/null`
rm -f ${INSCOPY}/${DIRFORM}/${OLDREDNAME}.dia
rm -f ${INSCOPY}/${DIRREC}/${OLDREDNAME}.def


# ����ѥ����¹Ԥ���
sh /usr/lib/jma-receipt/scripts/allways/site-upgrade.sh
if test $? -ne 0
then
	echomsg "����ѥ���˼��Ԥ��ޤ������������Ƽ¹Ԥ��뤫����ư�����ꤷ�Ƥ���������"
	exit
fi


# ���ԡ����ʤ��Ƥ��ɤ���Τ⤢��Τǡ�
#echo "�Ƕ�Υץ����ϡ�${INSCOPY}�� *.so���򥳥ԡ����ʤ��Ƥ��ɤ���Τ�����ޤ���"
#echo "�ܤ����ϡ���Ģɼ�ץ����� README�򻲾Ȥ��Ʋ�������"
#echo -n "����ѥ��뤷���ե������ ${INSCOPY}�˥��ԡ����ޤ�����[Y/n]: "
#read ANS
#if test "$ANS" = "n" -o "$ANS" = "N"
#then
#	echo "*.so �ե��������򥳥ԡ������˽�λ���ޤ���"	
#	exit
#fi


# �ե�����򥳥ԡ�����
#cp ${INSBASE}/${PRGNAME}.so ${INSCOPY}
#cp ${INSBASE}/${DIRFORM}/${FILERED} ${INSCOPY}/${DIRFORM}

#if test "$FILEDEF" != ""
#then
#	cp ${INSBASE}/${DIRREC}/${FILEDEF} ${INSCOPY}/${DIRREC}
#fi


echomsg "Ģɼ�ץ����Υ��󥹥ȡ����λ���ޤ�"
exit
