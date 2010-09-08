#!/bin/bash

. /etc/jma-receipt/jma-receipt.env

usrname=`whoami`

# Etch�ǻ��Ѥ��륳����UTF-8�ؤ��Ѵ�
# echo message (default character-code EUC)
function echomsg() {
  if [ "${LANG}" = "ja_JP.UTF-8" ] || [ "${LANG}" = "ja_JP.UTF8" ] ||
     [ "${LANG}" = "ja_JP.utf8" ]; then
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

# check user
if [ $usrname != "root" ] ; then
  echomsg "root�桼���Ǽ¹Ԥ��Ƥ�������"
  exit 1
fi

# start message
echomsg "�ץ���� ���ץ���� ���곫��..."

# check redirector
if [ -f /etc/default/jma-receipt ] ; then
  . /etc/default/jma-receipt
fi

if [ "$RUN_REDIRECTOR" = true ] ; then
  DBRED=`ps ax | grep dbredirector | grep -v grep`
  if [ -z "$DBRED" ] ; then
    echomsg "������쥯������ư���Ƥ�������"
    exit 1
  fi
fi

# program main
if test -z "$JMARECEIPT_ENV"; then
    JMARECEIPT_ENV="/etc/jma-receipt/jma-receipt.env"
fi
if ! test -f "$JMARECEIPT_ENV"; then
    exit 0
fi

. $JMARECEIPT_ENV

export APS_EXEC_PATH=`pwd`

if [ $# -ne 1 ]; then
  echomsg "�ѥ�᡼�� <hospnum> ����ꤷ�Ƥ�������"
  exit 1
else
  HOSPNUM="$1"
fi

ln -s $SYSCONFDIR/dbgroup.inc dbgroup.inc

su orca -c "$DBSTUB -bd prgoption Prgoption -parameter $HOSPNUM"

rm dbgroup.inc

# end message
sync
echomsg "�ץ���� ���ץ���� ���꽪λ!!"
exit 0
