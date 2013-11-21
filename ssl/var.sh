CURDATE=`date +%Y%m%d`
CURHOST=server 
KEYFILE=$CURHOST-$CURDATE.key
CSRFILE=$CURHOST-$CURDATE.csr
CRTFILE=$CURHOST-$CURDATE.crt

openssl genrsa -out $KEYFILE 1024
chmod 600 $KEYFILE
openssl req -new -key $KEYFILE -out $CSRFILE
openssl x509 -req -days 365 -in $CSRFILE -signkey $KEYFILE -out $CRTFILE
