# from http://torsten-traenkner.de/linux/development/wireshark.php

while [ ! -e start.txt ];do
    sleep 0.1;
done;

(sleep 0.4;
 printf "\x00\x02\x00\x00"; sleep 1;
 printf "\x00\x03\x00\x04\x00\x12\x00\x00"; sleep 1;
 printf "\x00\x03\x00\x06\x00\x13\x00\x02\x42\x42"; sleep 2;
 printf "\x00\x04\x00\x00";sleep 1) | nc 127.0.0.1 9000

rm -f start.txt
