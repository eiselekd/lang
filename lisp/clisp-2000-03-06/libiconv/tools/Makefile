
# Directory containing conversion tables.
# You have to unpack the conversion-tables.tar.gz package.
TABLESDIR = /home/bruno/chs/conversion-tables

ALL = \
 ascii.h \
 iso8859_1.h \
 iso8859_2.h \
 iso8859_3.h \
 iso8859_4.h \
 iso8859_5.h \
 iso8859_6.h \
 iso8859_7.h \
 iso8859_8.h \
 iso8859_9.h \
 iso8859_10.h \
 iso8859_13.h \
 iso8859_14.h \
 iso8859_15.h \
 iso8859_16.h \
 koi8_r.h \
 koi8_u.h \
 koi8_ru.h \
 cp1250.h \
 cp1251.h \
 cp1252.h \
 cp1253.h \
 cp1254.h \
 cp1255.h \
 cp1256.h \
 cp1257.h \
 cp1258.h \
 cp850.h \
 cp866.h \
 mac_roman.h \
 mac_centraleurope.h \
 mac_iceland.h \
 mac_croatian.h \
 mac_romania.h \
 mac_cyrillic.h \
 mac_ukraine.h \
 mac_greek.h \
 mac_turkish.h \
 mac_hebrew.h \
 mac_arabic.h \
 mac_thai.h \
 hp_roman8.h \
 nextstep.h \
 armscii_8.h \
 georgian_academy.h \
 georgian_ps.h \
 mulelao.h \
 cp1133.h \
 tis620.h \
 cp874.h \
 viscii.h \
 tcvn.h \
 jisx0201.h \
 jisx0208.h \
 jisx0212.h \
 cp932ext.h \
 gb2312.h \
 isoir165ext.h \
 gb12345ext.h \
 gbkext1.h \
 gbkext2.h \
 gbkext_inv.h \
 cp936ext.h \
 cns11643_1.h \
 cns11643_2.h \
 cns11643_3.h \
 cns11643_inv.h \
 big5.h \
 cp950ext.h \
 ksc5601.h \
 uhc_1.h \
 uhc_2.h \
 johab_hangul.h \
 cjk_variants.h

all : $(ALL)

ascii.h : $(TABLESDIR)/glibc-2.1-iconv/ASCII.TXT 8bit_tab_to_h
	./8bit_tab_to_h ASCII ascii < $<

iso8859_1.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-1.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-1 iso8859_1 < $<

iso8859_2.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-2.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-2 iso8859_2 < $<

iso8859_3.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-3.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-3 iso8859_3 < $<

iso8859_4.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-4.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-4 iso8859_4 < $<

iso8859_5.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-5.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-5 iso8859_5 < $<

iso8859_6.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-6.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-6 iso8859_6 < $<

iso8859_7.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-7.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-7 iso8859_7 < $<

iso8859_8.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-8.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-8 iso8859_8 < $<

iso8859_9.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-9.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-9 iso8859_9 < $<

iso8859_10.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-10.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-10 iso8859_10 < $<

iso8859_13.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-13.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-13 iso8859_13 < $<

iso8859_14.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-14.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-14 iso8859_14 < $<

iso8859_15.h : $(TABLESDIR)/unicode.org-mappings/ISO8859/8859-15.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-15 iso8859_15 < $<

iso8859_16.h : $(TABLESDIR)/csets-new/ISO-8859-16.TXT 8bit_tab_to_h
	./8bit_tab_to_h ISO-8859-16 iso8859_16 < $<

koi8_r.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MISC/KOI8-R.TXT 8bit_tab_to_h
	./8bit_tab_to_h KOI8-R koi8_r < $<

koi8_u.h : $(TABLESDIR)/csets-new/KOI8-U.TXT 8bit_tab_to_h
	./8bit_tab_to_h KOI8-U koi8_u < $<

koi8_ru.h : $(TABLESDIR)/freebsd-iconv-0.4/KOI8-RU.TXT 8bit_tab_to_h
	./8bit_tab_to_h KOI8-RU koi8_ru < $<

cp1250.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1250.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP1250 cp1250 < $<

cp1251.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1251.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP1251 cp1251 < $<

cp1252.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1252.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP1252 cp1252 < $<

cp1253.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1253.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP1253 cp1253 < $<

cp1254.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1254.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP1254 cp1254 < $<

cp1255.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1255.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP1255 cp1255 < $<

cp1256.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1256.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP1256 cp1256 < $<

cp1257.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1257.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP1257 cp1257 < $<

cp1258.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP1258.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP1258 cp1258 < $<

cp850.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP850.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP850 cp850 < $<

cp866.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/PC/CP866.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP866 cp866 < $<

mac_roman.h : $(TABLESDIR)/jdk-1.1.7b/MAC-ROMAN.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacRoman mac_roman < $<

mac_centraleurope.h : $(TABLESDIR)/jdk-1.1.7b/MAC-CENTRAL-EUROPE.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacCentralEurope mac_centraleurope < $<

mac_iceland.h : $(TABLESDIR)/jdk-1.1.7b/MAC-ICELAND.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacIceland mac_iceland < $<

mac_croatian.h : $(TABLESDIR)/jdk-1.1.7b/MAC-CROATIAN.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacCroatian mac_croatian < $<

mac_romania.h : $(TABLESDIR)/jdk-1.1.7b/MAC-ROMANIA.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacRomania mac_romania < $<

mac_cyrillic.h : $(TABLESDIR)/jdk-1.1.7b/MAC-CYRILLIC.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacCyrillic mac_cyrillic < $<

mac_ukraine.h : $(TABLESDIR)/jdk-1.1.7b/MAC-UKRAINE.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacUkraine mac_ukraine < $<

mac_greek.h : $(TABLESDIR)/jdk-1.1.7b/MAC-GREEK.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacGreek mac_greek < $<

mac_turkish.h : $(TABLESDIR)/jdk-1.1.7b/MAC-TURKISH.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacTurkish mac_turkish < $<

mac_hebrew.h : $(TABLESDIR)/jdk-1.1.7b/MAC-HEBREW.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacHebrew mac_hebrew < $<

mac_arabic.h : $(TABLESDIR)/jdk-1.1.7b/MAC-ARABIC.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacArabic mac_arabic < $<

mac_thai.h : $(TABLESDIR)/jdk-1.1.7b/MAC-THAI.TXT 8bit_tab_to_h
	./8bit_tab_to_h MacThai mac_thai < $<

hp_roman8.h : $(TABLESDIR)/glibc-2.1-iconv/HP-ROMAN8.TXT 8bit_tab_to_h
	./8bit_tab_to_h HP-ROMAN8 hp_roman8 < $<

nextstep.h : $(TABLESDIR)/clisp/NEXTSTEP.TXT 8bit_tab_to_h
	./8bit_tab_to_h NEXTSTEP nextstep < $<

armscii_8.h : $(TABLESDIR)/csets-1.6/AST166-8.TXT 8bit_tab_to_h
	./8bit_tab_to_h ARMSCII-8 armscii_8 < $<

georgian_academy.h : $(TABLESDIR)/csets-1.6/GEO-ITA.TXT 8bit_tab_to_h
	./8bit_tab_to_h GEORGIAN-ACADEMY georgian_academy < $<

georgian_ps.h : $(TABLESDIR)/csets-1.6/GEO-PS.TXT 8bit_tab_to_h
	./8bit_tab_to_h GEORGIAN-PS georgian_ps < $<

mulelao.h : $(TABLESDIR)/csets-new/MULELAO1.TXT 8bit_tab_to_h
	./8bit_tab_to_h MULELAO-1 mulelao < $<

cp1133.h : $(TABLESDIR)/csets-new/CP1133.TXT 8bit_tab_to_h
	./8bit_tab_to_h IBM-CP1133 cp1133 < $<

tis620.h : $(TABLESDIR)/glibc-2.1-iconv/TIS-620.TXT 8bit_tab_to_h
	./8bit_tab_to_h TIS620.2533-1 tis620 < $<

cp874.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP874.TXT 8bit_tab_to_h
	./8bit_tab_to_h CP874 cp874 < $<

viscii.h : $(TABLESDIR)/csets-1.6/VISCII.TXT 8bit_tab_to_h
	./8bit_tab_to_h VISCII1.1-1 viscii < $<

tcvn.h : $(TABLESDIR)/csets-1.6/VN5712-1.TXT 8bit_tab_to_h
	./8bit_tab_to_h TCVN-5712 tcvn < $<

jisx0201.h : $(TABLESDIR)/clisp/JIS_X0201.TXT 8bit_tab_to_h
	./8bit_tab_to_h JISX0201.1976-0 jisx0201 < $<

jisx0208.h : $(TABLESDIR)/unicode.org-mappings/EASTASIA/JIS/JIS0208.TXT cjk_tab_to_h
	sed -e 's,^0x....[ 	]*\(0x....[ 	]*0x.....*\)$$,\1,' < $< | ./cjk_tab_to_h JISX0208.1990-0 jisx0208 > $@

jisx0212.h : $(TABLESDIR)/unicode.org-mappings/EASTASIA/JIS/JIS0212.TXT cjk_tab_to_h
	./cjk_tab_to_h JISX0212.1990-0 jisx0212 < $< > $@

cp932ext.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP932.TXT cjk_tab_to_h
	(echo '#' ; grep '^0x\(87\|E[D-F]\|F[0-9A-F]\)[0-9A-F][0-9A-F]' < $< ) | ./cjk_tab_to_h 'CP932 extensions' cp932ext > $@

gb2312.h : $(TABLESDIR)/unicode.org-mappings/EASTASIA/GB/GB2312.TXT cjk_tab_to_h
	./cjk_tab_to_h GB2312.1980-0 gb2312 < $< > $@

isoir165ext.h : $(TABLESDIR)/csets-new/ISO-IR-165-additions.TXT cjk_tab_to_h
	(echo '#' ; grep -v '^0x2[8A]' < $<) | ./cjk_tab_to_h 'ISO-IR-165 extensions' isoir165ext > $@

gb12345ext.h : $(TABLESDIR)/unicode.org-mappings/EASTASIA/GB/GB2312.TXT $(TABLESDIR)/unicode.org-mappings/EASTASIA/GB/GB12345-FIXED.TXT $(TABLESDIR)/csets-new/GB12345-more.TXT cjk_tab_to_h
	(echo '#' ; sed -e '/^0x..[ 	]*#UNDEFINED/d' -e 's/#.*$$//' -e '/^[ 	]*$$/d' $(TABLESDIR)/unicode.org-mappings/EASTASIA/GB/GB2312.TXT > tmp1.TXT ; sed -e '/^0x..[ 	]*#UNDEFINED/d' -e 's/#.*$$//' -e '/^[ 	]*$$/d' $(TABLESDIR)/unicode.org-mappings/EASTASIA/GB/GB12345-FIXED.TXT > tmp2.TXT ; diff tmp1.TXT tmp2.TXT | grep '^> ' | sed -e 's,^> ,,' ; cat $(TABLESDIR)/csets-new/GB12345-more.TXT ) | ./cjk_tab_to_h 'GB/T 12345.1990-0 extensions' gb12345ext > $@ && rm -f tmp1.TXT tmp2.TXT

gbkext1.h : GBKext.TXT cjk_tab_to_h
	(echo '#' ; grep '^0x\([89].\|A0\)' < GBKext.TXT) | ./cjk_tab_to_h 'GBK/3 extensions' gbkext1 > $@

gbkext2.h : GBKext.TXT cjk_tab_to_h
	(echo '#' ; grep '^0x\(A[1-9A-F]\|[B-F][0-9A-F]\)\([4-9].\|A0\)' < GBKext.TXT) | ./cjk_tab_to_h 'GBK/4 and GBK/5 extensions' gbkext2 > $@

gbkext_inv.h : GBKext.TXT cjk_tab_to_h
	(echo '#' ; grep '^0x\([89].\|A0\)' < GBKext.TXT ; grep '^0x\(A[1-9A-F]\|[B-F][0-9A-F]\)\([4-9].\|A0\)' < GBKext.TXT) | ./cjk_tab_to_h 'GBK extensions' gbkext_inv > $@

cp936ext.h : GBKext.TXT cjk_tab_to_h
	(echo '#' ; grep '^0xA[1-9][B-F].' < GBKext.TXT) | ./cjk_tab_to_h 'CP936 extensions' cp936ext > $@

GBKext.TXT : $(TABLESDIR)/unicode.org-mappings/EASTASIA/GB/GB2312.TXT $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP936.TXT
	sed -e 's,^0x2,0xA,' -e 's,^0x3,0xB,' -e 's,^0x4,0xC,' -e 's,^0x5,0xD,' -e 's,^0x6,0xE,' -e 's,^0x7,0xF,' -e 's,^0x\(..\)2,0x\1A,' -e 's,^0x\(..\)3,0x\1B,' -e 's,^0x\(..\)4,0x\1C,' -e 's,^0x\(..\)5,0x\1D,' -e 's,^0x\(..\)6,0x\1E,' -e 's,^0x\(..\)7,0x\1F,' $(TABLESDIR)/unicode.org-mappings/EASTASIA/GB/GB2312.TXT > tmpEUCCN.TXT ; sed -e '/^0x..[ 	]*#UNDEFINED/d' -e 's/#.*$$//' -e '/^[ 	]*$$/d' tmpEUCCN.TXT > tmp1.TXT ; sed -e '/^0x..[ 	]/d' -e 's/#.*$$//' -e '/^[ 	]*$$/d' $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP936.TXT > tmp2.TXT ; diff -c1 tmp1.TXT tmp2.TXT | grep '^+ ' | sed -e 's,^+ ,,' > $@ && rm -f tmpEUCCN.TXT tmp1.TXT tmp2.TXT

cns11643_1.h : $(TABLESDIR)/csets-new/CNS11643.TXT cjk_tab_to_h
	grep '^\(#\|0x1\)' < $< | sed -e 's,^0x1,0x,' | ./cjk_tab_to_h 'CNS 11643-1986 plane 1' cns11643_1 > $@

cns11643_2.h : $(TABLESDIR)/csets-new/CNS11643.TXT cjk_tab_to_h
	grep '^\(#\|0x2\)' < $< | sed -e 's,^0x2,0x,' | ./cjk_tab_to_h 'CNS 11643-1986 plane 2' cns11643_2 > $@

cns11643_3.h : $(TABLESDIR)/csets-new/CNS11643.TXT cjk_tab_to_h
	grep '^\(#\|0x3\)' < $< | sed -e 's,^0x3,0x,' | ./cjk_tab_to_h 'CNS 11643-1986 plane 14 part 1' cns11643_3 > $@

cns11643_inv.h : $(TABLESDIR)/csets-new/CNS11643.TXT cjk_tab_to_h
	./cjk_tab_to_h 'CNS 11643-1992 planes 1-3' cns11643_inv < $< > $@

big5.h : $(TABLESDIR)/unicode.org-mappings/EASTASIA/OTHER/BIG5.TXT cjk_tab_to_h
	./cjk_tab_to_h BIG5 big5 < $< > $@

cp950ext.h : $(TABLESDIR)/unicode.org-mappings/VENDORS/MICSFT/WINDOWS/CP950.TXT cjk_tab_to_h
	(echo '#' ; grep '^0xF9\(D[6-9A-F]\|[EF][0-9A-F]\)' < $< ) | ./cjk_tab_to_h 'CP950 extensions' cp950ext > $@

ksc5601.h : $(TABLESDIR)/unicode.org-mappings/EASTASIA/KSC/KSC5601.TXT cjk_tab_to_h
	./cjk_tab_to_h KSC5601.1987-0 ksc5601 < $< > $@

uhc_1.h : $(TABLESDIR)/unicode.org-mappings/EASTASIA/KSC/KSC5601.TXT cjk_tab_to_h
	(echo '#' ; grep '^0x\([8-9]\|A0\)' < $< ) | ./cjk_tab_to_h 'Unified Hangul Code part 1' uhc_1 > $@

uhc_2.h : $(TABLESDIR)/unicode.org-mappings/EASTASIA/KSC/KSC5601.TXT cjk_tab_to_h
	(echo '#' ; grep '^0x\(A[1-9A-F]\|[B-F][0-9A-F]\)\([0-9]\|A0\)' < $< ) | ./cjk_tab_to_h 'Unified Hangul Code part 2' uhc_2 > $@

johab_hangul.h : $(TABLESDIR)/unicode.org-mappings/EASTASIA/KSC/JOHAB.TXT cjk_tab_to_h
	(echo '#' ; grep '^0x\([8-C]...\|D[0-7]..\)' < $< ) | ./cjk_tab_to_h 'JOHAB Hangul' johab_hangul > $@

cjk_variants.h : $(TABLESDIR)/yasuoka/UniVariants cjk_variants
	./cjk_variants < $< > $@


8bit_tab_to_h : 8bit_tab_to_h.c
	gcc -O -Wall -g $< -o $@

cjk_tab_to_h : cjk_tab_to_h.c
	gcc -O -Wall -g $< -o $@

cjk_variants : cjk_variants.c
	gcc -O -Wall -g $< -o $@


clean : force
	rm -f 8bit_tab_to_h cjk_tab_to_h cjk_variants $(ALL) GBKext.TXT

force :

