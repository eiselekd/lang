
/*
 * CP949 is EUC-KR, extended with UHC (Unified Hangul Code).
 */

#include "uhc_1.h"
#include "uhc_2.h"

static int
cp949_mbtowc (conv_t conv, wchar_t *pwc, const unsigned char *s, int n)
{
  unsigned char c = *s;
  /* Code set 0 (ASCII) */
  if (c < 0x80)
    return ascii_mbtowc(conv,pwc,s,n);
  /* UHC part 1 */
  if (c >= 0x81 && c <= 0xa0)
    return uhc_1_mbtowc(conv,pwc,s,n);
  if (c >= 0xa1 && c < 0xff) {
    if (n < 2)
      return RET_TOOFEW(0);
    {
      unsigned char c2 = s[1];
      if (c2 < 0xa1)
        /* UHC part 2 */
        return uhc_2_mbtowc(conv,pwc,s,n);
      else if (c2 < 0xff) {
        /* Code set 1 (KS C 5601-1992) */
        unsigned char buf[2];
        buf[0] = c-0x80; buf[1] = c2-0x80;
        return ksc5601_mbtowc(conv,pwc,buf,2);
      }
    }
  }
  return RET_ILSEQ;
}

static int
cp949_wctomb (conv_t conv, unsigned char *r, wchar_t wc, int n)
{
  unsigned char buf[2];
  int ret;

  /* Code set 0 (ASCII) */
  ret = ascii_wctomb(conv,r,wc,n);
  if (ret != RET_ILSEQ)
    return ret;

  /* Code set 1 (KS C 5601-1992) */
  ret = ksc5601_wctomb(conv,buf,wc,2);
  if (ret != RET_ILSEQ) {
    if (ret != 2) abort();
    if (n < 2)
      return RET_TOOSMALL;
    r[0] = buf[0]+0x80;
    r[1] = buf[1]+0x80;
    return 2;
  }

  /* UHC */
  if (wc >= 0xac00 && wc < 0xd7a4) {
    if (wc < 0xc8a5)
      return uhc_1_wctomb(conv,r,wc,n);
    else
      return uhc_2_wctomb(conv,r,wc,n);
  }

  return RET_ILSEQ;
}
