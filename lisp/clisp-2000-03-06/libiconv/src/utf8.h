/*
 * UTF-8
 */

/* Specification: RFC 2279 */

static int
utf8_mbtowc (conv_t conv, wchar_t *pwc, const unsigned char *s, int n)
{
  unsigned char c = s[0];

  if (c < 0x80) {
    *pwc = c;
    return 1;
  } else if (c < 0xc2) {
    return RET_ILSEQ;
  } else if (c < 0xe0) {
    if (n < 2)
      return RET_TOOFEW(0);
    if (!((s[1] ^ 0x80) < 0x40))
      return RET_ILSEQ;
    *pwc = ((wchar_t) (c & 0x1f) << 6)
           | (wchar_t) (s[1] ^ 0x80);
    return 2;
  } else if (c < 0xf0) {
    if (n < 3)
      return RET_TOOFEW(0);
    if (!((s[1] ^ 0x80) < 0x40 && (s[2] ^ 0x80) < 0x40
          && (c >= 0xe1 || s[1] >= 0xa0)))
      return RET_ILSEQ;
    *pwc = ((wchar_t) (c & 0x0f) << 12)
           | ((wchar_t) (s[1] ^ 0x80) << 6)
           | (wchar_t) (s[2] ^ 0x80);
    return 3;
  } else if (c < 0xf8 && sizeof(wchar_t)*8 >= 32) {
    if (n < 4)
      return RET_TOOFEW(0);
    if (!((s[1] ^ 0x80) < 0x40 && (s[2] ^ 0x80) < 0x40
          && (s[3] ^ 0x80) < 0x40
          && (c >= 0xf1 || s[1] >= 0x90)))
      return RET_ILSEQ;
    *pwc = ((wchar_t) (c & 0x07) << 18)
           | ((wchar_t) (s[1] ^ 0x80) << 12)
           | ((wchar_t) (s[2] ^ 0x80) << 6)
           | (wchar_t) (s[3] ^ 0x80);
    return 4;
  } else if (c < 0xfc && sizeof(wchar_t)*8 >= 32) {
    if (n < 5)
      return RET_TOOFEW(0);
    if (!((s[1] ^ 0x80) < 0x40 && (s[2] ^ 0x80) < 0x40
          && (s[3] ^ 0x80) < 0x40 && (s[4] ^ 0x80) < 0x40
          && (c >= 0xf9 || s[1] >= 0x88)))
      return RET_ILSEQ;
    *pwc = ((wchar_t) (c & 0x03) << 24)
           | ((wchar_t) (s[1] ^ 0x80) << 18)
           | ((wchar_t) (s[2] ^ 0x80) << 12)
           | ((wchar_t) (s[3] ^ 0x80) << 6)
           | (wchar_t) (s[4] ^ 0x80);
    return 5;
  } else if (c < 0xfe && sizeof(wchar_t)*8 >= 32) {
    if (n < 6)
      return RET_TOOFEW(0);
    if (!((s[1] ^ 0x80) < 0x40 && (s[2] ^ 0x80) < 0x40
          && (s[3] ^ 0x80) < 0x40 && (s[4] ^ 0x80) < 0x40
          && (s[5] ^ 0x80) < 0x40
          && (c >= 0xfd || s[1] >= 0x84)))
      return RET_ILSEQ;
    *pwc = ((wchar_t) (c & 0x01) << 30)
           | ((wchar_t) (s[1] ^ 0x80) << 24)
           | ((wchar_t) (s[2] ^ 0x80) << 18)
           | ((wchar_t) (s[3] ^ 0x80) << 12)
           | ((wchar_t) (s[4] ^ 0x80) << 6)
           | (wchar_t) (s[5] ^ 0x80);
    return 6;
  } else
    return RET_ILSEQ;
}

static int
utf8_wctomb (conv_t conv, unsigned char *r, wchar_t wc, int n) /* n == 0 is acceptable */
{
  int count;
  if (wc < 0x80)
    count = 1;
  else if (wc < 0x800)
    count = 2;
  else if (wc < 0x10000)
    count = 3;
  else if (wc < 0x200000)
    count = 4;
  else if (wc < 0x4000000)
    count = 5;
  else if (wc <= 0x7fffffff)
    count = 6;
  else
    return RET_ILSEQ;
  if (n < count)
    return RET_TOOSMALL;
  switch (count) { /* note: code falls through cases! */
    case 6: r[5] = 0x80 | (wc & 0x3f); wc = wc >> 6; wc |= 0x4000000;
    case 5: r[4] = 0x80 | (wc & 0x3f); wc = wc >> 6; wc |= 0x200000;
    case 4: r[3] = 0x80 | (wc & 0x3f); wc = wc >> 6; wc |= 0x10000;
    case 3: r[2] = 0x80 | (wc & 0x3f); wc = wc >> 6; wc |= 0x800;
    case 2: r[1] = 0x80 | (wc & 0x3f); wc = wc >> 6; wc |= 0xc0;
    case 1: r[0] = wc;
  }
  return count;
}
