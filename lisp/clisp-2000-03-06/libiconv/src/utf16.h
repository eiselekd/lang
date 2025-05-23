/*
 * UTF-16
 */

/* Here we accept FFFE/FEFF marks as endianness indicators everywhere
   in the stream, not just at the beginning. The default is big-endian. */
/* The state is 0 if big-endian, 1 if little-endian. */
static int
utf16_mbtowc (conv_t conv, wchar_t *pwc, const unsigned char *s, int n)
{
  state_t state = conv->istate;
  int count = 0;
  for (; n >= 2;) {
    wchar_t wc = (state ? s[0] + (s[1] << 8) : (s[0] << 8) + s[1]);
    if (wc == 0xfeff) {
    } else if (wc == 0xfffe) {
      state ^= 1;
    } else if (wc >= 0xd800 && wc < 0xdc00) {
      if (n >= 4) {
        wchar_t wc2 = (state ? s[2] + (s[3] << 8) : (s[2] << 8) + s[3]);
        if (!(wc2 >= 0xdc00 && wc2 < 0xe000))
          return RET_ILSEQ;
        *pwc = 0x10000 + ((wc - 0xd800) << 10) + (wc2 - 0xdc00);
        conv->istate = state;
        return count+4;
      } else
        break;
    } else if (wc >= 0xdc00 && wc < 0xe000) {
      return RET_ILSEQ;
    } else {
      *pwc = wc;
      conv->istate = state;
      return count+2;
    }
    s += 2; n -= 2; count += 2;
  }
  conv->istate = state;
  return RET_TOOFEW(count);
}

/* But we output UTF-16 in big-endian order, without byte-order mark. */
static int
utf16_wctomb (conv_t conv, unsigned char *r, wchar_t wc, int n)
{
  if (wc != 0xfffe) {
    if (wc < 0x10000) {
      if (n >= 2) {
        r[0] = (unsigned char) (wc >> 8);
        r[1] = (unsigned char) wc;
        return 2;
      } else
        return RET_TOOSMALL;
    }
    else if (wc < 0x110000) {
      if (n >= 4) {
        wchar_t wc1 = 0xd800 + ((wc - 0x10000) >> 10);
        wchar_t wc2 = 0xdc00 + ((wc - 0x10000) & 0x3ff);
        r[0] = (unsigned char) (wc1 >> 8);
        r[1] = (unsigned char) wc1;
        r[2] = (unsigned char) (wc2 >> 8);
        r[3] = (unsigned char) wc2;
        return 4;
      } else
        return RET_TOOSMALL;
    }
  }
  return RET_ILSEQ;
}
