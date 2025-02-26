
/*
 * ISO-2022-JP
 */

/* Specification: RFC 1468 */

#define ESC 0x1b

/*
 * The state can be one of the following values.
 */
#define STATE_ASCII          0
#define STATE_JISX0201ROMAN  1
#define STATE_JISX0208       2

static int
iso2022_jp_mbtowc (conv_t conv, wchar_t *pwc, const unsigned char *s, int n)
{
  state_t state = conv->istate;
  int count = 0;
  unsigned char c;
  for (;;) {
    c = *s;
    if (c == ESC) {
      if (n < count+3)
        goto none;
      if (s[1] == '(') {
        if (s[2] == 'B') {
          state = STATE_ASCII;
          s += 3; count += 3;
          if (n < count+1)
            goto none;
          continue;
        }
        if (s[2] == 'J') {
          state = STATE_JISX0201ROMAN;
          s += 3; count += 3;
          if (n < count+1)
            goto none;
          continue;
        }
        return RET_ILSEQ;
      }
      if (s[1] == '$') {
        if (s[2] == '@' || s[2] == 'B') {
          /* We don't distinguish JIS X 0208-1978 and JIS X 0208-1983. */
          state = STATE_JISX0208;
          s += 3; count += 3;
          if (n < count+1)
            goto none;
          continue;
        }
        return RET_ILSEQ;
      }
      return RET_ILSEQ;
    }
    break;
  }
  switch (state) {
    case STATE_ASCII:
      if (c < 0x80) {
        int ret = ascii_mbtowc(conv,pwc,s,1);
        if (ret == RET_ILSEQ)
          return RET_ILSEQ;
        if (ret != 1) abort();
        conv->istate = state;
        return count+1;
      } else
        return RET_ILSEQ;
    case STATE_JISX0201ROMAN:
      if (c < 0x80) {
        int ret = jisx0201_mbtowc(conv,pwc,s,1);
        if (ret == RET_ILSEQ)
          return RET_ILSEQ;
        if (ret != 1) abort();
        conv->istate = state;
        return count+1;
      } else
        return RET_ILSEQ;
    case STATE_JISX0208:
      if (n < count+2)
        goto none;
      if (s[0] < 0x80 && s[1] < 0x80) {
        int ret = jisx0208_mbtowc(conv,pwc,s,2);
        if (ret == RET_ILSEQ)
          return RET_ILSEQ;
        if (ret != 2) abort();
        conv->istate = state;
        return count+2;
      } else
        return RET_ILSEQ;
    default: abort();
  }

none:
  conv->istate = state;
  return RET_TOOFEW(count);
}

static int
iso2022_jp_wctomb (conv_t conv, unsigned char *r, wchar_t wc, int n)
{
  state_t state = conv->ostate;
  unsigned char buf[2];
  int ret;

  /* Try ASCII. */
  ret = ascii_wctomb(conv,buf,wc,1);
  if (ret != RET_ILSEQ) {
    if (ret != 1) abort();
    if (buf[0] < 0x80) {
      int count = (state == STATE_ASCII ? 1 : 4);
      if (n < count)
        return RET_TOOSMALL;
      if (state != STATE_ASCII) {
        r[0] = ESC;
        r[1] = '(';
        r[2] = 'B';
        r += 3;
        state = STATE_ASCII;
      }
      r[0] = buf[0];
      conv->ostate = state;
      return count;
    }
  }

  /* Try JIS X 0201-1976 Roman. */
  ret = jisx0201_wctomb(conv,buf,wc,1);
  if (ret != RET_ILSEQ) {
    if (ret != 1) abort();
    if (buf[0] < 0x80) {
      int count = (state == STATE_JISX0201ROMAN ? 1 : 4);
      if (n < count)
        return RET_TOOSMALL;
      if (state != STATE_JISX0201ROMAN) {
        r[0] = ESC;
        r[1] = '(';
        r[2] = 'J';
        r += 3;
        state = STATE_JISX0201ROMAN;
      }
      r[0] = buf[0];
      conv->ostate = state;
      return count;
    }
  }

  /* Try JIS X 0208-1990 in place of JIS X 0208-1978 and JIS X 0208-1983. */
  ret = jisx0208_wctomb(conv,buf,wc,2);
  if (ret != RET_ILSEQ) {
    if (ret != 2) abort();
    if (buf[0] < 0x80 && buf[1] < 0x80) {
      int count = (state == STATE_JISX0208 ? 2 : 5);
      if (n < count)
        return RET_TOOSMALL;
      if (state != STATE_JISX0208) {
        r[0] = ESC;
        r[1] = '$';
        r[2] = 'B';
        r += 3;
        state = STATE_JISX0208;
      }
      r[0] = buf[0];
      r[1] = buf[1];
      conv->ostate = state;
      return count;
    }
  }

  return RET_ILSEQ;
}

static int
iso2022_jp_reset (conv_t conv, unsigned char *r, int n)
{
  state_t state = conv->ostate;
  if (state != STATE_ASCII) {
    if (n < 3)
      return RET_TOOSMALL;
    r[0] = ESC;
    r[1] = '(';
    r[2] = 'B';
    /* conv->ostate = 0; will be done by the caller */
    return 3;
  } else
    return 0;
}

#undef STATE_JISX0208
#undef STATE_JISX0201ROMAN
#undef STATE_ASCII
