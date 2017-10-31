// A temporary test file
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define CYC_UTF8_ACCEPT 0

// Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
static const uint8_t utf8d[] = {
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 00..1f
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 20..3f
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 40..5f
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 60..7f
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, // 80..9f
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, // a0..bf
  8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, // c0..df
  0xa,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x4,0x3,0x3, // e0..ef
  0xb,0x6,0x6,0x6,0x5,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8, // f0..ff
  0x0,0x1,0x2,0x3,0x5,0x8,0x7,0x1,0x1,0x1,0x4,0x6,0x1,0x1,0x1,0x1, // s0..s0
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1, // s1..s2
  1,2,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1, // s3..s4
  1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,3,1,1,1,1,1,1, // s5..s6
  1,3,1,1,1,1,1,3,1,3,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // s7..s8
};

//uint32_t inline
uint32_t Cyc_utf8_decode(uint32_t* state, uint32_t* codep, uint32_t byte) {
  uint32_t type = utf8d[byte];

  *codep = (*state != CYC_UTF8_ACCEPT) ?
    (byte & 0x3fu) | (*codep << 6) :
    (0xff >> type) & (byte);

  *state = utf8d[256 + *state*16 + type];
  return *state;
}

/**
 * Simple macro to make it more convenient to convert a single char
 */
#define Cyc_utf8_encode_char(dest, dest_size, char_value) \
  Cyc_utf8_encode(dest, dest_size, &char_value, 1)

/**
 * This function takes one or more 32-bit chars and encodes them 
 * as an array of UTF-8 bytes.
 * FROM: https://www.cprogramming.com/tutorial/utf8.c
 *
 * @param dest    Destination byte buffer
 * @param sz      size of dest buffer in bytes
 * @param src     Buffer of source data, in 32-bit characters
 * @param srcsz   number of source characters, or -1 if 0-terminated
 *
 * @return Number of characters converted
 *
 * dest will only be '\0'-terminated if there is enough space. this is
 * for consistency; imagine there are 2 bytes of space left, but the next
 * character requires 3 bytes. in this case we could NUL-terminate, but in
 * general we can't when there's insufficient space. therefore this function
 * only NUL-terminates if all the characters fit, and there's space for
 * the NUL as well.
 * the destination string will never be bigger than the source string.
 */
int Cyc_utf8_encode(char *dest, int sz, uint32_t *src, int srcsz)
{
    u_int32_t ch;
    int i = 0;
    char *dest_end = dest + sz;

    while (srcsz<0 ? src[i]!=0 : i < srcsz) {
        ch = src[i];
        if (ch < 0x80) {
            if (dest >= dest_end)
                return i;
            *dest++ = (char)ch;
        }
        else if (ch < 0x800) {
            if (dest >= dest_end-1)
                return i;
            *dest++ = (ch>>6) | 0xC0;
            *dest++ = (ch & 0x3F) | 0x80;
        }
        else if (ch < 0x10000) {
            if (dest >= dest_end-2)
                return i;
            *dest++ = (ch>>12) | 0xE0;
            *dest++ = ((ch>>6) & 0x3F) | 0x80;
            *dest++ = (ch & 0x3F) | 0x80;
        }
        else if (ch < 0x110000) {
            if (dest >= dest_end-3)
                return i;
            *dest++ = (ch>>18) | 0xF0;
            *dest++ = ((ch>>12) & 0x3F) | 0x80;
            *dest++ = ((ch>>6) & 0x3F) | 0x80;
            *dest++ = (ch & 0x3F) | 0x80;
        }
        i++;
    }
    if (dest < dest_end)
        *dest = '\0';
    return i;
}

void encode(uint32_t val) {
  char dest[5];
  int rv, i;

  rv = Cyc_utf8_encode_char(dest, 5, val);
  printf("%x %d \n", val, rv);
  for(i = 0; i < 5; i++) {
    printf("[%x] ", (uint8_t)dest[i]);
  }
  printf("\n");
  return;
}

void multi_byte_memset(char *buf, int blen, char *src, int slen)
{
  int bi, si;
  for (bi = 0, si = 0; bi < blen; bi++, si++) {
    buf[bi] = src[si % slen];
  }
}

void substring(int s, int e, const char *expected) {
  uint8_t raw[] = {65, 66, 0xCE, 0xBB, 67};
    const char *tmp = raw;
    uint32_t codepoint;
    uint32_t state = 0;
    int num_ch, cur_ch_bytes = 0, start_i = 0, end_i = 0;
    for (num_ch = 0; *tmp; ++tmp){
      //printf("char = %d\n", (int)*tmp);
      if (!Cyc_utf8_decode(&state, &codepoint, (uint8_t)*tmp)){
        end_i += cur_ch_bytes;
        num_ch += 1;
        cur_ch_bytes = 0;

        if (num_ch == s) {
          start_i = end_i;
        }
        if (num_ch == e) {
          break;
        }

        //if (num_ch == s) {
        //  start_i = end_i;
        //} else if (num_ch == (e - 1)) {
        //  end_i += cur_ch_bytes;
        //  if (s == e) start_i = end_i;
        //  break;
        //}
      }
      cur_ch_bytes++;
    }
    raw[end_i + 1] = '\0';
    printf("expected=%s, raw=%s, s=%d, e=%d, start_i=%d, end_i=%d\n", expected, raw + start_i, s, e, start_i, end_i);
}

void main(){
  char c[128];
  uint8_t cv[] = {0xEC, 0xBA, 0xBB, 0x00}; // Lambda (0x03bb) is encoded with leading 0xCE
  uint8_t cv2[] = {0xCE, 0xBB}; // Lambda (0x03bb) is encoded with leading 0xCE
  //uint8_t cv2[] = {0xEC, 0xBA, 0xBB}; // Lambda (0x03bb) is encoded with leading 0xCE
//  uint8_t cv[] = {0xCE, 0xBB, 0x00}; // Lambda (0x03bb) is encoded with leading 0xCE
  char *cptr;
  uint32_t state = CYC_UTF8_ACCEPT, codepoint, val = 0x32363435;
  uint8_t *ptr = (uint8_t *)&val;
  int i, j = 0;
//  //memset(c, 0x34, 128);
//  for (i = 0; i < 127; i++) {
//    c[i] = ptr[j++];    
//    if (j == 4) j = 0;
//  }
//  c[127] = '\0';
//  printf("%s\n", c);
  multi_byte_memset(c, 126, cv2, 2);
  c[127] = '\0';
  printf("TEST: %s\n", c);

  ptr = cv;
  for (i = 0; i < 3; i++) {
    Cyc_utf8_decode(&state, &codepoint, ptr[i]);
  }
  printf("state = %d, cp = %d\n", state, codepoint);

  encode(0x3bb);
  encode(65);
  encode(0xcebb);

  printf("%06X\n", 0x0fff);
  substring(0, 1, "A   ");
  substring(0, 2, "AB  ");
  substring(1, 3, "Bx  ");
  substring(1, 4, "BxC ");
  substring(2, 2, "    ");
  substring(2, 3, "x   ");
  substring(2, 4, "xC  ");
  return;
}
