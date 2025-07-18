/* prim5.c
 * Copyright 1984-2017 Cisco Systems, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "system.h"
#include "sort.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <ctype.h>
#include <math.h>

#if defined(__GNU__) /* Hurd */
#include <sys/resource.h>
#endif

/* locally defined functions */
static INT s_errno(void);
static IBOOL s_addr_in_heap(uptr x);
static IBOOL s_ptr_in_heap(ptr x);
static ptr s_generation(ptr x);
static iptr s_fxmul(iptr x, iptr y);
static iptr s_fxdiv(iptr x, iptr y);
static ptr s_trunc_rem(ptr x, ptr y);
static ptr s_fltofx(ptr x);
static ptr s_weak_pairp(ptr p);
static ptr s_ephemeron_cons(ptr car, ptr cdr);
static ptr s_ephemeron_pairp(ptr p);
static ptr s_box_immobile(ptr p);
static ptr s_make_immobile_vector(uptr len, ptr fill);
static ptr s_make_immobile_bytevector(uptr len);
static ptr s_make_reference_bytevector(uptr len);
static ptr s_make_immobile_reference_bytevector(uptr len);
static ptr s_reference_bytevectorp(ptr p);
static ptr s_reference_star_address_object(ptr p);
static ptr s_bytevector_reference_star_ref(ptr p, uptr offset);
static ptr s_oblist(void);
static ptr s_bigoddp(ptr n);
static ptr s_float(ptr x);
static ptr s_decode_float(ptr x);
#ifdef segment_t2_bits
static void s_show_info(FILE *out);
#endif
static void s_show_chunks(FILE *out, ptr sorted_chunks);
static ptr sort_chunks(ptr ls, uptr n);
static ptr merge_chunks(ptr ls1, ptr ls2);
static ptr sorted_chunk_list(void);
static void s_showalloc(IBOOL show_dump, const char *outfn);
static ptr s_system(const char *s);
static ptr s_process(char *s, IBOOL stderrp);
static I32 s_chdir(const char *inpath);
#if defined(GETWD) || defined(__GNU__) /* Hurd */
static char *s_getwd(void);
#endif
static ptr s_set_code_byte(ptr p, ptr n, ptr x);
static ptr s_set_code_word(ptr p, ptr n, ptr x);
static ptr s_set_code_long(ptr p, ptr n, ptr x);
static void s_set_code_long2(ptr p, ptr n, ptr h, ptr l);
static ptr s_set_code_quad(ptr p, ptr n, ptr x);
static ptr s_set_reloc(ptr p, ptr n, ptr e);
static ptr s_flush_instruction_cache(void);
static ptr s_make_code(iptr flags, iptr free, ptr name, ptr arity_mark, iptr n, ptr info, ptr pinfos);
static ptr s_make_reloc_table(ptr codeobj, ptr n);
static ptr s_make_closure(ptr offset, ptr codeobj);
static ptr s_fxrandom(ptr n);
static ptr s_flrandom(ptr x);
static U32 s_random_seed(void);
static void s_set_random_seed(U32 x);
static ptr s_intern(ptr x);
static ptr s_intern2(ptr x, ptr n);
static ptr s_strings_to_gensym(ptr pname_str, ptr uname_str);
static ptr s_intern3(ptr x, ptr n, ptr m);
static ptr s_delete_file(const char *inpath);
static ptr s_delete_directory(const char *inpath);
static ptr s_rename_file(const char *inpath1, const char *inpath2);
static ptr s_mkdir(const char *inpath, INT mode);
static ptr s_chmod(const char *inpath, INT mode);
static ptr s_getmod(const char *inpath, IBOOL followp);
static ptr s_path_atime(const char *inpath, IBOOL followp);
static ptr s_path_ctime(const char *inpath, IBOOL followp);
static ptr s_path_mtime(const char *inpath, IBOOL followp);
static ptr s_fd_atime(INT fd);
static ptr s_fd_ctime(INT fd);
static ptr s_fd_mtime(INT fd);
static IBOOL s_fd_regularp(INT fd);
static void s_nanosleep(ptr sec, ptr nsec);
static ptr s_set_collect_trip_bytes(ptr n);
static void c_exit(I32 status);
static ptr s_get_reloc(ptr co, IBOOL with_offsets);
#ifdef PTHREADS
static s_thread_rv_t s_backdoor_thread_start(void *p);
static iptr s_backdoor_thread(ptr p);
static ptr s_threads(void);
static void s_mutex_acquire(ptr m);
static ptr s_mutex_acquire_noblock(ptr m);
static void s_mutex_release(ptr m);
static void s_condition_broadcast(ptr c);
static void s_condition_signal(ptr c);
static void s_condition_free(ptr c);
static IBOOL s_condition_wait(ptr c, ptr m, ptr t);
static void s_thread_preserve_ownership(ptr tc);
#endif
static void s_byte_copy(ptr src, iptr srcoff, ptr dst, iptr dstoff, iptr cnt);
static void s_ptr_copy(ptr src, iptr srcoff, ptr dst, iptr dstoff, iptr cnt);
static ptr s_tlv(ptr x);
static void s_stlv(ptr x, ptr v);
static void s_test_schlib(void);
static void s_breakhere(ptr x);
static IBOOL s_interactivep(void);
static IBOOL s_same_devicep(INT fd1, INT fd2);
static uptr s_malloc(iptr n);
static void s_free(uptr n);
#ifdef FEATURE_ICONV
static ptr s_iconv_open(const char *tocode, const char *fromcode);
static void s_iconv_close(uptr cd);
static ptr s_iconv_from_string(uptr cd, ptr in, uptr i, uptr iend, ptr out, uptr o, uptr oend);
static ptr s_iconv_to_string(uptr cd, ptr in, uptr i, uptr iend, ptr out, uptr o, uptr oend);
#endif
#ifdef WIN32
static ptr s_multibytetowidechar(unsigned cp, ptr inbv);
static ptr s_widechartomultibyte(unsigned cp, ptr inbv);
#endif
#ifdef PORTABLE_BYTECODE
static ptr s_separatorchar();
#endif

static ptr s_profile_counters(void);
static ptr s_profile_release_counters(void);

#ifdef WIN32
# define WIN32_UNUSED UNUSED
#else
# define WIN32_UNUSED
#endif

#define require(test,who,msg,arg) if (!(test)) S_error1(who, msg, arg)

ptr S_strerror(INT errnum) {
  ptr p; char *msg;

  tc_mutex_acquire();
#ifdef WIN32
  msg = Swide_to_utf8(_wcserror(errnum));
  if (msg == NULL)
    p = Sfalse;
  else {
    p = Sstring_utf8(msg, -1);
    free(msg);
  }
#else
  p = (msg = strerror(errnum)) == NULL ? Sfalse : Sstring_utf8(msg, -1);
#endif
  tc_mutex_release();
  return p;
}

static INT s_errno(void) {
  return errno;
}

static IBOOL s_addr_in_heap(uptr x) {
  return MaybeSegInfo(addr_get_segment(x)) != NULL;
}

static IBOOL s_ptr_in_heap(ptr x) {
  return MaybeSegInfo(ptr_get_segment(x)) != NULL;
}

static ptr s_generation(ptr x) {
  seginfo *si = MaybeSegInfo(ptr_get_segment(x));
  return si == NULL ? Sfalse : FIX(si->generation);
}

static iptr s_fxmul(iptr x, iptr y) {
    return x * y;
}

static iptr s_fxdiv(iptr x, iptr y) {
    return x / y;
}

static ptr s_trunc_rem(ptr x, ptr y) {
  ptr q, r;
  S_trunc_rem(get_thread_context(), x, y, &q, &r);
  return Scons(q, r);
}

static ptr s_fltofx(ptr x) {
    return FIX((iptr)FLODAT(x));
}

static ptr s_weak_pairp(ptr p) {
  seginfo *si;
  return Spairp(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->space == space_weakpair ? Strue : Sfalse;
}

static ptr s_ephemeron_cons(ptr car, ptr cdr) {
  ptr p;

  p = S_ephemeron_cons_in(0, car, cdr);

  return p;
}

static ptr s_ephemeron_pairp(ptr p) {
  seginfo *si;
  return Spairp(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->space == space_ephemeron ? Strue : Sfalse;
}

static ptr s_box_immobile(ptr p) {
  ptr b = S_box2(p, 1);
  S_immobilize_object(b);
  return b;
}

static ptr s_make_immobile_bytevector(uptr len) {
  ptr b = S_bytevector2(get_thread_context(), len, space_immobile_data);
  S_immobilize_object(b);
  return b;
}

static ptr s_make_immobile_vector(uptr len, ptr fill) {
  ptr tc = get_thread_context();
  ptr v;
  uptr i;

  v = S_vector_in(tc, space_immobile_impure, 0, len);

  S_immobilize_object(v);
  
  for (i = 0; i < len; i++)
    INITVECTIT(v, i) = fill;

  if (!(len & 0x1)) {
    /* pad, since we're not going to copy on a GC */
    INITVECTIT(v, len) = FIX(0);
  }

  return v;
}

static ptr s_make_reference_bytevector(uptr len) {
  ptr b = S_bytevector2(get_thread_context(), len, space_reference_array);

  /* In case of a dirty sweep at the current allocation site, we need
     to clear any padding bytes, either internal or for alignment */
  len = (len + ptr_bytes - 1) >> log2_ptr_bytes;
#ifdef bytevector_pad_disp
  *(ptr *)TO_VOIDP((uptr)b+bytevector_pad_disp) = FIX(0);
  if (len & 1) len++;
#else
  if (!(len & 1)) len++;
#endif

  memset(&BVIT(b, 0), 0, len << log2_ptr_bytes);

  return b;
}

static ptr s_make_immobile_reference_bytevector(uptr len) { 
  ptr b = s_make_reference_bytevector(len);
  S_immobilize_object(b);
  return b;  
}

static ptr s_reference_bytevectorp(ptr p) {
  seginfo *si;
  return (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->space == space_reference_array ? Strue : Sfalse;
}

static ptr s_reference_star_address_object(ptr p) {
  if (p == (ptr)0)
    return Sfalse;
  else if (MaybeSegInfo(addr_get_segment(p)))
    return (ptr)((uptr)p - reference_disp);
  else
    return Sunsigned((uptr)p);
}

static ptr s_bytevector_reference_star_ref(ptr p, uptr offset) {
  return s_reference_star_address_object(*(ptr *)&BVIT(p, offset));
}

static ptr s_oblist() {
  ptr ls = Snil;
  iptr idx = S_G.oblist_length;
  bucket *b;

  while (idx-- != 0) {
    for (b = S_G.oblist[idx]; b != NULL; b = b->next) {
      ls = Scons(b->sym, ls);
    }
  }

  return ls;
}

static ptr s_bigoddp(ptr n) {
    return Sboolean(BIGIT(n, BIGLEN(n) - 1) & 1); /* last bigit */;
}

static ptr s_float(ptr x) {
    return Sflonum(S_floatify(x));
}

static ptr s_decode_float(ptr x) {
    require(Sflonump(x),"decode-float","~s is not a float",x);
    return S_decode_float(FLODAT(x));
}

#define FMTBUFSIZE 120
#define CHUNKADDRLT(x, y) (((chunkinfo *)TO_VOIDP(Scar(x)))->addr < ((chunkinfo *)TO_VOIDP(Scar(y)))->addr)
mkmergesort(sort_chunks, merge_chunks, ptr, Snil, CHUNKADDRLT, INITCDR)

static ptr sorted_chunk_list(void) {
  chunkinfo *chunk; INT i, n = 0; ptr ls = Snil;

  for (i = PARTIAL_CHUNK_POOLS; i >= -1; i -= 1) {
    for (chunk = (i == -1) ? S_chunks_full : S_chunks[i]; chunk != NULL; chunk = chunk->next) {
      ls = Scons(TO_PTR(chunk), ls);
      n += 1;
    }
    for (chunk = (i == -1) ? S_code_chunks_full : S_code_chunks[i]; chunk != NULL; chunk = chunk->next) {
      ls = Scons(TO_PTR(chunk), ls);
      n += 1;
    }
  }

  return sort_chunks(ls, n);
}

#ifdef __MINGW32__
# include <inttypes.h>
# define PHtx "%" PRIxPTR
# define Ptd "%" PRIdPTR
#else
# define PHtx "%#tx"
# define Ptd "%td"
#endif

#ifdef segment_t2_bits
static void s_show_info(FILE *out) {
  void *max_addr = 0;
  INT addrwidth;
  const char *addrtitle = "address";
  char fmtbuf[FMTBUFSIZE];
  uptr i2;
#ifdef segment_t3_bits
  INT byteswidth;
  uptr i3;
  for (i3 = 0; i3 < SEGMENT_T3_SIZE; i3 += 1) {
    t2table *t2t = S_segment_info[i3];
    if (t2t != NULL) {
      if ((void *)t2t > max_addr) max_addr = (void *)t2t;
      for (i2 = 0; i2 < SEGMENT_T2_SIZE; i2 += 1) {
        t1table *t1t = t2t->t2[i2];
        if (t1t != NULL) {
          if ((void *)t1t > max_addr) max_addr = (void *)t1t;
        }
      }
    }
  }
  addrwidth = snprintf(fmtbuf, FMTBUFSIZE, ""PHtx"", (ptrdiff_t)max_addr);
  if (addrwidth < (INT)strlen(addrtitle)) addrwidth = (INT)strlen(addrtitle);
  byteswidth = snprintf(fmtbuf, FMTBUFSIZE, ""PHtx"", (ptrdiff_t)(sizeof(t1table) > sizeof(t2table) ? sizeof(t1table) : sizeof(t2table)));
  snprintf(fmtbuf, FMTBUFSIZE, "%%s  %%-%ds  %%-%ds\n\n", addrwidth, byteswidth);
  fprintf(out, fmtbuf, "level", addrtitle, "bytes");
  snprintf(fmtbuf, FMTBUFSIZE, "%%-5d  %%#0%dtx  %%#0%dtx\n", addrwidth, byteswidth);
  for (i3 = 0; i3 < SEGMENT_T3_SIZE; i3 += 1) {
    t2table *t2t = S_segment_info[i3];
    if (t2t != NULL) {
      fprintf(out, fmtbuf, 2, t2t, sizeof(t2table));
      for (i2 = 0; i2 < SEGMENT_T2_SIZE; i2 += 1) {
        t1table *t1t = t2t->t2[i2];
        if (t1t != NULL) {
          fprintf(out, fmtbuf, 1, (ptrdiff_t)t1t, (ptrdiff_t)sizeof(t1table));
        }
      }
    }
  }
#else
  for (i2 = 0; i2 < SEGMENT_T2_SIZE; i2 += 1) {
    t1table *t1t = S_segment_info[i2];
    if (t1t != NULL) {
      if ((void *)t1t > max_addr) max_addr = (void *)t1t;
    }
  }
  addrwidth = 1 + snprintf(fmtbuf, FMTBUFSIZE, ""PHtx"", (ptrdiff_t)max_addr);
  if (addrwidth < (INT)strlen(addrtitle) + 1) addrwidth = (INT)strlen(addrtitle) + 1;
  snprintf(fmtbuf, FMTBUFSIZE, "%%s %%-%ds %%s\n\n", addrwidth);
  fprintf(out, fmtbuf, "level", addrtitle, "bytes");
  snprintf(fmtbuf, FMTBUFSIZE, "%%-5d %%#0%dtx %"PHtx"\n", (ptrdiff_t)addrwidth);
  for (i2 = 0; i2 < SEGMENT_T2_SIZE; i2 += 1) {
    t1table *t1t = S_segment_info[i2];
    if (t1t != NULL) {
      fprintf(out, fmtbuf, 1, (ptrdiff_t)t1t, (ptrdiff_t)sizeof(t1table));
    }
  }
#endif
}
#endif

static void s_show_chunks(FILE *out, ptr sorted_chunks) {
  char fmtbuf[FMTBUFSIZE];
  chunkinfo *chunk;
  void *max_addr = 0;
  void *max_header_addr = 0;
  iptr max_segs = 0;
  INT addrwidth, byteswidth, headeraddrwidth, headerbyteswidth, segswidth, headerwidth;
  const char *addrtitle = "address", *bytestitle = "bytes", *headertitle = "(+ header)";
  ptr ls;

  for (ls = sorted_chunks; ls != Snil; ls = Scdr(ls)) {
    chunk = TO_VOIDP(Scar(ls));
    max_addr = chunk->addr;
    if (chunk->segs > max_segs) max_segs = chunk->segs;
    if ((void *)chunk > max_header_addr) max_header_addr = (void *)chunk;
  }

  addrwidth = (INT)snprintf(fmtbuf, FMTBUFSIZE, ""PHtx"", (ptrdiff_t)max_addr);
  if (addrwidth < (INT)strlen(addrtitle)) addrwidth = (INT)strlen(addrtitle);
  byteswidth = (INT)snprintf(fmtbuf, FMTBUFSIZE, ""PHtx"", (ptrdiff_t)(max_segs * bytes_per_segment));
  if (byteswidth < (INT)strlen(bytestitle)) byteswidth = (INT)strlen(bytestitle);
  headerbyteswidth = (INT)snprintf(fmtbuf, FMTBUFSIZE, ""PHtx"", (ptrdiff_t)(sizeof(chunkinfo) + sizeof(seginfo) * max_segs));
  headeraddrwidth = (INT)snprintf(fmtbuf, FMTBUFSIZE, ""PHtx"", (ptrdiff_t)max_header_addr);
  segswidth = (INT)snprintf(fmtbuf, FMTBUFSIZE, ""Ptd"", (ptrdiff_t)max_segs);
  headerwidth = headerbyteswidth + headeraddrwidth + 13;

  snprintf(fmtbuf, FMTBUFSIZE, "%%-%ds %%-%ds %%-%ds %%s\n\n", addrwidth, byteswidth, headerwidth);
  fprintf(out, fmtbuf, addrtitle, bytestitle, headertitle, "segments used");
  snprintf(fmtbuf, FMTBUFSIZE, "%%#0%dtx %%#0%dtx (+ %%#0%dtx bytes @ %%#0%dtx) %%%dtd of %%%dtd\n",
      addrwidth, byteswidth, headerbyteswidth, headeraddrwidth, segswidth, segswidth);
  for (ls = sorted_chunks; ls != Snil; ls = Scdr(ls)) {
    chunk = TO_VOIDP(Scar(ls));
    fprintf(out, fmtbuf, (ptrdiff_t)chunk->addr, (ptrdiff_t)chunk->bytes,
        (ptrdiff_t)(sizeof(chunkinfo) + sizeof(seginfo) * chunk->segs),
        (ptrdiff_t)chunk, (ptrdiff_t)chunk->nused_segs, (ptrdiff_t)chunk->segs);
  }
}

#define space_bogus (max_space + 1)
#define space_total (space_bogus + 1)
#define generation_total (static_generation + 1)
#define INCRGEN(g) (g = g == S_G.max_nonstatic_generation ? static_generation : g+1)
static void s_showalloc(IBOOL show_dump, const char *outfn) {
  FILE *out;
  iptr count[generation_total+1][space_total+1];
  uptr bytes[generation_total+1][space_total+1];
  int i, column_size[generation_total+1];
  char fmtbuf[FMTBUFSIZE];
  static char *spacename[space_total+1] = { alloc_space_names, "bogus", "total" };
  static char spacechar[space_total+1] = { alloc_space_chars, '?', 't' };
  chunkinfo *chunk; seginfo *si; ISPC s; IGEN g;
  ptr sorted_chunks;
  ptr tc = get_thread_context();

  tc_mutex_acquire();
  alloc_mutex_acquire();

  if (outfn == NULL) {
    out = stderr;
  } else {
#ifdef WIN32
    wchar_t *outfnw = Sutf8_to_wide(outfn);
    out = _wfopen(outfnw, L"w");
    free(outfnw);
#else
    out = fopen(outfn, "w");
#endif
    if (out == NULL) {
      ptr msg = S_strerror(errno);
      if (msg != Sfalse) {
        tc_mutex_release();
        S_error2("fopen", "open of ~s failed: ~a", Sstring_utf8(outfn, -1), msg);
      } else {
        tc_mutex_release();
        S_error1("fopen", "open of ~s failed", Sstring_utf8(outfn, -1));
      }
    }
  }
  for (g = 0; g <= generation_total; INCRGEN(g))
    for (s = 0; s <= space_total; s++)
      count[g][s] = bytes[g][s] = 0;

  for (g = 0; g <= static_generation; INCRGEN(g)) {
    for (s = 0; s <= max_real_space; s++) {
      /* add in bytes previously recorded */
      bytes[g][s] += S_G.bytes_of_space[g][s];
      /* add in bytes in active segments */
      if (THREAD_GC(tc)->next_loc[g][s] != FIX(0))
        bytes[g][s] += (uptr)THREAD_GC(tc)->next_loc[g][s] - (uptr)THREAD_GC(tc)->base_loc[g][s];
    }
  }

  for (g = 0; g <= static_generation; INCRGEN(g)) {
    for (s = 0; s <= max_real_space; s++) {
      for (si = S_G.occupied_segments[g][s]; si != NULL; si = si->next) {
        count[g][s] += 1;
      }
    }
  }

  for (g = 0; g < generation_total; INCRGEN(g)) {
    for (s = 0; s < space_total; s++) {
      count[g][space_total] += count[g][s];
      count[generation_total][s] += count[g][s];
      count[generation_total][space_total] += count[g][s];
      bytes[g][space_total] += bytes[g][s];
      bytes[generation_total][s] += bytes[g][s];
      bytes[generation_total][space_total] += bytes[g][s];
    }
  }

  for (g = 0; g <= generation_total; INCRGEN(g)) {
    if (count[g][space_total] != 0) {
      int n = 1 + snprintf(fmtbuf, FMTBUFSIZE, ""Ptd"", (ptrdiff_t)count[g][space_total]);
      column_size[g] = n < 8 ? 8 : n;
    }
  }

  fprintf(out, "Segments per space & generation:\n\n");
  fprintf(out, "%8s", "");
  for (g = 0; g <= generation_total; INCRGEN(g)) {
    if (count[g][space_total] != 0) {
      if (g == generation_total) {
        /* coverity[uninit_use] */
        snprintf(fmtbuf, FMTBUFSIZE, "%%%ds", column_size[g]);
        fprintf(out, fmtbuf, "total");
      } else if (g == static_generation) {
        /* coverity[uninit_use] */
        snprintf(fmtbuf, FMTBUFSIZE, "%%%ds", column_size[g]);
        fprintf(out, fmtbuf, "static");
      } else {
        /* coverity[uninit_use] */
        snprintf(fmtbuf, FMTBUFSIZE, "%%%dd", column_size[g]);
        fprintf(out, fmtbuf, g);
      }
    }
  }
  fprintf(out, "\n");
  for (s = 0; s <= space_total; s++) {
    if (s != space_empty) {
      if (count[generation_total][s] != 0) {
        fprintf(out, "%7s:", spacename[s]);
        for (g = 0; g <= generation_total; INCRGEN(g)) {
          if (count[g][space_total] != 0) {
            /* coverity[uninit_use] */
            snprintf(fmtbuf, FMTBUFSIZE, "%%%dtd", column_size[g]);
            fprintf(out, fmtbuf, (ptrdiff_t)(count[g][s]));
          }
        }
        fprintf(out, "\n");
        fprintf(out, "%8s", "");
        for (g = 0; g <= generation_total; INCRGEN(g)) {
          if (count[g][space_total] != 0) {
            if (count[g][s] != 0 && s <= max_real_space) {
              /* coverity[uninit_use] */
              snprintf(fmtbuf, FMTBUFSIZE, "%%%dd%%%%", column_size[g] - 1);
              fprintf(out, fmtbuf,
                  (int)(((double)bytes[g][s] /
                      ((double)count[g][s] * bytes_per_segment)) * 100.0));
            } else {
              /* coverity[uninit_use] */
              snprintf(fmtbuf, FMTBUFSIZE, "%%%ds", column_size[g]);
              fprintf(out, fmtbuf, "");
            }
          }
        }
        fprintf(out, "\n");
      }
    }
  }

  fprintf(out, "segment size = "PHtx" bytes.  percentages show the portion actually occupied.\n", (ptrdiff_t)bytes_per_segment);
  fprintf(out, ""Ptd" segments are presently reserved for future allocation or collection.\n", (ptrdiff_t)S_G.number_of_empty_segments);

  fprintf(out, "\nMemory chunks obtained and not returned to the O/S:\n\n");
  sorted_chunks = sorted_chunk_list();
  s_show_chunks(out, sorted_chunks);

#ifdef segment_t2_bits
  fprintf(out, "\nDynamic memory occupied by segment info table:\n\n");
  s_show_info(out);
#endif

  fprintf(out, "\nAdditional memory might be used by C libraries and programs in the\nsame address space.\n");

  if (show_dump) {
    iptr max_seg = 0;
    int segwidth, segsperline;
    iptr next_base = 0;
    int segsprinted = 0;
    char spaceline[100], genline[100];
    ptr ls;

    for (ls = sorted_chunks; ls != Snil; ls = Scdr(ls)) {
      iptr last_seg;
      chunk = TO_VOIDP(Scar(ls));
      last_seg = chunk->base + chunk->segs;
      if (last_seg > max_seg) max_seg = last_seg;
    }

    segwidth = snprintf(fmtbuf, FMTBUFSIZE, ""PHtx" ", (ptrdiff_t)max_seg);
    segsperline = (99 - segwidth) & ~0xf;
    
    snprintf(fmtbuf, FMTBUFSIZE, "  %%-%ds", segwidth);
    snprintf(genline, 100, fmtbuf, "");

    fprintf(out, "\nMap of occupied segments:\n");
    for (ls = sorted_chunks; ls != Snil; ls = Scdr(ls)) {
      seginfo *si;

      chunk = TO_VOIDP(Scar(ls));

      if (chunk->base != next_base && segsprinted != 0) {
        for (;;) {
          if (segsprinted == segsperline) {
            fprintf(out, "\n%s", spaceline);
            fprintf(out, "\n%s", genline);
            break;
          }
          if (next_base == chunk->base) break;
          spaceline[segwidth+segsprinted] = ' ';
          genline[segwidth+segsprinted] = ' ';
          segsprinted += 1;
          next_base += 1;
        }
      }

      if (chunk->base > next_base && next_base != 0) {
        fprintf(out, "\n-------- skipping "Ptd" segments --------", (ptrdiff_t)(chunk->base - next_base));
      }

      for (i = 0; i < chunk->segs; i += 1) {
        if (segsprinted >= segsperline) segsprinted = 0;
        
        if (segsprinted == 0) {
          if (i != 0) {
            fprintf(out, "\n%s", spaceline);
            fprintf(out, "\n%s", genline);
          }
          snprintf(fmtbuf, FMTBUFSIZE, "%%#0%dtx ", segwidth - 1);
          snprintf(spaceline, 100, fmtbuf, (ptrdiff_t)(chunk->base + i));
          segsprinted = 0;
        }

        si = &chunk->sis[i];
        s = si->space;
        if (s < 0 || s > max_space) s = space_bogus;
        spaceline[segwidth+segsprinted] = spacechar[s];

        g = si->generation;
        genline[segwidth+segsprinted] =
          (s == space_empty) ? '.' :
          (g < 10) ? '0' + g :
          (g < 36) ? 'A' + g - 10 :
          (g == static_generation) ? '*' : '+';
        segsprinted += 1;
      }
      next_base = chunk->base + chunk->segs;  
    }

    if (segsprinted != 0) {
      spaceline[segwidth+segsprinted] = 0;
      genline[segwidth+segsprinted] = 0;
      fprintf(out, "\n%s", spaceline);
      fprintf(out, "\n%s", genline);
    }

    fprintf(out, "\n\nSpaces:");
    for (s = 0; s < space_total; s += 1)
      fprintf(out, "%s%c = %s", s % 5 == 0 ? "\n  " : "\t",
          spacechar[s], spacename[s]);
    fprintf(out, "\n\nGenerations:\n  0-9: 0<=g<=9; A-Z: 10<=g<=35; +: g>=36; *: g=static; .: empty\n\n");
  }

  if (outfn == NULL) {
    fflush(out);
  } else {
    fclose(out);
  }

  alloc_mutex_release();
  tc_mutex_release();
}

#include <signal.h>
#ifdef WIN32
#include <io.h>
#include <process.h>
#include <fcntl.h>
#include <direct.h>
#include <malloc.h>
#else /* WIN32 */
#include <sys/param.h>
#include <sys/wait.h>
#endif /* WIN32 */

static ptr s_system(const char *s) {
  INT status;
#ifdef PTHREADS
  ptr tc = get_thread_context();
  char *s_arg = NULL;
#endif

#ifdef PTHREADS
  if (DISABLECOUNT(tc) == FIX(0)) {
    /* copy `s` in case a GC happens */
    uptr len = strlen(s) + 1;
    s_arg = malloc(len);
    if (s_arg == NULL)
      S_error("system", "malloc failed");
    memcpy(s_arg, s, len);
    deactivate_thread(tc);
    s = s_arg;
  } else
    s_arg = NULL;
#endif
  status = SYSTEM(s);
#ifdef PTHREADS
  if (DISABLECOUNT(tc) == FIX(0)) {
    reactivate_thread(tc);
    free(s_arg);
  }
#endif

  if ((status == -1) && (errno != 0)) {
    ptr msg = S_strerror(errno);

    if (msg != Sfalse)
      S_error1("system", "~a", msg);
    else
      S_error("system", "subprocess execution failed");
  }

#ifdef WIN32
  return Sinteger(status);
#else
  if (WIFEXITED(status)) return Sinteger(WEXITSTATUS(status));
  if (WIFSIGNALED(status)) return Sinteger(-WTERMSIG(status));
  S_error("system", "cannot determine subprocess exit status");
  return 0 /* not reached */;
#endif /* WIN32 */
}

static ptr s_process(char *s, IBOOL stderrp) {
    INT ifd = -1, ofd = -1, efd = -1, child = -1;

#ifdef WIN32
    HANDLE hToRead, hToWrite, hFromRead, hFromWrite, hFromReadErr, hFromWriteErr, hProcess;
    STARTUPINFOW si = {0};
    PROCESS_INFORMATION pi;
    char *comspec;
    char *buffer;
    wchar_t* bufferw;

    /* Create non-inheritable pipes, important to eliminate zombee children
     * when the parent sides are closed. */
    if (!CreatePipe(&hToRead, &hToWrite, NULL, 0))
        S_error("process", "cannot open pipes");
    if (!CreatePipe(&hFromRead, &hFromWrite, NULL, 0)) {
        CloseHandle(hToRead);
        CloseHandle(hToWrite);
        S_error("process", "cannot open pipes");
    }
    if (stderrp && !CreatePipe(&hFromReadErr, &hFromWriteErr, NULL, 0)) {
        CloseHandle(hToRead);
        CloseHandle(hToWrite);
        CloseHandle(hFromRead);
        CloseHandle(hFromWrite);
        S_error("process", "cannot open pipes");
    }

    si.cb = sizeof(STARTUPINFO);
    si.dwFlags = STARTF_USESTDHANDLES;
    hProcess = GetCurrentProcess();

    /* Duplicate the ToRead handle so that the child can inherit it. */
    if (!DuplicateHandle(hProcess, hToRead, hProcess, &si.hStdInput,
                         GENERIC_READ, TRUE, 0)) {
        CloseHandle(hToRead);
        CloseHandle(hToWrite);
        CloseHandle(hFromRead);
        CloseHandle(hFromWrite);
        if (stderrp) {
          CloseHandle(hFromReadErr);
          CloseHandle(hFromWriteErr);
        }
        S_error("process", "cannot open pipes");
    }
    CloseHandle(hToRead);

    /* Duplicate the FromWrite handle so that the child can inherit it. */
    if (!DuplicateHandle(hProcess, hFromWrite, hProcess, &si.hStdOutput,
                         GENERIC_WRITE, TRUE, 0)) {
        CloseHandle(si.hStdInput);
        CloseHandle(hToWrite);
        CloseHandle(hFromRead);
        CloseHandle(hFromWrite);
        if (stderrp) {
          CloseHandle(hFromReadErr);
          CloseHandle(hFromWriteErr);
        }
        S_error("process", "cannot open pipes");
    }
    CloseHandle(hFromWrite);

    if (stderrp) {
      /* Duplicate the FromWrite handle so that the child can inherit it. */
      if (!DuplicateHandle(hProcess, hFromWriteErr, hProcess, &si.hStdError,
                           GENERIC_WRITE, TRUE, 0)) {
          CloseHandle(si.hStdInput);
          CloseHandle(hToWrite);
          CloseHandle(hFromRead);
          CloseHandle(hFromWrite);
          CloseHandle(hFromReadErr);
          CloseHandle(hFromWriteErr);
          S_error("process", "cannot open pipes");
      }
      CloseHandle(hFromWriteErr);
    } else {
      si.hStdError = si.hStdOutput;
    }

    if ((comspec = Sgetenv("COMSPEC"))) {
        size_t n = strlen(comspec) + strlen(s) + 7;
        buffer = (char *)_alloca(n);
        snprintf(buffer, n, "\"%s\" /c %s", comspec, s);
        free(comspec);
    } else
        buffer = s;
    bufferw = Sutf8_to_wide(buffer);
    if (!CreateProcessW(NULL, bufferw, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
        free(bufferw);
        CloseHandle(si.hStdInput);
        CloseHandle(hToWrite);
        CloseHandle(hFromRead);
        CloseHandle(si.hStdOutput);
        if (stderrp) {
          CloseHandle(hFromReadErr);
          CloseHandle(si.hStdError);
        }
        S_error("process", "cannot spawn subprocess");
    }
    free(bufferw);
    CloseHandle(si.hStdInput);
    CloseHandle(si.hStdOutput);
    if (stderrp) {
      CloseHandle(si.hStdError);
    }
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    ifd = _open_osfhandle((intptr_t)hFromRead, 0);
    ofd = _open_osfhandle((intptr_t)hToWrite, 0);
    if (stderrp) {
      efd = _open_osfhandle((intptr_t)hFromReadErr, 0);
    }
    child = pi.dwProcessId;

#else /* WIN32 */

    INT tofds[2], fromfds[2], errfds[2];
    struct sigaction act, oint_act;

    if (pipe(tofds)) S_error("process","cannot open pipes");
    if (pipe(fromfds)) {
        CLOSE(tofds[0]); CLOSE(tofds[1]);
        S_error("process","cannot open pipes");
    }
    if (stderrp) {
      if (pipe(errfds)) {
          CLOSE(tofds[0]); CLOSE(tofds[1]);
          CLOSE(fromfds[0]); CLOSE(fromfds[1]);
          S_error("process","cannot open pipes");
      }
    }

    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    sigaction(SIGINT, &act, &oint_act);

    if ((child = fork()) == 0) {
      /* child does this: */
        CLOSE(0); if (dup(tofds[0]) != 0) _exit(1);
        CLOSE(1); if (dup(fromfds[1]) != 1) _exit(1);
        CLOSE(2); if (dup(stderrp ? errfds[1] : 1) != 2) _exit(1);
#ifndef __GNU__ /* Hurd */
        {INT i; for (i = 3; i < NOFILE; i++) (void)CLOSE(i);}
#else /* __GNU__ Hurd: no NOFILE */
        {
          INT i;
          struct rlimit rlim;
          getrlimit(RLIMIT_NOFILE, &rlim);
          for (i = 3; i < rlim.rlim_cur; i++) {
            (void)CLOSE(i);
          }
        }
#endif /* __GNU__ Hurd */
        execl("/bin/sh", "/bin/sh", "-c", s, NULL);
        _exit(1) /* only if execl fails */;
        /*NOTREACHED*/
    } else {
      /* parent does this: */
        CLOSE(tofds[0]); CLOSE(fromfds[1]); if (stderrp) CLOSE(errfds[1]);
        if (child < 0) {
            CLOSE(tofds[1]); CLOSE(fromfds[0]); if (stderrp) CLOSE(errfds[0]);
            sigaction(SIGINT, &oint_act, (struct sigaction *)0);
            S_error("process", "cannot fork subprocess");
            /*NOTREACHED*/
        } else {
            ifd = fromfds[0];
            ofd = tofds[1];
            if (stderrp) efd = errfds[0];
            sigaction(SIGINT, &oint_act, (struct sigaction *)0);
            S_register_child_process(child);
        }
    }
#endif /* WIN32 */

    if (stderrp)
      return LIST4(FIX(ifd), FIX(efd), FIX(ofd), FIX(child));
    else
      return LIST3(FIX(ifd), FIX(ofd), FIX(child));
}

static I32 s_chdir(const char *inpath) {
    char *path;
    I32 status;

    path = S_malloc_pathname(inpath);
#ifdef EINTR
    while ((status = CHDIR(path)) != 0 && errno == EINTR) ;
#else /* EINTR */
    status = CHDIR(path);
#endif /* EINTR */
    free(path);
    return status;
}

#ifdef GETWD
static char *s_getwd() {
  return GETWD(TO_VOIDP(&BVIT(S_bytevector(PATH_MAX), 0)));
}
#elif defined(__GNU__) /* Hurd: no PATH_MAX */
static char *s_getwd() {
  char *path;
  size_t len;
  ptr bv;
  path = getcwd(NULL, 0);
  if (NULL == path) {
    return NULL;
  } else {
    len = strlen(path);
    bv = S_bytevector(len);
    memcpy(TO_VOIDP(&BVIT(bv, 0)), path, len);
    free(path);
    return TO_VOIDP(&BVIT(bv, 0));
  }
}
#endif /* GETWD */

static ptr s_set_code_byte(ptr p, ptr n, ptr x) {
    I8 *a;
    ptr tc = get_thread_context();

    a = (I8 *)TO_VOIDP((uptr)p + UNFIX(n));
    S_thread_start_code_write(tc, 0, 0, TO_VOIDP(a), sizeof(I8));
    *a = (I8)UNFIX(x);
    S_thread_end_code_write(tc, 0, 0, TO_VOIDP(a), sizeof(I8));

    return Svoid;
}

static ptr s_set_code_word(ptr p, ptr n, ptr x) {
    I16 *a;
    ptr tc = get_thread_context();

    a = (I16 *)TO_VOIDP((uptr)p + UNFIX(n));
    S_thread_start_code_write(tc, 0, 0, TO_VOIDP(a), sizeof(I16));
    *a = (I16)UNFIX(x);
    S_thread_end_code_write(tc, 0, 0, TO_VOIDP(a), sizeof(I16));

    return Svoid;
}

static ptr s_set_code_long(ptr p, ptr n, ptr x) {
    I32 *a;
    ptr tc = get_thread_context();

    a = (I32 *)TO_VOIDP((uptr)p + UNFIX(n));
    S_thread_start_code_write(tc, 0, 0, TO_VOIDP(a), sizeof(I32));
    *a = (I32)(Sfixnump(x) ? UNFIX(x) : Sinteger_value(x));
    S_thread_end_code_write(tc, 0, 0, TO_VOIDP(a), sizeof(I32));

    return Svoid;
}

static void s_set_code_long2(ptr p, ptr n, ptr h, ptr l) {
    I32 *a;
    ptr tc = get_thread_context();

    a = (I32 *)TO_VOIDP((uptr)p + UNFIX(n));
    S_thread_start_code_write(tc, 0, 0, TO_VOIDP(a), sizeof(I32));
    *a = (I32)((UNFIX(h) << 16) + UNFIX(l));
    S_thread_end_code_write(tc, 0, 0, TO_VOIDP(a), sizeof(I32));
}

static ptr s_set_code_quad(ptr p, ptr n, ptr x) {
  I64 *a, val;
    ptr tc = get_thread_context();

    a = (I64 *)TO_VOIDP((uptr)p + UNFIX(n));
    S_thread_start_code_write(tc, 0, 0, TO_VOIDP(a), sizeof(I64));
    val = Sfixnump(x) ? UNFIX(x) : S_int64_value("\\#set-code-quad!", x);
    memcpy(a, &val, sizeof(I64));
    S_thread_end_code_write(tc, 0, 0, TO_VOIDP(a), sizeof(I64));

    return Svoid;
}

static ptr s_set_reloc(ptr p, ptr n, ptr e) {
    iptr *a;

    a = (iptr *)(&RELOCIT(CODERELOC(p), UNFIX(n)));
    STORE_UNALIGNED_UPTR(a, (uptr)(Sfixnump(e) ? UNFIX(e) : Sinteger_value(e)));

    return e;
}

static ptr s_flush_instruction_cache() {
    S_flush_instruction_cache(get_thread_context());
    return Svoid;
}

static ptr s_make_code(iptr flags, iptr free, ptr name, ptr arity_mark, iptr n, ptr info, ptr pinfos) {
    ptr co;
    ptr tc = get_thread_context();

    S_thread_start_code_write(tc, 0, 0, NULL, 0);

    co = S_code(tc, type_code | (flags << code_flags_offset), n);
    CODEFREE(co) = free;
    CODENAME(co) = name;
    CODEARITYMASK(co) = arity_mark;
    CODEINFO(co) = info;
    CODEPINFOS(co) = pinfos;
    if (pinfos != Snil) {
      S_G.profile_counters = Scons(S_weak_cons(co, pinfos), S_G.profile_counters);
    }

    S_thread_end_code_write(tc, 0, 0, NULL, 0);

    return co;
}

static ptr s_make_reloc_table(ptr codeobj, ptr n) {
    ptr tc = get_thread_context();

    S_thread_start_code_write(tc, 0, 0, TO_VOIDP(&CODERELOC(codeobj)), sizeof(ptr));
    CODERELOC(codeobj) = S_relocation_table(UNFIX(n));
    RELOCCODE(CODERELOC(codeobj)) = codeobj;
    S_thread_end_code_write(tc, 0, 0, TO_VOIDP(&CODERELOC(codeobj)), sizeof(ptr));
    return Svoid;
}

static ptr s_make_closure(ptr offset, ptr codeobj) {

    return S_closure((ptr)((iptr)codeobj + UNFIX(offset)), 0);
}

/* the random formula is based on Knuth.  It returns a random fixnum
 * between 0 and n-1.
 */
static ptr s_fxrandom(ptr p) {
  ptr tc = get_thread_context();
  uptr t, n = UNFIX(p);

  t = (RANDOMSEED(tc) = RANDOMSEED(tc) * 72931 + 90763387) >> 16;
  t = t | ((RANDOMSEED(tc) = RANDOMSEED(tc) * 72931 + 90763387) & 0xffff0000);
  if (n <= 0xffffffff) /* trivially true if sizeof(ptr) <= sizeof(U32) */
    return FIX(t % n);
  else {
    t = (t << 16) | ((RANDOMSEED(tc) = RANDOMSEED(tc) * 72931 + 90763387) >> 16);
    t = (t << 16) | ((RANDOMSEED(tc) = RANDOMSEED(tc) * 72931 + 90763387) >> 16);
    return FIX(t % n);
  }
}

static ptr s_flrandom(ptr x) {
    ptr tc = get_thread_context();
    U32 t1, t2, t3, t4;

    t1 = RANDOMSEED(tc) = RANDOMSEED(tc) * 72931 + 90763387;
    t2 = RANDOMSEED(tc) = RANDOMSEED(tc) * 72931 + 90763387;
    t3 = RANDOMSEED(tc) = RANDOMSEED(tc) * 72931 + 90763387;
    t4 = RANDOMSEED(tc) = RANDOMSEED(tc) * 72931 + 90763387;
    return Sflonum(S_random_double(t1, t2, t3, t4, FLODAT(x)));
}

static U32 s_random_seed() {
    ptr tc = get_thread_context();
    return RANDOMSEED(tc);
}

static void s_set_random_seed(U32 x) {
    ptr tc = get_thread_context();
    RANDOMSEED(tc) = x;
}

static ptr s_intern(ptr x) {
  require(Sstringp(x),"string->symbol","~s is not a string",x);

  return S_intern_sc(&STRIT(x, 0), Sstring_length(x), x);
}

static ptr s_intern2(ptr x, ptr n) {
  return S_intern_sc(&STRIT(x, 0), UNFIX(n), Sfalse);
}

/* first n chars str are pretty name; remaining m-n are unique name */
static ptr s_intern3(ptr x, ptr n, ptr m) {
  iptr plen = UNFIX(n);
  return S_intern3(&STRIT(x, 0), plen, &STRIT(x, plen), UNFIX(m) - plen, Sfalse, Sfalse);
}

static ptr s_strings_to_gensym(ptr pname_str, ptr uname_str) {
  return S_intern3(&STRIT(pname_str, 0), Sstring_length(pname_str),
                   &STRIT(uname_str, 0), Sstring_length(uname_str),
                   pname_str, uname_str);
}

ptr S_uninterned(ptr x) {
  ptr sym;
  static uptr hc;

  require(Sstringp(x),"string->uninterned-symbol","~s is not a string",x);
  if (!(STRTYPE(x) & string_immutable_flag))
    x = S_mkstring(&STRIT(x, 0), Sstring_length(x));

  sym = S_symbol(Scons(x, Sfalse));

  /* Wraparound on `hc++` is ok. It's technically illegal with
     threads, since multiple thread might increment `hc` at the same
     time; we don't care if we miss an increment sometimes, and we
     assume compilers won't take this as a license for arbitrarily bad
     behavior: */
  hc++;
  INITSYMHASH(sym) = FIX(hc);

  return sym;
}

static ptr s_mkdir(const char *inpath, WIN32_UNUSED INT mode) {
  INT status; ptr res; char *path;

  path = S_malloc_pathname(inpath);
#ifdef WIN32
  status = S_windows_mkdir(path);
#else /* WIN32 */
  status = mkdir(path, mode);
#endif /* WIN32 */

  res = status == 0 ? Strue : S_strerror(errno);
  free(path);
  return res;
}

static ptr s_delete_file(const char *inpath) {
  ptr res; char *path;

  path = S_malloc_pathname(inpath);
  res = UNLINK(path) == 0 ? Strue : S_strerror(errno);
  free(path);
  return res;
}

static ptr s_delete_directory(const char *inpath) {
  ptr res; char *path;

  path = S_malloc_pathname(inpath);
  res = RMDIR(path) == 0 ? Strue : S_strerror(errno);
  free(path);
  return res;
}

static ptr s_rename_file(const char *inpath1, const char *inpath2) {
  ptr res; char *path1, *path2;

  path1 = S_malloc_pathname(inpath1);
  path2 = S_malloc_pathname(inpath2);
  res = RENAME(path1, path2) == 0 ? Strue : S_strerror(errno);
  free(path1);
  free(path2);
  return res;
}

static ptr s_chmod(const char *inpath, INT mode) {
  ptr res; INT status; char *path;

  path = S_malloc_pathname(inpath);
#ifdef WIN32
 /* pathetic approximation: (a) only handles user permissions, (b) doesn't
    handle execute permissions, (c) windows won't make file not readable */
  status = CHMOD(path,
                 (mode & 0400 ? S_IREAD : 0) |
                 (mode & 0200 ? S_IWRITE : 0));
#else /* WIN32 */
  status = CHMOD(path, mode);
#endif /* WIN32 */
  res = status == 0 ? Strue : S_strerror(errno);
  free(path);
  return res;
}

static ptr s_getmod(const char *inpath, IBOOL followp) {
  ptr res; char *path; struct STATBUF statbuf;

  path = S_malloc_pathname(inpath);

 /* according to msdn, user read/write bits are set according to the file's
    permission mode, and user execute bits are set according to the
    filename extension.  it says nothing about group and other execute bits. */

  if ((followp ? STAT(path, &statbuf) : LSTAT(path, &statbuf)) != 0) {
    res = S_strerror(errno);
  } else {
    res = FIX(statbuf.st_mode & 07777);
  }
  free(path);
  return res;
}

static ptr s_path_atime(const char *inpath, WIN32_UNUSED IBOOL followp) {
#ifdef WIN32
  ptr res;
  wchar_t *wpath;
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  __int64 total, sec; int nsec;
  
  if ((wpath = S_malloc_wide_pathname(inpath)) == NULL) {
    res = S_LastErrorString();
  } else if (!GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata)) {
    DWORD err = GetLastError();
    res = err == ERROR_FILE_NOT_FOUND || err == ERROR_PATH_NOT_FOUND ?
          Sstring("no such file or directory") :
          S_LastErrorString();
  } else {
    total = filedata.ftLastAccessTime.dwHighDateTime;
    total <<= 32;
    total |= filedata.ftLastAccessTime.dwLowDateTime;
    sec = total / 10000000 - 11644473600L;
    nsec = (total % 10000000) * 100;
    res = Scons(Sinteger64(sec), Sinteger32(nsec));
  }
  free(wpath);
  return res;
#else /* WIN32 */
  ptr res;
  char *path;
  struct STATBUF statbuf;

  path = S_malloc_pathname(inpath);
  if ((followp ? STAT(path, &statbuf) : LSTAT(path, &statbuf)) != 0) {
    res = S_strerror(errno);
  } else {
    res = Scons(Sinteger64(SECATIME(statbuf)), Sinteger32(NSECATIME(statbuf)));
  }
  free(path);
  return res;
#endif /* WIN32 */
}

static ptr s_path_ctime(const char *inpath, WIN32_UNUSED IBOOL followp) {
#ifdef WIN32
  ptr res;
  wchar_t *wpath;
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  __int64 total, sec; int nsec;
  
  if ((wpath = S_malloc_wide_pathname(inpath)) == NULL) {
    res = S_LastErrorString();
  } else if (!GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata)) {
    DWORD err = GetLastError();
    res = err == ERROR_FILE_NOT_FOUND || err == ERROR_PATH_NOT_FOUND ?
          Sstring("no such file or directory") :
          S_LastErrorString();
  } else {
    total = filedata.ftLastWriteTime.dwHighDateTime;
    total <<= 32;
    total |= filedata.ftLastWriteTime.dwLowDateTime;
    sec = total / 10000000 - 11644473600L;
    nsec = (total % 10000000) * 100;
    res = Scons(Sinteger64(sec), Sinteger32(nsec));
  }
  free(wpath);
  return res;
#else /* WIN32 */
  ptr res;
  char *path;
  struct STATBUF statbuf;

  path = S_malloc_pathname(inpath);
  if ((followp ? STAT(path, &statbuf) : LSTAT(path, &statbuf)) != 0) {
    res = S_strerror(errno);
  } else {
    res = Scons(Sinteger64(SECCTIME(statbuf)), Sinteger32(NSECCTIME(statbuf)));
  }
  free(path);
  return res;
#endif /* WIN32 */
}

static ptr s_path_mtime(const char *inpath, WIN32_UNUSED IBOOL followp) {
#ifdef WIN32
  ptr res;
  wchar_t *wpath;
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  __int64 total, sec; int nsec;
  
  if ((wpath = S_malloc_wide_pathname(inpath)) == NULL) {
    res = S_LastErrorString();
  } else if (!GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata)) {
    DWORD err = GetLastError();
    res = err == ERROR_FILE_NOT_FOUND || err == ERROR_PATH_NOT_FOUND ?
          Sstring("no such file or directory") :
          S_LastErrorString();
  } else {
    total = filedata.ftLastWriteTime.dwHighDateTime;
    total <<= 32;
    total |= filedata.ftLastWriteTime.dwLowDateTime;
    sec = total / 10000000 - 11644473600L;
    nsec = (total % 10000000) * 100;
    res = Scons(Sinteger64(sec), Sinteger32(nsec));
  }
  free(wpath);
  return res;
#else /* WIN32 */
  ptr res;
  char *path;
  struct STATBUF statbuf;

  path = S_malloc_pathname(inpath);
  if ((followp ? STAT(path, &statbuf) : LSTAT(path, &statbuf)) != 0) {
    res = S_strerror(errno);
  } else {
    res = Scons(Sinteger64(SECMTIME(statbuf)), Sinteger32(NSECMTIME(statbuf)));
  }
  free(path);
  return res;
#endif /* WIN32 */
}

static ptr s_fd_atime(INT fd) {
  struct STATBUF statbuf;

  if (FSTAT(fd, &statbuf) != 0) return S_strerror(errno);

  return Scons(Sinteger64(SECATIME(statbuf)), Sinteger32(NSECATIME(statbuf)));
}

static ptr s_fd_ctime(INT fd) {
  struct STATBUF statbuf;

  if (FSTAT(fd, &statbuf) != 0) return S_strerror(errno);

  return Scons(Sinteger64(SECCTIME(statbuf)), Sinteger32(NSECCTIME(statbuf)));
}

static ptr s_fd_mtime(INT fd) {
  struct STATBUF statbuf;

  if (FSTAT(fd, &statbuf) != 0) return S_strerror(errno);

  return Scons(Sinteger64(SECMTIME(statbuf)), Sinteger32(NSECMTIME(statbuf)));
}

static IBOOL s_fd_regularp(INT fd) {
  struct STATBUF statbuf;
  
  if (FSTAT(fd, &statbuf) != 0) return 0;

  return statbuf.st_mode & S_IFREG;
}

static void s_nanosleep(ptr xsec, ptr xnsec) {
  ptr tc = get_thread_context();
  U64 sec = Sunsigned64_value(xsec);
  U32 nsec = Sunsigned32_value(xnsec);
#ifdef PTHREADS
  if (DISABLECOUNT(tc) == 0) {
    deactivate_thread(tc)
  }
#endif /* PTHREADS */
 /* give up our lightweight thread "quanta" */
  if (DISABLECOUNT(tc) == 0) {
    TRAP(get_thread_context()) = (ptr)1;
  }
#ifdef WIN32
 /* round to nearest ms represented by sec and nsec */
  Sleep((DWORD)(sec * 1000 + (nsec + 500000) / 1000000));
#else /* WIN32 */
  struct timespec rqtp;
  rqtp.tv_sec = sec;
  rqtp.tv_nsec = nsec;
  nanosleep(&rqtp, NULL);
#endif /* WIN32 */
#ifdef PTHREADS
  if (DISABLECOUNT(tc) == 0) {
    reactivate_thread(tc)
  }
#endif /* PTHREADS */
}

static int s_getpid(void) {
  return GETPID();
}

static ptr s_set_collect_trip_bytes(ptr n) {
    S_G.collect_trip_bytes = Sunsigned_value(n);
    return Svoid;
}

static void c_exit(I32 status) {
    exit(status);
}

static double s_mod(double x, double y) { return fmod(x, y); }

static double s_exp(double x) { return exp(x); }

static double s_log(double x) { return log(x); }

static double s_log2(double x, double y) { return log(x) / log(y); }

#if (machine_type == machine_type_i3fb || machine_type == machine_type_ti3fb)
#include <ieeefp.h>
/* freebsd's pow delivers precise results for integer inputs, e.g.,
 * 10.0^21.0, only with * extended-precision (80-bit) floats */
static double s_pow(double x, double y) {
  fp_prec_t p;
  p = fpgetprec();
  if (p != FP_PE) {
    double ans;
    fpsetprec(FP_PE);
    ans = pow(x, y);
    fpsetprec(p);
    return ans;
  } else
    return pow(x, y);
}
#elif (machine_type == machine_type_i3osx || machine_type == machine_type_ti3osx)
/* intel macosx delivers accurate results for integer inputs, e.g.,
 * 10.0^21.0, only with long double version of pow; it's not clear
 * whether that has been true for x86_64 as opposed to i386, but as
 * of macOS 10.15 (Catalina), pow seems ok for x86_64 */
static double s_pow(double x, double y) { return powl(x, y); }
#else /* i3fb/ti3fb/i3osx/ti3osx */
static double s_pow(double x, double y) { return pow(x, y); }
#endif /* i3fb/ti3fb/i3osx/ti3osx */

#ifdef __MINGW32__
/* asinh() and atanh() sometimes get zero sign wrong */
# define CHECK_ASINTAN_ZERO(x, e) ((x == 0.0) \
                                   ? (signbit(x) ? -0.0 : 0.0)  \
                                   : e)
/* see also "mingw.zuo" */
#else
# define CHECK_ASINTAN_ZERO(x, e) e
#endif

static double s_sqrt(double x) { return sqrt(x); }

static double s_sin(double x) { return sin(x); }

static double s_cos(double x) { return cos(x); }

static double s_tan(double x) { return tan(x); }

static double s_asin(double x) { return asin(x); }

static double s_acos(double x) { return acos(x); }

static double s_atan(double x) { return atan(x); }

static double s_atan2(double x, double y) { return atan2(x, y); }

static double s_sinh(double x) { return sinh(x); }

static double s_cosh(double x) { return cosh(x); }

static double s_tanh(double x) { return tanh(x); }

static double s_floor(double x) { return floor(x); }

static double s_ceil(double x) { return ceil(x); }

static double s_round(double x) { return rint(x); }

static double s_trunc(double x) { return trunc(x); }

static double s_hypot(double x, double y) { return HYPOT(x, y); }

#ifdef ARCHYPERBOLIC
static double s_asinh(double x) { return CHECK_ASINTAN_ZERO(x, asinh(x)); }

static double s_acosh(double x) { return acosh(x); }

static double s_atanh(double x) { return CHECK_ASINTAN_ZERO(x, atanh(x)); }
#endif /* ARCHHYPERBOLIC */

#ifdef LOG1P
static double s_log1p(double x) { return log1p(x); }
#endif /* LOG1P */

static ptr s_getenv(char *name) {
#ifdef WIN32
  char *s = Sgetenv(name);
#else /* WIN32 */
  char *s = getenv(name);
#endif /* WIN32 */
  if (s == (char *)0)
    return Sfalse;
  else {
    ptr r = Sstring_utf8(s, -1);
#ifdef WIN32
    free(s);
#endif
    return r;
  }
}

static void s_putenv(char *name, char *value) {
#ifdef WIN32
  wchar_t* namew;
  wchar_t* valuew;
  BOOL rc;
  namew = Sutf8_to_wide(name);
  valuew = Sutf8_to_wide(value);
  rc = SetEnvironmentVariableW(namew, valuew);
  free(namew);
  free(valuew);
  if (rc == 0)
    S_error1("putenv", "environment extension failed: ~a", S_LastErrorString());
#else /* WIN32 */
  if (setenv(name, value, 1) != 0) {
    ptr msg = S_strerror(errno);

    if (msg != Sfalse)
      S_error1("putenv", "environment extension failed: ~a", msg);
    else
      S_error("putenv", "environment extension failed");
  }
#endif /* WIN32 */
}

#ifdef PTHREADS
/* backdoor thread is for testing thread creation by Sactivate_thread */
#define display(s) do { const char *S = (s); if (WRITE(1, S, (unsigned int)strlen(S))) {} } while(0)
static s_thread_rv_t s_backdoor_thread_start(void *p_in) {
  ptr p = TO_PTR(p_in);
  display("backdoor thread started\n");
  (void) Sactivate_thread();
  display("thread activated\n");
  Scall0(Sboxp(p) ? Sunbox(p) : p);
  (void) Sdeactivate_thread();
  display("thread deactivated\n");
  (void) Sactivate_thread();
  display("thread reactivated\n");
  Scall0(Sboxp(p) ? Sunbox(p) : p);
  Sdestroy_thread();
  display("thread destroyed\n");
  s_thread_return;
}

static iptr s_backdoor_thread(ptr p) {
  display("creating thread\n");
  return s_thread_create(s_backdoor_thread_start, TO_VOIDP(p));
}

static ptr s_threads() {
  ptr ts;
  tc_mutex_acquire();
  ts = S_threads;
  tc_mutex_release();
  return ts;
}

static void s_mutex_acquire(ptr m_p) {
  scheme_mutex_t *m = TO_VOIDP(m_p);
  ptr tc = get_thread_context();

  if (m == &S_tc_mutex) {
    S_mutex_acquire(m);
    return;
  }

  if (S_mutex_tryacquire(m) == 0) return;

  if (DISABLECOUNT(tc) == 0) {
    deactivate_thread(tc)
  }
  S_mutex_acquire(m);
  if (DISABLECOUNT(tc) == 0) {
    reactivate_thread(tc)
  }
}

static ptr s_mutex_acquire_noblock(ptr m_p) {
  scheme_mutex_t *m = TO_VOIDP(m_p);
  return S_mutex_tryacquire(m) == 0 ? Strue : Sfalse;
}

static void s_mutex_release(ptr m) {
  S_mutex_release(TO_VOIDP(m));
}

static IBOOL s_mutex_is_owner(ptr m) {
  return S_mutex_is_owner(TO_VOIDP(m));
}

static void s_condition_broadcast(ptr c_p) {
  s_thread_cond_t *c = TO_VOIDP(c_p);
  s_thread_cond_broadcast(c);
}

static void s_condition_signal(ptr c_p) {
  s_thread_cond_t *c = TO_VOIDP(c_p);
  s_thread_cond_signal(c);
}

static void s_condition_free(ptr c) {
  S_condition_free(TO_VOIDP(c));
}

static IBOOL s_condition_wait(ptr c, ptr m, ptr t) {
  return S_condition_wait(TO_VOIDP(c), TO_VOIDP(m), t);
}


/* called with tc mutex held */
static void s_thread_preserve_ownership(ptr tc) {
  if (!THREAD_GC(tc)->preserve_ownership) {
    THREAD_GC(tc)->preserve_ownership = 1;
    S_num_preserve_ownership_threads++;
  }
}

#endif

static ptr s_profile_counters(void) {
  return S_G.profile_counters;
}

/* s_profile_release_counters assumes and maintains the property that each pair's
   tail is not younger than the pair and thereby avoids dirty sets. */
static ptr s_profile_release_counters(void) {
  ptr tossed, *p_keep, *p_toss, ls;
  p_keep = &S_G.profile_counters;
  p_toss = &tossed;
  for (ls = *p_keep; ls != Snil && (MaybeSegInfo(ptr_get_segment(ls)))->generation <= S_G.prcgeneration; ls = Scdr(ls)) {
    if (Sbwp_objectp(CAAR(ls))) {
      *p_toss = ls;
      p_toss = &Scdr(ls);
    } else {
      *p_keep = ls;
      p_keep = &Scdr(ls);
    }
  }
  *p_keep = ls;
  *p_toss = Snil;
  S_G.prcgeneration = 0;
  return tossed;
}

void S_dump_tc(ptr tc) {
  INT i;

  printf("AC0=%p AC1=%p SFP=%p CP=%p\n", TO_VOIDP(AC0(tc)), TO_VOIDP(AC1(tc)), TO_VOIDP(SFP(tc)), TO_VOIDP(CP(tc)));
  printf("ESP=%p AP=%p EAP=%p\n", TO_VOIDP(ESP(tc)), TO_VOIDP(AP(tc)), TO_VOIDP(EAP(tc)));
  printf("TRAP=%p XP=%p YP=%p REAL_EAP=%p\n", TO_VOIDP(TRAP(tc)), TO_VOIDP(XP(tc)), TO_VOIDP(YP(tc)), TO_VOIDP(REAL_EAP(tc)));
  printf("CCHAIN=%p RANDOMSEED=%ld SCHEMESTACK=%p STACKCACHE=%p\n", TO_VOIDP(CCHAIN(tc)), (long)RANDOMSEED(tc),
         TO_VOIDP(SCHEMESTACK(tc)), TO_VOIDP(STACKCACHE(tc)));
  printf("STACKLINK=%p SCHEMESTACKSIZE=%ld WINDERS=%p U=%p\n", TO_VOIDP(STACKLINK(tc)), (long)SCHEMESTACKSIZE(tc), TO_VOIDP(WINDERS(tc)), TO_VOIDP(U(tc)));
  printf("V=%p W=%p X=%p Y=%p\n", TO_VOIDP(V(tc)), TO_VOIDP(W(tc)), TO_VOIDP(X(tc)), TO_VOIDP(Y(tc)));
  printf("SOMETHING=%p KBDPEND=%p SIGPEND=%p TIMERTICKS=%p\n", TO_VOIDP(SOMETHINGPENDING(tc)), TO_VOIDP(KEYBOARDINTERRUPTPENDING(tc)),
         TO_VOIDP(SIGNALINTERRUPTPENDING(tc)), TO_VOIDP(TIMERTICKS(tc)));
  printf("DISABLECOUNT=%p PARAMETERS=%p\n", TO_VOIDP(DISABLECOUNT(tc)), TO_VOIDP(PARAMETERS(tc)));
  for (i = 0 ; i < virtual_register_count ; i += 1) {
    printf("VIRTREG[%d]=%p", i, TO_VOIDP(VIRTREG(tc, i)));
    if ((i & 0x11) == 0x11 || i == virtual_register_count - 1) printf("\n");
  }
  fflush(stdout);
}

static IBOOL s_native_little_endian() {
#define big 0
#define little 1
#ifdef PORTABLE_BYTECODE
# ifdef PORTABLE_BYTECODE_BIGENDIAN
#  define unknown big
# else
#  define unknown little
# endif
#endif
  return native_endianness == little;
}

#define proc2ptr(x) TO_PTR(x)

void S_prim5_init(void) {
    if (!S_boot_time) return;

#ifdef PTHREADS
    Sforeign_symbol("(cs)fork_thread", (void *)S_fork_thread);
    Sforeign_symbol("(cs)make_mutex", (void *)S_make_mutex);
    Sforeign_symbol("(cs)mutex_free", (void *)S_mutex_free);
    Sforeign_symbol("(cs)backdoor_thread", (void *)s_backdoor_thread);
    Sforeign_symbol("(cs)threads", (void *)s_threads);
    Sforeign_symbol("(cs)mutex_acquire", (void *)s_mutex_acquire);
    Sforeign_symbol("(cs)mutex_release", (void *)s_mutex_release);
    Sforeign_symbol("(cs)mutex_acquire_noblock", (void *)s_mutex_acquire_noblock);
    Sforeign_symbol("(cs)mutex_is_owner", (void *)s_mutex_is_owner);
    Sforeign_symbol("(cs)make_condition", (void *)S_make_condition);
    Sforeign_symbol("(cs)condition_free", (void *)s_condition_free);
    Sforeign_symbol("(cs)condition_broadcast", (void *)s_condition_broadcast);
    Sforeign_symbol("(cs)condition_signal", (void *)s_condition_signal);
    Sforeign_symbol("(cs)condition_wait", (void *)s_condition_wait);
    Sforeign_symbol("(cs)thread_preserve_ownership", (void *)s_thread_preserve_ownership);
#endif
    Sforeign_symbol("(cs)s_addr_in_heap", (void *)s_addr_in_heap);
    Sforeign_symbol("(cs)s_ptr_in_heap", (void *)s_ptr_in_heap);
    Sforeign_symbol("(cs)generation", (void *)s_generation);
    Sforeign_symbol("(cs)s_fltofx", (void *)s_fltofx);
    Sforeign_symbol("(cs)s_weak_cons", (void *)S_weak_cons);
    Sforeign_symbol("(cs)s_weak_pairp", (void *)s_weak_pairp);
    Sforeign_symbol("(cs)s_ephemeron_cons", (void *)s_ephemeron_cons);
    Sforeign_symbol("(cs)s_ephemeron_pairp", (void *)s_ephemeron_pairp);
    Sforeign_symbol("(cs)box_immobile", (void *)s_box_immobile);
    Sforeign_symbol("(cs)make_immobile_vector", (void *)s_make_immobile_vector);
    Sforeign_symbol("(cs)make_immobile_bytevector", (void *)s_make_immobile_bytevector);
    Sforeign_symbol("(cs)s_make_reference_bytevector", (void *)s_make_reference_bytevector);
    Sforeign_symbol("(cs)s_make_immobile_reference_bytevector", (void *)s_make_immobile_reference_bytevector);
    Sforeign_symbol("(cs)s_reference_bytevectorp", (void *)s_reference_bytevectorp);
    Sforeign_symbol("(cs)s_reference_star_address_object", (void *)s_reference_star_address_object);
    Sforeign_symbol("(cs)s_bytevector_reference_star_ref", (void *)s_bytevector_reference_star_ref);
    Sforeign_symbol("(cs)continuation_depth", (void *)S_continuation_depth);
    Sforeign_symbol("(cs)single_continuation", (void *)S_single_continuation);
    Sforeign_symbol("(cs)c_exit", (void *)c_exit);
    Sforeign_symbol("(cs)s_set_collect_trip_bytes", (void *)s_set_collect_trip_bytes);
    Sforeign_symbol("(cs)s_oblist", (void *)s_oblist);
    Sforeign_symbol("(cs)s_showalloc", (void *)s_showalloc);
    Sforeign_symbol("(cs)s_system", (void *)s_system);
    Sforeign_symbol("(cs)s_process", (void *)s_process);
    Sforeign_symbol("(cs)s_set_code_byte", (void *)s_set_code_byte);
    Sforeign_symbol("(cs)s_set_code_word", (void *)s_set_code_word);
    Sforeign_symbol("(cs)s_set_code_long", (void *)s_set_code_long);
    Sforeign_symbol("(cs)s_set_code_quad", (void *)s_set_code_quad);
    Sforeign_symbol("(cs)s_set_reloc", (void *)s_set_reloc);
    Sforeign_symbol("(cs)get_code_obj", (void *)S_get_code_obj);
    Sforeign_symbol("(cs)s_flush_instruction_cache", (void *)s_flush_instruction_cache);
    Sforeign_symbol("(cs)s_make_reloc_table", (void *)s_make_reloc_table);
    Sforeign_symbol("(cs)s_make_closure", (void *)s_make_closure);
    Sforeign_symbol("(cs)s_intern", (void *)s_intern);
    Sforeign_symbol("(cs)s_intern2", (void *)s_intern2);
    Sforeign_symbol("(cs)s_intern3", (void *)s_intern3);
    Sforeign_symbol("(cs)s_strings_to_gensym", (void *)s_strings_to_gensym);
    Sforeign_symbol("(cs)s_intern_gensym", (void *)S_intern_gensym);
    Sforeign_symbol("(cs)s_uninterned", (void *)S_uninterned);
    Sforeign_symbol("(cs)symbol_hash32", (void *)S_symbol_hash32);
    Sforeign_symbol("(cs)symbol_hash64", (void *)S_symbol_hash64);
    Sforeign_symbol("(cs)cputime", (void *)S_cputime);
    Sforeign_symbol("(cs)realtime", (void *)S_realtime);
    Sforeign_symbol("(cs)clock_gettime", (void *)S_clock_gettime);
    Sforeign_symbol("(cs)gmtime", (void *)S_gmtime);
    Sforeign_symbol("(cs)asctime", (void *)S_asctime);
    Sforeign_symbol("(cs)mktime", (void *)S_mktime);
    Sforeign_symbol("(cs)unique_id", (void *)S_unique_id);
    Sforeign_symbol("(cs)file_existsp", (void *)S_file_existsp);
    Sforeign_symbol("(cs)file_regularp", (void *)S_file_regularp);
    Sforeign_symbol("(cs)file_directoryp", (void *)S_file_directoryp);
    Sforeign_symbol("(cs)file_symbolic_linkp", (void *)S_file_symbolic_linkp);
    Sforeign_symbol("(cs)delete_file", (void *)s_delete_file);
    Sforeign_symbol("(cs)delete_directory", (void *)s_delete_directory);
    Sforeign_symbol("(cs)rename_file", (void *)s_rename_file);
    Sforeign_symbol("(cs)mkdir", (void *)s_mkdir);
    Sforeign_symbol("(cs)chmod", (void *)s_chmod);
    Sforeign_symbol("(cs)getmod", (void *)s_getmod);
    Sforeign_symbol("(cs)path_atime", (void *)s_path_atime);
    Sforeign_symbol("(cs)path_ctime", (void *)s_path_ctime);
    Sforeign_symbol("(cs)path_mtime", (void *)s_path_mtime);
    Sforeign_symbol("(cs)fd_atime", (void *)s_fd_atime);
    Sforeign_symbol("(cs)fd_ctime", (void *)s_fd_ctime);
    Sforeign_symbol("(cs)fd_mtime", (void *)s_fd_mtime);
    Sforeign_symbol("(cs)fd_regularp", (void *)s_fd_regularp);
    Sforeign_symbol("(cs)nanosleep", (void *)s_nanosleep);
    Sforeign_symbol("(cs)getpid", (void *)s_getpid);
    Sforeign_symbol("(cs)fasl_read", (void *)S_fasl_read);
    Sforeign_symbol("(cs)bv_fasl_read", (void *)S_bv_fasl_read);
    Sforeign_symbol("(cs)vfasl_to", (void *)S_vfasl_to);
    Sforeign_symbol("(cs)s_decode_float", (void *)s_decode_float);

    Sforeign_symbol("(cs)new_open_input_fd", (void *)S_new_open_input_fd);
    Sforeign_symbol("(cs)new_open_output_fd", (void *)S_new_open_output_fd);
    Sforeign_symbol("(cs)new_open_input_output_fd", (void *)S_new_open_input_output_fd);
    Sforeign_symbol("(cs)close_fd", (void *)S_close_fd);
    Sforeign_symbol("(cs)gzxfile_fd", (void *)S_gzxfile_fd);
    Sforeign_symbol("(cs)compress_input_fd", (void *)S_compress_input_fd);
    Sforeign_symbol("(cs)compress_output_fd", (void *)S_compress_output_fd);

    Sforeign_symbol("(cs)bytevector_read", (void*)S_bytevector_read);
    Sforeign_symbol("(cs)bytevector_read_nb", (void*)S_bytevector_read_nb);
    Sforeign_symbol("(cs)bytevector_write", (void*)S_bytevector_write);
    Sforeign_symbol("(cs)put_byte", (void*)S_put_byte);
    Sforeign_symbol("(cs)get_fd_pos", (void*)S_get_fd_pos);
    Sforeign_symbol("(cs)set_fd_pos", (void*)S_set_fd_pos);
    Sforeign_symbol("(cs)get_fd_non_blocking", (void*)S_get_fd_non_blocking);
    Sforeign_symbol("(cs)set_fd_non_blocking", (void*)S_set_fd_non_blocking);
    Sforeign_symbol("(cs)get_fd_length", (void*)S_get_fd_length);
    Sforeign_symbol("(cs)set_fd_length", (void*)S_set_fd_length);

    Sforeign_symbol("(cs)bytevector_compress_size", (void*)S_bytevector_compress_size);
    Sforeign_symbol("(cs)bytevector_compress", (void*)S_bytevector_compress);
    Sforeign_symbol("(cs)bytevector_uncompress", (void*)S_bytevector_uncompress);

    Sforeign_symbol("(cs)phantom_bytevector_adjust", (void*)S_phantom_bytevector_adjust);

    Sforeign_symbol("(cs)native_little_endian", (void *)s_native_little_endian);

    Sforeign_symbol("(cs)logand", (void *)S_logand);
    Sforeign_symbol("(cs)logbitp", (void *)S_logbitp);
    Sforeign_symbol("(cs)logbit0", (void *)S_logbit0);
    Sforeign_symbol("(cs)logbit1", (void *)S_logbit1);
    Sforeign_symbol("(cs)logtest", (void *)S_logtest);
    Sforeign_symbol("(cs)logor", (void *)S_logor);
    Sforeign_symbol("(cs)logxor", (void *)S_logxor);
    Sforeign_symbol("(cs)lognot", (void *)S_lognot);
    Sforeign_symbol("(cs)fxmul", (void *)s_fxmul);
    Sforeign_symbol("(cs)fxdiv", (void *)s_fxdiv);
    Sforeign_symbol("(cs)s_big_negate", (void *)S_big_negate);
    Sforeign_symbol("(cs)add", (void *)S_add);
    Sforeign_symbol("(cs)gcd", (void *)S_gcd);
    Sforeign_symbol("(cs)mul", (void *)S_mul);
    Sforeign_symbol("(cs)s_ash", (void *)S_ash);
    Sforeign_symbol("(cs)s_big_positive_bit_field", (void *)S_big_positive_bit_field);
    Sforeign_symbol("(cs)s_big_eq", (void *)S_big_eq);
    Sforeign_symbol("(cs)s_big_lt", (void *)S_big_lt);
    Sforeign_symbol("(cs)s_big_trailing_zero_bits", (void *)S_big_trailing_zero_bits);
    Sforeign_symbol("(cs)s_bigoddp", (void *)s_bigoddp);
    Sforeign_symbol("(cs)s_div", (void *)S_div);
    Sforeign_symbol("(cs)s_float", (void *)s_float);
    Sforeign_symbol("(cs)s_flrandom", (void *)s_flrandom);
    Sforeign_symbol("(cs)s_fxrandom", (void *)s_fxrandom);
    Sforeign_symbol("(cs)s_random_state_next_integer", (void *)S_random_state_next_integer);
    Sforeign_symbol("(cs)s_random_state_next_double", (void *)S_random_state_next_double);
    Sforeign_symbol("(cs)s_random_state_init", (void *)S_random_state_init);
    Sforeign_symbol("(cs)s_random_state_check", (void *)S_random_state_check);
    Sforeign_symbol("(cs)s_integer_length", (void *)S_integer_length);
    Sforeign_symbol("(cs)s_big_first_bit_set", (void *)S_big_first_bit_set);
    Sforeign_symbol("(cs)s_make_code", (void *)s_make_code);
    Sforeign_symbol("(cs)s_random_seed", (void *)s_random_seed);
    Sforeign_symbol("(cs)s_set_code_long2", (void *)s_set_code_long2);
    Sforeign_symbol("(cs)s_set_random_seed", (void *)s_set_random_seed);
    Sforeign_symbol("(cs)ss_trunc", (void *)S_trunc);
    Sforeign_symbol("(cs)ss_trunc_rem", (void *)s_trunc_rem);
    Sforeign_symbol("(cs)s_rational", (void *)S_rational);
    Sforeign_symbol("(cs)sub", (void *)S_sub);
    Sforeign_symbol("(cs)rem", (void *)S_rem);
#if defined(GETWD) || defined(__GNU__) /* Hurd */
    Sforeign_symbol("(cs)s_getwd", (void *)s_getwd);
#endif
    Sforeign_symbol("(cs)s_chdir", (void *)s_chdir);
#ifdef WIN32
    Sforeign_symbol("(cs)find_files", (void *)S_find_files);
#else
    Sforeign_symbol("(cs)directory_list", (void *)S_directory_list);
#endif
    Sforeign_symbol("(cs)dequeue_scheme_signals", (void *)S_dequeue_scheme_signals);
    Sforeign_symbol("(cs)register_scheme_signal", (void *)S_register_scheme_signal);

    Sforeign_symbol("(cs)mod", (void *)s_mod);
    Sforeign_symbol("(cs)exp", (void *)s_exp);
    Sforeign_symbol("(cs)log", (void *)s_log);
    Sforeign_symbol("(cs)pow", (void *)s_pow);
    Sforeign_symbol("(cs)sqrt", (void *)s_sqrt);
    Sforeign_symbol("(cs)sin", (void *)s_sin);
    Sforeign_symbol("(cs)cos", (void *)s_cos);
    Sforeign_symbol("(cs)tan", (void *)s_tan);
    Sforeign_symbol("(cs)asin", (void *)s_asin);
    Sforeign_symbol("(cs)acos", (void *)s_acos);
    Sforeign_symbol("(cs)atan", (void *)s_atan);
    Sforeign_symbol("(cs)atan2", (void *)s_atan2);
    Sforeign_symbol("(cs)sinh", (void *)s_sinh);
    Sforeign_symbol("(cs)cosh", (void *)s_cosh);
    Sforeign_symbol("(cs)tanh", (void *)s_tanh);
    Sforeign_symbol("(cs)floor", (void *)s_floor);
    Sforeign_symbol("(cs)ceil", (void *)s_ceil);
    Sforeign_symbol("(cs)hypot", (void *)s_hypot);

#ifdef ARCHYPERBOLIC
    Sforeign_symbol("(cs)asinh", (void *)s_asinh);
    Sforeign_symbol("(cs)acosh", (void *)s_acosh);
    Sforeign_symbol("(cs)atanh", (void *)s_atanh);
#endif /* ARCHHYPERBOLIC */

#ifdef LOG1P
    Sforeign_symbol("(cs)log1p", (void *)s_log1p);
#endif /* LOG1P */

    Sforeign_symbol("(cs)s_get_reloc", (void *)s_get_reloc);
    Sforeign_symbol("(cs)getenv", (void *)s_getenv);
    Sforeign_symbol("(cs)putenv", (void *)s_putenv);
    Sforeign_symbol("(cs)byte-copy", (void *)s_byte_copy);
    Sforeign_symbol("(cs)ptr-copy", (void *)s_ptr_copy);
    Sforeign_symbol("(cs)boot-error", (void *)S_boot_error);
    Sforeign_symbol("(cs)s_tlv", (void *)s_tlv);
    Sforeign_symbol("(cs)s_stlv", (void *)s_stlv);
    Sforeign_symbol("(cs)s_test_schlib", (void *)s_test_schlib);
    Sforeign_symbol("(cs)Sinteger_value", (void *)Sinteger_value);
    Sforeign_symbol("(cs)Sinteger32_value", (void *)Sinteger32_value);
    Sforeign_symbol("(cs)Sinteger64_value", (void *)Sinteger64_value);
    Sforeign_symbol("(cs)s_breakhere", (void *)s_breakhere);
    Sforeign_symbol("(cs)s_interactivep", (void *)s_interactivep);
    Sforeign_symbol("(cs)same_devicep", (void *)s_same_devicep);
    Sforeign_symbol("(cs)malloc", (void *)s_malloc);
    Sforeign_symbol("(cs)free", (void *)s_free);
#ifdef FEATURE_ICONV
    Sforeign_symbol("(cs)s_iconv_open", (void *)s_iconv_open);
    Sforeign_symbol("(cs)s_iconv_close", (void *)s_iconv_close);
    Sforeign_symbol("(cs)s_iconv_from_string", (void *)s_iconv_from_string);
    Sforeign_symbol("(cs)s_iconv_to_string", (void *)s_iconv_to_string);
#endif
    Sforeign_symbol("(cs)s_strerror", (void *)S_strerror);
    Sforeign_symbol("(cs)s_errno", (void *)s_errno);
#ifdef WIN32
    Sforeign_symbol("(cs)s_multibytetowidechar", (void *)s_multibytetowidechar);
    Sforeign_symbol("(cs)s_widechartomultibyte", (void *)s_widechartomultibyte);
#endif
#ifdef PORTABLE_BYTECODE
    Sforeign_symbol("(cs)s_separatorchar", (void *)s_separatorchar);
#endif
    Sforeign_symbol("(cs)s_profile_counters", (void *)s_profile_counters);
    Sforeign_symbol("(cs)s_profile_release_counters", (void *)s_profile_release_counters);

    S_install_c_entry(CENTRY_flfloor, proc2ptr(s_floor));
    S_install_c_entry(CENTRY_flceiling, proc2ptr(s_ceil));
    S_install_c_entry(CENTRY_flround, proc2ptr(s_round));
    S_install_c_entry(CENTRY_fltruncate, proc2ptr(s_trunc));
    S_install_c_entry(CENTRY_flsin, proc2ptr(s_sin));
    S_install_c_entry(CENTRY_flcos, proc2ptr(s_cos));
    S_install_c_entry(CENTRY_fltan, proc2ptr(s_tan));
    S_install_c_entry(CENTRY_flasin, proc2ptr(s_asin));
    S_install_c_entry(CENTRY_flacos, proc2ptr(s_acos));
    S_install_c_entry(CENTRY_flatan, proc2ptr(s_atan));
    S_install_c_entry(CENTRY_flatan2, proc2ptr(s_atan2));
    S_install_c_entry(CENTRY_flexp, proc2ptr(s_exp));
    S_install_c_entry(CENTRY_fllog, proc2ptr(s_log));
    S_install_c_entry(CENTRY_fllog2, proc2ptr(s_log2));
    S_install_c_entry(CENTRY_flexpt, proc2ptr(s_pow));
    S_install_c_entry(CENTRY_flsqrt, proc2ptr(s_sqrt));

    S_check_c_entry_vector();
}

static ptr s_get_reloc(ptr co, IBOOL with_offsets) {
  ptr t, ls; uptr a, m, n;

  require(Scodep(co),"s_get_reloc","~s is not a code object",co);

  if (s_generation(co) == FIX(static_generation))
    return Snil;

  ls = Snil;
  t = CODERELOC(co);
  m = RELOCSIZE(t);
  a = 0;
  n = 0;
  while (n < m) {
    uptr entry, item_off, code_off; ptr obj;
    entry = RELOCIT(t, n); n += 1;
    if (RELOC_EXTENDED_FORMAT(entry)) {
      item_off = RELOCIT(t, n); n += 1;
      code_off = RELOCIT(t, n); n += 1;
    } else {
      item_off = RELOC_ITEM_OFFSET(entry);
      code_off = RELOC_CODE_OFFSET(entry);
    }
    a += code_off;
    obj = S_get_code_obj(RELOC_TYPE(entry), co, a, item_off);
    if (!Sfixnump(obj)) {
      if (with_offsets) {
        ls = Scons(Scons(obj, FIX(a-code_data_disp)), ls);
      } else {
        ptr x;
        for (x = ls; ; x = Scdr(x)) {
          if (x == Snil) {
            ls = Scons(obj,ls);
            break;
          } else if (Scar(x) == obj)
            break;
        }
      }
    }
  }
  return ls;
}

static void s_byte_copy(ptr src, iptr srcoff, ptr dst, iptr dstoff, iptr cnt) {
  void *srcaddr = TO_VOIDP((iptr)src + srcoff);
  void *dstaddr = TO_VOIDP((iptr)dst + dstoff);
  if (dst != src)
     memcpy(dstaddr, srcaddr, cnt);
  else
     memmove(dstaddr, srcaddr, cnt);
}

static void s_ptr_copy(ptr src, iptr srcoff, ptr dst, iptr dstoff, iptr cnt) {
  void *srcaddr = TO_VOIDP((iptr)src + srcoff);
  void *dstaddr = TO_VOIDP((iptr)dst + dstoff);
  cnt = cnt << log2_ptr_bytes;
  if (dst != src)
     memcpy(dstaddr, srcaddr, cnt);
  else
     memmove(dstaddr, srcaddr, cnt);
}

/* these are used only for testing */
static ptr s_tlv(ptr x) {
  return Stop_level_value(x);
}

static void s_stlv(ptr x, ptr v) {
  Sset_top_level_value(x, v);
}

#define SCHLIBTEST(expr) {\
  test += 1;\
  if (!(expr)) S_error1("s_test_schlib", "test ~s failed", FIX(test));\
}

static void s_test_schlib(void) {
  INT test = 0;
  I32 n1 = 0x73215609;
  I64 n2 = n1 * 37;
  I32 n3 = (I32)1<<31;
  I64 n4 = (I64)1<<63;
  I32 n5 = -1;

  SCHLIBTEST(Sinteger_value(Sinteger(n1)) == n1)
  SCHLIBTEST(Sinteger_value(Sinteger(-n1)) == -n1)
  SCHLIBTEST(Sinteger_value(Sunsigned(n1)) == n1)
  SCHLIBTEST(Sinteger_value(Sunsigned(-n1)) == -n1)
  SCHLIBTEST(Sinteger32_value(Sinteger32(n1)) == n1)
  SCHLIBTEST(Sinteger32_value(Sinteger32(-n1)) == -n1)
  SCHLIBTEST(Sinteger32_value(Sunsigned32(n1)) == n1)
  SCHLIBTEST(Sinteger32_value(Sunsigned32(-n1)) == -n1)
  SCHLIBTEST(Sinteger64_value(Sinteger64(n1)) == n1)
  SCHLIBTEST(Sinteger64_value(Sinteger64(-n1)) == -n1)
  SCHLIBTEST(Sinteger64_value(Sunsigned64(n1)) == n1)
  SCHLIBTEST(Sinteger64_value(Sunsigned64(-n1)) == -n1)
#if (ptr_bits == 64)
  SCHLIBTEST(Sinteger_value(Sinteger(n2)) == n2)
  SCHLIBTEST(Sinteger_value(Sinteger(-n2)) == -n2)
  SCHLIBTEST(Sinteger_value(Sunsigned(n2)) == n2)
  SCHLIBTEST(Sinteger_value(Sunsigned(-n2)) == -n2)
#endif
  SCHLIBTEST(Sinteger64_value(Sinteger64(n2)) == n2)
  SCHLIBTEST(Sinteger64_value(Sinteger64(-n2)) == -n2)
  SCHLIBTEST(Sinteger64_value(Sunsigned64(n2)) == n2)
  SCHLIBTEST(Sinteger64_value(Sunsigned64(-n2)) == -n2)

  SCHLIBTEST(Sinteger_value(Sinteger(n3)) == n3)
  SCHLIBTEST(Sinteger_value(Sunsigned(n3)) == n3)
  SCHLIBTEST(Sinteger32_value(Sinteger32(n3)) == n3)
  SCHLIBTEST(Sinteger32_value(Sunsigned32(n3)) == n3)
  SCHLIBTEST(Sinteger64_value(Sinteger64(n3)) == n3)
  SCHLIBTEST(Sinteger64_value(Sunsigned64(n3)) == n3)
#if (ptr_bits == 64)
  SCHLIBTEST(Sinteger_value(Sunsigned(n4)) == n4)
  SCHLIBTEST(Sinteger_value(Sinteger(n4)) == n4)
  SCHLIBTEST(Sinteger_value(Sunsigned(n4)) == n4)
#endif
  SCHLIBTEST(Sinteger64_value(Sinteger64(n4)) == n4)
  SCHLIBTEST(Sinteger64_value(Sunsigned64(n4)) == n4)

  SCHLIBTEST(Sinteger_value(Sinteger(n5)) == n5)
  SCHLIBTEST(Sinteger_value(Sinteger(-n5)) == -n5)
  SCHLIBTEST(Sinteger_value(Sunsigned(n5)) == n5)
  SCHLIBTEST(Sinteger_value(Sunsigned(-n5)) == -n5)
  SCHLIBTEST(Sinteger32_value(Sinteger32(n5)) == n5)
  SCHLIBTEST(Sinteger32_value(Sinteger32(-n5)) == -n5)
  SCHLIBTEST(Sinteger32_value(Sunsigned32(n5)) == n5)
  SCHLIBTEST(Sinteger32_value(Sunsigned32(-n5)) == -n5)
  SCHLIBTEST(Sinteger64_value(Sinteger64(n5)) == n5)
  SCHLIBTEST(Sinteger64_value(Sinteger64(-n5)) == -n5)
  SCHLIBTEST(Sinteger64_value(Sunsigned64(n5)) == n5)
  SCHLIBTEST(Sinteger64_value(Sunsigned64(-n5)) == -n5)
}

/* place to break when debugging */
static void s_breakhere(UNUSED ptr x) {
  return;
}

static IBOOL s_interactivep(void) {
  static INT interactivep = -1;
  if (interactivep == -1) {
#ifdef WIN32
    HANDLE hStdout, hStdin; 
    CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 
    DWORD InMode, OutMode;
    interactivep =
       (hStdin = GetStdHandle(STD_INPUT_HANDLE)) != INVALID_HANDLE_VALUE
       && (hStdout = GetStdHandle(STD_OUTPUT_HANDLE)) != INVALID_HANDLE_VALUE
       && GetConsoleScreenBufferInfo(hStdout, &csbiInfo)
       && GetConsoleMode(hStdin, &InMode)
       && GetConsoleMode(hStdout, &OutMode);
#else /* WIN32 */
    interactivep = isatty(0) && isatty(1);
#endif /* WIN32 */
  }
  return interactivep;
}

static IBOOL s_same_devicep(INT fd1, INT fd2) {
#ifdef WIN32
  HANDLE h1, h2; DWORD mode1, mode2;
  if ((h1 = (HANDLE)_get_osfhandle(fd1)) != INVALID_HANDLE_VALUE) 
    if ((h2 = (HANDLE)_get_osfhandle(fd2)) != INVALID_HANDLE_VALUE)
      switch (GetFileType(h1)) {
        case FILE_TYPE_CHAR:
          if (GetFileType(h2) == FILE_TYPE_CHAR)
            return GetConsoleMode(h1, &mode1) && GetConsoleMode(h2, &mode2);
          break;
        case FILE_TYPE_DISK:
          if (GetFileType(h2) == FILE_TYPE_DISK) {
            BY_HANDLE_FILE_INFORMATION info1, info2;
            if (GetFileInformationByHandle(h1, &info1) && GetFileInformationByHandle(h1, &info2))
              return info1.dwVolumeSerialNumber == info2.dwVolumeSerialNumber 
                  && info1.nFileIndexHigh == info2.nFileIndexHigh 
                  && info1.nFileIndexLow == info2.nFileIndexLow;
          }
          break;
        case FILE_TYPE_PIPE:
         /* no clue */
          break;
        default: break;
      }
#else /* WIN32 */
  struct STATBUF statbuf1, statbuf2;
  if (FSTAT(fd1, &statbuf1) == 0 && FSTAT(fd2, &statbuf2) == 0)
    return statbuf1.st_ino == statbuf2.st_ino;
#endif /* WIN32 */

  return 0;
}

static uptr s_malloc(iptr n) {
  void *p;
  if ((p = malloc((size_t)n)) == NULL) {
    ptr msg = S_strerror(errno);

    if (msg != Sfalse)
      S_error1("foreign-alloc", "~a", msg);
    else
      S_error("foreign-alloc", "malloc failed");
  }
  return (uptr)TO_PTR(p);
}

static void s_free(uptr addr) {
  free(TO_VOIDP(addr));
}

#ifdef FEATURE_ICONV
#ifdef DISABLE_ICONV
# define iconv_t int
#define ICONV_OPEN(to, from) (use_sink(to), use_sink(from), -1)
#define ICONV(cd, in, inb, out, outb) (use_sinki(cd), use_sink(in), use_sink(inb), use_sink(out), use_sink(outb), -1)
#define ICONV_CLOSE(cd) (use_sinki(cd), -1)
static void use_sink(UNUSED const void *p) { }
static void use_sinki(UNUSED int i) { }
#elif defined(WIN32)
typedef void *iconv_t;
typedef iconv_t (*iconv_open_ft)(const char *tocode, const char *fromcode);
typedef size_t (*iconv_ft)(iconv_t cd, char **inbuf, size_t *inbytesleft, char **outbuf, size_t *outbytesleft);
typedef int (*iconv_close_ft)(iconv_t cd);

static iconv_open_ft iconv_open_f = (iconv_open_ft)0;
static iconv_ft iconv_f = (iconv_ft)0;
static iconv_close_ft iconv_close_f = (iconv_close_ft)0;
#define ICONV_OPEN iconv_open_f
#define ICONV iconv_f
#define ICONV_CLOSE iconv_close_f
#else
#include <iconv.h>
#define ICONV_OPEN iconv_open
#define ICONV iconv
#define ICONV_CLOSE iconv_close
#endif

#ifdef WIN32
static ptr s_iconv_trouble(HMODULE h, const char *what) {
  wchar_t dllw[PATH_MAX];
  char *dll;
  size_t n;
  char *msg;
  ptr r;
  if (0 != GetModuleFileNameW(h, dllw, PATH_MAX))
    dll = Swide_to_utf8(dllw);
  else
    dll = NULL;
  FreeLibrary(h);
  n = strlen(what) + strlen(dll) + 17;
  msg = (char *)malloc(n);
  sprintf(msg, "cannot find %s in %s", what, dll);
  free(dll);
  r = Sstring_utf8(msg, -1);
  free(msg);
  return r;
}
#endif /* WIN32 */

static ptr s_iconv_open(const char *tocode, const char *fromcode) {
  iconv_t cd;
#ifdef WIN32
  static int iconv_is_loaded = 0;
  if (!iconv_is_loaded) {
    HMODULE h = LoadLibraryW(L"iconv.dll");
    if (h == NULL) h = LoadLibraryW(L"libiconv.dll");
    if (h == NULL) h = LoadLibraryW(L"libiconv-2.dll");
    if (h == NULL) h = LoadLibraryW(L".\\iconv.dll");
    if (h == NULL) h = LoadLibraryW(L".\\libiconv.dll");
    if (h == NULL) h = LoadLibraryW(L".\\libiconv-2.dll");
    if (h == NULL) return Sstring("cannot load iconv.dll, libiconv.dll, or libiconv-2.dll");
    if ((iconv_open_f = (void *)GetProcAddress(h, "iconv_open")) == NULL &&
        (iconv_open_f = (void *)GetProcAddress(h, "libiconv_open")) == NULL)
      return s_iconv_trouble(h, "iconv_open or libiconv_open");
    if ((iconv_f = (void *)GetProcAddress(h, "iconv")) == NULL &&
        (iconv_f = (void *)GetProcAddress(h, "libiconv")) == NULL)
      return s_iconv_trouble(h, "iconv or libiconv");
    if ((iconv_close_f = (void *)GetProcAddress(h, "iconv_close")) == NULL &&
        (iconv_close_f = (void *)GetProcAddress(h, "libiconv_close")) == NULL)
      return s_iconv_trouble(h, "iconv_close or libiconv_close");
    iconv_is_loaded = 1;
  }
#endif /* WIN32 */

  if ((cd = ICONV_OPEN(tocode, fromcode)) == (iconv_t)-1) return Sfalse;

 /* have to be able to cast to int, since iconv_open can return (iconv_t)-1 */
  return Sunsigned((uptr)cd);
}

static void s_iconv_close(uptr cd) {
  ICONV_CLOSE((iconv_t)cd);
}

#ifdef DISTRUST_ICONV_PROGRESS
# define ICONV_FROM iconv_fixup
static size_t iconv_fixup(iconv_t cd, char **src, size_t *srcleft, char **dst, size_t *dstleft) {
  size_t r;
  char *orig_src = *src, *orig_dst = *dst;
  size_t orig_srcleft = *srcleft, orig_dstleft = *dstleft, srcuntried = 0;

  while (1) {
    r = ICONV((iconv_t)cd, src, srcleft, dst, dstleft);
    if ((r == (size_t)-1)
        && (errno == E2BIG)
        && ((*srcleft < orig_srcleft) || (*dstleft < orig_dstleft))) {
      /* Avoid a macOS (as of 14.2.1 and 14.3.1) iconv bug in this
         case, where we don't trust that consumed input characters are
         reflected in the output pointer. Reverting progress should be
         ok for a correct iconv, too, since a -1 result means that no
         irreversible progress was made. */
      *src = orig_src;
      *dst = orig_dst;
      *srcleft = orig_srcleft;
      *dstleft = orig_dstleft;

      /* We need to make progress, if possible, to satify normal iconv
         behavior and "io.ss" expectations. Try converting fewer
         characters. */
      if (orig_srcleft > sizeof(string_char)) {
        size_t try_chars = (orig_srcleft / sizeof(string_char)) / 2;
        srcuntried += orig_srcleft - (try_chars * sizeof(string_char));
        orig_srcleft = try_chars * sizeof(string_char);
        *srcleft = orig_srcleft;
      } else
        break;
    } else
      break;
  }

  *srcleft += srcuntried;

  return r;
}
#else
# define ICONV_FROM ICONV
#endif

#define ICONV_BUFSIZ 400

static ptr s_iconv_from_string(uptr cd, ptr in, uptr i, uptr iend, ptr out, uptr o, uptr oend) {
  U32 buf[ICONV_BUFSIZ];
  char *inbuf, *outbuf;
  size_t inbytesleft, outbytesleft;
  uptr inmax, k, new_i, new_o;

  outbuf = TO_VOIDP(&BVIT(out, o));
  outbytesleft = oend - o;

  inmax = iend - i;
  if (inmax > ICONV_BUFSIZ) inmax = ICONV_BUFSIZ;
  if (inmax > outbytesleft) inmax = outbytesleft;
  for (k = 0; k < inmax; k += 1) buf[k] = Sstring_ref(in, i + k);

  inbuf = (char *)buf;
  inbytesleft = inmax * sizeof(string_char);

 /* we ignore the iconv return value because we consider success to be the consumption
    of input or production of output.  we set errno to 0 before calling iconv, even though
    it should be set properly if neither input is consumed nor output is produced, because,
    under Windows, the iconv dll might have been linked against a different C runtime
    and might therefore set a different errno */
  errno = 0;
  ICONV_FROM((iconv_t)cd, (ICONV_INBUF_TYPE)&inbuf, &inbytesleft, &outbuf, &outbytesleft);

  new_i = i + inmax - inbytesleft / sizeof(string_char);
  new_o = oend - outbytesleft;
  if (new_i != i || new_o != o) return Scons(Sinteger(new_i), Sinteger(new_o));

  switch (errno) {
    case EILSEQ: return FIX(SICONV_INVALID);
    case EINVAL: return FIX(SICONV_INCOMPLETE);
    case E2BIG: return FIX(SICONV_NOROOM);
    default: return FIX(SICONV_DUNNO);
  }
}

static ptr s_iconv_to_string(uptr cd, ptr in, uptr i, uptr iend, ptr out, uptr o, uptr oend) {
  U32 buf[ICONV_BUFSIZ];
  char *inbuf, *outbuf;
  size_t inbytesleft, outbytesleft;
  uptr outmax, k, new_i, new_o;
  
  inbuf = TO_VOIDP(&BVIT(in, i));
  inbytesleft = iend - i;

  outmax = oend - o;
  if (outmax > ICONV_BUFSIZ) outmax = ICONV_BUFSIZ;
  if (outmax > inbytesleft) outmax = inbytesleft;

  outbuf = (char *)buf;
  outbytesleft = outmax * sizeof(string_char);

 /* see the comment about the iconv return value and errno in s_iconv_from_string */
  errno = 0;
  ICONV((iconv_t)cd, (ICONV_INBUF_TYPE)&inbuf, &inbytesleft, &outbuf, &outbytesleft);

  outmax -= outbytesleft / sizeof(string_char);
  for (k = 0; k < outmax; k += 1) Sstring_set(out, o + k, buf[k]);
  new_i = iend - inbytesleft;
  new_o = o + outmax;
  if (new_i != i || new_o != o) return Scons(Sinteger(new_i), Sinteger(new_o));

  switch (errno) {
    case EILSEQ: return FIX(SICONV_INVALID);
    case EINVAL: return FIX(SICONV_INCOMPLETE);
    case E2BIG: return FIX(SICONV_NOROOM);
    default: return FIX(SICONV_DUNNO);
  }
}
#endif /* FEATURE_ICONV */

#ifdef WIN32
static ptr s_multibytetowidechar(unsigned cp, ptr inbv) {
  uptr inbytes; int outwords; ptr outbv;
  
  inbytes = Sbytevector_length(inbv);

#if (ptr_bits > int_bits)
  if ((uptr)(int)inbytes != inbytes) S_error1("multibyte->string", "input size ~s is beyond MultiByteToWideChar's limit", Sinteger(inbytes));
#endif

  if ((outwords = MultiByteToWideChar(cp, 0, (char *)&BVIT(inbv,0), (int)inbytes, NULL, 0)) == 0)
    S_error1("multibyte->string", "conversion failed: ~a", S_LastErrorString());

  outbv = S_bytevector(outwords * 2);

  if (MultiByteToWideChar(cp, 0, (char *)&BVIT(inbv,0), (int)inbytes, (wchar_t *)&BVIT(outbv, 0), outwords) == 0)
    S_error1("multibyte->string", "conversion failed: ~a", S_LastErrorString());
  
  return outbv;
}

static ptr s_widechartomultibyte(unsigned cp, ptr inbv) {
  uptr inwords; int outbytes; ptr outbv;

  inwords = Sbytevector_length(inbv) / 2;

#if (ptr_bits > int_bits)
  if ((uptr)(int)inwords != inwords) S_error1("multibyte->string", "input size ~s is beyond WideCharToMultiByte's limit", Sinteger(inwords));
#endif

  if ((outbytes = WideCharToMultiByte(cp, 0, (wchar_t *)&BVIT(inbv,0), (int)inwords, NULL, 0, NULL, NULL)) == 0)
    S_error1("string->multibyte", "conversion failed: ~a", S_LastErrorString());

  outbv = S_bytevector(outbytes);

  if (WideCharToMultiByte(cp, 0, (wchar_t *)&BVIT(inbv,0), (int)inwords, (char *)&BVIT(outbv, 0), outbytes, NULL, NULL) == 0)
    S_error1("string->multibyte", "conversion failed: ~a", S_LastErrorString());
  
  return outbv;  
}
#endif /* WIN32 */

#ifdef PORTABLE_BYTECODE
static ptr s_separatorchar() {
#ifdef WIN32
  return Schar(';');
#else
  return Schar(':');
#endif
}
#endif
