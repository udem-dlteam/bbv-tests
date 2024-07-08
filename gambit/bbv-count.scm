(##c-declare #<<EOF
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
static int64_t ___jumpif_count = 0;
static int64_t ___string_in_bounds_count = 0;
static int64_t ___vector_in_bounds_count = 0;
static int64_t ___vector_length_count = 0;
static int64_t ___string_length_count = 0;
static void ___exiting() {
  #include <inttypes.h>
  printf("***primitive-call-counter");
  printf("%s\n", __FILE__);
  printf("(#gvm:ifjump %" PRId64 ")\n", ___jumpif_count);
  printf("(##string-in-bounds %" PRId64 ")\n", ___string_in_bounds_count);
  printf("(##vector-in-bounds %" PRId64 ")\n", ___vector_in_bounds_count);
  printf("(##vector-length %" PRId64 ")\n", ___vector_length_count);
  printf("(##string-length %" PRId64 ")\n", ___string_length_count);
}
#undef ___IF
#define ___IF(x) { ___jumpif_count++; if (x) {
#undef ___END_IF
#define ___END_IF }}
#undef ___STRINGINBOUNDSP
#undef ___VECTORINBOUNDSP
#undef ___VECTORLENGTH
#undef ___STRINGLENGTH
#define ___STRINGINBOUNDSP(v,i) ( ___string_in_bounds_count++, (___CAST(___UWORD,___INT(i))<___STRINGSIZE(v)))
#define ___VECTORINBOUNDSP(v,i) ( ___vector_in_bounds_count++, (___CAST(___UWORD,___INT(i))<___VECTORSIZE(v)))
#define ___VECTORLENGTH(v) ( ___vector_length_count++, ___FIX(___VECTORSIZE(v)))
#define ___STRINGLENGTH(v) ( ___string_length_count++, ___FIX(___STRINGSIZE(v)))
EOF
)

(##c-initialize #<<EOF
atexit(___exiting);
EOF
)
