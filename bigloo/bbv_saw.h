/*=====================================================================*/
/*    serrano/prgm/project/bbv-tests/bigloo/bbv_saw.h                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Mar 16 18:48:21 1995                          */
/*    Last change :  Thu Jan 11 11:22:45 2024 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Bigloo's stuff                                                   */
/*=====================================================================*/
#ifndef SAW_BBV_H 
#define SAW_BBV_H

#if (defined(SAW_BBV_STATS))
static long saw_ifne = 0;
static long saw_ifeq = 0;
static long saw_eq = 0;
static long saw_ge = 0;
static long saw_le = 0;
static long saw_gt = 0;
static long saw_lt = 0;
static long saw_add = 0;
static long saw_sub = 0;
static long saw_mul = 0;
static long saw_div = 0;
static long saw_rem = 0;
static long saw_addfx_ov = 0;
static long saw_subfx_ov = 0;
static long saw_mulfx_ov = 0;
static long saw_pairp = 0;
static long saw_procedurep = 0;
static long saw_fixnump = 0;
static long saw_flonump = 0;

#  undef BGL_RTL_IFNE
#  define BGL_RTL_IFNE(l,r) if (saw_ifne++, r) goto l

#  undef BGL_RTL_IFEQ
#  define BGL_RTL_IFEQ(l,r) if (saw_ifeq++, !r) goto l

#  undef BGL_RTL_EQ
#  define BGL_RTL_EQ(x,y) (saw_eq++, (x == y))

#  undef BGL_RTL_GE
#  define BGL_RTL_GE(x,y) (saw_ge++, (x >= y))

#  undef BGL_RTL_LE
#  define BGL_RTL_LE(x,y) (saw_le++, (x <= y))

#  undef BGL_RTL_GT
#  define BGL_RTL_GT(x,y) (saw_gt++, (x > y))

#  undef BGL_RTL_LT
#  define BGL_RTL_LT(x,y) (saw_lt++, (x < y))

#  undef BGL_RTL_ADD
#  define BGL_RTL_ADD(x,y) (saw_add++, (x + y))

#  undef BGL_RTL_SUB
#  define BGL_RTL_SUB(x,y) (saw_sub++,(x - y))

#  undef BGL_RTL_MUL
#  define BGL_RTL_MUL(x,y) (saw_mul++,(x * y))

#  undef BGL_RTL_DIV
#  define BGL_RTL_DIV(x,y) (saw_div++,(x / y))

#  undef BGL_RTL_REM
#  define BGL_RTL_REM(x,y) (x % y)

#  undef BGL_RTL_XOR
#  define BGL_RTL_XOR(x,y) (x ^ y)

#  undef BGL_RTL_RSH
#  define BGL_RTL_RSH(x,y) (x >> y)

#  undef BGL_RTL_LSH
#  define BGL_RTL_LSH(x,y) (x << y)

#  undef BGL_ADDFX_OV
#  define BGL_ADDFX_OV(x, y, res) (saw_addfx_ov++, __builtin_saddl_overflow((long)x, (long)y, (long*)&res))

#  undef BGL_SUBFX_OV
#  define BGL_SUBFX_OV(x, y, res) (saw_subfx_ov++, __builtin_ssubl_overflow((long)x, (long)y, (long*)&res))

#  undef BGL_MULFX_OV
#  define BGL_MULFX_OV(x, y, res) (saw_mulfx_ov++, __builtin_smull_overflow((long)x, (long)y, (long*)&res))

#  undef BGL_ADDFX_SANS_OV
#  define BGL_ADDFX_SANS_OV(x, y) (saw_add++, BINT(CINT(x) + CINT(y)))

#  undef BGL_SUBFX_SANS_OV
#  define BGL_SUBFX_SANS_OV(x, y) (saw_sub++, BINT(CINT(x) - CINT(y)))

#  undef BGL_MULFX_SANS_OV
#  define BGL_MULFX_SANS_OV(x, y) (saw_mul++, BINT(CINT(x) * CINT(y)))

#  undef PAIRP
#  if( defined( TAG_PAIR ) )
#   if( TAG_PAIR != 0 )
#      define PAIRP( c ) (saw_pairp++, (((long)c) & TAG_MASK) == TAG_PAIR)
#   else
#      define PAIRP( c ) (saw_pairp++, (c && ((((long)c) & TAG_MASK) == TAG_PAIR)))
#   endif
#  else
#   define PAIRP( c ) (saw_pairp++, POINTERP( c ) && (TYPE( c ) == PAIR_TYPE))
#  endif

#  undef PROCEDUREP
#  define PROCEDUREP(o) \
     (saw_procedurep++, POINTERP(o) && (TYPE(o) == PROCEDURE_TYPE))

#  undef INTEGERP
#  define INTEGERP( o ) (saw_fixnump++, ((((long)o) & TAG_MASK) == TAG_INT))

#  undef FLONUMP
#  define FLONUMP( c ) (saw_flonump++, ((c && ((((long)c)&TAG_MASK) == TAG_REAL))))

int bbv_saw_statistics() {
   fprintf(stderr, "***primitive-call-counter\n");
   fprintf(stderr, "(ifne %ld)\n", saw_ifne + saw_ifeq);
   fprintf(stderr, "(eq %ld)\n", saw_eq);
   fprintf(stderr, "(lefx %ld)\n", saw_le);
   fprintf(stderr, "(gtfx %ld)\n", saw_gt);
   fprintf(stderr, "(ltfx %ld)\n", saw_lt);
   fprintf(stderr, "(addfx %ld)\n", saw_add);
   fprintf(stderr, "(subfx %ld)\n", saw_sub);
   fprintf(stderr, "(mulfx %ld)\n", saw_mul);
   fprintf(stderr, "(div %ld)\n", saw_div);
   fprintf(stderr, "(add/ov %ld)\n", saw_addfx_ov);
   fprintf(stderr, "(sub/ov %ld)\n", saw_subfx_ov);
   fprintf(stderr, "(mul/ov %ld)\n", saw_mulfx_ov);
   fprintf(stderr, "(pair? %ld)\n", saw_pairp);
   fprintf(stderr, "(procedure? %ld)\n", saw_procedurep);
   fprintf(stderr, "(fixnum? %ld)\n", saw_fixnump);
   fprintf(stderr, "(flonum? %ld)\n", saw_flonump);
   return 0;
}
#else
int bbv_saw_statistics() {
   return 0;
}
#endif

#endif

