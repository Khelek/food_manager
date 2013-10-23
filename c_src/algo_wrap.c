#include "erl_nif.h"
#include "lib/ap.h"
// disable some irrelevant warnings
#if (AE_COMPILER==AE_MSVC)
#pragma warning(disable:4100)
#pragma warning(disable:4127)
#pragma warning(disable:4702)
#pragma warning(disable:4996)
#endif
#include "lib/alglibinternal.h"
#include "lib/alglibmisc.h"
#include "lib/linalg.h"
#include "lib/solvers.h"

using namespace alglib_impl;


ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

void print_matrix( int m, int n, alglib::real_1d_array* a) {
  int i;
  int j;
  printf( "prints\n");
  for( i = 0; i < m; i++ ) {
    for( j = 0; j < n; j++ ) {
      printf( " %6.2f", a[i][j] );
    }
    printf( "\n" );
  }
}

static ERL_NIF_TERM c_solve_eq(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  alglib::real_2d_array A;
  alglib::ae_int_t nrows = 3;
  alglib::ae_int_t ncols = 5;
  alglib::real_1d_array B;
  double threshold = 0.0;
  alglib::ae_int_t info = 0;
  alglib::densesolverlsreport rep;
  alglib::real_1d_array x;
  A.setlength(nrows,ncols);
  B.setlength(nrows);
  A[0][0] = 1; A[0][1] = 2; A[0][2] = 3; A[0][3] = 4; A[0][4] = 5;
  A[1][0] = 3; A[1][1] = 4; A[1][2] = 2; A[1][3] = 5; A[1][4] = 6;
  A[2][0] = 1; A[2][1] = 3; A[2][2] = 2; A[2][3] = 5; A[2][4] = 2;

  B[0] = 98;
  B[1] = 66;
  B[2] = 59;

  alglib::rmatrixsolvels(A, nrows, ncols, B, threshold, info, rep, x);

  print_matrix(1, ncols, &x);
  
  if(argc != 2) {
    return enif_make_badarg(env);
  }
  return mk_atom(env, "oke"); //create_matrix(n, nrhs, b, ldb);
}

static ErlNifFunc nif_funcs[] = {
  {"c_solve_eq", 2, c_solve_eq}
};

ERL_NIF_INIT(algo_wrap, nif_funcs, NULL, NULL, NULL, NULL)
