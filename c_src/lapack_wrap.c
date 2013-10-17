#include "erl_nif.h"
#include "lapacke.h"


#define N 5 //должно быть равно количеству продуктов
#define NRHS 1
#define LDA N
#define LDB NRHS

void print_result( ErlNifEnv* env, lapack_int n, double* a ) {
  lapack_int i, j; // в си вообще есть рефлексия?...
  return enif_make_list(env, enif_make_double(env, a[0]), enif_make_double(env, a[1]));
  // иначе повторять по числу продуктов
}


static ERL_NIF_TERM solve(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  double *a; 
  double *b; 
  enif_get_list_cell(env, argv[0], a, null);
  
  enif_get_list_cell(env, argv[1], b, null);
  
  lapack_int n = N, nrhs = NRHS, lda = LDA, ldb = LDB, info;
  lapack_int ipiv[N];
  info = LAPACKE_dgesv( LAPACK_ROW_MAJOR, n, nrhs, a, lda, ipiv, b, ldb );
  
  if( info > 0 ) {
    printf( "The diagonal element of the triangular factor of A,\n" );
    printf( "U(%i,%i) is zero, so that A is singular;\n", info, info );
    printf( "the solution could not be computed.\n" );
    exit( 1 );
  }
  
  return create_matrix(n, nrhs, b, ldb);
}

static ErlNifFunc nif_funcs[] = {
  {"solve", 2, solve}
};

ERL_NIF_INIT(lapack_wrap, nif_funcs, NULL, NULL, NULL, NULL)
