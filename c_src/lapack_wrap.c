#include "erl_nif.h"

static ERL_NIF_TERM get(
        ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double load_avg[3];
    int loads;

    loads = getloadavg(load_avg, 3);
    if(loads != 3) {
        return enif_make_tuple(
                env, 2,
                enif_make_atom(env, "error"),
                enif_make_atom(env, "invalid_result") 
            );
    }

    return enif_make_tuple(
            env, 2,
            enif_make_atom(env, "ok"),
            enif_make_tuple(
                env, 3,
                enif_make_double(env, load_avg[0]),
                enif_make_double(env, load_avg[1]),
                enif_make_double(env, load_avg[2])
            )
        );
}

static ErlNifFunc nif_funcs[] = {
    {"get", 0, get}
};

ERL_NIF_INIT(load_average, nif_funcs, NULL, NULL, NULL, NULL)
