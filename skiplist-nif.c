#include <string.h>
#include <stdio.h>

#include "erl_nif.h"
#include "skiplist.h"

typedef struct erl_node
{
    unsigned int rank_id;
    skiplist* sl;
    struct erl_node* next;
} erl_node;

typedef struct erl_global
{
    struct erl_node* header;
    struct erl_node* last;
    int rank_count;
} erl_global;

static erl_global* get_global(ErlNifEnv* env){
    erl_global* g = (erl_global*)enif_priv_data(env);
    return g;
}

static erl_node* get_node(ErlNifEnv* env, unsigned int rank_id){
    erl_global* g = get_global(env);
    if(NULL==g){
        return NULL;
    }
    erl_node* p = g->header;
    while(p){
        if(p->rank_id == rank_id){
            return p;
        }
        p = p->next;
    }
    return NULL;
}

static erl_node* create_node(unsigned int rank_id){
    erl_node* node = (erl_node*)enif_alloc(sizeof(erl_node));
    node->rank_id = rank_id;
    node->sl = NULL;
    node->next = NULL;
    return node;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    erl_global* g = (erl_global*)enif_alloc(sizeof(erl_global));
    if(NULL == g){
        return 1;
    }
    erl_node* node = create_node(0);
    g->header = node;
    g->last = g->header;
    g->rank_count = 0;

    *priv_data = g;
    return 0;
}

static ERL_NIF_TERM _create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id;

    if(!enif_get_uint(env, argv[0], &rank_id)){
        return enif_make_badarg(env);
    }

    erl_node* _check_node = get_node(env, rank_id);
    if (NULL != _check_node){
        return enif_make_badarg(env);
    }

    erl_node* node = create_node(rank_id);
    node->sl = slCreate();

    erl_global* g = get_global(env);
    if(NULL == g){
        return enif_make_badarg(env);
    }

    g->last->next = node;
    g->last = node;
    g->rank_count++;

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM _destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id;

    if(!enif_get_uint(env, argv[0], &rank_id)){
        return enif_make_badarg(env);
    }

    erl_node* _check_node = get_node(env, rank_id);
    if (NULL == _check_node){
        return enif_make_badarg(env);
    }

    erl_global* g = get_global(env);
    if(NULL==g){
        return enif_make_badarg(env);
    }

    if(g->rank_count<=0){
        return enif_make_badarg(env);
    }

    erl_node* p;
    erl_node* q;
    q=g->header;
    p=q->next;
    while (p)
    {
        if(p->rank_id==rank_id){
            break;
        }else{
            p=p->next;
            q=q->next;
        }
    }

    q->next = p->next;
    if(g->last==p){
        g->last = q;
    }
    slFree(p->sl);
    enif_free(p);

    g->rank_count--;

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM _insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id;
    double score;
    ErlNifBinary name;

    if(!enif_get_uint(env, argv[0], &rank_id)
        || !enif_get_double(env, argv[1], &score)
            || !enif_inspect_iolist_as_binary(env, argv[2], &name)){
        return enif_make_badarg(env);
    }
    erl_node* node = get_node(env, rank_id);
    if (NULL == node){
        return enif_make_badarg(env);
    }
    skiplist* sl = node->sl;

    char* pstr = (char*)enif_alloc(name.size+1);
    memcpy(pstr, name.data, name.size);
    *(pstr + name.size) = '\0';

    slobj *obj = slCreateObj(pstr, name.size);
    slInsert(sl, score, obj);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM _delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id;
    double score;
    ErlNifBinary name;

    if(!enif_get_uint(env, argv[0], &rank_id)
        || !enif_get_double(env, argv[1], &score)
            || !enif_inspect_iolist_as_binary(env, argv[2], &name)){
        return enif_make_badarg(env);
    }
    erl_node* node = get_node(env, rank_id);
    if (NULL == node){
        return enif_make_badarg(env);
    }
    skiplist* sl = node->sl;

    char* pstr = (char*)enif_alloc(name.size+1);
    memcpy(pstr, name.data, name.size);
    *(pstr + name.size) = '\0';

    slobj *obj = slCreateObj(pstr, name.size);
    int ret = slDelete(sl, score, obj);
    enif_free(pstr);
    slFreeObj(obj);

    return enif_make_int(env, ret);
}

typedef struct delete_cb_ud {
    ErlNifEnv* env;
    ERL_NIF_TERM* ret;
} delete_cb_ud;

static void _delete_rank_cb(void* ud, slobj* obj){
    delete_cb_ud* tmp_ud = (delete_cb_ud*)ud;
    ErlNifEnv* env = tmp_ud->env;
    ERL_NIF_TERM* ret = tmp_ud->ret;
    *ret = enif_make_list_cell(env, enif_make_string(env, obj->ptr, ERL_NIF_LATIN1), *ret);
}

static ERL_NIF_TERM _delete_by_rank(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id, start, end;

    if(!enif_get_uint(env, argv[0], &rank_id)
        || !enif_get_uint(env, argv[1], &start)
            || !enif_get_uint(env, argv[2], &end)){
        return enif_make_badarg(env);
    }
    erl_node* node = get_node(env, rank_id);
    if (NULL == node){
        return enif_make_badarg(env);
    }
    skiplist* sl = node->sl;
    if(start > end){
        unsigned int tmp = start;
        start = end;
        end = tmp;
    }

    ERL_NIF_TERM ret = enif_make_list(env, 0);
    delete_cb_ud tmp_ud = {env, &ret};
    slDeleteByRank(sl, start, end, _delete_rank_cb, &tmp_ud);
    return ret;
}

static ERL_NIF_TERM _get_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id;

    if(!enif_get_uint(env, argv[0], &rank_id)){
        return enif_make_badarg(env);
    }
    erl_node* node = get_node(env, rank_id);
    if (NULL == node){
        return enif_make_badarg(env);
    }
    skiplist* sl = node->sl;
    return enif_make_int(env, sl->length);
}

static ERL_NIF_TERM _get_rank(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id;
    double score;
    ErlNifBinary name;

    if(!enif_get_uint(env, argv[0], &rank_id)
        || !enif_get_double(env, argv[1], &score)
            || !enif_inspect_iolist_as_binary(env, argv[2], &name)){
        return enif_make_badarg(env);
    }
    erl_node* node = get_node(env, rank_id);
    if (NULL == node){
        return enif_make_badarg(env);
    }
    skiplist* sl = node->sl;

    char* pstr = (char*)enif_alloc(name.size+1);
    memcpy(pstr, name.data, name.size);
    *(pstr + name.size) = '\0';

    slobj *obj = slCreateObj(pstr, name.size);
    int ret = slGetRank(sl, score, obj);
    slFreeObj(obj);
    enif_free(pstr);

    return enif_make_int(env, ret);
}

static ERL_NIF_TERM _get_rank_range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id;
    unsigned long r1, r2;

    if(!enif_get_uint(env, argv[0], &rank_id)
        || !enif_get_ulong(env, argv[1], &r1)
            || !enif_get_ulong(env, argv[2], &r2)){
        return enif_make_badarg(env);
    }
    erl_node* node = get_node(env, rank_id);
    if (NULL == node){
        return enif_make_badarg(env);
    }
    skiplist* sl = node->sl;

    int reverse, rangelen;
    if(r1 <= r2) {
        reverse = 0;
        rangelen = r2 - r1 + 1;
    } else {
        reverse = 1;
        rangelen = r1 - r2 + 1;
    }

    skiplistNode* snode = slGetNodeByRank(sl, r1);
    ERL_NIF_TERM ret = enif_make_list(env, 0);
    int n = 0;
    while(snode && n < rangelen) {
        n++;
        ret = enif_make_list_cell(env, enif_make_string(env, snode->obj->ptr, ERL_NIF_LATIN1), ret);
        snode = reverse? snode->backward : snode->level[0].forward;
    }
    return ret;
}

static ERL_NIF_TERM _get_score_range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id;
    double s1, s2;
    if(!enif_get_uint(env, argv[0], &rank_id)
        || !enif_get_double(env, argv[1], &s1)
            || !enif_get_double(env, argv[2], &s2)){
        return enif_make_badarg(env);
    }
    erl_node* node = get_node(env, rank_id);
    if (NULL == node){
        return enif_make_badarg(env);
    }
    skiplist* sl = node->sl;

    int reverse;
    skiplistNode *snode;

    if(s1 <= s2) {
        reverse = 0;
        snode = slFirstInRange(sl, s1, s2);
    } else {
        reverse = 1;
        snode = slLastInRange(sl, s2, s1);
    }

    ERL_NIF_TERM ret = enif_make_list(env, 0);
    int n = 0;
    while(snode) {
        if(reverse) {
            if(snode->score < s2) break;
        } else {
            if(snode->score > s2) break;
        }
        n++;

        ret = enif_make_list_cell(env, enif_make_string(env, snode->obj->ptr, ERL_NIF_LATIN1), ret);
        snode = reverse? snode->backward:snode->level[0].forward;
    }
    return ret;
}

static ERL_NIF_TERM _get_member_by_rank(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id;
    unsigned long r;
    if(!enif_get_uint(env, argv[0], &rank_id) || !enif_get_ulong(env, argv[1], &r)){
        return enif_make_badarg(env);
    }

    erl_node* node = get_node(env, rank_id);
    if (NULL == node){
        return enif_make_badarg(env);
    }
    skiplist* sl = node->sl;

    skiplistNode *snode = slGetNodeByRank(sl, r);
    if (snode) {
        return enif_make_string(env, snode->obj->ptr, ERL_NIF_LATIN1);
    }
    return enif_make_atom(env, "fail");
}

static ERL_NIF_TERM _dump(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
    unsigned int rank_id;

    if(!enif_get_uint(env, argv[0], &rank_id)){
        return enif_make_badarg(env);
    }

    erl_node* node = get_node(env, rank_id);
    if(NULL == node){
        return enif_make_badarg(env);
    }
    skiplist* sl = node->sl;
    slDump(sl);

    erl_global* g = get_global(env);
    printf("rank_count = %d \n", g->rank_count);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM _dump_all(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    erl_global* g = get_global(env);
    printf("rank_count = %d \n", g->rank_count);

    erl_node* p = g->header->next;
    while (p)
    {
        printf("rank_id = %d \n", p->rank_id);
        skiplist* sl = p->sl;
        slDump(sl);
        // ERL_NIF_TERM argv[] = {enif_make_uint(env, p->rank_id)};
        // _dump(env, 1, argv);
        p=p->next;
    }

    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"create", 1, _create},
    {"destroy", 1, _destroy},

    {"insert", 3, _insert},
    {"delete", 3, _delete},
    {"delete_by_rank", 3, _delete_by_rank},

    {"get_count", 1, _get_count},
    {"get_rank", 3, _get_rank},
    {"get_rank_range", 3, _get_rank_range},
    {"get_score_range", 3, _get_score_range},
    {"get_member_by_rank", 2, _get_member_by_rank},

    {"dump", 1, _dump},
    {"dump_all", 0, _dump_all},
};

ERL_NIF_INIT(skiplist, nif_funcs, load, NULL, NULL, NULL)