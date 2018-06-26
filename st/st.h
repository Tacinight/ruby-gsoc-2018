#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <emmintrin.h>
#include <xmmintrin.h>
#include <malloc.h>
#include <assert.h>
#if defined(__cplusplus)
extern "C" {
#if 0
} /* satisfy cc-mode */
#endif
#endif

#define SIZEOF_LONG 8
#define SIZEOF_VOIDP 8
#if SIZEOF_LONG == SIZEOF_VOIDP
typedef unsigned long st_data_t;
#elif SIZEOF_LONG_LONG == SIZEOF_VOIDP
typedef unsigned LONG_LONG st_data_t;
#else
# error ---->> st.c requires sizeof(void*) == sizeof(long) or sizeof(LONG_LONG) to be compiled. <<----
#endif
#define ST_DATA_T_DEFINED

#ifndef CHAR_BIT
# ifdef HAVE_LIMITS_H
#  include <limits.h>
# else
#  define CHAR_BIT 8
# endif
#endif
#ifndef _
# define _(args) args
#endif
#ifndef ANYARGS
# ifdef __cplusplus
#   define ANYARGS ...
# else
#   define ANYARGS
# endif
#endif

typedef struct st_table st_table;
typedef struct st_bucket st_bucket;
typedef st_data_t st_index_t;
typedef unsigned long VALUE;

/* Maximal value of unsigned integer type st_index_t.  */
#define MAX_ST_INDEX_VAL (~(st_index_t) 0)

typedef int st_compare_func(st_data_t, st_data_t);
typedef st_index_t st_hash_func(st_data_t);

typedef char st_check_for_sizeof_st_index_t[SIZEOF_VOIDP == (int)sizeof(st_index_t) ? 1 : -1];
#define SIZEOF_ST_INDEX_T SIZEOF_VOIDP

struct st_hash_type {
    int (*compare)(ANYARGS /*st_data_t, st_data_t*/); /* st_compare_func* */
    st_index_t (*hash)(ANYARGS /*st_data_t*/);        /* st_hash_func* */
};

#define ST_INDEX_BITS (SIZEOF_ST_INDEX_T * CHAR_BIT)

#if defined(HAVE_BUILTIN___BUILTIN_CHOOSE_EXPR) && defined(HAVE_BUILTIN___BUILTIN_TYPES_COMPATIBLE_P)
# define ST_DATA_COMPATIBLE_P(type) \
   __builtin_choose_expr(__builtin_types_compatible_p(type, st_data_t), 1, 0)
#else
# define ST_DATA_COMPATIBLE_P(type) 0
#endif

#define CACHE_LINE_SIZE 64
#define ENTRIES_PER_BUCKET 2

#ifndef ALIGNED
#  if __GNUC__ && !SCC
#    define ALIGNED(N) __attribute__ ((aligned (N)))
#  else
#    define ALIGNED(N)
#  endif
#endif
typedef st_data_t st_lock_t;
typedef st_data_t st_hash_t;

typedef struct st_entry
{
    st_data_t hash;
    st_data_t key;
    st_data_t record;
} st_entry;

struct ALIGNED(CACHE_LINE_SIZE) st_bucket
{
    st_lock_t lock;
    st_entry entry[ENTRIES_PER_BUCKET];
    struct st_bucket* next;
};

struct ALIGNED(CACHE_LINE_SIZE) st_table
{
    union {
        struct {
            size_t version;
            st_index_t num_buckets;
            st_index_t num_entries;
            st_index_t num_expands;
            st_index_t num_expands_threshold;
            st_index_t entry_bound;
            volatile uint8_t resize_lock;
            const struct st_hash_type *type;
            st_table *table_new;
            st_index_t *ordered_entry;
            st_bucket* bucket;
        };
        uint8_t padding[1 * CACHE_LINE_SIZE];
    };
};

#define st_is_member(table,key) st_lookup((table),(key),(st_data_t *)0)

enum st_retval {ST_CONTINUE, ST_STOP, ST_DELETE, ST_CHECK};

st_table *st_init_table(const struct st_hash_type *);
st_table *st_init_table_with_size(const struct st_hash_type *, st_index_t);
st_table *st_init_numtable(void);
st_table *st_init_numtable_with_size(st_index_t);
st_table *st_init_strtable(void);
st_table *st_init_strtable_with_size(st_index_t);
st_table *st_init_strcasetable(void);
st_table *st_init_strcasetable_with_size(st_index_t);
int st_delete(st_table *, st_data_t *, st_data_t *); /* returns 0:notfound 1:deleted */
int st_delete_safe(st_table *, st_data_t *, st_data_t *, st_data_t);
int st_shift(st_table *, st_data_t *, st_data_t *); /* returns 0:notfound 1:deleted */
int st_insert(st_table *, st_data_t, st_data_t);
int st_insert2(st_table *, st_data_t, st_data_t, st_data_t (*)(st_data_t));
int st_lookup(st_table *, st_data_t, st_data_t *);
int st_get_key(st_table *, st_data_t, st_data_t *);
typedef int st_update_callback_func(st_data_t *key, st_data_t *value, st_data_t arg, int existing);
/* *key may be altered, but must equal to the old key, i.e., the
 * results of hash() are same and compare() returns 0, otherwise the
 * behavior is undefined */
int st_update(st_table *table, st_data_t key, st_update_callback_func *func, st_data_t arg);
int st_foreach(st_table *, int (*)(ANYARGS), st_data_t);
int st_foreach_check(st_table *, int (*)(ANYARGS), st_data_t, st_data_t);
st_index_t st_keys(st_table *table, st_data_t *keys, st_index_t size);
st_index_t st_keys_check(st_table *table, st_data_t *keys, st_index_t size, st_data_t never);
st_index_t st_values(st_table *table, st_data_t *values, st_index_t size);
st_index_t st_values_check(st_table *table, st_data_t *values, st_index_t size, st_data_t never);
void st_add_direct(st_table *, st_data_t, st_data_t);
void st_free_table(st_table *);
void st_cleanup_safe(st_table *, st_data_t);
void st_clear(st_table *);
st_table *st_copy(st_table *);
#define st_hash_start(h) ((st_index_t)(h))

void rb_hash_bulk_insert(long, const VALUE *, VALUE);


#if defined(__cplusplus)
#if 0
{ /* satisfy cc-mode */
#endif
}  /* extern "C" { */
#endif

#define ATOMIC_CAS(var, oldval, newval) __sync_val_compare_and_swap((var), (oldval), (newval))