#include "st.h"
#include <stdint.h>

#define NOT_RUBY 1
#define FALSE 0
#define TRUE 1
#define LIKELY(x) (__builtin_expect(!!(x), 1))
#define UNLIKELY(x) (__builtin_expect(!!(x), 0))

#ifdef __GNUC__
#define PREFETCH(addr, write_p) __builtin_prefetch(addr, write_p)
#define EXPECT(expr, val) __builtin_expect(expr, val)
#define ATTRIBUTE_UNUSED  __attribute__((unused))
#else
#define PREFETCH(addr, write_p)
#define EXPECT(expr, val) (expr)
#define ATTRIBUTE_UNUSED
#endif

#define TAS_U8(a) tas_uint8(a)
#define HALVE_RATIO 2
#define SWAP_U64(a,b) swap_uint64(a,b)


#define TAS_RLS_MFENCE()
#define HELP_RESIZE 0

#define SMALL_TABLE_THRESHOLD 4

#define LOCK_FREE   0
#define LOCK_UPDATE 1
#define LOCK_RESIZE 2

#define IAF_U32(a) __sync_add_and_fetch(a,1)

#define LOCK_ACQ(lock, tab)			\
  lock_acq_chk_resize(lock, tab)

#define LOCK_RLS(lock)			\
  TAS_RLS_MFENCE();				\
  *lock = 0;

#define LOCK_ACQ_RES(lock)			\
  lock_acq_resize(lock)

#define TRYLOCK_ACQ(lock)			\
  TAS_U8(lock)

#define TRYLOCK_RLS(lock)			\
  lock = LOCK_FREE

static inline int
lock_acq_chk_resize(st_lock_t *lock, st_table *tab)
{
    char once = 1;
    st_lock_t l;
    while ((l = ATOMIC_CAS(lock, LOCK_FREE, LOCK_UPDATE)) == LOCK_UPDATE){
	if (once) {
	    once = 0;
	}
	_mm_pause();
    }

    if (l == LOCK_RESIZE) {
#if HELP_RESIZE == 1
	resize_help(tab)
#endif
	while (tab->table_new == NULL) {
	    _mm_pause();
	    _mm_mfence();
	}

	return 0;
    }
    return 1;
}

static inline int
lock_acq_resize(st_lock_t *lock)
{
    st_lock_t l;
    while ((l = ATOMIC_CAS(lock, LOCK_FREE, LOCK_UPDATE)) == LOCK_UPDATE) {
	_mm_pause();
    }

    if (l == LOCK_RESIZE) {
	return 0;
    }

    return 1;
}

static inline uint8_t
tas_uint8(volatile uint8_t *addr) {
    uint8_t oldval;
    __asm__ __volatile__("xchgb %0,%1"
        : "=q"(oldval), "=m"(*addr)
        : "0"((unsigned char) 0xff), "m"(*addr) : "memory");
    return (uint8_t)oldval;
}

//Swap uint64_t
static inline uint64_t 
swap_uint64(volatile uint64_t* target,  uint64_t x) {
    __asm__ __volatile__("xchgq %0,%1"
	:"=r" ((uint64_t) x)
	:"m" (*(volatile uint64_t *)target), "0" ((uint64_t) x)
	:"memory");
    return x;
}

#ifdef ST_DEBUG
#define st_assert assert
#else
#define st_assert(cond) ((void)(0 && (cond)))
#endif

/* The type of hashes.  */
typedef st_index_t st_hash_t;
int
st_numcmp(st_data_t x, st_data_t y)
{
    return x != y;
}

st_index_t
st_numhash(st_data_t n)
{
    enum {s1 = 11, s2 = 3};
    return (st_index_t)((n>>s1|(n<<s2)) ^ (n>>s2));
}

#define type_numhash st_hashtype_num
static const struct st_hash_type st_hashtype_num = {
    st_numcmp,
    st_numhash,
};

/* extern int strcmp(const char *, const char *); */
static st_index_t strhash(st_data_t);
static const struct st_hash_type type_strhash = {
    strcmp,
    strhash,
};

int
st_locale_insensitive_strcasecmp(const char *s1, const char *s2)
{
    unsigned int c1, c2;

    while (1) {
        c1 = (unsigned char)*s1++;
        c2 = (unsigned char)*s2++;
        if (c1 == '\0' || c2 == '\0') {
            if (c1 != '\0') return 1;
            if (c2 != '\0') return -1;
            return 0;
        }
        if ((unsigned int)(c1 - 'A') <= ('Z' - 'A')) c1 += 'a' - 'A';
        if ((unsigned int)(c2 - 'A') <= ('Z' - 'A')) c2 += 'a' - 'A';
        if (c1 != c2) {
            if (c1 > c2)
                return 1;
            else
                return -1;
        }
    }
}

static st_index_t strcasehash(st_data_t);
static const struct st_hash_type type_strcasehash = {
    st_locale_insensitive_strcasecmp,
    strcasehash,
};

/* Value used to catch uninitialized entries/bins during debugging.
   There is a possibility for a false alarm, but its probability is
   extremely small.  */
#define ST_INIT_VAL 0xafafafafafafafaf
#define ST_INIT_VAL_BYTE 0xafa

#define EQUAL(tab,x,y) ((x) == (y) || (*(tab)->type->compare)((x),(y)) == 0)
#define PTR_EQUAL(tab, ptr, hash_val, key_) \
    ((ptr)->hash == (hash_val) && EQUAL((tab), (key_), (ptr)->key))

/* As PRT_EQUAL only its result is returned in RES.  REBUILT_P is set
   up to TRUE if the table is rebuilt during the comparison.  */
#define DO_PTR_EQUAL_CHECK(tab, ptr, hash_val, key, res, rebuilt_p) \
    do {							    \
	unsigned int _old_version = (tab)->version;       \
	res = PTR_EQUAL(tab, ptr, hash_val, key);		    \
	rebuilt_p = _old_version != (tab)->version;	    \
    } while (FALSE)

/* Features of a table.  */
struct st_features {
    /* Power of 2 used for number of allocated entries.  */
    unsigned char entry_power;
    /* Power of 2 used for number of allocated bins.  Depending on the
       table size, the number of bins is 2-4 times more than the
       number of entries.  */
    unsigned char bin_power;
    /* Enumeration of sizes of bins (8-bit, 16-bit etc).  */
    unsigned char size_ind;
    /* Bins are packed in words of type st_index_t.  The following is
       a size of bins counted by words.  */
    st_index_t bins_words;
};

/* Features of all possible size tables.  */
#if SIZEOF_ST_INDEX_T == 8
#define MAX_POWER2 62
static const struct st_features features[] = {
    {0, 1, 0, 0x0},
    {1, 2, 0, 0x1},
    {2, 3, 0, 0x1},
    {3, 4, 0, 0x2},
    {4, 5, 0, 0x4},
    {5, 6, 0, 0x8},
    {6, 7, 0, 0x10},
    {7, 8, 0, 0x20},
    {8, 9, 1, 0x80},
    {9, 10, 1, 0x100},
    {10, 11, 1, 0x200},
    {11, 12, 1, 0x400},
    {12, 13, 1, 0x800},
    {13, 14, 1, 0x1000},
    {14, 15, 1, 0x2000},
    {15, 16, 1, 0x4000},
    {16, 17, 2, 0x10000},
    {17, 18, 2, 0x20000},
    {18, 19, 2, 0x40000},
    {19, 20, 2, 0x80000},
    {20, 21, 2, 0x100000},
    {21, 22, 2, 0x200000},
    {22, 23, 2, 0x400000},
    {23, 24, 2, 0x800000},
    {24, 25, 2, 0x1000000},
    {25, 26, 2, 0x2000000},
    {26, 27, 2, 0x4000000},
    {27, 28, 2, 0x8000000},
    {28, 29, 2, 0x10000000},
    {29, 30, 2, 0x20000000},
    {30, 31, 2, 0x40000000},
    {31, 32, 2, 0x80000000},
    {32, 33, 3, 0x200000000},
    {33, 34, 3, 0x400000000},
    {34, 35, 3, 0x800000000},
    {35, 36, 3, 0x1000000000},
    {36, 37, 3, 0x2000000000},
    {37, 38, 3, 0x4000000000},
    {38, 39, 3, 0x8000000000},
    {39, 40, 3, 0x10000000000},
    {40, 41, 3, 0x20000000000},
    {41, 42, 3, 0x40000000000},
    {42, 43, 3, 0x80000000000},
    {43, 44, 3, 0x100000000000},
    {44, 45, 3, 0x200000000000},
    {45, 46, 3, 0x400000000000},
    {46, 47, 3, 0x800000000000},
    {47, 48, 3, 0x1000000000000},
    {48, 49, 3, 0x2000000000000},
    {49, 50, 3, 0x4000000000000},
    {50, 51, 3, 0x8000000000000},
    {51, 52, 3, 0x10000000000000},
    {52, 53, 3, 0x20000000000000},
    {53, 54, 3, 0x40000000000000},
    {54, 55, 3, 0x80000000000000},
    {55, 56, 3, 0x100000000000000},
    {56, 57, 3, 0x200000000000000},
    {57, 58, 3, 0x400000000000000},
    {58, 59, 3, 0x800000000000000},
    {59, 60, 3, 0x1000000000000000},
    {60, 61, 3, 0x2000000000000000},
    {61, 62, 3, 0x4000000000000000},
    {62, 63, 3, 0x8000000000000000},
};

#else
#define MAX_POWER2 30

static const struct st_features features[] = {
    {0, 1, 0, 0x1},
    {1, 2, 0, 0x1},
    {2, 3, 0, 0x2},
    {3, 4, 0, 0x4},
    {4, 5, 0, 0x8},
    {5, 6, 0, 0x10},
    {6, 7, 0, 0x20},
    {7, 8, 0, 0x40},
    {8, 9, 1, 0x100},
    {9, 10, 1, 0x200},
    {10, 11, 1, 0x400},
    {11, 12, 1, 0x800},
    {12, 13, 1, 0x1000},
    {13, 14, 1, 0x2000},
    {14, 15, 1, 0x4000},
    {15, 16, 1, 0x8000},
    {16, 17, 2, 0x20000},
    {17, 18, 2, 0x40000},
    {18, 19, 2, 0x80000},
    {19, 20, 2, 0x100000},
    {20, 21, 2, 0x200000},
    {21, 22, 2, 0x400000},
    {22, 23, 2, 0x800000},
    {23, 24, 2, 0x1000000},
    {24, 25, 2, 0x2000000},
    {25, 26, 2, 0x4000000},
    {26, 27, 2, 0x8000000},
    {27, 28, 2, 0x10000000},
    {28, 29, 2, 0x20000000},
    {29, 30, 2, 0x40000000},
    {30, 31, 2, 0x80000000},
};

#endif

/* The reserved hash value and its substitution.  */
#define RESERVED_HASH_VAL (~(st_hash_t) 0)
#define RESERVED_HASH_SUBSTITUTION_VAL ((st_hash_t) 0)

/* Return hash value of KEY for table TAB.  */
static inline st_hash_t
do_hash(st_data_t key, st_table *tab)
{
    st_hash_t hash = (st_hash_t)(tab->type->hash)(key);

    /* RESERVED_HASH_VAL is used for a deleted entry.  Map it into
       another value.  Such mapping should be extremely rare.  */
    return hash == RESERVED_HASH_VAL ? RESERVED_HASH_SUBSTITUTION_VAL : hash;
}

/* Power of 2 defining the minimal number of allocated entries.  */
#define MINIMAL_POWER2 2

#if MINIMAL_POWER2 < 2
#error "MINIMAL_POWER2 should be >= 2"
#endif

/* If the power2 of the allocated `entries` is less than the following
   value, don't allocate bins and use a linear search.  */
#define MAX_POWER2_FOR_TABLES_WITHOUT_BINS 4

/* Return smallest n >= MINIMAL_POWER2 such 2^n > SIZE.  */
static int
get_power2(st_index_t size)
{
    unsigned int n;

    for (n = 0; size != 0; n++)
        size >>= 1;
    if (n <= MAX_POWER2)
        return n < MINIMAL_POWER2 ? MINIMAL_POWER2 : n;
#ifndef NOT_RUBY
    /* Ran out of the table entries */
    rb_raise(rb_eRuntimeError, "st_table too big");
#endif
    /* should raise exception */
    return -1;
}

/* Return value of N-th bin in array BINS of table with bins size
   index S.  */
static inline st_index_t
get_bin(st_index_t *bins, int s, st_index_t n)
{
    return (s == 0 ? ((unsigned char *) bins)[n]
	    : s == 1 ? ((unsigned short *) bins)[n]
	    : s == 2 ? ((unsigned int *) bins)[n]
	    : ((st_index_t *) bins)[n]);
}

/* Set up N-th bin in array BINS of table with bins size index S to
   value V.  */
static inline void
set_bin(st_index_t *bins, int s, st_index_t n, st_index_t v)
{
    if (s == 0) ((unsigned char *) bins)[n] = (unsigned char) v;
    else if (s == 1) ((unsigned short *) bins)[n] = (unsigned short) v;
    else if (s == 2) ((unsigned int *) bins)[n] = (unsigned int) v;
    else ((st_index_t *) bins)[n] = v;
}

/* These macros define reserved values for empty table bin and table
   bin which contains a deleted entry.  We will never use such values
   for an entry index in bins.  */
#define EMPTY_BIN    0
#define DELETED_BIN  1
/* Base of a real entry index in the bins.  */
#define ENTRY_BASE 2


/* Values used for not found entry and bin with given
   characteristics.  */
#define UNDEFINED_ENTRY_IND (~(st_index_t) 0)
#define UNDEFINED_BIN_IND (~(st_index_t) 0)

/* Entry and bin values returned when we found a table rebuild during
   the search.  */
#define REBUILT_TABLE_ENTRY_IND (~(st_index_t) 1)
#define REBUILT_TABLE_BIN_IND (~(st_index_t) 1)

/* Macros for marking and checking deleted entries given by their
   pointer E_PTR.  */
#define MARK_ENTRY_DELETED(e_ptr) ((e_ptr)->hash = RESERVED_HASH_VAL)
#define DELETED_ENTRY_P(e_ptr) ((e_ptr)->hash == RESERVED_HASH_VAL)

/* Return the number of allocated bins of table TAB.  */
static inline st_index_t
get_bins_num(const st_table *tab)
{
    return ((st_index_t) 1)<<(tab->bin_power - 1);
}

/* Return mask for a bin index in table TAB.  */
static inline st_index_t
bins_mask(const st_table *tab)
{
    return get_bins_num(tab) - 1;
}

/* Return the index of table TAB bin corresponding to
   HASH_VALUE.  */
static inline st_index_t
hash_bin(st_table *tab, st_data_t key)
{
    return /*do_hash(key, tab)*/ key & bins_mask(tab);
}

/* Return size of the allocated bins of table TAB.  */
static inline st_index_t
bins_size(const st_table *tab)
{
    return features[tab->bin_power].bins_words * sizeof (st_index_t);
}

#ifdef ST_DEBUG
#define st_assert_notinitial(ent) \
    do { \
	st_assert(ent.hash != (st_hash_t) ST_INIT_VAL);  \
	st_assert(ent.key != ST_INIT_VAL); \
	st_assert(ent.record != ST_INIT_VAL); \
    } while (0)
/* Check the table T consistency.  It can be extremely slow.  So use
   it only for debugging.  */
static void
st_check(st_table *tab)
{
    st_index_t d, e, i, n, p;

    for (p = get_bins_num(tab), i = 0; p > 1; i++, p>>=1)
        ;
    p = i;
    st_assert(p >= MINIMAL_POWER2);
    st_assert(tab->entries_bound <= get_bins_num(tab));
    st_assert(tab->entries_start <= tab->entries_bound);
    n = 0;
    return;
    if (tab->entries_bound != 0)
        for (i = tab->entries_start; i < tab->entries_bound; i++) {
	    st_assert_notinitial(tab->entries[i]);
	    if (! DELETED_ENTRY_P(&tab->entries[i]))
	        n++;
	}
    st_assert(n == tab->num_entries);
    if (tab->bins == NULL)
        st_assert(p <= MAX_POWER2_FOR_TABLES_WITHOUT_BINS);
    else {
        st_assert(p > MAX_POWER2_FOR_TABLES_WITHOUT_BINS);
	for (n = d = i = 0; i < get_bins_num(tab); i++) {
	    st_assert(get_bin(tab->bins, tab->size_ind, i) != ST_INIT_VAL);
	    if (IND_DELETED_BIN_P(tab, i)) {
	        d++;
		continue;
	    }
	    else if (IND_EMPTY_BIN_P(tab, i))
	        continue;
	    n++;
	    e = get_bin(tab->bins, tab->size_ind, i) - ENTRY_BASE;
	    st_assert(tab->entries_start <= e && e < tab->entries_bound);
	    st_assert(! DELETED_ENTRY_P(&tab->entries[e]));
	    st_assert_notinitial(tab->entries[e]);
	}
	st_assert(n == tab->num_entries);
	st_assert(n + d < get_bins_num(tab));
    }
}
#endif

#ifdef HASH_LOG
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
static struct {
    int all, total, num, str, strcase;
}  collision;

/* Flag switching off output of package statistics at the end of
   program.  */
static int init_st = 0;

/* Output overall number of table searches and collisions into a
   temporary file.  */
static void
stat_col(void)
{
    char fname[10+sizeof(long)*3];
    FILE *f;
    if (!collision.total) return;
    f = fopen((snprintf(fname, sizeof(fname), "/tmp/col%ld", (long)getpid()), fname), "w");
    fprintf(f, "collision: %d / %d (%6.2f)\n", collision.all, collision.total,
            ((double)collision.all / (collision.total)) * 100);
    fprintf(f, "num: %d, str: %d, strcase: %d\n", collision.num, collision.str, collision.strcase);
    fclose(f);
}
#endif

st_bucket *
bucket_create()
{
    st_bucket *bucket = NULL;
    size_t j;

    bucket = (st_bucket*) memalign(CACHE_LINE_SIZE, sizeof(st_bucket));
    if (bucket == NULL) 
	return NULL;

    bucket->lock = 0;
    
    for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
	bucket->key[j] = 0;
    }
    bucket->next = NULL;
    return bucket;
}

st_bucket *
bucket_create_stats(st_table *tab, int *resize)
{
    st_bucket *bucket = bucket_create();
    if (IAF_U32(&tab->num_expands) == tab->num_expands_threshold) {
	/* printf("      -- hit threshold (%u ~ %u)\n", tab->num_expands, tab->num_expands_threshold); */
	*resize = 1;
    }
    return bucket;
}


/* Create and return table with TYPE which can hold at least SIZE
   entries.  The real number of entries which the table can hold is
   the nearest power of two for SIZE.  */
st_table *
st_init_table_with_size(const struct st_hash_type *type, st_index_t size)
{
    st_table *tab;
    int n;
    st_bucket *bucket;
#ifdef HASH_LOG
#if HASH_LOG+0 < 0
    {
        const char *e = getenv("ST_HASH_LOG");
        if (!e || !*e) init_st = 1;
    }
#endif
    if (init_st == 0) {
        init_st = 1;
        atexit(stat_col);
    }
#endif

    n = get_power2(size);
    tab = (st_table *) memalign(CACHE_LINE_SIZE, sizeof(st_table));
    tab->type = type;
//     tab->entry_power = n;
    tab->bin_power = features[n].bin_power;
    tab->table_new = NULL;
    tab->resize_lock = LOCK_FREE;
    // tab->size_ind = features[n].size_ind;
    tab->num_expands = 0;
    tab->num_buckets = n < SMALL_TABLE_THRESHOLD ? size + 1 : get_bins_num(tab);
    tab->bucket = (st_bucket *) memalign(CACHE_LINE_SIZE, tab->num_buckets * sizeof(st_bucket));
    tab->num_expands_threshold = 2 * tab->num_buckets;

    memset(tab->bucket, 0, tab->num_buckets * (sizeof(st_bucket)));

#ifdef ST_DEBUG
    st_check(tab);
#endif
    return tab;
}

/* Create and return table with TYPE which can hold a minimal number
   of entries (see comments for get_power2).  */
st_table *
st_init_table(const struct st_hash_type *type)
{
    return st_init_table_with_size(type, 0);
}

/* Create and return table which can hold a minimal number of
   numbers.  */
st_table *
st_init_numtable(void)
{
    return st_init_table(&type_numhash);
}

/* Create and return table which can hold SIZE numbers.  */
st_table *
st_init_numtable_with_size(st_index_t size)
{
    return st_init_table_with_size(&type_numhash, size);
}

/* Create and return table which can hold a minimal number of
   strings.  */
st_table *
st_init_strtable(void)
{
    return st_init_table(&type_strhash);
}

/* Create and return table which can hold SIZE strings.  */
st_table *
st_init_strtable_with_size(st_index_t size)
{
    return st_init_table_with_size(&type_strhash, size);
}

/* Create and return table which can hold a minimal number of strings
   whose character case is ignored.  */
st_table *
st_init_strcasetable(void)
{
    return st_init_table(&type_strcasehash);
}

/* Create and return table which can hold SIZE strings whose character
   case is ignored.  */
st_table *
st_init_strcasetable_with_size(st_index_t size)
{
    return st_init_table_with_size(&type_strcasehash, size);
}

/* Make table TAB empty.  */
void
st_clear(st_table *tab)
{
//     make_tab_empty(tab);
//     tab->rebuilds_num++;
#ifdef ST_DEBUG
    st_check(tab);
#endif
}

/* Free table TAB space.  */
void
st_free_table(st_table *tab)
{
    // if (tab->bins != NULL)
    //     free(tab->bins);
    free(tab->bucket);
    free(tab);
}

size_t
bucket_size(const st_table *tab)
{
    st_data_t num_buckets = tab->num_buckets;
    volatile st_bucket *bucket = NULL;
    size_t size = 0;

    st_data_t bin;
    for (bin = 0; bin < num_buckets; bin++) {
	bucket = tab->bucket + bin;

	while (bucket->next != NULL) {
	    bucket = bucket->next;
	    size++;
	}
    }
    return size + num_buckets;
}

/* Return byte size of memory allocted for table TAB.  */
size_t
st_memsize(const st_table *tab)
{
    return (sizeof(st_table) + sizeof(st_bucket) * bucket_size(tab));
}

#ifdef HASH_LOG
static void
count_collision(const struct st_hash_type *type)
{
    collision.all++;
    if (type == &type_numhash) {
        collision.num++;
    }
    else if (type == &type_strhash) {
        collision.strcase++;
    }
    else if (type == &type_strcasehash) {
        collision.str++;
    }
}

#define COLLISION (collision_check ? count_collision(tab->type) : (void)0)
#define FOUND_BIN (collision_check ? collision.total++ : (void)0)
#define collision_check 0
#else
#define COLLISION
#define FOUND_BIN
#endif

/* If the number of entries in the table is at least REBUILD_THRESHOLD
   times less than the entry array length, decrease the table
   size.  */
#define REBUILD_THRESHOLD 4

#if REBUILD_THRESHOLD < 2
#error "REBUILD_THRESHOLD should be >= 2"
#endif

static int
st_put_seq(st_table *tab, st_data_t key, st_data_t val, st_data_t bin)
{
    volatile st_bucket *bucket = tab->bucket + bin;
    size_t j;

    do {
	for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
	    if (bucket->key[j] == 0) {
		bucket->val[j] = val;
		bucket->key[j] = key;
		tab->num_entries++;
		return 1;
	    }
	}

	if (bucket->next == NULL) {
	    int null;
	    bucket->next = bucket_create_stats(tab, &null);
	    bucket->next->val[0] = val;
	    bucket->next->key[0] = key;
	    tab->num_entries++;
	    return 1;
	}
	
	bucket = bucket->next;
    } while (1);
}

static int 
bucket_copy(volatile st_bucket *bucket, st_table *tab)
{
    size_t j;

    if (!LOCK_ACQ_RES(&bucket->lock))
    {
	    return 0;
    }

    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            st_data_t key = bucket->key[j];
            if (key != 0) {
                st_index_t bin = hash_bin(tab, key);
                st_put_seq(tab, key, bucket->val[j], bin);
            }
        }
	    bucket = bucket->next;
    } while (bucket != NULL);

    return 1;
}

/* Rebuild table TAB.  Rebuilding removes all deleted bins and entries
   and can change size of the table entries and bins arrays.
   Rebuilding is implemented by creation of a new table or by
   compaction of the existing one.  */
int
rebuild_table(st_table *tab, int is_increase, int by)
{
    st_index_t num_buckets_new;
    st_table *new_tab;
    size_t b;

    if (TRYLOCK_ACQ(&tab->resize_lock))
	return 0;
    
    if (is_increase) 
	num_buckets_new = by * tab->num_buckets;
    else
	num_buckets_new = tab->num_buckets / HALVE_RATIO;

    new_tab = st_init_table_with_size(tab->type, num_buckets_new);
    new_tab->version = tab->version + 1;

    for (b = 0; b < tab->num_buckets; b++) {
	st_bucket * bu_cur = tab->bucket + b;
	bucket_copy(bu_cur, new_tab);
    }

//     new_tab->table_prev = tab;
    SWAP_U64(&tab, new_tab);
    tab->table_new = tab;
//     free(new_tab);
    TRYLOCK_RLS(tab->resize_lock);

    //gc_relase(tab);
    return 1;
}


static inline int
is_small_table(st_table *tab)
{
    return tab->num_buckets < SMALL_TABLE_THRESHOLD;
}

/* Find an entry with KEY in table TAB.  Return non-zero if we found
   it.  Set up *RECORD to the found entry record.  */
int
st_lookup(st_table *tab, st_data_t key, st_data_t *value)
{
    st_index_t bin = is_small_table(tab) ? 0 : hash_bin(tab, key);
    volatile st_bucket *bucket = tab->bucket + bin;

    size_t j;
    do {
	for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
	    st_data_t val = bucket->val[j];
	    if (bucket->key[j] == key) {
		if (LIKELY(bucket->val[j] == val)) {
		    *value = val;
		    return 1;
		}
		else {
		    return 0;
		}
	    }
	}
	bucket = bucket->next;
    } while (UNLIKELY(bucket != NULL));

    return 0;
}

/* Find an entry with KEY in table TAB.  Return non-zero if we found
   it.  Set up *RESULT to the found table entry key.  */
int
st_get_key(st_table *tab, st_data_t key, st_data_t *result)
{
    st_index_t bin = is_small_table(tab) ? 0 : hash_bin(tab, key);
    volatile st_bucket *bucket = tab->bucket + bin;

    size_t j;
    do {
	for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
	    st_data_t val = bucket->val[j];
	    if (bucket->key[j] == key) {
		if (LIKELY(bucket->val[j] == val)) {
		    *result = bucket->key[j];
		    return 1;
		}
		else {
		    return 0;
		}
	    }
	}
	bucket = bucket->next;
    } while (UNLIKELY(bucket != NULL));

    return 0;
}

/* Insert (KEY, VALUE) into table TAB and return zero.  If there is
   already entry with KEY in the table, return nonzero and and update
   the value of the found entry.  */
int
st_insert(st_table *tab, st_data_t key, st_data_t value)
{
    st_index_t bin = is_small_table(tab) ? 0 : hash_bin(tab, key);
    volatile st_bucket *bucket = tab->bucket + bin;
    st_lock_t *lock = &bucket->lock;
    st_data_t *empty = NULL;
    st_data_t *empty_v = NULL;
    size_t j, j1; 
    int resize = 0;
    while (!LOCK_ACQ(lock, tab)) {
        bin = is_small_table(tab) ? 0 : hash_bin(tab, key);
        bucket = tab->bucket + bin;
        lock = &bucket->lock;
    }

    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            st_data_t val = bucket->val[j];
            if (bucket->key[j] == key) {
                if (LIKELY(bucket->val[j] == val)) {
                    bucket->val[j] = value;
                    LOCK_RLS(lock);
                    return 1;
                }
            }
            else if (empty == NULL && bucket->key[j] == 0) {
                empty = &bucket->key[j];
                empty_v = &bucket->val[j];
            }
        }
             
        if (LIKELY(bucket->next == NULL)) {
            if (UNLIKELY(empty == NULL)) {
                st_bucket *b = bucket_create_stats(tab, &resize);
                b->val[0] = value;
                b->key[0] = key;
                bucket->next = b;
            }
            else {
                *empty_v = value;
                *empty = key;             
            }
            LOCK_RLS(lock);

            if (UNLIKELY(resize)) {
                printf("here2\n");
                rebuild_table(tab, 1, 2);
            }
            return 0;
        }
        
        bucket = bucket->next;
    } while(1);
}

/* Insert (KEY, VALUE) into table TAB.  The table should not have
   entry with KEY before the insertion.  */
void
st_add_direct(st_table *tab, st_data_t key, st_data_t value)
{
    st_data_t bin = is_small_table(tab) ? 0 : hash_bin(tab, key);
    st_put_seq(tab, key, value, bin);
}

/* Insert (FUNC(KEY), VALUE) into table TAB and return zero.  If
   there is already entry with KEY in the table, return nonzero and
   and update the value of the found entry.  */
int
st_insert2(st_table *tab, st_data_t key, st_data_t value,
           st_data_t (*func)(st_data_t))
{
    st_index_t bin = is_small_table(tab) ? 0 : hash_bin(tab, key);
    volatile st_bucket *bucket = tab->bucket + bin;
    st_lock_t *lock = &bucket->lock;
    st_data_t* empty = NULL;
    st_data_t* empty_v = NULL;
    size_t j; 
    int resize = 0;
    

    while (!LOCK_ACQ(lock, tab)) {
	bin = is_small_table(tab) ? 0 : hash_bin(tab, key);
	bucket = tab->bucket + bin;
	lock = &bucket->lock;
    }

    do {
	for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
	    st_data_t val = bucket->val[j];
	    if (bucket->key[j] == key) {
		if (bucket->val[j] == val) {
		    bucket->val[j] = value;
		    LOCK_RLS(lock);
		    return 1;
		}
	    }
	    else if (empty == NULL && bucket->key[j] == 0) {
		empty = &bucket->key[j];
		empty_v = &bucket->val[j];
	    }
	}

	if (LIKELY(bucket->next == NULL)) {
	    if (UNLIKELY(empty == NULL)) {
		st_bucket *b = bucket_create_stats(tab, &resize);
		b->val[0] = value;
		b->key[0] = (*func)(key);
		bucket->next = b;
	    }
	    else {
		*empty_v = value;
		*empty = (*func)(key);
	    }

	    LOCK_RLS(lock);
	    if (UNLIKELY(resize)) {
		rebuild_table(tab, 1, 2);
	    }
	    return 0;
	}
	bucket = bucket->next;
    } while (1);
}

/* Create and return a copy of table OLD_TAB.  */
st_table *
st_copy(st_table *old_tab)
{
    st_table *new_tab;
    size_t b;

    new_tab = (st_table *) memalign(CACHE_LINE_SIZE, sizeof(st_table));
    new_tab->type = old_tab->type;
    new_tab->bin_power = old_tab->bin_power;
    new_tab->table_new = NULL;
    new_tab->resize_lock = LOCK_FREE;
    new_tab->num_expands = 0;
    new_tab->num_buckets = old_tab->num_buckets;
    new_tab->bucket = (st_bucket*) memalign(CACHE_LINE_SIZE, old_tab->num_buckets * sizeof(st_bucket));
    new_tab->num_expands_threshold = old_tab->num_expands_threshold;
    
    for (b = 0; b < old_tab->num_buckets; b++) {
        st_bucket *bu_cur = old_tab->bucket + b;
        bucket_copy(bu_cur, new_tab);
    }

    return new_tab;
}


/* Delete entry with KEY from table TAB, set up *VALUE (unless
   VALUE is zero) from deleted table entry, and return non-zero.  If
   there is no entry with KEY in the table, clear *VALUE (unless VALUE
   is zero), and return zero.  */
static int
st_general_delete(st_table *tab, st_data_t *key, st_data_t *value)
{
    st_index_t bin = is_small_table(tab) ? 0 : hash_bin(tab, *key);
    size_t j;
    volatile st_bucket *bucket = tab->bucket + bin;
    st_lock_t *lock = &bucket->lock;

    while (!LOCK_ACQ(lock, tab)) {
        bin = is_small_table(tab) ? 0 : hash_bin(tab, key);
        bucket = tab->bucket + bin;
        lock = &bucket->lock;
    }

    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            if (bucket->key[j] == *key) {
                bucket->key[j] = 0;
                if (value != 0) 
                    *value = bucket->val[j];
                LOCK_RLS(lock);
                return 1;
            }
        }
	    bucket = bucket->next;
    } while (UNLIKELY(bucket != NULL));

    if (value != 0) *value = 0;
    LOCK_RLS(lock);
    return 0;
}

int
st_delete(st_table *tab, st_data_t *key, st_data_t *value)
{
    return st_general_delete(tab, key, value);
}

/* The function and other functions with suffix '_safe' or '_check'
   are originated from the previous implementation of the hash tables.
   It was necessary for correct deleting entries during traversing
   tables.  The current implementation permits deletion during
   traversing without a specific way to do this.  */
int
st_delete_safe(st_table *tab, st_data_t *key, st_data_t *value,
               st_data_t never ATTRIBUTE_UNUSED)
{
    return st_general_delete(tab, key, value);
}

/* If table TAB is empty, clear *VALUE (unless VALUE is zero), and
   return zero.  Otherwise, remove the first entry in the table.
   Return its key through KEY and its record through VALUE (unless
   VALUE is zero).  */
int
st_shift(st_table *tab, st_data_t *key, st_data_t *value)
{
    st_index_t bin = is_small_table(tab) ? 0 : hash_bin(tab, *key);
    size_t j;
    volatile st_bucket *bucket = tab->bucket + bin;
    st_lock_t *lock = &bucket->lock;

    while (!LOCK_ACQ(lock, tab)) {
	bin = is_small_table(tab) ? 0 : hash_bin(tab, key);
	bucket = tab->bucket + bin;
	lock = &bucket->lock;
    }


    do {
	for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
	    if (bucket->key[j] != 0) {
		if (value != 0) 
		    *value = bucket->val[j];
		*key = bucket->key[j];
		bucket->key[j] = 0;
		LOCK_RLS(lock);
		return 1;
	    }
	}
	bucket = bucket->next;
    } while (UNLIKELY(bucket != NULL));
    if (value != 0) *value = 0;
    LOCK_RLS(lock);
    return 0;
}

/* See comments for function st_delete_safe.  */
void
st_cleanup_safe(st_table *tab ATTRIBUTE_UNUSED,
                st_data_t never ATTRIBUTE_UNUSED)
{
}

/* Find entry with KEY in table TAB, call FUNC with the key and the
   value of the found entry, and non-zero as the 3rd argument.  If the
   entry is not found, call FUNC with KEY, and 2 zero arguments.  If
   the call returns ST_CONTINUE, the table will have an entry with key
   and value returned by FUNC through the 1st and 2nd parameters.  If
   the call of FUNC returns ST_DELETE, the table will not have entry
   with KEY.  The function returns flag of that the entry with KEY was
   in the table before the call.  */
int
st_update(st_table *tab, st_data_t key,
	  st_update_callback_func *func, st_data_t arg)
{
    st_index_t bin = is_small_table(tab) ? 0 : hash_bin(tab, key);
    st_data_t *cur_entry = NULL;
    st_data_t *cur_entry_v = NULL;
    st_data_t value = 0, old_key;
    int retval, existing = 0;
    size_t j;
    volatile st_bucket *bucket = tab->bucket + bin;
    st_lock_t *lock = &bucket->lock;

    do {
	for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
	    st_data_t val = bucket->val[j];
	    if (bucket->key[j] == key) {
		if (bucket->val[j] == val) {
		    key = bucket->key[j];
		    value = bucket->val[j];
		    cur_entry = &bucket->key[j];
		    cur_entry_v = &bucket->val[j];
		    existing = 1;
		    break;
		}
	    }
	}
	bucket = bucket->next;
    } while (bucket != NULL);

    old_key = key;
    retval = (*func)(&key, &value, arg, existing);

    switch (retval) {
	case ST_CONTINUE:
	    if (!existing) {
		st_put_seq(tab, key, value, bin);
		break;
	    }
	    if (old_key != key) {
		*cur_entry = key;
	    }
	    *cur_entry_v = value;
	    break;
	case ST_DELETE: 
	    if (existing) {
		*cur_entry = 0;
		*cur_entry_v = 0;
		tab->num_entries--;
	    }
	    break;

    }
    return existing;
}

/* Traverse all entries in table TAB calling FUNC with current entry
   key and value and zero.  If the call returns ST_STOP, stop
   traversing.  If the call returns ST_DELETE, delete the current
   entry from the table.  In case of ST_CHECK or ST_CONTINUE, continue
   traversing.  The function returns zero unless an error is found.
   CHECK_P is flag of st_foreach_check call.  The behavior is a bit
   different for ST_CHECK and when the current element is removed
   during traversing.  */
static inline int
st_general_foreach(st_table *tab, int (*func)(ANYARGS), st_data_t arg,
		   int check_p)
{
    size_t j, j1, version = tab->version;
    st_data_t key, bin;
    int retval;
    st_bucket *bucket;
    st_lock_t *lock = &bucket->lock;

    while (!LOCK_ACQ(lock, tab)) {
    	bucket = tab->bucket;
	    lock = &bucket->lock;
    }

    for (bin = 0; bin < tab->num_buckets; bin++) {
	    bucket = tab->bucket + bin;

	do {
	    for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
		if (bucket->key[j] != 0) {
		    retval = (*func)(bucket->key[j], bucket->val[j], arg, 0);
		}

		if (version != tab->version) {
		    if (check_p) {
			retval = (*func)(0, 0, arg, 1);
			return 1;
		    }
		}

		switch (retval) {
		    case ST_CONTINUE:
			break;
		    case ST_CHECK:
			if (check_p) break;
		    case ST_STOP:
			return 0;
		    case ST_DELETE: {
			key = bucket->key[j];

			for (j1 = 0; j1 < ENTRIES_PER_BUCKET; j1++) {
			    if (bucket->key[j1] == key) {
				bucket->key[j1] = 0;
				break;
			    }
			}
		    }
		}
	    }
	    bucket = bucket->next;
	} while (bucket->next != NULL);
    }
    return 1;
}

int
st_foreach(st_table *tab, int (*func)(ANYARGS), st_data_t arg)
{
    return st_general_foreach(tab, func, arg, FALSE);
}

/* See comments for function st_delete_safe.  */
int
st_foreach_check(st_table *tab, int (*func)(ANYARGS), st_data_t arg,
                 st_data_t never ATTRIBUTE_UNUSED)
{
    return st_general_foreach(tab, func, arg, TRUE);
}

/* Set up array KEYS by at most SIZE keys of head table TAB entries.
   Return the number of keys set up in array KEYS.  */
static inline st_index_t
st_general_keys(st_table *tab, st_data_t *keys, st_index_t size)
{
    st_index_t i, bin;
    st_data_t *keys_start, *keys_end;
    st_bucket *bucket = NULL;

    keys_start = keys;
    keys_end = keys + size;
    for (bin = 0; bin < tab->num_buckets; bin++) {
	if (keys == keys_end)
	    break;
	bucket = tab->bucket + bin;

	do {
	for (i = 0; i < ENTRIES_PER_BUCKET; i++) {
	    if (bucket->key[i] != 0) {
	    	*keys++ = bucket->key[i];
	    }
	}
	    bucket = bucket->next;
	} while (bucket != NULL);
    }

    return keys - keys_start;
}

st_index_t
st_keys(st_table *tab, st_data_t *keys, st_index_t size)
{
    return st_general_keys(tab, keys, size);
}

/* See comments for function st_delete_safe.  */
st_index_t
st_keys_check(st_table *tab, st_data_t *keys, st_index_t size,
              st_data_t never ATTRIBUTE_UNUSED)
{
    return st_general_keys(tab, keys, size);
}

/* Set up array VALUES by at most SIZE values of head table TAB
   entries.  Return the number of values set up in array VALUES.  */
static inline st_index_t
st_general_values(st_table *tab, st_data_t *values, st_index_t size)
{
    st_index_t i, bin;
    st_data_t *value_start, *value_end;
    st_bucket *bucket = NULL;

    value_start = values;
    value_end = values + size;
    for (bin = 0; bin < tab->num_buckets; bin++) {
	if (values == value_end)
	    break;
	bucket = tab->bucket + bin;

	do {
	for (i = 0; i < ENTRIES_PER_BUCKET; i++) {
	    if (bucket->key[i] != 0) {
	    	*values++ = bucket->val[i];
	    }
	}
	    bucket = bucket->next;
	} while (bucket != NULL);
    }

    return values - value_start;
}

st_index_t
st_values(st_table *tab, st_data_t *values, st_index_t size)
{
    return st_general_values(tab, values, size);
}

/* See comments for function st_delete_safe.  */
st_index_t
st_values_check(st_table *tab, st_data_t *values, st_index_t size,
		st_data_t never ATTRIBUTE_UNUSED)
{
    return st_general_values(tab, values, size);
}

#define FNV1_32A_INIT 0x811c9dc5

/*
 * 32 bit magic FNV-1a prime
 */
#define FNV_32_PRIME 0x01000193

#ifndef UNALIGNED_WORD_ACCESS
# if defined(__i386) || defined(__i386__) || defined(_M_IX86) || \
     defined(__x86_64) || defined(__x86_64__) || defined(_M_AMD64) || \
     defined(__powerpc64__) || \
     defined(__mc68020__)
#   define UNALIGNED_WORD_ACCESS 1
# endif
#endif
#ifndef UNALIGNED_WORD_ACCESS
# define UNALIGNED_WORD_ACCESS 0
#endif

/* This hash function is quite simplified MurmurHash3
 * Simplification is legal, cause most of magic still happens in finalizator.
 * And finalizator is almost the same as in MurmurHash3 */
#define BIG_CONSTANT(x,y) ((st_index_t)(x)<<32|(st_index_t)(y))
#define ROTL(x,n) ((x)<<(n)|(x)>>(SIZEOF_ST_INDEX_T*CHAR_BIT-(n)))

#if ST_INDEX_BITS <= 32
#define C1 (st_index_t)0xcc9e2d51
#define C2 (st_index_t)0x1b873593
#else
#define C1 BIG_CONSTANT(0x87c37b91,0x114253d5);
#define C2 BIG_CONSTANT(0x4cf5ad43,0x2745937f);
#endif
static inline st_index_t
murmur_step(st_index_t h, st_index_t k)
{
#if ST_INDEX_BITS <= 32
#define r1 (17)
#define r2 (11)
#else
#define r1 (33)
#define r2 (24)
#endif
    k *= C1;
    h ^= ROTL(k, r1);
    h *= C2;
    h = ROTL(h, r2);
    return h;
}
#undef r1
#undef r2

static inline st_index_t
murmur_finish(st_index_t h)
{
#if ST_INDEX_BITS <= 32
#define r1 (16)
#define r2 (13)
#define r3 (16)
    const st_index_t c1 = 0x85ebca6b;
    const st_index_t c2 = 0xc2b2ae35;
#else
/* values are taken from Mix13 on http://zimbry.blogspot.ru/2011/09/better-bit-mixing-improving-on.html */
#define r1 (30)
#define r2 (27)
#define r3 (31)
    const st_index_t c1 = BIG_CONSTANT(0xbf58476d,0x1ce4e5b9);
    const st_index_t c2 = BIG_CONSTANT(0x94d049bb,0x133111eb);
#endif
#if ST_INDEX_BITS > 64
    h ^= h >> 64;
    h *= c2;
    h ^= h >> 65;
#endif
    h ^= h >> r1;
    h *= c1;
    h ^= h >> r2;
    h *= c2;
    h ^= h >> r3;
    return h;
}
#undef r1
#undef r2
#undef r3

st_index_t
st_hash(const void *ptr, size_t len, st_index_t h)
{
    const char *data = ptr;
    st_index_t t = 0;
    size_t l = len;

#define data_at(n) (st_index_t)((unsigned char)data[(n)])
#define UNALIGNED_ADD_4 UNALIGNED_ADD(2); UNALIGNED_ADD(1); UNALIGNED_ADD(0)
#if SIZEOF_ST_INDEX_T > 4
#define UNALIGNED_ADD_8 UNALIGNED_ADD(6); UNALIGNED_ADD(5); UNALIGNED_ADD(4); UNALIGNED_ADD(3); UNALIGNED_ADD_4
#if SIZEOF_ST_INDEX_T > 8
#define UNALIGNED_ADD_16 UNALIGNED_ADD(14); UNALIGNED_ADD(13); UNALIGNED_ADD(12); UNALIGNED_ADD(11); \
    UNALIGNED_ADD(10); UNALIGNED_ADD(9); UNALIGNED_ADD(8); UNALIGNED_ADD(7); UNALIGNED_ADD_8
#define UNALIGNED_ADD_ALL UNALIGNED_ADD_16
#endif
#define UNALIGNED_ADD_ALL UNALIGNED_ADD_8
#else
#define UNALIGNED_ADD_ALL UNALIGNED_ADD_4
#endif
#undef SKIP_TAIL
    if (len >= sizeof(st_index_t)) {
#if !UNALIGNED_WORD_ACCESS
	int align = (int)((st_data_t)data % sizeof(st_index_t));
	if (align) {
	    st_index_t d = 0;
	    int sl, sr, pack;

	    switch (align) {
#ifdef WORDS_BIGENDIAN
# define UNALIGNED_ADD(n) case SIZEOF_ST_INDEX_T - (n) - 1: \
		t |= data_at(n) << CHAR_BIT*(SIZEOF_ST_INDEX_T - (n) - 2)
#else
# define UNALIGNED_ADD(n) case SIZEOF_ST_INDEX_T - (n) - 1:	\
		t |= data_at(n) << CHAR_BIT*(n)
#endif
		UNALIGNED_ADD_ALL;
#undef UNALIGNED_ADD
	    }

#ifdef WORDS_BIGENDIAN
	    t >>= (CHAR_BIT * align) - CHAR_BIT;
#else
	    t <<= (CHAR_BIT * align);
#endif

	    data += sizeof(st_index_t)-align;
	    len -= sizeof(st_index_t)-align;

	    sl = CHAR_BIT * (SIZEOF_ST_INDEX_T-align);
	    sr = CHAR_BIT * align;

	    while (len >= sizeof(st_index_t)) {
		d = *(st_index_t *)data;
#ifdef WORDS_BIGENDIAN
		t = (t << sr) | (d >> sl);
#else
		t = (t >> sr) | (d << sl);
#endif
		h = murmur_step(h, t);
		t = d;
		data += sizeof(st_index_t);
		len -= sizeof(st_index_t);
	    }

	    pack = len < (size_t)align ? (int)len : align;
	    d = 0;
	    switch (pack) {
#ifdef WORDS_BIGENDIAN
# define UNALIGNED_ADD(n) case (n) + 1: \
		d |= data_at(n) << CHAR_BIT*(SIZEOF_ST_INDEX_T - (n) - 1)
#else
# define UNALIGNED_ADD(n) case (n) + 1: \
		d |= data_at(n) << CHAR_BIT*(n)
#endif
		UNALIGNED_ADD_ALL;
#undef UNALIGNED_ADD
	    }
#ifdef WORDS_BIGENDIAN
	    t = (t << sr) | (d >> sl);
#else
	    t = (t >> sr) | (d << sl);
#endif

	    if (len < (size_t)align) goto skip_tail;
# define SKIP_TAIL 1
	    h = murmur_step(h, t);
	    data += pack;
	    len -= pack;
	}
	else
#endif
#ifdef HAVE_BUILTIN___BUILTIN_ASSUME_ALIGNED
#define aligned_data __builtin_assume_aligned(data, sizeof(st_index_t))
#else
#define aligned_data data
#endif
	{
	    do {
		h = murmur_step(h, *(st_index_t *)aligned_data);
		data += sizeof(st_index_t);
		len -= sizeof(st_index_t);
	    } while (len >= sizeof(st_index_t));
	}
    }

    t = 0;
    switch (len) {
#if UNALIGNED_WORD_ACCESS && SIZEOF_ST_INDEX_T <= 8 && CHAR_BIT == 8
    /* in this case byteorder doesn't really matter */
#if SIZEOF_ST_INDEX_T > 4
      case 7: t |= data_at(6) << 48;
      case 6: t |= data_at(5) << 40;
      case 5: t |= data_at(4) << 32;
      case 4:
	t |= (st_index_t)*(uint32_t*)aligned_data;
	goto skip_tail;
# define SKIP_TAIL 1
#endif
      case 3: t |= data_at(2) << 16;
      case 2: t |= data_at(1) << 8;
      case 1: t |= data_at(0);
#else
#ifdef WORDS_BIGENDIAN
# define UNALIGNED_ADD(n) case (n) + 1: \
	t |= data_at(n) << CHAR_BIT*(SIZEOF_ST_INDEX_T - (n) - 1)
#else
# define UNALIGNED_ADD(n) case (n) + 1: \
	t |= data_at(n) << CHAR_BIT*(n)
#endif
	UNALIGNED_ADD_ALL;
#undef UNALIGNED_ADD
#endif
#ifdef SKIP_TAIL
      skip_tail:
#endif
	h ^= t; h -= ROTL(t, 7);
	h *= C2;
    }
    h ^= l;
#undef aligned_data

    return murmur_finish(h);
}

st_index_t
st_hash_uint32(st_index_t h, uint32_t i)
{
    return murmur_step(h, i);
}

st_index_t
st_hash_uint(st_index_t h, st_index_t i)
{
    i += h;
/* no matter if it is BigEndian or LittleEndian,
 * we hash just integers */
#if SIZEOF_ST_INDEX_T*CHAR_BIT > 8*8
    h = murmur_step(h, i >> 8*8);
#endif
    h = murmur_step(h, i);
    return h;
}

st_index_t
st_hash_end(st_index_t h)
{
    h = murmur_finish(h);
    return h;
}

#undef st_hash_start
st_index_t
st_hash_start(st_index_t h)
{
    return h;
}

static st_index_t
strhash(st_data_t arg)
{
    register const char *string = (const char *)arg;
    return st_hash(string, strlen(string), FNV1_32A_INIT);
}



int
st_locale_insensitive_strncasecmp(const char *s1, const char *s2, size_t n)
{
    unsigned int c1, c2;

    while (n--) {
        c1 = (unsigned char)*s1++;
        c2 = (unsigned char)*s2++;
        if (c1 == '\0' || c2 == '\0') {
            if (c1 != '\0') return 1;
            if (c2 != '\0') return -1;
            return 0;
        }
        if ((unsigned int)(c1 - 'A') <= ('Z' - 'A')) c1 += 'a' - 'A';
        if ((unsigned int)(c2 - 'A') <= ('Z' - 'A')) c2 += 'a' - 'A';
        if (c1 != c2) {
            if (c1 > c2)
                return 1;
            else
                return -1;
        }
    }
    return 0;
}

static st_index_t
strcasehash(st_data_t arg)
{
    register const char *string = (const char *)arg;
    register st_index_t hval = FNV1_32A_INIT;

    /*
     * FNV-1a hash each octet in the buffer
     */
    while (*string) {
	unsigned int c = (unsigned char)*string++;
	if ((unsigned int)(c - 'A') <= ('Z' - 'A')) c += 'a' - 'A';
	hval ^= c;

	/* multiply by the 32 bit FNV magic prime mod 2^32 */
	hval *= FNV_32_PRIME;
    }
    return hval;
}


/* Expand TAB to be suitable for holding SIZ entries in total.
   Pre-existing entries remain not deleted inside of TAB, but its bins
   are cleared to expect future reconstruction. See rehash below. */
static void
st_expand_table(st_table **tab, st_index_t size)
{
    st_table *tmp;
    st_bucket *bu_cur;
    st_table *old_tab = *tab;
    st_index_t b;

    if (size < old_tab->num_buckets * ENTRIES_PER_BUCKET)
        return; /* enough room already */

    if (TRYLOCK_ACQ(&old_tab->resize_lock))
        return;

    tmp = st_init_table_with_size(old_tab->type, size);
    for (b = 0; b < old_tab->num_buckets; b++) {
        bu_cur = old_tab->bucket + b;
        bucket_copy(bu_cur, tmp);
    }

    tmp->table_new = tmp;
    *tab = tmp;
    TRYLOCK_RLS(old_tab->resize_lock);
    free(old_tab);

    return;
}

void print_ht(int mark, st_table *tab, int dis_bucket) {
    int j, bin = 0, counter = 0;
    printf("\nMARK: %d ", mark);
    printf("bin_power: %u, num_bucket: %u, num_expands:%u, threshold: %u, lock: %u\n", 
            tab->bin_power,
            tab->num_buckets,
            tab->num_expands,
            tab->num_expands_threshold,
            tab->resize_lock);
        
    for (bin = 0; bin < tab->num_buckets; bin++) {
        st_bucket *bucket = tab->bucket + bin;
        if (dis_bucket) printf("\n\n[bucket bin: %u] ==> ", bin);
        do {
            
            for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
                if (dis_bucket) {
                    counter++;
                    printf(" %u: { %u => %u} ", counter, bucket->key[j], bucket->val[j]);
                }
            }
            bucket = bucket->next;
            
        } while (bucket != NULL);
    }

    return;
}

int main() {
    int i = 0, b = 2;
    int *p_i = &i;
    p_i = &b;
    st_table *tab = st_init_numtable_with_size(100);
    // print_ht(tab, 1);
    for (i = 1; i <= 300; i++) {
        //printf("index i:%d \n", i);
        st_insert(tab, i, i);
    }
    //print_ht(tab, 1);
    st_table *cpy = st_copy(tab);

    printf("\\\\\\\\\\\\\\\\\\\\\\\\delete\n");
    st_data_t result = 1;
    st_data_t key;
    st_data_t *p_result = &result;
    for (i = 1; i <= 300; i++) {
        //printf("index i:%d \n", i);
        key = i;
        st_delete(tab, &key, p_result);
    }
    //print_ht(tab, 1);
    print_ht(1, cpy, 0);
    printf("%u\n", cpy);
    st_expand_table(&cpy, 400);
    printf("%u\n", cpy);
    print_ht(2, cpy, 0);
    return 0;
}