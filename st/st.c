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

#define EXPANSION_RATIO 2
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

    assert(*lock == LOCK_FREE);
    while ((l = ATOMIC_CAS(lock, LOCK_FREE, LOCK_UPDATE)) == LOCK_UPDATE){
        if (once) {
            once = 0;
        }
        assert(l == LOCK_UPDATE);
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
#define PTR_EQUAL(tab, entry, hash_, key_) \
    (entry.hash == hash_) && EQUAL((tab), (entry.key), (key_))
    
/* As PRT_EQUAL only its result is returned in RES.  REBUILT_P is set
   up to TRUE if the table is rebuilt during the comparison.  */
#define DO_PTR_EQUAL_CHECK(tab, ptr, hash_val, key, res, rebuilt_p) \
    do {							    \
	unsigned int _old_version = (tab)->version;       \
	res = PTR_EQUAL(tab, ptr, hash_val, key);		    \
	rebuilt_p = _old_version != (tab)->version;	    \
    } while (FALSE)

#define MAX_POWER2 62

/* The reserved hash value and its substitution.  */
#define RESERVED_HASH_VAL (~(st_hash_t) 0)
#define RESERVED_HASH_SUBSTITUTION_VAL ((st_hash_t) 0)

/* Return hash value of KEY for table TAB.  */
static inline st_hash_t
do_hash(st_data_t key, st_table *tab)
{
    return (st_hash_t)(tab->type->hash)(key);
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

static inline void
clear_entry(st_entry* entry)
{
    entry->hash = 0;
    entry->key = 0;
}

static inline void
set_entry(st_entry* entry, st_hash_t hash, st_data_t key, st_data_t val)
{
    entry->hash = hash;
    entry->record = val;
    entry->key = key;
}

static inline int
entry_empty(st_entry* entry)
{
    return entry == NULL || (entry->key == 0 && entry->hash == 0);
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


/* Return the index of table TAB bin corresponding to
   HASH_VALUE.  */
static inline st_index_t
hash_bin(st_table *tab, st_data_t key)
{
    return do_hash(key, tab) & tab->num_buckets - 1;
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
	    clear_entry(&bucket->entry[j]);
    }
    bucket->next = NULL;
    return bucket;
}

st_bucket *
bucket_create_stats(st_table *tab, int *resize)
{
    st_bucket *bucket = bucket_create();
    if (IAF_U32(&tab->num_expands) == tab->num_expands_threshold) {
	    //printf("\n-- hit threshold (%u ~ %u)\n", tab->num_expands, tab->num_expands_threshold);
	    *resize = 1;
    }
    return bucket;
}

static inline int
is_small_table(st_table *tab)
{
    return tab->num_buckets < SMALL_TABLE_THRESHOLD;
}

/* Create and return table with TYPE which can hold at least SIZE
   entries.  The real number of entries which the table can hold is
   the nearest power of two for SIZE.  */
st_table *
st_init_table_with_size(const struct st_hash_type *type, st_index_t size)
{
    st_table *tab;
    st_data_t n, num_buckets;
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
    tab->version = 0;
    tab->table_new = NULL;
    tab->resize_lock = LOCK_FREE;
    tab->num_expands = 0;
    tab->num_entries = 0;
    tab->entry_bound = 0;
    num_buckets = n < SMALL_TABLE_THRESHOLD ? size + 1 : ((st_data_t)1 << n);
    tab->num_buckets = num_buckets;
    tab->bucket = (st_bucket *) memalign(CACHE_LINE_SIZE, num_buckets * sizeof(st_bucket));
    tab->ordered_entry = (st_index_t *) memalign(CACHE_LINE_SIZE,
                                               num_buckets * 6 * sizeof(st_data_t));
    tab->num_expands_threshold = EXPANSION_RATIO * num_buckets;

    memset(tab->ordered_entry, 0, num_buckets * 6 * sizeof(st_data_t));
    memset(tab->bucket, 0, num_buckets * (sizeof(st_bucket)));

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

/* put object into hash table, this function wouldn't hit rebuilding 
   so be sure that the hash table have enough capacity before invoke 
   this function */
static int
st_put_seq(st_table *tab, st_data_t key, st_data_t val, st_data_t bin)
{
    st_bucket *bucket = tab->bucket + bin;
    st_hash_t hash_value = do_hash(key, tab);
    size_t j;

    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            if (entry_empty(&bucket->entry[j])) {
                set_entry(&bucket->entry[j], hash_value, key, val);
                tab->ordered_entry[tab->entry_bound++] = (st_index_t)&bucket->entry[j];
                tab->num_entries++;
                return 1;
            }
        }

        if (bucket->next == NULL) {
            int null;
            st_bucket *b = bucket_create_stats(tab, &null);
            set_entry(&b->entry[0], hash_value, key, val);
            tab->ordered_entry[tab->entry_bound++] = (st_index_t)&b->entry[0];
            bucket->next = b;
            tab->num_entries++;
            return 1;
        }
        
        bucket = bucket->next;
    } while (1);
}

static int 
bucket_copy(st_bucket *bucket, st_table *tab)
{
    size_t j;
    st_lock_t *lock = &bucket->lock;
    if (!LOCK_ACQ_RES(lock))
	    return 0;

    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            st_data_t key = bucket->entry[j].key;
            if (key != 0) {
                st_index_t bin = hash_bin(tab, key);
                st_put_seq(tab, key, bucket->entry[j].record, bin);
            }
        }
	    bucket = bucket->next;
    } while (bucket != NULL);

    LOCK_RLS(lock);
    return 1;
}

static int
bucket_copy_ordered(st_table *new_tab, st_table *old_tab)
{
    size_t j;
    for (j = 0; j < old_tab->entry_bound; j++) {
        if (entry_empty((st_entry*)old_tab->ordered_entry[j])) continue;
        st_entry *cur_entry = (st_entry*)old_tab->ordered_entry[j];
        st_index_t bin = hash_bin(new_tab, cur_entry->key);
        st_put_seq(new_tab, cur_entry->key, cur_entry->record, bin);
    }
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
    assert(tab->resize_lock == 0xff);

    if (is_increase) 
	    num_buckets_new = by * tab->num_buckets;
    else
	    num_buckets_new = tab->num_buckets / HALVE_RATIO;

    new_tab = (st_table *) memalign(CACHE_LINE_SIZE, sizeof(st_table));
    new_tab->type = tab->type;
    new_tab->num_buckets = tab->num_buckets * by;
    new_tab->entry_bound = 0;
    new_tab->bucket = (st_bucket *) memalign(CACHE_LINE_SIZE, new_tab->num_buckets * sizeof(st_bucket));
    new_tab->ordered_entry = (st_index_t *) memalign(CACHE_LINE_SIZE,
                                               new_tab->num_buckets * 6 * sizeof(st_data_t));
    new_tab->num_expands = 0;
    new_tab->num_expands_threshold = EXPANSION_RATIO * new_tab->num_buckets;
    new_tab->version = tab->version + 1;

    memset(new_tab->bucket, 0, new_tab->num_buckets * (sizeof(st_bucket)));
    memset(new_tab->ordered_entry, 0, new_tab->num_buckets * 6 * sizeof(st_data_t));
    // for (b = 0; b < tab->num_buckets; b++) {
    //     st_bucket * bu_cur = tab->bucket + b;
    //     bucket_copy(bu_cur, new_tab);
    // }
    bucket_copy_ordered(new_tab, tab);

    tab->table_new = tab;
    tab->version++;
    tab->num_expands = new_tab->num_expands;
    tab->bucket = new_tab->bucket;
    tab->num_buckets = new_tab->num_buckets;
    tab->entry_bound = new_tab->entry_bound;
    tab->ordered_entry = new_tab->ordered_entry;
    tab->num_expands_threshold = new_tab->num_expands_threshold;
    free(new_tab);
    TRYLOCK_RLS(tab->resize_lock);
    assert(tab->resize_lock == LOCK_FREE);
    return 1;
}

/* Find an entry with KEY in table TAB.  Return non-zero if we found
   it.  Set up *RECORD to the found entry record.  */
int
st_lookup(st_table *tab, st_data_t key, st_data_t *value)
{
    st_index_t bin = hash_bin(tab, key);
    volatile st_bucket *bucket = tab->bucket + bin;
    st_hash_t hash_value = do_hash(key, tab);
    size_t j;

    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            st_data_t val = bucket->entry[j].record;
            if (PTR_EQUAL(tab, bucket->entry[j], hash_value, key)) {
                if (LIKELY(bucket->entry[j].record == val)) {
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
    st_index_t bin = hash_bin(tab, key);
    volatile st_bucket *bucket = tab->bucket + bin;
    st_hash_t hash_value = do_hash(key, tab);

    size_t j;
    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            st_data_t val = bucket->entry[j].record;
            if (PTR_EQUAL(tab, bucket->entry[j], hash_value, key)) {
                *result = bucket->entry[j].key;
                return 1;
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
    st_index_t bin = hash_bin(tab, key);
    st_bucket *bucket = tab->bucket + bin;
    st_hash_t hash_value = do_hash(key, tab);
    st_lock_t *lock = &bucket->lock;
    st_entry *empty = NULL;
    size_t j, j1; 
    int resize = 0;
    
    while (!LOCK_ACQ(lock, tab)) {
        bin = hash_bin(tab, key);
        bucket = tab->bucket + bin;
        lock = &bucket->lock;
    }
    assert(*lock == LOCK_UPDATE);

    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            st_data_t val = bucket->entry[j].record;
            if (PTR_EQUAL(tab, bucket->entry[j], hash_value, key)) {
                if (LIKELY(bucket->entry[j].record == val)) {
                    bucket->entry[j].record = value;
                    LOCK_RLS(lock);
                    return 1;
                }
            }
            else if (empty == NULL && entry_empty(&bucket->entry[j])) {
                empty = &bucket->entry[j];
            }
        }
             
        if (LIKELY(bucket->next == NULL)) {
            if (UNLIKELY(empty == NULL)) {
                st_bucket *b = bucket_create_stats(tab, &resize);
                set_entry(&b->entry[0], hash_value, key, value);
                tab->ordered_entry[tab->entry_bound++] = (st_index_t)&b->entry[0];
                bucket->next = b;

            }
            else {
                set_entry(empty, hash_value, key, value);
                tab->ordered_entry[tab->entry_bound++] = (st_index_t)empty;
            }
            
            tab->num_entries++;
            LOCK_RLS(lock);
            assert(*lock == LOCK_FREE);

            if (UNLIKELY(resize)) {
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
    st_data_t bin = hash_bin(tab, key);
    st_put_seq(tab, key, value, bin);
}

/* Insert (FUNC(KEY), VALUE) into table TAB and return zero.  If
   there is already entry with KEY in the table, return nonzero and
   and update the value of the found entry.  */
int
st_insert2(st_table *tab, st_data_t key, st_data_t value,
           st_data_t (*func)(st_data_t))
{
    st_index_t bin = hash_bin(tab, key);
    st_bucket *bucket = tab->bucket + bin;
    st_lock_t *lock = &bucket->lock;
    st_hash_t hash_value = do_hash(key, tab);
    st_entry *empty = NULL;
    size_t j; 
    int resize = 0;
    

    while (!LOCK_ACQ(lock, tab)) {
        bin = hash_bin(tab, key);
        bucket = tab->bucket + bin;
        lock = &bucket->lock;
    }
    assert(*lock == LOCK_UPDATE);
    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            st_data_t val = bucket->entry[j].record;
            if (PTR_EQUAL(tab, bucket->entry[j], hash_value, key)) {
                if (bucket->entry[j].record == val) {
                    bucket->entry[j].record = value;
                    LOCK_RLS(lock);
                    return 1;
                }
            }
            else if (empty == NULL && entry_empty(&bucket->entry[j])) {
                empty = &bucket->entry[j];
            }
        }

        if (LIKELY(bucket->next == NULL)) {
            if (UNLIKELY(empty == NULL)) {
                st_bucket *b = bucket_create_stats(tab, &resize);
                set_entry(&b->entry[0], hash_value, (*func)(key), value);
                tab->ordered_entry[tab->entry_bound++] = (st_index_t)&b->entry[0];
                bucket->next = b;

            }
            else {
                set_entry(empty, hash_value, (*func)(key), value);
                tab->ordered_entry[tab->entry_bound++] = (st_index_t)empty;
            }

            tab->num_entries++;
            LOCK_RLS(lock);
            assert(*lock == LOCK_FREE);
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
    new_tab->version = old_tab->version;
    new_tab->type = old_tab->type;
    new_tab->table_new = NULL;
    new_tab->resize_lock = LOCK_FREE;
    new_tab->num_expands = 0;
    new_tab->entry_bound = 0;
    new_tab->num_buckets = old_tab->num_buckets;
    new_tab->bucket = (st_bucket*) memalign(CACHE_LINE_SIZE, 
                                            old_tab->num_buckets * sizeof(st_bucket));
    new_tab->ordered_entry = (st_index_t*) memalign(CACHE_LINE_SIZE, 
                                                    6 * old_tab->num_buckets * sizeof(st_data_t));
    new_tab->num_expands_threshold = old_tab->num_expands_threshold;
    
    memset(new_tab->bucket, 0, new_tab->num_buckets * (sizeof(st_bucket)));
    memset(new_tab->ordered_entry, 0, new_tab->num_buckets * 6 * sizeof(st_data_t));
    // for (b = 0; b < old_tab->num_buckets; b++) {
    //     st_bucket *bu_cur = old_tab->bucket + b;
    //     bucket_copy(bu_cur, new_tab);
    // }
    bucket_copy_ordered(new_tab, old_tab);

    return new_tab;
}


/* Delete entry with KEY from table TAB, set up *VALUE (unless
   VALUE is zero) from deleted table entry, and return non-zero.  If
   there is no entry with KEY in the table, clear *VALUE (unless VALUE
   is zero), and return zero.  */
static int
st_general_delete(st_table *tab, st_data_t *key, st_data_t *value)
{
    st_index_t bin = hash_bin(tab, *key);
    size_t j;
    st_bucket *bucket = tab->bucket + bin;
    st_lock_t *lock = &bucket->lock;
    st_hash_t hash_value = do_hash(*key, tab);

    assert(*lock == LOCK_FREE);
    while (!LOCK_ACQ(lock, tab)) {
        bin = hash_bin(tab, *key);
        bucket = tab->bucket + bin;
        lock = &bucket->lock;
    }
    assert(*lock == LOCK_UPDATE);
    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            if (PTR_EQUAL(tab, bucket->entry[j], hash_value, *key)) {
                clear_entry(&bucket->entry[j]);
                if (value != 0) 
                    *value = bucket->entry[j].record;
                LOCK_RLS(lock);
                assert(*lock == LOCK_FREE);
                return 1;
            }
        }
	    bucket = bucket->next;
    } while (UNLIKELY(bucket != NULL));

    if (value != 0) *value = 0;
    LOCK_RLS(lock);
    assert(*lock == LOCK_FREE);
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
    st_index_t bin;
    size_t j;
    st_bucket *bucket = NULL;

    for (j = 0; j < tab->entry_bound; j++) {
        if (entry_empty((st_entry*)tab->ordered_entry[j])) continue;
        st_entry *cur_entry = (st_entry*)tab->ordered_entry[j];
        if (value != 0) 
            *value = cur_entry->record;
        *key = cur_entry->key;
        clear_entry(cur_entry);
        return 1;
    }

    if (value != 0) *value = 0;
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
    st_index_t bin = hash_bin(tab, key);
    st_entry *cur_entry = NULL;
    st_data_t value = 0, old_key;
    int retval, existing = 0;
    size_t j;
    st_bucket *bucket = tab->bucket + bin;
    st_lock_t *lock = &bucket->lock;
    st_hash_t hash_value = do_hash(key, tab);

    do {
        for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
            st_data_t val = bucket->entry[j].record;
            if (PTR_EQUAL(tab, bucket->entry[j], hash_value, key)) {
                if (LIKELY(bucket->entry[j].record == val)) {
                    key = bucket->entry[j].key;
                    value = bucket->entry[j].record;
                    cur_entry = &bucket->entry[j];
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
                cur_entry->key = key;
            }
            cur_entry->record = value;
            break;
        case ST_DELETE: 
            if (existing) {
                clear_entry(cur_entry);
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
    st_bucket *bucket = NULL;

    for (j = 0; j < tab->entry_bound; j++) {
        if (j >= tab->num_entries) return 0;

        st_entry *cur_entry = (st_entry*)tab->ordered_entry[j];

        retval = (*func)(cur_entry->key, cur_entry->record, arg, 0);

        if (version != tab->version) {
            if (check_p) {
                retval = (*func)(0, 0, arg, 1);
                return 1;
            }
        }

        switch (retval) {
            case ST_CHECK:
                if (check_p) break;
            case ST_CONTINUE:
                break;
            case ST_STOP:
                return 0;
            case ST_DELETE: 
                clear_entry(cur_entry);
                break;
        }
    }
    // for (bin = 0; bin < tab->num_buckets; bin++) {
	//     bucket = tab->bucket + bin;

    //     do {
    //         for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
    //             if (!entry_empty(&bucket->entry[j])) {
    //                 
    //             }

    //         }
    //         bucket = bucket->next;
    //     } while (bucket != NULL);
    // }

    return 0;
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
    st_index_t j, bin;
    st_data_t *keys_start, *keys_end;
    st_bucket *bucket = NULL;

    keys_start = keys;
    keys_end = keys + size;

    for (j = 0; j < tab->entry_bound; j++) {
        if (j >= tab->num_entries) break;

        if (keys == keys_end)
            break;
        
        st_entry *cur_entry = (st_entry*)tab->ordered_entry[j];

        *keys++ = cur_entry->key;
    }
    // for (bin = 0; bin < tab->num_buckets; bin++) {
    //     if (keys == keys_end)
    //         break;
    //     bucket = tab->bucket + bin;

    //     do {
    //         for (i = 0; i < ENTRIES_PER_BUCKET; i++) {
    //             if (!entry_empty(&bucket->entry[i])) {
    //                 *keys++ = bucket->entry[i].key;
    //             }
    //         }
    //         bucket = bucket->next;
    //     } while (bucket != NULL);
    // }

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
    st_index_t j, bin;
    st_data_t *values_start, *values_end;
    st_bucket *bucket = NULL;

    values_start = values;
    values_end = values + size;
    for (j = 0; j < tab->entry_bound; j++) {
        if (j >= tab->num_entries) break;

        if (values == values_end)
            break;
        
        st_entry *cur_entry = (st_entry*)tab->ordered_entry[j];

        *values++ = cur_entry->key;
    }
    
    // for (bin = 0; bin < tab->num_buckets; bin++) {
    //     if (values == value_end)
    //         break;
    //     bucket = tab->bucket + bin;

    //     do {
    //         for (i = 0; i < ENTRIES_PER_BUCKET; i++) {
    //             if (!entry_empty(&bucket->entry[i])) {
    //                 *values++ = bucket->entry[i].record;
    //             }
    //         }
    //         bucket = bucket->next;
    //     } while (bucket != NULL);
    // }

    return values - values_start;
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
st_expand_table(st_table *tab, st_index_t size)
{
    st_table *tmp;
    st_bucket *bu_cur;
    st_table *old_tab = tab;
    st_index_t b;

    if (size < tab->num_buckets * ENTRIES_PER_BUCKET)
        return; /* enough room already */

    if (TRYLOCK_ACQ(&tab->resize_lock))
        return;
    assert(tab->resize_lock == 0xff);

    tmp = st_init_table_with_size(tab->type, size);
    tmp->version = tab->version + 1;
    
    // for (b = 0; b < tab->num_buckets; b++) {
    //     bu_cur = tab->bucket + b;
    //     bucket_copy(bu_cur, tmp);
    // }
    bucket_copy_ordered(tmp, tab);
    tab->table_new = tmp;
    tab->version = tmp->version;
    tab->num_buckets = tmp->num_buckets;
    tab->num_expands = tmp->num_expands;
    tab->bucket = tmp->bucket;
    tab->ordered_entry = tmp->ordered_entry;
    tab->num_expands_threshold = tmp->num_expands_threshold;
    free(old_tab);
    TRYLOCK_RLS(tab->resize_lock);
    assert(tab->resize_lock == LOCK_FREE);

    return;
}
 
void print_ht(int mark, st_table *tab, int dis_bucket) {
    int j, bin = 0, counter = 0;
    printf("\n------------------------[MARK: %d]--------------------------------\n", mark);
    printf("version: %u, num_bucket: %u, num_entries: %u, num_expands: %u, threshold: %u, resize_lock: %u", 
            tab->version,
            tab->num_buckets,
            tab->num_entries,
            tab->num_expands,
            tab->num_expands_threshold,
            tab->resize_lock);
        
    for (bin = 0; bin < tab->num_buckets; bin++) {
        st_bucket *bucket = tab->bucket + bin;
        if (dis_bucket) printf("\n\n[bin: %u, lock: %u] ==> ", bin, bucket->lock);
        do {
            for (j = 0; j < ENTRIES_PER_BUCKET; j++) {
                if (dis_bucket) {
                    counter++;
                    st_entry *entry = &bucket->entry[j];
                    printf(" #%u: {0x%x, %u => %u} ", counter, entry->hash, entry->key, entry->record);
                }
            }
            bucket = bucket->next;
        } while (bucket != NULL);
    }

    printf("\n-------------------------------\n");
    return;
}

static st_data_t doublekey(st_data_t key) { 
    if (key & 1)
        return 0;
    else
        return key;
}

static int 
doublevalue(st_data_t *key, st_data_t *value, st_data_t arg, int existing) {
    if ((*key) & 1) return ST_DELETE;
    else
        *value = 2 * (*value);
    return ST_CONTINUE;
}
static int
foreach_func(st_data_t key, st_data_t val, st_data_t arg, int last) {
    printf("key: %u, value: %u\n", key, val);
    return ST_CONTINUE;
}

static int
foreach_continue_func(st_data_t key, st_data_t val, st_data_t arg, int last) {
    return ST_CONTINUE;
}

static int
foreach_stop_func(st_data_t key, st_data_t val, st_data_t arg, int last) {
    if (key == 44) return ST_STOP;
}

static int
foreach_delete_func(st_data_t key, st_data_t val, st_data_t arg, int last) {
    if (key & 1) return ST_DELETE;
    return ST_CONTINUE;
}

int main() {
    st_data_t i = 0, ii;
    st_data_t result = 1;
    st_data_t *p_result = &result;
    st_data_t key;
    st_data_t keys[100], values[100];
    st_data_t *keys_p, *values_p;
    st_data_t (*fun_p)(st_data_t) = &doublekey;
    int (*update_func_p)(st_data_t*, st_data_t*, st_data_t, int) = &doublevalue;
    int (*foreach_func_p)(st_data_t, st_data_t, st_data_t, int) = &foreach_func;
    int (*continue_func_p)(st_data_t, st_data_t, st_data_t, int) = &foreach_continue_func;
    int (*stop_func_p)(st_data_t, st_data_t, st_data_t, int) = &foreach_stop_func;
    int (*delete_func_p)(st_data_t, st_data_t, st_data_t, int) = &foreach_delete_func;

    printf("sizeof st_table: %d\n", sizeof(st_table));
    printf("sizeof st_bucket: %d\n", sizeof(st_bucket));
    printf("sizeof st_entry: %d\n\n", sizeof(st_entry));

    /* 1. Tests for creating table with various size
     *
     */
    st_table *tab = st_init_numtable_with_size(100); 
    for (i = 1; i <= 300; i++) {
        st_insert(tab, i, i);
        assert(tab->num_entries == i);
    }
    for (i = 1, result = 0; i <= 300; i++) {
        int ret = st_lookup(tab, i, p_result);
        assert(ret);
        assert(result == i);
    }


    /* 2. Test for st_copy()
     * the copy should have exact same but independent elements as origin one 
     */ 
    st_table *cpy = st_copy(tab);
    assert(cpy->version == tab->version);
    assert(cpy->num_buckets == tab->num_buckets);
    assert(cpy->num_expands == tab->num_expands);
    assert(cpy->num_entries == tab->num_entries);
    assert(cpy->num_expands_threshold == tab->num_expands_threshold);
    assert(cpy->type == tab->type);
    assert(cpy->resize_lock == LOCK_FREE);
    assert(cpy->table_new == NULL);
    assert(cpy->bucket != tab->bucket);
    for (i = 1, result = 0; i <= 300; i++) {
        int ret = st_lookup(cpy, i, p_result);
        assert(ret);
        assert(result == i);
    }


    /* 3. Test for copy, deletion and expansion operation.
     *  
     */ 
    for (i = 1, result = 0; i <= 300; i++) {
        key = i;
        int ret = st_delete(tab, &key, p_result);
        assert(ret);
        assert(result == i);
    }
    st_expand_table(cpy, 400);

    /* 4. Test for st_insert2()
     * the func pointer return 0 if key is odd, return key if key is even
     */ 
    for (i = 1; i <= 300; i++) {
        key = i;
        st_insert2(tab, key, i, fun_p);
    }
    for (i = 1, result = 0; i <= 300; i++) {
        int ret = st_lookup(tab, i, p_result);
        if (i & 1) 
            assert(!ret); 
        else {
            assert(ret);
            assert(result == i);
        }
    }

    /* 5. Tests for auto resizeing operation, the table should resize if hit
     * expansion threshold. The assert judge whether all value can be found 
     * in the hash table.   
     */
    int prev_num_buckets = tab->num_buckets;
    rebuild_table(tab, 1, 2);
    assert(prev_num_buckets < tab->num_buckets);

    st_table *resize_tab;
    for (ii = 1; ii < 3000; ii++) {
        resize_tab = st_init_numtable();
        for (i = 1; i <= ii; i++) {
            st_insert(resize_tab, i, i);
        }
        //print_ht(ii, resize_tab, 1);
        //st_foreach(resize_tab, foreach_func, (st_data_t)1);
        for (i = 1, result = 0; i <= ii; i++) {
            int ret = st_lookup(resize_tab, i, p_result);
            assert(ret);
            assert(result == i);
        }
        free(resize_tab);
    }

    /* 6. Tests for st_shift() */
    st_table *empty = st_init_numtable();
    st_data_t ret_k = 0, ret_v = 1;
    int ret = st_shift(empty, &ret_k, &ret_v);
    assert(!ret);
    assert(ret_v == 0);

    ret_k = ret_v = 0;
    assert(ret_k == 0);
    assert(ret_v == 0);
    for (i = 1; i <= 100; i++) {
        st_insert(empty, i, i);
    }
    for (i = 1; i <= 100; i++) {
        ret = st_shift(empty, &ret_k, &ret_v);
        assert(ret != 0);
        assert(ret_k == i);
        assert(ret_v == i);
    }


    /* 7. Tests for st_get_key() */
    resize_tab = st_init_numtable();
    for (i = 1; i <= 100; i++) {
        st_insert(resize_tab, i, i);
    }
    for (i = 1, result = 0, ret = 0; i <= 100; i++) {
        ret = st_get_key(resize_tab, i, p_result);
        assert(ret);
        assert(result == i);
    }


    /* 8. Tests for st_update() */
    st_table *update_tab = st_init_numtable();
    for (i = 1; i < 100; i++) {
        st_insert(update_tab, i, i);
    }
    for (i = 1; i < 100; i++) {
        ret = st_update(update_tab, i, update_func_p, 1);
        assert(ret);
    }
    

    /* 9. Tests for st_foreach() */
    st_table *tab9 = st_init_numtable();
    for (i = 100; i <= 200; i++) {
        st_insert(tab9, i, i);
    }

    ret = st_foreach(resize_tab, foreach_func, (st_data_t)1);
    assert(ret == 0);

    ret = st_foreach(tab9, continue_func_p, (st_data_t)1);
    assert(ret == 0);

    ret = st_foreach(tab9, stop_func_p, (st_data_t)1);
    assert(ret == 0);

    ret = st_foreach(tab9, delete_func_p, (st_data_t)1);
    for (i = 1; i < 100; i++) {
        if (i & 1) {
            ret = st_lookup(tab9, i, p_result);
            assert(!ret);
        }
    }
    free(tab9);


    /* 10. Tests for st_keys() & st_values() */
    st_table *tab10 = st_init_numtable();
    memset(keys, 0, 100);
    memset(values, 0, 100);
    keys_p = keys;
    values_p = values;
    for (i = 1; i <= 100; i++) 
        st_insert(tab10, i, i);
    ret = st_keys(tab10, keys_p, 100);
    assert(ret == 100);
    ret = st_values(tab10, values_p, 100);
    assert(ret == 100);
    for (i = 0; i< 100; i++) {
        assert(keys[i] == (i + 1));
        assert(values[i] == (i + 1));
        assert(keys[i] == values[i]);        
    }
    free(tab10);

    /* 11. Tests for st_add_direct() */
    st_table *tab11 = st_init_numtable_with_size(100);
    for (i = 1; i <= 100; i++)
        st_add_direct(tab11, i, i);

    for (i = 1; i <= 100; i++) {
        ret = st_lookup(tab11, i, p_result);
        assert(ret);
        assert(*p_result == i);
    }
    //print_ht(1, tab11, 1);
    return 0;
}