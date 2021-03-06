# Ruby GSOC 2018
This is a repository for Ruby GSOC 2018.
More information on my project can be found at:
https://summerofcode.withgoogle.com/projects/5822943355994112

[Ticket in Ruby Issue Tracking System](https://bugs.ruby-lang.org/issues/14989)

## Transient Heap
The first discussion of transient heap was opened by ko1(Koichi Sasada) at [Introduce 2nd GC heap named Transient heap](https://bugs.ruby-lang.org/issues/14858). In his prototype, he completed the implement for arrays, and the future work should also support the type String, Hash as well as extend (shrink) policy for transient heap.

My work here is adding support for Hash so that transient heap can allocate space for Hash. I encounted some difficults that had to be handled.
1. Hash type objects use `struct st_table` to store elements, but the correlation between those two is weak. For transient heap, it has to know the ruby type of the objects. The ruby builtin types are represented as something like T_ARRAY, T_HASH. As I mentioned above, current transient heap only supports array(that is T_ARRAY). If we support Hash in transient heap, we need mark Hash in its flag field(`RHASH(hash)->basic.flag`) when transient heap allocates space for its member `struct st_table`. It is hard because Hash and `struct st_table` are quite independent as they are located into two separate files. To do so, it needs modify the interfaces prefixing with 'st', which is almost impossible due to `struct st_table` being used as a general purpose data structure in internal ruby. In this case, the compromise solution is add a dedicated data structure for Hash and it also needs to convert to `struct st_table` when necessary.
2. In our mind, the dedicated data structure is an array which is simple and can cover most cases. It is better if it is a fixed-size array so we don't have to resize it. To determine the size of the array, we need to survey the distribution of hash size.
3. Most of objects die at a young age. Transient heap is based on the hypothesis. After complete the coding part, We also need to know if this data structure has made a good use of transient heap. In other words, the newly introduced data structure will reduce the times of malloc/free, the conversion from malloc/free to transient_alloc will help ruby better manage its memory.

### The distribution of hash size
#### Implement
The capacity of st_table is always a power of 2, we recorded the exponent into a global array before gc or functions explicitly frees st_table. Look at the [patch](patch/st_table_size_statistics.patch) for more details.
#### Results
We measured 6 applications in order to find a real distribution of the hash size. Respectively, those applications are:
1. `make check` command under ruby source code.
2. `make gcbench_rdoc` command under ruby source code.
3. `gem uninstall --all` to uninstall over 300 gems
4. `bundle install` commmand under project [discourse](https://github.com/discourse/discourse)
5. `discourse` manually explore the web pages and simulate the different events for about 20 minutes (development environment)
6. `rails_ruby_bench` project [rails_ruby_bench](https://github.com/noahgibbs/rails_ruby_bench) with parameters : 8 workers, 1500 iterations
7. `rails_ruby_bench 2` 8 workers, 12000 iterations
8. `rails_ruby_bench 3` 20 workders, 20000 iterations

The results show that objects under size 8(area of exponent 2 to 3) account for more than 80% of the total data in most situations. The raw data can be found at [st_table size statistics](https://docs.google.com/spreadsheets/d/1xAjO_qb5K49aLnvk8SypGwO5Avtbm2X12cYb1d-n6Xs/edit?usp=sharing).

![makecheck](imgs/make_check.png)
![make_gcbench_rdoc](imgs/make_gcbench_rdoc.png)
![gem uninstall all](imgs/gem_uninstall_all.png)
![bundle install](imgs/bundle_install.png)
![discourse](imgs/discourse.png)
![rails_ruby_bench](imgs/rails_ruby_bench.png)
![rails_ruby_bench2](imgs/rails_ruby_bench_2.png)
![rails_ruby_bench3](imgs/rails_ruby_bench_3.png)

### Add Hash support
According to the previous survey result, we use a array-like fixed-size table to store the elements. We name it as LinearTable.
In this way, we have two benefits:

1. We don't have to create st_table for small hash(but it will when necessary)
2. the code is written into hash.c (while st_table code is in st.c). its easy to bind small hash with transient heap, and by default, when using st_table, it uses the malloced space.

So the basic data structure look like this.
```c
#define LINEAR_TABLE_MAX_SIZE 8

typedef struct li_table_entry {
    VALUE hash;
    VALUE key;
    VALUE record;
} li_table_entry;

typedef struct LinearTable {
    const struct st_hash_type *type;
    li_table_entry entries[LINEAR_TABLE_MAX_SIZE];
} li_table;

struct RHash {
    struct RBasic basic;
    union {
	struct st_table *ntbl;      /* possibly 0 */
	struct LinearTable *ltbl;
    } as;
    int iter_lev;
    const VALUE ifnone;
};

```
![RHash struct](imgs/RHash.png)

#### The layout of li_table_entry
Considering the size of 8 hashes/keys/records is just the size of a cache line, I also tried different data layouts.

```c
#define LINEAR_TABLE_MAX_SIZE 8

// the original layout
typedef struct li_table_entry {
    VALUE hash;
    VALUE key;
    VALUE record;
} li_table_entry;

typedef struct LinearTable {
    const struct st_hash_type *type;
    li_table_entry entries[LINEAR_TABLE_MAX_SIZE];
} li_table;
```

```c
#define LINEAR_TABLE_MAX_SIZE 8

// the original layout
typedef struct li_table_entry {
    VALUE hash;
    VALUE key;
    VALUE record;
} li_table_entry;

typedef struct LinearTable {
    const struct st_hash_type *type;
    li_table_entry entries[LINEAR_TABLE_MAX_SIZE];
} li_table;
```

```c
#define LINEAR_TABLE_MAX_SIZE 8

// The Variant 1
typedef struct li_table_entry {
    VALUE key;
    VALUE record;
} li_table_entry;

typedef struct LinearTable {
    const struct st_hash_type *type;
    VALUE hashes[LINEAR_TABLE_MAX_SIZE];
    li_table_entry pairs[LINEAR_TABLE_MAX_SIZE];
} li_table;
```

```c
#define LINEAR_TABLE_MAX_SIZE 8

// The Variant 2
typedef struct li_table_entry {
    VALUE hash;
    VALUE key;
} li_table_entry;

typedef struct LinearTable {
    const struct st_hash_type *type;
    li_table_entry entries[LINEAR_TABLE_MAX_SIZE];
    VALUE records[LINEAR_TABLE_MAX_SIZE];
} li_table;
```

```c
#define LINEAR_TABLE_MAX_SIZE 8

// The Variant 3
typedef struct LinearTable {
    const struct st_hash_type *type;
    VALUE hashes[LINEAR_TABLE_MAX_SIZE];
    VALUE keys[LINEAR_TABLE_MAX_SIZE];
    VALUE records[LINEAR_TABLE_MAX_SIZE];
} li_table;
```

The [microbench results](https://docs.google.com/spreadsheets/d/1Ag6DoAsmTNJkt3nmHyfXRQgy6fLhEnZJQF2G5GbykLc/edit?usp=sharing) show the original edition and variant 1 have better performance than the other two. The patches can be found in the repository as [linear_table_v1.patch](patch/linear_table_v1.patch), [linear_table_v2.patch](patch/linear_table_v2.patch), [linear_table_v3.patch](patch/linear_table_v3.patch), [linear_table_v4.patch](patch/linear_table_v4.patch);

#### Integrate data into flag bits
Considering the maximum size of entries is 8. We can store the num_entries and num_bound into RBasic(hash)->flag;
![RHash Flag Bits](imgs/rhash_flag_bits.png)
Related patch is [here](patch/0003-integrate-data-to-hash-flag.patch) which based on [linear_table_v1.patch](patch/linear_table_v1.patch)

### Benchmark results
I execuated comprehensive benchmark test based on official benchmark tool - [benchmark-driver](https://github.com/benchmark-driver/benchmark-driver).

#### Berief Summary
The outcome shows a degree fluctuation of the performance caused by the introduction of the patch. Among these results, the notable performance improvements are:
- so_fannkuch (1.52x)
- vm_thread_pass (1.53x)

and there are also performance degradation among those tests:

- hash_small2 (0.24x)
- hash_small4 (0.28x)
- hash_small8 (0.49x)
- require_thread (0.42x)
- vm1_length (0.63x)
- vm1_not (0.68x)
  
for those not so good test results, I need check what caused those degradation and fix the problems.

### Results list
1. [linear_table_benchmark](https://docs.google.com/spreadsheets/d/18JzY-q-boOZFu_GTwraRXSTJJVbt9Wmg7M2P4-8g41o/edit?usp=sharing), includes the `make benchmark` results of trunk vs. v1(the original version) vs. v2(the variant 1).
2. [transient hash benchmark](https://docs.google.com/spreadsheets/d/19074A0H0nwBQoumTb0aF-k9_a3GBBLA9NnFIpPGraPU/edit?usp=sharing), includes the `make benchmark` results of trunk vs. transient_heap(based on v1) vs. integrated flag
3. [linear_table_variant_benchmark](https://docs.google.com/spreadsheets/d/1Ag6DoAsmTNJkt3nmHyfXRQgy6fLhEnZJQF2G5GbykLc/edit?usp=sharing), includes the micro-benchmark results of trunk vs. v1 vs. v2 vs. v3 vs. v4. The used benchrmark scripts are located at microbench directory.
4.  [linear_table real app bench](https://docs.google.com/spreadsheets/d/1WoT2zRxm16DA-0fOf55HZKmRN9v2YJK4HoaMi6yq_0k/edit?usp=sharing), includes the test results of the six applications noted above.
5.  [transient heap microbench](https://docs.google.com/spreadsheets/d/1guaP_93ds1eSbdSg87gBrtiuJ8JzSFHJb_xcQsxn8eg/edit?usp=sharing), include the micro-benchrmark resutls of trunk vs. v1 vs. transient-hash vs integrated-flag. This time I also add memory usage comparison.
6. [linear_table_vary_size_microbench](https://docs.google.com/spreadsheets/d/1I7NFc893H7cUh0CAFgBupT259pVMqRFjC9CQwlE8Kyw/edit?usp=sharing), we once worried the size of linear table will impact on the benchmark results because of the linear search. So we test the linear table with different size and the results show the table size make no relevant difference.

### Future work
1. In [transient hash benchmark](https://docs.google.com/spreadsheets/d/19074A0H0nwBQoumTb0aF-k9_a3GBBLA9NnFIpPGraPU/edit?usp=sharing), we saw a preformance degradation compare to the preformance improvement in [linear_table_benchmark](https://docs.google.com/spreadsheets/d/18JzY-q-boOZFu_GTwraRXSTJJVbt9Wmg7M2P4-8g41o/edit?usp=sharing) in same items. It needs to be analyzed and to be solved.
2. Check conversion rate from small st_table to linear table and to see does it make good use of the new mechanism. 

## Cache Line Hash Table
Besides the efforts for transient heap, my work in this period also includes a attempt to introduce a hash table algorithm named [Cache Line Hash Table](https://github.com/LPD-EPFL/CLHT).

The attempt failed due to two main reasions:
1. The hash table in ruby not only do the operations like insert, update, and delete, but also includes lots of sequential operations like foreach, shift. The modification for this purpose will destroy the original design principle.
2. The memory usage is bad after modification.

Hence the development for cache line hash table stopped. The [patch](patch/0001-introduce-cache-line-hash-table.patch) and code is reversed in this repository. Benchmark results are saved as `report/bmlog-20180701-152738.21712.tsv` and `report/bmlog-20180706-155054.19279.tsv`.

## Other work
The one goal of this project is explore the gc performance when Ruby allocated a large number of objects. We expected abnormal overheads when memory is under very high pressure or running out. But the most significant overhead is reallocation when array or hash resizing its capacity. Otherwise, the time cost for operations like insertion changed as expected only with acceptable overheads.

The related scripts and reports are:
1. array_batch_benchmark.rb : Let's say we want to create a certain number of objects in one array, this number are divided into certain rounds, we want to know if the later rounds will spend more time than former rounds. The results ([report1](report/array_batch_benchmark_report_9B.txt) and [report2](report/array_batch_benchmark_report_25B.txt)) show it doesn't make any measurable difference in my experimental setup.
2. hash_batch_benchmark.rb : same as the one above expect changing array to hash. Reports are [this](report/hash_batch_benchmark_report_2B.txt) and [this](report/hash_batch_benchmark_report_5B.txt).
3. array_vary_size_benchmark.rb : We hope the creation time of array is linear and not exponential. And the results show they are close to linear relation. Reports are [this](report/array_benchmark_report_N30_v1.txt) and [this](report/array_benchmark_report_N31.txt).
4. hash_vary_size_benchmark.rb : same as the one above expect changing array to hash. Reports are [this](report/hash_benchmark_report_N30.txt) and [this](report/hash_benchmark_report_N32.txt).

## LICENSE
[BSD-2-Clause](https://opensource.org/licenses/BSD-2-Clause)