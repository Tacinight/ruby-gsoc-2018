
# Function need modified
## All function involved functions starts with "st_" need change
  1. X #325 st_foreach_safe -> st_foreach_check
  2. Y #384 hash_foreach_call -> st_foreach_check
  3. Y hash_foreach_iter -> ntbl
  4. X rb_hash_new_compare_by_id -> rb_init_identtable
  5. Y #446 rb_hash_new_with_size -> st_init_table_with_size
  6. Y #455 hash_dup -> st_copy
  7. W #505 hash_tbl -> st_init_table
  8. W #549 tbl_update -> st_update
  9. W #670 rb_hash_s_create -> st_copy
  10. Y 767 rb_hash_rehash_i <== rb_hash_rehash && rb_hash_compare_by_id
  11. Y #796 rb_hash_rehash -> st_init_table_with_size & st_free_table & ntbl->entries
  12. Y #848 rb_hash_aref -> st_lookup
  13. Y #859 rb_hash_lookup2 -> st_lookup
  14. Y #905 rb_hash_fetch_m -> st_lookup
  15. Y #1116 rb_hash_delete_entry -> st_delete
  16. Y #1216 rb_hash_shift -> st_shift 
  17. Y rb_hash_aset
  18. Y #1289 rb_hash_reject_bang -> ntbl->num_entries
  19. Y #1482 rb_hash_select_bang -> ntbl->num_entries
  20. Y #1536 rb_hash_clear -> st_clear
  21. W #1567 fstring_existing_str -> st_lookup
  22. Y #1668 rb_hash_initialize_copy -> st_copy & st_free_table & ntbl->num_entries
  23. Y rb_hash_replace -> hash_tbl
  24. Y #2158 rb_hash_keys -> st_keys
  25. Y #2202 rb_hash_values -> st_values
  26. Y #2246 rb_hash_has_key -> st_lookup
  27. Y #2299 eql_i -> st_lookup
  28. X #2414 hash_i -> st_hash
  29. Y #2898 rb_hash_compact_bang -> tab->num_entries
  30. Y #2927 rb_hash_compare_by_id -> st_init_table & st_free_table
  31. X #2964 rb_ident_hash_new -> st_init_table
  32. X #2971 rb_init_identtable -> st_init_table
  33. X #2977 rb_init_identtable -> st_init_table_with_size
  34. X #3223 rb_hash_add_new_element -> st_update 


# Difficults Analysis
## Ways to create st_table
1. `h = { 1 => 1, 2 => 2}`
newhash -> rb_hash_new_with_size -> rb_hash_bulk_insert

2. `h = Hash.new`
rb_hash_initialize -> hash_tbl -> st_init_table

3. `h = Hash['a' => 100, 'b' => 200]`
rb_hash_s_create

4. `Hash.try_convert({1=>2})`
rb_hash_s_try_convert -> rb_check_hash_type -> rb_check_convert_type_with_id(hash, T_HASH, "Hash", idTo_hash)

1. how much part of st_table will benefit from this opt?
2. where to defined this struct and related function?
3. some interface functions are hard to modified
        3.1 e.g. rb_hash_bulk_insert is defined in st.c, not hash.c



## Designed opt data structure
- fix array size (4 - 8 entries), linear search
- rewrite necessary function like : st_function_opt
- a flag to use opt
- convert to st_table when rebuilding
- size: 
        - if know the size if less then 4(8), use opt array, 
        - if size is zero, use opt array

## necessarity
Current st_table already optimized for small hash table, when size < 2^4, it use linear search. this part of code is simplified,
I feel it's unnecessary to repeat this part of work.