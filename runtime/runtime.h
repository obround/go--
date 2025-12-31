#ifndef GORUNTIME_H
#define GORUNTIME_H

#include <stddef.h>
#include <stdint.h>

// String type: { data pointer, length }
typedef struct {
    char* data;
    int64_t len;
} go_string;

// Slice type: { data pointer, length, capacity }
typedef struct {
    void* data;
    int64_t len;
    int64_t cap;
} go_slice;

typedef void* go_map;
typedef void* go_map_iter;

// String operations
go_string go_string_concat(go_string a, go_string b);
int32_t go_string_compare(go_string a, go_string b);
go_string go_string_from_cstr(const char* cstr);
go_string go_string_from_bytes(const char* data, int64_t len);
go_slice go_string_to_bytes(go_string s);
int64_t go_string_decode_rune(const char* data, int64_t len);

// Slice operations
void go_slice_append(go_slice* result, go_slice* slice, void* elem,
                     int64_t elem_size);
int64_t go_slice_copy(go_slice* dst, go_slice* src, int64_t elem_size);
void go_slice_grow(go_slice* result, go_slice* slice, int64_t newcap,
                   int64_t elem_size);

// Map operations
go_map go_map_make(int64_t key_size, int64_t value_size, int64_t key_type,
                   int64_t hint);
void* go_map_get(go_map m, void* key, int64_t key_size, int64_t value_size);
void go_map_set(go_map m, void* key, void* value, int64_t key_size,
                int64_t value_size);
void go_map_delete(go_map m, void* key, int64_t key_size);
int64_t go_map_len(go_map m);

// Map iteration
go_map_iter go_map_iter_init(go_map m);
int32_t go_map_iter_next(go_map_iter iter);
void* go_map_iter_key(go_map_iter iter);
void* go_map_iter_value(go_map_iter iter);

// Print operations
void go_print_int(int64_t v);
void go_print_uint(int64_t v);
void go_print_float(double v);
void go_print_bool(int8_t v);
void go_print_string(go_string s);
void go_print_pointer(void* p);
void go_print_space(void);
void go_print_newline(void);

// Panic
void go_panic(go_string msg);
void go_panic_bounds(int64_t index, int64_t len);
void go_panic_nil(void);

#endif  // GORUNTIME_H
