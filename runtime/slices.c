#include <gc.h>
#include <string.h>

#include "runtime.h"

void go_slice_append(go_slice* result, go_slice* slice, void* elem,
                     int64_t elem_size) {
    *result = *slice;

    if (result->len >= result->cap) {
        int64_t newcap = result->cap == 0 ? 1 : result->cap * 2;
        go_slice_grow(result, result, newcap, elem_size);
    }

    memcpy((char*)result->data + result->len * elem_size, elem, elem_size);
    result->len++;
}

int64_t go_slice_copy(go_slice* dst, go_slice* src, int64_t elem_size) {
    int64_t n = dst->len < src->len ? dst->len : src->len;
    if (n > 0) {
        memcpy(dst->data, src->data, n * elem_size);
    }
    return n;
}

void go_slice_grow(go_slice* result, go_slice* slice, int64_t newcap,
                   int64_t elem_size) {
    void* old_data = slice->data;
    int64_t old_len = slice->len;

    result->len = old_len;
    result->cap = newcap;
    result->data = GC_malloc(newcap * elem_size);
    if (result->data == NULL) {
        go_panic((go_string){"out of memory", 13});
    }

    if (old_len > 0 && old_data != NULL) {
        memcpy(result->data, old_data, old_len * elem_size);
    }
}
