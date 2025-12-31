#include <gc.h>
#include <string.h>

#include "runtime.h"

go_string go_string_concat(go_string a, go_string b) {
    go_string result;
    result.len = a.len + b.len;
    result.data = (char*)GC_malloc(result.len);
    if (result.data == NULL) {
        go_panic((go_string){"out of memory", 13});
    }
    memcpy(result.data, a.data, a.len);
    memcpy(result.data + a.len, b.data, b.len);
    return result;
}

int32_t go_string_compare(go_string a, go_string b) {
    int64_t min_len = a.len < b.len ? a.len : b.len;
    int cmp = memcmp(a.data, b.data, min_len);
    if (cmp != 0) return cmp;
    if (a.len < b.len) return -1;
    if (a.len > b.len) return 1;
    return 0;
}

go_string go_string_from_cstr(const char* cstr) {
    go_string result;
    result.len = strlen(cstr);
    result.data = (char*)GC_malloc(result.len);
    if (result.data == NULL) {
        go_panic((go_string){"out of memory", 13});
    }
    memcpy(result.data, cstr, result.len);
    return result;
}

go_string go_string_from_bytes(const char* data, int64_t len) {
    go_string result;
    result.len = len;
    result.data = (char*)GC_malloc(len);
    if (result.data == NULL) {
        go_panic((go_string){"out of memory", 13});
    }
    memcpy(result.data, data, len);
    return result;
}

go_slice go_string_to_bytes(go_string s) {
    go_slice result;
    result.data = GC_malloc(s.len);
    if (result.data == NULL) {
        go_panic((go_string){"out of memory", 13});
    }
    memcpy(result.data, s.data, s.len);
    result.len = s.len;
    result.cap = s.len;
    return result;
}

// TODO: utf-8
int64_t go_string_decode_rune(const char* data, int64_t len) {
    if (len <= 0 || data == NULL) return ((int64_t)1 << 32) | 0;
    uint8_t c = (uint8_t)data[0];
    return ((int64_t)1 << 32) | (int64_t)c;
}
