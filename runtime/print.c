#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "runtime.h"

void go_print_int(int64_t v) { printf("%lld", (long long)v); }

void go_print_uint(int64_t v) { printf("%llu", (unsigned long long)v); }

void go_print_float(double v) { printf("%e", v); }

void go_print_bool(int8_t v) { printf("%s", v ? "true" : "false"); }

void go_print_string(go_string s) {
    if (s.data != NULL && s.len > 0) {
        fwrite(s.data, 1, s.len, stdout);
    }
}

void go_print_pointer(void* p) { printf("%p", p); }

void go_print_space(void) { printf(" "); }

void go_print_newline(void) { printf("\n"); }
