#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"

void go_panic(go_string msg) {
    fprintf(stderr, "panic: ");
    if (msg.data != NULL && msg.len > 0) {
        fwrite(msg.data, 1, msg.len, stderr);
    }
    fprintf(stderr, "\n");
    exit(1);
}

void go_panic_bounds(int64_t index, int64_t len) {
    fprintf(
        stderr,
        "panic: runtime error: index out of range [%lld] with length %lld\n",
        (long long)index, (long long)len);
    exit(1);
}

void go_panic_nil(void) {
    fprintf(stderr,
            "panic: runtime error: invalid memory address or nil pointer "
            "dereference\n");
    exit(1);
}
