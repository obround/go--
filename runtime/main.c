#include <gc.h>

#include "runtime.h"

extern void go_main(void);

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;
    GC_init();
    go_main();
    return 0;
}
