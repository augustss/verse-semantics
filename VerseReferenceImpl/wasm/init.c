// Copyright Epic Games, Inc. All Rights Reserved.
#include "Main_stub.h"

#include <Rts.h>

__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
    // Use nonmoving garbage collection to optimize for pause time rather than
    // throughput.  A 64MB initial heap size is arbitrary, and won't affect the
    // size of the wizer output due to use of `rts_clearMemory`.
    char *args[] = {
        "versewasm.wasm", "+RTS", "--nonmoving-gc", "-H64m", "-RTS", NULL
    };
    int argc = sizeof(args) / sizeof(args[0]) - 1;
    char **argv = args;
    hs_init_with_rtsopts(&argc, &argv);
    // Invoke nonmoving garbage collection twice to ensure unused segments are
    // freed.
    hs_perform_gc();
    hs_perform_gc();
    // Initialize unused memory to zero, ensuring it is not snapshotted by
    // wizer, which would bloat the size of the WASM significantly.
    rts_clearMemory();
}
