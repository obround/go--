#include <gc.h>
#include <string.h>

#include "runtime.h"

// Internal map entry
typedef struct {
    void* key;
    void* value;
    int8_t used;
} map_entry;

// Internal map structure
typedef struct {
    map_entry* entries;
    int64_t capacity;
    int64_t size;
    int64_t key_size;
    int64_t value_size;
    int64_t key_type;  // 0 = normal, 1 = string
} map_internal;

// Internal iterator structure
typedef struct {
    map_internal* m;
    int64_t index;
} map_iter_internal;

// FNV-1a hash
static uint64_t hash_bytes(const void* data, int64_t len) {
    const uint8_t* bytes = (const uint8_t*)data;
    uint64_t hash = 14695981039346656037ULL;
    for (int64_t i = 0; i < len; i++) {
        hash ^= bytes[i];
        hash *= 1099511628211ULL;
    }
    return hash;
}

static uint64_t hash_string_key(go_string* s) {
    return hash_bytes(s->data, s->len);
}
static int keys_equal(map_internal* m, void* a, void* b) {
    if (m->key_type == 1) {
        // String key - compare by content
        go_string* sa = (go_string*)a;
        go_string* sb = (go_string*)b;
        if (sa->len != sb->len) return 0;
        return memcmp(sa->data, sb->data, sa->len) == 0;
    }
    return memcmp(a, b, m->key_size) == 0;
}

static uint64_t hash_key(map_internal* m, void* key) {
    if (m->key_type == 1) {
        return hash_string_key((go_string*)key);
    }
    return hash_bytes(key, m->key_size);
}

// Find slot for key (returns index or -1 if full)
static int64_t map_find_slot(map_internal* m, void* key) {
    uint64_t hash = hash_key(m, key);
    int64_t idx = hash % m->capacity;

    for (int64_t i = 0; i < m->capacity; i++) {
        int64_t probe = (idx + i) % m->capacity;
        if (!m->entries[probe].used) {
            return probe;
        }
        if (keys_equal(m, m->entries[probe].key, key)) {
            return probe;
        }
    }
    return -1;
}

// Grow map capacity and rehash
static void map_grow(map_internal* m) {
    int64_t old_cap = m->capacity;
    map_entry* old_entries = m->entries;

    m->capacity *= 2;
    m->entries = (map_entry*)GC_malloc(m->capacity * sizeof(map_entry));
    if (m->entries == NULL) {
        go_panic((go_string){"out of memory", 13});
    }
    memset(m->entries, 0, m->capacity * sizeof(map_entry));
    m->size = 0;

    // Rehash all entries
    for (int64_t i = 0; i < old_cap; i++) {
        if (old_entries[i].used) {
            int64_t slot = map_find_slot(m, old_entries[i].key);
            m->entries[slot].key = old_entries[i].key;
            m->entries[slot].value = old_entries[i].value;
            m->entries[slot].used = 1;
            m->size++;
        }
    }
}

go_map go_map_make(int64_t key_size, int64_t value_size, int64_t key_type,
                   int64_t hint) {
    map_internal* m = (map_internal*)GC_malloc(sizeof(map_internal));
    if (m == NULL) {
        go_panic((go_string){"out of memory", 13});
    }

    int64_t capacity = hint > 8 ? hint : 8;
    m->entries = (map_entry*)GC_malloc(capacity * sizeof(map_entry));
    if (m->entries == NULL) {
        go_panic((go_string){"out of memory", 13});
    }
    memset(m->entries, 0, capacity * sizeof(map_entry));

    m->capacity = capacity;
    m->size = 0;
    m->key_size = key_size;
    m->value_size = value_size;
    m->key_type = key_type;

    return (go_map)m;
}

void* go_map_get(go_map mp, void* key, int64_t key_size, int64_t value_size) {
    if (mp == NULL) return NULL;

    map_internal* m = (map_internal*)mp;
    (void)key_size;
    (void)value_size;

    uint64_t hash = hash_key(m, key);
    int64_t idx = hash % m->capacity;

    for (int64_t i = 0; i < m->capacity; i++) {
        int64_t probe = (idx + i) % m->capacity;
        if (!m->entries[probe].used) {
            return NULL;
        }
        if (keys_equal(m, m->entries[probe].key, key)) {
            return m->entries[probe].value;
        }
    }
    return NULL;
}

void go_map_set(go_map mp, void* key, void* value, int64_t key_size,
                int64_t value_size) {
    if (mp == NULL) return;

    map_internal* m = (map_internal*)mp;
    (void)key_size;
    (void)value_size;

    // Grow if load factor > 0.75
    if (m->size * 4 >= m->capacity * 3) {
        map_grow(m);
    }

    int64_t slot = map_find_slot(m, key);
    if (slot < 0) {
        go_panic((go_string){"map is full", 11});
    }

    if (!m->entries[slot].used) {
        m->entries[slot].key = GC_malloc(m->key_size);
        if (m->entries[slot].key == NULL) {
            go_panic((go_string){"out of memory", 13});
        }
        memcpy(m->entries[slot].key, key, m->key_size);
        m->size++;
    }

    if (m->entries[slot].value == NULL) {
        m->entries[slot].value = GC_malloc(m->value_size);
        if (m->entries[slot].value == NULL) {
            go_panic((go_string){"out of memory", 13});
        }
    }
    memcpy(m->entries[slot].value, value, m->value_size);
    m->entries[slot].used = 1;
}

void go_map_delete(go_map mp, void* key, int64_t key_size) {
    if (mp == NULL) return;

    map_internal* m = (map_internal*)mp;
    (void)key_size;

    uint64_t hash = hash_key(m, key);
    int64_t idx = hash % m->capacity;

    for (int64_t i = 0; i < m->capacity; i++) {
        int64_t probe = (idx + i) % m->capacity;
        if (!m->entries[probe].used) {
            return;
        }
        if (keys_equal(m, m->entries[probe].key, key)) {
            m->entries[probe].used = 0;
            m->size--;
            return;
        }
    }
}

int64_t go_map_len(go_map mp) {
    if (mp == NULL) return 0;
    map_internal* m = (map_internal*)mp;
    return m->size;
}

go_map_iter go_map_iter_init(go_map mp) {
    if (mp == NULL) return NULL;

    map_iter_internal* iter =
        (map_iter_internal*)GC_malloc(sizeof(map_iter_internal));
    if (iter == NULL) {
        go_panic((go_string){"out of memory", 13});
    }
    iter->m = (map_internal*)mp;
    iter->index = -1;
    return (go_map_iter)iter;
}

int32_t go_map_iter_next(go_map_iter it) {
    if (it == NULL) return 0;

    map_iter_internal* iter = (map_iter_internal*)it;
    iter->index++;

    while (iter->index < iter->m->capacity) {
        if (iter->m->entries[iter->index].used) {
            return 1;
        }
        iter->index++;
    }
    return 0;
}

void* go_map_iter_key(go_map_iter it) {
    if (it == NULL) return NULL;
    map_iter_internal* iter = (map_iter_internal*)it;
    if (iter->index < 0 || iter->index >= iter->m->capacity) return NULL;
    return iter->m->entries[iter->index].key;
}

void* go_map_iter_value(go_map_iter it) {
    if (it == NULL) return NULL;
    map_iter_internal* iter = (map_iter_internal*)it;
    if (iter->index < 0 || iter->index >= iter->m->capacity) return NULL;
    return iter->m->entries[iter->index].value;
}
