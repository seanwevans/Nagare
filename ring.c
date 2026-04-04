#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <pthread.h>
#include <unistd.h>
#include <stdatomic.h>

#define WRITER_ITERS 25
#define READER_ITERS 25


typedef struct RingBuffer {
    size_t idx;
    size_t capacity;
    void** buffer;
    size_t elem_size;
    pthread_mutex_t lock;
    atomic_int shutdown_requested;
} RingBuffer;

static size_t slot_allocations = 0;
static atomic_size_t slot_allocation_attempts = 0;

typedef enum DataType {
    INT,
    FLOAT,
    STRING,
} DataType;

int create_buffer(RingBuffer* cache, size_t buffer_size, size_t elem_size) {
    if (!cache) {
        fprintf(stderr, "Invalid cache pointer.\n");
        return -1;
    }

    cache->idx = 0;
    cache->capacity = 0;
    cache->buffer = NULL;
    cache->elem_size = elem_size;
    atomic_store(&cache->shutdown_requested, 0);
    atomic_store(&slot_allocation_attempts, 0);

    if (buffer_size <= 1) {
        fprintf(stderr, "Buffer size should be greater than 1.\n");
        return -1;
    }

    if (elem_size == 0) {
        fprintf(stderr, "Element size must be greater than 0.\n");
        return -1;
    }

    cache->capacity = buffer_size;
    cache->buffer = (void**) calloc(buffer_size, sizeof(void*));
    if (!cache->buffer) {
        fprintf(stderr, "Failed to allocate buffer.\n");
        cache->capacity = 0;
        return -1;
    }

    if (pthread_mutex_init(&cache->lock, NULL) != 0) {
        fprintf(stderr, "Failed to initialize mutex.\n");
        free(cache->buffer);
        cache->buffer = NULL;
        cache->capacity = 0;
        return -1;
    }

    return 0;
}

void print_buffer(RingBuffer* cache, DataType type) {
    pthread_mutex_lock(&cache->lock); // Critical section: iterate safely over shared buffer
    for (size_t i = 0; i < cache->capacity; ++i) {
        if (cache->buffer[i]) {
            switch (type) {
                case INT:
                    printf("%d ", *(int*)cache->buffer[i]);
                    break;
                case FLOAT:
                    printf("%f ", *(float*)cache->buffer[i]);
                    break;
                case STRING:
                    printf("%s ", (char*)cache->buffer[i]);
                    break;
                default:
                    printf("? ");
                    break;
            }
        } else {
            printf("NULL ");
        }
    }
    pthread_mutex_unlock(&cache->lock);
    printf("\n");
}


int add_to_buffer(RingBuffer* cache, const void* value) {
    pthread_mutex_lock(&cache->lock);

    void* slot = cache->buffer[cache->idx];

    if (slot == NULL) {
        size_t allocation_attempt = atomic_fetch_add(&slot_allocation_attempts, 1) + 1;
#ifdef RING_TEST_ALLOC_FAIL_AT
        if (allocation_attempt == (size_t)RING_TEST_ALLOC_FAIL_AT) {
            pthread_mutex_unlock(&cache->lock);
            fprintf(stderr, "Injected slot allocation failure at attempt %zu.\n", allocation_attempt);
            atomic_store(&cache->shutdown_requested, 1);
            return -1;
        }
#endif
        slot = malloc(cache->elem_size);
        if (!slot) {
            pthread_mutex_unlock(&cache->lock);
            fprintf(stderr, "Failed to allocate ring buffer slot.\n");
            atomic_store(&cache->shutdown_requested, 1);
            return -1;
        }
        cache->buffer[cache->idx] = slot;
        ++slot_allocations;
    }

    memcpy(slot, value, cache->elem_size);
    cache->idx = ( cache->idx + 1 ) % cache->capacity;

    pthread_mutex_unlock(&cache->lock);
    return 0;
}

void destroy_buffer(RingBuffer* cache) {
    if (!cache || !cache->buffer) {
        return;
    }
    for (size_t i = 0; i < cache->capacity; ++i) {
        if (cache->buffer[i]) {
            free(cache->buffer[i]);
            --slot_allocations;
        }
    }
    pthread_mutex_destroy(&cache->lock);
    free(cache->buffer);
    cache->buffer = NULL;
    cache->capacity = 0;
    cache->idx = 0;
}

void* writer(void* arg);
void* reader(void* arg);

int main() {
    RingBuffer cache;
    if (create_buffer(&cache, 10, sizeof(int)) != 0) {
        fprintf(stderr, "Failed to create ring buffer.\n");
        return EXIT_FAILURE;
    }

    pthread_t w1, w2, r;
    pthread_create(&w1, NULL, writer, &cache);
    pthread_create(&w2, NULL, writer, &cache);
    pthread_create(&r, NULL, reader, &cache);

    pthread_join(w1, NULL);
    pthread_join(w2, NULL);
    pthread_join(r, NULL);

    int should_fail = atomic_load(&cache.shutdown_requested);
    destroy_buffer(&cache);
    assert(slot_allocations == 0);

    return should_fail ? EXIT_FAILURE : EXIT_SUCCESS;
}

void* writer(void* arg) {
    RingBuffer* c = (RingBuffer*)arg;
    for (int i = 0; i < WRITER_ITERS; ++i) {
        if (atomic_load(&c->shutdown_requested)) {
            break;
        }
        int value = i;
        if (add_to_buffer(c, &value) != 0) {
            fprintf(stderr, "Writer thread stopping due to add_to_buffer failure.\n");
            atomic_store(&c->shutdown_requested, 1);
            break;
        }
        usleep(1000);
    }
    return NULL;
}

void* reader(void* arg) {
    RingBuffer* c = (RingBuffer*)arg;
    for (int i = 0; i < READER_ITERS; ++i) {
        if (atomic_load(&c->shutdown_requested)) {
            break;
        }
        print_buffer(c, INT);
        usleep(1000);
    }
    return NULL;
}
