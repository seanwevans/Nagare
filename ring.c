#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

#define WRITER_ITERS 25
#define READER_ITERS 25


typedef struct RingBuffer {
    size_t idx;
    size_t capacity;
    void** buffer;
    pthread_mutex_t lock;
} RingBuffer;

typedef enum DataType {
    INT,
    FLOAT,
    STRING,
} DataType;


RingBuffer create_buffer(size_t buffer_size) {
    if (buffer_size <= 1) {
        fprintf(stderr, "Buffer size should be greater than 1.\n");
        exit(EXIT_FAILURE);
    }

    RingBuffer cache = {
        .idx = 0,
        .capacity = buffer_size,
        .buffer = (void**) calloc(buffer_size, sizeof(void*))
    };

    pthread_mutex_init(&cache.lock, NULL);

    return cache;
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


void add_to_buffer(RingBuffer* cache, void* value) {
    pthread_mutex_lock(&cache->lock);

    cache->buffer[cache->idx] = value;
    cache->idx = ( cache->idx + 1 ) % cache->capacity;

    pthread_mutex_unlock(&cache->lock);
}

void destroy_buffer(RingBuffer* cache) {
    for (size_t i = 0; i < cache->capacity; ++i) {
        if (cache->buffer[i]) {
            free(cache->buffer[i]);
        }
    }
    pthread_mutex_destroy(&cache->lock);
    free(cache->buffer);
}

void* writer(void* arg);
void* reader(void* arg);

int main() {
    RingBuffer cache = create_buffer(10);

    pthread_t w1, w2, r;
    pthread_create(&w1, NULL, writer, &cache);
    pthread_create(&w2, NULL, writer, &cache);
    pthread_create(&r, NULL, reader, &cache);

    pthread_join(w1, NULL);
    pthread_join(w2, NULL);
    pthread_join(r, NULL);

    destroy_buffer(&cache);

    return 0;
}

void* writer(void* arg) {
    RingBuffer* c = (RingBuffer*)arg;
    for (int i = 0; i < WRITER_ITERS; ++i) {
        int* value = (int*)malloc(sizeof(int));
        *value = i;
        add_to_buffer(c, value);
        usleep(1000);
    }
    return NULL;
}

void* reader(void* arg) {
    RingBuffer* c = (RingBuffer*)arg;
    for (int i = 0; i < READER_ITERS; ++i) {
        print_buffer(c, INT);
        usleep(1000);
    }
    return NULL;
}
