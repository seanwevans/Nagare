#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>


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

int main() {
    RingBuffer cache = create_buffer(10);

    for (size_t i = 0; i < cache.capacity; ++i) {
        int* value = (int*)malloc(sizeof(int));
        *value = (int)(i * i);
        add_to_buffer(&cache, value);
    }

    printf("\n");
    
    destroy_buffer(&cache);

    return 0;
}
