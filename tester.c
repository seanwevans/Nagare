#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <pthread.h>
#include <string.h>

#define MAX_ITERATIONS 1000000000
#define MAX_THREADS    3
#define BUFFER_SIZE    1<<8
#define CIRCLE_ZONE_X  0
#define CIRCLE_ZONE_Y  0
#define CIRCLE_RAD_SQ  1

typedef struct {
    char filepath[BUFFER_SIZE];
    float x0, y0;    
    int result;
} ThreadData;


float df_1(const float* x) { 
    return (1+*x)*sin(1+*x) + tan(*x); 
}

float df_2(const float* x) { 
    return (1 + *x)*cos(1 + *x) + tan(1 + (*x) * (*x)); 
}

bool inside_circle(const float* x, const float* y) { 
    return ((*x-CIRCLE_ZONE_X)*(*x-CIRCLE_ZONE_X) + (*y-CIRCLE_ZONE_Y)*(*y-CIRCLE_ZONE_Y)) <= CIRCLE_RAD_SQ;
}

int load_data(const char* filepath, ThreadData** out_data) {
    FILE* file = fopen(filepath, "r");
    if (!file) {
        perror("Error opening file");
        return 0;
    }

    ThreadData* data = calloc(MAX_THREADS, sizeof(ThreadData));
    if (!data) {
        perror("Error allocating memory");
        fclose(file);
        return 0;
    }

    char line[BUFFER_SIZE];
    int index = 0;
    while (fgets(line, sizeof(line), file) && index < MAX_THREADS) {
        if (sscanf(line, "%f %f", &data[index].x0, &data[index].y0) == 2) {
            snprintf(data[index].filepath, sizeof(data[index].filepath),
                     "%f_%f.txt", data[index].x0, data[index].y0);
            index++;
        }
    }

    fclose(file);

    *out_data = data;
    return index;
}

void* simulate(void* arg) {
    ThreadData* data = (ThreadData*) arg;
    
    FILE *file = fopen(data->filepath, "a");
    if (file == NULL)
        return NULL;    
        
    float x = data->x0, y = data->y0;

    for(int i = 0; (i < MAX_ITERATIONS) && (!isinf(x)) && (!isinf(y)); ++i) {
        fprintf(file, "%d %d %.8f %.8f\n", i+1, inside_circle(&x, &y), x, y);       
        x += df_1(&x);
        y += df_2(&y);
    }

    fclose(file);

    return NULL;
}


int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "usage  tester  input.file\n");
        return EXIT_FAILURE;
    }

    // dummy data for reading and parsing input.file
    // ThreadData thread_data[MAX_THREADS] = {
        // {"0.0_0.0.txt", 0.0, 0.0},
        // {"0.5_0.5.txt", 0.5, 0.5},
        // {"-0.5_-0.5.txt", -0.5, -0.5}
    // };

    ThreadData* thread_data = NULL;
    int thread_count = load_data(argv[1], &thread_data);

    pthread_t threads[MAX_THREADS];
    for (int i = 0; i < thread_count; i++) {
        int rc = pthread_create(&threads[i], NULL, simulate, &thread_data[i]);
        if (rc != 0) {
            fprintf(stderr, "Error creating thread %d: %s\n", i, strerror(rc));
        }
    }

    for (int i = 0; i < thread_count; i++) {
        int rc = pthread_join(threads[i], NULL);
        if (rc != 0) {
            fprintf(stderr, "Error joining thread %d: %s\n", i, strerror(rc));
        }
    }

    free(thread_data);

    return 0;
}
