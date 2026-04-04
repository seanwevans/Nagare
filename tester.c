#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <pthread.h>
#include <string.h>
#include <errno.h>

#define MAX_ITERATIONS 1000000000
#define MAX_THREADS    3
#define BUFFER_SIZE    1<<8
#define CIRCLE_ZONE_X  0
#define CIRCLE_ZONE_Y  0
#define CIRCLE_RAD_SQ  1

typedef struct {
    char filepath[BUFFER_SIZE];
    float x0, y0;    
    bool append_output;
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
    if (out_data == NULL) {
        return -EINVAL;
    }

    *out_data = NULL;

    FILE* file = fopen(filepath, "r");
    if (!file) {
        return -errno;
    }

    ThreadData* data = calloc(MAX_THREADS, sizeof(ThreadData));
    if (!data) {
        int err = errno ? errno : ENOMEM;
        fclose(file);
        return -err;
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

    if (fclose(file) != 0) {
        int err = errno ? errno : EIO;
        free(data);
        return -err;
    }

    *out_data = data;
    return index;
}

void* simulate(void* arg) {
    ThreadData* data = (ThreadData*) arg;

    const char* file_mode = data->append_output ? "a" : "w";
    FILE *file = fopen(data->filepath, file_mode);
    if (file == NULL)
        return NULL;    
        
    float x = data->x0, y = data->y0;

    // Stop immediately once the simulated values leave the finite domain so
    // NaNs and infinities do not generate unbounded output.
    for (int i = 0; (i < MAX_ITERATIONS) && isfinite(x) && isfinite(y); ++i) {
        fprintf(file, "%d %d %.8f %.8f\n", i+1, inside_circle(&x, &y), x, y);       
        x += df_1(&x);
        y += df_2(&y);
    }

    fclose(file);

    return NULL;
}


int main(int argc, char* argv[]) {
    bool append_output = false;
    const char* input_filepath = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--append") == 0) {
            append_output = true;
            continue;
        }

        if (input_filepath == NULL) {
            input_filepath = argv[i];
            continue;
        }

        fprintf(stderr, "usage: tester [--append] input.file\n");
        return EXIT_FAILURE;
    }

    if (input_filepath == NULL) {
        fprintf(stderr, "usage: tester [--append] input.file\n");
        return EXIT_FAILURE;
    }

    // dummy data for reading and parsing input.file
    // ThreadData thread_data[MAX_THREADS] = {
        // {"0.0_0.0.txt", 0.0, 0.0},
        // {"0.5_0.5.txt", 0.5, 0.5},
        // {"-0.5_-0.5.txt", -0.5, -0.5}
    // };

    ThreadData* thread_data = NULL;
    int thread_count = load_data(input_filepath, &thread_data);
    if (thread_count < 0) {
        fprintf(stderr, "Failed to load thread data from '%s': %s\n", input_filepath,
                strerror(-thread_count));
        return EXIT_FAILURE;
    }

    for (int i = 0; i < thread_count; i++) {
        thread_data[i].append_output = append_output;
    }

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
