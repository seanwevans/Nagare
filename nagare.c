#include <stdbool.h>
#include <stddef.h>

typedef struct point {
    float x;
    float y;
} point;

typedef float (*func_ptr)(float);  
typedef bool (*boundary_func)(point*);
typedef struct function { func_ptr f; } function;

typedef struct field2 {
    function x;
    function y;
} field2;

typedef struct field3 {
    function x;
    function y;
    function z;
} field3;

typedef union field {
    field2 f2;
    field3 f3;
} field;

typedef struct initInfo {
    float T;          // initial time
    float max_T;      // end time
    float delta;      // time step
    point pos;        // default starting position
    field initial;    // default background field  
    field program;    // default running program
} initInfo;

typedef struct executionZone {
    boundary_func boundary;  // defines the execution zone boundary. True==inside
    char** instructions;     // list of instructions for zone
} executionZone;

float f_1(float x) { return x + 1; }
float f_2(float x) { return x * x + 1; }
float f_3(float x) { return x - 1; }
float f_4(float x) { return 3*x - 1; }

bool inside_circle(point* pt) { return pt->x * pt->x + pt->y * pt->y <= 1; }

int main() {
    initInfo init = { 
        .T = 0,
        .max_T = 1,
        .delta = 1e-5,
        .pos = { .x = 0, .y = 0 },
        .initial = { .f2 = { { f_1 }, { f_2 } } },
        .program = { .f2 = { { f_3 }, { f_4 } } }
    };

    executionZone ez1 = {
        .boundary = inside_circle,
        .instructions = (char*[]) {
            "thread_data_t* data = (thread_data_t*)arg;",
            "const uint8_t *grid = data->grid;",
            "const uint8_t *temp_grid = data->temp_grid;",
            "const uint64_t grid_width = data->grid_width;",
            "const uint64_t grid_height = data->grid_height;",
            "const uint64_t start_row = data->start_row;",
            "const uint64_t end_row = data->end_row;",
            "uint8_t live_neighbors;",
            "int neighbor_x;",
            "int neighbor_y;",
            "uint8_t current_cell;",
            "uint64_t idx;",            
            "for (uint64_t row = start_row; row < end_row; row++ ) {",                
            "    for (uint64_t column = 0; column < grid_width; column++) {",                    
            "        live_neighbors = 0;",
            "        for (int i = -1; i <= 1; i++) {",               
            "            for (int j = -1; j <= 1; j++) {",                        
            "                if (!(i == 0 && j == 0)) {",
            "                    neighbor_x = (column + i + grid_width) % grid_width;",
            "                    neighbor_y = (row + j + grid_height) % grid_height;",
            "                    live_neighbors += grid[neighbor_y * grid_width + neighbor_x];",
            "                }",                            
            "            }",
            "        }",            
            "        idx = row * grid_width + column;",
            "        current_cell = grid[idx];",
            "        temp_grid[idx] = current_cell;",
            "        if (current_cell == 1 && (live_neighbors < 2 || live_neighbors > 3)) {",
            "            temp_grid[idx] = 0;",
            "        }",
            "        if (current_cell == 0 && live_neighbors == 3) {",
            "            temp_grid[idx] = 1;",
            "        }",
            "    }",
            "}",
            "pthread_exit(NULL);",
            NULL
        }
    };

    return 0;
}
