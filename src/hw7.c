#include "hw7.h"
#include <ctype.h>

// first, im making a helper here to check if the matrix is temporary or not
int is_temp(char name) {
    return !isalpha(name);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if (mat1 == NULL || mat2 == NULL) return NULL;
    
    matrix_sf *result = malloc(sizeof(matrix_sf) + mat1->num_rows * mat1->num_cols * sizeof(int));
    if (result == NULL) return NULL;
    
    result->name = '?';
    result->num_rows = mat1->num_rows;
    result->num_cols = mat1->num_cols;
    
    for (unsigned int i = 0; i < mat1->num_rows * mat1->num_cols; i++) {
        result->values[i] = mat1->values[i] + mat2->values[i];
    }
    
    return result;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if (mat1 == NULL || mat2 == NULL) return NULL;
    
    unsigned int m = mat1->num_rows;
    unsigned int n = mat1->num_cols;
    unsigned int p = mat2->num_cols;
    
    matrix_sf *result = malloc(sizeof(matrix_sf) + m * p * sizeof(int));
    if (result == NULL) return NULL;
    
    result->name = '?';
    result->num_rows = m;
    result->num_cols = p;
    
    // we do the multiplication
    for (unsigned int i = 0; i < m; i++) {
        for (unsigned int j = 0; j < p; j++) {
            int sum = 0;
            for (unsigned int k = 0; k < n; k++) {
                // row-major: mat1[i][k] * mat2[k][j]
                sum += mat1->values[i * n + k] * mat2->values[k * p + j];
            }
            result->values[i * p + j] = sum;
        }
    }
    
    return result;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    if (mat == NULL) return NULL;
    
    unsigned int rows = mat->num_rows;
    unsigned int cols = mat->num_cols;
    
    matrix_sf *result = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    if (result == NULL) return NULL;
    
    result->name = '?';
    // swap rows and columns for transpose
    result->num_rows = cols;
    result->num_cols = rows;
    
    for (unsigned int i = 0; i < rows; i++) {
        for (unsigned int j = 0; j < cols; j++) {
            result->values[j * rows + i] = mat->values[i * cols + j];
        }
    }
    
    return result;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    if (expr == NULL) return NULL;
    
    const char *p = expr;
    
    // parse the dimensions firstt
    while (*p == ' ') p++;
    unsigned int num_rows = 0;
    while (*p >= '0' && *p <= '9') {
        num_rows = num_rows * 10 + (*p - '0');
        p++;
    }
    
    while (*p == ' ') p++;
    unsigned int num_cols = 0;
    while (*p >= '0' && *p <= '9') {
        num_cols = num_cols * 10 + (*p - '0');
        p++;
    }
    
    while (*p && *p != '[') p++;
    if (*p == '[') p++;
    
    matrix_sf *mat = malloc(sizeof(matrix_sf) + num_rows * num_cols * sizeof(int));
    if (mat == NULL) return NULL;
    
    mat->name = name;
    mat->num_rows = num_rows;
    mat->num_cols = num_cols;
    
    unsigned int idx = 0;
    while (*p && *p != ']' && idx < num_rows * num_cols) {
        while (*p == ' ' || *p == ';') p++;
        if (*p == ']') break;
        
        // to deal with negative numbers
        int value = 0;
        int negative = 0;
        if (*p == '-') {
            negative = 1;
            p++;
        }
        while (*p >= '0' && *p <= '9') {
            value = value * 10 + (*p - '0');
            p++;
        }
        if (negative) value = -value;
        
        mat->values[idx++] = value;
    }
    
    return mat;
}

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (mat == NULL) return root;
    
    // create a new node if we hit NULL
    if (root == NULL) {
        bst_sf *node = malloc(sizeof(bst_sf));
        if (node == NULL) return NULL;
        node->mat = mat;
        node->left_child = NULL;
        node->right_child = NULL;
        return node;
    }
    
    // we insert based on alphabetical order
    if (mat->name < root->mat->name) {
        root->left_child = insert_bst_sf(mat, root->left_child);
    } else {
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (root == NULL) return NULL;
    
    if (name == root->mat->name) {
        return root->mat;
    } else if (name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    } else {
        return find_bst_sf(name, root->right_child);
    }
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) return;
    
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root->mat);
    free(root);
}

int precedence(char op) {
    // higher number here means higher precedence
    if (op == '\'') return 3;
    if (op == '*') return 2;
    if (op == '+') return 1;
    return 0;
}

char* infix2postfix_sf(char *infix) {
    if (infix == NULL) return NULL;
    
    int len = strlen(infix);
    char *postfix = malloc(len * 2 + 1);
    if (postfix == NULL) return NULL;
    
    // use stack to convert infix to postfix
    char stack[MAX_LINE_LEN];
    int top = -1;
    int pos = 0;
    
    for (int i = 0; i < len; i++) {
        char c = infix[i];
        
        if (c == ' ') continue;
        
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
            postfix[pos++] = c;
        }
        else if (c == '(') {
            stack[++top] = c;
        }
        else if (c == ')') {
            while (top >= 0 && stack[top] != '(') {
                postfix[pos++] = stack[top--];
            }
            if (top >= 0) top--;
        }
        else if (c == '+' || c == '*' || c == '\'') {
            while (top >= 0 && stack[top] != '(' && 
                   precedence(stack[top]) >= precedence(c)) {
                postfix[pos++] = stack[top--];
            }
            stack[++top] = c;
        }
    }
    
    while (top >= 0) {
        postfix[pos++] = stack[top--];
    }
    
    postfix[pos] = '\0';
    return postfix;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    if (expr == NULL || root == NULL) return NULL;
    
    char *postfix = infix2postfix_sf(expr);
    if (postfix == NULL) return NULL;
    
    // evaluate postfix using a matrix stack
    matrix_sf *stack[MAX_LINE_LEN];
    int top = -1;
    
    int len = strlen(postfix);
    for (int i = 0; i < len; i++) {
        char c = postfix[i];
        
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
            matrix_sf *mat = find_bst_sf(c, root);
            if (mat != NULL) {
                stack[++top] = mat;
            }
        }
        else if (c == '\'') {
            if (top >= 0) {
                matrix_sf *mat = stack[top--];
                matrix_sf *result = transpose_mat_sf(mat);
                result->name = '!';
                stack[++top] = result;
                
                // we are only freeing temporary matrices
                if (is_temp(mat->name)) {
                    free(mat);
                }
            }
        }
        else if (c == '*') {
            if (top >= 1) {
                matrix_sf *mat2 = stack[top--];
                matrix_sf *mat1 = stack[top--];
                matrix_sf *result = mult_mats_sf(mat1, mat2);
                result->name = '@';
                stack[++top] = result;
                
                if (is_temp(mat1->name)) {
                    free(mat1);
                }
                if (is_temp(mat2->name)) {
                    free(mat2);
                }
            }
        }
        else if (c == '+') {
            if (top >= 1) {
                matrix_sf *mat2 = stack[top--];
                matrix_sf *mat1 = stack[top--];
                matrix_sf *result = add_mats_sf(mat1, mat2);
                result->name = '!';
                stack[++top] = result;
                
                if (is_temp(mat1->name)) {
                    free(mat1);
                }
                if (is_temp(mat2->name)) {
                    free(mat2);
                }
            }
        }
    }
    
    free(postfix);
    
    if (top >= 0) {
        matrix_sf *result = stack[top];
        result->name = name;
        return result;
    }
    
    return NULL;
}

matrix_sf *execute_script_sf(char *filename) {
    if (filename == NULL) return NULL;
    
    FILE *file = fopen(filename, "r");
    if (file == NULL) return NULL;
    
    bst_sf *root = NULL;
    matrix_sf *last_matrix = NULL;
    char *line = NULL;
    size_t max_line_size = MAX_LINE_LEN;
    
    // to process each line of the script
    while (getline(&line, &max_line_size, file) != -1) {
        char *p = line;
        
        while (*p == ' ') p++;
        
        char mat_name = *p;
        p++;
        
        while (*p == ' ' || *p == '=') p++;
        
        // to check if the line is matrix definition or formula
        if (*p >= '0' && *p <= '9') {
            matrix_sf *mat = create_matrix_sf(mat_name, p);
            if (mat != NULL) {
                root = insert_bst_sf(mat, root);
                last_matrix = mat;
            }
        } else {
            char expr[MAX_LINE_LEN];
            int idx = 0;
            while (*p && *p != '\n') {
                expr[idx++] = *p;
                p++;
            }
            expr[idx] = '\0';
            
            matrix_sf *mat = evaluate_expr_sf(mat_name, expr, root);
            if (mat != NULL) {
                root = insert_bst_sf(mat, root);
                last_matrix = mat;
            }
        }
    }
    
    free(line);
    fclose(file);
    
    return last_matrix;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
