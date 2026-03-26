#ifndef MUMPS_WRAPPER_H
#define MUMPS_WRAPPER_H

/* Opaque handle to a MUMPS solver instance */
typedef struct MumpsSolver MumpsSolver;

/* Create a solver for an n x n system with nnz non-zeros.
 * irn, jcn are 0-indexed row/col arrays; the wrapper converts to 1-indexed.
 * The arrays are copied internally — caller retains ownership. */
MumpsSolver* mumps_create(int n, int nnz, const int* irn, const int* jcn, const double* a);

/* Run symbolic analysis (ordering). Returns 0 on success, MUMPS error code on failure. */
int mumps_analyze(MumpsSolver* s);

/* Run numerical factorization. Returns 0 on success. */
int mumps_factorize(MumpsSolver* s);

/* Solve with a given RHS vector (length n). Solution is written to sol (length n).
 * Returns 0 on success. Can be called repeatedly after factorize. */
int mumps_solve(MumpsSolver* s, const double* rhs, double* sol);

/* Deallocate all MUMPS-internal and wrapper memory. */
void mumps_destroy(MumpsSolver* s);

/* Return the MUMPS global error code (infog[0]) from the last call. */
int mumps_get_error(MumpsSolver* s);

#endif /* MUMPS_WRAPPER_H */
