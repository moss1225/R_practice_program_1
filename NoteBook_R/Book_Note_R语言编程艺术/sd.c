#include<stdio.h>
// arguments
//  m: a square matrix
//  n: number of rows/columns of m
//  k: the subdiagonal index--0 for main diagonal . 1 for first
//     subdiagonal, 2 for second ,etc.
//  result: space for the requested subdiagonal ,return here

void subdiag(double *m , int *n , int *k , double *result)
{
	int nval = *n,kval = *k;
	int stride = nval + 1;
	for(int i = 0 , j = kval; i < nval - kval;i++ , j += stride)
		result[i] = m[j];
}