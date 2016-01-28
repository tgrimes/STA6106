#include <stdio.h>
#include <math.h>
#include <stdlib.h>

double rand_unif();
double simulation(int seed, int n);

int main(int argc, const char * argv[]) {
	int seed, n;
	if(argc > 1) {
		seed = atoi(argv[1]);
	}
	else {
		seed = 10;
	}
	
	if(argc > 2) {
		n = atoi(argv[2]);
	}
	else {
		n = 100000;
	}
	
	double ans = simulation(seed, n);
	
	printf("Result = %.6lf; seed = %d, n = %d\n", ans, seed, n); 
}

double rand_unif() {
	return rand()/(double) RAND_MAX;
}

double simulation(int seed, int n) {
	srand(seed);
	
	double sum1 = 0, sum2 = 0, sum3 = 0, y;
	int i;
	
	for(i = 0; i < n; i++) {
		y = rand_unif();
		sum1 = sum1 + y;
		sum2 = sum2 + exp(y);
		sum3 = sum3 + y*exp(y);
	}
	
	return (sum3 - sum1*sum2/(n))/(n);
}