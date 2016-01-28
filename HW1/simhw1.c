//
//  main.c
//  HW 1
//
//  Created by Tyler Grimes on 1/14/16.
//  Copyright © 2016 Tyler Grimes. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

double rand_unif();
double g(double x);
double simulation(unsigned int seed, int n);

int main(int argc, const char * argv[]) {
    
    unsigned int seed = 2;
    int n = 1000000000;
    
    double ans = simulation(seed, n);

    printf("Result = %lf\n", ans);
    
    return 0;
}

double rand_unif() {
    return rand()/(double)RAND_MAX;
}

double g(double x) {
    return ((1 - x)*(x)) / pow((1 - 2*x + 2*x*x), 2);
}

double simulation(unsigned int seed, int n) {
    srand(seed);
    
    double sum = 0;
    int i;
    for (i = 0; i < n; i++) {
        sum = sum + g(rand_unif());
    }
    
    return sum/n;   
}

