//
//  main.c
//  Project 1 - less100k
//
//  Created by Tyler Grimes on 1/31/16.
//
//  Code for binary tree obtained from:
//  http://www.thelearningpoint.net/computer-science/trees/binary-search-trees---c-program-source-code-and-documentation
//

#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<time.h>



int nextRandom(int xn) {
    int a = (int) pow(7, 5);
    int c = (int) 0;
    int m = (int) pow(2, 31) - 1;
    return (int)(((long)a * (long)xn + c) % m);
}


int main(int argc, const char * argv[]) {
    
    srand(time(NULL));
    
    int reps = pow(10,6);
    int iterations[reps];
    
    int k;
    for (k = 0; k < reps; k++) {
        int seed = rand();
        int xn = seed;
        
        int LESS_100K = 0;
        int i = 0;
        while (!LESS_100K) {
            i++;
            xn = nextRandom(xn);
            if (xn <= 100000) {
                LESS_100K = 1;
                break;
            }
        }
        
        iterations[k] = i;
    }
    
    int temp = 0;
    for (k = 0; k < reps; k++) {
        if (temp < iterations[k]) {
            temp = iterations[k];
        }
    }
    
    printf("Max iterations = %d\n", temp);
    
    return 0;
}


