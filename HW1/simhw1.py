import math
import random as ran

def g(x):
    return (1 - x)*x / (1 - 2*x + 2*x*x)**2
    
def sim(g, seed, n = 10**6):
    ran.seed(seed)
    sum = 0
    
    for i in xrange(n):
        y = ran.random()
        sum = sum + g(y)
        
    return sum/n

result = sim(g, seed = 10, n = 10**9)
print "result = %s" % result

#Run command:
#   time python <filename.py>