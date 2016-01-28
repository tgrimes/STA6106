import math
import numpy as np
    
def sim(func, seed, n = 10**6):
    np.random.seed(seed)
    
    rep = 1
    if n > 10**8:
        rep = n/10**7
        n = 10**7
        
    sum = 0
    for i in range(rep):
        y = np.random.uniform(0, 1, n)         
        sum = sum + np.sum(g(y))/n
        
    return sum/(rep)

def g(x):
    x = np.array(x, dtype = np.float64)
    return (1 - x)*x / (1 - 2*x + 2*x*x)**2

result = sim(g, seed = 10, n = 10**9)
print "result = %s" % result

#Run command:
#   time python <filename.py>