"""
A class that can be used for AD of order one.
"""

from __future__ import print_function
# This must be the first statement before other statements.
# You may only put a quoted or triple quoted string, 
# Python comments, other future statements, or blank lines before the __future__ line.

try:
    import __builtin__
except ImportError:
    # Python 3
    import builtins as __builtin__
import numpy as np

class valder:
    val = 0.0
    der = None
    
    def __init__(self, a, b=None):
        if b is None:
            self.val = a.val
            self.der = a.der
        elif isinstance(b, list):
            self.val = a
            self.der = np.asarray(b)
        elif isinstance(b, np.ndarray):
            self.val = a
            self.der = b
        elif isinstance(a, valder):
            # for higher derivatives
            self.val = a 
            self.der = b
        else:
            self.val = a
            self.der = np.asarray([b])

    def __add__(self, o):
        if isinstance(o, valder):
            return valder(self.val+o.val, self.der+o.der)
        else:
            return valder(self.val+o, self.der)
        
    def __radd__(self, o):
        if isinstance(o, valder):
            return valder(self.val+o.val, self.der+o.der)
        else:
            return valder(self.val+o, self.der)
        
    def __mul__(self, o):
        if isinstance(o, valder):
            return valder(self.val*o.val, self.val*o.der + self.der*o.val)
        else:
            return valder(self.val*o, self.der*o)
        
    def __rmul__(self, o):
        """Needed to calculate 2*x, whereas __mul__ can do x*2"""
        if isinstance(o, valder):
            return valder(self.val*o.val, self.val*o.der + self.der*o.val)
        else:
            return valder(self.val*o, self.der*o)
        
    def __sub__(self, o):
        if isinstance(o, valder):
            return valder(self.val-o.val, self.der-o.der)
        else:
            return valder(self.val-o, self.der)
        
    def __rsub__(self, o):
        if isinstance(o, valder):
            return valder(self.val-o.val, o.der-self.der)
        else:
            return valder(self.val-o, -self.der)
        
    def __truediv__(self, o):
        if isinstance(o, valder):
            return valder(self.val/o.val, 
                          self.der/o.val - self.val/(o.val**2)*o.der)
        else:
            return valder(self.val/o, self.der/o)
        
    def __rtruediv__(self, o):
        if isinstance(o, valder):
            return valder(o.val/self.val, 
                          o.der/self.val - o.val/(self.val**2)*self.der)
        else:
            return valder(o/self.val, -o/(self.val**2)*self.der)
        
#     def __floordiv__(self, o):
#         if isinstance(o, valder):
#             return valder()
#         else:
#             return valder()
        
#     def __rfloordiv__(self, o):
        
    def __neg__(self):
        return valder(-self.val, -self.der)
        
    def __pos__(self):
        return valder(self.val, self.der)
        
    def __invert__(self):
        return valder(1.0/self.val, -self.der/(self.val**2))
        
    def __pow__(self, o):
        if isinstance(o, valder):
            return valder(self.val**o.val, 
                          self.val**o.val 
                          * (o.der*np.log(self.val) + o.val/self.val) )
        else:
            return valder(self.val**o, self.val**(o-1)*o*self.der)
        
    def __rpow__(self, o):
        if isinstance(o, valder):
            return valder(o.val**self.val, 
                          o.val**self.val 
                          * (self.der*np.log(o.val) + self.val/o.val) )
        else:
            return valder(o**self.val, o**self.val*np.log(o)*self.der)
        
    def __mod__(self, o):
        if isinstance(o, valder):
            return valder(self.val%o.val, np.sign(self.val) * self.der)
        else:
            return valder(self.val%o, np.sign(self.val) * self.der)
        
    def __rmod__(self, o):
        if isinstance(o, valder):
            return valder(o.val%self.val, np.sign(o.val) * o.der)
        else:
            return valder(o%self.val, 0)
        
    def __lt__(self, o):
        if isinstance(o, valder):
            return self.val < o.val
        else:
            return self.val < o
        
    def __rlt__(self, o):
        if isinstance(o, valder):
            return self.val > o.val
        else:
            return self.val > o
        
    def __gt__(self, o):
        if isinstance(o, valder):
            return self.val > o.val
        else:
            return self.val > o
        
    def __rgt__(self, o):
        if isinstance(o, valder):
            return self.val < o.val
        else:
            return self.val < o
        
    def __le__(self, o):
        if isinstance(o, valder):
            return self.val <= o.val
        else:
            return self.val <= o
        
    def __rle__(self, o):
        if isinstance(o, valder):
            return self.val >= o.val
        else:
            return self.val >= o
        
    def __ge__(self, o):
        if isinstance(o, valder):
            return self.val >= o.val
        else:
            return self.val >= o
        
    def __rge__(self, o):    
        if isinstance(o, valder):
            return self.val <= o.val
        else:
            return self.val <= o
        
    def __isub__(self, o):
        if isinstance(o, valder):
            return valder(self.val-o.val, self.der-o.der)
        else:
            return valder(self.val-o, self.der)
        
    def __risub__(self, o):
        if isinstance(o, valder):
            return valder(self.val-o.val, o.der-self.der)
        else:
            return valder(self.val-o, -self.der)
        
    def __iadd__(self, o):
        if isinstance(o, valder):
            return valder(self.val+o.val, self.der+o.der)
        else:
            return valder(self.val+o, self.der)
        
    def __riadd__(self, o):
        if isinstance(o, valder):
            return valder(self.val+o.val, self.der+o.der)
        else:
            return valder(self.val+o, self.der)
        
    def __imul__(self, o):
        if isinstance(o, valder):
            return valder(self.val*o.val, self.val*o.der + self.der*o.val)
        else:
            return valder(self.val*o, self.der*o)
        
    def __rimul__(self, o):
        if isinstance(o, valder):
            return valder(self.val*o.val, self.val*o.der + self.der*o.val)
        else:
            return valder(self.val*o, self.der*o)
        
    def __idiv__(self, o):
        if isinstance(o, valder):
            return valder(self.val/o.val, 
                          self.der/o.val - self.val/(o.val**2)*o.der)
        else:
            return valder(self.val/o, self.der/o)
        
    def __ridiv__(self, o):
        if isinstance(o, valder):
            return valder(o.val/self.val, 
                          o.der/self.val - o.val/(self.val**2)*self.der)
        else:
            return valder(o/self.val, -o/(self.val**2)*self.der)
        
#     def __ifloordiv__(self, o):
#         if isinstance(o, valder):
#             return valder()
#         else:
#             return valder()
        
#     def __rifloordiv__(self, o):
        
    def __imod__(self, o):
        if isinstance(o, valder):
            return valder(self.val%o.val, np.sign(self.val) * self.der)
        else:
            return valder(self.val%o, np.sign(self.val) * self.der)
        
    def __rimod__(self, o):
        if isinstance(o, valder):
            return valder(o.val%self.val, np.sign(o.val) * o.der)
        else:
            return valder(o%self.val, 0)
        
    def __ipow__(self, o):
        if isinstance(o, valder):
            return valder(self.val**o.val, 
                          self.val**o.val 
                          * (other.der*np.log(self.val) + other.val/self.val) )
        else:
            return valder(self.val**o, self.val**(o-1)*o)
        
    def __ripow__(self, o):
        if isinstance(o, valder):
            return valder(self.val**o.val, 
                          self.val**o.val 
                          * (other.der*np.log(self.val) + other.val/self.val) )
        else:
            return valder(o**self.val, o**self.val*np.log(o)*self.der)
        
    def cos(self):
        return valder(np.cos(self.val), -np.sin(self.val)*self.der)
        
    def sin(self):
        return valder(np.sin(self.val), np.cos(self.val)*self.der)
    
    def tan(self):
        return valder(np.tan(self.val), self.der/(np.cos(self.val))**2)

    def log(self):
        return valder(np.log(self.val), self.der/self.val)
    
    def log10(self):
        return valder(np.log10(self.val), self.der/(self.val * np.log(10)))
    
    def log2(self):
        return valder(np.log2(self.val), self.der/(self.val * np.log(2)))
    
    def sqrt(self):
        return valder(np.sqrt(self.val), self.der/(2*np.sqrt(self.val)))
    
    def exp(self):
        return valder(np.exp(self.val), np.exp(self.val)*self.der)
    
    def __abs__(self):
        return np.abs(self.val)
    
def print(*args, **kwargs):
    """Print valder object in a nice way"""
    for arg in args:
        if isinstance(arg, valder):
            __builtin__.print("val: {}, der: {}".format(arg.val, arg.der.flatten("K")))
        elif isinstance(arg, list) and isinstance(arg[0], valder):
            for v in arg:
                if isinstance(v.der, int):
                    __builtin__.print("val: {}, der: {}".format(v.val, v.der))
                else:
                    __builtin__.print("val: {}, der: {}".format(v.val, v.der.flatten("K")))
        elif isinstance(arg, np.ndarray) and isinstance(arg[0], valder):
            for v in arg:
                if isinstance(v.der, int):
                    __builtin__.print("val: {}, der: {}".format(v.val, v.der))
                else:
                    __builtin__.print("val: {}, der: {}".format(v.val, v.der.flatten("K")))
        else:
            __builtin__.print(arg, **kwargs)
            
def valderlist_to_2d(x):
    y = []
    x = x.flatten("K")
    for v in x:
        y.append([v.val, v.der.flatten("K")])
    return np.asarray(y)