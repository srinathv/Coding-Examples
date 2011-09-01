from scipy import *
a = zeros(1000)
b = fft(a)
plot(abs(b))
from pylab import *
plot(abs(b))
b
a[:100]=1
a
b = fft(a)
plot(abs(b))
_ip.magic("history ")
_ip.magic("history -help")
#?history 
_ip.magic("history -n -f simplefft.py")
