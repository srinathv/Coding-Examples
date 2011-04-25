from matplotlib import pylab
import tables
rdArray=pylab.loadtxt('dataFile1.txt',comments='x1')
rdArray.shape
x1=rdArray[:,0]
y1=rdArray[:,1]
hf=tables.openFile('dataFile1.h5','w')
x1n=hf.createArray(hf.root,"x1",x1)
hf.setNodeAttr(x1n,"vsType", "mesh")
hf.setNodeAttr(x1n,"vsKind", "structured")
y1n=hf.createArray(hf.root,"y1",y1)
hf.setNodeAttr(x1n,"vsType", "variable")
hf.setNodeAttr(x1n,"vsMesh", "x1")
hf.close()


