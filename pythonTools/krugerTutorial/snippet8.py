import tables
hf=tables.openFile('dataFile1.h5','r')
newx1=hf.root.x1.read()
newy1=hf.root.y1.read()
hf.close()
