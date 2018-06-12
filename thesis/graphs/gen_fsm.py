from os import path, listdir
from os.path import isfile, join
from subprocess import call

for gvPath in listdir("."):
    if isfile(gvPath) and gvPath.endswith(".gv"):
        imgPath = path.splitext(gvPath)[0] + '.png'
        call(["dot", "-Tpng", "-Gdpi=300", gvPath, "-o", imgPath])
