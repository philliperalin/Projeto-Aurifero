import cv2
import numpy as np
import detect
from subprocess import Popen, PIPE

def run (img_path, thk):
    ter = cv2.imread(img_path)
    thk = cv2.imread(thk)
    image = cv2.imread(img_path)
    b = image[:, :, 0]
    g = image[:, :, 1]
    r = image[:, :, 2]
    for i in range(0, len(g)):
        for c in range(0, len(g[0])):
            if g[i, c] == 0:
                g[i, c] = 1
    fator_fn = np.array(r * b / g, dtype=np.uint8)
    fator_f = cv2.applyColorMap(fator_fn, cv2.COLORMAP_HOT)
    cv2.imwrite('fator_paaf_gis.tif', fator_f)

    detect.run(weights='bestter.pt', source=ter, imgsz=416, conf_thres=0.2, save_txt=True)
    detect.run(weights='bestthk.pt', source=thk, imgsz=416, conf_thres=0.2, save_txt=True)
    detect.run(weights='bestfator.pt', source=fator_f, imgsz=416, conf_thres=0.2, save_txt=True)

    run_map()

def run_map ():
    cmd = ["Rscript", "making-maps.R"]
    p = Popen(cmd, cwd="/making-maps.R/", stdin=PIPE, stdout=PIPE, stderr=PIPE)
    output, error = p.communicate()
    if p.returncode == 0:
        print('Mapa final:\n {0}'.format(output))
    else:
        print('Erro:\n {0}'.format(error))
