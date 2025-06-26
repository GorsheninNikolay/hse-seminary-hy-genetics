(import sys)

(.append sys.path ".")

(import numpy :as np)
(import src.utils.draw_line [draw-line])
(import PIL [Image])

(defn main []
  (setv canvas (np.ones [150, 150] :dtype float))

  (draw-line canvas 20 80 100 20 4)

  (setv image (Image.fromarray (.astype (* canvas 255) np.uint8)))
  (.save image "output.png"))

(main)
