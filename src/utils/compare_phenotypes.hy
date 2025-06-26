"""
Компаратор изображений - вычисление расстояния между биоморфой и целевым изображением
"""

(import sys)
(import numpy :as np)
(import math)
(import PIL [Image])

(defn load-image [filepath]
  "Загружает изображение и конвертирует в RGB"
  (try
    (let [image (Image.open filepath)]
      (image.convert "RGB"))
    (except [e Exception]
      (print (.format "Ошибка загрузки изображения {}: {}" filepath e))
      (sys.exit 1))))

(defn get-euclidean-distance [generated-image target-image]
  "Вычисляет эвклидово расстояние между двумя изображениями"
  
  ;; Проверяем размеры изображений
  (when (!= generated-image.size target-image.size)
    (print "Ошибка: изображения должны быть одинакового размера")
    (sys.exit 1))
  
  ;; Конвертируем в numpy массивы
  (setv gen-array (np.array generated-image :dtype np.float32))
  (setv target-array (np.array target-image :dtype np.float32))
  
  ;; Нормализуем значения пикселей к диапазону [0, 1]
  (setv gen-array (/ gen-array 255.0))
  (setv target-array (/ target-array 255.0))
  
  ;; Вычисляем разность
  (setv diff (- gen-array target-array))
  
  ;; Вычисляем эвклидово расстояние
  (setv squared-diff (np.sum (** diff 2)))
  (setv distance (math.sqrt squared-diff))
  
  distance)

(defn compare-images [generated-path target-path]
  "Сравнивает два изображения эвклидовым расстоянием"
  (setv generated (load-image generated-path))
  (setv target (load-image target-path))
  
  (get-euclidean-distance generated target))