;; Класс для представления гена в эволюционной симуляции биоморфов

(import random :as rnd)
(import math)
(import itertools :as it)
(import numpy :as np)
(import PIL [Image ImageDraw])

(setv LINE_COLOR "black")
(setv LINE_WIDTH 1)
(setv SKELETON_MIN -9)
(setv SKELETON_MAX 9)
(setv LENGTH_MIN 2)
(setv LENGTH_MAX 12)
(setv SEGMENT_WIDTH 1)
(setv IMAGE_SIZE 150)
(setv CENTER 75)
(setv MAX_GENES 15)

(defn select-random-genes [genes count]
  "Выбирает случайные гены из первых 15"
  (setv indices (rnd.sample (range MAX_GENES) count))
  (list (map (fn [i] (get genes i)) indices)))

  ;; DETERMINE
  ;; (list (map (fn [i] (get genes i)) (range count))))

(defn calculate-stems [genes]
  "Вычисляет направления роста биоморфы"
  (setv selected (select-random-genes genes 7))
  
  (defn g [i]
    (get selected (% i (len selected))))
  
  [
    {"dx" 0 "dy" (g 0)}        ;; Вверх
    {"dx" (g 1) "dy" (g 2)}    ;; Вверх-вправо  
    {"dx" (g 3) "dy" 0}        ;; Вправо
    {"dx" (g 4) "dy" (- (g 5))} ;; Вниз-вправо
    {"dx" 0 "dy" (- (g 6))}    ;; Вниз
    {"dx" (- (g 4)) "dy" (- (g 5))} ;; Вниз-влево
    {"dx" (- (g 3)) "dy" 0}    ;; Влево
    {"dx" (- (g 1)) "dy" (g 2)} ;; Вверх-влево
  ])

(defn render-segments [length stems direction position]
  "Рекурсивно строит сегменты биоморфы"
  (if (<= length 0)
      []
      (do
        (setv segments [])
        (setv new-dir (% direction 8))
        (setv stem (get stems new-dir))
        
        (setv new-x (+ (get position "x") (* length (get stem "dx"))))
        (setv new-y (+ (get position "y") (* length (get stem "dy"))))
        
        (setv segment {
          "start" (dict position)
          "finish" {"x" new-x "y" new-y}
        })
        (.append segments segment)
        
        (setv new-pos {"x" new-x "y" new-y})
        
        ;; Рекурсивно строим ветви
        (.extend segments (render-segments (- length 1) stems (+ direction 1) new-pos))
        (.extend segments (render-segments (- length 1) stems (- direction 1) new-pos))
        
        segments)))

(defn calculate-bounds [segments]
  "Вычисляет границы фигуры"
  (if (not segments)
      {"min_x" 0 "min_y" 0 "max_x" 0 "max_y" 0}
      (do
        (setv min-x (float "inf"))
        (setv min-y (float "inf"))
        (setv max-x (float "-inf"))
        (setv max-y (float "-inf"))
        
        (for [segment segments]
          (setv start (get segment "start"))
          (setv finish (get segment "finish"))
          (setv min-x (min min-x (get start "x") (get finish "x")))
          (setv min-y (min min-y (get start "y") (get finish "y")))
          (setv max-x (max max-x (get start "x") (get finish "x")))
          (setv max-y (max max-y (get start "y") (get finish "y"))))
        
        {"min_x" min-x "min_y" min-y "max_x" max-x "max_y" max-y})))

;; Класс для работы с генотипом
(defclass Genotype []
  "Класс для представления полного генотипа биоморфа."
  
  (defn __init__ [self genes]
    "Инициализация генотипа."
    (when (!= (len genes) 16)
        (raise (ValueError "Генотип должен содержать ровно 16 генов")))
    
    (setv self.genes genes)
    (setv self.length-gene (get genes 15))
    (setv self.skeleton-genes (list (it.islice genes 15))))
  
  (defn get-skeleton-genes [self]
    "Возвращает гены скелета."
    self.skeleton-genes)
  
  (defn get-length-gene [self]
    "Возвращает ген длины."
    self.length-gene)
  
  (defn get-segment-length [self]
    "Возвращает длину сегментов для всего генотипа."
    self.length-gene)
  
  (defn mutate [self]
    "Мутирует случайный ген в генотипе."
    (let [new-genes (list self.genes)
          gene-index (rnd.randint 0 15)
          old-value (get self.genes gene-index)]
      
      ;; Мутируем значение гена
      (if (= gene-index 15)
          ;; Мутируем ген длины
          (let [current-value (get new-genes gene-index)
                delta (rnd.choice [-1 1])
                new-value (max LENGTH_MIN (min LENGTH_MAX (+ current-value delta)))]
            (setv (get new-genes gene-index) new-value))
          ;; Мутируем ген скелета
          (let [current-value (get new-genes gene-index)
                delta (rnd.choice [-1 1]) 
                new-value (max SKELETON_MIN (min SKELETON_MAX (+ current-value delta)))]
            (setv (get new-genes gene-index) new-value)))

      (Genotype new-genes)))
  
  (defn generate-segments [self start-x start-y]
    "Генерирует все сегменты биоморфа"
    (setv stems (calculate-stems self.genes))
    (setv length (get self.genes -1))  ;; Последний ген - длина
    (render-segments length stems 0 {"x" 0 "y" 0})))

;; Фабричные функции
(defn create-random-gene [position]
  "Создаёт случайный ген для заданной позиции."
  (if (= position 15)
      ;; Ген длины
      (rnd.randint LENGTH_MIN LENGTH_MAX)
      ;; Ген скелета
      (rnd.randint SKELETON_MIN SKELETON_MAX)))

(defn create-random-genotype []
  "Создаёт случайный генотип из 16 генов."
  (let [genes []]
    (for [i (range 16)]
      (.append genes (create-random-gene i)))
    (Genotype genes)))


(defclass Phenotype []
  "Класс для представления сгенерированного определённым генотипом фенотипа."
  
  (defn draw-biomorph [self]
    "Отрисовывает биоморфу на изображении"
    (setv bounds (calculate-bounds self.segments))
    
    ;; Вычисляем масштаб
    (setv width (- (get bounds "max_x") (get bounds "min_x")))
    (setv height (- (get bounds "max_y") (get bounds "min_y")))
    (setv size (max width height))
    (setv scale (if (> size 0) (/ 60 (max 1 size)) 1.0))
    
    ;; Создаем изображение
    (setv image (Image.new "RGB" (tuple [IMAGE_SIZE IMAGE_SIZE]) "white"))
    (setv draw (ImageDraw.Draw image))

    ;; Рисуем сегменты
    (for [segment self.segments]
      (setv start (get segment "start"))
      (setv finish (get segment "finish"))
      
      ;; Преобразуем координаты
      (setv x1 (int (+ CENTER (* (get start "x") scale))))
      (setv y1 (int (- CENTER (* (get start "y") scale))))  ;; Инвертируем Y
      (setv x2 (int (+ CENTER (* (get finish "x") scale))))
      (setv y2 (int (- CENTER (* (get finish "y") scale))))
      
      ;; Рисуем линию (правильный синтаксис для Hy)
      (draw.line [x1 y1 x2 y2] :fill LINE_COLOR :width LINE_WIDTH))
    
    image)

  (defn save-image [self filepath]
    "Сохраняет изображение фенотипа по указанному пути"
    (setv image (self.draw-biomorph))
    (image.save filepath)
    filepath)

  (defn __init__ [self genotype]
    "Инициализация фенотипа."
    (setv self.genotype genotype)
    (setv self.segments (genotype.generate-segments 75 75))))
