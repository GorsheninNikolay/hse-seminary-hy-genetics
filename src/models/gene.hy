;; Класс для представления гена в эволюционной симуляции биоморфов

(import random :as rnd)
(import math)
(import itertools :as it)

(defclass Gene []
  "Класс для представления гена."
  
  ;; Константы для типов генов
  (setv SKELETON_GENE "skeleton")
  (setv LENGTH_GENE "length")
  
  ;; Константы для диапазонов значений
  (setv SKELETON_MIN -9)
  (setv SKELETON_MAX 9)
  (setv LENGTH_MIN 2)
  (setv LENGTH_MAX 12)
  
  ;; Константы для генерации отрезков
  (setv ANGLE_MULTIPLIER 20)  ; Каждая единица гена = 20 градусов
  (setv BASE_LENGTH 2)        ; Базовая длина сегмента
  
  (defn __init__ [self value position]
    "Инициализация гена."
    (setv self.position position)
    (setv self.value value)
    
    ;; Определяем тип гена по позиции
    (if (= position 15)
        (setv self.gene-type Gene.LENGTH_GENE)
        (setv self.gene-type Gene.SKELETON_GENE))
    
    ;; Валидация значения
    (self.validate-value)
    
    ;; Вычисляем параметры для генерации отрезков
    (self.calculate-segment-parameters))
  
  (defn validate-value [self]
    "Проверяет корректность значения гена."
    (let [valid-range (self.get-valid-range)]
      (when (not (and (>= self.value (get valid-range 0))
                      (<= self.value (get valid-range 1))))
        (raise (ValueError
                 f"Значение гена должно быть от {(get valid-range 0)} до {(get valid-range 1)} для типа {self.gene-type}")))))
  
  (defn get-valid-range [self]
    "Возвращает допустимый диапазон значений для типа гена."
    (if (= self.gene-type Gene.LENGTH_GENE)
        [Gene.LENGTH_MIN Gene.LENGTH_MAX]
        [Gene.SKELETON_MIN Gene.SKELETON_MAX]))
  
  (defn calculate-segment-parameters [self]
    "Вычисляет параметры для генерации отрезков."
    (if (= self.gene-type Gene.SKELETON_GENE)
        ;; Для гена скелета вычисляем угол
        (do
          (setv self.angle (* self.value Gene.ANGLE_MULTIPLIER))
          (setv self.segment-length None))
        ;; Для гена длины вычисляем длину сегмента
        (do
          (setv self.angle None)
          (setv self.segment-length (+ Gene.BASE_LENGTH self.value)))))
  
  (defn get-angle [self]
    "Возвращает угол в градусах для генерации отрезка."
    (if (self.is-skeleton-gene)
        self.angle
        (raise (ValueError "Угол можно получить только для гена скелета"))))
  
  (defn get-angle-radians [self]
    "Возвращает угол в радианах для генерации отрезка."
    (math.radians (self.get-angle)))
  
  (defn get-segment-length [self]
    "Возвращает длину сегмента."
    (if (self.is-length-gene)
        self.segment-length
        (raise (ValueError "Длину можно получить только для гена длины"))))
  
  (defn generate-segment [self start-x start-y segment-length]
    "Генерирует отрезок на основе параметров гена."
    (if (self.is-skeleton-gene)
        (let [angle-rad (self.get-angle-radians)
              end-x (+ start-x (* segment-length (math.cos angle-rad)))
              end-y (+ start-y (* segment-length (math.sin angle-rad)))]
          [start-x start-y end-x end-y])
        None))
  
  (defn mutate [self]
    "Мутирует ген, изменяя его значение на ±1."
    (let [mutation (rnd.choice [-1 1])
          new-value (+ self.value mutation)
          valid-range (self.get-valid-range)
          min-val (get valid-range 0)
          max-val (get valid-range 1)
          clamped-value (max min-val (min max-val new-value))]
      
      (Gene clamped-value self.position)))
  
  (defn is-skeleton-gene [self]
    "Проверяет, является ли ген геном скелета."
    (= self.gene-tyGene.ne.SKELETON_GENE))
  
  (defn is-length-gene [self]
    "Проверяет, является ли ген геном длины."
    (= self.gene-tyGene.ne.LENGTH_GENE))
  
  (defn copy [self]
    "Создаёт копию гена."
    (Gene self.value self.position)))

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
    (self.length-gene.get-segment-length))
  
  (defn select-random-skeleton-genes [self count]
    "Выбирает случайные гены скелета для генерации."
    (rnd.sample self.skeleton-genes count))
  
  (defn mutate [self]
    "Мутирует случайный ген в генотипе."
    (let [new-genes (list self.genes)
          gene-index (rnd.randint 0 15)
          gene-to-mutate (get new-genes gene-index)
          mutated-gene (gene-to-mutate.mutate)]
      
      (setv (get new-genes gene-index) mutated-gene)
      (Genotype new-genes)))
  
  (defn generate-segments [self start-x start-y]
    "Генерирует все сегменты биоморфа."
    (let [segments []
          current-x start-x
          current-y start-y
          segment-length (self.get-segment-length)
          selected-genes (self.select-random-skeleton-genes 7)]
      
      ;; Генерируем сегменты на основе выбранных генов
      (for [gene selected-genes]
        (let [segment (gene.generate-segment current-x current-y segment-length)]
          (when segment
              (do
                (.append segments segment)
                (setv current-x (get segment 2))
                (setv current-y (get segment 3))))))
      
      segments)))

;; Фабричные функции
(defn create-random-gene [position]
  "Создаёт случайный ген для заданной позиции."
  (if (= position 15)
      ;; Ген длины
      (let [value (rnd.randint Gene.LENGTH_MIN Gene.LENGTH_MAX)]
        (Gene value position))
      ;; Ген скелета
      (let [value (rnd.randint Gene.SKELETON_MIN Gene.SKELETON_MAX)]
        (Gene value position))))

(defn create-random-genotype []
  "Создаёт случайный генотип из 16 генов."
  (let [genes []]
    (for [i (range 16)]
      (.append genes (create-random-gene i)))
    (Genotype genes)))
(defclass Phenotype []
  "Класс для представления сегенрированного определённым генотипом фенотипа."
  (defn draw-segments [self]
    (let [canvas (np.ones [150, 150] :dtype float)]
      (for [segment self.segments]
        (let [start-x (get segment 0)
              start-y (get segment 1)
              end-x (get segment 2)
              end-y (get segment 3)]
          (draw-line canvas start-x start-y end-x end-y
           Phenotype.SEGMENT_WIDTH)))
      canvas))

  (defn __init__ [self genotype]
    "Инициализация фенотипа."
    (setv self.genotype genotype)
    (setv self.segments (genotype.generate-segments 75 149))
    (setv SEGMENT_WIDTH 4)
    (setv self.canvas (self.draw-segments))))

