(import sys)
;; Добавляем корень проекта в sys.path, чтобы найти пакет src
(.append sys.path ".")

(import src.models.gene [create-random-genotype])
(import src.utils.preprocess_image [preprocess_image])
(import random :as rnd)
(import os)
(import numpy :as np)

;; Константы эволюционного алгоритма
(setv POPULATION_SIZE 50)
(setv MAX_GENERATIONS 100)
(setv STAGNATION_LIMIT 10)
(setv MUTATION_RATE 0.1)

(defclass EvolutionSimulation []
  "Класс для эволюционной симуляции биоморфов."
  
  (defn __init__ [self target-image]
    "Инициализация симуляции с целевым изображением."
    (setv self.target-image target-image)
    (setv self.population [])
    (setv self.generation 0)
    (setv self.best-fitness 0)
    (setv self.stagnation-count 0)
    (self.initialize-population))
  
  (defn initialize-population [self]
    "Создаёт начальную популяцию случайных генотипов."
    (for [i (range POPULATION_SIZE)]
      (.append self.population (create-random-genotype))))
  
  (defn fitness-euclidean [self genotype]
    "Вычисляет fitness показатель схожести с целевым изображением."
    ;; Упрощённая версия - считаем случайное значение
    ;; TODO: Добавить свёртку с целевым изображением
    (rnd.uniform 0 1))
  
  (defn evaluate-population [self]
    "Оценивает fitness всей популяции."
    (setv fitness-scores [])
    (for [genotype self.population]
      (let [fitness (self.fitness-euclidean genotype)]
        (.append fitness-scores fitness)))
    fitness-scores)
  
  (defn select-parents [self fitness-scores]
    "Выбирает родителей для следующего поколения (турнирный отбор)."
    (setv parents [])
    (for [i (range POPULATION_SIZE)]
      (let [tournament-size 3
            tournament-indices (rnd.sample (range (len self.population)) tournament-size)
            tournament-fitness []
            winner-index None
            winner None]
        ;; Собираем fitness для турнира
        (for [j tournament-indices]
          (.append tournament-fitness (get fitness-scores j)))
        ;; Находим победителя
        (setv winner-index (get tournament-indices (.index tournament-fitness (max tournament-fitness))))
        (setv winner (get self.population winner-index))
        (.append parents winner)))
    parents)
  
  (defn mutate-population [self parents]
    "Создаёт новое поколение с мутациями."
    (setv new-population [])
    (for [parent parents]
      (if (< (rnd.random) MUTATION_RATE)
          ;; Мутируем один случайный ген
          (.append new-population (parent.mutate))
          ;; Без мутации
          (.append new-population parent)))
    new-population)
  
  (defn evolve [self]
    "Основной цикл эволюции."
    (print (.format "Начинаем эволюцию с популяцией из {} особей" POPULATION_SIZE))
    (print)
    
    (setv best-genotype-ever None)
    
    (while (and (< self.generation MAX_GENERATIONS)
                (< self.stagnation-count STAGNATION_LIMIT))
      ;; Оцениваем текущую популяцию
      (setv fitness-scores (self.evaluate-population))
      (setv best-fitness-this-gen (max fitness-scores))
      
      ;; Находим лучшую особь
      (setv best-index (.index fitness-scores best-fitness-this-gen))
      (setv best-genotype (get self.population best-index))
      
      ;; Проверяем улучшение
      (if (> best-fitness-this-gen self.best-fitness)
          (do
            (setv self.best-fitness best-fitness-this-gen)
            (setv self.stagnation-count 0)
            (setv best-genotype-ever best-genotype))
          (setv self.stagnation-count (+ self.stagnation-count 1)))
      
      ;; Выводим статистику
      (print (.format "Поколение {}:" self.generation))
      (print (.format "  Лучший fitness: {:.4f}" best-fitness-this-gen))
      (print (.format "  Лучшая особь: {}" best-genotype))
      (print (.format "  Застой: {}/{}" self.stagnation-count STAGNATION_LIMIT))
      (print)
      
      ;; Создаём следующее поколение
      (setv parents (self.select-parents fitness-scores))
      (setv self.population (self.mutate-population parents))
      (setv self.generation (+ self.generation 1)))
    
    ;; Финальный результат
    (print "Эволюция завершена!")
    (print (.format "Итоговый лучший fitness: {:.4f}" self.best-fitness))
    (print (.format "Поколений: {}" self.generation))
    (if (= self.stagnation-count STAGNATION_LIMIT)
        (print "Остановка по критерию застоя")
        (print "Остановка по максимальному числу поколений"))
    
    ;; Возвращаем лучший найденный генотип
    best-genotype-ever)
)
;; TODO: Загружать реальное изображение
(defn create-dummy-target []
  "Создаёт фиктивное целевое изображение для демонстрации."
  (np.random.rand 150 150))

(defn main []  
  (when (< (len sys.argv) 2)
      (do
        (print "Использование: hy main.hy <путь_к_изображению>")
	(sys.exit 1)))

  (setv image-path (get sys.argv 1))

  (setv processed-img (preprocess_image image-path))
  (setv [base-name ext] (os.path.splitext (os.path.basename image-path)))
  (setv output-name (+ base-name "_processed" ext))
  (setv output-path (os.path.join (os.path.dirname image-path) output-name))
  
  ;; Save processed image
  (processed-img.save output-path)
  (setv target-image (/ (np.array processed-img) 255.0))
  ;; Создаём фиктивное целевое изображение
  
  ;; Запускаем эволюцию
  (setv simulation (EvolutionSimulation target-image))
  (setv best-genotype (simulation.evolve))
  (print (.format "Лучший генотип: {}" best-genotype))
  (print (.format "Длина сегментов: {}" (best-genotype.get-segment-length)))
  (print (.format "Сгенерированные сегменты: {}" (best-genotype.generate-segments 0 0))))

(when (= __name__ "__main__")
    (main))
