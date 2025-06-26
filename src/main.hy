(import sys)
;; Добавляем корень проекта в sys.path, чтобы найти пакет src
(.append sys.path ".")

(import src.models [Phenotype create-random-genotype])
(import src.utils.preprocess_image [preprocess_image])
(import src.utils.compare_phenotypes [compare-images])
(import random :as rnd)
(import os)
(import numpy :as np)

;; Константы эволюционного алгоритма
(setv POPULATION_SIZE 50)
(setv MAX_GENERATIONS 1000)
(setv STAGNATION_LIMIT 20)


(defclass EvolutionSimulation []
  "Класс для эволюционной симуляции биоморфов."
  
  (defn __init__ [self target-image]
    "Инициализация симуляции с целевым изображением."
    (setv self.target-image target-image)
    (setv self.population [])
    (setv self.generation 0)
    (setv self.best-fitness (float "inf"))  ;; Инициализируем с бесконечностью для минимизации
    (setv self.stagnation-count 0)
    (setv self.individual-counter 0)
    
    ;; Создаем основную директорию для результатов
    (setv self.results-dir "evolution_results")
    (when (not (os.path.exists self.results-dir))
      (os.makedirs self.results-dir))
    
    ;; Создаем директорию для лучших фенотипов
    (setv self.best-dir (os.path.join self.results-dir "best_phenotypes"))
    (when (not (os.path.exists self.best-dir))
      (os.makedirs self.best-dir))
    
    (self.initialize-population))
  
  (defn create-generation-directory [self]
    "Создает директорию для текущего поколения."
    (setv gen-dir (os.path.join self.results-dir (.format "generation_{:03d}" self.generation)))
    (when (not (os.path.exists gen-dir))
      (os.makedirs gen-dir))
    gen-dir)
  
  (defn initialize-population [self]
    "Создаёт начальную популяцию случайных генотипов."
    (for [i (range POPULATION_SIZE)]
      (.append self.population (create-random-genotype))))
  
  (defn fitness-euclidean [self genotype generation-dir]
    "Вычисляет fitness показатель схожести с целевым изображением."
    ;; Создаем фенотип из генотипа
    (setv phenotype (Phenotype genotype))
    
    ;; Создаем уникальное имя файла для фенотипа
    (setv filename (.format "individual_{:03d}_fitness_TBD.png" self.individual-counter))
    (setv filepath (os.path.join generation-dir filename))
    
    ;; Сохраняем изображение фенотипа
    (phenotype.save-image filepath)
    
    ;; Вычисляем fitness, сравнивая с целевым изображением
    (setv fitness (compare-images self.target-image filepath))
    
    ;; Переименовываем файл, добавляя fitness в название
    (setv new-filename (.format "individual_{:03d}_fitness_{:.4f}.png" self.individual-counter fitness))
    (setv new-filepath (os.path.join generation-dir new-filename))
    (os.rename filepath new-filepath)
    
    ;; Увеличиваем счетчик особей
    (setv self.individual-counter (+ self.individual-counter 1))
    
    ;; Выводим информацию о сохраненном фенотипе
    (print (.format "    Особь {:03d}: fitness = {:.4f}" (- self.individual-counter 1) fitness))
    
    fitness)
  
  (defn evaluate-population [self]
    "Оценивает fitness всей популяции."
    ;; Создаем директорию для текущего поколения
    (setv generation-dir (self.create-generation-directory))
    
    (print (.format "Поколение {:03d} - Оценка популяции:" self.generation))
    (print (.format "  Директория: {}" generation-dir))
    
    (setv fitness-scores [])
    (setv self.individual-counter 0)  ;; Сбрасываем счетчик для каждого поколения
    
    (for [genotype self.population]
      (let [fitness (self.fitness-euclidean genotype generation-dir)]
        (.append fitness-scores fitness)))
    
    (print (.format "  Завершена оценка {} особей" (len fitness-scores)))
    (print)
    fitness-scores)
  
  (defn select-parents [self fitness-scores]
    "Выбирает родителей для следующего поколения (турнирный отбор)."
    (setv parents [])
    (for [i (range POPULATION_SIZE)]
      (let [tournament-size 5
            tournament-indices (rnd.sample (range (len self.population)) tournament-size)
            tournament-fitness []
            winner-index None
            winner None]
        ;; Собираем fitness для турнира
        (for [j tournament-indices]
          (.append tournament-fitness (get fitness-scores j)))
        ;; Находим победителя (минимальное расстояние = лучший fitness)
        (setv winner-index (get tournament-indices (.index tournament-fitness (min tournament-fitness))))
        (setv winner (get self.population winner-index))
        (.append parents winner)))
    parents)
  
  (defn mutate-population [self parents]
    "Создаёт новое поколение с мутациями."
    (setv new-population [])
    (setv mutation-count 0)
    (for [parent parents]
      (setv mutated-child (parent.mutate))
      ;; Проверяем, произошла ли мутация
      (when (!= mutated-child.genes parent.genes)
        (setv mutation-count (+ mutation-count 1)))
      (.append new-population mutated-child))
    
    (print (.format "Мутаций в поколении: {}/{}" mutation-count (len parents)))
    new-population)
  
  (defn save-best-phenotype [self best-genotype]
    "Сохраняет лучший фенотип поколения в специальную директорию"
    (setv phenotype (Phenotype best-genotype))
    (setv filename (.format "BEST_gen_{:03d}_fitness_{:.4f}.png" self.generation self.best-fitness))
    (setv filepath (os.path.join self.best-dir filename))
    (phenotype.save-image filepath)
    (print (.format "Лучший фенотип сохранен: {}" filename))
    filepath)
  
  (defn evolve [self]
    "Основной цикл эволюции."
    (print (.format "Начинаем эволюцию с популяцией из {} особей" POPULATION_SIZE))
    (print (.format "Результаты сохраняются в директории: {}" self.results-dir))
    (print (.format "Лучшие фенотипы: {}" self.best-dir))
    (print "Fitness = евклидово расстояние (меньше = лучше)")
    (print)
    
    (setv best-genotype-ever None)
    
    (while (and (< self.generation MAX_GENERATIONS)
                (< self.stagnation-count STAGNATION_LIMIT))
      ;; Оцениваем текущую популяцию
      (setv fitness-scores (self.evaluate-population))
      (setv best-fitness-this-gen (min fitness-scores))  ;; Минимальное расстояние = лучший fitness
      
      ;; Находим лучшую особь (с минимальным расстоянием)
      (setv best-index (.index fitness-scores best-fitness-this-gen))
      (setv best-genotype (get self.population best-index))
      
      ;; Проверяем улучшение (меньшее расстояние = улучшение)
      (if (< best-fitness-this-gen self.best-fitness)
          (do
            (setv self.best-fitness best-fitness-this-gen)
            (setv self.stagnation-count 0)
            (setv best-genotype-ever best-genotype)
            ;; Сохраняем лучший фенотип поколения
            (self.save-best-phenotype best-genotype))
          (setv self.stagnation-count (+ self.stagnation-count 1)))
      
      ;; Выводим статистику
      (print (.format "Статистика поколения {:03d}:" self.generation))
      (print (.format "Лучший fitness: {:.4f} (расстояние)" best-fitness-this-gen))
      (print (.format "Лучшая особь: {}" best-genotype-ever.genes))
      (print (.format "Застой: {}/{}" self.stagnation-count STAGNATION_LIMIT))
      (print)
      
      ;; Создаём следующее поколение
      (setv parents (self.select-parents fitness-scores))
      (setv self.population (self.mutate-population parents))
      (setv self.generation (+ self.generation 1)))
    
    ;; Финальный результат
    (print)
    (print "Эволюция завершена!")
    (print (.format "Итоговый лучший fitness: {:.4f} (минимальное расстояние)" self.best-fitness))
    (print (.format "Поколений: {}" self.generation))
    (print (.format "Всего сохранено изображений: {}" (* self.generation POPULATION_SIZE)))
    (if (= self.stagnation-count STAGNATION_LIMIT)
        (print "Остановка по критерию застоя")
        (print "Остановка по максимальному числу поколений"))
    
    ;; Возвращаем лучший найденный генотип
    best-genotype-ever)
)

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
  
  ;; ;; Запускаем эволюцию
  (setv simulation (EvolutionSimulation output-path))
  (setv best-genotype (simulation.evolve))
  (print (.format "Длина сегментов: {}" (best-genotype.get-segment-length)))
  )

(when (= __name__ "__main__")
    (main))
