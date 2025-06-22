;; Модуль для препроцессинга изображений

(import PIL [Image])
(import os)

(defn preprocess_image [image-path]
  "Препроцессинг изображения: ресайз до 150x150 и конвертация в чёрно-белое.
  
  Args:
      image-path (str): Путь к входному изображению.
      
  Returns:
      PIL.Image.Image: Обработанное изображение.
  "

  (when (not (os.path.isfile image-path))
    (raise (FileNotFoundError f"Файл не найден по указанному пути: {image-path}")))
  
  (try
    (with [img (Image.open image-path)]
      (.resize (.convert img "L") (tuple [150 150]) Image.Resampling.LANCZOS))
    (except [e Exception]
      (print f"Ошибка при обработке изображения {image-path}: {e}")
      (raise e))))

(defn preprocess_and_save [input-path output-path]
  "Преобразует изображение, делает его черно-белым, меняет размер и сохраняет.

  Args:
      input-path (str): Путь к входному изображению.
      output-path (str): Путь для сохранения обработанного изображения.
  "
  (setv processed-img (preprocess_image input-path))
  (.save processed-img output-path))
