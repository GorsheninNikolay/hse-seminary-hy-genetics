(import sys)
;; Добавляем корень проекта в sys.path, чтобы найти пакет src
(.append sys.path ".")

(import src.utils.preprocess_image [preprocess_and_save])

(setv example-abs-input-file-path "/PATH/TO/INPUT/FILE")
(setv example-abs-output-file-path "/ABS/static/result/result.png")

(defn main []
  (setv input-file example-abs-input-file-path)
  (setv output-file example-abs-output-file-path)

  (print f"Обработка файла '{input-file}'...")
  (try
    (preprocess_and_save input-file output-file)
    (print f"Результат сохранен в '{output-file}'.")
    (except [e Exception]
      (print f"Произошла ошибка: {e}"))))

(main)
