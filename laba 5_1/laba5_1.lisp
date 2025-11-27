(defstruct manufacturer
  name
  country
  year)

(defstruct drone
  model
  manufacturer
  speed)

(defun read-table (pathspec)
  "Reads all lines from the text file specified by pathspec."
  (let ((filepath (probe-file pathspec)))
    (if filepath
        (with-open-file (stream filepath :direction :input)
          (loop for line = (read-line stream nil)
                while line
                collect (string-trim '(#\return) line)))
        (error "File not found: ~A" pathspec))))

(defun read-records (pathspec constructor)
  "Reads records from a CSV file (excluding the header) and creates structures using the constructor."
  (let ((data-lines (cdr (read-table pathspec))))
    (format t "Lines read: ~A~%" data-lines)
    (loop for current-line in data-lines
          for parts = (split-csv-line current-line)
          collect (apply constructor parts))))

(defun split-csv-line (line &optional (delimiter #\;))
  "Splits a line into parts based on the specified delimiter."
  (let ((start 0)
        (parts-list '()))
    (loop for index from 0 to (1- (length line))
          for char = (aref line index)
          do (if (char= char delimiter)
                 (progn
                   (push (subseq line start index) parts-list)
                   (setf start (1+ index)))))
 
    (push (subseq line start) parts-list)
    (nreverse parts-list)))

(defun read-manufacturers (pathspec)
  "Reads manufacturer records from a CSV file."
  (read-records pathspec
                (lambda (name country year)
                  (make-manufacturer :name name
                                     :country country
                                     :year (parse-integer year)))))

(defun read-drones (pathspec)
  "Reads drone records from a CSV file."
  (read-records pathspec
                (lambda (model manufacturer speed)
                  (make-drone :model model
                              :manufacturer manufacturer
                              :speed (parse-integer speed)))))

(defun print-table (filtered-records record-type)
  "Prints the filtered records in a formatted table."
  (if (eq record-type 'manufacturer)
      (progn
        (format t "~%~A~%" (format nil "~{~15A~^ | ~}" '("Manufacturer" "Country" "Year")))
        (format t "~A~%" (format nil "~{~15A~^---~}" '("----------------" "----------------" "----------------")))
        (dolist (record filtered-records)
          (format t "~15A | ~15A | ~15A~%"
                  (manufacturer-name record)
                  (manufacturer-country record)
                  (manufacturer-year record))))
      (progn
        (format t "~%~A~%" (format nil "~{~15A~^ | ~}" '("Drone Model" "Manufacturer" "Speed")))
        (format t "~A~%" (format nil "~{~15A~^---~}" '("----------------" "----------------" "----------------")))
        (dolist (record filtered-records)
          (format t "~15A | ~15A | ~15A~%"
                  (drone-model record)
                  (drone-manufacturer record)
                  (drone-speed record)))))
  (format t "~%"))

(defun select (pathspec &optional (record-type 'manufacturer))
  "Returns a function that performs filtering, printing, and saving of records."
  (lambda (&rest filters)
    (let ((all-records (if (eq record-type 'manufacturer)
                           (read-manufacturers pathspec)
                           (read-drones pathspec))))
      (let ((filtered-records
              (remove-if-not
               (lambda (record)
                 (loop for (key value) on filters by #'cddr
                       always (equal (slot-value record
                                                 (intern (symbol-name key) "COMMON-LISP-USER"))
                                     (if (eq key :year) (parse-integer value) value))))
               all-records)))

        (print-table filtered-records record-type)
        (let ((output-path (format nil "filtered_~A.csv"
                                   (if (eq record-type 'manufacturer) "manufacturers" "drones"))))
          (write-table filtered-records output-path record-type))))))

(defun write-table (records pathspec &optional (record-type 'manufacturer))
  "Writes records to a CSV file."
  (with-open-file (stream pathspec :direction :output :if-exists :supersede)
    (format t "Writing to file: ~A~%" pathspec)
    (write-headers stream record-type)
    (dolist (record records)
      (cond ((eq record-type 'manufacturer)
             (format stream "~A;~A;~A~%"
                     (manufacturer-name record)
                     (manufacturer-country record)
                     (manufacturer-year record)))
            ((eq record-type 'drone)
             (format stream "~A;~A;~A~%"
                     (drone-model record)
                     (drone-manufacturer record)
                     (drone-speed record)))))))

(defun write-headers (stream record-type)
  "Writes the column headers to the file stream."
  (cond ((eq record-type 'manufacturer)
         (format stream "Name;Country;Year~%"))
        ((eq record-type 'drone)
         (format stream "Model;Manufacturer;Speed~%"))))

(defun struct-to-hashtable (record)
  "Converts a struct record to a hash table."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (slot (list-all-slots record))
      (setf (gethash slot table) (slot-value record slot)))
    table))

(defun list-all-slots (record)
  "Returns a list of slot names for a given struct."
  (let ((type (type-of record)))
    (mapcar #'car (struct-slots type))))

(defun struct-slots (type)
  "Returns an association list of slot-name -> accessor-function for the struct type."
  (case type
    (manufacturer '((name . manufacturer-name)
                    (country . manufacturer-country)
                    (year . manufacturer-year)))
    (drone '((model . drone-model)
             (manufacturer . drone-manufacturer)
             (speed . drone-speed)))
    (otherwise (error "Unknown struct type: ~A" type))))

(defun convert-to-hashtable (records)
  "Converts a list of records into a list of hash tables."
  (mapcar #'struct-to-hashtable records))

(defparameter *data-directory* "E:/lisp/portacle/projects/laba_5/")

(defun test-read-table ()
  (let ((path (merge-pathnames "manufacturers.csv" *data-directory*)))
    (format t "===== Running test for read-table =====~%")
    (let ((lines (read-table path)))
      (assert (not (null lines)) nil "Failed to read table")
      (format t "Lines read:~%")
      (dolist (line lines)
        (format t "~A~%" line))
      (format t "===== End of read-table test =====~%")
      (format t "~%"))))

(defun test-read-manufacturers ()
  (let ((path (merge-pathnames "manufacturers.csv" *data-directory*)))
    (format t "===== Running test for read-manufacturers =====~%")
    (let ((manufacturers (read-manufacturers path)))
      (assert (every #'(lambda (m) (and (manufacturer-name m)
                                        (manufacturer-country m)
                                        (manufacturer-year m))) manufacturers)
              nil "Invalid manufacturer record")
      (format t "Manufacturers read:~%")
      (dolist (m manufacturers)
        (format t "~A~%" m))
      (format t "===== End of read-manufacturers test =====~%")
      (format t "~%"))))

(defun test-write-manufacturers ()
  (let ((path (merge-pathnames "filtered_manufacturers.csv" *data-directory*)))
    (format t "===== Running test for write-table with manufacturers =====~%")
    (let ((manufacturers (read-manufacturers (merge-pathnames "manufacturers.csv" *data-directory*))))
      (write-table manufacturers path 'manufacturer)
      (let ((file-contents (with-open-file (stream path)
                             (loop for line = (read-line stream nil)
                                   while line
                                   collect line))))
        (format t "Lines read from the file:~%")
        (dolist (line file-contents)
          (format t "~A~%" line))
        (assert file-contents nil "Write test failed")
        (format t "Test passed: File contains data~%")))
    (format t "===== End of write-manufacturers test =====~%")
    (format t "~%")))

(defun test-write-drones ()
  (let ((path (merge-pathnames "filtered_drones.csv" *data-directory*)))
    (format t "===== Running test for write-table with drones =====~%")
    (let ((drones (read-drones (merge-pathnames "drones.csv" *data-directory*))))
      (write-table drones path 'drone)
      (let ((file-contents (with-open-file (stream path)
                             (loop for line = (read-line stream nil)
                                   while line
                                   collect line))))
        (format t "Lines read from the file:~%")
        (dolist (line file-contents)
          (format t "~A~%" line))
        (assert file-contents nil "Write test failed")
        (format t "Test passed: File contains data~%")))
    (format t "===== End of write-drones test =====~%")
    (format t "~%")))

(defun test-select-manufacturers ()
  (let ((path (merge-pathnames "manufacturers.csv" *data-directory*)))
    (format t "===== Running test for select with manufacturers =====~%")
    (let ((filtered (funcall (select path 'manufacturer) :country "USA")))
      (assert (every #'(lambda (m) (equal (manufacturer-country m) "USA")) filtered)
              nil "Filtering by country failed")
      (format t "Test passed: Filtered manufacturers~%")
      (format t "===== End of select-manufacturers test =====~%")
      (format t "~%"))))

(defun test-select-drones ()
  (let ((path (merge-pathnames "drones.csv" *data-directory*)))
    (format t "===== Running test for select with drones =====~%")
    (let ((filtered (funcall (select path 'drone) :speed 100)))
      (assert (every #'(lambda (d) (= (drone-speed d) 100)) filtered)
              nil "Filtering by speed failed")
      (format t "Test passed: Filtered drones~%")
      (format t "===== End of select-drones test =====~%")
      (format t "~%"))))

(defun test-convert-to-hashtable ()
  (let ((path (merge-pathnames "manufacturers.csv" *data-directory*)))
    (format t "===== Running test for convert-to-hashtable =====~%")
    (let ((manufacturers (read-manufacturers path)))
      (let ((hashtables (convert-to-hashtable manufacturers)))
        (assert (every #'hash-table-p hashtables) nil "Conversion failed")
        (format t "Test passed: Conversion to hashtable successful~%")
        (format t "===== End of convert-to-hashtable test =====~%")
        (format t "~%")))))

(defun run-all-tests ()
  "Runs all defined tests."
  (format t "===== Running all tests... =====~%")
  (test-read-table)
  (test-read-manufacturers)
  (test-write-manufacturers)
  (test-write-drones)
  (test-select-manufacturers)
  (test-select-drones)
  (test-convert-to-hashtable)
  (format t "===== All tests completed. =====~%"))
