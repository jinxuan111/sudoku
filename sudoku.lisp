;; ============================================================
;; Sudoku Solver Program - Using Backtracking and DFS Methods
;; ============================================================

;; ===== 1. Basic Data Structures and Functions =====

(defun create-easy-puzzle  ()
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 0 3)
    (4 0 0 8 0 3 0 0 1)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9)))

(defun create-medium-puzzle ()
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 0 0)
    (4 0 0 8 0 3 0 0 1)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9)))

(defun create-hard-puzzle ()
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)

    (8 0 0 0 6 0 0 0 0)
    (4 0 0 8 0 3 0 0 0)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9)))

(defun copy-board (board)
  "Create a deep copy of the board"
  (mapcar #'copy-list board))

(defun get-cell (board row col)
  (nth col (nth row board)))

(defun set-cell (board row col value)
  (setf (nth col (nth row board)) value)
  board)

(defun is-valid-row (board row num)
  (not (member num (nth row board))))

(defun is-valid-col (board col num)
  (not (member num (mapcar (lambda (r) (nth col r)) board))))

(defun is-valid-box (board row col num)
  (let* ((box-row (* (floor row 3) 3))
         (box-col (* (floor col 3) 3))
         (box-nums nil))
    (dotimes (i 3)
      (dotimes (j 3)
        (push (get-cell board (+ box-row i) (+ box-col j)) box-nums)))
    (not (member num box-nums))))

(defun is-valid-move (board row col num)
  (and (is-valid-row board row num)
       (is-valid-col board col num)
       (is-valid-box board row col num)))

(defun find-empty-cell (board)
  (dotimes (row 9)
    (dotimes (col 9)
      (when (zerop (get-cell board row col))
        (return-from find-empty-cell (list row col)))))
  nil)

(defun is-solved (board)
  (null (find-empty-cell board)))

(defun print-board (board)
  (dotimes (row 9)
    (when (and (> row 0) (zerop (mod row 3)))
      (format t "------+-------+------~%"))
    (dotimes (col 9)
      (when (and (> col 0) (zerop (mod col 3)))
        (format t "| "))
      (let ((val (get-cell board row col)))
        (format t "~a " (if (zerop val) "." val))))
    (format t "~%")))

;; ===== 2. Method 1: Backtracking =====

(defvar *backtrack-count* 0)

(defun solve-sudoku-backtrack (board &optional (count-nodes nil))
  (when count-nodes (setf *backtrack-count* 0))
  (labels ((backtrack (b)
             (when count-nodes (incf *backtrack-count*))
             (if (is-solved b)
                 (return-from backtrack t))
             (let ((empty (find-empty-cell b)))
               (unless empty (return-from backtrack t))
               (let ((row (first empty)) (col (second empty)))
                 (dotimes (num 9)
                   (let ((digit (+ num 1)))
                     (when (is-valid-move b row col digit)
                       (set-cell b row col digit)
                       (when (backtrack b)
                         (return-from backtrack t))
                       (set-cell b row col 0)))))
               nil)))
    (let ((copy (copy-board board)))
      (if (backtrack copy)
          (values copy *backtrack-count*)
          (values nil *backtrack-count*)))))

;; ===== 3. Method 2: DFS with Heuristic =====

(defvar *dfs-count* 0)

(defun get-candidates (board row col)
  (let ((candidates nil))
    (dotimes (num 9)
      (let ((digit (+ num 1)))
        (when (is-valid-move board row col digit)
          (push digit candidates))))
    (reverse candidates)))

(defun find-best-empty-cell (board)
  "Find empty cell with fewest candidates (MRV heuristic)"
  (let ((best nil) (min-cnt 10))
    (dotimes (row 9)
      (dotimes (col 9)
        (when (zerop (get-cell board row col))
          (let ((cnt (length (get-candidates board row col))))
            (when (< cnt min-cnt)
              (setf min-cnt cnt)
              (setf best (list row col)))))))
    best))

(defun solve-sudoku-dfs (board &optional (count-nodes nil))
  (when count-nodes (setf *dfs-count* 0))
  (labels ((dfs (b)
             (when count-nodes (incf *dfs-count*))
             (if (is-solved b)
                 (return-from dfs t))
             (let ((empty (find-best-empty-cell b)))
               (unless empty (return-from dfs t))
               (let* ((row (first empty))
                      (col (second empty))
                      (candidates (get-candidates b row col)))
                 (dolist (digit candidates)
                   (set-cell b row col digit)
                   (when (dfs b)
                     (return-from dfs t))
                   (set-cell b row col 0)))
               nil)))
    (let ((copy (copy-board board)))
      (if (dfs copy)
          (values copy *dfs-count*)
          (values nil *dfs-count*)))))

;; ===== 4. Run and report a method =====

(defun run-and-report-method (puzzle solver-fn method-name)
  (let* ((start (get-internal-run-time))
         (results (multiple-value-list (funcall solver-fn puzzle t)))
         (solution (first results))
         (nodes (second results))
         (end (get-internal-run-time))
         (time-ms (* 1000 (/ (- end start) (float internal-time-units-per-second)))))
    (if solution
        (progn
          (format t "Status: ✓ Solved successfully!~%")
          (format t "Nodes expanded: ~d~%" nodes)
          (format t "Time: ~,2f ms~%" time-ms)
          (format t "Verification: ~a~%"
                  (if (verify-solution solution) "✓ Correct" "✗ Incorrect"))
          (format t "~%Solution:~%")
          (print-board solution))
        (progn
          (format t "Status: ✗ Failed to solve!~%")
          (format t "Nodes expanded: ~d~%" nodes)))
    (values nodes time-ms)))

;; ===== 5. Verify solution =====

(defun verify-solution (board)
  (let ((valid t))
    (dotimes (row 9)
      (unless (equal (sort (copy-list (nth row board)) #'<) '(1 2 3 4 5 6 7 8 9))
        (setf valid nil)))
    (dotimes (col 9)
      (let ((col-vals (mapcar (lambda (r) (nth col r)) board)))
        (unless (equal (sort (copy-list col-vals) #'<) '(1 2 3 4 5 6 7 8 9))
          (setf valid nil))))
    (dotimes (br 3)
      (dotimes (bc 3)
        (let ((box nil))
          (dotimes (i 3)
            (dotimes (j 3)
              (push (get-cell board (+ (* br 3) i) (+ (* bc 3) j)) box)))
          (unless (equal (sort (copy-list box) #'<) '(1 2 3 4 5 6 7 8 9))
            (setf valid nil)))))
    valid))

(defun count-empty (board)
  (let ((cnt 0))
    (dotimes (row 9)
      (dotimes (col 9)
        (when (zerop (get-cell board row col))
          (incf cnt))))
    cnt))

;; ===== 6. Test one level and collect data =====

(defun test-puzzle-level (level-name puzzle)
  (format t "~%--- Difficulty: ~a ---~%" level-name)
  (format t "~%Initial Puzzle:~%")
  (print-board puzzle)
  (let ((empty-count (count-empty puzzle))
        bt-nodes bt-time dfs-nodes dfs-time)
    (format t "Empty cells: ~d~%" empty-count)

    (format t "~%[Method 1: Backtracking]~%")
    (setf (values bt-nodes bt-time)
          (run-and-report-method puzzle #'solve-sudoku-backtrack "Backtracking"))

    (format t "~%[Method 2: DFS with Heuristic]~%")
    (setf (values dfs-nodes dfs-time)
          (run-and-report-method puzzle #'solve-sudoku-dfs "DFS"))

    (list level-name empty-count bt-nodes dfs-nodes bt-time dfs-time)))

;; ===== 7. Performance summary =====

(defun performance-summary (data-list)
  (format t "~%========================================~%")
  (format t "  Performance Comparison Summary~%")
  (format t "========================================~%")
  (format t "~%Difficulty   | Empty | Backtrack Nodes | DFS Nodes | Backtrack (ms) | DFS (ms) | Faster Method~%")
  (format t "-------------|-------|-----------------|-----------|----------------|----------|--------------~%")
  (dolist (data data-list)
    (let ((faster (if (< (fifth data) (sixth data)) "Backtracking" "DFS")))
      (format t "~12a | ~5d | ~15d | ~9d | ~14,2f | ~8,2f | ~a~%"
              (first data) (second data) (third data) (fourth data)
              (fifth data) (sixth data) faster)))
  (format t "~%Conclusion: DFS with MRV heuristic dramatically reduces nodes expanded and is usually faster.~%~%"))

;; ===== 8. Test all levels =====

(defun test-sudoku ()
  (format t "~%========================================~%")
  (format t "  Sudoku Solver Test - Three Difficulty Levels~%")
  (format t "========================================~%~%")
  (let ((results (list
                  (test-puzzle-level "Easy" (create-easy-puzzle))
                  (test-puzzle-level "Medium" (create-medium-puzzle))
                  (test-puzzle-level "Hard" (create-hard-puzzle)))))
    (performance-summary results))
  (format t "~%========================================~%")
  (format t "  All tests completed~%")
  (format t "========================================~%~%"))

;; ===== 9. Interactive Menu =====

(defun display-menu ()
  (format t "~%========================================~%")
  (format t "    Sudoku Solver - Difficulty Selection~%")
  (format t "========================================~%")
  (format t "Please select difficulty level:~%")
  (format t "  1 - Easy~%")
  (format t "  2 - Medium~%")
  (format t "  3 - Hard~%")
  (format t "  4 - Test All Difficulties & Show Summary~%")
  (format t "  0 - Exit~%")
  (format t "========================================~%")
  (format t "Enter your choice (0-4): "))

(defun get-user-choice ()
  (let ((input (read)))
    (if (and (integerp input) (<= 0 input 4))
        input
        (progn
          (format t "~%Invalid input, please try again!~%")
          (get-user-choice)))))

(defun main-menu ()
  (loop
    (display-menu)
    (let ((choice (get-user-choice)))
      (case choice
        (1 (test-puzzle-level "Easy" (create-easy-puzzle)))
        (2 (test-puzzle-level "Medium" (create-medium-puzzle)))
        (3 (test-puzzle-level "Hard" (create-hard-puzzle)))
        (4 (test-sudoku))
        (0 (progn
             (format t "~%Thank you for using the Sudoku Solver! Goodbye!~%~%")
             (return)))
        (otherwise
         (format t "~%Invalid choice, please try again!~%"))))))

;; Start the program
(format t "~%")
(format t "======================================~%")
(format t "|    Welcome to Sudoku Solver        |~%")
(format t "|  Using Backtracking & DFS Methods  |~%")
(format t "======================================~%")

(main-menu)