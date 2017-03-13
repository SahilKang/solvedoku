;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Copyright (C) 2017 Sahil Singh Kang <sahil.kang@asilaycomputing.com>
 ;
 ; This file is part of solvedoku.
 ;
 ; solvedoku is free software: you can redistribute it and/or modify
 ; it under the terms of the GNU Affero General Public License as published by
 ; the Free Software Foundation, either version 3 of the License, or
 ; (at your option) any later version.
 ;
 ; solvedoku is distributed in the hope that it will be useful,
 ; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ; GNU Affero General Public License for more details.
 ;
 ; You should have received a copy of the GNU Affero General Public License
 ; along with solvedoku.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax range
  (syntax-rules ()
    ((_ n m)
     (let loop ((x n) (y '()))
       (if (> x m) y
           (loop (+ x 1) (append y (list x))))))))

(define-syntax uniq
  (syntax-rules ()
    ((_ l)
     (if (null? l) '()
         (let loop ((x (car l)) (y (cdr l)) (acc '()))
           (cond ((null? y)
                  (append acc (list x)))
                 ((not (= x (car y)))
                  (loop (car y) (cdr y) (append acc (list x))))
                 (else
                  (loop x (cdr y) acc))))))))

(define-syntax 1d->2d
  (syntax-rules ()
    ((_ 1d)
     `#(,(quotient 1d 9) ,(modulo 1d 9)))))

(define-syntax 2d->1d
  (syntax-rules ()
    ((_ x y)
     (+ y (* x 9)))
    ((_ 2d)
     (2d->1d (vector-ref 2d 0) (vector-ref 2d 1)))))

(define-syntax 1d->row
  (syntax-rules ()
    ((_ 1d)
     (vector-ref (1d->2d 1d) 0))))

(define-syntax 1d->column
  (syntax-rules ()
    ((_ 1d)
     (vector-ref (1d->2d 1d) 1))))

(define-syntax 1d->box
  (syntax-rules ()
    ((_ 1d)
     (let ((2d (1d->2d 1d)))
       (+
        (* 3 (quotient (vector-ref 2d 0) 3))
        (quotient (vector-ref 2d 1) 3))))))

(define-syntax box->1d
  (syntax-rules ()
    ((_ box offset)
     (+
      (- (* 9 box)
         (* 6 (modulo box 3)))
      (* 9 (quotient offset 3))
      (modulo offset 3)))
    ((_ box)
     (box->1d box 0))))

(define-syntax get-row
  (syntax-rules ()
    ((_ puzzle row)
     (map
      (lambda (column)
        (vector-ref puzzle (2d->1d `#(,row ,column))))
      (range 0 8)))))

(define-syntax get-column
  (syntax-rules ()
    ((_ puzzle column)
     (map
      (lambda (row)
        (vector-ref puzzle (2d->1d `#(,row ,column))))
      (range 0 8)))))

(define-syntax get-box
  (syntax-rules ()
    ((_ puzzle box)
     (map
      (lambda (offset)
        (vector-ref puzzle (box->1d box offset)))
      (range 0 8)))))

(define-syntax get-branches
  (syntax-rules ()
    ((_ puzzle 1d)
     (if (not (= 0 (vector-ref puzzle 1d)))
         '()
         (let ((row (get-row puzzle (1d->row 1d)))
               (column (get-column puzzle (1d->column 1d)))
               (box (get-box puzzle (1d->box 1d))))
           (let ((baddies (uniq (sort (append row column box) <))))
             (filter (lambda (x) (not (memq x baddies))) (range 1 9))))))
    ((_ puzzle)
     (list->vector
      (map (lambda (1d) (get-branches puzzle 1d)) (range 0 80))))))

(define-syntax solved?
  (syntax-rules ()
    ((_ puzzle)
     (let ((valid (range 1 9)))
       (let loop ((i 0))
         (let ((row (sort (get-row puzzle i) <))
               (column (sort (get-column puzzle i) <))
               (box (sort (get-box puzzle i) <)))
           (if (not (and (equal? row valid)
                         (equal? column valid)
                         (equal? box valid)))
               #f
               (if (= i 8)
                   #t
                   (loop (+ i 1))))))))))

(define-syntax get-deductions
  (syntax-rules ()
    ((_ puzzle branches)
     (let loop ((i 0) (acc '()))
       (cond ((> i 80) acc)
             ((and (= 0 (vector-ref puzzle i))
                   (= 1 (length (vector-ref branches i))))
              (loop (+ i 1) (append acc (list i))))
             (else (loop (+ i 1) acc)))))))

(define-syntax apply-deductions
  (syntax-rules ()
    ((_ puzzle branches deductions)
     (if (null? deductions)
         puzzle
         (let ((puzzle (vector-copy puzzle)))
           (let loop ((x (car deductions)) (y (cdr deductions)))
             (vector-set! puzzle x (car (vector-ref branches x)))
             (if (null? y)
                 puzzle
                 (loop (car y) (cdr y)))))))))

(define-syntax harvest-fruit-old
  (syntax-rules ()
    ((_ puzzle branches branch fruit)
     (let ((branches (vector-copy branches))
           (row (1d->row branch))
           (column (1d->column branch))
           (box (1d->box branch)))
       (if (not (memq fruit (vector-ref branches branch)))
           (throw 'fuck "fruit is not in branch!")
           (vector-set! branches branch
                        (delete fruit (vector-ref branches branch))))
       (map
        (lambda (i)
          (if (= 0 (vector-ref puzzle (2d->1d row i)))
              (vector-set! branches (2d->1d row i)
                           (delete fruit
                                   (vector-ref branches (2d->1d row i))))
              '())
          (if (= 0 (vector-ref puzzle (2d->1d i column)))
              (vector-set! branches (2d->1d i column)
                           (delete fruit
                                   (vector-ref branches (2d->1d i column))))
              '())
          (if (= 0 (vector-ref puzzle (box->1d box i)))
              (vector-set! branches (box->1d box i)
                           (delete fruit
                                   (vector-ref branches (box->1d box i))))
              '()))
        (range 0 8))
       branches))
    ((_ puzzle branches deductions)
     (if (null? deductions)
         branches
         (let loop ((x (car deductions)) (y (cdr deductions)))
           (let ((fruits (vector-ref branches x)))
             (if (not (null? fruits))
                 (let ((fruit (car fruits)))
                   (set! branches (harvest-fruit puzzle branches x fruit)))
                 '())
             (if (null? y)
                 branches
                 (loop (car y) (cdr y)))))))))

(define-syntax harvest-fruit
  (syntax-rules ()
    ((_ puzzle branches branch fruit)
     (let ((branches (vector-copy branches))
           (row (1d->row branch))
           (column (1d->column branch))
           (box (1d->box branch)))
       (if (not (memq fruit (vector-ref branches branch)))
           (throw 'fuck "fruit is not in branch!")
           (vector-set! branches branch
                        (delete fruit (vector-ref branches branch))))
       (map
        (lambda (i)
          (if (= 0 (vector-ref puzzle (2d->1d row i)))
              (vector-set! branches (2d->1d row i)
                           (delete fruit
                                   (vector-ref branches (2d->1d row i))))
              '())
          (if (= 0 (vector-ref puzzle (2d->1d i column)))
              (vector-set! branches (2d->1d i column)
                           (delete fruit
                                   (vector-ref branches (2d->1d i column))))
              '())
          (if (= 0 (vector-ref puzzle (box->1d box i)))
              (vector-set! branches (box->1d box i)
                           (delete fruit
                                   (vector-ref branches (box->1d box i))))
              '()))
        (range 0 8))
       branches))
    ((_ puzzle branches deductions)
     (if (null? deductions)
         branches
         (let loop ((x (car deductions)) (y (cdr deductions)))
           (let ((fruits (vector-ref branches x)))
             (if (null? fruits)
                 '()
                 (begin
                   (let ((fruit (car fruits)))
                     (set! branches (harvest-fruit puzzle branches x fruit)))
                   (if (null? y)
                       branches
                       (loop (car y) (cdr y)))))))))))

(define-syntax get-guess
  (syntax-rules ()
    ((_ puzzle branches)
     (let loop ((i 0) (len 0) (ret '()))
       (cond ((> i 80)
              ret)
             ((and (= 0 (vector-ref puzzle i))
                   (< 0 (length (vector-ref branches i)))
                   (or (= 0 len)
                       (< (length (vector-ref branches i))
                          len)))
              (loop (+ i 1) (length (vector-ref branches i)) i))
             (else
              (loop (+ i 1) len ret)))))))

(define-syntax make-game-state
  (syntax-rules ()
    ((_ puzzle branches guess-tree)
     `#(,puzzle ,branches ,guess-tree))))

(define-syntax game-state->puzzle
  (syntax-rules ()
    ((_ game-state)
     (vector-ref game-state 0))))

(define-syntax game-state->branches
  (syntax-rules ()
    ((_ game-state)
     (vector-ref game-state 1))))

(define-syntax game-state->guess-tree
  (syntax-rules ()
    ((_ game-state)
     (vector-ref game-state 2))))

(define-syntax make-guess-branch
  (syntax-rules ()
    ((_ guess deductions)
     (cons guess deductions))))

(define-syntax guess-branch->guess
  (syntax-rules ()
    ((_ guess-branch)
     (car guess-branch))))

(define-syntax guess-branch->deductions
  (syntax-rules ()
    ((_ guess-branch)
     (cdr guess-branch))))

(define-syntax add-deductions-to-guess-tree
  (syntax-rules ()
    ((_ guess-tree deductions)
     (if (null? guess-tree)
         '()
         (let ((guess-branch (car guess-tree)))
           (let ((guess (guess-branch->guess guess-branch))
                 (cur-ded (guess-branch->deductions guess-branch)))
             (let ((new-guess-branch
                    (make-guess-branch guess (append cur-ded deductions))))
               (cons new-guess-branch (cdr guess-tree)))))))))

(define-syntax deduce-old
  (syntax-rules ()
    ((_ game-state)
     (let ((puzzle (game-state->puzzle game-state))
           (branches (game-state->branches game-state))
           (guess-tree (game-state->guess-tree game-state)))
       (let ((deductions (get-deductions puzzle branches)))
         (if (= 0 (length deductions))
             '()
             (let ((puzzle (apply-deductions puzzle branches deductions))
                   (branches (harvest-fruit puzzle branches deductions))
                   (guess-tree
                    (add-deductions-to-guess-tree guess-tree deductions)))
               (make-game-state puzzle branches guess-tree))))))))

(define-syntax deduce
  (syntax-rules ()
    ((_ game-state)
     (let ((puzzle (game-state->puzzle game-state))
           (branches (game-state->branches game-state))
           (guess-tree (game-state->guess-tree game-state)))
       (let ((deductions (get-deductions puzzle branches)))
         (if (null? deductions)
             '()
             (let ((puzzle (apply-deductions puzzle branches deductions))
                   (branches (harvest-fruit puzzle branches deductions))
                   (guess-tree
                    (add-deductions-to-guess-tree guess-tree deductions)))
               (if (null? branches)
                   #f
                   (make-game-state puzzle branches guess-tree)))))))))

(define-syntax guess
  (syntax-rules ()
    ((_ game-state)
     (let ((puzzle (game-state->puzzle game-state))
           (branches (game-state->branches game-state))
           (guess-tree (game-state->guess-tree game-state)))
       (let ((guess (get-guess puzzle branches)))
         (if (null? guess)
             '()
             (let ((guess-tree (cons (make-guess-branch guess '())
                                     guess-tree))
                   (fruit (car (vector-ref branches guess))))
               (let ((branches (harvest-fruit puzzle branches guess fruit))
                     (puzzle (vector-copy puzzle)))
                 (vector-set! puzzle guess fruit)
                 (make-game-state puzzle branches guess-tree)))))))))

(define-syntax clear-puzzle-entries
  (syntax-rules ()
    ((_ puzzle entries)
     (if (null? entries)
         puzzle
         (let ((puzzle (vector-copy puzzle)))
           (let loop ((x (car entries)) (y (cdr entries)))
             (vector-set! puzzle x 0)
             (if (null? y)
                 puzzle
                 (loop (car y) (cdr y)))))))))

(define-syntax rebranch
  (syntax-rules ()
    ((_ puzzle branches)
     (let ((branches branches)
           (new-branches (get-branches puzzle)))
       (let loop ((i 0))
         (if (not (null? (vector-ref new-branches i)))
             (vector-set! branches i (vector-ref new-branches i))
             '())
         (if (= 80 i)
             branches
             (loop (+ i 1))))))))

(define-syntax get-next-fruit
  (syntax-rules ()
    ((_ branches branch)
     (let ((fruits (vector-ref branches branch)))
       (if (null? fruits)
           '()
           (car fruits))))))

(define-syntax backtrack
  (syntax-rules ()
    ((_ game-state)
     (let ((puzzle (game-state->puzzle game-state))
           (branches (game-state->branches game-state))
           (guess-tree (game-state->guess-tree game-state)))
       (if (null? guess-tree)
           '()
           (let loop ((prev-guess-branch (car guess-tree))
                      (rest-guess-branch (cdr guess-tree))
                      (puzzle puzzle))
             (let ((guess (guess-branch->guess prev-guess-branch))
                   (deductions (guess-branch->deductions prev-guess-branch)))
               (let ((puzzle (clear-puzzle-entries puzzle deductions))
                     (fruit (get-next-fruit branches guess)))
                 (if (not (null? fruit))
                     (begin
                       (vector-set! puzzle guess fruit)
                       (set! branches (harvest-fruit
                                       puzzle branches guess fruit))
                       (set! branches (rebranch puzzle branches))
                       (set! guess-tree (cons (make-guess-branch guess '())
                                              rest-guess-branch))
                       (make-game-state puzzle branches guess-tree))
                     (begin
                       (vector-set! puzzle guess 0)
                       (if (null? rest-guess-branch)
                           '()
                           (loop (car rest-guess-branch)
                                 (cdr rest-guess-branch)
                                 puzzle))))))))))))

(define-syntax solve-old
  (syntax-rules ()
    ((_ puzzle)
     (let solve-loop ((game-state (make-game-state
                                   puzzle (get-branches puzzle) '())))
       (let deduce-loop ((deduced-state (deduce game-state)))
         (if (null? deduced-state)
             '()
             (begin
               (set! game-state deduced-state)
               (deduce-loop (deduce game-state)))))
       (if (solved? (game-state->puzzle game-state))
           (game-state->puzzle game-state)
           (let ((guessed (guess game-state)))
             (if (null? guessed)
                 (let ((backtracked (backtrack game-state)))
                   (if (null? backtracked)
                       '()
                       (solve-loop backtracked)))
                 (solve-loop guessed))))))))

(define-syntax solve
  (syntax-rules ()
    ((_ puzzle)
     (let ((collision? #f))
       (let solve-loop ((game-state (make-game-state
                                     puzzle (get-branches puzzle) '())))
         (set! collision? #f)
         (let deduce-loop ((deduced-state (deduce game-state)))
           (cond ((null? deduced-state)
                  '())
                 ((eq? #f deduced-state)
                  (set! collision? #t))
                 (else
                  (set! game-state deduced-state)
                  (deduce-loop (deduce game-state)))))
         (cond (collision?
                (let ((backtracked (backtrack game-state)))
                  (if (null? backtracked)
                      '()
                      (solve-loop backtracked))))
               ((solved? (game-state->puzzle game-state))
                (game-state->puzzle game-state))
               (else
                (let ((guessed (guess game-state)))
                  (if (null? guessed)
                      (let ((backtracked (backtrack game-state)))
                        (if (null? backtracked)
                            '()
                            (solve-loop backtracked)))
                      (solve-loop guessed))))))))))
