;; Copyright 1994, Brown University, Providence, RI
;; See end of file for full copyright information

;; (in-package 'user)

;; Matching from the logic chapter.

(defun is-VAR (x) (and (consp x) (eq (first x) '?)))

;; Determine if a constant and a pattern match.
(defun match (const pat)
  (aux-match const pat '((match t))))

;; Keep track of the bindings established so far.
(defun aux-match (const pat bdgs)
  (cond ((not bdgs) nil)
        ((atom pat) (if (eq pat const) bdgs nil))
        ;; If the pattern is a bound variable, match the
        ;; constant and whatever the variable is bound to.
        ;; If the pattern is a variable and not bound,
        ;; then bind the variable to the constant.
        ((is-VAR pat)
         (let ((bdg (assoc pat bdgs :test #'equal)))
           (cond (bdg (aux-match const (second bdg) bdgs))
                 (t (cons (list pat const) bdgs)))))
        ((or (atom const) (null const)) nil)
        (t (aux-match (first const)
                      (first pat)
                      (aux-match (rest const)
                                 (rest pat) bdgs)))))

;; Test MATCH
(defun test ()
  (and (match '(loves (dog fred) fred) '(loves (? x) (? y)))
       (match '(loves (dog fred) mary) '(loves (dog (? x)) (? y)))
       (not (match '(loves (dog fred) fred) '(loves (? x) (? x))))
       (not (match '(loves (dog fred) mary) '(loves (dog (? x)) (? x))))))


;; Copyright 1994, Brown University, Providence, RI
;; Permission to use and modify this software and its documentation
;; for any purpose other than its incorporation into a commercial
;; product is hereby granted without fee.  Permission to copy and
;; distribute this software and its documentation only for
;; non-commercial use is also granted without fee, provided, however
;; that the above copyright notice appear in all copies, that both
;; that copyright notice and this permission notice appear in
;; supporting documentation, that the name of Brown University not
;; be used in advertising or publicity pertaining to distribution
;; of the software without specific, written prior permission, and
;; that the person doing the distribution notify Brown University
;; of such distributions outside of his or her organization. Brown
;; University makes no representations about the suitability of this
;; software for any purpose. It is provided "as is" without express
;; or implied warranty.  Brown University requests notification of
;; any modifications to this software or its documentation.
;;
;; Send the following redistribution information
;;
;;   Name:
;;   Organization:
;;   Address (postal and/or electronic):
;;
;; To:
;;   Software Librarian
;;   Computer Science Department, Box 1910
;;   Brown University
;;   Providence, RI 02912
;;
;;     or
;;
;;   brusd@cs.brown.edu
;;
;; We will acknowledge all electronic notifications.


(defun rewrite-conds (formula)
  (cond
    ((atom formula) formula)
    ((= 1 (length formula)) formula)
    (t (let ((bindings (match formula '(cond (? x) (? y)))))
         (cond ((not (null bindings))
                (let ((phi (rewrite-conds (second (first bindings))))
                      (psi (rewrite-conds (second (second bindings)))))
                  (list 'or (list 'not phi) psi)))
               ((eq 'not (car formula))
                (list (car formula) (rewrite-conds (second formula))))
               (t (list (car formula)
                        (rewrite-conds (second formula))
                        (rewrite-conds (third formula)))))))))

(defun rewrite-not-not (formula)
  (cond
    ((atom formula) formula)
    ((= 1 (length formula)) formula)
    (t (let ((bindings (match formula '(not (not (? x))))))
         (cond ((not (null bindings))
                (rewrite-not-not (second (first bindings))))
               ((eq 'not (car formula))
                (list (car formula) (rewrite-not-not (second formula))))
               (t (list (car formula)
                        (rewrite-not-not (second formula))
                        (rewrite-not-not (third formula)))))))))

(defun rewrite-not-and (formula)
  (cond
    ((atom formula) formula)
    ((= 1 (length formula)) formula)
    (t (let ((bindings (match formula '(not (and (? x) (? y))))))
         (cond ((not (null bindings))
                (let ((phi (rewrite-not-and (second (first bindings))))
                      (psi (rewrite-not-and (second (second bindings)))))
                  (list 'or (list 'not phi) (list 'not psi))))
               ((eq 'not (car formula))
                (list (car formula) (rewrite-not-and (second formula))))
               (t (list (car formula)
                        (rewrite-not-and (second formula))
                        (rewrite-not-and (third formula)))))))))

(defun rewrite-not-or (formula)
  (cond
    ((atom formula) formula)
    ((= 1 (length formula)) formula)
    (t (let ((bindings (match formula '(not (or (? x) (? y))))))
         (cond ((not (null bindings))
                (let ((phi (rewrite-not-or (second (first bindings))))
                      (psi (rewrite-not-or (second (second bindings)))))
                  (list 'and (list 'not phi) (list 'not psi))))
               ((eq 'not (car formula))
                (list (car formula) (rewrite-not-or (second formula))))
               (t (list (car formula)
                        (rewrite-not-or (second formula))
                        (rewrite-not-or (third formula)))))))))

(defun dist1 (formula)
  (cond
    ((atom formula) formula)
    ((= 1 (length formula)) formula)
    (t (let ((bindings (match formula '(or (? x) (and (? y) (? z))))))
         (cond ((not (null bindings))
                (let ((x (dist1 (second (first bindings))))
                      (y (dist1 (second (second bindings))))
                      (z (dist1 (second (third bindings)))))
                  (list 'and (list 'or x y) (list 'or x z))))
               ((eq 'not (car formula))
                (list (car formula) (dist1 (second formula))))
               (t (list (car formula)
                        (dist1 (second formula))
                        (dist1 (third formula)))))))))

(defun dist2 (formula)
  (cond
    ((atom formula) formula)
    ((= 1 (length formula)) formula)
    (t (let ((bindings (match formula '(or (and (? x) (? y)) (? z)))))
         (cond ((not (null bindings))
                (let ((x (dist2 (second (first bindings))))
                      (y (dist2 (second (second bindings))))
                      (z (dist2 (second (third bindings)))))
                  (list 'and (list 'or z x) (list 'or z y))))
               ((eq 'not (car formula))
                (list (car formula) (dist2 (second formula))))
               (t (list (car formula)
                        (dist2 (second formula))
                        (dist2 (third formula)))))))))

(defun distribute (formula)
  (do* ((start-value formula end-value)
        (end-value (dist2 (dist1 start-value)) (dist2 (dist1 start-value))))
       ((equal start-value end-value) end-value)))

(defun move-nots (formula)
 (do* ((start-value formula end-value)
       (end-value (rewrite-not-or (rewrite-not-and (rewrite-not-not start-value)))
                  (rewrite-not-or (rewrite-not-and (rewrite-not-not start-value)))))
      ((equal start-value end-value) end-value)))

(defun convert-to-clausal-form (formula)
  (cond
    ((= 1 (length formula)) (list formula))
    ((eq 'not (car formula)) (list formula))
    (t (let ((bindings-and (match formula '(and (? x) (? y))))
             (bindings-or (match formula '(or (? x) (? y)))))
         (cond ((not (null bindings-and)) (ccf-and formula))
               ((not (null bindings-or)) (list (ccf-or formula))))))))

(defun ccf-and (formula)
 (cond
   ((atom formula) (list (list formula)))
   ((eq 'not (car formula)) (list (list formula)))
   (t (let ((bindings-and (match formula '(and (? x) (? y))))
            (bindings-or (match formula '(or (? x) (? y)))))
        (cond ((not (null bindings-and))
               (let ((phi (ccf-and (second (first bindings-and))))
                     (psi (ccf-and (second (second bindings-and)))))
                 (append phi psi)))
              ((not (null bindings-or)) (list (ccf-or formula))))))))

;;I know there won't be any ands
(defun ccf-or (formula)
  (cond
    ((atom formula) (list formula))
    ((eq 'not (car formula)) (list formula))
    (t (let ((bindings (match formula '(or (? x) (? y)))))
         (cond ((not (null bindings))
                (let ((phi (ccf-or (second (first bindings))))
                      (psi (ccf-or (second (second bindings)))))
                  (append phi psi))))))))

(defun rewrite-formula (formula)
  (convert-to-clausal-form
    (distribute
      (move-nots
        (rewrite-conds formula)))))

(defun rewrite-formulas (formulas)
  (let (clauses)
    (dolist (formula formulas clauses)
      (setf clauses (append clauses (rewrite-formula formula))))))

(defun negate (literal)
  (cond
    ((atom literal) (list 'not literal))
    ((eq 'not (car literal)) (second literal))))

(defun resolve (clause1 clause2)
  (let ((resolvent)
        (resolved nil))
    (dolist (literal1 clause1 'fail)
      (dolist (literal2 clause2)
        (when (equal (negate literal1) literal2)
          (setf resolvent (append (remove literal1 clause1) (remove literal2 clause2)))
          (setf resolved t)
          (return)))
      (if resolved (return resolvent)))))

(defun prove (clauses)
    (do* ((proved nil)
          (working-clauses clauses)
          (i 1 (+ i 1))
          (clause1 (nth i working-clauses) (nth i working-clauses)))
         ((or (null clause1) proved) proved)
      (dotimes (j i)
        (let* ((clause2 (nth j working-clauses))
               (resolvent (resolve clause1 clause2)))
          (cond
            ((null resolvent) (setf proved t))
            ((not (eq resolvent 'fail)) (setf working-clauses (append working-clauses (list resolvent)))))))))
