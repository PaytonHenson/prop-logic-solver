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
