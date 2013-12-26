;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gate-compression.lisp
#|

c) 1999-2004, Lee Spector (lspector@hampshire.edu)

This code augments the Common Lisp version of the QGAME (Quantum Gate And
Measurement Emulator) with the MATRIX-GATE gate form and functions for 
compressing a sequence of gates into single MATRIX-GATE form that applies
the unitary matrix built from the elements of the sequence. Also included
is a function for checking the unitarity of a matrix -- this is used to
ensure that round-off errors during compression do not produce physicially
impossible results.

Example calls are provided at the end of this file.

QGAME and related documentation is distributed from:

  http://hampshire.edu/lspector/qgame.html

See also:

  Spector, Lee. 2004. Automatic Quantum Computer Programming: A Genetic 
  Programming Approach. Boston/Dordrecht/New York/London: Kluwer Academic 
  Publishers.

Version history:
Original sources from qc-matrices/matrices.lisp 
Oct 5 1999, made compatible with limited-oracle
Nov 12 2003, many updates, disentangled from LGP
May 26 2004, cosmetic changes for distribution

|#

;; compiler optimization settings

; for debugging
; (eval-when (compile)
;   (declaim (optimize (speed 2) (safety 1) (space 1) (debug 3))))

; for maximum reasonably safe speed
(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

(defparameter *max-depth-for-compression* 5)
(defparameter *max-depth-for-history* 5)
(defparameter *uncompressible* (list 'oracle 'limited-oracle 'measure 'end 'halt))
(defvar *NUMBER-OF-QUBITS*)
(defvar *ALL-QUBITS*)

(defun matrix-matrix-multiply (matrix1 matrix2)
  "Returns the result of multiplying (matrix1 X matrix2), where all
matrices are implemented as square arrays."
  (let* ((matrix-size (car (array-dimensions matrix1)))
         (result (make-array (list matrix-size matrix-size))))
    (dotimes (i matrix-size)
      (dotimes (j matrix-size)
        (setf (aref result i j)
              (let ((element 0))
                (dotimes (k matrix-size)
                  (incf element
                        (* (aref matrix1 i k)
                           (aref matrix2 k j))))
                element))))
    result))

(defun expand-matrix (gate targets)
  "Expands the operator matrix gate to a full matrix for operating
on a system of *number-of-qubits* qubits, with the operator being applied
to the qubits specified in targets. Written by Lee Spector, 1999.
Targets reversal added Sept 8, 1999."
  (let* ((targets (reverse targets))
         (m-size (expt 2 *number-of-qubits*))
         (m (make-array (list m-size m-size)))
         (non-targets (loop for i from 0 to (- *number-of-qubits* 1)
                            unless (member i targets)
                            collect i)))
    (dotimes (i m-size)
      (dotimes (j m-size)
        (setf (aref m i j)
              (if (=in-positions non-targets i j)
                (aref gate
                      (extract@positions targets i)
                      (extract@positions targets j))
                0))))
    m))

(defun =in-positions (positions int1 int2)
  "Returns non-nil if int1 and int2 are have the same bits at all
positions in positions. Written by Lee Spector, 1999."
  (every #'(lambda (index)
             (eq (logbitp index int1)
                 (logbitp index int2)))
         positions))

(defun extract@positions (positions int)
  "Returns the number formed by extracting and concatenating 
the bits of int indexed by the positions in positions.
Written by Lee Spector, 1999."
  (let ((exponent -1))
    (loop for index in positions
          do (incf exponent)
          when (logbitp index int)
          sum (expt 2 exponent))))

(defun long (n)
  "Returns the number n coerced to a long float."
  (coerce n 'long-float))

(defun expand-gate-form (gate-form)
  "Returns the expanded matrix corresponding to the provided
gate-form, which should conform to the QGAME interface syntax."
  (case (first gate-form)
    (qnot (expand-matrix #2A((0 1) (1 0)) (cdr gate-form)))
    (cnot (expand-matrix #2A((1 0 0 0)
                             (0 1 0 0)
                             (0 0 0 1)
                             (0 0 1 0))
                         (cdr gate-form)))
    (swap (expand-matrix #2A((1 0 0 0)
                             (0 0 1 0)
                             (0 1 0 0)
                             (0 0 0 1))
                         (cdr gate-form)))
    (hadamard (expand-matrix (make-array 
                              '(2 2)
                              :initial-contents 
                              (list (list (/ 1 (sqrt 2.0L0))  (/ 1 (sqrt 2.0L0)))
                                    (list (/ 1 (sqrt 2.0L0))  (- (/ 1 (sqrt 2.0L0))))))
                             (cdr gate-form)))
    (srn (expand-matrix (make-array 
                         '(2 2)
                         :initial-contents 
                         (list (list (/ 1 (sqrt 2.0L0))  (- (/ 1 (sqrt 2.0L0))))
                               (list (/ 1 (sqrt 2.0L0))  (/ 1 (sqrt 2.0L0)))
                               ))
                        (cdr gate-form)))
    (u-theta (expand-matrix (let ((theta (long (third gate-form))))
                              (make-array '(2 2)
                                          :initial-contents 
                                          (list (list (cos theta)  (sin theta))
                                                (list (- (sin theta))  (cos theta)))))
                            (list (second gate-form))))
    (cphase (expand-matrix (let ((alpha (long (fourth gate-form))))
                             (make-array '(4 4)
                                         :initial-contents 
                                         (list (list 1 0 0 0)
                                               (list 0 1 0 0)
                                               (list 0 0 1 0)
                                               (list 0 0 0 (exp (* (sqrt -1) alpha))))))
                           (list (second gate-form) (third gate-form))))
    (u2 (expand-matrix (let ((phi (long (third gate-form)))
                             (theta (long (fourth gate-form)))
                             (psi (long (fifth gate-form)))
                             (alpha (long (sixth gate-form)))
                             (i (sqrt -1)))
                         (make-array
                          '(2 2)
                          :initial-contents 
                          (list (list (* (exp (* i (+ (- phi) (- psi) alpha))) (cos theta))
                                      (* (exp (* i (+ (- phi) psi alpha))) (sin (- theta))))
                                (list (* (exp (* i (+ phi (- psi) alpha))) (sin theta))
                                      (* (exp (* i (+ phi psi alpha))) (cos theta))))))
                       (list (second gate-form))))
    (matrix-gate (second gate-form))))

(defun matrix-gate (qsys matrix history)
  "Implements the MATRIX-GATE gate form; applies the given matrix to
the given quantum system."
  (declare (ignore history))
  (apply-operator qsys
                  matrix
                  (reverse *All-Qubits*)))

(defun max-depth (tree)
  "Returns the maximum depth of the given tree."
  (if (not (listp tree))
    0
    (1+ (apply #'max (mapcar #'max-depth tree)))))
        
(defun process-for-history (gate-sequence)
  "Removes actual matrices from histories containing matrix-gate forms, 
substituting a COMPRESSED form with only the prior history. Punts and 
returns TOO-DEEP if the depth is greater than *max-depth-for-history*."
  (if (> (max-depth gate-sequence) *max-depth-for-history*)
    'too-deep
    (mapcar #'(lambda (gate-form)
                (if (eq (car gate-form) 'matrix-gate)
                  (list 'compressed (third gate-form))
                  gate-form))
            gate-sequence)))

(defun compress-compressible-gate-sequence (seq)
  "Compresses a sequence of gate forms into a single MATRIX-GATE form.
Assumes all gates can be expanded. Returns seq if the check for unitarity
fails for the compression result."
  (cond ((<= (length seq) 1) ;; don't compress a single gate
         seq)
        ((> (max-depth seq) *max-depth-for-compression*)
         seq)
        (t (let ((composite-matrix (expand-matrix #2A((1 0)(0 1)) nil)))
             ;; start with identity
             (dolist (gate-form seq)
               (setq composite-matrix
                     (matrix-matrix-multiply (expand-gate-form gate-form)
                                             composite-matrix)))
             (if (> (check-unitarity composite-matrix) 1.0E-10)
               ;; errors too high
               seq
               ;; errors OK
               (list (list 'matrix-gate
                           composite-matrix
                           (process-for-history seq))))))))

(defun thoroughly-compress-compressible-gate-sequence (seq)
  "Just like compress-compressible-gate-sequence except compresses even
single gates into matrix-gates."
  (cond ((> (max-depth seq) *max-depth-for-compression*)
         seq)
        (t (let ((composite-matrix (expand-matrix #2A((1 0)(0 1)) nil)))
             ;; start with identity
             (dolist (gate-form seq)
               (setq composite-matrix
                     (matrix-matrix-multiply (expand-gate-form gate-form)
                                             composite-matrix)))
             (if (> (check-unitarity composite-matrix) 1.0E-10)
               ;; errors too high
               seq
               ;; errors OK
               (list (list 'matrix-gate
                           composite-matrix
                           (process-for-history seq))))))))

(defun compress-gates (program &optional (pending nil))
  "Returns a version of the given program in which all compressible
sequences of gates are compressed into MATRIX-GATE forms. Leaves
single-gate sequences unchanged."
  (cond ((null program)
         (if pending
           (compress-compressible-gate-sequence pending)
           nil))
        ((member (caar program) *uncompressible*)
         (if pending
           (append (compress-compressible-gate-sequence pending)
                   (cons (car program) (compress-gates (cdr program))))
           (cons (car program) (compress-gates (cdr program)))))
        (t (compress-gates (cdr program) 
                           (append pending (list (car program)))))))

(defun thoroughly-compress-gates (program &optional (pending nil))
  "Returns a version of the given program in which all compressible
sequences of gates are compressed into MATRIX-GATE forms. Unlike 
COMPRESS-GATES, this function converts even single-gate sequences
into MATRIX-GATE forms."
  (cond ((null program)
         (if pending
           (thoroughly-compress-compressible-gate-sequence pending)
           nil))
        ((member (caar program) *uncompressible*)
         (if pending
           (append (thoroughly-compress-compressible-gate-sequence pending)
                   (cons (car program) (thoroughly-compress-gates (cdr program))))
           (cons (car program) (thoroughly-compress-gates (cdr program)))))
        (t (thoroughly-compress-gates (cdr program) 
                                      (append pending (list (car program)))))))

(defun check-unitarity (m)
  "Returns the cumulative difference of each element of (m times m*) (where
m* is the conjugate transpose of m) from the corresponding element of the
identity matrix. This will be 0 for a unitary matrix."
  (let* ((dim (car (array-dimensions m)))
         (m* (make-array (list dim dim))) ;; conjugate transpose of m
         (identity (make-array (list dim dim)))
         (product nil)
         (cumulative-error 0))
    ;; set up identity matrix
    (dotimes (i dim)
      (dotimes (j dim)
        (setf (aref identity i j) (if (= i j) 1 0))))
    ;; set up m*
    (dotimes (i dim)
      (dotimes (j dim)
        (setf (aref m* i j) 
              (conjugate (aref m j i)))))
    ;; multiply
    (setq product (matrix-matrix-multiply m m*))
    ;(print product)
    ;; sum error
    (dotimes (i dim)
      (dotimes (j dim)
        (incf cumulative-error
              (abs (- (aref product i j)
                      (aref identity i j))))))
    cumulative-error))


#|

EXAMPLES

;; this must be set prior to any actual compressions
(setq *NUMBER-OF-QUBITS* 3)

;; this will do nothing because no compression of a single gate is possible
(compress-gates '((hadamard 0)))

;; this will "compress" the single gate into a matrix gate form anyway
(thoroughly-compress-gates '((hadamard 0)))

;; compression of two hadamards
(compress-gates '((hadamard 0) (hadamard 1)))

;; something more complex
(compress-gates 
 '((hadamard 0) (hadamard 1) (cnot 1 2) (u-theta 1 0.12345)))

;; Check the unitarity of the matrix from the above call (which is
;; the second item in the first form in the resulting sequence);
;; this will be close to zero for a unitary matrix.
(check-unitarity
 (second (first
          (compress-gates 
           '((hadamard 0) (hadamard 1) (cnot 1 2) (u-theta 1 0.12345))))))

|#

;; EOF
