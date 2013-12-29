
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qgame.lisp
;; c) 1999-2004, Lee Spector (lspector@hampshire.edu)
;; version 1.20031226 (major version number.yyyymmdd)
;; version history below

(in-package :qgame)

; for maximum reasonably safe speed
(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class definition for a quantum system

(defclass quantum-system ()
  (;; the number of qubits in the system
   (number-of-qubits :accessor number-of-qubits :initarg :number-of-qubits)
   ;; an array of amplitudes
   (amplitudes :accessor amplitudes :initarg :amplitudes :initform nil)
   ;; the probability for having reached this system in the first place
   (prior-probability :accessor prior-probability :initarg :prior-probability 
                      :initform 1)
   ;; the number of oracle calls that have been made in the history of this system
   (oracle-count :accessor oracle-count :initarg :oracle-count :initform 0)
   ;; a list of measurements and their results in the history of this system
   (measurement-history :accessor measurement-history :initarg :measurement-history
                        :initform nil)
   ;; a list of all instructions executed in the history of this system
   (instruction-history :accessor instruction-history :initarg :instruction-history
                        :initform nil)
   ;; the program yet to be executed by this system (if it hasn't yet terminated)
   (program :accessor program :initarg :program :initform nil)
   ;; the following are just for convenience
   (qubit-numbers :accessor qubit-numbers)         ;; all valid qubit indices
   (amplitude-address :accessor amplitude-address) ;; used for looping through qubits
   ))

(defmethod initialize-instance :after ((qsys quantum-system) &rest args)
  "An initializer for quantum systems."
  (declare (ignore args))
  (let ((num-qubits (number-of-qubits qsys)))
    ;; if there are no amplitudes yet then initialize to |00...0>
    (unless (amplitudes qsys)
      (setf (amplitudes qsys)
            (let ((amps (make-array (expt 2 num-qubits) 
                                    :initial-element 0.0L0)))
              (setf (aref amps 0) 1.0L0) ;; start in zero state
              amps)))
    ;; initilize list of valid qubit indices
    (setf (qubit-numbers qsys)
          (let ((all nil))
            (dotimes (i num-qubits) (push i all))
            (reverse all)))
    ;; initialize address register for amplitudes
    (setf (amplitude-address qsys)
          (make-array num-qubits :initial-element 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quantum computer manipulation utilities

(defun set-address-components (qsys count qubits)
  "Sets (amplitude-address qsys) to refer to a particular amplitude, as
indicated by the bits in the integer count."
  (dotimes (i (length qubits))
    (setf (aref (amplitude-address qsys) (nth i qubits))
          (if (logbitp i count) 1 0))))
               
(defun map-qubit-combinations (qsys function qubits)
  "Calls function once for each of the 1/0 combinations of the provided
qubits, with the right-most qubit varying the fastest."
  (setq qubits (reverse qubits))
  (let ((number-of-iterations (expt 2 (length qubits))))
    (dotimes (i number-of-iterations)
      (set-address-components qsys i qubits)
      (funcall function))))

(defun get-addressed-amplitude (qsys)
  "Returns the amplitude currently addressed by (amplitude-address qsys)"
  (let ((numerical-address 0))
    (dotimes (i (number-of-qubits qsys))
      (unless (zerop (aref (amplitude-address qsys) i))
        (incf numerical-address (expt 2 i))))
    (aref (amplitudes qsys) numerical-address)))

(defun set-addressed-amplitude (qsys new-value)
  "Sets the amplitude currently addressed by (amplitude-address qsys) 
to new-value."
  (let ((numerical-address 0))
    (dotimes (i (number-of-qubits qsys))
      (unless (zerop (aref (amplitude-address qsys) i))
        (incf numerical-address (expt 2 i))))
    (setf (aref (amplitudes qsys) numerical-address) new-value)))

(defun matrix-multiply (matrix column)
  "Multiplies the given square matrix by the given column (assumed
to be the right length) and returns the resulting column."
  (let ((matrix-size (car (array-dimensions matrix)))
        (result nil))
    (dotimes (i matrix-size)
      (push (let ((element 0))
              (dotimes (j matrix-size)
                (incf element (* (aref matrix i j) (nth j column))))
              element)
            result))
    (reverse result)))

(defun extract-column (qsys qubits-to-vary)
  "Returns a column from the amplitudes obtained by varying the listed
qubits, with the right-most qubit varying the fastest."
  (let ((col nil))
    (map-qubit-combinations 
     qsys
     #'(lambda () 
         (push (get-addressed-amplitude qsys) col))
     qubits-to-vary)
    (reverse col)))

(defun install-column (qsys column qubits-to-vary)
  "Installs the given column in the amplitude positions obtained by
varying the listed qubits, with the right-most qubit varying the fastest."
  (map-qubit-combinations 
   qsys
   #'(lambda () 
       (set-addressed-amplitude qsys (car column))
       (setq column (cdr column)))
   qubits-to-vary))

(defun apply-operator (qsys operator qubits)
  "Applies the given matrix-form operator to the given qubits."
  (map-qubit-combinations
   qsys
   #'(lambda ()
       ;(format t "~%address:~A" (amplitude-address qsys))
       (let* ((pre-column (extract-column qsys qubits))
              (post-column (matrix-multiply operator pre-column)))
         (install-column qsys post-column qubits)))
   (set-difference (qubit-numbers qsys) qubits))
  qsys)

(defun qc-output-probabilities (qsys qubits)
  "Returns a list of the probabilities for all combinations for the
given qubits, in binary order with the rightmost qubit varying fastest."
  (let ((probabilities nil)
        (other-qubits (set-difference (qubit-numbers qsys) qubits)))
    (map-qubit-combinations
     qsys
     #'(lambda ()
         (push (let ((probability 0))
                 (map-qubit-combinations
                  qsys
                  #'(lambda ()
                      (incf probability 
                            (expt (abs (get-addressed-amplitude qsys)) 2)))
                  other-qubits)
                 probability)
               probabilities))
     qubits)
    (reverse probabilities)))

(defun multi-qsys-output-probabilities (qsys-list qubits)
  "Returns a list of the probabilities for all combinations for the
given qubits, in binary order with the rightmost qubit varying fastest.
This function takes a LIST of quantum systems as input and sums the
results across all systems."
  (let ((probabilities
         (mapcar #'(lambda (qsys) 
                     (qc-output-probabilities qsys qubits))
                 qsys-list)))
    (labels ((add-lists (l1 l2)
               (if (null l1) 
                 nil
                 (cons (+ (first l1) (first l2))
                       (add-lists (rest l1) (rest l2))))))
      (reduce #'add-lists probabilities))))


(defun expected-oracles (qsys-list)
  "Returns the expected number of oracle calls for the given
set of quantum systems."
  (reduce #'+
          (mapcar #'(lambda (qsys)
                      (* (prior-probability qsys)
                         (oracle-count qsys)))
                  qsys-list)))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; oracle gates

(defun binary-operator-matrix (tt-right-column)
  "Returns a matrix operator for a binary function with the
given tt-right-column as the right column of its truth table."
  (let* ((column-length (length tt-right-column))
         (operator-size (* 2 column-length))
         (matrix (make-array (list operator-size operator-size)
                             :initial-element 0)))
    (dotimes (i column-length)
      (let ((offset (* i 2)))
        (if (zerop (nth i tt-right-column))
          (setf (aref matrix offset offset) 1
                (aref matrix (1+ offset) (1+ offset)) 1)
          (setf (aref matrix offset (1+ offset)) 1
                (aref matrix (1+ offset) offset) 1))))
    matrix))

(defun oracle (qsys tt-right-column &rest qubits)
  "Applies the oracle operator built from tt-right-column, which
is the right column of the corresponding truth table."
  (incf (oracle-count qsys))
  (apply-operator
   qsys
   (binary-operator-matrix tt-right-column)
   qubits))

(defun limited-oracle (qsys max-calls tt-right-column &rest qubits)
  "If (oracle-count qsys) is less than max-calls then this applies 
the oracle operator built from tt-right-column, which is the right 
column of the corresponding truth table. Otherwise this does nothing."
  (if (< (oracle-count qsys) max-calls)
    (progn (incf (oracle-count qsys))
           (apply-operator
            qsys
            (binary-operator-matrix tt-right-column)
            qubits))
    qsys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other quantum gates

(defun qnot (qsys q)
  "Quantum NOT gate"
  (apply-operator qsys 
                  #2A((0 1)
                      (1 0))
                  (list q)))
  

(defun cnot (qsys q1 q2)
  "Quantum Controlled NOT gate"
  (apply-operator qsys 
                  #2A((1 0 0 0)
                      (0 1 0 0)
                      (0 0 0 1)
                      (0 0 1 0))
                  (list q1 q2)))

(defun srn (qsys q)
  "Quantum Square-Root-of-NOT gate"
  (apply-operator
   qsys 
   (make-array '(2 2)
               :initial-contents 
               (list (list (/ 1 (sqrt 2.0L0))  (- (/ 1 (sqrt 2.0L0))))
                     (list (/ 1 (sqrt 2.0L0))  (/ 1 (sqrt 2.0L0)))
                     ))
   (list q)))

(defun nand (qsys q1 q2 q3) 
  "Quantum NAND gate"
  (apply-operator
   qsys 
   (binary-operator-matrix '(1 1 1 0))
   (list q1 q2 q3)))

(defun hadamard (qsys q)
  "Quantum Hadamard gate"
  (apply-operator
   qsys 
   (make-array '(2 2)
              :initial-contents 
              (list (list (/ 1 (sqrt 2.0L0))  (/ 1 (sqrt 2.0L0)))
                    (list (/ 1 (sqrt 2.0L0))  (- (/ 1 (sqrt 2.0L0))))
                    ))
   (list q)))

(defun u-theta (qsys q theta)
  "Quantum U-theta (rotation) gate"
  (apply-operator
   qsys 
   (make-array '(2 2)
               :initial-contents 
               (list (list (cos theta)  (sin theta))
                     (list (- (sin theta))  (cos theta))
                     ))
   (list q)))

(defun cphase-old (qsys q1 q2 alpha)
  "Quantum conditional phase gate, OLD VERSION"
  (apply-operator
   qsys 
   (make-array '(4 4)
               :initial-contents 
               (list (list 1 0 0 0)
                     (list 0 1 0 0)
                     (list 0 0 0 (exp (* (sqrt -1.0L0) alpha)))
                     (list 0 0 (exp (- (* (sqrt -1.0L0) alpha))) 0)
                     ))
   (list q1 q2)))

(defun cphase (qsys q1 q2 alpha)
  "Quantum conditional phase gate"
  (apply-operator
   qsys 
   (make-array '(4 4)
               :initial-contents 
               (list (list 1 0 0 0)
                     (list 0 1 0 0)
                     (list 0 0 1 0)
                     (list 0 0 0 (exp (* (sqrt -1.0L0) alpha)))
                     ))
   (list q1 q2)))


;; U(2) =  U(phi) * R(theta) * U(psi) * exp(i alpha)I
;; where  U(a) = e^(-ia) 0
;;               0       e^(ia)
;; and    R(a) = cos(a) sin(-a)
;;               sin(a) cos(a)
;; This is all pre-multiplied in the following code

(defun u2 (qsys q phi theta psi alpha)
  "Quantum U2 gate, implemented as:
        e^(i(-phi-psi+alpha))*cos(theta)  e^(i(-phi+psi+alpha))*sin(-theta)
        e^(i(phi-psi+alpha))*sin(theta)   e^(i(phi+psi+alpha))*cos(theta)    "
  (apply-operator
   qsys 
   (let ((i (sqrt -1.0L0)))
     (make-array
      '(2 2)
      :initial-contents 
      (list (list (* (exp (* i (+ (- phi) (- psi) alpha))) (cos theta))
                  (* (exp (* i (+ (- phi) psi alpha))) (sin (- theta))))
            (list (* (exp (* i (+ phi (- psi) alpha))) (sin theta))
                  (* (exp (* i (+ phi psi alpha))) (cos theta)))
            )))
   (list q)))


(defun swap (qsys q1 q2)
  "A quantum gate that swaps the amplitudes for the two specified qubits."
  (apply-operator
   qsys
   (make-array '(4 4)
               :initial-contents 
               (list (list 1 0 0 0)
                     (list 0 0 1 0)
                     (list 0 1 0 0)
                     (list 0 0 0 1)
                     ))
   (list q1 q2)))

(defun printamps (qsys)
  "For use in quantum programs; causes the amplitudes of the executing 
quantum system to be printed."
  (print (amplitudes qsys))
  qsys)

(defun insp (qsys)
  "For use in quantum programs; causes the inspector to be invoked on 
the executing quantum system."
  (inspect qsys)
  qsys)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities for measurement and branching

(defun end (qsys) 
  "Marks the end of a measurement branch; has no effect when used
in a quantum program in any other context." 
  qsys)

(defun distance-to-next-unmatched-end (list &optional 
                                            (num-measures 0) (num-ends 0) 
                                            (distance-so-far 0))
  "Returns 0 if there is no unmatched (end) in list; otherwise returns
the number of instructions to the next unmatched (end) (counting the (end))."
  (if (null list) 
    0
    (if (eq (caar list) 'end)
      (if (zerop num-measures)
        (+ 1 distance-so-far)
        (if (oddp num-ends) ;; then this one closes a measure
          (distance-to-next-unmatched-end (cdr list)
                                          (- num-measures 1) (- num-ends 1)
                                          (+ 1 distance-so-far))
          (distance-to-next-unmatched-end (cdr list)
                                          num-measures (+ num-ends 1) 
                                          (+ 1 distance-so-far))))
      (if (eq (caar list) 'measure)
        (distance-to-next-unmatched-end (cdr list)
                                        (+ num-measures 1) num-ends
                                        (+ 1 distance-so-far))
        (distance-to-next-unmatched-end (cdr list)
                                        num-measures num-ends
                                        (+ 1 distance-so-far))))))

(defun without-if-branch (program)
  "Assuming that a MEASURE form has just been removed from the given
program, returns the remainder of the program without the IF (measure-1)
branch."
  (let* ((distance-to-first-unmatched-end 
          (distance-to-next-unmatched-end program))
         (distance-from-first-to-second-unmatched-end
          (distance-to-next-unmatched-end
           (nthcdr distance-to-first-unmatched-end program))))
    (if (zerop distance-to-first-unmatched-end)
      ;; it's all the if part
      nil
      ;; there is some else part
      (if (zerop distance-from-first-to-second-unmatched-end)
        ;; the else never ends
        (subseq program distance-to-first-unmatched-end)
        ;; the else does end
        (append (subseq program 
                        distance-to-first-unmatched-end
                        (+ distance-to-first-unmatched-end
                           distance-from-first-to-second-unmatched-end
                           -1))
                (subseq program (+ distance-to-first-unmatched-end
                                   distance-from-first-to-second-unmatched-end
                                   )))))))

(defun without-else-branch (program)
  "Assuming that a MEASURE form has just been removed from the given
program, returns the remainder of the program without the ELSE (measure-0)
branch."
  (let* ((distance-to-first-unmatched-end 
          (distance-to-next-unmatched-end program))
         (distance-from-first-to-second-unmatched-end
          (distance-to-next-unmatched-end
           (nthcdr distance-to-first-unmatched-end program))))
    (if (zerop distance-to-first-unmatched-end)
      ;; it's all the if part
      program
      ;; there is some else part
      (if (zerop distance-from-first-to-second-unmatched-end)
        ;; the else never ends
        (subseq program 0 (- distance-to-first-unmatched-end 1))
        ;; the else does end
        (append (subseq program 0 (- distance-to-first-unmatched-end 1))
                (subseq program (+ distance-to-first-unmatched-end
                                   distance-from-first-to-second-unmatched-end
                                   )))))))
        
; #|
; Test code for without-if-branch and without-else-branch:

; (setq p1 '((foo) (bar) (end) (baz) (bingo) (end) (biff) (boff)))
; (setq p2 '(  (foo) (bar) 
;              (measure 0) (blink) (end) (blank) (end) 
;            (end) 
;              (baz) (bingo) 
;              (measure 1) (plonk) (end) (plank) (end)
;            (end) 
;            (biff) (boff)))
; (setq p3 '(  (foo) (bar) 
;              (measure 0) (blink) (measure 0)(end)(end)(end) (blank) (end) 
;            (end) 
;              (baz) (bingo) 
;              (measure 1) (plonk) (end) (plank) (measure 0)(end)(end)(end)
;            (end) 
;            (biff) (boff)))

; (without-if-branch p1)
; (without-if-branch p2)
; (without-if-branch p3)
; (without-else-branch p1)
; (without-else-branch p2)
; (without-else-branch p3)


; (setq p4 '((end) (measure 1) (end) (end) (measure 1) (end)))
; (without-if-branch p4)
; (without-else-branch p4)
; |#

(defun force-to (measured-value qubit qsys)
  "Collapses a quantum system to the provided measured-value for the provided
qubit."
  (map-qubit-combinations
   qsys
   #'(lambda ()
       (let* ((pre-column (extract-column qsys (list qubit)))
              (new-column (case measured-value
                            (0 (list (first pre-column) 0))
                            (1 (list 0 (second pre-column))))))
         (install-column qsys new-column (list qubit))))
   (remove qubit (qubit-numbers qsys)))
  qsys)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top level functions

(defvar *post-oracle-measurements*) ;*T*

(defun run-qsys (qsys)
  "Takes a quantum system and returns the list of quantum systems that
results from the execution of its program."
  (if (or (null (program qsys))
          (zerop (prior-probability qsys)))
    (list qsys)
    (let ((instruction (first (program qsys))))
      (setf (instruction-history qsys)
            (append (instruction-history qsys) (list instruction)))
      (if (eq (first instruction) 'halt)
        (list qsys)
        (if (eq (first instruction) 'measure)
          ;; it's a measurement so split state and return list of results
          (let* ((measurement-qubit (second instruction))
                 (probabilities (qc-output-probabilities qsys (list measurement-qubit))))
            (append 
             ;; 1 branch
             (run-qsys
              (force-to 1 measurement-qubit
                        (make-instance 'quantum-system
                          :number-of-qubits (number-of-qubits qsys)
                          :amplitudes (copy-seq (amplitudes qsys))
                          :prior-probability (second probabilities)
                          :oracle-count (oracle-count qsys)
                          :measurement-history (append (measurement-history qsys)
                                                       (list (list measurement-qubit
                                                                   'is 1)))
                          :instruction-history (instruction-history qsys)
                          :program (without-else-branch (rest (program qsys))))))
             ;; 0 branch
             (run-qsys
              (force-to 0 measurement-qubit
                        (make-instance 'quantum-system
                          :number-of-qubits (number-of-qubits qsys)
                          :amplitudes (copy-seq (amplitudes qsys))
                          :prior-probability (first probabilities)
                          :oracle-count (oracle-count qsys)
                          :measurement-history (append (measurement-history qsys)
                                                       (list (list measurement-qubit
                                                                   'is 0)))
                          :instruction-history (instruction-history qsys)
                          :program (without-if-branch (rest (program qsys))))))))
          (let ((resulting-sys
                 (apply (first instruction) (cons qsys (rest instruction)))))
            (setf (program resulting-sys) (rest (program resulting-sys)))
            (run-qsys resulting-sys)))))))
           

(defun execute-quantum-program (pgm num-qubits &optional (oracle-tt nil))
  "Executes the provide quantum program with the specified number of qubits
and the provided oracle truth table, returning a list of the resulting
quantum systems."
  (run-qsys (make-instance 'quantum-system
              :number-of-qubits num-qubits
              :program (subst oracle-tt 'ORACLE-TT pgm))))


(defun test-quantum-program (pgm &key num-qubits cases final-measurement-qubits
                                 threshold (inspect nil) (debug 0))
  "The top-level function to evaluate a quantum program relative to a list of 
a list of (oracle value) cases. Returns a list of:
misses max-error average-error max-expected-oracles average-expected-oracles
See documentation for a more complete explanation of the arguments and
return values."
  (let ((misses 0)
        (max-error 0)
        (total-error 0)
        (average-error 0)
        (max-expected-oracles 0)
        (total-expected-oracles 0)
        (average-expected-oracles 0)
        (num-cases (length cases)))
    (dolist (case cases)
      (let* ((resulting-systems (execute-quantum-program
                                 pgm num-qubits (first case)))
             (raw-error (- 1.0 (nth (second case) 
                                    (multi-qsys-output-probabilities
                                     resulting-systems final-measurement-qubits))))
             (expected-oracles (expected-oracles resulting-systems)))
        (if (> raw-error threshold) (incf misses))
        (incf total-error raw-error)
        (when (> raw-error max-error) 
          (setq max-error raw-error))
        (incf total-expected-oracles expected-oracles)
        (when (> expected-oracles max-expected-oracles) 
          (setq max-expected-oracles expected-oracles))
        (when (>= debug 2)
          (format t "~%---~%Case:~A, Error:~,5F" case raw-error))
        (when inspect (inspect resulting-systems))))
    (setq average-error (/ total-error num-cases))
    (setq average-expected-oracles (/ total-expected-oracles num-cases))
    (when (>= debug 1)
      (format t "~%~%Misses:~A" misses)
      (format t "~%Max error:~A" max-error)
      (format t "~%Average error:~A" (float average-error))
      (format t "~%Max expected oracles:~A" max-expected-oracles)
      (format t "~%Average expected oracles:~A" (float average-expected-oracles)))
    (list misses max-error average-error max-expected-oracles average-expected-oracles)))



; #|

; EXAMPLES

; To run each example evaluate the relevant definition and then call the function
; with or without a debugging argument (which should be 0 for no debugging info,
; 1 for a little debugging info, and 1 for a lot of debugging info). For example,
; after evaluating the test-herbs-grover function definition you could try the
; following calls:

; (test-herbs-grover) ;; for no debugging info
; (test-herbs-grover 1) ;; for some debugging info (just results)
; (test-herbs-grover 2) ;; for more debugging info


;;


(defun test-branching (&optional (debug 0))
  "Creates 4 final quantum systems and invokes the inspector on each."
  (test-quantum-program 
   `((hadamard 0)
     (measure 0)
       (hadamard 1)
       (measure 1)
       (end)
       (end)
     (end)
       (hadamard 2)
       (measure 2)
       (end)
       (end)
     (end)
     )
   :num-qubits 3
   :cases '(((1 0) 0)) ;; an arbitrary case, just so it'll run
   :final-measurement-qubits (list 0)
   :threshold 0.48
   :debug debug
   :inspect t))

; (test-branching)


(defun test-herbs-grover (&optional (debug 0))
  "Tests Herb Bernstein's version of Grover's quantum database search
algorithm for a 4 item database on all four 'single marked item' test
cases."
  (test-quantum-program 
   `((hadamard 2)
     (hadamard 1)
     (u-theta 0 ,(/ pi 4))
     (oracle ORACLE-TT 2 1 0)
     (hadamard 2)
     (cnot 2 1)
     (hadamard 2)
     (u-theta 2 ,(/ pi 2))
     (u-theta 1 ,(/ pi 2))
     )
   :num-qubits 3
   :cases '(((1 0 0 0) 0)
            ((0 1 0 0) 1)
            ((0 0 1 0) 2)
            ((0 0 0 1) 3))
   :final-measurement-qubits (list 2 1)
   :threshold 0.48
   :debug debug
   :inspect nil))

; (test-herbs-grover 1)
; (test-herbs-grover 2)


(defun test-evolved-grover (&optional (debug 0))
  "Tests an evolved version of Grover's quantum database search
algorithm (evolved with lgp2) for a 4 item database on all four
'single marked item' test cases."
  (test-quantum-program 
   `((U-THETA 0 3.926990816987241) 
     (HADAMARD 1)
     (U-THETA 2 -8.63937979737193)
     (ORACLE ORACLE-TT 2 1 0)
     (CPHASE 1 2 3.141592653589793) 
     (CNOT 0 2)
     (HADAMARD 0)
     (U2 0 0.0 2.356194490192345 -3.4033920413889427 0)
     (HADAMARD 0) 
     (U-THETA 1 2.356194490192345))
   :num-qubits 3
   :cases '(((1 0 0 0) 0)
            ((0 1 0 0) 1)
            ((0 0 1 0) 2)
            ((0 0 0 1) 3))
   :final-measurement-qubits (list 1 0)
   :threshold 0.48
   :debug debug
   :inspect nil))

; (test-evolved-grover 1)
; (test-evolved-grover 2)


(defun test-evolved-and-or (&optional (debug 0))
  (test-quantum-program 
   '((U2 2 -6.088543013651391 -34.36116964863836 -7.682902920850156 0.0013517818812377553)
     (U-THETA 2 94.46204015939107) 
     (HADAMARD 0)
     (HADAMARD 1) 
     (ORACLE ORACLE-TT 1 0 2)
     (U-THETA 2 -54.494324298211346) 
     (HADAMARD 0)
     (MEASURE 0)
     (swap 2 0)
     (halt)
     (end)
     (U2 2 -0.20450950372104815 -34.76200757140856 -7.856634973508906 -0.04960986541249215)
     (U-THETA 2 190.24766604570047) 
     (MEASURE 2)
     (HADAMARD 2) 
     (CNOT 2 1)
     (U-THETA 2 3.9269907773297987)
     )
   :num-qubits 3
   :cases '(((0 0 0 0) 0) 
            ((0 0 0 1) 0) 
            ((0 0 1 0) 0) 
            ((0 0 1 1) 0) 
            ((0 1 0 0) 0) ((0 1 0 1) 1) ((0 1 1 0) 1) ((0 1 1 1) 1) 
            ((1 0 0 0) 0) ((1 0 0 1) 1) ((1 0 1 0) 1) ((1 0 1 1) 1) 
            ((1 1 0 0) 0) ((1 1 0 1) 1) ((1 1 1 0) 1) ((1 1 1 1) 1)
            )
   :final-measurement-qubits (list 2)
   :threshold 0.48
   :debug debug
   :inspect nil))

; (test-evolved-and-or 1)
; (test-evolved-and-or 2)


; |#

;; EOF
