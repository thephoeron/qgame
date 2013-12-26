;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qgame-gui.lisp
;; c) 2000-2002, Lee Spector, lspector@hampshire.edu
;;
;; This file contains some quick hacks to make a GUI for QGAME, a quantum
;; computer simulator. This will work only under Macintosh Common Lisp
;; (see http://www.digitool.com). Warning: this code (for the GUI) was
;; hastily written and is poorly documented (although some usage notes
;; follow below).

;; Revised December 2002: Now works under MCL 5.0/MacOS X

;; December 26, 2013: Need to rewrite GUI code for LTK --the Phoeron
#|

QGAME-GUI Notes

See the comment at the top of qgame.lisp for an introduction to the
QGAME emulator.

The QGAME graphical user interface (GUI) is a quick hack intended
to allow people with no knowledge of Lisp to experiment with QGAME.
It uses Macintosh Common Lisp (MCL) interface code and will work only
under MacOS with MCL. Not all features of QGAME are available from
the GUI. One uses the GUI by typing (or pasting) a program into the
"QGAME Program" dialog and clicking on the "Run" button. The amplitudes
can be monitored during the run as described below. The probabilities
of reading the system in any given state at the end of the computation
are printed in the "Listener" window at the end of the run -- the
probabilities are listed in the same order that the amplitudes are
listed in the amplitude display windows.

TO USE: first load qgame.lisp and then load this file. You'll see
a couple of warnings as functions defined in qgame.lisp are re-defined
here -- that's normal; don't worry. Test the system by pasting some
of the examples from the end of these notes into the "QGAME Program"
window and clicking "Run."

A quantum program is specified as a list of the instructions listed
below. The MATRIX-GATE form allows for the specification of an arbitrary
matrix operator. The documentation for most instructions includes a
description of the corresponding gate's matrix; those in Lisp array
notation (beginning with #2A) can be used directly in calls to MATRIX-GATE
(the others were too messy when expressed in this way so I wrote them more
readably). A line of instructions can be "commented out" with a semicolon
(";") at the beginning of a line. Notations in angle brackets ("<>") are
to be replaced by appropriate values -- the brackets themselves should NOT
be included. Code can be indented for readability -- this has no effect
on execution.

The GUI is not "bullet proofed" at all -- it will be easy to break it and
end up in a Lisp error loop. If this happens and you don't know how
to fix it your best bet is to quit and restart Lisp. If for some reason
you close the "QGAME Program" window you can re-create it by typing
(make-program-dialog) and hitting "return" in the "Listener" window.

Programs are not automatically saved. You should cut and paste them from/to
text files which you save independently.

You can find out the exact amplitude for a component of the quantum state
by clicking on the colored state indicators. Click again on the amplitude
to make it go away. Note that you can click to see the amplitudes while
the program is running -- you may want to increase the delay to give yourself
time to do this. The simulation will pause while you look at an amplitude
and will resume when you click on it to make it go away. When you see an
amplitude that looks something like #c(0.0 1.4142135623730951) you are
seeing a complex number. The first number within the #c(...) is the real
part of the number, while the second number is the imaginary part. So
#c(0.0 1.4142135623730951) has a real part of 0.0 and an imaginary part
of 1.4142135623730951. This particular complex number is the square root
of -2.0. Sometimes you will see #c(0.0 0.0) -- this is just zero, but it
is being expressed in complex form because that amplitude previously had
some non-zero complex value.

When a program branches (due to a measurement) the title bars of the
resulting amplitude display windows show the probability that the 
system will end up in that computational path.

The GUI was designed to look reasonably good with 2 to 4 qubit systems.
It doesn't scale correctly for 1 qubit systems (you can't really see the
program or measurements), and larger systems will probably go off your
screen (and they can't be scrolled).

A few examples are presented after the list of instructions.


INSTRUCTIONS
------------

(QNOT <q>)
  -- applies a quantum NOT gate to the specified qubit
     matrix: #2A((0 1)
                 (1 0))

(CNOT <control> <target>)
  -- applies a quantum controlled NOT gate to the specified control
     and target qubits
     matrix: #2A((1 0 0 0)
                 (0 1 0 0)
                 (0 0 0 1)
                 (0 0 1 0))

(SRN <q>)
  -- applies a quantum square-root-of-NOT gate to the specified qubit
     matrix: 1/sqrt(2) -1/sqrt(2)
             1/sqrt(2) 1/sqrt(2)

(NAND <in1> <in2> <out>)
  -- applies a quantum NAND gate to the specified input and output qubits
     matrix: #2A((0 1 0 0 0 0 0 0)
                 (1 0 0 0 0 0 0 0)
                 (0 0 0 1 0 0 0 0)
                 (0 0 1 0 0 0 0 0)
                 (0 0 0 0 0 1 0 0)
                 (0 0 0 0 1 0 0 0)
                 (0 0 0 0 0 0 1 0)
                 (0 0 0 0 0 0 0 1))

(HADAMARD <q>)
  -- applies a Hadamard gate to the specified qubit
     matrix: 1/sqrt(2) 1/sqrt(2)
             1/sqrt(2) -1/sqrt(2)

(U-THETA <q> <theta>)
  -- applies a rotation gate with the specified (real-valued) angle theta
     to the specified qubit
     matrix: cos(theta)  sin(theta)
             -sin(theta) cos(theta)

(CPHASE <control> <target> <alpha>)
  -- a controlled phase gate
     matrix: 1 0 0 0
             0 1 0 0
             0 0 1 0
             0 0 0 e^(i*alpha)
     
(U2 <q> <phi> <theta> <psi> <alpha>)
  -- a general rotation gate for a single qubit with 4 real-valued 
     parameters
     matrix:
        e^(i(-phi-psi+alpha))*cos(theta)  e^(i(-phi+psi+alpha))*sin(-theta)
        e^(i(phi-psi+alpha))*sin(theta)   e^(i(phi+psi+alpha))*cos(theta) 

(SWAP <q1> <q2>)
  -- applies a gate that swaps the amplitudes of the two specified qubits
     matrix: #2A((1 0 0 0)
                 (0 0 1 0)
                 (0 1 0 0)
                 (0 0 0 1))

(MEASURE <q>) <1-branch> (END) <0-branch> (END)
  -- causes a measurement-based branch in the execution of the quantum
     program. In one branch the state will be collapsed as if "1" was 
     read from the specified qubit, the 1-branch code will be executed
     and the 0-branch code will be skipped. In the other branch the
     state will be collapsed as if "0" was read from the specified qubit,
     the 0-branch code will be executed and the 1-branch code will be
     skipped. Measurement structures can be nested within the branches
     of other measurement structures. If there is no second END then
     execution in the 1 case terminates after execution of the 1-branch.
     If there is no first END then there is no 0-branch. Unmatched ENDs
     are ignored.

(HALT)
  -- causes the executing quantum system to halt execution (to execute
     no further instructions)

(PRINTAMPS)
  -- causes the amplitudes of the executing quantum system to be printed

(INSP)
  -- causes the inspector to be invoked on the executing quantum system



EXAMPLES
--------

;; some simple bit flipping (3 qubit system)
(qnot 0)
(qnot 1)
(qnot 2)
(qnot 1)
(qnot 0)
(qnot 2)

;; bit flipping with explicit matrix gates
(matrix-gate #2A((0 1)
                 (1 0))
             0)
(matrix-gate #2A((0 1)
                 (1 0))
             1)
(matrix-gate #2A((0 1)
                 (1 0))
             2)

;; with some controlled nots (3 qubit system)
(qnot 0)
(cnot 0 1)
(cnot 1 2)
(qnot 0)
(cnot 0 1)
(cnot 0 2)

;; with an initial Hadamard (3 qubit system)
(hadamard 0)
(cnot 0 1)
(cnot 1 2)
(qnot 0)
(cnot 0 1)
(cnot 0 2)

;; square-root-of-not test (3 qubit system)
(srn 0)
(srn 1)
(srn 2)
(srn 0)
(srn 1)
(srn 2)

;; with a measurement (3 qubit system)
;; note: the indentation is just for readability
(hadamard 0)
(measure 0)
  (cnot 0 1)
(end)
  (cnot 0 2)
(end)

;; nested measurements (3 qubit system)
;; note: the indentation is just for readability
(hadamard 0)
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

;; crazy rotations (3 qubit system)
(u-theta 0 3.9) 
(hadamard 1)
(u-theta 2 -8.6)
(cnot 0 2)
(hadamard 0)
(u2 0 0.0 2.3 -3.4 0)
(hadamard 0) 
(u-theta 1 2.3)

;; more colors (3 qubit system)
(u2 2 -6.1 -34.3 -7.68 0.001)
(u-theta 2 94.4) 
(hadamard 0)
(hadamard 1) 
(u-theta 2 -54.5) 
(hadamard 0)
(cphase 1 2 2.123)
(u-theta 2 94.4) 
(swap 2 0)
(u2 2 -0.2 -34.76 -7.85 -0.049)
(u-theta 2 190.24) 
(hadamard 2) 
(cnot 2 1)
(u-theta 2 3.9)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; source code for the gui


(defclass visible-quantum-system (quantum-system)
  ((qsys-view :accessor qsys-view :initarg qsys-view)
   (amplitude-items :accessor amplitude-items)
   (amplitude-label-item :accessor amplitude-label-item)
   (program-item :accessor program-item)
   (program-label-item :accessor program-label-item)
   (measurements-item :accessor measurements-item)
   (measurements-label-item :accessor measurements-label-item)))


(defparameter *qgame-gui-qubits* 5)
(defparameter *qgame-gui-num-amps* (expt 2 *qgame-gui-qubits*))
(defparameter *qgame-font* '("Helvetica" 32 :SRCOR :BOLD (:COLOR-INDEX 0)))
(defparameter *qgame-program-font* '("Monaco" 9 :SRCOR :PLAIN (:COLOR-INDEX 0)))
(defparameter *amplitude-item-height* (second *qgame-font*))
(defparameter *amplitude-item-width*
  (* *qgame-gui-qubits* (- *amplitude-item-height* 12)))
(defparameter *amplitude-column-height* 
  (* *amplitude-item-height* *qgame-gui-num-amps*))
(defparameter *program-item-width* 250)
(defparameter *qgame-dialog-width* 
  (+ *program-item-width* *amplitude-item-width* 50))
(defparameter *qgame-dialog-height* 
  (+ 50 ; margins
     *amplitude-item-height* ; label
     (* *amplitude-item-height* *qgame-gui-num-amps*)))
(defparameter *qgame-display-delay* 0.25)

(defparameter *qgame-gui-min-x* 30)
(defparameter *qgame-gui-min-y* 70)
(defparameter *qgame-gui-max-x* (- *screen-width* *qgame-dialog-width*))
(defparameter *qgame-gui-max-y* (- *screen-height* *qgame-dialog-height*))

(defparameter *qgame-gui-current-x* *qgame-gui-min-x*)
(defparameter *qgame-gui-current-y* *qgame-gui-min-y*)
(defparameter *qgame-gui-x-delta* *qgame-dialog-width*)
(defparameter *qgame-gui-y-delta* 30)

(defun resize-qgame-qui (num-qubits)
  "An ugly hack if you ever saw one! Resets every parameter, except for
those that are obviously constant, based on the new size."
  (setq *qgame-gui-qubits* num-qubits)
  (setq *qgame-gui-num-amps* (expt 2 *qgame-gui-qubits*))
  (setq *amplitude-item-height* (second *qgame-font*))
  (setq *amplitude-item-width*
    (* *qgame-gui-qubits* (- *amplitude-item-height* 12)))
  (setq *amplitude-column-height* 
    (* *amplitude-item-height* *qgame-gui-num-amps*))
  (setq *program-item-width* 250)
  (setq *qgame-dialog-width* 
    (+ *program-item-width* *amplitude-item-width* 50))
  (setq *qgame-dialog-height* 
    (+ 50 ; margins
       *amplitude-item-height* ; label
       (* *amplitude-item-height* *qgame-gui-num-amps*)))
  (setq *qgame-gui-max-x* (- *screen-width* *qgame-dialog-width*))
  (setq *qgame-gui-max-y* (- *screen-height* *qgame-dialog-height*))
  (setq *qgame-gui-current-x* *qgame-gui-min-x*)
  (setq *qgame-gui-current-y* *qgame-gui-min-y*)
  (setq *qgame-gui-x-delta* *qgame-dialog-width*)
  )
  
(defun next-qgame-dialog-position ()
  (let ((result (make-point *qgame-gui-current-x* *qgame-gui-current-y*)))
    (setq *qgame-gui-current-x* 
          (max *qgame-gui-min-x*
               (mod (+ *qgame-gui-current-x* *qgame-gui-x-delta*)
                    *qgame-gui-max-x*)))
    (setq *qgame-gui-current-y* 
          (max *qgame-gui-min-y*
               (mod (+ *qgame-gui-current-y* *qgame-gui-y-delta*)
                    *qgame-gui-max-y*)))
    result))

(defun phase360 (amplitude)
  (* (max 0 (phase amplitude))
     (/ 180 pi)))

(defun show-amp (qsys ampnum amp)
  (let ((h (phase360 amp))
        (s (abs amp #|(expt amp 2)|#))
        (v 1))
    (set-part-color (aref (amplitude-items qsys) ampnum)
                    :text (apply #'make-color (HSVtoRGB h s v))
                    )))

(defun show-amps (qsys)
  (dotimes (i (expt 2 (number-of-qubits qsys)))
    (show-amp qsys i (aref (amplitudes qsys) i)))
  (set-dialog-item-text (program-item qsys)
                        (format nil "~{~A~%~}" (instruction-history qsys)))
  (view-draw-contents (program-item qsys))
  (set-dialog-item-text (measurements-item qsys)
                        (format nil "~{~A~%~}" (measurement-history qsys)))
  (view-draw-contents (measurements-item qsys))
;(view-draw-contents (qsys-view qsys))
;(event-dispatch)
  (sleep *qgame-display-delay*))

(defun string-down-from (n)
  (if (zerop n)
    "0"
    (concatenate 'string (princ-to-string n)
                 (string-down-from (- n 1)))))

(defun amplitude-dialog (amp position)
  (modal-dialog
   (MAKE-INSTANCE
          'COLOR-DIALOG
          :WINDOW-TYPE
          :SHADOW-EDGE-BOX
          :VIEW-POSITION position
          :VIEW-SIZE
          #@(300 20)
          :CLOSE-BOX-P
          NIL
          ;:VIEW-FONT
          ;'("Times" 18 :SRCOR :PLAIN (:COLOR-INDEX 0))
          :VIEW-SUBVIEWS
          (LIST 
                (let ((new
                       (MAKE-DIALOG-ITEM
                        'STATIC-TEXT-DIALOG-ITEM
                        #@(5 6)
                        #@(395 20)
                        (princ-to-string amp)
                        #'(lambda (item)
                            (declare (ignore item))
                            (return-from-modal-dialog nil)))))
                  (SET-VIEW-FONT new *qgame-program-font*)
                  new)))))

(defmethod initialize-instance :after ((qsys visible-quantum-system) &rest args)
  (declare (ignore args))
  (setf (qsys-view qsys)
        (MAKE-INSTANCE
          'COLOR-DIALOG
          :WINDOW-TYPE :DOCUMENT ;:MOVABLE-DIALOG
          :VIEW-POSITION (next-qgame-dialog-position)
          :VIEW-SIZE (make-point *qgame-dialog-width*
                                 *qgame-dialog-height*)
          :VIEW-FONT *qgame-font*
          :window-title (format nil "qgame, p=~A" (prior-probability qsys))))
  (setf (amplitude-items qsys) (make-array *qgame-gui-num-amps*))
  ;; amplitude label
  (setf (amplitude-label-item qsys)
        (MAKE-DIALOG-ITEM 'STATIC-TEXT-DIALOG-ITEM
           (make-point 20 20)
           (make-point *amplitude-item-width* *amplitude-item-height*)
           (string-down-from (- *qgame-gui-qubits* 1))
           'NIL))
  (set-view-font (amplitude-label-item qsys)
                 *qgame-font*)
  (add-subviews (qsys-view qsys) (amplitude-label-item qsys))
  ;; amplitude items
  (dotimes (a *qgame-gui-num-amps*)
    (setf (aref (amplitude-items qsys) a)
          (MAKE-DIALOG-ITEM
           'STATIC-TEXT-DIALOG-ITEM
           (make-point 20 (+ 20 *amplitude-item-height* (* *amplitude-item-height* a)))
           (make-point *amplitude-item-width* *amplitude-item-height*)
           (format 
            nil 
            (concatenate 'string 
                         "~"
                         (princ-to-string *qgame-gui-qubits*)
                         ",'0B")
            a)
           ;))
           #'(lambda (item) 
               (amplitude-dialog (aref (amplitudes qsys)
                                       (let ((*read-base* 2))
                                         (read-from-string (dialog-item-text item))))
                                 (add-points (view-position item)
                                             (view-position (qsys-view qsys)))
                                 ))))
    (set-view-font (aref (amplitude-items qsys) a)
                   *qgame-font*)
    (set-part-color (aref (amplitude-items qsys) a)
                    :body CCL::*LIGHTER-GRAY-COLOR*) ;*Light-Gray-Color*)
    (set-part-color (aref (amplitude-items qsys) a)
                    :text *WHITE-COLOR*)
    (add-subviews (qsys-view qsys) (aref (amplitude-items qsys) a)))
  ;; program label
  (setf (program-label-item qsys)
        (MAKE-DIALOG-ITEM 'STATIC-TEXT-DIALOG-ITEM
           (make-point (+ 30 *amplitude-item-width*) 38)
           (make-point *program-item-width* 14)
           "Instruction History"
           'NIL))
  (set-view-font (program-label-item qsys) *qgame-program-font*)
  (add-subviews (qsys-view qsys) (program-label-item qsys))
  ;; program
  (setf (program-item qsys)
        (MAKE-INSTANCE
          'SCROLLING-FRED-VIEW
          :SAVE-BUFFER-P nil
          :H-SCROLLP T
          :V-SCROLLP T
          :WRAP-P nil
          :VIEW-SIZE
          (make-point *program-item-width* (truncate *amplitude-column-height* 2))
          :VIEW-POSITION
          (make-point (+ 30 *amplitude-item-width*) (+ *amplitude-item-height* 20))))
  (SET-VIEW-FONT (program-item qsys) *qgame-program-font*)
  (add-subviews (qsys-view qsys) (program-item qsys))
  ;; measurement history label
  (setf (measurements-label-item qsys)
        (MAKE-DIALOG-ITEM 'STATIC-TEXT-DIALOG-ITEM
           (make-point (+ 30 *amplitude-item-width*) 
                       (+ *amplitude-item-height*
                          (truncate *amplitude-column-height* 2) 38))
           (make-point *program-item-width* 14)
           "Measurement History"
           'NIL))
  (set-view-font (measurements-label-item qsys) *qgame-program-font*)
  (add-subviews (qsys-view qsys) (measurements-label-item qsys))
  ;; measurement history
  (setf (measurements-item qsys)
        (MAKE-INSTANCE
          'SCROLLING-FRED-VIEW
          :SAVE-BUFFER-P nil
          :H-SCROLLP T
          :V-SCROLLP T
          :WRAP-P nil
          :VIEW-SIZE
          (make-point *program-item-width* 
                      (- *amplitude-column-height*
                         (truncate *amplitude-column-height* 2)
                         *amplitude-item-height*))
          :VIEW-POSITION
          (make-point (+ 30 *amplitude-item-width*) 
                      (+ *amplitude-item-height* *amplitude-item-height* 
                         (truncate *amplitude-column-height* 2) 20))))
  (SET-VIEW-FONT (measurements-item qsys) *qgame-program-font*)
  (add-subviews (qsys-view qsys) (measurements-item qsys))
  ;(show-amps qsys)
  )

#| from http://www.cs.rit.edu/~ncs/color/t_convert.html

The Hue/Saturation/Value model was created by A. R. Smith in 1978. It is based
on such intuitive color characteristics as tint, shade and tone (or family,
purety and intensity). The coordinate system is cylindrical, and the colors
are defined inside a hexcone. The hue value H runs from 0 to 360¼. The
saturation S is the degree of strength or purity and is from 0 to 1. Purity
is how much white is added to the color, so S=1 makes the purest color (no white).
Brightness V also ranges from 0 to 1, where 0 is the black.


void HSVtoRGB( float *r, float *g, float *b, float h, float s, float v )
{
        int i;
        float f, p, q, t;

        if( s == 0 ) {
                // achromatic (grey)
                *r = *g = *b = v;
                return;
        }

        h /= 60;                        // sector 0 to 5
        i = floor( h );
        f = h - i;                      // factorial part of h
        p = v * ( 1 - s );
        q = v * ( 1 - s * f );
        t = v * ( 1 - s * ( 1 - f ) );

        switch( i ) {
                case 0:
                        *r = v;
                        *g = t;
                        *b = p;
                        break;
                case 1:
                        *r = q;
                        *g = v;
                        *b = p;
                        break;
                case 2
                        *r = p;
                        *g = v;
                        *b = t;
                        break;
                case 3:
                        *r = p;
                        *g = q;
                        *b = v;
                        break;
                case 4:
                        *r = t;
                        *g = p;
                        *b = v;
                        break;
                default:                // case 5:
                        *r = v;
                        *g = p;
                        *b = q;
                        break;
        }
|#

;; the above translated into Lisp:

(defun HSVtoRGB (h s v)
  (let (r g b i f p q tt)
    (when (zerop s)
      (setq r v
            g v
            b v))
    (setq h (/ h 60)) ;// sector 0 to 5
    (setq i (floor h))
    (setq f (- h i)) ;             // factorial part of h
    (setq p (* v (- 1 s)))
    (setq q (* v (- 1 (* s f))))
    (setq tt (* v (- 1 (* s (- 1 f)))))
    (case i
      (0 (setq r v
               g tt
               b p))
      (1 (setq r q
               g v
               b p))
      (2 (setq r p
               g v
               b tt))
      (3 (setq r p
               g q
               b v))
      (4 (setq r tt
               g p
               b v))
      (5 (setq r v
               g p
               b q)))
    ;(print (list r g b i f p q tt))
    (mapcar #'(lambda (val) (truncate (* val 65535)))
            (list r g b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; redefined from qgame.lisp:
(defun run-qsys (qsys)
  "Takes a quantum system and returns the list of quantum systems that
results from the execution of its program."
  (show-amps qsys)
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
            (window-close (qsys-view qsys)) ;***
            (append 
             ;; 1 branch
             (run-qsys
              (force-to 1 measurement-qubit
                        (make-instance 'visible-quantum-system ;***
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
                        (make-instance 'visible-quantum-system ;***
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

(defun upto (n)
  (if (< n 0)
    nil
    (append (upto (- n 1)) (list n))))

;; redefined from qgame.lisp:
(defun execute-quantum-program (pgm num-qubits &optional (oracle-tt nil))
  "Executes the provide quantum program with the specified number of qubits
and the provided oracle truth table, returning a list of the resulting
quantum systems."
  (resize-qgame-qui num-qubits)
  (print 
   (multi-qsys-output-probabilities
    (run-qsys (make-instance 'visible-quantum-system ;***
                :number-of-qubits num-qubits
                :program (subst oracle-tt 'ORACLE-TT pgm)))
    (reverse (upto (- num-qubits 1))))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matrix gates

(defun matrix-gate (qsys matrix &rest qubits)
  (apply-operator qsys
                  matrix
                  qubits))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; program dialog

(defvar *program-dialog-item*)
(defvar *qubits-dialog-item*)
(defvar *program-to-execute*)
(defvar *qubits-for-program-to-execute*)
(defvar *delay-dialog-item*)

(defun make-program-dialog ()
  (MAKE-INSTANCE
    'COLOR-DIALOG
    :WINDOW-TYPE 
    :DOCUMENT ;:MOVABLE-DIALOG
    :WINDOW-TITLE
    "QGAME Program"
    :VIEW-POSITION
    '(:TOP 96)
    :VIEW-SIZE
    #@(301 250)
    :VIEW-FONT
    '("Charcoal" 12 :SRCOR :PLAIN (:COLOR-INDEX 0))
    ;:CLOSE-BOX-P
    ;NIL ; t
    :VIEW-SUBVIEWS
    (LIST (LET ((NEW
                 (MAKE-INSTANCE
                   'SCROLLING-FRED-VIEW
                   :SAVE-BUFFER-P
                   T
                   :H-SCROLLP
                   T
                   :V-SCROLLP
                   T
                   :WRAP-P
                   T
                   :VIEW-SIZE
                   #@(291 207)
                   :VIEW-POSITION
                   #@(5 5))))
            (SET-VIEW-FONT NEW '("Monaco" 9 :SRCOR :PLAIN (:COLOR-INDEX 0)))
            (setq *program-dialog-item* new)
            NEW)
          (MAKE-DIALOG-ITEM
           'STATIC-TEXT-DIALOG-ITEM
           #@(5 224)
           #@(48 16)
           "Qubits:"
           'NIL)
          (setq *qubits-dialog-item*
                (MAKE-DIALOG-ITEM
                 'EDITABLE-TEXT-DIALOG-ITEM
                 #@(58 224)
                 #@(30 16)
                 "3"
                 'NIL
                 :ALLOW-RETURNS
                 NIL
                 :DRAW-OUTLINE
                 T))
          (MAKE-DIALOG-ITEM
           'STATIC-TEXT-DIALOG-ITEM
           #@(104 224)
           #@(48 16)
           "Delay:"
           'NIL)
          (setq *delay-dialog-item*
                (MAKE-DIALOG-ITEM
                 'EDITABLE-TEXT-DIALOG-ITEM
                 #@(150 224)
                 #@(40 16)
                 "0.5"
                 'NIL
                 :ALLOW-RETURNS
                 NIL
                 :DRAW-OUTLINE
                 T))
          (MAKE-DIALOG-ITEM
           'BUTTON-DIALOG-ITEM
           #@(220 223)
           #@(64 18)
           "Run"
           #'(LAMBDA (ITEM) ITEM
              (setq *program-to-execute*
                    (read-from-string 
                     (format nil "(~A)" 
                             (dialog-item-text *program-dialog-item*))))
              (setq *qubits-for-program-to-execute*
                    (read-from-string 
                     (dialog-item-text *qubits-dialog-item*)))
              (setq *qgame-display-delay*
                    (read-from-string 
                     (dialog-item-text *delay-dialog-item*)))
              (eval-enqueue '(run-program-globally)))
           :DEFAULT-BUTTON
           NIL)))
  )

(make-program-dialog)

(defun run-program-globally ()
  (execute-quantum-program *program-to-execute* *qubits-for-program-to-execute*))

;; EOF
