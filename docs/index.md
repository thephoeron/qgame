QGAME
=====

DESCRIPTION
-----------

QGAME (Quantum Gate And Measurement Emulator) is a system, written in
Common Lisp, that allows a user to run quantum computing algorithms
on an ordinary digital computer. Because quantum computers have complexity
advantages over classical computers, any classical emulator will necessarily
be less efficient than the quantum computer that it is emulating. 
QGAME nonetheless allows the user to find out what outputs the quantum
program would produce, and with what probabilities (since quantum
computation is in general not deterministic). 

QGAME was developed for use in conjunction with a genetic programming
system, to allow for the evolution of new quantum algorithms, but
it can be useful for testing quantum algorithms regardless of their
origin.

QGAME is based on the "quantum gate array" model of quantum computation,
in which quantum "gates" (represented as square matrices) are applied
to a register of qubits (via tensor product formation and matrix
multiplication). QGAME always starts with all qubits having the
value zero (in the state |00...0>), applies a sequence of gates,
and returns values about the resulting state. Measurement gates
cause the system to branch, following one execution path (with the
appropriate quantum state collapse) for each possible value. Final
measurements are made across the end-states of all of the resulting
branches.

Most interesting quantum algorithms involve calling an "oracle" or
"black box" function of which one is trying to determine some
property. QGAME supports only boolean oracles (that is, with single
qubit output), but it allows the user to specify any such oracle
and to indicate the desired system output for each specified
oracle. Oracles are implemented in the standard way, by applying
a (unitary) quantum "NOT" gate on the output qubit wherever the
oracle's truth table indicates a "1".

The primary user-level function is TEST-QUANTUM-PROGRAM which takes
a quantum program (described below) and the following keyword
arguments:

:num-qubits
   -- the number of qubits in the system
:cases
   -- a list of (oracle-truth-table output) pairs, where each 
      oracle-truth-table is a list of 0s and 1s specifying the
      right-hand (output) column of the oracle's output (where
      the rows are listed in binary order for the input qubits),
      and where the output is the integer that one would like
      to be measured across the final measurement qubits at
      the end of the computation 
:final-measurement-qubits
   -- a list of the qubits upon which final measurements will be
      performed, with the most significant qubit listed first
      and the least significant qubit listed last
:threshold
   -- the probability of error below which a run is considered
      successful for the sake of the "hits" component of the 
      return value (see below)
:debug 
   -- 0 for no debugging info, 1 for some debugging info,
      2 for more debugging info
:inspect 
   -- if non-nil, causes the inspector to be invoked on all
      resulting quantum states

TEST-QUANTUM-PROGRAM returns a list containing the following
values:

- the number of "misses" -- that is, cases in which the measured
  value will, with probability greater than the specified threshold,
  fail to equal the desired output
- the maximum error for any provided case
- the average error for all provided cases
- the maximum number of expected oracle calls across all cases
- the number of expected oracle calls averaged across all cases

A quantum program is specified as a list of the forms listed below. See
the documentation forms within function definitions and the function
definitions themselves for more information on the quantum gates, including
their matrix expressions.

(QNOT <q>)
  -- applies a quantum NOT gate to the specified qubit

(CNOT <control> <target>)
  -- applies a quantum controlled NOT gate to the specified control
     and target qubits

(SRN <q>)
  -- applies a quantum square-root-of-NOT gate to the specified qubit

(NAND <in1> <in2> <out>)
  -- applies a quantum NAND gate to the specified input and output qubits

(HADAMARD <q>)
  -- applies a Hadamard gate to the specified qubit

(U-THETA <q> <theta>)
  -- applies a rotation gate with the specified (real-valued) angle theta
     to the specified qubit

(CPHASE-OLD <control> <target> <alpha>)
  -- one version of a controlled phase gate -- see the definition for the
     matrix

(CPHASE <control> <target> <alpha>)
  -- another (probably preferable) version of a controlled phase gate -- 
     see the definition for the matrix
     
(U2 <q> <phi> <theta> <psi> <alpha>)
  -- a general rotation gate for a single qubit with 4 real-valued 
     parameters -- see the definition for the matrix

(SWAP <q1> <q2>)
  -- applies a gate that swaps the amplitudes of the two specified qubits

(ORACLE ORACLE-TT <q1> <q2> ... <qn> <out>)
  -- calls the oracle on the specified input qubits (specified as "q"s 
     above) and the specified output qubit. The input qubits are listed
     most significant first. Note that "ORACLE-TT" must appear as 
     a literal symbol in the call -- this will be replaced with the
     oracle's truth table before execution.

(LIMITED-ORACLE <max-calls> ORACLE-TT <q1> <q2> ... <qn> <out>)
  -- like ORACLE but with one additional argument, max-calls, which should
     be a positive integer. If the provided number of oracle calls has
     already been made by the time this instruction is executed then it
     will have no effect.

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

See the comment at the bottom of this file for some examples.


VERSION HISTORY
---------------

Version 1 was adapted from "qc-sim", a simulator that only allowed "measure
and stop or continue" measurements, in late November 1999. The slowdown from
non-branching (measure and stop/continue) simulator appears to be ~30% for a
2-intermediate-measurement 2-bit and-or algorithm (one of the examples below).

May 30, 2000: Added more explicit 'long float' declarations to minimize
roundoff errors under Allegro. NOTE: It is also necessary to ensure that
float args to gates are longs to avoid these roundoff errors.

November 13, 2000: Fixed bug in calculation of prior probabilities upon 
branching (was multiplying by parent's prior probability, which shouldn't
be done). Impact should only have been for calculating expected number of oracle
calls.

July 24, 2002: Cosmetic improvements for distribution.

December 26, 2003: Fixed documentation of LIMITED-ORACLE

EXAMPLES
--------

To run each example evaluate the relevant definition and then call the function
with or without a debugging argument (which should be 0 for no debugging info,
1 for a little debugging info, and 1 for a lot of debugging info). For example,
after evaluating the test-herbs-grover function definition you could try the
following calls:

```lisp
(test-herbs-grover) ;; for no debugging info
(test-herbs-grover 1) ;; for some debugging info (just results)
(test-herbs-grover 2) ;; for more debugging info
```

```lisp
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

(test-branching)
```

```lisp
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

(test-herbs-grover 1)
(test-herbs-grover 2)
```

```lisp
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

(test-evolved-grover 1)
(test-evolved-grover 2)
```

```lisp
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

(test-evolved-and-or 1)
(test-evolved-and-or 2)
```
