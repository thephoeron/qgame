# QGAME

[![DOI](https://zenodo.org/badge/15455903.svg)](https://zenodo.org/badge/latestdoi/15455903)
[![Quicklisp](http://quickdocs.org/badge/qgame.svg)](http://quickdocs.org/qgame/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Lee Spector's QGAME: Quantum Gate and Measurement Emulator, a quantum computer simulator

Unofficial Fork from http://faculty.hampshire.edu/lspector/qgame.html

Updated for SBCL and ASDF/Quicklisp. Port of GUI to Qtools/CommonQt for next release.

## Introduction

*From the original documentation*:

QGAME (Quantum Gate And Measurement Emulator) is a system, that allows a user to run quantum computing algorithms on an ordinary digital computer. Because quantum computers have complexity advantages over classical computers, any classical emulator will necessarily be less efficient than the quantum computer that it is emulating. QGAME nonetheless allows the user to find out what outputs the quantum program would produce, and with what probabilities (since quantum computation is in general not deterministic).

QGAME is based on the "quantum gate array" model of quantum computation, in which quantum "gates" (represented as square matrices) are applied to a register of qubits (via tensor product formation and matrix multiplication). QGAME always starts with all qubits having the value zero (in the state `|00...0>`), applies a sequence of gates, and returns values about the resulting state. Measurement gates cause the system to branch, following one execution path (with the appropriate quantum state collapse) for each possible value. Final measurements are made across the end-states of all of the resulting branches.

## Documentation

Documentation is available at: http://thephoeron.viewdocs.io/qgame

## Usage

Clone this project into `~/quicklisp/local-projects/`.

## System Requirements

* SBCL 1.1.7+
* Quicklisp
* Qtools

## License

Copyright &copy; 2000--2017, the authors.
