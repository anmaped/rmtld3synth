# *Porting of the formulas.py and ltlxml.py files.*

## 1\. Goals
The main goals with this porting process where the following :\
    Functional Equivalence : Ensure the ocaml implementation matches the functionality of the python code.\
    Readability : Write easy to understand (and mantain) ocaml code.\
    Optimization : Leverage ocaml tools to try and optimize the existing code.

## 2\. Ltlxml file
While python is dinamically typed, ocaml is statically typed meaning defining explicit types for "Term" and "Formula.\
The python implementation uses inheritance, while in ocaml sum types where used.\
By the nature of ocaml the ported code is much more readable and more concise than the python conterpart.

## 3\. Formulas file
While still using Z3 there where some challenges in the code implementation.\
Tranlating python list operations into ocaml static lists required more caution in the handling of indexes and lenghts.\
Some functions (such as "until" ) were changed as to better use ocaml functional oriented programming.\
Added modules for improved readability.

## 4\. Conclusion\
We successfully ported the files to the ocaml language. More effort is required for type definitions compared to python but\
in return we have a more type-safe, concise, and less error prone code.
