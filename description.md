---
title: Course description
---

### Contents

This course introduces *functional programming* through the programming language Haskell. In contrast with the language C# -- introduced in *Imperatief*/*Game*/*Mobiel programmeren* -- which is based on statements, organized in methods and classes, functional programming is based entirely on expressions and functions. This shifts the focus from *how* a program operates to *what* is does.

Concrete topics treated in this course include higher-order functions, parametric and ad-hoc polymorphism (also known as generics and overloading in other programming languages), algebraic data types and pattern matching. These ideas appear not only in Haskell, but in other modern languages such as Scala, Swift or Kotlin. An important part of the course is devoted to *reasoning about programs*, either by equations or by induction.

The language Haskell imposes a strong separation between pure computations and those with *side-effects*, such as input and output. *Monads* are introduced to model the idea of sequential computation in a functional language. Similar abstractions such as *functors* are also part of the contents of this course.

### Literature

* Required reading: [*Programming in Haskell*](http://www.cs.nott.ac.uk/~pszgmh/pih.html) by Graham Hutton
* Additional material:
    - [*Lecture Notes from 2015/2016*](http://www.cs.uu.nl/people/jur/FP-elec.pdf), mostly in Dutch
    - [*Learn You a Haskell for Great Good*](http://learnyouahaskell.com/) by Miran Lipovača
    - [*Haskell Wikibook*](https://en.wikibooks.org/wiki/Haskell)

## <a name="schedule"></a>Schedule

The load per week is 2 × 2 h. [lectures](lectures.html), 1 × 2 h. [practicals](labs.html) and 1 × 2 h. [werkcollege](exercises.html). Attendance is not strictly mandatory (albeit recommended).

## Assignments

There are two kinds of **mandatory** assignments:

* [Lab assignments](labs.html) pose programming problems with the aim of practicing your Haskell skills. These assignments are automatically corrected by [DOMJudge](https://domjudge.cs.uu.nl/dj/fp/team/). You can ask any of the teaching assistants to suggest improvements to your code style during labs sessions.
* The [final programming project](labs.html) consists of programming a small game in Haskell. The code is graded for correctness, design and style.

In addition to those mandatory assignments, there is an **optional** assignment to be made in groups to get some extra points in the grade. The assignment involves exploring and presenting some [language extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html) or [Haskell library](http://hackage.haskell.org/).

## Grading

The final grade depends on the two exams and the assignments.

- The *theory* grade is T = 0.3 × grade of mid-term + 0.7 × grade of final exam. You need T >= 5 to pass the course.
- The *practical* grade is P = 0.2 × average of the lab assignments + 0.8 × grade of final programming project. You need to pass at least two out of three lab assignments and obtain P >= 5 to pass the course.
- The *optional assignment* grade is O.

The *final grade* is computed as F = min( 0.5 × T + 0.5 × P + 0.05 × O, 10 ). The *final result* of the course is:

- Passed with grade F (rounded to 0.1 points) if F >= 5.5, T >= 5, P >= 5 and you passed at least two out of three lab assignments.
- Not passed with AANV if F >= 4.0, but T < 5 or P < 5 or you passed fewer than two of the lab assignments.
    * In this case you are entitled to *aanvullende toets*, to be determined per student.
    * If you need to re-submit the final programming exercise, that grade is a 6 maximum.
- Not passed with NVD otherwise.