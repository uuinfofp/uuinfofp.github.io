---
title: Course description
---

### Contents

This course introduces *functional programming* through the
programming language Haskell. In contrast with the language C# --
introduced in *Imperatief*/*Game*/*Mobiel programmeren* -- which is
based on statements, organized in methods and classes, functional
programming is based entirely on expressions and functions. This
shifts the focus from *how* a program operates to *what* it does.

Concrete topics treated in this course include higher-order functions,
parametric and ad-hoc polymorphism (also known as generics and
overloading in other programming languages), algebraic data types and
pattern matching. These ideas appear not only in Haskell, but in other
modern languages such as Scala, Swift or Kotlin. An important part of
the course is devoted to *reasoning about programs*, either by
equations or by induction.

The language Haskell imposes a strong separation between pure
computations and those with *side-effects*, such as input and
output. *Monads* are introduced to model the idea of sequential
computation in a functional language. Similar abstractions such as
*functors* are also part of the contents of this course.

### Literature

* Required reading: [*Programming in Haskell*](http://www.cs.nott.ac.uk/~pszgmh/pih.html) by Graham Hutton
* Additional material:
    - [*Lecture Notes from 2015/2016*](http://www.cs.uu.nl/people/jur/FP-elec.pdf), mostly in Dutch
    - [*Learn You a Haskell for Great Good*](http://learnyouahaskell.com/) by Miran Lipovača
    - [*Haskell Wikibook*](https://en.wikibooks.org/wiki/Haskell)

## <a name="schedule"></a>Schedule

There are weekly [lectures](lectures.html) (2 × 2 hour),
[instructions](exercises.html) (1 × 2 hour) and
[practicals](labs.html) (1 × 2 hour). Attendance is not strictly
mandatory (albeit strongly recommended). You are expected to work on
your own in addition to these hours! Due to COVID-19 the lectures and
the practicals will be given online, through MS Teams. The instruction
sessions will be at the Utrecht Science Park ('de Uithof'). In
particular, the weekly schedule is:

![](/images/week_schedule.svg)


## Assignments

There are two kinds of **mandatory** assignments:

* [Practical assignments](labs.html) pose programming problems with the aim of practicing your Haskell skills. These assignments are automatically corrected by [DOMJudge](https://domjudge.cs.uu.nl/dj/fp/team/). You can ask any of the teaching assistants to suggest improvements to your code style during labs sessions.
* The [final programming project](labs.html) consists of programming a small game in Haskell. The code is graded for correctness, design and style.

In addition to those mandatory assignments, there is an **optional** assignment to be made in groups to get some extra points in the grade. The assignment involves exploring and presenting some [language extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html) or [Haskell library](http://hackage.haskell.org/).

## Grading

The final grade depends on the two exams and the assignments.

- The *theory* grade is T = 0.3 × grade of mid-term + 0.7 × grade of
  final exam.

  CT
  : You need T >= 5 to pass the course.

- The *practical* grade is P is grade of final programming
  project (The game).

  CP
  : You need to pass at least two out of three DomJudge assignments,
  hand in the Game Design Document, and obtain P >= 5 to pass the course.

- The *optional assignment* grade is O.

The *final grade* is computed as F = min( 0.5 × T + 0.5 × P + 0.05 ×
O, 10 ).

The *final result* of the course is:

- Passed with grade F (appropriately rounded according to the OER
rules) if F >= 5.5 and you satisfy conditions CT and CP.

- Not passed with AANV if F >= 4.0, you passed at least one DomJudge
  assignment, but you do not satisfy the conditions CT and CP.

    * In this case you are entitled to *aanvullende toets*, to be determined per student.
    * If you need to re-submit the final programming project, that grade is a 6 maximum.

- Not passed with NVD otherwise.
