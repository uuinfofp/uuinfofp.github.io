---
title: Lectures
---

### Literature

* Required reading: [*Programming in Haskell*](http://www.cs.nott.ac.uk/~pszgmh/pih.html) by Graham Hutton
* Additional material:
    - [*Lecture Notes from 2015/2016*](http://www.cs.uu.nl/people/jur/FP-elec.pdf), mostly in Dutch
    - [*Learn You a Haskell for Great Good*](http://learnyouahaskell.com/) by Miran Lipovača
    - [*Haskell Wikibook*](https://en.wikibooks.org/wiki/Haskell)

### Slides

Be aware: the following schedule and the contents of the slides are
currently still subject to change.

<table class="table table-striped table-hover" style="font-size: 14px;">
  <thead>
    <tr>
      <th>Week</th>
      <th>Date</th>
      <th>Slides</th>
      <th>Preparatory reading</th>
      <th>Lecturer</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>36</td>
      <td>Tue 7 Sep</td>
      <td><a href="slides/fp-01-intro.pdf">1. Functional Programming? Haskell?</a></td>
      <td>Chapters 1 and 2</td>
      <td>Frank</td>
    </tr>
    <tr>
      <td></td>
      <td>Thu 9 Sep</td>
      <td><a href="slides/fp-02-basics.pdf">2. Basics</a>
      </td>
      <td>Chapters 4 (up to 4.4) and 3
        <br>Chapter 5 from the <a
                                 href="http://www.staff.science.uu.nl/~hage0101/FP-elec.pdf">Lecture
          Notes</a></td>
      <td>Frank</td>
    </tr>
    <tr>
      <td>37</td>
      <td>Tue 14 Sep</td>
      <td><a href="slides/fp-03-lists.pdf">3. Lists and recursion</a>
  </td>
      <td>Chapters 5 and 6</td>
      <td>Frank</td>
    </tr>
    <tr>
      <td></td>
      <td>Thu 16 Sep</td>
      <td><a href="slides/fp-04-h-o-functions.pdf">4. Higher-order functions</a>
        <br><a href="applyAllFold.html">Writing <tt>applyAll</tt> as a fold</a>
        <br><a href="slides/Lecture4.hs">Example Haskell code from lecture</a>
        <br><a href="slides/Lecture4.py">Example Python code from lecture</a>
        </td>
      <td>Chapter 7 and 4.5-4.6</td>
      <td>Matthijs</td>
    </tr>
    <tr>
      <td>38</td>
      <td>Tue 21 Sep</td>
      <td><a href="slides/fp-05-data-classes.pdf">5. Data types and type classes</a>
        <br><a href="slides/Lecture5.hs">Example code from lecture</a>
        </td>
      <td>Chapter 8 (until 8.6)</td>
      <td>Matthijs</td>
    </tr>
    <tr>
      <td></td>
      <td>Thu 23 Sep</td>
      <td><a href="slides/fp-06-data-structures-new.pdf">6. Data
        structures</a><br/>
        <a href="slides/sweep.hs">The Code for the point location example</a>
      </td>
      <td></td>
      <td>Frank</td>
    </tr>
    <tr>
      <td>39</td>
      <td>Tue 28 Sep</td>
      <td><a href="slides/fp-07-case-studies.pdf">7. Case studies</a>
        <br><a href="slides/fp-qa-2020.pdf">Q&A session</a>
        <!-- <br/><a href="trees.html">The problem statements for the Tree exercises</a> -->
     </td>
      <td>Chapters 8.6</td>
      <td>Frank</td>
    </tr>
    <tr>
      <td></td>
      <td>Thu 30 Sep</td>
      <td><a href="slides/fp-08-project-design-test.pdf">8. Project
  management and design</a></td>
      <td></td>
      <td>Frank</td>
    </tr>
    <tr class="warning">
      <td>40</td>
      <td>Tue 5 Oct 11:30-13:30</td>
      <td><b>Midterm exam</b></td>
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td></td>
      <td>Thu 7 Oct<br /></td>
      <td><a href="slides/fp-09-io.pdf">9. Input and output</a>
        <br><a href="slides/Lecture9.hs">Example code from lecture</a>
        </td>
      <td>Chapter 10</td>
      <td>Matthijs</td>
    </tr>
    <tr>
      <td>41</td>
      <td>Tue 12 Oct</td>
      <td><a href="slides/fp-10-monads-one.pdf">10. Functors and monads</a>
        <br><a href="slides/Lecture10.hs">Example code from lecture</a>
        </td>
      <td>Chapter 12 (except 12.2)</td>
      <td>Matthijs</td>
    </tr>
    <tr>
      <td></td>
      <td>Thu 14 Oct</td>
      <td><a href="slides/fp-11-laws.pdf">11. Laws and induction</a>
        <br><a href="slides/Lecture11.hs">Example proofs from lecture</a>
        </td>
      <td>Chapter 16 (up to 16.6)
        <br>Chapter 13 from the <a href="http://www.staff.science.uu.nl/~hage0101/FP-elec.pdf">Lecture Notes</a>
        <br>More about correctness (optional):
        <ul>
          <li><a href="https://www.youtube.com/watch?v=vQrutfPAERQ">Intro to LiquidHaskell</a></li>
          <li><a href="https://www.youtube.com/watch?v=X36ye-1x_HQ">Intro to Idris</a></li>
        </ul></td>
      <td>Matthijs</td>
    </tr>
    <tr>
      <td>42</td>
      <td>Tue 19 Oct</td>
      <td><a href="slides/fp-13-quickcheck.pdf">12. Testing</a>
         <br><a href="slides/lectureTesting.hs">Code from the lecture</a>
      </td>
      <td><a href="http://book.realworldhaskell.org/read/testing-and-quality-assurance.html">Chapter 11</a> of <i>Real World Haskell</i></td>
      <td>Frank</td>
    </tr>
    <tr>
      <td></td>
      <td>Thu 21 Oct</td>
      <td><a href="slides/fp-12-lazy-eval.pdf">13. Lazy evaluation</a></td>
      <td>Chapter 15</td>
      <td>Frank</td>
    </tr>
    <tr>
      <td>43</td>
      <td>Tue 26 Oct</td>
      <td><a href="slides/fp-14-monads-two.pdf">14. More monads and applicatives</a>
        <br><a href="slides/Lecture14Live.hs">Example code from lecture</a>
        </td>
      <td>Chapter 12.2</td>
      <td>Matthijs</td>
    </tr>
    <tr>
      <td></td>
      <td>Thu 28 Oct</td>
      <td>Guest lecture: Haskell ideas in modern programming</td>
      <td></td>
      <td>Alejandro Serrano Mena</td>
    </tr>
    <tr>
      <td>44</td>
      <td>Tue 2 Nov<br /></td>
      <td>Q&A session (for exam prep)
        <br><a href="slides/Test.hs">Example questions</a>
        <br><a href="slides/Test-Answers.hs">Example questions solutions</a>
      </td>
      <td></td>
      <td>Matthijs</td>
    </tr>
    <tr>
      <td></td>
      <td>Thu 4 Nov<br /></td>
      <td>No class -- prepare for the exam!
        <!-- <br><a href="slides/Lecture14-exam-prep.hs">Solutions to example questions</a> -->
      </td>
      <td></td>
      <td></td>
    </tr>
    <tr class="warning">
      <td>45</td>
      <td>Thu 11 Nov 19:00-22:00</td>
      <td><b>Final exam</b></td>
      <td></td>
      <td></td>
    </tr>
    </tr><tr class="warning">
      <td>01</td>
      <td>Tue 11 Jan 09:00-11:00</td>
      <td><b>Retake exam</b></td>
      <td></td>
      <td></td>
    </tr>
  </tbody>
</table>
