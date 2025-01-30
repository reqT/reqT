**THIS PAGE CONTAINS *OLD INFORMATION* AND IS UNDER UPDATE TO reqT 4**

# Quality Requirements and Release Planning

* This page contains instructions for lab1 in the course [ETSN15 "Requirements Engineering"](https://cs.lth.se/krav/) at Lund University. 
* Before you start, visit [reqt.github.io](https://reqt.github.io/) to download reqT and get started.
* The source code of this page is available [here](https://github.com/reqT/reqT/blob/4.x/docs/lab2.md).

## Goals

To complete this lab you should develop a requirements model using [reqT](http://reqt.github.io), based on your course project. Your model should include the following sections:

  * A **Quality** section including at least one quality requirement using open target, one quality requirement using open metric and at least one Quper model.

  * A **Plan** section with a release plan of least three high-level features allocated to at least 2 releases.


## Task 0: Mandatory Preparations

  * [Lab1](http://cs.lth.se/krav/labs/lab1) is a *pre-requisite* for this lab. You need to have knowledge of how reqT works and skills in using the most important shortcuts to work efficiently.

  * *Preparation for task 1.* Quality Requirements:

    * Read Chapter 6 on Quality Requirements in the course text book [Lau](https://cs.lth.se/krav/literature/).

    * Look at the video lecture on [Quper from Öredev here](http://cs.lth.se/krav/quality-requirements/).

    * Read the paper on quality requirements [QUPER](https://cs.lth.se/krav/literature/).

    *  **Select and write down** at least three quality requirements relevant to your course project. They should cover at least two different types of quality aspects (e.g. capacity, accuracy, performance, security, usability, ...).

  * *Preparation for task 2.* Release Planning:
    
    * Read the paper on release planning [RP](https://cs.lth.se/krav/literature/).
   
    * **Select and write down** 3 features and 2 stakeholders from your project and make estimates of relative *benefit* for each feature from the viewpoint of each stakeholder, and estimates of relative *cost* for each feature from development and test perspectives, e.g. using e.g. the 100$-method from [lab1](http://cs.lth.se/krav/labs/lab1). Use fictitious estimates if necessary but aim to be realistic if possible.  Create a tables with your estimates for each feature id similar to the following:

    | Stakeholder | Feature | Benefit |
    |-------------|---------|---------|
    | X | f1 | 4 |
    | X | f2 | 2 |
    | X | f3 | 1 |
    | Y | f1 | 2 |
    | Y | f2 | 1 |
    | Y | f3 | 1 |

    | Resource | Feature | Cost |
    |----------|---------|------|
    | dev | f1 | 10 |
    | dev | f2 | 70 |
    | dev | f3 | 40 |
    | test | f1 | 40 |
    | test | f2 | 10 |
    | test | f3 | 70 |



## Task 1: Create Quality Requirements

* Create a model in the reqT tree including a `Section: quality`.

* Add at least one quality requirements with the shape `Quality: someId1 has Spec: some good spec` that is relevant for your project with a specification using the *open target* style (see Lauesen, Chapter 6). What type of quality aspect are you specifying?

* Add at least one quality requirement with the shape `Quality: someId2 has Spec: some good spec` relevant for your project with a specification using the *open metric* style (see Lauesen, Chapter 6). What type of quality aspect are you specifying?

* Select an example Quper model from the `Templates -> Quper Model` menu and then transfer the model to the tree.

* Generate a Quper SVG image by `Export -> To Quper .svg`. Open the file from a web browser, e.g. Firefox. What is the direction of the scale used in the template example; is better quality to the left or to the right?

* Based on the template in the `Templates -> Quper Model`, create a Quper model relevant to your project with at least two different targets. Use fictitious estimates if necessary but aim to be realistic if possible. How can you use Quper in your project?


## Task 2: Create a Release Plan

* Create a node in your tree called `Section: ReleasePlan1` and select the node.

* Load into the editor the script from menu `Templates -> Release plan - simple` and study the code. What is maximized? Discuss how easy it would be for you to manually find an optimal release plan. How would you go about solving this constraint problem if you were using pen and paper?

* Run the script and store the result in your tree by choosing the menu `Tree -> Replace node with Scala model in editor` *Ctrl+Shift+R*

* Study the solution. The features with zero cost in a certain release are de-allocated from that release. Which feature(s) are chosen by the constraint solver for the first release? Why?

* Change the capacity of the first release for dev and test and re-evaluate the script until the order of features in the solution is changed. If you want to try solutions directly in terminal you can choose `Editor -> Run Script => Console` *Ctrl+Enter* and then type `sortedSolution.pp` to pretty-print the solution in terminal. Experiment with different priorities of stakeholders.

* What does it mean in practice that the release planning problem is **NP-complete**? See e.g. [wikipedia Knapsack_problem](https://en.wikipedia.org/wiki/Knapsack_problem)

* Load into the editor the reqT script in menu `Templates -> Release plan - advanced` and study the code.

* Run the script and store the result in your tree by choosing the menu `Tree -> Replace node with Scala model in editor` *Ctrl+Shift+R*. Which features are allocated to the March Release?

* Add a constraint `Feature("exportHtml") precedes Feature("exportGraphViz")` by by removing the comment markers `/*` and `*/` from the Scala code. How does this constraint affect the solution found.

* Add `Stakeholder("Ben")` by removing the comment markers `/*` and `*/` from the Scala code. Choose `Editor -> Run Script => Console` *Ctrl+Enter* and then type `solution.pp` to pretty-print the solution in terminal. Experiment with different capacities of `Release("later")`. How low capacity can you allocate and still finding a solution?

* Find a solution that minimize the cost of the March release by replacing the optimization method with `minimize(Release("March")/Cost)` and compare with the previous solution.

* Create a release plan for at least three features, two stakeholders and two releases for your project.

* Add constraints using one or more `precedes`, `excludes`, `requires` and investigate how the constraints impact the solution if found.

* Discuss how you will work with release planning in your project.
