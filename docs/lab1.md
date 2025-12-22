# Context, Features and Priorities

* This page contains instructions for lab1 in the course [ETSN15 "Requirements Engineering"](https://cs.lth.se/krav/) at Lund University. 
* Before you start, visit [reqt.github.io](https://reqt.github.io/) to download reqT and get started.
* The source code of this page is available [here](https://github.com/reqT/reqT/blob/4.x/docs/lab1.md).

## Lab Goals

To complete this lab you should develop a requirements model using [reqT](https://reqt.github.io/). Your model should be based on your course project and should include the following sections:

  * A **context** section including a draft product context of your course project with some relevant interfaces to some relevant users. The model does not have to be complete.

  * A **features** section with at least 5 high-level features from your course project, each with a descriptive id. Each feature should also have a Gist attribute with a short description of the feature.

  * A **priorities** section with priorities for each feature based on the criteria: *which feature is most important to spend more elicitation effort on*.

## Task 0: Mandatory Preparations

* Draw an initial version of a context diagram for your project using some drawing tool, e.g. *LibreOffice Draw*, or take a photo of a hand drawing, and save it as `.png` or `.jpg` or `.svg`

* Select around 5 to 10 high-level features from your project and for each feature create a short description of one line of text.

* Name at least 2 user roles (actors) from your project.

* Make sure you have [Java 21](https://adoptium.net/temurin/releases/?package=jdk&version=21) and [Scala 3](https://www.scala-lang.org/download/) installed for your system. Check this in terminal using:</br>`java -version`</br>`scala -version`

* Make sure you can launch the reqT Desktop app in terminal using: `java -jar reqT.jar`

* Study the slides of lectures [L1-L4]https://lunduniversity.github.io/krav/#lectures), and the [reqT homepage](https://reqt.github.io/).

* Study the [reqT Quickref](https://github.com/reqT/reqT-lang/releases/latest/download/reqT-quickref-GENERATED.pdf)

* At the beginning of the lab session be prepared to:
  * Show your context diagram, list of features with one-line descriptions, and stakeholders.
  * Answer these questions:
      * What are the differences between an entity `Ent` and an attribute `Attr`?
      * What types of instances can be connected with a `Rel` instance?
      * What makes `Model` a recursive data structure?

## Task 1: Investigate the reqT Desktop app GUI

* Launch the reqT Desktop app.

* Study the help text in the **Log** pane.

* Check out the items of each menu and their keyboard shortcuts. You can open the **File** menu with `Alt+F` and then navigate through the menus using the arrow keys. You can see the `Alt`-key shortcut of each menu by its underscored letter.
  * The left pane is connected to the **File** menu. 
  * The middle pane is connected to the **Editor** menu. 
  * The right pane is connected to the **Log** menu.


## Task 2: Create a `context` section

* In the **Editor** pane: create a reqT Model of your project context diagram from your lab preparations in Task 0 above, using reqT. Use entities in a similar way as in the example in the **Templates** menu item *Context diagram: simple*, also shown below.

```
* Title: Context Diagram, simple
* Section: context has
  * Product: hotelApp interactsWith
    * User: receptionist
    * User: guest
    * System: accounting
    * System: telephony 
```

* Replace the top node in the **Tree** with your model from the **Editor**. 

* Make sure that you understand how to transfer Model parts back and forth between the independent **Tree** and **Editor** panes.

* Save your reqT model.

* Export your reqT model as html.

* If a model contains an **Image** entity that has a **Location** attribute with a file name or URL to a file, a link will be included in the generated html code. Include something similar to the below **Image** entity and generate a html page that links to your context diagram figure in png or jpg.
  ```
  * Title: Context Diagram, simple
  * Section: context has
    * Image: ctxFig has Location: my-file.png
    * Product: hotelApp interactsWith
      * User: receptionist
      * User: guest
      * System: accounting
      * System: telephony 
  ```

* *Optional*. If you are on a machine with [Graphviz](https://graphviz.org/) installed (LTH student computers in the E-building have Graphviz installed), then generate a GraphViz diagram from the **Export** menu. 

* *Optional*. Study the template *"Context diagram: interfaces"* in the **Templates** menu and make a more detailed context diagram of your project including a model of interfaces with input/output data.

## Task 3: Create a `features` section  

* Add a `Section: features` to your model and include your features according to the following example. Each feature should have a short id that use camel case capitalization. Place your feature description in a `Gist` attribute. 

```
* Section: features has
  * Feature: checkIn has 
    * Gist: Guests can be checked in upon arrival.
  * Feature: checkOut has 
    * Gist: Guests can be checked out after paying and leaving.
  * Feature: roomSearch has 
    * Gist: Receptionists can find available rooms based on search criteria.
  * Feature: webBooking has 
    * Gist: A guest can book a future stay via a web interface.
  * Feature: groupCheckIn has 
    * Gist: Many guest arriving simultaneously can be checked in as a group.
```

* Append the Section to your Tree. Select different Model unparsing methods using the **Tree** menu toggle items *Markdown*, *Scala constructors*, *Scala classes*.  

* Use the menu item *Keep Distinct Entities* in the **Tools** menu. Note that the tools in the Tool menu operate on the contents of the Editor pane.

* Remove the entities that you don't want to include in the prioritization of next step, e.g. you can remove the Section entity.

## Task 4: Prioritize your features

* **Ordinal-scale prioritization using par-wise comparison**:
    * Make sure you have relevant entities in the Editor pane from the previous step.

    * Use the menu item *Id Pairs as Comparison Constraints* in the **Tools** menu to add all pair-wise comparisons on separate lines in a Constraint attribute. 

    * Use the menu item *Solve Comparison Constraint Problem* in the **Tools** menu to find a solution that satisfy all constraints in a consistent ranking in Order attributes.

    * Revisit and change all comparisons to either `<` or `>` depending on your actual prioritization according to this criteria of each pair of entities A and B:

        * *A is more/less important to spend more elicitation effort on than B*.

    * Use the menu item *Solve Comparison Constraint Problem* in the **Tools** menu to find a new solution to the updated constraint problem. You should get a model with priorities calculated in **Order** attributes as a solution to a constraint problem based on your comparisons. A higher Order value means more important.

    * Write down reflections on the result:

        * Are the priorities reflecting your gut feeling of "importance" according to the prioritization criteria?

        * Circular contradictions make the constraint problem inconsistent and thus unsolvable, but the constraint solver in reqT does not give up! Instead a *relaxed deviation error* `d` is allowed starting with `d = 1` and then retrying with `d += 1` until a solution can be found. This means that each priority rank `r` is representing any value within an error margin `[r - d, r + d]`.

        * If you had no inconsistencies when solving the constraint problem, then introduce some circular consistency among 3 features just to test what happens.

        * Given the minimum relaxed deviation needed for a solution to exist with your contradictory comparisons: find some features in your resulting priority rank order that could have be swapped and still fulfill the constraints if the deviation `d` is allowed.

        * Is it easy to be consistent? Discuss the difficulty of making consistent pairwise comparisons as the number of compared objects increase. Discuss if or how the risk of being consistent might depend on the prioritization criteria. Could/should the level of deviation be used as an indicator of the quality of the human judgment?

        * *Optional*. Study the source code for ordinal-scale prioritization here: [ `doSolveComparisonConstraints()` in the file `MainWindow.scala`](https://github.com/reqT/reqT/blob/4.x/src/main/scala/MainWindow.scala#L555) and try to figure out roughly what happens step-by-step.

* **Ratio-scale prioritization using the 100-dollar method**:

    * Choose the template *"Prioritization: 100$ test"* from the **Templates** menu. Study the model and explain what it means.

    * Choose menu item *"100$-test Normalized Votes"* from the **Tools** menu to append a calculation of normalized resulting votes. Explain how the calculation is done.

    * Do a similar 100$-test for you own features.

    * Write down reflections on the result:
        * Was it easy to assign ratio scale priorities?
    
    * *Optional*. Study the source code for ratio-scale prioritization [in the `normalizedVotes` method here](https://github.com/reqT/reqT-lang/blob/main/src/main/scala/06-examples.scala#L368) called from [here](https://github.com/reqT/reqT/blob/4.x/src/main/scala/MainWindow.scala#L552) and try to figure out roughly what happens step-by-step. Explain how the benefits values of each stakeholder are weighted together to normalized total benefit values. 

* **Ordinal-scale prioritization using sorting**:

    * Use *Tools -> Keep Distinct Entities* and keep the entities you want to prioritize.

    * Use the ALT+ARROW_UP and ALT+ARROW_DOWN keys to sort your entities in priority order by moving each up and down to reflect what you think is most important according to a specific criteria.

    * Use *Tool -> Entity order i Ordering* to append your priorities.

    * Write down reflections on the result:
      * Was it easy to assign ordinal scale priorities?
      * What are the pros and cons of ratio-scale 100$-test versus ordinal-scale prioritization with pairwise comparison versus ordinal scale prioritization with sorting?


## Task 5 (Optional): Dig deeper into reqT

Do this optional task if you still have time left at the lab session.

* Download [hello-reqt.scala](https://github.com/reqT/reqT.github.io/blob/master/src/hello-reqt.scala) and place it in a suitable folder.
```
curl https://raw.githubusercontent.com/reqT/reqT.github.io/refs/heads/master/src/hello-reqt.scala -O hello-reqt.scala
```

* Open a terminal and navigate to the folder with the scala program and run it with: 
```
scala run hello-reqt.scala
```

* Open the program in [vscode](https://code.visualstudio.com/) with the [Scala Metals extension](https://marketplace.visualstudio.com/items?itemName=scalameta.metals)

* Run the `hello-reqt.scala` program from within vscode.

* Try out some methods on Model by developing and running your scala program in vscode. 
```scala
$ scala repl -S 3.6.4 hello-reqt.scala -- --repl-init-script "import reqt.*"

Welcome to Scala 3.6.4 (21.0.6, Java OpenJDK 64-Bit Server VM).
Type in expressions for evaluation. Or try :help.

scala> val f = Feature("x")
val f: reqt.Ent = Ent(Feature,x)

scala> val f = Feature("greeting")
val f: reqt.Ent = Ent(Feature,greeting)

scala> val s = Spec("say hello")
val s: reqt.StrAttr = StrAttr(Spec,say hello)

scala> val r = f.has(s)
val r: reqt.Rel = Rel(Ent(Feature,greeting),Has,Model(StrAttr(Spec,say hello)))

scala> r.show
val res0: String = Feature("greeting").has(Spec("say hello"))

scala> val m = Model(r)
val m: reqt.Model = Model(Rel(Ent(Feature,greeting),Has,Model(StrAttr(Spec,say hello))))

scala> println(m.show)
Model(
  Feature("greeting") has Spec("say hello"),
)

scala> val ns = m.nodes
val ns: Vector[reqt.Node] = Vector(Ent(Feature,greeting), StrAttr(Spec,say hello))

scala> m.<TAB>   // see methods on Model m
```