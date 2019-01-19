//function: m => m  printing node to terminal
m =>
  println(m)
  m
//Goal-Design scale
Model(
  Goal("accuracy") has
    Spec("Our pre-calculations shall hit within 5%"),
  Feature("quotation") has
    Spec("Product shall support cost recording and quotation with experience data"),
  Function("experienceData") has
    Spec("Product shall have recording and retrieval functions for experience data"),
  Design("screenX") has
    Spec("System shall have screen pictures as shown in Fig. X"))
//Why+Spec+Example
Model(
  Feature("navigate") has (
    Why(
"Measuring neural response is a bit painful to the  patient. Electrodes must be kept in place ... So both hands should be at the patient during a measurement."),
    Spec(
"It shall be possible to perform the commands start, stop, ... with both hands at the patient."),
    Example(
"Might be done with mini keyboard (wrist keys), foot pedal, voice recognition, etc.")
  )
)
//Context diagram simple
Model(
  Section("context") has (
    Product("hotelApp") interactsWith (
      User("receptionist"),
      User("guest"),
      System("accounting"),
      System("telephony"))))
//Context diagram with interface
Model(
  Product("hotelApp") has (
    Interface("receptionUI") has Actor("Receptionist"),
    Interface("guestUI") has Actor("Guest"),
    Interface("phoneAPI") has System("Telephony"),
    Interface("accountAPI") has System("Accounting")),
  Data("InterfaceIO") has (
    Interface("receptionUI") has (
      Input("booking"), Input("checkOut"),
      Output("serviceNote")),
    Interface("guestUI") has (
      Output("confirmation"), Output("invoice"))))
//Data dictionary: class with spec+example+members
Model(
  Section("relations") has (
    Class("Guest") relatesTo (Class("Stay"), Min(1)),
    Class("Stay") relatesTo (Class("RoomState"), Class("Service"), Min(1)),
    Class("ServiceType") relatesTo (Class("Service"), Min(1)),
    Class("Room") relatesTo (Class("RoomState"), Min(1))),
  Section("attributes") has (
    Class("Guest") has (
      Member("name"),
      Member("address1"),
      Member("address2"),
      Member("address3"),
      Member("passport")),
    Class("Stay") has (Member("stayId"), Member("paymethod"), Member("employee")),
    Class("ServiceType") has (Member("name"), Member("price")),
    Class("Service") has (Member("serviceDate"), Member("serviceCount")),
    Class("Room") has (
      Member("roomId"),
      Member("bedCount"),
      Member("roomType"),
      Member("price1"),
      Member("price2")),
    Class("RoomState") has (Member("date"), Member("personCount"), Member("state"))))
//State transition model
Model(
  Section("roomState") has (
    Title("Room State Model"),
    State("free") has (
      Event("book") precedes State("booked"),
      Event("checkin") precedes State("occupied"),
      Event("changeRoom") precedes State("occupied"),
      Event("repair") precedes State("repairing")),
    State("booked") has (
      Event("checkIn") precedes State("occupied"),
      Event("cancel") precedes State("free")),
    State("occupied") has (
      Event("checkout") precedes State("free"),
      Event("changeRoom") precedes State("free")),
    State("repairing") has (Event("done") precedes State("free"))))
//Model with sections
Model(
  Title("Test Model"),
  Text("This is a model to test html generation."),
  Feature("topStuff") has Spec("hejsan"),
  Feature("deepTopStuff") has (Feature("Gurka") has Spec("hejsan")),
  Section("context") has (
    Text("This section describes the context of the system."),
    Image("context-diagram.svg"),
    Product("hotelApp") implements (
      Interface("receptionUI") has Actor("receptionist"),
      Interface("guestUI") has Actor("guest"),
      Interface("phoneAPI") requires System("telephony"),
      Interface("accountAPI") requires System("accounting")),
    Interface("receptionUI") has (
      Input("booking"), Input("checkOut"),
      Output("serviceNote")),
    Interface("guestUI") has (
      Output("confirmation"), Output("invoice"))
  ),
  Section("quality") has (
    Text("This section contains system-wide quality requirements."),
    Quality("databaseCapacity") has
      Spec("#guests < 10,000 growing 20% per year, #rooms < 1,000"),
    Quality("calendarAccuracy") has
      Spec("Bookings shall be possible at least two years ahead."),
    Quality("forecastPerformance") has
      Spec("Product shall compute a room occupation forecast within ___ minutes. (Customer expects one minute.)"),
    Quality("taskUsability") has
      Spec("Novice users shall perform tasks Q and R in 15 minutes. Experienced users shall perform tasks Q, R, S in 2 minutes."),
    Quality("taskUsability") relatesTo (Task("Q"), Task("R"), Task("S")),
    Quality("peakLoadPerformance") has
      Spec("Product shall be able to process 100 payment transactions per second in peak load.")
  )
)
//Prioritization $100 Method
val m = Model(
  Stakeholder("a") has (
    Prio(2),
    Req("1") has Benefit(5),
    Req("2") has Benefit(300),
    Req("3") has Benefit(8),
    Req("4") has Benefit(9),
    Req("5") has Benefit(100),
    Req("6") has Benefit(10),
    Req("7") has Benefit(20)),
  Stakeholder("b") has (
    Prio(4),
    Req("1") has Benefit(100),
    Req("2") has Benefit(7),
    Req("3") has Benefit(20),
    Req("4") has Benefit(80),
    Req("5") has Benefit(10),
    Req("6") has Benefit(90),
    Req("7") has Benefit(20)))

val shs = m.entitiesOfType(Stakeholder)
val rs = m.entitiesOfType(Req)
val prioSum = shs.map(s => m/s/Prio).sum
val benefitSum = shs.map(s => s -> (m/s).collect{ case Benefit(b) => b}.sum).toMap
val normalized = rs.map(r =>
  r has Benefit(
    math.round(shs.map(s =>
      (m/s/Prio)*(m/s/r/Benefit)*100.0 / (benefitSum(s)*prioSum)).sum).toInt)).toModel
println("\n--- Normalized, weighted priorities:\n" + normalized)
val sum = normalized.collect{ case Benefit(b) => b}.sum
println("\n--- Sum: " + sum)
normalized
//Prioritization Ordinal Ranking
val m = Vector("autoSave", "exportGraph", "exportTable", "modelTemplates", "releasePlanning", "syntaxColoring", "autoCompletion").map(Req(_)).toModel
  /* to use your tree model, replace above line with m => */
val rs = m.entitiesOfType(Req)
val pairs = scala.util.Random.shuffle(rs.combinations(2).toVector)
val rows = pairs.map{case Vector(p1,p2) => s"${p1.id} <> ${p2.id}"}.mkString("\n")
println(rows)
val fileName = "prio-ord.txt"
rows.save(fileName)
val msg1 = s"Edit file $fileName in another editor\n"
val msg2 = "Change <> to either > or <\nto reflect your priorities."
val msg3 = "Press OK when you have saved your changes in a NEW file called prio.txt"
javax.swing.JOptionPane.showMessageDialog(null,msg1+msg2+msg3)
val edited = load("prio.txt")
val ranked = reqT.parse.comparisonParser.parseAndSolve(edited,allowedDeviation=0)
if (ranked!=Model()) edit(ranked) else
  javax.swing.JOptionPane.showMessageDialog(null,"Inconsistent. See console.")
m
//Task: Hotel reception work
Model(
  Task("receptionWork") has (
    Task("booking"),
    Task("checkIn") has (
      Why("Guest wants room."),
      Frequency(3),
      Spec("Give guest a room, mark it as occupied and start account. Frequency scale is median number of check-ins/room/week. Trigger: A guest arrives. Critical: Group tour with 50 guests."),
      Task("findRoom"),
      Task("recordGuest") has
        Spec("variants: a) Guest has booked in advance, b) No suitable room"),
      Task("deliverKey"))))
//Quper
Model(
  Quality("mtts") has (
    Gist("Mean time to startup"),
    Spec("Measured in milliseconds using Test startup"),
    Breakpoint("Utility") has Value(4000),
    Breakpoint("Differentiation") has Value(1500),
    Breakpoint("Saturation") has Value(200),
    Target("basic") has (
        Value(2000),
        Comment("Probably possible with existing architecture.")),
    Target("strech") has (
        Value(1100),
        Comment("Probably needs new architecture.")),
    Barrier("first") has Value(2100),
    Barrier("second") has Value(1000),
    Product("competitorX") has Value(2000),
    Product("competitorY") has Value(3000)
  ),
  Test("startup") verifies Quality("mtts"),
  Test("startup") has (
    Spec("Calculate average time in milliseconds of the startup time over 10  executions from start button is pressed to logon screen is shown."),
    Target("stretch")
  )
)
//Quality requirements
Model(
  Quality("dbCapacity") has
    Spec("#guests < 10,000 growing 20% per year, #rooms < 1,000"),
  Quality("calendarAccuracy") has
    Spec("Bookings shall be possible at least two years ahead."),
  Quality("forecastPerformance") has
    Spec("Product shall compute a room occupation forecast within ___ minutes. (Customer expects one minute.)"),
  Quality("taskTimeUsability ") has
    Spec("Novice users shall perform tasks Q and R in 15 minutes. Experienced users tasks Q, R, S in 2 minutes."),
  Quality("taskTimeUsability") requires (Task("Q"), Task("R"), Task("S")),
  Quality("peakLoadPerformance") has
    Spec("Product shall be able to process 100 payment transactions per second in peak load."))
//Variability modelling
Model(
  Component("apperance") has (
    VariationPoint("color") has (
      Min(0), Max(2),
      Variant("blue"), Variant("red"), Variant("green")),
    VariationPoint("shape") has (
      Min(1), Max(1), Variant("round"), Variant("square")),
    VariationPoint("payment") has (
      Min(1), Max(2), Variant("cash"), Variant("credit")),
    VariationPoint("payment") requires Variant("cash"), /* mandatory */
    Variant("round") excludes Variant("red"),
    Variant("green") requires Variant("square")),
  Component("apperance") requires VariationPoint("shape"), /* mandatory */
  App("free") requires Component("apperance"),
  App("free") binds (
    VariationPoint("shape") binds Variant("round")),
  App("premium") requires Component("apperance"),
  App("premium") binds ( /* violating variability constraints */
    VariationPoint("color") binds (Variant("red"), Variant("green")),
    VariationPoint("shape") binds (Variant("round"), Variant("square")),
    VariationPoint("payment") binds Variant("cash")))
//Release planning example, simple
val simple = Model(
  Stakeholder("X") has (
    Prio(1),
    Feature("1") has Benefit(4),
    Feature("2") has Benefit(2),
    Feature("3") has Benefit(1)),
  Stakeholder("Y") has (
    Prio(2),
    Feature("1") has Benefit(2),
    Feature("2") has Benefit(1),
    Feature("3") has Benefit(1)),
  Release("A") precedes Release("B"),
  Resource("dev") has (
    Feature("1") has Cost(10),
    Feature("2") has Cost(70),
    Feature("3") has Cost(40),
    Release("A") has Capacity(100),
    Release("B") has Capacity(100)),
  Resource("test") has (
    Feature("1") has Cost(40),
    Feature("2") has Cost(10),
    Feature("3") has Cost(70),
    Release("A") has Capacity(100),
    Release("B") has Capacity(100)),
  Feature("3") precedes Feature("1"))
val problem = csp.releasePlan(simple)
val solution = problem.maximize(Release("A")/Benefit)
val sortedSolution = solution.sortByTypes(Release, Feature, Stakeholder, Resource)
sortedSolution
//Release planning example, advanced
val m = Model(
  Feature("exportHtml") has Gist("Export model to HTML, with special treatment of Section and Image."),
  Feature("exportGraphViz") has Gist("Export model to graph for visualization in GraphViz."),
  Feature("exportTabular") has Gist("Export model to table format for edit in spreadsheet apps."),
  Feature("exportLatex") has Gist("Export model to Latex, with special treatment of Section."),
  Feature("exportContexDiagramSvg") has Gist("Solve release planning problems."),
  Feature("syntaxColoring") has Gist("Syntax colored editing of models."),
  Feature("releasePlanning") has Gist("Constraint-solving support and gui for release planning."),
  Feature("autoCompletion") has Gist("Auto-completion of entity, attribute and relation types."),
  Feature("autoSave") has Gist("Save a model automatically after each update."),
  /* Feature("exportHtml") precedes Feature("exportGraphViz"), */
  Resource("TeamA") has (
    Feature("exportHtml") has Cost(9),
    Feature("exportGraphViz") has Cost(7),
    Feature("exportTabular") has Cost(3),
    Feature("exportLatex") has Cost(6),
    Feature("exportContexDiagramSvg") has Cost(3),
    Feature("syntaxColoring") has Cost(6),
    Feature("autoCompletion") has Cost(3),
    Feature("releasePlanning") has Cost(4),
    Feature("autoSave") has Cost(6),
    Release("March") has Capacity(20),
    Release("July") has Capacity(15),
    Release("later") has Capacity(1000)),
  Resource("TeamB") has (
    Feature("exportHtml") has Cost(2),
    Feature("exportGraphViz") has Cost(8),
    Feature("exportTabular") has Cost(9),
    Feature("exportLatex") has Cost(4),
    Feature("exportContexDiagramSvg") has Cost(4),
    Feature("syntaxColoring") has Cost(2),
    Feature("autoCompletion") has Cost(3),
    Feature("releasePlanning") has Cost(5),
    Feature("autoSave") has Cost(7),
    Release("March") has Capacity(15),
    Release("July") has Capacity(15),
    Release("later") has Capacity(1000)),
  Release("March") has Order(1),
  Release("July") has Order(2),
  Release("later") has Order(3),
  Stakeholder("Ada") has (Prio(1),
    Feature("exportHtml") has Benefit(10),
    Feature("exportGraphViz") has Benefit(10),
    Feature("exportTabular") has Benefit(10),
    Feature("exportLatex") has Benefit(7),
    Feature("exportContexDiagramSvg") has Benefit(6),
    Feature("syntaxColoring") has Benefit(3),
    Feature("releasePlanning") has Benefit(4),
    Feature("autoCompletion") has Benefit(7),
    Feature("autoSave") has Benefit(9))
/*  ,Stakeholder("Ben") has (Prio(1),
    Feature("exportHtml") has Benefit(1),
    Feature("exportGraphViz") has Benefit(9),
    Feature("exportTabular") has Benefit(3),
    Feature("exportLatex") has Benefit(4),
    Feature("exportContexDiagramSvg") has Benefit(7),
    Feature("syntaxColoring") has Benefit(8),
    Feature("releasePlanning") has Benefit(5),
    Feature("autoCompletion") has Benefit(10),
    Feature("autoSave") has Benefit(4))  */
  )
val solution = csp.releasePlan(m).
    maximize(Release("March")/Benefit).
    sortByTypes(Release, Feature, Stakeholder, Resource)
solution
