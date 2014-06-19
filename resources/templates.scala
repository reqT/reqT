//Context diagram simple
Model(
  Product("hotel application") interactsWith (
    User("receptionist"), 
    User("guest"), 
    System("telephony"), 
    System("accounting")))  
//Context diagram elaborated
Model( 
  Product("hotel application") implements (
    Interface("reception UI") has Actor("receptionist"),
    Interface("guest UI") has Actor("guest"), 
    Interface("phone API") requires System("telephony"), 
    Interface("account API") requires System("accounting")), 
  Interface("reception UI") has (
    Input("booking"), Input("checkOut"), 
    Output("service note")),
  Interface("guest UI") has (
    Output("confirmation"), Output("invoice")))
//Task: Hotel reception work
Model(
  Task("reception work") has (
    Task("booking"), 
    Task("check in") has (
      Why("Guest wants room."), 
      Frequency(3), 
      Spec("Give guest a room, mark it as occupied and start account. Frequency scale is median number of check-ins/room/week. Trigger: A guest arrives. Critical: Group tour with 50 guests."), 
      Task("find room"), 
      Task("record guest") has 
        Spec("variants: a) Guest has booked in advance, b) No suitable room"), 
      Task("deliver key"))))
//Quality: accuracy, performance, usability
Model( 
  Title("reqT model of Lauesen fig ??"),
  Quality("capacity of database") has 
    Spec("#guests < 10,000 growing 20% per year, #rooms < 1,000"), 
  Quality("accuracy of calendar") has 
    Spec("Bookings shall be possible at least two years ahead."), 
  Quality("performance of forecast") has 
    Spec("Product shall compute a room occupation forecast within ___ minutes. (Customer expects one minute.)"), 
  Quality("usability of tasks") has 
    Spec("Novice users shall perform tasks Q and R in 15 minutes. Experienced users shall perform tasks Q, R, S in 2 minutes."),
  Quality("usability of tasks") relatesTo (Task("Q"), Task("R"), Task("S")),
  Quality("performance at peak load") has 
    Spec("Product shall be able to process 100 payment transactions per second in peak load.") 
)  
//Variability modelling
Model(
  VariationPoint("color") has (
    Variant("blue"), Variant("red"), Variant("green"), Min(0), Max(2)),
  VariationPoint("shape") has (
    Variant("round"), Variant("square"), Min(1), Max(1)),
  Variant("round") excludes Variant("red"),
  Variant("green") requires Variant("square"),
  Configuration("cheap") binds (
    VariationPoint("color") binds Variant("red"),
    VariationPoint("shape") binds Variant("round")),
  Configuration("expensive") binds ( /* violating variability constraints */
    VariationPoint("color") binds (Variant("red"), Variant("green")),
    VariationPoint("shape") binds (Variant("round"), Variant("square"))),
  Product("free") has Configuration("cheap"),
  Product("premium") has Configuration("expensive"))
//Release planning simple
val m = Model(
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
  Release("A") has Order(1),
  Release("B") has Order(2),  
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
    Release("B") has Capacity(100)))
(m ++ solving.releasePlanningConstraints(m)).maximize(Release("A")/Benefit)    
//Release planning elaborated
Model(
  Feature("autoSave") has Gist("Save a model automatically after each update."),
  Feature("exportGraph") has Gist("Export model to graph for visualization in e.g. GraphViz."),
  Feature("exportTable") has Gist("Export model to table format for edit in e.g. Excel."),
  Feature("modelTemplates") has Gist("Provide templates of typical models to inspire modelling."),
  Feature("releasePlanning") has Gist("Solve release planning problems."),
  Feature("syntaxColoring") has Gist("Syntax colored editing of models."),
  Feature("autoCompletion") has Gist("Auto-completion of entity, attribute and relation types."),  
  Resource("Dev") has (
    Feature("autoSave") has Cost(4),
    Feature("exportGraph") has Cost(5),
    Feature("exportTable") has Cost(2),
    Feature("modelTemplates") has Cost(7),
    Feature("releasePlanning") has Cost(20),    
    Feature("syntaxColoring") has Cost(20),
    Feature("autoCompletion") has Cost(20)),
  Resource("Test") has (
    Feature("autoSave") has Cost(1),
    Feature("exportGraph") has Cost(2),
    Feature("exportTable") has Cost(2),
    Feature("modelTemplates") has Cost(3),
    Feature("releasePlanning") has Cost(8),
    Feature("syntaxColoring") has Cost(6),    
    Feature("autoCompletion") has Cost(6)),
  Release("Alfa") has Order(0),
  Release("Beta") has Order(1),
  Stakeholder("Ada") has (Prio(1), 
      Feature("autoSave") has Benefit(10),
      Feature("exportGraph") has Benefit(9),
      Feature("exportTable") has Benefit(10),
      Feature("modelTemplates") has Benefit(3),
      Feature("releasePlanning") has Benefit(4),
      Feature("syntaxColoring") has Benefit(7),    
      Feature("autoCompletion") has Benefit(8)),
    Stakeholder("Ben") has (Prio(1), 
      Feature("autoSave") has Benefit(10),
      Feature("exportGraph") has Benefit(9),
      Feature("exportTable") has Benefit(10),
      Feature("modelTemplates") has Benefit(3),
      Feature("releasePlanning") has Benefit(4),
      Feature("syntaxColoring") has Benefit(7),    
      Feature("autoCompletion") has Benefit(8)))