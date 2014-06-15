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
//Release planning
Model(
  Feature("autoSave") has Gist("Save changes automatically at regular intervals."),
  Feature("createPrio") has Gist("Easy entering of feature priorities by stakeholders."),
  Feature("editAttribute") has Gist("Attributes such as Gist or Description can be edited in the view."),
  Feature("editImportance") has Gist("The importance of a stakeholder's opinions should be editable."),
  Feature("editPrio") has Gist("A stakeholder's prioritisation of a requirement should be editable."),
  Feature("editRelations") has Gist("Relations of entities can be edited."),
  Feature("listView") has Gist("Requirements can be viewed as a list."),
  Feature("showUnsaved") has Gist("Show marker to indicate unsaved model."),
  Feature("viewPrio") has Gist("A stakeholder's prioritisation of requirement can be viewed."),
  Resource("Team A") has (
    Feature("autoSave") has Cost(4),
    Feature("createPrio") has Cost(9),
    Feature("editAttribute") has Cost(6),
    Feature("editImportance") has Cost(7),
    Feature("editPrio") has Cost(3),
    Feature("editRelations") has Cost(6),
    Feature("listView") has Cost(3),
    Feature("showUnsaved") has Cost(3),
    Feature("viewPrio") has Cost(6),
    Release("March") has Capacity(20),
    Release("July") has Capacity(15)
  ),
  Resource("Team B") has (
    Feature("autoSave") has Cost(5),
    Feature("createPrio") has Cost(2),
    Feature("editAttribute") has Cost(7),
    Feature("editImportance") has Cost(8),
    Feature("editPrio") has Cost(9),
    Feature("editRelations") has Cost(2),
    Feature("listView") has Cost(4),
    Feature("showUnsaved") has Cost(3),
    Feature("viewPrio") has Cost(4),
    Release("March") has Capacity(15),
    Release("July") has Capacity(15)
  ),
  Release("March") has (Prio(1), Order(0)),
  Release("July") has (Prio(1), Order(1)),
  Stakeholder("Supervisor") has (
    Prio(1), 
    Feature("autoSave") has Prio(7),
    Feature("createPrio") has Prio(10),
    Feature("editAttribute") has Prio(9),
    Feature("editImportance") has Prio(10),
    Feature("editPrio") has Prio(10),
    Feature("editRelations") has Prio(3),
    Feature("listView") has Prio(6),
    Feature("showUnsaved") has Prio(4),
    Feature("viewPrio") has Prio(7)
  )
)