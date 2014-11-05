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
  Product("hotelApp") interactsWith (User("receptionist"), User("guest"), System("accounting")), 
  User("guest") interactsWith Product("hotelApp"), 
  User("receptionist") interactsWith Product("hotelApp"), 
  System("telephony") interactsWith Product("hotelApp"))
//Context diagram with interface
Model( 
  Product("HotelApp") has (
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
//Model with sections
Model(
  Title("Test Model"),
  Text("This is a model to test html generation."),
  Feature("topStuff") has Spec("hejsan"),
  Feature("deepTopStuff") has (Feature("Gurka") has Spec("hejsan")),
  Section("Context") has (
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
  Section("Quality") has (
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
//Prioritization Ordinal ranking 
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
    Breakpoint("utility") has Value(4000), 
    Breakpoint("differentiation") has Value(1500), 
    Breakpoint("saturation") has Value(200),
    Target("basic") has (
        Value(2000), 
        Comment("Probably possible with existing architecture.")),
    Target("strech") has (
        Value(1100),
        Comment("Probably needs new architecture.")),
    Barrier("first") has (Min(1900), Max(2100)),
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
//Release planning example 1
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
val solution = csp.releasePlan(simple).
    maximize(Release("A")/Benefit).
    sortByTypes(Release, Feature, Stakeholder, Resource)
solution 
//Release planning example 2
val m = Model(
  Feature("autoSave") has Gist("Save a model automatically after each update."),
  Feature("exportGraph") has Gist("Export model to graph for visualization in e.g. GraphViz."),
  Feature("exportTable") has Gist("Export model to table format for edit in e.g. Excel."),
  Feature("modelTemplates") has Gist("Provide templates of typical models to inspire modelling."),
  Feature("releasePlanning") has Gist("Solve release planning problems."),
  Feature("syntaxColoring") has Gist("Syntax colored editing of models."),
  Feature("autoCompletion") has Gist("Auto-completion of entity, attribute and relation types."),  
  Feature("exportTable") precedes Feature("exportGraph"),   
  Resource("Dev") has (
    Feature("autoSave") has Cost(4),
    Feature("exportGraph") has Cost(5),
    Feature("exportTable") has Cost(2),
    Feature("modelTemplates") has Cost(7),
    Feature("releasePlanning") has Cost(20),    
    Feature("syntaxColoring") has Cost(20),
    Feature("autoCompletion") has Cost(20),    
    Release("A") has Capacity(30),
    Release("B") has Capacity(50)),
  Resource("Test") has (
    Feature("autoSave") has Cost(1),
    Feature("exportGraph") has Cost(2),
    Feature("exportTable") has Cost(2),
    Feature("modelTemplates") has Cost(3),
    Feature("releasePlanning") has Cost(8),
    Feature("syntaxColoring") has Cost(6),    
    Feature("autoCompletion") has Cost(6),    
    Release("A") has Capacity(20),
    Release("B") has Capacity(20)),
  Release("A") has Order(1),
  Release("B") has Order(2),
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
val solution = csp.releasePlan(m).
    maximize(Release("A")/Benefit).
    sortByTypes(Release, Feature, Stakeholder, Resource)
solution