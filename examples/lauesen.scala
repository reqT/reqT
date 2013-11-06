//examples based on Lauesen, S. "Software Requirements - styles and techniques" (2002) 

val contextDiagramExample = Model( 
  Product("Hotel system") owns (
    Interface("ReceptionUI"), 
    Interface("GuestUI"), 
    Interface("TelephonyAPI"), 
    Interface("AccountingAPI")
  ), 
  Product("Hotel system") has Image("context-diagram.png"), 
  Interface("ReceptionUI") has (
    Input("booking, check-out"), 
    Output("service note"),
    Image("receptionUI-screen.png")
  ), 
  Interface("GuestUI") has (
    Output("confirmation, invoice"), 
    Image("guestUI-screen.png")
  ), 
  Actor("Receptionist") requires Interface("ReceptionUI"), 
  Actor("Guest") requires Interface("GuestUI"), 
  Actor("Receptionist") requires Interface("ReceptionUI"), 
  Actor("Telephony System") requires Interface("TelephonyAPI"),
  Actor("Accounting System") requires Interface("AccountingAPI") 
)

Model( 
  Task("reception work") owns (Task("check in"), Task("booking")),
  Task("check in") has ( 
    Why("Guest wants room."),
    Trigger("A guest arrives"), 
    Frequency(3), 
    Spec("Give guest a room. Mark it as occupied. Start account. 	Frequency scale: median #check-ins/room/week"),
    Critical("Group tour with 50 guests.") 
  ), 
  Task("check in") owns (
    Task("find room"), Task("record guest"), Task("deliver key")
  ),
  Task("record guest") has Spec(
    "variants: a) Guest has booked in advance, b) No suitable room"
  ) 
)


val qualityReqtExample = Model( 
  Quality("capacity.database") has 
    Spec("#guests < 10,000 growing 20% per year, #rooms < 1,000"), 
  Quality("accuracy.calendar") has 
    Spec("Bookings shall be possible at least two years ahead."), 
  Quality("performance.forecast") has 
    Spec("Product shall compute a room occupation forecast within ___ minutes. (Customer expects one minute.)"), 
  Quality("usability.taskTime") has 
    Spec("Novice users shall perform tasks Q and R in 15 minutes. Experienced users shall perform tasks Q, R, S in 2 minutes."),
  Quality("usability.taskTime") requires (Task("Q"), Task("R"), Task("S")),
  Quality("performance.peakLoad") has 
    Spec("Product shall be able to process 100 payment transactions per second in peak load.") 
)