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
