Model( 
  Issue("svPhys") has Gist("Physics stuff lacks Swedish translation"),
  Issue("hiPhys") has Gist ("Physics stuff lacks Hindi translation"),
  Resource("Bjorn") implements Issue("svPhys"), 
  Resource("Lalit") implements Issue("hiPhys"), 
  Ticket("translPhys") implements (Issue("svPhys"), Issue("hiPhys"))
) 
