val m = Model( 
  Issue("svPhys") has Gist("Physics examples lacks Swedish translation"),
  Issue("hiPhys") has Gist ("Physics examples lacks Hindi translation"),
  Resource("Bjorn") implements Issue("svPhys"), 
  Resource("Lalit") implements Issue("hiPhys"), 
  Ticket("translPhys") implements (Issue("svPhys"), Issue("hiPhys"))
) 
