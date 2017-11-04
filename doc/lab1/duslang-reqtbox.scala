Model(
  Title("The DuSlang requirements specification"), 
  Section("context") has (
    Section("stakeholders") has (
      User("videoCreator") has Spec("______"), 
      User("videoViewer") has Spec("______")), 
    Section("product"), 
    Section("systems") has (System("ReklamSinne") has Spec("______")), 
    Section("interfaces")), 
  Section("intentions") has (
    Section("goals") has (
      Goal("advertisementDistribution") has 
        Spec("DuSlang shall ensure that advertisements are received by (shown to) the videoCreator and videoViewer"), 
      Goal("freeVideoDistribution") has 
        Spec("DuSlang shall ensure that videoCreators may distribute their media freely (as in a zero cost upload)")), 
    Section("priorities"), 
    Section("risks"), 
    Section("commitments")), 
  Section("requirements") has (
    Section("functions") has (
      Feature("subscriptions") has (
        Why("To enable a videoViewer to follow their favorite videoCreator"), 
        Spec("The videoViewer should be able to get an overview of the latest videos from their favorite videoCreators")), 
      Feature("favorites") has (
        Why("So that a videoViewer may easily access an older video they watched"), 
        Spec("______"), 
        Example("______")), 
      Feature("history") has (Why("______"), Spec("______")), 
      Feature("videoViews") has (
        Why("______"), 
        Spec("Views shall be recorded when a video has been viewed for more than 10 seconds - or when the full length has been viewed")), 
      Feature("videoLikes") has (
        Why("______"), 
        Spec("Record the amount of like votes a video has received from its viewers")), 
      Feature("videoDislikes") has (
        Why("______"), 
        Spec("Record the amount of dislike votes a video has received from its viewers")), 
      Feature("recommendedVideos") has Spec("______"), 
      Feature("uploadVideo") has Spec("______"), 
      Feature("playVideo") has Spec("______"), 
      Feature("playAdvertisement") has (
        Why("______"), 
        Spec("An advertisement should be played before playing the video to be viewed by the user")), 
      Feature("useAdvertisement") has (
        Why("So that it is possible to make use of the advertised content"), 
        Spec("The videoViewer should be able to access the contents of the viewed advertisements"))), 
    Section("data"), 
    Section("qualities"), 
    Section("tests")), 
  Section("delivery") has (
    Section("roadmap"), 
    Section("resources"), 
    Section("constraints"), 
    Section("releasePlan")))