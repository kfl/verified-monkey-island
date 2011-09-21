(* general rules *)
Rule Useable(x,y,d)
   {x.loc = Inv, y.loc = player.loc} ? useon(x, y) >> d
   {y.loc = Inv, x.loc = player.loc} ? useon(x, y) >> d
   {x.loc = Inv, y.loc = Inv} ? useon(x, y) >> d
   (*{y.loc = player.loc, x.loc = player.loc} ? useon(x, y) >> [d] *)

Rule Pickupable(x)
  {x.loc != Inv, x.loc = player.loc} ? pickUp(x) >> [x.loc := Inv]  & "You pick up an item"

Story MI2part1

(* initial state *)

Initial { 
  Not(knowPolish),
  Not(paper.hasspit),
  Not(rat.inbox),
  Not(tried),
  Not(rat.trapped),
  "money" ::= "None",
  "riches.loc" ::= "Inv", 
  "player.loc" ::= "WoodtickRight", 
  "ldoor" ::= "Closed", 
  Not(hasbeenrobbed), 
  "hatch" ::= "Closed",
  Not(hatch.sticked),
  Not(hatch.stringed),
  Not(talkedaboutLargo),
  Not(hasbeenhired),
  "largoembargo" ::= "Enforced",
  Not(bucketask),
  "bucket.cont" ::= "Empty",
  "job" ::= "Unavailable",
  Not(isHired),
  "ticket.loc" ::= "Undefined",
  Not(largohasdirtyshirt),
  "spit.loc" ::= "Undefined",

  "shovel.loc" ::= "WoodtickRight",
  "cheesesq.loc" ::= "Inn",
  "toupee.loc" ::= "Largosroom", 
  "bucket.loc" ::= "LaundryShip", 
  "bra.loc" ::= "LaundryShip", 
  "rat.loc" ::= "LaundryShip", 
  "paper.loc" ::= "WallysShip", 
  "monocle.loc" ::= "WallysShip", 
  "knife.loc" ::= "Kitchen", 
  (*shoppinglist,  *)
  "polish.loc" ::= "Woodsmith", 
  "string.loc" ::= "VoodooShack" , 
  "doll.loc" ::= "VoodooShack", 
  "pins.loc" ::= "VoodooShack", 
  "bones.loc" ::= "Graves", 
  "stick.loc" ::= "Beach" 
}


(* game story rules structured by locations *)
Location { name = "WoodtickRight",
           locals = ["path", "bridge"],
  Pickupable(shovel)
  {Not(hasbeenrobbed) } ? {
    walkto(path)      >> [ "riches.loc" ::= "Undefined", hasbeenrobbed ]  & "Largo comes and take your riches"
    walkto(bridge)    >> [ "riches.loc" ::= "Undefined", hasbeenrobbed ]  & "Largo comes and take your riches"
  }

  {hasbeenrobbed } ? {
    walkto(path)   >> [ "player.loc" ::= "Map" ]  & "You are now looking at the map of Scabb Island"
    walkto(bridge) >> [ "player.loc" ::= "WoodtickLeft" ]  & "You are among several ships"
  }
   
Location { name = "WoodtickLeft",
           locals = ["wallyDoor", "laundry", "window", "hatch", "wooddoor", "inndoor", "bridge"],
  { largohasdirtyshirt } ? 
	  walkto(laundry)  >> [ "ticket.loc" ::= "Largosroom", Not(largohasdirtyshirt), "player.loc" ::= "LaundryShip" ] 
                         & "You are at the laundry. Largo is talking to the laundry guy"

   walkto(wallyDoor) >> [ "player.loc" ::= "WallysShip" ]  & "You enter the cartographers ship"
   walkto(laundry)   >> [ "player.loc" ::= "LaundryShip"]  & "You are at the laundry"
   walkto(window)    >> [ "player.loc" ::= "Kitchen" ] 
                         & "You jump through the window very elegantly and fall into the kitchen of the Bloddylips Inn"
   walkto(hatch)     >> [ "player.loc" ::= "BloddyLips" ]  & "You enter the BloddyLips Inn"
   walkto(wooddoor)  >> [ "player.loc" ::= "Woodsmith" ]  & "You are in the Woodsmith's place"
   walkto(inndoor)   >> [ "player.loc" ::= "Inn" ]  & "You enter the Inn. There's rumors Largo has a room here"
   walkto(bridge)    >> [ "player.loc" ::= "WoodtickRight" ]  & "You are just outside Woodtick"


Location { name = "Inn",
           locals = ["rope", "alligator", "innkeeper", "door", "doorway"],

  walkto(doorway) >> [ "player.loc" ::= "WoodtickLeft" ]  & "You are among several ships"

  { "knife.loc" == "Inv", "rope.loc" == "Inn"  } ?
    useon(knife, rope) >> [
      "rope.loc" ::= "Undefined", 
      "innkeeper.loc" ::= "Undefined", 
      "alligator.loc" ::= "Undefined"
    ]  & "You cut the rope holding the Alligator. It runs out closely chased by the innkeeper"
  
  { Not("alligator.loc" == "Inn"), "cheesesq.loc" == "Inn" } ?
    pickUp(cheesesq) >> [ "cheesesq.loc" ::= "Inv" ]  & "You pick up delicious cheese squiggles"

  { "ldoor" == "Closed" , Not("innkeeper.loc" == "Inn") } ?
    oopen(door) >> [ "ldoor" ::= "Open" ]  & "The door to Largo's room is now open"

  { "ldoor" == "Open" } ? {
    walkto(door) >> [ "player.loc" ::= "Largosroom" ]  & "You enter Largo's room"
    close(door)  >> [ "ldoor" ::= "Closed"]  & "The door to Largo's room is now closed"
  }

Location { name = "Largosroom",
           locals = ["door"],
  Pickupable(toupee)
  { "ldoor" == "Open" } ? { 
    close(door) >> [ "ldoor" ::= "Closed" ]  & "The door closes -- almost"
    walkto(door) >> [ "player.loc" ::= "Inn" ]  & "You are back in the inn"
  }

  { "ldoor" == "Closed", "bucket.cont" == "Mud" } ? 
    useon(bucket, door) >> [ largohasdirtyshirt, "bucket.cont" ::= "Empty" ] 
                          & "You put the bucket'o'mud on the top of the door. Largo enters and the bucket ends up on his head. Very funny."

  { "ldoor" == "Closed" } ? {
    oopen(door) >> [ "ldoor" ::= "Open" ]  & "You open the door"
    { "ticket.loc" == "Largosroom" } ?
        pickUp(ticket) >> [ "ticket.loc" ::= "Inv" ]  & "You got the cleaning ticket for Largo's clothes"
  }

  {"largoembargo" == "Enforced", "doll.loc" == "Inv", "pins.loc" == "Inv"} ?
    useon(doll, pins) >> ["largoembargo" ::= "Alleviated", "player.loc" ::= "VoodooShack"]
                        & "You quickly stap the doll with the pins. Largo is clearly upset and leaves Scapp Island. You are now back at the voodooschack"

Location { name = "LaundryShip",
           locals = ["laundyGuy", "path"],
  { Not(hatch.sticked), "hatch" == "Open"} ? walkto(path) >> ["player.loc" ::= "WoodtickLeft", "hatch" ::= "Closed"] 
                                         & "You hear the hatch close as you go back to the several-ships place"
  { hatch.sticked } ? walkto(path) >> [ "player.loc" ::= "WoodtickLeft" ]  & "You are among several ships"
  walkto(path) >> [ "player.loc" ::= "WoodtickLeft" ]  & "You are among several ships"


  { "ticket.loc" == "Inv"} ? 
    give(ticket, laundyGuy) >> ["ticket.loc" ::= "Undefined", "bra.loc" ::= "Inv"] 
                                & "You give the ticket to the deaf laundry guy. You now have Largo's bra!"
  
  { "hatch" == "Closed" } ? oopen(hatch) >> [ "hatch" ::= "Open" ]  & "You hold the hatch open"

  { "hatch" == "Open" } ? {
    { Not(hatch.sticked), "cheesesq.loc" == "Inv" } ?
      useon(cheesesq,hatch) >> [ "hatch" ::= "Closed", "cheesesq.loc" ::= "LaundryShip" ] 
                            & "You put the cheese squiggles into the box. The hatch closes."
    { Not(hatch.sticked), "stick.loc" == "Inv" } ?
      useon(stick,hatch) >> [ hatch.sticked, "stick.loc" ::= "LaundryShip" ] 
                         & "The hatch is now held open by the stick."
    { hatch.sticked, "cheesesq.loc" == "Inv" } ?
      useon(cheesesq,hatch) >> [ "cheesesq.loc" ::= "LaundryShip", rat.inbox ] 
                            & "You put the cheese squiggles into the box. The rat enters the box."
  }

  { "bucket.loc" == "LaundryShip" } ? {
    { Not(tried) } ? trypickUp(bucket) >> [ tried ] 
                    & "You try to pick up the bucket, but the men of low moral fiber tell you not to pick up the bucket"
    { tried } ? {
      talkto(molmf, yours) >> [ "bucket.loc" ::= "Inv"] 
                             & "You ask if the bucket is theirs. It's not, so you get it"
      { Not(bucketask) } ? talkto(molmf, please) >> [ bucketask ] 
                          & "You kindly ask to have the bucket, but they persist not to let you have the bucket"
      { bucketask } ? talkto(molmf, please) >> ["bucket.loc" ::= "Inv" ]  & "Finally the men of low moral fiber give up and let you have the bucket"
      }
    }


  { hatch.sticked, "string.loc" == "Inv"} ? 
    useon(string,stick) >> [ hatch.stringed, "string.loc" ::= "LaundryShip" ]  & "The string is now tied to the stick. The trap is ready!"

  { rat.inbox } ? 
    { hatch.stringed } ?
      pull(string) >> [
        "string.loc" ::= "Inv", 
        "stick.loc" ::= "Inv", 
        rat.trapped, 
        Not(hatch.sticked),
        Not(hatch.stringed),
        "hatch" ::= "Closed"
      ]  & "Bum! The hatch closes and the rat is trapped."

  { "hatch" == "Open", rat.trapped } ?
      pickUp(rat) >> [ "rat.loc" ::= "Inv", Not(rat.trapped), "hatch" ::= "Closed"]  & "You pick up the rat from the box"

Location { name = "WallysShip",
           locals = ["door", "wally"],
  Pickupable(paper)
  Pickupable(monocle)
  { "monocle.loc" == "Inv" } ?
    give(monocle,wally) >> [ "monocle.loc" ::= "WallysShip" ]  & "You give back the monocle to Wally. Poor guy."

  walkto(door) >> [ "player.loc" ::= "WoodtickLeft" ]  & "You are among several ships"

Location { name = "BloddyLips",
           locals = [bartender, "doorway", "kitchendoor"],
  { Not(talkedaboutLargo) } ? 
    talkto(bartender, largo) >> [ "spit.loc" ::= "BloddyLips", talkedaboutLargo ]  & "As you talk about Largo, he enters, takes the bartenders money and spits on the wall."

  {"paper.loc" == "Inv", "spit.loc" == "BloddyLips"} ? 
    useon(paper, spit) >> [paper.hasspit, "spit.loc" ::= "Undefined"]  & "You scrape the spit off of the wall using the paper. You now have spit-encrusted paper."

  { "rat.loc" == "Vichyssoise", Not(hasbeenhired), Not("job" == "Available")  } ? 
    talkto(bartender, howisstew) >> [ "job" ::= "Available" ]  & "You ask for some stew knowingly that you just ruined it with the rat. The current chef is fired and the job is now available. Maybe something for you?"

  { "job" == "Available", Not(hasbeenhired) } ?
    talkto(bartender, askforjob) >> [ 
      hasbeenhired, 
      "job" ::= "Unavailable", 
      "money" ::= "Money420", 
      "player.loc" ::= "Kitchen"
    ]  & "You are now hired. The pay is paid in advance and you enter the kitchen with no intention whatsoever of fullfilling your newfound duties."

  walkto(doorway) >> [ "player.loc" ::= "WoodtickLeft" ]  & "You leave the BloddyLips and are now among several ships"

Location { name = "Kitchen",
           locals = ["window", "door", "stew"],
   Pickupable(knife)
   Useable(rat, stew, {"rat.loc" ::= "Vichyssoise"})
   walkto(window) >> [ "player.loc" ::= "WoodtickLeft" ]  & "You crawl out the window. You are among several ships"
   walkto(door)   >> [ "player.loc" ::= "BloddyLips" ]  & "You enter the BloddyLips from the kitchen"

Location { name = "Woodsmith",
           locals = ["woodsmith", "path"],
  walkto(path) >> [ "player.loc" ::= "WoodtickLeft" ]  & "You are among several ships"

Location { name = "VoodooShack",
           locals = ["coffinpath", "lady"],
  
  walkto(coffinpath) >> [ "player.loc" ::= "Coffin" ]  & "You enter the coffin"
  
  Pickupable(string)


  { Not("shoppinglist.loc" == "Inv")} ?
    talkto(lady, aboutlargoCurse) >> [ "shoppinglist.loc" ::= "Inv" ]  & "You talk to the voodoolady about Largo. You get a shoppinglist with four items to collect"

  { "shoppinglist.loc" == "Inv" } ? {
    { "bones.loc" == "Inv"} ?   give(bones,lady) >> [ "bones.loc" ::= "VoodooShack" ]  & "You give the bones to the lady"
    { "bra.loc" == "Inv" } ?    give(bra, lady) >> [ "bra.loc" ::= "VoodooShack"  ]  & "You give the bra to the lady"
    { "toupee.loc" == "Inv" } ? give(toupee, lady) >> [ "toupee.loc" ::= "VoodooShack" ]  & "You give the toupee to the lady"
    { paper.hasspit } ?    give(paper, lady) >> [ Not(paper.hasspit), "spit.loc" ::= "VoodooShack" ]  & "You give the spit-encrusted paper to the lady. You get to keep the paper without the spit"
  }

  { "bones.loc" == "VoodooShack",
    "bra.loc" == "VoodooShack", 
    "toupee.loc" == "VoodooShack",
    "spit.loc" == "VoodooShack" } ?
      talkto(lady, doll) >> [ 
        "doll.loc" ::= "Inv", "pins.loc" ::= "Inv", 
        "bones.loc" ::= "Undefined",
        "bra.loc" ::= "Undefined",
        "toupee.loc" ::= "Undefined",
        "spit.loc" ::= "Undefined"
      ] & "You now have the voodoodoll. Now just find Largo"

  { "largoembargo" == "Alleviated", Not("bigWhoopBook.loc" == "Inv") } ?
    talkto(lady, escapeLargo) >> [ "bigWhoopBook.loc" ::= "Inv" ]  & "The lady tells you that you can escape Largo by finding Big Whoop. You get a book about it."


Location { name = "Swamp",
           locals = ["coffin", "swamp", "path"],
  Useable(bucket, swamp, {"bucket.cont" ::= "Mud", "swamp.loc" ::= "Undefined"})
  useon(coffin) >> [ "player.loc" ::= "Coffin" ]  & "You enter the coffin. You can now row around in the swamp."

  walkto(path) >> [ "player.loc" ::= "Map" ]  & "You are looking at the map of Scabb island"

Location { name = "Coffin",
           locals = ["shore", "shackpath"],
	rowto(shore) >> [ "player.loc" ::= "Swamp" ]  & "You row to the shore"
	rowto(shackpath) >> [ "player.loc" ::= "VoodooShack" ]  & "You row into the swamp and find the entrance to the voodooschack"

Location { name = "Cemetery", 
           locals = ["path", "graves"],
  walkto(path)   >> [ "player.loc" ::= "Map" ]  & "You are looking at the map of Scabb island"
  walkto(graves) >> [ "player.loc" ::= "Graves" ]  & "You are at the top of a hill of graves"

Location { name = "Graves",
           locals = ["largosgrave", "cemetery"],
  Useable(shovel,largosgrave, {"bones.loc" ::= "Inv","largosgrave.loc" ::= "Undefined"})
  walkto(cemetery) >> [ "player.loc" ::= "Cemetery" ]  & "You are at the cemetery"

Location { name = "Peninsula",
           locals = ["houseboat", "path"],
  walkto(path) >> [ "player.loc" ::= "Map" ]  & "You are looking at the map of Scabb island"
  walkto(houseboat) >> [ "player.loc" ::= "Dreadsship" ]  & "You enter Dread's houseboat"

Location { name = "Dreadsship",
           locals = ["dread", "path"],
  { "monocle.loc" == "Inv" } ? 
    give(monocle,dread) >> [ "monocle.loc" ::= "Dread" ]  & "You give the monocle to Dread. He is happy to have found a new lucky charm"
  { Not("money" == "None"), "largoembargo" == "Alleviated", "monocle.loc" == "Dread" } ? 
    talkto(dread, charterShip) >> ["money" ::= "Money400", End MI2part1]  & "You charter Dread's ship"
  walkto(path) >> [ "player.loc" ::= "Peninsula" ]  & "You are now just outside Dread's houseboat"

Location { name = "Beach",
           locals = [path],
  Pickupable(stick)
  walkto(path) >> [ "player.loc" ::= "Map" ]  & "You are looking at the map of Scabb island"

Location { name = "Map",
           locals = ["beach", "woodtick", "cemetery", "peninsula", "swamp"],
   walkto(beach)     >> [ "player.loc" ::= "Beach" ]  & "You are at the beach. Nice"
   walkto(woodtick)  >> [ "player.loc" ::= "WoodtickRight" ]  & "You are just outside Woodtick"
   walkto(cemetery)  >> [ "player.loc" ::= "Cemetery" ]  & "You enter the creepy cemetery"
   walkto(peninsula) >> [ "player.loc" ::= "Peninsula" ]  & "You are at the peninsula just outside Dread's houseboat"
   walkto(swamp)     >> [ "player.loc" ::= "Swamp" ]  & "You are at the swamp"

End
