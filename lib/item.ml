type t = 
  | Observer
  | Item_interception
  | Medical_kit of Item_effect.t
  | Poisonous_dart of Item_effect.t
  | Pocket_knife of Item_effect.t
  | Gamblers_potion of Item_effect.t
  
let observer = Observer
let item_interception = Item_interception
let medical_kit = Medical_kit (Item_effect.create ~add_health:25 ~chance_of_adding:1.0 ())
let poisonous_dart = Poisonous_dart (Item_effect.create ~remove_health:75 ~chance_of_removing:0.75 ())
let pocket_knife = Pocket_knife (Item_effect.create ~remove_health:30 ~chance_of_removing:1.0 ())
let gamblers_potion = Gamblers_potion (Item_effect.create ~add_health:60 ~chance_of_adding:0.6 ~remove_health:40 ~chance_of_removing:0.4 ())