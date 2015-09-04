# HCM

*HCM* is a [Hearthstone](http://us.battle.net/hearthstone/en/)
 Collection Manager. It uses the great work of
 http://hearthstonejson.com/ to create a local library on your PC, that
 you can then query.


## Commands

***hcm help*** displays some help, just as one could expect.

***hcm update*** downloads the latest card data and create / updates your
  local collection. Run once before running any other command.

***hcm input*** lets you input your collection, by asking you, card after
  card, tirelessly, how much of it you own. It accepts *filters*, if you
  only want to input some specific cards.

***hcm add*** and ***hcm del*** expect card names, and respectively increase
  / decrease by one the amount of those cards in your collection.

***hcm list*** displays a table of all cards. Like ***hcm input***, it
  *accepts filters*.

***hcm stats*** displays stats about your collection and computes the
  average dust value of each set pack. This is what this project is all
  about.


## Filters

Some commands accept filters:

* **q=***x* where x is 0, 1 or 2: filter by amount you own;
* **s=***x* where x is "c", "gvg" or "tgt": filter by set;
* **h=***x* where x is a class name or "Neutral": filter by class;
* **r=***x* where x is a card rarity: filter by rarity;
* **c=***x* where x is an integer: filter by cost;
* **owned** would be equivalent to "q=1 or q=2" if "or" existed for filters.

Which means that `hcm list owned r:Legendary` will display the
legendaries you own.


## Auto-completion

the .sh file in the repo enables bash completion for card names and
filters. Trust me you want it.
