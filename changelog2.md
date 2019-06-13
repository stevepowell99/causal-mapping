commit e710c28a322f12ed413313966fd648f97d6d1e1c (HEAD -> master, origin/master)
Author: Steve Powell <steve@pogol.net>
Date:   Tue Jun 11 17:11:01 2019 +0100

    Highlighting of coded text; reset button
    
    reset button for resetting selected nodes to default for a particular statement
    Highlighted all coded text in statement. If highlights overlap, second and later highlights are ignored

commit 0b4c79eb07fb1d539866a536a190e70a1f0a6b4f
Author: Steve Powell <steve@pogol.net>
Date:   Tue Jun 11 11:50:20 2019 +0100

    more functionality
    
    cleaned csv upload variable names, to lowercase
    you can move control-clicked nodes together
    calculate x and y for layout earlier, in grafagg2
    colour edges if backwards

commit e4f792bf8398fb3ff42de728966830e326eb79b5
Author: Steve Powell <steve@pogol.net>
Date:   Mon Jun 10 09:26:40 2019 +0100

    Integrated Nic's changes
    
    also improved import of summary edgelists
    worked on coding interface
    improved layout algo
    fixed save png and html

commit f3a45e5e7dd8882c673817e865f1bea46f37a985
Author: Steve Powell <steve@pogol.net>
Date:   Sat Jun 8 17:25:41 2019 +0100

    changelog and roadmap after merge in from Nic

commit ceddd3a399c6799169c3072ae0c313fc0eeb5493
Merge: 240a458 54e6690
Author: Steve Powell <steve@pogol.net>
Date:   Sat Jun 8 16:19:26 2019 +0100

    Merge remote-tracking branch 'nic/master'

commit 54e66902ead8f50d6f495498f3a1180da2e593ba
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 14:04:51 2019 +0100

    Add better comments and variable names, remove redundant if TRUE statements, remove dependencies from renderer.

commit d379e9b3a17a13cac738295770da82a29f33135f
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 13:33:36 2019 +0100

    Add documentation and prevent automatically saving when first create viz (note: you may or may not want this funtionality?)

commit 8eb6ca47769f292afe59c404587ceb9c8a0468c5
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 13:09:37 2019 +0100

    Move ved join to the global file, add in reactive chain so that the network only updates on page transitions when we move to/from the "Code" page

commit 0fa931a8b0660d6ec4dfaf43f423dc4267dd7101
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 12:22:27 2019 +0100

    Move ved prep to its own function in a separate file, remove isolates from sides (temp), turn off hover select on visnetwork

commit 8129c2bc8040a6fa56f18137f37c83cf6b3314ae
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 11:52:19 2019 +0100

    Minor refactoring for efficiency, use prepare_vg function, rename vg for readibility, and ensure that when using findset, values is isolated

commit 9d74babcc402f7bf69a88c1d692c708efdda61d8
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 11:48:09 2019 +0100

    Move prepare_vg function to global

commit a30297da4d769f257eabd71025445eabe26cdf71
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 11:21:32 2019 +0100

    Refactor NN to nodes_as_tibble and EE to edges_as_tibble to improve readibility

commit 405a98f25a8c485ff040fc810862043136966824
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 10:33:12 2019 +0100

    Change values$net to not be a function

commit 865261e6b46cd020e5de1bc874af66ff0ce5eeb2
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 10:22:46 2019 +0100

    Isolate 'input' when we're checking names(input)

commit 4a32599e3239fb020d8c4cc39cebca58b0e05037
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 08:45:28 2019 +0100

    Minor whitespace/formatting tweaks

commit 4100b7e3a6e51942580e437d2a67a9cf8e756d0a
Author: Nic <thisisnic@gmail.com>
Date:   Sat Jun 8 08:08:31 2019 +0100

    Split app.R out into global.R, ui.R, and server.R

commit 240a45828c66d3bac71d1f81c5cbcd11bdd23244
Author: Steve Powell <steve@pogol.net>
Date:   Fri Jun 7 16:46:03 2019 +0100

    tidied the code, added comments

commit 4341d5ccbe1402358d6d50c8c1a2670578c0315d
Author: Steve Powell <steve@pogol.net>
Date:   Fri Jun 7 12:58:41 2019 +0100

    using gitKraken

commit 8c7dd2517f49d268018166983eefaee0cc68ee50
Author: Steve Powell <steve@pogol.net>
Date:   Fri Jun 7 12:56:00 2019 +0100

    Initial commit
