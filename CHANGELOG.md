CHANGELOG


21/5/2019 

changed functionality of highlighted text. Now, when there is already text in the Quote box, highlighting additional text will be appended to the previous text, separated by " ... "

increased height of Quote box to 3 rows

~~fixed focus so that other arrows not coded by current statement are paler~~

now the diagram always refreshes without having to press Render, but it is therefore a bit slower and jumps around when loading

Began with dual track for graph visualisation ("Interactive" vs "HiQuality"):  added button in main display windown to switch between them, removed other visualisation types

Fixed bug which was stopping rendering of HiQ graph type

Added setting HiQratio for aspect ratio of HiQ diagram

Added ability to export fairly hi-res png from interactive viz.

23/5/2019
(reverted)

24/5/2019
added ava (average conspicuous absence) and avp for edges, switched on by setting "absence"
fixed incorrect calculation of summary statistics
added code to skip aggregation when no edges
commented out report tab and code
speeded up sumfun and meanfun
tried and failed all day to get debounce() into the workflow
disabled mutate_if warnings

25/5/2019
added separate button for PNG save
fixed not autosaving in crowdsource
pager now selects nodes via proxy (fixme: network is still recalculated anyway) rather than hard-coded marking of edges
fixed edge selection
added debounce() thanks to https://stackoverflow.com/questions/56296342/how-to-use-debounce-with-reactivevalues-in-shiny/56296475#56296475, makes it worse??

29/5/19
improved annotations
moved todo to separate todo.txt
made video for Nic and sent outline of reactivity


30/5/2019
Changed default scaling for conditional arrow width
Added fifth colour palette in defaultsettingsglobal
fighting with colour bug in gya4. fixed. remember that if metric is not found e.g. age_mean, some other attribute will be transferred.
Added frequency column to main Variables table 
Commented out "switch to variables" /arrows 
Added attribute "details" to variable table

1/6/2019
Improved statements import, now only ensure that first column is text, it will be renamed anyway as text. 
If a column name contains at least one ",", say "sex1,district1,age2", that column will be replaced by, in this case, 3 columns named "sex" "district" and "age", and the contents of the column will be split according to the numbers in the original column name. OR, if a column name contains a ".", say sex.district.age, that column will be replaced by three columns (sex, district,age) and the contents of that column will be split on "." to give say 3 columns with those headings. this only works for one such column.

Added crosstalk back from node table highlight to diagram, so if user clicks in first row of variables table, that node is highlighted in the diagram.

2/6/2019
abstracted storage and added dropbox as possibility, but problems with unicode.
googledrive package seems to have problems finding a directory; use googlesheets4 with it. Now fixed, google sheets can be used as a way of loading editing and saving the node and edge tables. It's a bit slower though if you want to have live visualisaion.

3/6/2019
Get correct node from clicking left-hand column in nodes table
enlarge selected nodes on hover/select
select on search filter?
add button to allow link to next clicked node

4/6/2019
new layout algorithm
reduce some of the flexibility
changed interface to maximise diagram size
fixed artefact in visnetwork (misplaced horizontal arrows) by adding a bit of rnorm to y dimension
should be able to hide rather than exclude edges less than x
added static legend for colours






