I have most of my reactive values stored in values (  values <-reactiveValues()
), e.g. values$graf etc, but there are a few legacy ones, `loaded` and `inputtitl`, which are introduced via `makeReactiveBinding("loaded")`

You can use the app live at https://stevepowell.shinyapps.io/TMV-Theorymaker-visnetwork/

There is a setting right at the top of app.R for doNotification, which controls little notifications which appear bottom-right; setting this to 0 gives you an idea of the reactivity problems.

The basic flow is that tables for the arrows aka edges and variables aka nodes are combined into values$graf, which is a tidygraph object. The tabs "Code" "Variables" and "Arrows" can be used to edit this data. Then (around lines 1600 in the code) this object is broken back down into its consituent tables, merged with values$statements (from the first tab), filtered and processed mainly according to settings in the Display tab, also filtered according to any radio buttons within the Display tab (e.g. filter out all men, all older than 20 etc) and step by step converted into values$grafAgg2, which is the object from which the visualisation is constructed. The flow from values$graf to values$grafAgg2 is live, there is no actionButton. 

The visualisation with visNetwork can also be post-processed using a proxy network so that the graph does not have to be re-rendered for small changes like highlighting. I did have one such observe() around lines 2323 but commented it out because it seemed to make things worse re reactivity.

I tried using throttle and debounce to improve the reactivity, line 2294, but this did not seem to make any difference so I commented out the line.

If you want me to put more comments in the code, I can.






# focus selection by pager just flashes and goes, nothing to do with new select coral by node

#options(shiny.reactlog=TRUE) # note reactlog seems to cause an error, utf-8
# profiling - currently, render button used to aggregate and to render. edges joined to nodes under certain conds, see agg section
# profiling: mutate_if_sw and findset are bad

# maybe: if conditional setting just says mean, this means mean of mean of mean stc
# maybe: if conditional setting - can also say mean_mean_max_frequency or whatever. This then does this agg in aggregate on demand

# merge in infor on respondents into first arrows table

# have a viz with causal links in a chain - a table??
  
  


# viz related-------------
# note diagram height only affects the html export
# do d3!
# r2d3 is cool BUT EVEN R2D3 USES WEBSHOT https://rstudio.github.io/r2d3/articles/publishing.html#save_d3_png
# no svg export as yet! seems to be just about working upstream: https://github.com/almende/vis/issues/723, see end of that thread
# can do many of the updates using classic visEdges etc but networkproxy
# webshot: Unfortunately, PhantomJS is no longer being maintained. For webshot, we are planning on moving to a different headless browser in the future, but we don't have a fixed timeline for that yet.
# https://github.com/wch/webshot/issues/83 my issue

# arrows - select!
# put more of the interactivity into proxy
# "C:\Program Files\wkhtmltopdf\bin\wkhtmltopdf.exe" --javascript-delay 20000 http://127.0.0.1:6432/?permalink=SLP4genderMore qstring.pdf
# works but it doenst pick up the actual net
# decaptitated doesn't work
# webshot: rubbish quality
# plotly png export only OK. Plotly can probably do everything though https://plot.ly/r/network-graphs/
# funny, ggplotly renders ggraph better than ggraph (emojis)
# forcenetwork doesn't even have arrowheads and no layout

# EASY quick fixes

# overview_col for all statements from one source only works with colname "source"
# split notifications into needed and info
# finish changing arrows from to in arrows table
# auto backup for when connection breaks
# edit columns for vars and arrows
# reorder table cols , seems to be bug, wrong when update table
# refactor all lapply observers / buttons in combined style like obsList

# MOST GENERAL of these changes?
# easier to edit previous coding ... so when looking at one statement, already highlight the relevant nodes and arrows??
# custom additional random icon as an option in settings e.g. conditional on project / # random colours for nominal variables
# custom artithmetic e.g. trust*precision?
# instead of autodetect filter widgets, could put "filter" in the widget column of a new setting? 
# restructure import: kobo data needs to restructure data anyway ie age is one q but needs to become part of respondent ID
# review stats. e.g. overall key1 shd not be biassed when only a few or zero in from or to
# attribution to project - this is already covered!
# general tables / charts

# bsdr would like...
# move arrows or vars tab to rhs



# BUGS
# when you save under a new name, the permalink info does not change

# you have to stop the timeslider at 1
# merge/clusters:
# should prioritise when doing combined merged labels


# NICE TO HAVE
# journal of changes
# presentation mode
# custom import folder of pdfs as text?
# https://github.com/yonicd/rpdf pdfs - tried, doesnt work


## transparent fading other statements is buggy because 1 is in 134 etc
## add session ids eg to pagerinput
## label color opacity doesn't work if background is conditional
## set a column then (de)aggregate and some columns will disappear, so error
## if you have an if column, how does it know how to set the default value for the other arrows/variables


# aggfun - the problem is that you can have a group in which there are just two, so it tries different strategies for different groups

