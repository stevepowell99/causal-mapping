## overview

I have most of my reactive values stored in values (  values <-reactiveValues()
), e.g. values$graf etc, but there are a few legacy ones, `loaded` and `inputtitl`, which are introduced via `makeReactiveBinding("loaded")`

You can use the app live at https://stevepowell.shinyapps.io/TMV-Theorymaker-visnetwork/

There is a setting right at the top of app.R for doNotification, which controls little notifications which appear bottom-right; setting this to 0 gives you an idea of the reactivity problems.

The basic flow is that tables for the arrows aka edges and variables aka nodes are combined into values$graf, which is a tidygraph object. The tabs "Code" "Variables" and "Arrows" can be used to edit this data. Then (around lines 1600 in the code) this object is broken back down into its consituent tables, merged with values$statements (from the first tab), filtered and processed mainly according to settings in the Display tab, also filtered according to any radio buttons within the Display tab (e.g. filter out all men, all older than 20 etc) and step by step converted into values$grafAgg2, which is the object from which the visualisation is constructed. The flow from values$graf to values$grafAgg2 is live, there is no actionButton. 

OTHER STUFF IN DYNALIST