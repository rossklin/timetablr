## Software License Agreement (BSD License)
##
## Copyright (c) 2014, Tilo Wiklund (tilo@wiklund.co)
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are
## met:
##
##     Redistributions of source code must retain the above copyright
##     notice, this list of conditions and the following disclaimer.
##
##     Redistributions in binary form must reproduce the above copyright
##     notice, this list of conditions and the following disclaimer in
##     the documentation and/or other materials provided with the
##     distribution.
##
##     The names of its contributors may not be used to endorse or promote products
##     derived from this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
## A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
## DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
## THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#' Guess frequency of time series
#'
#' Guess frequency by looking at the smallest time difference within any single
#' entity.
guess.frequency <- function(times, groups) {
    values <- data.table(timecol=times, groupcol=groups)
    tname <- grep("^timecol", colnames(values), value=T)
    gname <- grep("^groupcol", colnames(values), value=T)
    tdname <- safe_name(values)
    delta <- values[, eval(tdname):=c(NA, diff(sort(.SD[[tname]])))
                    , by=eval(gname)][, min(.SD[[tdname]], na.rm=TRUE)]
    if(is.infinite(delta)) warning("Insufficient data to guess time delta")
    list(from=min(times), to=max(times), delta=delta)
}

#' Create a time.table
#'
#' Create a time.table given a data.frame containing its data and information
#' about how to interpret its columns.
#'
#' @param df data.frame or data.table containing data, one row for each observation.
#' @param id.vars Name(s) of columns identifiying the entity associated to an observation. Each unique combination of values corresponding to one entity.
#' @param time.var *Single* name identifying the column containing time information for each observation.
#' @param measurement.vars Name(s) of columns containing measurement values. These are the columns most functions will operate on by default. Will default to all column names not used for indexing/time/auxiliary.
#' @param aux.vars Name(s) of columns containing auxiliary measurements. These will be carried along by most functions but will not be mutated. Will defatult to all columns not used for anything else (with measurement.vars having precedence, i.e. if neither \code{measurement.vars} or \code{aux.vars} is specified the non-index/time columns will be interpreted as measurements).
#' @param frequency list containing values \code{from}, \code{to}, and \code{delta} specifying a (shared) range of the time series and the size of their time steps. Technically optional though some functions require it (in the future there will probably be a more clear separation between regular and irregual time series).
#'
#' time.table is a subclass of data.table (and therefore also of data.frame) and
#' can thus be used like one. Be careful though, as most operations *will lose*
#' any time.table specific attributes.
#' 
#' @export
as.time.table <- function( df, id.vars, time.var
                         , measurement.vars = NULL, aux.vars = NULL
                         , frequency=NULL ) {
    if(is.null(frequency)) {
        frequency <- guess.frequency( as.data.frame(df)[[time.var]]
                                    , as.data.frame(df)[id.vars] )
    }
    if(is.null(measurement.vars))
        measurement.vars <- setdiff(colnames(df), union(id.vars, union(time.var, aux.vars)))
    if(is.null(aux.vars))
        aux.vars <- setdiff(colnames(df), c(id.vars, union(time.var, measurement.vars)))
    #
    tt <- data.table(df)[,c(id.vars, time.var, measurement.vars, aux.vars),with=F]
    # TODO: class<- invalidates internal selfref for data table
    tt <- tt[,c(id.vars, time.var, measurement.vars, aux.vars), with=FALSE]
    class(tt) <- c("time.table", class(tt))
    tt <- copy(tt)
    #
    setkeyv(tt, c(id.vars, time.var))
    #
    setattr(tt,      "id.vars", id.vars)
    setattr(tt,    "time.var" , time.var)
    setattr(tt, "measurement.vars", measurement.vars)
    setattr(tt,     "aux.vars", aux.vars)
    #
    setattr(tt, "frequency", frequency)
    setkeyv(tt, c(id.vars, time.var))
    #
    tt
}

#' Extract index values as a data.table
#' 
#' @export
index <- function(tt, ...) UseMethod("index")
#' Extract auxiliary values as a data.table
#' 
#' @export
auxiliary <- function(tt, ...) UseMethod("auxiliary")
#' Extract measurement values as a data.table
#' 
#' @export
measurement <- function(tt, ...) UseMethod("measurement")

#' Extract specific columns from a time.table
#'
#' Extract columns from a time.table (producing a data.table) by their function
#' (as index, time, measurement, or auxiliary).
#'
#' @param tt time table to export from
#' @param with.index extract index columns
#' @param with.time extract time column
#' @param time.measurement extract measurement columns
#' @param with.auxiliary extract auxiliary columns
#' @param rekey whether to keep the index/time key(s) in the resulting data.table
#' @param manual add additional columns to include (by name)
#'
#' You probably want one of \code{\link{index}}, \code{\link{time}},
#' \code{\link{measurement}}, or \code{\link{auxiliary}}.
subset.time.table.parts <- function( tt, with.index=FALSE, with.time=FALSE
                                   , with.measurement=FALSE, with.auxiliary=FALSE
                                   , rekey=(with.index | with.time)
                                   , manual=c() ) {
    cols <- unique(c( if(with.index) index_names(tt) else c()
                    , if(with.time) time_name(tt) else c()
                    , if(with.measurement) measurement_names(tt) else c()
                    , if(with.auxiliary) auxiliary_names(tt) else c()
                    , manual ))
    tt2 <- tt[,cols,with=FALSE]
    if(rekey) {
        setkeyv(tt2, c( if(with.index) index_names(tt) else c()
                      , if(with.time)    time_name(tt) else c() ))
    }
    tt2
}

#' drop auxiliary values
#'
#' @param tt time.table to drop auxiliary columns from
#' @param destructive wether to mutate existing time table, defaults to false
drop_auxiliary <- function(tt, destructive=FALSE) {
    tt2 <- if(destructive) tt else copy(tt)
    for(col in auxiliary_names(tt2)) {
        tt2[[col]] <- NULL
    }
    setattr(tt2, "aux.vars", NULL)
    tt2
}

#' Extract time.table index
#' @export
index.time.table <- function(tt, with.time=FALSE, rekey=TRUE)
    subset.time.table.parts(tt, TRUE, with.time, FALSE, FALSE, rekey)

#' Extract time.table times
#' @export
time.time.table <- function(tt, with.index=FALSE, rekey=FALSE)
    subset.time.table.parts(tt, with.index, TRUE, FALSE, FALSE, rekey)

#' Extract time.table measurements
#' @export
measurement.time.table <- function( tt, with.index=FALSE, with.time=FALSE
                                  , rekey=(with.index|with.time) )
    subset.time.table.parts(tt, with.index, with.time, TRUE, FALSE, rekey=rekey)

#' Extract time.table auxiliaries
#' @export
auxiliary.time.table <- function( tt, with.index=FALSE, with.time=FALSE
                                , rekey=(with.index|with.time) ) 
    subset.time.table.parts(tt, with.index, with.time, FALSE, TRUE, rekey=rekey)

#' Index column name(s)
#'
#' @param tt time.table to extract names from
#' @export
index_names <- function(tt, with.time=FALSE)
    c(attr(tt, "id.vars"), if(with.time) attr(tt, "time.var") else c())

#' Time column name
#'
#' @param tt time.table to extract names from
#' @export
time_name <- function(tt, with.index=FALSE)
    c(if(with.index) attr(tt, "id.vars") else c(), attr(tt, "time.var"))

#' Measurement column name(s)
#'
#' @param tt time.table to extract names from
#' @export
measurement_names <- function(tt, with.index=FALSE, with.time=FALSE)
    c( if(with.index) attr(tt, "id.vars") else c()
     , if(with.time) attr(tt, "time.var") else c()
     , attr(tt, "measurement.vars") )

#' Auxiliary column name(s)
#'
#' @param tt time.table to extract names from
#' @export
auxiliary_names <- function(tt, with.index=FALSE, with.time=FALSE)
    c( if(with.index) attr(tt, "id.vars") else c()
     , if(with.time) attr(tt, "time.var") else c()
     , attr(tt, "aux.vars") )

#' time.table time delta
#'
#' @param tt time.table to extract time delta from
#' @export
deltat.time.table <- function(tt) attr(tt, "frequency")$delta
#' time.table frequency
#'
#' @param tt time.table to extract frequency from
#' @export
frequency.time.table <- function(tt) 1/attr(tt, "frequency")$delta

#' List of time points within a (regular) time.table
#'
#' @param tt time.table to extract time range from
#' @export
timerange <- function(tt) with(attr(tt, "frequency"), seq(from, to, delta))

#' time.table starting time
#'
#' @param tt time.table to extract starting time from
start.time.table <- function(tt) attr(tt, "frequency")$from
#' time.table ending time
#'
#' @param tt time.table to extract ending time from
end.time.table <- function(tt) attr(tt, "frequency")$to

#' number of observations
#'
#' @param tt time.table the number of observations of which to query
#' @param units whether to ask for te number of (cross sectional) units rather than the number of unit/time pairs, defaults to the latter
#' @param theoretical whether to compute the number of observations based on frequency information or on actual data stored in the time.table, defaults to the latter
#'
#' @export
nobs <- function(tt, units=FALSE, theoretical=FALSE) {
    if(units) nrow(unique(index(tt)))
    else {
        if(theoretical) {
            nrow(unique(index(tt))) * floor((end(tt) - start(tt))/deltat(tt))
        } else {
            nrow(tt)
        }
    }
}

#' Subset a time table
#'
#' Subset a time table rows and columns by expression or indices, preserving
#' time.table structure.
#'
#' @param tt time.table to subset
#' @param expr Expression evaluated within tt to subset the rows of tt
#' @param vars Non-index/time columns to subset
#' @param index data.table/data.frame containing indices or indices and times to subset
#' @param times times within each index to subset (to not specify if index already contains times to extract!)
#' @param preserve.frequency whether to preserve (or reguess) frequency information after subsetting
#'
#' @details
#' Note that for now \code{vars} *must* be a collection of strings.
#'
#' One can specify more than one of \code{expr}, \code{vars}, and
#' \code{index}/\code{time}, in which case the time.table is first subset by
#' index/time, then by expr and finally by vars, meaning index/expr can rely on
#' variables removed after the subset.  Note that this can result in unexpected
#' behaviour if expr evaluates to a vector of row numbers.
#'
#' Furthermore both \code{vars} and \code{expr} can mention extra columns in
#' index/time (merged in the intermediate data.table in the \code{index} step).
#'
#' If a column name exists in both \code{tt} and \code{index}/\code{time} then
#' the name will referr to the corresponding column of \code{tt} *if* the column
#' is to be kept (i.e. it is in \code{vars}), else it will refer to the merged
#' value from \code{index}/\code{time}.
#' 
#' @export
subset.time.table <- function( tt, expr=NULL, vars=NULL, index=NULL, times=NULL
                             , preserve.frequency = NULL ) {
    keep.measurement <- measurement_names(tt)
    keep.aux         <- auxiliary_names(tt)
    if(!is.null(vars)) {
        keep.measurement <- keep.measurement[keep.measurement %in% vars]
        keep.aux         <- keep.aux[keep.aux %in% vars]
    }
    #
    index <- if(is.null(index)) {
        index
    } else if(inherits(index, "data.frame")) {
        if(!any(colnames(index) %in% index_names(tt)))
            stop("subset.time.table: 'index' does not contain any index columns from 'tt'")
        as.data.frame(index)
    } else if(inherits(index, "matrix")) {
        if(!any(colnames(index) %in% index_names(tt)))
            stop("subset.time.table: 'index' does not contain any index columns from 'tt'")
        as.data.frame(index)
    } else if(is.null(dim(index))) {
        tmp <- as.data.frame(index)
        if(length(index_names(tt)) > 1)
            warning("subset.time.table: multi column index time.table subset by index vector, using only outermost index")
        setnames(tmp, head(index_names(tt), 1))
        tmp
    } else stop(paste0("subset.time.table: Unable to handle 'index' argument of class ", pasteSane0(class(index), collapse=", ")))
    #
    times <- if(is.null(times)) {
        times
    } else if(inherits(times, "data.frame")) {
        if(!any(colnames(times) %in% time_name(tt)))
            stop("subset.time.table: 'times' does not conain time column from 'tt'")
        as.data.frame(times)
    } else if(inherits(times, "matrix")) {
        if(!any(colnames(times) %in% time_name(tt)))
            stop("subset.time.table: 'times' does not conain time column from 'tt'")
        as.data.frame(times)
    } else if(is.null(dim(times))) {
        tmp <- as.data.frame(times)
        setnames(tmp, time_name(tt))
        tmp
    } else stop(paste0("subset.time.table: Unable to handle 'times' argument of class ", pasteSane0(class(times), collapse=", ")))
    #
    # data.table subsetting required an initial segment of the key to be specified
    missing.index.cols <- if(!is.null(times)) {
        # Time is always the final key, whence all indices are required
        index_names(tt)[which(!(index_names(tt) %in% colnames(index)))]
    } else if(!is.null(index)) {
        # Only add sufficiently many columns to make an initial segment
        segment.end <- max(which(index_names(tt) %in% colnames(index)))
        index_names(tt)[which(!(head(index_names(tt), segment.end) %in% index))]
    } else {
        c()
    }
    #
    index <- if(length(missing.index.cols) == 0) {
        index
    } else {
        lookup <- setkeyv(unique(index.time.table(tt)), maybe(intersect(index_names(tt), colnames(index)), character()))
        lookup[index]
    }
    #
    tt2 <- tt[,c(index_names(tt), time_name(tt), keep.measurement, keep.aux), with=F]
    orig.cols <- colnames(tt2)
    #
    if(!is.null(index)) {
        ss <- if(!is.null(times)) {
                  setkeyv( as.data.table(expand.data.frames(index, times))
                         , c(index_names(tt), time_name(tt)) )
              } else {
                  setkeyv(as.data.table(index), intersect(index_names(tt), colnames(index)))
              }
        tt2 <- tt2[ss,allow.cartesian=TRUE]
    }
    #
    # Lots of fiddling to keep all the additional colmns around...
    #
    expr_call <- substitute(expr)
    extra.aux <- if(!is.null(expr_call)) {
        rows <- eval(expr_call, as.list(tt2), parent.frame())
        tt2 <- tt2[rows]
        c( if(!is.null(index)) setdiff(colnames(index), orig.cols) else c()
         , if(!is.null(times)) setdiff(colnames(times), orig.cols) else c()
         , setdiff(colnames(rows), orig.cols) )
    } else {
        c( if(!is.null(index)) setdiff(colnames(index), orig.cols) else c()
         , if(!is.null(times)) setdiff(colnames(times), orig.cols) else c() )
    }
    #
    if(is.null(preserve.frequency)) {
        preserve.frequency <- is.null(times)
    }
    if(is.logical(preserve.frequency)) {
        if(preserve.frequency) preserve.frequency <- attr(tt, "frequency")
        else preserve.frequency <- NULL
    }
    #
    as.time.table( tt2
                 , id.vars=index_names(tt)
                 , time.var=time_name(tt)
                 , measurement.vars=keep.measurement
                 , aux.vars=c( keep.aux
                             , if (is.null(vars)) extra.aux
                               else intersect(extra.aux, vars) )
                 , frequency=preserve.frequency )
}

#' Lag time.table
#'
#' Lags each time series by a number of time steps, optionally preserving the
#' index/time values present in the data.table (in which case some information
#' is lost).
#'
#' @param tt time.table to lag
#' @param forward Shift forward (default)
#' @param steps Number of steps to shift (i.e. number of delta)
#' @param preserve.frequency Preserve range (indeed, the exact values) of index/time values present in \code{tt}
#' @param ... unused
#'
#' Note that for now preserve.freuqency==FALSE is not implemented.
#'
#' @export
lag.time.table <- function(tt, forward=TRUE, steps=1L, preserve.frequency=TRUE, ...) {
    stopifnot(preserve.frequency)
    if(!forward) steps <- -steps
    #
    tcol  <- time_name(tt)
    icols <- index_names(tt)
    acols <- auxiliary_names(tt)
    mcols <- measurement_names(tt)
    delta <- deltat(tt)
    if(is.null(delta)) stop("Cannot lag a time.table without frequency information")
    #
    tt2 <- measurement.time.table(tt, with.index=T, with.time=T)
    tt2[[tcol]] <- tt2[[tcol]] - steps*delta
    # NOTE: We modified time column so we need to re-set the key
    # In case this ever turns out to be slow we can use monotonicity
    # of the transformation and forcefully set the key attribute of
    # the data.table.
    setkeyv(tt2, c(icols, tcol))
    # Something weird is going on, somehow R won't let me 
    # one-line this...
    keys <- tt[,c(icols, tcol, acols),with=F]
    same_str_as(tt2[keys], tt)
}

embed <- function(x, dimension=1, ...) UseMethod("embed")
embed.default <- stats::embed

#' Delay embedding of time.table
#'
#' Creates a time delay embedding (a la Takens' theorem) for the measuement
#' values of a time.table, that is to say it adds columns for a number of
#' lagged values for each measuement.
#'
#' @param x time.table to embedd
#' @param dimension number of lagged values to add columns for
#' @param prefixes prefixes to add to lagged column names, either vector of length \code{column} or a single string, defaults to "lag."
#' @param suffixes suffixes to add to lagged column names, either vector of length \code{column} or a single string, defaults to \code{c(".1", ".2", ..., ".dimension")}.
#' @param forward whether to lag values (the alternative being to lead them), defaults to true
#' @param preserve.frequency whether the result should include only those index/time combinations present in \code{x}
#'
#' @export
embed.time.table <- function( x, dimension=1
                            , prefixes=NULL
                            , suffixes=NULL
                            , lag=TRUE, preserve.frequency=TRUE ) {
    stopifnot(preserve.frequency)
    #
    prefixes <- if(is.null(prefixes)) {
        rep("lag.", dimension)
    } else if(length(prefixes) == 1) {
        rep(prefixes, dimension) 
    } else {
        prefixes
    }
    #
    suffixes <- if(is.null(suffixes)) {
        pasteSane0(".", seq_len(dimension))
    } else if(length(suffixes) == 1) {
        rep(suffixes, dimension)
    } else {
        suffixes
    }
    if(length(prefixes) != dimension) stop("Insufficient prefix values")
    if(length(suffixes) != dimension) stop("Insufficient suffix values")
    #
    lagged <- lapply( seq_len(dimension), lag.time.table
                    , tt=drop_auxiliary(x)
                    , forward=!lag
                    , preserve.frequency=preserve.frequency )
    for(i in seq_along(lagged)) {
        affix_names( lagged[[i]]
                   , measurement.prefix=prefixes[i]
                   , measurement.suffix=suffixes[i] )
    }
    #
    Reduce(function(...) merge.time.table(..., all.x=TRUE, all.y=FALSE), c(list(x), lagged))
}

#' Difference time.table
#'
#' Difference (diff) each time series in a time.table, taking missing missing
#' values into account (i.e. each difference it taken at exactly one time step,
#' with a value of NA if the next observation is not present). Optionally diff
#' preserves the exact index/time combinations present in the original
#' time.table (in which case some information is lost).
#'
#' @param tt time.table to difference
#' @param forward compute forward differences (default).
#' @param preserve.frequency preserve the range of (indeed, exact) values present in \code{tt}
#' @param ... unused
#'
#' Note that for now only forward==TRUE and preserve.frequency==TRUE are implemented.
#'
#' @export
diff.time.table <- function(tt, forward=TRUE, preserve.frequency=TRUE, ...) {
    stopifnot(forward & preserve.frequency)
    #
    tt.lagged <- lag.time.table( tt, steps=1L, forward=forward
                               , preserve.frequency=TRUE )
    stopifnot(all.equal( index(tt, with.time=T)
                       , index(tt.lagged, with.time=T) ))
    for(col in measurement_names(tt)) {
        tt.lagged[[col]] <- tt.lagged[[col]] - tt[[col]]
    }
    tt.lagged
}

#' Create time.table from a template
#'
#' Turns a data.table into a time table using attributes from another
#' time.table.
#'
#' @param dt data.table containing data
#' @param tt time.table to use as template
#' @param add.index new index columns present in dt
#' @param add.measurement new measurement columns present in dt
#' @param add.auxiliary new auxiliary columns present in dt
#'
#' Note that all columns in \code{tt} must be present in \code{dt} and that any
#' columns in \code{dt} not in \code{tt} are lost.
#'
#' @export
same_str_as <- function( dt, tt
                       , add.index=c()
                       , add.measurement=c()
                       , add.auxiliary=c() ) {
    as.time.table( dt
                 , id.vars=c(index_names(tt), add.index)
                 , time.var=time_name(tt)
                 , measurement.vars=c(measurement_names(tt), add.measurement)
                 , aux.vars=c(auxiliary_names(tt), add.auxiliary)
                 , frequency=attr(tt, "frequency") )
}

merge.time.table <- function( tt1, tt2, combine.funs=NULL, all=NULL
                            , ...
                            , all.x=NULL, all.y=NULL ) {
    stopifnot(setequal(index_names(tt1), index_names(tt2)))
    stopifnot(time_name(tt1) == time_name(tt2))
    #
    if(is.null(all) & is.null(all.x) & is.null(all.y)) all <- TRUE
    all.x <- maybe(all.x, all)
    all.y <- maybe(all.y, all)
    if(is.null(all.x) & !is.null(all.y) & all.y) all.x <- FALSE
    if(is.null(all.y) & !is.null(all.x) & all.x) all.y <- FALSE
    all <- maybe(all, all.x & all.y)
    #
    shared.vars <- setdiff(intersect(colnames(tt1), colnames(tt2)), index_names(tt1, TRUE))
    stopifnot(all(shared.vars %in% names(combine.funs)))
    #
    tt.new <- data.table:::merge.data.table(tt1, tt2, all=all, all.x=all.x, all.y=all.y, suffixes=c("", ".REMOVE"))
    for(col in shared.vars) {
        MERGE.TIME.TABLE.FUN.SAFE.NAME <- combine.funs[[col]]
        MERGE.TIME.TABLE.COL1.SAFE.NAME <- col
        MERGE.TIME.TABLE.COL2.SAFE.NAME <- paste0(col, ".REMOVE")
        # tt.new[,eval(col):=fun(.SD[[col]], .SD[[col2]])]
        tt.new[,eval(MERGE.TIME.TABLE.COL1.SAFE.NAME):=MERGE.TIME.TABLE.FUN.SAFE.NAME(.SD[[MERGE.TIME.TABLE.COL1.SAFE.NAME]], .SD[[MERGE.TIME.TABLE.COL2.SAFE.NAME]])]
        # tt.new[,eval(col2):=NULL]
        tt.new[,eval(MERGE.TIME.TABLE.COL2.SAFE.NAME):=NULL]
    }
    #
    freq <- if(all | (all.x & all.y)) {
        list(from=min(start(tt1), start(tt2)), to=max(end(tt1), end(tt2)), delta=NULL)
    } else if(!(all | all.x | all.y)) {
        list(from=max(start(tt1), start(tt2)), to=min(end(tt1), end(tt2)), delta=NULL)
    } else if(all.x) {
        attr(tt1, "frequency")
    } else {
        attr(tt2, "frequency")
    }
    #
    as.time.table( tt.new, index_names(tt1), time_name(tt1)
                 , union(measurement_names(tt1), measurement_names(tt2))
                 , union(auxiliary_names(tt1), auxiliary_names(tt2))
                 , frequency=freq )
}

#' Affix measurement/auxiliary names
#'
#' Adds prefix/suffix to the measurement/auxiliary (column) names of a
#' time.table. This is useful for when you want to merge a transformed table
#' back into the original one while avoiding clashing column names.
#'
#' @param tt time.table to transform names of
#' @param prefix prefix to add to names (defaults to empty string)
#' @param suffix suffix to add to names (defaults to empty string)
#' @param measurement.prefix prefix to add to measurement names (defaults to \code{prefix})
#' @param measurement.suffix suffix to add to measurement names (defaults to \code{suffix})
#' @param auxiliary.prefix prefix to add to auxiliary names (defaults to \code{prefix})
#' @param auxiliary.suffix suffix to add to auxiliary names (defaults to \code{suffix})
#'
#' Note, changes tt by reference (i.e. mutates it).
#' 
#' @export
affix_names <- function( tt
                       , prefix=NULL, suffix=NULL
                       , measurement.prefix=NULL, measurement.suffix=NULL
                       , auxiliary.prefix=NULL, auxiliary.suffix=NULL ) {
    measurement.prefix <- maybe(maybe(measurement.prefix, prefix), "")
    measurement.suffix <- maybe(maybe(measurement.suffix, suffix), "")
    auxiliary.prefix <- maybe(maybe(auxiliary.prefix, prefix), "")
    auxiliary.suffix <- maybe(maybe(auxiliary.suffix, suffix), "")
    #
    newmn <- pasteSane0(measurement.prefix, measurement_names(tt), measurement.suffix)
    newan <- pasteSane0(auxiliary.prefix, auxiliary_names(tt), auxiliary.suffix)
    setnames(tt, c(measurement_names(tt), auxiliary_names(tt)), c(newmn, newan))
    #
    setattr(tt, "measurement.vars", newmn)
    setattr(tt,     "aux.vars",     newan)
    #
    tt
}

compute_proportions <- function(total, props, ties="random") {
    stopifnot(sum(props) <= 1)
    stopifnot(ties=="random")
    counts <- sapply(props, function(p) floor(p * total))
    left <- floor(total*sum(props) - sum(counts))
    if(left > 0)
        counts <- counts + rmultinom(1, left, props)
    if(!is.null(names(props)))
        names(counts) <- names(props)
    counts
}

#' Genrate cross validation sets
#'
#' Assign random indices to observations, useful for generating
#' train/test/validation sets.
#'
#' @param tt time.table to split
#' @param counts vector giving number of observations in each set
#' @param props vector giving proportion of observations in each set
#' @param sample.points whether to sample individual observations rather than whole time series (defaults to sampling time series)
#' @param cv.set.name name of column containing set labels
#' @param keep.all whether to keep datapoints not assigned to a subset
#'
#' @export
cv_assign_sets  <- 
    function( tt, counts = NULL, props = NULL
            , sample.points = FALSE
            , cv.set.name = "cv.set"
            , keep.all = FALSE ) {
    stopifnot(xor(is.null(counts), is.null(props)))
    idcol <- index_names(tt)
    tcol  <- time_name(tt)
    #
    idxs <- unique(index(tt, with.time=sample.points))
    #
    if(is.null(counts))
        counts <- compute_proportions(nrow(idxs), props)
    #
    labels <- if(!is.null(names(counts))) names(counts)
              else seq_along(counts)
    cv.sets <- c(rep(NA, nrow(idxs) - sum(counts))
                ,rep(labels, times=counts))[sample.int(nrow(idxs))]
    idxs[,eval(cv.set.name):=cv.sets]
    if(!keep.all)
        idxs <- idxs[get(cv.set.name) > 0]
    idxs
}

#' Split a data.table
#'
#' Splits a data.table according to a number of its columns.
#'
#' @param dt data.table to split
#' @param cols column names to split by
#'
#' Returns a list with attribute "values" containing the unique combination of
#' \code{cols} values associated to each element of the list.
#'
#' @export
split_by_cols <- function(dt, cols) {
    #
    split.by <- dt[,cols,with=F]
    #setkeyv(split.by, 
    unq <- unique(split.by)
    factor.name <- safe_name(unq)
    unq[,eval(factor.name):=as.factor(.I)]
    setkeyv(unq, cols)
    f <- unq[split.by, factor.name, with=FALSE]
    #
    dts <- lapply(split.data.frame(as.data.frame(dt), f), as.data.table)
    factor.vals <- lapply(dts, function(xs) unique(xs[1,cols,with=FALSE]))
    setattr(dts, "values", factor.vals)
}

#TODO: Make f optionally be a separate vector (in order to comply with
# the data.frame split spec?).
split.time.table <- function(x, f, drop=FALSE) {
    result <- split_by_cols(x, f)
    structure(lapply(result, same_str_as, tt=x), values=attr(result, "values"))
}

#' Split a time.table for cross validation
#'
#' Splits a time.table into a number of subsets useful for cross validation.
#'
#' @param tt time.table to split
#' @param counts vector containing number of observation in each subset
#' @param props vector of proportions of the full dataset for each subset
#' @param sample.points whether to sample individual observations rather than time.series as a whole (defaults to time.series as a whole)
#'
#' Names of the resulting list correspond to names used in the \code{counts} or
#' \code{props} vector.
#'
#' @export
cv_split_time_table <- function( tt, counts = NULL, props = NULL
                               , sample.points = FALSE ) {
    split.name <- safe_name(tt)
    splits <- cv_assign_sets( tt, counts, props, sample.points, keep.all=FALSE
                            , cv.set.name=split.name )
    result <- split_by_cols(tt[splits], split.name)
    nms <- unlist(attr(result, "values"))
    result <- lapply(result, same_str_as, tt=tt)
    names(result) <- nms
    result
}

#' World Bank data
#'
#' Construct a time.table from World Bank data.
#'
#' @param ... Arguments sent sent to \code{WDI} (primarily of interest are \code{country}, defaulting to "all", \code{start}, defaulting to 1961, \code{end}, defaulting to 2013, and \code{indicator}, defaulting to "NY.GDP.PCAP.PP.KD" (GDP per capita constant 2005 USD))
#' @param auxiliary.indicator additional indicators to include as auxiliary values
#' @param country.name column name to use for country code column (used as index), defaults to "iso2c" used by WDI
#' @param translation.table list mapping indicator code names to column names to be used in final \code{time.table}
#'
#' @export
#' @importFrom WDI WDI
from_WDI <- function( ..., auxiliary.indicator=c()
                    , country.name=NULL, translation.table=NULL ) {
    require(WDI)
    args <- list(...)
    indicator <- args$indicator
    #
    args$country   <- maybe( args$country   , "all" )
    args$start     <- maybe( args$start     , 1961  )
    args$end       <- maybe( args$end       , 2013  )
    args$indicator <- c( maybe( maybe(args$indicator, names(translation.table))
                              , "NY.GDP.PCAP.PP.KD" )
                       , auxiliary.indicator )
    #
    dt <- data.table(do.call(WDI, args))
    #
    extra <- c( "country", "iso3c"
              , "region", "capital"
              , "longitude", "latitude"
              , "income", "lending" )
    additional <- intersect(colnames(dt, extra))
        
    #
    if(!is.null(country.name)) setnames(dt, "iso2c", country.name)
    if(!is.null(translation.table)) {
        from <- intersect(names(translation.table), indicator)
        to   <- unlist(translation.table[from])
        if(length(from) > 0) {
            setnames(dt, from, to)
            indicator[indicator == from] <- to
        }
        from <- intersect(names(translation.table), auxiliary.indicator)
        to   <- unlist(translation.table[from])
        if(length(from) > 0) {
            setnames(dt, from, to)
            auxiliary.indicator[auxiliary.indicator == from] <- to
        }
    }
    #
    as.time.table( dt, maybe(country.name, "iso2c"), "year"
                 , indicator, c(auxiliary.indicator, additional) )
}

#' Role of column
#'
#' Gives the roles columns play in a time.table, either "index", "time",
#' "measurement", or "auxiliary"
#'
#' @param tt time.table to look for column in
#'
#' @export
roles <- function(tt) {
    rls <- list()
    rls[index_names(tt)]       <- "index"
    rls[time_name(tt)]         <- "time"
    rls[measurement_names(tt)] <- "measurement"
    rls[auxiliary_names(tt)]   <- "auxiliary"
    rls
}

#' Canges role of column
#'
#' Changes the role of columns in a time.table, making sure relevant internal
#' structure is canged.
#'
#' @param tt time.table to swich column roles in
#' @param changes list or character vector of role changes, values each being one of "index", "time", "measurement", and "auxiliary"
#' @param columns list of columns of same length as \code{changes}, defaults to names of elements in \code{changes}
#' @param destructive whether to mutate the time.table, defaults to True
#'
#' @export
promote <- function(tt, changes, columns=NULL, destructive=TRUE) {
    columns <- maybe(columns, names(changes))
    if(!all(columns %in% colnames(tt))) {
        stop(paste("No such column: ", setdiff(columns, colnames(tt)), collapse="\n"))
    }
    stopifnot(all(changes %in% c("index", "time", "measurement", "auxiliary")))
    rls <- roles(tt)
    rls[columns] <- changes
    new.layout <- split(names(rls), as.character(rls))
    if(is.null(new.layout$index)) stop("time.table without index not yet supported...")
    if(is.null(new.layout$time)) stop("time.table without time not yet supported...")
    result <- if(destructive) tt else copy(tt)
    setattr(result,          "id.vars", new.layout$index)
    setattr(result,         "time.var", new.layout$time)
    setattr(result, "measurement.vars", new.layout$measurement)
    setattr(result,         "aux.vars", new.layout$auxiliary)
    setkeyv(result, c(new.layout$index, new.layout$time))
    result
}

#' Create an integer index
#'
#' Create a single column integer index to replace the current one.
#'
#' @param tt time.table to replace index of
#' @param new.index.name name of new index column, defaults to "index"
#'
#' @export
integer_index <- function(tt, new.index.name="index") {
    tt2 <- copy(tt)
    unique.indices <- unique(index(tt2))
    unique.indices[,eval(new.index.name):=.I]
    tt2[[new.index.name]] <-
        unique.indices[index(tt2), new.index.name, with=FALSE][[new.index.name]]
    #
    as.time.table( tt2, new.index.name, time_name(tt2)
                 , measurement_names(tt2)
                 , c(index_names(tt2), auxiliary_names(tt2))
                 , attr(tt2, "frequency") )
}

#' Create an integer time
#'
#' Create a (discrete) integer based time column
#'
#' @param tt time.table to replace time of
#' @param new.time.name name of new time column, defaults to "time"
#'
#' @export
integer_time <- function(tt, new.time.name="time") {
    tt2 <- copy(tt)
    unique.times <- unique(time(tt2, rekey=TRUE))
    # TODO: This sould be done in a way that is consistent with frequency info
    unique.times[,eval(new.time.name):=.I]
    tt2[[new.time.name]] <- unique.times[time(tt2),new.time.name,with=FALSE][[new.time.name]]
    #
    # TODO: make the frequency thing more clever
    as.time.table( tt2, index_names(tt2), new.time.name
                 , measurement_names(tt2)
                 , c(time_name(tt2), auxiliary_names(tt2)) )
}

#' Create pdata.frame from time.table
#'
#' @param tt time.table to convert
#'
#' @export 
as_pdata_frame <- function(tt) {
    require(plm)
    if(length(index_names(tt)) > 1) stop("as_pdata_frame: pdata.frame only supports single column indices")
    pdata.frame(tt, index_names(tt, with.time=TRUE))
}
