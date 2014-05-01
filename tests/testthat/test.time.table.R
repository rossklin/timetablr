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

library(testthat)
library(data.table)

random.dataset <- function(number=list(), names=list(), prefix=list(), generator=list(), extend.names=NULL) {
    nid <- maybe(number$index, sample.int(4, 1))
    stopifnot(nid >= 0)
    if(nid == 0) stop("random.dataset: Cannot have a time.table without index column")
    ntm <- maybe(number$time, 1)
    stopifnot(ntm >= 0)
    if(ntm == 0) stop("random.dataset: Cannot have a time.table without time column")
    nms <- maybe(number$measurement, sample.int(8, 1))
    stopifnot(nms >= 0)
    nax <- maybe(number$auxiliary, sample.int(4, 1))
    stopifnot(nax >= 0)

    nent <- maybe(number$entities, sample.int(20, 1))
    stopifnot(nent >= 0)
    if(nent == 0) warning("time.table without entities will be empty")
    ntp  <- maybe(number$timepoints, sample.int(20, 1))
    stopifnot(ntp >= 0)
    if(ntp == 0) warning("time.table without time points will be empty")
    nob <- nent*ntp

    idn <- maybe(names$index, pasteSane0(maybe(prefix$index, "I"), rstring(nid)))
    stopifnot(length(idn) <= nid)
    if(length(idn) < nid) {
        if(!is.null(extend.names) & extend.names) {
            idn <- c(idn, paste0(maybe(prefix$index, "I"), rstring(nid-length(idn))))
        } else if(!is.null(extend.names) & !extend.names) {
            stop("random.dataset: Insufficient index names provided.")
        } else {
            warning("random.dataset: Insufficient index names provided, set extend.names=TRUE to disable this warning.")
        }
    }

    tmn <- maybe(names$time, pasteSane0(maybe(prefix$index, "T"), rstring(ntm)))
    stopifnot(length(tmn) <= ntm)
    if(length(tmn) < ntm) {
        if(!is.null(extend.names) & extend.names) {
            tmn <- c(tmn, paste0(maybe(prefix$index, "I"), rstring(ntm-length(tmn))))
        } else if(!is.null(extend.names) & !extend.names) {
            stop("random.dataset: Insufficient time names provided.")
        } else {
            warning("random.dataset: Insufficient time names provided, set extend.names=TRUE to disable this warning.")
        }
    }

    msn <- maybe(names$measurement, pasteSane0(maybe(prefix$index, "M"), rstring(nms)))
    stopifnot(length(msn) <= nms)
    if(length(msn) < nms) {
        if(!is.null(extend.names) & extend.names) {
            msn <- c(msn, paste0(maybe(prefix$index, "I"), rstring(nms-length(msn))))
        } else if(!is.null(extend.names) & !extend.names) {
            stop("random.dataset: Insufficient measurement names provided.")
        } else {
            warning("random.dataset: Insufficient measurement names provided, set extend.names=TRUE to disable this warning.")
        }
    }

    axn <- maybe(names$auxiliary, pasteSane0(maybe(prefix$index, "A"), rstring(nax)))
    stopifnot(length(axn) <= nax)
    if(length(axn) < nax) {
        if(!is.null(extend.names) & extend.names) {
            axn <- c(axn, paste0(maybe(prefix$index, "I"), rstring(nax-length(axn))))
        } else if(!is.null(extend.names) & !extend.names) {
            stop("random.dataset: Insufficient measurement names provided.")
        } else {
            warning("random.dataset: Insufficient measurement names provided, set extend.names=TRUE to disable this warning.")
        }
    }

    # A couple of simple generators
    clss <- c( rstring # character
             , rnorm   # numeric
             , function(n) sample.int(max(5, as.integer(nob/100)), n, TRUE) # small integer
             , function(n) sample.int(min(100, as.integer(nob*100)), n, TRUE) # large integer
             , function(n) as.logical(sample.int(2, n, TRUE)-1) ) # logical

    hack.first.column <- TRUE
    idgen <- maybe(generator$index, function(n) {
        if(hack.first.column) sample.int(n, n, FALSE)
        else sample(clss[-2], 1, TRUE)[[1]](n)
    })
    mid <- as.data.frame(replicate(simplify=FALSE, n=nid, idgen(nent)))
    setnames(mid, colnames(mid), idn)

    tmdelta <- maybe(generator$delta, function(n) sample.int(100, n, TRUE))(ntm)
    tmgen <- maybe(generator$time, function(n, d) seq(by=d, length.out=n))
    mtm <- as.data.frame(lapply(tmdelta, function(d) tmgen(ntp, d)))
    setnames(mtm, colnames(mtm), tmn)

    midtm <- expand.data.frames(mid, mtm)
    setnames(midtm, colnames(midtm), c(idn, tmn))

    msd <- if(nms > 0) {
        msgen <- maybe(generator$measurement, rnorm)
        msd.tmp <- as.data.frame(replicate(nms, msgen(nob), simplify = FALSE))
        setnames(msd.tmp, colnames(msd.tmp), msn)
        msd.tmp
    } else {
        NULL
    }

    maux <- if(nax > 0) {
        auxgen <- maybe(generator$auxiliary, function(n) sample(clss, nax, TRUE)[[1]](n))
        maux.tmp <- as.data.frame(replicate(nax, auxgen(nob), simplify = FALSE))
        setnames(maux.tmp, colnames(maux.tmp), axn)
        maux.tmp
    } else {
        NULL
    }

    mdf <- do.call( function(...) data.frame(..., stringsAsFactors=FALSE)
                  , c(list(midtm), maybe(msd, c()), maybe(maux, c())) )

    list( df=mdf
        , delta=tmdelta
        , number = list(index=nid, time=ntm, measurement=nms, auxiliary=nax, timepoints=ntp, entities=nent)
        , names  = list(index=idn, time=tmn, measurement=msn, auxiliary=axn)
        , classes = list( index=lapply(mid, class), time=lapply(mtm, class)
                        , measurement=lapply(msd, class), auxiliary=lapply(maux, class) )
        , data = list(index=mid, time=mtm, measurement=msd, auxiliary=maux) )
}

set.seed(1439874723)
test.tables <- list( general1 = random.dataset()
                   , general2 = random.dataset()
                   , general3 = random.dataset()
                   , no.measurement = random.dataset(number=list(measurement=0))
                   , no.aux = random.dataset(number=list(auxiliary=0))
                   , one.measurement = random.dataset(number=list(measurement=1))
                   , one.aux = random.dataset(number=list(auxiliary=1))
                   , only.index.time = random.dataset(number=list(measurement=0, auxiliary=0))
                   , trivial.index = random.dataset(number=list(entities=1))
                   , trivial.time = random.dataset(number=list(timepoints=1))
                   , trivial.index.time = random.dataset(number=list(timepoints=1, entities=1))
                   , almost.trivial =
                         random.dataset(number=list( timepoints=1, entities=1
                                                   , index=1, time=1
                                                   , measurement=0, auxiliary=0)) )

test.tables.dfs <- lapply(test.tables, function(xs) xs$df)

for(nm in names(test.tables)) {
    test.tables[[nm]]$dt <- as.data.table(test.tables[[nm]]$df)
}
test.tables.dts <- lapply(test.tables, function(xs) xs$dt)

for(nm in names(test.tables)) {
    xs <- test.tables[[nm]]
    test.tables[[nm]]$tt <- if(xs$number$timepoints > 1)
        as.time.table(xs$df, xs$names$index, xs$names$time, xs$names$measurement, xs$names$auxiliary)
    else
        as.time.table(xs$df, xs$names$index, xs$names$time, xs$names$measurement, xs$names$auxiliary
                     , frequency=list( from=min(xs$data$time), to=max(xs$data$time)
                                     , delta=xs$timedelta ) )
}
test.tables.tts <- lapply(test.tables, function(xs) xs$tt)

test_all <- function(title, what) {
    context(title)
    for(nm in names(test.tables)) {
        set.seed(483352)
        test_that(paste(title, "in", nm), {
            what(test.tables[[nm]])
        })
    }
}

test_all( "as.time.table does not produce unexpected structure", function(dat) with(dat, {
    expect_that(setequal(colnames(tt), colnames(df)), is_true())
    expect_that(nrow(tt), equals(with(number, timepoints*entities)))
}))

test_all("as.time.table stores column kinds and internal order", function(dat) with(dat, {

    expect_that(index_names(tt), equals(colnames(data$index)))
    expect_that(time_name(tt), equals(colnames(data$time)))
    expect_that(measurement_names(tt), equals(maybe(colnames(data$measurement), character())))
    expect_that(auxiliary_names(tt), equals(maybe(colnames(data$auxiliary), character())))
}))

test_all("as.time.table adds correct entity/time key", function(dat) with(dat, {
    expect_that(key(tt), equals(c(index_names(tt), time_name(tt))))
}))

test_all("as.time.table guesses correct frequency information", function(dat) with(dat, {
    if(number$timepoints > 1) {
        expect_equal(deltat(tt), delta)
    } else {
        expect_equal(deltat(tt), NULL)
    }
}))

test_all("as.time.table sets key correctly", function(dat) with(dat, {
    expect_equal(key(tt), c(index_names(tt), time_name(tt)))
}))

test_all("Column subsetting operations preserve (at least number of) rows", function(dat) with(dat, {
    expect_equal(nrow(tt), nrow(index(tt)))
    expect_equal(nrow(tt), nrow(time(tt)))
    if(number$measurement > 0)
        expect_equal(nrow(tt), nrow(measurement(tt)))
    if(number$auxiliary > 0)
        expect_equal(nrow(tt), nrow(auxiliary(tt)))
}))

test_all("Column subsetting operations add correct key when requested", function(dat) with(dat, {
    expect_equal( key(subset.time.table.parts(tt, with.index=T, with.time=T, rekey=T))
                , key(tt) )
    expect_equal( key(subset.time.table.parts(tt, with.index=T, with.time=F, rekey=T))
                , index_names(tt) )
    expect_equal( key(subset.time.table.parts(tt, with.index=F, with.time=T, rekey=T))
                , time_name(tt) )
    # measurement/auxiliary are generic functions introduced in timetablR,
    # whence the dedicated tests (had some issues with optional arguments to
    # generic functions).
    expect_equal( key(measurement(tt, with.index=T, with.time=T, rekey=T))
                , key(tt) )
    expect_equal( key(auxiliary(tt, with.index=T, with.time=T, rekey=T))
                , key(tt) )
}))

# TODO: Update this to the new index/times behaviour (i.e. add an example where
# one indexes using a data.table with additional columns.
test_all("Subsetting by expression produces the correct (number of) rows and cols", function(dat) with(dat, {
    picks.name <- safe_name(tt)
    test.picks <- as.logical(sample.int(2, nrow(tt), T)-1)
    subset.external.expr <- subset(tt, expr=test.picks)
    tt2 <- as.time.table( copy(tt)[,eval(picks.name):=test.picks]
                        , index_names(tt), time_name(tt)
                        , c(measurement_names(tt), picks.name)
                        , auxiliary_names(tt)
                        , frequency=attr(tt, "frequency") )
    subset.internal.expr <- subset(tt2, expr=get(picks.name)==TRUE)
    expect_equal(nrow(subset.external.expr), sum(test.picks))
    expect_equal(nrow(subset.internal.expr), sum(test.picks))
}))

test_all("Subsetting times guesses new frequency", function(dat) with(dat, {
    if(number$timepoints >= 4) {
        tps <- unique(time(tt))
        tps <- tps[rep(c(T, F), floor(nrow(tps)/2))]
        tt2 <- subset(tt, index=unique(index(tt)), times=tps)
        expect_equal(deltat(tt2), 2*deltat(tt))
    }
}))

test_all("subsetting indices with additonal columns preserved extra columns", function(dat) with(dat, {
    ss.index <- as.data.table(data$index)[sample.int(nrow(data$index), 3, T)][,extra.column:=.I]
    tt2 <- subset(tt, index=ss.index)
    expect_true("extra.column" %in% auxiliary_names(tt2))
    extracted <- unique(setkeyv(tt2[,c(index_names(tt2), "extra.column"),with=F], c(index_names(tt2), "extra.column")))
    expect_equivalent(extracted, ss.index)
}))

test_all("lag preserves exact index/time (if requested)", function(dat) with(dat, {
    if(number$timepoints > 1) {
        ttl <- lag(tt)
        expect_equal(index(tt), index(ttl))
        expect_equal(time(tt), time(ttl))
    }
}))

test_all("lag preserves columns and time.table structure", function(dat) with(dat, {
    if(number$timepoints > 1) {
        ttl <- lag(tt)
        expect_equal(colnames(tt), colnames(ttl))
        expect_true("time.table" %in% class(tt))
        expect_equal(index_names(tt), index_names(ttl))
        expect_equal(time_name(tt), time_name(ttl))
        expect_equal(measurement_names(tt), measurement_names(ttl))
        expect_equal(auxiliary_names(tt), auxiliary_names(ttl))
    }
}))

test_all("lag doesn't change auxiliary values", function(dat) with(dat, {
    if(number$timepoints > 1)
        expect_equal(auxiliary(tt), auxiliary(lag(tt)))
}))

test_all("lag lags measurements", function(dat) with(dat, {
    if(number$measurement > 0 & number$timepoints > 1) {
        lagged1 <- lag(tt)
        lagged2 <- copy(tt)[,eval(time_name(tt)):=get(time_name(tt))-delta]
        mn  <- measurement_names(tt)
        mnl <- paste0("LAGGED", measurement_names(tt))
        setnames(lagged2, mn, mnl)
        merged <- data.table:::merge.data.table( lagged1, lagged2
                                                , by=c(index_names(tt), time_name(tt))
                                                , all=FALSE )
        for(i in seq_along(mn)) {
            expect_equivalent(merged[[mn[i]]] - merged[[mnl[i]]], rep(0, nrow(merged)))
        }
    }
}))

test_all("negative lag works as expected", function(dat) with(dat, {
    if(number$measurement > 0 & number$timepoints > 1) {
        lagged1 <- lag(tt, steps=-1L)
        lagged2 <- copy(tt)[,eval(time_name(tt)):=get(time_name(tt))+delta]
        mn  <- measurement_names(tt)
        mnl <- paste0("LAGGED", measurement_names(tt))
        setnames(lagged2, mn, mnl)
        merged <- data.table:::merge.data.table(lagged1, lagged2, by=c(index_names(tt), time_name(tt)), all=FALSE)
        for(i in seq_along(mn)) {
            expect_equivalent(merged[[mn[i]]] - merged[[mnl[i]]], rep(0, nrow(merged)))
        }
    }
}))


test_all("diff preserves exact index/time (if requested)", function(dat) with(dat, {
    if(number$timepoints > 1) {
        ttd <- diff(tt)
        expect_equal(index(tt), index(ttd))
        expect_equal(time(tt), time(ttd))
    }
}))

test_all("diff preserves columns and time.table structure", function(dat) with(dat, {
    if(number$timepoints > 1) {
        ttd <- diff(tt)
        expect_equal(colnames(tt), colnames(ttd))
        expect_true("time.table" %in% class(ttd))
        expect_equal(index_names(tt), index_names(ttd))
        expect_equal(time_name(tt), time_name(ttd))
        expect_equal(measurement_names(tt), measurement_names(ttd))
        expect_equal(auxiliary_names(tt), auxiliary_names(ttd))
    }
}))

test_all("affix_names works the way one expects", function(dat) with(dat, {

    ttc <- copy(tt)
    affix_names(ttc, NULL, NULL, "a", "b", "c", "d")
    expect_equal( measurement_names(ttc)
                 , pasteSane0("a", measurement_names(tt), "b") )
    expect_equal( auxiliary_names(ttc)
                 , pasteSane0("c", auxiliary_names(tt), "d") )
    affix_names(ttc, "x", "y")
    expect_equal( measurement_names(ttc)
                 , pasteSane0("xa", measurement_names(tt), "by") )
    expect_equal( auxiliary_names(ttc)
                 , pasteSane0("xc", auxiliary_names(tt), "dy") )
    expect_equal( index_names(ttc)
                 , index_names(tt) )
    expect_equal( time_name(ttc)
                 , time_name(tt) )
}))

test_all("diff diffs values", function(dat) with(dat, {
    if(number$measurement > 0 & number$timepoints > 1) {
        diffed <- diff(tt)
        mn  <- measurement_names(tt)
        mnd <- paste0("D", measurement_names(tt))
        affix_names(diffed, measurement.prefix="D")
        diffed <- lag(diffed, steps=-1L)
        for(i in seq_along(mn)) {
            col <- mnd[i]
            base <- tt[, list(baseval=head(get(mn[i])))
                       , keyby=c(index_names(tt), time_name(tt)) ]
            diffed[ get(time_name(diffed)) == start(diffed)
                   , eval(col):=base[as.data.table(.BY)]$baseval
                   , by=eval(c(index_names(tt), time_name(tt))) ]
            diffed[,eval(col):=cumsum(get(col)),by=eval(index_names(tt))]
        }
        merged <- data.table:::merge.data.table(tt, diffed, by=c(index_names(tt), time_name(tt)), all=FALSE)
        for(i in seq_along(mn)) {
            expect_equivalent(merged[[mn[i]]] - merged[[mnd[i]]], rep(0, nrow(merged)))
        }
        expect_equivalent(merged[,index_names(tt),with=F], index(tt))
        expect_equivalent(merged[,time_name(tt),with=F], time(tt))
    }
}))

test_all("embed", function(dat) with(dat, {
    if(number$measurement > 0 & number$timepoints > 4) {
        test.dim <- 2
        embedding <- embed(tt, dimension=test.dim)
        correct <- function(dt) {
            for(col in measurement_names(tt)) {
                for(i in seq_len(test.dim)) {
                    if(!all.equal(head(dt[[col]], -i), tail(dt[[paste0("lag.", col, ".", i)]], -i)))
                        return(FALSE)
                }
            }
            TRUE
        }
        diagnostic <- embedding[, correct(.SD)
                                , .SDcols=measurement_names(embedding)
                                , by=c(index_names(tt), time_name(tt)) ]
        expect_true(all(diagnostic$V1))
    }
}))


test_all("Merging time.table produces time.table", function(dat) with(dat, {
    tt2 <- copy(tt)
    affix_names(tt2, prefix="SAFE.")
    tt.new <- merge(tt, tt2)
    expect_is(tt.new, "time.table")
}))

test_all("disjoint time.tables merge trivially", function(dat) with(dat, {
    tt2 <- copy(tt)
    affix_names(tt2, prefix="TEST.ME.")
    merge1 <- as.data.table(merge(tt, tt2, all=TRUE))
    merge2 <- as.data.table(data.table:::merge.data.table(tt, tt2, all=TRUE, by=c(index_names(tt, TRUE))))
    expect_true(setequal(colnames(merge1), colnames(merge2)))
    expect_equivalent(merge1, merge2[,colnames(merge1),with=F])
}))

test_all("merging table with itself using 'add' doubles values", function(dat) with(dat, {
    if(number$measurement > 0) {
        funs <- setNames(list(`+`)[rep(1, number$measurement)], names$measurement)
        tt2 <- copy(tt)
        affix_names(tt2, auxiliary.prefix="foobarbaz")
        tt3 <- merge(tt, tt2, funs)
        for(col in measurement_names(tt))
            expect_equal(tt3[[col]], tt[[col]] + tt2[[col]])
    }
}))

test_all("promote destructively alters structure (if and only if requested)", function(dat) with(dat, {
    if(number$measurement > 0 & number$auxiliary > 0) {
        num   <- min(number$measurement, number$auxiliary)
        swap1 <- head(names$measurement, num)
        swap2 <- head(names$auxiliary, num)
        swaps <- c(setNames(rep("auxiliary", num), swap1), setNames(rep("measurement", num), swap2))
        tt2 <- copy(tt)
        #
        tt3 <- promote(tt2, swaps, destructive=FALSE)
        expect_true(all(swap1 %in% auxiliary_names(tt3)))
        expect_true(all(swap2 %in% measurement_names(tt3)))
        expect_equivalent(tt, tt2)
        #
        promote(tt2, swaps, destructive=TRUE)
        expect_true(all(swap1 %in% auxiliary_names(tt2)))
        expect_true(all(swap2 %in% measurement_names(tt2)))
    }
}))

test_all("nobs agrees with data from generator", function(dat) with(dat, {
    expect_equal(nobs(tt, units=T), number$entities)
    expect_equal(nobs(tt, units=F), number$entities * number$timepoints)
}))

test_all("promote propertly updates keys", function(dat) with(dat, {
    tt2 <- promote( tt
                  , c(rep("index",number$time), rep("time",number$index))
                  , c(time_name(tt), index_names(tt))
                  , destructive=FALSE )
    expect_equal(key(tt2), c(time_name(tt), index_names(tt)))
}))

## test_all("", function(dat) with(dat, {
## }))
