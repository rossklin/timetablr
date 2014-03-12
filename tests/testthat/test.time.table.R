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

expect_that_all <- function(lst, ...) lapply(setNames(nm=maybe(names(lst), seq_along(lst))), function(x) {
    expect_that(lst[[x]], ..., label=if(is.null(names(lst))) as.character(lst[[x]]) else x)
})

context("as.time.table does not produce unexpected structure")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        expect_that(setequal(colnames(tt), colnames(df)), is_true())
        expect_that(nrow(tt), equals(with(number, timepoints*entities)))
    })
}

context("as.time.table stores column kinds and internal order")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        expect_that(index.names(tt), equals(colnames(data$index)))
        expect_that(time.name(tt), equals(colnames(data$time)))
        expect_that(measurement.names(tt), equals(maybe(colnames(data$measurement), character())))
        expect_that(auxiliary.names(tt), equals(maybe(colnames(data$auxiliary), character())))
    })
}

context("as.time.table adds correct entity/time key")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        expect_that(key(tt), equals(c(index.names(tt), time.name(tt))))
    })
}

context("as.time.table guesses correct frequency information")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        if(number$timepoints > 1) {
            expect_equal(deltat(tt), delta)
        } else {
            expect_equal(deltat(tt), NULL)
        }
    })
}

context("as.time.table sets key correctly")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        expect_equal(key(tt), c(index.names(tt), time.name(tt)))
    })
}

context("Column subsetting operations preserve (at least number of) rows")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        expect_equal(nrow(tt), nrow(index(tt)))
        expect_equal(nrow(tt), nrow(time(tt)))
        if(number$measurement > 0)
            expect_equal(nrow(tt), nrow(measurement(tt)))
        if(number$auxiliary > 0)
            expect_equal(nrow(tt), nrow(auxiliary(tt)))
    })
}

context("Column subsetting operations add correct key when requested")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        expect_equal( key(subset.time.table.parts(tt, with.index=T, with.time=T, rekey=T))
                    , key(tt) )
        expect_equal( key(subset.time.table.parts(tt, with.index=T, with.time=F, rekey=T))
                    , index.names(tt) )
        expect_equal( key(subset.time.table.parts(tt, with.index=F, with.time=T, rekey=T))
                    , time.name(tt) )
        # measurement/auxiliary are generic functions introduced in timetablR,
        # whence the dedicated tests (had some issues with optional arguments to
        # generic functions).
        expect_equal( key(measurement(tt, with.index=T, with.time=T, rekey=T))
                    , key(tt) )
        expect_equal( key(auxiliary(tt, with.index=T, with.time=T, rekey=T))
                    , key(tt) )
    })
}

context("Subsetting by expression produces the correct (number of) rows and cols")
for(nm in names(test.tables)) {
    set.seed(483352)
    with(test.tables[[nm]], {
        picks.name <- safe.name(tt)
        test.picks <- as.logical(sample.int(2, nrow(tt), T)-1)
        subset.external.expr <- subset(tt, expr=test.picks)
        tt2 <- as.time.table( copy(tt)[,eval(picks.name):=test.picks]
                            , index.names(tt), time.name(tt)
                            , c(measurement.names(tt), picks.name)
                            , auxiliary.names(tt)
                            , frequency=attr(tt, "frequency") )
        subset.internal.expr <- subset(tt2, expr=get(picks.name)==TRUE)
        expect_equal(nrow(subset.external.expr), sum(test.picks))
        expect_equal(nrow(subset.internal.expr), sum(test.picks))
    })
}

context("Subsetting times guesses new frequency")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        if(number$timepoints >= 4) {
            tps <- unique(time(tt))
            tps <- tps[rep(c(T, F), floor(nrow(tps)/2))]
            tt2 <- subset(tt, index=unique(index(tt)), times=tps)
            expect_equal(deltat(tt2), 2*deltat(tt))
        }
    })
}

context("lag preserves exact index/time (if requested)")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        if(number$timepoints > 1) {
            ttl <- lag(tt)
            expect_equal(index(tt), index(ttl))
            expect_equal(time(tt), time(ttl))
        }
    })
}

context("lag preserves columns and time.table structure")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        if(number$timepoints > 1) {
            ttl <- lag(tt)
            expect_equal(colnames(tt), colnames(ttl))
            expect_true("time.table" %in% class(tt))
            expect_equal(index.names(tt), index.names(ttl))
            expect_equal(time.name(tt), time.name(ttl))
            expect_equal(measurement.names(tt), measurement.names(ttl))
            expect_equal(auxiliary.names(tt), auxiliary.names(ttl))
        }
    })
}

context("lag doesn't change auxiliary values")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        if(number$timepoints > 1)
            expect_equal(auxiliary(tt), auxiliary(lag(tt)))
    })
}

context("lag lags measurements")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        if(number$measurement > 0 & number$timepoints > 1) {
            lagged1 <- lag(tt)
            lagged2 <- copy(tt)[,eval(time.name(tt)):=get(time.name(tt))-delta]
            mn  <- measurement.names(tt)
            mnl <- paste0("LAGGED", measurement.names(tt))
            setnames(lagged2, mn, mnl)
            merged <- merge(lagged1, lagged2, by=c(index.names(tt), time.name(tt)), all=FALSE)
            for(i in seq_along(mn)) {
                expect_equivalent(merged[[mn[i]]] - merged[[mnl[i]]], rep(0, nrow(merged)))
            }
        }
    })
}

context("negative lag works as expected")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        if(number$measurement > 0 & number$timepoints > 1) {
            lagged1 <- lag(tt, steps=-1L)
            lagged2 <- copy(tt)[,eval(time.name(tt)):=get(time.name(tt))+delta]
            mn  <- measurement.names(tt)
            mnl <- paste0("LAGGED", measurement.names(tt))
            setnames(lagged2, mn, mnl)
            merged <- merge(lagged1, lagged2, by=c(index.names(tt), time.name(tt)), all=FALSE)
            for(i in seq_along(mn)) {
                expect_equivalent(merged[[mn[i]]] - merged[[mnl[i]]], rep(0, nrow(merged)))
            }
        }
    })
}


context("diff preserves exact index/time (if requested)")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        if(number$timepoints > 1) {
            ttd <- diff(tt)
            expect_equal(index(tt), index(ttd))
            expect_equal(time(tt), time(ttd))
        }
    })
}

context("diff preserves columns and time.table structure")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        if(number$timepoints > 1) {
            ttd <- diff(tt)
            expect_equal(colnames(tt), colnames(ttd))
            expect_true("time.table" %in% class(ttd))
            expect_equal(index.names(tt), index.names(ttd))
            expect_equal(time.name(tt), time.name(ttd))
            expect_equal(measurement.names(tt), measurement.names(ttd))
            expect_equal(auxiliary.names(tt), auxiliary.names(ttd))
        }
    })
}

context("affix.names works the way one expects")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        ttc <- copy(tt)
        affix.names(ttc, NULL, NULL, "a", "b", "c", "d")
        expect_equal( measurement.names(ttc)
                    , pasteSane0("a", measurement.names(tt), "b") )
        expect_equal( auxiliary.names(ttc)
                    , pasteSane0("c", auxiliary.names(tt), "d") )
        affix.names(ttc, "x", "y")
        expect_equal( measurement.names(ttc)
                    , pasteSane0("xa", measurement.names(tt), "by") )
        expect_equal( auxiliary.names(ttc)
                    , pasteSane0("xc", auxiliary.names(tt), "dy") )
        expect_equal( index.names(ttc)
                    , index.names(tt) )
        expect_equal( time.name(ttc)
                    , time.name(tt) )
    })
}

context("diff diffs values")
for(nm in names(test.tables)) {
    with(test.tables[[nm]], {
        if(number$measurement > 0 & number$timepoints > 1) {
            diffed <- diff(tt)
            mn  <- measurement.names(tt)
            mnd <- paste0("D", measurement.names(tt))
            affix.names(diffed, measurement.prefix="D")
            diffed <- lag(diffed, steps=-1L)
            for(i in seq_along(mn)) {
                col <- mnd[i]
                base <- tt[, list(baseval=head(get(mn[i])))
                           , keyby=c(index.names(tt), time.name(tt)) ]
                diffed[ get(time.name(diffed)) == start(diffed)
                      , eval(col):=base[as.data.table(.BY)]$baseval
                      , by=eval(c(index.names(tt), time.name(tt))) ]
                diffed[,eval(col):=cumsum(get(col)),by=eval(index.names(tt))]
            }
            merged <- merge(tt, diffed, by=c(index.names(tt), time.name(tt)), all=FALSE)
            for(i in seq_along(mn)) {
                expect_equivalent(merged[[mn[i]]] - merged[[mnd[i]]], rep(0, nrow(merged)))
            }
            expect_equivalent(merged[,index.names(tt),with=F], index(tt))
            expect_equivalent(merged[,time.name(tt),with=F], time(tt))
        }
    })
}

## context("")
## for(nm in names(test.tables)) {
##     with(test.tables[[nm]], {
##     })
## }
