
library(survival)
data("lung")

res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno, data =  lung)

Publish::publish(res.cox, ci.digits=2, ci.handler="sprintf", 
        # digits=c(2,4),
        pvalue.eps=0.001,
        pvalue.digits=3,
        pvalue.stars=FALSE, ci.trim=FALSE,print=FALSE)$regressionTable

rawtab <- publish(rmodel1, ci.digits=2, ci.handler="sprintf", 
        pvalue.eps=0.001,
        pvalue.digits=3,
        pvalue.stars=FALSE, ci.trim=FALSE,print=FALSE)$rawTable

test <- do.call("rbind",rmodel1)

pp <- c("0.01", "<0.02", "0.0", "<")
length(test <- grepl("<",pp))

digits=c(2,3)

pvalue.defaults <- list(digits=digits[[2]],
                        eps=10^{-digits[[2]]},
                        stars=FALSE)
ci.defaults <- list(format="[l;u]",
                    digits=digits[[1]],
                    nsmall=digits[[1]],
                    degenerated="asis")
handler="sprintf"
nsmall <- NULL

smartF <- prodlim::SmartControl(call=list(ci.digits=2, ci.handler="sprintf", 
                                          pvalue.eps=0.001,
                                          pvalue.digits=3,
                                          pvalue.stars=FALSE, ci.trim=FALSE,print=FALSE),
                      keys=c("ci","pvalue"),
                      ignore=c("object","print","handler","digits","nsmall"),
                      defaults=list("ci"=ci.defaults,"pvalue"=pvalue.defaults),
                      forced=list("ci"=list(lower=rawtab[,"Lower"],
                                            upper=rawtab[,"Upper"],
                                            handler=handler,
                                            digits=digits[[1]],
                                            nsmall=nsmall[[1]]),
                                  "pvalue"=list(rawtab[,"Pvalue"])),
                      verbose=FALSE)

getfun<-function(x) {
  if(length(grep("::", x))>0) {
    parts<-strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    x
  }
}

pp <- do.call(getfun("Hmisc::format.pval"),smartF$pvalue)
pp
format(0.051, 5)


Hmisc::format.pval(c(0.35518, 0.01, 0.0003, 0.00000000032324), eps=0.001, digits=3)
pv <- list(pv=c(0.35518, 0.01, 0.0003), eps=0.001, digits=3)
pp <- do.call(getfun("base::format.pval"),pv)
pp

trace("format.pval", 
      tracer=quote({
        message(digits)
        message(eps)
      }),
      where=coxph,
      exit=quote(message(returnValue())))
