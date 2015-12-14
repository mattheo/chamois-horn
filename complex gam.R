library(mgcv)
load("db.RData")

## complex gamms: how far can we go?

system.time(
    fcham_c1 <-  gam(horn ~
            f.sex +
            s(weight, bs="ts") +
            s(Jday, bs="ts") +
            s(x.council, y.council) +
            s(q_media, bs="ts") +
            s(q_min, bs="ts") +
            s(q_max, bs="ts") +
            s(f.year, bs="re") +
            s(f.council_cod, bs="re") +
            f.substrate + s(density, bs="ts") +
            s(log.ndvi.slop1, bs="ts") +
            s(ndvi.maxincr1, bs="ts") +
            s(ndvi.may1.new, bs="ts") +
            s(log.ndvi.slop2, bs="ts") +
            s(ndvi.maxincr2, bs="ts") +
            s(ndvi.may2.new, bs="ts") +
            s(Perc.area.aperta, bs="ts") +
            s(snow_winter1, by=aspect, k=3, bs="ts") +
            s(r_spring1, k=3, bs="ts") +
            s(r_newsummer1, k=3, bs="ts") +
            s(r_autumn, k=3, bs="ts") +
            s(snow_winter2, by=aspect, k=3, bs="ts") +
            s(r_spring2, k=3, bs="ts") +
            s(r_newsummer2, k=3, bs="ts") +
            s(twinter.mean1, k=3, bs="ts") +
            s(tspring1.mean, k=3, bs="ts") +
            s(tsummer1.mean, k=3, bs="ts") +
            s(tautumn.mean, k=3, bs="ts") +
            s(twinter.mean2, k=3, bs="ts") +
            s(tspring2.mean, k=3, bs="ts") +
            s(tsummer2.mean, k=3, bs="ts"),
        data=db, REML=F)
)
summary(fcham_c1)
gam.check(fcham_c1)
plot(fcham_c1)

AIC(fcham_c1) # 21901.49


# NDVI summer instead of may
system.time(
#     fcham_c2 <-  gam(horn ~
#             f.sex +
#             s(weight, bs="ts") +
#             s(Jday, bs="ts") +
#             s(x.council, y.council) +
#             s(q_media, bs="ts") +
#             s(q_min, bs="ts") +
#             s(q_max, bs="ts") +
#             s(f.year, bs="re") +
#             s(f.council_cod, bs="re") +
#             f.substrate + s(density, bs="ts") +
#             s(log.ndvi.slop1, bs="ts") +
#             s(ndvi.maxincr1, bs="ts") +
#             s(ndvi.summer1, bs="ts") +
#             s(log.ndvi.slop2, bs="ts") +
#             s(ndvi.maxincr2, bs="ts") +
#             s(ndvi.summer2, bs="ts") +
#             s(Perc.area.aperta, bs="ts") +
#             s(snow_winter1, by=aspect, k=3, bs="ts") +
#             s(r_spring1, k=3, bs="ts") +
#             s(r_newsummer1, k=3, bs="ts") +
#             s(r_autumn, k=3, bs="ts") +
#             s(snow_winter2, by=aspect, k=3, bs="ts") +
#             s(r_spring2, k=3, bs="ts") +
#             s(r_newsummer2, k=3, bs="ts") +
#             s(twinter.mean1, k=3, bs="ts") +
#             s(tspring1.mean, k=3, bs="ts") +
#             s(tsummer1.mean, k=3, bs="ts") +
#             s(tautumn.mean, k=3, bs="ts") +
#             s(twinter.mean2, k=3, bs="ts") +
#             s(tspring2.mean, k=3, bs="ts") +
#             s(tsummer2.mean, k=3, bs="ts"),
#         data=db, REML=F)
    fcham_c2 <- update(fcham_c1, . ~ . - s(ndvi.may1.new, bs="ts") - s(ndvi.may2.new, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2, bs="ts"))
)

summary(fcham_c2)
gam.check(fcham_c2)
plot(fcham_c2)

AIC(fcham_c2)
