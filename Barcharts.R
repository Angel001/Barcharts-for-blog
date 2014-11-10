### packages dplyr, ggplot2, RColorBrewer

library(dplyr)
library(ggplot2)
library(RColorBrewer)

## Getting data and preparing it

download.file("http://visionproblemsus.org/downloads/2012_vpus_master.csv",
              "2012_vpus_master.csv")
vpus <- read.csv("2012_vpus_master.csv")
vpus <- filter(vpus, state != "state")
vpus <- droplevels(vpus)
vp1 <- filter(vpus, state == "UNITED_STATES", race != "ALL", sex != "ALL",
              age != "ALL")
levels(vp1$vp) <- c("AMD", "BLINDNESS", "CATARACT", "DIABETIC RETINOPATHY",
                    "GLAUCOMA", "HYPEROPIA", "MYOPIA", "VISION IMPAIRMENT")
vp1 <- droplevels(vp1)

## Making aggregates

dfvp <- group_by(vp1, vp) %>%
        summarise(prev = sum(prev), pop = sum(pop),
                  rate.pct = round(prev/pop, 4)*100)

agevp <- group_by(vp1, vp, age) %>%
        summarise(prev = sum(prev), pop = sum(pop),
                  rate.pct = round(prev/pop, 4)*100)

sexvp <- group_by(vp1, vp, sex) %>%
        summarise(prev = sum(prev), pop = sum(pop),
                  rate.pct = round(prev/pop, 4)*100)

bothvp <- group_by(vp1, vp, sex, age) %>%
        summarise(prev = sum(prev), pop = sum(pop),
                  rate.pct = round(prev/pop, 4)*100)

# Plots

ggplot(dfvp, aes(x = vp, y = rate.pct, fill = vp)) +
        theme_bw() +
        geom_bar(stat = "identity", position = "dodge") +
        ylab("Prevalence rate in %") +
        xlab("") +
        guides(fill = F) +
        scale_y_continuous(breaks = c(0, 2, seq(5, 25, 5))) +
        scale_fill_brewer(palette ="Dark2")

a1 <- filter(agevp, vp != "DIABETIC RETINOPATHY")
ggplot(a1, aes(x = vp, y = rate.pct, fill = age)) +
        geom_bar(stat = "identity", position = "dodge", colour = "black") + theme_bw() +
        ylab("Prevalence rate in %") + xlab("") + 
        labs(fill="Age") + scale_fill_brewer(palette ="PuBu") +
        scale_y_continuous(breaks = c(0, 2, 5, seq(10,70,10)))

a2 <- filter(agevp, vp == "DIABETIC RETINOPATHY")
ggplot(a2, aes(x = age, y = rate.pct, fill = age)) +
        theme_bw() +
        xlab("") +
        geom_bar(stat = "identity", position = "dodge", colour = "black", width=1) +
        ylab("Prevalence rate in %") +
        scale_fill_brewer(palette ="PuBu") +
        scale_y_continuous(limits = c(0, 56), breaks = c(0, 2, 5, seq(10,60,10))) +
        guides(fill = F) +
        ggtitle("DIABETIC\n RETINOPATHY")

ggplot(sexvp, aes(x = vp, y = rate.pct, fill = sex)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_bw() +
        ylab("Prevalence rate in %") +
        xlab("") +
        labs(fill="Sex") +
        scale_y_continuous(limits = c(0, 25), breaks = c(0, 2, 5, seq(10,30,5))) +
        scale_fill_brewer(palette = "Paired") + 
        theme(legend.position = "top")

ggplot(bothvp, aes(x = age, y = rate.pct, fill = sex)) +
        theme_bw() +
        geom_bar(stat = "identity", position = "dodge") +
        xlab("Age") +
        ylab("Prevalence rate in %") +
        facet_wrap( ~ vp, scales = "free_x", nrow=2) +
        scale_y_continuous(breaks=c(0, 2, 5, seq(10,70,10))) +
        scale_fill_brewer(palette = "Paired") +
        guides(fill=guide_legend(title=NULL)) +
        theme(legend.position = "top",
              strip.text = element_text(size=rel(1.25)),
              strip.background = element_rect(fill="lightblue"),
              legend.title = element_text(size = 14),
              axis.title.x = element_text(size = rel(1.25)),
              axis.title.y = element_text(size = rel(1.25)))
