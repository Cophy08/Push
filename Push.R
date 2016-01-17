# Push
# Last edited 1/17/2016
# Manny

## NOTES:
# REDUCE SIZE? NEED TO SHORTEN LOAD TIME
# REDUCE AT LEAST ONE OF STRENGTH OR SCORE STATE (GROUP PP/SH OR LEADING/TRAILING)
# KEEP ONLY LINES AND PAIRINGS (SET MIN TOI?)

# Load libraries
library(RSQLite)
library(dplyr)

## Load DB Tables

# Link to database
link <- "~/Documents/dryscrape data/dryscrape.sqlite"
newcon <- dbConnect(SQLite(), link)

start <- Sys.time()

# Read tables
roster <- dbReadTable(newcon, "roster")
team <- dbReadTable(newcon, "team")
goalie <- dbReadTable(newcon, "goalie")
player <- dbReadTable(newcon, "player")
combo <- dbReadTable(newcon, "combo")

## Aggregate Stats

# Roster
sumroster <- group_by(roster, Full.Name, Season, Season.Type) %>% 
  summarise(Team = paste(unique(Team), collapse = "/"), Number = paste(unique(Number), collapse = "/"), Team.Num = paste(unique(Team.Num), collapse = "/"), 
            Position = paste(unique(Position), collapse = "/"), Last.Name = first(Last.Name), First.Name = first(First.Name)) %>%
  data.frame()

# Team
teamgp <- group_by(team, Team, Season, Season.Type) %>% summarise(GP = length(unique(Newcode))) %>% data.frame() %>%
  mutate(Code = paste(Team, Season, Season.Type, sep = ".")) %>% data.frame()

# Group leftover strength states
team$Strength.State[which(team$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4", "0v0") == FALSE)] <- "XvX" # EXCLUDE SHOOTOUT

sumteam <- filter(team, Strength.State != "0v0") %>% group_by(Team, Season, Venue, Strength.State, Score.Cat, Season.Type) %>%
  summarise(GP = teamgp$GP[match(paste(first(Team), first(Season), first(Season.Type), sep = "."), teamgp$Code)], 
            TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
            xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
            AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
            DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
            PENT = sum(PENT), PEND = sum(PEND), DISTF = sum(DISTF), DISTA = sum(DISTA)) %>% data.frame()

# Goalie
goaliegp <- group_by(goalie, Player, Season, Season.Type) %>% summarise(GP = length(unique(Newcode))) %>% data.frame() %>%
  mutate(Code = paste(Player, Season, Season.Type, sep = ".")) %>% data.frame()

# Group leftover strength states
goalie$Strength.State[which(goalie$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4", "0v0") == FALSE)] <- "XvX" # EXCLUDE SHOOTOUT

sumgoalie <- filter(goalie, Strength.State != "0v0") %>% group_by(Player, Season, Venue, Strength.State, Score.Cat, Season.Type) %>%
  summarise(GP = goaliegp$GP[match(paste(first(Player), first(Season), first(Season.Type), sep = "."), goaliegp$Code)], 
            Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), 
            GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
            AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
            DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
            PENT = sum(PENT), PEND = sum(PEND), DISTA = sum(DISTA), G = sum(G), A1 = sum(na.omit(A1)), A2 = sum(na.omit(A2)), SOA = sum(SOA), SOG = sum(SOG),
            iPENT = sum(iPENT), iPEND = sum(na.omit(iPEND))) %>% data.frame()

# Player
playergp <- group_by(player, Player, Season, Season.Type) %>% summarise(GP = length(unique(Newcode))) %>% data.frame() %>%
  mutate(Code = paste(Player, Season, Season.Type, sep = ".")) %>% data.frame()

# Group leftover strength states
player$Strength.State[which(player$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4", "0v0") == FALSE)] <- "XvX" # EXCLUDE SHOOTOUT

sumplayer <- filter(player, Strength.State != "0v0") %>% group_by(Player, Season, Venue, Strength.State, Score.Cat, Season.Type) %>%
  summarise(GP = playergp$GP[match(paste(first(Player), first(Season), first(Season.Type), sep = "."), playergp$Code)], 
            Position = sumroster$Position[match(first(Player), sumroster$Full.Name)],
            Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI), CF = sum(CF), CA = sum(CA), iCF = sum(iCF), FF = sum(FF), FA = sum(FA), iFF = sum(iFF), 
            SF = sum(SF), SA = sum(SA), iSF = sum(iSF), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA), ixG = sum(ixG), 
            ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
            AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
            OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
            FOW = sum(FOW), FOL = sum(FOL), iFOW = sum(iFOW), iFOL = sum(iFOL), HF = sum(HF), HA = sum(HA), iHF = sum(iHF), iHA = sum(iHA), 
            GVA = sum(GVA), TKA = sum(TKA), iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK), PENT = sum(PENT), PEND = sum(PEND), 
            iDIST = sum(iDIST), G = sum(G), A1 = sum(na.omit(A1)), A2 = sum(na.omit(A2)), SOA = sum(SOA), SOG = sum(SOG), iPENT = sum(iPENT), iPEND = sum(na.omit(iPEND)),
            tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
            tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
            tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
            tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
            tOZS = sum(tOZS), tDZS = sum(tDZS), tNZS = sum(tNZS)) %>% data.frame() %>%
  mutate(OCF = tCF - CF, OCA = tCA - CA,
         OFF = tFF - FF, OFA = tFA - FA,
         OSF = tSF - SF, OSA = tSA - SA,
         OGF = tGF - GF, OGA = tGA - GA,
         OxGF = txGF - xGF, OxGA = txGA - xGA, 
         OACF = tACF - ACF, OACA = tACA - ACA,
         OAFF = tAFF - AFF, OAFA = tAFA - AFA,
         OASF = tASF - ASF, OASA = tASA - ASA,
         OAGF = tAGF - AGF, OAGA = tAGA - AGA,
         OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
         OMCF = tMCF - MCF, OMCA = tMCA - MCA,
         OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
         OMSF = tMSF - MSF, OMSA = tMSA - MSA,
         OMGF = tMGF - MGF, OMGA = tMGA - MGA,
         OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
         OOZS = tOZS - OZS, ODZS = tDZS - DZS, ONZS = tNZS - NZS) %>% data.frame() %>%
  select(-c(tCF:tNZS)) %>%
  data.frame()

# Combo

# Group leftover strength states
combo$Strength.State[which(combo$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "3v3", "0v0") == FALSE)] <- "XvX" # EXCLUDE SHOOTOUT

sumline <- filter(combo, grepl("C|L|R", as.character(P3.POS)) == TRUE & grepl("C|L|R", as.character(P2.POS)) == TRUE & grepl("C|L|R", as.character(P1.POS)) == TRUE & Strength.State != "0v0") %>% 
  group_by(Combo.Code, Season, Strength.State, Season.Type, Venue) %>%
  summarise(Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI), P1 = first(P1), P1.POS = first(P1.POS), 
            P2 = first(P2), P2.POS = first(P2.POS), P3 = first(P3), P3.POS = first(P3.POS),
            CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
            SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
            ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
            AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
            OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
            FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA),
            GVA = sum(GVA), TKA = sum(TKA), PENT = sum(PENT), PEND = sum(PEND), 
            P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
            P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
            P3.G = sum(P3.G), P3.A1 = sum(na.omit(P3.A1)), P3.A2 = sum(na.omit(P3.A2)),
            tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
            tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
            tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
            tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
            tOZS = sum(tOZS), tDZS = sum(DZS), tNZS = sum(tNZS)) %>% data.frame() %>%
  mutate(OCF = tCF - CF, OCA = tCA - CA,
         OFF = tFF - FF, OFA = tFA - FA,
         OSF = tSF - SF, OSA = tSA - SA,
         OGF = tGF - GF, OGA = tGA - GA,
         OxGF = txGF - xGF, OxGA = txGA - xGA, 
         OACF = tACF - ACF, OACA = tACA - ACA,
         OAFF = tAFF - AFF, OAFA = tAFA - AFA,
         OASF = tASF - ASF, OASA = tASA - ASA,
         OAGF = tAGF - AGF, OAGA = tAGA - AGA,
         OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
         OMCF = tMCF - MCF, OMCA = tMCA - MCA,
         OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
         OMSF = tMSF - MSF, OMSA = tMSA - MSA,
         OMGF = tMGF - MGF, OMGA = tMGA - MGA,
         OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
         OOZS = tOZS - OZS, ODZS = tDZS - DZS, ONZS = tNZS - NZS) %>%
  select(-c(tCF:tMxGA)) %>%
  data.frame()

sumpair <- filter(combo, as.character(P3) == "X" & grepl("D", as.character(P2.POS)) == TRUE & grepl("D", as.character(P1.POS)) == TRUE & Strength.State != "0v0") %>% 
  group_by(Combo.Code, Season, Strength.State, Season.Type, Venue) %>%
  summarise(Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI), 
            P1 = first(P1), P1.POS = first(P1.POS), P2 = first(P2), P2.POS = first(P2.POS),
            CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
            SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
            ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
            AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
            MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
            OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
            FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA),
            GVA = sum(GVA), TKA = sum(TKA), PENT = sum(PENT), PEND = sum(PEND), 
            P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
            P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
            tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
            tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
            tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
            tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
            tOZS = sum(tOZS), tDZS = sum(DZS), tNZS = sum(tNZS)) %>% data.frame() %>%
  mutate(OCF = tCF - CF, OCA = tCA - CA,
         OFF = tFF - FF, OFA = tFA - FA,
         OSF = tSF - SF, OSA = tSA - SA,
         OGF = tGF - GF, OGA = tGA - GA,
         OxGF = txGF - xGF, OxGA = txGA - xGA, 
         OACF = tACF - ACF, OACA = tACA - ACA,
         OAFF = tAFF - AFF, OAFA = tAFA - AFA,
         OASF = tASF - ASF, OASA = tASA - ASA,
         OAGF = tAGF - AGF, OAGA = tAGA - AGA,
         OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
         OMCF = tMCF - MCF, OMCA = tMCA - MCA,
         OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
         OMSF = tMSF - MSF, OMSA = tMSA - MSA,
         OMGF = tMGF - MGF, OMGA = tMGA - MGA,
         OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
         OOZS = tOZS - OZS, ODZS = tDZS - DZS, ONZS = tNZS - NZS) %>%
  select(-c(tCF:tMxGA)) %>%
  data.frame()
# ASSIST NETWORK

end <- Sys.time()
print(end - start)

################################################################################################################################################################################################################
################################################################################################################################################################################################################
################################################################################################################################################################################################################

## Write to Dropbox

# Roster
# write.csv(sumroster, file = "~/Dropbox/rostertest.csv")
save(sumroster, file = "~/Dropbox/rostertest.Rda")

# Team
# write.csv(sumteam, file = "~/Dropbox/teamtest.csv")
save(sumteam, file = "~/Dropbox/teamtest.Rda")

# Goalie
# write.csv(sumgoalie, file = "~/Dropbox/goalietest.csv")
save(sumgoalie, file = "~/Dropbox/goalietest.Rda")

# Player
# write.csv(sumplayer, file = "~/Dropbox/playertest.csv")
save(sumplayer, file = "~/Dropbox/playertest.Rda")

# Combo
# write.csv(sumcombo, file = "~/Dropbox/combotest.csv")
save(sumpair, file = "~/Dropbox/pairtest.Rda")
save(sumline, file = "~/Dropbox/linetest.Rda")

################################################################################################################################################################################################################
################################################################################################################################################################################################################
################################################################################################################################################################################################################
