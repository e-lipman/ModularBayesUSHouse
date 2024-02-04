library(tidyverse)
library(yaml)

configs <- read_yaml("IdealPointsCompare/configs.yml")

args = commandArgs(trailingOnly = T)
args <- as.list(args)
names(args) <- c("test")
test=(args$test==1)

CONGS = 93:113
if (test){CONGS=93:94}
FLDRS = c("joint","stage1")
LABELS = c("Full","Working")

res_path <- file.path("IdealPointsCompare","Results")
out_path <- file.path("Figures")
dir.create(out_path, showWarnings = F)

read_one <- function(cong_num, fldr, 
                     suffix="10000_5000_10"){
  
  suffix <- case_when(test~"10_10_1",
                      fldr!="joint"~suffix,
                      cong_num==105~"20000_20000_25", 
                      T~"20000_15000_20")
  
  cong_str = paste0("H", str_pad(cong_num, 3, pad=0))
  path = file.path(res_path,fldr,"Combined",
                   paste0(cong_str, "_", suffix, "_combined.RDS"))
  
  read_rds(path)$summy
}

# read results

res_all <- 
  cross_df(list(cong=CONGS, mod_num=1:length(FLDRS))) %>%
  mutate(res=map2(cong, mod_num, ~read_one(.x,FLDRS[.y])),
         label = map(mod_num, ~LABELS[.x])) %>%
  unnest(res,label) %>%
  mutate(label=fct_relevel(label, LABELS))
read_one(93, FLDRS[1])

party_info <- readRDS(file.path("IdealPointsCompare","Data","party_info.RDS"))

# plot ASF
res_all %>% 
  filter(type=="All" | is.na(type)) %>%
  ggplot(aes(x=cong, y=ASF, ymin=ASF_L95, ymax=ASF_U95, linetype=label)) +
  geom_linerange(position = position_dodge(.5)) +
  geom_point(position = position_dodge(.5), size=1) +
  geom_rect(data=party_info, ymin=0, ymax=1,
            aes(xmin=start-.5, xmax=end+.5),
            inherit.aes = F, alpha=.2) +
  xlab("House") + ylab("Bridging frequency") +
  scale_linetype_discrete("") +
  theme_bw()

ggsave(file.path(out_path,"ASF.jpeg"), height = 2.5, width=7)
