library(tidyverse)
library(yaml)
library(janitor)
library(scales)
library(pander)

configs <- read_yaml("IdealPointsCompare/configs.yml")

args = commandArgs(trailingOnly = T)
args <- as.list(args)
names(args) <- c("test","sens")
sens=(args$sens==1)
test=(args$test==1)

CONGS = 93:113
if (test){CONGS=93:94}

if (!sens){
  FLDRS = c("joint","stage2_50","stage2cut")
  LABELS = c("Full","Two-stage","Cut")
  out_folder = "."  
} else {
  FLDRS <- c("stage2_25", "stage2_50","stage2_75")
  LABELS <- c("PP>0.25","PP>0.5","PP>0.75")
  out_folder <- "sensitivity"  
}

res_path <- file.path("IdealPointsCompare","Results")
out_path <- file.path("Figures",out_folder)
dir.create(out_path, showWarnings = F)

SHAPES <- c(22,1,8)[1:length(LABELS)]
LINETYPES <- c("solid","dashed","dotted")[1:length(LABELS)]

read_one <- function(cong_num, fldr, 
                     suffix="10000_5000_10",
                     steps=200){
  
  suffix <- case_when(test~"10_10_1",
                      fldr!="joint"~suffix,
                      cong_num==105~"20000_20000_25", 
                      T~"20000_15000_20")
  steps <- ifelse(test,5,steps)
  
  cong_str = paste0("H", str_pad(cong_num, 3, pad=0))
 
  # get res stage
  if (grepl("joint",fldr)){
    path = file.path(res_path,fldr,"Combined",
                      paste0(cong_str, "_", suffix, "_combined.RDS"))
    res1 <- readRDS(path)
    res <- res1$stage2
  }
  else {
    if(grepl("cut", fldr)){
      paths <- list.files(file.path(res_path,fldr, 
                                    paste0(cong_str, "_cut_", steps)),
                          full.names = T)
    } else {
      paths <- list.files(file.path(res_path,fldr, 
                                     paste0(cong_str, "_", suffix)),
                           full.names = T)
    }
    res <- map(paths, readRDS)
    res$covariates <- res[[1]]$covariates
  }
  
  dat <- readRDS(file.path("IdealPointsCompare","Data",
                           paste0("inputs_", cong_str, ".RDS")))
  covs <- tibble(p_R=dat$covariates$p_R,
                 recentArrivalPrcnt=dat$covariates$recentArrivalPrcnt,
                 dwnom1=dat$covariates$dwnom1)
  binary <- map_dbl(dat$covariates[,configs$covariates],
                    ~all(.x %in% 0:1))
  cov_sd <- map_dbl(dat$covariates[,configs$covariates], sd)
  scale <- ifelse(binary, 1, cov_sd)
  
  tibble(stage2=list(res),
         covs = list(covs),
         party = dat$cong_info$partyControl,
         cov_scale=list(scale))
}


###########################################
res_all <- 
  cross_df(list(cong=CONGS, mod_num=1:length(FLDRS))) %>%
  mutate(res=map2(cong, mod_num, 
                  ~read_one(.x,FLDRS[.y], 
                            steps=ifelse(.y==1, 5000, 200))),
         label = map(mod_num, ~LABELS[.x])) %>%
  unnest(res,label) %>%
  mutate(label=fct_relevel(label, LABELS))

get_model_one <- function(res2, cov_scale){
  covariates <- res2$covariates
  res2 <- res2[names(res2)!="covariates"]
  eps <- map_dfr(res2, ~tibble(.$eps))[[1]]
  eta <- map_dfr(res2, ~tibble(.$eta))[[1]]
  if (ncol(eps)==length(covariates)){
    eps <- cbind(1,eps)
  }
  
  tibble(cov=c("int",covariates),
         eps=colMeans(eps),
         scale = c(1, cov_scale),
         eta_L95=apply(eta,2,quantile,.025),
         eta_U95=apply(eta,2,quantile,.975),
         eta=apply(eta,2,quantile,.5)) %>%
    filter(cov!="int")
}

get_model_size <- function(res2){
  size <- map_dfr(res2[names(res2)!="covariates"], ~tibble(.$eps)) %>%
    rowSums()
  if(ncol(res2[[1]]$eps)>length(res2$covariates)){
    size=size-1
  }
  tibble(size=size) %>%
    summarise(median=median(size), 
              L95 = quantile(size,.025),
              U95 = quantile(size, .975))
}

model_all <- select(res_all, cong, label, mod_num, 
                    stage2, cov_scale) %>%
  mutate(model = map2(stage2, cov_scale, get_model_one)) %>%
  select(-stage2, -cov_scale) %>%
  unnest(model) %>%
  group_by(cov, label) %>%
  mutate(sign = case_when(eps<.5~"",
                          eta<0~"-", eta>0~"+", eta==0~"o"),
         n_selected = sum(eps>.5),
         avg_post = mean(eps),
         max_post = max(eps)) %>%
  ungroup()

num_cov <- model_all %>%
  group_by(cong,label) %>%
  summarise(num_cov=sum(eps>.5))

size_all <- mutate(res_all,
                   size=map(stage2, get_model_size)) %>%
  select(cong, label, mod_num, size) %>%
  unnest(size) %>%
  left_join(num_cov, by=c("label","cong"))

party_info <- readRDS(file.path("IdealPointsCompare","Data","party_info.RDS"))

###########################################
#              Model size                 #
###########################################

# table
select(size_all, cong, label, num_cov) %>%
  pivot_wider(names_from=cong, values_from=num_cov) %>%
  pandoc.table()

# plot
size_all %>%
  mutate(label=fct_relevel(label,LABELS)) %>%
  ggplot(aes(x=cong, y=num_cov, #ymax=L95, ymin=U95,
             shape=label, color=label)) +
  geom_rect(data=party_info, ymin=0, ymax=21,
            aes(xmin=start-.5, xmax=end+.5),
            inherit.aes = F, alpha=.2) +
  #geom_linerange(position=position_dodge(.8)) +
  geom_point() +
  scale_linetype_manual(
    values=c("dashed","solid","dotted")) +
  scale_shape_manual("",values=SHAPES) +
  scale_color_discrete("") +
  xlab("Congress") +
  ylab("Median model size") +
  theme_bw()

ggsave(file.path(out_path,"size.jpeg"), 
       height = 2.5, width=7)

###########################################
#            SELECTED MODELS              #
###########################################

cov_cat <- tibble(type=names(configs$covariate_cats),
                  cov=configs$covariate_cats) %>%
  unnest(cov)

ORDER <- model_all %>%
  filter(label=="Full") %>%
  full_join(cov_cat, by="cov") %>%
  arrange(type, n_selected) %>% pull(cov) %>% unique()

make_colorplot <- function(which_labels){
  dat_colorplot <- model_all %>%
    filter(label %in% which_labels) %>%
    select(label, cong,cov,eps,n_selected,sign) %>%
    full_join(cov_cat, by="cov") %>%
    mutate(cov=fct_relevel(cov,ORDER))
  
  p <- dat_colorplot %>% 
    rename(PIP=eps) %>%
    ggplot(aes(x=cong, y=cov, fill=PIP)) +
    geom_tile() +
    #geom_point(data=filter(dat_colorplot,eps>.5)) +
    geom_text(aes(label=sign), size=5) +
    geom_hline(yintercept=nrow(cov_cat)-cumsum(count(cov_cat,type)$n[3:1])+0.5,
               color="white") +
    scale_fill_gradientn(colors=c("blue","white","red"), 
                         breaks=c(0,.5,1)) +
    xlab("House") + ylab("") +
    theme_minimal()
  
  if (length(which_labels)>1){
    p <- p + facet_grid(.~label) + coord_equal()
  }
  
  return(p)
  
  #ggsave(file.path(out_path,
  #                 paste0("red_blue_",which_label,".jpeg")), 
  #       height = 4, width=6)
}

# make combined plot
make_colorplot(LABELS)
ggsave(file.path(out_path, "red_blue.jpeg"),
       height = 3, width=10)

# number of covariates in each combo of models
if (!sens & !test){
combos <- select(model_all, cong, label, cov, eps) %>%
  mutate(eps=eps>.5) %>%
  pivot_wider(names_from=label, values_from = eps) %>%
  clean_names() %>%
  mutate(comb=paste0(ifelse(full,"f",""),
                ifelse(two_stage,"t",""),
                ifelse(cut,"c",""))) %>%
  filter(comb!="") %>%
  count(cong, comb) %>%
  pivot_wider(names_from=comb, values_from=n) %>%
  select(cong, ftc, ft, f, t)

combos %>%
  pivot_longer(-cong, names_to="combo", values_to="n") %>%
  mutate(n=ifelse(is.na(n), 0, n),
         combo=fct_relevel(combo, names(combos)[2:5])) %>%
  ggplot(aes(x=cong, y=n, color=combo, shape=combo)) +
  geom_point() +
  scale_shape_manual("",values=c(SHAPES,4)) +
  scale_color_discrete("") +
  xlab("Congress") +
  ylab("Number of covariates") +
  theme_bw()

ggsave(file.path(out_path,"size_combos.jpeg"), 
       height = 2.5, width=7)

## find exceptions to nestedness
except <- select(model_all, cong, label, cov, eps) %>%
  mutate(eps=eps>.5) %>%
  filter(eps) %>%
  pivot_wider(names_from=label, values_from = eps) %>%
  clean_names() %>%
  filter(is.na(full), !is.na(two_stage))

model_all %>%
  semi_join(except, by=c("cong","cov")) %>%
  arrange(cong,cov) %>%
  select(label,cong,cov,eps) %>%
  pivot_wider(names_from=label, values_from=eps)

}

###########################################
#              COEFFICIENTS               #
###########################################

plot_cov <- function(which_cov,title="", c=1,
                     lab_pow=10){
  
  plot_dat <- model_all %>%
    filter(cov==which_cov) %>%
    mutate_at(vars(contains("eta")), ~exp(.x*(c/scale))) %>%
    mutate(signif=ifelse(eps>=.5, "PIP>=0.5", "PIP<0.5"),
           signif=fct_rev(signif))
  
  print(paste0("Showing ORs for an increase of (median) ", 
               round(c/median(plot_dat$scale),2),
               " standard deviations"))
  
  shading <- 
    plot_dat %>% 
    #group_by(type) %>%
    summarize(min=min(c(eta,eta_L95)), max=max(eta,eta_U95)) %>%
    mutate(party_info = list(party_info)) %>%
    unnest(party_info)
  
  lrange = round(log(c(min(shading$min), max(shading$max)),
                     lab_pow))
  lrange =seq(lrange[1],lrange[2])
  labels = ifelse(lrange>=0,
                  round(lab_pow^lrange),lab_pow^lrange)
  
  g <- plot_dat %>% 
    ggplot(aes(x=eta, y=cong, shape=label,
               xmin=eta_L95,xmax=eta_U95,
               color=signif)) +
    geom_rect(data=shading, inherit.aes = F, alpha=.2,
              aes(xmin=min, xmax=max, ymin=start-.5, ymax=end+.5)) +
    geom_vline(xintercept=1, linetype="dotted") +
    geom_pointrange(position=position_dodge(.8),
                    aes(fill=signif), size=.5) +
    scale_x_continuous(trans = log_trans(),
                       labels = labels,
                       breaks = lab_pow^lrange) +
    scale_shape_manual("",values=SHAPES) +
    scale_color_manual("", values=c("black","gray")) +
    guides(fill=F) +
    coord_flip() +
    xlab("Increase in odds ratio\n(log scale)") + ylab("House") +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position = "none")
  plot(g)
  
  num_summy <- select(plot_dat, label,cong, eps, OR=eta) %>%
    filter(eps>.5)%>%
    mutate(eta=log(OR), OR_inv=1/OR) %>%
    #arrange(OR)   %>%
    select(cong, label, OR) %>%
    pivot_wider(names_from = label, values_from = OR) %>%
    arrange(cong)
  
  ggsave(file.path(out_path, paste0(which_cov,".jpeg")), 
         height = 2.5, width=7) 
  return(num_summy)
}

## top selected covariates
select(model_all, label, cov, n_selected) %>%
  distinct() %>%
  arrange(-n_selected) %>%
  filter(n_selected>3) %>%
  arrange(cov)

#paste0("Belongs to majority party",
#        "\nOR for majority vs. minority")
plot_cov("belto.partyControl", c=1) 

#paste0("Constituancy (Democratic legislators only)",
#"\nOR for increase of 5 percent")
plot_cov("p_R_D", c=.05, lab_pow = 2) 

#paste0("Constituancy (Republican legislators only)",
#"\nOR for increase of 5 percent")
plot_cov("p_R_R", c=.05, lab_pow = 2)


###########################################
#                 MODELS                  #
###########################################

get_mod_posterior <- function(res2, 
                              thresh=c(.8,.95), num=c(10)){
  eps <- map_dfr(res2[names(res2)!="covariates"], ~as.tibble(.$eps))
  if(all(eps[,1])){eps <- eps[,2:ncol(eps)]}
  
  cum_probs <- mutate_all(eps, as.numeric) %>%    
    unite("bin",sep="") %>%   
    count(bin) %>%
    mutate(p=n/sum(n)) %>%
    arrange(desc(p)) %>%
    mutate(p_cumm = cumsum(p),
           p_cumm_lag=lag(p_cumm),
           row=1:n()) 
  
  # number of models until a certain cummulative prob
  num_thresh <- map_dbl(thresh, 
                        ~pull(filter(cum_probs, p_cumm>=.x, 
                                     p_cumm_lag<.x),row))
  # cummulative prob for a certain number of models
  prob_num <- filter(cum_probs, row %in% num) %>% pull(p_cumm)
  
  tibble(thresh = list(tibble(thresh=thresh, num_thresh=num_thresh)),
         num = list(tibble(num=num, prob_num=prob_num)))
}

if (!sens & !test){
mod_post <- mutate(res_all,
                   post=map(stage2, get_mod_posterior,
                            num=seq(10,100,10))) %>% 
  select(cong, label, post) %>%
  unnest(post)

# cumulative prob for top n models
NUM <- 100
select(mod_post, cong, label, num) %>%
  unnest(num) %>%
  filter(num==NUM) %>%
  ggplot(aes(x=cong, y=prob_num, 
             color=label, shape=label)) +
  geom_point() +
  xlab("House") +
  ylab(paste("Cummulative probability\nfor top",NUM,"models")) +
  #scale_color_manual("", values=COLORS) +
  scale_color_discrete("") +
  scale_shape_manual("", values=SHAPES) +
  theme_bw()
  
ggsave(file.path(out_path,
                 paste0("xi_cprob,",NUM,".jpeg")), 
       height = 2.5, width=7)
}

