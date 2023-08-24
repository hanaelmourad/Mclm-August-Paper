# Load packages ----
library(here)
library(tidyverse)
library(mclm)
library(kableExtra)




#Load data
path_to_corpus <- here("anthrocorpus")
path_to_reference_corpus <-here("reference_corpus_random")

# List file names
anthro_fnames <- get_fnames(path_to_corpus)
reference_fnames <- get_fnames(path_to_reference_corpus)
print(anthro_fnames)
#frequency list for entire corpus

flist <-freqlist(anthro_fnames)
print(flist, n=20)

#exploring corpus data

n_tokens(flist)
n_types(flist)

stop_list <- read_types("stop_list2_names.txt")
corpus_flist <-flist %>% drop_types(stop_list)
print(corpus_flist, n=50)


# Stop list, to be used to filter out words of lesser interest
stop_list1 <- read_types(here::here("stop_list1.txt"))
stop_list2 <- read_types(here::here("stop_list2_names.txt"))

# Functions
create_frequency_list <- function(fname){ #expects file name
  frequencies <- fname %>% freqlist() %>% drop_types(stop_list2)
}

create_surf_cooc <- function(fname,re_exp){ #expects file name + regular expression e.g. "(?xi) ^jimmy$"
  s_cooc <- fname %>% surf_cooc(re_exp, w_left = 3, w_right = 3, re_boundary=TRUE)
  s_cooc$target_freqlist <- s_cooc$target_freqlist %>% drop_types(stop_list2)
  s_cooc$ref_freqlist <- s_cooc$ref_freqlist %>% drop_types(stop_list2)
  s_cooc
}

display_flist_as_tibble <- function(flist,number) {
  flist %>% as_tibble() %>% arrange((rank)) %>% select(rank, type, abs_freq, nrm_freq) %>% head(number) %>% kbl() %>% kable_paper(font_size = 16)
}



### exploratory flist, top 50 most frequent words in each novel, sorted by rank, original_rank discarded. ###


flist_atwood %>% as_tibble() %>% arrange((rank)) %>% select(rank, type, abs_freq, nrm_freq) %>% head(50) %>% kbl() %>% kable_paper(font_size = 16)

flist_itaranta %>% as_tibble() %>% arrange((rank)) %>% select(rank, type, abs_freq, nrm_freq) %>% head(50) %>% kbl() %>% kable_paper(font_size = 16)

flist_galchen %>% as_tibble() %>% arrange((rank)) %>% select(rank, type, abs_freq, nrm_freq) %>% head(50) %>% kbl() %>% kable_paper(font_size = 16)

flist_ma %>% as_tibble() %>% arrange((rank)) %>% select(rank, type, abs_freq, nrm_freq) %>% head(50) %>% kbl() %>% kable_paper(font_size = 16)

flist_mandel %>% as_tibble() %>% arrange((rank)) %>% select(rank, type, abs_freq, nrm_freq) %>% head(50) %>% kbl() %>% kable_paper(font_size = 16)

flist_mccarthy %>% as_tibble() %>% arrange((rank)) %>% select(rank, type, abs_freq, nrm_freq) %>% head(50) %>% kbl() %>% kable_paper(font_size = 16)

flist_vandermeer %>% as_tibble() %>% arrange((rank)) %>% select(rank, type, abs_freq, nrm_freq) %>% head(50) %>% kbl() %>% kable_paper(font_size = 16)



#####cooc scores for words selected from the top 50 most frequent words####




#####MCCARTHY ####

display_flist_as_tibble(flist_mccarthy,50)

coocs_mccarthy_road <- create_surf_cooc(mccarthy_fname,"(?xi) ^road$")
assoc_scores_mccarthy_road <- assoc_scores(coocs_mccarthy_road) 
assoc_scores_mccarthy_road %>% arrange(desc(G_signed))
#road has more than normal amount of strong associations, as is thematic to the book. e.g. side, crossed, stopped, stood, ran, south, ate, tracks, country.

head(assoc_scores_mccarthy_road)

assoc_scores_mccarthy_road %>% arrange(desc(PMI)) %>% 
  select(a, exp_a, PMI) %>% head(10) %>% 
  kbl() %>% kable_paper(font_size = 22)




####PLOT

top_scores_df <- as_tibble(assoc_scores_mccarthy_road)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 1)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_mccarthy_water <- create_surf_cooc(mccarthy_fname,"(?xi) ^water$")
assoc_scores_mccarthy_water <- assoc_scores(coocs_mccarthy_water) 
assoc_scores_mccarthy_water %>% arrange(desc(G_signed))
#water scores 10th on the top frequencies list - it also has a lot of strong associations, and while they are normal mostly (drink, poured, slow, glass) it could indicate lack of water, as it is so prominently mentioned, and verbs like "heated" or "plastic" or "fresh" or "black/gray" could indicate there were issues with water scarcity or purity.

top_scores_df <- as_tibble(assoc_scores_mccarthy_water)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_mccarthy_fire <- create_surf_cooc(mccarthy_fname,"(?xi) ^fire$")
assoc_scores_mccarthy_fire <- assoc_scores(coocs_mccarthy_fire) 
assoc_scores_mccarthy_fire %>% arrange(desc(G_signed))
#built, make, ate, sat, walked,... a lot of activity near fire, indicating perhaps traveling or lack of electricity

top_scores_df <- as_tibble(assoc_scores_mccarthy_fire)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))





coocs_mccarthy_cart <- create_surf_cooc(mccarthy_fname,"(?xi) ^cart$")
assoc_scores_mccarthy_cart <- assoc_scores(coocs_mccarthy_cart) 
assoc_scores_mccarthy_cart %>% arrange(desc(G_signed))
# a lot of strong associations, e.g. pushed, left, wheeled, handle, forward, pulled, walked - indicating lots of movement with items, does it mean there were no cars?

top_scores_df <- as_tibble(assoc_scores_mccarthy_cart)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))



coocs_mccarthy_hand <- create_surf_cooc(mccarthy_fname,"(?xi) ^hand$")
assoc_scores_mccarthy_hand <- assoc_scores(coocs_mccarthy_hand) 
assoc_scores_mccarthy_hand %>% arrange(desc(G_signed))
#held is 1st, but second is pistol. seems like there is indication if reliance on fire-arms, perhaps indicating a more of a lawlessness to the world

top_scores_df <- as_tibble(assoc_scores_mccarthy_hand)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_mccarthy_put <- create_surf_cooc(mccarthy_fname,"(?xi) ^put$")
assoc_scores_mccarthy_put <- assoc_scores(coocs_mccarthy_put) 
assoc_scores_mccarthy_put %>% arrange(desc(G_signed))
#pistol, wood, and cart are in the top 10 associations - indicating, perhaps, that weapos, fires, and the movement of goods were pivotal in this novel, all indicating a fallen world, due to climate?

top_scores_df <- as_tibble(assoc_scores_mccarthy_put)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_mccarthy_cold <- create_surf_cooc(mccarthy_fname,"(?xi) ^cold$")
assoc_scores_mccarthy_cold <- assoc_scores(coocs_mccarthy_cold) 
assoc_scores_mccarthy_cold %>% arrange(desc(G_signed))
#dark, wet, rain, world, night, long - all indicating - with the fact that "cold" was 13th freq list - that the world is somewhat unpleasant and not too hospitable.

top_scores_df <- as_tibble(assoc_scores_mccarthy_cold)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_mccarthy_blankets <- create_surf_cooc(mccarthy_fname,"(?xi) ^blankets$")
assoc_scores_mccarthy_blankets <- assoc_scores(coocs_mccarthy_blankets) 
assoc_scores_mccarthy_blankets %>% arrange(desc(G_signed))
#blankets figuring as the 15th most frequent word, taken together with "cold", and importance of "fire", would indeed indicate that keeping warm was a concern, top associations: wrapped, coats, piled, pulled, tarp

coocs_mccarthy_set <- create_surf_cooc(mccarthy_fname,"(?xi) ^set$")
assoc_scores_mccarthy_set <- assoc_scores(coocs_mccarthy_set) 
assoc_scores_mccarthy_set %>% arrange(desc(G_signed))
#tarp, cart, fire - these are top 3 associations, again, circling back to importance of warmth, movement of items, and protection from nature (tarp)

coocs_mccarthy_thought <- create_surf_cooc(mccarthy_fname,"(?xi) ^thought$")
assoc_scores_mccarthy_thought <- assoc_scores(coocs_mccarthy_thought) 
assoc_scores_mccarthy_thought %>% arrange(desc(G_signed))
#dark is the only strong association. probably from dark thought. how often did the character had those?

coocs_mccarthy_walked <- create_surf_cooc(mccarthy_fname,"(?xi) ^walked$")
assoc_scores_mccarthy_walked <- assoc_scores(coocs_mccarthy_walked) 
assoc_scores_mccarthy_walked %>% arrange(desc(G_signed))
#walked is 20th on the list. indicating movement is paramount. strong assocaitions: #streets, back, beach, cart, fire. 

#beyond the top 20: dead(21), black(27), plastic(35)


top_scores_df <- as_tibble(coocs_mccarthy_walked)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 1)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))

coocs_mccarthy_dead <- create_surf_cooc(mccarthy_fname,"(?xi) ^dead$")
assoc_scores_mccarthy_dead <- assoc_scores(coocs_mccarthy_dead) 
assoc_scores_mccarthy_dead %>% arrange(desc(G_signed))
#trees, grass, limbs, wind, fire

coocs_mccarthy_black <- create_surf_cooc(mccarthy_fname,"(?xi) ^black$")
assoc_scores_mccarthy_black <- assoc_scores(coocs_mccarthy_black) 
assoc_scores_mccarthy_black %>% arrange(desc(G_signed))
#trees, dead, water, road

coocs_mccarthy_plastic <- create_surf_cooc(mccarthy_fname,"(?xi) ^plastic$")
assoc_scores_mccarthy_plastic <- assoc_scores(coocs_mccarthy_plastic) 
assoc_scores_mccarthy_plastic %>% arrange(desc(G_signed))
#Interestingly enough, plastic doesn't figure as the evil in this book, it's widely used to contain water

###MA###


display_flist_as_tibble(flist_ma,50)


coocs_ma_water <- create_surf_cooc(ma_fname,"(?xi) ^masks$")
assoc_scores_ma_water <- assoc_scores(coocs_ma_water) 
assoc_scores_ma_water %>% arrange(desc(G_signed))
#contracted, victims, system, spread, eventually

coocs_ma_water <- create_surf_cooc(ma_fname,"(?xi) ^masks$")
assoc_scores_ma_water <- assoc_scores(coocs_ma_water) 
assoc_scores_ma_water %>% arrange(desc(G_signed)) %>% as_tibble() %>% select(type, a, b, G_signed, exp_a, dir) %>% kbl() %>% kable_paper(font_size = 22)



top_scores_df <- as_tibble(assoc_scores_ma_water)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))

coocs_ma_fever <- create_surf_cooc(ma_fname,"(?xi) ^fever$")
assoc_scores_ma_fever <- assoc_scores(coocs_ma_fever) 
assoc_scores_ma_fever %>% arrange(desc(G_signed))
#contracted, victims, system, spread, eventually


top_scores_df <- as_tibble(assoc_scores_ma_fever)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))

coocs_ma_city <- create_surf_cooc(ma_fname,"(?xi) ^city$")
assoc_scores_ma_city<- assoc_scores(coocs_ma_city) 
assoc_scores_ma_city %>% arrange(desc(G_signed))
#living, windows

top_scores_df <- as_tibble(assoc_scores_ma_city)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))

#exploring coocurences with "face"

coocs_ma_face <- create_surf_cooc(ma_fname,"(?xi) ^face$")
assoc_scores_ma_face <- assoc_scores(coocs_ma_face) 
assoc_scores_ma_face %>% arrange(desc(G_signed))
#masks, put



top_scores_df <- as_tibble(assoc_scores_ma_face)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


#######itaranta#####


display_flist_as_tibble(flist_itaranta,50)

coocs_itaranta_water <- create_surf_cooc(itaranta_fname,"(?xi) ^water$")
assoc_scores_itaranta_water <- assoc_scores(coocs_itaranta_water) 
assoc_scores_itaranta_water %>% arrange(desc(G_signed))
#quota, crime, illegal, guards, death

top_scores_df <- as_tibble(assoc_scores_itaranta_water)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_itaranta_plastic <- create_surf_cooc(itaranta_fname,"(?xi) ^plastic$")
assoc_scores_itaranta_plastic <- assoc_scores(coocs_itaranta_plastic) 
assoc_scores_itaranta_plastic %>% arrange(desc(G_signed))
#grave, junk, metal

top_scores_df <- as_tibble(assoc_scores_itaranta_plastic)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_itaranta_insect <- create_surf_cooc(itaranta_fname,"(?xi) ^insect$")
assoc_scores_itaranta_insect<- assoc_scores(coocs_itaranta_insect) 
assoc_scores_itaranta_insect %>% arrange(desc(G_signed))
#hood, hoods, removed, net


top_scores_df <- as_tibble(assoc_scores_itaranta_insect)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))

coocs_itaranta_test <- create_surf_cooc(itaranta_fname,"(?xi) ^world$")
assoc_scores_itaranta_test <- assoc_scores(coocs_itaranta_test) 
assoc_scores_itaranta_test %>% arrange(desc(G_signed))
#winters

coocs_itaranta_test <- create_surf_cooc(itaranta_fname,"(?xi) ^hidden$")
assoc_scores_itaranta_test <- assoc_scores(coocs_itaranta_test) 
assoc_scores_itaranta_test %>% arrange(desc(G_signed))
#waters, skins


#####################MANDEL#######################

display_flist_as_tibble(flist_mandel,50)

coocs_mandel_water<- create_surf_cooc(mandel_fname,"(?xi) ^water$")
assoc_scores_mandel_water <- assoc_scores(coocs_mandel_water) 
assoc_scores_mandel_water %>% arrange(desc(G_signed))
#ago, twenty, ten, fifteen, collapse, couple, past, back

coocs_mandel_years<- create_surf_cooc(mandel_fname,"(?xi) ^years$")
assoc_scores_mandel_years <- assoc_scores(coocs_mandel_years) 
assoc_scores_mandel_years %>% arrange(desc(G_signed))
#ago, twenty, ten, fifteen, collapse, couple, past, back

top_scores_df <- as_tibble(assoc_scores_mandel_years)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_mandel_road<- create_surf_cooc(mandel_fname,"(?xi) ^road$")
assoc_scores_mandel_road <- assoc_scores(coocs_mandel_road) 
assoc_scores_mandel_road %>% arrange(desc(G_signed))
#airport, ahead, crossed, walking, side, 

top_scores_df <- as_tibble(assoc_scores_mandel_road)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_mandel_world <- create_surf_cooc(mandel_fname,"(?xi) ^world$")
assoc_scores_mandel_world <- assoc_scores(coocs_mandel_world) 
assoc_scores_mandel_world %>% arrange(desc(G_signed))
#ended, end, changed, remembered

top_scores_df <- as_tibble(assoc_scores_mandel_world)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_mandel_city <- create_surf_cooc(mandel_fname,"(?xi) ^city$")
assoc_scores_mandel_city <- assoc_scores(coocs_mandel_city) 
assoc_scores_mandel_city %>% arrange(desc(G_signed))
#traverse, airport, leave

top_scores_df <- as_tibble(assoc_scores_mandel_city)
theme_set(theme_minimal(base_size = 15))
g <- top_scores_df %>% ggplot(aes(x=PMI, y = G_signed)) + labs(x="PMI", y="Signed G")
high_G_signed <- top_scores_df %>% filter(G_signed > 3)
g + geom_point(aes(size=a)) + ggrepel::geom_text_repel(data = high_G_signed, aes(label = type))


coocs_mandel_air <- create_surf_cooc(mandel_fname,"(?xi) ^air$")
assoc_scores_mandel_air <- assoc_scores(coocs_mandel_air) 
assoc_scores_mandel_air %>% arrange(desc(G_signed))
#travel, jet

coocs_mandel_walked<- create_surf_cooc(mandel_fname,"(?xi) ^walked$")
assoc_scores_mandel_walked <- assoc_scores(coocs_mandel_walked) 
assoc_scores_mandel_walked %>% arrange(desc(G_signed))



###### Keyword analysis - target corpus vs reference corpus (to find the more unique words of the anthro corpus) ######


# create an fname/flist target / reference, also drop stop list, names, and highly book specific words from frequency lists
keyword_analysis_fnames_target <- anthro_fnames
keyword_analysis_fnames_reference <- reference_fnames
keyword_analysis_flist_target <- keyword_analysis_fnames_target %>% freqlist() %>% drop_types(stop_list2)
keyword_analysis_flist_reference <- keyword_analysis_fnames_reference %>% freqlist() %>% drop_types(stop_list2)

# use tibble with a spanning header to display the target frequency list's top 100 results
keyword_analysis_flist_target %>%  as_tibble() %>% head(100) %>% kbl(col.names = c("Rank", "orig_rank", "Type", "Absolute", "Relative")) %>% kable_minimal(full_width = FALSE) %>% scroll_box(height = "400px")


# assoc scores for the keywords analysis; Haldane is true by default, gives 0.5 to values instead of 0, so not to divide by zero. small_pos is a choose your own adventure type of thing.
score_keywords <- assoc_scores(keyword_analysis_flist_target, keyword_analysis_flist_reference, haldane = FALSE, small_pos = 0.001)

# print the scores, using PMI as the ranking variable
print(score_keywords, sort_order = "PMI")

# print the scores, using G_signed as the ranking variable
print(score_keywords, sort_order = "G_signed")

# create a list of scores, in which PMI >= 1.1 and G_signed >=4
top_scores_keywords <- score_keywords %>% filter(PMI >= 1.1 & G_signed >= 4)

# transform the top_scores_keywords into a tibble
top_scored_keywords_filtered <- top_scores_keywords %>% as_tibble()

# from the tibble, order by "a", select only type, top 100, as a tibble
a = top_scored_keywords_filtered %>% arrange(desc(a)) %>% select(type, a, PMI, G_signed,p_fisher_1) %>% head(100) %>% kbl() %>% kable_paper(font_size = 22)
b = top_scored_keywords_filtered %>% arrange((p_fisher_1)) %>% select(type, a, PMI, G_signed,p_fisher_1) %>% head(100) %>% kbl(head,caption = "My data") %>% kable_paper(font_size = 22)
c = top_scored_keywords_filtered %>% arrange((p_fisher_1)) %>% select(type, a, PMI, G_signed,p_fisher_1) %>% head(100) %>% kbl() %>% add_header_above(data.frame("Title with an AMAAZZING font", 5), monospace = TRUE) %>% kable_paper(font_size = 22)

print(c,n=50)
print(a,n=50)
print(b,n=50)

