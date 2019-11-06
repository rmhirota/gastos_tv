library(dplyr)
library(stringr)
library(readr)
library(deflateBR)

dados1 <- data.table::fread("dados/Exportacao_Pagamentos_06-11-2019 (1).csv", encoding = "Latin-1",
                            sep = ";", dec = ",") %>% janitor::clean_names()
dados2 <- data.table::fread("dados/Exportacao_Pagamentos_06-11-2019 (2).csv", encoding = "Latin-1",
                            sep = ";", dec = ",") %>% janitor::clean_names()
dados3 <- data.table::fread("dados/Exportacao_Pagamentos_06-11-2019 (3).csv", encoding = "Latin-1",
                            sep = ";", dec = ",") %>% janitor::clean_names()
dados4 <- data.table::fread("dados/Exportacao_Pagamentos_06-11-2019 (4).csv", encoding = "Latin-1",
                            sep = ";", dec = ",") %>% janitor::clean_names()
dados5 <- data.table::fread("dados/Exportacao_Pagamentos_06-11-2019 (5).csv", encoding = "Latin-1",
                            sep = ";", dec = ",") %>% janitor::clean_names()
dados6 <- data.table::fread("dados/Exportacao_Pagamentos_06-11-2019.csv", encoding = "Latin-1",
                            sep = ";", dec = ",") %>% janitor::clean_names()

dados <- rbind(dados1,dados2,dados3,dados4,dados5,dados6)
rm(dados1,dados2,dados3,dados4,dados5,dados6)

dados %>% glimpse()
dados$meios %>% unique()

# Descartando a categoria "Internet,Televisão
dados <- dados %>% filter(meios != "Internet,Televisão")

# Algumas observações não possuem grupo_editora,
# vamos ver quais são os nomes fantasia de cada grupo_editora
# e considerá-los para a categorização de quem tem NA
depara <- dados %>%
  filter(!is.na(grupo_editora), grupo_editora != "") %>%
  group_by(grupo_editora, nome_fantasia_do_veiculo) %>%
  summarise()

com_cat <- dados %>% filter(!is.na(grupo_editora), grupo_editora != "")
sem_cat <- dados %>% filter(is.na(grupo_editora) | grupo_editora == "")

sem_cat <- sem_cat %>%
  left_join(depara, by = "nome_fantasia_do_veiculo") %>%
  mutate(grupo_editora.x = grupo_editora.y) %>%
  rename(grupo_editora = grupo_editora.x) %>%
  select(-grupo_editora.y)

dados <- com_cat %>% bind_rows(sem_cat)

# Classificando como "OUTROS" obs sem grupo_editora
# Extraindo ano de empenho e deflacionando valores
dados <- dados %>%
  mutate(grupo_editora = ifelse(is.na(grupo_editora), "OUTROS", grupo_editora),
         ano_empenho = str_sub(nº_empenho, 1, 4),
         deflacvalor_liquido_do_fornecedor = deflate(valor_liquido_do_fornecedor,
                                                     as.Date(data_do_pagamento), "09/2019","ipca"))

# Datas de pagamento futuras criam NAs na hora de deflacionar;
# Usar o próprio valor_liquido_do_fornecedor

dados <- dados %>%
  mutate(deflacvalor_liquido_do_fornecedor = ifelse(is.na(deflacvalor_liquido_do_fornecedor),
                                                    valor_liquido_do_fornecedor,
                                                    deflacvalor_liquido_do_fornecedor))
dados %>%
  filter(ano_empenho >= 2014) %>%
  group_by(ano_empenho, grupo_editora) %>%
  summarise(n = sum(deflacvalor_liquido_do_fornecedor)) %>%
  arrange(ano_empenho, desc(n)) %>% View()


temas <- data.frame(tema = unique(dados$tema), grupo = NA)
palavra_chave <- c("PREVIDÊNCIA",
                   "SAÚDE", "VACINAÇÃO", "CRACK", "IMUNIZAÇÃO",
                   "COPA", "OLIMPÍADAS",
                   "VIOLÊNCIA", "SEGURANÇA",
                   "ENERGIA",
                   "CONTAS", "IMPOSTO", "TAXA",
                   "FAMÍLIA",
                   "MULHER",
                   "MEIO AMBIENTE", "QUEIMADAS")
grupos <- c("PREVIDÊNCIA", rep("SAÚDE", 4), rep("COPA / OLIMPÍADAS", 2),
            rep("SEGURANÇA", 2), "ENERGIA", rep("CONTAS / IMPOSTO", 3),
            "FAMÍLIA", "MULHER", rep("MEIO AMBIENTE", 2))
depara_temas <- data.frame(grupos, palavra_chave)
depara_temas <- depara_temas %>% mutate(grupos = as.character(grupos),
                       palavra_chave = as.character(palavra_chave))

dados <- dados %>% mutate(tema = str_to_upper(tema))

for (i in seq(1,nrow(dados))) {
  ifelse(sum(str_detect(dados$tema[[i]], depara_temas$palavra_chave)) > 0,
         dados$palavra[[i]] <- list(which(str_detect(dados$tema[[i]], depara_temas$palavra_chave))),
         dados$palavra[[i]] <- NA)
  ifelse(length(dados$palavra[[i]][[1]]) > 1,
         dados$mais_de_uma_palavra_chave[[i]] <-  T,
         dados$mais_de_uma_palavra_chave[[i]] <-  F)
}

dados %>% filter(mais_de_uma_palavra_chave == T) %>% select(tema) %>% distinct() %>% View()
# São todos casos de violência contra a mulher -- utilizar grupo 'MULHER'

for (i in seq(1,nrow(dados))) {
  ifelse(dados$mais_de_uma_palavra_chave[[i]] == T,
         dados$palavra[[i]] <- dados$palavra[[i]][[1]][[2]],
         dados$palavra[[i]][[1]])
}

dados <- dados %>% cbind(palavra_chave = unlist(dados$palavra)) %>%
  select(-palavra)


dados <- dados %>% left_join(depara_temas %>% mutate(ind = row_number()),
                    by = c('palavra_chave' = 'ind')) %>%
  select(-palavra_chave.y)



dados %>%
  filter(ano_empenho >= 2013) %>%
  group_by(ano_empenho) %>%
  summarise(valor = sum(deflacvalor_liquido_do_fornecedor)) %>% arrange(desc(valor))

