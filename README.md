# ProjektWB2
Projekt na Warsztaty Badawcze 2

## [Prezentacja 1](https://github.com/airi314/ProjektWB2/blob/master/prezentacja1/prezentacja.md)

## Tabela wyników

| Zbiór (duży lub oryginalny) | Model | Parametry modelu | ACC | AUC | Folder/plik | Autor | Krótki opis preprocessingu |
| --- | --- | --- | --- | --- | --- | --- | --- |
| duży | classif.xgboost | predict.type = "response", objective = "multi:softmax", nrounds = 200 | 0.7528746 | 0.8755 | Eucalyptus/ Eucalyptus.R | Gosia | bez NA w Utitlity, lokalizacji, wartości bez sensu, sprzed 1984 r. |
| oryginalny | classif.xgboost | predict.type = "response", objective = "multi:softmax", nrounds = 200 | 0.6084337 | 0.8533 |  Eucalyptus/XgbOriginal.R | Gosia | bez NA w Utitlity i pustych obserwacji, z lokalizacją |
| oryginalny | classif.rpart | domyślne | 0.6024096 | 0.836 |  Eucalyptus/RpartOriginal.R | Gosia | bez NA w Utitlity i pustych obserwacji, z lokalizacją |
| oryginalny | classif.rpart x3 (z paperu) | domyślne | - | 0.8525 | Eucalyptus/eucalyptus_original_ordinal_classification(paper).R | Karol | preprocessing Gosi + usunięcie NA |
| oryginalny | classif.rpart | tuning | 0.6272727 | 0.8307 | Eucalyptus/rpart_tuning.R | Ola | bez NA w Utitlity |
