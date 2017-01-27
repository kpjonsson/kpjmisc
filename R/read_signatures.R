### See http://cancer.sanger.ac.uk/cosmic/signatures
signatures_map = c('Age','APOBEC','BRCA','Smoking','5',
                   'MMR','UV','8','POLN','POLE',
                   'TMZ','12','APOBEC','14','MMR',
                   '16','17','18','19','MMR',
                   'MMR-like','Aristolochic acid','23','Aflatoxin','25',
                   'MMR','27','28','Chewing tobacco','30')

custom_order = c('Age','APOBEC','BRCA','Smoking','MMR',
                 'MMR-like','UV','POLN','POLE','TMZ',
                 'Aristolochic acid','Aflatoxin','Chewing tobacco',
                 '5','8','12','14','16',
                 '17','18','19','23','25',
                 '27','28','30')

read_signatures = function(input_file, signature_order = NULL) {

    sign = fread(input_file) %>%
        melt(id.vars = c('Sample Name', 'Number of Mutations')) %>%
        mutate(variable = str_replace(variable, 'Signature.', ''),
               variable = mapvalues(variable, seq(1,30), signatures_map))

    if (is.null(signature_order)) {
        sign = mutate(sign, variable = factor(variable, custom_order, ordered = T))
        sign
    } else {
        sign = mutate(sign, variable = factor(variable, signature_order, ordered = T))
    }

}
