# Script to get correct category label expressions for plots (EPA impact categories)

unit_list <- c('kg', 'kg', 'kg', 'n', '$', 'kg', 'ctu', 'kg', 'kg', 'ctu', 'ctu', 'kg', 'ctu', 'kg', 'MJ', 'm2', 'kg', 'MJ', 'MJ', 'm3')
conversion_factor <- c(1e-6, 1e-6, 1e-6, 1e-6, 1e-9, 1e-9, 1e-12, 1e-9, 1e-9, 1, 1, 1e-9, 1, 1e-3, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9)

# Long names for y-axes, with units now in more reasonable ranges of numbers
category_labels <- list('Hazardous air pollutants released (kT)',
                        'Heavy metals released (kT)',
                        'Pesticides released (kT)',
                        'Millions of jobs created',
                        'Value added (billion $)',
                        expression(paste('Acid released (MT SO'[2], ' eq.)')),
                        expression(paste('Environmental toxicity (10'^12, ' CTUe)')),
                        'Eutrophication (MT N eq.)',
                        expression(paste('GHG emissions (MT CO'[2], ' eq.)')),
                        'Human toxicity, cancer (CTUh)',
                        'Human toxicity, non-cancer (CTUh)',
                        expression(paste('Particulates released (MT PM'[2.5], ' eq.)')),
                        'Human toxicity, total (CTUh)',
                        'Ozone depletion (MT CFC-11 eq.)',
                        expression(paste('Smog (kg O'[3], ' eq.)')),
                        'Total energy used (PJ)',
                        expression(paste('Land used (thousand km'^2, ' y'^-1,')', sep='')),
                        'Minerals used (MT)',
                        'Nonrenewable energy used (PJ)',
                        'Renewable energy used (PJ)',
                        expression(paste('Water used (km'^3, ')', sep = ''))
)

# Category labels all as a character vector
category_labels_character <- c('paste("Hazardous air pollutants released (kT)")',
                               'paste("Heavy metals released (kT)")',
                               'paste("Pesticides released (kT)")',
                               'paste("Millions of jobs created")',
                               'paste("Value added (billion $)")',
                               'paste("Acid released (MT ", SO[2], " eq.)")',
                               'paste("Environmental toxicity (", 10^12, " CTUe)")',
                               'paste("Eutrophication (MT N eq.)")',
                               'paste("GHG emissions (MT ", CO[2], " eq.)")',
                               'paste("Human toxicity, cancer (CTUh)")',
                               'paste("Human toxicity, non-cancer (CTUh)")',
                               'paste("Particulates released (MT ", PM[2.5], " eq.)")',
                               'paste("Human toxicity, total (CTUh)")',
                               'paste("Ozone depletion (MT CFC-11 eq.)")',
                               'paste("Smog (kg ", O[3], " eq.)")',
                               'paste("Total energy used (PJ)")',
                               'paste("Land used (thousand ", km^2~y^-1, ")")',
                               'paste("Minerals used (MT)")',
                               'paste("Nonrenewable energy used (PJ)")',
                               'paste("Renewable energy used (PJ)")',
                               'paste("Water used ", (km^3))'
)