t2<- tibble(Country = character(),
            Period = double(),
            Value = double())

t <- subset(sdgsMap, sdgsMap$Indicator =="Employed population below international poverty line, by sex and age (%)")


for (i in 1:nrow(t)) {
  if (t[i,3]=="Northern Africa"){
    t2 <- add_row(t2,Country="Algeria",Period=t[i,4],Value=t[i,6]/7)
    t2 <- add_row(t2,Country="Egypt",Period=t[i,4],Value=t[i,6]/7)
    t2 <- add_row(t2,Country="Libya",Period=t[i,4],Value=t[i,6]/7)
    t2 <- add_row(t2,Country="Morocco",Period=t[i,4],Value=t[i,6]/7)
    t2 <- add_row(t2,Country="Sudan",Period=t[i,4],Value=t[i,6]/7)
    t2 <- add_row(t2,Country="Tunisia",Period=t[i,4],Value=t[i,6]/7)
    t2 <- add_row(t2,Country="Western Sahara",Period=t[i,4],Value=t[i,6]/7)
    print(t[i,4])
  } 
}
for (i in 1:nrow(t)) {
  if (t[i,3]=="Sub-Saharan Africa"){
    t2 <- add_row(t2,Country="British Indian Ocean Territory",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Burundi",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Comoros",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Djibouti",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Eritrea",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Ethiopia",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="French Southern Territories",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Kenya",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Madagascar",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Malawi",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Mauritius",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Mayotte",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Mozambique",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Réunion",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Rwanda",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Seychelles",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Somalia",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="South Sudan",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Uganda",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="United Republic of Tanzania",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Zimbabwe",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Zambia",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Angola",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Cameroon",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Chad",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Central African Republic",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Congo",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Democratic Republic of the Congo",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Equatorial Guinea",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Gabon",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Sao Tome and Principe",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Botswana",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Eswatini",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Lesotho",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Namibia",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Togo",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Liberia",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="South Africa",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Burkina Faso",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Cabo Verde",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Benin",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Gambia",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Ghana",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Guinea",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Guinea-Bissau",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Mali",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Nigeria",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Niger",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Saint Helena",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Senegal",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Sierra Leone",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Mauritania",Period=t[i,4],Value=t[i,6]/53)
    t2 <- add_row(t2,Country="Côte d’Ivoire",Period=t[i,4],Value=t[i,6]/53)
  }
}
for (i in 1:nrow(t)) {
  if (t[i,3]=="Central Asia"){
    t2 <- add_row(t2,Country="Kazakhstan",Period=t[i,4],Value=t[i,6]/5)
    t2 <- add_row(t2,Country="Kyrgyzstan",Period=t[i,4],Value=t[i,6]/5)
    t2 <- add_row(t2,Country="Tajikistan",Period=t[i,4],Value=t[i,6]/5)
    t2 <- add_row(t2,Country="Turkmenistan",Period=t[i,4],Value=t[i,6]/5)
    t2 <- add_row(t2,Country="Uzbekistan",Period=t[i,4],Value=t[i,6]/5)
    print(t[i,4])
  } 
}
for (i in 1:nrow(t)) {
  if (t[i,3]=="Western Asia"){
    t2 <- add_row(t2,Country="Armenia",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Azerbaijan",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Bahrain",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Cyprus",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Georgia",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Iraq",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Israel",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Jordan",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Kuwait",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Lebanon",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Oman",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Qatar",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Saudi Arabia",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="State of Palestine",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Syrian Arab Republic",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Turkey",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="United Arab Emirates",Period=t[i,4],Value=t[i,6]/18)
    t2 <- add_row(t2,Country="Yemen",Period=t[i,4],Value=t[i,6]/18)
    print(t[i,4])
  }
}
for (i in 1:nrow(t)) {
  if (t[i,3]=="South-Eastern Asia"){
    t2 <- add_row(t2,Country="Brunei Darussalam",Period=t[i,4],Value=t[i,6]/11)
    t2 <- add_row(t2,Country="Cambodia",Period=t[i,4],Value=t[i,6]/11)
    t2 <- add_row(t2,Country="Indonesia",Period=t[i,4],Value=t[i,6]/11)
    t2 <- add_row(t2,Country="Lao People's Democratic Republic",Period=t[i,4],Value=t[i,6]/11)
    t2 <- add_row(t2,Country="Malaysia",Period=t[i,4],Value=t[i,6]/11)
    t2 <- add_row(t2,Country="Myanmar",Period=t[i,4],Value=t[i,6]/11)
    t2 <- add_row(t2,Country="Philippines",Period=t[i,4],Value=t[i,6]/11)
    t2 <- add_row(t2,Country="Singapore",Period=t[i,4],Value=t[i,6]/11)
    t2 <- add_row(t2,Country="Thailand",Period=t[i,4],Value=t[i,6]/11)
    t2 <- add_row(t2,Country="Timor-Leste",Period=t[i,4],Value=t[i,6]/11)
    t2 <- add_row(t2,Country="Viet Nam",Period=t[i,4],Value=t[i,6]/11)
  }
}
#note change name Central and Southern Asia to Southern Asia
for (i in 1:nrow(t)) {
  if (t[i,3]=="Southern Asia"){
    t2 <- add_row(t2,Country="Afghanistan",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Bangladesh",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Bhutan",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="India",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Iran (Islamic Republic of)",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Maldives",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Nepal",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Pakistan",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Sri Lanka",Period=t[i,4],Value=t[i,6]/9)
  }
}
for (i in 1:nrow(t)) {
  if (t[i,3]=="Eastern Europe"){
    t2 <- add_row(t2,Country="Belarus",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Bulgaria",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Czechia",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Hungary",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Poland",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Republic of Moldova",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Russian Federation",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Slovakia",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Ukraine",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Romania",Period=t[i,4],Value=t[i,6]/10)
  }
}
for (i in 1:nrow(t)) {
  if (t[i,3]=="Western Europe"){
    t2 <- add_row(t2,Country="Austria",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Belgium",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="France",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Germany",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Liechtenstein",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Luxembourg",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Monaco",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Netherlands",Period=t[i,4],Value=t[i,6]/9)
    t2 <- add_row(t2,Country="Switzerland",Period=t[i,4],Value=t[i,6]/9)
  }
}
for (i in 1:nrow(t)) {
  if (t[i,3]=="Southern Europe"){
    t2 <- add_row(t2,Country="Albania",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Andorra",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Bosnia and Herzegovina",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Croatia",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Gibraltar",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Greece",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Holy See",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Italy",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Malta",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Montenegro",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="North Macedonia",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Portugal",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="San Marino",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Serbia",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Slovenia",Period=t[i,4],Value=t[i,6]/16)
    t2 <- add_row(t2,Country="Spain",Period=t[i,4],Value=t[i,6]/16)
  }
}
for (i in 1:nrow(t)) {
  if (t[i,3]=="Northern Europe"){
    t2 <- add_row(t2,Country="Åland Islands",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Guernsey",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Jersey",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Sark",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Denmark",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Estonia",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Faroe Islands",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Finland",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Iceland",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Ireland",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Isle of Man",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Latvia",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Lithuania",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Norway",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Svalbard and Jan Mayen Islands",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="Sweden",Period=t[i,4],Value=t[i,6]/17)
    t2 <- add_row(t2,Country="United Kingdom of Great Britain and Northern Ireland",Period=t[i,4],Value=t[i,6]/17)
  }
}

for (i in 1:nrow(t)) {
  if (t[i,3]=="Polynesia"){
    t2 <- add_row(t2,Country="American Samoa",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Cook Islands",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="French Polynesia",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Niue",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Pitcairn",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Samoa",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Tokelau",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Tonga",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Tuvalu",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Wallis and Futuna Islands",Period=t[i,4],Value=t[i,6]/10)
    
  }
}
for (i in 1:nrow(t)) {
  if (t[i,3]=="Eastern Asia"){
    t2 <- add_row(t2,Country="China",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Hong Kong Special Administrative Region",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Macao Special Administrative Region",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Democratic People's Republic of Korea",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Japan",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Mongolia",Period=t[i,4],Value=t[i,6]/10)
    t2 <- add_row(t2,Country="Republic of Korea",Period=t[i,4],Value=t[i,6]/10)
  }
}