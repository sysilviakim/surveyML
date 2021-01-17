# Create Nationscape variable labels

relaber <- function(x) {
  x <- gsub(
    "immig_full_theta", "Immigration dim.",
    gsub(
      "econ_cons_theta", "Economic dim.",
      gsub(
        "racial10_C", "Racial attitudes",
        gsub(
          "pid7", "PID7",
          gsub(
            "Jwall_agree", "Build the wall",
            gsub(
              "Jdeportation_agree", "Supports deportations",
              gsub(
                "culture_full_theta", "Cultural dim.",
                gsub(
                  "democrats_favor_withDK", "Favorable view of Democrats",
                  gsub(
                    "blacks_favor_withDK", "Favorable view of Blacks",
                    gsub(
                      "latinos_favor_withDK", "Favorable view of Latinos",
                      gsub(
                        "whites_favor_withDK", "Favorable view of Whites",
                        gsub(
                          "labor_unions_favor_withDK", "Favorable view of labor unions",
                          gsub(
                            "republicans_favor_withDK", "Favorable view of Republicans",
                            gsub(
                              "lgbt_favor_withDK", "Favorable view of LGBT people",
                              gsub(
                                "undocumented_favor_withDK", "Favorable view of undoc. immigrants",
                                gsub(
                                  "muslims_favor_withDK", "Favorable view of Muslims",
                                  gsub(
                                    "republicans_favor_withDK", "Favorable view of Republicans",
                                    gsub(
                                      "republicans_unfavor_withDK", "Unfavorable view of Republicans",
                                      gsub(
                                        "whites_unfavor_withDK", "Unfavorable view of Whites",
                                        gsub(
                                          "democrats_unfavor_withDK", "Unfavorable view of Democrats",
                                          gsub(
                                            "blacks_unfavor_withDK", "Unfavorable view of Blacks",
                                            gsub(
                                              "latinos_unfavor_withDK", "Unfavorable view of Latinos",
                                              gsub(
                                                "whites_unfavor_withDK", "Unfavorable view of Whites",
                                                gsub(
                                                  "labor_unions_unfavor_withDK", "Unfav. view of labor unions",
                                                  gsub(
                                                    "republicans_unfavor_withDK", "Unfavorable view of Republicans",
                                                    gsub(
                                                      "lgbt_unfavor_withDK", "Unfavorable view of LGBT people",
                                                      gsub(
                                                        "undocumented_unfavor_withDK", "Unfavorable view of undoc. immigrants",
                                                        gsub(
                                                          "muslims_unfavor_withDK", "Unfavorable view of Muslims",
                                                          gsub(
                                                            "socialists_unfavor_withDK", "Unfavorable view of socialists",
                                                            gsub(
                                                              "PID7_1", "Strong Democrat",
                                                              gsub(
                                                                "PID7_2", "Weak Democrat",
                                                                gsub(
                                                                  "PID7_3", "Lean Democrat",
                                                                  gsub(
                                                                    "PID7_4", "Independent",
                                                                    gsub(
                                                                      "PID7_5", "Lean Republican",
                                                                      gsub(
                                                                        "PID7_6", "Weak Republican",
                                                                        gsub(
                                                                          "PID7_7", "Strong Republican",
                                                                          gsub(
                                                                            "ideo5_1", "Very Liberal",
                                                                            gsub(
                                                                              "ideo5_2", "Liberal",
                                                                              gsub(
                                                                                "ideo5_3", "Moderate",
                                                                                gsub(
                                                                                  "ideo5_4", "Conservative",
                                                                                  gsub(
                                                                                    "ideo5_5", "Very	Conservative",
                                                                                    gsub(
                                                                                      "ideo5_999", "Not Sure",
                                                                                      gsub(
                                                                                        "racial_attitudes_generations", "Racial attitudes: Slavery",
                                                                                        gsub(
                                                                                          "racial_attitudes_tryhard", "Racial attitudes: Try hard",
                                                                                          gsub(
                                                                                            "discrimination_blacks", "How much discrim. against Blacks",
                                                                                            gsub(
                                                                                              "discrimination_whites", "How much discrim. against whites",
                                                                                              gsub(
                                                                                                "discrimination_christians", "How much discrim. against Christians",
                                                                                                gsub(
                                                                                                  "gender_attitudes_complain", "Women who complain cause trouble",
                                                                                                  gsub(
                                                                                                    "age", "Age",
                                                                                                    x
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  return(x)
}

relab2 <- function(x) {
  x <- gsub(
    "educ_category_4", "Educ.: BA or higher",
    gsub(
      "educ_category_1", "Educ.: Less than HS grad",
      gsub(
        "educ_category_3", "Educ.: Some college",
        gsub(
          "educ_category_2", "Educ.: HS graduate",
          gsub(
            "inc_group_5", "Income: Over $100,000",
            gsub(
              "inc_group_1", "Income: Less than $25,000",
              gsub(
                "inc_group_2", "Income: $25,000 - $39,999",
                gsub(
                  "inc_group_3", "Income: $40,000 - $59,999",
                  gsub(
                    "inc_group_4", "Income: $60,000 - $99,999",
                    gsub(
                      "Jcap_agree", "Cap carbon emissions",
                      gsub(
                        "Jgov_ins_agree", "Supports gov. insurance",
                        gsub(
                          "Jban_AR_DISAGREE", "Against banning assault rifles",
                          gsub(
                            "Jpublic_option_agree", "Support a public option",
                            gsub(
                              "Jbanguns_DISAGREE", "Against banning guns",
                              gsub(
                                "Jlimit_magazines_DISAGREE", "Gun magazines limit (against)",
                                gsub(
                                  "PID7_legacy_1", "Strong Democrat",
                                  gsub(
                                    "PID7_legacy_2", "Weak Democrat",
                                    gsub(
                                      "PID7_legacy_3", "Lean Democrat",
                                      gsub(
                                        "PID7_legacy_4", "Independent",
                                        gsub(
                                          "PID7_legacy_5", "Lean Republican",
                                          gsub(
                                            "PID7_legacy_6", "Weak Republican",
                                            gsub(
                                              "PID7_legacy_7", "Strong Republican",
                                              gsub(
                                                "household_income_1", "Less than $14,999",
                                                gsub(
                                                  "household_income_2", "$15,000 to $19,999",
                                                  gsub(
                                                    "household_income_3", "$20,000 to $24,999",
                                                    gsub(
                                                      "household_income_4", "$25,000 to $29,999",
                                                      gsub(
                                                        "household_income_5", "$30,000 to $34,999",
                                                        gsub(
                                                          "household_income_6", "$35,000 to $39,999",
                                                          gsub(
                                                            "household_income_7", "$40,000 to $44,999",
                                                            gsub(
                                                              "household_income_8", "$45,000 to $49,999",
                                                              gsub(
                                                                "household_income_9", "$50,000 to $54,999",
                                                                gsub(
                                                                  "household_income_10", "$55,000 to $59,999",
                                                                  gsub(
                                                                    "household_income_11", "$60,000 to $64,999",
                                                                    gsub(
                                                                      "household_income_12", "$65,000 to $69,999",
                                                                      gsub(
                                                                        "household_income_13", "$70,000 to $74,999",
                                                                        gsub(
                                                                          "household_income_14", "$75,000 to $79,999",
                                                                          gsub(
                                                                            "household_income_15", "$80,000 to $84,999",
                                                                            gsub(
                                                                              "household_income_16", "$85,000 to $89,999",
                                                                              gsub(
                                                                                "household_income_17", "$90,000 to $94,999",
                                                                                gsub(
                                                                                  "household_income_18", "$95,000 to $99,999",
                                                                                  gsub(
                                                                                    "household_income_19", "$100,000 to $124,999",
                                                                                    gsub(
                                                                                      "household_income_20", "$125,000 to $149,999",
                                                                                      gsub(
                                                                                        "household_income_21", "$150,000 to $174,999",
                                                                                        gsub(
                                                                                          "household_income_22", "$175,000 to $199,999",
                                                                                          gsub(
                                                                                            "household_income_23", "$200,000 to $249,999",
                                                                                            gsub(
                                                                                              "household_income_24", "$250,000 and above",
                                                                                              x
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  return(x)
}
