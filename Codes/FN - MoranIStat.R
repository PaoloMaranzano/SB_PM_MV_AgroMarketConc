MoranIStat <- function(Data,VarName,ContMat_listw,Labels,cols = c("grey25","grey60","grey85","grey45"),
                       Plot_titles = rep(NA,5), Plot_subtitles = rep(NA,5)) {
  
  # Data: sf object
  # VarName <- "RAPPORTOM"
  # ContMat_listw <- contnb_q_listw
  # Labels <- "LAU_NAME"
  # Labels <- NULL
  
  Data_moran <- Data %>%
    select(Y = any_of(VarName),
           Lab = any_of(Labels)) %>%
    mutate(Y_scaled = as.vector(scale(x = Y,center = T, scale = T)),
           Y_scaled_lag = as.vector(lag.listw(x = ContMat_listw, var = Y_scaled)),
           Quarter = case_when(Y_scaled >= 0 & Y_scaled_lag >= 0 ~ "HH",
                               Y_scaled < 0 & Y_scaled_lag >= 0 ~ "LH",
                               Y_scaled < 0 & Y_scaled_lag < 0 ~ "LL",
                               Y_scaled >= 0 & Y_scaled_lag < 0 ~ "HL"))
  
  
  ##### Global Moran's I statistic
  # https://mgimond.github.io/simple_moransI_example/?
  # Global Moran's statistic
  MoranI <- moran(x = Data_moran$Y, listw = ContMat_listw, n = length(Data_moran$Y), S0 = Szero(ContMat_listw))
  # Testing significance of Moran's I (Gaussian distribution)
  # "the excess mortality rates are randomly distributed across municipalities following a completely random process"
  MoranI_test_rand <- moran.test(x = Data_moran$Y, listw = ContMat_listw)
  MoranI_test <- moran.test(x = Data_moran$Y, listw = ContMat_listw, randomisation = FALSE)
  # Testing significance of Moran's I (Monte Carlo empirical distribution)
  MoranI_MC <- moran.mc(x = Data_moran$Y, listw = ContMat_listw, nsim = 1000)
  P1 <- plot(MoranI_MC)
  
  ##### Lagged variable and Moran's plot (Global I)
  # Y_lag <- lag.listw(x = contnb_q_listw, var = Data_moran$Y)
  # plot(Y_lag ~ Data$RAPPORTOMA, pch=16, asp=1)
  # M1 <- lm(Y_lag ~ Data$RAPPORTOMA)
  # abline(M1, col="blue")
  # summary(M1)
  P2 <- moran.plot(x = Data_moran$Y, listw = ContMat_listw, labels = Data_moran$Lab, pch = 19, quiet = F,
                   main = "Moran's scatterplot", xlab = "Variable", ylab = "Lagged variable")
  
  ##### Map plot (Global I)
  brks <- c("HH","LH","LL","HL")
  P3 <- Data_moran %>%
    ggplot()+ 
    theme_void() + 
    geom_sf(colour = "white") + 
    geom_sf(aes(fill= Quarter), size = 0.2, col="black") + 
    scale_fill_manual(breaks = brks, values = cols) +
    labs(title = "Moran's quarters") + 
    theme(title = element_text(size = 25, face = "bold"))
  
  ##### Local Moran's I
  alpha <- 0.05
  LocMoranI <- localmoran(x = Data_moran$Y, listw = ContMat_listw, zero.policy = TRUE, na.action = na.omit)
  LocMoranI_MC <- localmoran_perm(x = Data_moran$Y, listw = ContMat_listw, nsim = 1000, zero.policy = TRUE, na.action = na.omit)
  Data_moran <- Data_moran %>%
    mutate(LISA_Moran = as.vector(LocMoranI[,1]),
           LISA_Moran_pv = as.vector(LocMoranI[,5]),
           LISA_Moran_MC = as.vector(LocMoranI_MC[,1]),
           LISA_Moran_MC_pv = as.vector(LocMoranI_MC[,5]),
           Quarter_LISA = case_when(LISA_Moran_pv > alpha ~ "Not significant",
                                    TRUE ~ Quarter),
           Quarter_LISA_MC = case_when(LISA_Moran_MC_pv > alpha ~ "Not significant",
                                       TRUE ~ Quarter))
  
  ##### Map plot (Local I with significance)
  P4 <- Data_moran %>%
    ggplot()+ 
    theme_void() + 
    geom_sf(colour = "white") + 
    geom_sf(aes(fill= Quarter_LISA), size = 0.2, col="black") + 
    scale_fill_manual("LISA quarters",breaks = c("Not significant","HH","LH","LL","HL"), values = c("white",cols)) +
    labs(
      #title = ifelse(is.na(Plot_titles[4]),"Local Moran's quarters",Plot_titles[4]),
      subtitle = ifelse(is.na(Plot_subtitles[4]),"Monte Carlo permutations",Plot_subtitles[4])) + 
    theme(title = element_text(size = 25, face = "bold"))
  
  P5 <- Data_moran %>%
    ggplot()+ 
    theme_void() + 
    geom_sf(colour = "white") + 
    geom_sf(aes(fill= Quarter_LISA), size = 0.2, col="black") + 
    scale_fill_manual("LISA quarters",breaks = c("Not significant","HH","LH","LL","HL"), values = c("white",cols)) +
    labs(title = ifelse(is.na(Plot_titles[5]),"Local Moran's quarters",Plot_titles[5]),
         subtitle = ifelse(is.na(Plot_subtitles[5]),"Monte Carlo permutations",Plot_subtitles[5])) + 
    theme(title = element_text(size = 25, face = "bold"))
  
  ##### Moran’s I spatial correlogram
  contnb_q <- spdep::poly2nb(Data_moran, queen = TRUE, row.names = Data_moran$Lab)
  MoranCorr <- sp.correlogram(contnb_q, Data_moran$Y, order = 10, method = "I", style = "B",zero.policy = TRUE)
  # plot(MoranCorr,main = "Moran's spatial correlogram", xlab = "Neighbors lag", ylab = "Variable")
  P6_data <- MoranCorr$res
  P6_data <- data.frame(Lag = 1:dim(P6_data)[1], P6_data)
  colnames(P6_data) <- c("Lag","Estimate","Expectation","Variance")
  P6 <- P6_data %>%
    ggplot(aes(x=Lag, y=Estimate), col = "black") + 
    geom_point()+
    geom_errorbar(aes(ymin=Estimate-2*sqrt(Variance), ymax=Estimate+2*sqrt(Variance)),
                  width=.2, position=position_dodge(0.05)) + 
    geom_hline(yintercept = 0, col = "red", size = 1) + 
    labs(title = "Moran's spatial correlogram", x = "Lags", y = "Moran's I estimate") + 
    scale_x_discrete(limits = 1:10) + 
    scale_y_continuous(breaks = round(seq(from = min(P6_data$Estimate)-0.05, to = max(P6_data$Estimate)+0.05, by = 0.05),2)) + 
    theme_bw()
  
  return(list = list(Data_moran = Data_moran,
                     MoranI = MoranI,
                     MoranI_test_rand = MoranI_test_rand,
                     MoranI_test = MoranI_test,
                     MoranI_MC = MoranI_MC,
                     LocMoranI = LocMoranI,
                     LocMoranI_MC = LocMoranI_MC,
                     MoranCorr = MoranCorr,
                     P1 = P1,P2 = P2, P3 = P3,P4 = P4, P5 = P5, P6 = P6))
}
