plotEveryBody <- function(df, SI.units = TRUE, ncol = 2) {
        require(dplyr)
        require(ggplot2)
        require(gridExtra) # grid.arrange() function
        
        # Period range for axes
        y0 <- min(df$Period)
        y1 <- max(df$Period)
        
        if (SI.units) {
                # Metric system
                
                b0 <-   floor(min(df$BustSI))
                b1 <- ceiling(max(df$BustSI))
                h0 <-   floor(min(df$HeightSI) / 5) * 5
                h1 <- ceiling(max(df$HeightSI) / 5) * 5
                
                g.Bust <- ggplot(df, aes(x = Period, y = BustSI)) +
                        geom_point(size = 2, shape = 5) +
                        xlab("") +
                        ylab("Bust, cm") +
                        scale_y_continuous(minor_breaks = seq(b0, b1, 1), breaks = seq(b0, b1, 2)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Waist <- ggplot(df, aes(x = Period, y = WaistSI)) +
                        geom_point(size = 2, shape = 5) +
                        xlab("") +
                        ylab("Waist, cm") +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Hips <- ggplot(df, aes(x = Period, y = HipsSI)) +
                        geom_point(size = 2, shape = 5) +
                        xlab("") +
                        ylab("Hips, cm") +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Height <- ggplot(df, aes(x = Period, y = HeightSI)) +
                        geom_point(size = 2, shape = 5) +
                        xlab("") +
                        ylab("Height, cm") +
                        scale_y_continuous(limits = c(h0, h1),
                                           minor_breaks = seq(h0, h1, 1), 
                                           breaks = seq(h0, h1, 5)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Weight <- ggplot(df, aes(x = Period, y = WeightSI)) +
                        geom_point(size = 2, shape = 5) +
                        xlab("") +
                        ylab("Weight, kg") +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
        } else {
                # US customary units
                
                b0 <-   floor(min(df$BustUS))
                b1 <- ceiling(max(df$BustUS))
                
                h0 <- min(df$HeightUS)
                h1 <- max(df$HeightUS)
                
                w0 <-   floor(min(df$WeightUS))
                w1 <- ceiling(max(df$WeightUS))
                
                g.Bust <- ggplot(df, aes(x = Period, y = BustUS)) +
                        geom_point(size = 2, shape = 5) +
                        xlab("") +
                        ylab("Bust, in") +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Waist <- ggplot(df, aes(x = Period, y = WaistUS)) +
                        geom_point(size = 2, shape = 5) +
                        xlab("") +
                        ylab("Waist, in") +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Hips <- ggplot(df, aes(x = Period, y = HipsUS)) +
                        geom_point(size = 2, shape = 5) +
                        xlab("") +
                        ylab("Hips, in") +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Height <- ggplot(df, aes(x = Period, y = HeightUS)) +
                        geom_point(size = 2, shape = 5) +
                        xlab("") +
                        ylab("Height, in") +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Weight <- ggplot(df, aes(x = Period, y = WeightUS)) +
                        geom_point(size = 2, shape = 5) +
                        xlab("") +
                        ylab("Weight, lb") +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
        
        g.BMI <- ggplot(df, aes(x = Period, y = BMI)) +
                geom_point(size = 2, shape = 5) +
                xlab("") +
                ylab("Body mass index") +
                geom_smooth(method='lm') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
#         png(filename = "everybody.png", width = 1080, height = 720)
        grid.arrange(g.Bust,  g.Height,
                     g.Waist, g.Weight,
                     g.Hips,  g.BMI, 
                     ncol = ncol, main = "")
#         dev.off()
        
}