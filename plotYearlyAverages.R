plotYearlyAverages <- function(df, SI.units = TRUE, ncol = 2) {
        require(dplyr)
        require(ggplot2)
        require(gridExtra) # grid.arrange() function
        
        # Period range for axes
        y0 <-   floor(min(df$Year) / 10) * 10
        y1 <- ceiling(max(df$Year) / 10) * 10
        
        if (SI.units) {
                # Metric system
                
                b0 <-   floor(min(df$BustSI))
                b1 <- ceiling(max(df$BustSI))
                h0 <-   floor(min(df$HeightSI) / 5) * 5
                h1 <- ceiling(max(df$HeightSI) / 5) * 5
                
                g.Bust <- ggplot(df, aes(x = Year, y = BustSI)) +
                        geom_point(shape = 18, size = 4) +
                        xlab("") +
                        ylab("Bust, cm") +
                        scale_x_continuous(minor_breaks = seq(y0, y1, 1), 
                                                 breaks = seq(y0, y1, 5)) +
                        scale_y_continuous(minor_breaks = seq(b0, b1, 1), 
                                                 breaks = seq(b0, b1, 2)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Waist <- ggplot(df, aes(x = Year, y = WaistSI)) +
                        geom_point(shape = 18, size = 4) +
                        xlab("") +
                        ylab("Waist, cm") +
                        scale_x_continuous(minor_breaks = seq(y0, y1, 1), breaks = seq(y0, y1, 5)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Hips <- ggplot(df, aes(x = Year, y = HipsSI)) +
                        geom_point(shape = 18, size = 4) +
                        xlab("") +
                        ylab("Hips, cm") +
                        scale_x_continuous(minor_breaks = seq(y0, y1, 1), breaks = seq(y0, y1, 5)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Height <- ggplot(df, aes(x = Year, y = HeightSI)) +
                        geom_point(shape = 18, size = 4) +
                        xlab("") +
                        ylab("Height, cm") +
                        scale_y_continuous(limits = c(h0, h1),
                                           minor_breaks = seq(h0, h1, 1), 
                                           breaks = seq(h0, h1, 5)) +
                        scale_x_continuous(minor_breaks = seq(y0, y1, 1), breaks = seq(y0, y1, 5)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Weight <- ggplot(df, aes(x = Year, y = WeightSI)) +
                        geom_point(shape = 18, size = 4) +
                        xlab("") +
                        ylab("Weight, kg") +
                        scale_x_continuous(minor_breaks = seq(y0, y1, 1), breaks = seq(y0, y1, 5)) +
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
                
                g.Bust <- ggplot(df, aes(x = Year, y = BustUS)) +
                        geom_point(shape = 18, size = 4) +
                        xlab("") +
                        ylab("Bust, in") +
                        scale_x_continuous(minor_breaks = seq(y0, y1, 1), breaks = seq(y0, y1, 5)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Waist <- ggplot(df, aes(x = Year, y = WaistUS)) +
                        geom_point(shape = 18, size = 4) +
                        xlab("") +
                        ylab("Waist, in") +
                        scale_x_continuous(minor_breaks = seq(y0, y1, 1), breaks = seq(y0, y1, 5)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Hips <- ggplot(df, aes(x = Year, y = HipsUS)) +
                        geom_point(shape = 18, size = 4) +
                        xlab("") +
                        ylab("Hips, in") +
                        scale_x_continuous(minor_breaks = seq(y0, y1, 1), breaks = seq(y0, y1, 5)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Height <- ggplot(df, aes(x = Year, y = HeightUS)) +
                        geom_point(shape = 18, size = 4) +
                        xlab("") +
                        ylab("Height, in") +
                        scale_x_continuous(minor_breaks = seq(y0, y1, 1), breaks = seq(y0, y1, 5)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
                
                g.Weight <- ggplot(df, aes(x = Year, y = WeightUS)) +
                        geom_point(shape = 18, size = 4) +
                        xlab("") +
                        ylab("Weight, lb") +
                        scale_y_continuous(minor_breaks = seq(w0, w1, 1), breaks = seq(w0, w1, 2)) +
                        scale_x_continuous(minor_breaks = seq(y0, y1, 1), breaks = seq(y0, y1, 5)) +
                        geom_smooth(method='lm') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
        
        g.BMI <- ggplot(df, aes(x = Year, y = BMI)) +
                geom_point(shape = 18, size = 4) +
                xlab("") +
                ylab("Body mass index") +
                scale_x_continuous(minor_breaks = seq(y0, y1, 1), breaks = seq(y0, y1, 5)) +
                geom_smooth(method='lm') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        # png(filename = "averages.png", width = 1080, height = 720)
        grid.arrange(g.Bust,  g.Height,
                     g.Waist, g.Weight,
                     g.Hips,  g.BMI, 
                     ncol = ncol)
        # dev.off()
        
}