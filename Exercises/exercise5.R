library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Read the half-hourly flux data
flux_data <- read_csv("data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")


# If TIMESTAMP_START is numeric, e.g. 200401010000 (YYYYMMDDhhmm),
# convert it to character, then parse with ymd_hm().
# This turns "200401010000" into POSIXct date-time like 2004-01-01 00:00:00 UTC.
flux_data <- flux_data %>%
  mutate(
    # First, ensure TIMESTAMP_START is character:
    TIMESTAMP_START = as.character(TIMESTAMP_START),
    # Next, parse as date-time using lubridate:
    TIMESTAMP_START = ymd_hm(TIMESTAMP_START)
  )


################### Part A. Labeling and Visualizing Spurious Data
flux_data <- flux_data %>%
  mutate(spurious = duplicated(GPP_NT_VUT_REF) | duplicated(GPP_NT_VUT_REF, fromLast = TRUE))


flux_2004 <- flux_data %>%
  filter(year(TIMESTAMP_START) == 2004)

ggplot(flux_2004, aes(x = TIMESTAMP_START, y = GPP_NT_VUT_REF, color = spurious)) +
  geom_line() +
  labs(title = "Half-hourly GPP for 2004",
       x = "Time",
       y = expression(paste("GPP (", mu, "mol CO"[2], " m"^-2, " s"^-1, ")")),
       color = "Spurious?") +
  theme_classic()


daily_data <- flux_data %>%
  mutate(date = as_date(TIMESTAMP_START)) %>%
  group_by(date) %>%
  summarise(daily_mean_GPP = mean(GPP_NT_VUT_REF, na.rm = TRUE),
            prop_spurious = mean(spurious, na.rm = TRUE))  # Because TRUE=1, FALSE=0


ggplot(daily_data, aes(x = date, y = daily_mean_GPP, color = prop_spurious)) +
  geom_line() +
  scale_color_viridis_c(option = "plasma") +
  labs(title = "Daily Mean GPP with Proportion of Spurious Data (2004)",
       x = "Date",
       y = expression(paste("Daily Mean GPP (", mu, "mol CO"[2], " m"^-2, " s"^-1, ")")),
       color = "Prop. Spurious") +
  theme_classic()


############################################# Part B. Detecting Outliers in GPP
# Calculate quartiles and IQR
q1 <- quantile(flux_data$GPP_NT_VUT_REF, 0.25, na.rm = TRUE)
q3 <- quantile(flux_data$GPP_NT_VUT_REF, 0.75, na.rm = TRUE)
iqr <- q3 - q1
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

flux_data <- flux_data %>%
  mutate(outlier_raw = GPP_NT_VUT_REF < lower_bound | GPP_NT_VUT_REF > upper_bound)


ggplot(flux_data, aes(x = SW_IN_F, y = GPP_NT_VUT_REF, color = outlier_raw)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  labs(title = "GPP vs. Shortwave Radiation (Raw Outliers Highlighted)",
       x = expression(paste("Shortwave Radiation (W m"^-2, ")")),
       y = expression(paste("GPP (", mu, "mol CO"[2], " m"^-2, " s"^-1, ")")),
       color = "Outlier") +
  theme_classic()


model <- lm(GPP_NT_VUT_REF ~ SW_IN_F, data = flux_data)
flux_data <- flux_data %>%
  mutate(residuals = resid(model))

# Determine quartiles and IQR for residuals
q1_res <- quantile(flux_data$residuals, 0.25, na.rm = TRUE)
q3_res <- quantile(flux_data$residuals, 0.75, na.rm = TRUE)
iqr_res <- q3_res - q1_res
lower_bound_res <- q1_res - 5 * iqr_res
upper_bound_res <- q3_res + 5 * iqr_res

flux_data <- flux_data %>%
  mutate(outlier_res = residuals < lower_bound_res | residuals > upper_bound_res)


ggplot(flux_data, aes(x = SW_IN_F, y = GPP_NT_VUT_REF, color = outlier_res)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  labs(title = "GPP vs. Shortwave Radiation (Residual Outliers Highlighted)",
       x = expression(paste("Shortwave Radiation (W m"^-2, ")")),
       y = expression(paste("GPP (", mu, "mol CO"[2], " m"^-2, " s"^-1, ")")),
       color = "Outlier (Residuals)") +
  theme_classic()
