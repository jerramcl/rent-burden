# Displacement Risk in LA vs. SF County 
<div align="center">
  
![download](https://github.com/user-attachments/assets/2ce8e8f0-84fd-420e-9406-19877cb82887)

</div>

This project analyzes displacement risk across two of California’s largest and most well-known counties, Los Angeles and San Francisco, by combining housing and health data to assess which region faces greater pressure on its most vulnerable populations. To do this, I merged publicly available datasets from the CDC and the U.S. Census to build a comparative framework around both economic and demographic indicators of displacement.

To assess displacement at the county level, I examined two key proxies:
- **Median rent as a percentage of income**, to capture cost burden
- **Prevalence of chronic health conditions**, which correlate with housing instability due to the financial and physical strain they place on households

**Los Angeles residents were more likely to be rent burdened** (54% vs. 35% in SF) and experienced **higher rates of nearly every major chronic illness**, including diabetes, depression, and high blood pressure. Combined, these factors suggest a higher baseline risk of eviction and displacement in LA County.

I then analyzed **subgroup vulnerability** using five demographic indicators:
1. **Renters:** LA has more rent-burdened households
2. **Homeowners with high-cost mortgages:** SF homeowners face higher housing costs, but income data would be needed for better context
3. **Racial groups receiving SNAP benefits:** LA’s low-income population skews more Hispanic and Black; SF’s is more White and Asian
4. **Uninsured seniors:** Slightly higher in LA, but rates were low overall
5. **Children in poverty:** 17% of low-income households in LA have children vs. 8% in SF

While both counties face affordability challenges, the results suggest that **Los Angeles exhibits a higher overall risk of displacement**, driven by its greater proportion of marginalized groups, worse public health indicators, and higher rent burden. San Francisco’s high mortgage costs and income inequality pose different challenges, but are less immediately tied to displacement based on the metrics used.

Note: R code and CDC data are in the code and data files, U.S. Census data was accessed via the R tidycensus package. 

